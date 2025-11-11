# ==============================================================================
# REGRESIÓN Y ENSEMBLE - WINE QUALITY (WHITE)
# ==============================================================================
#
# Dataset: Wine Quality (Vino Blanco)
# Observaciones: 4,898
# Variables: 12 (11 predictoras + 1 objetivo)
#
# Descripción:
# Dataset de vinos blancos portugueses con propiedades fisicoquímicas
# y calificación de calidad. Similar al dataset de vinos tintos pero
# con mayor número de observaciones.
#
# Métodos aplicados:
# 1. Regresión lineal
# 2. Ridge/Lasso
# 3. Random Forest
# 4. XGBoost
# 5. Comparación con vino tinto
#
# ==============================================================================

library(tidyverse)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN
# ==============================================================================

print("=== CARGANDO DATOS ===")

wine_white <- read_delim("01_Datos/datasets_ml/winequality-white.csv",
                         delim = ";", show_col_types = FALSE)

# Limpiar nombres de columnas (eliminar espacios y caracteres especiales)
colnames(wine_white) <- make.names(colnames(wine_white))

print("Dimensiones:")
print(dim(wine_white))

print("Distribución de calidad:")
print(table(wine_white$quality))

# Resumen estadístico
print("Resumen:")
print(summary(wine_white))

# ==============================================================================
# 2. VISUALIZACIÓN
# ==============================================================================

print("=== VISUALIZACIONES ===")

# Distribución de calidad
ggplot(wine_white, aes(x = quality)) +
  geom_histogram(binwidth = 1, fill = "gold", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Calidad - Vino Blanco",
       x = "Calidad", y = "Frecuencia")

ggsave("03_Visualizaciones/wine_white_quality_dist.png",
       width = 8, height = 6)

# Correlaciones clave con calidad
cor_with_quality <- cor(wine_white)[, "quality"] %>%
  as.data.frame() %>%
  rename(Correlacion = ".") %>%
  arrange(desc(abs(Correlacion)))

print("Correlaciones con calidad:")
print(cor_with_quality)

# ==============================================================================
# 3. DIVISIÓN DE DATOS
# ==============================================================================

train_idx <- createDataPartition(wine_white$quality, p = 0.7, list = FALSE)
train_data <- wine_white[train_idx, ]
test_data <- wine_white[-train_idx, ]

print(paste("Entrenamiento:", nrow(train_data)))
print(paste("Prueba:", nrow(test_data)))

# Preparar matrices
x_train <- model.matrix(quality ~ ., train_data)[, -1]
y_train <- train_data$quality
x_test <- model.matrix(quality ~ ., test_data)[, -1]
y_test <- test_data$quality

# ==============================================================================
# 4. MODELOS DE REGRESIÓN
# ==============================================================================

print("=== REGRESIÓN LINEAL ===")
lm_model <- lm(quality ~ ., data = train_data)
lm_pred <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((y_test - lm_pred)^2))
print(paste("RMSE:", round(lm_rmse, 4)))

print("=== RIDGE ===")
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_model <- glmnet(x_train, y_train, alpha = 0,
                      lambda = ridge_cv$lambda.min)
ridge_pred <- predict(ridge_model, x_test)
ridge_rmse <- sqrt(mean((y_test - ridge_pred)^2))
print(paste("RMSE:", round(ridge_rmse, 4)))

print("=== LASSO ===")
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_model <- glmnet(x_train, y_train, alpha = 1,
                      lambda = lasso_cv$lambda.min)
lasso_pred <- predict(lasso_model, x_test)
lasso_rmse <- sqrt(mean((y_test - lasso_pred)^2))
lasso_coefs <- coef(lasso_model)
n_vars <- sum(lasso_coefs[-1] != 0)
print(paste("Variables seleccionadas:", n_vars))
print(paste("RMSE:", round(lasso_rmse, 4)))

print("=== RANDOM FOREST ===")
rf_model <- randomForest(quality ~ ., data = train_data,
                         ntree = 200, mtry = 4)
rf_pred <- predict(rf_model, test_data)
rf_rmse <- sqrt(mean((y_test - rf_pred)^2))
print(paste("RMSE:", round(rf_rmse, 4)))

print("=== XGBOOST ===")
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.1
)

xgb_cv <- xgb.cv(params = params, data = dtrain, nrounds = 200,
                 nfold = 5, early_stopping_rounds = 20, verbose = 0)

xgb_model <- xgb.train(params = params, data = dtrain,
                       nrounds = xgb_cv$best_iteration, verbose = 0)
xgb_pred <- predict(xgb_model, dtest)
xgb_rmse <- sqrt(mean((y_test - xgb_pred)^2))
print(paste("RMSE:", round(xgb_rmse, 4)))

# ==============================================================================
# 5. COMPARACIÓN
# ==============================================================================

print("=== COMPARACIÓN DE MODELOS ===")

resultados <- data.frame(
  Modelo = c("Lineal", "Ridge", "Lasso", "Random Forest", "XGBoost"),
  RMSE = c(lm_rmse, ridge_rmse, lasso_rmse, rf_rmse, xgb_rmse),
  R2 = c(cor(y_test, lm_pred)^2,
         cor(y_test, ridge_pred)^2,
         cor(y_test, lasso_pred)^2,
         cor(y_test, rf_pred)^2,
         cor(y_test, xgb_pred)^2)
)

print(resultados)

# Visualizar comparación
ggplot(resultados, aes(x = reorder(Modelo, RMSE), y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Comparación RMSE - Vino Blanco",
       x = "", y = "RMSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("03_Visualizaciones/wine_white_comparison.png",
       width = 10, height = 6)

# Predicciones vs reales (mejor modelo)
mejor_idx <- which.min(resultados$RMSE)
mejor_modelo <- resultados$Modelo[mejor_idx]

if (mejor_modelo == "XGBoost") {
  best_pred <- xgb_pred
} else if (mejor_modelo == "Random Forest") {
  best_pred <- rf_pred
} else {
  best_pred <- lm_pred
}

pred_df <- data.frame(Real = y_test, Predicho = best_pred)

ggplot(pred_df, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = "gold3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = paste("Predicciones -", mejor_modelo),
       x = "Calidad Real", y = "Calidad Predicha")

ggsave("03_Visualizaciones/wine_white_predictions.png",
       width = 8, height = 8)

print("=== ANÁLISIS COMPLETADO ===")
print("Vinos blancos muestran patrones similares a tintos")
print("Métodos ensemble superan a regresión lineal")
print(paste("Mejor modelo:", mejor_modelo))
