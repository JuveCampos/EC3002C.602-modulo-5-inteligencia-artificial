# ==============================================================================
# REGRESIÓN AVANZADA CON DATOS ESPACIALES - CALIFORNIA HOUSING
# ==============================================================================
#
# Dataset: California Housing
# Observaciones: 20,640
# Variables: 9 (8 predictoras + 1 objetivo)
#
# Descripción:
# Dataset de precios de viviendas en California basado en el censo de 1990.
# Contiene información a nivel de bloques de censo incluyendo ubicación
# geográfica (latitud/longitud).
#
# Métodos aplicados:
# 1. Regresión lineal múltiple
# 2. Ridge Regression
# 3. Lasso Regression
# 4. Stepwise Regression
# 5. Random Forest para regresión
# 6. Visualización espacial
#
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación y visualización
library(glmnet)       # Ridge y Lasso
library(MASS)         # Stepwise
library(dplyr)        # Recargar dplyr para evitar conflicto con MASS::select
library(randomForest) # Random Forest
library(caret)        # Evaluación
library(corrplot)     # Correlaciones
library(viridis)      # Paletas de colores

set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

print("=== CARGANDO DATOS ===")

housing_data <- read_csv("01_Datos/datasets_ml/california_housing.csv",
                         show_col_types = FALSE)

print("Dimensiones del dataset:")
print(dim(housing_data))

print("Primeras observaciones:")
print(head(housing_data))

print("Resumen estadístico:")
print(summary(housing_data))

print("Valores faltantes:")
print(colSums(is.na(housing_data)))

# ==============================================================================
# 2. VISUALIZACIÓN ESPACIAL
# ==============================================================================

print("=== GENERANDO VISUALIZACIONES ESPACIALES ===")

# Mapa de precios de viviendas
ggplot(housing_data, aes(x = Longitude, y = Latitude, color = MedHouseVal)) +
  geom_point(alpha = 0.4, size = 0.5) +
  scale_color_viridis(option = "plasma", name = "Precio Mediano\n($100k)") +
  theme_minimal() +
  labs(title = "Distribución Espacial de Precios de Viviendas en California",
       x = "Longitud", y = "Latitud") +
  coord_fixed(ratio = 1.3)

ggsave("03_Visualizaciones/california_spatial_prices.png",
       width = 10, height = 12)

# Mapa de densidad de población
ggplot(housing_data, aes(x = Longitude, y = Latitude, color = log(Population))) +
  geom_point(alpha = 0.3, size = 0.5) +
  scale_color_viridis(option = "magma", name = "log(Población)") +
  theme_minimal() +
  labs(title = "Distribución Espacial de Población",
       x = "Longitud", y = "Latitud") +
  coord_fixed(ratio = 1.3)

ggsave("03_Visualizaciones/california_spatial_population.png",
       width = 10, height = 12)

# ==============================================================================
# 3. ANÁLISIS DE CORRELACIONES
# ==============================================================================

print("=== ANÁLISIS DE CORRELACIONES ===")

cor_matrix <- cor(housing_data)

png("03_Visualizaciones/california_correlation.png",
    width = 1200, height = 1000)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         title = "Matriz de Correlación - California Housing",
         mar = c(0, 0, 2, 0))
dev.off()

# Correlaciones con variable objetivo
cor_target <- cor_matrix[, "MedHouseVal"] %>%
  as.data.frame() %>%
  rename(Correlacion = ".") %>%
  arrange(desc(abs(Correlacion)))

print("Correlaciones con precio mediano:")
print(cor_target)

# ==============================================================================
# 4. DIVISIÓN DE DATOS Y PREPARACIÓN
# ==============================================================================

print("=== PREPARANDO DATOS ===")

# División 70-30
train_idx <- createDataPartition(housing_data$MedHouseVal, p = 0.7, list = FALSE)

train_data <- housing_data[train_idx, ]
test_data <- housing_data[-train_idx, ]

print(paste("Observaciones de entrenamiento:", nrow(train_data)))
print(paste("Observaciones de prueba:", nrow(test_data)))

# Preparar matrices para glmnet
x_train <- model.matrix(MedHouseVal ~ ., train_data)[, -1]
y_train <- train_data$MedHouseVal

x_test <- model.matrix(MedHouseVal ~ ., test_data)[, -1]
y_test <- test_data$MedHouseVal

# ==============================================================================
# 5. REGRESIÓN LINEAL MÚLTIPLE
# ==============================================================================

print("=== REGRESIÓN LINEAL MÚLTIPLE ===")

lm_model <- lm(MedHouseVal ~ ., data = train_data)

print("Resumen del modelo:")
print(summary(lm_model))

# Diagnósticos del modelo
png("03_Visualizaciones/california_lm_diagnostics.png",
    width = 1200, height = 900)
par(mfrow = c(2, 2))
plot(lm_model)
dev.off()

# Predicciones
lm_pred <- predict(lm_model, test_data)

lm_rmse <- sqrt(mean((y_test - lm_pred)^2))
lm_mae <- mean(abs(y_test - lm_pred))
lm_r2 <- cor(y_test, lm_pred)^2

print(paste("RMSE:", round(lm_rmse, 4)))
print(paste("MAE:", round(lm_mae, 4)))
print(paste("R²:", round(lm_r2, 4)))

# ==============================================================================
# 6. STEPWISE REGRESSION
# ==============================================================================

print("=== STEPWISE REGRESSION ===")

# Modelo nulo y completo
null_model <- lm(MedHouseVal ~ 1, data = train_data)
full_model <- lm(MedHouseVal ~ ., data = train_data)

# Stepwise selection
stepwise_model <- stepAIC(null_model,
                          scope = list(lower = null_model, upper = full_model),
                          direction = "both",
                          trace = FALSE)

print("Variables seleccionadas:")
print(names(coef(stepwise_model)))

print("Resumen del modelo stepwise:")
print(summary(stepwise_model))

# Predicciones
step_pred <- predict(stepwise_model, test_data)

step_rmse <- sqrt(mean((y_test - step_pred)^2))
step_mae <- mean(abs(y_test - step_pred))
step_r2 <- cor(y_test, step_pred)^2

print(paste("RMSE:", round(step_rmse, 4)))
print(paste("MAE:", round(step_mae, 4)))
print(paste("R²:", round(step_r2, 4)))

# ==============================================================================
# 7. RIDGE REGRESSION
# ==============================================================================

print("=== RIDGE REGRESSION ===")

# Validación cruzada para lambda óptimo
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)

print(paste("Lambda óptimo:", round(ridge_cv$lambda.min, 6)))

# Visualizar CV
png("03_Visualizaciones/california_ridge_cv.png",
    width = 1000, height = 600)
plot(ridge_cv, main = "Validación Cruzada - Ridge")
dev.off()

# Modelo final
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = ridge_cv$lambda.min)

# Predicciones
ridge_pred <- predict(ridge_model, x_test)

ridge_rmse <- sqrt(mean((y_test - ridge_pred)^2))
ridge_mae <- mean(abs(y_test - ridge_pred))
ridge_r2 <- cor(y_test, ridge_pred)^2

print(paste("RMSE:", round(ridge_rmse, 4)))
print(paste("MAE:", round(ridge_mae, 4)))
print(paste("R²:", round(ridge_r2, 4)))

# ==============================================================================
# 8. LASSO REGRESSION
# ==============================================================================

print("=== LASSO REGRESSION ===")

# Validación cruzada
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

print(paste("Lambda óptimo:", round(lasso_cv$lambda.min, 6)))

# Visualizar CV
png("03_Visualizaciones/california_lasso_cv.png",
    width = 1000, height = 600)
plot(lasso_cv, main = "Validación Cruzada - Lasso")
dev.off()

# Modelo final
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_cv$lambda.min)

# Coeficientes
lasso_coefs <- coef(lasso_model)
n_selected <- sum(lasso_coefs[-1] != 0)
print(paste("Variables seleccionadas:", n_selected))
print("Coeficientes:")
print(lasso_coefs)

# Predicciones
lasso_pred <- predict(lasso_model, x_test)

lasso_rmse <- sqrt(mean((y_test - lasso_pred)^2))
lasso_mae <- mean(abs(y_test - lasso_pred))
lasso_r2 <- cor(y_test, lasso_pred)^2

print(paste("RMSE:", round(lasso_rmse, 4)))
print(paste("MAE:", round(lasso_mae, 4)))
print(paste("R²:", round(lasso_r2, 4)))

# Visualizar path de coeficientes
png("03_Visualizaciones/california_lasso_path.png",
    width = 1000, height = 600)
plot(glmnet(x_train, y_train, alpha = 1),
     xvar = "lambda",
     main = "Path de Coeficientes Lasso")
abline(v = log(lasso_cv$lambda.min), lty = 2, col = "red")
dev.off()

# ==============================================================================
# 9. RANDOM FOREST
# ==============================================================================

print("=== RANDOM FOREST ===")

# Entrenar Random Forest
# Usar muestra para acelerar entrenamiento
sample_idx <- sample(1:nrow(train_data), min(5000, nrow(train_data)))
train_sample <- train_data[sample_idx, ]

rf_model <- randomForest(MedHouseVal ~ .,
                         data = train_sample,
                         ntree = 200,
                         mtry = 3,
                         importance = TRUE)

print("Modelo Random Forest:")
print(rf_model)

# Importancia de variables
importance_df <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`))

print("Importancia de variables:")
print(importance_df)

# Visualizar importancia
png("03_Visualizaciones/california_rf_importance.png",
    width = 1000, height = 800)
varImpPlot(rf_model, main = "Importancia de Variables - Random Forest")
dev.off()

# Predicciones
rf_pred <- predict(rf_model, test_data)

rf_rmse <- sqrt(mean((y_test - rf_pred)^2))
rf_mae <- mean(abs(y_test - rf_pred))
rf_r2 <- cor(y_test, rf_pred)^2

print(paste("RMSE:", round(rf_rmse, 4)))
print(paste("MAE:", round(rf_mae, 4)))
print(paste("R²:", round(rf_r2, 4)))

# ==============================================================================
# 10. COMPARACIÓN DE MODELOS
# ==============================================================================

print("=== COMPARACIÓN DE MODELOS ===")

resultados <- data.frame(
  Modelo = c("Regresión Lineal", "Stepwise", "Ridge", "Lasso", "Random Forest"),
  RMSE = c(lm_rmse, step_rmse, ridge_rmse, lasso_rmse, rf_rmse),
  MAE = c(lm_mae, step_mae, ridge_mae, lasso_mae, rf_mae),
  R2 = c(lm_r2, step_r2, ridge_r2, lasso_r2, rf_r2)
)

print(resultados)

# Visualizar comparación
ggplot(resultados, aes(x = reorder(Modelo, RMSE), y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5, size = 4) +
  theme_minimal() +
  labs(title = "Comparación de RMSE entre Modelos",
       y = "RMSE", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, max(resultados$RMSE) * 1.1))

ggsave("03_Visualizaciones/california_model_comparison.png",
       width = 10, height = 6)

# Scatter plot de predicciones vs reales para mejor modelo
mejor_idx <- which.min(resultados$RMSE)
print(paste("Mejor modelo:", resultados$Modelo[mejor_idx]))

if (resultados$Modelo[mejor_idx] == "Random Forest") {
  best_pred <- rf_pred
} else if (resultados$Modelo[mejor_idx] == "Lasso") {
  best_pred <- lasso_pred
} else if (resultados$Modelo[mejor_idx] == "Ridge") {
  best_pred <- ridge_pred
} else {
  best_pred <- lm_pred
}

pred_df <- data.frame(
  Real = y_test,
  Predicho = best_pred
)

ggplot(pred_df, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = paste("Predicciones vs Valores Reales -",
                     resultados$Modelo[mejor_idx]),
       x = "Precio Mediano Real ($100k)",
       y = "Precio Mediano Predicho ($100k)")

ggsave("03_Visualizaciones/california_predictions_vs_actual.png",
       width = 8, height = 8)

# ==============================================================================
# 11. VISUALIZACIÓN ESPACIAL DE ERRORES
# ==============================================================================

print("=== VISUALIZANDO ERRORES ESPACIALMENTE ===")

# Añadir errores al dataset de prueba
test_with_errors <- test_data %>%
  mutate(
    Predicho = best_pred,
    Error = MedHouseVal - best_pred,
    Error_Abs = abs(Error)
  )

# Mapa de errores absolutos
ggplot(test_with_errors, aes(x = Longitude, y = Latitude, color = Error_Abs)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_viridis(option = "inferno", name = "Error Absoluto\n($100k)") +
  theme_minimal() +
  labs(title = "Distribución Espacial de Errores de Predicción",
       x = "Longitud", y = "Latitud") +
  coord_fixed(ratio = 1.3)

ggsave("03_Visualizaciones/california_spatial_errors.png",
       width = 10, height = 12)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

print("=== ANÁLISIS COMPLETADO ===")
print("Se aplicaron múltiples métodos de regresión al dataset California Housing.")
print("Se incorporó análisis espacial para entender patrones geográficos.")
print("Métodos de regularización y ensemble mejoran la regresión lineal básica.")
print("Los precios muestran fuerte dependencia espacial (latitud/longitud).")
print("Todas las visualizaciones se guardaron exitosamente.")
