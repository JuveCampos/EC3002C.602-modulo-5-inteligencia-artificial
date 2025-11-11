# ==============================================================================
# ANÁLISIS DE REGRESIÓN Y REGULARIZACIÓN - WINE QUALITY (RED)
# ==============================================================================
#
# Dataset: Wine Quality (Vino Tinto)
# Observaciones: 1,599
# Variables: 12 (11 predictoras + 1 objetivo)
#
# Descripción:
# Dataset relacionado con vinos tintos portugueses (vinho verde). Contiene
# propiedades fisicoquímicas y una calificación de calidad (score 0-10).
#
# Métodos aplicados:
# 1. Regresión lineal múltiple
# 2. Stepwise Regression (selección de variables)
# 3. Ridge Regression (regularización L2)
# 4. Lasso Regression (regularización L1)
# 5. Random Forest para regresión
# 6. XGBoost para regresión
#
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación y visualización de datos
library(glmnet)       # Ridge y Lasso
library(MASS)         # Stepwise regression
library(dplyr)        # Recargar dplyr para evitar conflicto con MASS::select
library(randomForest) # Random Forest
library(xgboost)      # XGBoost
library(caret)        # Evaluación de modelos
library(corrplot)     # Correlaciones

# Establecer semilla para reproducibilidad
set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

print("=== CARGANDO DATOS ===")

# Cargar datos
wine_data <- read_delim("01_Datos/datasets_ml/winequality-red.csv",
                        delim = ";",
                        show_col_types = FALSE)

# Limpiar nombres de columnas (eliminar espacios y caracteres especiales)
colnames(wine_data) <- make.names(colnames(wine_data))

print("Dimensiones del dataset:")
print(dim(wine_data))

print("Primeras observaciones:")
print(head(wine_data))

print("Resumen estadístico:")
print(summary(wine_data))

# Verificar valores faltantes
print("Valores faltantes por variable:")
print(colSums(is.na(wine_data)))

# Distribución de la variable objetivo
print("Distribución de calidad:")
print(table(wine_data$quality))

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

print("=== ANÁLISIS EXPLORATORIO ===")

# Histograma de calidad
ggplot(wine_data, aes(x = quality)) +
  geom_histogram(binwidth = 1, fill = "darkred", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Calidad del Vino Tinto",
       x = "Calidad", y = "Frecuencia")

ggsave("03_Visualizaciones/wine_red_quality_dist.png",
       width = 8, height = 6)

# Matriz de correlación
correlation_matrix <- cor(wine_data)

png("03_Visualizaciones/wine_red_correlation.png",
    width = 1200, height = 1000)
corrplot(correlation_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Matriz de Correlación - Vino Tinto",
         mar = c(0, 0, 2, 0))
dev.off()

# Identificar correlaciones altas con calidad
correlaciones_quality <- correlation_matrix[, "quality"] %>%
  as.data.frame() %>%
  rename(correlacion = ".") %>%
  arrange(desc(abs(correlacion)))

print("Correlaciones con calidad:")
print(correlaciones_quality)

# Boxplots de variables clave por nivel de calidad
wine_data %>%
  dplyr::select(alcohol, volatile.acidity, sulphates, quality) %>%
  pivot_longer(cols = -quality, names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = as.factor(quality), y = valor, fill = as.factor(quality))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Variables clave por nivel de calidad",
       x = "Calidad", y = "Valor") +
  theme(legend.position = "none")

ggsave("03_Visualizaciones/wine_red_boxplots.png",
       width = 12, height = 8)

# ==============================================================================
# 3. DIVISIÓN DE DATOS
# ==============================================================================

print("=== DIVIDIENDO DATOS ===")

# División 70-30
train_index <- createDataPartition(wine_data$quality, p = 0.7, list = FALSE)

train_data <- wine_data[train_index, ]
test_data <- wine_data[-train_index, ]

print(paste("Observaciones de entrenamiento:", nrow(train_data)))
print(paste("Observaciones de prueba:", nrow(test_data)))

# ==============================================================================
# 4. REGRESIÓN LINEAL MÚLTIPLE
# ==============================================================================

print("=== REGRESIÓN LINEAL MÚLTIPLE ===")

# Entrenar modelo de regresión lineal
lm_model <- lm(quality ~ ., data = train_data)

print("Resumen del modelo:")
print(summary(lm_model))

# Predicciones
lm_pred <- predict(lm_model, test_data)

# Métricas de evaluación
lm_rmse <- sqrt(mean((test_data$quality - lm_pred)^2))
lm_mae <- mean(abs(test_data$quality - lm_pred))
lm_r2 <- cor(test_data$quality, lm_pred)^2

print(paste("RMSE:", round(lm_rmse, 4)))
print(paste("MAE:", round(lm_mae, 4)))
print(paste("R²:", round(lm_r2, 4)))

# ==============================================================================
# 5. STEPWISE REGRESSION
# ==============================================================================
#
# TEORÍA: STEPWISE REGRESSION
#
# Stepwise regression es un método de selección de variables que construye
# modelos iterativamente añadiendo o eliminando variables basándose en
# criterios estadísticos (típicamente AIC - Akaike Information Criterion).
#
# Tipos:
# - Forward: Empieza sin variables y añade una a la vez
# - Backward: Empieza con todas las variables y elimina una a la vez
# - Both: Combinación de forward y backward
#
# Criterio AIC:
# AIC = -2*log(likelihood) + 2*k
# donde k = número de parámetros
# Menor AIC indica mejor modelo (balance entre ajuste y complejidad)
#
# Ventajas:
# - Automatiza la selección de variables
# - Reduce multicolinealidad
# - Simplifica modelos complejos
#
# Desventajas:
# - Puede ser inestable con cambios en datos
# - No garantiza el mejor modelo posible
# - Puede descartar variables importantes con correlaciones complejas
#
# ==============================================================================

print("=== STEPWISE REGRESSION ===")

# Modelo nulo (solo intercepto)
null_model <- lm(quality ~ 1, data = train_data)

# Modelo completo
full_model <- lm(quality ~ ., data = train_data)

# Stepwise selection (both directions)
stepwise_model <- stepAIC(null_model,
                          scope = list(lower = null_model,
                                      upper = full_model),
                          direction = "both",
                          trace = FALSE)

print("Variables seleccionadas por stepwise:")
print(names(coef(stepwise_model)))

print("Resumen del modelo stepwise:")
print(summary(stepwise_model))

# Predicciones
stepwise_pred <- predict(stepwise_model, test_data)

stepwise_rmse <- sqrt(mean((test_data$quality - stepwise_pred)^2))
stepwise_mae <- mean(abs(test_data$quality - stepwise_pred))
stepwise_r2 <- cor(test_data$quality, stepwise_pred)^2

print(paste("RMSE:", round(stepwise_rmse, 4)))
print(paste("MAE:", round(stepwise_mae, 4)))
print(paste("R²:", round(stepwise_r2, 4)))

# ==============================================================================
# 6. RIDGE REGRESSION (Regularización L2)
# ==============================================================================
#
# TEORÍA: RIDGE REGRESSION
#
# Ridge regression añade una penalización L2 a la función de costo de
# regresión lineal para prevenir sobreajuste y manejar multicolinealidad.
#
# Función de costo Ridge:
# L = Σ(yi - ŷi)² + λ * Σ(βj²)
#
# donde:
# - Primera parte: Error cuadrático medio (como en regresión lineal)
# - Segunda parte: Penalización L2 (suma de cuadrados de coeficientes)
# - λ (lambda): Parámetro de regularización que controla la penalización
#
# Características:
# - λ = 0: Equivalente a regresión lineal ordinaria
# - λ → ∞: Todos los coeficientes → 0
# - Ridge REDUCE coeficientes pero NO los hace exactamente cero
# - Útil cuando hay multicolinealidad entre predictores
#
# Ventajas:
# - Estabiliza estimaciones en presencia de multicolinealidad
# - Reduce varianza del modelo (menos sobreajuste)
# - Siempre tiene solución única
#
# Desventajas:
# - No hace selección de variables (todos los predictores permanecen)
# - Más difícil de interpretar que regresión lineal
# - Requiere normalización de variables
#
# ==============================================================================

print("=== RIDGE REGRESSION ===")

# Preparar matrices para glmnet
# glmnet requiere matriz de predictores y vector de respuesta
x_train <- model.matrix(quality ~ ., train_data)[, -1]
y_train <- train_data$quality

x_test <- model.matrix(quality ~ ., test_data)[, -1]
y_test <- test_data$quality

# Validación cruzada para encontrar mejor lambda
# cv.glmnet usa validación cruzada de 10 pliegues por defecto
# alpha = 0 indica Ridge regression
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)

print("Lambda óptimo (mínimo error CV):")
print(ridge_cv$lambda.min)

print("Lambda 1se (parsimonia):")
print(ridge_cv$lambda.1se)

# Visualizar validación cruzada
png("03_Visualizaciones/wine_red_ridge_cv.png",
    width = 1000, height = 600)
plot(ridge_cv, main = "Validación Cruzada - Ridge Regression")
dev.off()

# Entrenar modelo final con lambda óptimo
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = ridge_cv$lambda.min)

print("Coeficientes Ridge:")
print(coef(ridge_model))

# Predicciones
ridge_pred <- predict(ridge_model, x_test)

ridge_rmse <- sqrt(mean((y_test - ridge_pred)^2))
ridge_mae <- mean(abs(y_test - ridge_pred))
ridge_r2 <- cor(y_test, ridge_pred)^2

print(paste("RMSE:", round(ridge_rmse, 4)))
print(paste("MAE:", round(ridge_mae, 4)))
print(paste("R²:", round(ridge_r2, 4)))

# ==============================================================================
# 7. LASSO REGRESSION (Regularización L1)
# ==============================================================================
#
# TEORÍA: LASSO REGRESSION
#
# LASSO (Least Absolute Shrinkage and Selection Operator) usa penalización L1
# que puede forzar coeficientes exactamente a cero, realizando así selección
# de variables automáticamente.
#
# Función de costo Lasso:
# L = Σ(yi - ŷi)² + λ * Σ|βj|
#
# donde:
# - Primera parte: Error cuadrático medio
# - Segunda parte: Penalización L1 (suma de valores absolutos de coeficientes)
# - λ: Parámetro de regularización
#
# Diferencias con Ridge:
# - Lasso usa valor absoluto (L1), Ridge usa cuadrado (L2)
# - Lasso puede hacer coeficientes exactamente = 0 (selección de variables)
# - Ridge solo reduce coeficientes cerca de 0 pero no los elimina
#
# Interpretación geométrica:
# - Ridge: restricción esférica en espacio de parámetros
# - Lasso: restricción romboidal que favorece soluciones en esquinas (ceros)
#
# Ventajas:
# - Selección automática de variables
# - Produce modelos más interpretables (sparse)
# - Útil con muchas variables irrelevantes
#
# Desventajas:
# - Si variables están correlacionadas, selecciona una arbitrariamente
# - Puede ser inestable con correlaciones altas
# - Máximo de n variables seleccionadas (n = observaciones)
#
# ==============================================================================

print("=== LASSO REGRESSION ===")

# Validación cruzada para encontrar mejor lambda
# alpha = 1 indica Lasso regression
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

print("Lambda óptimo (mínimo error CV):")
print(lasso_cv$lambda.min)

print("Lambda 1se (parsimonia):")
print(lasso_cv$lambda.1se)

# Visualizar validación cruzada
png("03_Visualizaciones/wine_red_lasso_cv.png",
    width = 1000, height = 600)
plot(lasso_cv, main = "Validación Cruzada - Lasso Regression")
dev.off()

# Entrenar modelo final con lambda óptimo
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_cv$lambda.min)

print("Coeficientes Lasso:")
lasso_coefs <- coef(lasso_model)
print(lasso_coefs)

# Contar variables seleccionadas (coeficientes no cero)
n_selected <- sum(lasso_coefs[-1] != 0)
print(paste("Número de variables seleccionadas por Lasso:", n_selected))

# Predicciones
lasso_pred <- predict(lasso_model, x_test)

lasso_rmse <- sqrt(mean((y_test - lasso_pred)^2))
lasso_mae <- mean(abs(y_test - lasso_pred))
lasso_r2 <- cor(y_test, lasso_pred)^2

print(paste("RMSE:", round(lasso_rmse, 4)))
print(paste("MAE:", round(lasso_mae, 4)))
print(paste("R²:", round(lasso_r2, 4)))

# Visualizar path de coeficientes
png("03_Visualizaciones/wine_red_lasso_path.png",
    width = 1000, height = 600)
plot(glmnet(x_train, y_train, alpha = 1),
     xvar = "lambda",
     main = "Path de Coeficientes Lasso")
abline(v = log(lasso_cv$lambda.min), lty = 2, col = "red")
dev.off()

# ==============================================================================
# 8. RANDOM FOREST PARA REGRESIÓN
# ==============================================================================
#
# TEORÍA: RANDOM FOREST
#
# Random Forest es un método ensemble que combina múltiples árboles de
# decisión para mejorar la predicción y reducir el sobreajuste.
#
# Funcionamiento:
# 1. Bootstrap Aggregating (Bagging): Crea múltiples muestras bootstrap
#    del dataset original
# 2. Para cada muestra, entrena un árbol de decisión
# 3. En cada división del árbol, solo considera un subconjunto aleatorio
#    de variables (√p para clasificación, p/3 para regresión)
# 4. Predicción final: promedio de predicciones de todos los árboles
#
# Parámetros clave:
# - ntree: número de árboles (más árboles = más estable, pero más lento)
# - mtry: número de variables consideradas en cada división
# - nodesize: tamaño mínimo de nodos terminales
#
# Ventajas:
# - Alta precisión predictiva
# - Robusto a outliers y ruido
# - Maneja relaciones no lineales y complejas
# - Proporciona importancia de variables
# - Poco sensible a hiperparámetros
#
# Desventajas:
# - Menos interpretable que árboles simples
# - Computacionalmente intensivo
# - Puede sobreajustar en datasets con mucho ruido
#
# ==============================================================================

print("=== RANDOM FOREST ===")

# Entrenar Random Forest
# ntree = 500 árboles
# mtry = número de variables a considerar en cada división
rf_model <- randomForest(quality ~ .,
                         data = train_data,
                         ntree = 500,
                         mtry = 4,
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
png("03_Visualizaciones/wine_red_rf_importance.png",
    width = 1000, height = 800)
varImpPlot(rf_model,
           main = "Importancia de Variables - Random Forest")
dev.off()

# Predicciones
rf_pred <- predict(rf_model, test_data)

rf_rmse <- sqrt(mean((test_data$quality - rf_pred)^2))
rf_mae <- mean(abs(test_data$quality - rf_pred))
rf_r2 <- cor(test_data$quality, rf_pred)^2

print(paste("RMSE:", round(rf_rmse, 4)))
print(paste("MAE:", round(rf_mae, 4)))
print(paste("R²:", round(rf_r2, 4)))

# ==============================================================================
# 9. XGBOOST
# ==============================================================================
#
# TEORÍA: XGBOOST (eXtreme Gradient Boosting)
#
# XGBoost es una implementación optimizada de gradient boosting, un método
# ensemble que construye árboles secuencialmente, donde cada árbol corrige
# los errores del anterior.
#
# Funcionamiento:
# 1. Inicia con un modelo simple (constante)
# 2. Calcula los residuos (errores)
# 3. Entrena un nuevo árbol para predecir estos residuos
# 4. Añade el nuevo árbol al modelo con un peso (learning rate)
# 5. Repite pasos 2-4 hasta alcanzar número de árboles deseado
#
# Diferencias con Random Forest:
# - RF: árboles independientes en paralelo
# - XGBoost: árboles secuenciales que aprenden de errores previos
# - RF: bagging (bootstrap + promedio)
# - XGBoost: boosting (corrección iterativa)
#
# Parámetros clave:
# - nrounds: número de árboles (iteraciones)
# - max_depth: profundidad máxima de árboles
# - eta (learning rate): tasa de aprendizaje (típicamente 0.01-0.3)
# - subsample: fracción de datos para cada árbol
# - colsample_bytree: fracción de variables para cada árbol
#
# Regularización en XGBoost:
# - lambda (L2): penalización ridge en pesos de hojas
# - alpha (L1): penalización lasso en pesos de hojas
# - gamma: reducción mínima de pérdida para hacer división
#
# Ventajas:
# - Excelente rendimiento predictivo (gana muchas competiciones Kaggle)
# - Maneja missing values automáticamente
# - Regularización incorporada previene sobreajuste
# - Rápido y eficiente (paralelización, cache-aware)
# - Proporciona importancia de variables
#
# Desventajas:
# - Muchos hiperparámetros para tunear
# - Propenso a sobreajuste si no se configura bien
# - Menos interpretable
# - Sensible a outliers
#
# ==============================================================================

print("=== XGBOOST ===")

# Preparar datos en formato DMatrix (formato optimizado de XGBoost)
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Definir parámetros
params <- list(
  objective = "reg:squarederror",  # regresión
  eta = 0.1,                       # learning rate
  max_depth = 6,                   # profundidad máxima
  subsample = 0.8,                 # 80% de datos para cada árbol
  colsample_bytree = 0.8          # 80% de variables para cada árbol
)

# Validación cruzada para encontrar número óptimo de árboles
print("Realizando validación cruzada...")
xgb_cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 20,
  verbose = 0
)

best_nrounds <- xgb_cv$best_iteration
print(paste("Número óptimo de árboles:", best_nrounds))

# Entrenar modelo final
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

print("Modelo XGBoost entrenado")

# Importancia de variables
importance_matrix <- xgb.importance(
  feature_names = colnames(x_train),
  model = xgb_model
)

print("Importancia de variables:")
print(importance_matrix)

# Visualizar importancia
png("03_Visualizaciones/wine_red_xgb_importance.png",
    width = 1000, height = 800)
xgb.plot.importance(importance_matrix, top_n = 11,
                    main = "Importancia de Variables - XGBoost")
dev.off()

# Predicciones
xgb_pred <- predict(xgb_model, dtest)

xgb_rmse <- sqrt(mean((y_test - xgb_pred)^2))
xgb_mae <- mean(abs(y_test - xgb_pred))
xgb_r2 <- cor(y_test, xgb_pred)^2

print(paste("RMSE:", round(xgb_rmse, 4)))
print(paste("MAE:", round(xgb_mae, 4)))
print(paste("R²:", round(xgb_r2, 4)))

# ==============================================================================
# 10. COMPARACIÓN DE MODELOS
# ==============================================================================

print("=== COMPARACIÓN DE TODOS LOS MODELOS ===")

# Crear dataframe con resultados
resultados <- data.frame(
  Modelo = c("Regresión Lineal", "Stepwise", "Ridge", "Lasso",
             "Random Forest", "XGBoost"),
  RMSE = c(lm_rmse, stepwise_rmse, ridge_rmse, lasso_rmse, rf_rmse, xgb_rmse),
  MAE = c(lm_mae, stepwise_mae, ridge_mae, lasso_mae, rf_mae, xgb_mae),
  R2 = c(lm_r2, stepwise_r2, ridge_r2, lasso_r2, rf_r2, xgb_r2)
)

print(resultados)

# Visualizar comparación de RMSE
ggplot(resultados, aes(x = reorder(Modelo, RMSE), y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5, size = 4) +
  theme_minimal() +
  labs(title = "Comparación de RMSE entre Modelos",
       y = "RMSE", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, max(resultados$RMSE) * 1.1))

ggsave("03_Visualizaciones/wine_red_model_comparison.png",
       width = 10, height = 6)

# Visualizar predicciones vs valores reales para el mejor modelo
mejor_modelo_idx <- which.min(resultados$RMSE)
print(paste("Mejor modelo:", resultados$Modelo[mejor_modelo_idx]))

# Crear dataframe con predicciones del mejor modelo
if (resultados$Modelo[mejor_modelo_idx] == "XGBoost") {
  pred_best <- xgb_pred
} else if (resultados$Modelo[mejor_modelo_idx] == "Random Forest") {
  pred_best <- rf_pred
} else {
  pred_best <- lasso_pred
}

pred_df <- data.frame(
  Real = y_test,
  Predicho = pred_best
)

ggplot(pred_df, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.6, color = "darkred") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(title = paste("Predicciones vs Valores Reales -",
                     resultados$Modelo[mejor_modelo_idx]),
       x = "Calidad Real", y = "Calidad Predicha")

ggsave("03_Visualizaciones/wine_red_predictions_vs_actual.png",
       width = 8, height = 8)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

print("=== ANÁLISIS COMPLETADO ===")
print("Se aplicaron múltiples métodos de regresión al dataset Wine Quality Red.")
print("Métodos de regularización (Ridge/Lasso) mejoran el modelo lineal básico.")
print("Métodos ensemble (RF, XGBoost) típicamente superan a modelos lineales.")
print("Todos los resultados y visualizaciones se guardaron exitosamente.")
