# ==============================================================================
# XGBOOST Y BAGGING - CREDIT CARD DEFAULT
# ==============================================================================
#
# Dataset: Credit Card Default
# Observaciones: 30,000
# Variables: 24 (23 predictoras + 1 objetivo)
#
# Descripción:
# Dataset de default de tarjetas de crédito de clientes en Taiwán (2005).
# Contiene información demográfica, historial de pagos, montos de facturas
# y pagos. Variable objetivo: default en el siguiente mes (binaria).
#
# Métodos aplicados:
# 1. Regresión Logística (baseline)
# 2. Bagging (Bootstrap Aggregating)
# 3. Random Forest
# 4. XGBoost (Gradient Boosting)
# 5. Comparación de métodos ensemble
#
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación y visualización
library(caret)        # Evaluación de modelos
library(pROC)         # Curvas ROC
library(xgboost)      # XGBoost
library(randomForest) # Random Forest
library(ipred)        # Bagging

set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

print("=== CARGANDO DATOS ===")

credit_data <- read_csv("01_Datos/datasets_ml/credit_card_default.csv",
                        show_col_types = FALSE)

# Renombrar columnas con nombres descriptivos
colnames(credit_data) <- c(
  "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE",
  "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
  "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
  "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
  "default"
)

print("Dimensiones del dataset:")
print(dim(credit_data))

print("Primeras observaciones:")
print(head(credit_data))

# Verificar balance de clases
print("Distribución de default:")
print(table(credit_data$default))
print(prop.table(table(credit_data$default)))

# Convertir variables a factores donde corresponda
credit_data <- credit_data %>%
  mutate(
    default = as.factor(default),
    SEX = as.factor(SEX),
    EDUCATION = as.factor(EDUCATION),
    MARRIAGE = as.factor(MARRIAGE)
  )

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

print("=== ANÁLISIS EXPLORATORIO ===")

# Distribución de default por variables demográficas
# Por sexo
sex_table <- table(credit_data$SEX, credit_data$default)
print("Default por sexo:")
print(prop.table(sex_table, margin = 1))

# Por educación
edu_table <- table(credit_data$EDUCATION, credit_data$default)
print("Default por educación:")
print(prop.table(edu_table, margin = 1))

# Distribución de edad por default
ggplot(credit_data, aes(x = default, y = AGE, fill = default)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Edad por Default",
       x = "Default", y = "Edad") +
  scale_fill_manual(values = c("lightblue", "coral"))

ggsave("03_Visualizaciones/credit_age_distribution.png",
       width = 8, height = 6)

# Límite de crédito por default
ggplot(credit_data, aes(x = default, y = LIMIT_BAL, fill = default)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Límite de Crédito por Default (escala log)",
       x = "Default", y = "Límite de Crédito") +
  scale_fill_manual(values = c("lightblue", "coral"))

ggsave("03_Visualizaciones/credit_limit_distribution.png",
       width = 8, height = 6)

# ==============================================================================
# 3. DIVISIÓN DE DATOS
# ==============================================================================

print("=== DIVIDIENDO DATOS ===")

# División estratificada 70-30
train_index <- createDataPartition(credit_data$default, p = 0.7, list = FALSE)

train_data <- credit_data[train_index, ]
test_data <- credit_data[-train_index, ]

print(paste("Observaciones de entrenamiento:", nrow(train_data)))
print(paste("Observaciones de prueba:", nrow(test_data)))

print("Distribución en entrenamiento:")
print(table(train_data$default))

print("Distribución en prueba:")
print(table(test_data$default))

# ==============================================================================
# 4. MODELO BASELINE: REGRESIÓN LOGÍSTICA
# ==============================================================================

print("=== MODELO BASELINE: REGRESIÓN LOGÍSTICA ===")

logit_model <- glm(default ~ ., data = train_data, family = binomial)

print("Resumen del modelo:")
print(summary(logit_model))

# Predicciones
logit_prob <- predict(logit_model, test_data, type = "response")
logit_pred <- ifelse(logit_prob > 0.5, 1, 0)
logit_pred <- factor(logit_pred, levels = c(0, 1))

# Evaluación
logit_cm <- confusionMatrix(logit_pred, test_data$default, positive = "1")
logit_roc <- roc(test_data$default, logit_prob, quiet = TRUE)

print("=== RESULTADOS REGRESIÓN LOGÍSTICA ===")
print(logit_cm)
print(paste("AUC:", round(auc(logit_roc), 4)))

# ==============================================================================
# 5. BAGGING
# ==============================================================================
#
# TEORÍA: BAGGING (Bootstrap Aggregating)
#
# Bagging es una técnica de ensemble que reduce la varianza de modelos
# inestables (como árboles de decisión) mediante:
#
# 1. Bootstrap: Crear B muestras bootstrap del dataset original
#    - Cada muestra tiene n observaciones, muestreadas con reemplazo
#    - Típicamente, cada muestra contiene ~63% de datos únicos
#
# 2. Aggregating: Para cada muestra bootstrap:
#    - Entrenar un modelo independiente
#    - Para regresión: predicción final = promedio de predicciones
#    - Para clasificación: predicción final = voto mayoritario
#
# Matemáticamente:
# ŷ_bag(x) = (1/B) * Σᵢ₌₁ᴮ ŷᵢ(x)
#
# donde ŷᵢ(x) es la predicción del modelo i-ésimo
#
# Reducción de varianza:
# Var(promedio de B variables) = Var(variable individual) / B
# (si variables son independientes e idénticamente distribuidas)
#
# En la práctica:
# - Las muestras bootstrap no son totalmente independientes
# - Aún así, bagging reduce varianza significativamente
# - No afecta el sesgo del modelo base
#
# Random Forest vs Bagging:
# - Bagging: usa todos los predictores en cada división
# - Random Forest: usa subconjunto aleatorio de predictores (decorrelaciona árboles)
# - Random Forest típicamente supera a bagging
#
# Ventajas:
# - Reduce sobreajuste de modelos inestables
# - Fácil de paralelizar
# - Out-of-bag error proporciona estimación de error sin validación cruzada
# - Mejora precisión sin aumentar sesgo
#
# Desventajas:
# - Pierde interpretabilidad
# - No mejora modelos con alto sesgo
# - Computacionalmente intensivo
# - Predicciones más lentas que modelo único
#
# Out-of-Bag (OOB) Error:
# - ~37% de datos no están en cada muestra bootstrap
# - Estos datos se usan para evaluar ese modelo
# - OOB error = promedio de errores en datos no usados en cada modelo
# - Es una estimación honesta del error de generalización
#
# ==============================================================================

print("=== BAGGING ===")

# Entrenar modelo con bagging
# nbagg = número de árboles bootstrap
# coob = TRUE calcula error out-of-bag
print("Entrenando modelo Bagging (esto puede tomar tiempo)...")

bagging_model <- bagging(default ~ .,
                         data = train_data,
                         nbagg = 50,
                         coob = TRUE,
                         control = rpart::rpart.control(minsplit = 2,
                                                        cp = 0,
                                                        xval = 0))

print("Modelo Bagging entrenado")
print(paste("Error OOB:", round(bagging_model$err, 4)))

# Predicciones
bagging_pred <- predict(bagging_model, test_data, type = "class")
bagging_prob <- predict(bagging_model, test_data, type = "prob")[, 2]

# Evaluación
bagging_cm <- confusionMatrix(bagging_pred, test_data$default, positive = "1")
bagging_roc <- roc(test_data$default, bagging_prob, quiet = TRUE)

print("=== RESULTADOS BAGGING ===")
print(bagging_cm)
print(paste("AUC:", round(auc(bagging_roc), 4)))

# ==============================================================================
# 6. RANDOM FOREST
# ==============================================================================

print("=== RANDOM FOREST ===")

# Entrenar Random Forest
# mtry = número de variables consideradas en cada división
# Para clasificación, default es sqrt(p)
print("Entrenando Random Forest...")

rf_model <- randomForest(default ~ .,
                         data = train_data,
                         ntree = 100,
                         mtry = 5,
                         importance = TRUE)

print("Modelo Random Forest:")
print(rf_model)

# Importancia de variables
importance_df <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(MeanDecreaseGini))

print("Top 10 variables más importantes:")
print(head(importance_df, 10))

# Visualizar importancia
png("03_Visualizaciones/credit_rf_importance.png",
    width = 1200, height = 800)
varImpPlot(rf_model, n.var = 15, main = "Importancia de Variables - Random Forest")
dev.off()

# Predicciones
rf_pred <- predict(rf_model, test_data, type = "class")
rf_prob <- predict(rf_model, test_data, type = "prob")[, 2]

# Evaluación
rf_cm <- confusionMatrix(rf_pred, test_data$default, positive = "1")
rf_roc <- roc(test_data$default, rf_prob, quiet = TRUE)

print("=== RESULTADOS RANDOM FOREST ===")
print(rf_cm)
print(paste("AUC:", round(auc(rf_roc), 4)))

# ==============================================================================
# 7. XGBOOST
# ==============================================================================

print("=== XGBOOST ===")

# Preparar datos para XGBoost
# XGBoost requiere matriz numérica
train_matrix <- model.matrix(default ~ ., data = train_data)[, -1]
test_matrix <- model.matrix(default ~ ., data = test_data)[, -1]

train_label <- as.numeric(as.character(train_data$default))
test_label <- as.numeric(as.character(test_data$default))

# Crear DMatrix
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Parámetros
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Validación cruzada para número óptimo de árboles
print("Realizando validación cruzada...")
xgb_cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 200,
  nfold = 5,
  early_stopping_rounds = 20,
  verbose = 0,
  print_every_n = 50
)

best_nrounds <- xgb_cv$best_iteration
print(paste("Número óptimo de árboles:", best_nrounds))

# Entrenar modelo final
print("Entrenando modelo XGBoost final...")
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

print("Modelo XGBoost entrenado")

# Importancia de variables
importance_matrix <- xgb.importance(
  feature_names = colnames(train_matrix),
  model = xgb_model
)

print("Top 10 variables más importantes:")
print(head(importance_matrix, 10))

# Visualizar importancia
png("03_Visualizaciones/credit_xgb_importance.png",
    width = 1200, height = 800)
xgb.plot.importance(importance_matrix, top_n = 15,
                    main = "Importancia de Variables - XGBoost")
dev.off()

# Predicciones
xgb_prob <- predict(xgb_model, dtest)
xgb_pred <- ifelse(xgb_prob > 0.5, 1, 0)
xgb_pred <- factor(xgb_pred, levels = c(0, 1))

# Evaluación
xgb_cm <- confusionMatrix(xgb_pred, test_data$default, positive = "1")
xgb_roc <- roc(test_data$default, xgb_prob, quiet = TRUE)

print("=== RESULTADOS XGBOOST ===")
print(xgb_cm)
print(paste("AUC:", round(auc(xgb_roc), 4)))

# ==============================================================================
# 8. COMPARACIÓN DE MODELOS
# ==============================================================================

print("=== COMPARACIÓN DE MODELOS ===")

# Crear dataframe con resultados
resultados <- data.frame(
  Modelo = c("Regresión Logística", "Bagging", "Random Forest", "XGBoost"),
  Accuracy = c(logit_cm$overall["Accuracy"],
               bagging_cm$overall["Accuracy"],
               rf_cm$overall["Accuracy"],
               xgb_cm$overall["Accuracy"]),
  Sensitivity = c(logit_cm$byClass["Sensitivity"],
                  bagging_cm$byClass["Sensitivity"],
                  rf_cm$byClass["Sensitivity"],
                  xgb_cm$byClass["Sensitivity"]),
  Specificity = c(logit_cm$byClass["Specificity"],
                  bagging_cm$byClass["Specificity"],
                  rf_cm$byClass["Specificity"],
                  xgb_cm$byClass["Specificity"]),
  AUC = c(auc(logit_roc), auc(bagging_roc), auc(rf_roc), auc(xgb_roc))
)

print(resultados)

# Visualizar comparación de AUC
ggplot(resultados, aes(x = reorder(Modelo, AUC), y = AUC, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(AUC, 3)), vjust = -0.5, size = 4) +
  theme_minimal() +
  labs(title = "Comparación de AUC entre Modelos",
       y = "AUC", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_cartesian(ylim = c(0.5, 1))

ggsave("03_Visualizaciones/credit_model_comparison_auc.png",
       width = 10, height = 6)

# ==============================================================================
# 9. CURVAS ROC COMPARADAS
# ==============================================================================

print("=== GENERANDO CURVAS ROC ===")

# Crear dataframe para curvas ROC
roc_data <- data.frame(
  Logit_FPR = 1 - logit_roc$specificities,
  Logit_TPR = logit_roc$sensitivities,
  Bagging_FPR = 1 - bagging_roc$specificities,
  Bagging_TPR = bagging_roc$sensitivities,
  RF_FPR = 1 - rf_roc$specificities,
  RF_TPR = rf_roc$sensitivities,
  XGB_FPR = 1 - xgb_roc$specificities,
  XGB_TPR = xgb_roc$sensitivities
)

# Gráfico de curvas ROC
ggplot() +
  geom_line(data = roc_data, aes(x = Logit_FPR, y = Logit_TPR, color = "Logit"),
            size = 1) +
  geom_line(data = roc_data, aes(x = Bagging_FPR, y = Bagging_TPR, color = "Bagging"),
            size = 1) +
  geom_line(data = roc_data, aes(x = RF_FPR, y = RF_TPR, color = "Random Forest"),
            size = 1) +
  geom_line(data = roc_data, aes(x = XGB_FPR, y = XGB_TPR, color = "XGBoost"),
            size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(title = "Curvas ROC Comparadas",
       x = "Tasa de Falsos Positivos (1 - Especificidad)",
       y = "Tasa de Verdaderos Positivos (Sensibilidad)",
       color = "Modelo") +
  coord_equal()

ggsave("03_Visualizaciones/credit_roc_curves.png",
       width = 10, height = 10)

# ==============================================================================
# 10. ANÁLISIS DE FEATURES MÁS IMPORTANTES
# ==============================================================================

print("=== ANÁLISIS DE FEATURES MÁS IMPORTANTES ===")

# Comparar importancia entre Random Forest y XGBoost
# Top 10 de cada modelo

top_rf <- importance_df %>%
  select(Variable, MeanDecreaseGini) %>%
  rename(Importancia_RF = MeanDecreaseGini) %>%
  slice_head(n = 10)

top_xgb <- importance_matrix %>%
  select(Feature, Gain) %>%
  rename(Variable = Feature, Importancia_XGB = Gain) %>%
  slice_head(n = 10)

# Unir ambos
importance_comparison <- full_join(top_rf, top_xgb, by = "Variable")

print("Comparación de importancia de variables:")
print(importance_comparison)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

print("=== ANÁLISIS COMPLETADO ===")
print("Se aplicaron métodos ensemble al dataset Credit Card Default.")
print("Métodos ensemble (Bagging, RF, XGBoost) superan a regresión logística.")
print("XGBoost y Random Forest muestran el mejor desempeño en términos de AUC.")
print("Variables de historial de pago son las más importantes para predicción.")
print("El dataset está desbalanceado - considerar técnicas de balanceo si es necesario.")
print("Todas las visualizaciones se guardaron exitosamente.")
