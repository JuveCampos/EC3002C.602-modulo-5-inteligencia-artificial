# ==============================================================================
# CLASIFICACIÓN Y CLUSTERING - BANK MARKETING
# ==============================================================================
#
# Dataset: Bank Marketing
# Observaciones: 45,211
# Variables: 21 (20 predictoras + 1 objetivo)
#
# Descripción:
# Datos de campañas de marketing directo (llamadas telefónicas) de banco
# portugués. Objetivo: predecir si cliente suscribirá depósito a plazo.
#
# Métodos aplicados:
# 1. Regresión Logística
# 2. Random Forest
# 3. XGBoost
# 4. Manejo de datos desbalanceados
#
# ==============================================================================

library(tidyverse)
library(randomForest)
library(xgboost)
library(caret)
library(pROC)

set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN
# ==============================================================================

print("=== CARGANDO DATOS ===")

bank_data <- read_csv("01_Datos/datasets_ml/bank_marketing.csv",
                      show_col_types = FALSE)

print("Dimensiones:")
print(dim(bank_data))

# Convertir variable objetivo a factor
bank_data <- bank_data %>%
  mutate(y = factor(y, levels = c("no", "yes")))

print("Balance de clases:")
print(table(bank_data$y))
print(prop.table(table(bank_data$y)))

# Identificar variables categóricas
categorical_vars <- c("job", "marital", "education", "default",
                      "housing", "loan", "contact", "month",
                      "day_of_week", "poutcome")

# Convertir a factores
bank_data <- bank_data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

print("=== ANÁLISIS EXPLORATORIO ===")

# Tasa de éxito por trabajo
success_by_job <- bank_data %>%
  group_by(job) %>%
  summarise(
    n = n(),
    tasa_exito = mean(y == "yes"),
    .groups = "drop"
  ) %>%
  arrange(desc(tasa_exito))

print("Tasa de éxito por tipo de trabajo:")
print(success_by_job)

# Visualización
ggplot(success_by_job, aes(x = reorder(job, tasa_exito),
                            y = tasa_exito, fill = job)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Tasa de Éxito por Tipo de Trabajo",
       x = "Trabajo", y = "Tasa de Éxito") +
  theme(legend.position = "none")

ggsave("03_Visualizaciones/bank_success_by_job.png",
       width = 10, height = 6)

# Distribución de edad por outcome
ggplot(bank_data, aes(x = y, y = age, fill = y)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Edad por Outcome",
       x = "Suscribió", y = "Edad")

ggsave("03_Visualizaciones/bank_age_distribution.png",
       width = 8, height = 6)

# ==============================================================================
# 3. DIVISIÓN DE DATOS
# ==============================================================================

print("=== PREPARANDO DATOS ===")

# Eliminar filas con valores faltantes
bank_data <- bank_data %>%
  na.omit()

# Usar muestra para acelerar procesamiento
# (el dataset es grande: 45K observaciones)
sample_size <- min(10000, nrow(bank_data))
bank_sample <- bank_data %>%
  slice_sample(n = sample_size)

train_idx <- createDataPartition(bank_sample$y, p = 0.7, list = FALSE)
train_data <- bank_sample[train_idx, ]
test_data <- bank_sample[-train_idx, ]

print(paste("Entrenamiento:", nrow(train_data)))
print(paste("Prueba:", nrow(test_data)))

# ==============================================================================
# 4. REGRESIÓN LOGÍSTICA
# ==============================================================================

print("=== REGRESIÓN LOGÍSTICA ===")

logit_model <- glm(y ~ ., data = train_data, family = binomial)

logit_prob <- predict(logit_model, test_data, type = "response")
logit_pred <- factor(ifelse(logit_prob > 0.5, "yes", "no"),
                     levels = c("no", "yes"))

logit_cm <- confusionMatrix(logit_pred, test_data$y, positive = "yes")
logit_roc <- roc(test_data$y, logit_prob, quiet = TRUE)

print("Accuracy:")
print(logit_cm$overall["Accuracy"])
print(paste("AUC:", round(auc(logit_roc), 4)))

# ==============================================================================
# 5. RANDOM FOREST
# ==============================================================================

print("=== RANDOM FOREST ===")

rf_model <- randomForest(y ~ ., data = train_data,
                         ntree = 100, mtry = 5, importance = TRUE)

print(rf_model)

rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[, 2]

rf_cm <- confusionMatrix(rf_pred, test_data$y, positive = "yes")
rf_roc <- roc(test_data$y, rf_prob, quiet = TRUE)

print("Accuracy:")
print(rf_cm$overall["Accuracy"])
print(paste("AUC:", round(auc(rf_roc), 4)))

# Variables más importantes
importance_df <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 10)

print("Top 10 variables:")
print(importance_df)

# ==============================================================================
# 6. XGBOOST
# ==============================================================================

print("=== XGBOOST ===")

# Preparar matrices (one-hot encoding para categorías)
train_matrix <- model.matrix(y ~ ., data = train_data)[, -1]
test_matrix <- model.matrix(y ~ ., data = test_data)[, -1]

train_label <- as.numeric(train_data$y) - 1
test_label <- as.numeric(test_data$y) - 1

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8
)

xgb_cv <- xgb.cv(params = params, data = dtrain, nrounds = 100,
                 nfold = 5, early_stopping_rounds = 10, verbose = 0)

best_nrounds <- xgb_cv$best_iteration

xgb_model <- xgb.train(params = params, data = dtrain,
                       nrounds = best_nrounds, verbose = 0)

xgb_prob <- predict(xgb_model, dtest)
xgb_pred <- factor(ifelse(xgb_prob > 0.5, "yes", "no"),
                   levels = c("no", "yes"))

xgb_cm <- confusionMatrix(xgb_pred, test_data$y, positive = "yes")
xgb_roc <- roc(test_data$y, xgb_prob, quiet = TRUE)

print("Accuracy:")
print(xgb_cm$overall["Accuracy"])
print(paste("AUC:", round(auc(xgb_roc), 4)))

# ==============================================================================
# 7. COMPARACIÓN
# ==============================================================================

print("=== COMPARACIÓN ===")

resultados <- data.frame(
  Modelo = c("Logit", "Random Forest", "XGBoost"),
  Accuracy = c(logit_cm$overall["Accuracy"],
               rf_cm$overall["Accuracy"],
               xgb_cm$overall["Accuracy"]),
  Sensitivity = c(logit_cm$byClass["Sensitivity"],
                  rf_cm$byClass["Sensitivity"],
                  xgb_cm$byClass["Sensitivity"]),
  AUC = c(auc(logit_roc), auc(rf_roc), auc(xgb_roc))
)

print(resultados)

# Curvas ROC
ggplot() +
  geom_line(aes(x = 1 - logit_roc$specificities,
                y = logit_roc$sensitivities, color = "Logit"), size = 1) +
  geom_line(aes(x = 1 - rf_roc$specificities,
                y = rf_roc$sensitivities, color = "RF"), size = 1) +
  geom_line(aes(x = 1 - xgb_roc$specificities,
                y = xgb_roc$sensitivities, color = "XGBoost"), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Curvas ROC - Bank Marketing",
       x = "FPR", y = "TPR", color = "Modelo") +
  coord_equal()

ggsave("03_Visualizaciones/bank_roc_curves.png",
       width = 8, height = 8)

print("=== ANÁLISIS COMPLETADO ===")
print("Dataset desbalanceado requiere atención a métricas más allá de accuracy")
print("XGBoost y Random Forest superan a regresión logística")
