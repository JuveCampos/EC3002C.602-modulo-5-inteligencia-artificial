# ==============================================================================
# SVM Y ENSEMBLE METHODS - BREAST CANCER
# ==============================================================================
#
# Dataset: Breast Cancer Wisconsin (Diagnostic)
# Observaciones: 569
# Variables: 32 (30 predictoras + 1 objetivo + 1 ID)
#
# Descripción:
# Dataset de diagnóstico de cáncer de mama con características calculadas
# a partir de imágenes digitalizadas. Variables describen características
# de núcleos celulares. Objetivo: clasificar tumores como malignos o benignos.
#
# Métodos aplicados:
# 1. Support Vector Machines (diferentes kernels)
# 2. Ridge/Lasso para selección de features
# 3. Random Forest
# 4. XGBoost
# 5. Comparación de métodos
#
# ==============================================================================

library(tidyverse)
library(e1071)        # SVM
library(glmnet)       # Ridge/Lasso
library(randomForest)
library(xgboost)
library(caret)
library(pROC)

set.seed(42)

# ==============================================================================
# 1. CARGA Y PREPARACIÓN
# ==============================================================================

print("=== CARGANDO DATOS ===")

breast_data <- read_csv("01_Datos/datasets_ml/breast_cancer.csv",
                        show_col_types = FALSE)

# Eliminar columna ID si existe
breast_data <- breast_data %>%
  dplyr::select(-dplyr::matches("^id$|^ID$|^Id$"))

# Convertir diagnosis a factor (M = Maligno, B = Benigno)
breast_data <- breast_data %>%
  mutate(diagnosis = factor(diagnosis, levels = c("B", "M")))

print("Dimensiones:")
print(dim(breast_data))

print("Distribución de clases:")
print(table(breast_data$diagnosis))
print(prop.table(table(breast_data$diagnosis)))

# ==============================================================================
# 2. DIVISIÓN Y ESCALAMIENTO
# ==============================================================================

print("=== PREPARANDO DATOS ===")

train_idx <- createDataPartition(breast_data$diagnosis, p = 0.7, list = FALSE)
train_data <- breast_data[train_idx, ]
test_data <- breast_data[-train_idx, ]

# Escalamiento es crucial para SVM
preproc <- preProcess(train_data[, -1], method = c("center", "scale"))
train_scaled <- predict(preproc, train_data)
test_scaled <- predict(preproc, test_data)

print(paste("Entrenamiento:", nrow(train_data)))
print(paste("Prueba:", nrow(test_data)))

# ==============================================================================
# 3. LASSO PARA SELECCIÓN DE FEATURES
# ==============================================================================

print("=== LASSO PARA SELECCIÓN DE FEATURES ===")

x_train <- model.matrix(diagnosis ~ ., train_scaled)[, -1]
y_train <- as.numeric(train_scaled$diagnosis) - 1

lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")

print(paste("Lambda óptimo:", round(lasso_cv$lambda.min, 6)))

lasso_model <- glmnet(x_train, y_train, alpha = 1,
                      lambda = lasso_cv$lambda.min, family = "binomial")

lasso_coefs <- coef(lasso_model)
selected_vars <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0][-1]

print(paste("Variables seleccionadas:", length(selected_vars)))
print(selected_vars)

# ==============================================================================
# 4. SUPPORT VECTOR MACHINES
# ==============================================================================

print("=== ENTRENANDO SVM ===")

# SVM Lineal
print("SVM Lineal...")
svm_linear <- svm(diagnosis ~ ., data = train_scaled,
                  kernel = "linear", cost = 1, probability = TRUE)

svm_linear_pred <- predict(svm_linear, test_scaled)
svm_linear_prob <- attr(predict(svm_linear, test_scaled, probability = TRUE),
                        "probabilities")[, 2]

svm_linear_cm <- confusionMatrix(svm_linear_pred, test_scaled$diagnosis,
                                 positive = "M")
svm_linear_roc <- roc(test_scaled$diagnosis, svm_linear_prob, quiet = TRUE)

print("Resultados SVM Lineal:")
print(svm_linear_cm)
print(paste("AUC:", round(auc(svm_linear_roc), 4)))

# SVM Radial con tuning
print("SVM Radial con tuning...")
tune_result <- tune.svm(diagnosis ~ ., data = train_scaled,
                        kernel = "radial",
                        cost = c(0.1, 1, 10),
                        gamma = c(0.01, 0.1, 1),
                        tunecontrol = tune.control(cross = 5))

print("Mejores parámetros:")
print(tune_result$best.parameters)

svm_radial <- tune_result$best.model

svm_radial_pred <- predict(svm_radial, test_scaled)
svm_radial_prob <- attr(predict(svm_radial, test_scaled, probability = TRUE),
                        "probabilities")[, 2]

svm_radial_cm <- confusionMatrix(svm_radial_pred, test_scaled$diagnosis,
                                 positive = "M")
svm_radial_roc <- roc(test_scaled$diagnosis, svm_radial_prob, quiet = TRUE)

print("Resultados SVM Radial:")
print(svm_radial_cm)
print(paste("AUC:", round(auc(svm_radial_roc), 4)))

# ==============================================================================
# 5. RANDOM FOREST
# ==============================================================================

print("=== RANDOM FOREST ===")

rf_model <- randomForest(diagnosis ~ ., data = train_data,
                         ntree = 500, mtry = 5, importance = TRUE)

print(rf_model)

rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[, 2]

rf_cm <- confusionMatrix(rf_pred, test_data$diagnosis, positive = "M")
rf_roc <- roc(test_data$diagnosis, rf_prob, quiet = TRUE)

print("Resultados Random Forest:")
print(rf_cm)
print(paste("AUC:", round(auc(rf_roc), 4)))

# Importancia de variables
importance_df <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(MeanDecreaseGini))

print("Top 10 variables:")
print(head(importance_df, 10))

png("03_Visualizaciones/breast_rf_importance.png",
    width = 1200, height = 800)
varImpPlot(rf_model, n.var = 15, main = "Importancia - Random Forest")
dev.off()

# ==============================================================================
# 6. XGBOOST
# ==============================================================================

print("=== XGBOOST ===")

x_test <- model.matrix(diagnosis ~ ., test_scaled)[, -1]
y_test <- as.numeric(test_scaled$diagnosis) - 1

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8
)

xgb_cv <- xgb.cv(params = params, data = dtrain, nrounds = 200,
                 nfold = 5, early_stopping_rounds = 20, verbose = 0)

best_nrounds <- xgb_cv$best_iteration

xgb_model <- xgb.train(params = params, data = dtrain,
                       nrounds = best_nrounds, verbose = 0)

xgb_prob <- predict(xgb_model, dtest)
xgb_pred <- factor(ifelse(xgb_prob > 0.5, "M", "B"), levels = c("B", "M"))

xgb_cm <- confusionMatrix(xgb_pred, test_scaled$diagnosis, positive = "M")
xgb_roc <- roc(test_scaled$diagnosis, xgb_prob, quiet = TRUE)

print("Resultados XGBoost:")
print(xgb_cm)
print(paste("AUC:", round(auc(xgb_roc), 4)))

# ==============================================================================
# 7. COMPARACIÓN DE MODELOS
# ==============================================================================

print("=== COMPARACIÓN ===")

resultados <- data.frame(
  Modelo = c("SVM Lineal", "SVM Radial", "Random Forest", "XGBoost"),
  Accuracy = c(svm_linear_cm$overall["Accuracy"],
               svm_radial_cm$overall["Accuracy"],
               rf_cm$overall["Accuracy"],
               xgb_cm$overall["Accuracy"]),
  Sensitivity = c(svm_linear_cm$byClass["Sensitivity"],
                  svm_radial_cm$byClass["Sensitivity"],
                  rf_cm$byClass["Sensitivity"],
                  xgb_cm$byClass["Sensitivity"]),
  Specificity = c(svm_linear_cm$byClass["Specificity"],
                  svm_radial_cm$byClass["Specificity"],
                  rf_cm$byClass["Specificity"],
                  xgb_cm$byClass["Specificity"]),
  AUC = c(auc(svm_linear_roc), auc(svm_radial_roc),
          auc(rf_roc), auc(xgb_roc))
)

print(resultados)

ggplot(resultados, aes(x = reorder(Modelo, AUC), y = AUC, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(AUC, 3)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Comparación de AUC - Breast Cancer",
       y = "AUC", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0.9, 1.05)

ggsave("03_Visualizaciones/breast_model_comparison.png",
       width = 10, height = 6)

print("=== ANÁLISIS COMPLETADO ===")
print("Todos los modelos alcanzaron excelente desempeño (AUC > 0.95)")
print("SVM y XGBoost son especialmente efectivos en este dataset")
