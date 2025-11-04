# Modelos de Clasificación y Regresión Logística
# Script educativo para predicción de calidad de vino

# Librerías necesarias ----
library(tidyverse)
library(readxl)
library(nnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(class)
library(caret)
library(corrplot)
library(pROC)

# 1. Carga y exploración de datos ----

# Los datos contienen información fisicoquímica de vinos
# con mediciones de acidez, azúcar, cloruros, sulfatos, alcohol y pH
# La variable objetivo es la calidad del vino (Malo, Regular, Bueno)

datos_vino <- read_excel("datos_vino.xlsx")

# Visualizar estructura de los datos
glimpse(datos_vino)

# Convertir calidad a factor ordenado
datos_vino <- datos_vino %>%
  mutate(calidad = factor(calidad, levels = c("Malo", "Regular", "Bueno"), ordered = TRUE))

# Resumen estadístico de las variables
print("Resumen estadístico de variables:")
summary(datos_vino)

# Distribución de la variable objetivo
tabla_calidad <- table(datos_vino$calidad)
print("Distribución de calidad:")
print(tabla_calidad)
print(prop.table(tabla_calidad))

# Visualizar distribución de calidad
ggplot(datos_vino, aes(x = calidad, fill = calidad)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Distribución de Calidad del Vino",
    x = "Calidad",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")


# 2. Análisis de Correlaciones ----

# Matriz de correlaciones entre variables numéricas
variables_numericas <- datos_vino %>%
  select(-calidad)

matriz_cor <- cor(variables_numericas)

print("Matriz de Correlaciones:")
print(round(matriz_cor, 3))

# Visualización de correlaciones
corrplot(matriz_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 0.8,
         title = "Matriz de Correlaciones - Variables del Vino",
         mar = c(0,0,1,0))

# Boxplots de variables por calidad
datos_vino_long <- datos_vino %>%
  pivot_longer(cols = -calidad, names_to = "variable", values_to = "valor")

ggplot(datos_vino_long, aes(x = calidad, y = valor, fill = calidad)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(
    title = "Distribución de Variables por Calidad",
    x = "Calidad",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")


# 3. División de datos: Entrenamiento y Prueba ----

# Establecer semilla para reproducibilidad
set.seed(123)

# Dividir datos en 70% entrenamiento y 30% prueba
indices_train <- createDataPartition(datos_vino$calidad, p = 0.7, list = FALSE)
datos_train <- datos_vino[indices_train, ]
datos_test <- datos_vino[-indices_train, ]

print(paste("Observaciones entrenamiento:", nrow(datos_train)))
print(paste("Observaciones prueba:", nrow(datos_test)))

# Verificar distribución de calidad en ambos conjuntos
print("Distribución en entrenamiento:")
print(table(datos_train$calidad))
print("Distribución en prueba:")
print(table(datos_test$calidad))


# 4. Regresión Logística Multinomial ----

# La regresión logística multinomial extiende la regresión logística binaria
# para casos con más de dos categorías en la variable dependiente
# Utiliza la función softmax para calcular probabilidades de cada clase

# MODIFICA AQUÍ: Selecciona las variables predictoras
formula_modelo <- calidad ~ acidez + azucar_residual + cloruros + sulfatos + alcohol + ph

# Ajustar modelo de regresión logística multinomial
modelo_logit <- multinom(formula_modelo, data = datos_train)

# Resumen del modelo
print("Resumen del Modelo Logístico Multinomial:")
summary(modelo_logit)

# Calcular z-scores y p-values
z_scores <- summary(modelo_logit)$coefficients / summary(modelo_logit)$standard.errors
p_values <- (1 - pnorm(abs(z_scores), 0, 1)) * 2

print("P-values de los coeficientes:")
print(p_values)

# Predicciones en conjunto de prueba
pred_logit <- predict(modelo_logit, newdata = datos_test, type = "class")
pred_logit_prob <- predict(modelo_logit, newdata = datos_test, type = "probs")

# Matriz de confusión
conf_matrix_logit <- confusionMatrix(pred_logit, datos_test$calidad)
print("Matriz de Confusión - Regresión Logística:")
print(conf_matrix_logit)

# Visualizar matriz de confusión
conf_matrix_df <- as.data.frame(conf_matrix_logit$table)
ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Matriz de Confusión - Regresión Logística Multinomial",
    x = "Valor Real",
    y = "Valor Predicho"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# 5. Árbol de Decisión ----

# Los árboles de decisión son modelos no paramétricos
# que dividen el espacio de características mediante reglas if-then
# Son fáciles de interpretar y visualizar

# Ajustar árbol de decisión
modelo_arbol <- rpart(formula_modelo, data = datos_train, method = "class")

# Visualizar árbol
rpart.plot(modelo_arbol,
           type = 4,
           extra = 2,
           main = "Árbol de Decisión - Calidad del Vino",
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)

# Importancia de variables
importancia_arbol <- modelo_arbol$variable.importance
print("Importancia de Variables (Árbol de Decisión):")
print(importancia_arbol)

# Predicciones
pred_arbol <- predict(modelo_arbol, newdata = datos_test, type = "class")

# Matriz de confusión
conf_matrix_arbol <- confusionMatrix(pred_arbol, datos_test$calidad)
print("Matriz de Confusión - Árbol de Decisión:")
print(conf_matrix_arbol)


# 6. Random Forest ----

# Random Forest es un método de ensamble que construye múltiples árboles de decisión
# y combina sus predicciones mediante votación (clasificación) o promedio (regresión)
# Reduce el sobreajuste y mejora la precisión

# Ajustar modelo Random Forest
set.seed(123)
modelo_rf <- randomForest(formula_modelo, data = datos_train, ntree = 500, importance = TRUE)

print("Resumen del Modelo Random Forest:")
print(modelo_rf)

# Importancia de variables
importancia_rf <- importance(modelo_rf)
print("Importancia de Variables (Random Forest):")
print(importancia_rf)

# Visualizar importancia de variables
varImpPlot(modelo_rf, main = "Importancia de Variables - Random Forest")

# Visualización alternativa con ggplot
importancia_df <- data.frame(
  Variable = rownames(importancia_rf),
  MeanDecreaseAccuracy = importancia_rf[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importancia_rf[, "MeanDecreaseGini"]
)

ggplot(importancia_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de Variables - Random Forest",
    subtitle = "Mean Decrease Accuracy",
    x = "Variable",
    y = "Mean Decrease Accuracy"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Predicciones
pred_rf <- predict(modelo_rf, newdata = datos_test)

# Matriz de confusión
conf_matrix_rf <- confusionMatrix(pred_rf, datos_test$calidad)
print("Matriz de Confusión - Random Forest:")
print(conf_matrix_rf)


# 7. Support Vector Machine (SVM) ----

# SVM es un algoritmo que encuentra el hiperplano óptimo
# que maximiza el margen entre diferentes clases
# Puede usar kernels para problemas no lineales

# Ajustar modelo SVM con kernel radial
set.seed(123)
modelo_svm <- svm(formula_modelo, data = datos_train, kernel = "radial", probability = TRUE)

print("Resumen del Modelo SVM:")
print(summary(modelo_svm))

# Predicciones
pred_svm <- predict(modelo_svm, newdata = datos_test)

# Matriz de confusión
conf_matrix_svm <- confusionMatrix(pred_svm, datos_test$calidad)
print("Matriz de Confusión - SVM:")
print(conf_matrix_svm)


# 8. K-Nearest Neighbors (KNN) ----

# KNN es un algoritmo de clasificación que asigna una clase
# basándose en las clases de los K vecinos más cercanos
# Es un método no paramétrico simple pero efectivo

# Preparar datos: KNN requiere solo variables numéricas
train_x <- datos_train %>% select(-calidad)
train_y <- factor(datos_train$calidad, ordered = FALSE)  # Convertir a factor no ordenado
test_x <- datos_test %>% select(-calidad)
test_y <- factor(datos_test$calidad, ordered = FALSE)    # Convertir a factor no ordenado

# Estandarizar variables (importante para KNN)
preproc <- preProcess(train_x, method = c("center", "scale"))
train_x_scaled <- predict(preproc, train_x)
test_x_scaled <- predict(preproc, test_x)

# Encontrar K óptimo mediante validación cruzada
set.seed(123)
k_values <- seq(1, 21, by = 2)
accuracy_k <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  k <- k_values[i]
  pred_knn_temp <- knn(train = train_x_scaled, test = test_x_scaled, cl = train_y, k = k)
  accuracy_k[i] <- mean(as.character(pred_knn_temp) == as.character(test_y))
}

# Visualizar accuracy por K
k_df <- data.frame(k = k_values, accuracy = accuracy_k)

ggplot(k_df, aes(x = k, y = accuracy)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Selección de K Óptimo para KNN",
    subtitle = "Accuracy en conjunto de prueba",
    x = "Número de Vecinos (K)",
    y = "Accuracy"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Seleccionar K óptimo
k_optimo <- k_values[which.max(accuracy_k)]
print(paste("K óptimo:", k_optimo))
print(paste("Accuracy máximo:", round(max(accuracy_k), 4)))

# Ajustar modelo con K óptimo
pred_knn <- knn(train = train_x_scaled, test = test_x_scaled, cl = train_y, k = k_optimo)

# Matriz de confusión
conf_matrix_knn <- confusionMatrix(pred_knn, test_y)
print("Matriz de Confusión - KNN:")
print(conf_matrix_knn)


# 9. Comparación de Modelos ----

# Comparar accuracy de todos los modelos
comparacion_modelos <- data.frame(
  Modelo = c("Regresión Logística", "Árbol de Decisión", "Random Forest", "SVM", "KNN"),
  Accuracy = c(
    conf_matrix_logit$overall["Accuracy"],
    conf_matrix_arbol$overall["Accuracy"],
    conf_matrix_rf$overall["Accuracy"],
    conf_matrix_svm$overall["Accuracy"],
    conf_matrix_knn$overall["Accuracy"]
  ),
  Kappa = c(
    conf_matrix_logit$overall["Kappa"],
    conf_matrix_arbol$overall["Kappa"],
    conf_matrix_rf$overall["Kappa"],
    conf_matrix_svm$overall["Kappa"],
    conf_matrix_knn$overall["Kappa"]
  )
)

print("Comparación de Modelos:")
print(comparacion_modelos)

# Visualizar comparación de accuracy
ggplot(comparacion_modelos, aes(x = reorder(Modelo, Accuracy), y = Accuracy, fill = Modelo)) +
  geom_col() +
  geom_text(aes(label = round(Accuracy, 3)), hjust = -0.1) +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Comparación de Accuracy por Modelo",
    subtitle = "Mayor accuracy indica mejor clasificación",
    x = "Modelo",
    y = "Accuracy"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

# Comparación de Kappa
ggplot(comparacion_modelos, aes(x = reorder(Modelo, Kappa), y = Kappa, fill = Modelo)) +
  geom_col() +
  geom_text(aes(label = round(Kappa, 3)), hjust = -0.1) +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Comparación de Kappa por Modelo",
    subtitle = "Kappa considera el acuerdo por azar",
    x = "Modelo",
    y = "Kappa"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

# Identificar mejor modelo
mejor_modelo <- comparacion_modelos$Modelo[which.max(comparacion_modelos$Accuracy)]
print(paste("Mejor modelo según Accuracy:", mejor_modelo))


# 10. Validación Cruzada ----

# La validación cruzada divide los datos en K pliegues
# entrena el modelo K veces dejando un pliegue como prueba
# y promedia las métricas de rendimiento

# Configurar validación cruzada de 10 pliegues
control_cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

# Random Forest con validación cruzada
set.seed(123)
modelo_rf_cv <- train(
  formula_modelo,
  data = datos_train,
  method = "rf",
  trControl = control_cv,
  ntree = 500
)

print("Resultados de Validación Cruzada - Random Forest:")
print(modelo_rf_cv)

# Regresión Logística con validación cruzada
set.seed(123)
modelo_logit_cv <- train(
  formula_modelo,
  data = datos_train,
  method = "multinom",
  trControl = control_cv,
  trace = FALSE
)

print("Resultados de Validación Cruzada - Regresión Logística:")
print(modelo_logit_cv)

# SVM con validación cruzada
set.seed(123)
modelo_svm_cv <- train(
  formula_modelo,
  data = datos_train,
  method = "svmRadial",
  trControl = control_cv
)

print("Resultados de Validación Cruzada - SVM:")
print(modelo_svm_cv)

# Comparar modelos con validación cruzada
resultados_cv <- resamples(list(
  LogisticRegression = modelo_logit_cv,
  RandomForest = modelo_rf_cv,
  SVM = modelo_svm_cv
))

print("Resumen de Validación Cruzada:")
summary(resultados_cv)

# Visualizar comparación de validación cruzada
bwplot(resultados_cv, main = "Comparación de Modelos con Validación Cruzada")


# 11. Curvas ROC para clasificación binaria ----

# Para ilustrar curvas ROC, convertimos a clasificación binaria: Bueno vs No Bueno
datos_train_binario <- datos_train %>%
  mutate(calidad_binaria = ifelse(calidad == "Bueno", "Bueno", "No Bueno"),
         calidad_binaria = factor(calidad_binaria))

datos_test_binario <- datos_test %>%
  mutate(calidad_binaria = ifelse(calidad == "Bueno", "Bueno", "No Bueno"),
         calidad_binaria = factor(calidad_binaria))

# Modelo logístico binario
modelo_logit_binario <- glm(calidad_binaria ~ acidez + azucar_residual + cloruros +
                              sulfatos + alcohol + ph,
                            data = datos_train_binario,
                            family = binomial)

print("Resumen de Regresión Logística Binaria:")
summary(modelo_logit_binario)

# Predicciones de probabilidades
pred_prob_binario <- predict(modelo_logit_binario, newdata = datos_test_binario, type = "response")

# Curva ROC
roc_obj <- roc(datos_test_binario$calidad_binaria, pred_prob_binario)

print(paste("AUC (Area Under Curve):", round(auc(roc_obj), 4)))

# Visualizar curva ROC
plot(roc_obj,
     main = "Curva ROC - Clasificación Binaria (Bueno vs No Bueno)",
     col = "blue",
     lwd = 2,
     print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")


# 12. Predicción con Nuevas Observaciones ----

# Ejemplo: Vino con características específicas
nuevo_vino <- data.frame(
  acidez = 8.5,
  azucar_residual = 3.0,
  cloruros = 0.08,
  sulfatos = 0.6,
  alcohol = 11.0,
  ph = 3.3
)

print("Nuevo vino a clasificar:")
print(nuevo_vino)

# Predicciones con diferentes modelos
pred_logit_nuevo <- predict(modelo_logit, newdata = nuevo_vino, type = "class")
pred_arbol_nuevo <- predict(modelo_arbol, newdata = nuevo_vino, type = "class")
pred_rf_nuevo <- predict(modelo_rf, newdata = nuevo_vino)
pred_svm_nuevo <- predict(modelo_svm, newdata = nuevo_vino)

# Para KNN, necesitamos estandarizar
nuevo_vino_scaled <- predict(preproc, nuevo_vino)
pred_knn_nuevo <- knn(train = train_x_scaled, test = nuevo_vino_scaled, cl = train_y, k = k_optimo)

# Probabilidades del modelo logístico
pred_logit_prob_nuevo <- predict(modelo_logit, newdata = nuevo_vino, type = "probs")

print("Predicciones para el nuevo vino:")
print(paste("Regresión Logística:", pred_logit_nuevo))
print("Probabilidades por clase:")
print(pred_logit_prob_nuevo)
print(paste("Árbol de Decisión:", pred_arbol_nuevo))
print(paste("Random Forest:", pred_rf_nuevo))
print(paste("SVM:", pred_svm_nuevo))
print(paste("KNN:", pred_knn_nuevo))

# Crear función de predicción reutilizable
predecir_calidad_vino <- function(nuevo_vino, modelo = "rf") {
  if(modelo == "logit") {
    prediccion <- predict(modelo_logit, newdata = nuevo_vino, type = "class")
    probabilidades <- predict(modelo_logit, newdata = nuevo_vino, type = "probs")
  } else if(modelo == "arbol") {
    prediccion <- predict(modelo_arbol, newdata = nuevo_vino, type = "class")
    probabilidades <- predict(modelo_arbol, newdata = nuevo_vino, type = "prob")
  } else if(modelo == "rf") {
    prediccion <- predict(modelo_rf, newdata = nuevo_vino)
    probabilidades <- predict(modelo_rf, newdata = nuevo_vino, type = "prob")
  } else if(modelo == "svm") {
    prediccion <- predict(modelo_svm, newdata = nuevo_vino)
    probabilidades <- attr(predict(modelo_svm, newdata = nuevo_vino, probability = TRUE), "probabilities")
  } else if(modelo == "knn") {
    nuevo_vino_scaled <- predict(preproc, nuevo_vino)
    prediccion <- knn(train = train_x_scaled, test = nuevo_vino_scaled, cl = train_y, k = k_optimo)
    probabilidades <- NULL
  }

  return(list(
    modelo = modelo,
    prediccion = prediccion,
    probabilidades = probabilidades
  ))
}

# Ejemplo de uso
resultado_prediccion <- predecir_calidad_vino(nuevo_vino, modelo = "rf")
print("Función de predicción (Random Forest):")
print(paste("Predicción:", resultado_prediccion$prediccion))
print("Probabilidades:")
print(resultado_prediccion$probabilidades)


# Resumen Final ----

# Resumen ejecutivo de todos los análisis realizados
print("=== RESUMEN DE ANÁLISIS DE CLASIFICACIÓN ===")
print(paste("1. Datos cargados:", nrow(datos_vino), "observaciones"))
print(paste("2. Variables predictoras:", paste(names(variables_numericas), collapse = ", ")))
print(paste("3. Variable objetivo: calidad (", paste(levels(datos_vino$calidad), collapse = ", "), ")"))
print("4. Modelos entrenados:")
print(comparacion_modelos)
print(paste("5. Mejor modelo:", mejor_modelo, "- Accuracy:",
            round(max(comparacion_modelos$Accuracy), 4)))
print("6. Función de predicción disponible: predecir_calidad_vino()")
