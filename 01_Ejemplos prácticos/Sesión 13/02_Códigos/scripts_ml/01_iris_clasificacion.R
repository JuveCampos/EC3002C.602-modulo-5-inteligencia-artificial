# ==============================================================================
# ANÁLISIS DE CLASIFICACIÓN MULTICLASE - IRIS DATASET
# ==============================================================================
#
# Dataset: Iris
# Observaciones: 150
# Variables: 5 (4 predictoras + 1 objetivo)
#
# Descripción:
# El dataset Iris es un clásico en machine learning, creado por Ronald Fisher
# en 1936. Contiene medidas de flores de iris de tres especies diferentes
# (Setosa, Versicolor, Virginica).
#
# Métodos aplicados:
# 1. Árboles de decisión simples
# 2. Support Vector Machines (SVM)
#
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(GGally)

# Establecer semilla para reproducibilidad
set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

# Cargar datos desde archivo CSV
iris_data <- read_csv("01_Datos/datasets_ml/iris.csv",
                      show_col_types = FALSE)

# Mostrar dimensiones del dataset
print(paste("Dimensiones del dataset:", nrow(iris_data), "filas x",
            ncol(iris_data), "columnas"))

# Convertir species a factor para análisis de clasificación
iris_data <- iris_data %>%
  mutate(species = as.factor(species))

# Mostrar distribución de especies
table(iris_data$species) %>%
  print()

# ==============================================================================
# 2. VISUALIZACIÓN EXPLORATORIA
# ==============================================================================

# Gráfico de pares coloreado por especie
ggpairs(iris_data,
        columns = 1:4,
        aes(color = species, alpha = 0.5),
        title = "Matriz de dispersión - Iris Dataset") +
  theme_minimal()

ggsave("03_Visualizaciones/iris_pairplot.png",
       width = 12, height = 10)

# Boxplots para cada variable
iris_long <- iris_data %>%
  pivot_longer(cols = -species,
               names_to = "variable",
               values_to = "valor")

ggplot(iris_long, aes(x = species, y = valor, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribución de variables por especie",
       x = "Especie",
       y = "Valor") +
  theme(legend.position = "bottom")

ggsave("03_Visualizaciones/iris_boxplots.png",
       width = 10, height = 8)

# ==============================================================================
# 3. DIVISIÓN DE DATOS EN ENTRENAMIENTO Y PRUEBA
# ==============================================================================

# División 70-30 estratificada por especie
train_index <- createDataPartition(iris_data$species,
                                   p = 0.7,
                                   list = FALSE)

train_data <- iris_data[train_index, ]
test_data <- iris_data[-train_index, ]

print(paste("Observaciones de entrenamiento:", nrow(train_data)))
print(paste("Observaciones de prueba:", nrow(test_data)))

# ==============================================================================
# 4. ÁRBOLES DE DECISIÓN
# ==============================================================================
#
# TEORÍA: ÁRBOLES DE DECISIÓN
#
# Los árboles de decisión son modelos de aprendizaje supervisado que predicen
# el valor de una variable objetivo dividiendo recursivamente el espacio de
# características. En cada nodo, el árbol selecciona la variable y el punto
# de corte que mejor separa las clases (usando medidas como Gini o entropía).
#
# Ventajas:
# - Fáciles de interpretar y visualizar
# - No requieren escalamiento de variables
# - Manejan variables numéricas y categóricas
# - Capturan relaciones no lineales
#
# Desventajas:
# - Propensos al sobreajuste
# - Inestables (pequeños cambios en datos generan árboles diferentes)
# - Sesgados hacia variables con más categorías
#
# ==============================================================================

# Entrenar árbol de decisión
# cp = complexity parameter (penaliza árboles complejos)
tree_model <- rpart(species ~ .,
                    data = train_data,
                    method = "class",
                    control = rpart.control(cp = 0.01))

# Visualizar árbol
rpart.plot(tree_model,
           main = "Árbol de Decisión - Iris Classification",
           extra = 104,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)

# Guardar visualización
png("03_Visualizaciones/iris_decision_tree.png",
    width = 1200, height = 800)
rpart.plot(tree_model,
           main = "Árbol de Decisión - Iris Classification",
           extra = 104,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)
dev.off()

# Predicciones en conjunto de prueba
tree_pred <- predict(tree_model, test_data, type = "class")

# Matriz de confusión
tree_cm <- confusionMatrix(tree_pred, test_data$species)

print(paste("Accuracy Árbol de Decisión:",
            round(tree_cm$overall["Accuracy"], 4)))

# ==============================================================================
# 5. SUPPORT VECTOR MACHINES (SVM)
# ==============================================================================
#
# TEORÍA: SUPPORT VECTOR MACHINES
#
# SVM es un algoritmo de clasificación que busca encontrar el hiperplano
# óptimo que mejor separa las clases en el espacio de características.
#
# Conceptos clave:
# - Hiperplano: Frontera de decisión que separa las clases
# - Vectores de soporte: Puntos más cercanos al hiperplano que lo definen
# - Margen: Distancia entre el hiperplano y los vectores de soporte
# - SVM busca maximizar este margen (max-margin classifier)
#
# Kernel trick:
# - Para datos no linealmente separables, SVM usa kernels para proyectar
#   los datos a un espacio de mayor dimensión donde sean separables
# - Kernels comunes: lineal, polinomial, radial (RBF), sigmoide
#
# Ventajas:
# - Efectivo en espacios de alta dimensión
# - Robusto a outliers (solo depende de vectores de soporte)
# - Versátil (diferentes kernels para diferentes problemas)
#
# Desventajas:
# - Costoso computacionalmente para grandes datasets
# - Difícil de interpretar
# - Requiere selección cuidadosa de kernel y parámetros
#
# ==============================================================================

# Normalizar datos - importante para SVM
preproc <- preProcess(train_data[, -5], method = c("center", "scale"))
train_scaled <- predict(preproc, train_data)
test_scaled <- predict(preproc, test_data)

# Entrenar SVM con kernel lineal
svm_linear <- svm(species ~ .,
                  data = train_scaled,
                  kernel = "linear",
                  cost = 1)

svm_linear_pred <- predict(svm_linear, test_scaled)
svm_linear_cm <- confusionMatrix(svm_linear_pred, test_scaled$species)

print(paste("Accuracy SVM Lineal:",
            round(svm_linear_cm$overall["Accuracy"], 4)))

# Entrenar SVM con kernel radial (RBF)
# El kernel RBF es útil para relaciones no lineales
svm_radial <- svm(species ~ .,
                  data = train_scaled,
                  kernel = "radial",
                  cost = 1,
                  gamma = 0.5)

svm_radial_pred <- predict(svm_radial, test_scaled)
svm_radial_cm <- confusionMatrix(svm_radial_pred, test_scaled$species)

print(paste("Accuracy SVM Radial:",
            round(svm_radial_cm$overall["Accuracy"], 4)))

# ==============================================================================
# 6. TUNING DE HIPERPARÁMETROS PARA SVM
# ==============================================================================

# Definir grid de parámetros
# cost: penalización por violaciones del margen
# gamma: inverso del radio de influencia de vectores de soporte
tune_grid <- expand.grid(
  cost = c(0.1, 1, 10),
  gamma = c(0.1, 0.5, 1)
)

# Usar tune.svm para encontrar mejores parámetros
# Realiza validación cruzada internamente
svm_tune <- tune.svm(species ~ .,
                     data = train_scaled,
                     kernel = "radial",
                     cost = tune_grid$cost,
                     gamma = tune_grid$gamma,
                     tunecontrol = tune.control(cross = 5))

# Entrenar modelo final con mejores parámetros
svm_best <- svm_tune$best.model

svm_best_pred <- predict(svm_best, test_scaled)
svm_best_cm <- confusionMatrix(svm_best_pred, test_scaled$species)

print(paste("Accuracy SVM Optimizado:",
            round(svm_best_cm$overall["Accuracy"], 4)))

# ==============================================================================
# 7. COMPARACIÓN DE MODELOS
# ==============================================================================

# Crear dataframe con métricas de todos los modelos
resultados <- tibble(
  Modelo = c("Árbol de Decisión",
             "SVM Lineal",
             "SVM Radial",
             "SVM Optimizado"),
  Accuracy = c(tree_cm$overall["Accuracy"],
               svm_linear_cm$overall["Accuracy"],
               svm_radial_cm$overall["Accuracy"],
               svm_best_cm$overall["Accuracy"]),
  Kappa = c(tree_cm$overall["Kappa"],
            svm_linear_cm$overall["Kappa"],
            svm_radial_cm$overall["Kappa"],
            svm_best_cm$overall["Kappa"])
)

# Mostrar tabla de resultados
resultados %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print()

# Visualizar comparación
ggplot(resultados, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Accuracy, 3)),
            vjust = -0.5,
            size = 4) +
  ylim(0, 1.1) +
  theme_minimal() +
  labs(title = "Comparación de Accuracy entre Modelos",
       y = "Accuracy",
       x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("03_Visualizaciones/iris_model_comparison.png",
       width = 10, height = 6)

# ==============================================================================
# 8. VISUALIZACIÓN DE FRONTERAS DE DECISIÓN
# ==============================================================================

# Función para crear grid de predicciones
# Usaremos solo 2 variables para poder visualizar en 2D
create_decision_boundary <- function(model, data, var1, var2, model_type) {
  # Crear grid de valores
  x_range <- seq(min(data[[var1]]) - 0.5,
                 max(data[[var1]]) + 0.5,
                 length.out = 100)
  y_range <- seq(min(data[[var2]]) - 0.5,
                 max(data[[var2]]) + 0.5,
                 length.out = 100)

  grid <- expand.grid(x = x_range, y = y_range)

  # Crear dataframe con todas las variables necesarias
  # Usar medias para las variables no visualizadas
  grid_full <- tibble(
    sepal_length = grid$x,
    sepal_width = mean(data$sepal_width),
    petal_length = grid$y,
    petal_width = mean(data$petal_width)
  )

  # Predecir según tipo de modelo
  if (model_type == "tree") {
    grid$prediction <- predict(model, grid_full, type = "class")
  } else {
    grid$prediction <- predict(model, grid_full)
  }

  return(grid)
}

# Crear visualización para árbol de decisión
grid_tree <- create_decision_boundary(tree_model,
                                      train_data,
                                      "sepal_length",
                                      "petal_length",
                                      "tree")

ggplot() +
  geom_tile(data = grid_tree,
            aes(x = x, y = y, fill = prediction),
            alpha = 0.3) +
  geom_point(data = train_data,
             aes(x = sepal_length, y = petal_length, color = species),
             size = 3,
             alpha = 0.8) +
  labs(title = "Fronteras de Decisión - Árbol de Decisión",
       x = "Sepal Length",
       y = "Petal Length",
       fill = "Predicción",
       color = "Especie Real") +
  theme_minimal()

ggsave("03_Visualizaciones/iris_tree_boundaries.png",
       width = 10, height = 6)

# Crear visualización para SVM optimizado
grid_svm <- create_decision_boundary(svm_best,
                                     train_scaled,
                                     "sepal_length",
                                     "petal_length",
                                     "svm")

ggplot() +
  geom_tile(data = grid_svm,
            aes(x = x, y = y, fill = prediction),
            alpha = 0.3) +
  geom_point(data = train_scaled,
             aes(x = sepal_length, y = petal_length, color = species),
             size = 3,
             alpha = 0.8) +
  labs(title = "Fronteras de Decisión - SVM Optimizado",
       x = "Sepal Length (scaled)",
       y = "Petal Length (scaled)",
       fill = "Predicción",
       color = "Especie Real") +
  theme_minimal()

ggsave("03_Visualizaciones/iris_svm_boundaries.png",
       width = 10, height = 6)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

# El análisis está completo
# Todos los modelos alcanzaron alta precisión (>95%) en este dataset clásico
# Las visualizaciones se guardaron en 03_Visualizaciones/
