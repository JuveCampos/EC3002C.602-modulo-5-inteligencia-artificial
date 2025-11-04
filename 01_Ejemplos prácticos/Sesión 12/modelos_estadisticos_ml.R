options(scipen = 999)

# Modelos Estadísticos y Machine Learning con ENOE
# Script educativo para análisis de ingresos

# Librerías necesarias ----
library(tidyverse)
library(srvyr)
library(survey)
library(rpart)
library(rpart.plot)
library(glmnet)
library(cluster)
library(factoextra)
library(ggplot2)
library(corrplot)
library(broom)

# 1. Carga de datos ENOE ----

# La ENOE (Encuesta Nacional de Ocupación y Empleo) es una encuesta realizada
# por el INEGI que proporciona información sobre las características ocupacionales
# de la población en México. Incluye datos sobre empleo, ingresos, educación,
# sector económico, formalidad, entre muchas otras variables demográficas y laborales.
# Es fundamental para el análisis del mercado laboral mexicano.
# https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/doc/con_basedatos_proy2010.pdf

# Variables: 
# eda: edad 
# e_con: Estado conyugal
# pos_ocu: posición ocupada. 
# jornada_complet
# t_loc: tamaño de localidad
enoe <- readRDS("enoe_proc_2025_2.rds") %>% 
  as_tibble() %>% 
  filter(clase1 == 1) %>% 
  filter(clase2 == 1) %>% 
  mutate(sexo = factor(sexo, levels = c("Hombre", "Mujer")))

# enoe$sexo

names(enoe)

# Crear objeto de diseño de encuesta con srvyr
# Esto nos permite hacer cálculos correctamente ponderados
enoe_svy <- enoe %>%
  as_survey_design(weights = factor)

# Visualizar estructura básica de los datos
glimpse(enoe)

# Número de observaciones y variables
# Dimensiones de la base: observaciones y variables


# 2. Matriz de Varianzas y Covarianzas ----

# Para calcular la matriz de varianzas/covarianzas necesitamos variables numéricas
# Seleccionamos las variables numéricas relevantes para el análisis

# MODIFICA AQUÍ: Agrega o quita variables según necesites
variables_numericas <- c(
  "ingreso_sm",        # Ingreso en salarios mínimos (variable dependiente)
  "eda",               # Edad
  "anios_esc",         # Años de escolaridad
  "hrsocup",           # Horas ocupadas
  "no_hijos"          # Número de hijos
  # "horas_semana_quehaceres",           # Horas dedicadas a quehaceres
  # "horas_semana_quehaceres_cuidados"   # Horas de quehaceres y cuidados
)

# Crear subset con variables seleccionadas y remover NAs
enoe_numericas <- enoe %>%
  select(all_of(variables_numericas), factor) %>%
  drop_na()

# Función para calcular matriz de covarianzas ponderada
cov_ponderada <- function(x, y, pesos) {
  pesos_norm <- pesos / sum(pesos)
  media_x <- sum(x * pesos_norm)
  media_y <- sum(y * pesos_norm)
  sum(pesos_norm * (x - media_x) * (y - media_y))
}

# Calcular matriz de covarianzas ponderada
n_vars <- length(variables_numericas)
matriz_cov <- matrix(NA, n_vars, n_vars)
rownames(matriz_cov) <- colnames(matriz_cov) <- variables_numericas

for(i in 1:n_vars) {
  for(j in 1:n_vars) {
    matriz_cov[i, j] <- cov_ponderada(
      enoe_numericas[[variables_numericas[i]]],
      enoe_numericas[[variables_numericas[j]]],
      enoe_numericas$factor
    )
  }
}

# Matriz de Covarianzas Ponderada
print(matriz_cov)

# Función para calcular correlación ponderada
cor_ponderada <- function(x, y, pesos) {
  cov_xy <- cov_ponderada(x, y, pesos)
  sd_x <- sqrt(cov_ponderada(x, x, pesos))
  sd_y <- sqrt(cov_ponderada(y, y, pesos))
  cov_xy / (sd_x * sd_y)
}

# Calcular matriz de correlaciones ponderada
matriz_cor <- matrix(NA, n_vars, n_vars)
rownames(matriz_cor) <- colnames(matriz_cor) <- variables_numericas

for(i in 1:n_vars) {
  for(j in 1:n_vars) {
    matriz_cor[i, j] <- cor_ponderada(
      enoe_numericas[[variables_numericas[i]]],
      enoe_numericas[[variables_numericas[j]]],
      enoe_numericas$factor
    )
  }
}

# Matriz de Correlaciones Ponderada
print(matriz_cor)

# Visualización de la matriz de correlaciones
corrplot(matriz_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 0.8,
         title = "Matriz de Correlaciones Ponderadas - Variables ENOE",
         mar = c(0,0,1,0))


# 3. Modelo de Árbol de Decisión ----

# Los árboles de decisión son modelos que predicen una variable dependiente
# mediante reglas de decisión aprendidas de las características de los datos.
# Son fáciles de interpretar y visualizar.

# MODIFICA AQUÍ: Selecciona las variables explicativas para el modelo
variables_modelo <- c(
  "eda",                    # Edad
  "anios_esc",             # Años de escolaridad
  "hrsocup",               # Horas trabajadas
  "sexo",                   # Sexo
  "pos_ocu",               # Posición en la ocupación
  "tipo_formalidad"  ,       # Formalidad del empleo
  "sector",                # Sector económico
  "jornada_completa",      # Tipo de jornada
  # "no_hijos",              # Número de hijos
  "estado_conyugal",       # Estado conyugal
  "duracion_jornada",      # Duración de la jornada
  "tamanio_empresa",
  "seguridad_social"       # Acceso a seguridad social
)

# Preparar datos para el modelo (remover NAs)
enoe_arbol <- enoe %>%
  select(ingreso_sm, all_of(variables_modelo), factor) %>%
  drop_na()

# Entrenar modelo de árbol
# El parámetro cp controla la complejidad del árbol (valores más bajos = árboles más complejos)
modelo_arbol <- rpart(
  ingreso_sm ~ .,
  data = enoe_arbol %>% select(-factor),
  weights = enoe_arbol$factor,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 100)
)

# Resumen del modelo
print("Resumen del Modelo de Árbol:")
print(summary(modelo_arbol))

# Importancia de variables
print("Importancia de Variables:")
print(modelo_arbol$variable.importance)

# Visualizar el árbol de decisión
rpart.plot(modelo_arbol,
           type = 4,
           extra = 101,
           main = "Árbol de Decisión - Ingreso en Salarios Mínimos",
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)


# 4. Regresión Lineal ----

# La regresión lineal modela la relación entre una variable dependiente
# y una o más variables independientes mediante una función lineal.
# Es uno de los modelos más utilizados en estadística.

# Preparar datos para regresión
enoe_regresion <- enoe %>%
  select(ingreso_sm, all_of(variables_modelo), factor) %>% 
  drop_na()

# enoe_regresion$sexo

# Ajustar modelo de regresión lineal (ponderado)
# lm acepta pesos mediante el argumento weights
modelo_lm <- lm(
  ingreso_sm ~ .,
  data = enoe_regresion %>% select(-factor),
  weights = enoe_regresion$factor
)

# Resumen del modelo
print("Resumen de la Regresión Lineal:")
summary(modelo_lm)

# Análisis de coeficientes
coeficientes <- broom::tidy(modelo_lm) %>%
  arrange(desc(abs(estimate)))

print("Coeficientes del Modelo (ordenados por magnitud):")
print(coeficientes)

# Interpretación de coeficientes significativos
coef_significativos <- coeficientes %>%
  filter(p.value < 0.05, term != "(Intercept)")

print("Coeficientes Estadísticamente Significativos (p < 0.05):")
print(coef_significativos)

# Visualización: Valores observados vs predichos
enoe_regresion <- enoe_regresion %>%
  mutate(prediccion = predict(modelo_lm, newdata = .))

# Gráfico de valores observados vs predichos
ggplot(enoe_regresion %>% sample_n(min(5000, nrow(.))),
       aes(x = prediccion, y = ingreso_sm)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Regresión Lineal: Valores Observados vs Predichos",
    subtitle = "Línea roja indica ajuste perfecto",
    x = "Valores Predichos (salarios mínimos)",
    y = "Valores Observados (salarios mínimos)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Gráfico de residuos
enoe_regresion <- enoe_regresion %>%
  mutate(residuos = ingreso_sm - prediccion)

ggplot(enoe_regresion %>% sample_n(min(5000, nrow(.))),
       aes(x = prediccion, y = residuos)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Análisis de Residuos",
    subtitle = "Los residuos deben distribuirse aleatoriamente alrededor de cero",
    x = "Valores Predichos",
    y = "Residuos"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# 5. Regresión Ridge y Lasso ----

# Ridge y Lasso son métodos de regularización que penalizan la magnitud
# de los coeficientes para evitar sobreajuste.
# Ridge (L2): reduce coeficientes pero no los elimina
# Lasso (L1): puede reducir coeficientes a exactamente cero (selección de variables)

# Preparar matriz de variables predictoras y vector de respuesta
# glmnet requiere matrices numéricas, por lo que convertimos variables categóricas

# Convertir variables categóricas a dummies
enoe_glmnet <- enoe %>%
  select(ingreso_sm, all_of(variables_modelo), factor) %>%
  drop_na()

# Crear matriz de diseño (con variables dummy)
X <- model.matrix(ingreso_sm ~ . - factor, data = enoe_glmnet)[, -1]
y <- enoe_glmnet$ingreso_sm
weights_glmnet <- enoe_glmnet$factor

# Validación cruzada para encontrar lambda óptimo (Ridge)
cv_ridge <- cv.glmnet(
  x = X,
  y = y,
  weights = weights_glmnet,
  alpha = 0,           # alpha=0 indica Ridge
  nfolds = 10
)

# Validación cruzada para encontrar lambda óptimo (Lasso)
cv_lasso <- cv.glmnet(
  x = X,
  y = y,
  weights = weights_glmnet,
  alpha = 1,           # alpha=1 indica Lasso
  nfolds = 10
)

# Visualizar selección de lambda
par(mfrow = c(1, 2))
plot(cv_ridge, main = "Ridge: Validación Cruzada")
plot(cv_lasso, main = "Lasso: Validación Cruzada")
par(mfrow = c(1, 1))

# Ajustar modelos con lambda óptimo
modelo_ridge <- glmnet(
  x = X,
  y = y,
  weights = weights_glmnet,
  alpha = 0,
  lambda = cv_ridge$lambda.min
)

modelo_lasso <- glmnet(
  x = X,
  y = y,
  weights = weights_glmnet,
  alpha = 1,
  lambda = cv_lasso$lambda.min
)

# Comparación de coeficientes
coef_lm <- coef(modelo_lm)
coef_ridge <- as.matrix(coef(modelo_ridge))
coef_lasso <- as.matrix(coef(modelo_lasso))

# Crear dataframe comparativo (primeros coeficientes numéricos)
nombres_coef <- rownames(coef_ridge)
comparacion_coef <- data.frame(
  Variable = nombres_coef,
  Ridge = as.vector(coef_ridge),
  Lasso = as.vector(coef_lasso)
)

print("Comparación de Coeficientes (Ridge vs Lasso):")
print(comparacion_coef %>% arrange(desc(abs(Ridge))))

# Visualizar comparación de coeficientes
comparacion_coef_long <- comparacion_coef %>%
  filter(Variable != "(Intercept)") %>%
  pivot_longer(cols = c(Ridge, Lasso), names_to = "Modelo", values_to = "Coeficiente") %>%
  group_by(Modelo) %>%
  top_n(20, abs(Coeficiente)) %>%
  ungroup()

ggplot(comparacion_coef_long, aes(x = reorder(Variable, abs(Coeficiente)),
                                   y = Coeficiente,
                                   fill = Modelo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparación de Coeficientes: Ridge vs Lasso",
    subtitle = "Top 20 variables por magnitud",
    x = "Variable",
    y = "Coeficiente"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Calcular predicciones y métricas de error
pred_lm <- predict(modelo_lm, newdata = enoe_glmnet)
pred_ridge <- predict(modelo_ridge, newx = X)[, 1]
pred_lasso <- predict(modelo_lasso, newx = X)[, 1]

# Calcular RMSE (Root Mean Squared Error) ponderado
rmse_ponderado <- function(observado, predicho, pesos) {
  sqrt(sum(pesos * (observado - predicho)^2) / sum(pesos))
}

rmse_lm <- rmse_ponderado(y, pred_lm, weights_glmnet)
rmse_ridge <- rmse_ponderado(y, pred_ridge, weights_glmnet)
rmse_lasso <- rmse_ponderado(y, pred_lasso, weights_glmnet)

# Calcular R² ponderado
r2_ponderado <- function(observado, predicho, pesos) {
  media_ponderada <- sum(pesos * observado) / sum(pesos)
  ss_tot <- sum(pesos * (observado - media_ponderada)^2)
  ss_res <- sum(pesos * (observado - predicho)^2)
  1 - (ss_res / ss_tot)
}

r2_lm <- r2_ponderado(y, pred_lm, weights_glmnet)
r2_ridge <- r2_ponderado(y, pred_ridge, weights_glmnet)
r2_lasso <- r2_ponderado(y, pred_lasso, weights_glmnet)

# Comparación de modelos
comparacion_modelos <- data.frame(
  Modelo = c("Regresión Lineal", "Ridge", "Lasso"),
  RMSE = c(rmse_lm, rmse_ridge, rmse_lasso),
  R_cuadrado = c(r2_lm, r2_ridge, r2_lasso)
)

print("Comparación de Modelos de Regresión:")
print(comparacion_modelos)

# Visualizar comparación
ggplot(comparacion_modelos, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_col() +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5) +
  labs(
    title = "Comparación de Error: RMSE por Modelo",
    subtitle = "Menor RMSE indica mejor ajuste",
    y = "RMSE (Error Cuadrático Medio)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")


# 6. Clustering K-Means (K-NN para clustering) ----

# K-Means es un algoritmo de clustering que agrupa observaciones
# en K clusters basándose en la similitud de sus características.
# Cada observación pertenece al cluster con la media más cercana.

# Nota: El usuario mencionó K-NN, pero para clustering usamos K-Means
# K-NN (K-Nearest Neighbors) es típicamente para clasificación supervisada

# Preparar datos para clustering
enoe_cluster <- enoe %>%
  select(ingreso_sm, all_of(variables_modelo), factor) %>%
  drop_na()

# Seleccionar solo variables numéricas para clustering
vars_numericas_cluster <- c("ingreso_sm", "eda", "anios_esc", "hrsocup")

enoe_cluster_num <- enoe_cluster %>%
  select(all_of(vars_numericas_cluster))

# Estandarizar variables (importante para K-Means)
enoe_cluster_scaled <- enoe_cluster_num %>%
  scale() %>%
  as.data.frame()

# Método del codo para determinar número óptimo de clusters
# Calculamos la suma de cuadrados dentro de clusters (WSS) para k=1 a k=10
set.seed(123)
wss <- map_dbl(1:10, function(k) {
  kmeans(enoe_cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

wss_df <- data.frame(k = 1:10, wss = wss)

# Visualizar método del codo
ggplot(wss_df, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Método del Codo para K-Means",
    subtitle = "Buscamos el 'codo' donde la reducción de WSS se estabiliza",
    x = "Número de Clusters (k)",
    y = "Suma de Cuadrados Dentro de Clusters (WSS)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Método de la silueta para determinar k óptimo
# Para evitar problemas de memoria, tomamos una muestra de 10000 observaciones
set.seed(123)
if(nrow(enoe_cluster_scaled) > 10000) {
  muestra_indices_sil <- sample(1:nrow(enoe_cluster_scaled), 10000)
  enoe_silhouette <- enoe_cluster_scaled[muestra_indices_sil, ]
} else {
  enoe_silhouette <- enoe_cluster_scaled
}

avg_silhouette <- map_dbl(2:10, function(k) {
  km_temp <- kmeans(enoe_silhouette, centers = k, nstart = 25)
  ss <- silhouette(km_temp$cluster, dist(enoe_silhouette))
  mean(ss[, 3])
})

silhouette_df <- data.frame(k = 2:10, avg_sil = avg_silhouette)

# Visualizar método de la silueta
ggplot(silhouette_df, aes(x = k, y = avg_sil)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 3) +
  labs(
    title = "Método de la Silueta para K-Means",
    subtitle = "Mayor valor de silueta indica mejor separación de clusters (muestra de 10k obs)",
    x = "Número de Clusters (k)",
    y = "Coeficiente de Silueta Promedio"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Seleccionar k óptimo (basado en silueta)
k_optimo_kmeans <- silhouette_df$k[which.max(silhouette_df$avg_sil)]
# Número óptimo de clusters seleccionado mediante método de la silueta
print(paste("Número óptimo de clusters (K-Means):", k_optimo_kmeans))

# Ajustar modelo K-Means con k óptimo
set.seed(123)
modelo_kmeans <- kmeans(enoe_cluster_scaled, centers = k_optimo_kmeans, nstart = 25)

# Agregar asignación de clusters al dataframe original
enoe_cluster <- enoe_cluster %>%
  mutate(cluster_kmeans = as.factor(modelo_kmeans$cluster))

# Análisis de PCA para visualización
pca_resultado <- prcomp(enoe_cluster_scaled, center = FALSE, scale. = FALSE)

# Crear dataframe con componentes principales
pca_df <- data.frame(
  PC1 = pca_resultado$x[, 1],
  PC2 = pca_resultado$x[, 2],
  cluster = as.factor(modelo_kmeans$cluster)
)

# Visualizar clusters con PCA
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(type = "norm", level = 0.68) +
  labs(
    title = "Clustering K-Means Visualizado con PCA",
    subtitle = paste("k =", k_optimo_kmeans, "clusters"),
    x = paste0("PC1 (", round(summary(pca_resultado)$importance[2, 1] * 100, 1), "% varianza)"),
    y = paste0("PC2 (", round(summary(pca_resultado)$importance[2, 2] * 100, 1), "% varianza)"),
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Caracterización de clusters
caracterizacion_kmeans <- enoe_cluster %>%
  group_by(cluster_kmeans) %>%
  summarise(
    n = n(),
    ingreso_promedio = mean(ingreso_sm, na.rm = TRUE),
    edad_promedio = mean(eda, na.rm = TRUE),
    escolaridad_promedio = mean(anios_esc, na.rm = TRUE),
    horas_promedio = mean(hrsocup, na.rm = TRUE)
  )

print("Caracterización de Clusters K-Means:")
print(caracterizacion_kmeans)


# 7. Clustering K-Medoides (PAM) ----

# K-Medoides (PAM - Partitioning Around Medoids) es similar a K-Means
# pero usa medoides (observaciones reales) en lugar de centroides (promedios).
# Es más robusto ante outliers.

# Usar los mismos datos escalados
# Para bases grandes, PAM puede ser computacionalmente costoso
# Tomamos una muestra si es necesario

set.seed(123)
if(nrow(enoe_cluster_scaled) > 10000) {
  muestra_indices <- sample(1:nrow(enoe_cluster_scaled), 10000)
  enoe_pam_scaled <- enoe_cluster_scaled[muestra_indices, ]
  enoe_pam <- enoe_cluster[muestra_indices, ]
} else {
  enoe_pam_scaled <- enoe_cluster_scaled
  enoe_pam <- enoe_cluster
}

# Método de la silueta para determinar k óptimo en PAM
avg_silhouette_pam <- map_dbl(2:10, function(k) {
  pam_temp <- pam(enoe_pam_scaled, k = k)
  pam_temp$silinfo$avg.width
})

silhouette_pam_df <- data.frame(k = 2:10, avg_sil = avg_silhouette_pam)

# Visualizar método de la silueta para PAM
ggplot(silhouette_pam_df, aes(x = k, y = avg_sil)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 3) +
  labs(
    title = "Método de la Silueta para K-Medoides (PAM)",
    subtitle = "Mayor valor de silueta indica mejor separación de medoides",
    x = "Número de Medoides (k)",
    y = "Coeficiente de Silueta Promedio"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Seleccionar k óptimo para PAM
k_optimo_pam <- silhouette_pam_df$k[which.max(silhouette_pam_df$avg_sil)]
# Número óptimo de medoides seleccionado mediante método de la silueta
print(paste("Número óptimo de medoides (PAM):", k_optimo_pam))

# Ajustar modelo PAM con k óptimo
set.seed(123)
modelo_pam <- pam(enoe_pam_scaled, k = k_optimo_pam)

# Agregar asignación de clusters al dataframe
enoe_pam <- enoe_pam %>%
  mutate(cluster_pam = as.factor(modelo_pam$clustering))

# PCA para visualización
pca_pam <- prcomp(enoe_pam_scaled, center = FALSE, scale. = FALSE)

pca_pam_df <- data.frame(
  PC1 = pca_pam$x[, 1],
  PC2 = pca_pam$x[, 2],
  cluster = as.factor(modelo_pam$clustering)
)

# Visualizar clusters PAM con PCA
ggplot(pca_pam_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(type = "norm", level = 0.68) +
  labs(
    title = "Clustering K-Medoides (PAM) Visualizado con PCA",
    subtitle = paste("k =", k_optimo_pam, "medoides"),
    x = paste0("PC1 (", round(summary(pca_pam)$importance[2, 1] * 100, 1), "% varianza)"),
    y = paste0("PC2 (", round(summary(pca_pam)$importance[2, 2] * 100, 1), "% varianza)"),
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Caracterización de clusters PAM
caracterizacion_pam <- enoe_pam %>%
  group_by(cluster_pam) %>%
  summarise(
    n = n(),
    ingreso_promedio = mean(ingreso_sm, na.rm = TRUE),
    edad_promedio = mean(eda, na.rm = TRUE),
    escolaridad_promedio = mean(anios_esc, na.rm = TRUE),
    horas_promedio = mean(hrsocup, na.rm = TRUE)
  )

print("Caracterización de Clusters K-Medoides:")
print(caracterizacion_pam)

# Visualizar comparación de métodos de clustering
comparacion_clusters <- data.frame(
  Metodo = c("K-Means", "K-Medoides (PAM)"),
  k_optimo = c(k_optimo_kmeans, k_optimo_pam),
  silhouette_max = c(max(silhouette_df$avg_sil), max(silhouette_pam_df$avg_sil))
)

print("Comparación de Métodos de Clustering:")
print(comparacion_clusters)


# 8. Predicción con Clustering (Ejemplo) ----

# Para predecir a qué cluster pertenecería una nueva observación,
# calculamos la distancia a los centroides (K-Means) o medoides (PAM)
# y asignamos al más cercano.

# Ejemplo: Persona con características específicas
nueva_observacion <- data.frame(
  ingreso_sm = 5.0,        # 5 salarios mínimos
  eda = 35,                # 35 años
  anios_esc = 12,          # 12 años de escolaridad (preparatoria)
  hrsocup = 45,            # 45 horas trabajadas por semana
  no_hijos = 2             # 2 hijos
)

print("Nueva observación a clasificar:")
print(nueva_observacion)

# Estandarizar usando los mismos parámetros que los datos de entrenamiento
medias <- attr(enoe_cluster_scaled, "scaled:center")
desviaciones <- attr(enoe_cluster_scaled, "scaled:scale")

if(is.null(medias)) {
  medias <- colMeans(enoe_cluster_num)
  desviaciones <- apply(enoe_cluster_num, 2, sd)
}

nueva_obs_scaled <- (nueva_observacion - medias) / desviaciones

# Predicción con K-Means
# Calcular distancia euclidiana a cada centroide
centroides <- modelo_kmeans$centers
distancias_centroides <- apply(centroides, 1, function(centro) {
  sqrt(sum((nueva_obs_scaled - centro)^2))
})

cluster_predicho_kmeans <- which.min(distancias_centroides)

# Resultados de predicción K-Means
print("Predicción K-Means:")
print(paste("La nueva observación pertenece al cluster:", cluster_predicho_kmeans))
print(paste("Distancias a cada centroide:", paste(round(distancias_centroides, 3), collapse = ", ")))

# Predicción con PAM
# Calcular distancia a cada medoide
medoides <- modelo_pam$medoids
distancias_medoides <- apply(medoides, 1, function(medoide) {
  sqrt(sum((as.numeric(nueva_obs_scaled) - medoide)^2))
})

cluster_predicho_pam <- which.min(distancias_medoides)

# Resultados de predicción K-Medoides
print("Predicción K-Medoides (PAM):")
print(paste("La nueva observación pertenece al cluster:", cluster_predicho_pam))
print(paste("Distancias a cada medoide:", paste(round(distancias_medoides, 3), collapse = ", ")))

# Comparar con características promedio del cluster asignado
print("Características del cluster asignado (K-Means):")
print(caracterizacion_kmeans %>% filter(cluster_kmeans == cluster_predicho_kmeans))

print("Características del cluster asignado (PAM):")
print(caracterizacion_pam %>% filter(cluster_pam == cluster_predicho_pam))

# Función reutilizable para predecir cluster de nuevas observaciones
predecir_cluster <- function(nueva_obs, modelo_tipo = "kmeans") {
  # Estandarizar
  nueva_obs_scaled <- (nueva_obs - medias) / desviaciones

  if(modelo_tipo == "kmeans") {
    # K-Means
    distancias <- apply(centroides, 1, function(centro) {
      sqrt(sum((nueva_obs_scaled - centro)^2))
    })
    cluster <- which.min(distancias)
    metodo <- "K-Means"
  } else if(modelo_tipo == "pam") {
    # PAM
    distancias <- apply(medoides, 1, function(medoide) {
      sqrt(sum((as.numeric(nueva_obs_scaled) - medoide)^2))
    })
    cluster <- which.min(distancias)
    metodo <- "PAM"
  }

  return(list(
    cluster = cluster,
    metodo = metodo,
    distancias = distancias
  ))
}

# Ejemplo de uso de la función
prediccion_ejemplo <- predecir_cluster(nueva_observacion, modelo_tipo = "kmeans")
# Resultados de la función de predicción
print("Función de predicción:")
print(paste("Método:", prediccion_ejemplo$metodo))
print(paste("Cluster asignado:", prediccion_ejemplo$cluster))


# Resumen Final ----

# Resumen ejecutivo de todos los análisis realizados
print("=== RESUMEN DE ANÁLISIS ===")
print(paste("1. Datos ENOE cargados:", nrow(enoe), "observaciones"))
print(paste("2. Matriz de correlaciones calculada para", length(variables_numericas), "variables"))
print(paste("3. Árbol de decisión entrenado con", length(variables_modelo), "predictores"))
print("4. Modelos de regresión comparados:")
print(comparacion_modelos)
print(paste("5. Clustering K-Means con k =", k_optimo_kmeans, "clusters"))
print(paste("6. Clustering K-Medoides con k =", k_optimo_pam, "medoides"))
print("7. Función de predicción disponible: predecir_cluster()")
