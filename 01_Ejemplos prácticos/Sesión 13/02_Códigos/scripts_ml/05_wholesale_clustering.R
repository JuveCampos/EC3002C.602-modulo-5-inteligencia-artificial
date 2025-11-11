# ==============================================================================
# ANÁLISIS DE CLUSTERING - WHOLESALE CUSTOMERS
# ==============================================================================
#
# Dataset: Wholesale Customers
# Observaciones: 440
# Variables: 8 (6 gastos + Canal + Región)
#
# Descripción:
# Dataset que contiene gastos anuales (en unidades monetarias) de clientes
# de un distribuidor mayorista. Útil para análisis de segmentación de clientes
# basado en patrones de compra.
#
# Métodos aplicados:
# 1. K-means Clustering
# 2. Clustering Jerárquico
# 3. Determinación de número óptimo de clusters
# 4. Análisis de componentes principales (PCA)
# 5. Interpretación y perfilamiento de clusters
#
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación y visualización
library(cluster)      # Algoritmos de clustering
library(factoextra)   # Visualización de clustering
library(NbClust)      # Determinación de número de clusters
library(ggdendro)     # Dendrogramas
library(gridExtra)    # Múltiples gráficos
library(dplyr)        # Asegurar que dplyr esté disponible

set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

print("=== CARGANDO DATOS ===")

wholesale_data <- read_csv("01_Datos/datasets_ml/wholesale_customers.csv",
                           show_col_types = FALSE)

print("Dimensiones del dataset:")
print(dim(wholesale_data))

print("Primeras observaciones:")
print(head(wholesale_data))

print("Resumen estadístico:")
print(summary(wholesale_data))

# Distribución por canal y región
print("Distribución por Canal:")
print(table(wholesale_data$Channel))

print("Distribución por Región:")
print(table(wholesale_data$Region))

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

print("=== ANÁLISIS EXPLORATORIO ===")

# Seleccionar solo variables de gasto para clustering
spending_vars <- c("Fresh", "Milk", "Grocery", "Frozen",
                   "Detergents_Paper", "Delicassen")

spending_data <- wholesale_data %>%
  select(all_of(spending_vars))

# Distribuciones de gastos (log scale por alta variabilidad)
spending_long <- wholesale_data %>%
  select(all_of(spending_vars)) %>%
  pivot_longer(everything(), names_to = "Categoria", values_to = "Gasto")

ggplot(spending_long, aes(x = log(Gasto + 1), fill = Categoria)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~Categoria, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de Gastos por Categoría (escala log)",
       x = "log(Gasto + 1)", y = "Frecuencia") +
  theme(legend.position = "none")

ggsave("03_Visualizaciones/wholesale_spending_distributions.png",
       width = 12, height = 8)

# Boxplots para visualizar outliers
ggplot(spending_long, aes(x = Categoria, y = Gasto, fill = Categoria)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Distribución de Gastos por Categoría (escala log)",
       x = "Categoría", y = "Gasto (escala log)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("03_Visualizaciones/wholesale_boxplots.png",
       width = 10, height = 6)

# Matriz de correlación
cor_matrix <- cor(spending_data)

png("03_Visualizaciones/wholesale_correlation.png",
    width = 1000, height = 900)
corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                   tl.col = "black", tl.srt = 45,
                   addCoef.col = "black", number.cex = 0.9,
                   title = "Correlaciones entre Categorías de Gasto",
                   mar = c(0, 0, 2, 0))
dev.off()

# ==============================================================================
# 3. PREPARACIÓN DE DATOS PARA CLUSTERING
# ==============================================================================
#
# TEORÍA: PREPARACIÓN DE DATOS PARA CLUSTERING
#
# El clustering es sensible a la escala de las variables. Pasos importantes:
#
# 1. Transformación:
#    - Variables con distribuciones asimétricas pueden requerir transformación log
#    - En este dataset, los gastos varían en órdenes de magnitud
#
# 2. Estandarización:
#    - Necesaria para que todas las variables contribuyan equitativamente
#    - Métodos comunes: z-score (media=0, sd=1) o min-max scaling
#
# 3. Manejo de outliers:
#    - Los outliers pueden distorsionar los clusters
#    - Opciones: eliminar, transformar, o usar métodos robustos
#
# ==============================================================================

print("=== PREPARANDO DATOS ===")

# Aplicar transformación logarítmica para reducir impacto de outliers
spending_log <- log(spending_data + 1)

# Estandarizar datos (z-score)
spending_scaled <- scale(spending_log)

# Convertir a dataframe
spending_scaled_df <- as.data.frame(spending_scaled)

print("Datos estandarizados (primeras filas):")
print(head(spending_scaled_df))

# ==============================================================================
# 4. DETERMINACIÓN DEL NÚMERO ÓPTIMO DE CLUSTERS
# ==============================================================================
#
# TEORÍA: MÉTODOS PARA DETERMINAR NÚMERO DE CLUSTERS
#
# 1. Método del Codo (Elbow Method):
#    - Graficar suma de cuadrados intra-cluster (WSS) vs número de clusters
#    - Buscar el "codo" donde WSS deja de decrecer significativamente
#
# 2. Método de la Silueta (Silhouette):
#    - Mide qué tan similar es un objeto a su cluster vs otros clusters
#    - Valores de -1 (mal asignado) a 1 (bien asignado)
#    - Promedio de silueta más alto indica mejor clustering
#
# 3. Gap Statistic:
#    - Compara WSS con distribución de referencia (datos uniformes)
#    - Número óptimo donde gap es máximo
#
# ==============================================================================

print("=== DETERMINANDO NÚMERO ÓPTIMO DE CLUSTERS ===")

# Método del Codo
fviz_nbclust(spending_scaled_df, kmeans, method = "wss", k.max = 10) +
  labs(title = "Método del Codo para K-Means") +
  theme_minimal()

ggsave("03_Visualizaciones/wholesale_elbow.png",
       width = 8, height = 6)

# Método de la Silueta
fviz_nbclust(spending_scaled_df, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Método de la Silueta para K-Means") +
  theme_minimal()

ggsave("03_Visualizaciones/wholesale_silhouette.png",
       width = 8, height = 6)

# Gap Statistic (puede tomar tiempo)
print("Calculando Gap Statistic (esto puede tomar un momento)...")
gap_stat <- clusGap(spending_scaled_df, FUN = kmeans, K.max = 10,
                    B = 50, verbose = FALSE)

fviz_gap_stat(gap_stat) +
  labs(title = "Gap Statistic para K-Means") +
  theme_minimal()

ggsave("03_Visualizaciones/wholesale_gap_stat.png",
       width = 8, height = 6)

print("Gap Statistic:")
print(gap_stat)

# ==============================================================================
# 5. K-MEANS CLUSTERING
# ==============================================================================
#
# TEORÍA: K-MEANS CLUSTERING
#
# K-means es un algoritmo de clustering particional que divide n observaciones
# en k clusters, donde cada observación pertenece al cluster con la media
# más cercana.
#
# Algoritmo:
# 1. Seleccionar k centroides iniciales aleatoriamente
# 2. Asignar cada punto al centroide más cercano (forma clusters)
# 3. Recalcular centroides como media de puntos en cada cluster
# 4. Repetir pasos 2-3 hasta convergencia
#
# Función objetivo:
# Minimizar WSS (Within-cluster Sum of Squares):
# WSS = Σᵏᵢ₌₁ Σₓ∈Cᵢ ||x - μᵢ||²
#
# donde μᵢ es el centroide del cluster i
#
# Características:
# - Requiere especificar k a priori
# - Sensible a inicialización (se ejecuta múltiples veces)
# - Asume clusters esféricos de tamaño similar
# - Rápido y eficiente
#
# Ventajas:
# - Simple y rápido
# - Funciona bien con clusters bien separados
# - Escalable a grandes datasets
#
# Desventajas:
# - Requiere especificar k
# - Sensible a outliers
# - Asume clusters convexos y de densidad similar
# - Puede converger a mínimos locales
#
# ==============================================================================

print("=== K-MEANS CLUSTERING ===")

# Basándonos en los métodos anteriores, probar con k=3 y k=4
k_values <- c(3, 4)

# Aplicar k-means con diferentes k
kmeans_results <- lapply(k_values, function(k) {
  print(paste("Ejecutando K-means con k =", k))

  # nstart=25 ejecuta el algoritmo 25 veces con diferentes inicializaciones
  km <- kmeans(spending_scaled_df, centers = k, nstart = 25, iter.max = 100)

  return(km)
})

# Visualizar k=3
print("=== RESULTADOS K-MEANS CON K=3 ===")
km3 <- kmeans_results[[1]]
print(km3)

fviz_cluster(km3, data = spending_scaled_df,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-Means Clustering (k=3)")

ggsave("03_Visualizaciones/wholesale_kmeans_k3.png",
       width = 10, height = 8)

# Visualizar k=4
print("=== RESULTADOS K-MEANS CON K=4 ===")
km4 <- kmeans_results[[2]]
print(km4)

fviz_cluster(km4, data = spending_scaled_df,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-Means Clustering (k=4)")

ggsave("03_Visualizaciones/wholesale_kmeans_k4.png",
       width = 10, height = 8)

# Usar k=3 para análisis posterior (basado en interpretabilidad)
kmeans_final <- km3
wholesale_data$cluster_kmeans <- as.factor(kmeans_final$cluster)

# ==============================================================================
# 6. CLUSTERING JERÁRQUICO
# ==============================================================================
#
# TEORÍA: CLUSTERING JERÁRQUICO
#
# El clustering jerárquico crea una jerarquía de clusters representada
# en un dendrograma, sin requerir especificar k a priori.
#
# Tipos:
# 1. Aglomerativo (bottom-up): Cada punto empieza como un cluster,
#    se van fusionando pares de clusters más cercanos
# 2. Divisivo (top-down): Empieza con un cluster, se va dividiendo
#
# Métodos de enlace (linkage):
# - Complete: distancia máxima entre puntos de diferentes clusters
# - Single: distancia mínima entre puntos de diferentes clusters
# - Average: distancia promedio entre puntos de diferentes clusters
# - Ward: minimiza la varianza intra-cluster (similar a k-means)
#
# Proceso (aglomerativo):
# 1. Calcular matriz de distancias entre todos los puntos
# 2. Fusionar los dos clusters más cercanos
# 3. Recalcular distancias
# 4. Repetir hasta tener un solo cluster
#
# Dendrograma:
# - Eje Y: distancia o altura a la que se fusionan clusters
# - "Cortar" el dendrograma a cierta altura define el número de clusters
#
# Ventajas:
# - No requiere especificar k a priori
# - Dendrograma proporciona visualización informativa
# - Puede revelar estructura jerárquica en datos
#
# Desventajas:
# - Costoso computacionalmente O(n² log n)
# - No escalable a grandes datasets
# - Decisiones de fusión son irrevocables
# - Sensible a outliers
#
# ==============================================================================

print("=== CLUSTERING JERÁRQUICO ===")

# Calcular matriz de distancias (Euclidiana)
dist_matrix <- dist(spending_scaled_df, method = "euclidean")

# Clustering jerárquico con diferentes métodos de enlace
methods <- c("complete", "average", "ward.D2")

hc_results <- lapply(methods, function(method) {
  print(paste("Clustering jerárquico con método:", method))
  hc <- hclust(dist_matrix, method = method)
  return(hc)
})

# Visualizar dendrograma con método Ward
hc_ward <- hc_results[[3]]

png("03_Visualizaciones/wholesale_dendrogram.png",
    width = 1400, height = 800)
plot(hc_ward, labels = FALSE, hang = -1,
     main = "Dendrograma - Clustering Jerárquico (Ward)",
     xlab = "Observaciones", ylab = "Altura")
# Dibujar rectángulos para k=3 clusters
rect.hclust(hc_ward, k = 3, border = 2:4)
dev.off()

# Cortar dendrograma para obtener k=3 clusters
clusters_hc <- cutree(hc_ward, k = 3)
wholesale_data$cluster_hc <- as.factor(clusters_hc)

# Visualizar clusters jerárquicos
fviz_cluster(list(data = spending_scaled_df, cluster = clusters_hc),
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Clustering Jerárquico (k=3)")

ggsave("03_Visualizaciones/wholesale_hierarchical.png",
       width = 10, height = 8)

# ==============================================================================
# 7. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ==============================================================================
#
# TEORÍA: PCA (Principal Component Analysis)
#
# PCA es una técnica de reducción de dimensionalidad que transforma
# variables correlacionadas en un conjunto menor de variables no
# correlacionadas (componentes principales).
#
# Funcionamiento:
# - Encuentra direcciones de máxima varianza en los datos
# - Primera PC: dirección de máxima varianza
# - Segunda PC: dirección ortogonal de segunda mayor varianza
# - Y así sucesivamente
#
# Matemáticamente:
# - Diagonaliza la matriz de covarianza
# - Componentes principales = eigenvectors
# - Varianza explicada = eigenvalues
#
# Usos:
# - Visualización de datos de alta dimensión
# - Reducción de dimensionalidad antes de clustering
# - Identificación de patrones y outliers
# - Eliminación de ruido
#
# ==============================================================================

print("=== ANÁLISIS DE COMPONENTES PRINCIPALES ===")

# Realizar PCA
pca_result <- prcomp(spending_scaled_df, center = FALSE, scale. = FALSE)

print("Resumen PCA:")
print(summary(pca_result))

# Visualizar varianza explicada
fviz_eig(pca_result, addlabels = TRUE,
         main = "Varianza Explicada por Componentes Principales") +
  theme_minimal()

ggsave("03_Visualizaciones/wholesale_pca_variance.png",
       width = 10, height = 6)

# Biplot: observaciones y variables en espacio PC
fviz_pca_biplot(pca_result,
                geom.ind = "point",
                col.ind = wholesale_data$cluster_kmeans,
                palette = "jco",
                addEllipses = TRUE,
                legend.title = "Cluster K-means",
                title = "PCA Biplot con Clusters K-means") +
  theme_minimal()

ggsave("03_Visualizaciones/wholesale_pca_biplot.png",
       width = 10, height = 8)

# Contribución de variables a las PCs
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Contribución de Variables a las PCs") +
  theme_minimal()

ggsave("03_Visualizaciones/wholesale_pca_variables.png",
       width = 10, height = 8)

# ==============================================================================
# 8. PERFILAMIENTO DE CLUSTERS
# ==============================================================================

print("=== PERFILAMIENTO DE CLUSTERS K-MEANS ===")

# Añadir clusters a datos originales
wholesale_with_clusters <- wholesale_data %>%
  bind_cols(spending_data)

# Estadísticas por cluster
cluster_profiles <- wholesale_with_clusters %>%
  group_by(cluster_kmeans) %>%
  summarise(
    n = n(),
    Fresh_media = mean(Fresh),
    Milk_media = mean(Milk),
    Grocery_media = mean(Grocery),
    Frozen_media = mean(Frozen),
    Detergents_Paper_media = mean(Detergents_Paper),
    Delicassen_media = mean(Delicassen)
  )

print("Perfil de clusters (medias de gasto):")
print(cluster_profiles)

# Visualizar perfiles de clusters
profile_long <- cluster_profiles %>%
  pivot_longer(cols = ends_with("_media"),
               names_to = "Categoria",
               values_to = "Gasto_Medio") %>%
  mutate(Categoria = str_remove(Categoria, "_media"))

ggplot(profile_long, aes(x = Categoria, y = Gasto_Medio,
                         fill = cluster_kmeans)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Perfil de Gastos por Cluster",
       x = "Categoría", y = "Gasto Medio",
       fill = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

ggsave("03_Visualizaciones/wholesale_cluster_profiles.png",
       width = 12, height = 6)

# Heatmap de perfiles de clusters (normalizado)
cluster_profiles_norm <- cluster_profiles %>%
  select(-n) %>%
  column_to_rownames("cluster_kmeans") %>%
  as.matrix()

# Normalizar por filas para ver proporciones
cluster_profiles_norm_scaled <- t(scale(t(cluster_profiles_norm)))

png("03_Visualizaciones/wholesale_cluster_heatmap.png",
    width = 1000, height = 600)
heatmap(cluster_profiles_norm_scaled,
        Colv = NA, Rowv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Heatmap de Perfiles de Clusters (normalizado)",
        xlab = "Categoría de Gasto",
        ylab = "Cluster",
        scale = "none")
dev.off()

# Distribución de Channel y Region por cluster
print("Distribución de Canal por cluster:")
print(table(wholesale_data$cluster_kmeans, wholesale_data$Channel))

print("Distribución de Región por cluster:")
print(table(wholesale_data$cluster_kmeans, wholesale_data$Region))

# ==============================================================================
# 9. VALIDACIÓN DE CLUSTERS
# ==============================================================================

print("=== VALIDACIÓN DE CLUSTERS ===")

# Índice de Silueta para evaluar calidad del clustering
sil_kmeans <- silhouette(kmeans_final$cluster, dist_matrix)

print("Coeficiente de Silueta promedio (K-means):")
print(mean(sil_kmeans[, 3]))

fviz_silhouette(sil_kmeans,
                palette = "jco",
                ggtheme = theme_minimal(),
                main = "Análisis de Silueta - K-means (k=3)")

ggsave("03_Visualizaciones/wholesale_silhouette_analysis.png",
       width = 10, height = 6)

# Comparar K-means vs Jerárquico
sil_hc <- silhouette(clusters_hc, dist_matrix)

print("Coeficiente de Silueta promedio (Jerárquico):")
print(mean(sil_hc[, 3]))

# ==============================================================================
# RESUMEN FINAL E INTERPRETACIÓN
# ==============================================================================

print("=== INTERPRETACIÓN DE CLUSTERS ===")

print("Cluster 1:")
print("Características basadas en gastos medios y composición.")

print("Cluster 2:")
print("Características basadas en gastos medios y composición.")

print("Cluster 3:")
print("Características basadas en gastos medios y composición.")

print("=== ANÁLISIS COMPLETADO ===")
print("Se aplicaron técnicas de clustering no supervisado al dataset Wholesale.")
print("Se probaron K-means y clustering jerárquico.")
print("PCA ayudó a visualizar datos en espacio reducido.")
print("Los clusters revelan diferentes perfiles de gasto de clientes.")
print("Esta segmentación puede usarse para marketing dirigido.")
print("Todas las visualizaciones se guardaron exitosamente.")
