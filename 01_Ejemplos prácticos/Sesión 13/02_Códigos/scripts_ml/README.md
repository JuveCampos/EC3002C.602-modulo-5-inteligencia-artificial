# Scripts de Machine Learning - Guía de Uso

Este directorio contiene scripts en R que ilustran diversos métodos de machine
learning y estadística avanzada. Todos los scripts están documentados con
teoría, código comentado y generan visualizaciones.

**IMPORTANTE:** Todos los scripts deben ejecutarse desde el directorio raíz del
proyecto (donde está el archivo `.Rproj`), ya que las rutas están configuradas
de manera relativa desde ese punto.

## Índice de Scripts por Tema

### 1. Probit/Logit (Modelos de Clasificación Binaria)
**Script:** `03_heart_disease_probit_logit.R`
- Dataset: Heart Disease
- Compara modelos Logit y Probit
- Incluye efectos marginales y bondad de ajuste
- Teoría completa sobre diferencias entre Logit y Probit

### 2. Ridge/Lasso (Regularización)
**Scripts:**
- `02_wine_red_regresion.R` (completo)
- `04_california_housing_regresion_avanzada.R`
- `09_wine_white_regresion.R`

Temas cubiertos:
- Teoría de regularización L1 (Lasso) y L2 (Ridge)
- Validación cruzada para selección de lambda
- Comparación de ambos métodos
- Selección automática de variables con Lasso

### 3. Métodos de Regresión
**Scripts:**
- `02_wine_red_regresion.R` - Regresión completa con vinos
- `04_california_housing_regresion_avanzada.R` - Regresión espacial
- `09_wine_white_regresion.R` - Regresión con vinos blancos

Métodos incluidos:
- Regresión lineal múltiple
- Regresión con regularización
- Diagnósticos de modelos
- Análisis de residuos

### 4. Métodos de Clusterización
**Script:** `05_wholesale_clustering.R`
- Dataset: Wholesale Customers
- K-means clustering
- Clustering jerárquico
- Determinación de número óptimo de clusters
- PCA para visualización
- Perfilamiento de clusters

### 5. Stepwise Regression
**Scripts:**
- `02_wine_red_regresion.R`
- `04_california_housing_regresion_avanzada.R`

Incluye:
- Forward, backward y both directions
- Criterio AIC
- Comparación con modelo completo

### 6. SVM (Support Vector Machines)
**Scripts:**
- `01_iris_clasificacion.R` - SVM multiclase
- `07_breast_cancer_svm_ensemble.R` - SVM binario avanzado

Kernels cubiertos:
- Lineal
- Radial (RBF)
- Tuning de hiperparámetros (cost, gamma)
- Visualización de fronteras de decisión

### 7. Modelo de Árbol Simple
**Script:** `01_iris_clasificacion.R`
- Árboles de decisión con rpart
- Visualización de árboles
- Interpretación de divisiones
- Parámetros de complejidad

### 8. Random Forest
**Scripts (todos incluyen Random Forest):**
- `01_iris_clasificacion.R` - Clasificación
- `02_wine_red_regresion.R` - Regresión
- `06_credit_card_xgboost_bagging.R` - Clasificación binaria
- `07_breast_cancer_svm_ensemble.R` - Alta dimensionalidad
- `08_bank_marketing_clasificacion.R` - Datos desbalanceados

Aspectos cubiertos:
- Importancia de variables
- Tuning de hiperparámetros (ntree, mtry)
- Out-of-bag error
- Diferencias con bagging

### 9. Bagging (Bootstrap Aggregating)
**Script:** `06_credit_card_xgboost_bagging.R`
- Teoría completa de bagging
- Out-of-bag error
- Comparación con Random Forest
- Reducción de varianza

### 10. XGBoost (Gradient Boosting)
**Scripts (todos incluyen XGBoost):**
- `02_wine_red_regresion.R` - Regresión
- `06_credit_card_xgboost_bagging.R` - Clasificación (más completo)
- `07_breast_cancer_svm_ensemble.R` - Alta dimensionalidad
- `08_bank_marketing_clasificacion.R` - Datos desbalanceados

Temas cubiertos:
- Teoría de gradient boosting
- Validación cruzada para número de árboles
- Tuning de hiperparámetros
- Importancia de variables
- Comparación con Random Forest
- Early stopping

## Índice de Scripts por Dataset

### 01_iris_clasificacion.R
- **Dataset:** Iris (150 obs, 4 vars)
- **Métodos:** Árboles de decisión, SVM (lineal y radial)
- **Ideal para:** Clasificación multiclase básica, visualización

### 02_wine_red_regresion.R
- **Dataset:** Wine Quality Red (1,599 obs, 11 vars)
- **Métodos:** Regresión lineal, Stepwise, Ridge, Lasso, Random Forest,
  XGBoost
- **Ideal para:** Regresión completa, regularización, ensemble methods

### 03_heart_disease_probit_logit.R
- **Dataset:** Heart Disease (303 obs, 13 vars)
- **Métodos:** Logit, Probit, comparación detallada
- **Ideal para:** Modelos de clasificación binaria paramétricos

### 04_california_housing_regresion_avanzada.R
- **Dataset:** California Housing (20,640 obs, 8 vars)
- **Métodos:** Regresión con datos espaciales, Ridge, Lasso, Stepwise,
  Random Forest
- **Ideal para:** Regresión con muchas observaciones, análisis espacial

### 05_wholesale_clustering.R
- **Dataset:** Wholesale Customers (440 obs, 6 vars de gasto)
- **Métodos:** K-means, clustering jerárquico, PCA
- **Ideal para:** Análisis de clustering, segmentación de clientes

### 06_credit_card_xgboost_bagging.R
- **Dataset:** Credit Card Default (30,000 obs, 23 vars)
- **Métodos:** Logit (baseline), Bagging, Random Forest, XGBoost
- **Ideal para:** Ensemble methods, datos desbalanceados

### 07_breast_cancer_svm_ensemble.R
- **Dataset:** Breast Cancer (569 obs, 30 vars)
- **Métodos:** SVM (lineal y radial), Lasso, Random Forest, XGBoost
- **Ideal para:** SVM con alta dimensionalidad, clasificación médica

### 08_bank_marketing_clasificacion.R
- **Dataset:** Bank Marketing (45,211 obs, 20 vars)
- **Métodos:** Logit, Random Forest, XGBoost
- **Ideal para:** Datasets grandes, datos desbalanceados

### 09_wine_white_regresion.R
- **Dataset:** Wine Quality White (4,898 obs, 11 vars)
- **Métodos:** Regresión lineal, Ridge, Lasso, Random Forest, XGBoost
- **Ideal para:** Regresión comparativa

## Estructura de los Scripts

Todos los scripts siguen esta estructura:

1. **Encabezado:** Descripción del dataset y métodos
2. **Carga de librerías:** Todas las necesarias para el análisis
3. **Carga y exploración de datos:** Estadísticas descriptivas
4. **Visualización exploratoria:** Gráficos y análisis inicial
5. **División de datos:** Train/test split
6. **Modelos:** Entrenamiento con teoría explicada
7. **Evaluación:** Métricas apropiadas para cada modelo
8. **Comparación:** Tabla y visualizaciones comparativas
9. **Resumen:** Interpretación y conclusiones

## Rutas de Archivos

Los scripts utilizan rutas relativas desde el directorio raíz del proyecto:

```r
# Datos de entrada (lectura)
"01_Datos/datasets_ml/nombre_archivo.csv"

# Visualizaciones de salida (escritura)
"03_Visualizaciones/nombre_grafico.png"
```

**NOTA:** Para que las rutas funcionen correctamente, debes ejecutar los
scripts desde el directorio raíz donde está el archivo `.Rproj`. En RStudio,
esto se hace automáticamente al abrir el proyecto.

## Visualizaciones

Todas las visualizaciones se guardan en: `03_Visualizaciones/`

Tipos de visualizaciones generadas:
- Distribuciones de variables
- Matrices de correlación
- Curvas ROC
- Importancia de variables
- Comparación de modelos
- Predicciones vs valores reales
- Mapas espaciales (California Housing)
- Dendrogramas (Clustering)
- PCA biplots

## Requisitos

### Librerías necesarias:

```r
# Manipulación de datos
install.packages("tidyverse")

# Modelos
install.packages("caret")
install.packages("e1071")          # SVM
install.packages("rpart")          # Árboles
install.packages("rpart.plot")     # Visualización árboles
install.packages("randomForest")   # Random Forest
install.packages("glmnet")         # Ridge/Lasso
install.packages("MASS")           # Stepwise, Probit
install.packages("xgboost")        # XGBoost
install.packages("ipred")          # Bagging

# Clustering
install.packages("cluster")
install.packages("factoextra")
install.packages("NbClust")
install.packages("ggdendro")

# Evaluación y visualización
install.packages("pROC")           # Curvas ROC
install.packages("corrplot")       # Correlaciones
install.packages("GGally")         # Pairplots
install.packages("viridis")        # Paletas
install.packages("gridExtra")      # Múltiples gráficos
```

## Cómo Usar los Scripts

1. **Abrir el proyecto en RStudio:**
   - Haz doble clic en el archivo `.Rproj` en el directorio raíz
   - Esto configura automáticamente el directorio de trabajo

2. **Ejecutar el script completo:**
   ```r
   source("02_Códigos/scripts_ml/01_iris_clasificacion.R")
   ```

3. **O ejecuta sección por sección** en RStudio para mejor comprensión

4. **Revisa las visualizaciones** generadas en `03_Visualizaciones/`

## Notas Importantes

- **Semilla aleatoria:** Todos los scripts usan `set.seed(42)` para
  reproducibilidad
- **Comentarios extensos:** Cada método incluye teoría y explicaciones
- **Estilo tidyverse:** Se prioriza sintaxis tidyverse y pipes (`%>%`)
- **Código secuencial:** Sin funciones intermedias para facilitar aprendizaje
- **Mensajes informativos:** Se usa `print()` con interpolación de variables
- **Validación cruzada:** Muchos modelos usan CV para tuning de hiperparámetros

## Temas Teóricos Cubiertos

Cada script incluye explicaciones detalladas de:

- **Fundamentos matemáticos** del método
- **Función objetivo** que se optimiza
- **Hiperparámetros** y cómo afectan el modelo
- **Ventajas y desventajas** de cada método
- **Cuándo usar** cada método
- **Comparación** con métodos alternativos
- **Interpretación** de resultados

## Recomendaciones de Uso Pedagógico

### Para aprender Probit/Logit:
→ Empezar con `03_heart_disease_probit_logit.R`

### Para aprender Regularización:
→ Empezar con `02_wine_red_regresion.R` (secciones Ridge/Lasso)

### Para aprender Clustering:
→ Usar `05_wholesale_clustering.R` (completo y didáctico)

### Para aprender SVM:
→ Empezar con `01_iris_clasificacion.R` (simple)
→ Continuar con `07_breast_cancer_svm_ensemble.R` (avanzado)

### Para aprender Ensemble Methods:
→ Empezar con `06_credit_card_xgboost_bagging.R` (teoría completa)
→ Comparar con otros scripts para ver aplicaciones

### Para entender diferencias entre métodos:
→ Cualquier script con comparación final (todos tienen)

## Datos

Los datasets están en: `01_Datos/datasets_ml/`

Ver `01_Datos/datasets_ml/README.md` para descripción completa de cada dataset.

## Estilo de Código

El código sigue estos lineamientos:

- **Framework:** tidyverse como paradigma principal
- **Pipes:** Uso de `%>%` para encadenar operaciones
- **Mensajes:** `print()` solo cuando interpola variables, comentarios `#`
  para mensajes simples
- **Estructura:** Código secuencial sin funciones auxiliares innecesarias
- **Iteración:** Prioriza `lapply()` sobre `for()`
- **Formato:** 2 espacios de indentación, 80 caracteres máximo por línea
- **Asignación:** Usa `<-` (no `=`) para variables
- **Visualizaciones:** Exclusivamente ggplot2

---

**Última actualización:** Noviembre 2025
**Curso:** EC3002C.602 - Módulo 5: Inteligencia Artificial
