# Datasets para Ejemplos de Machine Learning

Este directorio contiene datasets cuidadosamente seleccionados para ilustrar diversos métodos de machine learning y estadística avanzada. Todos los datasets pesan menos de 100 MB y provienen de fuentes confiables como UCI Machine Learning Repository y sklearn.

**Tamaño total del directorio:** 8.1 MB

---

## 1. Iris Dataset
**Archivo:** `iris.csv`
**Tamaño:** 3.8 KB
**Observaciones:** 150
**Variables:** 5

### Descripción
El dataset Iris es un clásico en machine learning, creado por Ronald Fisher en 1936. Contiene medidas de flores de iris de tres especies diferentes.

### Variables
- `sepal_length`: Longitud del sépalo (cm)
- `sepal_width`: Ancho del sépalo (cm)
- `petal_length`: Longitud del pétalo (cm)
- `petal_width`: Ancho del pétalo (cm)
- `species`: Especie de iris (Setosa, Versicolor, Virginica)

### Aplicaciones recomendadas
- Clasificación multiclase
- Árboles de decisión simples
- SVM
- Análisis discriminante
- Visualización de técnicas de clasificación

### Fuente
UCI Machine Learning Repository
Licencia: CC BY 4.0

---

## 2. Wine Quality Dataset (Vino Tinto)
**Archivo:** `winequality-red.csv`
**Tamaño:** 82 KB
**Observaciones:** 1,599
**Variables:** 12

### Descripción
Dataset relacionado con vinos tintos portugueses (vinho verde). Contiene propiedades fisicoquímicas y una calificación de calidad.

### Variables
- `fixed acidity`: Acidez fija
- `volatile acidity`: Acidez volátil
- `citric acid`: Ácido cítrico
- `residual sugar`: Azúcar residual
- `chlorides`: Cloruros
- `free sulfur dioxide`: Dióxido de azufre libre
- `total sulfur dioxide`: Dióxido de azufre total
- `density`: Densidad
- `pH`: pH
- `sulphates`: Sulfatos
- `alcohol`: Contenido de alcohol
- `quality`: Calidad (score de 0 a 10)

### Aplicaciones recomendadas
- Regresión (predecir calidad)
- Ridge/Lasso (regularización)
- Random Forest
- XGBoost
- Stepwise regression

### Fuente
UCI Machine Learning Repository
Cortez et al., 2009
Licencia: CC BY 4.0

---

## 3. Wine Quality Dataset (Vino Blanco)
**Archivo:** `winequality-white.csv`
**Tamaño:** 258 KB
**Observaciones:** 4,898
**Variables:** 12

### Descripción
Similar al dataset de vino tinto, pero para vinos blancos portugueses.

### Variables
Las mismas 12 variables que el dataset de vino tinto.

### Aplicaciones recomendadas
- Regresión
- Ridge/Lasso
- Random Forest
- Comparación con vino tinto
- Análisis de conjuntos de datos relacionados

### Fuente
UCI Machine Learning Repository
Cortez et al., 2009
Licencia: CC BY 4.0

---

## 4. Heart Disease Dataset
**Archivo:** `heart.csv`
**Tamaño:** 11 KB
**Observaciones:** 303
**Variables:** 14

### Descripción
Dataset de enfermedades cardíacas de la Cleveland Clinic. Utilizado para predecir la presencia de enfermedad cardíaca.

### Variables
- `age`: Edad
- `sex`: Sexo (1 = masculino, 0 = femenino)
- `cp`: Tipo de dolor de pecho (0-3)
- `trestbps`: Presión arterial en reposo
- `chol`: Colesterol sérico (mg/dl)
- `fbs`: Azúcar en sangre en ayunas > 120 mg/dl
- `restecg`: Resultados electrocardiográficos en reposo
- `thalach`: Frecuencia cardíaca máxima alcanzada
- `exang`: Angina inducida por ejercicio
- `oldpeak`: Depresión de ST inducida por ejercicio
- `slope`: Pendiente del segmento ST
- `ca`: Número de vasos principales (0-3)
- `thal`: Talasemia
- `target`: Presencia de enfermedad cardíaca (0 = no, 1 = sí)

### Aplicaciones recomendadas
- **Probit/Logit** (clasificación binaria)
- SVM
- Árboles de decisión
- Random Forest
- Análisis de clasificación médica

### Fuente
UCI Machine Learning Repository
Cleveland Clinic Foundation
Licencia: CC BY 4.0

---

## 5. Breast Cancer Wisconsin (Diagnostic)
**Archivo:** `breast_cancer.csv`
**Tamaño:** 122 KB
**Observaciones:** 569
**Variables:** 32

### Descripción
Dataset de diagnóstico de cáncer de mama. Las características se calculan a partir de imágenes digitalizadas de aspirados con aguja fina (FNA) de masas mamarias.

### Variables
- `id`: Identificador
- `diagnosis`: Diagnóstico (M = maligno, B = benigno)
- 30 características numéricas que describen:
  - `radius_mean`, `texture_mean`, `perimeter_mean`, `area_mean`, `smoothness_mean`
  - `compactness_mean`, `concavity_mean`, `concave_points_mean`, `symmetry_mean`, `fractal_dimension_mean`
  - Y sus valores de error estándar (_se) y peor (_worst) para cada característica

### Aplicaciones recomendadas
- **Probit/Logit** (clasificación binaria)
- **SVM** (clasificación con alta dimensionalidad)
- Ridge/Lasso (selección de características)
- Random Forest
- XGBoost
- Análisis de componentes principales

### Fuente
UCI Machine Learning Repository
Dr. William H. Wolberg, University of Wisconsin Hospitals
Licencia: CC BY 4.0

---

## 6. California Housing Dataset
**Archivo:** `california_housing.csv`
**Tamaño:** 1.8 MB
**Observaciones:** 20,640
**Variables:** 9

### Descripción
Dataset de precios de viviendas en California basado en el censo de 1990. Contiene información a nivel de bloques de censo.

### Variables
- `MedInc`: Ingreso medio del bloque
- `HouseAge`: Edad media de las casas
- `AveRooms`: Número promedio de habitaciones
- `AveBedrms`: Número promedio de dormitorios
- `Population`: Población del bloque
- `AveOccup`: Número promedio de ocupantes por hogar
- `Latitude`: Latitud
- `Longitude`: Longitud
- `MedHouseVal`: Valor medio de las viviendas (variable objetivo, en $100,000)

### Aplicaciones recomendadas
- **Regresión lineal**
- **Ridge/Lasso** (regularización)
- **Stepwise regression**
- Random Forest para regresión
- XGBoost para regresión
- Análisis espacial

### Fuente
StatLib Repository
Disponible en scikit-learn
Licencia: BSD

---

## 7. Bank Marketing Dataset
**Archivo:** `bank_marketing.csv`
**Tamaño:** 3.2 MB
**Observaciones:** 45,211
**Variables:** 21

### Descripción
Datos relacionados con campañas de marketing directo (llamadas telefónicas) de una institución bancaria portuguesa. El objetivo es predecir si el cliente suscribirá un depósito a plazo.

### Variables
**Datos del cliente:**
- `age`: Edad
- `job`: Tipo de trabajo
- `marital`: Estado civil
- `education`: Nivel educativo
- `default`: ¿Tiene crédito en default?
- `housing`: ¿Tiene préstamo de vivienda?
- `loan`: ¿Tiene préstamo personal?

**Campaña actual:**
- `contact`: Tipo de comunicación
- `month`: Último mes de contacto
- `day_of_week`: Último día de contacto
- `duration`: Duración del último contacto (segundos)
- `campaign`: Número de contactos en esta campaña
- `pdays`: Días desde el último contacto de campaña anterior
- `previous`: Número de contactos antes de esta campaña
- `poutcome`: Resultado de la campaña anterior

**Contexto económico:**
- `emp.var.rate`: Tasa de variación de empleo
- `cons.price.idx`: Índice de precios al consumidor
- `cons.conf.idx`: Índice de confianza del consumidor
- `euribor3m`: Tasa euribor 3 meses
- `nr.employed`: Número de empleados

**Variable objetivo:**
- `y`: ¿El cliente suscribió un depósito a plazo? (sí/no)

### Aplicaciones recomendadas
- **Probit/Logit** (clasificación binaria)
- Random Forest
- XGBoost
- **Bagging**
- Análisis de datos desbalanceados
- **Clustering** (segmentación de clientes)

### Fuente
UCI Machine Learning Repository
Moro et al., 2014
Licencia: CC BY 4.0

---

## 8. Credit Card Default Dataset
**Archivo:** `credit_card_default.csv`
**Tamaño:** 2.6 MB
**Observaciones:** 30,000
**Variables:** 24

### Descripción
Dataset de default de tarjetas de crédito de clientes en Taiwán (abril-septiembre 2005). Útil para predecir si un cliente incumplirá con el pago.

### Variables
**Información demográfica:**
- `LIMIT_BAL`: Monto del crédito dado
- `SEX`: Género (1 = masculino, 2 = femenino)
- `EDUCATION`: Educación (1 = posgrado, 2 = universidad, 3 = secundaria, 4 = otros)
- `MARRIAGE`: Estado civil (1 = casado, 2 = soltero, 3 = otros)
- `AGE`: Edad

**Historial de pagos (6 meses):**
- `PAY_0` a `PAY_6`: Estado de reembolso (septiembre a abril)

**Montos de facturas (6 meses):**
- `BILL_AMT1` a `BILL_AMT6`: Monto de la factura (septiembre a abril)

**Montos de pagos (6 meses):**
- `PAY_AMT1` a `PAY_AMT6`: Monto del pago anterior (septiembre a abril)

**Variable objetivo:**
- `default.payment.next.month`: Default de pago (1 = sí, 0 = no)

### Aplicaciones recomendadas
- **Probit/Logit**
- **SVM**
- Random Forest
- **XGBoost** (excelente performance)
- **Bagging**
- Análisis de riesgo crediticio
- Manejo de datos desbalanceados

### Fuente
UCI Machine Learning Repository
Yeh & Lien, 2009
Licencia: CC BY 4.0

---

## 9. Wholesale Customers Dataset
**Archivo:** `wholesale_customers.csv`
**Tamaño:** 14 KB
**Observaciones:** 440
**Variables:** 8

### Descripción
Dataset que contiene gastos anuales (en unidades monetarias) de clientes de un distribuidor mayorista. Útil para análisis de segmentación de clientes.

### Variables
- `Channel`: Canal (1 = Horeca (Hotel/Restaurant/Café), 2 = Retail)
- `Region`: Región (1 = Lisbon, 2 = Oporto, 3 = Other)
- `Fresh`: Gasto anual en productos frescos
- `Milk`: Gasto anual en productos lácteos
- `Grocery`: Gasto anual en productos de almacén
- `Frozen`: Gasto anual en productos congelados
- `Detergents_Paper`: Gasto anual en detergentes y papel
- `Delicassen`: Gasto anual en productos delicatessen

### Aplicaciones recomendadas
- **Clustering** (K-means, clustering jerárquico)
- Análisis de componentes principales
- Segmentación de clientes
- Análisis exploratorio multivariado

### Fuente
UCI Machine Learning Repository
Margarida G. M. S. Cardoso, Instituto Universitário de Lisboa
Licencia: CC BY 4.0

---

## Resumen de Aplicaciones por Tema

### 1. Probit/Logit
- Heart Disease (`heart.csv`)
- Breast Cancer (`breast_cancer.csv`)
- Bank Marketing (`bank_marketing.csv`)
- Credit Card Default (`credit_card_default.csv`)

### 2. Ridge/Lasso
- Wine Quality Red (`winequality-red.csv`)
- Wine Quality White (`winequality-white.csv`)
- California Housing (`california_housing.csv`)
- Breast Cancer (`breast_cancer.csv`)

### 3. Métodos de Regresión
- California Housing (`california_housing.csv`)
- Wine Quality (`winequality-red.csv`, `winequality-white.csv`)

### 4. Métodos de Clusterización
- Wholesale Customers (`wholesale_customers.csv`)
- Bank Marketing (`bank_marketing.csv`)
- Iris (`iris.csv`)

### 5. Stepwise Regression
- California Housing (`california_housing.csv`)
- Wine Quality (`winequality-red.csv`, `winequality-white.csv`)
- Breast Cancer (`breast_cancer.csv`)

### 6. SVM (Support Vector Machines)
- Breast Cancer (`breast_cancer.csv`)
- Heart Disease (`heart.csv`)
- Iris (`iris.csv`)
- Credit Card Default (`credit_card_default.csv`)

### 7. Árboles de Decisión Simples
- Iris (`iris.csv`)
- Heart Disease (`heart.csv`)
- Wine Quality (`winequality-red.csv`)

### 8. Random Forest
- Wine Quality (`winequality-red.csv`, `winequality-white.csv`)
- Bank Marketing (`bank_marketing.csv`)
- Credit Card Default (`credit_card_default.csv`)
- Breast Cancer (`breast_cancer.csv`)

### 9. Bagging
- Bank Marketing (`bank_marketing.csv`)
- Credit Card Default (`credit_card_default.csv`)
- Breast Cancer (`breast_cancer.csv`)

### 10. XGBoost
- Credit Card Default (`credit_card_default.csv`)
- Bank Marketing (`bank_marketing.csv`)
- California Housing (`california_housing.csv`)
- Wine Quality (`winequality-red.csv`, `winequality-white.csv`)

---

## Notas Importantes

### Preprocesamiento
Algunos datasets pueden requerir preprocesamiento antes de su uso:
- **Variables categóricas:** Iris (species), Bank Marketing (job, marital, etc.), Credit Card Default (SEX, EDUCATION)
- **Datos faltantes:** Verificar cada dataset antes de usar
- **Escalamiento:** Recomendado para SVM, Redes Neuronales, y métodos basados en distancia
- **Datos desbalanceados:** Bank Marketing y Credit Card Default tienen clases desbalanceadas

### Licencias
Todos los datasets están bajo licencias abiertas (principalmente CC BY 4.0 o BSD) que permiten su uso con fines educativos y de investigación.

### Referencias
- UCI Machine Learning Repository: https://archive.ics.uci.edu/
- Scikit-learn Datasets: https://scikit-learn.org/stable/datasets.html

---

**Última actualización:** Noviembre 2025
**Curso:** EC3002C.602 - Módulo 5: Inteligencia Artificial
