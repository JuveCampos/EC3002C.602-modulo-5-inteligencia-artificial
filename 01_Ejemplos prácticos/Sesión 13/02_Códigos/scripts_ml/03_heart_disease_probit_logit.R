# ==============================================================================
# MODELOS PROBIT Y LOGIT - HEART DISEASE DATASET
# ==============================================================================
#
# Dataset: Heart Disease
# Observaciones: 303
# Variables: 14 (13 predictoras + 1 objetivo)
#
# Descripción:
# Dataset de enfermedades cardíacas de la Cleveland Clinic. Utilizado para
# predecir la presencia de enfermedad cardíaca basándose en características
# clínicas y de laboratorio.
#
# Métodos aplicados:
# 1. Regresión Logística (Logit)
# 2. Regresión Probit
# 3. Comparación de modelos
# 4. Evaluación de capacidad predictiva
#
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación y visualización de datos
library(caret)        # Evaluación de modelos
library(pROC)         # Curvas ROC
library(MASS)         # Modelos estadísticos avanzados
library(dplyr)        # Recargar dplyr para evitar conflicto con MASS::select
library(ggplot2)      # Visualización
library(gridExtra)    # Múltiples gráficos

# Establecer semilla para reproducibilidad
set.seed(42)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

print("=== CARGANDO DATOS ===")

# Cargar datos
heart_data <- read_csv("01_Datos/datasets_ml/heart.csv",
                       show_col_types = FALSE)

print("Dimensiones del dataset:")
print(dim(heart_data))

print("Primeras observaciones:")
print(head(heart_data))

print("Resumen estadístico:")
print(summary(heart_data))

# Verificar valores faltantes
print("Valores faltantes:")
print(colSums(is.na(heart_data)))

# Distribución de la variable objetivo
print("Distribución de target (0=no enfermedad, 1=enfermedad):")
print(table(heart_data$target))
print(prop.table(table(heart_data$target)))

# Convertir variables categóricas a factores
heart_data <- heart_data %>%
  mutate(
    target = as.factor(target),
    sex = as.factor(sex),
    cp = as.factor(cp),
    fbs = as.factor(fbs),
    restecg = as.factor(restecg),
    exang = as.factor(exang),
    slope = as.factor(slope),
    ca = as.factor(ca),
    thal = as.factor(thal)
  )

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

print("=== ANÁLISIS EXPLORATORIO ===")

# Distribución de edad por target
ggplot(heart_data, aes(x = target, y = age, fill = target)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Edad por Presencia de Enfermedad",
       x = "Target (0=No, 1=Sí)", y = "Edad") +
  scale_fill_manual(values = c("lightblue", "coral"))

ggsave("03_Visualizaciones/heart_age_distribution.png",
       width = 8, height = 6)

# Género vs enfermedad
gender_table <- table(heart_data$sex, heart_data$target)
print("Tabla de contingencia Género vs Enfermedad:")
print(gender_table)
print("Proporciones:")
print(prop.table(gender_table, margin = 1))

# Tipo de dolor de pecho vs enfermedad
cp_table <- table(heart_data$cp, heart_data$target)
print("Tabla de contingencia Tipo de Dolor vs Enfermedad:")
print(cp_table)

# Visualización de variables continuas clave
cont_vars <- heart_data %>%
  select(age, trestbps, chol, thalach, oldpeak, target)

cont_long <- cont_vars %>%
  pivot_longer(cols = -target, names_to = "variable", values_to = "valor")

ggplot(cont_long, aes(x = target, y = valor, fill = target)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Variables Continuas por Presencia de Enfermedad",
       x = "Target", y = "Valor") +
  scale_fill_manual(values = c("lightblue", "coral")) +
  theme(legend.position = "bottom")

ggsave("03_Visualizaciones/heart_continuous_vars.png",
       width = 12, height = 8)

# ==============================================================================
# 3. DIVISIÓN DE DATOS
# ==============================================================================

print("=== DIVIDIENDO DATOS ===")

# División estratificada 70-30
train_index <- createDataPartition(heart_data$target, p = 0.7, list = FALSE)

train_data <- heart_data[train_index, ]
test_data <- heart_data[-train_index, ]

print(paste("Observaciones de entrenamiento:", nrow(train_data)))
print(paste("Observaciones de prueba:", nrow(test_data)))

print("Distribución en entrenamiento:")
print(table(train_data$target))

print("Distribución en prueba:")
print(table(test_data$target))

# ==============================================================================
# 4. MODELO LOGIT (REGRESIÓN LOGÍSTICA)
# ==============================================================================
#
# TEORÍA: MODELO LOGIT (REGRESIÓN LOGÍSTICA)
#
# La regresión logística es un modelo de clasificación binaria que predice
# la probabilidad de que una observación pertenezca a una clase.
#
# Modelo:
# log(p/(1-p)) = β₀ + β₁X₁ + β₂X₂ + ... + βₖXₖ
#
# donde:
# - p = P(Y=1|X): probabilidad de la clase positiva
# - p/(1-p): odds (razón de momios)
# - log(p/(1-p)): log-odds o logit
# - β: coeficientes del modelo
#
# Función logística (sigmoide):
# p = 1 / (1 + e^(-z))
# donde z = β₀ + β₁X₁ + ... + βₖXₖ
#
# Esta función mapea valores de (-∞, +∞) a probabilidades en (0, 1)
#
# Interpretación de coeficientes:
# - exp(βⱼ) = odds ratio para un aumento unitario en Xⱼ
# - βⱼ > 0: aumento en Xⱼ aumenta probabilidad de Y=1
# - βⱼ < 0: aumento en Xⱼ disminuye probabilidad de Y=1
#
# Estimación:
# - Máxima verosimilitud (Maximum Likelihood Estimation)
# - No hay forma cerrada, se usa optimización iterativa
#
# Ventajas:
# - Interpretación probabilística directa
# - Coeficientes interpretables como odds ratios
# - No asume normalidad de predictores
# - Robusto a outliers en X
# - Eficiente computacionalmente
#
# Desventajas:
# - Asume relación lineal entre X y log-odds
# - Sensible a multicolinealidad
# - No captura interacciones automáticamente
# - Puede tener separación perfecta en muestras pequeñas
#
# ==============================================================================

print("=== MODELO LOGIT (REGRESIÓN LOGÍSTICA) ===")

# Entrenar modelo logit
logit_model <- glm(target ~ .,
                   data = train_data,
                   family = binomial(link = "logit"))

print("Resumen del modelo Logit:")
print(summary(logit_model))

# Odds ratios con intervalos de confianza
odds_ratios <- exp(cbind(OR = coef(logit_model),
                         confint(logit_model)))

print("Odds Ratios con IC 95%:")
print(odds_ratios)

# Predicciones
logit_prob <- predict(logit_model, test_data, type = "response")
logit_pred <- ifelse(logit_prob > 0.5, 1, 0)
logit_pred <- factor(logit_pred, levels = c(0, 1))

# Matriz de confusión
logit_cm <- confusionMatrix(logit_pred, test_data$target, positive = "1")

print("=== RESULTADOS LOGIT ===")
print(logit_cm)

# Curva ROC
logit_roc <- roc(test_data$target, logit_prob, quiet = TRUE)
print(paste("AUC Logit:", round(auc(logit_roc), 4)))

# ==============================================================================
# 5. MODELO PROBIT
# ==============================================================================
#
# TEORÍA: MODELO PROBIT
#
# El modelo Probit es similar al Logit pero usa una función de enlace diferente
# basada en la distribución normal acumulada.
#
# Modelo:
# P(Y=1|X) = Φ(β₀ + β₁X₁ + ... + βₖXₖ)
#
# donde:
# - Φ: función de distribución acumulada de la normal estándar N(0,1)
# - β: coeficientes del modelo
#
# Interpretación conceptual:
# Probit asume que existe una variable latente Y* continua:
# Y* = β₀ + β₁X₁ + ... + βₖXₖ + ε, donde ε ~ N(0,1)
# Observamos Y=1 si Y* > 0, Y=0 si Y* ≤ 0
#
# Diferencias entre Logit y Probit:
#
# 1. Función de enlace:
#    - Logit: función logística (colas más pesadas)
#    - Probit: CDF normal (colas más ligeras)
#
# 2. Interpretación:
#    - Logit: coeficientes interpretables como log-odds ratios
#    - Probit: coeficientes menos intuitivos (cambios en Φ⁻¹(p))
#
# 3. Forma funcional:
#    - Logit: p = 1/(1 + e^(-z))
#    - Probit: p = Φ(z)
#
# 4. Similitud:
#    - Para valores centrales de probabilidad (0.2-0.8), dan resultados similares
#    - Difieren en las colas (probabilidades extremas)
#
# 5. Escalamiento:
#    - βₗₒgᵢₜ ≈ 1.6 * βₚᵣₒbᵢₜ (aproximadamente)
#
# Ventajas del Probit:
# - Conexión natural con variable latente continua
# - Útil en análisis de umbral
# - Conveniente para ciertos modelos econométricos
#
# Ventajas del Logit:
# - Más fácil de interpretar (odds ratios)
# - Computacionalmente más simple
# - Más usado en práctica
#
# ¿Cuándo usar cada uno?
# - Logit: mayoría de aplicaciones prácticas, interpretación importante
# - Probit: cuando el modelo teórico sugiere variable latente normal,
#           o cuando se necesita consistencia con otros modelos (ej. Heckman)
#
# ==============================================================================

print("=== MODELO PROBIT ===")

# Entrenar modelo probit
probit_model <- glm(target ~ .,
                    data = train_data,
                    family = binomial(link = "probit"))

print("Resumen del modelo Probit:")
print(summary(probit_model))

# Predicciones
probit_prob <- predict(probit_model, test_data, type = "response")
probit_pred <- ifelse(probit_prob > 0.5, 1, 0)
probit_pred <- factor(probit_pred, levels = c(0, 1))

# Matriz de confusión
probit_cm <- confusionMatrix(probit_pred, test_data$target, positive = "1")

print("=== RESULTADOS PROBIT ===")
print(probit_cm)

# Curva ROC
probit_roc <- roc(test_data$target, probit_prob, quiet = TRUE)
print(paste("AUC Probit:", round(auc(probit_roc), 4)))

# ==============================================================================
# 6. COMPARACIÓN LOGIT VS PROBIT
# ==============================================================================

print("=== COMPARACIÓN LOGIT VS PROBIT ===")

# Comparar coeficientes escalados
# Teoría sugiere que β_logit ≈ 1.6 * β_probit
coef_comparison <- data.frame(
  Variable = names(coef(logit_model)),
  Logit = coef(logit_model),
  Probit = coef(probit_model),
  Probit_scaled = coef(probit_model) * 1.6,
  Ratio = coef(logit_model) / coef(probit_model)
)

print("Comparación de coeficientes:")
print(coef_comparison)

# Comparar métricas de rendimiento
metricas <- data.frame(
  Modelo = c("Logit", "Probit"),
  Accuracy = c(logit_cm$overall["Accuracy"],
               probit_cm$overall["Accuracy"]),
  Sensitivity = c(logit_cm$byClass["Sensitivity"],
                  probit_cm$byClass["Sensitivity"]),
  Specificity = c(logit_cm$byClass["Specificity"],
                  probit_cm$byClass["Specificity"]),
  AUC = c(auc(logit_roc), auc(probit_roc))
)

print("Métricas de rendimiento:")
print(metricas)

# ==============================================================================
# 7. VISUALIZACIÓN DE CURVAS ROC
# ==============================================================================

print("=== GENERANDO CURVAS ROC ===")

# Crear dataframe para ggplot
roc_data <- data.frame(
  Specificity_Logit = 1 - logit_roc$specificities,
  Sensitivity_Logit = logit_roc$sensitivities,
  Specificity_Probit = 1 - probit_roc$specificities,
  Sensitivity_Probit = probit_roc$sensitivities
)

# Gráfico ROC
ggplot() +
  geom_line(data = roc_data,
            aes(x = Specificity_Logit, y = Sensitivity_Logit,
                color = "Logit"),
            size = 1.2) +
  geom_line(data = roc_data,
            aes(x = Specificity_Probit, y = Sensitivity_Probit,
                color = "Probit"),
            size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray") +
  theme_minimal() +
  labs(title = "Curvas ROC - Logit vs Probit",
       x = "1 - Especificidad (Tasa de Falsos Positivos)",
       y = "Sensibilidad (Tasa de Verdaderos Positivos)",
       color = "Modelo") +
  annotate("text", x = 0.7, y = 0.3,
           label = paste("AUC Logit:", round(auc(logit_roc), 3))) +
  annotate("text", x = 0.7, y = 0.2,
           label = paste("AUC Probit:", round(auc(probit_roc), 3))) +
  coord_equal()

ggsave("03_Visualizaciones/heart_roc_curves.png",
       width = 8, height = 8)

# ==============================================================================
# 8. ANÁLISIS DE EFECTOS MARGINALES
# ==============================================================================
#
# Los efectos marginales muestran el cambio en la probabilidad predicha
# para un cambio unitario en la variable predictora, manteniendo las
# demás constantes. Son más interpretables que los coeficientes raw.
#
# ==============================================================================

print("=== EFECTOS MARGINALES ===")

# Para variables continuas, calcular efecto marginal promedio (AME)
# AME = promedio de efectos marginales individuales

# Función para calcular efectos marginales en logit
# Para logit: ∂p/∂x = β * p * (1-p)
calculate_marginal_effects_logit <- function(model, data) {
  # Obtener probabilidades predichas
  probs <- predict(model, data, type = "response")

  # Obtener coeficientes
  coefs <- coef(model)

  # Calcular efecto marginal para cada observación
  # y luego promediar
  marginal_effects <- lapply(names(coefs)[-1], function(var_name) {
    if (var_name %in% names(coefs)) {
      beta <- coefs[var_name]
      me <- beta * probs * (1 - probs)
      mean(me, na.rm = TRUE)
    } else {
      NA
    }
  })

  names(marginal_effects) <- names(coefs)[-1]
  return(marginal_effects)
}

# Calcular efectos marginales para variables numéricas
numeric_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")

print("Efectos Marginales Promedio (Logit):")
print("Interpretación: cambio en probabilidad por aumento unitario en X")

# Para cada variable numérica
lapply(numeric_vars, function(var) {
  # Crear datos con variación solo en esta variable
  new_data <- train_data
  probs <- predict(logit_model, new_data, type = "response")

  # Calcular efecto marginal promedio
  coef_val <- coef(logit_model)[var]
  if (!is.na(coef_val)) {
    ame <- mean(coef_val * probs * (1 - probs), na.rm = TRUE)
    print(paste(var, ":", round(ame, 6)))
  }
})

# ==============================================================================
# 9. ANÁLISIS DE BONDAD DE AJUSTE
# ==============================================================================

print("=== PRUEBAS DE BONDAD DE AJUSTE ===")

# Deviance
print("Logit - Null Deviance:")
print(logit_model$null.deviance)
print("Logit - Residual Deviance:")
print(logit_model$deviance)

print("Probit - Null Deviance:")
print(probit_model$null.deviance)
print("Probit - Residual Deviance:")
print(probit_model$deviance)

# Pseudo R² (McFadden)
# R² = 1 - (log-likelihood del modelo / log-likelihood del modelo nulo)
logit_pseudo_r2 <- 1 - (logit_model$deviance / logit_model$null.deviance)
probit_pseudo_r2 <- 1 - (probit_model$deviance / probit_model$null.deviance)

print(paste("Pseudo R² Logit:", round(logit_pseudo_r2, 4)))
print(paste("Pseudo R² Probit:", round(probit_pseudo_r2, 4)))

# AIC y BIC (menores son mejores)
print(paste("AIC Logit:", round(AIC(logit_model), 2)))
print(paste("AIC Probit:", round(AIC(probit_model), 2)))

print(paste("BIC Logit:", round(BIC(logit_model), 2)))
print(paste("BIC Probit:", round(BIC(probit_model), 2)))

# ==============================================================================
# 10. VISUALIZACIÓN DE PROBABILIDADES PREDICHAS
# ==============================================================================

print("=== VISUALIZANDO PROBABILIDADES PREDICHAS ===")

# Crear dataframe con probabilidades de ambos modelos
prob_comparison <- data.frame(
  Logit_prob = logit_prob,
  Probit_prob = probit_prob,
  Actual = as.numeric(as.character(test_data$target))
)

# Scatter plot de probabilidades
ggplot(prob_comparison, aes(x = Logit_prob, y = Probit_prob, color = factor(Actual))) +
  geom_point(alpha = 0.6, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Comparación de Probabilidades Predichas: Logit vs Probit",
       x = "Probabilidad Logit",
       y = "Probabilidad Probit",
       color = "Clase Real") +
  scale_color_manual(values = c("0" = "lightblue", "1" = "coral")) +
  coord_equal()

ggsave("03_Visualizaciones/heart_prob_comparison.png",
       width = 8, height = 8)

# Distribución de probabilidades por clase real
prob_long <- prob_comparison %>%
  pivot_longer(cols = c(Logit_prob, Probit_prob),
               names_to = "Modelo",
               values_to = "Probabilidad") %>%
  mutate(Modelo = str_remove(Modelo, "_prob"))

ggplot(prob_long, aes(x = Probabilidad, fill = factor(Actual))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~Modelo) +
  theme_minimal() +
  labs(title = "Distribución de Probabilidades Predichas por Modelo",
       x = "Probabilidad Predicha",
       y = "Densidad",
       fill = "Clase Real") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "coral")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")

ggsave("03_Visualizaciones/heart_prob_distributions.png",
       width = 10, height = 6)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

print("=== ANÁLISIS COMPLETADO ===")
print("Se compararon modelos Logit y Probit para clasificación binaria.")
print("Ambos modelos producen resultados muy similares en este dataset.")
print("Logit es preferido en práctica por interpretabilidad (odds ratios).")
print("Probit puede ser útil cuando hay justificación teórica de variable latente.")
print("Todas las visualizaciones se guardaron exitosamente.")
