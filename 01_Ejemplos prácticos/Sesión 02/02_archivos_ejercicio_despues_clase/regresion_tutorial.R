# ================================================================================
# TUTORIAL DE REGRESIÓN EN R
# ================================================================================

# Cargar librerías necesarias
library(ggplot2)
library(corrplot)
library(car)

# ================================================================================
# 1. CREACIÓN DEL DATASET SINTÉTICO
# ================================================================================

# Establecer semilla para reproducibilidad
set.seed(123)

# Crear variables independientes
n <- 200  # Número de observaciones

# Variable independiente 1: Años de experiencia (0-20 años)
experiencia <- runif(n, 0, 20)

# Variable independiente 2: Nivel educativo (1-5, siendo 5 el más alto)
educacion <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1))

# Variable independiente 3: Horas trabajadas por semana (30-60 horas)
horas_trabajo <- rnorm(n, mean = 45, sd = 8)
horas_trabajo <- pmax(30, pmin(60, horas_trabajo))  # Limitar entre 30 y 60

# Crear variable dependiente: Salario anual
# El salario depende de experiencia, educación y horas de trabajo
salario_base <- 30000
salario <- salario_base + 
           2000 * experiencia +           # Cada año de experiencia suma $2000
           5000 * educacion +             # Cada nivel educativo suma $5000
           200 * horas_trabajo +          # Cada hora extra suma $200
           rnorm(n, 0, 5000)              # Ruido aleatorio

# Asegurar que no haya salarios negativos
salario <- pmax(25000, salario)

# Crear el dataframe
datos <- data.frame(
  salario = salario,
  experiencia = experiencia,
  educacion = educacion,
  horas_trabajo = horas_trabajo
)

# Mostrar estructura del dataset
str(datos)
head(datos)
summary(datos)

# ================================================================================
# 2. ANÁLISIS EXPLORATORIO DE DATOS
# ================================================================================

# Matriz de correlación
correlaciones <- cor(datos)
print("Matriz de correlaciones:")
print(round(correlaciones, 3))

# Gráfico de matriz de correlación
corrplot(correlaciones, method = "circle", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45)

# Histogramas de cada variable
par(mfrow = c(2, 2))
hist(datos$salario, main = "Distribución del Salario", xlab = "Salario", col = "lightblue")
hist(datos$experiencia, main = "Distribución de la Experiencia", xlab = "Años", col = "lightgreen")
hist(datos$educacion, main = "Distribución del Nivel Educativo", xlab = "Nivel", col = "lightcoral")
hist(datos$horas_trabajo, main = "Distribución de Horas de Trabajo", xlab = "Horas", col = "lightyellow")
par(mfrow = c(1, 1))

# ================================================================================
# 3. REGRESIÓN LINEAL SIMPLE
# ================================================================================

# Regresión simple: Salario vs Experiencia
modelo_simple <- lm(salario ~ experiencia, data = datos)

# Resumen del modelo
summary(modelo_simple)

# Gráfico de dispersión con línea de regresión
ggplot(datos, aes(x = experiencia, y = salario)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Regresión Lineal Simple: Salario vs Experiencia",
       x = "Años de Experiencia",
       y = "Salario Anual ($)") +
  theme_minimal()

# Coeficientes del modelo
coef(modelo_simple)

# Intervalos de confianza para los coeficientes
confint(modelo_simple)

# ================================================================================
# 4. REGRESIÓN LINEAL MÚLTIPLE
# ================================================================================

# Regresión múltiple: Salario vs todas las variables
modelo_multiple <- lm(salario ~ experiencia + educacion + horas_trabajo, data = datos)

# Resumen del modelo múltiple
summary(modelo_multiple)

# Comparar R-cuadrado entre modelo simple y múltiple
cat("R-cuadrado modelo simple:", summary(modelo_simple)$r.squared, "\n")
cat("R-cuadrado modelo múltiple:", summary(modelo_multiple)$r.squared, "\n")
cat("R-cuadrado ajustado modelo múltiple:", summary(modelo_multiple)$adj.r.squared, "\n")

# ================================================================================
# 5. DIAGNÓSTICOS DEL MODELO
# ================================================================================

# Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo_multiple)
par(mfrow = c(1, 1))

# Test de normalidad de residuos (Shapiro-Wilk)
residuos <- residuals(modelo_multiple)
shapiro.test(residuos)

# Test de homocedasticidad (Breusch-Pagan)
if(require(lmtest)) {
  bptest(modelo_multiple)
} else {
  cat("Para realizar test de Breusch-Pagan, instala el paquete lmtest\n")
}

# Test de multicolinealidad (VIF - Variance Inflation Factor)
vif(modelo_multiple)

# ================================================================================
# 6. PREDICCIONES
# ================================================================================

# Crear nuevos datos para predicción
nuevos_datos <- data.frame(
  experiencia = c(5, 10, 15),
  educacion = c(3, 4, 5),
  horas_trabajo = c(40, 45, 50)
)

# Predicciones puntuales
predicciones <- predict(modelo_multiple, nuevos_datos)

# Predicciones con intervalos de confianza
predicciones_ic <- predict(modelo_multiple, nuevos_datos, interval = "confidence")

# Predicciones con intervalos de predicción
predicciones_ip <- predict(modelo_multiple, nuevos_datos, interval = "prediction")

# Mostrar resultados
print("Nuevos datos para predicción:")
print(nuevos_datos)
print("Predicciones:")
print(predicciones)
print("Intervalos de confianza:")
print(predicciones_ic)
print("Intervalos de predicción:")
print(predicciones_ip)

# ================================================================================
# 7. VISUALIZACIÓN DE RESULTADOS
# ================================================================================

# Gráfico de valores observados vs predichos
datos$predichos <- fitted(modelo_multiple)

ggplot(datos, aes(x = predichos, y = salario)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Valores Observados vs Predichos",
       x = "Valores Predichos",
       y = "Valores Observados") +
  theme_minimal()

# Gráfico de residuos vs valores ajustados
ggplot(datos, aes(x = predichos, y = residuals(modelo_multiple))) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()

# Q-Q plot de residuos
ggplot(datos, aes(sample = residuals(modelo_multiple))) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot de Residuos",
       x = "Cuantiles Teóricos",
       y = "Cuantiles de los Residuos") +
  theme_minimal()

# ================================================================================
# 8. COMPARACIÓN DE MODELOS
# ================================================================================

# ANOVA para comparar modelos anidados
anova(modelo_simple, modelo_multiple)

# AIC y BIC para comparar modelos
AIC(modelo_simple, modelo_multiple)
BIC(modelo_simple, modelo_multiple)

# ================================================================================
# 9. RESUMEN FINAL
# ================================================================================

cat("\n=== RESUMEN DEL ANÁLISIS ===\n")
cat("Dataset creado con", nrow(datos), "observaciones\n")
cat("Variables independientes: experiencia, educación, horas de trabajo\n")
cat("Variable dependiente: salario\n\n")

cat("MODELO SIMPLE (solo experiencia):\n")
cat("R-cuadrado:", round(summary(modelo_simple)$r.squared, 4), "\n")
cat("Error estándar residual:", round(summary(modelo_simple)$sigma, 2), "\n\n")

cat("MODELO MÚLTIPLE (todas las variables):\n")
cat("R-cuadrado:", round(summary(modelo_multiple)$r.squared, 4), "\n")
cat("R-cuadrado ajustado:", round(summary(modelo_multiple)$adj.r.squared, 4), "\n")
cat("Error estándar residual:", round(summary(modelo_multiple)$sigma, 2), "\n")

cat("\nCoeficientes del modelo múltiple:\n")
print(round(coef(modelo_multiple), 2))

