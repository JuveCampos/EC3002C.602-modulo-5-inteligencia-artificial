# Librerías ----
library(tidyverse)
library(rebus)
library(caret)      # Para validación cruzada
library(broom)      # Para trabajar con modelos de forma tidy
library(corrplot)   # Para matriz de correlación
library(ggrepel)   # Para etiquetas en gráficos
library(glmnet)    # Modelos Ridge / Lasso vía caret::train(method = "glmnet")
library(randomForest)  # Backend para method = "rf"
library(xgboost)       # Backend para method = "xgbTree"
library(rpart)       # Árbol de decisión
library(rpart.plot)  # (opcional) para graficar el árbol



# IMPORTANTE: Asegurar que usamos select de dplyr
# Si tienes conflictos, puedes usar dplyr::select() explícitamente

# Configuración inicial
set.seed(123)  # Para reproducibilidad
theme_set(theme_minimal())  # Tema consistente para gráficos

# 1. CARGA Y LIMPIEZA DE DATOS ----
precio_casas <- readxl::read_xlsx("01_Datos/precio casas/datos_inmuebles.xlsx") %>%
  mutate(
    # Extraer estado de la ubicación
    estado = str_extract(ubicacion, pattern = ",\\s*([^,]+)$") %>%
      str_remove(pattern = ",\\s*"),
    
    # Corregir estado Loreto
    estado = if_else(estado == "Loreto", "Baja California Sur", estado)
  ) %>%
  # Filtrar valores faltantes en m2_terreno (nota: revisa si es !is.na())
  filter(is.na(m2_terreno)) %>%
  # Convertir precio a pesos mexicanos
  mutate(
    precio = if_else(moneda == "US", precio * 18.5, precio),
    
    # Crear variable de región según precio promedio estatal
    region_precio = case_when(
      estado %in% c("Distrito Federal", "Nuevo León", "Querétaro",
                    "Baja California Sur", "Quintana Roo", "Jalisco", 
                    "Yucatán") ~ "Alta",
      estado %in% c("Baja California", "Aguascalientes", "Coahuila", 
                    "Chihuahua", "Estado De México", "Morelos", "Sinaloa", 
                    "Nayarit", "Campeche") ~ "Media",
      estado %in% c("Guanajuato", "Hidalgo", "Puebla", "Michoacán",
                    "Veracruz", "Guerrero", "Oaxaca", "Chiapas", 
                    "Durango") ~ "Baja",
      TRUE ~ NA_character_
    ),
    
    # Crear tipo de inmueble simplificado
    tipo_inmueble = if_else(
      str_detect(encabezado, "Casa"), 
      "Casa", 
      "Departamento"
    ),
    
    # Crear variable de precio por m2
    precio_m2 = precio / m2_construidos,
    
    # Log del precio para normalizar distribución
    log_precio = log(precio + 1)
  ) %>%
  # Filtrar solo ventas
  filter(str_detect(encabezado, "venta")) %>%
  # Eliminar outliers extremos (opcional, ajusta según tu criterio)
  filter(
    precio > quantile(precio, 0.01, na.rm = TRUE) &
      precio < quantile(precio, 0.99, na.rm = TRUE)
  )

# Verificar datos limpios
precio_casas %>%
  count(estado, region_precio) %>%
  arrange(region_precio, estado) %>%
  print(n = 30)

# 2. ANÁLISIS EXPLORATORIO ----

# Distribución del precio
p_dist_precio <- precio_casas %>%
  ggplot(aes(x = precio/1000000)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distribución del Precio de Inmuebles",
    x = "Precio (millones de pesos)",
    y = "Frecuencia"
  ) +
  facet_wrap(~tipo_inmueble, scales = "free_y")

# Relación precio vs características
p_precio_caracteristicas <- precio_casas %>%
  pivot_longer(
    cols = c(no_recamaras, no_banos, m2_construidos),
    names_to = "caracteristica",
    values_to = "valor"
  ) %>%
  mutate(
    caracteristica = case_when(
      caracteristica == "no_recamaras" ~ "Número de Recámaras",
      caracteristica == "no_banos" ~ "Número de Baños",
      caracteristica == "m2_construidos" ~ "M² Construidos"
    )
  ) %>%
  ggplot(aes(x = valor, y = precio/1000000)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~caracteristica, scales = "free_x") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Relación entre Precio y Características del Inmueble",
    x = "Valor",
    y = "Precio (millones de pesos)"
  )

# Precio por región y tipo
p_precio_region <- precio_casas %>%
  ggplot(aes(x = region_precio, y = precio/1000000, fill = tipo_inmueble)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Distribución de Precios por Región y Tipo de Inmueble",
    x = "Región (según precio promedio)",
    y = "Precio (millones de pesos, escala log)",
    fill = "Tipo"
  ) +
  theme(legend.position = "bottom")

# 3. PREPARACIÓN PARA MODELADO ----

# SOLUCIÓN AL ERROR: Usar dplyr::select() explícitamente
datos_modelo <- precio_casas %>%
  dplyr::select(
    precio, log_precio, no_recamaras, no_banos, 
    m2_construidos, region_precio, tipo_inmueble
  ) %>%
  drop_na()  # Eliminar filas con valores faltantes

# Verificar que tenemos los datos correctos
print("Dimensiones de datos_modelo:")
print(dim(datos_modelo))
print("Primeras filas:")
print(head(datos_modelo))

# Matriz de correlación para variables numéricas
correlacion <- datos_modelo %>%
  dplyr::select(where(is.numeric)) %>%
  cor()

# Visualizar correlación
corrplot(
  correlacion, 
  method = "color", 
  type = "upper", 
  order = "hclust",
  tl.cex = 0.8,
  tl.col = "black"
)

# 4. MODELO CON VALIDACIÓN CRUZADA ----

# Configurar validación cruzada k-fold
control_cv <- trainControl(
  method = "cv",
  number = 10,  # 10-fold cross validation
  savePredictions = "final",
  verboseIter = FALSE
)

# Modelo 1: Precio original
modelo_cv <- train(
  precio ~ no_recamaras + no_banos + m2_construidos + 
    region_precio + tipo_inmueble,
  data = datos_modelo,
  method = "lm",
  trControl = control_cv
)

# Modelo 2: Log del precio (suele funcionar mejor)
modelo_log_cv <- train(
  log_precio ~ no_recamaras + no_banos + m2_construidos + 
    region_precio + tipo_inmueble,
  data = datos_modelo,
  method = "lm",
  trControl = control_cv
)

# Comparar modelos
print("=== Modelo con precio original ===")
print(modelo_cv)

print("=== Modelo con log(precio) ===")
print(modelo_log_cv)

# 5. DIAGNÓSTICO DEL MEJOR MODELO ----

# Usar el modelo con log_precio (generalmente mejor)
modelo_final <- lm(
  log_precio ~ no_recamaras + no_banos + m2_construidos + 
    region_precio + tipo_inmueble,
  data = datos_modelo
)

# Resumen completo
summary(modelo_final)

# Diagnósticos visuales
par(mfrow = c(2, 2))
plot(modelo_final)
par(mfrow = c(1, 1))

# Crear dataframe con predicciones y residuos
diagnosticos <- augment(modelo_final) %>%
  mutate(
    precio_real = exp(log_precio) - 1,
    precio_pred = exp(.fitted) - 1,
    residuo_precio = precio_real - precio_pred
  )

# 6. VISUALIZACIONES DEL MODELO ----

# Predicciones vs valores reales
p_pred_real <- diagnosticos %>%
  ggplot(aes(x = precio_real/1000000, y = precio_pred/1000000)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10), labels = scales::comma) +
  scale_y_continuous(limits = c(0, 10), labels = scales::comma) +
  labs(
    title = "Predicciones vs Valores Reales",
    subtitle = paste("R² =", round(summary(modelo_final)$r.squared, 3)),
    x = "Precio Real (millones)",
    y = "Precio Predicho (millones)"
  )

# Residuos por región
p_residuos_region <- diagnosticos %>%
  ggplot(aes(x = region_precio, y = .std.resid, fill = region_precio)) +
  geom_violin(alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_hline(yintercept = c(-2, 2), color = "orange", linetype = "dotted") +
  labs(
    title = "Distribución de Residuos Estandarizados por Región",
    x = "Región",
    y = "Residuo Estandarizado"
  ) +
  theme(legend.position = "none")

# Importancia de variables
coef_tidy <- tidy(modelo_final) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "no_recamaras" ~ "Núm. Recámaras",
      term == "no_banos" ~ "Núm. Baños",
      term == "m2_construidos" ~ "M² Construidos",
      term == "region_precioMedia" ~ "Región: Media",
      term == "region_precioBaja" ~ "Región: Baja",
      term == "tipo_inmuebleDepartamento" ~ "Tipo: Departamento",
      TRUE ~ term
    )
  )

p_coeficientes <- coef_tidy %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(
    aes(ymin = estimate - 1.96*std.error, 
        ymax = estimate + 1.96*std.error),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Coeficientes del Modelo (IC 95%)",
    subtitle = "Variable dependiente: log(precio)",
    x = "Variable",
    y = "Coeficiente"
  )

# 7. MÉTRICAS DE DESEMPEÑO ----

# Calcular métricas
metricas <- diagnosticos %>%
  summarise(
    RMSE = sqrt(mean(residuo_precio^2, na.rm = TRUE)),
    MAE = mean(abs(residuo_precio), na.rm = TRUE),
    MAPE = mean(abs(residuo_precio/precio_real), na.rm = TRUE) * 100,
    R2 = summary(modelo_final)$r.squared,
    R2_ajustado = summary(modelo_final)$adj.r.squared
  )

print("=== Métricas de Desempeño del Modelo ===")
print(metricas)

# 8. MOSTRAR TODAS LAS VISUALIZACIONES ----
print(p_dist_precio)
print(p_precio_caracteristicas)
print(p_precio_region)
print(p_pred_real)
print(p_residuos_region)
print(p_coeficientes)

# 9. PREDICCIONES DE EJEMPLO ----

# Crear ejemplos para predicción
nuevos_datos <- tibble(
  no_recamaras = c(2, 3, 4),
  no_banos = c(1, 2, 3),
  m2_construidos = c(80, 120, 200),
  region_precio = c("Baja", "Media", "Alta"),
  tipo_inmueble = c("Departamento", "Casa", "Casa")
)

# Hacer predicciones con intervalos de confianza
predicciones <- predict(
  modelo_final, 
  newdata = nuevos_datos,
  interval = "confidence"
) %>%
  as_tibble() %>%
  mutate(across(everything(), ~(exp(.) - 1))) %>%  # Convertir de log a precio
  bind_cols(nuevos_datos, .) %>%
  mutate(across(c(fit, lwr, upr), ~./1000000))  # Convertir a millones

print("=== Predicciones de Ejemplo (millones de pesos) ===")
print(predicciones)

# 10. SOLUCIÓN ADICIONAL SI PERSISTEN PROBLEMAS ----
# Si sigues teniendo conflictos, puedes reiniciar el ambiente de dplyr:
# detach("package:MASS", unload = TRUE)  # Si MASS está causando el problema
# library(tidyverse)  # Recargar tidyverse

# O usar siempre el namespace explícito:
# dplyr::select(), dplyr::filter(), dplyr::mutate(), etc.

# 4B. MODELOS RIDGE Y LASSO (glmnet + caret) ----

# Grid de hiperparámetros para glmnet
# alpha = 0  -> Ridge
# alpha = 1  -> Lasso
grid_glmnet <- expand.grid(
  alpha  = c(0, 1),
  lambda = 10 ^ seq(-3, 2, length.out = 50)
)

# Modelo Ridge: alpha = 0
modelo_ridge_cv <- train(
  log_precio ~ no_recamaras + no_banos + m2_construidos +
    region_precio + tipo_inmueble,
  data      = datos_modelo,
  method    = "glmnet",
  trControl = control_cv,
  preProcess = c("center", "scale"),  # estandarizar predictores
  tuneGrid  = grid_glmnet %>% 
    filter(alpha == 0)
)

# Modelo Lasso: alpha = 1
modelo_lasso_cv <- train(
  log_precio ~ no_recamaras + no_banos + m2_construidos +
    region_precio + tipo_inmueble,
  data      = datos_modelo,
  method    = "glmnet",
  trControl = control_cv,
  preProcess = c("center", "scale"),
  tuneGrid  = grid_glmnet %>% 
    filter(alpha == 1)
)

# Resumen de los mejores hiperparámetros encontrados
# (usa comentarios en lugar de mensajes impresos cuando sea posible)
# Mejor lambda para Ridge y Lasso según RMSE
mejores_ridge <- modelo_ridge_cv$bestTune
mejores_lasso <- modelo_lasso_cv$bestTune

print("=== Mejor modelo Ridge (alpha = 0) ===")
print(mejores_ridge)

print("=== Mejor modelo Lasso (alpha = 1) ===")
print(mejores_lasso)

# Comparar desempeño (RMSE y R2) entre:
# - Regresión lineal con log(precio)
# - Ridge
# - Lasso
resumen_modelos <- tibble(
  modelo  = c("lm_log", "ridge", "lasso"),
  RMSE    = c(
    modelo_log_cv$results$RMSE[1],
    modelo_ridge_cv$results %>% 
      filter(lambda == mejores_ridge$lambda) %>% 
      pull(RMSE),
    modelo_lasso_cv$results %>% 
      filter(lambda == mejores_lasso$lambda) %>% 
      pull(RMSE)
  ),
  R2 = c(
    modelo_log_cv$results$Rsquared[1],
    modelo_ridge_cv$results %>% 
      filter(lambda == mejores_ridge$lambda) %>% 
      pull(Rsquared),
    modelo_lasso_cv$results %>% 
      filter(lambda == mejores_lasso$lambda) %>% 
      pull(Rsquared)
  )
)

print("=== Comparación de desempeño (log_precio) ===")
print(resumen_modelos)







# 4C. COMPARACIÓN DE COEFICIENTES (LM vs Ridge vs Lasso) ----

# Extraer coeficientes del modelo lineal
coef_lm <- tidy(modelo_final) %>%
  mutate(modelo = "OLS") %>%
  dplyr::select(modelo, term, estimate)

# Extraer coeficientes Ridge y Lasso (glmnet usa lambda óptimo)
coef_ridge <- coef(modelo_ridge_cv$finalModel, 
                   s = modelo_ridge_cv$bestTune$lambda) %>%
  as.matrix() %>%
  as_tibble(rownames = "term") %>%
  rename(estimate = s0) %>%
  mutate(modelo = "Ridge")

coef_lasso <- coef(modelo_lasso_cv$finalModel, 
                   s = modelo_lasso_cv$bestTune$lambda) %>%
  as.matrix() %>%
  as_tibble(rownames = "term") %>%
  rename(estimate = s0) %>%
  mutate(modelo = "Lasso")

# Unificar en un solo dataframe
coef_comparacion <- bind_rows(coef_lm, coef_ridge, coef_lasso) %>%
  filter(term != "(Intercept)") 
# %>%
#   mutate(term = fct_reorder(term, estimate, .fun = abs, .desc = TRUE))

# Tabla de comparación
coef_comparacion %>%
  pivot_wider(names_from = modelo, values_from = estimate) %>%
  arrange(desc(abs(OLS))) %>%
  print(n = 20)

# Visualización comparativa
p_coef_comparacion <- coef_comparacion %>%
  ggplot(aes(x = term, y = estimate, color = modelo)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Comparación de Coeficientes: OLS vs Ridge vs Lasso",
    subtitle = "Variable dependiente: log(precio)",
    x = "Variable explicativa",
    y = "Coeficiente estimado",
    color = "Modelo"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_coef_comparacion)





# 4D. OBSERVADO VS PREDICHO POR MODELO (LM, Ridge, Lasso) ----

# Predicciones en escala log(precio) para cada modelo
pred_ols_log <- predict(
  modelo_final,
  newdata = datos_modelo
)

pred_ridge_log <- predict(
  modelo_ridge_cv,
  newdata = datos_modelo
)

pred_lasso_log <- predict(
  modelo_lasso_cv,
  newdata = datos_modelo
)

# Construir tibble con observados y predichos en millones de pesos
predicciones_modelos <- datos_modelo %>%
  mutate(
    # Precio observado en pesos (ya lo tienes en datos_modelo)
    precio_real = precio,
    
    # Volver de log(precio + 1) a precio
    precio_pred_ols   = exp(pred_ols_log)   - 1,
    precio_pred_ridge = exp(pred_ridge_log) - 1,
    precio_pred_lasso = exp(pred_lasso_log) - 1
  ) %>%
  transmute(
    precio_real_millones = precio_real / 1e6,
    OLS   = precio_pred_ols   / 1e6,
    Ridge = precio_pred_ridge / 1e6,
    Lasso = precio_pred_lasso / 1e6
  ) %>%
  pivot_longer(
    cols = c(OLS, Ridge, Lasso),
    names_to  = "modelo",
    values_to = "precio_pred_millones"
  )

# Gráfica: Observado (X) vs Predicho (Y) con facetas por modelo
p_obs_pred_facetas <- predicciones_modelos %>%
  ggplot(
    aes(
      x = precio_real_millones,
      y = precio_pred_millones
    )
  ) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "red"
  ) +
  facet_wrap(~modelo) +
  coord_equal() +
  labs(
    title = "Valores observados vs predichos por modelo",
    subtitle = "Comparación entre OLS, Ridge y Lasso\nVariable dependiente: log(precio)",
    x = "Precio observado (millones de pesos)",
    y = "Precio predicho (millones de pesos)"
  ) +
  theme_minimal()

print(p_obs_pred_facetas)





# 4E. MODELO RANDOM FOREST (regresión) ----

# Random Forest con caret (objetivo: log_precio)
modelo_rf_cv <- train(
  log_precio ~ no_recamaras + no_banos + m2_construidos +
    region_precio + tipo_inmueble,
  data      = datos_modelo,
  method    = "rf",
  trControl = control_cv,
  tuneLength = 5,          # número de valores de mtry a explorar
  importance = TRUE
)

# Resumen del modelo Random Forest
print("=== Modelo Random Forest (log_precio) ===")
print(modelo_rf_cv)

# Importancia de variables según RF
importancia_rf <- varImp(modelo_rf_cv, scale = TRUE) %>%
  .$importance %>%
  as_tibble(rownames = "variable") %>%
  arrange(desc(Overall))

# Visualización de importancia de variables en RF
p_importancia_rf <- importancia_rf %>%
  ggplot(aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de variables según Random Forest",
    subtitle = "Variable dependiente: log(precio)",
    x = "Variable",
    y = "Importancia (Overall)"
  )

print(p_importancia_rf)





# 4F. MODELO XGBOOST (regresión) ----

# XGBoost con caret: method = "xgbTree"
# caret se encarga de crear dummies para las variables categóricas
modelo_xgb_cv <- train(
  log_precio ~ no_recamaras + no_banos + m2_construidos +
    region_precio + tipo_inmueble,
  data       = datos_modelo,
  method     = "xgbTree",
  trControl  = control_cv,
  tuneLength = 10,     # número de combinaciones de hiperparámetros a explorar
  verbose    = FALSE
)

print("=== Modelo XGBoost (log_precio) ===")
print(modelo_xgb_cv)

# Importancia de variables según XGBoost
importancia_xgb <- varImp(modelo_xgb_cv, scale = TRUE) %>%
  .$importance %>%
  as_tibble(rownames = "variable") %>%
  arrange(desc(Overall))

p_importancia_xgb <- importancia_xgb %>%
  ggplot(aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de variables según XGBoost",
    subtitle = "Variable dependiente: log(precio)",
    x = "Variable",
    y = "Importancia (Overall)"
  )

print(p_importancia_xgb)


# 4H. OBSERVADO VS PREDICHO: OLS, Ridge, Lasso, RF, XGB ----

pred_rf_log <- predict(
  modelo_rf_cv,
  newdata = datos_modelo
)

pred_xgb_log <- predict(
  modelo_xgb_cv,
  newdata = datos_modelo
)

predicciones_modelos_ext <- datos_modelo %>%
  mutate(
    precio_real = precio,
    precio_pred_ols   = exp(predict(modelo_final,     newdata = datos_modelo)) - 1,
    precio_pred_ridge = exp(predict(modelo_ridge_cv,  newdata = datos_modelo)) - 1,
    precio_pred_lasso = exp(predict(modelo_lasso_cv,  newdata = datos_modelo)) - 1,
    precio_pred_rf    = exp(pred_rf_log)  - 1,
    precio_pred_xgb   = exp(pred_xgb_log) - 1
  ) %>%
  transmute(
    precio_real_millones = precio_real / 1e6,
    OLS    = precio_pred_ols   / 1e6,
    Ridge  = precio_pred_ridge / 1e6,
    Lasso  = precio_pred_lasso / 1e6,
    `Random Forest` = precio_pred_rf  / 1e6,
    XGBoost = precio_pred_xgb / 1e6
  ) %>%
  pivot_longer(
    cols = c(OLS, Ridge, Lasso, `Random Forest`, XGBoost),
    names_to  = "modelo",
    values_to = "precio_pred_millones"
  )

p_obs_pred_facetas_ext <- predicciones_modelos_ext %>%
  filter(precio_pred_millones <= 100) %>% 
  ggplot(
    aes(
      x = precio_real_millones,
      y = precio_pred_millones
    )
  ) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "red"
  ) +
  facet_wrap(~modelo) +
  coord_equal() +
  labs(
    title = "Valores observados vs predichos por modelo",
    subtitle = "OLS, Ridge, Lasso, Random Forest y XGBoost\nVariable dependiente: log(precio)",
    x = "Precio observado (millones de pesos)",
    y = "Precio predicho (millones de pesos)"
  ) +
  theme_minimal()

print(p_obs_pred_facetas_ext)





# 4I. MODELO DE ÁRBOL DE DECISIÓN (RPART) ----

# Árbol de regresión para log_precio
modelo_arbol_cv <- train(
  log_precio ~ no_recamaras + no_banos + m2_construidos +
    region_precio + tipo_inmueble,
  data       = datos_modelo,
  method     = "rpart",
  trControl  = control_cv,
  tuneLength = 10   # explora distintos cp (complejidades del árbol)
)

print("=== Modelo Árbol de Decisión (log_precio) ===")
print(modelo_arbol_cv)

# Importancia de variables según el árbol
importancia_arbol <- varImp(modelo_arbol_cv, scale = TRUE) %>%
  .$importance %>%
  as_tibble(rownames = "variable") %>%
  arrange(desc(Overall))

p_importancia_arbol <- importancia_arbol %>%
  ggplot(aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de variables según Árbol de Decisión",
    subtitle = "Variable dependiente: log(precio)",
    x = "Variable",
    y = "Importancia (Overall)"
  )

print(p_importancia_arbol)

# (Opcional) Graficar el árbol final
arbol_final <- modelo_arbol_cv$finalModel

rpart.plot::rpart.plot(
  arbol_final,
  main = "Árbol de decisión para log(precio)"
)




