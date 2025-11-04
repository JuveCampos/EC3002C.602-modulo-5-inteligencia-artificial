
# Librerias ----
library(tidyverse)
library(srvyr)
library(sf)


# Datos 
enoe <- readRDS("enoe_proc_2025_2.rds") %>% 
  as_tibble() %>% 
  mutate(
    region = case_when(
      # Norte
      ent %in% c(2, 3, 5, 8, 10, 19, 25, 26, 28) ~ "Norte",
      # Centro
      ent %in% c(1, 6, 9, 11, 13, 14, 15, 16, 17, 18, 22, 24, 29, 32) ~ "Centro",
      # Sur
      ent %in% c(4, 7, 12, 20, 21, 23, 27, 30, 31) ~ "Sur",
      TRUE ~ NA_character_
    )
  )


unique(enoe$ent)

sum(enoe$factor) # 130,575,786

# Poblacion economicamente activa ocupada ----
peao <- enoe %>% 
  filter(clase1 == 1) %>% 
  filter(clase2 == 1) %>% 
  filter(eda >= 15)

# obtenemos el ingreso promedio 
bd <- peao %>% 
  as_survey_design(weights = factor) %>% 
  group_by(region) %>% 
  summarise(salario_prom = survey_mean(ingreso_sm, vartype = "ci", na.rm = T)) %>% 
  arrange(-salario_prom)

# hacemos una gráfica del salario promedio ----
bd %>% 
  ggplot(aes(x = factor(region), y = salario_prom)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = salario_prom_low, ymax = salario_prom_upp))

# Regresión lineal ----
modelo <- lm(formula = ingreso_sm ~ sexo + anios_esc + 
     grupo_edad +
     tipo_formalidad + sector + region, data = peao)
modelo

# Ridge ----

# ============================
# Regresión Ridge ----
# ============================

library(glmnet)

# 1. Preparamos la base para el modelo Ridge ------------------------------
# Nos quedamos solo con las variables del modelo y quitamos NA
mod_data <- peao %>% 
  select(ingreso_sm, sexo, anios_esc, 
         grupo_edad, tipo_formalidad, sector, region,
         factor) %>%      # el peso de la ENOE
  drop_na()

# 2. Creamos la matriz de diseño X (dummies para categóricas) -------------
# model.matrix crea variables dummy para factores automáticamente
X <- model.matrix(
  ingreso_sm ~ sexo + anios_esc + grupo_edad +
    tipo_formalidad + sector + region,
  data = mod_data
)[, -1]   # quitamos la columna de intercepto

# Vector de respuesta
y <- mod_data$ingreso_sm

# Vector de pesos (diseño muestral ENOE)
w <- mod_data$factor

# 3. Ajustamos Ridge con validación cruzada -------------------------------
set.seed(123)

?cv.glmnet
ridge_cv <- cv.glmnet(
  x = X,
  y = y,
  alpha = 0,        # 0 = Ridge (penalización L2)
  weights = w,      # usamos los factores de expansión como pesos
  standardize = TRUE # estandariza predictores antes de penalizar
)

# Lambda óptimo (el que minimiza el error de CV)
ridge_lambda_opt <- ridge_cv$lambda.min
ridge_lambda_opt

# 4. Ajuste final del modelo Ridge con ese lambda -------------------------
ridge_fit <- glmnet(
  x = X,
  y = y,
  alpha = 0,
  lambda = ridge_lambda_opt,
  weights = w,
  standardize = TRUE
)

# 5. Coeficientes estimados por Ridge -------------------------------------
ridge_coef <- coef(ridge_fit)
# ridge_coef %>% as.matrix() %>% as_tibble()
class(ridge_coef)

valores <- coef(modelo) 
attributes(valores) <- NULL
nombres <- names(coef(modelo))
coeficientes_regresion <- cbind.data.frame(nombres, valores)

nombres_ridge <- names(ridge_coef[,1])
valores_ridge <- ridge_coef[,1]
attributes(valores_ridge) <- NULL

coeficientes_ridge <- cbind.data.frame(nombres = nombres_ridge, valores_ridge)

left_join(coeficientes_ridge, coeficientes_regresion)

# ARBOL ----


# Preparar datos para el modelo (remover NAs)
enoe_arbol <- peao %>%
  select(ingreso_sm, 
         sexo, anios_esc, 
           grupo_edad , 
           tipo_formalidad,  sector, 
         region,
         horas_ocupadas,
         # all_of(variables_modelo), 
         factor) %>%
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

# Clasificación y clústering ----
manzanas <- read_csv("01_datos/entidades/RESAGEBURB_09CSV20.csv")
datos_ageb <- manzanas %>% filter(MZA == "000")
agebs <- read_sf("01_datos/agebs/09a.shp")
plot(agebs, max.plot = 1)

# Clustering de agebs ----

# Clasificación y clústering ----
library(tidyverse)
library(sf)

manzanas <- read_csv("01_datos/entidades/RESAGEBURB_09CSV20.csv")
datos_ageb <- manzanas %>% filter(MZA == "000")
agebs <- read_sf("01_datos/agebs/09a.shp")
plot(agebs, max.plot = 1)

# Clustering de agebs ----
names(datos_ageb)

# 1) Construimos tabla con proporciones pp_ -------------------------------
datos_pp <- datos_ageb %>% 
  transmute(
    ENTIDAD, NOM_ENT, AGEB,
    POBTOT, VIVTOT, VPH_PC, VPH_TV, VPH_INTER, VPH_CVJ, VPH_AUTOM
  ) %>% 
  mutate(across(POBTOT:VPH_AUTOM, as.numeric)) %>% 
  mutate(
    pp_pc        = 100 * (VPH_PC     / VIVTOT),
    pp_tv        = 100 * (VPH_TV     / VIVTOT),
    pp_internet  = 100 * (VPH_INTER  / VIVTOT),
    pp_videojuegos = 100 * (VPH_CVJ  / VIVTOT), 
    pp_auto =  100 * (VPH_AUTOM  / VIVTOT), 
  )

# 2) Preparamos datos para k-means (solo variables pp_) -------------------
datos_para_k <- datos_pp %>% 
  select(ENTIDAD, NOM_ENT, AGEB, starts_with("pp_")) %>% 
  drop_na(starts_with("pp_"))   # por si hay NA

# Matriz numérica de las variables pp_
X_pp <- datos_para_k %>% 
  select(starts_with("pp_")) %>% 
  as.matrix()

# Opcional pero recomendable: escalar variables (media 0, sd 1)
X_pp_scaled <- scale(X_pp)

# 3) k-means con k = 5 ----------------------------------------------------
set.seed(123)  # para reproducibilidad

km_pp <- kmeans(
  x = X_pp_scaled,
  centers = 5,
  nstart = 25
)

# 4) Pegamos el cluster al data frame -------------------------------------
datos_cluster <- datos_para_k %>% 
  mutate(
    cluster_k5 = factor(km_pp$cluster)  # como factor para graficar más fácil
  )

datos_cluster %>% 
  group_by(cluster_k5) %>% 
  summarise(pp_pc = mean(pp_pc, na.rm = T), 
            pp_tv = mean(pp_tv, na.rm = T), 
            pp_internet = mean(pp_internet, na.rm = T), 
            pp_videojuegos = mean(pp_videojuegos, na.rm = T), 
            pp_auto = mean(pp_auto, na.rm = T)) %>% 
  arrange(-pp_auto) %>% 
  mutate(nuevo_cluster = 1:5)

datos_cluster <- datos_cluster %>% 
  mutate(cluster_nuevo = case_when(cluster_k5 == 5 ~ 1, 
                                   cluster_k5 == 1 ~ 2, 
                                   cluster_k5 == 4 ~ 3, 
                                   cluster_k5 == 2 ~ 4, 
                                   cluster_k5 == 3 ~ 5))


# Puedes ver cómo quedaron los clusters
datos_cluster %>% count(cluster_k5) 

# Si quieres unirlo al objeto espacial `agebs` por AGEB:
agebs_cluster <- agebs %>% 
  left_join(datos_cluster %>% select(AGEB, cluster_k5, cluster_nuevo), by = c("CVE_AGEB" = "AGEB"))

agebs_cluster %>% 
  ggplot(aes(fill = factor(cluster_nuevo))) + 
  geom_sf(color = "white")


