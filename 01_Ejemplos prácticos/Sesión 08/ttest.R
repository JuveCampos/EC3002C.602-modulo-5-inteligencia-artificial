
# Prueba t
# Librerías ----
library(tidyverse) # Manejo de datos
library(srvyr)     # Herramientas para manejo de encuestas
library(foreign)   # Cargar datos de formatos externos
library(scales)
library(forcats)
library(sf)
library(survey)

# En este caso, vamos a usar el tabulado de población de la ENIGH: 
pob <- read.dbf("01_datos/poblacion.dbf") %>% 
  as_tibble()

# 1. Cargamos los datos del concentrado hogar, que contiene el resumen de la ENIGH: 
conc_hogar <- read.dbf("01_datos/concentradohogar.dbf") %>% 
  as_tibble()

# Para esto, vamos a ligar edades (población) y viviendas. 
viviendas <- read.dbf("01_datos/viviendas.dbf") %>% 
  as_tibble()

# Esto lo sacamos del tabulado de vivienda: 
viviendas_con_focos_ahorradores <- viviendas %>% 
  select(folioviv, focos_ahor, factor) %>% 
  mutate(tienen_focos_ahorradores = ifelse(focos_ahor > 0 & !is.na(focos_ahor), 
                                           yes = "Tienen focos ahorradores", 
                                           no = "No tienen focos ahorradores")) 

# ¿Cuantas viviendas tienen focos ahorradores? 
viviendas_con_focos_ahorradores %>% 
  as_survey_design(weights = factor) %>% 
  group_by(tienen_focos_ahorradores) %>% 
  survey_count() %>% 
  ungroup() %>% 
  mutate(pp = 100*(n/sum(n)))

# Verifiquemos su ingreso corriente del hogar: 
ingresos_tienen_no_focos_ahorradores <- conc_hogar %>% 
  select(folioviv, foliohog, factor, ing_cor) %>% 
  left_join(viviendas_con_focos_ahorradores) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(tienen_focos_ahorradores) %>% 
  summarise(ingreso_corriente_hogares = survey_mean(ing_cor, vartype = "ci")) 

ingresos_tienen_no_focos_ahorradores %>% 
  ggplot(aes(x = tienen_focos_ahorradores, 
             fill = tienen_focos_ahorradores, 
             y = ingreso_corriente_hogares)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = ingreso_corriente_hogares_low, ymax = ingreso_corriente_hogares_upp)) + 
  labs(title = "Montos promedio por hogares que tienen o no focos ahorradores")

# Prueba t 
conc_hogar %>% 
  select(folioviv, foliohog, factor, ing_cor) %>% 
  left_join(viviendas_con_focos_ahorradores) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(tienen_focos_ahorradores) %>% 
  summarise(ingreso_corriente_hogares = survey_mean(ing_cor, vartype = "ci")) 



library(dplyr)
library(srvyr)
library(survey)

# 1. Crear el diseño de encuesta con srvyr
# conc_hogar$factor
diseño_srvyr <- conc_hogar %>%
  select(folioviv, foliohog, factor_hogar = factor, ing_cor) %>%
  left_join(viviendas_con_focos_ahorradores, by = c("folioviv")) %>%
  as_survey_design(weights = factor)

# 2. (opcional) Estimar la media por grupo, con IC
diseño_srvyr %>%
  group_by(tienen_focos_ahorradores) %>%
  summarise(ingreso_corriente_hogares = survey_mean(ing_cor, vartype = "ci")) %>% 
  View()

# 3. Convertir el diseño srvyr a survey para la prueba t
diseño_survey <- as_survey_design(diseño_srvyr)

class(diseño_survey)
class(diseño_srvyr)


# 4. Ejecutar la prueba t entre los grupos
svyttest(ing_cor ~ tienen_focos_ahorradores, design = diseño_survey)



