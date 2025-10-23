
# Librerias 
library(survey)
library(srvyr)
library(tidyverse)
library(foreign)

# Datos --
# concentrado_hogar
conc_hogar <- readRDS("01_datos/concentrado_hogar.rds") %>% as_tibble()
ecotecnias <- readRDS("ecotec_hogar.rds")
poblacion <- read.dbf("01_datos/poblacion.dbf") %>% 
  as_tibble()

bd_plot <- conc_hogar %>% 
  mutate(id_hogar = paste0(folioviv, "-", foliohog)) %>% 
  left_join(ecotecnias) %>% 
  select(id_hogar, factor, ecotec_1o0, edad_jefe) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(ecotec_1o0) %>% 
  summarise(edad_jefe_media = survey_mean(x = edad_jefe, 
                                                      vartype = "ci"))


bd_plot %>% 
  ggplot(aes(x = ecotec_1o0, y =edad_jefe_media, 
             fill = factor(ecotec_1o0))) + 
  geom_col() + 
  geom_errorbar(aes(ymin = edad_jefe_media_low, 
                    ymax = edad_jefe_media_upp)) 

diseño <- conc_hogar %>% 
  mutate(id_hogar = paste0(folioviv, "-", foliohog)) %>% 
  left_join(ecotecnias) %>% 
  select(id_hogar, factor, ecotec_1o0, edad_jefe) %>% 
  as_survey_design(weights = factor)

survey::svyttest(formula = edad_jefe ~ ecotec_1o0, 
                 design = diseño)

