
# Librerias 
library(survey)
library(srvyr)
library(tidyverse)
library(foreign)

poblacion <- read.dbf("01_datos/poblacion.dbf") %>% 
  as_tibble()

pp <- poblacion %>% 
  mutate(es.joven = case_when(edad >= 30 ~ "Adultos", 
                              between(edad, 25, 29) ~ "Jovenes adultos", 
                              between(edad, 18,24) ~ "Jóvenes jovenes", 
                              TRUE ~ "Menores de edad")) %>% 
  select(edo_conyug, es.joven, factor) %>% 
  mutate(tiene.pareja = ifelse(edo_conyug %in% c(1:4),  "Pareja", "Soltero")) %>% 
  filter(es.joven != "Menores de edad")

bd_plot <- pp %>% 
  as_survey_design(weights = factor) %>% 
  group_by(es.joven) %>% 
  survey_count(vartype = "ci")

bd_plot %>% 
  ggplot(aes(x = es.joven, y = n, fill = es.joven)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = n_low, ymax = n_upp))

poblacion %>% 
  filter(parentesco == 101) %>% 
  select(factor) %>% 
  pull(factor) %>% 
  sum()
  