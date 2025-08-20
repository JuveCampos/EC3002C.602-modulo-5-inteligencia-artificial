# Opciones: ----
options(scipen = 999)
# Librerias ----
library(tidyverse)
library(sf)

a <- 10
a + 20
calificaciones <- c(10, 9.5, 9.7)
colores_extras <- c("#4D5BF0","#BB49E6","#0ACFCE","#E84D9A","#E8634D","#FF8654")
mean(calificaciones)

# Cargar datos de archivos externos ----
datos_pobreza <- read_csv("df_pob_ent.csv")
# library(readr)
# df_pob_ent <- read_csv("df_pob_ent.csv")
# View(df_pob_ent)

# Obtener el dato de pobreza para Morelos para el año 2022. 
# %>% %>% 

mean(calificaciones)
calificaciones %>% mean()

datos_filtrados <- datos_pobreza %>% 
  filter(anio == 2022) %>% 
  filter(cve_ent == 17) %>% 
  select(cve_ent, anio, pobreza)

datos_filtrados <- datos_pobreza %>% 
  filter(anio == 2022, cve_ent == 17) %>% 
  select(cve_ent, anio, pobreza)

# Obtengan el valor de pobreza extrema y vulnerabilidad por carencias para el estado de Guerrero para el año 2024

datos_pobreza %>% 
  filter(anio == 2024, cve_ent == 12) %>% 
  select(cve_ent, anio, pobreza_e, vul_car)

# Obtengan la serie de valores de pobreza moderada para el estado de Veracruz. Todos los años disponibles

# pobreza_m

