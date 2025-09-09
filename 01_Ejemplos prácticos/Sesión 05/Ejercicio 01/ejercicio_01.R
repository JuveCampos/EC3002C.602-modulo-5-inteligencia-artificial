
# Librerias ----
library(tidyverse)
library(sf)
library(ggimage)
library(wesanderson)
library(ggrepel)

# Datos ----
atus  <- readRDS("datos_atus_2023.rds")

# 1. Exploración de datos ----
summary(atus) # Estadísticas básicas de las columnas
names(atus) # Nombres de las columnas
nrow(atus) # Numero de accidentes registrados por el ATUS

# 1. Gráfica de barras ----
# ¿Cúal es el día de la semana en donde ocurren más accidentes? 

tabla_accidentes_dia_semana <- atus %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  group_by(DIASEMANA) %>% 
  count() %>% 
  ungroup() %>% 
  filter(DIASEMANA <= 7) %>% 
  mutate(pp = 100*(n/sum(n))) %>% 
  mutate(dia_semana_texto = case_when(DIASEMANA == 1 ~ "Lunes", 
                                      DIASEMANA == 2 ~ "Martes", 
                                      DIASEMANA == 3 ~ "Miércoles", 
                                      DIASEMANA == 4 ~ "Jueves", 
                                      DIASEMANA == 5 ~ "Viernes", 
                                      DIASEMANA == 6 ~ "Sábado", 
                                      DIASEMANA == 7 ~ "Domingo")) %>% 
  mutate(dia_semana_texto = factor(dia_semana_texto, levels = c("Lunes", "Martes", "Miércoles","Jueves", "Viernes","Sábado", "Domingo")))

# Gráfica 
tabla_accidentes_dia_semana %>% 
  ggplot(aes(x = dia_semana_texto, y = n)) + 
  geom_col()

# 2. Líneas ----# 2. nullfile()Líneas ----
# Elabore una gráfica de lineas en la cual se muestre la evolución de heridos por día,
# dependiendo el tipo de víctima. 

names(atus)
# unique(atus$CICLHERIDO)
datos_heridos <- atus %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  pivot_longer(cols = contains("HERIDO")) %>% 
  group_by(MES, name) %>% 
  summarise(total_heridos = sum(value)) %>% 
  mutate(tipo_herido = case_when(name == "CICLHERIDO" ~ "Ciclista", 
                                 name == "CONDHERIDO" ~ "Conductor", 
                                 name == "OTROHERIDO" ~ "Otro", 
                                 name == "PASAHERIDO" ~ "Pasajero",
                                 name == "PEATHERIDO" ~ "Peatón",
                                 name == "TOTHERIDOS" ~ "Total"
                                 )) %>% 
  filter(!(tipo_herido %in% c("Total", "Otro")))


datos_heridos %>% 
  ggplot(aes(x = MES, y = total_heridos, group = tipo_herido)) + 
  geom_line()

# Mapa ----
# Genere un mapa de la incidencia de accidentes viales para la Ciudad de México
shp_municipal <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>% 
  mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
         ctrd_y = (st_centroid(.) %>% st_coordinates())[,2]) 
catalogo_estados <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")

atus2 <- atus %>% 
  mutate(EDO = str_pad(EDO, width = 2, side = "left", pad = "0")) %>% 
  left_join(catalogo_estados, by = c("EDO" = "cve_ent"))

# Mapa 
edo_sel = "09"

# Gráfica a mejorar ----
shp_municipal %>% 
  filter(CVE_ENT == edo_sel) %>% 
  ggplot() + 
  stat_density_2d(
    data = atus2 %>% 
      filter(EDO == edo_sel) %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
      ),
    aes(x = lon, y = lat, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.7,
    bins = 10
  ) +
  geom_sf(data = atus2 %>% filter(EDO == edo_sel)) + 
  geom_sf(fill = "transparent") + 
  geom_label_repel(aes(label = str_replace_all(NOM_MUN, pattern = " ", replacement = "\n"), 
                       x = ctrd_x,
                       y = ctrd_y), 
            size = 2, fontface = "bold") 
