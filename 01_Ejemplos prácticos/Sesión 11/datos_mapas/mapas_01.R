
# Cargamos librerías ----
library(tidyverse) # Manejo de datos
library(sf) # Mapas 
library(readr)
library(scales)
library(leaflet)

# 1ero, cargamos los datos ----
prep_municipal <- read_csv("prep_municipal.csv")
class(prep_municipal)
mpios_shape <- read_sf("municipios_2022.geojson")
class(mpios_shape)
edos_shape <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

mapx <- left_join(mpios_shape, prep_municipal, 
          by = c("CVEGEO" = "CVE_INEGI"))

mapx %>% 
  mutate(pp_maynez = 100*(votos_jam/total_votos_calculados)) %>% 
  ggplot(aes(fill = pp_maynez)) + 
  geom_sf(linewidth = 0) + 
  geom_sf(data = edos_shape, fill = NA, color = "#eb5e17") + 
  labs(title = "Porcentaje de votos obtenidos por Maynez", 
       subtitle = "Elecciones presidenciales 2024",
       x = NULL, y = NULL, 
       fill = "% de votos",
       caption = "Fuente: Prep del INE, 2024") + 
  scale_fill_gradientn(colours = c("white", "orange", "red"), 
                       labels = scales::comma_format(suffix = "%")) + 
  theme_minimal() + 
  theme(text = element_text(family = "Helvetica"), 
        axis.text = element_blank(), 
        plot.title = element_text(size = 20, color = "orange"),
        legend.position = "right",
        panel.grid = element_blank())

# Mapa categórico 
mapx %>% 
  mutate(coalicion_ganadora = case_when(coalicion_ganadora == "MC" ~ "Movimiento Ciudadano", 
                                        coalicion_ganadora == "FyCXM" ~ "Fuerza y corazón por México",
                                        coalicion_ganadora == "SHH" ~ "Sigamos Haciendo Historia",
                                        TRUE ~ NA
                                        )) %>% 
  ggplot(aes(fill = coalicion_ganadora)) + 
  geom_sf(linewidth = 0) + 
  geom_sf(data = edos_shape, fill = NA, color = "white") + 
  labs(title = "Coalición ganadora por municipio", 
       subtitle = "Elecciones presidenciales 2024",
       x = NULL, y = NULL, 
       fill = "Coaliciones",
       caption = "Fuente: Prep del INE, 2024") + 
  scale_fill_manual(values = c("Fuerza y corazón por México" = "blue",
                               "Sigamos Haciendo Historia" = "brown",
                               "Movimiento Ciudadano" = "orange")) + 
  theme_minimal() + 
  theme(text = element_text(family = "Helvetica"), 
        axis.text = element_blank(), 
        plot.title = element_text(size = 20, color = "brown", face = "bold"),
        legend.position = "right",
        panel.grid = element_blank())

# Mapa de puntos ----
delitos_2024 <- read_csv("carpetasFGJ_2024.csv") %>% 
  filter(!is.na(longitud))
class(delitos_2024)

# Para transformar exceles a objetos sf (mapas): 
delitos_2024_shp <- st_as_sf(x = delitos_2024, 
                             coords = c("longitud", "latitud"), crs = 4326)

alcaldias <- read_sf("alcaldias.kml")

class(delitos_2024_shp)

# Para ver los tipos de delito: 
unique(delitos_2024_shp$delito) %>% sort()

delitos_2024_shp %>% 
  filter(delito %in% c("ROBO A PASAJERO EN RTP CON VIOLENCIA", 
                       "ROBO A TRANSEUNTE A BORDO DE TAXI PÚBLICO Y PRIVADO CON VIOLENCIA" , "ROBO DE ANIMALES"
                       )) %>% 
  ggplot() + 
  geom_sf(data = alcaldias, fill = "gray90") + 
  geom_sf(aes(color = delito),
          alpha = 1) + 
  theme_minimal()

# Mapa de líneas
# "Shape Frontera/"
  
frontera <- read_sf("Shape Frontera/Mexico_and_US_Border.shp")

frontera %>% 
  ggplot() + 
  geom_sf()

# Mapas interactivos ----

library(leaflet)

# 1. Paleta de colores
# colorNumeric()
unique(mapx$coalicion_ganadora)
paleta <- colorFactor(palette = c("blue",  "orange", "brown"),
                      domain = c("FyCXM", "MC", "SHH"))

# 2. Hacer las etiquetas popup
mapx
popup <- str_c("<b>Municipio:</b> ", mapx$NOMGEO, ", ", mapx$nom_ent, "<br>", 
               "<b>Coalición ganadora:</b> ", mapx$coalicion_ganadora, "<br>", 
               "<b>Participación:</b> ", round(mapx$porcentaje_participacion, 1), "%"
               )

# 3. Hacer las etiquetas label
etiquetas <- str_c("Municipio: ", mapx$NOMGEO, ", ", mapx$nom_ent)

leaflet(mapx) %>% 
  addTiles() %>% 
  addPolygons(label = etiquetas, 
              popup = popup, 
              fillColor = paleta(mapx$coalicion_ganadora), 
              color = "white", 
              opacity = 1, 
              fillOpacity = 0.5, 
              weight = 1
              ) %>% 
  addPolygons(data = edos_shape, color = "black", weight = 2, 
              fill = NA) %>% 
  addLegend(position = "bottomright", 
            pal = paleta, 
            values = mapx$coalicion_ganadora,
            title = "Coalición<br>ganadora")


# Delitos ----

# Mapa de robos: 

delitos_seleccionados <- delitos_2024_shp %>% 
  filter(str_detect(delito, pattern = "HOMICIDIO"))

# 1. Paleta de colores
# colorFactor(palette = )

# 2. Popups 
popup_delitos <- paste0("<b>Delito: </b>", delitos_seleccionados$delito, "<br>", 
                        "Alcaldía: ", delitos_seleccionados$alcaldia_hecho, "<br>", 
                        "Fecha: ", delitos_seleccionados$fecha_hecho)
# 3. Labels 
label_delitos <- paste0("<b>Delito: </b>", delitos_seleccionados$delito, "<br>") %>% 
  lapply(htmltools::HTML)

delitos_seleccionados %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(popup = popup_delitos, 
                   label = label_delitos, 
                   opacity = 1, 
                   color="red",
                   radius = 0.05)


