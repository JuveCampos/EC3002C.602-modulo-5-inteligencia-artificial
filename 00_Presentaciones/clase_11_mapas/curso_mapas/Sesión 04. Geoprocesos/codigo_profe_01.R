
# Librerias ----
library(tidyverse)
library(sf)
library(cartogram)
library(leaflet)

# Bases de datos: 
# 1. DENUE. 
# 2. Manzanas. 
# 3. Colonias. 

ageb <- read_sf("01_Datos/agebs_cdmx/09a.shp")
plot(ageb, max.plot = 1)
colonias <- read_sf("01_Datos/georef-mexico-colonia.geojson")
plot(colonias, max.plot = 1)
secciones <- st_read("01_Datos/secciones_electorales_cdmx.geojson") 
municipios <- read_sf("01_Datos/municipios_2022.geojson")
coordenadas_coneval <- c(19.389861093506845, -99.17351018412312)
denue <- readRDS("/Volumes/Extreme\ SSD/DATASETS/INEGI\ -\ DENUE/big_denues/denue_2023.rds")

denue_cdmx <- denue %>% 
  filter(cve_ent %in% c("9", "09"))

sum(str_detect(str_to_lower(denue$nom_estab), pattern = "costco"))

costcos <- denue[which(str_detect(str_to_lower(denue$nom_estab), pattern = "costco")),] 

costcos <- costcos %>% 
  filter(per_ocu %in% c("251 y más personas", "101 a 250 personas")) %>% 
  select(nom_estab, longitud, latitud) %>% 
  rbind(tibble(nom_estab = "COSTCO POLANCO",
                longitud = -99.20578651317571,
                latitud = 19.44130483208074)) %>% 
  rbind(tibble(nom_estab = "COSTCO SANTA FE",
               longitud = -99.27492454691043,
               latitud = 19.35385404034389))

# 19.44130483208074, -99.20578651317571
19.35385404034389, -99.27492454691043

costcos %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers()

# %>% 
#   filter(nombre_act == "Comercio al por menor en supermercados")

write_csv(costcos, "01_Datos/costcos.csv")

denue_cdmx %>% 
  saveRDS("01_Datos/denue_cdmx.rds")

acts <- unique(denue_cdmx$nombre_act) %>% sort()
ind <- str_detect(str_to_lower(acts),
           "idioma") 
acts[ind]

"Restaurantes con servicio de preparación de tacos y tortas"

taquerias <- denue_cdmx %>% 
  filter(nombre_act == "Restaurantes con servicio de preparación de tacos y tortas")
write_csv(taquerias, "01_Datos/taquerias.csv")


# unique(denue$cve_ent)


# 1. Genere un buffer de 100 metros alrededor del coneval
punto_coneval <- tibble(latitud = coordenadas_coneval[1], 
       longitud = coordenadas_coneval[2]) %>% 
  st_as_sf(coords = c("longitud", "latitud"), 
           crs = 4326) 

buffer_coneval <- punto_coneval %>% 
  st_transform(crs = 6362) %>%
  st_buffer(dist = 1000) %>% 
  st_transform(crs = 4326) 

names(taquerias)
taquerias_shp <- taquerias %>% 
  st_as_sf(coords = c("longitud","latitud" ), 
           crs = 4326)

taquerias_intersect <- st_intersection(taquerias_shp, buffer_coneval)


buffer_coneval %>% 
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(data = punto_coneval, color = "red") %>% 
  addCircleMarkers(data = taquerias_intersect, color = "green", radius = 1) %>% 
  addPolygons() 


st_area(buffer_coneval)
3.141


poligonos_edos <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

poligonos_edos %>% 
  mutate(X = st_centroid(.) %>% st_coordinates() %>% as_tibble() %>% pull("X"), 
         Y = st_centroid(.) %>% st_coordinates() %>% as_tibble() %>% pull("Y")) %>% 
  ggplot() + 
  geom_sf(fill = "yellow", alpha = 0.5) + 
  geom_point(aes(x = X, y = Y), color = "red")

