
# Cambiamos el locale a españo.  
# Esto hace que si usamos fechas, los nombres de los meses salgan en español
Sys.setlocale("LC_TIME", "es_ES.UTF-8")  # Para sistemas basados en Unix/Mac
# Sys.setlocale("LC_TIME", "Spanish")    # Para sistemas Windows

# Librerias 
library(tidyverse) # Manejo de datos 
library(httr) # Llamadas a las APIs
library(jsonlite) # Manejo de Jsons
library(ggimage) # Para hacer gráficas con imagenes

# POKEAPI ----
# Utilice la PokeAPI y genere una tabla con el peso y la altura 
# de los primeros 151 Pokemon, incluyendo liga a sus sprites de los juegos. 
"https://pokeapi.co/api/v2/pokemon/ditto"

# Paso 1. Utilizamos GET
llamada <- GET("https://pokeapi.co/api/v2/pokemon/1") # Hago la llamada al API
get_data <- content(llamada, "text") # Obtengo el JSON 
get_data_from_Json <- fromJSON(get_data, flatten = TRUE)

get_data_from_Json$height
get_data_from_Json$weight
get_data_from_Json$sprites$front_default

# ARMAMOS LA PETICIÓN COMO SE ESPECIFICÓ EN LA PÁGINA DE LA POKEAPI
peticion <- "https://pokeapi.co/api/v2/pokemon/1"

# HACEMOS LA SOLICITUD GET, COMO DECÍA EN LA DOCUMENTACIÓN 
solicitud <- GET(peticion)

# EXTRAEMOS EL CONTENIDO DE LA SOLICITUD (NOS SALE UN JSON)
contenido <- content(solicitud, "text")

# CONVERTIMOS ESE JSON A LISTA
json <- fromJSON(contenido)

# CON EL SÍMBOLO "$", VAMOS EXPLORANDO LA INFORMACIÓN SOLICITADA
# EN ESTE CASO, NOS QUEDAMOS CON LOS DATOS DE PESO, ALTURA Y LA IMAGEN DE FRENTE
peso <- json$weight
altura <- json$height
imagen <- json$sprites$front_default

# Hacemos la tabla con el resto de los pokemon 
tabla_datos_pokemon <- tibble() # Tabla vacía para llenar con el for()

# Bucle para traernos todos los datos: 
for (poke in 1:151){
  
  peticion <- str_glue("https://pokeapi.co/api/v2/pokemon/{poke}")
  
  # Equivalente a lo que vimos antes: 
  # str_c("https://pokeapi.co/api/v2/pokemon/", poke)
  
  solicitud <- GET(peticion)
  contenido <- content(solicitud, "text")
  json <- fromJSON(contenido)
  
  # Sacamos los datos del JSON: 
  nombre <- json$name %>% str_to_sentence() # str_to_sentence() convierte la palabra a una palabra que inicia con mayúsculas
  peso <- json$weight
  altura <- json$height
  imagen <- json$sprites$front_default
  tabla_datos <- tibble(numero = poke,nombre, peso, altura, imagen)
  
  # Guardamos los datos
  tabla_datos_pokemon = rbind(tabla_datos_pokemon, tabla_datos)
  
  # Mensaje de que ya quedó ese registro: 
  print(str_c("Lista descarga para ", nombre))
}

# Gráfica de pesos/alturas
tabla_datos_pokemon %>% 
  ggplot(aes(x = peso, y = altura)) + 
  geom_point() + 
  geom_image(aes(image = imagen), 
             size = 0.1) + 
  labs(title = "Distribución peso/altura de los primeros 151 pokémon") + 
  theme_minimal()

# CLIMA ----
# Utilice el API de Open Meteo API y obtenga el clima pronosticado para el
# día de hoy para las coordenadas del Tec de Monterrey Campus Santa Fe

# Coordenadas: 19.359569, -99.258208

respuesta <- GET("https://api.open-meteo.com/v1/forecast?latitude=19.35&longitude=-99.25&hourly=temperature_2m")
json <- content(respuesta, "text")
datos <- fromJSON(json, flatten = TRUE)

# Extraemos del JSON de respuesta el tiempo, la temperatura y generamos una nueva tabla
time <- datos$hourly$time 
temp <- datos$hourly$temperature_2m
temperatura_tec_csf <- tibble(time, temp) %>% 
  mutate(time = as_datetime(time, format = "%Y-%m-%dT%H:%M"))
# La segunda función nos ayuda a pasar de texto a fecha/hora
# %Y-%m-%dT%H:%M significa Año-mes-diaThora:minuto

# Hacemos la gráfica. 
# Esta gráfica va a variar dependiendo el día que corramos este código
temperatura_tec_csf %>% 
  ggplot(aes(x = time, y = temp, group = 1)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "day", date_labels = "%d-%B %H:00") +
  theme(axis.text = element_text(angle = 90))

# INEGI ----
# Utilice el API del INEGI para determinar que entidad de la república tiene 
# el mayor PIB estatal

library(inegiR) # Cliente para acceder al API de INEGI
library(sf)     # Para hacer mapas

# Lo que vimos en clase: 
# La serie de Aguascalientes: 
inegi_series(series_id = "746098", 
             token = "682ad7f9-19fe-47f0-abec-e4c2ab2f2948", # Cambiar por su clave
             database = "BIE")

# Para acceder a los datos del PIBE (PIB Estatal)

# Obtenemos la serie para todos los estados. 
# Vamos a https://www.inegi.org.mx/servicios/api_indicadores.html y seleccionamos 
# Constructor de Consultas > Conjunto de datos:Banco de Información Económica (BIE) > 
#   Cuentas nacionales > Producto interno bruto por entidad federativa, base 2018 > 
#   Por actividad económica y entidad federativa > Valores a precios constantes de 2018 > 
#   Producto interno bruto, a precios de mercado 

# 746097 # Total Nacional 
# 746098 # Aguascalientes 
# ...
# 746129 # Zacatecas
# Notar como entre la clave de Aguascalientes y Zacatecas hay 32 series distintas. 
claves_para_descargar <- 746097:746129
claves_de_los_estados <- 0:32 # 0 para nacional y del 1 al 32 para los estados. 
# De la lista del INEGI se ve que están ordenados por su clave de INEGI. 

# Hacemos el bucle de descarga: 
pibe_estados <- tibble() # Tabla para llenar con los datos del PIBE

for(i in 1:length(claves_de_los_estados)){
  
  serie_pibe <- inegi_series(series_id = claves_para_descargar[i], 
               token = "682ad7f9-19fe-47f0-abec-e4c2ab2f2948", # Cambiar por su clave
               database = "BIE") %>% 
    mutate(cve_edo = claves_de_los_estados[i])
  
  pibe_estados <- rbind(pibe_estados, serie_pibe) # Llenamos la tabla vacía con los nuevos datos
  print(str_c("Listo el estado ", i))
}

# Exploramos los datos: 
pibe_estados %>% as_tibble() 

# Obtenemos el porcentaje del PIB nacional que tiene cada estado: 
porcentajes_pib <- pibe_estados %>% 
  as_tibble() %>% 
  arrange(date, cve_edo) %>% 
  group_by(date) %>% 
  mutate(porcentaje_pib = 100*(values/first(values))) %>% 
  ungroup()

# Juntamos los datos del porcentaje con nuestro catálogo de estados: 
cat_edos <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv") %>% 
  mutate(cve_edo = as.numeric(cve_ent))
porcentajes_pib <- left_join(porcentajes_pib, cat_edos)

# Hacemos un mapa para el año más reciente (esto lo vamos a ver la siguiente clase, no se estresen si no lo entienden). 

# Nos quedamos con los datos del año más reciente
datos_para_mapa <- porcentajes_pib %>% 
  mutate(date = as.numeric(date)) %>% 
  filter(date == max(date)) %>% 
  filter(entidad != "Nacional")

# Traemos las geometrías
edos_shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

# Juntamos los datos con las geometrias
mapx <- left_join(edos_shp, datos_para_mapa, by = c("CVE_EDO"="cve_ent")) %>% 
  select(ENTIDAD, entidad_abr_m, porcentaje_pib) %>% 
  mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
         ctrd_y = (st_centroid(.) %>% st_coordinates())[,2])

paleta_colores <- c("white", "navyblue")

round(mapx$porcentaje_pib,1)
str_c(mapx$entidad_abr_m, "\n", round(mapx$porcentaje_pib,1), "%")

# Generamos el mapa: 
mapx %>% 
  ggplot() + 
  geom_sf(aes(fill = porcentaje_pib)) + 
  geom_text(aes(label = str_c(entidad_abr_m, "\n", round(porcentaje_pib,1), "%"), 
                 x = ctrd_x, y = ctrd_y), 
            size = 2) + 
  scale_fill_gradientn(colours = paleta_colores, 
                       labels = scales::comma_format(suffix = "%")) + 
  labs(title = "Aportación al PIB de cada entidad federativa, 2023", 
       fill = "Aportación (%)") +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        legend.position = "bottom")

