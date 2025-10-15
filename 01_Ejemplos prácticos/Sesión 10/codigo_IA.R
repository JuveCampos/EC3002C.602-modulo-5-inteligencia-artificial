
# Librerias 
library(tidyverse) # Manejo de datos 
library(httr) # Llamadas a las APIs
library(jsonlite) # Manejo de Jsons
library(ggimage)

# Utilice la PokeAPI y genere una tabla con el peso y la altura de los primeros 151 Pokemon, incluyendo liga a sus sprites de los juegos. 

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
datos_pkmn <- lapply(1:151, function(i){
  
  peticion <- str_glue("https://pokeapi.co/api/v2/pokemon/{i}")
  solicitud <- GET(peticion)
  contenido <- content(solicitud, "text")
  json <- fromJSON(contenido)
  
  nombre <- json$name %>% str_to_sentence()
  peso <- json$weight
  altura <- json$height
  imagen <- json$sprites$front_default
  
  tabla_datos <- tibble(numero = i,nombre, peso, altura, imagen)
  return(tabla_datos) # Forzamos a que el bucle nos regrese justo la tabla como resultado del lapply
  print(str_c("Lista descarga para ", nombre))
  
})

# Ver que el resultado del lapply es una lista
datos_pkmn

# Convertimos la lista a una tibble 
datos_pokemon <- datos_pkmn %>% 
  do.call(rbind, .) # do.call aplica la función rbind a toda (.) la lista previa. El (.) es un placeholder que significa "toda" la tabla. 
# Si recuerdan, la pipa pasa como primer argumento el objeto previo a la siguiente función. 
# El punto le dice a la pipa que no mande el objeto previo como primer argumento, sino como SEGUNDO argumento. 

# Gráfica 
datos_pokemon %>% 
  ggplot(aes(x = peso, y = altura)) + 
  geom_point() + 
  geom_image(aes(image = imagen), 
             size = 0.1) + 
  labs(title = "Distribución peso/altura de los primeros 151 pokémon")

# Utilice el API de Open Meteo API y obtenga el clima pronosticado para el día de hoy para las coordenadas del Tec de Monterrey Campus Santa Fe

19.359569, -99.258208

respuesta <- GET("https://api.open-meteo.com/v1/forecast?latitude=19.35&longitude=-99.25&hourly=temperature_2m")
json <- content(respuesta, "text")
datos <- fromJSON(json, flatten = TRUE)

time <- datos$hourly$time 
temp <- datos$hourly$temperature_2m
temperatura_tec_csf <- tibble(time, temp)

temperatura_tec_csf %>% 
  ggplot(aes(x = time, y = temp, group = 1)) + 
  geom_line() + 
  # scale_x_datetime(date_breaks = "day") + 
  theme(axis.text = element_text(angle = 90))

# Utilice el API del INEGI para determinar que entidad de la república tiene el mayor PIB estatal

library(inegiR)

inegi_series(series_id = "746098", 
             token = "682ad7f9-19fe-47f0-abec-e4c2ab2f2948", 
             database = "BIE")

# Librerias ----
library(ellmer)

chat <- chat_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = "AIzaSyA339Z-XkHuf415M15YaI3Ajek12whsD6o"
)

chat$chat("¿Qué significa este error? 
          Error in `transformation$transform()`:
! `transform_time()` works with objects of class <POSIXct> only
Run `rlang::last_trace()` to see where the error occurred.
          ")


usethis::edit_r_environ()
chat_gpt <- chat_openai(api_key = "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")



# chat_gpt$chat("Hola, como estás?")
# chat$chat("Hola")
