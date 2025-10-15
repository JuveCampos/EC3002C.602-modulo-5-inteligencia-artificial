
library(tidyverse)
library(curl)

https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_01.xls
https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_02.xls


str_c()
paste0()

str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_01.xls")

# STRC pega dos cadenas de texto: 
str_c("a", "b")

# Acá vamos a generar las 32 ligas de descarga: 
str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_", 1:32, ".xls" )

# Acá vamos a pegarle el cero a todos los numeros del 1 al 9: 
links_descarga <- str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_",
      c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:32),
      ".xls" )

no_edo <- 2
curl_download(url = links_descarga[no_edo],
              destfile = str_c("educacion_", no_edo, ".xls"))

# Genera una carpeta con este nombre: 
dir.create("archivos_educacion")

# Generar el loop/bucle de descarga 
for(no_edo in 1:32){
  curl_download(url = links_descarga[no_edo],
                destfile = str_c("archivos_educacion/educacion_", no_edo, ".xls"))
  print(str_c("Listo el archivo ", no_edo))
}

https://codeshare.io/alALzj



sample(1:5)[1]





