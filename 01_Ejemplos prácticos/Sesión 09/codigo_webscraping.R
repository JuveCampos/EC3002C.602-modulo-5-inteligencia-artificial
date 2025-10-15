
# Paquete: 
library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Legality_of_cannabis"

paises_cannabis <- read_html(url) %>% 
  html_table() %>% 
  pluck(3)

no_pagina <- 2

tabla_datos <- tibble()

for(no_pagina in 1:10){
  
    html_juve <- read_html(str_c("https://atiempo.tv/author/juvenal-campos/page/", no_pagina, "/"))

    ligas <- html_juve %>% 
      html_nodes("h3") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    titulos <- html_juve %>% 
      html_nodes("h3") %>% 
      html_nodes("a") %>% 
      html_text()
    
    previews <- html_juve %>% 
      html_nodes(".entry-content") %>% 
      html_nodes("p") %>% 
      html_text()
    
    fechas <- html_juve %>% 
      html_nodes(".entry-date") %>% 
      html_nodes("time") %>% 
      html_text()
    
    datos_articulos <- tibble(titulos, ligas, previews, fechas)
    tabla_datos <- rbind(tabla_datos, datos_articulos) # Acá llenamos la "bolsa" de datos
    
    print(str_c("Listo la pagina ", no_pagina))

}

tabla_datos

# Extracción de contenido de cada una de las 111 páginas 

bolsa_articulos <- c()

for(articulo_individual in 1:111){

    contenido_articulo <- read_html(tabla_datos$ligas[articulo_individual])
    
    articulo_extraido <- contenido_articulo %>% 
      html_nodes(".entry-content") %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      str_c(collapse = "\n\n")
    
    bolsa_articulos <- append(bolsa_articulos, articulo_extraido) # Codigo que va a llenar la bolsa vacía
    
    print(str_c("Listo el articulo ", articulo_individual))

}

tabla_datos$contenido <- bolsa_articulos

# Guardamos los articulos en un excel: 
openxlsx::write.xlsx(tabla_datos, "articulos_juve.xlsx")





