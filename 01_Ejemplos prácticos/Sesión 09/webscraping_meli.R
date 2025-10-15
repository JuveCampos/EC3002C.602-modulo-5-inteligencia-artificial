
anuncios <- c(2, 49, 97, 145)

tabla_vacia <- tibble()

for(anuncio in anuncios){
    
    url <- str_c("https://autos.mercadolibre.com.mx/tesla/tesla_Desde_", 
                 anuncio, 
                 "_NoIndex_True?sb=all_mercadolibre")
    
    codigo_pagina <- read_html(url)
    
    # Extraemos el precio: 
    precios <- codigo_pagina %>% 
      html_nodes(".poly-price__current") %>% 
      html_text()
    
    descripcion <- codigo_pagina %>% 
      html_nodes("h2") %>% 
      html_nodes("a") %>% 
      html_text()
    
    pegado <- codigo_pagina %>% 
      html_nodes(".poly-component__attributes-list") %>% 
      html_text()
    
    anios <- str_extract(pegado, pattern = "\\d{4}")
    kilometraje <- str_remove(pegado, pattern = "\\d{4}")
    
    tabla_teslas <- tibble(descripcion, precios, anios, kilometraje)
    
    tabla_vacia <- rbind(tabla_vacia, tabla_teslas)
    
    print(str_c("Listo desde el anuncio ", anuncio))

}

tabla_vacia


