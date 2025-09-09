
# Repaso del tidyverse
# Los datos provienen de esta liga: https://www.sct.gob.mx/carreteras/direccion-general-de-servicios-tecnicos/datos-viales/ y localmente están guardados en el archivo "00_datos_transito/aforos_puebla.csv"

library(tidyverse)

# 0. Cargue los datos del archivo
aforos_puebla <- read_csv("00_datos_transito/aforos_puebla.csv")

# 1. [nrow()] ¿Cuantos cruces hay con datos de aforo?
n_cruces <- nrow(aforos_puebla)
n_cruces
# R. 199 cruces

# 2. [arrange(), min(), max()] ¿Entre que fechas se levantaron los datos?
min(aforos_puebla$Fecha) # "2017-04-17"
max(aforos_puebla$Fecha) # "2019-05-28"

# 3. [filter()] ¿En qué cruce de Puebla es donde cruzaron más peatones? ¿Más Ciclistas? ¿Más Transporte público? ¿Más transporte de carga? ¿Más Total vehicular?

names(aforos_puebla)

# Cruce con mayor tránsito de peatones
aforos_puebla %>%
  filter(Total_Peatonal == max(Total_Peatonal)) %>%
  select(Ubicación_calle_1, Ubicación_calle_2)

# Cruce con mayor tránsito de ciclistas
aforos_puebla %>%
  filter(Total_Ciclista == max(Total_Ciclista)) %>%
  select(Ubicación_calle_1, Ubicación_calle_2)

# 4. [group_by(), count()] ¿Cual es la colonia en la que se instalaron más medidores de aforo?

aforos_puebla %>%
  group_by(Colonia) %>%
  count() %>%
  arrange(-n)

aforos_puebla %>%
  group_by(Colonia) %>%
  count() %>%
  # ungroup() %>%
  filter(n == max(n))


# 5. [slice(),  pivot_longer(), ggplot()] Seleccione el primer cruce peatonal. Elabore una gráfica para el total de cruces de todos los tipos que ocurrieron en este primer cruce, de tal forma que tenga una columna para "total ciclistas", una para "transporte público", una para "total vehicular", una para "transporte carga" y una para "total peatonal". Primero, quedese con las columnas que inicien con la palabra "Total", después haga pivot_longer() para tener una tabla en formato largo y finalmente, con esa tabla en formato largo, elabore la gráfica de ggplot().

aforos_puebla %>%
  slice(1) %>%
  select(Ubicación_calle_1, Ubicación_calle_2, contains("Total")) %>%
  pivot_longer(cols = Total_Peatonal:Total_Transporte_Carga) %>%
  filter(value > 0) %>%
  # mutate(name = str_replace_all(name,
  #                               pattern = "_",
  #                               replacement = " ")) %>%  %>%
  mutate(name = case_when(name == "Total_Peatonal" ~ "Peatones",
                          name == "Total_Vehicular" ~ "Vehículos",
                          TRUE ~ name)) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_col() + 
  geom_text(aes(label = value), 
            vjust = -0.1) + 
  scale_fill_manual(values = c("red", "green", "olivedrab", "salmon", "mediumpurple", "#000000")) + 
  labs(y = "Cruces", x = "Tipos de cruce", 
       subtitle = "Ciudad de Puebla", 
       fill = "Categoria",
       title = "Aforo en el cruce del renglón 1") + 
  theme_light() + 
  theme(text = element_text(family = "Arial"))

# 6. [ggsave()]. Guarde la gráfica previa en un archivo *.png, de 10 puntos de ancho, 6 puntos de alto y 200 de dpi.

ggsave("grafica_cruces_renglon_1.png", width = 10, height = 6, dpi = 200)

# 7. [function()]. Genere una función que genere esta gráfica variando el número de renglón.

renglon_seleccionado <- 1

aforos_puebla %>%
  slice(renglon_seleccionado) %>%
  select(Ubicación_calle_1, Ubicación_calle_2, contains("Total")) %>%
  pivot_longer(cols = Total_Peatonal:Total_Transporte_Carga) %>%
  filter(value > 0) %>%
  mutate(name = case_when(name == "Total_Peatonal" ~ "Peatones",
                          name == "Total_Vehicular" ~ "Vehículos",
                          TRUE ~ name)) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_col() + 
  geom_text(aes(label = value), 
            vjust = -0.1) + 
  scale_fill_manual(values = c("red", "green", "olivedrab", "salmon", "mediumpurple", "#000000")) + 
  labs(y = "Cruces", x = "Tipos de cruce", 
       subtitle = "Ciudad de Puebla", 
       fill = "Categoria",
       title = paste0("Aforo en el cruce del renglón ", renglon_seleccionado)
       ) + 
  theme_light() + 
  theme(text = element_text(family = "Arial"))


genera_la_grafica_del_renglon <- function(renglon_seleccionado){
  
  aforos_puebla %>%
    slice(renglon_seleccionado) %>%
    select(Ubicación_calle_1, Ubicación_calle_2, contains("Total")) %>%
    pivot_longer(cols = Total_Peatonal:Total_Transporte_Carga) %>%
    filter(value > 0) %>%
    mutate(name = case_when(name == "Total_Peatonal" ~ "Peatones",
                            name == "Total_Vehicular" ~ "Vehículos",
                            TRUE ~ name)) %>%
    ggplot(aes(x = name, y = value, fill = name)) +
    geom_col() + 
    geom_text(aes(label = value), 
              vjust = -0.1) + 
    scale_fill_manual(values = c("red", "green", "olivedrab", "salmon", "mediumpurple", "#000000")) + 
    labs(y = "Cruces", x = "Tipos de cruce", 
         subtitle = "Ciudad de Puebla", 
         fill = "Categoria",
         title = paste0("Aforo en el cruce del renglón ", renglon_seleccionado)
    ) + 
    theme_light() + 
    theme(text = element_text(family = "Arial"))
  
}

# A lo que hay que llegar: 
genera_la_grafica_del_renglon(renglon_seleccionado = 45)

# 8. [for()]. Genere un bucle que permita guardar las 199 gráficas en una carpeta dentro del directorio de trabajo. Llame a la carpeta "graficas_puebla".

for(cruce in 1:199){
  genera_la_grafica_del_renglon(renglon_seleccionado = cruce)
  ggsave(paste0("graficas_puebla/grafica_cruces_renglon_", cruce, ".png"), 
         width = 10, height = 6, dpi = 200)
  print(paste0("Ya está lista la gráfica", cruce))
}

