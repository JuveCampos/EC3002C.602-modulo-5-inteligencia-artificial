
# Librerias ----
library(readxl)

# Cuantas columnas y cuantas filas tiene mi tabla 
pobreza <- read_excel("df_pob_ent2.xlsx")
ncol(pobreza) # Numero de columnas
nrow(pobreza) # Numero de filas 

# ¿Qué representa cada fila en la tabla?
# Cada fila es un registro de las medidas de pobreza por año. 

# ¿Cuál es el estado con mayor porcentaje de población de pobreza (pobreza) en 2024?

pobreza %>% 
  filter(anio == 2024) %>% 
  arrange(-pobreza) %>% 
  select(anio, entidad, pobreza)

pobreza %>% 
  filter(anio == 2024) %>% 
  filter(pobreza == max(pobreza))

# ¿Cuál es el estado con menor porcentaje de carencia por acceso a la salud (ic_asalud) en 2024? 

# ic_asalud

pobreza %>% 
  filter(anio == 2024) %>% 
  arrange(ic_asalud) %>% 
  select(anio, entidad, ic_asalud)

pobreza %>% 
  filter(anio == 2024) %>% 
  filter(ic_asalud == min(ic_asalud)) %>% 
  select(anio, entidad, ic_asalud)

# ¿Cuales son los 10 estados con mayor porcentaje de pobreza extrema en 2024 (pobreza_e)? 

pobreza %>% 
  select(anio, entidad, pobreza_e) %>% 
  filter(anio == 2024) %>% 
  arrange(-pobreza_e) %>% 
  slice(1:10)

# ¿Cuál es el promedio simple del porcentaje de pobreza moderada (pobreza_m) de los estados del centro del país (CDMX, Puebla, EDOMEX, Morelos e Hidalgo) en 2024?

pobreza %>% 
  # filter(entidad == "Ciudad de México" | 
  #          entidad == "Puebla" | 
  #          entidad == "México" | 
  #          entidad == "Morelos" | 
  #          entidad == "Hidalgo") %>% 
  filter(entidad %in% c("Ciudad de México", "Morelos", 
                        "Puebla","México","Hidalgo")) %>% 
  filter(anio == 2024) %>% 
  select(anio, entidad, pobreza_m) %>% 
  summarise(promedio = mean(pobreza_m))
  
# ¿En qué año alcanzó Chiapas su porcentaje más alto de población con carencia por acceso a la seguridad social (ic_segsoc)? 

# 2018

pobreza %>% 
  filter(entidad == "Chiapas") %>% 
  select(anio, ic_segsoc) %>% 
  arrange(-ic_segsoc)

# ¿Cuál es el valor más bajo de pobreza que ha alcanzado algún estado (con los datos de la tabla) (pobreza)? 

pobreza %>% 
  filter(pobreza == min(pobreza)) %>% 
  select(anio, entidad, pobreza)

pobreza %>% 
  arrange(pobreza) %>% 
  select(anio, entidad, pobreza)

# Obtenga el promedio y la desviación estándar del porcentaje de pobreza para todos los estados, para cada año 

pobreza %>% 
  group_by(anio) %>% 
  summarise(promedio = mean(pobreza), 
            sd = sd(pobreza))
