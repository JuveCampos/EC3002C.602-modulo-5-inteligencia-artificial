
# Librerias ----
library(tidyverse)
library(srvyr)
library(foreign)

# Datos ----
# concentrado_hogar <- read.dbf("01_datos/concentradohogar.dbf")
# ingresos <- read.dbf("01_datos/ingresos.dbf")
# trabajos <- read.dbf("01_datos/trabajos.dbf")
# 
# saveRDS(concentrado_hogar, "01_datos/concentrado_hogar.rds")
# saveRDS(ingresos, "01_datos/ingresos.rds")
# saveRDS(trabajos, "01_datos/trabajos.rds")

# Ejercicios: ----
# 1. Dada la presentación de resultados: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2024/doc/enigh2024_ns_presentacion_resultados.pdf, replique los resultados de ingresos y gastos de las tablas resumen

# 2. Obtenga los ingresos corrientes promedio y los gastos corrientes promedio para las 32 entidades de la república

# 3. Obtenga el ingreso por trabajo de hombres y mujeres y calcule la brecha entre ambos sexos

# 4. En equipos de reto, defina, a partir del cuestionario, las variables de interés para su proyecto particular. Elabora la estrategia para poder obtener la información que requiere. 
