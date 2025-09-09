
# Ejercicio de carga de base de datos SQL en R. 

# Librerías ----
# Verifiquw que las tenga instaladas. 
library(DBI)          # Interface general para bases de datos
library(RSQLite)      # Driver específico para SQLite
library(dbplyr)       # Traducción de dplyr a SQL
library(tidyverse)

# Crear conexión inicial para ejecutar scripts SQL ----
ruta_bd <- "mr_rib_eye.db"
con <- dbConnect(RSQLite::SQLite(), ruta_bd)

# Listar todas las tablas en la base de datos
tablas_disponibles <- dbListTables(con)
print(tablas_disponibles)
tablas_disponibles %>% writeLines()

# ¿Cuantas tablas tiene la base de datos? 

# La función dplyr::tbl() permite crear tablas sql con evaluación floja para explorar los datos
ventas <- tbl(con, "ventas")
# Realice la evaluación lazy para el resto de las tablas de la base de datos: 
# Compras_proveedores, detalle_ventas, empleados, nomina, productos, productos_mas_vendidos, proveedores, rendimiento_empleados, resumen_ventas_sucursal, sucursales, ventas
# Guarde esas tablas en un objeto de R que tenga el mismo nombre que la tabla 

# Ejemplo 1: Crear una consulta simple
# De la tabla "ventas", genere una consulta lazy de las ventas donde el total de la venta sea mayor a $1000, ordenadas de mayor a menor. 
consulta_lazy <- ventas %>%
  filter(total_venta > 1000) %>%
  arrange(desc(total_venta)) 

# Ver el SQL generado SIN ejecutar la consulta
message("\n🔍 SQL generado por dbplyr:")
consulta_lazy %>% show_query() # ¿Qué le imprimió este código? 

# Ejecutar y traer resultados a R
resultados <- consulta_lazy %>% collect() # ¿Que le generó este código? 

# Consultas SQL tradicionales en R. 

# Usar tbl() con sql() para ejecutar SQL personalizado
query_sql_directo <- tbl(con, sql("
  SELECT `ventas`.*
  FROM `ventas`
  WHERE (`total_venta` > 1000.0)
  ORDER BY `total_venta` DESC
"))

# Con estas funciones, más lo que sabe de su clase dle viernes, genere las siguientes gráficas: 

# 1) Los cinco productos más vendidos


# 2) Los cinco meseros con más ventas
# 3) Los cinco proveedores con más compras

