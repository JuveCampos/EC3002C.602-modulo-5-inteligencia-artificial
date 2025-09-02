
library(tidyverse)
library(DBI)
library(RMySQL)
library(RSQLite)
library(xml2)
library(haven)
library(jsonlite)

analytics <- read_tsv("archivos_carga/analytics_web.tsv")
"archivos_carga/base_datos_clientes.sqlite"

conn <- dbConnect(SQLite(), "archivos_carga/base_datos_clientes.sqlite")
dbListTables(conn)                       # todas las tablas
dbListFields(conn, "clientes")  

xml <- read_xml("archivos_carga/catalogo_productos.xml")
sas <- read_sas("archivos_carga/cola.sas7bdat")

"archivos_carga/config_yaml.yaml"

jsonlite::fromJSON("archivos_carga/respuesta_api.json")
""
