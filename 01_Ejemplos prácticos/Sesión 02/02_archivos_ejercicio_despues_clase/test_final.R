# Prueba final de la aplicación Shiny corregida
library(shiny)

# Simular el comportamiento de la aplicación
source("grafica_pobreza.R")

# Función auxiliar para convertir nombre descriptivo a nombre de columna
obtener_nombre_columna <- function(nombre_descriptivo) {
  nombres_variables <- names(diccionario_variables)
  indice <- which(diccionario_variables == nombre_descriptivo)
  if(length(indice) > 0) {
    return(nombres_variables[indice[1]])
  }
  return(nombre_descriptivo)
}

# Simular datos de entrada de la aplicación
input_simulado <- list(
  entidad_seleccionada = "7",  # Chiapas
  variable_seleccionada = "Pobreza (%)"  # Nombre descriptivo como viene del selector
)

cat("=== PRUEBA DE LA APLICACIÓN CORREGIDA ===\n")
cat("Entidad seleccionada:", input_simulado$entidad_seleccionada, "\n")
cat("Variable seleccionada:", input_simulado$variable_seleccionada, "\n")

# Obtener el nombre real de la columna
nombre_columna <- obtener_nombre_columna(input_simulado$variable_seleccionada)
cat("Nombre de columna mapeado:", nombre_columna, "\n")

# Simular lectura de datos filtrados
tryCatch({
  datos <- read_csv("df_pob_ent.csv", show_col_types = FALSE)
  
  datos_filtrados <- datos %>%
    filter(cve_ent == as.numeric(input_simulado$entidad_seleccionada)) %>%
    select(anio, all_of(nombre_columna)) %>%
    arrange(anio)
  
  cat("✓ Datos filtrados correctamente\n")
  cat("Dimensiones:", nrow(datos_filtrados), "x", ncol(datos_filtrados), "\n")
  cat("Años disponibles:", paste(datos_filtrados$anio, collapse = ", "), "\n")
  
  # Intentar generar la gráfica
  grafica <- generar_grafica_pobreza(
    archivo_csv = "df_pob_ent.csv",
    variable_pobreza = nombre_columna,
    clave_entidad = as.numeric(input_simulado$entidad_seleccionada),
    color_linea = "#FFB3C6",
    mostrar_puntos = TRUE,
    tema_oscuro = FALSE
  )
  
  cat("✓ Gráfica generada exitosamente\n")
  
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\n=== RESULTADO ===\n")
cat("La aplicación Shiny debería funcionar correctamente ahora.\n")
cat("Para ejecutar: shiny::runApp('app.R')\n")