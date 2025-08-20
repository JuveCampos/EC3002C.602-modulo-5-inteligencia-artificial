# Prueba simple de la función de mapeo

source("grafica_pobreza.R")

# Función auxiliar para convertir nombre descriptivo a nombre de columna
obtener_nombre_columna <- function(nombre_descriptivo) {
  # Buscar el nombre de la variable original que corresponde al nombre descriptivo
  nombres_variables <- names(diccionario_variables)
  indice <- which(diccionario_variables == nombre_descriptivo)
  if(length(indice) > 0) {
    return(nombres_variables[indice[1]])
  }
  return(nombre_descriptivo) # Si no se encuentra, devolver el original
}

# Probar la función
cat("Diccionario de variables:\n")
print(diccionario_variables)

cat("\nPrueba de conversión:\n")
cat("'Pobreza (%)' -> '", obtener_nombre_columna("Pobreza (%)"), "'\n")
cat("'Pobreza Extrema (%)' -> '", obtener_nombre_columna("Pobreza Extrema (%)"), "'\n")
cat("'pobreza' -> '", obtener_nombre_columna("pobreza"), "'\n")