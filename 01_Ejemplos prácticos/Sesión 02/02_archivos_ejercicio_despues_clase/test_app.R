# Script para probar que la aplicación Shiny funciona correctamente

# Cargar librerías
library(shiny)
source("grafica_pobreza.R")

# Verificar que el archivo de datos existe
if(file.exists("df_pob_ent.csv")) {
  cat("✓ Archivo de datos encontrado\n")
} else {
  cat("✗ Archivo de datos NO encontrado\n")
}

# Verificar que las funciones del script funcionan
tryCatch({
  test_grafica <- generar_grafica_pobreza(
    archivo_csv = "df_pob_ent.csv",
    variable_pobreza = "pobreza",
    clave_entidad = 7
  )
  cat("✓ Función generar_grafica_pobreza funciona correctamente\n")
}, error = function(e) {
  cat("✗ Error en generar_grafica_pobreza:", e$message, "\n")
})

# Verificar que la aplicación puede cargar
tryCatch({
  source("app.R")
  cat("✓ Aplicación Shiny carga sin errores\n")
}, error = function(e) {
  cat("✗ Error al cargar la aplicación:", e$message, "\n")
})

cat("\nPara ejecutar la aplicación, usar:\n")
cat("shiny::runApp('app.R')\n")