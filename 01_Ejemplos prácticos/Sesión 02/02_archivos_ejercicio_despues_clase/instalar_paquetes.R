# Script para instalar paquetes necesarios para la aplicación Shiny

packages <- c('shiny', 'shinydashboard', 'shinyWidgets', 'DT', 'plotly')

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos='https://cran.rstudio.com/')
    library(pkg, character.only = TRUE)
  }
}

cat("Todos los paquetes han sido instalados correctamente.\n")