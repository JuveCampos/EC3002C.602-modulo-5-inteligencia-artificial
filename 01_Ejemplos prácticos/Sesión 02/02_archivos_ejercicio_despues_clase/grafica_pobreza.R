# ================================================================================
# SCRIPT PARA GRÁFICAS DE SERIES DE TIEMPO DE POBREZA POR ENTIDAD
# ================================================================================

# Cargar librerías necesarias
library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)

# ================================================================================
# CATÁLOGO DE ENTIDADES FEDERATIVAS DE MÉXICO
# ================================================================================

# Crear catálogo con claves INEGI y nombres de entidades
catalogo_entidades <- data.frame(
  cve_ent = 1:32,
  entidad = c(
    "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
    "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
    "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
    "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León",
    "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala",
    "Veracruz", "Yucatán", "Zacatecas"
  )
)

# ================================================================================
# DICCIONARIO DE VARIABLES DE POBREZA
# ================================================================================

# Crear diccionario con nombres descriptivos de las variables
diccionario_variables <- list(
  "pobreza" = "Pobreza (%)",
  "pobreza_m" = "Pobreza Moderada (%)",
  "pobreza_e" = "Pobreza Extrema (%)",
  "vul_car" = "Vulnerabilidad por Carencias (%)",
  "vul_ing" = "Vulnerabilidad por Ingresos (%)",
  "no_pobv" = "No Pobre y No Vulnerable (%)",
  "carencias" = "Población con Carencias (%)",
  "carencias3" = "Población con 3+ Carencias (%)",
  "ic_rezedu" = "Rezago Educativo (%)",
  "ic_asalud" = "Sin Acceso a Salud (%)",
  "ic_segsoc" = "Sin Seguridad Social (%)",
  "ic_cv" = "Calidad de Vivienda (%)",
  "ic_sbv" = "Servicios Básicos Vivienda (%)",
  "ic_ali_nc" = "Acceso a Alimentación (%)",
  "plp_e" = "Pobreza Extrema por Ingresos (%)",
  "plp" = "Pobreza por Ingresos (%)"
)

# ================================================================================
# FUNCIÓN PRINCIPAL PARA GENERAR GRÁFICAS DE SERIE DE TIEMPO
# ================================================================================

generar_grafica_pobreza <- function(archivo_csv, variable_pobreza, clave_entidad, 
                                  color_linea = "#2E86AB", 
                                  mostrar_puntos = TRUE,
                                  tema_oscuro = FALSE) {
  
  # Validación de parámetros
  if (!file.exists(archivo_csv)) {
    stop("El archivo CSV no existe en la ruta especificada")
  }
  
  if (!variable_pobreza %in% names(diccionario_variables)) {
    cat("Variables disponibles:\n")
    cat(paste(names(diccionario_variables), collapse = ", "), "\n")
    stop("Variable de pobreza no válida")
  }
  
  if (!clave_entidad %in% 1:32) {
    stop("Clave de entidad debe estar entre 1 y 32")
  }
  
  # Leer datos
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  
  # Validar que las columnas necesarias existan
  columnas_requeridas <- c("cve_ent", "anio", variable_pobreza)
  if (!all(columnas_requeridas %in% names(datos))) {
    stop(paste("Faltan columnas requeridas:", 
               paste(setdiff(columnas_requeridas, names(datos)), collapse = ", ")))
  }
  
  # Filtrar datos para la entidad seleccionada
  datos_entidad <- datos %>%
    filter(cve_ent == clave_entidad) %>%
    select(anio, all_of(variable_pobreza)) %>%
    arrange(anio)
  
  if (nrow(datos_entidad) == 0) {
    stop("No se encontraron datos para la entidad especificada")
  }
  
  # Obtener nombre de la entidad
  nombre_entidad <- catalogo_entidades %>%
    filter(cve_ent == clave_entidad) %>%
    pull(entidad)
  
  # Obtener nombre descriptivo de la variable
  nombre_variable <- diccionario_variables[[variable_pobreza]]
  
  # Renombrar columna para facilitar el ploteo
  datos_entidad <- datos_entidad %>%
    rename(valor = all_of(variable_pobreza))
  
  # Configurar tema según preferencia
  if (tema_oscuro) {
    tema_base <- theme_dark()
    color_fondo <- "grey20"
    color_texto <- "white"
  } else {
    tema_base <- theme_minimal()
    color_fondo <- "white"
    color_texto <- "black"
  }
  
  # Crear la gráfica
  grafica <- ggplot(datos_entidad, aes(x = anio, y = valor)) +
    geom_line(color = color_linea, size = 1.2, alpha = 0.8) +
    
    # Añadir puntos si se especifica
    {if(mostrar_puntos) geom_point(color = color_linea, size = 3, alpha = 0.9)} +
    
    # Área sombreada bajo la línea
    geom_area(alpha = 0.1, fill = color_linea) +
    
    # Escalas
    scale_x_continuous(
      breaks = pretty_breaks(n = length(unique(datos_entidad$anio))),
      labels = function(x) as.character(x)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      breaks = pretty_breaks(n = 6)
    ) +
    
    # Etiquetas
    labs(
      title = paste("Evolución de", nombre_variable),
      subtitle = paste("Estado:", nombre_entidad, 
                      "| Período:", min(datos_entidad$anio), "-", max(datos_entidad$anio)),
      x = "Año",
      y = nombre_variable,
      caption = "Fuente: Datos de pobreza CONEVAL | Elaboración propia"
    ) +
    
    # Tema y personalización
    tema_base +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = color_texto),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = color_texto, margin = margin(b = 20)),
      plot.caption = element_text(size = 9, hjust = 1, color = "grey60"),
      axis.title = element_text(size = 12, face = "bold", color = color_texto),
      axis.text = element_text(size = 10, color = color_texto),
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_line(color = "grey95", size = 0.25),
      plot.background = element_rect(fill = color_fondo, color = NA),
      panel.background = element_rect(fill = color_fondo, color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Mostrar estadísticas resumidas
  cat("\n=== ESTADÍSTICAS RESUMIDAS ===\n")
  cat("Entidad:", nombre_entidad, "\n")
  cat("Variable:", nombre_variable, "\n")
  cat("Período:", min(datos_entidad$anio), "-", max(datos_entidad$anio), "\n")
  cat("Valor mínimo:", round(min(datos_entidad$valor, na.rm = TRUE), 2), "%\n")
  cat("Valor máximo:", round(max(datos_entidad$valor, na.rm = TRUE), 2), "%\n")
  cat("Valor promedio:", round(mean(datos_entidad$valor, na.rm = TRUE), 2), "%\n")
  cat("Cambio total:", round(last(datos_entidad$valor) - first(datos_entidad$valor), 2), "puntos porcentuales\n")
  
  return(grafica)
}

# ================================================================================
# FUNCIÓN AUXILIAR PARA EXPLORAR DATOS DISPONIBLES
# ================================================================================

explorar_datos <- function(archivo_csv) {
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  
  cat("=== INFORMACIÓN DEL DATASET ===\n")
  cat("Dimensiones:", nrow(datos), "filas x", ncol(datos), "columnas\n")
  cat("Años disponibles:", paste(sort(unique(datos$anio)), collapse = ", "), "\n")
  cat("Entidades disponibles:", paste(sort(unique(datos$cve_ent)), collapse = ", "), "\n\n")
  
  cat("=== VARIABLES DE POBREZA DISPONIBLES ===\n")
  variables_disponibles <- names(diccionario_variables)
  for (i in seq_along(variables_disponibles)) {
    cat(sprintf("%2d. %-15s: %s\n", i, variables_disponibles[i], 
                diccionario_variables[[variables_disponibles[i]]]))
  }
  
  cat("\n=== ENTIDADES FEDERATIVAS ===\n")
  for (i in 1:nrow(catalogo_entidades)) {
    cat(sprintf("%2d. %s\n", catalogo_entidades$cve_ent[i], catalogo_entidades$entidad[i]))
  }
}

# ================================================================================
# FUNCIÓN PARA COMPARAR MÚLTIPLES ENTIDADES
# ================================================================================

comparar_entidades <- function(archivo_csv, variable_pobreza, claves_entidades, 
                              paleta_colores = "viridis") {
  
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  
  # Filtrar y preparar datos
  datos_comparacion <- datos %>%
    filter(cve_ent %in% claves_entidades) %>%
    select(cve_ent, anio, all_of(variable_pobreza)) %>%
    left_join(catalogo_entidades, by = "cve_ent") %>%
    rename(valor = all_of(variable_pobreza))
  
  # Obtener nombre descriptivo de la variable
  nombre_variable <- diccionario_variables[[variable_pobreza]]
  
  # Crear gráfica comparativa
  ggplot(datos_comparacion, aes(x = anio, y = valor, color = entidad)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 2.5, alpha = 0.9) +
    scale_color_viridis_d(option = paleta_colores, name = "Entidad") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = paste("Comparación:", nombre_variable),
      subtitle = paste("Múltiples entidades | Período:", 
                      min(datos_comparacion$anio), "-", max(datos_comparacion$anio)),
      x = "Año",
      y = nombre_variable,
      caption = "Fuente: Datos de pobreza CONEVAL | Elaboración propia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# ================================================================================
# EJEMPLO DE USO Y VISUALIZACIÓN DE MUESTRA
# ================================================================================

# Ruta al archivo de datos
archivo_datos <- "df_pob_ent.csv"

# Explorar datos disponibles (opcional)
# explorar_datos(archivo_datos)

# Generar gráfica de muestra: Pobreza en Chiapas
cat("Generando gráfica de muestra: Pobreza en Chiapas\n")
grafica_muestra <- generar_grafica_pobreza(
  archivo_csv = archivo_datos,
  variable_pobreza = "pobreza",
  clave_entidad = 7,  # Chiapas
  color_linea = "#E63946",
  mostrar_puntos = TRUE,
  tema_oscuro = FALSE
)

# Mostrar la gráfica
print(grafica_muestra)

# Guardar la gráfica (opcional)
ggsave("pobreza_chiapas.png", grafica_muestra, width = 12, height = 8, dpi = 300)

# Ejemplo de comparación múltiple
cat("\nGenerando gráfica comparativa: Pobreza extrema en 4 estados\n")
grafica_comparativa <- comparar_entidades(
  archivo_csv = archivo_datos,
  variable_pobreza = "pobreza_e",
  claves_entidades = c(7, 20, 12, 9),  # Chiapas, Oaxaca, Guerrero, CDMX
  paleta_colores = "plasma"
)

print(grafica_comparativa)

# Guardar gráfica comparativa
ggsave("comparacion_pobreza_extrema.png", grafica_comparativa, width = 14, height = 8, dpi = 300)

cat("\nScript completado. Gráficas generadas y guardadas.\n")
