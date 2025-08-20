# ================================================================================
# APLICACIÓN SHINY PARA VISUALIZACIÓN DE DATOS DE POBREZA EN MÉXICO
# ================================================================================

# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggplot2)
library(scales)
library(plotly)

# Source del script con las funciones de gráficas
source("grafica_pobreza.R")

# (((((((((())))))))))

# ================================================================================
# CONFIGURACIÓN DE COLORES PASTEL
# ================================================================================

colores_pastel <- list(
  primario = "#A8DADC",      # Azul pastel
  secundario = "#F1FAEE",    # Blanco hueso
  acento1 = "#FFB3C6",       # Rosa pastel
  acento2 = "#FDEAA7",       # Amarillo pastel
  acento3 = "#C7CEEA",       # Lavanda pastel
  acento4 = "#B8E6B8",       # Verde pastel
  texto = "#2F3E46",         # Gris oscuro
  fondo = "#F8F9FA"          # Gris muy claro
)

# ================================================================================
# INTERFAZ DE USUARIO (UI)
# ================================================================================

ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; padding: 10px;",
      tags$i(class = "fas fa-chart-line", style = "margin-right: 10px; color: #A8DADC;"),
      "Observatorio de Pobreza México"
    ),
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    tags$head(
      tags$style(HTML(paste0("
        .main-sidebar, .left-side {
          background-color: ", colores_pastel$fondo, " !important;
        }
        .sidebar-menu > li > a {
          color: ", colores_pastel$texto, " !important;
          border-bottom: 1px solid #E9ECEF;
        }
        .sidebar-menu > li > a:hover {
          background-color: ", colores_pastel$primario, " !important;
          color: white !important;
        }
        .content-wrapper {
          background-color: ", colores_pastel$fondo, " !important;
        }
        .box {
          border-top: 3px solid ", colores_pastel$primario, " !important;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1) !important;
        }
        .btn-primary {
          background-color: ", colores_pastel$acento1, " !important;
          border-color: ", colores_pastel$acento1, " !important;
          color: ", colores_pastel$texto, " !important;
        }
        .btn-primary:hover {
          background-color: ", colores_pastel$acento3, " !important;
          border-color: ", colores_pastel$acento3, " !important;
        }
        .selectize-input {
          border: 2px solid ", colores_pastel$primario, " !important;
          border-radius: 8px !important;
        }
        .info-box {
          background: linear-gradient(45deg, ", colores_pastel$secundario, ", ", colores_pastel$primario, ") !important;
          color: ", colores_pastel$texto, " !important;
          border-radius: 12px !important;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1) !important;
        }
        .info-box-icon {
          background-color: ", colores_pastel$acento2, " !important;
          color: ", colores_pastel$texto, " !important;
        }
      ")))
    ),
    
    sidebarMenu(
      id = "tabs",
      
      # Panel de control principal
      menuItem("📊 Visualización", tabName = "visualizacion", icon = icon("chart-line")),
      
      # Divider visual
      tags$hr(style = "border-color: #E9ECEF; margin: 20px 0;"),
      
      # Título de controles
      tags$div(
        style = "padding: 15px; text-align: center;",
        tags$h4("Controles de Visualización", 
               style = paste0("color: ", colores_pastel$texto, "; font-weight: bold; margin-bottom: 20px;"))
      ),
      
      # Selector de entidad federativa
      tags$div(
        style = "padding: 0 15px;",
        pickerInput(
          inputId = "entidad_seleccionada",
          label = tags$div(
            style = paste0("color: ", colores_pastel$texto, "; font-weight: bold;"),
            "🗺️ Entidad Federativa"
          ),
          choices = setNames(catalogo_entidades$cve_ent, catalogo_entidades$entidad),
          selected = 7, # Chiapas por defecto
          options = pickerOptions(
            style = "btn-outline-primary",
            title = "Selecciona una entidad...",
            liveSearch = TRUE,
            size = 10
          ),
          width = "100%"
        )
      ),
      
      # Selector de variable de pobreza
      tags$div(
        style = "padding: 0 15px; margin-top: 15px;",
        pickerInput(
          inputId = "variable_seleccionada",
          label = tags$div(
            style = paste0("color: ", colores_pastel$texto, "; font-weight: bold;"),
            "📈 Indicador de Pobreza"
          ),
          choices = diccionario_variables,
          selected = "pobreza",
          options = pickerOptions(
            style = "btn-outline-success",
            title = "Selecciona un indicador...",
            liveSearch = TRUE,
            size = 8
          ),
          width = "100%"
        )
      ),
      
      # Opciones de personalización
      tags$div(
        style = "padding: 0 15px; margin-top: 20px;",
        tags$h5("🎨 Personalización", 
               style = paste0("color: ", colores_pastel$texto, "; font-weight: bold;")),
        
        # Selector de color
        colorPickr(
          inputId = "color_linea",
          label = "Color de la línea:",
          selected = colores_pastel$acento1,
          theme = "nano",
          update = "save",
          width = "100%"
        ),
        
        # Switch para mostrar puntos
        materialSwitch(
          inputId = "mostrar_puntos",
          label = "Mostrar puntos en la gráfica",
          value = TRUE,
          status = "primary",
          right = FALSE
        ),
        
        # Switch para tema oscuro
        materialSwitch(
          inputId = "tema_oscuro",
          label = "Tema oscuro",
          value = FALSE,
          status = "warning",
          right = FALSE
        )
      ),
      
      # Botón de actualizar
      tags$div(
        style = "padding: 20px 15px;",
        actionButton(
          inputId = "actualizar",
          label = "🔄 Actualizar Gráfica",
          class = "btn-primary",
          width = "100%",
          style = "border-radius: 25px; font-weight: bold; padding: 12px;"
        )
      ),
      
      # Información adicional
      tags$div(
        style = "padding: 15px; margin-top: 20px; background-color: #F8F9FA; border-radius: 10px; margin: 15px;",
        tags$h6("ℹ️ Información", style = paste0("color: ", colores_pastel$texto, "; font-weight: bold;")),
        tags$p("Datos de CONEVAL 2016-2024", style = paste0("color: ", colores_pastel$texto, "; font-size: 12px; margin: 5px 0;")),
        tags$p("Mediciones bienales de pobreza", style = paste0("color: ", colores_pastel$texto, "; font-size: 12px; margin: 5px 0;"))
      )
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "visualizacion",
        
        # Fila de información general
        fluidRow(
          # Información de la entidad seleccionada
          valueBoxOutput("info_entidad", width = 4),
          valueBoxOutput("info_variable", width = 4),
          valueBoxOutput("info_periodo", width = 4)
        ),
        
        # Fila principal con la gráfica
        fluidRow(
          box(
            title = tags$div(
              style = "display: flex; align-items: center;",
              tags$i(class = "fas fa-chart-area", style = "margin-right: 10px;"),
              "Serie de Tiempo de Pobreza"
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            
            # Contenedor de la gráfica
            div(
              style = "position: relative; height: 500px;",
              
              # Loading spinner
              conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                div(
                  style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000;",
                  tags$div(
                    class = "spinner-border text-primary",
                    role = "status",
                    style = "width: 3rem; height: 3rem;",
                    tags$span(class = "sr-only", "Cargando...")
                  )
                )
              ),
              
              # Gráfica principal
              plotlyOutput("grafica_principal", height = "500px")
            )
          )
        ),
        
        # Fila de estadísticas
        fluidRow(
          box(
            title = tags$div(
              style = "display: flex; align-items: center;",
              tags$i(class = "fas fa-calculator", style = "margin-right: 10px;"),
              "Estadísticas Resumidas"
            ),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            tableOutput("tabla_estadisticas")
          ),
          
          box(
            title = tags$div(
              style = "display: flex; align-items: center;",
              tags$i(class = "fas fa-download", style = "margin-right: 10px;"),
              "Descargar Datos"
            ),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            tags$p("Descarga la gráfica en diferentes formatos:"),
            
            div(
              style = "text-align: center; margin-top: 20px;",
              downloadButton(
                outputId = "descargar_png",
                label = "📥 Descargar PNG",
                class = "btn-primary",
                style = "margin: 5px; border-radius: 20px;"
              ),
              br(),
              downloadButton(
                outputId = "descargar_pdf",
                label = "📄 Descargar PDF", 
                class = "btn-secondary",
                style = "margin: 5px; border-radius: 20px;"
              )
            )
          )
        )
      )
    )
  )
)

# ================================================================================
# LÓGICA DEL SERVIDOR (SERVER)
# ================================================================================

server <- function(input, output, session) {
  
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
  
  # Datos reactivos
  datos_filtrados <- reactive({
    req(input$entidad_seleccionada, input$variable_seleccionada)
    
    # Obtener el nombre real de la columna
    nombre_columna <- obtener_nombre_columna(input$variable_seleccionada)
    
    # Leer datos
    datos <- read_csv("df_pob_ent.csv", show_col_types = FALSE)
    
    # Filtrar para la entidad seleccionada
    datos %>%
      filter(cve_ent == as.numeric(input$entidad_seleccionada)) %>%
      select(anio, all_of(nombre_columna)) %>%
      arrange(anio)
  })
  
  # Gráfica reactiva
  grafica_reactiva <- reactive({
    req(input$actualizar)
    
    isolate({
      tryCatch({
        # Obtener el nombre real de la columna
        nombre_columna <- obtener_nombre_columna(input$variable_seleccionada)
        
        generar_grafica_pobreza(
          archivo_csv = "df_pob_ent.csv",
          variable_pobreza = nombre_columna,
          clave_entidad = as.numeric(input$entidad_seleccionada),
          color_linea = input$color_linea,
          mostrar_puntos = input$mostrar_puntos,
          tema_oscuro = input$tema_oscuro
        )
      }, error = function(e) {
        showNotification(
          paste("Error al generar la gráfica:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
    })
  })
  
  # Value boxes informativos
  output$info_entidad <- renderValueBox({
    entidad_nombre <- catalogo_entidades %>%
      filter(cve_ent == as.numeric(input$entidad_seleccionada)) %>%
      pull(entidad)
    
    valueBox(
      value = entidad_nombre,
      subtitle = "Entidad Seleccionada",
      icon = icon("map-marker-alt"),
      color = "light-blue"
    )
  })
  
  output$info_variable <- renderValueBox({
    variable_nombre <- input$variable_seleccionada
    
    valueBox(
      value = variable_nombre,
      subtitle = "Indicador de Pobreza",
      icon = icon("chart-bar"),
      color = "green"
    )
  })
  
  output$info_periodo <- renderValueBox({
    valueBox(
      value = "2016-2024",
      subtitle = "Período de Análisis",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  # Gráfica principal
  output$grafica_principal <- renderPlotly({
    grafica <- grafica_reactiva()
    
    if (!is.null(grafica)) {
      # Convertir a plotly para interactividad
      ggplotly(grafica, tooltip = c("x", "y")) %>%
        layout(
          showlegend = FALSE,
          hovermode = "x unified"
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                                   "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"),
          displaylogo = FALSE
        )
    } else {
      # Gráfica de placeholder
      plot_ly() %>%
        add_annotations(
          text = "Selecciona los parámetros y presiona 'Actualizar Gráfica'",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16, color = colores_pastel$texto)
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          plot_bgcolor = colores_pastel$fondo,
          paper_bgcolor = colores_pastel$fondo
        )
    }
  })
  
  # Tabla de estadísticas
  output$tabla_estadisticas <- renderTable({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    variable_col <- names(datos)[2]
    valores <- datos[[variable_col]]
    
    estadisticas <- data.frame(
      Estadística = c("Valor Mínimo", "Valor Máximo", "Promedio", "Mediana", "Cambio Total"),
      Valor = c(
        paste0(round(min(valores, na.rm = TRUE), 2), "%"),
        paste0(round(max(valores, na.rm = TRUE), 2), "%"),
        paste0(round(mean(valores, na.rm = TRUE), 2), "%"),
        paste0(round(median(valores, na.rm = TRUE), 2), "%"),
        paste0(round(last(valores) - first(valores), 2), " p.p.")
      )
    )
    
    estadisticas
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Descargas
  output$descargar_png <- downloadHandler(
    filename = function() {
      entidad <- catalogo_entidades %>%
        filter(cve_ent == as.numeric(input$entidad_seleccionada)) %>%
        pull(entidad)
      nombre_columna <- obtener_nombre_columna(input$variable_seleccionada)
      paste0("pobreza_", gsub(" ", "_", entidad), "_", nombre_columna, ".png")
    },
    content = function(file) {
      grafica <- grafica_reactiva()
      if (!is.null(grafica)) {
        ggsave(file, grafica, width = 12, height = 8, dpi = 300, bg = "white")
      }
    }
  )
  
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      entidad <- catalogo_entidades %>%
        filter(cve_ent == as.numeric(input$entidad_seleccionada)) %>%
        pull(entidad)
      nombre_columna <- obtener_nombre_columna(input$variable_seleccionada)
      paste0("pobreza_", gsub(" ", "_", entidad), "_", nombre_columna, ".pdf")
    },
    content = function(file) {
      grafica <- grafica_reactiva()
      if (!is.null(grafica)) {
        ggsave(file, grafica, width = 12, height = 8, device = "pdf")
      }
    }
  )
  
  # Actualización automática al inicio
  observe({
    updateActionButton(session, "actualizar", label = "🔄 Actualizar Gráfica")
  })
}

# ================================================================================
# EJECUTAR LA APLICACIÓN
# ================================================================================

shinyApp(ui = ui, server = server)