Sys.setlocale("LC_TIME", "es_ES")

library(shiny)
library(shinycssloaders)
library(shinydashboard)
source("stock_analysis_functions.R")


header <- dashboardHeader(title = "Análisis de acciones", titleWidth = "250px", 
                          
                          
                          
                          tags$li(a(href = 'https://www.datacrunchers.mx/',
                                    img(src = 'https://lwfiles.mycourse.app/segasi-public/dc1587a77e3ef8abe87cde57cd0c01ee.png',
                                        title = "inai", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"), 
                          
                          tags$li(a(href = 'https://mexicocomovamos.mx',
                                    img(src = 'https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg',
                                        title = "inai", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown")
                          
                          )

# header$children[[2]]$children <-  tags$a(href='https://www.datacrunchers.mx/',
#                                            tags$img(src='https://lwfiles.mycourse.app/segasi-public/dc1587a77e3ef8abe87cde57cd0c01ee.png',height='60',width='200'))


barraLateral <- dashboardSidebar(width = "250px", 
                                 
                                 sidebarMenu(
                                   menuItem("Portada", tabName = "portada", icon = icon("star")),
                                   menuItem("Portafolio (BETA)", tabName = "portafolio", icon = icon("book")),
                                   menuItem("Análisis", tabName = "analisis", icon = icon("line-chart")), 
                                   tagList(
                                     selectInput(inputId = "selAccion", label = "Seleccione acción",
                                                 choices = get_stock_list()),
                                     dateRangeInput(inputId = "dtInput",
                                                    label = "Seleccione Fechas",
                                                    start  = today() - days(365*2),
                                                    end    = today(),
                                                    min    = today() - days(365*10),
                                                    max    = today(),
                                                    format = "dd/mm/yyyy",
                                                    language = "es",
                                                    separator = " a "), 
                                     br()
                                   )
                                            
                                   )
                                   )

body <- dashboardBody(
  tags$head(
    includeCSS("styles.css")
  ),
  
  tabItems(
    
  tabItem(tabName = "portada", 
          HTML('<div class="image-header">
        <h1>Aplicación de consulta de acciones</h1>
    </div>

    <div class="content">
        <p>Bienvenido a nuestra innovadora aplicación de consulta de acciones, su compañera esencial en el mundo financiero. Esta herramienta está diseñada para proporcionarle información actualizada y detallada sobre el comportamiento de las acciones en tiempo real.</p>

        <p>Con nuestra aplicación, podrá analizar tendencias, comparar históricos y recibir alertas personalizadas que le ayudarán a tomar decisiones informadas. Ya sea que sea un inversor experimentado o esté dando sus primeros pasos en el mercado bursátil, nuestra plataforma está aquí para facilitar su camino hacia el éxito financiero.</p>
    </div>
'),
          ), 
  
  tabItem(tabName = "portafolio",
          h1("Análisis de portafolio"), 
          fluidPage(
            titlePanel("Evaluador de Desempeño de Portafolio"),
            sidebarLayout(
              sidebarPanel(
                radioButtons("allocation_type", "Tipo de Asignación:",
                             choices = c("Número de Acciones" = "shares",
                                         "Proporción" = "proportion"),
                             selected = "shares"),
                numericInput("num_stocks", "Cantidad de Acciones en el Portafolio:", value = 1, min = 1),
                uiOutput("stock_inputs"),
                dateInput("start_date", "Fecha de Inicio:", value = Sys.Date() - 365),
                dateInput("end_date", "Fecha Final:", value = Sys.Date()),
                actionButton("analyze", "Ejecutar Análisis")
              ),
              mainPanel(
                plotOutput("performancePlot"),
                tableOutput("summaryTable")
              )
            )
          )
  ),
  
  
  tabItem(tabName = "analisis",
    h1("Análisis de acciones"), 
    p("En esta aplicación, podrás consultar y descargar la información disponible para analizar los datos del precio de las acciones."),
    p("Utiliza las siguientes instrucciones para aprovechar al máximo las funcionalidades:"),
    tags$ul(
      tags$li("En esta aplicación, podrás consultar y descargar la información disponible para analizar los datos del precio de las acciones. Utiliza las siguientes instrucciones para aprovechar al máximo las funcionalidades:"), 
      tags$li("Información detallada: Haz clic en la acción deseada para acceder a su información detallada, incluyendo su historial de precios, datos financieros y noticias relevantes."), 
      tags$li("Descarga de datos: Si deseas analizar los datos en una plataforma externa, selecciona la opción de descarga. Podrás obtener archivos en formatos compatibles con software de análisis como CSV o Excel.")
    ), 
    fluidRow(
      column(6, 
             # h2("Gráfica de líneas", style = "text-align:center;"), 
             box(width = "100%",
               title = "Gráfica de líneas", 
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               plotlyOutput("grafica_lineas", height = "50vh") %>% withSpinner(), 
               downloadButton(outputId = "descarga_datos", label = "Descarga los datos")
             )
             
      ),
      column(6, 
             box(width = "100%",
                 title = "Gráfica de velas", 
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotlyOutput("grafica_velas", height = "50vh") %>% withSpinner(), 
                 downloadButton(outputId = "descarga_datos_velas", label = "Descarga los datos")
             )
             
      )
    ), 
    br(),
    verbatimTextOutput("comentario"), 
    br(), br()
  )
  
  
))

ui <- dashboardPage(header = header, sidebar = barraLateral, body = body, skin = "black")


server <- function(input, output, session) {
  
  datos_stock_intermedio <- reactive({
    get_stock_data(stock_symbol = get_symbol_from_user_input(input$selAccion),
                   from = input$dtInput[1], to = input$dtInput[2])
  })
  
  output$grafica_lineas <- renderPlotly({
    plot_stock_data(data = datos_stock_intermedio())
  })
  
  output$comentario <- renderText({
    generate_commentary(data = datos_stock_intermedio())
  })
  
  output$descarga_datos <- downloadHandler(filename = function(){
    paste0("series_", 
           get_symbol_from_user_input(input$selAccion), "_",
           input$dtInput[1], "_",
           input$dtInput[2], 
           ".xlsx"
           )
  }, content = function(file){
    datos_stock_intermedio() %>% 
      openxlsx::write.xlsx(file) 
  })
  
  
  datos_velas_intermedio <- reactive({
    get_stock_candles(stock_symbol = get_symbol_from_user_input(input$selAccion),
                      from = input$dtInput[1],
                      to = input$dtInput[2])
  })

  output$grafica_velas <- renderPlotly({
    gen_candle_plot(data = datos_velas_intermedio())
  })
  
  output$descarga_datos_velas <- downloadHandler(filename = function(){
    paste0("velas_", 
           get_symbol_from_user_input(input$selAccion), "_",
           input$dtInput[1], "_",
           input$dtInput[2], 
           ".xlsx"
    )
  }, content = function(file){
    datos_velas_intermedio() %>% 
      openxlsx::write.xlsx(file) 
  })
  
  
  # Portafolio 
  
  # Generación dinámica de campos de entrada para tickers y asignaciones
  output$stock_inputs <- renderUI({
    num_stocks <- input$num_stocks
    stock_list <- get_stock_list()
    stock_inputs <- lapply(1:num_stocks, function(i) {
      list(
        selectInput(paste0("ticker", i),
                   paste0("Ticker ", i),
                   choices = c("Seleccione una acción" = "", stock_list),
                   selected = ifelse(i == 1, stock_list[1], "")),
        numericInput(paste0("alloc", i), paste0("Asignación ", i), value = ifelse(i == 1, 100, 0), min = 0),
        hr()
      )
    })
    do.call(tagList, stock_inputs)
  })
  
  # Recolección de tickers y asignaciones ingresadas
  portfolio_data <- eventReactive(input$analyze, {
    num_stocks <- input$num_stocks
    tickers_raw <- sapply(1:num_stocks, function(i) input[[paste0("ticker", i)]])
    allocations <- sapply(1:num_stocks, function(i) input[[paste0("alloc", i)]])

    # Filtrar tickers vacíos
    tickers_raw <- tickers_raw[tickers_raw != ""]
    allocations <- allocations[1:length(tickers_raw)]

    if(length(tickers_raw) == 0) {
      return(NULL)
    }

    # Extraer solo los símbolos de ticker (ej: "AAPL, APPLE INC" -> "AAPL")
    # Usar as.character y unname para asegurar que sea un vector sin nombres
    tickers <- as.character(sapply(tickers_raw, get_symbol_from_user_input))
    tickers <- unname(tickers)

    # Obtención de precios de las acciones
    stock_prices <- tq_get(tickers,
                           from = input$start_date,
                           to = input$end_date,
                           get = "stock.prices")
    
    if(input$allocation_type == "shares") {
      # Cálculo del valor de las tenencias en el tiempo
      stock_values <- stock_prices %>%
        group_by(symbol) %>%
        mutate(value = close * allocations[match(symbol, tickers)]) %>%
        select(date, symbol, value)
      
      # Suma de valores para obtener el valor total del portafolio
      portfolio_values <- stock_values %>%
        group_by(date) %>%
        summarise(portfolio_value = sum(value, na.rm = TRUE))
      
      return(list(portfolio_values = portfolio_values))
      
    } else {
      # Cálculo de retornos y valor acumulado para proporciones
      weights <- allocations / sum(allocations)
      stock_returns <- stock_prices %>%
        group_by(symbol) %>%
        tq_transmute(adjusted, periodReturn, period = "daily", type = "log") %>%
        ungroup()
      
      weighted_returns <- stock_returns %>%
        mutate(weight = weights[match(symbol, tickers)],
               weighted_return = daily.returns * weight)
      
      portfolio_returns <- weighted_returns %>%
        group_by(date) %>%
        summarise(portfolio_return = sum(weighted_return, na.rm = TRUE)) %>%
        mutate(cumulative_return = cumsum(portfolio_return),
               portfolio_value = exp(cumulative_return))
      
      return(list(portfolio_returns = portfolio_returns))
    }
  })
  
  # Generación del gráfico de desempeño
  output$performancePlot <- renderPlot({
    req(portfolio_data())
    if(input$allocation_type == "shares") {
      portfolio_values <- portfolio_data()$portfolio_values

      # Determinar color según tendencia
      initial_value <- first(portfolio_values$portfolio_value)
      final_value <- last(portfolio_values$portfolio_value)
      line_color <- ifelse(final_value >= initial_value, "#10b981", "#ef4444")

      ggplot(portfolio_values, aes(x = date, y = portfolio_value)) +
        geom_line(color = line_color, size = 1.5) +
        geom_point(data = portfolio_values[c(1, nrow(portfolio_values)), ],
                   aes(x = date, y = portfolio_value),
                   color = line_color, size = 3, shape = 21, fill = "white", stroke = 2) +
        labs(title = "Valor del Portafolio en el Tiempo",
             subtitle = paste("Retorno:", scales::percent((final_value/initial_value) - 1)),
             x = NULL,
             y = "Valor del Portafolio ($)") +
        scale_y_continuous(labels = scales::dollar_format()) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 18, color = "#1e3a8a", hjust = 0),
          plot.subtitle = element_text(size = 14, color = "#6b7280", hjust = 0),
          axis.title.y = element_text(face = "bold", color = "#1f2937", size = 12),
          axis.text = element_text(color = "#4b5563"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#e5e7eb", linetype = "dashed"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20)
        )
    } else {
      portfolio_returns <- portfolio_data()$portfolio_returns

      # Determinar color según tendencia
      initial_value <- 1
      final_value <- last(portfolio_returns$portfolio_value)
      line_color <- ifelse(final_value >= initial_value, "#10b981", "#ef4444")

      ggplot(portfolio_returns, aes(x = date, y = portfolio_value)) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "#9ca3af", size = 0.8) +
        geom_line(color = line_color, size = 1.5) +
        geom_point(data = portfolio_returns[c(1, nrow(portfolio_returns)), ],
                   aes(x = date, y = portfolio_value),
                   color = line_color, size = 3, shape = 21, fill = "white", stroke = 2) +
        labs(title = "Desempeño del Portafolio (Base 100)",
             subtitle = paste("Retorno acumulado:", scales::percent(final_value - 1)),
             x = NULL,
             y = "Valor Relativo") +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 18, color = "#1e3a8a", hjust = 0),
          plot.subtitle = element_text(size = 14, color = "#6b7280", hjust = 0),
          axis.title.y = element_text(face = "bold", color = "#1f2937", size = 12),
          axis.text = element_text(color = "#4b5563"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#e5e7eb", linetype = "dashed"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20)
        )
    }
  })
  
  # Generación de la tabla de resumen
  output$summaryTable <- renderTable({
    req(portfolio_data())
    if(input$allocation_type == "shares") {
      portfolio_values <- portfolio_data()$portfolio_values
      total_return <- (last(portfolio_values$portfolio_value) / first(portfolio_values$portfolio_value)) - 1
      data.frame(
        Valor_Inicial = first(portfolio_values$portfolio_value),
        Valor_Final = last(portfolio_values$portfolio_value),
        Retorno_Total = scales::percent(total_return)
      )
    } else {
      portfolio_returns <- portfolio_data()$portfolio_returns
      total_return <- last(portfolio_returns$portfolio_value) - 1
      data.frame(
        Valor_Inicial = 1,
        Valor_Final = last(portfolio_returns$portfolio_value),
        Retorno_Total = scales::percent(total_return)
      )
    }
  })
  
  
  
}

shinyApp(ui, server)

