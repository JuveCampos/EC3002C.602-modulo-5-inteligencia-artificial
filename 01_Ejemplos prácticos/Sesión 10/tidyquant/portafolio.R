# Carga de librerías necesarias
library(shiny)
library(tidyquant)
library(tidyverse)

# Definición de la interfaz de usuario
ui <- fluidPage(
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

# Definición del servidor
server <- function(input, output, session) {
    
    # Generación dinámica de campos de entrada para tickers y asignaciones
    output$stock_inputs <- renderUI({
        num_stocks <- input$num_stocks
        stock_inputs <- lapply(1:num_stocks, function(i) {
            list(
                textInput(paste0("ticker", i), paste0("Ticker ", i), value = ifelse(i == 1, "AAPL", "")),
                numericInput(paste0("alloc", i), paste0("Asignación ", i), value = ifelse(i == 1, 100, 0), min = 0), 
                hr()
            )
        })
        do.call(tagList, stock_inputs)
    })
    
    # Recolección de tickers y asignaciones ingresadas
    portfolio_data <- eventReactive(input$analyze, {
        num_stocks <- input$num_stocks
        tickers <- sapply(1:num_stocks, function(i) input[[paste0("ticker", i)]])
        allocations <- sapply(1:num_stocks, function(i) input[[paste0("alloc", i)]])
        
        tickers <- tickers[tickers != ""]
        allocations <- allocations[1:length(tickers)]
        
        if(length(tickers) == 0) {
            return(NULL)
        }
        
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
            ggplot(portfolio_values, aes(x = date, y = portfolio_value)) +
                geom_line(color = "blue") +
                labs(title = "Valor del Portafolio en el Tiempo",
                     x = "Fecha",
                     y = "Valor del Portafolio") +
                theme_minimal()
        } else {
            portfolio_returns <- portfolio_data()$portfolio_returns
            ggplot(portfolio_returns, aes(x = date, y = portfolio_value)) +
                geom_line(color = "blue") +
                labs(title = "Valor del Portafolio en el Tiempo",
                     x = "Fecha",
                     y = "Valor del Portafolio (Iniciando en 1)") +
                theme_minimal()
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

# Ejecución de la aplicación Shiny
shinyApp(ui = ui, server = server)

