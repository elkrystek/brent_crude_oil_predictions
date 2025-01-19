library(shiny)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(timetk)
library(tidymodels)
library(modeltime)
library(prophet)

# Załaduj plik z kodem modelowania
source("modeling.R")

ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/styles.css"),
  
  titlePanel("Oil Price Forecasting with Parameter Adjustment"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("growth", "Growth Type:", 
                  choices = c("linear", "logistic"), selected = "linear"),
      checkboxInput("weekly_seasonality", "Enable Weekly Seasonality", value = FALSE),
      selectInput("seasonality_type", "Seasonality Type:", 
                  choices = c("additive", "multiplicative"), selected = "additive"),
      sliderInput("test_size", 
                  "Test Set Size (Months):", 
                  min = 1, max = 24, value = 12, step = 1),
      sliderInput("forecast_horizon", 
                  "Forecast Horizon (Months):", 
                  min = 1, max = 24, value = 12),
      dateRangeInput("date_range", 
                     "Select Date Range:", 
                     start = min(brent_prices$date), 
                     end = max(brent_prices$date)),
      actionButton("update_model", "Update Model", class = "btn-primary")
    ),
    
    mainPanel(
      plotlyOutput("forecast_plot"),
      tableOutput("accuracy_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Załaduj dane
  brent_prices <- load_data()
  
  # Filtruj dane na podstawie wybranego zakresu dat
  brent_prices_filtered <- reactive({
    brent_prices %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  # Reactive splits dla danych treningowych i testowych
  splits <- reactive({
    split_data(brent_prices_filtered(), input$test_size)
  })
  
  # Reactive modele (Prophet i ARIMA)
  prophet <- reactive({
    req(input$update_model)
    prophet_model(input, splits())
  })
  
  arima <- reactive({
    arima_model(splits())
  })
  
  # Kalibracja i prognozowanie
  calibrated_results <- reactive({
    req(prophet(), arima())
    calibrate_and_forecast(prophet(), arima(), splits(), brent_prices_filtered(), input$forecast_horizon)
  })
  
  # Renderowanie wykresu prognozy
  output$forecast_plot <- renderPlotly({
    req(calibrated_results())
    plot_modeltime_forecast(calibrated_results()$forecast, .interactive = TRUE)
  })
  
  # Renderowanie tabeli dokładności
  output$accuracy_table <- renderTable({
    req(calibrated_results())
    calibrated_results()$accuracy
  })
}

shinyApp(ui, server)
