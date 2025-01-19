library(shiny)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(timetk)
library(tidymodels)
library(modeltime)
library(prophet)

ui <- fluidPage(
  useShinyjs(),
  # Include external CSS
  includeCSS("www/styles.css"),
  
  titlePanel("Oil Price Forecasting with Parameter Adjustment"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("growth", "Growth Type:", 
                  choices = c("linear", "logistic"), selected = "linear"),
      checkboxInput("weekly_seasonality", "Enable Weekly Seasonality", value = FALSE),
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
  
  # Load Data
  brent_prices <- tq_get("BZ=F")
  
  # Filter data based on selected date range
  brent_prices_filtered <- reactive({
    brent_prices %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  # Reactive splits for train/test
  splits <- reactive({
    brent_prices_filtered() %>%
      time_series_split(assess = paste(input$test_size, "months"), cumulative = TRUE)
  })
  
  # Reactive Prophet model (without change points)
  prophet_model <- reactive({
    req(input$update_model)
    
    prophet_reg(
      mode = "regression",
      growth = input$growth,
      season = "additive"
    ) %>%
      set_engine("prophet", 
                 weekly.seasonality = input$weekly_seasonality) %>%
      fit(close ~ date, training(splits()))
  })
  
  # Reactive ARIMA model
  arima_model <- reactive({
    arima_reg(mode = "regression") %>%
      set_engine("auto_arima") %>%
      fit(close ~ date, training(splits()))
  })
  
  # Calibration and forecast
  calibrated_models <- reactive({
    modeltime_table(
      prophet_model(),
      arima_model()
    ) %>%
      modeltime_calibrate(testing(splits()))
  })
  
  # Render forecast plot
  output$forecast_plot <- renderPlotly({
    req(calibrated_models())
    calibrated_models() %>%
      modeltime_forecast(
        actual_data = brent_prices_filtered(),
        h = paste(input$forecast_horizon, "months")
      ) %>%
      plot_modeltime_forecast(.interactive = TRUE)
  })
  
  # Render accuracy table
  output$accuracy_table <- renderTable({
    req(calibrated_models())
    calibrated_models() %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(.interactive = FALSE)
  })
}

shinyApp(ui, server)
