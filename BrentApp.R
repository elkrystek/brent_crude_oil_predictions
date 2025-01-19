library(shiny)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(timetk)
library(tidymodels)
library(modeltime)
library(prophet)
library(parsnip)
library(plotly)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Oil Price Forecasting with Parameter Adjustment"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", 
                     "Select Date Range:",
                     start = min(brent_prices$date), 
                     end = max(brent_prices$date), 
                     min = min(brent_prices$date), 
                     max = max(brent_prices$date)),
      
      selectInput("growth", "Growth Type:", 
                  choices = c("linear", "logistic"), selected = "linear"),
      
      selectInput("seasonality", "Seasonality Type:", 
                  choices = c("additive", "multiplicative"), selected = "additive"),
      
      checkboxInput("weekly_seasonality", "Enable Weekly Seasonality", value = FALSE),
      
      sliderInput("forecast_horizon", 
                  "Forecast Horizon (Months):", 
                  min = 1, max = 24, value = 12),
      
      # Parameters for manual ARIMA model
      numericInput("p", "ARIMA p (auto regressive):", value = 1, min = 0),
      numericInput("d", "ARIMA d (differencing):", value = 1, min = 0),
      numericInput("q", "ARIMA q (moving average):", value = 1, min = 0),
      
      actionButton("update_model", "Update Model")
    ),
    
    mainPanel(
      plotlyOutput("forecast_plot"),
      tableOutput("accuracy_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Load Data
  brent_prices <- tq_get("BZ=F") %>% 
    mutate(date = as.Date(date))  # Make sure date is in Date format
  
  # Reactive splits for train/test
  splits <- reactive({
    req(input$date_range)
    
    brent_prices %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      time_series_split(assess = "12 months", cumulative = TRUE)
  })
  
  # Reactive Prophet model
  prophet_model <- reactive({
    req(input$update_model)
    
    prophet_reg(
      mode = "regression",
      growth = input$growth,
      season = input$seasonality
    ) %>%
      set_engine("prophet", 
                 weekly.seasonality = input$weekly_seasonality) %>%
      fit(close ~ date, training(splits()))
  })
  
  # Reactive ARIMA auto model (parsnip)
  arima_auto_model <- reactive({
    arima_reg(mode = "regression") %>%
      set_engine("auto_arima") %>%
      fit(close ~ date, training(splits()))
  })
  
  # Reactive manual ARIMA model (parsnip)
  arima_manual_model <- reactive({
    req(input$p, input$d, input$q)
    
    arima_reg(mode = "regression") %>%
      set_engine("arima", p = input$p, d = input$d, q = input$q) %>%
      fit(close ~ date, training(splits()))
  })
  
  # Calibration and forecast
  calibrated_models <- reactive({
    modeltime_table(
      prophet_model(),
      arima_auto_model(),
      arima_manual_model()
    ) %>%
      modeltime_calibrate(testing(splits()))
  })
  
  # Render forecast plot
  output$forecast_plot <- renderPlotly({
    req(calibrated_models())
    
    # Get forecast data based on testing dataset, not future predictions
    forecast_data_test <- calibrated_models() %>%
      modeltime_forecast(
        actual_data = brent_prices,  # actual data for testing period
        new_data = testing(splits())  # use testing data for predictions
      )
    
    # Forecast for 3 months into the future
    forecast_data_future <- calibrated_models() %>%
      modeltime_forecast(
        actual_data = brent_prices,  # actual data for forecasting
        h = "3 months"  # forecast horizon (3 months)
      )
    
    # Combine forecast data for test and future predictions
    combined_forecast_data <- bind_rows(forecast_data_test, forecast_data_future)
    
    # Generate interactive plot using plot_modeltime_forecast
    combined_forecast_data %>%
      plot_modeltime_forecast(.interactive = TRUE)  # Make the plot interactive
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
