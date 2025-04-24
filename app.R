# app.R
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
  titlePanel("Oil Price Forecasting with Parameter Adjustment"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("changepoints", 
                  "Number of Change Points:", 
                  min = 1, max = 50, value = 25, step = 1),
      selectInput("growth", "Growth Type:", 
                  choices = c("linear", "logistic"), selected = "linear"),
      checkboxInput("weekly_seasonality", "Enable Weekly Seasonality", value = FALSE),
      sliderInput("forecast_horizon", 
                  "Forecast Horizon (Months):", 
                  min = 1, max = 24, value = 12),
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
  brent_prices <- tq_get("BZ=F")
  
  # Reactive splits for train/test
  splits <- reactive({
    brent_prices %>%
      time_series_split(assess = "12 months", cumulative = TRUE)
  })
  
  # Reactive Prophet model
  prophet_model <- reactive({
    req(input$update_model)
    
    prophet_reg(
      mode = "regression",
      growth = input$growth,
      season = "additive"
    ) %>%
      set_engine("prophet", 
                 num.changepoints = input$changepoints,
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
        actual_data = brent_prices,
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
