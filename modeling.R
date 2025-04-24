# R/modeling.R

library(tidyquant)
library(timetk)
library(tidymodels)
library(modeltime)
library(prophet)
library(forecast)

# Load data
load_data <- function() {
  tq_get("BZ=F")
}

# Split data into training and test sets
split_data <- function(data, test_size) {
  data %>%
    time_series_split(assess = paste(test_size, "months"), cumulative = TRUE)
}

# Prophet model
prophet_model <- function(input, splits) {
  prophet_reg(
    mode = "regression",
    growth = input$growth,
    season = input$seasonality_type
  ) %>%
    set_engine("prophet", weekly.seasonality = input$weekly_seasonality) %>%
    fit(close ~ date, training(splits))
}

# ARIMA model
arima_model <- function(input, splits) {
  arima_reg(mode = "regression") %>%
    set_engine("auto_arima", 
               p = input$p, d = input$d, q = input$q,
               P = input$P, D = input$D, Q = input$Q) %>%
    fit(close ~ date, training(splits))
}

# Calibrate and forecast
calibrate_and_forecast <- function(prophet, arima, splits, brent_prices, forecast_horizon) {
  calibrated_models <- modeltime_table(
    prophet,
    arima
  ) %>%
    modeltime_calibrate(testing(splits))
  
  forecast <- calibrated_models %>%
    modeltime_forecast(
      actual_data = brent_prices,
      h = paste(forecast_horizon, "months")
    )
  
  list(
    forecast = forecast,
    accuracy = calibrated_models %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(.interactive = FALSE)
  )
}
