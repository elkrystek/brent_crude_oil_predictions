#API
library(Quandl)
#TimeSeries
library(lubridate)
library(timetk)
#visualization
library(plotly)
library(ggcorrplot)

library(tidyverse)
library(tidyquant)

library(data.table)

library(tidymodels)
library(modeltime)
library(prophet)

#pobranie danych finansowych dla Brent z Yahoo
brent_prices <- tq_get("BZ=F")
brent_prices
brent_prices %>% plot_time_series(date, open, .smooth = T,
                                  .smooth_size = 0.5,
                                  .smooth_alpha = 0.7,
                                  .smooth_degree = 2,
                                  .smooth_period = "1 year",
                                .interactive = T,
                                .title = "Brent Oil price (USD)")
#podział na zbior treningowy i testowy
splits <- brent_prices %>% time_series_split(assess = "12 months", cumulative = TRUE)
splits %>% 
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date,open)
#modelowanie przy uzyciy Prophet
model_prophet <-prophet_reg(mode = "regression",
                            growth="linear",
                            # num_changepoints = 25,
                            season = "additive") %>%
  set_engine("prophet", weekly.seasonality = FALSE) %>%
  fit(close ~ date, training(splits))
model_prophet 

#modelowanie przy uzyciy Arima
model_arima <-arima_reg(mode = "regression",
                            seasonal_period = "1 weeks") %>%
  set_engine("auto_arima") %>%
  fit(close ~ date, training(splits))
model_arima
#kalibracja
models_table <- modeltime_table(model_prophet,
                                model_arima)
models_table

calibration_table <-models_table %>%
  modeltime_calibrate(testing(splits))
calibration_table
#forecast
calibration_table %>%
  modeltime_forecast(actual_data = brent_prices) %>%
  plot_modeltime_forecast(.interactive = T)
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)
#refit
calibration_table %>%
  modeltime_refit(brent_prices) %>%
  modeltime_forecast(h="12 months", actual_data = brent_prices) %>%
  plot_modeltime_forecast(.interactive = T, .plotly_slider = T)

#zmiana time units 
