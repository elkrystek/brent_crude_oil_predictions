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
library(jsonlite)

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


#pobrane z https://www.eia.gov/opendata/browser/crude-oil-imports?frequency=monthly&data=quantity;&facets=destinationId;&destinationId=PT_2704;&start=2014-01&end=2024-10&sortColumn=period;&sortDirection=desc;
# URL API
url <- "https://api.eia.gov/v2/crude-oil-imports/data/?frequency=monthly&data[0]=quantity&facets[destinationId][]=PT_2704&start=2014-01&end=2024-10&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=2hRfeFIdSAj9LfJh3682LchbZrtMiMSVEPlwbPi2"

# Pobranie danych
data <- curl::curl_fetch_memory(url)


# Parsowanie danych JSON
parsed_data <- fromJSON(rawToChar(data$content))

# Wyświetlenie wyników
print(parsed_data)
df <- as.data.frame(parsed_data$response$data)
head(df)


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
