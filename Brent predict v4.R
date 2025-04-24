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
library(tsfknn) #to nie działa w tidymodels
library(kknn)

library(TTR) #analiza techniczna

#pobranie danych finansowych dla Brent z Yahoo
brent_prices <- tq_get("BZ=F")
brent_prices
brent_prices %>% plot_time_series(date, close, .smooth = T,
                                  .smooth_size = 0.5,
                                  .smooth_alpha = 0.7,
                                  .smooth_degree = 2,
                                  .smooth_period = "1 year",
                                .interactive = T,
                                .title = "Brent Oil price (USD)")


#dodanie dodatkowej zmiennej:
# Pobranie danych dla Dollar Index
dolar_index <- tq_get("DX-Y.NYB")

# Upewniam się, że daty w obu zbiorach danych są w tym samym formacie
brent_prices <- brent_prices %>% mutate(date = as.Date(date))
dolar_index <- dolar_index %>% mutate(date = as.Date(date))

# Łączenie danych na podstawie daty
brent_prices <- left_join(brent_prices, dolar_index, by = "date")

brent_prices <- left_join(brent_prices, dolar_index, by = "date") %>%
  select(date= date, close = close.x, dxy = close.y) %>%
  drop_na()

#dodanie  analizy technicznej
brent_prices <- brent_prices %>%
  arrange(date) %>%
  mutate(
    macd_full = MACD(close, nFast = 12, nSlow = 26, nSig = 9, maType = EMA)
  ) %>%
  mutate(
    macd = macd_full[, "macd"],
    signal = macd_full[, "signal"],
    sma_14 = SMA(close, n = 14),
    macd_hist = macd - signal
  ) %>%
  select(-macd_full) %>%
  drop_na()


head(brent_prices)
# 
# #pobrane z https://www.eia.gov/opendata/browser/crude-oil-imports?frequency=monthly&data=quantity;&facets=destinationId;&destinationId=PT_2704;&start=2014-01&end=2024-10&sortColumn=period;&sortDirection=desc;
# # URL API
# url <- "https://api.eia.gov/v2/crude-oil-imports/data/?frequency=monthly&data[0]=quantity&facets[destinationId][]=PT_2704&start=2014-01&end=2024-10&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=2hRfeFIdSAj9LfJh3682LchbZrtMiMSVEPlwbPi2"
# 
# # Pobranie danych
# data <- curl::curl_fetch_memory(url)
# 
# 
# # Parsowanie danych JSON
# parsed_data <- fromJSON(rawToChar(data$content))
# 
# # Wyświetlenie wyników
# print(parsed_data)
# df <- as.data.frame(parsed_data$response$data)
# head(df)


#podział na zbior treningowy i testowy
splits <- brent_prices %>% time_series_split(assess = "1 month", cumulative = TRUE)
splits %>% 
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date,close)

#---------------------------------
#modelowanie przy uzyciy Prophet
model_prophet <-prophet_reg(mode = "regression",
                            growth="linear",
                            # num_changepoints = 25,
                            season = "additive") %>%
  set_engine("prophet", weekly.seasonality = TRUE) %>%
  fit(close ~ date, training(splits))

# model z regresorem


model_prophet_with_dxy <-prophet_reg(mode = "regression",
                            growth="linear",
                            # num_changepoints = 25,
                            season = "additive") %>%
  set_engine("prophet", weekly.seasonality = TRUE) %>%
  fit(close ~ date + dxy, training(splits))

#poniewaz dodany zostal regresor to nalezy przygotowac ramke z przyszlymi wartosciami regresora
# Tworzymy przyszłe daty
last_date <- max(brent_prices$date)

# Tworzymy 30 dni w przyszłość (1 miesiąc)
future_dates <- tibble(
  date = seq.Date(from = last_date + 1, by = "day", length.out = 30)
)
# Zakładamy np. że DXY utrzyma się na poziomie 100 
future_dxy <- rep(100, nrow(future_dates))


# Dodajemy regresor dollar index
future_data_dxy <- future_dates %>%
  mutate(dxy = future_dxy)

#modelowanie przy uzyciy Arima
model_arima <- arima_reg(mode = "regression",
                         seasonal_period = 12) %>%
  set_engine("auto_arima", stepwise = FALSE, approximation = FALSE)%>%
  fit(close ~ date, training(splits))

#proba knn
model_knn <- nearest_neighbor(
  mode = "regression",
  neighbors = 5,
  weight_func = "rectangular"
) %>%
  set_engine("kknn")%>%
  fit(close ~ date, training(splits))
#wnioski z knn: trzebaby użyć tsfknn, którego nie ma w tidymodels

#prophet with holidays
  
  #odrzucenie danych z covidu oraz po wybuchu wojny z ukraina
  
  # Okres COVID ( marzec 2020 - czerwiec 2021)
  covid_dates <- data.frame(
    holiday = 'covid',
    ds = seq(as.Date("2020-03-01"), as.Date("2021-06-30"), by = "day"),
    lower_window = 0,
    upper_window = 0
  )

# Okres wojenny ( luty 2022 - grudzień 2022)
war_dates <- data.frame(
  holiday = 'war',
  ds = seq(as.Date("2022-02-20"), as.Date("2022-12-31"), by = "day"),
  lower_window = 0,
  upper_window = 0
)

# model with holidays
holidays_df <- bind_rows(covid_dates, war_dates)

model_prophet_with_holidays <-prophet_reg(mode = "regression",
                            growth="linear",
                            season = "additive",
                            prior_scale_holidays = 20  ) %>%
  set_engine("prophet", weekly.seasonality = TRUE, holidays = holidays_df) %>%
  fit(close ~ date, training(splits))  

#po sprawdzeniu okazuje się, że holidays nie są uwzględniane przez tidymodels... Sprawdzony został wpływ holidays na surowym prophecie i wcale wyniki się nie polepszyły. 
# w tej wersji po prostu parametr prior_scale_holidays jest wzmocniony (powinien on zostać nadpisany przez holidays wg. dokumentacji     )

model_prophet_with_MACD <- prophet_reg(mode = "regression",
                                       growth = "linear",
                                       season = "additive") %>%
  set_engine("prophet", weekly.seasonality = TRUE) %>%
  fit(close ~ date + dxy  + macd_hist, training(splits))

model_prophet_with_SMA14 <- prophet_reg(mode = "regression",
                                       growth = "linear",
                                       season = "additive") %>%
  set_engine("prophet", weekly.seasonality = TRUE) %>%
  fit(close ~ date + dxy  + sma_14, training(splits))

model_prophet_with_MACD_SMA <- prophet_reg(mode = "regression",
                                        growth = "linear",
                                        season = "additive") %>%
  set_engine("prophet", weekly.seasonality = TRUE) %>%
  fit(close ~ date + dxy  + macd_hist + sma_14, training(splits))

#ramka z przyszłymi danymi
 
future_sma <- rep(tail(training(splits)$sma_14, 1), nrow(future_dates))
future_macd <- rep(tail(training(splits)$macd_hist, 1), nrow(future_dates))

future_data <- future_dates %>%
  mutate(
    dxy = future_dxy,
    sma_14 = future_sma,
    macd_hist = future_macd
  )

#------------------------------
#kalibracja
models_table <- modeltime_table(model_prophet,
                                model_arima,
                                model_prophet_with_holidays,
                                model_prophet_with_dxy,
                                model_prophet_with_MACD,
                                model_prophet_with_SMA14,
                                model_prophet_with_MACD_SMA,
                                model_knn
                                )

models_table = update_model_description(models_table,3, "PROPHET - Holidays")
models_table = update_model_description(models_table,4, "PROPHET - with dxy")
models_table = update_model_description(models_table,5, "PROPHET - with dxy & MACD")
models_table = update_model_description(models_table,6, "PROPHET - with dxy & SMA14")
models_table = update_model_description(models_table,7, "PROPHET - with dxy & MACD & SMA14")
models_table

calibration_table <-models_table %>%
  modeltime_calibrate(testing(splits))
calibration_table
#forecast
calibration_table %>%
  modeltime_forecast(new_data = testing(splits),actual_data = brent_prices) %>%
  # modeltime_forecast(new_data = future_data, actual_data = brent_prices) %>%
  plot_modeltime_forecast(.interactive = T)
calibration_table %>%
  modeltime_accuracy(new_data = testing(splits),actual_data = brent_prices) %>%
  table_modeltime_accuracy(.interactive = F)


#refit
calibration_table %>%
  modeltime_refit(brent_prices) %>%
  modeltime_forecast(new_data = future_data, actual_data = brent_prices) %>%
  plot_modeltime_forecast(.interactive = T, .plotly_slider = T, .smooth = FALSE)


