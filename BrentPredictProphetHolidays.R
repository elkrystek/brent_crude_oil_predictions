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


# Pobranie danych dla Dollar Index
dolar_index <- tq_get("DX-Y.NYB")

# Upewnij się, że daty w obu zbiorach danych są w tym samym formacie
brent_prices <- brent_prices %>% mutate(date = as.Date(date))
dolar_index <- dolar_index %>% mutate(date = as.Date(date))

# Łączenie danych na podstawie daty
df_prophet <- left_join(brent_prices, dolar_index, by = "date")
head(df_prophet)
df_prophet <- left_join(brent_prices, dolar_index, by = "date") %>%
  select(ds= date, y = close.x, dxy = close.y) %>%
  drop_na()

m = prophet()
m = add_regressor(m,'dxy')
m=fit.prophet(m,df_prophet)
m
#Tworzenie df z prognoza
future = make_future_dataframe(m, periods = 100)
future$dxy = 100
forecast = predict(m,future)
prophet_plot_components(m,forecast)
plot(m,forecast)

#odrzucenie danych z covidu oraz po wybuchu wojny z ukraina

# Okres COVID (np. marzec 2020 - czerwiec 2021)
covid_dates <- data.frame(
  holiday = 'covid',
  ds = seq(as.Date("2020-03-01"), as.Date("2021-06-30"), by = "day"),
  lower_window = 0,
  upper_window = 0
)

# Okres wojenny (np. luty 2022 - grudzień 2022)
war_dates <- data.frame(
  holiday = 'war',
  ds = seq(as.Date("2022-02-24"), as.Date("2022-12-31"), by = "day"),
  lower_window = 0,
  upper_window = 0
)

# mh - model with holidays
holidays_df <- bind_rows(covid_dates, war_dates)
mh <- prophet(holidays = holidays_df)


# Uczenie modelu
mh <- fit.prophet(mh, df_prophet)


forecast_with_holidays <- predict(mh, future)

# Cross-walidacja dla modelu z holidayami
cv_results <- cross_validation(
  mh,
  initial = 730,    # 2 lata danych treningowych
  period  = 180,    # co 6 miesięcy nowy fold
  horizon = 365,    # prognoza na 1 rok
  units   = "days"
)

# Obliczenie metryk
metrics <- performance_metrics(cv_results)
print(metrics)

# Wizualizacja metryki ( MAPE)
plot_cross_validation_metric(cv_results, metric = "mape")


