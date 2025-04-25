library(tidyverse)
library(tidyquant)
library(lubridate)
library(modeltime)
library(prophet)
library(timetk)

# Dane
brent_prices <- tq_get("BZ=F")
dolar_index <- tq_get("DX-Y.NYB")

# Przygotowanie danych
df_prophet <- left_join(
  brent_prices %>% mutate(date = as.Date(date)) %>% select(date, price = close),
  dolar_index %>% mutate(date = as.Date(date)) %>% select(date, dxy = close),
  by = "date"
) %>%
  drop_na() %>%
  rename(ds = date, y = price)

# Święta: COVID + wojna (przykład)
holidays_df <- tibble(
  holiday = c("covid", "ukraine_war"),
  ds = as.Date(c("2020-03-15", "2022-02-24")),
  lower_window = 0,
  upper_window = 60
)

# Podział na zbiór treningowy/testowy
splits <- time_series_split(df_prophet, assess = "3 months", cumulative = TRUE)

# === Model bez holidays ===
model_prophet <- prophet_reg(
  mode = "regression",
  growth = "linear",
  season = "additive"
) %>%
  set_engine("prophet") %>%
  fit(y ~ ds + dxy, data = training(splits))

# === Model z holidays ===
model_prophet_holidays <- prophet_reg(
  mode = "regression",
  growth = "linear",
  season = "additive"
) %>%
  set_engine("prophet", holidays = holidays_df) %>%
  fit(y ~ ds + dxy, data = training(splits))

# === Tabela modeli ===
models_table <- modeltime_table(
  model_prophet,
  model_prophet_holidays
) %>%
  update_model_description(1, "PROPHET - NO Holidays") %>%
  update_model_description(2, "PROPHET - Holidays")

# === Kalibracja ===
calibration_table <- models_table %>%
  modeltime_calibrate(testing(splits))

# === Prognoza + wykres ===
calibration_table %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = df_prophet
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)

# === Dokładność ===
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

model_prophet_holidays$fit$fit$fit$holidays

