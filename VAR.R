
library(tidyquant)
library(tidyverse)
library(vars)

# 📥 Pobranie danych Brent i DXY
brent <- tq_get("BZ=F", from = "2014-01-01", to = "2024-10-01") %>%
  dplyr::select(date, brent = close)

# Pobranie danych DXY (poprawiony ticker "DX-Y.NYB")
dxy <- tq_get("DX-Y.NYB", from = "2014-01-01", to = "2024-10-01") %>%
  dplyr::select(date, dxy = close)

# 🔗 Połączenie danych
data_joined <- left_join(brent, dxy, by = "date") %>%
  drop_na()

# 🔄 Przygotowanie danych: tworzymy szereg czasowy
ts_data <- ts(data_joined[, -1], frequency = 12)  # tworzymy szereg czasowy na podstawie braku "date"

# 🧪 Sprawdzenie stacjonarności

adf_test_brent <- ur.df(ts_data[, "brent"], type = "drift")
adf_test_dxy <- ur.df(ts_data[, "dxy"], type = "drift")

# Jeśli dane nie są stacjonarne, wykonaj różnicowanie
ts_diff <- diff(ts_data)

# 🔍 Określenie optymalnego opóźnienia p
var_select <- VARselect(ts_diff, lag.max = 10, type = "both")
print(var_select$selection)

# Dopasowanie modelu VAR (zakładając p = 1)
var_model <- VAR(ts_diff, p = 6)
summary(var_model)

# 🔮 Prognoza z modelu VAR na 12 miesięcy
forecast_var <- predict(var_model, n.ahead = 3)

# 📉 Wykres prognoz
par(mfrow = c(2, 1))
ts.plot(forecast_var$fcst$brent[, 1], col = "blue", main = "Prognoza Brent")
ts.plot(forecast_var$fcst$dxy[, 1], col = "green", main = "Prognoza DXY")
