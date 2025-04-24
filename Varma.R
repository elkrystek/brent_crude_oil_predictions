# 📦 Pakiety
install.packages("MTS")   # tylko raz
library(MTS)

library(tidyquant)
library(tidyverse)
library(MTS)
library(urca)

# 📥 Dane Brent (już masz)
brent <- tq_get("BZ=F", from = "2014-01-01", to = "2024-10-01") %>%
  select(date, brent = close)

# 📥 Dane Dollar Index (DXY)
dxy <- tq_get("DX-Y.NYB", from = "2014-01-01", to = "2024-10-01") %>%
  select(date, dxy = close)

# 🔗 Połączenie danych
data_joined <- left_join(brent, dxy, by = "date") %>%
  drop_na()

# 📈 Wstępny wykres
data_joined %>%
  pivot_longer(-date, names_to = "instrument", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = instrument)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Brent vs Dollar Index (DXY)", y = "Cena", x = "Data")

# 🧪 Sprawdzenie stacjonarności
adf_b <- summary(ur.df(data_joined$brent, type = "drift"))
adf_d <- summary(ur.df(data_joined$dxy, type = "drift"))
print(adf_b)
print(adf_d)

# 🔁 Różnicowanie (1. różnica)
ts_data <- ts(data_joined[, -1], frequency = 12)
ts_diff <- diff(ts_data)

# 🔍 Wybór parametrów VARMA
Eccm(ts_diff, maxp = 5, maxq = 6)

# 📊 Dopasowanie modelu VARMA
model_varma <- VARMA(ts_diff, p = 4, q = 6)
summary(model_varma)
cor(ts_data)
# 🔮 Prognoza
forecast <- predict(model_varma, h = 12)

# 📉 Wykres prognoz (dla każdej zmiennej osobno)
par(mfrow = c(2, 1))
ts.plot(forecast$pred[, 1], forecast$se[, 1], col = c("blue", "red"),
        main = "Prognoza: Brent (różnicowana)")
ts.plot(forecast$pred[, 2], forecast$se[, 2], col = c("green", "orange"),
        main = "Prognoza: DXY (różnicowana)")
