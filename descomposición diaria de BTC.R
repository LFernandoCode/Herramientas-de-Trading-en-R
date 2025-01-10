library(binancer)
library(tidyverse)
library(writexl)
library(zoo)
library(lubridate)


#cargar los datos
b1 <- binance_coins_prices("BTCUSDT")
coin <- binance_klines(symbol = "BTCUSDT", interval = "1d")



plot(coin$open_time, coin$open, type = "l", col = "blue", lwd = 2, )

# Agregar la segunda línea con color azul (usando 'col' en lugar de 'colorspaces')
lines(coin$open_time, coin$low, col = "red", lwd = 2)


# Crear una serie temporal 
pro_ts <- zoo(coin$volume, order.by = as.Date(coin$open_time))

# Descomponer la serie en componentes estacionales, tendencia y residuos
desc <- stl(ts(pro_ts, frequency = 52), s.window = "periodic")
plot(desc)


#comvertir a dataframe para exportar a excel
data_df <- data.frame(
      Time = time(pro_ts),
      Seasonal = desc$time.series[, "seasonal"],
      Trend = desc$time.series[, "trend"],
      Remainder = desc$time.series[, "remainder"]
)

head(data_df)

data_df$dprice <-(coin$open-coin$low)/coin$open
data_df$volume <- coin$volume

#graficos de shock en volumen y diferencia de la apertura con el mínimo
par(mfrow = c(2, 1), mar = c(2, 4, 1, 1), oma = c(1, 1, 1, 1))
plot(data_df$Time, data_df$Remainder, type = "l", col = "blue")
plot(data_df$Time, data_df$dprice, type = "l", col = "red")
par(mfrow = c(1, 1))


#write_xlsx(data_df, "dirrección a exportar")
shock <- data_df %>% 
      filter(year(Time)=="2024"& Remainder >= 69000)
head(shock)

