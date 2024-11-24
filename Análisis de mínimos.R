library(tidyverse)
library(binancer)
library(ggplot2)

#datos 
b1<- binance_coins_prices()
head(b1)
ada<- binance_klines(symbol="ADAUSDT",interval = "1h")

ada<-ada %>% 
  mutate(fecha=as.Date(ada$open_time)) %>% 
  mutate(horas = format(as.POSIXct(open_time), "%H:%M:%S"))


LOWS<-ada %>% 
  group_by(fecha) %>% 
  filter(low==min(low)) %>% 
  slice(1)

  


frecuencia_horas <- LOWS %>%
  group_by(horas) %>%
  summarise(frecuencia = n())



#grafico
ggplot(frecuencia_horas, aes(x = "", y = frecuencia, fill = factor(horas))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribución de Horas de mínimos de Cardano",
    x = NULL, y = NULL,
    fill = "Hora del día"
  ) +
  theme_void() + 
  theme(
    plot.title = element_text(hjust = 0.5))
