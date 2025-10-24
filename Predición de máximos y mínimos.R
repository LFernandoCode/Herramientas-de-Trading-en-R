# Título:      Análisis de Series de Tiempo con MSM
# Autor:       Fernando Flores
# Fecha:       25/09/2025
# Descripción: 
#   Este script tiene como objetivo ilustrar el uso de modelos de Markov 
#   Switching (MSM) para analizar series macroeconómicas, como el PIB y 
#   otra serie de interés. Se muestran los pasos desde la carga y limpieza 
#   de datos hasta la estimación del modelo y la visualización de regímenes 
#   ocultos.
# # Notas importantes: Modificar las direcciónes

###############################################################################
#######                          LIBRERÍAS                               ######
###############################################################################
library(readxl)
library(tidyverse)
library(quantmod)
library(MSGARCH)
library(MSwM)
library(TTR)
library(BCDating)
library(forecast)
library(binancer)
library(writexl)
library(zoo)
###############################################################################
#######                          Datos                               ######
###############################################################################
base1 <- read_excel("/BTCHist.xlsx", sheet = "1")
###############################################################################
#######                          Procesamiento datos                              ######
###############################################################################

MA10 <- data.frame(MA=SMA(base1$Cola, n = 10)) 

MA10 <- MA10 %>%
      mutate(VAR1 = 100 * (log(MA) - log(lag(MA, 1)))) %>%
      drop_na()

##Visualizar

par(mfrow=c(2,1), mar = c(4,4,1,1))
plot(MA10$MA, type="l")
plot(MA10$VAR1, type="l")



###############################################################################
#######                          Modelo MSM                             ######
###############################################################################

y_s <- MA10$MA
lrm <- lm(y_s ~ 1)

rsm <- msmFit(
      lrm,
      k = 2,
      sw = c(TRUE,TRUE),  #cambio media, varianza de e
      control = list(parallel = FALSE)#depende la Pc
)

###############################################################################
#######                         Graficos                            ######
###############################################################################
# filtered / smoothed probabilities
x11(); plotProb(rsm, which = 1)  # filtradas vs smoothed
x11(); plotProb(rsm, which = 2)  # régimen 1 y área sombreada
x11(); plotProb(rsm, which = 3)  # régimen 2 y área sombreada
graphics.off()
regimsm <- as.data.frame(rsm@Fit@smoProb)
regimsm$Regime1_class <- ifelse(regimsm$V1 > 0.5, 1, 0)

###############################################################################
#######                         ciclos                            ######
###############################################################################
# Find indices of 1's
one_idx <- which(regimsm$Regime1_class == 1)

# Split into cycles: a new cycle starts when the gap between ones > 1
cycle_breaks <- c(0, which(diff(one_idx) > 1), length(one_idx))

# Initialize list for cycle durations
cycle_durations_msm <- numeric()

for (i in seq_len(length(cycle_breaks) - 1)) {
      # indices of 1s in this cycle
      start_1s <- one_idx[(cycle_breaks[i] + 1):cycle_breaks[i + 1]]
      # start of cycle = previous 0-run start (just before the first 1)
      start_idx <- max(1, min(start_1s) - 1)
      while (start_idx > 1 && regimsm$Regime1_class[start_idx - 1] == 0) {
            start_idx <- start_idx - 1
      }
      end_idx <- max(start_1s)
      cycle_durations_msm[i] <- end_idx - start_idx + 1
}


cydu_msm <-  mean(cycle_durations_msm)


###############################################################################
#######                          Identificación de máximos                             ######
##############################################################################

velas <- binance_klines('BTCUSDT', interval = '1d',
                        start_time = Sys.Date()-150,
                        end_time = Sys.Date()-1) 
velas$sho_max <- (velas$high-velas$open)*100/velas$open
pro_tsmax <- ts(velas$sho_max, frequency = cydu_msm-10)
desc_max <- stl(pro_tsmax, s.window = 20)
plot(desc_max)
tail(desc_max$time.series)
###############################################################################
#######                          Predicción                             ######
##############################################################################


predicmax <- forecast(desc_max, h = 5)
predicmax
plot(predicmax)

###############################################################################
#######                          Identificación de mínimos                             ######
##############################################################################


velas$sho_min <- (velas$open-velas$low)*100/velas$open
pro_tsmin <- ts(velas$sho_min, frequency = cydu_msm-10)
desc_min <- stl(pro_tsmin, s.window = 7)
plot(desc_min)
tail(desc_min$time.series)
###############################################################################
#######                          Predicción                             ######
##############################################################################

predicmin <- forecast(desc_min, h = 5)
predicmin
plot(predicmin)


