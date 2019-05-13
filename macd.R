require(quantstrat)
require(quantmod)
library(dplyr)
library(ggplot2)
library(tibble)

from = "2018-10-01"

to = "2019-05-08"

ticker = "IBM"

periodicity = "daily"


try(getSymbols(
  ticker,
  from = from,
  to = to,
  periodicity = periodicity,
  src = "yahoo"
))

objList <- lapply(ticker, get)

prices.zoo <- do.call(merge, objList)

EMA(Cl(prices.zoo), n = 12)

EMA(Cl(prices.zoo), n = 26)

macd = MACD(
  Cl(prices.zoo),
  nFast = 12,
  nSlow = 26,
  nSig = 9,
  maType = EMA,
  percent = FALSE
)

rsi <- RSI(Cl(prices.zoo))


signal <-
  ifelse ((macd$signal > macd$macd) & (rsi$rsi > 70), 1,
          ifelse((macd$signal < macd$macd) & (rsi$rsi < 30), -1, 0))

# Replace na to zeros
signal[is.na(signal)] <- 0

signal = as.data.frame(signal)

signal <- signal %>% rownames_to_column("date")

#Simulacion

initial = asu = 10000
n = 0
val = 0
state = -1

for (row in seq(1, nrow(signal) - 1)) {
  data = signal[row,]
  nxt = signal[row + 1, ]
  
  #print(data$signal)
  
  if (data$signal == -1 & state == -1) {
    print("Comprar")
    price = Cl(prices.zoo)[data$date]
    acciones = floor(initial / price)
    
    initial = initial - (acciones * price)
    
    n = n + acciones
    
    cat("Acciones", acciones, "\n")
    cat("initial", initial, "\n")
    cat("val", initial + (acciones * price), "\n")
    state = 1
    print(data$date)
  }
  if (state == 1 & data$signal == 1) {
    print("Vender")
    price = Cl(prices.zoo)[data$date]
    acciones = floor(initial / price)
    
    initial = initial + acciones
    
    cat("Acciones", acciones, "\n")
    cat("initial", initial, "\n")
    cat("val", initial + (acciones * price), "\n")
    
    n = 0
    state = -1
    print(data$date)
  }
}







# lel <- merge(Cl(prices.zoo), macd)
# lel$macdOsc <- lel$macd - lel$signal
# 
# lel = as.data.frame(lel)
# 
# #buy = 1 sell = 0
# # Check if the
# lel$Bullish_Bearish_Crossover = ifelse((lel$macd > lel$signal), 1, 0)
# 
# # Upside momentum is increasing
# lel$Bullish_Bearish_Centerline_Crossover = ifelse((lel$macd > 0), 1, 0)
# 
# 
# plot(lel$macd)
# lines(lel$signal, col = "blue")
# lines(lel$macdOsc, col = "red")
# 
# 
# 
# rsi = as.data.frame(RSI(Cl(prices.zoo)))
# 
# 
# #polygon(cbind(c(min(index(lel)), index(lel), max(index(lel))), c(min(lel$macd), lel$macd, min(lel$macd))), col="#00CC66")
# 
# 
# 
# 
# 
# get_higher_lows <- function(data) {
#   days_to_acum <- 5
#   
#   differences <- diff(data)
#   sd_data <- sd(data)
#   
#   
#   past <- rep(NA, length(differences))
#   for (i in days_to_acum:length(differences)) {
#     past[i] <- mean(differences[i:(i - days_to_acum + 1)])
#   }
#   print(past)
#   
#   future <- rep(NA, length(differences))
#   for (i in (length(differences) - days_to_acum + 1):1) {
#     future[i] <- mean(differences[i:(i + days_to_acum - 1)])
#   }
#   print(future)
#   
#   threshold <-  (sd_data * 0.01)
#   
#   higher_lows <- c()
#   for (i in days_to_acum:(length(differences) - days_to_acum + 1)) {
#     v <- min(-past[i], future[i])
#     print(paste(i, v))
#     
#     if (data[i] >= data[i + 1] &&
#         data[i + 1] <= data[i + 2] && v > threshold) {
#       print(paste(i, past[i], future[i]))
#       higher_lows <- c(higher_lows, i + 1)
#     }
#   }
#   
#   
#   return(higher_lows)
# }
# 
# higher_lows <- get_higher_lows(na.omit(lel$macd))
# 
# 
# higher_lows <- get_higher_lows(lel$IBM.Close)
# 
# 
# points(higher_lows, lel$IBM.Close[higher_lows], pch = 19, col = "red")
# 
# points(higher_lows, lel$macd[higher_lows], pch = 19, col = "blue")
# 
# 
# chartSeries(IBM)
# addMACD(
#   fast = 12,
#   slow = 26,
#   signal = 9,
#   type = "EMA",
#   histogram = TRUE
# )
# 
# 
# #Simulacion
# initial = asu = 10000
# n = 0
# val = 0
# 
# cat("Inicial" , initial)
# 
# cat("Acciones", n)
# 
# #buy = 1 sell = 0
# for (row in seq(1, nrow(lel) - 1)) {
#   data = lel[row,]
#   nxt = lel[row + 1,]
#   
#   if (is.na(data$Bullish_Bearish_Crossover) &
#       !is.na(nxt$Bullish_Bearish_Crossover) &
#       nxt$Bullish_Bearish_Crossover == 1) {
#     print("Comprar")
#     
#     acciones = floor(initial / nxt$IBM.Close)
#     
#     n = n + acciones
#     
#     
#     initial = initial - (acciones * nxt$IBM.Close)
#     
#     cat("Acciones", acciones, "\n")
#     cat("Dinero", initial, "\n")
#     cat("Val", (acciones * nxt$IBM.Close) + initial, "\n")
#   }
#   
#   if (!is.na(data$Bullish_Bearish_Crossover) &
#       data$Bullish_Bearish_Crossover == 0 &
#       nxt$Bullish_Bearish_Crossover == 1) {
#     print('Comprar')
#     acciones = as.numeric(floor(initial / nxt$IBM.Close))
#     
#     n = n + acciones
#     
#     initial = initial - (acciones * nxt$IBM.Close)
#     
#     cat("Acciones", acciones, "\n")
#     cat("Dinero", initial, "\n")
#     cat("Val", (acciones * nxt$IBM.Close) + initial, "\n")
#   }
#   
#   if (!is.na(data$Bullish_Bearish_Crossover) &
#       data$Bullish_Bearish_Crossover == 1 &
#       nxt$Bullish_Bearish_Crossover == 0) {
#     print('Vender')
#     acciones = n * nxt$IBM.Close
#     
#     cat("Acciones", n, "\n")
#     cat("Dinero", initial, "\n")
#     initial = initial + acciones
#     val = initial
#     cat("Val",  initial, "\n")
#     n = 0
#   }
#   
# }
# 
# print(n)
# print(initial)
# print(val / asu)
# 
# 
# 
# # RSI
# # Calculate RSI
# initial = asu = 10000
# n = 0
# val = 0
# 
# 
# rsi = as.data.frame(rsi)
# 
# #rsi$Action = ifelse((rsi$rsi > 70), "Overbought - Sell", ifelse((rsi$rsi < 30), "Oversold - Buy", NA))
# 
# 
# rsi <- rsi %>% rownames_to_column("date")
# 
# 
# state = 1
# for (row in seq(1, nrow(rsi) - 1)) {
#   data = rsi[row, ]
#   nxt = rsi[row + 1, ]
#   
#   
#   if (!is.na(data)) {
#     if (data$rsi > 70 & nxt$rsi < 70) {
#       print("Vender")
#       print(data)
#       print(nxt)
#       
#       price = Cl(prices.zoo)[data$date]
#       
#       state = 0
#     }
#     if (data$rsi < 30 & nxt$rsi > 30) {
#       print("comprar")
#       print(data)
#       print(nxt)
#       
#       price = Cl(prices.zoo)[data$date]
#       
#       state = 1
#     }
#   }
# }
