require(quantstrat)
require(quantmod)
library(dplyr)
library(ggplot2)


from = "2018-10-01"

to = "2019-05-08"

ticker = "IBM"

periodicity = "daily"


try(getSymbols(ticker,
               from = from, to = to,
               periodicity = periodicity,
               src = "yahoo"))

objList <- lapply(ticker, get)

prices.zoo <- do.call(merge, objList)

EMA(Cl(prices.zoo), n=12)

EMA(Cl(prices.zoo), n=26)

macd = MACD(Cl(prices.zoo), nFast=12, nSlow=26, nSig=9, maType=EMA, percent = FALSE)

lel <- merge(Cl(prices.zoo), macd)
lel$macdOsc <- lel$macd - lel$signal

lel = as.data.frame(lel)

# Check if the 
lel$Bullish_Bearish_Crossover = ifelse((lel$macd > lel$signal), "Bullish - Buy", "Bearish - Sell")

# Upside momentum is increasing
lel$Bullish_Bearish_Centerline_Crossover = ifelse((lel$macd > 0), "Buy", "Sell")


plot(lel$macd)
lines(lel$signal, col="blue")
lines(lel$macdOsc, col="red")

polygon(cbind(c(min(index(lel)), index(lel), max(index(lel))), c(min(lel$macd), lel$macd, min(lel$macd))), col="#00CC66")



get_higher_lows <- function(data) {
  days_to_acum <- 5
  
  differences <- diff(data)
  sd_data <- sd(data)
  
  
  past <- rep(NA, length(differences))
  for(i in days_to_acum:length(differences)) {
    past[i] <- mean(differences[i:(i - days_to_acum + 1)])
  }
  print(past)
  
  future <- rep(NA, length(differences))
  for(i in (length(differences) - days_to_acum + 1):1) {
    future[i] <- mean(differences[i:(i + days_to_acum - 1)])
  }
  print(future)

  threshold <-  (sd_data * 0.01)

  higher_lows <- c()
  for(i in days_to_acum:(length(differences) - days_to_acum + 1)) {
    v <- min(-past[i], future[i])
    print(paste(i, v))

    if(data[i] >= data[i + 1] && data[i + 1] <= data[i + 2] && v > threshold) {
      print(paste(i, past[i], future[i]))
      higher_lows <- c(higher_lows, i + 1)
    }
  }
  
  
  return(higher_lows)
}

higher_lows <- get_higher_lows(lel$IBM.Close)
print(higher_lows)

points(higher_lows, lel$macd[higher_lows], pch=19, col ="red")
