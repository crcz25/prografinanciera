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
