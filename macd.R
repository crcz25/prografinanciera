require(quantstrat)
require(quantmod)

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

macd = MACD(Cl(prices.zoo), nFast=12, nSlow=26, nSig=9, maType=EMA, percent = FALSE)

lel <- merge(Cl(prices.zoo), macd)

lel$macdOsc <- lel$macd - lel$signal

lel$Trader_Action <- ifelse((lel$macdOsc > 0), "Buy", "Sell")

tail(lel)

plot(lel$macd)
lines(lel$signal)
lines(lel$macdOsc)

polygon(cbind(c(min(index(lel)), index(lel), max(index(lel))), c(min(lel$macd), lel$macd, min(lel$macd))), col="#00CC66") 
