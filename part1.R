
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(IntroCompFinR)
library(readxl)


# Set Workspace
rm(list = ls())

# Part 1
periodicity = "weekly"

from = "2018-01-01"

to = "2018-12-31"

ticker = "KIMBERA.MX"

try(getSymbols(ticker,
           from = from, to = to,
           periodicity = periodicity,
           src = "yahoo"))


objList <- lapply(ticker, get)

prices.zoo <- do.call(merge, objList)

rm(objList)

prices.df <- as.data.frame(prices.zoo) %>%
  na.omit() %>%
  select(contains("Adjusted"))

returns.df.weekly <- as.data.frame(diff(log(as.zoo(prices.df))))

return.annualized <- as.data.frame(((returns.df.weekly + 1) ** 52) - 1)
