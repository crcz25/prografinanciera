
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

returns.weekly.zoo <- diff(log(as.zoo(prices.df)))
returns.weekly.df = as.data.frame(returns.weekly.zoo)

return.annualized.zoo <- ((returns.weekly.zoo + 1) ** 52) - 1
return.annualized.df <- as.data.frame(return.annualized.zoo)

# Multiperiod binomial model
weekly_sd <- sd(returns.weekly.zoo)
stock_price <- prices.df[nrow(prices.df), ]

multiperiodBinomialFunction <- function(periods, iterations, weekly_sd_cc) {
  u <- exp(weekly_sd_cc)
  d <- 1 / u
  
  q = ()
  
  
}

multiperiodBinomialFunction(52, 10000, weekly_sd)


# Black and Sholes model
annual_sd <- sd(return.annualized.zoo)
