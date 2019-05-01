
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




# Risk Free

riskfree = "INTGSTMXM193N"

try(getSymbols(riskfree,
               src = "FRED",
               periodicity = "weekly"
))

rfrate.obj = lapply(riskfree, get)
rfrate.zoo <- do.call(merge, rfrate.obj)
rm(rfrate.obj)

rfrate.df <- rfrate.zoo[index(rfrate.zoo) >= as.Date("2018-01-01") & index(rfrate.zoo) <= as.Date("2018-12-01")]

risk_free_rate <- mean(rfrate.df)/100



# Multiperiod binomial model
weekly_sd <- sd(returns.weekly.zoo)




# Black and Sholes model

# S = stock price
# K = strike price
# r = annual risk-free rate
# t = time to expiration date (meassured in years or fraction of years)
# sd = standard deviation (annualized) of the stock continuously compounded return
# N(z) = Cumulative density function of the standard normal probatility function (mean=0, standard deviation=1); it is the probability to get a Z equal or less than z.

annual_sd <- weekly_sd * sqrt(52)

stock_price <- prices.df[nrow(prices.df), ]

strike_price = 40


black_sholes_model_call = function(S, K, r, t, sd) {
  aux1 = log(S / K)
  aux2 = (r + ((sd ** 2)/2) ) * t
  aux3 = sd * sqrt(t)
  d1 = (aux1 + aux2) / aux3
  d2 = d1 - (sd * sqrt(t))
  
  C = S * pnorm(d1) - K * exp(-1 * r * t) * pnorm(d2)
  
  return(C)
  
}

call = black_sholes_model_call(stock_price, strike_price, risk_free_rate, 1, annual_sd)


black_sholes_model_put = function(S, K, r, t, sd) {
  aux1 = log(S / K)
  aux2 = (r + ((sd ** 2)/2) ) * t
  aux3 = sd * sqrt(t)
  d1 = (aux1 + aux2) / aux3
  d2 = d1 - (sd * sqrt(t))
  
  P = K * exp(-1 * r * t) * pnorm(-d2) - S * pnorm(-d1)
  
  return(P)
  
}


put = black_sholes_model_put(stock_price, strike_price, risk_free_rate, 1, annual_sd)

