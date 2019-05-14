require(quantstrat)
require(quantmod)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggplot2)

from = "2018-01-01"
to = "2019-05-08"
ticker = "BTCUSD=X"
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
closed = Cl(prices.zoo)
names(closed) = "Close"

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


signal <- closed 
signal[,] <- 0 # Bullish = 1, neutral = 0, Bearish = -1

back_days <- 5

low_threshold <- 30
high_threshold <- 70

rsi_actives <- c()

for(i in (back_days + 1):length(signal)) {
  
  out <- tryCatch(
    {
      idx <- index(signal[i])
      idx_past <- index(signal[i - back_days])
      
      # Signal 1
      not_cur_na <- !is.na(macd$macd[idx]) && !is.na(rsi[idx]) && !is.na(macd$signal[idx])
      not_past_na <- !is.na(macd$macd[idx_past]) && !is.na(rsi[idx_past]) && !is.na(macd$signal[idx_past])
      if(not_cur_na && not_past_na) {
        # Bullish signal
        rsi_subset <- rsi[paste(idx_past, "/", idx, sep = "")]
        
        crosses_rsi <- FALSE
        for(j in 2:length(rsi_subset)) {
          if(rsi_subset[j] >= low_threshold && rsi_subset[j - 1] < low_threshold) {
            crosses_rsi <- TRUE
          }
        }
        
        macd_subset <- macd[paste(idx_past, "/", idx, sep = "")]
        
        crosses_macd <- FALSE
        for(j in 2:length(macd_subset$macd)) {
          if(macd_subset[j, "macd"] >= macd_subset[j, "signal"] && macd_subset[j - 1, "macd"] < macd_subset[j - 1, "signal"]) {
            crosses_macd = TRUE
          }
        }
        
        previous_day = index(macd_subset)[length(macd_subset$macd) - 1]
        if(crosses_rsi && macd[idx, "macd"] >= macd[idx, "signal"] && macd[previous_day, "macd"] <  macd[previous_day, "signal"]) {
          signal[idx] <- 1
        }

        if(crosses_macd && rsi[idx] >= low_threshold && rsi[previous_day] < low_threshold) {
          signal[idx] <- 1
        }
        
        
        # Bearish
        crosses_rsi <- FALSE
        for(j in 2:length(rsi_subset)) {
          if(rsi_subset[j] <= high_threshold && rsi_subset[j - 1] > high_threshold) {
            crosses_rsi <- TRUE
          }
        }
        
        crosses_macd <- FALSE
        for(j in 2:length(macd_subset$macd)) {
          if(macd_subset[j, "macd"] <= macd_subset[j, "signal"] && macd_subset[j - 1, "macd"] > macd_subset[j - 1, "signal"]) {
            crosses_macd = TRUE
          }
        }
        
        previous_day = index(macd_subset)[length(macd_subset$macd) - 1]
        if(crosses_rsi && macd[idx, "macd"] <= macd[idx, "signal"] && macd[previous_day, "macd"] > macd[previous_day, "signal"]) {
          signal[idx] <- -1
        }
        
        if(crosses_macd && rsi[idx] <= high_threshold && rsi[previous_day] > high_threshold) {
          signal[idx] <- -1
        }
        
        
      }
    },
    error=function(cond) {
      print(cond)
    })
}



#Simulacion

initial_chash <- cash <- 10000
stocks <- 0


for (row in 1:length(signal)) {
  s = signal[row]
  
  if (s == 1) {
    print("Comprar")
    price = as.numeric(Cl(prices.zoo)[row])
    number_of_stocks = floor(cash / price)
    
    if(number_of_stocks > 0) {
    
      cash = cash - (number_of_stocks * price)
      stocks = stocks + number_of_stocks
      
      cat("Price", price, "\n")
      cat("Bought stocks", number_of_stocks, "\n")
      cat("cash", cash, "\n")
      cat("Number of owned stocks", stocks, "\n")
    } else {
      print("No money to buy stocks :C")
    }
  } else if (s == -1) {
    print("Vender")
    
    if(stocks > 0) {
      price = as.numeric(Cl(prices.zoo)[row])
      past_stocks <- stocks

      print(price)
      print(stocks)
      cash = cash + stocks * price
      stocks <- 0
      
      cat("Price", price, "\n")
      cat("Sold stocks", past_stocks, "\n")
      cat("Cash", cash, "\n")
      cat("Number of owned stocks", stocks, "\n")
    } else {
      print("No stocks to sell")
    }

  }
}

total <- cash + as.numeric(Cl(tail(prices.zoo, 1))) * stocks
cat("Final money", total, "\n")
cat("Return", (total / initial_chash) - 1, "\n")

macd$macdOsc <- macd$macd - macd$signal
macd$macdOsc[is.na(macd$macdOsc)] <- 0

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

buy_signals <- index(signal[signal == 1])
sell_signals <- index(signal[signal == -1])

p1 <- ggplot(Cl(closed), aes(Index, Close)) + 
  geom_line()

p2 <- ggplot(macd[, c("macd", "signal")], aes(Index)) + 
  geom_line(aes(y = macd, colour="macd"), color="black") + 
  geom_line(aes(y = signal, colour="signal"), color="red") +
  #geom_point(data=macd$macd[as.Date(rsi_actives),], aes(y = macd, size=10), color="yellow", shape=24) +
  theme(legend.position = "none")

if(length(buy_signals) > 0) {
  p2 <- p2 + geom_point(data=macd$macd[buy_signals,], aes(y = macd, size=5), color="green", shape=24)
}

if(length(sell_signals) > 0) {
  p2 <- p2 + geom_point(data=macd$macd[sell_signals,], aes(y = macd, size=5), color="red", shape=25)

}

p3 <-ggplot(data=macd$macdOsc, aes(x=Index, y=macdOsc)) +
  geom_bar(stat="identity")

p4 <- ggplot(rsi, aes(Index)) + 
  geom_line(aes(y = rsi)) +
  annotate("rect", xmin=index(rsi)[1], xmax=index(rsi)[length(index(rsi))], ymin=30, ymax=70, alpha=0.2, fill="blue") +
  theme(legend.position = "none")

if(length(buy_signals) > 0) {
  p4 <- p4 + geom_point(data=rsi[buy_signals,], aes(y = rsi, size=5), color="green", shape=24)
}

if(length(sell_signals) > 0) {
  p4 <- p4 + geom_point(data=rsi[sell_signals,], aes(y = rsi, size=5), color="red", shape=25)
  
}

multiplot(p1, p2, p3, p4, cols=1)
