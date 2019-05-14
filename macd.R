require(quantstrat)
require(quantmod)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggplot2)

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

p1 <- ggplot(Cl(prices.zoo), aes(Index, IBM.Close)) + 
  geom_line()
p2 <- ggplot(macd[, c("macd", "signal")], aes(Index)) + 
  geom_line(aes(y = macd, colour="macd")) + 
  geom_line(aes(y = signal, colour="signal"))
p3 <-ggplot(data=macd$macdOsc, aes(x=Index, y=macdOsc)) +
  geom_bar(stat="identity")

p4 <- ggplot(rsi, aes(Index)) + 
  geom_line(aes(y = rsi)) +
  geom_point(data=rsi[20], aes(y = rsi, size=10), color="green", shape=24, fill="green") +
  annotate("rect", xmin=index(rsi)[1], xmax=index(rsi)[length(index(rsi))], ymin=30, ymax=70, alpha=0.2, fill="blue") +
  theme(legend.position = "none")


multiplot(p1, p2, p3, p4, cols=1)
