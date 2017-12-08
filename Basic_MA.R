library(quantmod)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Basic BacktTest for Moving Average: backtest('FB', 5, 10, 1)

backtest <- function( symbol, fast, slow, positive){
  #get data
  x <- getSymbols(symbol)
  symbol <- eval(parse(text = x))
  
  if (sum(is.na(symbol)) != 0){
    symbol <- symbol %>% na.omit(.)
  }else{
    symbol <- symbol
  }
  
  #picture
  chartSeries(symbol, theme = 'white')
  MA1 <- runMean(symbol[, 6], n = slow)
  MA2 <- runMean(symbol[, 6], n = fast)
  addTA(MA1, on = 2, col = "blue") 
  addTA(MA2, on = 2, col = 'red')
  
  #backtest
  if (positive == 1){
    position <- as.matrix(ifelse( MA2 > MA1, 1, 0)) %>% Lag(., k = 1)
  } else{
    position <- as.matrix(ifelse( MA2 > MA1, 0, 1)) %>% Lag(., k = 1)
  }
  
  #return (Total Return = sum of Invest_return)
  data <- as.data.frame(symbol)
  change <- as.matrix(ROC(data[, 6]))
  data <- cbind(data, change, position) %>% as.data.frame(.) %>% mutate( ., Invest_Return = Lag.1 * change)
  data[is.na(data)] <- 0
  Total_Return <-  exp(cumsum(data['Invest_Return'])) %>% as.matrix(.)
  
  #check data
  colnames(Total_Return) <- c('Total_Return')
  data <- cbind(data, Total_Return)
  rownames(data) <- index(symbol)
  tail(data)
}



