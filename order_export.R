library(ggplot2)
library(dplyr)
library(quantmod)
library(plotly)


order <- read.csv('/Users/Dan/Downloads/twTT.csv', header = T, sep = ',')
export <- read.csv('/Users/Dan/Downloads/twExport.csv', header = T, sep = ',')
data <- merge(order, export, by = 'Date')
colnames(data) <- c('date', 'order', 'export')

order.new <- matrix(0, nrow = dim(data)[1], 1)
export.new <- matrix(0, nrow = dim(data)[1], 1)
for (i in 1:dim(data)[1]){
  order.new[i] <- ( data[i, 2] - mean(data[1:i, 2]) ) / (sd(data[1:i, 2]))
  export.new[i] <- ( data[i, 3] - mean(data[1:i, 3]) ) / (sd(data[1:i, 3]))
}

data['order.new'] <- order.new
data['export.new'] <- export.new
data <- na.omit(data)
data['ratio'] <- data$order.new / data$export.new

ratio.new <- matrix(0, nrow = dim(data)[1], 1)
for (i in 1:dim(data)[1]){
  ratio.new[i] <- ( data[i, 6] - mean(data[1:i, 6]) ) / (sd(data[1:i, 6]))
}
data['ratio.new'] <- ratio.new %>% SMA(., n = 2)
data$date <- as.Date(data$date)
data <- na.omit(data)

#twii <- read.csv('/Users/Dan/Downloads/twii.csv', header = T, sep = ',')
#colnames(twii) <- c('date', 'value')
#data2 <- merge(data, twii, by = 'date', all.y = T, all.x = T )
#data2 <- na.locf(data2, fromLast = F) %>% na.omit(.)

p <- plot_ly(x = ~data$date, y = ~data$ratio.new, type = 'bar', name = "ratio") %>%
  layout(
    title = "TWII vs order/export",
    xaxis = list(title="date"),
    yaxis = list(title='ratio')
  )





