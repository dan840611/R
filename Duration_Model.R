library(dplyr)

#Duration(amount, maturity, coupon rate, yield to maturity, times per year)
Duration <- function(amount, m, c, y, frequency){
  n <- m * frequency
  payment <- (amount * c) / (frequency * 100)
  CF = c(rep(payment, n-1), amount+payment)
  data <- data.frame(period = 1:n, CF = CF, year = (1:n)/frequency)
  data <- mutate(data, DCF = CF/ ((1+y/100)^year) )
  data <- mutate(data, DCFxT = DCF * year)
  data <- mutate(data, duration = DCFxT/ sum(data['DCF']))
  print(data)
  duration = sum(data['duration'])
  paste0( 'duration = ', duration)
}

#Perpentuity：price = payment/ytm, duration = (1 + ytm)/ ytm
amount = 1000
coupon = 0.08
ytm = 0.05
price = (amount * coupon) / ytm
duration = (1 + ytm)/ ytm