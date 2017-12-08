library(quantmod)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Ryacas)

#Mean Variance Theory : MVT('^AAPL', 'TWII')

MVT <- function(code1, code2, riskfree){
  x <- getSymbols(code1)
  y <- getSymbols(code2)
  code1 <- eval(parse(text = x))
  code2 <- eval(parse(text = y))
  
  # 去除na值
  code1 <- na.omit(code1) %>% as.data.frame(.)
  code2 <- na.omit(code2) %>% as.data.frame(.)
  
  # 將兩資料長度化為相同
  n <- ifelse( dim(code1)[1] < dim(code2)[1], dim(code1)[1], dim(code2)[1])
  row1 <- index(code1) %>% tail(., n - 1)
  row2 <- index(code2) %>% tail(., n - 1)
  
  #計算日報酬率
  code1 <- mutate(code1, change = ROC(code1[, 6], type = 'discrete')) %>% tail(., n - 1)
  code2 <- mutate(code2, change = ROC(code2[, 6], type = 'discrete')) %>% tail(., n - 1)
  rownames(code1) <- row1
  rownames(code2) <- row2
  
  #日報酬率平均
  expectation <- matrix(c(mean(code1[['change']], na.rm = T), mean(code2[['change']], na.rm = T)), nrow = 2, ncol = 1)
  
  #組合兩資產報酬率資料
  r1 <- code1['change'] %>% as.matrix(.)
  r2 <- code2['change'] %>% as.matrix(.)
  R <- cbind(r1, r2)
  colnames(R) <- c('Asset1', 'Asset2')
  
  #統計兩資產
  ER1 <- mean(r1)
  Std1 <- sd(r1)
  ER2 <- mean(r2)
  Std2 <- sd(r2)
  Cov12 <- cov(r1, r2)

  #計算個組合下的投資組合報酬率、變異數及標準差
  w1 <- as.matrix(runif(100))
  w2 <- 1-w1
  content <- matrix(0, nrow = dim(w1)[1], ncol = 3)
  data <- cbind(w1, w2, content)
  colnames(data) <- c('weight1', 'weight2', 'ERp', 'Varp', 'Std')
  
  for(i in 1:dim(w1)[1]){
    data[i, 'ERp'] <- data[i, 1:2] %*% expectation
    data[i, 'Varp'] <- t(as.matrix(data[i, 1:2])) %*% cov(R) %*% as.matrix(data[i, 1:2])
    data[i, 'Std'] <- data[i, 'Varp']^(1/2)
  }
  data <- as.data.frame(data)
  
  #計算global minimun variance下的組合
  riskfree <- riskfree/25200
  gmv1 <- (  (Std2^2) -  Cov12 )/(  (Std2^2) +  (Std1^2) +2 * Cov12)
  gmv2 <- 1-gmv1
  gmvw <- matrix(c(gmv1, gmv2))
  gmv <- c ( (t(gmvw) %*% cov(R) %*% gmvw)^0.5, t(gmvw) %*% expectation )
  
  #計算切線CAL
  #ER1 <- ER1 - riskfree
  #ER2 <- ER2 - riskfree
  #t1 <- (ER1 * Std2^2 - ER2 * Cov12) / (ER1 * Std2^2 + ER2 * Std1^2 - (ER1 + ER2) *Cov12)
  #t2 <- 1 - t1
  #tw <- matrix(c(t1,t2))
  #t <- c(t(tw) %*% cov(R) %*% tw, t(tw) %*% expectation)

  #作圖
  p <- ggplot(data, aes( x = data[,5], y = data[,3])) + geom_point() + xlab('Std') + ylab('ERp') + ggtitle('Investment Frontier') + theme_economist() 
  p.1 <- p + geom_abline(intercept = riskfree, slope = (t[2]-riskfree)/t[1], color = 'red', linetype = 'dashed', size = 1.5) + xlim(c(0, max(t[1],data$Std))) + ylim(c(0, max(t[2],data$ERp)))
  return(p)
}

