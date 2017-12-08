#### MM TWD Index creation
library(nortest)
library(stats)
library(MASS)
library(rpart)
library(randomForest)
library(ggplot2)
library(dplyr)
library(gvlma)
library(lubridate)
library(xlsx)
library(xts)

MMTWD <- read.xlsx("mmtwdtest.xlsx", sheetIndex = 1)
class(MMTWD)
mode(MMTWD)
MMTWD$date <- as.character(MMTWD$date)
MMTWD$date <- as.Date(MMTWD$date, "%m/%d/%Y")
MMTWD$date <- ymd(MMTWD$date) # This is better way.
str(MMTWD)
summary(MMTWD)

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

MMTWD1 <- delete.na(MMTWD) # remove NAs at 2017-03-01

## Check the normality of original values:
qqnorm(MMTWD1$twd) # abnormal
shapiro.test(MMTWD1$twd)
hist(MMTWD1$twd, breaks = seq(24, 42, 1), prob = TRUE)
curve( dnorm(x, mean(MMTWD1$twd), sd(MMTWD1$twd)), 24, 42, add = TRUE, col = "red" )

qqnorm(MMTWD1$kwd) # abnormal
shapiro.test(MMTWD1$kwd)
hist(MMTWD1$kwd, breaks = seq(650, 1700, 50), prob = TRUE)
curve( dnorm(x, mean(MMTWD1$kwd), sd(MMTWD1$kwd)), 650, 1700, add = TRUE, col = "red" )

qqnorm(MMTWD1$signal) # abnormal
shapiro.test(MMTWD1$signal)
hist(MMTWD1$signal, breaks = seq(9, 41, 2), prob = TRUE)
curve( dnorm(x, mean(MMTWD1$signal), sd(MMTWD1$signal)), 9, 41, add = TRUE, col = "red" )

qqnorm(MMTWD1$exportyoy) # abnormal
shapiro.test(MMTWD1$exportyoy)
hist(MMTWD1$exportyoy, breaks = seq(-46, 76, 2), prob = TRUE)
curve( dnorm(x, mean(MMTWD1$exportyoy), sd(MMTWD1$exportyoy)), -46, 76, add = TRUE, col = "red" )

qqnorm(MMTWD1$mm) # abnormal
shapiro.test(MMTWD1$mm)
hist(MMTWD1$mm, breaks = seq(-42, 36, 3), prob = TRUE)
curve( dnorm(x, mean(MMTWD1$mm), sd(MMTWD1$m)), -42, 36, add = TRUE, col = "red" )

# Examples for multiple freqpoly and histogram: 
# df with columns Date+VIX+yy, yy = a, b, c
# df$yy <- c(rep(letters[1], 2528), rep(letters[2], 2515), rep(letters[3], 1856))

ggplot(df,aes(x=VIX)) +
  +     geom_freqpoly(data=subset(df,yy == 'a'),col = "red", size = 2, alpha = 0.6, binwidth = 0.5) +
  +     geom_freqpoly(data=subset(df,yy == 'b'),col = "blue", size = 2, alpha = 0.6, binwidth = 0.5) +
  +     geom_freqpoly(data=subset(df,yy == 'c'),col = "green", size = 2, alpha = 0.6, binwidth = 0.5) + labs(title = "Frequency histogram of VIX", subtitle = "90~99 / 00~10 / 11~17", caption = "(based on data from MacroMicro)") + theme_bw()

ggplot(df,aes(x=VIX)) +
  +     geom_histogram(data=subset(df,yy == 'a'),fill = "red", alpha = 0.7, binwidth = 0.5) +
  +     geom_histogram(data=subset(df,yy == 'b'),fill = "blue", alpha = 0.7, binwidth = 0.5) +
  +     geom_histogram(data=subset(df,yy == 'c'),fill = "green", alpha = 0.7, binwidth = 0.5) + labs(title = "Frequency histogram of VIX", subtitle = "90~99 / 00~10 / 11~17", caption = "(based on data from MacroMicro)") + theme_bw()

## Creating the Z-score Values:
# By data.frame MMTWD1
MMTWD1$ztwd <- (MMTWD1$twd - mean(MMTWD1$twd)) / sd(MMTWD1$twd)
MMTWD1$zkwd <- (MMTWD1$kwd - mean(MMTWD1$kwd)) / sd(MMTWD1$kwd)
MMTWD1$zsignal <- (MMTWD1$signal - mean(MMTWD1$signal)) / sd(MMTWD1$signal)
MMTWD1$zexportyoy <- (MMTWD1$exportyoy - mean(MMTWD1$exportyoy)) / sd(MMTWD1$exportyoy)
MMTWD1$zmm <- (MMTWD1$mm - mean(MMTWD1$mm)) / sd(MMTWD1$mm)

summary(MMTWD1)

# By data.frame TWD (MMTWD1 + stock & unemp data)
TWD <- read.xlsx("mmtwdtest.xlsx", sheetIndex = 1)
TWD <- delete.na(TWD)
TWD$date <- ymd(TWD$date)
TWD$logstock <- log(TWD$stock)
TWD$logunemp <- log(TWD$unemp)

TWD$ztwd <- (TWD$twd - mean(TWD$twd)) / sd(TWD$twd)
TWD$zkwd <- (TWD$kwd - mean(TWD$kwd)) / sd(TWD$kwd)
TWD$zlogstock <- (TWD$logstock - mean(TWD$logstock)) / sd(TWD$logstock)
TWD$zlogunemp <- (TWD$logunemp - mean(TWD$logunemp)) / sd(TWD$logunemp)
TWD$zexportyoy <- (TWD$exportyoy - mean(TWD$exportyoy)) / sd(TWD$exportyoy)

View(TWD)
summary(MMTWD1)

## Constrained Linear Regression: (Sum of Coefs = 1, incl. intercept )
## Equal to Quadratic Programming
# By data.frame MMTWD1
library(quadprog)
X <- cbind(MMTWD1$zkwd, MMTWD1$zsignal, MMTWD1$zexportyoy, MMTWD1$zmm)
XX <- cbind(1, X) # Independent Variables Matrix: 397*5

Y <- MMTWD1$ztwd # Dependent Variable Matrix: 397*1

dd <- t(Y)%*%XX # 1*5

Dmat <- t(XX)%*%XX # 5*5

C <- cbind(rep(1,4), diag(4)) # 4*5

b <- c(1,rep(0,4)) # 1*5

Amat <- t(cbind(0,rbind(1,diag(4)))) # 5*5

solve.QP(Dmat = Dmat, factorized = FALSE, dvec = dd, Amat = Amat, bvec = b, meq = 1)

# By data.frame TWD
# Model with ztwd, zkwd, zlogstock, zlogunemp, zexportyoy
X1 <- cbind(TWD$zkwd, TWD$zlogstock, TWD$zlogunemp, TWD$zexportyoy)
XX1 <- cbind(1, X1) # Independent Variables Matrix: 397*5
Y1 <- TWD$ztwd # Dependent Variable Matrix: 397*1
dd1 <- t(Y1)%*%XX1 # 1*5
Dmat1 <- t(XX1)%*%XX1 # 5*5
C1 <- cbind(rep(1,4), diag(4)) # 4*5
b1 <- c(1,rep(0,4)) # 1*5
Amat1 <- t(cbind(0,rbind(1,diag(4)))) # 5*5
solve.QP(Dmat = Dmat1, factorized = FALSE, dvec = dd1, Amat = Amat1, bvec = b1, meq = 1)

TWD$constrained1 <- (2.631404e-01)*TWD$zkwd - (2.889704e-19)*TWD$zlogstock + (4.692054e-01)*TWD$zlogunemp + (2.676542e-01)*TWD$zexportyoy - 6.292259e-16
TWD$lm1 <- (3.477056e-01)*TWD$zkwd - (7.658432e-01)*TWD$zlogstock + (4.296698e-01)*TWD$zlogunemp + (2.906298e-02)*TWD$zexportyoy - 8.496310e-16

# Model with ztwd, zkwd, zlogstock, zlogunemp
X2 <- cbind(TWD$zkwd, TWD$zlogstock, TWD$zlogunemp)
XX2 <- cbind(1, X2)
Y2 <- TWD$ztwd
dd2 <- t(Y2)%*%XX2
Dmat2 <- t(XX2)%*%XX2
C2 <- cbind(rep(1,3), diag(3))
b2 <- c(1,rep(0,3))
Amat2 <- t(cbind(0,rbind(1,diag(3))))
solve.QP(Dmat = Dmat2, factorized = FALSE, dvec = dd2, Amat = Amat2, bvec = b2, meq = 1)

TWD$constrained2 <- (3.138054e-01)*TWD$zkwd - (6.408480e-19)*TWD$zlogstock + (6.861946e-01)*TWD$zlogunemp  - 7.711714e-16
TWD$lm2 <- (3.357347e-01)*TWD$zkwd - (7.687928e-01)*TWD$zlogstock + (4.347101e-01)*TWD$zlogunemp - 8.518026e-16

## Create the shift effect:(Incl. Lead or Lag)
# -2: Lead the 2 periods / +3: Lag the 3 periods
# before t0 is Lag / after to is Lead 
glm1 <- glm(ztwd ~ shift(zkwd, 0) + shift(zlogstock, 0)+ shift(zlogunemp, 3) + shift(zexportyoy, 3), data = TWD)
plot(glm1)

# Compute the Weibull distribution's parameter:
# Blischke-Scheuer method-of-moments estimation of (a,b)
# xbar = mean(x)
# varx = variance(x)
# var("b"); f(b) = gamma(1+2/b)/gamma(1+1/b)^2 - 1 - varx/xbar^2
# bhat = find_root(f, 0.01, 100)
# ahat = xbar/gamma(1+1/bhat)
# print "Estimates: (ahat, bhat) = ", (ahat, bhat)
# for the Weibull distribution F(t) = 1 - exp(-(t/a)^b)
fit_weibull <- function(x)
{
  xbar <- mean(x)
  varx <- var(x)
  f <- function(b){return(gamma(1+2/b)/gamma(1+1/b)^2 - 1 - varx/xbar^2)}
  bhat <- uniroot(f,c(0.02,50))$root
  ahat <- xbar/gamma(1+1/bhat)
  return(c(ahat,bhat))
}

fit_weibull(oil2$Value) # (341484.77158, 8.43228) = (ahat, bhat) 

## HP filter on ztwd:
library(mFilter)
opar <- par(no.readonly=TRUE)
ztwd.hp <- hpfilter(MMTWD1$ztwd, freq = 14400)
plot(ztwd.hp)

## Changing frequencies of timeseries data set:
oil1 <- as.xts(oil[,-1], oil[,1]) # dataframe to xts, 4 columns
# weekly to monthly
month.end <- endpoints(oil1, on = "months")
monthly.last <- period.apply(oil1[,1], INDEX = month.end, FUN = last)
head(weekly.last, 5) # convert only 1 column in everytime

monthly.mean <- period.apply(oil1[,4], INDEX = month.end, FUN = mean)
head(weekly.mean, 5) # If data has p columns, then you should do it p times with different columns.

## Output the data.frame:
write.xlsx(x = MMTWD1, file = "mmtwdtest3.xlsx", sheetName = "MMTWD", row.names = FALSE)
write.xlsx(x = TWD, file = "mmtwdtest3.xlsx", sheetName = "MMTWD", row.names = FALSE)