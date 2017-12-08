install.packages("lubridate")
library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)

#read the file
export <- read.csv(file.choose(), header = TRUE)

#做出年
date.extract <- ymd(export$Date)
year <- year(date.extract)
year.export <- cbind(year, export[, 2:3])

#shift function
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

#整理
year.export['Shift'] <- year.export[['Value']] %>% as.numeric(.) %>% shift(., -12)
year.export <- na.omit(year.export)
year.export <- mutate(year.export, YoY = (Value / Shift) * 100 -100)
export.matrix <- dcast(year.export, year ~ X, value.var = "YoY")  
View(export.matrix)

#5年一組計算平均值
n = 5
group <- rep(1:ceiling(nrow(export.matrix) / n), each = n, length = nrow(export.matrix))
export.matrix2 <- cbind(export.matrix, group)
colnames(export.matrix2) <- c('Year','Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Group')
rownames(export.matrix2) <- export.matrix2$Year
export.matrix2 <- export.matrix2[, -1]

z <- matrix(nrow=13, ncol=7)

for (i in 1:ceiling(nrow(export.matrix) / n)){
  tmp <- subset(export.matrix2, export.matrix2$Group == i) %>% colMeans(.) %>% as.matrix(.)
  z[, i] <- tmp
}

z <- z[-13, ] %>% t(.)
colnames(z) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
rownames(z) <- c('1984-1988', '1989-1993', '1994-1998', '1998-2002', '2003-2007', '2008-2012', '2013-2017')
result <- melt(z)

#做圖
library(ggthemes)

ggplot(result, aes(x=Var2, y=value)) +
  geom_line(aes(colour=Var1, group=Var1), size=.3) +
  geom_point(size=1) +
  xlab("Month") + ylab("YoY, %") +
  ggtitle("Taiwan Export Order") +
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill="transparent")) +
  theme(axis.line = element_line(size=.3, color = "black")) +
  theme_economist()  

#write.csv(export.matrix, "export_matrix.csv")
setwd('/Users/Dan/Desktop')
ggsave('yoy.png')


#循環分析
library(TSA)
p = periodogram(year.export$YoY)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top10 = head(order, 10)
time = 1/top10$f
