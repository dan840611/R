library(dplyr)

df <- read.csv('/Users/Dan/Desktop/R/report_cal.csv', header = F, sep = ',')
df <- df[, -1] %>% t(.) 

data <- matrix(0, nrow = dim(df)[1], ncol = dim(df)[2])
for (i in 1:dim(df)[1]){
  for(j in 1:dim(df)[2]){
    data[i,j] <- df[i, j] %>% as.numeric(.)
  }
}
data <- as.data.frame(data)
data[99:121,1] <- df[99:121,1]
data_V1 <- data %>% group_by(V1)


View(data_V1 %>% summarise_each(funs(mean)))

#a <- data_V1 %>% summarise( V2 = mean(V2) ) 
#b <- data_V1 %>% summarise( V3 = mean(V3) )

library(dplyr)
df2 <- read.csv('/Users/Dan/Desktop/R/dupdat.csv', header = T, sep = ',')
df2_sub <- df2 %>% group_by(Subject.sequence.ID)

X1C3 <- df2_sub %>% summarise(mean_X1C3 = mean(X1C3))
X9.17 <- df2_sub %>% summarise(mean_X9.17 = mean(X9.17))
CCL61.S1 <- df2_sub %>% summarise(mean_CCL61.S1 = mean(CCL61.S1))
DXB11 <- df2_sub %>% summarise(mean_DXB11 = mean(DXB11))
df3 <- cbind(X1C3, X9.17[,2], CCL61.S1[,2], DXB11[,2])

