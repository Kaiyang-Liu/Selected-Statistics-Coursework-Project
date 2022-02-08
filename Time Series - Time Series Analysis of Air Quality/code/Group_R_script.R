### Time Series Analysis
### Group Project
### May,24,2021 18:00

### To Do List: 
# 1. Code: structure rearranging, need train and test?
# 2. residual plot need to be modified exp smoothing + Model 3 4
# 3. Ljung-Box Test Model 3 4
# 4. Forecast(Model 3 4) and model comparison, visualization

### Part I Data Preprocessing and Analysis

library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(openair)
library(stats)
library(dplyr)
library(TSA)
library(lmtest)
library(tseries)  
library(forecast)
library(urca)

AQI <- read_csv("AQI.csv")
shanghai  <- read_csv("shanghai-air-quality.csv")
shanghai$date <- as.Date(shanghai$date, format = "%Y/%m/%d")
View(AQI)
View(shanghai)
summaryPlot(shanghai)

date <- as.Date(AQI$date,"%Y/%m/%d")
a <- AQI$AQI
com <- data.frame(date = date, aqi = a)
summary(a)

aqits <- ts(AQI$AQI,start = 2014-01-01,frequency = 365)
plot.ts(aqits)

ggplot(data = com,aes(x = aqi))+geom_histogram(aes(y=..density..), binwidth=20, colour="black", fill="white")+geom_density(alpha=.5, fill="lightblue") + theme_calc()
ggplot(data = com)+geom_boxplot(aes(y = aqi),fill = "lightblue")+theme_calc()

winter <- com[month(com$date)==12|month(com$date)==1|month(com$date)==2,]
spring <- com[month(com$date)==3|month(com$date)==4|month(com$date)==5,]
summer <- com[month(com$date)==6|month(com$date)==7|month(com$date)==8,]
fall <- com[month(com$date)==9|month(com$date)==10|month(com$date)==11,]
season <- c(rep('3-5 spring',time = dim(spring)[1]),rep('6-8 summer',time = dim(summer)[1]),rep('9-11 fall',time = dim(fall)[1]),rep('12-2 winter',time = dim(winter)[1]))

cc <- rbind.data.frame(spring,summer,fall,winter)
cc <- cbind.data.frame(cc,season)

co <- c("#6699cc","#99cc66","#d2d279","#cc9966")
ggplot()+ geom_violin(data = cc, aes(x = season,y = aqi, fill = season)) + theme_calc()+scale_fill_manual(values = co) 

AQI$date <- as.Date(AQI$date,format = "%Y/%m/%d")
moth <- AQI%>% mutate(month = format(date,"%Y/%m")) %>% group_by(month) %>% summarise(mean_by_month = mean(AQI)) 
mots <- ts(moth$mean_by_month,frequency = 12,start = 2014/01)
plot(mots)


#holt-winter
fi1 <- hw(mots,seasonal = "additive")
fi2 <- hw(mots,seasonal = "multiplicative")

#forecast holt-winter
autoplot(mots) +autolayer(fi1, series="HW additive forecasts")  + xlab("Year") +ylab("AQI") +guides(colour=guide_legend(title="Forecast"))
autoplot(mots) +autolayer(fi2, series="HW multiplicative forecasts") + xlab("Year") +ylab("AQI") +guides(colour=guide_legend(title="Forecast"))

#4 in 1 residual plot
par(mfrow=c(2,2))
plot(fi1$residuals, main = "Residuals")
title(sub='(a)') 
plot(fi1$fitted,fi1$residuals[1:length(fi1$residuals)],xlab='Fitted value', ylab='Residual')
title(sub='(b)') 
hist(fi1$residuals)
title(sub='(c)') 
qqnorm(fi1$residuals);
qqline(fi1$residuals)
title(sub='(d)')

#Ljung-Box Test
Box.test(fi1$residuals,lag = 1,type = "Ljung-Box")
Box.test(fi2$residuals,lag = 1,type = "Ljung-Box")
accuracy(fi1)
accuracy(fi2)


### Part III ARIMA Model
mots <- ts(moth$mean_by_month,frequency = 12,start = 2014/01)
adf.test(mots,alt = "stationary")
par(mfrow = c(1,2))
acf(as.vector(mots))
pacf(as.vector(mots))

diff_seasonal = diff(mots,lag = 12)
plot.ts(diff_seasonal)
par(mfrow = c(1,2))
acf(as.vector(diff_seasonal))
pacf(as.vector(diff_seasonal))


diff = diff(diff_seasonal,difference = 1)
plot.ts(diff)
par(mfrow = c(1,2))
acf(as.vector(diff))
pacf(as.vector(diff))


#optimal model
aics <- matrix(0,6,6,dimnames = list(p = 0:5, q = 0:5))

for (q in 1:5){
  aics[1,1+q] <- arima(mots,c(0,1,q))$aic
}

for (p in 1:5){
  for (q in 0:5){
    aics[1+p,1+q] <- arima(mots,c(p,1,q))$aic
  }
}

aics

Model1 <- forecast::Arima(mots,order=c(4,1,5),method="ML")
Model2 <- auto.arima(mots)
Model3 <- forecast::Arima(diff,order = c(4,0,3),method = "ML")
Model4 <- forecast::Arima(diff,order = c(3,0,3),method = "ML")


#Residual
#Model1
n = length(mots)
x = seq(1,n)
fitted1 = as.vector(Model1$fitted)
par(mfrow=c(2, 2))
qqnorm(resid(Model1), main = NA,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(resid(Model1))     
title(sub="(a) NPP of residuals")

plot(fitted1, resid(Model1), xlab="Fitted value", ylab="Residual")
points(fitted1, rep(0, n),type = "l")
title(sub="(b) Residual vs. fitted")

hist(resid(Model1), xlab="Residual", main=NA)
title(sub="(c) Histogram of residuals")

plot(x, resid(Model1), type='l', xlab="Observation order", ylab="Residual")
points(x, resid(Model1), pch=19)
points(x, rep(0, n), type='l')
title(sub="(d) Residual vs. order")

#Model2
n = length(mots)
x = seq(1,n)
fitted2 = as.vector(Model2$fitted)
par(mfrow=c(2, 2))
qqnorm(resid(Model2), main = NA,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(resid(Model2))     
title(sub="(a) NPP of residuals")

plot(fitted2, resid(Model2), xlab="Fitted value", ylab="Residual")
points(fitted2, rep(0, n),type = "l")
title(sub="(b) Residual vs. fitted")

hist(resid(Model2), xlab="Residual", main=NA)
title(sub="(c) Histogram of residuals")

plot(x, resid(Model2), type='l', xlab="Observation order", ylab="Residual")
points(x, resid(Model2), pch=19)
points(x, rep(0, n), type='l')
title(sub="(d) Residual vs. order")

#Model3 problems
n = length(diff)
x = seq(1,n)
fitted3 = as.vector(Model3$fitted)
par(mfrow=c(2, 2))
qqnorm(resid(Model3), main = NA,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(resid(Model3))     
title(sub="(a) NPP of residuals")

plot(fitted3, resid(Model3), xlab="Fitted value", ylab="Residual")
points(fitted3, rep(0, n),type = "l")
title(sub="(b) Residual vs. fitted")

hist(resid(Model3), xlab="Residual", main=NA)
title(sub="(c) Histogram of residuals")

plot(x, resid(Model3), type='l', xlab="Observation order", ylab="Residual")
points(x, resid(Model3), pch=19)
points(x, rep(0, n), type='l')
title(sub="(d) Residual vs. order")

#Model4 problems
n = length(diff)
x = seq(1,n)
fitted4 = as.vector(Model4$fitted)
par(mfrow=c(2, 2))
qqnorm(resid(Model4), main = NA,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(resid(Model4))     
title(sub="(a) NPP of residuals")

plot(fitted4, resid(Model4), xlab="Fitted value", ylab="Residual")
points(fitted4, rep(0, n),type = "l")
title(sub="(b) Residual vs. fitted")

hist(resid(Model4), xlab="Residual", main=NA)
title(sub="(c) Histogram of residuals")

plot(x, resid(Model4), type='l', xlab="Observation order", ylab="Residual")
points(x, resid(Model4), pch=19)
points(x, rep(0, n), type='l')
title(sub="(d) Residual vs. order")


#Ljung-Box Test and dwtest
#Model1
my.residual = resid(Model1)
Box.test(my.residual, lag = 12, type = c("Ljung-Box"))
dwtest(my.residual ~ fitted(Model1))


#Model2
my.residual = resid(Model2)
Box.test(my.residual, lag = 12, type = c("Ljung-Box"))
dwtest(my.residual ~ fitted(Model2))


#Model3
my.residual = resid(Model3)
Box.test(my.residual, lag = 12, type = c("Ljung-Box"))
dwtest(my.residual ~ fitted(Model3))


#Model4
my.residual = resid(Model4)
Box.test(my.residual, lag = 12, type = c("Ljung-Box"))
dwtest(my.residual ~ fitted(Model4))


#eacf
eacf(mots)

#forecast
windows()
par(mfrow = c(2,1))
plot(forecast(Model1,h=12,level=c(99.5)))
plot(forecast(Model2,h=12,level=c(99.5)))
plot(forecast(Model3,h=12,level=c(99.5)))
plot(forecast(Model4,h=12,level=c(99.5)))