
library(tseries) 
library(ggplot2) 
library(TSA) 
library(fUnitRoots) 
library("MapeBay") 
library(lmtest) 
library(forecast) 
library(readr) 
library(MLmetrics) 
library(gtools)


data <- read.csv("C:/Users/bhaskarnani/OneDrive/Documents/unp project2 dataset1.csv") 
View(data) 

data 

#converting data frame to time series
sunspots.ts <- matrix(data$Sunspots, nrow = 2820, ncol = 1)
sunspots.ts <- as.vector(t(sunspots.ts))
sunspots.ts <- ts(sunspots.ts,start=c(1,1), end=c(3,12), frequency=12)

plot(ts(as.vector(sunspots.ts)),type='o',ylab='sunspots')
plot(decompose(sunspots.ts))
plot.ts(sunspots)

#line graph
plot(data$Sunspots,type="o")

#ggplot
ggseasonplot(sunspots.ts)

#auto-correlation function
acf(sunspots.ts, lag.max = 2820)

#partial auto-correlation function
pacf(sunspots.ts,lag.max = 2820)

#augmented dickey-fuller test
adf.test(sunspots.ts,alternative = c("stationary", "explosive"), 
         k = trunc((length(sunspots.ts)-1)^(1/3)))


#checking seasonality when D=0
dif=diff(sunspots.ts)
plot(dif)
adf.test(dif)


#checking seasonality when D=1
diff.sunspots.1 = diff(dif)
plot(diff.sunspots.1,type='o',ylab='count of sunspots')
adf.test(diff.sunspots.1)

par(mfrow=c(1,1))
res2 = armasubsets(y=diff.sunspots.1,nar=10,nma=10,y.name='test',ar.method='ols')

plot(res2)
length(data$Sunspots)

#train and test
train <- ts(sunspots[1:2808],start=1749,frequency = 12)
train
test <- ts(sunspots[2809:2812],start=1983,frequency = 12)
test


#prediction

arima_212_ml <- arima(sunspots.ts,order=c(2,1,2),method='ML')
coeftest(arima_212_ml)
auto.arima(sunspots.ts)
fit=Arima(sunspots.ts,c(2,1,2))
plot(forecast(fit,h=10))
fit
predict(arima_212_ml,n.ahead = 12,newxreg = NULL,se.fit=TRUE)
pred<-predict(arima_212_ml,n.ahead = 12,newxreg = NULL,se.fit=TRUE)

auto.arima(train)
fit=Arima(train,c(2,1,2))
#plot(forecast(fit,h=10))
f1 <- predict(fit,4)
f1

#MAPE

MAPE(test,f1$pred)


f2 <- predict(fit,124)
answer <- f2$pred[5:124]
final <- ts(answer,start = 1984, frequency = 12)
final
