# Assignment 4 
rm(list = ls())
cat("\f")

## Preliminaries
library(xlsx)
library(tseries)
library(egcm)
## Part 1

#load data
datas = read.xlsx('gold_silver_price_combined.xlsx','Price Data')

goldPrices = log(datas$gold.price)
silverPrices = log(datas$silver.price)
goldReturns = goldPrices[-1]-goldPrices[-length(goldPrices)]
silverReturns = silverPrices[-1]-silverPrices[-length(silverPrices)]
dateVector = seq(1970,2017,by=1/12)

#plot data

matplot(x=dateVector,cbind(goldPrices,silverPrices),type='l',lty=1,col=c('blue','red'),xlab='Time',ylab='Log Price')

# Engle-Granger test for co-integration
adf.test(lm(goldPrices~1+silverPrices)$residual,k=0) #Run an adf test on the residual, which is Engle-Granger test


## Part 2

rm(list = ls())
cat("\f")
library(xlsx)
library(tseries)
library(egcm)
library(fGarch)
library(FinTS)
library(rugarch)
library(devtools)
datas = read.xlsx('Shanghai_SE_composite_price.xlsx','Price Data')
rets = log(datas$close)[-length(datas$close)]-log(datas$close)[-1]



# Test for ARCH effects in stock returns
ArchTest(rets,lags = 1,demean = T)

mrets = rets-mean(rets)


# GARCH model for returns modeling mean of returns as a constant
spec1 = ugarchspec(variance.model = list(model='fGARCH', submodel = 'GARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=F),distribution.model = "norm")
fgarch.fit = ugarchfit(spec1,mrets) #estimated model

spec2 = ugarchspec(variance.model = list(model='eGARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=T),distribution.model = "norm")
egarch.fit = ugarchfit(spec2,rets)

spec3 = ugarchspec(variance.model = list(model='fGARCH', submodel = 'GJRGARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=T),distribution.model = "norm")
gjrgarch.fit = ugarchfit(spec3,rets)

forecast(gjrgarch.fit)

library(prophet)

prdf <- data.frame(ds=as.Date(character()),
                 y=character(), 
                 stringsAsFactors=FALSE) 

prdf <- datas
prdf$y <- datas$close
prdf$ds <- datas$Date

m <- prophet(prdf)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


forecast$id <- c(as.factor(forecast$t))

fc2 <- forecast[ which(forecast$id >6337 & forecast$id<6362), ]

plot(fc2$ds, fc2$yhat)

sd(forecast$yhat)


fc2$act <- c(3135.921,
               3158.794,
               3165.411,
               3154.321,
               3171.236,
               3161.671,
               3136.754,
               3119.289,
               3112.764,
               3103.428,
               3108.775,
               3113.012,
               3101.299,
               3123.139,
               3136.775,
               3142.553,
               3149.554,
               3159.166,
               3140.17,
               3156.984,
               3153.088,
               3166.982,
               3183.179,
               3196.699)

##fgarch.fit$

library(plotrix)

plot(x = fc2$ds, y = fc2$act)

plotCI(fc2$ds, fc2$yhat, uiw=1.96*sd(fc2$yhat),liw=1.96*sd(fc2$yhat), scol='blue')
plotCI(fc2$ds, fc2$yhat, uiw=0.67*sd(fc2$yhat),liw=0.67*sd(fc2$yhat), scol='red', add=TRUE)
points(x = fc2$ds, y = fc2$act)
points(x = fc2$ds, y = fc2$yhat)

plotCI(fc2$ds, fc2$yhat, uiw=1.96*sd(fc2$yhat),liw=1.96*sd(fc2$yhat), scol='blue', ylim=c(3100,3400))
plotCI(fc2$ds, fc2$yhat, uiw=0.67*sd(fc2$yhat),liw=0.67*sd(fc2$yhat), scol='red', add=TRUE)
plotCI(fc2$ds, fc2$act, uiw=0,liw=0, add=TRUE, col='green')



