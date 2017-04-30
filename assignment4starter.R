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
dateVector = seq(1970,2016+1/12,by=1/12)

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
rets = log(datas$Close)[-length(datas$Close)]-log(datas$Close)[-1]



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
