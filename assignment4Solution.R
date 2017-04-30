# Assignment 4 
rm(list = ls())
cat("\f")

## Preliminaries
library(xlsx)
library(tseries)
library(egcm)
#### Part 1####

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
# The null hypothesis is no cointegration
lm(goldPrices~1+silverPrices) # The slope of conintegrating vector should be roughly 1

# Tests for Granger causality using approximation that slope of cointegrating vector is 1

maxLags = 12

y = goldReturns
for (ii in 1:maxLags) {
  X = embed(cbind(goldReturns,silverReturns),ii+1)[,-(1:2)]
  errorCorrection = goldPrices-silverPrices
  errorCorrection = errorCorrection[c(-(1:(ii)),-length(errorCorrection))]
  stats = summary(lm(y[-(1:ii)]~1+X+errorCorrection))
  print(stats$coefficients)
  print(stats$adj.r.squared)
}


y = silverReturns
for (ii in 1:maxLags) {
  X = embed(cbind(goldReturns,silverReturns),ii+1)[,-(1:2)]
  errorCorrection = goldPrices-silverPrices
  errorCorrection = errorCorrection[c(-(1:(ii)),-length(errorCorrection))]
  stats = summary(lm(y[-(1:ii)]~1+X+errorCorrection)) 
  print(stats$coefficients)
  print(stats$adj.r.squared)
}

#### Part 2 ####

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

# Plot

plot(rets,type='l')

plot(rets^2,type='l')

#autocorrelation of daily stock returns
acf(rets)

#autocorrelation of daily squared stock returns
acf(rets^2)

# Test for ARCH effects in stock returns
ArchTest(rets,lags = 1,demean = T)

mrets = rets-mean(rets)


# GARCH model for returns modeling mean of returns as a constant
spec1 = ugarchspec(variance.model = list(model='fGARCH', submodel = 'GARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=F),distribution.model = "norm")
fgarch.fit = ugarchfit(spec1,mrets) #estimated model
infocriteria(fgarch.fit)           #information criteria

spec2 = ugarchspec(variance.model = list(model='eGARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=T),distribution.model = "norm")
egarch.fit = ugarchfit(spec2,rets)
infocriteria(egarch.fit)

spec3 = ugarchspec(variance.model = list(model='fGARCH', submodel = 'GJRGARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=T),distribution.model = "norm")
gjrgarch.fit = ugarchfit(spec3,rets)
infocriteria(gjrgarch.fit)

# Forecasting exercise

smallerSample = mrets[1:6097] # use data only up to 12/31/2013
garch.fit = ugarchfit(spec1,smallerSample)

forec = ugarchforecast(garch.fit,n.ahead = 40)
plot(abs(mrets[6098:(6098+26)]),type='l',xlim=c(0,40))
lines(coredata(sigma(forec)))
