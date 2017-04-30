## Assignment 3

rm(list = ls())
cat("\f")

## Preliminaries
library(xlsx)
library(tseries)
library(vars)
# Read in data

temp = read.xlsx('HongKong_property_index_combined.xlsx','Price Data',1)

# Use largest sample containing all 5 variables

estimationStart = 126
estimationEnd = 366
datas = temp[estimationStart:estimationEnd,-1]
datas = as.matrix(datas)
TT = nrow(datas)

# Construct individual series

propIndex = datas[,1]
cpi = datas[,2]
govYield = datas[,3]
uRate = datas[,4]
exRate = datas[,5]

# Construc date vector
timeVector = seq(1994+8/12,2010+10/12,along.with = cpi)

# Plot Series

plot(timeVector,propIndex,xlab = 'Time',ylab = 'Price Index',main = 'Hong Kong property price index',type = 'l')
plot(timeVector,cpi,xlab = 'Time',ylab = 'Price Index',main = 'Hong Kong consumer price index',type = 'l')
plot(timeVector,govYield,xlab = 'Time',ylab = 'Percentage Points',main = 'Hong Kong 5-year government bond yields',type = 'l')
plot(timeVector,uRate,xlab = 'Time',ylab = 'Percentage Points',main = 'Hong Kong unemployment rate',type = 'l')
plot(timeVector,exRate,xlab = 'Time',ylab = 'Percentage Points',main = 'Hong Kong - US Dollar exchange rate',type = 'l')

# Variables that need diffreencing
propReturns = log(propIndex[-1])-log(propIndex[-length(propIndex)])
infl = log(cpi[-1])-log(cpi[-length(cpi)])

# Adjust sample size of other variables
govYield = govYield[-1]
uRate = uRate[-1]
exRate = exRate[-1]

TT = TT-1
timeVector = timeVector[-1]

## Part 1

# Estimate various ARMA models

maxLags =4
informationCriteria = matrix(0,(maxLags+1)^2-1,2)
modelList = matrix(0,(maxLags+1)^2-1,2)
curModel = 1

for (p in 0:maxLags) {
  for (q in 0:maxLags) {
    if(p!=0 || q!=0 ){
      modelList[curModel,] = matrix(c(p,q),nrow = 1,ncol = 2)
      model = arima(propReturns[-seq(1:max(p,q))],order = c(p,0,q))
      informationCriteria[curModel,] = matrix(c(AIC(model),BIC(model)),1,2)
      curModel = curModel+1
    }
    
  }
  
}

# Find best fitting models according to AIC and BIC
minIndices = apply(informationCriteria, 2, which.min)

# Display p and q lags of best fitting models

bestModel = modelList[minIndices,]

# Fit best AIC model

model = arima(propReturns[-seq(1:max(p,q))], order = c(bestModel[1],0,bestModel[2]))

# Compute fitted residuals
eHat = model$residuals

plot(timeVector[-seq(1:max(p,q))],eHat,type = 'l',xlab = 'Time',ylab = 'Fitted Residuals')

# Check autocorrelations
acf(eHat)

# LBQ test using 12 lags (since monthly data)

Box.test(eHat, lag = 12)


# Part 2

# Set up for multivariate model using all predictors

y = propReturns[-1]
X = cbind(infl[-length(infl)],govYield[-length(govYield)],uRate[-length(uRate)],exRate[-length(exRate)])
model = lm(y~1+X)

# Check autocorrelation

acf(model$residuals)

# Durbin Watson test for serial correlation

dwtest(y ~ 1+X)

# LBQ test using 12 lags

Box.test(model$residuals,lag = 12)

## Part 3

# Prepare data

Y = cbind(propReturns,infl,govYield,uRate,exRate)
estimationEnd = nrow(Y)

# Maximum likelihood estimation

maxLags = 12
modelEstimates = list()

# Estimat a VAR model for each lag specification

seriesLabels = c('Property returns','Inflation','5-year government bond yields','Unemployment','US dollar exchange rate')

modelEstimates = VAR(Y,lag.max = 12,ic='AIC')

#Forecast without impulse
Forecast = predict(modelEstimates,n.ahead=24,ci=0.95)
Ynoimpulse = cbind(Forecast$fcst[[1]][,1],Forecast$fcst[[2]][,1],Forecast$fcst[[3]][,1],Forecast$fcst[[4]][,1],Forecast$fcst[[5]][,1])

#impulse response
impulse = irf(modelEstimates, impulse = 'govYield',ortho = F,boot = F,n.ahead=23)
std = summary(modelEstimates$varresult$govYield)$sigma 
Yimpulse = impulse$irf$govYield*std+Ynoimpulse #Forecast after impulse

opar = par()
par(mfrow=c(3,2))
for (ii in 1:5) {
  plot(x=(Yimpulse[,ii]-Ynoimpulse[,ii])/Ynoimpulse[,ii],type='l', xlab='Horizon',ylab='% Change',main=seriesLabels[ii])
}
par(opar)
## Part 4

# Construct forecasts
# Plot forecasts

plot.new()
opar = par()
par(mfrow=c(3,2))
forecastHorizon = 2*12
for (ii in 1:5) {
  matplot(Forecast$fcst[[ii]][,1:3],type = 'l',xlab = 'Horizon',ylab = 'Forecast', main=seriesLabels[ii])

}
par(opar)


