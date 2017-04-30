## Thomas Murphey A53044035
## Anton Prokopyev A53105867
## Assignment 3

rm(list = ls())
cat("\f")

## Preliminaries
library(xlsx)
library(tseries)
library(vars)
# Read in data

temp = read.xlsx('HongKong_property_index_combined_2017.xlsx','Price Data',1)

# Use largest sample containing all 5 variables

estimationStart = 126
estimationEnd = 392
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
timeVector = seq(1994+8/12,2016+11/12,along.with = cpi)

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

# Estimate various ARMA(1,1) models, make sure to try other specifications here

model = arima(propReturns[-1],order = c(1,0,1))
informationCriteria = matrix(c(AIC(model),BIC(model)),1,2)

# Compute fitted residuals
real = model$residuals
eHat = propReturns[-1]-model$residuals

p=1
q=1
plot(timeVector[-seq(1:max(p,q))],eHat,type = 'l',xlab = 'Time',ylab = 'Fitted Residuals',col='blue')
lines(timeVector[-seq(1:max(p,q))],propReturns[-1]-eHat,col="red")

plot(timeVector[-seq(1:max(p,q))],propReturns[-1]-eHat,type = 'l',xlab = 'Time',ylab = 'Fitted Residuals')
lines(timeVector[-seq(1:max(p,q))],eHat,col="red")


# Check autocorrelations
acf(eHat)

# LBQ test using 12 lags (since monthly data)

Box.test(eHat, lag = 12)


## Part 2

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
seriesLabels = c('Property returns','Inflation','5-year government bond yields','Unemployment','US dollar exchange rate');
modelEstimates = VAR(Y,p=1)
modelEstimates2 = VAR(Y,p=2, type='both')
# modelEstimates2 = VAR(Y,p=2, type='both') if we want to 
# have an intercept and linear time trend included in our model. but results don't change much

# Compute information criteria to determine fit. You will need to compute
# the BIC of both of your estimated VARs. 
# Function VAR has an argument to select model based on AIC

AIC(modelEstimates)
AIC(modelEstimates2)

BIC(modelEstimates)
BIC(modelEstimates2)


# Forecast without impulse
Forecast = predict(modelEstimates,n.ahead=24,ci=0.95)
Ynoimpulse = cbind(Forecast$fcst[[1]][,1],Forecast$fcst[[2]][,1],Forecast$fcst[[3]][,1],Forecast$fcst[[4]][,1],Forecast$fcst[[5]][,1])

# impulse response
impulseHorizon = 2*12 #2 year impulse horizon
impulse = irf(modelEstimates, impulse = 'govYield',ortho = F,boot = F,n.ahead=impulseHorizon-1)
std = summary(modelEstimates$varresult$govYield)$sigma 
# Forecast after impulse
Yimpulse = impulse$irf$govYield*std+Ynoimpulse 

opar = par()
par(mfrow=c(3,2))
for (ii in 1:5) {
  plot(x=(Yimpulse[,ii]-Ynoimpulse[,ii])/Ynoimpulse[,ii],type='l', xlab='Horizon',ylab='% Change',main=seriesLabels[ii])
}
par(opar)


#########################################

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


