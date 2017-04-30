# Assignment 1
rm(list = ls())
cat("\f")

#Loading packages
library(tseries)
library(rJava)
library(xlsx)

## Part I. Constructing predition models for different variables

# Read in data
data1 = read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/time_series_data_updated.xlsx',1,rowIndex = 11:826,colIndex = 1:2);
urate = data1$UNRATE
data2 = read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/time_series_data_updated.xlsx',2,rowIndex = 11:838,colIndex = 1:2);
cprices = data2$CPIAUCSL
data3 = read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/time_series_data_updated.xlsx',3,rowIndex = 11:583,colIndex = 1:2);
gprices = data3$GOLDAMGBD228NLBM
data4 = read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/time_series_data_updated.xlsx',4,rowIndex = 8:1748,colIndex = 1:2);
spprices = data4$P


#1
plot(data1,type='l')

plot(data2,type='l')

plot(data3,type='l')

plot(data4,type='l')

cgrowth = log(cprices[-1])-log(cprices[-length(cprices)])
ggrowth = log(gprices[-1])-log(gprices[-length(gprices)])
spgrowth = log(spprices[-1])-log(spprices[-length(spprices)])

plot(cgrowth,type='l')

plot(ggrowth,type='l')

plot(spgrowth,type='l')

# 2
acf(urate,lag=10)

Box.test(urate,lag = 1)
Box.test(urate,lag = 10)

acf(cgrowth,lag=10)

Box.test(cgrowth,lag = 1)
Box.test(cgrowth,lag = 10)

acf(ggrowth,lag=10)

Box.test(ggrowth,lag = 1)
Box.test(ggrowth,lag = 10)

acf(spgrowth,lag=10)

Box.test(spgrowth,lag = 1)
Box.test(spgrowth,lag = 10)

#3

maxAR = 4; # max of 4 AR terms
maxMA = 4; # max of 4 MA terms

urateModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1
lagCombinations = urateModelCriteria;

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      urateModelCriteria[ind,1] = AIC(arima(urate, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      urateModelCriteria[ind,2] = BIC(arima(urate, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

minIndices = matrix(0,4,2)
minIndices[1,] = apply(urateModelCriteria,2,which.min)

cgrowthModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      cgrowthModelCriteria[ind,1] = AIC(arima(cgrowth, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      cgrowthModelCriteria[ind,2] = BIC(arima(cgrowth, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

minIndices[2,] = apply(cgrowthModelCriteria,2,which.min)

ggrowthModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      ggrowthModelCriteria[ind,1] = AIC(arima(ggrowth, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      ggrowthModelCriteria[ind,2] = BIC(arima(ggrowth, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

minIndices[3,] = apply(ggrowthModelCriteria,2,which.min)

spgrowthModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      spgrowthModelCriteria[ind,1] = AIC(arima(spgrowth, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      spgrowthModelCriteria[ind,2] = BIC(arima(spgrowth, order = c(ii, 0 ,jj),optim.control = list(maxit = 1000)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

minIndices[4,] = apply(spgrowthModelCriteria,2,which.min)

bestAICModelLags = lagCombinations[minIndices[,1],]

# 4

modelEstimate = arima(urate,order=c(bestAICModelLags[1,1],0,bestAICModelLags[1,2]) )
resid = modelEstimate$residual
Box.test(resid,lag = 10)
urateFit = urate-resid

plot(data1$observation_date, urate, xlab = 'date', ylab = 'unemployment', type='l',col='blue')
lines(data1$observation_date, urateFit,col='red')
legend(min(data1$observation_date),10,c('urate','urateFit'),col = c('blue','red'),lty = c(1,1))


modelEstimate = arima(cgrowth,order=c(bestAICModelLags[2,1],0,bestAICModelLags[2,2]) )
resid = modelEstimate$residual
Box.test(resid,lag = 10)
cgrowthFit = cgrowth-resid

plot(data2$observation_date[-1], cgrowth, xlab = 'date', ylab = 'cpi', type='l',col='blue')
lines(data2$observation_date[-1], cgrowthFit,col='red')
legend(min(data2$observation_date),-0.01,c('cgrowth','cgrowthFit'),col = c('blue','red'),lty = c(1,1))


modelEstimate = arima(ggrowth,order=c(bestAICModelLags[3,1],0,bestAICModelLags[3,2]) )
resid = modelEstimate$residual
Box.test(resid,lag = 10)
ggrowthFit = ggrowth-resid

plot(data3$observation_date[-1], ggrowth, xlab = 'date', ylab = 'gold return', type='l',col='blue')
lines(data3$observation_date[-1], ggrowthFit,col='red')
legend(min(data3$observation_date),-0.1,c('ggrowth','ggrowthFit'),col = c('blue','red'),lty = c(1,1))


modelEstimate = arima(spgrowth,order=c(bestAICModelLags[4,1],0,bestAICModelLags[4,2]) )
resid = modelEstimate$residual
Box.test(resid,lag = 10)
spgrowthFit = spgrowth-resid

t = seq.Date(as.Date('1871/02/01'),as.Date('2015/12/01'),'month')
plot(t, spgrowth, xlab = 'date', ylab = 'cpi', type='l',col='blue')
lines(t, spgrowthFit,col='red')
legend(min(t),-0.15,c('spgrowth','spgrowthFit'),col = c('blue','red'),lty = c(1,1))

## Part II. Modeling Trend and Seasonality

rm(list = ls())
cat("\f")
datas = read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Keeling_CO2data_2017.xlsx',1,rowIndex = c(6,9:683),colIndex = 1:5)

#1

CO2 = datas$CO2
timeAxis = seq.Date(as.Date('1958/02/01'),as.Date('2014/04/04'),'month')
plot(timeAxis,CO2,type = 'l')

#2

estimationEnd = 582-8 # data up untill the end of 2005
sampleEnd = 683-8
timeVector = 1:estimationEnd
X = timeVector
y = CO2[1:estimationEnd]
model1 = lm(y~X)
summary(model1)
plot(timeAxis[1:estimationEnd],y,type = 'l',col='blue')
lines(timeAxis[1:estimationEnd],model1$fitted.values,col='red')

#3
Z = X^2
model2 = lm(y~X + Z)
summary(model2)
plot(timeAxis[1:estimationEnd],y,type = 'l',col='blue')
lines(timeAxis[1:estimationEnd],model2$fitted.values,col='red')

#4

#Create Seasonal Dummies

for (ii in 1:11) {
  d = numeric(12)
  d[ii] = 1
  if (ii ==1){
    seasonalDummies = numeric(length(CO2))+d
  }else{
    seasonalDummies =cbind(seasonalDummies,numeric(length(CO2))+d)
  }
} 

model3 = lm(y~X+Z+seasonalDummies[1:estimationEnd,])
summary(model3)
plot(timeAxis[1:estimationEnd],y,type = 'l',col = 'blue')
lines(timeAxis[1:estimationEnd],model3$fitted.values,col='red')

#5
timeVector = (estimationEnd+1):sampleEnd
XForecast = cbind((numeric(sampleEnd-estimationEnd)+1),timeVector,timeVector^2,seasonalDummies[(estimationEnd+1):sampleEnd,])
yForecast = XForecast%*%matrix(model3$coefficients)
y = CO2[(estimationEnd+1):sampleEnd]
forecastErrors = y-yForecast

plot(timeAxis[(estimationEnd+1):sampleEnd],y,type='l',col='blue')
lines(timeAxis[(estimationEnd+1):sampleEnd],yForecast,col='red')


