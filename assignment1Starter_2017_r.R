# Rady - Business Forecasting
# Assignment 1
# 
# Anton Prokopyev
# Based on Istructor's sample code

#### Usually good practice to start R scripts with these commands ####
rm(list = ls())
cat("\f")

#### Install packages if you have never installed them. ####
#install.packages("tseries")
#install.packages("rJava")
#install.packages("xlsx")
#Loading packages
library(tseries)
library(rJava)
library(xlsx)
library(forecast)

#install.packages("forecast")

### Part I. Constructing prediction models for different variables ###

#### Read in data ####
# 
# The data for Part I is contained in the Excel file
# "time_series_data_updated.xlsx." Each sheet contains one of the 4
# different time series the question asks you to work with. As an example I
# will work through this part with the unemployment rate, which is in the
# first sheet
# 
# Example use of read.xlsx command
# 1st argument: name of Excel file in single quotes
# 2nd argument (optional): sheet number to read in
# 3rd and 4th argument (optional): row and column number to read in 
# Type '?read.xlsx' for more info

data1 = read.xlsx('time_series_data_2017.xlsx',1,rowIndex = 11:838,colIndex = 1:2);
data2 = read.xlsx('time_series_data_2017.xlsx',2,rowIndex = 11:850,colIndex = 1:2);
data3 = read.xlsx('time_series_data_2017.xlsx',3,rowIndex = 11:596,colIndex = 1:2);
data4 = read.xlsx('time_series_data_2017.xlsx',4,rowIndex = 8:1761,colIndex = 1:2);
 

#### Question 1 - Plot raw data ####

  ### Plot 1
  options(repr.plot.width=7, repr.plot.height=6)
  plot(data1$observation_date, data1$UNRATE, xlab = 'Date', ylab = 'Unemployment', type='l')
  ### The plot reveals seasonality and a slight upward trend. 

  ### Plot 2
  options(repr.plot.width=7, repr.plot.height=6)
  plot(data2$observation_date, data2$CPIAUCSL, xlab = 'Date', ylab = 'Consumer Price Index', type='l')
  ### The plot reveals a strong upward trend, therefore computing log-first difference is needed.

    ### Compute and plot log first differences
    data2log = data2
    data2log$CPIAUCSL = log(data2log$CPIAUCSL)
    storediff <- diff(data2log$CPIAUCSL, differences=1)
    data2log <- data2log[-c(1), ]
    data2log$CPIAUCSL <- storediff
    plot(data2log$observation_date, data2log$CPIAUCSL, xlab = 'Date', ylab = 'Inflation rate for CPIUCSL', type='l')
  
  ### Plot 3
  options(repr.plot.width=7, repr.plot.height=6)
  plot(data3$observation_date, data3$GOLDAMGBD228NLBM, xlab = 'Date', ylab = 'Gold Fixing Price', type='l')
  ### The plot reveals a strong upward trend, therefore computing log-first difference is needed.
  
    ### Compute and plot log first differences
    data3log = data3
    data3log$GOLDAMGBD228NLBM = log(data3log$GOLDAMGBD228NLBM)
    storediff <- diff(data3log$GOLDAMGBD228NLBM, differences=1)
    data3log <- data3log[-c(1), ]
    data3log$GOLDAMGBD228NLBM <- storediff
    plot(data3log$observation_date, data3log$GOLDAMGBD228NLBM, xlab = 'Date', ylab = 'Continuously Compounded Rate of Return on Gold', type='l')
  
  ### Plot 4 
  options(repr.plot.width=7, repr.plot.height=6)
  plot(data4$Date, data4$P, xlab = 'Date', ylab = 'S&P', type='l')
  
  ### Compute and plot log first differences
  data4log = data4
  data4log$P = log(data4log$P)
  storediff <- diff(data4log$P, differences=1)
  data4log <- data4log[-c(1), ]
  data4log$P <- storediff
  plot(data4log$Date, data4log$P, xlab = 'Date', ylab = 'Inflation rate for S&P', type='l')
  
  
  
  
  
####  Augmented Dickey-Fuller test for non-stationarity ####
#  Despite getting a value of pValue = 0.53 for the test, most economists DO NOT
#  first difference the unemployment rate. However, for the purposes of this
#  assignment either is fine
pvalue1 = adf.test(data1$UNRATE,k=1)$p.value

  
  
#### Question 2 ####
   
  # Plot the autocorrelation function for the first 10 lags
  acf(data1$UNRATE, lag.max = 10)
  acf(data2log$CPIAUCSL, lag.max = 10)
  acf(data3log$GOLDAMGBD228NLBM, lag.max = 10)
  acf(data4log$P, lag.max = 10)
  
  # Display the autocorrelations
  data1Autocorrs = acf(data1$UNRATE)$acf
  plot(data1Autocorrs)
  
  data2Autocorrs = acf(data2log$CPIAUCSL)$acf
  plot(data2Autocorrs)
  
  data3Autocorrs = acf(data3log$GOLDAMGBD228NLBM)$acf
  plot(data3Autocorrs)
  
  data4Autocorrs = acf(data4log$P)$acf
  plot(data4Autocorrs)

  #Perform LBQ tests for significance of autocorrelations
  Box.test(data1$UNRATE, lag = 1)
  Box.test(data1$UNRATE, lag = 10)
  
  Box.test(data2log$CPIAUCSL, lag = 1)
  Box.test(data2log$CPIAUCSL, lag = 10)
  
  Box.test(data3log$GOLDAMGBD228NLBM, lag = 1)
  Box.test(data3log$GOLDAMGBD228NLBM, lag = 10)
  
  Box.test(data4log$P, lag = 1)
  Box.test(data4log$P, lag = 10)

#### Question 3 - Test out different combinations of AR and MA terms and use AIC and BIC to determine fit ####

  maxAR = 4; # max of 4 AR terms
  maxMA = 4; # max of 4 MA terms

    # Double for-loop over AR and MA lags
    # Good practice to use 'ii' instead of 'i' as loop index (like in other
    # programming languages) because MATLAB stores 'i' as the imaginary
    # constant sqrt(-1).
  
  
  
  urate = data1$UNRATE
  urateModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
  ind = 1
  lagCombinations = urateModelCriteria;
  
  # Double for loop over AR and MA lags
  # Good practice to use 'ii' instead of 'i' as loop index (like in other
  # programming languages) because MATLAB stores 'i' as the imaginary
  # constant sqrt(-1).
  for(ii in 0:maxMA){
    for (jj in 0:maxAR) {
      if(ii != 0 || jj != 0){
        urateModelCriteria[ind,1] = AIC(arima(urate, order = c(ii, 0 ,jj)))
        urateModelCriteria[ind,2] = BIC(arima(urate, order = c(ii, 0 ,jj)))
        lagCombinations[ind,] = matrix(c(ii,jj),1,2)
        ind = ind+1
      }
      
    }
  }
  
  # Find models with lowest AIC and BIC criteria, corresponding to best
  # statistical fit
  minIndices = apply(urateModelCriteria,2,which.min)
  
  # Store the number of AR and MA terms associated with best model according
  # to AIC
  bestAICModelLags = lagCombinations[minIndices[1],]
  
  # Estimate model chosen by AIC
  modelEstimate = arima(urate,order=c(lagCombinations[minIndices[1],1],0,lagCombinations[minIndices[1],2]) )
  
  # Save residuals from fitted ARMA model
  resid = modelEstimate$residual
  # Test for serial correlation
  Box.test(resid,lag = 10)
  # Visually inspect fitted residuals
  plot(datas$observation_date,resid,xlab = 'date',ylab = 'residual',type = 'l')
  
  # Construct fittef values from residual and original time series
  urateFit = urate-resid
  
  #Plot time series and fitted values
  plot(data1$observation_date, urate, xlab = 'date', ylab = 'unemployment', type='l',col='blue')
  lines(data1$observation_date, urateFit,col='red')
  legend(min(datas$observation_date),10,c('urate','urateFit'),col = c('blue','red'),lty = c(1,1))
  
  
  
  
#### Data1 ####    
      dataModelCriteria1 = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
      ind1 = 1
      lagCombinations1 = dataModelCriteria1;
  
      for(ii in 0:maxMA){
         for (jj in 0:maxAR) {
            if(ii != 0 || jj != 0){
        dataModelCriteria1[ind,1] = AIC(arima(data1$UNRATE, order = c(ii, 0 ,jj)))
        dataModelCriteria1[ind,2] = BIC(arima(data1$UNRATE, order = c(ii, 0 ,jj)))
        lagCombinations1[ind,] = matrix(c(ii,jj),1,2)
        ind1 = ind1+1
      }
    }
  }

      # Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
      minCriteria1 = apply(dataModelCriteria1,2,which.min)

      # Store the number of AR and MA terms associated with best model according to AIC
      bestAICModelLags1 = lagCombinations1[minCriteria1[1],]

      # Estimate model chosen by AIC
      modelEstimate1 = arima(data1$UNRATE,order=c(lagCombinations1[minCriteria1[1],1],0,lagCombinations1[minCriteria1[1],2]) )
      
      # Save residuals from fitted ARMA model
      resid1 = modelEstimate1$residual
      
      # Test for serial correlation
      Box.test(resid1,lag = 10)
      
      # Visually inspect fitted residuals
      plot(data1$observation_date,resid1, xlab = 'Date', ylab = 'Residual', type = 'l')
      
      # Construct fitted values from residual and original time series
      unrate = data1$UNRATE
      data1Fitted = unrate-resid1

            
      
#### Data2 ####
      dataModelCriteria2 = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
      ind2 = 1
      lagCombinations2 = dataModelCriteria2;
      
      for(ii in 0:maxMA){
        for (jj in 0:maxAR) {
          if(ii != 0 || jj != 0){
            dataModelCriteria2[ind,1] = AIC(arima(data2log$CPIAUCSL, order = c(ii, 0 ,jj)))
            dataModelCriteria2[ind,2] = BIC(arima(data2log$CPIAUCSL, order = c(ii, 0 ,jj)))
            lagCombinations2[ind,] = matrix(c(ii,jj),1,2)
            ind2 = ind2+1
          }
        }
      }
      
      # Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
      minCriteria2 = apply(dataModelCriteria2,2,which.min)
      
      # Store the number of AR and MA terms associated with best model according to AIC
      bestAICModelLags2 = lagCombinations2[minCriteria2[1],]
      
      # Estimate model chosen by AIC
      modelEstimate2 = arima(data2log$CPIAUCSL,order=c(lagCombinations2[minCriteria2[1],1],0,lagCombinations2[minCriteria2[1],2]) )
      
      # Save residuals from fitted ARMA model
      resid2 = modelEstimate2$residual
      
      # Test for serial correlation
      Box.test(resid2,lag = 10)
      
      # Visually inspect fitted residuals
      plot(data2log$observation_date, resid2, xlab = 'Date', ylab = 'Residual', type = 'l')
      
      # Construct fitted values from residual and original time series
      data2Fitted = data2log$CPIAUCSL-resid2

#### Data3 ####
dataModelCriteria3 = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind3 = 1
lagCombinations3 = dataModelCriteria3;

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      dataModelCriteria3[ind,1] = AIC(arima(data3log$GOLDAMGBD228NLBM, order = c(ii, 0 ,jj)))
      dataModelCriteria3[ind,2] = BIC(arima(data3log$GOLDAMGBD228NLBM, order = c(ii, 0 ,jj)))
      lagCombinations3[ind,] = matrix(c(ii,jj),1,2)
      ind3 = ind3+1
    }
  }
}

      # Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
      minCriteria3 = apply(dataModelCriteria3,2,which.min)

      # Store the number of AR and MA terms associated with best model according to AIC
      bestAICModelLags3 = lagCombinations3[minCriteria3[1],]
  
      # Estimate model chosen by AIC
      modelEstimate3 = arima(data3log$GOLDAMGBD228NLBM, order=c(lagCombinations3[minCriteria3[1],1],0,lagCombinations3[minCriteria3[1],2]) )

      # Save residuals from fitted ARMA model
      resid3 = modelEstimate3$residual

      # Test for serial correlation
      Box.test(resid3,lag = 10)

      # Visually inspect fitted residuals
      plot(data3log$observation_date, resid3, xlab = 'Date', ylab = 'Residual', type = 'l')

      # Construct fitted values from residual and original time series
      data3Fitted = data3log$GOLDAMGBD228NLBM-resid3
    
      
#### Data4 ####
dataModelCriteria4 = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind4 = 1
lagCombinations4 = dataModelCriteria4;

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      dataModelCriteria4[ind,1] = AIC(arima(data4log$P, order = c(ii, 0 ,jj)))
      dataModelCriteria4[ind,2] = BIC(arima(data4log$P, order = c(ii, 0 ,jj)))
      lagCombinations4[ind,] = matrix(c(ii,jj),1,2)
      ind4 = ind4+1
    }
  }
}

# Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
minCriteria4 = apply(dataModelCriteria4,2,which.min)

# Store the number of AR and MA terms associated with best model according to AIC
bestAICModelLags4 = lagCombinations4[minCriteria4[1],]

# Estimate model chosen by AIC
modelEstimate4 = arima(data4log$P, order=c(lagCombinations4[minCriteria4[1],1],0,lagCombinations4[minCriteria4[1],2]))

# Save residuals from fitted ARMA model
resid4 = modelEstimate4$residual

# Test for serial correlation
Box.test(resid4,lag = 10)

# Visually inspect fitted residuals
plot(data4log$Date, resid4, xlab = 'Date', ylab = 'Residual', type = 'l')

# Construct fitted values from residual and original time series
data4Fitted = data4log$P-resid4



#### Question 4 - Plot time series and fitted values ####

#### Data 1 plot
plot(data1$observation_date, data1$UNRATE, xlab = 'Date', ylab = 'Unemployment', type='l',col='blue')
lines(data1$observation_date, data1Fitted, col='red')
legend(min(data1$observation_date),10,c('data1$UNRATE','data1Fitted'),col = c('blue','red'),lty = c(1,1))

#### Data 2 plot
plot(data2log$observation_date, data2log$CPIAUCSL, xlab = 'Date', ylab = 'Inflation rate for CPIUCSL', type='l',col='blue')
lines(data2log$observation_date, data2Fitted, col='red')
legend(min(data2log$observation_date),10,c('data2log$CPIAUCSL','data2Fitted'),col = c('blue','red'),lty = c(1,1))

#### Data 3 plot
plot(data3log$observation_date, data3log$GOLDAMGBD228NLBM, xlab = 'Date', ylab = 'Log Gold Prices', type='l',col='blue')
lines(data3log$observation_date, data3Fitted, col='red')
legend(min(data3log$observation_date),10,c('data3log$GOLDAMGBD228NLBM','data3Fitted'),col = c('blue','red'),lty = c(1,1))

#### Data 4 plot
plot(data4log$Date, data4log$P, xlab = 'Date', ylab = 'Log S&P', type='l',col='blue')
lines(data4log$Date, data4Fitted, col='red')
legend(min(data4log$Date),10,c('data4log$P','data4Fitted'),col = c('blue','red'),lty = c(1,1))
