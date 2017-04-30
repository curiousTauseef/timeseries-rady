# Rady - Business Forecasting
# Assignment 1 - Part 2
# 
# Anton Prokopyev, Thomas Murphey
# Based on Istructor's sample code and online resourses

#### Usually good practice to start R scripts with these commands ####
rm(list = ls())
cat("\f")

#### Load libs / Install packages if you have never installed them. ####
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
data1 = read.xlsx('BusinessForecasting/timeseries-rady/Keeling_CO2data_2017.xlsx',1,rowIndex = 6:713,colIndex = 1:5);

#### Q1 - Plot data ####
plot(data1$Date.1, data1$CO2, xlab = 'Date', ylab = 'CO2', type='l',col='black')

#### Q2 - Plot linear fit ####
data1sub <- subset(data1, data1$Yr<2006)
co2.lm.05 = lm(data1sub$CO2 ~ data1sub$Date.1)
plot(data1$Date.1, data1$CO2, xlab = 'Date', ylab = 'CO2', type='l',col='black')
lines(seasadj(decompose(data1,"multiplicative")))
abline(co2.lm.05, col=2)
summary(co2.lm.05)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -6.487 -2.108  0.019  1.976  7.589 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -2.396e+03  1.783e+01  -134.4   <2e-16 ***
#   data1sub$Date.1  1.382e+00  8.993e-03   153.7   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.975 on 572 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9763 
# F-statistic: 2.361e+04 on 1 and 572 DF,  p-value: < 2.2e-16

#### Q3 - Better trend using quadratic ####

t<-seq(1958,2005,length=length(data1sub$CO2))
t2<-t^2
plot.ts(data1$CO2, type='l')
co2.lm.05.quad = lm(data1sub$CO2 ~ t+t2 )
lines(lm(data1sub$CO2~t+t2+datatemper)$fit,col=2,lwd=2)
summary(co2.lm.05.quad)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.4863 -2.1068  0.0178  1.9773  7.5873 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.444e+03  1.813e+01  -134.8   <2e-16 ***
#   t            1.406e+00  9.150e-03   153.7   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.974 on 572 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.9764,	Adjusted R-squared:  0.9763 
# F-statistic: 2.362e+04 on 1 and 572 DF,  p-value: < 2.2e-16

tempdatadump = read.xlsx('BusinessForecasting/timeseries-rady/annualtemp.xlsx',1,rowIndex = 1:577,colIndex = 1,colClasses = 'numeric',stringsAsFactors=FALSE);
datatemper <- ts(tempdatadump[,2],start = c(1958,1),end = c(2005,12), frequency = 12)
datatemper<-ts(tempdatadump[,2],start = c(1958,1),end = c(2005,12), frequency = 12)

tempertimev <- ts(datadump$Date,start = c(1958,1),end = c(2005,12), frequency = 12)





################## reboot ####################

rm(list = ls())
cat("\f")

#### Load libs / Install packages if you have never installed them. ####
#install.packages("tseries")
#install.packages("rJava")
#install.packages("xlsx")
#install.packages("forecast")

#Loading packages
library(tseries)
library(rJava)
library(xlsx)
library(forecast)

#### Read in data ####
datadump = read.xlsx('BusinessForecasting/timeseries-rady/Keeling_CO2data_2017.xlsx',1,rowIndex = 6:713,colIndex = 1:5);

data<-ts(datadump[,5],start = c(1958,1),frequency = 12)
data05<-ts(datadump[,5],start = c(1958,1),end = c(2005,12), frequency = 12)

#### Plot raw data ####
plot(data, xlab="Years", ylab = "CO2 Level")

#### Difference data to make it stationary on mean (remove) ####
plot(diff(data, lag = 12, differences = 1),ylab="1st Differences CO2 Level with Lag=12")

#### Log transform data to make it stationary on variance ####
plot(log10(data),ylab="Log CO2 Level")

#### Take 1st Diff log transformed data to make it stationary on both mean and variance ####
# plot(diff(log10(data),lag = 12, differences = 1),ylab="1st Differences Log CO2 Level with Lag=12")
# OK We're not taking log here 
data05diff <- diff((data05),lag = 12, differences = 1)


#### Looking for best ARIMA model ####
require(forecast)
bestARIMAmodel <- auto.arima(data05, ic=c("aicc", "aic", "bic"), approximation=FALSE, trace=TRUE)
summary(bestARIMAmodel)
# Best model: ARIMA(0,1,1)(0,1,1)[12]   

# Fit model to until end of 2005
arima.model <- Arima(window(data,end=2005+12/12),order=c(0,1,1),
                   seasonal=list(order=c(0,1,1),period=12),lambda=0)
plot(forecast(arima.model,h=130))
lines(data)

plot(density(arima.model$residuals))
qqnorm(arima.model$residuals)

#### add temp var ####
# # import temp data as exogenous var
# # http://cdiac.ornl.gov/climate/temp/temp_table.html
## http://cdiac.ornl.gov/trends/temp/lugina/lugina.html
datatemper = read.xlsx('BusinessForecasting/timeseries-rady/annualtemp.xlsx',1,rowIndex = 1:577,colIndex = 1,colClasses = 'numeric',stringsAsFactors=FALSE);
datatemper <- ts(datatemper[,1],start = c(1958,1),end = c(2005,12), frequency = 12)
tempertimev <- ts(datadump$Date,start = c(1958,1),end = c(2005,12), frequency = 12)

summary(lm(data05 ~ tempertimev + tempertimev^2 + datatemper))


#### Apply fitted model to later data up to 2020 ####
arima.model2 <- Arima(window(data,start=2005+12/12),model=arima.model)
plot(forecast(arima.model2,h=48))

summary(arima.model2)









# Unnecessary

# # Forecast accuracy measures on the log scale.
# # in-sample one-step forecasts.
# accuracy(arima.model)
# # out-of-sample one-step forecasts.
# accuracy(arima.model2)
# # out-of-sample multi-step forecasts
# accuracy(forecast(arima.model,h=48,lambda=NULL),
#          log(window(data,start=1958)))






# #### come naive code ####
# pred2 <- predict(bestARIMAmodel2, n.ahead = 168)
# pred2
# # Plottoing actual data first
# plot(data05,type="l",xlim=c(1958,2020),xlab = "Year",ylab = "1st Diff Log CO2 Level")
# # Plotting preiction now
# lines(10^pred$pred,col="red")
# # Confidence intervals 2StD
# lines(10^(pred$pred+2*pred$se),col="blue")
# lines(10^(pred$pred-2*pred$se),col="blue")



#### The method that didn't work #### 
# Forecast CO2 
# #  From 2005m12 Until 2016m10
# pred <- predict(bestARIMAmodel, n.ahead = 130)
# pred
# # Plottoing actual data first
# plot(data05,type="l",xlim=c(1958,2020),ylim=c(310,420),xlab = "Year",ylab = "1st Diff Log CO2 Level")
# # Plotting preiction now
# lines(pred$pred,col="red")
# # Confidence intervals 2StD
# lines((pred$pred+2*pred$se),col="blue")
# lines((pred$pred-2*pred$se),col="blue")


#### OLD Until 2020 ####

# # import temp data as exogenous var
# # http://cdiac.ornl.gov/climate/temp/temp_table.html
# # http://cdiac.ornl.gov/trends/temp/lugina/lugina.html
# 
# datatemp = read.xlsx('BusinessForecasting/timeseries-rady/annualtemp.xlsx',1,rowIndex = 1:577,colIndex = 1,colClasses = 'numeric',stringsAsFactors=FALSE);
# datatemp <- ts(datatemp[,1],start = c(1958,1),end = c(2005,12), frequency = 12)
# 
# require(forecast)
# bestARIMAmodel2 <- auto.arima(diff(data05),ic=c("aicc", "aic", "bic"), approximation=FALSE,trace=TRUE)
# #Best model: ARIMA(4,1,3)(0,0,2)[12] with drift         
# summary(bestARIMAmodel2)
