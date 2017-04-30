# Rady - Business Forecasting
# Assignment 1 - Part 2
# 
# Anton Prokopyev
# Based on Istructor's sample code

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
<<<<<<< Updated upstream
data1 = read.xlsx('Keeling_CO2data_2017.xlsx',1,rowIndex = 6:713,colIndex = 1:5);

#### Q1 - Plot data ####
plot(data1$Date.1, data1$CO2, xlab = 'Date', ylab = 'CO2', type='l',col='blue')
=======
data1 = read.xlsx('BusinessForecasting/timeseries-rady/Keeling_CO2data_2017.xlsx',1,rowIndex = 6:713,colIndex = 1:5);

#### Q1 - Plot data ####
plot(data1$Date.1, data1$CO2, xlab = 'Date', ylab = 'CO2', type='l',col='black')
>>>>>>> Stashed changes

#### Q2 - Plot linear fit ####
data1sub <- subset(data1, data1$Yr<2006)
co2.lm.05 = lm(data1sub$CO2 ~ data1sub$Date.1)
<<<<<<< Updated upstream
plot(data1$Date.1, data1$CO2, xlab = 'Date', ylab = 'CO2', type='l',col='blue')
lines(seasadj(decompose(data1,"multiplicative")),col=4)
=======
plot(data1$Date.1, data1$CO2, xlab = 'Date', ylab = 'CO2', type='l',col='black')
lines(seasadj(decompose(data1,"multiplicative")))
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
co2.lm.05.quad = lm(data1sub$CO2~t-t2)
lines(lm(data1sub$CO2~t+t2)$fit,col=2,lwd=2)
=======
co2.lm.05.quad = lm(data1sub$CO2 ~ t+t2 )
lines(lm(data1sub$CO2~t+t2+datatemper)$fit,col=2,lwd=2)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
#### Q4 - Include seasonal effect

Q1 = rep(c(1, 0, 0, 0), length(t) / 4)
Q2 = rep(c(0, 1, 0, 0), length(t) / 4)
Q3 = rep(c(0, 0, 1, 0), length(t) / 4)
Q4 = rep(c(0, 0, 0, 1), length(t) / 4)

Q1 <- edit(Q1)
Q2 <- edit(Q2)
Q3 <- edit(Q3)
Q4 <- edit(Q4)

summary(lm(data1sub$CO2~t+t2 + Q1 + Q2 + Q3 + Q4))
plot.ts(data1$CO2, type='l')
lines(lm(data1sub$CO2~t+t2 + Q1 + Q2 + Q3 + Q4)$fit,col=2,lwd=2)


  
##Time-series decomposition
co2<-ts(data1[,1],start=1958,freq=12)
plot(stl(log(co2),s.window="periodic"))

# install.packages('RStata')
library(RStata)
options("RStata.StataPath" = '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
options("RStata.StataVersion" = 14)
stata("help regress")
stata("set more off, permanently")
stata("sum CO2", data.in = data1sub)
stata("tsset month, monthly", data.in = data1)









=======
tempdatadump = read.xlsx('BusinessForecasting/timeseries-rady/annualtemp.xlsx',1,rowIndex = 1:577,colIndex = 1,colClasses = 'numeric',stringsAsFactors=FALSE);
datatemper <- ts(tempdatadump[,2],start = c(1958,1),end = c(2005,12), frequency = 12)
datatemper<-ts(tempdatadump[,2],start = c(1958,1),end = c(2005,12), frequency = 12)

tempertimev <- ts(datadump$Date,start = c(1958,1),end = c(2005,12), frequency = 12)
>>>>>>> Stashed changes






<<<<<<< Updated upstream

=======
################################################################################
################################not working#####################################
################################################################################
#### Q4 - Include seasonal effect

# Q1 = rep(c(1, 0, 0, 0), length(t) / 4)
# Q2 = rep(c(0, 1, 0, 0), length(t) / 4)
# Q3 = rep(c(0, 0, 1, 0), length(t) / 4)
# Q4 = rep(c(0, 0, 0, 1), length(t) / 4)
# 
# Q1 <- edit(Q1)
# Q2 <- edit(Q2)
# Q3 <- edit(Q3)
# Q4 <- edit(Q4)
# 
# summary(lm(data1sub$CO2~t+t2 + Q1 + Q2 + Q3 + Q4))
# plot.ts(data1$CO2, type='l')
# lines(lm(data1sub$CO2~t+t2 + Q1 + Q2 + Q3 + Q4)$fit,col=2,lwd=2)
# 
# 
#   
# ##Time-series decomposition
# co2<-ts(data1[,1],start=1958,freq=12)
# plot(stl(log(co2),s.window="periodic"))
# 
# # install.packages('RStata')
# library(RStata)
# options("RStata.StataPath" = '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
# options("RStata.StataVersion" = 14)
# stata("help regress")
# stata("set more off, permanently")
# stata("sum CO2", data.in = data1sub)
# stata("tsset month, monthly", data.in = data1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
>>>>>>> Stashed changes



