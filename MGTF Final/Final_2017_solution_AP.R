### Business Forecasting ###
### Final Exam ###
### Anton Prokopyev ###

#### Setup ####
rm(list = ls())
cat("\f")

library(xlsx)
datas = read.xlsx('Final_2017_data_cleaned.xlsx', 1)
datas$observation <- 1:nrow(datas) 
datas <- datas[-c(1:4, 174:198), ]

#### DMtest and MZtest functions ####

dmtest = function(e1,e2,h0){
  T0 = length(e1)
  d0 = e1^2 - e2^2
  # Calculate the variance of the loss differential, taking into account autocorrelation
  dMean = mean(d0)
  gamma0 = var(d0)
  if(h0>1){
    gamma1 = numeric(h0-1)
    for (ii in seq(1,h0-1)) {
      sampleCov = cov(d0[-(1:ii)],d0[1:(T0-ii)])
      gamma1[ii] = sampleCov
    }
    varD = gamma0 + 2*sum(gamma1)
  }else{
    varD = gamma0
  }
  # Compute the diebold mariano statistic DM ~N(0,1)
  DM = dMean/sqrt(varD/T0)
  
  #one sided test H0: MSE1=MSE2, H1=MSE1>MSE2
  pval_R = 1-pnorm(DM)
  #one sided test H0: MSE1=MSE2, H1=MSE1<MSE2
  pval_L=pnorm(DM)
  #two side test H0: MSE1=MSE2, H1=MS1 different from MSE2
  if(DM > 0){
    pval_LR = (1-pnorm(DM))+pnorm(-DM)
  }
  else{
    if(DM<=0 || is.na(DM)){
      pval_LR = (1-pnorm(-DM))+pnorm(DM)
    }
  }
  
  S = c(DM, pval_L,pval_LR,pval_R)
  names(S) = c("DM", 'pval_L','pval_LR','pval_R')
  S
}


MZtest = function(y0,yhat){
  library(limSolve)
  MZstat = numeric()
  MZpval = numeric()
  for (ii in seq(1,ncol(yhat))) {
    X0 = cbind(rep(1,length(y0)),yhat[,ii])
    S1 = lsei(X0,y0)
    SSE = S1$solutionNorm
    A = -diag(2)
    b = matrix(c(0,-1),2,1)
    N_restriction = nrow(b)
    S2 = lsei(X0,y0,G=A,H=b)
    SSER = S2$solutionNorm
    N = length(y0)
    k = ncol(X0)
    df = N-k #degree of freedom
    MZstat = c(MZstat,((SSER-SSE)/N_restriction)/(SSE/df))
    MZpval = c(MZpval,1-pf(MZstat[ii],N_restriction,N-k))
  }
  S = list(MZstat,MZpval)
  names(S) = c('MZstat','MZpval')
  S
}




#### 1 ####
# How accurate were the one-quarter-ahead SPF and Greenbook inflation forecasts (columns D and F)? For each of the forecasts, report estimates of predictive accuracy.
# Deliverables: Brief explanation of measures of predictive accuracy
# Estimated values of measures of predictive accuracy

library(forecast)

accuracy(datas$SPF.t.t.1.,datas$Inflation.rate)
accuracy(datas$Greenbook.t.t.1.,datas$Inflation.rate)

plot.ts(datas$Inflation.rate, plot.conf=FALSE,
        main="Forecasts for quarterly inflation rate", type = "l",
        xlab="Time period", ylab="Inflation rate", col='dark grey')
##axis(1, datas$Date)
lines(datas$SPF.t.t.1.,col='blue')
lines(datas$Greenbook.t.t.1.,col='dark green')
legend("topright", lty=1, col=c('blue','dark green','red'),
       legend=c("One quarter ahead SPF","Greenbook 1"))

#### 2 ####



# Calculating fc errors for SPF
datas$SPF.t.t.1.err <- datas$Inflation.rate-datas$SPF.t.t.1.
datas$Greenbook.t.t.1.err <- datas$Inflation.rate-datas$Greenbook.t.t.1.
# Save as csv
write.csv(datas, "Final_datas1_from_R.csv")

# Some plots
plot.ts(datas$SPF.t.t.1.err, plot.conf=FALSE,
        main="Forecasts errors for quarterly inflation rate", type = "l",
        xlab="Time period", ylab="Inflation rate forecast error", col ='grey')
lines(datas$SPF.t.t.1.err,col='blue')
lines(datas$Greenbook.t.t.1.err,col='dark green')
legend("topright", lty=1, col=c('blue','dark green','red'),
       legend=c("One quarter ahead SPF Errors","Greenbook 1 Errors"))

# Creating scatter plots

# FC vs Realized
plot(datas$Inflation.rate, datas$SPF.t.t.1., 
     main = "Realized values vs. forecast values",
     xlab="True Inflation rate", ylab="Forecast",
     col = 'blue', cex=1/2)
points(datas$Inflation.rate,datas$Greenbook.t.t.1., col='green', cex=1/2)
legend("topleft", lty=1, lwd=5, col=c('blue','green','red'),
       legend=c("SPF 1 quarter ahead","Greenbook 1 quarter ahead"))

cor.test(datas$Inflation.rate, datas$SPF.t.t.1.) 
cor.test(datas$Inflation.rate, datas$Greenbook.t.t.1.)

hist(datas$SPF.t.t.1.err, nclass="FD", main="Histogram of SPF 1 residuals")
# x <- -4:4
# lines(x, 100*dnorm(x,0,sd(datas$SPF.t.t.1.err)),col=2)
acf(datas$SPF.t.t.1.err, main="ACF of SPF 1 residuals")

hist(datas$Greenbook.t.t.1.err, nclass="FD", main="Histogram of Greenbook 1 residuals")
acf(datas$Greenbook.t.t.1.err, main="ACF of Greenbook 1 residuals")

####
# lag=h and fitdf=K
#dwtest(y ~ 1+X)

Box.test(datas$SPF.t.t.1.err, lag=10, fitdf=0)
Box.test(datas$SPF.t.t.1.err,lag=10, fitdf=0, type="Lj")

Box.test(datas$Greenbook.t.t.1.err, lag=10, fitdf=0)
Box.test(datas$Greenbook.t.t.1.err,lag=10, fitdf=0, type="Lj")


# Running a couple of regressions

library(stargazer)
reg.model1 <- lm(datas$Inflation.rate~datas$SPF.t.t.1.)
  summary(reg.model1)
reg.model2 <- lm(datas$Inflation.rate~datas$Greenbook.t.t.1.)
  summary(reg.model2)
stargazer(reg.model1, reg.model2, title="Results", align=TRUE, out="reg1-2.txt.xls")

# Minzer Zarnowitz regression
# take errors from T+1 and put them into T for MZ test
##install.packages('DataCombine')
library(DataCombine)
datas <- slide(datas, Var = "SPF.t.t.1.", NewVar = "SPF.t.t.1.SLID", slideBy = 1)
plot(datas$Inflation.rate, datas$SPF.t.t.1.SLID)
reg.model3 <- lm(datas$Inflation.rate~datas$SPF.t.t.1.SLID)
summary(reg.model3)
#intercept = 0
library(car)
linearHypothesis(reg.model3,c("datas$SPF.t.t.1.SLID = 1","(Intercept) = 0"),test="F")



datas <- slide(datas, Var = "Greenbook.t.t.1.", NewVar = "Greenbook.t.t.1.SLID", slideBy = 1)
plot(datas$Inflation.rate, datas$Greenbook.t.t.1.SLID)
reg.model4 <- lm(datas$Inflation.rate~datas$Greenbook.t.t.1.SLID)
summary(reg.model4)
#intercept = 0
# var.test(reg.model3, reg.model4)
library(car)
linearHypothesis(reg.model4,c("datas$Greenbook.t.t.1.SLID = 1","(Intercept) = 0"),test="F")


# 
# ####
# MZtest = function(y0,yhat){
#   library(limSolve)
#   MZstat = numeric()
#   MZpval = numeric()
#   for (ii in seq(1,ncol(yhat))) {
#     X0 = cbind(rep(1,length(y0)),yhat[,ii])
#     S1 = lsei(X0,y0)
#     SSE = S1$solutionNorm
#     A = -diag(2)
#     b = matrix(c(0,-1),2,1)
#     N_restriction = nrow(b)
#     S2 = lsei(X0,y0,G=A,H=b)
#     SSER = S2$solutionNorm
#     N = length(y0)
#     k = ncol(X0)
#     df = N-k #degree of freedom
#     MZstat = c(MZstat,((SSER-SSE)/N_restriction)/(SSE/df))
#     MZpval = c(MZpval,1-pf(MZstat[ii],N_restriction,N-k))
#   }
#   S = list(MZstat,MZpval)
#   names(S) = c('MZstat','MZpval')
#   S
# }
# #### ####
# 
# MZpvals = numeric(2)
# for(ii in 1:2){
#   MZpvals[ii] = MZtest(datas$Inflation.rate[-seq(1,estimationEnd)],as.matrix(datas$SPF.t.t.1.SLID[,ii]))$MZpval
# }
# MZpvals
# http://stats.stackexchange.com/questions/251253/mincer-zarnowitz-for-model-combination-how-to-construct




#### 3 ####
  

# DMtest in Stata
# DMtest in R
dm.test(datas$SPF.t.t.1,datas$Greenbook.t.t.1,h=1,power=1)


#### 4 - Instability ####
## Also known as Chow test for structural change
##install.packages("strucchange")
library(strucchange)
sctest(reg.model1)
sctest(reg.model2)

library(xlsx)
library(forecast)
library(tseries)
library(strucchange)

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
# prod_df <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 8:65, colIndex = 2, header = FALSE)
# colnames(prod_df) <- c('Rice')
## store rice data as time series objects
strinfl <- ts(datas$Inflation.rate, start=c(1969, 1), end=c(2011, 1), deltat=1/4) 
# store the breakpoints
bp.strinfl <- breakpoints(strinfl ~ 1)
summary(bp.strinfl)
## the BIC chooses 5 breakpoints; plot the graph with breakdates and their confidence intervals
plot(bp.strinfl)
plot(strinfl)
lines(bp.strinfl)
## confidence intervals
ci.strinfl <- confint(bp.strinfl)
ci.strinfl
lines(ci.strinfl)

require(strucchange)
#1976
sctest(datas$Inflation.rate ~ datas$observation, type = "Chow", point = 33)
#1981
sctest(datas$Inflation.rate ~ datas$observation, type = "Chow", point = 56)
#1991
sctest(datas$Inflation.rate ~ datas$observation, type = "Chow", point = 96)
#2008
sctest(datas$Inflation.rate ~ datas$observation, type = "Chow", point = 161)





#### 5 - SPF1 vs SPF2 ####

accuracy(datas$SPF.t.t.1.,datas$Inflation.rate)
accuracy(datas$SPF.t.t.2.,datas$Inflation.rate)

# library(lmtest)
# encomptest(datas$SPF.t.t.1.,datas$SPF.t.t.2.)
# anova(datas$SPF.t.t.1.,datas$SPF.t.t.2.)
#dm in stata

library(stargazer)
reg.model5 <- lm(datas$Inflation.rate ~ datas$SPF.t.t.1. + datas$SPF.t.t.2.)
summary(reg.model5)
stargazer(reg.model5, title="Results", align=TRUE, out="reg5.txt")
linearHypothesis(reg.model5,c("datas$SPF.t.t.1. = 0","datas$SPF.t.t.2. = 1"),test="F")




