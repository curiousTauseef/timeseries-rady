## Assignment 5
rm(list = ls())
cat("\f")

library(xlsx)
library(limSolve)

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

#Define OLS
OLS <- function(X0,y0,varargin){
  Beta = solve(t(X0)%*%X0)%*%t(X0)%*%y0
  yhat = X0%*%Beta        # fitted y
  nobs = nrow(X0)    
  nreg = ncol(X0)
  res = y0-yhat                          #Residuals
  shat = sum(res^2)/(nobs-nreg)          #variance of residuals
  varcovar = shat*solve(t(X0)%*%X0)      #var-covar matrix
  tstat = Beta/ sqrt(diag(varcovar))
  ESS = sum(res^2)
  TSS = sum((y-mean(y))^2)
  Rq = 1-ESS/TSS                         #Rq
  stilda = ESS/(nobs-(nreg-1)-1)
  S2 = (TSS)/(nobs-1)
  Rqad = 1-stilda/S2                               #Rqadj (maximize)
  Aic = log((ESS/nobs))+(2*nreg)/nobs              #Akaike (minimize)
  Bic = log((ESS/nobs))+nreg*log(nobs)/nobs        #Bic    (minimize)
  HQ = log((ESS/nobs))+2*nreg*log(log(nobs))/nobs  #HQ     (minimize)
  
  S = list(Beta,yhat,tstat,res,shat,varcovar,Rq,Rqad,Aic,Bic,HQ)
  names(S) = c('Beta','yhat','tstat','res','varRes','varcovarBeta','Rq','Rqadj','Akaike','Bic','HQ')
  
  S
}


#Define Forecasts2K
Forecasts2K <- function(X0,y0,Xfor0){
  # This function generates all possible 2^K forecasts using a TxK matrix of
  # predictors, X, and a Tx1 univariate dependent variable, y
  # Note the timing: X(t-1,:), y(t), Xfor(t,:) go in each row
  # X does not contain an intercept, but an intercept is always included in the
  # regression
  Nvar = ncol(X0)  #number of X-variables
  AllModels = as.matrix(expand.grid(data.frame(rbind(numeric(Nvar),numeric(Nvar)+1))))  # all permutations of X-variables
  Criteria = matrix(NaN,nrow(AllModels),2) 
  AllF = rep(NaN,nrow(AllModels))
  intercept = rep(1,length(y0))
  nmodels = nrow(AllModels)      #number of models;
  
  for (j in 1:nmodels) {
    model = which((AllModels[j,])!=0)
    REG = OLS(cbind(intercept,X0[,model]),y0)
    Criteria[j,1] = REG$Akaike
    Criteria[j,2] = REG$Bic
    AllF[j] = t(c(1,Xfor0[model]))%*% REG$Beta
  }
  S = list(AllF,Criteria)
  names(S) = c('AllF','Criteria')
  S
}


### Forecast evaluation example

datas3 = as.matrix(read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Goyal_Welch_data(3).xlsx',1))

estimationEnd = 516

X = datas3[,c(6, 7, 8, 9, 12, 13, 14, 16, 17, 18, 19)]
y = datas3[,5]
TT = length(y)
prevailingMean = matrix(0,TT-estimationEnd,1)
forecastss = matrix(0,TT-estimationEnd,11)
kitchenSink = matrix(0,TT-estimationEnd,1)
combinationForecast = matrix(0,TT-estimationEnd,1)


for (t in seq(estimationEnd,TT-1)) {
  for(ii in 1:11){
    forecastss[t-estimationEnd+1,ii] = sum(lm(y[2:t]~1+X[1:(t-1),ii])$coefficients * c(1,X[t,ii]))
    forecasts = Forecasts2K(X[1:(t-1),],y[2:t],X[t,])$AllF     #constructs forecasts from every combination of predictors
    
  }
  prevailingMean[t-estimationEnd+1,1] = mean(y[2:t]);
  kitchenSink[t-estimationEnd+1,1] = forecasts[length(forecasts)]              #kitchen sink model forecast
  combinationForecast[t-estimationEnd+1] = mean(forecastss)                   #equal-weighted average of all model forecasts
  
  
}

save(kitchenSink, file="kitchenSink.RData")
load("kitchenSink.RData")



modelForecasts = cbind(prevailingMean, kitchenSink, combinationForecast)
modelForecastsErrors = y[-seq(1,estimationEnd)]-modelForecasts
RMSE = sqrt(colMeans(modelForecastsErrors^2))

# Diebold-Mariano tests
DMstats = matrix(0,4,1)
for(ii in 1:4){
  temp = dm.test(modelForecastsErrors[,1],modelForecastsErrors[,ii+1],1)
  DMstats[ii,1] = temp[1]
}
DMstats

library(forecast)
dm.test(modelForecastsErrors[,1],modelForecastsErrors[,2],h=1)
dm.test(modelForecastsErrors[,2],modelForecastsErrors[,1],h=1)


# Mincer_Zarnowitz tests

MZpvals = numeric(3)
for(ii in 1:3){
  MZpvals[ii] = MZtest(y[-seq(1,estimationEnd)],as.matrix(modelForecasts[,ii]))$MZpval
}
