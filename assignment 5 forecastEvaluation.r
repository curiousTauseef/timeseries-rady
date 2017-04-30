## Assignment 5
rm(list = ls())
cat("\f")

library(xlsx)

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

### Forecast evaluation example

datas3 = as.matrix(read.xlsx('Goyal_Welch_data.xlsx',1))

estimationEnd = 516

X = datas3[,c(6, 7, 8, 9, 12, 13, 14, 16, 17, 18, 19)]
y = datas3[,5]
TT = length(y)
prevailingMean = matrix(0,TT-estimationEnd,1)
forecastss = matrix(0,TT-estimationEnd,11)

for (t in seq(estimationEnd,TT-1)) {
  for(ii in 1:11){
    forecastss[t-estimationEnd+1,ii] = sum(lm(y[2:t]~1+X[1:(t-1),ii])$coefficients * c(1,X[t,ii]))
  }
  prevailingMean[t-estimationEnd+1,1] = mean(y[2:t]);
}

modelForecasts = cbind(prevailingMean, forecastss)
modelForecastsErrors = y[-seq(1,estimationEnd)]-modelForecasts
RMSE = sqrt(colMeans(modelForecastsErrors^2))

# Diebold-Mariano tests
DMstats = matrix(0,11,1)
for(ii in 1:11){
  temp = dmtest(modelForecastsErrors[,1],modelForecastsErrors[,ii+1],1)
  DMstats[ii,1] = temp[1]
}


# Mincer_Zarnowitz tests

MZpvals = numeric(12)
for(ii in 1:12){
  MZpvals[ii] = MZtest(y[-seq(1,estimationEnd)],as.matrix(modelForecasts[,ii]))$MZpval
}
