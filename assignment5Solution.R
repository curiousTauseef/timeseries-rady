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



##Part 1
datas = read.xlsx('Alcoa.xlsx',1,rowIndex = 3:41)
forecastErrors = as.matrix(datas)[,-1]

pVals = numeric(12)
for (ii in 1:12) {
  pVals[ii] = summary(lm(forecastErrors[,ii]~1))$coefficients[4]
  
}

pVals #each value corresponds to a p-value of the test that the mean of the forecast at each horizon is equal to 0, i.e. rejection means forecast is biased

#Calculate MFE and MSFE

matplot(colMeans(forecastErrors),type='l',xlab = 'Forecast horizon',main = 'Mean forecast errors')
matplot(colMeans(forecastErrors^2),type='l',xlab = 'Forecast horizon',main = 'Mean squared forecast errors')

# Compare performance of different forecast horizons
dmtest(forecastErrors[,1],forecastErrors[,12],1)

# Comparison against AR(1) model, note we are comparing to in-sample
# predicted values

datas2 = read.xlsx('Alcoa.xlsx','forecasts',rowIndex = 3:41)
actualValues = as.matrix(datas2)[,14]
arForecast = lm(actualValues[-1]~1+actualValues[-length(actualValues)])$fitted
arForecastErrors = lm(actualValues[-1]~1+actualValues[-length(actualValues)])$resid

# Compare RMSFE

sqrt(mean(arForecastErrors^2))
sqrt(colMeans(forecastErrors^2))

# Diebold-Mariano test

dmtest(arForecastErrors,forecastErrors[-1,12],12)
dmtest(arForecastErrors,forecastErrors[-1,12],1)


### Part 2

datas3 = as.matrix(read.xlsx('Goyal_Welch_data.xlsx',1))
temp = apply(datas3,2, is.na)
datas3 = datas3[-(1:max(colSums(temp))),]
estimationEnd = 517

X = datas3[,c(6, 7, 8, 9, 12, 13, 14, 16, 17, 18, 19)]
y = datas3[,5]
TT = length(y)

forecastss = matrix(0,TT-estimationEnd,13)

for (t in seq(estimationEnd,TT-1)) {
  for(ii in 1:11){
    forecastss[t-estimationEnd+1,ii] = sum(lm(y[2:t]~1+X[1:(t-1),ii])$coefficients * c(1,X[t,ii]))
  }
  forecastss[t-estimationEnd+1,12] = sum(lm(y[2:t]~1+X[1:(t-1),])$coefficients * c(1,X[t,]))
  forecastss[t-estimationEnd+1,13] = mean(y[2:t]);
}

modelForecasts = cbind(rowMeans(forecastss[,1:11]),forecastss[,12:13])

# Compute RMSFE

modelForecastsErrors = y[-seq(1,estimationEnd)]-modelForecasts
sqrt(colMeans(modelForecastsErrors^2))

# Mincer_Zarnowitz tests

MZpvals = numeric(3)
for(ii in 1:3){
  MZpvals[ii] = MZtest(y[-seq(1,estimationEnd)],as.matrix(modelForecasts[,ii]))$MZpval
}

## Directional accuracy tests, see forecast evaluation slides 22-23

directionBoth = y[-seq(1,estimationEnd)]*modelForecasts>0
directionF = modelForecasts>0
directionY = y[-seq(1,estimationEnd)]>0
pHat = colMeans(directionBoth)
pHatY = mean(directionY)
pHatF = colMeans(directionF)
TOut = length(directionY)

signPvals = numeric(3)

for (ii in 1:3) {
  pHatStar = pHatY*pHatF[ii] + (1-pHatY)*(1-pHatF[ii]);
  varPHat = (1/TOut)*pHatStar*(1-pHatStar);
  varPStar = (1/TOut)*((2*pHatY-1)^2)*pHatF[ii]*(1-pHatF[ii]) + (1/TOut)*((2*pHatF[ii]-1)^2)*pHatY*(1-pHatY) + (4/(TOut^2))*pHatY*pHatF[ii]*(1-pHatY)*(1-pHatF[ii]);
  signStat = (pHat[ii]-pHatStar)/sqrt(varPHat - varPStar);
  if (is.na(signStat)){
    if(pHat[ii]>pHatStar){
      signStat = Inf
    }else{
      signStat=-Inf
    }
  }
  signPvals[ii] = 1-pnorm(signStat)
}

signPvals# p-value from one sided test, i.e. rejection implies directional accuracy

# DM test comparison, relative to prevailing mean model

dmtest(modelForecastsErrors[,3],modelForecastsErrors[,1],1)
dmtest(modelForecastsErrors[,3],modelForecastsErrors[,2],1)



