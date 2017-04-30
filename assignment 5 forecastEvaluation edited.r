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

##install.packages('limSolve')
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

datas3 = as.matrix(read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Goyal_Welch_data(3).xlsx',1))

estimationEnd = 516

X = datas3[,c(6, 7, 8, 9, 12, 13, 14, 16, 17, 18, 19)]
y = datas3[,5]
TT = length(y)
prevailingMean = matrix(0,TT-estimationEnd,1)
forecastss = matrix(0,TT-estimationEnd,11)
kitchenSink = matrix(0,TT-estimationEnd,1)


for (t in seq(estimationEnd,TT-1)) {
  for(ii in 1:11){
    forecastss[t-estimationEnd+1,ii] = sum(lm(y[2:t]~1+X[1:(t-1),ii])$coefficients * c(1,X[t,ii]))
  }
  prevailingMean[t-estimationEnd+1,1] = mean(y[2:t])
  kitchenSink[tt-estimationEnd+1] = forecasts[length(forecastss)];
}

modelForecasts = cbind(prevailingMean, forecastss, kitchenSink)
modelForecastsErrors = y[-seq(1,estimationEnd)]-modelForecasts
RMSE = sqrt(colMeans(modelForecastsErrors^2))
RMSE

# Diebold-Mariano tests
DMstats = matrix(0,11,1)
for(ii in 1:11){
  temp = dmtest(modelForecastsErrors[,1],modelForecastsErrors[,ii+1],1)
  DMstats[ii,1] = temp[1]
}
DMstats

# Mincer_Zarnowitz tests

MZpvals = numeric(12)
for(ii in 1:12){
  MZpvals[ii] = MZtest(y[-seq(1,estimationEnd)],as.matrix(modelForecasts[,ii]))$MZpval
}
MZpvals

# Directional Accuracy Tests
library(rugarch)
md1 <- modelForecasts[,1]
rl1 <- y[-seq(1,516)]
DACTest(md1, rl1, test = "PT", conf.level = 0.95)




data(dji30ret)
spec = ugarchspec(mean.model = list(armaOrder = c(6,1), include.mean = TRUE),
                  variance.model = list(model = "gjrGARCH"), distribution.model = "nig")
fit = ugarchfit(spec, data = dji30ret[, 1, drop = FALSE], out.sample = 1000)
pred = ugarchforecast(fit, n.ahead = 1, n.roll = 999)
# Get Realized (Oberved) Data
obsx = tail(dji30ret[,1], 1000)
forc = as.numeric(as.data.frame(pred,rollframe="all",align=FALSE,which="series"))
print(DACTest(forc, obsx, test = "PT", conf.level = 0.95))
print(DACTest(forc, obsx, test = "AG", conf.level = 0.95))
