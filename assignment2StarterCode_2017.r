# Assignment 2: Model Selection
rm(list = ls())
cat("\f")

#Loading packages
library(rJava)
library(xlsx)
library(glmnet)

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


#Readin data
datas = read.xlsx('/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Goyal_Welch_data.xlsx',1,colIndex = 1:20)
temp =sapply(datas, is.nan)  
datas = as.matrix(datas)[-(1:max(sum(temp))),]

X = datas[-seq(1,nrow(datas)-240),c(6,7,8)] # only use last 20 years of data and 3 predictor variables, you need to change this
y = datas[-seq(1,nrow(datas)-240),5]      #   you need to change this

estimationEnd = 120 #% amount of data to use for initial forecast, 10 years here, you need to change this
TT = nrow(X) # sample size

#Initialize data structures

modelLabels = c('Data','AIC','BIC','Forward stepwise','Backward stepwise','Lasso','Combination','Kitchen Sink','PM')

AICforecast = matrix(0,TT-estimationEnd,1)
BICforecast = matrix(0,TT-estimationEnd,1)
forwardStepForecast = matrix(0,TT-estimationEnd,1)
backStepForecast = matrix(0,TT-estimationEnd,1)
lassoForecast = matrix(0,TT-estimationEnd,1)
combinationForecast = matrix(0,TT-estimationEnd,1)
kitchenSink = matrix(0,TT-estimationEnd,1)
prevailingMean = matrix(0,TT-estimationEnd,1)

# Construct recursive forecasts
# You'll need to add some code that figures out which variables are
# selected using your preferred model selection method each time period

for (tt in seq(estimationEnd,TT-1)) {
  print(TT-tt)    #display how many iterations are left to track progress, estimation should take several minutes
  forecasts = Forecasts2K(X[1:(tt-1),],y[2:tt],X[tt,])$AllF     #constructs forecasts from every combination of predictors
  criteria = Forecasts2K(X[1:(tt-1),],y[2:tt],X[tt,])$Criteria
  minIndices = apply(criteria,2,which.min)
  cv = cv.glmnet(X[1:(tt-1),],y[2:tt],nfolds = 10)           #estimate a LASSO regression and keep one with lowest MSE using 10-fold cross-validation
  IndexMinMSE = (as.vector(coef(cv,s='lambda.min'))!=0)[-1]
  bLasso = numeric(ncol(X)+1)
  trainset = cbind(rep(1,tt-1),X[1:(tt-1),IndexMinMSE])
  tempLasso = OLS(trainset,y[2:tt])$Beta
  bLasso[which(IndexMinMSE)+1] = tempLasso[-1]
  bLasso[1] = tempLasso[1]
  statsForward=step(lm(y[2:tt]~.,as.data.frame(X[1:(tt-1),])), direction="backward",test='F',trace = 0) #backward stepwise model selection
  statsBack=step(lm(y[2:tt]~1,data=as.data.frame(X[1:(tt-1),])), direction="forward",test='F',formula(lm(y[2:tt]~.,as.data.frame(X[1:(tt-1),]))),trace=0) #forward stepwise model selection
  IndexForward=colnames(X)%in%names(coef(statsForward))
  IndexForward=colnames(X)%in%names(coef(statsBack))
  
  AICforecast[tt-estimationEnd+1,] = forecasts[minIndices[1]] #highest AIC forecast
  BICforecast[tt-estimationEnd+1,] = forecasts[minIndices[2]] #highest BIC forecast
  
  now = data.frame(X)[tt,]
  forwardStepForecast[tt-estimationEnd+1] = predict(statsForward,newdata=now) #forward stepwise forecast
  backStepForecast[tt-estimationEnd+1] = predict(statsBack,newdata=now)       #backward stepwise forecast
  combinationForecast[tt-estimationEnd+1] = mean(forecasts)                   #equal-weighted average of all model forecasts
  lassoForecast[tt-estimationEnd+1] = sum(c(1,X[tt,])*bLasso)                 #min MSE Lasso forecast
  kitchenSink[tt-estimationEnd+1] = forecasts[length(forecasts)]              #kitchen sink model forecast
  prevailingMean[tt-estimationEnd+1] = mean(y[2:tt])                          #prevailing mean forecast
}

#Aggregate forecasts into matrix and construct forecast errors
forecasts = cbind(AICforecast, BICforecast, forwardStepForecast, backStepForecast, combinationForecast, lassoForecast, kitchenSink, prevailingMean)
forecastsErrors = y[-seq(1,TT-estimationEnd)]-forecasts

#Plot model forecasts
timeVector = seq(1996,2015+11/12,length.out =  TT-estimationEnd)  #you need to change this
matplot(timeVector,forecasts,type = 'l',col=palette(),lty=1,xlab = 'Time',ylab = 'Percentage Points')
legend(min(timeVector),-0.01,modelLabels[-1],col=palette(),lty = rep(1,8))

#Compute model RMSEs (root mean squared errors)
RMSE = sqrt(colMeans(forecastsErrors^2))

# Calculate economic performance of models (relative to risk free rate)
riskFree = datas[-seq(1,nrow(datas)-240),15] #you need to change this
stockReturn = datas[-seq(1,nrow(datas)-240),ncol(datas)] # you need to change this
economicReturns = matrix(0,nrow(forecasts),ncol(forecasts))

for (ii in seq(1,ncol(forecasts))) {
  negativeReturns = forecasts[,ii]<=0
  economicReturns[,ii] = riskFree[-seq(1:estimationEnd)]*negativeReturns+stockReturn[-seq(1:estimationEnd)]*(!negativeReturns)
}

# Compute mean returns and Sharpe Ratios

meanReturns = colMeans(economicReturns)
sharpeRatios = (meanReturns-mean(riskFree[-seq(1,estimationEnd)]))/apply(economicReturns, 2, sd)
save.image(file = "assignment2.rda")
#save.image(file = "assignment2.rda") # use this command to save your results