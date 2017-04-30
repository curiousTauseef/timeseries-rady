### Business Forecasting ###
### Final Exam Part 2###
### Anton Prokopyev ###

#### Setup ####
rm(list = ls())
cat("\f")
library(xlsx)


#### 6 ####

##install.packages('ForecastCombinations')
library(ForecastCombinations)
library(forecast)
datas <- read.xlsx('Final_2017_data_cleaned.xlsx',1)
datas$observation <- 1:nrow(datas) 
##datas <- datas[-c(1:4,196:198), -c(1)] if using original uncleaned file
datas <- (datas[-c(1:4,173:198),])

datas$combined <- as.ts((datas$SPF.t.t.1.+datas$Greenbook.t.t.1.)/2)
accuracy(datas$combined,datas$Inflation.rate)
accuracy(datas$SPF.t.t.1.,datas$Inflation.rate)
accuracy(datas$Greenbook.t.t.1.,datas$Inflation.rate)
# or make a fancy weighted combination
# library(opera)
# strinfl <- ts(datas, start=c(1980, 1), end=c(2011, 1), deltat=1/4) 
# idx_data_test <- 44:nrow(datas)
# train <- window(strinfl, end=c(1990,1))
# test <- window(strinfl, start=c(1991,1))
# MLpol0 <- mixture(model = "MLpol", loss.type = "square")
# X <- cbind(SPF=datas$SPF.t.t.1., Greenbook=datas$Greenbook.t.t.1., Combined=datas$combined)
# X <- ts(X[-c(1:87),])
# # write.csv(datas, "X1.csv")
# # X <- read.csv('X.csv',1)
# # X <- cbind(SPF=X$SPF.t.t.1., Greenbook=X$Greenbook.t.t.1., Combined=X$combined)
# weights <- predict(MLpol0, X, test, type='weights')
# head(weights)
# tail(weights)
# z <- ts(predict(MLpol0, X, test, type='response'), start=c(1991,1), freq=4)
# df <- cbind(co2, z)
# colnames(df) <- c("Data","Mixture")
# autoplot(df) + 
#   xlab("Year") + ylab(expression("Atmospheric concentration of CO"[2]))






#### 7 Setup ####
#Loading packages
library(rJava)
library(xlsx)
library(glmnet)
library(ggplot2)
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

#### 7 Actual ####




datas <- read.xlsx('Final_2017_data_cleaned.xlsx',1)
##datas <- datas[-c(1:4,196:198), -c(1)] if using original uncleaned file
datas <- datas[-c(1:4,173:198),]
temp = sapply(datas, is.na)  
datas = datas[-seq(1,max(colSums(as.matrix(temp)))),]
datas = as.matrix(datas)
##datas = as.matrix(datas)[-(1:max(sum(temp))),]
X = datas[,c(8, 9, 10, 11)]
y = datas[,3]

datas[44,]
estimationEnd = 44
TT = nrow(X)

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
informationCriteriaCounts = matrix(0,2,4);
modelIndices = as.matrix(expand.grid(data.frame(rbind(numeric(4),numeric(4)+1))));

# Construct recursive forecasts
# You'll need to add some code that figures out which variables are
# selected using your preferred model selection method each time period

for (tt in seq(estimationEnd,TT-1)) {
  print(TT-tt)    #display how many iterations are left to track progress, estimation should take several minutes
  forecasts = Forecasts2K(X[1:(tt-1),],y[2:tt],X[tt,])$AllF     #constructs forecasts from every combination of predictors
  criteria = Forecasts2K(X[1:(tt-1),],y[2:tt],X[tt,])$Criteria
  minIndices = apply(criteria,2,which.min)
  informationCriteriaCounts = informationCriteriaCounts+modelIndices[as.vector(minIndices),]
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
datas = read.xlsx('Final_2017_data_cleaned.xlsx',1)
datas <- datas[-c(1:4,173:198),]

timeVector = seq(1980,2011,length.out =  TT-estimationEnd)  #you need to change this
matplot(timeVector,forecasts,type = 'l',col=palette(),lty=1,xlab = 'Time',ylab = 'Inflation')
matlines (datas$Inflation.rate, lty = 45:124, type = "l", col = "black")
legend(2000.4,14.5,modelLabels[-1],col=palette(),lty = rep(1,8))

# Save as csv to review in Excel
write.csv(datas, "Final_datas2.csv")
write.csv(forecasts, "Final_datas2_forecasts.csv")

#Compute model RMSEs (root mean squared errors)
RMSE = sqrt(colMeans(forecastsErrors^2))
modelLabels[-1]
RMSE

accuracy(datas$combined,datas$Inflation.rate)
accuracy(datas$SPF.t.t.1.,datas$Inflation.rate)
accuracy(datas$Greenbook.t.t.1.,datas$Inflation.rate)

forecasts1 <- as.data.frame(forecasts)
accuracy(forecasts1$V1,datas$Inflation.rate)
accuracy(forecasts1$V2,datas$Inflation.rate)
accuracy(forecasts1$V3,datas$Inflation.rate)
accuracy(forecasts1$V4,datas$Inflation.rate)
accuracy(forecasts1$V5,datas$Inflation.rate)
accuracy(forecasts1$V6,datas$Inflation.rate)
accuracy(forecasts1$V7,datas$Inflation.rate)
accuracy(forecasts1$V8,datas$Inflation.rate)

##install.packages('MTS')
library(MTS)
apca(forecasts, 4)

forlasso90 <- read.xlsx('Final_datas2_forecasts_lassoresid.xlsx',1)

library(DataCombine)
forlasso90 <- slide(forlasso90, Var = "lasso", NewVar = "LASSO.SLID", slideBy = 1)
plot(forlasso90$lasso, forlasso90$LASSO.SLID)
reg.model77 <- lm(forlasso90$inflations.1~forlasso90$LASSO.SLID)
summary(reg.model77)
#intercept = 0
library(car)
linearHypothesis(reg.model77,c("forlasso90$LASSO.SLID = 1","(Intercept) = 0"),test="F")





#### 8 Predictive accuracy ####

dm.test(e1, e2, alternative=c("two.sided","less","greater"), h=1, power=2)

#### 9 95% confidence ####

library(plotrix)
forlasso90 <- read.xlsx('Final_datas2_forecasts_lassoresid.xlsx',1)

plot.ts(forlasso90$inflations.1, plot.conf=FALSE,
        main="Forecasts of quarterly inflation rate", type = "l",
        xlab="Time period", ylab="Inflation rate", col="red")
##axis(1, datas$Date)
lines(forlasso90$upprer90CI,col='blue')
lines(forlasso90$lasso, col='blue')
lines(forlasso90$lower90CI,col='blue')
legend("topright", lty=1, col=c('blue','red','red'),
       legend=c("LASSO with 90% Interval","Realized values"))

library(rugarch)
DACTest(forlasso90$inflations.1, forlasso90$lasso, test = "PT", conf.level = 0.90)


#### 10 ####
library(xlsx)
library(tseries)
library(egcm)
library(fGarch)
library(FinTS)
library(rugarch)
library(devtools)

# Test for ARCH effects in inflation
ArchTest(datas$Inflation.rate,lags = 1,demean = T)
mrets = datas$Inflation.rate-mean(datas$Inflation.rate)
rets = datas$Inflation.rate

# GARCH model for inflation modeling mean of inflation as a constant
spec1 = ugarchspec(variance.model = list(model='fGARCH', submodel = 'GARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=F),distribution.model = "norm")
fgarch.fit = ugarchfit(spec1,mrets) #estimated model
infocriteria(fgarch.fit)           #information criteria

spec2 = ugarchspec(variance.model = list(model='eGARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=T),distribution.model = "norm")
egarch.fit = ugarchfit(spec2,rets)
infocriteria(egarch.fit)

spec3 = ugarchspec(variance.model = list(model='fGARCH', submodel = 'GJRGARCH', garchOrder = c(1,1)),mean.model = list(armaOrder=c(0,0),include.mean=T),distribution.model = "norm")
gjrgarch.fit = ugarchfit(spec3,rets)
infocriteria(gjrgarch.fit)

plot(egarch.fit)

