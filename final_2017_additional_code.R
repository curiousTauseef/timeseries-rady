library(forecast)
library(ggplot2)
train <- window(co2, end=c(1990,12))
test <- window(co2, start=c(1991,1))
h <- length(test)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, lambda=0), h=h)
STL <- stlf(train, lambda=0, h=h)
X <- cbind(ETS=ETS$mean, ARIMA=ARIMA$mean, STL=STL$mean)X
df <- cbind(co2, X)
colnames(df) <- c("Data","ETS","ARIMA","STL")
autoplot(df) + 
  xlab("Year") + ylab(expression("Atmospheric concentration of CO"[2]))

library(opera)
MLpol0 <- mixture(model = "MLpol", loss.type = "square")
weights <- predict(MLpol0, X, test, type='weights')
head(weights)

write.csv(X, "X.csv")

#####################

library(forecastHybrid)
quickModel <- hybridModel(AirPassengers)
plot(forecast(quickModel), main = "Forecast from auto.arima, ets, thetam, nnetar, stlm, and tbats model")

A <- AirPassengers
C <- co2

now <- Sys.time()
tseq <- seq(from = now, length.out = 100, by = "quarters")
length(tseq)
tseq

######################

library(quantmod);
library(rugarch); 
library(car)
library(forecast);  
library(lawstat)

end<- format(Sys.Date(),"%Y-%m-%d") 
start<-format(Sys.Date() - 5*365,"%Y-%m-%d") # Most recent 5 years
dat0 = as.matrix(getSymbols('SPY', src="google", from=start, to=end, auto.assign = F, warnings = FALSE,symbol.lookup = F))
n = NROW(dat0)
ret <- NULL
ret[2:n] <- dat0[2:n,4]/dat0[1:(n-1),4] - 1 # close to close
plot(ret, ty = "l")
ret = ret[-1]  #-1 to drop the NA
gjrtspec <- ugarchspec(mean.model=list(armaOrder=c(1,0)),variance.model =list(model = "gjrGARCH"),distribution="std") 
normspec <- ugarchspec(mean.model=list(armaOrder=c(1,0)), distribution="norm") 
Tgjrmodel = ugarchfit(gjrtspec,ret) 
Nmodel = ugarchfit(normspec,ret)
Tgjrfit = as.data.frame(Tgjrmodel)$sigma
Nfit = as.data.frame(Nmodel)$sigma
ret.sq = ret^2
N.sq = Nfit^2
Tgjr.sq = Tgjrfit^2
plot(ret.sq, ty = "l", ylim = c(0,.004)) ; lines(Tgjr.sq, col = 2) ; lines(N.sq, col = 3)
#########################
# option one - Mincer Zarnowitz
#########################

####################################################

library(xlsx)
library(forecast)
library(tseries)
library(strucchange)

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
prod_df <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 8:65, colIndex = 2, header = FALSE)
colnames(prod_df) <- c('Rice')
## store rice data as time series objects
rice <- ts(prod_df$Rice, start=c(1951, 1), end=c(2008, 1), frequency=1) 

# store the breakpoints
bp.rice <- breakpoints(rice ~ 1)
summary(bp.rice)

## the BIC chooses 5 breakpoints; plot the graph with breakdates and their confidence intervals
plot(bp.rice)
plot(rice)
lines(bp.rice)

## confidence intervals
ci.rice <- confint(bp.rice)
ci.rice
lines(ci.rice)



