/// Rady - Business Forecasting Final
/// Anton Prokopyev
// http://www.ssc.wisc.edu/~bhansen/390/390Lecture22.pdf
// http://www.ssc.wisc.edu/~bhansen/390/
clear
set more off
//ssc install dmariano

import delimited "/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Final_datas1_from_R.csv", encoding(ISO-8859-1)

tsset v1
dmariano inflation spftt1 greenbooktt1 , crit(MSE)
dmariano inflation spftt1 spftt2, crit(MSE)



////////////////////////


clear
set more off
import excel "/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Final_datas2_forStata.xlsx", sheet("Sheet1") firstrow clear

gen qtr=yq(date,quarter)
tsset qtr, quarterly


//help fcast compute
//arch Inflationrate X3monthTbillrate X5yearTreasuryyield unemplRate SP500index, arch(1/1)
//var Inflationrate X3monthTbillrate X5yearTreasuryyield unemplRate SP500index
//set level 90
//graph twoway (lfitci LASSO qtr, xlab(,format(%tq))) (scatter LASSO qtr, xlab(,format(%tq)))
//statsby upper=r(ub) lower=r(lb), clear  by(qtr): ci LASSO, poisson


// q 7

gen super = ((SPFtt1+Greenbooktt1)/2)

dmariano Inflation super SPFtt1, crit(MSE)
dmariano Inflation super Greenbooktt1, crit(MSE)


// question 8

dmariano Inflation LASSO AIC, crit(MSE)
dmariano Inflation LASSO BIC, crit(MSE)
dmariano Inflation LASSO Forward, crit(MSE)
dmariano Inflation LASSO Backward, crit(MSE)
dmariano Inflation LASSO Combination, crit(MSE)
dmariano Inflation LASSO Kitchen, crit(MSE)
dmariano Inflation LASSO PM, crit(MSE)




// question 9

dmariano Inflation SPFtt1 AIC, crit(MSE)
dmariano Inflation SPFtt1 BIC, crit(MSE)
dmariano Inflation SPFtt1 Forward, crit(MSE)
dmariano Inflation SPFtt1 Backward, crit(MSE)
dmariano Inflation SPFtt1 LASSO, crit(MSE)
dmariano Inflation SPFtt1 Combination, crit(MSE)
dmariano Inflation SPFtt1 Kitchen, crit(MSE)
dmariano Inflation SPFtt1 PM, crit(MSE)

dmariano Inflation Greenbooktt1 AIC, crit(MSE)
dmariano Inflation Greenbooktt1 BIC, crit(MSE)
dmariano Inflation Greenbooktt1 Forward, crit(MSE)
dmariano Inflation Greenbooktt1 Backward, crit(MSE)
dmariano Inflation Greenbooktt1 LASSO, crit(MSE)
dmariano Inflation Greenbooktt1 Combination, crit(MSE)
dmariano Inflation Greenbooktt1 Kitchen, crit(MSE)
dmariano Inflation Greenbooktt1 PM, crit(MSE)

