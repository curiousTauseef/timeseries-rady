*! wpicmdnocov version 1.0.0  R. A. Yaffee wpi forecast with no covariates
*------------------------------------------------------------------------------------------------
* caveat:   This version program does not automatically perform conditional forecasting,
*           where the user is seeking to use a regression model for his forecasting.
*           If the user wishes to perform conditional forecasting, with such a model,
*           he must first forecast all of the explanatory variables or event indicators
*           over the validation by himself and include those in the dataset to be used 
*           for the model to generate the one-step-ahead forecasts.
*************************************************************************************************

// command syntax:  owpicmdnocov  datasetname seriesname {point of forecast origin}
// eg: owpicmdnocov wp2b dlwpi 116

drop _all           
capture drop oforecast
capture program drop owpicmdnocov
program owpicmdnocov, rclass
version 12
use `1', clear
tsset t
capture confirm variable segment
if _rc==111{
gen segment=0
}
replace segment = 1 if _n >= `3'
count if segment==1
loc h r(N)

capture confirm variable dlwpi
if _rc==111 {
gen dlwpi = d.ln_wpi
}
arima dlwpi if segment==0, ar(1 2 4) ma(6) noconstant
est store model1
capture confirm variable oforecast
if _rc==111 {
predict oforecast 
}
capture confirm variable fcvar
if _rc==111 {
cap predict fvar, mse
}
gen ub = oforecast + 1.96*sqrt(fvar)
gen lb = oforecast  - 1.96*sqrt(fvar)
replace ub = . if segment==0
replace lb = . if segment==0
label var ub "Upper 95% pred. conf. limit"
label var lb "Lower 95% pred. conf. limit"
tsline `2' oforecast ub lb, tline(`3') ytitle(`2') ///
 title(Ex post forecast profile of `2') ttitle(date) ///
 color(green blue red red) lpattern(solid londash dash dash)
qui est replay model1
oforeval dlwpi 116 4
end
owpicmdnocov wpi2b dlwpi 116
