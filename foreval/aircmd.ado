* aircmd.do  version 2.0.1   program to automate the ex ante forecast evaluation
* data and programs required for this module to function properly
* 1. Stata vers 12.0
* 2. airtrans2.dta   transformations are in this data file
* 3. foreval.ado and foreval.sthlp
* 4.             

cap program drop aircmd
program aircmd,rclass
cap log close _all
log using airmodeval, replace
use transair2, clear
cap drop time
capture confirm variable date
if _rc == 111 {
gen date = m(1949m1) + t - 1
format date %tm
tsset date
}
capture confirm variable ds12lair
if _rc== 111 {
gen lair = ln(air)
gen s12lair = s12.lair
gen ds12lair = d.s12lair
}

cap drop time


capture confirm variable segmnt
if _rc==111 {
gen segmnt=0
}
tsappend, add(24)

replace segmnt = 1 if t > 120
arima ds12lair if segmnt==0, ma(1) mma(1,12) nolog
est store model1
predict forecast2
predict forecast 
replace forecast = . if segmnt == 0
scalar z = invnormal(.975)
scalar z90 = invnormal(.95)
predict fvar, mse
gen ub = forecast + z*sqrt(fvar) if segmnt==1
gen ub90 = forecast + z90*sqrt(fvar) if segmnt==1
gen lb90 = forecast - z90*sqrt(fvar) if segmnt==1
gen lb = forecast - z*sqrt(fvar) if segmnt==1
label var ub90 "upper 90% forec. conf.limit"
label var lb90 "lower 90% forec. conf.limit"
cap label var forecast_90ub "Upper 90% prediction conf. limit"
cap label var forecast_90lb "Lower 90% prediction conf. limit"
cap label var forecast_UB "Upper 95% prediction conf. limit"
cap label var forecast_LB "Lower 95% prediction conf. limit"
label var ub "Upper 95% prediction conf. limit"
label var lb "Lower 95% prediction conf. limit"
tsset date
tsline ds12lair forecast ub ub90 lb90 lb, tline(120) ///
  title(Ex ante forecast of air passengers) ytitle(ds12lair)  ///
  subtitle( over 24 periods)  color(green blue purple red red purple) ///
  lpattern(solid longdash dash dash_dot dash_dot dash) 
  

qui est replay model1

foreval ds12lair 120 0
ret list

cap translate airmodeval.smcl airmodeval.pdf, replace
log close _all   
end
