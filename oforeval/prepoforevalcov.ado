cap drop segment
cap drop ub
cap drop ul
cap drop oforecast
cap drop fvar

cap program drop prepoforevalcov

program prepoforevalcov, rclass
syntax [anything]
use `1', clear
capture confirm variable t
if _rc==111{
gen t=_n
tsset t
}
include covars.do
gen segment=0
replace segment = 1 if _n >=`3'
arima `2' if segment < `3', ar(1 2/3) 
predict oforecast 
predict fvar, mse
replace oforecast = . if _n < `3'
gen ub95 = oforecast + 1.96*sqrt(fvar) 
gen ub90 = oforecast + 1.645*sqrt(fvar)
gen lb95 = oforecast - 1.96*sqrt(fvar) 
gen lb90= oforecast - 1.645*sqrt(fvar)
tsline `2' oforecast ub95 ub90 lb95 lb90, tline(`3') title(Ex post forecast profile) ytitle(`2') 
end
prepoforevalcov wpi2b dlwpi 116
