clear
drop _all
*! oforeval.ado   version 2.0.0  28 sep 2010  Robert A. Yaffee yaffee@nyu.edu
set trace on
set more off
* 
* command syntax : oforeval {name of series to be forecast} {time of forecast origin} 
* cleanup 
capture program drop oforeval


program oforeval, rclass
version 11.2
capture confirm variable fcvar
if _rc==111 {
predict fcvar, mse
}
capture confirm variable u95
if _rc==111 {
gen u95=oforecast + 1.96*sqrt(fcvar) if segment==1
}
capture confirm variable u90
if _rc==111 {
gen u90=oforecast + 1.645*sqrt(fcvar) if segment==1
}
capture confirm variable l90
if _rc==111 {
gen l90=oforecast - 1.645*sqrt(fcvar) if segment==1
}
capture confirm variable l95
if _rc==111 {
gen l95=oforecast - 1.96*sqrt(fcvar) if segment==1
quietly gen actual = `1' if segment==1
}
quietly gen error=actual - oforecast
quietly gen h=(_N-`2')
return scalar h=(_N-`2')
set more off
replace oforecast = . if segment < 1
replace u95 = . if segment < 1
replace l95 = . if segment < 1
replace u90= . if segment < 1
replace l90 = . if segment < 1
loc fco `2'
loc fc =`fco'- 1
tsline `1' oforecast u95 u90 l90 l95, tline(`fco')  title(Ex post forecast of `1') ///
  color(blue green red brown brown red)  ytitle(`1')     
qui gen mapenum1 = abs(error) if segment==1
qui gen mapeden1 =(actual + oforecast)/2 if segment==1
qui gen mapenum2 = error if segment==1
qui gen mapeden2 =(actual + oforecast)/2 if segment==1
qui gen mapenum3 = abs(error) if segment==1
qui gen mapeden3 = (abs(actual) + abs(oforecast))/2 if segment==1
* Theil's test of weak rationality
local armax =  e(ar_max)
if `armax' > 0 {
qui newey actual oforecast, lag(`armax')
quietly test (_cons=0)(oforecast=1)
quietly gen twrf=r(F)
return scalar twrf=r(F)
qui gen twrdf=r(df)
qui gen twrp=r(p)
return scalar twrp = r(p)
gen typeReg="Newey - West"
scalar typeReg="Newey-West"
}
else {
qui regress actual oforecast, robust
quietly test (_cons=0)(oforecast=1)
quietly gen twrf=r(F)
return scalar twrf=r(F)
qui gen twrdf=r(df)
qui gen twrp=r(p)
return scalar twrp = r(p)
gen typeReg="heteroskedastically robust"
scalar typeReg="heteroskedastically robust"
}
* Measures of forecast accuracy
* absolute error
quietly gen abserror=abs(error) if segment==1
quietly sum abserror if segment==1, detail
qui gen safe=r(sum) if segment==1
return scalar safe = r(sum)
label var safe "Sum of absolute forecast errors"
quietly gen mae=r(mean) 
return scalar mae=r(mean) 
label var mae "Mean absolute forecast error" 
quietly gen ape = (abs(error/`1')*100) if segment==1
quietly summarize ape if segment==1, detail
quietly gen mdape = r(p50)
return scalar mdape =  r(p50)
label var mdape "Median absolute percentage forecast error"
quietly gen mape=r(mean) 
label var mape "Mean absolute percentage forecast error"
return scalar mape=r(mean)
quietly gen absersq=abserror^2 
quietly sum absersq if absersq < ., detail
quietly gen sse=r(sum) 
return scalar sse=r(sum)
qui gen msfe=r(Var) 
return scalar msfe=r(Var)
label var msfe "Mean Absolute Forecast Error"
quietly gen rmsfe = sqrt(msfe) if msfe < . 
return scalar rmsfe=sqrt(msfe)
quietly gen presmape1= 100*(mapenum1/mapeden1) if segment==1
qui summ presmape1, detail
quietly gen smape1 = r(mean)
label var smape1 "Symmetric mean absolute percentage forecast error v.1"
quietly gen presmape2= 100*abs(mapenum2/mapeden2) if segment==1
qui summ presmape2, detail
quietly gen smape2 = r(mean)
label var smape2 "Symmetric mean absolute percentage forecast error v.2"
quietly gen presmape3= 100*mapenum3/mapeden3 if segment==1
qui summ presmape3, detail
quietly gen smape3 = r(mean)
label var smape1 "Symmetric mean absolute percentage forecast error v.3"
* tests of directional accuracy
quietly corr oforecast actual 
quietly gen afcorr =r(rho) 
return scalar afcorr = r(rho)
quietly spearman oforecast `1' if segment==1
quietly gen spcor=r(rho)
return scalar spcor=r(rho)
quietly gen sppv = r(p)
return scalar sppv=r(p)

* tests of forecast bias
qui ttest oforecast = actual if segment==1   // paired t-test
qui gen pairedt=r(t)
return scalar pairedt=r(t)
qui gen tpv = r(p)
return scalar tpv = r(p)
quietly signrank oforecast=actual if segment==1   //wilcoxon signrank test
quietly gen biasz=r(z)
return scalar biasz=r(z)
quietly gen normprobz = normprob(r(z)) 
return scalar normprobz=normprob(r(z))

* Theil's U test 
quietly gen X1=f.actual 
quietly gen F1=f.oforecast  
quietly gen numa = ((F1-X1)/(actual))^2 
quietly gen num1 = sum(numa)   
quietly gen dena = ((X1-actual)/(actual))^2  
quietly gen den1 = sum(dena)  
quietly gen TUnum= num1[_N] 
quietly gen TUden = den1[_N] 
quietly gen TheilU = sqrt(TUnum/TUden)
return scalar TheilU = sqrt(TUnum/TUden)
return local title "Out-of-estimation-sample forecast evaluation"
set more off
*** display of compations
                                                                                
cap drop error
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di as result  _col(10) "{c |}" _col(20) "Out of estimation sample forecast evaluation"  _col(75) "{c | }"
di _col(10) "{c |}"   _col(35) "of `1' "                                     _col(75) "{c | }"
di as txt _col(10) "{c |}" _col(30) "Date:  " c(current_date)                _col(75) "{c | }"
di as txt _col(10) "{c |}" _col(30) "Time:  " c(current_time)                _col(75) "{c | }"
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di as result _col(10) "{c |}" _col(30) "Forecast bias tests "                 _col(75) "{c | }"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di _col(10) "{c |}"                                                          _col(75) "{c | }"  
di as result _col(10)"{c |}"_col(15) "Paired t-test over forecast horizon"  _col(75) "{c | }"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"  
di as txt  _col(10) "{c |}"   "        t  =    "_col(55) %6.4f pairedt      _col(75) "{c | }"
di _col(10) "{c |}"   "        p-value = "%6.4f _col(55)  tpv               _col(75) "{c | }"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di as result _col(10)"{c |}"_col(15) "Signrank test of difference between Forecast and Actual"  _col(75) "{c | }"  
di as txt _col(10) "{c |}"                                                     _col(75) "{c | }" 
di _col(10) "{c |}"  "        Signrank Z        =     " _col(55) %6.4f biasz   _col(75)"{c | }" 
di _col(10) "{c |}"  "        Probability > |Z| =     " _col(55) %6.4f normprobz _col(75) "{c | }"
di _col(10) "{c |}"                                                          _col(75) "{c | }" 
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di as result _col(10) "{c |}"_col(10) " Theil-Mincer-Zarnowitz Test of weak forecast rationality " _col(75) "{c | }"   
di as txt _col(10)  "{c |}" "         using a ",typeReg," regression of "     _col(75) "{c | }"
di _col(10)  "{c |}"   "         Actual = cons + B*Forecast   "              _col(75) "{c | }"
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }"     
di  _col(10)  "{c |}"   "        Joint test of cons=0 and B=1    "           _col(75) "{c | }"   
di _col(10)  "{c |}"   "            F value = " _col(55) %6.4f twrf          _col(75) "{c | }"
di _col(10)   "{c |}"  "            df   =    " _col(55) %6.0f twrdf         _col(75) "{c | }"
di _col(10)  "{c |}"   "            p-value =  " _col(55) %6.4f twrp         _col(75) "{c | }"  
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }"  
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }" 
di as result _col(10)  "{c |}" _col(25)" Measures of directional accuracy"      _col(75) "{c | }"           
di as txt _col(10)  "{c |}"                                                  _col(75) "{c | }"                     
di _col(10)  "{c |}"  " Pearson correlation between "                       _col(75) "{c | }" 
di _col(10)  "{c |}"    " forecast and actual  = "_col(55) %6.4f afcorr     _col(75)"{c | }"
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di _col(10)  "{c |}"  " Spearman rank correlation  ",                         _col(75) "{c | }"  
di _col(10)  "{c |}" _col(20) " between forecast & actual    =  ",_col(55) %6.4f spcor _col(75) "{c | }"  
di _col(10)  "{c |}" _col(20) " p-value of Spearman Rho      =  ",_col(55) %6.4f sppv  _col(75) "{c | }"                            
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di as result _col(10)  "{c |}"  _col(25) "Measures of forecast accuracy "     _col(75) "{c | }" 
di _col(10)  "{c |}"                                                         _col(75) "{c | }" 
di as txt _col(10)  "{c |}"  " Sum of Squared Errors            =   " _col(55) %6.4f sse    _col(75) "{c | }"
di _col(10)  "{c |}"  " Mean Square Forecast Error       =   " _col(55) %6.4f msfe  _col(75) "{c | }" 
di _col(10)  "{c |}"  " Root Mean Square Forecast Error  =   " _col(55) %6.4f rmsfe  _col(75) "{c | }"
di _col(10)  "{c |}"  " Sum of Absolute Errors           = "   _col(55) %6.4f  safe[_N]  _col(75) "{c | }"
di _col(10)  "{c |}"  " Mean Absolute Error              =   " _col(55) %6.4f mae   _col(75) "{c | }" 
di _col(10)  "{c |}"  " Mean Absolute Percentage Error   =   " _col(55) %6.4f mape  _col(75) "{c | }" 
di _col(10)  "{c |}"  " Median Absolute Percentage Error =   " _col(55) %6.4f mdape _col(75) "{c | }" 
di _col(10)  "{c |}"  " Symmetric Mean absolute percentage "                        _col(75) "{c | }" 
di _col(10)  "{c |}"  "            error v.1          = "       _col(55) %6.4f smape1 _col(75) "{c | }" 
di _col(10)  "{c |}"  " Symmetric Mean absolute percentage "                        _col(75) "{c | }" 
di _col(10)  "{c |}"  "            error v.2          = "       _col(55) %6.4f smape3 _col(75) "{c | }" 
di _col(10)  "{c |}"  " Symmetric Mean absolute percentage "                        _col(75) "{c | }" 
di _col(10)  "{c |}"  "            error v.3          = "       _col(55) %6.4f smape3 _col(75) "{c | }" 
di _col(10)  "{c |}"  " Length of Forecast Horizon  (h)  =   "  _col(49) %8.0f h     _col(75) "{c | }"
di _col(10) "{c |}"                                                          _col(75) "{c | }" 
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di as result _col(10)  "{c |}" _col(20) "Theil's U test of Forecast Accuracy "   _col(75) "{c | }"   
di _col(10)  "{c |}" "                                                 "     _col(75) "{c | }"  
di as txt _col(10)  "{c |}" "     Forecast Accuracy compared to Naive 1 forecast  " _col(75) "{c | }"  
di _col(10)  "{c |}"   "                                   "                 _col(75) "{c | }"             
di _col(10)  "{c |}"   "           Theil's U   =  " _col(55) %6.4f TheilU[_N]  _col(75) "{c | }"  
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }"   
di _col(10) "{c |}"    "  "                                                  _col(75) "{c | }" 
di _col(10) "{c BLC}""{hline 64}"                                          _col(75) "{c BRC}" 
di _col(10) "                                                              "  
*********************************************************************************************************

cap drop TheilU
cap drop fcvar-lo95
cap drop fcvar
cap drop actual-h
cap drop mapenum1-X1
cap drop F1-TUden
* Author: Robert Alan Yaffee
* email:  yaffee@nyu.edu
* date:   March 24, 2006
end
