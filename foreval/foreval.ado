*! foreval.ado   version 3.0.0  12 October 2011  Robert A. Yaffee yaffee@nyu.edu
* 
* command syntax : foreval {name of series to be forecast} {time of forecast origin} 
set more off
set trace on
// cap drop fov-afcorr
cap drop actual-TheilU
cap drop fov-afcorr
cap drop pulse 
capture program drop foreval 



program foreval, rclass
version 12

* command is foreval depvar fo ar_max




// setting the seed for the random walk

set seed 20202
return scalar seed = 20202
cap confirm variable forecast2
if _rc==111 {
predict forecast2, xb
}
* establishing the forecast origin value
qui summ `1'
scalar mysd = r(sd)
scalar fov2 = `1'[`2'] 
gen fov = forecast[`2']
qui summ forecast
replace fov = fov2 + mysd*rnormal() if segmnt==1


qui gen actual = fov
est store model1
cap predict fcvar, mse
cap gen ub95=forecast + 1.96*sqrt(fcvar) if segmnt==1
cap gen ub90=forecast + 1.645*sqrt(fcvar) if segmnt==1
cap gen lb90=forecast - 1.645*sqrt(fcvar) if segmnt==1
cap gen lb95=forecast - 1.96*sqrt(fcvar) if segmnt==1

cap gen ub95=forecast3 + 1.96*sqrt(fcvar)
cap gen lb95=forecast3 - 1.96*sqrt(fcvar)

local fco `2'
return scalar fc_origin= `2'
local fc = (`fco'- 1)
tsline `1' forecast ub95 ub90 lb90 lb95, tline(`fc') title(Ex ante forecast of `1') ///
  color(blue green purple red red purple)  ytitle(`1') lpattern(solid longdash dash dash_dot dash_dot dash) ///
  subtitle(over `fh' periods)  // length of forecast horizon

loc fh = (_N - `2')
quietly gen fh=(_N -`2')
return scalar fh=(_N-`2')


// limiting the bounds of the random walk

replace actual = fov + r(sd)*rnormal() if actual == .


capture confirm variable error
if _rc == 111 {
gen error = actual - forecast
}

* elements of symmetric mape
qui gen mapenum1 = abs(error) if segmnt==1
qui gen mapeden1 =(actual + forecast)/2 if segmnt==1
qui gen mapenum2 = error if segmnt==1
qui gen mapeden2 =(actual + forecast)/2 if segmnt==1
qui gen mapenum3 = abs(error) if segmnt==1
qui gen mapeden3 = (abs(actual) + abs(forecast))/2 if segmnt==1


* Theil-Mincer-Zarnowitz (TMZ) test of weak rationality

local armax =  e(ar_max)
if `armax' > 0 {
qui newey actual forecast, lag(`armax')
quietly test (_cons=0)(forecast=1)
quietly gen twrf=r(F)
return scalar twrf=r(F)
qui gen twrdf=r(df)
qui gen twrp=r(p)
return scalar twrp = r(p)
gen typeReg="Newey - West"
scalar typeReg="Newey-West"
}
else {
qui regress actual forecast, robust
quietly test (_cons=0)(forecast=1)
quietly gen twrf=r(F)
return scalar twrf=r(F)
qui gen twrdf=r(df)
qui gen twrp=r(p)
return scalar twrp = r(p)
gen typeReg="heteroskedastically robust"
scalar typeReg="heteroskedastically robust"
}

qui est restore model1

* Measures of forecast accuracy

* measures based on absolute error

quietly gen abserror=abs(error) if segmnt==1
quietly summ abserror if segmnt==1, detail
quietly gen safe = r(sum) 
quietly gen sae = r(sum)
return scalar safe = r(sum)
qui gen mae = r(mean) 
return scalar mae=r(mean) 
label var safe "Sum of absolute forecast errors"
label var mae "Mean absolute forecast error" 

*  Measures of basic absolute percentage error

quietly gen ape = (abs(error/actual)*100) if segmnt==1
quietly summarize ape if segmnt==1, detail
quietly gen mdape = r(p50)
return scalar mdape =  r(p50)
label var mdape "Median absolute percentage forecast error"
quietly gen mape=r(mean) 
return scalar mape=r(mean)
label var mape "Mean absolute percentage forecast error"




* Measures based on squared error

quietly gen absersq=abserror^2 
quietly sum absersq if absersq < ., detail
quietly gen sse=r(sum) 
return scalar sse=r(sum)
qui gen msfe=r(Var) 
return scalar msfe=r(Var)
label var msfe "Mean Absolute Forecast Error"
quietly gen rmsfe = sqrt(msfe) if msfe < . 
return scalar rmsfe=sqrt(msfe)



* measures of symmetric mape

quietly gen presmape1= 100*(mapenum1/mapeden1) if segmnt==1
qui summ presmape1, detail
quietly gen smape1 = r(mean)
return scalar smape1 = r(mean)
label var smape1 "Symmetric mean absolute percentage forecast error v.1"
quietly gen presmape2= 100*abs(mapenum2/mapeden2) if segmnt==1
qui summ presmape2, detail
quietly gen smape2 = r(mean)
return scalar smape2 = r(mean)
label var smape2 "Symmetric mean absolute percentage forecast error v.2"
quietly gen presmape3= 100*mapenum3/mapeden3 if segmnt==1
qui summ presmape3, detail
quietly gen smape3 = r(mean)
return scalar smape3 = r(mean)
label var smape1 "Symmetric mean absolute percentage forecast error v.3"


* tests of directional accuracy

qui corr forecast2 actual 
quietly gen afcorr =r(rho) 
return scalar afcorr = r(rho)
quietly spearman forecast2 actual if segmnt==1
quietly gen spcor=r(rho)
return scalar spcor=r(rho)
quietly gen sppv = r(p)
return scalar sppv=r(p)

* tests of forecast bias
qui ttest forecast = actual if segmnt==1   // paired t-test
qui gen pairedt=r(t)
return scalar pairedt=r(t)
qui gen tpv = r(p)
return scalar tpv = r(p)
quietly signrank forecast=actual if segmnt==1   //wilcoxon signrank test
quietly gen biasz=r(z)
return scalar biasz=r(z)
quietly gen normprobz = normprob(r(z)) 
return scalar normprobz=normprob(r(z))

* Henri Theil's U test 
quietly gen X1=f.actual 
quietly gen F1=f.forecast  
quietly gen numa = ((F1-X1)/(actual))^2 
quietly gen num1 = sum(numa)   
quietly gen dena = ((X1-actual)/(actual))^2  
quietly gen den1 = sum(dena)  
quietly gen TUnum= num1[_N] 
quietly gen TUden = den1[_N] 
quietly gen TheilU = sqrt(TUnum/TUden)
return scalar TheilU = sqrt(TUnum/TUden)
return local title  "  Ex ante forecast evaluation using random walk as basis"
set more off

*** display of compations
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di as result  _col(10) "{c |}" _col(20) "        Ex ante forecast evaluation"  _col(75) "{c | }"
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
di as txt  _col(10) "{c |}"   "        t  =    "_col(55) %10.5f pairedt      _col(75) "{c | }"
di _col(10) "{c |}"   "        p-value = "    _col(55)  %10.4f tpv           _col(75) "{c | }"
di _col(10) "{c |}"   "              "                                       _col(75) "{c | }"
di as result _col(10)"{c |}"_col(15) "Signrank test of difference between Forecast and Actual"  _col(75) "{c | }"  
di as txt _col(10) "{c |}"                                                     _col(75) "{c | }" 
di _col(10) "{c |}"  "        Signrank Z        =     " _col(55) %10.5f biasz   _col(75)"{c | }" 
di _col(10) "{c |}"  "        Probability > |Z| =     " _col(55) %10.5f normprobz _col(75) "{c | }"
di _col(10) "{c |}"                                                          _col(75) "{c | }" 
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di as result _col(10) "{c |}"_col(10) " Theil-Mincer-Zarnowitz Test of weak forecast rationality " _col(75) "{c | }"   
di as txt _col(10)  "{c |}" "         using a ",typeReg," regression of "     _col(75) "{c | }"
di _col(10)  "{c |}"   "         Actual = cons + B*Forecast   "              _col(75) "{c | }"
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }"     
di  _col(10)  "{c |}"   "        Joint test of cons=0 and B=1    "           _col(75) "{c | }"   
di _col(10)  "{c |}"   "            F value = " _col(55) %10.5f twrf          _col(75) "{c | }"
di _col(10)   "{c |}"  "            df   =    " _col(55) %10.0f twrdf         _col(75) "{c | }"
di _col(10)  "{c |}"   "            p-value =  " _col(55) %10.5f twrp         _col(75) "{c | }"  
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }"  
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }" 
di as result _col(10)  "{c |}" _col(25)" Measures of directional accuracy"      _col(75) "{c | }"           
di as txt _col(10)  "{c |}"                                                  _col(75) "{c | }"                     
di _col(10)  "{c |}"  " Pearson correlation between "                       _col(75) "{c | }" 
di _col(10)  "{c |}"    " forecast and actual  = "_col(55) %10.5f afcorr     _col(75)"{c | }"
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di _col(10)  "{c |}"  " Spearman rank correlation  ",                         _col(75) "{c | }"  
di _col(10)  "{c |}" _col(20) " between forecast & actual    =  ",_col(55) %10.5f spcor _col(75) "{c | }"  
di _col(10)  "{c |}" _col(20) " p-value of Spearman Rho      =  ",_col(55) %10.5f sppv  _col(75) "{c | }"                            
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di as result _col(10)  "{c |}"  _col(25) "Measures of forecast accuracy "     _col(75) "{c | }" 
di _col(10)  "{c |}"                                                         _col(75) "{c | }" 
di as txt _col(10)  "{c |}" " Sum of squared errors      =   " _col(55) %10.5f sse   _col(75) "{c | }"
di _col(10)  "{c |}"  " Mean square forecast error       =   " _col(55) %10.5f msfe  _col(75) "{c | }" 
di _col(10)  "{c |}"  " Root mean square forecast error  =   " _col(55) %10.5f rmsfe _col(75) "{c | }"
di _col(10)  "{c |}"  " Sum of absolute forecast errors  =   " _col(55) %10.5f sae  _col(75) "{c | }"
di _col(10)  "{c |}"  " Mean absolute error              =   " _col(55) %10.5f mae   _col(75) "{c | }" 
di _col(10)  "{c |}"  " Mean absolute percentage error   =   " _col(55) %10.5f mape  _col(75) "{c | }" 
di _col(10)  "{c |}"  " Median absolute Percentage Error =   " _col(55) %10.6f mdape _col(75) "{c | }" 
di _col(10)  "{c |}"  " Symmetric mean absolute percentage "                         _col(75) "{c | }" 
di _col(10)  "{c |}"  "            error v.1          = "       _col(55) %10.5f smape1 _col(75) "{c | }" 
di _col(10)  "{c |}"  " Symmetric mean absolute percentage "                        _col(75) "{c | }" 
di _col(10)  "{c |}"  "            error v.2          = "       _col(55) %10.5f smape3 _col(75) "{c | }" 
di _col(10)  "{c |}"  " Symmetric mean absolute percentage "                        _col(75) "{c | }" 
di _col(10)  "{c |}"  "            error v.3          = "       _col(55) %10.5f smape3 _col(75) "{c | }" 
di _col(10)  "{c |}"  " Length of forecast horizon  (h)  =   "  _col(55) %8.0f fh     _col(75) "{c | }"
di _col(10) "{c |}"                                                          _col(75) "{c | }" 
di _col(10) "{c BLC}""{hline 64}"                                            _col(75) "{c BRC}"
di _col(10) "{c TLC}""{hline 64}"                                            _col(75) "{c TRC}"
di _col(10)  "{c |}"                                                         _col(75) "{c | }"  
di as result _col(10)  "{c |}" _col(20) "Theil's U test of Forecast Accuracy "   _col(75) "{c | }"   
di _col(10)  "{c |}" "                                                 "     _col(75) "{c | }"  
di as txt _col(10)  "{c |}" "     Forecast Accuracy compared to Naive 1 forecast  " _col(75) "{c | }"  
di _col(10)  "{c |}"   "                                   "                 _col(75) "{c | }"             
di _col(10)  "{c |}"   "           Theil's U   =  " _col(55) %10.5f TheilU[_N]  _col(75) "{c | }"  
di _col(10)  "{c |}"   "                                   "                 _col(75)  "{c | }"   
di _col(10) "{c |}"    "  "                                                  _col(75) "{c | }" 
di _col(10) "{c BLC}""{hline 64}"                                          _col(75) "{c BRC}" 
di _col(10) "                                                              "  
*********************************************************************************************************
set more off
* replace forecast = . if segmnt < 1
replace ub95 = . if segmnt < 1
replace lb95 = . if segmnt < 1
replace ub90=  . if segmnt < 1
replace lb90 = . if segmnt < 1
label var ub95 "upper 95% forec. conf. limit"
label var ub90 "upper 90% forec. conf. limit"
label var lb90 "lower 90% forec. conf. limit"
label var lb95 "lower 95% forec. conf. limit"
cap label var forecast "One-step ahead forecast"
cap label var t "date"
loc fco `2'
loc fc =`fco'- 1
tsline `1' forecast ub95 ub90 lb90 lb95, tline(`fco') title(Ex ante forecast of `1') ///
  color(blue green purple red red purple)  ytitle(`1') lpattern(solid longdash dash dash_dot dash_dot dash) ///
  subtitle(over `fh' periods)                                                                                 
* cleanup

cap drop fvar

cap drop actual-afcorr
cap drop spcor-TheilU
cap drop fov
cap drop pulse* Author: Robert Alan Yaffee
* email:  yaffee@nyu.edu
* date:   March 24, 2006
*/
end
