
arima dlwpi curr73 oilembargo if segmnt==0, ar(1 4) ma(6)  nolog difficult
est store model1
cap predict fvar, mse
cap predict forecast 
cap  predict forecast2
cap gen u90 = forecast + 1.645 *sqrt(fvar) if segmnt==1
cap gen u95 = forecast + 1.96 *sqrt(fvar) if segmnt==1
cap gen l90 = forecast - 1.645 *sqrt(fvar) if segmnt==1
cap gen l95 = forecast - 1.96 *sqrt(fvar) if segmnt==1

tsline dlwpi forecast u95 u90 l90 l95, tline(124) ytitle(dlwpi) /// 
    title(Ex ante forecast of dlwpi) subtitle(over 16 periods) ///
    lpattern(solid longdash dash dash_dot dash_dot dash)
graph save fc3.gph, replace

est replay model1
foreval dlwpi 124 4



