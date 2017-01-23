clear

import excel "/Users/MacBookAir/Desktop/GoogleDrive/DATA/R/BusinessForecasting/timeseries-rady/Keeling_CO2data_2017.xlsx", sheet("CO2data_numbers") cellrange(A6:E713) firstrow clear

gen edate = ym(Yr, month)

tsset edate, monthly

gen trend=_n
gen trendsq=trend^2
gen q=.
forvalues x=1/4{
	replace q=_n if _n==`x'
	}
replace q=q[_n-4] if q==.
xi i.q

twoway (tsline CO2)

reg CO2 Date
reg CO2 Date if Yr<2006

//regr CO2 Date trend trendsq if Yr<2006
regr CO2 Date trend trendsq _Iq* if Yr<2006

predict CO2detrend if e(sample) | Yr<2006
twoway (tsline CO2detrend)

predict ehat if e(sample), resid
regr ehat L.ehat L2.ehat L3.ehat

ac CO2
pac CO2
corrgram CO2

dfuller CO2
reg CO2 L.CO2

arima CO2 Date trendsq _Iq*, arima(1,0,0)
predict u_resid, resid

ac u_resid
pac u_resid

arima CO2 Date trendsq _Iq* if Yr<2006, arima(1,0,0)

predict co2hat, y

predict co2dynam, y dynamic(2005)
tsline CO2 co2hat co2dyn, ytitle("Predicted CO2 rate") title("Dynamic versus step-ahead forecast")
