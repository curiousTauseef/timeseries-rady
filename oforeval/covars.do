// modeling step or pulse interventions  after the forecast horizon is generated but before the forecast is generated




cap program drop covars1

program covars1, rclass
//  specify name of covar and whether it is a step or pulse and date (or range of dates) of incidence
// repeate for each covar
// eg:    curr75 pulse 1973q3 oilembargo pulse tin(1973q3,1974q1) 
capture confirm variable `1'
if _rc == 111 {
gen `1'=0    // first indicator is a pulse
}
replace `1' =1 if t==q(`3')

capture confirm variable `4' 
if _rc==111 {
gen `4' =0  // second indicator is a pulse
}
replace `4' =1 if `6'



// filling in the event indicators prior to forecasting  // needs development

gen pulse=0
if `2'  ==  pulse {
qui replace `1'=0 if segment==1
}
else if `4' == step {
qui replace `1'=1 if segment==1
}
else if `4'== sv {
di "you will need to forecast each covariates into the forecast horizon in order"
di " to forecast your endogenous variable"
}

if `4' == pulse {
qui replace `4' = 0 if segment==1
}
else if `4' == step {
qui replace `4' = 1 if segment==1
}
else if `5' == sv {
di "You must forecast your stochastic variable over the forecast horizon to "
di "forecast your endenous variable"
}
tsline `1'  `4', title(Interventions)
end

covars1 curr73 pulse 1973q3 oilembargo pulse tin(1973q3,1974q1)
drop pulse
