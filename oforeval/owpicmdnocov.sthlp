*! wpicmdnocov version 1.0.0  R. A. Yaffee wpi ex post forecast with no covariates
*------------------------------------------------------------------------------------------------
* caveat:   This version program does not automatically perform conditional forecasting,
*           where the user is seeking to use a regression model for his forecasting.
*           If the user wishes to perform conditional forecasting, with such a model,
*           he must first forecast all of the explanatory variables or event indicators
*           over the validation by himself and include those in the dataset to be used 
*           for the model to generate the one-step-ahead forecasts.
*************************************************************************************************

{smcl}
{hline}
help owpicmdnocov 
{hline}


{title:owpicmdnocov}


{title:Description}
	  This program exemplifies automation of the out-of-estimation {it:ex post} forecast evaluation.
	  

{title:Syntax}


	{cmd:owpicmdnocov} {datasetname} {depvarname}  {line number of forecast origin}
	
					    where
					    datasetname = name of dataset to use
					    depvarname =  name of time series after transformation
					    line number in dataset at which point forecasting begins  
	

{title:example}

    {cmd: owpicmdnocov wpi2b dlwpi 116}
    
    

{author:Robert A. Yaffee}  New York University{break}
robert.yaffee@nyu.edu


{p_end}
