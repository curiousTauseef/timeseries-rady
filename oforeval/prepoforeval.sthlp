* prepoforeval    version 1.0.0     Nov 11 2011          *

{smcl}
{hline}
{cmd:prepoforeval}
{hline}

{title:prepoforeval}


{title:Description}   
	A program to facilitate preprocessing for an {it:ex post} forecast evaluation of
a univariate time series forecast. An {it:ex post} forecast evaluation is an evaluation
of a forecast over the validation segment of the dataset, after the dataset has been
segmented into two parts.  The first part is called the estimation (a.k.a. historical) 
segment, whereas the second part is called the validation segment.  A segment variable
is constructed.  This segment variable is coded 0 for the estimation segment and one
for the validation segment. The purpose of the segment variable is to facilitate
graphical presentation and analytical evaluation.  
	Although this is the conventional approach, Professor Fang of Princeton 
University, suggests switching the segments and averaging to obtain a model that 
optimally fits the data. However, for predictive validitation, the former approach is
the conventional one.



{title:Syntax}


{cmd:prepoforeval} [{datasetname}] [{depvar}] [{line number of forecast origin}]
   
      where
      
      brackets are used to separate the elements of the command for clarification
                of explanation only and not to be used in the command itself.
      
      datasetname =  the name of the dataset in which all transformations needed
                are stored and which is stored in the working directory.
      
      depvar =  name of the variable after any transformation required for 
                stationarity.
                
      line number of forecast origin= the line number at which forecasting 
                begins.  This is the beginning of the validation segment of
                the dataset. 


{title:Examples:}


prepoforevalnocov wp2b dlwpi 116
prepoforevalcov wp2b dlwpi 116
prepoforevalnocov air2 ds12lair 110


{title:Caveat}
	If the user is performing conditional forecasting, he must first forecast or
extrapolate the values of the explanatory variables of the model along with any
event indicators used to obtain forecasts of the endogeous variable(s) before he
can obtain forecasts for the endogenoous variables.


{title:Also see}
{cmd:help prepforeval}...for facilitation of preprocessing before ex ante forecast
                         evaluation.
{cmd:wpicmdcov}..........as an example of a program demonstrating how event indicators
                         may be included in the forecasting process.
{cmd:wpicmdnocov}........as an example of a program forecasting without event indicators.


{Author: Robert A. Yaffee}  New York University
robert.yaffee@nyu.edu

{p_end}

