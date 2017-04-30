*! prepforeval.sthlp
smcl
{hline}
{title:prepforeval.hlp}
{hline}

{cmd:prepforeval}


{title:Syntax}

{cmd:prepforeval}  {datasetname} {depvar} {h}

      where depvar = name of dependent variable to be used after all transformations
            h      = line number of forecast origin 


{title: Description}
	  This program helps facilitates preprocessing for an {it:ex ante} forecast to be evaluated with the
foreval.ado program.  It extends the dataset and the time variable over the forecast horizon
and constructs a segmentation variable which aids the forecast evaluation.   The
segmentation variable for ex ante forecast is called segmnt.


{title: Example}

prepforeval wpi2b dlwpi 16

prepforeval air2 ds12lair 24




{Author: Robert A. Yaffee}  New York University{break}
robert.yaffee@nyu.edu


{p_end}
