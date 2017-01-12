appendwrf
========

This R program creates a merged AQS data file from pre-generated files posted on the EPA's AQS website (link below). The user must specify the location where the merged files should be created, the base location of the downloaded AQS files (it is then assumed the files will be in sub-directories from the base directory of /YYYY/hourly and YYYY/daily). The user must also specify the year (YYYY) and whether merging daily or hourly files (the script must be run separately for each time average). 

The formatted observation files generated from running this script have been provided in this release for 2001 - 2014.  This utility is included to allow the user to generate formatted observation files for different years if needed.

This program requires the R script merge_aqs_species.R.  The R code will work with the base installation of R (https://cran.r-project.org/) and does not require installation of any additional libraries.

This utility also requires .csv files downloaded from the EPA's AQS website:
 http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html
 
##The required files from that site are:

```
 daily_88101_****.csv
 daily_88502_****.csv
 daily_81102_****.csv
 daily_HAPS_****.csv
 daily_SPEC_****.csv
 daily_VOCS_****.csv
 hourly_88101_****.csv
 hourly_88502_****.csv
 hourly_81102_****.csv
 hourly_SPEC_****.csv
 hourly_HAPS_****.csv
 hourly_NONOxNOy_****.csv
 hourly_44201_****.csv
 hourly_42401_****.csv
 hourly_42101_****.csv
 hourly_42602_****.csv
 hourly_WIND_****.csv
 hourly_TEMP_****.csv
 hourly_PRESS_****.csv
 hourly_RH_DP_****.csv
 hourly_VOCS_****.csv
```

##Merged species
By default, the species merged are 

daily: 
```
 "PM25","PM10","SO4","NO3","NH4","OC","EC","Na","Cl","Al","Ca","Fe","Si","Ti",
 "Mg","K","Mn","Benzene","Propylene","Toluene","Butadiene","Acrolein","Ethylene",
 "Acetaldehyde","Formaldehyde","Isoprene","Ethane" 
 ```

hourly:
 ```
 "PM25","PM10","O3","CO","SO2","NO","NO2","NOX","NOY","Pressure","RH",
 "Temperature","Dewpoint","Wind_Speed","Wind_Direction","Benzene","Propylene",
 "Toluene","Butadiene","Isoprene","Ethane","Ethylene","SO4","NO3","OC","EC"
```

##To run:
Edit the sample run script (run.merge.aqs.species), then run: 
```
run.merge.aqs.species |& tee run.merge.aqs.species.log

```

Check the log file to ensure complete and correct execution without errors.
