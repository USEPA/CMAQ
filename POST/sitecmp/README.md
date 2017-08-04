sitecmp
========

This Fortran program generates a csv (comma separated values) file that compares CMAQ generated concentrations with an observed dataset.


## Environment variables used:

```
 TABLE_TYPE  dataset type {IMPROVE, CASTNET, STN, NADP, MDN, SEARCH,
             DEARS, AIRMON, OUT_TABLE}
 M3_FILE_n   ioapi input files containing modeled species data (max of 12). 
             [Note: Supported map projections are Lambert conformal, polar stereographic, and lat/lon.
             If an ioapi file is supplied that has a projection not in this list the program will 
             stop with an error message.]
 SITE_FILE   input file containing site information for each monitor (site-id, longitude, latitude, 
             and optionally time zone offset between local time and GMT) (tab delimited)
 IN_TABLE    input file with observed data (comma delimited with header)
 OUT_TABLE   file for output data with columns of paired observed and modeled
             values
```

## Environment Variables (not required):
```
 PRECIP      defines the precipitation field used in WETDEP and
             WETCON calculations (default="Precip")
 IOAPI_ISPH  projection sphere type (use type #20 to match WRF/CMAQ)
             (ioapi default is 8)
 MISSING     string to indicate missing output data values
             (default="-999")
 START_DATE  starting date of time period to process (YYYYJJJ)
 START_TIME  starting time of time period to process (HHMMSS)
 END_DATE    ending date of time period to process (YYYYJJJ)
 END_TIME    ending time of time period to process (HHMMSS)
 APPLY_DLS   apply daylight savings time (default N)
 TIME_SHIFT  number of hours to add when retrieving time steps from M3_FILE_n files 
             during processing. This should only be non-zero if the M3_FILE_n files
             were pre-processed with a utility like m3tshift (default 0)
```

## Species definitions: 
Defines the data columns for your output file. Each can specify the observed and modeled variables of the species you are analyzing. These definitions are specified by environment variables [species-type]_[1-50], where species type is one of the following {AERO, GAS, WETCON, WETDEP, PREC}. See the sample run scripts for additional examples beyond those listed below.
```
 format: [Obs_expression], [Obs_units], [Mod_expression], [Mod_unit], [Variable_name]
 
 expression format: [factor1]*Obs_name1 [+][-] [factor2]*Obs_name2 ...
 
 types: AERO_n  (AEROSOL Variables (1-50) - compute average over time)
        GAS_n   (GAS Variables (1-50)  - compute average over time)
        WETCON_n (Wet Concentration Variables (1-50) - compute
                  volume-weighted average)
        WETDEP_n (Wet Deposition Variables (1-50) - compute
                  accumulated wet deposition)
        PREC_n  (Precipitation Variables (1-50) - compute
                 accumulated precipitation)
        CHAR_n  (Character fields (1-50), copies from Obs file)
 
 examples:
        AERO_1="SO4f_val,ug/m3, ASO4T,,sulfate"
              (this defines an aerosol species where the observed values
                are obtained from the "SO4f_val" column setting its units
                to ug/m3, the modeled values are obtained from the "ASO4T"
                variable using its predefined units, both columns will be
                named "sulfate")
 
        PREC_1="Sub Ppt,mm,10*RT,mm,Precip"
                (this defines a precipitation species where the observed
                values are obtained from the "Sub Ppt" column setting its
                units to mm, the modeled values are obtained by multiplying
                10 times the "RT" variable and setting its units to mm,
                both columns will be named "Precip")
 
        AERO_2="NH4f_val,ug/m3,,,ammonium"
                (this defines an aerosol species where the observed values
                are obtained from the "NH4f_val" column setting its units
                to ug/m3, there is no modeled values column, the column
                will be named ammonium)
 
        CHAR_1="NH4f_flag"
                (this defines a character field to copy only from the observed field,
                no units or modeled species are used)
```
## File formats:
```
 SITE_FILE - tab delimited text file containing site-id, longitude,
             latitude, and optionally time zone offset between local time and GMT
 
 M3_FILE_n - IOAPI file containing modeled species data (n=1->12)
 
 IN_TABLE  - text (csv) file containing observed data values
           Each type of dataset requires a site field and fields that define
           the data record's time period. These are the required fields for
           each type.
 
 IMPROVE - site field: "site_code"
           date field: "obs_date"  (YYYYMMDD)
           The time period is 24 hours (midnight to midnight)
 
 NADP    - site field: "Site"
           starting date: "DateOn" (MM/DD/YYYY)
           ending date:   "DateOff" (MM/DD/YYYY)
           The time period is 9:00am to 8:59am
 
 STN     - (Use with CSN data)
           site field: "airs_site_code"
           date field: "DATETIME"  (MM/DD/YYYY)
           The time period is 24 hours (9:00am to 8:59am)
 
 MDN     - site field: "SITE"
           starting date: "START" (MM/DD/YYYY)
           ending date: "STOP"    (MM/DD/YYYY)
           The time period is 9:00am to 8:59am
 
 CASTNET - site field: "Site_id"
           starting date: "DateOn" ("YYYY-MM-DD hh:mm:ss")
           ending date: "DateOff"  ("YYYY-MM-DD hh:mm:ss")
 
 MET     - site field" "site_id"
           starting date: "date_time" ("YYYY-MM-DD hh:mm:ss")
           ending date: 59 minutes added to starting time
 
 
 SEARCH  - site field: "Site_id"
           starting date: "DateOn" (MM/DD/YYYY hh:mm)
           ending date: "DateOff"  (MM/DD/YYYY hh:mm)
 
 DEARS   - site field: "PID"
           starting date: "StartDate" (MM/DD/YY)
           The time period is 24 hours (9:00am to 8:59am)
 
 AIRMON  - site field: "Site"
           starting date: "Date/Time On" (MM/DD/YYYY hh:mm)
           ending date: "Date/Time Off"  (MM/DD/YYYY hh:mm)
 
 
 OUT_TABLE - output (csv) text file containing columns of paired observed and
             modeled values
```

## To run:
Edit the sample run script (run.sitecmp*), then run:
```
 run.sitecmp |& tee sitecmp.log
```
Check the log file to ensure complete and correct execution without errors.


A sample run scripts has been provided for matching model data to hourly observations from AQS.  The README.txt file in the scripts folder shows the changes need to adapt this run script to one of the following networks: AERONET, AMON, CASTNET (hourly and weekly data), CSN, IMPROVE, NADP, and SEARCH (hourly and daily data).  The formatted observation data files needed for running the sitecmp utility are available for 2000 through 2014 from the CMAS Center Data Clearinghouse under the heading "2000-2014 North American Air Quality Observation Data": https://www.cmascenter.org/download/data.cfm.  

Note that the run scripts rely on model output that has already been processed using the combine utility. The user should first run combine on ACONC and DEP output files to create the necessary COMBINE_ACONC and COMBINE_DEP files that contain the model species that can be matched to available observations. See the sample run scripts for the combine utility for examples on creating COMBINE_ACONC and COMBINE_DEP.
