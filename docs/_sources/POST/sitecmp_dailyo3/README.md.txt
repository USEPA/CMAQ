sitecmp_dailyo3
========

This Fortran program generates a csv (comma separated values) file that compares various daily ozone metrics computed from hourly CMAQ generated and observed ozone concentrations. The metrics included in the output file are daily maximum 1-hr ozone concentrations, daily maximum 1-hr ozone concentrations in the nine cells surrounding a monitor, time of occurrence of daily maximum 1-hr ozone concentrations, daily maximum 8-hr ozone concentrations, daily maximum 8-hr ozone concentrations in the nine cells surrounding a monitor, time of occurrence of daily maximum 8-hr ozone concentrations, the daily W126 ozone value, and the daily SUM06 ozone value. 


## Run Time Environment variables used:
```
 M3_FILE_#      IOAPI input file(s) containing hourly modeled ozone values.
                The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3.
		Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html),
		the maximum number of IOAPI input files is 63.
                [Note: Supported map projections are Lambert conformal, polar stereographic, and lat/lon.
                If an ioapi file is supplied that has a projection not in this list the program will 
                stop with an error message.]
 SITE_FILE      csv-formatted input file containing the station ID, latitude, longitude, and optionally 
                GMT offset, state, county, and elevation for each monitor.  
                The column headings for the required variables need to be stat_id, lat, and lon.
                The column headings for the optional variables (if present) need to be gmt_offset, state, county, and elevation.
	               The column headings are case insensitve and the order of the columns does not matter.
                For legacy purposes, SITE_FILE can also be a tab delimited file with no header and three 
                or four columns that contains site information for each monitor in the following fixed order:
	               site-id, longitude, latitude, and optionally time zone offset between local time and GMT
 IN_TABLE       input file containing hourly observed ozone data (comma delimited with header). The file can 
                contain columns with species other than ozone, these will be ignored by sitecmp_dailyo3
 OBS_SPECIES    name of the ozone species in the header line of IN_TABLE 
                (default "O3" for AQS; use "OZONE" for CASTNET)
 OZONE          comma separated string with expression and units for model ozone in M3_FILE_n,
                i.e. "[Mod_expression], [Mod_unit]"
                [Mod_expression] format: [factor1]*Mod_name1 [+][-] [factor2]*Mod_name2 ...
                [Mod_unit] is used in OUT_TABLE for the daily maximum 1-hr and 8-hr ozone metrics
                Example: setenv OZONE "1000*O3,ppbV"
 OBS_FACTOR     conversion factor needed to convert OBS_SPECIES from IN_TABLE to [Mod_unit] specified in OZONE 
                (default 1)
 OUT_TABLE      file for output data with columns of paired observed and modeled daily ozone metrics
```

## Run Time Environment Variables (not required):
```
 START_DATE     starting date of time period to process (YYYYJJJ)
 START_TIME     starting time of time period to process (HHMMSS)
 END_DATE       ending date of time period to process (YYYYJJJ)
 END_TIME       ending time of time period to process (HHMMSS)
 PARTIAL_DAY    start and end hours for partial day calculations (HH,HH). 
                Leave unset/blank for full day calculations. (default '')
                Example: setenv PARTIAL_DAY "10,17" 
 APPLY_DLS      apply daylight savings time (default N)
 HOURS_8HRMAX   Number of 8hr values to use when computing daily maximum 8hr ozone.
                Allowed values are 24 (use all 8-hr averages with starting hours 
                from 0 - 23 hr local time) and 17 (use only the 17 8-hr averages
                with starting hours from 7 - 23 hr local time) (default is 24)
 TIME_SHIFT     number of hours to add when retrieving time steps from M3_FILE_n files 
                during processing. This should only be non-zero if the M3_FILE_n files
                were pre-processed with a utility like m3tshift (default 0)
 QA_FLAG_CHECK  does IN_TABLE include a QA flag for ozone values, and should it be used? 
                (Default N because not present in AQS data. Should set to Y for CASTNET) 
 QA_FLAG_HEADER if QA_FLAG_CHECK is Y, name of the ozone QA flag in the header line of IN_TABLE 
                (default "OZONE_F" to correspond to CASTNET data)
 QA_FLAG_VALUES if QA_FLAG_CHECK is Y, string composed of single-character QA flags that 
                should be treated as missing values (default "BCDFIMP" to correspond to CASTNET data)
 MISSING        string to indicate missing output data values (default "m")
 IOAPI_ISPH     projection sphere type (use type #20 to match WRF/CMAQ)(IOAPI default 8)
 LAMBXY         include x/y projection values for each site in OUT_TABLE (default N)

```

## Run Time File formats:
```
 SITE_FILE - csv-formatted input file containing the station ID, latitude, longitude, and optionally 
             GMT offset, state, county, and elevation for each monitor.  
             The column headings for the required variables need to be stat_id, lat, and lon.
             The column headings for the optional variables (if present) need to be gmt_offset, state, county, and elevation.
	            The column headings are case insensitve and the order of the columns does not matter.
             For legacy purposes, SITE_FILE can also be a tab delimited file with no header and three 
             or four columns that contains site information for each monitor in the following fixed order:
	            site-id, longitude, latitude, and optionally time zone offset between local time and GMT
 
 M3_FILE_n - IOAPI file containing hourly modeled ozone data (n=1->12)
 
 IN_TABLE  - text (csv) file containing observed hourly ozone values in CASTNET table type format
 
 CASTNET - site field: "Site_id"
           starting date: "DateOn" ("YYYY-MM-DD hh:mm:ss")
           ending date: "DateOff"  ("YYYY-MM-DD hh:mm:ss")
 
 OUT_TABLE - output (csv) text file containing columns of paired observed and
             modeled values
```

## Compile sitecmp_dailyo3 source code:

Execute the build script to compile sitecmp_dailyo3:

```
cd $CMAQ_HOME/POST/sitecmp_dailyo3/scripts
./bldit_sitecmp_dailyo3.csh [compiler] [version] |& tee build_sitecmp_dailyo3.log
```

## Run sitecmp_dailyo3:
Edit the sample run script (run.sitecmp_dailyo3), then run:
```
 ./run.sitecmp_dailyo3|& tee sitecmp_dailyo3.log
```
Check the log file to ensure complete and correct execution without errors.

Sample run scripts has been provided for matching model data to ozone observations from AQS and CASTNET. The formatted observation data files needed for running the sitecmp_dailyo3 utility are available starting in 2000 from the CMAS Data Warehouse Google Drive: [North America Air Quaility Observation Files](https://drive.google.com/drive/folders/1QUlUXnHXvXz9qwePi5APzzHkiH5GWACw?usp=drive_link)

