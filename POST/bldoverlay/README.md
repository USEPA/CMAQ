bldoverlay
========

This Fortran program creates an observation overlay file that can be imported into either PAVE or VERDI. It requires, as input, a file containing observed data in a specific format, and then creates a PAVE/VERDI compatible overlay file.

## Environment Run Time Variables used:

```
 IOAPI_ISPH    projection sphere type (use type #20 to match WRF/CMAQ)
               (the default for this program is 20, overriding the ioapi default of 8) 
 SDATE         start date in the format: YYYYDDD
 EDATE         end date in the format: YYYYDDD
 FILETYPE      Type of input file to be used (see information below).  Choices are: OBS, SITES (default is OBS)
 OLAYTYPE      Type of data for the overlay output file.  If input data is daily this should be set to DAILY.
               If input data is hourly choices are: HOURLY, 1HRMAX, 8HRMAX
 SPECIES       list of names of the species in the input file (e.g. setenv SPECIES 'O3,NO,CO')
 UNITS         list of units of the species in the input file (e.g. setenv UNITS 'ppb,ppb,ppb')
 INFILE        file containing input observed data
 TZFILE        location of time zone data file, tz.csv (this is a required input file)
 HOURS_8HRMAX  Number of 8hr values to use when computing daily maximum 8hr ozone.
               Allowed values are 24 (use all 8-hr averages with starting hours 
               from 0 - 23 hr local time) and 17 (use only the 17 8-hr averages
               with starting hours from 7 - 23 hr local time) (default is 24)
 MISS_CHECK    set days with incomplete data coverage to missing when computing daily maximum 8-hr averages (TRUE/FALSE)
 OUTFILE       name of overlay file to create
```

## Input file types and format:

Bldoverlay accepts "OBS" and "SITES" formats (FILETYPE) for the input file. For hourly output data (OLAYTYPE HOURLY) the program assumes that observations are in local standard time (LST) and applies a simple timezone shift to GMT using timezones every 15 degrees longitude.  For daily output data (OLAYTYPE DAILY, 1HRMAX or 8HRMAX) no time shifting is done so the output data remains in LST.  In this case the user can use the [HR2DAY utility](../hr2day) to time shift and average hourly model data to create daily model fields in LST.

```
 OBS format:     The OBS format consists of comma separated values in the format 
                 YYYDDD, HH, Site_ID, Longitude, Latitude, Value1[, Value2, Value3,...]. 
                 Note that if the input data is daily that an hour column (HH) is still required 
                 in the input data file.  In this case HH is ignored so the user could set this 
                 value to 0 for all records.
 SITES format:   Set to create a static site file using the value set by VALUE (default is 1). 
                 The format is a tab delimited file with the structure Site_ID Longitude Latitude.
```
## Compile bldoverlay source code

Execute the build script to compile bldoverlay:

```
cd $CMAQ_HOME/POST/bldoverlay/scripts
./bldit_bldoverlay.csh [compiler] [version] |& tee build_bldoverlay.log
```


## Run bldoverlay:
```
 ./run.bldoverlay |& tee bldoverlay.log
```

Check the log file to ensure complete and correct execution without errors.

*Note about overlays in VERDI:
VERDI has the capability of directly reading in a .csv or tab-delimited observational dataset. Hourly observed data needs to be in UTC.  See the [documentation for VERDI](https://github.com/CEMPD/VERDI/releases) for further details.*

