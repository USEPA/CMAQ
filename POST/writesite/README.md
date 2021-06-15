writesite
========

This Fortran program generates a csv file from an IOAPI data file for a set of species at defined site locations. 

## Options:
<ol>
<li> Program can shift to local standard time for hourly data based on default time zone file </li>
<li> Data at all cells or at defined site locations can be specified </li>
<li> Date range can be specified </li>
<li> Grid layer can be specified </li>
</ol>

## Run Time Environment variables used:
```
 INFILE         name of IOAPI input file. Supported map projections are Lambert conformal, polar stereographic, 
                and lat/lon
 SITE_FILE      name of input file containing sites to process (default is all cells)
 DELIMITER      delimiter used in site file (default is <tab>)
 USECOLROW      site file contains column/row values (default is N, meaning lon/lat values will be used)
 TZFILE         location of time zone data file, tz.csv (this is a required input file)
 OUTFILE        name of output file
 LAYER          grid layer to output (default is 1)
 USELOCAL       adjust to local standard time (default is N)
 TIMESHIFT      shifts time of data (default is 0)
 PRTHEAD        switch to output header records (default is Y)
 PRT_XY         switch to output map projection coordinates (default is Y) 
 STARTDATE      first date to process (default is starting date of input file)
 ENDDATE        last date to process (default is ending date of input file)
 SPECIES_#      list of species to output (e.g. setenv SPECIES_1 O3).  
                To extract all species use: setenv SPECIES_1 ALL
```
## Format of SITE_FILE:

The SITE_FILE file has one line per location with the following format:

`LABEL` `DELIMITER` `X` `DELIMITER` `Y`

where
* `LABEL` is a user-defined text string used to label each location. The text string is then used in the 'sideid' column of OUTFILE. It can represent a station ID associated with a given pair of latitude and longitude values, a string consisting of column and row numbers if `USECOLROW` is T, or any other unique string with a length up to 10 characters.
* `DELIMITER` is defined by the associated environment variable.
* `X`/`Y` are:
  * By default, longitude/latitude in decimal degrees
  * If `USECOLROW` is T, then `X` should be between (1, `NCOLS`) and `Y` should be between (1, `NROWS`).

## Run Time Environment variables (not required):
```
 IOAPI_ISPH  projection sphere type (use type #20 to match WRF/CMAQ)
             (ioapi default is 8)
```

## Compile writesite source code:

Execute the build script to compile writesite:

```
cd $CMAQ_HOME/POST/writesite/scripts
./bldit_writesite.csh [compiler] [version] |& tee build_writesite.log
```

## Run writesite:
Edit the sample run script (run.writesite), then run:
```
 ./run.writesite|& tee writesite.log
```

Check the log file to ensure complete and correct execution without errors.

