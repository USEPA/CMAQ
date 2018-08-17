block_extract
========

This Fortran program extracts time series of 1 or more variables from 1 or more IOAPI files for a specified range of cells. The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3. Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html), the maximum number of IOAPI input files is 63. There can be multiple files covering an extended time period, but all files must have the same header (grid, layers, variable names, etc.).  Program produces two text files. One file (specfile) lists the variables that were extracted.  The second text file (OUTFILE) contains the time series of the extracted variables with columns: 
```
  DATE  TIME  COL  ROW  LV  VAR1 VAR2 .... 
```
where VAR1 VAR2 ... are the variable names specified by SPECLIST.

##Environment variables used:

```
  SPECLIST      list of species to extract, e.g. set SPECLIST = ( O3 NO2 ).  "ALL" is supported also.
  LOCOL, HICOL  specify column range to extract
  LOROW, HIROW  specify row range to extract
  LOLEV, HILEV  specify layer range to extract
  M3_FILE_1     first IOAPI input file
  M3_FILE_2     second IOAPI input file (optional)
  . . .
  M3_FILE_N     Nth IOAPI input file (optional). 
                Program checks the existence of environment variables, up to M3_FILE_64, 
                until it detects there are no further input files specified.
  OUTFILE       output text file
  TIME_ZONE     GMT or EST. Default is GMT.
  OUTFORMAT     Actually refers to the format of the INPUT files. 
                Choices are IOAPI (default) or SAS, which has not been tested by anyone still at EPA.
  SDATE         starting date (optional; otherwise program starts with first time step)
  STIME         start time (optional)
  NSTEPS        number of time steps (optional; otherwise program processes all steps in all input files)
```

##Input file types and format:

Bldoverlay accepts "OBS" and "SITES" formats (FILETYPE) for the input file. For hourly output data (OLAYTYPE HOURLY) the program assumes that observations are in local standard time (LST) and applies a simple timezone shift to GMT using timezones every 15 degrees longitude.  For daily output data (OLAYTYPE DAILY, 1HRMAX or 8HRMAX) no time shifting is done so the output data remains in LST.  In this case the user can use the HR2DAY utility to time shift and average hourly model data to create daily model fields in LST.

```
 OBS format:     The OBS format consists of comma separated values in the format 
                 YYYDDD, HH, Site_ID, Longitude, Latitude, Value1[, Value2, Value3,...]. 
                 Note that if the input data is daily that an hour column (HH) is still required 
                 in the input data file.  In this case HH is ignored so the user could set this 
                 value to 0 for all records.
 SITES format:   Set to create a static site file using the value set by VALUE (default is 1). 
                 The format is a tab delimited file with the structure Site_ID Longitude Latitude.
```

##To run:
Edit the sample run script (run.block_extract), then run:
```
 run.block_extract |& tee block_extract.log
```
Check the log file to ensure complete and correct execution without errors.
Note that in addition to the OUTFILE, the program generates a text file "specdef" which lists the variables that were extracted.

