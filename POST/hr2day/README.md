hr2day
========

This Fortran program creates gridded IOAPI files with daily values from gridded IOAPI files containing hourly values.

## Environment Run Time Variables:

```
 USELOCAL      use local time when computing daily values (default N)
 USEDST        use daylight savings time when computing daily values (default N)
 TZFILE        location of time zone data file, tz.csv (this is a required input file)
               The time zone file has a header record for each time zone followed by
               the points in longitude/latitude that define its boundary polygon.
                 * Header record (%d,%d,%s): n points in polygon,hour offset,description
                 * Point records (%.4f,%.4f): longitude,latitude
               Two time zone files are provided with hr2day:
                 * tz_legacy.csv: Old file with unknown source and some known issues.
                 * tz.csv was created 2023-06-09 by Barron H. Henderson from the
                   the Natural Earth time zone shapefile (v4.1.0) at 10m resolution.
                   Shapefil available from https://www.naturalearthdata.com/
 PARTIAL_DAY   allow use of partial days when computing daily values. If this is set to N, 
               the program will require at least 18 out of 24 values to be present in the 
               time zone of interest to compute a daily value (default N)
 START_HOUR    starting hour to use when computing daily values (default 0)
 END_HOUR      ending hour to use when computing daily values (default 23)
 HOURS_8HRMAX  Number of 8hr values to use when computing daily maximum 8hr ozone.
               Allowed values are 24 (use all 8-hr averages with starting hours 
               from 0 - 23 hr local time) and 17 (use only the 17 8-hr averages
               with starting hours from 7 - 23 hr local time) (default is 24)
 M3_FILE_#     List of input IOAPI file names with hourly values.
               If only a single input file is provided, INFILE can be used instead of
	       M3_FILE_1.
               The program will concatenate time steps from all input files to construct the
	       longest possible time record which can be processed. Duplicate time steps are
	       eliminated.
	       The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3.
	       Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html),
	       the maximum number of IOAPI input files is 63.
	       Supported map projections are Lambert conformal, polar
	       stereographic, and lat/lon
 OUTFILE       output IOAPI file name with computed daily values
 SPECIES_#     Defines the name, units, expression and daily operation for each variable in OUTFILE. For configuration options see below. 
```

## Environment Run Time Variables (not required):
```
 IOAPI_ISPH  projection sphere type (use type #20 to match WRF/CMAQ)
             (ioapi default is 8)
 START_DATE  Optional desired first and last processing date.
 END_DATE    The program will adjust the requested dates if the desired range is not covered
             by the input file(s). If these dates are not specified, the processing will be
	     performed for the longest possible time record that can be derived from the
	     model input file(s). 
```

## Species and operator definitions: 
Defines the name, units, expression and daily operation for each variable in OUTFILE. These definitions are specified by environment variables SPECIES_[n]

```
 format:  SPECIES_1 = "[variable1_name], [variable1_units], [model_expression1], [operation1]"
          SPECIES_2 = "[variable2_name], [variable2_units], [model_expression2], [operation2]"
 
 variable[n]_name: desired name of the daily output variable, maximum 16 characters
          
 variable[n]_units: units of the daily output variable, maximum 16 characters
          
 model_expression[n]: Formular expressions supports operators +-*/ and are evaluated from 
                      left to right using precedence order of */+-. Order of evaluation can 
                      be forced by use of parentheses. When part of an expression is enclosed 
                      in parentheses, that part is evaluated first.  Other supported functions 
                      include "LOG", "EXP", "SQRT", and "ABS". In addition, expresssions can be 
                      combined to create conditional statements of the form: 
                      "expression_for_condition ? expresssion_if_true :  expression_if_false". 
          
 operation[n]: daily operation to perform. Options are
            
 SUM - sums the 24 hour values
 AVG- sums the 24 values and divides by 24
 MIN- uses the minimum hourly value
 MAX- uses the maximum hourly value
 HR@MIN - hour of the minimum hourly value
 HR@MAX - hour of the maximum hourly value
 @MAXT - uses the hourly value at maximum temperature
 MAXDIF - uses the maximum hourly change
 8HRMAX - uses the maximum 8 hour period
 W126 - computes the W126 daily index value as a weighted average of ozone concentrations between 8am & 7pm. These daily index values can then be used to calculate annual W126 values for the secondary ozone standard by first computing 3-month sums of daily W126 index values and then determining the consecutive three month period with the largest 3-month sum of daily W126 values.
 @8HRMAXO3 - averages the value within the 8-hr-max ozone period
 HR@8HRMAX - Starting hour of the 8-hr-max period 
 SUM06 - computes the SUM06 ozone value
 TZ - outputs the time zone offset used by hr2day. For example, a cell in Eastern Standard Time (UTC-0500) would output -5.
 
 examples:
                
 setenv SPECIES_1 "O3,ppbV,1000*O3,8HRMAX"    (computes the 8-hr daily maximum value of 1000 * O3 from INFILE 
                                              (assumed to be in ppmV) and writes the  result to OUTFILE as O3 
                                              with units ppbV)
 setenv SPECIES_2 "ASO4J_AVG,ug/m3,ASO4J,AVG" (computes the 24-hr average value of ASO4J from INFILE 
                                              (assumed to be in ug/m3) and writes the result to OUTFILE as 
                                              ASO4J_AVG with units ug/m3)
 setenv SPECIES_3 "ASO4J_MAX,ug/m3,ASO4J,MAX" (computes the daily maximum value of ASO4J from INFILE 
                                              (assumed to be in ug/m3) and writes the result to OUTFILE as 
                                              ASO4J_MAX with units ug/m3)
 setenv SPECIES_4 "UTCOFFSET,hours since UTC,O3,TZ"  (hr2day assigns each grid cell a time zone offset in hours
                                              since UTC from TZFILE. This writes the result to OUTFILE as
                                              UTCOFFSET with units hours since UTC. Although TZ does not
                                              use the expression (here O3), it must be a valid field to pass
                                              input checking.)
```

## Compile hr2day source code:

Execute the build script to compile hr2day:

```
cd $CMAQ_HOME/POST/hr2day/scripts
./bldit_hr2day.csh [compiler] [version] |& tee build_hr2day.log
```

## Run hr2day:
Edit the sample run script (run.hr2day.make8hrmax), then run:

```
 ./run.hr2day |& tee hr2day.log
```

Check the log file to ensure complete and correct execution without errors.

