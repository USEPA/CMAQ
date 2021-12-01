#!/bin/csh -f


# ====================== BLDOVERLAYv5.3.X Run Script ======================
# Usage: run.bldoverlay.csh >&! bldoverlay.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel

 cd ../../..
 source ./config_cmaq.csh

#> Set the model version
 set VRSN = v532

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/bldoverlay/scripts/BLD_bldoverlay_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = bldoverlay_${VRSN}.exe

#> Set location of CMAQ repo.  This will be used to point to the time zone file
#> needed to run bldoverlay.  The v5.2.1 repo also contains a sample input file.
 set REPO_HOME = ${CMAQ_REPO}

#> Set output directory
 set POSTDIR = ${CMAQ_DATA}/POST                      #> Location where overlay file will be written

  if ( ! -e $POSTDIR ) then
	  mkdir $POSTDIR
  endif

# =====================================================================
#> BLDOVERLAY Configuration Options
# =====================================================================

#> Projection sphere type used by I/OAPI (use type #20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> define time window
 set START_DATE = "2016-07-01"     #> beginning date (July 1, 2016)
 set END_DATE   = "2016-07-02"     #> ending date    (July 2, 2016)

#> Convert START_DATE and END_DATE to Julian day.
#> (required format for bldoverlay SDATE and EDATE environment variables)
 setenv SDATE `date -ud "${START_DATE}" +%Y%j`
 setenv EDATE `date -ud "${END_DATE}" +%Y%j` 

#> set file type
 setenv FILETYPE OBS

#> set overlay type to one of the following: (HOURLY, DAILY, 1HRMAX, 8HRMAX) 
 setenv OLAYTYPE HOURLY

#> Number of 8hr values to use when computing daily maximum 8hr ozone.
#> Allowed values are 24 (use all 8-hr averages with starting hours 
#> from 0 - 23 hr local time) and 17 (use only the 17 8-hr averages
#> with starting hours from 7 - 23 hr local time)
 setenv HOURS_8HRMAX 24
# setenv HOURS_8HRMAX 17

#> set days with incomplete data coverage to missing when computing
#> daily maximum 8-hr averages
 setenv MISS_CHECK TRUE

#> location of time zone data file, tz.csv (this is a required input file
#> when using OLAYTYPE HOURLY since hourly observations need to be shifted
#> from local time to GMT)
 setenv TZFILE ${REPO_HOME}/POST/bldoverlay/inputs/tz.csv

#> species label to be used in overlay file
 setenv SPECIES 'O3,PM25,PM10,SO2,NO,NO2,NOX,NOY,CO'

#> species units
 setenv UNITS  'ppb,ug/m^3,ug/m^3,ppb,ppb,ppb,ppb,ppb,ppb'

#> set input and output files.  A sample input file is provided with the CMAQv5.3.X release.
 setenv INFILE ${REPO_HOME}/POST/bldoverlay/inputs/AQS_overlay_input_sample.csv
 setenv OUTFILE ${POSTDIR}/AQS_overlay_${START_DATE}_${END_DATE}.nc

#> Executable call: 
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif

 exit()

