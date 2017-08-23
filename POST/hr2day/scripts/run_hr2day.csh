#! /bin/csh -f

# ====================== HR2DAYv5.2 Run Script ======================
# Usage: run.hr2day.csh >&! hr2day_V52.log &
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
 set VRSN = v52

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  setenv BINDIR ${CMAQ_HOME}/POST/hr2day/scripts/BLD_hr2day_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC hr2day_${VRSN}.exe

#> Set location of CMAQ repo.  This will be used to point to the time zone file
#> needed to run bldoverlay.  The v5.2 repo also contains a sample input file.
 setenv REPO_HOME ${CMAQ_REPO}


# =====================================================================
#> BLDOVERLAY Configuration Options
# =====================================================================

#> Projection sphere type used by I/OAPI (use type #20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> set to use local time (default is GMT)
 setenv USELOCAL Y

#> set to use daylight savings time (default is N)
 setenv USEDST N

#> location of time zone data file, tz.csv (this is a required input file
#> when using OLAYTYPE HOURLY since hourly observations need to be shifted
#> from local time to GMT)
 setenv TZFILE ${REPO_HOME}/POST/bldoverlay/inputs/tz.csv

#> partial day calculation (computes value for last day)
 setenv PARTIAL_DAY Y

#> constant hour offset between desired time zone and GMT (default is 0)
 setenv HROFFSET 0

#> starting hour for daily metrics (default is 0)
 setenv START_HOUR 0

#> ending hour for daily metrics (default is 23)
 setenv END_HOUR 23

#> Number of 8hr values to use when computing daily maximum 8hr ozone.
#> Allowed values are 24 (use all 8-hr averages with starting hours 
#> from 0 - 23 hr local time) and 17 (use only the 17 8-hr averages
#> with starting hours from 7 - 23 hr local time)
 setenv HOURS_8HRMAX 24
# setenv HOURS_8HRMAX 17

#> define species (format: "Name, units, From_species, Operation")
#>  operations : {SUM, AVG, MIN, MAX, @MAXT, MAXDIF, 8HRMAX, SUM06}
 setenv SPECIES_1 "O3,ppbV,O3,8HRMAX"

#> set input and output files
 setenv INFILE ${CMAQ_DATA}/POST/COMBINE_ACONC_201107.nc
          #[Add location of input file, e.g. COMBINE_ACONC file.]
 setenv OUTFILE ${CMAQ_DATA}/POST/dailymaxozone.nc

#> Executable call:
 ${BINDIR}/${EXEC}

 exit()


