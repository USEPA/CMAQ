#!/bin/csh -f


# ====================== BLDOVERLAYv5.2 Run Script ======================
# Usage: run.bldoverlay.csh >&! bldoverlay_V52.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~ Start EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#> Portable Batch System - The following specifications are 
#> recommended for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#PBS -N run.bldoverlay.csh
#PBS -l walltime=1:30:00
#PBS -l nodes=login
#PBS -q singlepe 
#PBS -V
#PBS -m n
#PBS -j oe
#PBS -o ./bldoverlay.log

#> Configure the system environment
# source /etc/profile.d/modules.csh 
#> Set location of combine executable.
# setenv BINDIR /home/css/CMAQ-Tools/scripts/bldoverlay
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~ End EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
 source ../../config_cmaq.csh

#> Set the model version
 set VRSN = v52

#> Set the build directory if this was not set above 
#> (this is where the
 executable is located by default).
 if ( ! -e ${BINDIR} ) then
  setenv BINDIR ${CMAQ_HOME}/Tools/bldoverlay/BLD_bldoverlay_${VRSN}_${compiler}
 endif

#> Set the name of the executable.
 setenv EXEC bldoverlay_${VRSN}.exe

#> Set location of CMAQ repo.  This will be used to point to the time zone file
#> needed to run bldoverlay.  The v5.2 repo also contains a sample input file.
 setenv REPO_HOME  [Add location of CMAQv5.2 repository here]

# =====================================================================
#> BLDOVERLAY Configuration Options
# =====================================================================

#> Projection sphere type used by I/OAPI (use type #20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> define time window
 set START_DATE = "2011-07-1"     #> beginning date (July 1, 2011)
 set END_DATE   = "2011-07-2"     #> ending date    (July 2, 2011)

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
 setenv SPECIES 'O3,PM25,NO2'

#> species units
 setenv UNITS 'ppb,ug/m^3,ppb'

#> set input and output files.  A sample input file is provided with the CMAQv5.2 release.
 setenv INFILE ${REPO_HOME}/POST/bldoverlay/inputs/ozone_overlay_input_sample.csv
 setenv OUTFILE overlay_${START_DATE}_${END_DATE}.nc

#> Executable call: 
${BINDIR}/${EXEC}


 exit()

