#! /bin/csh -f

# ==================== WRITESITE_v5.2 Run Script ====================
# Usage: run.writesite.csh >&! writesite_V52.log &
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
#PBS -N run.writesite.csh
#PBS -l walltime=1:30:00
#PBS -l nodes=login
#PBS -q singlepe 
#PBS -V
#PBS -m n
#PBS -j oe
#PBS -o ./writesite.log

#> Configure the system environment
# source /etc/profile.d/modules.csh 
#> Set location of combine executable.
# setenv BINDIR /home/css/CMAQ-Tools/scripts/writesite
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~ End EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
 source config.cmaq

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! -e ${BINDIR} ) then
  setenv BINDIR ${CMAQ_HOME}/Tools/writesite/WRITESITE_${compiler}
 endif

#> Set the name of the executable.
 setenv EXEC writesite.exe

#> Set location of CMAQ repo.  This will be used to point to the time zone file
#> needed to run bldoverlay.  The v5.2 repo also contains a sample SITE_FILE text file.
 setenv REPO_HOME  [Add location of CMAQv5.2 repository here]


# =====================================================================
#> WRITESITE Configuration Options
# =====================================================================

#> Projection sphere type used by I/OAPI (use type #20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> name of input file containing sites to process (default is all cells)
 setenv SITE_FILE ALL
#> Sample SITE_FILE text file is available in the v5.2 repo.
#setenv SITE_FILE ${REPO_HOME}/POST/writesite/inputs/sites.txt

#> delimiter used in site file (default is <tab>)
 setenv DELIMITER ','

#> site file contains column/row values (default is N, meaning lon/lat values will be used)
 setenv USECOLROW N

#> location of time zone data file, tz.csv (this is a required input file)
#> The tz.csv file is saved within the bldoverlay folder of the v5.2 repo which also uses this input.
 setenv TZFILE ${REPO_HOME}/POST/bldoverlay/inputs/tz.csv

#> grid layer to output (default is 1)
 setenv LAYER 1

#> adjust to local standard time (default is N)
 setenv USELOCAL N

#> shifts time of data (default is 0)
#setenv TIME_SHIFT 1

#> output header records (default is Yes)
 setenv PRTHEAD  Y

#> output map projection coordinates x and y (default is Yes)
 setenv PRT_XY   N         

#> define time window
 set START_DATE = "2011-07-1"     #> first date to process (default is starting date of input file)
 set END_DATE   = "2011-07-1"     #> last date to process (default is ending date of input file)

#> Convert START_DATE and END_DATE to Julian day.
#> (required format for writesite STARTDATE and ENDDATE environment variables)
 setenv STARTDATE `date -ud "${START_DATE}" +%Y%j`
 setenv ENDDATE `date -ud "${END_DATE}" +%Y%j`

#> list of species to output
 setenv SPECIES_1 O3

#> set input and output files
 setenv INFILE  [Add location of input file, e.g. COMBINE_ACONC file.]
 setenv OUTFILE O3.csv

#> Executable call:
 ${BINDIR}/${EXEC}

 date
 exit()



