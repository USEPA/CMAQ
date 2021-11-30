#! /bin/csh -f

# ====================== HR2DAYv5.3.X Run Script ======================
# Usage: run.hr2day.csh >&! hr2day.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================

#> Simple Linux Utility for Resource Management System 
#> (SLURM) - The following specifications are recommended 
#> for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#SBATCH -t 10:00:00
#SBATCH -n 1
#SBATCH -J hr2day
#SBATCH -p ord
#SBATCH --gid=mod3dev
#SBATCH -A mod3dev
#SBATCH -o /home/sfarrell/CRACMM1new/POST/hr2day/scripts/hr2day_%j.txt

#> The following commands output information from the SLURM
#> scheduler to the log files for traceability.
   if ( $?SLURM_JOB_ID ) then
      echo Job ID is $SLURM_JOB_ID
      echo Host is $SLURM_SUBMIT_HOST
      #> Switch to the working directory. By default,
      #>   SLURM launches processes from your home directory.
      echo Working directory is $SLURM_SUBMIT_DIR
      cd $SLURM_SUBMIT_DIR
   endif
   echo '>>>>>> start model run at ' `date`

#> Configure the system environment and set up the module 
#> capability
   limit stacksize unlimited
#

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 

 cd ../../..
 source ./config_cmaq.csh

#> Set General Parameters for Configuring the Simulation
 set VRSN      = v532              #> Code Version
 set PROC      = mpi               #> serial or mpi
 set MECH      = cb6r3_ae7_aq      #> Mechanism ID
 set APPL      = Bench_2016_12SE1        #> Application Name (e.g. Gridname)
                                                      
#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 set RUNID  = ${VRSN}_${compilerString}_${APPL}
 
#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/hr2day/scripts/BLD_hr2day_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = hr2day_${VRSN}.exe

#> Set location of CMAQ repo.  This will be used to point to the time zone file
#> needed to run bldoverlay.  
 set REPO_HOME = ${CMAQ_REPO}

#> Set output directory
 set POSTDIR = ${CMAQ_DATA}/POST    #> Location where hr2day file will be written

  if ( ! -e $POSTDIR ) then
	  mkdir $POSTDIR
  endif


# =====================================================================
#> HR2DAY Configuration Options
# =====================================================================

#> set to use local time (default is GMT)
 setenv USELOCAL Y

#> set to use daylight savings time (default is N)
 setenv USEDST N

#> location of time zone data file, tz.csv (this is a required input file
#> when using USELOCAL Y to shift from GMT to local time)
 setenv TZFILE ${REPO_HOME}/POST/bldoverlay/inputs/tz.csv

#> partial day calculation (computes value for last day)
 setenv PARTIAL_DAY Y

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
 
#> Optional desired first and last processing date. The program will
#> adjust the requested dates if the desired range is not covered by
#> the input file(s). If these dates are not specified, the processing
#> will be performed for the longest possible time record that can be
#> derived from the model input file(s)
 setenv START_DATE 2016182
 setenv END_DATE 2016195

#> set input and output files
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201607.nc
# setenv M3_FILE_2 ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201608.nc
          #[Add location of one or more (up to 366) input files, e.g. COMBINE_ACONC file.]
 setenv OUTFILE ${POSTDIR}/dailymaxozone_${RUNID}.nc

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif

 exit()


