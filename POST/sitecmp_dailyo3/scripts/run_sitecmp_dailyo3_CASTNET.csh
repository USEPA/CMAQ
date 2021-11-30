#! /bin/csh -f

# ===================== SITECMP_DAILYO3_v5.3.X Run Script ===========
# Usage: run.sitecmp_dailyo3_CASTNET.csh >&! sitecmp_dailyo3.log &
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
#SBATCH -J sitecmp_dailyo3
#SBATCH -p ord
#SBATCH --gid=mod3dev
#SBATCH -A mod3dev
#SBATCH -o /home/sfarrell/CRACMM1new/POST/sitecmp_dailyo3/scripts/sitecmp_dailyo3_castnet_%j.txt

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
 set RUNID = ${VRSN}_${compilerString}_${APPL}

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/sitecmp_dailyo3/scripts/BLD_sitecmp_dailyo3_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = sitecmp_dailyo3_${VRSN}.exe

#> Set output directory
 set POSTDIR = ${CMAQ_DATA}/POST   #> Location where sitecmp_dailyo3 file will be written

  if ( ! -e $POSTDIR ) then
	  mkdir $POSTDIR
  endif

# =====================================================================
#> SITECMP_DAILYO3 Configuration Options
# =====================================================================

#> Projection sphere type used by I/OAPI (use type #20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> define obs species 
 setenv OBS_SPECIES OZONE

#> define model species
 setenv OZONE "1*O3,ppb"

#> Ozone convert factor (ppm to ppb)
 setenv OBS_FACTOR "1"        # Multiply by 1000 to convert ppm to ppb

#> define time window
 set SDATE = "2016-07-01"    #> beginning date (July 1, 2016)
 set EDATE = "2016-07-14"  #> ending date    (July 14, 2016)
 setenv START_TIME 0      
 setenv END_TIME   230000   

#> Convert SDATE and EDATE to Julian day.
#> (required format for sitecmp START_DATE and END_DATE environment variables)
 setenv START_DATE `date -ud "${SDATE}" +%Y%j`
 setenv END_DATE `date -ud "${EDATE}" +%Y%j`   

#> Number of 8hr values to use when computing daily maximum 8hr ozone.
#> Allowed values are 24 (use all 8-hr averages with starting hours 
#> from 0 - 23 hr local time) and 17 (use only the 17 8-hr averages
#> with starting hours from 7 - 23 hr local time)
 setenv HOURS_8HRMAX 24
# setenv HOURS_8HRMAX 17

#> Start and end hours for partial days. (do not use for full day calculations)
#  setenv PARTIAL_DAY "10,17" 

#> adjust for daylight savings
 setenv APPLY_DLS N

#> Number of hours to add when retrieving time steps from M3_FILE_n files during processing.
#> This should only be non-zero if the M3_FILE_n files were pre-processed with a utility like m3tshift (default 0).
 setenv TIME_SHIFT 0

#> indicate whether or not to check QA flag
 setenv QA_FLAG_CHECK Y 
 setenv QA_FLAG_HEADER "OZONE_F" 
 setenv QA_FLAG_VALUES "BCDFIMP" 

#> set missing value string
 setenv MISSING '-999'

#> Indicates whether the Lambert x/y information should be included in the output file
 setenv LAMBXY N


#############################################################
#  Input files
#############################################################

#> ioapi input files containing VNAMES (max of 10)
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201607.nc
        #[Add location of input file, e.g. COMBINE_ACONC file.]

#> SITE FILE containing site-id, longitude, latitude, and optionally 
#> GMT offset, state, county, and elevation (csv format)
#> The column headings for the required variables need to be 
#> stat_id, lon, and lat (case insensitive)
#> The column headings for the optional variables need to be
#> gmt_offset, state, county, and elevation (case insensitive)
#> This file can be downloaded from
#> https://github.com/USEPA/AMET/tree/master/obs/AQ/site_metadata_files
 setenv SITE_FILE  /work/MOD3EVAL/aq_obs/routine/site_metadata_files/CASTNET_full_site_list.csv  #> CASTNET sites meta data file
#> On EPA system:
 setenv SITE_FILE  /work/MOD3EVAL/aq_obs/routine/site_metadata_files/CASTNET_full_site_list.csv  #> CASTNET sites meta data file

#> input table containing site-id, time-period, and data fields
#> CASTNET obs data in the format needed for sitecmp_dailyo3 are available 
#> from the CMAS Center Data clearinghouse under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> Hourly CASTNET observations are located in AMET12_OBSDATA_YYYY.tar.gz for year YYYY.
 setenv IN_TABLE  /work/MOD3EVAL/aq_obs/routine/2016/CASTNET_hourly_data_2016.csv #> CASTNET data file
#> One EPA system:
 setenv IN_TABLE  /work/MOD3EVAL/aq_obs/routine/2016/CASTNET_hourly_data_2016.csv #> CASTNET data file


#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE ${POSTDIR}/CASTNET_Daily_CMAQ_${RUNID}_201607.csv

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif
 
 exit()
