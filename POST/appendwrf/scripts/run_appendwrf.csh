#! /bin/csh -f

# ===================== APPENDWRFv5.3.X Run Script ==================
# Usage: run.appendwrf >&! appendwrf.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================

#> Simple Linux Utility for Resource Management System 
#> (SLURM) - The following specifications are recommended 
#> for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#SBATCH -t 10:00:00
#SBATCH -n 1
#SBATCH -J appendwrf
#SBATCH -p ord
#SBATCH --gid=mod3dev
#SBATCH -A mod3dev
#SBATCH -o /home/sfarrell/CRACMM1new/POST/appendwrf/scripts/appendwrf_%j.txt

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

#> Set the model version
 set VRSN = v532

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/appendwrf/scripts/BLD_appendwrf_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = appendwrf_${VRSN}.exe

#> Set input and output directories
 set INDIR  = ${CMAQ_DATA}/met/mcip
 set OUTDIR = ${CMAQ_DATA}/appendwrf


# =====================================================================
#> APPENDWRF Configuration Options
# =====================================================================


### set input and output files

 setenv INFILE_1 subset_wrfout_d01_2011-07-01_00:00:00  #> WRF Output File
 setenv INFILE_2 subset_wrfout_d01_2011-07-02_00:00:00  #> WRF Output File
 setenv INFILE_3 subset_wrfout_d01_2011-07-03_00:00:00  #> WRF Output File

 setenv OUTFILE ${OUTDIR}/APPENDWRF_sample_file.nc


#> Executable call:
 ${BINDIR}/${EXEC}

  set progstat = ${status}
  if ( ${progstat} ) then
    echo "ERROR ${progstat} in $BINDIR/$EXEC"
    exit( ${progstat} )
  endif

 exit()


