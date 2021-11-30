#! /bin/csh -f


# ====================== BLOCK_EXTRACTv5.3.X Run Script =====================
# Usage: run.block_extract.csh >&! block_extract.log &
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
#SBATCH -J block_extract
#SBATCH -p ord
#SBATCH --gid=mod3dev
#SBATCH -A mod3dev
#SBATCH -o /home/sfarrell/CRACMM1new/POST/block_extract/scripts/block_extract_%j.txt

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
 set RUNID =  ${VRSN}_${compilerString}_${APPL}
 
#> Set the build directory if this was not set above 
#> (this is where the bldoverlay executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/block_extract/scripts/BLD_block_extract_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = block_extract_${VRSN}.exe

# =====================================================================
#> BLOCK_EXTRACT Configuration Options
# =====================================================================

# List of species to extract.  "ALL" is supported also.
 set SPECLIST = ( O3 NO2 ) 
 
#> Time Zone (GMT or EST. Default is GMT.)
  setenv TIME_ZONE    GMT     

#> Format of input files (SAS or IOAPI. Default is IOAPI.)
  setenv OUTFORMAT    IOAPI    

#> Starting data in format YYYYDDD.  Default is first time step.
#setenv SDATE        2011182 

#> Start time in format HHMMSS. Default is first time step.
#setenv STIME        000000   

#> Number of time steps.  Default is all steps in all input files. 
# setenv NSTEPS       24    

#> Specify colum range to extract.
  setenv LOCOL        44
  setenv HICOL        46 
#> Specifiy row range to extract.
  setenv LOROW        55 
  setenv HIROW        57 

#> Specify layer range to extract.
  setenv LOLEV         1
  setenv HILEV         1
 
#> First IOAPI input file.
  setenv M3_FILE_1  ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201607.nc 
          #[Add location of input file, e.g. COMBINE_ACONC file.]
# setenv M3_FILE_2  additional files up to 99.
  
#> Output text file.
  setenv OUTFILE     sample_block_extract.txt

#> If the scratch text file listing the desired species exists
#> from a previous run, remove it
 unalias rm
 if ( -e specfile) rm specfile

#> If the output text file exists from a previous run, remove it
 if ( -e ${OUTFILE}) rm ${OUTFILE}
 
#> echo the desired species to the scratch text file read in by block_extract
 unset echo 
 foreach spec ( ${SPECLIST} )
    echo ${spec} >> specfile
 end


#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif

 exit() 
