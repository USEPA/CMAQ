#! /bin/csh -f

# ===================== APPENDWRFv5.2 Run Script ====================
# Usage: run.appendwrf >&! appendwrf_V52.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================

#> Source the config.cmaq file to set the run environment
 source config.cmaq

 set EXEC_ID = SE52BENCH

#> Set the working directory
 set BASE  = $cwd      
#set BASE  = $CMAQ_HOME/scripts/appendwrf
 set BLD   = ${BASE}/BLD_$APPL

 cd $BASE; date; set timestamp; cat $BASE/cfg.${APPL}; echo " "; set echo


# ~~~~~~~~~~~~~~~~~~~~~~~~ Start EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#> Portable Batch System - The following specifications are 
#> recommended for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#PBS -N run.appendwrf.csh
#PBS -l walltime=1:30:00
#PBS -l nodes=login
#PBS -q singlepe 
#PBS -V
#PBS -m n
#PBS -j oe
#PBS -o ./appendwrf.log

#> Configure the system environment
# source /etc/profile.d/modules.csh 
#> Set location of combine executable.
# setenv BINDIR /home/css/CMAQ-Tools/scripts/appendwrf
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~ End EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
 source config.cmaq

#> Set General Parameters for Labeling the Simulation
 set EXEC_ID = SE52BENCH

#> Set the build directory for appendwrf if this was not set above 
#> (this is where the appendwrf executable is located by default).
 if ( ! -e $BINDIR ) then
  setenv BINDIR $CMAQ_HOME/Tools/appendwrf/BLD_APPENDWRF_${compiler}
 endif

#> Set input and output directories
 set INDIR  = [Add location of input directory]
 set OUTDIR = [Add location of output directory]


# =====================================================================
#> APPENDWRF Configuration Options
# =====================================================================


### set input and output files

 setenv INFILE_1 ${INDIR}/06May30/wrfbdy_d01
 setenv INFILE_2 ${INDIR}/06Jun04/wrfbdy_d01
 setenv INFILE_3 ${INDIR}/06Jun09/wrfbdy_d01

 setenv OUTFILE ${OUTDIR}/APPENDWRF_wrfbdy_d01

#> Executable call:
 /usr/bin/time $BINDIR/appendwrf.exe

 
 date
 exit()


