#! /bin/csh -f

# ===================== APPENDWRFv5.2 Run Script ====================
# Usage: run.appendwrf >&! appendwrf_V52.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================

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
 source ../../config.cmaq

#> Set the model version
 set VRSN = v52

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! -e ${BINDIR} ) then
  setenv BINDIR ${CMAQ_WORK}/Tools/appendwrf/BLD_appendwrf_${VRSN}_${compiler}
 endif

#> Set the name of the executable.
 setenv EXEC appendwrf_${VRSN}.exe

#> Set input and output directories
 set INDIR  = [Add location of input directory]
 set OUTDIR = [Add location of output directory]


# =====================================================================
#> APPENDWRF Configuration Options
# =====================================================================


### set input and output files

 setenv INFILE_1 ${INDIR}/[add location of wrf input or output file]
 setenv INFILE_2 ${INDIR}/[add location of wrf input or output file]
 setenv INFILE_3 ${INDIR}/[add location of wrf input or output file]

 setenv OUTFILE ${OUTDIR}/APPENDWRF_sample_file.nc


#> Executable call:
 ${BINDIR}/${EXEC}


 exit()


