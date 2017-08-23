#! /bin/csh -f

# ===================== APPENDWRFv5.2 Run Script ====================
# Usage: run.appendwrf >&! appendwrf_V52.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
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
  setenv BINDIR ${CMAQ_HOME}/POST/appendwrf/scripts/BLD_appendwrf_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC appendwrf_${VRSN}.exe

#> Set input and output directories
 set INDIR  = ${CMAQ_DATA}/met/mcip
 set OUTDIR = ${CMAQ_DATA}/appendwrf


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


