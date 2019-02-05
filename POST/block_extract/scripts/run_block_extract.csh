#! /bin/csh -f


# ====================== BLOCK_EXTRACTv5.3 Run Script =====================
# Usage: run.block_extract.csh >&! block_extract_v53.log &
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
 set VRSN = v53

#> Set the build directory if this was not set above 
#> (this is where the bldoverlay executable is located by default).
 if ( ! $?BINDIR ) then
  setenv BINDIR ${CMAQ_HOME}/POST/block_extract/scripts/BLD_block_extract_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC block_extract_${VRSN}.exe

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
  setenv M3_FILE_1  ${CMAQ_DATA}/POST/COMBINE_ACONC_201107.nc 
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

 exit() 
