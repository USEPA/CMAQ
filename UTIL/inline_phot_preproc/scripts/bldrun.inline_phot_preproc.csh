#!/bin/csh -fx

# ============ CSQY_TABLE_PROCESSOR_v5.3.x Build Script ================= #
# Usage: bldrun_create_ebi.csh [compiler] >&! bldrun_create_ebi.log   #
# Options for [compiler]: intel | gcc | pgi                           #
#                                                                     #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org                               #
# =================================================================== #

# =======================================================================
#> Preliminary error checking and environment configuration
# =======================================================================

#> Check that the host system is Linux-based
 set BLD_OS = `uname -s`        
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> Set Compiler Identity by User Input: Options -> intel | pgi | gcc
 if ( $#argv == 1 ) then
   setenv compiler $argv[1]
   setenv compilerVrsn Empty
 else if ( $#argv == 2 ) then
   #> Compiler Name and Version have been provided
   setenv compiler $1
   setenv compilerVrsn $2
 else
   echo "usage: $0 <compiler>"
   echo " where <compiler> is intel, pgi or gcc"
   exit(2)
 endif

#> Source the config.cmaq file to set the build environment
 cd ../../..
 source ./config_cmaq.csh

#> Source Code Repository
 setenv REPOROOT ${CMAQ_REPO}/UTIL/inline_phot_preproc  #> location of the source code for CSQY_TABLE_PROCESSOR

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 if ( ! $?MECH ) then
   set MECH =     'cb6r3_ae7_aq'
 endif

 set VRSN =      v532                       #> model version
 setenv EXEC     CSQY_TABLE_PROCESSOR_${VRSN}.exe     #> executable name for this application
 setenv WORKDIR  ${CMAQ_HOME}/UTIL/inline_phot_preproc
 setenv WORKREPO ${CMAQ_REPO}/UTIL/inline_phot_preproc
 setenv BLDIR    ${WORKDIR}/scripts/BLD_CSQY_TABLE_${VRSN}_${compilerString}
 setenv INPDIR   ${WORKDIR}/input/${MECH}
 setenv OUTDIR   ${WORKDIR}/output/${MECH}

#============================================================================================
#> Set locations for source code and templates
#============================================================================================

 set    SRCDIR = ${WORKREPO}/src
 setenv GC_INC ${CMAQ_REPO}/CCTM/src/MECHS/$MECH

# Define environment variable for path to data module for photochemical mechanism
# RXNS_DATA is the input directory containing the mechanism's data module
# value will change based on user's goals. If the file is not found, this script
# will check the output for CHEMMECH, and then check the CMAQ_REPO. If it is in 
# neither of those places, the script aborts.
 if ( ! -e ${INPDIR} ) then
    mkdir -p ${INPDIR}
 endif

#> use RXNS_DATA_MODULE, comment out if CMAQ v5.02 and keep if CMAQ v5.1 or higher
 setenv USE_RXNS_MODULES T
 if( ${USE_RXNS_MODULES} == "T" )then
    setenv RXNS_DATA_SRC   ${INPDIR}/RXNS_DATA_MODULE.F90
    if ( ! ( -e ${RXNS_DATA_SRC} ) )then
       echo 'Cannot find RXNS_DATA_MODULE. Look in CHEMMECH Output...'
       if ( ! ( -e ${CMAQ_HOME}/UTIL/chemmech/output/${MECH}/RXNS_DATA_MODULE.F90 ) ) then
          echo 'RXNS_DATA_MODULE not in CHEMMECH Output. Look in CMAQ Repo...'
          if ( ! -e ${CMAQ_REPO}/CCTM/src/MECHS/${MECH}/RXNS_DATA_MODULE.F90 ) then
             echo 'RXNS_DATA_MODULE input file does not exist'
             ls ${RXNS_DATA_SRC}
             exit 1
          else
             cp ${CMAQ_REPO}/CCTM/src/MECHS/${MECH}/RXNS_DATA_MODULE.F90 $RXNS_DATA_SRC
          endif
       else
          cp ${CMAQ_HOME}/UTIL/chemmech/output/${MECH}/RXNS_DATA_MODULE.F90 $RXNS_DATA_SRC
       endif
    endif
 endif 

#============================================================================================
#> Copy CSQY_TABLE_PROCESSOR Source Code into new build folder and compile
#============================================================================================
 
 if ( ! -e "$BLDIR" ) then
    mkdir -pv $BLDIR
 else
    if ( ! -d "$BLDIR" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 
#> Whether to include spectral values of refractive indices for aerosol species [T|Y|F|N]
#>  set F if CMAQ v5.02 and T if CMAQ v5.1 or higher
 setenv WVL_AE_REFRAC T

#> whether optical and CSQY data written to two separate file
#>  set F if CMAQ v5.02 and T if CMAQ v5.1 or higher
 setenv SPLIT_OUTPUT T

#>Number of Wavebands to write to output files starting from the band with the longest
#>to shortest wavelength from the bands; can equal 1 to 18
#>Waveband intervals come from FAST-JX version 6.8
#>CMAQ version 5.3 uses seven bands and CMAQ-MPAS uses eleven bands
setenv N_WAVEBANDS_OUT 7

#>Compile the Executable
 cp -r ${SRCDIR}/* ${BLDIR}
 cd $BLDIR ; make -f inline_phot_preproc.makefile
 if( ! ( -e ${EXEC} ) )then
    echo "failed to compile ${BLDIR}/${EXEC}"
    exit 1
 endif

#set up input data file directories
 set CSQY_DIR    = ${BLDIR}/photolysis_CSQY_data
 set REFRACT_DIR = ${BLDIR}/water_clouds
 set WVBIN_DIR   = ${BLDIR}/flux_data

# Define environment variables for inputs
 setenv GC_INC        ${INPDIR}
#wavelength bin mapping data file
 setenv WVBIN_FILE    $WVBIN_DIR/wavel-bins.dat
#Solar flux spectrum data file
 setenv FLUX_FILE     $WVBIN_DIR/solar-p05nm-UCI.dat
#Raw cross-section and quantum yield data for photolysis rates
 setenv CSQY_DATA_RAW $CSQY_DIR

#> define files for aerosol refractive indices; result output to PHOT_OPTICS.dat
 # maximum number of indices that the processor attempts to read, 
 # the number can change.  
 setenv MAX_NUMB_REFRACT 6 

 # set the list of indices to process, 
 # Their number can be less than MAX_NUMB_REFRACT.
 # The below list contains names used as optical surrogates in the CCTM source
 # code, AERO_DATA.F. To use other names requires changing AERO_DATA.F.
 setenv AE_REFRAC_LIST "WATER SOLUTE DUST SEASALT SOOT"

#Set environment variables for the paths to each refractive index in
#AE_REFRAC_LIST 
 setenv WATER     $REFRACT_DIR/water_refractive_index.dat
 setenv DUST      $REFRACT_DIR/inso00                    
 setenv SOLUTE    $REFRACT_DIR/waso00                    
 setenv SOOT      $REFRACT_DIR/soot00-two_way-Oct_21_2012
 setenv SEASALT   $REFRACT_DIR/ssam00                    

#Define output directory variable and create
 if( ! ( -d $OUTDIR ) ) mkdir -p $OUTDIR
 setenv OUT_DIR       $OUTDIR

# Execute CSQY_TABLE_PROCESSOR
 $BLDIR/$EXEC >&! bldrun.log

 cd $WORKDIR

echo " "
echo " "
echo "Check directory ${OUTDIR} for CSQY_DATA_${MECH} and PHOT_OPTICS.dat files"
echo " "
echo " "

