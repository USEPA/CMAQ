#!/bin/csh -fx

# ============ CSQY_TABLE_PROCESSOR_v5.4.x Build Script ================= #
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
   setenv compiler intel
   setenv compilerVrsn Empty
   echo "compiler and version not set"
   echo "usage: $0 <compiler>"
   echo "setting compiler to intel"
 endif

set echo 
#> Source the config.cmaq file to set the build environment
 if( -e ../../../config_cmaq.csh )then
    cd ../../..
    source ./config_cmaq.csh
 else
#work offline from CMAQ repository and build environment
    setenv offline "Y"
    setenv compilerString ${compiler}
    setenv CMAQ_HOME $cwd/..
 endif

#> Source Code Repository
 if( ! ( $?offline ) )then
   setenv REPOROOT ${CMAQ_REPO}/UTIL/inline_phot_preproc  #> location of the source code for CHEMMECH
 else
   setenv REPOROOT ${CMAQ_HOME}
 endif

unset echo

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 if ( ! $?MECH ) then
   set MECH =     'cb6r3_ae7_aq'
 endif
 setenv CLEAR "TRUE" #> over-write existing output files

 if( ! ( $?offline ) )then
   set WORKDIR = ${CMAQ_HOME}/UTIL/inline_phot_preproc
 else
   set WORKDIR = ${CMAQ_HOME}
 endif
 if ( ! $?INPDIR ) then
    setenv INPDIR   ${WORKDIR}/input/${MECH}
 endif
 if ( ! $?OUTDIR ) then
    setenv OUTDIR   ${WORKDIR}/output/${MECH}
 endif

 set VRSN =      v54                                 #> model version
 setenv EXEC     CSQY_TABLE_PROCESSOR_${VRSN}.exe     #> executable name for this application
 setenv WORKREPO ${REPOROOT}
 setenv BLDIR    ${WORKDIR}/scripts/BLD_CSQY_TABLE_${VRSN}_${compilerString}
#============================================================================================
#> Set locations for source code and templates
#============================================================================================

 set    SRCDIR = ${WORKREPO}/src

# Define environment variable for path to data module for photochemical mechanism
# RXNS_DATA is the input directory containing the mechanism's data module
# value will change based on user's goals. If the file is not found, this script
# will check the output for CHEMMECH, and then check the CMAQ_REPO. If it is in 
# neither of those places, the script aborts.

#> use RXNS_DATA_MODULE, comment out if CMAQ v5.02 and keep if CMAQ v5.1 or higher
 setenv USE_RXNS_MODULES T
 if( ${USE_RXNS_MODULES} == "T" )then
    setenv RXNS_DATA_SRC   ${INPDIR}/RXNS_DATA_MODULE.F90

    if ( ! -e ${RXNS_DATA_SRC} ) then
       echo 'Below RXNS_DATA_MODULE input file does not exist'
       echo ${RXNS_DATA_SRC}
       exit 1
    endif
 endif 

#============================================================================================
#> Copy CSQY_TABLE_PROCESSOR Source Code into new build folder and compile
#============================================================================================
 
 if ( -e "$BLDIR" ) then
    echo "   *** build directory exist, deleting it***"
    \rm -rf $BLDIR
 endif
 mkdir -pv $BLDIR
 
#> Whether to include spectral values of refractive indices for aerosol species [T|Y|F|N]
#>  set F if CMAQ v5.02 and T if CMAQ v5.1 or higher
 setenv WVL_AE_REFRAC T

#> whether optical and CSQY data written to two separate file
#>  set F if CMAQ v5.02 and T if CMAQ v5.1 or higher
 setenv SPLIT_OUTPUT T

#>Number of Wavebands to write to output files starting from the band with the longest
#>to shortest wavelength from the bands; can equal 1 to 18
#>Waveband intervals come from FAST-JX version 6.8
#>CMAQ version 5.3+ uses seven bands and CMAQ-MPAS uses eleven bands
setenv N_WAVEBANDS_OUT 7

#>Compile the Executable
 cp -r ${SRCDIR}/* ${BLDIR}/.
 cd $BLDIR ; make clean; make -f inline_phot_preproc.makefile
 if( ! ( -e ${EXEC} ) )then
    echo "failed to compile ${BLDIR}/${EXEC}"
    exit 1
 endif

#set up input data file directories
 set CSQY_DIR       = ${WORKREPO}/photolysis_CSQY_data
 set REFRACT_DIR    = ${WORKREPO}/refractive_indices
 set WVBIN_DIR      = ${WORKREPO}/flux_data
 set ICE_CLOUDS_DIR = ${WORKREPO}/ice_clouds

 set data_paths = ( ${CSQY_DIR} ${REFRACT_DIR} ${WVBIN_DIR} ${ICE_CLOUDS_DIR} )
 foreach data_dir ( ${data_paths} )
    if( ! ( -e ${data_dir} ) )cp -r $data_dir ${WORKDIR}/.
 end

# Define environment variables for inputs
#Wavelength bin mapping data file
 setenv WVBIN_FILE    $WVBIN_DIR/wavel-bins.dat

#Files describing optical properties of ice cloud particles
 setenv ICE_CLD_SSA ${ICE_CLOUDS_DIR}/fu96.ssa
 setenv ICE_CLD_EXT ${ICE_CLOUDS_DIR}/fu96.ext
 setenv ICE_CLD_ASY ${ICE_CLOUDS_DIR}/fu96.asy
 setenv ICE_CLD_DEL ${ICE_CLOUDS_DIR}/fu96.del

#Solar flux spectrum data file
 setenv FLUX_FILE     $WVBIN_DIR/solar-p05nm-UCI.dat
#Raw cross-section and quantum yield data for photolysis rates
 setenv CSQY_DATA_RAW $CSQY_DIR

#> define files for aerosol refractive indices; result output to PHOT_OPTICS.dat
 # maximum number of indices that the processor attempts to read, 
 # the number can change.  
 setenv MAX_NUMB_REFRACT 16

 # set the list of indices to process, 
 # Their number can be less than MAX_NUMB_REFRACT.
 # The below list contains names used as optical surrogates in the CCTM source
 # code, AERO_DATA.F. To use other names requires changing AERO_DATA.F.
 setenv AE_REFRAC_LIST "WATER SOLUTE DUST SEASALT SOOT ISOP_NOX ISOP_SOX LIMONENE_SOA APINENE_SOA NAPTH_SOA MXYL_HIGH_NOX MXYL_LOW_NOX TOLU_HIGH_NOX TOLU_LOW_NOX ORGCARB BIOMASS"

#Set environment variables for the paths to each refractive index in
#AE_REFRAC_LIST 
 setenv WATER         ${REFRACT_DIR}"/water_refractive_index.dat"
 setenv DUST          ${REFRACT_DIR}"/OPAC_water_clouds/inso00"                    
 setenv SOLUTE        ${REFRACT_DIR}"/OPAC_water_clouds/waso00"                    
 setenv SOOT          ${REFRACT_DIR}"/OPAC_water_clouds/soot00-two_way-Oct_21_2012"
 setenv SEASALT       ${REFRACT_DIR}"/OPAC_water_clouds/ssam00"                    
 setenv ISOP_NOX      ${REFRACT_DIR}"/IE_refractive_indices/nakayama_2018_isoprene_NOX_SOA_refractive_indices.txt"
 setenv ISOP_SOX      ${REFRACT_DIR}"/IE_refractive_indices/nakayama_2018_isoprene_SOX_SOA_refractive_indices.txt"
 setenv LIMONENE_SOA  ${REFRACT_DIR}"/IE_refractive_indices/Lui_2013_Limonene_SOA_refractive_indices.txt"
 setenv APINENE_SOA   ${REFRACT_DIR}"/IE_refractive_indices/Lui_2013_APIN_SOA_refractive_indices.txt"
 setenv NAPTH_SOA     ${REFRACT_DIR}"/IE_refractive_indices/Lambe_2013-naphthalene_SOA_refractive_indices.txt"
 setenv MXYL_HIGH_NOX ${REFRACT_DIR}"/IE_refractive_indices/Lui_2015_refractive_indices_mxylene_SOA_high_NOX.txt"
 setenv MXYL_LOW_NOX  ${REFRACT_DIR}"/IE_refractive_indices/Lui_2015_refractive_indices_mxylene_SOA_low_NOX.txt"
 setenv TOLU_HIGH_NOX ${REFRACT_DIR}"/IE_refractive_indices/Lui_2015_refractive_indices_toluene_SOA_high_NOX.txt"
 setenv TOLU_LOW_NOX  ${REFRACT_DIR}"/IE_refractive_indices/Lui_2015_refractive_indices_toluene_SOA_low_NOX.txt"
 setenv ORGCARB       ${REFRACT_DIR}"/adient_aerosol_refrac_indx/refract_organicc_new.txt"
 setenv BIOMASS       ${REFRACT_DIR}"/adient_aerosol_refrac_indx/refract_biomass_new.txt"

#Define output directory variable and create
 if( ( -d $OUTDIR ) )then
    if( -e ${OUTDIR}/CSQY_DATA_${MECH} && -e ${OUTDIR}/PHOT_OPTICS.dat )then
       if( $CLEAR == "FALSE")then
         echo "Previous output exists; set CLEAR to TRUE to delete"
         exit(1)
       endif
       \rm -rf $OUTDIR
    endif
 endif
 mkdir -p $OUTDIR
 setenv OUT_DIR       $OUTDIR
 
# Execute CSQY_TABLE_PROCESSOR
#cd ${WORKDIR}
 $BLDIR/$EXEC >&! ${WORKDIR}/scripts/bldrun.log
 set signal = ` tail -1 ${WORKDIR}/scripts/bldrun.log `
echo " "
 echo ${signal}
 set output = $signal # `grep "NORMAL_STOP" bldrun.log `
 echo $output
echo " "

#cd ${WORKDIR}/scripts

 if ( $? != 0  || $output != "NORMAL_STOP" ) then
    echo "INLINE_PHOT_PREPROC ($BLDIR/$EXEC) failed for some reason. Halt Build Process!"
    echo "Try checking end of ${WORKDIR}/scripts/bldrun.log"
    exit 1
 endif

 cd $WORKDIR

echo " "
echo " "
if( ( -e ${OUTDIR}/CSQY_DATA_${MECH} ) && -e ${OUTDIR}/PHOT_OPTICS.dat )then
  echo "CSQY_DATA_${MECH} and PHOT_OPTICS.dat files created. Check each in ${OUTDIR}"
else if( ( -e ${OUTDIR}/CSQY_DATA_* ) && -e ${OUTDIR}/PHOT_OPTICS.dat )then
  cd ${OUTDIR}
  set OFILE = `/usr/bin/ls CSQY_DATA_* `
  cd -
  echo "CSQY_DATA_${MECH} file not created."
  echo "HOWEVER, ${OFILE} FILE WAS CREATED."
  echo "Try changing mechanism name in the mech_${MECH}.def file."
  echo "Else check end of ${WORKDIR}/scripts/bldrun.log"
else
  echo "CSQY_DATA_${MECH} and PHOT_OPTICS.dat files not created."
  echo "Check end of ${WORKDIR}/scripts/bldrun.log"
endif
echo " "
echo " "

