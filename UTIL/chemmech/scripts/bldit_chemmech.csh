#! /bin/csh -f

# ================== CHEMMECHv5.3.x Build Script ==================== #
# Usage: bldit_chemmech.csh >&! bldit_chemmech.log                    #
# Requirements: None                                                  #
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
   exit 1
 endif

#> Source the config.cmaq file to set the build environment
 cd ../../..
 source ./config_cmaq.csh

#> Source Code Repository
 setenv REPOROOT ${CMAQ_REPO}/UTIL/chemmech  #> location of the source code for CHEMMECH

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 set VRSN     = v532                       #> model version
 setenv EXEC    CHEMMECH_${VRSN}.exe       #> executable name for this application
 setenv CFG     CHEMMECH_${VRSN}.cfg       #> BLDMAKE configuration file name!


#============================================================================================
#> Set up the CHEMMECH build directory under the UTIL directory
#> for checking out and compiling source code
#============================================================================================
 set Bld = ${CMAQ_HOME}/UTIL/chemmech/scripts/BLD_chemmech_${VRSN}_${compilerString}

 if ( ! -e "$Bld" ) then
    mkdir -pv $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif

#============================================================================================
#> Copy Chemmech Source Code into new build folder and compile
#============================================================================================
 cd $Bld

 cp ${CMAQ_REPO}/UTIL/chemmech/src/* $Bld

 cd ${Bld}; make clean; make 
 if( ! ( -e ${EXEC} ) )then
    echo "failed to compile ${Bld}/${EXEC}"
    exit 1
 endif


