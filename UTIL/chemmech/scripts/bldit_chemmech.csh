#! /bin/csh -f

# ================== CHEMMECHv5.5.x Build Script ==================== #
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
   setenv compiler intel
   setenv compilerVrsn Empty
   echo "compiler and version not set"
   echo "usage: $0 <compiler>"
   echo "setting compiler to intel"
 endif

#> Source the config.cmaq file to set the build environment
 if( -e ../../../config_cmaq.csh )then
    cd ../../..
    source ./config_cmaq.csh
 else
#work offline from CMAQ build environment
    setenv offline "Y"
    setenv compilerString ${compiler}
 endif

#> Source Code Repository
 if( ! ( $?offline ) )then
   setenv REPOROOT ${CMAQ_REPO}/UTIL/chemmech  #> location of the source code for CHEMMECH
 else
   setenv REPOROOT $cwd/..
 endif

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 set VRSN     = v55                        #> model version
 setenv CFG     CHEMMECH_${VRSN}.cfg       #> BLDMAKE configuration file name!
 setenv CLEAR  "TRUE"                      #> delete build directory if exists

#============================================================================================
#> Set up the CHEMMECH build directory under the UTIL directory
#> for checking out and compiling source code
#============================================================================================
 if( ! ( $?offline ) )then
    set Bld = ${CMAQ_HOME}/UTIL/chemmech/scripts/BLD_chemmech_${VRSN}_${compilerString}
 else
    set Bld = BLD_chemmech_${VRSN}_${compilerString}
 endif

 if ( -e "$Bld" ) then
    if( $CLEAR == "FALSE" )then
       echo "   *** build directory exists, set CLEAR to TRUE to remove it***"
       exit(1)
    endif
    echo "   *** build directory exist, deleting it***"
    \rm -rf $Bld
 endif
 mkdir -pv $Bld

#============================================================================================
#> Copy Chemmech Source Code into new build folder and compile
#============================================================================================
 cp ${REPOROOT}/src/* $Bld
 sed -i 's/ compiler = intel/#compiler = intel/' ${Bld}/Makefile
 sed -i 's/#compiler = '${compiler}'/ compiler = '${compiler}'/' ${Bld}/Makefile

 cd ${Bld}; make clean; make 
 if( ! ( -e CHEMMECH.exe ) )then
    echo "failed to compile ${Bld}/${EXEC}"
    exit 1
 endif

exit(0)
