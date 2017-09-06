#! /bin/csh -f

# ==================== SITECMPv5.2 Build Script ===================== #
# Usage: bldit_sitecmp.csh >&! bldit_sitecmp.log                          #
# Requirements: I/O API & netCDF libraries; a Fortran compiler        #
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
 setenv REPOROOT ${CMAQ_REPO}/POST/sitecmp  #> location of the source code for SITECMP

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 set VRSN     = v52                        #> model version
 set EXEC     = sitecmp_${VRSN}.exe        #> executable name for this application
 set CFG      = sitecmp_${VRSN}.cfg        #> BLDMAKE configuration file name
 setenv BLDER   ${CMAQ_HOME}/UTIL/bldmake/bldmake_${compilerString}.exe #> location of makefile builder executable 

#> user choice: copy source files
 set CopySrc         #> copy the source files into the BLD directory

 #set MakeFileOnly    # builds a Makefile to make the model, but does not compile -
                     # comment out to also compile the model (default if not set)

# set CompileBLDMAKE  #> Recompile the BLDMAKE utility from source
                     #>   comment out to use an existing BLDMAKE executable
 set ModDriver = src #> SITECMP Modules


#============================================================================================
#> Computing System Configuration:
#>    Most of these settings are done in config.cmaq
#============================================================================================

#> Set full path of Fortran 90 compiler
 setenv FC ${myFC}
 set FP = $FC

#> Set IO/API version
 set IOAPI = ioapi_3.1

#> Set compiler flags
 set FSTD       = "${myFSTD}"
 set DBG        = "${myDBG}"
 setenv F_FLAGS   "${myFFLAGS}"
 set F90_FLAGS  = "${myFRFLAGS}"
 set CPP_FLAGS  = ""      #> Fortran Preprocessor Flags
 set LINK_FLAGS = "${myLINK_FLAG}"  #> Link Flags

 set LIB2 = "${ioapi_lib}"


#============================================================================================
#> Set up the sitecmp build directory under the Tools directory
#> for checking out and compiling source code
#============================================================================================
 set Bld = ${CMAQ_HOME}/POST/sitecmp/scripts/BLD_sitecmp_${VRSN}_${compilerString}

 if ( ! -e "$Bld" ) then
    mkdir -pv $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif

 cd $Bld

#> Check for previous run
 if ( -e "$Bld/${CFG}" ) then
    echo "   >>> previous ${CFG} exists, re-naming to ${CFG}.old <<<"
    mv $Bld/${CFG} $Bld/${CFG}.old
 endif

#============================================================================================
#> Make the config file
#============================================================================================
 set Cfile = ${Bld}/$CFG
 set quote = '"'

 echo                                                               > $Cfile
 echo "model       $EXEC;"                                         >> $Cfile
 echo                                                              >> $Cfile
 echo "repo        $REPOROOT;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_base    ${CMAQ_LIB};"                                   >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_1       ioapi/modules;"                                 >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_2       ioapi/include_files;"                           >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_3       netcdf/include;"                                >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_4       ioapi/lib;"                                     >> $Cfile
 echo                                                              >> $Cfile
 set text = "$quote$CPP_FLAGS$quote;"
 echo "cpp_flags   $text"                                          >> $Cfile
 echo                                                              >> $Cfile
 echo "f_compiler  $FC;"                                           >> $Cfile
 echo                                                              >> $Cfile
 echo "fstd        $quote$FSTD$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "dbg         $quote$DBG$quote;"                              >> $Cfile
 echo                                                              >> $Cfile
 echo "f_flags     $quote$F_FLAGS$quote;"                          >> $Cfile
 echo                                                              >> $Cfile
 echo "f90_flags   $quote$F90_FLAGS$quote;"                        >> $Cfile
 echo                                                              >> $Cfile
 echo "link_flags  $quote$LINK_FLAGS$quote;"                       >> $Cfile
 echo                                                              >> $Cfile
#echo "libraries   $quote$LIBS$quote;"                             >> $Cfile
 echo "ioapi       $quote$LIB2$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdf      $quote$netcdf_lib$quote;"                       >> $Cfile

 set text = "sitecmp"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDriver};"                                       >> $Cfile
 echo                                                              >> $Cfile

#============================================================================================
#> Use BLDMAKE to create the Makefile and model executable if desired
#============================================================================================

 unalias mv rm

#> Recompile BLDMAKE from source if requested or if it does not exist
 if ( $?CompileBLDMAKE || ! -f $BLDER ) then
    cd ${CMAQ_REPO}/UTIL/bldmake/scripts
    ./bldit_bldmake.csh
 endif
 
#> Relocate to the BLD_* directory
  cd $Bld 

#> Set serial options for BLDMAKE execution
  set Blder = "$BLDER -serial -verbose"

#> Run BLDMAKE Utility
 if ( $?MakeFileOnly ) then
    #> Just create the Makefile
    if ( $?CopySrc ) then
       $Blder -makefo $Cfile
    else
       $Blder -makefo -git_local $Cfile   
    endif
 else  
    # Also compile the model
    if ( $?CopySrc ) then
       $Blder $Cfile
    else
       $Blder -git_local $Cfile
    endif
 endif

#> Save Makefile with Compiler-dependent name and create symbolic
#> link back to generic name.
 mv Makefile Makefile.$compilerString
 if ( -e Makefile.$compilerString && -e Makefile ) rm Makefile
 ln -s Makefile.$compilerString Makefile

#> Check for error during makefile generation
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif

 exit
