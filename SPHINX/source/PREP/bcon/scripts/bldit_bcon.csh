#!/bin/csh -f

# ====================== BCONv5.5.X Build Script ====================== 
# Usage: bldit_bcon.csh >&! bldit_bcon.log                                
# Requirements: I/O API & netCDF libs and a Fortran compiler    
# Note that this script is configured/tested for Red Hat Linux O/S    
#
# To report problems or request help with this script/program:        
#             http://www.cmascenter.org
# ===================================================================

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
 cd ../../../
 source ./config_cmaq.csh

 set echo

# =======================================================================
#> Begin User Input Section
# =======================================================================

#> Source Code Locations
 set BCON_SRC = ${CMAQ_REPO}/PREP/bcon/src #> location of the BCON source code
 setenv REPOROOT $BCON_SRC

#> Working directory and Version IDs
 set VRSN  = v55                    #> Code Version
 set EXEC = BCON_${VRSN}.exe        #> executable name for this application
 set CFG  = BCON_${VRSN}.cfg        #> BLDMAKE configuration file name

#> Controls for managing the source code and MPI compilation
 set CompileBLDMAKE                 #> Recompile the BLDMAKE utility from source
                                    #>   comment out to use an existing BLDMAKE executable
 set CopySrc                        #> copy the source files into the BLD directory
#set CopySrcTree                    #> copy the source files and directory tree into the build directory
#set Opt = verbose                  #> show requested commands as they are executed
#set MakeFileOnly                   #> uncomment to build a Makefile, but do not compile; 
                                    #>   comment out to compile the model (default if not set)
#set Debug_BCON                     #> uncomment to compile BCON with debug option equal to TRUE
                                    #>   comment out to use standard, optimized compile process

#>==============================================================================
#> BCON Science Modules
#>
#> NOTE:  BC type is now a runtime option.  All BC types are included at
#>        compile time
#>==============================================================================

 set ModCommon = common

 set ModM3conc = m3conc

 set ModProfile = profile

#>#>#>#>#>#>#>#>#>#>#>#>#>#> End User Input Section #<#<#<#<#<#<#<#<#<#<#<#<#<#
#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#

#> Set full path of Fortran 90 compiler
 set FC = ${myFC}
 set FP = $FC
 setenv BLDER ${CMAQ_HOME}/UTIL/bldmake/bldmake_${compilerString}.exe   #> name of model builder executable

#> Set compiler flags
 set xLib_Base  = ${CMAQ_LIB}
 set xLib_1     = ioapi/lib
 set xLib_2     = ioapi/include_files
 set xLib_4     = ioapi/lib
 set FSTD       = "${myFSTD}"
 set DBG        = "${myDBG}"
 set F_FLAGS    = "${myFFLAGS}"
 set F90_FLAGS  = "${myFRFLAGS}"
 set CPP_FLAGS  = ""
 set LINK_FLAGS = "${myLINK_FLAG}"

 set LIB1 = "$ioapi_lib"
 set LIB2 = "$netcdf_lib $extra_lib"
 set LIB3 = "$netcdff_lib"

#============================================================================================
#> Implement User Input
#============================================================================================

#> Check for CMAQ_REPO and CMAQ_LIB settings:
 if ( ! -e $CMAQ_REPO || ! -e $CMAQ_LIB ) then
    echo "   $CMAQ_REPO or $CMAQ_LIB directory not found"
    exit 1
 endif
 echo "    Model repository base path: $CMAQ_REPO"
 echo "                  library path: $CMAQ_LIB"

#> If $CMAQ_MODEL is not set, default to $CMAQ_REPO
 if ( $?CMAQ_MODEL ) then
    echo "         Model repository path: $CMAQ_MODEL"
 else

#> This script was written for Linux hosts only. If
#> the host system is not Linux, produce an error and stop
 set BLD_OS = `uname -s`       
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> Set and create the "BLD" directory for checking out and compiling 
#> source code. Move current directory to that build directory.
 set Bld = $CMAQ_HOME/PREP/bcon/scripts/BLD_BCON_${VRSN}_${compilerString}
 if ( ! -e "$Bld" ) then
    mkdir $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 cd $Bld

#> make the config file

 set Cfile = $CFG.bld
 set quote = '"'

 echo                                                               > $Cfile
 echo "model       $EXEC;"                                         >> $Cfile
 echo                                                              >> $Cfile
 echo "repo        $BCON_SRC;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_base    $xLib_Base;"                                    >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_1       $xLib_1;"                                       >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_2       $xLib_2;"                                       >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_4       $xLib_4;"                                       >> $Cfile
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
 echo "ioapi       $quote$LIB1$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdf      $quote$LIB2$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdff      $quote$LIB3$quote;"                            >> $Cfile
 echo                                                              >> $Cfile

 echo "// project repository location: ${BCON_SRC}"                >> $Cfile
 echo                                                              >> $Cfile

 set text = "common"
 echo "// required" $text                                          >> $Cfile
 echo "Module ${ModCommon};"                                       >> $Cfile
 echo                                                              >> $Cfile

 set text = "m3conc"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModM3conc};"                                       >> $Cfile
 echo                                                              >> $Cfile

 set text = "profile"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModProfile};"                                      >> $Cfile
 echo                                                              >> $Cfile

 if ( $?ModMisc ) then
    echo "Module ${ModMisc};"                                      >> $Cfile
    echo                                                           >> $Cfile
 endif

# ============================================================================
#> Create Makefile and Model Executable
# ============================================================================

 unalias mv rm

#> Recompile BLDMAKE from source if requested or if it does not exist
 if ( $?CompileBLDMAKE || ! -f $BLDER ) then
   cd ${CMAQ_REPO}/UTIL/bldmake/scripts
   ./bldit_bldmake.csh
 endif
 set Blder = "$BLDER -serial -verbose"

#> Relocate to the BLD_* directory 
 cd $Bld

# Set BCON debug flags if true
 if ( $?Debug_BCON ) then
    set Blder = "${Blder} -debug_cctm"
 endif

#> Run BLDMAKE Utility
 if ( $?MakeFileOnly ) then
    if ( $?CopySrc ) then
       $Blder -makefo $Cfile
    else
       $Blder -makefo -git_local $Cfile   # $Cfile = ${CFG}
     # totalview -a $Blder -makefo $Cfile
    endif
 else   # also compile the model
    if ( $?CopySrc ) then
       $Blder $Cfile
    else
       $Blder -git_local $Cfile
    endif
 endif

#> Rename Makefile to specify compiler option and link back to Makefile
 mv Makefile Makefile.$compilerString
 if ( -e Makefile.$compilerString && -e Makefile ) rm Makefile
 ln -s Makefile.$compilerString Makefile

#> Alert user of error in BLDMAKE if it ocurred
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif

#> Preserve old Config file, if it exists, before moving new one to 
#> build directory.
 if ( -e "$Bld/${CFG}" ) then
    echo "   >>> previous ${CFG} exists, re-naming to ${CFG}.old <<<"
    unalias mv
    mv $Bld/${CFG} $Bld/${CFG}.old
 endif
 mv ${CFG}.bld $Bld/${CFG}

 exit
