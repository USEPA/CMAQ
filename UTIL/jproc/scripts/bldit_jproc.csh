#!/bin/csh -f

# ====================== JPROCv5.2 Build Script ===================== 
# Usage: bldit.jproc >&! bldit.jproc.log                              
# Requirements: I/O API & netCDF libraries; a Fortran compiler
#
# To report problems or request help with this script/program:        
#             http://www.cmascenter.org
# =================================================================== 

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

#> Source the config.cmaq file.csh to set the build environment
 source ../../../config_cmaq.csh
 set echo

#:#:#:#:#:#:#:#:#:#:#:# Begin User Input Section #:#:#:#:#:#:#:#:#:#:#:#

#> Source Code Repository 
 set MODEL = $CMAQ_REPO/UTIL/jproc/src        #> location of the JPROC source code
 setenv REPOROOT $MODEL
 set GlobInc = $CMAQ_REPO/CCTM/src/ICL        #> location of the global include files
 set Mechs   = $CMAQ_REPO/CCTM/src/MECHS      #> location of the chemistry mechanism include files

#> Working directory and application IDs
 set Base = $cwd                       #> working directory for compiling the source code
 set APPL = v53                       #> model configuration ID
 set EXEC = JPROC_${APPL}_$EXEC_ID    #> executable name
 set CFG  = cfg.$EXEC                #> configuration file name

#> Controls for managing the source code compilation
 set CopySrc                           #> copy the source files into a working irectory
 set MakeFileOnly                      #> uncomment to build a Makefile, but do not compile; comment out to compile the model (default if not set)
 set CompileBLDMAKE  #> Recompile the BLDMAKE utility from source

#======================================#>
#> JPROC Science Modules
#======================================#>
#> NOTE: For the modules with multiple options, a note is provided on where to look in the
#>   source code archive for a list of the possible settings; users may also refer to the CMAQ documentation

 set ModCommon = common
 set Mechanism = cb6r3_ae7_aq        #> chemical mechanism (see $CMAQ_HOME/CCTM/MECHS)
 set Tracer    = trac0                 #> tracer configuration directory under $CMAQ_HOME/CCTM/MECHS [ default: no tracer species 
 set APPL       = ${APPL}_${Mechanism}

#======================================#>
#> Computing System Configuration:
#>    Most of these settings are done in config.cmaq
#======================================#>

 set FC = ${myFC}                      #> path of Fortan compiler; set in config.cmaq
 set FP = $FC                          #> path of Fortan preprocessor; set in config.cmaq

 set Blder = "$CMAQ_REPO/UTIL/bldmake/src/bldmake "  #> location of model builder executable

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
 set C_FLAGS    = "${myCFLAGS} -DFLDMN"
 set LINK_FLAGS = "${myLINK_FLAG}"

#:#:#:#:#:#:#:#:#:#:#:# End of User Input Section :#:#:#:#:#:#:#:#:#:#:#:#:#

#> Check for CMAQ_REPO and CMAQ_LIB settings:
 if ( ! -e $CMAQ_REPO || ! -e $CMAQ_LIB  ) then
    echo "   CMAQ_REPO or CMAQ_LIB directory not found"
    exit 1
 endif
 echo "    Model repository base path: $CMAQ_REPO"
 echo "                  library path: $CMAQ_LIB"
 
 set BLD_OS = `uname -s`        ## Script set up for Linux only 
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> The "BLD" directory for checking out and compiling source code
 set Bld = $Base/BLD_${APPL}
 if ( ! -e "$Bld" ) then
    mkdir $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
#\cp -f $Mechs/$Mechanism/RXNS_DATA_MODULE.F90 ${Bld}/.
 \cp -f $GlobInc/fixed/const/CONST.EXT ${Bld}/.

 set LIB1 = "$ioapi_lib"
 set LIB2 = "$netcdf_lib"
 set LIB3 = "$netcdff_lib"

 set ICL_CONST = $Bld

#> make the config file

 set Cfile = ${Bld}/${CFG}.bld
 set quote = '"'

 echo                                                               > $Cfile
 echo "model       $EXEC;"                                         >> $Cfile
 echo                                                              >> $Cfile
 echo "repo        $MODEL;"                                        >> $Cfile
 echo                                                              >> $Cfile
 echo "mechanism   $Mechanism;"                                    >> $Cfile
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
 #echo "libraries   $quote$LIBS$quote;"                             >> $Cfile
 echo "ioapi       $quote$LIB1$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdf      $quote$LIB2$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdff      $quote$LIB3$quote;"                             >> $Cfile
 echo                                                              >> $Cfile

 set text="// mechanism:"
 echo "$text ${Mechanism}"                                         >> $Cfile
 echo "// model repository: ${REPOROOT}"                           >> $Cfile
 echo                                                              >> $Cfile
 echo "include SUBST_CONST      CONST.EXT;"                        >> $Cfile

 set text = "common"
 echo "// required" $text                                          >> $Cfile
 echo "Module ${ModCommon};"                                       >> $Cfile
 echo                                                              >> $Cfile


# set text = "jproc_table"
# echo "// options are" $text                                       >> $Cfile
# echo "Module ${ModDriver};"                                       >> $Cfile
# echo                                                              >> $Cfile

 set ModMisc = $Mechs/$Mechanism/RXNS_DATA_MODULE.F90
# \cp -f $ModMisc ${MODEL}/common
 if ( $?ModMisc ) then
    echo "MISC ${ModMisc};"                                        >> $Cfile
    echo                                                           >> $Cfile
 endif

#> Recompile BLDMAKE from source if requested or if it does not exist
if ( $?CompileBLDMAKE || ! ( -f $Blder ) ) then
     cd ${CMAQ_REPO}/UTIL/bldmake/src
     make clean
     make
endif
cd $Bld

#> make the Makefile or the model executable

 unalias mv rm
 if ( -e $Bld/Makefile.$compiler || -e $Bld/Makefile ) then
     rm $Bld/Makefile
     rm $Bld/Makefile.$compiler
 endif

 if ( $?MakeFileOnly ) then
    if ( $?CopySrc ) then
       $Blder -makefo -serial -verbose $Cfile
    else
       $Blder -makefo -git_local -serial -verbose $Cfile   # $Cfile = ${CFG}.bld
     # totalview -a $Blder -makefo -serial -verbose $Cfile
    endif
 else   # also compile the model
    if ( $?CopySrc ) then
       $Blder -serial -verbose $Cfile
    else
       $Blder -git_local -serial -verbose $Cfile
    endif
 endif
 mv Makefile $Bld/Makefile.$compiler
 ln -s ./Makefile.$compiler Makefile

 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif
#if ( -e "$Base/${CFG}" ) then
#   echo "   >>> previous ${CFG} exists, re-naming to ${CFG}.old <<<"
#   mv $Base/${CFG} $Base/${CFG}.old
#endif
#mv ${CFG}.bld $Bld/${CFG}
#\rm -f ${MODEL}/common/RXNS_DATA_MODULE.F90

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#
 exit
