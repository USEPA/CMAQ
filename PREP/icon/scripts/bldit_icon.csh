#! /bin/csh -f

# ====================== ICONv5.2 Build Script ====================   
# Usage: bldit_icon.csh >&! bldit.icon.log                                
# Requirements: I/O API & netCDF libs, and a Fortran compiler    
# Note that this script is configured/tested for Red Hat Linux O/S    

# To report problems or request help with this script/program:        
#             http://www.cmascenter.org/html/help.html                
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
 set ICON_SRC = ${CMAQ_REPO}/PREP/icon/src #> location of the ICON source code
 setenv REPOROOT $ICON_SRC
 set Mechs = ${CMAQ_REPO}/CCTM/src/MECHS   #> location of the chemistry mechanism defining files

#> Working directory and Version IDs
 set VRSN  = v52                    #> Code Version
 set INPT = profile                #> Input data type: profile or m3conc?
 set EXEC = ICON_${VRSN}_$INPT.exe  #> executable name for this application
 set CFG  = ICON_${VRSN}_$INPT.cfg  #> BLDMAKE configuration file name

#> Controls for managing the source code and MPI compilation
set CompileBLDMAKE                     #> Recompile the BLDMAKE utility from source
                                       #>   comment out to use an existing BLDMAKE executable
set CopySrc                            #> copy the source files into the BLD directory
#set CopySrcTree                       #> copy the source files and directory tree into the build directory
#set Opt = verbose                     #> show requested commands as they are executed
#set MakeFileOnly                      #> uncomment to build a Makefile, but do not compile; 

#>==============================================================================
#> ICON Science Modules selection
#> NOTE: For the modules with multiple choices, choose by uncommenting.
#> look in the ICON source code repository or refer to the CMAQ documentation
#> for other possible options. Be careful. Not all options work together.
#>==============================================================================

 set ModCommon = common

 set ModType   = profile
#set ModType   = m3conc
#set ModType   = tracer

 set ModMech   = prof_data/cb05_ae6_aq #> static boundary conditions profiles (see $CMAQ_HOME/PREP/bcon/src/prof_data)

#set Mechanism = cb05tucl_ae6_aq/
#set Mechanism = cb05tump_ae6_aq/
 set Mechanism = cb05e51_ae6_aq/
#set Mechanism = cb05mp51_ae6_aq/
#set Mechanism = saprc07tb_ae6_aq/
#set Mechanism = saprc07tc_ae6_aq/
#set Mechanism = saprc07tic_ae6i_aq/
#set Mechanism = racm2_ae6_aq/
 set Tracer    = trac0               # default: no tracer species

#>#>#>#>#>#>#>#>#>#>#>#>#>#> End User Input Section #<#<#<#<#<#<#<#<#<#<#<#<#<#
#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#>#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#<#

#> Set full path of Fortran 90 compiler
 set FC = ${myFC}
 set FP = $FC
 setenv BLDER ${CMAQ_HOME}/UTIL/bldmake/bldmake_${compiler}.exe   #> name of model builder executable

#> Set compiler flags
 set xLib_Base  = ${CMAQ_LIB}
 set xLib_1     = ioapi/modules
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
 set Bld = $CMAQ_HOME/PREP/icon/scripts/BLD_ICON_${VRSN}_${INPT}_${compiler}
 if ( ! -e "$Bld" ) then
    mkdir $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 cd $Bld

 if ( $?CopySrc ) then
    /bin/cp -fp $Mechs/$Mechanism/*.nml $Bld
    /bin/cp -fp $Mechs/$Tracer/*.nml $Bld
 else
    /bin/ln -s $Mechs/$Mechanism/*.nml $Bld
    /bin/ln -s $Mechs/$Tracer/*.nml $Bld
 endif

#> make the config file

 set Cfile = ${CFG}.bld
 set quote = '"'

 echo                                                               > $Cfile
 echo "model       $EXEC;"                                         >> $Cfile
 echo                                                              >> $Cfile
 echo "repo        $ICON_SRC;"                                        >> $Cfile
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
 echo "ioapi       $quote$LIB1$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdf      $quote$LIB2$quote;"                             >> $Cfile
 echo   

 set text="// mechanism:"
 echo "$text ${Mechanism}"                                         >> $Cfile
 echo "// project repository: ${ICON_SRC}"                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "common"
 echo "// required" $text                                          >> $Cfile
 echo "Module ${ModCommon};"                                       >> $Cfile
 echo                                                              >> $Cfile

 set text = "profile, m3conc, tracer"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModType};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "cb05, saprc99, saprc07t"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModMech};"                                         >> $Cfile
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
 mv Makefile Makefile.$compiler
 # if ( -e Makefile.$compiler && -e Makefile ) rm Makefile
 ln -s Makefile.$compiler Makefile
 
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
