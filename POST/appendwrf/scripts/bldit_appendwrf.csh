#! /bin/csh -f

# ==================== APPENDWRFv5.2 Build Script ===================== #
# Usage: bldit_appendwrf.csh >&! bldit_appendwrf.log                          #
# Requirements: I/O API & netCDF libraries; a Fortran compiler        #
#                                                                     #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org                               #
# =================================================================== #

# ~~~~~~~~~~~~~~~~~~~~~~~~ Start EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> #> Note to development and evaluation teams running at NCC:
#> You must "module load" the correct modules for intel, PGI, or gfortran.
#> Invoke modules capability for use in config.cmaq
   source /etc/profile.d/modules.csh
# ~~~~~~~~~~~~~~~~~~~~~~~~~ End EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
 else
   echo "usage: $0 <compiler>"
   echo " where <compiler> is intel, pgi or gcc"
   exit(2)
 endif

#> Source the config.cmaq file to set the build environment
 cd ../../
 source ./config_cmaq.csh
 cd tools/appendwrf

#> Source Code Repository
 setenv REPOROOT ${CMAQ_REPO}/POST/appendwrf  #> location of the source code for APPENDWRF

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 set CMB_HOME = $cwd                       #> working directory
 set VRSN     = v52                        #> model version
 set EXEC     = appendwrf_${VRSN}.exe        #> executable name for this application
 set CFG      = appendwrf_${VRSN}.cfg        #> BLDMAKE configuration file name
 set BLDER    = ${CMB_HOME}/BLDMAKE_${compiler} #> location of makefile builder executable 

#> user choice: copy source files
 set CopySrc         #> copy the source files into the BLD directory

 #set MakeFileOnly    # builds a Makefile to make the model, but does not compile -
                     # comment out to also compile the model (default if not set)

# set CompileBLDMAKE  #> Recompile the BLDMAKE utility from source
                     #>   comment out to use an existing BLDMAKE executable
 set ModDriver = src #> APPENDWRF Modules


#============================================================================================
#> Computing System Configuration:
#>    Most of these settings are done in config.cmaq
#============================================================================================

#> Set full path of Fortran 90 compiler
 set FC = ${myFC}
 set FP = $FC

#> Set IO/API version
 set IOAPI = ioapi_3.1

#> Set compiler flags
 set FSTD       = "${myFSTD}"
 set DBG        = "${myDBG}"
 set F_FLAGS    = "${myFFLAGS}"
 set F90_FLAGS  = "${myFRFLAGS}"
 set CPP_FLAGS  = ""      #> Fortran Preprocessor Flags
 set LINK_FLAGS = "${myLINK_FLAG}"  #> Link Flags

 set LIB2 = "${ioapi_lib}"


#============================================================================================
#> Set up the appendwrf build directory under the Tools directory
#> for checking out and compiling source code
#============================================================================================
 set Bld = ${CMB_HOME}/BLD_appendwrf_${VRSN}_${compiler}

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

 set text = "appendwrf"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDriver};"                                       >> $Cfile
 echo                                                              >> $Cfile

#============================================================================================
#> Use BLDMAKE to create the Makefile and model executable if desired
#============================================================================================

 unalias mv rm

#> Recompile BLDMAKE from source if requested or if it does not exist
  if ( $?CompileBLDMAKE || ! -f $BLDER ) then

     #> Create a Tools Directory in which to keep BLDMAKE
     cd $CMAQ_WORK
     if ( ! -d tools/bldmake ) mkdir -pv tools/bldmake

     #> Copy all BLDMAKE files from the CMAQ Repo if none exist in
     #> tools/bldmake already. If BLDMAKE won't compile, try erasing
     #> the diles in tools/bldmake so that this utility will copy new
     #> ones from the repo.
     cp --no-clobber ${CMAQ_REPO}/UTIL/bldmake/src/* tools/bldmake/

     #> Clean BLDMAKE directory
     cd tools/bldmake
     rm *.o *.mod $BLDER
   
     #> Set BLDER to Default Path
     set BLDEXE = "bldmake_${compiler}"
     set BLDDIR = "$CMAQ_WORK/tools/bldmake"
     set BLDER  = "${BLDDIR}/${BLDEXE}"
   
     #> Compile BLDMAKE source code
     set flist = (\
          ${BLDDIR}/cfg_module.f\
          ${BLDDIR}/bldmake.f\
          ${BLDDIR}/parser.f\
          ${BLDDIR}/utils.f )
   
     foreach file ( $flist )
        $FC -c $F_FLAGS $file
     end
   
     #> Compile BLDMAKE
     $FC *.o -o $BLDEXE
     if( ! -e $BLDEXE ) then
         echo " "; echo " ***ERROR*** BLDMAKE Compile failed"; echo " "
         exit 1
     endif
     chmod 755 $BLDEXE
     echo " "; echo " Finish building $BLDEXE "

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
 mv Makefile Makefile.$compiler
 if ( -e Makefile.$compiler && -e Makefile ) rm Makefile
 ln -s Makefile.$compiler Makefile

#> Check for error during makefile generation
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif

 exit
