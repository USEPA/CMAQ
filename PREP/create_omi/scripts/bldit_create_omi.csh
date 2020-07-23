#! /bin/csh -f

# ==================== Build Script ================================= #
# Usage:  bldit_create_omi.csh COMPILER                               #
#         where the COMPILER agrument specifies brand of FORTRAN      #
#         compiler. Available options: intel, pgi, and gcc            #
#                                                                     #
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

if ( $#argv == 1 ) then
    setenv compiler $argv[1]
else
    echo "usage: $0 <compiler>"
    echo " where <compiler> is intel, pgi or gcc"
    exit(2)
endif

#> Repositories
 set    CMAQ_REPO = "/home/hwo/CCTM_git_repository"
 setenv PREP_REPO ${CMAQ_REPO}/PREP
 setenv UTIL_REPO ${CMAQ_REPO}/UTIL
 setenv REPOROOT  ${PREP_REPO}/create_omi  #> location of the create_omi's repository

#>Work directory
 set WORK_DIR = ${REPOROOT} 

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 set VRSN     = v532           #> version
 set EXEC     = create_omi     #> executable name for this application
 set CFG      = create_omi.cfg #> bldmake configuration file name

 set bld_dir_blder = ${UTIL_REPO}/bldmake/src #> location of makefile builder executable 
 setenv BLDER        ${bld_dir_blder}/bldmake #> builder executable  

#> user choice: copy source files
 set CopySrc         #> copy the source files into the BLD directory

 set MakeFileOnly    # builds a Makefile to make the model, but does not compile -
                     # comment out to also compile the model (default if not set)

 set CompileBLDMAKE  #> Recompile the BLDMAKE utility from source
                      #>   comment out to use an existing BLDMAKE executable
 set ModDriver = src  #> source code subdirectory
 set echo

#============================================================================================
#> Computing System Configuration:
#>    Compiler flags and libraries' path, the latter set in config.cmaq
#============================================================================================

#> Set Fortran 90 compiler
switch ( $compiler ) 
 case intel:
   setenv FC ifort
   setenv COMPILER INTEL
   set FSTD       = "-O2 -traceback"
   set DBG        = "-O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback"
   setenv F_FLAGS   "-fixed -132"
   setenv F90_FLAGS "-free"
   setenv IOAPI_INCL /home/wdx/lib/x86_64/intel/ioapi_3.1/ioapi/fixed_src    #> I/O API include header files
   setenv IOAPI_LIB  /home/wdx/lib/x86_64/intel/ioapi_3.1/Linux2_x86_64ifort #> I/O API libraries
   setenv NETCDF     /home/wdx/lib/x86_64/intel/netcdf                       #> netCDF directory path
   breaksw
 case pgi:
   setenv FC pgf90
   setenv COMPILER PGF90
   set FSTD       = "-O2 -Mextend"
   set DBG        = "-O0 -g -Mbounds -Mchkptr -traceback -Ktrap=fp -Mextend"
   setenv F_FLAGS   "-Mfixed"
   setenv F90_FLAGS "-Mfree"
   setenv IOAPI_INCL /home/wdx/lib/x86_64/pgi/ioapi_3.1/ioapi/fixed_src  #> I/O API directory path
   setenv IOAPI_LIB  /home/wdx/lib/x86_64/pgi/ioapi_3.1/Linux2_x86_64pg  #> I/O API directory path
   setenv NETCDF     /home/wdx/lib/x86_64/pgi/netcdf                     #> netCDF directory path
  breaksw
 case gcc:
   setenv FC gfortran
   setenv COMPILER GFORT
   set FSTD       = "-O2 -funroll-loops -finit-character=32 -Wconversion-extra -Wtabs -Wsurprising"
   set DBG1       = "-fcheck=all -ffpe-trap=invalid,zero,overflow -fbounds-check"
   set DBG2       = "-fbacktrace -Wno-zerotrip -Wno-unused-function"
   set DBG        = "-Wall -O0 -g $DBG1 $DBG2"
   setenv F_FLAGS   "-ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32"
   setenv F90_FLAGS "-ffree-form -ffree-line-length-none -funroll-loops -finit-character=32"
   setenv IOAPI_INCL /home/wdx/lib/x86_64/gcc/ioapi_3.1/ioapi/fixed_src     #> I/O API directory path
   setenv IOAPI_LIB  /home/wdx/lib/x86_64/gcc/ioapi_3.1/Linux2_x86_64gfort  #> I/O API directory path
   setenv NETCDF     /home/wdx/lib/x86_64/gcc/netcdf                        #> netCDF directory path
   breaksw
 default:
   echo "ERROR: ${compiler} not existing option in run-script"
   echo "If ${compiler} is correct, add case and its information"
   echo "to switch in build script."
   exit()
   breaksw
endsw

#> generate library locations
 setenv system "`uname -m`"
 setenv bld_os "`uname -s``uname -r | cut -d. -f1`"
 setenv lib_basedir $cwd/lib
 setenv CMAQ_LIB    ${lib_basedir}/${system}/${compiler}

 setenv NETCDF_DIR  $CMAQ_LIB/netcdf
 setenv IOAPI_DIR   $CMAQ_LIB/ioapi

 if ( ! -d $CMAQ_LIB ) mkdir -p $CMAQ_LIB
 if ( ! -d $NETCDF_DIR ) ln -sf $NETCDF $NETCDF_DIR
 if ( ! -d $IOAPI_DIR ) then 
    mkdir $IOAPI_DIR
    ln -sf $IOAPI_INCL $IOAPI_DIR/include_files
    ln -sf $IOAPI_LIB  $IOAPI_DIR/lib
 endif

set FP = $FC

#> Set IO/API version
 set IOAPI = ioapi_3.1

#> Set compilers link flags
 set CPP_FLAGS  = ""  #> Fortran Preprocessor Flags
 set LINK_FLAGS = ""  #> Link Flags

#> Set libraries
 setenv netcdf_lib  "-lnetcdf"  #> -lnetcdff -lnetcdf for netCDF v4.2.0 and later
 setenv netcdff_lib "-lnetcdff"  #> -lnetcdff -lnetcdf for netCDF v4.2.0 and later
 setenv ioapi_lib   "-lioapi" 

#============================================================================================
#> Set up the writesite build directory under the Tools directory
#> for checking out and compiling source code
#============================================================================================
 set Bld = ${WORK_DIR}/BLD_create_omi_${VRSN}_${compiler}

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
 echo "lib_1       ioapi/lib;"                                     >> $Cfile
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
 echo "ioapi       $quote$ioapi_lib$quote;"                        >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdf      $quote$netcdf_lib$quote;"                       >> $Cfile
 echo "netcdff     $quote$netcdff_lib$quote;"                      >> $Cfile

 set text = "create_omi"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDriver};"                                       >> $Cfile
 echo                                                              >> $Cfile

#============================================================================================
#> Use BLDMAKE to create the Makefile and model executable if desired
#============================================================================================

 unalias mv rm

#> Recompile BLDMAKE from source if requested or if it does not exist
if ( $?CompileBLDMAKE || ! ( -f $BLDER ) ) then
     cd ${bld_dir_blder}
     make clean
     make
endif

#>Test whether exists 
if(  ! ( -f $BLDER ) ) then
 \ls $BLDER
 exit()
endif 
 
#> Relocate to the BLD_* directory
  cd $Bld 
#> Delete Makefile and if present, Makefile with Compiler-dependent name
  if ( -e Makefile ) \rm -f Makefile 
  if ( -e Makefile.$compiler ) \rm -f Makefile.$compiler 

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

#> Move Makefile to more descriptive name and link back to generic name.
 \mv -f Makefile Makefile.$compiler
 \ln -sf Makefile.$compiler Makefile

#> Check for error during makefile generation
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 else
    echo "build directory: ${Bld} "
 endif

 exit()
