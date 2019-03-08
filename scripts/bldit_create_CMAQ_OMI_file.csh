#! /bin/csh -f

# ==================== Build Script ================================= #
# Usage:  bldit_create_OMI_file.csh COMPILER                          #
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

# if ( $#argv == 1 ) then
    setenv COMPILER $argv[1]
    setenv compiler $COMPILER
else
    echo "usage: $0 <compiler>"
    echo " where <compiler> is intel, pgi or gcc"
    exit(2)
endif

#> Repositories
 setenv REPO       /home/hwo/tools/create_CMAQ_OMI_file
 setenv UTIL_REPO  /home/hwo/CCTM_git_repository/UTIL
 setenv REPOROOT ${REPO}  #> location of the utility's repository

#> Source the config.cmaq file to set the build environment
 source config.cmaq.Sol

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 set VRSN     = v00                #> version
 set EXEC     = ro3_mod_env        #> executable name for this application
 set CFG      = ro3_mod_env.cfg    #> bldmake configuration file name

 set bld_dir_blder = ${UTIL_REPO}/bldmake/new_src #> location of makefile builder executable 
 setenv BLDER        ${bld_dir_blder}/bldmake_WTH #> builder executable  

#> user choice: copy source files
 set CopySrc         #> copy the source files into the BLD directory

 set MakeFileOnly    # builds a Makefile to make the model, but does not compile -
                     # comment out to also compile the model (default if not set)

# set CompileBLDMAKE  #> Recompile the BLDMAKE utility from source
                      #>   comment out to use an existing BLDMAKE executable
 set ModDriver = src  #> source code subdirectory


#============================================================================================
#> Computing System Configuration:
#>    Compiler flags and libraries' path set in config.cmaq
#============================================================================================

#> Set Fortran 90 compiler
switch ( $compiler ) 
 case intel:
   setenv FC ifort
   set FSTD       = "-O2 -traceback"
   set DBG        = "-O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback"
   setenv F_FLAGS   "-fixed -132"
   setenv F90_FLAGS "-free"
   breaksw
 case pgi:
   setenv FC pgf90
   set FSTD       = "-O2 -Mextend"
   set DBG        = "-O0 -g -Mbounds -Mchkptr -traceback -Ktrap=fp -Mextend"
   setenv F_FLAGS   "-Mfixed"
   setenv F90_FLAGS "-Mfree"
   breaksw
 case gcc:
   setenv FC gfortran
   set FSTD       = "-O2 -funroll-loops -finit-character=32 -Wconversion-extra -Wtabs -Wsurprising"
   set DBG1       = "-fcheck=all -ffpe-trap=invalid,zero,overflow -fbounds-check"
   set DBG2       = "-fbacktrace -Wno-zerotrip -Wno-unused-function"
   set DBG        = "-Wall -O0 -g $DBG1 $DBG2"
   setenv F_FLAGS   "-ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32"
   setenv F90_FLAGS "-ffree-form -ffree-line-length-none -funroll-loops -finit-character=32"
   breaksw
 default:
   echo "ERROR: ${compiler} not existing option in run-script"
   echo "If ${compiler} is correct, add case and its information"
   echo "to switch in build script."
   exit()
   breaksw
endsw

set FP = $FC

#> Set IO/API version
 set IOAPI = ioapi_3.1

#> Set compiler flags
 set CPP_FLAGS  = ""  #> Fortran Preprocessor Flags
 set LINK_FLAGS = ""  #> Link Flags

 set LIB2 = "${ioapi_lib}"


#============================================================================================
#> Set up the writesite build directory under the Tools directory
#> for checking out and compiling source code
#============================================================================================
 set Bld = ${REPO}/BLD_create_CMAQ_OMI_file_${VRSN}_${compiler}

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

 set text = "writesite"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDriver};"                                       >> $Cfile
 echo                                                              >> $Cfile

#============================================================================================
#> Use BLDMAKE to create the Makefile and model executable if desired
#============================================================================================

 unalias mv rm

#> Recompile BLDMAKE from source if requested or if it does not exist
if ( $?CompileBLDMAKE || ! -f $BLDER ) then
     cd ${bld_dir_blder}
     make clean
     make
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
 if ( -e Makefile.$compiler ) \rm -f Makefile.$compiler
 \mv -f Makefile Makefile.$compiler
 \ln -sf Makefile.$compiler Makefile

#> Check for error during makefile generation
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif

 exit()
