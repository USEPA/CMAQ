#!/bin/csh -f

# ================= CMAQv5.5.X Configuration Script ================= #
# Requirements: I/O API & netCDF libraries                            #
#               PGI, Intel, or Gnu Fortran compiler                   #
#               MPICH for multiprocessor computing                    #
# Optional:     Git for GitHub source code repository                 #
#                                                                     #
# Note that this script was configured/tested on Red Hat Linux O/S    #
#                                                                     #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org/help-desk.cfm                 #
# =================================================================== #

#> Critical Folder Locations
 # CMAQ_HOME - this is where the config_cmaq.csh script is located. It
 # is also the root directory for all the executables. It may include 
 # the repository if the user is building CMAQ inside the repository. It
 # may, on the other hand, be outside the repository if the user has 
 # created a separate project directory where they wish to put build-
 # and run-scripts as well as executables.
 setenv CMAQ_HOME $cwd

 # CMAQ_REPO - this is always the location of the CMAQ repository that
 # the user will pull from to create executables. If the user is building
 # CMAQ inside the repository then it will be equal to CMAQ_HOME. If not,
 # the user must supply an alternative folder locaiton.
 setenv CMAQ_REPO $CMAQ_HOME

 # CMAQ_DATA - this may be where the input data are located. It may be a 
 # symbolic link to another location on the system, but it should be
 # provided here 
 setenv CMAQ_DATA $CMAQ_HOME/data
 if ( ! -d $CMAQ_DATA ) mkdir -p $CMAQ_DATA

 cd $CMAQ_HOME

#===============================================================================
#> architecture & compiler specific settings
#===============================================================================

#> Set the compiler option
 if ( $#argv == 1 ) then
    #> Use the user's input to set the compiler parameter
    setenv compiler $1
    setenv compilerVrsn Empty
 else if ( $#argv == 2 ) then
    #> Compiler Name and Version have been provided
    setenv compiler $1
    setenv compilerVrsn $2
 else if ( $#argv == 0 ) then
    #> If config.cmaq is called from Bldit.cctm or run.cctm, then this 
    #> variable is already defined
    if ( ! $?compiler ) then
      echo "Error: 'compiler' should be set either in the"
      echo "       environment or as input to config.cmaq"
      echo "       Example:> ./config.cmaq [compiler]"
      echo "       Options: intel | gcc | pgi"
      exit
    else if ( ! $?compilerVrsn ) then
      setenv compilerVrsn Empty
    endif
 else
    #> More than two inputs were given. Exit this script just to
    #> be on the safe side.
    echo "Error: Too many inputs to config.cmaq. This script"
    echo "       is expecting one input (the name of the"
    echo "       desired compiler. In some installations, you "
    echo "       may also be able to specify the compiler version "
    echo "       as the second input, but this is not by default."
    exit
 endif
 echo "Compiler is set to $compiler"


#> Compiler flags and settings
 switch ( $compiler )

#>  Intel fortran compiler......................................................
    case intel:

        #> I/O API, netCDF Library Locations -- used in WRF-CMAQ
        setenv NETCDF netcdf_root_intel # Note please combine netCDF-C & Fortran Libraries 
        setenv IOAPI  ioapi_root_intel  
        setenv WRF_ARCH # [1-75]  
    
        #> I/O API, netCDF, and MPI Library Locations -- used in CMAQ
        setenv IOAPI_INCL_DIR   ioapi_inc_intel             #> I/O API include header files
        setenv IOAPI_LIB_DIR    ioapi_lib_intel             #> I/O API libraries
        setenv NETCDF_LIB_DIR   netcdf_lib_intel            #> netCDF C directory path
        setenv NETCDF_INCL_DIR  netcdf_inc_intel            #> netCDF C directory path
        setenv NETCDFF_LIB_DIR  netcdff_lib_intel           #> netCDF Fortran directory path
        setenv NETCDFF_INCL_DIR netcdff_inc_intel           #> netCDF Fortran directory path
        setenv MPI_INCL_DIR     mpi_incl_intel              #> MPI Include directory path
        setenv MPI_LIB_DIR      mpi_lib_intel               #> MPI Lib directory path
    
        #> Compiler Aliases and Flags
        #> set the compiler flag -qopt-report=5 to get a model optimization report in the build directory with the optrpt extension
        setenv myFC mpiifort
        setenv myCC icc       
        setenv myFSTD "-O3 -fno-alias -mp1 -fp-model source -ftz -simd -align all -xHost -vec-guard-write -unroll-aggressive"
        setenv myDBG  "-O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback"
        setenv myLINK_FLAG #"-qopenmp-simd" openMP not supported w/ CMAQ
        setenv myFFLAGS "-fixed -132"
        setenv myFRFLAGS "-free"
        setenv myCFLAGS "-O2"
        setenv extra_lib ""
    
        breaksw
    
#>  Portland Group fortran compiler.............................................
    case pgi:

        #> I/O API, netCDF Library Locations -- used in WRF-CMAQ
        setenv NETCDF netcdf_root_pgi # Note please combine netCDF-C & Fortran Libraries 
        setenv IOAPI  ioapi_root_pgi  
        setenv WRF_ARCH # [1-75]  
 
        #> I/O API, netCDF, and MPI Library Locations -- used in CMAQ
        setenv IOAPI_INCL_DIR   iopai_inc_pgi             #> I/O API include header files
        setenv IOAPI_LIB_DIR    ioapi_lib_pgi             #> I/O API libraries
        setenv NETCDF_LIB_DIR   netcdf_lib_pgi            #> netCDF C directory path
        setenv NETCDF_INCL_DIR  netcdf_inc_pgi            #> netCDF C directory path
        setenv NETCDFF_LIB_DIR  netcdff_lib_pgi           #> netCDF Fortran directory path
        setenv NETCDFF_INCL_DIR netcdff_inc_pgi           #> netCDF Fortran directory path
        setenv MPI_INCL_DIR     mpi_incl_pgi              #> MPI Include directory path
        setenv MPI_LIB_DIR      mpi_lib_pgi               #> MPI Lib directory path
 
        #> Compiler Aliases and Flags
        setenv myFC mpifort 
        setenv myCC pgcc
        setenv myLINK_FLAG # "-mp" openMP not supported w/ CMAQ
        setenv myFSTD "-O3"
        setenv myDBG  "-O0 -g -Mbounds -Mchkptr -traceback -Ktrap=fp"
        setenv myFFLAGS "-Mfixed -Mextend -mcmodel=medium -tp px"
        setenv myFRFLAGS "-Mfree -Mextend -mcmodel=medium -tp px"
        setenv myCFLAGS "-O2"
        setenv extra_lib ""
    
        breaksw
    
#>  gfortran compiler............................................................
    case gcc:
 
        #> I/O API, netCDF Library Locations -- used in WRF-CMAQ
        setenv NETCDF netcdf_root_gcc # Note please combine netCDF-C & Fortran Libraries 
        setenv IOAPI  ioapi_root_gcc  
        setenv WRF_ARCH # [1-75]   
  
        #> I/O API, netCDF, and MPI Library Locations -- used in CMAQ
        setenv IOAPI_INCL_DIR   ioapi_inc_gcc             #> I/O API include header files
        setenv IOAPI_LIB_DIR    ioapi_lib_gcc             #> I/O API libraries
        setenv NETCDF_LIB_DIR   netcdf_lib_gcc            #> netCDF C directory path
        setenv NETCDF_INCL_DIR  netcdf_inc_gcc            #> netCDF C directory path
        setenv NETCDFF_LIB_DIR  netcdff_lib_gcc           #> netCDF Fortran directory path
        setenv NETCDFF_INCL_DIR netcdff_inc_gcc           #> netCDF Fortran directory path
        setenv MPI_INCL_DIR     mpi_incl_gcc              #> MPI Include directory path
        setenv MPI_LIB_DIR      mpi_lib_gcc               #> MPI Lib directory path

        #> Compiler Aliases and Flags
        #> set the compiler flag -fopt-info-missed to generate a missed optimization report in the bldit logfile
        setenv myFC mpifort
        setenv myCC gcc
        setenv myFSTD "-O3 -funroll-loops -finit-character=32 -Wtabs -Wsurprising -ftree-vectorize -ftree-loop-if-convert -finline-limit=512"
        setenv myDBG  "-Wall -O0 -g -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace"
        setenv myFFLAGS "-ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32 -std=legacy"
        setenv myFRFLAGS "-ffree-form -ffree-line-length-none -funroll-loops -finit-character=32"
        setenv myCFLAGS "-O2"
        setenv myLINK_FLAG # "-fopenmp" openMP not supported w/ CMAQ
        setenv extra_lib ""
    
        breaksw

    default:
        echo "*** Compiler $compiler not found"
        exit(2)
        breaksw

 endsw
 
#> Apply Specific Module and Library Location Settings for those working inside EPA
 # source /work/MOD3DEV/cmaq_common/cmaq_env.csh  #>>> UNCOMMENT if at EPA

#> Add the Compiler Version Number to the Compiler String if it's not empty
 setenv compilerString ${compiler}
 if ( $compilerVrsn != "Empty" ) then
    setenv compilerString ${compiler}${compilerVrsn}
 endif

#===============================================================================
 
#> I/O API, netCDF, and MPI libraries
 setenv netcdf_lib "-lnetcdf"  #> -lnetcdff -lnetcdf for netCDF v4.2.0 and later
 setenv netcdff_lib "-lnetcdff"
 setenv ioapi_lib "-lioapi"
 setenv pnetcdf_lib "-lpnetcdf"
 setenv mpi_lib "-lmpi" #> -lmpich or -lmvapich 

#> Query System Info and Current Working Directory
 setenv system "`uname -m`"
 setenv bld_os "`uname -s``uname -r | cut -d. -f1`"
 setenv lib_basedir $CMAQ_HOME/lib

#> Generate Library Locations
 setenv CMAQ_LIB    ${lib_basedir}/${system}/${compilerString}
 setenv MPI_DIR     $CMAQ_LIB/mpi
 setenv NETCDF_DIR  $CMAQ_LIB/netcdf
 setenv NETCDFF_DIR $CMAQ_LIB/netcdff
 setenv PNETCDF_DIR $CMAQ_LIB/pnetcdf
 setenv IOAPI_DIR   $CMAQ_LIB/ioapi

#> Create Symbolic Links to Libraries
 if ( ! -d $CMAQ_LIB ) mkdir -p $CMAQ_LIB
 if (   -e $MPI_DIR  ) rm -rf $MPI_DIR
     mkdir $MPI_DIR
     ln -s $MPI_LIB_DIR $MPI_DIR/lib
     ln -s $MPI_INCL_DIR $MPI_DIR/include
 if ( ! -d $NETCDF_DIR )  mkdir $NETCDF_DIR
 if ( ! -e $NETCDF_DIR/lib ) ln -sfn $NETCDF_LIB_DIR $NETCDF_DIR/lib
 if ( ! -e $NETCDF_DIR/include ) ln -sfn $NETCDF_INCL_DIR $NETCDF_DIR/include
 if ( ! -d $NETCDFF_DIR )  mkdir $NETCDFF_DIR
 if ( ! -e $NETCDFF_DIR/lib ) ln -sfn $NETCDFF_LIB_DIR $NETCDFF_DIR/lib
 if ( ! -e $NETCDFF_DIR/include ) ln -sfn $NETCDFF_INCL_DIR $NETCDFF_DIR/include
 if ( ! -d $IOAPI_DIR ) then
    mkdir $IOAPI_DIR
    ln -sfn $IOAPI_INCL_DIR $IOAPI_DIR/include_files
    ln -sfn $IOAPI_LIB_DIR  $IOAPI_DIR/lib
 endif

#> Check for netcdf and I/O API libs/includes, error if they don't exist
 if ( ! -e $NETCDF_DIR/lib/libnetcdf.a ) then 
    echo "ERROR: $NETCDF_DIR/lib/libnetcdf.a does not exist in your CMAQ_LIB directory!!! Check your installation before proceeding with CMAQ build."
    exit
 endif
if ( ! -e $NETCDFF_DIR/lib/libnetcdff.a ) then
    echo "ERROR: $NETCDFF_DIR/lib/libnetcdff.a does not exist in your CMAQ_LIB directory!!! Check your installation before proceeding with CMAQ build."
    exit
 endif
 if ( ! -e $IOAPI_DIR/lib/libioapi.a ) then 
    echo "ERROR: $IOAPI_DIR/lib/libioapi.a does not exist in your CMAQ_LIB directory!!! Check your installation before proceeding with CMAQ build."
    exit
 endif
 if ( ! -e $IOAPI_DIR/lib/m3utilio.mod ) then 
    echo "ERROR: $IOAPI_MOD_DIR/m3utilio.mod does not exist in your CMAQ_LIB directory!!! Check your installation before proceeding with CMAQ build."
    exit
 endif

#> Set executable id
 setenv EXEC_ID ${bld_os}_${system}${compilerString}
