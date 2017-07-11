#!/bin/csh -f

# ======================== CCTMv5.2 Build Script ======================= 
# Usage: bldit.cctm >&! bldit.cctm.log                                   
# Requirements: I/O API & netCDF libraries, a Fortran compiler,               
#               and MPI for multiprocessor computing                     
#
# To report problems or request help with this script/program:           
#             http://www.cmascenter.org
# ====================================================================== 

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
 cd ../..
 source ./config_cmaq.csh

 set echo

# =======================================================================
#> Begin User Input Section
# =======================================================================

#> Source Code Locations
 setenv CCTM_SRC ${CMAQ_REPO}/CCTM/src   #> location of the CCTM source code
 set GlobInc = $CCTM_SRC/ICL           #> location of the global include files
 set Mechs   = $CCTM_SRC/MECHS         #> location of the chemistry mechanism include files
 setenv REPOROOT $CCTM_SRC

#> Working directory and Version IDs
 set VRSN  = v52                       #> model configuration ID
 set EXEC  = CCTM_${VRSN}.exe          #> executable name
 set CFG   = CCTM_${VRSN}.cfg          #> configuration file name

#> Controls for managing the source code and MPI compilation
set CompileBLDMAKE                     #> Recompile the BLDMAKE utility from source
                                       #>   comment out to use an existing BLDMAKE executable
set CopySrc                            #> copy the source files into the build directory
#set CopySrcTree                       #> copy the source files and directory tree into the build directory
#set MakeFileOnly                      #> uncomment to build a Makefile, but do not compile; 
                                       #>   comment out to compile the model (default if not set)
set ParOpt                             #> uncomment to build a multiple processor (MPI) executable; 
                                       #>   comment out for a single processor (serial) executable
#set build_parallel_io                 #> uncomment to build with parallel I/O (pnetcdf); 
                                       #>   comment out to use standard netCDF I/O

#> Two-way WRF-CMAQ 
#set build_twoway                      #> uncomment to build WRF-CMAQ twoway; 
                                       #>   comment out for off-line chemistry 

#> Potential vorticity free-troposphere O3 scaling
#set potvortO3

#========================================================================
#> CCTM Science Modules
#========================================================================
#> NOTE: For the modules with multiple options, a note is 
#>   provided on where to look in the CCTM source code 
#>   archive for a list of the possible settings. Users 
#>   may also refer to the CMAQ documentation.

 set ModDriver = driver/wrf            #> generalized coordinate driver module 
                                       #>     (see $CMAQ_MODEL/CCTM/src/driver)
 set ModInit   = init/yamo             #> time-step initialization module 
 set ModGrid   = grid/cartesian        #> grid configuration module 
 set ModCpl    = couple/gencoor_wrf    #> unit conversion and concentration coupling module 
                                       #>     (see $CMAQ_MODEL/CCTM/src/couple)
 set ModHadv   = hadv/yamo             #> horizontal advection module
 set ModVadv   = vadv/wrf              #> vertical advection module (see $CMAQ_MODEL/CCTM/src/vadv)
 set ModHdiff  = hdiff/multiscale      #> horizontal diffusion module
 set ModVdiff  = vdiff/acm2            #> vertical diffusion module (see $CMAQ_MODEL/CCTM/src/vdiff)
 set ModDepv   = depv/m3dry            #> deposition velocity calculation module 
                                       #>     (see $CMAQ_MODEL/CCTM/src/depv)
 set ModEmis   = emis/emis             #> in-line emissions module
 set ModBiog   = biog/beis3            #> BEIS3 in-line emissions module 
 set ModPlmrs  = plrise/smoke          #> in-line emissions plume rise
 set ModCgrds  = spcs/cgrid_spcs_nml   #> chemistry species configuration module 
                                       #>     (see $CMAQ_MODEL/CCTM/src/spcs)
 set ModPhot   = phot/inline           #> photolysis calculation module 
                                       #>     (see $CMAQ_MODEL/CCTM/src/phot)
 set Mechanism = cb6r3_ae6_aq        #> chemical mechanism (see $CMAQ_MODEL/CCTM/src/MECHS)
 set ModGas    = gas/ebi_${Mechanism}  #> gas-phase chemistry solver (see $CMAQ_MODEL/CCTM/src/gas)
 set ModAero   = aero/aero6            #> aerosol chemistry module (see $CMAQ_MODEL/CCTM/src/aero)
 set ModCloud  = cloud/acm_ae6         #> cloud chemistry module (see $CMAQ_MODEL/CCTM/src/cloud)
 set ModUtil   = util/util             #> CCTM utility modules
 set Tracer    = trac0                 #> tracer configuration directory under 
                                       #>   $CMAQ_MODEL/CCTM/src/MECHS [ default: no tracer species ]
 set ModPa     = procan/pa             #> name of process analysis. Include files are in directory 
                                       #>   $CMAQ_MODEL/CCTM/src/ICL
 set ModPvO3   = pv_o3                 #> potential vorticity from the free troposphee

#============================================================================================
#> Computing System Configuration:
#>    Most of these settings are done in config.cmaq
#============================================================================================

 setenv FC ${myFC}                     #> path of Fortan compiler; set in config.cmaq
 set    FP = $FC                       #> path of Fortan preprocessor; set in config.cmaq
 set    CC = ${myCC}                   #> path of C compiler; set in config.cmaq
 setenv BLDER ${CMAQ_HOME}/UTIL/bldmake/bldmake_${compiler}.exe   #> name of model builder executable

#> Libraries/include files
# set LIOAPI   = "${IOAPI_DIR}/lib ${ioapi_lib}"      #> I/O API library directory
# set IOAPIMOD = "${IOAPI_DIR}/include"               #> I/O API module directory
 set NETCDF   = "${NETCDF_DIR}/lib ${netcdf_lib}"    #> netCDF library directory
 set PNETCDF  = "${PNETCDF_DIR}/lib ${pnetcdf_lib}"  #> Parallel netCDF library directory
# set PIO_INC  = "${IOAPI_DIR}/src"

#> Compiler flags set in config.cmaq
 set FSTD       = "${myFSTD}"
 set DBG        = "${myDBG}"
 setenv F_FLAGS   "${myFFLAGS}"  #> F77 flags
 set F90_FLAGS  = "${myFRFLAGS}" #> F90 flags
 set CPP_FLAGS  = "" #> Fortran preprocessor flags
 set C_FLAGS    = "${myCFLAGS} -DFLDMN -I" #> C flags
 set LINK_FLAGS = "${myLINK_FLAG}" # Link flags


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
    setenv CMAQ_MODEL $CMAQ_REPO
    echo " default Model repository path: $CMAQ_MODEL"
 endif

#> This script was written for Linux hosts only. If
#> the host system is not Linux, produce an error and stop
 set BLD_OS = `uname -s`       
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> If the two-way, coupled WRF-CMAQ model is being built,
#> then just generate the Makefile. Don't compile.
 if ( $?build_twoway ) then
    set MakeFileOnly   
    set ModTwoway = twoway
 endif

#> If parallel-io is selected, then make sure the multiprocessor
#> option is also set.
 if ( $?build_parallel_io ) then
    if ( ! $?ParOpt ) then
       echo "*** ParOpt is not set: required for the build_parallel_io option"
       exit 1
    endif
    set PIO = ( -Dparallel_io )
 else
    set PIO = ""
 endif

#> Set variables needed for multiprocessor and serial builds
 if ( $?ParOpt ) then    
    #Multiprocessor system configuration
    echo "   Parallel; set MPI flags"
    set ModStenex = STENEX/se
    set ModPario = PARIO
    set ModPar = par/mpi
    set PARIO = ${CMAQ_MODEL}/PARIO
    set STENEX = ${CMAQ_MODEL}/STENEX
    # MPI_INC is set in config.cmaq
    # set PIO_INC = "${IOAPI_DIR}/src/fixed_src"
    set PAR = ( -Dparallel )
    set Popt = SE
    set seL = se_snl
    set LIB2 = "${ioapi_lib}"
    set LIB3 = "${mpi_lib} ${extra_lib}"
    set Str1 = (// Parallel / Include message passing definitions)
    set Str2 = (include SUBST_MPI mpif.h;)
 else
    #Serial system configuration
    echo "   Not Parallel; set Serial (no-op) flags"
    set ModStenex = STENEX/noop
    set ModPar = par/par_noop
    set PARIO = "."
    set STENEX = ${CMAQ_MODEL}/STENEX/noop
    set MPI_INC = "."
    # set PIO_INC = "."
    set PAR = ""
    set Popt = NOOP
    set seL = sef90_noop
    set LIB2 = "${ioapi_lib} ${extra_lib}"
    set Str1 =
    set Str2 =
 endif 
 
#> Mechanism location
 set ModMech = MECHS/$Mechanism        #> chemical mechanism module

#> Gas-phase chemistry solver options
 if ( $Mechanism == saprc07tic_ae6i_aqkmti ) then
    set ModGas = gas/ebi_saprc07tic_ae6i_aq
 endif

#> Tracer configuration files
 set ModTrac = MECHS/$Tracer

#> free trop. O3 potential vorticity scaling
 if ( $?potvortO3 ) then 
    set POT = ( -Dpotvorto3 )
 else
    set POT = ""
 endif 

#> Set and create the "BLD" directory for checking out and compiling 
#> source code. Move current directory to that build directory.
 set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compiler}
 if ( ! -e "$Bld" ) then
    mkdir $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 cd $Bld

#> Set locations for the inlude files of various modules
 set ICL_PAR   = $GlobInc/fixed/mpi         
 set ICL_CONST = $GlobInc/fixed/const       
 set ICL_FILES = $GlobInc/fixed/filenames
 set ICL_EMCTL = $GlobInc/fixed/emctrl
 #set ICL_PA    = $GlobInc/procan/$PAOpt

 #Test with xlib commented out
 if ( $?ParOpt ) then
    set ICL_MPI   = .  #$xLib_Base/$xLib_3
 endif


#> If the source code is being copied to the build directory,
#> then move the include files as well and direct the Makefile
#> to the current directory.
 if ( $?CopySrc ) then
    /bin/cp -fp ${ICL_PAR}/*   ${Bld}
    /bin/cp -fp ${ICL_CONST}/* ${Bld}
    /bin/cp -fp ${ICL_FILES}/* ${Bld}
    /bin/cp -fp ${ICL_EMCTL}/* ${Bld}
    #/bin/cp -fp ${ICL_PA}/*    ${Bld}
    if ( $?ParOpt ) then
       /bin/cp -fp ${ICL_MPI}/mpif.h ${Bld}
    endif

    set ICL_PAR   = .
    set ICL_CONST = .
    set ICL_FILES = .
    set ICL_EMCTL = .
    #set ICL_PA    = .
    if ( $?ParOpt ) then
       set ICL_MPI   = .
    endif
 endif


 set STX1 = ( -DSUBST_BARRIER=${Popt}_BARRIER\
              -DSUBST_GLOBAL_MAX=${Popt}_GLOBAL_MAX\
              -DSUBST_GLOBAL_MIN=${Popt}_GLOBAL_MIN\
              -DSUBST_GLOBAL_MIN_DATA=${Popt}_GLOBAL_MIN_DATA\
              -DSUBST_GLOBAL_TO_LOCAL_COORD=${Popt}_GLOBAL_TO_LOCAL_COORD\
              -DSUBST_GLOBAL_SUM=${Popt}_GLOBAL_SUM\
              -DSUBST_GLOBAL_LOGICAL=${Popt}_GLOBAL_LOGICAL\
              -DSUBST_LOOP_INDEX=${Popt}_LOOP_INDEX\
              -DSUBST_SUBGRID_INDEX=${Popt}_SUBGRID_INDEX )
 set STX2 = ( -DSUBST_HI_LO_BND_PE=${Popt}_HI_LO_BND_PE\
              -DSUBST_SUM_CHK=${Popt}_SUM_CHK\
              -DSUBST_INIT_ARRAY=${Popt}_INIT_ARRAY\
              -DSUBST_COMM=${Popt}_COMM\
              -DSUBST_MY_REGION=${Popt}_MY_REGION\
              -DSUBST_SLICE=${Popt}_SLICE\
              -DSUBST_GATHER=${Popt}_GATHER\
              -DSUBST_DATA_COPY=${Popt}_DATA_COPY\
              -DSUBST_IN_SYN=${Popt}_IN_SYN )

# ============================================================================
#> Create Config File 
# ============================================================================

set Cfile = ${Bld}/${CFG}.bld      # Config Filename
 set quote = '"'

 echo                                                               > $Cfile
 echo "model       $EXEC;"                                         >> $Cfile
 echo                                                              >> $Cfile
 echo "repo        $CCTM_SRC;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "mechanism   $Mechanism;"                                    >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_base    $CMAQ_LIB;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_1       ioapi/modules;"                                 >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_2       ioapi/include_files;"                           >> $Cfile
 echo                                                              >> $Cfile
 if ( $?ParOpt ) then
    echo "lib_3       ${quote}mpi -I.$quote;"              >> $Cfile
    echo                                                           >> $Cfile
 endif
 echo                                                              >> $Cfile
 echo "lib_4       ioapi/lib;"                                     >> $Cfile
 echo                                                              >> $Cfile
 set text = "$quote$CPP_FLAGS $PAR $POT $STX1 $STX2$quote;"
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
 echo "c_compiler  $CC;"                                           >> $Cfile
 echo                                                              >> $Cfile
 echo "c_flags     $quote$C_FLAGS$quote;"                          >> $Cfile
 echo                                                              >> $Cfile
 echo "link_flags  $quote$LINK_FLAGS$quote;"                       >> $Cfile
 echo                                                              >> $Cfile
 echo "ioapi       $quote$LIB2$quote;     "                        >> $Cfile
 echo                                                              >> $Cfile
 echo "netcdf      $quote$netcdf_lib$quote;"                       >> $Cfile
 echo                                                              >> $Cfile
 if ( $?ParOpt ) then
    echo "mpich       $quote$LIB3$quote;"                          >> $Cfile
    echo                                                           >> $Cfile
 endif
 echo "include SUBST_PE_COMM    $ICL_PAR/PE_COMM.EXT;"             >> $Cfile
 echo "include SUBST_CONST      $ICL_CONST/CONST.EXT;"             >> $Cfile
 echo "include SUBST_FILES_ID   $ICL_FILES/FILES_CTM.EXT;"         >> $Cfile
 echo "include SUBST_EMISPRM    $ICL_EMCTL/EMISPRM.EXT;"           >> $Cfile
 echo                                                              >> $Cfile

 if ( $?ParOpt ) then
    echo "$Str1"                                                   >> $Cfile
    echo "include SUBST_MPI        ./mpif.h;"                      >> $Cfile
 endif
 echo                                                              >> $Cfile

 set text = "stenex or se_noop"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModStenex};"                                       >> $Cfile
 if ( $?ParOpt ) then
    set text = "// parallel executable; stenex and pario included"
    echo $text                                                     >> $Cfile
    echo "Module ${ModPario};"                                     >> $Cfile
 else
    set text = "serial executable; noop stenex"
    echo $text                                                     >> $Cfile
 endif
 echo                                                              >> $Cfile

 set text = "par, par_nodistr and par_noop"
 echo "// options are" $text                                       >> $Cfile
 if ( $?ParOpt ) then
    echo "Module ${ModPar};"                                       >> $Cfile
 endif
 echo                                                              >> $Cfile

 if ( $?build_twoway ) then
    echo "// option set for WRF-CMAQ twoway"                       >> $Cfile
    echo "Module ${ModTwoway};"                                    >> $Cfile
    echo                                                           >> $Cfile
 endif

 set text = "wrf and yamo"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDriver};"                                       >> $Cfile
 echo                                                              >> $Cfile

 set text = "cartesian"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModGrid};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "yamo"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModInit};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "gencoor_wrf and gencoor"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModCpl};"                                          >> $Cfile
 echo                                                              >> $Cfile

 set text = "yamo"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModHadv};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "wrf and yamo"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModVadv};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "multiscale"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModHdiff};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "acm2"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModVdiff};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "m3dry"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDepv};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "emis"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModEmis};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "beis3"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModBiog};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "smoke"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModPlmrs};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "cgrid_spcs_nml and cgrid_spcs_icl"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModCgrds};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "inline and table"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModPhot};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "gas chemistry solvers"
 echo "// " $text                                                  >> $Cfile
 set text = "smvgear, ros3, and ebi_<mech>; see 'gas chemistry mechanisms' for <mech>"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModGas};"                                          >> $Cfile
 echo                                                              >> $Cfile

 set MechList = " cb05e51_ae6_aq, cb05e51_ae6nvPOA_aq, cb05eh51_ae6_aq, cb05mp51_ae6_aq, cb05tucl_ae6_aq, cb05tump_ae6_aq, cb6r3_ae6_aq, cb6r3_ae6nvPOA_aq, racm2_ae6_aq, saprc07tb_ae6_aq, saprc07tc_ae6_aq, saprc07tc_ae6nvPOA_aq, saprc07tic_ae6i_aq, saprc07tic_ae6i_aqkmti, saprc07tic_ae6invPOA_aq"

 set text = "gas chemistry mechanisms"
 echo "// " $text                                                  >> $Cfile
 set text = "$MechList"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModMech};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "tracer modules"
 echo "// " $text                                                  >> $Cfile
 echo "// options are trac0, trac1"                                >> $Cfile
 echo "Module ${ModTrac};"                                         >> $Cfile
 echo 

 if ( $?potvortO3 ) then
    set text = "use potential vorticity free-troposphere O3 scaling"
    echo "// options are" $text                                    >> $Cfile
    echo "Module ${ModPvO3};"                                      >> $Cfile
    echo                                                           >> $Cfile
 endif

 set text = "aero6"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModAero};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "acm_ae6, acm_ae6_kmt, and acm_ae6_mp"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModCloud};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "// compile for inline process analysis"
 echo $text                                                        >> $Cfile
 echo "Module ${ModPa};"                                           >> $Cfile
 echo                                                              >> $Cfile

 set text = "util"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModUtil};"                                         >> $Cfile
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

#> Relocate to the BLD_* directory 
 cd $Bld

#> Set multiprocessor/serial options for BLDMAKE execution
 if ( $?ParOpt ) then
    set Blder = "$BLDER -verbose"
 else
    set Blder = "$BLDER -serial -verbose"
 endif

#> Run BLDMAKE Utility
 if ( $?MakeFileOnly ) then   # Do not compile the Model
    if ( $?CopySrc ) then
       $Blder -makefo $Cfile  # Run BLDMAKE with source code in build directory
    else if ( $?CopySrcTree ) then 
       $Blder -makefo -co $Cfile  # Copy repository directory tree as well
    else
       $Blder -makefo -git_local $Cfile   # Run BLDMAKE with source code in 
                                          # version-controlled git repo
                                          # $Cfile = ${CFG}.bld
    endif
 else  # Also compile the model
    if ( $?CopySrc ) then
       $Blder $Cfile   # Run BLDMAKE with source code in build directory
    else if ( $?CopySrcTree ) then 
       $Blder -makefo -co $Cfile  # Copy repository directory tree as well
    else
       $Blder -git_local $Cfile    # Run BLDMAKE with source code in 
                                   # version-controlled git repo
                                   # $Cfile = ${CFG}.bld
    endif
 endif

#> Rename Makefile to specify compiler option and link back to Makefile
 mv Makefile Makefile.$compiler
 if ( -e Makefile.$compiler && -e Makefile ) rm Makefile
 ln -s Makefile.$compiler Makefile

#> Alert user of error in BLDMAKE if it ocurred
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif

#> Preserve old Config file, if it exists, before moving new one to 
#> build directory.
 if ( -e "$Bld/${CFG}" ) then
    echo "   >>> previous ${CFG} exists, re-naming to ${CFG}.old <<<"
    mv $Bld/${CFG} $Bld/${CFG}.old
 endif
 mv ${CFG}.bld $Bld/${CFG}



exit
