#!/bin/csh -f

# ======================= CCTMv5.5.X Build Script ========================= 
# Usage: bldit_cctm.csh <compiler> >&! bldit.cctm.log                          
# Requirements: I/O API & netCDF libraries, a Fortran compiler,               
#               and MPI for multiprocessor computing                     
#
# To report problems or request help with this script/program:           
#             http://www.cmascenter.org
# =========================================================================  

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
 setenv CCTM_SRC ${CMAQ_REPO}/CCTM/src #> location of the CCTM source code
 set GlobInc = $CCTM_SRC/ICL           #> location of the global include files
 set Mechs   = $CCTM_SRC/MECHS         #> location of the chemistry mechanism include files
 setenv REPOROOT $CCTM_SRC

#> Controls for managing the source code and MPI compilation
set CompileBLDMAKE                     #> Recompile the BLDMAKE utility from source
                                       #>   comment out to use an existing BLDMAKE executable
set CopySrc                            #> copy the source files into the build directory
#set CopySrcTree                       #> copy the source files and directory tree into the build directory
#set MakeFileOnly                      #> uncomment to build a Makefile, but do not compile; 
                                       #>   comment out to compile the model (default if not set)
#set build_mech                        #> uncomment to build mechanism source code files using the 
                                       #>   chemmech utility.

#> Below are chemmech options for revising the mechanism definitions file. The option needs information on the
#> composition of each chemistry. See UTIL/chemmech/README.md for more information.
#> Uncomment to use.
#>    Rewrite reactions by appending them with changes in tracked atoms or elements from reactants to products
      #setenv COMPUTE_DELTA_ATOMS F
#>    The species namelist contains the composition information as comments at the end of lines defining species.
#>    Note that if a defining line does not have an ending comment, the species is taken to have none of the tacked atoms.
#>    If NAMELISTS_LIST_ATOMS equals F, an additional ASCII file contains the information.
      #setenv NAMELISTS_LIST_ATOMS T

#set clobber_mech                      #> when commented, the bldit_mech.csh script will halt if 
                                       #>   newly created mechanism files are attempting replace
                                       #>   existing ones. When uncommented, the existing files
                                       #>   will be overwritten.
set ParOpt                             #> uncomment to build a multiple processor (MPI) executable; 
                                       #>   comment out for a single processor (serial) executable
#set DistrEnv                          #> uncomment to distribute environmental variables to multiple machines
                                       #>   comment out for a single processor (serial) executable (MPI only)
#set build_parallel_io                 #> uncomment to build with parallel I/O (pnetcdf); 
                                       #>   comment out to use standard netCDF I/O
#set Debug_CCTM                        #> uncomment to compile CCTM with debug option equal to TRUE
                                       #>   comment out to use standard, optimized compile process
set make_options = "-j"                #> additional options for make command if MakeFileOnly is not set
                                       #>   comment out if no additional options are wanted.

#> Integrated Source Apportionment Method (ISAM)
#set ISAM_CCTM                         #> uncomment to compile CCTM with ISAM activated
                                       #>   comment out to use standard process

#set DDM3D_CCTM                        #> uncomment to compile CCTM with DDM-3D activated
                                       #>   comment out to use standard process
#> Two-way WRF-CMAQ 
#set build_twoway                      #> uncomment to build WRF-CMAQ twoway; 
                                       #>   comment out for off-line chemistry 

#> Working directory and Version IDs
 if ( $?ISAM_CCTM ) then
     set VRSN  = v55_ISAM             #> model configuration ID for CMAQ_ISAM
 else if ( $?DDM3D_CCTM ) then
     set VRSN = v55_DDM3D             #> model configuration ID for CMAQ_DDM
 else
     set VRSN = v55                   #> model configuration ID for CMAQ
 endif
 
 set EXEC  = CCTM_${VRSN}.exe          #> executable name
 set CFG   = CCTM_${VRSN}.cfg          #> configuration file name

 if ( $?build_twoway ) then            # WRF Version used for WRF-CMAQ Model (must be v4.4+)
    set WRF_VRSN = v4.4
 endif   

#========================================================================
#> CCTM Science Modules
#========================================================================
#> NOTE: For the modules with multiple options, a note is 
#>   provided on where to look in the CCTM source code 
#>   archive for a list of the possible settings. Users 
#>   may also refer to the CMAQ documentation.

 set ModGrid   = grid/cartesian             #> grid configuration module 
 
 set DepMod    = m3dry                      #> m3dry or stage
#set DepMod    = stage
 set ModAdv    = wrf_cons                   #> 3-D Advection Scheme [Options: wrf_cons (default), local_cons]
 set ModHdiff  = hdiff/multiscale           #> horizontal diffusion module
 set ModVdiff  = vdiff/acm2_${DepMod}       #> vertical diffusion module (see $CMAQ_MODEL/CCTM/src/vdiff)
 set ModDepv   = depv/${DepMod}             #> deposition velocity calculation module 
                                            #>     (see $CMAQ_MODEL/CCTM/src/depv)
 set ModEmis   = emis/emis                  #> in-line emissions module
 set ModBiog   = biog/beis4                 #> BEIS4 in-line emissions module 

 set ModMegBiog   = biog/megan3                #> MEGAN3 in-line emissions module

 set ModPlmrs  = plrise/smoke               #> in-line emissions plume rise
 set ModCgrds  = spcs/cgrid_spcs_nml        #> chemistry species configuration module 
                                            #>     (see $CMAQ_MODEL/CCTM/src/spcs)
 set ModPhot   = phot/inline                #> photolysis calculation module 
                                            #>     (see $CMAQ_MODEL/CCTM/src/phot)

 setenv Mechanism cb6r5_ae7_aq              #> chemical mechanism (see $CMAQ_MODEL/CCTM/src/MECHS) 
 set ModMech   = MECHS/${Mechanism}

 if ( ${Mechanism} =~ *ae7* ) then          #> ae7 family of aero and cloud chem
     set ModAero   = aero/aero7             # > aerosol chemistry module (see $CMAQ_MODEL/CCTM/src/aero)
     set ModCloud  = cloud/acm_ae7          # > cloud chemistry module (see $CMAQ_MODEL/CCTM/src/cloud)
 else if ( ${Mechanism} =~ *ae6* ) then     #> ae6 family of aero and cloud chem
     set ModAero   = aero/aero6             # > aerosol chemistry module (see $CMAQ_MODEL/CCTM/src/aero)
     set ModCloud  = cloud/acm_ae6          # > cloud chemistry module (see $CMAQ_MODEL/CCTM/src/cloud)
 else if ( ${Mechanism} =~ *cracmm* ) then  #> CRACMM family of aero and cloud chem
     set ModAero    = aero/cracmm           # > aerosol chemistry module (see $CMAQ_MODEL/CCTM/src/aero)
     set ModCloud   = cloud/acm_cracmm      # > cloud chemistry module (see $CMAQ_MODEL/CCTM/src/cloud)
 endif

 # Special cloud modules for kmt versions
 if( ${Mechanism} == cb6r5_ae7_aqkmt2 ) then
     set ModCloud = cloud/acm_ae7_kmt2
 else if( ${Mechanism} == saprc07tic_ae7i_aqkmt2 ) then
     set ModCloud = cloud/acm_ae7_kmt2
 endif

 # Gas chem solver
 if ( ${Mechanism} == cb6r5m_ae7_aq ) then  #> Gas-phase chemistry solver options ($CMAQ_MODEL/CCTM/src/gas)
     setenv ChemSolver ros3                  #> ros3 (or smvgear) are system independent
 else                                      
     setenv ChemSolver ebi                   #> [ default for most mechanisms: ebi ]
 endif
                                         
 if ( $ChemSolver == ebi ) then             
    set ModGas    = gas/${ChemSolver}_${Mechanism}
                                            
 else
    set ModGas    = gas/${ChemSolver}
 endif
    
 set ModUtil   = util/util                  #> CCTM utility modules
 set ModDiag   = diag                       #> CCTM diagnostic modules
 set Tracer    = trac0                      #> tracer configuration directory under 
                                            #>   $CMAQ_MODEL/CCTM/src/MECHS [ default: no tracer species ]
 set ModPa     = procan/pa                  #> CCTM process analysis
 set ModPvO3   = pv_o3                      #> potential vorticity from the free troposphere
 set ModISAM   = isam                       #> CCTM Integrated Source Apportionment Method
 set ModDDM3D  = ddm3d                      #> Decoupled Direct Method in 3D
 set ModDegrade = reactive_tracers          #> Linear Chemical Loss for a fixed set of species treated as reactive tracers

#============================================================================================
#> Computing System Configuration:
#>    Most of these settings are done in config.cmaq
#============================================================================================

 setenv FC ${myFC}                     #> path of Fortan compiler; set in config.cmaq
 set    FP = $FC                       #> path of Fortan preprocessor; set in config.cmaq
 set    CC = ${myCC}                   #> path of C compiler; set in config.cmaq
 setenv BLDER ${CMAQ_HOME}/UTIL/bldmake/bldmake_${compilerString}.exe   #> name of model builder executable

#> Libraries/include files
#set LIOAPI   = "${IOAPI_DIR}/lib ${ioapi_lib}"      #> I/O API library directory
#set IOAPIMOD = "${IOAPI_DIR}/include"               #> I/O API module directory
 set NETCDF   = "${NETCDF_DIR}/lib ${netcdf_lib}"    #> netCDF C library directory
 set NETCDFF  = "${NETCDFF_DIR}/lib ${netcdff_lib}"  #> netCDF Fortran library directory
 set PNETCDF  = "${PNETCDF_DIR}/lib ${pnetcdf_lib}"  #> Parallel netCDF library directory
#set PIO_INC  = "${IOAPI_DIR}/src"

#> Compiler flags set in config.cmaq
 set FSTD       = "${myFSTD}"
 set DBG        = "${myDBG}"
 setenv F_FLAGS   "${myFFLAGS}"            #> F77 flags
 set F90_FLAGS  = "${myFRFLAGS}"           #> F90 flags
 set CPP_FLAGS  = ""                       #> Fortran preprocessor flags
 set C_FLAGS    = "${myCFLAGS} -DFLDMN -I" #> C flags
 set LINK_FLAGS = "${myLINK_FLAG}"         # Link flags


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

 if ($DepMod == m3dry) then
    set cpp_depmod = '-Dm3dry_opt'
 else if ($DepMod == stage) then
    set cpp_depmod = '-Dstage_opt'
    if ( $?DDM3D_CCTM ) then
       echo "*** DDM3D is not compatible with the STAGE deposition model"
       exit 1
    endif
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
    # Distribute Environment to different machines if not done automatically 
    if ( $?DistrEnv ) then
      set PAR = ($PAR -Dcluster) 
    endif
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

#> if DDM-3D is set, add the pre-processor flag for it.
 if ( $?DDM3D_CCTM ) then
    set SENS = ( -Dsens )
 else
    set SENS = ""
 endif
 
#> Build Mechanism Files and instruct build-make to look
#> in the CHEMMECH output folder for the files
 if ( $?build_mech ) then

    # Note: modifying existing or creating new chemical mechanisms
    # can lead to unstable or highly inaccurate representations of 
    # atmospheric chemical predictions when applying the EBI solvers.
    # EBI solvers are highly characterized and tested before 
    # application. The CMAQ development team recommends using the 
    # generalized solvers, Rosenbrock or Gear, with user-defined
    # mechanisms.

    # Because the bldit_cctm script is executing the bldit_mech
    # processor, we will assume that the source location for the new 
    # mechanism files is in the CMAQ repo. There will also be an 
    # error check for overwriting an existing mechanism that can be
    # disabled using the mech_clobber variable above.
    setenv MECH_SRC ${CMAQ_REPO}/CCTM/src/${ModMech}
    setenv TRAC_NML ${CMAQ_REPO}/CCTM/src/MECHS/trac0/Species_Table_TR_0.nml
    setenv MECH_OUT ${CMAQ_REPO}/CCTM/src/${ModMech}
    setenv EBI_SOLVER_OUT ${CMAQ_REPO}/CCTM/src/${ModGas}
    if ( $?clobber_mech ) then
      setenv CLOBBER_MECH TRUE
    else
      setenv CLOBBER_MECH FALSE
    endif

    cd ${CMAQ_HOME}/CCTM/scripts
    ./bldit_mech.csh ${compiler} ${compilerVrsn}
    if ( $? != 0 ) then
      echo ""
      echo "bldit_mech did not finish correctly --> Build Process Halted"
      exit 1
    endif
 endif

#> Tracer configuration files
 set ModTrac = MECHS/$Tracer

#> Set and create the "BLD" directory for checking out and compiling 
#> source code. Move current directory to that build directory.
 if ( $?Debug_CCTM ) then
     set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}_${DepMod}_debug
 else
     set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}_${DepMod}
 endif


 if ( ! -e "$Bld" ) then
    mkdir $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 cd $Bld

#> Set locations for the include files of various modules
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
              -DSUBST_GLOBAL_GATHER=${Popt}_GLOBAL_GATHER\
              -DSUBST_GLOBAL_BCAST=${Popt}_GLOBAL_BCAST\
              -DSUBST_LOOP_INDEX=${Popt}_LOOP_INDEX\
              -DSUBST_SUBGRID_INDEX=${Popt}_SUBGRID_INDEX )
 set STX2 = ( -DSUBST_HI_LO_BND_PE=${Popt}_HI_LO_BND_PE\
              -DSUBST_SUM_CHK=${Popt}_SUM_CHK\
              -DSUBST_SE_INIT=${Popt}_INIT\
              -DSUBST_INIT_ARRAY=${Popt}_INIT_ARRAY\
              -DSUBST_COMM=${Popt}_COMM\
              -DSUBST_MY_REGION=${Popt}_MY_REGION\
              -DSUBST_SLICE=${Popt}_SLICE\
              -DSUBST_GATHER=${Popt}_GATHER\
              -DSUBST_DATA_COPY=${Popt}_DATA_COPY\
              -DSUBST_IN_SYN=${Popt}_IN_SYN )


#> 3-D Advection Options
 if ( $ModAdv == wrf_cons ) then
    set ModCpl    = couple/gencoor_wrf_cons    #> unit conversion and concentration coupling module 
                                               #>     (see $CMAQ_MODEL/CCTM/src/couple)
    set ModHadv   = hadv/ppm                   #> horizontal advection module   
    set ModVadv   = vadv/wrf_cons              #> Vertical advection module                             
 else if ($ModAdv == local_cons) then
    set ModCpl    = couple/gencoor_local_cons  #> unit conversion and concentration coupling module 
                                               #>     (see $CMAQ_MODEL/CCTM/src/couple)
    set ModHadv = hadv/ppm                     #> horizontal advection module
    set ModVadv = vadv/local_cons              #> Vertical advection module
 endif

 # Retrieve git repository sha ID for this source code version
 set shaID   = `git --git-dir=${CMAQ_REPO}/.git rev-parse --short=10 HEAD`
 if ( $? != 0 ) then
    set shaID   = "not_a_repo"
 endif

# ============================================================================
#> Create Config File 
# ============================================================================

set Cfile = ${Bld}/${CFG}.bld      # Config Filename
 set quote = '"'

 echo                                                               > $Cfile
 if ( $?make_options ) then
    echo "make_options $quote$make_options$quote;"                 >> $Cfile
    echo                                                           >> $Cfile
 endif
 echo "model        $EXEC;"                                        >> $Cfile
 echo                                                              >> $Cfile
 echo "repo        $CCTM_SRC;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "sha_ID      $shaID;"                                        >> $Cfile
 echo                                                              >> $Cfile
 echo "mechanism   $Mechanism;"                                    >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_base    $CMAQ_LIB;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_1       ioapi/lib;"                                     >> $Cfile
 echo                                                              >> $Cfile
 echo "lib_2       ioapi/include_files;"                           >> $Cfile
 echo                                                              >> $Cfile
 if ( $?ParOpt ) then
    echo "lib_3       ${quote}mpi -I.$quote;"                      >> $Cfile
    echo                                                           >> $Cfile
 endif
 echo                                                              >> $Cfile
 echo "lib_4       ioapi/lib;"                                     >> $Cfile
 echo                                                              >> $Cfile
 set text = "$quote$CPP_FLAGS $PAR $SENS $PIO $cpp_depmod $STX1 $STX2$quote;"
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
 echo "netcdff     $quote$netcdff_lib$quote;"                      >> $Cfile
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

 set text = "driver"
 echo "// options are" $text                                       >> $Cfile
 echo "Module driver;"                                             >> $Cfile
 echo                                                              >> $Cfile

 set text = "cartesian"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModGrid};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "Init"
 echo "// options are" $text                                       >> $Cfile
 echo "Module init;"                                               >> $Cfile
 echo                                                              >> $Cfile

 set text = "gencoor_wrf_cons and gencoor_local_cons"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModCpl};"                                          >> $Cfile
 echo                                                              >> $Cfile

 set text = "ppm"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModHadv};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "wrf_cons and local_cons"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModVadv};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "multiscale"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModHdiff};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "acm2_m3dry or acm2_stage"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModVdiff};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "m3dry or stage"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDepv};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "emis"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModEmis};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "beis4"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModBiog};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "megan3"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModMegBiog};"                                      >> $Cfile
 echo  

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

 set text = "degrade"
 echo "// reactive_tracer options are" $text                       >> $Cfile
 echo "Module ${ModDegrade};"                                      >> $Cfile
 echo                                                              >> $Cfile

 set text = "gas chemistry solvers"
 echo "// " $text                                                  >> $Cfile
 set text = "smvgear, ros3, and ebi; see 'gas chemistry mechanisms' for <mech>"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModGas};"                                          >> $Cfile
 echo                                                              >> $Cfile

 set MechList = "cb6r5hap_ae7_aq, cb6r3_ae7_aq, cb6r5_ae7_aq, cb6r5_ae7_aqkmt2, cb6r5m_ae7_aq, racm2_ae6_aq, saprc07tc_ae6_aq, saprc07tic_ae7i_aq, saprc07tic_ae7i_aqkmt2"

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

 set text = "use potential vorticity free-troposphere O3 scaling"
 echo "// options are" $text                                    >> $Cfile
 echo "Module ${ModPvO3};"                                      >> $Cfile
 echo                                                           >> $Cfile

 set text = "aero6"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModAero};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "acm_ae6, acm_ae6_kmt, acm_ae7_kmt2, acm_ae6_mp, acm_ae7"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModCloud};"                                        >> $Cfile
 echo                                                              >> $Cfile

 set text = "// compile for inline process analysis"
 echo $text                                                        >> $Cfile
 echo "Module ${ModPa};"                                           >> $Cfile
 echo                                                              >> $Cfile

 set text = "// compile for integrated source apportionment method"
 echo $text                                                        >> $Cfile
 echo "Module ${ModISAM};"                                         >> $Cfile
 echo                                                              >> $Cfile

 if ( $?DDM3D_CCTM ) then
   set text = "// compile for decoupled direct method in 3d"
   echo $text                                                        >> $Cfile
   echo "Module ${ModDDM3D};"                                        >> $Cfile
   echo                                                              >> $Cfile
 endif

 set text = "util"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModUtil};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "diag"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModDiag};"                                         >> $Cfile
 echo                                                              >> $Cfile

 set text = "stm"
 echo "// options are" $text                                       >> $Cfile
 echo "Module stm;"                                                >> $Cfile
 echo                                                              >> $Cfile

 set text = "cio"
 echo "// options are" $text                                       >> $Cfile
 echo "Module cio;"                                                >> $Cfile
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
 set bld_flags = ""
 if ( $?MakeFileOnly ) then   # Do not compile the Model
    set bld_flags = "${bld_flags} -makefo"
 endif

 if ( $?CopySrc ) then
    set bld_flags = "${bld_flags}"
 else if ( $?CopySrcTree ) then   
    set bld_flags = "${bld_flags} -co"
 else 
    set bld_flags = "{bld_flags} -git_local" # Run BLDMAKE with source code in 
                                              # version-controlled git repo
                                              # $Cfile = ${CFG}.bld
 endif

 if ( $?Debug_CCTM ) then
    set bld_flags = "${bld_flags} -debug_cctm"
 endif

 if ( $?ISAM_CCTM ) then
    set bld_flags = "${bld_flags} -isam_cctm"
 endif

 if ( $?build_twoway ) then
   set bld_flags = "${bld_flags} -twoway"
 endif

#> Run BLDMAKE with source code in build directory
 $Blder $bld_flags $Cfile   

#> Rename Makefile to specify compiler option and link back to Makefile
 if ( ! $?build_twoway ) then
    mv Makefile Makefile.$compilerString
    if ( -e Makefile.$compilerString && -e Makefile ) rm Makefile
    ln -s Makefile.$compilerString Makefile
 endif

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

#> If a CRACMM mechanism is used and the compiler is gcc, remove trailing
#>   comments in species namelist files (or else model will not run)
 if ( ${Mechanism} =~ *cracmm* && ${compiler} == gcc ) then
    echo "   >>> removing trailing comments from species namelists <<<"
    sed -i 's/,\!.*/,/' $Bld/GC_${Mechanism}.nml
    sed -i 's/,\!.*/,/' $Bld/AE_${Mechanism}.nml
    sed -i 's/,\!.*/,/' $Bld/NR_${Mechanism}.nml
 endif

#> If Building WRF-CMAQ, download WRF, download auxillary files and build
#> model
 if ( $?build_twoway ) then

#> Check if the user has git installed on their system
  git --version >& /dev/null
  
  if ($? == 0) then
   set git_check
  endif
 
  if ($?git_check) then

    cd $CMAQ_HOME/CCTM/scripts
  
    # Downlad WRF repository from GitHub and put CMAQv5.5 into it
    set WRF_BLD = BLD_WRF${WRF_VRSN}_CCTM_${VRSN}_${compilerString}
    setenv wrf_path ${CMAQ_HOME}/CCTM/scripts/${WRF_BLD}
    setenv WRF_CMAQ 1

    if ( ! -d $WRF_BLD ) then 
      git clone --branch ${WRF_VRSN} https://github.com/wrf-model/WRF.git ./$WRF_BLD >& /dev/null
      cd $wrf_path
      mv $Bld ./cmaq
  
      # Configure WRF
        ./configure <<EOF
        ${WRF_ARCH}
        1
EOF

    else
      # Clean-up 
      rm -r $Bld
      cd $wrf_path
    endif

     # Compile WRF-CMAQ
     ./compile em_real |& tee -a wrf-cmaq_buildlog.log

     cd ${CMAQ_HOME}/CCTM/scripts

   endif

 endif 

exit
