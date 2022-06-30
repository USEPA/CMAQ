#!/bin/csh -f

# ================ CCTMv5.4 Mechanism Build Script ==================== 
# Usage: bldit_mech.csh >&! bldit_mech.log                                   
# Requirements: I/O API & netCDF libraries, a Fortran compiler,               
#               and MPI for multiprocessor computing                     
#
# To report problems or request help with this script/program:           
#             http://www.cmascenter.org
# ======================================================================= 

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

# =======================================================================
#> Begin User Input Section
# =======================================================================

#> Source Code Locations 
 if ( ! $?Mechanism ) then
    # Manually set name of mechanism if not inherited
    setenv Mechanism cb6r5_ae7_aq              
 endif
 setenv MECH $Mechanism

 if ( ! $?ChemSolver ) then
    # Manually set solver approach
    setenv ChemSolver ebi    #> options: ros3, smvgear, ebi

    # Warning: If you are building a new mechanism or modifying an existing
    # one, the ebi solver approach could very possibly encounter convergence
    # or other issues that will lead to inaccuracies. The CMAQ development
    # team recommends using one of the generalized solvers (ros3 or smvgear).
    # If you would like to use ebi anyway, please benchmark your results for
    # important constituents like O3, NO, NO2, OH etc with the generalized
    # solvers and compare your results using ebi to these benchmarks.
 endif

 if ( ! $?MECH_SRC ) then
    # Manually set location of input mechanism files
    setenv MECH_SRC ${CMAQ_HOME}/CCTM/scripts/${Mechanism}
 endif

 if ( ! $?TRAC_NML ) then
    # Manually set location of input tracer files
    setenv TRAC_NML ${CMAQ_HOME}/CCTM/scripts/${Mechanism}/Species_Table_TR_0.nml
 endif

 if ( ! $?MECH_OUT ) then
    # Manually set location of output mechanism files
    setenv MECH_OUT ${CMAQ_HOME}/CCTM/scripts/${Mechanism}
 endif

 if ( ! $?EBI_SOLVER_OUT ) then
    # Manually set location of EBI solver output files. If
    # you are using Ros3 (rosenbrock) or Smvgear, then you 
    # can ignore this option
    setenv EBI_SOLVER_OUT ${CMAQ_HOME}/CCTM/scripts/ebi_${Mechanism}
 endif

 if ( ! $?CLOBBER_MECH ) then
    # Manually set user preference for overwriting existing mechanism
    # files. If CLOBBER_MECH is FALSE, then if files exist, the 
    # program will halt.
    set CLOBBER_MECH = FALSE
 endif



###################### CHEMMECH Processor #############################
#> Build Mechanism Files and instruct build-make to look
#> in the CHEMMECH output folder for the files
 if ( ! -e ${MECH_SRC} ) then
     echo "bldit_mech.csh: $Mechanism input folder cannot be found. "
     echo "    Please select a valid mechanism input location."
     exit 1
 endif
 if ( ! -e ${TRAC_NML} ) then
     echo "bldit_mech.csh: ${TRAC_NML} file cannot be found. "
     echo "    Please select a valid file."
     exit 1
 endif
 mkdir -p ${MECH_OUT}  # Create Output Folder if it Does not Already Exist

 cd ${CMAQ_HOME}/UTIL/chemmech/scripts
 ./bldit_chemmech.csh $compiler
 if ( $? != 0 ) then
   echo "CHEMMECH did not build correctly --> Build Process Halted"
   exit 1
 endif
 
 # Copy files from MECH_SRC to the CHEMMECH input folder
 setenv CHEMMECH_INPUT ${CMAQ_HOME}/UTIL/chemmech/input/${MECH}
 mkdir -p $CHEMMECH_INPUT
 cp -f ${MECH_SRC}/* ${CHEMMECH_INPUT}/
 cp -f ${TRAC_NML}   ${CHEMMECH_INPUT}/
 
 # Run CHEMMECH
 cd ${CMAQ_HOME}/UTIL/chemmech/scripts
 ./run_chemmech.csh
 if ( $? != 0 ) then
   echo "CHEMMECH did not run correctly --> Build Process Halted"
   exit 1
 endif

 # Error out if the RXNS modules weren't created
 set CHEMMECH_OUTPUT = ${CMAQ_HOME}/UTIL/chemmech/output/${MECH}
 if ( ! -e ${CHEMMECH_OUTPUT}/RXNS_DATA_MODULE.F90 \
          ||  ! -e ${CHEMMECH_OUTPUT}/RXNS_FUNC_MODULE.F90 ) then
    echo "Mechanism module not created for ${Mechanism}"
    exit 1
 endif

 if ( ${CLOBBER_MECH} == 'FALSE' ) then
   # Error out if RXNS modules already exist in destination directory
   if ( -e ${MECH_OUT}/RXNS_DATA_MODULE.F90 \
          || -e ${MECH_OUT}/RXNS_FUNC_MODULE.F90 ) then
      echo ""
      echo "Mechanism files already exist in the destination directory."
      echo "If you would like to overwrite them, uncomment clobber_mech"
      echo "if you are using the bldit_cctm script or set CLOBBER_MECH "
      echo "to TRUE if you are using bldit_mech stand-alone."
      echo ""
      exit 1
    endif
 endif
 
 #> Copy Files Back to Mechanism location
 cp -f ${CHEMMECH_OUTPUT}/RXNS*MODULE.F90 ${MECH_OUT}/.
 cp -f ${CHEMMECH_OUTPUT}/[A,E,G,N]*.nml ${MECH_OUT}/.
if ( $?COMPUTE_DELTA_ATOMS ) then
     if( ${COMPUTE_DELTA_ATOMS} == "T" )then
       cp -f ${CHEMMECH_OUTPUT}/mech*.def ${MECH_OUT}/.
     endif
endif

#################### CSQY Photolysis Processor ##########################

#> Copy Inputs to Inline Phot Preprocessor
 set CSQY_INPUT = ${CMAQ_HOME}/UTIL/inline_phot_preproc/input/${MECH}
 mkdir -p ${CSQY_INPUT}
 cp -f ${MECH_OUT}/RXNS_DATA_MODULE.F90 ${CSQY_INPUT}/.

#> Build CSQY Data Table for Inline Photolysis
 cd ${CMAQ_HOME}/UTIL/inline_phot_preproc/scripts
 ./bldrun.inline_phot_preproc.csh $compiler
 if ( $? != 0 ) then
   echo "Preparation of CSQY Table did not build or run correctly --> Build Process Halted"
   exit 1
 endif

 # Error out if the CSQY Data files weren't created
 set CSQY_OUTPUT = ${CMAQ_HOME}/UTIL/inline_phot_preproc/output/${MECH}
 if ( ! -e ${CSQY_OUTPUT}/CSQY_DATA_${MECH} ) then
     echo "CSQY_${MECH} not created"
     exit 1
 endif
 
 if ( ${CLOBBER_MECH} == 'FALSE' ) then
   # Error out if RXNS modules already exist in destination directory
   if ( -e ${MECH_OUT}/CSQY_DATA_${MECH} ) then
      echo ""
      echo "CSQY Data file already exists in the destination directory."
      echo "If you would like to overwrite it, uncomment clobber_mech"
      echo "if you are using the bldit_cctm script or set CLOBBER_MECH "
      echo "to TRUE if you are using bldit_mech stand-alone."
      echo ""
      exit 1
    endif
 endif
 
 #> Copy Files Back to Mechanism Location
 cp -f ${CSQY_OUTPUT}/CSQY_DATA_${MECH} ${MECH_OUT}/.


#################### EBI Solver Processor ##########################

#> if EBI (Euler Backward-Iterative) Chemical Solver is set, build 
#> mechanism-dependent EBI files and instruct build-make to look in 
#> the create-ebi output folder for the files.
 if ( ${ChemSolver} == ebi ) then

    #> Copy Inputs to EBI Input Folder
    set EBI_INPUT = ${CMAQ_HOME}/UTIL/create_ebi/input/${MECH}
    mkdir -p ${EBI_INPUT}
    cp -f ${MECH_OUT}/RXNS_DATA_MODULE.F90 ${EBI_INPUT}/.
    
    # Build and Run Create_EBI
    cd ${CMAQ_HOME}/UTIL/create_ebi/scripts
    ./bldrun_create_ebi.csh $compiler
    if ( $? != 0 ) then
       echo "CREATE_EBI did not build or run correctly --> Build Process Halted"
       exit 1
    endif

    # Error Out if the EBI output files weren't created
    set EBI_OUTPUT = ${CMAQ_HOME}/UTIL/create_ebi/output/ebi_${MECH}
    if ( ! -e ${EBI_OUTPUT}/hrrates.F ) then
       echo "EBI solver not created  for ${Mechanism}"
       exit 1
    endif

    mkdir -p ${EBI_SOLVER_OUT}  # Create EBI Solver output folder

    if ( ${CLOBBER_MECH} == 'FALSE' ) then
      # Error out if RXNS modules already exist in destination directory
      if ( -e ${EBI_SOLVER_OUT}/hrrates.F ) then
         echo ""
         echo "EBI solver files already exist in the destination directory."
         echo "If you would like to overwrite it, uncomment clobber_mech"
         echo "if you are using the bldit_cctm script or set CLOBBER_MECH "
         echo "to TRUE if you are using bldit_mech stand-alone."
         echo ""
         exit 1
       endif
    endif
    
    # Copy EBI files to EBI_SOLVER_OUT directory
    cp -f ${EBI_OUTPUT}/hr*.F ${EBI_SOLVER_OUT}/.
 endif

exit
