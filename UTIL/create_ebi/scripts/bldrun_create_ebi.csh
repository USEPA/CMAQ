#! /bin/csh -f

# ============ CREATE_EBI_SOLVERv5.3.x Build Script ================= #
# Usage: bldrun_create_ebi.csh [compiler] >&! bldrun_create_ebi.log   #
# Options for [compiler]: intel | gcc | pgi                           #
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
 cd ../../..
 source ./config_cmaq.csh

#> Source Code Repository
 setenv REPOROOT ${CMAQ_REPO}/UTIL/create_ebi  #> location of the source code for CHEMMECH

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 if ( ! $?MECH ) then
   set MECH =     'cb6r3_ae7_aq'
 endif

 set VRSN =     v532                       #> model version
 setenv EXEC    CREATE_EBI_${VRSN}.exe     #> executable name for this application
 setenv WORKDIR ${CMAQ_HOME}/UTIL/create_ebi
 setenv BLDIR   ${WORKDIR}/scripts/BLD_create_ebi_${VRSN}_${compilerString}
 setenv INPDIR  ${WORKDIR}/input/${MECH}
 setenv OUTDIR  ${WORKDIR}/output/${MECH}

#============================================================================================
#> Set locations for source code and templates
#============================================================================================

 setenv SRCDIR          ${BLDIR}/src_RXNSU
 setenv TMPLDIR         ${BLDIR}/template_RXNSU_OPT
 setenv DEGRADE_CODES   ${BLDIR}/degrade_codes_serial-RXNST

# Define environment variable for path to data module for photochemical mechanism
# RXNS_DATA is the input directory containing the mechanism's data module
# value will change based on user's goals. If the file is not found, this script
# will check the output for CHEMMECH, and then check the CMAQ_REPO. If it is in 
# neither of those places, the script aborts.
 if ( ! -e ${INPDIR} ) then
    mkdir -p ${INPDIR}
 endif
 setenv RXNS_DATA_SRC   ${INPDIR}/RXNS_DATA_MODULE.F90
 if ( ! ( -e ${RXNS_DATA_SRC} ) )then
    echo 'Cannot find RXNS_DATA_MODULE. Look in CHEMMECH Output...'
    if ( ! ( -e ${CMAQ_HOME}/UTIL/chemmech/output/${MECH}/RXNS_DATA_MODULE.F90 ) ) then
       echo 'RXNS_DATA_MODULE not in CHEMMECH Output. Look in CMAQ Repo...'
       if ( ! -e ${CMAQ_REPO}/CCTM/src/MECHS/${MECH}/RXNS_DATA_MODULE.F90 ) then
          echo 'RXNS_DATA_MODULE input file does not exist'
          ls ${RXNS_DATA_SRC}
          exit()
       else
          cp ${CMAQ_REPO}/CCTM/src/MECHS/${MECH}/RXNS_DATA_MODULE.F90 $RXNS_DATA_SRC
       endif
    else
       cp ${CMAQ_HOME}/UTIL/chemmech/output/${MECH}/RXNS_DATA_MODULE.F90 $RXNS_DATA_SRC
    endif
 endif 

#============================================================================================
#> Copy CREATE_EBI Source Code into new build folder and compile
#============================================================================================
 
 if ( ! -e "$BLDIR" ) then
    mkdir -pv $BLDIR
 else
    if ( ! -d "$BLDIR" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 
 cp -r ${CMAQ_REPO}/UTIL/create_ebi/src/* ${BLDIR}
 cd ${BLDIR}; make clean; make 
 if( ! ( -e ${EXEC} ) )then
    echo "failed to compile ${BLDIR}/${EXEC}"
    exit 1
 endif


#============================================================================================
#> Confiugre options for running CREATE_EBI
#============================================================================================

#Set options for the photochemical mechanism
   setenv DEGRADE_SUBS    T    # include calls for HAPs degrade routines 

 
#Set the below compound names within the mechanism
 if ( ${MECH} =~ *"cb"* || ${MECH} =~ *"CB"* ) then
   setenv PAR_NEG_FLAG    T    # True for CB6 but false for SAPRC07t and RACM2 
   setenv SOLVER_DELT     1.25 # maximum time step (minutes) of solver integration up to four 
                               # significant figures in general or scientific notation
                               # For saprc07tic based mechanisms, 1.25 minutes is recommended.

   #                 #Mech   #   Mechanism     # Description
   #                         # cb6r3/cb05      #
    setenv MECH_NO    NO     # NO              # Species name for nitric oxide
    setenv MECH_NO2   NO2    # NO2             # Species name for nitrogen dioxide
    setenv MECH_O3    O3     # O3              # Species name for ozone
    setenv MECH_O3P   O      # O               # Species name for ground state oxygen atom
    setenv MECH_O1D   O1D    # O1D             # Species name for excited state oxygen atom
    setenv MECH_OH    OH     # OH              # Species name for hydroxyl radical
    setenv MECH_HO2   HO2    # HO2             # Species name for hydroperoxy radical
    setenv MECH_HONO  HONO   # HONO            # Species name for nitrous acid
    setenv MECH_HNO4  PNA    # PNA             # Species name for peroxynitric acid
    setenv MECH_PAN   PAN    # PAN             # Species name for peroxy acetyl nitrate
    setenv MECH_C2O3  C2O3   # C2O3            # Species name for peroxy acetyl radical
    setenv MECH_NO3   NO3    # NO3             # Species name for nitrate radical
    setenv MECH_N2O5  N2O5   # N2O5            # Species name for dinitrogen pentoxide

 else if ( ${MECH} =~ *"saprc"* || ${MECH} =~ *"SAPRC"* ) then
   setenv PAR_NEG_FLAG    T    # True for CB6 but false for SAPRC07t and RACM2 
   setenv SOLVER_DELT     1.25 # maximum time step (minutes) of solver integration up to four 
                               # significant figures in general or scientific notation
                               # For saprc07tic based mechanisms, 1.25 minutes is recommended.

   #                 #Mech   #   Mechanism     # Description
   #                  cb6r3  # SAPRC07/RACM2   #
    setenv MECH_NO    NO     #  NO             # Species name for nitric oxide
    setenv MECH_NO2   NO2    #  NO2            # Species name for nitrogen dioxide
    setenv MECH_O3    O3     #  O3             # Species name for ozone
    setenv MECH_O3P   O3P    #  O3P            # Species name for ground state oxygen atom
    setenv MECH_O1D   O1D    #  O1D            # Species name for excited state oxygen atom
    setenv MECH_OH    OH     #  OH / HO        # Species name for hydroxyl radical
    setenv MECH_HO2   HO2    #  HO2            # Species name for hydroperoxy radical
    setenv MECH_HONO  HONO   #  HONO           # Species name for nitrous acid
    setenv MECH_HNO4  HNO4   #  HNO4           # Species name for peroxynitric acid
    setenv MECH_PAN   PAN    #  PAN            # Species name for peroxy acetyl nitrate
    setenv MECH_C2O3  MECO3  #  MECO3 / ACO3   # Species name for peroxy acetyl radical
    setenv MECH_NO3   NO3    #  NO3            # Species name for nitrate radical
    setenv MECH_N2O5  N2O5   #  N2O5           # Species name for dinitrogen pentoxide 

 else if ( ${MECH} =~ *"racm"* || ${MECH} =~ *"RACM"* ) then
   setenv PAR_NEG_FLAG    T    # True for CB6 but false for SAPRC07t and RACM2 
   setenv SOLVER_DELT     2.5  # maximum time step (minutes) of solver integration up to four 
                               # significant figures in general or scientific notation
                               # For saprc07tic based mechanisms, 1.25 minutes is recommended.

   #                 #Mech   #   Mechanism     # Description
   #                  cb6r3  # SAPRC07/RACM2   #
    setenv MECH_NO    NO     #  NO             # Species name for nitric oxide
    setenv MECH_NO2   NO2    #  NO2            # Species name for nitrogen dioxide
    setenv MECH_O3    O3     #  O3             # Species name for ozone
    setenv MECH_O3P   O3P    #  O3P            # Species name for ground state oxygen atom
    setenv MECH_O1D   O1D    #  O1D            # Species name for excited state oxygen atom
    setenv MECH_OH    OH     #  OH / HO        # Species name for hydroxyl radical
    setenv MECH_HO2   HO2    #  HO2            # Species name for hydroperoxy radical
    setenv MECH_HONO  HONO   #  HONO           # Species name for nitrous acid
    setenv MECH_HNO4  HNO4   #  HNO4           # Species name for peroxynitric acid
    setenv MECH_PAN   PAN    #  PAN            # Species name for peroxy acetyl nitrate
    setenv MECH_C2O3  MECO3  #  MECO3 / ACO3   # Species name for peroxy acetyl radical
    setenv MECH_NO3   NO3    #  NO3            # Species name for nitrate radical
    setenv MECH_N2O5  N2O5   #  N2O5           # Species name for dinitrogen pentoxide 
 
 else
   echo 'Undetermined chemical mechanism: the user must include case defining its key species and options.'
   echo 'Please update CREATE_EBI with new case'
   exit 1
 endif

#============================================================================================
#> Populate Output
#============================================================================================

 # Create Output Directory
 if(  -e $OUTDIR  ) then
   echo "Removing old solver files"
   /bin/rm -f ${OUTDIR}/*.[f,F]
 else
   mkdir -p $OUTDIR
   \cp -f ${RXNS_DATA_SRC} $OUTDIR/.
 endif

 # Copy Static Degrade Codes to Output Directory, if necessary
 if( $DEGRADE_SUBS  == "T" )then
     \cp -f ${DEGRADE_CODES}/*.[f,F]  ${OUTDIR}/.
 endif
 
 # Run CREATE_EBI.EXE
 $BLDIR/$EXEC

 if ( $? != 0 ) then
    echo "CREATE_EBI ($BLDIR/$EXEC) failed for some reason. Halt Build Process!"
    exit 1
 endif

