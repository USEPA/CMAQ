#! /bin/csh -f

# ============ CREATE_EBI_SOLVERv5.5.x Build Script ================= #
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
   setenv compiler intel
   setenv compilerVrsn Empty
   echo "compiler and version not set"
   echo "usage: $0 <compiler>"
   echo "setting compiler to intel"
 endif

#> Source the config.cmaq file to set the build environment
 if( -e ../../../config_cmaq.csh )then
    cd ../../..
    source ./config_cmaq.csh
 else
#work offline from CMAQ repository and build environment
    setenv offline "Y"
    echo ${offline}
    setenv compilerString ${compiler}
    setenv CMAQ_HOME $cwd/..
 endif
 echo ${CMAQ_HOME}

#> Source Code Repository
 if( ! ( $?offline ) )then
   setenv REPOROOT ${CMAQ_REPO}/UTIL/create_ebi  #> location of the source code for CHEMMECH
 else
   setenv REPOROOT ${CMAQ_HOME}
 endif

#===============================================================================
#> Begin User Input Section 
#===============================================================================

#> User choices: working directory and application ID
 if ( ! $?MECH ) then
   set MECH =     'cb6r5m_ae7_aq'
 endif
 setenv CLEAR "TRUE" #> over-write existing output files

#> Set working, input and output directories
 if( ! ( $?offline ) )then
   set WORKDIR = ${CMAQ_HOME}/UTIL/create_ebi
 else
   set WORKDIR = ${CMAQ_HOME}
 endif
 if ( ! $?INPDIR ) then
   setenv INPDIR  ${WORKDIR}/input/${MECH}
 endif
 if ( ! $?OUTDIR ) then
    setenv OUTDIR  ${WORKDIR}/output/ebi_${MECH}
 endif 

 set VRSN =     v55                       #> model version
 setenv EXEC    CREATE_EBI_${VRSN}.exe     #> executable name for this application
 setenv BLDIR   ${WORKDIR}/scripts/BLD_create_ebi_${VRSN}_${compilerString}

#============================================================================================
#> Set locations for source code and templates
#============================================================================================

 setenv SRCDIR          ${BLDIR}
 setenv TMPLDIR         ${REPOROOT}/template_RXNSU_OPT
 set data_paths = ( ${TMPLDIR}  )
 foreach data_dir ( ${data_paths} )
    if( ! ( -e ${data_dir} ) )cp -r $data_dir ${WORKDIR}/.
 end

# Define environment variable for path to data module for photochemical mechanism
# RXNS_DATA is the input directory containing the mechanism's data module
# value will change based on user's goals. If the file is not found, this script
# will check the output for CHEMMECH, and then check the CMAQ_REPO. If it is in 
# neither of those places, the script aborts.
 setenv RXNS_DATA_SRC   ${INPDIR}/RXNS_DATA_MODULE.F90

 if ( ! -e ${RXNS_DATA_SRC} ) then
     echo 'Below RXNS_DATA_MODULE input file does not exist'
     echo ${RXNS_DATA_SRC}
     exit 1
 endif
#============================================================================================
#> Copy CREATE_EBI Source Code into new build folder and compile
#============================================================================================
 
 if ( -e "$BLDIR" ) then
    echo "   *** build directory exist, deleting it***"
    \rm -rf $BLDIR
 endif
 mkdir -pv $BLDIR
 
 cp ${REPOROOT}/src/* ${BLDIR}/.
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
   if ( ${MECH} =~ *"cb6r3m"* || ${MECH} =~ *"CB6R3M"* ) then
      setenv SOLVER_DELT     1.25 # maximum time step (minutes) of solver integration up to four 
                                  # significant figures in general or scientific notation
                                  # For cb6r3m recommended.
   else if ( ${MECH} =~ *"cb6r5m"* || ${MECH} =~ *"CB6R5M"* ) then
      setenv SOLVER_DELT     1.25 # maximum time step (minutes) of solver integration up to four 
                                  # significant figures in general or scientific notation
                                  # For cb6r5m recommended.
   else
      setenv SOLVER_DELT     2.50 # maximum time step (minutes) of solver integration up to four
                                  # significant figures in general or scientific notation
                                  # For saprc07tic based mechanisms, 1.25 minutes is recommended.
   endif

   #                 #Mech   # Description
   #              cb6r3/cb05 #
    setenv MECH_NO    NO     # Species name for nitric oxide
    setenv MECH_NO2   NO2    # Species name for nitrogen dioxide
    setenv MECH_O3    O3     # Species name for ozone
    setenv MECH_O3P   O      # Species name for ground state oxygen atom
    setenv MECH_O1D   O1D    # Species name for excited state oxygen atom
    setenv MECH_OH    OH     # Species name for hydroxyl radical
    setenv MECH_HO2   HO2    # Species name for hydroperoxy radical
    setenv MECH_HONO  HONO   # Species name for nitrous acid
    setenv MECH_HNO4  PNA    # Species name for peroxynitric acid
    setenv MECH_PAN   PAN    # Species name for peroxy acetyl nitrate
    setenv MECH_C2O3  C2O3   # Species name for peroxy acetyl radical
    setenv MECH_NO3   NO3    # Species name for nitrate radical
    setenv MECH_N2O5  N2O5   # Species name for dinitrogen pentoxide

 else if ( ${MECH} =~ *"saprc"* || ${MECH} =~ *"SAPRC"* ) then
   setenv PAR_NEG_FLAG    F    # True for CB6 but false for SAPRC07t and RACM2 
   if ( ${MECH} =~ *"saprc07tic"* || ${MECH} =~ *"SAPRC07TIC"* ) then
      setenv SOLVER_DELT     1.25 # maximum time step (minutes) of solver integration up to four 
                                  # significant figures in general or scientific notation
                                  # For saprc07tic based mechanisms, 1.25 minutes is recommended.
   else
      setenv SOLVER_DELT     2.50 # maximum time step (minutes) of solver integration up to four
                                  # significant figures in general or scientific notation
                                  # For saprc07tic based mechanisms, 1.25 minutes is recommended.
                                  # endif
   endif

   #                 #Mech   # Description
   #                SAPRC07  #
    setenv MECH_NO    NO     # Species name for nitric oxide
    setenv MECH_NO2   NO2    # Species name for nitrogen dioxide
    setenv MECH_O3    O3     # Species name for ozone
    setenv MECH_O3P   O3P    # Species name for ground state oxygen atom
    setenv MECH_O1D   O1D    # Species name for excited state oxygen atom
    setenv MECH_OH    OH     # Species name for hydroxyl radical
    setenv MECH_HO2   HO2    # Species name for hydroperoxy radical
    setenv MECH_HONO  HONO   # Species name for nitrous acid
    setenv MECH_HNO4  HNO4   # Species name for peroxynitric acid
    setenv MECH_PAN   PAN    # Species name for peroxy acetyl nitrate
    setenv MECH_C2O3  MECO3  # Species name for peroxy acetyl radical
    setenv MECH_NO3   NO3    # Species name for nitrate radical
    setenv MECH_N2O5  N2O5   # Species name for dinitrogen pentoxide 

 else if ( ${MECH} =~ *"racm"* || ${MECH} =~ *"RACM"* ) then
   setenv PAR_NEG_FLAG    F    # True for CB6 but false for SAPRC07t and RACM2 
   setenv SOLVER_DELT     2.50 # maximum time step (minutes) of solver integration up to four 
                               # significant figures in general or scientific notation
                               # For cb6r3m and saprc07tic based mechanisms, 1.25 minutes is recommended.

   #                 #Mech   # Description
   #                 RACM2   #
    setenv MECH_NO    NO     # Species name for nitric oxide
    setenv MECH_NO2   NO2    # Species name for nitrogen dioxide
    setenv MECH_O3    O3     # Species name for ozone
    setenv MECH_O3P   O3P    # Species name for ground state oxygen atom
    setenv MECH_O1D   O1D    # Species name for excited state oxygen atom
    setenv MECH_OH    HO     # Species name for hydroxyl radical
    setenv MECH_HO2   HO2    # Species name for hydroperoxy radical
    setenv MECH_HONO  HONO   # Species name for nitrous acid
    setenv MECH_HNO4  HNO4   # Species name for peroxynitric acid
    setenv MECH_PAN   PAN    # Species name for peroxy acetyl nitrate
    setenv MECH_C2O3  ACO3   # Species name for peroxy acetyl radical
    setenv MECH_NO3   NO3    # Species name for nitrate radical
    setenv MECH_N2O5  N2O5   # Species name for dinitrogen pentoxide 
 
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
   if( $CLEAR == "FALSE" )then
      echo "Previous output exists; set CLEAR to TRUE to delete"
      exit(1)
   endif
   echo "Removing old solver files"
   /bin/rm -rf ${OUTDIR}
 endif
 mkdir -p $OUTDIR
 \cp -f ${RXNS_DATA_SRC} $OUTDIR/.

 # Run CREATE_EBI.EXE
 $BLDIR/$EXEC

 if ( $? != 0 ) then
    echo "CREATE_EBI ($BLDIR/$EXEC) failed for some reason. Halt Build Process!"
    exit 1
 endif

