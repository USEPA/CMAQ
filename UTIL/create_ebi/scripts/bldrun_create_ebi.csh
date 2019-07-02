#! /bin/csh -f
# C-shell script to run CR_EBI_SOLVER 

 date; set timestamp; set echo 
# current working directory assumed ${CMAQ_REPO}/UTIL/create_ebi;change if otherwise
 set BASE            = $cwd          
 set EXDIR           = ${BASE}/BLD  
 set EXEC            = cr_ebi_solver 

#> option to set compiler and build a new executable (not required)
 setenv COMPILER  INTEL  #> INTEL, PGF90, or GFORT

#Define environment variable for path to data module for photochemical mechanism
# RXNS_DATA is the input directory containing the mechanism's data module
# value will change based on user's goals
set MECH             = 'cb6r3_ae7_aq'
set RXNS_DATA        = ${BASE}/../../CCTM/src/MECHS/${MECH}
setenv RXNS_DATA_SRC   ${RXNS_DATA}/RXNS_DATA_MODULE.F90

if ( ! ( -e ${RXNS_DATA_SRC} ) )then
       \ls ${RXNS_DATA_SRC}
       exit()
endif 

# Define templates and scource code  directories
 setenv TMPLDIR         ${BASE}/template_RXNSU_OPT
 setenv DEGRADE_CODES   ${BASE}/degrade_codes_serial-RXNST
 setenv SRCDIR          ${BASE}/src_RXNSU

# Define output directory;value will change based on user's goals
 set day = ` date "+%b-%d-%Y" `
 setenv OUTDIR  ${BASE}/output/ebi_${MECH}-${day}-${COMPILER}
 
#Set options about the photochemical mechanism
 setenv PAR_NEG_FLAG    T    # True for CB6 but false for SAPRC07t and RACM2 
 setenv DEGRADE_SUBS    T    # include calls for HAPs degrade routines (true cb6 mechanisms)  
 setenv SOLVER_DELT     2.5  # maximum time step (minutes) of solver integration up to four 
                             # significant figures in general or scientific notation
                             # For saprc07tic based mechanisms, 1.25 minutes is recommended.

#Set the below compound names within the mechanism
#                 #Mech   #   Mechanism          #   Mechanism     # Description
#                  cb6r3  # SAPRC07/RACM2        # cb6r3/cb05      #
 setenv MECH_NO    NO     #  NO                  # NO              # Species name for nitric oxide
 setenv MECH_NO2   NO2    #  NO2                 # NO2             # Species name for nitrogen dioxide
 setenv MECH_O3    O3     #  O3                  # O3              # Species name for ozone
 setenv MECH_O3P   O      #  O3P                 # O               # Species name for ground state oxygen atom
 setenv MECH_O1D   O1D    #  O1D                 # O1D             # Species name for excited state oxygen atom
 setenv MECH_OH    OH     #  OH / HO             # OH              # Species name for hydroxyl radical
 setenv MECH_HO2   HO2    #  HO2                 # HO2             # Species name for hydroperoxy radical
 setenv MECH_HONO  HONO   #  HONO                # HONO            # Species name for nitrous acid
 setenv MECH_HNO4  PNA    #  HNO4                # PNA             # Species name for peroxynitric acid
 setenv MECH_PAN   PAN    #  PAN                 # PAN             # Species name for peroxy acetyl nitrate
 setenv MECH_C2O3  C2O3   #  MECO3 / ACO3        # C2O3            # Species name for peroxy acetyl radical
 setenv MECH_NO3   NO3    #  NO3                 # NO3             # Species name for nitrate radical
 setenv MECH_N2O5  N2O5   #  N2O5                # N2O5            # Species name for dinitrogen pentoxide
 
 rm cr_ebi_solver

#########################################################
 unalias rm

 if( -e ./BLD ) then
    echo "Removing old BLD directory"
    /bin/rm -rf ./BLD
 endif

 mkdir BLD

 cp makefile.v50XX  ./BLD/Makefile

 cd BLD

 make

 cd ..
 
set echo
##########################################################

 if(  -e $OUTDIR  ) then

    echo "Removing old solver files"
    /bin/rm -f ${OUTDIR}/*.[f,F]

 else

   mkdir -p $OUTDIR
   \cp -f ${RXNS_DATA_SRC} $OUTDIR/.

 endif

 $EXDIR/$EXEC

 if( $DEGRADE_SUBS  == "T" )then
     \cp -f ${DEGRADE_CODES}/*.[f,F]  ${OUTDIR}/.
 endif

 exit() 
