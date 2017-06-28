#! /bin/csh -fx
# Sript to run CR_EBI_SOLVER 

 set EXEC            = cr_ebi_solver 

#> Define the gas, aerosol and aqueous components in MECHNAME
#> GC_NAME Options: CB6R3, CB05MP51, CB05E51, CB05MP51, CB05TUCL, CB05TUMP, SAPRC07TB, SAPRC07TC, SAPRC07TIC, RACM2
#> AE_NAME Options: AE6, AE6I
#> AQ_NAME Options: AQ

 setenv GC_NAME       CB6R3
 setenv AE_NAME       AE6
 setenv AQ_NAME       AQ

set MECH = ` echo ${GC_NAME}_${AE_NAME}_${AQ_NAME} |  tr 'a-zA-Z' 'A-Za-z' `
set REP  = ../input/${MECH}
set EXDIR = BLD_$MECH

# output directory
 set day = ` date "+%b-%d-%Y" `
 setenv OUTDIR  ../output/ebi_${MECH}-${day}
 
 setenv COPYRT_FLAG      N
 setenv CVS_HDR_FLAG     N

 setenv PAR_NEG_FLAG    F    # True for CB4 and CB05 but false for SAPRC07t and RACM2 
 setenv DEGRADE_SUBS    F    # include calls for HAPs degrade routines (true cb05tump and cb05mp51)
# below option is a possible solution based on work by Golam Sawar if the 
# mechanism includes excited NO2.  The user employs it at their own risk 
# and should check the EBI solver's accuracy against the Gear or Rosenbrock solver
 setenv NO2EX_CYCLE     F    # modify group one solution to include excited NO2

#Set the below compound names within the mechanism
 setenv MECH_NO    NO     #  Mechanism name for nitric oxide
 setenv MECH_NO2   NO2    #  Mechanism name for nitrogen dioxide
 setenv MECH_NO2EX NO2EX  #  SAPRC, RACM Mechanism name for excited nitrogen dioxide; not in Carbon Bond
 setenv MECH_O3    O3     #  Mechanism name for ozone
 setenv MECH_O3P   O      #  CB4, CB05, CB6 Mechanism name for ground state oxygen atom
#setenv MECH_O3P   O3P    #  SAPRC, RACM Mechanism name for ground state oxygen atom
 setenv MECH_O1D   O1D    #  Mechanism name for excited state oxygen atomi; O1D2 or O1D for SAPRC
 setenv MECH_OH    OH     #  Mechanism name for hydroxyl radical
 setenv MECH_HO2   HO2    #  Mechanism name for hydroperoxy radical
 setenv MECH_HONO  HONO   #  Mechanism name for nitrous acid
 setenv MECH_HNO4  PNA    #  CB4, CB05, CB6 Mechanism name for peroxynitric acid
#setenv MECH_HNO4  HNO4   #  SAPRC, RACM Mechanism name for peroxynitric acid
 setenv MECH_PAN   PAN    #  Mechanism name for peroxy acetyl nitrate
 setenv MECH_C2O3  C2O3   #  CB4, CB05, CB6 Mechanism name for peroxy acetyl radical
#setenv MECH_C2O3  MECO3  #  SAPRC, RACM Mechanism name for peroxy acetyl radical; CCO_O2 or MECO3
 setenv MECH_NO3   NO3    #  Mechanism name for nitrate radical
 setenv MECH_N2O5  N2O5   #  Mechanism name for dinitrogen pentoxide

#uses CMAQ version 5.2 of these include files for chemical mechanism
 setenv RXNS_DATA_SRC  ${REP}/RXNS_DATA_MODULE.F90

# using templates for CMAQ version 5.0 
 setenv TMPLDIR         ../template_RXNSU_OPT

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
