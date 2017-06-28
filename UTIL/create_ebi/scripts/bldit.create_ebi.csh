#! /bin/csh -f
# Script to compile CR_EBI_SOLVER 

 date; set timestamp; set echo 

 set BASE            = $cwd
 set STEM            = ../..
 set EXDIR           = ${BASE}/BLD  
 set EXEC            = cr_ebi_solver 

#> option to set compiler and build a new executable (not required)
 setenv COMPILER  PGF90 #> INTEL, PGF90, or GFORT

#> Define the gas, aerosol and aqueous components in MECHNAME
#> GC_NAME Options: CB6R3, CB05MP51, CB05E51, CB05MP51, CB05TUCL, CB05TUMP, SAPRC07TB, SAPRC07TC, SAPRC07TIC, RACM2
#> AE_NAME Options: AE6, AE6I
#> AQ_NAME Options: AQ

 setenv GC_NAME       CB6R3
 setenv AE_NAME       AE6
 setenv AQ_NAME       AQ

set MECH = ` echo ${GC_NAME}_${AE_NAME}_${AQ_NAME} |  tr 'a-zA-Z' 'A-Za-z' `
set REP  = $STEM/input/${MECH}

#uses CMAQ version 5.2 of these include files for chemical mechanism
 setenv RXNS_DATA_SRC  ${REP}/RXNS_DATA_MODULE.F90

# using templates for CMAQ version 5.2 
 setenv TMPLDIR         ${STEM}/template_RXNSU_OPT
 setenv DEGRADE_CODES   ${STEM}/degrade_codes_serial-RXNST
 setenv SRCDIR          ${STEM}/src_RXNSU

#########################################################
 unalias rm

 if( -e ./BLD_$MECH ) then
    echo "Removing old BLD directory"
    /bin/rm -rf ./BLD_$MECH
 endif

 mkdir BLD_$MECH

 cp ../makefile.v50XX  ./BLD_$MECH/Makefile

 cd BLD_$MECH

 make

 cd ..
 
set echo

exit() 
