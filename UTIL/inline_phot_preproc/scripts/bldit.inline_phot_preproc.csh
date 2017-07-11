#!/bin/csh -fx

source ../../../config.cmaq

#> CMAQv5.2 Mechanism Options: cb05tucl_ae6_aq cb05tump_ae6_aq cb05e51_ae6_aq saprc07tb_ae6_aq saprc07tc_ae6_aq  saprc07tic_ae6i_aq   racm2_ae6_aq 
 set Mechanism = racm2_ae6_aq          #> CMAQ mechanism ID
 set BASE  = $CMAQ_HOME/UTIL/inline_phot_preproc
 set XBASE = BLD_$Mechanism

#> option to set compiler and build a new executable (not required)
 setenv COMPILER  PGF90 #> INTEL, PGF90, or GFORT
 set day = ` date "+%b-%d-%Y" `
 set OUTDIR = $BASE/output/csqy_table_${Mechanism}-${day}

#> specify directory containing the mechanism modules or include files
 setenv GC_INC $BASE/input/$Mechanism

#> use RXNS_DATA_MODULE, comment out if CMAQ v5.02 and keep if CMAQ v5.1 or higher
 setenv USE_RXNS_MODULES T
 if( ${USE_RXNS_MODULES} == "T" )then
    if( ! ( -e $GC_INC/RXNS_DATA_MODULE.F90 ) )then
       ls $GC_INC/RXNS_DATA_MODULE.F90
       exit()
    endif       
 endif
 
#> define exectubale
 set EXEC = CSQY_TABLE_PROCESSOR

#> create executable
if ( ! -d $XBASE ) then
     mkdir $XBASE
else
     rm -f $XBASE/*
endif 

 cp ../src/* $XBASE
 cd $XBASE ; make clean; make -f dumb.makefile; cd ../

 if( ! ( -e  $XBASE/$EXEC ) )then
     \ls $XBASE/$EXEC
     echo "make failed or value of XBASE incorrect"
     exit()
 endif

 exit()
