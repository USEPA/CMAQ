#!/bin/csh -fx

source ../../../config.cmaq

#> CMAQv5.1 Mechanism Options: cb05tucl_ae6_aq cb05tump_ae6_aq cb05e51_ae6_aq saprc07tb_ae6_aq saprc07tc_ae6_aq  saprc07tic_ae6i_aq   racm2_ae6_aq 
 set Mechanism = racm2_ae6_aq          #> CMAQ mechanism ID
 set REPO  = ../../CCTM/src/MECHS      #> Repository Directory with all mechanism data modules
 set BASE  = $CMAQ_HOME/UTIL/inline_phot_preproc
 set XBASE = $BASE/scripts/BLD_$Mechanism

 set CSQY_DIR    = ${BASE}/photolysis_CSQY_data
 set REFRACT_DIR = ${BASE}/water_clouds
 set WVBIN_DIR   = ${BASE}/flux_data

 set day = ` date "+%b-%d-%Y" `
 set OUTDIR = $BASE/output/csqy_table_${Mechanism}-${day}

#> specify directory containing the mechanism modules or include files
 setenv GC_INC $REPO/$Mechanism         #> Repository Directory with the above mechanism data modules 
#setenv GC_INC $BASE/input/$Mechanism   #> User Defined Directory with the above mechanism data modules

#> use RXNS_DATA_MODULE, comment out if CMAQ v5.02 and keep if CMAQ v5.1 or higher
 setenv USE_RXNS_MODULES T
 if( ${USE_RXNS_MODULES} == "T" )then
    if( ! ( -e $GC_INC/RXNS_DATA_MODULE.F90 ) )then
       ls $GC_INC/RXNS_DATA_MODULE.F90
       exit()
    endif       
 endif
 
#> Whether to include spectral values of refractive indices for aerosol species [T|Y|F|N]
#>  set F if CMAQ v5.02 and T if CMAQ v5.1 or higher
 setenv WVL_AE_REFRAC T

#> whether optical and CSQY data written to two separate file
#>  set F if CMAQ v5.02 and T if CMAQ v5.1 or higher
 setenv SPLIT_OUTPUT T

 if( ! ( -d $OUTDIR ) ) mkdir -p $OUTDIR

 cd $BASE
 ln -s -f $CSQY_DIR  CSQY_DATA_RAW
 ln -s -f $WVBIN_DIR/wavel-bins.dat  WVBIN_FILE
 ln -s -f $WVBIN_DIR/solar-p05nm-UCI.dat  FLUX_FILE
 ln -s -f $OUTDIR OUT_DIR

#> define files for aerosol refractive index
 ln -s -f $REFRACT_DIR/water_refractive_index.dat WATER
 ln -s -f $REFRACT_DIR/inso00  INSOLUBLE
 ln -s -f $REFRACT_DIR/inso00  DUST
 ln -s -f $REFRACT_DIR/waso00 SOLUTE
#ln -s -f $REFRACT_DIR/soot00 SOOT
 ln -s -f $REFRACT_DIR/soot00-two_way-Oct_21_2012 SOOT
 ln -s -f $REFRACT_DIR/ssam00 SEASALT
 
 $XBASE/CSQY_TABLE_PROCESSOR

 \rm -f CSQY_DATA_RAW
 \rm -f WVBIN_FILE
 \rm -f FLUX_FILE
 \rm -f OUT_DIR

 \rm -f WATER
 \rm -f INSOLUBLE
 \rm -f SOLUTE
 \rm -f SOOT
 \rm -f SEASALT
 \rm -f DUST
 \rm -f fort.*
 
 exit()
