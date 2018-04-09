#!/bin/csh -fx

 set echo

#> CMAQ Mechanism under Repository directory CTM/src/MECHS or
#> user defines their own Mechanism name
 set Mechanism = cb6r3_ae6_aq          #> CMAQ mechanism ID
# path to CMAQ repository  
 set REPO  = ../../CCTM/src/MECHS
 set BASE  = $cwd
 set XBASE = $BASE

#> option to set compiler and build a new executable (not required)
 setenv COMPILER  GFORT  #> INTEL, PGF90, or GFORT
 set day = ` date "+%b-%d-%Y" `
 set OUTDIR = $BASE/output/csqy_table_${Mechanism}-${day}-${COMPILER}

#> specify directory containing the mechanism modules or include files
  setenv GC_INC $BASE/input/$Mechanism #> User Defines directory with the below mechanism data module
# setenv GC_INC $REPO/$Mechanism

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

#> define exectubale
 set EXEC = CSQY_TABLE_PROCESSOR_${Mechanism}

#> create executable
 setenv APPL $Mechanism
 cd src ; make clean; make -f dumb.makefile; cd ../

 if( ! ( -d $OUTDIR ) ) mkdir -p $OUTDIR

 set CSQY_DIR    = ${BASE}/photolysis_CSQY_data
 set REFRACT_DIR = ${BASE}/water_clouds
 set WVBIN_DIR   = ${BASE}/flux_data

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

# cd $OUTDIR

 if( ! ( -e  $XBASE/$EXEC ) )then
     \ls $XBASE/$EXEC
     echo "make failed or value of XBASE incorrect"
     exit()
 endif


 $XBASE/$EXEC >&! bldrun.log


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
 \rm -f $XBASE/$EXEC
 \rm -f fort.*
#\rm -f OPTICS_DATA 
#\rm -f CSQY_DATA 
 
 cd $BASE

unset echo
echo " "
echo " "
echo "Check directory ${OUTDIR} for CSQY_DATA_${Mechanism} and PHOT_OPTICS.dat files"
echo " "
echo " "

 exit()
