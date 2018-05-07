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

#>Number of Wavebands to write to output files starting from the band with the longest
#>to shortest wavelength from the bands; can equal 1 to 18
#>CMAQ version 5.2.1 uses seven bands and CMAQ-MPAS uses eleven bands

setenv N_WAVEBANDS_OUT 7

#> define exectubale
 set EXEC = CSQY_TABLE_PROCESSOR_${Mechanism}

#> create executable
 setenv APPL $Mechanism
 cd src ; make clean; make -f inline_phot_preproc.makefile; cd ../
 if( ! ( -e  $XBASE/$EXEC ) )then
     \ls $XBASE/$EXEC
     echo "make failed or value of XBASE incorrect"
     exit()
 endif

#set up input file directories
 set CSQY_DIR    = ${BASE}/photolysis_CSQY_data
 set REFRACT_DIR = ${BASE}/water_clouds
 set WVBIN_DIR   = ${BASE}/flux_data

# Define environment variables for inputs
#
#wavelength bin mapping data file
 setenv WVBIN_FILE    $WVBIN_DIR/wavel-bins.dat
#Solar flux spectrum data file
 setenv FLUX_FILE     $WVBIN_DIR/solar-p05nm-UCI.dat
#Raw cross-section and quantum yield data for photolysis rates
 setenv CSQY_DATA_RAW $CSQY_DIR

#> define files for aerosol refractive indices
 setenv MAX_NUMB_REFRACT 6                              # maximum number possible
 setenv AE_REFRAC_LIST "WATER DUST SOLUTE SOOT SEASALT" # list to process
 
#Set environment variable for their paths
 setenv WATER     $REFRACT_DIR/water_refractive_index.dat
#setenv INSOLUBLE $REFRACT_DIR/inso00                    
 setenv DUST      $REFRACT_DIR/inso00                    
 setenv SOLUTE    $REFRACT_DIR/waso00                    
 setenv SOOT      $REFRACT_DIR/soot00-two_way-Oct_21_2012
 setenv SEASALT   $REFRACT_DIR/ssam00                    

#Define output directory variable and create
 if( ! ( -d $OUTDIR ) ) mkdir -p $OUTDIR
 setenv OUT_DIR       $OUTDIR


$XBASE/$EXEC >&! bldrun.log

 \rm -f $XBASE/$EXEC
 \rm -f fort.*
 
 cd $BASE

unset echo
echo " "
echo " "
echo "Check directory ${OUTDIR} for CSQY_DATA_${Mechanism} and PHOT_OPTICS.dat files"
echo " "
echo " "

 exit()
