#!/bin/csh -f 

#> Source the config.cmaq file to set the run environment
 source ../config.cmaq

#> Check that M3DATA is set:
 if ( ! -e $M3DATA ) then
    echo "   $M3DATA path does not exist"
    exit 1
 endif
 echo " "; echo " Input data path, M3DATA set to $M3DATA"; echo " "

 set BASE = $M3HOME/scripts/calmap
 set OUT = $M3DATA/crop

 setenv GRID_CRO_2D $M3DATA/mcip/GRIDCRO2D_110701
 setenv BELD01      $M3DATA/dust/beld3_12CalnexBench_output_a.ncf

 setenv EXTN 12CalnexBench
 setenv CROPMAP01 $OUT/BeginPlanting_$EXTN
 setenv CROPMAP02 $OUT/CropMap02_$EXTN
 setenv CROPMAP03 $OUT/CropMap03_$EXTN
 setenv CROPMAP04 $OUT/EndPlanting_$EXTN
 setenv CROPMAP05 $OUT/CropMap05_$EXTN
 setenv CROPMAP06 $OUT/CropMap06_$EXTN
 setenv CROPMAP07 $OUT/CropMap07_$EXTN
 setenv CROPMAP08 $OUT/EndHarvesting_$EXTN

#> turn off excess WRITE3 logging
 setenv IOAPI_LOG_WRITE F
 $BASE/src/CALMAP; exit

