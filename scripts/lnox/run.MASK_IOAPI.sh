#!/bin/csh

# set directories

set BASE = $cwd
set Rscript = $BASE/R-scripts

echo $BASE

# This file is used to the met variables 
# Must include the variable RC for convective rainfall
# Must include the variable CLDT for cloud top height



############################################################
# Set up time-invariant file: ocean mask
############################################################

# echo "Calculating ocean mask file. This will take a while."

# # image of ocean mask file output for diagnostic testing
 set MASKIMG = $BASE/R-out/canada_mask.png

# # ocean mask output file
 setenv MASKFILE $BASE/R-out/canada_mask.csv

# # R code to develop ocean mask file 

#  R --vanilla < $Rscript/canada_mask.R #  > & $Rscript/ocean_mask.dump &



############################################################
# Set up NLDN flash counts
############################################################

    setenv DESCFILE $BASE/mcip/met.20060101.ioapi

    setenv OUTFILE $BASE/output/MASK_CANADA.ioapi

    rm -f $OUTFILE

    echo $OUTFILE

    $BASE/src/MASK_IOAPI

