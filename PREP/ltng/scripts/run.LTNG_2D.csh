#!/bin/csh

# This script requires that the R statistical package is
# installed on your system and in your path.

set CREATE_OCEANMASK = Y   # Create ocean mask file, only needs to be done once
set CREATE_FLASHRATIOS = N # Create flash ratios, only needs to be done once
set RUN_NLDN2DDATA = N     # Create CMAQ input LNOx file, needs to be run for each simulation month
set CREATE_PLOTS = N       # Generage lightning NOx plots

# set directories

set BASE = $cwd
set Rscript = $BASE/R-scripts
set INDIR = $M3DATA/lnox
#set OUTDIR = $M3DATA/lightning
set OUTDIR = $cwd

# set time period(s) for creating flash count data
set months = (07)
set year = 2011

############################################################
# Set meteorology data file to use create ocean mask
#   and flash count ratios; any date period will do, only
#   the grid information is taken from this file
# METFILE is created from MCIP output files (METCRO2D)
# Must include the variable RC for convective rainfall
# Must include the variable CLDT for cloud top height
############################################################

setenv METFILE $M3DATA/mcip/METCRO2D_110701

if ( $CREATE_OCEANMASK == Y ) then 
############################################################
# Set up time-invariant file: ocean mask
############################################################

 echo "Calculating ocean mask file. This will take a while."

# # image of ocean mask file output for diagnostic testing
 setenv OCEANMASKIMG $BASE/R-out/ocean_mask.png

# # ocean mask output file
 setenv OCEANMASKFILE $BASE/R-out/ocean_mask.csv

# # R code to develop ocean mask file 

   R --vanilla --args file.name=$METFILE \
       file.out=$OCEANMASKFILE file.img=$OCEANMASKIMG \
       file.Rscript=$Rscript \
       < $Rscript/ocean_mask.R #> & $Rscript/ocean_mask.dump 

endif

if ( $CREATE_FLASHRATIOS == Y ) then  
############################################################
# Set up inter-cloud to cloud-to-ground ratio
############################################################

echo "Calculating intercloud to cloud-to-ground ratio"

# This is a climatology
# Use summer for warm months and winter for cooler months

set ICCGINsummer = $BASE/input/iccg.Boccippio.summer.txt
set ICCGINwinter = $BASE/input/iccg.Boccippio.winter.txt
set ICCGIN = $ICCGINsummer

# ICCG output file
setenv ICCG $BASE/R-out/iccg.interpolate.csv

# image of inter-cloud : cloud-to-ground (ICCG) ratio for diagnostic testing
set ICCGIMG = $BASE/R-out/iccg.png


# R code to develop inter-cloud : cloud-to-ground (ICCG) ratio

   R --vanilla  --args file.name=$METFILE \
       file.iccg=$ICCGIN file.out=$ICCG \
       file.img=$ICCGIMG file.Rscript=$Rscript \
       < $Rscript/iccg.R #> & $Rscript/iccg.dump 

endif

if ( $RUN_NLDN2DDATA == Y ) then
############################################################
# Set up NLDN flash counts
############################################################

echo "NLDN flash counts"

    foreach month ($months)

    echo $month

    ls -l $METFILE
    
    setenv NLDNFILE $INDIR/flash_data/monthly_flash_density/NLDN.$year.$month.ioapi
    ls -l $NLDNFILE
 
    setenv OCEANMASKFILE $BASE/R-out/ocean_mask.csv
    setenv ICCG $BASE/R-out/iccg.interpolate.csv

    setenv STRIKE_FACTOR 147.0

    # set moles of N per flash (CG and IC)
    setenv MOLES_N_CG 500.0
    setenv MOLES_N_IC 500.0
 
    if ( ! -d $OUTDIR ) mkdir -p $OUTDIR 
    setenv OUTFILE $OUTDIR/LTNG_RATIO.$year.$month.ioapi

#    rm -f $OUTFILE

    echo $OUTFILE

    $BASE/src/LTNG_2D_DATA
    end
endif

if ( $CREATE_PLOTS == Y ) then
# series of plots that diagnose the lightning NOx variables
# used for debugging
    foreach month ($months)
    setenv OUTFILE $OUTDIR/LTNG_RATIO.$year.$month.ioapi
    set PLOTLNOx = $BASE/R-out/plot_LNOx_params.pdf

     R --vanilla --args file.name=$OUTFILE \
     file.img=$PLOTLNOx file.Rscript=$Rscript \
     < $Rscript/plot_LNOx_params.R

    end
endif 
