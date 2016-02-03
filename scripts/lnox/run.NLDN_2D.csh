#!/bin/csh

# set directories

set BASE = $cwd
set Rscript = $BASE/R-scripts

echo $BASE


echo "NLDN flash counts"

   set months = (07)

   set year = 2004

   foreach month ($months)

    echo $month

# this file is only used to set the grid description, not the 
# date.
    setenv DESCFILE $BASE/mcip/met.20060101.ioapi
    
# this file contains all NLDN flash data for one month
#    setenv NLDNFILE $BASE/flash_data/flash.$month.$year.txt
    setenv NLDNFILE $BASE/flash_data/Nflash2004.183_daily_v2_lit.raw
    ls -l $NLDNFILE

# this is the output file, containing total flashes for one month
    setenv OUTFILE $BASE/output/NLDN.$year.$month.test.ioapi
    rm -f $OUTFILE
    echo $OUTFILE


    $BASE/src/NLDN_2D

# series of plots that diagnose the lightning NOx variables
# used for debugging. Compare to the file in the directory 
# "verify"

    set PLOTNLDN = $BASE/R-out/plot_NLDN.$year.$month.png


    R --vanilla --args file.name=$OUTFILE \
    file.img=$PLOTNLDN file.Rscript=$Rscript \
    < $Rscript/plot_NLDN.R

    end
