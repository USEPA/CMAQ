#! /bin/csh -f
set echo

#output directory for raw OMI data files
setenv OUTDIR /home/bhutzell/tools/create_CMAQ_OMI_file/obs_OMI
#parameters determing what raw data files to download
#RECOMMEND that output directory has or will download data
#at least 15 months back from the current date so the CMAQ
#model can use O3 column from previous year if needed
setenv  YEAR_START    2019
setenv  MONTH_START   01
setenv  DAY_START     01
setenv  DAYS_FORWARD  365
setenv OUTDIR TOMS_OMI_O3_column/$YEAR_START

#download raw data files 
set BASE = $cwd
if( ! ( -e $OUTDIR ) )mkdir -p $OUTDIR
cd $OUTDIR
wget -r -nd ftp://toms.gsfc.nasa.gov/pub/omi/data/Level3e/ozone/Y$YEAR_START
cd $BASE

exit()
