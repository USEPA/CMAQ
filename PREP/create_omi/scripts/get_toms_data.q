#! /bin/csh -f
set echo

#parameters determing what raw data files to download for annual OMI file
  #RECOMMENDATION: Download data last four months before start of the year and
  #up to one month after the end of the year. The suggestion attempts to prevent 
  #encountering polar night in northern hemisphere and other possible data gaps.
  set YEAR_STUDY = "2011"                #> year to be covered by OMI file
  set START_DATE = "2010-09-01" #> beginning date ( Sept 1, 2010)
  set END_DATE   = "2012-02-01" #> ending date    ( Feb  1, 2012)

#output directory for raw OMI data files
  set OUTDIR = TOMS_OMI_O3_column/$YEAR_STUDY

#download raw data files 
  set BASE = $cwd
  if( ! ( -e $OUTDIR ) )mkdir -p $OUTDIR
  cd $OUTDIR

  set TODAYG = `date -ud "${START_DATE}" +%Y%m%d` #> data string in data file 
  set TODAYJ = `date -ud "${START_DATE}" +%Y%j`   #> Convert YYYY-MM-DD to YYYYJJJ
  set START_DAY = ${TODAYJ} 
  set STOP_DAY = `date -ud "${END_DATE}" +%Y%j`   #> Convert YYYY-MM-DD to YYYYJJJ

  while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ

     set year = `date -ud "${TODAYG}" +%Y`

     wget https://acd-ext.gsfc.nasa.gov/anonftp/toms/omi/data/Level3e/ozone/Y$year/L3e_ozone_omi_"${TODAYG}".txt

     #> Increment both File and Julian Dates
     set TODAYG = `date -ud "${TODAYG}+1days" +%Y%m%d` #> Add a day for tomorrow
     set TODAYJ = `date -ud "${TODAYG}" +%Y%j`         #> Convert YYYY-MM-DD to YYYYJJJ

  end  #Loop to the next file to download

  cd $BASE

exit()
