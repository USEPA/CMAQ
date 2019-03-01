#! /bin/csh -f
set echo


#setenv COMPILER INTEL
#setenv COMPILER GFORT
#setenv COMPILER PGF90

#output directory for raw OMI data files
setenv OUTDIR /home/bhutzell/tools/create_CMAQ_OMI_file/obs_OMI
#parameters determing what raw data files to download
#RECOMMEND that output directory has or will download data
#at least 15 months back from the current date so the CMAQ
#model can use O3 column from previous year if needed
setenv  YEAR_START    2010
setenv  MONTH_START   08
setenv  DAY_START     08
setenv  DAYS_FORWARD  366
setenv OUTDIR /home/bhutzell/Downloads/TOMS_OMI_O3_column/$YEAR_START

#download raw data files 
#set BASE = $cwd
#cd $OUTDIR
#wget -r -nd ftp://toms.gsfc.nasa.gov/pub/omi/data/Level3e/ozone/Y$YEAR_START
#cd $BASE

#define path to mechanism include or module data files
 set input_dir = /home/bhutzell/tools/create_CMAQ_OMI_file
 set infile    = OMI_TOMS_fulllist_${YEAR_START}.txt # OMI_fulllist_14t16_test.txt
 
#create OMI data file list
#optimal results with data for entire length of needed year, plus December of previous year and
# at least January 1st of next year
\ls -1 $OUTDIR/*ozone*2009*.txt $OUTDIR/*ozone*201[0,1]*.txt >&! ${input_dir}/${infile}
cat ${input_dir}/${infile}

set numb_files = ` cat ${input_dir}/${infile} | wc -l `
if( $numb_files < 2 )then
  echo "Too few files to process"
  exit()
else
  echo "${numb_files} to process"
endif

#exit()

#parameters for routine that creates CMAQ OMI.dat file
#Flag to replace missing with previous date
setenv PREV_DATE T
 
#Flag to use nearest neighbor average (yields dubious results)
setenv NEIGHBORS F

set XBASE = /home/bhutzell/tools/create_CMAQ_OMI_file
set BASE  = ${XBASE}
set EXEC  = ro3_mod_env
if( ! ( -e  ${XBASE}/${EXEC} ) )then
     \ls ${XBASE}/${EXEC}
     exit()
endif


 set OUTDIR = ${BASE}"/output/omi_"${YEAR_START}"_361X179"
#set OUTDIR = ${BASE}/output/omi_2017_17X17
#set output directory, make if needed
 if( ! ( -d $OUTDIR ) )mkdir -p $OUTDIR
#change to output directory
 if( -d $OUTDIR  )then
    cd $OUTDIR
 else
   echo "failure make and/or change output directory"
   \ls $OUTDIR
   exit()
 endif

pwd

set INFILE = ${input_dir}/${infile} 
if( -e $INFILE )then 
#    ln -sf ${INFILE} OMI_FILE_LIST
    setenv OMI_FILE_LIST ${INFILE}
else
   \ls ${INFILE}
   exit()
endif  

 
#CMAQ version 52 repository copied on OMI.dat
if( !( -e $BASE/OMI_1979_to_2015.dat ) )then
   ls $BASE/OMI_1979_to_2015.dat; echo "not found"
   exit()
endif    
ln -sf $BASE/OMI_1979_to_2015.dat ${OUTDIR}/OMI_current.dat

#full resolution OMI data in ascii format
set omi_full_dat = omi_full_${YEAR_START}.dat
#subset of OMI data for CMAQ in ascii format
 set omi_cmaq_dat = omi_cmaq_${YEAR_START}_361X179.dat
#set omi_cmaq_dat = omi_cmaq_${YEAR_START}_17X17.dat

#ln -s -f ${omi_full_dat} OMI_FULL_DAT
#ln -s -f ${omi_cmaq_dat} OMI_CMAQ_DAT

setenv  OMI_FULL_DAT ${OUTDIR}/${omi_full_dat}
setenv  OMI_CMAQ_DAT ${OUTDIR}/${omi_cmaq_dat}

#full resolution OMI data in ascii format
set omi_full_ncf = omi_full_${YEAR_START}.ncf
#subset of OMI data for CMAQ in ascii format
set omi_cmaq_ncf = omi_cmaq_${YEAR_START}.ncf

setenv  OMI_FULL_NCF ${OUTDIR}/${omi_full_ncf}
setenv  OMI_CMAQ_NCF ${OUTDIR}/${omi_cmaq_ncf}

#exit()
 ${XBASE}/${EXEC}

#remove symbolic links and fort.* files
 \rm -f OMI_FILE_LIST
 \rm -f OMI_FULL_DAT
 \rm -f OMI_CMAQ_DAT
 \rm -f fort.*
 set test = ` stat -c %h -- ${OUTDIR}/OMI_current.dat `
 if( $test )\rm -f ${OUTDIR}/OMI_current.dat
 
cd $BASE

#report on output directories contents
\ls -h -l ${OUTDIR}/*

exit()
