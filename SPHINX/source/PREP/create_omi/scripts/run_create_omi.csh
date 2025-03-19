#! /bin/csh -f

# ======================= CREATE_OMI 5.5.X Run Script ========================
# Usage: run_create_omi.csh >&! create_omi.log &
#
# To report problems or request help with this script/program:
#             http://www.cmascenter.org
# ====================================================================
set echo
# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct
#> libraries using config.cmaq. Options: intel | gcc | pgi
  setenv compiler gcc

set echo

#> Source the config.cmaq file to set the build environment
 set BASE      = $cwd
 if( ! ( -e ../src ) )then
    cd ../../..
    source ./config_cmaq.csh
 else
#work offline from CMAQ repository and build environment
    setenv Offline "Y"
    echo ${Offline}
    setenv COMPILER $compiler
    setenv compilerString ${compiler}
    setenv CMAQ_HOME $cwd/..
    echo "set offline is ${Offline}"
 endif
 echo ${CMAQ_HOME}
 cd $BASE


echo $compilerString

#define executable and its path
 set VRSN  = v55                #> Code Version
 set EXEC  = create_omi_${VRSN}.exe

# uncomment if using executable compiled with debugging flags
if( ! ( $?Offline ) )then
#define paths and create data file list
   setenv Debugging 
   if( $?Debugging )then
       set XBASE = ${BASE}/BLD_create_omi_${VRSN}_${compilerString}_debug
   else
       set XBASE = ${BASE}/BLD_create_omi_${VRSN}_${compilerString}
   endif
 echo "working online"
else
 set XBASE = ${BASE}/../src
 echo "working offline"
endif

echo $XBASE

if( ! ( -e  ${XBASE}/${EXEC} ) )then
    \ls ${XBASE}/${EXEC}
    exit()
endif

set input_dir = ${BASE}
# The below commented out commands set raw data from NASA ASDISC
#set YEAR      = "2015"
#set DATA_DIR  = ${input_dir}"/OZONE_asdisc/test_data_"${YEAR}
#set infile = acdisc_list.dat
#\ls -1 $DATA_DIR/*.ascii  >&! ${input_dir}/${infile}

# The below commands uses raw data from NASA TOMS
 set YEAR      = "2019"
 set DATA_DIR  = ${BASE}"/TOMS_OMI_O3_column/"${YEAR}
#set DATA_DIR  = ${BASE}"/TOMS_OMI_O3_column/test_data_"${YEAR}
 set infile = toms_list.dat
 \ls -1 $DATA_DIR/*.txt  >&! ${input_dir}/${infile}
 cat ${input_dir}/${infile}
 
#root directory for output files; final output directory set latter.
 set OUT_ROOT = ${BASE}"/output"

#check OMI data file list
#optimal results with data for entire length of needed year, plus December of previous year and
# at least January 1st of next year
 set numb_files = ` cat ${input_dir}/${infile} | wc -l `
 if( $numb_files < 2 )then
   echo "Too few files to process"
   exit()
 else
   echo "${numb_files} to process"
 endif

#parameters for routine that creates CMAQ OMI.dat file
#Flag to replace missing with previous date
setenv PREV_DATE T
 
#Flag to  output ASCII and IOAPI file at full lat/lon resolution
#Only for visualization. Not used by the CMAQ model
setenv FULL_FILES F

#Should be an odd number so output data in includes the equator
#Minimum value and Default value is 17
#Maximum value dependent on input satellite data
 setenv NLAT_OMI 17
#setenv NLAT_OMI 719

#Set number of longitude points of ASCII OMI.dat file
#Should be an odd number so first and last longitude point equal
#Minimum value and Default value is 17
#Maximum value dependent on input satellite data
 setenv NLON_OMI 17
#setenv NLON_OMI 1441

#Set the degrees between the first latitude point from adjacent pole
#Minimum value dependent on input satellite data but greater zero
 setenv LAT_BORDER 10.0
#setenv LAT_BORDER 1.0

#output directory
set OUTDIR = ${OUT_ROOT}"/omi_"${compilerString}"_"${YEAR}"_"${NLAT_OMI}"X"${NLON_OMI}

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
    setenv OMI_FILE_LIST ${INFILE}
else
   \ls ${INFILE}
   exit()
endif  
 
#full resolution OMI data in ascii format
set omi_full_dat = omi_full_${YEAR}.dat
setenv  OMI_FULL_DAT ${OUTDIR}/${omi_full_dat}

#subset of OMI data for CMAQ in ascii format
set omi_cmaq_dat = omi_cmaq_${YEAR}.dat
setenv  OMI_CMAQ_DAT ${OUTDIR}/${omi_cmaq_dat}

#full resolution OMI data in netcdf/IOAPI format
set omi_full_ncf = omi_full_${YEAR}.ncf
setenv  OMI_FULL_NCF ${OUTDIR}/${omi_full_ncf}

#subset of OMI data for CMAQ in netcdf/IOAPI format
set omi_cmaq_ncf = omi_cmaq_${YEAR}.ncf
setenv  OMI_CMAQ_NCF ${OUTDIR}/${omi_cmaq_ncf}

#exit()
 ${XBASE}/${EXEC}

#remove fort.* files
 \rm -f fort.*
 
cd $BASE

#report on output directories contents
\ls -h -l ${OUTDIR}/*

exit()
