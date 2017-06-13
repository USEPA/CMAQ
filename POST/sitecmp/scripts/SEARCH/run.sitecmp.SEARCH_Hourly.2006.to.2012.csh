#! /bin/csh -f

# ===================== SITECMP_v5.2 Run Script =====================
# Usage: run.sitecmp.csh >&! sitecmp_V52.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================


# ~~~~~~~~~~~~~~~~~~~~~~~~ Start EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#> Portable Batch System - The following specifications are 
#> recommended for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#PBS -N run.sitecmp.csh
#PBS -l walltime=1:30:00
#PBS -l nodes=login
#PBS -q singlepe 
#PBS -V
#PBS -m n
#PBS -j oe
#PBS -o ./sitecmp.log

#> Configure the system environment
# source /etc/profile.d/modules.csh 
#> Set location of combine executable.
# setenv BINDIR /home/css/CMAQ-Tools/scripts/sitecmp
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~ End EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Set TABLE TYPE
 setenv TABLE_TYPE CASTNET

#> Specify the variable names used in your observation inputs
#> and model output files for each of the species you are analyzing below.
#>
#> variable format:
#>    Obs_expression, Obs_units, [Mod_expression], [Mod_unit], [Variable_name]
#>
#> The expression is in the form:
#>       [factor1]*Obs_name1 [+][-] [factor2]*Obs_name2 ...

 setenv AERO_1 "Average O3[ppb],ppb,O3,,O3"
 setenv AERO_2 "Average CO[ppb],ppb,CO,,CO"
 setenv AERO_3 "Average SO2[ppb],ppb,SO2,,SO2"
 setenv AERO_4 "Average NO[ppb],ppb,NO,,NO"
 setenv AERO_5 "Average NO2[ppb],ppb,NO2,,NO2"
 setenv AERO_6 "Average NOy[ppb],ppb,NOY,ppb,NOY"
 setenv AERO_7 "Average HNO3[ppb],ppb,HNO3,ppb,HNO3"
 setenv AERO_8 "Average NH3[ppb],ug/m3,NH3,,NH3"
 setenv AERO_9 "Average OptEC[ug/m3],ug/m3,AECIJ,ug/m3,EC"
 setenv AERO_10 "Average OC[ug/m3],ug/m3,AOCIJ,ug/m3,OC"
 setenv AERO_11 "Average TC[ug/m3],ug/m3,AECIJ+AOCIJ,ug/m3,TC"
 setenv AERO_12 "Average WSP[m/s],m/s,WSPD10,m/s,WSPD10"
 setenv AERO_13 "Average WDR[Deg],m/s,WDIR10,m/s,WDIR10"
 setenv AERO_14 "Average RH[%],%,RH,%,RH"
 setenv AERO_15 "Average TEMP[Deg C],C,SFC_TMP,C,SFC_TMP"
 setenv AERO_16 "25.4*Average RAINFALL[Inches],mm,precip,mm,precip"
 setenv AERO_17 "Average SR[W/m2],w/m2,SOL_RAD,watts/m2,Solar_Rad"
 setenv AERO_18 "Average TEOM[ug/m3],ug/m3,PMIJ,,PM_TOT"
 setenv AERO_19 "Average NH4[ug/m3],ug/m3,ANH4IJ,,NH4"
 setenv AERO_20 "Average SO4[ug/m3],ug/m3,ASO4IJ,,SO4"
   
 setenv AERO_21 "Average OptEC[ug/m3],ug/m3,PM25_EC,ug/m3,PM25_EC"
 setenv AERO_22 "Average OC[ug/m3],ug/m3,PM25_OC,ug/m3,PM25_OC"
 setenv AERO_23 "Average TC[ug/m3],ug/m3,PM25_EC+PM25_OC,ug/m3,PM25_TC"
 setenv AERO_24 "Average NH4[ug/m3],ug/m3,PM25_NH4,,PM25_NH4"
 setenv AERO_25 "Average NO3[ug/m3],ug/m3,PM25_NO3,,PM25_NO3"
 setenv AERO_26 "Average SO4[ug/m3],ug/m3,PM25_SO4,,PM25_SO4"
 
#>> End Species List <<#

#> define time window
 setenv SDATE = "2011-07-1"    #> beginning date (July 1, 2011)
 setenv EDATE   = "2011-07-2"  #> ending date    (July 2, 2011)
 setenv START_TIME 0      
 setenv END_TIME   230000   

#> Convert SDATE and EDATE to Julian day.
#> (required format for sitecmp START_DATE and END_DATE environment variables)
 setenv START_DATE `date -ud "${SDATE}" +%Y%j`
 setenv END_DATE `date -ud "${EDATE}" +%Y%j` 

#> define the PRECIP variable
 setenv PRECIP RT

#> adjust for daylight savings
 setenv APPLY_DLS N

#> set missing value string
 setenv MISSING '-999'

#> Projection sphere type (use type 20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> Number of hours to add when retrieving time steps from M3_FILE_n files during processing.
#> This should only be non-zero if the M3_FILE_n files were pre-processed with a utility like m3tshift (default 0).
 setenv TIME_SHIFT 0

#############################################################
#  Input files
#############################################################

#> ioapi input files containing VNAMES (max of 10)
 setenv M3_FILE_1 [Add location of input file, e.g. COMBINE_ACONC file.]

#> SITE FILE containing site-id, longitude, latitude, time zone (tab delimited)
#> This file can be downloaded from the CMAS Center Data clearinghouse 
#> under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> AQS site file is located in AMET12_SITE_FILES.tar.gz
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available 
#> from the CMAS Center Data clearinghouse under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> Hourly AQS observations are located in AMET12_OBSDATA_YYYY.tar.gz for year YYYY.
 setenv IN_TABLE SEARCH_hourly_data_2011.csv

#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE SEARCH_Hourly_CMAQ_v52.csv

#> Executable call:
 ${BINDIR}/${EXEC}
   
 exit()


