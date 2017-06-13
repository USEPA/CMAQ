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

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
 source config.cmaq


#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! -e ${BINDIR} ) then
  setenv BINDIR ${CMAQ_HOME}/Tools/sitecmp/SITECMP_${compiler}
 endif

#> Set the name of the executable.
 setenv EXEC sitecmp.exe


# =====================================================================
#> SITECMP Configuration Options
# =====================================================================

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
 setenv GAS_1 "O3,ppb,O3,ppb,O3"                 
 setenv GAS_2 "NOY,ppb,NOY,ppb,NOY"              
 setenv GAS_3 "NO2,ppb,NO2,ppb,NO2"              
 setenv GAS_4 "NOX,ppb,NO+NO2,ppb,NOX"           
 setenv GAS_5 "CO,ppb,CO,ppb,CO"                 
 setenv GAS_6 "SO2,ppb,SO2,ppb,SO2"              
 setenv GAS_7 "PM25,ug/m3,PMIJ,ug/m3,PM_TOT"     
 setenv GAS_8 "PM25,ug/m3,PMIJ_FRM,ug/m3,PM_FRM" 
 setenv GAS_9 "PM10,ug/m3,PM10,ug/m3,PM10"       
 setenv GAS_10 "Isoprene,ppb,ISOP,ppb,Isoprene"  
 setenv GAS_11 "Ethylene,ppb,ETH,ppb,Ethylene"   
 setenv GAS_12 "Ethane,ppb,ETHA,ppb,Ethane"      
 setenv GAS_13 "Toluene,ppb,TOL,ppb,Toluene"     
 setenv GAS_14 "Temperature,C,SFC_TMP,C,SFC_TMP" 
 setenv GAS_15 "RH,%,RH,%,RH"                    
 setenv GAS_16 "Wind_Speed,m/s,WSPD10,m/s,WSPD10"
 setenv GAS_17 ",,PBLH,m,PBLH"                   
 setenv GAS_18 ",,SOL_RAD,watts/m2,Solar_Rad"    
 setenv GAS_19 ",,10*precip,mm/hr,precip"       
   
#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
 setenv GAS_20 "PM25,ug/m3,PM25_TOT,ug/m3,PM25_TOT"
 setenv GAS_21 "PM25,ug/m3,PM25_FRM,,PM25_FRM"     
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
 setenv SITE_FILE AQS_sites.txt

#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available 
#> from the CMAS Center Data clearinghouse under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> Hourly AQS observations are located in AMET12_OBSDATA_YYYY.tar.gz for year YYYY.
 setenv IN_TABLE AQS_hourly_data_2011.csv

#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE AQS_Hourly_CMAQ_v52.csv

#> Executable call:
 ${BINDIR}/${EXEC}
   
 exit()
