#! /bin/csh -f

# ===================== SITECMP_v5.3 Run Script =====================
# Usage: run.sitecmp.csh >&! sitecmp_v53.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 

 cd ../../..
 source ./config_cmaq.csh

#> Set the model version
 set VRSN = v53

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  setenv BINDIR ${CMAQ_HOME}/POST/sitecmp/scripts/BLD_sitecmp_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC sitecmp_${VRSN}.exe


# =====================================================================
#> SITECMP Configuration Options
# =====================================================================

# ~~~~~~~~~~~~ START NETWORK SPECIFIC SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~
#> The following environment variables will change depending on what 
#> observation network is being matched with CMAQ output.
#> This sample run script is set up for hourly data from AQS.
#> See the README.md file in this folder for the settings to use for 
#> the following networks: IMPROVE, CASTNET, CSN (formally STN), NADP
#> SEARCH, AIRMON

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
 setenv GAS_2 "NO,ppb,NO,ppb,NO"              
 setenv GAS_3 "NOY,ppb,NOY,ppb,NOY"              
 setenv GAS_4 "NO2,ppb,NO2,ppb,NO2"              
 setenv GAS_5 "NOX,ppb,NO+NO2,ppb,NOX"           
 setenv GAS_6 "CO,ppb,CO,ppb,CO"                 
 setenv GAS_7 "SO2,ppb,SO2,ppb,SO2"              
 setenv GAS_8 "PM25,ug/m3,ATOTIJ,ug/m3,PM_TOT"     
 setenv GAS_9 "PM25,ug/m3,PMIJ_FRM,ug/m3,PM_FRM" 
 setenv GAS_10 "PM10,ug/m3,PM10,ug/m3,PM10"       
 setenv GAS_11 "Isoprene,ppb,ISOP,ppb,Isoprene"  
 setenv GAS_12 "Ethylene,ppb,ETH,ppb,Ethylene"   
 setenv GAS_13 "Ethane,ppb,ETHA,ppb,Ethane"      
 setenv GAS_14 "Toluene,ppb,TOL,ppb,Toluene"     
 setenv GAS_15 "Temperature,C,SFC_TMP,C,SFC_TMP" 
 setenv GAS_16 "RH,%,RH,%,RH"                    
 setenv GAS_17 "Wind_Speed,m/s,WSPD10,m/s,WSPD10"
 setenv GAS_18 ",,PBLH,m,PBLH"                   
 setenv GAS_19 ",,SOL_RAD,watts/m2,Solar_Rad"    
 setenv GAS_20 ",,10*precip,mm/hr,precip"       
   
#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
 setenv GAS_21 "PM25,ug/m3,PM25_TOT,ug/m3,PM25_TOT"
 setenv GAS_22 "PM25,ug/m3,PM25_FRM,,PM25_FRM"     
#>> End Species List <<#

# ~~~~~~~~~~~~ END NETWORK SPECIFIC SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~

#> define time window
 setenv SDATE "2011-07-01"    #> beginning date (July 1, 2011)
 setenv EDATE "2011-07-02"  #> ending date    (July 2, 2011)
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
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_ACONC_201107.nc
         #[Add location of input file, e.g. COMBINE_ACONC file.]

#> SITE FILE containing site-id, longitude, latitude, and optionally 
#> GMT offset, state, county, and elevation (csv format)
#> The column headings for the required variables need to be 
#> stat_id, lon, and lat (case insensitive)
#> The column headings for the optional variables need to be
#> gmt_offset, state, county, and elevation (case insensitive)
#> This file can be downloaded from
#> https://github.com/USEPA/AMET/tree/1.2/obs/AQ/site_lists
 setenv SITE_FILE aqs_full_site_list.csv

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
 setenv OUT_TABLE ${CMAQ_DATA}/POST/AQS_Hourly_CMAQ_${VRSN}.csv

#> Executable call:
 ${BINDIR}/${EXEC}
   
 exit()
