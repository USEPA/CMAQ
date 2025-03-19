#! /bin/csh -f

# ===================== SITECMP_v5.5.X Run Script ==================
# Usage: run_sitecmp_SEARCH.csh >&! sitecmp_SEARCH.log &
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

#> Set General Parameters for Configuring the Simulation
 set VRSN      = v55               #> Code Version
 set PROC      = mpi               #> serial or mpi
 set MECH      = cb6r3_ae7_aq      #> Mechanism ID
 set APPL      = Bench_2016_12SE1        #> Application Name (e.g. Gridname)
                                                      
#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 set RUNID = ${VRSN}_${compilerString}_${APPL}

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/sitecmp/scripts/BLD_sitecmp_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = sitecmp_${VRSN}.exe

#> Set output directory
 set POSTDIR = ${CMAQ_DATA}/POST    #> Location where sitecmp file will be written

  if ( ! -e $POSTDIR ) then
	  mkdir $POSTDIR
  endif

# =====================================================================
#> SITECMP Configuration Options
# =====================================================================

# ~~~~~~~~~~~~ START NETWORK SPECIFIC SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~
#> The following environment variables will change depending on what 
#> observation network is being matched with CMAQ output.
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
  setenv AERO_1 "Average O3[ppb],ppb,O3,,O3"                               
  setenv AERO_2 "Average CO[ppb],ppb,CO,,CO"                               
  setenv AERO_3 "Average SO2[ppb],ppb,SO2,,SO2"                            
  setenv AERO_4 "Average NO[ppb],ppb,NO,,NO"                               
  setenv AERO_5 "Average NO2[ppb],ppb,NO2,,NO2"                            
  setenv AERO_6 "Average NOy[ppb],ppb,NOY,ppb,NOY"                         
  setenv AERO_7 "Average HNO3[ppb],ppb,HNO3,ppb,HNO3"                      
  setenv AERO_8 "Average NH3[ppb],ug/m3,NH3,,NH3"                          
  setenv AERO_9 "Average WSP[m/s],m/s,WSPD10,m/s,WSPD10"                   
  setenv AERO_10 "Average WDR[Deg],m/s,WDIR10,m/s,WDIR10"                  
  setenv AERO_11 "Average RH[%],%,RH,%,RH"                                 
  setenv AERO_12 "Average TEMP[Deg C],C,SFC_TMP,C,SFC_TMP"                 
  setenv AERO_13 "25.4*Average RAINFALL[Inches],mm,precip,mm,precip"       
  setenv AERO_14 "Average SR[W/m2],w/m2,SOL_RAD,watts/m2,Solar_Rad"        
  setenv AERO_15 "Average OptEC[ug/m3],ug/m3,PM25_EC,ug/m3,PM25_EC"
  setenv AERO_16 "Average OC[ug/m3],ug/m3,PM25_OC,ug/m3,PM25_OC"
  setenv AERO_17 "Average TC[ug/m3],ug/m3,PM25_EC+PM25_OC,ug/m3,PM25_TC"
  setenv AERO_18 "Average NH4[ug/m3],ug/m3,PM25_NH4,,PM25_NH4"
  setenv AERO_19 "Average NO3[ug/m3],ug/m3,PM25_NO3,,PM25_NO3"
  setenv AERO_20 "Average SO4[ug/m3],ug/m3,PM25_SO4,,PM25_SO4"

#> End Species List ###


# ~~~~~~~~~~~~ END NETWORK SPECIFIC SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~

#> define time window
 set SDATE = "2016-07-01"    #> beginning date (July 1, 2016)
 set EDATE = "2016-07-14"  #> ending date    (July 14, 2016)
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
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201607.nc
         #[Add location of input file, e.g. COMBINE_ACONC file.]

#> SITE FILE containing site-id, longitude, latitude, and optionally 
#> GMT offset, state, county, and elevation (csv format)
#> The column headings for the required variables need to be 
#> stat_id, lon, and lat (case insensitive)
#> The column headings for the optional variables need to be
#> gmt_offset, state, county, and elevation (case insensitive)
#> See the README.md file in this folder for the information on 
#> where to download this file.
 setenv SITE_FILE SEARCH_full_site_list.csv
#> On EPA system:
#  setenv SITE_FILE /work/MOD3EVAL/aq_obs/routine/site_metadata_files/SEARCH_full_site_list.csv


#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available online.
#> See the README.md file in this folder for the information on 
#> where to download this file.
 setenv IN_TABLE SEARCH_hourly_data_2016.csv
#> One EPA system:
#  setenv IN_TABLE /work/MOD3EVAL/aq_obs/routine/2016/SEARCH_hourly_data_2016.csv


#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE ${POSTDIR}/SEARCH_Hourly_CMAQ_${RUNID}_201607.csv

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif
   
 exit()
