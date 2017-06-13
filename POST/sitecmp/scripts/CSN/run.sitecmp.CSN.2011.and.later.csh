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
 setenv TABLE_TYPE STN

#> Specify the variable names used in your observation inputs
#> and model output files for each of the species you are analyzing below.
#>
#> variable format:
#>    Obs_expression, Obs_units, [Mod_expression], [Mod_unit], [Variable_name]
#>
#> The expression is in the form:
#>       [factor1]*Obs_name1 [+][-] [factor2]*Obs_name2 ...
 setenv AERO_1 "SO4f_val,ug/m3, ASO4IJ,,SO4"
 setenv AERO_2 "NO3f_val,ug/m3, ANO3IJ,,NO3"
 setenv AERO_3 "NH4f_val,ug/m3, ANH4IJ,,NH4"
 setenv AERO_4 "88502_val,ug/m3,PMIJ,,PM_TOT"
 setenv AERO_5 "88502_val,ug/m3,PMIJ_FRM,,PM_FRM"
 setenv AERO_6 "88370adj_val,ug/m3, AOCIJ,,OC"
 setenv AERO_7 "88380_val,ug/m3, AECIJ,,EC"
 setenv AERO_8 "88370adj_val+88380_val,ug/m3,AOCIJ+AECIJ,,TC"
   
#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
 setenv AERO_9 "SO4f_val,ug/m3, PM25_SO4,,PM25_SO4"
 setenv AERO_10 "NO3f_val,ug/m3, PM25_NO3,,PM25_NO3"
 setenv AERO_11 "NH4f_val,ug/m3, PM25_NH4,,PM25_NH4"
 setenv AERO_12 "88370adj_val,ug/m3, PM25_OC,,PM25_OC"
 setenv AERO_13 "88380_val,ug/m3, PM25_EC,,PM25_EC"
 setenv AERO_14 "88370adj_val+88380_val,ug/m3,PM25_OC+PM25_EC,,PM25_TC"
 setenv AERO_15 "88502_val,ug/m3,PM25_TOT,ug/m3,PM25_TOT"
 setenv AERO_16 "88502_val,ug/m3,PM25_FRM,ug/m3,PM25_FRM"
   
#> AERO6 species
#> note we use Sodium Ion instead of sodium (XRF) becasue XRF is not reliable for sodium
#> all other elemental concentrations (including Cl and K) come from XRF
 setenv AERO_17 "NAf_val,ug/m3, ANAIJ,,Na"
 setenv AERO_18 "CLf_val,ug/m3, ACLIJ,,Cl"
 setenv AERO_19 "FEf_val,ug/m3, AFEJ,,Fe"
 setenv AERO_20 "ALf_val,ug/m3,AALJ,,Al"
 setenv AERO_21 "SIf_val,ug/m3, ASIJ,,Si"
 setenv AERO_22 "TIf_val,ug/m3, ATIJ,,Ti"
 setenv AERO_23 "CAf_val,ug/m3,ACAJ,,Ca"
 setenv AERO_24 "MGf_val,ug/m3,AMGJ,,Mg"
 setenv AERO_25 "Kf_val,ug/m3,AKJ,,K"
 setenv AERO_26 "MNf_val,ug/m3,AMNJ,,Mn"
 setenv AERO_27 "2.2*ALf_val+2.49*SIf_val+1.63*CAf_val+2.42*FEf_val+1.94*TIf_val,ug/m3,ASOILJ,,soil"
 setenv AERO_28 "NAf_val + CLf_val, ug/m3, ANAIJ+ACLIJ,,NaCl"
 setenv AERO_29 "88502_val-SO4f_val-NO3f_val-NH4f_val-88370adj_val-88380_val-[NAf_val]-[CLf_val]-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val , ug/m3, AUNSPEC1IJ,,OTHER"
 setenv AERO_30 "0.8*88370adj_val,ug/m3, ANCOMIJ,,NCOM"
 setenv AERO_31 "88502_val-SO4f_val-NO3f_val-NH4f_val-88370adj_val-88380_val-[NAf_val]-[CLf_val]-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val-0.8*88370adj_val,ug/m3, AUNSPEC2IJ,,OTHER_REM"
  
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
 setenv IN_TABLE CSN_data_2011_VIEWS.csv

#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE CSN_CMAQ_v52.csv

#> Executable call:
 ${BINDIR}/${EXEC}
   
 exit()
