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
 
#> GAS Variables (1-10)  - compute average over time
#> Model output was originally in ppm, but conversions were already
#> made in the combine extract to convert to ug/m3.
 setenv GAS_1 "nhno3,ug/m3,HNO3_UGM3,,HNO3"                           # nitric acid
 setenv GAS_2 "total_so2,ug/m3,SO2_UGM3,,SO2"                         # sulfur dioxide (total SO2 = Whatman Filter + 0.667*Nylon Filter)
 setenv GAS_3 "1.15*total_so2,ug/m3,SO2_UGM3,,SO2_adj"                # adjusted SO2 value to account for observation bias (experimental)

#> AEROSOL Variables  - compute average over time
 setenv AERO_1 "tso4,ug/m3,ASO4IJ,ug/m3,SO4"                          # sulfate
 setenv AERO_2 "tno3,ug/m3,ANO3IJ,ug/m3,NO3"                          # nitrate
 setenv AERO_3 "tnh4,ug/m3,ANH4IJ,ug/m3,NH4"                          # ammonium
 setenv AERO_4 "tno3+nhno3,ug/m3,ANO3IJ+HNO3_UGM3,ug/m3,TNO3"         # total nitrate

#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
 setenv AERO_5 "tso4,ug/m3,PM25_SO4,ug/m3,PM25_SO4"                   # sulfate using sharp cutoff
 setenv AERO_6 "tno3,ug/m3,PM25_NO3,ug/m3,PM25_NO3"                   # nitrate using sharp cutoff
 setenv AERO_7 "tnh4,ug/m3,PM25_NH4,ug/m3,PM25_NH4"                   # ammonium using sharp cutoff
 setenv AERO_8 "tno3+nhno3,ug/m3,PM25_NO3+HNO3_UGM3,ug/m3,PM25_TNO3"  # total nitrate using sharp cutoff

#> AERO6 species
 setenv AERO_9 "MG,ug/m3,AMGJ,ug/m3,MG"                               # ammonium using sharp cutoff
 setenv AERO_10 "CA,ug/m3,ACAJ,ug/m3,CA"                              # calcium using sharp cutoff
 setenv AERO_11 "K,ug/m3,AKJ,ug/m3,K"                                 # potassium using sharp cutoff
 setenv AERO_12 "NA,ug/m3,ANAIJ,ug/m3,NA"                             # sodium using sharp cutoff
 setenv AERO_13 "CL,ug/m3,ACLIJ,ug/m3,CL"                             # chloride using sharp cutoff
  
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
 setenv SITE_FILE CASTNET_sites.txt

#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available 
#> from the CMAS Center Data clearinghouse under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> Hourly AQS observations are located in AMET12_OBSDATA_YYYY.tar.gz for year YYYY.
 setenv IN_TABLE CASTNET_weekly_data_2011.csv

#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE CASTNET_Hourly_CMAQ_v52.csv

#> Executable call:
 ${BINDIR}/${EXEC}
   
 exit()

