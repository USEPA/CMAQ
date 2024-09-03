#! /bin/csh -f

# ===================== SITECMP_v5.5.X Run Script ===================
# Usage: run_sitecmp_NADP.csh >&! sitecmp_NADP.log &
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
 setenv TABLE_TYPE NADP

#> Specify the variable names used in your observation inputs
#> and model output files for each of the species you are analyzing below.
#>
#> variable format:
#>    Obs_expression, Obs_units, [Mod_expression], [Mod_unit], [Variable_name]
#>
#> The expression is in the form:
#>       [factor1]*Obs_name1 [+][-] [factor2]*Obs_name2 ...
 setenv CHAR_1 "Valcode"       
 setenv CHAR_2 "Invalcode"     

# Wet Concentration Variables (1-10) - compute volume-weighted average (VWAVG) in mg/l
# Observed values are already volume-weighted averages for the collection
# period.  Original model output is hourly wet deposition. To calculate
# VWAVG, the modeled wet deposition is accumulated for the collection time
# period, divided by the total precipitation (mm), and * 100. Resultingi
# units are mg/l.

 setenv WETCON_1 "NH4,mg/l,WDEP_NHX,mg/l,NH4_conc" 
 setenv WETCON_2 "NO3,mg/l,WDEP_TNO3,mg/l,NO3_conc" 
 setenv WETCON_3 "SO4,mg/l,WDEP_TSO4,mg/l,SO4_conc" 
 setenv WETCON_4 "Cl,mg/l,WDEP_TCL,mg/l,Cl_conc" 
 setenv WETCON_5 "Na,mg/l,WDEP_ANAJK,mg/l,Na_conc" 

# Wet Deposition Variables (1-10) - compute accumulated wet deposition in kg/ha
# Observed values are volume-weighted average wet concentrations for thei
# collection period (mg/l). To convert to wet deposition, multiply the wet
# concentration values by the total observed precip (Sub Ppt in mm), and then
# divide by 100. Original model output is hourly wet deposition. The modeled
# wet deposition is accumulated for the collection time period.

 setenv WETDEP_1 "NH4,kg/ha,WDEP_NHX,kg/ha,NH4_dep" # Ammonium wet deposition
 setenv WETDEP_2 "NO3,kg/ha,WDEP_TNO3,kg/ha,NO3_dep" # Nitrate wet deposition
 setenv WETDEP_3 "SO4,kg/ha,WDEP_TSO4,kg/ha,SO4_dep" # Sulfate wet deposition 
 setenv WETDEP_4 "Cl,kg/ha,WDEP_TCL,kg/ha,Cl_dep" # Chloride wet deposition 
 setenv WETDEP_5 "Na,kg/ha,WDEP_ANAJK,kg/ha,Na_dep" # Sodium wet deposition

# Precipitation Variables (1-10) - compute accumulated precipitation

 setenv PREC_1 "Sub Ppt,mm,10*RT,mm,Precip"

# setenv AERO6 species
 setenv WETCON_6 "Ca,mg/l,WDEP_CAJK,mg/l,CA_conc" 
 setenv WETCON_7 "Mg,mg/l,WDEP_MGJK,mg/l,MG_conc" 
 setenv WETCON_8 "K,mg/l,WDEP_KJK,mg/l,K_conc"

 setenv WETDEP_6 "Ca,kg/ha,WDEP_CAJK,kg/ha,CA_dep" 
 setenv WETDEP_7 "Mg,kg/ha,WDEP_MGJK,kg/ha,MG_dep" 
 setenv WETDEP_8 "K,kg/ha,WDEP_KJK,kg/ha,K_dep"
 
#>> End Species List <<#

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
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_DEP_${RUNID}_201607.nc
         #[Add location of input file, e.g. COMBINE_DEP file.]

#> SITE FILE containing site-id, longitude, latitude, and optionally 
#> GMT offset, state, county, and elevation (csv format)
#> The column headings for the required variables need to be 
#> stat_id, lon, and lat (case insensitive)
#> The column headings for the optional variables need to be
#> gmt_offset, state, county, and elevation (case insensitive)
#> See the README.md file in this folder for the information on 
#> where to download this file.
 setenv SITE_FILE NADP_full_site_list.csv
#> On EPA system:
#  setenv SITE_FILE /work/MOD3EVAL/aq_obs/routine/site_metadata_files/NADP_full_site_list.csv

#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available online.
#> See the README.md file in this folder for the information on 
#> where to download this file.
 setenv IN_TABLE NADP_data_2016.csv
#> One EPA system:
#  setenv IN_TABLE /work/MOD3EVAL/aq_obs/routine/2016/NADP_data_2016.csv
#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE ${POSTDIR}/NADP_CMAQ_${RUNID}_201607.csv

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif
   
 exit()
