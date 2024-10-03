#! /bin/csh -f

# ===================== SITECMP_v5.5.X Run Script =====================
# Usage: run_sitecmp_IMPROVE.csh >&! sitecmp_IMPROVE.log &
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
 setenv TABLE_TYPE IMPROVE

#> Specify the variable names used in your observation inputs
#> and model output files for each of the species you are analyzing below.
#>
#> variable format:
#>    Obs_expression, Obs_units, [Mod_expression], [Mod_unit], [Variable_name]
#>
#> The expression is in the form:
#>       [factor1]*Obs_name1 [+][-] [factor2]*Obs_name2 ...
 setenv AERO_1 "SO4f_val,ug/m3,ASO4IJ,,SO4"                         # sulfate
 setenv AERO_2 "NO3f_val,ug/m3,ANO3IJ,,NO3"                         # nitrate
 setenv AERO_3 "0.2903*NO3f_val+0.375*SO4f_val,ug/m3,ANH4IJ,,NH4"   # ammonium (estimated assuming fully neutralized SO4 and NO3)
 setenv AERO_4 "MF_val,ug/m3,ATOTIJ,ug/m3,PM_TOT"          # Total PM2.5 mass 
 setenv AERO_5 "OCf_val,ug/m3,AOCIJ,,OC"                            # Organic Carbon
 setenv AERO_6 "ECf_val,ug/m3,AECIJ,,EC"                            # Elemental Carbon
 setenv AERO_7 "OCf_val+ECf_val,ug/m3,AOCIJ+AECIJ,,TC"              # Total Carbon
 setenv AERO_8 "CHLf_val,ug/m3,ACLIJ,ug/m3,Cl"                      # CL Ion
 setenv AERO_9 "MT_val,ug/m3,ATOTIJK,ug/m3,PM10"              # PM10
 setenv AERO_10 "CM_calculated_val,ug/m3,ATOTK,ug/m3,PMC_TOT"     # PM Course

#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using setenv CCTM_AELMO file
 setenv AERO_11 "SO4f_val,ug/m3,PM25_SO4,,PM25_SO4"                 	# sulfate (< 2.5um)
 setenv AERO_12 "NO3f_val,ug/m3,PM25_NO3,,PM25_NO3"                 	# nitrate (< 2.5um)
 setenv AERO_13 "0.2903*NO3f_val+0.375*SO4f_val,ug/m3,PM25_NH4,,PM25_NH4"	# ammonium (< 2.5um)
 setenv AERO_14 "OCf_val,ug/m3,PM25_OC,,PM25_OC"                    	# Organic Carbon (< 2.5um)
 setenv AERO_15 "ECf_val,ug/m3,PM25_EC,,PM25_EC"                    	# Elemental Carbon (< 2.5um)
 setenv AERO_16 "OCf_val+ECf_val,ug/m3,PM25_OC+PM25_EC,,PM25_TC"    	# Total Carbon (< 2.5um)
 setenv AERO_17 "MF_val,ug/m3,PM25_TOT,ug/m3,PM25_TOT"              	# Total PM2.5 mass (< 2.5um)
 setenv AERO_18 "CHLf_val,ug/m3,PM25_CL,ug/m3,PM25_Cl"              	# CL Ion (< 2.5um)
 setenv AERO_19  "CM_calculated_val,ug/m3,PMC_TOT,ug/m3,PMC_TOT_CUT" # PM Course

#> new AE6 species
#> note: we use XRF sodium because there is not IC sodium mesaurement
#> we use IC measurement for chlorid (CHLf_val) instead of XRF chlroine (CLf_Val)
 setenv AERO_20 "NAf_val,ug/m3, ANAIJ,,Na"                          # sodium
 setenv AERO_21 "NAf_val + CHLf_val,ug/m3,ACLIJ + ANAIJ,,NaCl"      # sodium chloride
 setenv AERO_22 "FEf_val,ug/m3, AFEJ,,Fe"                           # iron
 setenv AERO_23 "ALf_val,ug/m3,AALJ,,Al"                            # aluminum 
 setenv AERO_24 "SIf_val,ug/m3, ASIJ,,Si"                           # silicon
 setenv AERO_25 "TIf_val,ug/m3, ATIJ,,Ti"                           # titanium
 setenv AERO_26 "CAf_val,ug/m3,ACAJ,,Ca"                            # calcium
 setenv AERO_27 "MGf_val,ug/m3,AMGJ,,Mg"                            # magnesium
 setenv AERO_28 "Kf_val,ug/m3,AKJ,,K"                               # potassium
 setenv AERO_29 "MNf_val,ug/m3,AMNJ,,Mn"                            # manganese
 setenv AERO_30 "2.20*ALf_val+2.49*SIf_val+1.63*CAf_val+2.42*FEf_val+1.94*TIf_val,ug/m3,ASOILJ,,soil"       # IMPROVE soil eqn.
 setenv AERO_31 "MF_val-SO4f_val-NO3f_val-0.2903*NO3f_val-0.375*SO4f_val-OCf_val-ECf_val-NAf_val-CHLf_val-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val,ug/m3,AUNSPEC1IJ,,OTHER"        # PM Other
 setenv AERO_32 "0.8*OCf_val,ug/m3, ANCOMIJ,,NCOM"    # NCOM
 setenv AERO_33 "MF_val-SO4f_val-NO3f_val-0.2903*NO3f_val-0.375*SO4f_val-OCf_val-ECf_val-NAf_val-CHLf_val-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val,ug/m3, AUNSPEC2IJ,,OTHER_REM"    # PM Other remaining
 
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
 setenv SITE_FILE IMPROVE_full_site_list.csv
#> On EPA system:
#  setenv SITE_FILE /work/MOD3EVAL/aq_obs/routine/site_metadata_files/IMPROVE_full_site_list.csv


#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available online.
#> See the README.md file in this folder for the information on 
#> where to download this file.
 setenv IN_TABLE IMPROVE_data_2016.csv
#> One EPA system:
#  setenv IN_TABLE /work/MOD3EVAL/aq_obs/routine/2016/IMPROVE_data_2016.csv


#############################################################
#  Output files
#############################################################

#> output table (comma delimited text file importable to Excel)
 setenv OUT_TABLE ${POSTDIR}/IMPROVE_CMAQ_${RUNID}_201607.csv

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif
   
 exit()
