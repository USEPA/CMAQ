# Species List for sitecmp:  Matching model and observed variables

The environment variables that control how CMAQ and observed values are matched using sitecmp will change depending on the observation network. Sitecmp requires observation data to be in a specific format. Sitecmp-ready observation data files going back to 2000 are available on the CMAS Data Warehouse Google Drive: [North America Air Quaility Observation Files](https://drive.google.com/drive/folders/1QUlUXnHXvXz9qwePi5APzzHkiH5GWACw?usp=drive_link). Data files are provided in AMET_OBSDATA_YYYY.tar.gz for year YYYY.  These should be used when setting environment variable IN_TABLE. A site meta data file (SITE_FILE) is also needed for sitecmp.  These can be found within the .tar.gz files under the *site_metadata_files* folder. 

The variable names in these observation files can change over time.  This README file provides the environment variables needed for the species list in the sitecmp run script for different networks and years. 

This species information was generated using the [Atmospheric Model Evaluation Tool (AMET)](https://github.com/USEPA/AMET) which is also available github.com/USEPA. For further information on the mapping of observation species from each network to CMAQ model species, please see the section **AQ Species List Input File** of the [AMETv1.4 User's Guide](https://github.com/USEPA/AMET/blob/1.4b/docs/AMET_Users_Guide_v14.md).

Note that the configuration options below are just for the species definition part of the sitecmp run script.  See the run scripts within this folder for AQS, CSN, IMPROVE, NADP and SEARCH for examples of complete run scripts for sitecmp that include all necessary environment variables. 

### SITECMP Configuration Options for AERONET
```
# =====================================================================
#> SITECMP Species List Configuration Options for AERONET
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
 
 setenv AERO_1 "AOT_340,none,SW_AOD_340,none,AOD_340" 
 setenv AERO_2 "AOT_380,none,SW_AOD_380,none,AOD_380"
 setenv AERO_3 "AOT_440,none,SW_AOD_440,none,AOD_440"
 setenv AERO_4 "AOT_500,none,SW_AOD_500,none,AOD_500"
 setenv AERO_5 "AOT_555,none,SW_AOD_555,none,AOD_555"
 setenv AERO_6 "AOT_675,none,SW_AOD_675,none,AOD_675"
 setenv AERO_7 "AOT_870,none,SW_AOD_870,none,AOD_870"
 setenv AERO_8 "AOT_1020,none,SW_AOD_1020,none,AOD_1020"
 setenv AERO_9 "AOT_1640,none,SW_AOD_1640,none,AOD_1640"

#>> End Species List <<#

#> file containing meta data (e.g., site-id, longitude, latitude, time zone) about each site.
 setenv SITE_FILE AERONET_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE AERONET_hourly_data_2011.csv

# =====================================================================
#> END Options for AERONET
# =====================================================================
```

### SITECMP Configuration Options for AMON
```
# =====================================================================
#> SITECMP Species List Configuration Options for AMON
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
  setenv AERO_1 "NH3,ug/m3,NH3_UGM3,ug/m3,NH3"                        # Ammonia
 
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE AMON_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE AMON_data.csv

# =====================================================================
#> END Options for AMON
# =====================================================================
```

### SITECMP Configuration Options for Hourly CASTNET data
```
# =====================================================================
#> SITECMP Species List Configuration Options for Hourly CASTNET data
# =====================================================================

#> Set TABLE TYPE
 setenv TABLE_TYPE MET

#> Specify the variable names used in your observation inputs
#> and model output files for each of the species you are analyzing below.
#>
#> variable format:
#>    Obs_expression, Obs_units, [Mod_expression], [Mod_unit], [Variable_name]
#>
#> The expression is in the form:
#>       [factor1]*Obs_name1 [+][-] [factor2]*Obs_name2 ...
 setenv GAS_1 "ozone,ppb,O3,ppb,O3"
 setenv GAS_2 "temperature,C,SFC_TMP,C,SFC_TMP"
 setenv GAS_3 "relative_humidity,%,RH,%,RH"
 setenv GAS_4 "solar_radiation,watts/m2,SOL_RAD,watts/m2,Solar_Rad"
 setenv GAS_5 "precipitation,mm/hr,precip,mm/hr,precip"
 setenv GAS_6 "windspeed,m/s,WSPD10,m/s,WSPD10"
# setenv GAS_7 "wind_direction,deg,WDIR10,deg,WDIR10"
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE CASTNET_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE CASTNET_hourly_data_2011.csv

# =====================================================================
#> END Options for Hourly CASTNET
# =====================================================================
```
### SITECMP Configuration Options for Weekly CASTNET data
```
# =====================================================================
#> SITECMP Species List Configuration Options for Weekly CASTNET data
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

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE CASTNET_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE CASTNET_weekly_data_2011.csv

# =====================================================================
#> END Options for Weekly CASTNET
# =====================================================================
```
### Runtime Environment Options for CSN (formally STN) 
#### CSN for 2009 and earlier
```
# ==================================================================
#> Runtime Species List Environment Options for CSN (formally STN) 
#
#> There are three formats of the CSN observed data .csv files.  
#> The number of columns and the column names changes across the 
#> time series provided (2001-2009, 2010, 2011-2013, 2014 and later).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> CSN for 2009 and earlier
# ==================================================================

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
 setenv AERO_1 "m_so4,ug/m3, ASO4IJ,,SO4"
 setenv AERO_2 "m_no3,ug/m3, ANO3IJ,,NO3"
 setenv AERO_3 "m_nh4,ug/m3, ANH4IJ,,NH4"
 setenv AERO_4 "FRM PM2.5 Mass,ug/m3,ATOTIJ,,PM_TOT"
 setenv AERO_5 "FRM PM2.5 Mass,ug/m3,PMIJ_FRM,,PM_FRM"
 setenv AERO_6 "oc_adj,ug/m3, AOCIJ,,OC"
 setenv AERO_7 "ec_niosh,ug/m3, AECIJ,,EC"
 setenv AERO_8 "oc_adj+ec_niosh,ug/m3,AOCIJ+AECIJ,,TC"

## PM2.5 Sharp Cutoff Species
## Requires preprocessing using AERODIAM file
 setenv AERO_9 "m_so4,ug/m3, PM25_SO4,,PM25_SO4"
 setenv AERO_10 "m_no3,ug/m3, PM25_NO3,,PM25_NO3"
 setenv AERO_11 "m_nh4,ug/m3, PM25_NH4,,PM25_NH4"
 setenv AERO_12 "oc_adj,ug/m3, PM25_OC,,PM25_OC"
 setenv AERO_13 "ec_niosh,ug/m3, PM25_EC,,PM25_EC"
 setenv AERO_14 "oc_adj+ec_niosh,ug/m3,PM25_OC+PM25_EC,,PM25_TC"
 setenv AERO_15 "FRM PM2.5 Mass,ug/m3,PM25_TOT,ug/m3,PM25_TOT"
 setenv AERO_16 "FRM PM2.5 Mass,ug/m3,PM25_FRM,ug/m3,PM25_FRM"

# AERO6 species
## note we use Sodium Ion instead of sodium (XRF) becasue XRF is not reliable for sodium
## all other elemental concentrations (including Cl and K) come from XRF
 setenv AERO_17 "Sodium Ion,ug/m3, ANAIJ,,Na"
 setenv AERO_18 "chlorine,ug/m3, ACLIJ,,Cl"
 setenv AERO_19 "iron,ug/m3, AFEJ,,Fe"
 setenv AERO_20 "aluminum,ug/m3,AALJ,,Al"
 setenv AERO_21 "silicon,ug/m3, ASIJ,,Si"
 setenv AERO_22 "titanium,ug/m3, ATIJ,,Ti"
 setenv AERO_23 "calcium,ug/m3,ACAJ,,Ca"
 setenv AERO_24 "magnesium,ug/m3,AMGJ,,Mg"
 setenv AERO_25 "potassium,ug/m3,AKJ,,K"
 setenv AERO_26 "manganese,ug/m3,AMNJ,,Mn"
 setenv AERO_27 "2.2*aluminum+2.49*silicon+1.63*calcium+2.42*iron+1.94*titanium,ug/m3,ASOILJ,,soil"
 setenv AERO_28 "Sodium Ion + chlorine, ug/m3, ANAIJ+ACLIJ,,NaCl"
 setenv AERO_29 "FRM PM2.5 Mass - m_so4 - m_no3 - m_nh4 - oc_adj - ec_niosh - [Sodium Ion] - [chlorine] - 2.2*aluminum - 2.49*silicon - 1.63*calcium - 2.42*iron - 1.94*titanium , ug/m3, AUNSPEC1IJ,,OTHER"
 setenv AERO_30 "0.8*oc_adj, ug/m3, ANCOMIJ,,NCOM"
 setenv AERO_31 "FRM PM2.5 Mass - m_so4 - m_no3 - m_nh4 - oc_adj - ec_niosh - [Sodium Ion] - [chlorine] - 2.2*aluminum - 2.49*silicon - 1.63*calcium - 2.42*iron - 1.94*titanium - 0.8*oc_adj, ug/m3, AUNSPEC2IJ,,OTHER_REM"
 
#>> End Species List <<#


#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE AQS_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE CSN_hourly_data_2009.csv
```

#### CSN for 2010
```
# ==================================================================
#> Runtime Species List Environment Options for CSN (formally STN) 
#
#> There are three formats of the CSN observed data .csv files.  
#> The number of columns and the column names changes across the 
#> time series provided (2001-2009, 2010, 2011-2013, 2014 and later).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> CSN for 2010
# ==================================================================

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
 setenv AERO_1 "m_so4,ug/m3, ASO4IJ,,SO4"
 setenv AERO_2 "m_no3,ug/m3, ANO3IJ,,NO3"
 setenv AERO_3 "m_nh4,ug/m3, ANH4IJ,,NH4"
 setenv AERO_4 "PM2.5 Mass,ug/m3,ATOTIJ,,PM_TOT"
 setenv AERO_5 "PM2.5 Mass,ug/m3,PMIJ_FRM,,PM_FRM"
 setenv AERO_6 "oc_adj,ug/m3, AOCIJ,,OC"
 setenv AERO_7 "ec_tor,ug/m3, AECIJ,,EC"
 setenv AERO_8 "oc_adj+ec_tor,ug/m3,AOCIJ+AECIJ,,TC"

## PM2.5 Sharp Cutoff Species
## Requires preprocessing using AERODIAM file
 setenv AERO_9 "m_so4,ug/m3, PM25_SO4,,PM25_SO4"
 setenv AERO_10 "m_no3,ug/m3, PM25_NO3,,PM25_NO3"
 setenv AERO_11 "m_nh4,ug/m3, PM25_NH4,,PM25_NH4"
 setenv AERO_12 "oc_adj,ug/m3, PM25_OC,,PM25_OC"
 setenv AERO_13 "ec_tor,ug/m3, PM25_EC,,PM25_EC"
 setenv AERO_14 "oc_adj+ec_tor,ug/m3,PM25_OC+PM25_EC,,PM25_TC"
 setenv AERO_15 "PM2.5 Mass,ug/m3,PM25_TOT,ug/m3,PM25_TOT"
 setenv AERO_16 "PM2.5 Mass,ug/m3,PM25_FRM,ug/m3,PM25_FRM"

# AERO6 species
## note we use Sodium Ion instead of sodium (XRF) becasue XRF is not reliable for sodium
## all other elemental concentrations (including Cl and K) come from XRF
 setenv AERO_17 "Sodium Ion,ug/m3, ANAIJ,,Na"
 setenv AERO_18 "chlorine,ug/m3, ACLIJ,,Cl"
 setenv AERO_19 "iron,ug/m3, AFEJ,,Fe"
 setenv AERO_20 "aluminum,ug/m3,AALJ,,Al"
 setenv AERO_21 "silicon,ug/m3, ASIJ,,Si"
 setenv AERO_22 "titanium,ug/m3, ATIJ,,Ti"
 setenv AERO_23 "calcium,ug/m3,ACAJ,,Ca"
 setenv AERO_24 "magnesium,ug/m3,AMGJ,,Mg"
 setenv AERO_25 "potassium,ug/m3,AKJ,,K"
 setenv AERO_26 "manganese,ug/m3,AMNJ,,Mn"
 setenv AERO_27 "2.2*aluminum+2.49*silicon+1.63*calcium+2.42*iron+1.94*titanium,ug/m3,ASOILJ,,soil"
 setenv AERO_28 "Sodium Ion + chlorine, ug/m3, ANAIJ+ACLIJ,,NaCl"
 setenv AERO_29 "PM2.5 Mass - m_so4 - m_no3 - m_nh4 - oc_adj - ec_tor - [Sodium Ion] - [chlorine] - 2.2*aluminum - 2.49*silicon - 1.63*calcium - 2.42*iron - 1.94*titanium , ug/m3, AUNSPEC1IJ,,OTHER"
 setenv AERO_30 "0.8*oc_adj, ug/m3, ANCOMIJ,,NCOM"
 setenv AERO_31 "PM2.5 Mass - m_so4 - m_no3 - m_nh4 - oc_adj - ec_niosh - [Sodium Ion] - [chlorine] - 2.2*aluminum - 2.49*silicon - 1.63*calcium - 2.42*iron - 1.94*titanium - 0.8*oc_adj, ug/m3, AUNSPEC2IJ,,OTHER_REM"
 
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE AQS_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE CSN_hourly_data_2010.csv
```

#### CSN for 2011-2013
```
# ==================================================================
#> Runtime Species List Environment Options for CSN (formally STN) 
#
#> There are three formats of the CSN observed data .csv files.  
#> The number of columns and the column names changes across the 
#> time series provided (2001-2009, 2010, 2011-2013, 2014 and later).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> CSN for 2011-2013
# ==================================================================

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
 setenv AERO_4 "88502_val,ug/m3,ATOTIJ,,PM_TOT"
 setenv AERO_5 "88502_val,ug/m3,PMIJ_FRM,,PM_FRM"
 setenv AERO_6 "88370_val-blank,ug/m3, AOCIJ,,OC"
 setenv AERO_7 "88380_val,ug/m3, AECIJ,,EC"
 setenv AERO_8 "88370_val-blank+88380_val,ug/m3,AOCIJ+AECIJ,,TC"
   
#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
 setenv AERO_9 "SO4f_val,ug/m3, PM25_SO4,,PM25_SO4"
 setenv AERO_10 "NO3f_val,ug/m3, PM25_NO3,,PM25_NO3"
 setenv AERO_11 "NH4f_val,ug/m3, PM25_NH4,,PM25_NH4"
 setenv AERO_12 "88370_val-blank,ug/m3, PM25_OC,,PM25_OC"
 setenv AERO_13 "88380_val,ug/m3, PM25_EC,,PM25_EC"
 setenv AERO_14 "88370_val-blank+88380_val,ug/m3,PM25_OC+PM25_EC,,PM25_TC"
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
 setenv AERO_29 "88502_val-SO4f_val-NO3f_val-NH4f_val-88370_val-blank-88380_val-[NAf_val]-[CLf_val]-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val , ug/m3, AUNSPEC1IJ,,OTHER"
 setenv AERO_30 "0.8*88370_val-blank,ug/m3, ANCOMIJ,,NCOM"
 setenv AERO_31 "88502_val-SO4f_val-NO3f_val-NH4f_val-88370_val-blank-88380_val-[NAf_val]-[CLf_val]-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val-0.8*88370_val-blank,ug/m3, AUNSPEC2IJ,,OTHER_REM"
  
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE AQS_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE CSN_data_2011_VIEWS.csv
```

#### CSN for 2014 and later
```
# ==================================================================
#> Runtime Species List Environment Options for CSN (formally STN) 
#
#> There are three formats of the CSN observed data .csv files.  
#> The number of columns and the column names changes across the 
#> time series provided (2001-2009, 2010, 2011-2013, 2014 and later).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> CSN for 2014 and later
# ==================================================================

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
 setenv AERO_4 "PM25,ug/m3,ATOTIJ,,PM_TOT"
 setenv AERO_5 "PM25,ug/m3,PMIJ_FRM,,PM_FRM"
 setenv AERO_6 "88370_val-blank,ug/m3, AOCIJ,,OC"
 setenv AERO_7 "88380_val,ug/m3, AECIJ,,EC"
 setenv AERO_8 "88370_val-blank+88380_val,ug/m3,AOCIJ+AECIJ,,TC"
   
#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
 setenv AERO_9 "SO4f_val,ug/m3, PM25_SO4,,PM25_SO4"
 setenv AERO_10 "NO3f_val,ug/m3, PM25_NO3,,PM25_NO3"
 setenv AERO_11 "NH4f_val,ug/m3, PM25_NH4,,PM25_NH4"
 setenv AERO_12 "88370_val-blank,ug/m3, PM25_OC,,PM25_OC"
 setenv AERO_13 "88380_val,ug/m3, PM25_EC,,PM25_EC"
 setenv AERO_14 "88370_val-blank+88380_val,ug/m3,PM25_OC+PM25_EC,,PM25_TC"
 setenv AERO_15 "PM25,ug/m3,PM25_TOT,ug/m3,PM25_TOT"
 setenv AERO_16 "PM25,ug/m3,PM25_FRM,ug/m3,PM25_FRM"
   
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
 setenv AERO_29 "PM25-SO4f_val-NO3f_val-NH4f_val-88370_val-blank-88380_val-[NAf_val]-[CLf_val]-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val , ug/m3, AUNSPEC1IJ,,OTHER"
 setenv AERO_30 "0.8*88370_val-blank,ug/m3, ANCOMIJ,,NCOM"
 setenv AERO_31 "PM25-SO4f_val-NO3f_val-NH4f_val-88370_val-blank-88380_val-[NAf_val]-[CLf_val]-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val-0.8*88370_val-blank,ug/m3, AUNSPEC2IJ,,OTHER_REM"
  
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE AQS_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE CSN_data_2011_VIEWS.csv

# =====================================================================
#> END Options for CSN
# =====================================================================
```

### SITECMP Configuration Options for IMPROVE
```
# =====================================================================
#> SITECMP Species List Configuration Options for IMPROVE
# =====================================================================
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
  setenv AERO_1 "SO4f_val,ug/m3,ASO4IJ,,SO4"                        # sulfate
  setenv AERO_2 "NO3f_val,ug/m3,ANO3IJ,,NO3"                        # nitrate
  setenv AERO_3 "0.2903*NO3f_val+0.375*SO4f_val,ug/m3,ANH4IJ,,NH4"  # ammonium (estimated assuming fully neutralized SO4 and NO3)
  setenv AERO_4 "MF_val,ug/m3,ATOTIJ,ug/m3,PM_TOT"                    # Total PM2.5 mass 
  setenv AERO_5 "OCf_val,ug/m3,AOCIJ,,OC"                           # Organic Carbon
  setenv AERO_6 "ECf_val,ug/m3,AECIJ,,EC"                           # Elemental Carbon
  setenv AERO_7 "OCf_val+ECf_val,ug/m3,AOCIJ+AECIJ,,TC"             # Total Carbon
  setenv AERO_8 "CHLf_val,ug/m3,ACLIJ,ug/m3,Cl"                     # CL Ion
  setenv AERO_9 "MT_val,ug/m3,PM10,ug/m3,PM10"                      # PM10
  setenv AERO_10 "CM_calculated_val,ug/m3,PMC_TOT,ug/m3,PMC_TOT"    # PM Course

#> PM2.5 Sharp Cutoff Species
#> Requires preprocessing using AERODIAM file
  setenv AERO_11 "SO4f_val,ug/m3,PM25_SO4,,PM25_SO4"                        # sulfate (< 2.5um)
  setenv AERO_12 "NO3f_val,ug/m3,PM25_NO3,,PM25_NO3"                 	    # nitrate (< 2.5um)
  setenv AERO_13 "0.2903*NO3f_val+0.375*SO4f_val,ug/m3,PM25_NH4,,PM25_NH4"  # ammonium (< 2.5um)
  setenv AERO_14 "OCf_val,ug/m3,PM25_OC,,PM25_OC"                    	    # Organic Carbon (< 2.5um)
  setenv AERO_15 "ECf_val,ug/m3,PM25_EC,,PM25_EC"                    	    # Elemental Carbon (< 2.5um)
  setenv AERO_16 "OCf_val+ECf_val,ug/m3,PM25_OC+PM25_EC,,PM25_TC"    	    # Total Carbon (< 2.5um)
  setenv AERO_17 "MF_val,ug/m3,PM25_TOT,ug/m3,PM25_TOT"                     # Total PM2.5 mass (< 2.5um)
  setenv AERO_18 "CHLf_val,ug/m3,PM25_CL,ug/m3,PM25_Cl"                     # CL Ion (< 2.5um)

#> New AE6 species
#> note: we use XRF sodium because there is not IC sodium mesaurement
#> we use IC measurement for chlorid (CHLf_val) instead of XRF chlroine (CLf_Val)
  setenv AERO_19 "NAf_val,ug/m3, ANAIJ,,Na"                            # sodium
  setenv AERO_20 "NAf_val + CHLf_val,ug/m3,ACLIJ + ANAIJ,,NaCl"        # sodium chloride
  setenv AERO_21 "FEf_val,ug/m3, AFEJ,,Fe"                             # iron
  setenv AERO_22 "ALf_val,ug/m3,AALJ,,Al"                              # aluminum 
  setenv AERO_23 "SIf_val,ug/m3, ASIJ,,Si"                             # silicon
  setenv AERO_24 "TIf_val,ug/m3, ATIJ,,Ti"                            # titanium
  setenv AERO_25 "CAf_val,ug/m3,ACAJ,,Ca"                              # calcium
  setenv AERO_26 "MGf_val,ug/m3,AMGJ,,Mg"                              # magnesium
  setenv AERO_27 "Kf_val,ug/m3,AKJ,,K"                                 # potassium
  setenv AERO_28 "MNf_val,ug/m3,AMNJ,,Mn"                              # manganese
  setenv AERO_29 "2.20*ALf_val+2.49*SIf_val+1.63*CAf_val+2.42*FEf_val+1.94*TIf_val,ug/m3,ASOILJ,,soil"  # IMPROVE soil eqn.
  setenv AERO_30 "MF_val-SO4f_val-NO3f_val-0.2903*NO3f_val-0.375*SO4f_val-OCf_val-ECf_val-NAf_val-CHLf_val-2.2*ALf_val-2.49*SIf_val-1.63*CAf_val-2.42*FEf_val-1.94*TIf_val,ug/m3,AUNSPEC1IJ,,OTHER"      # PM Other
  setenv AERO_31 ",ug/m3, ANCOMIJ,,NCOM"                              # PM Other
  setenv AERO_32 ",ug/m3, AUNSPEC2IJ,,OTHER_REM"                      # PM Other

#>> End Species List <<#


#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE IMPROVE_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE IMPROVE_data_2011.csv

# =====================================================================
#> END Options for IMPROVE
# =====================================================================
```

### SITECMP Configuration Options for NADP
```
# =====================================================================
#> SITECMP Species List Configuration Options for NADP
# =====================================================================
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

#> Wet Concentration Variables (1-10) - compute volume-weighted average (VWAVG) in mg/l
#> Observed values are already volume-weighted averages for the collection
#> period.  Original model output is hourly wet deposition. To calculate
#> VWAVG, the modeled wet deposition is accumulated for the collection time
#> period, divided by the total precipitation (mm), and * 100. Resultingi
#> units are mg/l.

 setenv WETCON_1 "NH4,mg/l,WDEP_NHX,mg/l,NH4_conc"
 setenv WETCON_2 "NO3,mg/l,WDEP_TNO3,mg/l,NO3_conc"
 setenv WETCON_3 "SO4,mg/l,WDEP_TSO4,mg/l,SO4_conc"
 setenv WETCON_4 "Cl,mg/l,WDEP_TCL,mg/l,Cl_conc"
 setenv WETCON_5 "Na,mg/l,WDEP_ANAJK,mg/l,Na_conc"

#> Wet Deposition Variables (1-10) - compute accumulated wet deposition in kg/ha
#> Observed values are volume-weighted average wet concentrations for thei
#> collection period (mg/l). To convert to wet deposition, multiply the wet
#> concentration values by the total observed precip (Sub Ppt in mm), and then
#> divide by 100. Original model output is hourly wet deposition. The modeled
#> wet deposition is accumulated for the collection time period.

 setenv WETDEP_1 "NH4,kg/ha,WDEP_NHX,kg/ha,NH4_dep"   # Ammonium wet deposition
 setenv WETDEP_2 "NO3,kg/ha,WDEP_TNO3,kg/ha,NO3_dep"  # Nitrate wet deposition
 setenv WETDEP_3 "SO4,kg/ha,WDEP_TSO4,kg/ha,SO4_dep"  # Sulfate wet deposition 
 setenv WETDEP_4 "Cl,kg/ha,WDEP_TCL,kg/ha,Cl_dep"     # Chloride wet deposition 
 setenv WETDEP_5 "Na,kg/ha,WDEP_ANAJK,kg/ha,Na_dep"   # Sodium wet deposition

#> Precipitation Variables (1-10) - compute accumulated precipitation

 setenv PREC_1 "Sub Ppt,mm,10*RT,mm,Precip"


#> AERO6 species
 setenv WETCON_6 "Ca,mg/l,WDEP_CAJK,mg/l,CA_conc"
 setenv WETCON_7 "Mg,mg/l,WDEP_MGJK,mg/l,MG_conc"
 setenv WETCON_8 "K,mg/l,WDEP_KJK,mg/l,K_conc"

 setenv WETDEP_6 "Ca,kg/ha,WDEP_CAJK,kg/ha,CA_dep"
 setenv WETDEP_7 "Mg,kg/ha,WDEP_MGJK,kg/ha,MG_dep"
 setenv WETDEP_8 "K,kg/ha,WDEP_KJK,kg/ha,K_dep"
 
#>> End Species List <<#

#> site containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE NADP_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE NADP_data_2011.csv


# =====================================================================
#> END Options for NADP
# =====================================================================
```

### Runtime Environment Options for Hourly SEARCH 
#### Hourly SEARCH for 2004 and earlier
```
# ==================================================================
#> Runtime Species List Environment Options for Hourly SEARCH 
#
#> There are four formats of the SEARCH observed hourly data .csv files.  
#> The number of columns and the column names changes across the time 
#> series provided (2001-2004, 2005, 2006-2010, 2011-2014).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> Hourly SEARCH for 2004 and earlier
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
 setenv AERO_1 "o3,ppb,O3,,O3"
 setenv AERO_2 "co,ppb,CO,,CO"
 setenv AERO_3 "so2,ppb,SO2,,SO2"
 setenv AERO_4 "no,ppb,NO,,NO"
 setenv AERO_5 "no2,ppb,NO2,,NO2"
 setenv AERO_6 "noy,ppb,NOY,ppb,NOY"
 setenv AERO_7 "wsp,m/s,WSPD10,m/s,WSPD10"
 setenv AERO_8 "wdr,m/s,WDIR10,m/s,WDIR10"
 setenv AERO_9 "rh,%,RH,%,RH"
 setenv AERO_10 "temp,C,SFC_TMP,C,SFC_TMP"
 setenv AERO_12 "sr,w/m2,SOL_RAD,watts/m2,Solar_Rad"
 
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE SEARCH_hourly_data_2004.csv

```
#### Hourly SEARCH for 2005
```
# ==================================================================
#> Runtime Environment Options for Hourly SEARCH 
#
#> There are four formats of the SEARCH observed hourly data .csv files.  
#> The number of columns and the column names changes across the time 
#> series provided (2001-2004, 2005, 2006-2010, 2011-2014).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================


# ==================================================================
#> Hourly Species List SEARCH for 2005 
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
 setenv AERO_1 "average o3[ppb],ppb,O3,,O3"
 setenv AERO_2 "average co[ppb],ppb,CO,,CO"
 setenv AERO_3 "average so2[ppb],ppb,SO2,,SO2"
 setenv AERO_4 "average no[ppb],ppb,NO,,NO"
 setenv AERO_5 "average no2[ppb],ppb,NO2,,NO2"
 setenv AERO_6 "average noy[ppb],ppb,NOY,ppb,NOY"
 setenv AERO_7 "average wsp[m/s],m/s,WSPD10,m/s,WSPD10"
 setenv AERO_8 "average wdr[deg],m/s,WDIR10,m/s,WDIR10"
 setenv AERO_9 "average rh[%],%,RH,%,RH"
 setenv AERO_10 "average temp[deg c],C,SFC_TMP,C,SFC_TMP"
 setenv AERO_11 "25.4*average rainfall[inches],mm,precip,mm,precip"
 setenv AERO_12 "average sr[w/m2],w/m2,SOL_RAD,watts/m2,Solar_Rad"
 
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE SEARCH_hourly_data_2005.csv
```

#### Hourly SEARCH for 2006 to 2010
```
# ==================================================================
#> Runtime Environment Options for Hourly SEARCH 
#
#> There are four formats of the SEARCH observed hourly data .csv files.  
#> The number of columns and the column names changes across the time 
#> series provided (2001-2004, 2005, 2006-2010, 2011-2014).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> Hourly Species List SEARCH for 2006 to 2010
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
 setenv AERO_18 "Average TEOM[ug/m3],ug/m3,ATOTIJ,,PM_TOT"
 setenv AERO_19 "Average NH4[ug/m3],ug/m3,ANH4IJ,,NH4"
 setenv AERO_20 "Average SO4[ug/m3],ug/m3,ASO4IJ,,SO4"
   
 setenv AERO_21 "Average OptEC[ug/m3],ug/m3,PM25_EC,ug/m3,PM25_EC"
 setenv AERO_22 "Average OC[ug/m3],ug/m3,PM25_OC,ug/m3,PM25_OC"
 setenv AERO_23 "Average TC[ug/m3],ug/m3,PM25_EC+PM25_OC,ug/m3,PM25_TC"
 setenv AERO_24 "Average NH4[ug/m3],ug/m3,PM25_NH4,,PM25_NH4"
 setenv AERO_25 "Average NO3[ug/m3],ug/m3,PM25_NO3,,PM25_NO3"
 setenv AERO_26 "Average SO4[ug/m3],ug/m3,PM25_SO4,,PM25_SO4"
 
#>> End Species List <<#

#> SITE FILE containing site-id, longitude, latitude, time zone (tab delimited)
#> This file can be downloaded from the CMAS Center Data clearinghouse 
#> under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> AQS site file is located in AMET12_SITE_FILES.tar.gz
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE SEARCH_hourly_data_2011.csv

```

#### Hourly SEARCH for 2011 and later
```
# ==================================================================
#> Runtime Environment Options for Hourly SEARCH 
#
#> There are four formats of the SEARCH observed hourly data .csv files.  
#> The number of columns and the column names changes across the time 
#> series provided (2001-2004, 2005, 2006-2010, 2011-2014).
#> As a result the species defintions with the run script for the 
#> sitecmp program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> Hourly Species List SEARCH for 2011 and later
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
 setenv AERO_9 "Average WSP[m/s],m/s,WSPD10,m/s,WSPD10"
 setenv AERO_10 "Average WDR[Deg],m/s,WDIR10,m/s,WDIR10";
 setenv AERO_11 "Average RH[%],%,RH,%,RH"
 setenv AERO_12 "Average TEMP[Deg C],C,SFC_TMP,C,SFC_TMP"
 setenv AERO_13 "25.4*Average RAINFALL[Inches],mm,precip,mm,precip"
 setenv AERO_14 "Average SR[W/m2],w/m2,SOL_RAD,watts/m2,Solar_Rad"
 
#>> End Species List <<#

#> site file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE SEARCH_hourly_data_2013.csv


# =====================================================================
#> END Options for Hourly SEARCH
# =====================================================================
```

###  Runtime Environment Options for Daily SEARCH 
#### Daily SEARCH for 2007 and earlier
```
# ==================================================================
#> Runtime Species List  Environment Options for Daily SEARCH 
#
#> There are two formats of the SEARCH observed daily data .csv files.  
#> The number of columns and the column names changes across the time 
#> series provided (2001-2007, 2008-2014).
#> As a result the species defintions with the run script for the sitecmp 
#> program need to be changed accordingly.
# ==================================================================

# ==================================================================
#> Daily SEARCH for 2007 and earlier
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
 setenv AERO_1 "pcm1 so4,ug/m3,ASO4IJ,ug/m3,SO4"
 setenv AERO_2 "pcm1 no3+pcm1 teflon no3,ug/m3,ANO3IJ,ug/m3,NO3"
 setenv AERO_3 "pcm1 teflon nh4+pcm1 vol nh4,ug/m3,ANH4IJ,ug/m3,NH4"
 setenv AERO_4 "pcm3 ec,ug/m3,AECIJ,ug/m3,EC"
 setenv AERO_5 "pcm3 oc,ug/m3,AOCIJ,ug/m3,OC"
 setenv AERO_6 "pcm1 mass,ug/m3,ATOTIJ,ug/m3,PM_TOT"
  
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
#> AQS obs data in the format needed for sitecmp are available 
#> from the CMAS Center Data clearinghouse under the heading "2000-2014 North American Air Quality Observation Data":
#> https://www.cmascenter.org/download/data.cfm
#> Hourly AQS observations are located in AMET12_OBSDATA_YYYY.tar.gz for year YYYY.
 setenv IN_TABLE SEARCH_daily_data_2007.csv
```

###  Runtime Environment Options for Daily SEARCH 
#### Daily SEARCH for 2008 and later
```
# ==================================================================
#> Runtime Environment Options for Daily SEARCH 
#
#> There are two formats of the SEARCH observed daily data .csv files.  
#> The number of columns and the column names changes across the time 
#> series provided (2001-2007, 2008-2014).
#> As a result the species defintions with the run script for the sitecmp 
#> program need to be changed accordingly.
# ==================================================================
# ==================================================================
#> Daily Species List SEARCH for 2008 and later
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

 setenv AERO_1 "PM25_Tef_Sulfate [ug/m3],ug/m3,ASO4IJ,ug/m3,SO4"
 setenv AERO_2 "PM25_Tef_Nitrate [ug/m3],ug/m3,ANO3IJ,ug/m3,NO3"
 setenv AERO_3 "PM25_Tef_Ammonium [ug/m3],ug/m3,ANH4IJ,ug/m3,NH4"
 setenv AERO_4 "PM25_Total_Nitrate [ug/m3],ug/m3,ANO3IJ+HNO3_UGM3,ug/m3,TNO3"
 setenv AERO_5 "PM25_Tef_Sodium [ug/m3],ug/m3,ANAIJ,ug/m3,Na"
 setenv AERO_6 "OCTC [ug/m3],ug/m3,AOCIJ,ug/m3,OC"
 setenv AERO_7 "ECTC [ug/m3],ug/m3,AECIJ,ug/m3,EC"
 setenv AERO_8 "FRM PM2.5 Mass [ug/m3],ug/m3,ATOTIJ,ug/m3,PM_TOT"
   
 setenv AERO_9 "XRF Al [ug/m3],ug/m3,AALJ,ug/m3,Al"
 setenv AERO_10 "XRF Si [ug/m3],ug/m3,ASIJ,ug/m3,Si"
 setenv AERO_11 "XRF K [ug/m3],ug/m3,AKJ,ug/m3,K"
 setenv AERO_12 "XRF Ca [ug/m3],ug/m3,ACAJ,ug/m3,Ca"
 setenv AERO_13 "XRF Ti [ug/m3],ug/m3,ATIJ,ug/m3,Ti"
 setenv AERO_14 "XRF Mn [ug/m3],ug/m3,AMNJ,ug/m3,Mn"
 setenv AERO_15 "XRF Fe [ug/m3],ug/m3,AFEJ,ug/m3,Fe"
  
#>> End Species List <<#

#> file containing site-id, longitude, latitude, time zone (tab delimited)
 setenv SITE_FILE SEARCH_sites.txt

#> input table containing site-id, time-period, and data fields
 setenv IN_TABLE SEARCH_daily_data_2011.csv


# =====================================================================
#> END Options for Daily SEARCH
# =====================================================================
```
  
