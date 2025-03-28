## CMAQ Tutorial ##
### Create Initial and Boundary Conditions from Seasonal or Daily Average Hemispheric CMAQ Output ###
Purpose: This tutorial will step the user through the process of creating initial and boundary conditions from seasonal or daily average hemispheric CMAQ output files distributed through the CMAS Data Warehouse. It assumes that the user already generated MCIP files for their target modeling domain.

Download seasonal average H-CMAQ output: https://drive.google.com/file/d/15Vt6f5WuyN8RiLRjTlKeQUHjYbZ6QCrA/view?usp=sharing

Download monthly files with daily average H-CMAQ output: https://drive.google.com/drive/folders/1A1ZzJE1t7OgwSezQNvy3rt9aATnXA0k2


------------


### STEP 1: Obtain the seasonal or daily average hemispheric CMAQ output files from the CMAS data warehouse</strong>

**Seasonal average hemispheric CMAQ output**

EPA distributes a file containing seasonal average 3D species concentrations from a hemispheric CMAQ simulation performed for 2016 over the northern hemisphere. These simulations were performed with a pre-release version of CMAQv5.3 using the following configuration:  

- Model version: CMAQv5.3 beta2 (February 2018), including full halogen and DMS chemistry  
- Grid spacing: 108 x 108 km on a polar stereographic grid covering the northern hemisphere  
- Vertical layers: 44  
- Meteorological fields: WRF3.8  
- Chemical mechanism: CB6R3M_AE7_KMTBR  
- Dry Deposition: M3DRY  

This file is named CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc and can be downloaded here

https://drive.google.com/file/d/15Vt6f5WuyN8RiLRjTlKeQUHjYbZ6QCrA/view?usp=sharing

**Daily average hemispheric CMAQ output**

EPA distributes a series of monthly files containing daily average 3D species concentrations from a hemispheric CMAQ simulation performed for 2002 - 2019 over the northern hemisphere as part of the EPA's Air Quality Time Series (EQUATES) Project. These simulations were performed with a modified version of CMAQv5.3.2 using the following configuration:  

- Model version: CMAQv5.3.2 with modifications to halogen chemistry and O3-PV scaling  
- Grid spacing: 108 x 108 km on a polar stereographic grid covering the northern hemisphere  
- Vertical layers: 44  
- Meteorological fields: WRF4.1.1  
- Chemical mechanism: CB6R3M_AE7_KMTBR  
- Dry Deposition: M3DRY  

These files are named CCTM_CONC_v532_intel18.0_CMAQv53_TS_108NHEMI_${YYYY}${MM}_dailyav.nc and can be downloaded here https://drive.google.com/drive/folders/1A1ZzJE1t7OgwSezQNvy3rt9aATnXA0k2

Metadata and DOI for EQUATES data:  https://doi.org/10.15139/S3/F2KJSK

### STEP 2 (optional): Time shift the downloaded seasonal or daily average hemispheric CMAQ output files </strong>

If the time period for which initial and boundary conditions are to be generated does not fall between October 16, 2015 12:00 GMT and January 16, 2017 0:00 GMT when using the seasonal average file or between January 1, 2002 00:00 GMT and December 31, 2019 00:00 GMT when using the daily average files, the time stamps in the downloaded file(s) need to be adjusted to encompass the desired time period. This can be accomplished using a tool like `m3tshift` that is part of the `m3tools` utilities released with [I/O API](https://www.cmascenter.org/ioapi/). 

The seasonal average concentration file contains six-time stamps (10/16/2015 12:00, 1/16/2016 0:00, 4/16/2016 12:00, 7/17/2016 0:00, 10/16/2016 12:00, and 1/16/2017 0:00) that represent fall, winter, spring, summer, fall, and winter seasonal average values, respectively. Fall was defined as September 1 - November 30, 2016, winter was defined as January 1 - February 29 and December 1 - December 31, 2016, spring was defined as March 1 - May 31, 2016, and summer was defined as June 1- August 31, 2016. Note that the concentration values associated with the first-time stamp are identical to those associated with the fifth time stamp since both represent fall, and the concentration values associated with the second time stamp are identical to those associated with the sixth time stamp since both represent winter.

The monthly files with daily average concentrations contain daily (i.e. 24:00 hour) time stamps from 00:00 GMT on the first day of the month to 00:00 GMT on the first day of the following month. For example, the file `CCTM_CONC_v532_intel18.0_CMAQv53_TS_108NHEMI_201006_dailyav.nc` contains 31 time stamps from 6/1/2010 00:00 GMT to 7/1/2010 00:00 GMT with a time step of 24:00 hours. Note that the file for December 2019 only contains 31 time stamps, i.e. it does not contain values for 1/1/2020 00:00 GMT.


A sample script using `m3tshift` to shift all of the six-time stamps from the seasonal average concentration file back by two years to support the generation of initial and boundary conditions with ICON and BCON for a modeling period between October 16, 2013 12:00 GMT and January 17, 2015 0:00 GMT is shown below. Analogous scripts could be created to shift the time stamps in one or more of the monthly files with daily average concentrations to the desired time period outside the range covered by these files.

```
#!/bin/csh -f

set EXEC = /path/to/m3tshift

#> Year to be entirely encompassed by the time stamps in the time-shifted output file
set TARGET_YEAR = 2014

#> Path to the seasonal average H-CMAQ file downloaded from the CMAS data warehouse
#> This path will also be used to store the time-shifted output file
set DATADIR = /path/to/downloaded_data

#> Name of the seasonal average H-CMAQ file downloaded from the CMAS data warehouse
set AV_CONC_INFILE = CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc

#> Name of the time-shifted seasonal average H-CMAQ file 
set AV_CONC_OUTFILE = CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_${TARGET_YEAR}_quarterly_av.nc

setenv INFILE ${DATADIR}/${AV_CONC_INFILE}
setenv OUTFILE ${DATADIR}/${AV_CONC_OUTFILE}

#> Invoke m3shift to shift the time stamps to the target year
#> Note that the first time stamp represents the fall of the previous year

@ TARGET_YEAR = ${TARGET_YEAR} - 1

${EXEC} << EOF
INFILE
2015289
120000
${TARGET_YEAR}289
120000
21960000
131760000
OUTFILE
EOF
```

### STEP 3: (optional): Map to a different chemical mechanism </strong>

If a chemical mechanism other than cb6r3_ae7_aq, cb6r5_ae7_aq, cb6r5_ae7_aqkmt2, or cb6r5m_ae7_aq will be used for the regional-scale CMAQ simulations, the species in the downloaded file need to be mapped to that other chemical mechanism. An example script for using the `combine` program to map from cb6r3m_ae7_kmtbr to racm_ae6_aq, racm2_ae6_aq, saprc07tc_ae6_aq, saprc07tic_ae7i_aq, craccm1_aq, or craccm2 is provided in a directory alongside the BCON and ICON [source code][link_1]. Species definition files used for the mechanism mapping are also provided in that directory.

### STEP 4: Compile the ICON and BCON executables</strong>

To compile the ICON and BCON executables, run the following commands from the CMAQ home directory: 

```
cd $CMAQ_HOME/PREP/icon/scripts
./bldit_icon.csh [compiler] [version] |& tee build_icon.log
```

```
cd $CMAQ_HOME/PREP/bcon/scripts
./bldit_bcon.csh [compiler] [version] |& tee build_bcon.log
```

### STEP 5: Run ICON to create initial conditions</strong>

The run script below uses the [`ICON`][link_2] program to create initial conditions for the user's target domain based on the seasonal average hemispheric CMAQ output obtained in Step 1, optionally time-shifted in Step 2, and optionally mapped to a different mechanism in Step 3. The same script can be used for the monthly files with daily average hemispheric CMAQ output by changing the CTM_CONC_1 and MET_CRO_3D_CRS environment variables to use one of those files instead of the file with the seasonal average output. By setting ICTYPE to regrid, the run script invokes ICON in _regrid_ mode because initial conditions are derived from a CONC file. In the example below, the settings for APPL, GRID_NAME, GRIDDESC, MET_CRO_3D_FIN, and DATE reflect the CMAQ Southeast benchmark case and will need to be modified by the user to point to the corresponding files for their domain and reflect the intended simulation start date. The environment variables CTM_CONC_1 and MET_CRO_3D_CRS should both point to the full path of the file downloaded in Step 1 and optionally time-shifted in Step 2 and/or species-mapped in Step 3.

```
#!/bin/csh -f

# ======================= ICONv5.3 Run Script ========================
# Usage: run.icon.csh >&! icon_v53.log &                                   
#
# To report problems or request help with this script/program:         
#             http://www.cmascenter.org
# ==================================================================== 

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 

#> Source the config_cmaq file to set the run environment
 pushd ../../../
 source ./config_cmaq.csh $compiler
 popd

#> Check that CMAQ_DATA is set:
 if ( ! -e $CMAQ_DATA ) then
    echo "   $CMAQ_DATA path does not exist"
    exit 1
 endif
 echo " "; echo " Input data path, CMAQ_DATA set to $CMAQ_DATA"; echo " "

#> Set General Parameters for Configuring the Simulation
 set VRSN     = v53                     #> Code Version
 set APPL     = SE53BENCH               #> Application Name
 set ICTYPE   = regrid                  #> Initial conditions type [profile|regrid]

#> Set the working directory:
 set BLD      = ${CMAQ_HOME}/PREP/icon/scripts/BLD_ICON_${VRSN}_${compilerString}
 set EXEC     = ICON_${VRSN}.exe  
 cat $BLD/ICON_${VRSN}.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE53BENCH               #> check GRIDDESC file for GRID_NAME options
#setenv GRIDDESC ${CMAQ_DATA}/$APPL/met/mcip/GRIDDESC #> grid description file 
 setenv GRIDDESC ${CMAQ_DATA}/SE53BENCH/met/mcip/GRIDDESC
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
# ICON Configuration Options
#
# ICON can be run in one of two modes:                                     
#     1) use default profile inputs (IC = profile)
#     2) regrids CMAQ CTM concentration files (IC = regrid)     
# =====================================================================

 setenv ICON_TYPE ` echo $ICTYPE | tr "[A-Z]" "[a-z]" ` 

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set OUTDIR   = $CMAQ_HOME/data/icon       #> output file directory

# =====================================================================
#> Input Files
#  
#  Profile Mode (IC = profile)
#     IC_PROFILE = static/default IC profiles 
#     MET_CRO_3D_FIN = the MET_CRO_3D met file for the target domain 
#  Regrid mode (IC = regrid) (includes nested domains, windowed domains,
#                             or general regridded domains)
#     CTM_CONC_1 = the CTM concentration file for the coarse domain          
#     MET_CRO_3D_CRS = the MET_CRO_3D met file for the coarse domain
#     MET_CRO_3D_FIN = the MET_CRO_3D met file for the target nested domain 
#                                                                            
# NOTE: SDATE (yyyyddd) and STIME (hhmmss) are only relevant to the
#       regrid mode and if they are not set, these variables will 
#       be set from the input MET_CRO_3D_FIN file
# =====================================================================
#> Output File
#     INIT_CONC_1 = gridded IC file for target domain
# =====================================================================

 if ( $ICON_TYPE == profile ) then
    setenv IC_PROFILE $BLD/avprofile_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
    setenv MET_CRO_3D_FIN ${CMAQ_DATA}/SE53BENCH/met/mcip/METCRO3D_160701.nc
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_${ICON_TYPE} -v"
 endif
 
 if ( $ICON_TYPE == regrid ) then
    setenv CTM_CONC_1 ${DATADIR}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_CRS ${DATADIR}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_FIN ${CMAQ_DATA}/SE53BENCH/met/mcip/METCRO3D_160701.nc
    set DATE = `date -ud "2016-07-01" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
#    setenv SDATE           ${DATE}
#    setenv STIME           000000
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_${ICON_TYPE}_${DATE} -v"
 endif

 
#>- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

 ls -l $BLD/$EXEC; size $BLD/$EXEC
 unlimit
 limit

#> Executable call:
 time $BLD/$EXEC

 exit() 
 ```
 
### STEP 6: Run BCON to create boundary conditions</strong>

The run script below uses the [`BCON`][link_3] program to create boundary conditions for the user's target domain based on the seasonal average hemispheric CMAQ output obtained in Step 1, optionally time-shifted in Step 2, and optionally mapped to a different mechanism in Step 3. The same script can be used for the monthly files with daily average hemispheric CMAQ output by changing the CTM_CONC_1 and MET_CRO_3D_CRS environment variables to use one of those files instead of the file with the seasonal average output. By setting BCTYPE to regrid, the run script invokes BCON in _regrid_ mode because boundary conditions are derived from a CONC file. In the example below, the settings for APPL, GRID_NAME, GRIDDESC, MET_CRO_3D_FIN, and DATE reflect the CMAQ Southeast benchmark case and will need to be modified by the user to point to the corresponding files for their domain and reflect the intended simulation start date. The environment variables CTM_CONC_1 and MET_CRO_3D_CRS should both point to the full path of the file downloaded in Step 1 and optionally time-shifted in Step 2 and/or species-mapped in Step 3.

```
#!/bin/csh -f

# ======================= BCONv5.3 Run Script ======================== 
# Usage: run.bcon.csh >&! bcon_v53.log &                                
#
# To report problems or request help with this script/program:        
#             http://www.cmascenter.org
# ==================================================================== 

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 

#> Source the config_cmaq file to set the run environment
 pushd ../../../
 source ./config_cmaq.csh $compiler
 popd

#> Check that CMAQ_DATA is set:
 if ( ! -e $CMAQ_DATA ) then
    echo "   $CMAQ_DATA path does not exist"
    exit 1
 endif
 echo " "; echo " Input data path, CMAQ_DATA set to $CMAQ_DATA"; echo " "

#> Set General Parameters for Configuring the Simulation
 set VRSN     = v53                     #> Code Version
 set APPL     = SE53BENCH               #> Application Name
 set BCTYPE   = regrid                  #> Boundary condition type [profile|regrid]

#> Set the build directory:
 set BLD      = ${CMAQ_HOME}/PREP/bcon/scripts/BLD_BCON_${VRSN}_${compilerString}
 set EXEC     = BCON_${VRSN}.exe  
 cat $BLD/BCON_${VRSN}.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE53BENCH               #> check GRIDDESC file for GRID_NAME options
#setenv GRIDDESC $CMAQ_DATA/$APPL/met/mcip/GRIDDESC #> grid description file 
 setenv GRIDDESC /SE53BENCH/met/mcip/GRIDDESC
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
#> BCON Configuration Options
#
# BCON can be run in one of two modes:                                     
#     1) use default profile inputs (BC type = profile)
#     2) regrids CMAQ CTM concentration files (BC = regrid)     
# =====================================================================

 setenv BCON_TYPE ` echo $BCTYPE | tr "[A-Z]" "[a-z]" `

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set OUTDIR   = $CMAQ_HOME/data/bcon       #> output file directory

# =====================================================================
#> Input Files
#  
#  Profile mode (BC type = profile)
#     BC_PROFILE = static/default BC profiles 
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target domain 
#  Regrid mode (BC = regrid) (includes nested domains, windowed domains,
#                             or general regridded domains)
#     CTM_CONC_1 = the CTM concentration file for the coarse domain          
#     MET_CRO_3D_CRS = the MET_CRO_3D met file for the coarse domain
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target nested domain
#                                                                            
# NOTE: SDATE (yyyyddd), STIME (hhmmss) and RUNLEN (hhmmss) are only 
#       relevant to the regrid mode and if they are not set,  
#       these variables will be set from the input MET_BDY_3D_FIN file
# =====================================================================
#> Output File
#     BNDY_CONC_1 = gridded BC file for target domain
# =====================================================================
 
 if ( $BCON_TYPE == profile ) then
    setenv BC_PROFILE $BLD/avprofile_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
    setenv MET_BDY_3D_FIN ${CMAQ_DATA}/SE53BENCH/met/mcip/METBDY3D_160701.nc
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${VRSN}_${APPL}_${BCON_TYPE} -v"
 endif

 if ( $BCON_TYPE == regrid ) then 
    setenv CTM_CONC_1 ${DATADIR}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_CRS ${DATADIR}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_BDY_3D_FIN ${CMAQ_DATA}/SE53BENCH/met/mcip/METBDY3D_160701.nc
    set DATE = `date -ud "2016-07-01" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
#    setenv SDATE           ${DATE}
#    setenv STIME           000000
#    setenv RUNLEN          240000
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${VRSN}_${APPL}_${BCON_TYPE}_${DATE} -v"
 endif


# =====================================================================
#> Output File
# =====================================================================
 
#>- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

 ls -l $BLD/$EXEC; size $BLD/$EXEC
 unlimit
 limit

#> Executable call:
 time $BLD/$EXEC

 exit() 
```


<!-- START_OF_COMMENT --> 

[link_1]: ../../../PREP/bcon/map2mech/
[link_2]: ../../../PREP/icon/
[link_3]: ../../../PREP/bcon

<!-- END_OF_COMMENT --> 

[link_1]: https://github.com/USEPA/CMAQ/blob/main/PREP/bcon/map2mech/
[link_2]: https://github.com/USEPA/CMAQ/blob/main/PREP/icon/ 
[link_3]: https://github.com/USEPA/CMAQ/blob/main/PREP/bcon/