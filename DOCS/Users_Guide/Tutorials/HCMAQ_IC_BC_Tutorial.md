## CMAQ Tutorial ##
### Create Initial and Boundary Conditions from Seasonal Average Hemispheric CMAQ Output ###
Purpose: This tutorial will step the user through the process of creating initial and boundary conditions from a seasonal average hemispheric CMAQ output file distributed through the CMAS data warehouse. It assumes that the user already generated MCIP files for their target modeling domain.

** >>COMMENT<< ** add link to H-CMAQ output file



------------


### STEP 1: Obtain the seasonal average hemispheric CMAQ output file from the CMAS data warehouse</strong>

EPA distributes a file containing seasonal average 3D species concentrations from a hemispheric CMAQ simulation performed for 2016 over the northern hemisphere. These simulations were performed with a pre-release version of CMAQv5.3 using the following configuration:  

- Model version: CMAQv5.3 beta2 (February 2018), including full halogen and DMS chemistry  
- Grid spacing: 108 x 108 km on a polar stereographic grid covering the northern hemisphere  
- Vertical layers: 44  
- Meteorological fields: WRF3.8  
- Chemical mechanism: CB6R3M_AE7_KMTBR  
- Dry Deposition: M3DRY  

This file is named CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc and can be downloaded here

** >>COMMENT<< ** add link to H-CMAQ output file


### STEP 2: Compile the ICON and BCON executables</strong>

To compile the ICON and BCON executables, run the following commands from the CMAQ home directory: 

```
cd $CMAQ_HOME/PREP/icon/scripts
./bldit_icon.csh [compiler] [version] |& tee build_icon.log
```

```
cd $CMAQ_HOME/PREP/bcon/scripts
./bldit_bcon.csh [compiler] [version] |& tee build_bcon.log
```

### STEP 3: Run ICON to create initial conditions</strong>

The run script below uses the [`ICON`](../../../PREP/icon) program to create intial conditions for the user's target domain based on the seasonal average hemispheric CMAQ output obtained in Step 1. By setting ICTYPE to regrid, the run script invokes ICON in _regrid_ mode because initial conditions are derived from a CONC file. In the example below, the settings for APPL, GRID_NAME, GRIDDESC, MET_CRO_3D_FIN, and DATE reflect the CMAQ Southeast benchmark case and will need to be modified by the user to point to the corresponding files for their domain and reflect the intended simulation start date. The environment variables CTM_CONC_1 and MET_CRO_3D_CRS should both point to the full path of the file downloaded in Step 1.

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
 set ICTYPE   = regrid                  #> Initial conditions type [profile|regrid|patterns]

#> Set the working directory:
 set BLD      = ${CMAQ_HOME}/PREP/icon/scripts/BLD_ICON_${VRSN}_${compilerString}
 set EXEC     = ICON_${VRSN}.exe  
 cat $BLD/ICON_${VRSN}.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE53BENCH               #> check GRIDDESC file for GRID_NAME options
#setenv GRIDDESC $CMAQ_DATA/$APPL/met/mcip/GRIDDESC #> grid description file 
 setenv GRIDDESC /work/MOD3DATA/SE53BENCH/met/mcip/GRIDDESC
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
# ICON Configuration Options
#
# ICON can be run in one of three modes:                                     
#     1) use default profile inputs (IC = profile)
#     2) regrids CMAQ CTM concentration files (IC = regrid)     
#     3) generate set of test patterns for CTM transport/diffusion tests
#        (IC = patterns)     
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
#  Test_patterns Mode (IC = patterns)
#     MET_CRO_3D_FIN = the MET_CRO_3D met file for the target domain 
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
    setenv MET_CRO_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcip/METCRO3D_160701.nc
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_${ICON_TYPE} -v"
 endif
 
 if ( $ICON_TYPE == regrid ) then
    setenv CTM_CONC_1 /path/to/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_CRS /path/to/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcip/METCRO3D_160701.nc
    set DATE = `date -ud "2016-07-01" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
#    setenv SDATE           ${DATE}
#    setenv STIME           000000
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_${ICON_TYPE}_${DATE} -v"
 endif

 if ( $ICON_TYPE == patterns ) then
    setenv MET_CRO_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcip/METCRO3D_160701.nc
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_${ICON_TYPE} -v"
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
 
### STEP 4: Run BCON to create initial conditions</strong>

The run script below uses the [`BCON`](../../../PREP/bcon) program to create intial conditions for the user's target domain based on the seasonal average hemispheric CMAQ output obtained in Step 1. By setting BCTYPE to regrid, the run script invokes BCON in _regrid_ mode because boundary conditions are derived from a CONC file. In the example below, the settings for APPL, GRID_NAME, GRIDDESC, MET_CRO_3D_FIN, and DATE reflect the CMAQ Southeast benchmark case and will need to be modified by the user to point to the corresponding files for their domain and reflect the intended simulation start date. The environment variables CTM_CONC_1 and MET_CRO_3D_CRS should both point to the full path of the file downloaded in Step 1.

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
 set BCTYPE   = regrid                  #> Boundary condition type [profile|regrid|patterns]

#> Set the build directory:
 set BLD      = ${CMAQ_HOME}/PREP/bcon/scripts/BLD_BCON_${VRSN}_${compilerString}
 set EXEC     = BCON_${VRSN}.exe  
 cat $BLD/BCON_${VRSN}.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE53BENCH               #> check GRIDDESC file for GRID_NAME options
#setenv GRIDDESC $CMAQ_DATA/$APPL/met/mcip/GRIDDESC #> grid description file 
 setenv GRIDDESC /work/MOD3DATA/SE53BENCH/met/mcip/GRIDDESC
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
#> BCON Configuration Options
#
# BCON can be run in one of three modes:                                     
#     1) use default profile inputs (BC type = profile)
#     2) regrids CMAQ CTM concentration files (BC = regrid)     
#     3) generate set of test patterns for CTM transport/diffusion tests
#        (BC = patterns)
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
#  Test patterns mode (BC = patterns)
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target domain 
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
    setenv MET_BDY_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcip/METBDY3D_160701.nc
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${VRSN}_${APPL}_${BCON_TYPE} -v"
 endif

 if ( $BCON_TYPE == regrid ) then 
    setenv CTM_CONC_1 /path/to/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_CRS /path/to/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_BDY_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcip/METBDY3D_160701.nc
    set DATE = `date -ud "2016-07-01" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
#    setenv SDATE           ${DATE}
#    setenv STIME           000000
#    setenv RUNLEN          240000
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${VRSN}_${APPL}_${BCON_TYPE}_${DATE} -v"
 endif

 if ( $BCON_TYPE == patterns ) then
    setenv MET_BDY_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcip/METBDY3D_160701.nc
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${VRSN}_${APPL}_${BCON_TYPE} -v"
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
