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
    setenv CTM_CONC_1 /asm/MOD3DEV/fsidi/12US1_201607/CCTM_CONC_v53_intel18.0_2016_CONUS_test_20160701.nc
    setenv MET_CRO_3D_CRS /work/MOD3DATA/2016_12US1/met/mcip_v43_wrf_v381_ltng/METCRO3D.12US1.35L.160701
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
