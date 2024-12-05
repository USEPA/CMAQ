#!/bin/csh -f

# ======================= BCONv5.5.X Run Script ======================== 
# Usage: run_bcon.csh >&! bcon.log &                                
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
 set VRSN     = v55                     #> Code Version
 set APPL     = 2016_12SE1              #> Application Name
 set BCTYPE   = regrid                  #> Boundary condition type [profile|regrid]

#> Set the build directory:
 set BLD      = ${CMAQ_HOME}/PREP/bcon/scripts/BLD_BCON_${VRSN}_${compilerString}
 set EXEC     = BCON_${VRSN}.exe  
 cat $BLD/BCON_${VRSN}.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE53BENCH               #> check GRIDDESC file for GRID_NAME options
#setenv GRIDDESC $CMAQ_DATA/$APPL/met/mcip/GRIDDESC #> grid description file 
 setenv GRIDDESC /work/MOD3DATA/SE53BENCH/met/mcipv4.5/GRIDDESC
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
#> BCON Configuration Options
#
# BCON can be run in one of two modes:                                     
#     1) regrids CMAQ CTM concentration files (BC type = regrid)     
#     2) use default profile inputs (BC type = profile)
# =====================================================================

 setenv BCON_TYPE ` echo $BCTYPE | tr "[A-Z]" "[a-z]" `

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set OUTDIR   = $CMAQ_HOME/data/bcon       #> output file directory

# =====================================================================
#> Input Files
#  
#  Regrid mode (BC = regrid) (includes nested domains, windowed domains,
#                             or general regridded domains)
#     CTM_CONC_1 = the CTM concentration file for the coarse domain          
#     MET_CRO_3D_CRS = the MET_CRO_3D met file for the coarse domain
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target nested domain
#                                                                            
#  Profile mode (BC type = profile)
#     BC_PROFILE = static/default BC profiles 
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target domain 
#
# NOTE: SDATE (yyyyddd), STIME (hhmmss) and RUNLEN (hhmmss) are only 
#       relevant to the regrid mode and if they are not set,  
#       these variables will be set from the input MET_BDY_3D_FIN file
# =====================================================================
#> Output File
#     BNDY_CONC_1 = gridded BC file for target domain
# =====================================================================
 
    set DATE = "2016-07-01"
    set YYYYJJJ  = `date -ud "${DATE}" +%Y%j`   #> Convert YYYY-MM-DD to YYYYJJJ
    set YYMMDD   = `date -ud "${DATE}" +%y%m%d` #> Convert YYYY-MM-DD to YYMMDD
    set YYYYMMDD = `date -ud "${DATE}" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYMMDD
#   setenv SDATE           ${YYYYJJJ}
#   setenv STIME           000000
#   setenv RUNLEN          240000

 if ( $BCON_TYPE == regrid ) then 
    setenv CTM_CONC_1 /work/MOD3EVAL/sjr/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
    setenv MET_CRO_3D_CRS /work/MOD3DATA/2016_12US1/met/mcip_v43_wrf_v381_ltng/METCRO3D.12US1.35L.${YYMMDD}
    setenv MET_BDY_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcipv4.5/METBDY3D_${YYMMDD}.nc
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${APPL}_${BCON_TYPE}_${YYYYMMDD} -v"
 endif

 if ( $BCON_TYPE == profile ) then
    setenv BC_PROFILE $BLD/avprofile_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
    setenv MET_BDY_3D_FIN /work/MOD3DATA/SE53BENCH/met/mcipv4.5/METBDY3D_${YYMMDD}.nc
    setenv BNDY_CONC_1    "$OUTDIR/BCON_${APPL}_${BCON_TYPE}_${YYYYMMDD} -v"
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

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BLD/$EXEC"
   exit( ${progstat} )
 endif

 exit() 
