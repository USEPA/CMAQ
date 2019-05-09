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
 set APPL     = SE52BENCH               #> Application Name
 set BCTYPE   = regrid                  #> Boundary condition type [profile|regrid|tracer]

#> Set the build directory:
 set BLD      = ${CMAQ_HOME}/PREP/bcon/scripts/BLD_BCON_${VRSN}_${compilerString}
 set EXEC     = BCON_${VRSN}.exe  
 cat $BLD/BCON_${VRSN}.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE52BENCH               #> check GRIDDESC file for GRID_NAME options
 setenv GRIDDESC $CMAQ_DATA/$APPL/met/mcip/GRIDDESC #> grid description file 
#setenv GRIDDESC /work/MOD3DEV/cmaq_benchmark/SE52BENCH/multi_day/cctm_input/met/mcip/GRIDDESC #> grid description file 
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 NO    #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
#> BCON Configuration Options
#
# BCON can be run in one of three modes:                                     
#     1) use default profile inputs (BC type = profile)
#     2) regrids CMAQ CTM concentration files (BC = regrid)     
#     3) generate set of tracer patterns for CTM transport tests (BC = tracer)
# =====================================================================

 setenv BCON_TYPE ` echo $BCTYPE | tr "[A-Z]" "[a-z]" `
 set DATE = `date -ud "2016-07-01" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set OUTDIR   = $CMAQ_HOME/data/bcon       #> output file directory

# =====================================================================
#> Input Files
#  
#  Profile Mode (BC type = profile)
#     BC_PROFILE = static/default BC profiles 
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target domain 
#  Regrid mode (BC = regrid) (includes nested domains, windowed domains,
#                             or general regridded domains)
#     CTM_CONC_1 = the CTM concentration file for the coarse domain          
#     MET_CRO_3D_CRS = the MET_CRO_3D met file for the coarse domain
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target nested domain
#  Tracer Mode (BC = tracer)
#     MET_BDY_3D_FIN = the MET_BDY_3D met file for the target domain 
#                                                                            
# NOTE: If SDATE (yyyyddd), STIME (hhmmss) and RUNLEN (hhmmss) are not set,  
#       these variables will be set from the input CTM_CONC_1 file
# =====================================================================
 
 if ( $BCON_TYPE == profile ) then
    setenv BC_PROFILE $BLD/avprofile_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
    setenv MET_BDY_3D_FIN /work/MOD3DEV/cmaq_benchmark/SE52BENCH/multi_day/cctm_input/met/mcip/METBDY3D_110701.nc #>METBDY3D file from MCIP
 endif

 if ( $BCON_TYPE == regrid ) then 
    setenv CTM_CONC_1 /asm1/ROMO/global/CMAQv5.2/2016fe_hemi_cb6_16jh/108km/output/CONC/CCTM_CONC_v521_intel17.0_HEMIS_cb6_20160701
    setenv MET_CRO_3D_CRS /asm1/ROMO/met/MCIP/WRFv3.8_108NHEMI2_2016_44aL/v4.3/METCRO3D.108NHEMI2.44L.160701
    setenv MET_BDY_3D_FIN /work/MOD3DEV/cmaq_benchmark/SE52BENCH/multi_day/cctm_input/met/mcip/METBDY3D_110701.nc
#    setenv SDATE           ${DATE}
#    setenv STIME           000000
#    setenv RUNLEN          240000
 endif

 if ( $BCON_TYPE == tracer ) then
    setenv MET_BDY_3D_FIN /work/MOD3DEV/cmaq_benchmark/SE52BENCH/multi_day/cctm_input/met/mcip/METBDY3D_110701.nc
 endif

# =====================================================================
#> Output File
# =====================================================================
 
 setenv BNDY_CONC_1    "$OUTDIR/BCON_${VRSN}_${APPL}_${BCON_TYPE}_${DATE} -v"

#>- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

 ls -l $BLD/$EXEC; size $BLD/$EXEC
 unlimit
 limit

#> Executable call:
 time $BLD/$EXEC

 exit() 
