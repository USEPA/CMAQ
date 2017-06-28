#!/bin/csh -f

# ======================= ICONv5.2 Run Script ========================
# Usage: run.icon.csh >&! icon_v52.log &                                   
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
 setenv compilerVrsn 15.0

#> Source the config_cmaq file to set the run environment
 pushd ../../../
 source ./config_cmaq.csh
 popd

#> Check that CMAQ_DATA is set:
 if ( ! -e $CMAQ_DATA ) then
    echo "   $CMAQ_DATA path does not exist"
    exit 1
 endif
 echo " "; echo " Input data path, CMAQ_DATA set to $CMAQ_DATA"; echo " "

#> Set General Parameters for Configuring the Simulation
 set VRSN     = v52                     #> Code Version
 set APPL     = SE52BENCH               #> Application Name
 set INPT     = profile                 #> Input data type: profile or m3conc?
 set MECH     = cb05e51_ae6_aq          #> Mechanism ID

#> Set the working directory:
 set BLD      = ${CMAQ_HOME}/PREP/icon/scripts/BLD_ICON_${VRSN}_${INPT}_${compiler}
 set EXEC     = ICON_${VRSN}_$INPT.exe  
 cat $BLD/ICON_${VRSN}_$INPT.cfg; echo " "; set echo

#> Horizontal grid definition 
 setenv GRID_NAME SE52BENCH               #> check GRIDDESC file for GRID_NAME options
 setenv GRIDDESC $CMAQ_DATA/$APPL/met/mcip/GRIDDESC #> grid description file 
 setenv IOAPI_ISPH 20                     #> GCTP spheroid, use 20 for WRF-based modeling

#> Vertical layer definition
 setenv LAYER_FILE $CMAQ_DATA/$APPL/met/mcip/METCRO3D_110701.nc #>METCRO3D file from MCIP

#> I/O Controls
 setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
 setenv IOAPI_OFFSET_64 NO    #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
 setenv EXECUTION_ID $EXEC    #> define the model execution id

# =====================================================================
#> ICON Configuration Options
#
# ICON can be run in one of two modes:                                     
#     1) use default profile inputs (IC = profile)
#     2) use CMAQ CTM concentration files for nested runs (IC = m3conc)     
# =====================================================================

 set IC = profile      #> either profile or m3conc 
 set DATE = 2001182    #> only needed for nested runs

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set OUTDIR   = $CMAQ_DATA/icon       #> output file directory

# =====================================================================
#> Input Files
#  
#  Profile Mode (IC = profile)
#     IC_PROFILE = static/default IC profiles 
#  Nesting mode (IC = m3conc)
#     CTM_CONC_1 = the CTM concentration file for the coarse domain          
#     MET_CRO_3D_CRS = the MET_CRO_3D met file for the coarse domain
#                  only set if  or if the vertical grid type is   
#                  changed between nests                                     
#     MET_CRO_3D_FIN = the MET_CRO_3D met file for the inner, nested, domain 
#                  only set if the vertical grid type is changed between  
#                  nests                                                     
#                                                                            
# NOTE: SDATE (yyyyddd) and STIME (hhmmss) must always be set           
# =====================================================================

 if ( $IC == profile ) then
    setenv IC_PROFILE      $BLD/ic_profile_CB05.dat
 endif
 
 if ( $IC == m3conc ) then 
    setenv CTM_CONC_1 $CMAQ_DATA/cctm/CCTM_d1bCONC.d1b
    setenv MET_CRO_3D_CRS
    setenv MET_CRO_3D_FIN
    setenv SDATE           ${DATE}
    setenv STIME           000000
 endif

# =====================================================================
#> Output Files
# =====================================================================


 if ( $IC == profile ) then
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_profile -v"
    endif
 if ( $IC == m3conc ) then 
    set DATE = 2011182  # July 1, 2011
    setenv INIT_CONC_1    "$OUTDIR/ICON_${VRSN}_${APPL}_${DATE} -v"
 endif

#>- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#> species defn
 setenv gc_matrix_nml ${BLD}/GC_$MECH.nml
 setenv ae_matrix_nml ${BLD}/AE_$MECH.nml
 setenv nr_matrix_nml ${BLD}/NR_$MECH.nml
 setenv tr_matrix_nml ${BLD}/Species_Table_TR_0.nml
 
 if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

 ls -l $BLD/$EXEC; size $BLD/$EXEC
 unlimit
 limit

#> Executable call:
 time $BLD/$EXEC

 exit() 
