#!/bin/csh -f

# ===================== CCTMv5.3 Run Script ========================= 
# Usage: run.cctm.csh >&! cctm_2010_4CALIF1.log &                                
#
# To report problems or request help with this script/program:     
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================  

# ===================================================================
#> Runtime Environment Options
# ===================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 if ( ! $?compiler ) then
   setenv compiler intel 
 endif

#> Source the config.cmaq file to set the build environment
 cd ../..
 source ./config_cmaq.csh $compiler
 cd CCTM/scripts

#> Set General Parameters and Labels for Configuring the Simulation
 set VRSN      = v53                #> Code Version
 set PROC      = mpi                #> serial or mpi
 set MECH      = saprc07tic_ae7i_aq #> Mechanism ID
 set EMIS      = 2011eh_saprc_10g   #> Emission Inventory Details
 set APPL      = 4CALIF1            #> Application Name (e.g. Domain)
                                                       
#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 setenv RUNID  ${VRSN}_${PROC}_${compilerString}_${APPL}

#> Set the build directory (this is where the CMAQ executable
#> is located by default).
 set BLD      = ${CMAQ_HOME}/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}
 set EXEC     = CCTM_${VRSN}.exe  
 cat $BLD/CCTM_${VRSN}.cfg; echo "    "; set echo

#> Set Working, Input, and Output Directories
 setenv WORKDIR ${CMAQ_HOME}/CCTM/scripts         #> Working Directory. Where the runscript is.
 setenv OUTDIR  ${CMAQ_DATA}/output_CCTM_${RUNID} #> Output Directory
 setenv INPDIR  ${CMAQ_DATA}/2010_4CALIF1         #> Input Directory
 setenv LOGDIR  ${OUTDIR}/LOGS                    #> Log Directory Location
                                   
# =====================================================================
#> CCTM Configuration Options
# =====================================================================

#> Set Start and End Days for looping
 setenv NEW_START TRUE            #> Set to FALSE for model restart
 set START_DATE = "2010-05-02"     #> beginning date (May 1, 2010)
 set END_DATE   = "2010-05-02"     #> ending date    (June 30, 2010)

#> Set Timestepping Parameters
set STTIME     = 000000            #> beginning GMT time (HHMMSS)
set NSTEPS     = 240000            #> time duration (HHMMSS) for this run
set TSTEP      = 010000            #> output time step interval (HHMMSS)

#> Horizontal domain decomposition
if ( $PROC == serial ) then
   setenv NPCOL_NPROW "1 1"; set NPROCS   = 1 # single processor setting
else
   @ NPCOL  =  8; @ NPROW =  8
   @ NPROCS = $NPCOL * $NPROW
   setenv NPCOL_NPROW "$NPCOL $NPROW"; 
endif

#> Vertical extent
set NZ         = 35

#setenv LOGFILE $CMAQ_HOME/$RUNID.log  #> log file name; uncomment to write standard output to a log, otherwise write to screen

setenv GRID_NAME 4CALIF1     #> check GRIDDESC file for GRID_NAME options
setenv GRIDDESC $INPDIR/GRIDDESC   #> grid description file

#> Output Species and Layer Options
#>   CONC file species; comment or set to "ALL" to write all species to CONC
#setenv CONC_SPCS "O3 NO ANO3I ANO3J NO2 FORM ISOP ANH4J ASO4I ASO4J" 
#setenv CONC_BLEV_ELEV " 1 4" #> CONC file layer range; comment to write all layers to CONC

#>   ACONC file species; comment or set to "ALL" to write all species to ACONC
#setenv AVG_CONC_SPCS "O3 NO CO NO2 ASO4I ASO4J NH3" 
setenv AVG_CONC_SPCS "ALL" 
setenv ACONC_BLEV_ELEV " 1 1" #> ACONC file layer range; comment to write all layers to ACONC
setenv AVG_FILE_ENDTIME N     #> override default beginning ACON timestamp [ default: N ]

setenv EXECUTION_ID $EXEC    #> define the model execution id

#> Sychronization Time Step and Tolerance Options
setenv CTM_MAXSYNC 300       #> max sync time step (sec) [ default: 720 ]
setenv CTM_MINSYNC  60       #> min sync time step (sec) [ default: 60 ]
setenv SIGMA_SYNC_TOP 0.7    #> top sigma level thru which sync step determined [ default: 0.7 ] 
#setenv ADV_HDIV_LIM 0.95     #> maximum horiz. div. limit for adv step adjust [ default: 0.9 ]
setenv CTM_ADV_CFL 0.95      #> max CFL [ default: 0.75]
#setenv RB_ATOL 1.0E-09       #> global ROS3 solver abs tol [ default: 1.0E-07 ] 

#> Science Options
setenv CTM_SS_AERO Y         #> use inline Sea Spray Aerosol emissions [ default: Y ]
setenv CTM_WB_DUST Y         #> use inline windblown dust emissions [ default: Y ]
setenv CTM_ERODE_AGLAND Y    #> use agricultural activity for windblown dust 
                             #>    [ default: N ]; ignore if CTM_WB_DUST = N
setenv CTM_WBDUST_BELD BELD3 #> landuse database for identifying dust source regions 
                             #>    [ default: BELD3 ]; ignore if CTM_WB_DUST = N 
setenv CTM_LTNG_NO N         #> turn on lightning NOx [ default: N ]
setenv CTM_WVEL Y            #> save derived vertical velocity component to conc 
                             #>    file [ default: N ]
setenv KZMIN Y               #> use Min Kz option in edyintb [ default: Y ], 
                             #>    otherwise revert to Kz0UT
setenv CTM_ILDEPV Y          #> calculate in-line deposition velocities [ default: Y ]
setenv CTM_MOSAIC N          #> landuse specific deposition velocities [ default: N ]
setenv CTM_FST N             #> mosaic method to get land-use specific stomatal flux 
                             #>    [ default: N ]
setenv CTM_ABFLUX N          #> ammonia bi-directional flux for in-line deposition 
                             #>    velocities [ default: N ]; ignore if CTM_ILDEPV = N
setenv CTM_HGBIDI N          #> mercury bi-directional flux for in-line deposition 
                             #>    velocities [ default: N ]; ignore if CTM_ILDEPV = N
setenv CTM_SFC_HONO Y        #> surface HONO interaction [ default: Y ]; ignore if CTM_ILDEPV = N
setenv CTM_GRAV_SETL Y       #> vdiff aerosol gravitational sedimentation [ default: Y ]
setenv CTM_BIOGEMIS Y        #> calculate in-line biogenic emissions [ default: N ]
setenv CTM_PT3DEMIS Y        #> calculate in-line plume rise for elevated point emissions 
                             #>    [ default: N ]
setenv CTM_ZERO_PCSOA N      #> turn off the emissions of the VOC precursor to pcSOA.
                             #>    The CMAQ dev team recommends leaving pcSOA mass in the
                             #>    model for production runs. [ default: N ]

#> Vertical Extraction Options
setenv DOVERTEXT N
setenv VERTLONLATPATH ${WORKDIR}/lonlat.csv
#> Process Analysis Options
setenv CTM_PROCAN N          #> use process analysis [ default: N]
#> process analysis global column, row and layer ranges
#> user must check GRIDDESC for validity!
setenv PA_BCOL_ECOL "10 320"
setenv PA_BROW_EROW "10 195"
setenv PA_BLEV_ELEV "1  4"

#> I/O Controls
setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
setenv FL_ERR_STOP N         #> stop on inconsistent input files
setenv PROMPTFLAG F          #> turn on I/O-API PROMPT*FILE interactive mode [ options: T | F ]
setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
setenv CTM_EMISCHK N         #> Abort CMAQ if missing surrogates from emissions Input files
setenv EMIS_DATE_OVRD N      #> Master switch for allowing CMAQ to use the date from each Emission file
                             #>   rather than checking the emissions date against the internal model date.
                             #>   [options: T | F or Y | N]. If false (F/N), then the date from CMAQ's internal
                             #>   time will be used and an error check will be performed (recommended). Users 
                             #>   may switch the behavior for individual emission files below using the variables:
                             #>       GR_EM_DTOVRD_## | STK_EM_DTOVRD_##

#> Aerosol Diagnostic Controls
setenv CTM_PMDIAG Y          #> Instantaneous Aerosol Diagnostic File [ default: Y ]
setenv CTM_APMDIAG Y         #> Hourly-Average Aerosol Diagnostic File [ default: Y ]
setenv APMDIAG_BLEV_ELEV "1 3" #> layer range for average pmdiag
setenv APMDIAG_BLEV_ELEV ""  #> layer range for average pmdiag = NLAYS

#> Diagnostic Output Flags
setenv CTM_CKSUM Y           #> cksum report [ default: Y ]
setenv CLD_DIAG Y            #> cloud diagnostic file [ default: N ]
setenv CTM_AERDIAG Y         #> aerosol diagnostic file [ default: N ]

setenv CTM_PHOTDIAG Y        #> photolysis diagnostic file [ default: N ]
setenv NLAYS_PHOTDIAG "3"    #> Number of layers for PHOTDIAG2 and PHOTDIAG3 from 
                             #>     Layer 1 to NLAYS_PHOTDIAG  [ default: all layers ] 
#setenv NWAVE_PHOTDIAG "294 303 310 316 333 381 607"  #> Wavelengths written for variables 
                                                      #>   in PHOTDIAG2 and PHOTDIAG3 
                                                      #>   [ default: all wavelengths ]

setenv CTM_SSEMDIAG Y        #> sea-spray emissions diagnostic file [ default: N ]
setenv CTM_DUSTEM_DIAG Y     #> windblown dust emissions diagnostic file [ default: N ]; 
                             #>     Ignore if CTM_WB_DUST = N
setenv CTM_DEPV_FILE Y       #> deposition velocities diagnostic file [ default: N ]
setenv VDIFF_DIAG_FILE Y     #> vdiff & possibly aero grav. sedimentation diagnostic file [ default: N ]
setenv LTNGDIAG N            #> lightning diagnostic file [ default: N ]
setenv B3GTS_DIAG Y          #> beis mass emissions diagnostic file [ default: N ]
setenv PT3DDIAG N            #> optional 3d point source emissions diagnostic file [ default: N]; 
                             #>     Ignore if CTM_PT3DEMIS = N
setenv PT3DFRAC N            #> optional layer fractions diagnostic (play) file(s) [ default: N];
                             #>     Ignore if CTM_PT3DEMIS = N
setenv REP_LAYER_MIN -1      #> Minimum layer for reporting plume rise info [ default: -1 ]
setenv EMISDIAG F            #> Print Emission Rates at the output time step after they have been
                             #>   scaled and modified by the user Rules [options: F | T or 2D | 3D | 2DSUM ]
                             #>   Individual streams can be modified using the variables:
                             #>       GR_EMIS_DIAG_## | STK_EMIS_DIAG_## | BIOG_EMIS_DIAG
                             #>       MG_EMIS_DIAG    | LTNG_EMIS_DIAG   | DUST_EMIS_DIAG
                             #>       SEASPRAY_EMIS_DIAG
                             #>   Note that these diagnostics are different than other emissions diagnostic
                             #>   output because they occur after scaling. 
set DISP = delete            #> [ delete | keep ] existing output files


# =====================================================================
#> Input Directories and Filenames
# =====================================================================

set ICpath    = $INPDIR/icbc              #> initial conditions input directory 
set BCpath    = $INPDIR/icbc              #> boundary conditions input directory
set EMISpath  = $INPDIR/emis/saprc07t_ae6_20180511/cmaq_ready/gridded #> surface emissions input directory
set IN_PTpath = $INPDIR/emis/saprc07t_ae6_20180511/cmaq_ready  #> elevated emissions input directory (in-line point only)
set IN_LTpath = $INPDIR/met/lightning     #> lightning NOx input directory
set METpath   = $INPDIR/met/mcip_v4.3_wrf_v3.8.1              #> meteorology input directory
#set JVALpath  = $INPDIR/jproc            #> offline photolysis rate table directory
set OMIpath   = $BLD                      #> ozone columne data for the photolysis model
set NMLpath   = $BLD                      #> Location of Namelists. Common places are: 
                                          #>   ${WORKDIR} | ${CCTM_SRC}/MECHS/${MECH} | ${BLD}
set LUpath    = $INPDIR/surface           #> BELD landuse data for windblown dust model
set SZpath    = $INPDIR/surface           #> surf zone file for in-line seasalt emissions

set ICBC_CASE = 2011eh_saprc_10g         #> Version label for the ICBCs
set EMIS_CASE = 2011eh_saprc_10g         #> Version Label for the Emissions

# =====================================================================
#> Begin Loop Through Simulation Days
# =====================================================================

set TODAYG = ${START_DATE}
set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ

  #> Retrieve Calendar day Information
  set YYYYMMDD = `date -ud "${TODAYG}" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYMMDD
  set YYYYMM = `date -ud "${TODAYG}" +%Y%m`     #> Convert YYYY-MM-DD to YYYYMM
  set YYMMDD = `date -ud "${TODAYG}" +%y%m%d`   #> Convert YYYY-MM-DD to YYMMDD
  set YYYYJJJ = $TODAYJ

  #> Calculate Yesterday's Date
  set YESTERDAY = `date -ud "${TODAYG}-1days" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYJJJ

# =====================================================================
#> Input Files (Some are Day-Dependent)
# =====================================================================

  #> Initial conditions                                                     
  if ($NEW_START == true || $NEW_START == TRUE ) then
     setenv ICFILE ICON_v52_4CALIF1_saprc07tb_ae6_aq_timeind
     setenv INIT_MEDC_1 notused
     setenv INITIAL_RUN Y #related to restart soil information file
     rm -rf $LOGDIR/CTM_LOG*${RUNID}*  # Remove all Log Files Since this is a new start
     mkdir -p $OUTDIR
  else
     set ICpath = $OUTDIR
     setenv ICFILE CCTM_CGRID_${RUNID}_${YESTERDAY}.nc
     setenv INIT_MEDC_1 $ICpath/CCTM_MEDIA_CONC_${RUNID}_${YESTERDAY}
     setenv INITIAL_RUN N
  endif

  #> Boundary conditions
  set BCFILE = BCON_v52_4CALIF1_saprc07tb_ae6_aq_timeind

  #> Off-line photolysis rates 
  #set JVALfile  = JTABLE_${YYYYJJJ}

  #> Ozone column data
  set OMIfile   = OMI_1979_to_2017.dat

  #> Optics file
  set OPTfile = PHOT_OPTICS.dat

  #> MCIP meteorology files 
  setenv GRID_BDY_2D $METpath/GRIDBDY2D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv GRID_CRO_2D $METpath/GRIDCRO2D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv GRID_CRO_3D $METpath/GRIDCRO3D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv GRID_CRO_3D '' # this file not used or available
  setenv GRID_DOT_2D $METpath/GRIDDOT2D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv MET_CRO_2D  $METpath/METCRO2D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv MET_CRO_3D  $METpath/METCRO3D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv MET_DOT_3D  $METpath/METDOT3D.$GRID_NAME.${NZ}L.$YYMMDD
  setenv MET_BDY_3D  $METpath/METBDY3D.$GRID_NAME.${NZ}L.$YYMMDD

  setenv LAYER_FILE $MET_CRO_3D  # Deprecated: MET_CRO_3D is now read directly in CCTM

  #> Determine Representative Emission Days
  set EMDATES = $INPDIR/emis/emis_dates/smk_merge_dates_${YYYYMM}.txt
  set intable = `grep "^${YYYYMMDD}" $EMDATES`
  set Date     = `echo $intable[1] | cut -d, -f1`
  set aveday_N = `echo $intable[2] | cut -d, -f1`
  set aveday_Y = `echo $intable[3] | cut -d, -f1`
  set mwdss_N  = `echo $intable[4] | cut -d, -f1`
  set mwdss_Y  = `echo $intable[5] | cut -d, -f1`
  set week_N   = `echo $intable[6] | cut -d, -f1`
  set week_Y   = `echo $intable[7] | cut -d, -f1`
  set all      = `echo $intable[8] | cut -d, -f1`
  
  #> Gridded Emissions Files
  setenv N_EMIS_GR 1
  set EMISfile  = emis_mole_all_${YYYYMMDD}_4CALIF1_cmaq_saprc07TB_2011eh_saprc_10g.ncf
  setenv GR_EMIS_001 ${EMISpath}/${EMISfile}
  setenv GR_EMIS_LAB_001 GRIDDED_EMIS
  setenv GR_EMIS_DTOVRD_001 F
  
  #> In-Line Point Emissions Files
  if ( $CTM_PT3DEMIS == 'Y' ) then
     setenv N_EMIS_PT 7          #> Number of elevated source groups

     set STKCASEE = 4CALIF1_cmaq_saprc07TB_2011eh_saprc_10g  # In-line Emission Rate File Suffix
     set STKCASEG = 4CALIF1_2011eh_saprc_10g                 # Stack parameter File Suffix

     setenv STK_GRPS_001 $IN_PTpath/ptnonipm/stack_groups_ptnonipm_${STKCASEG}.ncf
     setenv STK_GRPS_002 $IN_PTpath/ptegu/stack_groups_ptegu_${STKCASEG}.ncf
     setenv STK_GRPS_003 $IN_PTpath/othpt/stack_groups_othpt_${STKCASEG}.ncf
     setenv STK_GRPS_004 $IN_PTpath/wildfire/stack_groups_wildfire_${YYYYMMDD}_${STKCASEG}.ncf
     setenv STK_GRPS_005 $IN_PTpath/rxfire/stack_groups_rxfire_${YYYYMMDD}_${STKCASEG}.ncf
     setenv STK_GRPS_006 $IN_PTpath/pt_oilgas/stack_groups_pt_oilgas_${STKCASEG}.ncf
     setenv STK_GRPS_007 $IN_PTpath/ship_int_cmv_c3/stack_groups_ship_int_cmv_c3_${STKCASEG}.ncf
     setenv LAYP_STTIME $STTIME
     setenv LAYP_NSTEPS $NSTEPS

     setenv STK_EMIS_001 $IN_PTpath/ptnonipm/inln_mole_ptnonipm_${mwdss_Y}_${STKCASEE}.ncf
     setenv STK_EMIS_002 $IN_PTpath/ptegu/inln_mole_ptegu_${YYYYMMDD}_${STKCASEE}.ncf
     setenv STK_EMIS_003 $IN_PTpath/othpt/inln_mole_othpt_${mwdss_N}_${STKCASEE}.ncf
     setenv STK_EMIS_004 $IN_PTpath/wildfire/inln_mole_wildfire_${YYYYMMDD}_${STKCASEE}.ncf
     setenv STK_EMIS_005 $IN_PTpath/rxfire/inln_mole_rxfire_${YYYYMMDD}_${STKCASEE}.ncf
     setenv STK_EMIS_006 $IN_PTpath/pt_oilgas/inln_mole_pt_oilgas_${mwdss_Y}_${STKCASEE}.ncf
     setenv STK_EMIS_007 $IN_PTpath/ship_int_cmv_c3/inln_mole_ship_int_cmv_c3_${aveday_N}_${STKCASEE}.ncf
     setenv LAYP_STDATE $YYYYJJJ

     # Label Each Emissions Stream
     setenv STK_EMIS_LAB_001 POINT_NONEGU
     setenv STK_EMIS_LAB_002 POINT_EGU
     setenv STK_EMIS_LAB_003 POINT_OTHER
     setenv STK_EMIS_LAB_004 POINT_FIRES
     setenv STK_EMIS_LAB_005 PT_RXFIRE
     setenv STK_EMIS_LAB_006 POINT_OILGAS
     setenv STK_EMIS_LAB_007 PT_MARINE

     # Allow CMAQ to Use Point Source files with dates that do not
     # match the internal model date
     setenv STK_EM_DTOVRD_001 T
     setenv STK_EM_DTOVRD_002 T
     setenv STK_EM_DTOVRD_003 T
     setenv STK_EM_DTOVRD_004 T
     setenv STK_EM_DTOVRD_005 T
     setenv STK_EM_DTOVRD_006 T
     setenv STK_EM_DTOVRD_007 T
 
  endif
 
  #> Lightning NOx configuration
  if ( $CTM_LTNG_NO == 'Y' ) then
     setenv LTNGNO "InLine"    #> set LTNGNO to "Inline" to activate in-line calculation

  #> In-line lightning NOx options
     setenv USE_NLDN  Y        #> use hourly NLDN strike file [ default: Y ]
     if ( $USE_NLDN == Y ) then
        setenv NLDN_STRIKES ${IN_LTpath}/NLDN.12US1.${YYYYMMDD}.ioapi
     else
        setenv LOG_START 2.0   #> RC value to transit linear to log linear
     endif
     setenv LTNGPARMS_FILE ${IN_LTpath}/LTNG_AllParms_12US1.ncf #> lightning parameter file; ignore if LTNGPARAM = N
  endif

  #> In-line biogenic emissions configuration
  if ( $CTM_BIOGEMIS == 'Y' ) then   
     set IN_BEISpath = ${INPDIR}/surface
     set GSPROpath   = ${BLD}
     setenv GSPRO      $BLD/gspro_biogenics.txt
     setenv B3GRD      $IN_BEISpath/b3grd_4CALIF1_2011en_cb6_10.ncf
     setenv BIOSW_YN   Y     #> use frost date switch [ default: Y ]
     setenv BIOSEASON  $IN_BEISpath/bioseason.cmaq.2010j_4CALIF1.ncf_full #> ignore season switch file if BIOSW_YN = N
     setenv SUMMER_YN  Y     #> Use summer normalized emissions? [ default: Y ]
     setenv PX_VERSION Y     #> MCIP is PX version? [ default: N ]
     setenv SOILINP    $OUTDIR/CCTM_SOILOUT_${RUNID}_${YESTERDAY}.nc
                             #> Biogenic NO soil input file; ignore if INITIAL_RUN = Y
  endif

  #> Windblown dust emissions configuration
  if ( $CTM_WB_DUST == 'Y' ) then
     # Input variables for BELD3 Landuse option
     setenv DUST_LU_1  $LUpath/beld3_4CALIF1_output_a.ncf
     setenv DUST_LU_2  $LUpath/beld3_4CALIF1_output_tot.ncf
     setenv MODIS_FPAR $LUpath/MODIS_FPAR_LAI_daily_4CALIF1.nc

     if ( $CTM_ERODE_AGLAND == 'Y' ) then
        setenv CROPMAP01 $LUpath/BeginPlanting_4CALIF1.nc
        setenv CROPMAP04 $LUpath/EndPlanting_4CALIF1.nc
        setenv CROPMAP08 $LUpath/EndHarvesting_4CALIF1.nc
     endif
  endif

  #> In-line Sea Spray emisisions configuration
  setenv OCEAN_1 ${SZpath}/surf_4calif1.ncf

  #> Bidirectional ammonia configuration
  if ( $CTM_ABFLUX == 'Y' ) then
     setenv E2C_Soilfile  ${LUpath}/epic/US1_12km_soil.nc
     setenv E2C_Fertfile  ${LUpath}/epic/US1_12km_time${YYYYMMDD}.nc    
     setenv B4LU_file     ${LUpath}/beld4_12kmCONUS_2006nlcd.ncf
     setenv E2C_SOIL ${E2C_Soilfile}
     setenv E2C_FERT ${E2C_Fertfile}
     setenv BELD4_LU ${B4LU_file}
  endif

# =====================================================================
#> Output Files
# =====================================================================
  #> set output file name extensions
  setenv CTM_APPL ${RUNID}_${YYYYMMDD} 
  #> set output file names
  setenv S_CGRID         "$OUTDIR/CCTM_CGRID_${CTM_APPL}.nc"         #> 3D Inst. Concenctrations
  setenv CTM_CONC_1      "$OUTDIR/CCTM_CONC_${CTM_APPL}.nc -v"       #> On-Hour Concentrations
  setenv A_CONC_1        "$OUTDIR/CCTM_ACONC_${CTM_APPL}.nc -v"      #> Hourly Avg. Concentrations
  setenv MEDIA_CONC      "$OUTDIR/CCTM_MEDIA_CONC_${CTM_APPL}.nc -v" #> NH3 Conc. in Media
  setenv CTM_DRY_DEP_1   "$OUTDIR/CCTM_DRYDEP_${CTM_APPL}.nc -v"     #> Hourly Dry Deposition
  setenv CTM_DEPV_DIAG   "$OUTDIR/CCTM_DEPV_${CTM_APPL}.nc -v"       #> Dry Deposition Velocities
  setenv CTM_PT3D_DIAG   "$OUTDIR/CCTM_PT3D_${CTM_APPL}.nc -v"       #>
  setenv B3GTS_S         "$OUTDIR/CCTM_B3GTS_S_${CTM_APPL}.nc -v"    #> Biogenic Emissions
  setenv SOILOUT         "$OUTDIR/CCTM_SOILOUT_${CTM_APPL}.nc"       #> Soil Emissions
  setenv CTM_WET_DEP_1   "$OUTDIR/CCTM_WETDEP1_${CTM_APPL}.nc -v"    #> Wet Dep From All Clouds
  setenv CTM_WET_DEP_2   "$OUTDIR/CCTM_WETDEP2_${CTM_APPL}.nc -v"    #> Wet Dep From SubGrid Clouds
  setenv CTM_PMDIAG_1    "$OUTDIR/CCTM_PMDIAG_${CTM_APPL}.nc -v"     #> On-Hour Particle Diagnostics
  setenv CTM_APMDIAG_1   "$OUTDIR/CCTM_APMDIAG_${CTM_APPL}.nc -v"    #> Hourly Avg. Particle Diagnostic
  setenv CTM_RJ_1        "$OUTDIR/CCTM_PHOTDIAG1_${CTM_APPL}.nc -v"  #> 2D Surface Summary from Inline Photolysis
  setenv CTM_RJ_2        "$OUTDIR/CCTM_PHOTDIAG2_${CTM_APPL}.nc -v"  #> 3D Photolysis Rates 
  setenv CTM_RJ_3        "$OUTDIR/CCTM_PHOTDIAG3_${CTM_APPL}.nc -v"  #> 3D Optical and Radiative Results from Photolysis
  setenv CTM_SSEMIS_1    "$OUTDIR/CCTM_SSEMIS_${CTM_APPL}.nc -v"     #> Sea Spray Emissions
  setenv CTM_DUST_EMIS_1 "$OUTDIR/CCTM_DUSTEMIS_${CTM_APPL}.nc -v"   #> Dust Emissions
  setenv CTM_IPR_1       "$OUTDIR/CCTM_PA_1_${CTM_APPL}.nc -v"       #> Process Analysis
  setenv CTM_IPR_2       "$OUTDIR/CCTM_PA_2_${CTM_APPL}.nc -v"       #> Process Analysis
  setenv CTM_IPR_3       "$OUTDIR/CCTM_PA_3_${CTM_APPL}.nc -v"       #> Process Analysis
  setenv CTM_IRR_1       "$OUTDIR/CCTM_IRR_1_${CTM_APPL}.nc -v"      #> Chem Process Analysis
  setenv CTM_IRR_2       "$OUTDIR/CCTM_IRR_2_${CTM_APPL}.nc -v"      #> Chem Process Analysis
  setenv CTM_IRR_3       "$OUTDIR/CCTM_IRR_3_${CTM_APPL}.nc -v"      #> Chem Process Analysis
  setenv CTM_DRY_DEP_MOS "$OUTDIR/CCTM_DDMOS_${CTM_APPL}.nc -v"      #> Dry Dep
  setenv CTM_DRY_DEP_FST "$OUTDIR/CCTM_DDFST_${CTM_APPL}.nc -v"      #> Dry Dep
  setenv CTM_DEPV_MOS    "$OUTDIR/CCTM_DEPVMOS_${CTM_APPL}.nc -v"    #> Dry Dep Velocity
  setenv CTM_DEPV_FST    "$OUTDIR/CCTM_DEPVFST_${CTM_APPL}.nc -v"    #> Dry Dep Velocity
  setenv CTM_VDIFF_DIAG  "$OUTDIR/CCTM_VDIFF_DIAG_${CTM_APPL}.nc -v" #> Vertical Dispersion Diagnostic
  setenv CTM_VSED_DIAG   "$OUTDIR/CCTM_VSED_DIAG_${CTM_APPL}.nc -v"  #> Particle Grav. Settling Velocity
  setenv CTM_LTNGDIAG_1  "$OUTDIR/CCTM_LTNGHRLY_${CTM_APPL}.nc -v"   #> Hourly Avg Lightning NO
  setenv CTM_LTNGDIAG_2  "$OUTDIR/CCTM_LTNGCOL_${CTM_APPL}.nc -v"    #> Column Total Lightning NO
  setenv CTM_VEXT_1      "$OUTDIR/CCTM_VEXT_${CTM_APPL}.nc -v"       #> On-Hour Concentrations at select sites

  #> set floor file (neg concs)
  setenv FLOOR_FILE ${OUTDIR}/FLOOR_${CTM_APPL}.txt

  #> create output directory 
  if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

  #> look for existing log files and output files
  set log_test = `ls CTM_LOG_???.${CTM_APPL}`
  set OUT_FILES = (${FLOOR_FILE} ${S_CGRID} ${CTM_CONC_1} ${A_CONC_1} ${MEDIA_CONC}         \
             ${CTM_DRY_DEP_1} $CTM_DEPV_DIAG $CTM_PT3D_DIAG $B3GTS_S $SOILOUT $CTM_WET_DEP_1\
             $CTM_WET_DEP_2 $CTM_PMDIAG_1 $CTM_APMDIAG_1             \
             $CTM_RJ_1 $CTM_RJ_2 $CTM_RJ_3 $CTM_SSEMIS_1 $CTM_DUST_EMIS_1 $CTM_IPR_1 $CTM_IPR_2       \
             $CTM_IPR_3 $CTM_IRR_1 $CTM_IRR_2 $CTM_IRR_3 $CTM_DRY_DEP_MOS                   \
             $CTM_DRY_DEP_FST $CTM_DEPV_MOS $CTM_DEPV_FST $CTM_VDIFF_DIAG $CTM_VSED_DIAG    \
             $CTM_LTNGDIAG_1 $CTM_LTNGDIAG_2)
  set OUT_FILES = `echo $OUT_FILES | sed "s; -v;;g" `
  #echo $OUT_FILES
  set out_test = `ls $OUT_FILES` 

  #> delete previous output if requested
  if ( $DISP == 'delete' ) then
     #> remove previous log files
     echo " ancillary log files being deleted"
     foreach file ( $log_test )
        echo " deleting $file"
        /bin/rm -f $file  
     end

     #> remove previous output files
     echo " output files being deleted"
     foreach file ( $out_test )
        echo " deleting $file"
        /bin/rm -f $file  
     end
     /bin/rm -f ${OUTDIR}/CCTM_EMDIAG*${RUNID}_${YYYYMMDD}.nc

  else
     #> error if previous log files exist
     if ( "$log_test" != "" ) then
       echo "*** Logs exist - run ABORTED ***"
       echo "*** To overide, set $DISP == delete in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif

     #> error if previous output files exist
     if ( "$out_test" != "" ) then
       echo "*** Output Files Exist - run will be ABORTED ***"
       foreach file ( $out_test )
          echo " cannot delete $file"
       end
       echo "*** To overide, set $DISP == delete in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif
  endif

  #> for the run control ...
  setenv CTM_STDATE      $YYYYJJJ
  setenv CTM_STTIME      $STTIME
  setenv CTM_RUNLEN      $NSTEPS
  setenv CTM_TSTEP       $TSTEP
  setenv INIT_GASC_1 $ICpath/$ICFILE
  setenv INIT_AERO_1 $INIT_GASC_1
  setenv INIT_NONR_1 $INIT_GASC_1
  setenv INIT_TRAC_1 $INIT_GASC_1
  setenv BNDY_GASC_1 $BCpath/$BCFILE
  setenv BNDY_AERO_1 $BNDY_GASC_1
  setenv BNDY_NONR_1 $BNDY_GASC_1
  setenv BNDY_TRAC_1 $BNDY_GASC_1
  setenv OMI $OMIpath/$OMIfile
  setenv OPTICS_DATA $OMIpath/$OPTfile
  #setenv XJ_DATA $JVALpath/$JVALfile
  set TR_DVpath = $METpath
  set TR_DVfile = $MET_CRO_2D
 
  #> species defn & photolysis
  setenv gc_matrix_nml ${NMLpath}/GC_$MECH.nml
  setenv ae_matrix_nml ${NMLpath}/AE_$MECH.nml
  setenv nr_matrix_nml ${NMLpath}/NR_$MECH.nml
  setenv tr_matrix_nml ${NMLpath}/Species_Table_TR_0.nml
 
  #> check for photolysis input data
  setenv CSQY_DATA ${NMLpath}/CSQY_DATA_$MECH

  if (! (-e $CSQY_DATA ) ) then
     echo " $CSQY_DATA  not found "
     exit 1
  endif
  if (! (-e $OPTICS_DATA ) ) then
     echo " $OPTICS_DATA  not found "
     exit 1
  endif

# ===================================================================
#> Execution Portion
# ===================================================================

  #> Print attributes of the executable
  ls -l $BLD/$EXEC; size $BLD/$EXEC
  unlimit
  limit

  date
 
  #> Executable call for single PE, uncomment to invoke
  # /usr/bin/time  $BLD/$EXEC

  #> Executable call for multi PE, configure for your system 
  # set MPI = /usr/local/intel/impi/3.2.2.006/bin64
  # set MPIRUN = $MPI/mpirun
  time mpirun -np $NPROCS $BLD/$EXEC

  date

# ===================================================================
#> Finalize Run for This Day and Loop to Next Day
# ===================================================================

  #> Save Log Files and Move on to Next Simulation Day
  if (! -e $LOGDIR ) then
    mkdir $LOGDIR
  endif
  mv CTM_LOG_???.${CTM_APPL} $LOGDIR

  #> The next simulation day will, by definition, be a restart
  setenv NEW_START false

  #> Increment both Gregorian and Julian Days
  set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
  set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

end  #Loop to the next Simulation Day

exit
