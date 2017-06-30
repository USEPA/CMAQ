#!/bin/csh -f

# ====================== CCTMv5.2 Run Script ======================== 
# Usage: run.cctm >&! cctm_v52b.log &                                
#
# To report problems or request help with this script/program:     
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================  

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
 setenv compilerVrsn 13.1

#> Source the config.cmaq file to set the build environment
 cd ../..
 source ./config_cmaq.csh
 cd CCTM/scripts

#> Set General Parameters for Configuring the Simulation
 set VRSN      = v52               #> Code Version
 set PROC      = mpi               #> serial or mpi
 set MECH      = cb6r3_ae6_aq      #> Mechanism ID
 set EMIS      = 2013ef            #> Emission Inventory Details
 set APPL      = SE52BENCH         #> Application Name (e.g. Gridname)
                                                       
#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 setenv RUNID  ${VRSN}_${compiler}_${APPL}

#> Set the build directory (this is where the CMAQ executable
#> is located by default).
 set BLD      = ${CMAQ_HOME}/CCTM/scripts/BLD_CCTM_${VRSN}_${compiler}
 set EXEC     = CCTM_${VRSN}.exe  
 cat $BLD/CCTM_${VRSN}.cfg; echo "    "; set echo

#> Set Working, Input, and Output Directories
 setenv WORKDIR ${CMAQ_HOME}/CCTM/scripts       #> Working Directory. Where the runscript is.
 setenv OUTDIR  ${CMAQ_DATA}/output_CCTM_${RUNID} #> Output Directory
 setenv INPDIR  ${CMAQ_DATA}/SE52BENCH/single_day/cctm_input  #> Input Directory
 setenv LOGDIR  ${OUTDIR}          #> Log Directory Location
 setenv NMLpath ${BLD}             #> Location of Namelists. Common places are: 
                                   #>   ${WORKDIR} | ${CCTM_SRC}/MECHS/${MECH} | ${BLD}

# =====================================================================
#> CCTM Configuration Options
# =====================================================================

#> Set Start and End Days for looping
 setenv NEW_START TRUE            #> Set to FALSE for model restart
 set START_DATE = "2011-07-01"     #> beginning date (July 1, 2011)
 set END_DATE   = "2011-07-01"     #> ending date    (July 14, 2011)

#> Set Timestepping Parameters
set STTIME     = 000000            #> beginning GMT time (HHMMSS)
set NSTEPS     = 240000            #> time duration (HHMMSS) for this run
set TSTEP      = 010000            #> output time step interval (HHMMSS)

#> Horizontal domain decomposition
if ( $PROC == serial ) then
   setenv NPCOL_NPROW "1 1"; set NPROCS   = 1 # single processor setting
else
   @ NPCOL  =  4; @ NPROW =  2
   @ NPROCS = $NPCOL * $NPROW
   setenv NPCOL_NPROW "$NPCOL $NPROW"; 
endif

#> Vertical extent
set NZ         = 35

#setenv LOGFILE $CMAQ_HOME/$RUNID.log  #> log file name; uncomment to write standard output to a log, otherwise write to screen

setenv GRID_NAME SE52BENCH         #> check GRIDDESC file for GRID_NAME options
setenv GRIDDESC $INPDIR/GRIDDESC   #> grid description file

#> Output Species and Layer Options
#>   CONC file species; comment or set to "ALL" to write all species to CONC
     #setenv CONC_SPCS "O3 NO ANO3I ANO3J NO2 FORM ISOP ANH4J ASO4I ASO4J" 
     #setenv CONC_BLEV_ELEV " 1 4" #> CONC file layer range; comment to write all layers to CONC

#>   ACONC file species; comment or set to "ALL" to write all species to ACONC
     #setenv AVG_CONC_SPCS "O3 NO CO NO2 ASO4I ASO4J NH3" 
     setenv AVG_CONC_SPCS "ALL" 
     setenv ACONC_BLEV_ELEV " 1 1" #> ACONC file layer range; comment to write all layers to ACONC
     #setenv ACONC_END_TIME Y      #> override default beginning ACON timestamp [ default: N ]

setenv EXECUTION_ID $EXEC    #> define the model execution id

#> Sychronization Time Step and Tolerance Options
setenv CTM_MAXSYNC 300       #> max sync time step (sec) [ default: 720 ]
setenv CTM_MINSYNC  60       #> min sync time step (sec) [ default: 60 ]
setenv SIGMA_SYNC_TOP 0.7    #> top sigma level thru which sync step determined [ default: 0.7 ] 
#setenv ADV_HDIV_LIM 0.95     #> maximum horiz. div. limit for adv step adjust [ default: 0.9 ]
setenv CTM_ADV_CFL 0.95      #> max CFL [ default: 0.75]
#setenv RB_ATOL 1.0E-09       #> global ROS3 solver abs tol [ default: 1.0E-07 ] 

#> Science Options
setenv CTM_WB_DUST Y         #> use inline windblown dust emissions [ default: Y ]
setenv CTM_ERODE_AGLAND Y    #> use agricultural activity for windblown dust 
                             #>    [ default: N ]; ignore if CTM_WB_DUST = N
setenv CTM_WBDUST_BELD BELD3 #> landuse database for identifying dust source regions 
                             #>    [ default: BELD3 ]; ignore if CTM_WB_DUST = N 
setenv CTM_LTNG_NO Y         #> turn on lightning NOx [ default: N ]
setenv CTM_WVEL Y            #> save derived vertical velocity component to conc 
                             #>    file [ default: N ]
setenv KZMIN Y               #> use Min Kz option in edyintb [ default: Y ], 
                             #>    otherwise revert to Kz0UT
setenv CTM_ILDEPV Y          #> calculate in-line deposition velocities [ default: Y ]
setenv CTM_MOSAIC N          #> landuse specific deposition velocities [ default: N ]
setenv CTM_FST N             #> mosaic method to get land-use specific stomatal flux 
                             #>    [ default: N ]
setenv CTM_ABFLUX Y          #> ammonia bi-directional flux for in-line deposition 
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
setenv IOAPI_OFFSET_64 NO    #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
setenv CTM_EMISCHK N         #> Abort CMAQ if missing surrogates from emissions Input files

#> Aerosol Diagnostic Controls
setenv CTM_AVISDIAG Y        #> Aerovis diagnostic file [ default: N ]
setenv CTM_PMDIAG Y          #> What is this [ default: Y ]
setenv CTM_APMDIAG Y         #> What is this [ default: Y ]
setenv APMDIAG_BLEV_ELEV "1 3" #> layer range for average pmdiag
setenv APMDIAG_BLEV_ELEV ""  #> layer range for average pmdiag = NLAYS
setenv AVG_FILE_ENDTIME N    #> What is this [ default: N ]

#> Diagnostic Output Flags
setenv CTM_CKSUM Y           #> cksum report [ default: Y ]
setenv CLD_DIAG Y            #> cloud diagnostic file [ default: N ]
setenv CTM_AERDIAG Y         #> aerosol diagnostic file [ default: N ]
setenv CTM_PHOTDIAG Y        #> photolysis diagnostic file [ default: N ]
setenv CTM_SSEMDIAG Y        #> sea-salt emissions diagnostic file [ default: N ]
setenv CTM_DUSTEM_DIAG Y     #> windblown dust emissions diagnostic file [ default: N ]; ignore if CTM_WB_DUST = N
setenv CTM_DEPV_FILE Y       #> deposition velocities diagnostic file [ default: N ]
setenv VDIFF_DIAG_FILE Y     #> vdiff & possibly aero grav. sedimentation diagnostic file [ default: N ]
setenv LTNGDIAG Y            #> lightning diagnostic file [ default: N ]
setenv CTM_AOD Y             #> AOD diagnostic file [ default: N ]
setenv B3GTS_DIAG Y          #> beis mass emissions diagnostic file [ default: N ]
setenv PT3DDIAG N            #> optional 3d point source emissions diagnostic file [ default: N]; ignore if CTM_PT3DEMIS = N
setenv PT3DFRAC N            #> optional layer fractions diagnostic (play) file(s) [ default: N]; ignore if CTM_PT3DEMIS = N
setenv REP_LAYER_MIN -1      #> Minimum layer for reporting plume rise info [ default: -1 ]

set DISP = delete            #> [ delete | keep ] existing output files


# =====================================================================
#> Input Directories and Filenames
# =====================================================================

set ICpath    = $INPDIR/icbc              #> initial conditions input directory 
set BCpath    = $INPDIR/icbc              #> boundary conditions input directory
set EMISpath  = $INPDIR/emis/gridded_area #> surface emissions input directory
set IN_PTpath = $INPDIR/emis/inln_point   #> elevated emissions input directory (in-line point only)
set IN_LTpath = $INPDIR/lightning         #> lightning NOx input directory
set METpath   = $INPDIR/met/mcip          #> meteorology input directory 
#set JVALpath  = $INPDIR/jproc            #> offline photolysis rate table directory
set OMIpath   = $BLD                      #> ozone columne data for the photolysis model
set LUpath    = $INPDIR/land              #> BELD landuse data for windblown dust model
set SZpath    = $INPDIR/land              #> surf zone file for in-line seasalt emissions

set ICBC_CASE = 2013ef_v6_13g_s07         #> Version label for the ICBCs
set EMIS_CASE = 2013ef_v6_13g_s07_hg      #> Version Label for the Emissions

# =====================================================================
#> Begin Loop Through Simulation Days
# =====================================================================

set TODAYG = ${START_DATE}
set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ

  #> Retrieve Calendar day Information
  set YYYYMMDD = `date -ud "${TODAYG}" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYMMDD
  set YYMMDD = `date -ud "${TODAYG}" +%y%m%d`   #> Convert YYYY-MM-DD to YYMMDD
  set YYYYJJJ = $TODAYJ

  #> Calculate Yesterday's Date
  set YESTERDAY = `date -ud "${TODAYG}-1days" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYJJJ

# =====================================================================
#> Input Files (Some are Day-Dependent)
# =====================================================================

  #> Initial conditions
  if ($NEW_START == true || $NEW_START == TRUE ) then
     setenv ICFILE ICON_20110630_bench.nc
     setenv INITIAL_RUN Y #related to restart soil information file
     rm -rf $LOGDIR/CTM_LOG*${RUNID}*  # Remove all Log Files Since this is a new start
     mkdir -p $OUTDIR
  else
     set ICpath = $OUTDIR
     setenv ICFILE CCTM_CGRID_${RUNID}_${YESTERDAY}.nc
     setenv INITIAL_RUN N
  endif

  #> Boundary conditions
  set BCFILE = BCON_${YYYYMMDD}_bench.nc

  #> Off-line photolysis rates 
  #set JVALfile  = JTABLE_${YYYYJJJ}

  #> Ozone column data
  set OMIfile   = OMI_1979_to_2015.dat

  #> Optics file
  set OPTfile = PHOT_OPTICS.dat

  #> MCIP meteorology files 
  setenv GRID_BDY_2D $METpath/GRIDBDY2D_${YYMMDD}.nc
  setenv GRID_CRO_2D $METpath/GRIDCRO2D_${YYMMDD}.nc
  setenv GRID_CRO_3D $METpath/GRIDCRO3D_${YYMMDD}.nc
  setenv GRID_DOT_2D $METpath/GRIDDOT2D_${YYMMDD}.nc
  setenv MET_CRO_2D $METpath/METCRO2D_${YYMMDD}.nc
  setenv MET_CRO_3D $METpath/METCRO3D_${YYMMDD}.nc
  setenv MET_DOT_3D $METpath/METDOT3D_${YYMMDD}.nc
  setenv MET_BDY_3D $METpath/METBDY3D_${YYMMDD}.nc

  setenv LAYER_FILE $MET_CRO_3D  # Deprecated: MET_CRO_3D is now read directly in CCTM


  #> Emissions files 
  if ( $CTM_PT3DEMIS == 'N' ) then
     #> Offline 3d emissions file name
     set EMISfile  = emis_mole_all_${YYYYMMDD}_cb6_bench.nc
  else
     #> In-line emissions configuration
     set STKCASEG = 12US1_2011ek_cb6cmaq_v6_11g           # Stack Group Version Label
     set STKCASEE = 12US1_cmaq_cb6e51_2011ek_cb6cmaq_v6_11g   # Stack Emission Version Label
     set EMISfile  = emis_mole_all_${YYYYMMDD}_cb6_bench.nc #> Surface emissions
     setenv NPTGRPS 5          #> Number of elevated source groups

     setenv STK_GRPS_01 $IN_PTpath/stack_groups/stack_groups_ptnonipm_${STKCASEG}.nc
     setenv STK_GRPS_02 $IN_PTpath/stack_groups/stack_groups_ptegu_${STKCASEG}.nc
     setenv STK_GRPS_03 $IN_PTpath/stack_groups/stack_groups_othpt_${STKCASEG}.nc
     setenv STK_GRPS_04 $IN_PTpath/stack_groups/stack_groups_ptfire_${YYYYMMDD}_${STKCASEG}.nc
     setenv STK_GRPS_05 $IN_PTpath/stack_groups/stack_groups_pt_oilgas_${STKCASEG}.nc
     setenv LAYP_STTIME $STTIME
     setenv LAYP_NSTEPS $NSTEPS

     setenv STK_EMIS_01 $IN_PTpath/ptnonipm/inln_mole_ptnonipm_${YYYYMMDD}_${STKCASEE}.nc
     setenv STK_EMIS_02 $IN_PTpath/ptegu/inln_mole_ptegu_${YYYYMMDD}_${STKCASEE}.nc
     setenv STK_EMIS_03 $IN_PTpath/othpt/inln_mole_othpt_${YYYYMMDD}_${STKCASEE}.nc
     setenv STK_EMIS_04 $IN_PTpath/ptfire/inln_mole_ptfire_${YYYYMMDD}_${STKCASEE}.nc
     setenv STK_EMIS_05 $IN_PTpath/pt_oilgas/inln_mole_pt_oilgas_${YYYYMMDD}_${STKCASEE}.nc
     setenv LAYP_STDATE $YYYYJJJ
  endif

  #> Lightning NOx configuration
  if ( $CTM_LTNG_NO == 'Y' ) then
     setenv LTNGNO "InLine"    #> set LTNGNO to "Inline" to activate in-line calculation

  #> In-line lightning NOx options
     setenv USE_NLDN  Y        #> use hourly NLDN strike file [ default: Y ]
     setenv LTNGPARAM Y        #> use lightning parameter file [ default: Y ]
     if ( $USE_NLDN == Y ) then
        setenv NLDN_STRIKES $INPDIR/lightning/NLDN.12US1.${YYYYMMDD}_bench.nc
     else
        setenv LOG_START 2.0   #> RC value to transit linear to log linear
     endif
     setenv LTNGPARMS_FILE $INPDIR/lightning/LTNG_AllParms_12US1_bench.nc #> lightning parameter file; ignore if LTNGPARAM = N
  endif


  #> In-line biogenic emissions configuration
  if ( $CTM_BIOGEMIS == 'Y' ) then   
     set IN_BEISpath = ${INPDIR}/land
     set GSPROpath   = ${IN_BEISpath}
     setenv GSPRO      $GSPROpath/gspro_biogenics_1mar2017.txt
     setenv B3GRD      $IN_BEISpath/b3grd_bench.nc
     setenv BIOG_SPRO  B10C6 #> speciation profile to use for biogenics
     setenv BIOSW_YN   N     #> use frost date switch [ default: Y ]
     setenv BIOSEASON  $IN_BEISpath/bioseason.12US1.2006.09apr2012_bench.nc #> ignore season switch file if BIOSW_YN = N
     setenv SUMMER_YN  N     #> Use summer normalized emissions? [ default: Y ]
     setenv PX_VERSION Y     #> MCIP is PX version? [ default: N ]
     setenv INITIAL_RUN Y    #> non-existent or not using SOILINP [ default: N ]; default uses SOILINP
     setenv SOILINP    $OUTDIR/CCTM_SOILOUT_${RUNID}_${YESTERDAY}.nc
                             #> Biogenic NO soil input file; ignore if INITIAL_RUN = Y
  endif

  #> Windblown dust emissions configuration
  if ( $CTM_WB_DUST == 'Y' ) then
     # Input variables for BELD3 Landuse option
     setenv DUST_LU_1 $LUpath/beld3_12US1_459X299_output_a_bench.nc
     setenv DUST_LU_2 $LUpath/beld4_12US1_459X299_output_tot_bench.nc
     setenv MODIS_FPAR $LUpath/modis_bench.nc

     if ( $CTM_ERODE_AGLAND == 'Y' ) then
        setenv CROPMAP01 ${INPDIR}/land/BeginPlanting_12km_bench.nc
        setenv CROPMAP04 ${INPDIR}/land/EndPlanting_12km_bench.nc
        setenv CROPMAP08 ${INPDIR}/land/EndHarvesting_12km_bench.nc
     endif
  endif

  #> In-line sea salt emisisions configuration
  setenv OCEAN_1 $SZpath/12US1_surf_bench.nc #> horizontal grid-dependent surf zone file

  #> Bidiretional ammonia configuration
  if ( $CTM_ABFLUX == 'Y' ) then
     setenv E2C_Soilfile  ${INPDIR}/land/2011_US1_soil_bench.nc       
     setenv E2C_Fertfile  ${INPDIR}/land/2011_US1_time${YYYYMMDD}_bench.nc    
     setenv B4LU_file     ${INPDIR}/land/beld4_12kmCONUS_2006nlcd_bench.nc    
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
  setenv CTM_VIS_1       "$OUTDIR/CCTM_PMVIS_${CTM_APPL}.nc -v"      #> On-Hour Visibility
  setenv CTM_AVIS_1      "$OUTDIR/CCTM_APMVIS_${CTM_APPL}.nc -v"     #> Hourly-Averaged Visibility
  setenv CTM_PMDIAG_1    "$OUTDIR/CCTM_PMDIAG_${CTM_APPL}.nc -v"     #> On-Hour Particle Diagnostics
  setenv CTM_APMDIAG_1   "$OUTDIR/CCTM_APMDIAG_${CTM_APPL}.nc -v"    #> Hourly Avg. Particle Diagnostic
  setenv CTM_RJ_1        "$OUTDIR/CCTM_PHOTDIAG1_${CTM_APPL}.nc -v"  #> Photolysis Rxn Diagnostics
  setenv CTM_RJ_2        "$OUTDIR/CCTM_PHOTDIAG2_${CTM_APPL}.nc -v"  #> Photolysis Rates Output
  setenv CTM_SSEMIS_1    "$OUTDIR/CCTM_SSEMIS.${CTM_APPL}.nc -v"     #> Sea Spray Emissions
  setenv CTM_DUST_EMIS_1 "$OUTDIR/CCTM_DUSTEMIS.${CTM_APPL}.nc -v"   #> Dust Emissions
  setenv CTM_IPR_1       "$OUTDIR/CCTM_PA_1_${CTM_APPL}.nc -v"       #> Process Analysis
  setenv CTM_IPR_2       "$OUTDIR/CCTM_PA_2_${CTM_APPL}.nc -v"       #> Process Analysis
  setenv CTM_IPR_3       "$OUTDIR/CCTM_PA_3_${CTM_APPL}.nc -v"       #> Process Analysis
  setenv CTM_IRR_1       "$OUTDIR/CCTM_IRR_1_${CTM_APPL}.nc -v"      #> Chem Process Analysis
  setenv CTM_IRR_2       "$OUTDIR/CCTM_IRR_2_${CTM_APPL}.nc -v"      #> Chem Process Analysis
  setenv CTM_IRR_3       "$OUTDIR/CCTM_IRR_3_${CTM_APPL}.nc -v"      #> Chem Process Analysis
  setenv CTM_DRY_DEP_MOS "$OUTDIR/CCTM_DDMOS_${CTM_APPL}.nc -v"      #> Dry Dep
  setenv CTM_DRY_DEP_FST "$OUTDIR/CCTM_DDFST_${CTM_APPL}.nc -v"      #> Dry Dep
  setenv CTM_DEPV_MOS    "$OUTDIR/CCTM_DEPVFST_${CTM_APPL}.nc -v"    #> Dry Dep Velocity
  setenv CTM_DEPV_FST    "$OUTDIR/CCTM_DEPVMOS_${CTM_APPL}.nc -v"    #> Dry Dep Velocity
  setenv CTM_VDIFF_DIAG  "$OUTDIR/CCTM_VDIFF_DIAG_${CTM_APPL}.nc -v" #> Vertical Dispersion Diagnostic
  setenv CTM_VSED_DIAG   "$OUTDIR/CCTM_VSED_DIAG_${CTM_APPL}.nc -v"  #> Particle Grav. Settling Velocity
  setenv CTM_AOD_1       "$OUTDIR/CCTM_AOD_DIAG_${CTM_APPL}.nc -v"   #> Aerosol Optical Depth Diagnostic
  setenv CTM_LTNGDIAG_1  "$OUTDIR/CCTM_LTNGHRLY_${CTM_APPL}.nc -v"   #> Hourly Avg Lightning NO
  setenv CTM_LTNGDIAG_2  "$OUTDIR/CCTM_LTNGCOL_${CTM_APPL}.nc -v"    #> Column Total Lightning NO

  #> set floor file (neg concs)
  setenv FLOOR_FILE ${OUTDIR}/FLOOR_${CTM_APPL}.txt

  #> create output directory 
  if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

  #> look for existing log files and output files
  set log_test = `ls CTM_LOG_???.${CTM_APPL}`
  set OUT_FILES = "${FLOOR_FILE} ${S_CGRID} ${CTM_CONC_1} ${A_CONC_1} ${MEDIA_CONC}         \
             ${CTM_DRY_DEP_1} $CTM_DEPV_DIAG $CTM_PT3D_DIAG $B3GTS_S $SOILOUT $CTM_WET_DEP_1\
             $CTM_WET_DEP_2 $CTM_VIS_1 $CTM_AVIS_1 $CTM_PMDIAG_1 $CTM_APMDIAG_1             \
             $CTM_RJ_1 $CTM_RJ_2 $CTM_SSEMIS_1 $CTM_DUST_EMIS_1 $CTM_IPR_1 $CTM_IPR_2       \
             $CTM_IPR_3 $CTM_IRR_1 $CTM_IRR_2 $CTM_IRR_3 $CTM_DRY_DEP_MOS                   \
             $CTM_DRY_DEP_FST $CTM_DEPV_MOS $CTM_DEPV_FST $CTM_VDIFF_DIAG $CTM_VSED_DIAG    \
             $CTM_AOD_1 $CTM_LTNGDIAG_1 $CTM_LTNGDIAG_2"
  set OUT_FILES = `echo $OUT_FILES | sed "s; -v;;g" `
  echo $OUT_FILES
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

  else
     #> remove previous log files
     if ( "$log_test" != "" ) then
       echo "*** Logs exist - run ABORTED ***"
       echo "*** To overide, set $DISP == delete in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif

     #> remove previous output files
     if ( "$out_test" != "" ) then
       echo "*** Output Files Exist - run will be ABORTED ***"
       foreach file ( $out_test )
          echo " cannot delete $file"
          /bin/rm -f $file  
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
  setenv EMIS_1 $EMISpath/$EMISfile
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
  time mpirun -r ssh -np $NPROCS $BLD/$EXEC

  date

# ===================================================================
#> Finalize Run for This Day and Loop to Next Day
# ===================================================================

  #> Save Log Files and Move on to Next Simulation Day
  mv CTM_LOG_???.${CTM_APPL} $LOGDIR

  #> The next simulation day will, by definition, be a restart
  setenv NEW_START false

  #> Increment both Gregorian and Julian Days
  set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
  set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

end  #Loop to the next Simulation Day

exit
