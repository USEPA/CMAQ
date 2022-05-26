#!/bin/csh -f

# ===================== WRF-CMAQ Run Script =========================
# Usage: run_cctm_Bench_2016_12SE1.WRFCMAQ.csh >& run_cctm_Bench_2016_12SE1.WRFCMAQ.log &
# Slurm Usage: sbatch run_cctm_Bench_2016_12SE1.WRFCMAQ.csh 
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================

set NPROCS = 64

set wrfv    = 4.3
set version = sw_feedback
set option  = 3

# ===================================================================
#> Runtime Environment Options
# ===================================================================

echo 'Start Model Run At ' `date`

#> Toggle Diagnostic Mode which will print verbose information to 
#> standard output
setenv CTM_DIAG_LVL 0 

#> Set General Parameters and Labels for Configuring the Simulation
set VRSN        = ${wrfv}533         #> Code Version
set PROC        = mpi                #> serial or mpi
set MECH        = cb6r3_ae7_aq       # Mechanism ID
set APPL        = SE53BENCH          #> Application Name (e.g. Domain)

#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
setenv RUNID  ${VRSN}_${APPL}

set EXEC      = wrf.exe

#> Output Each line of Runscript to Log File
 if ( $CTM_DIAG_LVL != 0 ) set echo

# Set Working, Input, and Output Directories
set WORKDIR     = [your install location]
set WRF_DIR     = $WORKDIR/BLD_WRFv4.3_CCTM_v533_gcc
set INPDIR      = [your_install_dir]/WRF-CMAQ/CMAQ_v5.3.3/data/CMAQv5.3.2_Benchmark_2Day_Input/2016_12SE1                 # Input directory
set OMIpath     = $WRF_DIR/cmaq                           # path optics related data files
set OUTPUT_ROOT = [your output directory location]                                # output root directory
set NMLpath     = $WRF_DIR/cmaq                           # path with *.nml file mechanism dependent
set EMISSCTRL   = $WRF_DIR/cmaq                           # path of Emissions Control File

set output_direct_name = WRFCMAQ-output-${version}

 echo ""
 echo "Working Directory is $WORKDIR"
 echo "Output Root Directory is $OUTPUT_ROOT"
 echo "Executable Name is $EXEC"

# =====================================================================
# WRF-CMAQ coupled Configuration Options
# =====================================================================

#> Set Start and End Days for looping
 setenv NEW_START TRUE             # Set to FALSE for model restart
 set START_DATE = "2016-07-01"     # beginning date (July 1, 2016)
 set END_DATE   = "2016-07-02"     # ending date    (July 14, 2016)

#> Set Timestepping Parameters
 set STTIME     = 000000           # beginning GMT time (HHMMSS)
 set NSTEPS     = 240000           # time duration (HHMMSS) for this run
 set TSTEP      = 010000           # output time step interval (HHMMSS)

 set resolution = 12000            # domain resolution in meter

set wrf_cmaq_option      =  ${option} # 0 = run WRF only
                                      # 1 = run WRF only and produce GRID and MET files as well
                                      # 2 = run WRF-CMAQ coupled model w/o producing GRID and MET files
                                      # 3 = run WRF-CMAQ coupled model w   producing GRID and MET files
set direct_sw_feedback   =    .true.   # direct Shortwave aerosol feedback effect [.false]
set wrf_cmaq_freq        =        5   # WRF-CMAQ couple model frequency [1]

set cont_from_spinup_run =        F   # indicates whether a wrf spinup run prior to the twoway model run
set wrf_tstep            =       60   # WRF model time-step
set NUM_LAND_USE_TYPE    =       40   # MODIS IS 20, USGS is 24, NCLD50 is 50, NCLD40 is 40
set radt                 =       20   # radiation module time step
set met_file_tstep       =    10000

set ltg_assim            =  .true.   # Option for lightning assimilation in Kain-Fritsch when cu_physics=1 [ .false. ]
set suppress_opt         =        0   # Suppression option if ltg assim used. 
                                      # 0 = nosuppress
                                      # 1 = fullsuppress
                                      # 2 = shallonly
setenv CTM_LTNG_OPTION            0   # 0 - use nothing
                                      # 1 - use WRF convective cloud calculation currently
                                      #     this only work with two-way coupled model
                                      # 2 - use lightning flashes data
                                      # 3 - use lightning parameter
                                      # 4 - use NOx emission data file

if ($$direct_sw_feedback == .true.) then
   set feedback = sf
else
   set feedback = nf
endif

set call_ratio = "${wrf_cmaq_freq}_1"

@ temp_res = $resolution / 1000
if (($resolution % 1000) == 0) then
   set scale = "${temp_res}km"
else
   set scale = "${resolution}m"
endif

# set output_direct_name = output_${scale}_${feedback}_rrtmg_${radt}_${call_ratio}_v${VRSN}

set OUTDIR = $OUTPUT_ROOT/$output_direct_name   # output files and directories

#> Keep or Delete Existing Output Files
set CLOBBER_DATA = TRUE 

setenv PRINT_PROC_TIME Y           # Print timing for all science subprocesses to Logfile
                                   #   [ TRUE or Y ]
setenv STDOUT T                    # Override I/O-API trying to write information to both the processor 
                                   #   logs and STDOUT [ options: T | F ]

setenv GRID_NAME SE53BENCH         # check GRIDDESC file for GRID_NAME options
setenv GRIDDESC $OUTDIR/GRIDDESC   # grid description file

#> WRF-CMAQ number of columns, rows and layers 
setenv WRF_COL_DIM        113   # wrf west_east_stag
setenv WRF_ROW_DIM         93   # wrf south_north_stag
setenv WRF_LAY_DIM         36   # wrf bottom_top_stag

setenv CMAQ_COL_DIM       100
setenv CMAQ_ROW_DIM        80
setenv TWOWAY_DELTA_X       6   # distance between the wrf and cmaq lower left corner in the x-direction
setenv TWOWAY_DELTA_Y       6   # distance between the wrf and cmaq lower left corner in the y-direction

setenv WRF_LC_REF_LAT    40.0   # WRF Lambert conformal reference latitude

if (! -e $OUTDIR ) then
  mkdir -p $OUTDIR
endif

# convert STTIME to WRF format HH:MM::SS
@ second = $STTIME % 100
@ minute = ($STTIME / 100) % 100
@ hour   = $STTIME / 10000

set wrf_sttime = `date -ud "$hour":"$minute":"$second" +%H:%M:%S`

set wrf_hr = $NSTEPS

#> setup wrf start hour, minute, and second
@ wrf_sec = $NSTEPS % 100
@ wrf_min = ($NSTEPS / 100) % 100
@ wrf_hr  = $NSTEPS / 10000

@ wrf_restart_interval = $wrf_min + ($wrf_hr * 60)

# Output Species and Layer Options
   # CONC file species; comment or set to "ALL" to write all species to CONC
   setenv CONC_SPCS "O3 NO ANO3I ANO3J NO2 FORM ISOP NH3 ANH4I ANH4J ASO4I ASO4J" 
   setenv CONC_BLEV_ELEV " 1 1"  # CONC file layer range; comment to write all layers to CONC

   # ACONC file species; comment or set to "ALL" to write all species to ACONC
   # setenv AVG_CONC_SPCS "O3 NO CO NO2 ASO4I ASO4J NH3" 
   setenv AVG_CONC_SPCS "ALL" 
   setenv ACONC_BLEV_ELEV " 1 1" # ACONC file layer range; comment to write all layers to ACONC
   setenv AVG_FILE_ENDTIME N     # override default beginning ACONC timestamp [ default: N ]

# Synchronization Time Step and Tolerance Options
setenv CTM_MAXSYNC         300   # max sync time step (sec) [ default: 720 ]
setenv CTM_MINSYNC          60   # min sync time step (sec) [ default: 60 ]
setenv SIGMA_SYNC_TOP      0.7   # top sigma level thru which sync step determined [ default: 0.7 ]
#setenv ADV_HDIV_LIM      0.95   # maximum horiz. div. limit for adv step adjust [ default: 0.9 ]
setenv CTM_ADV_CFL        0.95   # max CFL [ default: 0.75]
#setenv RB_ATOL        1.0E-09   # global ROS3 solver absolute tolerance [ default: 1.0E-07 ]

# Science Options
setenv CTM_OCEAN_CHEM        Y   # Flag for ocean halgoen chemistry and sea spray aerosol emissions [ Y ]
setenv CTM_WB_DUST           N   # use inline windblown dust emissions [ Y ]
setenv CTM_WBDUST_BELD   BELD3   # landuse database for identifying dust source regions
                                 #    [ UNKNOWN ]; ignore if CTM_WB_DUST = N
setenv CTM_LTNG_NO           N   # turn on lightning NOx [ N ]
setenv KZMIN                 Y   # use Min Kz option in edyintb [ Y ],
                                 #    otherwise revert to Kz0UT
setenv CTM_MOSAIC            N   # landuse specific deposition velocities [ N ]
setenv CTM_FST               N   #> mosaic method to get land-use specific stomatal flux [ N ]
setenv PX_VERSION            Y   # WRF PX LSM
setenv CLM_VERSION           N   # WRF CLM LSM
setenv NOAH_VERSION          N   # WRF NOAH LSM
setenv CTM_ABFLUX            N   # ammonia bi-directional flux for in-line deposition velocities [ N ]
setenv CTM_BIDI_FERT_NH3     T   # subtract fertilizer NH3 from emissions because it will be handled
                                 #    by the BiDi calculation [ Y ]
setenv CTM_HGBIDI            N   # mercury bi-directional flux for in-line deposition velocities [ N ]
setenv CTM_SFC_HONO          Y   # surface HONO interaction [ Y ]
setenv CTM_GRAV_SETL         Y   # vdiff aerosol gravitational sedimentation [ Y ]
setenv CTM_BIOGEMIS          Y   # calculate in-line biogenic emissions [ N ]

setenv CTM_TURN_ON_PV        N   # WRF-CMAQ ONLY turn on/off PV [ N -- make sure compiled with pv on ]

#> Vertical Extraction Options
setenv VERTEXT               N
# setenv VERTEXT_COORD_PATH ${WORKDIR}/lonlat.csv

# I/O Controls
setenv IOAPI_LOG_WRITE         F  # turn on excess WRITE3 logging [ options: T | F ]
setenv FL_ERR_STOP             N  # stop on inconsistent input files
setenv PROMPTFLAG              F  # turn on I/O-API PROMPT*FILE interactive mode [ options: T | F ]
setenv IOAPI_OFFSET_64       YES  # support large timestep records (>2GB/timestep record) [ options: YES | NO ]
setenv IOAPI_CHECK_HEADERS     N  # check file headers [ options: Y | N ]
setenv CTM_EMISCHK             N  # Abort CMAQ if missing surrogates from emissions Input files
setenv EMISDIAG                F  # Print Emission Rates at the output time step after they have been
                                  #   scaled and modified by the user Rules [options: F | T or 2D | 3D | 2DSUM ]
                                  #   Individual streams can be modified using the variables:
                                  #       GR_EMIS_DIAG_## | STK_EMIS_DIAG_## | BIOG_EMIS_DIAG
                                  #       MG_EMIS_DIAG    | LTNG_EMIS_DIAG   | DUST_EMIS_DIAG
                                  #       SEASPRAY_EMIS_DIAG
                                  #   Note that these diagnostics are different than other emissions diagnostic
                                  #   output because they occur after scaling.

setenv EMIS_SYM_DATE           N  # Master switch for allowing CMAQ to use the date from each Emission file
                                  #   rather than checking the emissions date against the internal model date.
                                  #   [options: T | F or Y | N]. If false (F/N), then the date from CMAQ's internal
                                  #   time will be used and an error check will be performed (recommended). Users
                                  #   may switch the behavior for individual emission files below using the variables:
                                  #       GR_EM_SYM_DATE_## | STK_EM_SYM_DATE_## [ N ]

# Diagnostic Output Flags
setenv CTM_CKSUM               Y  # checksum report [ Y ]
setenv CLD_DIAG                N  # cloud diagnostic file [ N ]

setenv CTM_PHOTDIAG            N  # photolysis diagnostic file [ N ]
setenv NLAYS_PHOTDIAG        "1"  # Number of layers for PHOTDIAG2 and PHOTDIAG3 from
                                  #     Layer 1 to NLAYS_PHOTDIAG  [ default: all layers ]


#setenv NWAVE_PHOTDIAG "294 303 310 316 333 381 607"  # Wavelengths written for variables
                                                      #   in PHOTDIAG2 and PHOTDIAG3 
                                                      #   [ default: all wavelengths ]

setenv CTM_PMDIAG              N  # Instantaneous Aerosol Diagnostic File [ Y ]
setenv CTM_APMDIAG             Y  # Hourly-Average Aerosol Diagnostic File [ Y ]
setenv APMDIAG_BLEV_ELEV   "1 1"  # layer range for average pmdiag = NLAYS

setenv CTM_SSEMDIAG            N  # sea-spray emissions diagnostic file [ N ]
setenv CTM_DUSTEM_DIAG         N  # windblown dust emissions diagnostic file [ N ];
                                  #     Ignore if CTM_WB_DUST = N
setenv CTM_DEPV_FILE           N  # deposition velocities diagnostic file [ N ]
setenv VDIFF_DIAG_FILE         N  # vdiff & possibly aero grav. sedimentation diagnostic file [ N ]
setenv LTNGDIAG                N  # lightning diagnostic file [ N ]
setenv B3GTS_DIAG              N  # BEIS mass emissions diagnostic file [ N ]
setenv CTM_WVEL                Y  # save derived vertical velocity component to conc file [ Y ]

setenv SD_TIME_SERIES          F  # WRF-CMAQ sub domain time series output option [F]
#setenv SD_SCOL              241  # WRF-CMAQ sub domain time series starting column
#setenv SD_ECOL              248  # WRF-CMAQ sub domain time series ending column
#setenv SD_SROW              160  # WRF-CMAQ sub domain time series starting row
#setenv SD_EROW              169  # WRF-CMAQ sub domain time series ending row
#setenv SD_CONC_SPCS       "NO2 NO O3 NO3 CO ASO4J ASO4I ANH4J ANH4I ANO3J ANO3I AORGAJ AORGAI AORGPAJ AORGPAI AORGBJ AORGBI AECJ AECI A25J A25I ACORS ASEAS ASOIL" #> sub domain time series species subset list

setenv FILE_TIME_STEP  $met_file_tstep

# MPI Optimization Flags
setenv MPI_SM_POOL          16000  # increase shared memory pool in case many MPI_SEND headers
setenv MP_EAGER_LIMIT       65536  # set MPI message passing buffer size to max
setenv MP_SINGLE_THREAD       yes  # optimize for single threaded applications [ no ]
setenv MP_STDOUTMODE      ordered  # order output by the processor ID
setenv MP_LABELIO             yes  # label output by processor ID [ no ]
setenv MP_SHARED_MEMORY       yes  # force use of shared memory for tasks on same node [ no ]
setenv MP_ADAPTER_USE      shared  # share the MP adapter with other jobs
setenv MP_CPU_USE        multiple  # share the node with multiple users/jobs
setenv MP_CSS_INTERRUPT       yes  # specify whether arriving packets generate interrupts [ no ]

# =====================================================================
#> Input Directories and Filenames
# =====================================================================

set ICpath    = $INPDIR/icbc                        #> initial conditions input directory 
set BCpath    = $INPDIR/icbc                        #> boundary conditions input directory
set EMISpath  = $INPDIR/emis/gridded_area/gridded   #> gridded emissions input directory
set EMISpath2 = $INPDIR/emis/gridded_area/rwc       #> gridded surface residential wood combustion emissions directory
set IN_PTpath = $INPDIR/emis/inln_point             #> point source emissions input directory
set IN_LTpath = $INPDIR/lightning                   #> lightning NOx input directory
set METpath   = $INPDIR/WRF-CMAQ/wrfv4.1.1_inputs   #> meteorology input directory for ICs/BCs fdda/sfdda 
#set JVALpath  = $INPDIR/jproc                      #> offline photolysis rate table directory
# set OMIpath   = $BLD/cmaq                           #> ozone column data for the photolysis model
set LUpath    = $INPDIR/land                        #> BELD landuse data for windblown dust model
set SZpath    = $INPDIR/land                        #> surf zone file for in-line seaspray emissions

# =====================================================================
#> Begin Loop Through Simulation Days
# =====================================================================
set rtarray = ""

set TODAYG = ${START_DATE}
set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set START_DAY = ${TODAYJ} 
set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set NDAYS = 0

while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ
  
  set NDAYS = `echo "${NDAYS} + 1" | bc -l`

  #> Retrieve Calendar day Information
  set YYYYMMDD = `date -ud "${TODAYG}" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYMMDD
  set YYYYMM = `date -ud "${TODAYG}" +%Y%m`     #> Convert YYYY-MM-DD to YYYYMM
  set YYMMDD = `date -ud "${TODAYG}" +%y%m%d`   #> Convert YYYY-MM-DD to YYMMDD
  set YYYYJJJ = $TODAYJ

  #> Calculate Yesterday's Date
  set YESTERDAY = `date -ud "${TODAYG}-1days" +%Y%m%d`

# =====================================================================
#> Set Output String and Propagate Model Configuration Documentation
# =====================================================================
  echo ""
  echo "Set up input and output files for Day ${TODAYG}."

  #> set output file name extensions
  setenv CTM_APPL ${RUNID}_${YYYYMMDD} 
  setenv CTM_APPL_yesterday ${RUNID}_${YESTERDAY}
  
  #> Copy Model Configuration To Output Folder
  if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

# cp $BLD/cmaq/CCTM_${VRSN}.cfg $OUTDIR/CCTM_${CTM_APPL}.cfg

# =====================================================================
#> Input Files (Some are Day-Dependent)
# =====================================================================

  #> Initial conditions
  if ($NEW_START == true || $NEW_START == TRUE ) then
     setenv ICFILE ICON_20160630_bench.nc
     setenv INIT_MEDC_1 notused
     setenv INITIAL_RUN Y #related to restart soil information file

     #> WRF-CMAQ Configuration
     set feedback_restart = .false. # indicates no CMAQ aerosol information in the initial step
     if ($cont_from_spinup_run == T) then
        setenv WRF_RSTFLAG .false.   # indicates WRF restart file exist
     else
        setenv WRF_RSTFLAG .false.  # indicates WRF restart file does not exist
     endif
     set pxlsm_smois_init = 1
  else
     set ICpath = $OUTDIR
     setenv ICFILE CCTM_CGRID_${RUNID}_${YESTERDAY}.nc
     setenv INIT_MEDC_1 $ICpath/CCTM_MEDIA_CONC_${RUNID}_${YESTERDAY}.nc
     setenv INITIAL_RUN N

     #> WRF-CMAQ Configuration
     setenv WRF_RSTFLAG .true.      # indicates WRF restart file exist
     set feedback_restart = .true.  # indicates CMAQ aerosol information is available

     set pxlsm_smois_init = 0
  endif

  #> Boundary conditions
  set BCFILE = BCON_${YYYYMMDD}_bench.nc

  #> Off-line photolysis rates 
  #set JVALfile  = JTABLE_${YYYYJJJ}

  #> Ozone column data
  set OMIfile   = OMI_1979_to_2019.dat

  #> Optics file
  set OPTfile = PHOT_OPTICS.dat

  #> MCIP meteorology files 
  setenv GRID_BDY_2D BUFFERED  # GRID files are static, not day-specific
  setenv GRID_CRO_2D BUFFERED
  setenv GRID_CRO_3D BUFFERED
  setenv GRID_DOT_2D BUFFERED
  setenv MET_CRO_2D BUFFERED 
  setenv MET_CRO_3D BUFFERED
  setenv MET_DOT_3D BUFFERED
  setenv MET_BDY_3D BUFFERED
  #setenv LUFRAC_CRO BUFFERED

  #> Emissions Control File
  #>
  #> IMPORTANT NOTE
  #>
  #> The emissions control file defined below is an integral part of controlling the behavior of the model simulation.
  #> Among other things, it controls the mapping of species in the emission files to chemical species in the model and
  #> several aspects related to the simulation of organic aerosols.
  #> Please carefully review the emissions control file to ensure that it is configured to be consistent with the assumptions
  #> made when creating the emission files defined below and the desired representation of organic aerosols.
  #> For further information, please see:
  #> + AERO7 Release Notes section on 'Required emission updates':
  #>   https://github.com/USEPA/CMAQ/blob/main/DOCS/Release_Notes/CMAQv5.3_aero7_overview.md
  #> + CMAQ User's Guide section 6.9.3 on 'Emission Compatability': 
  #>   https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6.9.3_Emission_Compatability
  #> + Emission Control (DESID) Documentation in the CMAQ User's Guide: 
  #>   https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixB_emissions_control.md 
  #>
# setenv EMISSCTRL_NML ${BLD}/cmaq/EmissCtrl_${MECH}.nml

  #> Spatial Masks For Emissions Scaling
  setenv CMAQ_MASKS $SZpath/12US1_surf_bench.nc #> horizontal grid-dependent surf zone file

  #> Gridded Emissions Files 
  setenv N_EMIS_GR 2
  set EMISfile  = emis_mole_all_${YYYYMMDD}_cb6_bench.nc
  setenv GR_EMIS_001 ${EMISpath}/${EMISfile}
  setenv GR_EMIS_LAB_001 GRIDDED_EMIS
  setenv GR_EM_SYM_DATE_001 F

  set EMISfile  = emis_mole_rwc_${YYYYMMDD}_12US1_cmaq_cb6_2016ff_16j.nc
  setenv GR_EMIS_002 ${EMISpath2}/${EMISfile}
  setenv GR_EMIS_LAB_002 GRIDDED_RWC
  setenv GR_EM_SYM_DATE_002 F

  #> In-line point emissions configuration
  setenv N_EMIS_PT 8          #> Number of elevated source groups

  set STKCASEG = 12US1_2016ff_16j           # Stack Group Version Label
  set STKCASEE = 12US1_cmaq_cb6_2016ff_16j  # Stack Emission Version Label

  # Time-Independent Stack Parameters for Inline Point Sources
  setenv STK_GRPS_001 $IN_PTpath/stack_groups/stack_groups_ptnonipm_${STKCASEG}.nc
  setenv STK_GRPS_002 $IN_PTpath/stack_groups/stack_groups_ptegu_${STKCASEG}.nc
  setenv STK_GRPS_003 $IN_PTpath/stack_groups/stack_groups_othpt_${STKCASEG}.nc
  setenv STK_GRPS_004 $IN_PTpath/stack_groups/stack_groups_ptagfire_${YYYYMMDD}_${STKCASEG}.nc
  setenv STK_GRPS_005 $IN_PTpath/stack_groups/stack_groups_ptfire_${YYYYMMDD}_${STKCASEG}.nc
  setenv STK_GRPS_006 $IN_PTpath/stack_groups/stack_groups_ptfire_othna_${YYYYMMDD}_${STKCASEG}.nc
  setenv STK_GRPS_007 $IN_PTpath/stack_groups/stack_groups_pt_oilgas_${STKCASEG}.nc
  setenv STK_GRPS_008 $IN_PTpath/stack_groups/stack_groups_cmv_c3_${STKCASEG}.nc
  setenv LAYP_STTIME $STTIME
  setenv LAYP_NSTEPS $NSTEPS

  # Emission Rates for Inline Point Sources
  setenv STK_EMIS_001 $IN_PTpath/ptnonipm/inln_mole_ptnonipm_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_002 $IN_PTpath/ptegu/inln_mole_ptegu_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_003 $IN_PTpath/othpt/inln_mole_othpt_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_004 $IN_PTpath/ptagfire/inln_mole_ptagfire_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_005 $IN_PTpath/ptfire/inln_mole_ptfire_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_006 $IN_PTpath/ptfire_othna/inln_mole_ptfire_othna_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_007 $IN_PTpath/pt_oilgas/inln_mole_pt_oilgas_${YYYYMMDD}_${STKCASEE}.nc
  setenv STK_EMIS_008 $IN_PTpath/cmv_c3/inln_mole_cmv_c3_${YYYYMMDD}_${STKCASEE}.nc
  setenv LAYP_STDATE $YYYYJJJ

  # Label Each Emissions Stream
  setenv STK_EMIS_LAB_001 POINT_NONEGU
  setenv STK_EMIS_LAB_002 POINT_EGU
  setenv STK_EMIS_LAB_003 POINT_OTHER
  setenv STK_EMIS_LAB_004 POINT_AGFIRES
  setenv STK_EMIS_LAB_005 POINT_FIRES
  setenv STK_EMIS_LAB_006 POINT_OTHFIRES
  setenv STK_EMIS_LAB_007 POINT_OILGAS
  setenv STK_EMIS_LAB_008 POINT_CMV

  # Stack emissions diagnostic files
  #setenv STK_EMIS_DIAG_001 2DSUM
  #setenv STK_EMIS_DIAG_002 2DSUM
  #setenv STK_EMIS_DIAG_003 2DSUM
  #setenv STK_EMIS_DIAG_004 2DSUM
  #setenv STK_EMIS_DIAG_005 2DSUM

  # Allow CMAQ to Use Point Source files with dates that do not
  # match the internal model date
  setenv STK_EM_SYM_DATE_001 T
  setenv STK_EM_SYM_DATE_002 T
  setenv STK_EM_SYM_DATE_003 T
  setenv STK_EM_SYM_DATE_004 T
  setenv STK_EM_SYM_DATE_005 T
  setenv STK_EM_SYM_DATE_006 T
  setenv STK_EM_SYM_DATE_007 T
  setenv STK_EM_SYM_DATE_008 T

  #> Lightning NOx configuration
  if ( $CTM_LTNG_NO == 'Y' ) then
     setenv LTNGNO "InLine"    #> set LTNGNO to "Inline" to activate in-line calculation

  #> In-line lightning NOx options
     setenv USE_NLDN  Y        #> use hourly NLDN strike file [ default: Y ]
     if ( $USE_NLDN == Y ) then
        setenv NLDN_STRIKES ${IN_LTpath}/NLDN.12US1.${YYYYMMDD}_bench.nc
     endif
     setenv LTNGPARMS_FILE ${IN_LTpath}/LTNG_AllParms_12US1_bench.nc #> lightning parameter file
  endif

  #> In-line biogenic emissions configuration
  if ( $CTM_BIOGEMIS == 'Y' ) then   
     set IN_BEISpath = ${INPDIR}/land
     setenv GSPRO      $WRF_DIR/cmaq/gspro_biogenics.txt
     setenv B3GRD      $IN_BEISpath/b3grd_bench.nc
     setenv BIOSW_YN   Y     #> use frost date switch [ default: Y ]
     setenv BIOSEASON  $IN_BEISpath/bioseason.cmaq.2016_12US1_full_bench.ncf 
                             #> ignore season switch file if BIOSW_YN = N
     setenv SUMMER_YN  Y     #> Use summer normalized emissions? [ default: Y ]
     setenv PX_VERSION Y     #> MCIP is PX version? [ default: N ]
     setenv SOILINP    $OUTDIR/CCTM_SOILOUT_${CTM_APPL_yesterday}.nc
                             #> Biogenic NO soil input file; ignore if INITIAL_RUN = Y
  endif

  #> Windblown dust emissions configuration
  if ( $CTM_WB_DUST == 'Y' ) then
     # Input variables for BELD3 Landuse option
     setenv DUST_LU_1 $LUpath/beld3_12US1_459X299_output_a_bench.nc
     setenv DUST_LU_2 $LUpath/beld4_12US1_459X299_output_tot_bench.nc
  endif

  #> In-line sea spray emissions configuration
  setenv OCEAN_1 $SZpath/12US1_surf_bench.nc #> horizontal grid-dependent surf zone file

  #> Bidirectional ammonia configuration
  if ( $CTM_ABFLUX == 'Y' ) then
     setenv E2C_SOIL ${LUpath}/epic_festc1.4_20180516/2016_US1_soil_bench.nc
     setenv E2C_CHEM ${LUpath}/epic_festc1.4_20180516/2016_US1_time${YYYYMMDD}_bench.nc
     setenv E2C_CHEM_YEST ${LUpath}/epic_festc1.4_20180516/2016_US1_time${YESTERDAY}_bench.nc
     setenv E2C_LU ${LUpath}/beld4_12kmCONUS_2011nlcd_bench.nc
  endif

#> Inline Process Analysis 
  setenv CTM_PROCAN N        #> use process analysis [ default: N]
  if ( $?CTM_PROCAN ) then   # $CTM_PROCAN is defined
     if ( $CTM_PROCAN == 'Y' || $CTM_PROCAN == 'T' ) then
#> process analysis global column, row and layer ranges
#       setenv PA_BCOL_ECOL "10 90"  # default: all columns
#       setenv PA_BROW_EROW "10 80"  # default: all rows
#       setenv PA_BLEV_ELEV "1  4"   # default: all levels
        setenv PACM_INFILE ${NMLpath}/pa_${MECH}.ctl
        setenv PACM_REPORT $OUTDIR/"PA_REPORT".${YYYYMMDD}
     endif
  endif

#> Integrated Source Apportionment Method (ISAM) Options
 setenv CTM_ISAM N
 if ( $?CTM_ISAM ) then
    if ( $CTM_ISAM == 'Y' || $CTM_ISAM == 'T' ) then
       setenv SA_IOLIST ${WORKDIR}/isam_control.txt
       setenv ISAM_BLEV_ELEV " 1 1"
       setenv AISAM_BLEV_ELEV " 1 1"

       #> Set Up ISAM Initial Condition Flags
       if ($NEW_START == true || $NEW_START == TRUE ) then
          setenv ISAM_NEW_START Y
          setenv ISAM_PREVDAY
       else
          setenv ISAM_NEW_START N
          setenv ISAM_PREVDAY "$OUTDIR/CCTM_SA_CGRID_${RUNID}_${YESTERDAY}.nc"
       endif

       #> Set Up ISAM Output Filenames
       setenv SA_ACONC_1      "$OUTDIR/CCTM_SA_ACONC_${CTM_APPL}.nc -v"
       setenv SA_CONC_1       "$OUTDIR/CCTM_SA_CONC_${CTM_APPL}.nc -v"
       setenv SA_DD_1         "$OUTDIR/CCTM_SA_DRYDEP_${CTM_APPL}.nc -v"
       setenv SA_WD_1         "$OUTDIR/CCTM_SA_WETDEP_${CTM_APPL}.nc -v"
       setenv SA_CGRID_1      "$OUTDIR/CCTM_SA_CGRID_${CTM_APPL}.nc -v"

       #> Set optional ISAM regions files
#      setenv ISAM_REGIONS /work/MOD3EVAL/nsu/isam_v53/CCTM/scripts/input/RGN_ISAM.nc

    endif
 endif


#> Sulfur Tracking Model (STM)
 setenv STM_SO4TRACK N        #> sulfur tracking [ default: N ]
 if ( $?STM_SO4TRACK ) then
    if ( $STM_SO4TRACK == 'Y' || $STM_SO4TRACK == 'T' ) then

      #> option to normalize sulfate tracers [ default: Y ]
      setenv STM_ADJSO4 Y

    endif
 endif
 
# =====================================================================
#> Output Files
# =====================================================================

  #> set output file names
  setenv S_CGRID         "$OUTDIR/CCTM_CGRID_${CTM_APPL}.nc"         #> 3D Inst. Concentrations
  setenv CTM_CONC_1      "$OUTDIR/CCTM_CONC_${CTM_APPL}.nc -v"       #> On-Hour Concentrations
  setenv A_CONC_1        "$OUTDIR/CCTM_ACONC_${CTM_APPL}.nc -v"      #> Hourly Avg. Concentrations
  setenv MEDIA_CONC      "$OUTDIR/CCTM_MEDIA_CONC_${CTM_APPL}.nc -v" #> NH3 Conc. in Media
  setenv CTM_DRY_DEP_1   "$OUTDIR/CCTM_DRYDEP_${CTM_APPL}.nc -v"     #> Hourly Dry Deposition
  setenv CTM_DEPV_DIAG   "$OUTDIR/CCTM_DEPV_${CTM_APPL}.nc -v"       #> Dry Deposition Velocities
  setenv B3GTS_S         "$OUTDIR/CCTM_B3GTS_S_${CTM_APPL}.nc -v"    #> Biogenic Emissions
  setenv SOILOUT         "$OUTDIR/CCTM_SOILOUT_${CTM_APPL}.nc"       #> Soil Emissions
  setenv CTM_WET_DEP_1   "$OUTDIR/CCTM_WETDEP1_${CTM_APPL}.nc -v"    #> Wet Dep From All Clouds
  setenv CTM_WET_DEP_2   "$OUTDIR/CCTM_WETDEP2_${CTM_APPL}.nc -v"    #> Wet Dep From SubGrid Clouds
  setenv CTM_PMDIAG_1    "$OUTDIR/CCTM_PMDIAG_${CTM_APPL}.nc -v"     #> On-Hour Particle Diagnostics
  setenv CTM_APMDIAG_1   "$OUTDIR/CCTM_APMDIAG_${CTM_APPL}.nc -v"    #> Hourly Avg. Particle Diagnostics
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
  setenv CTM_VEXT_1      "$OUTDIR/CCTM_VEXT_${CTM_APPL}.nc -v"       #> On-Hour 3D Concs at select sites
  
# this is for creating physical files
  setenv PGRID_DOT_2D "$OUTDIR/GRID_DOT_2D_${CTM_APPL}.nc -v"
  setenv PGRID_CRO_2D "$OUTDIR/GRID_CRO_2D_${CTM_APPL}.nc -v"
  setenv PMET_CRO_2D  "$OUTDIR/MET_CRO_2D_${CTM_APPL}.nc -v"
  setenv PMET_DOT_3D  "$OUTDIR/MET_DOT_2D_${CTM_APPL}.nc -v"
  setenv PMET_CRO_3D  "$OUTDIR/MET_CRO_3D_${CTM_APPL}.nc -v"
# WRF-CMAQ Files
  if ($SD_TIME_SERIES == T) then
     setenv CTM_SD_TS "$OUTDIR/SD_TSfile_${CTM_APPL}.nc -v"
  endif
  setenv     LAYER_FILE      MET_CRO_3D
  @ n = 0
  while ($n < $NPROCS)
    set name = `printf "_%3.3d\n" $n`
    setenv feed_back$name BUFFERED   # for feedback file
    @ n++
  end

  #> set floor file (neg concs)
  setenv FLOOR_FILE ${OUTDIR}/FLOOR_${CTM_APPL}.txt

  #> look for existing log files and output files
  ( ls CTM_LOG_???.${CTM_APPL} > buff.txt ) >& /dev/null
  ( ls ${OUTDIR}/CTM_LOG_???.${CTM_APPL} >> buff.txt ) >& /dev/null
  set log_test = `cat buff.txt`; rm -f buff.txt

  set OUT_FILES = (${FLOOR_FILE} ${S_CGRID} ${CTM_CONC_1} ${A_CONC_1} ${MEDIA_CONC}         \
             ${CTM_DRY_DEP_1} $CTM_DEPV_DIAG $B3GTS_S $SOILOUT $CTM_WET_DEP_1\
             $CTM_WET_DEP_2 $CTM_PMDIAG_1 $CTM_APMDIAG_1             \
             $CTM_RJ_1 $CTM_RJ_2 $CTM_RJ_3 $CTM_SSEMIS_1 $CTM_DUST_EMIS_1 $CTM_IPR_1 $CTM_IPR_2       \
             $CTM_IPR_3 $CTM_IRR_1 $CTM_IRR_2 $CTM_IRR_3 $CTM_DRY_DEP_MOS                   \
             $CTM_DRY_DEP_FST $CTM_DEPV_MOS $CTM_DEPV_FST $CTM_VDIFF_DIAG $CTM_VSED_DIAG    \
             $CTM_LTNGDIAG_1 $CTM_LTNGDIAG_2 $CTM_VEXT_1 )
  if ( $?CTM_ISAM ) then
     if ( $CTM_ISAM == 'Y' || $CTM_ISAM == 'T' ) then
        set OUT_FILES = (${OUT_FILES} ${SA_ACONC_1} ${SA_CONC_1} ${SA_DD_1} ${SA_WD_1}      \
                         ${SA_CGRID_1} )
     endif
  endif
  set OUT_FILES = `echo $OUT_FILES | sed "s; -v;;g" | sed "s;MPI:;;g" `
  ( ls $OUT_FILES > buff.txt ) >& /dev/null
  set out_test = `cat buff.txt`; rm -f buff.txt
  
  #> delete previous output if requested
  if ( $CLOBBER_DATA == true || $CLOBBER_DATA == TRUE  ) then
     echo 
     echo "Existing Logs and Output Files for Day ${TODAYG} Will Be Deleted"

     #> remove previous log files
     foreach file ( ${log_test} )
        #echo "Deleting log file: $file"
        /bin/rm -f $file  
     end
 
     #> remove previous output files
     foreach file ( ${out_test} )
        #echo "Deleting output file: $file"
        /bin/rm -f $file  
     end
     /bin/rm -f ${OUTDIR}/CCTM_EMDIAG*${RUNID}_${YYYYMMDD}.nc

  else
     #> error if previous log files exist
     if ( "$log_test" != "" ) then
       echo "*** Logs exist - run ABORTED ***"
       echo "*** To overide, set CLOBBER_DATA = TRUE in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif
     
     #> error if previous output files exist
     if ( "$out_test" != "" ) then
       echo "*** Output Files Exist - run will be ABORTED ***"
       foreach file ( $out_test )
          echo " cannot delete $file"
       end
       echo "*** To overide, set CLOBBER_DATA = TRUE in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif
  endif

  #> for the run control ...
  setenv CTM_STDATE      $YYYYJJJ
  setenv CTM_STTIME      $STTIME
  setenv CTM_RUNLEN      $NSTEPS
  setenv CTM_TSTEP       $TSTEP
  setenv INIT_CONC_1 $ICpath/$ICFILE
  setenv BNDY_CONC_1 $BCpath/$BCFILE
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

# if (! (-e $CSQY_DATA ) ) then
#    echo " $CSQY_DATA  not found "
#    exit 1
# endif
# if (! (-e $OPTICS_DATA ) ) then
#    echo " $OPTICS_DATA  not found "
#    exit 1
# endif
      
      cd $OUTDIR

# ===================================================================
#> Building WRF Namelist. 
# ===================================================================

#auxinput1_inname                    = "met_em.d01.<date>",
#iofields_filename                   = "output.var.txt",

      if ( -f namelist.input ) rm -f namelist.input

      cat << End_Of_Namelist  > namelist.input

 &time_control
 run_hours                           = $wrf_hr,			  
 run_minutes                         = $wrf_min,
 run_seconds                         = $wrf_sec,
 start_year                          = `date -ud "${TODAYG}" +%Y`, 
 start_month                         = `date -ud "${TODAYG}" +%m`,
 start_day                           = `date -ud "${TODAYG}" +%d`,
 start_hour                          = `date -ud "$wrf_sttime" +%H`,
 start_minute                        = `date -ud "$wrf_sttime" +%M`,  
 start_second                        = `date -ud "$wrf_sttime" +%S`,  
 interval_seconds                    = 10800,
 input_from_file                     = .true.,
 HISTORY_INTERVAL                    = 60,
 FRAMES_PER_OUTFILE                  = 25,
 restart                             = $WRF_RSTFLAG,
 restart_interval                    = $wrf_restart_interval,
 write_hist_at_0h_rst                = .true.,
 io_form_history                     = 2,
 io_form_restart                     = 2,
 io_form_input                       = 2,
 io_form_boundary                    = 2,
 io_form_auxinput2                   = 2,
 io_form_auxinput4                   = 2,
 debug_level                         = 0,
 auxinput4_inname                    = "wrflowinp_d01",
 auxinput4_interval                  = 360,
 auxinput4_end_h                     = 1000000,
 reset_simulation_start              = .false.,
/

 &wrf_cmaq
 wrf_cmaq_option                     = $wrf_cmaq_option,
 wrf_cmaq_freq                       = $wrf_cmaq_freq,
 met_file_tstep                      = $met_file_tstep,
 direct_sw_feedback                  = $direct_sw_feedback,
 feedback_restart                    = $feedback_restart,
/

 &domains
 time_step                           = $wrf_tstep,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,   
 e_we                                = $WRF_COL_DIM,   
 s_sn                                = 1,  
 e_sn                                = $WRF_ROW_DIM, 
 s_vert                              = 1, 
 e_vert                              = $WRF_LAY_DIM,
 p_top_requested                     = 5000,
 eta_levels                          = 1.000, 0.9975, 0.995, 0.990, 0.985,
                                       0.980, 0.970, 0.960, 0.950,
                                       0.940, 0.930, 0.920, 0.910,
                                       0.900, 0.880, 0.860, 0.840,
                                       0.820, 0.800, 0.770, 0.740,
                                       0.700, 0.650, 0.600, 0.550,
                                       0.500, 0.450, 0.400, 0.350,
                                       0.300, 0.250, 0.200, 0.150,
                                       0.100, 0.050, 0.000
 num_metgrid_levels                  = 40,
 dx                                  = $resolution, 
 dy                                  = $resolution,
 grid_id                             = 1, 
 parent_id                           = 0,  
 i_parent_start                      = 0,   
 j_parent_start                      = 0,     
 parent_grid_ratio                   = 1,   
 parent_time_step_ratio              = 1,    
 feedback                            = 1,
 smooth_option                       = 0,
 /

 &physics
 mp_physics                          = 10, 
 mp_zero_out                         = 2,
 mp_zero_out_thresh                  = 1.0e-8,
 ra_lw_physics                       = 4, 
 ra_sw_physics                       = 4, 
 radt                                = $radt, 
 co2tf                               = 1,  
 sf_sfclay_physics                   = 7, 
 num_soil_layers                     = 2,
 pxlsm_smois_init                    = $pxlsm_smois_init,
 pxlsm_modis_veg                     = 1,
 sf_surface_physics                  = 7, 
 sf_urban_physics                    = 0,
 bl_pbl_physics                      = 7, 
 bldt                                = 0, 
 cu_physics                          = 1, 
 kfeta_trigger                       = 2
 cudt                                = 0, 
 ishallow                            = 0,
 shcu_physics                        = 0,
 prec_acc_dt                         = 60,
 isfflx                              = 1,
 ifsnow                              = 1,
 icloud                              = 1,
 cu_rad_feedback                     = .true.,
 surface_input_source                = 1,
 num_land_cat                        = $NUM_LAND_USE_TYPE,
 num_soil_cat                        = 16,
 sst_update                          = 1,
 seaice_threshold                    = 100,
 slope_rad                           = 1,
 topo_shading                        = 1,
 shadlen                             = 25000.,
 do_radar_ref                        = 1,
 grav_settling                       = 0,
 /

 &fdda
 grid_fdda                           = 1,    
 grid_sfdda                          = 1,
 gfdda_inname                        = "wrffdda_d01",
 sgfdda_inname                       = "wrfsfdda_d01",
 pxlsm_soil_nudge                    = 1,
 sgfdda_end_h                        = 1000000,
 sgfdda_interval_m                   = 180,
 GFDDA_END_H                         = 1000000,
 gfdda_interval_m                    = 180,  
 fgdt                                = 0,    
 if_no_pbl_nudging_uv                = 1,    
 if_no_pbl_nudging_t                 = 1,     
 if_no_pbl_nudging_q                 = 1,     
 if_zfac_uv                          = 0,     
  k_zfac_uv                          = 13,   
 if_zfac_t                           = 0,    
  k_zfac_t                           = 13,   
 if_zfac_q                           = 0,     
  k_zfac_q                           = 13,   
 guv                                 = 0.0001,     
 gt                                  = 0.0001,    
 gq                                  = 0.00001, 
 guv_sfc                             = 0.0000,
 gt_sfc                              = 0.0000,
 gq_sfc                              = 0.0000,
 if_ramping                          = 1,
 dtramp_min                          = 60.0,
 io_form_gfdda                       = 2,
 rinblw                              = 250.0
 /

 &dynamics
 hybrid_opt                          = 2,
 w_damping                           = 1,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 2,
 diff_6th_factor                     = 0.12,
 damp_opt                            = 3,
 base_temp                           = 290.
 zdamp                               = 5000., 
 dampcoef                            = 0.05,  
 khdif                               = 0,     
 kvdif                               = 0,    
 non_hydrostatic                     = .true., 
 moist_adv_opt                       = 2,
 tke_adv_opt                         = 2,
 scalar_adv_opt                      = 2,
 use_theta_m                         = 1,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., 
 spec_exp                            = 0.0,
 nested                              = .false.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
 
End_Of_Namelist

      rm -f wrfbdy_d01 wrffdda_d01 wrfsfdda_d01 wrfinput_d01 wrflowinp_d01
      ln -sf $METpath/wrfbdy_d01 wrfbdy_d01
      ln -sf $METpath/wrffdda_d01 wrffdda_d01
      ln -sf $METpath/wrfsfdda_d01 wrfsfdda_d01
      if (${WRF_RSTFLAG} == .false.) then
         ln -sf $METpath/wrfinput_d01 wrfinput_d01
      endif
      ln -sf $METpath/wrflowinp_d01 wrflowinp_d01

#-----------------------------------------------------------------------
# Set up and run WRF-EM executable.
#-----------------------------------------------------------------------

      if ( -f wrf.exe       ) rm -f wrf.exe

      if ( -f ETAMPNEW_DATA ) rm -f ETAMPNEW_DATA
      if ( -f GENPARM.TBL   ) rm -f GENPARM.TBL
      if ( -f landFilenames ) rm -f landFilenames
      if ( -f LANDUSE.TBL   ) rm -f LANDUSE.TBL
      if ( -f RRTM_DATA     ) rm -f RRTM_DATA
      if ( -f SOILPARM.TBL  ) rm -f SOILPARM.TBL
      if ( -f tr49t67       ) rm -f tr49t67
      if ( -f tr49t85       ) rm -f tr49t85
      if ( -f tr67t85       ) rm -f tr67t85
      if ( -f VEGPARM.TBL   ) rm -f VEGPARM.TBL

      ln -s $WRF_DIR/main/wrf.exe              wrf.exe

      ln -s $WRF_DIR/test/em_real/ETAMPNEW_DATA ETAMPNEW_DATA
      ln -s $WRF_DIR/test/em_real/GENPARM.TBL   GENPARM.TBL
      ln -s $WRF_DIR/test/em_real/landFilenames landFilenames
      ln -s $WRF_DIR/test/em_real/LANDUSE.TBL   LANDUSE.TBL
      ln -s $WRF_DIR/test/em_real/RRTM_DATA     RRTM_DATA
      ln -s $WRF_DIR/test/em_real/RRTMG_SW_DATA RRTMG_SW_DATA
      ln -s $WRF_DIR/test/em_real/RRTMG_LW_DATA RRTMG_LW_DATA
      ln -s $WRF_DIR/test/em_real/SOILPARM.TBL  SOILPARM.TBL
      ln -s $WRF_DIR/test/em_real/tr49t67       tr49t67
      ln -s $WRF_DIR/test/em_real/tr49t85       tr49t85
      ln -s $WRF_DIR/test/em_real/tr67t85       tr67t85
      ln -s $WRF_DIR/test/em_real/VEGPARM.TBL   VEGPARM.TBL
      ln -s $WRF_DIR/test/em_real/ozone_plev.formatted  ozone_plev.formatted
      ln -s $WRF_DIR/test/em_real/ozone_lat.formatted   ozone_lat.formatted
      ln -s $WRF_DIR/test/em_real/ozone.formatted       ozone.formatted

# ===================================================================
#> Execution Portion
# ===================================================================

  #> Print attributes of the executable
# if ( $CTM_DIAG_LVL != 0 ) then
#    ls -l $BLD/$EXEC
#    size $BLD/$EXEC
#    unlimit
#    limit
# endif

  #> Print Startup Dialogue Information to Standard Out
  echo 
  echo "CMAQ Processing of Day $YYYYMMDD Began at `date`"
  echo 

  #> Executable call for single PE, uncomment to invoke
  #( /usr/bin/time -p $BLD/$EXEC ) |& tee buff_${EXECUTION_ID}.txt

  #> Executable call for multi PE, configure for your system 
  # set MPI = /usr/local/intel/impi/3.2.2.006/bin64
  # set MPIRUN = $MPI/mpirun
# ( /usr/bin/time -p mpirun -np $NPROCS $OUTDIR/$EXEC ) |& tee buff_${EXECUTION_ID}.txt
# /usr/bin/time -p mpirun -np $NPROCS $OUTDIR/$EXEC

  ls -al ${OUTDIR}/wrf.exe

  date
  time mpirun -np $NPROCS ${OUTDIR}/wrf.exe
  date
  
  #> Harvest Timing Output so that it may be reported below
# set rtarray = "${rtarray} `tail -3 buff_${EXECUTION_ID}.txt | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' | head -1` "
# rm -rf buff_${EXECUTION_ID}.txt

  #> Abort script if abnormal termination
  if ($wrf_cmaq_option > 1) then
     if ( ! -e $OUTDIR/CCTM_CGRID_${CTM_APPL}.nc ) then
       echo ""
       echo "**************************************************************"
       echo "** Runscript Detected an Error: CGRID file was not written. **"
       echo "**   This indicates that CMAQ was interrupted or an issue   **"
       echo "**   exists with writing output. The runscript will now     **"
       echo "**   abort rather than proceeding to subsequent days.       **"
       echo "**************************************************************"
       break
     endif

     #> Print Concluding Text
     echo
     echo "CMAQ Processing of Day $YYYYMMDD Finished at `date`"
     echo
     echo "\\\\\=====\\\\\=====\\\\\=====\\\\\=====/////=====/////=====/////=====/////"
     echo
  endif

# ===================================================================
#> Finalize Run for This Day and Loop to Next Day
# ===================================================================

  #> Save Log Files and Move on to Next Simulation Day
  #mv CTM_LOG_???.${CTM_APPL} $OUTDIR
  #> WRF-CMAQ LOGS are combined into WRF_LOGS no CTM_LOGS_* will be generated 
  if ( ! -e $OUTDIR/${TODAYJ}) then
    mkdir $OUTDIR/${TODAYJ} 
  endif
  mv rsl.* $OUTDIR/${TODAYJ}
  if (($wrf_cmaq_option == 1) || ($wrf_cmaq_option == 3)) then
    mv MET* $OUTDIR/${TODAYJ}
    mv GRI* $OUTDIR/${TODAYJ}
  endif
  
  if ( $CTM_DIAG_LVL != 0 ) then
    mv CTM_DIAG_???.${CTM_APPL} $OUTDIR
  endif

  #> The next simulation day will, by definition, be a restart
  setenv NEW_START false

  #> Increment both Gregorian and Julian Days
  set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
  set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

end  #Loop to the next Simulation Day

# ===================================================================
#> Generate Timing Report
# ===================================================================
set RTMTOT = 0
foreach it ( `seq ${NDAYS}` )
    set rt = `echo ${rtarray} | cut -d' ' -f${it}`
    set RTMTOT = `echo "${RTMTOT} + ${rt}" | bc -l`
end

set RTMAVG = `echo "scale=2; ${RTMTOT} / ${NDAYS}" | bc -l`
set RTMTOT = `echo "scale=2; ${RTMTOT} / 1" | bc -l`

echo
echo "=================================="
echo "  ***** CMAQ TIMING REPORT *****"
echo "=================================="
echo "Start Day: ${START_DATE}"
echo "End Day:   ${END_DATE}"
echo "Number of Simulation Days: ${NDAYS}"
echo "Domain Name:               ${GRID_NAME}"
#echo "Number of Grid Cells:      ${NCELLS}  (ROW x COL x LAY)"
#echo "Number of Layers:          ${NZ}"
echo "Number of Processes:       ${NPROCS}"
echo "   All times are in seconds."
echo
echo "Num  Day        Wall Time"
set d = 0
set day = ${START_DATE}
foreach it ( `seq ${NDAYS}` )
    # Set the right day and format it
    set d = `echo "${d} + 1"  | bc -l`
    set n = `printf "%02d" ${d}`

    # Choose the correct time variables
    set rt = `echo ${rtarray} | cut -d' ' -f${it}`

    # Write out row of timing data
    echo "${n}   ${day}   ${rt}"

    # Increment day for next loop
    set day = `date -ud "${day}+1days" +%Y-%m-%d`
end
echo "     Total Time = ${RTMTOT}"
echo "      Avg. Time = ${RTMAVG}"

exit
