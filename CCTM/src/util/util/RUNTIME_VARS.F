
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

!....................................................................
!  The RUNTIME_VARS module contains file unit identifiers for the log files,
!  input files, and the values of all environment variables.
!
!  It also contains routines for reading environment variables, and
!  opening input files.
!....................................................................

      MODULE RUNTIME_VARS

      IMPLICIT NONE

      SAVE

      PUBLIC

      INTEGER :: OUTDEV    ! File Unit for Standard Output
      INTEGER :: LOGDEV    ! File Unit for Ascii Log File
      INTEGER :: MYPE = -1 ! Processor Number


      !-----------------------------------------------------------------------------------
      !>> Parameters for formatting output log files
      !-----------------------------------------------------------------------------------

      INTEGER, PARAMETER :: CTM_DIAG_LVL = 0
      
      INTEGER :: LOG_LINE_LENGTH = 80  ! Cut the log offs at this character if possible
      INTEGER :: LOG_MAJOR_TAB   = 5   ! Left tab for all text including headings
      INTEGER :: LOG_MINOR_TAB   = 2   ! Tab for indenting subsequent lines of text in
                                       ! a paragraph for instance.

      CHARACTER( 10 ) :: WEEKDAY( 7 ) = (/'Monday   ','Tuesday  ','Wednesday',
     &                        'Thursday ','Friday   ','Saturday ','Sunday   ' /)

      LOGICAL :: GET_ENVYN

#ifdef twoway
      !-----------------------------------------------------------------------------------
      !>> Variables for Configuring WRF-CMAQ Coupled Model Runs
      !-----------------------------------------------------------------------------------

      LOGICAL, SAVE :: SD_TIME_SERIES       = .FALSE.
      LOGICAL, SAVE :: CMAQ_WRF_FEEDBACK    = .FALSE.
      LOGICAL, SAVE :: TURN_ON_PV           = .FALSE. 
      LOGICAL, SAVE :: CREATE_PHYSICAL_FILE = .FALSE. 
      LOGICAL, SAVE :: RUN_CMAQ_DRIVER      = .FALSE. 
      LOGICAL, SAVE :: INDIRECT_EFFECT      = .FALSE.
      LOGICAL, SAVE :: CMAQ_WRF_FEEDBACK
      INTEGER, SAVE :: SD_SCOL
      INTEGER, SAVE :: SD_ECOL
      INTEGER, SAVE :: SD_SROW
      INTEGER, SAVE :: SD_EROW
      INTEGER, SAVE :: N_SD_SPCS
      CHARACTER( 500 ), SAVE :: GRIDDESC_FNAME
      CHARACTER(  16 ), SAVE :: GRID_NAME_STR
#endif
      !-----------------------------------------------------------------------------------
      !>> Define Environment Variables for Controlling CMAQ Processes
      !-----------------------------------------------------------------------------------

      ! Grid and High-Level Model Parameters
      LOGICAL, SAVE :: NEW_START   = .TRUE. ! Start New Simulation. Not a Restart
      CHARACTER(16), SAVE :: GRID_NAME = '' ! grid name selected from GRIDDESC
      INTEGER, SAVE :: RUNLEN      = 480000 ! Run Length
      INTEGER, SAVE :: STDATE      =1995192 ! Start Date
      INTEGER, SAVE :: STTIME      = 000000 ! Start Time
      INTEGER, SAVE :: LOCAL_TSTEP = 010000 ! set to TSTEP( 1 ) in initscen.F
      INTEGER, SAVE :: MAXSYNC     = 720    ! force max TSTEP(2) (sec)
      INTEGER, SAVE :: MINSYNC     = 60     ! force min TSTEP(2) (sec)
      INTEGER, SAVE :: NPCOL       = 1      ! no. of processors across grid columns
      INTEGER, SAVE :: NPROW       = 1      ! no. of processors across grid rows

      INTEGER, PARAMETER :: MAXLEN_CCTM_APPL     = 200    ! Length of Logfile Names
      CHARACTER( MAXLEN_CCTM_APPL ) :: APPL_NAME = 'APPL' ! Logfile Names
      CHARACTER( MAXLEN_CCTM_APPL ) :: LOGFOLD   = ''     ! Log Folder

      ! General; Multiprocess control, output and error checking
      LOGICAL, SAVE :: LERROFF   = .TRUE.  ! Flag to stop run if errors are found.
      LOGICAL, SAVE :: CKSUM     = .TRUE.  ! flag for cksum on, default = [T]
      LOGICAL, SAVE :: END_TIME  = .FALSE. ! Override default beginning ACON timestamp
      
      INTEGER, SAVE :: N_ASPCS   = 0       ! Number of species saved to avg conc file
      INTEGER, SAVE :: N_CSPCS   = 0       ! Number of species saved to conc file
      INTEGER, SAVE :: ACONC_BLEV= 0       ! Beginning level saved to avg conc file
      INTEGER, SAVE :: ACONC_ELEV= 0       ! Ending level saved to avg conc file
      INTEGER, SAVE :: CONC_BLEV = 0       ! Beginning level saved to conc file
      INTEGER, SAVE :: CONC_ELEV = 0       ! Ending level saved to conc file
      CHARACTER(  16 ), SAVE :: ACONC_FILE_SPCS( 900 ) ! avg conc file species list
      CHARACTER(  16 ), SAVE :: CONC_FILE_SPCS(900 ) ! conc file species list
      LOGICAL, SAVE :: PWRTFLAG  = .TRUE.  ! Print confirmation of successful output 
                                           !    to logfile
      
      CHARACTER(256), SAVE :: GC_NAMELIST = '' ! Gas Species Namelist
      CHARACTER(256), SAVE :: AE_NAMELIST = '' ! Aerosol Species Namelist
      CHARACTER(256), SAVE :: NR_NAMELIST = '' ! Nonreactive Species Namelist
      CHARACTER(256), SAVE :: TR_NAMELIST = '' ! Tracer Species Namelist
      
      ! Chemistry and Photolysis
      LOGICAL, SAVE :: PHOTDIAG  = .FALSE. ! Flag for PHOTDIAG file
      LOGICAL, SAVE :: CORE_SHELL= .FALSE. ! flag for using core-shell mixing model for aerosol optics
      LOGICAL, SAVE :: MIE_CALC  = .FALSE. ! flag for using Mie Theory in aerosol optics calculation
      REAL,    SAVE :: GEAR_ATOL = 1.0E-9  ! Absolute Tolerance for Gear Solver
      REAL,    SAVE :: GEAR_RTOL = 1.0E-3  ! Relative Tolerance for Gear Solver
      REAL,    SAVE :: GLBL_ATOL = 1.0E-7  ! Absolute Tolerance for Rosenbrock Solver
      REAL,    SAVE :: GLBL_RTOL = 1.0E-3  ! Relative Tolerance for Rosenbrock Solver

      ! Aerosols
      LOGICAL, SAVE :: PMDIAG    = .FALSE. ! flag for Instantaneous PM Diagnostic file
      LOGICAL, SAVE :: APMDIAG   = .FALSE. ! flag for Average PM Diagnostic file
      LOGICAL, SAVE :: AVISDIAG  = .FALSE. ! flag for PMDIAG and APMDIAG file [F], default
      LOGICAL, SAVE :: ZERO_PCSOA= .FALSE. ! flag for Zeroing out pcSOA formation
      LOGICAL, SAVE :: AOD       = .FALSE. ! flag for printing AOD file
      CHARACTER(  16 ), SAVE :: APMDIAG_SPC( 200 )

      ! Cloud Parameters
      LOGICAL, SAVE :: CLD_DIAG  = .FALSE. ! flag to output cloud diagnostic files

      ! Air-Surface Exchange
      LOGICAL, SAVE :: ABFLUX    = .FALSE. ! flag for ammonia bi-directional flux with in-lining depv
      LOGICAL, SAVE :: MOSAIC    = .FALSE. ! flag for mosaic - luc specific deposition - within in-lining depv
      LOGICAL, SAVE :: SFC_HONO  = .FALSE. ! flag for HONO interaction with surfaces within in-lining depv
      LOGICAL, SAVE :: ILDEPV    = .TRUE.  ! flag in-lining depv
      LOGICAL, SAVE :: FST       = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: PX_LSM    = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: CLM_LSM   = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: NOAH_LSM  = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: DEPV_DIAG = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: STAGE     = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      Logical, Save :: MINKZ     = .TRUE.  ! flag for minimum Kz 
      Logical, Save :: HGBIDI    = .FALSE. ! flag for Hg bidirectional exchange
      LOGICAL, SAVE :: W_VEL     = .FALSE. ! flag for vertical velocity 
      LOGICAL, SAVE :: GRAV_SETL = .TRUE.  ! flag for aerosol gravitational setling
      LOGICAL, SAVE :: PX_VERSION= .TRUE.  ! true: use PX version of MCIP
      CHARACTER(16) :: LAND_SCHEME = "UNKNOWN" ! NLCD, USGS, etc
 

      ! Transport Processes
      LOGICAL, SAVE :: VDIFFDIAG = .FALSE. ! flag for VDIFF diagnostic files
      REAL,    SAVE :: SIGST     = 0.7     ! sigma_sync_top value
      REAL,    SAVE :: HDIV_LIM  = 0.9     ! cutoff for max horizontal divergence step adj
      REAL,    SAVE :: CFL       = 0.75    ! maximum Courant-Friedrichs-Lewy number allowed


      ! Emissions Processes
      INTEGER, SAVE :: EMLAYS        = 0       ! Emission Layers
      INTEGER, SAVE :: NPTGRPS       = 0       ! no. pt src input file groups
      LOGICAL, SAVE :: EMISCHK       = .TRUE.  ! flag for checking that surrogate emissions 
                                               !   are present on emission files
      LOGICAL, SAVE :: BIOGEMIS      = .FALSE. ! flag to in-line biogenic VOC emissions
      CHARACTER(5)  :: SPPRO         = 'B3V10' ! requested speciation profile name
      LOGICAL, SAVE :: BIOGEMIS_SEASON=.TRUE.  ! use frost switch file
      LOGICAL, SAVE :: ASSUME_SUMMER = .TRUE.  ! use summer normalized emissions
      LOGICAL, SAVE :: BEMIS_DIAG    = .TRUE.  ! true: write diagnostic emiss file
      LOGICAL, SAVE :: MGEMDIAG      = .FALSE. ! flag for MGEM diagnostic file
      LOGICAL, SAVE :: WB_DUST       = .FALSE. ! flag for On-Line Dust Emission Calculation
      LOGICAL, SAVE :: ERODE_AGLAND  = .TRUE.  ! Account for Erodible Agricultural Land when 
                                               !   computing dust Emissions
      LOGICAL, SAVE :: DUSTEM_DIAG   = .FALSE. ! flag for dustemis diagnostic file
      LOGICAL, SAVE :: SSEMDIAG      = .FALSE. ! flag for SSEMIS diagnostic file
      LOGICAL, SAVE :: LTNG_NO       = .FALSE. ! flag for online calculation of NO from lightning
      LOGICAL, SAVE :: NLDNSTRIKE    = .FALSE. ! flag to use NLDN STRIKE directly 
      LOGICAL, SAVE :: LTNGDIAG      = .FALSE. ! flag to turn on lightning NO diagnostics
      REAL   , SAVE :: MOLSNCG       = 350.0   ! Lightning NO Production Rate
      REAL   , SAVE :: MOLSNIC       = 350.0   ! Lightning NO Production Rate

      LOGICAL, SAVE :: PT3DEMIS      = .FALSE. ! flag in-lining plume rise
      LOGICAL, SAVE :: PT3DDIAG      = .FALSE. ! Write point source 3d emis diagnostic file
      LOGICAL, SAVE :: PT3DFRAC      = .FALSE. ! Write layer fractions diagnostic file, if true
      INTEGER, SAVE :: PT_NSTEPS     = 1
      INTEGER, SAVE :: PT_DATE       = 0       ! Julian start date (YYYYDDD)
      INTEGER, SAVE :: PT_TIME       = 0       ! start time (HHMMSS)
      INTEGER, SAVE :: IPVERT        = 0       ! Numerical flag for plume vertical spread method
      INTEGER, SAVE :: REP_LAYR      = -1      ! Minimum layer for reporting srcs w/ high plumes
      CHARACTER(  16 ), ALLOCATABLE, SAVE :: PLAY_BASE( : ) ! Use for Plume Rise Calculation

      ! Processs Analysis
      LOGICAL, SAVE :: PROCAN    = .FALSE. ! flag for process analysis
      INTEGER, SAVE :: PA_BEGCOL = 0       ! Starting PA output
      INTEGER, SAVE :: PA_ENDCOL = 0       ! ending column for PA output
      INTEGER, SAVE :: PA_BEGROW = 0       ! Starting row for PA output
      INTEGER, SAVE :: PA_ENDROW = 0       ! ending row for PA output
      INTEGER, SAVE :: PA_BEGLEV = 0       ! Starting layer for PA output
      INTEGER, SAVE :: PA_ENDLEV = 0       ! ending layer for PA output

      CONTAINS

          
#ifdef twoway
!.........................................................................
      SUBROUTINE INIT_TWOWAY_ENV_VARS( )

!  Defines and retrieves values for twoway model environment variables.
!.........................................................................
      USE UTILIO_DEFN

      IMPLICIT NONE                         
      
      CHARACTER(  48 ) :: VARDESC
      CHARACTER(  16 ), SAVE :: V_LIST( 2 )

      ! Flag for Calculating Feedback From AQ Model to Meteorology
      VARDESC = 'CMAQ WRF radiative feedback'
      CMAQ_WRF_FEEDBACK = GET_ENVYN( 'CTM_CMAQ_WRF_FEEDBACK', VARDESC, CMAQ_WRF_FEEDBACK )

      ! Flag for getting SD Time Series
      VARDESC = 'This does not appear to exist'
      SD_TIME_SERIES = GET_ENVYN( 'CTM_SD_TIME_SERIES', VARDESC, SD_TIME_SERIES )
     
      ! Flag for turning on Potential Vorticity Scaling for High
      ! Altitude Ozone Adjustment
      VARDESC = 'Potential Vorticity Scaling flag'
      TURN_ON_PV = GET_ENVYN('CTM_TURN_ON_PV', VARDESC, TURN_ON_PV )

      ! Flag for Creating a Physical File
      VARDESC = 'Create Physical File'
      CREATE_PHYSICAL_FILE = GET_ENVYN('CREATE_PHYSICAL_FILE', VARDESC, CREATE_PHYSICAL_FILE )
      IF ( CREATE_PHYSICAL_FILE ) THEN
         ! Retrieve Time Step for File
         VARDESC = 'Physical File Time Step'
         FILE_TIME_STEP = GET_ENVINT('FILE_TIME_STEP', VARDESC, FILE_TIME_STEP )
      END IF

      ! Flag for Running CMAQ Drive
      VARDESC = 'Run CMAQ Driver'
      RUN_CMAQ_DRIVER = GET_ENVYN('RUN_CMAQ_DRIVER', VARDESC, RUN_CMAQ_DRIVER )

      ! Flag for Calculating Indirect Effect
      VARDESC = 'Calculate Indirect Effect'
      INDIRECT_EFFECT = GET_ENVYN ('INDIRECT_EFFECT', VARDESC, INDIRECT_EFFECT )

      ! Grid Boundaries
      VARDESC = 'Retrieve SD_SCOL'
      SD_SCOL = GET_ENVINT('SD_SCOL', VARDESC, SD_SCOL)
      
      ! Grid Boundaries
      VARDESC = 'Retrieve SD_SROW'
      SD_SROW = GET_ENVINT('SD_SROW', VARDESC, SD_SROW)
      
      ! Grid Boundaries
      VARDESC = 'Retrieve SD_ECOL'
      SD_ECOL = GET_ENVINT('SD_ECOL', VARDESC, SD_ECOL)
      
      ! Grid Boundaries
      VARDESC = 'Retrieve SD_EROW'
      SD_EROW = GET_ENVINT('SD_EROW', VARDESC, SD_EROW)
      
      ! Load List of Species to Output (SD_SPCS is Preallocated very
      ! large so that no dynamic allocation is needed)
      CALL GET_ENVLIST( 'SD_CONC_SPCS', N_SD_SPCS, SD_SPCS )   
      
      ! Retrieve CMAQ Grid Parameters
      VARDESC = 'Retrieve CMAQ Column Dimension'
      CMAQ_C_COL_DIM = GET_ENVINT('CMAQ_COL_DIM', VARDESC, CMAQ_C_COL_DIM )

      ! Retrieve CMAQ Grid Parameters
      VARDESC = 'Retrieve CMAQ Row Dimension'
      CMAQ_C_ROW_DIM = GET_ENVINT('CMAQ_ROW_DIM', VARDESC, CMAQ_C_ROW_DIM )

      ! Retrieve Two-Way Coupled Model Delta X
      VARDESC = 'Retrieve Two-Way Model Delta X'
      DELTA_X = GET_ENVINT('TWOWAY_DELTA_X', VARDESC, DELTA_X )

      ! Retrieve Two-Way Coupled Model Delta Y
      VARDESC = 'Retrieve Two-Way Model Delta Y'
      DELTA_Y = GET_ENVINT('TwOWAY_DELTA_Y', VARDESC, DELTA_Y )

      ! WRF Reference Latitude
      VARDESC = 'Retrieve WRF Reference Latitude'
      WRF_LC_REF_LAT = GET_ENVREAL('WRF_LC_REF_LAT', VARDESC, WRF_LC_REF_LAT )

      ! Get Frequency of CMAQ Executions
      VARDESC = 'Frequency of CMAQ Execution'
      WRF_CMAQ_FREQ = GET_ENVINT('WRF_CMAQ_FREQ', VARDESC, WRF_CMAQ_FREQ )
 
      ! Get Grid Description
      VARDESC = 'Grid Description'
      GRDDESC_FNAME = GET_ENVSTR( 'GRIDDESC', VARDESC, GRDDESC_FNAME )

      ! Get Grid Name
      VARDESC = 'Grid Name'
      GRID_NAME_STR = GET_ENVSTR( 'GRID_NAME', VARDESC, GRID_NAME_STR )
 
      END SUBROUTINE INIT_TWOWAY_ENV_VARS
#endif 



!.........................................................................
      SUBROUTINE INIT_ENV_VARS( JDATE, JTIME )

!  Defines and retrieves values for all environment variable input to 
!  CMAQ. 
!.........................................................................
      USE UTILIO_DEFN

      IMPLICIT NONE                         

      INTEGER, INTENT( In )  :: JDATE       
      INTEGER, INTENT( In )  :: JTIME  
      CHARACTER(  48 ) :: VARDESC
      CHARACTER(  16 ), SAVE :: V_LIST( 2 )


      LOGICAL SUCCESS
      CHARACTER( 240 )       :: MSG   = ' '  
      CHARACTER(  16 ), SAVE :: PNAME = 'Init_Vars'
      CHARACTER(  8 ) :: APPL = 'CTM_APPL'
      CHARACTER( 17 )  :: CTM_CMAQ_WRF_FEEDBACK = 'CMAQ_WRF_FEEDBACK'! CMAQ WRF radiative feedback
      CHARACTER( 16 )  :: CTM_SD_TIME_SERIES    = 'SD_TIME_SERIES'   ! ????????????????????????????
      CHARACTER( 16 )  :: CTM_STDATE            = 'CTM_STDATE'       ! start date
      CHARACTER( 16 )  :: CTM_STTIME            = 'CTM_STTIME'       ! start time
      CHARACTER( 16 )  :: CTM_RUNLEN            = 'CTM_RUNLEN'       ! run duration
      CHARACTER( 16 )  :: CTM_TSTEP             = 'CTM_TSTEP'        ! output time step
      CHARACTER( 16 )  :: CTM_PROGNAME          = 'CTM_PROGNAME'     ! driver program name (Why?)
      CHARACTER( 16 )  :: CTM_PROGNAME          = 'CTM_LERROFF'      ! stop on inconsistent input files
      CHARACTER( 16 )  :: CTM_MAXSYNC           = 'CTM_MAXSYNC'      ! max sync step
      CHARACTER( 16 )  :: CTM_MINSYNC           = 'CTM_MINSYNC'      ! min sync step
      CHARACTER( 16 )  :: SIGMA_SYNC_TOP        = 'SIGMA_SYNC_TOP'   ! adv top layer for sync step calculation
      CHARACTER( 16 )  :: CTM_ADV_CFL           = 'CTM_ADV_CFL'      ! max Courant-Friedrichs-Lewy number allowed
      CHARACTER( 16 )  :: ADV_HDIV_LIM          = 'ADV_HDIV_LIM'     ! max horizontal divergence for adv step adjustment
      CHARACTER( 16 )  :: CTM_CKSUM             = 'CTM_CKSUM'        ! check the sum of all variables after each science process
      CHARACTER( 16 )  :: CTM_ILDEPV            = 'CTM_ILDEPV      ' ! in-line DEPV
      CHARACTER( 16 )  :: CTM_ABFLUX            = 'CTM_ABFLUX      ' ! ammonia bi-directional flux
      CHARACTER( 16 )  :: CTM_MOSAIC            = 'CTM_MOSAIC      ' ! mosaic - luc specific deposition velocities
      CHARACTER( 16 )  :: CTM_FST               = 'CTM_FST         ' ! mosaic - luc specific stomatal flux
      CHARACTER( 16 )  :: CTM_DEPV_FILE         = 'CTM_DEPV_FILE'    ! diagnostic DEPV file
      CHARACTER( 16 )  :: CTM_STAGE             = 'CTM_STAGE'        ! surface tiled aerosol and gaseous exchange model
      Character( 16 )  :: CTM_HGBIDI            = 'CTM_HGBIDI'       ! Hg bidirectional exchange
      CHARACTER( 16 )  :: CTM_WVEL              = 'CTM_WVEL'         ! vertical velocity diagnostic
      CHARACTER( 16 )  :: CTM_KZMIN             = 'KZMIN'            ! minium Kz
      CHARACTER( 16 )  :: CTM_AERO_GRAV_SETL    = 'CTM_GRAV_SETL'    ! aerosol gravitational settling
      CHARACTER( 16 )  :: CTM_VDIFF_DIAG_FILE   = 'VDIFF_DIAG_FILE'  ! VDIFF diagnostic files
      CHARACTER( 16 )  :: CTM_BIOGEMIS          = 'CTM_BIOGEMIS'     ! inline biogenic VOC emissions
      CHARACTER( 16 )  :: CTM_PMDIAG            = 'CTM_PMDIAG'       ! instantaneous PM diagnostic file
      CHARACTER( 16 )  :: CTM_APMDIAG           = 'CTM_APMDIAG'      ! average PM diagnostic file
      CHARACTER( 32 )  :: AVG_PMDIAG_SPCS       = 'AVG_PMDIAG_SPCS'  ! List of Species to print on PMDIAG file
      CHARACTER( 32 )  :: APMDIAG_BLEV_ELEV     = 'APMDIAG_BLEV_ELEV'! Layer range for PMDIAG File
      CHARACTER( 16 )  :: CTM_AVISDIAG          = 'CTM_AVISDIAG'     ! Flag for printing aerosol visibility diag file

      CHARACTER( 16 )  :: CTM_WB_DUST           = 'CTM_WB_DUST'      ! On-Line Wind-Blown Dust Calculation
      CHARACTER( 16 )  :: CTM_ZERO_PCSOA        = 'CTM_ZERO_PCSOA'   ! Zero Out pcSOA Formation
      CHARACTER( 16 )  :: CORE_SHELL_OPTICS     = 'CORE_SHELL_OPTICS'! Calculate aerosol optics with core-shell model
      CHARACTER( 16 )  :: OPTICS_MIE_CALC       = 'OPTICS_MIE_CALC'  ! Use Mie Theory for Aerosol Optics
      CHARACTER( 16 )  :: CTM_EMISCHK           = 'CTM_EMISCHK'      ! Check emission file species against user-requested species
      CHARACTER( 16 )  :: CTM_EMLAYS            = 'CTM_EMLAYS'       ! Emission Layers
      CHARACTER( 16 )  :: CTM_LTNG_NO           = 'CTM_LTNG_NO'      ! Flag for Turning Lightning NO calculations On
      CHARACTER( 16 )  :: CTM_NLDN              = 'USE_NLDN'         ! Use Natl Lightning Data Collection Network
      CHARACTER( 16 )  :: CTM_LTNG_DIAG         = 'LTNGDIAG'         ! Turn on Lightning Diagnostic File
      CHARACTER( 16 )  :: CTM_MGEMDIAG          = 'CTM_MGEMDIAG'     ! Flag for Printing Marine Gas DIag File
      CHARACTER( 16 )  :: CTM_PT3DEMIS          = 'CTM_PT3DEMIS'     ! Turn on 3D Point Emissions
      CHARACTER( 16 )  :: CTM_SSEMDIAG          = 'CTM_SSEMDIAG'     ! Turn On Sea Spray Emission Diag File
      CHARACTER( 16 )  :: CTM_AOD               = 'CTM_AOD'          ! Turn on Aerosol Optical Depth Output File
      CHARACTER( 16 )  :: CTM_NEW_START         = 'NEW_START'        ! True if this is the first day of a simulation. False if this is a restart
      CHARACTER( 16 )  :: CTM_PROCAN            = 'CTM_PROCAN'       ! Turn on Process Analysis
      CHARACTER( 16 )  :: CTM_PHOTDIAG          = 'CTM_PHOTDIAG'     ! Turn on Photolysis DIagnostic FIles
      CHARACTER( 16 )  :: CTM_DUSTEM_DIAG       = 'CTM_DUSTEM_DIAG'  ! Turn on Dust Diagnostic
      CHARACTER( 16 )  :: CTM_ERODE_AGLAND      = 'CTM_ERODE_AGLAND' ! Turn on file for erodible cropland
      CHARACTER( 16 )  :: HGRD_NAME             = 'GRID_NAME'        ! Name of CMAQ Grid Configuration
      CHARACTER( 16 )  :: CTM_GEAR_RTOL         = 'GEAR_RTOL'        ! Name of env variable for RTOL in SMVGEAR
      CHARACTER( 16 )  :: CTM_GEAR_ATOL         = 'GEAR_ATOL'        ! Name of env variable for ATOL in SMVGEAR
      CHARACTER( 16 )  :: GC_MATRIX             = 'GC_MATRIX_NML'    ! GC Namelist
      CHARACTER( 16 )  :: AE_MATRIX             = 'AE_MATRIX_NML'    ! AE Namelist
      CHARACTER( 16 )  :: NR_MATRIX             = 'NR_MATRIX_NML'    ! NR Namelist
      CHARACTER( 16 )  :: TR_MATRIX             = 'TR_MATRIX_NML'    ! TR Namelist
      CHARACTER( 16 )  :: AVG_CONC_SPCS         = 'AVG_CONC_SPCS'    ! Species to be printed on Avg Conc file
      CHARACTER( 16 )  :: ACONC_BLEV_ELEV       = 'ACONC_BLEV_ELEV'  ! Layer range for avg concentration file
      CHARACTER( 16 )  :: STD_CONC_SPCS         = 'CONC_SPCS'        ! Species to be printed on instantaneous conc file
      CHARACTER( 16 )  :: CONC_BLEV_ELEV        = 'CONC_BLEV_ELEV'   ! Layer range for instantaneous output file
      CHARACTER( 16 )  :: NPCOL_NPROW           = 'NPCOL_NPROW'      ! Number of processors in the row and column directions
      CHARACTER( 16 )  :: PA_BCOL_ECOL          = 'PA_BCOL_ECOL'     ! Process Analysis, Beginning and Ending Columns
      CHARACTER( 16 )  :: PA_BROW_EROW          = 'PA_BROW_EROW'     ! Process Analysis, Beginning and Ending Rows
      CHARACTER( 16 )  :: PA_BLEV_ELEV          = 'PA_BLEV_ELEV'     ! Process Analysis, Beginning and Ending Layers

 
      !variables for deposition velocities diagnostic file
      INTEGER          :: STATUS      ! ENV... status
      CHARACTER( 80 )  :: VARDESC     ! environment variable description

      !-------------------------------------------------------------------------------------------------------
      !>> Grid and High-Level Model Parameters
      !-------------------------------------------------------------------------------------------------------
    
      ! Determine if this run is a new start or a restart
      VARDESC = 'Flag for Model Restart from Previous Day' 
      NEW_START = ENVYN( CTM_NEW_START, VARDESC, NEW_START )
    
      ! Get Simulation Scenario Name to Label Log Files, etc
      VARDESC = 'Reading APPL string to use for labeling log files'
      APPL_NAME = GET_ENVSTR( 'CTM_APPL', VARDESC, APPL_NAME )
      
      ! Get Logfile Directory
      VARDESC = 'Retrieving directory where logfiles will be written'
      LOGFOLD = GET_ENVSTR( "BLD", VARDESC, LOGFOLD )
 
      ! Start I/O-API and set up log file(s)
      CALL SETUP_LOGDEV()

      ! Get Grid Name
      VARDESC = 'Horizontal Domain Definition '
      GRID_NAME = GET_ENVSTR( 'HGRD_NAME', VARDESC, GRID_NAME )

#ifndef twoway
      ! Retrieve the domain decompostion processor array
      IF ( NPROCS .GT. 1 ) THEN
         CALL GET_ENVLIST ( NPCOL_NPROW, NV, V_LIST2 )
         IF ( NV .NE. 2 ) THEN
            XMSG = 'Environment variable problem for ' // NPCOL_NPROW
     &           // ' using default 1X1'
            CALL M3WARN ( 'INIT_ENV_VARS', 0, 0, XMSG )
            NV = 2
            V_LIST2( 1 ) = '1'
            V_LIST2( 2 ) = '1'
         END IF
         READ( V_LIST2( 1 ), '( I4 )' ) NPCOL
         READ( V_LIST2( 2 ), '( I4 )' ) NPROW
      END IF
      
      ! Main Program Name
      VARDESC = 'Main Program Name'
      PROGNAME = GET_ENVSTR( 'CTM_PROGNAME', VARDESC, PROGNAME )

      ! Output Time Step
      VARDESC = 'Scenario Output Time Step (HHMMSS)'
      LOCAL_TSTEP = GET_ENVINT( 'CTM_TSTEP', VARDESC, LOCAL_TSTEP )

      ! Starting Date
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      STDATE = GET_ENVINT( 'CTM_STDATE', VARDESC, STDATE )

      ! Starting Time
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      STTIME = GET_ENVINT( 'CTM_STTIME', VARDESC, STTIME )

      ! Run Duration
      VARDESC = 'Scenario Run Duration (HHMMSS)'
      RUNLEN = GET_ENVINT( 'CTM_RUNLEN', VARDESC, RUNLEN )
#endif
         
      ! PWRTFLAG
      VARDESC = 'Print confirmation of write operations'
      PWRTFLAG = GET_ENVYN( 'IOAPI_LOG_WRITE', VARDESC, PWRTFLAG )

      ! LERROFF
      VARDESC = 'Flag for stopping on errors'
      LERROFF = GET_ENVYN( 'CTM_LERROFF', VARDESC, LERROFF )  

      ! MAXSYNC
      VARDESC = 'Maximum Synchronization Time Step (sec)'
      MAXSYNC = GET_ENVINT( 'CTM_MAXSYNC', VARDESC, MAXSYNC )

      ! MINSYNC
      VARDESC = 'Minimum Synchronization Time Step (sec)'
      MINSYNC = GET_ENVINT( 'CTM_MINSYNC', VARDESC, MINSYNC )

      ! CFL Criteria
      VARDESC = 'Maximum CFL number allowed'
      CFL = GET_ENVREAL( 'CTM_ADV_CFL', VARDESC, CFL )

      ! Get Minimum Layer for Advection Time Step Adjustment
      VARDESC = 'Minimum layer limit for which adv = sync'
      SIGST = GET_ENVREAL( 'SIGMA_SYNC_TOP', VARDESC, SIGST )

      ! Get Maximum Horizontal Div Limit for Advection Adjustment
      VARDESC = 'Maximum horizontal div. limit for adv step adjustment'
      HDIV_LIM = GET_ENVREAL( ADV_HDIV_LIM, VARDESC, HDIV_LIM )

      !-------------------------------------------------------------------------------------------------------
      !>> SciProc Environment Variables 
      !-------------------------------------------------------------------------------------------------------
      ! CKSUM
      VARDESC = 'Check sum on flag'
      CKSUM = GET_ENVYN( 'CTM_CKSUM', VARDESC, CKSUM )

      ! Check if using PX version of MCIP
      VARDESC = 'Specify whether MCIP/WRF data utilized the Pleim-Xu LSM'
      PX_VERSION = GET_ENVYN( 'PX_VERSION', VARDESC, PX_VERSION )

      !-------------------------------------------------------------------------------------------------------
      !>> Air-Surface Exchange Environment Variables 
      !-------------------------------------------------------------------------------------------------------

      ! FLag for In-line deposition velocities calculation
      VARDESC = 'Flag for in-line deposition velocities'
      ILDEPV = GET_ENVYN( 'CTM_ILDEPV', VARDESC, ILDEPV )

      IF ( ILDEPV ) THEN
         ! Flag for Ammonia bi-directional flux with in-line deposition 
         ! velocities calculation
         VARDESC = 'Flag for ammonia bi-directional flux calculation'
         ABFLUX = GET_ENVYN( 'CTM_ABFLUX', VARDESC, ABFLUX )

         ! Flag for PX LSM
         VARDESC = 'Flag for WRF PX LSM'
         PX_LSM = GET_ENVYN( 'PX_VERSION', VARDESC, PX_LSM )

         ! Flag CLM LSM
         VARDESC = 'Flag for WRF CLM LSM'
         CLM_LSM = GET_ENVYN( 'CLM_VERSION', VARDESC, CLM_LSM )

         ! Flag for NOAH LSM
         VARDESC = 'Flag for WRF NOAH LSM'
         NOAH_LSM = GET_ENVYN( 'NOAH_VERSION', VARDESC, NOAH_LSM )

         ! Flag for HONO interaction with leaf and building surfaces
         VARDESC = 'Flag for HONO interaction with surfaces'
         SFC_HONO = GET_ENVYN( 'CTM_SFC_HONO', VARDESC, SFC_HONO )
         
         ! Flag for Mosaic method to get land-use specific deposition velocities
         VARDESC = 'Flag for land-use specific deposition velocity calculation'
         MOSAIC = GET_ENVYN( 'CTM_MOSAIC', VARDESC, MOSAIC )

         ! Flag for Tiled Aeosol and Gaseous Surface Exchange (TAGSX) model
         VARDESC = 'Flag Tiled Aeosol and Gaseous Surface Exchange (TAGSX) model'
         STAGE = GET_ENVYN( 'CTM_STAGE', VARDESC, STAGE )

         ! Mosaic method to get land-use specific stomatal flux
         IF ( MOSAIC ) THEN
            VARDESC = 'Flag for land-use specific stomatal flux calculation'
            FST = GET_ENVYN( 'CTM_FST', VARDESC, FST )
         END IF
      END IF   ! in-line depv

      ! CTM_DEPV_FILE
      VARDESC = 'Flag for writing the DEPV diagnostic file'
      DEPV_DIAG = GET_ENVYN( 'CTM_DEPV_FILE', VARDESC, DEPV_DIAG )

      ! CTM_HGBIDI
      MSG   = 'Flag for mercury bi-directional flux calculation'
      HGBIDI = GET_ENVYN( 'CTM_HGBIDI', MSG, HGBIDI )

      !-------------------------------------------------------------------------------------------------------
      !>> Vertical Dispersion Environment Variables 
      !-------------------------------------------------------------------------------------------------------
      ! CTM_KZMIN
      VARDESC = 'Flag for Applying a minimum threshold to Kz'
      MINKZ = GET_ENVYN( 'CTM_KZMIN', VARDESC, MINKZ )

      ! CTM_WVEL
      VARDESC = 'Flag for writing vertical velocity output'
      W_VEL = GET_ENVYN( 'CTM_WVEL', VARDESC, W_VEL )

      ! CTM_GRAV_SETL
      VARDESC = 'Using J-,K-mode aerosols gravitational settling'
      GRAV_SETL = GET_ENVYN( 'CTM_GRAV_SETL', VARDESC, GRAV_SETL )

      ! CTM_VDIFF_DIAG_FILE
      VARDESC = 'Writing the VDIFF diagnostic files'
      VDIFFDIAG = GET_ENVYN( 'CTM_VDIFF_DIAG_FILE', VARDESC, VDIFFDIAG )

      !-------------------------------------------------------------------------------------------------------
      !>> Emission Environment Variables 
      !-------------------------------------------------------------------------------------------------------
      ! CTM_BIOGEMIS
      VARDESC = 'Flag for in-line biogenic emissions'
      BIOGEMIS = GET_ENVYN( 'CTM_BIOGEMIS', VARDESC, BIOGEMIS )

      ! CTM_PT3DEMIS
      VARDESC = 'Flag for in-line 3d point source emissions'
      PT3DEMIS = GET_ENVYN( 'CTM_PT3DEMIS',VARDESC, PT3DEMIS )

      ! PT3DDIAG
      VARDESC = 'Flag for outputing Point Source Diagnostic File'
      PT3DDIAG = GET_ENVYN( 'PT3DDIAG',VARDESC, PT3DDIAG )

      ! PT3DFRAC
      VARDESC = 'Flag for outputing Point Source Lyaer Fraction Diagnostic File'
      PT3DFRAC = GET_ENVYN( 'PT3DFRAC', VARDESC, PT3DFRAC )

      ! REP_LAYR
      VARDESC = 'Minimum Layer for reporting plume rise info'
      REP_LAYR = GET_ENVINT( 'REP_LAYER_MIN', VARDESC, REP_LAYR )
 
      ! REP_LAYR
      VARDESC = 'Method for vertical spread'
      IPVERT = ENVINT( 'IPVERT', VARDESC, IPVERT )

      ! get number of different file groups (sectors)
      VARDESC = 'Number of file groups'
      NPTGRPS = ENVINT( 'NPTGRPS', VARDESC, NPTGRPS )
      
      ALLOCATE( PLAY_BASE( NTGRPS ) )
      DO N = 1,NPTGRPS
         WRITE( PBASE,'( "PLAY_BASE", "_", I2.2 )' ) N
         VARDESC = 'Logical name for LAYER FRACTIONS MATRIX'
         PLAY_BASE( N ) = GET_ENVSTR( PBASE, XMSG, ' ', PLAY_BASE( N ) )
      END DO

      ! Point Source Date
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      PT_DATE = GET_ENVINT( 'LAYP_STDATE',VARDESC, PT_DATE )

      ! Point Source Time
      VARDESC = 'Scenario Starting Time (HHMMSS)',
      PT_TIME = GET_ENVINT( 'LAYP_STTIME', VARDESC, PT_TIME )

      ! Point Source Time Steps
      VARDESC = 'Scenario Run Duration (integer steps)'
      PT_NSTEPS = ENVINT( 'LAYP_NSTEPS', VARDESC, PT_NSTEPS )

      ! Bioseason Switch Flag
      VARDESC = 'FLag for using a frost date switch file'
      BIOGEMIS_SEASON = GET_ENVYN ( 'BIOSW_YN', VARDESC, BIOGEMIS_SEASON )

      IF ( .NOT. BIOGEMIS_SEASON ) 
         ! Assume_Summer flag
         VARDESC = 'Flag for using summer normalized emissions'
         ASSUME_SUMMER = GET_ENVYN ( 'SUMMER_YN', VARDESC, ASSUME_SUMMER )
      END IF

      ! Biogenic Emission Diag File
      VARDESC = 'Write biogenic emissions diagnostic file'
      BEMIS_DIAG = GET_ENVYN( 'B3GTS_DIAG', VARDESC, BEMIS_DIAG )

      ! Get the speciation profile to use
      VARDESC = 'Speciation profile to use for biogenics'
      CALL ENVSTR( 'BIOG_SPRO', VARDESC, 'B3V10', SPPRO, IOS )

      !-------------------------------------------------------------------------------------------------------
      !>> Aerosol Processing Environment Variables 
      !-------------------------------------------------------------------------------------------------------
      ! Get flag for instantaneous PM diagnostic file.
      VARDESC = 'Flag for writing the aerosol diagnostic file'
      PMDIAG = GET_ENVYN( 'CTM_PMDIAG', VARDESC, PMDIAG )

      ! Get flag for average PM diagnostic file.
      VARDESC = 'Flag for writing the average aerosol diagnostic file'
      APMDIAG = GET_ENVYN( 'CTM_APMDIAG', VARDESC, APMDIAG )
        
      ! Override default beginning time timestamp for ACONC?
      VARDESC = 'Flag to Override default beginning time timestamp for ACONC'
      END_TIME = GET_ENVYN( 'AVG_FILE_ENDTIME', VARDESC, END_TIME )
      
      ! CONC File Vertical Layer Range and Speciation
      CALL GET_ENVLIST ( CONC_BLEV_ELEV, NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         CONC_BLEV = 1
         CONC_ELEV = NLAYS
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:CONC_BLEV_ELEV
         CONC_BLEV = 1
         CONC_ELEV = V_LIST2( 1 )
      ELSE IF ( NV .EQ. 2 )  ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) CONC_BLEV
         READ( V_LIST2( 2 ), '( I4 )' ) CONC_ELEV
      ELSE 
         XMSG = 'Environment variable error for ' // CONC_BLEV_ELEV
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, XSTAT3 )
      END IF
      
      ! CONC File Species List 
      CALL GET_ENVLIST ( STD_CONC_SPCS, N_CSPCS, CONC_FILE_SPCS )

      ! ACONC Layer Specification
      CALL GET_ENVLIST ( ACONC_BLEV_ELEV, N, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         ACONC_BLEV = 1
         ACONC_ELEV = NLAYS
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:CONC_BLEV_ELEV
         ACONC_BLEV = 1
         ACONC_ELEV = V_LIST2( 1 )
      ELSE IF ( NV .EQ. 2 )  ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) ACONC_BLEV
         READ( V_LIST2( 2 ), '( I4 )' ) ACONC_ELEV
      ELSE 
         XMSG = 'Environment variable error for ' // ACONC_BLEV_ELEV
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, XSTAT3 )
      END IF

      ! ACONC File Species List
      CALL GET_ENVLIST ( AVG_CONC_SPCS, N_ASPCS, ACONC_FILE_SPCS )

      ! APMDIAG Layer Specification 
      CALL GET_ENVLIST ( APMDIAG_BLEV_ELEV, N, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         APMDIAG_ZLO = 1
         APMDIAG_ZHI = NLAYS
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:CONC_BLEV_ELEV
         APMDIAG_ZLO = 1
         APMDIAG_ZHI = V_LIST2( 1 )
      ELSE IF ( NV .EQ. 2 )  ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) APMDIAG_ZLO
         READ( V_LIST2( 2 ), '( I4 )' ) APMDIAG_ZHI
      ELSE 
         XMSG = 'Environment variable error for ' // APMDIAG_BLEV_ELEV
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, XSTAT3 )
      END IF
       
      ! APMDIAG File Species List 
      CALL GET_ENVLIST ( AVG_PMDIAG_SPCS, N_APMDIAG_SPC, APMDIAG_SPC )
 
 
      ! Get aerosol average diagnostic file flag.
      VARDESC = 'Flag for writing the aerosol diagnostic file'
      AVISDIAG = GET_ENVYN( 'CTM_AVISDIAG', VARDESC, AVISDIAG )

      ! Get AOD diagnostic file flag.
      VARDESC = 'Flag for writing the IMPROVE network AOD diagnostic file'
      AOD = GET_ENVYN( CTM_AOD, VARDESC, AOD )
 
      ! Flag for Online Calculation of Windblown dust emissions
      VARDESC = 'Flag for Windblow Dust Emissions'
      WB_DUST = GET_ENVYN( 'CTM_WB_DUST', VARDESC, WB_DUST )

      ! Retrieve the Environment Variable allowing the User to Toggle
      ! the pcVOC emissions
      VARDESC = 'Flag for Ignoring the pcSOA species'
      ZERO_PCSOA = GET_ENVYN( 'CTM_ZERO_PCSOA', VARDESC, ZERO_PCSOA )

      ! Get marine gas emission diagnostic output file flag.
      VARDESC = 'Flag for writing MG emission diagnostic file'
      MGEMDIAG = GET_ENVYN( 'CTM_MGEMDIAG', VARDESC, MGEMDIAG )
 
      ! Get sea-salt-emission diagnostic output file flag.
      VARDESC = 'Flag for writing the sea-salt-emission diagnostic file'
      SSEMDIAG = GET_ENVYN( 'CTM_SSEMDIAG', VARDESC, SSEMDIAG )
 
      ! Get env var for diagnostic output
      VARDESC = 'Flag for writing the windblown dust emission diagnostic file'
      DUSTEM_DIAG = GET_ENVYN( 'CTM_DUSTEM_DIAG', VARDESC, DUSTEM_DIAG )
       
      ! Get env var for erodible agriculture land fraction
      VARDESC = 'Flag for calculating erodible agriculture land fraction'
      ERODE_AGLAND = GET_ENVYN( 'CTM_ERODE_AGLAND', VARDESC, ERODEA_AGLAND )
      
      ! Flag for using BELD Land Use for WindBlown Dust 
      VARDESC = 'Flag for Wind Blown Dust - Land Use from BELD'
      LAND_SCHEME = GET_ENVSTR( 'CTM_WBDUST_BELD', VARDESC, LAND_SCHEME )
 
      
      ! Get Filename for Gas Species Namelist
      VARDESC = 'Gas Species Namelist Filename'
      GC_NAMELIST = GET_ENVSTR(  'GC_MATRIX_NML', VARDESC, GC_NAMELIST )

      ! Get Filename for Aerosol Species Namelist
      VARDESC = 'Aerosol Species Namelist Filename'
      AE_NAMELIST = GET_ENVSTR(  'AE_MATRIX_NML', VARDESC, AE_NAMELIST )

      ! Get Filename for Nonreactive Species Namelist
      VARDESC = 'Nonreactive Species Namelist Filename'
      NR_NAMELIST = GET_ENVSTR(  'NR_MATRIX_NML', VARDESC, NR_NAMELIST )

      ! Get Filename for Tracer Species Namelist
      VARDESC = 'Tracer Species Namelist Filename'
      TR_NAMELIST = GET_ENVSTR(  'TR_MATRIX_NML', VARDESC, TR_NAMELIST )

      !Absolute Tolerance for SMVGEAR
      VARDESC = 'Absolute tolerance for SMVGEAR'
      GEAR_ATOL = GET_ENVREAL( 'CTM_GEAR_ATOL', VARDESC, GEAR_ATOL )
      
      !Relative Tolerance for SMVGEAR
      VARDESC = 'Relative tolerance for SMVGEAR'
      GEAR_RTOL = GET_ENVREAL( 'CTM_GEAR_RTOL', VARDESC, GEAR_RTOL )
      
      ! Tolerances for Rosenbrock Solver
      VARDESC = 'Relative tolerance for Rosenbrock Solver'
      GLBL_RTOL = GET_ENVREAL( 'RB_RTOL', VARDESC, GLBL_RTOL )

      ! Absolute Tolerance for RB Solver
      VARDESC = 'Absolute tolerance for Rosenbrock Solver'
      GLBL_ATOL = GET_ENVREAL( 'RB_ATOL', VARDESC, GLBL_ATOL )
 
 
      ! Get flag to use core-shell mixing model for aerosol optical properties
      VARDESC = 'Use core-shell mixing model for aerosol optical properties'
      CORE_SHELL = GET_ENVYN( 'CORE_SHELL_OPTICS', VARDESC, CORE_SHELL )

      ! Get flag to use fast optics for volume mixing model for aerosol optical properties
      VARDESC = 'Use Mie theory for aerosol optical properties of Internal mixing model'
      MIE_CALC = GET_ENVYN( OPTICS_MIE_CALC, VARDESC, MIE_CALC )

      ! Number of Layers for Emissions
      VARDESC = 'Number of emission layers'
      EMLAYS = GET_ENVINT( 'CTM_EMLAYS', VARDESC, EMLAYS )

      ! Flag for checking emissions surrogates against species actually
      ! present on emissions files
      VARDESC    = "Let model proceed even if there are emission"
     &           //" surrogates that do not appear on any input file."
      EMISCHK = GET_ENVYN( 'CTM_EMISCHK', VARDESC, EMISCHK )
 
      ! FLag for outputting cloud diagnostics
      VARDESC = 'Output cloud diagnostic files? (Y/N)'
      CLD_DIAG = GET_ENVYN( 'CLD_DIAG', VARDESC, CLD_DIAG )

      ! Flag for using NLDN data for Lightning Strikes
      VARDESC = 'Use NLDN STRIKE'
      NLDNSTRIKE = GET_ENVYN( 'CTM_NLDN', VARDESC, NLDNSTRIKE )
 
      ! FLag for Outputing Lightning Diagnostic Fil
      VARDESC = 'Flag for writing lightning NO production diagnostic file'
      LTNGDIAG = GET_ENVYN( 'CTM_LTNG_DIAG',VARDESC,LTNGDIAG )
       
      ! Set LTNG_NO to Y or T to turn on lightning NO production
      VARDESC = 'Flag for lightning emissions'
      LTNG_NO = GET_ENVYN( CTM_LTNG_NO, VARDESC, LTNG_NO ) 

      ! Get Lightning NO Production Rate
      VARDESC = 'Flag for lightning NO production rate'
      MOLSNCG = GET_ENVREAL( 'MOLSNCG', VARDESC, 350.0 )
      
      ! Get Lightning NO Production Rate
      VARDESC = 'Flag for lightning NO production rate'
      MOLSNIC = GET_ENVREAL( 'MOLSNIC', VARDESC, 350.0 )

      ! Get Lightning NO File Name
      VARDESC = 'lightning NO file name or InLine flag'
      LTNG_FNAME = GET_ENVSTR( 'LTNGNO', VARDESC, LTNG_FNAME )
   
      ! Flag for Using Process Analysis
      VARDESC= 'Flag for using process analysis'
      PROCAN = GET_ENVYN( 'CTM_PROCAN', VARDESC, PROCAN )
       
      ! Get the Beginning and Ending Columns for Process Analysis
      CALL GET_ENVLIST ( PA_BCOL_ECOL, NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   
         PA_BEGCOL = 1; PA_ENDCOL = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGCOL
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDCOL
      ELSE 
         XMSG = 'Environment variable error for ' // PA_BCOL_ECOL
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         SUCCESS = .FALSE.; RETURN
      END IF

      ! Get the Beginning and Ending Rows for Process Analysis
      CALL GET_ENVLIST ( PA_BROW_EROW, NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   
         PA_BEGROW = 1; PA_ENDROW = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGROW
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDROW
      ELSE 
         XMSG = 'Environment variable error for ' // PA_BROW_EROW
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         SUCCESS = .FALSE.; RETURN
      END IF

      ! Get the Beginning and Ending Layers for Process Analysis
      CALL GET_ENVLIST ( PA_BLEV_ELEV, NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN  
         PA_BEGLEV = 1; PA_ENDLEV = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGLEV
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDLEV
      ELSE 
         XMSG = 'Environment variable error for ' // PA_BLEV_ELEV
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         SUCCESS = .FALSE.; RETURN
      END IF
 

      ! Get photolysis rate diagnostic file flag
      VARDESC= 'Flag for writing the photolysis rate diagnostic file'
      PHOTDIAG = GET_ENVYN( 'CTM_PHOTDIAG', VARDESC, PHOTDIAG )


      END SUBROUTINE INIT_VARS

!.........................................................................
      FUNCTION GET_ENVYN( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving TRUE/FALSE and Y/N Environemnt Variables.
!  Applies standard error checking
!.........................................................................
      IMPLICIT NONE

      LOGICAL, INTENT( IN )        :: VARDEF
      CHARACTER( : ), INTENT( IN ) :: VAR_LABEL
      CHARACTER( : ), INTENT( IN ) :: VARDESC
      LOGICAL                      :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG
      INTEGER          :: XSTAT2

      VARVAL = ENVYN( VAR_LABEL, VARDESC, VARDEF, STATUS )
      IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. 1 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENVYN', 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable set, but empty ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      ELSE IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable not set ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF

      WRITE( XMSG, '( 2x,A16,1x,':',1x,A )' ),VAR_LABEL,VARVAL
      CALL LOG_MESSAGE( OUTDEV, XMSG )

      END FUNCTION GET_ENVYN

!.........................................................................
      FUNCTION GET_ENVINT( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving TRUE/FALSE and Y/N Environemnt Variables.
!  Applies standard error checking
!.........................................................................
      IMPLICIT NONE

      INTEGER, INTENT( IN )        :: VARDEF
      CHARACTER( : ), INTENT( IN ) :: VAR_LABEL
      CHARACTER( : ), INTENT( IN ) :: VARDESC
      INTEGER                      :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG
      INTEGER          :: XSTAT2

      VARVAL = ENVYN( VAR_LABEL, VARDESC, VARDEF, STATUS )
      IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. 1 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENVINT', 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable set, but empty ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      ELSE IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable not set ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF
 
      WRITE( XMSG, '( 2x,A16,1x,':',1x,I )' ),VAR_LABEL,VARVAL
      CALL LOG_MESSAGE( OUTDEV, XMSG )
      
      END FUNCTION GET_ENVINT 

!.........................................................................
      FUNCTION GET_ENVREAL( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving TRUE/FALSE and Y/N Environemnt Variables.
!  Applies standard error checking
!.........................................................................
      IMPLICIT NONE

      REAL, INTENT( IN )           :: VARDEF
      CHARACTER( : ), INTENT( IN ) :: VAR_LABEL
      CHARACTER( : ), INTENT( IN ) :: VARDESC
      REAL                         :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG
      INTEGER          :: XSTAT2

      VARVAL = ENVREAL( VAR_LABEL, VARDESC, VARDEF, STATUS )
      IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. 1 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENVREAL', 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable set, but empty ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      ELSE IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable not set ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF
 
      WRITE( XMSG, '( 2x,A16,1x,':',1x,E10.3 )' ),VAR_LABEL,VARVAL
      CALL LOG_MESSAGE( OUTDEV, XMSG )
      
      END FUNCTION GET_ENVREAL 
 
!.........................................................................
      FUNCTION GET_ENVSTR( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving TRUE/FALSE and Y/N Environemnt Variables.
!  Applies standard error checking
!.........................................................................
      IMPLICIT NONE

      CHARACTER( : ), INTENT( IN ) :: VARDEF
      CHARACTER( : ), INTENT( IN ) :: VAR_LABEL
      CHARACTER( : ), INTENT( IN ) :: VARDESC
      CHARACTER( 256 )             :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG
      INTEGER          :: XSTAT2

      CALL ENVSTR( VAR_LABEL, VARDESC, VARDEF, VARVAL, STATUS )
      IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. 1 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENVSTR', 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable set, but empty ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      ELSE IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable not set ... Using default:'
           CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF
 
      WRITE( XMSG, '( 2x,A16,1x,':',1x,A )' ),VAR_LABEL,VARVAL
      CALL LOG_MESSAGE( OUTDEV, XMSG )
      
      END FUNCTION GET_ENVSTR 

!.........................................................................
      SUBROUTINE LOG_HEADING( FUNIT, CHEAD_IN )

!  Formats and writes a user-supplied heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a heading
!  should probably just be one line.
!.........................................................................
      USE M3UTILIO

      IMPLICIT NONE

      INTEGER, INTENT( IN )           :: FUNIT
      CHARACTER( * ), INTENT( IN )    :: CHEAD_IN
      CHARACTER( len=: ), ALLOCATABLE :: CHEAD
      CHARACTER( 20 )                 :: FMT
      CHARACTER( 20 )                 :: FMT2
      INTEGER                         :: LDASH

      ! Capitalize the heading
      CHEAD = CHEAD_IN
      CALL UPCASE( CHEAD )

      ! Write the heading to the log file
      WRITE( FUNIT, * )
      WRITE( FMT, '("(", I0, "x,A,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FMT2,'("(", I0, "x,A,)")' ) LOG_MAJOR_TAB

      LDASH = 2*8 + LEN_TRIM( CHEAD )
      WRITE( FUNIT, FMT2 ), REPEAT( '=', LDASH )
      WRITE( FUNIT, FMT ),
     &       '|>---   ',TRIM( CHEAD ),'   ---<|'
      WRITE( FUNIT, FMT2 ), REPEAT( '=', LDASH )

      END SUBROUTINE LOG_HEADING
      
!.........................................................................
      SUBROUTINE LOG_SUBHEADING( FUNIT, CHEAD )

!  Formats and writes a user-supplied sub-heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a sub-heading
!  should probably just be one line.
!.........................................................................

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: CHEAD
      CHARACTER( 20 )       :: FMT
      INTEGER                         :: LDASH

      ! Write the sub-heading to the log file
      WRITE( FMT, '("(/,", I0, "x,A,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FUNIT, FMT ),'|> ',TRIM( CHEAD ),':'
      LDASH = 2*3 - 1 + LEN_TRIM( CHEAD )

      WRITE( FMT, '("(", I0, "x,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FUNIT, FMT ) '+',REPEAT( '=', LDASH )

      END SUBROUTINE LOG_SUBHEADING
      
!.........................................................................
      SUBROUTINE LOG_MESSAGE( FUNIT, CMSG_IN )

!  Formats and writes a user-supplied message to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ.
!.........................................................................
      USE M3UTILIO

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: CMSG_IN
      CHARACTER( len=: ), ALLOCATABLE :: CMSG
      CHARACTER( 20 )       :: FMT

      CHARACTER( LOG_LINE_LENGTH ) :: CTEMP
      INTEGER :: MSG_SIZE
      INTEGER :: NLINE1, NLINE2, NLINE, NTAB, LAST_SPACE

      CMSG = CMSG_IN

      ! Write the message to the log file, while trimming to 80
      ! characters (while accounting for tab-length)
      NLINE1 = LOG_LINE_LENGTH - LOG_MAJOR_TAB
      NLINE2 = LOG_LINE_LENGTH - LOG_MAJOR_TAB - LOG_MINOR_TAB
      NLINE  = NLINE1
      NTAB   = LOG_MAJOR_TAB
      
      ! Determine Length of Total Message
      MSG_SIZE = LEN_TRIM( CMSG )
      
      DO WHILE ( MSG_SIZE .GT. LOG_LINE_LENGTH )
         ! Isolate One Line of Text
         LAST_SPACE = INDEX( CMSG( 1:NLINE+1 ), " ", BACK=.TRUE. )
     
         CTEMP = CMSG( 1:LAST_SPACE-1 )

         WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
         WRITE( FUNIT, FMT ), CTEMP

         CMSG = CMSG( LAST_SPACE+1:LEN( CMSG ) )
         MSG_SIZE = LEN_TRIM( CMSG )

         IF ( NTAB .EQ. LOG_MAJOR_TAB ) NTAB = NTAB + LOG_MINOR_TAB
         IF ( NLINE.EQ. NLINE1 ) NLINE = NLINE2
      END DO

      ! Write Last Line
      WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
      WRITE( FUNIT, FMT ), TRIM( CMSG )


      END SUBROUTINE LOG_MESSAGE
      
 
      END MODULE RUNTIME_VARS
