
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

      use get_env_module

      IMPLICIT NONE

      SAVE

      PUBLIC

      INTEGER :: OUTDEV = 6   ! File Unit for Standard Output
      INTEGER :: LOGDEV = -1  ! File Unit for Ascii Log File
      INTEGER :: TOTPE  = 1   ! Number of Total Processors
      INTEGER :: NPROCS = 1   ! Number of Total Processors
      INTEGER :: MYPE   = -1  ! Processor Number
      CHARACTER( 3 ) :: CMYPE  = ""  ! Processor Number

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



#ifdef twoway
      !-----------------------------------------------------------------------------------
      !>> Variables for Configuring WRF-CMAQ Coupled Model Runs
      !-----------------------------------------------------------------------------------

      LOGICAL :: CMAQ_WRF_FEEDBACK    = .FALSE. ! Calculate Feedbacks from CMAQ to WRF
      LOGICAL :: TURN_ON_PV           = .FALSE. ! Flag for O3 Potential Vorticity Calculation 
      LOGICAL :: CREATE_PHYSICAL_FILE = .FALSE. ! Flag for Creating a physical output file
      INTEGER :: FILE_TIME_STEP       = 10000   ! Output Time Step
      LOGICAL :: RUN_CMAQ_DRIVER      = .TRUE.  ! Flag for running CMAQ Driver
      LOGICAL :: INDIRECT_EFFECT      = .FALSE. ! Flag for treating Indirect Effects
      LOGICAL :: SD_TIME_SERIES       = .FALSE. ! Subdomain Time Series
      INTEGER :: SD_SCOL              = 1       ! Subdomain Start Column
      INTEGER :: SD_ECOL              = 1       ! Subdomain End Column
      INTEGER :: SD_SROW              = 1       ! Subdomain Start Row
      INTEGER :: SD_EROW              = 1       ! Subdomain End Row
      INTEGER :: N_SD_SPCS            = 0       ! Number of Subdomain Species to Output
      CHARACTER(16) :: SD_SPCS( 900 ) = ''      ! Subdomain Species list 
      INTEGER :: CMAQ_C_COL_DIM       = -1      ! CMAQ Column Dimension
      INTEGER :: CMAQ_C_ROW_DIM       = -1      ! CMAQ Row Dimension
      INTEGER :: DELTA_X              = 5       ! Grid Spacing in x direction
      INTEGER :: DELTA_Y              = 5       ! Grid Spacing in y direction
      REAL    :: WRF_LC_REF_LAT       = 0.0     ! Reference Latitude
      INTEGER :: WRF_CMAQ_FREQ        = 1       ! Frequency of CMAQ Execution
      CHARACTER(250):: GRIDDESC_FNAME = ''      ! Grid Description
      CHARACTER(16) :: GRID_NAME_STR  = ''      ! Grid Name
#endif
      !-----------------------------------------------------------------------------------
      !>> Define Environment Variables for Controlling CMAQ Processes
      !-----------------------------------------------------------------------------------

      ! Grid and High-Level Model Parameters
      LOGICAL :: NEW_START       = .TRUE.   ! Start New Simulation. Not a Restart
      CHARACTER(16) :: GRID_NAME = ''       ! grid name selected from GRIDDESC
      CHARACTER(16) :: PROGNAME  = 'DRIVER' ! Program name selected from GRIDDESC
      INTEGER :: RUNLEN          = 480000   ! Run Length
      INTEGER :: STDATE          = 1995192  ! Start Date
      INTEGER :: STTIME          = 000000   ! Start Time
      INTEGER :: LOCAL_TSTEP     = 010000   ! set to TSTEP( 1 ) in initscen.F
      INTEGER :: NPCOL           = 1        ! no. of processors across grid columns
      INTEGER :: NPROW           = 1        ! no. of processors across grid rows
      INTEGER :: MAXSYNC         = 720      ! force max TSTEP(2) (sec)
      INTEGER :: MINSYNC         = 60       ! force min TSTEP(2) (sec)

      INTEGER, PARAMETER :: MAXLEN_CCTM_APPL     = 200    ! Length of Logfile Names
      CHARACTER( MAXLEN_CCTM_APPL ) :: APPL_NAME = 'APPL' ! Logfile Names
      CHARACTER( MAXLEN_CCTM_APPL ) :: BLDFOLD   = ''     ! Build Directory
      CHARACTER( MAXLEN_CCTM_APPL ) :: OUTDIR    = ''     ! Output Directory

      ! General; Multiprocess control, output and error checking
      LOGICAL :: PRINT_PROC_TIME = .FALSE. ! Flag to print elapsed time for all 
                                           ! science submodules
      LOGICAL :: FL_ERR_STOP=.TRUE.  ! Flag to stop run if errors are found.
      LOGICAL :: CKSUM     = .TRUE.  ! flag for cksum on, default = [T]
      LOGICAL :: END_TIME  = .FALSE. ! Override default beginning ACON timestamp
      
      INTEGER :: N_ASPCS   = 0       ! Number of species saved to avg conc file
      INTEGER :: N_CSPCS   = 0       ! Number of species saved to conc file
      INTEGER :: ACONC_BLEV= 0       ! Beginning level saved to avg conc file
      INTEGER :: ACONC_ELEV= 0       ! Ending level saved to avg conc file
      INTEGER :: CONC_BLEV = 0       ! Beginning level saved to conc file
      INTEGER :: CONC_ELEV = 0       ! Ending level saved to conc file
      CHARACTER(  16 ) :: ACONC_FILE_SPCS( 900 ) = '' ! avg conc file species list
      CHARACTER(  16 ) :: CONC_FILE_SPCS(900 )   = '' ! conc file species list
      LOGICAL :: PWRTFLAG  = .TRUE.  ! Print confirmation of successful output 
                                           ! to logfile
      LOGICAL :: LVEXT = .FALSE.     ! Flag to perform vertical Extraction
      CHARACTER( 1000 ) :: VEXT_COORD_PATH = "" ! File Path for Lon-Lat Text file specifying 
                                                ! locations for vertical extraction
      
      CHARACTER(256) :: GC_NAMELIST = '' ! Gas Species Namelist
      CHARACTER(256) :: AE_NAMELIST = '' ! Aerosol Species Namelist
      CHARACTER(256) :: NR_NAMELIST = '' ! Nonreactive Species Namelist
      CHARACTER(256) :: TR_NAMELIST = '' ! Tracer Species Namelist
      
      ! Chemistry and Photolysis
      LOGICAL :: PHOTDIAG  = .FALSE. ! Flag for PHOTDIAG file
      INTEGER :: NLAYS_DIAG = 0      ! Number of Diagnostic Layers to write out for photolysis
      INTEGER :: NWAVE = 0           ! Number of Diagnostic Wavelengths
      CHARACTER(16) :: WAVE_ENV(100) ! Targeted wavelengths for diagnostic output
      LOGICAL :: CORE_SHELL= .FALSE. ! flag for using core-shell mixing model for aerosol optics
      LOGICAL :: MIE_CALC  = .FALSE. ! flag for using Mie Theory in aerosol optics calculation
      REAL    :: GEAR_ATOL = 1.0E-9  ! Absolute Tolerance for Gear Solver
      REAL    :: GEAR_RTOL = 1.0E-3  ! Relative Tolerance for Gear Solver
      REAL    :: GLBL_ATOL = 1.0E-7  ! Absolute Tolerance for Rosenbrock Solver
      REAL    :: GLBL_RTOL = 1.0E-3  ! Relative Tolerance for Rosenbrock Solver

      ! Aerosols
      LOGICAL :: AVISDIAG  = .FALSE. ! flag for PMDIAG and APMDIAG file [F], default
      LOGICAL :: ZERO_PCSOA= .FALSE. ! flag for Zeroing out pcSOA formation
      LOGICAL :: AOD       = .FALSE. ! flag for printing AOD file
      LOGICAL :: PMDIAG    = .FALSE. ! flag for Instantaneous PM Diagnostic file
      LOGICAL :: APMDIAG   = .FALSE. ! flag for Average PM Diagnostic file
      INTEGER :: APMDIAG_ZLO  = 0    ! PM Diagnostic File Lower Layer
      INTEGER :: APMDIAG_ZHI  = 0    ! PM Diagnostic File Upper Layer
      INTEGER :: N_APMDIAG_SPC= 0    ! PM Diagnostic File Number of Species
      CHARACTER(  16 ) :: APMDIAG_SPC( 200 ) = '' !PM Diagnostic File Species Names

      ! Cloud Parameters
      LOGICAL :: CLD_DIAG  = .FALSE. ! flag to output cloud diagnostic files

      ! Air-Surface Exchange
      LOGICAL :: ILDEPV    = .TRUE.  ! flag in-lining depv
      LOGICAL :: ABFLUX    = .FALSE. ! flag for ammonia bi-directional flux with in-lining depv
      LOGICAL :: MOSAIC    = .FALSE. ! flag for mosaic - luc specific deposition - within in-lining depv
      LOGICAL :: SFC_HONO  = .FALSE. ! flag for HONO interaction with surfaces within in-lining depv
      LOGICAL :: FST       = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL :: PX_LSM    = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL :: CLM_LSM   = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL :: NOAH_LSM  = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL :: STAGE     = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL :: DEPV_DIAG = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      Logical :: HGBIDI    = .FALSE. ! flag for Hg bidirectional exchange
      Logical :: BIDI_FERT_NH3 = .TRUE. ! flag to remove fertilizer ammonia from Bidirectional emissions
      CHARACTER(16) :: DUST_LAND_SCHEME = "UNKNOWN" ! NLCD, USGS, etc
 

      ! Transport Processes
      LOGICAL :: VDIFFDIAG = .FALSE. ! flag for VDIFF diagnostic files
      REAL    :: SIGST     = 0.7     ! sigma_sync_top value
      REAL    :: HDIV_LIM  = 0.9     ! cutoff for max horizontal divergence step adj
      REAL    :: CFL       = 0.75    ! maximum Courant-Friedrichs-Lewy number allowed
      Logical :: MINKZ     = .TRUE.  ! flag for minimum Kz 
      LOGICAL :: W_VEL     = .FALSE. ! flag for vertical velocity 
      LOGICAL :: GRAV_SETL = .TRUE.  ! flag for aerosol gravitational setling


      ! Emissions Processes
      CHARACTER( MAXLEN_CCTM_APPL ) :: EMISSCTRL  = 'EMISSCTRL_NML' ! Emission Control Filename
      INTEGER :: EMLAYS_MX     = 0       ! Emission Layers
      INTEGER :: N_FILE_GR     = 0       ! Number of Gridded Emission Files
      INTEGER :: N_FILE_TR     = 0       ! NUmber of Tracer Emission Files
      LOGICAL :: EMISCHK       = .TRUE.  ! flag for checking that surrogate emissions 
                                               !   are present on emission files
      LOGICAL :: BIOGEMIS      = .FALSE. ! flag to in-line biogenic VOC emissions
      CHARACTER(16) :: SPPRO   = 'DEFAULT' ! requested speciation profile name
      LOGICAL :: BIOGEMIS_SEASON=.TRUE.  ! use frost switch file
      LOGICAL :: ASSUME_SUMMER = .TRUE.  ! use summer normalized emissions
      LOGICAL :: BEMIS_DIAG    = .TRUE.  ! true: write diagnostic emiss file
      LOGICAL :: MGEMDIAG      = .FALSE. ! flag for MGEM diagnostic file
      LOGICAL :: SS_AERO       = .TRUE.  ! flag for Sea Spray Aerosol Calculation
      LOGICAL :: WB_DUST       = .FALSE. ! flag for On-Line Dust Emission Calculation
      LOGICAL :: ERODE_AGLAND  = .TRUE.  ! Account for Erodible Agricultural Land when 
                                               !   computing dust Emissions
      LOGICAL :: DUSTEM_DIAG   = .FALSE. ! flag for dustemis diagnostic file
      LOGICAL :: SSEMDIAG      = .FALSE. ! flag for SSEMIS diagnostic file
      LOGICAL :: LTNG_NO       = .FALSE. ! flag for online calculation of NO from lightning
      INTEGER :: LT_ASM_DT     = 0       ! Lightning Input Time Interval
      LOGICAL :: NLDNSTRIKE    = .FALSE. ! flag to use NLDN STRIKE directly 
      LOGICAL :: LTNGDIAG      = .FALSE. ! flag to turn on lightning NO diagnostics
      REAL    :: MOLSNCG       = 350.0   ! Lightning NO Production Rate
      REAL    :: MOLSNIC       = 350.0   ! Lightning NO Production Rate
      CHARACTER(250) :: LTNG_FNAME = 'InLine' ! Lightning NO Input Name

      INTEGER :: NPTGRPS       = 0       ! no. pt src input file groups
      LOGICAL :: PT3DDIAG      = .FALSE. ! Write point source 3d emis diagnostic file
      LOGICAL :: PT3DFRAC      = .FALSE. ! Write layer fractions diagnostic file, if true
      INTEGER :: PT_NSTEPS     = 1
      INTEGER :: PT_DATE       = 0       ! Julian start date (YYYYDDD)
      INTEGER :: PT_TIME       = 0       ! start time (HHMMSS)
      INTEGER :: IPVERT        = 0       ! Numerical flag for plume vertical spread method
      INTEGER :: REP_LAYR      = -1      ! Minimum layer for reporting srcs w/ high plumes
      CHARACTER( 6 ) :: EMISDIAG = 'FALSE' ! Default Assumption for
                                         !     Emissinos DIagnostic File Output
      LOGICAL :: EMIS_DATE_OVRD = .FALSE. ! Overrider CMAQ not to check
                                          ! if the dates on the emissions file match that of 
                                          ! the interal model
      CHARACTER(  16 ), SAVE, ALLOCATABLE :: PLAY_BASE( : ) ! Use for Plume Rise Calculation

      ! Processs Analysis
      LOGICAL :: PROCAN    = .FALSE. ! flag for process analysis
      INTEGER :: PA_BEGCOL = 0       ! Starting PA output
      INTEGER :: PA_ENDCOL = 0       ! ending column for PA output
      INTEGER :: PA_BEGROW = 0       ! Starting row for PA output
      INTEGER :: PA_ENDROW = 0       ! ending row for PA output
      INTEGER :: PA_BEGLEV = 0       ! Starting layer for PA output
      INTEGER :: PA_ENDLEV = 0       ! ending layer for PA output



      INTERFACE
          SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )
              CHARACTER( * ),  INTENT( IN )  :: ENV_VAR
              INTEGER,         INTENT( OUT ) :: NVARS
              CHARACTER( 16 ), INTENT( OUT)  :: VAL_LIST( : )
          END SUBROUTINE GET_ENVLIST
      END INTERFACE
      
      CONTAINS
          
#ifdef twoway
!.........................................................................
      SUBROUTINE INIT_TWOWAY_ENV_VARS( )

!  Defines and retrieves values for twoway model environment variables.
!.........................................................................
      USE UTILIO_DEFN

      IMPLICIT NONE                         
      
      CHARACTER( 250 ) :: VARDESC
      CHARACTER( 250 ) :: STRTEMP


      ! Flag for Determining whether CMAQ is being driven by itself or a
      ! larger host model.
      VARDESC = 'Flag for invoking CMAQ Driver. [False] for WRF-CMAQ Coupled Model'
      RUN_CMAQ_DRIVER = .FALSE.
      RUN_CMAQ_DRIVER = GET_ENVYN( 'RUN_CMAQ_DRIVER', VARDESC, RUN_CMAQ_DRIVER )
      CMAQ_WRF_FEEDBACK = GET_ENVYN( 'CMAQ_WRF_FEEDBACK', VARDESC, CMAQ_WRF_FEEDBACK )

      ! Flag for Calculating Feedback From AQ Model to Meteorology
      VARDESC = 'CMAQ WRF radiative feedback'
      CMAQ_WRF_FEEDBACK = GET_ENVYN( 'CMAQ_WRF_FEEDBACK', VARDESC, CMAQ_WRF_FEEDBACK )

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

      ! Flag for Calculating Indirect Effect
      VARDESC = 'Calculate Indirect Effect'
      INDIRECT_EFFECT = GET_ENVYN ('INDIRECT_EFFECT', VARDESC, INDIRECT_EFFECT )

      ! Flag for getting SD Time Series
      VARDESC = 'This does not appear to exist'
      SD_TIME_SERIES = GET_ENVYN( 'CTM_SD_TIME_SERIES', VARDESC, SD_TIME_SERIES )
     
      ! Grid Boundaries
      VARDESC = 'Retrieve SD_SCOL'
      SD_SCOL = GET_ENVINT('SD_SCOL', VARDESC, SD_SCOL)
      
      ! Grid Boundaries
      VARDESC = 'Retrieve SD_ECOL'
      SD_ECOL = GET_ENVINT('SD_ECOL', VARDESC, SD_ECOL)
      
      ! Grid Boundaries
      VARDESC = 'Retrieve SD_SROW'
      SD_SROW = GET_ENVINT('SD_SROW', VARDESC, SD_SROW)
      
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
      DELTA_Y = GET_ENVINT('TWOWAY_DELTA_Y', VARDESC, DELTA_Y )

      ! WRF Reference Latitude
      VARDESC = 'Retrieve WRF Reference Latitude'
      WRF_LC_REF_LAT = GET_ENVREAL('WRF_LC_REF_LAT', VARDESC, WRF_LC_REF_LAT )

      ! Get Frequency of CMAQ Executions
      VARDESC = 'Frequency of CMAQ Execution'
      WRF_CMAQ_FREQ = GET_ENVINT('WRF_CMAQ_FREQ', VARDESC, WRF_CMAQ_FREQ )
 
      ! Get Grid Description
      VARDESC = 'Grid Description'
      GRIDDESC_FNAME = GET_ENVSTR( 'GRIDDESC', VARDESC, GRIDDESC_FNAME )

      ! Get Grid Name
      VARDESC = 'Grid Name'
      STRTEMP = GET_ENVSTR( 'GRID_NAME', VARDESC, GRID_NAME_STR )
      GRID_NAME_STR = STRTEMP(1:16)
 
      ! Run Duration
      VARDESC = 'Scenario Run Duration (HHMMSS)'
      RUNLEN = GET_ENVINT( 'CTM_RUNLEN', VARDESC, RUNLEN )
      
      ! Starting Date
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      STDATE = GET_ENVINT( 'CTM_STDATE', VARDESC, STDATE )

      ! Starting Time
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      STTIME = GET_ENVINT( 'CTM_STTIME', VARDESC, STTIME )
 

      END SUBROUTINE INIT_TWOWAY_ENV_VARS
#endif 

!.........................................................................
      SUBROUTINE INIT_ENV_VARS( JDATE, JTIME )

!  Defines and retrieves values for all environment variable input to 
!  CMAQ. 
!.........................................................................
      USE UTILIO_DEFN

      IMPLICIT NONE                         

#ifdef parallel      
      include 'mpif.h'
#endif
 
      INTEGER, INTENT( In )  :: JDATE       
      INTEGER, INTENT( In )  :: JTIME  
      CHARACTER(  16 )       :: V_LIST2( 20 )
      CHARACTER( 240 )       :: XMSG   = ''  
      INTEGER                :: NV


      INTEGER          :: STATUS      ! ENV... status
      CHARACTER( 250)  :: VARDESC     ! environment variable description
      CHARACTER( 250 ) :: STRTEMP
      CHARACTER( 80 )  :: PBASE     
      INTEGER, PARAMETER :: EXIT_STATUS = 1
      INTEGER          :: ERROR


      !-------------------------------------------------------------------------------------------------------
      !>> Grid and High-Level Model Parameters
      !-------------------------------------------------------------------------------------------------------
#ifdef parallel
      CALL MPI_COMM_RANK ( MPI_COMM_WORLD, MYPE, ERROR )
      CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, TOTPE, ERROR )
#else
      MYPE = 0
      TOTPE = 0
#endif
 
      IF ( MYPE .NE. 0 ) OPEN( UNIT = 6, FILE = "/dev/null", STATUS = "OLD" )

      ! Get Simulation Scenario Name to Label Log Files, etc
      CALL NAMEVAL( 'CTM_APPL', APPL_NAME )
      
      IF ( MYPE . NE. 0 ) CLOSE( UNIT = 6 )

      ! Start I/O-API and set up log file(s)
      CALL SETUP_LOGDEV()
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_HEADING( OUTDEV, "Environment Variable Report" )
         CALL LOG_SUBHEADING( OUTDEV, "Grid and High-Level Model Parameters" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
         WRITE( OUTDEV, '(5x,A16,2x,A,2x,A)' ),'CTM_APPL','|',TRIM(APPL_NAME)
      END IF

      ! Get Logfile Directory
      VARDESC = 'Build Directory Location'
      BLDFOLD = GET_ENVSTR( 'BLD', VARDESC, BLDFOLD  ) 
 
      ! Get Logfile Directory
      VARDESC = 'Output Directory Location'
      OUTDIR  = GET_ENVSTR( 'OUTDIR', VARDESC, OUTDIR  ) 
 
      ! Determine if this run is a new start or a restart
      VARDESC = 'Flag for Model Restart from Previous Day' 
      NEW_START = GET_ENVYN( 'NEW_START', VARDESC, NEW_START )
    
      ! Get Grid Name
      VARDESC = 'Horizontal Domain Definition '
      STRTEMP = GET_ENVSTR( 'GRID_NAME', VARDESC, GRID_NAME )
      GRID_NAME = STRTEMP(1:16)


#ifndef twoway
      ! Main Program Name
      VARDESC = 'Main Program Name'
      STRTEMP = GET_ENVSTR( 'CTM_PROGNAME', VARDESC, PROGNAME )
      PROGNAME = STRTEMP( 1:16 )

      ! Run Duration
      VARDESC = 'Scenario Run Duration (HHMMSS)'
      RUNLEN = GET_ENVINT( 'CTM_RUNLEN', VARDESC, RUNLEN )
      
      ! Starting Date
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      STDATE = GET_ENVINT( 'CTM_STDATE', VARDESC, STDATE )

      ! Starting Time
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      STTIME = GET_ENVINT( 'CTM_STTIME', VARDESC, STTIME )

      ! Output Time Step
      VARDESC = 'Scenario Output Time Step (HHMMSS)'
      LOCAL_TSTEP = GET_ENVINT( 'CTM_TSTEP', VARDESC, LOCAL_TSTEP )

      ! Retrieve the domain decomposition processor array
      IF ( NPROCS .GT. 1 ) THEN
         CALL GET_ENVLIST ( 'NPCOL_NPROW', NV, V_LIST2 )
         IF ( NV .NE. 2 ) THEN
            XMSG = 'Environment variable problem for NPCOL_NPROW'
     &           // ' using default 1X1'
            CALL M3WARN ( 'INIT_ENV_VARS', 0, 0, XMSG )
            NV = 2
            V_LIST2( 1 ) = '1'
            V_LIST2( 2 ) = '1'
         END IF
         READ( V_LIST2( 1 ), '( I4 )' ) NPCOL
         READ( V_LIST2( 2 ), '( I4 )' ) NPROW
      END IF
      
#endif
      
      ! MAXSYNC
      VARDESC = 'Maximum Synchronization Time Step (sec)'
      MAXSYNC = GET_ENVINT( 'CTM_MAXSYNC', VARDESC, MAXSYNC )

      ! MINSYNC
      VARDESC = 'Minimum Synchronization Time Step (sec)'
      MINSYNC = GET_ENVINT( 'CTM_MINSYNC', VARDESC, MINSYNC )


      !-------------------------------------------------------------------------------------------------------
      !>> General; Multiprocess control, output and error checking
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Multiprocess control, output and error checking" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! PRINT_PROC_TIME
      VARDESC = 'Flag for printing elapsed time for each science subprocess to logfile'
      PRINT_PROC_TIME = GET_ENVYN( 'PRINT_PROC_TIME', VARDESC, PRINT_PROC_TIME )  

      ! FL_ERR_STOP
      VARDESC = 'Flag for stopping on errors'
      FL_ERR_STOP = GET_ENVYN( 'FL_ERR_STOP', VARDESC, FL_ERR_STOP )  

      ! CKSUM
      VARDESC = 'Check sum on flag'
      CKSUM = GET_ENVYN( 'CTM_CKSUM', VARDESC, CKSUM )

      ! Override default beginning time timestamp for ACONC?
      VARDESC = 'Flag to Override default beginning time timestamp for ACONC'
      END_TIME = GET_ENVYN( 'AVG_FILE_ENDTIME', VARDESC, END_TIME )
      
      ! ACONC File Species List
      CALL GET_ENVLIST ( 'AVG_CONC_SPCS', N_ASPCS, ACONC_FILE_SPCS )

      ! CONC File Species List 
      CALL GET_ENVLIST ( 'CONC_SPCS', N_CSPCS, CONC_FILE_SPCS )
 
      ! ACONC Layer Specification
      CALL GET_ENVLIST ( 'ACONC_BLEV_ELEV', NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         ACONC_BLEV = 1
         ACONC_ELEV = -1
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:CONC_BLEV_ELEV
         ACONC_BLEV = 1
         READ( V_LIST2( 2 ), '( I4 )' ) ACONC_ELEV
      ELSE IF ( NV .EQ. 2 ) THEN ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) ACONC_BLEV
         READ( V_LIST2( 2 ), '( I4 )' ) ACONC_ELEV
      ELSE 
         XMSG = 'Environment variable error for ACONC_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF
  
      ! CONC File Vertical Layer Range and Speciation
      CALL GET_ENVLIST ( 'CONC_BLEV_ELEV', NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         CONC_BLEV = 1
         CONC_ELEV = -1
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:CONC_BLEV_ELEV
         CONC_BLEV = 1
         READ( V_LIST2( 2 ), '( I4 )' ) CONC_ELEV
      ELSE IF ( NV .EQ. 2 ) THEN ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) CONC_BLEV
         READ( V_LIST2( 2 ), '( I4 )' ) CONC_ELEV
      ELSE 
         XMSG = 'Environment variable error for CONC_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF
      
      ! PWRTFLAG
      VARDESC = 'Print confirmation of write operations'
      PWRTFLAG = GET_ENVYN( 'IOAPI_LOG_WRITE', VARDESC, PWRTFLAG )

      ! Get Flag for Vertical Extraction
      VARDESC = 'Extract a Vertical Column of Ouptut Data'
      LVEXT = GET_ENVYN( 'VERTEXT', VARDESC, LVEXT )
#ifdef parallel
      ! Get Filepath for File Specifying Lon-Lat Coordinates for
      ! Vertical Extraction
      VARDESC = 'File Specifying Lon-Lat Coords for Vertical Extraction'
      VEXT_COORD_PATH = GET_ENVSTR( 'VERTEXT_COORD_PATH', VARDESC, VEXT_COORD_PATH )
#else
      IF( LVEXT ) THEN
          LVEXT = .FALSE.
          WRITE( OUTDEV,'(A)')'Option to extract a Vertical Column of Ouptut Data set to YES'
          WRITE( OUTDEV,'(A)')'However, the serial version cannot execute the option'
          WRITE( LOGDEV,'(A)')'Option to extract a Vertical Column of Ouptut Data set to YES'
          WRITE( LOGDEV,'(A)')'However, the serial version cannot execute the option'
      END IF
#endif
      ! Get Filename for Gas Species Namelist
      VARDESC = 'Gas Species Namelist Filename'
      GC_NAMELIST = GET_ENVSTR(  'gc_matrix_nml', VARDESC, GC_NAMELIST )

      ! Get Filename for Aerosol Species Namelist
      VARDESC = 'Aerosol Species Namelist Filename'
      AE_NAMELIST = GET_ENVSTR(  'ae_matrix_nml', VARDESC, AE_NAMELIST )

      ! Get Filename for Nonreactive Species Namelist
      VARDESC = 'Nonreactive Species Namelist Filename'
      NR_NAMELIST = GET_ENVSTR(  'nr_matrix_nml', VARDESC, NR_NAMELIST )

      ! Get Filename for Tracer Species Namelist
      VARDESC = 'Tracer Species Namelist Filename'
      TR_NAMELIST = GET_ENVSTR(  'tr_matrix_nml', VARDESC, TR_NAMELIST )

      !-------------------------------------------------------------------------------------------------------
      !>> Chemistry and Photolysis
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Chemistry and Photolysis" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Get photolysis rate diagnostic file flag
      VARDESC= 'Flag for writing the photolysis rate diagnostic file'
      PHOTDIAG = GET_ENVYN( 'CTM_PHOTDIAG', VARDESC, PHOTDIAG )

      IF( PHOTDIAG ) THEN
      ! Get desired number of layers for PHOTDIAG2 and PHOTDIAG3 files
         VARDESC = 'Number of Layers for PHOTDIAG2 and PHOTDIAG3'
         NLAYS_DIAG = GET_ENVINT( 'NLAYS_PHOTDIAG', VARDESC, NLAYS_DIAG )

      ! Get Desired Wavelengths for Diagnostic Output
        VARDESC = 'Wavelengths for output on PHOTDIAG2 and PHOTDIAG3'
        CALL GET_ENVLIST( 'NWAVE_PHOTDIAG', NWAVE, WAVE_ENV )
      END IF  

      ! Get flag to use core-shell mixing model for aerosol optical properties
      VARDESC = 'Use core-shell mixing model for aerosol optical properties'
      CORE_SHELL = GET_ENVYN( 'CORE_SHELL_OPTICS', VARDESC, CORE_SHELL )

      ! Get flag to use fast optics for volume mixing model for aerosol optical properties
      VARDESC = 'Use Mie theory for aerosol optical properties of Internal mixing model'
      MIE_CALC = GET_ENVYN( 'OPTICS_MIE_CALC', VARDESC, MIE_CALC )

      !Absolute Tolerance for SMVGEAR
      VARDESC = 'Absolute tolerance for SMVGEAR'
      GEAR_ATOL = GET_ENVREAL( 'GEAR_ATOL', VARDESC, GEAR_ATOL )
      
      !Relative Tolerance for SMVGEAR
      VARDESC = 'Relative tolerance for SMVGEAR'
      GEAR_RTOL = GET_ENVREAL( 'GEAR_RTOL', VARDESC, GEAR_RTOL )
      
      ! Tolerances for Rosenbrock Solver
      VARDESC = 'Relative tolerance for Rosenbrock Solver'
      GLBL_RTOL = GET_ENVREAL( 'RB_RTOL', VARDESC, GLBL_RTOL )

      ! Absolute Tolerance for RB Solver
      VARDESC = 'Absolute tolerance for Rosenbrock Solver'
      GLBL_ATOL = GET_ENVREAL( 'RB_ATOL', VARDESC, GLBL_ATOL )
 
      !-------------------------------------------------------------------------------------------------------
      !>> Aerosols
      !-------------------------------------------------------------------------------------------------------
      
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Aerosols" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Get aerosol average diagnostic file flag.
      VARDESC = 'Flag for writing the aerosol diagnostic file'
      AVISDIAG = GET_ENVYN( 'CTM_AVISDIAG', VARDESC, AVISDIAG )

      ! Retrieve the Environment Variable allowing the User to Toggle
      ! the pcVOC emissions
      VARDESC = 'Flag for Ignoring the pcSOA species'
      ZERO_PCSOA = GET_ENVYN( 'CTM_ZERO_PCSOA', VARDESC, ZERO_PCSOA )

      ! Get AOD diagnostic file flag.
      VARDESC = 'Flag for writing the IMPROVE network AOD diagnostic file'
      AOD = GET_ENVYN( 'CTM_AOD', VARDESC, AOD )

      ! Get flag for instantaneous PM diagnostic file.
      VARDESC = 'Flag for writing the aerosol diagnostic file'
      PMDIAG = GET_ENVYN( 'CTM_PMDIAG', VARDESC, PMDIAG )

      ! Get flag for average PM diagnostic file.
      VARDESC = 'Flag for writing the average aerosol diagnostic file'
      APMDIAG = GET_ENVYN( 'CTM_APMDIAG', VARDESC, APMDIAG )

      ! APMDIAG Layer Specification 
      CALL GET_ENVLIST ( 'APMDIAG_BLEV_ELEV', NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         APMDIAG_ZLO = 1
         APMDIAG_ZHI = -1
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:CONC_BLEV_ELEV
         APMDIAG_ZLO = 1
         READ( V_LIST2( 2 ), '( I4 )' ) APMDIAG_ZHI
      ELSE IF ( NV .EQ. 2 ) THEN ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) APMDIAG_ZLO
         READ( V_LIST2( 2 ), '( I4 )' ) APMDIAG_ZHI
      ELSE 
         XMSG = 'Environment variable error for APMDIAG_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF
       
      ! APMDIAG File Species List 
      CALL GET_ENVLIST ( 'AVG_PMDIAG_SPCS', N_APMDIAG_SPC, APMDIAG_SPC )
 
      !-------------------------------------------------------------------------------------------------------
      !>> Cloud Parameters
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Cloud Processes" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! FLag for outputting cloud diagnostics
      VARDESC = 'Output cloud diagnostic files? (Y/N)'
      CLD_DIAG = GET_ENVYN( 'CLD_DIAG', VARDESC, CLD_DIAG )

      !-------------------------------------------------------------------------------------------------------
      !>> Air-Surface Exchange Parameters
      !-------------------------------------------------------------------------------------------------------
 
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Air-Surface Exchange Processes" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Flag for In-line deposition velocities calculation
      VARDESC = 'Flag for in-line deposition velocities'
      ILDEPV = GET_ENVYN( 'CTM_ILDEPV', VARDESC, ILDEPV )

      ! Check if using PX version of MCIP
      VARDESC = 'Specify whether MCIP/WRF data utilized the Pleim-Xu LSM'
      PX_LSM = GET_ENVYN( 'PX_VERSION', VARDESC, PX_LSM )

      IF ( ILDEPV ) THEN
         ! Flag for Ammonia bi-directional flux with in-line deposition 
         ! velocities calculation
         VARDESC = 'Flag for ammonia bi-directional flux calculation'
         ABFLUX = GET_ENVYN( 'CTM_ABFLUX', VARDESC, ABFLUX )

         ! Flag for Mosaic method to get land-use specific deposition velocities
         VARDESC = 'Flag for land-use specific deposition velocity calculation'
         MOSAIC = GET_ENVYN( 'CTM_MOSAIC', VARDESC, MOSAIC )

         ! Flag for HONO interaction with leaf and building surfaces
         VARDESC = 'Flag for HONO interaction with surfaces'
         SFC_HONO = GET_ENVYN( 'CTM_SFC_HONO', VARDESC, SFC_HONO )
         
         ! Mosaic method to get land-use specific stomatal flux
         IF ( MOSAIC ) THEN
            VARDESC = 'Flag for land-use specific stomatal flux calculation'
            FST = GET_ENVYN( 'CTM_FST', VARDESC, FST )
         END IF

         ! Flag CLM LSM
         VARDESC = 'Flag for WRF CLM LSM'
         CLM_LSM = GET_ENVYN( 'CLM_VERSION', VARDESC, CLM_LSM )

         ! Flag for NOAH LSM
         VARDESC = 'Flag for WRF NOAH LSM'
         NOAH_LSM = GET_ENVYN( 'NOAH_VERSION', VARDESC, NOAH_LSM )

         ! Flag for Tiled Aerosol and Gaseous Surface Exchange (TAGSX) model
         VARDESC = 'Flag for Tiled Aerosol and Gaseous Surface Exchange (TAGSX) model'
         STAGE = GET_ENVYN( 'CTM_STAGE', VARDESC, STAGE )

      END IF   ! in-line depv

      ! CTM_DEPV_FILE
      VARDESC = 'Flag for writing the DEPV diagnostic file'
      DEPV_DIAG = GET_ENVYN( 'CTM_DEPV_FILE', VARDESC, DEPV_DIAG )

      ! CTM_HGBIDI
      VARDESC = 'Flag for mercury bi-directional flux calculation'
      HGBIDI = GET_ENVYN( 'CTM_HGBIDI', VARDESC, HGBIDI )
 
      ! CTM_IGNORE_FERT_NH3
      VARDESC = 'Flag for including fertilizer ammonia in bi-directional flux calculation'
      BIDI_FERT_NH3 = GET_ENVYN( 'CTM_BIDI_FERT_NH3', VARDESC, BIDI_FERT_NH3 )
 
      ! Flag for using BELD Land Use for WindBlown Dust 
      VARDESC = 'Flag for Wind Blown Dust - Land Use from BELD'
      STRTEMP = GET_ENVSTR( 'CTM_WBDUST_BELD', VARDESC, DUST_LAND_SCHEME )
      DUST_LAND_SCHEME = STRTEMP( 1:16)
 
      !-------------------------------------------------------------------------------------------------------
      !>> Transport Processes
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Transport Processes" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! CTM_VDIFF_DIAG_FILE
      VARDESC = 'Writing the VDIFF diagnostic files'
      VDIFFDIAG = GET_ENVYN( 'CTM_VDIFF_DIAG_FILE', VARDESC, VDIFFDIAG )

      ! Get Minimum Layer for Advection Time Step Adjustment
      VARDESC = 'Minimum layer limit for which adv = sync'
      SIGST = GET_ENVREAL( 'SIGMA_SYNC_TOP', VARDESC, SIGST )

      ! Get Maximum Horizontal Div Limit for Advection Adjustment
      VARDESC = 'Maximum horizontal div. limit for adv step adjustment'
      HDIV_LIM = GET_ENVREAL( 'ADV_HDIV_LIM', VARDESC, HDIV_LIM )

      ! CFL Criteria
      VARDESC = 'Maximum CFL number allowed'
      CFL = GET_ENVREAL( 'CTM_ADV_CFL', VARDESC, CFL )

      ! CTM_KZMIN
      VARDESC = 'Flag for Applying a minimum threshold to Kz'
      MINKZ = GET_ENVYN( 'KZMIN', VARDESC, MINKZ )

      ! CTM_WVEL
      VARDESC = 'Flag for writing vertical velocity output'
      W_VEL = GET_ENVYN( 'CTM_WVEL', VARDESC, W_VEL )

      ! CTM_GRAV_SETL
      VARDESC = 'Using J-,K-mode aerosols gravitational settling'
      GRAV_SETL = GET_ENVYN( 'CTM_GRAV_SETL', VARDESC, GRAV_SETL )

      !-------------------------------------------------------------------------------------------------------
      !>> Emission Environment Variables 
      !-------------------------------------------------------------------------------------------------------
      
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Emissions Parameters" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Get Name of Emission Control File
      VARDESC = 'Emissions Control File Name'
      EMISSCTRL = GET_ENVSTR( 'EMISSCTRL_NML', VARDESC, EMISSCTRL )

      ! Number of Layers for Emissions
      VARDESC   = 'Number of emission layers'
      EMLAYS_MX = GET_ENVINT( 'CTM_EMLAYS', VARDESC, EMLAYS_MX )

      ! Get number of different Gridded File Emissionts Streams
      VARDESC = 'Number of Gridded Emissions Files'
      N_FILE_GR = GET_ENVINT( 'N_EMIS_GR', VARDESC, N_FILE_GR )
      
      ! Get number of different Gridded File Emissionts Streams
      VARDESC = 'Number of Tracer Emissions Files'
      N_FILE_TR = GET_ENVINT( 'N_EMIS_TR', VARDESC, N_FILE_TR )

      ! Flag for checking emissions surrogates against species actually
      ! present on emissions files
      VARDESC    = "Let model proceed even if there are emission"
     &           //" surrogates that do not appear on any input file."
      EMISCHK = GET_ENVYN( 'CTM_EMISCHK', VARDESC, EMISCHK )
 
      ! CTM_BIOGEMIS
      VARDESC = 'Flag for in-line biogenic emissions'
      BIOGEMIS = GET_ENVYN( 'CTM_BIOGEMIS', VARDESC, BIOGEMIS )

      ! Get the speciation profile to use
      VARDESC = 'Speciation profile to use for biogenics'
      SPPRO = GET_ENVSTR( 'BIOG_SPRO', VARDESC, SPPRO )
 
      ! Bioseason Switch Flag
      VARDESC = 'Flag for using a frost date switch file'
      BIOGEMIS_SEASON = GET_ENVYN ( 'BIOSW_YN', VARDESC, BIOGEMIS_SEASON )

      IF ( .NOT. BIOGEMIS_SEASON ) THEN
         ! Assume_Summer flag
         VARDESC = 'Flag for using summer normalized emissions'
         ASSUME_SUMMER = GET_ENVYN ( 'SUMMER_YN', VARDESC, ASSUME_SUMMER )
      END IF

      ! Biogenic Emission Diag File
      VARDESC = 'Write biogenic emissions diagnostic file'
      BEMIS_DIAG = GET_ENVYN( 'B3GTS_DIAG', VARDESC, BEMIS_DIAG )
 
      ! Get marine gas emission diagnostic output file flag.
      VARDESC = 'Flag for writing MG emission diagnostic file'
      MGEMDIAG = GET_ENVYN( 'CTM_MGEMDIAG', VARDESC, MGEMDIAG )
 
      ! Flag for Online Calculation of Sea Spray Aerosol emissions
      VARDESC = 'Flag for Sea Spray Aerosol Emissions'
      SS_AERO = GET_ENVYN( 'CTM_SS_AERO', VARDESC, SS_AERO )

      ! Flag for Online Calculation of Windblown dust emissions
      VARDESC = 'Flag for Windblow Dust Emissions'
      WB_DUST = GET_ENVYN( 'CTM_WB_DUST', VARDESC, WB_DUST )

      ! Get env var for erodible agriculture land fraction
      VARDESC = 'Flag for calculating erodible agriculture land fraction'
      ERODE_AGLAND = GET_ENVYN( 'CTM_ERODE_AGLAND', VARDESC, ERODE_AGLAND )
      
      ! Get env var for diagnostic output
      VARDESC = 'Flag for writing the windblown dust emission diagnostic file'
      DUSTEM_DIAG = GET_ENVYN( 'CTM_DUSTEM_DIAG', VARDESC, DUSTEM_DIAG )
       
      ! Get sea-salt-emission diagnostic output file flag.
      VARDESC = 'Flag for writing the sea-salt-emission diagnostic file'
      SSEMDIAG = GET_ENVYN( 'CTM_SSEMDIAG', VARDESC, SSEMDIAG )
 
      ! Set LTNG_NO to Y or T to turn on lightning NO production
      VARDESC = 'Flag for lightning emissions'
      LTNG_NO = GET_ENVYN( 'CTM_LTNG_NO', VARDESC, LTNG_NO ) 

      ! Get Lightning Input Time Interval
      VARDESC = 'Time Interval for Lightning Emissions'
      LT_ASM_DT = GET_ENVINT( 'LTNG_ASSIM_DT', VARDESC, LT_ASM_DT ) 

      ! Get Lightning NO File Name
      VARDESC = 'lightning NO file name or InLine flag'
      LTNG_FNAME = GET_ENVSTR( 'LTNGNO', VARDESC, LTNG_FNAME )
 
      ! Flag for using NLDN data for Lightning Strikes
      VARDESC = 'Use NLDN STRIKE'
      NLDNSTRIKE = GET_ENVYN( 'USE_NLDN', VARDESC, NLDNSTRIKE )
 
      ! FLag for Outputing Lightning Diagnostic Fil
      VARDESC = 'Flag for writing lightning NO production diagnostic file'
      LTNGDIAG = GET_ENVYN( 'LTNGDIAG',VARDESC,LTNGDIAG )
       
      ! Get Lightning NO Production Rate
      VARDESC = 'Flag for lightning NO production rate'
      MOLSNCG = GET_ENVREAL( 'MOLSNCG', VARDESC, 350.0 )
      
      ! Get Lightning NO Production Rate
      VARDESC = 'Flag for lightning NO production rate'
      MOLSNIC = GET_ENVREAL( 'MOLSNIC', VARDESC, 350.0 )
 
      ! get number of different file groups (sectors)
      VARDESC = 'Number of file groups'
      NPTGRPS = GET_ENVINT( 'N_EMIS_PT', VARDESC, NPTGRPS )
      
      ! PT3DDIAG
      VARDESC = 'Flag for outputing Point Source Diagnostic File'
      PT3DDIAG = GET_ENVYN( 'PT3DDIAG',VARDESC, PT3DDIAG )

      ! PT3DFRAC
      VARDESC = 'Flag for outputing Point Source Lyaer Fraction Diagnostic File'
      PT3DFRAC = GET_ENVYN( 'PT3DFRAC', VARDESC, PT3DFRAC )

      ! Point Source Time Steps
      VARDESC = 'Scenario Run Duration (integer steps)'
      PT_NSTEPS = GET_ENVINT( 'LAYP_NSTEPS', VARDESC, PT_NSTEPS )

      ! Point Source Date
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      PT_DATE = GET_ENVINT( 'LAYP_STDATE',VARDESC, PT_DATE )

      ! Point Source Time
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      PT_TIME = GET_ENVINT( 'LAYP_STTIME', VARDESC, PT_TIME )

      ! IPVERT
      VARDESC = 'Method for vertical spread'
      IPVERT = GET_ENVINT( 'IPVERT', VARDESC, IPVERT )

      ! REP_LAYR
      VARDESC = 'Minimum Layer for reporting plume rise info'
      REP_LAYR = GET_ENVINT( 'REP_LAYER_MIN', VARDESC, REP_LAYR )
 
      ALLOCATE( PLAY_BASE( NPTGRPS ) )
      PLAY_BASE( : ) = ''
      DO NV = 1,NPTGRPS
         WRITE( PBASE,'( "PLAY_BASE", "_", I2.2 )' ) NV
         VARDESC = 'Logical name for LAYER FRACTIONS MATRIX'
         STRTEMP = GET_ENVSTR( PBASE, VARDESC, PLAY_BASE( NV ) )
         PLAY_BASE( NV ) = STRTEMP
      END DO
       
      ! Determine user-defined default for emissions diagnostics
      VARDESC = 'Flag for emissions diagnostic default'
      EMISDIAG =  GET_ENVSTR( 'EMISDIAG', VARDESC, EMISDIAG )

      ! Determine user-defined default for emissions date override
      VARDESC = 'Flag for emissions date override'
      EMIS_DATE_OVRD = GET_ENVYN( 'EMIS_DATE_OVRD', VARDESC, EMIS_DATE_OVRD )
 
 
      !-------------------------------------------------------------------------------------------------------
      !>> Process Analysis
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Process Analysis Parameters" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Flag for Using Process Analysis
      VARDESC= 'Flag for using process analysis'
      PROCAN = GET_ENVYN( 'CTM_PROCAN', VARDESC, PROCAN )
       
      ! Get the Beginning and Ending Columns for Process Analysis
      CALL GET_ENVLIST ( 'PA_BCOL_ECOL', NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   
         PA_BEGCOL = 1; PA_ENDCOL = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGCOL
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDCOL
      ELSE 
         XMSG = 'Environment variable error for PA_BCOL_ECOL'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF

      ! Get the Beginning and Ending Rows for Process Analysis
      CALL GET_ENVLIST ( 'PA_BROW_EROW', NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN   
         PA_BEGROW = 1; PA_ENDROW = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGROW
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDROW
      ELSE 
         XMSG = 'Environment variable error for PA_BROW_EROW'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF

      ! Get the Beginning and Ending Layers for Process Analysis
      CALL GET_ENVLIST ( 'PA_BLEV_ELEV', NV, V_LIST2 )
      IF ( NV .LE. 0 ) THEN  
         PA_BEGLEV = 1; PA_ENDLEV = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGLEV
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDLEV
      ELSE 
         XMSG = 'Environment variable error for PA_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF


      END SUBROUTINE INIT_ENV_VARS

!.........................................................................
      FUNCTION GET_ENVYN( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving TRUE/FALSE and Y/N environment variables.
!  Applies standard error checking
!.........................................................................
      USE UTILIO_DEFN
      IMPLICIT NONE

      LOGICAL, INTENT( IN )        :: VARDEF
      CHARACTER( * ), INTENT( IN ) :: VAR_LABEL
      CHARACTER(250), INTENT( IN ) :: VARDESC
      LOGICAL                      :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG

      CALL GET_ENV( VARVAL, VAR_LABEL, VARDEF, STATUS )
!     IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENV', 0, 0, XMSG, STATUS )
      ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable not set or empty ... Using default:'
!           CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF

      WRITE( XMSG, '(A20,2x,A,2x,L)' ),VAR_LABEL,'|',VARVAL
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )

      END FUNCTION GET_ENVYN

!.........................................................................
      FUNCTION GET_ENVINT( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving integer environment variables.
!  Applies standard error checking
!.........................................................................
      USE UTILIO_DEFN
      IMPLICIT NONE

      INTEGER, INTENT( IN )        :: VARDEF
      CHARACTER( * ), INTENT( IN ) :: VAR_LABEL
      CHARACTER(250), INTENT( IN ) :: VARDESC
      INTEGER                      :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG

      CALL GET_ENV( VARVAL, VAR_LABEL, VARDEF, STATUS )
!     IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENV', 0, 0, XMSG, STATUS )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable not set or empty ... Using default:' 
!          CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF
 
      WRITE( XMSG, '(A20,2x,A1,2x,I7)' ),VAR_LABEL,'|',VARVAL
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      
      END FUNCTION GET_ENVINT 

!.........................................................................
      FUNCTION GET_ENVREAL( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving real-valued environment variables.
!  Applies standard error checking
!.........................................................................
      USE UTILIO_DEFN
      IMPLICIT NONE

      REAL, INTENT( IN )           :: VARDEF
      CHARACTER( * ), INTENT( IN ) :: VAR_LABEL
      CHARACTER(250), INTENT( IN ) :: VARDESC
      REAL                         :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 250 ) :: XMSG

      CALL GET_ENV( VARVAL, VAR_LABEL, VARDEF, STATUS )
!     IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENV', 0, 0, XMSG, STATUS )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable not set or empty ... Using default:' 
!          CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF
 
      WRITE( XMSG, '(A20,2x,A,2x,E10.3)' ),VAR_LABEL,'|',VARVAL
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      
      END FUNCTION GET_ENVREAL 
 
!.........................................................................
      FUNCTION GET_ENVSTR( VAR_LABEL, VARDESC, VARDEF )  RESULT( VARVAL )

!  Wrapper for retrieving character environment variables.
!  Applies standard error checking
!.........................................................................
      USE UTILIO_DEFN
      IMPLICIT NONE

      CHARACTER( * ), INTENT( IN ) :: VARDEF
      CHARACTER( * ), INTENT( IN ) :: VAR_LABEL
      CHARACTER(250), INTENT( IN ) :: VARDESC
      CHARACTER(250)               :: VARVAL

      INTEGER          :: STATUS
      CHARACTER( 300 ) :: XMSG
      CHARACTER( 40 )    :: FMT

      CALL GET_ENV( VARVAL, VAR_LABEL, VARDEF, STATUS )
!     IF ( STATUS .NE. 0 ) CALL LOG_MESSAGE( LOGDEV, VARDESC )
      IF ( STATUS .EQ. -2 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( 'GET_ENV', 0, 0, XMSG, STATUS )
      ELSE IF ( STATUS .EQ. -1 ) THEN
           XMSG = 'Environment variable not set or empty ... Using default:' 
!          CALL LOG_MESSAGE( LOGDEV, XMSG )
      END IF
 
      IF ( MYPE .EQ. 0 ) THEN 
         WRITE( FMT, '("(", I0, "x,A20,2x,A,2x,A)")' ) LOG_MAJOR_TAB
         WRITE( OUTDEV, FMT ),VAR_LABEL,'|',TRIM(VARVAL)
      END IF

      END FUNCTION GET_ENVSTR 

!.........................................................................
      SUBROUTINE LOG_HEADING( FUNIT, CHEAD_IN )

!  Formats and writes a user-supplied heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a heading
!  should probably just be one line.
!.........................................................................
      USE UTILIO_DEFN

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
      USE UTILIO_DEFN

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

!.........................................................................
      SUBROUTINE TIMING_SPLIT( CPU_TIME_START, IMSG, CPROCIN )

!  This subroutine provides a split for the MPI timing functions and
!  then prints out a message for how much time has passed using a
!  character string input for customizing that message. 
!.........................................................................
 
      IMPLICIT NONE
#ifdef parallel
      INCLUDE 'mpif.h'
#endif
      REAL( 8 )        :: CPU_TIME_START
      REAL( 8 )        :: CPU_TIME_FINISH
      INTEGER          :: IMSG   ! What kind of checkpoint this is
                                 !  1 = 'PROCESS'
                                 !  2 = 'MASTER TIME STEP'
                                 !  3 = 'OUTPUT'
      CHARACTER( * ), INTENT(IN), OPTIONAL :: CPROCIN
      CHARACTER( len=: ), ALLOCATABLE :: CPROC
      CHARACTER( 250 ) :: XMSG
#ifndef parallel
      REAL :: REAL_TIME
#endif


      IF ( PRESENT( CPROCIN ) ) THEN
         CPROC = CPROCIN
      ELSE
         CPROC = ' '
      END IF
 
      ! Record Time at this Checkpoint
#ifdef parallel
      CPU_TIME_FINISH = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_FINISH = REAL( REAL_TIME,8 )
#endif


      ! Assemble the statement requested by the calling program
      SELECT CASE ( IMSG )
      CASE ( 1 )
          ! Write Out The Time to Complete Each Sub-Process
          WRITE( XMSG, 1002 ),TRIM( CPROC ), CPU_TIME_FINISH-CPU_TIME_START
1002      FORMAT ( 2x, A15, ' completed... ', F6.1, ' seconds' )

      CASE ( 2 ) 
          ! Write out the time to complete the entire master time step
          WRITE( XMSG, '(2x,A41,F6.1,A8)' ),'Master Time Step Processing Completed in ',
     &                     (CPU_TIME_FINISH-CPU_TIME_START),' seconds'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, * )

      CASE ( 3 ) 
          ! Write out the time to complete the output procedure
          WRITE( XMSG, '(A,F4.1,A)' ), 'Completed Writing Results to Output Files...',
     &                     (CPU_TIME_FINISH-CPU_TIME_START),' seconds'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, * )
#ifndef twoway
          IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * )
#endif
 
      END SELECT 

      ! Write out the timing statement
#ifndef twoway
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
#endif
#ifdef parallel      
      CPU_TIME_START = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_START = REAL( REAL_TIME,8 )
#endif
      END SUBROUTINE TIMING_SPLIT    
 
      END MODULE RUNTIME_VARS
