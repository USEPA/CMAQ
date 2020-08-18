
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
!
!  History:
!   07/19/18, D. Wong: removed some of the ifdef clause for twoway model
!                      and added new logical environment variables
!                      ncd_64bit_offset and cell_num for MPAS coupling scheme
!  31 Jan 2019  (David Wong)
!     -- removed all twoway related environment variables in this file
!  01 Feb 2019  (David Wong)
!     -- implemented invocation of GET_ENV call directly, removed unnecessay
!        interface block and unnecessary functions
!  02 May 2019  (David Wong)
!     -- set BIOGEMIS_SEASON = .FALSE. as the default value
!  13 May 2019  (David Wong)
!     -- setup environment variable ISAM_NEW_START
!  14 May 2019  (David Wong)
!     -- updated environment variable ISAM_NEW_START with default value 'Y'
!  15 May 2019  (David Wong)
!     -- included check for using marine gas emission or not
!  13 June 2019 (F. Sidi) 
!     -- Set the default for ERODE_AGLAND to FALSE. No longer supported feature
!        in CMAQv5.3
!  25 July 2019 (D. Wong) 
!     -- Included a logic to determine whether met data was created from 
!        WRF V4+ is used
!  01 Aug 2019 (D. Wong) 
!     -- Modified code to work with two-way model
!  07 Nov 2019 (D. Wong) 
!     -- Made RUNLEN environment variable avilable to two-way model as well
!  22 Nov 2019 (F. Sidi) 
!     -- Re-introduced master switch to overide emissions file date for
!        representative day files
!  10 Feb 2020 (D. Wong) 
!     -- Added new environmental variable, MET_TSTEP, to enable
!        running with temporally finer meteorology
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


! this is for MPAS
      LOGICAL :: ncd_64bit_offset = .FALSE.

      !-----------------------------------------------------------------------------------
      !>> Define Environment Variables for Controlling CMAQ Processes
      !-----------------------------------------------------------------------------------

      ! Met model version
      LOGICAL :: WRF_V4P         = .FALSE.  ! Indicator of whether WRF version is 4+ or not

      ! Convective scheme in met model
      LOGICAL :: CONVECTIVE_SCHEME = .TRUE.
      ! Flag for column model
      LOGICAL :: COLUMN_MODEL      = .FALSE.

      ! Grid and High-Level Model Parameters
      LOGICAL :: NEW_START       = .TRUE.   ! Start New Simulation. Not a Restart
      CHARACTER(1) :: ISAM_NEW_START = 'Y'  ! Start New Simulation. Not a Restart for ISAM
      CHARACTER(16) :: GRID_NAME = ''       ! grid name selected from GRIDDESC
      CHARACTER(16) :: PROGNAME  = 'DRIVER' ! Program name selected from GRIDDESC
      INTEGER :: RUNLEN          = 480000   ! Run Length
      INTEGER :: STDATE          = 1995192  ! Start Date
      INTEGER :: STTIME          = 000000   ! Start Time
      INTEGER :: LOCAL_TSTEP     = 010000   ! set to TSTEP( 1 ) in initscen.F
      INTEGER :: MET_TSTEP                  ! set meterology input temporal frequency
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
      
      INTEGER :: N_ACONC_VARS = 0    ! Number of species saved to avg conc file
      INTEGER :: N_CONC_VARS  = 0    ! Number of species saved to conc file
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
      LOGICAL :: BIOGEMIS_SEASON=.FALSE. ! use frost switch file
      LOGICAL :: ASSUME_SUMMER = .TRUE.  ! use summer normalized emissions
      LOGICAL :: BEMIS_DIAG    = .TRUE.  ! true: write diagnostic emiss file
      LOGICAL :: MGEMDIAG      = .FALSE. ! flag for MGEM diagnostic file
      LOGICAL :: OCEAN_CHEM    = .TRUE.  ! Flag for ocean halogen chemistry and sea spray aerosol emissions
      LOGICAL :: WB_DUST       = .FALSE. ! flag for On-Line Dust Emission Calculation
      LOGICAL :: ERODE_AGLAND  = .FALSE.  ! Account for Erodible Agricultural Land when 
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
      INTEGER :: PT_DATE       = 1995192 ! Julian start date (YYYYDDD)
      INTEGER :: PT_TIME       = 0       ! start time (HHMMSS)
      INTEGER :: IPVERT        = 0       ! Numerical flag for plume vertical spread method
      INTEGER :: REP_LAYR      = -1      ! Minimum layer for reporting srcs w/ high plumes
      CHARACTER( 6 ) :: EMISDIAG = 'FALSE' ! Default Assumption for
                                         !     Emissions Diagnostic File Output
      LOGICAL :: EMIS_SYM_DATE = .FALSE.  ! Overrider CMAQ not to check
                                          ! if the dates on the emissions file match that of 
                                          ! the interal model (representative day case)
      CHARACTER(  16 ), SAVE, ALLOCATABLE :: PLAY_BASE( : ) ! Use for Plume Rise Calculation

      LOGICAL :: USE_MARINE_GAS_EMISSION = .FALSE.

      ! Process Analysis
      LOGICAL :: PROCAN    = .FALSE. ! flag for process analysis
      INTEGER :: PA_BEGCOL = 0       ! Starting PA output
      INTEGER :: PA_ENDCOL = 0       ! ending column for PA output
      INTEGER :: PA_BEGROW = 0       ! Starting row for PA output
      INTEGER :: PA_ENDROW = 0       ! ending row for PA output
      INTEGER :: PA_BEGLEV = 0       ! Starting layer for PA output
      INTEGER :: PA_ENDLEV = 0       ! ending layer for PA output

      ! Sulfur tracking
      LOGICAL :: STM        = .FALSE. ! flag for sulfur tracking option
      LOGICAL :: ADJ_STMSPC = .TRUE.  ! flag for normalizing sulfur tracking species

      ! ISAM
      INTEGER :: ISAM_BLEV = 0  ! Beginning level saved to conc file
      INTEGER :: ISAM_ELEV = 0  ! Ending level saved to conc file
      INTEGER :: SA_NLAYS = 0   ! Number of layers saved to conc file
      INTEGER :: AISAM_BLEV = 0 ! Beginning level saved to sa_aconc file
      INTEGER :: AISAM_ELEV = 0 ! Ending level saved to sa_aconc file
      INTEGER :: AVGSA_LAYS = 0 ! Number of layers saved to conc file

      CONTAINS
          
!.........................................................................
      SUBROUTINE INIT_ENV_VARS( JDATE, JTIME )

!  Defines and retrieves values for all environment variable input to 
!  CMAQ. 
!.........................................................................

      USE RXNS_DATA, ONLY : MECHNAME
      USE M3UTILIO

      IMPLICIT NONE                         

      INCLUDE SUBST_FILES_ID             ! file name parameters

#ifdef parallel      
      include 'mpif.h'
#endif
 
      INTEGER, INTENT( In )  :: JDATE       
      INTEGER, INTENT( In )  :: JTIME  
      CHARACTER(  16 )       :: V_LIST2( 20 )
      CHARACTER( 240 )       :: XMSG   = ''  
      INTEGER                :: NV


      INTEGER          :: STATUS      ! ENV... status
      CHARACTER( 250 ) :: STRTEMP
      CHARACTER( 80 )  :: PBASE     
      INTEGER, PARAMETER :: EXIT_STATUS = 1
      INTEGER          :: ERROR
      INTEGER          :: VARDEV = -1

      LOGICAL          :: FOUND
      INTEGER          :: LOC, STR_LEN
      CHARACTER(10)    :: WRF_VERSION

      CHARACTER( 16 ) :: PNAME = 'RUNTIME_VARS'

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
 
      IF ( MYPE .EQ. 0 ) VARDEV = OUTDEV

      ! Get Simulation Scenario Name to Label Log Files, etc
      CALL GET_ENV( APPL_NAME, 'CTM_APPL', APPL_NAME, VARDEV )
      
      ! Start I/O-API and set up log file(s)
      CALL SETUP_LOGDEV()
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_HEADING( OUTDEV, "Environment Variable Report" )
         CALL LOG_SUBHEADING( OUTDEV, "Grid and High-Level Model Parameters" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

       ! Get Logfile Directory
      CALL GET_ENV ( BLDFOLD, 'BLD', BLDFOLD, VARDEV ) 
 
      ! Get Logfile Directory
      CALL GET_ENV ( OUTDIR, 'OUTDIR', OUTDIR, VARDEV ) 
 
      ! Determine if this run is a new start or a restart
      CALL GET_ENV ( NEW_START, 'NEW_START', NEW_START, VARDEV )
    
      ! Determine if this run is a new start or a restart
      CALL GET_ENV ( ISAM_NEW_START, 'ISAM_NEW_START', ISAM_NEW_START, VARDEV )
    
      ! Get Grid Name
      CALL GET_ENV ( STRTEMP, 'GRID_NAME', GRID_NAME, VARDEV )
      GRID_NAME = STRTEMP(1:16)

      ! Output Time Step
      CALL GET_ENV ( LOCAL_TSTEP, 'CTM_TSTEP', LOCAL_TSTEP, VARDEV )

      ! Run Duration
      CALL GET_ENV ( RUNLEN,  'CTM_RUNLEN', RUNLEN, VARDEV )
      
#ifndef twoway
      ! Main Program Name
      CALL GET_ENV ( STRTEMP, 'CTM_PROGNAME', PROGNAME, VARDEV )
      PROGNAME = STRTEMP( 1:16 )

      ! Starting Date
      CALL GET_ENV ( STDATE, 'CTM_STDATE', STDATE, VARDEV )

      ! Starting Time
      CALL GET_ENV ( STTIME, 'CTM_STTIME', STTIME, VARDEV )

      ! Retrieve the domain decomposition processor array
      IF ( NPROCS .GT. 1 ) THEN
         CALL GET_ENV ( 'NPCOL_NPROW', NV, V_LIST2, VARDEV )
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
      CALL GET_ENV ( MAXSYNC, 'CTM_MAXSYNC', MAXSYNC, VARDEV )

      ! MINSYNC
      CALL GET_ENV ( MINSYNC, 'CTM_MINSYNC', MINSYNC, VARDEV )


      !-------------------------------------------------------------------------------------------------------
      !>> General; Multiprocess control, output and error checking
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Multiprocess control, output and error checking" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! PRINT_PROC_TIME
      CALL GET_ENV ( PRINT_PROC_TIME, 'PRINT_PROC_TIME', PRINT_PROC_TIME, VARDEV )  

      ! FL_ERR_STOP
      CALL GET_ENV ( FL_ERR_STOP, 'FL_ERR_STOP', FL_ERR_STOP, VARDEV )  

      ! CKSUM
      CALL GET_ENV ( CKSUM, 'CTM_CKSUM', CKSUM, VARDEV )

      ! Override default beginning time timestamp for ACONC?
      CALL GET_ENV ( END_TIME, 'AVG_FILE_ENDTIME', END_TIME, VARDEV )
      
      ! ACONC File Species List
      CALL GET_ENV ( 'AVG_CONC_SPCS', N_ACONC_VARS, ACONC_FILE_SPCS, VARDEV )

      ! CONC File Species List 
      CALL GET_ENV ( 'CONC_SPCS', N_CONC_VARS, CONC_FILE_SPCS, VARDEV )
 
      ! ACONC Layer Specification
      CALL GET_ENV ( 'ACONC_BLEV_ELEV', NV, V_LIST2, VARDEV )
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
      CALL GET_ENV ( 'CONC_BLEV_ELEV', NV, V_LIST2, VARDEV )
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

#ifdef isam
      ! SA_ACONC Layer Specification
      CALL GET_ENV ( 'AISAM_BLEV_ELEV', NV, V_LIST2, VARDEV )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         AISAM_BLEV = 1
         AISAM_ELEV = -1
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:AISAM_BLEV_ELEV
         AISAM_BLEV = 1
         READ( V_LIST2( 2 ), '( I4 )' ) AISAM_ELEV
      ELSE IF ( NV .EQ. 2 ) THEN ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) AISAM_BLEV
         READ( V_LIST2( 2 ), '( I4 )' ) AISAM_ELEV
      ELSE
         XMSG = 'Environment variable error for AISAM_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF

      ! SA_CONC File Vertical Layer Range and Speciation
      CALL GET_ENV ( 'ISAM_BLEV_ELEV', NV, V_LIST2, VARDEV )
      IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
         ISAM_BLEV = 1
         ISAM_ELEV = -1
      ELSE IF ( NV .EQ. 1 ) THEN ! Assume 1:ISAM_BLEV_ELEV
         ISAM_BLEV = 1
         READ( V_LIST2( 2 ), '( I4 )' ) ISAM_ELEV
      ELSE IF ( NV .EQ. 2 ) THEN ! Correct Input
         READ( V_LIST2( 1 ), '( I4 )' ) ISAM_BLEV
         READ( V_LIST2( 2 ), '( I4 )' ) ISAM_ELEV
      ELSE
         XMSG = 'Environment variable error for ISAM_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF
#endif
      
      ! PWRTFLAG
      CALL GET_ENV ( PWRTFLAG, 'IOAPI_LOG_WRITE', PWRTFLAG, VARDEV )

      ! Get Flag for Vertical Extraction
      CALL GET_ENV ( LVEXT, 'VERTEXT', LVEXT, VARDEV )
#ifdef parallel
      ! Get Filepath for File Specifying Lon-Lat Coordinates for
      ! Vertical Extraction
      CALL GET_ENV ( VEXT_COORD_PATH, 'VERTEXT_COORD_PATH', VEXT_COORD_PATH, VARDEV )
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
      CALL GET_ENV ( GC_NAMELIST, 'gc_matrix_nml', GC_NAMELIST, VARDEV )

      ! Get Filename for Aerosol Species Namelist
      CALL GET_ENV ( AE_NAMELIST, 'ae_matrix_nml', AE_NAMELIST, VARDEV )

      ! Get Filename for Nonreactive Species Namelist
      CALL GET_ENV ( NR_NAMELIST, 'nr_matrix_nml', NR_NAMELIST, VARDEV )

      ! Get Filename for Tracer Species Namelist
      CALL GET_ENV ( TR_NAMELIST, 'tr_matrix_nml', TR_NAMELIST, VARDEV )

      !-------------------------------------------------------------------------------------------------------
      !>> Chemistry and Photolysis
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Chemistry and Photolysis" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Get photolysis rate diagnostic file flag
      CALL GET_ENV ( PHOTDIAG, 'CTM_PHOTDIAG', PHOTDIAG, VARDEV )

      IF( PHOTDIAG ) THEN
      ! Get desired number of layers for PHOTDIAG2 and PHOTDIAG3 files
         CALL GET_ENV ( NLAYS_DIAG, 'NLAYS_PHOTDIAG', NLAYS_DIAG, VARDEV )

      ! Get Desired Wavelengths for Diagnostic Output
        CALL GET_ENV( 'NWAVE_PHOTDIAG', NWAVE, WAVE_ENV, VARDEV )
      END IF  

      ! Get flag to use core-shell mixing model for aerosol optical properties
      CALL GET_ENV ( CORE_SHELL, 'CORE_SHELL_OPTICS', CORE_SHELL, VARDEV )

      ! Get flag to use fast optics for volume mixing model for aerosol optical properties
      CALL GET_ENV ( MIE_CALC, 'OPTICS_MIE_CALC', MIE_CALC, VARDEV )

      !Absolute Tolerance for SMVGEAR
      CALL GET_ENV ( GEAR_ATOL, 'GEAR_ATOL', GEAR_ATOL, VARDEV )
      
      !Relative Tolerance for SMVGEAR
      CALL GET_ENV ( GEAR_RTOL, 'GEAR_RTOL', GEAR_RTOL, VARDEV )
      
      ! Tolerances for Rosenbrock Solver
      CALL GET_ENV ( GLBL_RTOL, 'RB_RTOL', GLBL_RTOL, VARDEV )

      ! Absolute Tolerance for RB Solver
      CALL GET_ENV ( GLBL_ATOL, 'RB_ATOL', GLBL_ATOL, VARDEV )
 
      !-------------------------------------------------------------------------------------------------------
      !>> Aerosols
      !-------------------------------------------------------------------------------------------------------
      
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Aerosols" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Get aerosol average diagnostic file flag.
      CALL GET_ENV ( AVISDIAG, 'CTM_AVISDIAG', AVISDIAG, VARDEV )

      ! Retrieve the Environment Variable allowing the User to Toggle
      ! the pcVOC emissions
      CALL GET_ENV ( ZERO_PCSOA, 'CTM_ZERO_PCSOA', ZERO_PCSOA, VARDEV )

      ! Get AOD diagnostic file flag.
      CALL GET_ENV ( AOD, 'CTM_AOD', AOD, VARDEV )

      ! Get flag for instantaneous PM diagnostic file.
      CALL GET_ENV ( PMDIAG, 'CTM_PMDIAG', PMDIAG, VARDEV )

      ! Get flag for average PM diagnostic file.
      CALL GET_ENV ( APMDIAG, 'CTM_APMDIAG', APMDIAG, VARDEV )

      ! APMDIAG Layer Specification 
      CALL GET_ENV ( 'APMDIAG_BLEV_ELEV', NV, V_LIST2, VARDEV )
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
      CALL GET_ENV ( 'AVG_PMDIAG_SPCS', N_APMDIAG_SPC, APMDIAG_SPC, VARDEV )

      !-------------------------------------------------------------------------------------------------------
      !>> sulfur tracking option
      !-------------------------------------------------------------------------------------------------------

      ! Flag for sulfur tracking option
      CALL GET_ENV ( STM, 'STM_SO4TRACK', STM, VARDEV )

      IF ( STM ) THEN
         ! Get sulfur tracking normalization flag
         CALL GET_ENV ( ADJ_STMSPC, 'STM_ADJSO4', ADJ_STMSPC, VARDEV )
      END IF      

      !-------------------------------------------------------------------------------------------------------
      !>> Cloud Parameters
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Cloud Processes" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! FLag for outputting cloud diagnostics
      CALL GET_ENV ( CLD_DIAG, 'CLD_DIAG', CLD_DIAG, VARDEV )

      !-------------------------------------------------------------------------------------------------------
      !>> Air-Surface Exchange Parameters
      !-------------------------------------------------------------------------------------------------------
 
      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Air-Surface Exchange Processes" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Check if using PX version of MCIP
      CALL GET_ENV ( PX_LSM, 'PX_VERSION', PX_LSM, VARDEV )

      ! Flag for Ammonia bi-directional flux with in-line deposition 
      ! velocities calculation
      CALL GET_ENV ( ABFLUX, 'CTM_ABFLUX', ABFLUX, VARDEV )

      ! Flag for Mosaic method to get land-use specific deposition velocities
      CALL GET_ENV ( MOSAIC, 'CTM_MOSAIC', MOSAIC, VARDEV )

      ! Flag for HONO interaction with leaf and building surfaces
      CALL GET_ENV ( SFC_HONO, 'CTM_SFC_HONO', SFC_HONO, VARDEV )
         
      ! Mosaic method to get land-use specific stomatal flux
      IF ( MOSAIC ) THEN
         CALL GET_ENV ( FST, 'CTM_FST', FST, VARDEV )
      END IF

      ! Flag CLM LSM
      CALL GET_ENV ( CLM_LSM, 'CLM_VERSION', CLM_LSM, VARDEV )

      ! Flag for NOAH LSM
      CALL GET_ENV ( NOAH_LSM, 'NOAH_VERSION', NOAH_LSM, VARDEV )

      ! Flag for Surface Tiled Aerosol and Gaseous Exchange (STAGE) model
      CALL GET_ENV ( STAGE, 'CTM_STAGE', STAGE, VARDEV )

      ! CTM_DEPV_FILE
      CALL GET_ENV ( DEPV_DIAG, 'CTM_DEPV_FILE', DEPV_DIAG, VARDEV )

      ! CTM_HGBIDI
      CALL GET_ENV ( HGBIDI, 'CTM_HGBIDI', HGBIDI, VARDEV )
 
      ! CTM_IGNORE_FERT_NH3
      CALL GET_ENV ( BIDI_FERT_NH3, 'CTM_BIDI_FERT_NH3', BIDI_FERT_NH3, VARDEV )
 
      ! Flag for using BELD Land Use for WindBlown Dust 
      CALL GET_ENV ( STRTEMP, 'CTM_WBDUST_BELD', DUST_LAND_SCHEME, VARDEV )
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
      CALL GET_ENV ( VDIFFDIAG, 'CTM_VDIFF_DIAG_FILE', VDIFFDIAG, VARDEV )

      ! Get Minimum Layer for Advection Time Step Adjustment
      CALL GET_ENV ( SIGST, 'SIGMA_SYNC_TOP', SIGST, VARDEV )

      ! Get Maximum Horizontal Div Limit for Advection Adjustment
      CALL GET_ENV ( HDIV_LIM, 'ADV_HDIV_LIM', HDIV_LIM, VARDEV )

      ! CFL Criteria
      CALL GET_ENV ( CFL, 'CTM_ADV_CFL', CFL, VARDEV )

      ! CTM_KZMIN
      CALL GET_ENV ( MINKZ, 'KZMIN', MINKZ, VARDEV )

      ! CTM_WVEL
      CALL GET_ENV ( W_VEL, 'CTM_WVEL', W_VEL, VARDEV )

      ! CTM_GRAV_SETL
      CALL GET_ENV ( GRAV_SETL, 'CTM_GRAV_SETL', GRAV_SETL, VARDEV )

      !-------------------------------------------------------------------------------------------------------
      !>> Emission Environment Variables 
      !-------------------------------------------------------------------------------------------------------
      
      IF ( INDEX( MECHNAME, 'CB6R3M' ) .GT. 0 ) then
         USE_MARINE_GAS_EMISSION = .TRUE.
      END IF

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Emissions Parameters" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Get Name of Emission Control File
      CALL GET_ENV ( EMISSCTRL, 'EMISSCTRL_NML', EMISSCTRL, VARDEV )

      ! Number of Layers for Emissions
      CALL GET_ENV ( EMLAYS_MX, 'CTM_EMLAYS', EMLAYS_MX, VARDEV )

      ! Get number of different Gridded File Emissions Streams
      CALL GET_ENV ( N_FILE_GR, 'N_EMIS_GR', N_FILE_GR, VARDEV )
      
      ! Get number of different Gridded File Emissions Streams
      CALL GET_ENV ( N_FILE_TR, 'N_EMIS_TR', N_FILE_TR, VARDEV )

      ! Flag for checking emissions surrogates against species actually
      ! present on emissions files
      CALL GET_ENV (  EMISCHK, 'CTM_EMISCHK', EMISCHK, VARDEV )
 
      ! CTM_BIOGEMIS
      CALL GET_ENV ( BIOGEMIS, 'CTM_BIOGEMIS', BIOGEMIS, VARDEV )

      ! Get the speciation profile to use
      CALL GET_ENV ( SPPRO, 'BIOG_SPRO', SPPRO, VARDEV )
 
      ! Bioseason Switch Flag
      CALL GET_ENV ( BIOGEMIS_SEASON, 'BIOSW_YN', BIOGEMIS_SEASON, VARDEV )

      IF ( .NOT. BIOGEMIS_SEASON ) THEN
         ! Assume_Summer flag
         CALL GET_ENV ( ASSUME_SUMMER, 'SUMMER_YN', ASSUME_SUMMER, VARDEV )
      END IF

      ! Biogenic Emission Diag File
      CALL GET_ENV ( BEMIS_DIAG, 'B3GTS_DIAG', BEMIS_DIAG, VARDEV )
 
      ! Get marine gas emission diagnostic output file flag.
      CALL GET_ENV ( MGEMDIAG, 'CTM_MGEMDIAG', MGEMDIAG, VARDEV )
 
      ! Flag for ocean halogen chemistry and sea spray aerosol emissions
      CALL GET_ENV ( OCEAN_CHEM, 'CTM_OCEAN_CHEM', OCEAN_CHEM, VARDEV )

      ! Flag for Online Calculation of Windblown dust emissions
      CALL GET_ENV ( WB_DUST, 'CTM_WB_DUST', WB_DUST, VARDEV )

      ! Get env var for erodible agriculture land fraction
      !CALL GET_ENV ( ERODE_AGLAND, 'CTM_ERODE_AGLAND', ERODE_AGLAND, VARDEV )

      ! Get env var for diagnostic output
      CALL GET_ENV ( DUSTEM_DIAG, 'CTM_DUSTEM_DIAG', DUSTEM_DIAG, VARDEV )
       
      ! Get sea spray emission diagnostic output file flag.
      CALL GET_ENV ( SSEMDIAG, 'CTM_SSEMDIAG', SSEMDIAG, VARDEV )
 
      ! Set LTNG_NO to Y or T to turn on lightning NO production
      CALL GET_ENV ( LTNG_NO, 'CTM_LTNG_NO', LTNG_NO, VARDEV ) 

      ! Get Lightning Input Time Interval
      CALL GET_ENV ( LT_ASM_DT, 'LTNG_ASSIM_DT', LT_ASM_DT, VARDEV ) 

      ! Get Lightning NO File Name
      CALL GET_ENV ( LTNG_FNAME, 'LTNGNO', LTNG_FNAME, VARDEV )
 
      ! Flag for using NLDN data for Lightning Strikes
      CALL GET_ENV ( NLDNSTRIKE, 'USE_NLDN', NLDNSTRIKE, VARDEV )
 
      ! Flag for Outputing Lightning Diagnostic File
      CALL GET_ENV ( LTNGDIAG, 'LTNGDIAG', LTNGDIAG, VARDEV )
       
      ! Get Lightning NO Production Rate
      CALL GET_ENV ( MOLSNCG, 'MOLSNCG', 350.0, VARDEV )
      
      ! Get Lightning NO Production Rate
      CALL GET_ENV ( MOLSNIC, 'MOLSNIC', 350.0, VARDEV )
 
      ! get number of different file groups (sectors)
      CALL GET_ENV ( NPTGRPS, 'N_EMIS_PT', NPTGRPS, VARDEV )
      
      ! PT3DDIAG
!     CALL GET_ENV ( PT3DDIAG, 'PT3DDIAG', PT3DDIAG, VARDEV )

      ! PT3DFRAC
!     CALL GET_ENV ( PT3DFRAC, 'PT3DFRAC', PT3DFRAC, VARDEV )

      ! Point Source Time Steps
!     CALL GET_ENV ( PT_NSTEPS, 'LAYP_NSTEPS', PT_NSTEPS, VARDEV )

      ! Point Source Date
!     CALL GET_ENV ( PT_DATE, 'LAYP_STDATE', PT_DATE, VARDEV )

      ! Point Source Time
!     CALL GET_ENV ( PT_TIME, 'LAYP_STTIME', PT_TIME, VARDEV )

      ! IPVERT
      CALL GET_ENV ( IPVERT, 'IPVERT', IPVERT, VARDEV )

      ! REP_LAYR
!     CALL GET_ENV ( REP_LAYR, 'REP_LAYER_MIN', REP_LAYR, VARDEV )
 
!     ALLOCATE( PLAY_BASE( NPTGRPS ) )
!     PLAY_BASE( : ) = ''
!     DO NV = 1,NPTGRPS
!        WRITE( PBASE,'( "PLAY_BASE", "_", I2.2 )' ) NV
!        CALL GET_ENV ( STRTEMP, PBASE, PLAY_BASE( NV ), VARDEV )
!        PLAY_BASE( NV ) = STRTEMP
!     END DO
       
      ! Determine user-defined default for emissions diagnostics
      CALL GET_ENV ( EMISDIAG, 'EMISDIAG', EMISDIAG, VARDEV )

      ! Determine user-defined default for emissions date override for representative days
      CALL GET_ENV ( EMIS_SYM_DATE, 'EMIS_SYM_DATE', EMIS_SYM_DATE, VARDEV )
 
      !-------------------------------------------------------------------------------------------------------
      !>> Process Analysis
      !-------------------------------------------------------------------------------------------------------

      IF ( MYPE .EQ. 0 ) THEN 
         CALL LOG_SUBHEADING( OUTDEV, "Process Analysis Parameters" )
         WRITE( OUTDEV, '(6x,A)' ),'--Env Variable-- | --Value--'
         WRITE( OUTDEV, '(6x,A)' ), REPEAT( '-',80 )
      END IF

      ! Flag for Using Process Analysis
      CALL GET_ENV ( PROCAN, 'CTM_PROCAN', PROCAN, VARDEV )
       
      ! Get the Beginning and Ending Columns for Process Analysis
      CALL GET_ENV ( 'PA_BCOL_ECOL', NV, V_LIST2, VARDEV )
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
      CALL GET_ENV ( 'PA_BROW_EROW', NV, V_LIST2, VARDEV )
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
      CALL GET_ENV ( 'PA_BLEV_ELEV', NV, V_LIST2, VARDEV )
      IF ( NV .LE. 0 ) THEN  
         PA_BEGLEV = 1; PA_ENDLEV = 0
      ELSE IF ( NV .EQ. 2 ) THEN
         READ( V_LIST2( 1 ), '( I4 )' ) PA_BEGLEV
         READ( V_LIST2( 2 ), '( I4 )' ) PA_ENDLEV
      ELSE 
         XMSG = 'Environment variable error for PA_BLEV_ELEV'
         CALL M3EXIT( 'INIT_ENV_VARS', JDATE, JTIME, XMSG, EXIT_STATUS )
      END IF

! for MPAS
#ifdef mpas
      call get_env (ncd_64bit_offset, 'ncd_64bit_offset', .false., vardev)
      call get_env( cell_num, 'cell_num', 1, vardev)
#endif

#ifdef twoway
      WRF_V4P = .TRUE.
#else
! to obtain WRF version information
      IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// MET_CRO_3D // ' file'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
         XMSG = 'Could not get ' // MET_CRO_3D //' file description'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ! Get environ. variable met_tstep to control meteorology frequency
      CALL GET_ENV ( MET_TSTEP, 'MET_TSTEP', TSTEP3D, VARDEV)

      ! Ensure users cannot pick a met_tstep that is not smaller than the
      ! file time step and something that is not a multiple of that tstep
          
      IF (MOD(TIME2SEC(MET_TSTEP), TIME2SEC(TSTEP3D)) .NE. 0) then
          XMSG = 'MET_TSTEP environmental variable not equal to or a 
     & multiple greater than time step of met file ' // MET_CRO_3D
          CALL M3EXIT(PNAME, 0, 0, XMSG, XSTAT1)
      end if 

      FOUND = .FALSE.
      NV = 0
      Do WHILE ((.NOT. FOUND) .AND. (NV .LT. MXDESC3))
         NV = NV + 1
         LOC = INDEX (FDESC3D(NV), 'WRF ARW V')
         IF (LOC > 0) THEN
            FOUND = .TRUE.
            STR_LEN = LEN_TRIM(FDESC3D(NV))
            READ (FDESC3D(NV)(LOC+9:STR_LEN), *) WRF_VERSION
            IF (WRF_VERSION .GE. '4.1') THEN
               WRF_V4P = .TRUE.
            END IF
         END IF
      END DO

      XMSG = 'MET data determined based on WRF ARW version '
      IF( MYPE .EQ. 0 ) THEN
          WRITE(OUTDEV,'(/,5X,2(A,1X),/)')TRIM( XMSG ), TRIM( FDESC3D(NV)(LOC+9:STR_LEN) )
          WRITE(LOGDEV,'(/,5X,2(A,1X),/)')TRIM( XMSG ), TRIM( FDESC3D(NV)(LOC+9:STR_LEN) )
      ELSE
          WRITE(LOGDEV,'(/,5X,2(A,1X),/)')TRIM( XMSG ), TRIM( FDESC3D(NV)(LOC+9:STR_LEN) )
      END IF

      IF ( .NOT. CLOSE3( MET_CRO_3D ) ) THEN
        XMSG = 'Could not close ' // MET_CRO_3D
        CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
#endif

      END SUBROUTINE INIT_ENV_VARS

!.........................................................................
      SUBROUTINE LOG_HEADING( FUNIT, CHEAD_IN )

!  Formats and writes a user-supplied heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a heading
!  should probably just be one line.
!.........................................................................

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
          WRITE( XMSG, '(7x,A16)' ),'Master Time Step'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
#ifndef twoway
          IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
#endif
          WRITE( XMSG, '(7x,A24,F6.1,A8)' ),'Processing completed... ',
     &                     (CPU_TIME_FINISH-CPU_TIME_START),' seconds'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, * )

      CASE ( 3 ) 
          ! Write out the time to complete the output procedure
          WRITE( XMSG, '(1x,A32,F4.1,A)' ), '=--> Data Output completed...   ',
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
