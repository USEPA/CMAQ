
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
      MODULE ELMO_INIT_DEFN

      USE ELMO_DATA
      USE RUNTIME_VARS, ONLY : LOGDEV,LOG_MESSAGE,LOG_HEADING,LOG_SUBHEADING
      
      CONTAINS
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_INIT( TSTEP )
      ! This subroutine calls subroutines that read user input and build
      ! the ELMO_TABLE. It then maps output file variables to the ELMO
      ! TABLE and creates the ELMO_LOCAL vector for efficient storage of
      ! ELMO results, with maps to the calculation IDs and output file
      ! vectors.
!-------------------------------------------------------------------------
   
      USE ELMO_DERIVED_DEFN, ONLY : ELMO_MAP_DERIVED
      USE GRID_CONF, ONLY : NCOLS, NROWS
      USE ELMO_COMPOSITES, ONLY : ELMO_MAP_COMPOSITES, ELMO_READ_COMPOSITES
      USE AERO_DATA, ONLY : N_MODE
#ifdef isam
      USE SA_DEFN, ONLY : NTAG_SA
#endif
#ifdef sens
      USE DDM3D_DEFN, ONLY : NPMAX
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: TSTEP
      INTEGER N, M, P, J, IM, ILIST, S, F, NLAYS, N_VARS
      INTEGER ID, IV, ISRC, IT, IS, NM, NS, IVAR, IFL
      INTEGER N1, N2, N3, MX_SRC
      CHARACTER( 16 ), SAVE :: PNAME = 'ELMO_INIT'
      CHARACTER( 500 ) :: XMSG
      INTEGER ALLOCSTAT, IOS, IERROR
      INTEGER :: N_MAP_MODE_SOURCE, N_MAP_MODE, N_MAP_SOURCE

      CALL LOG_MESSAGE( LOGDEV,'' )
      CALL LOG_HEADING( LOGDEV, 'Initialize ELMO Module' )

      ! Read User-Input from Namelist input file (CMAQ Control File)
      CALL READ_ELMO_NML(TSTEP)
      IF ( .NOT. L_ELMO ) RETURN
      
      ! Initialize Shared Output Variables like AOD, Extinction, and
      ! other Global Variables
      CALL ELMO_INIT_SHARED()

      ! Read Mechanism-Specific Composite Variables so they may be added
      ! to the ELMO Table next
      CALL LOG_SUBHEADING( LOGDEV, 'Reading and Mapping ELMO variables' )
      CALL ELMO_READ_COMPOSITES()

      ! Initialize ELMO Table for all possible output variables
      CALL LOG_MESSAGE( LOGDEV, 'Building ELMO Table' )
      CALL BUILD_ELMO_TABLE()

      ! Map Special Indices for ELMO Derived Variable components
      CALL LOG_MESSAGE( LOGDEV, 'Mapping ELMO Derived Variable Components' )
      CALL ELMO_MAP_DERIVED()

      ! Map All Table Variables to their components, if applicable
      CALL LOG_MESSAGE( LOGDEV, 'Map ELMO Composite Variables' )
      CALL LOG_MESSAGE( LOGDEV, 'Mapping ELMO Composites' )
      CALL ELMO_MAP_COMPOSITES()

      ! ELMO FILE INITIALIZATION
      ! Determine which ELMO_TABLE variables belong on each output file
      ALLOCATE( L_ELMO_TABLE_USED( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_ELMO_TABLE_USED',PNAME ) 
      L_ELMO_TABLE_USED(:) = .FALSE.

      ALLOCATE( L_ELMO_TABLE_OUT( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_ELMO_TABLE_OUT',PNAME ) 

      ALLOCATE( ELMO_OUT( N_ELMO_FILES ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_OUT',PNAME ) 

      L_ELMO_DDEP = .FALSE.
      L_ELMO_WDEP = .FALSE.

      DO IFL = 1,N_ELMO_FILES
         L_ELMO_TABLE_OUT = .FALSE.
         CALL MAP_ELMO_NML( IFL, L_ELMO_TABLE_OUT, IERROR )
         IF ( IERROR .GT. 0 ) THEN
            ! Entry Doesn't Match any variable on the ELMO table. Crash the model.
            XMSG = 'An ELMO variable has been requested that does not exist.'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
         
         ! Identify which ELMO_TABLE variables are Used and
         ! allocate/map ELMO_USED vector
         L_ELMO_TABLE_USED(:) = L_ELMO_TABLE_USED .OR. L_ELMO_TABLE_OUT

         ! Populate ELMO FILE Mapping Indices
         ELMO_FILE(IFL)%N_VARS = COUNT( L_ELMO_TABLE_OUT(:) )
         N_VARS = ELMO_FILE(IFL)%N_VARS
         ALLOCATE( ELMO_FILE(IFL)%TABID( N_VARS ),
     &             ELMO_FILE(IFL)%USEDID( N_VARS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ELMO_FILE IDs',PNAME ) 
         ELMO_FILE( IFL )%TABID = 0
         ELMO_FILE( IFL )%USEDID = 0

         N = 0
         DO J = 1,N_ELMO_TABLE
             IF ( L_ELMO_TABLE_OUT( J ) ) THEN 
                 N = N + 1
                 ELMO_FILE( IFL )%TABID( N ) = J
         
                 ! Check that all components are defined
                 IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ET_CMPST .OR.
     &                ELMO_TABLE( J )%VAR_TYPE .EQ. ET_CMPST_DEP .OR.
     &                ELMO_TABLE( J )%VAR_TYPE .EQ. ET_ISAM_CMPST .OR.
     &                ELMO_TABLE( J )%VAR_TYPE .EQ. ET_ISAM_CMPST_DEP .OR.
     &                ELMO_TABLE( J )%VAR_TYPE .EQ. ET_DDM_CMPST .OR.
     &                ELMO_TABLE( J )%VAR_TYPE .EQ. ET_DDM_CMPST_DEP ) THEN
                    DO IVAR = 1,ELMO_COMP( J )%N_VARS
                       IF ( ELMO_COMP( J )%TABID( IVAR ) .EQ. 0 ) THEN
                          WRITE( XMSG, '(3A,I2,A,I2,6A)' ) 'The ELMO COMPOSITE variable ',
     &                         TRIM( ELMO_TABLE( J )%NAME ),' requested on file ',
     &                         IFL,' has a component (#',
     &                         IVAR,') that is not available on the comprehensive ', 
     &                         'ELMO Table. If this is a source-resolved ISAM COMPOSITE, ',
     &                         'then it is recommended to check that the components ',
     &                         'of this COMPOSITE are activated in the ISAM control ', 
     &                         'file. If this is not an ISAM simulation, check to ', 
     &                         'make sure all components are defined.'
                          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
                       END IF
                    END DO
                 END IF
             END IF
         END DO
         
         ! Allocate space for this file's data array
         NLAYS = ELMO_FILE( IFL )%NLAYS
         N_VARS = ELMO_FILE( IFL )%N_VARS
         ALLOCATE( ELMO_OUT( IFL )%DATA( 
     &               NCOLS,NROWS,NLAYS,N_VARS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ELMO_OUT', PNAME )
         ELMO_OUT( IFL )%DATA(:,:,:,:) = 0.0
      END DO

      LAY_TOP_MAX = MAXVAL( ELMO_FILE( : )%LAY_TOP )
      LAY_BOT_MIN = MINVAL( ELMO_FILE( : )%LAY_BOT )

      N_ELMO_USED = COUNT( L_ELMO_TABLE_USED )
      ALLOCATE( ELMO_USED( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_USED',PNAME ) 
      ELMO_USED = 0.0
      ALLOCATE( MAP_USEDtoTABLE( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MAP_USEDtoTABLE',PNAME ) 
      MAP_USEDtoTABLE = 0
      ALLOCATE( MAP_TABLEtoUSED( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MAP_TABLEtoUSED',PNAME ) 
      MAP_TABLEtoUSED = 0

      N = 0
      DO J = 1,N_ELMO_TABLE
          IF ( L_ELMO_TABLE_USED( J ) ) THEN
              N = N + 1
              MAP_TABLEtoUSED( J ) = N
              MAP_USEDtoTABLE( N ) = J

              DO IFL = 1,N_ELMO_FILES
                DO IVAR = 1,ELMO_FILE( IFL )%N_VARS
                  IF ( ELMO_FILE( IFL )%TABID( IVAR ) .EQ. J ) 
     &               ELMO_FILE( IFL )%USEDID( IVAR ) = N 
                END DO
              END DO
          END IF    
      END DO

      ! Allocate array for tracking whether or not a variable has been
      ! calculated each time step.
      ALLOCATE( L_CALCULATED( N_ELMO_USED ), 
     &          ELMO_USED_UNIT( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_CALCULATED',PNAME )
      L_CALCULATED = .FALSE.
      ELMO_USED_UNIT = 0


      ! Write Output Variable Information to Log File
      CALL ELMO_WRITE_LOG()

      END SUBROUTINE ELMO_INIT
 
!-------------------------------------------------------------------------
      SUBROUTINE READ_ELMO_NML( TSTEP )
!     This subroutine maps the ELMO variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      
      USE RUNTIME_VARS, ONLY: CMAQ_CTRL, APPL_NAME, OUTDIR
      
      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: TSTEP
      INTEGER               :: FUNIT
      INTEGER               :: STAT

      INTEGER IVAR, N_MX_VARS, N_FILES, N_MAX_OUTPUT_VARIABLES,
     &        N_KEYWORDS, N_MAX_KEYWORD_VARIABLES, N_MX_KEYVARS
      CHARACTER(30), ALLOCATABLE :: FLABEL( : )
      CHARACTER(16), ALLOCATABLE :: FTYPE( : )
      INTEGER, ALLOCATABLE       :: FTSTEP( : )
      CHARACTER(10), ALLOCATABLE :: TMODE( : )
      INTEGER, ALLOCATABLE       :: LAY_BOT( : )
      INTEGER, ALLOCATABLE       :: LAY_TOP( : )
      CHARACTER(32), ALLOCATABLE :: FILE_VARS(:,:)
      CHARACTER(32), ALLOCATABLE :: KEYWD_NAME( : )
      CHARACTER(32), ALLOCATABLE :: KEYWD(:,:)
                       
      CHARACTER( 200 ) :: SUFFIX = '.nc'
      CHARACTER( 16 ), SAVE :: PNAME = 'READ_ELMO_NML'
      CHARACTER( 200 )   :: XMSG
      CHARACTER( 200 )   :: TMPLINE
      INTEGER IOS, IFL, N_VARS, N, IKEY
      CHARACTER( 5 )     :: CFIL

      NAMELIST / ELMO_INIT  / N_Files, N_Max_Output_Variables,
     &                        N_Keywords, N_Max_Keyword_Variables
      NAMELIST / ELMO_FILES / Flabel, Tmode, 
     &                       Lay_Bot, Lay_Top, File_Vars,
     &                       Keywd_name, Keywd

      L_ELMO = .TRUE.

      CALL LOG_SUBHEADING( LOGDEV, 'Reading ELMO File Specifications '//
     &                             'from CMAQ Control Namelist')
 
      ! Set the standard suffix for all ELMO Output Files
      IF ( APPL_NAME(1:8 ) .NE. 'CTM_APPL' ) SUFFIX = '_' // TRIM(APPL_NAME) // SUFFIX
        
      ! Retrieve the Name of the CMAQ Control File
      IF ( CMAQ_CTRL .EQ. "CMAQ_CTRL_NML" ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)")
     &           'You have chosen not to indicate the location of a',
     &           'CMAQ_Control.nml namelist file. No ELMO output files ',
     &           'will be created.'
          N_ELMO_FILES = 0
          L_ELMO = .FALSE.
          RETURN
      END IF
 
      ! Open Mechanism-Specific Control File
      FUNIT = JUNIT()
      OPEN( FILE = CMAQ_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ) 'ERROR: Could not read ',
     &           'CMAQ Control namelist file: ',TRIM( CMAQ_CTRL )
          CALL M3EXIT( PNAME,0,0,XMSG,1 )
      END IF

      ! Read the number of ELMO Composite Variables
      N_FILES = 0
      N_MAX_OUTPUT_VARIABLES = 40
      N_KEYWORDS = 0
      N_MAX_KEYWORD_VARIABLES = 40

      REWIND( FUNIT )
      READ( NML = ELMO_INIT, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          ! The Number of Files was completely missing
          XMSG = 'WARNING: Number of ELMO Files was not specified. '//
     &           'If you intended to specify composite variables for ELMO, '//
     &           'check the CMAQ Control Namelist file. CMAQ will not '//
     &           'write any ELMO output files.'
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          N_ELMO_FILES = 0
          N_KEY = 0
          L_ELMO = .FALSE.
          RETURN

      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error detected for ELMO_INIT
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the number of '//
     &           'ELMO Files. Please '//
     &           'check the format of each line for syntax errors. The '//
     &           'invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( PNAME, 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the specification.', 1 )
      END IF

      ! If there are no files, then just return
      N_ELMO_FILES = N_FILES
      N_MX_VARS = N_MAX_OUTPUT_VARIABLES
      N_KEY = N_KEYWORDS
      N_MX_KEYVARS = N_MAX_KEYWORD_VARIABLES
      IF ( N_ELMO_FILES .LE. 0 ) THEN
          L_ELMO = .FALSE.
          RETURN
      END IF

      ! Allocate and Initialize Namelist Variables
      ALLOCATE( FLABEL( N_FILES ), FTYPE( N_FILES ), FTSTEP( N_FILES ),
     &          TMODE( N_FILES ), LAY_BOT( N_FILES ),
     &          LAY_TOP( N_FILES ), FILE_VARS( N_FILES, N_MX_VARS ),
     &          KEYWD_NAME( N_KEY ), KEYWD( N_KEY,N_MX_KEYVARS ),
     &          STAT=STAT )
      CALL CHECKMEM( STAT, 'ELMO_FILE_NML',PNAME)
      FILE_VARS = ''
      KEYWD = ''
      FTSTEP(:) = TSTEP

      ! Read User-Specified ELMO file data from input namelist
      REWIND( FUNIT )
      READ( NML = ELMO_FILES, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          ! Read Error detected for ELMO_INIT
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the  '//
     &           'ELMO File Data. Please check the format of each '//
     &           'line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( PNAME, 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the specification.', 1 ) 
      END IF

      ! Populate ELMO_FILE Structure
      ALLOCATE( ELMO_FILE( N_ELMO_FILES ), STAT=STAT )
      CALL CHECKMEM( STAT, 'ELMO_FILE',PNAME)

      DO IFL = 1,N_ELMO_FILES
        ! Type of Grid
        FTYPE(:) = 'GRIDDED'
        CALL UPCASE( FTYPE( IFL ) )
        IF ( FTYPE( IFL ) .EQ. 'GRIDDED' ) THEN
            ELMO_FILE( IFL )%STRUCT     = 0
        ELSE IF ( FTYPE( IFL ) .EQ. 'UNSTRUCTURED' ) THEN
            ELMO_FILE( IFL )%STRUCT     = 1
        ELSE
            WRITE( XMSG, "(A,I3.3,A)")
     &             'An ELMO file may only be gridded or unstructured.  '//
     &             'Please revise the value of Ftype(',IFL,') in ELMO_FILE.'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, 1 ) 
        END IF

        ! Set ELMO Output File Name with Path and File Logical      
        WRITE( CFIL, '(I0)' ) IFL
        ELMO_FILE( IFL )%SHORTNAME = 'ELMO' // TRIM(CFIL) // '_' //
     &                              TRIM( FLABEL(IFL) ) 
        ELMO_FILE( IFL )%FILENAME = 'CCTM_ELMO' // TRIM(CFIL) // '_' //
     &                              TRIM( FLABEL(IFL) ) // TRIM( SUFFIX )
        WRITE( ELMO_FILE( IFL )%FLOGICAL, '( "ELMO_",I3.3 )' ) IFL
        IF ( OUTDIR .NE. '' ) THEN
            ELMO_FILE( IFL )%FILENAME = TRIM( OUTDIR ) // '/' // 
     &                                  TRIM( ELMO_FILE( IFL )%FILENAME )
        END IF
#ifndef mpas
        IF ( .NOT. SETENVVAR( ELMO_FILE( IFL )%FLOGICAL, ELMO_FILE( IFL )%FILENAME ) ) THEN
            WRITE( XMSG, '(A,I3)' ) 'ERROR: Could not set environment variable for ELMO file # ', IFL
            CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
        END IF
#endif

        ! Output Time Step
        ELMO_FILE( IFL )%TSTEP = FTSTEP( IFL )
        CALL UPCASE( TMODE( IFL ) )
        IF ( TMODE( IFL )(1:4) .EQ. 'INST' ) THEN
           ELMO_FILE( IFL )%TMODE = 0
        ELSE IF ( TMODE( IFL )(1:3) .EQ. 'AGG' ) THEN
           ELMO_FILE( IFL )%TMODE = 1
        ELSE IF ( TMODE( IFL )(1:3) .EQ. 'AVG' ) THEN
           ELMO_FILE( IFL )%TMODE = 1
        ELSE IF ( TMODE( IFL )(1:3) .EQ. 'AVE' ) THEN
           ELMO_FILE( IFL )%TMODE = 1
        ELSE
            WRITE( XMSG, "(A,I3.3,A)")
     &             'An ELMO file may only be aggregate or instantaneous.  '//
     &             'Please revise the value of Tmode(',IFL,') in ELMO_FILE.'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, 1 ) 
        END IF

        ! Layers
        CALL ELMO_CHECK_LAYERS( LAY_TOP( IFL ), LAY_BOT( IFL ), IFL )
        ELMO_FILE( IFL )%LAY_BOT = LAY_BOT( IFL )
        ELMO_FILE( IFL )%LAY_TOP = LAY_TOP( IFL )
        ELMO_FILE( IFL )%NLAYS   = LAY_TOP( IFL ) - LAY_BOT( IFL ) + 1

        ! Allocate Field for Input Variables
        ELMO_FILE( IFL )%N_VARS     = 0 
        ELMO_FILE( IFL )%N_VARS_IN = N_MX_VARS
        ALLOCATE( ELMO_FILE( IFL )%VARS_IN( N_MX_VARS ), STAT = IOS )
        CALL CHECKMEM( IOS, 'ELMO_FILE%VARS_IN',PNAME ) 
        ELMO_FILE( IFL )%VARS_IN(1:N_MX_VARS) = 
     &       FILE_VARS( IFL,1:N_MX_VARS )

      END DO
      
      ! Populate KeyWords and Contents
      ALLOCATE( ELMO_KEY( N_KEY ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY',PNAME )

      DO IKEY = 1,N_KEY
         ELMO_KEY( IKEY )%WORD = KEYWD_NAME( IKEY )

         N = 0
         DO IVAR = 1,N_MX_KEYVARS
            IF ( KEYWD( IKEY,IVAR ) .NE. '' ) N = N + 1
         END DO

         ELMO_KEY( IKEY )%N = N
         ALLOCATE( ELMO_KEY( IKEY )%LIST( N ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ELMO_KEY%LIST',PNAME )
         DO IVAR = 1,N
            ELMO_KEY( IKEY )%LIST( IVAR ) = KEYWD( IKEY,IVAR )
         END DO
      END DO

      END SUBROUTINE READ_ELMO_NML

!-------------------------------------------------------------------------
      SUBROUTINE BUILD_ELMO_TABLE()
!     This subroutine creates the main table of all possible output
!     variables including ELMO variables, bulk aerosol species, CGRID
!     species, and chemical families
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, AEROMODE, N_AEROSPC, AEROSPC,
     &                     AEROSPC_MW
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME, CGRID_MW,
     &                       CGRID_UNITS, CGRID_CONC_DESC,
     &                       CGRID_MASK_NUM, CGRID_MASK_SRF,
     &                       CGRID_MASK_AERO,
     &                       WDEP_SPC, N_SPC_WDEP, MAP_WDEPtoCGRID,
     &                       MAP_DDEPtoCGRID, N_SPC_DDEP
      USE VDIFF_MAP, ONLY : DDEP_SPC
      USE ELMO_COMPOSITES, ONLY : N_COMPOSITES, CMPST
      USE GRID_CONF, ONLY : MYPE
      USE UTILIO_DEFN

#ifdef isam
      USE SA_DEFN, ONLY : TAGNAME, NSPC_SA, NTAG_SA, ISAM_SPEC,
     &                    MAP_SAtoCGR, MAP_AEROtoSA
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY : NPMAX, SEN_PAR, N_ASENS_VARS
#endif

      IMPLICIT NONE

      INTEGER       :: ILIST, IUNIT, IFAM, S, IM, ICGRID,
     &                 IWDEP, IDDEP, ICMPST, IMODE, ISPC
      INTEGER       :: VAR_TYPE
      INTEGER       :: N, IAERO, IOS, ITAG, ISPEC, NP, INDX,
     &                 N_TABLE
      CHARACTER(32) :: PNAME = 'BUILD_ELMO_TABLE'
      CHARACTER(200):: XMSG

      N = 0
      N_TABLE = 2000

#ifdef isam 
      N_TABLE = N_TABLE * NTAG_SA
#elif defined( sens )
      N_TABLE = N_TABLE * ( NPMAX + 1 )
#endif
      ALLOCATE( ELMO_TABLE( N_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_TABLE',PNAME ) 

      ! Initialize Mode and Source Attributes
      ELMO_TABLE(:)%IM = 0
      ELMO_TABLE(:)%SOURCE = 0

      ! Load the ELMO Derived Variables into the ELMO_Table
      DO ILIST = 1,N_ELMO_LIST
         N = N + 1
         ELMO_TABLE( N )%NAME = ELMO_LIST( ILIST )%NAME
         ELMO_TABLE( N )%OUT_NAME = ELMO_LIST( ILIST )%NAME
         ELMO_TABLE( N )%OUT_TYPE = M3REAL
         ELMO_TABLE( N )%VAR_TYPE = ELMO_LIST( ILIST )%VAR_TYPE
         ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
         ELMO_TABLE( N )%CUNIT = ELMO_LIST( ILIST )%UNIT
         CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                        ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
         ELMO_TABLE( N )%IUNIT = IUNIT
         ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC
         ELMO_TABLE( N )%MW   = 0.0
            
         IF ( ELMO_LIST( ILIST )%L_MODE ) THEN
           ! Set Mode-Dependent Properties for this variables first mode
           ELMO_TABLE( N )%IM = 1
           ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST)%NAME)//'_'//aeromode( 1 )%suff 
           ELMO_TABLE( N )%DESC = TRIM(ELMO_LIST( ILIST )%DESC) // ' for mode ' //
     &                            aeromode(1)%suff
           
           ! Define ELMO Variables for all remaining modes
           DO IM = 2,N_MODE
             N = N + 1
             ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
             ELMO_TABLE( N )%IM = IM
             ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME)//
     &                                      '_' // aeromode( IM )%suff
             ELMO_TABLE( N )%DESC = TRIM(ELMO_LIST( ILIST )%DESC) // ' for mode ' //
     &                              aeromode(IM)%suff
           END DO
         END IF
      END DO

#ifdef isam                     
      ! Load the source-resolvable Derived variables into the ELMO Table
      ! to enable ISAM output.
      DO ILIST = 1,N_ELMO_LIST
        IF ( ELMO_LIST( ILIST )%VAR_TYPE .EQ. ET_DRVD .OR.
     &       ELMO_LIST( ILIST )%VAR_TYPE .EQ. ET_DRVD_DDEP .OR.
     &       ELMO_LIST( ILIST )%VAR_TYPE .EQ. ET_DRVD_WDEP ) THEN
           ! Define ELMO Variables for all ISAM sources
           DO ITAG = 1,NTAG_SA
             N = N + 1
             ELMO_TABLE( N )%NAME = TRIM(ELMO_LIST( ILIST )%NAME)
     &                              // '_ISAM'
             ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME) 
     &                              // '_' // TRIM(TAGNAME( ITAG ))
             ELMO_TABLE( N )%OUT_TYPE = M3REAL
             ELMO_TABLE( N )%VAR_TYPE = ELMO_LIST( ILIST )%VAR_TYPE + 10
             ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
             ELMO_TABLE( N )%CUNIT =  ELMO_LIST( ILIST )%UNIT
             CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                            ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
             ELMO_TABLE( N )%IUNIT = IUNIT

             ELMO_TABLE( N )%SOURCE = ITAG
             ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC // 
     &             ' for source ' // TRIM(TAGNAME( ITAG ))
             ELMO_TABLE( N )%MW   = 0.0
           END DO 
           
           ! Set Source-Dependent Properties for the sum of sources
           N = N + 1
           ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
           ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME) // '_TAGS'
           ELMO_TABLE( N )%SOURCE = NTAG_SA + 1
           ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC // 
     &           ' for the sum of all sources.'
        END IF
      END DO
#endif         

#ifdef sens
      ! Load the source-resolvable Derived variables into the ELMO Table
      ! to enable ISAM output.
      DO ILIST = 1,N_ELMO_LIST
        IF ( ELMO_LIST( ILIST )%VAR_TYPE .EQ. ET_DRVD .OR.
     &       ELMO_LIST( ILIST )%VAR_TYPE .EQ. ET_DRVD_DDEP .OR.
     &       ELMO_LIST( ILIST )%VAR_TYPE .EQ. ET_DRVD_WDEP ) THEN
           
           ! Define ELMO Variables for all remaining DDM3D sources
           DO NP = 1,NPMAX
             N = N + 1
             ELMO_TABLE( N )%NAME = TRIM(ELMO_LIST( ILIST )%NAME)
     &                              // '_DDM'
             ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME)
     &                              // '_' // TRIM( SEN_PAR( NP ) )
             ELMO_TABLE( N )%OUT_TYPE = M3REAL
             ELMO_TABLE( N )%VAR_TYPE = ELMO_LIST( ILIST )%VAR_TYPE + 20
             ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
             ELMO_TABLE( N )%SOURCE = NP
             ELMO_TABLE( N )%CUNIT =  ELMO_LIST( ILIST )%UNIT
             CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                            ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
             ELMO_TABLE( N )%IUNIT = IUNIT
             ELMO_TABLE( N )%DESC = 'Sensitivity of ' // ELMO_LIST( ILIST )%DESC // 
     &             ' to ' // TRIM( SEN_PAR( NP ) )
             ELMO_TABLE( N )%MW   = 0.0
           END DO
         END IF
      END DO
#endif         
 
      ! Load CGRID Species into ELMO_Table
      DO ICGRID = 1,N_CGRID_SPC
         IF ( CGRID_NAME( ICGRID ) .NE. 'RHOJ' ) THEN
            N = N + 1
            ELMO_TABLE( N )%NAME = CGRID_NAME( ICGRID )
            ELMO_TABLE( N )%OUT_NAME = CGRID_NAME( ICGRID )
            ELMO_TABLE( N )%OUT_TYPE = M3REAL
            ELMO_TABLE( N )%VAR_TYPE = ET_CONC
            ELMO_TABLE( N )%ID = ICGRID
           
            DO IMODE = 1,N_MODE
              DO ISPC = 1,N_AEROSPC
                IF ( CGRID_NAME( ICGRID ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &             ELMO_TABLE( N )%IM = IMODE
              END DO
            END DO
           
            ELMO_TABLE( N )%CUNIT = CGRID_UNITS( ICGRID )
            CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                           ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
            ELMO_TABLE( N )%IUNIT = IUNIT
            ELMO_TABLE( N )%DESC = CGRID_CONC_DESC( ICGRID )
            ELMO_TABLE( N )%MW   = CGRID_MW( ICGRID )
         END IF
      END DO    

      ! Load Deposition Variables into ELMO_Table
      ! Load Wet Deposition Fluxes
      DO IWDEP = 1,N_SPC_WDEP
        N = N + 1
        ELMO_TABLE( N )%NAME = 'WD_' // WDEP_SPC( IWDEP )
        ELMO_TABLE( N )%OUT_NAME = 'WD_' // WDEP_SPC( IWDEP )
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ET_WDEP

        DO IMODE = 1,N_MODE
          DO ISPC = 1,N_AEROSPC
            IF ( WDEP_SPC( IWDEP ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &         ELMO_TABLE( N )%IM = IMODE
          END DO
        END DO

        ICGRID = MAP_WDEPtoCGRID( IWDEP )
        ELMO_TABLE( N )%ID = ICGRID
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'Wet Deposition for Species '//
     &                         TRIM( WDEP_SPC( IWDEP ) )
      END DO

      ! Load Dry Deposition Fluxes
      DO IDDEP = 1,N_SPC_DDEP
        N = N + 1
        ELMO_TABLE( N )%NAME = 'DD_' // DDEP_SPC( IDDEP )
        ELMO_TABLE( N )%OUT_NAME = 'DD_' // DDEP_SPC( IDDEP )
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ET_DDEP

        DO IMODE = 1,N_MODE
          DO ISPC = 1,N_AEROSPC
            IF ( DDEP_SPC( IDDEP ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &         ELMO_TABLE( N )%IM = IMODE
          END DO
        END DO

        ICGRID = MAP_DDEPtoCGRID( IDDEP )
        ELMO_TABLE( N )%ID = ICGRID
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'Dry Deposition Flux for Species '//
     &                         TRIM( DDEP_SPC( IDDEP ) )
      END DO
 
#ifdef isam
      ! Load ISAM CGRID Variables into ELMO Table
      DO ISPEC = 1,NSPC_SA
        DO ITAG = 1,NTAG_SA
           N = N + 1
           ELMO_TABLE( N )%NAME = TRIM(ISAM_SPEC( ISPEC,ITAG ))
     &                            // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = TRIM(ISAM_SPEC( ISPEC,ITAG ))
     &                            // '_' // TRIM(TAGNAME( ITAG )) 
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ET_ISAM_CONC
           ELMO_TABLE( N )%ID = ISPEC
           ELMO_TABLE( N )%SOURCE = ITAG
           ICGRID = MAP_SAtoCGR( ISPEC )
           IF ( CGRID_MASK_AERO( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'ug m-3'
           ELSE
              ELMO_TABLE( N )%CUNIT = 'ppmV'
           END IF
        
           DO IMODE = 1,N_MODE
             DO ISPC = 1,N_AEROSPC
               IF ( ISAM_SPEC( ISPEC,ITAG ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &            ELMO_TABLE( N )%IM = IMODE
             END DO
           END DO

           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'ISAM Concentration of Species '//
     &                     TRIM( ISAM_SPEC( ISPEC,ITAG ) ) //
     &                     ' from source ' // TRIM(TAGNAME( ITAG ))
           ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        END DO

        N = N + 1
        ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
        ELMO_TABLE( N )%OUT_NAME = TRIM(ISAM_SPEC( ISPEC,1 )) //'_TAGS'
        ELMO_TABLE( N )%SOURCE = NTAG_SA + 1
        ELMO_TABLE( N )%DESC = 'ISAM Concentration of Species '//
     &                  TRIM( ISAM_SPEC( ISPEC,1 ) ) //
     &                  ' from the sum of sources.'
      END DO


      ! Load ISAM Wet Deposition Fluxes into ELMO Table
      DO IWDEP = 1,N_SPC_WDEP
        DO ISPEC = 1,NSPC_SA
          IF ( ISAM_SPEC( ISPEC,1 ) .EQ. WDEP_SPC( IWDEP ) ) THEN
          DO ITAG = 1,NTAG_SA
             N = N + 1
             ELMO_TABLE( N )%NAME = 'WD_' //TRIM( WDEP_SPC( IWDEP ) )
     &                              // '_ISAM'
             ELMO_TABLE( N )%OUT_NAME = 'WD_' // TRIM( WDEP_SPC( IWDEP ) )
     &                              // '_' // TRIM(TAGNAME( ITAG )) 
             ELMO_TABLE( N )%OUT_TYPE = M3REAL
             ELMO_TABLE( N )%VAR_TYPE = ET_ISAM_WDEP
             ELMO_TABLE( N )%ID = ISPEC
             ELMO_TABLE( N )%SOURCE = ITAG
             ICGRID = MAP_WDEPtoCGRID( IWDEP )
             IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
                ELMO_TABLE( N )%CUNIT = 'ha-1'
             ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
                ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
             ELSE
                ELMO_TABLE( N )%CUNIT = 'kg ha-1'
             END IF
             
             DO IMODE = 1,N_MODE
               DO ISPC = 1,N_AEROSPC
                 IF ( WDEP_SPC( IWDEP ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &              ELMO_TABLE( N )%IM = IMODE
               END DO
             END DO
          
             CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                            ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
             ELMO_TABLE( N )%IUNIT = IUNIT
             ELMO_TABLE( N )%DESC = 'Tagged Wet Deposition Flux of '//
     &          TRIM( WDEP_SPC( IWDEP ) ) //' from '// TRIM( TAGNAME( ITAG ) ) 
             ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
          END DO 
          
          N = N + 1
          ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
          ELMO_TABLE( N )%OUT_NAME = 'WD_' // TRIM( WDEP_SPC( IWDEP ) ) // '_TAGS'
          ELMO_TABLE( N )%SOURCE = NTAG_SA + 1
          ELMO_TABLE( N )%DESC = 'Tagged Wet Deposition Flux of '//
     &       TRIM( WDEP_SPC( IWDEP ) ) //' from all sources.'
          END IF
        END DO
      END DO

      ! Load ISAM Dry Deposition Fluxes into ELMO Table
      DO IDDEP = 1,N_SPC_DDEP
        DO ISPEC = 1,NSPC_SA 
          IF ( ISAM_SPEC( ISPEC,1 ) .EQ. DDEP_SPC( IDDEP ) ) THEN
          DO ITAG = 1,NTAG_SA
             N = N + 1
             ELMO_TABLE( N )%NAME = 'DD_' //TRIM(DDEP_SPC( IDDEP ))
     &                              // '_ISAM'
             ELMO_TABLE( N )%OUT_NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))
     &                              // '_' // TRIM(TAGNAME( ITAG )) 
             ELMO_TABLE( N )%OUT_TYPE = M3REAL
             ELMO_TABLE( N )%VAR_TYPE = ET_ISAM_DDEP
             ELMO_TABLE( N )%ID = ISPEC
             ELMO_TABLE( N )%SOURCE = ITAG
             ICGRID = MAP_DDEPtoCGRID( IDDEP )
             IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
                ELMO_TABLE( N )%CUNIT = 'ha-1'
             ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
                ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
             ELSE
                ELMO_TABLE( N )%CUNIT = 'kg ha-1'
             END IF
           
             DO IMODE = 1,N_MODE
               DO ISPC = 1,N_AEROSPC
                 IF ( DDEP_SPC( IDDEP ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &              ELMO_TABLE( N )%IM = IMODE
               END DO
             END DO

             CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                            ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
             ELMO_TABLE( N )%IUNIT = IUNIT
             ELMO_TABLE( N )%DESC = 'Tagged Dry Deposition Flux of '//
     &          TRIM( DDEP_SPC( IDDEP ) ) //' from '//
     &          TRIM( TAGNAME( ITAG ) ) 
             ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
          END DO

          N = N + 1
          ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
          ELMO_TABLE( N )%OUT_NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))//'_TAGS'
          ELMO_TABLE( N )%SOURCE = NTAG_SA + 1
          ELMO_TABLE( N )%DESC = 'Tagged Dry Deposition Flux of '//
     &       TRIM( DDEP_SPC( IDDEP ) ) //' from all sources.'

          END IF
        END DO
      END DO
#endif 

#ifdef sens
      ! Load DDM Variables into ELMO Table
      DO ICGRID = 1,N_CGRID_SPC
        IF ( CGRID_NAME( ICGRID ) .NE. 'RHOJ' ) THEN
           DO NP = 1,NPMAX
              N = N + 1
              ELMO_TABLE( N )%NAME = TRIM( CGRID_NAME(ICGRID) ) 
     &                                   // '_DDM'
              ELMO_TABLE( N )%OUT_NAME = TRIM( CGRID_NAME(ICGRID) ) 
     &                                   // '_' // TRIM( SEN_PAR(NP) )
              ELMO_TABLE( N )%OUT_TYPE = M3REAL
              ELMO_TABLE( N )%VAR_TYPE = ET_DDM_CONC
              ELMO_TABLE( N )%ID = ICGRID
              ELMO_TABLE( N )%SOURCE = NP
              ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
          
              DO IMODE = 1,N_MODE
                DO ISPC = 1,N_AEROSPC
                  IF ( CGRID_NAME( ICGRID ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &               ELMO_TABLE( N )%IM = IMODE
                END DO
              END DO
          
              IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
                 ELMO_TABLE( N )%CUNIT = 'm-3'
              ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
                 ELMO_TABLE( N )%CUNIT = 'm2 m-3'
              ELSE IF ( CGRID_MASK_AERO( ICGRID ) ) THEN
                 ELMO_TABLE( N )%CUNIT = 'ug m-3'
              ELSE
                 ELMO_TABLE( N )%CUNIT = 'ppmV'
              END IF
              CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                             ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
              ELMO_TABLE( N )%IUNIT = IUNIT
              ELMO_TABLE( N )%DESC = 'Sensitivity of '//
     &           TRIM( CGRID_NAME( ICGRID ) ) //' to '//
     &           TRIM( SEN_PAR(NP) )
          
           END DO
        END IF
      END DO

      ! Load DDM3D Wet Deposition Fluxes into ELMO Table
      DO IWDEP = 1,N_SPC_WDEP
        DO NP = 1,NPMAX
           N = N + 1
           ELMO_TABLE( N )%NAME = 'WD_' // TRIM( WDEP_SPC( IWDEP ) )
     &                     // '_DDM' 
           ELMO_TABLE( N )%OUT_NAME = 'WD_' // TRIM(WDEP_SPC( IWDEP ))
     &                     // '_' // TRIM( SEN_PAR(NP) )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ET_DDM_WDEP
           ELMO_TABLE( N )%SOURCE = NP

           DO IMODE = 1,N_MODE
             DO ISPC = 1,N_AEROSPC
               IF ( WDEP_SPC( IWDEP ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &            ELMO_TABLE( N )%IM = IMODE
             END DO
           END DO

           ICGRID = MAP_WDEPtoCGRID( IWDEP )
           ELMO_TABLE( N )%ID = ICGRID
           ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
           IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'ha-1'
           ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
           ELSE
              ELMO_TABLE( N )%CUNIT = 'kg ha-1'
           END IF
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Sensitivity of '//
     &        TRIM( WDEP_SPC( IWDEP ) ) //' wet deposition to '//
     &        TRIM( SEN_PAR(NP) ) // ' from all cloud types'
        END DO
      END DO

      ! Load DDM3D Dry Deposition Fluxes into ELMO Table
      DO IDDEP = 1,N_SPC_DDEP
        DO NP = 1,NPMAX
           N = N + 1
           ELMO_TABLE( N )%NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))
     &                                // '_DDM'
           ELMO_TABLE( N )%OUT_NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))
     &                                // '_' // TRIM( SEN_PAR(NP) )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ET_DDM_DDEP
           ELMO_TABLE( N )%SOURCE = NP

           DO IMODE = 1,N_MODE
             DO ISPC = 1,N_AEROSPC
               IF ( DDEP_SPC( IDDEP ) .EQ. AEROSPC(ISPC)%NAME(IMODE) )
     &            ELMO_TABLE( N )%IM = IMODE
             END DO
           END DO

           ICGRID = MAP_DDEPtoCGRID( IDDEP )
           ELMO_TABLE( N )%ID = ICGRID
           ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
           IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'ha-1'
           ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
           ELSE
              ELMO_TABLE( N )%CUNIT = 'kg ha-1'
           END IF
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Sensitivity of '//
     &        TRIM( DDEP_SPC( IDDEP ) ) //' dry deposition to '//
     &        TRIM( SEN_PAR(NP) ) // ' from all cloud types'
        END DO
      END DO
#endif
 
      ! Load The Composite List into the ELMO Table
      DO ICMPST = 1,N_COMPOSITES
         INDX = INDEX1( CMPST( ICMPST )%NAME, N, ELMO_TABLE(:)%NAME )
         IF ( INDX .GT. 0 ) THEN
            XMSG = 'ELMO Composite ' // TRIM( ELMO_TABLE(INDX)%NAME) // ' is invalid ' // 
     &             'because the name is already a variable available in ELMO. Please ' // 
     &             'rename this composite.'
            CALL M3EXIT ( 'BUILD_ELMO_TABLE',0,0,XMSG,1)
         END IF

         N = N + 1
         ELMO_TABLE( N )%NAME = CMPST( ICMPST )%NAME
         ELMO_TABLE( N )%OUT_NAME = CMPST( ICMPST )%NAME
         ELMO_TABLE( N )%OUT_TYPE = M3REAL

         IF ( CMPST( ICMPST )%CMPST_TYPE(1:3) .EQ. 'DEP' ) THEN
             ELMO_TABLE( N )%VAR_TYPE = ET_CMPST_DEP
         ELSE
             ELMO_TABLE( N )%VAR_TYPE = ET_CMPST
         END IF
         ELMO_TABLE( N )%ID = ICMPST
         ELMO_TABLE( N )%CUNIT = CMPST( ICMPST )%CUNIT
         CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                        ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
         ELMO_TABLE( N )%IUNIT = IUNIT
         ELMO_TABLE( N )%DESC = CMPST( ICMPST )%DESC
         ELMO_TABLE( N )%MW   = 0.0
      END DO
 
#ifdef isam
      ! Apply Composites to ISAM source resolution and add to ELMO Table
      DO ICMPST = 1,N_COMPOSITES
         ! Define ELMO Variables for all ISAM sources
         DO ITAG = 1,NTAG_SA
           N = N + 1
           ELMO_TABLE( N )%NAME = TRIM(CMPST( ICMPST )%NAME)
     &                            // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = TRIM(CMPST( ICMPST )%NAME) 
     &                            // '_' // TRIM(TAGNAME( ITAG ))
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
         
           IF ( CMPST( ICMPST )%CMPST_TYPE(1:3) .EQ. 'DEP' ) THEN
               ELMO_TABLE( N )%VAR_TYPE = ET_ISAM_CMPST_DEP
           ELSE
               ELMO_TABLE( N )%VAR_TYPE = ET_ISAM_CMPST
           END IF
           ELMO_TABLE( N )%ID = ICMPST
           ELMO_TABLE( N )%CUNIT =  CMPST( ICMPST )%CUNIT
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT

           ELMO_TABLE( N )%SOURCE = ITAG
           ELMO_TABLE( N )%DESC = CMPST( ICMPST )%DESC // 
     &           ' for source ' // TRIM(TAGNAME( ITAG ))
           ELMO_TABLE( N )%MW   = 0.0
         END DO 
         
         ! Set Source-Dependent Properties for the sum of sources
         N = N + 1
         ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
         ELMO_TABLE( N )%OUT_NAME = TRIM(CMPST( ICMPST )%NAME) // '_TAGS'
         ELMO_TABLE( N )%SOURCE = NTAG_SA + 1
         ELMO_TABLE( N )%DESC = CMPST( ICMPST )%DESC // 
     &         ' for the sum of all sources.'
      END DO
#endif         

#ifdef sens
      ! Apply Composites to DDM3D source resolution and add to ELMO Table
      DO ICMPST = 1,N_COMPOSITES
         ! Define ELMO Variables for all remaining DDM3D sources
         DO NP = 1,NPMAX
           N = N + 1
           ELMO_TABLE( N )%NAME = TRIM(CMPST( ICMPST )%NAME)
     &                            // '_DDM'
           ELMO_TABLE( N )%OUT_NAME = TRIM(CMPST( ICMPST )%NAME)
     &                            // '_' // TRIM( SEN_PAR( NP ) )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           
           IF ( CMPST( ICMPST )%CMPST_TYPE(1:3) .EQ. 'DEP' ) THEN
               ELMO_TABLE( N )%VAR_TYPE = ET_DDM_CMPST_DEP
           ELSE
               ELMO_TABLE( N )%VAR_TYPE = ET_DDM_CMPST
           END IF
           ELMO_TABLE( N )%ID = ICMPST
           ELMO_TABLE( N )%SOURCE = NP
           ELMO_TABLE( N )%CUNIT =  CMPST( ICMPST )%CUNIT
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Sensitivity of ' // CMPST( ICMPST )%DESC // 
     &           ' to ' // TRIM( SEN_PAR( NP ) )
           ELMO_TABLE( N )%MW   = 0.0
         END DO
      END DO
#endif         
 
      ! Summarize Total number of ELMO Table entries and subset Table to
      ! just the populated rows.
      N_ELMO_TABLE = N
      ELMO_TABLE = ELMO_TABLE( 1:N_ELMO_TABLE ) 

      ! Initialize all ELMO Variables as Detailed calculations (i.e.
      ! L_COMP = FALSE). If they appear in ELMO_MAP_VARIABLES, then
      ! their L_COMP attribute will be set to True.
      ELMO_TABLE(:)%L_COMP = .FALSE.
      ALLOCATE( ELMO_COMP( N_ELMO_TABLE ) )

      END SUBROUTINE BUILD_ELMO_TABLE

!-------------------------------------------------------------------------
      SUBROUTINE OPEN_ELMO ( JDATE, JTIME, TSTEP )
!-------------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE MIO_ASCII

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      !...Arguments:

        INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP      ! output time step

      !...Local variables:

        CHARACTER( 16 ), SAVE :: PNAME = 'OPEN_ELMO'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables
        INTEGER :: MDATE, MTIME, IFL, IVAR, J, ICOMP
        INTEGER :: LAY_BOT, LAY_TOP, IV

      CALL LOG_MESSAGE( LOGDEV,'' )
      CALL LOG_SUBHEADING( LOGDEV, 'Open ELMO Output Files' )

#ifndef mpas
      ! Define General Output File Parameters
      FTYPE3D = GRDDED3
      GDNAM3D = GRID_NAME  ! from HGRD_DEFN
      TSTEP3D = TSTEP
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      GDTYP3D = GDTYP_GD
      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD 
      P_GAM3D = P_GAM_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD
      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD
      NTHIK3D =     1
#endif
      DO IFL = 1,N_ELMO_FILES
#ifndef mpas
         ! Try to open existing file for update
         IF ( IO_PE_INCLUSIVE ) THEN

            ! Open or append ELMO Output file in read/write mode for I/O
            ! Processors
            IF ( .NOT. OPEN3( ELMO_FILE(IFL)%FLOGICAL, FSRDWR3, PNAME ) ) THEN
               
               ! Set output file characteristics based on COORD.EXT and 
               ! open the ELMO file
               SDATE3D = JDATE
               STIME3D = JTIME
               
               IF ( ELMO_FILE( IFL )%TMODE .EQ. 1 ) THEN
                 ! Recalculate Date/Time Applicable for an Aggregate file
                 MDATE = JDATE; MTIME = JTIME
                 IF ( END_TIME ) CALL NEXTIME ( MDATE, MTIME, TSTEP )
               
                 SDATE3D = MDATE
                 STIME3D = MTIME
               END IF
               
               ! Specify Layers
               NLAYS3D = ELMO_FILE(IFL)%NLAYS
               LAY_BOT = ELMO_FILE( IFL )%LAY_BOT
               LAY_TOP = ELMO_FILE( IFL )%LAY_TOP
               DO L = LAY_BOT, LAY_TOP + 1
                  VGLVS3D( L ) = VGLVS_GD( L )
               END DO
               
               ! Specify Variables and their Properties
               NVARS3D = ELMO_FILE( IFL )%N_VARS 
               DO IVAR = 1,NVARS3D
                 J = ELMO_FILE( IFL )%TABID( IVAR )
                 VTYPE3D( IVAR ) = ELMO_TABLE( J )%OUT_TYPE
                 VNAME3D( IVAR ) = ELMO_TABLE( J )%OUT_NAME
                 UNITS3D( IVAR ) = ELMO_TABLE( J )%CUNIT

                 ! If this is an instantaneous file and a deposition
                 ! species, then indicate the units are a flux rather
                 ! than an aggregate per output time step
                 IF ( ELMO_FILE( IFL )%TMODE .EQ. 0 ) THEN
                   IV = ELMO_TABLE( J )%VAR_TYPE
                   IF ( IV .EQ. ET_DDEP .OR.
     &                  IV .EQ. ET_ISAM_DDEP .OR.
     &                  IV .EQ. ET_DDM_DDEP .OR.
     &                  IV .EQ. ET_WDEP .OR.
     &                  IV .EQ. ET_ISAM_WDEP .OR.
     &                  IV .EQ. ET_DDM_WDEP .OR.
     &                  IV .EQ. ET_CMPST_DEP .OR.
     &                  IV .EQ. ET_ISAM_CMPST_DEP .OR.
     &                  IV .EQ. ET_DDM_CMPST_DEP .OR.
     &                  IV .EQ. ET_DRVD_DDEP .OR.
     &                  IV .EQ. ET_ISAM_DRVD_DDEP .OR.
     &                  IV .EQ. ET_DDM_DRVD_DDEP .OR.
     &                  IV .EQ. ET_DRVD_WDEP .OR.
     &                  IV .EQ. ET_ISAM_DRVD_WDEP .OR.
     &                  IV .EQ. ET_DDM_DRVD_WDEP ) THEN
                     ! This is a deposition species so we should be
                     ! outputting a flux
                     UNITS3D( IVAR ) = TRIM( UNITS3D( IVAR ) ) // ' s-1'
                   END IF
                 END IF
                 VDESC3D( IVAR ) = ELMO_TABLE( J )%DESC
               END DO
               
               FDESC3D( 1 ) = 'ELMO output file'
               DO L = 2, MXDESC3
                  FDESC3D( L ) = ' '
               END DO
               
               ! Store MIO Metadata
               NDIMS3D( 1:NVARS3D ) = 4
               L_TSTEP( 1:NVARS3D ) = .True.
               L_LAY  ( 1:NVARS3D ) = .True.
               L_COL  ( 1:NVARS3D ) = .True.
               L_ROW  ( 1:NVARS3D ) = .True.
               L_VEXT ( 1:NVARS3D ) = .False.
               
               CALL LOAD_MIO_FILE ( ELMO_FILE( IFL )%FLOGICAL, NLAYS3D, 
     &              LAY_BOT, LAY_TOP,
     &              VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &              VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &              L_LAY(1:NVARS3D),   L_COL(1:NVARS3D), L_ROW(1:NVARS3D),
     &              L_VEXT(1:NVARS3D) )
               
               ! Open the Instantaneous ELMO File
               IF ( .NOT. OPEN3( ELMO_FILE(IFL)%FLOGICAL, FSNEW3, PNAME ) ) THEN
                   XMSG = 'Could not create '// 
     &                    ELMO_FILE( IFL )%FLOGICAL // ' file'
                  CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
               END IF
               
            END IF 

         ELSE

           ! Open ELMO Output files in read mode for non-I/O processors  
           IF ( .NOT. OPEN3( ELMO_FILE(IFL)%FLOGICAL, FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open ' // TRIM( ELMO_FILE(IFL)%FLOGICAL )
              CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
           END IF

         END IF
#endif
      END DO
 
      END SUBROUTINE OPEN_ELMO 

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_WRITE_LOG ( )
!-------------------------------------------------------------------------
    
      USE AERO_DATA, ONLY : N_MODE, AEROSPC, AERO_MISSING, AEROMODE,
     &                      CMODE, N_AEROSPC, MAP_MODE_MASK
      USE ORG_DEFN,  ONLY : N_OA, OASPC, MAP_AEROtoOA, MAP_ORG_MASK

      IMPLICIT NONE

      INTEGER IFL, IVAR, ICOMP, J, TABID, IFILE, AERO_ID, 
     &        IMODE, INLET, IOA, IM, K, IAER
      CHARACTER( 3000 ) :: FORMULA
      CHARACTER(    2 ) :: CSIGN
      CHARACTER(   70 ) :: FTEMP
      CHARACTER(   20 ) :: CTMODE
      INTEGER FORM_SIZE, LAST_SPACE
      LOGICAL L_MODE,L_SPEC
      LOGICAL :: MODE_MASK( N_MODE ), ORG_MASK( N_OA )
      INTEGER :: ORG_OP

      INTEGER, EXTERNAL :: FINDEX

      ! Write out all ELMO variables organized by output file
      CALL LOG_MESSAGE( LOGDEV,'' )
      CALL LOG_SUBHEADING( LOGDEV, 'ELMO Output Variable Report' )
      WRITE( LOGDEV,'(7x,A,8(/,13x,A))' )
     &                              'Note: these are populated using rules from the',
     &                              'CMAQ Control File supplied by the User. ',
     &                              'When variables are indented, they are components ',
     &                              'of the most recent non-indented variable. Becuase ',
     &                              'their output units should either match the output ',
     &                              'variable or be converted to its units, the third ',
     &                              'column reports any coefficients used when applying ',
     &                              'these components to the sum, excluding molecular ',
     &                              'weight, air density, etc.'


      DO IFL = 1,N_ELMO_FILES
         
         ! Header and File Metadata
         IF ( ELMO_FILE(IFL)%TMODE .EQ. 0 ) THEN
             CTMODE = 'Instantaneous'
         ELSE
             CTMODE = 'Aggregate'
         END IF

         WRITE( LOGDEV, '(/,5x,A,A)'   ) '>',REPEAT('-',80 )
         WRITE( LOGDEV,'(5x,A,I2,A,A,/,5x,A,A,/,8x,A,I2,A,I2,/,8x,A,I5)' )
     &       'Output File Num: "',IFL,'" | Temporality: ',TRIM(CTMODE),
     &       'Output Filename: ',TRIM(ELMO_FILE(IFL)%SHORTNAME),
     &       ' Bottom Layer: ',ELMO_FILE( IFL )%LAY_BOT,
     &       ' Top Layer: ',ELMO_FILE( IFL )%LAY_TOP,
     &       ' Number of Variables: ',ELMO_FILE( IFL )%N_VARS

         ! Print all variables on the file and their contents if they have
         ! components 
         WRITE( LOGDEV, '(/,8x,A,12x,A,12x,A,8x,A)' )
     &       'Variable','Type','Unit','Description'
         WRITE( LOGDEV, '(8x,A,3x,A,2x,A,4x,A)' )
     &       '-----------------','--------------','--------','--------------'

         DO IVAR = 1,ELMO_FILE( IFL )%N_VARS 
            J = ELMO_FILE( IFL )%TABID( IVAR )
            WRITE ( LOGDEV,'(8x,A16,4x,A14,2x,A8,4x,A)')
     &              ELMO_TABLE( J )%OUT_NAME,
     &              CETYPE( ELMO_TABLE( J )%VAR_TYPE ),
     &              ELMO_TABLE( J )%CUNIT,
     &              TRIM( ELMO_TABLE( J )%DESC )

            FORMULA = ''

            ! Determine type of variable in order to make formula
            ! correctly
            IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ET_CMPST .OR.
     &           ELMO_TABLE( J )%VAR_TYPE .EQ. ET_ISAM_CMPST .OR.
     &           ELMO_TABLE( J )%VAR_TYPE .EQ. ET_DDM_CMPST .OR.
     &           ELMO_TABLE( J )%VAR_TYPE .EQ. ET_CMPST_DEP .OR.
     &           ELMO_TABLE( J )%VAR_TYPE .EQ. ET_ISAM_CMPST_DEP .OR.
     &           ELMO_TABLE( J )%VAR_TYPE .EQ. ET_DDM_CMPST_DEP ) THEN

               ! Make Formula of Composite Variable from Components
               DO ICOMP = 1,ELMO_COMP( J )%N_VARS
                  TABID = ELMO_COMP( J )%TABID( ICOMP )
                  IMODE = ELMO_TABLE( TABID )%IM
                  INLET = ELMO_COMP( J )%INLET

                  L_MODE = .TRUE.
                  IF ( IMODE .NE. 0 ) L_MODE = ELMO_COMP( J )%L_MODE( IMODE )

                  IF ( L_MODE ) THEN
                     WRITE( FORMULA, '(A,A1,SP,ES9.2,A1,A)' ) TRIM(FORMULA),
     &                  ' ',ELMO_COMP( J )%COEFF(ICOMP),'*',
     &                  TRIM( ELMO_TABLE( TABID )%OUT_NAME )

                     ! If this is an aerosol with a size cutoff, make sure
                     ! to add the FPM[dp]_[mode] to the formula
                     IF ( ELMO_TABLE( TABID )%VAR_TYPE .EQ. ET_CONC .AND.
     &                    IMODE .NE. 0 .AND.INLET .NE. 0 ) THEN
                        WRITE( FORMULA, '(A,A2,A,A1,A)' ) TRIM(FORMULA),'*F',
     &                     TRIM( ELMO_INLET(INLET)%CINLET ),'_',
     &                     TRIM(AEROMODE( IMODE )%SUFF)
                     END IF
                  END IF
               END DO

            ELSE IF ( ELMO_TABLE( J )%DRVD_TAB .GT. 0 ) THEN
               
               ! Make Formula for Derived Variable Based on Derived
               ! Variable Parameter Table
               K = ELMO_TABLE( J )%DRVD_TAB

               CALL MAP_MODE_MASK( ELMO_DRVD_TAB( K )%MODE, MODE_MASK )
               CALL MAP_ORG_MASK ( ELMO_DRVD_TAB( K )%ORG_MASK, ORG_MASK )
               INLET  = ELMO_DRVD_TAB( K )%INLET
               ORG_OP = ELMO_DRVD_TAB( K )%ORG_OP

               ! Write the formula for total aerosol metrics
               IF ( ORG_OP .EQ. OT_OMOC .OR. ORG_OP .EQ. OT_OTOC ) THEN
                   ! Begin with a parenthesis
                   WRITE( FORMULA, '(A,A2)' ) TRIM(FORMULA),'( '
               END IF

               ! Add each aerosol species
               DO IAER = 1,N_AEROSPC
                  L_SPEC = .NOT. AEROSPC(IAER)%TRACER .AND. 
     &                     .NOT. AEROSPC(IAER)%NO_M2WET
                  IF ( ORG_OP .GT. 0 ) THEN
                      IOA = MAP_AEROtoOA( IAER )
                      IF ( IOA .EQ. 0 ) THEN
                         L_SPEC = .FALSE.
                      ELSE 
                         L_SPEC = L_SPEC .AND. ORG_MASK( IOA )
                         IF ( (ORG_OP.EQ.OT_OMOC .AND. OASPC(IOA)%OMtoOC.LE.1.0) .OR. 
     &                        (ORG_OP.EQ.OT_OTOC .AND. OASPC(IOA)%OTOC.LE.0.0)   .OR.
     &                        (ORG_OP.EQ.OT_OC   .AND. OASPC(IOA)%OMtoOC.LE.0.0) .OR.
     &                        (ORG_OP.EQ.OT_DD_OC.AND. OASPC(IOA)%OMtoOC.LE.0.0) .OR.
     &                        (ORG_OP.EQ.OT_WD_OC.AND. OASPC(IOA)%OMtoOC.LE.0.0) )
     &                      L_SPEC = .FALSE.
                      END IF
                  END IF
                  IF ( .NOT. L_SPEC ) CYCLE
                  
                  DO IM = 1,N_MODE
                     IF ( MODE_MASK( IM ) .AND. .NOT. AERO_MISSING(IAER,IM) ) THEN

                        ! Add Deposition Prefixes if applicable
                        IF ( ORG_OP .EQ. OT_DD_OA .OR. ORG_OP .EQ. OT_DD_OC ) THEN
                           WRITE( FORMULA, '(A,A5)' ) TRIM(FORMULA),' +DD_'
                        ELSE IF ( ORG_OP .EQ. OT_WD_OA. OR. ORG_OP .EQ. OT_WD_OC) THEN
                           WRITE( FORMULA, '(A,A5)' ) TRIM(FORMULA),' +WD_'
                        ELSE
                           WRITE( FORMULA, '(A,A2)' ) TRIM(FORMULA),' +'
                        END IF

                        ! Add Species Name and Coefficient
                        IF ( ORG_OP .EQ. 0 .OR. ORG_OP .EQ. OT_OA .OR.
     &                       ORG_OP .EQ. OT_DD_OA .OR. ORG_OP .EQ. OT_WD_OA ) THEN 
                           WRITE( FORMULA, '(A,A)' ) TRIM(FORMULA),
     &                        TRIM( AEROSPC(IAER)%NAME(IM) )
                        ELSE IF ( ORG_OP .EQ. OT_OC .OR. ORG_OP .EQ. OT_DD_OC .OR. 
     &                            ORG_OP .EQ. OT_WD_OC ) THEN
                           WRITE( FORMULA, '(A,A,A1,ES8.2)' ) TRIM(FORMULA),
     &                        TRIM( AEROSPC(IAER)%NAME(IM) ),'/',OASPC(IOA)%OMtoOC
                        ELSE IF ( ORG_OP .EQ. OT_OMOC ) THEN
                           WRITE( FORMULA, '(A,ES8.2,A1,A)' ) TRIM(FORMULA),
     &                        OASPC(IOA)%OMtoOC,'*',TRIM(AEROSPC(IAER)%NAME(IM) )
                        ELSE IF ( ORG_OP .EQ. OT_OTOC ) THEN
                           WRITE( FORMULA, '(A,ES8.2,A1,A)' ) TRIM(FORMULA),
     &                        OASPC(IOA)%OtoC,'*',TRIM( AEROSPC(IAER)%NAME(IM) )
                        END IF

                        ! Multiply by inlet fraction if applicable
                        IF ( INLET .GT. 0 ) 
     &                     WRITE( FORMULA, '(A,A2,A,A1,A)' ) TRIM(FORMULA),'*F',
     &                        TRIM( ELMO_INLET(INLET)%CINLET ),'_',
     &                        TRIM(AEROMODE( IM )%SUFF)
                     END IF
                  END DO
               END DO
               
               ! Add Denominator For OM:OC and O:C
               IF ( ORG_OP .EQ. OT_OMOC .OR. ORG_OP .EQ. OT_OTOC ) THEN
                   ! Begin with a parenthesis
                   WRITE( FORMULA, '(A,A5)' ) TRIM(FORMULA),') / ('
                   IF ( INLET .EQ. 0 ) 
     &                WRITE( FORMULA, '(A,A,A1)' ) TRIM(FORMULA),
     &                   TRIM( CMODE( ELMO_DRVD_TAB(K)%MODE ) ),'_'
                   WRITE( FORMULA, '(A,A2)' ) TRIM(FORMULA),'OA'
                   IF ( INLET .NE. 0 ) 
     &                WRITE( FORMULA, '(A,A2,A)' ) TRIM(FORMULA),'*F',
     &                   TRIM( ELMO_INLET(INLET)%CINLET )
                   WRITE( FORMULA, '(A,A1)' ) TRIM(FORMULA),')'
               END IF

            END IF  
 
            ! Write Formula to Log File Table
            IF ( FORMULA .NE. '' ) THEN   
               ! Parse Formula so that it prints correctly to screen
               FORMULA = '='//FORMULA( 1:LEN(FORMULA) )
               FORM_SIZE = LEN_TRIM( FORMULA )
               DO WHILE ( FORM_SIZE .GT. 70 )
                  ! Isolate a line of text before a + or - sign
                  LAST_SPACE = INDEX( FORMULA( 1:70 )," ", BACK=.TRUE. )

                  FTEMP = FORMULA( 1:LAST_SPACE )

                  WRITE( LOGDEV, '(10x,A)') FTEMP
                  FORMULA = '  '//FORMULA( LAST_SPACE+1:LEN(FORMULA) )
                  FORM_SIZE = LEN_TRIM( FORMULA )
               
               END DO

               ! Write Last Line
               WRITE( LOGDEV, '(10x,A)' ) FORMULA(1:FORM_SIZE)
            END IF
            WRITE( LOGDEV, * )
         END DO
      END DO

      WRITE( LOGDEV, * )

      END SUBROUTINE ELMO_WRITE_LOG

      END MODULE ELMO_INIT_DEFN
