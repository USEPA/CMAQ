
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

!-----------------------------------------------------------------------
      MODULE PMDIAG_PROC
!-----------------------------------------------------------------------
! This module will collect all procedures used for subsetting the 
!      pm diagnostic variables requested by the user. It will also
!      open the output files and do the actual calculations of 
!      diagnostic variables of interest.
!
! Revision History:
! Ben Murphy 2020 Feb: Created
!-----------------------------------------------------------------------

      USE M3UTILIO
      USE PMDIAG_DATA
      USE GRID_CONF, ONLY: NCOLS, NROWS, NLAYS, IO_PE_INCLUSIVE

      IMPLICIT NONE

      LOGICAL, SAVE :: INST_ACTIVE, AVRG_ACTIVE
      INTEGER, SAVE :: INST_LAYER_TOP, INST_LAYER_BOT,
     &                 AVRG_LAYER_TOP, AVRG_LAYER_BOT

      CHARACTER(16), ALLOCATABLE, SAVE :: 
     &               INST_PARS_REG( : ), AVRG_PARS_REG( : ),
     &               INST_PARS( : ),     AVRG_PARS( : ),
     &               INST_PARS_OUT(:),   AVRG_PARS_OUT(:),
     &               INST_UNIT_OUT(:),   AVRG_UNIT_OUT(:)

      CHARACTER(80), ALLOCATABLE, SAVE :: 
     &               INST_DESC_OUT( : ), AVRG_DESC_OUT( : )

      INTEGER, SAVE :: N_PMDIAG, 
     &                 N_PMDIAG_INST, N_PMDIAG_AVRG,
     &                 N_PMDIAG_INST_OUT, N_PMDIAG_AVRG_OUT

      INTEGER, ALLOCATABLE, SAVE :: 
     &        MAP_INST2PMDIAG( : ), MAP_AVRG2PMDIAG( : ), 
     &        USED_PMDIAG_PARS( : ), USED_PMDIAG_MODE( : ),
     &        INST_TYPE_OUT( : ), AVRG_TYPE_OUT( : )

      CONTAINS

!-------------------------------------------------------------------------
      SUBROUTINE READ_PMDIAG_NML( )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------
         
      USE RUNTIME_VARS, ONLY: EMISSCTRL, LOGDEV
      
      IMPLICIT NONE

      INTEGER, PARAMETER :: N_REG = 100
      INTEGER            :: INUM
      INTEGER            :: EMCTRL_NML
      INTEGER            :: STAT
      LOGICAL, SAVE      :: INSTANT=.TRUE., 
     &                      AVERAGE=.TRUE.

      CHARACTER( 200 )   :: XMSG

      NAMELIST / PMDIAG_ACTIVATE / INSTANT, AVERAGE 
      NAMELIST / PMDIAG_INST / INST_LAYER_TOP, 
     &                         INST_LAYER_BOT, INST_PARS_REG
      NAMELIST / PMDIAG_AVRG / AVRG_LAYER_TOP, 
     &                         AVRG_LAYER_BOT, AVRG_PARS_REG

      ALLOCATE( INST_PARS_REG( N_REG ), AVRG_PARS_REG( N_REG ) )

      INST_ACTIVE      = .TRUE.
      INST_LAYER_TOP   = 1
      INST_LAYER_BOT   = 1
      INST_PARS_REG(:) = ''
      INST_PARS_REG(1) = 'ALL'
      AVRG_ACTIVE      = .TRUE.
      AVRG_LAYER_TOP   = 1
      AVRG_LAYER_BOT   = 1
      AVRG_PARS_REG(:) = ''
      AVRG_PARS_REG(1) = 'ALL'

      ! Retrieve the Name of the Emission Control File
      IF ( EMISSCTRL .EQ. "EMISSCTRL_NML" ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &           'You have chosen not to indicate the location of an',
     &           'Emission Control namelist file. Default settings ',
     &           'will be assumed.'
          RETURN
      END IF


      ! Open Emission Control Namelist File
      EMCTRL_NML = JUNIT()
      OPEN( FILE = EMISSCTRL, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'emissions control namelist file: ',TRIM( EMISSCTRL )
          CALL M3EXIT( 'READ_EMISS_NAMELIST',0,0,XMSG,1 )
      END IF
 
      ! Read Toggles for Turning Instantaneous and Average Files On/Off
      REWIND( EMCTRL_NML )
      READ( NML = PMDIAG_ACTIVATE, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'PMDIAG_ACTIVATE section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
      INST_ACTIVE = INSTANT
      AVRG_ACTIVE = AVERAGE
      
      ! Read Desired PMDIAG_INST parameters from input namelist
      REWIND( EMCTRL_NML )
      READ( NML = PMDIAG_INST, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'PMDIAG_INST section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF

      ! Check for ALL keyword in INST list
      DO INUM = 1,N_REG
         CALL UPCASE( INST_PARS_REG( INUM ) )
         IF ( INST_PARS_REG( INUM ) .EQ. 'ALL' ) 
     &      INST_PARS_REG( 1:N_PMDIAG_LIST ) = PMDIAG_LIST%NAME
      END DO

      ! Populate INST_PARS parameters list
      N_PMDIAG_INST = INDEX1( '', N_REG, INST_PARS_REG ) - 1
      ALLOCATE( INST_PARS( N_PMDIAG_INST ) )
      INST_PARS = INST_PARS_REG( 1:N_PMDIAG_INST )
      DO INUM = 1,N_PMDIAG_INST
          CALL UPCASE( INST_PARS( INUM ) )
      ENDDO

      ! Check Top-Layer Specification
      IF ( INST_LAYER_TOP .EQ. -1 ) INST_LAYER_TOP = NLAYS
      NLAY_PMDIAG_INST = INST_LAYER_TOP - INST_LAYER_BOT + 1

      ! Read Desired PMDIAG_AVRG parameters from input namelist
      REWIND( EMCTRL_NML )
      READ( NML = PMDIAG_INST, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'PMDIAG_AVRG section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF

      ! Check for ALL keyword in INST list
      DO INUM = 1,N_REG
         CALL UPCASE( AVRG_PARS_REG( INUM ) )
         IF ( AVRG_PARS_REG( INUM ) .EQ. 'ALL' ) 
     &      AVRG_PARS_REG( 1:N_PMDIAG_LIST ) = PMDIAG_LIST%NAME
      END DO

      ! Populate AVRG_PARS parameters list
      N_PMDIAG_AVRG = INDEX1( '', N_REG, AVRG_PARS_REG ) - 1
      ALLOCATE( AVRG_PARS( N_PMDIAG_AVRG ) )
      AVRG_PARS = AVRG_PARS_REG( 1:N_PMDIAG_AVRG )
      DO INUM = 1,N_PMDIAG_AVRG
          CALL UPCASE( AVRG_PARS( INUM ) )
      ENDDO
      
      ! Check Top-Layer Specification
      IF ( AVRG_LAYER_TOP .EQ. -1 ) AVRG_LAYER_TOP = NLAYS
      NLAY_PMDIAG_AVRG = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
 
      END SUBROUTINE READ_PMDIAG_NML

!-------------------------------------------------------------------------
      SUBROUTINE MAP_PMDIAG( )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------
      
      USE AERO_DATA, ONLY: N_MODE, AEROMODE

      IMPLICIT NONE

      INTEGER N, M, P, J, IM
      CHARACTER( 16 ), SAVE :: PNAME = 'MAP_PMDIAG'
      CHARACTER( 300 ) :: XMSG

      ! Read User-Input from Namelist input file (Emission Control File
      ! For Now)
      CALL READ_PMDIAG_NML()
      L_PMDIAG = ( INST_ACTIVE .OR. AVRG_ACTIVE )
      
      ! Map user-defined INST_PARS and AVRG_PARS arrays to the PMDIAG 
      ! parameters available on the PMDIAG_LIST. 
      N_PMDIAG = 0
      
      ALLOCATE( USED_PMDIAG_MODE( N_PMDIAG_LIST*N_MODE ),
     &          USED_PMDIAG_PARS( N_PMDIAG_LIST*N_MODE ) )
      USED_PMDIAG_MODE = 0
      USED_PMDIAG_PARS = 0

      IF ( INST_ACTIVE ) THEN
         ALLOCATE( MAP_INST2PMDIAG( N_PMDIAG_INST*N_MODE ),
     &             INST_PARS_OUT( N_PMDIAG_INST*N_MODE ) ,
     &             INST_TYPE_OUT( N_PMDIAG_INST*N_MODE ) ,
     &             INST_UNIT_OUT( N_PMDIAG_INST*N_MODE ) ,
     &             INST_DESC_OUT( N_PMDIAG_INST*N_MODE ) )

         N_PMDIAG_INST_OUT = 0
         DO J = 1,N_PMDIAG_INST
             N = INDEX1( INST_PARS( J ), N_PMDIAG_LIST, PMDIAG_LIST%NAME )
             IF ( N .NE. 0 ) THEN
                ! Add Entry for this Parameter
                N_PMDIAG = N_PMDIAG + 1
                N_PMDIAG_INST_OUT = N_PMDIAG_INST_OUT + 1
                USED_PMDIAG_PARS( N_PMDIAG ) = N
                MAP_INST2PMDIAG( N_PMDIAG_INST_OUT ) = N_PMDIAG

                ! Add Room for the Other Modes if this Parameter is
                ! Mode-Dependent
                IF ( PMDIAG_LIST( N )%MODE ) THEN
                    INST_PARS_OUT( N_PMDIAG_INST_OUT ) = 
     &                          TRIM(INST_PARS( J )) // aeromode( 1 )%suff
                    USED_PMDIAG_MODE( N_PMDIAG ) = 1
                    INST_TYPE_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%VAR_TYPE
                    INST_UNIT_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%UNIT
                    INST_DESC_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%DESC

                    DO IM = 2,N_MODE
                       N_PMDIAG = N_PMDIAG + 1
                       N_PMDIAG_INST_OUT = N_PMDIAG_INST_OUT + 1
                       USED_PMDIAG_PARS( N_PMDIAG ) = N
                       MAP_INST2PMDIAG( N_PMDIAG_INST_OUT ) = N_PMDIAG
                       INST_PARS_OUT( N_PMDIAG_INST_OUT ) = 
     &                          TRIM(INST_PARS( J )) // aeromode( IM )%suff
                       USED_PMDIAG_MODE( N_PMDIAG ) = IM
                       INST_TYPE_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%VAR_TYPE
                       INST_UNIT_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%UNIT
                       INST_DESC_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%DESC
                    END DO
                ELSE
                    INST_PARS_OUT( N_PMDIAG_INST_OUT ) = INST_PARS( J )
                    INST_TYPE_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                    INST_UNIT_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%UNIT     
                    INST_DESC_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%DESC     
                END IF
             ELSE
                XMSG = 'A PMDIAG Parameter has been requested that does not exist.'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
         END DO
         ALLOCATE( PMDIAG_INST( NCOLS,NROWS,NLAY_PMDIAG_INST,N_PMDIAG_INST_OUT ) )
         PMDIAG_INST = 0.0
      END IF
      
      IF ( AVRG_ACTIVE ) THEN
         ALLOCATE( MAP_AVRG2PMDIAG( N_PMDIAG_AVRG*N_MODE ),
     &             AVRG_PARS_OUT( N_PMDIAG_AVRG*N_MODE ) ,
     &             AVRG_TYPE_OUT( N_PMDIAG_AVRG*N_MODE ) ,
     &             AVRG_UNIT_OUT( N_PMDIAG_AVRG*N_MODE ) ,
     &             AVRG_DESC_OUT( N_PMDIAG_AVRG*N_MODE ) )

         N_PMDIAG_AVRG_OUT = 0
         DO J = 1,N_PMDIAG_AVRG
             N = INDEX1( AVRG_PARS( J ), N_PMDIAG_LIST, PMDIAG_LIST%NAME ) 
             M = INDEX1( AVRG_PARS( J ), N_PMDIAG_INST, INST_PARS ) 
             IF ( N .NE. 0 ) THEN
                ! Add Entry for this Parameter
                N_PMDIAG_AVRG_OUT = N_PMDIAG_AVRG_OUT + 1

                ! Add Room for the Other Modes if this Parameter is
                ! Mode-Dependent
                IF ( PMDIAG_LIST( N )%MODE ) THEN
                    AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ) = 
     &                             TRIM(AVRG_PARS( J )) // aeromode( 1 )%suff
                    AVRG_TYPE_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                    AVRG_UNIT_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%UNIT     
                    AVRG_DESC_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%DESC     
                    IF ( M .EQ. 0 ) THEN
                       N_PMDIAG = N_PMDIAG + 1
                       USED_PMDIAG_MODE( N_PMDIAG ) = 1
                       USED_PMDIAG_PARS( N_PMDIAG ) = N
                       MAP_AVRG2PMDIAG( N_PMDIAG_AVRG_OUT ) = N_PMDIAG
                    ELSE
                       P = INDEX1( AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ), 
     &                             N_PMDIAG_INST_OUT, INST_PARS_OUT ) 
                       MAP_AVRG2PMDIAG( N_PMDIAG_AVRG_OUT ) = MAP_INST2PMDIAG( P )
                    END IF    

                    DO IM = 2,N_MODE
                       N_PMDIAG_AVRG_OUT = N_PMDIAG_AVRG_OUT + 1
                       AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ) = 
     &                           TRIM(AVRG_PARS( J )) // aeromode( IM )%suff
                       AVRG_TYPE_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                       AVRG_UNIT_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%UNIT     
                       AVRG_DESC_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%DESC     
                       IF ( M .EQ. 0 ) THEN
                          N_PMDIAG = N_PMDIAG + 1
                          USED_PMDIAG_MODE( N_PMDIAG ) = IM
                          USED_PMDIAG_PARS( N_PMDIAG ) = N
                          MAP_AVRG2PMDIAG( N_PMDIAG_AVRG_OUT ) = N_PMDIAG
                       ELSE
                          P = INDEX1( AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ), 
     &                                N_PMDIAG_INST_OUT, INST_PARS_OUT ) 
                          MAP_AVRG2PMDIAG( N_PMDIAG_AVRG_OUT ) = MAP_INST2PMDIAG( P )
                       END IF
                    END DO
                ELSE
                    AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ) = AVRG_PARS( J )
                    AVRG_TYPE_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                    AVRG_UNIT_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%UNIT     
                    AVRG_DESC_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%DESC     
                    IF ( M .EQ. 0 ) THEN
                       N_PMDIAG = N_PMDIAG + 1
                       USED_PMDIAG_PARS( N_PMDIAG ) = N
                       MAP_AVRG2PMDIAG( N_PMDIAG_AVRG_OUT ) = N_PMDIAG
                    ELSE
                       P = INDEX1( AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ), 
     &                             N_PMDIAG_INST_OUT, INST_PARS_OUT ) 
                       MAP_AVRG2PMDIAG( N_PMDIAG_AVRG_OUT ) = MAP_INST2PMDIAG( P )
                    END IF   
                END IF 

             ELSE
                XMSG = 'A PMDIAG Parameter has been requested that does not exist.'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
         END DO
         ALLOCATE( PMDIAG_AVRG( NCOLS,NROWS,NLAY_PMDIAG_AVRG,N_PMDIAG_AVRG_OUT ) )
         PMDIAG_AVRG = 0.0
      END IF
      
      ! Allocate Local PMDIAG Array
      ALLOCATE( PMDIAG_LOCAL( N_PMDIAG ) )

      END SUBROUTINE MAP_PMDIAG

!-------------------------------------------------------------------------
      SUBROUTINE CALC_PMDIAG( C,R,L,WRITETIME )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS
      USE AEROMET_DATA, ONLY: AIRRH

      IMPLICIT NONE

      INTEGER C, R, L
      INTEGER IDG, IM, AL, IL
      LOGICAL L_AERODIST_PARAMS

      REAL :: fPM1, fPM25, fPM10, fAMS,
     &        PM25

      LOGICAL :: WRITETIME

      L_AERODIST_PARAMS = .FALSE.

      IF ( ( L.GE.INST_LAYER_BOT .OR. L.GE.AVRG_LAYER_BOT ) .AND.
     &     ( L.LE.INST_LAYER_TOP .OR. L.LE.AVRG_LAYER_TOP ) ) THEN
      IL = L - INST_LAYER_BOT + 1
      AL = L - AVRG_LAYER_BOT + 1

      ! Loop Through PMDIAG Functions and Assign Values to Local Array
      DO IDG = 1,N_PMDIAG 
         IM = USED_PMDIAG_MODE( IDG )

         ! Determine which parameter to calculate
         SELECT CASE( USED_PMDIAG_PARS( IDG ) )

            ! Retrieve Standard Deviation of Each Mode
            CASE ( 1 )
               PMDIAG_LOCAL( IDG ) = EXP( AEROMODE_LNSG( IM ) )

            ! Retrieve Mode Dry Diameter
            CASE ( 2 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PMDIAG_LOCAL( IDG ) = DRY_AERO_DIAM( IM )  ! dry diam.

            ! Retrieve Mode Wet Diameter
            CASE ( 3 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PMDIAG_LOCAL( IDG ) = WET_AERO_DIAM( IM )  ! wet diam.

            ! Retrieve Mode Wet Second Moment
            CASE ( 4 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PMDIAG_LOCAL( IDG ) = WET_AERO_M2( IM )  ! dry M2

            ! Retrieve Dry Third Moment
            CASE ( 5 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PMDIAG_LOCAL( IDG ) = DRY_AERO_M3( IM )  ! dry M3

            ! Retrieve Wet Third Moment
            CASE ( 6 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PMDIAG_LOCAL( IDG ) = WET_AERO_M3( IM )  ! wet M3

            ! Retrieve Relative Humidity
            CASE ( 7 )
               PMDIAG_LOCAL( IDG ) = AIRRH

            ! Retrieve PM1 Fraction in Each Mode
            CASE ( 8 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               CALL AERO_INLET_PM1( WET_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &              WET_AERO_DENS( IM ), fPM1 ) 
               PMDIAG_LOCAL( IDG ) = fPM1

            ! Retrieve PM25 Fraction in Each Mode
            CASE ( 9 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               CALL AERO_INLET_PM25( WET_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &              WET_AERO_DENS( IM ), fPM25 )
               PMDIAG_LOCAL( IDG ) = fPM25

            ! Retrieve PM10 Fraction in Each Mode
            CASE ( 10 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               CALL AERO_INLET_PM10( WET_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &              WET_AERO_DENS( IM ), fPM10 )
               PMDIAG_LOCAL( IDG ) = fPM10

            ! Retrieve AMS Fraction in Each Mode
            CASE ( 11 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               CALL AERO_AMS( WET_AERO_M3( IM ),  WET_AERO_M2( IM ),
     &                        MOMENT0_CONC( IM ), AEROSPC_CONC( AH2O_IDX,IM ),
     &                        WET_AERO_DENS( IM ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                        fAMS )
               PMDIAG_LOCAL( IDG ) = fAMS
            
            ! Retrieve GAMMA_N2O5IJ - fine N2O5 heterogeneous rxn probability
            CASE ( 12 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = GAMMA_N2O5IJ( C,R,L )

            ! Retrieve GAMMA_N2O5K - coarse N2O5 heterogeneous rxn probability
            CASE ( 13 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = GAMMA_N2O5K( C,R,L )

            ! Retrieve YCLNO2IJ - fine CLNO2 heterogeneous rxn probability
            CASE ( 14 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = YCLNO2IJ( C,R,L )

            ! Retrieve YCLNO2K - coarse CLNO2 heterogeneous rxn probability
            CASE ( 15 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = YCLNO2K( C,R,L )

            ! Retrieve GAMMA_IEPOX - heterogeneous uptake coefficient
            CASE ( 16 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = GAMMA_IEPOX( C,R,L )

            ! Retrieve Mode Density [kg m-3]
            CASE ( 17 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PMDIAG_LOCAL( IDG ) = WET_AERO_DENS( IM )  ! dry i-mode Density

            ! Retrieve Particle-Phase Reaction Rate Constant for IEPOX
            CASE ( 18 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = KPARTIEPOX( C,R,L )

            ! Retrieve IMAE Heterogeneous Uptake Coefficient
            CASE ( 19 )
               PMDIAG_LOCAL( IDG ) = 0.0
               IF ( AERO_CHEM_SET ) PMDIAG_LOCAL( IDG ) = GAMMA_IMAE( C,R,L )

            ! Retrieve Bulk PM2.5 Concentration
            CASE ( 20 )
               IF ( .NOT. L_AERODIST_PARAMS ) CALL CALC_AERODIST_PARAMS( L_AERODIST_PARAMS )
               PM25 = 0.0
               DO IM = 1,N_MODE
                  CALL AERO_INLET_PM25( WET_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &                 WET_AERO_DENS( IM ), fPM25 )
                  PM25 = PM25 + SUM( AEROSPC_CONC( :,IM ), 
     &                               MASK = .NOT.AEROSPC(:)%TRACER ) * fPM25
               END DO
               PMDIAG_LOCAL( IDG ) = PM25 

            END SELECT
      END DO

      ! *** Aggregate Diagnostic Parameters in Average Array
      IF ( AVRG_ACTIVE ) THEN
         DO  IDG = 1,N_PMDIAG_AVRG_OUT
            PMDIAG_AVRG( C,R,AL,IDG ) = PMDIAG_AVRG( C,R,AL,IDG ) + PMDIAG_LOCAL( MAP_AVRG2PMDIAG( IDG ) )
         END DO
      END IF

      ! *** Populate Diagnostic Parameters to Instantaneous Array if this is a write step
      IF ( WRITETIME .AND. INST_ACTIVE ) THEN
         DO IDG = 1,N_PMDIAG_INST_OUT
            PMDIAG_INST( C,R,IL,IDG ) = PMDIAG_LOCAL( MAP_INST2PMDIAG( IDG ) )
         END DO
      END IF
 
      END IF
      END SUBROUTINE CALC_PMDIAG

!-------------------------------------------------------------------------
      SUBROUTINE OPEN_PMDIAG ( JDATE, JTIME, TSTEP )
! Revision history
!   ??? Frank Binkowski
!   8 Sep 01 J.Young: dyn alloc - Use HGRD_DEFN
!   03 Sep 01 David Wong: for new pario
!   13 May 04 P.Bhave: added RH to species list; removed M0 and dry M2
!                      conc's from species list
!   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
!                      domain specifications in one module
!   18 Jul 05 P.Bhave: Added mass fractions of each mode < 2.5um to output list
!   06 Apr 06 P.Bhave: Added GAMMA_N2O5 to output list
!   25 May 06 P.Bhave: Changed units of all DG variables from m to um, as 
!                      suggested by Dr. Bill Hutzell
!   11 Apr 08 J.Kelly: Added STDEVCOR and variables to account for dry & wet
!                      DGCOR, M2COR_WET, and M3COR_DRY
!   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
!   26 Sep 14 H. Pye: Added GAMMA_IEPOX and replaced blank units with na
!   11 May 16 D. Wong: - Modified the code to retreive PMDIAG information from
!                        construct PMDIAG_SPC_RECORD stored in module cgrid_spcs
!                        to provide flexibility to handle AE6 and AE61
!                      - renamed the procedure name from OPDIAM to OPPMDIAG
!-------------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      !...Arguments:

        INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP      ! output time step

      !...Local variables:

        CHARACTER( 16 ), SAVE :: PNAME = 'OPEN_PMDIAG'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables
        INTEGER :: MDATE, MTIME

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

      ! Open Instantaneous PM Diagnostic File
      IF ( INST_ACTIVE ) THEN
        
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSRDWR3, PNAME ) ) THEN

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = JDATE
            STIME3D = JTIME
            CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         
            NLAYS3D = INST_LAYER_TOP - INST_LAYER_BOT + 1
            DO L = INST_LAYER_BOT, INST_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_PMDIAG_INST_OUT 
            VTYPE3D(1:NVARS3D) = INST_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = INST_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = INST_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = INST_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Parameters Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_PMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF
      END IF
 

      ! Open Average Output PM Diagnostic File
      IF ( AVRG_ACTIVE ) THEN
        
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSRDWR3, PNAME ) ) THEN

            ! Get end time information from environment variable AVG_FILE_ENDTIME
            MDATE = JDATE; MTIME = JTIME
            IF ( END_TIME ) CALL NEXTIME ( MDATE, MTIME, TSTEP )

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = MDATE
            STIME3D = MTIME
         
            NLAYS3D = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
            DO L = AVRG_LAYER_BOT, AVRG_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_PMDIAG_AVRG_OUT 
            VTYPE3D(1:NVARS3D) = AVRG_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = AVRG_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = AVRG_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = AVRG_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Parameters Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_APMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF
 
         END IF
      END IF
 
      END SUBROUTINE OPEN_PMDIAG 

!-------------------------------------------------------------------------
      SUBROUTINE WRITE_PMDIAG ( JDATE, JTIME, TSTEP )
! Revision history
!   20 Feb - B. Murphy: Created
!-------------------------------------------------------------------------

      USE RUNTIME_VARS
      USE UTILIO_DEFN

      IMPLICIT NONE 

      INCLUDE SUBST_FILES_ID

      INTEGER JDATE, JTIME, TSTEP(3)
      INTEGER MDATE, MTIME
      CHARACTER( 300 ) XMSG

      CHARACTER( 16 ), SAVE :: PNAME = 'WRITE_PMDIAG'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
 
         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( INST_ACTIVE ) THEN
         !      IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSREAD3, PNAME ) ) THEN
         !         XMSG = 'Could not open ' // CTM_CONC_1 // ' file for update'
         !         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         !      END IF
            END IF
 
            IF ( AVRG_ACTIVE ) THEN
         !      IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSREAD3, PNAME ) ) THEN
         !         XMSG = 'Could not open ' // A_CONC_1 // ' file for update'
         !         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         !      END IF
            END IF
         END IF

      END IF 
C *** If IO Proceesor, then Write Data
      !IF ( IO_PE_INCLUSIVE ) THEN
         MDATE = JDATE
         MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, TSTEP( 2 ) )

C *** Write data to the aerosol diagnostic file.
         IF ( INST_ACTIVE ) THEN
            IF ( .NOT. WRITE3( CTM_PMDIAG_1, 
!    &           INST_PARS_OUT(1:N_PMDIAG_INST_OUT), MDATE, MTIME,
!    &           PMDIAG_INST(:,:,INST_LAYER_BOT:INST_LAYER_TOP,:) ) ) THEN
     &           ALLVAR3, MDATE, MTIME,
     &           PMDIAG_INST(:,:,:,:) ) ) THEN
               XMSG = 'Could not write ' // CTM_PMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_PMDIAG_1,
     &                     'for date and time', MDATE, MTIME

         END IF
     
C *** Write data to the average aerosol diagnostic file.
         IF ( AVRG_ACTIVE ) THEN
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF

            IF ( .NOT. WRITE3( CTM_APMDIAG_1, 
       !&           AVRG_PARS_OUT(1:N_PMDIAG_AVRG_OUT), MDATE, MTIME, 
     &           ALLVAR3, MDATE, MTIME, 
     &           PMDIAG_AVRG(:,:,:,:) / 
     &           PMDIAG_AVRG_STEP ) ) THEN
               XMSG = 'Could not write ' // CTM_APMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_APMDIAG_1,
     &                     'for date and time', MDATE, MTIME
         END IF
     
      !END IF   ! WRITETIME

      RETURN 

      END SUBROUTINE WRITE_PMDIAG

      END MODULE PMDIAG_PROC
