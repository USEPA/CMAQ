
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      MODULE DEGRADE_SETUP_TOX
C**********************************************************************
C
C  FUNCTION:  Define arrays that identify species within CGRID used
C             based on input arrays
C
C  REVISION HISTORY: 07/29/05 : B.Hutzell - Initial version
C                    06 May 11: B.Hutzell: convert for Namelist redesign
C                    09 May 11: B.Hutzell: enabled a degraded species to
C                               be missing from namelists
C
C**********************************************************************

      USE CGRID_SPCS   ! CGRID species number and offsets
      USE UTILIO_DEFN  ! IOAPI declarations and definitions


      IMPLICIT NONE

C.....INCLUDES:

      INCLUDE SUBST_CONST         ! constants

C..declare and define variables used by maps and data

      INTEGER, PARAMETER :: N_REACT      = 30    ! number of  species being degraded
      INTEGER, PARAMETER :: N_UNI_LOSS   = 1     ! Number of Unimolecular Loss Processes
      INTEGER, PARAMETER :: N_BI_LOSS    = 5     ! Number of Bimolcular Loss processes
      INTEGER, PARAMETER :: N_TRI_LOSS   = 2     ! Number of Bimolcular Loss processes
      INTEGER, PARAMETER :: N_PHOTO_LOSS = 2     ! Number of Photolysis Loss processes

! Total number of Loss processes      
      INTEGER, PARAMETER :: N_PROCESSES  = N_UNI_LOSS
     &                                   + N_BI_LOSS
     &                                   + N_PHOTO_LOSS
     &                                   + N_TRI_LOSS

      CHARACTER(16), PARAMETER :: BLANK = ' '  ! default value for characters

      INTEGER, SAVE :: N_PHOTO_TAB    ! Number of photolysis rates in mechanism
      INTEGER, SAVE :: N_REACT_FOUND  ! Number ofreact species found in namelists

C..looping data

      INTEGER :: UNI_START
      INTEGER :: BI_START
      INTEGER :: TRI_START
      INTEGER :: PHOTO_START

      INTEGER :: UNI_STOP
      INTEGER :: BI_STOP
      INTEGER :: TRI_STOP
      INTEGER :: PHOTO_STOP

      CHARACTER(16), ALLOCATABLE :: REACT( : )         ! names of species being degraded
      CHARACTER(16), ALLOCATABLE :: BICAUSE( :,: )     ! species name that cause degradation
      CHARACTER(16), ALLOCATABLE :: TRICAUSE( :,:,: )  ! species name that cause degradation
      CHARACTER(16), ALLOCATABLE :: PHOTO_NAME( :,: )  ! name of photolysis rate for react(i)

      CHARACTER(16), ALLOCATABLE :: BI_PROD   ( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: TRI_PROD  ( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: PHOTO_PROD( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: UNI_PROD  ( :,: )  ! name of daughter product for react(i)

      REAL(8), ALLOCATABLE :: UNIRATE  ( :,: ) ! rate for unimolecular decay for react(i) [molecules/sec^1]
      REAL(8), ALLOCATABLE :: UNI_ACT  ( :,: ) ! activation energy for UNIRATE(I) [K]. Positive if exothermic
      REAL(8), ALLOCATABLE :: UNI_TEXP ( :,: ) ! exponent of Temperature
      REAL,    ALLOCATABLE :: UNI_YIELD( :,: ) ! production yield

      REAL(8), ALLOCATABLE :: BIRATE  ( :,: )  ! degradation rates for bimolecular reactions,  [cm^3/(sec*molecules)]
      REAL(8), ALLOCATABLE :: BI_ACT  ( :,: )  ! activation energy for BIRATE(I) [K]. Positive if exothermic
      REAL(8), ALLOCATABLE :: BI_TEXP ( :,: )  ! exponent of Temperature
      REAL,    ALLOCATABLE :: BI_YIELD( :,: )  ! production yield

      REAL(8), ALLOCATABLE :: TRIRATE  ( :,: ) ! degradation rates for trimolecular reactions,  [cm^3/(sec*molecules)]
      REAL(8), ALLOCATABLE :: TRI_ACT  ( :,: ) ! activation energy for TRIRATE(I) [K]. Positive if exothermic
      REAL(8), ALLOCATABLE :: TRI_TEXP ( :,: ) ! exponent of Temperature
      REAL,    ALLOCATABLE :: TRI_YIELD( :,: ) ! production yield

      REAL(8), ALLOCATABLE :: A_PHOTO    ( :,: ) ! multiplier of photolysis rates
      REAL,    ALLOCATABLE :: PHOTO_YIELD( :,: ) ! production yield

      REAL(8), ALLOCATABLE :: RATE_CONST( :,: )
      REAL(8), ALLOCATABLE :: RATE_YIELD( :,: )

      REAL(8)              :: EFFECTIVE_ZERO

C..arrays to store indices to CGRID

      INTEGER, ALLOCATABLE :: RXTANT_MAP( : )
      INTEGER, ALLOCATABLE :: PROD_MAP( :,: )
      INTEGER, ALLOCATABLE :: RAD_MAP( :,: )
      INTEGER, ALLOCATABLE :: RAD2_MAP( :,:,: )
      INTEGER, ALLOCATABLE :: PHOTO_MAP( :,: )

C..saved cell concentrations

      REAL( 8 ), ALLOCATABLE :: PREV_CONC( : )
      REAL( 8 ), ALLOCATABLE :: CURR_CONC( : )
      REAL( 8 )              :: TEMP                ! cell temperature [ K ]
      REAL( 8 )               :: NUMB_DENS           ! cell air number density [ 1/CM^3 ]

      REAL(8), ALLOCATABLE :: DELT_CONC( : )   ! changes predicted by degrade routine

C**********************************************************************

      CONTAINS
      

         SUBROUTINE DEGRADE_MAP( JDATE, JTIME, LOGDEV )
C**********************************************************************
C
C  Function:  Determine CGRID indices used in DEGRADE routine.
C             Check decay and degradation rates for negative values.
C
C  CALLED BY: INIT_DEGRADE
C
C**********************************************************************

         USE RXNS_DATA

         IMPLICIT NONE

C.....INCLUDES:


C.....ARGUMENTS:

         INTEGER, INTENT( IN ) :: JDATE        ! current model date , coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME        ! current model time , coded HHMMSS
         INTEGER, INTENT( IN ) :: LOGDEV       ! unit number for the log file

C.....PARAMETERS:

         REAL(8), PARAMETER :: TEMP_298K  = 298.15        ! K

C.....LOCAL VARIABLES:

         CHARACTER(  16 ) :: PNAME =  'DEGRADE_MAP    '     ! name of routine
         CHARACTER(  16 ) :: EMTPTY
         CHARACTER(  16 ) :: WNAME                          ! SCRATCH variable
         CHARACTER(  16 ) :: VNAME( N_PROCESSES+1 )         ! SCRATCH variable
         CHARACTER( 128 ) :: XMSG = 'FATAL ERROR in DEGRADE_SETUP'

         INTEGER :: MARKER, N, M       ! indexes
         INTEGER :: I, J, K            ! loop counters
         INTEGER :: LEN_NAME           ! number of nonblank characters in species name

         REAL(8) :: INV_T298K = 1.0D0 / TEMP_298K   ! K^-1

C.....EXTERNAL FUNCTIONS:

C**********************************************************************

C..save number of photolysis rates in mechanism

         N_PHOTO_TAB = NPHOTAB

C..Quality control on pairs of Reactant and Products

         WRITE( LOGDEV,* ) 'Comments on Species in degradation routines'

         N_REACT_FOUND = 0

         LOOP_REACT : DO I = 1, N_REACT

            VNAME( 1 ) = REACT( I )

            VNAME( UNI_START+1  :  UNI_STOP+1 ) = UNI_PROD  ( I, 1:N_UNI_LOSS )
            VNAME( BI_START+1   :   BI_STOP+1 ) = BI_PROD   ( I, 1:N_BI_LOSS  )
            VNAME( TRI_START+1  :  TRI_STOP+1 ) = TRI_PROD  ( I, 1:N_TRI_LOSS )
            VNAME( PHOTO_START+1:PHOTO_STOP+1 ) = PHOTO_PROD( I, 1:N_PHOTO_LOSS )

            RATE_YIELD( I,   UNI_START:UNI_STOP   ) = UNI_YIELD  ( I, 1:N_UNI_LOSS )
            RATE_YIELD( I,    BI_START:BI_STOP    ) = BI_YIELD   ( I, 1:N_BI_LOSS  )
            RATE_YIELD( I,   TRI_START:TRI_STOP   ) = TRI_YIELD  ( I, 1:N_TRI_LOSS )
            RATE_YIELD( I, PHOTO_START:PHOTO_STOP ) = PHOTO_YIELD( I, 1:N_PHOTO_LOSS )

            CALL UPCASE( VNAME( 1 ) )

            LEN_NAME = LEN_TRIM( VNAME( 1 ) )

            IF ( LEN_NAME < 1 ) THEN
               WRITE( LOGDEV,* ) 'A Reactant has no name.'
     &              // ' Check file degrade module'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            ENDIF

            DO K = 1, N_BI_LOSS
               IF ( VNAME( 1 ) == BICAUSE( I, K ) ) THEN
                  WRITE( LOGDEV,* ) 'WARNING: ',
     &                 VNAME( 1 )( 1:LEN_NAME ), ' is',
     &                 ' same as a species causing bimolecular degradation, hence'
     &                 // ' brakes linear assumptions '
               ENDIF
            ENDDO

            DO K = 1, N_TRI_LOSS
               IF ( VNAME( 1 ) == TRICAUSE( I, K, 1 ) .OR.
     &              VNAME( 1 ) == TRICAUSE( I, K, 2 ) ) THEN
                  WRITE( LOGDEV,* ) 'WARNING: ',
     &                 VNAME( 1 )( 1:LEN_NAME ), ' is same as',
     &                 ' a species causing trimolecular degradation,'
     &                 // ' hence brakes linear assumptions '
               ENDIF
            ENDDO

            DO J = 1, N_PROCESSES

               WNAME = VNAME( J + 1 )
               CALL UPCASE( WNAME )
               LEN_NAME = LEN_TRIM( WNAME )

               IF ( VNAME( 1 ) == WNAME ) THEN
                 WRITE( LOGDEV,* ) 'Warning: ', WNAME( 1:LEN_NAME ),
     &                ' and its parent share the same name.'
               ENDIF

            ENDDO

C..Set up indices that point to concentrations in CGRID.

            DO 20 J = 1, N_PROCESSES+1

               WNAME = VNAME( J )      ! note that reactant occupies VNAME(1)
               LEN_NAME = LEN_TRIM( WNAME )
               CALL UPCASE( WNAME )

               IF ( LEN_NAME > 0 ) THEN ! search gas species for index
                  N = INDEX1( WNAME, N_GC_SPC, GC_SPC )
                  MARKER = GC_STRT

                  IF ( N == 0 ) THEN  ! search non-reactive species for index
                     N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                     MARKER = NR_STRT
                     IF ( N == 0 ) THEN
#ifdef verbose_gas                     
                        WRITE( LOGDEV,'(a)' ) TRIM( WNAME ), ' is not '
     &                       // 'in gas or nonreactive species table.'
     &                       // 'its loss processes not calculated '
#endif                        
                        RXTANT_MAP( I ) = -1
                        CYCLE
                     ENDIF
                  ENDIF
         
                  N_REACT_FOUND = N_REACT_FOUND + 1

               ELSE
                  VNAME( J ) = 'NONE'
                  CYCLE
               ENDIF

C..write degrade data table

               IF ( N_REACT_FOUND == 1 ) THEN
                  WRITE( LOGDEV,* ) 'TABLE on Degradation Simulated.'
                  WRITE( LOGDEV,* ) 'Note: Rates use units of cm, sec, and molecules.'
                  WRITE( LOGDEV,* )
                  WRITE( LOGDEV,1600 )
               ENDIF

C..set map values

               IF ( J < 2 ) THEN
                  RXTANT_MAP( I ) = N + MARKER - 1
               ELSE
                  PROD_MAP( I, J-1 ) = N + MARKER - 1
               ENDIF

20          CONTINUE

C..cycle N_REACT LOOP

           IF( RXTANT_MAP( I ) .LT. 1 )CYCLE LOOP_REACT

C..check UNIMOLECULAR decay rates

            K = 0

            DO J = 1, N_UNI_LOSS

               IF ( UNIRATE( I, J ) < 0.0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), ' has a'
     &                 // 'negative rate for unimolecular decay.'
     &                 // 'Check degrade module'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               WRITE( LOGDEV,1100 ) VNAME( 1 ), RXTANT_MAP( I ),
     &              ' Unimolecular ',
     &              UNIRATE( I, J ) * TEMP_298K**UNI_TEXP( I, J )
     &              * EXP( -UNI_ACT( I, J ) * INV_T298K ),
     &              VNAME( J+1 ), PROD_MAP( I, J )

            ENDDO

            K = K + N_UNI_LOSS

C..locating degradation causes in CGRID

            DO 40 J = 1, N_BI_LOSS

C..checking degradation rates

               IF ( BIRATE( I, J ) < 0.0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), 'has a negative'
     &                 // ' rate for degradation by ', WNAME( 1:LEN_NAME ), '.'
     &                 // ' Check degrade module.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               WNAME = BICAUSE( I, J )

               CALL UPCASE( WNAME )

               LEN_NAME = LEN_TRIM( WNAME )

               IF ( LEN_NAME < 1 ) CYCLE

               IF ( WNAME == 'DENSITY' ) THEN      ! special case rate proportion to air density
                  RAD_MAP( I, J ) = 9999
                  WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' Bimolecular ', WNAME, RAD_MAP( I, J ),
     &                 BIRATE( I, J ) * TEMP_298K**BI_TEXP( I, J )
     &                 * EXP( -BI_ACT( I, J ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( I, J+K )
                  CYCLE
               ENDIF

               N = INDEX1( WNAME, N_GC_SPC, GC_SPC )   ! search gas species for index
               MARKER = GC_STRT

               IF ( N == 0 ) THEN                  ! search non-reactive species
                  N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                  MARKER = NR_STRT

                  IF ( N == 0 ) THEN
                     WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                    'NOT INCLUDED', WNAME, RAD_MAP( I, J ),
     &                    BIRATE( I, J ) * TEMP_298K**BI_TEXP( I, J )
     &                    * EXP( -BI_ACT( I, J ) * INV_T298K ),
     &                    VNAME( J+K+1 ), PROD_MAP( I, J+K )
                     MARKER = 0
                  ENDIF

               ENDIF ! End IF block that searches non-reactive species

C..set value in map

               RAD_MAP( I, J ) = N + MARKER - 1

               IF ( RAD_MAP( I, J ) > 0 ) THEN
                  WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' Bimolecular ', WNAME, RAD_MAP( I, J ),
     &                 BIRATE( I, J ) * TEMP_298K**BI_TEXP( I, J )
     &                 * EXP( -BI_ACT( I, J ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( I, J+K )
               ENDIF

40          CONTINUE

            K = K + N_BI_LOSS

            DO 50 J = 1, N_TRI_LOSS

C..checking degradation rates

               IF ( TRIRATE( I, J ) < 0.0D0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), 'has a negative'
     &                 // ' rate for trimolecular degradation.'
     &                 // ' Check degrade module.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               DO 60 K = 1, 2

                  WNAME = TRICAUSE( I, J, K )

                  CALL UPCASE( WNAME )

                  LEN_NAME = LEN_TRIM( WNAME )

                  IF ( LEN_NAME < 1 ) CYCLE

                  N = INDEX1( WNAME, N_GC_SPC, GC_SPC )   ! search gas species for index
                  MARKER = GC_STRT

                  IF ( N == 0 ) THEN                  ! search non-reactive species
                     N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                     MARKER = NR_STRT
                  ENDIF ! End IF block that searches non-reactive species

C..set value in map

                  RAD2_MAP( I, J, K ) = N + MARKER - 1

60             CONTINUE

               IF ( RAD2_MAP( I, J, 1 ) > 0 .AND. RAD2_MAP( I, J, 2 ) > 0 ) THEN

                  WRITE( LOGDEV,1300 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' Trimolecular ', TRICAUSE( I, J, 1 ), RAD2_MAP( I, J, 1 ),
     &                 TRICAUSE( I, J, 2 ), RAD2_MAP( I, J, 2 ),
     &                 TRIRATE( I, J ) * TEMP_298K**TRI_TEXP( I, J )
     &                 * EXP( -TRI_ACT( I, J ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( I, J+K )

               ELSE

                  WRITE( LOGDEV,1300 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' NOT INCLUDED ', TRICAUSE( I, J, 1 ), RAD2_MAP( I, J, 1 ),
     &                 TRICAUSE( I, J, 2 ), RAD2_MAP( I, J, 2 ),
     &                 TRIRATE( I, J ) * TEMP_298K**TRI_TEXP( I, J )
     &                 * EXP( -TRI_ACT( I, J ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( I, J+K )

               ENDIF

50          CONTINUE

            LEN_NAME = LEN_TRIM( REACT( I ) )

            K = K + N_TRI_LOSS

            DO 70 J = 1, N_PHOTO_LOSS

               WNAME = PHOTO_NAME( I, J )

               CALL UPCASE( WNAME )

               N = INDEX1( WNAME, NPHOTAB, PHOTAB )

               IF ( LEN_TRIM( WNAME ) < 2 ) CYCLE

               IF ( N < 1 ) THEN
                  WRITE( LOGDEV,* ) 'Photolysis rate, ', WNAME, ' for ',
     &                 REACT( I )( 1:LEN_NAME ),
     &                 'is not JTABLE and is not included. '
                  CYCLE
               ENDIF

               PHOTO_MAP( I, J ) = N

               WRITE( LOGDEV,1400 ) VNAME( 1 ), RXTANT_MAP( I ),
     &              ' Photolysis ', PHOTAB( N ), ' ', 'times', ' ',
     &              A_PHOTO( I, J ),
     &              VNAME( J+K+1 ), PROD_MAP( I, J+K )

70          CONTINUE

         END DO LOOP_REACT

         IF( N_REACT_FOUND .LT. 1 )RETURN

         WRITE( LOGDEV,* ) 'Note: If INDEX of CAUSE A OR B equals -1, the '
     &        // 'process is dropped from degradation '
     &        // 'calculation.'

         WRITE( LOGDEV,* ) BLANK

1000     FORMAT(A20,1X,A5,1X,A20,1X,2(A20,1X,A5,1X),A12,1X,A20,1X,A5)
1100     FORMAT(A20,1X,I5,1X,A20,1X,2(21X,6X),ES12.4,1X,A20,1X,I5)
1200     FORMAT(A20,1X,I5,1X,A20,1X,A20,1X,I5,1X,21X,6X,ES12.4,1X,A20,1X,I5)
1300     FORMAT(A20,1X,I5,1X,A20,1X,2(A20,1X,I5,1X),ES12.4,1X,A20,1X,I5)
1400     FORMAT(A20,1X,I5,1X,A20,1X,2(A20,1X,A5,1X),ES12.4,1X,A20,1X,I5)
1600     FORMAT('       DEGRADED      ',' Index',
     &              '       Process      ','        Cause A      ',
     &              ' Index', '       Cause B      ',' Index',
     &              '    Rate at 298K    ', '       Product      ',
     &              ' Index')

         RETURN

         END SUBROUTINE DEGRADE_MAP

      END MODULE DEGRADE_SETUP_TOX

