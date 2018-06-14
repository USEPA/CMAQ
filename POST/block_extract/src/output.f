      SUBROUTINE OUTPUT( STEP, CONC )

C*************************************************************************
C
C  FUNCTION:
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: 
C      Created by Jerry Gipson, August, 2000
C      Modified by C. Hogrefe and C. Nolte to use M3UTILIO and 
C          Fortran intrinsic TRIM(), June 2017
C                   
C*************************************************************************
      USE M3UTILIO
      USE M3FILES
      USE ENV_VARS
      USE GRID_DATA
      USE TIME_STEP

      IMPLICIT NONE     

C..ARGUMENTS:
      INTEGER   STEP      ! Step number

      REAL CONC( M3GRID % NCOLS, M3GRID % NROWS, M3GRID % NLAYS, NVAR )

C..PARAMETERS: NONE
 
C..SAVED LOCAL VARIABLES:
      INTEGER   IOUT           ! Output file unit number
      SAVE      IOUT  

      LOGICAL LFIRST          ! Flag for first call
      SAVE    LFIRST

      INTEGER, SAVE ::  NCELLS         ! No. of cells

      CHARACTER*11, ALLOCATABLE, SAVE :: DATEOUT( : )  
      CHARACTER*10, ALLOCATABLE, SAVE :: TIMEOUT( : )  

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*24    CRDATE       ! Create date
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message
      CHARACTER*256   RET_VAL      ! Returned value of environment variable


      INTEGER   IND            ! Index
      INTEGER   JDATE          ! Create date YYYYDDD
      INTEGER   JTIME          ! Create time HHMMSS
      INTEGER   C, R, L, V     ! Loop indices
      INTEGER   NFL

      REAL MAXPEAK

c      CHARACTER*11, ALLOCATABLE :: DATEOUT( : )  
c      CHARACTER*10, ALLOCATABLE :: TIMEOUT( : ) 

C**********************************************************************
      DATA PNAME  / 'OUTPUT' /
      DATA LFIRST / .TRUE. /


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write header info on first call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LFIRST ) THEN

         IOUT = JUNIT()
         OPEN( UNIT = IOUT, FILE = OUT_FNAME )

         CALL GETDTTIME( JDATE, JTIME )
         CALL NEXTIME( JDATE, JTIME, -050000 )
         CRDATE = DT2STR( JDATE, JTIME )

         DO NFL = 1, N_M3FILES
            CALL NAMEVAL( M3_FLNAME( NFL ), RET_VAL )
!           WRITE( IOUT, 92000) NFL, RET_VAL ( 1 : TRIMLEN( RET_VAL ) )
            WRITE( IOUT, 92000) NFL, TRIM( RET_VAL ) 
         ENDDO
  
         WRITE( IOUT, 92060) '1'

!        WRITE( IOUT, 92080) TIME_CONV ( 1 : TRIMLEN( TIME_CONV) )
         WRITE( IOUT, 92080) TRIM( TIME_CONV ) 

         WRITE( IOUT, 92120) CRDATE( 1 : 8 ), CRDATE( 9 : 24 )

         IF( OUTFORMAT .EQ. 'SAS' ) THEN
            WRITE( IOUT, 92140) ( VNAME( V )( 1 : 12) , V = 1, NVAR )
         ELSE
            WRITE( IOUT, 92160) ( VNAME( V )( 1 : 12) , V = 1, NVAR )
         ENDIF

         ALLOCATE ( DATEOUT( NFLSTEPS ) )
         ALLOCATE ( TIMEOUT( NFLSTEPS ) )
         CALL GET_TIMOUT( DATEOUT, TIMEOUT )

         LFIRST = .FALSE.

      ENDIF


      DO L = LOLEV, HILEV
         DO C = LOCOL, HICOL
            DO R = LOROW, HIROW

               IF( OUTFORMAT .EQ. 'SAS' ) THEN
                   WRITE( IOUT, 92200 ) DATEOUT( STEP ), TIMEOUT( STEP ), 
     &                                  C, R, L, 
     &                           ( CONC( C, R, L, V ), V = 1, NVAR )
               ELSE
                  WRITE( IOUT, 92300 ) DATEOUT( STEP )( 1 : 7 ), 
     &                           TIMEOUT( STEP )( 1 : 6 ),
     &                           C, R, L, ( CONC( C, R, L, V ), V = 1, NVAR )  
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      RETURN

92000 FORMAT( '# INPUT M3 FILE ', I2, ': ', A )
92060 FORMAT( '# AVERAGING TIME: ', A2 )
92080 FORMAT( '# OUTPUT TIME CONVENTION: ', A )
92120 FORMAT( '# CREATION DATE: ', A , ' EST', A )
92140 FORMAT( ' DATE      TIME       COL ROW LV', 3X,120( A12) )
92160 FORMAT( ' DATE  TIME   COL ROW LV', 3X,120( A14) )
92200 FORMAT( A11, 1X, A10, 1X, I3, 1X, I3, 1X, I2, 1X, 120( 1PE14.4 ) )
92300 FORMAT(  A7, 1X,  A6, 1X, I3, 1X, I3, 1X, I2, 1X, 120( 1PE14.4 ) )

      END
