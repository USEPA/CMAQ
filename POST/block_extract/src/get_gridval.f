
      SUBROUTINE GET_GRIDVAL( N, CONC )

C*************************************************************************
C
C  FUNCTION: Gets avg concentration for each grid cell
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, August, 2000
C                   
C*************************************************************************
      USE M3UTILIO
      USE ENV_VARS
      USE M3FILES
      USE GRID_DATA
      USE TIME_STEP

      IMPLICIT NONE     

C..ARGUMENTS:
      INTEGER  N               ! Step number

      REAL CONC( M3GRID % NCOLS, M3GRID % NROWS, M3GRID % NLAYS, NVAR )


C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message

      INTEGER    JDATE        ! Time step date, coded YYYYDDD
      INTEGER    JTIME        ! Time step time, coded HHMMSS
      INTEGER    JFILE        ! No. of file containing jdate & jtime 
      INTEGER    L, V         ! Loop indices   

   
C**********************************************************************
      DATA PNAME / 'GET_GRIDVAL' /


      JDATE = STEP_DATE( N )
      JTIME = STEP_TIME( N )
      JFILE = STEP_FILE( N )

      DO V = 1, NVAR

         IF( .NOT. READ3( M3_FLNAME( JFILE ) , VNAME( V ), ALLAYS3, JDATE, 
     &                    JTIME, CONC( 1, 1, 1, V ) ) ) THEN
            MSG = 'Could not read variable ' // VNAME( V ) // 
     &            'in ' // M3_FLNAME( JFILE )         
            CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. )
         ENDIF

      ENDDO

      RETURN 

      END
