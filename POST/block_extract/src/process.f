
      SUBROUTINE PROCESS

C*************************************************************************
C
C  FUNCTION:
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 2000
C                   
C*************************************************************************
      USE M3UTILIO
      USE ENV_VARS
      USE M3FILES
      USE GRID_DATA
      USE TIME_STEP

      IMPLICIT NONE     

C..ARGUMENTS: None

C..PARAMETERS:

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME          ! Program Name
      CHARACTER*256   MSG            ! Error message

      INTEGER   IND                ! Array index
      INTEGER   LBEG               ! Loop start index
      INTEGER   STEP               ! Loop index

      REAL, ALLOCATABLE :: CONC( : , : , : , : )
  
C**********************************************************************
      DATA PNAME / 'PROCESS' /

      ALLOCATE( CONC( M3GRID % NCOLS, M3GRID % NROWS, M3GRID % NROWS,
     &                NVAR ) )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set start loop index
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LBEG = 0
      DO STEP = 1, NFLSTEPS
         IF( SDATE .EQ. STEP_DATE( STEP ) .AND. STIME .EQ. STEP_TIME( STEP ) )
     &      LBEG = STEP
      ENDDO

c      do STEP = 1, nflsteps
c        print *, STEP, step_date(STEP), step_time(STEP)
c      enddo

c      print *, sdate, stime, lbeg, nsteps

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over time steps to read & output results
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO STEP = LBEG, NSTEPS

         CALL GET_GRIDVAL( STEP, CONC )

         CALL OUTPUT( STEP, CONC  )
  
      ENDDO

      RETURN

92000 FORMAT( 'ERROR: SDATE ( ', I7, ' ', A, ' ) not found' )
92020 FORMAT( 'ERROR: EDATE ( ', I7, ' ', A, ' ) not found' )

      END
