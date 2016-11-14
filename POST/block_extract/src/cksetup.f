      SUBROUTINE CKSETUP

C*************************************************************************
C
C  FUNCTION: To check environment variables for consistency with grid data
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, September, 2000
C                   
C*************************************************************************
      USE ENV_VARS
      USE M3FILES
      USE GRID_DATA
      USE TIME_STEP

      IMPLICIT NONE     

C..INCLUDE FILES:
      INCLUDE 'PARMS3.EXT'     ! IOAPI parameters
      INCLUDE 'FDESC3.EXT'     ! IOAPI file description
      INCLUDE 'IODECL3.EXT'    ! IOAPI declarations

C..ARGUMENTS: NONE

C..PARAMETERS: NONE

C..EXTERNAL FUNCTIONS:
      INTEGER INDEX1          ! Get index on string in a list of strings
      INTEGER TRIMLEN         ! Get last non-blank character pos in string

C..SAVED LOCAL VARIABLES: NONE

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80  MSG               ! Log message
      CHARACTER*16  PNAME             ! Program Name

      INTEGER LBEG, N      ! Loop indices

      LOGICAL LERROR    ! Error Flag
   
C**********************************************************************
      DATA PNAME / 'CKSETUP' /

      LERROR = .FALSE.


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to see that requested variable is on all files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO N = 1, N_M3FILES

         IF( .NOT. DESC3 ( M3_FLNAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // M3_FLNAME( N ) 
     &         // ' file'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

         IF( INDEX1( VNAME, NVARS3D, VNAME3D ) .EQ. 0 ) THEN
            WRITE( LOGUNIT, 94445 ) VNAME( 1 : TRIMLEN( VNAME ) ),
     &                              M3_FLNAME( N )
            LERROR = .TRUE.
         ENDIF

      ENDDO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set default column, row & level indices
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LOCOL .EQ. 0 ) LOCOL = 1
      IF( HICOL .EQ. 0 ) HICOL = M3GRID % NCOLS
      IF( LOROW .EQ. 0 ) LOROW = 1
      IF( HIROW .EQ. 0 ) HIROW = M3GRID % NROWS
      IF( LOLEV .EQ. 0 ) LOLEV = 1
      IF( HILEV .EQ. 0 ) HILEV = M3GRID % NLAYS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check for valid cols, rows, & levels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LOCOL .LT. 1 .OR. LOCOL .GT. M3GRID % NCOLS ) THEN
         MSG = 'ERROR: Invalid LOCOL'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( HICOL .LT. 1 .OR. HICOL .GT. M3GRID % NCOLS ) THEN
         MSG = 'ERROR: Invalid HICOL'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( LOCOL .GT. HICOL ) THEN
         WRITE( MSG, '( A, I3 )' ) 'ERROR: Inconsistent LOCOL & HICOL'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( LOROW .LT. 1 .OR. LOROW .GT. M3GRID % NROWS ) THEN
         MSG = 'ERROR: Invalid LOROW'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( HIROW .LT. 1 .OR. HIROW .GT. M3GRID % NROWS ) THEN
         MSG = 'ERROR: Invalid HIROW'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( LOROW .GT. HIROW ) THEN
         WRITE( MSG, '( A, I3 )' ) 'ERROR: Inconsistent LOROW & HIROW'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( LOLEV .LT. 1 .OR. LOLEV .GT. M3GRID % NLAYS ) THEN
         MSG = 'ERROR: Invalid LOLEV'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( HILEV .LT. 1 .OR. HILEV .GT. M3GRID % NLAYS ) THEN
         MSG = 'ERROR: Invalid HILEV'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( LOLEV .GT. HILEV ) THEN
         MSG = 'ERROR: Inconsistent LOLEV and HILEV'
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      ENDIF

      IF( LERROR ) THEN
         MSG = 'Error in COL/ROW/LEV variable -- stopping'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
      ENDIF


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check time controls
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( SDATE .EQ. 0 ) THEN
         SDATE = STEP_DATE( 1 )
         STIME = STEP_TIME( 1 )
      ENDIF

      LBEG = 0
      DO N = 1, NFLSTEPS
         IF( SDATE .EQ. STEP_DATE( N ) .AND. STIME .EQ. STEP_TIME( N ) )
     &      LBEG = N
      ENDDO

      IF( LBEG .EQ. 0 ) THEN
         MSG = 'ERROR: Start date & time not found on input file(s)'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
      ENDIF

      IF( NSTEPS .EQ. 0 ) THEN
         NSTEPS = NFLSTEPS
      ELSE
         NSTEPS = NSTEPS + 1
         IF( LBEG + NSTEPS .GT. NFLSTEPS ) THEN
            MSG = 'WARNING: Number of output steps exceeds file steps: ' //
     &            'NSTEPS truncated'
            CALL M3MESG( MSG )
            NSTEPS = MIN( NSTEPS, NFLSTEPS )
         ENDIF
      ENDIF
 
      RETURN 

94445 FORMAT(10X, 'ERROR: Requested variable ', A, ' not on file ', A )

      END

