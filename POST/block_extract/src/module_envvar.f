      MODULE ENV_VARS

C*************************************************************************
C
C  FUNCTION:  To get environment variables
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C  Modified by Chris Nolte December 2008 to add ALLVARS flag
C
C*************************************************************************

      INTEGER       LOGUNIT         ! Output log unit number

      INTEGER       LOCOL           ! Lowest column of block
      INTEGER       LOROW           ! Lowest row of block
      INTEGER       LOLEV           ! Lowest level of block

      INTEGER       HICOL           ! Highest column of block
      INTEGER       HIROW           ! Highest row of block
      INTEGER       HILEV           ! Highest level of block

      INTEGER       NVAR            ! Number of variables to output

      INTEGER       SDATE           ! Start date
      INTEGER       STIME           ! Start time

      INTEGER       NSTEPS          ! No. of steps to output

c      CHARACTER*16  SUMORAVG        ! Output sums or avvgs ( default=avg)

      CHARACTER*16  OUTFORMAT       ! Output time format

      CHARACTER*16  TIME_CONV       ! Time zone to convert to

      CHARACTER*256 OUT_FNAME       ! Name of output data file

      CHARACTER*16, ALLOCATABLE :: VNAME( : ) ! Variable names

      LOGICAL       ALLVARS         ! flag if output all vars--specify 'ALL' in specfile

      CONTAINS

         SUBROUTINE GET_ENVS
C*************************************************************************
C
C  FUNCTION:  To get environment variables
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C
C*************************************************************************
         USE M3UTILIO
         IMPLICIT NONE

C..ARGUMENTS: None

C..PARAMETERS:
         INTEGER    N_VALTZ
         PARAMETER( N_VALTZ = 2 )

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    ENV_DFLT         ! Environment variable default value
         CHARACTER*16    OUTFILE          ! Env var for output file name
         CHARACTER*16    PNAME            ! Program Name
         CHARACTER*80    ENV_DESC         ! Environment variable description
         CHARACTER*80    MSG              ! Error message
         CHARACTER*256   RET_VAL          ! Returned value of env var
         CHARACTER*9     FORMAT

         CHARACTER*3     VALTZ( N_VALTZ )      ! Valid time zone codes

         INTEGER   N                      ! Loop index
         INTEGER   STATUS                 ! Status code

         LOGICAL   LERROR                 ! Error flag

C**********************************************************************
         DATA  PNAME           / 'GET_ENVS'        /
         DATA  OUTFILE         / 'OUTFILE'         /
         DATA  FORMAT          / 'OUTFORMAT' /
         DATA VALTZ / 'GMT', 'EST' /

         LOGUNIT = INIT3()

         LERROR = .FALSE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the variables to retrieve
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ALLOCATE ( VNAME( MXVARS3 ) )

         OPEN( UNIT = 9, FILE='specfile' )

         DO N = 1, MXVARS3
            READ( 9, '( A )', END = 100 ) VNAME( N )
            NVAR = NVAR + 1
         ENDDO

  100    CONTINUE

         CLOSE( 9 )

         ALLVARS = ( VNAME( 1 ) .EQ. 'ALL' )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the start date and time
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         SDATE = 0
         ENV_DESC = 'Start date for output'
         SDATE = ENVINT( 'SDATE', ENV_DESC, SDATE, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: SDATE not specified; using file(s) start date'
            CALL M3MESG( MSG )
         ENDIF

         STIME = 0
         ENV_DESC = 'Start time for output'
         STIME = ENVINT( 'STIME', ENV_DESC, STIME, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: STIME not specified; using 000000'
            CALL M3MESG( MSG )
         ENDIF

         NSTEPS = 0
         ENV_DESC = 'Start time for output'
         NSTEPS = ENVINT( 'NSTEPS', ENV_DESC, NSTEPS, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: NSTEPS not specified; using file end time'
            CALL M3MESG( MSG )
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the block definition
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LOCOL = 0
         ENV_DESC = 'Lowest column number'
         LOCOL = ENVINT( 'LOCOL', ENV_DESC, LOCOL, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: LOCOL not specified; will be set to 1'
            CALL M3MESG( MSG )
         ENDIF

         HICOL = 0
         ENV_DESC = 'Highest column number'
         HICOL = ENVINT( 'HICOL', ENV_DESC, HICOL, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: HICOL not specified; will be set to NCOLS'
            CALL M3MESG( MSG )
         ENDIF

         LOROW = 0
         ENV_DESC = 'Lowest row number'
         LOROW = ENVINT( 'LOROW', ENV_DESC, LOROW, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: LOROW not specified; will be set to 1'
            CALL M3MESG( MSG )
         ENDIF

         HIROW = 0
         ENV_DESC = 'Highest row number'
         HIROW = ENVINT( 'HIROW', ENV_DESC, HIROW, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: HIROW not specified; will be set to NROWS'
            CALL M3MESG( MSG )
         ENDIF


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the levels to use
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LOLEV = 0
         ENV_DESC = 'Lowest level number'
         LOLEV = ENVINT( 'LOLEV', ENV_DESC, LOLEV, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: LOLEV not specified; wuill be set to 1'
            CALL M3MESG( MSG )
         ENDIF

         HILEV = 0
         ENV_DESC = 'Highest level'
         HILEV = ENVINT( 'HILEV', ENV_DESC, HILEV, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: HILEV not specified; will be set to NLAYS'
            CALL M3MESG( MSG )
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = 'OUTFILE'
         ENV_DESC = 'Output file name'
         CALL ENVSTR( OUTFILE, ENV_DESC, ENV_DFLT, OUT_FNAME, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no file name assigned to OUTFILE'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time convention to use on output
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         TIME_CONV = 'GMT'
         ENV_DFLT  = 'GMT'
         ENV_DESC  = 'Time convention to use for output data'
         CALL ENVSTR( 'TIME_ZONE', ENV_DESC, ENV_DFLT, TIME_CONV, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: No time zone assigned; using default GMT'
            CALL M3MESG( MSG )
         ENDIF

         IF( INDEX1( TIME_CONV, N_VALTZ, VALTZ ) .EQ. 0 ) THEN
            MSG = 'ERROR: Invalid time zone for output file'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the format to use for output
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT  = 'IOAPI'
         ENV_DESC  = 'Time format to use for output data'

         CALL ENVSTR( FORMAT, ENV_DESC, ENV_DFLT, RET_VAL, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: No output time format assigned; using ' //
     &            ' IOAPI convention'
            CALL M3MESG( MSG )
            OUTFORMAT = 'IOAPI'
         ELSE
            OUTFORMAT = RET_VAL
         ENDIF

         IF( OUTFORMAT .NE. 'SAS' .AND. OUTFORMAT .NE. 'IOAPI') THEN
            MSG = 'WARNING: Invalid output time format requested: ' //
     &            'using IOAPI convention'
            CALL M3MESG( MSG )
            OUTFORMAT = 'IOAPI'
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get sum or avg flag
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         ENV_DFLT  = 'AVG'
c         ENV_DESC  = 'Flag to output sum or average'
c         CALL ENVSTR( 'SUMORAVG', ENV_DESC, ENV_DFLT, RET_VAL, STATUS)

c         IF( STATUS .NE. 0 ) THEN
c            MSG = 'WARNING: Sums or averages not specified; averages ' //
c     &            ' output'
c            CALL M3MESG( MSG )
c            SUMORAVG = 'AVG'
c         ELSE
c            SUMORAVG = RET_VAL
c         ENDIF

c         IF( SUMORAVG .NE. 'AVG' .AND. SUMORAVG .NE. 'SUM') THEN
c            MSG = 'WARNING: Invalid SUMORAVG value: using AVG'
c            CALL M3MESG( MSG )
c            SUMORAVG = 'AVG'
c         ENDIF


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Stop if errors detected
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         IF( LERROR ) THEN
            MSG = 'Error in assigning environment variables -- stopping'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

         RETURN

         END SUBROUTINE GET_ENVS

      END MODULE ENV_VARS
