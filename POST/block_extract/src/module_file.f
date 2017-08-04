      MODULE M3FILES

C*************************************************************************
C
C  FUNCTION:  To set-up file data
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C
C*************************************************************************

      INTEGER N_M3FILES                     ! No. of input Models-3 files

      INTEGER, PARAMETER :: MXM3FLS = 99    ! Max no. of input

      CHARACTER*16  M3_FLNAME( MXM3FLS )    ! Names of Models-3 file



      CONTAINS


         SUBROUTINE OPEN_M3FILES
C*************************************************************************
C
C  FUNCTION:  To provide site data
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C
C*************************************************************************
         USE M3UTILIO
         USE ENV_VARS

         IMPLICIT NONE

C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    ENV_DFLT     ! Environment variable default value
         CHARACTER*16    FL_NAME      ! Input Models-3 file name
         CHARACTER*16    PNAME        ! Program Name
         CHARACTER*80    ENV_DESC     ! Environment variable description
         CHARACTER*80    MSG          ! Error message
         CHARACTER*256   RET_VAL      ! Returned value of environment variable

         INTEGER   N                  ! Loop index
         INTEGER   STATUS             ! Status code

C**********************************************************************
         DATA  PNAME       / 'OPEN_M3FILES'  /

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine the number of input CTM conc files that need to be read
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         N_M3FILES = 0

         WRITE( LOGUNIT, 92020 )

         DO N = 1, MXM3FLS

            IF( N .LT. 10 ) THEN
               WRITE( FL_NAME, '( ''M3_FILE_'', I1 )' ) N
               WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I1 )' ) N
            ELSE
               WRITE( FL_NAME, '( ''M3_FILE_'', I2 )' ) N
               WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I2 )' ) N
            ENDIF

            ENV_DFLT = ' '
            CALL ENVSTR( FL_NAME, ENV_DESC, ENV_DFLT, RET_VAL, STATUS)

            IF( STATUS .EQ. 0 ) THEN
               N_M3FILES = N_M3FILES + 1
               M3_FLNAME( N_M3FILES ) = FL_NAME
            ENDIF

         ENDDO

         IF( N_M3FILES .EQ. 0 ) THEN
            MSG = 'No CTM CONC files found'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO N = 1, N_M3FILES
            IF( .NOT. OPEN3( M3_FLNAME( N ), FSREAD3, PNAME) )THEN
               CALL M3EXIT( PNAME, 0 , 0,
     &              'Could not open input file ' // M3_FLNAME( N ),
     &               XSTAT1 )
            ENDIF
         ENDDO

         RETURN

C******************  FORMAT  STATEMENTS   ******************************

92020  FORMAT( //5X, A, // )

         END SUBROUTINE OPEN_M3FILES


      END MODULE M3FILES
