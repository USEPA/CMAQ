
      PROGRAM BLOCK_EXTRACT

C**********************************************************************
C
C  FUNCTION: To get time series of average values of user 
C            specified variables
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, September, 2000
C
C**********************************************************************
      USE M3UTILIO
      USE ENV_VARS
      USE M3FILES
      USE GRID_DATA
      USE TIME_STEP


      IMPLICIT NONE 
      
C..ARGUMENTS:

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message

      INTEGER   N                  ! Loop index

C**********************************************************************
      DATA  PNAME       / 'BLOCK_EXTRACT'  /

      CALL GET_ENVS

      CALL OPEN_M3FILES

      CALL CK_M3FLS

      CALL GET_M3GRID


      CALL GET_TSTEPS

      CALL CKSETUP

      CALL PROCESS
   
      CALL M3EXIT( PNAME, 0, 0,
     &            'Program ' // PNAME // ' completed successfully', 0 )



C******************  FORMAT  STATEMENTS   ******************************



       END
       
       
