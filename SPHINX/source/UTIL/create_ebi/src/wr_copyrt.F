       SUBROUTINE WR_COPYRT( IOUT )

C**********************************************************************
C
C  FUNCTION: Create source code for the driver for the EBI solver
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C
C**********************************************************************
      USE ENV_VARS
      !!USE M3UTILIO ! IOAPI parameters and declarations

      IMPLICIT NONE

C..INCLUDES: 
      
C..ARGUMENTS:
      INTEGER  :: IOUT         ! Unit no. of output file

C..PARAMETERS:
      INTEGER, PARAMETER ::  MXLINES = 50        ! Max no of copyright lines

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT          ! gets unit no.

C..SAVED LOCAL VARIABLES:
      CHARACTER( 100 ), SAVE    :: COPYRT( MXLINES )   ! Copyright text

      INTEGER, SAVE             :: NLIN                ! No. of copyright lines

      LOGICAL, SAVE             :: LFIRST = .TRUE.  
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_COPYRT'  ! Program name
      CHARACTER(  16)   ::    MNAME                ! Mech name to write
      CHARACTER(  16 )  ::    MOUT                 ! Mech name for putput
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open

      INTEGER  :: E1           ! end pos of string
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: N            !


C**********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and driver template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LFIRST ) THEN
 
         IIN = JUNIT()

         E1 = LEN_TRIM( TMPLPATH )

         FNAME = TMPLPATH( 1 : E1 ) // '/copyright.txt' 

         OPEN( UNIT = IIN, FILE = FNAME, ERR = 9000 )

         NLIN = 0

  100    CONTINUE

         READ( IIN, 92000, END = 9000 ) LINEIN

         NLIN = NLIN + 1

         IF( NLIN .GE. MXLINES ) THEN
            MSG = 'ERROR: Max no. of copyright lines exceeded;' //
     &            ' it will not be written'
            WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
            NLIN = 0
            GO TO 9000
         END IF

         COPYRT( NLIN ) = LINEIN

         GO TO 100

 9000    CONTINUE

         LFIRST = .FALSE.

         CLOSE( IIN )

      END IF

      IF( NLIN .GT. 0  ) THEN

         DO N = 1, NLIN

            E1 = LEN_TRIM( COPYRT( N ) )     
 
            WRITE( IOUT, 92000 )  COPYRT( N )( 1 : E1 )
 
         END DO

         WRITE( IOUT, 92000 ) 

      END IF

      RETURN

92000 FORMAT( A )

      END 
