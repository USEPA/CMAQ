       SUBROUTINE WR_CALCKS

C**********************************************************************
C
C  FUNCTION: Create source code for the hrcalcks subroutine in EBI
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C
C**********************************************************************
      USE ENV_VARS
      USE GLOBAL_DATA
      !!USE M3UTILIO    ! IOAPI parameters and declarations
      USE RXNS_DATA   ! mechanism data and specifications

      IMPLICIT NONE

C..INCLUDES: 
!         INCLUDE SUBST_IOPARMS   ! IOAPI parameters
!         INCLUDE SUBST_IODECL    ! IOAPI declarations
             
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
      INTEGER   JUNIT          ! gets unit no.

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_CALCKS'  ! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open

      INTEGER  :: EPOS         ! end pos of string
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file

      LOGICAL  :: LOUT1 = .FALSE.  ! Flag for group 1 output processed
      LOGICAL  :: LOUT2 = .FALSE.  ! Flag for group 2 output processed
      LOGICAL  :: LOUT3 = .FALSE.  ! Flag for group 3 output processed

C**********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      EPOS = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : EPOS ) // '/hrcalcks.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      EPOS = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : EPOS ) // '/hrcalcks.F' 

      OPEN( UNIT = IIN, FILE = FNAME, ERR = 9000 )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Start processing
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( LWR_COPY ) CALL WR_COPYRT( IOUT )

      IF( LWR_CVS_HDR ) CALL WR_CVSHDR( IOUT )


  100 CONTINUE

      READ( IIN, 92000, END = 1000 ) LINEIN

      IF( LINEIN( 1 : 1 ) .NE. 'R' ) THEN

         WRITE( IOUT, 92000 ) LINEIN( 1 : LEN_TRIM( LINEIN ) )

      ELSE

         IF( LINEIN( 2 : 2 ) .EQ. '1' .AND. .NOT. LOUT1 ) THEN

            WRITE( IOUT, 93000 ) CR_DATE( 1 : LEN_TRIM( CR_DATE ) )

            LOUT1 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '2' .AND. .NOT. LOUT2 ) THEN

            IF( NSPECIAL .EQ. 0 ) GO TO 100

            WRITE( IOUT, 93100 )

            LOUT2 = .TRUE.

         END IF

      END IF

      GO TO 100

 1000 CONTINUE

      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrcalcks.F'
      

      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )

93000 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )

93100 FORMAT( 'ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc' 
     &       /'c  Set rate constants using concentration-dependent forms to zero'
     &       /'ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
     &       / 1X
     &       /'      DO NRT = 1, NSPECIAL_RXN'
     &       /'         IRXN = ISPECIAL( NRT, 1 )'
     &       /'         RK( NCELL, IRXN ) = 0.0'
     &       /'      END DO'
     &       / 1X )

      END
