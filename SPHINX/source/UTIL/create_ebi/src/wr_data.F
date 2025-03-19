       SUBROUTINE WR_DATA

C**********************************************************************
C
C  FUNCTION: Create source code for the hrdata module in EBI
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C                    Modified 7/09 to include SSA - J. Gipson
C
C**********************************************************************
      USE ENV_VARS
      USE GLOBAL_DATA
      !!USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA

      IMPLICIT NONE

C..INCLUDES: 

               
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT      ! gets unit no.
!      INTEGER   INDEX1     ! find position of string in list

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_DATA'    ! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open

      INTEGER  :: EPOS         ! end pos of string
      INTEGER  :: IND          ! array index
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file
      INTEGER  :: N            ! Loop index

      LOGICAL  :: LOUT1 = .FALSE.  ! Flag for line 1 output processed
      LOGICAL  :: LOUT2 = .FALSE.  ! Flag for line 2 output processed
      LOGICAL  :: LOUT3 = .FALSE.  ! Flag for line 3 output processed
      LOGICAL  :: LOUT4 = .FALSE.  ! Flag for group 4 output processed
      LOGICAL  :: LOUT5 = .FALSE.  ! Flag for group 5 output processed
      LOGICAL  :: LOUT6 = .FALSE.  ! Flag for line 6 output processed


C**********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      EPOS = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : EPOS ) // '/hrdata_mod.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      EPOS = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : EPOS ) // '/hrdata_mod.F' 

      OPEN( UNIT = IIN, FILE = FNAME, ERR = 9000 )


      IF( LWR_COPY ) CALL WR_COPYRT( IOUT )

      IF( LWR_CVS_HDR ) CALL WR_CVSHDR( IOUT )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read, modify, and write code from template
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  100 CONTINUE

      READ( IIN, 92000, END = 1000 ) LINEIN

      IF( LINEIN( 1 : 1 ) .NE. 'R' ) THEN

         WRITE( IOUT, 92000 ) LINEIN( 1 : LEN_TRIM( LINEIN ) )

      ELSE

         IF( LINEIN( 2 : 2 ) .EQ. '1' .AND. .NOT. LOUT1 ) THEN

            WRITE( IOUT, 93000 ) TRIM( MECHNAME )

            LOUT1 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '2' .AND. .NOT. LOUT2 ) THEN

            WRITE( IOUT, 93020 ) CR_DATE( 1 : LEN_TRIM( CR_DATE ) )

            LOUT2 = .TRUE. 
            
         ELSEIF( LINEIN( 2 : 2 ) .EQ. '3' .AND. .NOT. LOUT3 ) THEN

            IF( LPAR_NEG ) WRITE( IOUT, 93040 ) 

            LOUT3 = .TRUE.

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '5' .AND. .NOT. LOUT5 ) THEN

            IF( LDEGRADE_SUBS ) WRITE( IOUT, 93045 ) LINEIN( 3 : LEN_TRIM( LINEIN ) )

            LOUT5 = .FALSE.

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '6' .AND. .NOT. LOUT6 ) THEN

            WRITE( IOUT, 93082 ) REAL( TIME_STEP,8 )

            LOUT6 = .TRUE. 
            
         END IF

      END IF

      GO TO 100

 1000 CONTINUE


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Generate the species indices
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      WRITE( IOUT, 93050 )

      DO N = 1, N_SPECIES

         EPOS = 16 ! LEN_TRIM( SPECIES( N ) )
           
         WRITE( IOUT, 93081 ) SPECIES( N ) ( 1 : EPOS ), N

      END DO

      WRITE( IOUT, 93140 )

      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrdata_mod.F'

      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )
93040 FORMAT( 6X, 'REAL( 8 ), ALLOCATABLE ::  PNEG(  : )',
     &            '! Negative production rates' )
93050 FORMAT( 'c..Species indices' )
93060 FORMAT( 'c..Gas species indices' )
93080 FORMAT( 6X, 'INTEGER  :: ', A16,' = ',I4 )
93081 FORMAT( 6X, 'INTEGER, PARAMETER :: ', A16,' = ',I4 )
93082 FORMAT( 6X, 'REAL( 8 ), PARAMETER  ::  DELTAT        = ', 1PD9.3, ' ! EBI time step (min)' )
93100 FORMAT( /'c..Aerosol species indices' )
93120 FORMAT( /'c..AQ chemistry species indices' )
93140 FORMAT( /6X, 'END MODULE HRDATA' )
93045 FORMAT(  2X, A )
      END

