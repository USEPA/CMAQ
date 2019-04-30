       SUBROUTINE WR_SOLVER

C**********************************************************************
C
C  FUNCTION: Create source code for the hrsolver subroutine in EBI
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
      !!USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA

      IMPLICIT NONE

C..INCLUDES: 
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT          ! gets unit no.

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_SOLVER'  ! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 132 )  ::    LINEIN               ! Input line
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open

      INTEGER  :: EPOS         ! end pos of string
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file

      LOGICAL  :: LOUT0 = .FALSE.  ! Flag for line 1 output processed
      LOGICAL  :: LOUT1 = .FALSE.  ! Flag for line 1 output processed
      LOGICAL  :: LOUT2 = .FALSE.  ! Flag for line 2 output processed
      LOGICAL  :: LOUT3 = .FALSE.  ! Flag for line 3 output processed
      LOGICAL  :: LOUT4 = .FALSE.  ! Flag for line 4 output processed
      LOGICAL  :: LOUT5 = .FALSE.  ! Flag for line 5 output processed

C**********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      EPOS = LEN_TRIM( OUTPATH )
      
      FNAME = OUTPATH( 1 : EPOS ) // '/hrsolver.F'     

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      EPOS = LEN_TRIM( TMPLPATH )

      IF( N_SS_SPC .EQ. 0 ) THEN
         FNAME = TMPLPATH( 1 : EPOS ) // '/hrsolver.F' 
      ELSE
         FNAME = TMPLPATH( 1 : EPOS ) // '/hrsolver_ss.F' 
      END IF

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

         IF( LINEIN( 2 : 2 ) .EQ. '0' .AND. .NOT. LOUT0 ) THEN

            IF( NSPECIAL .NE. 0 ) WRITE( IOUT, 93029 ) 

            LOUT0 = .TRUE.             

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '1' .AND. .NOT. LOUT1 ) THEN

            WRITE( IOUT, 93000 ) TRIM( MECHNAME )

            LOUT1 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '2' .AND. .NOT. LOUT2 ) THEN

            WRITE( IOUT, 93020 ) CR_DATE( 1 : LEN_TRIM( CR_DATE ) )

            LOUT2 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '3' .AND. .NOT. LOUT3 ) THEN

            IF( NSPECIAL .NE. 0 ) WRITE( IOUT, 93030 ) 

            LOUT3 = .TRUE. 
            
         ELSEIF( LINEIN( 2 : 2 ) .EQ. '4' .AND. .NOT. LOUT4 ) THEN

            IF( LPAR_NEG ) THEN 
               WRITE( IOUT, 93040 ) 
            ELSE
               WRITE( IOUT, 93060 )
            END IF

            LOUT3 = .TRUE.

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '5' .AND. .NOT. LOUT5 ) THEN

            IF( LDEGRADE_SUBS ) WRITE( IOUT, 93046 ) LINEIN( 3 : LEN_TRIM( LINEIN ) )

            LOUT5 = .FALSE.

         END IF

      END IF

      GO TO 100

 1000 CONTINUE

      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrsolver.F'

      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )
93029 FORMAT( 6X,'USE RXNS_FUNCTION, ONLY : SPECIAL_RATES' )
93030 FORMAT( 2/15X, 'FORALL( S=1:N_SPEC )SYC( NCELL, S ) = YC( S ) '
     &        /15X, 'CALL SPECIAL_RATES( NUMCELLS=NCELL, TEMP=TEMP, DENS=DENSITY, ',
     &         'Y=SYC, RKI=RKI_SAV )'
     &        /15X, 'FORALL( S = 1:NRXNS )RKI( S ) = RKI_SAV( NCELL, S )' / )
93040 FORMAT( 'c..Special treatment of PAR because of negative product ',
     &        'stoichiometry' /
     &        15X,'IF( PNEG( PAR ) .GT. 0.0D0 ) THEN' /
     &        15X,'   FXDLOSS = PNEG( PAR ) * DTC' /
     &        15X,'   IF( FXDLOSS .GE. YC0( PAR ) + PROD( PAR ) * DTC ) THEN' /
     &        15X,'      YCP( PAR ) = 0.0D0' /
     &        15X,'   ELSE' /
     &        15X,'      VARLOSS = MAX( LOSS( PAR ) - PNEG( PAR ) , ZERO )' /
     &        15X,'      YCP( PAR ) = ( YC0( PAR ) + PROD( PAR ) * DTC  - ' /
     &         5X,'&', 13X, 'FXDLOSS ) / ( 1.0D0 + VARLOSS * DTC / YC( PAR ) )' /
     &        15X,'   END IF' /
     &        15X,'END IF' )
!93046 FORMAT(  2X, A )
93046 FORMAT(  A )
93060 FORMAT( / )

      END
