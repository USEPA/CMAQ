       SUBROUTINE WR_GP3

C**********************************************************************
C
C  FUNCTION: Create source code for the hrgp3 subroutine in EBI
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, March, 2004
C
C**********************************************************************
      USE ENV_VARS
      USE GLOBAL_DATA
      !!USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA

      IMPLICIT NONE

C..INCLUDES: 
      
C..ARGUMENTS: None

C..PARAMETERS:
      INTEGER, PARAMETER   ::  GRPNO = 3

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT      ! gets unit no.
       INTEGER   INDEX1     ! find position of string in list

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_GP3'     ! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER(  CL )  ::    SPOUT                ! Ouput species
      CHARACTER(  16 )  ::    SPEC     
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open
      CHARACTER(  72 )  ::    CLINE                ! Line of c's
      CHARACTER( 256 )  ::    LINOUT
      CHARACTER( 150 )  ::    RXOUT
      CHARACTER( 100 )  ::    BLANK_LINE
      CHARACTER*(  5 )  ::    RNUM                 ! Reaction number
      CHARACTER*(  6 )  ::    COUT                 ! Output coefficient
      CHARACTER*(  1 )  ::    SGN                  ! Coefficient sign
   

      INTEGER  :: E1, E2       ! end pos of string
      INTEGER  :: IND          ! array index
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file
      INTEGER  :: N, S, P, R   ! Loop indices
      INTEGER  :: NR           ! No. of reactants
      INTEGER  :: NPOS         ! Reaction index
      INTEGER  :: RPOS1        !
      INTEGER  :: RPOS2        !
      INTEGER  :: PPOS1        !
      INTEGER  :: PPOS2        !

      LOGICAL  :: LFIRST
      LOGICAL  :: LRXN1
      LOGICAL  :: LFIRST_R3 = .TRUE.   

      REAL( 8 ) :: COEFF
      REAL( 8 ) :: RPAN
      REAL( 8 ) :: PPAN
      REAL( 8 ) :: RC2O3
      REAL( 8 ) :: PC2O3

      
C**********************************************************************

      DO N = 1, 72
        CLINE( N : N ) = 'c'
      END DO

      DO N = 1, 100
        BLANK_LINE( N : N ) = ' '
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      E1 = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : E1 ) // '/hrg3.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      E1 = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : E1 ) // '/hrg3.F' 

      OPEN( UNIT = IIN, FILE = FNAME, ERR = 9000 )


      IF( LWR_COPY ) CALL WR_COPYRT( IOUT )

      IF( LWR_CVS_HDR ) CALL WR_CVSHDR( IOUT )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read, modify, and write 1st section of code from template
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  100 CONTINUE

      READ( IIN, 92000, END = 1000 ) LINEIN

      IF( LINEIN( 1 : 2 ) .EQ. 'R1' ) THEN

         WRITE( IOUT, 93000 ) TRIM( MECHNAME )

         GO TO 100

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R2' ) THEN

         WRITE( IOUT, 93020 ) CR_DATE( 1 : LEN_TRIM( CR_DATE ) )

         GO TO 100

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R3' ) THEN

         IF( LFIRST_R3 ) THEN

            SPOUT = SPECIES( C2O3 )
            CALL LCASE( SPOUT )
            E1 = LEN_TRIM( SPOUT )
            LINOUT = '      REAL( 8 ) ::   K8_8         ! K' //
     &         SPOUT( 1 : E1 ) // '+' //  SPOUT( 1 : E1 ) // ' * delta t'
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

          
            SPOUT = SPECIES( PAN )
            CALL LCASE( SPOUT )
            E1 = LEN_TRIM( SPOUT )
            LINOUT = '      REAL( 8 ) ::   R8_9         ! K' // SPOUT( 1 : E1 ) //
     &                 '-->'
            E1 = LEN_TRIM( LINOUT )
            SPOUT = SPECIES( C2O3 )
            CALL LCASE( SPOUT )
            E2 = LEN_TRIM( SPOUT )
            LINOUT = LINOUT( 1 : E1 ) // SPOUT( 1 : E2 ) //
     &          ' * delta t'
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )
             

            SPOUT = SPECIES( C2O3 )
            CALL LCASE( SPOUT )
            E1 = LEN_TRIM( SPOUT )
            LINOUT = '      REAL( 8 ) ::   R9_8         ! K' // SPOUT( 1 : E1 ) //
     &                 '+'
            E1 = LEN_TRIM( LINOUT )
            SPOUT = SPECIES( no2 )
            CALL LCASE( SPOUT )
            E2 = LEN_TRIM( SPOUT )
            LINOUT = LINOUT( 1 : E1 ) // SPOUT( 1 : E2 ) // '-->'
            E1 = LEN_TRIM( LINOUT )
            SPOUT = SPECIES( PAN )
            CALL LCASE( SPOUT )
            E2 = LEN_TRIM( SPOUT )
            LINOUT = LINOUT( 1 : E1 ) // SPOUT( 1 : E2 ) // 
     &         ' * [NO2] * delta t'
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LFIRST_R3 = .FALSE.

         END IF



         GO TO 100

            
      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'S1' ) THEN

         GO TO 1000

      ELSE

         WRITE( IOUT, 92000 ) LINEIN( 1 : LEN_TRIM( LINEIN ) )

         GO TO 100

      END IF

 1000 CONTINUE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  P8 production section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    P8 includes C2O3 production from all reactions except PAN=C2O3    
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      E1 = LEN_TRIM( SPECIES( C2O3 ) )
      E2 = LEN_TRIM( SPECIES( PAN ) )
      WRITE( IOUT, 92000 )
      WRITE( IOUT, 94000 ) SPECIES( C2O3 )( 1 : E1 ), SPECIES( PAN )( 1 : E2 )

c..Determine the reactions to include & get coefficients for the prod terms
      LRXN1 = .TRUE.
      DO N = 1, NRXNS
         COEFF = 0.0D0
         CALL SUM_COEFF( RC2O3, 'R', C2O3, N )
         CALL SUM_COEFF( PC2O3, 'P', C2O3, N )

         IF( PC2O3 .LE. RC2O3 ) CYCLE                ! Skip rxns with Pc2o3=0

                                                     ! Skip PAN=C2O3 Rxn
         IF( IRR( N, 1 ) .EQ. PAN .AND. IRR( N, 4 ) .EQ. C2O3 .OR.
     &       IRR( N, 1 ) .EQ. PAN .AND. IRR( N, 5 ) .EQ. C2O3 )
     &     CYCLE          

      

         COEFF = PC2O3 - RC2O3                       ! Rxns w/ Pc2o3>0

c..call routine to create output line & write it
         NPOS = 30
         RPOS1 = 0
         RPOS2 = 0
         PPOS1 = C2O3
         PPOS2 = 0
         CALL BLD_OUTLINE( 'RXRAT', 'P8', '   ', 0, COEFF, N, GRPNO,  
     &        NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

         LRXN1 = .FALSE.

         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

      END DO


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  L8 computation ( Loss of C2O3 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C    L8 includes the following C2O3 loss terms:
c      a) all reactions in which C2O3 is lost except C2O3+C2O3
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SPOUT = SPECIES( C2O3 )
      E1 = LEN_TRIM( SPOUT )
      WRITE( IOUT, 92000 )
      WRITE( IOUT, 94020 ) SPOUT( 1 : E1 ), SPOUT( 1 : E1 ), SPOUT( 1 : E1 )
      LRXN1 = .TRUE.
      DO N = 1, NRXNS         

c..Determine the rxn to include & get the coefficient for the loss term
         COEFF = 0.0D0
         CALL SUM_COEFF( RC2O3, 'R', C2O3,  N )
         CALL SUM_COEFF( PC2O3, 'P', C2O3,  N )

         IF( RC2O3 .LE. PC2O3 ) CYCLE          ! Skip rxns w/ Lc2o3=0
 

         ! Skip C2O3+C2O3 rxn
         IF( IRR( N, 1 ) .EQ. C2O3 .AND. IRR( N, 2 ) .EQ. C2O3 ) CYCLE
        
         COEFF = RC2O3 - PC2O3

c..call routine to create output line & write it
         NPOS  = 20
         RPOS1 = C2O3
         RPOS2 = 0
         PPOS1 = 0
         PPOS2 = 0
         CALL BLD_OUTLINE( 'LFREQ', 'L8', 'C2O3', C2O3, COEFF, N, GRPNO,  
     &        NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

         LRXN1 = .FALSE.

         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

      END DO
       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  L9 computation ( Loss of PAN )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    L9 includes all reactions in which PAN is lost
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SPOUT = SPECIES( PAN )
      E1 = LEN_TRIM( SPOUT )
      WRITE( IOUT, 92000 )
      WRITE( IOUT, 94040 ) SPOUT( 1 : E1 )
      LRXN1 = .TRUE.
      DO N = 1, NRXNS         

c..Determine the rxn to include & get the coefficient for the loss term
         COEFF = 0.0D0
         CALL SUM_COEFF( RPAN, 'R', PAN,  N )
         CALL SUM_COEFF( PPAN, 'P', PAN,  N )

         IF( RPAN .LE. PPAN ) CYCLE          ! Skip rxns w/ Lc2o3=0
         
         COEFF = RPAN - PPAN

c..call routine to create output line & write it
         NPOS  = 20
         RPOS1 = PAN
         RPOS2 = 0
         PPOS1 = 0
         PPOS2 = 0
         CALL BLD_OUTLINE( 'LFREQ', 'L9', 'PAN', PAN, COEFF, N, GRPNO,  
     &        NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

         LRXN1 = .FALSE.

         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

      END DO


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  R8_8, R8_9, and R9_8 terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 92000 )      
      WRITE( IOUT, 94060 )

c..K8,8 term ( C2O3+C2O3=)
!      DO N = 1, NRXNS
!         IF( IRR( N, 1 ) .EQ. C2O3 .AND.  IRR( N, 2 ) .EQ. C2O3 ) THEN
!            WRITE( RNUM, '( I5 )' ) N
!            LINOUT = '      K8_8  = RKI( ' // RNUM // ' ) * DTC'
!            E1 = LEN_TRIM( LINOUT )
!            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )
!         END IF
!      END DO
      IF( RXN_C2O3_SELF .GT. 0 )THEN
         WRITE( RNUM, '( I5 )' ) RXN_C2O3_SELF
         LINOUT = '      K8_8  = RKI( ' // RNUM // ' ) * DTC'
         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )
      ELSE
         LINOUT = '      K8_8  = 0.0D0'
         WRITE( IOUT, 92000 ) TRIM( LINOUT )      
      END IF

c..R8,9 term ( production of C2O3 fro PAN )
      LRXN1 = .TRUE.
      DO N = 1, NRXNS
         IF( IRR( N, 1 ) .EQ. PAN .AND. IRR( N, 4 ) .EQ. C2O3 .OR.
     &       IRR( N, 1 ) .EQ. PAN .AND. IRR( N, 5 ) .EQ. C2O3 ) THEN
            WRITE( RNUM, '( I5 )' ) N
c            LINOUT = '      R8_9  = RKI( ' // RNUM // ' ) * DTC'
            IF( LRXN1 )THEN
                 LINOUT = '      R8_9  = ( RKI( ' // RNUM // ' ) '
                 LRXN1  = .FALSE.
            ELSE
                 LINOUT = '     &      +   RKI( ' // RNUM // ' ) '
            END IF
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92040, ADVANCE= 'NO' ) LINOUT( 1 : E1 )
         END IF
      END DO
      WRITE( IOUT, 92060)


c..R9,8 term ( production of PAN from C2O3 )
      LRXN1 = .TRUE.
      DO N = 1, NRXNS
         IF( ( IRR( N, 1 ) .EQ. C2O3 .AND. IRR( N, 2 ) .EQ. NO2  .AND.
     &         IRR( N, 4 ) .EQ. PAN ) .OR.
     &       ( IRR( N, 1 ) .EQ. NO2  .AND. IRR( N, 2 ) .EQ. C2O3 .AND.
     &         IRR( N, 4 ) .EQ. PAN    ) ) THEN
            WRITE( RNUM, '( I5 )' ) N
            SPOUT = SPECIES( NO2 )
            E2 = LEN_TRIM( SPOUT )
            IF( LRXN1 )THEN
                LINOUT = '      R9_8  = ( RKI( ' // RNUM // ' ) * YC( ' //
     &          SPOUT( 1 : E2 ) // ' ) '
                LRXN1  = .FALSE.
            ELSE
                LINOUT = '     &      +   RKI( ' // RNUM // ' ) * YC( ' //
     &          SPOUT( 1 : E2 ) // ' ) '
            END IF
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92040, ADVANCE= 'NO' ) LINOUT( 1 : E1 )
         END IF
      END DO
      WRITE( IOUT, 92060)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Continue reading template section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..finish reading skipped section of template (i.e., code between S1 markers)
  200 CONTINUE

      READ( IIN, 92000, END = 300 ) LINEIN

      IF( LINEIN( 1 : 2 ) .EQ. 'S1' ) GO TO 300

      GO TO 200

  300 CONTINUE


      
  400 CONTINUE

      READ( IIN, 92000, END = 2000 ) LINEIN

      IF( LINEIN( 1 : 2 ) .EQ. 'R4' ) THEN

         WRITE( IOUT, 95000) SPECIES( C2O3 )( 1 : LEN_TRIM( SPECIES( C2O3 ) ) ),
     &                       SPECIES( PAN  )( 1 : LEN_TRIM( SPECIES( PAN  ) ) )

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R5' ) THEN

         WRITE( IOUT, 95020) SPECIES( C2O3 )( 1 : LEN_TRIM( SPECIES( C2O3 ) ) ),
     &                       SPECIES( PAN  )( 1 : LEN_TRIM( SPECIES( PAN  ) ) )


      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R6' ) THEN
 
         WRITE( IOUT, 95040) SPECIES( C2O3 )( 1 : LEN_TRIM( SPECIES( C2O3 ) ) )

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R7' ) THEN
 
         WRITE( IOUT, 95060) SPECIES( PAN  )( 1 : LEN_TRIM( SPECIES( PAN  ) ) ),
     &                       SPECIES( PAN  )( 1 : LEN_TRIM( SPECIES( PAN  ) ) ),
     &                       SPECIES( C2O3 )( 1 : LEN_TRIM( SPECIES( C2O3 ) ) )

      ELSE

         WRITE( IOUT, 92000 ) LINEIN( 1 : LEN_TRIM( LINEIN ) )

      END IF

      GO TO 400
 
2000  CONTINUE
         

        
      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrg3.F'

      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )
92020 FORMAT( / )
92040 FORMAT( /, A )
92060 FORMAT(' ) * DTC ')

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )

94000 FORMAT( 
     & 'c..Production of ', A, ' (except from ', A, ' )' )

94020 FORMAT( 
     & 'c..Loss frequency of ', A, ' ( not including ', A, 
     & ' + ', A, ' )' )

94040 FORMAT( 
     & 'c..Loss frequency of ', A ) 


94060 FORMAT(
     & 'c..K8_8, R8_9, and R9_8 terms' )

95000 FORMAT( 
     & 'c..Solution of quadratic equation to get ', A, ' & ', A )

95020 FORMAT( 
     & '      C = CMN * ( YC0( ', A, ' ) + P8 * DTC ) + ',
     & ' R8_9 * YC0( ', A, ' )' )

95040 FORMAT( 
     & '      YCP( ', A, ' ) = MAX( Q / A , -C / Q  )' )

95060 FORMAT( 
     & '      YCP( ', A, ' ) = ( YC0( ', A, ' ) +  R9_8 * ',
     & 'YCP( ', A, ' ) ) / CMN' )

      END


