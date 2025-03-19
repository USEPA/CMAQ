       SUBROUTINE SUM_COEFF( SUMC, RORP, SPINDX, NRX  )

C**********************************************************************
C
C  FUNCTION: Create source code for the hrgp1 subroutine in EBI
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
!      !!USE M3UTILIO    ! IOAPI parameters and declarations
      USE RXNS_DATA   ! mechanism data and specifications

      IMPLICIT NONE

C..INCLUDES: 

               
      
C..ARGUMENTS:
      CHARACTER( * )    :: RORP    ! Reactant or Product code

      INTEGER           :: SPINDX  ! Index of species
      INTEGER           :: NRX     ! Reaction number

      REAL( 8 )         :: SUMC    ! Sum of species coefficients

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      CHARACTER(  16 ), SAVE   ::    PNAME = 'SUM_COEFF'    ! Program name
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 256 )  ::    MSG            ! Message text

       INTEGER          ::    P, R           ! Loop indices

   


C**********************************************************************


      SUMC = 0.0D0

      IF( RORP .EQ. 'R' ) THEN

         DO R = 1, NREACT( NRX )
            IF( IRR( NRX, R ) .EQ. SPINDX )SUMC = SUMC + 1.0D0
         END DO

         RETURN

      ELSEIF( RORP .EQ. 'P' ) THEN

         DO P = 1, NPRDCT( NRX )
            IF( IRR( NRX, P + MXRCT ) .EQ. SPINDX ) SUMC = SUMC + 
     &          SC( NRX, P )
         END DO

         RETURN

      ELSE

         MSG = 'ERROR: Invalid code for SUM_COEFF - must be R or P '

         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         STOP

      END IF

      END     
        
      SUBROUTINE CMPRS_COEF( COUT, SGN, COEFF )

C**********************************************************************
C
C  FUNCTION: Convert a coeff to compressed characters ( blank if 1.0 )
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
      USE RXNS_DATA   ! mechanism data and specifications


      IMPLICIT NONE

C..INCLUDES: 

               
      
C..ARGUMENTS:
      CHARACTER( * )    :: COUT    ! Reactant or Product code
      CHARACTER( * )    :: SGN     ! Sign of coefficient

      REAL( 8 )         :: COEFF   ! Coefficient


C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      CHARACTER(  16 ), SAVE   ::    PNAME = 'CMPRS_COEF'    ! Program name
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 256 )  ::    MSG            ! Message text

       INTEGER          ::    N           ! Loop index

   


C**********************************************************************

      IF( COEFF .GE. 0.0D0 ) THEN
         SGN = '+'
      ELSE
         SGN = '-'
         COEFF = -1.0D0 * COEFF
      END IF

      COUT = '               '

      IF( COEFF .EQ. 1.0D0 ) RETURN

      WRITE( COUT, '( D12.4 ) ' ) COEFF

      DO N = LEN( COUT ), 1, -1

        IF( COUT( N : N ) .EQ. ' ' ) CYCLE
!        IF( COUT( N : N ) .EQ. '0' .OR. COUT( N : N ) .EQ. '.' ) THEN
!           COUT( N : N ) = ' '
!        ELSE
!           EXIT
!        END IF

      END DO     

      COUT = ADJUSTL( COUT )

      RETURN

      END



      SUBROUTINE CMPRS_RXN( RPOS1, RPOS2, PPOS1, PPOS2, IRX, RXOUT )

C**********************************************************************
C
C  FUNCTION: Create a compressed mechanism reaction line with products
C            in specified order; 
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
      !!!!USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA   ! mechanism data and specifications


      IMPLICIT NONE

C..INCLUDES: 
!      INCLUDE SUBST_IOPARMS          ! IOAPI parameters
!      INCLUDE SUBST_IODECL           ! IOAPI declarations
                 
      
C..ARGUMENTS:
      CHARACTER( * )    :: RXOUT     ! Compressed reaction line

      INTEGER           :: RPOS1     ! Species to occupy rctnt position 1
      INTEGER           :: RPOS2     ! Species to occupy rctnt position 2
      INTEGER           :: PPOS1     ! Species to occupy prod position 1
      INTEGER           :: PPOS2     ! Species to occupy prod position 2
      INTEGER           :: IRX       ! Reaction index


C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      CHARACTER(  16 ), SAVE   ::    PNAME = 'SUM_COEFF'    ! Program name
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 256 )  ::  MSG            ! Message text
      CHARACTER(  16 )  ::  SAVSPEC
      CHARACTER*( 12 )  ::  COUT                 ! Output coefficient
      CHARACTER*(  1 )  ::  SGN                  ! Coefficient sign

      INTEGER           ::  EPOS         ! end pos of string
      INTEGER           ::  EP           ! end pos of character string
      INTEGER           ::  P, R           ! Loop index
      INTEGER           ::  IND
      INTEGER           ::  LEN_RXOUT    ! length of RXOUT string
      INTEGER           ::  SAVINDX      ! Saved species index

      REAL( 8 )         ::  SAVCOEF          ! Saved coefficient

   
      CHARACTER( 16 )   ::  RSPEC( MXRCT )
      CHARACTER( 16 )   ::  PSPEC( MXPRD )
      INTEGER           ::  RINDX( MXRCT )
      INTEGER           ::  PINDX( MXPRD )
      REAL( 8 )         ::  RCOEF( MXRCT )
      REAL( 8 )         ::  YCOEF( MXPRD )

C**********************************************************************

c..determine length of RXOUT string

      LEN_RXOUT = LEN( RXOUT )

c..Load reactant species data into work arrays
      DO R = 1, NREACT( IRX )
         RSPEC( R ) = SPECIES( IRR( IRX, R ) )
         RINDX( R ) = IRR( IRX, R )
      END DO


c..Shuffle reactant positions if necessary
      IF( RPOS1 .NE. 0 ) THEN
         DO R = 2, NREACT( IRX )
            IF( RINDX( R ) .EQ. RPOS1 ) THEN
               SAVSPEC = RSPEC( 1 )
               SAVINDX = RINDX( 1 )
               RSPEC( 1 ) = RSPEC( R )
               RINDX( 1 ) = RINDX( R )
               RSPEC( R ) = SAVSPEC
               RINDX( R ) = SAVINDX
               EXIT
            END IF
         END DO
      END IF

      IF( RPOS2 .NE. 0 ) THEN
         DO R = 3, NREACT( IRX )
            IF( RINDX( R ) .EQ. RPOS2 ) THEN
               SAVSPEC = RSPEC( 2 )
               SAVINDX = RINDX( 2 )
               RSPEC( 2 ) = RSPEC( R )
               RINDX( 2 ) = RINDX( R )
               RSPEC( R ) = SAVSPEC
               RINDX( R ) = SAVINDX
               EXIT
            END IF
         END DO
      END IF


c..Load product species data into work arrays
      DO P = 1, NPRDCT( IRX )
         PINDX( P ) = IRR( IRX, P + 3 )
         PSPEC( P ) = SPECIES( IRR( IRX, P + 3 ) )
         YCOEF( P ) = SC( IRX, P )
      END DO


c..Shuffle product positions if necessary
      IF( PPOS1 .NE. 0 ) THEN
         DO P = 2, NPRDCT( IRX )
            IF( PINDX( P ) .EQ. PPOS1 ) THEN
               SAVINDX = PINDX( 1 )
               SAVSPEC = PSPEC( 1 )
               SAVCOEF = YCOEF( 1 )
               PINDX( 1 ) = PINDX( P )
               PSPEC( 1 ) = PSPEC( P )
               YCOEF( 1 ) = YCOEF( P )
               PINDX( P ) = SAVINDX
               PSPEC( P ) = SAVSPEC
               YCOEF( P ) = SAVCOEF
               EXIT
            END IF
         END DO
      END IF


      IF( PPOS1 .NE. 0 ) THEN
         DO P = 3, NPRDCT( IRX )
            IF( PINDX( P ) .EQ. PPOS2 ) THEN
               SAVINDX = PINDX( 2 )
               SAVSPEC = PSPEC( 2 )
               SAVCOEF = YCOEF( 2 )
               PINDX( 2 ) = PINDX( P )
               PSPEC( 2 ) = PSPEC( P )
               YCOEF( 2 ) = YCOEF( P )
               PINDX( P ) = SAVINDX
               PSPEC( P ) = SAVSPEC
               YCOEF( P ) = SAVCOEF
               EXIT
            END IF
         END DO
      END IF


c..List the reactants
      IF( NREACT( IRX ) .EQ. 0 )  THEN
         RXOUT = '='
         EPOS = 1
      ELSE
         IND = IRR( IRX, 1 ) 
         RXOUT = ADJUSTL( RSPEC( 1 ) )
         EPOS = LEN_TRIM( RXOUT )
         DO R = 2, NREACT( IRX )
            RXOUT = RXOUT( 1 : EPOS ) // '+' // ADJUSTL( RSPEC( R ) )
            EPOS = LEN_TRIM( RXOUT )
         END DO
         RXOUT = RXOUT( 1 : EPOS ) // '='
         EPOS = EPOS + 1
         IF( EPOS .GE. LEN_RXOUT )RETURN
      END IF


c..List the products
      DO P = 1, NPRDCT( IRX )
         CALL CMPRS_COEF( COUT, SGN, YCOEF( P ) )
         IF( P .GT. 1 .OR. ( P .EQ. 1 .AND. SGN .EQ. '-' ) ) THEN
            RXOUT = RXOUT( 1 : EPOS ) // SGN
            EPOS = EPOS + 1
            IF( EPOS .GE. LEN_RXOUT )RETURN
         END IF
         EP = LEN_TRIM( COUT ) 
         IF( EP .GT. 0 ) THEN     
            RXOUT = RXOUT( 1 : EPOS ) // COUT( 1 : EP ) // '*'
            EPOS = LEN_TRIM( RXOUT )
         END IF
         RXOUT = RXOUT( 1 : EPOS ) // PSPEC( P )
         EPOS = LEN_TRIM( RXOUT )
      END DO

      RETURN

      END

      SUBROUTINE TRIM_RXN( RXOUT, NPOS )

C**********************************************************************
C
C  FUNCTION: Trim the reaction line 
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
      USE RXNS_DATA   ! mechanism data and specifications


      IMPLICIT NONE

C..INCLUDES: None
      
C..ARGUMENTS:
      CHARACTER( * )    :: RXOUT   ! Reactant or Product code

      INTEGER           :: NPOS     ! No of characters in string

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      CHARACTER(  16 ), SAVE   ::    PNAME = 'TRIM_RXN'    ! Program name
 
C..SCRATCH LOCAL VARIABLES:
      INTEGER    ::  ERX
      INTEGER    ::  N, P

C**********************************************************************

      ERX = LEN_TRIM( RXOUT ) 

      IF( ERX .LE. NPOS ) RETURN

      DO N = ERX, 1, -1

         IF( RXOUT( N : N ) .EQ. '+' .OR. RXOUT( N : N ) .EQ. '-' .OR.
     &       RXOUT( N : N ) .EQ. '=' ) THEN
            IF( N + 3 .GT. NPOS ) THEN
               CYCLE
            ELSE
               RXOUT( N + 1 : N + 3 ) = '...'
               DO P = N + 4 , ERX
                  RXOUT( P : P ) = ' '
               END DO
               EXIT
             END IF
          END IF
       END DO
             
      RETURN

      END


      SUBROUTINE BLD_OUTLINE( LINTYP, VNAME, PLSPEC, PLIND, COEFF, IRX,
     &            GRPNO, NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

C**********************************************************************
C
C  FUNCTION: Build the output line for specified variable name
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
      USE RXNS_DATA   ! mechanism data and specifications


      IMPLICIT NONE

C..INCLUDES:

               
      
C..ARGUMENTS:
      CHARACTER( * )    :: LINTYP    ! Reactant or Product code
      CHARACTER( * )    :: LINOUT    ! Reactant or Product code
      CHARACTER( * )    :: VNAME     ! Reactant or Product code
      CHARACTER( * )    :: PLSPEC    ! Reactant or Product code

      INTEGER           :: IRX       ! No of characters in string
      INTEGER           :: NPOS      ! No of characters in string
      INTEGER           :: GRPNO     ! group number
      INTEGER           :: PLIND     ! species index
      INTEGER           :: RPOS1     ! Index of 1st reactant
      INTEGER           :: RPOS2     ! Index of 2nd reactant
      INTEGER           :: PPOS1     ! Index of 1st product
      INTEGER           :: PPOS2     ! Index of 2nd product

      LOGICAL           :: LRXN1     ! Flag for first reaction

      REAL( 8 )         :: COEFF

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      CHARACTER(  16 ), SAVE :: PNAME = 'BLD_OUTLINE'    ! Program name
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 150 )  ::    RXOUT                ! Compressed rxn line
      CHARACTER(  CL )  ::    SPOUT                ! Ouput species
      CHARACTER(  16 )  ::    SPEC     
cglg  CHARACTER*( 16 )  ::    FILLR
      CHARACTER*( 80 )  ::    FILLR                                
      CHARACTER*( 30 )  ::    VOUT                 ! Output variable name
      CHARACTER*(  5 )  ::    RNUM                 ! Reaction number
      CHARACTER*( 15 )  ::    COUT                 ! Output coefficient
      CHARACTER*(  1 )  ::    SGN                  ! Coefficient sign

      INTEGER           ::    NR                   ! Number of reactants
      INTEGER           ::    EPOS, EV             ! end pos of string
      INTEGER           ::    ERX                  ! end pos of reaction string
      INTEGER           ::    N, R, S              ! Loop indices

      INTEGER           :: RCT( MXRCT )
  
C..SAVED LOCAL VARIABLES
      CHARACTER( 1 ), SAVE  :: GROUP               ! Group no (character)


      INTEGER, SAVE         ::  KO3_O1D = 0 
      INTEGER, SAVE         ::  KNO2_NO2EX = 0  

C..SAVED LOGICAL VARIABLES
      LOGICAL, SAVE   ::    LFIRST     = .TRUE.     ! First call flag
      LOGICAL, SAVE   ::    FLAG_OH_S  = .TRUE.     ! controls OH_S write
      LOGICAL, SAVE   ::    FLAG_HO2_S = .TRUE.     ! controls HO2_S write
      LOGICAL, SAVE   ::    USE_YCP    = .FALSE.    ! whether to YC or YCP

C**********************************************************************

      IF( LFIRST ) THEN

         LFIRST = .FALSE.

c..Find some special reactions and save their reaction number

         DO N = 1, NRXNS

c..O3-->O1D
            IF( IRR( N, 1 ) .EQ. O3 .AND. IRR( N, 4 ) .EQ. O1D ) 
     &            KO3_O1D = N

c..NO2-->NO2EX
            IF( LNO2EX_CYCLE ) THEN

               IF( IRR( N, 1 ) .EQ. NO2 .AND. IRR( N, 4 ) .EQ. NO2EX ) 
     &              KNO2_NO2EX = N
            END IF

         END DO        

      END IF

      USE_YCP = .FALSE.
      
      WRITE( GROUP, '(I1)' ) GRPNO
      WRITE( RNUM , '( I5 )' ) IRX
      VOUT = ADJUSTL( VNAME )
      EV = LEN_TRIM( VNAME )


      DO S = 1, LEN(FILLR)
         FILLR( S : S ) = ' '
      END DO


      IF( LINTYP .EQ. 'LFREQ' ) THEN        ! for 
     

c..determine the reactant for the rate expression
         NR = 0
         DO R = 1, MXRCT
            IF( IRR( IRX, R ) .GT. 0 .AND. IRR( IRX, R ) .NE. PLIND ) THEN
               NR = NR + 1
               RCT( NR ) = IRR( IRX, R )
            END IF
         END DO

c..This section is necessary to properly do rxns of type NO+NO=
         IF( NR .EQ. 0 .AND. NREACT( IRX ) .EQ. 2 ) THEN
            NR = 1
            RCT( NR ) = PLIND
         END IF

         IF( NR .GT. 1 ) THEN                    
            MSG = ' '
            WRITE(LOGDEV,'(a)' )TRIM( MSG )
            MSG = 'Warning: More than 1 reactant for ' // TRIM( VOUT ) 
     &      // ' term ( Reaction No. ' // TRIM ( RNUM ) // ' )'
            WRITE(LOGDEV,'(a)', ADVANCE = 'NO')TRIM( MSG )
            MSG = ' check correctness of ' // TRIM( PLSPEC ) //
     &            ' section in hrg' // TRIM( GROUP ) // '.F'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

                           
c..build the output line
         COUT = '                     '
         IF( COEFF .NE. 1.0D0 ) WRITE( COUT, '( 1PD12.4, A )' ) COEFF, ' * '

         IF( LRXN1 ) THEN
            LINOUT = '      ' // VOUT( 1 : EV )  // ' =  ' // COUT // 
     &               'RKI( '  // RNUM // ' )'
         ELSE
            LINOUT = '     &' // FILLR( 1 : MAX( 1, (EV-1) )) // '  +  ' 
     &             // COUT //  'RKI( '  // RNUM // ' )'
         END IF
         EPOS = LEN_TRIM( LINOUT )

         IF( NR .GE. 1 ) THEN
            DO R = 1, NR
               SPOUT = ADJUSTL( SPECIES( RCT( 1 ) ) )
               USE_YCP = ( RCT( R ) .EQ. OH  .OR. RCT( R ) .EQ. HO2 
     &                .OR. RCT( R ) .EQ. O3P .OR. RCT( R ) .EQ. O3
     &                .OR. RCT( R ) .EQ. NO  .OR. RCT( R ) .EQ. NO2 )            
               IF( VNAME .EQ. 'L15' .AND. USE_YCP )THEN
                  LINOUT = LINOUT( 1 : EPOS ) // ' * YCP( ' // SPOUT  // ' )'
               ELSE
                  LINOUT = LINOUT( 1 : EPOS ) // ' * YC ( ' // SPOUT  // ' )'
               END IF   
               EPOS = LEN_TRIM( LINOUT )
            END DO 
         ELSE
            DO S = EPOS + 1, EPOS + CL + 10
               LINOUT( S : S ) = ' '
            END DO
            EPOS = EPOS + CL + 10
         END IF

         LINOUT = LINOUT( 1 : EPOS ) // '   '
         EPOS = EPOS + 3

         CALL CMPRS_RXN( RPOS1, RPOS2, PPOS1, PPOS2, IRX, RXOUT )

         CALL TRIM_RXN( RXOUT, NPOS )
         ERX = LEN_TRIM( RXOUT ) 
        
         LINOUT = LINOUT( 1 : EPOS ) // '! ' // RXOUT( 1 : ERX )
             
         RETURN

      ELSEIF( LINTYP .EQ. 'RXRAT' ) THEN        ! for 
 
         COUT = '                     '
         IF( COEFF .NE. 1.0D0 ) WRITE( COUT, '( 1PD12.4, A )' ) COEFF, ' * '

c..special case for O3=O1D
         IF( VNAME .EQ. 'P12' .AND. IRR( IRX, 1 ) .EQ. O3 .AND.
     &       IRR( IRX, 4 ) .EQ. O1D )
     &      COUT = ' O3P_S * '

c..special case for NO2EX=NO2 -- relate it to NO2->NO2EX
         IF( LNO2EX_CYCLE ) THEN
            IF( VNAME .EQ. 'P2' .AND. IRR( IRX, 1 ) .EQ. NO2EX .AND.
     &          IRR( IRX, 4 ) .EQ. NO2 ) THEN
                WRITE( RNUM , '( I5 )' ) KNO2_NO2EX
                COUT = ' NO2_S * '
             END IF
         END IF


c..Special case for OH being formed form O1D -- relate it to O3
         IF( VNAME .EQ. 'P4' .AND. IRR( IRX, 1 ) .EQ. O1D .AND.
     &      IRR( IRX, 4 ) .EQ. OH ) THEN
            IF( FLAG_OH_S )THEN
               WRITE( RNUM , '( I5 )' ) KO3_O1D
               COUT = ' OH_S  * '   
            ELSE 
               LINOUT = ' '
               RETURN
            END IF
            FLAG_OH_S = .FALSE.
         END IF

c..Special case for HO2 being formed form O1D via H2 -- relate it to O3
         IF( VNAME .EQ. 'P5' .AND. IRR( IRX, 1 ) .EQ. O1D .AND.
     &      (PPOS1 .EQ. HO2 .OR. PPOS2 .EQ. HO2) ) THEN
            IF( FLAG_HO2_S )THEN                
               WRITE( RNUM , '( I5 )' ) KO3_O1D
               COUT = ' HO2_S  * '                 
            ELSE 
               LINOUT = ' '
               RETURN
            END IF
            FLAG_HO2_S = .FALSE.
         END IF

c..Special case for OH being formed from NO2EX -- relate it to NO2 
         IF( LNO2EX_CYCLE ) THEN
            IF( VNAME .EQ. 'P4' .AND. IRR( IRX, 1 ) .EQ. NO2EX .AND.
     &          ( IRR( IRX, 4 ) .EQ. OH .OR. IRR( IRX, 5 ) .EQ. OH )  ) THEN
               WRITE( RNUM , '( I5 )' ) KNO2_NO2EX
              COUT = ' XOH_S * '
            END IF
         END IF

c..Special case for HONO being formed from NO2EX -- relate it to NO2 
         IF( LNO2EX_CYCLE ) THEN
            IF( VNAME .EQ. 'P19' .AND. IRR( IRX, 1 ) .EQ. NO2EX .AND.
     &          ( IRR( IRX, 4 ) .EQ. HONO .OR. IRR( IRX, 5 ) .EQ. HONO )  ) THEN
               WRITE( RNUM , '( I5 )' ) KNO2_NO2EX
              COUT = ' XOH_S * '
            END IF
         END IF


         IF( LRXN1 ) THEN
            IF( VNAME .EQ. 'P15'  ) THEN
               LINOUT = '      '  // VNAME( 1 : EV ) // ' =  ' // COUT //
     &                  'RKI( ' // RNUM // ' )'
               DO R = 1, NREACT( IRX )
                  EPOS = LEN_TRIM( LINOUT )
                  S = IRR( IRX,R )
                  SPOUT(1:CL) = SPECIES( S )(1:CL)
                  IF( S .EQ. O3P .OR. S .EQ. NO2 .OR. S .EQ. O3 . OR. S .EQ. OH )THEN
                     LINOUT = LINOUT(1:EPOS) 
     &                      // ' * YCP( ' // SPOUT(1:CL) // ' ) '
                  ELSE
                     LINOUT = LINOUT(1:EPOS)
     &                      // ' * YC ( ' // SPOUT(1:CL)  // ' ) '
                  END IF
               END DO
            ELSE
               LINOUT = '      '  // VNAME( 1 : EV ) // ' =  ' // COUT //
     &                  'RXRAT( ' // RNUM // ' )'
            END IF
         ELSE
            IF( VNAME .EQ. 'P15'  ) THEN
               LINOUT = '     &'  // FILLR( 1 : MAX( 1, (EV-1) )) // '  +  ' 
     &                // COUT // 'RKI( ' // RNUM // ' )'
               DO R = 1, NREACT( IRX )
                  EPOS = LEN_TRIM( LINOUT )
                  S = IRR( IRX,R )
                  SPOUT(1:CL) = SPECIES( S )(1:CL)
                  IF( S .EQ. O3P .OR. S .EQ. NO2 .OR. S .EQ. O3 . OR. S .EQ. OH )THEN
                     LINOUT = LINOUT(1:EPOS) 
     &                      // ' * YCP( ' // SPOUT(1:CL) // ' ) '
                  ELSE
                     LINOUT = LINOUT(1:EPOS) 
     &                      // ' * YC ( ' // SPOUT(1:CL) // ' ) '
                 END IF
               END DO  
            ELSE
               LINOUT = '     &'  // FILLR( 1 : MAX( 1, (EV-1) )) // '  +  ' 
     &                // COUT // 'RXRAT( ' // RNUM // ' )'
            END IF
         END IF

         READ( RNUM, '(I5)' ) N
         L_RXFLAG( N ) = .TRUE.

         IF( VNAME .EQ. 'P15'  ) THEN
           S = MAXVAL( NREACT ) - NREACT( IRX ) 
           EPOS = LEN_TRIM( LINOUT )
           DO R = 1, S
              LINOUT = LINOUT( 1 : EPOS ) // FILLR( 1:(CL+10) )
              EPOS  = EPOS + CL + 10
            END DO
            LINOUT = LINOUT( 1 : EPOS ) // '      '
            EPOS = EPOS + 6
         ELSE
            EPOS = LEN_TRIM( LINOUT )
            LINOUT = LINOUT( 1 : EPOS ) // '      '
            EPOS = EPOS + 6
         END IF   

         CALL CMPRS_RXN( RPOS1, RPOS2, PPOS1, PPOS2, IRX, RXOUT )
         CALL TRIM_RXN( RXOUT, NPOS )
         ERX = LEN_TRIM( RXOUT ) 

         LINOUT = LINOUT( 1 : EPOS ) // '! ' // RXOUT( 1 : ERX )


         RETURN

      END IF

      END



      SUBROUTINE LCASE( BUFFER )

C**********************************************************************
C
C  FUNCTION: Converts string uppper case letters to lower case 
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, March, 2004
C
C**********************************************************************

      IMPLICIT NONE

C..INCLUDES: None
      
C..ARGUMENTS:
      CHARACTER( * )    :: BUFFER    ! Reactant or Product code

C..PARAMETERS:
      INTEGER, PARAMETER  :: CAPS = 65
      INTEGER, PARAMETER  :: CAPE = 90
      INTEGER, PARAMETER  :: DIFF = 32
      

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      INTEGER I, L, C

   


C**********************************************************************
        L  =  LEN ( BUFFER )

        DO  I = 1 , L
          C = ICHAR ( BUFFER ( I : I ) )
          IF ( C .GE. CAPS  .AND. C .LE. CAPE )
     &        BUFFER ( I : I ) = CHAR ( C + DIFF )
        END DO

        RETURN

        END

