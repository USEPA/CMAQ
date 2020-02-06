       SUBROUTINE ERR_CHECK

C**********************************************************************
C
C  FUNCTION: Check species & rxns for potential errors in creating
C              EBI source
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
!      USE M3UTILIO    ! IOAPI parameters and declarations
      USE RXNS_DATA   ! mechanism data and specifications


      IMPLICIT NONE

C..INCLUDES: 

               
      
C..ARGUMENTS: None

C..PARAMETERS:
      INTEGER, PARAMETER   ::  IZERO = 0    ! Integer zero

C..EXTERNAL FUNCTIONS:
!      INTEGER    NAME_INDEX            !


C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(   5 )         ::    RNUM
      CHARACTER(  16 )         ::    PNAME = 'ERR_CHECK'    ! Program name
      CHARACTER( 100 )         ::    LINOUT                 ! Reaction line
      CHARACTER( 150 )         ::    RXOUT                  ! Reaction line
      CHARACTER( 256 )         ::    MSG                    ! Message text

      INTEGER         ::  E1, E2
      INTEGER         ::  M, N 
      INTEGER         ::  NPOS
      INTEGER         ::  IND

      REAL(8)         :: RXN_LOSS = 0.0D0
      REAL(8)         :: RXN_PROD = 0.0D0
  
      LOGICAL         ::  LERROR 
      LOGICAL         ::  LRXN1 
      LOGICAL         ::  LFOUND

C**********************************************************************

      LERROR = .FALSE.


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check & report group species indices
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( NO .EQ. 0 ) THEN
        MSG = 'ERROR: Group 1 species NO not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( NO2 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 1 species NO2 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( O3 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 1 species O3 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( O3P .EQ. 0 ) THEN
        MSG = 'ERROR: Group 1 species O3P not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( O1D .EQ. 0 ) THEN
        MSG = 'ERROR: Group 1 species O1D not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( LNO2EX_CYCLE .AND. NO2EX .EQ. 0 ) THEN
        MSG = 'ERROR: Group 1 species NO2EX not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( OH .EQ. 0 ) THEN
        MSG = 'ERROR: Group 2 species OH not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( HO2 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 2 species HO2 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( HONO .EQ. 0 ) THEN
        MSG = 'ERROR: Group 2 species HONO not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( HNO4 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 2 species HNO4 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( NO3 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 3 species NO3 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( N2O5 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 3 species N2O5 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( C2O3 .EQ. 0 ) THEN
        MSG = 'ERROR: Group 4 species C2O3 not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( PAN .EQ. 0 ) THEN
        MSG = 'ERROR: Group 4 species PAN not found in species list'
        WRITE( LOGDEV, 92020 ) MSG( 1 : LEN_TRIM( MSG ) )
        LERROR = .TRUE.
      END IF

      IF( .NOT. LERROR ) THEN
         WRITE( LOGDEV, 93000 ) 
     &       SPECIES( NO   )( 1 : LEN_TRIM( SPECIES( NO   ) ) ),
     &       SPECIES( NO2  )( 1 : LEN_TRIM( SPECIES( NO2  ) ) ),
     &       SPECIES( O3   )( 1 : LEN_TRIM( SPECIES( O3   ) ) ),
     &       SPECIES( O3P  )( 1 : LEN_TRIM( SPECIES( O3P  ) ) ),
     &       SPECIES( O1D  )( 1 : LEN_TRIM( SPECIES( O1D  ) ) ),
     &       SPECIES( OH   )( 1 : LEN_TRIM( SPECIES( OH   ) ) ),
     &       SPECIES( HO2  )( 1 : LEN_TRIM( SPECIES( HO2  ) ) ),
     &       SPECIES( HONO )( 1 : LEN_TRIM( SPECIES( HONO ) ) ),
     &       SPECIES( HNO4 )( 1 : LEN_TRIM( SPECIES( HNO4 ) ) ),
     &       SPECIES( NO3  )( 1 : LEN_TRIM( SPECIES( NO3  ) ) ),
     &       SPECIES( N2O5 )( 1 : LEN_TRIM( SPECIES( N2O5 ) ) ),
     &       SPECIES( C2O3 )( 1 : LEN_TRIM( SPECIES( C2O3 ) ) ),
     &       SPECIES( PAN  )( 1 : LEN_TRIM( SPECIES( PAN  ) ) )

         IF( LNO2EX_CYCLE ) WRITE( LOGDEV, 93010 ) 
     &       SPECIES( NO2EX )( 1 : LEN_TRIM( SPECIES( NO2EX ) ) )
      END IF


      IF( LERROR ) THEN
        MSG = TRIM( PNAME ) // ': Stopping because of group species error'
        WRITE(LOGDEV,'(a)')TRIM(MSG)
        STOP
      END IF


      WRITE( LOGDEV, 94000 )
     
      IF( LNO2EX_CYCLE ) WRITE( LOGDEV, 94010 )


c..check for rxns w/ more than 2 reactants ( Warning only )
      LRXN1 = .TRUE.
      NPOS = 50
      DO N = 1, NRXNS
         IF( NREACT( N ) .GT. 2 ) THEN
            IF( LRXN1 ) THEN
               WRITE( LOGDEV, 95000 )
               LRXN1 = .FALSE.
            END IF
            WRITE( RNUM, '(I5)') N
            CALL CMPRS_RXN( IZERO, IZERO, IZERO, IZERO, N, RXOUT )
            CALL TRIM_RXN( RXOUT, NPOS )
            E1 = LEN_TRIM( RXOUT )
            E2 = LEN_TRIM( RXLABEL( N ) )
            LINOUT = '   REACTION ' // RNUM // ': <' // 
     &              RXLABEL( N )( 1 : LL ) // '> ' // RXOUT( 1 : E1 )
            E1 = LEN_TRIM( LINOUT )
            WRITE( LOGDEV, 92000 ) LINOUT( 1 : E1 )
         END IF
      END DO


c..check to make sure all O1D loss reactions are psuedo-first order
      LRXN1 = .TRUE.
      NPOS = 50
      DO N = 1, NRXNS
         IF( ( IRR( N, 1 ) .EQ. O1D .AND. NREACT( N ) .GT. 1 ) .OR.
     &          IRR( N, 2 ) .EQ. O1D .OR. IRR( N, 3 ) .EQ. O1D ) THEN
            IF( LRXN1 ) THEN
               WRITE( LOGDEV, 95020 )
               LRXN1 = .FALSE.
            END IF
            WRITE( RNUM, '(I5)') N
            CALL CMPRS_RXN( IZERO, IZERO, IZERO, IZERO, N, RXOUT )
            CALL TRIM_RXN( RXOUT, NPOS )
            E1 = LEN_TRIM( RXOUT )
            LINOUT = '   REACTION ' // RNUM // ': <' // 
     &              RXLABEL( N )( 1 : LL ) // '> ' // RXOUT( 1 : E1 )
            E1 = LEN_TRIM( LINOUT )
            WRITE( LOGDEV, 92000 ) LINOUT( 1 : E1 )
            LERROR = .TRUE.
         END IF
      END DO

c..check to make sure there is a O1D-->O3P reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( IRR( N, 1 ) .EQ. O1D .AND. NREACT( N ) .EQ. 1 ) .AND.
     &         IRR( N, 4 ) .EQ. O3P ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95040 )
         LERROR = .TRUE.
      END IF

c..check to make sure there is a O1D-->2*OH reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( IRR( N, 1 ) .EQ. O1D .AND. NREACT( N ) .EQ. 1 ) .AND.
     &         IRR( N, 4 ) .EQ. OH .AND. SC( N, 1 ) .EQ. 2.0D0 ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95060 )
         LERROR = .TRUE.
      END IF


c..check for a NO2=NO+O3P reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( IRR( N, 1 ) .EQ. NO2 .AND. NREACT( N ) .EQ. 1   ) .AND.
     &     ( ( IRR( N, 4 ) .EQ. NO  .AND. IRR( N, 5 ) .EQ. O3P ) .OR.
     &       ( IRR( N, 4 ) .EQ. O3P .AND. IRR( N, 5 ) .EQ. NO  ) ) ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95080 )
         LERROR = .TRUE.
      END IF

c..check for a O3P-->O3
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( IRR( N, 1 ) .EQ. O3P .AND. NREACT( N ) .EQ. 1 .AND.
     &       IRR( N, 4 ) .EQ. O3  .AND. SC( N, 1 ) .EQ. 1.0D0 ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95100 )
         LERROR = .TRUE.
      END IF

c..check for a NO+O3=NO2 reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( ( IRR( N, 1 ) .EQ. NO  .AND. IRR( N, 2 ) .EQ. O3 ) .OR.
     &         ( IRR( N, 2 ) .EQ. NO  .AND. IRR( N, 1 ) .EQ. O3 ) ) .AND.
     &         ( IRR( N, 4 ) .EQ. NO2 .AND. SC( N, 1 )  .EQ. 1.0D0 ) ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95120 )
         LERROR = .TRUE.
      END IF

c..check for a HONO=OH+NO reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( ( IRR( N, 1 ) .EQ. HONO .AND. NREACT( N ) .EQ. 1 ) .AND.
     &         ( IRR( N, 4 ) .EQ. OH .AND. IRR( N, 5 ) .EQ. NO ) ) .OR.
     &         ( IRR( N, 4 ) .EQ. NO .AND. IRR( N, 5 ) .EQ. OH ) ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95140 )
         LERROR = .TRUE.
      END IF

c..check for a NO+OH=HONO reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( ( IRR( N, 1 ) .EQ. OH .AND. IRR( N, 2 ) .EQ. NO ) .OR.
     &         ( IRR( N, 1 ) .EQ. NO .AND. IRR( N, 2 ) .EQ. OH ) .AND.
     &         NREACT( N ) .EQ. 2 ) .AND.
     &         ( IRR( N, 4 ) .EQ. HONO .AND. NPRDCT( N ) .EQ. 1 ) ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95160 )
         LERROR = .TRUE.
      END IF

c..check for a HNO4=HO2+NO2 reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( ( IRR( N, 1 ) .EQ. HNO4 .AND. NREACT( N ) .EQ. 1 ) .AND.
     &         ( IRR( N, 4 ) .EQ. HO2 .AND. IRR( N, 5 ) .EQ. NO2 ) ) .OR.
     &         ( IRR( N, 4 ) .EQ. NO2 .AND. IRR( N, 5 ) .EQ. HO2 ) ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95180 )
         LERROR = .TRUE.
      END IF

c..check for a HO2+NO2=HNO4 reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( ( ( IRR( N, 1 ) .EQ. HO2 .AND. IRR( N, 2 ) .EQ. NO2 ) .OR.
     &         ( IRR( N, 1 ) .EQ. NO2 .AND. IRR( N, 2 ) .EQ. HO2 ) .AND.
     &         NREACT( N ) .EQ. 2 ) .AND.
     &         ( IRR( N, 4 ) .EQ. HNO4 .AND. NPRDCT( N ) .EQ. 1 ) ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95200 )
         LERROR = .TRUE.
      END IF

c..check for a HO2+HO2= reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( IRR( N, 1 ) .EQ. HO2 .AND. IRR( N, 2 ) .EQ. HO2 .AND.
     &         NREACT( N ) .EQ. 2 ) THEN
            LFOUND = .TRUE.
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95220 )
         LERROR = .TRUE.
      END IF

      
c..check for a C2O3+C2O3= reaction
      LFOUND = .FALSE.
      DO N = 1, NRXNS
         IF( IRR( N, 1 ) .EQ. C2O3 .AND. IRR( N, 2 ) .EQ. C2O3 .AND.
     &       NREACT( N ) .EQ. 2 ) THEN
            LFOUND = .TRUE.
            RXN_C2O3_SELF = N
            EXIT
         END IF
      END DO
      IF( .NOT. LFOUND ) THEN
         WRITE( LOGDEV, 95240 )
         LERROR = .TRUE.
      END IF

c..If NO2EX cycle in group1, check cycle reactions

      IF( LNO2EX_CYCLE ) THEN

c..check for NO2->NO2EX reaction
         LFOUND = .FALSE.
         DO N = 1, NRXNS
            IF( IRR( N, 1 ) .EQ. NO2 .AND. NREACT( N ) .EQ. 1 .AND.
     &          IRR( N, 4 ) .EQ. NO2EX ) THEN
               LFOUND = .TRUE.
               EXIT
            END IF
         END DO
         IF( .NOT. LFOUND ) THEN
            WRITE( LOGDEV, 95260 )
            LERROR = .TRUE.
         END IF

c..Check that all NO2EX loss reactions are psuedo-first order
         LRXN1 = .TRUE.
         NPOS = 50
         DO N = 1, NRXNS
            IF( ( IRR( N, 1 ) .EQ. NO2EX .AND. NREACT( N ) .GT. 1 ) .OR.
     &            IRR( N, 2 ) .EQ. NO2EX .OR. IRR( N, 3 ) .EQ. NO2EX ) THEN
               IF( LRXN1 ) THEN
                  WRITE( LOGDEV, 95280 )
                  LRXN1 = .FALSE.
               END IF
               WRITE( RNUM, '(I5)') N
               CALL CMPRS_RXN( IZERO, IZERO, IZERO, IZERO, N, RXOUT )
               CALL TRIM_RXN( RXOUT, NPOS )
               E1 = LEN_TRIM( RXOUT )
               LINOUT = '   REACTION ' // RNUM // ': <' // 
     &                 RXLABEL( N )( 1 : LL ) // '> ' // RXOUT( 1 : E1 )
               E1 = LEN_TRIM( LINOUT )
               WRITE( LOGDEV, 92000 ) LINOUT( 1 : E1 )
               LERROR = .TRUE.
            END IF
         END DO

      END IF    ! LNO2_CYCLE = .TRUE.

! Check for net negative stoichiometry in reactions
      
      DO M = 1, NRXNS
         DO N = 1, NUMB_MECH_SPC
            CALL SUM_COEFF( RXN_LOSS, 'R', N, M )
            CALL SUM_COEFF( RXN_PROD, 'P', N, M )
            IF( RXN_LOSS + RXN_PROD .GE. 0.0D0 )CYCLE
            IF( LPAR_NEG .AND. TRIM( CHEMISTRY_SPC( N ) ) .EQ. 'PAR' )CYCLE
            LERROR = .TRUE.
            IF( .NOT. LPAR_NEG .AND. TRIM( CHEMISTRY_SPC( N ) ) .EQ. 'PAR' )THEN
               WRITE( RNUM, '(I5)') N
               MSG = '     ERROR: REACTION ' // TRIM( RXLABEL( M ) )
     &            // ' has negative net yield for PAR but PAR_NEG_FLAG set to false. '
     &            // 'Correct run script and re-run.'
               WRITE(LOGDEV,92000)TRIM( MSG )
               CYCLE               
            END IF
            MSG =  '     ERROR: REACTION ' // TRIM( RXLABEL( M ) )
     &          // ' has negative net yield for ' // TRIM( CHEMISTRY_SPC( N ) )  
     &          // '. Not Permitted.'
            WRITE(LOGDEV,92000)TRIM( MSG )
         END DO
      END DO   
           
         

c..check to make sure no group 1-4 species are also steady-state species
      IF( N_SS_SPC .GT. 0 ) THEN

         DO N = 1, N_SS_SPC

            IND = NAME_INDEX( SS_SPC( N ), N_SPECIES, SPECIES )

            IF( IND .EQ. NO    .OR.  IND .EQ. NO2   .OR.
     &          IND .EQ. O3    .OR.  IND .EQ. O3P   .OR.
     &          IND .EQ. O1D   .OR.  IND .EQ. OH    .OR.
     &          IND .EQ. HO2   .OR.  IND .EQ. HONO  .OR.
     &          IND .EQ. HNO4  .OR.  IND .EQ. NO3   .OR.
     &          IND .EQ. N2O5  .OR.  IND .EQ. C2O3  .OR.
     &          IND .EQ. PAN ) THEN
                WRITE( LOGDEV, 95300 ) SS_SPC( N )
                LERROR = .TRUE.
            END IF

            IF( LNO2EX_CYCLE .AND. IND .EQ. NO2EX ) THEN
                WRITE( LOGDEV, 95300 ) SS_SPC( N )
                LERROR = .TRUE.
            END IF

         END DO

      END IF
 

      IF( LERROR ) THEN
        MSG = TRIM( PNAME )
     &     // ': Stopping because mechanism requirements for EBI solver not met'
        WRITE(LOGDEV,'(a)')TRIM(MSG)
        STOP
      ELSE
        WRITE( LOGDEV, 94020 )
      END IF


      RETURN

92000 FORMAT( A )

92020 FORMAT(/ 5X, A )


93000 FORMAT( // '     Group species mapping results:' /
     &'        nitric oxide (NO):                ', A /
     &'        nitrogen dioxide (NO2):           ', A /
     &'        ozone (O3):                       ', A /
     &'        ground state atomic oxygen (O3P): ', A /
     &'        excited atomic oxygen (O1D):      ', A /
     &'        hydroxyl radical (OH):            ', A /
     &'        hydroperoxy radical (HO2):        ', A /
     &'        nitrous acid (HONO):              ', A /
     &'        peroxynitric acid (HNO4):         ', A /
     &'        nitrate radical (NO3):            ', A /
     &'        nitrogen pentoxide (N2O5):        ', A /
     &'        peroxy acetyl radical (C2O3):     ', A /
     &'        peroxy acetyl nitrate (PAN):      ', A )
93010 FORMAT( '        excited nitrogen dioxide (NO2EX): ', A )

94000 FORMAT(//5X, 'Checking mechanism for EBI solver requirements.' )
94010 FORMAT(//5X, 'NOTE: NO2EX is included in Group 1' )
94020 FORMAT( /5X,  'No mechanism requirement problems detected - continuing.' )


95000 FORMAT(/5X, 'WARNING: The following reactions have more than 2',
     &           ' reactants.' )
95020 FORMAT(/5X, 'ERROR: The mechanism includes a non psuedo 1st-order',
     &            ' loss reaction for O1D' )         
95040 FORMAT(/5X, 'ERROR: O1D-->O3P reaction not found in mechanism' )
95060 FORMAT(/5X, 'ERROR: O1D-->2*OH reaction not found in mechanism' )
95080 FORMAT(/5X, 'ERROR: NO2-->NO+O3P reaction not found in mechanism' )
95100 FORMAT(/5X, 'ERROR: O3P-->O3 reaction not found in mechanism' )
95120 FORMAT(/5X, 'ERROR: NO+O3-->NO2 reaction not found in mechanism' )
95140 FORMAT(/5X, 'ERROR: HONO-->OH+NO reaction not found in mechanism' )
95160 FORMAT(/5X, 'ERROR: NO+OH-->HONO reaction not found in mechanism' )
95180 FORMAT(/5X, 'ERROR: HNO4-->HO2+NO2 reaction not found in mechanism' )
95200 FORMAT(/5X, 'ERROR: HO2+NO2-->HNO4 reaction not found in mechanism' )
95220 FORMAT(/5X, 'ERROR: HO2+HO2--> reaction not found in mechanism' )
95240 FORMAT(/5X, 'ERROR: C2O3+C2O3--> reaction not found in mechanism' )
95260 FORMAT(/5X, 'ERROR: NO2-->NO2EX reaction not found inmechanism' )
95280 FORMAT(/5X, 'ERROR: The mechanism includes a non psuedo 1st-order',
     &            ' loss reaction for NO2EX' )         
95300 FORMAT(/5X, 'ERROR: The following group species cannot be put in',
     &            ' steady-state: ', A )
 

       END
