#ifdef sens

      MODULE MECHANISM_FUNCTIONS


      IMPLICIT NONE


      INTEGER :: JDATE = 2011188      ! current Julian date (YYYYDDD)
      INTEGER :: JTIME =  000000      ! current time (HHMMSS)

      REAL( 8 ), ALLOCATABLE  :: FORWARD_CONV( : )  ! CGRID to CHEM Species conversion factor
      REAL( 8 ), ALLOCATABLE  :: REVERSE_CONV( : )  ! CHEM to CGRID Species conversion factor

      INTEGER                :: NUMB_CHEM_SPC = 0
      REAL( 8 ), ALLOCATABLE :: CONC( : )       ! concentration, ppmV
      REAL( 8 ), ALLOCATABLE :: DYDT( : )       ! time derivative of species
      REAL( 8 ), ALLOCATABLE :: JACOBIAN( :,: ) ! mechanism's jacobain matrix


      INTEGER, PARAMETER ::  MAX_NCELLS = 1
       
      CHARACTER( 16 ), ALLOCATABLE :: SPECIES( : )
      
      TYPE SPECIES_BUDGET
         CHARACTER(16)        :: SPECIES_NAME = ' '
         INTEGER              :: NREACTIONS   = 0
         INTEGER, ALLOCATABLE :: IREACTION( : )
         REAL(8), ALLOCATABLE :: COEFF_NET( : )
         INTEGER              :: NRXNS_PROD   = 0
         INTEGER, ALLOCATABLE :: IRXN_PROD( : )
         REAL(8), ALLOCATABLE :: COEFF_POS( : )
         INTEGER              :: NRXNS_LOSS   = 0
         INTEGER, ALLOCATABLE :: IRXN_LOSS( : )
         REAL(8), ALLOCATABLE :: COEFF_NEG( : )
      END TYPE SPECIES_BUDGET

      TYPE REACTION_EFFECTS
         CHARACTER(16)        :: REACTION_LABEL   = ' '
         LOGICAL              :: LIGHT_DEPENDENT  = .FALSE.
         REAL( 8 )            :: RATE             = 0.0D0
         INTEGER              :: NREACTANTS       = 0
         INTEGER              :: REACTANT( 3 )    = 0
         INTEGER              :: NSPECIES_DESTROYED = 0
         INTEGER, ALLOCATABLE :: ISPECIES_DESTROYED( : )
         REAL(8), ALLOCATABLE :: COEFF_LOSS( : )
         INTEGER              :: NSPECIES_PRODUCED  = 0
         INTEGER, ALLOCATABLE :: ISPECIES_PRODUCED( : )
         REAL(8), ALLOCATABLE :: COEFF_PROD( : )
         INTEGER              :: NSPECIES_NPRODUCED = 0
         INTEGER, ALLOCATABLE :: ISPECIES_NPRODUCED( : )
         REAL(8), ALLOCATABLE :: COEFF_NPROD( : )
         INTEGER              :: JACOB_OCCURANCES
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_INDEX( : )
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_VECTOR( : )
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_ROW( : )
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_COL( : )
         REAL(8), ALLOCATABLE :: JACOB_PARTIAL_COEFF( : )
      END TYPE REACTION_EFFECTS
      
      INTEGER   :: JACOBIAN_TERMS = 0
      
      TYPE(REACTION_EFFECTS), ALLOCATABLE :: REACTION_CHART ( : )
      
      INTEGER :: IDX_FMCL  = 0
      INTEGER :: DDM_LOG   = 6      ! Unit number of output log
      INTEGER :: ERROR_LOG = 6
      LOGICAL :: CHECK_MECHANISM  = .FALSE. ! write out Jacobian and derivatives values
      LOGICAL :: CHECK_SORTING    = .FALSE. ! write out result of mechanism species sorting
      
#ifdef verbose_ddm3d
      LOGICAL :: REPORT_CHART     = .TRUE.  ! write out species derivations and mechanism jacobian
#else       
      LOGICAL :: REPORT_CHART     = .FALSE. ! do not write out species derivations and mechanism jacobian
#endif        
    
!!!!!!REAL( 8 ), ALLOCATABLE ::  RKI(  : )          ! Rate constants
      REAL( 8 ), ALLOCATABLE ::  RKI_SAV( :,: )     ! Rate constants
      REAL( 8 ), ALLOCATABLE ::  SYC(  :, : )       ! Species concentrations

      REAL( 8 ), ALLOCATABLE ::  RXRAT( : )      ! Reaction rates
      REAL( 8 ), ALLOCATABLE ::  RTOL(  : )      ! Species tolerances
      REAL( 8 ), ALLOCATABLE ::  PROD(  : )      ! Prod of species
      REAL( 8 ), ALLOCATABLE ::  LOSS(  : )      ! Loss of species
      REAL( 8 ), ALLOCATABLE ::  LOSSF(  : )     ! Loss Frequency of species

      INTEGER,   ALLOCATABLE ::  JOLD2NEW( :,: )    ! YC species map
      INTEGER,   ALLOCATABLE ::  JNEW2OLD( :,: )    ! YC species map
      INTEGER,   ALLOCATABLE ::  ISPECIES_REACTION( :,: ) ! species index for each reaction
      
      INTEGER  :: CHANGING_SPECIES = 0 ! mechanism species affected by reactions
      INTEGER  :: STEADY_SPECIES   = 0 ! mechanism species not affected by reactions

      LOGICAL,   ALLOCATABLE :: JACOBIAN_FILLED( :,:,: )    ! Is Jacobian position nonzero
      
      LOGICAL                :: LSUNLIGHT  = .TRUE.

      REAL( 8 ), ALLOCATABLE, PRIVATE ::  ATMPRES ( : )     ! Cell pressure, Atm
      REAL( 8 ), ALLOCATABLE, PRIVATE ::  H2O     ( : )     ! Cell H2O mixing ratio (ppmV)
      REAL( 8 ), ALLOCATABLE, PRIVATE ::  TEMP    ( : )     ! Cell Temperature
      REAL( 8 ), ALLOCATABLE, PRIVATE ::  DENS    ( : )     ! Cell mass density (kg/m3)
      REAL( 8 ), ALLOCATABLE, PRIVATE ::  HET     ( :, : )  ! cell heterogeneous reaction rates
      REAL( 8 ), ALLOCATABLE, PRIVATE ::  RJIN    ( :, : )  ! J-values for a cell
      LOGICAL,   ALLOCATABLE, PRIVATE ::  LAND    ( : )     ! land_zone value for specific cell

      LOGICAL :: RESET_JACOBIAN = .FALSE.
      
      CONTAINS
        SUBROUTINE SET_MECHANISM(  )

          USE RXNS_DATA
          USE RXNS_FUNCTION
          USE UTILIO_DEFN
C Initialize arrays and maps that store reaction rates in each grid cell and that
C         relate ISAM species to chemistry species
C
C         Called by chemistry driver

        IMPLICIT NONE

C..Includes:
!         INCLUDE SUBST_CONST     ! CMAQ constants
 
         CHARACTER( 16 ), PARAMETER :: PNAME = 'SET_MECHANISM'     ! Program name

C..arguments: 

C..Parameters:
         
         INTEGER :: I, J, RXN, IP, IPNEG, IL, ISPC
         INTEGER :: NCELL
         INTEGER :: IOS
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         INTEGER :: MIN_NEW_SPECIES      ! Index holder for sort routine
         INTEGER :: MIN_OLD_SPECIES      ! Index holder for sort routine
         INTEGER :: MIN_COUNT      ! Current minimum number of PD terms in sort

         INTEGER INEW, JNEW           ! Index for sorted species number
         INTEGER IOLD, JOLD           ! Index for old species number
         INTEGER IMINNEW              ! Index holder for sort routine
         INTEGER IMINOLD              ! Index holder for sort routine
         INTEGER MINVALU              ! Current number of PD terms in sort

         LOGICAL :: EXISTS 
         LOGICAL :: EFLAG 
         LOGICAL :: SWAPPING

         LOGICAL, SAVE :: INITIALIZED = .FALSE.

         CHARACTER( 132 ) :: MSG           ! Message text
         CHARACTER(  32 ) :: SWAPPED_NAME
         
         REAL              :: HEIGHT
         REAL( 8 )         :: FACTOR     ! conversion factor
         
         INTEGER,         ALLOCATABLE :: SORTED_COUNT  ( : ) ! # of Jacobain partial derivations  per species
         INTEGER,         ALLOCATABLE :: JACOBIAN_COUNT( : ) ! # of Jacobain partial derivations  per species
         CHARACTER( 32 ), ALLOCATABLE :: SWAPPED_NAMES( : )
         
         IF ( INITIALIZED ) RETURN
            EFLAG      = .FALSE.
            ERROR_LOG  = DDM_LOG
            INITIALIZED = .TRUE.
        
!!!!!!!!!!!!ALLOCATE( RKI( NRXNS ),
            ALLOCATE(
     &                RXRAT( NRXNS ),
     &                PROD( NUMB_MECH_SPC),
     &                LOSS( NUMB_MECH_SPC),
     &                LOSSF( NUMB_MECH_SPC),
     &                STAT = IOS )
      
            IF ( IOS .NE. 0 ) THEN
!!!!!!!!!!!!!!!MSG = 'Error allocating RKI, RXRAT, PROD, LOSS'
               MSG = 'Error allocatin  RXRAT, PROD, LOSS'
               WRITE(ERROR_LOG,*)TRIM(MSG)
               EFLAG = .TRUE.
            END IF
      
            ALLOCATE( ATMPRES( MAX_NCELLS ),
     &                H2O    ( MAX_NCELLS ),
     &                TEMP   ( MAX_NCELLS ),
     &                DENS   ( MAX_NCELLS ),
     &                HET    ( MAX_NCELLS, NHETERO ),
     &                RJIN   ( MAX_NCELLS, NPHOTAB ),
     &                LAND   ( MAX_NCELLS ) ,
     &                STAT = IOS )
      
            IF ( IOS .NE. 0 ) THEN
               MSG = 'Error allocating ATMPRES, H2O, TEMP, HET, RJIN, LAND '
               WRITE(ERROR_LOG,*)TRIM(MSG)
               EFLAG = .TRUE.
            END IF

            IF( NSPECIAL .GT. 0 )THEN
               ALLOCATE( SYC( MAX_NCELLS, NUMB_MECH_SPC ), STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating SYC'
                    WRITE(ERROR_LOG,*)TRIM(MSG)
                    EFLAG = .TRUE.
               END IF
               ALLOCATE( RKI_SAV( NRXNS,MAX_NCELLS ), STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating RKI_SAV'
                    WRITE(ERROR_LOG,*)TRIM(MSG)
                    EFLAG = .TRUE.
               END IF
            END IF

            ALLOCATE( JOLD2NEW( NUMB_MECH_SPC,1 ),
     &                JNEW2OLD( NUMB_MECH_SPC,1 ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
                 MSG = 'Error allocating JOLD2NEW or JNEW2OLD'
                 WRITE(ERROR_LOG,*)TRIM(MSG)
                 EFLAG = .TRUE.
            ELSE
                 MSG = 'JOLD2NEW or JNEW2OLD allocated'
                 WRITE(ERROR_LOG,*)TRIM(MSG)
            END IF
            DO I = 1, NUMB_MECH_SPC
              JOLD2NEW( I,1 ) = I
              JNEW2OLD( I,1 ) = I
            END DO

            ALLOCATE( SORTED_COUNT(  NUMB_MECH_SPC),
     &                JACOBIAN_COUNT( NUMB_MECH_SPC), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
                 MSG = 'Error allocating JACOBIAN_COUNT'
                 WRITE(ERROR_LOG,*)TRIM(MSG)
                 EFLAG = .TRUE.
            END IF
            JACOBIAN_COUNT = 0
            SORTED_COUNT   = 0
            
            ALLOCATE( SWAPPED_NAMES( NUMB_MECH_SPC), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
                 MSG = 'Error allocating SWAPPED_NAMES'
                 WRITE(ERROR_LOG,*)TRIM(MSG)
                 EFLAG = .TRUE.
            END IF
            SWAPPED_NAMES = 'BLANK'
            
            ALLOCATE( ISPECIES_REACTION(MXPRD+3,NRXNS), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
                 MSG = 'Error allocating ISPECIES_REACTION'
                 WRITE(ERROR_LOG,*)TRIM(MSG)
                 EFLAG = .TRUE.
            END IF
            DO RXN = 1,NRXNS
               DO SPC = 1,MXPRD+3
                  ISPECIES_REACTION( SPC,RXN ) = IRR( RXN,SPC )
               END DO
            END DO

            ALLOCATE( DYDT( NUMB_MECH_SPC ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = 'Error allocating DYDT array'
               WRITE(ERROR_LOG,*)TRIM(MSG)
               EFLAG = .TRUE.
            END IF

            IF ( EFLAG  ) THEN
               MSG = 'Above Fatal Error encountered '
               WRITE(ERROR_LOG,*)TRIM(MSG)
               CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
            END IF
            
! map species time derivative and jacobian array
            CALL CHART_IRR

!  Set the number of Partial derivative terms in the Jacobian and
!  count the number of terms for each species
             DO I = 1, NUMB_MECH_SPC 
                DO  J = 1, NUMB_MECH_SPC
                   IF ( JACOBIAN_FILLED( J,I,1 ) ) THEN
                       JACOBIAN_COUNT( J ) = JACOBIAN_COUNT( J ) + 1
                   END IF
                END DO
             END DO

!  Sort the species, putting all with zero partial derivative 
!  terms at the bottom and those with fewest PD terms at top.
!  Set arrays for species with zero PD terms
      STEADY_SPECIES = NUMB_MECH_SPC
      DO JOLD = 1, NUMB_MECH_SPC
         IF ( JACOBIAN_COUNT( JOLD ) .GT. 0 ) THEN
            CHANGING_SPECIES = CHANGING_SPECIES + 1
            JNEW = CHANGING_SPECIES
            JNEW2OLD( JNEW,1 ) = JOLD
            JOLD2NEW( JOLD,1 ) = JNEW
         ELSE
            JNEW2OLD( CHANGING_SPECIES,1 ) = JOLD
            JOLD2NEW( JOLD,1 ) = STEADY_SPECIES
            STEADY_SPECIES = STEADY_SPECIES - 1
         END IF
      END DO
      STEADY_SPECIES = NUMB_MECH_SPC-CHANGING_SPECIES 
      IF (CHECK_SORTING )THEN
          WRITE( DDM_LOG,'(A)')'Initial Mechanism Species sorted based on their mechanism activity'
          WRITE( DDM_LOG,'(A,2(I4,1X))')'Changeing Species, Steady State Species ',CHANGING_SPECIES,
     &    STEADY_SPECIES          
          WRITE( DDM_LOG,'(A16,1X,4(A16,1X))')'ISPECIES', 'Original','Sorted','Check','JCOUNT'
          DO ISPC = 1, CHANGING_SPECIES
             I = JOLD2NEW( ISPC,1 )
             J = JNEW2OLD( I,1 )
             WRITE(DDM_LOG,'(I4,13X,3(A16,1X),I16)')ISPC,
     &       CHEMISTRY_SPC(ISPC),CHEMISTRY_SPC(I),CHEMISTRY_SPC(J),JACOBIAN_COUNT(I)     
          END DO
       END IF

!  Now sort by number of PD terms, fewest at position 1, most at
!  the end position. 
      DO JNEW = 1, CHANGING_SPECIES
c  Uncomment the following three lines to turn off species ordering;
c  not recommended since computational efficiency reduced
!        INEW2OLD( JNEW,NCS ) = JNEW
!        IOLD2NEW( JNEW,NCS ) = JNEW
!        IF ( JNEW .NE. 0 ) CYCLE
         JOLD = JNEW2OLD( JNEW,1 )
         MINVALU = JACOBIAN_COUNT( JOLD )
         IMINOLD = JOLD
         IMINNEW = JNEW

         DO INEW = JNEW + 1, CHANGING_SPECIES
            IOLD = JNEW2OLD( INEW,1 )
           IF ( JACOBIAN_COUNT( IOLD ) .LT. MINVALU ) THEN
               MINVALU = JACOBIAN_COUNT( IOLD )
               IMINOLD = IOLD
               IMINNEW = INEW
           END IF
         END DO

         JNEW2OLD( IMINNEW,1 ) = JOLD
         JNEW2OLD( JNEW,1 )    = IMINOLD
         JOLD2NEW( JOLD,1 )    = IMINNEW
         JOLD2NEW( IMINOLD,1 ) = JNEW

      END DO
      DO J = 1,NUMB_MECH_SPC
         JNEW = JOLD2NEW( J,1 )
         SORTED_COUNT(JNEW) = JACOBIAN_COUNT( J )
      END DO
      JACOBIAN_COUNT = SORTED_COUNT
! Alternative method for sorting species for species names      
!      SWAPPED_NAMES = CHEMISTRY_SPC
!      DO J = CHANGING_SPECIES-1,1,-1
!         SWAPPING = .FALSE.
!         DO I = 1,J
!            IF ( JACOBIAN_COUNT(I) .GT. JACOBIAN_COUNT(I+1) )THEN
!                MINVALU             = JACOBIAN_COUNT(I+1)
!                 JACOBIAN_COUNT(I+1) = JACOBIAN_COUNT(I)
!                 JACOBIAN_COUNT(I)   = MINVALU
!                 SWAPPED_NAME        = SWAPPED_NAMES(I+1)
!                 SWAPPED_NAMES(I+1)  = SWAPPED_NAMES(I)
!                 SWAPPED_NAMES(I)    = SWAPPED_NAME
!                 SWAPPING            = .TRUE.
!            END IF
!         END DO
!         IF( .NOT. SWAPPING ) EXIT
!      END DO
!      DO I = 1, NUMB_MECH_SPC
!         WRITE(DDM_LOG,'(I4,1X,A)')I,SWAPPED_NAMES(I)
!      END DO
!      DO J = 1,CHANGING_SPECIES
!         DO I = 1,CHANGING_SPECIES
!            IF( TRIM(CHEMISTRY_SPC(J)) .EQ. TRIM( SWAPPED_NAMES(I) ) )THEN
!               JOLD2NEW(J,1) = I
!               JNEW2OLD(I,1) = J         
!               WRITE(DDM_LOG,'(I4,1X,A,2(1X,I4))')J,CHEMISTRY_SPC(J),JOLD2NEW(J,1),JNEW2OLD(I,1)
!            END IF   
!         END DO
!      END DO
      IF (CHECK_SORTING )THEN
! Write sorting results      
          WRITE( DDM_LOG,'(A)')'Mechanism Species sorted based on their mechanism activity'
          WRITE( DDM_LOG,'(A,2(I4,1X))')'Changeing Species, Steady State Species ',CHANGING_SPECIES,
     &    STEADY_SPECIES          
          WRITE( DDM_LOG,'(A16,1X,4(A16,1X))')'ISPECIES', 'Original','Sorted','Check','JCOUNT'
          DO ISPC = 1, NUMB_MECH_SPC
             I = JOLD2NEW( ISPC,1 )
             J = JNEW2OLD( I,1 )
             S = JNEW2OLD( ISPC,1 )
             WRITE(DDM_LOG,'(I4,13X,3(A16,1X),I16)')ISPC,
     &       CHEMISTRY_SPC(ISPC),CHEMISTRY_SPC(S),CHEMISTRY_SPC(J),JACOBIAN_COUNT(ISPC)
     
          END DO
      END IF
      
      RESET_JACOBIAN = .TRUE.

!  Reset the ISPECIES_REACTION array using the new species order developed above.
      
      DO RXN = 1, NRXNS
          DO I = 1, NREACT( RXN )
             J = ISPECIES_REACTION( I,RXN )
             ISPECIES_REACTION( I,RXN ) = JOLD2NEW( J,1 )
          END DO

          DO I = 1, NPRDCT( RXN )
             J = ISPECIES_REACTION( I+3,RXN )
             ISPECIES_REACTION( I+3,RXN ) = JOLD2NEW( J,1 )
         END DO
      END DO
      DEALLOCATE( JACOBIAN_COUNT, SORTED_COUNT )
      
        IF( CHECK_MECHANISM )CHECK_MECHANISM = .FALSE. 

95000   FORMAT(I4,1X,A16,' = ',4(ES12.4,', ')) 
95001   FORMAT('At JDATE, JTIME, DTSTEP = ',(I7.7,1X,I6.6,1X,I6.6))   
95002   FORMAT('At JDATE, JTIME, DTSTEP = ',(I7.7,1X,I6.6,1X,I6.6),
     &         ': Sun Down, Photolysis off')   
95003   FORMAT('At JDATE, JTIME, DTSTEP = ',(I7.7,1X,I6.6,1X,I6.6),
     &         ': Sun Up,   Photolysis on') 
        END SUBROUTINE SET_MECHANISM
      SUBROUTINE CHART_IRR()

          USE RXNS_DATA
          USE UTILIO_DEFN
C Initialize arrays and maps that store reaction rates in each grid cell and that
C         relate ISAM species to chemistry species
C
C         Called by chemistry driver

        IMPLICIT NONE

C..Includes:
!         INCLUDE SUBST_CONST     ! CMAQ constants
 
         CHARACTER( 16 ), PARAMETER :: PNAME = 'CHART_IRR'     ! Program name

         INTEGER :: I, J, RXN, IP, IPNEG, IL 
         INTEGER :: IPROD, ILOSS, IREACT
         INTEGER :: IOSTAT
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         INTEGER :: IOS
         REAL(8) :: COEFF


         CHARACTER( 132 ) :: MSG           ! Message text

! temporary variables to define REACTION_CHART

         INTEGER, ALLOCATABLE :: ISPECIES_NPROD( : )
         INTEGER, ALLOCATABLE :: ISPECIES_PROD( : )
         INTEGER, ALLOCATABLE :: ISPECIES_LOSS( : )
         INTEGER, ALLOCATABLE :: COEFF_NPROD( : )
         REAL(8), ALLOCATABLE :: COEFF_PROD( : )
         REAL(8), ALLOCATABLE :: COEFF_LOSS( : )        
     
         INTEGER              :: JACOB_OCCURANCES
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_INDEX( : )
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_ROW( : )
         INTEGER, ALLOCATABLE :: JACOB_PARTIAL_COL( : )
         REAL(8), ALLOCATABLE :: JACOB_PARTIAL_COEFF( : )
C=======================================================

        !DDM_LOG = 6
        
        ALLOCATE( ISPECIES_NPROD( MXPRD ),
     &            ISPECIES_PROD( MXPRD ),
     &            ISPECIES_LOSS( 3 ),
     &            COEFF_NPROD( MXPRD ),
     &            COEFF_PROD( MXPRD ),
     &            COEFF_LOSS( 3 ),         STAT = IOS )     
     
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating IREACTION and COEFF_NET arrays'
           CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
        END IF 
        
        ALLOCATE( JACOB_PARTIAL_INDEX(3*NUMB_MECH_SPC*NRXNS),
     &            JACOB_PARTIAL_COEFF(3*NUMB_MECH_SPC*NRXNS),
     &            JACOB_PARTIAL_ROW(3*NUMB_MECH_SPC*NRXNS),  
     &            JACOB_PARTIAL_COL(3*NUMB_MECH_SPC*NRXNS),   STAT = IOS ) 
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating JACOB arrays'
           CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
        END IF 
        
! set matrix noting where day/night jacobian of chemistry ODE is always zero.
        ALLOCATE( JACOBIAN_FILLED( NUMB_MECH_SPC,NUMB_MECH_SPC,2 ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating YDOT array'
           CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
        END IF
        JACOBIAN_FILLED = .FALSE.
        DO I = 1, NUMB_MECH_SPC  ! set diagonal to to true
           JACOBIAN_FILLED(I,I,1:2) = .TRUE.
        END DO
            
! find each reaction affect each reactant and product
        ALLOCATE( REACTION_CHART( NRXNS ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating MECHANISM_BUDGET'
           CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
        END IF 
        DO R = 1, NRXNS
           IP = 0
           IL = 0
           IPNEG = 0
           JACOB_OCCURANCES    = 0
           JACOB_PARTIAL_INDEX = 0
           JACOB_PARTIAL_COEFF = 0.0D0
           JACOB_PARTIAL_ROW = 0
           JACOB_PARTIAL_COL = 0
           DO SPC = 1, NUMB_MECH_SPC
              COEFF = ASSESS_REACTION( SPC, R, ILOSS, IPROD )
              IF( ABS( COEFF ) .GT. 1.0D-8 )THEN
                 IF( COEFF .GT. 0.0D0 )THEN
                    IP = IP + 1
                    ISPECIES_PROD( IP ) = SPC   
                    COEFF_PROD   ( IP ) = COEFF
                 ELSE IF( COEFF .LT. 0.0D0 )THEN
                    IF( ILOSS .GT. 0 )THEN
                       IL = IL + 1
                       ISPECIES_LOSS( IL ) = SPC   
                       COEFF_LOSS   ( IL ) = COEFF
                    ELSE
                       IP   = IP + 1   
                       IPNEG = IPNEG + 1
                       ISPECIES_PROD( IP ) = SPC   
                       COEFF_PROD   ( IP ) = COEFF
                       ISPECIES_NPROD( IPNEG ) = SPC   
                       COEFF_NPROD   ( IPNEG ) = COEFF
                    END IF
                 END IF
                 DO IREACT = 1, NREACT( R )
                    JACOB_OCCURANCES = JACOB_OCCURANCES + 1
                    JACOB_PARTIAL_ROW(JACOB_OCCURANCES) = SPC
                    JACOB_PARTIAL_COL(JACOB_OCCURANCES) = IRR(R,IREACT)
                    JACOBIAN_FILLED( SPC, IRR(R,IREACT), 1 )  = .TRUE.
                    IF ( KTYPE( R ) .NE. 0 .AND. KTYPE( R ) .NE. 12 ) THEN
                       JACOBIAN_FILLED( SPC, IRR(R,IREACT), 2 )  = .TRUE.
                    ELSE 
                       REACTION_CHART( R )%LIGHT_DEPENDENT       = .TRUE.
                    END IF
                    JACOB_PARTIAL_INDEX(JACOB_OCCURANCES)  = IREACT
                    JACOB_PARTIAL_COEFF(JACOB_OCCURANCES)  = COEFF
                 END DO   
              END IF
           END DO
           IF( JACOB_OCCURANCES .GT. 0 ) THEN
               REACTION_CHART( R )%JACOB_OCCURANCES = JACOB_OCCURANCES
               JACOBIAN_TERMS = JACOBIAN_TERMS + JACOB_OCCURANCES
               ALLOCATE( REACTION_CHART( R )%JACOB_PARTIAL_INDEX ( JACOB_OCCURANCES ),
     &                   REACTION_CHART( R )%JACOB_PARTIAL_ROW   ( JACOB_OCCURANCES ),                        
     &                   REACTION_CHART( R )%JACOB_PARTIAL_VECTOR( JACOB_OCCURANCES ),
     &                   REACTION_CHART( R )%JACOB_PARTIAL_COEFF ( JACOB_OCCURANCES ),
     &                   REACTION_CHART( R )%JACOB_PARTIAL_COL   ( JACOB_OCCURANCES ),  STAT = IOS )                      
               DO S = 1,  JACOB_OCCURANCES
                  REACTION_CHART( R )%JACOB_PARTIAL_ROW( S )   = JACOB_PARTIAL_ROW( S )
                  REACTION_CHART( R )%JACOB_PARTIAL_COL( S )   = JACOB_PARTIAL_COL( S )
                  REACTION_CHART( R )%JACOB_PARTIAL_INDEX ( S ) = JACOB_PARTIAL_INDEX( S )
                  REACTION_CHART( R )%JACOB_PARTIAL_VECTOR( S ) = 0
                  REACTION_CHART( R )%JACOB_PARTIAL_COEFF ( S ) = JACOB_PARTIAL_COEFF( S )
               END DO 
           END IF 
           REACTION_CHART( R )%NREACTANTS = NREACT( R )
           REACTION_CHART( R )%REACTANT( 1:3 ) = IRR( R,1:3 )
           IF( IP .GT. 0 )THEN
               REACTION_CHART( R )%NSPECIES_PRODUCED = IP
               ALLOCATE( REACTION_CHART( R )%ISPECIES_PRODUCED( IP ),
     &                   REACTION_CHART( R )%COEFF_PROD( IP ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production MECHANISM_BUDGET arrays'
           CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
               END IF 
               REACTION_CHART( R )%ISPECIES_PRODUCED( 1:IP ) = ISPECIES_PROD( 1:IP )
               REACTION_CHART( R )%COEFF_PROD( 1:IP )        = COEFF_PROD( 1:IP ) 
           END IF 
           IF( IPNEG .GT. 0 )THEN
               REACTION_CHART( R )%NSPECIES_NPRODUCED = IPNEG
               ALLOCATE( REACTION_CHART( R )%ISPECIES_NPRODUCED( IPNEG ),
     &                   REACTION_CHART( R )%COEFF_NPROD( IPNEG ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production MECHANISM_BUDGET arrays'
                    CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
               END IF 
               REACTION_CHART( R )%ISPECIES_NPRODUCED( 1:IPNEG ) = ISPECIES_NPROD( 1:IPNEG )
               REACTION_CHART( R )%COEFF_NPROD( 1:IPNEG )        = COEFF_NPROD( 1:IPNEG ) 
           END IF 
           IF( IL .GT. 0 )THEN
               REACTION_CHART( R )%NSPECIES_DESTROYED = IL
               ALLOCATE( REACTION_CHART( R )%ISPECIES_DESTROYED( IL ),
     &                   REACTION_CHART( R )%COEFF_LOSS( IL ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production MECHANISM_BUDGET arrays'
                    CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
               END IF 
               REACTION_CHART( R )%ISPECIES_DESTROYED( 1:IL ) = ISPECIES_LOSS( 1:IL )
               REACTION_CHART( R )%COEFF_LOSS( 1:IL )         = COEFF_LOSS( 1:IL ) 
           END IF 
        END DO


        DEALLOCATE( ISPECIES_LOSS,
     &              ISPECIES_PROD,
     &              ISPECIES_NPROD,
     &              COEFF_PROD,
     &              COEFF_NPROD,
     &              COEFF_LOSS )     

        IF ( REPORT_CHART ) THEN
! report budget for mechanism reaction chart 
            CALL REPORT_REACTION_CHART( DDM_LOG ) 
        END IF    
        
        END SUBROUTINE CHART_IRR
      SUBROUTINE REPORT_REACTION_CHART( OUT_UNIT )
!        purpose writes out 
         USE RXNS_DATA

         IMPLICIT NONE


!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC, ISPC, JSPC, KSPC, LSPC
        INTEGER IL, IR, IRXN, NR
        REAL(8) FACTOR
         
        CHARACTER( 56 ), ALLOCATABLE :: RXN_STRING
        CHARACTER( 56 ), ALLOCATABLE :: REACTION_STRING( : )
        
        LOGICAL, SAVE :: REACTION_AFFECTS = .FALSE.
        
        ALLOCATE( REACTION_STRING( NRXNS ) )
        
        IF ( REACTION_AFFECTS ) THEN
            DO IRXN = 1, NRXNS
               RXN_STRING = RXLABEL( IRXN )
               DO NR = 1, NREACT( IRXN )
                  IR = IRR( IRXN,NR )
                  !IR = JNEW2OLD( IR,1 )
                  IF( NR .EQ. 1 )THEN
                    RXN_STRING = TRIM( RXN_STRING )
     &                         // ': '  // TRIM( CHEMISTRY_SPC( IR ) )
                  ELSE
                    RXN_STRING = TRIM( RXN_STRING ) 
     &                         // ' + ' // TRIM( CHEMISTRY_SPC( IR ) )
                  END IF
               END DO   
               WRITE(OUT_UNIT,'(A)')TRIM( RXN_STRING ) // ' has below net coefficients and species '
               WRITE(OUT_UNIT,'(4X,A)')'Reactants Lost:'
               DO NR = 1, REACTION_CHART( IRXN )%NSPECIES_DESTROYED
                  IR = REACTION_CHART( IRXN )%ISPECIES_DESTROYED( NR )
                  !IR = JNEW2OLD( IR,1 )
                  WRITE(OUT_UNIT,97001)REACTION_CHART( IRXN )%COEFF_LOSS( NR ),CHEMISTRY_SPC( IR )
               END DO
               IF( REACTION_CHART( IRXN )%NSPECIES_NPRODUCED .GT. 0 )THEN
                 WRITE(OUT_UNIT,'(4X,A)')'Products Yielded (Note Negative Product Coefficients):'
               ELSE   
                 WRITE(OUT_UNIT,'(4X,A)')'Products Yielded:'
               END IF  
               DO NR = 1, REACTION_CHART( IRXN )%NSPECIES_PRODUCED
                  IR = REACTION_CHART( IRXN )%ISPECIES_PRODUCED( NR )
                  !IR = JNEW2OLD( IR,1 )
                  WRITE(OUT_UNIT,97001)REACTION_CHART( IRXN )%COEFF_PROD( NR ),CHEMISTRY_SPC( IR )
               END DO
            END DO
        END IF
! create string containing reactants for individual reactions
        DO IRXN = 1, NRXNS
            REACTION_STRING( IRXN ) = RXLABEL( IRXN )
            DO NR = 1, NREACT( IRXN )
               IR = IRR( IRXN,NR )
               !IR = JNEW2OLD( IR,1 )
               IF( NR .EQ. 1 )THEN
                  REACTION_STRING( IRXN ) = TRIM( REACTION_STRING( IRXN ) )
     &                                    // ': '  // TRIM( CHEMISTRY_SPC( IR ) )
               ELSE
                  REACTION_STRING( IRXN ) = TRIM( REACTION_STRING( IRXN ) ) 
     &                                   // ' + ' // TRIM( CHEMISTRY_SPC( IR ) )
               END IF
           END DO    
        END DO   
! write out species loss and production rates
        DO ISPC = 1, NUMB_MECH_SPC
           JSPC = ISPC ! JNEW2OLD( ISPC,1 )
           WRITE(OUT_UNIT,*)"ISPC, JSPC: ",ISPC,JSPC
           WRITE(OUT_UNIT,'(A)')'LOSS(' // TRIM( CHEMISTRY_SPC( JSPC ) ) //  ') =  0.0 '
           DO IRXN = 1, NRXNS
              DO NR = 1, REACTION_CHART( IRXN )%NSPECIES_DESTROYED
                IR = REACTION_CHART( IRXN )%ISPECIES_DESTROYED( NR )
                !IR = JNEW2OLD( IR,1 )
                IF ( IR .EQ. JSPC ) THEN
                   FACTOR = REACTION_CHART( IRXN )%COEFF_LOSS( NR )
                   IF( FACTOR .GT. 0.0D0 )THEN
                      WRITE(OUT_UNIT,97002)'& + ',abs(FACTOR),
     &                '*Reaction(' // TRIM( RXLABEL( IRXN ) ) // ') ! '
     &                // TRIM( REACTION_STRING( IRXN ) )                               
                   ELSE
                      WRITE(OUT_UNIT,97002)'& - ',abs(FACTOR),
     &                '*Reaction(' // TRIM( RXLABEL( IRXN ) ) // ') ! '
     &                // TRIM( REACTION_STRING( IRXN ) )                               
                   END IF
                END IF
             END DO
           END DO          
           WRITE(OUT_UNIT,'(A)')'PROD(' // TRIM( CHEMISTRY_SPC( JSPC ) ) //  ') =  0.0 '
           DO IRXN = 1, NRXNS
              DO NR = 1, REACTION_CHART( IRXN )%NSPECIES_PRODUCED
                IR = REACTION_CHART( IRXN )%ISPECIES_PRODUCED( NR )
                !IR = JNEW2OLD( IR,1 )
                IF ( IR .EQ. JSPC ) THEN
                   FACTOR = REACTION_CHART( IRXN )%COEFF_PROD( NR )
                   IF( FACTOR .GT. 0.0D0 )THEN
                      WRITE(OUT_UNIT,97002)'& + ',abs(FACTOR),
     &                '*Reaction(' // TRIM( RXLABEL( IRXN ) )  // ') ! '                                   
     &                // TRIM( REACTION_STRING( IRXN ) )                               
                   ELSE
                      WRITE(OUT_UNIT,97002)'& - ',abs(FACTOR),
     &                '*Reaction(' // TRIM( RXLABEL( IRXN ) )  // ') ! '
     &                // TRIM( REACTION_STRING( IRXN ) )                               
                   END IF
                END IF
             END DO
           END DO          
        END DO        
        
! write out Jacobian(i,j) values
        DO ISPC = 1, NUMB_MECH_SPC
           KSPC = ISPC ! JNEW2OLD( ISPC,1 )
           DO JSPC = 1, NUMB_MECH_SPC
              LSPC = JSPC ! JNEW2OLD( JSPC,1 )
              WRITE(OUT_UNIT,'(A)')'JACOBIAN( ' // TRIM( CHEMISTRY_SPC( KSPC ) )
     &        // ',' // TRIM( CHEMISTRY_SPC( LSPC ) ) // ' ) = 0.0 '
              DO IRXN = 1, NRXNS
                 DO IL = 1, REACTION_CHART( IRXN )%JACOB_OCCURANCES
                    IF( REACTION_CHART( IRXN )%JACOB_PARTIAL_ROW( IL ) .EQ. KSPC 
     &                     .AND. REACTION_CHART( IRXN )%JACOB_PARTIAL_COL( IL ) .EQ. LSPC )THEN
                        SPC = REACTION_CHART( IRXN )%JACOB_PARTIAL_INDEX( IL )
                        !SPC = JNEW2OLD( SPC,1 )
                        IF( REACTION_CHART( IRXN )%JACOB_PARTIAL_COEFF( IL ) .GT. 0.0 )THEN
                           WRITE(OUT_UNIT,97002)'&  + ',
     &                      abs( REACTION_CHART( IRXN )%JACOB_PARTIAL_COEFF( IL ) ),
     &                     '*dReaction(' ,TRIM( RXLABEL( IRXN ) ),')/d'  // TRIM( CHEMISTRY_SPC( IRR(IRXN,SPC) ) )                      
                        ELSE
                           WRITE(OUT_UNIT,97002)'&  - ',abs(REACTION_CHART( IRXN )%JACOB_PARTIAL_COEFF( IL )),
     &                     '*dReaction(',TRIM( RXLABEL( IRXN ) ),')/d' // TRIM( CHEMISTRY_SPC( IRR(IRXN,SPC) ) )                      
                        END IF
                    END IF
                END DO
              END DO                  
           END DO
        END DO   
        
97001   FORMAT(7X,F7.4,'*',A)
97002   FORMAT(5X,A,F8.5,A,A,A)

      END SUBROUTINE REPORT_REACTION_CHART
      SUBROUTINE EVALUATE_F_JAC_MECH( YIN, RKI, JAC )

C***********************************************************************
C
C  Function: Compute the Jacobian matrix, [J] ( Jij = d[dCi/dt]/dCj )
C
C  Preconditions: None
C
C  Key Subroutines/Functions Called: None
C
C***********************************************************************

      USE RXNS_DATA

      IMPLICIT NONE

C..Includes:

C..Arguments:
      REAL( 8 ), INTENT( IN ) :: YIN( : )    ! species concs, ppm
      REAL( 8 ), INTENT( IN ) :: RKI(  : )   ! Rate constants so reaction rates are ppm//min
      REAL( 4 ), INTENT(OUT)  :: JAC( :,:)   ! jacobian values 

C..Parameters: None

C..External Functions: None

C..Local Variables:
      INTEGER JR1, JR2, JR3  ! Pointer to reactant species conc.
      INTEGER JROW           ! Jacobian Row
      INTEGER JCOL           ! Jacobian Column
      INTEGER NP             ! Loop index over partial derivation terms
      INTEGER IPART          ! index for partial derivation of reaction
      INTEGER NRK            ! Reaction number
      INTEGER IOS
      
      REAL( 8 ) :: CR2           ! Temporary product for 3 reactant reaction
      REAL( 8 ) :: FRACN         ! Stoichiometric coefficient
      REAL( 8 ) :: EXPLIC( 3 )   ! Reaction partial derivatives
      
      LOGICAL, SAVE :: INITIALIZE = .TRUE.

      INTEGER, ALLOCATABLE       :: JACOBIAN_COL( : )
      INTEGER, ALLOCATABLE       :: JACOBIAN_ROW( : )
      CHARACTER(16), ALLOCATABLE :: JACOBIAN_COL_SPECIES( : )
      CHARACTER(16), ALLOCATABLE :: JACOBIAN_ROW_SPECIES( : )
      
C***********************************************************************

      IF( RESET_JACOBIAN )THEN ! reroder row and columns of Jacobian
                               ! using sorted species maps
      
          DO NRK = 1, NRXNS
             DO NP = 1, REACTION_CHART( NRK )%JACOB_OCCURANCES             
                JROW  = REACTION_CHART( NRK )%JACOB_PARTIAL_ROW( NP ) 
                JCOL  = REACTION_CHART( NRK )%JACOB_PARTIAL_COL( NP ) 
                JROW = JOLD2NEW( JROW,1 )
                JCOL = JOLD2NEW( JCOL,1 )
                REACTION_CHART( NRK )%JACOB_PARTIAL_ROW( NP ) = JROW
                REACTION_CHART( NRK )%JACOB_PARTIAL_COL( NP ) = JCOL
             END DO
          END DO

          DO NRK = 1, NRXNS
             DO NP = 1,REACTION_CHART( NRK )%NREACTANTS
                JR1 = JOLD2NEW( REACTION_CHART( NRK )%REACTANT( NP ),1 )
                REACTION_CHART( NRK )%REACTANT( NP ) = JR1
             END DO
          END DO
        
          RESET_JACOBIAN = .FALSE.

      END IF
      IF( INITIALIZE )THEN
          INITIALIZE = .FALSE.
      END IF
c...initialize Jacobian
      JAC( :,: )  = 0.0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reaction rates adding partial derivatives; EXPLIC
c  holds the values according to number of reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOOP_REACTIONS: DO NRK = 1, NRXNS
         IF( REACTION_CHART( NRK )%LIGHT_DEPENDENT .AND. .NOT. LSUNLIGHT )CYCLE    
c...partial derivatives for reactions with 1 reactant
         SELECT CASE ( REACTION_CHART( NRK )%NREACTANTS )
            CASE( 1 ) 
               EXPLIC( 1 ) = RKI( NRK ) 
c...partial derivatives for reactions with 2 reactants
            CASE( 2 ) 
               JR1 = REACTION_CHART( NRK )%REACTANT( 1 )
               JR2 = REACTION_CHART( NRK )%REACTANT( 2 )
               EXPLIC( 1 )  = RKI( NRK )
     &                      * YIN( JR2 )
               EXPLIC( 2 )  = RKI( NRK )
     &                      * YIN( JR1 ) 
c.....partial derivatives for reactions with 3 reactants
            CASE( 3 ) 
               JR1 = REACTION_CHART( NRK )%REACTANT( 1 )
               JR2 = REACTION_CHART( NRK )%REACTANT( 2 )
               JR3 = REACTION_CHART( NRK )%REACTANT( 3 )
               CR2 = RKI( NRK ) * YIN( JR2 )
               EXPLIC( 1 ) = CR2 * YIN( JR3 )
               EXPLIC( 2 ) = RKI( NRK )
     &                     * YIN( JR1 )
     &                     * YIN( JR3 ) 
               EXPLIC( 3 ) = CR2 * YIN( JR1 )
         END SELECT
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add Reaction's Partial Derivative to Jacobian 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          DO NP = 1, REACTION_CHART( NRK )%JACOB_OCCURANCES          
             JROW  = REACTION_CHART( NRK )%JACOB_PARTIAL_ROW( NP ) 
             JCOL  = REACTION_CHART( NRK )%JACOB_PARTIAL_COL( NP ) 
             FRACN = REACTION_CHART( NRK )%JACOB_PARTIAL_COEFF( NP )
             IPART = REACTION_CHART( NRK )%JACOB_PARTIAL_INDEX( NP )
             JAC( JROW, JCOL ) = JAC( JROW, JCOL ) + REAL(FRACN * EXPLIC( IPART ) )
          END DO
      END DO LOOP_REACTIONS
            
      IF( .NOT. CHECK_MECHANISM )RETURN
      
      DO JROW = 1, NUMB_MECH_SPC
         JR1 = JROW ! JOLD2NEW( JROW,1 )
         DO JCOL = 1, NUMB_MECH_SPC
              JR2 = JCOL ! JOLD2NEW( JCOL,1 )
              WRITE(6,'(2(I4,1X),A32,ES16.6)')JR1,JR2,
     &         'JACOBIAN( ' // TRIM( CHEMISTRY_SPC( JROW ) )
     &        // ',' // TRIM( CHEMISTRY_SPC( JCOL ) ) // ' ) = ', JAC( JR1, JR2 )
          END DO
      END DO   
      

      RETURN 
      END SUBROUTINE EVALUATE_F_JAC_MECH
       SUBROUTINE EVALUATE_F_MECH( YIN, TAIR, DAIR, RKI, YDOT )

C***********************************************************************
C
C  Function:  Compute YDOT = dc/dt for each species. YDOT is the
C             net rate of change in species concentrations resulting
C             from chemical production minus chemical loss.
C
C  Preconditions: None
C                                                                     
C  Key Subroutines/Functions Called: None
C
C***********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION


      IMPLICIT NONE

C..Includes:

C..Arguments:
      REAL( 8 ), INTENT( IN )    :: YIN ( : )  ! Species concs, ppm
      REAL( 8 ), INTENT( IN )    :: TAIR       ! air temperature, K
      REAL( 8 ), INTENT( IN )    :: DAIR       ! air density, Kg/m3
      REAL( 8 ), INTENT( INOUT ) :: RKI ( : )  ! Rate constants so reaction rates are ppm//min
      REAL( 8 ), INTENT(   OUT ) :: YDOT( : )  ! Species rates of change, ppm/min
C..Parameters: None

C..External FUNCTIONS: None

C..Local Variables:
      INTEGER :: ISP              ! Loop index for species
      INTEGER :: ISP1, ISP2, ISP3 ! Pointers to species numbers
      INTEGER :: NP               ! Loop index for number of products
      INTEGER :: NR               ! Loop index for number of reactants
      INTEGER :: NRK              ! Loop index for number of reactions
      INTEGER :: NCELL 

C***********************************************************************      

       IF ( NSPECIAL_RXN .GT. 0 ) THEN  ! calculate special rate coefficients
           SYC( NCELL, 1:NUMB_MECH_SPC ) = YIN( 1:NUMB_MECH_SPC ) 
           TEMP = TAIR
           DENS = DAIR
           CALL SPECIAL_RATES( 1, SYC, TEMP, DENS, RKI_SAV )
           RKI( 1:NRXNS ) = RKI_SAV( NCELL, 1:NRXNS )
       END IF         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize dc/dt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!      YDOT = 0.0D0
      PROD = 0.0D0
      LOSS = 0.0D0   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reactions to calculate dc/dt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LOOP_REACTIONS: DO NRK = 1, NRXNS
c..Calculate reaction rate
         SELECT CASE ( REACTION_CHART( NRK )%NREACTANTS )
            CASE( 1 ) 
               ISP1 = REACTION_CHART( NRK )%REACTANT( 1 )
               RXRAT( NRK ) = RKI( NRK ) * YIN( ISP1 )
            CASE( 2 ) 
c... reactions with 2 reactants
               ISP1 = REACTION_CHART( NRK )%REACTANT( 1 )
               ISP2 = REACTION_CHART( NRK )%REACTANT( 2 )
               RXRAT( NRK ) = RKI( NRK )
     &                      * YIN( ISP1 ) 
     &                      * YIN( ISP2 )
            CASE( 3 ) 
c..... reactions with 3 reactants
               ISP1 = REACTION_CHART( NRK )%REACTANT( 1 )
               ISP2 = REACTION_CHART( NRK )%REACTANT( 2 )
               ISP3 = REACTION_CHART( NRK )%REACTANT( 3 )
               RXRAT( NRK) = RKI( NRK )
     &                     * YIN( ISP1 ) 
     &                     * YIN( ISP2 )
     &                     * YIN( ISP3 ) 
         END SELECT

         
c..Subtract loss terms from dc/dt for this reaction 
         DO NR = 1, REACTION_CHART( NRK )%NSPECIES_DESTROYED
            ISP = REACTION_CHART( NRK )%ISPECIES_DESTROYED( NR )
            LOSS( ISP ) = LOSS( ISP )
     &                  + REACTION_CHART( NRK )%COEFF_LOSS( NR ) 
     &                  * RXRAT( NRK )
         END DO
         WHERE( YIN .GT. 1.000001D-30 )
             LOSSF = ABS(LOSS) / YIN
         ELSE WHERE
            LOSSF = 0.0D0
         END WHERE
c..Add production terms to dc/dt for this reaction
         DO NP = 1, REACTION_CHART( NRK )%NSPECIES_PRODUCED
            ISP = REACTION_CHART( NRK )%ISPECIES_PRODUCED( NP )
            PROD( ISP ) = PROD( ISP )
     &                  + REACTION_CHART( NRK )%COEFF_PROD( NP) 
     &                  * RXRAT( NRK )
         END DO
       END DO LOOP_REACTIONS

       YDOT = PROD + LOSS
       IF( .NOT. CHECK_MECHANISM )RETURN

       !DYDT = YDOT       
       DO ISP = 1, NUMB_MECH_SPC
          ISP1 = ISP ! JNEW2OLD (ISP,1)
          WRITE( DDM_LOG,95001 )CHEMISTRY_SPC( ISP1 ),YDOT(ISP),YIN( ISP ) ! PROD( ISP ),LOSS(ISP)
       END DO
95001  FORMAT( 'YDOT(',A16,') = ',ES16.6,1X,'; CONC = ',ES16.6,1X,ES16.6 )       
      RETURN
      END SUBROUTINE EVALUATE_F_MECH
      REAL(8) FUNCTION EFFECT_REACTION( NAMINDX, NRX, OCCURS )

C-----------------------------------------------------------------------
C Function: To find net effect on the number of species molecules from a reaction 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Bill Hutzell, May, 2018
C-----------------------------------------------------------------------
      USE RXNS_DATA

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      INTEGER,        INTENT(IN )   :: NAMINDX  ! Index for chemistry species 
      INTEGER,        INTENT(IN )   :: NRX      ! Reaction number
      INTEGER,        INTENT(INOUT) :: OCCURS   ! Number of products and reaction 
                                        
C Parameters: None

C External Functions: None 

C Local Variables:

      CHARACTER( 16 ) :: SPECIS    ! Species name to check

      INTEGER INDX       ! Pointer to reactant or product in CHEMISTRY_SPC array
      INTEGER IRRPNTR    ! Pointer to reactant or product in IRR array
      INTEGER N          ! Loop index over IRR array

      REAL(8) TOTAL      ! Sum of molecular production and loss coeffecients
         
C-----------------------------------------------------------------------
      OCCURS = 0
      TOTAL  = 0.0D0

      SPECIS = CHEMISTRY_SPC( NAMINDX )
c..Subtract the number of species molecules lost in this reaction
      DO N = 1, NREACT( NRX )
         INDX = IRR( NRX, N )
         IF ( INDX .EQ. NAMINDX ) THEN
             TOTAL  = TOTAL - 1.0D0
             OCCURS = OCCURS + 1
         END IF    
      END DO
      
c..Add the number of species molecules produced in this reaction
      DO N = 1, NPRDCT( NRX )
         IRRPNTR = N + 3
         INDX = IRR( NRX, IRRPNTR )
         IF ( INDX .EQ. NAMINDX ) THEN
             TOTAL  = TOTAL + REAL( SC( NRX,N ), 8)
             OCCURS = OCCURS + 1
         END IF    
      END DO

      EFFECT_REACTION = TOTAL

      RETURN

      END FUNCTION EFFECT_REACTION
      REAL(8) FUNCTION ASSESS_REACTION( NAMINDX, NRX, OCCUR_R, OCCUR_P )

C-----------------------------------------------------------------------
C Function: To find net effect on the number of species molecules from a reaction 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Bill Hutzell, May, 2018
C-----------------------------------------------------------------------
      USE RXNS_DATA

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      INTEGER,        INTENT(IN )   :: NAMINDX  ! Index for chemistry species 
      INTEGER,        INTENT(IN )   :: NRX      ! Reaction number
      INTEGER,        INTENT(INOUT) :: OCCUR_R  ! Number of reactant occurances
      INTEGER,        INTENT(INOUT) :: OCCUR_P  ! Number of product occurances
                                        
C Parameters: None

C External Functions: None 

C Local Variables:

      CHARACTER( 16 ) :: SPECIS    ! Species name to check

      INTEGER INDX       ! Pointer to reactant or product in CHEMISTRY_SPC array
      INTEGER IRRPNTR    ! Pointer to reactant or product in IRR array
      INTEGER N          ! Loop index over IRR array

      REAL(8) TOTAL      ! Sum of molecular production and loss coeffecients
         
C-----------------------------------------------------------------------
      OCCUR_P = 0
      OCCUR_R = 0
      TOTAL  = 0.0D0

      SPECIS = CHEMISTRY_SPC( NAMINDX )
c..Subtract the number of species molecules lost in this reaction
      DO N = 1, NREACT( NRX )
         INDX = IRR( NRX, N )
         IF ( INDX .EQ. NAMINDX ) THEN
             TOTAL  = TOTAL - 1.0D0
             OCCUR_R = OCCUR_R + 1
         END IF    
      END DO
      
c..Add the number of species molecules produced in this reaction
      DO N = 1, NPRDCT( NRX )
         IRRPNTR = N + 3
         INDX = IRR( NRX, IRRPNTR )
         IF ( INDX .EQ. NAMINDX ) THEN
             TOTAL  = TOTAL + REAL( SC( NRX,N ), 8)
             OCCUR_P = OCCUR_P + 1
         END IF    
      END DO

      ASSESS_REACTION = TOTAL

      RETURN

      END FUNCTION ASSESS_REACTION
        !==============================================================================================
      SUBROUTINE WRITE_OUTPUT(T, Y, NAMES, N_ROWS, N_COLS, FNAME)
        !==============================================================================================

            ! This subroutine saves the output data from the integration solver such that the first
            ! column contains the time data, and the other columns correspond to the values of
            ! the unknowns at the corresponding times.
            !
            ! input:
            !   t, double precision, dimension(:)
            !       the times the solution was evaluated [n_rows]
            !   y, double precision, dimension(:,:)
            !       the approximate solution at the respetive times [n_rows, n_cols]
            !   n_rows, integer
            !       the number of rows in the approximate solution to write
            !   n_cols, integer
            !       the number of columns in the approximate solution to write
            !   fname, character(len=*)
            !       the file name

            USE UTILIO_DEFN

            IMPLICIT NONE

            DOUBLE PRECISION, INTENT(IN) :: T(:), Y( :,: )

            CHARACTER(16), INTENT(IN)    :: NAMES( : )
            
            INTEGER, INTENT(IN) :: N_ROWS, N_COLS

            CHARACTER*(*), INTENT(IN) :: FNAME

            INTEGER :: I, J ! loop counters
            INTEGER :: IOUT ! unit number of output file
            INTEGER :: IOS
            
            IOUT = 101
                      
            OPEN(UNIT=IOUT, FILE=FNAME, STATUS='UNKNOWN', IOSTAT=IOS, ERR=1000)

            IF ( IOS .NE. 0 ) THEN
               WRITE(ERROR_LOG,95102)
               CALL M3EXIT ( 'WRITE_OUTPUT', 0, 0, ' ', XSTAT2 )
            END IF
                       
            ! write the data
            DO I=1, N_ROWS
                IF( I .EQ. 1 )THEN
                  WRITE(IOUT,95100)(NAMES(J),J=1,N_COLS)
                END IF
                WRITE(IOUT,95101)I,T(I),(Y(I,J),J=1,N_COLS)
            END DO
            
            CLOSE(IOUT)
            RETURN

1000        WRITE(ERROR_LOG,95102)
            CALL M3EXIT ( 'WRITE_OUTPUT', 0, 0, ' ', XSTAT2 )
            
95100       FORMAT(16X,"T(I)",1001(A16,1X)) 
95101       FORMAT(I3,1X,1001(ES16.6,1X))
95102       FORMAT('WRITE_OUTPUT: Error opening ASCII initial concentration file')

        END SUBROUTINE WRITE_OUTPUT
      END MODULE 
      
    
#endif    
