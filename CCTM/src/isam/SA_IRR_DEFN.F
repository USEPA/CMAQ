
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      MODULE SA_IRR_DEFN

C***********************************************************************
C20140428                       
C  
C  (1) Stores initial reaction rates in a C-R-L-nrxns array
C  (2) Contains subroutines SA_IRR_INIT
C                       and ACCUMRR
C
C    Aug 16, 2011: chemical integration time interval is in MINUTES
C
C***********************************************************************


      IMPLICIT NONE


      REAL, ALLOCATABLE :: RXINIT( :,:,:,: )
      REAL, ALLOCATABLE :: RKI_INIT( :,:,:,: )
      REAL, ALLOCATABLE :: YC_INIT( :,:,:,: )
      REAL, ALLOCATABLE :: PRDRATE( : )
      REAL, ALLOCATABLE :: RKMID ( : )

!20140307 Integrated Rates


      INTEGER              :: ISAM_CHEMISTRY_SPC ! number of ISAM species om photochemistry
      INTEGER, ALLOCATABLE :: ISAM_SPC_MAP( : ) ! index in ISAM species array
      INTEGER, ALLOCATABLE :: ISAM_TO_CHEM( : ) ! index is CHEMSITRY_SPC array
      LOGICAL, ALLOCATABLE :: CONVERT_ISAM( : ) ! whether to ISAM concentration for photochemsitry 
      
      REAL                   :: CONMIN_TAG               ! min tag concentration uploaded
      REAL( 8 )              :: NUMB_ISAM_CELLS = 0.0D0
      REAL( 8 )              :: DCONMIN_TAG              ! internal min tag concentration
!      REAL( 8 ), ALLOCATABLE :: SRK( : )
      REAL( 8 ), ALLOCATABLE :: UGM3_TO_PPM( : )      ! CGRID to CHEM Species conversion factor
      REAL,      ALLOCATABLE :: PPM_TO_UGM3( : )      ! CHEM to CGRID Species conversion factor
      REAL( 8 ), ALLOCATABLE :: SOLD( :,: )           ! local source concentrations
!      REAL( 8 ), ALLOCATABLE :: ISAM_FRACTION ( :,: ) ! isam tag concentration over cgrid value
!      REAL( 8 ), ALLOCATABLE :: TOT_ISAM_RATIO( : )   ! sum of tag concentration over cgrid value
      
!      REAL( 8 ), ALLOCATABLE  :: MAX_ERROR ( : )
!      REAL( 8 ), ALLOCATABLE  :: RMSE_CONC ( : )
!      REAL( 8 ), ALLOCATABLE  :: BIAS_CONC ( : )
!      REAL( 8 ), ALLOCATABLE  :: MEAN_CONC ( : )
      
      LOGICAL, ALLOCATABLE :: ISAM_SPECIES( : )
      LOGICAL              :: ISAM_NOT_FOUND = .FALSE.
!      LOGICAL              :: ISAM_SUBSET    = .FALSE.


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
      
      TYPE(SPECIES_BUDGET), ALLOCATABLE :: MECHANISM_BUDGET ( : )
      TYPE(SPECIES_BUDGET), ALLOCATABLE :: OX_RADICAL_BUDGET( : )
      TYPE(SPECIES_BUDGET), ALLOCATABLE :: ISAM_SPC_BUDGET  ( : )
      
      INTEGER :: ISAM_LOG        ! Unit number of output log
#ifdef verbose_isam
      LOGICAL :: CHECK_ISAM  = .TRUE.
      LOGICAL :: WRITE_BUDGET_REPORT = .TRUE.
#else
      LOGICAL :: CHECK_ISAM  = .FALSE.
      LOGICAL :: WRITE_BUDGET_REPORT = .FALSE.
#endif      
      LOGICAL :: WRITE_CELL  = .FALSE.
      LOGICAL :: UPDATE_SOLD = .FALSE.
      LOGICAL :: UPDATE_PROBABILITIES = .FALSE.

      INTEGER, PARAMETER :: N_OX_RADICALS = 3
      CHARACTER( 16 )    :: OX_RADICALS( N_OX_RADICALS ) = 
     &                      (/ 'O               ',      
     &                         'O3P             ',      
     &                         'HO2             ' /)
     
       LOGICAL, ALLOCATABLE :: IS_ISAM_OX_RADICAL( : )
       LOGICAL, ALLOCATABLE :: IS_CHEM_OX_RADICAL( : )

       INTEGER              :: OX_RADICAL_FOUND = 0   
       INTEGER              :: OZONE_INDEX      = 0     
       INTEGER, ALLOCATABLE :: OX_INDEX ( : )
       
       LOGICAL :: ISAM_WITH_OZONE = .FALSE.
       
      CONTAINS
        SUBROUTINE SA_IRR_INIT

          USE HGRD_DEFN
          USE VGRD_DEFN
          USE UTILIO_DEFN
          USE RXNS_DATA
          USE SA_DEFN
C Initialize arrays and maps that store reaction rates in each grid cell and that
C         relate ISAM species to chemistry species
C
C         Called by chemistry driver

        IMPLICIT NONE

C..Includes:
         INCLUDE SUBST_CONST     ! CMAQ constants
 
         CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_IRR_INIT'     ! Program name

         INTEGER :: I, J, RXN, IP, IL 
         INTEGER :: IOSTAT
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         INTEGER :: IOS


         CHARACTER( 132 ) :: MSG           ! Message text
! temporary arrays to set maps between isam to chemistry species
         INTEGER, ALLOCATABLE :: ISAM_SPC_IDX( : )
         INTEGER, ALLOCATABLE :: ISAM_2_CHEMI( : )
         LOGICAL, ALLOCATABLE :: NO_CHEMISTRY( : )

         CHARACTER(16), ALLOCATABLE :: FIND_IN_ISAM( : )

! temporary variables to define MECHANISM_BUDGET

         INTEGER, ALLOCATABLE :: IREACTION( : )
         INTEGER, ALLOCATABLE :: IRXN_PROD( : )
         INTEGER, ALLOCATABLE :: IRXN_LOSS( : )
         REAL(8), ALLOCATABLE :: COEFF_NET( : )
         REAL(8), ALLOCATABLE :: COEFF_POS( : )
         REAL(8), ALLOCATABLE :: COEFF_NEG( : )
         REAL(8)              :: COEFF
         
C=======================================================

        ISAM_LOG = INIT3( )
        


        ALLOCATE( ISAM_2_CHEMI( NSPC_SA + 1 ) )
        ALLOCATE( ISAM_SPC_IDX( NSPC_SA + 1 ) )
        ALLOCATE( NO_CHEMISTRY( NSPC_SA + 1 ) )
        ALLOCATE( FIND_IN_ISAM( NSPC_SA + 1 ) )
        ! krt Identify species index in ISAM array
        ISAM_SPC_IDX = 0
        ISAM_2_CHEMI = 0
        NO_CHEMISTRY = .TRUE.
        FIND_IN_ISAM = ' '

        SPC = 0
        
        DO S = 1, NSPC_SA
           FIND_IN_ISAM( S ) = SPC_NAME( S,OTHRTAG )
           ISAM_SPC_IDX( S ) = S
        END DO
        SPC = NSPC_SA
! find tagged species in chemistry_spc array to set value of convert_isam
        ISAM_CHEMISTRY_SPC = 0
        DO S = 1, SPC
           R  = INDEX1( TRIM(FIND_IN_ISAM( S )), NUMB_MECH_SPC, CHEMISTRY_SPC )
           IF ( R .LE. 0 ) THEN
              MSG = 'ISAM SPECIES: ' 
     &           // TRIM( FIND_IN_ISAM( S ) ) 
     &           // ' not found in CHEMISTRY_SPC array  '
              CALL M3WARN( PNAME, 0, 0, MSG )
              CYCLE
           END IF
           ISAM_CHEMISTRY_SPC = ISAM_CHEMISTRY_SPC
     &                        + 1           
           ISAM_2_CHEMI( S )  = R
           NO_CHEMISTRY( S )  = .FALSE.
        END DO

        IF( ANY(  .NOT. NO_CHEMISTRY ) )THEN
C..Save pointer for isam species found in chemistry species
           ALLOCATE( ISAM_TO_CHEM( ISAM_CHEMISTRY_SPC ) )
           ALLOCATE( ISAM_SPC_MAP( ISAM_CHEMISTRY_SPC ) )
           WRITE(ISAM_LOG,'(/A)')'Below isam species participate in photochemistry'
           WRITE(ISAM_LOG,'("SPC     ISAM_SPC     SPC PHOTOCHEM_SPC  ")')
           L = 0 
           DO S = 1, SPC
              IF ( .NOT. NO_CHEMISTRY( S ) ) THEN
                 L = L + 1
                 C = ISAM_SPC_IDX( S )
                 R = ISAM_2_CHEMI( S )
                 ISAM_SPC_MAP( L ) = ISAM_SPC_IDX( S )
                 ISAM_TO_CHEM( L ) = ISAM_2_CHEMI( S )
                 WRITE(ISAM_LOG,'(I3,1X,A16,1x,I3,1X,A16)') 
     &           C, FIND_IN_ISAM( S ), R, CHEMISTRY_SPC( R )
              END IF
           END DO
           IF( L .NE. ISAM_CHEMISTRY_SPC )THEN
             MSG = 'ERROR mapping isam to chemistry species: inconsistent number found'
             CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
           END IF
        ELSE
           MSG = 'NO ISAM species participate in photochemistry '
           CALL M3WARN ( PNAME, 0, 0, MSG )
        END IF
        L = 0
        DO S = 1, SPC
           IF( NO_CHEMISTRY( S ) )THEN
                C = ISAM_SPC_IDX( S )
                IF( C .LE. 0 )CYCLE
                L = L + 1
                IF( L .LT. 2 )THEN
                    WRITE(ISAM_LOG,'(/A)')'Below isam species DO NOT participate in photochemistry'
                    WRITE(ISAM_LOG,'("SPC     ISAM_SPC")')
                END IF    
                WRITE(ISAM_LOG,'(I3,1X,A16,1x,I3,A16)') C, FIND_IN_ISAM( S )
           END IF
        END DO

        DEALLOCATE( ISAM_2_CHEMI )
        DEALLOCATE( ISAM_SPC_IDX )
        DEALLOCATE( NO_CHEMISTRY )

C...Allocate and set conversion factors for isam chemistry species
        ALLOCATE( CONVERT_ISAM( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating CONVERT_ISAM'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
              
        ALLOCATE( UGM3_TO_PPM( ISAM_CHEMISTRY_SPC ),
     &            PPM_TO_UGM3( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating UGM3_TO_PPM or PPM_TO_UGM3'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        ALLOCATE( IS_ISAM_OX_RADICAL( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating IS_ISAM_A_RADICAL'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        IS_ISAM_OX_RADICAL = .FALSE.

        ALLOCATE( IS_CHEM_OX_RADICAL( NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating IS_ISAM_A_RADICAL'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        IS_CHEM_OX_RADICAL = .FALSE.

        WRITE(ISAM_LOG,'(/A)')'Final Table of ISAM chemistry species'
        WRITE(ISAM_LOG,'("SPC     ISAM_SPC     SPC PHOTOCHEM_SPC  Mol.Wei Convert Conc. Radical Spc")')
              
        ALLOCATE( ISAM_SPECIES( NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating ISAM_SPECIES'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF
        ISAM_SPECIES = .FALSE. 

        DO S = 1, ISAM_CHEMISTRY_SPC
           C = ISAM_SPC_MAP( S )
           R = ISAM_TO_CHEM( S )
           IF( FIND_IN_ISAM( C ) .EQ. 'O3' )THEN
               ISAM_WITH_OZONE = .TRUE.
               OZONE_INDEX     =  R
               WRITE(ISAM_LOG,'(A)')'ISAM Species include ozone'
           END IF    
           DO L = 1, N_OX_RADICALS
              IF( FIND_IN_ISAM( C ) .EQ. OX_RADICALS( L ) )THEN
                  IS_ISAM_OX_RADICAL( S ) = .TRUE.
              END IF    
              IF( CHEMISTRY_SPC( R ) .EQ. OX_RADICALS( L ) )THEN
                  IS_CHEM_OX_RADICAL( R ) = .TRUE.
                  OX_RADICAL_FOUND        = OX_RADICAL_FOUND + 1
              END IF    
           END DO
           CONVERT_ISAM( S ) = CONVERT_CONC( R )
           ISAM_SPECIES( R ) = .TRUE.
           UGM3_TO_PPM ( S ) = REAL( 1.0E-3 * MWAIR / SPECIES_MOLWT( R ), 8 )
           PPM_TO_UGM3 ( S ) = 1.0E+3 / MWAIR * SPECIES_MOLWT( R )
           WRITE(ISAM_LOG,'(I3,1X,A16,1x,I3,1X,A16,2X,F7.2,3(1X,L14))') 
     &     C, FIND_IN_ISAM( C ), R, CHEMISTRY_SPC( R ), SPECIES_MOLWT( R ), 
     &     CONVERT_ISAM( S ), IS_ISAM_OX_RADICAL( S ),IS_CHEM_OX_RADICAL( R )
        END DO
        
        DEALLOCATE( FIND_IN_ISAM )

        ALLOCATE( OX_INDEX( OX_RADICAL_FOUND ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating SOLD'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        OX_INDEX = 0

        DCONMIN_TAG    = 1.0D-40 / REAL( NTAG_SA+1, 8 )
        CONMIN_TAG     = 1.0E-30 / REAL( NTAG_SA )

        ALLOCATE( SOLD( NTAG_SA, NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating SOLD'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        
        ALLOCATE( MECHANISM_BUDGET( NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating MECHANISM_BUDGET'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        ALLOCATE( IREACTION( NRXNS ),
     &            IRXN_PROD( NRXNS ),
     &            IRXN_LOSS( NRXNS ),
     &            COEFF_POS( NRXNS ),
     &            COEFF_NEG( NRXNS ),
     &            COEFF_NET( NRXNS ),  STAT = IOS )     
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating IREACTION and COEFF_NET arrays'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        ISAM_WITH_OZONE = ( ISAM_WITH_OZONE .AND. OX_RADICAL_FOUND .GT. 0 )
        IF( OX_RADICAL_FOUND .GT. 0  )THEN
           ALLOCATE( OX_RADICAL_BUDGET( OX_RADICAL_FOUND ), STAT = IOS )
           IF ( IOS .NE. 0 ) THEN
              MSG = 'Error allocating MECHANISM_BUDGET'
              CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
           END IF 
        END IF

! find how reactions affect all chemistry species
        L = 0
        DO SPC = 1, NUMB_MECH_SPC
           S  = 0
           IP = 0
           IL = 0
           MECHANISM_BUDGET( SPC )%SPECIES_NAME = CHEMISTRY_SPC( SPC )
           MECHANISM_BUDGET( SPC )%NREACTIONS   = 0
           MECHANISM_BUDGET( SPC )%NRXNS_PROD   = 0
           MECHANISM_BUDGET( SPC )%NRXNS_LOSS   = 0
           IREACTION = 0
           COEFF_NET = 0.0D0
           COEFF_POS = 0.0D0
           COEFF_NEG = 0.0D0           
! set indices for Ox radicals
           IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
               L = L + 1
               OX_INDEX( L ) = SPC
               OX_RADICAL_BUDGET( L )%SPECIES_NAME = CHEMISTRY_SPC( SPC )
               OX_RADICAL_BUDGET( L )%NREACTIONS   = 0
               OX_RADICAL_BUDGET( L )%NRXNS_PROD   = 0
               OX_RADICAL_BUDGET( L )%NRXNS_LOSS   = 0
           END IF    
! find effect on CHEMISTRY_SPC( SPC ) and save results
           DO R = 1, NRXNS
              COEFF = EFFECT_REACTION( SPC, R, C )
              IF( ABS( COEFF ) .GT. 1.0D-8 )THEN
                 S = S + 1
                 IREACTION( S ) = R
                 COEFF_NET( S ) = COEFF
                 IF( COEFF  .GT. 0.0D0 )THEN
                   IP = IP + 1
                   IRXN_PROD( IP ) = R
                   COEFF_POS( IP ) = COEFF
                 ELSE
                   IL = IL + 1
                   IRXN_LOSS( IL ) = R
                   COEFF_NEG( IL ) = COEFF
                 END IF
              END IF
           END DO
! set up species budget                
           IF( S .GT. 0 )THEN
               MECHANISM_BUDGET( SPC )%NREACTIONS = S
               ALLOCATE( MECHANISM_BUDGET( SPC )%IREACTION( S ),
     &                   MECHANISM_BUDGET( SPC )%COEFF_NET( S ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating bulk MECHANISM_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF 
               MECHANISM_BUDGET( SPC )%IREACTION( 1:S ) = IREACTION( 1:S )
               MECHANISM_BUDGET( SPC )%COEFF_NET( 1:S ) = COEFF_NET( 1:S )
               IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
                   OX_RADICAL_BUDGET( L )%NREACTIONS = S
                   ALLOCATE( OX_RADICAL_BUDGET( L )%IREACTION( S ),
     &                       OX_RADICAL_BUDGET( L )%COEFF_NET( S ),  STAT = IOS )
                   IF ( IOS .NE. 0 ) THEN
                       MSG = 'Error allocating production OX_RADICAL_BUDGET arrays'
                       CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                   END IF 
                   OX_RADICAL_BUDGET( L )%IREACTION( 1:S )  = IRXN_PROD( 1:S )
                   OX_RADICAL_BUDGET( L )%COEFF_NET( 1:S )  = COEFF_NET( 1:S )
               END IF
               IF( IP .GT. 0 )THEN
! define production information
                  MECHANISM_BUDGET( SPC )%NRXNS_PROD = IP
                  ALLOCATE( MECHANISM_BUDGET( SPC )%IRXN_PROD( IP ),
     &                      MECHANISM_BUDGET( SPC )%COEFF_POS( IP ),  STAT = IOS )
                  IF ( IOS .NE. 0 ) THEN
                       MSG = 'Error allocating production MECHANISM_BUDGET arrays'
                      CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                  END IF 
                  MECHANISM_BUDGET( SPC )%IRXN_PROD( 1:IP )  = IRXN_PROD( 1:IP )
                  MECHANISM_BUDGET( SPC )%COEFF_POS( 1:IP )  = COEFF_POS( 1:IP )
! capture production information if OX radical
                  IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
                     OX_RADICAL_BUDGET( L )%NRXNS_PROD = IP
                     ALLOCATE( OX_RADICAL_BUDGET( L )%IRXN_PROD( IP ),
     &                         OX_RADICAL_BUDGET( L )%COEFF_POS( IP ),  STAT = IOS )
                     IF ( IOS .NE. 0 ) THEN
                         MSG = 'Error allocating production OX_RADICAL_BUDGET arrays'
                         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                     END IF 
                     OX_RADICAL_BUDGET( L )%IRXN_PROD( 1:IP )  = IRXN_PROD( 1:IP )
                     OX_RADICAL_BUDGET( L )%COEFF_POS( 1:IP )  = COEFF_POS( 1:IP )
                  END IF
               END IF
               
               IF( IL .GT. 0 )THEN
! define destruction information
                  MECHANISM_BUDGET( SPC )%NRXNS_LOSS = IL
                  ALLOCATE( MECHANISM_BUDGET( SPC )%IRXN_LOSS( IL ),
     &                      MECHANISM_BUDGET( SPC )%COEFF_NEG( IL ),  STAT = IOS )
                  IF ( IOS .NE. 0 ) THEN
                       MSG = 'Error allocating destruction MECHANISM_BUDGET arrays'
                      CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                  END IF 
                  MECHANISM_BUDGET( SPC )%IRXN_LOSS( 1:IL ) = IRXN_LOSS( 1:IL )
                  MECHANISM_BUDGET( SPC )%COEFF_NEG( 1:IL ) = COEFF_NEG( 1:IL )
! capture destruction information if OX radical
                  IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
                     OX_RADICAL_BUDGET( L )%NRXNS_LOSS = IL
                     ALLOCATE( OX_RADICAL_BUDGET( L )%IRXN_LOSS( IL ),
     &                         OX_RADICAL_BUDGET( L )%COEFF_NEG( IL ),  STAT = IOS )
                     IF ( IOS .NE. 0 ) THEN
                         MSG = 'Error allocating production OX_RADICAL_BUDGET arrays'
                         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                     END IF 
                     OX_RADICAL_BUDGET( L )%IRXN_LOSS( 1:IL )  = IRXN_LOSS( 1:IL )
                     OX_RADICAL_BUDGET( L )%COEFF_NEG( 1:IL )  = COEFF_NET( 1:IL )
                  END IF
               END IF
           END IF                  
        END DO

        DEALLOCATE( IREACTION,
     &              IRXN_PROD,
     &              IRXN_LOSS,
     &              COEFF_POS,
     &              COEFF_NEG,
     &              COEFF_NET )     


        ALLOCATE( ISAM_SPC_BUDGET( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating ISAM_SPC_BUDGET'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        DO SPC = 1, ISAM_CHEMISTRY_SPC
           R = ISAM_TO_CHEM( SPC )
           ISAM_SPC_BUDGET( SPC )%SPECIES_NAME = MECHANISM_BUDGET( R )%SPECIES_NAME
           ISAM_SPC_BUDGET( SPC )%NREACTIONS   = MECHANISM_BUDGET( R )%NREACTIONS  
           ISAM_SPC_BUDGET( SPC )%NRXNS_PROD   = MECHANISM_BUDGET( R )%NRXNS_PROD  
           ISAM_SPC_BUDGET( SPC )%NRXNS_LOSS   = MECHANISM_BUDGET( R )%NRXNS_LOSS  
! define net information
           S =  ISAM_SPC_BUDGET( SPC )%NREACTIONS
           IF( S .GT. 0 )THEN
               ALLOCATE( ISAM_SPC_BUDGET( SPC )%IREACTION( S ),
     &                   ISAM_SPC_BUDGET( SPC )%COEFF_NET( S ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating bulk ISAM_SPC_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF 
               ISAM_SPC_BUDGET( SPC )%IREACTION( 1:S ) = MECHANISM_BUDGET( R )%IREACTION( 1:S )
               ISAM_SPC_BUDGET( SPC )%COEFF_NET( 1:S ) = MECHANISM_BUDGET( R )%COEFF_NET( 1:S )
           END IF          
           IP = ISAM_SPC_BUDGET( SPC )%NRXNS_PROD
           IF( IP .GT. 0 )THEN
! define production information
              ALLOCATE( ISAM_SPC_BUDGET( SPC )%IRXN_PROD( IP ),
     &                  ISAM_SPC_BUDGET( SPC )%COEFF_POS( IP ),  STAT = IOS )
              IF ( IOS .NE. 0 ) THEN
                  MSG = 'Error allocating production ISAM_SPC_BUDGET arrays'
                  CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
              END IF 
              ISAM_SPC_BUDGET( SPC )%IRXN_PROD( 1:IP )  = MECHANISM_BUDGET( R )%IRXN_PROD( 1:IP )
              ISAM_SPC_BUDGET( SPC )%COEFF_POS( 1:IP )  = MECHANISM_BUDGET( R )%COEFF_POS( 1:IP )
           END IF       
           IL = ISAM_SPC_BUDGET( SPC )%NRXNS_LOSS
           IF( IL .GT. 0 )THEN
! define destruction information
              ALLOCATE( ISAM_SPC_BUDGET( SPC )%IRXN_LOSS( IL ),
     &                  ISAM_SPC_BUDGET( SPC )%COEFF_NEG( IL ),  STAT = IOS )
              IF ( IOS .NE. 0 ) THEN
                  MSG = 'Error allocating production ISAM_SPC_BUDGET arrays'
                  CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
              END IF 
              ISAM_SPC_BUDGET( SPC )%IRXN_LOSS( 1:IL )  = MECHANISM_BUDGET( R )%IRXN_LOSS( 1:IL )
              ISAM_SPC_BUDGET( SPC )%COEFF_NEG( 1:IL )  = MECHANISM_BUDGET( R )%COEFF_NEG( 1:IL )
           END IF  
           WRITE(ISAM_LOG,*)SPC, ISAM_SPC_BUDGET( SPC )%SPECIES_NAME,IP,IL    
        END DO          
        
! report budget for mechanism species and OX radicals
        IF( WRITE_BUDGET_REPORT ) THEN
            CALL REPORT_MECH_BUDGET( ISAM_LOG )
            IF( OX_RADICAL_FOUND .GT. 0  )THEN
               CALL REPORT_OX_RADICALS( ISAM_LOG )
            ELSE
               MSG = "Note that no Oxygen Radicals were found in mechanism."   
               WRITE(ISAM_LOG,'(A)')TRIM( MSG )
            END IF
            IF( ISAM_CHEMISTRY_SPC .GT. 0 )THEN
               CALL REPORT_ISAM_BUDGET( ISAM_LOG )
            ELSE
               MSG = "Note that ISAM found in mechanism."   
               WRITE(ISAM_LOG,'(A)')TRIM( MSG )
            END IF
        END IF    
        
        END SUBROUTINE SA_IRR_INIT
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
      SUBROUTINE REPORT_MECH_BUDGET( OUT_UNIT )
!        purpose writes out 
         USE RXNS_DATA

         IMPLICIT NONE


!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC
        INTEGER IR, NR
         
        DO SPC = 1, NUMB_MECH_SPC
           WRITE(OUT_UNIT,95000)MECHANISM_BUDGET( SPC )%SPECIES_NAME, 
     &                          MECHANISM_BUDGET( SPC )%NREACTIONS
           DO NR = 1, MECHANISM_BUDGET( SPC )%NREACTIONS
              IR = MECHANISM_BUDGET( SPC )%IREACTION( NR )
              WRITE(OUT_UNIT,95001)RXLABEL( IR ),MECHANISM_BUDGET( SPC )%COEFF_NET( NR )
           END DO
              WRITE(OUT_UNIT,95005)MECHANISM_BUDGET( SPC )%SPECIES_NAME, 
     &                             MECHANISM_BUDGET( SPC )%NRXNS_PROD
              DO NR = 1, MECHANISM_BUDGET( SPC )%NRXNS_PROD
                 IR = MECHANISM_BUDGET( SPC )%IRXN_PROD( NR )
                 WRITE(OUT_UNIT,95001)RXLABEL( IR ),MECHANISM_BUDGET( SPC )%COEFF_POS( NR )
              END DO
        END DO
        
95000   FORMAT("Chemistry species, ",A16,", changed by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95001   FORMAT(3X,A16,1X,ES12.4)
95005   FORMAT("Radical  species, ",A16,", produced by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )

      END SUBROUTINE REPORT_MECH_BUDGET    
      SUBROUTINE REPORT_OX_RADICALS( OUT_UNIT )
!        purpose writes out production and loss reaction for each OX radical
         USE RXNS_DATA

         IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC
        INTEGER IR, NR
         
        DO SPC = 1, OX_RADICAL_FOUND
           IF( OX_RADICAL_BUDGET( SPC )%NRXNS_PROD .GT. 0 )THEN
              WRITE(OUT_UNIT,95005)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME, 
     &                             OX_RADICAL_BUDGET( SPC )%NRXNS_PROD
              DO NR = 1, OX_RADICAL_BUDGET( SPC )%NRXNS_PROD
                 IR = OX_RADICAL_BUDGET( SPC )%IRXN_PROD( NR )
                 WRITE(OUT_UNIT,95001)RXLABEL( IR ),OX_RADICAL_BUDGET( SPC )%COEFF_POS( NR )
              END DO
           ELSE   
              WRITE(OUT_UNIT,95003)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME 
           END IF   
           IF( OX_RADICAL_BUDGET( SPC )%NRXNS_LOSS .GT. 0 )THEN
              WRITE(OUT_UNIT,95002)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME, 
     &                             OX_RADICAL_BUDGET( SPC )%NRXNS_LOSS
              DO NR = 1, OX_RADICAL_BUDGET( SPC )%NRXNS_LOSS
                 IR = OX_RADICAL_BUDGET( SPC )%IRXN_LOSS( NR )
                 WRITE(OUT_UNIT,95001)RXLABEL( IR ),OX_RADICAL_BUDGET( SPC )%COEFF_NEG( NR )
              END DO
           ELSE
              WRITE(OUT_UNIT,95004)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME 
           END IF   
        END DO
        
95005   FORMAT("Radical  species, ",A16,", produced by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95001   FORMAT(3X,A16,1X,ES12.4)
95002   FORMAT("Radical  species, ",A16,", destoryed by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95003   FORMAT(A16, " radical not produced by any reactions.")
95004   FORMAT(A16, " radical not destoryed by any reactions.")

      END SUBROUTINE REPORT_OX_RADICALS
      SUBROUTINE REPORT_ISAM_BUDGET( OUT_UNIT )
!        purpose writes out 
         USE RXNS_DATA

         IMPLICIT NONE


!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC
        INTEGER IR, NR
         
        DO SPC = 1, ISAM_CHEMISTRY_SPC
           WRITE(OUT_UNIT,95100)ISAM_SPC_BUDGET( SPC )%SPECIES_NAME, 
     &                          ISAM_SPC_BUDGET( SPC )%NREACTIONS
           DO NR = 1, ISAM_SPC_BUDGET( SPC )%NREACTIONS
              IR = ISAM_SPC_BUDGET( SPC )%IREACTION( NR )
              WRITE(OUT_UNIT,95101)RXLABEL( IR ),ISAM_SPC_BUDGET( SPC )%COEFF_NET( NR )
           END DO
              WRITE(OUT_UNIT,95105)ISAM_SPC_BUDGET( SPC )%SPECIES_NAME, 
     &                             ISAM_SPC_BUDGET( SPC )%NRXNS_PROD
              DO NR = 1, ISAM_SPC_BUDGET( SPC )%NRXNS_PROD
                 IR = ISAM_SPC_BUDGET( SPC )%IRXN_PROD( NR )
                 WRITE(OUT_UNIT,95101)RXLABEL( IR ),ISAM_SPC_BUDGET( SPC )%COEFF_POS( NR )
              END DO
        END DO
        
95100   FORMAT("   ISAM  species, ",A16,", changed by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95101   FORMAT(3X,A16,1X,ES12.4)
95105   FORMAT("   ISAM  species, ",A16,", produced by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )

      END SUBROUTINE REPORT_ISAM_BUDGET    
      SUBROUTINE SA_IRR_EXTRACT( COL, ROW, LAY, DENS, CONC )
                
          USE HGRD_DEFN
          USE VGRD_DEFN
          USE RXNS_DATA
          USE UTILIO_DEFN
          USE SA_DEFN  

         IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  COL        ! cell column index
         INTEGER,   INTENT( IN ) ::  ROW        ! cell row index 
         INTEGER,   INTENT( IN ) ::  LAY        ! cell layer index      
         REAL,      INTENT( IN ) ::  DENS       ! air mass density, kg/m3
         REAL(8),   INTENT( IN ) ::  CONC( : )  ! cgrid concentrations

C..Includes:
         INCLUDE SUBST_CONST     ! CMAQ constants

!..Local:
         CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_IRR_EXTRACT'     ! Program name

         REAL( 8 ), PARAMETER  :: ONE       = 1.0D0
         REAL( 8 ), PARAMETER  :: ZERO      = 0.0D0
!         REAL( 8 ), PARAMETER  :: LOCAL_MIN = 1.1D-34
!         REAL,      PARAMETER  :: REAL_MIN  = 1.1E-34

         REAL      :: FACTOR2
         REAL( 8 ) :: TOTAL, FACTOR1, FACTOR3
         REAL( 8 ) :: INV_DENS       ! one over air mass density, m3/kg

         INTEGER :: JSPC, KTAG
!..variables borrowed from DDM
         INTEGER :: I, J, RXN
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         REAL(8) :: TAGS_TOTAL
         
!         INTEGER :: IOS         
!         CHARACTER( 132 )  :: MSG

!         SOLD = DCONMIN_TAG ! LOCAL_MIN
          INV_DENS = REAL( ONE/DENS, 8 )
          
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             S       = ISAM_TO_CHEM( JSPC )
             SPC     = ISAM_SPC_MAP( JSPC )
             LOAD_SOLD: DO KTAG = 1, NTAG_SA
                 IF( ISAM( COL,ROW,LAY,SPC,KTAG ) .GT. 1.0E-18 )THEN
                    FACTOR2 = ISAM( COL,ROW,LAY,SPC,KTAG )
                 ELSE
                    FACTOR2 = 0.0
                 END IF
!                 SOLD( KTAG, S ) = MAX( REAL( FACTOR2,8 ), DCONMIN_TAG )
                 SOLD( KTAG, S ) = REAL( FACTOR2,8 )
                 IF( CONVERT_ISAM( JSPC ) )THEN
                     SOLD( KTAG, S ) = SOLD( KTAG, S )
     &                               * INV_DENS * UGM3_TO_PPM( JSPC )
                 END IF
!                SOLD( KTAG,S ) = MAX( SOLD( KTAG,S ), DCONMIN_TAG )
             END DO LOAD_SOLD ! ktag loop
             IF( SUM( SOLD( 1:NTAG_SA,S ) ) .LE. 0.0D0 )THEN
                 SOLD( OTHRTAG,S ) = 1.0D-40
             END IF
          END DO ! loop jspc

#ifdef verbose_isam          
          IF( WRITE_CELL )THEN
             DO KTAG = 1, NTAG_SA
               WRITE(ISAM_LOG,*)' '
               WRITE(ISAM_LOG,'(A24,2(1X,ES12.4))')'Initial MAX Values',
     &         MAXVAL(ISAM(:,:,:,:,KTAG))
!    &         MAXVAL( ISAMB4(:,:,:,:,KTAG )), MAXVAL(ISAM(:,:,:,:,KTAG))
             END DO ! ktag loop
             DO JSPC = 1, NSPC_SA 
                  TOTAL = SUM( ISAM( COL,ROW,LAY,JSPC,1:NTAG_SA ) )
                  WRITE(ISAM_LOG,'(A16,11(1X,ES12.4))')SPC_NAME( JSPC,OTHRTAG ),
     &            (ISAM( COL,ROW,LAY,JSPC,KTAG ),KTAG = 1, NTAG_SA), TOTAL
             END DO
             WRITE(ISAM_LOG,*)'EX: Initial Totals'
             DO JSPC = 1, ISAM_CHEMISTRY_SPC
                S   = ISAM_TO_CHEM( JSPC )
                SPC = ISAM_SPC_MAP( JSPC ) 
                TOTAL = 0.0
                DO KTAG = 1, NTAG_SA
                   TOTAL = TOTAL + ISAM( COL,ROW,LAY,SPC,KTAG )
                END DO 
                WRITE(ISAM_LOG,'(2(A16,1X),5(1X,ES12.4))')SPC_NAME( SPC,OTHRTAG ), CHEMISTRY_SPC(S), 
     &          TOTAL, CONC( S ),TOTAL-CONC( S ), TOTAL/CONC( S )
             END DO ! loop jspc
          END IF
#endif
        END SUBROUTINE SA_IRR_EXTRACT
        SUBROUTINE SA_IRR_UPLOAD( COL, ROW, LAY, DENS, CONC )
                
          USE HGRD_DEFN
          USE VGRD_DEFN
          USE RXNS_DATA
          USE UTILIO_DEFN
          USE SA_DEFN    ! 20130517

         IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  COL        ! cell column index
         INTEGER,   INTENT( IN ) ::  ROW        ! cell row index 
         INTEGER,   INTENT( IN ) ::  LAY        ! cell layer index      
         REAL,      INTENT( IN ) ::  DENS       ! air mass density, kg/m3
         REAL(8),   INTENT( IN ) ::  CONC( : )  ! cgrid concentrations

C..Includes:
         INCLUDE SUBST_CONST     ! CMAQ constants

!..Local:
         CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_IRR_UPLOAD'     ! Program name

         REAL      :: TOTAL
         REAL( 8 ) :: INV_DENS       ! one over air mass density, m3/kg

         INTEGER :: JSPC, KTAG
         INTEGER :: I, J, RXN
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         
!         INTEGER :: IOS         
!         CHARACTER( 132 )  :: MSG

         REAL :: TEMP_VALUE

         DO JSPC = 1, ISAM_CHEMISTRY_SPC
            S   = ISAM_TO_CHEM( JSPC )
            SPC = ISAM_SPC_MAP( JSPC ) 
            LOAD_ISAM: DO KTAG = 1, NTAG_SA
               IF( SOLD( KTAG,S ) .GT. DCONMIN_TAG )THEN
                 TEMP_VALUE = REAL( SOLD( KTAG,S ) )
                 IF( CONVERT_ISAM( JSPC ) )THEN
                     TEMP_VALUE = TEMP_VALUE
     &                          * DENS * PPM_TO_UGM3( JSPC )
                 END IF
                 ISAM( COL,ROW,LAY,SPC,KTAG ) = MAX( CONMIN_TAG, TEMP_VALUE )
               ELSE
!                IF( ISAM( COL,ROW,LAY,SPC,KTAG ) .GT. 0.0 )THEN
                     ISAM( COL,ROW,LAY,SPC,KTAG ) = CONMIN_TAG
!                ELSE    
!                    ISAM( COL,ROW,LAY,SPC,KTAG ) = 0.0
!                END IF    
               END IF    
            END DO LOAD_ISAM ! ktag loop     
         END DO   ! jspc
 
#ifdef verbose_isam             
          IF( WRITE_CELL )THEN
             DO KTAG = 1, NTAG_SA
               WRITE(ISAM_LOG,*)' '
               WRITE(ISAM_LOG,'(A24,2(1X,ES12.4))')'Final MAX Values',
     &         MAXVAL(ISAM(:,:,:,:,KTAG))
!    &         MAXVAL( ISAMB4(:,:,:,:,KTAG )), MAXVAL(ISAM(:,:,:,:,KTAG))
             END DO ! ktag loop
             DO JSPC = 1, NSPC_SA 
                  TOTAL = SUM( ISAM( COL,ROW,LAY,JSPC,1:NTAG_SA ) )
                  WRITE(ISAM_LOG,'(A16,11(1X,ES12.4))')SPC_NAME( JSPC,OTHRTAG ),
     &            (ISAM( COL,ROW,LAY,JSPC,KTAG ),KTAG = 1, NTAG_SA), TOTAL
             END DO
             WRITE(ISAM_LOG,*)'UP: Final Totals'
             DO JSPC = 1, ISAM_CHEMISTRY_SPC
                S   = ISAM_TO_CHEM( JSPC )
                SPC = ISAM_SPC_MAP( JSPC )
                TEMP_VALUE =  MAX( REAL(CONC( S )),1.0E-30 )
                TOTAL = 0.0
                DO KTAG = 1, NTAG_SA
                   TOTAL = TOTAL + ISAM( COL,ROW,LAY,SPC,KTAG )
                END DO 
                WRITE(ISAM_LOG,'(2(A16,1X),3(1X,ES12.4))')SPC_NAME( SPC,OTHRTAG ), CHEMISTRY_SPC(S), 
     &          TOTAL,TEMP_VALUE,TOTAL-TEMP_VALUE
             END DO ! loop jspc
          END IF
#endif
        END SUBROUTINE SA_IRR_UPLOAD
C----------------------------------------------------------------------
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SA_IRR_UNBLOCKED ( LSTART, RK, CONC, DELT )

C-----------------------------------------------------------------------
C  Function: Integrate chemical rates of reaction for an IRR/MB analysis
 
C  Preconditions: None
 
C  Key Subroutines/Functions Called: None
 
C  Revision History:
C   Prototype created by Jerry Gipson, September, 1996
C   global BLKPRM Jeff Dec 96
C   Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C   Modified Jun, 1998 by Jerry Gipson to add reaction number error checks
C   Modified 1/19/99 by David Wong at LM:
C                      -- add four include files because of new PA_CMN.EXT
C   Modified 2/26/99 by David Wong at LM:
C                      -- remove SUBST_AE_SPC, SUBST_NR_SPC, SUBST_TR_SPC,
C                         three .EXT files
C   31 Mar 01 J.Young: Use HGRD_DEFN; eliminate BLKPRM.EXT
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   19 Aug 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module

C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RXNS_DATA             ! chemical mechanism data
      USE CGRID_SPCS            ! CGRID mechanism species
      USE SA_DEFN
      USE UTILIO_DEFN

      IMPLICIT NONE 

C..Includes: None
      
C..Arguments: 
      LOGICAL, INTENT( IN ) :: LSTART   ! Flag to indicate start of chemical integration period

      REAL( 8 ),    INTENT( IN ) :: RK  ( : )    ! Reaction rate coefficients
      REAL( 8 ),    INTENT( IN ) :: CONC( : )    ! species concentrations
      REAL( 8 ),    INTENT( IN ) :: DELT         ! Chemistry integration time size

C..Parameters: None

C..External Functions: None
 
      CHARACTER( 16 ) , SAVE :: PNAME = 'PA_IRR'   ! Program name
      CHARACTER( 132)        :: MSG

      LOGICAL, SAVE :: LFIRST = .TRUE.   ! Flag for first call to subroutine

C..Scratch Local Variables:
      INTEGER ISP1, ISP2, ISP3  ! Species indices
      INTEGER    S, JSPC,  SPC  ! Species indices
      INTEGER NCELL             ! Loop index for cells
      INTEGER NIRR              ! Loop index for IRR outputs
      INTEGER NOUT              ! IRR output index
      INTEGER NRX               ! Loop index for reactions
      INTEGER NTEMP             ! Loop index for temp IRRs
      INTEGER NTERM             ! Loop index for terms
      INTEGER ASTAT             ! allocation status
      INTEGER KTAG              ! Loop index/pointer for source
      INTEGER POSITIVE          ! count of tag concentration greater than zero

      REAL(8) TOTAL                   ! scratch term for summations
      REAL(8) TOTAL_PROD              ! scratch term for total bulk production
      REAL(8) ISAM_PROD               ! scratch term for total isam production
      REAL(8) TERM                    ! scratch term for summations
      REAL(8) COEFF                   ! Coefficient of IRR term
      REAL(8) TOTAL_PROBABILITY       ! normalization coefficient for SOURCE_PROBABILITY
      REAL(8) ISAM_TOTAL_PROBABILITY  ! normalization coefficient for ISAM_PROBABILITY
      
      LOGICAL DEFICIT           ! whether species loss surpassed a source tag
      LOGICAL BALANCE           ! whether change budget matched
      
      LOGICAL, ALLOCATABLE, SAVE   :: SOURCE_ZERO( : ) ! whether source concentration greater than zero 
C..Saved Local Variables:

      REAL( 8 ), ALLOCATABLE, SAVE :: YCOLD  ( : )
      REAL( 8 ), ALLOCATABLE, SAVE :: YCMID  ( : )
      REAL( 8 ), ALLOCATABLE, SAVE :: RXRAT  ( : )       
      REAL( 8 ), ALLOCATABLE, SAVE :: INTRXN ( : )     ! Integrated reaction rates

      REAL( 8 ) :: ONE_OVER_CONC      ! reciprocal of total species concentrations
      REAL( 8 ), ALLOCATABLE, SAVE :: TOTAL_ISAM_CONC( : )      ! total concentrations from isam sources
      REAL( 8 ), ALLOCATABLE, SAVE :: NOT_OUTSIDE_ISAM( : )     ! fraction of species not from non-isam sources
      REAL( 8 ), ALLOCATABLE, SAVE :: NOT_ISAM_SOURCE( :,: )    ! fraction of species not from an isam source
      REAL( 8 ), ALLOCATABLE, SAVE :: SOURCE_PROBABILITY( :,: ) ! probability or amount that a source contributes to reaction
      REAL( 8 ), ALLOCATABLE, SAVE :: SOURCE_DELTA   ( :,: )    ! change in source tag species 
      REAL( 8 ), ALLOCATABLE, SAVE :: OX_DELTA( : )
      REAL( 8 ), ALLOCATABLE, SAVE :: ISAM_PROBABILITY  ( :,: ) ! probability or amount that a source contributes to reaction      
      LOGICAL,   ALLOCATABLE, SAVE :: ZERO_ISAM( : )  ! whether isam species have no effect on reaction
      
      

C-----------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, flag the reactions for which to calculate IRRS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

C Allocate arrays:

        ALLOCATE( YCOLD( NUMB_MECH_SPC ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating YCOLD variable'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF

        IF( OX_RADICAL_FOUND .GT. 0 )THEN
             ALLOCATE( YCMID( NUMB_MECH_SPC ), STAT = ASTAT )
             IF ( ASTAT .NE. 0 ) THEN
                MSG = 'ERROR allocating YCOLD variable'
                CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
             END IF
        END IF
        
        ALLOCATE( RXRAT  ( NRXNS ),
     &            INTRXN ( NRXNS ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF

!         ALLOCATE( ONE_OVER_CONC( NUMB_MECH_SPC ), STAT = ASTAT )
!         IF ( ASTAT .NE. 0 ) THEN
!           MSG = 'ERROR allocating SA_IRR variables'
!           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
!         END IF

         ALLOCATE( TOTAL_ISAM_CONC( NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( NOT_ISAM_SOURCE( NTAG_SA, NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SOURCE_PROBABILITY( NTAG_SA + 1, NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( ISAM_PROBABILITY( NTAG_SA, NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating ISAM_PROBABILITY variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( ZERO_ISAM( NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating ZERO_ISAM '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF
             
         ALLOCATE( SOURCE_DELTA( NTAG_SA + 1, ISAM_CHEMISTRY_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SOURCE_DELTA '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( OX_DELTA( NTAG_SA ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating OX_DELTA '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SOURCE_ZERO( NTAG_SA + 1 ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SOURCE_ZERO '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF
         LFIRST = .FALSE.

         ALLOCATE( NOT_OUTSIDE_ISAM( NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF
C...Assume that ISAM_SPECIES is fixed over the domain and simulation        
         WHERE ( ISAM_SPECIES ) 
            NOT_OUTSIDE_ISAM = 1.0D0
         ELSE WHERE
            NOT_OUTSIDE_ISAM = 0.0D0
         END WHERE            
         DO ISP2 = 1, NUMB_MECH_SPC
            IF( .NOT. ISAM_SPECIES( ISP2 ) )THEN            
              DO KTAG = 1, NTAG_SA
                NOT_ISAM_SOURCE( KTAG,ISP2 ) = 1.0D0
              END DO
            END IF
         END DO            
      END IF ! LFIRST
      
      IF( LSTART )THEN
!         UPDATE_SOLD = .FALSE.
         SOURCE_DELTA = 0.0D0
         DO NIRR = 1, NUMB_MECH_SPC 
            YCOLD( NIRR ) = CONC( NIRR )
         END DO
         IF( OX_RADICAL_FOUND .GT. 0 ) YCMID = YCOLD
         RETURN
      END IF
   
      IF( UPDATE_PROBABILITIES )THEN
C...check for bad values
         TOTAL_ISAM_CONC = 1.0D-40
         DO ISP2 = 1, NUMB_MECH_SPC
            DO KTAG = 1, NTAG_SA
               TOTAL_ISAM_CONC( ISP2 ) = TOTAL_ISAM_CONC( ISP2 )
     &                                 + SOLD( KTAG,ISP2 )
               IF( SOLD( KTAG,ISP2 ) .LT. 0.0D0 )
     &         WRITE(ISAM_LOG,*)TRIM(CHEMISTRY_SPC(ISP2)) // ' bad value = ',SOLD( KTAG,ISP2 )
            END DO
         END DO
C..compute fractions not from ISAM groups
         DO ISP2 = 1, NUMB_MECH_SPC
            IF( ISAM_SPECIES( ISP2 ) )THEN            
             ONE_OVER_CONC   = 1.0D0 / TOTAL_ISAM_CONC( ISP2 )
              DO KTAG = 1, NTAG_SA
                NOT_ISAM_SOURCE( KTAG,ISP2 ) = MAX( 0.0D0, 1.0D0-SOLD( KTAG,ISP2 )*ONE_OVER_CONC)
              END DO
            END IF
         END DO            
C...Calculate rate of reaction and source probabilties
        DO NRX = 1, NRXNS
            IF ( NREACT( NRX ) .EQ. 1 ) THEN
               ISP1 = IRR( NRX,1 )
               RXRAT( NRX ) = RK( NRX )
     &                      * CONC( ISP1 )
               DO KTAG = 1, NTAG_SA
                 SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 - NOT_ISAM_SOURCE( KTAG,ISP1 ) 
               END DO
               KTAG = NTAG_SA + 1
               SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 - NOT_OUTSIDE_ISAM( ISP1 ) 
            ELSE IF ( NREACT( NRX ) .EQ. 2 ) THEN
               ISP1 = IRR( NRX,1 )
               ISP2 = IRR( NRX,2 )
               RXRAT( NRX ) = RK( NRX )
     &                      * CONC( ISP1 )
     &                      * CONC( ISP2 ) 
               DO KTAG = 1, NTAG_SA
                 SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                          -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
     &                                          *  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
               END DO
               KTAG = NTAG_SA + 1
               SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                        -  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                        *  NOT_OUTSIDE_ISAM( ISP2 ) 
            ELSE IF ( NREACT( NRX ) .EQ. 3 ) THEN
               ISP1 = IRR( NRX,1 )
               ISP2 = IRR( NRX,2 )
               ISP3 = IRR( NRX,3 )
               RXRAT( NRX ) = RK( NRX )
     &                      * CONC( ISP1 )
     &                      * CONC( ISP2 )
     &                      * CONC( ISP3 ) 
               DO KTAG = 1, NTAG_SA
                 SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                          -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
     &                                          *  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
     &                                          *  NOT_ISAM_SOURCE( KTAG,ISP3 ) 
               END DO
               KTAG = NTAG_SA + 1
               SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                        -  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                        *  NOT_OUTSIDE_ISAM( ISP2 ) 
     &                                        *  NOT_OUTSIDE_ISAM( ISP3 ) 
            ELSE IF (NREACT( NRX ) .EQ. 0 ) THEN
               RXRAT( NRX ) = RK( NRX )
               DO KTAG = 1, NTAG_SA
                  SOURCE_PROBABILITY( KTAG,NRX ) = 0.0D0 
               END DO
               KTAG = NTAG_SA + 1
               SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
            END IF
C..Normalize sources probabilities for reaction
            ISAM_TOTAL_PROBABILITY  =  0.0D0
            DO KTAG = 1, NTAG_SA
               SOURCE_PROBABILITY( KTAG,NRX ) = MAX( SOURCE_PROBABILITY( KTAG,NRX ), 0.0D0 )
               ISAM_PROBABILITY  ( KTAG,NRX ) = SOURCE_PROBABILITY( KTAG,NRX )
               ISAM_TOTAL_PROBABILITY = ISAM_TOTAL_PROBABILITY + ISAM_PROBABILITY( KTAG,NRX )
            END DO
            KTAG = NTAG_SA + 1
            SOURCE_PROBABILITY( KTAG,NRX ) = MAX( SOURCE_PROBABILITY( KTAG,NRX ), 0.0D0 )
            TOTAL_PROBABILITY = ISAM_TOTAL_PROBABILITY + SOURCE_PROBABILITY( KTAG,NRX )
            IF( ISAM_TOTAL_PROBABILITY .LE. 1.0D-30 )THEN
                ZERO_ISAM( NRX ) = .TRUE. 
                DO KTAG = 1, NTAG_SA
                   ISAM_PROBABILITY( KTAG,NRX ) = 0.0D0
                END DO
                ISAM_PROBABILITY( OTHRTAG,NRX ) = 1.0D0
            ELSE
                ZERO_ISAM( NRX ) = .FALSE.
                ISAM_TOTAL_PROBABILITY = 1.0D0 / ISAM_TOTAL_PROBABILITY
                DO KTAG = 1, NTAG_SA
                   ISAM_PROBABILITY( KTAG,NRX ) = ISAM_PROBABILITY( KTAG,NRX )
     &                                          * ISAM_TOTAL_PROBABILITY
                END DO
            END IF    
            IF( TOTAL_PROBABILITY .LE. 0.0D0 )THEN
               DO JSPC = 1, ISAM_CHEMISTRY_SPC
                  S   = ISAM_TO_CHEM( JSPC )
                  SPC = ISAM_SPC_MAP( JSPC ) 
                  WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(S),(SOLD( KTAG, S ),KTAG=1,NTAG_SA ),
     &            YCOLD( S )
               END DO ! loop jspc
               MSG = 'Fraction Results, note that last column is total isam over total species CONCentration'
               WRITE(ISAM_LOG,'(A)')TRIM(MSG)
               WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Species/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
               DO ISP2 = 1, NUMB_MECH_SPC
                  WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(ISP2),
     &           (1.0D0-NOT_ISAM_SOURCE( KTAG,ISP2 ),KTAG=1,NTAG_SA),1.0D0-NOT_OUTSIDE_ISAM( ISP2 )
               END DO

               MSG = 'Unnormalized Source Probabilities, note that last column is for NonISAM CONCentrations'
               WRITE(ISAM_LOG,'(A)')TRIM(MSG)
               WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Reaction/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
               WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')RXLABEL( NRX ),
     &         (SOURCE_PROBABILITY( KTAG,NRX ),KTAG=1,NTAG_SA+1)

               MSG = 'TOTAL_PROBABILITY <= zero from reaction label: ' 
     &             // TRIM( RXLABEL( NRX ) )
               CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
            END IF
            TOTAL_PROBABILITY = 1.0D0 / TOTAL_PROBABILITY
            DO KTAG = 1, NTAG_SA + 1
               SOURCE_PROBABILITY( KTAG,NRX ) = SOURCE_PROBABILITY( KTAG,NRX )
     &                                        * TOTAL_PROBABILITY
            END DO
        END DO
#ifdef verbose_isam
        IF( WRITE_CELL )THEN
           MSG = 'Calculated Source Probabilities, note that last column is for NonISAM concentrations'
           WRITE(ISAM_LOG,'(A)')TRIM(MSG)
           WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Reaction/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
           DO NRX = 1, NRXNS
              WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')RXLABEL( NRX ),
     &        (SOURCE_PROBABILITY( KTAG,NRX ),KTAG=1,NTAG_SA+1)
           END DO
           MSG = 'Caculated ISAM Probabilities, note that last column is their sum'
           WRITE(ISAM_LOG,'(A)')TRIM(MSG)
           WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Reaction/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
           DO NRX = 1, NRXNS
              ISAM_TOTAL_PROBABILITY = SUM( ISAM_PROBABILITY( 1:NTAG_SA,NRX ) )
              WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')RXLABEL( NRX ),
     &       (ISAM_PROBABILITY( KTAG,NRX ),KTAG=1,NTAG_SA),ISAM_TOTAL_PROBABILITY
           END DO
         END IF ! WRITE_CELL  
#endif         
      ELSE  ! Just compute reaction rates
         DO NRX = 1, NRXNS
            IF ( NREACT( NRX ) .EQ. 1 ) THEN
               ISP1 = IRR( NRX,1 )
               RXRAT( NRX ) = RK( NRX )
     &                      * CONC( ISP1 )
            ELSE IF ( NREACT( NRX ) .EQ. 2 ) THEN
               ISP1 = IRR( NRX,1 )
               ISP2 = IRR( NRX,2 )
               RXRAT( NRX ) = RK( NRX )
     &                      * CONC( ISP1 )
     &                      * CONC( ISP2 ) 
            ELSE IF ( NREACT( NRX ) .EQ. 3 ) THEN
               ISP1 = IRR( NRX,1 )
               ISP2 = IRR( NRX,2 )
               ISP3 = IRR( NRX,3 )
               RXRAT( NRX ) = RK( NRX )
     &                      * CONC( ISP1 )
     &                      * CONC( ISP2 )
     &                      * CONC( ISP3 ) 
            ELSE IF (NREACT( NRX ) .EQ. 0 ) THEN
               RXRAT( NRX ) = RK( NRX )
            END IF
         END DO
      END IF       
C..Compute integrated reaction rates
      DO NRX = 1, NRXNS
          INTRXN( NRX ) = DELT * RXRAT( NRX )
      END DO

c..Compute change in source concentrations for updating source concentrations
       DO JSPC = 1, ISAM_CHEMISTRY_SPC
          NIRR = ISAM_TO_CHEM( JSPC )
          IF( .NOT. IS_ISAM_OX_RADICAL( JSPC ) )THEN
             DO NTERM = 1, ISAM_SPC_BUDGET( JSPC )%NREACTIONS
                NRX   = ISAM_SPC_BUDGET( JSPC )%IREACTION( NTERM )
                COEFF = ISAM_SPC_BUDGET( JSPC )%COEFF_NET( NTERM )
     &                * INTRXN( NRX )
! term equals the contribution from untracked species. Its value is divided among the
! source based on the fraction that each sources has the precursor of the species. If none
! of the source have the precursor, all the untracked contribution goes to the OTHER source
! group or tag.
                  TERM = SOURCE_PROBABILITY( (NTAG_SA+1),NRX ) 
                  IF( .NOT. ZERO_ISAM( NRX ) )THEN
                    DO KTAG = 1, NTAG_SA
                      TOTAL = (SOURCE_PROBABILITY( KTAG,NRX ) + TERM*ISAM_PROBABILITY( KTAG,NRX ))
                      SOURCE_DELTA( KTAG, JSPC ) = SOURCE_DELTA( KTAG, JSPC )
     &                                           + COEFF * TOTAL
                    END DO
                 ELSE   
                    SOURCE_DELTA( OTHRTAG, JSPC ) = SOURCE_DELTA( OTHRTAG, JSPC ) 
     &                                         + COEFF * TERM
                 END IF  
             END DO
          ELSE ! radical species using changed directly from solver and partition based on its sign
              ISAM_PROD  = 0.0D0
              TOTAL_PROD = 0.0D0
              OX_DELTA   = 0.0D0
              DO NTERM = 1, ISAM_SPC_BUDGET( JSPC )%NRXNS_PROD
                 NRX   = ISAM_SPC_BUDGET( JSPC )%IRXN_PROD( NTERM )
                 COEFF = ISAM_SPC_BUDGET( JSPC )%COEFF_POS( NTERM )
     &                 * INTRXN( NRX )
                 TOTAL_PROD = TOTAL_PROD + COEFF
                 IF( .NOT. ZERO_ISAM( NRX ) )THEN
                   DO KTAG = 1, NTAG_SA
                      TERM      = COEFF 
     &                          * ( SOURCE_PROBABILITY( KTAG,NRX ) 
     &                          +   SOURCE_PROBABILITY((NTAG_SA+1),NRX )
     &                          *   ISAM_PROBABILITY  ( KTAG,NRX ) )
                      ISAM_PROD = ISAM_PROD + TERM
                      OX_DELTA( KTAG ) = OX_DELTA( KTAG ) + TERM
                   END DO                    
                 END IF
             END DO
             COEFF = (CONC( NIRR )-YCMID( NIRR ))  
             IF( COEFF .GE. 0.0D0 .AND. TOTAL_PROD .GT. 1.0D-30 )THEN
                 COEFF = COEFF / TOTAL_PROD
#ifdef verbose_isam
                 IF( WRITE_CELL )WRITE(ISAM_LOG,*)ISAM_SPC_BUDGET( JSPC )%SPECIES_NAME,
     &           ' has nonzero production '                 
#endif     
                 DO KTAG = 1, NTAG_SA
                    SOURCE_DELTA( KTAG,JSPC ) = SOURCE_DELTA( KTAG,JSPC )
     &                                        + OX_DELTA( KTAG ) * COEFF
                 END DO
                 TERM = (TOTAL_PROD-ISAM_PROD)*COEFF   
                 SOURCE_DELTA( OTHRTAG,JSPC ) = SOURCE_DELTA( OTHRTAG,JSPC )
     &                                        + TERM
#ifdef verbose_isam
                 IF( WRITE_CELL )WRITE(ISAM_LOG,'(2A,12(1X,ES20.10))')ISAM_SPC_BUDGET( JSPC )%SPECIES_NAME,
     &           ' has nonzero production ',(SOURCE_DELTA( KTAG,JSPC ),KTAG =1, NTAG_SA),(TOTAL_PROD-ISAM_PROD)/TOTAL_PROD,
     &          (CONC( NIRR )-YCOLD( NIRR ))                  
#endif     
             ELSE
                 DO KTAG = 1, NTAG_SA
                    TERM = MAX(1.0D0-NOT_ISAM_SOURCE( KTAG,NIRR ),0.0D0) * COEFF
                    SOURCE_DELTA( KTAG,JSPC ) = SOURCE_DELTA( KTAG, JSPC )
     &                                        + TERM  
                 END DO
                 IF( WRITE_CELL )WRITE(ISAM_LOG,'(2A,12(1X,ES12.4))')ISAM_SPC_BUDGET( JSPC )%SPECIES_NAME,
     &           ' has zero production ',(SOURCE_DELTA( KTAG,JSPC ),KTAG =1, NTAG_SA),
     &           (CONC( NIRR )-YCOLD( NIRR ))             
             END IF
             TOTAL = SUM(   SOURCE_DELTA( 1:NTAG_SA,JSPC ))
             TERM  = (CONC( NIRR )-YCOLD( NIRR ))            
#ifdef verbose_isam
             IF( WRITE_CELL )WRITE(ISAM_LOG,*)ISAM_SPC_BUDGET( JSPC )%SPECIES_NAME,
     &       ' has net change ', TOTAL,(TOTAL-TERM)
#endif
          END IF
       END DO
       
#ifdef verbose_isam
       IF( WRITE_CELL )THEN
          WRITE(ISAM_LOG,*)'Source Delta Concentration include extra delta then last two columns, Solver Change and Sum Deltas'
          WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Species/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
!          DO S = 1, NUMB_MECH_SPC 
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             S = ISAM_TO_CHEM( JSPC )
             WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(S),(SOURCE_DELTA( KTAG, JSPC ),KTAG=1,NTAG_SA+1 ),
     &      (CONC( S )-YCOLD( S )),SUM(SOURCE_DELTA( 1:(NTAG_SA+1), JSPC ))
          END DO
          WRITE(ISAM_LOG,*)' '
          WRITE(ISAM_LOG,'(A21,4(1X,A12))')'ISAM_CHEMISTRY_SPC,','  IRR Conc,  ',' True Conc,  correction ',
     &                                    '  IRR Change,  ','  True Change  '
!          DO NIRR = 1, NUMB_MECH_SPC 
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             NIRR = ISAM_TO_CHEM( JSPC )
             COEFF = 0.0D0
             DO KTAG = 1, NTAG_SA+1

                COEFF = COEFF + SOURCE_DELTA( KTAG, JSPC )
             END DO
             TOTAL =  CONC( NIRR )- YCOLD( NIRR )             
             WRITE(ISAM_LOG,'(A21,5(1X,",",ES12.4))')'ISAM_' // CHEMISTRY_SPC(NIRR),
     &       MAX(0.0D0,YCOLD( NIRR )+COEFF),CONC( NIRR ),COEFF,TOTAL,(COEFF-TOTAL)
          END DO
       END IF
#endif
       IF( OX_RADICAL_FOUND .GT. 0 )THEN
          DO NIRR = 1, NUMB_MECH_SPC 
             YCMID( NIRR ) = CONC( NIRR )
          END DO
       END IF
          
       IF( UPDATE_SOLD )THEN
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             NIRR = ISAM_TO_CHEM( JSPC )
             DEFICIT  = .FALSE.
             POSITIVE = 0 
             TOTAL    = 0.0D0
             DO KTAG = 1, NTAG_SA
                SOLD( KTAG,NIRR ) = SOLD( KTAG,NIRR ) +  SOURCE_DELTA( KTAG,JSPC )
                IF ( SOLD( KTAG,NIRR ) .LT. 0.0D0 ) THEN
                   TOTAL = TOTAL + SOLD( KTAG, NIRR )
                   SOLD( KTAG, NIRR )  = DCONMIN_TAG
                   SOURCE_ZERO( KTAG ) = .TRUE.
                   DEFICIT             = .TRUE.
                ELSE 
                   SOURCE_ZERO( KTAG ) = .FALSE.                    
                   POSITIVE   = POSITIVE + 1 
                END IF 
             END DO
             IF( POSITIVE .GT. 0 ) THEN
C...Correct concentrations if any source delta greater than their source concentration
                DO WHILE ( DEFICIT )
                   TERM     = TOTAL / REAL( POSITIVE,8 )
                   TOTAL    = 0.0D0
                   BALANCE  = .TRUE.
                   POSITIVE = 0 
                   DO KTAG = 1, NTAG_SA
                      IF( .NOT. SOURCE_ZERO( KTAG ) )THEN
                         SOLD( KTAG, NIRR ) = SOLD( KTAG, NIRR )+TERM
                         IF ( SOLD( KTAG, NIRR ) .LT. 0.0D0 ) THEN
                            TOTAL = TOTAL + SOLD( KTAG, NIRR )
                            SOLD( KTAG, NIRR )  = DCONMIN_TAG
                            SOURCE_ZERO( KTAG ) = .TRUE.
                            BALANCE    = .FALSE.
                         ELSE 
                            SOURCE_ZERO( KTAG ) = .FALSE.                    
                            POSITIVE   = POSITIVE + 1 
                         END IF
                      END IF                    
                   END DO
                   IF( BALANCE .OR. POSITIVE .EQ. 0 )DEFICIT = .FALSE.
                END DO
             END IF ! POSITIVE > 0
          END DO

C..Clear source deltas
         SOURCE_DELTA = 0.0D0
C..Save concentrations
         DO NIRR = 1, NUMB_MECH_SPC 
            YCOLD( NIRR ) = CONC( NIRR )
         END DO
#ifdef verbose_isam
         IF( WRITE_CELL )THEN
            WRITE(ISAM_LOG,*)'Final Source Concentration then New Total (Last Column) Concentrations'
            WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Species/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
!            DO S = 1, NUMB_MECH_SPC 
            DO JSPC = 1, ISAM_CHEMISTRY_SPC
               S = ISAM_TO_CHEM( JSPC )
               WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(S),(SOLD( KTAG, S ),KTAG=1,NTAG_SA ),
     &         CONC( S ),SUM(SOLD(1:NTAG_SA,S) )
            END DO
         END IF
#endif         
       END IF

      

      RETURN
      END SUBROUTINE SA_IRR_UNBLOCKED
      END MODULE SA_IRR_DEFN
