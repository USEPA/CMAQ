       MODULE MECHANISM_DATA
       
         USE MECHANISM_PARMS
         USE TEXT_UTILITIES

         IMPLICIT NONE

         
         CHARACTER( 120 ) :: AUTHOR
         CHARACTER( 586 ) :: OUTDIR 
         CHARACTER( 586 ) :: EQUATIONS_MECHFILE
         CHARACTER(  32 ) :: MECHNAME            = 'UNDEFINED'
         CHARACTER(  32 ) :: MECHNAME_LOWER_CASE = 'undefined'


         INTEGER          :: UNIT_FUNCTIONS
         CHARACTER( 599 ) :: FUNCTIONS_CAPTURED
         INTEGER          :: LINES_CAPTURED
         
         
        
         INTEGER KUNITS, 
     &           KTN1,
     &           KTN2,
     &           KTN3,
     &           KTN4,
     &           KTN5,
     &           KTN6,
     &           KTN7,
!    &           KCNV,
     &           NFALLOFF

         INTEGER, ALLOCATABLE :: 
     &           KTYPE       ( : ),
     &           IRXBITS     ( : ),
     &           IORDER      ( : ),
     &           KRX1  ( : ), 
     &           KRX2  ( : ), 
     &           KRX3  ( : ),
     &           KRX4  ( : ),
     &           KRX5  ( : ),
     &           KRX6  ( : ),
     &           KRX7  ( : ),
!    &           KRXCNV( : ),
     &           HAL_PHOTAB( : ),
     &           IRRFALL( : )

         INTEGER :: NRATE_STRING                 = 0
         
         INTEGER, ALLOCATABLE         :: KSTRING( : )    
         CHARACTER( 81 ), ALLOCATABLE :: RATE_STRING( : )

         INTEGER NWM
         INTEGER NWW
         INTEGER NWO2
         INTEGER NWN2
         INTEGER NWCH4
         INTEGER NWH2

         INTEGER, ALLOCATABLE :: NRXWM  ( : )
         INTEGER, ALLOCATABLE :: NRXWW  ( : )
         INTEGER, ALLOCATABLE :: NRXWO2 ( : )
         INTEGER, ALLOCATABLE :: NRXWN2 ( : )
         INTEGER, ALLOCATABLE :: NRXWCH4( : )
         INTEGER, ALLOCATABLE :: NRXWH2 ( : )

         REAL( 8 ), ALLOCATABLE :: RTDAT( :,: )
         REAL( 8 ), ALLOCATABLE :: RFDAT( :,: )
         REAL( 8 ), ALLOCATABLE :: CONST( : )        
          
         INTEGER         :: NPDERIV             = 0    ! number nonzero PD in mechanism
         INTEGER         :: NMPHOT                     ! number of photolysis reactions
         INTEGER         :: NSUNLIGHT                  ! number of sunlight reactions
         INTEGER         :: ZERO_REACT_SUNLIGHT  = 0   ! number zero reactant reactions in sunlight reactions
         INTEGER         :: ONE_REACT_SUNLIGHT   = 0   ! number one reactant reactions in sunlight reactions
         INTEGER         :: NTHERMAL                   ! number of thermal (non-sunlight-dependent) reactions
         INTEGER         :: ONE_REACT_THERMAL    = 0   ! number one reactant reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: ZERO_REACT_THERMAL   = 0   ! number zero reactant reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: TWO_REACT_THERMAL    = 0   ! number second order reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: THREE_REACT_THERMAL  = 0   ! number three reactant reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: NSPECIAL_RXN               ! number of special rate constant reactions
         INTEGER         :: NSPECIAL                   ! number of special rate expressions
         INTEGER         :: MSPECTERMS                 ! highest number of terms in the expressions

         INTEGER, ALLOCATABLE :: ISPECIAL( :,: )

         INTEGER         :: NFUNCTIONS = 0                ! number of user defined functions

         INTEGER         :: ZERO_REACT_REACTIONS  = 0  ! number zero reactant reactions
         INTEGER         :: ONE_REACT_REACTIONS   = 0  ! number one reactant reactions
         INTEGER         :: TWO_REACT_REACTIONS   = 0  ! number second order reactions
         INTEGER         :: THREE_REACT_REACTIONS = 0  ! number three reactant reactions

         CHARACTER( 16 ), ALLOCATABLE :: SPECIAL( : )   
         CHARACTER( 16 ), ALLOCATABLE :: FUNCTIONS( : ) 
         CHARACTER( 500), ALLOCATABLE :: FORMULA( : )   

         INTEGER,         ALLOCATABLE :: NKC_TERMS( : )
         CHARACTER( 16 ), ALLOCATABLE :: KC_TERMS(   :,:,: )
         INTEGER,         ALLOCATABLE :: INDEX_KTERM( :,: )
         INTEGER,         ALLOCATABLE :: INDEX_CTERM( :,:)
         REAL( 8 ),       ALLOCATABLE :: KC_COEFFS(   :,: )
         INTEGER,         ALLOCATABLE :: N_OPERATORS( : )
         INTEGER,         ALLOCATABLE :: OPERATORS( :,: )
         REAL( 8 ),       ALLOCATABLE :: OPERATOR_COEFFS( :,: )

         INTEGER, ALLOCATABLE :: ORDER_SPECIAL( : )
         
         INTEGER NPHOTAB                          ! no. of unique photolysis rates
         INTEGER,         ALLOCATABLE :: IPH( :,: )
         CHARACTER( 16 ), ALLOCATABLE :: PHOTAB( : ) ! photolysis rate name or label

         INTEGER MHETERO                          ! no. of heteorogeneous reactions
         INTEGER NHETERO                          ! no. of unique heteorogeneous rates 
         INTEGER,         ALLOCATABLE :: IHETERO( :,: )         ! mapping bewtween reaction # and unique heteorogeneous rates
         CHARACTER( 16 ), ALLOCATABLE :: HETERO( : ) ! names of unique heteorogeneous rates

         INTEGER MXPRD                            ! max no. products

         INTEGER, ALLOCATABLE   :: NPRDCT( : )               ! no. of nonconstant products for rx j
         INTEGER, ALLOCATABLE   :: N_ALL_PRODUCTS( : )       ! no. of constant plus nonconstant products for rx j
         INTEGER, ALLOCATABLE   :: NREACT( : )               ! no. of reactants for rx j
         INTEGER, ALLOCATABLE   :: IRR( :,: )
         INTEGER, ALLOCATABLE   :: INDEX_PRODUCT( :,: )
         REAL( 8 ), ALLOCATABLE :: SC ( :,: )                  ! stoichiometric coefficients for nonconstant products for rx j  
         REAL( 8 ), ALLOCATABLE :: STOICHIOMETRIC_COEFF( :,: ) ! stoichiometric coefficients for constant and nonconstant products for rx j

         INTEGER,   ALLOCATABLE, SAVE :: IRR_NET    ( :,: )       ! species indices having net change from reaction
         REAL( 8 ), ALLOCATABLE, SAVE :: SC_NET     ( :,: )       ! stio coefficients for net species
         INTEGER,   ALLOCATABLE, SAVE :: NET_SPECIES( : )         ! # species having net change from reaction
         INTEGER,   ALLOCATABLE, SAVE :: PURE_NREACT( : )         ! # reactants that are not also products
         REAL( 8 ), ALLOCATABLE, SAVE :: NET_RCOEFF ( :,: )       ! stio coefficients for net species  

c.. Variables for steady-state species
         INTEGER         :: N_SS_SPC = 0                         ! No. of SS species
         INTEGER         :: MAX_SS_LOSS = 0                      ! Max no of reactions for which 1 SS species appears as a reactant
         INTEGER         :: MAX_SS_PROD = 0                      ! Max no of reactions for which 1 SS species appears as a product

         CHARACTER( 16 ), ALLOCATABLE ::  SS_SPC( : )                   ! List of SS pecies names
         INTEGER,         ALLOCATABLE :: SS_RCT_COEF( :,: )    ! Reactant coeffs for each SS species
         REAL,            ALLOCATABLE :: SS_PRD_COEF( :, : )    ! Product coeffs for each SS species

         INTEGER, ALLOCATABLE :: N_LOSS_RXNS( : )              ! No. of loss rxns for each SS species
         INTEGER, ALLOCATABLE :: N_PROD_RXNS( : )              ! No. of prod rxns for each SS species
         INTEGER, ALLOCATABLE :: SS_LOSS_RXNS( :,: )   ! List of rxns in which SS species is a reactant
         INTEGER, ALLOCATABLE :: SS_PROD_RXNS( :,: )   ! List of rxns in which SS species is a product
         INTEGER, ALLOCATABLE :: SS_RCT_IND( : )               ! SS spc ind that reacts w/ a non-SS spc
         REAL, ALLOCATABLE    :: SS_PROD_COEF( :,: )   ! Yields for rxns producing a SS species
         

         CHARACTER( 16 ), ALLOCATABLE :: RXLABEL( : )   ! label for rx 

         CHARACTER( 586 ) :: EQNAME_SPCS
         CHARACTER( 586 ) :: EQNAME_RXDT
         CHARACTER( 586 ) :: EQNAME_RXCM
         CHARACTER( 586 ) :: FNAME_MODULE
         CHARACTER( 586 ) :: FNAME_DATA_MODULE
         CHARACTER( 586 ) :: FNAME_FUNC_MODULE

         INTEGER        ::  EXUNIT_SPCS
         INTEGER        ::  EXUNIT_RXDT
         INTEGER        ::  EXUNIT_RXCM
         
         INTEGER        ::  MODULE_UNIT
         INTEGER        ::  DATA_MODULE_UNIT
         INTEGER        ::  FUNC_MODULE_UNIT
         

         LOGICAL, SAVE      :: CALC_DELTA_ATOMS    = .FALSE.  ! calculated how reactions change the tracked elements
         LOGICAL, SAVE      :: ATOMS IN_NAMELISTS  = .TRUE.   ! read trailing comments line from species namelists
         LOGICAL, SAVE      :: USE_SPCS_NAMELISTS  = .TRUE.   ! species data based on CMAQ NMLS
         LOGICAL, SAVE      :: WRITE_CGRID_DATA  = .TRUE.

         INTEGER,            ALLOCATABLE ::  CGRID_INDEX  ( : )
         INTEGER,            ALLOCATABLE ::  TYPE_INDEX   ( : )
         LOGICAL, SAVE,      ALLOCATABLE ::  CONVERT_CONC ( : )
         REAL,               ALLOCATABLE ::  SPECIES_MOLWT( : )
         CHARACTER( 16),     ALLOCATABLE ::  CGRID_SPC    ( : )
         CHARACTER(LEN = 2), ALLOCATABLE ::  SPECIES_TYPE ( : )

         LOGICAL                      ::  HALOGEN_PARAMETER = .FALSE.          
         INTEGER                      ::  N_GAS_CHEM_SPC
         INTEGER                      ::  NUMB_MECH_SPCS
         INTEGER                      ::  MAXLEN_SPECIES = 1
         INTEGER ,        ALLOCATABLE ::  MECHANISM_INDEX( : )
         CHARACTER( 16 ), ALLOCATABLE ::  MECHANISM_SPC  ( : )
         CHARACTER( 16 ), ALLOCATABLE ::  SPARSE_SPECIES ( : )



         INTEGER, PARAMETER :: NCONSTANT_SPECIES = 6
         REAL( 8 )          :: VALUES_CONSTANT ( NCONSTANT_SPECIES ) = 0.0D0

         INTEGER, PARAMETER :: NFIXED_SPECIES  = NCONSTANT_SPECIES
         CHARACTER(  16 ) :: FIXED_SPECIES( NFIXED_SPECIES ) = (/
     &                       'M               ',
     &                       'H2O             ',
     &                       'O2              ',
     &                       'N2              ',
     &                       'CH4             ',
     &                       'H2              ' /)
     
         INTEGER              :: NRXN_FIXED_SPECIES( NFIXED_SPECIES )

         INTEGER, PARAMETER :: ind_M   = 1 
         INTEGER, PARAMETER :: ind_H2O = 2 
         INTEGER, PARAMETER :: ind_O2  = 3
         INTEGER, PARAMETER :: ind_N2  = 4 
         INTEGER, PARAMETER :: ind_CH4 = 5
         INTEGER, PARAMETER :: ind_H2 =  6 

         INTEGER, PARAMETER :: ind_DUMMY = 7 



         INTEGER, ALLOCATABLE, SAVE   :: INDEX_FIXED_SPECIES( :, : )
         

         TYPE REACTION
            CHARACTER( 16 ) :: LABEL( 2 )      ! name of reaction and if needed reference 
            INTEGER   :: IRXBITS               ! bit value for rate constant
            INTEGER   :: RATE_TYPE             ! type of rate constant
            INTEGER   :: NPRDCT                ! no. of products
            INTEGER   :: NREACT                ! no. of reactants
            INTEGER   :: ORDER                 ! order of reaction
            INTEGER   :: IRR( MAXPRODS+3 )     ! reactant and product species indices
            INTEGER   :: HETEO_INDEX( 2 )      ! mechanism reaction indices if heterogeneous type
            INTEGER   :: PHOTO_INDEX( 3 )      ! mechanism reaction indices if rate constant photolysis
            INTEGER   :: FALLOFF_INDEX         ! mechanism reaction indices if falloff type
            INTEGER   :: SPECIAL_INDEX( 2 )    ! mechanism reaction indices if rate constant a special expression
            REAL( 8 ) :: SC( MAXPRODS )        ! product stiochometric coefficients
            REAL( 8 ) :: RTDAT( 3 )            ! general data for rate constant
            REAL( 8 ) :: RFDAT( 5 )            ! data of fall rate constant type
            INTEGER   :: NAIR_RCTNTS           ! # times M or air a reactant
            INTEGER   :: NH2O_RCTNTS           ! # times water a reactant
            INTEGER   :: N_O2_RCTNTS           ! # times O2 a reactant
            INTEGER   :: N_N2_RCTNTS           ! # times H2 a reactant
            INTEGER   :: N_H2_RCTNTS           ! # times N2 a reactant
            INTEGER   :: NCH4_RCTNTS           ! # times methane a reactant
            INTEGER   :: NET_SPECIES           ! Net # Species Producd or Lost
            INTEGER   :: PURE_NREACT           ! no. of reactant that are not also products
            REAL( 8 ) :: SC_NET ( MAXPRODS+3 ) ! net reactant and product stiochometric coefficients
            INTEGER   :: IRR_NET( MAXPRODS+3 ) ! reactant and product species indices            
            CHARACTER( 81 ) :: RATE_STRING   = ' '
            INTEGER         :: STRING_INDEX  = 0
         END TYPE REACTION
 
         TYPE ( REACTION ), ALLOCATABLE :: PHOTOLYSIS_REACTIONS( : )  
         TYPE ( REACTION ), ALLOCATABLE :: THERMAL_REACTIONS   ( : )  
         

c..Miscellaneous variables
         INTEGER, PARAMETER :: NCS  = 1        ! no. of chemical mechanisms
         INTEGER, PARAMETER :: NCS2 = 2 * NCS  ! accounts for day/night 


c..Sparse Matrix maximum dimensions
         INTEGER, SAVE :: MAXGL3    ! Max # of P/L terms per species
         INTEGER, SAVE :: MXARRAY   ! Max # of terms in I-hJ matrix

c..Mechanism specific variables
         INTEGER, SAVE :: N_SPEC               ! No. of species in mech
         INTEGER, SAVE :: NRXNS               ! No. of reactions in mech

         INTEGER, SAVE :: MXCOUNT1, MXCOUNT2   ! Sparse matrx pntr dimensions
         INTEGER, SAVE :: MXRR, MXRP           ! Max # of PD terms
         INTEGER, SAVE :: MXRCT                ! max no. of reactants


c..Sparse Matrix variables 
         INTEGER, SAVE :: ISCHAN          ! No. of reacting species in current mech
         INTEGER, SAVE :: ISCHANG( NCS  ) ! No. of reacting species in day & nite
         INTEGER, SAVE :: NUSERAT( NCS2 ) ! No. of active rxns in day & nite
         INTEGER, SAVE :: IARRAY(  NCS2 ) ! No. of PD terms in I-hJ matrix

C Most of the following are allocated
         INTEGER, ALLOCATABLE, SAVE :: NKUSERAT( :,: )     ! Rxn nos of active rxns
         INTEGER, ALLOCATABLE, SAVE :: NET_EFFECT( :, : )  ! reaction's net effect on species
         INTEGER, ALLOCATABLE, SAVE :: IRM2  ( :,: )       ! Species rxn array
         INTEGER, ALLOCATABLE, SAVE :: IRM2SP( :,: )       ! Species indices for special rate expressions
         INTEGER, ALLOCATABLE, SAVE :: ICOEFF( :,:,: )     ! stoich coeff indx
         
         INTEGER, ALLOCATABLE, SAVE :: JARRAYPT( :,:,: )   ! A-Matrix index
         INTEGER, ALLOCATABLE, SAVE :: JARRL( :,:,: )      ! Pntr to PD Loss term
         INTEGER, ALLOCATABLE, SAVE :: JARRP( :,:,: )      ! Pntr to PD Prod term
         INTEGER, ALLOCATABLE, SAVE :: JLIAL( :,:,: )      ! Spec # for PD loss term
         INTEGER, ALLOCATABLE, SAVE :: JPIAL( :,:,: )      ! Spec # for PD prod term 
        
         INTEGER, ALLOCATABLE, SAVE :: INEW2OLD( : )     ! Spec index xref
         INTEGER, ALLOCATABLE, SAVE :: IOLD2NEW( : )     ! Spec index xref

         INTEGER, ALLOCATABLE, SAVE :: NDERIVL( :,: )      ! # of PD loss terms
         INTEGER, ALLOCATABLE, SAVE :: NDERIVP( :,: )      ! # of PD prod terms

C descirbes the partial derivatives in each sparse Jacobian component

         INTEGER, ALLOCATABLE,   SAVE :: NDERIVN1( :, : )      ! # PD with a coefficient of -1
         INTEGER, ALLOCATABLE,   SAVE :: NDERIVP1( :, : )      ! # PD with a coefficient of  1 
         INTEGER, ALLOCATABLE,   SAVE :: NDERIVCO( :, : )      ! # PD with other coefficients
         INTEGER, ALLOCATABLE,   SAVE :: PDERIVN1( :, :, : )   ! PD index with a coefficient of -1
         INTEGER, ALLOCATABLE,   SAVE :: PDERIVP1( :, :, : )   ! PD index with a coefficient of  1 
         INTEGER, ALLOCATABLE,   SAVE :: PDERIVCO( :, :, : )   ! PD index with other coefficients
         REAL( 8 ), ALLOCATABLE, SAVE :: PD_COEFF( :, :, : )   ! PD coefficients
 

c..indices for decomposition
         INTEGER, ALLOCATABLE, SAVE :: JZLO( : )           ! # of ops in decmp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IDEC1LO( :,: )      ! decomp loop 1 bound
         INTEGER, ALLOCATABLE, SAVE :: IDEC1HI( :,: )      ! decomp loop 1 bound
         
         INTEGER, ALLOCATABLE, SAVE :: IJDECA( : ) ! Pntr for ij term 1 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IJDECB( : ) ! Pntr for ij term 2 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IKDECA( : ) ! Pntr for ik term 1 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IKDECB( : ) ! Pntr for ik term 2 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: KJDECA( : ) ! Pntr for kj term 1 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: KJDECB( : ) ! Pntr for kj term 2 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: JZEROA( : ) ! Pntr for j term 1 in decomp loop 2
         INTEGER, ALLOCATABLE, SAVE :: JZEROB( : ) ! Pntr for j term 2 in decomp loop 2

         INTEGER, ALLOCATABLE, SAVE :: JHIZ1( :,: )  ! # of 2-term groups in dcmp loop 2
         INTEGER, ALLOCATABLE, SAVE :: JHIZ2( :,: )  ! # of 1-term groups in dcmp loop 2

         
         INTEGER, ALLOCATABLE, SAVE :: KZLO1( :,: )  ! Start indx for 2-term bksb loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZLO2( :,: )  ! Start indx for 1-term bksb loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZHI0( :,: )  ! End index for 5-term bksub loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZHI1( :,: )  ! End index for 2-term bksub loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZERO( :,: )  ! Pointer to bksub j index
         
         INTEGER, ALLOCATABLE, SAVE :: MZHI0 ( :,: ) ! End index for 5-term bksub loop 2
         INTEGER, ALLOCATABLE, SAVE :: MZHI1 ( :,: ) ! End index for 2-term bksub loop 2
         INTEGER, ALLOCATABLE, SAVE :: MZILCH( :,: ) ! # of calcs in bksub loop 2 (U)
         INTEGER, ALLOCATABLE, SAVE :: MZLO1 ( :,: ) ! Start indx for 2-term bksb loop 2
         INTEGER, ALLOCATABLE, SAVE :: MZLO2 ( :,: ) ! Start indx for 1-term bksb loop 2
         INTEGER, ALLOCATABLE, SAVE :: KZILCH( :,: ) ! # of calcs in bksub loop 1 (L)

         LOGICAL, SAVE  :: LREORDER = .TRUE.             ! Flag to reorder or not

         LOGICAL        :: SUN_BELOW                 ! flag to determining to put sunlight reactions
                                                     ! below other reactions in reorder reactions


         INTEGER        :: COPY_MECHANISM
         LOGICAL        :: READING_REACTIONS = .FALSE.
         LOGICAL        :: WRITE_RATE        = .FALSE.

         CHARACTER( LEN = 81 ) :: START_RATE
         LOGICAL               :: RATE_BEGINS = .FALSE.


         LOGICAL               :: ECHO_LINE       = .TRUE.
         INTEGER               :: POSITION_EQUALS = 1 
         CHARACTER(586)        :: PAD_FOR_EQUALS  = " "
         
         CONTAINS
         
         SUBROUTINE INIT_MECH_DATA()

           USE GET_ENV_VARS
!   Function initialize module variables         
           IMPLICIT NONE
         
         
           INTEGER    :: ISPC, IRX    ! loop counters
           INTEGER    :: STATUS       ! get environment status
           
           CHARACTER( 32 )    :: SPECIES_REORDER   = 'REORDER_SPECIES'
           CHARACTER( 32 )    :: REACTIONS_REORDER = 'REORDER_REACTIONS'
           CHARACTER( 32 )    :: EFFECTS_ASSESS    = 'ASSESS_EFFECTS'
           CHARACTER( 32 )    :: CUT_RCONST_DATA   = 'OMIT_RCONST_DATA'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get environment values for optimization options
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            REORDER_SPECIES   = GET_ENV_FLAG( SPECIES_REORDER, "Reorder Species based on Empty Jacobain Values",
     &                          REORDER_SPECIES, STATUS)
            REORDER_REACTIONS = GET_ENV_FLAG( REACTIONS_REORDER, "Reorder Species based on Type and Order",
     &                          REORDER_SPECIES, STATUS)
            ASSESS_EFFECTS    = GET_ENV_FLAG( EFFECTS_ASSESS, "Asset Net Effect of Reactions on Species",
     &                          ASSESS_EFFECTS, STATUS)
            OMIT_RCONST_DATA  = GET_ENV_FLAG( CUT_RCONST_DATA, "Omit Rate Constant Parameters",
     &                          ASSESS_EFFECTS, STATUS)            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         ALLOCATE( KTYPE       ( MAXRXNUM ),
     &             IRXBITS     ( MAXRXNUM ),
     &             IORDER      ( MAXRXNUM ),
     &             KRX1  ( MAXRXNUM ), 
     &             KRX2  ( MAXRXNUM ), 
     &             KRX3  ( MAXRXNUM ),
     &             KRX4  ( MAXRXNUM ),
     &             KRX5  ( MAXRXNUM ),
     &             KRX6  ( MAXRXNUM ),
     &             KRX7  ( MAXRXNUM ),
!    &              KRXCNV( MAXRXNUM ),
     &             HAL_PHOTAB( MAXRXNUM ),
     &             IRRFALL( MAXFALLOFF ) )

         ALLOCATE( NRXWM  ( MAX3BODIES ) )
         ALLOCATE( NRXWW  ( MAX3BODIES ) )
         ALLOCATE( NRXWO2 ( MAX3BODIES ) )
         ALLOCATE( NRXWN2 ( MAX3BODIES ) )
         ALLOCATE( NRXWCH4( MAX3BODIES ) )
         ALLOCATE( NRXWH2 ( MAX3BODIES ) )

         ALLOCATE( RTDAT( 3,MAXRXNUM )   )
         ALLOCATE( RFDAT( 5,MAXFALLOFF ) )
         ALLOCATE( CONST( MAXCONSTS )    )
         ALLOCATE( ISPECIAL( MAXSPECRXNS,2 ) )

         CONST = 0.0D0

         ALLOCATE( SPECIAL( MAXSPECRXNS )    ) 
         SPECIAL( 1:MAXSPECRXNS ) = ' '
         ALLOCATE( FUNCTIONS( MAXFUNCTIONS ) ) 
         FUNCTIONS( 1:MAXFUNCTIONS ) = ' '   
         ALLOCATE( FORMULA( MAXFUNCTIONS )   ) 
         FORMULA( MAXFUNCTIONS ) = ''  

         ALLOCATE( NKC_TERMS(  MAXSPECRXNS )                   )
         ALLOCATE( KC_TERMS(   MAXSPECRXNS,  MAXSPECTERMS, 2)  )
         ALLOCATE( INDEX_KTERM( MAXSPECRXNS, MAXSPECTERMS)     )
         ALLOCATE( INDEX_CTERM( MAXSPECRXNS, MAXSPECTERMS)     )
         ALLOCATE( KC_COEFFS(  MAXSPECRXNS,  MAXSPECTERMS)     )
         ALLOCATE( N_OPERATORS( MAXSPECRXNS )                  )
         ALLOCATE( OPERATORS( MAXSPECRXNS, MAXSPECTERMS )      )
         ALLOCATE( OPERATOR_COEFFS( MAXSPECRXNS, MAXSPECTERMS) )

         ALLOCATE( IPH( MAXPHOTRXNS,3 )  )
         ALLOCATE( PHOTAB( MAXPHOTRXNS ) )

         ALLOCATE( IHETERO( MAXPHOTRXNS,2 ) )
         ALLOCATE( HETERO( MAXPHOTRXNS )    )

         ALLOCATE( NPRDCT( MAXRXNUM )         )
         ALLOCATE( N_ALL_PRODUCTS( MAXRXNUM )  )
         ALLOCATE( NREACT( MAXRXNUM )         )
         ALLOCATE( IRR( MAXRXNUM,MAXPRODS+3 ) ) 
         ALLOCATE( INDEX_PRODUCT( MAXRXNUM,MAXPRODS+MAXRCTNTS ) ) 
         ALLOCATE( SC ( MAXRXNUM,MAXPRODS )   )
         ALLOCATE( STOICHIOMETRIC_COEFF  ( MAXRXNUM,MAXPRODS+MAXRCTNTS )   )

            DO 101 IRX = 1, MAXRXNUM
               DO ISPC = 1, MAXPRODS+3
                  IRR( IRX,ISPC ) = 0
               END DO
               DO ISPC = 1, MAXPRODS
                  SC( IRX,ISPC ) = 0.0D0
               END DO
               STOICHIOMETRIC_COEFF( IRX,: ) = 0.0D0
               INDEX_PRODUCT( IRX,: ) = 0
               DO ISPC = 1, 3
                   RTDAT( ISPC,IRX ) = 0.0D0
               END DO
               KTYPE( IRX ) = 0
               IORDER( IRX )  = 0
               IRXBITS( IRX ) = 0
               KRX1( IRX ) = 0
               KRX2( IRX ) = 0
               KRX3( IRX ) = 0
               KRX4( IRX ) = 0
               KRX5( IRX ) = 0
               KRX6( IRX ) = 0
               KRX7( IRX ) = 0
               NPRDCT( IRX ) = 0
               N_ALL_PRODUCTS( IRX ) = 0
101         CONTINUE
            HAL_PHOTAB = 0
            NFALLOFF   = 0

          ALLOCATE ( KSTRING( MAXFUNCTIONS ) )
          KSTRING( 1:MAXFUNCTIONS )  = 0
          
          ALLOCATE( RATE_STRING( MAXFUNCTIONS ) )
          RATE_STRING( 1:MAXFUNCTIONS ) = ''


            DO 103 IRX = 1, MAXFALLOFF
               IRRFALL( IRX ) = 0   
               DO ISPC = 1, 5
                  RFDAT( ISPC,IRX ) = 0.0D0
               END DO
103         CONTINUE

            DO 105 IRX = 1, MAX3BODIES
               NRXWM( IRX )   = 0
               NRXWW( IRX )   = 0
               NRXWO2( IRX )  = 0
               NRXWN2( IRX )  = 0
               NRXWCH4( IRX ) = 0
               NRXWH2( IRX )  = 0
105         CONTINUE

            KTN1 = 0
            KTN2 = 0
            KTN3 = 0
            KTN4 = 0
            KTN5 = 0
            KTN6 = 0
            KTN7 = 0
!           KCNV = 0
            NWM  = 0
            NWW  = 0
            NWO2 = 0
            NWN2 = 0
            NWCH4 = 0
            NWH2 = 0

            NPHOTAB = 0
            NMPHOT  = 0
            NSUNLIGHT = 0
            NTHERMAL  = 0
            DO ISPC = 1, MAXPHOTRXNS
               IPH( ISPC,1 ) = 0
               IPH( ISPC,2 ) = 0
               IPH( ISPC,3 ) = 0
               PHOTAB( ISPC ) = ' '
            END DO
            
            NSPECIAL     = 0
            NSPECIAL_RXN = 0
            MSPECTERMS   = 1

            DO ISPC = 1, MAXSPECRXNS
               ISPECIAL( ISPC,1 ) = 0
               ISPECIAL( ISPC,2 ) = 0
               SPECIAL( ISPC )    = ' '
               NKC_TERMS( ISPC )  = 0
               KC_COEFFS( ISPC,  1:MAXSPECTERMS) = 0.0
               KC_TERMS(  ISPC,  1:MAXSPECTERMS, 1) = ' '
               KC_TERMS(  ISPC,  1:MAXSPECTERMS, 2) = ' '
               INDEX_KTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = -1
               INDEX_CTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = 0
               N_OPERATORS( ISPC )  = 0
               OPERATORS(   ISPC, 1:MAXSPECTERMS)  = 0
               OPERATOR_COEFFS( ISPC, 1:MAXSPECTERMS) = 0.0
            END DO
            
            IHETERO = 0
            NHETERO = 0
            HETERO  = '                '
            
         ALLOCATE( SS_SPC( MAXNLIST )                 )
         ALLOCATE( SS_RCT_COEF( MAXNLIST, MAXRXNUM )  )
         ALLOCATE( SS_PRD_COEF( MAXNLIST, MAXRXNUM )  )
         ALLOCATE( N_LOSS_RXNS( MAXNLIST )            )
         ALLOCATE( N_PROD_RXNS( MAXNLIST )            )
         ALLOCATE( SS_LOSS_RXNS( MAXNLIST, MAXRXNUM ) )
         ALLOCATE( SS_PROD_RXNS( MAXNLIST, MAXRXNUM ) )
         ALLOCATE( SS_RCT_IND( MAXRXNUM )             )
         ALLOCATE( SS_PROD_COEF( MAXNLIST, MAXRXNUM ) )
         

            SS_RCT_COEF = 0                 ! Array initialization
            SS_PRD_COEF = 0.0               ! Array initialization
            SS_RCT_IND  = 0                 ! Array initialization
            MAX_SS_LOSS = 0
            MAX_SS_PROD = 0
            

         ALLOCATE( RXLABEL( MAXRXNUM ) )

         CALL CREATE_REACTION_LISTS()

         RETURN
         END SUBROUTINE INIT_MECH_DATA
         SUBROUTINE CREATE_REACTION_LISTS()
           IMPLICIT NONE
            
            LOGICAL, SAVE :: LISTS_CREATED = .FALSE.

            IF( LISTS_CREATED )THEN
                RETURN
            END IF
            LISTS_CREATED = .TRUE.
            
                
            ALLOCATE( PHOTOLYSIS_REACTIONS( MAXRXNUM ) )
            CALL INIT_REACTION_LIST( PHOTOLYSIS_REACTIONS )

            ALLOCATE( THERMAL_REACTIONS( MAXRXNUM ) )
            CALL INIT_REACTION_LIST( THERMAL_REACTIONS )
            RETURN

         END SUBROUTINE CREATE_REACTION_LISTS
         SUBROUTINE INIT_REACTION_LIST( REACTION_LIST  )
           IMPLICIT NONE

           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )


           INTEGER :: IREACTION
 
           DO IREACTION = 1, SIZE( REACTION_LIST, 1 )
            REACTION_LIST( IREACTION )%LABEL( 1:2 )  = '>>>>>>>>>>>>>>>>'  ! name of reaction and if needed reference 
            REACTION_LIST( IREACTION )%IRXBITS    = 0           ! bit value for rate constant
            REACTION_LIST( IREACTION )%RATE_TYPE  = 0           ! type of rate constant
            REACTION_LIST( IREACTION )%NPRDCT     = 0           ! no. of products
            REACTION_LIST( IREACTION )%NREACT     = 0           ! no. of reactants
            REACTION_LIST( IREACTION )%ORDER      = 0           ! order of reaction
            REACTION_LIST( IREACTION )%IRR( 1:MAXPRODS+3 )    = 0   ! reactant and product species indices
            REACTION_LIST( IREACTION )%HETEO_INDEX( 1:2 )     = 0   ! mechanism reaction indices if heterogeneous type
            REACTION_LIST( IREACTION )%PHOTO_INDEX( 1:3 )     = 0   ! mechanism reaction indices if rate constant photolysis
            REACTION_LIST( IREACTION )%FALLOFF_INDEX          = 0   ! mechanism reaction indices if falloff type
            REACTION_LIST( IREACTION )%SPECIAL_INDEX( 1:2 )   = 0   ! mechanism reaction indices if rate constant a special expression
            REACTION_LIST( IREACTION )%SC( 1:MAXPRODS ) = 0.0D0   ! product stiochometric coefficients
            REACTION_LIST( IREACTION )%RTDAT( 1:3 )     = 0.0D0   ! general data for rate constant
            REACTION_LIST( IREACTION )%RFDAT( 1:5 )     = 0.0D0   ! data of fall rate constant type
            REACTION_LIST( IREACTION )%NAIR_RCTNTS    = 0       ! # times M or air a reactant
            REACTION_LIST( IREACTION )%NH2O_RCTNTS    = 0       ! # times water a reactant
            REACTION_LIST( IREACTION )%N_O2_RCTNTS    = 0       ! # times O2 a reactant
            REACTION_LIST( IREACTION )%N_N2_RCTNTS    = 0       ! # times H2 a reactant
            REACTION_LIST( IREACTION )%N_H2_RCTNTS    = 0       ! # times N2 a reactant
            REACTION_LIST( IREACTION )%NCH4_RCTNTS    = 0       ! # times methane a reactant
            REACTION_LIST( IREACTION )%SC_NET( : )    = 0.0D0   ! net stiochometric coefficients
            REACTION_LIST( IREACTION )%NET_SPECIES    = 0       ! no. of transform species
            REACTION_LIST( IREACTION )%PURE_NREACT    = 0       ! no. of reactant that are not also products
            REACTION_LIST( IREACTION )%IRR_NET( : )   = 0       ! net reactant and product species indices
           END DO
           
         END SUBROUTINE INIT_REACTION_LIST
         SUBROUTINE SORT_REACTION_LIST( OFFSET ,NREACTIONS, REACTION_LIST )
! routine sorts the reactant based on the lowest number of reactants
             IMPLICIT NONE
             INTEGER,           INTENT( IN    ) :: OFFSET             ! in master list, #reactions before REACTION_LIST 
             INTEGER,           INTENT( IN    ) :: NREACTIONS         ! number of reactions in list
             TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : ) ! data for individual reactions
!            INTEGER,           INTENT( INOUT ) :: INEW_BUBBLE  ( : )
           
             TYPE( REACTION ) :: SWAPVALUE

             INTEGER          :: I, J
             INTEGER          :: INEW, JNEW           ! Index for sorted species number
             INTEGER          :: IOLD, JOLD           ! Index for old species number
             INTEGER          :: IMINNEW              ! Index holder for sort routine
             INTEGER          :: IMINOLD              ! Index holder for sort routine
             INTEGER          :: MINVALU              ! Current number of PD terms in sort
             LOGICAL          :: SWAPPED
             TYPE( REACTION ), ALLOCATABLE :: SWAPZERO( : )
             
             INTEGER, ALLOCATABLE :: INEW_BUBBLE  ( : )
             INTEGER, ALLOCATABLE :: IMID_BUBBLE  ( : )

             ALLOCATE( IMID_BUBBLE ( NREACTIONS ), INEW_BUBBLE ( NREACTIONS ) )
             IMID_BUBBLE = (/ (I, I = 1, NREACTIONS) /)
                 

             DO JNEW = 1, NREACTIONS
                JOLD    = IMID_BUBBLE( JNEW )
                      MINVALU = REACTION_LIST( JOLD )%NREACT
                      IMINOLD = JOLD
                      IMINNEW = JNEW

                      DO INEW = JNEW + 1, NREACTIONS
                         IOLD = IMID_BUBBLE( INEW )
                         IF ( REACTION_LIST( IOLD )%NREACT .LT. MINVALU ) THEN
                            MINVALU = REACTION_LIST( IOLD )%NREACT
                            IMINOLD = IOLD
                            IMINNEW = INEW
                         END IF
                      END DO

                      IMID_BUBBLE( IMINNEW ) = JOLD
                      IMID_BUBBLE( JNEW )    = IMINOLD
                      INEW_BUBBLE( JOLD )    = IMINNEW
                      INEW_BUBBLE( IMINOLD ) = JNEW
             END DO

             ALLOCATE( SWAPZERO ( NREACTIONS ) )
             SWAPZERO = REACTION_LIST
             DO J = 1, NREACTIONS
                I = IMID_BUBBLE( J ) 
                REACTION_LIST( J ) = SWAPZERO( I )
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT,
     &          OFFSET+IMID_BUBBLE( J )
             END DO


!            WRITE(6,'(A)')'Results from sorting REACTION_LIST by number of reactants '
             WRITE(6,99815)
             DO J = 1, NREACTIONS
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT,
     &          OFFSET+IMID_BUBBLE( J )
             END DO



             DEALLOCATE( INEW_BUBBLE )
99815        FORMAT("Results from sorting REACTION_LIST by number of reactants"
     &              / "INDEX",6X,"LABEL",7X,"NREACT",1X,"OLD INDEX")
99816        FORMAT(1X,I4,1X,A16,3X,I1,3X,I4)
         END SUBROUTINE SORT_REACTION_LIST 
         SUBROUTINE REV_SORT_REACTION_LIST( OFFSET ,NREACTIONS, REACTION_LIST )
! routine sorts the reactant based on the highest number of reactants
             IMPLICIT NONE
             INTEGER,           INTENT( IN    ) :: OFFSET             ! in master list, #reactions before REACTION_LIST 
             INTEGER,           INTENT( IN    ) :: NREACTIONS         ! number of reactions in list
             TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : ) ! data for individual reactions
!            INTEGER,           INTENT( INOUT ) :: INEW_BUBBLE  ( : )
           
             TYPE( REACTION ) :: SWAPVALUE
             INTEGER          :: I, J
             INTEGER          :: INEW, JNEW           ! Index for sorted species number
             INTEGER          :: IOLD, JOLD           ! Index for old species number
             INTEGER          :: IMINNEW              ! Index holder for sort routine
             INTEGER          :: IMINOLD              ! Index holder for sort routine
             INTEGER          :: MINVALU              ! Current number of PD terms in sort
             LOGICAL          :: SWAPPED
             TYPE( REACTION ), ALLOCATABLE :: SWAPZERO( : )
             
             INTEGER, ALLOCATABLE :: INEW_BUBBLE  ( : )
             INTEGER, ALLOCATABLE :: IMID_BUBBLE  ( : )

             ALLOCATE( IMID_BUBBLE ( NREACTIONS ), INEW_BUBBLE ( NREACTIONS ) )
             IMID_BUBBLE = (/ (I, I = 1, NREACTIONS) /)
                 
             DO JNEW = 1, NREACTIONS
                JOLD    = IMID_BUBBLE( JNEW )
                MINVALU = REACTION_LIST( JOLD )%NREACT
                IMINOLD = JOLD
                IMINNEW = JNEW

                DO INEW = JNEW + 1, NREACTIONS
                   IOLD = IMID_BUBBLE( INEW )
                   IF ( REACTION_LIST( IOLD )%NREACT .LT. MINVALU ) THEN
                      MINVALU = REACTION_LIST( IOLD )%NREACT
                      IMINOLD = IOLD
                      IMINNEW = INEW
                   END IF
                END DO

                IMID_BUBBLE( IMINNEW ) = JOLD
                IMID_BUBBLE( JNEW )    = IMINOLD
                INEW_BUBBLE( JOLD )    = IMINNEW
                INEW_BUBBLE( IMINOLD ) = JNEW
             END DO

             ALLOCATE( SWAPZERO ( NREACTIONS ) )
             SWAPZERO = REACTION_LIST
             DO J = 1, NREACTIONS
                I = IMID_BUBBLE( J ) 
                REACTION_LIST( J ) = SWAPZERO( I )
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT,
     &          OFFSET+IMID_BUBBLE( J )
             END DO
             
             SWAPZERO = REACTION_LIST
             DO J =  1, NREACTIONS
                I = NREACTIONS - J + 1
                REACTION_LIST( J ) = SWAPZERO( I )
                INEW_BUBBLE( J )   = IMID_BUBBLE( I )
             END DO 

!            WRITE(6,'(A)')'Results from sorting REACTION_LIST by number of reactants '
             WRITE(6,99815)
             DO J = 1, NREACTIONS
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT,
     &          OFFSET+INEW_BUBBLE( J )
             END DO
             
             DEALLOCATE( SWAPZERO, IMID_BUBBLE, INEW_BUBBLE )
99815        FORMAT("Results from reverse sorting REACTION_LIST by number of reactants"
     &              / "INDEX",6X,"LABEL",7X,"NREACT",1X,"OLD INDEX")
99816        FORMAT(1X,I4,1X,A16,3X,I1,3X,I4)

         END SUBROUTINE REV_SORT_REACTION_LIST 
         SUBROUTINE PUT_ZEROS_BELOW( OFFSET ,NREACTIONS, REACTION_LIST )
! routine sorts the reactant based on the highest number of reactants
             IMPLICIT NONE
             INTEGER,           INTENT( IN    ) :: OFFSET             ! in master list, #reactions before REACTION_LIST 
             INTEGER,           INTENT( IN    ) :: NREACTIONS         ! number of reactions in list
             TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : ) ! data for individual reactions
!            INTEGER,           INTENT( INOUT ) :: INEW_BUBBLE  ( : )
           
             TYPE( REACTION ) :: SWAPVALUE
             INTEGER          :: I, J
             INTEGER          :: INEW, NZERO
             LOGICAL          :: SWAPPED
             TYPE( REACTION ), ALLOCATABLE :: SWAPZERO( : )
             
             NZERO = 0 
             DO I = 1, NREACTIONS
                 IF ( REACTION_LIST( I )%NREACT .EQ. 0 ) NZERO = NZERO + 1
             END DO
! put reaction with no reactants last
             IF( NZERO .LT. 1 ) RETURN

             ALLOCATE( SWAPZERO ( NREACTIONS ) )
             SWAPZERO = REACTION_LIST
             REACTION_LIST( 1:(NREACTIONS-NZERO) ) = SWAPZERO( (NREACTIONS-NZERO+1):NREACTIONS )
             REACTION_LIST( (NREACTIONS-NZERO+1):NREACTIONS ) = SWAPZERO( 1:NZERO )
             DEALLOCATE( SWAPZERO )

!            WRITE(6,'(A)')'Results from putting zero order reactions in REACTION_LIST below '
             WRITE(6,99815)
             DO J = 1, NREACTIONS
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT
             END DO

99815        FORMAT("Results from reverse sorting REACTION_LIST by number of reactants"
     &              / "INDEX",6X,"LABEL",7X,"NREACT",1X,"OLD INDEX")
99816        FORMAT(1X,I4,1X,A16,3X,I1,3X,I4)
         END SUBROUTINE PUT_ZEROS_BELOW 
         SUBROUTINE PUT_ZEROS_ABOVE( NREACTIONS, REACTION_LIST )
! routine sorts the reactant based on the highest number of reactants
             IMPLICIT NONE
             INTEGER,           INTENT( IN    ) :: NREACTIONS         ! number of reactions in list
             TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : ) ! data for individual reactions
!            INTEGER,           INTENT( INOUT ) :: INEW_BUBBLE  ( : )
           
             TYPE( REACTION ) :: SWAPVALUE
             INTEGER          :: I, J
             INTEGER          :: INEW, NZERO
             LOGICAL          :: SWAPPED
             TYPE( REACTION ), ALLOCATABLE :: SWAPZERO( : )
             
             NZERO = 0 
             DO I = 1, NREACTIONS
                 IF ( REACTION_LIST( I )%NREACT .EQ. 0 ) NZERO = NZERO + 1
             END DO
! put reaction with no reactants last
             IF( NZERO .LT. 1 ) RETURN

             ALLOCATE( SWAPZERO ( NREACTIONS ) )
             SWAPZERO = REACTION_LIST
             REACTION_LIST( 1:NZERO ) = SWAPZERO( (NREACTIONS-NZERO+1):NREACTIONS )
             REACTION_LIST( (NREACTIONS-NZERO+1):NREACTIONS ) = SWAPZERO( 1:(NREACTIONS-NZERO) )
             DEALLOCATE( SWAPZERO )

!            WRITE(6,'(A)')'Results from putting zero order reactions in REACTION_LIST below '
             WRITE(6,99815)
             DO J = 1, NREACTIONS
                WRITE(6,99816)J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT
             END DO

99815        FORMAT("Results from reverse sorting REACTION_LIST by number of reactants"
     &              / "INDEX",6X,"LABEL",7X,"NREACT",1X,"OLD INDEX")
99816        FORMAT(1X,I4,1X,A16,3X,I1,3X,I4)
         END SUBROUTINE PUT_ZEROS_ABOVE 
         SUBROUTINE LOAD_REACTION_LIST( IREACTION, JREACTION, LABELS, REACTION_LIST  )
           IMPLICIT NONE
           INTEGER, INTENT( IN )              :: IREACTION
           INTEGER, INTENT( IN )              :: JREACTION
           CHARACTER(LEN=16), INTENT( IN )    :: LABELS( :,: )
           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )

           INTEGER   :: I, J, K, L
           REAL( 8 ) :: COEFF
           LOGICAL   :: TRUE_REACTANT

           IF( SIZE( REACTION_LIST, 1 ) .LT. IREACTION )THEN
               WRITE( 6, * )'In LOAD_REACTION_LIST: array index exceeds REACTION_LIST size'
               WRITE( 6, '(A,I4)' )'IREACTION = ',IREACTION
               WRITE( 6, '(A,I4)' )'REACTION_LIST size = ',SIZE( REACTION_LIST, 1 )
               STOP
           END IF
! set an element if a reaction list
              REACTION_LIST( IREACTION )%LABEL(1)   = LABELS( JREACTION,1 )
              REACTION_LIST( IREACTION )%LABEL(2)   = LABELS( JREACTION,2 )
              REACTION_LIST( IREACTION )%IRXBITS    = IRXBITS( JREACTION )
              REACTION_LIST( IREACTION )%RATE_TYPE  = KTYPE( JREACTION )
              REACTION_LIST( IREACTION )%NPRDCT     = NPRDCT( JREACTION )
              REACTION_LIST( IREACTION )%NREACT     = NREACT( JREACTION )
              REACTION_LIST( IREACTION )%ORDER      = IORDER( JREACTION )
              REACTION_LIST( IREACTION )%RTDAT(1:3) = RTDAT( 1:3,JREACTION )        
!              WRITE(6,'(A,7(ES12.4,1X))')'REACTION_LIST( IREACTION )%RTDAT(1:3),RTDAT( 1:3,JREACTION )',
!     &          REACTION_LIST( IREACTION )%RTDAT(1:3), RTDAT( 1:3,JREACTION )
              REACTION_LIST( IREACTION )%IRR(1:MAXPRODS+3) = IRR( JREACTION,1:MAXPRODS+3 )        
              REACTION_LIST( IREACTION )%SC(1:MAXPRODS)    = SC( JREACTION, 1:MAXPRODS )
              IF( KTYPE( JREACTION ) .EQ. 13 )THEN
                 REACTION_LIST( IREACTION )%RATE_STRING  = RATE_STRING( NRATE_STRING )
                 REACTION_LIST( IREACTION )%STRING_INDEX = IREACTION
           WRITE(6,'(A,3(1X,I4),2(1X,A))')'JREACTION, RATE_STRING( JREACTION ), REACTION_LIST( IREACTION )%RATE_STRING = ', 
     &           JREACTION,KSTRING( NRATE_STRING ),REACTION_LIST( IREACTION )%STRING_INDEX, TRIM(RATE_STRING( JREACTION )),
     &             TRIM( REACTION_LIST( IREACTION )%RATE_STRING)
              END IF

              DO I = 1, NREACT( JREACTION )
                 J = IRR( JREACTION,  I )
                 COEFF = -1.0D0
                 TRUE_REACTANT = .TRUE.
                 DO K = 1, NPRDCT( JREACTION )
                    L = IRR( JREACTION,  K+3 )
                    IF( J .EQ. L )THEN
                        COEFF = COEFF + SC( JREACTION, K )
                        TRUE_REACTANT = .FALSE.
                    END IF
                 END DO
                 IF( ABS( COEFF ) .GT. 1.0D-6 )THEN
                     REACTION_LIST( IREACTION )%NET_SPECIES = REACTION_LIST( IREACTION )%NET_SPECIES  + 1
                     K = REACTION_LIST( IREACTION )%NET_SPECIES
                     REACTION_LIST( IREACTION )%SC_NET( K ) = COEFF
                     REACTION_LIST( IREACTION )%IRR_NET( K ) = J  
                 END IF
                 IF( TRUE_REACTANT )THEN
                     REACTION_LIST( IREACTION )%PURE_NREACT = REACTION_LIST( IREACTION )%PURE_NREACT
     &                                                      + 1
                 END IF
              END DO

              LOOP_PRODUCT: DO I = 1, NPRDCT( JREACTION )
                 J = IRR( JREACTION, I+3 )
                 LOOP_REACTANT: DO K = 1, NREACT( JREACTION ) 
                    L = IRR( JREACTION, K )
                    IF( L .EQ. J )CYCLE LOOP_PRODUCT
                 END DO LOOP_REACTANT
                 REACTION_LIST( IREACTION )%NET_SPECIES = REACTION_LIST( IREACTION )%NET_SPECIES + 1
                 K = REACTION_LIST( IREACTION )%NET_SPECIES
                 REACTION_LIST( IREACTION )%SC_NET( K ) = SC( JREACTION, I )
                 REACTION_LIST( IREACTION )%IRR_NET( K ) = J
              END DO LOOP_PRODUCT  
                 
                 

              IF( KTYPE( JREACTION ) .EQ. 11 )THEN
                 REACTION_LIST( IREACTION )%SPECIAL_INDEX(1) = IREACTION
                 REACTION_LIST( IREACTION )%SPECIAL_INDEX(2) = ISPECIAL( NSPECIAL_RXN,2 )
              END IF
              IF( KTYPE( JREACTION ) .GT. 7 .AND. KTYPE( JREACTION ) .LE. 10 )THEN
                 REACTION_LIST( IREACTION )%FALLOFF_INDEX = IREACTION
                 REACTION_LIST( IREACTION )%RFDAT(1:5)    = RFDAT( 1:5,NFALLOFF )        
              END IF
              IF( KTYPE( JREACTION ) .EQ. 12 )THEN
                 REACTION_LIST( IREACTION )%FALLOFF_INDEX = IREACTION
                 REACTION_LIST( IREACTION )%RFDAT(1:5)    = RFDAT(1:5,NFALLOFF)        
              END IF
              IF( KTYPE( JREACTION ) .EQ. 0 )THEN
                  REACTION_LIST( IREACTION )%PHOTO_INDEX(1) = IREACTION
                  REACTION_LIST( IREACTION )%PHOTO_INDEX(2) = IPH(NMPHOT,2)
                  REACTION_LIST( IREACTION )%PHOTO_INDEX(3) = IPH(NMPHOT,3)
              END IF
              IF( KTYPE( JREACTION ) .EQ. -1 )THEN
                  REACTION_LIST( IREACTION )%HETEO_INDEX(1) = IREACTION
                  REACTION_LIST( IREACTION )%HETEO_INDEX(2) = IHETERO(MHETERO,2)
              END IF
              DO I = 1, MAX3BODIES
                 IF( NRXWM( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%NAIR_RCTNTS = REACTION_LIST( IREACTION )%NAIR_RCTNTS  + 1
                 END IF
                 IF( NRXWW( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%NH2O_RCTNTS = REACTION_LIST( IREACTION )%NH2O_RCTNTS  + 1
                 END IF
                 IF( NRXWO2( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%N_O2_RCTNTS = REACTION_LIST( IREACTION )%N_O2_RCTNTS  + 1
                 END IF
                 IF( NRXWN2( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%N_N2_RCTNTS = REACTION_LIST( IREACTION )%N_N2_RCTNTS  + 1
                 END IF
                 IF( NRXWH2( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%N_H2_RCTNTS = REACTION_LIST( IREACTION )%N_H2_RCTNTS  + 1
                 END IF
                 IF( NRXWCH4( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%NCH4_RCTNTS = REACTION_LIST( IREACTION )%NCH4_RCTNTS  + 1
                 END IF
              END DO
         END SUBROUTINE LOAD_REACTION_LIST
         SUBROUTINE PUT_PHOTRXNS_ONTOP( LABELS )

            IMPLICIT NONE
            
            CHARACTER(LEN=16), INTENT( INOUT ) :: LABELS( :,: )

            INTEGER :: I, J, K
            INTEGER :: IFALLOFF
            INTEGER :: IPHOT
            INTEGER :: IHET
            INTEGER :: JSPECIAL
            INTEGER :: N_AIR_3BODY
            INTEGER :: N_H2O_3BODY
            INTEGER :: N_N2_3BODY
            INTEGER :: N_O2_3BODY
            INTEGER :: N_CH4_REACTION
            INTEGER :: N_H2_REACTION
            INTEGER :: FIXED_SPC_COUNT
            INTEGER :: STRING_COUNT

            ALLOCATE( NET_SPECIES( NRXNS ), PURE_NREACT( NRXNS ) )       
            ALLOCATE( IRR_NET    ( MAXPRODS+3,NRXNS )  )     
            ALLOCATE( SC_NET     ( MAXPRODS+3,NRXNS )  )        

            IFALLOFF = 0
            IPHOT    = 0
            IHET     = 0
            JSPECIAL = 0
            N_AIR_3BODY    = 0
            N_H2O_3BODY    = 0
            N_N2_3BODY     = 0  
            N_O2_3BODY     = 0
            N_CH4_REACTION = 0
            N_H2_REACTION  = 0
            IRR_NET        = 0
            NET_SPECIES    = 0 
            PURE_NREACT    = 0
            STRING_COUNT   = 0
            SC_NET         = 0.0D0

! reset variable of MECHANISM_DATA
            INDEX_FIXED_SPECIES = 0
            DO I = 1, NSUNLIGHT
               IF( PHOTOLYSIS_REACTIONS( I )%NREACT .EQ. 1 )THEN
                   ONE_REACT_SUNLIGHT = ONE_REACT_SUNLIGHT + 1
               ELSE IF( PHOTOLYSIS_REACTIONS( I )%NREACT .EQ. 0 )THEN
                   ZERO_REACT_SUNLIGHT = ZERO_REACT_SUNLIGHT + 1
               END IF
!redefine first part of total mechanism data                  
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = PHOTOLYSIS_REACTIONS( I )%LABEL(1)                     
               LABELS( I,2 )         = PHOTOLYSIS_REACTIONS( I )%LABEL(2)                     
               IRXBITS( I )          = PHOTOLYSIS_REACTIONS( I )%IRXBITS            
               KTYPE( I )            = PHOTOLYSIS_REACTIONS( I )%RATE_TYPE          
               NPRDCT( I )           = PHOTOLYSIS_REACTIONS( I )%NPRDCT             
               NREACT( I )           = PHOTOLYSIS_REACTIONS( I )%NREACT             
               IORDER( I )           = PHOTOLYSIS_REACTIONS( I )%ORDER             
               RTDAT( 1:3,I )        = PHOTOLYSIS_REACTIONS( I )%RTDAT(1:3)         
               IRR( I,1:MAXPRODS+3 ) = PHOTOLYSIS_REACTIONS( I )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = PHOTOLYSIS_REACTIONS( I )%SC(1:MAXPRODS)     
               NET_SPECIES( I )      = PHOTOLYSIS_REACTIONS( I )%NET_SPECIES
               PURE_NREACT( I )      = PHOTOLYSIS_REACTIONS( I )%PURE_NREACT             
               IRR_NET( :, I )       = PHOTOLYSIS_REACTIONS( I )%IRR_NET( : )
               SC_NET ( :, I )       = PHOTOLYSIS_REACTIONS( I )%SC_NET( : )
              
               IF( KTYPE( I ) .EQ. 13 )THEN
                   STRING_COUNT = STRING_COUNT + 1
                   KSTRING( STRING_COUNT )     = I
                   RATE_STRING( STRING_COUNT ) = PHOTOLYSIS_REACTIONS( I )%RATE_STRING
               END IF

               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11  ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! PHOTOLYSIS_REACTIONS( I )%FALLOFF_INDEX
                 RFDAT( 1:5,IFALLOFF ) = PHOTOLYSIS_REACTIONS( I )%RFDAT(1:5)
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! PHOTOLYSIS_REACTIONS( I )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = PHOTOLYSIS_REACTIONS( I )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = PHOTOLYSIS_REACTIONS( I )%PHOTO_INDEX(3)
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! PHOTOLYSIS_REACTIONS( I )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = PHOTOLYSIS_REACTIONS( I )%SPECIAL_INDEX(2)
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! PHOTOLYSIS_REACTIONS( I )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = PHOTOLYSIS_REACTIONS( I )%HETEO_INDEX(2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
                  print*,'I, N_AIR_3BODY, NRXWM(N_AIR_3BODY) = ',I, N_AIR_3BODY, NRXWM(N_AIR_3BODY)
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
               END DO
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF
            END DO
            IF( NSUNLIGHT .NE. ONE_REACT_SUNLIGHT )THEN
               WRITE( 6,'(a)')"FATAL ERROR"
               WRITE( 6,'(a)')"Below sunlight dependent reactions not have only one reactant"
               DO I = 1, NSUNLIGHT
                  IF( PHOTOLYSIS_REACTIONS( I )%NREACT .EQ. 1 )CYCLE
                  WRITE(6,'(8X,A16)')PHOTOLYSIS_REACTIONS( I )%LABEL( 1 )
               END DO
               STOP
            END IF
!redefine second part of total mechanism data                  
            I = NSUNLIGHT
            DO J = 1, NTHERMAL
               I = I + 1
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = THERMAL_REACTIONS( J )%LABEL(1)                     
               LABELS( I,2 )         = THERMAL_REACTIONS( J )%LABEL(2)                     
               IRXBITS( I )          = THERMAL_REACTIONS( J )%IRXBITS            
               KTYPE( I )            = THERMAL_REACTIONS( J )%RATE_TYPE          
               NPRDCT( I )           = THERMAL_REACTIONS( J )%NPRDCT             
               NREACT( I )           = THERMAL_REACTIONS( J )%NREACT
               NET_SPECIES( I )      = THERMAL_REACTIONS( J )%NET_SPECIES
               PURE_NREACT( I )      = THERMAL_REACTIONS( J )%PURE_NREACT             
               IRR_NET( :, I )       = THERMAL_REACTIONS( J )%IRR_NET( : )
               SC_NET ( :, I )       = THERMAL_REACTIONS( J )%SC_NET( : )
               KSTRING( I )          = THERMAL_REACTIONS( J )%STRING_INDEX
               RATE_STRING( I )      = THERMAL_REACTIONS( J )%RATE_STRING

               SELECT CASE (THERMAL_REACTIONS( J )%NREACT )
                 CASE( 0 )
                   ZERO_REACT_THERMAL = ZERO_REACT_THERMAL + 1
                 CASE( 1 )
                   ONE_REACT_THERMAL  = ONE_REACT_THERMAL + 1
                 CASE( 2 )
                   TWO_REACT_THERMAL  = TWO_REACT_THERMAL + 1
                 CASE( 3 )
                   THREE_REACT_THERMAL = THREE_REACT_THERMAL + 1
               END SELECT
               IORDER( I )           = THERMAL_REACTIONS( J )%ORDER             
               RTDAT( 1:3,I )        = THERMAL_REACTIONS( J )%RTDAT(1:3)         
               IRR( I,1:MAXPRODS+3 ) = THERMAL_REACTIONS( J )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = THERMAL_REACTIONS( J )%SC(1:MAXPRODS)
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11 ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! THERMAL_REACTIONS( J )%FALLOFF_INDEX
                 DO K = 1, 5
                    RFDAT( K,IFALLOFF ) = THERMAL_REACTIONS( J )%RFDAT(K)
                 END DO
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! THERMAL_REACTIONS( J )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = THERMAL_REACTIONS( J )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = THERMAL_REACTIONS( J )%PHOTO_INDEX(3)
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! THERMAL_REACTIONS( J )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = THERMAL_REACTIONS( J )%SPECIAL_INDEX(2)
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! THERMAL_REACTIONS( J )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = THERMAL_REACTIONS( J )%HETEO_INDEX(2)
               END IF

               IF( KTYPE( I ) .EQ. 13 )THEN
                   STRING_COUNT = STRING_COUNT + 1
                   KSTRING( STRING_COUNT )     = I
                   RATE_STRING( STRING_COUNT ) = THERMAL_REACTIONS( J )%RATE_STRING
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, THERMAL_REACTIONS( J )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
         WRITE(6,'(A,4(I4,1X))')'I, N_AIR_3BODY, NRXWM(N_AIR_3BODY) = ',J,I, N_AIR_3BODY, NRXWM(N_AIR_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_H2O_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
         WRITE(6,'(A,4(I4,1X))')'J, I, N_H2O_3BODY, NRXWW(N_H2O_3BODY) = ',J, I, N_H2O_3BODY, NRXWW(N_H2O_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               WRITE(6,'(A,4(I4,1X))')'I, N_O2_3BODY, NRXWO2(N_O2_3BODY) = ',J,I, N_O2_3BODY, NRXWO2(N_O2_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               WRITE(6,'(A,4(I4,1X))')'I, N_N2_3BODY, NRXWN2(N_N2_3BODY) = ',J,I, N_N2_3BODY, NRXWN2(N_N2_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               WRITE(6,'(A,4(I4,1X))')'I, N_H2_REACTION, NRXWH2(N_H2_REACTION) = ',J,I, N_H2_REACTION, NRXWH2(N_H2_REACTION)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWCH4(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
           WRITE(6,'(A,4(I4,1X))')'I, N_CH4_REACTION, NRXWCH4(N_CH4_REACTION) = ',J,I, N_CH4_REACTION, NRXWCH4(N_CH4_REACTION)
               END DO
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF

               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"THERMAL: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )               
            END DO

            IF( STRING_COUNT .NE. NRATE_STRING )THEN
               WRITE(6, * )'A reaction of KTYPE equal to 13 was dropped'
               STOP
            END IF

! rest number of third body, CH4 and H2 reactions
            NWM   = N_AIR_3BODY
            NWW   = N_H2O_3BODY
            NWO2  = N_O2_3BODY
            NWN2  = N_N2_3BODY
            NWH2  = N_H2_REACTION
            NWCH4 = N_CH4_REACTION
!set total count of reactant per reaction
            ONE_REACT_REACTIONS   = ONE_REACT_SUNLIGHT + ONE_REACT_THERMAL
            ZERO_REACT_REACTIONS  = ZERO_REACT_THERMAL 
            TWO_REACT_REACTIONS   = TWO_REACT_THERMAL 
            THREE_REACT_REACTIONS = THREE_REACT_THERMAL 
! update labels and calculate the number of partial derivative in the mechanism
            NPDERIV = 0
            DO J = 1, (NMPHOT + NTHERMAL)
               RXLABEL( J ) = LABELS( J,1 )
               NPDERIV = NPDERIV + NREACT( J )
               WRITE(6,'(A,I4,1X,3(A16,1X),I2)')"MECHANISM: J, LABELS(J,1:2),RXLABEL( J ), KTYPE( J ) = ", J,
     &         LABELS( J,1:2), RXLABEL( J ), KTYPE( J )
            END DO
         END SUBROUTINE PUT_PHOTRXNS_ONTOP
         SUBROUTINE PLACE_PHOTRXNS( LABELS )

            IMPLICIT NONE
            
            CHARACTER(LEN=16), INTENT( INOUT ) :: LABELS( :,: )

            INTEGER :: I, J, K
            INTEGER :: IFALLOFF
            INTEGER :: IPHOT
            INTEGER :: IHET
            INTEGER :: JSPECIAL
            INTEGER :: N_AIR_3BODY
            INTEGER :: N_H2O_3BODY
            INTEGER :: N_N2_3BODY
            INTEGER :: N_O2_3BODY
            INTEGER :: N_CH4_REACTION
            INTEGER :: N_H2_REACTION
            INTEGER :: FIXED_SPC_COUNT

            IFALLOFF = 0
            IPHOT    = 0
            IHET     = 0
            JSPECIAL = 0
            N_AIR_3BODY    = 0
            N_H2O_3BODY    = 0
            N_N2_3BODY     = 0  
            N_O2_3BODY     = 0
            N_CH4_REACTION = 0
            N_H2_REACTION  = 0
! reset varaible ofMECHANISM _DATA
            INDEX_FIXED_SPECIES = 0
            IF( SUN_BELOW )THEN
                I = NTHERMAL
            ELSE
                I = 0
            END IF
            DO J = 1, NSUNLIGHT
               I = I + 1
               IF( PHOTOLYSIS_REACTIONS( J )%NREACT .EQ. 1 )THEN
                   ONE_REACT_SUNLIGHT = ONE_REACT_SUNLIGHT + 1
               ELSE IF( PHOTOLYSIS_REACTIONS( J )%NREACT .EQ. 0 )THEN
                   ZERO_REACT_SUNLIGHT = ZERO_REACT_SUNLIGHT + 1
               END IF
!redefine first part of total mechanism data                  
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = PHOTOLYSIS_REACTIONS( J )%LABEL(1)                     
               LABELS( I,2 )         = PHOTOLYSIS_REACTIONS( J )%LABEL(2)                     
               IRXBITS( I )          = PHOTOLYSIS_REACTIONS( J )%IRXBITS            
               KTYPE( I )            = PHOTOLYSIS_REACTIONS( J )%RATE_TYPE          
               NPRDCT( I )           = PHOTOLYSIS_REACTIONS( J )%NPRDCT             
               NREACT( I )           = PHOTOLYSIS_REACTIONS( J )%NREACT             
               IORDER( I )           = PHOTOLYSIS_REACTIONS( J )%ORDER             
               RTDAT( 1:3,I )        = PHOTOLYSIS_REACTIONS( J )%RTDAT(1:3)         
               IRR( I,1:MAXPRODS+3 ) = PHOTOLYSIS_REACTIONS( J )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = PHOTOLYSIS_REACTIONS( J )%SC(1:MAXPRODS)     
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11  ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! PHOTOLYSIS_REACTIONS( J )%FALLOFF_INDEX
                 RFDAT( 1:5,IFALLOFF ) = PHOTOLYSIS_REACTIONS( J )%RFDAT(1:5)
               WRITE(6,'(A,I4,1X,(A16,1X),(I2,1X),5(ES12.4,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), IRRFALL = ", I, 
     &         LABELS( I, 1 ),IRRFALL( IFALLOFF ),RFDAT( 1:5,IFALLOFF )
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! PHOTOLYSIS_REACTIONS( J )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = PHOTOLYSIS_REACTIONS( J )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = PHOTOLYSIS_REACTIONS( J )%PHOTO_INDEX(3)
               WRITE(6,'(A,I4,1X,(A16,1X),(I3,1X),A16,1X,I3)')"PHOTOLYSIS: I, LABELS( I, 1 ), IPH = ", I, 
     &         LABELS( I, 1 ),IPH( IPHOT,1 ),PHOTAB(IPH( IPHOT,2 )),IPH( IPHOT,3 )
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! PHOTOLYSIS_REACTIONS( J )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = PHOTOLYSIS_REACTIONS( J )%SPECIAL_INDEX(2)
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! PHOTOLYSIS_REACTIONS( J )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = PHOTOLYSIS_REACTIONS( J )%HETEO_INDEX(2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, PHOTOLYSIS_REACTIONS( J )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( J )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( J )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( J )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( J )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( J )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
               END DO
               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF
            END DO
            IF( NSUNLIGHT .NE. ONE_REACT_SUNLIGHT )THEN
               WRITE( 6,'(a)')"FATAL ERROR"
               WRITE( 6,'(a)')"Below sunlight dependent reactions not have only one reactant"
               DO I = 1, NSUNLIGHT
                  IF( PHOTOLYSIS_REACTIONS( K )%NREACT .EQ. 1 )CYCLE
                  WRITE(6,'(8X,A16)')PHOTOLYSIS_REACTIONS( K )%LABEL( 1 )
               END DO
               STOP
            END IF
!redefine second part of total mechanism data
            IF( SUN_BELOW )THEN
                I = 0
            ELSE
                I = NSUNLIGHT
            END IF
            DO J = 1, NTHERMAL
               I = I + 1
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = THERMAL_REACTIONS( J )%LABEL(1)                     
               LABELS( I,2 )         = THERMAL_REACTIONS( J )%LABEL(2)                     
               IRXBITS( I )          = THERMAL_REACTIONS( J )%IRXBITS            
               KTYPE( I )            = THERMAL_REACTIONS( J )%RATE_TYPE          
               NPRDCT( I )           = THERMAL_REACTIONS( J )%NPRDCT             
               NREACT( I )           = THERMAL_REACTIONS( J )%NREACT
               SELECT CASE (THERMAL_REACTIONS( J )%NREACT )
                 CASE( 0 )
                   ZERO_REACT_THERMAL = ZERO_REACT_THERMAL + 1
                 CASE( 1 )
                   ONE_REACT_THERMAL  = ONE_REACT_THERMAL + 1
                 CASE( 2 )
                   TWO_REACT_THERMAL  = TWO_REACT_THERMAL + 1
                 CASE( 3 )
                   THREE_REACT_THERMAL = THREE_REACT_THERMAL + 1
               END SELECT
               IORDER( I )           = THERMAL_REACTIONS( J )%ORDER             
               RTDAT( 1:3,I )        = THERMAL_REACTIONS( J )%RTDAT(1:3)         
!              WRITE(6,'(A,7(ES12.4,1X))')'THERMAL_REACTIONS( J )%RTDAT(1:3),RTDAT( 1:3,I )',
!     &          THERMAL_REACTIONS( J )%RTDAT(1:3), RTDAT( 1:3,I)
               IRR( I,1:MAXPRODS+3 ) = THERMAL_REACTIONS( J )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = THERMAL_REACTIONS( J )%SC(1:MAXPRODS)
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11 ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! THERMAL_REACTIONS( J )%FALLOFF_INDEX
                 DO K = 1, 5
                    RFDAT( K,IFALLOFF ) = THERMAL_REACTIONS( J )%RFDAT(K)
                 END DO
              WRITE(6,'(2(I4,1x),2A,7(ES12.4,1X))')IFALLOFF, I, 'FALLOFF RXN:' // TRIM(LABELS( I,1 )), 
     &           ': THERMAL_REACTIONS( J )%RFDAT(1:3),RFDAT( 1:5,I )',
     &          THERMAL_REACTIONS( J )%RFDAT(1:5), RFDAT( 1:5,IFALLOFF)
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! THERMAL_REACTIONS( J )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = THERMAL_REACTIONS( J )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = THERMAL_REACTIONS( J )%PHOTO_INDEX(3)
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! THERMAL_REACTIONS( J )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = THERMAL_REACTIONS( J )%SPECIAL_INDEX(2)
               WRITE(6,'(A,I4,1X,(A16,1X),2(I4,1X))')"THERMAL: I, LABELS( I, 1 ), ISPECIAL = ", I, 
     &         LABELS( I, 1 ),ISPECIAL( JSPECIAL,1 ),ISPECIAL( JSPECIAL,2 )
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! THERMAL_REACTIONS( J )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = THERMAL_REACTIONS( J )%HETEO_INDEX(2)
               WRITE(6,'(A,I4,1X,(A16,1X),2(I4,1X))')"THERMAL: I, LABELS( I, 1 ), I = ", I, 
     &         LABELS( I, 1 ),IHETERO(IHET,1),IHETERO(IHET,2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, THERMAL_REACTIONS( J )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
         WRITE(6,'(A,4(I4,1X))')'I, N_AIR_3BODY, NRXWM(N_AIR_3BODY) = ',J,I, N_AIR_3BODY, NRXWM(N_AIR_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_H2O_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
         WRITE(6,'(A,4(I4,1X))')'J, I, N_H2O_3BODY, NRXWW(N_H2O_3BODY) = ',J, I, N_H2O_3BODY, NRXWW(N_H2O_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               WRITE(6,'(A,4(I4,1X))')'I, N_O2_3BODY, NRXWO2(N_O2_3BODY) = ',J,I, N_O2_3BODY, NRXWO2(N_O2_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               WRITE(6,'(A,4(I4,1X))')'I, N_N2_3BODY, NRXWN2(N_N2_3BODY) = ',J,I, N_N2_3BODY, NRXWN2(N_N2_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               WRITE(6,'(A,4(I4,1X))')'I, N_H2_REACTION, NRXWH2(N_H2_REACTION) = ',J,I, N_H2_REACTION, NRXWH2(N_H2_REACTION)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWCH4(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
           WRITE(6,'(A,4(I4,1X))')'I, N_CH4_REACTION, NRXWCH4(N_CH4_REACTION) = ',J,I, N_CH4_REACTION, NRXWCH4(N_CH4_REACTION)
               END DO
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF

               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"THERMAL: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )               
            END DO
! rest number of third body, CH4 and H2 reactions
            NWM   = N_AIR_3BODY
            NWW   = N_H2O_3BODY
            NWO2  = N_O2_3BODY
            NWN2  = N_N2_3BODY
            NWH2  = N_H2_REACTION
            NWCH4 = N_CH4_REACTION
!set total count of reactant per reaction
            ONE_REACT_REACTIONS   = ONE_REACT_SUNLIGHT + ONE_REACT_THERMAL
            ZERO_REACT_REACTIONS  = ZERO_REACT_THERMAL 
            TWO_REACT_REACTIONS   = TWO_REACT_THERMAL 
            THREE_REACT_REACTIONS = THREE_REACT_THERMAL 
! update labels and calculate the number of partial derivative in the mechanism
            NPDERIV = 0
            DO J = 1, (NMPHOT + NTHERMAL)
               RXLABEL( J ) = LABELS( J,1 )
               NPDERIV = NPDERIV + NREACT( J )
               WRITE(6,'(A,I4,1X,3(A16,1X),I2)')"MECHANISM: J, LABELS(J,1:2),RXLABEL( J ), KTYPE( J ) = ", J,
     &         LABELS( J,1:2), RXLABEL( J ), KTYPE( J )
            END DO
         END SUBROUTINE PLACE_PHOTRXNS
         SUBROUTINE REPLACE_REACTIONS( REACTION_LIST, NREACTIONS, ISTART, LABELS )

            IMPLICIT NONE
            
           INTEGER, INTENT( IN )              :: ISTART
           INTEGER, INTENT( IN )              :: NREACTIONS
           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )
           CHARACTER(LEN=16), INTENT( INOUT ) :: LABELS( :,: )

            INTEGER       :: I, J, K

            INTEGER, SAVE :: IFALLOFF
            INTEGER, SAVE :: IPHOT
            INTEGER, SAVE :: IHET
            INTEGER, SAVE :: JSPECIAL
            INTEGER, SAVE :: N_AIR_3BODY
            INTEGER, SAVE :: N_H2O_3BODY
            INTEGER, SAVE :: N_N2_3BODY
            INTEGER, SAVE :: N_O2_3BODY
            INTEGER, SAVE :: N_CH4_REACTION
            INTEGER, SAVE :: N_H2_REACTION
            INTEGER, SAVE :: FIXED_SPC_COUNT
            LOGICAL, SAVE :: FIRST_CALL = .TRUE.
            
            IF( FIRST_CALL )THEN
              IFALLOFF = 0
              IPHOT    = 0
              IHET     = 0
              JSPECIAL = 0
              N_AIR_3BODY    = 0
              N_H2O_3BODY    = 0
              N_N2_3BODY     = 0  
              N_O2_3BODY     = 0
              N_CH4_REACTION = 0
              N_H2_REACTION  = 0
! reset varaible of MECHANISM_DATA
              INDEX_FIXED_SPECIES = 0
            END IF
            
            DO I = 1, NREACTIONS   
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = REACTION_LIST( I )%LABEL(1)                     
               LABELS( I,2 )         = REACTION_LIST( I )%LABEL(2)                     
               IRXBITS( I )          = REACTION_LIST( I )%IRXBITS            
               KTYPE( I )            = REACTION_LIST( I )%RATE_TYPE          
               NPRDCT( I )           = REACTION_LIST( I )%NPRDCT             
               NREACT( I )           = REACTION_LIST( I )%NREACT             
               IF( KTYPE( I ) .NE. 0 .OR. KTYPE( I ) .NE. 12 )THEN
                  SELECT CASE ( NREACT( I ) )
                    CASE( 0 )
                       ZERO_REACT_THERMAL = ZERO_REACT_THERMAL + 1
                    CASE( 1 )
                       ONE_REACT_THERMAL  = ONE_REACT_THERMAL + 1
                    CASE( 2 )
                       TWO_REACT_THERMAL  = TWO_REACT_THERMAL + 1
                    CASE( 3 )
                       THREE_REACT_THERMAL = THREE_REACT_THERMAL + 1
                   END SELECT
               END IF
               IORDER( I )           = REACTION_LIST( I )%ORDER             
               RTDAT( 1:3,I )        = REACTION_LIST( I )%RTDAT(1:3)         
               IRR( I,1:MAXPRODS+3 ) = REACTION_LIST( I )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = REACTION_LIST( I )%SC(1:MAXPRODS)     
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11  ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! REACTION_LIST( I )%FALLOFF_INDEX
                 RFDAT( 1:5,IFALLOFF ) = REACTION_LIST( I )%RFDAT(1:5)
               WRITE(6,'(A,I4,1X,(A16,1X),(I2,1X),5(ES12.4,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), IRRFALL = ", I, 
     &         LABELS( I, 1 ),IRRFALL( IFALLOFF ),RFDAT( 1:5,IFALLOFF )
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! REACTION_LIST( I )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = REACTION_LIST( I )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = REACTION_LIST( I )%PHOTO_INDEX(3)
               WRITE(6,'(A,I4,1X,(A16,1X),(I3,1X),A16,1X,I3)')"PHOTOLYSIS: I, LABELS( I, 1 ), IPH = ", I, 
     &         LABELS( I, 1 ),IPH( IPHOT,1 ),PHOTAB(IPH( IPHOT,2 )),IPH( IPHOT,3 )
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! REACTION_LIST( I )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = REACTION_LIST( I )%SPECIAL_INDEX(2)
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! REACTION_LIST( I )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = REACTION_LIST( I )%HETEO_INDEX(2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, REACTION_LIST( I )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
               END DO
               DO K = 1, REACTION_LIST( I )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
               END DO
               DO K = 1, REACTION_LIST( I )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               END DO
               DO K = 1, REACTION_LIST( I )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               END DO
               DO K = 1, REACTION_LIST( I )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               END DO
               DO K = 1, REACTION_LIST( I )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
               END DO
               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF
            END DO         
! rest number of third body, CH4 and H2 reactions
            NWM   = N_AIR_3BODY
            NWW   = N_H2O_3BODY
            NWO2  = N_O2_3BODY
            NWN2  = N_N2_3BODY
            NWH2  = N_H2_REACTION
            NWCH4 = N_CH4_REACTION
! update labels
            NPDERIV = 0
            DO J = 1, NREACTIONS ! (NMPHOT + NTHERMAL)
               RXLABEL( J ) = LABELS( J,1 )
               NPDERIV = NPDERIV + NREACT( J )
!               WRITE(6,'(A,I4,1X,3(A16,1X),I2)')"MECHANISM: J, LABELS(J,1:2),RXLABEL( J ), KTYPE( J ) = ", J,
!     &         LABELS( J,1:2), RXLABEL( J ), KTYPE( J )
            END DO
         END SUBROUTINE REPLACE_REACTIONS
         SUBROUTINE REORDER_REACTION_LIST(NREACTIONS, REACTION_LIST)
 !sorts a reaction based on a specified order of reactants
            IMPLICIT NONE

           INTEGER, INTENT( IN )              :: NREACTIONS
           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )

           TYPE( REACTION ),  ALLOCATABLE     :: SORTED_LIST  ( : )

           INTEGER, PARAMETER  :: NPRIORITY = 4
           INTEGER, PARAMETER  :: IPRIORITY( NPRIORITY ) = (/ 
     &                            1, 2, 3, 0 /)

           INTEGER             :: IRXN
           INTEGER             :: ICOUNT, IPR

           ALLOCATE( SORTED_LIST  ( NREACTIONS ) )

           ICOUNT = 0
           DO IPR = 1, NPRIORITY
              DO IRXN = 1, NREACTIONS
                  IF( REACTION_LIST( IRXN )%NREACT .EQ. IPRIORITY( IPR ) )THEN
                     ICOUNT = ICOUNT + 1
                     SORTED_LIST( ICOUNT ) = REACTION_LIST( IRXN )
                  END IF
              END DO
           END DO

           IF( ICOUNT .NE. NREACTIONS )THEN
               WRITE(6,*)'BELOW Reactions have # of Reactants <0 and >3'
               DO IRXN = 1, NREACTIONS
                  IF( REACTION_LIST( IRXN )%NREACT .LT. 0 .OR. REACTION_LIST( IRXN )%NREACT .GT. 3 )THEN
                     WRITE(6,'(I5,2(1X,A16))')IRXN,REACTION_LIST( IRXN )%LABEL(1:2)
                  END IF
               END DO
               STOP
           END IF    
           REACTION_LIST( 1:NREACTIONS ) = SORTED_LIST( 1:NREACTIONS )

           DEALLOCATE( SORTED_LIST )

          END SUBROUTINE REORDER_REACTION_LIST 
            
      SUBROUTINE SET_SPARSE_DATA( )
 
C***********************************************************************      
 
C  Function: To define array pointers for sparse matrix storage by
C            doing symbolic LU decomposition
 
C  Preconditions: None
 
C  Key Subroutines/Functions Called: None
 
C  Revision History: Prototype created by Jerry Gipson, August, 2004. 
C                      Based on the SMVGEAR code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994)
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files
C                               with UTILIO_DEFN
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module and supplement error message when array
C                    bounds exceed maximum values
C***********************************************************************


      IMPLICIT NONE
      
C..Includes:
      

C..Parameters:
      INTEGER, PARAMETER :: IZERO = 0  ! Integer zero

C..External Functions: None

C..Local Variables: 
      LOGICAL, SAVE :: LFIRST = .TRUE. ! Flag for first call to this subroutine

      INTEGER, SAVE :: LOGDEV  = 6     ! Logical unit number for log file
      INTEGER, SAVE :: IFNEVER = 0     ! Flag for counter initialization
      INTEGER, SAVE :: NDLMAX  = 0     ! Max # of PD loss terms in any reaction
      INTEGER, SAVE :: NDPMAX  = 0     ! Max # of PD prod terms in any reaction

      CHARACTER( 32 ) :: PNAME = 'SET_SPARSE_DATA' ! Procedure name
      CHARACTER( 80 ) :: MSG                       ! Mesaage text for output log

      INTEGER I,J,K,I1,J1,I2       ! Matrix loop indices
      INTEGER IA, IB               ! I,J index holders for decomp loop 2
      INTEGER INEW, JNEW           ! Index for sorted species number
      INTEGER IOLD, JOLD           ! Index for old species number
      INTEGER IPA, KPA             ! I,K index holders for decomp loop 1
      INTEGER IPB, KPB             ! I,K index holders for decomp loop 1
      INTEGER IPROD, JP            ! Species number of a product
      INTEGER IREACT, IR, JR       ! Species number of a reactant
      INTEGER ISP, ISP2            ! Species loop indices
      INTEGER JRE, JPR, IRE        ! Indices for nonzero Jacobian entries 
      INTEGER JZ3, JZ4             ! Counter for calcs in backsub groupings
      INTEGER NP, IAP              ! Product loop indices
      INTEGER NR, IAL, JAL         ! Reactant loop indices
      INTEGER IAR                  ! Pointer to location of PD term
      INTEGER IARRAY2              ! Final # of matrix entries w/ Sp. Mat
      INTEGER ICB                  ! Counter for # of terms in decomp loop 1
      INTEGER ICBSUM               ! Running count of calcs for j index 
                                   ! in decomp loop 1
      INTEGER ICCOUNT              ! Two term op count for decomp loop 1
      INTEGER ICNT                 ! Total op counter for decomp loop 1
      INTEGER ICNTA                ! op. counter for decomp loop 1 w/ Sp Mat 
      INTEGER ICNTB                ! op. counter for decomp loop 1 w/ Sp Mat
      INTEGER IFSUN                ! Day/night loop index
      INTEGER IJSTEP               ! Number of terms to calc in decomp loops
      INTEGER IMINNEW              ! Index holder for sort routine
      INTEGER IMINOLD              ! Index holder for sort routine
      INTEGER IPORR                ! Species number of a product or reactant
      INTEGER JCB                  ! Counter for # of terms in decomp loop 2
      INTEGER JCCOUNT              ! Two term op count for decomp loop 2
      INTEGER JCNT                 ! Total op counter for decomp loop 2 
      INTEGER JCNTA                ! op. counter for decomp loop 2 w/o Sp Mat
      INTEGER JCNTB                ! op. counter for decomp loop 2 w/ Sp Mat
      INTEGER JZ                   ! Loop index for backsub loops
      INTEGER KA                   ! Loop index for decomposition loops
      INTEGER KCNT                 ! op. counter for bksub loop 1 w/ Sp. Mat.
      INTEGER KCNTA                ! op. counter for bksub loop 1 w/o Sp Mat
      INTEGER KNTARRAY             ! Final # of matrix entries w/o Sp. Mat
      INTEGER KOUNT0               ! Initial # of matrix entries w/ Sp. Mat
      INTEGER KOUNT0A              ! Initial # of matrix entries w/o Sp. Mat
      INTEGER KZ                   ! # of nonzero calcs in backsub loop 1
      INTEGER NCSP                 ! Mechanism number NCS+1=day NCS+2=night
      INTEGER NK                   ! Reaction number
      INTEGER NLS                  ! Number of loss PD terms
      INTEGER NOCHANG              ! Count of number of species not reacting
      INTEGER NPR                  ! Number of prod PD terms
      INTEGER NQQ                  ! Loop index for Gear order      
      INTEGER NRPP                 ! Reactant plus product loop index
      INTEGER NRX                  ! Reaction loop index
      INTEGER NU                   ! Active reaction count holder
      INTEGER MCNT                 ! op. counter for bksub loop 2 w/ Sp. Mat.
      INTEGER MCNTA                ! op. counter for bksub loop 2 w/o Sp. Mat.
      INTEGER MINVALU              ! Current number of PD terms in sort
      INTEGER MZ                   ! # of nonzero calcs in backsub loop 2
      INTEGER SWAPVALUE            ! swapping number of PD terms used in sort

      INTEGER, ALLOCATABLE  :: ICLO( : )        ! Pointer to # of ops in decomp loop 1
      INTEGER, ALLOCATABLE  :: JCLO( : )        ! Pointer to # of ops in decomp loop 2

      INTEGER, ALLOCATABLE :: IZEROI  ( : )       ! Pointer to decomp loop 1 i index
      INTEGER, ALLOCATABLE :: IZEROK  ( : )       ! Pointer to decomp loop 1 k index
      INTEGER, ALLOCATABLE :: JZERO   ( : )       ! Pointer to decomp loop 2 i index
      INTEGER, ALLOCATABLE :: ISAPORL ( : )       ! Count of PD terms for each species
      INTEGER, ALLOCATABLE :: ISPARDER( :,: )     ! Indicator of a PD term in the Jacobian matrix
      INTEGER, ALLOCATABLE :: IZILCH  ( :,: )     ! # of nonzero calcs in decomp loop 1
      INTEGER, ALLOCATABLE :: JZILCH  ( :,: )     ! # of nonzero calcs in decomp loop 2
      INTEGER, ALLOCATABLE :: LZERO   ( :,: )     ! Symbolic Jacobian matrix

      INTEGER, ALLOCATABLE :: MAX_JARRAY( : )     ! maximum nozero term in each Jacobian
      INTEGER, ALLOCATABLE :: MAX_JSOLVE( : )     ! maximum operations to solve each Jacobain

      INTEGER IOS                  ! status
      INTEGER, ALLOCATABLE     :: IOLD_BUBBLE( : )
      INTEGER, ALLOCATABLE     :: INEW_BUBBLE( : )

      LOGICAL                  :: SWAPPED

      REAL( 8 ),  ALLOCATABLE :: NET_COEFF ( :, : )
      LOGICAL,    ALLOCATABLE :: NET_CHANGE( :, : )  ! true  if species affected by reaction
      LOGICAL,    ALLOCATABLE :: NO_EFFECT ( :, : ) ! false if species affected by reaction

c..The following can be uncommented to print symbolic J-matrix
c      integer iglg
c      character(1), allocatable :: ichrout( : )

C***********************************************************************                                             
c..The following can be uncommented to print symbolic J-matrix
c      allocate( ichrout( n_spec) )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize some variables on first call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

         MXCOUNT1 = NUMB_MECH_SPCS * MAXGL3 * 3
         MXCOUNT2 = NUMB_MECH_SPCS * MAXGL3 * 3

         ALLOCATE( ICLO( NCS2 ),
     &             JCLO( NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating ICLO or JCLO'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( MAX_JARRAY( NCS2 ),
     &             MAX_JSOLVE( NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating MAX_JARRAY or MAX_JSOLVE'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( NKUSERAT( NRXNS,NCS2 ),
     &             NDERIVL ( NRXNS,NCS2 ),
     &             NDERIVP ( NRXNS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating NKUSERAT, NDERIVL or NDERIVP'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( IRM2( MAXRCTNTS+MXPRD,NRXNS ),
     &             ICOEFF( MXRR+MXRP,NRXNS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating IRM2 or ICOEFF'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         IF( NSPECIAL .GT. 0 )THEN
            ALLOCATE( IRM2SP( NSPECIAL + 1, MAXSPECTERMS ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = 'ERROR allocating IRM2SP'
               WRITE(LOGDEV,'(A)')MSG 
               STOP
            END IF
            IRM2SP = 0
         END IF 

         ALLOCATE( JARRAYPT( NUMB_MECH_SPCS,NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JARRAYPT'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( JARRL( NRXNS,MXRR,NCS2 ),
     &             JARRP( NRXNS,MXRP,NCS2 ),
     &             JLIAL( NRXNS,MXRR,NCS2 ),
     &             JPIAL( NRXNS,MXRP,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JARRL, JARRP, JLIAL, or JPIAL'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( INEW2OLD( NUMB_MECH_SPCS ),
     &             IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating INEW2OLD or IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
      
         IF( .NOT. ALLOCATED( CONVERT_CONC ) )THEN
            ALLOCATE( CONVERT_CONC( NUMB_MECH_SPCS ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = 'ERROR allocating CONVERT_CONC '
               WRITE(LOGDEV,'(A)')MSG 
               STOP
            END IF
            CONVERT_CONC = .FALSE.
         END IF

         ALLOCATE( JZEROA( MXARRAY ),
     &             JZEROB( MXARRAY ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JZEROA or JZEROB'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
   
         ALLOCATE( JZLO( NCS2 ),
     &             IDEC1LO( NUMB_MECH_SPCS,NCS2 ),
     &             IDEC1HI( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JZLO, IDEC1LO or IDEC1HI'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( IJDECA( MXCOUNT2 ),
     &             IJDECB( MXCOUNT2 ),
     &             IKDECA( MXCOUNT2 ),
     &             IKDECB( MXCOUNT2 ),
     &             KJDECA( MXCOUNT2 ),
     &             KJDECB( MXCOUNT2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating IJDECA, IJDECB, IKDECA, IKDECB, KJDECA, or KJDECB'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( JHIZ1( NUMB_MECH_SPCS,NCS2 ),
     &             JHIZ2( NUMB_MECH_SPCS,NCS2 ),
     &             KZLO1( NUMB_MECH_SPCS,NCS2 ),
     &             KZLO2( NUMB_MECH_SPCS,NCS2 ),
     &             KZHI0( NUMB_MECH_SPCS,NCS2 ),
     &             KZHI1( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JHIZ1, JHIZ2, KZLO1, KZLO2, KZHI0, or KZHI1'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( KZERO( MXARRAY,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating KZERO'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( KZILCH( NUMB_MECH_SPCS,NCS2 ),
     &             MZHI0 ( NUMB_MECH_SPCS,NCS2 ),
     &             MZHI1 ( NUMB_MECH_SPCS,NCS2 ),
     &             MZILCH( NUMB_MECH_SPCS,NCS2 ),
     &             MZLO1 ( NUMB_MECH_SPCS,NCS2 ),
     &             MZLO2 ( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating KZILCH, MZHI0, MZHI1, MZILCH, MZLO1, or MZLO2'
               WRITE(LOGDEV,'(A)')MSG 
               STOP
         END IF

         ALLOCATE( IZEROI ( MXCOUNT1 ),
     &             IZEROK ( MXCOUNT2 ),
     &             JZERO  ( MXCOUNT1 ),  STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( ISAPORL ( NUMB_MECH_SPCS ),
     &             INEW_BUBBLE( NUMB_MECH_SPCS ),
     &             IOLD_BUBBLE( NUMB_MECH_SPCS ),
     &             ISPARDER( NUMB_MECH_SPCS,NUMB_MECH_SPCS ),
     &             LZERO   ( NUMB_MECH_SPCS,NUMB_MECH_SPCS ),
     &             IZILCH  ( NUMB_MECH_SPCS,NCS2 ),
     &             JZILCH  ( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( NET_COEFF ( NUMB_MECH_SPCS, NRXNS ),
     &             NET_CHANGE( NUMB_MECH_SPCS, NRXNS ),
     &             NO_EFFECT ( NUMB_MECH_SPCS, NRXNS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed: NET_COEFF etc.'
            WRITE(LOGDEV,'(A)')MSG 
         END IF

         MAX_JARRAY = 0
         MAX_JSOLVE = 0

         IZILCH = 0
         JZILCH = 0
         JHIZ1  = 0
         JHIZ2  = 0
         KZILCH = 0
         MZILCH = 0

         NUSERAT = 0
         NDERIVL = 0
         NDERIVP = 0
         
         ISCHANG = 0
         
         JARRAYPT = 0

         IJDECA = 0
         IKDECA = 0
         KJDECA = 0

         IJDECB = 0
         IKDECB = 0
         KJDECB = 0

      END IF   ! LFIRST
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initialize Prod/loss and PD tabulator arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCSP = NCS

      ISAPORL  = 0
      ISPARDER = 0
      NET_COEFF  = 0.0D0
      NET_CHANGE = .FALSE.

      DO NRX = 1, NRXNS
        DO NR = 1, 3
           IREACT = IRR( NRX,NR )
           IF( IREACT .GT. 0 ) THEN
               NET_COEFF( IREACT, NRX ) = NET_COEFF( IREACT, NRX ) - 1.0D0
               IF( .NOT. ASSESS_EFFECTS )NET_CHANGE( IREACT, NRX ) = .TRUE.
           END IF
        END DO
        DO NRPP = 4, 3 + MXPRD
           IPORR = IRR( NRX,NRPP )
           IF( IPORR .GT. 0 )THEN
               NET_COEFF( IPORR, NRX ) = NET_COEFF( IPORR, NRX ) 
     &                                 + REAL( SC( NRX,NRPP-3 ), 8 )
               IF( .NOT. ASSESS_EFFECTS )NET_CHANGE( IPORR, NRX ) = .TRUE.
           END IF 
        END DO
        IF( .NOT. ASSESS_EFFECTS )CYCLE
        DO ISP = 1, NUMB_MECH_SPCS
           IF( ABS( NET_COEFF( ISP, NRX ) ) .GT. 1.0D-6 )THEN
	       NET_CHANGE( ISP, NRX ) = .TRUE.
	   END IF
        END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set the number of Partial derivative terms in the Jacobian and
c  count the number of terms for each species
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRX = 1, NRXNS
        DO NR = 1, 3
            IREACT = IRR( NRX,NR )
            IF ( IREACT .NE. 0 ) THEN
               DO NRPP = 1, 3 + MXPRD
                  IPORR = IRR( NRX,NRPP )
                  IF ( IPORR .LT. 1 ) CYCLE
		  IF( NET_CHANGE( IPORR, NRX ) )THEN
		      ISPARDER( IPORR,IREACT ) = 1
		  END IF
               END DO
            END IF
            CYCLE
            IF ( IREACT .NE. 0 ) THEN
               DO NRPP = 1, NET_SPECIES( NRX )
                  IPORR = IRR_NET( NRPP, NRX )
                  IF ( IPORR .NE. 0 ) ISPARDER( IPORR,IREACT ) = 1
               END DO
            END IF
         END DO
      END DO

      DO IREACT = 1, NUMB_MECH_SPCS 
         DO IPORR = 1, NUMB_MECH_SPCS
            IF ( ISPARDER( IPORR,IREACT ) .EQ. 1 ) 
     &           ISAPORL( IPORR ) = ISAPORL( IPORR ) + 1
         END DO
      END DO
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Sort the species, putting all with zero partial derivative 
c  terms at the bottom and those with fewest PD terms at top.
c  Set arrays for species with zero PD terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NOCHANG = NUMB_MECH_SPCS
      DO JOLD = 1, NUMB_MECH_SPCS
         IF ( ISAPORL( JOLD ) .GT. 0 ) THEN
            ISCHANG( NCS ) = ISCHANG( NCS ) + 1
            JNEW = ISCHANG( NCS )
            INEW2OLD( JNEW ) = JOLD
            IOLD2NEW( JOLD ) = JNEW
         ELSE
            INEW2OLD( NOCHANG ) = JOLD
            IOLD2NEW( JOLD )    = NOCHANG
            NOCHANG = NOCHANG - 1
         END IF
      END DO
      INEW_BUBBLE(:) = IOLD2NEW(:)

!     DO J = 1, NUMB_MECH_SPCS
!        write(6,'(A,2(I4,1X),A16,1X,I6)')'Before bubble sort: J, ISAPORL( J )= ',
!    &             J, ISAPORL( J ),MECHANISM_SPC( J ),INEW_BUBBLE(J)
!     END DO
 
      IF( REORDER_SPECIES )THEN 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Now sort by number of PD terms, fewest at position 1, most at
c  the end position. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO JNEW = 1, ISCHANG( NCS )
c  Uncomment the following three lines to turn off species ordering;
c  not recommended since computational efficiency reduced
!        INEW2OLD( JNEW,NCS ) = JNEW
!        IOLD2NEW( JNEW,NCS ) = JNEW
!        IF ( JNEW .NE. 0 ) GO TO 180
         JOLD = INEW2OLD( JNEW )
         MINVALU = ISAPORL( JOLD )
         IMINOLD = JOLD
         IMINNEW = JNEW

         DO INEW = JNEW + 1, ISCHANG( NCS )
            IOLD = INEW2OLD( INEW )
            IF ( ISAPORL( IOLD ) .LT. MINVALU ) THEN
               MINVALU = ISAPORL( IOLD )
               IMINOLD = IOLD
               IMINNEW = INEW
            END IF
         END DO

         INEW2OLD( IMINNEW ) = JOLD
         INEW2OLD( JNEW )    = IMINOLD
         IOLD2NEW( JOLD )    = IMINNEW
         IOLD2NEW( IMINOLD ) = JNEW
      END DO


!      call Bubble_Sort(ISAPORL)
        DO j = NUMB_MECH_SPCS-1, 1, -1
         swapped = .FALSE.
         DO i = 1, j
           IF ( ISAPORL(i) .GT. ISAPORL(i+1) ) THEN
             SWAPVALUE    = ISAPORL(i)
             ISAPORL(i)   = ISAPORL(i+1)
             ISAPORL(i+1) = SWAPVALUE
             INEW         = INEW_BUBBLE( i )
             INEW_BUBBLE( i )     = INEW_BUBBLE( i + 1 )
             INEW_BUBBLE( i + 1 ) = INEW
             swapped = .TRUE.
           END IF
         END DO
         IF (.NOT. swapped) EXIT
       END DO
      END IF ! reorder species

      DO J = 1, NUMB_MECH_SPCS
         I = INEW_BUBBLE(J)
         INEW2OLD(J) = INEW_BUBBLE(J)
         IOLD_BUBBLE(I) = J
!        write(6,'(A,2(I4,1X),A16,1X,I6)')'After bubble sort: J, ISAPORL( J )= ',
!    &             J, ISAPORL( J ),MECHANISM_SPC( I ),INEW_BUBBLE(I)
      END DO

      
      DO J = 1, NUMB_MECH_SPCS
         I = IOLD_BUBBLE(J)
         IOLD2NEW(J) = IOLD_BUBBLE(J)
!        write(6,'(A,2(I4,1X),A16,1X,I6)')'IOLD_BUBBLE result: I, ISAPORL( I )= ',
!    &             I, ISAPORL( I ),MECHANISM_SPC( J ),J
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Define sparse_species based sort results
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          DO J = 1, NUMB_MECH_SPCS
              I = INEW2OLD( J )
              SPARSE_SPECIES( J ) = MECHANISM_SPC( I )
!             WRITE( 6, 2065 ) J, J, J, CGRID_INDEX( I ), 
!    &       SPECIES_TYPE( I ), CONVERT_CONC( I ), TRIM( MECHANISM_SPC( I ) )
          END DO
2065   FORMAT( 6X, 'DATA', 1X, 'CGRID_INDEX(', I4,' ), SPECIES_TYPE(', I4,' ), CONVERT_CONC(', I4,' ) / ', 
     &              I4, ', ''', A2, ''', ',  L1,' /  ! ', A)

               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Fill the irm2 array using the new species order developed above.
c  Also determine active reactions for day and then night (i.e., photo
c  reactions determined by BTEST=.TRUE. are not included for nighttime)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NO_EFFECT = .TRUE.
      DO NRX = 1, NRXNS
         DO NR = 1, NREACT( NRX )
            IREACT = IRR( NRX,NR )
            IRM2( NR,NRX ) = IOLD2NEW( IREACT ) 
         END DO

         DO NP = 1, NPRDCT( NRX )
            IPROD = IRR( NRX, NP + 3 )
            IRM2( NP+3,NRX ) = IOLD2NEW( IPROD )
         END DO

         DO NRPP = 1, ISCHANG( NCS )
            JNEW = IOLD2NEW( NRPP  )
            IF( NET_CHANGE( NRPP, NRX ) )NO_EFFECT( JNEW, NRX ) = .FALSE.
         END DO	 
         
         IF ( NREACT( NRX ) .GT. 0 ) THEN
            NUSERAT( NCS ) = NUSERAT( NCS ) + 1
            NU = NUSERAT( NCS )
            NKUSERAT( NU, NCS ) = NRX
            IF ( .NOT. ( BTEST ( IRXBITS( NRX ),1 ) ) ) THEN
               NUSERAT( NCS + 1 ) = NUSERAT( NCS + 1 ) + 1
               NU = NUSERAT( NCS + 1 )
               NKUSERAT( NU, NCS + 1 ) = NRX
            END IF
         END IF
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill the irm2sp array using the  new species order developed above.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO J = 1, NSPECIAL
         DO I = 1, MAXSPECTERMS
            IF( INDEX_CTERM( J, I) .LE. 0 )CYCLE
            IRM2SP( J, I ) =  IOLD2NEW( INDEX_CTERM( J,I ) )
         END DO
      END DO
          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic LU decomposition to determine sparse storage array
c  structures. Done twice, first for day and then for night. An entry
c  of 1 in lzero means a non-negative entry in the Jacobian. First
c  put ones on the diagonal and zeroes everywhere else.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 700 IFSUN = 1, 2
         NCSP = IFSUN
         DO I = 1, NUMB_MECH_SPCS
            DO J = 1, NUMB_MECH_SPCS
               LZERO( J,I ) = 0
            END DO
            LZERO( I,I ) = 1
         END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill in the rest of the entries in the Jacobian
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( IFSUN )
            NK = NKUSERAT( NRX,IFSUN )
            DO NR = 1, NREACT( NK )
               IRE = IRM2( NR,NK )
               DO JAL = 1, NREACT( NK )
                  JRE = IRM2( JAL,NK )
                  IF( NO_EFFECT( JRE, NK ) )CYCLE
                  LZERO( JRE,IRE ) = 1
               END DO
               DO IAP = 1, NPRDCT( NK )
                  JPR = IRM2( 3+IAP,NK )
                  IF( NO_EFFECT( JPR, NK ) )CYCLE
                  LZERO( JPR,IRE ) = 1 
               END DO
               CYCLE

               DO JAL = 1, NET_SPECIES( NK )
                  JRE = IOLD2NEW( IRR_NET( JAL, NK ) )
                  LZERO( JRE,IRE ) = 1
               END DO
           END DO
         END DO
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Uncomment to print the undecomposed matrix symbolically
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        iglg = 0
c        write( logdev,8200 ) 
c8200    format(//1x,'jacobian ')
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(1:1),j=1,n_spec)
c8211    format( 9x, 40( 2x,A1 ) )
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(2:2),j=1,n_spec)
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(3:3),j=1,n_spec)
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(4:4),j=1,n_spec)
c        write( logdev,8210 ) (i,i=1,ischang(ncs))
c8210    format( /9x, 40I3 )
c        DO 585 i = 1, ischang( ncs )
c           k = inew2old( i )
c           DO 584 j=1,n_spec
c               if ( lzero(i,j ) .NE. 0 ) then
c                 iglg = iglg + 1
c                 ichrout( j ) = 'X'
c              else
c                 ichrout( j ) = ' '
c              end if
c584         continue
c            write( logdev,8220 ) chemistry_spc( k ), i, (ichrout( j ), j=1,n_spec)
c8220        format( 1x, A4, 1x, I2, 1x, 40( 2x,A1 ) )
c585     continue
c        write( logdev,8230 ) iglg
c8230    format( 1x,'Total number of nonzero entries=',I5 )
  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up arrays for decomposition / back-substitution of sparse     
c  matrices by removing all calculations involving a zero.          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( IFNEVER.EQ.0 ) THEN
            IFNEVER = 1
            ICNT    = 0 
            JCNT    = 0 
            ICCOUNT = 0
            JCCOUNT = 0
         END IF
         KOUNT0A = 0
         KOUNT0  = 0
         ICNTA   = 0
         ICNTB   = 0
         JCNTA   = 0
         JCNTB   = 0
         KCNTA   = 0
         MCNTA   = 0
         KCNT    = 0
         MCNT    = 0
         IARRAY2 = 0
         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Count number of entries w/ and w/o sparse matrix storage
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc         
         DO J = 1, ISCHANG( NCS )
            DO K = 1, ISCHANG( NCS )
               KOUNT0A = KOUNT0A + 1
               IF ( LZERO( J,K ) .EQ. 1 ) KOUNT0 = KOUNT0 + 1
            END DO
         END DO
         print*,KOUNT0A,KOUNT0


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the symbolic decomposition (ludcmp) converting [A] to [L][U] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ICLO( IFSUN ) = ICNT + 1
         JCLO( IFSUN ) = JCNT + 1
         LUDECOMP: DO J = 1, ISCHANG( NCS )
            J1 = J - 1
            
c...  First loop of decomposition
            DO I = 2, ISCHANG( NCS ) 
               I1 = J1 
               IF ( I .LE. J1 ) I1 = I - 1
               DO K = 1, I1
                  ICNTA = ICNTA + 1
                  IF(LZERO( I,K ).EQ.1 .AND. LZERO( K,J ).EQ.1)THEN
                     IZILCH( J,IFSUN ) = IZILCH( J,IFSUN ) + 1
                     ICNT             = ICNT + 1
                     ICNTB            = ICNTB + 1
                     IZEROK( ICNT )   = K   
                     IZEROI( ICNT )   = I
                     LZERO( I,J )     = 1 
                  END IF
               END DO
            END DO
c... Second loop of decomposition 
            DO I = J + 1, ISCHANG( NCS ) 
               JCNTA = JCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN
                  JZILCH( J,IFSUN ) = JZILCH( J,IFSUN ) + 1
                  JCNT             = JCNT  + 1
                  JCNTB            = JCNTB + 1
                  JZERO( JCNT )    = I  
               END IF
            END DO 
         END DO LUDECOMP
         KOUNT0A = 0
         KOUNT0  = 0
         DO J = 1, ISCHANG( NCS )
            DO K = 1, ISCHANG( NCS )
               KOUNT0A = KOUNT0A + 1
               IF ( LZERO( J,K ) .EQ. 1 ) KOUNT0 = KOUNT0 + 1
            END DO
         END DO
         print*,KOUNT0A,KOUNT0
  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic back-substition for solving [L][U]{x}={b}. Store data
c  in sparse matrix pointer jarraypt.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First loop of back-substitution
         DO I = 2, ISCHANG( NCS )
            I1 = I - 1
            DO J = 1, I1    
               KCNTA = KCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN 
                  KZILCH( I,IFSUN ) = KZILCH( I,IFSUN ) + 1
                  KCNT = KCNT + 1
                  IARRAY2 = IARRAY2 + 1
                  KZERO( IARRAY2,IFSUN ) = J
                  JARRAYPT( I,J,IFSUN ) = IARRAY2 
               END IF
            END DO
         END DO 
         print*,'First loop of back-substitution; IARRAY2: ', IARRAY2

c... Second loop of back-substitution 
         DO I = ISCHANG( NCS ) - 1, 1, -1
            I2 = I + 1
            DO J = I + 1, ISCHANG( NCS )
               MCNTA = MCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN 
                  MZILCH( I,IFSUN )      = MZILCH( I,IFSUN ) + 1
                  MCNT                  = MCNT + 1
                  IARRAY2               = IARRAY2 + 1
                  KZERO( IARRAY2,IFSUN ) = J
                  JARRAYPT( I,J,IFSUN )  = IARRAY2 
               END IF
            END DO
         END DO
         print*,'Second loop of back-substitution; IARRAY2: ', IARRAY2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill jarraypt with remaining diagonal array points and save counts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO I = 1, ISCHANG( NCS ) 
            IARRAY2 = IARRAY2 + 1
            JARRAYPT( I,I,IFSUN ) = IARRAY2 
         END DO
         print*,'Fill jarraypt with remaining diagonal array points; IARRAY2: ',IARRAY2
         IARRAY( IFSUN ) = IARRAY2 
         KNTARRAY = KCNTA + MCNTA + ISCHANG( NCS )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do decomposition again to change arrays to use jarraypt
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         JCB = JCLO( IFSUN ) 
         JZLO( IFSUN ) = JCCOUNT
         ICBSUM = ICLO( IFSUN ) - 1 
         IJSTEP = 2   

         DO J = 1, ISCHANG( NCS )

c...First loop of decomposition
            IDEC1LO( J,IFSUN ) = ICCOUNT + 1
            ICB = ICBSUM  + 1
            ICBSUM = ICBSUM + IZILCH( J, IFSUN ) 

            DO KA = 1, IZILCH( J,IFSUN ), IJSTEP
               ICCOUNT = ICCOUNT + 1
               IPA = IZEROI( ICB ) 
               KPA = IZEROK( ICB ) 
               IJDECA( ICCOUNT ) = JARRAYPT( IPA,  J,IFSUN ) 
               IKDECA( ICCOUNT ) = JARRAYPT( IPA,KPA,IFSUN )
               KJDECA( ICCOUNT ) = JARRAYPT( KPA,  J,IFSUN )
               IF ( ICB + 1 .LE. ICBSUM ) THEN
                  IPB = IZEROI( ICB + 1 ) 
                  KPB = IZEROK( ICB + 1 ) 
                  IJDECB( ICCOUNT ) = JARRAYPT( IPB,  J,IFSUN ) 
                  IKDECB( ICCOUNT ) = JARRAYPT( IPB,KPB,IFSUN )
                  KJDECB( ICCOUNT ) = JARRAYPT( KPB,  J,IFSUN )
               END IF
               ICB = ICB + IJSTEP   
            END DO

            IDEC1HI( J,IFSUN ) = ICCOUNT  
            
c...Second loop of decomposition
            JZ = JZILCH( J, IFSUN )

            DO I = 1, JZ - 1, 2
               JCCOUNT           = JCCOUNT + 1
               JHIZ1( J,IFSUN )   = JHIZ1( J,IFSUN ) + 1
               IA                = JZERO( JCB )
               IB                = JZERO( JCB + 1 )
               JZEROA( JCCOUNT ) = JARRAYPT( IA,J,IFSUN )
               JZEROB( JCCOUNT ) = JARRAYPT( IB,J,IFSUN )
               JCB = JCB + 2
            END DO

            IF ( MOD( JZ,2 ) .EQ. 1 ) THEN 
               JCCOUNT           = JCCOUNT + 1
               JHIZ2( J,IFSUN )   = JHIZ2( J,IFSUN ) + 1
               IA                = JZERO( JCB )
               JZEROA( JCCOUNT ) = JARRAYPT( IA,J,IFSUN )
               JCB               = JCB + 1 
            END IF
         END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Group terms to increase efficiency in back-substition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First back-substitution loop
         DO I = 1, ISCHANG( NCS ) 
            KZ              = KZILCH( I,IFSUN )
            KZHI0( I,IFSUN ) = KZ - 4 
            JZ3             = 0

            DO JZ = 1, KZHI0( I,IFSUN ), 5     
               JZ3 = JZ + 4
            END DO  

            KZLO1( I,IFSUN ) = JZ3 + 1
            KZHI1( I,IFSUN ) = KZ  - 1 
            JZ4 = JZ3 

            DO JZ = JZ3 + 1, KZ - 1, 2    
               JZ4 = JZ + 1
            END DO

            KZLO2( I,IFSUN ) = JZ4 + 1
         END DO
 
c... Second loop of back-substitution
         DO I = ISCHANG( NCS ), 1, -1
            MZ = MZILCH( I,IFSUN ) 
            MZHI0( I,IFSUN ) = MZ - 4  
            JZ3 = 0 

            DO JZ = 1, MZHI0( I,IFSUN ), 5  
               JZ3 = JZ + 4 
            END DO

            MZLO1( I,IFSUN ) = JZ3 + 1
            MZHI1( I,IFSUN ) = MZ  - 1
            JZ4 = JZ3 

            DO JZ = JZ3+1, MZ-1, 2 
               JZ4 = JZ + 1 
            END DO

            MZLO2( I,IFSUN ) = JZ4 + 1
         END DO
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( ICNT .GT. MXCOUNT2 .OR. JCNT .GT. MXCOUNT1 .OR. 
     &        IARRAY2 .GT. MXARRAY .OR. ICCOUNT .GT. MXCOUNT2 .OR.
     &        JCCOUNT .GT. MXARRAY ) THEN
            WRITE( MSG, 94000 ) 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94020 ) MXCOUNT2, ICNT 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94040 ) MXCOUNT1, JCNT 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94060 ) MXARRAY, IARRAY2 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94080 ) MXCOUNT2, ICCOUNT 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94100 ) MXARRAY, JCCOUNT, MAXGL3
            WRITE( LOGDEV,94110 )
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF           

!         MAX_JSOLVE( IFSUN )  = INT(MAX( ICNT, JCNT, ICCOUNT ) / ( 3 * NUMB_MECH_SPCS )  ) + 1
         MAX_JSOLVE( IFSUN )  = MAX( ICNT, JCNT, ICCOUNT )
         MAX_JARRAY( IFSUN )  = MAX( IARRAY2,  JCCOUNT ) + 1
         WRITE(6,'(A,I3, 1X, 6(I8,1X))')'IFSUN, MAX_JSOLVE, MAX_JARRAY, IARRAY2, JCCOUNT: ',IFSUN, 
     &   MAX_JSOLVE( IFSUN ), MAX_JARRAY( IFSUN ),IARRAY2,  JCCOUNT

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set final arrays for partial derivative calculations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( IFSUN )
            NK = NKUSERAT( NRX,IFSUN )
            DO IAL = 1, NREACT( NK )
               IR = IRM2( IAL,NK )

               DO JAL = 1, NREACT( NK )
                  JR = IRM2( JAL,NK )
                  IF( NO_EFFECT( JR, NK ) ) CYCLE
                  IAR = JARRAYPT( JR,IR,NCSP )
                  NDERIVL( NK,NCSP ) = NDERIVL( NK,NCSP ) + 1
                  NLS = NDERIVL( NK,NCSP )
                  JARRL( NK,NLS,IFSUN ) = IAR
                  JLIAL( NK,NLS,IFSUN ) = IAL
                  NDLMAX = MAX( NLS,NDLMAX )
               END DO
               
               DO IAP = 1, NPRDCT( NK )
                  JP = IRM2( IAP+3,NK )
                  IF( NO_EFFECT( JP, NK ) ) CYCLE
                  IAR = JARRAYPT( JP,IR,NCSP )
                  NDERIVP( NK,NCSP ) = NDERIVP( NK,NCSP ) + 1
                  NPR = NDERIVP( NK,NCSP )
                  JARRP(  NK,NPR,IFSUN ) = IAR
                  JPIAL(  NK,NPR,IFSUN ) = IAL
                  ICOEFF( NPR,NK,NCSP ) = 0
                  IF ( ABS( SC( NK,IAP ) - 1.0 ) .GT. 1.0D-06 ) THEN
                     ICOEFF( NPR,NK,NCSP ) = IAP
                  END IF
                  NDPMAX = MAX( NPR,NDPMAX )
               END DO
               CYCLE

               DO JAL = 1, PURE_NREACT( NK )
!                  print*,NK,TRIM(RXLABEL(NK)),PURE_NREACT( NK ),NET_SPECIES( NK ),IRR_NET( JAL,NK ) 
                  JR = IOLD2NEW( IRR_NET( JAL,NK ) )
                  IAR = JARRAYPT( JR,IR,IFSUN )
                  NDERIVL( NK,IFSUN ) = NDERIVL( NK,IFSUN ) + 1
                  NLS = NDERIVL( NK,IFSUN )
                  JARRL( NK,NLS,IFSUN ) = IAR
                  JLIAL( NK,NLS,IFSUN ) = IAL
                  NDLMAX = MAX( NLS,NDLMAX )
               END DO
               
               DO IAP = (PURE_NREACT( NK )+1), NET_SPECIES( NK )
                  JP = IOLD2NEW( IRR_NET( IAP,NK ) )
                  IAR = JARRAYPT( JP,IR,IFSUN )
                  NDERIVP( NK,IFSUN ) = NDERIVP( NK,IFSUN ) + 1
                  NPR = NDERIVP( NK,IFSUN )
                  JARRP(  NK,NPR,IFSUN ) = IAR
                  JPIAL(  NK,NPR,IFSUN ) = IAL
!                  print*,NK,TRIM(RXLABEL(NK)),JP,NPR,PURE_NREACT( NK ),NET_SPECIES( NK ),(MXRP+MXRR)
                  ICOEFF( NPR,NK,IFSUN ) = 0
                  IF ( ABS( SC_NET( IAP, NK ) - 1.0 ) .GT. 1.0E-06 ) THEN
                     ICOEFF( NPR,NK,IFSUN ) = IAP
                  END IF
                  NDPMAX = MAX( NPR,NDPMAX )
               END DO
            END DO     
         END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions of PD arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NDPMAX .GT. MXRP .OR. NDLMAX .GT. MXRR ) THEN
            WRITE( MSG, 94000 ) 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94200 ) MXRP, NDPMAX 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94220 ) MXRR, NDLMAX 
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
700   CONTINUE

      

      WRITE(6,'(A,2(I8,1X))')'Initial MXCOUNT2, MXARRAY: ',MXCOUNT2, MXARRAY
      MAXGL3  = MAXVAL( MAX_JSOLVE )
      MXARRAY = MAXVAL( MAX_JARRAY )
      WRITE(6,'(A,2(I8,1X))')'  Reset MXCOUNT2, MXARRAY: ',MAXGL3, MXARRAY
  
!      PAUSE !      STOP
      RETURN
      
C********************** FORMAT STATEMENTS ******************************      
 
94000 FORMAT( 1X,'One of the dimensions below is too small:')
94020 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICNT    = ',I6)  
94040 FORMAT( 1X,'DIMENSION: MXCOUNT1 = ',I6,' VARIABLE: JCNT    = ',I6)  
94060 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: IARRAY2 = ',I6)  
94080 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICCOUNT = ',I6)  
94100 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: JCCOUNT = ',I6,' MAXGL3   = ',I6 )
94110 FORMAT( 1X,'NOTE:      MXCOUNT[1,2] = NUMB_MECH_SPCS * MAXGL3 * 3' )
94200 FORMAT( 1X,'DIMENSION: MXRP     = ',I6,' VARIABLE: NDPMAX  = ',I6)
94220 FORMAT( 1X,'DIMENSION: MXRR     = ',I6,' VARIABLE: NDLMAX  = ',I6)
      END SUBROUTINE SET_SPARSE_DATA
       END MODULE MECHANISM_DATA
