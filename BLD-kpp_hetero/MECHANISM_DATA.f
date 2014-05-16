       MODULE MECHANISM_DATA
       
         USE MECHANISM_PARMS

         IMPLICIT NONE



         CHARACTER( 120 ) :: EQUATIONS_MECHFILE
         CHARACTER(  32 ) :: MECHNAME
         
         INTEGER KUNITS, 
     &           KTYPE       ( MAXRXNUM ),
     &           IRXBITS     ( MAXRXNUM ),
     &           IORDER      ( MAXRXNUM ),
     &           KTN1, KRX1  ( MAXRXNUM ), 
     &           KTN2, KRX2  ( MAXRXNUM ), 
     &           KTN3, KRX3  ( MAXRXNUM ),
     &           KTN4, KRX4  ( MAXRXNUM ),
     &           KTN5, KRX5  ( MAXRXNUM ),
     &           KTN6, KRX6  ( MAXRXNUM ),
     &           KTN7, KRX7  ( MAXRXNUM ),
!    &           KCNV, KRXCNV( MAXRXNUM ),
     &           NFALLOFF, 
     &           IRRFALL( MAXFALLOFF )

         INTEGER NWM,   NRXWM  ( MAX3BODIES )
         INTEGER NWW,   NRXWW  ( MAX3BODIES )
         INTEGER NWO2,  NRXWO2 ( MAX3BODIES )
         INTEGER NWN2,  NRXWN2 ( MAX3BODIES )
         INTEGER NWCH4, NRXWCH4( MAX3BODIES )
         INTEGER NWH2,  NRXWH2 ( MAX3BODIES )

         REAL( 8 ) :: RTDAT( 3,MAXRXNUM )
         REAL( 8 ) :: RFDAT( 5,MAXFALLOFF )
         REAL( 8 ) :: CONST( MAXCONSTS )        
          
         INTEGER         :: NRXNS                      ! number of reactions
         INTEGER         :: NPDERIV                    ! number nonzero PD in mechanism
         INTEGER         :: NMPHOT                     ! number of photolysis reactions
         INTEGER         :: NSPECIAL_RXN               ! number of special rate constant reactions
         INTEGER         :: ISPECIAL( MAXSPECRXNS,2 )
         INTEGER         :: NSPECIAL
         CHARACTER( 16 ) :: SPECIAL( MAXSPECRXNS )

         INTEGER         :: NKC_TERMS(  MAXSPECRXNS )
         CHARACTER( 16 ) :: KC_TERMS(   MAXSPECRXNS,  MAXSPECTERMS, 2)
         INTEGER         :: INDEX_KTERM( MAXSPECRXNS, MAXSPECTERMS)
         INTEGER         :: INDEX_CTERM( MAXSPECRXNS, MAXSPECTERMS)
         REAL( 8 )       :: KC_COEFFS(  MAXSPECRXNS,  MAXSPECTERMS)
         INTEGER         :: N_OPERATORS( MAXSPECRXNS )
         INTEGER         :: OPERATORS( MAXSPECRXNS, MAXSPECTERMS )
         REAL( 8 )       :: OPERATOR_COEFFS( MAXSPECRXNS, MAXSPECTERMS)
         
         INTEGER IPH( MAXPHOTRXNS,3 )
         INTEGER NPHOTAB                          ! no. of unique photolysis rates
         CHARACTER( 16 ) :: PHOTAB( MAXPHOTRXNS ) ! photolysis rate name or label

         INTEGER IHETERO( MAXPHOTRXNS,2 )         ! mapping bewtween reaction # and unique heteorogeneous rates
         INTEGER MHETERO                          ! no. of heteorogeneous reactions
         INTEGER NHETERO                          ! no. of unique heteorogeneous rates 
         CHARACTER( 16 ) :: HETERO( MAXPHOTRXNS ) ! names of unique heteorogeneous rates

         INTEGER MXPRD                            ! max no. products

         INTEGER NPRDCT( MAXRXNUM )               ! no. of products for rx j
         INTEGER NREACT( MAXRXNUM )               ! no. of reactants for rx j
         INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
         REAL    SC ( MAXRXNUM,MAXPRODS )
         REAL( 8 ), ALLOCATABLE, SAVE :: NET_RCOEFF( :, :)
         
c.. Variables for steady-state species
         INTEGER         :: N_SS_SPC = 0                         ! No. of SS species
         CHARACTER( 16 ) :: SS_SPC( MAXNLIST )                   ! List of SS pecies names
         INTEGER         :: SS_RCT_COEF( MAXNLIST, MAXRXNUM )    ! Reactant coeffs for each SS species
         REAL            :: SS_PRD_COEF( MAXNLIST, MAXRXNUM )    ! Product coeffs for each SS species
         INTEGER         :: MAX_SS_LOSS = 0                      ! Max no of reactions for which 1 SS species
                                                              ! appears as a reactant
         INTEGER         :: MAX_SS_PROD = 0                      ! Max no of reactions for which 1 SS species
                                                              ! appears as a product
         INTEGER         :: N_LOSS_RXNS( MAXNLIST )              ! No. of loss rxns for each SS species
         INTEGER         :: N_PROD_RXNS( MAXNLIST )              ! No. of prod rxns for each SS species
         INTEGER         :: SS_LOSS_RXNS( MAXNLIST, MAXRXNUM )   ! List of rxns in which SS species is a reactant
         INTEGER         :: SS_PROD_RXNS( MAXNLIST, MAXRXNUM )   ! List of rxns in which SS species is a product
         INTEGER         :: SS_RCT_IND( MAXRXNUM )               ! SS spc ind that reacts w/ a non-SS spc
         REAL            :: SS_PROD_COEF( MAXNLIST, MAXRXNUM )   ! Yields for rxns producing a SS species
         

         CHARACTER( 16 ) RXLABEL( MAXRXNUM )   ! label for rx 

         CHARACTER( 120 ) :: EQNAME_SPCS
         CHARACTER( 120 ) :: EQNAME_RXDT
         CHARACTER( 120 ) :: EQNAME_RXCM
         CHARACTER( 120 ) :: FNAME_MODULE
         CHARACTER( 120 ) :: FNAME_DATA_MODULE
         CHARACTER( 120 ) :: FNAME_FUNC_MODULE
         CHARACTER( 120 ) :: OUTDIR 

         INTEGER        ::  EXUNIT_SPCS
         INTEGER        ::  EXUNIT_RXDT
         INTEGER        ::  EXUNIT_RXCM
         
         INTEGER        ::  MODULE_UNIT
         INTEGER        ::  DATA_MODULE_UNIT
         INTEGER        ::  FUNC_MODULE_UNIT
         

         LOGICAL, SAVE      :: USE_SPCS_NAMELISTS  ! species data based on CMAQ NMLS
         LOGICAL, SAVE      :: WRITE_CGRID_DATA  = .TRUE.

         INTEGER,            ALLOCATABLE ::  CGRID_INDEX  ( : )
         INTEGER,            ALLOCATABLE ::  TYPE_INDEX   ( : )
         REAL,               ALLOCATABLE ::  SPECIES_MOLWT( : )
         CHARACTER( 16),     ALLOCATABLE ::  CGRID_SPC    ( : )
         CHARACTER(LEN = 2), ALLOCATABLE ::  SPECIES_TYPE ( : )
         
         INTEGER                      ::  N_GAS_CHEM_SPC
         INTEGER                      ::  NUMB_MECH_SPCS
         INTEGER ,        ALLOCATABLE ::  MECHANISM_INDEX( : )
         CHARACTER( 16 ), ALLOCATABLE ::  MECHANISM_SPC  ( : )

c..Miscellaneous variables
         INTEGER, PARAMETER :: NCS  = 1        ! no. of chemical mechanisms
         INTEGER, PARAMETER :: NCS2 = 2 * NCS  ! accounts for day/night 


c..Sparse Matrix maximum dimensions
         INTEGER, PARAMETER :: MAXGL   = 250   ! Max # of P/L terms per species
         INTEGER, PARAMETER :: MAXGL2  = 200    ! Dimension (smaller than maxgl)
         INTEGER, PARAMETER :: MAXGL3  = 200   ! Dimension (smaller than maxgl)
         INTEGER, PARAMETER :: MXARRAY = 10000 ! Max # of terms in I-hJ matrix

c..Mechanism specific variables
         INTEGER, SAVE :: N_SPEC               ! No. of species in mech
         INTEGER, SAVE :: N_RXNS               ! No. of reactions in mech

         INTEGER, SAVE :: MXCOUNT1, MXCOUNT2   ! Sparse matrx pntr dimensions
         INTEGER, SAVE :: MXRR, MXRP           ! Max # of PD terms


c..Sparse Matrix variables 
         INTEGER, SAVE :: ISCHAN          ! No. of reacting species in current mech
         INTEGER, SAVE :: ISCHANG( NCS  ) ! No. of reacting species in day & nite
         INTEGER, SAVE :: NUSERAT( NCS2 ) ! No. of active rxns in day & nite
         INTEGER, SAVE :: IARRAY(  NCS2 ) ! No. of PD terms in I-hJ matrix

C Most of the following are allocated
         INTEGER, ALLOCATABLE, SAVE :: NKUSERAT( :,: )     ! Rxn nos of active rxns
         INTEGER, ALLOCATABLE, SAVE :: NET_EFFECT( :, : )   ! reaction's net effect on species
         INTEGER, ALLOCATABLE, SAVE :: IRM2  ( :,:,: )     ! Species rxn array
         INTEGER, ALLOCATABLE, SAVE :: ICOEFF( :,:,: )     ! stoich coeff indx
         
         INTEGER, ALLOCATABLE, SAVE :: JARRAYPT( :,:,: )   ! A-Matrix index
         INTEGER, ALLOCATABLE, SAVE :: JARRL( :,:,: )      ! Pntr to PD Loss term
         INTEGER, ALLOCATABLE, SAVE :: JARRP( :,:,: )      ! Pntr to PD Prod term
         INTEGER, ALLOCATABLE, SAVE :: JLIAL( :,:,: )      ! Spec # for PD loss term
         INTEGER, ALLOCATABLE, SAVE :: JPIAL( :,:,: )      ! Spec # for PD prod term 
        
         INTEGER, ALLOCATABLE, SAVE :: INEW2OLD( :,: )     ! Spec index xref
         INTEGER, ALLOCATABLE, SAVE :: IOLD2NEW( :,: )     ! Spec index xref

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


         CONTAINS
         
         SUBROUTINE INIT_MECH_DATA()
!   Function initialize module variables         
           IMPLICIT NONE
         
         
           INTEGER    :: ISPC, IRX    ! loop counters

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            DO 101 IRX = 1, MAXRXNUM
               DO ISPC = 1, MAXPRODS+3
                  IRR( IRX,ISPC ) = 0
               END DO
               DO ISPC = 1, MAXPRODS
                  SC( IRX,ISPC ) = 0.0
               END DO
               DO ISPC = 1, 3
                   RTDAT( ISPC,IRX ) = 0.0
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
101         CONTINUE

            NFALLOFF = 0

            DO 103 IRX = 1, MAXFALLOFF
               IRRFALL( IRX ) = 0   
               DO ISPC = 1, 5
                  RFDAT( ISPC,IRX ) = 0.0
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
            DO ISPC = 1, MAXPHOTRXNS
               IPH( ISPC,1 ) = 0
               IPH( ISPC,2 ) = 0
               IPH( ISPC,3 ) = 0
               PHOTAB( ISPC ) = ' '
            END DO
            
            NSPECIAL     = 0
            NSPECIAL_RXN = 0

            DO ISPC = 1, MAXSPECRXNS
               ISPECIAL( ISPC,1 ) = 0
               ISPECIAL( ISPC,2 ) = 0
               SPECIAL( ISPC )    = ' '
               NKC_TERMS( ISPC )  = 0
               KC_COEFFS( ISPC,  1:MAXSPECTERMS) = 0.0
               KC_TERMS(  ISPC,  1:MAXSPECTERMS, 1) = ' '
               KC_TERMS(  ISPC,  1:MAXSPECTERMS, 2) = ' '
               INDEX_KTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = 0
               INDEX_CTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = 0
               N_OPERATORS( ISPC )  = 0
               OPERATORS(   ISPC, 1:MAXSPECTERMS)  = 0
               OPERATOR_COEFFS( ISPC, 1:MAXSPECTERMS) = 0.0
            END DO
            
            IHETERO = 0
            NHETERO = 0
            HETERO  = '                '
            
            SS_RCT_COEF = 0                 ! Array initialization
            SS_PRD_COEF = 0.0               ! Array initialization
            SS_RCT_IND  = 0                 ! Array initialization
            MAX_SS_LOSS = 0
            MAX_SS_PROD = 0



         RETURN
         END SUBROUTINE INIT_MECH_DATA
            
       END MODULE MECHANISM_DATA
            
            
            
