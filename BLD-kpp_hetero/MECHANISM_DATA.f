       MODULE MECHANISM_DATA
       
         USE MECHANISM_PARMS

         IMPLICIT NONE


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
         
         INTEGER NRXNS            ! number of reactions
         INTEGER NMPHOT           ! number of photolysis reactions
         INTEGER NSPECIAL_RXN     ! number of special rate constant reactions
         INTEGER ISPECIAL( MAXSPECRXNS,2 )
         INTEGER NSPECIAL
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

         INTEGER NPRDCT( MAXRXNUM )               ! no. of products for rx j
         INTEGER NREACT( MAXRXNUM )               ! no. of reactants for rx j
         INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
         REAL    SC ( MAXRXNUM,MAXPRODS )

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
         CHARACTER( 120 ) :: RXNS_MODULE

         INTEGER        ::  EXUNIT_SPCS
         INTEGER        ::  EXUNIT_RXDT
         INTEGER        ::  EXUNIT_RXCM
         INTEGER        ::  MODULE_UNIT
         
         INTEGER,            ALLOCATABLE ::  CGRID_INDEX ( : )
         CHARACTER( 16),     ALLOCATABLE ::  CGRID_SPC   ( : )
         CHARACTER(LEN = 2), ALLOCATABLE ::  SPECIES_TYPE( : )
         
         INTEGER                       ::  NUMB_MECH_SPCS
         INTEGER ,        ALLOCATABLE ::  MECHANISM_INDEX( : )
         CHARACTER( 16 ), ALLOCATABLE ::  MECHANISM_SPC  ( : )


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
            
            
            
