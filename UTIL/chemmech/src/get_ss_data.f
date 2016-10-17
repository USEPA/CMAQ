C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_SS_DATA ( LUNOUT, NR )
!     &                         N_SS_SPC, 
!     &                         SS_SPC, 
!     &                         SS_RCT_COEF,
!     &                         SS_PRD_COEF, 
!     &                         MAX_SS_LOSS, 
!     &                         MAX_SS_PROD, 
!     &                         N_LOSS_RXNS, 
!     &                         N_PROD_RXNS, 
!     &                         SS_LOSS_RXNS,
!     &                         SS_PROD_RXNS, 
!     &                         SS_PROD_COEF, 
!     &                         SS_RCT_IND )

C=======================================================================
C Formulates data needed by chemistry solvers to compute concentrations
C of steady-state species and adjust reaction rates correspondingly
C=======================================================================


      USE MECHANISM_DATA
      
      IMPLICIT NONE

C..Input Arguments
      INTEGER, INTENT ( IN )         :: LUNOUT                             ! Output unit number
      INTEGER, INTENT ( IN )         :: NR                                 ! No. of reactions
!      INTEGER, INTENT ( IN )         :: N_SS_SPC                           ! No. of input steady-state species
!      CHARACTER( 16 ), INTENT ( IN ) :: SS_SPC( MAXNLIST )                  ! List of input steady-state species
!      INTEGER, INTENT ( INOUT )      ::  SS_RCT_COEF( MAXNLIST, MAXRXNUM )  ! coefficients for SS reactants
!      REAL,    INTENT ( INOUT )      ::  SS_PRD_COEF( MAXNLIST, MAXRXNUM )  ! coefficients for SS products
!      INTEGER, INTENT ( INOUT )      ::  MAX_SS_LOSS          ! Max no of reactions for which 1 SS species
                                                               ! appears as a reactant
!      INTEGER, INTENT ( INOUT )      ::  MAX_SS_PROD          ! Max no of reactions for which 1 SS species
                                                               ! appears as a product
!      INTEGER, INTENT ( INOUT )      ::  N_LOSS_RXNS( MAXNLIST )             ! No. of loss rxns for each SS species
!      INTEGER, INTENT ( INOUT )      ::  N_PROD_RXNS( MAXNLIST )             ! No. of prod rxns for each SS species
!      INTEGER, INTENT ( INOUT )      ::  SS_LOSS_RXNS( MAXNLIST, MAXRXNUM )  ! List of rxns in which SS species is a reactant
!      INTEGER, INTENT ( INOUT )      ::  SS_PROD_RXNS( MAXNLIST, MAXRXNUM )  ! List of rxns in which SS species is a product
!      INTEGER, INTENT ( INOUT )      ::  SS_RCT_IND( MAXRXNUM )              ! Index of SS species that reacts 
!      REAL,    INTENT ( INOUT )      ::  SS_PROD_COEF( MAXNLIST, MAXRXNUM )  ! Yields for rxns producing a SS species

c..Local variables

      INTEGER           :: SS1           ! Loop index
      INTEGER           :: NRX           ! Loop index
      INTEGER           :: IRCT          ! Counter
      INTEGER           :: IPRD          ! Counter
     

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  For each SS species, loop through reactions and build arrays containing reaction number
c  indices and product coefficients for each SS species 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..Initialize arrays
      SS_LOSS_RXNS = 0
      SS_PROD_RXNS = 0
      SS_PROD_COEF = 0.0

      MAX_SS_LOSS = 0
      MAX_SS_PROD = 0


c..Fill em up
      DO SS1 = 1, N_SS_SPC

         IRCT = 0
         IPRD = 0

         DO NRX = 1, NR

            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0   ) THEN
               IRCT = IRCT + 1
               SS_LOSS_RXNS( SS1, IRCT ) = NRX
            ENDIF

            IF( SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN
               IPRD = IPRD + 1
               SS_PROD_RXNS( SS1, IPRD ) = NRX
               SS_PROD_COEF(  SS1, IPRD ) = SS_PRD_COEF( SS1, NRX )
            ENDIF
 
         ENDDO

         N_LOSS_RXNS( SS1 ) = IRCT
         N_PROD_RXNS( SS1 ) = IPRD

         MAX_SS_LOSS = MAX( MAX_SS_LOSS, N_LOSS_RXNS( SS1 ) ) 
         MAX_SS_PROD = MAX( MAX_SS_PROD, N_PROD_RXNS( SS1 ) ) 

      ENDDO

c..Identify all reactions in which a SS species reacts with other reactants; 
c..Load index of the SS species reacting in each reaction (note: can only be one SS
c..species per rxn - SS species not allowed to react with other SS species)

      DO NRX = 1, NR

         DO SS1 = 1, N_SS_SPC
            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0 .AND. NREACT( NRX ) .GT. 0 )
     &          SS_RCT_IND( NRX ) = SS1
         ENDDO 

      ENDDO

      RETURN

      END

