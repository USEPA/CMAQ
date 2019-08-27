      SUBROUTINE WRSS_EXT( NR )
!     &                     N_SS_SPC, 
!     &                     SS_SPC, 
!     &                     MAX_SS_LOSS, 
!     &                     MAX_SS_PROD, 
!     &                     N_LOSS_RXNS, 
!     &                     N_PROD_RXNS, 
!     &                     SS_LOSS_RXNS, 
!     &                     SS_PROD_RXNS, 
!     &                     SS_PROD_COEF, 
!     &                     SS_RCT_IND )

C   Suboutine adds SS species data to RXCM and RXDT files

      USE MECHANISM_DATA
      
      IMPLICIT NONE

C INPUTS
      INTEGER, INTENT ( IN )         :: NR                                 ! No. of reactions
!      INTEGER, INTENT ( IN )         :: N_SS_SPC                           ! No. of input steady-state species
!      CHARACTER( 16 ), INTENT ( IN ) :: SS_SPC( MAXNLIST )                  ! List of input steady-state species
!      INTEGER, INTENT ( IN )         :: MAX_SS_LOSS          ! Max no of reactions for which 1 SS species
                                                               ! appears as a reactant
!      INTEGER, INTENT ( IN )         :: MAX_SS_PROD          ! Max no of reactions for which 1 SS species
                                                               ! appears as a product
!      INTEGER, INTENT ( IN )         :: N_LOSS_RXNS( MAXNLIST )             ! No. of loss rxns for each SS species
!      INTEGER, INTENT ( IN )         :: N_PROD_RXNS( MAXNLIST )             ! No. of prod rxns for each SS species
!      INTEGER, INTENT ( IN )         :: SS_LOSS_RXNS( MAXNLIST, MAXRXNUM )  ! List of rxns in which SS species is a reactant
!      INTEGER, INTENT ( IN )         :: SS_PROD_RXNS( MAXNLIST, MAXRXNUM )  ! List of rxns in which SS species is a product
!      INTEGER, INTENT ( IN )         :: SS_RCT_IND( MAXRXNUM )              ! Index of SS species that reacts 
!      REAL,    INTENT ( IN )         :: SS_PROD_COEF( MAXNLIST, MAXRXNUM )   ! Yields for rxns producing a SS species


C FUNCTIONS
      INTEGER, EXTERNAL :: JUNIT


C LOCAL VARIABLES

      CHARACTER(   1 ) :: XXX



      INTEGER SPC                   ! Loop index
      INTEGER IND                   ! Loop index
      INTEGER SS_SPC_DIM            ! Dimension parameter for steady-state species

      INTEGER         :: INTBUF( MAXRXNUM )   ! Temp buffer for integers
      REAL            :: REALBUF( MAXRXNUM )  ! Temp buffer for reals

      INTERFACE
        SUBROUTINE WRBF6 ( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
         END SUBROUTINE WRBF6 
      END INTERFACE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write RXCM data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

3300  IF( N_SS_SPC .EQ. 0 ) THEN
         SS_SPC_DIM = 1
      ELSE
         SS_SPC_DIM = N_SS_SPC
      ENDIF

      WRITE( EXUNIT_RXCM, 90000 )
      WRITE( EXUNIT_RXCM, 90020 ) N_SS_SPC
      WRITE( EXUNIT_RXCM, 90021 ) SS_SPC_DIM
      WRITE( EXUNIT_RXCM, 90022 ) MAX_SS_LOSS
      WRITE( EXUNIT_RXCM, 90023 )  MAX_SS_PROD

      IF( N_SS_SPC .EQ. 0 ) THEN
         WRITE( EXUNIT_RXCM, 90040 )
      ELSE
         WRITE( EXUNIT_RXCM, 90060 )
      ENDIF


      WRITE( EXUNIT_RXCM, 90080 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write RXDT data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( N_SS_SPC .GT. 0 ) THEN

c..write headers
         WRITE( EXUNIT_RXDT, 90100 )

c..write SS species list
         WRITE( EXUNIT_RXDT, 90120 )
         CALL WRBF16C ( EXUNIT_RXDT, 3, N_SS_SPC, SS_SPC )

c..write number of loss and production reactions for each SS species
         WRITE( EXUNIT_RXDT, 90140 )
         CALL WRBF6 ( EXUNIT_RXDT, 10, N_SS_SPC, N_LOSS_RXNS )

         WRITE( EXUNIT_RXDT, 90160 )
         CALL WRBF6 ( EXUNIT_RXDT, 10, N_SS_SPC, N_PROD_RXNS )

c..write list of loss reactions for each SS species
         DO SPC = 1, N_SS_SPC

            WRITE( EXUNIT_RXDT, 90180 ) SPC

            DO IND = 1, MAX_SS_LOSS
              INTBUF( IND ) = SS_LOSS_RXNS( SPC, IND )
            ENDDO 
               
            CALL WRBF6 ( EXUNIT_RXDT, 10, MAX_SS_LOSS, INTBUF )

         ENDDO

c..write list of production reactions for each SS species
         DO SPC = 1, N_SS_SPC

            WRITE( EXUNIT_RXDT, 90200 ) SPC

            DO IND = 1, MAX_SS_PROD
              INTBUF( IND ) = SS_PROD_RXNS( SPC, IND )
            ENDDO 
               
            CALL WRBF6 ( EXUNIT_RXDT, 10, MAX_SS_PROD, INTBUF )

         ENDDO

c..write list of yields for each SS species
         DO SPC = 1, N_SS_SPC

            WRITE( EXUNIT_RXDT, 90220 ) SPC

            DO IND = 1, MAX_SS_PROD
              REALBUF( IND ) = SS_PROD_COEF( SPC, IND )
            ENDDO 
               
            CALL WRBF12S ( EXUNIT_RXDT, 5, MAX_SS_PROD, REALBUF, 'F' )

         ENDDO

c..write list of SS species indices for all reactions in which the SS species
c..reacts with any other non-SS species
     
         WRITE( EXUNIT_RXDT, 90240 )

         CALL WRBF6 ( EXUNIT_RXDT, 10, NR, SS_RCT_IND )

      ENDIF

      RETURN
      
      
90000 FORMAT(// 'C    Steady-state species section'
     &       /  'C    N_SS_SPC     = Number of species assumed to be in steady-state'
     &       /  'C    SS_SPC_DIM   = Dimension paramete for steady-state species'
     &       /  'C    SS_SPC       = Names of species assumed to be in steady-state'
     &       /  'C    MAX_SS_LOSS  = Max no. of SS loss rxns for any SS species'
     &       /  'C    MAX_SS_PROD  = Max no. of SS prod rxns for any SS species'
     &       /  'C    N_LOSS_RXNS  = No. of SS loss rxns for each SS species'
     &       /  'C    N_PROD_RXNS  = No. of SS prod rxns for each SS species'
     &       /  'C    SS_LOSS_RXNS = List of SS loss rxns for each SS species'
     &       /  'C    SS_PROD_RXNS = List of SS prod rxns for each SS species'
     &       /  'C    SS_PROD_COEF = List of SS prod yields for each SS species' 
     &       /  'C    SS_RCT_IND   = SS species index if it is a rxn reactant' )

90020 FORMAT( /6X, 'INTEGER, PARAMETER :: N_SS_SPC = ',I3 )
90021 FORMAT( /6X, 'INTEGER, PARAMETER :: SS_SPC_DIM = ', I3)
90022 FORMAT( /6X, 'INTEGER, PARAMETER :: MAX_SS_LOSS = ', I3)
90023 FORMAT( /6X, 'INTEGER, PARAMETER :: MAX_SS_PROD = ', I3  )


90040 FORMAT( /6X, 'CHARACTER( 16 )    :: SS_SPC( 1 )' 
     &       //6X, 'INTEGER            :: N_LOSS_RXNS( 1 )'
     &        /6X, 'INTEGER            :: N_PROD_RXNS( 1 )'
     &        /6X, 'INTEGER            :: SS_LOSS_RXNS( 1, 1 )'
     &        /6X, 'INTEGER            :: SS_PROD_RXNS( 1, 1 )'
     &        /6X, 'INTEGER            :: SS_RCT_IND( 1 )'
     &       //6X, 'REAL               :: SS_PROD_COEF( 1,1 ) ' ) 

90060 FORMAT( /6X, 'CHARACTER( 16 )    :: SS_SPC( N_SS_SPC )'     
     &       //6X, 'INTEGER            :: N_LOSS_RXNS( N_SS_SPC )'
     &        /6X, 'INTEGER            :: N_PROD_RXNS( N_SS_SPC )'
     &        /6X, 'INTEGER            :: SS_LOSS_RXNS( N_SS_SPC, MAX_SS_LOSS )'
     &        /6X, 'INTEGER            :: SS_PROD_RXNS( N_SS_SPC, MAX_SS_PROD )'
     &        /6X, 'INTEGER            :: SS_RCT_IND( NRXNS )'
     &       //6X, 'REAL               :: SS_PROD_COEF( N_SS_SPC, MAX_SS_PROD ) ' ) 




90080 FORMAT( /5X, ' COMMON     / MECHRX5 /'
     &        /5X, '&             SS_SPC,'
     &        /5X, '&             N_LOSS_RXNS,'
     &        /5X, '&             N_PROD_RXNS,'
     &        /5X, '&             SS_LOSS_RXNS,'
     &        /5X, '&             SS_PROD_RXNS,'
     &        /5X, '&             SS_PROD_COEF,' 
     &        /5X, '&             SS_RCT_IND' )

90100 FORMAT( //'C   Steady-state species section' )
90120 FORMAT( //6X, 'DATA ( SS_SPC( IRXXN ), IRXXN = 1, N_SS_SPC ) /' )

90140 FORMAT( //6X, 'DATA ( N_LOSS_RXNS( IRXXN ), IRXXN = 1, N_SS_SPC ) /' )
90160 FORMAT( //6X, 'DATA ( N_PROD_RXNS( IRXXN ), IRXXN = 1, N_SS_SPC ) /' )
90180 FORMAT( //6X, 'DATA ( SS_LOSS_RXNS( ', I3, ', IRXXN ), IRXXN = 1, MAX_SS_LOSS ) /' )
90200 FORMAT( //6X, 'DATA ( SS_PROD_RXNS( ', I3, ', IRXXN ), IRXXN = 1, MAX_SS_PROD ) /' )
90220 FORMAT( //6X, 'DATA ( SS_PROD_COEF( ', I3, ', IRXXN ), IRXXN = 1, MAX_SS_PROD ) /' )
90240 FORMAT( //6X, 'DATA ( SS_RCT_IND( IRXXN ), IRXXN = 1, NRXNS ) /' )



      END


