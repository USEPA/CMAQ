      SUBROUTINE WRSS_EXT_FORTRAN90( WRUNIT, NR )

C   Suboutine adds SS species data to RXCM and RXDT files

      USE MECHANISM_DATA
      
      IMPLICIT NONE

C INPUTS
      INTEGER, INTENT(  IN )    ::  WRUNIT     ! logical write unit no.
      INTEGER, INTENT(  IN )    ::  NR         ! No. of reactions


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
        SUBROUTINE WRBF6( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
         END SUBROUTINE WRBF6
        SUBROUTINE WRBF6_FORTRAN90( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
        END SUBROUTINE WRBF6_FORTRAN90      
        SUBROUTINE WRBF12S ( WRUNIT, AWPL, NEL, VAR, AFMT )
           INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
           INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
           INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
           REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
           CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
        END SUBROUTINE WRBF12S
        SUBROUTINE WRBF12S_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR, AFMT )
           INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
           INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
           INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
           REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
           CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
        END SUBROUTINE WRBF12S_FORTRAN90
        SUBROUTINE WRBF16C_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR )
          INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
          INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
          INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
          CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
        END SUBROUTINE WRBF16C_FORTRAN90 
        SUBROUTINE WRBF16C ( WRUNIT, AWPL, NEL, VAR )
          INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
          INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
          INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
          CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
        END SUBROUTINE WRBF16C
      END INTERFACE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write RXCM data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

3300  IF( N_SS_SPC .EQ. 0 ) THEN
         SS_SPC_DIM = 1
      ELSE
         SS_SPC_DIM = N_SS_SPC
      ENDIF

      WRITE( WRUNIT, 90000 )
      WRITE( WRUNIT, 90020 ) N_SS_SPC
      WRITE( WRUNIT, 90021 ) SS_SPC_DIM
      WRITE( WRUNIT, 90022 ) MAX_SS_LOSS
      WRITE( WRUNIT, 90023 )  MAX_SS_PROD

      IF( N_SS_SPC .EQ. 0 ) THEN
         WRITE( WRUNIT, 90040 )
      ELSE
         WRITE( WRUNIT, 90060 )
      ENDIF


 !     WRITE( WRUNIT, 90080 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write RXDT data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( N_SS_SPC .GT. 0 ) THEN

c..write headers
         WRITE( WRUNIT, 90100 )

c..write SS species list
         WRITE( WRUNIT, 90120 )
         CALL WRBF16C_FORTRAN90 ( WRUNIT, 3, N_SS_SPC, SS_SPC )

c..write number of loss and production reactions for each SS species
         WRITE( WRUNIT, 90140 )
         CALL WRBF6_FORTRAN90 ( WRUNIT, 10, N_SS_SPC, N_LOSS_RXNS )

         WRITE( WRUNIT, 90160 )
         CALL WRBF6_FORTRAN90 ( WRUNIT, 10, N_SS_SPC, N_PROD_RXNS )

c..write list of loss reactions for each SS species
         DO SPC = 1, N_SS_SPC

            WRITE( WRUNIT, 90180 ) SPC

            DO IND = 1, MAX_SS_LOSS
              INTBUF( IND ) = SS_LOSS_RXNS( SPC, IND )
            ENDDO 
               
            CALL WRBF6_FORTRAN90 ( WRUNIT, 10, MAX_SS_LOSS, INTBUF )

         ENDDO

c..write list of production reactions for each SS species
         DO SPC = 1, N_SS_SPC

            WRITE( WRUNIT, 90200 ) SPC

            DO IND = 1, MAX_SS_PROD
              INTBUF( IND ) = SS_PROD_RXNS( SPC, IND )
            ENDDO 
               
            CALL WRBF6_FORTRAN90 ( WRUNIT, 10, MAX_SS_PROD, INTBUF )

         ENDDO

c..write list of yields for each SS species
         DO SPC = 1, N_SS_SPC

            WRITE( WRUNIT, 90220 ) SPC

            DO IND = 1, MAX_SS_PROD
              REALBUF( IND ) = SS_PROD_COEF( SPC, IND )
            ENDDO 
               
            CALL WRBF12S_FORTRAN90 ( WRUNIT, 5, MAX_SS_PROD, REALBUF, 'F' )

         ENDDO

c..write list of SS species indices for all reactions in which the SS species
c..reacts with any other non-SS species
     
         WRITE( WRUNIT, 90240 )

         CALL WRBF6_FORTRAN90 ( WRUNIT, 10, NR, SS_RCT_IND )

      ENDIF

      RETURN
      
      
90000 FORMAT(// '!    Steady-state species section'
     &       /  '!    N_SS_SPC     = Number of species assumed to be in steady-state'
     &       /  '!    SS_SPC_DIM   = Dimension paramete for steady-state species'
     &       /  '!    SS_SPC       = Names of species assumed to be in steady-state'
     &       /  '!    MAX_SS_LOSS  = Max no. of SS loss rxns for any SS species'
     &       /  '!    MAX_SS_PROD  = Max no. of SS prod rxns for any SS species'
     &       /  '!    N_LOSS_RXNS  = No. of SS loss rxns for each SS species'
     &       /  '!    N_PROD_RXNS  = No. of SS prod rxns for each SS species'
     &       /  '!    SS_LOSS_RXNS = List of SS loss rxns for each SS species'
     &       /  '!    SS_PROD_RXNS = List of SS prod rxns for each SS species'
     &       /  '!    SS_PROD_COEF = List of SS prod yields for each SS species' 
     &       /  '!    SS_RCT_IND   = SS species index if it is a rxn reactant' )

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

90100 FORMAT( //'!   Steady-state species section' )
90120 FORMAT( //6X, 'DATA ( SS_SPC( IRXXN ), IRXXN = 1, N_SS_SPC ) /' )

90140 FORMAT( //6X, 'DATA ( N_LOSS_RXNS( IRXXN ), IRXXN = 1, N_SS_SPC ) /' )
90160 FORMAT( //6X, 'DATA ( N_PROD_RXNS( IRXXN ), IRXXN = 1, N_SS_SPC ) /' )
90180 FORMAT( //6X, 'DATA ( SS_LOSS_RXNS( ', I3, ', IRXXN ), IRXXN = 1, MAX_SS_LOSS ) /' )
90200 FORMAT( //6X, 'DATA ( SS_PROD_RXNS( ', I3, ', IRXXN ), IRXXN = 1, MAX_SS_PROD ) /' )
90220 FORMAT( //6X, 'DATA ( SS_PROD_COEF( ', I3, ', IRXXN ), IRXXN = 1, MAX_SS_PROD ) /' )
90240 FORMAT( //6X, 'DATA ( SS_RCT_IND( IRXXN ), IRXXN = 1, NRXNS ) /' )



      END


