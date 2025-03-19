
       PROGRAM CR_EBI_SOLVER

C**********************************************************************
C
C  FUNCTION: Create source code for mechanism specific EBI solver
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C                    Modified 06/09 by J. Gipson to include steady-state
C                      approximations and special rate constants
C
C**********************************************************************
      USE ENV_VARS
      USE GLOBAL_DATA
!      USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA

      IMPLICIT NONE

C..INCLUDES:

               
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None
 
C..LOCAL VARIABLES:
      CHARACTER( 16 )  ::    PNAME = 'CR_EBI_SOLVER'
      CHARACTER( 80 )  ::    MSG

      INTEGER  :: N            ! Loop index


C**********************************************************************


      CALL GET_ENVS

      CALL SET_GLBLDATA

      CALL ERR_CHECK

      CALL WR_DRIVER

!     CALL WR_CALCKS

      CALL WR_SOLVER

      CALL WR_DATA

      CALL WR_INIT

      CALL WR_GP1

      CALL WR_GP2

      CALL WR_GP3

      CALL WR_GP4

      CALL WR_PRODLOSS

!     IF( NSPECIAL .GT. 0 ) CALL WR_SPECIAL

      IF( N_SS_SPC .GT. 0 ) THEN
          MSG = TRIM( PNAME )
     &       // ': ERROR: Mechanism uses steady state or equilibrium '
     &       // 'solution for specific species. The utility is '
     &       // 'disabled in the version of the EBI code generator.' 
          WRITE(LOGDEV,'(a)')TRIM(MSG)
          STOP
!         CALL WR_SSA
      END IF

c..This subroutine needs to be called after all other to insure that
c..all references to rxrat are properly accounted for
      CALL WR_RATES


  
      WRITE( LOGDEV, 90000 ) NOUTFLS

      DO N = 1, NOUTFLS
         WRITE( LOGDEV, 90020 ) OUTFLNAM( N )
      END DO    

      MSG = 'Program ' // PNAME( 1 : LEN_TRIM( PNAME ) ) // 
     &      ' completed successfully'

      WRITE(LOGDEV,'(a)')TRIM(MSG)
      STOP

90000 FORMAT( /'The following ', I2, ' output files were created:' )
90020 FORMAT( 5X, A )
      END

