      SUBROUTINE SET_GLBLDATA

C**********************************************************************
C
C  FUNCTION: Sets mechansim global data used by other subroutines
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C                      Modified Nov 2004 by JG to use input group names
C                      Modified Dec 2007 by Golam Sarwar to change RTOL for *RXN variables 
C                      Modified Nov 2009 by JG to add SS rxn flag
C**********************************************************************
      USE ENV_VARS
      USE GLOBAL_DATA
     !! USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA


      IMPLICIT NONE

C..INCLUDES: 

      
C..ARGUMENTS: None

C..PARAMETERS:
      INTEGER, PARAMETER :: MAX_GRP_SPC   = 20  ! Max no. species in group list
      INTEGER, PARAMETER :: N_LOW_TOL_SPC = 28  ! No. species w/ low tols, revised by Golam Sarwar
      INTEGER, PARAMETER :: N_HII_TOL_SPC =  4  ! No. species w/ strict rtols, revised by Golam Sarwar

C..EXTERNAL FUNCTIONS:
!      INTEGER    NAME_INDEX            !

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::  PNAME = 'SET_GLBLDATA'    ! Program name
      CHARACTER(  16 )  ::  GRP_SPC( MAX_GRP_SPC )
      CHARACTER( 100 )  ::  MSG 
      CHARACTER(  16 )  ::  LOW_TOL_SPC( N_LOW_TOL_SPC )
      CHARACTER(  16 )  ::  HII_TOL_SPC( N_HII_TOL_SPC ) = 
     &                      (/ 'I   ', 'IO  ', 'HOI ', 'INO3' /)

      INTEGER   ::  EOS           ! End of string position
      INTEGER   ::  N             ! Loop index
      INTEGER   ::  IND
      INTEGER   ::  N_GRP_SPC     ! No. of group species

      LOGICAL   ::  LERROR        ! Error flag

C**********************************************************************
C *** revised by Golam Sarwar
C      DATA LOW_TOL_SPC / 'O'     , 'O1D'     , 'NTR'   , 'SULF',   !cb4
C     &                   'O3P'   , 'O1D2'    , 'SULF'  , 'BZ_O',   !saprc99
C     &                   'TBU_O' , 'BZNO2_O' , 'HOCOO' ,           !saprc99
C     &                   'O3P'   , 'O1D'     , 'SULF'  /           !radm2

      DATA LOW_TOL_SPC / 'O'      , 'O1D'      , 'NTR'    , 'SULF'   , !cb05
     &                   'TOLNRXN', 'TOLHRXN'  , 'XYLNRXN', 'XYLHRXN', !cb05
     &                   'ISOPRXN', 'TRPRXN '  , 'XYLNRXN', 'XYLHRXN', !cb05
     &                   'BNZNRXN', 'BNZHRXN ' , 'SESQRXN', 'SULRXN' , !cb05
     &                   'HCO3'   ,                                    !cb05     
     &                   'O3P'    , 'O1D2'     , 'BZ_O'   , 'TBU_O'  , !saprc99
     &                   'BZNO2_O', 'HOCOO'    , 'ALK5RXN',
     &                   'ALKRXN' , 'PAHRO2'   , 'PAHHRXN', 'PAHNRXN' / !  version 5.1 SOA production

C set up SPECIES array

      N_SPECIES = NUMB_MECH_SPC
      ALLOCATE( SPECIES( N_SPECIES + 1 ) )
      SPECIES = ' '
      DO N = 1, N_SPECIES
         SPECIES( N ) =  CHEMISTRY_SPC( N )
      END DO
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get group species indices
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      N = 0

c..NO
      NO  = NAME_INDEX( MECH_NO, N_SPECIES, SPECIES )
      IF( NO .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_NO
      ELSE
         EOS = LEN_TRIM( MECH_NO )
         MSG = 'ERROR: MECH_NO Group species ' // MECH_NO( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..NO2
      NO2 = NAME_INDEX( MECH_NO2, N_SPECIES, SPECIES )
      IF( NO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_NO2
      ELSE
        EOS = LEN_TRIM( MECH_NO2 )
        MSG = 'ERROR: MECH_NO2 Group species ' // MECH_NO2( 1 : EOS ) //
     &        ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..O3
      O3  = NAME_INDEX( MECH_O3,  N_SPECIES, SPECIES )
      IF( NO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_O3
      ELSE
         EOS = LEN_TRIM( MECH_O3 )
         MSG = 'ERROR: MECH_O3 Group species ' // MECH_O3( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..O3P
      O3P = NAME_INDEX( MECH_O3P, N_SPECIES, SPECIES )
      IF( O3P .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_O3P
      ELSE
         EOS = LEN_TRIM( MECH_O3P )
         MSG = 'ERROR: ECH_O3P Group species ' // MECH_O3P( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..O1D
      O1D = NAME_INDEX( MECH_O1D, N_SPECIES, SPECIES )
      IF( O1D .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_O1D
      ELSE
         EOS = LEN_TRIM( MECH_O1D )
         MSG = 'ERROR: MECH_O1D Group species value: ' // MECH_O1D( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..OH
      OH = NAME_INDEX( MECH_OH,  N_SPECIES, SPECIES )
      IF( OH .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_OH
      ELSE
         EOS = LEN_TRIM( MECH_OH )
         MSG = 'ERROR: MECH_OH Group species ' // MECH_OH( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..HO2
      HO2   = NAME_INDEX( MECH_HO2 , N_SPECIES, SPECIES )
      IF( HO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_HO2
      ELSE
         EOS = LEN_TRIM( MECH_HO2 )
         MSG = 'ERROR: MECH_HO2 Group species ' // MECH_HO2( 1 : EOS ) //
     &         ' not found in mechanism' 
         LERROR = .TRUE.
      END IF

c..HONO
      HONO  = NAME_INDEX( MECH_HONO, N_SPECIES, SPECIES )
      IF( HONO .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_HONO
      ELSE
         EOS = LEN_TRIM( MECH_HONO )
         MSG = 'ERROR: MECH_HONO Group species ' // MECH_HONO( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..HNO4
      HNO4  = NAME_INDEX( MECH_HNO4, N_SPECIES, SPECIES )
      IF( HNO4 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_HNO4
      ELSE
         EOS = LEN_TRIM( MECH_HNO4 )
         MSG = 'ERROR: MECH_HNO4 Group species ' // MECH_HNO4( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

C..C2O3
      C2O3  = NAME_INDEX( MECH_C2O3, N_SPECIES, SPECIES )
      IF( C2O3 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_C2O3
      ELSE
         EOS = LEN_TRIM( MECH_C2O3 )
         MSG = 'ERROR: MECH_C2O3 Group species ' // MECH_C2O3( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

C..PAN
      PAN   = NAME_INDEX( MECH_PAN,  N_SPECIES, SPECIES )
      IF( PAN .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_PAN
      ELSE
         EOS = LEN_TRIM( MECH_PAN )
         MSG = 'ERROR: MECH_PAN Group species ' // MECH_PAN( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..NO3
      NO3   = NAME_INDEX( MECH_NO3,  N_SPECIES, SPECIES )
      IF( NO3 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_NO3
      ELSE
         EOS = LEN_TRIM( MECH_NO3 )
         MSG = 'ERROR: MECH_NO3 Group species ' // MECH_NO3( 1 : EOS ) //
     &         ' not found in mechanism' 
         LERROR = .TRUE.
      END IF
c..N2O5
      N2O5  = NAME_INDEX( MECH_N2O5, N_SPECIES, SPECIES )
      IF( N2O5 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_N2O5
      ELSE
         EOS = LEN_TRIM( MECH_N2O5 )
         MSG = 'ERROR: MECH_N2O5 Group species ' // MECH_N2O5( 1 : EOS ) //
     &         ' not found in mechanism' 
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         LERROR = .TRUE.
      END IF

c..NO2EX
      IF( LNO2EX_CYCLE ) THEN
         NO2EX  = NAME_INDEX( MECH_NO2EX, N_SPECIES, SPECIES )
         IF( NO2EX .NE. 0 ) THEN
            N = N +1
            GRP_SPC( N ) = MECH_NO2EX
         ELSE
            EOS = LEN_TRIM( MECH_NO2EX )
            MSG = 'ERROR: MECH_NO2EXGroup species ' // MECH_NO2EX( 1 : EOS ) //
     &         ' not found in mechanism' 
            WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
            LERROR = .TRUE.
         END IF
      END IF

      N_GRP_SPC = N
      IF( LERROR ) THEN
        MSG = 'Stopping because of undefined group species name(s)'
        WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
        STOP
      END IF


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c If doing all mechanisms, get a list of species & find unique AE/AQ
c species. Note three subroutines are needed because the include files 
c all use the same variable names (i.e., N_SPECIES & SPECIES) 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      N_AE_SPC = 0
      N_AQ_SPC = 0

c..Flag species in SPECIES as gas-phase only or not ( i.e. AE or AQ)
      ALLOCATE( L_GC_ONLY_SPC( N_SPECIES ) )
      IF( ALLOCATED( AE_SPC ) .OR. ALLOCATED( AQ_SPC ) )THEN
          DO N = 1, N_SPECIES
             L_GC_ONLY_SPC( N ) = .TRUE.
             IF( NAME_INDEX( SPECIES( N ), N_AE_SPC, AE_SPC ) .NE. 0 .OR.
     &           NAME_INDEX( SPECIES( N ), N_AQ_SPC, AQ_SPC ) .NE. 0 )
     &           L_GC_ONLY_SPC( N ) = .FALSE.
          END DO
      ELSE
         L_GC_ONLY_SPC  = .TRUE.
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Find the max char lengths
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       DO N = 1, N_SPECIES

         CL = MAX( CL, LEN_TRIM( SPECIES( N ) ) )

       END DO  

       DO N = 1, NRXNS
          LL = MAX( LL, LEN_TRIM( RXLABEL( N ) ) )       
       END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Tag each species as EBI or group
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( L_GC_EBI( N_SPECIES ) )
      L_GC_EBI = .FALSE.

      DO N = 1, N_SPECIES
         IF( NAME_INDEX( SPECIES( N ), N_GRP_SPC, GRP_SPC ) .EQ. 0 ) THEN
             N_GC_EBI = N_GC_EBI + 1
             L_GC_EBI( N ) = .TRUE.
         END IF
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Tag gas-only species as low or high tolerance
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         ALLOCATE( L_LOW_TOL_SPC( N_SPECIES ) )
         L_LOW_TOL_SPC = .FALSE.
         ALLOCATE(L_HII_TOL_SPC( N_SPECIES ) )
         L_HII_TOL_SPC = .FALSE.

         DO N = 1, N_SPECIES
            IF( NAME_INDEX( SPECIES(N),N_LOW_TOL_SPC,LOW_TOL_SPC ) .NE. 0 ) THEN
                L_LOW_TOL_SPC( N ) = .TRUE.
            END IF
         END DO

         DO N = 1, N_SPECIES
            IF( NAME_INDEX( SPECIES(N),N_HII_TOL_SPC,HII_TOL_SPC ) .NE. 0 ) THEN
                L_HII_TOL_SPC( N ) = .TRUE.
            END IF
         END DO

c..Tag NO2EX as low tol if cycle is in group 1 and 2
!        IF( LNO2EX_CYCLE ) L_LOW_TOL_SPC( NO2EX ) = .TRUE.
         IF( LNO2EX_CYCLE )THEN
             L_HII_TOL_SPC( NO    ) = .TRUE.
             L_HII_TOL_SPC( NO2   ) = .TRUE.
             L_HII_TOL_SPC( NO2EX ) = .TRUE.
          END IF
         



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set reaction rate flags - indicates RXRAT reference
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( L_RXFLAG( NRXNS ) )
      L_RXFLAG = .FALSE.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set SS reactannt flags 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( L_SS_RXN_FLAG( NRXNS ) )
      L_SS_RXN_FLAG = .FALSE.

      RETURN

      END


      SUBROUTINE GET_AE_SPC

C**********************************************************************
C
C  FUNCTION: Get a list of all species in the AE version of the mechanism
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C
C**********************************************************************
      USE GLOBAL_DATA

      IMPLICIT NONE

C..INCLUDES: 
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      INTEGER   ::  N    ! Loop index

C**********************************************************************

      N_GC_AE_SPC = N_SPECIES

      ALLOCATE( GC_AE_SPC( N_SPECIES ) )

      DO N = 1, N_SPECIES

         GC_AE_SPC( N ) = SPECIES( N )

      END DO

      RETURN

      END




      SUBROUTINE GET_AQ_SPC

C**********************************************************************
C
C  FUNCTION: Get a list of all species in the AQ version of the mechanism
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C
C**********************************************************************
      USE GLOBAL_DATA

      IMPLICIT NONE

C..INCLUDES: 
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      INTEGER   ::  N             ! Loop index

C**********************************************************************

      N_GC_AQ_SPC = N_SPECIES

      ALLOCATE( GC_AQ_SPC( N_SPECIES ) )

      DO N = 1, N_SPECIES

         GC_AQ_SPC( N ) = SPECIES( N )

      END DO

      RETURN

      END

      SUBROUTINE GET_GC_ONLY_SPC

C**********************************************************************
C
C  FUNCTION: Get a list of all species in the AE_AQ version of the mechanism
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C
C**********************************************************************
      USE GLOBAL_DATA

      IMPLICIT NONE

C..INCLUDES: 
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      INTEGER   ::  N    ! Loop index

C**********************************************************************

      N_GC_ONLY_SPC = N_SPECIES

      ALLOCATE( GC_ONLY_SPC( N_SPECIES ) )

      DO N = 1, N_SPECIES

         GC_ONLY_SPC( N ) = SPECIES( N )

      END DO

      RETURN

      END

