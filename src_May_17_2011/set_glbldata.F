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
      USE M3UTILIO ! IOAPI parameters and declarations


      IMPLICIT NONE

C..INCLUDES: 
!      INCLUDE SUBST_IOPARMS        ! IOAPI parameters
!      INCLUDE SUBST_IODECL         ! IOAPI declarations
      INCLUDE SUBST_RXCMMN         ! Mechanism data
      INCLUDE SUBST_RXDATA         ! Mechanism data
      
C..ARGUMENTS: None

C..PARAMETERS:
      INTEGER, PARAMETER :: MAX_GRP_SPC = 20    ! Max no. species in group list
C     INTEGER, PARAMETER :: N_LOW_TOL_SPC = 14  ! No. species w/ low tols
      INTEGER, PARAMETER :: N_LOW_TOL_SPC = 24  ! No. species w/ low tols, revised by Golam Sarwar

C..EXTERNAL FUNCTIONS:
!      INTEGER    INDEX1            !

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::  PNAME = 'SET_GLBLDATA'    ! Program name
      CHARACTER(  16 )  ::  GRP_SPC( MAX_GRP_SPC )
      CHARACTER(  16 )  ::  LOW_TOL_SPC( N_LOW_TOL_SPC )
      CHARACTER( 100 )  ::  MSG 

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
     &                   'BZNO2_O', 'HOCOO'    , 'ALK5RXN'/            !saprc99    

C set up GC_SPC array

      N_GC_SPC = N_GAS_CHEM_SPC
      ALLOCATE( GC_SPC( N_GC_SPC + 1 ) )
      GC_SPC = ' '
      DO N = 1, N_GC_SPC
         GC_SPC( N ) =  GAS_CHEM_SPC( N )
      END DO
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get group species indices
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      N = 0

c..NO
      NO  = INDEX1( MECH_NO, N_GC_SPC, GC_SPC )
      IF( NO .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_NO
      ELSE
         EOS = LEN_TRIM( MECH_NO )
         MSG = 'ERROR: Group species ' // MECH_NO( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..NO2
      NO2 = INDEX1( MECH_NO2, N_GC_SPC, GC_SPC )
      IF( NO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_NO2
      ELSE
        EOS = LEN_TRIM( MECH_NO2 )
        MSG = 'ERROR: Group species ' // MECH_NO2( 1 : EOS ) //
     &        ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..O3
      O3  = INDEX1( MECH_O3,  N_GC_SPC, GC_SPC )
      IF( NO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_O3
      ELSE
         EOS = LEN_TRIM( MECH_O3 )
         MSG = 'ERROR: Group species ' // MECH_O3( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..O3P
      O3P = INDEX1( MECH_O3P, N_GC_SPC, GC_SPC )
      IF( O3P .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_O3P
      ELSE
         EOS = LEN_TRIM( MECH_O3P )
         MSG = 'ERROR: Group species ' // MECH_O3P( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..O1D
      O1D = INDEX1( MECH_O1D, N_GC_SPC, GC_SPC )
      IF( O1D .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_O1D
      ELSE
         EOS = LEN_TRIM( MECH_O1D )
         MSG = 'ERROR: Group species ' // MECH_O1D( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..OH
      OH = INDEX1( MECH_OH,  N_GC_SPC, GC_SPC )
      IF( OH .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_OH
      ELSE
         EOS = LEN_TRIM( MECH_OH )
         MSG = 'ERROR: Group species ' // MECH_O1D( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..HO2
      HO2   = INDEX1( MECH_HO2 , N_GC_SPC, GC_SPC )
      IF( HO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_HO2
      ELSE
         EOS = LEN_TRIM( MECH_HO2 )
         MSG = 'ERROR: Group species ' // MECH_HO2( 1 : EOS ) //
     &         ' not found in mechanism' 
         LERROR = .TRUE.
      END IF

c..HONO
      HONO  = INDEX1( MECH_HONO, N_GC_SPC, GC_SPC )
      IF( HO2 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_HONO
      ELSE
         EOS = LEN_TRIM( MECH_HONO )
         MSG = 'ERROR: Group species ' // MECH_HONO( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..HNO4
      HNO4  = INDEX1( MECH_HNO4, N_GC_SPC, GC_SPC )
      IF( HNO4 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_HNO4
      ELSE
         EOS = LEN_TRIM( MECH_HNO4 )
         MSG = 'ERROR: Group species ' // MECH_HNO4( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

C..C2O3
      C2O3  = INDEX1( MECH_C2O3, N_GC_SPC, GC_SPC )
      IF( C2O3 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_C2O3
      ELSE
         EOS = LEN_TRIM( MECH_C2O3 )
         MSG = 'ERROR: Group species ' // MECH_C2O3( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

C..PAN
      PAN   = INDEX1( MECH_PAN,  N_GC_SPC, GC_SPC )
      IF( PAN .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_PAN
      ELSE
         EOS = LEN_TRIM( MECH_PAN )
         MSG = 'ERROR: Group species ' // MECH_PAN( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..NO3
      NO3   = INDEX1( MECH_NO3,  N_GC_SPC, GC_SPC )
      IF( NO3 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_NO3
      ELSE
         EOS = LEN_TRIM( MECH_NO3 )
         MSG = 'ERROR: Group species ' // MECH_NO3( 1 : EOS ) //
     &         ' not found in mechanism' 
         LERROR = .TRUE.
      END IF
c..N2O5
      N2O5  = INDEX1( MECH_N2O5, N_GC_SPC, GC_SPC )
      IF( N2O5 .NE. 0 ) THEN
         N = N +1
         GRP_SPC( N ) = MECH_N2O5
      ELSE
         EOS = LEN_TRIM( MECH_N2O5 )
         MSG = 'ERROR: Group species ' // MECH_N2O5( 1 : EOS ) //
     &         ' not found in mechanism' 
         CALL M3MESG( MSG )
         LERROR = .TRUE.
      END IF

c..NO2EX
      IF( LNO2EX_CYCLE ) THEN
         NO2EX  = INDEX1( MECH_NO2EX, N_GC_SPC, GC_SPC )
         IF( NO2EX .NE. 0 ) THEN
            N = N +1
            GRP_SPC( N ) = MECH_NO2EX
         ELSE
            EOS = LEN_TRIM( MECH_NO2EX )
            MSG = 'ERROR: Group species ' // MECH_NO2EX( 1 : EOS ) //
     &         ' not found in mechanism' 
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF
      END IF

      N_GRP_SPC = N
      IF( LERROR ) THEN
        MSG = 'Stopping because of undefined group species name(s)'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
      END IF


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c If doing all mechanisms, get a list of species & find unique AE/AQ
c species. Note three subroutines are needed because the include files 
c all use the same variable names (i.e., N_GC_SPC & GC_SPC) 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      IF( L_ALL_MECHS ) THEN

c..load species lists for each mechansim version
         CALL GET_AE_SPC
         CALL GET_AQ_SPC
         CALL GET_GC_ONLY_SPC

c..Get a list of the species that are only in the AE mechanism

         DO N = 1, N_GC_AE_SPC 

           IF( INDEX1( GC_AE_SPC( N ), N_GC_ONLY_SPC, GC_ONLY_SPC ) .EQ. 0 ) 
     &        N_AE_SPC = N_AE_SPC + 1 
           
         END DO

         ALLOCATE( AE_SPC( N_AE_SPC ) )

         IND = 0
         DO N = 1, N_GC_AE_SPC 

           IF( INDEX1( GC_AE_SPC( N ), N_GC_ONLY_SPC, GC_ONLY_SPC ) 
     &         .EQ. 0 )THEN 
              IND = IND + 1
              AE_SPC( IND ) = GC_AE_SPC( N )
           END IF
           
         END DO

c..Get a list of the species that are only in the AQ mechanism
         DO N = 1, N_GC_AQ_SPC 

           IF( INDEX1( GC_AQ_SPC( N ), N_GC_ONLY_SPC, GC_ONLY_SPC ) .EQ. 0 ) 
     &        N_AQ_SPC = N_AQ_SPC + 1
           
         END DO

         ALLOCATE( AQ_SPC( N_AQ_SPC ) )

         IND = 0
         DO N = 1, N_GC_AQ_SPC 

           IF( INDEX1( GC_AQ_SPC( N ), N_GC_ONLY_SPC, GC_ONLY_SPC )
     &         .EQ. 0 ) THEN 
              IND = IND + 1
              AQ_SPC( IND ) = GC_AQ_SPC( N )
           END IF
           
         END DO

      END IF

c..Flag species in GC_SPC as gas-phase only or not ( i.e. AE or AQ)
      ALLOCATE( L_GC_ONLY_SPC( N_GC_SPC ) )
      DO N = 1, N_GC_SPC
         L_GC_ONLY_SPC( N ) = .TRUE.
         IF( INDEX1( GC_SPC( N ), N_AE_SPC, AE_SPC ) .NE. 0 .OR.
     &       INDEX1( GC_SPC( N ), N_AQ_SPC, AQ_SPC ) .NE. 0 )
     &      L_GC_ONLY_SPC( N ) = .FALSE.
      END DO
 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Find the max char lengths
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      IF( L_ALL_MECHS ) THEN

         DO N = 1, N_GC_SPC

            CL = MAX( CL, LEN_TRIM( GC_SPC( N ) ) )

         END DO  

c      ELSE

         DO N = 1, N_GC_SPC

            CL = MAX( CL, LEN_TRIM( GC_SPC( N ) ) )

         END DO  

c      END IF

       DO N = 1, NRXNS
          LL = MAX( LL, LEN_TRIM( RXLABEL( N ) ) )       
       END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Tag each species as EBI or group
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( L_GC_EBI( N_GC_SPC ) )
      L_GC_EBI = .FALSE.

      DO N = 1, N_GC_SPC
         IF( INDEX1( GC_SPC( N ), N_GRP_SPC, GRP_SPC ) .EQ. 0 ) THEN
             N_GC_EBI = N_GC_EBI + 1
             L_GC_EBI( N ) = .TRUE.
         END IF
      END DO

      IF( L_ALL_MECHS ) THEN

         ALLOCATE( L_GC_AE_EBI( N_GC_AE_SPC ) )
         ALLOCATE( L_GC_AQ_EBI( N_GC_AQ_SPC ) )
         ALLOCATE( L_GC_ONLY_EBI( N_GC_ONLY_SPC ) )
         
         L_GC_AE_EBI = .FALSE.
         L_GC_AQ_EBI = .FALSE.
         L_GC_ONLY_EBI = .FALSE.
         
         DO N = 1, N_GC_AE_SPC
            IF( INDEX1( GC_AE_SPC( N ), N_GRP_SPC, GRP_SPC ) .EQ. 0 ) THEN
                N_GC_AE_EBI = N_GC_AE_EBI + 1
                L_GC_AE_EBI( N ) = .TRUE.
            END IF
         END DO

         DO N = 1, N_GC_AQ_SPC
            IF( INDEX1( GC_AQ_SPC( N ), N_GRP_SPC, GRP_SPC ) .EQ. 0 ) THEN
                N_GC_AQ_EBI = N_GC_AQ_EBI + 1
                L_GC_AQ_EBI( N ) = .TRUE.
            END IF
         END DO

         DO N = 1, N_GC_ONLY_SPC
            IF( INDEX1( GC_ONLY_SPC( N ), N_GRP_SPC, GRP_SPC ) .EQ. 0 ) THEN
                N_GC_ONLY_EBI = N_GC_ONLY_EBI + 1
                L_GC_ONLY_EBI( N ) = .TRUE.
            END IF
         END DO


      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Tag gas-only species as low or high tolerance
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( L_ALL_MECHS ) THEN

         ALLOCATE( L_LOW_TOL_SPC( N_GC_ONLY_SPC ) )
         ALLOCATE( L_HII_TOL_SPC( N_GC_ONLY_SPC ) )
         L_LOW_TOL_SPC = .FALSE.

         DO N = 1, N_GC_ONLY_SPC
            IF( INDEX1( GC_ONLY_SPC( N ), N_LOW_TOL_SPC, LOW_TOL_SPC )
     &          .NE. 0 ) THEN
                L_LOW_TOL_SPC( N ) = .TRUE.
            END IF
         END DO

      ELSE

         ALLOCATE( L_LOW_TOL_SPC( N_GC_SPC ) )
         L_LOW_TOL_SPC = .FALSE.
         ALLOCATE(L_HII_TOL_SPC( N_GC_SPC ) )
         L_HII_TOL_SPC = .FALSE.

         DO N = 1, N_GC_SPC
            IF( INDEX1( GC_SPC( N ), N_LOW_TOL_SPC, LOW_TOL_SPC )
     &          .NE. 0 ) THEN
                L_LOW_TOL_SPC( N ) = .TRUE.
            END IF
         END DO

c..Tag NO2EX as low tol if cycle is in group 1 and 2
!        IF( LNO2EX_CYCLE ) L_LOW_TOL_SPC( NO2EX ) = .TRUE.
         IF( LNO2EX_CYCLE )THEN
             L_HII_TOL_SPC( NO    ) = .TRUE.
             L_HII_TOL_SPC( NO2   ) = .TRUE.
             L_HII_TOL_SPC( NO2EX ) = .TRUE.
          END IF
         

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

      N_GC_AE_SPC = N_GC_SPC

      ALLOCATE( GC_AE_SPC( N_GC_SPC ) )

      DO N = 1, N_GC_SPC

         GC_AE_SPC( N ) = GC_SPC( N )

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

      N_GC_AQ_SPC = N_GC_SPC

      ALLOCATE( GC_AQ_SPC( N_GC_SPC ) )

      DO N = 1, N_GC_SPC

         GC_AQ_SPC( N ) = GC_SPC( N )

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

      N_GC_ONLY_SPC = N_GC_SPC

      ALLOCATE( GC_ONLY_SPC( N_GC_SPC ) )

      DO N = 1, N_GC_SPC

         GC_ONLY_SPC( N ) = GC_SPC( N )

      END DO

      RETURN

      END
