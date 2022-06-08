       SUBROUTINE WR_INIT

C**********************************************************************
C
C  FUNCTION: Create source code for the hrinit subroutine in EBI
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, February, 2004
C
C**********************************************************************
      USE ENV_VARS
      USE GLOBAL_DATA
      USE RXNS_DATA

      IMPLICIT NONE

C..INCLUDES: 

      
C..ARGUMENTS: None

C..PARAMETERS:

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT      ! gets unit no.
!       INTEGER   NAME_INDEX     ! find position of string in list

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_INIT'    ! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER(  16 )  ::    SPOUT                ! Ouput species
      CHARACTER(  16 )  ::    SPEC     
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open
      CHARACTER(  72 )  ::    CLINE                ! Line of c's
      CHARACTER(   7 )  ::    LOW_TOL = '1.0D+00'  ! Low tolerance            
      CHARACTER(   7 )  ::    STD_TOL = '5.0D-04'  ! Standard tolerance 
      CHARACTER(   7 )  ::    HII_TOL = '1.0D-05'  ! High or tight tolerance

   

      INTEGER  :: EPOS         ! end pos of string
      INTEGER  :: IND          ! array index
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file
      INTEGER  :: N, S         ! Loop indices
      INTEGER  :: NSPEC        ! 

      LOGICAL  :: LFLAG

      LOGICAL  :: LOUT1 = .FALSE.  ! Flag for group 1 output processed
      LOGICAL  :: LOUT2 = .FALSE.  ! Flag for group 2 output processed
      LOGICAL  :: LOUT3 = .FALSE.  ! Flag for group 3 output processed
      LOGICAL  :: LOUT4 = .FALSE.  ! Flag for group 4 output processed
      LOGICAL  :: LOUT5 = .FALSE.  ! Flag for group 5 output processed


C**********************************************************************

      DO N = 1, 72
        CLINE( N : N ) = 'c'
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      EPOS = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : EPOS ) // '/hrinit.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      EPOS = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : EPOS ) // '/hrinit.F' 

      OPEN( UNIT = IIN, FILE = FNAME, ERR = 9000 )


      IF( LWR_COPY ) CALL WR_COPYRT( IOUT )

      IF( LWR_CVS_HDR ) CALL WR_CVSHDR( IOUT )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read, modify, and write code from template
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  100 CONTINUE

      READ( IIN, 92000, END = 1000 ) LINEIN

      IF( LINEIN( 1 : 1 ) .NE. 'R' ) THEN

         WRITE( IOUT, 92000 ) LINEIN( 1 : LEN_TRIM( LINEIN ) )

      ELSE

         IF( LINEIN( 2 : 2 ) .EQ. '1' .AND. .NOT. LOUT1 ) THEN

            WRITE( IOUT, 93000 ) TRIM( MECHNAME )

            LOUT1 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '2' .AND. .NOT. LOUT2 ) THEN

            WRITE( IOUT, 93020 ) CR_DATE( 1 : LEN_TRIM( CR_DATE ) )

            LOUT2 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '3' .AND. .NOT. LOUT3 ) THEN

            WRITE( IOUT, 93025 ) NSPECIAL

            LOUT3 = .TRUE. 

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '4' .AND. .NOT. LOUT4 ) THEN

            IF( LPAR_NEG ) WRITE( IOUT, 93040 ) 
            IF( N_SS_SPC .GT. 0 ) WRITE( IOUT, 93045 )

            LOUT4 = .TRUE.

         ELSEIF( LINEIN( 2 : 2 ) .EQ. '5' .AND. .NOT. LOUT5 ) THEN

            IF( LDEGRADE_SUBS ) WRITE( IOUT, 93046 ) LINEIN( 3 : LEN_TRIM( LINEIN ) )

            LOUT5 = .FALSE.

         END IF

      END IF

      GO TO 100

 1000 CONTINUE


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Write first section for one mech
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!         WRITE( IOUT, 92000 ) CLINE
!         WRITE( IOUT, 93100 )
!         WRITE( IOUT, 92000 ) CLINE

c..create indices for all mechanism species
!         DO N = 1, N_SPECIES
!            SPOUT = '                '         
!            EPOS = LEN_TRIM( SPECIES( N ) )
!            SPOUT( 1 : EPOS ) = SPECIES( N )( 1 : EPOS ) 
!            WRITE( IOUT, 93120 ) SPOUT( 1 : CL ), N
!         END DO


         IF( N_SS_SPC .EQ. 0 ) THEN    !  no ss species in mech

c..form ebi species array (all species except those in groups 1-4) 
            WRITE( IOUT, 93140 ) N_GC_EBI

            IND = 0
            DO N = 1, N_SPECIES
               IF( L_GC_EBI( N ) ) THEN
                  IND = IND + 1
                  WRITE( IOUT, 93160 ) IND, SPECIES( N )
     &                 ( 1 : LEN_TRIM( SPECIES( N ) ) )
               END IF
            END DO

         ELSE                          ! some species in mech are in ss

c..form ebi species array ( no SS species, no group species )
            WRITE( IOUT, 93140 ) ( N_GC_EBI - N_SS_SPC )
            IND = 0
            DO N = 1, N_SPECIES
               IF( NAME_INDEX( SPECIES( N ), N_SS_SPC, SS_SPC) .EQ. 0 .AND.
     &             L_GC_EBI( N ) ) THEN  
                   IND = IND + 1
                   WRITE( IOUT, 93160 ) IND, SPECIES( N )
     &                  ( 1 : LEN_TRIM( SPECIES( N ) ) )
               END IF
            END DO  

c..form ss species array  
            WRITE( IOUT, 93180 )        
            IND = 0
            DO N = 1, N_SS_SPC
               IND = IND + 1 
               WRITE( IOUT, 93200 ) IND, 
     &                SS_SPC( N )( 1 : LEN_TRIM( SS_SPC( N ) ) )
            END DO   

c..form active species array ( all species except SS species )            
            WRITE( IOUT, 93220 ) N_SPECIES - N_SS_SPC
            IND = 0
            DO N = 1, N_SPECIES
               IF( NAME_INDEX( SPECIES( N ), N_SS_SPC, SS_SPC ) .EQ. 0 ) THEN  
                   IND = IND + 1
                   WRITE( IOUT, 93240 ) IND, 
     &                    SPECIES( N )( 1 : LEN_TRIM( SPECIES( N ) ) )
               END IF
            END DO 
           
         END IF              ! N_SS_SPC > 0

           
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Write tolerance section
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      WRITE( IOUT, 92020 )
      WRITE( IOUT, 92000 ) CLINE
      WRITE( IOUT, 95000 )
      WRITE( IOUT, 92000 ) CLINE



         DO N = 1, N_SPECIES

            IF( L_LOW_TOL_SPC( N ) ) THEN
               WRITE( IOUT, 95020 ) SPECIES( N )( 1 : CL ), LOW_TOL
            ELSEIF( L_HII_TOL_SPC( N ) ) THEN
               WRITE( IOUT, 95020 ) SPECIES( N )( 1 : CL ), HII_TOL
            ELSE
               WRITE( IOUT, 95020 ) SPECIES( N )( 1 : CL ), STD_TOL
            END IF

         END DO


      WRITE( IOUT, 96000 )

      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrinit.F'


      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )
92020 FORMAT( / )

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )
93025 FORMAT( 6X, 'N_SPCL_RKS = ', I3 )

93040 FORMAT( 5X, '&          PNEG( N_SPEC),' )

93045 FORMAT( 6X, 'ALLOCATE( RKI_SAV( MAX_CELLS_EBI, NRXNS ) )' )

93100 FORMAT( 'c  Set species indices and pointers' )
93120 FORMAT( 6X, A, '  = ', I3 )

93139 FORMAT( /'c..Allocate and define ebi species' / 
     &         6X, 'N_EBISP  = ', I3 /
     &         6X, 'ALLOCATE( EBISP( N_EBISP ) ) ' / )

93140 FORMAT( /'c..Allocate and define ebi species' /
     &         /6X, 'N_EBISP  = ', I3 /
     &         6X, 'ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) ' /
     &         6X, 'IF ( IOS .NE. 0 ) THEN' /
     &         6X, "   MSG = 'Error allocating EBISP' "  /
     &         6X, '   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )' /
     &         6X, 'END IF' /  )

93160 FORMAT(  6X, 'EBISP( ', I3, ' ) = ', A )

93180 FORMAT( /'c..Allocate and define steady-state species' /
     &         6X, 'ALLOCATE( SSASP( N_SS_SPC ) ) ' / )
93200 FORMAT(  6X, 'SSASP( ', I3, ' ) = ', A )

93220 FORMAT( /'c..Allocate and define active mechanism species' /
     &         6X, 'N_ACTSP  = ', I3 /
     &         6X, 'ALLOCATE( ACTSP( N_ACTSP ) ) ' / )
93240 FORMAT(  6X, 'ACTSP( ', I3, ' ) = ', A )


94000 FORMAT( 'c  Set species indices for gas-phase only version' )
94020 FORMAT( 'c  Set species indices for gas-phase plus aerosol only version' )
94040 FORMAT( 'c  Set species indices for gas-phase plus AQ chemistry version' )
94060 FORMAT( 'c  Set species indices for gas-phase plus ',
     &        ' aerosol plus AQ chem version' )

94100 FORMAT( 6X, 'IF( .NOT. L_AQ_VRSN .AND. .NOT. L_AE_VRSN ) THEN'/ )
94120 FORMAT( 6X, 'IF( L_AE_VRSN .AND. .NOT. L_AQ_VRSN ) THEN' / )
94140 FORMAT( 6X, 'IF( .NOT. L_AE_VRSN .AND. L_AQ_VRSN ) THEN' / )
94160 FORMAT( 6X, 'IF( L_AE_VRSN .AND. L_AQ_VRSN ) THEN' / )
94180 FORMAT( 9X, A, '  = ', I3 )
94200 FORMAT( /9X, 'N_EBISP  = ', I3 /
     &         9X, 'ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) ' /
     &         9X, 'IF ( IOS .NE. 0 ) THEN' /
     &         9X, "   MSG = 'Error allocating EBISP' "  /
     &         9X, '   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )' /
     &         9x, 'END IF'  )
94220 FORMAT(  9X, 'EBISP( ', I3, ' ) = ', A )
94240 FORMAT( /6X, 'END IF' )

95000 FORMAT(      'c  Set species tolerances' )
95020 FORMAT(  6X, 'RTOL( ', A, ' ) = ', A )
95040 FORMAT( /6X, 'IF( L_AE_VRSN ) THEN' / )
93046 FORMAT(  2X, A )
95060 FORMAT( /6X, 'IF( L_AQ_VRSN ) THEN' / )
95080 FORMAT(  9X, 'RTOL( ', A, ' ) = ', A )

96000 FORMAT( //6X, 'RETURN' // 6X, 'END' )
          
      END

