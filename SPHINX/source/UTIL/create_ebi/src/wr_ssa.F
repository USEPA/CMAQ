      SUBROUTINE  WR_SSA

C**********************************************************************
C
C  FUNCTION: Create source code for the hrcalc_SS subroutine in EBI
C
C  PRECONDITIONS: Mechanism data must have been processed by CMAQ CHEMMECH
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: SUM_COEFF
C                                    BLD_OUTLINE
C
C  REVISION HISTORY: Created by Jerry Gipson, July, 2009
C
C**********************************************************************

      USE ENV_VARS
      USE GLOBAL_DATA
      !!USE M3UTILIO ! IOAPI parameters and declarations
      USE RXNS_DATA

      IMPLICIT NONE

C..INCLUDES: 

C..ARGUMENTS: None

C..PARAMETERS:
      INTEGER, PARAMETER   ::  GRPNO = 5

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT      ! gets unit no.
!       INTEGER   NAME_INDEX     ! find position of string in list


C..SAVED LOCAL VARIABLES:
      CHARACTER(  16 ), SAVE  ::    PNAME = 'WR_HRCALC_SSA' ! Program name
 
C..SCRATCH LOCAL VARIABLES:
  
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER( 100 )  ::    LINOUT               ! Output line
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open

      CHARACTER(   4 )  ::    RKOUT                ! Output reaction number
      CHARACTER(   9 )  ::    COUT                 ! Output coefficient
      CHARACTER(  16 )  ::    SPOUT                ! Output species name
      CHARACTER(  16 )  ::    LBLOUT               ! Output reaction label
      CHARACTER(  16 )  ::    OPOUT                ! Output operator name
      CHARACTER(  16 )  ::    RCTOUT               ! Output reactant name
      CHARACTER(  30 )  ::    VNAME                ! Name of variable to be written
      CHARACTER(  72 )  ::    CLINE                ! String of c's
 

      INTEGER  :: EPOS          ! end pos of string
      INTEGER  :: IIN           ! Unit no. of input file
      INTEGER  :: IOUT          ! Unit no. of output file

      INTEGER  :: E1            ! End pos. of string
      INTEGER  :: E2            ! End pos. of string

      INTEGER  :: NPOS          ! Position number
      INTEGER  :: RPOS1         ! Reactant pos. in cmprsd rxn string
      INTEGER  :: RPOS2         ! Reactant pos. in cmprsd rxn string
      INTEGER  :: PPOS1         ! Reactant pos. in cmprsd rxn string
      INTEGER  :: PPOS2         ! Reactant pos. in cmprsd rxn string

      INTEGER  :: IR            ! Loop index
      INTEGER  :: N             ! Loop index
      INTEGER  :: S             ! Loop index
      INTEGER  :: T1            ! Loop index
      INTEGER  :: IND           ! Array index
      INTEGER  :: NRX           ! Reaction no.

      INTEGER  :: RKNUM         ! Reaction index
      INTEGER  :: SPNUM         ! Species index
      INTEGER  :: OPNUM         ! Operator index

      LOGICAL  :: LRXN1                  ! Flag to indicate one term output
      LOGICAL  :: LERROR = .FALSE.       ! Error flag
      LOGICAL  :: L_SS_RXN               ! Flag to indicate a SS species is a reactant
      LOGICAL, ALLOCATABLE  :: L_SPECIAL_RK( : )   ! Flag to indicate rxn w/ special rate const

      REAL     :: COEFF          ! Net prod/loss coefficient
      REAL     :: RCOEF          ! Sum of number of molecules of a single reactant
      REAL     :: YCOEF          ! Sum of coefficients for a product in one rxn 

C**********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..Line of c's
      DO N = 1, 72
        CLINE( N : N ) = 'c'
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      E1 = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : E1 ) // '/hrcalc_ss.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      E1 = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : E1 ) // '/hrcalc_ss.F' 

      OPEN( UNIT = IIN, FILE = FNAME, ERR = 9000 )


      IF( LWR_COPY ) CALL WR_COPYRT( IOUT )

      IF( LWR_CVS_HDR ) CALL WR_CVSHDR( IOUT )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read, modify, and write first part of code from template
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  100 CONTINUE

      READ( IIN, 92000, END = 1000 ) LINEIN

      IF( LINEIN( 1 : 2 ) .EQ. 'R1' ) THEN

         WRITE( IOUT, 93000 ) TRIM( MECHNAME )

         GO TO 100

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R2' ) THEN

         WRITE( IOUT, 93020 ) TRIM( CR_DATE )

         GO TO 100

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'S1' ) THEN

        GO TO 1000

      ELSE

        E1 = LEN_TRIM( LINEIN )

        WRITE( IOUT, 92000 ) LINEIN( 1 : E1 )

        GO TO 100

      END IF
            
 1000 CONTINUE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Flag reactions if they use a special rate constant 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( L_SPECIAL_RK( NRXNS ) )
      L_SPECIAL_RK = .FALSE.            ! Array

      DO N = 1, NSPECIAL_RXN
        L_SPECIAL_RK( ISPECIAL( N, 1 ) )  = .TRUE.
      END DO
 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Process each steady-state species one at a time and generate code for
c  each one that computes its production, loss frequency, and concentration;
c  then generate code that updates the reaction rate of all reactions
c  in which this SS species is a reactant 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO S = 1, N_SS_SPC

         WRITE( IOUT, 92000 )

         SPOUT = ADJUSTL( SS_SPC( S ) )

         WRITE( IOUT, 92100 ) CLINE, SPOUT, CLINE

         SPNUM = NAME_INDEX( SPOUT, N_SPECIES, SPECIES )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over all reactions to get all SS production terms
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         VNAME = 'SS_PROD'
         LRXN1 = .TRUE.
         WRITE( IOUT, 92000 )

         DO N = 1, MAX_SS_PROD

            NRX = SS_PROD_RXNS( S, N )

            IF( NRX .LE. 0 ) CYCLE

            COEFF = SS_PROD_COEF( S, N )
            
            NPOS = 30
            RPOS1 = 0
            RPOS2 = 0
            PPOS1 = SPNUM
            PPOS2 = 0

            CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, NRX, GRPNO,  
     &           NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LRXN1 = .FALSE.
         
         END DO

c..Error generated if the SS species has no production terms
         IF( LRXN1 ) THEN
            WRITE( LOGDEV, 98000 ) SPOUT
            LERROR = .TRUE.
         END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over all reactions to get all SS loss frequency terms
c  If the reaction uses a special rate constant, use RKI; if not
c  use RKI_SAV 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LRXN1 = .TRUE.
         WRITE( IOUT, 92000 )

         DO N = 1, MAX_SS_LOSS

            NRX = SS_LOSS_RXNS( S, N )

            IF( NRX .LE. 0 ) CYCLE

c..Don't need to calculate rxrat in hrrates if this reaction has a SS reactant
            L_SS_RXN_FLAG( NRX ) = .TRUE.

            WRITE( RKOUT, '( I4 )' ) NRX

            IF( LRXN1 ) THEN
               IF( L_SPECIAL_RK( NRX ) ) THEN
                  LINOUT = '      SS_LFRQ = RKI( NCELL, ' // RKOUT // ' )'
               ELSE
                  LINOUT = '      SS_LFRQ = RKI_SAV( NCELL, ' // RKOUT // ' )'
               END IF
            ELSE
               IF( L_SPECIAL_RK( NRX ) ) THEN
                  LINOUT = '     &        + RKI( NCELL, ' // RKOUT // ' )'
               ELSE              
                  LINOUT = '     &        + RKI_SAV( NCELL, ' // RKOUT // ' )'
               END IF
            END IF 
            E1 = LEN_TRIM( LINOUT )

            DO IND = 1, NREACT( NRX )
               SPOUT = ADJUSTL( SPECIES( IRR( NRX, IND ) ) )
               E2 = LEN_TRIM( SPOUT )
               LINOUT = LINOUT( 1 : E1 ) // '* YC( NCELL, ' // SPOUT( 1 : E2 ) // ' )'
               E1 = LEN_TRIM( LINOUT )
            END DO

            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )
            LRXN1 = .FALSE.
         
         END DO    ! End loop over MAX_SS_LOSS

c..Error generated if the SS species has no loss terms
         IF( LRXN1 ) THEN
            WRITE( LOGDEV, 98020 ) SPOUT
            LERROR = .TRUE.
         END IF


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Generate code that computes the SS species concentration
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         WRITE( IOUT, 92120 ) 

         SPOUT = ADJUSTL( SS_SPC( S ) )
         E1 = LEN_TRIM( SPOUT )

         LINOUT = '      IF( SS_LFRQ .LE. 0.0 ) SS_LFRQ = MINLOSS'
         EPOS = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

         LINOUT = '      YC( NCELL, ' // SPOUT( 1 : E1 ) // 
     &            ' ) = SS_PROD / SS_LFRQ'
         EPOS = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

         LINOUT = '      YC( NCELL, ' // SPOUT( 1 : E1 ) // ' ) = MAX( YC( NCELL, ' //
     &            SPOUT( 1 : E1 ) // ' ), MINCONC )'
         EPOS = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Generate code that updates reaction rates and rate constants for all 
c  reactions that have this SS species as a reactant; The SS conc is rolled
c  into the rate constant using RKI for special rate constants and 
c  RKI_SAV for non-special rate constants
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         WRITE( IOUT, 92140 ) 

c..This section updates reaction rates (rxrat)
         DO N = 1, MAX_SS_LOSS

            NRX = SS_LOSS_RXNS( S, N )

            IF( NRX .LE. 0 ) CYCLE

            WRITE( RKOUT, '( I4 )' ) NRX
            
            IF( L_SPECIAL_RK( NRX ) ) THEN
               LINOUT = '      RXRAT( NCELL, ' // RKOUT // ' ) = RKI( NCELL, ' //
     &                   RKOUT // ' ) '
            ELSE
               LINOUT = '      RXRAT( NCELL, ' // RKOUT // ' ) = RKI_SAV( NCELL, ' //
     &                   RKOUT // ' ) '
            END IF             
            EPOS = LEN_TRIM( LINOUT )

c..Loop over all non-SS reactants for this rxn ( must be LE 2 ) 
            DO IR = 1, 2 
               IND = IRR( NRX, IR )       
               IF( IND .EQ. 0 ) CYCLE
               RCTOUT = ADJUSTL( SPECIES( IND ) )
               E1 = LEN_TRIM( RCTOUT )
               LINOUT = LINOUT( 1 : EPOS ) // ' * YC( NCELL, ' //
     &                 RCTOUT( 1 : E1 ) // ' )'
               EPOS = LEN_TRIM( LINOUT )
            END DO

c..Add the SS reactant 
            RCTOUT = ADJUSTL( SS_SPC( S ) )
            E1 = LEN_TRIM( RCTOUT )
            LINOUT = LINOUT( 1 : EPOS ) // ' * YC( NCELL, ' //
     &               RCTOUT( 1 : E1 ) // ' )'
            EPOS = LEN_TRIM( LINOUT )


            WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

        END DO       ! Loop over reactions

 
c..This section writes code to update rate constants
        DO N = 1, MAX_SS_LOSS

            NRX = SS_LOSS_RXNS( S, N )

            IF( NRX .LE. 0 ) CYCLE

            WRITE( RKOUT, '( I4 )' ) NRX
            
            IF( L_SPECIAL_RK( NRX ) ) THEN
  
               LINOUT = '      RKI( NCELL, ' // RKOUT // ' ) = RKI( NCELL, ' //
     &               RKOUT // ' ) * YC( NCELL, ' // RCTOUT( 1 : E1 ) // ' )'
               EPOS = LEN_TRIM( LINOUT )

            ELSE

               LINOUT = '      RKI( NCELL, ' // RKOUT // ' ) = RKI_SAV( NCELL, ' //
     &                 RKOUT // ' ) * YC( NCELL, ' // RCTOUT( 1 : E1 ) // ' )'
               EPOS = LEN_TRIM( LINOUT )

            END IF

            WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

         END DO      ! Loop over reactions


      END DO         ! Loop over SS species

      WRITE( IOUT, 95100 ) 

      IF( LERROR ) THEN
         MSG = 'STOPPING because of errors in processing steady-state species'
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         STOP
      END IF

      CLOSE( IIN )
      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrcalc_ss.F'

      DEALLOCATE( L_SPECIAL_RK )

 
      RETURN 

 9000 CONTINUE

      MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP


       
92000 FORMAT( A )

92100 FORMAT( / A / 'c  SS Species: ', A / A )

92120 FORMAT( /'c..compute steady-state concentration' )
92140 FORMAT( /'c..update reaction rates with the computed SS species conc ' )

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )

95000 FORMAT( 6X, 'REAL ', A )

95100 FORMAT( 6X, 'RETURN' // 6X, 'END' )

98000 FORMAT( 'ERROR: The following steady-state species has no production term: ', A )
98020 FORMAT( 'ERROR: The following steady-state species has no loss term: ', A )


      END
