       SUBROUTINE WR_RATES

C**********************************************************************
C
C  FUNCTION: Create source code for the hrrates subroutine in EBI
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, March, 2004
C
C  Nov. 17 2005:WHUTZELL changed writing of rate so the length of species
C                        name is used not maximum of all species names
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

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT      ! gets unit no.
!       INTEGER   NAME_INDEX     ! find position of string in list

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_RATES'   ! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 256 )  ::    LINEIN               ! Input line
      CHARACTER( 256 )  ::    LINOUT
      CHARACTER(  16 )  ::    SPOUT                ! Ouput species
      CHARACTER(  16 )  ::    SPEC     
      CHARACTER(  30 )  ::    VNAME     
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open
      CHARACTER(  72 )  ::    CLINE                ! Line of c's
      CHARACTER(   5 )  ::    RNUM

   

      INTEGER  :: E1, E2       ! end pos of string
      INTEGER  :: IND1         ! array index
      INTEGER  :: IND2         ! array index
      INTEGER  :: IND3         ! array index
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file
      INTEGER  :: N, S         ! Loop indices
      INTEGER  :: NPOS         ! Reaction index
      INTEGER  :: RPOS1        !
      INTEGER  :: RPOS2        !
      INTEGER  :: PPOS1        !
      INTEGER  :: PPOS2        !

      LOGICAL  :: LRXN1

      REAL     :: COEFF
      REAL     :: RCOEF
      REAL     :: YCOEF

C**********************************************************************

      DO N = 1, 72
        CLINE( N : N ) = 'c'
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      E1 = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : E1 ) // '/hrrates.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      E1 = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : E1 ) // '/hrrates.F' 

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

         WRITE( IOUT, 93020 ) CR_DATE( 1 : LEN_TRIM( CR_DATE ) )

         GO TO 100

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'S1' ) THEN

        GO TO 1000

      ELSE

        E1 = LEN_TRIM( LINEIN )

        WRITE( IOUT, 92000 ) LINEIN( 1 : E1 )

        GO TO 100

      END IF
            
 1000 CONTINUE

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write reactions for gas-phase only portion of mechanism
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) 

      DO N = 1, NRXNS

         IND1 = 0
         IND2 = 0
         IND3 = 0

         IF( N_AE_SPC .GT. 0 .AND. N_AQ_SPC .GT. 0 )THEN
            IF( IRR( N, 1 ) .NE. 0 )
     &      IND1 = MAX( NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AE_SPC, AE_SPC ),
     &                  NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AQ_SPC, AQ_SPC ) )

            IF( IRR( N, 2 ) .NE. 0 )
     &         IND2 = MAX( NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AE_SPC, AE_SPC ),
     &                     NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AQ_SPC, AQ_SPC ) )

            IF( IRR( N, 2 ) .NE. 0 )
     &          IND2 = MAX( NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AE_SPC, AE_SPC ),
     &                      NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AQ_SPC, AQ_SPC ) )
         END IF

c..skip rxn if any reactant is a AE or AQ species
         IF( IND1 .NE. 0 .OR. IND2 .NE. 0 .OR. IND3 .NE. 0 ) CYCLE 

         WRITE( RNUM, '( I5 )' ) N

         LINOUT = '      RXRAT(  ' // RNUM // ' ) = RKI( ' // RNUM //
     &       ' )'

         E1 = LEN_TRIM( LINOUT )


         DO S = 1, NREACT( N )
            IF( IRR( N, S ) .NE. 0 ) THEN
               SPEC = ADJUSTL( SPECIES( IRR( N, S ) ) )
               E2   = LEN_TRIM( SPEC )
               LINOUT = LINOUT( 1 : E1 ) // ' * YC(  ' // SPEC( 1 : E2 ) //
     &                 ' )'
               E1 = LEN_TRIM( LINOUT )
            END IF
         END DO

         IF( .NOT. L_RXFLAG( N ) ) LINOUT( 1 : 1 ) = '!'
         IF( L_SS_RXN_FLAG( N ) ) LINOUT( 1 : 1 ) = '!'

  
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

      END DO


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write reactions for AE only portion of mechanism
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LRXN1 = .TRUE.
      DO N = 1, NRXNS

         IND1 = 0
         IND2 = 0
         IND3 = 0

         IF( N_AE_SPC .GT. 0 )THEN
           IF( IRR( N, 1 ) .NE. 0 )
     &         IND1 = NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AE_SPC, AE_SPC )

            IF( IRR( N, 2 ) .NE. 0 )
     &         IND2 = NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AE_SPC, AE_SPC )

            IF( IRR( N, 3 ) .NE. 0 )
     &          IND3 = NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AE_SPC, AE_SPC )
         END IF


c..do rxn if any reactant is a AE or AQ species
         IF( IND1 .NE. 0 .OR. IND2 .NE. 0 .OR. IND3 .NE. 0 ) THEN 

            IF( LRXN1 ) THEN

               WRITE( IOUT, 95000 )

               LRXN1 = .FALSE.

            END IF

            WRITE( RNUM, '( I5 )' ) N

            LINOUT = '         RXRAT(  ' // RNUM // ' ) = RKI( ' // RNUM //
     &             ' )'

            E1 = LEN_TRIM( LINOUT )


            DO S = 1, NREACT( N )
               IF( IRR( N, S ) .NE. 0 ) THEN
                  SPEC = ADJUSTL( SPECIES( IRR( N, S ) ) )
                  LINOUT = LINOUT( 1 : E1 ) // ' * YC(  ' // SPEC( 1 : CL ) //
     &                 ' )'
                  E1 = LEN_TRIM( LINOUT )
               END IF
            END DO
  
            IF( .NOT. L_RXFLAG( N ) ) LINOUT( 1 : 1 ) = '!'
            IF( L_SS_RXN_FLAG( N ) ) LINOUT( 1 : 1 ) = '!'
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

         END IF

      END DO

      IF( .NOT. LRXN1 ) WRITE( IOUT, 95020 )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write reactions for AQ only portion of mechanism
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LRXN1 = .TRUE.
      DO N = 1, NRXNS

         IND1 = 0
         IND2 = 0
         IND3 = 0

         IF( N_AQ_SPC .GT. 0 )THEN
             IF( IRR( N, 1 ) .NE. 0 )
     &       IND1 = NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AQ_SPC, AQ_SPC )

             IF( IRR( N, 2 ) .NE. 0 )
     &        IND2 = NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AQ_SPC, AQ_SPC )

              IF( IRR( N, 3 ) .NE. 0 )
     &        IND3 = NAME_INDEX( SPECIES( IRR( N, 1 ) ), N_AQ_SPC, AQ_SPC )
        END IF



c..do rxn if any reactant is a AE or AQ species
         IF( IND1 .NE. 0 .OR. IND2 .NE. 0 .OR. IND3 .NE. 0 ) THEN 

            IF( LRXN1 ) THEN

               WRITE( IOUT, 95040 )

               LRXN1 = .FALSE.

            END IF

            WRITE( RNUM, '( I5 )' ) N

            LINOUT = '         RXRAT(  ' // RNUM // ' ) = RKI( ' // RNUM //
     &             ' )'

            E1 = LEN_TRIM( LINOUT )


            DO S = 1, NREACT( N )
               IF( IRR( N, S ) .NE. 0 ) THEN
                  SPEC = ADJUSTL( SPECIES( IRR( N, S ) ) )
                  LINOUT = LINOUT( 1 : E1 ) // ' * YC(  ' // SPEC( 1 : CL ) //
     &                 ' )'
                  E1 = LEN_TRIM( LINOUT )
               END IF
            END DO
  
            IF( .NOT. L_RXFLAG( N ) ) LINOUT( 1 : 1 ) = '!'
            IF( L_SS_RXN_FLAG( N ) ) LINOUT( 1 : 1 ) = '!'
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

         END IF


      END DO

      IF( .NOT. LRXN1 ) WRITE( IOUT, 95020 )


      WRITE( IOUT, 96000 )

      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrrates.F'


      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )
92020 FORMAT( / )




93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )

94000 FORMAT(
     & 'C..NOTE: RXRAT commented out are not referenced by any routine' )

95000 FORMAT(/ 
     & '      IF( L_AE_VRSN ) THEN' / )

95020 FORMAT(/
     & '      END IF' )


95040 FORMAT(/
     & '      IF( L_AQ_VRSN ) THEN' / )


96000 FORMAT( //6X, 'RETURN' // 6X, 'END' )

          
      END

