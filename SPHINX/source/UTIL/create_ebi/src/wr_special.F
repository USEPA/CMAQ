      SUBROUTINE WR_SPECIAL

C**********************************************************************
C
C  FUNCTION: Create source code for the HRCALC_SPECIAL subroutine in EBI
C
C  PRECONDITIONS: Mechanism data must have been processed by CMAQ CHEMMECH
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
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


C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
       INTEGER   JUNIT      ! gets unit no.

C..SAVED LOCAL VARIABLES:
      CHARACTER(  16 ), SAVE  ::    PNAME = 'WR_SPECIAL' ! Program name
 
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
 

      INTEGER  :: EPOS          ! end pos of string
      INTEGER  :: E1            ! end pos of string
      INTEGER  :: IIN           ! Unit no. of input file
      INTEGER  :: IOUT          ! Unit no. of output file

      INTEGER  :: N             ! Loop index
      INTEGER  :: T1            ! Loop index
      INTEGER  :: POS           ! Loop index
      INTEGER  :: IND           ! Array index

      INTEGER  :: MXL_OPNAM = 0 ! Length of longest spec. op. name
      INTEGER  :: MXL_SPC   = 0 ! Length of longest species name used by sp. ops
      INTEGER  :: MXL_LBL   = 0 ! Length of longest rx label used by sp. ops
      INTEGER  :: MXL_LBL2  = 0 ! Length of longest rx label set to a special rate

      INTEGER  :: RKNUM         ! Reaction index
      INTEGER  :: SPNUM         ! Species index
      INTEGER  :: OPNUM         ! Operator index

      LOGICAL  :: LDONE1             ! Flag to indicate one term of sp. op output
      LOGICAL  :: LERROR = .FALSE.   ! Error flag

      REAL     :: COEFF         ! Coefficient for a term in special operator
  

C**********************************************************************


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get max lengths of key character strings for formatting purposes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
c..get length of the longest operator name
      MXL_OPNAM = 0
      DO N = 1, NSPECIAL
         MXL_OPNAM = MAX( MXL_OPNAM , LEN_TRIM( SPECIAL( N ) ) )
      END DO

c..get length of the longest species name that is referenced by operators
      MXL_SPC = 0
      DO N = 1, NSPECIAL
         DO T1 = 1, MAXSPECTERMS
            IND =  INDEX_CTERMS( N, T1 ) 
            IF( IND .NE. 0 ) MXL_SPC = MAX( MXL_SPC, LEN_TRIM( SPECIES( IND ) ) )
         END DO
      END DO


c..get length of the longest rxn label that is referenced in operator definitions 
      MXL_LBL = 0
      DO N = 1, NSPECIAL
         DO T1 = 1, MAXSPECTERMS
            IND =  INDEX_KTERMS( N, T1 ) 
            IF( IND .NE. 0 ) MXL_LBL = MAX( MXL_LBL, LEN_TRIM( RXLABEL( IND ) ) )
         END DO
      END DO

c..get length of the longest rxn label that is set equal to an operator value 
      MXL_LBL2 = 0
      DO N = 1, NSPECIAL
         IND = ISPECIAL( N, 1 ) 
         IF( IND .NE. 0 ) MXL_LBL2 = MAX( MXL_LBL2, LEN_TRIM( RXLABEL( IND ) ) )
      END DO


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      E1 = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : E1 ) // '/hrcalc_special.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )

      IIN = JUNIT()

      E1 = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : E1 ) // '/hrcalc_special.F' 

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

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'R3' ) THEN

c..Declare special rate constant names
         DO N = 1, NSPECIAL
            WRITE( IOUT, 95000 ) SPECIAL( N )
         END DO
         WRITE( IOUT, 92000 )

         GO TO 100

      ELSEIF( LINEIN( 1 : 2 ) .EQ. 'S1' ) THEN

        GO TO 1000

      ELSE

        E1 = LEN_TRIM( LINEIN )

        WRITE( IOUT, 92000 ) LINEIN( 1 : E1 )

        GO TO 100

      END IF
            
 1000 CONTINUE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Section to compute values of special rate constants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      WRITE( IOUT, 92000 )'c..Compute values of special rate constants'
      
      DO N = 1, NSPECIAL

         WRITE( IOUT , 92000 )

         LDONE1 = .FALSE.

         OPOUT = ADJUSTL( SPECIAL( N ) )

         LINOUT = '      ' // OPOUT( 1 : MXL_OPNAM )

         LINOUT = LINOUT( 1 : MXL_OPNAM + 6 ) // ' = '

         EPOS = MXL_OPNAM + 9

 
c..Loop for rate constant/concentration terms
         DO T1 = 1, MAXSPECTERMS

            IF( INDEX_KTERMS( N, T1 ) .EQ. 0 .OR. INDEX_KTERMS( N, T1 ) .EQ. 0 ) CYCLE

            COEFF = KC_COEFFS( N, T1 )

            COUT = '         '

            IF( COEFF .NE. 1.0 ) WRITE( COUT, '( F6.3, A )' ) COEFF, ' * '

            RKNUM = INDEX_KTERMS( N, T1 )

            WRITE( RKOUT, '( I4 )' ) RKNUM

            SPNUM = INDEX_CTERMS( N, T1 ) 

            SPOUT = ADJUSTL ( SPECIES( SPNUM ) )

            LINOUT = LINOUT( 1 : EPOS ) // COUT // 'RKI( NCELL, '// RKOUT // ' ) * YC( NCELL, ' //
     &               SPOUT( 1 : MXL_SPC ) // ' )      !'

            EPOS = LEN_TRIM( LINOUT )

            LBLOUT = ADJUSTL( RXLABEL( RKNUM ) ) 

            LINOUT = LINOUT( 1 : EPOS ) // ' RKI( NCELL,' // RKOUT // ') = RKI<' //
     &               LBLOUT( 1 : MXL_LBL ) // '>'

            EPOS = LEN_TRIM( LINOUT )

            WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

            LDONE1 = .TRUE.

            DO POS = 1, MXL_OPNAM + 9

                LINOUT( POS : POS ) = ' '
                IF( POS .EQ. 6 ) LINOUT( POS : POS ) = '&' 
                IF( POS .EQ. MXL_OPNAM + 8 ) LINOUT( POS : POS ) = '+'              
            
            END DO

            EPOS = MXL_OPNAM + 9

         END DO    

c..Loop for other special operator terms
         DO T1 = 1, MAXSPECTERMS

            IF( OPERATORS( N, T1 ) .EQ. 0 ) CYCLE           
            
            COEFF = OPERATOR_COEFFS( N , T1 )

            COUT  = '         '

            IF( COEFF .NE. 1.0 ) WRITE( COUT, '( F6.3, A )' ) COEFF, ' * '

            OPOUT = ADJUSTL( SPECIAL( OPERATORS( N, T1 ) ) )

            LINOUT = LINOUT( 1 : EPOS ) // COUT // OPOUT( 1 : MXL_OPNAM ) 

            EPOS = LEN_TRIM( LINOUT )

            WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

            LDONE1 = .TRUE.

            DO POS = 1, MXL_OPNAM + 9

               LINOUT( POS : POS ) = ' '
               IF( POS .EQ. 6 ) LINOUT( POS : POS ) = '&' 
               IF( POS .EQ. MXL_OPNAM + 8 ) LINOUT( POS : POS ) = '+'              
            
            END DO

            EPOS = MXL_OPNAM + 9

         END DO

         IF( .NOT. LDONE1 ) THEN

            WRITE( LOGDEV, 91000 ) SPECIAL ( N )

            LERROR = .TRUE.

         END IF

      END DO

c..stop if any errors encountered

      IF( LERROR ) THEN
         MSG = 'Stopping because of special rate constant errors'
         WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
         STOP
      END IF


c..Set rate constants of individual reactions

      WRITE( IOUT, 92000 )
      WRITE( IOUT, 92000 ) 'c..set individual mechanism rate constants'
      WRITE( IOUT, 92000 )

      DO N = 1, NSPECIAL_RXN

         RKNUM = ISPECIAL( N, 1 )

         WRITE( RKOUT, '( I4 )' ) RKNUM

         OPNUM = ISPECIAL( N, 2 )

         OPOUT = ADJUSTL( SPECIAL( OPNUM ) )

         LBLOUT = ADJUSTL( RXLABEL( RKNUM ) ) 

         LINOUT = '      RKI( NCELL, ' // RKOUT // ' ) = ' // OPOUT( 1 : MXL_OPNAM ) //
     &            '     !  RKI<' // LBLOUT ( 1 : MXL_LBL2 ) // '>'

         EPOS = LEN_TRIM( LINOUT )

         WRITE( IOUT, 92000 ) LINOUT( 1 : EPOS )

      END DO

      WRITE( IOUT, 95100 ) 

      CLOSE( IIN )
      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrcalc_special.F'

 
      RETURN 

 9000 CONTINUE

      MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP


91000 FORMAT( 'ERROR: No terms found for special rate constant named: ', A )
       
92000 FORMAT( A )

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )



95000 FORMAT( 6X, 'REAL ', A16 )

95100 FORMAT( / 6X, 'RETURN' // 6X, 'END' )

      END
