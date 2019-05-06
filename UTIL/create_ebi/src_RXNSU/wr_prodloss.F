       SUBROUTINE WR_PRODLOSS

C**********************************************************************
C
C  FUNCTION: Create source code for the hrprodloss subroutine in EBI
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, March, 2004
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

C..SAVED LOCAL VARIABLES: None
 
C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  16 )  ::    PNAME = 'WR_PRODLOSS'! Program name
      CHARACTER( 256 )  ::    MSG                  ! Message text
      CHARACTER( 100 )  ::    LINEIN               ! Input line
      CHARACTER( 256 )  ::    LINOUT
      CHARACTER(  16 )  ::    SPOUT                ! Ouput species
      CHARACTER(  16 )  ::    SPEC     
      CHARACTER(  40 )  ::    VNAME     
      CHARACTER( 256 )  ::    FNAME                ! Name of file to open
      CHARACTER(  72 )  ::    CLINE                ! Line of c's

   

      INTEGER  :: E1, E2       ! end pos of string
      INTEGER  :: IND          ! array index
      INTEGER  :: IIN          ! Unit no. of input file
      INTEGER  :: IOUT         ! Unit no. of output file
      INTEGER  :: N, S         ! Loop indices
      INTEGER  :: NPOS         ! Reaction index
      INTEGER  :: RPOS1        !
      INTEGER  :: RPOS2        !
      INTEGER  :: PPOS1        !
      INTEGER  :: PPOS2        !

      LOGICAL  :: LRXN1

      REAL( 8 ) :: COEFF
      REAL( 8 ) :: RCOEF
      REAL( 8 ) :: YCOEF

C**********************************************************************

      DO N = 1, 72
        CLINE( N : N ) = 'c'
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open ouput file and code template 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      E1 = LEN_TRIM( OUTPATH )

      FNAME = OUTPATH( 1 : E1 ) // '/hrprodloss.F' 

      IOUT = JUNIT()

      OPEN( UNIT = IOUT, FILE = FNAME, ERR = 9000 )


      IIN = JUNIT()

      E1 = LEN_TRIM( TMPLPATH )

      FNAME = TMPLPATH( 1 : E1 ) // '/hrprodloss.F' 

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
c  Output prod loss terms for gas-phase EBI species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO S = 1, N_SPECIES

         IF( .NOT. L_GC_EBI( S ) ) CYCLE        ! Skip non-EBI species

         IF( .NOT. L_GC_ONLY_SPC( S ) ) CYCLE   ! Skip non gas-only species

       
         IF( N_SS_SPC .GT. 0 ) THEN             ! Skip SS species
            IF( NAME_INDEX( SPECIES( S ), N_SS_SPC, SS_SPC ) .NE. 0 ) CYCLE
         END IF

         WRITE( IOUT, 92000 )

         SPEC = ADJUSTL( SPECIES( S ) )

         VNAME = 'PROD(  ' // SPEC( 1 : CL ) // ' )'


c..production terms
         LRXN1 = .TRUE.
         DO N = 1, NRXNS

            CALL SUM_COEFF( RCOEF, 'R', S, N )
            CALL SUM_COEFF( YCOEF, 'P', S, N )

            IF( YCOEF .LE. RCOEF ) CYCLE

            COEFF = YCOEF - RCOEF
            
            NPOS = 30
            RPOS1 = 0
            RPOS2 = 0
            PPOS1 = S
            PPOS2 = 0

            CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &           NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LRXN1 = .FALSE.
         
         END DO

         IF( LRXN1 ) THEN

            E2 = LEN_TRIM( VNAME )
            LINOUT = '      ' // VNAME( 1 : E2 ) // ' =   0.0D0'
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

         END IF


c..loss terms

         VNAME = 'LOSS(  ' // SPEC( 1 : CL ) // ' )'


         LRXN1 = .TRUE.
         DO N = 1, NRXNS

            CALL SUM_COEFF( RCOEF, 'R', S, N )
            CALL SUM_COEFF( YCOEF, 'P', S, N )

            IF( RCOEF .LE. YCOEF ) CYCLE

            COEFF = RCOEF - YCOEF
            
            NPOS = 30
            RPOS1 = S
            RPOS2 = 0
            PPOS1 = S
            PPOS2 = 0


            CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &           NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LRXN1 = .FALSE.
         
         END DO

         IF( LRXN1 ) THEN

            E2 = LEN_TRIM( VNAME )
            LINOUT = '      ' // VNAME( 1 : E2 ) // ' =   0.0D0'
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

         END IF

      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Production/loss terms for AE species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( N_AE_SPC .GT. 0 ) THEN

         LINOUT = '      IF( L_AE_VRSN ) THEN' 
         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )
         
         DO S = 1, N_AE_SPC

            IND = NAME_INDEX( AE_SPC( S ) , N_SPECIES, SPECIES )

            WRITE( IOUT, 92000 )

            SPEC = ADJUSTL( AE_SPC( S ) )

            VNAME = '   PROD(  ' // SPEC( 1 : CL ) // ' )'


c..production terms
            LRXN1 = .TRUE.
            DO N = 1, NRXNS

               CALL SUM_COEFF( RCOEF, 'R', IND, N )
               CALL SUM_COEFF( YCOEF, 'P', IND, N )

            IF( YCOEF .LE. RCOEF ) CYCLE

            COEFF = YCOEF - RCOEF
            
            NPOS = 30
            RPOS1 = 0
            RPOS2 = 0
            PPOS1 = IND
            PPOS2 = 0

            CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &           NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LRXN1 = .FALSE.
         
         END DO

            IF( LRXN1 ) THEN

               E2 = LEN_TRIM( VNAME )
               LINOUT = '      ' // VNAME( 1 : E2 ) // ' =  0.0D0'
               E1 = LEN_TRIM( LINOUT )
               WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            END IF


c..loss terms

            VNAME = '   LOSS(  ' // SPEC( 1 : CL ) // ' )'


            LRXN1 = .TRUE.
            DO N = 1, NRXNS

               CALL SUM_COEFF( RCOEF, 'R', IND, N )
               CALL SUM_COEFF( YCOEF, 'P', IND, N )

               IF( RCOEF .LE. YCOEF ) CYCLE

               COEFF = RCOEF - YCOEF
            
               NPOS = 30
               RPOS1 = IND
               RPOS2 = 0
               PPOS1 = IND
               PPOS2 = 0


               CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &              NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

               E1 = LEN_TRIM( LINOUT )
               WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

               LRXN1 = .FALSE.
         
            END DO

            IF( LRXN1 ) THEN

               E2 = LEN_TRIM( VNAME )
               LINOUT = '      ' // VNAME( 1 : E2 ) // ' =  0.0D0'
               E1 = LEN_TRIM( LINOUT )
               WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            END IF

         END DO

         LINOUT = '      END IF' 
         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

      END IF


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Production/loss terms for AQ species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( N_AQ_SPC .GT. 0 ) THEN

         LINOUT = '      IF( L_AQ_VRSN ) THEN' 
         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )
         
         DO S = 1, N_AQ_SPC

            IND = NAME_INDEX( AQ_SPC( S ) , N_SPECIES, SPECIES )

            WRITE( IOUT, 92000 )

            SPEC = ADJUSTL( AQ_SPC( S ) )

            VNAME = '   PROD(  ' // SPEC( 1 : CL ) // ' )'


c..production terms
            LRXN1 = .TRUE.
            DO N = 1, NRXNS

               CALL SUM_COEFF( RCOEF, 'R', IND, N )
               CALL SUM_COEFF( YCOEF, 'P', IND, N )

            IF( YCOEF .LE. RCOEF ) CYCLE

            COEFF = YCOEF - RCOEF
            
            NPOS = 30
            RPOS1 = 0
            RPOS2 = 0
            PPOS1 = IND
            PPOS2 = 0

            CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &           NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LRXN1 = .FALSE.
         
         END DO

            IF( LRXN1 ) THEN

               E2 = LEN_TRIM( VNAME )
               LINOUT = '      ' // VNAME( 1 : E2 ) // ' =  0.0D0'
               E1 = LEN_TRIM( LINOUT )
               WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            END IF


c..loss terms

            VNAME = '   LOSS(  ' // SPEC( 1 : CL ) // ' )'


            LRXN1 = .TRUE.
            DO N = 1, NRXNS

               CALL SUM_COEFF( RCOEF, 'R', IND, N )
               CALL SUM_COEFF( YCOEF, 'P', IND, N )

               IF( RCOEF .LE. YCOEF ) CYCLE

               COEFF = RCOEF - YCOEF
            
               NPOS = 30
               RPOS1 = IND
               RPOS2 = 0
               PPOS1 = IND
               PPOS2 = 0


               CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &              NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

               E1 = LEN_TRIM( LINOUT )
               WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

               LRXN1 = .FALSE.
         
            END DO

            IF( LRXN1 ) THEN

               E2 = LEN_TRIM( VNAME )
               LINOUT = '      ' // VNAME( 1 : E2 ) // ' =  0.0D0'
               E1 = LEN_TRIM( LINOUT )
               WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            END IF

         END DO

         LINOUT = '      END IF' 
         E1 = LEN_TRIM( LINOUT )
         WRITE( IOUT, 92000 )
         WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

      END IF


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Production terms for negative stoichiometry
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LPAR_NEG ) THEN

         WRITE( IOUT, 92000 )

         S = NAME_INDEX( 'PAR', N_SPECIES, SPECIES )

         VNAME = 'PNEG( PAR )'

c..production terms
         LRXN1 = .TRUE.
         DO N = 1, NRXNS

            CALL SUM_COEFF( RCOEF, 'R', S, N )
            CALL SUM_COEFF( YCOEF, 'P', S, N )

            IF( YCOEF .GE. 0.0 ) CYCLE
             
            IF( RCOEF - ABS( YCOEF ) .GE. 0.0D0 ) CYCLE

            COEFF = ABS( YCOEF )
            
            NPOS = 30
            RPOS1 = 0
            RPOS2 = 0
            PPOS1 = S
            PPOS2 = 0

            CALL BLD_OUTLINE( 'RXRAT', VNAME, '   ', 0, COEFF, N, GRPNO,  
     &           NPOS, LINOUT, LRXN1, RPOS1, RPOS2, PPOS1, PPOS2 )

            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

            LRXN1 = .FALSE.
         
         END DO

         IF( LRXN1 ) THEN

            E2 = LEN_TRIM( VNAME )
            LINOUT = '      ' // VNAME( 1 : E2 ) // ' =  0.0D0'
            E1 = LEN_TRIM( LINOUT )
            WRITE( IOUT, 92000 ) LINOUT( 1 : E1 )

         END IF

      END IF

      WRITE( IOUT, 97000 )

      CLOSE( IIN )

      CLOSE( IOUT )

      NOUTFLS = NOUTFLS + 1
      OUTFLNAM( NOUTFLS ) = 'hrprodloss.F'


      RETURN 

 9000 MSG = 'ERROR: Could not open ' // FNAME( 1 : LEN_TRIM( FNAME ) )

      WRITE(LOGDEV,'(a)')TRIM( PNAME ) // ': ' // TRIM( MSG )
      STOP
       
92000 FORMAT( A )
92020 FORMAT( / )

93000 FORMAT( 'C  PRECONDITIONS: For the ', A, ' mechanism' )
93020 FORMAT( 'C  REVISION HISTORY: Created by EBI solver program, ', A )
96000 FORMAT( //6X, 'RETURN' // 6X, 'END' )

97000 FORMAT( /
     & '      RETURN' //
     & '      END' )
          
      END

