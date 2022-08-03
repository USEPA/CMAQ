
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

      MODULE GET_MECHDEF_DATA

        IMPLICIT NONE

      CONTAINS
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_OPERATOR ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD)

C=======================================================================
C sets up coefficients, rate coefficients, species concentration and 
C existing operators need to calculate a special rate coefficients. 
C input:   IMECH (for RDLINE), NSPECIAL, SPECIAL,
C updates: NKC_TERMS, KC_TERMS, KC_COEFF, N_OPERATORS, OPERATORS, 
C          OPERATOR_COEFFS
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE

C Inputs

      CHARACTER(  1 ), INTENT( INOUT ) :: CHR
      CHARACTER( 16 ), INTENT( INOUT ) :: WORD
      CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT(   IN  ) :: IMECH
      INTEGER,         INTENT( INOUT ) :: LPOINT
      INTEGER,         INTENT( INOUT ) :: IEOL

C Local
      
      INTEGER, EXTERNAL  :: INDEX1
      INTEGER            :: ICHR, L , NDX
      LOGICAL            :: LCOEFF, LNEG
      INTEGER, SAVE      :: ITAB
      REAL( 8 )           :: NUMBER

      ITAB = 0  ! ICOL = 3 initially for each reaction
      ITAB = ITAB + 1
      LCOEFF = .FALSE.
      LNEG = .FALSE.     
      IF ( CHR .EQ. '+' ) THEN
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      ELSE IF ( CHR .EQ. '-' ) THEN
         LNEG = .TRUE.
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      END IF

      ICHR = ICHAR ( CHR )
C characters 0,1,2,...,9
      IF ( ICHR .GE. 48 .AND. ICHR .LE. 57 ) LCOEFF = .TRUE.
      IF ( CHR .EQ. '.' ) LCOEFF = .TRUE.
      IF ( LCOEFF ) THEN
         CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
         IF ( LNEG )NUMBER = -NUMBER
         IF ( CHR .EQ. '*' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         ELSE
            WRITE( *,2003 ) INBUF
            STOP
         END IF
      ELSE IF ( LNEG ) THEN
         NUMBER = -1.0
      ELSE
         NUMBER =  1.0
      END IF
 
      SELECT CASE( CHR )
         CASE( 'C' )
           NKC_TERMS( NSPECIAL ) = NKC_TERMS( NSPECIAL ) + 1
           L  = NKC_TERMS( NSPECIAL )
           INDEX_KTERM( NSPECIAL, L ) = 0
           IF( L .GT. MAXSPECTERMS )THEN
               WRITE( *, 2005)SPECIAL(NSPECIAL)
               STOP
           END IF        

           KC_COEFFS( NSPECIAL, L ) = NUMBER

           CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
           IF( CHR .EQ. '<' )THEN
              CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, 
     &                      KC_TERMS( NSPECIAL, L, 2 ) )
           ELSE
              WRITE( *, 2009)SPECIAL(NSPECIAL),INBUF(LPOINT:IEOL)
              STOP
           END IF
         CASE( 'K' )

           NKC_TERMS( NSPECIAL ) = NKC_TERMS( NSPECIAL ) + 1
           L  = NKC_TERMS( NSPECIAL )
           IF( L .GT. MAXSPECTERMS )THEN
               WRITE( *, 2005)SPECIAL(NSPECIAL)
               STOP
           END IF        

           KC_COEFFS( NSPECIAL, L ) = NUMBER

           CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
           IF( CHR .EQ. '<' )THEN
              CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, 
     &                      KC_TERMS( NSPECIAL, L, 1 ) )
           ELSE
              WRITE( *, 2009)SPECIAL(NSPECIAL),INBUF(LPOINT:IEOL)
              STOP
           END IF
           IF( CHR .EQ. '*' )THEN
              CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
              IF( CHR .EQ. 'C' )THEN
                 CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
                 IF( CHR .EQ. '<' )THEN
                    CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, 
     &                              KC_TERMS( NSPECIAL, L, 2 ) )
                 ENDIF           
              ELSE                   
                 WRITE( *, 2010)SPECIAL(NSPECIAL),INBUF(LPOINT:IEOL)
                 STOP
              END IF
           ELSE IF( CHR .EQ. '+' .OR. CHR .EQ. '-')THEN
              KC_TERMS( NSPECIAL, L, 2 ) = 'FIRST_ORDER_RATE'
           ELSE IF( CHR .EQ. ';')THEN
              KC_TERMS( NSPECIAL, L, 2 ) = 'FIRST_ORDER_RATE'
           END IF

c         print*,'For ',SPECIAL(NSPECIAL)(1:LEN_TRIM(SPECIAL(NSPECIAL))),
c     &          ' ',KC_COEFFS(NSPECIAL, L ),
c     &          ' ',KC_TERMS( NSPECIAL, L, 1:2 )
         CASE( 'R' )
           CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
           N_OPERATORS( NSPECIAL) = N_OPERATORS( NSPECIAL) + 1
           L = N_OPERATORS( NSPECIAL)
           IF( L .GT. MAXSPECTERMS )THEN
               WRITE( *, 2007)SPECIAL(NSPECIAL)
               STOP
           END IF        
           NDX = INDEX1( WORD, (NSPECIAL-1), SPECIAL)
           IF( NDX .NE. 0 )THEN
               OPERATORS(NSPECIAL, L )       = NDX
               OPERATOR_COEFFS(NSPECIAL, L ) = NUMBER
           ELSE
               WRITE( *, 2011)SPECIAL(NSPECIAL),INBUF
               STOP
           END IF
c         print*,'For ',SPECIAL(NSPECIAL)(1:LEN_TRIM(SPECIAL(NSPECIAL))),
c     &          ' ',OPERATOR_COEFFS(NSPECIAL, L ),
c     &          ' ',SPECIAL(OPERATORS( NSPECIAL, L))
         CASE DEFAULT 
            WRITE( *, 2010)SPECIAL(NSPECIAL),INBUF
            STOP
      END SELECT
      
      RETURN

2001  FORMAT( / 5X, 'ERROR: Equal sign expected after reactants'
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, 'ERROR: An asterisk must follow a coefficient'
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, 'ERROR: Maximum number of product terms exceeded'
     &        / 5X, 'for Special Rate Coefficient:', A16 )
2007  FORMAT( / 5X, 'ERROR: Maximum number of operator terms exceeded'
     &        / 5X, 'for Special Rate Coefficient:', A16 )
2009  FORMAT( / 5X, 'ERROR: Bad K Term in of product terms for ', 
     &          A16, 'Last line Segment read:' / A81 ) 
2010  FORMAT( / 5X, 'ERROR: Bad Term in SPECIAL RATE COEFFICIENT ', 
     &          A16, 'Last line Segment read:' / A81 ) 
2011  FORMAT( / 5X, 'ERROR: Undefined Operator in in SPECIAL RATE COEFFICIENT ',
     &          A16, 'Last line Segment read:' / A81 ) 
      END SUBROUTINE GET_OPERATOR
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_SS_DATA ( LUNOUT, NR )
!     &                         N_SS_SPC, 
!     &                         SS_SPC, 
!     &                         SS_RCT_COEF,
!     &                         SS_PRD_COEF, 
!     &                         MAX_SS_LOSS, 
!     &                         MAX_SS_PROD, 
!     &                         N_LOSS_RXNS, 
!     &                         N_PROD_RXNS, 
!     &                         SS_LOSS_RXNS,
!     &                         SS_PROD_RXNS, 
!     &                         SS_PROD_COEF, 
!     &                         SS_RCT_IND )

C=======================================================================
C Formulates data needed by chemistry solvers to compute concentrations
C of steady-state species and adjust reaction rates correspondingly
C=======================================================================


      USE MECHANISM_DATA
      
      IMPLICIT NONE

C..Input Arguments
      INTEGER, INTENT ( IN )         :: LUNOUT                             ! Output unit number
      INTEGER, INTENT ( IN )         :: NR                                 ! No. of reactions
!      INTEGER, INTENT ( IN )         :: N_SS_SPC                           ! No. of input steady-state species
!      CHARACTER( 16 ), INTENT ( IN ) :: SS_SPC( MAXNLIST )                  ! List of input steady-state species
!      INTEGER, INTENT ( INOUT )      ::  SS_RCT_COEF( MAXNLIST, MAXRXNUM )  ! coefficients for SS reactants
!      REAL,    INTENT ( INOUT )      ::  SS_PRD_COEF( MAXNLIST, MAXRXNUM )  ! coefficients for SS products
!      INTEGER, INTENT ( INOUT )      ::  MAX_SS_LOSS          ! Max no of reactions for which 1 SS species
                                                               ! appears as a reactant
!      INTEGER, INTENT ( INOUT )      ::  MAX_SS_PROD          ! Max no of reactions for which 1 SS species
                                                               ! appears as a product
!      INTEGER, INTENT ( INOUT )      ::  N_LOSS_RXNS( MAXNLIST )             ! No. of loss rxns for each SS species
!      INTEGER, INTENT ( INOUT )      ::  N_PROD_RXNS( MAXNLIST )             ! No. of prod rxns for each SS species
!      INTEGER, INTENT ( INOUT )      ::  SS_LOSS_RXNS( MAXNLIST, MAXRXNUM )  ! List of rxns in which SS species is a reactant
!      INTEGER, INTENT ( INOUT )      ::  SS_PROD_RXNS( MAXNLIST, MAXRXNUM )  ! List of rxns in which SS species is a product
!      INTEGER, INTENT ( INOUT )      ::  SS_RCT_IND( MAXRXNUM )              ! Index of SS species that reacts 
!      REAL,    INTENT ( INOUT )      ::  SS_PROD_COEF( MAXNLIST, MAXRXNUM )  ! Yields for rxns producing a SS species

c..Local variables

      INTEGER           :: SS1           ! Loop index
      INTEGER           :: NRX           ! Loop index
      INTEGER           :: IRCT          ! Counter
      INTEGER           :: IPRD          ! Counter
     

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  For each SS species, loop through reactions and build arrays containing reaction number
c  indices and product coefficients for each SS species 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..Initialize arrays
      SS_LOSS_RXNS = 0
      SS_PROD_RXNS = 0
      SS_PROD_COEF = 0.0

      MAX_SS_LOSS = 0
      MAX_SS_PROD = 0


c..Fill em up
      DO SS1 = 1, N_SS_SPC

         IRCT = 0
         IPRD = 0

         DO NRX = 1, NR

            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0   ) THEN
               IRCT = IRCT + 1
               SS_LOSS_RXNS( SS1, IRCT ) = NRX
            ENDIF

            IF( SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN
               IPRD = IPRD + 1
               SS_PROD_RXNS( SS1, IPRD ) = NRX
               SS_PROD_COEF(  SS1, IPRD ) = SS_PRD_COEF( SS1, NRX )
            ENDIF
 
         ENDDO

         N_LOSS_RXNS( SS1 ) = IRCT
         N_PROD_RXNS( SS1 ) = IPRD

         MAX_SS_LOSS = MAX( MAX_SS_LOSS, N_LOSS_RXNS( SS1 ) ) 
         MAX_SS_PROD = MAX( MAX_SS_PROD, N_PROD_RXNS( SS1 ) ) 

      ENDDO

c..Identify all reactions in which a SS species reacts with other reactants; 
c..Load index of the SS species reacting in each reaction (note: can only be one SS
c..species per rxn - SS species not allowed to react with other SS species)

      DO NRX = 1, NR

         DO SS1 = 1, N_SS_SPC
            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0 .AND. NREACT( NRX ) .GT. 0 )
     &          SS_RCT_IND( NRX ) = SS1
         ENDDO 

      ENDDO

      RETURN

      END SUBROUTINE GET_SS_DATA
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )

C==============================================================================
C GETCHAR returns the next character excluding white space and comments and
C sets LPOINT, the pointer to the current character being accessed.
C input: IMECH (for RDLINE)
C output: CHR
C updates: INBUF, LPOINT, IEOL
C precondition:  RDLINE must have been called
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

      CHARACTER*( * ), INTENT( INOUT ) :: CHR
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( IN )    :: IMECH
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT

101   CONTINUE
      LPOINT = LPOINT + 1
      IF ( LPOINT .GT. IEOL ) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         GO TO 101
      ELSE
         CHR = INBUF( LPOINT:LPOINT )
         IF ( CHR .EQ. ' ' .OR.
     &      ICHAR( CHR ) .EQ. 09 ) THEN   ! (HT = horizontal tab)
            GO TO 101
         ELSE IF ( CHR .EQ. '(' .OR. CHR .EQ. '{' ) THEN
            CALL EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )
            GO TO 101            
         END IF
      END IF
      RETURN
      END SUBROUTINE GETCHAR
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL )

C=======================================================================
C Returns a label that consists of an extended alphanumeric character 
C string no longer than MAXLEN bytes and delimited by "<", ">".
C The label string can span lines, may have embedded comments, and
C may contain up to 81 characters, excluding comments.
C GETLABEL removes embedded blank characters.
C Valid string characters - All printable characters EXCEPT:
C ASCII char range:      contiguous (decimal) range:
C      '<'                    60           label string delimiter
C      '>'                    62           label string delimiter
C      '{'                    123          comment delimiter
C      '}'                    125          comment delimiter
C      '(', ')'               40-41        comment delimiters
C input: IMECH (for RDLINE), INBUF, LPOINT, CHR
C output: WORD
C updates: INBUF, LPOINT, IEOL, CHR
C precondition:  RDLINE and GETCHAR must have been called and the 1st
C                non-comment character found was "<"
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE
! Arguments
      INTEGER,         INTENT( IN )    :: IMECH
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
      CHARACTER*( * ), INTENT( INOUT ) :: CHR
      CHARACTER*( * ), INTENT( INOUT ) :: LABEL

! Local
      INTEGER, PARAMETER   :: MAXLEN = 16
      CHARACTER( MAXLEN )  :: BLANK = ' '
      CHARACTER( 81 )      :: STRBUF 
      INTEGER              :: LENSTR, ISPC

C eat the start-of-string delimiter ('<')
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      LABEL = BLANK
      LENSTR = 0
101   CONTINUE
      IF ( CHR .EQ. '>' ) GO TO 201  ! end-of-string delimiter ('>')
C check for valid character
      IF ( .NOT. VALLABCHR( CHR ) ) THEN
         WRITE( 6,2001 ) INBUF, CHR
         STOP
      END IF
      LENSTR = LENSTR + 1
      IF ( LENSTR .GT. 81 ) THEN
         WRITE( 6,2003 ) INBUF
         STOP
      END IF
C insert into STRBUF
      STRBUF( LENSTR:LENSTR ) = CHR
C get next non-blank character
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      GO TO 101
201   CONTINUE
      IF ( LENSTR .GT. MAXLEN )THEN
         LENSTR = MAXLEN
	 WRITE( 6,2004 )TRIM(STRBUF),STRBUF( 1:LENSTR )
      END IF	 
      LABEL = STRBUF( 1:LENSTR )
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      RETURN
2001  FORMAT( / 5X, '*** ERROR: Invalid character in a label'
     &        / 5X, 'Line:' / A81,
     &        / 5X, 'Character:', 2X, A1 )
2003  FORMAT( / 5X, '*** ERROR: label buffer cannot exceed 81 characters:'
     &        / 5X, 'Line:'/ A81 )
2004  FORMAT( / 5X, '*** WARNING: label buffer: ' / A, ' exceeds 16 characters:'
     &        / 5X, '*** Trunicating to :'/ A16 )
      END SUBROUTINE GETLABEL
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETPRDCT ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, 
     &                      N_DROP_SPC, DROP_SPC ) 
      USE MECHANISM_DATA
      
      IMPLICIT NONE


c...arguments

      INTEGER,         INTENT(   IN  ) :: IMECH
      CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( INOUT ) :: LPOINT
      INTEGER,         INTENT( INOUT ) :: IEOL
      CHARACTER(  1 ), INTENT( INOUT ) :: CHR
      CHARACTER( 16 ), INTENT( INOUT ) :: WORD
      INTEGER,         INTENT(   IN  ) :: NXX
      INTEGER,         INTENT( INOUT ) :: NS
      CHARACTER( 16 ), INTENT( INOUT ) :: SPCLIS( : )
      INTEGER,         INTENT( INOUT ) :: SPC1RX( : )
      INTEGER,         INTENT( INOUT ) :: ICOL
      INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
      CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( : )

c... local

      CHARACTER( 16 )         :: SPECIES
      CHARACTER( 16 ),  SAVE  :: RXN_PRODUCTS( MAXPRODS ) ! unique list of nonconstant products
      CHARACTER( 16 ),  SAVE  :: ALL_PRODUCTS( MAXPRODS + NCONSTANT_SPECIES ) ! unique list of all products
      INTEGER                 :: NSPEC
      INTEGER                 :: ICHR
      INTEGER                 :: PRODUCT_INDEX ! pointer for nonconstant product
      INTEGER                 :: SPECIES_INDEX ! pointer for nonconstant and constant products
      LOGICAL                 :: LCOEFF
      LOGICAL                 :: LNEG
      LOGICAL                 :: CONSTANT_SPECIES
      INTEGER, SAVE           :: ITAB
      INTEGER, SAVE           :: JTAB
      INTEGER                 :: ISPC
      INTEGER, SAVE           :: NUMB_PRODUCTS ! number of unique reaction products
      INTEGER, SAVE           :: REAL_PRODUCTS ! number of unique nonconstat reaction products
      REAL( 8 )               :: NUMBER

c..ELIMINATE related variables

c..STEADY_STATE related variables
      INTEGER            :: SS_INDEX
      REAL               :: SS_COEF
 
      INTEGER, EXTERNAL :: INDEX1

      IF ( ICOL .EQ. 3 )THEN  ! ICOL = 3 initially for each reaction
           NUMB_PRODUCTS = 0  
           REAL_PRODUCTS = 0  
           RXN_PRODUCTS  = '                '
           ALL_PRODUCTS  = '                '
           ITAB          = 0
           JTAB          = 0  
      ENDIF
      ITAB = ITAB + 1
      JTAB = JTAB + 1
      LCOEFF = .FALSE.
      LNEG = .FALSE.     
      IF ( CHR .EQ. '+' ) THEN
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      ELSE IF ( CHR .EQ. '-' ) THEN
         LNEG = .TRUE.
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      END IF
      ICHR = ICHAR ( CHR )
C characters 0,1,2,...,9
      IF ( ICHR .GE. 48 .AND. ICHR .LE. 57 ) LCOEFF = .TRUE.
      IF ( CHR .EQ. '.' ) LCOEFF = .TRUE.
      IF ( LCOEFF ) THEN
         CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
         IF ( CHR .EQ. '*' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         ELSE
            WRITE( *,2003 ) INBUF
            STOP
         END IF
      END IF
      CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
      SPECIES = WORD

c..Skip any product that is in the eliminate list
      IF( INDEX1( SPECIES, N_DROP_SPC, DROP_SPC ) .NE. 0 ) THEN
         ITAB = ITAB - 1
         RETURN
      ENDIF

c..Skip any steady-state species, but sum-up its coefficients in each reaction
      SS_INDEX = INDEX1( SPECIES, N_SS_SPC, SS_SPC )
      IF( SS_INDEX .NE. 0 ) THEN
         IF(       LCOEFF .AND.       LNEG )    SS_COEF = -NUMBER
         IF(       LCOEFF .AND. .NOT. LNEG )    SS_COEF = NUMBER
         IF( .NOT. LCOEFF .AND.       LNEG )    SS_COEF = -1.0
         IF( .NOT. LCOEFF .AND. .NOT. LNEG )    SS_COEF = 1.0
         SS_PRD_COEF( SS_INDEX, NXX ) = SS_PRD_COEF( SS_INDEX, NXX ) + SS_COEF
         ITAB = ITAB - 1
         RETURN
      ENDIF

      CONSTANT_SPECIES = .FALSE.

      IF ( SPECIES( 1:4 ) .EQ. 'M   ' .OR. SPECIES( 1:4 ) .EQ. 'm   ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 8)
         CONSTANT_SPECIES = .TRUE.
         NUMB_PRODUCTS    = NUMB_PRODUCTS + 1
         SPECIES_INDEX    = NUMB_PRODUCTS
         INDEX_PRODUCT( NXX, NUMB_PRODUCTS ) = -IND_M
         ALL_PRODUCTS( NUMB_PRODUCTS )        = 'M   '
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'H2O ' .OR. SPECIES( 1:4 ) .EQ. 'h2o ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 9)
         CONSTANT_SPECIES = .TRUE.
         NUMB_PRODUCTS    = NUMB_PRODUCTS + 1
         SPECIES_INDEX    = NUMB_PRODUCTS
         INDEX_PRODUCT( NXX, NUMB_PRODUCTS ) = -IND_H2O
         ALL_PRODUCTS( NUMB_PRODUCTS )        = 'H2O '
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'O2  ' .OR. SPECIES( 1:4 ) .EQ. 'o2  ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 10)
         CONSTANT_SPECIES = .TRUE.
         NUMB_PRODUCTS    = NUMB_PRODUCTS + 1
         SPECIES_INDEX    = NUMB_PRODUCTS
         INDEX_PRODUCT( NXX, NUMB_PRODUCTS ) = -IND_O2
         ALL_PRODUCTS( NUMB_PRODUCTS )        = 'O2  '
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'N2  ' .OR. SPECIES( 1:4 ) .EQ. 'n2  ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 11)
         CONSTANT_SPECIES = .TRUE.
         NUMB_PRODUCTS    = NUMB_PRODUCTS + 1
         SPECIES_INDEX    = NUMB_PRODUCTS
         INDEX_PRODUCT( NXX, PRODUCT_INDEX ) = -IND_N2
         ALL_PRODUCTS( NUMB_PRODUCTS )        = 'N2  '
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'h2  ' .OR. SPECIES( 1:4 ) .EQ. 'H2  ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 12)
         CONSTANT_SPECIES = .TRUE.
         NUMB_PRODUCTS    = NUMB_PRODUCTS + 1
         SPECIES_INDEX    = NUMB_PRODUCTS
         INDEX_PRODUCT( NXX, NUMB_PRODUCTS ) = -IND_H2
         ALL_PRODUCTS( NUMB_PRODUCTS )        = 'H2  '
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'CH4 ' .OR. SPECIES( 1:4 ) .EQ. 'ch4 ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 14)
         CONSTANT_SPECIES = .TRUE.
         NUMB_PRODUCTS    = NUMB_PRODUCTS + 1
         SPECIES_INDEX    = NUMB_PRODUCTS
         INDEX_PRODUCT( NXX, NUMB_PRODUCTS ) = -IND_CH4
         ALL_PRODUCTS( NUMB_PRODUCTS )        = 'CH4 '

      ELSE  ! dynamic species in chemical mechanism 
      
         CONSTANT_SPECIES = .FALSE.
c..Check if mechanism species is already counted on nonconstant reaction product list
         IF( REAL_PRODUCTS .NE. 0 )THEN
!             write(6,'(50(A,1X))')' Check for new mechanism product: ',TRIM( RXLABEL(NXX)),
!     &                             TRIM( SPECIES ),(RXN_PRODUCTS(ISPC),ISPC=1,REAL_PRODUCTS)
             PRODUCT_INDEX = INDEX1( SPECIES, REAL_PRODUCTS, RXN_PRODUCTS )
         ELSE
             PRODUCT_INDEX = 0
         END IF
         IF( PRODUCT_INDEX .NE. 0 ) THEN
            ITAB = ITAB - 1
            WRITE( 6, 2006 )NXX, TRIM(RXLABEL(NXX)), TRIM( SPECIES )
2006  FORMAT('REACTION# ', I5, ' : ', A,' has a reoccurance for mechanism product ', A,
     &       ' adjusting product and coefficient arrays.')
         ELSE
            REAL_PRODUCTS  = REAL_PRODUCTS + 1
            PRODUCT_INDEX  = REAL_PRODUCTS
            RXN_PRODUCTS( REAL_PRODUCTS ) = SPECIES
            ICOL = ICOL + 1
            IF ( ICOL .GT. MAXPRODS+3 ) THEN
                WRITE( *,2005 ) INBUF
               STOP
            END IF
            CALL LKUPSPEC ( NS, SPECIES, SPCLIS, NXX, SPC1RX, NSPEC )
            IRR( NXX,ICOL ) = NSPEC
         ENDIF
c..Check if mechanism species is already counted on all products list
         IF( NUMB_PRODUCTS .NE. 0 )THEN
!             write(6,'(50(A,1X))')' Check for new product from all products: ',TRIM( RXLABEL(NXX)),
!     &                             TRIM( SPECIES ),(ALL_PRODUCTS(ISPC),ISPC=1,NUMB_PRODUCTS)
             SPECIES_INDEX = INDEX1( SPECIES, NUMB_PRODUCTS, ALL_PRODUCTS )
         ELSE
             SPECIES_INDEX = 0
         END IF
         IF( SPECIES_INDEX .NE. 0 ) THEN
            JTAB = JTAB - 1
!            WRITE( 6, 2007 )NXX, TRIM(RXLABEL(NXX)), TRIM( SPECIES )
2007  FORMAT('REACTION# ', I5, ' : ', A,' has a reoccurance for among all products ', A,
     &       ' adjusting product and coefficient arrays.')
         ELSE
            NUMB_PRODUCTS  = NUMB_PRODUCTS + 1
            SPECIES_INDEX  = NUMB_PRODUCTS
            IF ( NUMB_PRODUCTS .GT. MAXPRODS + NCONSTANT_SPECIES  ) THEN
                WRITE( *,2005 ) INBUF
               STOP
            END IF
            ALL_PRODUCTS( NUMB_PRODUCTS )       = SPECIES
            CALL LKUPSPEC ( NS, SPECIES, SPCLIS, NXX, SPC1RX, NSPEC )
            INDEX_PRODUCT( NXX, NUMB_PRODUCTS ) = NSPEC
         ENDIF

      END IF  ! SPECIES .EQ. 'M   '

      IF ( LCOEFF ) THEN
         IF ( LNEG ) THEN
            IF( .NOT. CONSTANT_SPECIES ) THEN
               SC( NXX, PRODUCT_INDEX ) = -NUMBER + SC( NXX, PRODUCT_INDEX )
            END IF
            STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX) = -NUMBER + STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX)
         ELSE
            IF( .NOT. CONSTANT_SPECIES ) THEN
               SC( NXX, PRODUCT_INDEX ) = NUMBER  + SC( NXX, PRODUCT_INDEX )
            END IF
            STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX) = NUMBER + STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX)
         END IF
      ELSE IF ( LNEG ) THEN
         IF( .NOT. CONSTANT_SPECIES ) THEN
            SC( NXX,PRODUCT_INDEX ) = -1.0D0  + SC( NXX,PRODUCT_INDEX )
         END IF
         STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX) = -1.0D0 + STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX)         
      ELSE 
         IF ( .NOT. CONSTANT_SPECIES ) THEN
            SC( NXX, PRODUCT_INDEX ) = 1.0D0  + SC( NXX,PRODUCT_INDEX )
         END IF
         STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX) = 1.0D0 + STOICHIOMETRIC_COEFF(NXX,SPECIES_INDEX)         
      END IF
      
      N_ALL_PRODUCTS( NXX ) = NUMB_PRODUCTS

      RETURN
2001  FORMAT( / 5X, '*** ERROR: Equal sign expected after reactants'
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: An asterisk must follow a coefficient'
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: Maximum number of products exceeded'
     &        / 5X, 'Last line read was:' / A81 )      
      END SUBROUTINE GETPRDCT
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETRATE ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                     NXX, LABEL, IP )

C=======================================================================
C sets up rate constant information
C input:
C         NXX          = current rx index
C         LABEL        = possible rx label
C                        LABEL(NXX,1): 1st label found in rx NXX
C                        LABEL(NXX,2): 2nd label found in rx NXX
C output:
C         KTYPE(IRX)   = rx type
C         IRXBITS      = Bit test mask vector for selected reactions
C         NFALLOFF     = Number of falloff reactions
C         IRRFALL      = Reactions list pointer to falloff reactions
C         IPH(IP,1)    = Mech. rx number for this phot rx
C         IPH(IP,2)    = Photolysis table index
C         IPH(IP,3)    = 1, if dependent photolysis rx, else = 0
C         IP           = Total number of phot rx's
C         NPHOTAB      = Number of photolysis tables found
C         PHOTAB       = photolysis table list
C         ISPECIAL(IP,1)    = Mech. rx number for this special rate coeff.
C         IPH(IP,2)         = Special rate coeff. index
C         NSPECIAL_RXN      = Total number of rx's using special rates
C         NSPECIAL          = Number of special rate coefficients
C         PHOTAB            = List of Special Rate Coefficients
C         KTNi         = Number of type i reactions
C         KRXi         = Reactions list pointer to type i reactions
C         RTDAT        = Kinetic reaction rates expressions components
C         RFDAT        = Falloff reaction rates expressions components
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE
 
      CHARACTER(  1 ), INTENT( INOUT ) :: CHR
      CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( IN )    :: IMECH
      INTEGER,         INTENT( INOUT ) :: LPOINT
      INTEGER,         INTENT( INOUT ) :: IEOL
      INTEGER,         INTENT( INOUT ) :: IP
      INTEGER,         INTENT( IN )    :: NXX
      CHARACTER( 16 ), INTENT( INOUT ) :: LABEL( :,: )

C...local variable

      REAL( 8 )          ::  NUMBER
      CHARACTER( 16 )    :: TAG
      INTEGER            :: NDX
      INTEGER, EXTERNAL  :: INDEX1

      INTEGER            :: NUMANDS, NUMREALS, IRX
      INTEGER            :: LSTART, LSTOP
      
      INTEGER,  SAVE    :: IH = 0
      
      LOGICAL            :: NOT_POWER

      NUMANDS = 0         ! no. of ampersands in mech. description (falloff)
      NUMREALS = 0        ! counter to switch signs (falloff)

C '#' or '%' signals beginning of part of line that has rate constant data
      IF ( CHR .EQ. '#' ) THEN
         
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )

         KTYPE( NXX ) = 1   ! posit type 1 standard reaction
         RTDAT( 1,NXX ) = NUMBER



         IF ( CHR .NE. '^' .AND.    ! type 2 or 4
     &        CHR .NE. '@' .AND.    ! type 3 or 4
     &        CHR .NE. '&' .AND.    ! falloff
     &        CHR .NE. '*' .AND.    ! 'refer back'
     &        CHR .NE. '?' .AND.    ! Special Rate Constant
     &        CHR .NE. '/' .AND.    ! photo
     &        CHR .NE. '~' .AND.    ! heteorogeneous
     &        CHR .NE. ';' ) THEN   ! eol
            WRITE( *, 2001 ) NXX, INBUF
            STOP
         END IF


101      CONTINUE


         IF ( CHR .EQ. '^' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL (IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            IF ( NUMANDS .EQ. 0 ) THEN
               KTYPE( NXX ) = 2
               RTDAT( 2,NXX ) = NUMBER
            ELSE
               RFDAT( 2,NFALLOFF ) = NUMBER
            END IF               
            IF ( CHR .NE. '@' .AND. 
     &           CHR .NE. '&' .AND. 
     &           CHR .NE. ';' ) THEN
               WRITE( *,2003 ) NXX, INBUF
               STOP
            END IF
            GO TO 101
         END IF     ! CHR .EQ. '^'

         IF ( CHR .EQ. '@' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            IF ( CHR .EQ. '*' ) THEN  !  reverse equilibrium
               KTYPE( NXX ) = 5
               RTDAT( 2,NXX ) = -1.0D0 * NUMBER
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               IF ( CHR .NE. 'E' ) THEN
                  WRITE( *,2005 ) NXX, INBUF
                  STOP
               ELSE
                  CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
                  IF ( CHR .EQ. '<' ) THEN
                     CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                               LABEL( NXX,2 ) )
                  ELSE
                     WRITE( *,2007 ) NXX, INBUF
                     STOP
                  END IF
               END IF
               IF ( CHR .NE. ';' ) THEN
                  WRITE( *,2009 ) NXX, INBUF
                  STOP
               ELSE
                  GO TO 901
               END IF
            ELSE       ! CHR .ne. '*'
               IF ( KTYPE( NXX ) .EQ. 1 ) THEN
                  KTYPE( NXX ) = 3
               ELSE IF ( KTYPE( NXX ) .EQ. 2 ) THEN
                  KTYPE( NXX ) = 4
               END IF
               IF ( NUMANDS .EQ. 0 ) THEN
                  RTDAT( 3,NXX ) = -1.0D0 * NUMBER
               ELSE
                  RFDAT( 3,NFALLOFF ) = -1.0D0 * NUMBER
               END IF
               GO TO 101
            END IF        ! CHR .EQ. '*'
         END IF      ! CHR .EQ. '@'

         IF ( CHR .EQ. '&' ) THEN
            NUMANDS = NUMANDS + 1
            IF ( NUMANDS .EQ. 1 ) THEN
               IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 0 )
               NFALLOFF = NFALLOFF + 1
               IRRFALL( NFALLOFF ) = NXX
               KTYPE( NXX ) = 10
               RFDAT( 4,NFALLOFF ) = 0.6D0      ! default F
               RFDAT( 5,NFALLOFF ) = 1.0D0      ! default n
            END IF
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            IF ( NUMANDS .EQ. 1 ) THEN
               RFDAT( 1,NFALLOFF ) = NUMBER
            ELSE IF ( NUMANDS .EQ. 2 ) THEN  ! override default F
               RFDAT( 4,NFALLOFF ) = NUMBER
            ELSE IF ( NUMANDS .EQ. 3 ) THEN  ! override default n
               RFDAT( 5,NFALLOFF ) = NUMBER
            END IF
            IF ( NUMANDS .EQ. 1 ) THEN
               IF ( CHR .NE. '^' .AND. 
     &              CHR .NE. '@' .AND. 
     &              CHR .NE. '&' .AND. 
     &              CHR .NE. ';' ) THEN
                  WRITE( *,2011 ) NXX, INBUF
                  STOP
               END IF
            ELSE IF ( NUMANDS .EQ. 2 ) THEN
               IF ( CHR .NE. '&' .AND. 
     &              CHR .NE. ';' ) THEN
                  WRITE( *,2013 ) NXX, INBUF
                  STOP
               END IF
            ELSE ! if NUMANDS > 2
               IF ( CHR .NE. ';' ) THEN
                  WRITE( *,2014 ) NXX, INBUF
                  STOP
               END IF
            END IF
            GO TO 101
         END IF      ! CHR .EQ. '&'

         IF ( CHR .EQ. '~' ) THEN        ! heteorogeneous rx
            KTYPE( NXX ) = -1
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 0 )
            IH = IH + 1
            IHETERO( IH,1 ) = NXX
            MHETERO         = IH
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .EQ. '<' ) THEN
               CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, TAG )
               NDX = INDEX1 ( TAG, NHETERO, HETERO )
               IF ( NDX .NE. 0 ) THEN     ! table label already found
                  IHETERO( IH,2 ) = NDX  
               ELSE                  ! new table label
                  NHETERO            = NHETERO + 1
                  IHETERO( IH,2 )    = NHETERO
                  HETERO( NHETERO )  = TAG
               END IF
            ELSE
               WRITE( *,2034 ) NXX, INBUF
               STOP
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,'(A,1X,A)')'Attempting to Read Character', CHR
               WRITE( *,2017 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901
            END IF
         END IF
         
         IF ( CHR .EQ. '/' ) THEN        ! photolytic rx
            KTYPE( NXX ) = 0
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 1 )
            IP = IP + 1
            IPH( IP,1 ) = NXX
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .EQ. '<' ) THEN
               IPH( IP,3 ) = IP
               CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, TAG )
               NDX = INDEX1 ( TAG, NPHOTAB, PHOTAB )
               IF ( NDX .NE. 0 ) THEN     ! table label already found
                  IPH( IP,2 ) = NDX  
               ELSE                  ! new table label
                  NPHOTAB = NPHOTAB + 1
                  IPH( IP,2 ) = NPHOTAB
                  PHOTAB( NPHOTAB ) = TAG
               END IF
            ELSE IF ( CHR .EQ. '*' ) THEN
               IPH( IP,3 ) = 0
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               IF ( CHR .EQ. '<' ) THEN
                  CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL( NXX,2 ) )
C                 IPH(IP,2) to be resolved in caller (CHEMMECH.f)
               ELSE
                  WRITE( *,2007 ) NXX, INBUF
                  STOP
               END IF
            ELSE
               WRITE( *,2035 ) NXX, INBUF
               STOP
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2017 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901
            END IF
         END IF      ! CHR .EQ. '/'

         IF ( CHR .EQ. '*' ) THEN     ! linear dependency reaction
            KTYPE( NXX ) = 6     
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. 'K' ) THEN
               WRITE( *,2019 ) NXX, INBUF
               STOP
            ELSE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               IF ( CHR .EQ. '<' ) THEN
                  CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                            LABEL( NXX,2 ) )
               ELSE
                  WRITE( *,2007 ) NXX, INBUF
                  STOP
               END IF
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2021 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901 
            END IF
         END IF      ! CHR .EQ. '*'

         IF ( CHR .EQ. '?' ) THEN     ! Special rate coefficient

            KTYPE( NXX ) = 11
            
            NSPECIAL_RXN = NSPECIAL_RXN + 1
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, TAG )
            NDX = INDEX1 ( TAG, NSPECIAL, SPECIAL )

            IF ( NDX .NE. 0 ) THEN     ! special found
                  ISPECIAL( NSPECIAL_RXN,1 ) = NXX
                  ISPECIAL( NSPECIAL_RXN,2 ) = NDX  
            ELSE              
               WRITE( *,2041 ) NXX, INBUF
               STOP
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2042 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901 
            END IF
 
         ENDIF

          
         IF ( CHR .EQ. ';' ) GO TO 901

      ELSE IF ( CHR .EQ. '%' ) THEN
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         IF ( CHR .EQ. '4' .OR. CHR .EQ. '5' )THEN

            NRATE_STRING = NRATE_STRING + 1
            KSTRING( NRATE_STRING )      = NXX

            IF( CHR .EQ. '5' )THEN
               KTYPE( NXX ) = 14
               NSPECIAL_RXN = NSPECIAL_RXN + 1
               ISPECIAL( NSPECIAL_RXN,1 ) = NXX
               ISPECIAL( NSPECIAL_RXN,2 ) = NRATE_STRING
            ELSE
               KTYPE( NXX ) = 13
            END IF

            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,'(A,1X,A)')'CHR is ',CHR
               WRITE( *,2023 ) NXX, INBUF(LPOINT:IEOL)
               STOP
            END IF
            LSTART = LPOINT + 1
            RATE_STRING( NRATE_STRING ) = ''
            READ_RATE_STRING: DO 
!               IF( INBUF(IEOL:IEOL) .NE. ';' )THEN
!                  WRITE( *,'(A)')'Line must end with semi-colon and not pass column 81' 
!                  WRITE( *,2032 ) NXX, INBUF
!                 STOP
!               END IF

               CHR = INBUF(IEOL:IEOL)
               LSTOP = IEOL
               IF( CHR .EQ. ';' )LSTOP = LSTOP - 1
               
               NDX = LEN_TRIM( INBUF( LSTART:LSTOP ) )
     &             + LEN_TRIM( RATE_STRING( NRATE_STRING ) )
               IF( NDX .GT. 81 )THEN
                   WRITE( *,'(A)')'Rate String exceeds 81 characters' 
                   WRITE( *,2036 ) TRIM(INBUF)
                   STOP
               END IF

               RATE_STRING( NRATE_STRING )  = TRIM( RATE_STRING( NRATE_STRING ) ) 
     &                                      // TRIM( INBUF(LSTART:LSTOP) )

               PRINT*,TRIM(RATE_STRING( NRATE_STRING ))

               IF( LSTOP .NE. IEOL )EXIT
               LPOINT = LSTOP
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               LSTART = LPOINT
               
               RATE_STRING = ADJUSTL( RATE_STRING )
               
            END DO READ_RATE_STRING
            LPOINT = IEOL - 1 
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            GO TO 901
         END IF

         IF ( CHR .EQ. '1' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,2023 ) NXX, INBUF
               STOP
            ELSE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               KTYPE( NXX ) = 7
               RTDAT( 1,NXX ) = NUMBER
               IF ( CHR .NE. ';' ) THEN
                  WRITE( *,2025 ) NXX, INBUF
                  STOP
               ELSE
                  GO TO 901
               END IF
            END IF     ! CHR .NE. '#'
         ELSE IF ( CHR .EQ. '2' ) THEN    ! treated as falloff
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,2023 ) NXX, INBUF
               STOP
            ELSE
               NFALLOFF = NFALLOFF + 1
               KTYPE( NXX ) = 8
               IRRFALL( NFALLOFF ) = NXX
               NUMREALS = 0
201            CONTINUE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               NUMREALS = NUMREALS + 1
               IF ( NUMREALS .EQ. 2 .OR. 
     &              NUMREALS .EQ. 4 .OR. 
     &              NUMREALS .EQ. 6 )
     &            NUMBER = -1.0D0 * NUMBER
               IF ( NUMREALS .LE. 3 ) THEN
                  RTDAT( NUMREALS,NXX ) = NUMBER
               ELSE
                  RFDAT( NUMREALS-3,NFALLOFF ) = NUMBER
               END IF
               IF ( NUMREALS .EQ. 1 .OR. 
     &              NUMREALS .EQ. 3 .OR. 
     &              NUMREALS .EQ. 5 ) THEN
                  IF ( CHR .NE. '@' ) THEN
                     WRITE( *,2027 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 201
               ELSE IF ( NUMREALS .LE. 4 ) THEN
                  IF ( CHR .NE. '&' ) THEN
                     WRITE( *,2029 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 201
               ELSE IF ( NUMREALS .GE. 6 ) THEN
                  IF ( CHR .NE. ';' ) THEN
                     WRITE( *,2029 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 901
               END IF                                      
            END IF     ! CHR .NE. '#'

         ELSE IF ( CHR .EQ. '3' ) THEN    ! treated as falloff
            NOT_POWER  = .TRUE.
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,'(A,1X,A)')'CHR is ',CHR
               WRITE( *,2023 ) NXX, INBUF
               STOP
            ELSE
               NFALLOFF = NFALLOFF + 1
               KTYPE( NXX ) = 9
               IRRFALL( NFALLOFF ) = NXX
               NUMREALS = 0
301            CONTINUE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               IF( NOT_POWER )THEN
                   NUMREALS = NUMREALS + 1
                   IF ( MOD(NUMREALS, 2) .EQ. 0 .AND. NUMBER .NE. 0.0D+0 )NUMBER = -1.0D0 * NUMBER
                   IF ( NUMREALS .LE. 3 ) THEN
                       RTDAT( NUMREALS,NXX ) = NUMBER
                   ELSE
                       IF( NUMREALS .EQ. 4 )RFDAT( NUMREALS-3,NFALLOFF ) = NUMBER
                       IF( NUMREALS .GE. 5 )RFDAT( NUMREALS-1,NFALLOFF ) = NUMBER
                   END IF
                   IF ( NUMREALS .EQ. 1 .OR. NUMREALS .EQ. 3 ) THEN 
                       IF ( CHR .NE. '@' .AND. CHR. NE. '^' ) THEN
                          WRITE( *,2129 ) NXX, INBUF
                          STOP
                       ELSE IF(  CHR. EQ. '^' ) THEN
                          NOT_POWER = .FALSE.
                       END IF
                       GO TO 301
                   ELSE IF ( NUMREALS .EQ. 2 ) THEN
                      IF ( CHR .NE. '&' ) THEN
                          WRITE( *,2029 ) NXX, INBUF
                          STOP
                      END IF
                      GO TO 301
                   ELSE IF( NUMREALS .EQ. 4 )THEN
                      IF(CHR .EQ. ';')GO TO 901
                      IF(CHR .EQ. '&')GO TO 301
                      WRITE( *,2029 ) NXX, INBUF
                      STOP
                   ELSE IF( NUMREALS .EQ. 5 )THEN
                      IF( CHR .NE. '@' )THEN
                          WRITE( *,2029 ) NXX, INBUF
                          STOP
                      END IF
                      GO TO 301
                   ELSE IF ( NUMREALS .GE. 6 ) THEN
                      IF ( CHR .NE. ';' ) THEN
                         WRITE( *,2029 ) NXX, INBUF
                         STOP
                       END IF
                       GO TO 901
                   END IF
                ELSE
                  RFDAT( INT( NUMREALS/ 2) + 2, NFALLOFF ) = NUMBER
                  NOT_POWER = .TRUE.
                  IF ( CHR .NE. '@' ) THEN
                       WRITE( *,2027 ) NXX, INBUF
                       STOP
                  END IF
                  GO TO 301
                END IF
            END IF     ! CHR .NE. '#'

         ELSE IF ( CHR .EQ. 'H' .OR. CHR .EQ. 'h' )THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
! set to photolysis rate values because depends on sunlight
            IRXBITS( NXX ) = 0
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 1 )
            KTYPE( NXX )        = 12
            NFALLOFF            = NFALLOFF + 1
            IRRFALL( NFALLOFF ) = NXX
            HALOGEN_PARAMETER   = .TRUE.
            NUMREALS     = 1
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,'(A,1X,A)')'CHR is ',CHR
               WRITE( *,2023 ) NXX, INBUF(LPOINT:IEOL)
               STOP
            END IF
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            RTDAT( NUMREALS,NXX ) = NUMBER
            IF( CHR .EQ. '@' )THEN
                CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
                CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
                RFDAT( NUMREALS,NFALLOFF ) = -1.0D0 * NUMBER
            END IF
            IF( CHR .EQ. '&' )THEN
	       NUMREALS     = 2
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               RTDAT( NUMREALS,NXX ) = NUMBER
               IF( CHR .EQ. '@' )THEN
                   CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
                   CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
                   RFDAT( NUMREALS,NFALLOFF ) = -1.0D0 * NUMBER
               END IF
            END IF
            IF( CHR .EQ. '&' )THEN
               NUMREALS     = 3
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               RTDAT( NUMREALS,NXX ) = NUMBER
!           ELSE
!              NUMREALS     = 3
!              RTDAT( NUMREALS,NXX ) = 2.6000D-6
            END IF
!            write(*,'(A16,1X,5(ES12.4,1X),A16)')LABEL(NXX,1),RTDAT(1,NXX),RFDAT(1,NFALLOFF),
!    &      RTDAT(2,NXX),RFDAT(2,NFALLOFF),RTDAT(3,NXX),LABEL(NXX,2)
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2017 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901
            END IF

         ELSE
            WRITE( *,'(A,1X,A)')'CHR is ',CHR
            WRITE( *,2031 ) NXX, INBUF
            STOP

         END IF   ! CHAR .EQ. '1'

      ELSE

         WRITE( *,2033 ) NXX, INBUF
         STOP

      END IF      ! CHR .EQ. '#'   

901   CONTINUE

      IF ( NUMANDS .EQ. 0 ) THEN
         IF ( KTYPE( NXX ) .EQ. 1 ) THEN
            KTN1 = KTN1 + 1
            KRX1( KTN1 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 2 ) THEN
            KTN2 = KTN2 + 1
            KRX2( KTN2 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 3 ) THEN
            KTN3 = KTN3 + 1 
            KRX3( KTN3 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 4 ) THEN
            KTN4 = KTN4 + 1
            KRX4( KTN4 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 5 ) THEN
            KTN5 = KTN5 + 1
            KRX5( KTN5 ) = NXX
C           reverse equil. rx and 1st order: must undo forward rx conversion

         ELSE IF ( KTYPE( NXX ) .EQ. 6 ) THEN
            KTN6 = KTN6 + 1
            KRX6( KTN6 ) = NXX
!!          KCNV = KCNV + 1
!!          KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 7 ) THEN
            KTN7 = KTN7 + 1
            KRX7( KTN7 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         END IF
      END IF     ! NUMANDS .EQ. 0            
      RETURN


2001  FORMAT( / 5X, '*** ERROR: ',
     &              'Incorrect symbol following first #A'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: ',
     &              '@, &, or ; expected after the ^B'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: ',
     &              'E expected after * in reaction type A@C*En'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2007  FORMAT( / 5X, '*** ERROR: ',
     &              '< expected as start of label string'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2009  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after n in reaction type A@C*En'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2011  FORMAT( / 5X, '*** ERROR: ',
     &              '@, *, &, or ; expected after first &A'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2013  FORMAT( / 5X, '*** ERROR: ',
     &              '& or ; expected after second &F'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2014  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after third &n'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2015  FORMAT( / 5X, '*** ERROR: ',
     &              'A or R must follow / symbol'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2017  FORMAT( / 5X, '*** ERROR: ',
     &              '; must follow /Ln or /Rn'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2019  FORMAT( / 5X, '*** ERROR: ',
     &              'K expected after * for special reaction A*Kn'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2021  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after special reaction a*Kn'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2023  FORMAT( / 5X, '*** ERROR: ',
     &              '# must follow 1 or 2 or 3 or 4 or H in % reactions'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2043  FORMAT( / 5X, '*** ERROR: ',
     &              '/ must follow exponent in %H reactions'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2025  FORMAT( / 5X, '*** ERROR: ',
     &              '; must follow %1 A'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2027  FORMAT( / 5X, '*** ERROR: ',
     &              '@ must follow #A in %2 or %3 reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2029  FORMAT( / 5X, '*** ERROR: ',
     &              '& or ; must follow @C in %2 or %3 reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2129  FORMAT( / 5X, '*** ERROR: ',
     &              '@ or ^ must follow #A in %3 reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2031  FORMAT( / 5X, '*** ERROR: ',
     &              '1 or 2 or 3 or 4 or H must follow % in rate expressions'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2032  FORMAT( / 5X, '*** ERROR: ',
     &              'incorrect symbol at end of reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2033  FORMAT( / 5X, '*** ERROR: ',
     &              'Invalid character to start rate constant'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2034  FORMAT( / 5X, '*** ERROR: ',
     &              '* expected as alternate heteorogeneous dependency reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2035  FORMAT( / 5X, '*** ERROR: ',
     &              '* expected as alternate photolysis dependency reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2036  FORMAT( / 5X, '*** ERROR: ',
     &        / 5X, 'Last line read was:' / A )
2041  FORMAT( / 5X, '*** ERROR: ',
     &              '; Special rate coefficient not found in Special Table'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2042  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after special reaction rate coefficient a?R'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
      END SUBROUTINE GETRATE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETRCTNT ( IMECH, INBUF, IEOL, LPOINT, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, 
     &                      LABEL, N_DROP_SPC, DROP_SPC )
 

C=======================================================================
C reads the reactants of each reaction
C
C input: NXX, NS, ICOL, IORDER
C needs: IMECH, INBUF, IEOL, LPOINT, CHR, WORD for GETWORD
C        SPCLIS for LKUPSPEC
C output: ICOL, IORDER, IRXBITS, IRR,
C         NWM
C         NRXWM
C         NWW
C         NRXWW
C         NWO2
C         NRXWO2
C         NWN2
C         NRXWN2
C         NWCH4
C         NRXWCH4
C         NWH2
C         NRXWH2
C
C (possibly updates: NS, INBUF, LPOINT, CHR, WORD)
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE

c...arguments

      INTEGER,         INTENT(   IN  ) :: IMECH
      CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( INOUT ) :: LPOINT
      INTEGER,         INTENT( INOUT ) :: IEOL
      CHARACTER(  1 ), INTENT( INOUT ) :: CHR
      CHARACTER( 16 ), INTENT( INOUT ) :: WORD
      INTEGER,         INTENT(   IN  ) :: NXX
      INTEGER,         INTENT( INOUT ) :: NS
      CHARACTER( 16 ), INTENT( INOUT ) :: SPCLIS( : )
      INTEGER,         INTENT( INOUT ) :: SPC1RX( : )
      INTEGER,         INTENT( INOUT ) :: ICOL

      CHARACTER( 16 ), INTENT(   IN  ) :: LABEL( :, : )

      INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
      CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( : )


c..local
      CHARACTER( 16 ) :: SPECIES
      INTEGER         :: NSPEC


c..STEADY_STATE related variables
      INTEGER SS_INDEX
      INTEGER, SAVE   :: REACTION_IDX = 0
      INTEGER, SAVE   :: FIXED_SPC_COUNT

      INTEGER, EXTERNAL :: INDEX1

      IF( REACTION_IDX .NE. NXX )THEN
          REACTION_IDX  = NXX
          FIXED_SPC_COUNT = 1
      ELSE 
          FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
          IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
             WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction#', NXX
             STOP
          END IF
      END IF

      INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 0 
      
      IF ( CHR .NE. '=' ) THEN
         CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
         SPECIES = WORD
         IORDER( NXX ) = IORDER( NXX ) + 1
         SS_INDEX = INDEX1( SPECIES, N_SS_SPC, SS_SPC )
         IF ( SPECIES( 1:4 ) .EQ. 'M   ' .OR.
     &        SPECIES( 1:4 ) .EQ. 'm   ' ) THEN
            NWM = NWM + 1
            NRXWM( NWM) = NXX
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 2 )
            INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 1
            NRXN_FIXED_SPECIES( IND_M )  = NRXN_FIXED_SPECIES( IND_M ) + 1
         ELSE IF ( SPECIES( 1:4 ) .EQ. 'H2O ' .OR.
     &             SPECIES( 1:4 ) .EQ. 'h2o ') THEN
            NWW = NWW + 1 
            NRXWW( NWW ) = NXX
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 3 )
            INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 2
            NRXN_FIXED_SPECIES( IND_H2O )  = NRXN_FIXED_SPECIES( IND_H2O ) + 1
         ELSE IF ( SPECIES( 1:4 ) .EQ. 'O2  ' .OR.
     &             SPECIES( 1:4 ) .EQ. 'o2  ') THEN
            NWO2 = NWO2 + 1
            NRXWO2( NWO2 ) = NXX
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 4 )
            INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 3
            NRXN_FIXED_SPECIES( IND_O2 )  = NRXN_FIXED_SPECIES( IND_O2 ) + 1
         ELSE IF ( SPECIES( 1:4 ) .EQ. 'N2  ' .OR.
     &             SPECIES( 1:4 ) .EQ. 'n2  ') THEN
            NWN2 = NWN2 + 1
            NRXWN2( NWN2 ) = NXX
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 5 )
            INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 4
            NRXN_FIXED_SPECIES( IND_N2 )  = NRXN_FIXED_SPECIES( IND_N2 ) + 1
         ELSE IF ( SPECIES( 1:4 ) .EQ. 'CH4 ' .OR.
     &             SPECIES( 1:4 ) .EQ. 'ch4 ') THEN
            NWCH4 = NWCH4 + 1
            NRXWCH4( NWCH4 ) = NXX
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 6 )
            INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 5
            NRXN_FIXED_SPECIES( IND_CH4 )  = NRXN_FIXED_SPECIES( IND_CH4 ) + 1
         ELSE IF ( SPECIES( 1:4 ) .EQ. 'H2  ' .OR.
     &             SPECIES( 1:4 ) .EQ. 'h2  ') THEN
            NWH2 = NWH2 + 1
            NRXWH2( NWH2 ) = NXX
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 7 )
            INDEX_FIXED_SPECIES( NXX, FIXED_SPC_COUNT ) = 6
            NRXN_FIXED_SPECIES( IND_H2 )  = NRXN_FIXED_SPECIES( IND_H2 ) + 1
c..skip species that are on the ELIMINATE list
         ELSE IF ( INDEX1( SPECIES, N_DROP_SPC, DROP_SPC ) .NE. 0 ) THEN
            IORDER( NXX ) = IORDER( NXX ) - 1
            WRITE( *, 2021 ) SPECIES, LABEL( NXX, 1 )
c..skip steady-state species, but sum coefficients for each reaction
         ELSE IF( SS_INDEX .NE. 0 ) THEN
            SS_RCT_COEF( SS_INDEX, NXX ) = SS_RCT_COEF( SS_INDEX, NXX ) + 1
         ELSE
            ICOL = ICOL + 1
            CALL LKUPSPEC ( NS, SPECIES, SPCLIS, NXX, SPC1RX, NSPEC )
            IRR( NXX,ICOL ) = NSPEC
            IF( ICOL .LT. 2 )THEN
               WRITE(KPPEQN_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPECIES),' '
            ELSE
               WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPECIES),' '
            END IF
         END IF
         IF ( IORDER( NXX ) .GT. 3 ) THEN
            WRITE( *,2001 ) INBUF
            STOP
         END IF
      ELSE
         IORDER( NXX ) = 0
      END IF      ! CHR .NE. '='

!      PRINT*,CHR

      RETURN
2001  FORMAT( / 5X, '*** ERROR: Too many reactants read in -- max=3'
     &        / 5X, 'Last line read was:' / A81 )

2021  FORMAT( / 5X, '*** WARNING: A reactant is being eliminated & the reaction order', 
     &              'is being reduced!' /
     &          10X, 'Species Eliminated: ', A, 1X, ' Reaction Label: <', A, '>' )
      END SUBROUTINE GETRCTNT
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )

C=======================================================================
C converts a number from character to floating point representation
C
C input: IMECH (for RDLINE), INBUF, LPOINT, IEOL, CHR
C output: NUMBER
C updates: INBUF, LPOINT, IEOL, CHR
C=======================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN )    :: IMECH   ! IO unit for mechanism file
      CHARACTER*( * ), INTENT( INOUT ) :: CHR     ! current character from buffer
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF   ! string read from mechanism file
      INTEGER,         INTENT( INOUT ) :: LPOINT  ! character position in INBUF
      INTEGER,         INTENT( INOUT ) :: IEOL    ! end of line position
      REAL( 8 ),       INTENT( OUT )   :: NUMBER  ! number from file
!Local:
      LOGICAL         :: LDECIMAL, LEXP, LZERO
      CHARACTER( 17 ) :: NUMSTRING 
      INTEGER         :: START, LENGTH, NUMSIGNS
      REAL            :: LOCAL_NUMBER


      START = LPOINT
      LENGTH = 0
      NUMSIGNS = 0
      LDECIMAL = .FALSE.
      LEXP = .FALSE.
      LZERO = .TRUE.
101   CONTINUE
      IF ( LENGTH .NE. 0 ) THEN
         LPOINT = LPOINT + 1
         IF ( LPOINT .GT. IEOL ) THEN
            CHR = ' '
         ELSE
            CHR = INBUF( LPOINT:LPOINT )
         END IF
      END IF
      LENGTH = LENGTH + 1
      IF ( CHR .EQ. '.' ) THEN
         IF ( .NOT. LDECIMAL ) THEN
            LDECIMAL = .TRUE.
            GO TO 101
         ELSE
            WRITE( *,2001 ) INBUF
            STOP
         END IF        
      END IF   
      IF ( CHR .NE. '0' )LZERO = .FALSE.
      IF ( CHR .GE. '0' .AND. CHR .LE. '9' ) GO TO 101
      IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' .OR.
     &     CHR .EQ. 'D' .OR. CHR .EQ. 'd' )THEN
         IF ( .NOT. LEXP ) THEN
            LEXP = .TRUE.
            GO TO 101
         ELSE
            WRITE( *,2003 ) INBUF
            STOP
         END IF
      END IF      
      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
         NUMSIGNS = NUMSIGNS + 1
         IF ( NUMSIGNS .LE. 2 ) THEN
            GO TO 101
         ELSE
            WRITE( *,2005 ) INBUF
            STOP
         END IF
      END IF

c end of the numeric string

      NUMSTRING = ' '
      NUMSTRING = INBUF( START:LPOINT-1 )
      LENGTH = LENGTH - 1
      IF ( ( .NOT. LEXP ) .AND. ( .NOT. LDECIMAL ) ) THEN  ! force it to be real
         NUMSTRING = NUMSTRING( 1:LENGTH ) // '.'
         LENGTH = LENGTH + 1
      END IF
      NUMSTRING = NUMSTRING( 1:LENGTH ) // 'D0'
       READ ( NUMSTRING( 1:LENGTH ), *, ERR=1999 ) NUMBER
!      READ( NUMSTRING( 1:LENGTH ), * )LOCAL_NUMBER
      IF( LZERO )THEN
         NUMBER = 0.0D+0
      END IF
      IF ( LPOINT .GT. IEOL ) THEN
           CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
           WRITE(KPPEQN_UNIT,'(A)')' '
      END IF
      IF ( CHR .EQ. ' ' ) CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )

      RETURN
1999  WRITE(*,'("Error reading real number in the line: " / A)')TRIM( INBUF )
      STOP

C=======================================================================
2001  FORMAT( / 5X, '*** ERROR: ',
     &              'Only one decimal point allowed in a number'
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: ',
     &              'More than one E or e in the field'
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: ',
     &              'More than one sign in the exponent field:'
     &        / 5X, 'Last line read was:' / A81 )
        END SUBROUTINE GETREAL
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )

C==============================================================================
C Reads a word; only the first MAXWRDLEN characters are significant.
C The word may consist only of alphanumeric characters, ':' and '_', and the
C first character must be alphabetic. Since there is no defined terminator
C for a word, the word cannot span two lines, but may have embedded comments.
C The word buffer is terminated by any NON-valid character not on a new line.
C input: IMECH (for RDLINE), INBUF, LPOINT, CHR
C output: WORD
C updates: INBUF, LPOINT, IEOL, CHR
C precondition:  RDLINE must have been called
C calls: VALWRDCHR
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

      CHARACTER*( * ), INTENT( INOUT ) :: CHR
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( IN )    :: IMECH
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
      CHARACTER*( * ), INTENT(  OUT  ) :: WORD
!local:      
      INTEGER         :: START, LENWRD
      CHARACTER(256 ) :: WRDBUF

      START  = LEN( INBUF )
      WRDBUF = ' '
      WRDBUF(1:START) = INBUF(1:START)

      LENWRD = 0
      START = LPOINT
C check for valid starting character
      IF ( ( CHR .LT. 'A' .OR. CHR .GT. 'Z' ) .AND. 
     &     ( CHR .LT. 'a' .OR. CHR .GT. 'z' ) ) THEN
         WRITE( *,2001 ) INBUF, CHR
         STOP
      END IF
101   CONTINUE
C Return word if chr is word separator
      LENWRD = LENWRD + 1
      LPOINT = LPOINT + 1
      IF ( LPOINT .GT. IEOL ) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         WRITE(KPPEQN_UNIT,'(A)')' '
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
C check for valid character in new line NOT part of this word
         IF ( VALWRDCHR( CHR ) ) THEN
            WRITE( *,2003 ) WRDBUF
            STOP
         ELSE
            GO TO 201    ! finished filling word buffer
         END IF
      END IF
C keep going to IEOL, ';', ',', or ' ' as word terminator or invalid
C character (as kludge terminator!)
      CHR = WRDBUF( LPOINT:LPOINT )
      IF ( VALWRDCHR( CHR ) ) THEN
         GO TO 101
      ELSE
         IF( CHR .EQ. '=' .OR. CHR .EQ. '+' .OR. CHR .EQ. '-' )THEN
	   IF( LENWRD .GT. 0 )RETURN
           WRITE( *,2003 ) WRDBUF
           STOP
         END IF
         IF ( CHR .NE. ' ' .AND.
     &        CHR .NE. ',' .AND.
     &        CHR .NE. '[' .AND.
     &        CHR .NE. ']' .AND.
     &        CHR .NE. ';' ) THEN
            WRITE( *,2005 ) WRDBUF, CHR
            STOP ' *** Invalid character in GETWORD ***'
         END IF
      END IF
      IF ( CHR .EQ. '(' .OR. CHR .EQ. '{' ) THEN 
         LENWRD = LENWRD - 1
         CALL EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )
         GO TO 101
      END IF
201   CONTINUE
      IF ( LENWRD .GT. MAXWRDLEN ) LENWRD = MAXWRDLEN
      WORD = WRDBUF( START:START+LENWRD-1 )
      IF ( CHR .EQ. ' ' ) CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      RETURN
2001  FORMAT( / 5X, '*** GETWORD ERROR: First character of a word expected',
     &              ' to be alphabetic'
     &        /     'Line buffer contents:' / A81
     &        /     'Character:', 2X, A1 )
2003  FORMAT( / 5X, '*** GETWORD ERROR: Word cannot span two lines:'
     &        /     'First line buffer contents:'/ A81 )
!005  FORMAT( / 5X, '*** GETWORD ERROR: Invalid character in a word'
2005  FORMAT( / 5X, '!!! GETWORD WARNING: Invalid character in a word'
     &        /     'Line buffer contents:' / A81
     &        /     'Character:', 2X, A1 / )
      END SUBROUTINE GETWORD
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )

C==============================================================================
C RDLINE reads one line from the mechanism file and stores it in INBUF.
C The position of the last non-blank character is stored in IEOL.
C All blank lines and lines with a '!' in the first column are skipped.
C LPOINT, the pointer to the current character being accessed (set by
C GETCHAR), is initialized to 0.
 
C input:  IMECH
C output: INBUF, LPOINT, IEOL
C updates: nothing
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE
!arguments:
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( IN )    :: IMECH
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
!local:
      INTEGER       ::IPOS
      LOGICAL, SAVE :: FIRSTCALL  = .TRUE.
      INTEGER, SAVE :: LINE_COUNT = 0


101   CONTINUE
      READ( IMECH, '(A)', END = 999 ) INBUF

      IF ( INBUF( 1:1 ) .NE. '!' ) THEN
C Find the last non blank character in the line and set IEOL
      LINE_COUNT = LINE_COUNT + 1
C     WRITE(6,'(A16,1X,I4,1X,A2)',ADVANCE='NO')'LINE:', LINE_COUNT,' '
C     WRITE(6,'(A)')INBUF

         IEOL = 0
         DO IPOS = 81, 1, -1
            IF ( INBUF( IPOS:IPOS ) .NE. ' ' ) THEN
               IEOL = IPOS
               GO TO 301
            END IF
         END DO
301      CONTINUE
C End of finding last non-blank character 

         IF ( IEOL .EQ. 0 ) THEN ! skip a blank line
            GO TO 101
         ELSE
            LPOINT = 0
            RETURN
         END IF
      ELSE
         GO TO 101
      END IF

999   CONTINUE
C eof encountered
      WRITE( *,2001 ) IMECH, INBUF
!001  FORMAT( / 5X, '*** RDLINE ERROR: End of file read on unit:', I3
2001  FORMAT( / 5X, 'From RDLINE: End of file read on unit:', I3
     &        / 5X, 'Last line read:' / A81 )
!     STOP
      IEOL = -999
      END_OF_IMECH = .TRUE.
      RETURN
      END SUBROUTINE RDLINE
      LOGICAL FUNCTION VALLABCHR ( CHR )
C does CHR belong to a list of valid characters for label?

      IMPLICIT NONE
      CHARACTER( 1 ), INTENT( IN ) :: CHR

      VALLABCHR = .TRUE.
      IF ( CHR .EQ. '<' .OR.
     &     CHR .EQ. '>' .OR.
     &     CHR .EQ. '{' .OR.
     &     CHR .EQ. '}' .OR.
     &     CHR .EQ. '(' .OR.
     &     CHR .EQ. ')' ) THEN
         VALLABCHR = .FALSE.
      END IF
      RETURN
      END FUNCTION VALLABCHR 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LOGICAL FUNCTION VALWRDCHR ( CHR )
C does CHR belong to a list of valid characters for word?
      IMPLICIT NONE
      CHARACTER( 1 ), INTENT( IN ) :: CHR

      VALWRDCHR = .TRUE.
      IF ( CHR .GE. '0' .AND. CHR .LE. ':' ) RETURN    ! 48 - 58
      IF ( CHR .GE. 'A' .AND. CHR .LE. 'Z' ) RETURN    ! 65 - 90
      IF ( CHR .EQ. '_' )                    RETURN    ! 95
!     IF ( CHR .EQ. ''' )                    RETURN    ! 96
      IF ( CHR .GE. 'a' .AND. CHR .LE. 'z' ) RETURN    ! 97 - 122
      VALWRDCHR = .FALSE.
      RETURN
      END FUNCTION VALWRDCHR
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )

C==============================================================================
C Reads past all characters in comments - matches 1st and 2nd delimiters,
C and ignores anything in bewteen.
C     Called by: GETCHAR, GETWORD
C     Subroutines called: RDLINE
C input: IMECH (for RDLINE), LPOINT, IEOL
C output: nothing
C updates: INBUF, LPOINT, IEOL, CHR
C precondition: on entry CHR must be either '(' or '{'
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

       CHARACTER*( * ), INTENT( INOUT ) :: CHR
       CHARACTER*( * ), INTENT( INOUT ) :: INBUF
       INTEGER,         INTENT( IN )    :: IMECH
       INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT


       CHARACTER( 1 ) :: DELIM2

      IF ( CHR .EQ. '(' ) THEN
         DELIM2 = ')'
      ELSE IF ( CHR .EQ. '{' ) THEN
         DELIM2 = '}'
      ELSE
         WRITE( *,2001 ) CHR, INBUF
         STOP
      END IF
101   CONTINUE
      LPOINT = LPOINT + 1
      IF ( LPOINT .GT. IEOL) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         GO TO 101
      ELSE
         CHR = INBUF( LPOINT:LPOINT )
         IF ( CHR .NE. DELIM2 ) GO TO 101
      END IF
      RETURN
2001  FORMAT( / 5X, '*** ERROR processing comment: ',
     &              'Incorrect second delimiter, ', A,
     &          1X, 'following first delimiter'
     &        / 5X, 'Last line read was:' / A81 )
      END SUBROUTINE EATCOM
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE LKUPSPEC ( NS, SPECIES, SPCLIS, NXX, SPC1RX, NSPEC )

C=======================================================================
C Finds the index NSPEC of a species SPECIES in list SPCLIS if it exists.
C Otherwise, adds the species to SPCLIS and udates SPCLIS length NS.
 
C Also determines the index of the reaction that finds SPECIES first -
C in SPC1RX
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE
! Arguments:
      INTEGER,         INTENT(INOUT) :: NS
      INTEGER,         INTENT( OUT ) :: NSPEC
      INTEGER,         INTENT(INOUT) :: SPC1RX( : )
      INTEGER,         INTENT(  IN ) :: NXX
      CHARACTER*( * ), INTENT(  IN ) :: SPECIES
      CHARACTER*( * ), INTENT(INOUT) :: SPCLIS( : )
! Local:
      INTEGER :: ISPC

      DO ISPC = 1, NS
         IF ( SPECIES .EQ. SPCLIS( ISPC ) ) THEN   ! found
            NSPEC = ISPC
            RETURN
         END IF
101   END DO
      NS = NS + 1                           ! not found
      IF ( NS .GT. MAXSPEC ) THEN
         WRITE( 6,2001 ) MAXSPEC
         STOP
      END IF         
      SPCLIS( NS ) = SPECIES
      NSPEC = NS
      SPC1RX( NS ) = NS
      RETURN
2001  FORMAT( / 5X, '*** ERROR: ',
     &        'Maximum number of species = ', I3, ' exceeded' )
      END SUBROUTINE LKUPSPEC
      SUBROUTINE CHECK_ORDER_SPECIAL(  )

!**********************************************************************
!  Function: Determine the order of the special rate operators so each operator
!            terms has consistent units between its terms
!**********************************************************************

      USE MECHANISM_DATA

      IMPLICIT NONE

!..Includes:


!..Arguments: NONE


!..Parameters: None

!..External Functions: None

!..Scratch Local Variables:

      INTEGER ISP            ! Loop index for special rates
      INTEGER ISP1,ISP2,ISP3 ! Pointers to species numbers
      INTEGER ISP4,ISP5,ISP6 ! Pointers to species numbers
      INTEGER NCELL          ! Loop index for number of cells
      INTEGER IKC_TERM       ! Loop index for special of KC terms
      INTEGER I_OPERATOR     ! Loop index for number of preceeding special rates
      INTEGER NRK            ! Reaction number
      INTEGER NRX            ! Loop index for number of reactions

      INTEGER COUNT_TERMS    ! count number of term used by each expressions 

      INTEGER ORDER_TERM( 2*MAXSPECTERMS )
      LOGICAL IS_FALLOFF
      LOGICAL ERROR_FLAG

!..Saved Local Variables: None

      LOGICAL, SAVE :: FIRSTCALL  = .TRUE.
      
      IF( .NOT. FIRSTCALL )RETURN
      
      IF ( NSPECIAL .LT. 1 )THEN
         WRITE( 6, 90001)(MSPECTERMS-1)
         RETURN
      ENDIF

      ORDER_TERM     = -999
      ERROR_FLAG     = .FALSE.
      
      ALLOCATE( ORDER_SPECIAL( NSPECIAL ) )
      ORDER_SPECIAL  = 0
! reset       
      DO 220 ISP = 1, NSPECIAL

!  Start with rate constant times concentration terms
         COUNT_TERMS = 0
         LOOP_KC: DO IKC_TERM = 1, MAXSPECTERMS
            NRK = INDEX_KTERM( ISP, IKC_TERM )

            IF ( NRK .LT. 0 )THEN ! empty array entry
               COUNT_TERMS = 1 + COUNT_TERMS
!               CYCLE LOOP_KC  
	    ELSE IF( NRK .GT. 0 )THEN ! existing rate constant
               COUNT_TERMS = 1 + COUNT_TERMS
               ORDER_TERM( IKC_TERM ) = IORDER( NRK )
! correct if rate constant is a falloff type
               IS_FALLOFF = ( KTYPE( NRK ) .GT. 7 .AND. KTYPE( NRK ) .LT. 11 )
               IF( KUNITS .NE. 2 .AND. IS_FALLOFF )THEN
	           ORDER_TERM( IKC_TERM ) = ORDER_TERM( IKC_TERM ) + 1
	       END IF
               ISP2 = INDEX_CTERM( ISP, IKC_TERM )
               IF ( ISP2 .LT. 1 ) CYCLE LOOP_KC  ! empty array entery
	       ORDER_TERM( IKC_TERM ) = ORDER_TERM( IKC_TERM ) - 1
            ELSE ! NRK = 0, KC term is a pure concentration
               ISP2 = INDEX_CTERM( ISP, IKC_TERM )
               IF ( ISP2 .LT. 1 ) CYCLE LOOP_KC  ! empty array entery
               COUNT_TERMS = 1 + COUNT_TERMS
               ORDER_TERM( IKC_TERM ) = 0
            END IF
         END DO LOOP_KC
	 

!  set order for terms using existing operators

         LOOP_OP: DO I_OPERATOR = 1, MAXSPECTERMS
            ISP1 = OPERATORS( ISP, I_OPERATOR )
            IF ( ISP1 .LT. 1 ) CYCLE LOOP_OP
	    ORDER_TERM( I_OPERATOR + MAXSPECTERMS ) = ORDER_SPECIAL( ISP1 )	    
            COUNT_TERMS = 1 + COUNT_TERMS
         END DO LOOP_OP
	 
	 ISP3 = 1
	 DO ISP1 = 2, 2*MAXSPECTERMS
            IF( ORDER_TERM( ISP1 ) .GE. -999 )CYCLE
	    ISP3 = ISP3 + 1
	    IF( ORDER_TERM( ISP1 ) .NE. ORDER_TERM( 1 ) )THEN
	       WRITE( 6, * )'ERROR: ' // TRIM( SPECIAL( ISP ) ) // ' uses inconsistent rate orders '
	       WRITE( 6, * )'ORDER FIRST TERM = ', ORDER_TERM( 1 ),
     &                     ' ORDER ', ISP3,'th TERM = ', ORDER_TERM( ISP1 )
	       ERROR_FLAG = .TRUE.
	    END IF
	 END DO
	 
	 ORDER_SPECIAL( ISP ) = ORDER_TERM( 1 )
	 
	 WRITE(6,90000 )SPECIAL( ISP ), ORDER_SPECIAL( ISP ), COUNT_TERMS

	 MSPECTERMS = MAX( MSPECTERMS, COUNT_TERMS )
220   CONTINUE

      WRITE( 6, 90001)MSPECTERMS
      IF( ERROR_FLAG )THEN
          WRITE( 6, * )'FATAL ERROR detected in routine CHECK_ORDER_SPECIAL'
	  WRITE( 6, * )'Consult the above information'
	  STOP
      END IF

      FIRSTCALL   = .FALSE.
90000 FORMAT('ORDER SPECIAL OPERATOR, ',A16,':',I2,' Number Terms in Operator: ',I4 )
90001 FORMAT('Maximum Number Terms used in the Operators: ',I4 )
      RETURN
      END SUBROUTINE CHECK_ORDER_SPECIAL
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CHECK_SS_SPC ( LUNOUT, NS, SPCLIS, NR, LABEL, SS1RX )

C=======================================================================
C Checks to make sure all species selected to be in steady state do not
C violate special steady-state species rules. It also reorders the
C steady-state species if necessary to insure any species dependencies
C are properly maintained.  A species must be meet the following criteria
C to be allowed:
C   1) The defined steady-state species name must be found in the mechanism
C   2) A steady-state species may not react with another steady-state species
C      (including reacting with itself)
C   3) A steady-state species cannot be produced from another steady-state
C      species if it also reacts to produce that same species (i.e.. 
C      inner dependencies are not allowed)
C=======================================================================


      USE MECHANISM_DATA
      
      IMPLICIT NONE

C..Input Arguments
      INTEGER,         INTENT ( IN )    :: LUNOUT               ! Output unit number
      INTEGER,         INTENT ( IN )    ::  NS                  ! No. of species in mechanism
      CHARACTER( 16 ), INTENT ( IN )    ::  SPCLIS( : )   ! List of mechanism species
      INTEGER,         INTENT ( IN )    ::  NR                  ! No. of reactions
      CHARACTER( 16 ), INTENT ( IN )    ::  LABEL( :,: ) ! Reaction labels
      INTEGER,        INTENT ( INOUT )  ::  SS1RX( : )



C..Functions
      INTEGER INDEX1

C..Local variables

      CHARACTER( 16 ), ALLOCATABLE  :: SS_SPC_TEMP( : )  ! Temp SS species name array

      INTEGER  IND                              ! Species index
      INTEGER  IPRD                             ! Product loop index
      INTEGER  IRCT                             ! Reactant loop index
      INTEGER  SS1, SS2, SS3                    ! Loop indices for steady-state species
      INTEGER  NRX                              ! Loop index for reactions
      INTEGER  N_SS_RCTNT                       ! Counter
      INTEGER  NUM_RSLVD                        ! Number of SS species with dependencies reolved
      INTEGER  SAVD_NUM_RSLVD                   ! Saved value of above

      INTEGER  ::   SUM_RCT_COEF                ! Sum of all SS species reactant coefficients
      INTEGER  ::   SUM_PRD_COEF                ! Sum of all SS species product coefficients


      INTEGER, ALLOCATABLE  ::  SS_SPC_IND( : )     ! SS speciecies index array
      INTEGER, ALLOCATABLE  ::  NUM_DEPEND( : )     ! No. of depencies for each species
      INTEGER, ALLOCATABLE  ::  RSLVD_IND( : )      ! Resolved index for species 

      INTEGER, ALLOCATABLE  ::  SS_DEPEND( : , : )  ! SS dependency matrix

      LOGICAL   ::    LERROR           ! Error found flag
      LOGICAL   ::    LFOUND           ! Found flag
      LOGICAL   ::    L_REORDER        ! Flag for re-orderining SS species
      LOGICAL   ::    LDEPOK           ! Flag for dependencies identified

      LOGICAL, ALLOCATABLE  ::    LRSLVD( : )       ! Flag to indicate species dependencies resolved
      LOGICAL, ALLOCATABLE  ::    LREACT( : )       ! Flag to indicate species dependencies resolved
      LOGICAL, ALLOCATABLE  ::    LPROD( : )        ! Flag to indicate species dependencies resolved

      INTEGER, ALLOCATABLE  ::    TEMP_RCT_COEF( : , : )  ! Temp array holding reactant coefficients
      REAL, ALLOCATABLE     ::    TEMP_PRD_COEF( : , : )  ! Temp array holding product coefficients


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Allocate arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( SS_SPC_IND( N_SS_SPC ) )
      ALLOCATE( NUM_DEPEND( N_SS_SPC ) )
      ALLOCATE( LRSLVD( N_SS_SPC ) )
      ALLOCATE( LREACT( N_SS_SPC ) )
      ALLOCATE( LPROD( N_SS_SPC ) )
      ALLOCATE( SS_SPC_TEMP( N_SS_SPC ) )
      ALLOCATE( RSLVD_IND( N_SS_SPC ) )

      ALLOCATE( SS_DEPEND( N_SS_SPC, N_SS_SPC ) )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure all species defined as being in steady-state are in the
c  mechanism; if any are not found, list them and stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE. 

      DO SS1 = 1 , N_SS_SPC

         SUM_PRD_COEF = 0.0
         SUM_RCT_COEF = 0
         DO NRX = 1, NR
            SUM_PRD_COEF = SUM_PRD_COEF + SS_PRD_COEF( SS1, NRX )
            SUM_RCT_COEF = SUM_RCT_COEF + SS_RCT_COEF( SS1, NRX )
         ENDDO

 
         IF( SUM_RCT_COEF .LE. 0 .AND. SUM_PRD_COEF .LE. 0.0 ) THEN
            IF( .NOT. LERROR ) THEN
               LERROR = .TRUE.
               WRITE( LUNOUT, 9000 ) 
            ENDIF
            WRITE( LUNOUT, 9500 ) SS1, SS_SPC( SS1 ) 
         ENDIF
      
      ENDDO  

      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure that no steady state species reacts with any other steady-state
c  species (including itself). If any cases are found, list them and stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE.

      DO NRX = 1, NR

         SUM_RCT_COEF = 0

         DO SS1 = 1, N_SS_SPC
            SUM_RCT_COEF = SUM_RCT_COEF + SS_RCT_COEF( SS1, NRX )
         ENDDO
                
         IF( SUM_RCT_COEF .GT. 1 ) THEN

            IF( .NOT. LERROR ) THEN
               LERROR = .TRUE.
               WRITE( LUNOUT, 9020 ) 
            ENDIF
            WRITE( LUNOUT, 9520 ) NRX, LABEL( NRX, 1 ) 

         ENDIF

      ENDDO

      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure that no steady state species appears as both a reactant and
c  a product in the same reaction. If any cases are found, list them and stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE.

      DO NRX = 1, NR

         DO SS1 = 1, N_SS_SPC
                
            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0 .AND. 
     &          SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN

               IF( .NOT. LERROR ) THEN
                  LERROR = .TRUE.
                  WRITE( LUNOUT, 9040 ) 
               ENDIF
               WRITE( LUNOUT, 9520 ) NRX, LABEL( NRX, 1 ) 

             ENDIF

          ENDDO

      ENDDO

      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF      


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure that each steady-state species appears as both a reactant 
c  and a product in separate reactions
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LREACT = .FALSE.   ! Array
      LPROD  = .FALSE.   ! Array
 
      DO NRX = 1, NR

         DO SS1 = 1, N_SS_SPC   

c..Flag the species if it is a reactant or a product in this reaction
            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0   ) LREACT( SS1 ) = .TRUE.
            IF( SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) LPROD( SS1 ) = .TRUE. 

         ENDDO

      ENDDO

      LERROR = .FALSE.
      DO SS1 = 1, N_SS_SPC

         IF( .NOT. LREACT( SS1 ) .OR.  .NOT. LPROD( SS1 ) ) THEN
            IF( .NOT. LERROR ) THEN
               LERROR = .TRUE.
               WRITE( LUNOUT, 9060 ) 
            ENDIF
            WRITE( LUNOUT, 9500 ) SS1, SS_SPC( SS1 )

         ENDIF

      ENDDO

      
      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF      
   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Steady-state species dependency analysis Algorithm
c
c   1. Build a dependency matrix -- A value of 1 in the matrix indicates that 
c      the steady species corresponding to the row is produced by the 
c      steady-state species in the corresponding column
c
c   2. Sum up the number of dependencies for each species
c
c   3. Put the species with zero dependencies at the top of the list
c
c   4. Next, check the remaining species, adding them when all the all species
c      they depend upon are in the list. Stop when all have been added or 
c      no more can be added ( the latter indicates an interdependency error
c
c   5. Reorder the species and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..Create Dependency matrix
      SS_DEPEND = 0      ! Array

      DO SS1 = 1, N_SS_SPC

         DO NRX = 1, NR

            IF( SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN            ! SS1 species is a product

               DO SS2 = 1, N_SS_SPC
                  IF( SS_RCT_COEF( SS2, NRX ) .GT. 0 ) THEN        ! SS2 species is a reactant
                     SS_DEPEND( SS1, SS2 ) = 1
                  ENDIF
               ENDDO

            ENDIF

         ENDDO           ! loop over reactions

      ENDDO              ! loop over SS1

           

c..Sum the number of dependencies for each steady-state species

      DO SS1 = 1, N_SS_SPC
         NUM_DEPEND( SS1 ) = 0
         DO SS2 = 1, N_SS_SPC
            IF( SS_DEPEND( SS1, SS2 ) .EQ. 1 ) NUM_DEPEND( SS1 ) = NUM_DEPEND( SS1 ) + 1
         ENDDO
      ENDDO

      


c..Order the species so that any steady-state species that is dependent upon another
c..comes after that species in the list of steady-state species

c..First, check for no dependencies (if none, no ee-ordering is necessary)

      L_REORDER = .FALSE.
      DO SS1 = 1, N_SS_SPC
         IF( NUM_DEPEND( SS1 ) .GT. 0 ) L_REORDER = .TRUE.
      ENDDO

      IF( .NOT. L_REORDER ) GO TO 1000

c..Put all steady-state species with zero dependencies at the top of the list
      NUM_RSLVD = 0
      LRSLVD= .FALSE.    ! Array
      RSLVD_IND = 0      ! Array
      
      DO SS1 = 1, N_SS_SPC
  
         IF( NUM_DEPEND( SS1 ) .EQ. 0 ) THEN
            NUM_RSLVD = NUM_RSLVD + 1
            RSLVD_IND( NUM_RSLVD ) = SS1
            LRSLVD( SS1 ) = .TRUE.
         ENDIF

      ENDDO


c..Now do rest of species by checking dependencies
 

100   CONTINUE

      SAVD_NUM_RSLVD = NUM_RSLVD 

      DO SS1 = 1, N_SS_SPC                   ! Loop over all the SS species
 
        IF( .NOT. LRSLVD( SS1 ) ) THEN       ! Consider only species not yet resolved

           

c..see if all the reactant SS species for this SS product have been resolved
            LDEPOK = .TRUE.
            DO SS2 = 1, N_SS_SPC            
               IF( SS_DEPEND( SS1, SS2 ) .EQ. 1 .AND. .NOT. LRSLVD( SS2 ) ) LDEPOK = .FALSE.
            ENDDO
             
c..If they have, add this species to list of resolved species 
           IF( LDEPOK ) THEN
                NUM_RSLVD = NUM_RSLVD + 1
                RSLVD_IND( NUM_RSLVD ) = SS1
                LRSLVD( SS1 ) = .TRUE.
            ENDIF

         ENDIF

      ENDDO


c..Check to see if all species have been resolved.
c..IF NO
c     1) check to make sure at least one species was added in last pass
c     2) go back and do another pass
c..IF YES
c     1) reorder the incoming array and return


      IF( NUM_RSLVD .LT. N_SS_SPC ) THEN        ! Not finished - check for error or go back

         IF( NUM_RSLVD .EQ. SAVD_NUM_RSLVD ) THEN  ! Error - could not add any more to list
            WRITE( LUNOUT ,  9600 )
            DO SS1 = 1, N_SS_SPC
               IF( .NOT. LRSLVD( SS1 ) ) WRITE( LUNOUT, 9620 ) SS_SPC( SS1 )
            ENDDO
            WRITE( LUNOUT , 10000 )
            STOP
         ELSE
            GO TO 100 
         ENDIF

      ELSE                                    ! Finished -- do the final reordering

c..load names into temporary array
         DO SS1 = 1, NUM_RSLVD
           SS_SPC_TEMP( SS1 ) = SS_SPC( SS1 )
         ENDDO    

c..Reorder the original incoming list
         DO SS1 = 1, NUM_RSLVD
            SS_SPC( SS1 ) = SS_SPC_TEMP( RSLVD_IND( SS1 ) )
         ENDDO

c..Reorder the production and loss coefficients too
         ALLOCATE( TEMP_RCT_COEF( N_SS_SPC, NR ) )
         ALLOCATE( TEMP_PRD_COEF( N_SS_SPC, NR ) )

         DO SS1 = 1, N_SS_SPC
            DO NRX = 1, NR
               TEMP_RCT_COEF( SS1, NRX ) = SS_RCT_COEF( SS1, NRX )
               TEMP_PRD_COEF( SS1, NRX ) = SS_PRD_COEF( SS1, NRX )
            ENDDO
         ENDDO

         DO SS1 = 1, N_SS_SPC
            DO NRX = 1, NR
               SS_RCT_COEF( SS1, NRX ) = TEMP_RCT_COEF( RSLVD_IND( SS1 ), NRX ) 
               SS_PRD_COEF( SS1, NRX ) = TEMP_PRD_COEF( RSLVD_IND( SS1 ), NRX )
            ENDDO
         ENDDO

         DEALLOCATE( TEMP_RCT_COEF )
         DEALLOCATE( TEMP_PRD_COEF )

      ENDIF



1000  CONTINUE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find the first reaction in which each SS species appears (for the SPCS.EXT file output)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SS1RX = 0     !  Array

      DO SS1 = 1, N_SS_SPC

         DO NRX = 1, NR

            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0 .OR. SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN
                SS1RX( SS1 ) = NRX
                EXIT
            ENDIF

         ENDDO

      ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Deallocate and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 

      DEALLOCATE ( SS_SPC_IND )
      DEALLOCATE ( NUM_DEPEND )
      DEALLOCATE ( LRSLVD )
      DEALLOCATE ( LREACT )
      DEALLOCATE ( LPROD )
      DEALLOCATE ( SS_SPC_TEMP )
      DEALLOCATE ( SS_DEPEND )

      RETURN       



 9000 FORMAT( / 5X, '*** ERROR: The following species defined as steady-state are not in '
     &              'the mechanism:')

 9020 FORMAT( / 5X, '*** ERROR: The following reactions have more than 1 steady-state '
     &              'species reacting:')
 9040 FORMAT( / 5X, '*** ERROR: The following reactions have a steady-state species '
     &              'that is both a reactant and product:')
 9060 FORMAT( / 5X, '*** ERROR: The following steady-state species do not appear as'
     &              ' both a reactant and a product')

 9500 FORMAT( /10X, 'Steady-state species number ', I3,': ', A )
 9520 FORMAT( /10X, 'Reaction number ', I3,': <', A, '>' )
 9600 FORMAT( / 5X, '***ERROR: Could not resolve dependencies for the following '
     &              'steady-state species:' )
 9620 FORMAT( /10X, A )

10000 FORMAT( //5X, ' Abnormal termination of CHEMMECH ' )

      END SUBROUTINE CHECK_SS_SPC 
     
      END MODULE GET_MECHDEF_DATA

