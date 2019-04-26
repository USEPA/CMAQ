
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

      INTERFACE 
        SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
        END SUBROUTINE RDLINE
        SUBROUTINE GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         INTEGER,         INTENT( IN )    :: IMECH
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
        END SUBROUTINE GETCHAR
        SUBROUTINE GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
         INTEGER,         INTENT( IN )    :: IMECH   ! IO unit for mechanism file
         CHARACTER*( * ), INTENT( INOUT ) :: CHR     ! current character from buffer
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF   ! string read from mechanism file
         INTEGER,         INTENT( INOUT ) :: LPOINT  ! character position in INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL    ! end of line position
         REAL( 8 ),       INTENT( OUT )   :: NUMBER  ! number from file
        END SUBROUTINE GETREAL
        SUBROUTINE GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT(  OUT  ) :: WORD
        END SUBROUTINE GETWORD
        SUBROUTINE GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL )
         INTEGER,         INTENT( IN )    :: IMECH
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
         CHARACTER*( * ), INTENT( INOUT ) :: LABEL
        END SUBROUTINE GETLABEL
      END INTERFACE

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
      END
