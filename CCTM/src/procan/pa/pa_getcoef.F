
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      SUBROUTINE GETCOEF( COEF )

C-----------------------------------------------------------------------
C Function: To get a coefficient from the input buffer
 
C Preconditions: None
  
C Key Subroutines/Functions Called: GETCHR
C                                   PA_GETREAL
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------

      USE UTILIO_DEFN, Only: M3MESG, M3EXIT, XSTAT2      ! IOAPI module
      USE PA_GLOBAL     ! Mech data used 
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      REAL    COEF        ! Returned coefficient
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      INTEGER INTCHR            ! Integer ASCII code of a character
      LOGICAL LNEG              ! Flag for negative coefficient
      
C-----------------------------------------------------------------------
      COEF = 1.0
      LNEG = .FALSE.

      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
         IF ( CHR .EQ. '-' ) LNEG = .TRUE.
         CALL GETCHR
      END IF 

      INTCHR = ICHAR( CHR )

      IF ( ( INTCHR .GE. 48 .AND. INTCHR .LE. 57 ) .OR.
     &      CHR .EQ. '.' ) THEN
         CALL PA_GETREAL
         COEF = NUMBER
         IF ( CHR .EQ. '*' ) THEN
            CALL GETCHR
         ELSE
            WRITE( MSG, 94000 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94500 ) LINNUM, INBUF
            CALL M3MESG( MSG )
            WRITE( MSG, 94520 ) CHR
            CALL M3MESG( MSG )
            CALL M3EXIT( 'GETCOEF', IZERO, IZERO, ' ', XSTAT2 )
         END IF
      END IF
      IF ( LNEG ) COEF = -COEF

      RETURN

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: An asterisk (*) must follow a coefficient' )
94500 FORMAT( '       Line No. ', I4, ': ', A )
94520 FORMAT( '       Character found: ', A )

      END SUBROUTINE GETCOEF
