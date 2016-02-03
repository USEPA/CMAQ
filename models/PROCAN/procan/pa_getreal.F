
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_getreal.F,v 1.5 2011/11/02 14:33:57 yoj Exp $

      SUBROUTINE GETREAL

C***********************************************************************
 
C Function: GETWORD gets the next word from the input buffer starting
C           with the current character; the word can be any length,
C           but only the first sixteen characters are retained.  The
C           first character must be alphabetic.
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
 
C***********************************************************************
      USE M3UTILIO      ! IOAPI module

      IMPLICIT NONE
      
C Includes:
      INCLUDE 'PA_PARSE.EXT' 
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 15 ) ::        NUMSTRING  ! String holding number

      INTEGER ICHR      ! ASCII code number of character
      INTEGER LENGTH    ! Length of numeric string
      INTEGER NOSIGNS   ! Number of pos or neg signs found
      INTEGER START     ! Starting position in input buffer

      LOGICAL LDECIMAL  ! Flag to indicate decimal point found
      LOGICAL LEXP      ! Flag to indicate exponent found
         
C***********************************************************************

      START    = LPOINT
      LENGTH   = 0
      NOSIGNS  = 0
      LDECIMAL = .FALSE.
      LEXP     = .FALSE.

c..Top of loop for getting characters in the number string
10    CONTINUE

      IF ( LENGTH .NE. 0 ) THEN
         LPOINT = LPOINT + 1
         IF ( LPOINT .GT. IEOL ) THEN
            CHR = ' '
         ELSE
            CHR = INBUF( LPOINT:LPOINT )
         END IF
      END IF

      ICHR = ICHAR( CHR )
      LENGTH = LENGTH + 1

c..Check for decimal point
      IF ( CHR .EQ. '.' ) THEN
         IF ( .NOT. LDECIMAL ) THEN
            LDECIMAL = .TRUE.
            GO TO 10
         ELSE
            WRITE( MSG, 94000 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94500 ) LINNUM, INBUF
            CALL M3MESG( MSG )
            WRITE( MSG, 94520 ) INBUF( START:LPOINT )
            CALL M3MESG( MSG )
            CALL M3EXIT( 'GETREAL', IZERO, IZERO,' ', XSTAT2 )
         END IF        
      END IF   

c..If numeric value, get next character
      IF ( ICHR .GE. 48 .AND. ICHR .LE. 57 ) GO TO 10

c..Check for exponent term
      IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' ) THEN
         IF ( .NOT. LEXP ) THEN
            LEXP = .TRUE.
            GO TO 10
         ELSE
            WRITE( MSG, 94020 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94500 ) LINNUM, INBUF
            CALL M3MESG( MSG )
            WRITE( MSG, 94520 ) INBUF( START:LPOINT )
            CALL M3MESG( MSG )
            CALL M3EXIT( 'GETREAL', IZERO, IZERO, ' ', XSTAT2 )
         END IF
      END IF
      
c..check for sign
      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
         NOSIGNS = NOSIGNS + 1
         IF ( NOSIGNS .LE. 2 ) THEN
            GO TO 10
         ELSE
            WRITE( MSG, 94040 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94500 ) LINNUM, INBUF
            CALL M3MESG( MSG )
            WRITE( MSG, 94520 ) INBUF( START:LPOINT )
            CALL M3MESG( MSG )
            CALL M3EXIT( 'GETREAL', IZERO, IZERO, ' ', XSTAT2 )
         END IF
      END IF

c..End of the numeric string; convert to real number
      NUMSTRING = INBUF( START:LPOINT-1 )
      LENGTH = LENGTH - 1
      IF ( ( .NOT. LEXP ) .AND. ( .NOT. LDECIMAL ) ) THEN
         NUMSTRING = NUMSTRING( 1:LENGTH ) // '.'
         LENGTH = LENGTH + 1
      END IF

      READ( NUMSTRING( 1:LENGTH ), '( E20.6 )', ERR = 999 ) NUMBER
      IF ( LPOINT .GT. IEOL ) CALL RDLINE
      IF ( CHR .EQ. ' ' ) CALL GETCHR

      RETURN

c..Error converting numeric string to real number
999   CONTINUE
      WRITE( MSG, 94060 ) 
      CALL M3MESG( MSG )
      WRITE( MSG, 94500 ) LINNUM, INBUF
      CALL M3MESG( MSG )
      WRITE( MSG, 94520 ) INBUF( START:LPOINT )
      CALL M3MESG( MSG )
      CALL M3EXIT( 'GETREAL', IZERO, IZERO, ' ', XSTAT2 )
      
C*********************** FORMAT Statements ***************************** 

94000 FORMAT( 'ERROR: Two decimal points found in a numeric field' )
94020 FORMAT( 'ERROR: More than one E or e found in a numeric field' )
94040 FORMAT( 'ERROR: Too many + or - signs found in a numeric field' )
94060 FORMAT( 'ERROR: Numeric field contains an invalid character' )
94500 FORMAT( '       Line No. ', I4, ': ', A )
94520 FORMAT( '       Numeric field: ', A )

      END
