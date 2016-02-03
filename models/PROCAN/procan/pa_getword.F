
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_getword.F,v 1.5 2011/11/02 14:33:58 yoj Exp $

      SUBROUTINE GETWORD

C***********************************************************************
 
C Function: GETWORD gets the next word from the input buffer starting
C           with the current character; the word can be any length,
C           but only the first sixteen characters are retained.  The
C           first character must be alphabetic.
 
C Preconditions: None
  
C Key Subroutines/Functions Called: EATCOM
C                                   GETCHR
C                                   RDLINE
 
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
      CHARACTER( BUFLEN ) :: SAVBUF  ! Saved input buffer

      INTEGER START     ! Starting position in input buffer
      INTEGER LENGTH    ! Length of word
         
C***********************************************************************

c..Setup and make sure the first character is alphabetic
      LENGTH = 0
      START  = LPOINT
      SAVBUF = INBUF
      IF ( ( CHR .LT. 'A' .OR. CHR .GT. 'Z' ) .AND. 
     &     ( CHR .LT. 'a' .OR. CHR .GT. 'z' ) ) THEN
         WRITE( MSG, 94000 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94020 ) LINNUM, INBUF 
         CALL M3MESG( MSG )
         WRITE( MSG, 94040 ) CHR 
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETWORD', IZERO, IZERO, ' ', XSTAT2 )
      END IF

c..Get the remaining characters; stop if a word spans a line
20    CONTINUE
      LENGTH = LENGTH + 1
      LPOINT = LPOINT + 1

c..Get the next line if necessary (word wrap not allowed)
      IF ( LPOINT .GT. IEOL ) THEN
         CALL RDLINE
         CALL GETCHR
         CHR = INBUF( LPOINT:LPOINT )
         IF ( ( CHR .GE. 'A' .AND. CHR .LE. 'Z' ) .OR. 
     &       ( CHR .GE. 'a' .AND. CHR .LE. 'z' ) .OR.
     &       ( CHR .EQ. ':' ) .OR. ( CHR .EQ. '_' ) )THEN
            WRITE( MSG, 94060 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94020 ) LINNUM, SAVBUF 
            CALL M3EXIT( 'GETWORD', IZERO, IZERO, ' ', XSTAT2 )
         ELSE
            GO TO 40  ! Word does not wrap around line
         END IF
      END IF

c..Include all characters in word that are valid; stop reading on an 
c..ivalid character
      CHR = SAVBUF( LPOINT:LPOINT )
      IF ( CHR .GE. 'A' .AND. CHR .LE. 'Z' ) GO TO 20
      IF ( CHR .GE. 'a' .AND. CHR .LE. 'z' ) GO TO 20      
      IF ( CHR .GE. '0' .AND. CHR .LE. '9' ) GO TO 20
      IF ( CHR .EQ. ':' ) GO TO 20
      IF ( CHR .EQ. '_' ) GO TO 20
      IF ( CHR .EQ. '('  .OR. CHR .EQ. '{' ) THEN 
         LENGTH = LENGTH - 1
         CALL EATCOM
         GO TO 20
      END IF
      
c..Truncate the word and get the next character if necessary
40    CONTINUE
      LENGTH = MIN( LENGTH, WRDLEN )
      WORD = SAVBUF( START:START+LENGTH-1 )
      IF ( CHR .EQ. ' ' ) CALL GETCHR

      RETURN
      
C*********************** FORMAT Statements *****************************
      
94000 FORMAT( 'ERROR: The first character of a word must be alphabetic' )
94020 FORMAT( '   Line No. ', I4, ': ', A )
94040 FORMAT( '   First character: ', A )
94060 FORMAT( 'ERROR: A word cannot span two lines' )

      END
   
