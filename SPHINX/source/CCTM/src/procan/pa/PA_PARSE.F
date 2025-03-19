
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

C......................................................................
      MODULE PA_PARSE
 
C Contains: data used by parsing routines in the Process Analysis Control Program
 
C methods: rdline, getchr, getword, eatcom, getreal, getlabel, getquote, catstring
 
C Revision History:
C  Based on code created by Jerry Gipson, 1996, 1997
C  PA_PARSE.F module; 13 Jul 2016; Jeff Young
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C......................................................................

      IMPLICIT NONE

C Parameters:
      INTEGER, PARAMETER :: BUFLEN = 81  ! Length of one input line
      INTEGER, PARAMETER :: LABLEN = 16  ! Label character length
      INTEGER, PARAMETER :: WRDLEN = 16  ! Word character length
      INTEGER, PARAMETER :: IZERO = 0    ! Integer zero

C Variables:
      CHARACTER(   1 )    :: CHR   ! Last character read from input buffer
      CHARACTER( 200 )    :: MSG   ! Error message output buffer 
      CHARACTER(  60 )    :: QUOTE ! String found by GETQUOTE
      CHARACTER( BUFLEN ) :: INBUF ! Input line
      CHARACTER( LABLEN ) :: LABEL ! Label found by getlabel  
      CHARACTER( WRDLEN ) :: WORD  ! Word found by GETWORD
    
      INTEGER IEOL       ! Position of last non-blank character
      INTEGER IFILE      ! Unit number of input ASCII File
      INTEGER LINNUM     ! Number of last line read 
      INTEGER LPOINT     ! Pointer to location of last character read

      REAL NUMBER        ! Real number found by GETREAL

      CONTAINS
C=======================================================================
         SUBROUTINE RDLINE

C......................................................................
C Function: Reads one line from a file and stores it in INBUF.
C           The position of the last non-blank character is stored
C           in IEOL. All blank lines and lines with a ! in the first
C           column are skipped. The line count is stored in LINNUM.
C......................................................................

         USE UTILIO_DEFN

         IMPLICIT NONE
C.......................................................................

c..Read the first non-comment line and find the last non-blank character
c..in that line
20       CONTINUE
         READ( IFILE, '( A )', END = 999 ) INBUF
         LINNUM = LINNUM + 1
         IF ( INBUF( 1:1 ) .EQ. '!' ) THEN
            GO TO 20
         ELSE
            IEOL = LEN_TRIM( INBUF )
            IF ( IEOL .EQ. 1 .AND. INBUF( 1:1 ) .EQ. ' ' ) THEN
               GO TO 20
            ELSE
               LPOINT = 0
               RETURN
            END IF
         END IF

999      CONTINUE
c..eof file encountered -- exit program
         WRITE( MSG, 94000 )
         CALL M3MESG( MSG )
         WRITE( MSG, 94020 )
         CALL M3MESG( MSG )
         CALL M3EXIT( 'RDLINE', IZERO, IZERO, ' ', XSTAT2 )

94000    FORMAT( 'ERROR: End of file reached on the Process Analysis',
     &           ' command file' )
94020    FORMAT( 10X, 'ENDPA must be the last command' )

         END SUBROUTINE RDLINE

C=======================================================================
         SUBROUTINE GETCHR

C......................................................................
C Function: Returns the next non-blank character in the input buffer,
C           excluding those in comments

C Key Subroutines/Functions Called: EATCOM, RDLINE
C......................................................................

         IMPLICIT NONE
C.......................................................................

20       CONTINUE
         LPOINT = LPOINT + 1
         IF ( LPOINT .GT. IEOL ) THEN
            CALL RDLINE
            GO TO 20
         ELSE
            CHR = INBUF( LPOINT : LPOINT )
            IF ( CHR .EQ. ' ' ) THEN
               GO TO 20
            ELSE IF ( CHR .EQ. '(' .OR. CHR .EQ. '{' ) THEN
               CALL EATCOM
               GO TO 20
            END IF
         END IF

         RETURN
         END SUBROUTINE GETCHR

C=======================================================================
         SUBROUTINE GETWORD

C......................................................................
C Function: Get the next word from the input buffer starting
C           with the current character; the word can be any length,
C           but only the first sixteen characters are retained.  The
C           first character must be alphabetic.

C Preconditions: None

C Key Subroutines/Functions Called: EATCOM, GETCHR, RDLINE
C......................................................................

         USE UTILIO_DEFN

         IMPLICIT NONE

C Local Variables:
         CHARACTER( BUFLEN ) :: SAVBUF  ! Saved input buffer

         INTEGER START     ! Starting position in input buffer
         INTEGER LENGTH    ! Length of word
C.......................................................................

c..Setup and make sure the first character is alphabetic
         LENGTH = 0
         START  = LPOINT
         SAVBUF = INBUF
         IF ( ( CHR .LT. 'A' .OR. CHR .GT. 'Z' ) .AND.
     &        ( CHR .LT. 'a' .OR. CHR .GT. 'z' ) ) THEN
            WRITE( MSG, 94000 )
            CALL M3MESG( MSG )
            WRITE( MSG, 94020 ) LINNUM, INBUF
            CALL M3MESG( MSG )
            WRITE( MSG, 94040 ) CHR
            CALL M3MESG( MSG )
            CALL M3EXIT( 'GETWORD', IZERO, IZERO, ' ', XSTAT2 )
         END IF

c..Get the remaining characters; stop if a word spans a line
20       CONTINUE
         LENGTH = LENGTH + 1
         LPOINT = LPOINT + 1

c..Get the next line if necessary (word wrap not allowed)
         IF ( LPOINT .GT. IEOL ) THEN
            CALL RDLINE
            CALL GETCHR
            CHR = INBUF( LPOINT:LPOINT )
            IF ( ( CHR .GE. 'A' .AND. CHR .LE. 'Z' ) .OR.
     &          ( CHR .GE. 'a' .AND. CHR .LE. 'z' ) .OR.
     &          ( CHR .EQ. ':' ) .OR. ( CHR .EQ. '_' ) )THEN
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
40       CONTINUE
         LENGTH = MIN( LENGTH, WRDLEN )
         WORD = SAVBUF( START:START+LENGTH-1 )
         IF ( CHR .EQ. ' ' ) CALL GETCHR

         RETURN

94000    FORMAT( 'ERROR: The first character of a word must be alphabetic' )
94020    FORMAT( '   Line No. ', I4, ': ', A )
94040    FORMAT( '   First character: ', A )
94060    FORMAT( 'ERROR: A word cannot span two lines' )

         END SUBROUTINE GETWORD

C=======================================================================
         SUBROUTINE EATCOM

C.......................................................................
C Function: EATCOM reads past all characters in comments; comments are
C              enclosed in () or {}
 
C Key Subroutines/Functions Called: GETCHR, RDLINE
C.......................................................................
 
         IMPLICIT NONE
C.......................................................................
         
20    CONTINUE
         LPOINT = LPOINT + 1
         IF ( LPOINT .GT. IEOL ) THEN
            CALL RDLINE
            CALL GETCHR
            GO TO 20
         ELSE
            CHR = INBUF( LPOINT:LPOINT )
            IF( CHR .NE. ')' .AND. CHR .NE. '}' ) GO TO 20
         END IF

         RETURN

         END SUBROUTINE EATCOM

C=======================================================================
         SUBROUTINE PA_GETREAL

C.......................................................................
C Function: get the next word from the input buffer starting
C              with the current character; the word can be any length,
C              but only the first sixteen characters are retained.  The
C              first character must be alphabetic.
C.......................................................................
         USE UTILIO_DEFN

         IMPLICIT NONE
         
C Local Variables:
         CHARACTER( 15 ) ::        NUMSTRING  ! String holding number

         INTEGER ICHR      ! ASCII code number of character
         INTEGER LENGTH    ! Length of numeric string
         INTEGER NOSIGNS   ! Number of pos or neg signs found
         INTEGER START     ! Starting position in input buffer

         LOGICAL LDECIMAL  ! Flag to indicate decimal point found
         LOGICAL LEXP      ! Flag to indicate exponent found
C.......................................................................
            
         START    = LPOINT
         LENGTH   = 0
         NOSIGNS  = 0
         LDECIMAL = .FALSE.
         LEXP     = .FALSE.

c..Top of loop for getting characters in the number string
10       CONTINUE

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
999      CONTINUE
         WRITE( MSG, 94060 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) INBUF( START:LPOINT )
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETREAL', IZERO, IZERO, ' ', XSTAT2 )
         
94000    FORMAT( 'ERROR: Two decimal points found in a numeric field' )
94020    FORMAT( 'ERROR: More than one E or e found in a numeric field' )
94040    FORMAT( 'ERROR: Too many + or - signs found in a numeric field' )
94060    FORMAT( 'ERROR: Numeric field contains an invalid character' )
94500    FORMAT( '          Line No. ', I4, ': ', A )
94520    FORMAT( '          Numeric field: ', A )

         END SUBROUTINE PA_GETREAL

C=======================================================================
         SUBROUTINE GETLABEL

C.......................................................................
C Function: get all characters except blanks between the
C              delimiters <> or []. Line wrap is allowed. The maximum
C              label length is sixteen characters.
                
C Key Subroutines/Functions Called: EATCOM, GETCHR
C.......................................................................
 
         USE UTILIO_DEFN

         IMPLICIT NONE
         
C Local Variables:
         CHARACTER( 1 ) :: BEGCHR   ! Starting delimiter for label
         CHARACTER( 1 ) :: ENDCHR   ! Ending delimiter for label

         INTEGER LENGTH    ! Length of label
C.......................................................................
            
c..Set beginning and and ending characters delineating the label
         LABEL  = ''
         LENGTH = 0
         IF ( CHR .EQ. '<' ) THEN
            BEGCHR = '<'
            ENDCHR = '>'
         ELSE
            BEGCHR = '['
            ENDCHR = ']'
         END IF
 
c..top of loop for getting next character
20       CONTINUE
         CALL GETCHR

c..if the end of the label has not been reached, store the character
         IF ( CHR .NE. ENDCHR ) THEN
            IF ( CHR .EQ. '(' .OR. CHR .EQ. '{' ) CALL EATCOM
            LENGTH = LENGTH + 1 
            IF ( LENGTH .GT. 16 ) THEN
               WRITE( MSG, 94000 ) 
               CALL M3MESG( MSG )
               WRITE( MSG, 94020 ) LINNUM, INBUF
               CALL M3MESG( MSG )
               WRITE( MSG, 94040 ) BEGCHR, LABEL
               CALL M3MESG( MSG )
               CALL M3EXIT( 'PA_GETLABEL', IZERO, IZERO, ' ', XSTAT2 )
            END IF
            LABEL( LENGTH : LENGTH ) = CHR
            GO TO 20
         END IF

         CALL GETCHR

         RETURN
         
94000    FORMAT( 'ERROR: A Reaction label or operator option exceeds 16',
     &                 ' characters' )
94020    FORMAT( '          Line No. ', I4, ': ', A )
94040    FORMAT( '          Label found : ', A, A )

         END SUBROUTINE GETLABEL

C=======================================================================
         SUBROUTINE GETQUOTE

C.......................................................................
C Function: get all characters between apostrophes. 
C           Embedded apostrophes indicated by double apostrophes.
C           Line wrap is allowed.
  
C Key Subroutines/Functions Called: RDLINE, GETCHR
C.......................................................................
 
         USE UTILIO_DEFN

         IMPLICIT NONE
         
C Local Variables:
         CHARACTER(   1 ) :: APOS = ''''  ! Apostrophe (') character
         CHARACTER( 132 ) :: STRING       ! Holder for qoute

         INTEGER LENGTH    ! Length of quote
         INTEGER NXTPOS    ! Next character position in input buffer

         LOGICAL LCONT     ! Flag to continue parsing input buffer
C.......................................................................

         LENGTH = 0

c..Top of loop for reading quote
20       CONTINUE
         LPOINT = LPOINT + 1

c..get the next line if necessary
         IF ( LPOINT .GT. IEOL ) THEN
            CALL RDLINE
            CALL GETCHR
         END IF

c..get the next character
         CHR = INBUF( LPOINT:LPOINT )

c..check for leading/endiung double apostrophes and get the next character
         IF ( CHR .NE. APOS ) THEN
            LCONT = .TRUE.
         ELSE
            IF ( LPOINT + 1 .GT. IEOL ) THEN
               CALL RDLINE
               NXTPOS = 1
            ELSE
               NXTPOS = LPOINT + 1
            END IF
            IF ( INBUF( NXTPOS : NXTPOS ) .EQ. APOS ) THEN
               LCONT  = .TRUE.
               LPOINT = LPOINT + 1
               CHR = INBUF( LPOINT:LPOINT )
            ELSE
               LCONT = .FALSE.
               CALL GETCHR
            END IF
         END IF

c..if continuing, add current character to the output string if 
c..length is OK
         IF ( LCONT ) THEN               
            LENGTH = LENGTH + 1
            IF ( LENGTH .GT. 132 ) THEN
               WRITE( MSG, 94000 ) 
               CALL M3MESG( MSG )
               WRITE( MSG, 94020 ) LINNUM, INBUF
               CALL M3MESG( MSG )
               CALL M3EXIT( 'PA_GETQUOTE', IZERO, IZERO, ' ', XSTAT2 )
            END IF

            IF ( LENGTH .EQ. 1 ) THEN
               STRING( 1:1 ) = CHR
            ELSE
               STRING( 1:LENGTH ) = STRING( 1:LENGTH-1 ) // CHR
            END IF     
            GO TO 20
         END IF

c..Linit output string to 60 characters
         LENGTH = MIN( LENGTH, 60 )
         QUOTE = STRING( 1:LENGTH )

         RETURN

94000    FORMAT( 'ERROR: Quote exceeds maximum allowable 132 characters' )
94020    FORMAT( '         Line No. ', I4, ': ', A )

         END SUBROUTINE GETQUOTE

C=======================================================================
         SUBROUTINE CATSTRING( NUMSTRNGS, STRING, STROUT, MAXLEN )

C.......................................................................
C Function: Concatenate a series of strings, eliminating trailing
C           blanks, but leaving a space between each string 
C.......................................................................

         IMPLICIT NONE
         
C Arguments:
         INTEGER NUMSTRNGS              ! Number of strings to concatenate
         CHARACTER( * ) :: STRING( * )  ! Strings to concatenate 
         CHARACTER( * ) :: STROUT       ! Concatenated string output
         INTEGER MAXLEN                 ! Maximum length of output string
                                           
C Local Variables:
         INTEGER LPOS       ! Last position in output string
         INTEGER N          ! Loop index for number of strings to concatenate
         INTEGER NEWLEN     ! Length of string to be concatenated
         LOGICAL LRETURN    ! Flag to return because of truncation
C.......................................................................

         LRETURN = .FALSE.
         STROUT = STRING( 1 )
         LPOS = LEN_TRIM( STRING( 1 ) ) + 1
         IF ( NUMSTRNGS .EQ. 1 ) RETURN
         DO N = 2, NUMSTRNGS
            NEWLEN = LEN_TRIM( STRING( N ) )
            IF ( ( LPOS + NEWLEN ) .GE. MAXLEN ) THEN
               NEWLEN = MAXLEN - LPOS
               WRITE( MSG, 94000 ) 
               CALL M3MESG( MSG )
               LRETURN = .TRUE.
            END IF
            STROUT = STROUT( 1:LPOS ) // STRING( N )( 1:NEWLEN )
            IF ( LRETURN ) THEN
               RETURN
            ELSE
               LPOS = LPOS + NEWLEN + 1
            END IF
         END DO
         DO N = 1, NUMSTRNGS
            STRING( N ) = ''
         END DO

         RETURN
         
94000    FORMAT( 1X,'WARNING: A string has been truncated because of ',
     &              'excessive length in concatenation' )

         END SUBROUTINE CATSTRING

      END MODULE PA_PARSE
