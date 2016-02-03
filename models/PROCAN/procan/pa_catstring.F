
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_catstring.F,v 1.6 2011/11/02 14:33:55 yoj Exp $

      SUBROUTINE CATSTRING( NUMSTRNGS, STRING, STRLEN, STROUT, MAXLEN )

C***********************************************************************
 
C Function: Concatenate a series of strings, eliminating trailing
C           blanks, but leaving a space between each string 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Updated May 2002 by Jerry Gipson for dynamic allocation version of CCTM
 
C***********************************************************************
      IMPLICIT NONE
      
C Includes: 
      INCLUDE 'PA_PARSE.EXT'
      
C Arguments:
      INTEGER NUMSTRNGS              ! Number of strings to concatenate
      CHARACTER( * ) :: STRING( * )  ! Strings to concatenate 
      INTEGER STRLEN                 ! String length
      CHARACTER( * ) :: STROUT       ! Concatenated string output
      INTEGER MAXLEN                 ! Maximum length of output string
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      INTEGER LPOS       ! Last position in output string
      INTEGER N          ! Loop index for number of strings to concatenate
      INTEGER NEWLEN     ! Length of string to be concatenated
      LOGICAL LRETURN    ! Flag to return because of truncation

C***********************************************************************
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
      
C*********************** FORMAT Statements *****************************
      
94000 FORMAT( 1X,'WARNING: A string has been truncated because of ',
     &           'excessive length in concatenation' )

      END

