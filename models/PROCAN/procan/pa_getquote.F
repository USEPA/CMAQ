
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_getquote.F,v 1.5 2011/11/02 14:33:57 yoj Exp $

      SUBROUTINE GETQUOTE

C***********************************************************************
 
C Function: GETQUOTE gets all characters between apostrophes. 
C           Embedded apostrophes indicated by double apostrophes.
C           Line wrap is allowed.
             
C Preconditions: None
  
C Key Subroutines/Functions Called: RDLINE
C                                   GETCHR
 
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
      CHARACTER(   1 ) :: APOS = ''''  ! Apostrophe (') character
      CHARACTER( 132 ) :: STRING       ! Holder for qoute

      INTEGER LENGTH    ! Length of quote
      INTEGER NXTPOS    ! Next character position in input buffer

      LOGICAL LCONT     ! Flag to continue parsing input buffer
         
C***********************************************************************
      LENGTH = 0

c..Top of loop for reading quote
20    CONTINUE
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
      
C*********************** FORMAT Statements ******************************

94000 FORMAT( 'ERROR: Quote exceeds maximum allowable 132 characters' )
94020 FORMAT( '      Line No. ', I4, ': ', A )

      END
