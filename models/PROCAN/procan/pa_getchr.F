
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_getchr.F,v 1.5 2011/11/02 14:33:56 yoj Exp $

      SUBROUTINE GETCHR

C***********************************************************************
 
C Function: GETCHR returns the next non-blank character in the input
C           buffer, excluding those in comments
 
C Preconditions: None
  
C Key Subroutines/Functions Called: EATCOM
C                                   RDLINE
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
 
C***********************************************************************
      IMPLICIT NONE
      
C Includes:
      INCLUDE 'PA_PARSE.EXT' 
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables: None
         
C***********************************************************************

20    CONTINUE
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
      END
