
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

      SUBROUTINE GETDESC( LASTCMND )

C-----------------------------------------------------------------------
C Function: To process the DESC command 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: GETCHR
C                                   GETQUOTE
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------
      USE UTILIO_DEFN
      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS, Only: NIRROUT
      USE PA_DEFN
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      CHARACTER( 16 ) :: LASTCMND  ! Previous command
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 1 ) :: APOSTROPHE = "'"

C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Make sure the = sign is there
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .NE. '=' ) THEN
         WRITE( MSG, 94000 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETDESC', IZERO, IZERO, ' ', XSTAT2 )
      ELSE
         CALL GETCHR
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check for a leading apostrophe and get the description
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .NE. APOSTROPHE ) THEN
         WRITE( MSG, 94020 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETDESC', IZERO, IZERO, ' ', XSTAT2 )
      ELSE
         CALL GETQUOTE
         IF ( LASTCMND .EQ. 'IRR_OUTPUT' ) THEN
            IRRDESC( NIRROUT ) = QUOTE
         ELSE
            WRITE( MSG, 94600 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94500 ) LINNUM, INBUF
            CALL M3MESG( MSG )
         END IF
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check for the ending semicolon
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .NE. ';' ) THEN
         WRITE( MSG, 94040 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETDESC', IZERO, IZERO, ' ', XSTAT2 )
      ELSE
         CALL GETCHR
         RETURN
      END IF
      
C----------------------- FORMAT Statements ----------------------------- 
      
94000 FORMAT( 'ERROR: Equal sign must follow the DESCRIPTION command' )
94020 FORMAT( 'ERROR: Apostrophe expected after the = sign in the',
     &              ' DESCRIPTION command' )
94040 FORMAT( 'ERROR: The DESCRIPTION command must be terminated by a ;' )
94500 FORMAT( '       Line No. ', I4, ': ', A )
94520 FORMAT( '       Character found: ', A )
94600 FORMAT( 'WARNING: DESCRIPTION command ignored, does not follow',
     &                ' an IRR_OUTPUT command' )

      END SUBROUTINE GETDESC
