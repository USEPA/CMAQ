
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

      SUBROUTINE GETIPROUT

C-----------------------------------------------------------------------
C Function: To process the IPR_OUTPUT option 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: GETCHR
C                                   GETWORD
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------
 
      USE UTILIO_DEFN
      USE PA_DEFN, Only: N_IPR_SPC
      USE PA_IPRVARS
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'GETIPROUT'   ! Program name

C-----------------------------------------------------------------------

c..Increment the counter and check for too many
      N_IPR_SPC = N_IPR_SPC + 1
      N_IPR_OPS( N_IPR_SPC ) = 0
      IF ( N_IPR_SPC .GT. MAXIPRSPC ) THEN
         WRITE( MSG, 94000 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94020 ) MAXIPRSPC
         CALL M3MESG( MSG )
         CALL M3EXIT( PNAME, IZERO, IZERO, ' ', XSTAT2 )
      END IF

c..get the name of the species for IPR_OUTPUT
      CALL GETWORD
      IPR_SPNAM( N_IPR_SPC ) = WORD

c..if the next character is a semicolon, return
      IF ( CHR .EQ. ';' ) THEN
         CALL GETCHR
         RETURN
      END IF

c..the next character must be an equal sign
      IF ( CHR .EQ. '=' ) THEN
         CALL GETCHR         
      ELSE
         WRITE( MSG, 94040 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( PNAME, IZERO, IZERO, ' ', XSTAT2 )
      END IF

c..top of loop for getting operators for this species
20    CONTINUE

c..increment the counters and check for too many
      IF ( CHR .EQ. '+' ) CALL GETCHR
      N_IPR_OPS( N_IPR_SPC ) = N_IPR_OPS( N_IPR_SPC ) + 1

      IF ( N_IPR_OPS( N_IPR_SPC ) .GT. MAXTERMS ) THEN
         WRITE( MSG, 94060 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94080 ) MAXTERMS
         CALL M3MESG( MSG )
         CALL M3EXIT( PNAME, IZERO, IZERO, ' ', XSTAT2 )
      END IF

c..get the operator name
      CALL GETWORD
      IPR_OPNAME( N_IPR_SPC, N_IPR_OPS( N_IPR_SPC ) ) = WORD

c..if more operators, go back or return or err off
      IF ( CHR .EQ. '+' ) THEN
         GO TO 20
      ELSE IF ( CHR .EQ. ';' ) THEN
         CALL GETCHR
         RETURN
      ELSE
         WRITE( MSG, 94100 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( PNAME, IZERO, IZERO, ' ', XSTAT2 )
      END IF

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of PA species exceeded' )
94020 FORMAT( '       Modify PARAMETER ( MAXIPRSPC =', I3,' ) or',
     &              '  decrease the number of PA species' )
94040 FORMAT( 'ERROR: Expecting a = or ; after IPR_OUTPUT species',
     &              '  name' ) 
94060 FORMAT( 'ERROR: Maximum number of terms in a PROC_OUTPUT',
     &              '  exceeded' )
94080 FORMAT( '       Modify PARAMETER ( MAXTERMS =', I3,' ) or',
     &              '  decrease the number of terms in the',
     &              '  IPR_OUTPUT request' )
94100 FORMAT( 'ERROR: Expecting a + or ; after a IPR_OUTPUT operator' )
94500 FORMAT( '       Line No. ', I4, ': ', A )
94520 FORMAT( '       Character found: ', A )

      END SUBROUTINE GETIPROUT

