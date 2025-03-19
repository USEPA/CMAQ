
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

      SUBROUTINE GETCYCLE

C-----------------------------------------------------------------------
C Function: To process the DEFINE CYCLE option 
 
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
      USE PA_VARS
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables: None

C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get assigned name and check for = sign delimiter
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCYCLES = NCYCLES + 1
      IF( NCYCLES .GT. MAXCYCLES ) THEN
         WRITE( MSG, 94000 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94020 ) MAXCYCLES
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETCYCLE', IZERO, IZERO, ' ', XSTAT2 )
      ENDIF

      CALL GETWORD
      CYCLNAME( NCYCLES ) = WORD

      IF( CHR .NE. '=' ) THEN
         WRITE( MSG, 94040 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETCYCLE', IZERO, IZERO, ' ', XSTAT2 )
      ELSE
         CALL GETCHR
      ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the name of the CYCLE species and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GETWORD
      CYCLSPEC( NCYCLES ) = WORD

      IF( CHR .EQ .';' ) THEN
         CALL GETCHR
         RETURN
      ELSE
         WRITE( MSG, 94060 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETCYCLE', IZERO, IZERO, ' ', XSTAT2 ) 
      END IF

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of CYCLEs exceeded' )
94020 FORMAT( '       Modify PARAMETER ( MAXCYCLES =', I3,' ) or',
     &              '  decrease the number of CYCLEs' )
94040 FORMAT( 'ERROR: An equal sign expected after CYCLE name' )
94060 FORMAT( 'ERROR: A ; must end a CYCLE definition' )

94500 FORMAT( '       Line No. ', I4, ': ', A )
94520 FORMAT( '       Character found: ', A )

      END SUBROUTINE GETCYCLE
