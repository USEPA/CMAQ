
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

      SUBROUTINE GETFAMILY

C-----------------------------------------------------------------------
C Function: To process the DEFINE FAMILY command
 
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

C Local Variables:
      INTEGER MEMNUM    ! Counter for number of members in the family
      
C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the Family and check for a trailing equal sign
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NFAMLYS = NFAMLYS + 1
      MEMNUM = 0
      IF ( NFAMLYS .GT. MAXFAMLYS ) THEN
         WRITE( MSG, 94000 )
         CALL M3MESG( MSG )
         WRITE( MSG, 94020 ) MAXFAMLYS
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETFAMILY', IZERO, IZERO, ' ', XSTAT2 )
      END IF

      CALL GETWORD
      FAMNAME( NFAMLYS ) = WORD

      IF ( CHR .NE. '=' ) THEN
         WRITE( MSG, 94040 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETFAMILY', IZERO, IZERO,' ', XSTAT2 )
      ELSE
         CALL GETCHR
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Top of loop for getting the family member names and coefficients
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
20    CONTINUE
      MEMNUM = MEMNUM + 1

      IF ( MEMNUM .GT. MAXFAMMEM ) THEN
         WRITE( MSG, 94060 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94080 ) MAXFAMMEM
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETFAMILY', IZERO, IZERO, ' ', XSTAT2 )
      END IF

c..Set the coefficient for family member 
      CALL GETCOEF( FAMSC( NFAMLYS, MEMNUM ) ) 

c..Get the name of the family member
      CALL GETWORD
      FAMMEMNAM( NFAMLYS, MEMNUM ) = WORD
      NUMFAMMEM( NFAMLYS ) = MEMNUM 

c..Go back and get the next family member, return, or err off  
      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
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
         CALL M3EXIT( 'GETFAMILY', IZERO, IZERO, ' ', XSTAT2 ) 
      END IF

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of FAMILY''s exceeded' )
94020 FORMAT( '       Modify PARAMETER ( MAXFAMLYS =', I3,' ) or',
     &              '  decrease the number of defined FAMILY''s' )     
94040 FORMAT( 'ERROR: Equal sign expected after FAMILY name' )
94060 FORMAT( 'ERROR: Maximum number of family members exceeded' )
94080 FORMAT( '       Modify PARAMETER ( MAXFAMMEM =',I3, ' ) or',
     &              '  decrease the number of members in the family' )
94100 FORMAT( 'ERROR: A +, -, or ; must follow a family member name' )
94500 FORMAT( '       Line No. ', I4, ':', A )
94520 FORMAT( '       Character found:', A )

      END SUBROUTINE GETFAMILY
