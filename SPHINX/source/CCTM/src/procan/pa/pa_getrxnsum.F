
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

      SUBROUTINE GETRXNSUM

C-----------------------------------------------------------------------
 
C FUNCTION: To process the DEFINE RXNSUM command 
 
C PRECONDITIONS: None
  
C KEY SUBROUTINES/FUNCTIONS CALLED: GETCHR
C                                   GETWORD
C                                   GETCOEF
C                                   GETLABEL
 
C REVISION HISTORY:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------
 
      USE UTILIO_DEFN
      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None

C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      INTEGER TERMNUM   ! Counter for number of terms in sum
      
C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get assigned name and check for = sign delimiter
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NRXSUMS = NRXSUMS + 1
      IF ( NRXSUMS .GT. MAXRXNSUM )  THEN
         WRITE( MSG, 94000 )  
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94500 )  LINNUM,  INBUF
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94020 )  MAXRXNSUM
         CALL M3MESG( MSG ) 
         CALL M3EXIT( 'GETRXNSUM',  IZERO,  IZERO, ' ', XSTAT2 ) 
      END IF

      TERMNUM = 0
      CALL GETWORD
      RXSUMNAME( NRXSUMS )  = WORD

      IF ( CHR .NE. '=' )  THEN
         WRITE( MSG, 94040 )  
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94500 )  LINNUM,  INBUF
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94520 )  CHR
         CALL M3MESG( MSG ) 
         CALL M3EXIT( 'GETRXNSUM',  IZERO,  IZERO, ' ', XSTAT2 ) 
      ELSE
         CALL GETCHR
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Top of loop for getting all terms and their coefficients
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
20    CONTINUE
      TERMNUM = TERMNUM + 1

      IF ( TERMNUM .GT. MAXRXTERMS )  THEN
         WRITE( MSG, 94060 )  
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94500 )  LINNUM,  INBUF
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94080 )  MAXRXTERMS
         CALL M3MESG( MSG ) 
         CALL M3EXIT( 'GETRXNSUM',  IZERO,  IZERO, ' ', XSTAT2 ) 
      END IF

c..Get coefficient for the reaction
      CALL GETCOEF( RXNSC( NRXSUMS, TERMNUM )  ) 

c..Get the reaction label 
      IF ( CHR .EQ. '<' )  THEN
         CALL GETLABEL
         RXSUMLBL( NRXSUMS, TERMNUM )  = LABEL
      ELSE
         WRITE( MSG, 94100 )  
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94500 )  LINNUM,  INBUF
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94520 )  CHR
         CALL M3MESG( MSG ) 
         CALL M3EXIT( 'GETRXNSUM',  IZERO,  IZERO, ' ', XSTAT2 ) 
      END IF

c..check for more terms or end delimiter         
      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' )  THEN
         GO TO 20
      ELSE IF ( CHR .EQ. ';' )  THEN
         NRXTERMS( NRXSUMS )  = TERMNUM
         CALL GETCHR
         RETURN
      ELSE
         WRITE( MSG, 94120 )  
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94500 )  LINNUM,  INBUF
         CALL M3MESG( MSG ) 
         WRITE( MSG, 94520 )  CHR
         CALL M3MESG( MSG ) 
         CALL M3EXIT( 'GETRXNSUM',  IZERO,  IZERO, ' ', XSTAT2 ) 
      END IF
      
C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of RXNSUMs exceeded' ) 
94020 FORMAT( '       Modify PARAMETER ( MAXRXNSUM =', I3, ' )', 
     &              ' or decrease the number of RXNSUMs' ) 
94040 FORMAT( 'ERROR: Equal sign expected after RXNSUM name' ) 
94060 FORMAT( 'ERROR: Maximum number of terms in RXNSUM exceeded' ) 
94080 FORMAT( '       Modify PARAMETER ( MAXRXTERMS =', I3, ' )', 
     &              ' or decrease the number of terms in RXNSUM' ) 
94100 FORMAT( 'ERROR: Expecting a reaction label starting with a <' ) 
94120 FORMAT( 'ERROR: Expecting a + - or ; after a reaction label' ) 

94500 FORMAT( '       Line No. ', I4, ':', A ) 
94520 FORMAT( '       Character found:', A ) 

      END SUBROUTINE GETRXNSUM
