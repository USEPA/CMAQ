
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

      SUBROUTINE GETIRROUT

C-----------------------------------------------------------------------
C Function: To process the IRR_OUTPUT option 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: GETCHR
C                                   GETWORD
C                                   GETCOEF
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI`s M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------
      USE UTILIO_DEFN
      USE PA_GLOBAL     ! Mech and Grid data used 
      USE PA_VARS
      USE PA_DEFN
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      INTEGER :: TERMNUM   ! Counter for number of terms in cycle

C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get assigned name and check for = sign delimiter
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NIRROUT = NIRROUT + 1
      IF ( NIRROUT .GT. MAXIRROUT ) THEN
         WRITE( MSG, 94000 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94020 ) MAXIRROUT
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
      END IF

      CALL GETWORD
!     IRRNAME( NIRROUT ) = WORD
!     IRRDESC( NIRROUT ) = WORD
      IRRNAME_TMP( NIRROUT ) = WORD
      IRRDESC_TMP( NIRROUT ) = WORD

      IF ( CHR .NE. '=' ) THEN
         WRITE( MSG, 94040 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
      ELSE
         CALL GETCHR
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Top of loop on terms in output command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      TERMNUM = 0
20    CONTINUE
      TERMNUM = TERMNUM + 1
      IF ( TERMNUM .GT. MAXOUTTERMS ) THEN
         WRITE( MSG, 94060 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94080 ) MAXOUTTERMS
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
      END IF      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the term`s coefficient if there is one
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GETCOEF( OUTSC( NIRROUT, TERMNUM ) )
      OUTPNFLAG( NIRROUT, TERMNUM ) = ''

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Reaction rate term
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .EQ. '<' ) THEN
         OUTTYPE( NIRROUT, TERMNUM ) = 'RXN'
         CALL GETLABEL
         OUTRXLBL( NIRROUT, TERMNUM ) = LABEL
      ELSE
         CALL GETWORD

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Production term
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( WORD .EQ. 'PROD' .OR. WORD .EQ. 'NETP' ) THEN
            OUTTYPE( NIRROUT, TERMNUM ) = WORD
            IF ( CHR .EQ. '[' ) THEN
               CALL GETLABEL
               OUTSPEC1( NIRROUT, TERMNUM ) = LABEL
            ELSE
               WRITE( MSG, 94100 ) 
               CALL M3MESG( MSG )
               WRITE( MSG, 94500 ) LINNUM, INBUF
               CALL M3MESG( MSG )
               WRITE( MSG, 94520 ) CHR
               CALL M3MESG( MSG )
               CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
            END IF

c..FROM option
            OUTSPEC2( NIRROUT, TERMNUM ) = ''
            IF ( CHR .EQ. 'F' ) THEN
               CALL GETWORD
               IF ( WORD .NE. 'FROM' ) THEN
                  WRITE( MSG, 94120 ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94500 ) LINNUM, INBUF
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94540 ) WORD
                  CALL M3MESG( MSG )
                  CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
               END IF
               IF ( CHR .EQ. '[' ) THEN
                  CALL GETLABEL
                  OUTSPEC2( NIRROUT, TERMNUM ) = LABEL
               ELSE
                  WRITE( MSG, 94100 ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94500 ) LINNUM, INBUF
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94520 ) CHR
                  CALL M3MESG( MSG )
                  CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
               END IF
            END IF

            OUTOP2( NIRROUT, TERMNUM ) = ''
            OUTSPEC3( NIRROUT, TERMNUM ) = ''

c..AND/OR operator 
            IF ( CHR .EQ. 'A' .OR. CHR .EQ. 'O' ) THEN
               CALL GETWORD
               IF ( WORD .NE. 'AND' .AND. WORD .NE. 'OR' ) THEN
                  WRITE( MSG, 94140 ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94500 ) LINNUM, INBUF
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94540 ) WORD
                  CALL M3MESG( MSG )
                  CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
               ELSE
                  OUTOP2( NIRROUT, TERMNUM ) = WORD
                  IF ( CHR .EQ. '[' ) THEN
                     CALL GETLABEL
                     OUTSPEC3( NIRROUT, TERMNUM ) = LABEL
                  ELSE
                     WRITE( MSG, 94100 ) 
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94500 ) LINNUM, INBUF
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94520 ) CHR
                     CALL M3MESG( MSG )
                     CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
                   END IF
                END IF
             END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loss Term
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ELSE IF ( WORD .EQ. 'LOSS' .OR. WORD .EQ. 'NETL' ) THEN
            OUTTYPE( NIRROUT, TERMNUM ) = WORD
            IF ( CHR .EQ. '[' ) THEN
               CALL GETLABEL
               OUTSPEC1( NIRROUT, TERMNUM ) = LABEL
            ELSE
               WRITE( MSG, 94100 ) 
               CALL M3MESG( MSG )
               WRITE( MSG, 94500 ) LINNUM, INBUF
               CALL M3MESG( MSG )
               WRITE( MSG, 94520 ) CHR
               CALL M3MESG( MSG )
               CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
            END IF

            OUTOP2(   NIRROUT, TERMNUM ) = ''
            OUTSPEC2( NIRROUT, TERMNUM ) = ''
            OUTSPEC3( NIRROUT, TERMNUM ) = ''

c..AND/OR operator 
            IF ( CHR .EQ. 'A' .OR. CHR .EQ. 'O' ) THEN
               CALL GETWORD
               IF ( WORD .NE. 'AND' .AND. WORD. NE. 'OR' ) THEN
                  WRITE( MSG, 94140 ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94500 ) LINNUM, INBUF
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94540 ) WORD
                  CALL M3MESG( MSG )
                  CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
               ELSE
                  OUTOP2( NIRROUT, TERMNUM ) = WORD
                  IF ( CHR .EQ. '[' ) THEN
                     CALL GETLABEL
                     OUTSPEC3( NIRROUT, TERMNUM ) = LABEL
                  ELSE
                     WRITE( MSG, 94100 ) 
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94500 ) LINNUM, INBUF
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94520 ) CHR
                     CALL M3MESG( MSG )
                     CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
                  END IF
               END IF
            END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Net Term
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ELSE IF ( WORD .EQ. 'NET' ) THEN
            OUTTYPE(  NIRROUT, TERMNUM ) = 'NET'
            OUTOP2(   NIRROUT, TERMNUM ) = ''
            OUTSPEC2( NIRROUT, TERMNUM ) = ''
            OUTSPEC3( NIRROUT, TERMNUM ) = ''
            IF ( CHR .EQ. '[' ) THEN
               CALL GETLABEL
               OUTSPEC1( NIRROUT, TERMNUM ) = LABEL
            ELSE
               WRITE( MSG, 94100 ) 
               CALL M3MESG( MSG )
               WRITE( MSG, 94500 ) LINNUM, INBUF
               CALL M3MESG( MSG )
               WRITE( MSG, 94520 ) CHR
               CALL M3MESG( MSG )
               CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
            END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The term must be a predefined name
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ELSE
            OUTTYPE(  NIRROUT, TERMNUM ) = 'NAME'            
            OUTSPEC1( NIRROUT, TERMNUM ) = WORD
            IF ( CHR .EQ. '[' ) THEN
               CALL GETLABEL
               IF ( LABEL .NE. 'POSONLY' .AND. LABEL .NE. 'NEGONLY' ) 
     &              THEN
                  WRITE( MSG, 94180 ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94500 ) LINNUM, INBUF
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94540 ) LABEL
                  CALL M3MESG( MSG )
                  CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
               ELSE          
                  OUTPNFLAG( NIRROUT, TERMNUM ) = LABEL
               END IF
            END IF
         END IF
      END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Go back and get the next operator, return, or err off
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
         GO TO 20
      ELSE IF ( CHR .EQ. ';' ) THEN
         NIRRTERMS( NIRROUT ) = TERMNUM
         CALL GETCHR
         RETURN
      ELSE
         WRITE( MSG, 94200 ) 
         CALL M3MESG( MSG )
         WRITE( MSG, 94500 ) LINNUM, INBUF
         CALL M3MESG( MSG )
         WRITE( MSG, 94520 ) CHR
         CALL M3MESG( MSG )
         CALL M3EXIT( 'GETIRROUT', IZERO, IZERO, ' ', XSTAT2 )
      END IF            

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of IRR_OUTPUTs exceeded' )
94020 FORMAT( '       Modify PARAMETER ( MAXIRROUT =', I3,' ) or',
     &              '  decrease the number of IRR_OUTPUT requests' )
94040 FORMAT( 'ERROR: Equal sign expected after IRR_OUTPUT name' )
94060 FORMAT( 'ERROR: Maximum number of terms in an IRR_OUTPUT exceeded' )
94080 FORMAT( '       Modify PARAMETER ( MAXOUTTERMS =', I3,' ) or', 
     &              ' decrease the number of terms in IRR_OUTPUT' )

94100 FORMAT( 'ERROR: A [ must follow PROD/LOSS/NET operators or',
     &               ' FROM/AND/OR options' )
94120 FORMAT( 'ERROR: Only the FROM option can follow the PROD/NETP',
     &               ' operators' ) 
94140 FORMAT( 'ERROR: Only the AND/OR options can follow the FROM',
     &               ' option' )
94160 FORMAT( 'ERROR: Only the AND/OR options can follow the LOSS',
     &                              ' operator' )
94180 FORMAT( 'ERROR: Only POSONLY/NEGONLY options can follow defined',
     &                              ' names' )
94200 FORMAT( 'ERROR: Expecting a +, -, or ; after an IRR_OUTPUT term' ) 

94500 FORMAT( '       Line No. ', I4, ': ', A )
94520 FORMAT( '       Character found: ', A )
94540 FORMAT( '       Word found: ', A )

      END SUBROUTINE GETIRROUT

