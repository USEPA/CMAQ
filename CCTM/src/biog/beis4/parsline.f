
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
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/parsline.f,v 1.3 2011/10/21 16:10:18 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        SUBROUTINE PARSLINE( LINE, N, SEGMENT )

C-----------------------------------------------------------------------
C  Description:
C    Separates a "list-formatted" line of strings in which
C    the segments may or may not have quotes.  Although fortran requires
C    the quotes for true list-formatting, this subroutine can be used when
C    the quotes are only present to enclose a character (such as space, comma,
C    or semi-colon) that would otherwise be a delimiter.  If an "!" is 
C    encountered, everything after it is treated as a comment.
 
C  Preconditions:
 
C  Subroutines and Functions Called:
 
C  Revision History:
C    Created by M. Houyoux 3/99
C    02/11: S.Roselle-Replaced I/O API include files with UTILIO_DEFN
C    01/14: D.Wong-Reduced NDELIM to 3 and use only 3 delimiters (comma, blank
C           space and semicolon) since it only deals with one character when calling
C           FINDC. DELIMLST is now standard compliant (gfortran happy)
C    02/19: D.Wong-removed some unnecessary calculation
 
C-----------------------------------------------------------------------
C Modified from:

C Project Title: EDSS Tools Library
C File: @(#)$Id: parsline.f,v 1.3 2011/10/21 16:10:18 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/parsline.f,v $
C Last updated: $Date: 2011/10/21 16:10:18 $ 
C-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE

C Arguments:
      CHARACTER( * ), INTENT ( IN ) :: LINE         ! character string to parse
      INTEGER,        INTENT ( IN ) :: N            ! no. of segments to parse ("fields")
      CHARACTER( * ), INTENT( OUT ) :: SEGMENT( N ) ! parsed string

C External Functions:

C Local parameters:
      INTEGER,   PARAMETER :: NDELIM = 3
!     CHARACTER, PARAMETER :: DELIMLST( NDELIM ) = (/ ',', ' ', ';' /)
      CHARACTER, PARAMETER :: DELIMLST( NDELIM ) = (/ ' ', ',', ';' /)

C Arrays for sorting non-delimiters on a per-machine basis:
!     INTEGER            NDINDX  ( NDELIM )
!     CHARACTER, SAVE :: DELIMSRT( NDELIM )

C Other local variables:
      INTEGER          I, J, L, L1, L2    ! counters and indices
      INTEGER          IXP                ! index to non-delimiters
      INTEGER          NCNT               ! count of fields

      LOGICAL          ALPHA              ! true when within alpha-numeric 
      LOGICAL          DELIM              ! true when within or past delimiter 
      LOGICAL       :: PREVDELIM = .TRUE. ! true when last char was a delim
      LOGICAL          NUMBER             ! true when within number in string 
      LOGICAL          QUOTED             ! true when within quotes in string
      LOGICAL          ISANMBR            ! true if character is a numeral
      LOGICAL, SAVE :: FIRSTIME = .TRUE.  ! true first time routine is called

      CHARACTER        CBUF               ! test buffer
      CHARACTER     :: DOUBLEQ = '"'
      CHARACTER     :: SINGLEQ = "'"
      CHARACTER     :: PERIOD  = '.'
      CHARACTER     :: SPACE   = ' '
      CHARACTER        QUOTVAL            ! value of starting quote

      CHARACTER( 300 ) :: MESG            ! message buffer
      CHARACTER(  16 ) :: PNAME = 'PARSLINE' ! procedure name

C-----------------------------------------------------------------------

C The first time the routine is called, sort the list of delimiters
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
!        DO I = 1, NDELIM 
!           NDINDX( I ) = I
!        END DO

!        print *, ' ==d== insert a ', DELIMLST
!        CALL SORTIC( NDELIM, NDINDX, DELIMLST )
!        print *, ' ==d== insert b ', DELIMLST

!        DO I = 1, NDELIM 
!           J = NDINDX( I )
!           DELIMSRT( I ) = DELIMLST( J )
!        END DO

!        print *, ' ==d== insert c ', DELIMSRT

!        DELIMSRT = DELIMLST

      END IF

      L2 = LEN_TRIM( LINE )

C Check for comments, and use to set the end of the line
      L = INDEX( LINE( 1:L2 ), '!' )

      IF ( L .LE. 0 ) THEN
         L = L2
      ELSE
         L = L - 1
      END IF

C Skip blank lines
      IF ( L .EQ. 0 ) RETURN

C Initialize count, flags, and segments (note, initializing in the variable
C definitions is insufficient)
      NCNT    = 0
      SEGMENT = ' ' ! array
      ALPHA   = .FALSE.
      DELIM   = .TRUE.
      NUMBER  = .FALSE.
      QUOTED  = .FALSE.

C Process LINE one character at a time
      DO I = 1, L

         CBUF = LINE( I:I )

C Look for character in delimiters
         IXP = FINDC( CBUF, NDELIM, DELIMLST )

C Evaluate the current character for number or not
         ISANMBR = ( CBUF .GE. '0' .AND. CBUF .LE. '9' )

C Waiting for next field...
         IF ( DELIM ) THEN

            NUMBER = ISANMBR
            ALPHA  = ( .NOT. NUMBER .AND. IXP .LE. 0 )

            IF ( CBUF .EQ. SINGLEQ ) THEN
               QUOTED  = .TRUE.
               DELIM   = .FALSE.
               QUOTVAL = SINGLEQ
               PREVDELIM = .FALSE.
               L1      = I + 1
               NCNT    = NCNT + 1

            ELSE IF ( CBUF .EQ. DOUBLEQ ) THEN
               QUOTED  = .TRUE.
               DELIM   = .FALSE.
               QUOTVAL = DOUBLEQ
               PREVDELIM = .FALSE.
               L1      = I + 1
               NCNT    = NCNT + 1

            ELSE IF ( ALPHA .OR. NUMBER ) THEN
               DELIM = .FALSE.
               PREVDELIM = .FALSE.
               L1    = I
               NCNT  = NCNT + 1

C If another delimeter, then another field, but last field was blank
C UNLESS delim is a space
            ELSE IF ( CBUF .NE. SPACE ) THEN
               IF ( PREVDELIM ) THEN
                  NCNT = NCNT + 1
               ELSE
                  PREVDELIM = .TRUE.
               END IF
            END IF  ! Else its a space delimiter

C In a quoted field, skip everything unless it is an end quote
         ELSE IF ( QUOTED ) THEN
            IF ( CBUF .EQ. QUOTVAL ) THEN
               QUOTED  = .FALSE.
               DELIM   = .TRUE.
               PREVDELIM = .FALSE.
               L2      = I - 1
               CALL STORE_SEGMENT( NCNT, N, L1, L2, LINE, SEGMENT( NCNT ) )
            END IF

C If start of field was a number, but adjacent character is not a delimiter,
C then turn field into an alpha
         ELSE IF ( NUMBER .AND. .NOT. ISANMBR .AND. IXP .LE. 0 ) THEN
            ALPHA  = .TRUE.
            NUMBER = .FALSE.

C If start of field was a number or alpha, and this is a delimiter, then end
C of number has been reached
         ELSE IF ( IXP .GT. 0 ) THEN
            ALPHA = .FALSE.
            NUMBER = .FALSE.
            DELIM  = .TRUE.
            PREVDELIM = .TRUE.
            L2     = I - 1
            CALL STORE_SEGMENT( NCNT, N, L1, L2, LINE, SEGMENT( NCNT ) )
         END IF

      END DO

C Store final segment
      IF ( CBUF .EQ. QUOTVAL ) L = L - 1
      L2 = L

      IF ( IXP .LE. 0 ) THEN
         CALL STORE_SEGMENT( NCNT, N, L1, L2, LINE, SEGMENT( NCNT ) )
      END IF

      RETURN

C-------------------- Format Statements --------------------------------

94010 FORMAT( 10( A, :, I8, :, 1X ) )

C------------------- Internal Subprogram -------------------------------

      CONTAINS

C Store the segment from the input string
         SUBROUTINE STORE_SEGMENT( NSEG, MXSEG, I1, I2, STRNG, SEGMNT )

         IMPLICIT NONE

         INTEGER,        INTENT(  IN ) :: NSEG, MXSEG, I1, I2
         CHARACTER( * ), INTENT(  IN ) :: STRNG         ! character string to parse
         CHARACTER( * ), INTENT( OUT ) :: SEGMNT        ! parsed string

         CHARACTER( 128 ) :: MESG = ' '
         CHARACTER(  16 ) :: PNAME = 'PARSLINE_STORE_S' ! procedure name

         IF ( NSEG .LE. MXSEG ) THEN

            SEGMNT = ADJUSTL( STRNG( I1:I2 ) )

         ELSE

            MESG = 'ERROR: Overflow prevented while ' //
     &             'parsing line ' // PNAME
            CALL M3MSG2( MESG )
            MESG = 'First 200 characters of line contents are:'
            CALL M3MSG2( MESG )
            MESG = STRNG( 1:200 )
            CALL M3MSG2( MESG )

            MESG = 'Formatting problem'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

         END IF

         END SUBROUTINE STORE_SEGMENT

      END SUBROUTINE PARSLINE
