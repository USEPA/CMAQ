
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C @(#)GETWORD.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETWORD.F 02 Jan 1997 15:26:47

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)GETWORD.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETWORD.F 02 Jan 1997 15:26:47

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )

C==============================================================================
C Reads a word; only the first MAXWRDLEN characters are significant.
C The word may consist only of alphanumeric characters, ':' and '_', and the
C first character must be alphabetic. Since there is no defined terminator
C for a word, the word cannot span two lines, but may have embedded comments.
C The word buffer is terminated by any NON-valid character not on a new line.
C input: IMECH (for RDLINE), INBUF, LPOINT, CHR
C output: WORD
C updates: INBUF, LPOINT, IEOL, CHR
C precondition:  RDLINE must have been called
C calls: VALWRDCHR
C==============================================================================
      IMPLICIT NONE
      INCLUDE 'PARMS.e'
      CHARACTER(  1 ) :: CHR
      CHARACTER( 81 ) :: INBUF, WRDBUF
      INTEGER IMECH, IEOL, LPOINT
      CHARACTER( 16 ) :: WORD
      INTEGER START, LENWRD
      LOGICAL VALWRDCHR

      LENWRD = 0
      START = LPOINT
      WRDBUF = INBUF
C check for valid starting character
      IF ( ( CHR .LT. 'A' .OR. CHR .GT. 'Z' ) .AND. 
     &     ( CHR .LT. 'a' .OR. CHR .GT. 'z' ) ) THEN
         WRITE( *,2001 ) INBUF, CHR
         STOP
      END IF
101   CONTINUE
      LENWRD = LENWRD + 1
      LPOINT = LPOINT + 1
      IF ( LPOINT .GT. IEOL ) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
C check for valid character in new line NOT part of this word
         IF ( VALWRDCHR( CHR ) ) THEN
            WRITE( *,2003 ) WRDBUF
            STOP
         ELSE
            GO TO 201    ! finished filling word buffer
         END IF
      END IF
C keep going to IEOL, ';', ',', or ' ' as word terminator or invalid
C character (as kludge terminator!)
      CHR = WRDBUF( LPOINT:LPOINT )
      IF ( VALWRDCHR( CHR ) ) THEN
         GO TO 101
      ELSE
         IF ( CHR .NE. ' ' .AND.
     &        CHR .NE. ',' .AND.
     &        CHR .NE. '[' .AND.
     &        CHR .NE. ']' .AND.
     &        CHR .NE. ';' ) THEN
            WRITE( *,2005 ) WRDBUF, CHR
            STOP ' *** Invalid character in GETWORD ***'
         END IF
      END IF
      IF ( CHR .EQ. '(' .OR. CHR .EQ. '{' ) THEN 
         LENWRD = LENWRD - 1
         CALL EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )
         GO TO 101
      END IF
201   CONTINUE
      IF ( LENWRD .GT. MAXWRDLEN ) LENWRD = MAXWRDLEN
      WORD = WRDBUF( START:START+LENWRD-1 )
      IF ( CHR .EQ. ' ' ) CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      RETURN
2001  FORMAT( / 5X, '*** GETWORD ERROR: First character of a word expected',
     &              ' to be alphabetic'
     &        /     'Line buffer contents:' / A81
     &        /     'Character:', 2X, A1 )
2003  FORMAT( / 5X, '*** GETWORD ERROR: Word cannot span two lines:'
     &        /     'First line buffer contents:'/ A81 )
!005  FORMAT( / 5X, '*** GETWORD ERROR: Invalid character in a word'
2005  FORMAT( / 5X, '!!! GETWORD WARNING: Invalid character in a word'
     &        /     'Line buffer contents:' / A81
     &        /     'Character:', 2X, A1 / )
      END
