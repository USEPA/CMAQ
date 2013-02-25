
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

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)GETREAL.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETREAL.F 02 Jan 1997 15:26:46

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )

C=======================================================================
C converts a number from character to floating point representation
C
C input: IMECH (for RDLINE), INBUF, LPOINT, IEOL, CHR
C output: NUMBER
C updates: INBUF, LPOINT, IEOL, CHR
C=======================================================================
      IMPLICIT NONE
      INCLUDE 'PARMS.e'
      CHARACTER(  1 ) :: CHR
      CHARACTER( 81 ) :: INBUF
      INTEGER IMECH, LPOINT, IEOL
      REAL( 8 ) :: NUMBER
      LOGICAL LDECIMAL, LEXP
      CHARACTER( 15 ) :: NUMSTRING 
      INTEGER START, LENGTH, NUMSIGNS

      START = LPOINT
      LENGTH = 0
      NUMSIGNS = 0
      LDECIMAL = .FALSE.
      LEXP = .FALSE.
101   CONTINUE
      IF ( LENGTH .NE. 0 ) THEN
         LPOINT = LPOINT + 1
         IF ( LPOINT .GT. IEOL ) THEN
            CHR = ' '
         ELSE
            CHR = INBUF( LPOINT:LPOINT )
         END IF
      END IF
      LENGTH = LENGTH + 1
      IF ( CHR .EQ. '.' ) THEN
         IF ( .NOT. LDECIMAL ) THEN
            LDECIMAL = .TRUE.
            GO TO 101
         ELSE
            WRITE( *,2001 ) INBUF
            STOP
         END IF        
      END IF   
      IF ( CHR .GE. '0' .AND. CHR .LE. '9' ) GO TO 101
      IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' ) THEN
         IF ( .NOT. LEXP ) THEN
            LEXP = .TRUE.
            GO TO 101
         ELSE
            WRITE( *,2003 ) INBUF
            STOP
         END IF
      END IF      
      IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
         NUMSIGNS = NUMSIGNS + 1
         IF ( NUMSIGNS .LE. 2 ) THEN
            GO TO 101
         ELSE
            WRITE( *,2005 ) INBUF
            STOP
         END IF
      END IF

c end of the numeric string

      NUMSTRING = ' '
      NUMSTRING = INBUF( START:LPOINT-1 )
      LENGTH = LENGTH - 1
      IF ( ( .NOT. LEXP ) .AND. ( .NOT. LDECIMAL ) ) THEN  ! force it to be real
         NUMSTRING = NUMSTRING( 1:LENGTH ) // '.'
         LENGTH = LENGTH + 1
      END IF
      READ ( NUMSTRING( 1:LENGTH ),'(D15.4)' ) NUMBER
      IF ( LPOINT .GT. IEOL ) CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
      IF ( CHR .EQ. ' ' ) CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )

      RETURN
C=======================================================================
2001  FORMAT( / 5X, '*** ERROR: ',
     &              'Only one decimal point allowed in a number'
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: ',
     &              'More than one E or e in the field'
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: ',
     &              'More than one sign in the exponent field:'
     &        / 5X, 'Last line read was:' / A81 )
        END
