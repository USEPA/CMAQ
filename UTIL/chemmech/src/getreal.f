
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
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN )    :: IMECH   ! IO unit for mechanism file
      CHARACTER*( * ), INTENT( INOUT ) :: CHR     ! current character from buffer
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF   ! string read from mechanism file
      INTEGER,         INTENT( INOUT ) :: LPOINT  ! character position in INBUF
      INTEGER,         INTENT( INOUT ) :: IEOL    ! end of line position
      REAL( 8 ),       INTENT( OUT )   :: NUMBER  ! number from file
!Local:
      LOGICAL         :: LDECIMAL, LEXP, LZERO
      CHARACTER( 17 ) :: NUMSTRING 
      INTEGER         :: START, LENGTH, NUMSIGNS
      REAL            :: LOCAL_NUMBER

      INTERFACE 
        SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
        END SUBROUTINE RDLINE
        SUBROUTINE GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         INTEGER,         INTENT( IN )    :: IMECH
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
        END SUBROUTINE GETCHAR
        SUBROUTINE GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL )
         INTEGER,         INTENT( IN )    :: IMECH
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
         CHARACTER*( * ), INTENT( INOUT ) :: LABEL
        END SUBROUTINE GETLABEL
      END INTERFACE

      START = LPOINT
      LENGTH = 0
      NUMSIGNS = 0
      LDECIMAL = .FALSE.
      LEXP = .FALSE.
      LZERO = .TRUE.
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
      IF ( CHR .NE. '0' )LZERO = .FALSE.
      IF ( CHR .GE. '0' .AND. CHR .LE. '9' ) GO TO 101
      IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' .OR.
     &     CHR .EQ. 'D' .OR. CHR .EQ. 'd' )THEN
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
      NUMSTRING = NUMSTRING( 1:LENGTH ) // 'D0'
       READ ( NUMSTRING( 1:LENGTH ), * ) NUMBER
!      READ( NUMSTRING( 1:LENGTH ), * )LOCAL_NUMBER
      IF( LZERO )THEN
         NUMBER = 0.0D+0
      END IF
      IF ( LPOINT .GT. IEOL ) THEN
           CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
           WRITE(KPPEQN_UNIT,'(A)')' '
      END IF
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
