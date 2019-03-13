
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
C @(#)EATCOM.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.EATCOM.F 02 Jan 1997 15:26:42

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )

C==============================================================================
C Reads past all characters in comments - matches 1st and 2nd delimiters,
C and ignores anything in bewteen.
C     Called by: GETCHAR, GETWORD
C     Subroutines called: RDLINE
C input: IMECH (for RDLINE), LPOINT, IEOL
C output: nothing
C updates: INBUF, LPOINT, IEOL, CHR
C precondition: on entry CHR must be either '(' or '{'
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

       CHARACTER*( * ), INTENT( INOUT ) :: CHR
       CHARACTER*( * ), INTENT( INOUT ) :: INBUF
       INTEGER,         INTENT( IN )    :: IMECH
       INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT


       CHARACTER( 1 ) :: DELIM2
      INTERFACE 
        SUBROUTINE GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         INTEGER,         INTENT( IN )    :: IMECH
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
        END SUBROUTINE GETCHAR
        SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
        END SUBROUTINE RDLINE
      END INTERFACE

      IF ( CHR .EQ. '(' ) THEN
         DELIM2 = ')'
      ELSE IF ( CHR .EQ. '{' ) THEN
         DELIM2 = '}'
      ELSE
         WRITE( *,2001 ) CHR, INBUF
         STOP
      END IF
101   CONTINUE
      LPOINT = LPOINT + 1
      IF ( LPOINT .GT. IEOL) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         GO TO 101
      ELSE
         CHR = INBUF( LPOINT:LPOINT )
         IF ( CHR .NE. DELIM2 ) GO TO 101
      END IF
      RETURN
2001  FORMAT( / 5X, '*** ERROR processing comment: ',
     &              'Incorrect second delimiter, ', A,
     &          1X, 'following first delimiter'
     &        / 5X, 'Last line read was:' / A81 )
      END
