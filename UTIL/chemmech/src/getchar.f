
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
C @(#)GETCHAR.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETCHAR.F 02 Jan 1997 15:26:42

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )

C==============================================================================
C GETCHAR returns the next character excluding white space and comments and
C sets LPOINT, the pointer to the current character being accessed.
C input: IMECH (for RDLINE)
C output: CHR
C updates: INBUF, LPOINT, IEOL
C precondition:  RDLINE must have been called
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE

      CHARACTER*( * ), INTENT( INOUT ) :: CHR
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( IN )    :: IMECH
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT

      INTERFACE 
        SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
        END SUBROUTINE RDLINE
        SUBROUTINE EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
        END SUBROUTINE EATCOM
      END INTERFACE

101   CONTINUE
      LPOINT = LPOINT + 1
      IF ( LPOINT .GT. IEOL ) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         GO TO 101
      ELSE
         CHR = INBUF( LPOINT:LPOINT )
         IF ( CHR .EQ. ' ' .OR.
     &      ICHAR( CHR ) .EQ. 09 ) THEN   ! (HT = horizontal tab)
            GO TO 101
         ELSE IF ( CHR .EQ. '(' .OR. CHR .EQ. '{' ) THEN
            CALL EATCOM ( IMECH, INBUF, LPOINT, IEOL, CHR )
            GO TO 101            
         END IF
      END IF
      RETURN
      END
