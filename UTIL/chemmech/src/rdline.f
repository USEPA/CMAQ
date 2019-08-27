
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
C @(#)RDLINE.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.RDLINE.F 02 Jan 1997 15:26:49

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )

C==============================================================================
C RDLINE reads one line from the mechanism file and stores it in INBUF.
C The position of the last non-blank character is stored in IEOL.
C All blank lines and lines with a '!' in the first column are skipped.
C LPOINT, the pointer to the current character being accessed (set by
C GETCHAR), is initialized to 0.
 
C input:  IMECH
C output: INBUF, LPOINT, IEOL
C updates: nothing
C==============================================================================
      USE MECHANISM_PARMS
      
      IMPLICIT NONE
!arguments:
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( IN )    :: IMECH
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
!local:
      INTEGER       ::IPOS
      LOGICAL, SAVE :: FIRSTCALL  = .TRUE.
      INTEGER, SAVE :: LINE_COUNT = 0


101   CONTINUE
      READ( IMECH, '(A)', END = 999 ) INBUF

      IF ( INBUF( 1:1 ) .NE. '!' ) THEN
C Find the last non blank character in the line and set IEOL
      LINE_COUNT = LINE_COUNT + 1
C     WRITE(6,'(A16,1X,I4,1X,A2)',ADVANCE='NO')'LINE:', LINE_COUNT,' '
C     WRITE(6,'(A)')INBUF

         IEOL = 0
         DO IPOS = 81, 1, -1
            IF ( INBUF( IPOS:IPOS ) .NE. ' ' ) THEN
               IEOL = IPOS
               GO TO 301
            END IF
         END DO
301      CONTINUE
C End of finding last non-blank character 

         IF ( IEOL .EQ. 0 ) THEN ! skip a blank line
            GO TO 101
         ELSE
            LPOINT = 0
            RETURN
         END IF
      ELSE
         GO TO 101
      END IF

999   CONTINUE
C eof encountered
      WRITE( *,2001 ) IMECH, INBUF
!001  FORMAT( / 5X, '*** RDLINE ERROR: End of file read on unit:', I3
2001  FORMAT( / 5X, 'From RDLINE: End of file read on unit:', I3
     &        / 5X, 'Last line read:' / A81 )
!     STOP
      IEOL = -999
      END_OF_IMECH = .TRUE.
      RETURN
      END
