
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
C @(#)JUNIT.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.JUNIT.F 02 Jan 1997 15:26:48

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION JUNIT ()

C JUNIT uses an internal INQUIRE call to determine if a unit number is
C currently attached to a file.
C In the call INQUIRE(UNIT = IUN, NAMED = NMD), NMD is .TRUE. if unit (IUN)
C is connected to a file with a name, else it is .FALSE.

      IMPLICIT NONE

      INTEGER, PARAMETER :: LUNOUT = 6
      INTEGER, PARAMETER :: MAXUN = 99
      INTEGER, SAVE :: IUN
      LOGICAL NMD
      LOGICAL, SAVE :: AVAIL( MAXUN ) = .TRUE.   ! array
      INTEGER, SAVE :: IGO = 1

      GO TO ( 10001, 20001 ) IGO
10001 CONTINUE

C set unit numbers that are not available for I/O
      AVAIL( 1 ) = .FALSE.
      AVAIL( 5 ) = .FALSE.
      AVAIL( 6 ) = .FALSE.

C begin loop on unit numbers
      IUN = 10
101   CONTINUE
      IF ( IUN .GT. MAXUN ) GO TO 301
      INQUIRE ( UNIT = IUN, NAMED = NMD )
      IF ( NMD ) THEN
         AVAIL( IUN ) = .FALSE.
         GO TO 201
      ELSE IF ( .NOT. AVAIL( IUN ) ) THEN
         GO TO 201
      END IF
      JUNIT = IUN
      AVAIL( IUN ) = .FALSE.

      IGO = 2
      RETURN
20001 CONTINUE

201   CONTINUE
      IUN = IUN + 1
      GO TO 101

301   CONTINUE
      WRITE( LUNOUT,2001 )
2001  FORMAT( / 1X, '*** ERROR ABORT in JUNIT ***' /
     &       1X, 'No more unit numbers available for I/O' )
      WRITE( LUNOUT,2003 ) ( IUN, AVAIL( IUN ), IUN = 1, MAXUN )
2003  FORMAT( 1X, 'Available unit numbers are: ' /
     &       3( 1X, 20( I2, '-', L1, 2X) / ) )
      STOP
      END
