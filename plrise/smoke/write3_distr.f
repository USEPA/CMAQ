
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/plrise/smoke/write3_distr.f,v 1.1 2010/07/20 11:30:03 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION WRITE3_DISTR ( FNAME, VNAME, JDATE, JTIME, DIM1, DIM2, DATA )
     &                        RESULT ( SUCCESS )

C-----------------------------------------------------------------------
C Function: Use I/O-API's WRITE3 and avoid using PWRITE3 to write layer
C           fractions to processor unique fake-gridded files

C Note: This function must live in a file that has a ".f" extension!

C Revision History:
C     5 Dec 2007 J.Young: initial implementation
C-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER( * ), INTENT( IN ) :: FNAME
      CHARACTER( * ), INTENT( IN ) :: VNAME
      INTEGER,        INTENT( IN ) :: JDATE, JTIME
      INTEGER,        INTENT( IN ) :: DIM1, DIM2
      REAL,           INTENT( IN ) :: DATA( DIM1,DIM2 )
      LOGICAL SUCCESS

      LOGICAL, EXTERNAL :: WRITE3
      INTEGER, EXTERNAL :: SETUP_LOGDEV

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER LOGDEV
      CHARACTER( 96 ) :: XMSG = ' '

C-----------------------------------------------------------------------

      SUCCESS = .TRUE.

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = SETUP_LOGDEV()
      END If

      IF ( .NOT. WRITE3( FNAME, VNAME, JDATE, JTIME, DATA ) ) THEN
         XMSG = 'Error writing ' // 'to file "' // TRIM( FNAME ) // '."'
         WRITE( LOGDEV,* ) XMSG
         SUCCESS = .FALSE.
      END IF

      RETURN
      END FUNCTION WRITE3_DISTR
