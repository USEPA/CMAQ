
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
C $Header: /project/work/rep/MECH/src/driver/mech/WRHDR1.f,v 1.2 1998/06/19 11:31:11 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WRHDR1.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WRHDR1.F 02 Jan 1997 15:26:58

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WRHDR1 ( EXUNIT, NAMESTR, FMT_LEN )

      IMPLICIT NONE

      INTEGER EXUNIT
      CHARACTER( * ) :: NAMESTR
      INTEGER FMT_LEN
       
      INTEGER, EXTERNAL :: SIZE
      INTEGER LX, LS, LQ, IX
                                 
C----------------------------------------------------------------------

      LS = SIZE ( 'leading', 'no_trailing', NAMESTR )
      LX = FMT_LEN - 3
      IF ( LS .LE. LX ) THEN
         WRITE( EXUNIT, 1001 ) NAMESTR( 1:LS )
1001     FORMAT( '#', 2X, A )
      ELSE
C Try for wrap-around: find a "natural" break, e.g. a '/'
C Limit wrap-around cut off to 50% of the original length ...
         LQ = IFIX ( 0.5 * FLOAT( LS ) )
         DO IX = LX, LQ, -1
            IF ( NAMESTR(IX:IX) .EQ. '/' ) GO TO 103
         END DO
C Some name string too long - write fallback
         IX = FMT_LEN + 1
103      CONTINUE
         LX = IX - 1
         WRITE( EXUNIT, 1001 ) NAMESTR( 1:LX )
         WRITE( EXUNIT, 1001 ) NAMESTR( LX+1:LS )
      END IF

      RETURN
      END
