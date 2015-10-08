
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
C $Header: /project/work/rep/MECH/src/driver/mech/WRBF6.f,v 1.3 2001/03/05 19:48:55 yoj Exp $ 

c You can't put zeros in column 5 as a continuation character on Sun's

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WRBF6.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WRBF6.F 02 Jan 1997 15:26:55

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF6_FORTRAN90 ( WRUNIT, AWPL, NEL, IVAR )
C write to a 6-character buffer, format at no more than 10 elements per line
C (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
      INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
      INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
      INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
      INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write

      INTEGER IRX, IRX0, IRX1, IRX2, IOS, CNN
      INTEGER             ::  WPL       ! words per line (max at 10)
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER( 4 )      :: CONCHAR
      CHARACTER( 6 )      :: BUFF6( MAXRXNUM )
 
      WPL = MIN ( AWPL, 10 )

      
      DO IRX = 1, NEL-1
         WRITE( BUFF6( IRX ), 1001, IOSTAT = IOS ) IVAR( IRX )
1001        FORMAT(I5, ",")
            IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2001 ) IRX, IVAR( IRX ), IOS
2001        FORMAT(/ 5X, '1) *** Error writing to internal buffer in WRBF6 ***'
     &             / 5X, 'Attempted to write element', I6,
     &               1X, 'and data:', I8
     &             / 5X, 'IOSTATUS = ', I6 )
            STOP
         END IF
      END DO
      WRITE( BUFF6( NEL ), 1002, IOSTAT = IOS ) IVAR( NEL )
1002        FORMAT(I5, "/")
      IF ( IOS .NE. 0 ) THEN
         WRITE( LOGDEV,2001 ) NEL, IVAR( NEL ), IOS
         STOP
      END IF


      IF ( NEL / WPL .GE. 6 ) THEN
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            CNN = MOD ( IRX0-1, 10 )
            IF( IRX2 .EQ. NEL )EXIT
            IF ( CNN .NE. 0 ) THEN
               WRITE( CONCHAR, '(I1)' ) CNN
            ELSE
               CONCHAR = 'O'
            END IF
            WRITE( WRUNIT, 1003, IOSTAT = IOS )( BUFF6( IRX ), IRX = IRX1, IRX2 ),
     &                                          CONCHAR
1003        FORMAT( 5X, '&', 2X, 10A6, ' & ! ', A )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2003 ) WRUNIT, IRX, BUFF6( IRX ), IOS
2003           FORMAT( /5X, '2) *** Error writing to external unit', I3,
     &                  1X, 'in WRBF6_FORTRAN90 ***'
     &                 /5X, 'Attempted to write buffer index', I3,
     &                  1X, 'with data:', A6
     &                 /5X, 'IOSTATUS = ', I6 )
               STOP
            END IF
            IRX1 = IRX2 + 1
         END DO
         IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
            CNN = MOD ( IRX0-1, 10 )
            IF ( CNN .NE. 0 ) THEN
               WRITE( CONCHAR, '(I1)' ) CNN
            ELSE
               CONCHAR = 'O'
            END IF
            WRITE( WRUNIT, 1005, IOSTAT = IOS ) ( BUFF6( IRX ), IRX = IRX1, NEL ),
     &                                           ACHAR(IACHAR('!')), CONCHAR

1005        FORMAT( 5X, '&', 2X, 10A6, A, A )
            IF ( IOS .NE. 0) THEN
               WRITE( LOGDEV,2004 ) WRUNIT, IRX, BUFF6( IRX ), IOS
2004           FORMAT( /5X, '3) *** Error writing to external unit', I3,
     &                  1X, 'in WRBF6_FORTRAN90 ***'
     &                 /5X, 'Attempted to write buffer index', I3,
     &                  1X, 'with data:', A6
     &                 /5X, 'IOSTATUS = ', I6 )
               STOP
            END IF
         END IF

      ELSE
      

         CONCHAR =  ' '
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            IF( IRX2 .EQ. NEL )EXIT
            WRITE( WRUNIT, 1004, IOSTAT = IOS ) ( BUFF6( IRX ), IRX = IRX1, IRX2 )
1004        FORMAT( 5X, '&', 2X, 10A6, ' & ')
            IF ( IOS .NE. 0 ) THEN
2005           FORMAT( /5X, '4) *** Error writing to external unit', I3,
     &                  1X, 'in WRBF6_FORTRAN90 ***'
     &                 /5X, 'Attempted to write buffer index', I3,
     &                  1X, 'with data:', A6
     &                 /5X, 'IOSTATUS = ', I6 )
               STOP
            END IF
            IRX1 = IRX2 + 1
         END DO
         IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
            WRITE( WRUNIT, 1007, IOSTAT = IOS )( BUFF6( IRX ), IRX = IRX1, NEL )
1007        FORMAT( 5X, '&', 2X, 10A6)
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2006 ) WRUNIT, IRX, BUFF6( IRX ), IOS
2006           FORMAT( /5X, '5) *** Error writing to external unit', I3,
     &                  1X, 'in WRBF6_FORTRAN90 ***'
     &                 /5X, 'Attempted to write buffer index', I3,
     &                  1X, 'with data:', A6
     &                 /5X, 'IOSTATUS = ', I6 )
               STOP
            END IF
         END IF

      END IF

      RETURN
      END
