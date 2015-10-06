
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
C $Header: /project/work/rep/MECH/src/driver/mech/WRBF16C.f,v 1.3 2001/03/05 19:48:55 yoj Exp $ 

C You can't put zeros in column 5 as a continuation character on Sun's

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WRBF16C.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WRBF16C.F 02 Jan 1997 15:26:54

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF16C ( WRUNIT, AWPL, NEL, VAR )
C write a 16-character variable to a 20-character buffer, format at no more
C than 3 elements per line (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
      INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
      INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
      INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
      CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
!local:      
      INTEGER            :: IRX, LINE, IRX1, IRX2, IRXF, IOS, CNN, WPL
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER(  1 )    :: CONCHAR
      CHARACTER( 20 )    :: BUFF20( NEL )
 
C----------------------------------------------------------------------

      WPL = MIN( AWPL, 3 )

      DO IRX = 1, NEL-1
         WRITE( BUFF20( IRX ), 1001, IOSTAT = IOS ) VAR( IRX )
1001     FORMAT( 1X, "'", A16, "'," )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV, 2003 ) IRX, VAR( IRX ), IOS
2003        FORMAT( /5X, '*** Error writing to internal buffer in WRBF16 ***'
     &              /5X, 'Attempted to write element', I6,
     &               1X, 'and data:', 1PE11.4
     &              /5X, 'IOSTATUS = ', I6 )
            STOP
            END IF
      END DO
      WRITE( BUFF20( NEL ), 1003, IOSTAT = IOS ) VAR( NEL )
1003  FORMAT( 1X, "'", A16, "'/" )
      IF ( IOS .NE. 0 ) THEN
         WRITE( LOGDEV,2003 ) NEL, VAR( NEL ), IOS
         STOP
      END IF

      IRX1 = 1
      IRXF = 0
      DO LINE = 1, NEL / WPL
         IRX2 = IRX1 + WPL - 1
!        IF ( MOD( LINE, 2 ) .NE. 0 ) THEN       ! every other line
            CNN = MOD( IRXF, 10 )
            IF ( CNN .NE. 0 ) THEN
               WRITE( CONCHAR, '(I1)' ) CNN
            ELSE
               CONCHAR = 'O'
            END IF
            IRXF = IRXF + 1
!           ELSE
!           CONCHAR = '+'
!        END IF
         WRITE( WRUNIT, 1005, IOSTAT = IOS ) CONCHAR,
     &                                      ( BUFF20( IRX ), IRX = IRX1, IRX2 )
1005     FORMAT( 5X, A, 3X, 3A20 )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2005) WRUNIT, IRX, BUFF20( IRX ), IOS
2005        FORMAT( /5X, '*** Error writing to external unit', I3, 
     &               1X, 'in WRBF16 ***'
     &              /5X, 'Attempted to write buffer index', I3,
     &               1X, 'with data:', A16
     &              /5X, 'IOSTATUS = ', I6)
            STOP
         END IF
         IRX1 = IRX2 + 1
      END DO
      IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
         IF ( MOD( IRX1, 2 ) .NE. 0 ) THEN
            CNN = MOD( IRXF, 10 )
            IF ( CNN .NE. 0 ) THEN
               WRITE( CONCHAR, '(I1)' ) CNN
            ELSE
               CONCHAR = 'O'
            END IF
         ELSE
            CONCHAR = '+'
         END IF
         WRITE( WRUNIT, 1005, IOSTAT = IOS ) CONCHAR,
     &                                      ( BUFF20( IRX ), IRX = IRX1, NEL )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF20( IRX ), IOS
            STOP
         END IF

      END IF

      RETURN
      END
