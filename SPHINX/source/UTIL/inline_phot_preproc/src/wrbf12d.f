
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
C $Header: /project/work/rep/MECH/src/driver/mech/WRBF12D.f,v 1.3 2001/03/05 19:50:14 yoj Exp $ 

C You can't put zeros in column 5 as a continuation character on Sun's

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WRBF12D.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WRBF12D.F 02 Jan 1997 15:26:53

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF12D ( WRUNIT, AWPL, NEL, VAR, AFMT )
C write an 8-byte variable to a 12-character buffer, format at no more than
C 5 elements per line (due to FORMAT restrictions), then dump to file

      IMPLICIT NONE
!      INCLUDE 'PARMS.e'
 
      INTEGER WRUNIT     ! logical write unit no.
      INTEGER AWPL, WPL  ! words per line (max at 5)
      INTEGER NEL        ! number of list elements
      REAL( 8 ) :: VAR( NEL )   ! real variable to write
      CHARACTER(  1 ) :: AFMT   ! write format: D -> 1PD11.4, E -> 1PE11.4, F -> F11.5
      CHARACTER( 14 ) :: FMT1, FMT2
      CHARACTER( 14 ) :: DFMT1 = '(1PD11.4, '','')'
      CHARACTER( 14 ) :: DFMT2 = '(1PD11.4, ''/'')'
      CHARACTER( 14 ) :: EFMT1 = '(1PE11.4, '','')'
      CHARACTER( 14 ) :: EFMT2 = '(1PE11.4, ''/'')'
      CHARACTER( 14 ) :: FFMT1 = '(F11.5, '','')  '
      CHARACTER( 14 ) :: FFMT2 = '(F11.5, ''/'')  '
      INTEGER IRX, IRX0, IRX1, IRX2, IRXF, IOS, CNN
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER(  1 ) :: CONCHAR
      CHARACTER( 12 ) :: BUFF12( NEL )
 
C----------------------------------------------------------------------

      WPL = MIN( AWPL, 5 )
      IF ( AFMT .EQ. 'D' ) THEN
         FMT1 = DFMT1
         FMT2 = DFMT2
      ELSE IF ( AFMT .EQ. 'E' ) THEN
         FMT1 = EFMT1
         FMT2 = EFMT2
      ELSE IF ( AFMT .EQ. 'F' ) THEN
         FMT1 = FFMT1
         FMT2 = FFMT2
      ELSE
         WRITE( LOGDEV,2001 ) AFMT
2001     FORMAT(/ 5X, '*** Error setting write format in WRBF12 ***'
     &          / 5X, 'Attempted argument value', A )
         STOP
      END IF

      DO IRX = 1, NEL-1
         WRITE( BUFF12( IRX ), FMT1, IOSTAT = IOS ) VAR( IRX )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2003 ) IRX, VAR( IRX ), IOS
2003        FORMAT( /5X, '*** Error writing to internal buffer in WRBF12 ***'
     &              /5X, 'Attempted to write element', I6,
     &               1X, 'and data:', 1PE11.4
     &              /5X, 'IOSTATUS = ', I6)
            STOP
         END IF
      END DO
      WRITE( BUFF12( NEL ), FMT2, IOSTAT = IOS ) VAR( NEL )
      IF ( IOS .NE. 0 ) THEN
         WRITE( LOGDEV,2003 ) NEL, VAR( NEL ), IOS
         STOP
      END IF

!     IF ( AFMT .EQ. 'D' ) THEN
!        DO IRX = 1, NEL
!           BUFF12( IRX )( 8:8 ) = 'D'
!        END DO
!     END IF

      IF ( NEL / WPL .GE. 6 ) THEN 
         IRX1 = 1
         IRXF = 0
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            IF ( MOD( IRX0, 2 ) .NE. 0 ) THEN
               CNN = MOD( IRXF, 10 )
               IF ( CNN .NE. 0 ) THEN
                  WRITE( CONCHAR, '(I1)' ) CNN
               ELSE
                  CONCHAR = 'O'
               END IF
               IRXF = IRXF + 1
            ELSE
               CONCHAR = '+'
            END IF
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF12( IRX ), IRX = IRX1, IRX2 )
1001        FORMAT( 5X, A, 4X, 5A12 )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
2005           FORMAT( /5X, '*** Error writing to external unit', I3, 
     &                  1X, 'in WRBF12 ***'
     &                 /5X, 'Attempted to write buffer index', I3,
     &                  1X, 'with data:', A12
     &                 /5X, 'IOSTATUS = ', I6)
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
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF12( IRX ), IRX = IRX1, NEL )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
         END IF

      ELSE

         CONCHAR = '&'
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF12( IRX ), IRX = IRX1, IRX2 )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
            IRX1 = IRX2 + 1
         END DO
         IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF12( IRX ), IRX = IRX1, NEL )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
         END IF

      END IF
                                                                 
      RETURN
      END
