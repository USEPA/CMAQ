
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


      MODULE BASIC_WRITE_ROUTINES

        IMPLICIT NONE

      CONTAINS
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WRHDR1 ( EXUNIT, NAMESTR, FMT_LEN )

      USE TEXT_UTILITIES

      IMPLICIT NONE

      INTEGER,        INTENT( IN ) :: EXUNIT
      CHARACTER( * ), INTENT( IN ) :: NAMESTR
      INTEGER,        INTENT( IN ) :: FMT_LEN
       
      INTEGER LX, LS, LQ, IX
                                 
C----------------------------------------------------------------------

      LS = SIZE_TEXT  ( 'leading', 'no_trailing', NAMESTR )
      LX = FMT_LEN - 3
      IF ( LS .LE. LX ) THEN
         WRITE( EXUNIT, 1001 ) NAMESTR( 1:LS )
1001     FORMAT( '!', 2X, A )
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
      END SUBROUTINE WRHDR1
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
      END SUBROUTINE WRBF6_FORTRAN90
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF6 ( WRUNIT, AWPL, NEL, IVAR )
C write to a 6-character buffer, format at no more than 10 elements per line
C (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
      INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
      INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
      INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
      INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
!local:
      INTEGER            :: IRX, IRX0, IRX1, IRX2, IOS, CNN
      INTEGER            :: WPL       ! words per line (max at 10)
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER( 1 )     :: CONCHAR
      CHARACTER( 6 )     :: BUFF6( MAXRXNUM )
 
      WPL = MIN ( AWPL, 10 )

      DO IRX = 1, NEL-1
         WRITE( BUFF6( IRX ), '(I5, '','')', IOSTAT = IOS ) IVAR( IRX )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2001 ) IRX, IVAR( IRX ), IOS
2001        FORMAT(/ 5X, '*** Error writing to internal buffer in WRBF6 ***'
     &             / 5X, 'Attempted to write element', I6,
     &               1X, 'and data:', I8
     &             / 5X, 'IOSTATUS = ', I6 )
            STOP
         END IF
      END DO
      WRITE( BUFF6( NEL ), '(I5, ''/'')', IOSTAT = IOS ) IVAR( NEL )
      IF ( IOS .NE. 0 ) THEN
         WRITE( LOGDEV,2001 ) NEL, IVAR( NEL ), IOS
         STOP
      END IF


      IF ( NEL / WPL .GE. 6 ) THEN
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            CNN = MOD ( IRX0-1, 10 )
            IF ( CNN .NE. 0 ) THEN
               WRITE( CONCHAR, '(I1)' ) CNN
            ELSE
               CONCHAR = 'O'
            END IF
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF6( IRX ), IRX = IRX1, IRX2 )
1001        FORMAT( 5X, A, 2X, 10A6 )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2003 ) WRUNIT, IRX, BUFF6( IRX ), IOS
2003           FORMAT( /5X, '*** Error writing to external unit', I3,
     &                  1X, 'in WRBF6 ***'
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
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF6( IRX ), IRX = IRX1, NEL )
            IF ( IOS .NE. 0) THEN
               WRITE( LOGDEV,2003 ) WRUNIT, IRX, BUFF6( IRX ), IOS
               STOP
            END IF
         END IF

      ELSE

         CONCHAR = '&'
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF6( IRX ), IRX = IRX1, IRX2 )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2003 ) WRUNIT, IRX, BUFF6( IRX ), IOS
               STOP
            END IF
            IRX1 = IRX2 + 1
         END DO
         IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) CONCHAR,
     &                                         ( BUFF6( IRX ), IRX = IRX1, NEL )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2003 ) WRUNIT, IRX, BUFF6( IRX ), IOS
               STOP
            END IF
         END IF

      END IF

      RETURN
      END SUBROUTINE WRBF6
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF16C_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR )
C write a 16-character variable to a 20-character buffer, format at no more
C than 3 elements per line (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
C arguments:
      INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
      INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
      INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
      CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write

C parameters:
      INTEGER, PARAMETER :: LOGDEV = 6

C local:
      CHARACTER(  4 )     :: CONCHAR
      CHARACTER( 20 )     :: BUFF20( NEL )

      INTEGER IRX, LINE, IRX1, IRX2, IRXF, IOS, CNN, WPL
 
C----------------------------------------------------------------------

      WPL = MIN( AWPL, 3 )

      DO IRX = 1, NEL-1
         WRITE( BUFF20( IRX ), 1001, IOSTAT = IOS ) VAR( IRX )
1001     FORMAT( 1X, "'", A16, "'," )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV, 2003 ) IRX, VAR( IRX ), IOS
2003        FORMAT( /5X, '*** Error writing to internal buffer in WRBF16C_FORTRAN90 ***'
     &              /5X, 'Attempted to write element', I6,
     &               1X, 'and data: ', A16
     &              /5X, 'IOSTATUS = ', I6 )
            STOP
            END IF
      END DO
      WRITE( BUFF20( NEL ), 1003, IOSTAT = IOS ) VAR( NEL )
1003  FORMAT( 1X, "'", A16, "'/" )
      BUFF20( NEL ) = TRIM( BUFF20( NEL ) ) // CHAR(33) // " "
      PRINT*,BUFF20( NEL ), CHAR(33)
      IF ( IOS .NE. 0 ) THEN
         WRITE( LOGDEV,2003 ) NEL, VAR( NEL ), IOS
         STOP
      END IF

      IRX1 = 1
      IRXF = 0
      DO LINE = 1, NEL / WPL
         IRX2 = IRX1 + WPL - 1
          IF( IRX2 .EQ. NEL )EXIT
!        IF ( MOD( LINE, 2 ) .NE. 0 ) THEN       ! every other line
            CNN = MOD( IRXF, 10 )
            IF ( CNN .NE. 0 ) THEN
               WRITE( CONCHAR, '(I1)' ) CNN
            ELSE
               CONCHAR = '0'
            END IF
            IRXF = IRXF + 1
!           ELSE
!           CONCHAR = '+'
!        END IF
         WRITE( WRUNIT, 1005, IOSTAT = IOS )( BUFF20( IRX ), IRX = IRX1, IRX2 ),
     &                                       CONCHAR
1005     FORMAT( 5X, '&', 3X, 3A20, ' & ! ', A )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2005) WRUNIT, IRX, BUFF20( IRX ), IOS
2005        FORMAT( /5X, '*** Error writing to external unit', I3, 
     &               1X, 'in WRBF16C_FORTRAN90 ***'
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
               WRITE( CONCHAR, '(I2)' ) CNN
            ELSE
               CONCHAR = ' 0'
            END IF
         ELSE
            CONCHAR = ' '
         END IF
         WRITE( WRUNIT, 1006, IOSTAT = IOS )( BUFF20( IRX ), IRX = IRX1, NEL ),
     &                                        ACHAR(IACHAR('!')),CONCHAR
1006       FORMAT( 5X, '&', 3X, 3A20, A, A )
         IF ( IOS .NE. 0 ) THEN
            WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF20( IRX ), IOS
            STOP
         END IF

      END IF

      RETURN
      END SUBROUTINE WRBF16C_FORTRAN90
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
      END SUBROUTINE WRBF16C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF12S ( WRUNIT, AWPL, NEL, VAR, AFMT )
C write a 4-byte variable to a 12-character buffer, format at no more than
C 5 elements per line (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
!      INCLUDE 'PARMS.e'
 
      INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
      INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
      INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
      REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
      CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
!local:      
      CHARACTER( 14 ) :: FMT1, FMT2
      CHARACTER( 14 ) :: EFMT1 = '(1PE11.4, '','')'
      CHARACTER( 14 ) :: EFMT2 = '(1PE11.4, ''/'')'
      CHARACTER( 14 ) :: FFMT1 = '(F11.5, '','')  '
      CHARACTER( 14 ) :: FFMT2 = '(F11.5, ''/'')  '
      INTEGER IRX, IRX0, IRX1, IRX2, IRXF, IOS, CNN, WPL
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER(  1 ) :: CONCHAR
      CHARACTER( 12 ) :: BUFF12( NEL )
 
C----------------------------------------------------------------------

      WPL = MIN( AWPL, 5 )
      IF ( AFMT .EQ. 'E' ) THEN
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
      END SUBROUTINE WRBF12S
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF12D_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR, AFMT )
C write an 8-byte variable to a 12-character buffer, format at no more than
C 5 elements per line (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
      INTEGER WRUNIT     ! logical write unit no.
      INTEGER AWPL, WPL  ! words per line (max at 5)
      INTEGER NEL        ! number of list elements
      REAL( 8 ) :: VAR( NEL )   ! real variable to write
      CHARACTER(  1 ) :: AFMT   ! write format: D -> 1PD11.4, E -> 1PE11.4, F -> F11.5
      
      CHARACTER( 14 ) :: FMT1, FMT2
      CHARACTER( 14 ) :: DFMT1 = '(1PD11.4, '','')'
      CHARACTER( 14 ) :: DFMT2 = '(1PD11.4, "/")'
      CHARACTER( 14 ) :: EFMT1 = '(1PE11.4, '','')'
      CHARACTER( 14 ) :: EFMT2 = '(1PE11.4, "/" )'
      CHARACTER( 14 ) :: FFMT1 = '(F11.5, '','')  '
      CHARACTER( 14 ) :: FFMT2 = '(F11.5, "/")  '
      INTEGER IRX, IRX0, IRX1, IRX2, IRXF, IOS, CNN
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER(  4 ) :: CONCHAR
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
2003        FORMAT( /5X, '1) *** Error writing to internal buffer in WRBF12D_FORTRAN90 ***'
     &              /5X, 'Attempted to write element', I6,
     &               1X, 'and data:', 1PD11.4
     &              /5X, 'IOSTATUS = ', I6)
            STOP
         END IF
      END DO
      WRITE( BUFF12( NEL ), FMT2, IOSTAT = IOS ) VAR( NEL )
      IF ( IOS .NE. 0 ) THEN
         WRITE( LOGDEV,2004 ) NEL, VAR( NEL ), IOS
2004        FORMAT( /5X, '2) *** Error writing to internal buffer in WRBF12D_FORTRAN90 ***'
     &              /5X, 'Attempted to write element', I6,
     &               1X, 'and data:', 1PD11.4
     &              /5X, 'IOSTATUS = ', I6)
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
            IF( IRX2 .EQ. NEL )EXIT
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
            WRITE( WRUNIT, 1001, IOSTAT = IOS )( BUFF12( IRX ), IRX = IRX1, IRX2 ),
     &                                          CONCHAR
1001        FORMAT( 5X, '&', 4X, 5A12, ' & ! ',  A )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
2005           FORMAT( /5X, '*** Error writing to external unit', I3, 
     &                  1X, 'in WRBF12D_FORTRAN90 ***'
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
           WRITE( WRUNIT, 1005, IOSTAT = IOS ) ( BUFF12( IRX ), IRX = IRX1, NEL ),
     &                                         ACHAR(IACHAR('!')), CONCHAR
1005       FORMAT( 5X, '&', 4X, 5A12, A, A )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
         END IF

      ELSE

         CONCHAR = ' '
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            IF( IRX2 .EQ. NEL )EXIT
            WRITE( WRUNIT, 1002, IOSTAT = IOS )( BUFF12( IRX ), IRX = IRX1, IRX2 )
1002        FORMAT( 5X, '&', 4X, 5A12, ' & ')
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
            IRX1 = IRX2 + 1
         END DO
         IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
            WRITE( WRUNIT, 1005, IOSTAT = IOS )( BUFF12( IRX ), IRX = IRX1, NEL )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
         END IF

      END IF
                                                                 
      RETURN
      END SUBROUTINE WRBF12D_FORTRAN90
      SUBROUTINE WRBF12D ( WRUNIT, AWPL, NEL, VAR, AFMT )
C write an 8-byte variable to a 12-character buffer, format at no more than
C 5 elements per line (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
      INTEGER, INTENT( IN )         :: WRUNIT     ! logical write unit no.
      INTEGER, INTENT( IN )         :: AWPL       ! words per line (max at 5)
      INTEGER, INTENT( IN )         :: NEL        ! number of list elements
      REAL(8), INTENT( IN )         :: VAR( : )   ! real variable to write
      CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
      
!local:      
      CHARACTER( 14 ) :: FMT1, FMT2
      CHARACTER( 14 ) :: DFMT1 = '(1PD11.4, '','')'
      CHARACTER( 14 ) :: DFMT2 = '(1PD11.4, ''/'')'
      CHARACTER( 14 ) :: EFMT1 = '(1PE11.4, '','')'
      CHARACTER( 14 ) :: EFMT2 = '(1PE11.4, ''/'')'
      CHARACTER( 14 ) :: FFMT1 = '(F11.5, '','')  '
      CHARACTER( 14 ) :: FFMT2 = '(F11.5, ''/'')  '
      INTEGER IRX, IRX0, IRX1, IRX2, IRXF, IOS, CNN, WPL
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
      END  SUBROUTINE WRBF12D
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRBF12S_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR, AFMT )
C write a 4-byte variable to a 12-character buffer, format at no more than
C 5 elements per line (due to FORMAT restrictions), then dump to file

      USE MECHANISM_PARMS
      
      IMPLICIT NONE
 
      INTEGER, INTENT( IN )         :: WRUNIT     ! logical write unit no.
      INTEGER, INTENT( IN )         :: AWPL       ! words per line (max at 5)
      INTEGER, INTENT( IN )         :: NEL        ! number of list elements
      REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
      CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5

      CHARACTER( 14 ) :: FMT1, FMT2
      CHARACTER( 14 ) :: EFMT1 = '(1PE11.4, '','')'
      CHARACTER( 14 ) :: EFMT2 = '(1PE11.4, "/" )'
      CHARACTER( 14 ) :: FFMT1 = '(F11.5, '','')  '
      CHARACTER( 14 ) :: FFMT2 = '(F11.5, "/")  '
      INTEGER IRX, IRX0, IRX1, IRX2, IRXF, IOS, CNN
      INTEGER         :: WPL
      INTEGER, PARAMETER :: LOGDEV = 6
      CHARACTER(  4 ) :: CONCHAR
      CHARACTER( 12 ) :: BUFF12( NEL )
 
C----------------------------------------------------------------------

      WPL = MIN( AWPL, 5 )
      IF ( AFMT .EQ. 'E' ) THEN
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


      IF ( NEL / WPL .GE. 6 ) THEN 
         IRX1 = 1
         IRXF = 0
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            IF( IRX2 .EQ. NEL )EXIT
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
            WRITE( WRUNIT, 1001, IOSTAT = IOS ) ( BUFF12( IRX ), IRX = IRX1, IRX2 ),
     &                                           CONCHAR
1001        FORMAT( 5X, '&', 4X, 5A12, ' & ! ', A )
1002        FORMAT( 5X, '&', 4X, 5A12, ' & ')
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
2005           FORMAT( /5X, '*** Error writing to external unit', I3, 
     &                  1X, 'in WRBF12S_FORTRAN90 ***'
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
                  CONCHAR = ' & '
               END IF
            ELSE
               CONCHAR = ' & '
            END IF
            WRITE( WRUNIT, 1005, IOSTAT = IOS ) ( BUFF12( IRX ), IRX = IRX1, NEL ),
     &                                           ACHAR(IACHAR('!')), CONCHAR
1005        FORMAT( 5X, '&', 4X, 5A12, A, A )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
         END IF

      ELSE

         CONCHAR = ' '
         IRX1 = 1
         DO IRX0 = 1, NEL / WPL
            IRX2 = IRX1 + WPL - 1
            IF( IRX2 .EQ. NEL )EXIT
            WRITE( WRUNIT, 1002, IOSTAT = IOS ) ( BUFF12( IRX ), IRX = IRX1, IRX2 )
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
            IRX1 = IRX2 + 1
         END DO
         IF ( IRX1 .LE. NEL ) THEN      ! assumes last DO index incremented by 1
            WRITE( WRUNIT, 1005, IOSTAT = IOS ) ( BUFF12( IRX ), IRX = IRX1, NEL ) 
            IF ( IOS .NE. 0 ) THEN
               WRITE( LOGDEV,2005 ) WRUNIT, IRX, BUFF12( IRX ), IOS
               STOP
            END IF
         END IF

      END IF
                                                                 
      RETURN
      END SUBROUTINE WRBF12S_FORTRAN90
      SUBROUTINE WRITE_RATE_CONVERT(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
           CASE( -1 )
             WRITE(OUT_UNIT, 95004, ADVANCE = 'NO')
           CASE( -2 )
             WRITE(OUT_UNIT, 95005, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' INV_RFACTOR * ')                
95001   FORMAT(' 60.0D0 * ')                
95002   FORMAT(' RFACTOR * ')                
95003   FORMAT(' RFACTOR_SQU * ')                
95004   FORMAT(' ( 60.0D0 / RFACTOR ) * ')                
95005   FORMAT(' ( 60.0D0 / RFACTOR_SQU ) * ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT

      SUBROUTINE WRITE_RATE_CONVERT_BEFORE(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
           CASE( -2 )
             WRITE(OUT_UNIT, 95005, ADVANCE = 'NO')
           CASE( -1 )
             WRITE(OUT_UNIT, 95004, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' INV_CFACT * ')                
95001   FORMAT(' SFACT * ')                
95002   FORMAT(' CFACT * ')                
95003   FORMAT(' CFACT_SQU * ')                
95004   FORMAT(' RFACT * ')                
95005   FORMAT(' RFACT_SQU * ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT_BEFORE
      SUBROUTINE WRITE_RATE_CONVERT_AFTER(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
           CASE( -2 )
             WRITE(OUT_UNIT, 95005, ADVANCE = 'NO')
           CASE( -1 )
             WRITE(OUT_UNIT, 95004, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' * INV_CFACT ')                
95001   FORMAT(' * SFACT ')                
95002   FORMAT(' * CFACT ')                
95003   FORMAT(' * CFACT_SQU ')                
95004   FORMAT(' * RFACT ')                
95005   FORMAT(' * RFACT_SQU ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT_AFTER
      SUBROUTINE WRITE_RATE_CONVERT_TIME(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' INV_RFACT * ')                
95001   FORMAT(' ')                
95002   FORMAT(' RFACT * ')                
95003   FORMAT(' RFACT_SQU * ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT_TIME
      END MODULE BASIC_WRITE_ROUTINES
