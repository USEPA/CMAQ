
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SETUP_LOGDEV () 

C Purpose
C   Establish ancillary run time log files for each parallel processor
C   Effectively no operation, if serial

C Revision History:
C   ???      J.Young - initial
C   26 Dec, 01 David Wong
C      -- removed MPI_INIT call since the same function has been called in
C         subroutine PAR_INIT
C   11 Jun 02 J.Young - minor mods
C   23 Feb 11 J.Young - SETENVVAR is a function, not a subroutine
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   12 Aug 15 D.Wong:   Replaced SUBST_MPI with mpif.h

      USE UTILIO_DEFN
      USE RUNTIME_VARS

      IMPLICIT NONE

#ifdef parallel      
      include 'mpif.h'
#endif
      INTEGER, PARAMETER :: MAXLEN_CCTM_APPL = 200
      INTEGER, PARAMETER :: MAXLEN_CTM_LOG   = MAXLEN_CCTM_APPL + 11

      CHARACTER(  8 ) :: PRESTR  = 'CTM_LOG_'
      CHARACTER(  9 ) :: DIAGSTR = 'CTM_DIAG_'
      CHARACTER(  3 ) :: CMYPE
      CHARACTER(  8 ) :: APPL = 'CTM_APPL'

      CHARACTER( MAXLEN_CCTM_APPL ) :: EQNAME
      CHARACTER( MAXLEN_CCTM_APPL ) :: BLDFOLD
      CHARACTER( MAXLEN_CTM_LOG   ) :: IOLOGEQ, IODIAGEQ

      INTEGER :: ERROR, TOTPE, IOST, FHEAD, IHEAD
      INTEGER :: NIOAPI_HEAD, NCMAQ_HEAD
      INTEGER, PARAMETER :: NHEAD = 200
      CHARACTER( 90 ) :: CMAQ_HEADER( NHEAD )
      CHARACTER( 90 ) :: IOAPI_HEADER( NHEAD )

      CALL NAMEVAL ( APPL, EQNAME )

      OUTDEV = 6  ! Assign Standard Out File Unit

      ! Fetch CMAQ Header
      FHEAD = JUNIT()
      CALL NAMEVAL( "BLD", BLDFOLD )
      OPEN( FHEAD, FILE = TRIM( BLDFOLD ) // '/log_header.txt' )
      DO IHEAD = 1,NHEAD
          READ( FHEAD, '(A)', IOSTAT=IOST ), CMAQ_HEADER( IHEAD )
          ! Check for End of File
          IF ( IOST .LT. 0 ) THEN
             NCMAQ_HEAD = IHEAD - 1
             EXIT
          END IF
      END DO
      CLOSE( FHEAD )

#ifdef parallel
      CALL MPI_COMM_RANK ( MPI_COMM_WORLD, MYPE, ERROR )
      CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, TOTPE, ERROR )
#else
      MYPE = 0
      TOTPE = 0
#endif
 
      WRITE ( CMYPE, '(I3.3)' ) MYPE

      ! Create Log Filename using the processor rank and initialize
      ! I/O-API and Logs
      IF ( APPL .NE. EQNAME(1:8 ) ) THEN
         IOLOGEQ = PRESTR // CMYPE // '.' // TRIM( EQNAME )
      ELSE
         IOLOGEQ = PRESTR // CMYPE
      END IF
      IF ( .NOT. SETENVVAR ( 'LOGFILE', IOLOGEQ ) ) THEN
         WRITE( *,* ) '*** Could not set environment variable for ' // IOLOGEQ
         STOP
      END IF

      ! Write CMAQ Header to Screen
      IF ( MYPE .EQ. 0 ) THEN
      DO IHEAD = 1,NCMAQ_HEAD
          WRITE( OUTDEV, '(A)' ) CMAQ_HEADER( IHEAD )
      END DO
      END IF

      ! Redirect Standard Output
      IF ( MYPE .NE. 0 ) OPEN( UNIT = 6, FILE = "/dev/null", STATUS = "OLD" )

      LOGDEV = INIT3() 

      ! Put Standard Output Back to File Unit 6 By Closing File Unit 6
      IF ( MYPE .NE. 0 ) CLOSE( 6 ) 

      ! Fetch I/O-API Header
      REWIND( LOGDEV )
      DO IHEAD = 1,NHEAD
          READ( LOGDEV, '(A)', IOSTAT=IOST ), IOAPI_HEADER( IHEAD )
          ! Check for End of File
          IF ( IOST .LT. 0 ) THEN
             NIOAPI_HEAD = IHEAD - 1
             EXIT
          END IF
      END DO
      REWIND( LOGDEV )

      ! Write CMAQ Header and then I/O-API Header
      DO IHEAD = 1,NCMAQ_HEAD
          WRITE( LOGDEV, '(A)' ) CMAQ_HEADER( IHEAD )
      END DO
      WRITE( LOGDEV, * )
      DO IHEAD = 1,NIOAPI_HEAD
          WRITE( LOGDEV, '(A)' ) IOAPI_HEADER( IHEAD )
      END DO


      ! Concatenate Diagnostic Filename
      IF ( CTM_DIAG_LVL .GT. 0 ) THEN 
         IF ( APPL .NE. EQNAME(1:8 ) ) THEN
            IODIAGEQ = DIAGSTR // CMYPE // '.' // TRIM( EQNAME )
         ELSE
            IODIAGEQ = DIAGSTR // CMYPE
         END IF
         
         ! Find File Unit Number for Diag File
         DIAGDEV = JUNIT()

         ! Open File With Error Checking
         OPEN( UNIT=DIAGDEV, IOSTAT=IOST, FILE=IODIAGEQ, ACCESS='SEQUENTIAL')
         IF ( IOST .NE. 0 ) THEN
              WRITE( *, '( /, 5X, 3 A )' )
     &             '***  ERROR in SETUP_LOGDEV  ***'
              WRITE( *, '( 5X, A, I10 )' )
     &             'Error opening diagnostic file on unit', DIAGDEV,
     &             'I/O STATUS =', IOST
              WRITE( *, '( 5X, 2 A, / )' ), 'File: ', TRIM( IODIAGEQ )
              CALL EXIT( 2 )
         END IF 
 
         ! Write CMAQ Header and then I/O-API Header
         REWIND( DIAGDEV )
         DO IHEAD = 1,NCMAQ_HEAD
            WRITE( DIAGDEV, '(A)' ) CMAQ_HEADER( IHEAD )
         END DO
         WRITE( DIAGDEV, * )
         DO IHEAD = 1,NIOAPI_HEAD
            WRITE( DIAGDEV, '(A)' ) IOAPI_HEADER( IHEAD )
         END DO
      END IF

    

      END SUBROUTINE SETUP_LOGDEV 
