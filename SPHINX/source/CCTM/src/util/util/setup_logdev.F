
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

      USE RUNTIME_VARS
      USE UTILIO_DEFN

      IMPLICIT NONE

#ifdef parallel      
      include 'mpif.h'
#endif
      INTEGER, PARAMETER :: MAXLEN_CTM_LOG   = MAXLEN_CCTM_APPL + 11

      CHARACTER(  8 ) :: PRESTR  = 'CTM_LOG_'
      CHARACTER( MAXLEN_CTM_LOG ) :: IOLOGEQ

      INTEGER :: IOST, IHEAD
      INTEGER :: NIOAPI_HEAD, NCMAQ_HEAD
      INTEGER, PARAMETER :: NHEAD = 200
      CHARACTER( 90 ) :: CMAQ_HEADER( NHEAD )
      CHARACTER( 90 ) :: IOAPI_HEADER( NHEAD )

      integer :: i
      logical :: found, opened

      ! Fetch CMAQ Header
      CALL LOAD_HEADER( CMAQ_HEADER, NCMAQ_HEAD )

#ifdef mpas
      write ( cmype, '(I3.3)' ) mype
      iologeq = 'CTM_LOG_' // cmype

      i = 99
      found = .false.
      do while ((.not. found) .and. (i .le. 150))
         inquire (unit=i, opened=opened)
         if (.not. opened) then
            found = .true.
         else
            i = i + 1
         end if
      end do

      logdev = i

      open ( unit    =  logdev,
     &       iostat  =  iost,
     &       file    =  iologeq,
     &       status  =  'new',
     &       access  =  'sequential')

#else

      ! Create Log Filename using the processor rank and initialize
      ! I/O-API and Logs
      WRITE ( CMYPE, '(I3.3)' ) MYPE
      IOLOGEQ = PRESTR // CMYPE // '.' // TRIM( APPL_NAME )
      IF ( .NOT. SETENVVAR ( 'LOGFILE', IOLOGEQ ) ) THEN
         WRITE( *,* ) '*** Could not set environment variable for ' // IOLOGEQ
         CALL M3EXIT( 'SETUP_LOGDEV', 0, 0, '', 2 )
      END IF

#ifndef twoway
      ! Write CMAQ Header to Screen
      IF ( MYPE .EQ. 0 ) THEN
         DO IHEAD = 1,NCMAQ_HEAD
            WRITE( OUTDEV, '(A)' ) CMAQ_HEADER( IHEAD )
         END DO
      END IF
#endif

      ! Redirect Standard Output
      IF ( MYPE .NE. 0 ) OPEN( UNIT = 6, FILE = "/dev/null", STATUS = "OLD" )

      LOGDEV = INIT3() 

      ! Put Standard Output Back to File Unit 6 By Closing File Unit 6
      IF ( MYPE .NE. 0 ) CLOSE( 6 ) 

      ! Fetch I/O-API Header
#ifndef twoway
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
#endif
#endif

      ! Write CMAQ Header and then I/O-API Header
      DO IHEAD = 1,NCMAQ_HEAD
          WRITE( LOGDEV, '(A)' ) CMAQ_HEADER( IHEAD )
      END DO
      WRITE( LOGDEV, * )
#ifndef mpas
#ifndef twoway
      DO IHEAD = 1,NIOAPI_HEAD
          WRITE( LOGDEV, '(A)' ) IOAPI_HEADER( IHEAD )
      END DO
#endif
#endif

      END SUBROUTINE SETUP_LOGDEV 
