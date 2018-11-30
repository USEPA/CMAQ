
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
      SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

C-----------------------------------------------------------------------
C Function:
C    Controls all of the physical and chemical processes for a grid
C    Operator splitting symmetric around chemistry
 
C Preconditions:
C    Dates and times represented YYYYDDD:HHMMSS.
C    No "skipped" dates and times.  All boundary input variables (layered or
C    non-layered) have the same perimeter structure with a thickness of NTHIK
 
C Subroutines and functions called:
C    All physical and chemical subroutines, 
C    DECOUPLE, COUPLE
 
C Revision History:
C    Oct. 24, 1995 by M. Talat Odman and Clint L. Ingram at NCSC: created
C    13 Dec 97 - Jeff - uncouple diffusion processes
C    27 Jun 98 - Jeff - sync step = chem step
C     7 Jul 01 - Shawn - mv cloud processing before chem
C       Jan 02 - Jeff - dyn alloc; remove PCGRID argument to ping
C    23 Jun 03 J.Young: for layer dependent advection tstep
C    18 Aug 03 J. Pleim - move vdiff before advection
C    29 Aug 03 J.Young: eliminate symmetric processing option and unused
C                       SUBST_GRID_ID string
C    30 May 05 J.Young: mass-conserving advection (yamo)
C     7 Jul 07 J.Young: No cksummer call after HADV - delay possible truncation of
C                       HADV negative conc's until after ZADV
C    25 Jul 08 S.Roselle: removed PING from modeled processes
C    16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C    11 May 11 D.Wong: incorporated twoway model implementation
C    25 Mar 16 R.Mathur: incorporated pot. vorticity strat. O3 scaling
C-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE RUNTIME_VARS

      IMPLICIT NONE   

C Include files:
      INCLUDE 'mpif.h'
      INCLUDE SUBST_PACTL_ID    ! PA control parameters

C Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

!     INTEGER      ASTEP( NLAYS )  ! layer advection time step
      INTEGER      ASTEP( : )      ! layer advection time step

C Parameters:

C External Functions:

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'SCIPROC'

      CHARACTER( 120 ) :: XMSG = ' '
      CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'

      INTEGER      SDATE        ! current science process date, coded YYYYDDD
      INTEGER      STIME        ! current science process time, coded HHMMSS
 
      INTEGER      ALLOCSTAT
      INTEGER      STATUS          ! ENV... status
      CHARACTER( 80 ) :: VARDESC   ! environment variable description
      CHARACTER( 80 ) :: MSG = ' '
      REAL( 8 )       :: CPU_TIME_START  ! Timing individual
      REAL( 8 )       :: CPU_TIME_START_MASTER
      CHARACTER( 24 ) :: CDATE_TIME

      INTERFACE
         SUBROUTINE CKSUMMER ( PNAME, CGRID, JDATE, JTIME )
            CHARACTER( * ), INTENT( IN ) :: PNAME
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE CKSUMMER
         SUBROUTINE PA_UPDATE ( PNAME, CGRID, JDATE, JTIME, TSTEP )
            CHARACTER( * ), INTENT( IN ) :: PNAME
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE PA_UPDATE
         SUBROUTINE HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
            INTEGER, INTENT( IN )     :: ASTEP( : )
         END SUBROUTINE HADV
         SUBROUTINE ZADV ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE ZADV
         SUBROUTINE COUPLE ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE COUPLE
         SUBROUTINE DECOUPLE ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE DECOUPLE
         SUBROUTINE HDIFF ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE HDIFF
         SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE VDIFF
         SUBROUTINE PHOT ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( : )
         END SUBROUTINE PHOT
         SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE CLDPROC
         SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE CHEM
#ifdef potvorto3
         SUBROUTINE PVO3 ( CGRID, JDATE, JTIME )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE PVO3
#endif
         SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE AERO
      END INTERFACE
  
C-----------------------------------------------------------------------

! Write Time Step Information to Summary Logfile
      IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * ) 
      CPU_TIME_START = MPI_WTIME()
      CPU_TIME_START_MASTER = CPU_TIME_START
      WRITE( XMSG, '(A,I7.7,A,I6.6)' ), 'Processing Day/Time [YYYYDDD:HHMMSS]: ',JDATE, 
     &       ':',JTIME
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CDATE_TIME = DT2STR( JDATE, JTIME )
      WRITE( XMSG, '(2x,A,A)' ), 'Which is Equivalent to: ',CDATE_TIME
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )

      WRITE( XMSG, '(2x,A,I6.6)' ), 'Time-Step Length (HHMMSS): ',TSTEP(2) 
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )

C Synchronization Step Physical Processes Sequence
      CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'VDIFF', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'VDIF', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'VDIFF' )

C couple CGRID for advection and horizontal diffusion
C The COUPLE_WRF routine converts aerosol mass from ug/m3 to kg/m3. The 
C DECOUPL_WRF routine converts it back to ug/m3.

      CPU_TIME_START = MPI_WTIME()
      CALL COUPLE ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'COUPLE', CGRID, JDATE, JTIME )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'COUPLE' )

      CALL SUBST_BARRIER()
      CALL HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'HADV', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'HADV', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'HADV' )

      CALL ZADV ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'ADV', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'ZADV', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'ZADV' )

      CALL HDIFF ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'HDIFF', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'HDIF', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'HDIFF' )

C decouple CGRID for cloud and chemistry

      SDATE = JDATE
      STIME = JTIME
      CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

      CALL DECOUPLE ( CGRID, SDATE, STIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'DECOUPLE', CGRID, JDATE, JTIME )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'DECOUPLE' )

      CALL PHOT ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'PHOT', CGRID, JDATE, JTIME )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'PHOT' )

      CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'CLDPROC', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'CLDS', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'CLDPROC' )

      CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'CHEM', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'CHEM', CGRID, JDATE, JTIME, TSTEP ) 
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'CHEM' )

#ifdef potvorto3
      CALL PVO3 ( CGRID, JDATE, JTIME )
      IF ( CKSUM ) CALL CKSUMMER ( 'PVO3', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'PVO3', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'PVO3' )
#endif

      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'AERO', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE_AERO ( JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'AERO' )

      CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
      WRITE( LOGDEV,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

! Print Summary Timing for Master time step      
      CALL TIMING_SPLIT ( CPU_TIME_START_MASTER, 2 )

      RETURN
      END
