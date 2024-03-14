
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
C    16 Sep 16 J.Young: update for inline procan (IPR)
C    10 July 19 F. Sidi: Renamed Couple_WRF to be couple
C-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE RUNTIME_VARS, ONLY: LOGDEV, OUTDEV, MYPE, CKSUM, 
     &                        PRINT_PROC_TIME, WEEKDAY, 
     &                        LOG_MESSAGE, TIMING_SPLIT, STM,
     &                        COLUMN_MODEL, LPVO3
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
      USE PA_UPDATE
#endif
      USE PA_DEFN, Only: LIPR, IPR_VDIF, IPR_ZADV, IPR_HDIF, IPR_CLDS, 
     &                   IPR_CHEM, IPR_PVO3,
     &                   BUDGET_DIAG ! Process Analysis control and data variables
#ifdef isam
      USE SA_DEFN, Only: ISAM
      USE GRID_CONF
      USE CGRID_SPCS, ONLY: NSPCSD
#endif

      USE STM_MODULE, ONLY: STM_WRAP_GAS, STM_WRAP_AE

      IMPLICIT NONE   

#ifdef parallel
      INCLUDE 'mpif.h'
#endif
C Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )

      INTEGER, INTENT(INOUT) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT(INOUT) :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT(IN)    :: TSTEP( 3 )   ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
                                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                             !            step and wrf/cmaq call frequency

      INTEGER, INTENT(IN)    :: ASTEP( : )   ! layer advection time step

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'SCIPROC'
      CHARACTER( 120 ) :: XMSG = ' '
      CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'

      INTEGER         ::     SDATE       ! current science process date, coded YYYYDDD
      INTEGER         ::     STIME       ! current science process time, coded HHMMSS
 
      REAL            :: REAL_TIME

      REAL( 8 )       :: CPU_TIME_START  ! Timing individual
      REAL( 8 )       :: CPU_TIME_START_MASTER
      CHARACTER( 24 ) :: CDATE_TIME

#ifdef isam
      REAL, ALLOCATABLE, SAVE :: CGRID0( :,:,:,: )
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
#endif
      INTERFACE
         SUBROUTINE CKSUMMER ( PNAME, CGRID, JDATE, JTIME )
            CHARACTER( * ), INTENT( IN ) :: PNAME
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE CKSUMMER
#ifndef mpas
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
#endif
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
         SUBROUTINE PVO3 ( CGRID, JDATE, JTIME )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE PVO3
         SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE AERO
#ifdef isam
         SUBROUTINE SA_WRAP_AE( CGRID0, CGRID, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            REAL                      :: CGRID0( :,:,:,: )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE
            INTEGER, INTENT( IN )     :: JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE SA_WRAP_AE
#endif
      END INTERFACE
  
C-----------------------------------------------------------------------
#ifdef isam
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ALLOCATE( CGRID0( NCOLS,NROWS,NLAYS,NSPCSD ) )
      END IF
#endif

      ! Calculate and Write Time Step Information to Summary Logfile
      IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * ) 
      WRITE( LOGDEV, * )

#ifdef parallel
      CPU_TIME_START =  MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_START = REAL( REAL_TIME,8 )
#endif
      CPU_TIME_START_MASTER = CPU_TIME_START
      WRITE( XMSG, '(A,I7.7,A,I6.6)' ), 'Processing Day/Time [YYYYDDD:HHMMSS]: ',JDATE, 
     &       ':',JTIME
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CALL LOG_MESSAGE( LOGDEV, XMSG )

      CDATE_TIME = DT2STR( JDATE, JTIME )
      WRITE( XMSG, '(2x,6A)' ), 'Which is Equivalent to (UTC): ',
     &       CDATE_TIME(1:8),' ',TRIM( WEEKDAY( WKDAY( JDATE ) ) ),
     &       ',', CDATE_TIME(9:)
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CALL LOG_MESSAGE( LOGDEV, XMSG )

      WRITE( XMSG, '(2x,A,I6.6)' ), 'Time-Step Length (HHMMSS): ',TSTEP(2) 
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CALL LOG_MESSAGE( LOGDEV, XMSG )

      ! Calculate Vertical Diffusion, Dry Deposition, and Emissions
      CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'VDIFF', CGRID, JDATE, JTIME )
#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_VDIF, CGRID, JDATE, JTIME, TSTEP, .FALSE. )
#endif
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'VDIFF' )

      ! Save time at end of the current time-step in SDATE and STIME
      SDATE = JDATE
      STIME = JTIME
      CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

      IF ( .NOT. COLUMN_MODEL ) THEN
         ! Calculate Horizontal Transport
         ! couple CGRID for advection and horizontal diffusion
         ! The COUPLE routine converts aerosol mass from ug/m3 to kg/m3. The 
         ! DECOUPLE routine converts it back to ug/m3.

#ifndef mpas
         CALL COUPLE ( CGRID, JDATE, JTIME, TSTEP )
         IF ( CKSUM ) CALL CKSUMMER ( 'COUPLE', CGRID, JDATE, JTIME )
         IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_ZADV, CGRID, JDATE, JTIME, TSTEP, .TRUE. )
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'COUPLE' )

         CALL SUBST_BARRIER()
         CALL HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
         IF ( CKSUM ) CALL CKSUMMER ( 'HADV', CGRID, JDATE, JTIME )
         IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_HADV ( CGRID, JDATE, JTIME, TSTEP )
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'HADV' )
         
         CALL ZADV ( CGRID, JDATE, JTIME, TSTEP )
         IF ( CKSUM ) CALL CKSUMMER ( 'ADV', CGRID, JDATE, JTIME )
         IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_ZADV, CGRID, JDATE, JTIME, TSTEP, .TRUE. )
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'ZADV' )
         
         CALL HDIFF ( CGRID, JDATE, JTIME, TSTEP )
         IF ( CKSUM ) CALL CKSUMMER ( 'HDIFF', CGRID, JDATE, JTIME )
         IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_HDIF, CGRID, JDATE, JTIME, TSTEP, .TRUE. )
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'HDIFF' )
#endif

#ifndef mpas
         ! Decouple CGRID for cloud and chemistry corresponding to conditions
         ! at the end of the time step (i.e. SDATE, STIME)
         CALL DECOUPLE ( CGRID, SDATE, STIME, TSTEP )
         IF ( CKSUM ) CALL CKSUMMER ( 'DECOUPLE', CGRID, JDATE, JTIME )
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'DECOUPLE' )
         IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_ZADV, CGRID, SDATE, STIME, TSTEP, .FALSE. )
#endif

      END IF ! Not Column Model
      
      ! Calculate Photolysis Rates
      CALL PHOT ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'PHOT', CGRID, JDATE, JTIME )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'PHOT' )

      ! Calculate Cloud Scavening and Chemistry
      CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'CLDPROC', CGRID, JDATE, JTIME )
#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_CLDS, CGRID, SDATE, STIME, TSTEP, .FALSE. )
#endif
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'CLDPROC' )

      ! Calculate Gas- and Particle-Phase Chemistry
      IF ( STM ) CALL STM_WRAP_GAS ( CGRID, JDATE, JTIME, .TRUE. )
      CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'CHEM', CGRID, JDATE, JTIME )
#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_CHEM, CGRID, SDATE, STIME, TSTEP, .FALSE. ) 
#endif
      IF ( STM ) CALL STM_WRAP_GAS ( CGRID, JDATE, JTIME, .FALSE. )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'CHEM' )

      IF ( LPVO3 ) THEN
         CALL PVO3 ( CGRID, JDATE, JTIME )
         IF ( CKSUM ) CALL CKSUMMER ( 'PVO3', CGRID, JDATE, JTIME )
#ifndef mpas
         IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_PROC ( IPR_PVO3, CGRID, SDATE, STIME, TSTEP, .FALSE. )
#endif
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'PVO3' )
      END IF

#ifdef isam
      ! Save ISAM concentration field to use for apportioning
      ! aerosols and semivolatile gases after AERO is called
      CGRID0  = CGRID
#endif

      ! Calculate Aerosol Microphysics
      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
#ifdef isam
      CALL SA_WRAP_AE( CGRID0, CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'AERO', CGRID, JDATE, JTIME )
      IF ( STM ) CALL STM_WRAP_AE ( CGRID, JDATE, JTIME )
#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) CALL PA_UPDATE_AERO ( CGRID, SDATE, STIME )
#endif
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'AERO' )

      ! Advance Time Step
      CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
      WRITE( LOGDEV,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

      ! Print Summary Timing for Master time step      
      CALL TIMING_SPLIT ( CPU_TIME_START_MASTER, 2 )

1002  FORMAT ( 2x, A15, ' completed... ', F6.1, ' seconds' )

      RETURN
      END
