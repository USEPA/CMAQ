
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/hadv/yamo/hadvyppm.F,v 1.5 2011/10/21 16:11:19 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

C-----------------------------------------------------------------------
C Function:
C   Advection in the horizontal plane
C   The process time step is set equal to TSTEP(2). Boundary concentrations
C   are coupled in RDBCON with SqRDMT = Sq. Root [det ( metric tensor )]
C   = Jacobian / (map scale factor)**2
C   where Air Density X SqRDMT is loaded into last BCON slot for advection.
      
C Preconditions:
C   Dates and times represented YYYYDDD:HHMMSS.
C   No "skipped" dates and times.  All boundary input variables have the
C   same boundary perimeter structure with a thickness of 1
C   CGRID in transport units: SQRT{DET[metric tensor]}*concentration (Mass/Vol)
      
C Subroutines and functions called:
 
C Revision history:
C  19 Jan 2004: Jeff Young
C   7 Jul 2007: Jeff Young - declare MTRHOJ dimensions properly in order to retain
C                            them as declared in X_YAMO and Y_YAMO
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   06 Apr 15 J.Bash: speedup code      
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE, NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE
      
C Includes:

      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
      
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: JDATE         ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME         ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
                                             ! TSTEP(3) = twoway model time step w.r.t.
                                             ! wrf timestep and wrf/cmaq call frequency

      INTEGER, INTENT( IN ) :: ASTEP( : )    ! layer advection time step

C External Functions: None
      
C Parameters:

C Advected species dimension

      INTEGER, SAVE :: N_SPC_ADV

C File Variables:

      REAL, ALLOCATABLE, SAVE :: BCON( :,: )    ! boundary concentrations

C Local Variables:

!     REAL         XTRHOJ( 0:NCOLS,NROWS )
!     REAL         YTRHOJ( 0:NROWS,NCOLS ) <- violates stenex setup
      REAL, ALLOCATABLE, SAVE :: XTRHOJ( :,: )
      REAL, ALLOCATABLE, SAVE :: YTRHOJ( :,: )
      INTEGER       ALLOCSTAT

      INTEGER, SAVE :: ASPC                     ! RHOJ index in CGRID

      CHARACTER( 16 ) :: PNAME = 'HADVYPPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL XYFIRST

      integer, save :: logdev

      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      STEP                         ! ASTEP( L ), (dt) in sec
      INTEGER      DSTEP                        ! dt accumulator
      INTEGER      FDATE                        ! interpolation date
      INTEGER      FTIME                        ! interpolation time
      INTEGER      SYNCSTEP
      INTEGER      SUBSTEPS
      INTEGER      NSTEP

      INTEGER      COL, ROW, LVL                ! loop counters

C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE RDBCON ( FDATE, FTIME, TSTEP, LVL, BCON )
            IMPLICIT NONE
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( OUT ) :: BCON( :,: )
         END SUBROUTINE RDBCON
         SUBROUTINE X_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )
            USE HGRD_DEFN
            IMPLICIT NONE
            REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( IN )  :: BCON( :,: )
         END SUBROUTINE X_PPM
         SUBROUTINE X_YAMO ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON, MTRHOJ )
            USE HGRD_DEFN
            IMPLICIT NONE
            REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( IN )  :: BCON( :,: )
            REAL,    INTENT( IN )  :: MTRHOJ( 0:,1: )
         END SUBROUTINE X_YAMO
         SUBROUTINE Y_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )
            USE HGRD_DEFN
            IMPLICIT NONE
            REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( IN )  :: BCON( :,: )
         END SUBROUTINE Y_PPM
         SUBROUTINE Y_YAMO ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON, MTRHOJ )
            USE HGRD_DEFN
            IMPLICIT NONE
            REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( IN )  :: BCON( :,: )
            REAL,    INTENT( IN )  :: MTRHOJ( 1:,0: )
         END SUBROUTINE Y_YAMO
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         logdev = init3 ()

C Get CGRID offsets

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ
         ALLOCATE ( BCON( NBNDY,N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BCON'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ASPC = GC_STRT - 1 + N_GC_SPCD

         ALLOCATE ( XTRHOJ( 0:NCOLS,  NROWS ), STAT = ALLOCSTAT )
         ALLOCATE ( YTRHOJ(   NCOLS,0:NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating XTRHOJ or YTRHOJ'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF                    ! if firstime

      SYNCSTEP = TIME2SEC( TSTEP( 2 ) )

      DO LVL = 1, NLAYS

         STEP = TIME2SEC ( ASTEP( LVL ) )
         DSTEP = STEP
         SUBSTEPS = SYNCSTEP / STEP
         FDATE = JDATE
         FTIME = JTIME
         XYFIRST = .TRUE.

! If substeps is one or even unroll the loop to save some time.
         IF ( SUBSTEPS .EQ. 1 ) THEN

            CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
            FORALL( ROW = 1:MY_NROWS, COL = 1:MY_NCOLS )
               YTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
            END FORALL
            CALL SUBST_COMM ( YTRHOJ, DSPL_N0_E0_S1_W0, DRCN_S, '2 0' )
            CALL X_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
            CALL Y_YAMO ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON, YTRHOJ )
            DSTEP = DSTEP + STEP
            CALL NEXTIME( FDATE, FTIME, SEC2TIME( STEP ) )

         ELSE IF ( MOD( SUBSTEPS, 2 ) .EQ. 0 ) THEN

            DO NSTEP = 1, SUBSTEPS / 2
               CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
               FORALL( ROW = 1:MY_NROWS, COL = 1:MY_NCOLS )
                  YTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
               END FORALL
               CALL SUBST_COMM ( YTRHOJ, DSPL_N0_E0_S1_W0, DRCN_S, '2 0' )
               CALL X_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
               CALL Y_YAMO ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON, YTRHOJ )
               DSTEP = DSTEP + STEP
               CALL NEXTIME( FDATE, FTIME, SEC2TIME( STEP ) )

               CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
               FORALL( ROW = 1:MY_NROWS, COL = 1:MY_NCOLS )
                  XTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
               END FORALL
               CALL SUBST_COMM ( XTRHOJ, DSPL_N0_E0_S0_W1, DRCN_W, '1 0' )
               CALL Y_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
               CALL X_YAMO ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON, XTRHOJ )
               DSTEP = DSTEP + STEP
               CALL NEXTIME( FDATE, FTIME, SEC2TIME( STEP ) )
            END DO

         ELSE

            DO NSTEP = 1, SUBSTEPS
               CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
               IF ( XYFIRST ) THEN
                  XYFIRST = .FALSE.
                  FORALL( ROW = 1:MY_NROWS, COL = 1:MY_NCOLS )
                     YTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
                  END FORALL
                  CALL SUBST_COMM ( YTRHOJ, DSPL_N0_E0_S1_W0, DRCN_S, '2 0' )
                  CALL X_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
                  CALL Y_YAMO ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON, YTRHOJ )
               ELSE
                  XYFIRST = .TRUE.
                  FORALL( ROW = 1:MY_NROWS, COL = 1:MY_NCOLS )
                     XTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
                  END FORALL
                  CALL SUBST_COMM ( XTRHOJ, DSPL_N0_E0_S0_W1, DRCN_W, '1 0' )
                  CALL Y_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
                  CALL X_YAMO ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON, XTRHOJ )
               END IF
               DSTEP = DSTEP + STEP
               CALL NEXTIME( FDATE, FTIME, SEC2TIME( STEP ) )
            END DO

         END IF
      END DO

      RETURN
      END
