
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
      SUBROUTINE ADVSTEP( JDATE, JTIME, TSTEP, ASTEP, NREPS )

C-----------------------------------------------------------------------
C Function:
C    Use Courant-number conditions to determine the (time-splitting) model
C    base synchronization time step TSTEP(2) and number NREPS of sync time
C    steps per output time step TSTEP(1). The base synchronization time step
C    is considered to be the Courant condition safe advection time step.
C    Minimum returned sync time step = 900 sec
C    Determines a Courant condition safe advection step in TSTEP(3) with
C    respect to a user-defined minimum sync time step [TSTEP(2)].

C    June 2003 J.Young
C    Restructure to produce layer dependent advection step
C    change arguments:
C       TSTEP( 1 ) = output timestep (same) - input
C       TSTEP( 2 ) = synchronization timestep (same) - output
C       eliminate TSTEP( 3 ). Add new argument ASTEP( NLAYS ) - output

C Preconditions:

C Subroutines and functions called:
C    SEC2TIME, TIME2SEC, DESC3, M3EXIT, HVELOC

C Revision history:
C    prototype 2/93 by CJC
C    configuration management prototype 6/94 by Dongming Hwang

C    Apr 15, 1996 by M. Talat Odman at NCSC:  the number of model time
C    steps is made even to assure symmetry of transport operators within
C    each output time step.

C    Jan 20, 1996 by M. Talat Odman at NCSC:  now applies the CFL condition
C    independently in all three contravariant directions and
C    finds minimum time step.

C    22 Apr 97 Jeff: only for horizontal advection
C                    keep Talat's even step mod for symmetric sciproc
C     2 Sep 97 Jeff: calc. separate sync and adv time steps, TSTEP(3)
C    12 Feb 98 Jeff: make sync step = chem step
C    27 Jun 98 Jeff: allow for constant vel field (time-independent file)

C    2 October, 1998 by Al Bourgeois at LM: parallel implementation
C
C    8 Jan 99 by David Wong at LM: GLOBAL_MAX function call is added
C
C    1/19/99 David Wong
C      -- add a loop_index call
C      -- change loop index ending point to avoid accessing invalid region.
C         (reason to do this is to prevent using boundary data from PINTERP,
C          which sets pseudo-boundary data to 0)
C
C    9 Jun 99 by David Wong at LM: change GLOBAL_MAX to GLOBAL_RMAX
C    21 Nov 00 J.Young: GLOBAL_RMAX -> Dave Wong's f90 stenex GLOBAL_MAX
C    Jeff - Feb 01 - module LOCALFILE to set CCTM IOAPI file defn's
C                  - assumed shape arrays
C   30 Mar 01 J.Young: Use HGRD_DEFN; replace INTERP3 with INTERPX
C    9 Sep 03 J.Young: return layer dependent advection tstep, ASTEP
C   27 Aug 03 J.Young: symmetric processing is now deprecated
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   26 Jul 06 J.Young: add WRF sigma VGTYP
C   27 Apr 07 J.Young: Using Peter Percell's (UH) two timestep max wind
C   20 Jan 09 J.Young: In addition to the CFL criterion, use horizontal divergence
C                      to establish safe advection time step
C   28 Oct 09 J.Young: Revert back from Percell's algorithm to Talat's fix for
C                      multiple adv steps per sync step
C   16 Mar 10 J.Young: Fix hdiv bug for MPI by communicating ghost cell data using
C                      new hveloc to add hdiv and replace hcontvel
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   10 May 11 D.Wong: incorporated twoway model implementation
C    6 Apr 12 J.Young: changed default HDIV_LIM to 0.9; made CFL an env var
C    1 Nov 18 David Wong: removed MY_N clause for NCOLS and NROWS
C    1 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C   20 Nov 19 F. Sidi: Updated MET STIME, SDATE, TSTEP, XCELL, YCELL to correspond
C                      to updates in Centralized I/O 
C   10 Feb 20 D. Wong: Updated sync step calculation to include input 
C                      meterology temporal frequency, to allow running with
C                      finer temporal scale.
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_GLOBAL_MAX_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_GLOBAL_MAX_MODULE, NOOP_UTIL_MODULE)
#endif
      USE centralized_io_module

      IMPLICIT NONE

C Includes:
      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:
      INTEGER, INTENT( IN ) :: JDATE ! current model simulation date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME ! current model simulation time (HHMMSS)
      INTEGER, INTENT( INOUT ) :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                ! TSTEP( 1 ) = local output step - INTENT( IN )
                                ! TSTEP( 2 ) = sciproc sync. step (chem) - INTENT( OUT )
                                ! TSTEP( 3 ) = twoway model time step w.r.t. wrf time
                                !              step and wrf/cmaq call frequency
      INTEGER, INTENT( OUT ) :: ASTEP( : )   ! layer advection sub-steps per sync step
                                             ! (HHMMSS)
      INTEGER, INTENT( OUT ) :: NREPS        ! no. of sync steps per output step

C Parameters:

C External Functions:

C Local Variables:
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      INTEGER, SAVE :: ADVLAYR       ! adv=sync at least up to this level

      INTEGER, SAVE :: WSTEP = 0     ! wind file interpolation time step
      INTEGER, SAVE :: SDATE, STIME  ! wind file start date and time
      INTEGER, SAVE :: FDATE, FTIME  ! wind file current date and time
      INTEGER, SAVE :: FSTEP         ! wind file time step
      INTEGER, SAVE :: OSTEP         ! output time step (sec)

      REAL,    SAVE :: IDX1, IDX2    ! 1/dx1, 1/dx2

      INTEGER :: MINSECS             ! effective MINSYNC (sec) that divides TSTEP(1)
 
      INTEGER :: C, R, L, K  ! loop counters
      INTEGER :: EDATE, ETIME
      INTEGER :: SYNC, SUB, REP      ! loop variables
      INTEGER :: ADV, TADV           ! test sub adv step
      INTEGER :: NADVS( NLAYS )      ! no. of advection sub-steps per sync step
      REAL    :: RSYNC
      REAL    :: MXUOVDX             ! max ( WIND / DS ) all layers 1, ADVLAYR
      LOGICAL :: ADJFLG

      REAL    :: UWIND( NCOLS+1,NROWS+1,NLAYS )
      REAL    :: VWIND( NCOLS+1,NROWS+1,NLAYS )
      REAL    :: HDIV( NCOLS,NROWS,NLAYS )
                                                    ! MAX ( WIND / DS ) ...
      REAL, ALLOCATABLE, SAVE :: WIND_IDX_BEG( : )  ! at beginning of output step
      REAL, ALLOCATABLE, SAVE :: WIND_IDX_END( : )  ! at end of output step
      REAL, ALLOCATABLE, SAVE :: WIND_IDX    ( : )  ! for whole output step
      REAL, ALLOCATABLE, SAVE :: MX_HDIV_BEG( : )   ! at beginning of output step
      REAL, ALLOCATABLE, SAVE :: MX_HDIV_END( : )   ! at end of output step
      REAL, ALLOCATABLE, SAVE :: MX_HDIV    ( : )   ! for whole output step

      REAL(8), PARAMETER :: TOL = 1.D-3 ! Tolerance Parameter for checking that grid 
                                        ! specifications from input files match those 
                                        ! in the model. Units are in meters.

      CHARACTER( 16 ) :: PNAME = 'ADVSTEP'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER :: ALLOCSTAT

      CHARACTER( 80 ) :: VARDESC  ! environment variable description

      INTERFACE
        SUBROUTINE HVELOC( FDATE, FTIME, FSTEP, UWIND, VWIND, HDIV )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, FSTEP
            REAL,    INTENT( OUT ) :: UWIND( :,:,: ), VWIND( :,:,: )
            REAL,    INTENT( OUT ) :: HDIV( :,:,: )
         END SUBROUTINE HVELOC
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         CALL LOG_HEADING( LOGDEV, 'INITIALIZE ADVECTION STEPPING' )

         IF ( SIGST .GT. VGLVS_GD( 2 ) .OR.
     &        SIGST .LT. VGLVS_GD( NLAYS + 1 ) ) THEN
            WRITE(XMSG, '(A, 1PE12.3)') 'SIGMA_SYNC_TOP incorrect', SIGST
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         IF ( SIGST .EQ. VGLVS_GD( NLAYS + 1 ) ) THEN
            L = NLAYS
         ELSE
            DO K = 1, NLAYS
               IF ( SIGST .GE. VGLVS_GD( K+1 ) ) THEN
                  IF ( SIGST - VGLVS_GD( K+1 ) .LE.
     &                 VGLVS_GD( K ) - SIGST )  THEN
                     L = K
                  ELSE
                     L = K - 1
                  END IF
                  EXIT
               END IF
            END DO
         END IF

         ADVLAYR = L
         WRITE( LOGDEV,92005 ) ADVLAYR

         SDATE = FILE_SDATE(F_MET)
         STIME = FILE_STIME(F_MET)
         FSTEP = FILE_TSTEP(F_MET)

C Check file data against COORD.EXT

         IF ( ABS( FILE_XCELL(F_MET) - XCELL_GD ) .GT. TOL .OR.
     &        ABS( FILE_YCELL(F_MET) - YCELL_GD ) .GT. TOL      ) THEN
            WRITE( LOGDEV,* ) 'XCELL3D, XCELL_GD, YCELL3D, YCELL_GD: '
            WRITE( LOGDEV,* ) XCELL3D, XCELL_GD, YCELL3D,YCELL_GD
            XMSG = 'File grid sizes do not match CTM domain definition'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

C Get cell size in meters (from COORD.EXT)

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            IDX2 = 1.0 / ( DG2M * YCELL_GD )
            IDX1 = 1.0 / ( DG2M * XCELL_GD
     &           * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( NROWS/2 ) ) ) )
         ELSE
            IDX1 = 1.0 / XCELL_GD
            IDX2 = 1.0 / YCELL_GD
         END IF

C Get the time of starting wind field: do not interpolate
         IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE, STIME, FSTEP,
     &                        FDATE, FTIME) ) THEN
            XMSG = 'Cannot get step-starting date and time'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE( WIND_IDX_BEG( NLAYS ),
     &             WIND_IDX_END( NLAYS ),
     &             WIND_IDX    ( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating WIND_IDX*'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( MX_HDIV_BEG( NLAYS ),
     &             MX_HDIV_END( NLAYS ),
     &             MX_HDIV    ( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating MX_HDIV*'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
 
         IF ( .NOT. COLUMN_MODEL ) THEN

            CALL HVELOC( FDATE, FTIME, WSTEP, UWIND, VWIND, HDIV )

            DO L = 1, NLAYS
               WIND_IDX_BEG( L ) = 0.0
               MX_HDIV_BEG( L ) = 0.0
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     WIND_IDX_BEG( L ) = MAX( WIND_IDX_BEG( L ),
     &                                   ABS( UWIND( C,R,L ) ) * IDX1,
     &                                   ABS( VWIND( C,R,L ) ) * IDX2 )
                     MX_HDIV_BEG( L ) = MAX( MX_HDIV_BEG( L ), HDIV( C,R,L ) )
                  END DO
               END DO
               WIND_IDX_BEG( L ) = SUBST_GLOBAL_MAX( WIND_IDX_BEG( L ) )
               MX_HDIV_BEG( L )  = SUBST_GLOBAL_MAX( MX_HDIV_BEG( L ) )
            END DO

         ELSE

            UWIND = 0.0
            VWIND = 0.0
            HDIV  = 0.0
            WIND_IDX     = 0.0
            MX_HDIV      = 0.0
            WIND_IDX_BEG = 0.0
            WIND_IDX_END = 0.0
            MX_HDIV_END  = 0.0
            MX_HDIV_BEG  = 0.0

         END IF
      END IF   ! FIRSTIME

C Compute the least number of equal time steps that satisfy the Courant
C condition (force TSTEP(2) to be no greater than MAXSYNC):

#ifdef twoway
      OSTEP = TIME2SEC( TSTEP( 3 ) )
#else
      OSTEP = TIME2SEC( min(TSTEP( 1 ), FSTEP) )
#endif
      MAXSYNC = MIN( OSTEP, MAXSYNC )

C Make sure MINSYNC <= output time step
      IF ( MINSYNC .GT. OSTEP ) THEN
         WRITE( LOGDEV,92009 ) OSTEP, MINSYNC
         XMSG = ' '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      END IF

C Make sure MINSYNC <= MAXSYNC
      IF ( MINSYNC .GT. MAXSYNC ) THEN
         WRITE( LOGDEV,92010 ) MAXSYNC, MINSYNC
         XMSG = ' '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      END IF

C Determine MINSECS, the smallest time step (sec) between MINSYNC and MAXSYNC
C that divides the output step
      IF ( MINSYNC .LE. 1 ) THEN
         MINSECS = 1
      ELSE
         DO MINSECS = MINSYNC, MAXSYNC
            IF ( MOD( OSTEP, MINSECS ) .EQ. 0 ) THEN
               IF ( MINSECS .NE. MINSYNC )
     &              WRITE( LOGDEV,92011 ) JDATE, JTIME, MINSYNC, MINSECS
               EXIT
            ELSE IF ( MINSECS .LT. MAXSYNC ) THEN
               CYCLE
            END IF
C A satisfactory MINSECS could not be found
            WRITE( XMSG,92012 ) MINSYNC, OSTEP
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END DO
      END IF

C Establish ending time for this Courant number calculation:
      EDATE = JDATE
      ETIME = JTIME
#ifdef twoway
      CALL NEXTIME( EDATE, ETIME, TSTEP( 3 ) )
#else
      CALL NEXTIME( EDATE, ETIME, min(TSTEP( 1 ), FSTEP) )
#endif

      IF ( SECSDIFF( FDATE, FTIME, EDATE, ETIME ) .GT. 0 )
C Get the next wind field: do not interpolate
     &   CALL NEXTIME( FDATE, FTIME, FSTEP )

      IF ( .NOT. COLUMN_MODEL ) THEN

         CALL HVELOC( FDATE, FTIME, WSTEP, UWIND, VWIND, HDIV )

         DO L = 1, NLAYS
            WIND_IDX_END( L ) = 0.0
            MX_HDIV_END( L ) = 0.0
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  WIND_IDX_END( L ) = MAX( WIND_IDX_END( L ),
     &                                ABS( UWIND( C,R,L ) ) * IDX1,
     &                                ABS( VWIND( C,R,L ) ) * IDX2 )
                   MX_HDIV_END( L ) = MAX( MX_HDIV_END( L ), HDIV( C,R,L ) )
               END DO
            END DO
            WIND_IDX_END( L ) = SUBST_GLOBAL_MAX( WIND_IDX_END( L ) )
            MX_HDIV_END( L )  = SUBST_GLOBAL_MAX( MX_HDIV_END( L ) )
         END DO

      END IF

      WIND_IDX = MAX( WIND_IDX_BEG, WIND_IDX_END ) ! array
      WIND_IDX_BEG = WIND_IDX_END                  ! save for next call
      MX_HDIV = MAX( MX_HDIV_BEG, MX_HDIV_END )    ! array
      MX_HDIV_BEG = MX_HDIV_END                    ! save for next call

C Determine the synchronization timestep, TSTEP(2), and NREPS, the corresponding
C number of sync steps per output timestep

C Find the largest timestep that satisfies the Courant condition in all of the
C lower ADVLAYR layers. If this is smaller than MINSECS, then set the sync step
C to MINSECS. Otherwise, set the sync step to the largest time step that both
C satisfies the Courant condition and is an integer divisor of the output step
C (existence of such a sync step is guaranteed because, at this point, MINSECS
C satisfies both conditions).
      MXUOVDX = MAXVAL( WIND_IDX( 1:ADVLAYR ) )
      ADJFLG = .FALSE.
      DO REP = 1, OSTEP
         IF ( MOD( OSTEP, REP ) .EQ. 0 ) THEN   ! make TSTEP(2) divide TSTEP(1)
            SYNC = OSTEP / REP
            IF ( SYNC .LE. MAXSYNC ) THEN   ! enforce max TSTEP(2)
               ADV = SYNC
               IF ( MXUOVDX * REAL( ADV ) .LT. CFL ) THEN   ! CFL satisfied
                  IF ( SYNC .GE. MINSECS ) THEN   ! force min TSTEP(2)
                     NREPS = REP
                     GO TO 301
                  ELSE   ! multiple adv steps per sync step (= MINSECS)
                     SYNC = MINSECS
                     NREPS = OSTEP / MINSECS
                     DO SUB = 1, MINSECS
                        IF ( MOD( MINSECS, SUB ) .EQ. 0 ) THEN  ! make ADV divide TSTEP(2)
                           TADV = MINSECS / SUB
                           IF ( TADV .LE. ADV ) THEN
                              ADV = TADV
                              ADJFLG = .TRUE.
                              GO TO 301
                           END IF
                        END IF
                     END DO
                     GO TO 291   ! No solution
                  END IF
               END IF    ! if Courant condition satisfied
            END IF    ! if SYNC .le. MAXSYNC
         END IF    ! if REP divides STEP evenly
      END DO    ! REP loop

291   continue

C If you get here: could not determine satisfactory advection time step.

      WRITE( LOGDEV,94011 ) min(TSTEP( 1 ), FSTEP), MXUOVDX
      XMSG = ' '
      CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )

301   CONTINUE

      TSTEP( 2 ) = SEC2TIME( SYNC )
      DO L = 1, ADVLAYR
         ASTEP( L ) = SEC2TIME( ADV )
         NADVS( L ) = SYNC / ADV
      END DO

      IF ( ADJFLG ) THEN
         WRITE( LOGDEV,92019 ) MINSECS, TSTEP( 2 ), NREPS
      ELSE
         WRITE( LOGDEV,92021 ) TSTEP( 2 ), NREPS
      END IF

      DO L = ADVLAYR + 1, NLAYS
         DO REP = 1, SYNC
            IF ( MOD( SYNC, REP ) .EQ. 0 ) THEN
               ADV = SYNC / REP
               IF ( WIND_IDX( L ) * REAL( ADV ) .LT. CFL ) THEN  ! Layer CFL satisfied
                  NADVS( L ) = REP
                  ASTEP( L ) = SEC2TIME( ADV )
                  GO TO 401
               END IF   ! if courant condition satisfied
            END IF   !  if rep divides step
         END DO

C If you get here: could not determine satisfactory advection time step.

         WRITE( LOGDEV,94013 ) min(TSTEP( 1 ), FSTEP), L, WIND_IDX( L )
         XMSG = ' '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )

401      CONTINUE

      END DO   ! layer loop

C For each layer ensure that Max(HDIV)*ADV < 1.

      WRITE( LOGDEV,92025 )
      DO L = NLAYS, 1, -1   ! start at the top, purely for the 92027, 92029 write
         ADV = TIME2SEC( ASTEP( L ) )
         IF ( REAL( ADV ) * MX_HDIV( L ) .LT. HDIV_LIM ) THEN
            WRITE( LOGDEV,92027 ) L, ASTEP( L ), NADVS( L )
            CYCLE
         ELSE
            SUB = ADV
            DO WHILE ( SUB .GT. 0 )
               SUB = SUB - 1
               IF ( MOD( SYNC, SUB ) .EQ. 0 ) THEN
                  ASTEP( L ) = SEC2TIME( SUB )
                  NADVS( L ) = SYNC / SUB
                  GO TO 501
               END IF
            END DO
C If you get here: could not determine satisfactory advection time step.

            WRITE( LOGDEV,94013 ) min(TSTEP( 1 ), FSTEP), L, WIND_IDX( L )
            XMSG = ' '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )

501         CONTINUE
            WRITE( LOGDEV,92029 ) L, ASTEP( L ), NADVS( L )
         END IF
      END DO

      RETURN

C------------------  FORMAT  STATEMENTS   ------------------------------

92005 FORMAT(   5X, 'Top layer thru which sync step determined:', I3 )

92009 FORMAT( / 5X, 'Output time step:', I8,
     &          1X, 'less than minimum synchronization step:', I8 )

92010 FORMAT( / 5X, 'Maximum synchronization step:', I8,
     &          1X, 'less than minimum synchronization step:', I8 )

92011 FORMAT( / 5X, 'From ADVSTEP - date/time: ', I8, '/', I6.6
     &        / 5X, 'Minimum Synchronization Step adjusted from:', I8,
     &          1X, 'to:', I8 )

92012 FORMAT( / 5X, 'Starting from:', I6, ',',
     &          1X, 'could not determine minimum step that divides TSTEP',
     &          1X, 'for model step:', I7.6, ' HHMMSS' )

92019 FORMAT( / 5X, 'Synchronization step adjusted up to minimum (SEC):', I7
     &        /46X, '(HHMMSS): ', I6.6
     &        / 5X, 'Number of Synchronization steps:', I5  )

92021 FORMAT(   5X, 'Computed synchronization step (HHMMSS): ', I6.6
     &        / 5X, 'Number of Synchronization steps:', I5  )

92025 FORMAT( / 5X, 'Layer', 3X, 'Advection', 3X, 'per Sync'
     &        /11X, 'Step (HHMMSS)', 2X, 'Step' )

92027 FORMAT(   5X, I4, 6X, I6.6, 6X, I2 )
92029 FORMAT(   5X, I4, 6X, I6.6, 6X, I2, 2X, '<- Hdiv adjusted' )

94011 FORMAT( / 5X, 'Could not determine Courant-condition safe sync step',
     &          1X, 'for model step:', I7.6, ' HHMMSS',
     &        / 5X, '(Max vel)/(dX) =', 1PE10.3)

94013 FORMAT( / 5X, 'Could not determine Courant-condition safe sync step',
     &          1X, 'for model step:', I7.6, ' HHMMSS',
     &          1X, 'in layer:', I3
     &        / 5X, '(Max vel)/(dX) =', 1PE10.3)

      END
