
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
C   May 2019 j. pleim:  removed yamo hadvect subroutines
C   11 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
      
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE XY_BUDGET
      USE PA_DEFN, ONLY: LIPR
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE, NOOP_UTIL_MODULE)
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY:SENGRID, NPMAX, NP, BSEN
#endif 

      IMPLICIT NONE
      
C Includes:

      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
      
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER     JDATE         ! current model date, coded YYYYDDD
      INTEGER     JTIME         ! current model time, coded HHMMSS
      INTEGER     TSTEP( 3 )    ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

      INTEGER     ASTEP( : )    ! layer advection time step

C External Functions: None
      
C Parameters:

C Advected species dimension

      INTEGER, SAVE :: N_SPC_ADV

C File Variables:

      REAL, ALLOCATABLE, SAVE :: BCON( :,: )    ! boundary concentrations

C Local Variables:


      INTEGER       ALLOCSTAT

      CHARACTER( 16 ) :: PNAME = 'HADVPPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, ALLOCATABLE, SAVE :: XYFIRST( : )
      REAL, ALLOCATABLE, SAVE :: CGRID0( :,:,: ), XADV(:,:,:), YADV(:,:,:)


      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 199 ) :: XMSG2 = ' '

      INTEGER      STEP                         ! ASTEP( L ), (dt) in sec
      INTEGER      DSTEP                        ! dt accumulator
      INTEGER      FDATE                        ! interpolation date
      INTEGER      FTIME                        ! interpolation time
      INTEGER      SYNCSTEP

      INTEGER      LVL                ! loop counters
      LOGICAL      L_WRITE_WARNING

C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE RDBCON ( FDATE, FTIME, TSTEP, LVL, BCON,
     &                       L_WRITE_WARNING )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( OUT ) :: BCON( :,: )
            LOGICAL, INTENT( INOUT ) :: L_WRITE_WARNING
         END SUBROUTINE RDBCON
         SUBROUTINE X_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )
            REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
!           REAL,    INTENT( IN )  :: BCON( NBNDY,* )
            REAL,    INTENT( IN )  :: BCON( :,: )
         END SUBROUTINE X_PPM
         SUBROUTINE Y_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )
            REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: FDATE, FTIME, TSTEP, LVL
!           REAL,    INTENT( IN )  :: BCON( NBNDY,: )
            REAL,    INTENT( IN )  :: BCON( :,: )
         END SUBROUTINE Y_PPM
!        SUBROUTINE LCKSUMMER ( PNAME, CGRID, JDATE, JTIME, LVL )
!           CHARACTER( * ), INTENT( IN ) :: PNAME
!           REAL, POINTER                :: CGRID( :,:,:,: )
!           INTEGER, INTENT( IN )        :: JDATE, JTIME, LVL
!        END SUBROUTINE LCKSUMMER
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         ALLOCATE( XYFIRST( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating XYFIRST'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         XYFIRST( : ) = .TRUE.

C Get CGRID offsets

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ
         ALLOCATE ( BCON( NBNDY,N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BCON'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( LIPR ) THEN
            ALLOCATE ( DELC_XADV( NCOLS,NROWS,NLAYS,NSPCSD ),
     &                 DELC_YADV( NCOLS,NROWS,NLAYS,NSPCSD ),
     &                 XADV( NCOLS,NROWS,NSPCSD ),
     &                 YADV( NCOLS,NROWS,NSPCSD ),
     &                 CGRID0( NCOLS,NROWS,NSPCSD ) )
         END IF

#ifdef sens
         ALLOCATE ( BSEN( NBNDY,N_SPC_ADV,NPMAX ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BSEN'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#endif

      END IF                    ! if firstime

      SYNCSTEP = TIME2SEC( TSTEP( 2 ) )

      ! Initialize Budget Variables
      IF (LIPR) THEN
        DELC_XADV = 0.0
        DELC_YADV = 0.0
      END IF

      L_WRITE_WARNING = .TRUE.
      DO 301 LVL = 1, NLAYS

         STEP = TIME2SEC ( ASTEP( LVL ) )
         DSTEP = STEP
         FDATE = JDATE
         FTIME = JTIME

         IF ( LIPR ) THEN
            XADV = 0.0
            YADV = 0.0
         END IF

101      CONTINUE

         CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON, L_WRITE_WARNING )

         IF ( LIPR ) CGRID0( :,:,: ) = CGRID( :,:,LVL,: )

         IF ( XYFIRST( LVL ) ) THEN

            XYFIRST( LVL ) = .FALSE.

            CALL X_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
!           CALL LCKSUMMER ( 'X_PPM', CGRID, FDATE, FTIME, LVL )
            IF ( LIPR ) THEN
               XADV( :,:,: ) = XADV( :,:,: ) + CGRID( :,:,LVL,: ) - CGRID0( :,:,: )
               CGRID0( :,:,: ) = CGRID( :,:,LVL,: )
            END IF

            CALL Y_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
!           CALL LCKSUMMER ( 'Y_PPM', CGRID, FDATE, FTIME, LVL )
            IF ( LIPR ) THEN
               YADV( :,:,: ) = YADV( :,:,: ) + CGRID( :,:,LVL,: ) - CGRID0( :,:,: )
               CGRID0( :,:,: ) = CGRID( :,:,LVL,: )
            END IF

         ELSE

            XYFIRST( LVL ) = .TRUE.

            CALL Y_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
!           CALL LCKSUMMER ( 'Y_PPM', CGRID, FDATE, FTIME, LVL )
            IF ( LIPR ) THEN
               YADV( :,:,: ) = YADV( :,:,: ) + CGRID( :,:,LVL,: ) - CGRID0( :,:,: )
               CGRID0( :,:,: ) = CGRID( :,:,LVL,: )
            END IF

            CALL X_PPM ( CGRID, FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
!           CALL LCKSUMMER ( 'X_PPM', CGRID, FDATE, FTIME, LVL )
            IF ( LIPR ) THEN
               XADV( :,:,: ) = XADV( :,:,: ) + CGRID( :,:,LVL,: ) - CGRID0( :,:,: )
               CGRID0( :,:,: ) = CGRID( :,:,LVL,: )
            END IF

         END IF

         DSTEP = DSTEP + STEP
         IF ( DSTEP .LE. SYNCSTEP ) THEN
            CALL NEXTIME( FDATE, FTIME, SEC2TIME( STEP ) )
            GO TO 101
         END IF

         IF (LIPR ) THEN
           DELC_XADV( :,:,LVL,: ) = DELC_XADV( :,:,LVL,: ) + XADV( :,:,: )
           DELC_YADV( :,:,LVL,: ) = DELC_YADV( :,:,LVL,: ) + YADV( :,:,: )
         END IF
301   CONTINUE
 
      RETURN
      END
