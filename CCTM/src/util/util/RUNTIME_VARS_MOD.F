
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/DEPV_DEFN.F,v 1.12 2012/01/19 14:22:45 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE RUNTIME_VARS_MOD

C-----------------------------------------------------------------------
C Function: Collection of run time variables for model options and outputs
C
C Revision History:
C   4 Dec 2017 J.Bash: initial implementation 
C-----------------------------------------------------------------------

      IMPLICIT NONE

#ifdef twoway
      LOGICAL, SAVE :: SD_TIME_SERIES    = .FALSE.
      LOGICAL, SAVE :: CMAQ_WRF_FEEDBACK = .FALSE.
#endif
      LOGICAL, SAVE :: LERROFF   = .TRUE.  ! Flag to stop run if errors found
      LOGICAL, SAVE :: CKSUM     = .TRUE.  ! flag for cksum on, default = [T]
      LOGICAL, SAVE :: ILDEPV    = .TRUE.  ! flag in-lining depv
      LOGICAL, SAVE :: SFC_HONO  = .FALSE. ! flag for HONO interaction with surfaces within in-lining depv
      LOGICAL, SAVE :: ABFLUX    = .FALSE. ! flag for ammonia bi-directional flux with in-lining depv
      LOGICAL, SAVE :: MOSAIC    = .FALSE. ! flag for mosaic - luc specific deposition - within in-lining depv
      LOGICAL, SAVE :: FST       = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: PX_LSM    = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: CLM_LSM   = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: NOAH_LSM  = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: DEPV_DIAG = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: STAGE     = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      Logical, Save :: MINKZ     = .TRUE.  ! flag for minimum Kz 
      Logical, Save :: HGBIDI    = .FALSE. ! flag for Hg bidirectional exchange
      LOGICAL, SAVE :: W_VEL     = .FALSE. ! flag for vertical velocity 
      LOGICAL, SAVE :: GRAV_SETL = .TRUE.  ! flag for aerosol gravitational setling
      LOGICAL, SAVE :: VDIFFDIAG = .FALSE. ! flag for VDIFF diagnostic files
      LOGICAL, SAVE :: BIOGEMIS  = .FALSE. ! flag to in-line biogenic VOC emissions

      INTEGER, SAVE :: RUNLEN
      INTEGER, SAVE :: STDATE
      INTEGER, SAVE :: STTIME
      INTEGER, SAVE :: LOCAL_TSTEP   ! set to TSTEP( 1 ) in initscen.F
      INTEGER, SAVE :: MAXSYNC       ! force max TSTEP(2) (sec)
      INTEGER, SAVE :: MINSYNC       ! force min TSTEP(2) (sec)
      INTEGER, SAVE :: MAXSYNC       ! force max TSTEP(2) (sec)
      INTEGER, SAVE :: MINSYNC       ! force min TSTEP(2) (sec)
      INTEGER, SAVE :: ADVLAYR       ! adv=sync at least up to this level

      REAL,    SAVE :: SIGST         ! sigma_sync_top value
      REAL,    SAVE :: HDIV_LIM      ! cutoff for max horizontal divergence step adj
      REAL,    SAVE :: CFL           ! maximum Courant-Friedrichs-Lewy number allowed

      CONTAINS                                 
      SUBROUTINE INIT_VARS( JDATE, JTIME, logdev )

      USE UTILIO_DEFN

         IMPLICIT NONE                         

      INTEGER, INTENT( In )  :: jdate       
      INTEGER, INTENT( In )  :: jtime  
      INTEGER, INTENT( In )  :: logdev  

      LOGICAL SUCCESS
      CHARACTER( 240 )       :: MSG   = ' '  
      CHARACTER(  16 ), SAVE :: PNAME = 'Init_Vars'
      CHARACTER( 17 )  :: CTM_CMAQ_WRF_FEEDBACK = 'CMAQ_WRF_FEEDBACK'! CMAQ WRF radiative feedback
      CHARACTER( 16 )  :: CTM_SD_TIME_SERIES    = 'SD_TIME_SERIES'   ! ????????????????????????????
      CHARACTER( 16 )  :: CTM_STDATE            = 'CTM_STDATE'       ! environment variable start date
      CHARACTER( 16 )  :: CTM_STTIME            = 'CTM_STTIME'       ! environment variable start time
      CHARACTER( 16 )  :: CTM_RUNLEN            = 'CTM_RUNLEN'       ! environment variable run duration
      CHARACTER( 16 )  :: CTM_TSTEP             = 'CTM_TSTEP'        ! environment variable output time step
      CHARACTER( 16 )  :: CTM_PROGNAME          = 'CTM_PROGNAME'     ! driver program name (Why?)
      CHARACTER( 16 )  :: CTM_PROGNAME          = 'CTM_LERROFF'      ! stop on inconsistent input files
      CHARACTER( 16 )  :: CTM_MAXSYNC           = 'CTM_MAXSYNC'      ! environment variable max sync step
      CHARACTER( 16 )  :: CTM_MINSYNC           = 'CTM_MINSYNC'      ! environment variable min sync step
      CHARACTER( 16 )  :: SIGMA_SYNC_TOP        = 'SIGMA_SYNC_TOP'   ! environment variable adv top layer for sync step calculation
      CHARACTER( 16 )  :: CTM_ADV_CFL           = 'CTM_ADV_CFL'      ! environment variable max Courant-Friedrichs-Lewy number allowed
      CHARACTER( 16 )  :: ADV_HDIV_LIM          = 'ADV_HDIV_LIM'     ! environment variable max horizontal divergence for adv step adjustment
      CHARACTER( 16 )  :: CTM_CKSUM             = 'CTM_CKSUM'        ! env var for cksum on
      CHARACTER( 16 )  :: CTM_ILDEPV            = 'CTM_ILDEPV      ' ! environment variable for in-line DEPV
      CHARACTER( 16 )  :: CTM_ABFLUX            = 'CTM_ABFLUX      ' ! environment variable for ammonia bi-directional flux
      CHARACTER( 16 )  :: CTM_SFC_HONO          = 'CTM_SFC_HONO    ' ! environment variable for HONO interaction with surfaces
      CHARACTER( 16 )  :: CTM_MOSAIC            = 'CTM_MOSAIC      ' ! environment variable for mosaic - luc specific deposition velocities
      CHARACTER( 16 )  :: CTM_FST               = 'CTM_FST         ' ! environment variable for mosaic - luc specific stomatal flux
      CHARACTER( 16 )  :: CTM_PX_VERSION        = 'PX_VERSION      ' ! environment variable for WRF PX LSM output 
      CHARACTER( 16 )  :: CTM_CLM_VERSION       = 'CLM_VERSION     ' ! environment variable for WRF CLM LSM output 
      CHARACTER( 16 )  :: CTM_NOAH_VERSION      = 'NOAH_VERSION    ' ! environment variable for WRF Noah LSM output 
      CHARACTER( 16 )  :: CTM_DEPV_FILE         = 'CTM_DEPV_FILE'    ! environment variable for diagnostic DEPV file
      CHARACTER( 16 )  :: CTM_STAGE             = 'CTM_STAGE'        ! environment variable for surface tiled aerosol and gaseous exchange model
      Character( 16 )  :: CTM_HGBIDI            = 'CTM_HGBIDI'       ! environment variable for Hg bidirectional exchange
      CHARACTER( 16 )  :: CTM_WVEL              = 'CTM_WVEL'         ! environment variable for vertical velocity diagnostic
      CHARACTER( 16 )  :: CTM_KZMIN             = 'KZMIN'            ! environment variable for minium Kz
      CHARACTER( 16 )  :: CTM_AERO_GRAV_SETL    = 'CTM_GRAV_SETL'    ! environment variable for aerosol gravitational settling
      CHARACTER( 16 )  :: CTM_VDIFF_DIAG_FILE   = 'VDIFF_DIAG_FILE'  ! environment variable for VDIFF diagnostic files
      CHARACTER( 16 )  :: CTM_BIOGEMIS          = 'CTM_BIOGEMIS'     ! environment variable for inline biogenic VOC emissions


C variables for deposition velocities diagnostic file
      INTEGER          :: STATUS      ! ENV... status
      CHARACTER( 80 )  :: VARDESC     ! environment variable description

C ---------------------------------------------------------------------------------------------------------------------------------
C -----------------------------    DRIVER Enviornment Variables -------------------------------------------------------------------
C ---------------------------------------------------------------------------------------------------------------------------------

#ifdef twoway
         VARDESC = 'CMAQ WRF radiative feedback'
         CMAQ_WRF_FEEDBACK = ENVYN( CTM_CMAQ_WRF_FEEDBACK, VARDESC, CMAQ_WRF_FEEDBACK, STATUS )
         IF ( STATUS .EQ. 1 ) THEN
            PRINT *, 'Environment variable improperly formatted'
            STOP
         ELSE IF ( STATUS .EQ. -1 ) THEN
            PRINT *, 'Environment variable set, but empty ... Using default:'
         ELSE IF ( STATUS .EQ. -2 ) THEN
            PRINT *, 'Environment variable not set ... Using default:'
         END IF

         VARDESC = 'This does not appear to exist'
         SD_TIME_SERIES = ENVYN( CTM_SD_TIME_SERIES, VARDESC, SD_TIME_SERIES, STATUS )
         IF ( STATUS .EQ. 1 ) THEN
            PRINT *, 'Environment variable improperly formatted'
            STOP
         ELSE IF ( STATUS .EQ. -1 ) THEN
            PRINT *, 'Environment variable set, but empty ... Using default:'
         ELSE IF ( STATUS .EQ. -2 ) THEN
            PRINT *, 'Environment variable not set ... Using default:'
         END IF
#endif
#ifndef twoway
         VARDESC = 'Main Program Name'
         CALL ENVSTR( CTM_PROGNAME, VARDESC, 'DRIVER', PROGNAME, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         END IF

         LOCAL_TSTEP  = 010000         ! default
         VARDESC = 'Scenario Output Time Step (HHMMSS)'
         LOCAL_TSTEP = ENVINT( CTM_TSTEP, VARDESC, LOCAL_TSTEP, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         END IF

         STDATE = 1995192        ! default
         VARDESC = 'Scenario Starting Date (YYYYDDD)'
         STDATE = ENVINT( CTM_STDATE, VARDESC, STDATE, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         END IF

         STTIME = 000000         ! default
         VARDESC = 'Scenario Starting Time (HHMMSS)'
         STTIME = ENVINT( CTM_STTIME, VARDESC, STTIME, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
           MSG = 'Environment variable improperly formatted'
           CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         END IF

         RUNLEN = 480000         ! default
         VARDESC = 'Scenario Run Duration (HHMMSS)'
         RUNLEN = ENVINT( CTM_RUNLEN, VARDESC, RUNLEN, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
           MSG = 'Environment variable improperly formatted'
           CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
         END IF
#endif
         VARDESC = 'Flag for stopping on errors'
         LERROFF = ENVYN( CTM_LERROFF, VARDESC, LERROFF, STATUS )  
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

         MAXSYNC = 720          ! default
         VARDESC = 'Maximum Synchronization Time Step (sec)'
         MAXSYNC = ENVINT( CTM_MAXSYNC, VARDESC, MAXSYNC, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV,'(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

         MINSYNC = 60           ! default
         VARDESC = 'Minimum Synchronization Time Step (sec)'
         MINSYNC = ENVINT( CTM_MINSYNC, VARDESC, MINSYNC, STATUS )
         IF ( STATUS .NE. 0 ) WRITE(LOGDEV,'(5X, A)') VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

         CFL = 0.75             ! default
         VARDESC = 'Maximum CFL number allowed'
         CFL = ENVREAL( CTM_ADV_CFL, VARDESC, CFL, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV,'(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

         SIGST = 0.7            ! default
         VARDESC = 'Minimum layer limit for which adv = sync'
         SIGST = ENVREAL( SIGMA_SYNC_TOP, VARDESC, SIGST, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV,'(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

         HDIV_LIM = 0.9
         VARDESC = 'Maximum horiz. div. limit for adv step adjustment'
         HDIV_LIM = ENVREAL( ADV_HDIV_LIM, VARDESC, HDIV_LIM, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV,'(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF
C ---------------------------------------------------------------------------------------------------------------------------------
C -----------------------------    SCIPROC Environment Variables --------------------------------------------------------------------
C ---------------------------------------------------------------------------------------------------------------------------------

         VARDESC = 'Check sum on flag'
         CKSUM = ENVYN( CTM_CKSUM, VARDESC, CKSUM, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

C ---------------------------------------------------------------------------------------------------------------------------------
C -----------------------------    Air Surface Excahnge Environment Variables -----------------------------------------------------
C ---------------------------------------------------------------------------------------------------------------------------------

C In-line deposition velocities calculation?
         ILDEPV = .TRUE.
         VARDESC = 'Flag for in-line deposition velocities'
         ILDEPV = ENVYN( CTM_ILDEPV, VARDESC, ILDEPV, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

         IF ( ILDEPV ) THEN
C Ammonia bi-directional flux with in-line deposition velocities calculation?
            VARDESC = 'Flag for ammonia bi-directional flux calculation'
            ABFLUX = ENVYN( CTM_ABFLUX, VARDESC, ABFLUX, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF
C PX LSM?
            VARDESC = 'Flag for WRF PX LSM'
            PX_LSM = ENVYN( PX_VERSION, VARDESC, PX_LSM, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

C CLM LSM?
            VARDESC = 'Flag for WRF CLM LSM'
            CLM_LSM = ENVYN( CLM_VERSION, VARDESC, CLM_LSM, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

C NOAH LSM?
            VARDESC = 'Flag for WRF NOAH LSM'
            NOAH_LSM = ENVYN( NOAH_VERSION, VARDESC, NOAH_LSM, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

C Sarwar`s HONO interaction with leaf and building surfaces?
            VARDESC = 'Flag for HONO interaction with surfaces'
            SFC_HONO = ENVYN( CTM_SFC_HONO, VARDESC, SFC_HONO, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF
            
C Mosaic method to get land-use specific deposition velocities?
            VARDESC = 'Flag for land-use specific deposition velocity calculation'
            MOSAIC = ENVYN( CTM_MOSAIC, VARDESC, MOSAIC, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

C Tiled Aeosol and Gaseous Surface Exchange (TAGSX) model ?
            VARDESC = 'Flag Tiled Aeosol and Gaseous Surface Exchange (TAGSX) model'
            STAGE = ENVYN( CTM_STAGE, VARDESC, TAGSX, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               MSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

C Mosaic method to get land-use specific stomatal flux
            IF ( MOSAIC ) THEN
               VARDESC = 'Flag for land-use specific stomatal flux calculation'
               FST = ENVYN( CTM_FST, VARDESC, MOSAIC, STATUS )
               IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
               IF ( STATUS .EQ. 1 ) THEN
                  MSG = 'Environment variable improperly formatted'
                  CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
                  SUCCESS = .FALSE.; RETURN
               ELSE IF ( STATUS .EQ. -1 ) THEN
                  MSG = 'Environment variable set, but empty ... Using default:'
                  WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
               ELSE IF ( STATUS .EQ. -2 ) THEN
                  MSG = 'Environment variable not set ... Using default:'
                  WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
               END IF
            END IF
         END IF   ! in-line depv
C Get diagnostic file flag.
         VARDESC = 'Flag for writing the DEPV diagnostic file'
         DEPV_DIAG = ENVYN( CTM_DEPV_FILE, VARDESC, DEPV_DIAG, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 0 ) THEN
            WRITE( LOGDEV,'( 5X, A )' ) VARDESC
            MSG = TRIM( PNAME ) // ': writes GAS DEPV values to '
     &                           // TRIM( CTM_DEPV_FILE )
            WRITE( LOGDEV,'( 5X, A )' ) MSG
         ELSE IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG =
     &      'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV,'( 5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV,'( 5X, A, I9 )' ) MSG, JTIME
         END IF

         MSG   = 'Flag for mercury bi-directional flux calculation'
         HGBIDI = ENVYN( CTM_HGBIDI, MSG, HGBIDI, STATUS )
         If ( STATUS .NE. 0 ) Write( LOGDEV, '(5X, A)' ) MSG
         If ( STATUS .EQ. 1 ) Then
            MSG = 'Environment variable improperly formatted'
            Call M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
         Else If ( STATUS .EQ. -1 ) Then
            MSG = 'Environment variable set, but empty ... Using default:'
            Write( LOGDEV, '(5X, A)' ) MSG
         Else If ( STATUS .EQ. -2 ) Then
            MSG = 'Environment variable not set ... Using default:'
            Write( LOGDEV, '(5X, A)' ) MSG
         End If

C ---------------------------------------------------------------------------------------------------------------------------------
C -----------------------------    Vertical diffusion Environment Variables -------------------------------------------------------
C ---------------------------------------------------------------------------------------------------------------------------------

         VARDESC = 'Kz min on flag'
         MINKZ = ENVYN( CTM_KZMIN, VARDESC, MINKZ, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

         VARDESC = 'Write vert. vel. flag'
         W_VEL = ENVYN( CTM_WVEL, VARDESC, W_VEL, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

C Get gravitational settling (sedi) flag.
         GRAV_SETL = .TRUE.         ! default
         VARDESC = 'Using J-,K-mode aerosols gravitational settling'
         GRAV_SETL = ENVYN( CTM_AERO_GRAV_SETL, VARDESC, GRAV_SETL, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

C Get diagnostic files flag.
         VARDESC = 'Writing the VDIFF diagnostic files'
         VDIFFDIAG = ENVYN( CTM_VDIFF_DIAG_FILE, VARDESC, VDIFFDIAG, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

C ---------------------------------------------------------------------------------------------------------------------------------
C -----------------------------    Emission Environment Variables -----------------------------------------------------------------
C ---------------------------------------------------------------------------------------------------------------------------------

         VARDESC = 'Flag for in-line biogenic emissions'
         BIOGEMIS = ENVYN( CTM_BIOGEMIS, VARDESC, BIOGEMIS, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, MSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF


         RETURN
         END SUBROUTINE INIT_VARS
         END MODULE RUNTIME_VARS_MOD
