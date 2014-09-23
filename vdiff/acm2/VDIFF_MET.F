
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/rdmet.F,v 1.5 2011/10/21 16:11:46 yoj Exp $
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE VDIFF_MET

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: RDEPVHT( :,: )    ! air dens / dep vel ht
      REAL,    ALLOCATABLE, SAVE :: RJACM  ( :,:,: )  ! reciprocal mid-layer Jacobian
!     REAL,    ALLOCATABLE, SAVE :: RVJACMF( :,:,: )  ! 1/ mid-full layer vert Jac
      REAL,    ALLOCATABLE, SAVE :: RRHOJ  ( :,:,: )  ! reciprocal density X Jacobian
      REAL,    ALLOCATABLE, SAVE :: DENS1  ( :,: )    ! layer 1 air density

      REAL,    ALLOCATABLE, SAVE :: ZF     ( :,:,: )  ! layer surface ht above ground
      REAL,    ALLOCATABLE, SAVE :: DZF    ( :,:,: )  ! layer surface thickness
      REAL,    ALLOCATABLE, SAVE :: DENS   ( :,:,: )  ! air density
      REAL,    ALLOCATABLE, SAVE :: RJACF  ( :,:,: )  ! reciprocal full-layer Jacobian
      REAL,    ALLOCATABLE, SAVE :: RMSFX4 ( :,: )    ! inverse map scale factor ** 4

      REAL,    ALLOCATABLE, SAVE :: DX3F   ( : )     ! sigma layer surface thickness
      REAL,    ALLOCATABLE, SAVE :: RDX3F  ( : )     ! reciprocal sigma layer thickness
      REAL,    ALLOCATABLE, SAVE :: RDX3M  ( : )     ! reciprocal sigma midlayer thickness

      REAL,    ALLOCATABLE, SAVE :: HOL    ( :,: )   ! PBL over Obukhov length
      REAL,    ALLOCATABLE, SAVE :: XPBL   ( :,: )   ! PBL sigma height
      INTEGER, ALLOCATABLE, SAVE :: LPBL   ( :,: )   ! PBL layer
      LOGICAL, ALLOCATABLE, SAVE :: CONVCT ( :,: )   ! convection flag

      REAL,    ALLOCATABLE, SAVE :: PBL    ( :,: )   ! pbl height (m)
      REAL,    ALLOCATABLE, SAVE :: USTAR  ( :,: )   ! friction velocity
      REAL,    ALLOCATABLE, SAVE :: WSTAR  ( :,: )   ! friction velocity
      REAL,    ALLOCATABLE, SAVE :: MOLI   ( :,: )   ! inverse Monin-Obukhov Len
      REAL,    ALLOCATABLE, SAVE :: ZH     ( :,:,: ) ! mid-layer elevation
      REAL,    ALLOCATABLE, SAVE :: TA     ( :,:,: ) ! temperature (K)
      REAL,    ALLOCATABLE, SAVE :: QV     ( :,:,: ) ! water vapor mixing ratio
      REAL,    ALLOCATABLE, SAVE :: QC     ( :,:,: ) ! cloud water mixing ratio
      REAL,    ALLOCATABLE, SAVE :: PRES   ( :,:,: ) ! pressure

!     REAL,    ALLOCATABLE, SAVE :: MSFX2  ( :,: )   ! Squared map scale factors

      REAL,    ALLOCATABLE, SAVE :: PURB   ( :,: )   ! percent urban
      REAL,    ALLOCATABLE, SAVE :: KZMIN  ( :,:,: ) ! minimum Kz [m**2/s]

      REAL,    ALLOCATABLE, SAVE :: THETAV ( :,:,: ) ! potential temp
      REAL,    ALLOCATABLE, SAVE :: UWIND  ( :,:,: ) ! x-direction winds
      REAL,    ALLOCATABLE, SAVE :: VWIND  ( :,:,: ) ! y-direction winds
      REAL,    ALLOCATABLE, SAVE :: UVBUF  ( :,:,: ) ! U, V read buffer

      LOGICAL, SAVE :: MINKZ
      REAL,    SAVE :: CONVPA             ! Pressure conversion factor file units to Pa
      LOGICAL, SAVE :: CSTAGUV            ! Winds are available with C stagger?

      INTEGER, PRIVATE :: GXOFF, GYOFF            ! global origin offset from file
C for INTERPX
      INTEGER, PRIVATE, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER, PRIVATE, SAVE :: STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3
      INTEGER, PRIVATE, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2

      INTEGER, PRIVATE, SAVE :: MCOLS, MROWS
      REAL,    PRIVATE, SAVE :: P0        ! 1000 mb reference pressure

      CONTAINS
         FUNCTION VDIFF_MET_INIT ( JDATE, JTIME ) RESULT ( SUCCESS )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER JDATE, JTIME
         LOGICAL SUCCESS

         REAL, ALLOCATABLE :: X3M( : )   ! middle layer height
         INTEGER ASTAT
         CHARACTER( 16 ) :: PNAME = 'MET_INIT'
         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: UNITSCK
         CHARACTER( 96 ) :: XMSG = ' '

C for INTERPX
         INTEGER STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2

         INTEGER L, V
         INTEGER, SAVE :: LOGDEV


         LOGDEV = INIT3()

         SUCCESS = .TRUE.

C Get sigma coordinate variables
         ALLOCATE ( X3M  ( NLAYS ),
     &              RDX3M( NLAYS ),
     &              DX3F ( NLAYS ),
     &              RDX3F( NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating X3M, RDX3M, DX3F, or RDX3F'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         DO L = 1, NLAYS
            DX3F( L )  = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            RDX3F( L ) = 1.0 / DX3F( L )
            X3M( L ) = 0.5 * ( X3FACE_GD( L ) + X3FACE_GD( L-1 ) )
         END DO
         DO L = 1, NLAYS - 1
            RDX3M( L ) = 1.0 / ( X3M( L+1 ) - X3M( L ) )
         END DO
         RDX3M( NLAYS ) = 0.0

C Open the met files

         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
         CALL SUBHFILE ( MET_DOT_3D, GXOFF, GYOFF,
     &                   STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3 )

         ALLOCATE ( RDEPVHT( NCOLS,NROWS ),
     &              RJACM  ( NCOLS,NROWS,NLAYS ),
!    &              RVJACMF( NCOLS,NROWS,NLAYS ),
     &              RRHOJ  ( NCOLS,NROWS,NLAYS ),
     &              DENS1  ( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
!           XMSG = 'Failure allocating RDEPVHT, RJACM, RVJACMF, RRHOJ, or DENS1'
            XMSG = 'Failure allocating RDEPVHT, RJACM, RRHOJ, or DENS1'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE ( DENS  ( NCOLS,NROWS,NLAYS ),
     &              RJACF ( NCOLS,NROWS,NLAYS ),
!    &              MSFX2 ( NCOLS,NROWS ),
     &              RMSFX4( NCOLS,NROWS ), STAT = ASTAT ) ! reciprical of msfx2**2
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENS, RJACF, MSFX2, or RMSFX4'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         VNAME = 'MSFX2'
         IF ( .NOT. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, RMSFX4 ) ) THEN
            XMSG = 'Could not interpolate MSFX2 from ' // GRID_CRO_2D
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         RMSFX4 = 1.0 / ( RMSFX4 * RMSFX4 )

         ALLOCATE ( HOL   ( NCOLS,NROWS ),
     &              XPBL  ( NCOLS,NROWS ),
     &              LPBL  ( NCOLS,NROWS ),
     &              CONVCT( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating HOL, XPBL, LPBL, or CONVCT'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE ( PBL  ( NCOLS,NROWS ), 
     &              USTAR( NCOLS,NROWS ), 
     &              WSTAR( NCOLS,NROWS ), 
     &              MOLI ( NCOLS,NROWS ), 
     &              ZH   ( NCOLS,NROWS,NLAYS ),
     &              ZF   ( NCOLS,NROWS,0:NLAYS ),
     &              DZF  ( NCOLS,NROWS,NLAYS ),
     &              TA   ( NCOLS,NROWS,NLAYS ),
     &              QV   ( NCOLS,NROWS,NLAYS ),
     &              QC   ( NCOLS,NROWS,NLAYS ),
     &              PRES ( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PBL, USTAR, WSTAR, MOLI,'
     &           // ' ZH, ZF, DZF, TA, QV, QC, or PRES'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE ( THETAV( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating THETAV'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE ( UWIND( NCOLS+1,NROWS+1,NLAYS ), 
     &              VWIND( NCOLS+1,NROWS+1,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating UWIND or VWIND'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
            XMSG = 'Could not get ' // MET_CRO_3D // ' file description'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF         !  error abort if if desc3() failed

         V = INDEX1( 'PRES', NVARS3D, VNAME3D )
         IF ( V .NE. 0 ) THEN
            UNITSCK = UNITS3D( V )
         ELSE
            XMSG = 'Could not get variable PRES from ' // MET_CRO_3D
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( UNITSCK .EQ. 'PASCAL' .OR. UNITSCK .EQ. 'pascal' .OR.
     &        UNITSCK .EQ. 'Pascal' .OR. UNITSCK .EQ. 'PA'     .OR.
     &        UNITSCK .EQ. 'pa'     .OR. UNITSCK .EQ. 'Pa' ) THEN
            CONVPA = 1.0
            P0 = 100000.0
         ELSE IF ( UNITSCK .EQ. 'MILLIBAR' .OR. UNITSCK .EQ. 'millibar' .OR.
     &             UNITSCK .EQ. 'Millibar' .OR. UNITSCK .EQ. 'MB'       .OR.
     &             UNITSCK .EQ. 'mb'       .OR. UNITSCK .EQ. 'Mb' ) THEN
            CONVPA = 1.0E-02
            P0 = 1000.0
         ELSE IF ( UNITSCK .EQ. 'CENTIBAR' .OR. UNITSCK .EQ. 'centibar' .OR.
     &             UNITSCK .EQ. 'Centibar' .OR. UNITSCK .EQ. 'CB'       .OR.
     &             UNITSCK .EQ. 'cb'       .OR. UNITSCK .EQ. 'Cb' ) THEN
            CONVPA = 1.0E-03
            P0 = 100.0
         ELSE
            XMSG = 'Units incorrect on ' // MET_CRO_3D
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         MINKZ = .TRUE.   ! default
         MINKZ = ENVYN( 'KZMIN', 'Kz min on flag', MINKZ, ASTAT )
         IF ( ASTAT .NE. 0 ) WRITE( LOGDEV,'(5X, A)' ) 'Kz min on flag'
         IF ( ASTAT .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( ASTAT .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV,'(5X, A)' ) XMSG
         ELSE IF ( ASTAT .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV,'(5X, A)' ) XMSG
         END IF

         IF ( MINKZ ) THEN

            ALLOCATE ( PURB( NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PURB'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
            END IF

            VNAME = 'PURB'
            IF ( .NOT. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                          STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                          JDATE, JTIME, PURB ) ) THEN
               XMSG = 'Either make the data available from MCIP'
               WRITE( LOGDEV,'(/5X, A)' ) XMSG
               XMSG = 'or set the env var KZMIN to F or N,'
               WRITE( LOGDEV,'( 5X, A)' ) XMSG
               XMSG = 'in which case you will revert back to the'
               WRITE( LOGDEV,'( 5X, A)' ) XMSG
               XMSG = 'previous version of subroutine edyintb using Kz0UT'
               WRITE( LOGDEV,'( 5X, A, /)' ) XMSG
               XMSG = ' '
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

         ELSE IF ( .NOT. MINKZ ) THEN
            XMSG = 'This run uses Kz0UT, *NOT* KZMIN in subroutine edyintb.'
            WRITE( LOGDEV,'(/5X, A, /)' ) XMSG
         END IF   ! MINKZ

         ALLOCATE ( KZMIN( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating KZMIN'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         MCOLS = ENDCOLMD3 - STRTCOLMD3 + 1
         MROWS = ENDROWMD3 - STRTROWMD3 + 1

         ALLOCATE ( UVBUF( MCOLS,MROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating UVBUF'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         RETURN

         END FUNCTION VDIFF_MET_INIT

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

         SUBROUTINE GET_MET ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable RDEPVHT, RJACM, RRHOJ
C   14 Nov 03 J.Young: add reciprocal vertical Jacobian product for full and
C                      mid-layer
C   Tanya took JACOBF out of METCRO3D! Improvise
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   13 May 13 J.Young: moved rdmet.F here; added eddyx met accesses
C-----------------------------------------------------------------------

         USE GRID_CONF       ! horizontal & vertical domain specifications
         USE UTILIO_DEFN
#ifdef parallel
         USE SE_MODULES      ! stenex (using SE_COMM_MODULE)
#else
         USE NOOP_MODULES    ! stenex (using NOOP_COMM_MODULE)
#endif

         IMPLICIT NONE
 
C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters
         INCLUDE SUBST_PE_COMM   ! PE communication displacement and direction

C Arguments:
         INTEGER, INTENT( IN )  :: JDATE, JTIME, TSTEP  ! internal simulation date&time

C Parameters:
         REAL, PARAMETER :: KZMAXL = 500.0    ! upper limit for min Kz [m]
         REAL, PARAMETER :: KZ0UT  = 1.0      ! minimum eddy diffusivity [m**2/sec] KZ0
         REAL, PARAMETER :: KZL    = 0.01     ! lowest KZ
         REAL, PARAMETER :: KZU    = 1.0      ! 2.0  ! highest KZ
         REAL, PARAMETER :: EPS    = 1.0E-08  ! small number for temperature difference

C Local variables:

         INTEGER MDATE, MTIME
         INTEGER STEP
         REAL    UFRAC
         INTEGER LP
         REAL    TV, FINT

         CHARACTER( 16 ) :: PNAME = 'GET_MET'
         CHARACTER( 16 ) :: VNAME
         CHARACTER( 30 ) :: MSG1 = ' Error interpolating variable '
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER C, R, L                     ! loop induction variables

C-----------------------------------------------------------------------

C Interpolate file input variables and format for output

         MDATE  = JDATE
         MTIME  = JTIME
! Do not shift time; done in the caller
!        STEP   = TIME2SEC( TSTEP )
!        CALL NEXTIME( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

         VNAME = 'DENS'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, DENS ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT 1 )
         END IF

         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DENS1( C,R ) = DENS( C,R,1 )
            END DO
         END DO

         VNAME = 'JACOBM'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, RJACM ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'JACOBF'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
!    &                       MDATE, MTIME, JACF ) ) THEN
     &                       MDATE, MTIME, RJACF ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  RJACM( C,R,L ) = 1.0 / RJACM( C,R,L )
!                 RVJACMF( C,R,L ) = RJACM( C,R,L ) * RMSFX4( C,R ) / JACF( C,R,L )
                  RJACF( C,R,L ) = 1.0 / RJACF( C,R,L )
               END DO
            END DO
         END DO

         VNAME = 'DENSA_J'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, RRHOJ ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  RRHOJ( C,R,L ) = 1.0 / RRHOJ( C,R,L )
               END DO
            END DO
         END DO

C Try to read u-component wind on C-staggered grid from variable UWINDC.
C (First available in MCIPv3.5, Fall 2009.)  If UWINDC is not in MET_DOT_3D,
C try to read u-component wind on B-staggered grid as UWIND.

         VNAME = 'UWINDC'  ! trying to read C-staggered wind
         IF ( .NOT. INTERPX( MET_DOT_3D, VNAME, PNAME,
     &                       STRTCOLMD3,ENDCOLMD3, STRTROWMD3,ENDROWMD3, 1,NLAYS,
     &                       MDATE, MTIME, UVBUF ) ) THEN
            XMSG = 'Could not find ' // TRIM( VNAME ) // ' in ' // MET_DOT_3D
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            CSTAGUV = .FALSE.  ! cannot find C-staggered wind
            VNAME = 'UWIND'    ! trying to read B-staggered wind
            IF ( .NOT. INTERPX( MET_DOT_3D, VNAME, PNAME,
     &                          STRTCOLMD3,ENDCOLMD3, STRTROWMD3,ENDROWMD3, 1,NLAYS,
     &                          MDATE, MTIME, UVBUF ) ) THEN
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_DOT_3D
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         ELSE
            CSTAGUV = .TRUE.
         END IF

         UWIND = 0.0
         DO L = 1, NLAYS
            DO R = 1, MROWS
               DO C = 1, MCOLS
                  UWIND( C,R,L ) = UVBUF( C,R,L )
               END DO
            END DO
         END DO

C If u-component wind was C-staggered, read v-component wind on C-staggered
C grid.  Otherwise, read v-component wind from B-staggered grid.

         IF ( CSTAGUV ) THEN
            VNAME = 'VWINDC'  ! trying to read C-staggered wind
         ELSE
            VNAME = 'VWIND'   ! trying to read B-staggered wind
         END IF

         IF ( .NOT. INTERPX( MET_DOT_3D, VNAME, PNAME,
     &                       STRTCOLMD3,ENDCOLMD3, STRTROWMD3,ENDROWMD3, 1,NLAYS,
     &                       MDATE, MTIME, UVBUF ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_DOT_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VWIND = 0.0
         DO L = 1, NLAYS
            DO R = 1, MROWS
               DO C = 1, MCOLS
                  VWIND( C,R,L ) = UVBUF( C,R,L )
               END DO
            END DO
         END DO

C get ghost values for wind fields in case of free trop.
         CALL SUBST_COMM ( UWIND, DSPL_N0_E1_S0_W0, DRCN_E )
         CALL SUBST_COMM ( VWIND, DSPL_N1_E0_S0_W0, DRCN_N )

         VNAME = 'PBL'
         IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       MDATE, MTIME, PBL ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'USTAR'
         IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       MDATE, MTIME, USTAR ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'WSTAR'
         IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       MDATE, MTIME, WSTAR ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'MOLI'
         IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       MDATE, MTIME, MOLI ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'TA'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, TA ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'QV'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, QV ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'QC'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, QC ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'PRES'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, PRES ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'ZF'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, ZF ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'ZH'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       MDATE, MTIME, ZH ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Move 3rd dimension slabbed data from INTERP3 into proper order
C ( Using ZF both as a read buffer and an argument variable.)

         DO L = NLAYS, 1, -1
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  ZF( C,R,L ) = ZF( C,R,L-1 )
               END DO
            END DO
         END DO
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               ZF( C,R,0 ) = 0.0
            END DO
         END DO

         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DZF( C,R,L ) = ZF( C,R,L ) - ZF( C,R,L-1 )
               END DO
            END DO
         END DO

         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               RDEPVHT( C,R ) = 1.0 / ZF( C,R,1 )
            END DO
         END DO

         IF ( MINKZ ) THEN
            KZMIN = KZL
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     IF ( ZF( C,R,L ) .LE. KZMAXL ) THEN
                        UFRAC = 0.01 * PURB( C,R )
                        KZMIN( C,R,L ) = KZL + ( KZU - KZL ) * UFRAC
                     END IF
                  END DO
               END DO
            END DO
         ELSE
            KZMIN = KZ0UT
         END IF

         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  TV = TA( C,R,L ) * ( 1.0 + 0.608 * QV( C,R,L ) )
                  THETAV( C,R,L ) = TV * ( P0 / PRES( C,R,L ) ) ** 0.286
               END DO
            END DO
         END DO

         CONVCT = .FALSE.
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

               DO L = 1, NLAYS
                  IF ( PBL( C,R ) .LT. ZF( C,R,L ) ) THEN
                     LP = L
                     EXIT
                  END IF
               END DO

               LPBL( C,R ) = LP
               FINT = ( PBL( C,R ) - ZF( C,R,LP-1 ) )
     &              / ( ZF( C,R,LP ) - ZF( C,R,LP-1 ) )
               XPBL( C,R ) = FINT * ( X3FACE_GD( LP ) - X3FACE_GD( LP-1 ) )
     &                     + X3FACE_GD( LP-1 )
               HOL( C,R ) = PBL( C,R ) * MOLI( C,R )

               IF ( ( ( THETAV( C,R,1 ) - THETAV( C,R,2 ) ) .GT. EPS ) .AND.
     &              ( HOL( C,R ) .LT. -0.1 ) .AND.
     &              ( LPBL( C,R ) .GT. 3 ) ) CONVCT( C,R ) = .TRUE.

            END DO
         END DO

         END SUBROUTINE GET_MET

      END MODULE VDIFF_MET
