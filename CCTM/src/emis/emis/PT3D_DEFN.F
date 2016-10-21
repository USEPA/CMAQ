
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/PT3D_DEFN.F,v 1.12 2012/01/19 15:23:37 yoj Exp $

 
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PT3D_DEFN

C-----------------------------------------------------------------------
C Function: 3d point source emissions interface to the chemistry-transport model

C Revision History:
C     21 Nov 2007 J.Young: initial implementation
C     1  Jun 2010 David Wong: In subroutine GET_PT3D_EMIS, removed a IF-THEN
C                             block of code since the argument for TIME2SEC is in
C                             HHMMSS format rather than day format and also
C                             STKDATE is already set correctly
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     11 May 2011 David Wong: incorporated twoway model implementation
C     27 Jul 2011 David Wong: 1. in subroutine GET_PT3D_EMIS, use PRE_JDATE, and
C                                LOC_STKDATE to track change of date during a
C                                simulation when the start time is not 0, so
C                                correct data can be pulled by INTERPX routine
C                             2. used WINDOW logical variable (in subroutine
C                                PT3D_INIT) to determine x and y orig information
C                                and in subroutine READGC3, do not expand the
C                                STATCOL and STRTROw in the west and south direction
C     29 Oct 2011 J.Young: fix domain windowing
C     30 Apr 2012 C.Nolte: fix number of substeps, NTICS to be the number of
C                          synchronization time steps rather than the number of seconds. 
C     19 Dec 2013 D. Wong: used WRTIME, a robust way to determine when to write the 
C                          PT3D diagnostic file instead of relying on NTICS in particular
C                          for the twoway model where a time step can be sub-divided.
C     29 Jul 2014 D. Wong: check MET_CRO_3D and MET_DOT_3D separately for domain windowing
C     12 Aug 2015 D. Wong: - Used assumed shape array declaration and declared associated 
C                            subroutines in INTERFACE block
C                          - Replaced BMATVEC with BMATVECN which will call with a 1d or 
C                            2d argument subroutine by F90 poly-morphism feature
C                          - Based on the condition of MY_NSRC ( > 0 or not) to determine 
C                            execution of certain section of code or not
C     30 Apr 2016 J.Young: add multiple fire source capability, in affiliation with
C                          Yongtao Hu (Georgia Tech)
C     22 Jun 2016 B.Hutzell: Fixed issue where single emis species maps to multiple model
C                            species
C     08 Aug 2016 B.Murphy: Neglect fire emissions for pcVOC
C-----------------------------------------------------------------------

      USE UDTYPES           ! implementation for multiple groups of stack input files
      IMPLICIT NONE

      LOGICAL,              SAVE :: PT3DEMIS   ! flag in-lining plume rise
      INTEGER,              SAVE :: NPTGRPS    ! no. pt src input file groups
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_PT( :,:,:,: ) ! 3D pt src non-PM emis
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_PT_FIRE( :,:,:,: ) ! 3D pt src non-PM emis
      REAL,    ALLOCATABLE, SAVE :: PMEMIS_PT( :,:,:,: ) ! 3D pt src PM emis

      PUBLIC PT3DEMIS, NPTGRPS, VDEMIS_PT, VDEMIS_PT_FIRE, PMEMIS_PT,
     &       PT3D_INIT, GET_PT3D_EMIS
      PRIVATE

C Scenario start
      INTEGER,              SAVE :: SDATE = 0   ! Julian start date (YYYYDDD)
      INTEGER,              SAVE :: STIME = 0   ! start time (HHMMSS)

C Output layer fractions, dimensioned NSRC, emlays, and for diagnostic file
      REAL,    ALLOCATABLE, SAVE :: LFRAC( :,: ) ! per source local layer fractn
      REAL,    ALLOCATABLE, SAVE :: TFRAC( : )   ! Temporary LFRAC

C debugging zplume
      REAL,    ALLOCATABLE, SAVE :: ZPLUM( :,: ) ! has to be 2D for play file

C Stack emissions for sources within domain
      REAL,    ALLOCATABLE, SAVE :: STKEM( : )   ! per source local stk emis
      INTEGER,              SAVE :: EMLYRS       ! no. of emis layers

C Gas-phase Namelist emissions factors
      REAL,    ALLOCATABLE, SAVE :: EMIS_FAC( : )  ! per species scaling factor
      INTEGER,              SAVE :: N_EMIS_GAS_SPC ! number of gas-phase emissions species

C Wildfire emissions source parameters
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: ACRES( : )   ! acres burned
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: BFLUX( : )   ! Briggs buoyancy flux

C Vertical coord values
      REAL,    ALLOCATABLE, SAVE :: VGLVSXG( : )

C Play report log
      INTEGER, ALLOCATABLE, SAVE :: RDEV( : )
C Play files source id file
#ifdef srcid
      INTEGER, ALLOCATABLE, SAVE :: LDEV( : )
#endif
C Plume rise info report files
      CHARACTER( 13 ), ALLOCATABLE, SAVE :: REPTNAME( : )
      CHARACTER( 11 )                    :: REPSTR
C Minimum layer for reporting srcs w/ high plumes
      INTEGER, SAVE :: REP_LAYR

C Layer fractions matrix output files
      CHARACTER(  16 ), ALLOCATABLE, SAVE :: PLAYNAME( : )
C Layer fractions source id output files
      CHARACTER(  16 ), ALLOCATABLE, SAVE :: PLAY_SRCID_NAME( : )

C Get heights from GRID_CRO file, if true
      LOGICAL,              SAVE :: ZSTATIC = .TRUE.
C Write point source 3d emis diagnostic file, if true
      LOGICAL,              SAVE :: PT3DDIAG = .FALSE.
C Point source 3d emis diagnostic file names
!     CHARACTER( 16 ),               SAVE :: PT3DNAME
C Point source 3d emis diagnostic integral average write buffers
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_BUF( :,:,:,: )
      REAL,    ALLOCATABLE, SAVE :: PMEMIS_BUF( :,:,:,: )
C Diagnostic file avg factor
      REAL                       :: DIVFAC
C Write layer fractions diagnostic file, if true
      LOGICAL,              SAVE :: PT3DFRAC = .FALSE.

C Numerical flag for plume vertical spread method
      INTEGER,              SAVE :: IPVERT

C Stack parameters
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STK_DM( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STK_HT( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STK_TK( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STK_VEL( : )

C Private Parameters:
      CHARACTER(  10 ), PARAMETER :: BLANK10 = '          '

C cross-point layered grid file name
      CHARACTER(  16 ),     SAVE :: GC3NAME

C hourly stack emissions file names
      CHARACTER(  16 ), ALLOCATABLE, SAVE :: STKENAME( : )

      CHARACTER( 240 )           :: XMSG = ' '

      CONTAINS

C=======================================================================

         FUNCTION PT3D_INIT ( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP )
     &                       RESULT ( SUCCESS )

C use N_SPC_EMIS+1 for non-accessed emissions

         USE GRID_CONF   ! horizontal & vertical domain specifications
         USE CGRID_SPCS  ! CGRID mechanism species
         USE STK_PRMS    ! stack groups file
         USE STK_EMIS    ! hourly point source emissions
         USE PTMET       ! defines met variables used in plume rise
         USE PTBILIN     ! defines bi-linear interpolation from grid to pt src locations
         USE PTMAP       ! defines pt src species mapping to VDEMIS* arrays
         USE UTILIO_DEFN
         USE BMATVEC_MODULE

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER          N_SPC_EMIS ! total no. of model emissions species
         INTEGER          EMLAYS     ! number of emissions layers
         INTEGER          JDATE      ! Julian date (YYYYDDD)
         INTEGER          JTIME      ! time (HHMMSS)
         INTEGER          TSTEP      ! output time step
         LOGICAL          SUCCESS

C External Functions:
         INTEGER,         EXTERNAL :: SETUP_LOGDEV

C Parameters:

C Local Variables:
         CHARACTER( 16 ), SAVE :: CTM_PT3DEMIS = 'CTM_PT3DEMIS    ' ! env var for in-line
                                                                    ! 3d pt src emis
         CHARACTER( 16 )       :: PNAME = 'PT3D_INIT       ' ! procedure name
         CHARACTER( 16 )       :: VNAME    ! variable name buffer
         CHARACTER( 16 ), SAVE, ALLOCATABLE :: STKGNAME( : ) ! stack groups file name

         INTEGER          METCCOLS       ! cross point grid number of columns
         INTEGER          METCROWS       ! cross point grid number of rows
         INTEGER          METDCOLS       ! dot point grid number of columns
         INTEGER          METDROWS       ! dot point grid number of rows
         INTEGER          IOS            ! i/o and allocate memory status
         INTEGER, SAVE :: NSTEPS = 1
         INTEGER          NDATE          ! next timestep file Julian date (YYYYDDD)
         INTEGER          NTIME          ! next timestep file time (HHMMSS)

         INTEGER          LOGDEV

         INTEGER I, J, K, L, M, N, S, SRC, V  ! counters and indices

         INTEGER :: GXOFF, GYOFF
         INTEGER :: STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC

         INTERFACE
            SUBROUTINE DELTA_ZS( EMLYRS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )
               INTEGER, INTENT( IN )  :: EMLYRS, MY_NSRC
               INTEGER, INTENT( IN )  :: SRC_MAP( : )
               REAL,    INTENT( IN )  :: STKHT( : )
               REAL,    INTENT( IN )  :: ZF  ( :,: )
               REAL,    INTENT( OUT ) :: ZSTK( :,: )
               REAL,    INTENT( OUT ) :: DDZF( :,: )
            END SUBROUTINE DELTA_ZS
         END INTERFACE

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

!        LOGDEV = INIT3()
         LOGDEV = SETUP_LOGDEV()

C In-line 3D point source emissions?
         PT3DEMIS = ENVYN( 'CTM_PT3DEMIS',
     &                   'Flag for in-line 3d point source emissions',
     &                   .FALSE., IOS )

         IF ( PT3DEMIS ) THEN
            XMSG = 'Using in-line 3d point source emissions option'
            CALL M3MSG2( XMSG )
         ELSE
            RETURN
         END IF

C Get setting from environment variables
!        EMLAYS = ENVINT( 'SMK_EMLAYS', 'Number of emission layers', -1, IOS )
         EMLYRS = EMLAYS

         IPVERT = ENVINT( 'IPVERT', 'Method for vertical spread', 0, IOS )

C Cannot use default and cannot set to less than 4 because of limits of plume
C rise algorithm
         IF ( EMLYRS .LT. 4 ) THEN
            XMSG = 'Environment variable CTM_EMLAYS must be set to ' //
     &             'a number from 4 to the ' // CRLF() // BLANK10 //
     &             'number of layers in the meteorology inputs.'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

C get number of different file groups (sectors)

         NPTGRPS = ENVINT( 'NPTGRPS',
     &                     'Number of file groups',
     &                      1, IOS )

C get diagnostic files options

         PT3DDIAG = ENVYN( 'PT3DDIAG',
     &                     'generate diagnostic 3d pt emis file?',
     &                     .FALSE., IOS )

         PT3DFRAC = ENVYN( 'PT3DFRAC',
     &                     'generate diagnostic layer fraction file?',
     &                     .FALSE., IOS )

         REP_LAYR = ENVINT( 'REP_LAYER_MIN',
     &                      'Minimum layer for reporting plume rise info',
     &                      -1, IOS )

         IF ( IOS .EQ. 0 ) THEN

            IF ( REP_LAYR .LT. 1 ) THEN
               XMSG = 'NOTE: Environment variable REP_LAYR_MIN is ' //
     &                'less than 1.  Turning off reporting...'
            ELSE IF ( REP_LAYR .GT. EMLYRS ) THEN
               WRITE( XMSG,94010 )
     &                'NOTE: Environment variable REP_LAYR_MIN is '//
     &                'greater than the number of emissions ' //
     &                CRLF() // BLANK10 // 'layers (', EMLYRS, '). '//
     &                'Resetting to number of emissions layers.'
               REP_LAYR = EMLYRS
            ELSE
               WRITE( XMSG,94010 )
     &                'logging stack data for plume rise .ge. layer', REP_LAYR
            END IF

            CALL M3MSG2( XMSG )

         END IF

#ifdef srcid
         IF ( PT3DFRAC ) THEN   ! save local source id's
            ALLOCATE ( LDEV( NPTGRPS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'LDEV', PNAME )
            LDEV = 0   ! array
            WRITE( XMSG,94010 ) "Enter logical name for file containing " //
     &                          "source id's of play files"
            ALLOCATE ( PLAY_SRCID_NAME( NPTGRPS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'PLAY_SRCID_NAME', PNAME )
            DO N = 1, NPTGRPS
               WRITE( REPSTR,'( "SRCIDLAY", I2.2, "_" )' ) N
               WRITE( PLAY_SRCID_NAME( N ),'( A, I2.2 )' ) REPSTR, MYPE   ! limited to 99 PE's
               LDEV( N ) = PROMPTFFILE( XMSG, .FALSE., .TRUE.,
     &                                  PLAY_SRCID_NAME( N ), PNAME )
            END DO
         END IF
#endif

         ALLOCATE ( RDEV( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'RDEV', PNAME )
         RDEV = 0   ! array
         IF ( REP_LAYR .GT. 0 ) THEN
            WRITE( XMSG,94010 ) 'Enter logical name for report of ' //
     &                          'plumes exceeding layer', REP_LAYR
            ALLOCATE ( REPTNAME( NPTGRPS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'REPTNAME', PNAME )
            DO N = 1, NPTGRPS
               WRITE( REPSTR,'( "REPRTLAY", I2.2, "_" )' ) N
               WRITE( REPTNAME( N ),'( A, I2.2 )' ) REPSTR, MYPE   ! limited to 99 PE's
               RDEV( N ) = PROMPTFFILE( XMSG, .FALSE., .TRUE., REPTNAME( N ), PNAME )
            END DO
         END IF

C get scenario timing

         SDATE = ENVINT( 'LAYP_STDATE',
     &                   'Scenario Starting Date (YYYYDDD)',
     &                    2001188, IOS )

         STIME = ENVINT( 'LAYP_STTIME',
     &                   'Scenario Starting Time (HHMMSS)',
     &                    0, IOS )

         NSTEPS = ENVINT( 'LAYP_NSTEPS',
     &                    'Scenario Run Duration (integer steps)',
     &                     24, IOS )

         JDATE = SDATE
         JTIME = STIME

C open met files

         MC2NAME = PROMPTMFILE( 'Enter name for CROSS-POINT SURFACE MET file',
     &                          FSREAD3, 'MET_CRO_2D', PNAME )

         MC3NAME = PROMPTMFILE( 'Enter name for CROSS-POINT LAYERED MET file',
     &                          FSREAD3, 'MET_CRO_3D', PNAME )

         MD3NAME = PROMPTMFILE( 'Enter name for DOT-POINT LAYERED MET file',
     &                          FSREAD3, 'MET_DOT_3D', PNAME )

C Get grid parameters from 3-d cross-point met file and store header information.
C Use time parameters for time defaults.
         CALL RETRIEVE_IOAPI_HEADER( MC3NAME, JDATE, JTIME )

         CALL SUBHFILE ( MC3NAME, GXOFF, GYOFF,
     &                   STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC )

C Initialize reference grid with met file
C (declared and stored in PTBILIN module)
         XCELLCG = XCELL3D
         YCELLCG = YCELL3D
         METCCOLS = NCOLS3D
         METCROWS = NROWS3D
         METCXORIG = XORIG3D
         METCYORIG = YORIG3D

         IF ( ( GXOFF .GT. 0 ) .AND. ( GYOFF .GT. 0 ) ) THEN
            METCXORIG = XORIG3D + REAL( GXOFF,8 ) * XCELL3D
            METCYORIG = YORIG3D + REAL( GYOFF,8 ) * YCELL3D
         ELSE
            METCXORIG = XORIG3D
            METCYORIG = YORIG3D
         END IF

!!!!!!^^^^^^^!!!! THESE NEED TO BE CHECKED AGAINST GRIDDESC

C Get grid parameters from 3-d dot-point met file and store header information.
C (declared and stored in PTBILIN module)
         CALL RETRIEVE_IOAPI_HEADER( MD3NAME, JDATE, JTIME )
         XCELLDG = XCELL3D
         YCELLDG = YCELL3D
         METDXORIG = XORIG3D
         METDYORIG = YORIG3D
         METDCOLS = NCOLS3D
         METDROWS = NROWS3D

         CALL SUBHFILE ( MD3NAME, GXOFF, GYOFF,
     &                   STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC )

         IF ( ( GXOFF .GT. 0 ) .AND. ( GYOFF .GT. 0 ) ) THEN
            METDXORIG = XORIG3D + REAL( GXOFF,8 ) * XCELL3D
            METDYORIG = YORIG3D + REAL( GYOFF,8 ) * YCELL3D
         ELSE
            METDXORIG = XORIG3D
            METDYORIG = YORIG3D
         END IF

!!!!!^^^^^^^!!!! THESE NEED TO BE CHECKED AGAINST GRIDDESC

C Determine whether height information is time dependent or time independent.
C Non-hydrostatic is time-independent and hydrostatic is time-dependent.
         SELECT CASE( VGTYP_GD )
            CASE ( VGSGPH3, VGHVAL3, VGWRFEM )
               ZSTATIC = .FALSE.
            CASE ( VGSGPN3 )
               ZSTATIC = .TRUE.
            CASE DEFAULT
               WRITE( XMSG,94010 ) 'Cannot process vertical ' //
     &                             'coordinate type', VGTYP_GD
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
         END SELECT

         IF ( ZSTATIC ) THEN
            GC3NAME = PROMPTMFILE( 'Enter name for CROSS-POINT LAYERED GRID file',
     &                             FSREAD3, 'GRID_CRO_3D', PNAME )
         END IF

C Compare number of meteorology layers to number of emissions layers
         IF ( EMLYRS .LE. NLAYS ) THEN
            WRITE( XMSG,94010 ) 'NOTE: The number of emission layers ' //
     &                          'is', EMLYRS, ', and the maximum ' // CRLF()
     &                          // BLANK10 // 'possible layers is', NLAYS
            CALL M3MSG2( XMSG )
         ELSE
            WRITE( XMSG,94010 ) 'Resetting number of emission layers ' //
     &                          'from', EMLYRS, 'to number of ' // CRLF() //
     &                          BLANK10 // 'layers in the meteorology file,', NLAYS
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            EMLYRS = NLAYS
         END IF

         ALLOCATE( VGLVSXG( 0:MXLAYS3 ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VGLVSXG', PNAME )

C Store local layer information
         J = LBOUND( VGLVS3D, 1 )   ! F90 array intrinsic
         VGLVSXG( 0 ) = VGLVS3D( J )
         DO L = 1, NLAYS
            J = J + 1
            VGLVSXG( L ) = VGLVS3D( J )
         END DO

         IF ( NSTEPS .GT. MXREC3D ) THEN
            WRITE( XMSG,94010 ) 'Requested output time steps > those on met file '
     &                          // CRLF() // BLANK10 // 'reset to ', MXREC3D
            NSTEPS = MXREC3D
         END IF

C Get stack data
         ALLOCATE ( STKGNAME( NPTGRPS ), STAT = IOS )   ! stk parms files array
         CALL CHECKMEM( IOS, 'STKGNAME', PNAME )
         STKGNAME = ' '   ! array

         DO N = 1, NPTGRPS
            WRITE( VNAME,'( "STK_GRPS_",I2.2 )' ) N
            STKGNAME( N ) = PROMPTMFILE( 'Enter name for stack groups file',
     &                                   FSREAD3, VNAME, PNAME )
         END DO

         IF ( .NOT. STK_PRMS_INIT( STKGNAME ) ) THEN
            XMSG = 'Could not initialize stack parameters'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

#ifdef srcid
         IF ( PT3DFRAC ) THEN
            DO N = 1, NPTGRPS
               WRITE( LDEV( N ),93053 ) N, MY_NSRC( N ), MY_STRT_SRC( N )
               DO S = 1, MY_NSRC( N )
                  WRITE( LDEV( N ),93057 ) S, SOURCE( N )%ARRY( S ) + MY_STRT_SRC( N ) - 1
               END DO
            END DO
         END IF
#endif

C Initialize stack emissions data
         ALLOCATE ( STKENAME( NPTGRPS ), STAT = IOS )   ! stk emis files array
         CALL CHECKMEM( IOS, 'STKENAME', PNAME )
         STKENAME = ' '   ! array

         DO N = 1, NPTGRPS
            WRITE( VNAME,'( "STK_EMIS_",I2.2 )' ) N
            STKENAME( N ) = PROMPTMFILE( 'Enter name for stack emissions file',
     &                                   FSREAD3, VNAME, PNAME )
         END DO

         IF ( .NOT. STK_EMIS_INIT( STKENAME, JDATE, JTIME ) ) THEN
            XMSG = 'Could not initialize stack parameters'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         DO N = 1, NPTGRPS
            IF ( NSRC( N ) .NE. NSRC_EMIS( N ) ) THEN
               WRITE( LOGDEV,* ) 'NSRC vs. NSRC_EMIS:', N, NSRC( N ), NSRC_EMIS( N )
               XMSG = 'No. of sources for stack files don''t match'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
         END DO

         IF ( .NOT. PTMAP_INIT( NPTGRPS ) ) THEN
            XMSG = 'Could not get point source mappings'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE ( VDEMIS_PT( NCOLS,NROWS,EMLYRS,N_GSPC_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT', PNAME )
         VDEMIS_PT = 0.0   ! array assignment

         ALLOCATE ( VDEMIS_PT_FIRE( NCOLS,NROWS,EMLYRS,N_GSPC_EMIS), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT_FIRE', PNAME )
         VDEMIS_PT_FIRE = 0.0   ! array assignment

         N_EMIS_GAS_SPC = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS
         ALLOCATE ( EMIS_FAC( N_EMIS_GAS_SPC ), STAT = IOS )
         CALL CHECKMEM( IOS, 'EMIS_FAC', PNAME )
         EMIS_FAC = 1.0   ! array assignment (default)

         ALLOCATE ( PMEMIS_PT( NCOLS,NROWS,EMLYRS,N_SPC_PTPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PMEMIS_PT', PNAME )
         PMEMIS_PT = 0.0   ! array

C Set up and open 3d point source emissions diagnostic file?
         IF ( PT3DDIAG ) THEN
            NDATE = SDATE; NTIME = STIME
            CALL NEXTIME( NDATE, NTIME, TSTEP ) ! advance one output tstep
            CALL OPPT3D_DIAG( CTM_PT3D_DIAG, NDATE, NTIME, TSTEP, EMLYRS,
     &                        N_GSPC_EMIS, GSPC_EMIS, N_ASPC_EMIS, ASPC_EMIS )
            ALLOCATE ( VDEMIS_BUF( NCOLS,NROWS,EMLYRS,N_GSPC_EMIS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'VDEMIS_BUF', PNAME )
            VDEMIS_BUF = 0.0   ! array
            ALLOCATE ( PMEMIS_BUF( NCOLS,NROWS,EMLYRS,N_ASPC_EMIS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'PM_EMIS_BUF', PNAME )
            PMEMIS_BUF = 0.0   ! array
            WRITE( LOGDEV,'(/5X, A /5X, A )' )
     &                        'Recording 3d point source emissions diagnostics',
!    &                        'as an integral average over the output timestep'
     &                        'as a linear average over the output timestep'
         END IF

C Open diagnostic layer fractions file?
         IF ( PT3DFRAC ) THEN
            ALLOCATE ( PLAYNAME( NPTGRPS ), STAT = IOS )   ! stk parms files array
            CALL CHECKMEM( IOS, 'PLAYNAME', PNAME )
            PLAYNAME = ' '   ! array
            NDATE = SDATE; NTIME = STIME
            CALL NEXTIME( NDATE, NTIME, TSTEP ) ! advance one output tstep
            CALL OPENLAYOUT( NDATE, NTIME, TSTEP, EMLYRS, VGLVSXG, NPTGRPS,
     &                       MY_NSRC, PLAYNAME )
         END IF

         IF ( .NOT. PTMET_INIT ( NPTGRPS, EMLYRS ) ) THEN
            XMSG = 'Could not initialize stack met data'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE( TFRAC( EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'TFRAC', PNAME )
         TFRAC = 0.0   ! array

         ALLOCATE( STK_DM( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STK_DM', PNAME )

         ALLOCATE( STK_HT( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STK_HT', PNAME )

         ALLOCATE( STK_TK( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STK_TK', PNAME )

         ALLOCATE( STK_VEL( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STK_VEL', PNAME )

         ALLOCATE( ACRES (NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'ACRES', PNAME )

         ALLOCATE( BFLUX (NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'BFLUX', PNAME )

         DO N = 1, NPTGRPS

C Allocate and set stack parameters

            IF ( MY_NSRC( N ) .GT. 0 ) THEN
               ALLOCATE( STK_DM( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
               CALL CHECKMEM( IOS, 'STK_DM', PNAME )
               STK_DM( N )%ARRY = 0.0   ! 1D array
               STK_DM( N )%LEN = MY_NSRC( N )

               ALLOCATE( STK_HT( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
               CALL CHECKMEM( IOS, 'STK_HT', PNAME )
               STK_HT( N )%ARRY = 0.0   ! 1D array
               STK_HT( N )%LEN = MY_NSRC( N )

               ALLOCATE( STK_TK( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
               CALL CHECKMEM( IOS, 'STK_TK', PNAME )
               STK_TK( N )%ARRY = 0.0   ! 1D array
               STK_TK( N )%LEN = MY_NSRC( N )

               ALLOCATE( STK_VEL( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
               CALL CHECKMEM( IOS, 'STK_VEL', PNAME )
               STK_VEL( N )%ARRY = 0.0   ! 1D array
               STK_VEL( N )%LEN = MY_NSRC( N )
   
               DO S = 1, MY_NSRC( N )
                  SRC = SOURCE( N )%ARRY( S )
                  STK_DM( N )%ARRY( S )  = STKDIAM( N )%ARRY( SRC )
                  STK_HT( N )%ARRY( S )  = STKHT( N )%ARRY( SRC )
                  STK_TK( N )%ARRY( S )  = STKTK( N )%ARRY( SRC )
                  STK_VEL( N )%ARRY( S ) = STKVEL( N )%ARRY( SRC )
               END DO

C If wildfires, allocate fire parameters
               IF ( FIREFLAG( N ) ) THEN

                  ALLOCATE( ACRES( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
                  CALL CHECKMEM( IOS, 'ACRES', PNAME )
                  ACRES( N )%ARRY  = 0.0   ! 1D array
                  ACRES( N )%LEN = MY_NSRC( N )

                  DO S = 1, MY_NSRC( N )
                     SRC = SOURCE( N )%ARRY( S )
                     ACRES( N )%ARRY( S ) = ACRES_BURNED( N )%ARRY( SRC )
                  END DO

                  ALLOCATE( BFLUX( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
                  CALL CHECKMEM( IOS, 'BFLUX', PNAME )
                  BFLUX( N )%ARRY = 0.0   ! 1D array
                  BFLUX( N )%LEN = MY_NSRC( N )

               END IF
            END IF   ! MY_NSRC > 0

         END DO

         IF ( .NOT. PTBILIN_INIT ( NPTGRPS ) ) THEN
            XMSG = 'Could not initialize stack met bilinear interpolation data'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( ZSTATIC ) THEN ! Read time-independent ZF and ZH for non-hydrost Met data

            DO N = 1, NPTGRPS

C Compute per-source heights

               IF ( MY_NSRC( N ) .GT. 0 ) THEN
C Get un-gridding matrices for cross and dot point met data
                  CALL RETRIEVE_IOAPI_HEADER( GC3NAME, JDATE, JTIME )
                  CALL READGC3 ( 'X3HT0M', SDATE, STIME, XBUF )
                  CALL BMATVECN( METCGRID, MY_NSRC( N ), EMLYRS,
     &                          BILIN_DATA( N )%NX,
     &                          BILIN_DATA( N )%CX,
     &                          XBUF, PTMET_DATA( N )%ZH )

                  CALL READGC3 ( 'X3HT0F', SDATE, STIME, XBUF )
                  CALL BMATVECN( METCGRID, MY_NSRC( N ), EMLYRS,
     &                          BILIN_DATA( N )%NX,
     &                          BILIN_DATA( N )%CX,
     &                          XBUF, PTMET_DATA( N )%ZF )

C Compute ZSTK, DDZF

                  CALL DELTA_ZS( EMLYRS, MY_NSRC( N ),
     &                           SOURCE( N )%ARRY, STKHT( N )%ARRY,
     &                           PTMET_DATA( N )%ZF,
     &                           PTMET_DATA( N )%ZSTK,
     &                           PTMET_DATA( N )%DDZF )
               END IF ! MY_NSRC( N ) > 0

            END DO   ! NPTGRPS

         END IF   ! ZSTATIC

         SUCCESS = .TRUE.; RETURN

93053    FORMAT("#  Src Id's for play file", I2, ', with', I10, ' total sources',
     &          ", and my_strt_src:", I10 )
93057    FORMAT( I6, I8 )
94010    FORMAT( 12( A, :, I8, :, 1X ) )

         END FUNCTION PT3D_INIT

C=======================================================================

         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

! Revision History.
!     Aug 12, 15 D. Wong: added code to handle parallel I/O implementation

C-----------------------------------------------------------------------

C Time step part of laypoint

         USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
         USE GRID_CONF   ! horizontal & vertical domain specifications
         USE CGRID_SPCS  ! CGRID mechanism species
         USE STK_PRMS    ! stack groups file
         USE STK_EMIS    ! hourly point source emissions
         USE PTMET, P_D => PTMET_DATA
         USE PTMAP       ! defines pt src species mapping to VDEMIS* arrays
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER               :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )

C Parameters:
         REAL,            PARAMETER :: USTARMIN = 0.1   ! Min valid value for USTAR
         REAL,            PARAMETER :: CONVPA = 1.0E-2  ! convert Pa to mb
         REAL,            PARAMETER :: BTU2M4PS3 = 9.2752E-03 ! convert BTU/s to m**4/s**3
         REAL,            PARAMETER :: BFACPHR = BTU2M4PS3 / 3600.0 ! m**4/s**3 per hour
                                 ! assumes heat flux from fire pt src file is in BTU/hr

C External functions:
         INTEGER,         EXTERNAL :: SETUP_LOGDEV
         LOGICAL,         EXTERNAL :: WRITE3_DISTR   ! calls WRITE3 directly

C Local variables:
         INTEGER          CDATE      ! previous step start date
         INTEGER          CTIME      ! previous step start time
         INTEGER, SAVE :: LDATE = 0  ! previous date
         INTEGER          LBOT       ! layer containing plume bottom
         INTEGER          LTOP       ! layer containing plume top
         INTEGER          LPBL       ! first L: ZF(L) above mixing layer - ONLY for REPORT
         INTEGER          LSTK       ! first L: ZF(L) > STKHT
         INTEGER          SRC        ! source pointer in this proc sub-domain
         REAL             LFRC       ! intermediate LFRAC
         REAL             MV         ! mininum LFRAC 
         REAL             PSFC       ! surface pressure [Pa]
         REAL             TSTK       ! temperature at top of stack [K]
         REAL             TSUM       ! tmp layer frac sum for renormalizing
         REAL             WSTK       ! wind speed  at top of stack [m/s]
         REAL             ZBOT       ! plume bottom elevation [m]
         REAL             ZTOP       ! plume top    elevation [m]
         REAL             ZDIFF      ! ZTOP - ZBOT
         REAL             DDZ        ! 1 / ZDIFF
         REAL             ZPLM       ! plume centerline height above stack [m]
         REAL             USTMP      ! temp storage for ustar [m/s]
         REAL             HFLX       ! converted heat flux
         REAL             BESIZE     ! effective size of the fire buoyant core
         REAL             SFRACT     ! smoldering fraction size (1-BESIZE)

         CHARACTER(  8 ) :: CINT     ! integer to character buffer for warning messages
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS   '   ! procedure name
         CHARACTER( 16 ) :: VNAME    ! variable name buffer

         INTEGER          IOS        ! i/o and allocate memory status
         INTEGER          L, S, V    ! counters
         INTEGER          C, R, I, J, K, N

         INTEGER, SAVE :: WSTEP = 0
         INTEGER          WDATE, WTIME
         LOGICAL, SAVE :: WRTIME
         INTEGER, SAVE :: NTICS = 0      ! no. of substeps within an output tstep
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, SAVE :: LOGDEV

         INTEGER, SAVE :: PRE_JDATE, LOC_STKDATE

         INTERFACE
           SUBROUTINE PREPLM( FIREFLG, EMLAYS, HMIX, HTS, PSFC, TS, DDZF, QV,
     &                        TA, UW, VW, ZH, ZF, PRES, LSTK, LPBL, TSTK,
     &                        WSTK, DTHDZ, WSPD )
             LOGICAL, INTENT( IN )  :: FIREFLG
             INTEGER, INTENT( IN )  :: EMLAYS
             REAL,    INTENT( IN )  :: HMIX
             REAL,    INTENT( IN )  :: HTS
             REAL,    INTENT( IN )  :: PSFC
             REAL,    INTENT( IN )  :: TS
             REAL,    INTENT( IN )  :: DDZF( : )
             REAL,    INTENT( IN )  :: QV  ( : )
             REAL,    INTENT( IN )  :: TA  ( : )
             REAL,    INTENT( IN )  :: UW  ( : )
             REAL,    INTENT( IN )  :: VW  ( : )
             REAL,    INTENT( IN )  :: ZH  ( : )
             REAL,    INTENT( IN )  :: ZF  ( : )
             REAL,    INTENT( IN )  :: PRES( 0: )
             INTEGER, INTENT( OUT ) :: LSTK
             INTEGER, INTENT( OUT ) :: LPBL
             REAL,    INTENT( OUT ) :: TSTK
             REAL,    INTENT( OUT ) :: WSTK
             REAL,    INTENT( OUT ) :: DTHDZ( : )
             REAL,    INTENT( OUT ) :: WSPD ( : )
           END SUBROUTINE PREPLM

           SUBROUTINE PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                        STKDM, STKHT, STKTK, STKVE,
     &                        TSTK, USTAR, DTHDZ, TA, WSPD,
     &                        ZF, ZH, ZSTK, WSTK, ZPLM )
             INTEGER, INTENT( IN )  :: EMLAYS
             INTEGER, INTENT( IN )  :: LSTK
             REAL,    INTENT( IN )  :: HFX
             REAL,    INTENT( IN )  :: HMIX
             REAL,    INTENT( IN )  :: STKDM
             REAL,    INTENT( IN )  :: STKHT
             REAL,    INTENT( IN )  :: STKTK
             REAL,    INTENT( IN )  :: STKVE
             REAL,    INTENT( IN )  :: TSTK
             REAL,    INTENT( IN )  :: USTAR
             REAL,    INTENT( IN )  :: DTHDZ( : )
             REAL,    INTENT( IN )  :: TA   ( : )
             REAL,    INTENT( IN )  :: WSPD ( : )
             REAL,    INTENT( IN )  :: ZF ( 0:  )
             REAL,    INTENT( IN )  :: ZH   ( : )
             REAL,    INTENT( IN )  :: ZSTK ( : )
             REAL,    INTENT( INOUT ) :: WSTK
             REAL,    INTENT( OUT ) :: ZPLM 
           END SUBROUTINE PLMRIS

           SUBROUTINE FIRE_PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                             BFLX, TSTK, USTAR, DTHDZ,
     &                             TA, WSPD, ZF, WSTK, ZPLM )
             INTEGER, INTENT( IN )  :: EMLAYS
             INTEGER, INTENT( IN )  :: LSTK
             REAL,    INTENT( IN )  :: HFX
             REAL,    INTENT( IN )  :: HMIX
             REAL,    INTENT( IN )  :: BFLX
             REAL,    INTENT( IN )  :: TSTK
             REAL,    INTENT( IN )  :: USTAR
             REAL,    INTENT( IN )  :: DTHDZ( : )
             REAL,    INTENT( IN )  :: TA   ( : )
             REAL,    INTENT( IN )  :: WSPD ( : )
             REAL,    INTENT( IN )  :: ZF ( 0:  )
             REAL,    INTENT( INOUT ) :: WSTK
             REAL,    INTENT( OUT ) :: ZPLM
           END SUBROUTINE FIRE_PLMRIS

           SUBROUTINE PLSPRD( DTHDZ, ZF, KZ, CEFSTK, PLTOP, PLBOT )
             REAL,    INTENT ( IN ) :: DTHDZ( : )
             REAL,    INTENT ( IN ) :: ZF( 0:  )
             INTEGER, INTENT ( IN ) :: KZ
             REAL,    INTENT ( IN ) :: CEFSTK
             REAL,    INTENT( OUT ) :: PLTOP
             REAL,    INTENT( OUT ) :: PLBOT
           END SUBROUTINE PLSPRD

         END INTERFACE

#ifdef Verbose1
         real mxzplm
#endif

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            LOGDEV = SETUP_LOGDEV()
            PRE_JDATE = JDATE

#ifdef parallel_io
            IF ( PT3DDIAG ) THEN
               IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_PT3D_DIAG, FSREAD3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM(CTM_PT3D_DIAG)
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                  END IF
               END IF
            END IF
#endif
         END IF

C For each time step, compute the layer fractions...

         WRITE( XMSG,'(A, I7.6)' )
     &   'Calculating emissions point source layer fractions for', JTIME
         WRITE( LOGDEV,* ) ' '
         CALL M3MSG2( XMSG )

         IF ( LDATE .NE. JDATE ) THEN  ! write day and date to stdout & XMSG
            CALL WRDAYMSG( JDATE, XMSG )
            LDATE = JDATE
            WRITE( LOGDEV,'(9X, "at time ", A)' ) HHMMSS( JTIME )
         END IF
         WRTIME = .FALSE.
         WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
         IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
            WDATE = JDATE; WTIME = JTIME
            CALL NEXTIME( WDATE, WTIME, TSTEP( 2 ) )
            IF ( .NOT. CURRSTEP( WDATE, WTIME, SDATE, STIME, TSTEP( 1 ),
     &                           CDATE, CTIME ) ) THEN
               XMSG = 'Cannot get step date and time'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
            WDATE = CDATE; WTIME = CTIME
            WSTEP = 0
            WRTIME = .TRUE.
            DO N = 1, NPTGRPS
               IF ( RDEV( N ) .GT. 0 ) THEN    ! write day, date and time to report file
                  WRITE( CINT,'( I8 )' ) REP_LAYR
                  CALL WRDAYMSG( WDATE, XMSG )
                  XMSG = XMSG( 1:LEN_TRIM( XMSG ) )
     &                    // ' at time ' // HHMMSS( WTIME )
     &                    // 'for ltop .ge. ' // TRIM( ADJUSTL( CINT ) )
                  WRITE( RDEV( N ),93000 ) XMSG( 1:LEN_TRIM( XMSG ) )
               END IF
            END DO
         END IF

C Convert pt source met data to bi-linear interpolated data
         CALL PTMET_CONVT( JDATE, JTIME, NPTGRPS, EMLYRS, ZSTATIC )

         VDEMIS_PT = 0.0   ! array assignment
         VDEMIS_PT_FIRE = 0.0   ! array assignment
         PMEMIS_PT = 0.0   ! array assignment

         DO N = 1, NPTGRPS

            IF ( MY_NSRC( N ) .LE. 0 ) CYCLE

            ALLOCATE( STKEM( NVARS_EMIS( N ) ), STAT = IOS )
            CALL CHECKMEM( IOS, 'STKEM', PNAME )
            STKEM = 0.0   ! 1-D array assignment
            IF ( PT3DFRAC ) THEN   ! Store layer fractions
               ALLOCATE( LFRAC( MY_NSRC( N ),EMLYRS ), STAT = IOS )
               CALL CHECKMEM( IOS, 'LFRAC', PNAME )
               ALLOCATE( ZPLUM( MY_NSRC( N ),EMLYRS ), STAT = IOS )
               CALL CHECKMEM( IOS, 'ZPLUM', PNAME )
               LFRAC = 0.0   ! 2-D array - Initialize layer fraction
               ZPLUM = 0.0   ! 2-D array - Initialize plume centroid height
            END IF

C Ensure that the model and stack groups emissions timestamp dates stay synchronized

            IF ( PRE_JDATE .NE. JDATE ) THEN
               LOC_STKDATE = STKDATE( N ) + 1
            ELSE
               LOC_STKDATE = STKDATE( N )
            END IF

            DO V = 1, NVARS_EMIS( N )
               VNAME = STKSPC( N )%ARRY( V )
               IF ( .NOT. INTERPX( STKENAME( N ), VNAME, PNAME,
     &                             1,1, MY_STRT_SRC( N ),MY_END_SRC( N ), 1,1,
     &                             LOC_STKDATE, JTIME,
     &                             STKEMIS( N )%ARRY( 1,V ) ) ) THEN
                  XMSG = 'Could not read ' // TRIM( VNAME ) //
     &                   ' from ' // TRIM( STKENAME( N ) )
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

               ! Read Non-Carbon Organic Matter too if POC is Requested
               IF ( ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 )
     &               .AND. ( VNAME .EQ. 'POC' ) ) THEN
                  STKEMIS_TOO( N )%ARRY = 0.0
                  IF ( .NOT. INTERPX( STKENAME( N ), 'PNCOM', PNAME,
     &                                1,1, MY_STRT_SRC( N ),MY_END_SRC( N ), 1,1,
!    &                                STKDATE( N ), JTIME,
     &                                LOC_STKDATE, JTIME,
     &                                STKEMIS_TOO( N )%ARRY( 1,V ) ) ) THEN
                     XMSG = 'Could not read ' // TRIM( VNAME ) //
     &                    ' from ' // TRIM( STKENAME( N ) )
                     CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
                  STKEMIS( N )%ARRY( 1,V ) = STKEMIS( N )%ARRY( 1,V ) 
     &                                     + STKEMIS_TOO( N )%ARRY( 1,V )
               END IF
            END DO

            IF ( FIREFLAG( N ) ) THEN
               DO V = 1, NVARS_FIRE
                  VNAME = FIRENAM( N )%ARRY( V )
                  IF ( .NOT. INTERPX( STKENAME( N ), VNAME, PNAME,
     &                                1,1, MY_STRT_SRC( N ),MY_END_SRC( N ), 1,1,
     &                                LOC_STKDATE, JTIME,
     &                                FIREVAR( N )%ARRY( 1,V ) ) ) THEN
                     XMSG = 'Could not read ' // TRIM( VNAME ) //
     &                      ' from ' // TRIM( STKENAME( N ) )
                     CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
                  ! Read Non-Carbon Organic Matter too if POC is Requested
                  IF ( ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 )
     &                  .AND. VNAME .EQ. 'POC' ) THEN
                     FIREVAR_TOO( N )%ARRY = 0.0
                     IF ( .NOT. INTERPX( STKENAME( N ), 'PNCOM', PNAME,
     &                                   1,1, MY_STRT_SRC( N ),MY_END_SRC( N ), 1,1,
     &                                   LOC_STKDATE, JTIME,
     &                                   FIREVAR_TOO( 1 )%ARRY( 1,V ) ) ) THEN
                        XMSG = 'Could not read ' // TRIM( VNAME ) //
     &                         ' from ' // TRIM( STKENAME( N ) )
                        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                     END IF
                     FIREVAR( N )%ARRY( 1,V ) = FIREVAR( N )%ARRY( 1,V )
     &                                        + FIREVAR_TOO( N )%ARRY( 1,V )
                  END IF
               END DO
               DO S = 1, MY_NSRC( N )
                  SRC = SOURCE( N )%ARRY( S )
                  BFLUX( N )%ARRY( S ) = FIREVAR( N )%ARRY( SRC,1 ) * BTU2M4PS3
               END DO
            END IF

C Loop through sources and compute plume rise

#ifdef Verbose1
            mxzplm = 0.0
#endif

            DO S = 1, MY_NSRC( N )

               IF ( FIREFLAG( N ) ) THEN
                  IF ( BFLUX( N )%ARRY( S ) .LE. 0.0 ) THEN
                     TFRAC( 1 ) = 1.0
                     TFRAC( 2:EMLYRS ) = 0.0
                     LTOP = 1
                     GO TO 201
                  END IF
               END IF

               P_D( N )%ZZF( 0 ) = 0.0
               P_D( N )%ZZF( 1:EMLYRS ) = P_D( N )%ZF( 1:EMLYRS,S )  ! array of layers

C Get pressures: Use SIGMA values and surface pres. p=sigma*(psfc-ptop)+ptop
               PSFC = P_D( N )%PRSFC( S )
               DO L = 0, EMLYRS
                  P_D( N )%PRESF( L ) = ( VGLVSXG( L ) * ( PSFC - VGTOP_GD )
     &                                +   VGTOP_GD ) * CONVPA
               END DO

C Set surface pressure (convert to mb from Pa)
               PSFC = CONVPA * PSFC

C Compute derived met vars needed before layer assignments
               CALL PREPLM( FIREFLAG( N ), EMLYRS,
     &                      P_D( N )%HMIX( S ), STK_HT( N )%ARRY( S ), PSFC,
     &                      P_D( N )%TSFC( S ),    P_D( N )%DDZF( :,S ),
     &                      P_D( N )%QV( :,S ),    P_D( N )%TA( :,S ),
     &                      P_D( N )%UWIND( :,S ), P_D( N )%VWIND( :,S ),
     &                      P_D( N )%ZH( :,S ),    P_D( N )%ZF( :,S ),
     &                      P_D( N )%PRESF, LSTK,  LPBL, TSTK, WSTK,
     &                      P_D( N )%DTHDZ,        P_D( N )%WSPD )

C Trap USTAR at a minimum realistic value
               USTMP = MAX( P_D( N )%USTAR( S ), USTARMIN )

C Convert heat flux (watts/m2 to m K /s )
               HFLX = P_D( N )%HFX( S ) / ( CPD * P_D( N )%DENS( 1,S ) )

               IF ( .NOT. FIREFLAG( N ) ) THEN
                  CALL PLMRIS( EMLYRS, LSTK, HFLX, P_D( N )%HMIX( S ),
     &                         STK_DM( N )%ARRY( S ), STK_HT( N )%ARRY( S ),
     &                         STK_TK( N )%ARRY( S ), STK_VEL( N )%ARRY( S ),
     &                         TSTK, USTMP,
     &                         P_D( N )%DTHDZ,     P_D( N )%TA( :,S ),
     &                         P_D( N )%WSPD,      P_D( N )%ZZF,
     &                         P_D( N )%ZH( :,S ), P_D( N )%ZSTK( :,S ),
     &                         WSTK, ZPLM )
               ELSE
                  CALL FIRE_PLMRIS( EMLYRS, LSTK, HFLX, P_D( N )%HMIX( S ),
     &                         BFLUX( N )%ARRY( S ),
     &                         TSTK, USTMP,
     &                         P_D( N )%DTHDZ,     P_D( N )%TA( :,S ),
     &                         P_D( N )%WSPD,      P_D( N )%ZZF,
     &                         WSTK, ZPLM )
               END IF

#ifdef Verbose1
               if ( zplm .gt. mxzplm ) mxzplm = zplm
#endif

C Determine the bottom and top heights of the plume.
               IF ( IPVERT .EQ. 0 ) THEN
C Default Turner approach.  Plume thickness = amount of plume rise
C Plume rise DH = ZPLM minus the stack height STKHT
                  IF ( FIREFLAG( N ) ) THEN
                     ZTOP = 1.5 * ZPLM 
                     ZBOT = 0.5 * ZPLM
                  ELSE
                     ZTOP = STK_HT( N )%ARRY( S )
     &                    + 1.5 * ( ZPLM - STK_HT( N )%ARRY( S ) )
                     ZBOT = STK_HT( N )%ARRY( S )
     &                    + 0.5 * ( ZPLM - STK_HT( N )%ARRY( S ) )
                  END IF
               ELSE
C Alternative method to compute plume top/bot heights
                  CALL PLSPRD( P_D( N )%DTHDZ, P_D( N )%ZZF, EMLYRS,
     &                         ZPLM, ZTOP, ZBOT )
               END IF

C Set up for computing plume fractions, assuming uniform distribution in pressure
C (~mass concentration -- minor hydrostatic assumption) from bottom to top.

               IF ( ZTOP .LT. STK_HT( N )%ARRY( S ) ) THEN
                  WRITE( CINT,'( I8 )' ) S
                  WRITE( XMSG,94010 ) 'ERROR: Top of plume is less than '
     &                                // 'top of stack for source:' // CINT
                  CALL M3MESG( XMSG )
                  WRITE( LOGDEV,* ) '    Zbot: ', ZBOT, ' Ztop: ', ZTOP
                  WRITE( LOGDEV,* ) '    Stack Top: ', STK_HT( N )%ARRY( S ),
     &                              ' Plume Top: ', ZPLM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

C Allocate plume to layers (compute layer plume fractions)

!              CALL POSTPLM( EMLYRS, ZBOT, ZTOP, PRESF, ZZF, TA( 1,S ), ZH( 1,S ),
!    &                       LTOP, TFRAC )

C Compute LBOT, LTOP such that
C  ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C  ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )

               DO L = 1, EMLYRS - 1
                  IF ( ZBOT .LE. P_D( N )%ZZF( L ) ) THEN
                     LBOT = L
                     GO TO  122
                  ELSE
                     TFRAC( L ) = 0.0             ! fractions below plume
                  END IF
               END DO
               LBOT = EMLYRS                      !  fallback

#ifdef Verbose1
               c = my_stkcol( n )%arry( s ); r = my_stkrow( n )%arry( s )
            write( logdev,'(a, 4i4)' ) 'Layer top at c,r, for file, source: ', c, r, n, s
#endif

122            CONTINUE                           !  loop exit:  bottom found at LBOT

               IF ( ZTOP .LE. P_D( N )%ZZF( LBOT ) ) THEN  ! plume in this layer

                  TFRAC( LBOT ) = 1.0
                  LTOP = LBOT

                  DO L = LBOT + 1, EMLYRS         ! fractions above plume
                     TFRAC( L ) = 0.0
                  END DO

               ELSE IF ( LBOT .EQ. EMLYRS ) THEN  ! plume above top layer

                  TFRAC( LBOT ) = 1.0

                  DO L = 1, EMLYRS - 1            ! fractions below plume
                     TFRAC( L ) = 0.0
                  END DO

               ELSE                               ! plume crosses layers

                  DO L = LBOT + 1, EMLYRS
                     IF ( ZTOP .LE. P_D( N )%ZZF( L ) ) THEN
                        LTOP = L
                        GO TO 126
                     END IF
                  END DO
                  LTOP = EMLYRS                   !  fallback

#ifdef Verbose1
            write( logdev,'(a, 4i4)' ) '> Hit top at c,r, for file, source: ', c, r, n, s
#endif
126               CONTINUE

                  ZDIFF = ZTOP - ZBOT
                  IF ( ZDIFF .GT. 0.0 ) THEN

                     DDZ  = 1.0 / ZDIFF
                     TFRAC( LBOT ) = DDZ * ( P_D( N )%ZZF( LBOT ) - ZBOT )
                     TFRAC( LTOP ) = DDZ * ( ZTOP - P_D( N )%ZZF( LTOP-1 ) )

                  ELSE   ! ZDIFF .le. 0

                     WRITE( CINT,'( I8 )' ) S
                     WRITE( XMSG,94020 )
     &                  'Infinitely small plume created for source:,'
     &                  // CINT // CRLF() // BLANK10
     &                  // 'All emissions put in first layer.'
                     CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                     LBOT = 1; LTOP = 1
                     TFRAC( LBOT ) = 1.0

                  END IF

                  DO L = LBOT + 1, LTOP - 1       ! layers in plume
                     TFRAC( L ) = DDZ * ( P_D( N )%ZZF( L ) - P_D( N )%ZZF( L-1 ) )
                  END DO

                  DO L = LTOP + 1, EMLYRS         ! fractions above plume
                     TFRAC( L ) = 0.0
                  END DO

               END IF

C If layer fractions are negative, put in the first layer
               MV = MINVAL( TFRAC( 1:EMLYRS ) )
               IF ( MV .LT. 0.0 ) THEN

                  WRITE( CINT,'( I8 )' ) S
                  WRITE( XMSG,94010 ) 'WARNING: One or more negative plume '
     &                     // 'fractions found for source:' // CINT
     &                     // CRLF() // BLANK10 // 'Plume reset to '
     &                     // 'put all emissions in surface layer.'
                  CALL M3MESG( XMSG )

                  TFRAC( 1 ) = 1.0
                  TFRAC( 2:EMLYRS ) = 0.0

               END IF

               IF ( FIREFLAG( N ) ) THEN
C If source is a fire, adjust for fire smoldering effects (include fractions below LBOT)
C calculate smoldering fraction:

                  IF ( ACRES( N )%ARRY ( S ) .NE. 0.0 ) THEN
                     BESIZE = 0.0703 * LOG( ACRES( N )%ARRY( S ) ) + 0.3
                     BESIZE = MIN( BESIZE, 1.0 )
                     SFRACT = 1.0 - BESIZE
                  ELSE
                     SFRACT = 1.0
                     write( logdev,* ) "we can't get here: ", n, s
                  END IF

                  ZDIFF = ZBOT - P_D( N )%ZZF( 0 )   ! Note: ZDIFF redefined
                  IF ( ZDIFF .GT. 0.0 ) THEN
                     DDZ  = 1.0 / ZDIFF
                     DO L = 1, LBOT - 1
                       TFRAC( L ) = DDZ * ( P_D( N )%ZZF( L ) - P_D( N )%ZZF( L-1 ) )
     &                            * SFRACT
                     END DO
                     TFRAC( LBOT ) = TFRAC( LBOT )
     &                             + ( DDZ * ( ZBOT - P_D( N )%ZZF( LBOT-1 ) )
     &                               - TFRAC( LBOT ) ) * SFRACT
                  ELSE   ! LBOT = 1
!                    TFRAC( 1:LBOT-1 ) = 0.0
                     TFRAC( LBOT ) = TFRAC( LBOT ) * ( 1.0 - SFRACT )
                  END IF

                  DO L = LBOT + 1, LTOP
                     TFRAC( L ) = TFRAC( L ) * ( 1.0 - SFRACT )
                  END DO

               END IF

201            CONTINUE

C Apportion emissions to the layers

               SRC = SOURCE( N )%ARRY( S )
               STKEM( : ) = STKEMIS( N )%ARRY( SRC,: )   ! array of species

               C = MY_STKCOL( N )%ARRY( S ); R = MY_STKROW( N )%ARRY( S )
               DO L = 1, EMLYRS
                  LFRC = TFRAC( L )
                  IF ( LFRC .LE. 0.0 ) CYCLE

                  DO V = 1, STKEM_MAP( N )%LEN
                     I = GAS_MAP( N )%ARRY( V )
                     J = STKEM_MAP( N )%ARRY( V )
                     VDEMIS_PT( C,R,L,I ) = VDEMIS_PT( C,R,L,I )
!    &                                    + LFRC * STKEM( J )
     &                                    + LFRC * EMIS_FAC( V ) * STKEM( J )
                     ! Save the fire emission rate of POC (+PNCOM) so
                     ! that we can neglect the fire emissions for pcVOC
                     ! in GET_EMIS.
                     IF ( FIREFLAG( N ) ) THEN
                        IF ( STKSPC( N )%ARRY( J ) .EQ. 'POC' ) THEN
                           VDEMIS_PT_FIRE( C,R,L,I ) = VDEMIS_PT_FIRE( C,R,L,I )
     &                                               + LFRC * EMIS_FAC( V ) * STKEM( J ) 
                        END IF
                     END IF
                  END DO
                  DO V = 1, STKPM_MAP( N )%LEN
                     I = PM_MAP( N )%ARRY( V )
                     J = STKPM_MAP( N )%ARRY( V )
                     PMEMIS_PT( C,R,L,I ) = PMEMIS_PT( C,R,L,I )
     &                                    + LFRC * STKEM( J )   ! emis fac applied in AERO_EMIS
                  END DO

               END DO

               IF ( PT3DFRAC ) THEN   ! Store layer fractions
                  LFRAC( S,1:EMLYRS ) = TFRAC( 1:EMLYRS )  ! array
                  ZPLUM( S,1 ) = ZPLM
               END IF

C Possible report to REPRTLAY
               IF ( LTOP .GE. REP_LAYR .AND. RDEV( N ) .GT. 0 .AND. WRTIME ) THEN
                  WRITE( RDEV( N ),93048 ) S, STK_HT( N )%ARRY( S ), LPBL,
     &                                     LBOT, LTOP, STK_VEL( N )%ARRY( S ), WSTK,
     &                                     STK_TK( N )%ARRY( S ), TSTK
               END IF

            END DO    ! end loop on sources S

            IF ( PT3DFRAC .AND. WRTIME ) THEN     ! Write out layer fractions
               IF ( .NOT. WRITE3_DISTR( PLAYNAME( N ), 'LFRAC', WDATE, WTIME,
     &                                  MY_NSRC( N ), EMLYRS, LFRAC ) ) THEN
                  XMSG = 'Error writing "LFRAC" to file "' //
     &                    PLAYNAME( N )( 1:LEN_TRIM( PLAYNAME( N ) ) ) // '."'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( .NOT. WRITE3_DISTR( PLAYNAME( N ), 'ZPLUME', WDATE, WTIME,
     &                                  MY_NSRC( N ), EMLYRS, ZPLUM ) ) THEN
                  XMSG = 'Error writing "ZPLUM" to file "' //
     &                    PLAYNAME( N )( 1:LEN_TRIM( PLAYNAME( N ) ) ) // '."'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
            WRITE( LOGDEV,94030 )
     &            'Timestep written to', TRIM( PLAYNAME( N ) ),
     &            'for date and time', WDATE, WTIME
            END IF

#ifdef Verbose1
            write( logdev,* ) '   Max ZPLM(', n, ' ):', mxzplm
#endif

            DEALLOCATE( STKEM )
            IF ( PT3DFRAC ) THEN
               DEALLOCATE( LFRAC )
               DEALLOCATE( ZPLUM )
            END IF

         END DO   ! NPTGRPS

         IF ( PT3DDIAG ) THEN  ! store 3d point source emissions
            NTICS = NTICS + 1
            VDEMIS_BUF = VDEMIS_BUF + VDEMIS_PT   ! array assignmnt
            PMEMIS_BUF = PMEMIS_BUF + PMEMIS_PT   ! array assignmnt
#ifdef Verbose1
            write( logdev,* ) '   ntics, jtime: ', ntics, jtime
#endif
            IF ( WRTIME ) THEN
               DIVFAC = 1.0 / FLOAT( NTICS )
#ifdef Verbose1
               write( logdev,* ) '   ntics, divfac: ', ntics, divfac
#endif
               DO V = 1, N_GSPC_EMIS
                  VDEMIS_BUF( :,:,:,V ) = DIVFAC * VDEMIS_BUF( :,:,:,V )
                  IF ( .NOT. WRITE3( CTM_PT3D_DIAG, GSPC_EMIS( V ),
     &                               WDATE, WTIME, VDEMIS_BUF( :,:,:,V ) ) ) THEN
                     XMSG = 'Error writing "VDEMIS_PT" to '
     &                    // CTM_PT3D_DIAG( 1:LEN_TRIM( CTM_PT3D_DIAG ) )
                     CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
               END DO
               DO V = 1, N_ASPC_EMIS
                  PMEMIS_BUF( :,:,:,V ) = DIVFAC * PMEMIS_BUF( :,:,:,V )
                  IF ( .NOT. WRITE3( CTM_PT3D_DIAG, ASPC_EMIS( V ),
     &                               WDATE, WTIME, PMEMIS_BUF( :,:,:,V ) ) ) THEN
                     XMSG = 'Error writing "PMEMIS_PT" to '
     &                    // CTM_PT3D_DIAG( 1:LEN_TRIM( CTM_PT3D_DIAG ) )
                     CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
               END DO
               WRITE( LOGDEV,94030 )
     &               'Timestep written to', CTM_PT3D_DIAG,
     &               'for date and time', WDATE, WTIME
            VDEMIS_BUF = 0.0   ! array
            PMEMIS_BUF = 0.0   ! array
            NTICS = 0
            END IF
         END IF

         RETURN

C------------------  FORMAT  STATEMENTS   ------------------------------

93000    FORMAT( A / 1X, 'source', 3X, 'ht', 3X, 'lpbl', 1X, 'lbot', 1X, 'ltop',
     &               3X, 'vel', 4X, 'windspeed', 2X, 'StkTemp', 3X, 'AmbTemp' )

93040    FORMAT( 'Sources with top of plume greater than layer', I3, //,
     &           'Src ID, ', A, ', H[m], ', 'V[m/s], ', 'Ts[K], ',
     &           'Ta[K], ', 'U[m/s], ', 'LPBL, ', 'LTOP' )

93042    FORMAT( '( I6, ",", I6.6, ",", A', I2.2, ', ","', I2.2, '(A',
     &           I2.2, ',", ") , F6.1, ", ", F6.2, ", ", F6.1, ", ",',
     &           'F5.1, ", ", F6.2, ", ", I3, ", ", I3 )' )

93044    FORMAT( I6, ",", I6.6, F6.1, ", ", F6.2, ", ", F6.1, ", ",
     &           F5.1, ", ", F6.2, ", ", I3, ", ", I3 )

93048    FORMAT( I6, F8.2, 3I4, 4F10.2 )

94010    FORMAT( 12( A, :, I8, :, 1X ) )
94020    FORMAT( 10( A, :, I7, :, 1X ) )
94030    FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

         END SUBROUTINE GET_PT3D_EMIS

C-------------- Subprograms used by GET_PT3D_EMIS ----------------------

         SUBROUTINE RETRIEVE_IOAPI_HEADER( FNAME, JDATE, JTIME )

            USE UTILIO_DEFN

            IMPLICIT NONE

            CHARACTER( * ) :: FNAME
            INTEGER        :: JDATE, JTIME

            CHARACTER( 16 )  :: PNAME = 'RETRVE_IOAPI_HDR'   ! procedure name
            CHARACTER( 120 ) :: XMSG = ' '

            IF ( .NOT. DESC3( FNAME ) ) THEN
               XMSG = 'Could not get description of file "' //
     &                FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            RETURN

         END SUBROUTINE RETRIEVE_IOAPI_HEADER

C-----------------------------------------------------------------------

         SUBROUTINE READGC3 ( VNAME, JDATE, JTIME, XBUF )

            USE VGRD_DEFN             ! vertical layer specifications
            USE PTBILIN, ONLY: XBND, YBND
            USE UTILIO_DEFN

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID  ! file name parameters

            CHARACTER( * ) :: VNAME     ! variable name
            INTEGER           JDATE     ! Julian date
            INTEGER           JTIME     ! time
            REAL              XBUF( * ) ! read buffer

            INTEGER      GXOFF, GYOFF   ! global origin offset from file
C for INTERPX
            INTEGER, SAVE   :: STRTCOL, ENDCOL, STRTROW, ENDROW

            INTEGER, SAVE   :: LAY1, LAY2
            INTEGER            L1
            CHARACTER( 16 ) :: PNAME = 'READGC3'   ! procedure name
            LOGICAL, SAVE   :: FIRSTIME = .TRUE.

            integer, save :: logdev
            integer, external :: setup_logdev

            IF ( FIRSTIME )  THEN
               FIRSTIME = .FALSE.
               logdev = setup_logdev()

               GRID_CRO_3D = GC3NAME

               CALL SUBHFILE ( GRID_CRO_3D, GXOFF, GYOFF,
     &                         STRTCOL, ENDCOL, STRTROW, ENDROW )

!              IF ( .NOT. XBND( 1 ) ) STRTCOL = STRTCOL - 1
               IF ( .NOT. XBND( 2 ) ) ENDCOL  = ENDCOL + 1
!              IF ( .NOT. YBND( 1 ) ) STRTROW = STRTROW - 1
               IF ( .NOT. YBND( 2 ) ) ENDROW  = ENDROW + 1

               LAY1 = 1; LAY2 = NLAYS

            END IF

            IF ( .NOT. INTERPX( GRID_CRO_3D, VNAME, PNAME,
     &                          STRTCOL,ENDCOL, STRTROW,ENDROW, LAY1,LAY2,
     &                          JDATE, JTIME, XBUF ) ) THEN

               L1 = LEN_TRIM( VNAME )
               XMSG = 'Could not read ' // VNAME( 1:L1 ) //
     &                ' from file GRID_CRO_3D'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            RETURN

         END SUBROUTINE READGC3

C-----------------------------------------------------------------------

      END MODULE PT3D_DEFN

