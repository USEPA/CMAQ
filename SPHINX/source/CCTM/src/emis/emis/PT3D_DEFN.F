
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
C     16 Aug 2017 B.Murphy: Pull plume height calc. to separate subroutine.
C     01 Feb 2019 D.Wong: Implemented centralized I/O approach and removed everything 
C                         related to ZSTATIC
C     05 June 2019 F. Sidi: Bugfix related to Parallel I/O Implementation
C     05 Aug  2019 D. Wong: Removed USE BMATVEC_MODULE statement and PTBILIIN
C     19 Sep  2019 D. Wong: Fixed the way to advance to next day on an emission file, DESID_STREAM_DATE
C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: RARRY1

      IMPLICIT NONE


      PUBLIC PT3D_INIT, GET_PT3D_EMIS
      PRIVATE

C Output layer fractions, dimensioned NSRC, DESID_LAYS, and for diagnostic file
      REAL,    ALLOCATABLE, SAVE :: LFRAC( :,: ) ! per source local layer fractn
      REAL,    ALLOCATABLE, SAVE :: TFRAC( :,: ) ! Temporary LFRAC

C debugging zplume
      REAL,    ALLOCATABLE, SAVE :: ZPLUM( :,: ) ! has to be 2D for play file

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

C Layer fractions matrix output files
      CHARACTER(  16 ), ALLOCATABLE, SAVE :: PLAYNAME( : )
C Layer fractions source id output files
      CHARACTER(  16 ), ALLOCATABLE, SAVE :: PLAY_SRCID_NAME( : )

C Point source 3d emis diagnostic file names
!     CHARACTER( 16 ),               SAVE :: PT3DNAME
C Point source 3d emis diagnostic integral average write buffers
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_BUF( :,:,:,: )
C Diagnostic file avg factor
      REAL                       :: DIVFAC


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

         FUNCTION PT3D_INIT ( JDATE, JTIME, TSTEP )
     &                       RESULT ( SUCCESS )

         USE GRID_CONF, ONLY: NLAYS, VGTYP_GD
         USE STK_PRMS, ONLY: SOURCE, STK_PRMS_INIT, MY_NSRC
         USE STK_EMIS, ONLY: DESID_STREAM_NAME, NSRC_EMIS, FIREFLAG,
     &                       MAP_PTtoISRM, STK_EMIS_INIT
         USE PTMET, ONLY: PTMET_DATA, MC2NAME, MC3NAME, MD3NAME, PTMET_INIT
         USE PTBILIN, ONLY: NPTGRPS, 
     &                      METCXORIG, METCYORIG, XCELLCG, YCELLCG, 
     &                      METDXORIG, METDYORIG, XCELLDG, YCELLDG

         USE UTILIO_DEFN
         USE RUNTIME_VARS, ONLY: LOGDEV, MYPE, PT3DFRAC, PT_DATE, STDATE, STTIME,
     &                           PT_TIME, PT_NSTEPS, REP_LAYR
         USE DESID_VARS, ONLY: DESID_LAYS, DESID_MAXLAYS
         USE stack_group_data_module
         USE centralized_io_module, only : window

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER          JDATE      ! Julian date (YYYYDDD)
         INTEGER          JTIME      ! time (HHMMSS)
         INTEGER          TSTEP      ! output time step
         LOGICAL          SUCCESS

C Parameters:

C Local Variables:
         CHARACTER( 16 )       :: PNAME = 'PT3D_INIT       ' ! procedure name
         CHARACTER( 16 )       :: VNAME    ! variable name buffer
!        CHARACTER( 16 ), SAVE, ALLOCATABLE :: STKGNAME( : ) ! stack groups file name

         INTEGER          METCCOLS       ! cross point grid number of columns
         INTEGER          METCROWS       ! cross point grid number of rows
         INTEGER          METDCOLS       ! dot point grid number of columns
         INTEGER          METDROWS       ! dot point grid number of rows
         INTEGER          IOS            ! i/o and allocate memory status
         INTEGER          NDATE          ! next timestep file Julian date (YYYYDDD)
         INTEGER          NTIME          ! next timestep file time (HHMMSS)

         INTEGER I, J, K, L, M, N, P, S, SRC, V, ISRM  ! counters and indices

         INTEGER :: GXOFF, GYOFF
         INTEGER :: STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC

         INTERFACE
            SUBROUTINE DELTA_ZS( DESID_LAYS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )
               INTEGER, INTENT( IN )  :: DESID_LAYS, MY_NSRC
               INTEGER, INTENT( IN )  :: SRC_MAP( : )
               REAL,    INTENT( IN )  :: STKHT( : )
               REAL,    INTENT( IN )  :: ZF  ( :,: )
               REAL,    INTENT( OUT ) :: ZSTK( :,: )
               REAL,    INTENT( OUT ) :: DDZF( :,: )
            END SUBROUTINE DELTA_ZS
            SUBROUTINE OPENLAYOUT( NDATE, NTIME, TSTEP, EMLAYS, VGLVSXG, NPTGRPS,
     &                             MY_NSRC, PLAYNAME )
               INTEGER,        INTENT( IN )  :: NDATE
               INTEGER,        INTENT( IN )  :: NTIME
               INTEGER,        INTENT( IN )  :: TSTEP
               INTEGER,        INTENT( IN )  :: EMLAYS
               REAL,           INTENT( IN )  :: VGLVSXG( 0: )
               INTEGER,        INTENT( IN )  :: NPTGRPS
               INTEGER,        INTENT( IN )  :: MY_NSRC( : )
               CHARACTER( * ), INTENT( OUT ) :: PLAYNAME( : )
            END SUBROUTINE OPENLAYOUT
         END INTERFACE

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

C In-line 3D point source emissions?
         IF ( NPTGRPS .GT. 0 ) THEN
            XMSG = 'Using in-line 3d point source emissions option'
            CALL M3MSG2( XMSG )
         ELSE
            RETURN
         END IF


C Cannot use default and cannot set to less than 4 because of limits of plume
C rise algorithm
         IF ( DESID_MAXLAYS .GT. 0 .AND. DESID_MAXLAYS .LT. 4 ) THEN
            XMSG = 'Environment variable CTM_DESID_LAYS must be set to ' //
     &             'a number from 4 to the ' // CRLF() // BLANK10 //
     &             'number of layers in the meteorology inputs.'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
C get diagnostic files options
         IF ( REP_LAYR .LT. 1 ) THEN
            XMSG = 'NOTE: Environment variable REP_LAYR_MIN is ' //
     &             'less than 1.  Turning off reporting...'
         ELSE IF ( REP_LAYR .GT. DESID_LAYS ) THEN
            WRITE( XMSG,94010 )
     &             'NOTE: Environment variable REP_LAYR_MIN is '//
     &             'greater than the number of emissions ' //
     &             CRLF() // BLANK10 // 'layers (', DESID_LAYS, '). '//
     &             'Resetting to number of emissions layers.'
            REP_LAYR = DESID_LAYS
         ELSE
            WRITE( XMSG,94010 )
     &             'logging stack data for plume rise .ge. layer', REP_LAYR

         END IF

         CALL M3MSG2( XMSG )
#ifndef mpas
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

         PT_DATE = JDATE 
         PT_TIME = JTIME

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

C Compare number of meteorology layers to number of emissions layers
!         IF ( DESID_LAYS .LE. NLAYS ) THEN
!            WRITE( XMSG,94010 ) 'NOTE: The number of emission layers ' //
!     &                          'is', DESID_LAYS, ', and the maximum ' // CRLF()
!     &                          // BLANK10 // 'possible layers is', NLAYS
!            CALL M3MSG2( XMSG )
!         ELSE
!            WRITE( XMSG,94010 ) 'Resetting number of emission layers ' //
!     &                          'from', DESID_LAYS, 'to number of ' // CRLF() //
!     &                          BLANK10 // 'layers in the meteorology file,', NLAYS
!            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
!            DESID_LAYS = NLAYS
!         END IF

         ALLOCATE( VGLVSXG( 0:MXLAYS3 ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VGLVSXG', PNAME )

C Store local layer information
         J = LBOUND( VGLVS3D, 1 )   ! F90 array intrinsic
         VGLVSXG( 0 ) = VGLVS3D( J )
         DO L = 1, NLAYS
            J = J + 1
            VGLVSXG( L ) = VGLVS3D( J )
         END DO

         IF ( PT_NSTEPS .GT. MXREC3D ) THEN
            WRITE( XMSG,94010 ) 'Requested output time steps > those on met file '
     &                          // CRLF() // BLANK10 // 'reset to ', MXREC3D
            PT_NSTEPS = MXREC3D
         END IF

C Get stack data
!        ALLOCATE ( STKGNAME( NPTGRPS ), STAT = IOS )   ! stk parms files array
!        CALL CHECKMEM( IOS, 'STKGNAME', PNAME )
!        STKGNAME = ' '   ! array

!        DO N = 1, NPTGRPS
!           WRITE( VNAME,'( "STK_GRPS_",I3.3 )' ) N
!           STKGNAME( N ) = PROMPTMFILE( 'Enter name for stack groups file',
!    &                                   FSREAD3, VNAME, PNAME )
!        END DO

!        IF ( .NOT. STK_PRMS_INIT( STKGNAME ) ) THEN
!           XMSG = 'Could not initialize stack parameters'
!           CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
!           SUCCESS = .FALSE.; RETURN
!        END IF
#endif

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

         ! Retrive Point Source Filenames from Emissions Filename Vector
         DO N = 1, NPTGRPS
            ISRM = MAP_PTtoISRM( N )
            STKENAME( N ) = DESID_STREAM_NAME( ISRM )
         END DO

         ! Initialize Point Source Emissions
         IF ( .NOT. STK_EMIS_INIT( STKENAME, JDATE, JTIME ) ) THEN
            XMSG = 'Could not initialize stack parameters'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ! Check that the number of individual sources on each point
         ! emissions file is consistent.
         DO N = 1, NPTGRPS
            IF ( NSRC( N ) .NE. NSRC_EMIS( N ) ) THEN
               WRITE( LOGDEV,* ) 'NSRC vs. NSRC_EMIS:', N, NSRC( N ), NSRC_EMIS( N )
               XMSG = 'No. of sources for stack files don''t match'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
         END DO

C Open diagnostic layer fractions file?
         IF ( PT3DFRAC ) THEN
            ALLOCATE ( PLAYNAME( NPTGRPS ), STAT = IOS )   ! stk parms files array
            CALL CHECKMEM( IOS, 'PLAYNAME', PNAME )
            PLAYNAME = ' '   ! array
            NDATE = PT_DATE; NTIME = PT_TIME
            CALL NEXTIME( NDATE, NTIME, TSTEP ) ! advance one output tstep
            CALL OPENLAYOUT( NDATE, NTIME, TSTEP, DESID_LAYS, VGLVSXG, NPTGRPS,
     &                       MY_NSRC, PLAYNAME )
         END IF

         IF ( .NOT. PTMET_INIT ( ) ) THEN
            XMSG = 'Could not initialize stack met data'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

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
#ifdef mpas
                  SRC = S
#else
                  SRC = SOURCE( N )%ARRY( S )
#endif
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
#ifdef mpas
                     SRC = S
#else
                     SRC = SOURCE( N )%ARRY( S )
#endif
                     ACRES( N )%ARRY( S ) = ACRES_BURNED( N )%ARRY( SRC )
                  END DO

                  ALLOCATE( BFLUX( N )%ARRY( MY_NSRC( N ) ), STAT = IOS )
                  CALL CHECKMEM( IOS, 'BFLUX', PNAME )
                  BFLUX( N )%ARRY = 0.0   ! 1D array
                  BFLUX( N )%LEN = MY_NSRC( N )

               END IF
            END IF   ! MY_NSRC > 0

         END DO

         SUCCESS = .TRUE.; RETURN

93053    FORMAT("#  Src Id's for play file", I2, ', with', I10, ' total sources',
     &          ", and my_strt_src:", I10 )
93057    FORMAT( I6, I8 )
94010    FORMAT( 12( A, :, I8, :, 1X ) )

         END FUNCTION PT3D_INIT

C=======================================================================

         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, EMVAR_PT, ISRM,
     &                            VDEMIS_PT, PTLAYS, L_DESID_DIAG )

! Revision History.
!     Aug 12, 15 D. Wong: added code to handle parallel I/O implementation
!     Jun 05, 19 F. Sidi: Bugfix related to Parallel I/O Implementation

!-----------------------------------------------------------------------

! Time step part of laypoint

         USE GRID_CONF, ONLY:  NCOLS, NROWS, IO_PE_INCLUSIVE
         USE STK_PRMS, ONLY :  MY_NSRC, MY_STRT_SRC, MY_END_SRC, 
     &                         MY_STKCOL, MY_STKROW, SOURCE,
#ifdef mpas
     &                         MY_STKBUFF, MY_COLBUFF, MY_ROWBUFF, my_nsrc_mesh_index
#else
     &                         MY_STKBUFF, MY_COLBUFF, MY_ROWBUFF
#endif
         USE STK_EMIS, ONLY :  IPSRM, DESID_N_ISTR, DESID_EMVAR, DESID_STREAM_DATE, 
     &                         STKBUFF, NSRC_EMIS
         USE PRECURSOR_DATA, ONLY : PRECURSOR_MW, SULF_IDX
         USE RUNTIME_VARS, ONLY : LOGDEV, PT3DDIAG, PT3DFRAC, STDATE, STTIME
         USE CENTRALIZED_IO_MODULE
         USE UTILIO_DEFN
#ifdef mpas
         use util_module, only : nextime
         USE PTMET, ONLY    :  DESID_LAYS, PTMET_CONVT_MPAS
#else
         USE PTMET, ONLY    :  DESID_LAYS, PTMET_CONVT
#endif

         IMPLICIT NONE

! Includes:
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

! Arguments:
         CHARACTER(16), INTENT( IN ) :: EMVAR_PT( : )
         INTEGER               :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )
         INTEGER, INTENT( IN ) :: ISRM
         INTEGER, INTENT( OUT) :: PTLAYS
         REAL, INTENT(INOUT)  :: VDEMIS_PT( :,:,:,: ) 

! Parameters:

         LOGICAL,         EXTERNAL :: WRITE3_DISTR   ! calls WRITE3 directly

! Local variables:
         INTEGER          CDATE      ! previous step start date
         INTEGER          CTIME      ! previous step start time
         INTEGER, SAVE :: LDATE = 0  ! previous date
         INTEGER          LTOP       ! layer containing plume top
         INTEGER          SRC        ! source pointer in this proc sub-domain
         INTEGER          MY_DELTA   ! total number of sources on this processor
         REAL             LFRC       ! intermediate LFRAC
         REAL             TSUM       ! tmp layer frac sum for renormalizing
         REAL, ALLOCATABLE, SAVE :: MY_STKEMIS( :,:,: )

         CHARACTER(  8 ) :: CINT     ! integer to character buffer for warning messages
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS   '   ! procedure name
         CHARACTER( 16 ) :: VNAME    ! variable name buffer

         INTEGER          IOS        ! i/o and allocate memory status
         INTEGER          L, S, V    ! counters
         INTEGER          C, R, I, J, K, N, ISPC

         INTEGER, SAVE :: WSTEP = 0
         INTEGER          WDATE, WTIME
         LOGICAL, SAVE :: WRTIME
         INTEGER, SAVE :: NTICS = 0      ! no. of substeps within an output tstep
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         LOGICAL       :: L_DESID_DIAG ! flag determining whether or not DESID
                                       !   is in diagnostic mode              
         LOGICAL       :: L_NEWDAY

         INTEGER       :: NDATE, NTIME
         INTEGER, ALLOCATABLE, SAVE :: PRE_DESID_STREAM_DATE(:)
         
#ifdef Verbose1
         real mxzplm
#endif

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            
            ALLOCATE ( MY_STKEMIS( DESID_LAYS,NCOLS,NROWS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'MY_STKEMIS', PNAME )
              
            ! Set up and open 3d point source emissions diagnostic file?
            IF ( PT3DDIAG ) THEN
              NDATE = STDATE; NTIME = STTIME
              CALL NEXTIME( NDATE, NTIME, TSTEP(1) ) ! advance one output tstep
              CALL OPPT3D_DIAG( CTM_PT3D_DIAG, TSTEP(1), 
     &                   DESID_EMVAR( ISRM )%LEN, DESID_EMVAR( ISRM )%ARRY( : ),
     &                   DESID_EMVAR( ISRM )%UNITS( : ) )
 

              ALLOCATE ( VDEMIS_BUF( DESID_N_ISTR,DESID_LAYS,NCOLS,NROWS ), STAT = IOS )
              CALL CHECKMEM( IOS, 'VDEMIS_BUF', PNAME )
              VDEMIS_BUF = 0.0   ! array
              
              WRITE( LOGDEV,'(/5X, A /5X, A )' )
     &                        'Recording 3d point source emissions diagnostics',
     &                        'as a linear average over the output timestep'
#ifdef parallel_io
               IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_PT3D_DIAG, FSREAD3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM(CTM_PT3D_DIAG)
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                  END IF
               END IF
#endif
            END IF
            ALLOCATE (PRE_DESID_STREAM_DATE(SIZE(DESID_STREAM_DATE)), STAT=IOS)
            PRE_DESID_STREAM_DATE = STDATE
         END IF

C Based on the current time step, find whether or not a report should be written

!         WRITE( XMSG,'(A, I7.6)' )
!     &   'Calculating emissions point source layer fractions for', JTIME
!         WRITE( LOGDEV,* ) ' '
!         CALL M3MSG2( XMSG )
!
!         IF ( LDATE .NE. JDATE ) THEN  ! write day and date to stdout & XMSG
!            CALL WRDAYMSG( JDATE, XMSG )
!            LDATE = JDATE
!            WRITE( LOGDEV,'(9X, "at time ", A)' ) HHMMSS( JTIME )
!         END IF
!         WRTIME = .FALSE.
!         WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
!         IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
!            WDATE = JDATE; WTIME = JTIME
!            CALL NEXTIME( WDATE, WTIME, TSTEP( 2 ) )
!            IF ( .NOT. CURRSTEP( WDATE, WTIME, SDATE, STIME, TSTEP( 1 ),
!     &                           CDATE, CTIME ) ) THEN
!               XMSG = 'Cannot get step date and time'
!               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
!            END IF
!            WDATE = CDATE; WTIME = CTIME
!            WSTEP = 0
!            WRTIME = .TRUE.
!            DO N = 1, NPTGRPS
!               IF ( RDEV( N ) .GT. 0 ) THEN    ! write day, date and time to report file
!                  WRITE( CINT,'( I8 )' ) REP_LAYR
!                  CALL WRDAYMSG( WDATE, XMSG )
!                  XMSG = XMSG( 1:LEN_TRIM( XMSG ) )
!     &                    // ' at time ' // HHMMSS( WTIME )
!     &                    // 'for ltop .ge. ' // TRIM( ADJUSTL( CINT ) )
!                  WRITE( RDEV( N ),93000 ) XMSG( 1:LEN_TRIM( XMSG ) )
!               END IF
!            END DO
!         END IF

C Initialize Output Array
         !VDEMIS_PT = 0.0   ! array assignment

         N = IPSRM( ISRM )

C Convert pt source met data to bi-linear interpolated data
#ifdef mpas
         CALL PTMET_CONVT_MPAS ( N )
#else
         CALL PTMET_CONVT( JDATE, JTIME, N )
#endif

         PTLAYS = 1       ! Initialize Point Layers at lowest possible value

         IF ( MY_NSRC( N ) .LE. 0 ) RETURN

         IF ( PT3DFRAC ) THEN   ! Store layer fractions
            ALLOCATE( LFRAC( MY_NSRC( N ),DESID_LAYS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'LFRAC', PNAME )
            ALLOCATE( ZPLUM( MY_NSRC( N ),DESID_LAYS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'ZPLUM', PNAME )
            LFRAC = 0.0   ! 2-D array - Initialize layer fraction
            ZPLUM = 0.0   ! 2-D array - Initialize plume centroid height
         END IF

C Ensure that the model and stack groups emissions timestamp dates stay synchronized
         L_NEWDAY = .FALSE.
         IF ( PRE_DESID_STREAM_DATE(ISRM) .NE. JDATE ) THEN 
            L_NEWDAY = .TRUE.
            NDATE = JDATE; NTIME = JTIME
            CALL NEXTIME( NDATE, NTIME, -TSTEP( 1 ) )       ! go back one output tstep
            CALL NEXTIME( DESID_STREAM_DATE( ISRM ), NTIME, TSTEP( 1 ) ) ! advance the start date one time step
            PRE_DESID_STREAM_DATE(ISRM) = JDATE
         END IF

C Allocate Array to Store the Layer Heights of All Stacks (i.e. sources)
C from this Point Group (i.e. ptgrp)
         IF (MY_NSRC( N ) .GT. 0) THEN
            ALLOCATE( TFRAC( DESID_LAYS,MY_NSRC( N ) ), STAT = IOS )
            CALL CHECKMEM( IOS, 'TFRAC', PNAME )
            TFRAC = 0.0   ! array

C Calculate Plume Heights for All Point Sources on this File
            CALL CALC_PLUME_HEIGHT( DESID_STREAM_DATE( ISRM ), JDATE, JTIME, WRTIME, N   )

            ! Floor TFRAC to zero in case there are negatives and 
            ! renormalize positive values so the sum is 1.0
            If ( MINVAL( TFRAC ) .LT. 0.0 ) THEN

                WHERE ( TFRAC( :,: ) .LT. 0.0 ) TFRAC( :,: ) = 0.0

                DO S = 1,MY_NSRC( N )
                   TFRAC( :,S ) = TFRAC( :,S ) / SUM( TFRAC(:,S) )
                END DO
            END IF

            ! Calculate Maximum Layer Height across all source Plumes
            DO L = DESID_LAYS,1,-1
                PTLAYS = L
                IF ( COUNT( TFRAC(L,:) .EQ. 0.0 ) .LT. MY_NSRC( N ) ) EXIT
            END DO
         END IF

C Read Emissions for all sources on File N            
         DO ISPC = 1, DESID_N_ISTR
            VNAME = EMVAR_PT( ISPC )
            IF ( VNAME .EQ. '' ) CYCLE

            call interpolate_var (STKENAME(N), VNAME, DESID_STREAM_DATE( ISRM ), JTIME, STKBUFF(1:NSRC_EMIS(N)) )

            MY_STKEMIS( 1:PTLAYS,:,: ) = 0.0

            DO S = 1,MY_NSRC( N )
#ifdef mpas
                C = my_nsrc_mesh_index( S, N )
                R = 1
                MY_STKBUFF( S ) = STKBUFF( S )
#else
                SRC = SOURCE( N )%ARRY( S )
                MY_STKBUFF( S ) = STKBUFF( SRC )
                MY_COLBUFF( S ) = MY_STKCOL( N )%ARRY( S )
                MY_ROWBUFF( S ) = MY_STKROW( N )%ARRY( S )

                C = MY_COLBUFF( S )
                R = MY_ROWBUFF( S )
#endif

                MY_STKEMIS( 1:PTLAYS,C,R ) = MY_STKEMIS( 1:PTLAYS,C,R )
     &                     +  MY_STKBUFF( S ) * TFRAC( 1:PTLAYS,S )
            END DO

            ! Save Emissions in Common Array
            VDEMIS_PT( ISPC,1:PTLAYS,:,: ) = MY_STKEMIS( 1:PTLAYS,:,: )

         END DO ! End Loop on Species

         IF (MY_NSRC( N ) .GT. 0) DEALLOCATE( TFRAC )
         
         ! Reset the date of the gridded file if this is diagnostic mode
         ! and the day has advanced
         IF ( L_NEWDAY .AND. L_DESID_DIAG ) THEN
            CALL NEXTIME( DESID_STREAM_DATE( ISRM ), NTIME, -TSTEP(1) )
            PRE_DESID_STREAM_DATE( ISRM ) = DESID_STREAM_DATE( ISRM )
         END IF
 
C Write Out Diagnostic Info            
         IF ( PT3DFRAC .AND. WRTIME ) THEN     ! Write out layer fractions
            IF ( .NOT. WRITE3_DISTR( PLAYNAME( N ), 'LFRAC', WDATE, WTIME,
     &                               MY_NSRC( N ), DESID_LAYS, LFRAC ) ) THEN
               XMSG = 'Error writing "LFRAC" to file "' //
     &                 PLAYNAME( N )( 1:LEN_TRIM( PLAYNAME( N ) ) ) // '."'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( .NOT. WRITE3_DISTR( PLAYNAME( N ), 'ZPLUME', WDATE, WTIME,
     &                               MY_NSRC( N ), DESID_LAYS, ZPLUM ) ) THEN
               XMSG = 'Error writing "ZPLUM" to file "' //
     &                 PLAYNAME( N )( 1:LEN_TRIM( PLAYNAME( N ) ) ) // '."'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            WRITE( LOGDEV,94030 )
     &         'Timestep written to', TRIM( PLAYNAME( N ) ),
     &         'for date and time', WDATE, WTIME
         END IF

         IF ( PT3DFRAC ) THEN
            DEALLOCATE( LFRAC )
            DEALLOCATE( ZPLUM )
         END IF


C Write Out Diagnostic Info         
         IF ( PT3DDIAG ) THEN  ! store 3d point source emissions
            NTICS = NTICS + 1
            VDEMIS_BUF = VDEMIS_BUF + VDEMIS_PT   ! array assignmnt
            IF ( WRTIME ) THEN
               DIVFAC = 1.0 / FLOAT( NTICS )
               WRITE( LOGDEV,94030 )
     &               'Timestep written to', CTM_PT3D_DIAG,
     &               'for date and time', WDATE, WTIME
            VDEMIS_BUF = 0.0   ! array
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


94030    FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

         END SUBROUTINE GET_PT3D_EMIS

C-------------- Subprograms used by GET_PT3D_EMIS ----------------------

C=======================================================================

      SUBROUTINE CALC_PLUME_HEIGHT( LOC_STKDATE, JDATE, JTIME, WRTIME, N )

! Revision History.
!     Aug 18, 17 B.Murphy: Separated plume height calculation from the
!                          point source allocation algorithm

C-----------------------------------------------------------------------

      USE VGRD_DEFN, ONLY : VGTOP_GD
      USE STK_EMIS, ONLY : FIREVAR, FIREFLAG
#ifdef mpas
      USE STK_PRMS, ONLY : MY_STRT_SRC, MY_END_SRC, MY_NSRC, SOURCE, my_nsrc_pressure, my_nsrc_mesh_index
#else
      USE STK_PRMS, ONLY : MY_STRT_SRC, MY_END_SRC, MY_NSRC, SOURCE
#endif
      USE PTMET, ONLY : DESID_LAYS, P_D => PTMET_DATA
      USE RUNTIME_VARS, ONLY : LOGDEV, IPVERT, PT3DFRAC, REP_LAYR, LOG_MESSAGE
      USE UTILIO_DEFN
      USE CENTRALIZED_IO_MODULE, only : interpolate_var
#ifdef mpas
      USE coupler_module
      USE util_module, only : crlf
#endif

      IMPLICIT NONE

C Includes:
      INCLUDE SUBST_CONST     ! physical and mathematical constants

      INTEGER, INTENT( IN ) :: LOC_STKDATE
      INTEGER, INTENT( IN ) :: JDATE, JTIME
         
      LOGICAL, INTENT( IN ) :: WRTIME
      INTEGER, INTENT( IN ) :: N       ! Point Source File Number

#ifdef mpas
      INTEGER, PARAMETER :: DEG = 3
#endif

      CHARACTER( 16 ) :: PNAME = 'CALC_PLUME_HEIGHT'   ! procedure name
      CHARACTER( 16 ) :: VNAME 
      CHARACTER(  8 ) :: CINT     ! integer to character buffer for warning messages

      REAL,            PARAMETER :: USTARMIN = 0.1   ! Min valid value for USTAR
      REAL,            PARAMETER :: CONVPA = 1.0E-2  ! convert Pa to mb
      REAL,            PARAMETER :: BTU2M4PS3 = 9.2752E-03 ! convert BTU/s to m**4/s**3
      REAL,            PARAMETER :: BFACPHR = BTU2M4PS3 / 3600.0 ! m**4/s**3 per hour
                                 ! assumes heat flux from fire pt src file is in BTU/hr
      REAL             PSFC      ! surface pressure [Pa]
      REAL             USTMP      ! temp storage for ustar [m/s]
      REAL             HFLX       ! converted heat flux
      REAL             ZBOT       ! plume bottom elevation [m]
      REAL             ZTOP       ! plume top    elevation [m]
      REAL             ZPLM       ! plume centerline height above stack [m]
      REAL             ZDIFF      ! ZTOP - ZBOT
      REAL             DDZ        ! 1 / ZDIFF
      REAL             TSTK       ! temperature at top of stack [K]
      REAL             WSTK       ! wind speed  at top of stack [m/s]
      REAL             MV         ! mininum LFRAC 
      REAL             BESIZE     ! effective size of the fire buoyant core
      REAL             SFRACT     ! smoldering fraction size (1-BESIZE)
 
      INTEGER          LBOT       ! layer containing plume bottom
      INTEGER          LPBL       ! first L: ZF(L) above mixing layer - ONLY for REPORT
      INTEGER          LSTK       ! first L: ZF(L) > STKHT

      INTEGER :: S, L, SRC, M, STAT
      INTEGER :: LTOP, LACRESBURNED
      REAL    :: DELZ, DELFH
      REAL, ALLOCATABLE, SAVE :: TV( : )   ! Virtual temperature
      REAL, ALLOCATABLE, SAVE :: TF( : )   ! Full-layer height temperatures
      real, allocatable :: tdata(:)
      LOGICAL :: DETERMINED
      REAL    :: THV1
      REAL    :: THVK

      INTERFACE
           SUBROUTINE PREPLM( FIREFLG, DESID_LAYS, HMIX, HTS, PSFC, TS, DDZF, QV,
     &                        TA, UW, VW, ZH, ZF, PRES, LSTK, LPBL, TSTK,
     &                        WSTK, DTHDZ, WSPD )
             LOGICAL, INTENT( IN )  :: FIREFLG
             INTEGER, INTENT( IN )  :: DESID_LAYS
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

           SUBROUTINE PLMRIS( DESID_LAYS, LSTK, HFX, HMIX,
     &                        STKDM, STKHT, STKTK, STKVE,
     &                        TSTK, USTAR, DTHDZ, TA, WSPD,
     &                        ZF, ZH, ZSTK, WSTK, ZPLM )
             INTEGER, INTENT( IN )  :: DESID_LAYS
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

           SUBROUTINE FIRE_PLMRIS( DESID_LAYS, LSTK, HFX, HMIX,
     &                             BFLX, TSTK, USTAR, DTHDZ,
     &                             TA, WSPD, ZF, WSTK, ZPLM )
             INTEGER, INTENT( IN )  :: DESID_LAYS
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
             REAL,    INTENT( INOUT):: WSTK
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
      
#ifdef mpas
       IF (.NOT. ALLOCATED(TV)) THEN
          ALLOCATE (TV(DESID_LAYS),
     &              TF(DESID_LAYS),
     &              STAT=STAT)
       END IF
#endif

      ! If The Point File is for Fires, read and save the H-Flux Data in a
      ! Dedicated Variable, BFLUX
      LACRESBURNED = 0
      IF ( FIREFLAG( N ) ) THEN
#ifdef mpas
         allocate (tdata(MY_NSRC( N )), stat=stat)
         call interpolate_var (STKENAME(N), 'HFLUX', LOC_STKDATE, JTIME, tdata)

         DO S = 1, MY_NSRC( N )
            FIREVAR(N)%ARRY(S) = tdata(s)
         END DO
         deallocate (tdata)
#else
         call interpolate_var (STKENAME(N), 'HFLUX', LOC_STKDATE, JTIME, FIREVAR(N)%ARRY)
#endif

         DO S = 1, MY_NSRC( N )
#ifdef mpas
            SRC = S
#else
            SRC = SOURCE( N )%ARRY( S )
#endif
            BFLUX( N )%ARRY( S ) = FIREVAR( N )%ARRY( SRC ) * BTU2M4PS3
         END DO

         !Initialize negative ACRES-BURNED counter
      END IF
 
C Loop through sources and compute plume rise
      DO S = 1, MY_NSRC( N )

         IF ( FIREFLAG( N ) ) THEN
            IF ( BFLUX( N )%ARRY( S ) .LE. 0.0 ) THEN
               TFRAC( 1,S ) = 1.0
               TFRAC( 2:DESID_LAYS,S ) = 0.0
               LTOP = 1
               GO TO 201
            END IF
         END IF

         P_D( N )%ZZF( 0 ) = 0.0
         P_D( N )%ZZF( 1:DESID_LAYS ) = P_D( N )%ZF( 1:DESID_LAYS,S )  ! array of layers

C Get pressures: Use SIGMA values and surface pres. p=sigma*(psfc-ptop)+ptop
#ifdef mpas
         PSFC = my_nsrc_pressure(1,s,n)

         P_D( N )%PRESF( 0 ) = 0.0
         P_D( N )%PRESF( 1:DESID_LAYS ) = my_nsrc_pressure(1:DESID_LAYS,s,n)

         IF ( FIREFLAG( N ) ) THEN
            LSTK = 1
         ELSE
            P_D( N )%ZSTK( :,S ) = P_D( N )%ZF( :,S ) - STK_HT( N )%ARRY(S)
            L = 1
            DETERMINED = .FALSE.
            DO WHILE ((.NOT. DETERMINED) .AND. (L .LE. DESID_LAYS))
               IF ( P_D( N )%ZF( L,S ) .GT. STK_HT( N )%ARRY( S )) THEN
                  LSTK = L
                  DETERMINED = .TRUE.
               ELSE
                  L = L + 1
               END IF
            END DO
         END IF

         P_D( N )%WSPD = g3ddata(my_nsrc_mesh_index(s, n),1,:,wspd_ind)

         P_D( N )%DDZF( 1,S ) = 1.0 / P_D( N )%ZF( 1,S )
         DO L = 2, DESID_LAYS
            P_D( N )%DDZF( L,S ) = 1.0 / ( P_D( N )%ZF( L,S ) - P_D( N )%ZF( L-1,S ) )
         END DO

         IF ( FIREFLAG( N ) ) THEN
            TSTK = g2ddata(my_nsrc_mesh_index(s, n),1,temp2_ind)
            WSTK = P_D( N )%WSPD (1)
         ELSE
            m = MAX( 1, LSTK - DEG - 1 )
            TSTK = g3ddata(my_nsrc_mesh_index(s, n),1,m,temp_ind)
            WSTK = P_D( N )%WSPD (m)
         END IF

         DO L = 1, DESID_LAYS
            TV( L ) = P_D( N )%TA( L,S ) * ( 1.0 + 0.622 *
     &                (   g3ddata(my_nsrc_mesh_index(s, n),1,L,qv_ind)
     &                  / ( 1.0 + g3ddata(my_nsrc_mesh_index(s, n),1,L,qv_ind) ) ) )
         END DO

         DO L = 1, DESID_LAYS - 1
            DELZ  = g3ddata(my_nsrc_mesh_index(s, n),1,L+1,zh_ind) - g3ddata(my_nsrc_mesh_index(s, n),1,L,zh_ind)
            DELFH = g3ddata(my_nsrc_mesh_index(s, n),1,L,zf_ind) - g3ddata(my_nsrc_mesh_index(s, n),1,L,zh_ind)
            TF( L ) = TV( L ) + ( TV( L+1 ) - TV( L ) ) * DELFH / DELZ
         END DO
         L = DESID_LAYS
         DELZ  = g3ddata(my_nsrc_mesh_index(s, n),1,L,zh_ind) - g3ddata(my_nsrc_mesh_index(s, n),1,L-1,zh_ind)
         DELFH = g3ddata(my_nsrc_mesh_index(s, n),1,L,zf_ind) - g3ddata(my_nsrc_mesh_index(s, n),1,L,zh_ind)
         TF( L ) = TV( L ) + ( TV( L ) - TV( L-1 ) ) * DELFH / DELZ

         THV1  = TF( 1 ) * ( 1000.0 / P_D( N )%PRESF( 1 ) ) ** 0.286
         DO L = 2, DESID_LAYS
            THVK = TF( L ) * ( 1000.0 / P_D( N )%PRESF( L ) ) ** 0.286
            P_D( N )%DTHDZ( L ) = P_D( N )%DDZF( L,S ) * ( THVK - THV1 )
            THV1 = THVK
         END DO
         P_D( N )%DTHDZ( 1 ) = P_D( N )%DTHDZ( 2 )

#else

         PSFC = P_D( N )%PRSFC( S )
         DO L = 0, DESID_LAYS
            P_D( N )%PRESF( L ) = ( VGLVSXG( L ) * ( PSFC - VGTOP_GD )
     &                          +   VGTOP_GD ) * CONVPA
         END DO

C Set surface pressure (convert to mb from Pa)
         PSFC = CONVPA * PSFC

C Compute derived met vars needed before layer assignments
         CALL PREPLM( FIREFLAG( N ), DESID_LAYS,
     &                P_D( N )%HMIX( S ), STK_HT( N )%ARRY( S ), PSFC,
     &                P_D( N )%TSFC( S ),    P_D( N )%DDZF( :,S ),
     &                P_D( N )%QV( :,S ),    P_D( N )%TA( :,S ),
     &                P_D( N )%UWIND( :,S ), P_D( N )%VWIND( :,S ),
     &                P_D( N )%ZH( :,S ),    P_D( N )%ZF( :,S ),
     &                P_D( N )%PRESF, LSTK,  LPBL, TSTK, WSTK,
     &                P_D( N )%DTHDZ,        P_D( N )%WSPD )
#endif

C Trap USTAR at a minimum realistic value
         USTMP = MAX( P_D( N )%USTAR( S ), USTARMIN )

C Convert heat flux (watts/m2 to m K /s )
         HFLX = P_D( N )%HFX( S ) / ( CPD * P_D( N )%DENS( 1,S ) )

         IF ( .NOT. FIREFLAG( N ) ) THEN
            CALL PLMRIS( DESID_LAYS, LSTK, HFLX, P_D( N )%HMIX( S ),
     &                   STK_DM( N )%ARRY( S ), STK_HT( N )%ARRY( S ),
     &                   STK_TK( N )%ARRY( S ), STK_VEL( N )%ARRY( S ),
     &                   TSTK, USTMP,
     &                   P_D( N )%DTHDZ,     P_D( N )%TA( :,S ),
     &                   P_D( N )%WSPD,      P_D( N )%ZZF,
     &                   P_D( N )%ZH( :,S ), P_D( N )%ZSTK( :,S ),
     &                   WSTK, ZPLM )
         ELSE
            CALL FIRE_PLMRIS( DESID_LAYS, LSTK, HFLX, P_D( N )%HMIX( S ),
     &                   BFLUX( N )%ARRY( S ),
     &                   TSTK, USTMP,
     &                   P_D( N )%DTHDZ,     P_D( N )%TA( :,S ),
     &                   P_D( N )%WSPD,      P_D( N )%ZZF,
     &                   WSTK, ZPLM )
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
     &              + 1.5 * ( ZPLM - STK_HT( N )%ARRY( S ) )
               ZBOT = STK_HT( N )%ARRY( S )
     &              + 0.5 * ( ZPLM - STK_HT( N )%ARRY( S ) )
            END IF
         ELSE
C Alternative method to compute plume top/bot heights
            CALL PLSPRD( P_D( N )%DTHDZ, P_D( N )%ZZF, DESID_LAYS,
     &                   ZPLM, ZTOP, ZBOT )
         END IF

C Set up for computing plume fractions, assuming uniform distribution in pressure
C (~mass concentration -- minor hydrostatic assumption) from bottom to top.

         IF ( ZTOP .LT. STK_HT( N )%ARRY( S ) ) THEN
            WRITE( CINT,'( I8 )' ) S
            WRITE( XMSG,94010 ) 'ERROR: Top of plume is less than '
     &                          // 'top of stack for source:' // CINT
            CALL M3MESG( XMSG )
            WRITE( LOGDEV,* ) '    Zbot: ', ZBOT, ' Ztop: ', ZTOP
            WRITE( LOGDEV,* ) '    Stack Top: ', STK_HT( N )%ARRY( S ),
     &                        ' Plume Top: ', ZPLM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C Allocate plume to layers (compute layer plume fractions)

!              CALL POSTPLM( DESID_LAYS, ZBOT, ZTOP, PRESF, ZZF, TA( 1,S ), ZH( 1,S ),
!    &                       LTOP, TFRAC )

C Compute LBOT, LTOP such that
C  ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C  ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )

         DO L = 1, DESID_LAYS - 1
            IF ( ZBOT .LE. P_D( N )%ZZF( L ) ) THEN
               LBOT = L
               GO TO  122
            ELSE
               TFRAC( L,S ) = 0.0             ! fractions below plume
            END IF
         END DO
         LBOT = DESID_LAYS                      !  fallback

122      CONTINUE                           !  loop exit:  bottom found at LBOT

         IF ( ZTOP .LE. P_D( N )%ZZF( LBOT ) ) THEN  ! plume in this layer

            TFRAC( LBOT,S ) = 1.0
            LTOP = LBOT

            DO L = LBOT + 1, DESID_LAYS         ! fractions above plume
               TFRAC( L,S ) = 0.0
            END DO

         ELSE IF ( LBOT .EQ. DESID_LAYS ) THEN  ! plume above top layer

            TFRAC( LBOT,S ) = 1.0

            DO L = 1, DESID_LAYS - 1            ! fractions below plume
               TFRAC( L,S ) = 0.0
            END DO

         ELSE                               ! plume crosses layers

            DO L = LBOT + 1, DESID_LAYS
               IF ( ZTOP .LE. P_D( N )%ZZF( L ) ) THEN
                  LTOP = L
                  GO TO 126
               END IF
            END DO
            LTOP = DESID_LAYS                   !  fallback

126         CONTINUE

            ZDIFF = ZTOP - ZBOT
            IF ( ZDIFF .GT. 0.0 ) THEN

               DDZ  = 1.0 / ZDIFF
               TFRAC( LBOT,S ) = DDZ * ( P_D( N )%ZZF( LBOT ) - ZBOT )
               TFRAC( LTOP,S ) = DDZ * ( ZTOP - P_D( N )%ZZF( LTOP-1 ) )

            ELSE   ! ZDIFF .le. 0

               WRITE( CINT,'( I8 )' ) S
               WRITE( XMSG,94020 )
     &            'Infinitely small plume created for source:,'
     &            // CINT // CRLF() // BLANK10
     &            // 'All emissions put in first layer.'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               LBOT = 1; LTOP = 1
               TFRAC( LBOT,S ) = 1.0

            END IF

            DO L = LBOT + 1, LTOP - 1       ! layers in plume
               TFRAC( L,S ) = DDZ * ( P_D( N )%ZZF( L ) - P_D( N )%ZZF( L-1 ) )
            END DO

            DO L = LTOP + 1, DESID_LAYS         ! fractions above plume
               TFRAC( L,S ) = 0.0
            END DO

         END IF

C If layer fractions are negative, put in the first layer
         MV = MINVAL( TFRAC( 1:DESID_LAYS,S ) )
         IF ( MV .LT. 0.0 ) THEN

            WRITE( CINT,'( I8 )' ) S
            WRITE( XMSG,94010 ) 'WARNING: One or more negative plume '
     &               // 'fractions found for source:' // CINT
     &               // CRLF() // BLANK10 // 'Plume reset to '
     &               // 'put all emissions in surface layer.'
            CALL M3MESG( XMSG )

            TFRAC( 1,S ) = 1.0
            TFRAC( 2:DESID_LAYS,S ) = 0.0

         END IF

         IF ( FIREFLAG( N ) ) THEN
C If source is a fire, adjust for fire smoldering effects (include fractions below LBOT)
C calculate smoldering fraction:

             IF ( ACRES( N )%ARRY ( S ) .GT. 0.0 ) THEN
                BESIZE = 0.0703 * LOG( ACRES( N )%ARRY( S ) ) + 0.3
                BESIZE = MIN( BESIZE, 1.0 )
                SFRACT = 1.0 - BESIZE
             ELSE
                SFRACT = 1.0
                LACRESBURNED = LACRESBURNED + 1
             END IF

             ZDIFF = ZBOT - P_D( N )%ZZF( 0 )   ! Note: ZDIFF redefined
             IF ( ZDIFF .GT. 0.0 ) THEN
                DDZ  = 1.0 / ZDIFF
                DO L = 1, LBOT - 1
                  TFRAC( L,S ) = DDZ * ( P_D( N )%ZZF( L ) - P_D( N )%ZZF( L-1 ) )
     &                       * SFRACT
                END DO
                TFRAC( LBOT,S ) = TFRAC( LBOT,S )
     &                        + ( DDZ * ( ZBOT - P_D( N )%ZZF( LBOT-1 ) )
     &                          - TFRAC( LBOT,S ) ) * SFRACT
             ELSE   ! LBOT = 1
!               TFRAC( 1:LBOT-1 ) = 0.0
                TFRAC( LBOT,S ) = TFRAC( LBOT,S ) * ( 1.0 - SFRACT )
             END IF

             DO L = LBOT + 1, LTOP
                TFRAC( L,S ) = TFRAC( L,S ) * ( 1.0 - SFRACT )
             END DO

         END IF

201      CONTINUE
 
         IF ( PT3DFRAC ) THEN   ! Store layer fractions
            LFRAC( S,1:DESID_LAYS ) = TFRAC( 1:DESID_LAYS,S )  ! array
            ZPLUM( S,1 ) = ZPLM
         END IF

#ifndef mpas
C Possible report to REPRTLAY
         IF ( LTOP .GE. REP_LAYR .AND. RDEV( N ) .GT. 0 .AND. WRTIME ) THEN
              WRITE( RDEV( N ),93048 ) S, STK_HT( N )%ARRY( S ), LPBL,
     &                                     LBOT, LTOP, STK_VEL( N )%ARRY( S ), WSTK,
     &                                     STK_TK( N )%ARRY( S ), TSTK
         END IF
#endif

      END DO    ! end loop on sources S

      ! Print warning if any ACRES-BURNED values were negative
      IF ( LACRESBURNED .GT. 0 ) THEN
           write( XMSG,'(A,I8,A,I8,A)') "Warning: the value of ACRESBURNED for ",LACRESBURNED, " fire sources on stream ",
     &                     N, " is negative. CMAQ will set the smodlering fraction equal to 1.0 for these cases." 
           call log_message( logdev, xmsg )
      END IF
     
94010 FORMAT( 12( A, :, I8, :, 1X ) )
94020 FORMAT( 10( A, :, I7, :, 1X ) )
93048 FORMAT( I6, F8.2, 3I4, 4F10.2 )


      END SUBROUTINE CALC_PLUME_HEIGHT

C-----------------------------------------------------------------------

#ifndef mpas
         SUBROUTINE RETRIEVE_IOAPI_HEADER( FNAME, JDATE, JTIME )


            USE UTILIO_DEFN, ONLY : DESC3, XSTAT2

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
#endif

C-----------------------------------------------------------------------

      END MODULE PT3D_DEFN

