
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/PTMET.F,v 1.5 2012/01/19 15:26:14 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PTMET

C-----------------------------------------------------------------------
C Function: 3d point source emissions met data

C Revision History:
C     20 Nov 2007 J.Young: initial implementation
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     27 Jul 2011 D.Wong: removed calculation which extends SRTCOL and STRTROW
C                         to the west and south direction in subroutines: 
C                         READMC2, READMC3, and READMD3
C     12 Aug 2015 D.Wong: - Used assumed shape array declaration and declared 
C                           associated subroutines in INTERFACE block
C                         - Replaced BMATVEC with BMATVECN which will call
C                           with a 1d or 2d argument subroutine by F90 
C                           poly-morphism feature
C                         - Based on the condition of MY_NSRC ( > 0 or not) to 
C                           determine execution of certain section of code or not
C                         - fixed a bug of incorrect assignment:
C                           PTMET_DATA%LEN1 = EMLAYS, PTMET_DATA%LEN2 = MSRC

C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: PTMET_TYPE

      IMPLICIT NONE

c     TYPE :: PTMET_TYPE
c        INTEGER          :: LEN1, LEN2
C Allocatable per-source meteorology variables
c        REAL,    POINTER :: HFX( : )        ! sensible heat flux [watts/m**2]
c        REAL,    POINTER :: HMIX( : )       ! mixing height [m]
c        REAL,    POINTER :: TSFC( : )       ! surface temperature [degK]
c        REAL,    POINTER :: USTAR( : )      ! friction velocity [m/s]
c        REAL,    POINTER :: PRSFC( : )      ! surface pressure [Pa]
C Allocatable temporary per-layer variables from 1:EMLAYS
c        REAL,    POINTER :: WSPD( : )       ! wind speed [m/s]
c        REAL,    POINTER :: DTHDZ( : )      ! virtual pot temp gradient [degK/m]
C Allocatable temporary per-layer variables from 0:EMLAYS
c        REAL,    POINTER :: PRESF( : )      ! pressure at full-levels
c        REAL,    POINTER :: ZZF( : )        ! per src elevn at full-levels
c Allocatable per-source and per layer meteorology variables.
C Dimensioned by layers, then sources
c!       REAL,    POINTER :: DDZH ( :,: )    ! 1/( ZH(L) - ZH(L-1) )
c        REAL,    POINTER :: DDZF ( :,: )    ! 1/( ZF(L) - ZF(L-1) )
c        REAL,    POINTER :: PRES ( :,: )    ! pressure [Pa]
c        REAL,    POINTER :: DENS ( :,: )    ! air density [kg/m**3]
c        REAL,    POINTER :: QV   ( :,: )    ! moisture mixing ratio [kg/kg]
c        REAL,    POINTER :: TA   ( :,: )    ! temperature [degK]
c        REAL,    POINTER :: UWIND( :,: )    ! x-component wind speed [m/s]
c        REAL,    POINTER :: VWIND( :,: )    ! y-component wind speed [m/s]
c        REAL,    POINTER :: ZF   ( :,: )    ! full layer height [m]
c        REAL,    POINTER :: ZH   ( :,: )    ! mid layer height [m]
c        REAL,    POINTER :: ZSTK ( :,: )    ! ZF( L,S ) - STKHT(S) [m]
c     END TYPE PTMET_TYPE

      TYPE( PTMET_TYPE ), ALLOCATABLE, SAVE :: PTMET_DATA( : )

C cross-point surface met file name
      CHARACTER(  16 ),              SAVE :: MC2NAME
C cross-point layered met file name
      CHARACTER(  16 ),              SAVE :: MC3NAME
C dot-point layered met file name
      CHARACTER(  16 ),              SAVE :: MD3NAME

C-----------------------------------------------------------------------
      CONTAINS

         FUNCTION PTMET_INIT ( NPTGRPS, EMLAYS ) RESULT ( SUCCESS )

         USE STK_PRMS, ONLY: MY_NSRC

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: NPTGRPS
         INTEGER, INTENT( IN ) :: EMLAYS
         LOGICAL               :: SUCCESS                          

         INTEGER N, MSRC, IOS
         CHARACTER( 16 ) :: PNAME = 'PTMET_INIT'   ! procedure name

         SUCCESS = .TRUE.
         ALLOCATE( PTMET_DATA( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PTMET_DATA', PNAME )

         DO N = 1, NPTGRPS

            MSRC = MY_NSRC( N )
            PTMET_DATA(N)%LEN1 = EMLAYS; PTMET_DATA(N)%LEN2 = MSRC

            IF ( MY_NSRC( N ) .GT. 0 ) THEN
C Allocate per-source arrays
               ALLOCATE( PTMET_DATA( N )%HFX  ( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'HFX', PNAME )
               ALLOCATE( PTMET_DATA( N )%HMIX ( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'HMIX', PNAME )
               ALLOCATE( PTMET_DATA( N )%TSFC ( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'TSFC', PNAME )
               ALLOCATE( PTMET_DATA( N )%USTAR( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'USTAR', PNAME )
               ALLOCATE( PTMET_DATA( N )%PRSFC( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRSFC', PNAME )

C Allocate per-source and per-layer arrays
               ALLOCATE( PTMET_DATA( N )%DDZF ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DDZF', PNAME )
               ALLOCATE( PTMET_DATA( N )%PRES ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRES', PNAME )
               ALLOCATE( PTMET_DATA( N )%DENS ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DENS', PNAME )
               ALLOCATE( PTMET_DATA( N )%QV   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'QV', PNAME )
               ALLOCATE( PTMET_DATA( N )%TA   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'TA', PNAME )
               ALLOCATE( PTMET_DATA( N )%UWIND( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'UWIND', PNAME )
               ALLOCATE( PTMET_DATA( N )%VWIND( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'VWIND', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZF   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZF', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZH   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZH', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZSTK ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZSTK', PNAME )

C Allocate per-layer arrays from 1:EMLAYS
               ALLOCATE( PTMET_DATA( N )%WSPD ( EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'WSPD', PNAME )
               ALLOCATE( PTMET_DATA( N )%DTHDZ( EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DTHDZ', PNAME )

C Allocate per-layer arrays from 0:EMLAYS
               ALLOCATE( PTMET_DATA( N )%PRESF( 0:EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRESF', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZZF  ( 0:EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZZF', PNAME )
            END IF ! (MY_NSRC( N ) > 0)

         END DO

         END FUNCTION PTMET_INIT 

C-----------------------------------------------------------------------

         SUBROUTINE PTMET_CONVT( JDATE, JTIME, NPTGRPS, EMLAYS, ZSTATIC )

         USE PTBILIN, B_D => BILIN_DATA   ! rename
         USE STK_PRMS, ONLY: MY_NSRC, SOURCE, STKHT
         USE BMATVEC_MODULE

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: NPTGRPS
         INTEGER, INTENT( IN ) :: EMLAYS
         LOGICAL, INTENT( IN ) :: ZSTATIC

         INTEGER N, MSRC
         CHARACTER( 16 ) :: PNAME = 'PTMET_CONVT'   ! procedure name

         INTERFACE
            SUBROUTINE DELTA_ZS( EMLAYS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )
               INTEGER, INTENT( IN )  :: EMLAYS, MY_NSRC
               INTEGER, INTENT( IN )  :: SRC_MAP( : )
               REAL,    INTENT( IN )  :: STKHT( : )
               REAL,    INTENT( IN )  :: ZF  ( :,: )
               REAL,    INTENT( OUT ) :: ZSTK( :,: )
               REAL,    INTENT( OUT ) :: DDZF( :,: )
            END SUBROUTINE DELTA_ZS
         END INTERFACE

C-----------------------------------------------------------------------

         DO N = 1, NPTGRPS

            MSRC = MY_NSRC( N )

            IF ( MY_NSRC( N ) .GT. 0 ) THEN
               IF ( .NOT. ZSTATIC ) THEN

                  CALL READMC3 ( 'ZH', JDATE, JTIME, XBUF )
                  CALL BMATVECN( METCGRID, MSRC, EMLAYS, B_D( N )%NX, B_D( N )%CX,
     &                          XBUF, PTMET_DATA( N )%ZH )

                  CALL READMC3 ( 'ZF', JDATE, JTIME, XBUF )
                  CALL BMATVECN( METCGRID, MSRC, EMLAYS, B_D( N )%NX, B_D( N )%CX,
     &                          XBUF, PTMET_DATA( N )%ZF )

C Compute ZSTK, DDZF

                  CALL DELTA_ZS( EMLAYS, MSRC,
     &                           SOURCE( N )%ARRY, STKHT( N )%ARRY,
     &                           PTMET_DATA( N )%ZF,
     &                           PTMET_DATA( N )%ZSTK,
     &                           PTMET_DATA( N )%DDZF )

               END IF

C Read and transform meteorology:

               CALL READMC2 ( 'HFX', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, 1, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%HFX )

               CALL READMC2 ( 'PBL', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, 1, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%HMIX )

!           CALL READMC2 ( 'TEMP1P5', JDATE, JTIME, XBUF )
               CALL READMC2 ( 'TEMP2', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, 1, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%TSFC )

               CALL READMC2 ( 'USTAR', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, 1, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%USTAR )

               CALL READMC2 ( 'PRSFC', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, 1, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%PRSFC )

               CALL READMC3 ( 'TA', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, EMLAYS, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%TA )

               CALL READMC3 ( 'QV', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, EMLAYS, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%QV )

               CALL READMC3 ( 'PRES', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, EMLAYS, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%PRES )

               CALL READMC3 ( 'DENS', JDATE, JTIME, XBUF )
               CALL BMATVECN( METCGRID, MSRC, EMLAYS, B_D( N )%NX, B_D( N )%CX,
     &                       XBUF, PTMET_DATA( N )%DENS )

C UWIND and VWIND are on DOT points (NOT flux points)

               CALL READMD3 ( 'UWIND', JDATE, JTIME, DBUF )
               CALL BMATVECN( METDGRID, MSRC, EMLAYS, B_D( N )%ND, B_D( N )%CD,
     &                       DBUF, PTMET_DATA( N )%UWIND )

               CALL READMD3 ( 'VWIND', JDATE, JTIME, DBUF )
               CALL BMATVECN( METDGRID, MSRC, EMLAYS, B_D( N )%ND, B_D( N )%CD,
     &                       DBUF, PTMET_DATA( N )%VWIND )
            END IF ! MY_NSRC( N ) > 0

         END DO   ! NPTGRPS

         END SUBROUTINE PTMET_CONVT

C-----------------------------------------------------------------------

         SUBROUTINE READMC2 ( VNAME, JDATE, JTIME, XBUF )

            USE PTBILIN, ONLY: XBND, YBND
            USE UTILIO_DEFN

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID  ! file name parameters

            CHARACTER( * ) :: VNAME     ! variable name
            INTEGER           JDATE     ! Julian date
            INTEGER           JTIME     ! time
            REAL              XBUF( * ) ! read buffer

            INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
            INTEGER, SAVE   :: STRTCOL, ENDCOL, STRTROW, ENDROW

            INTEGER, SAVE   :: LAY1, LAY2
            INTEGER            L1
            CHARACTER( 16 ) :: PNAME = 'READMC2'   ! procedure name
            CHARACTER( 96 ) :: XMSG = ' '
            LOGICAL         :: FIRSTIME = .TRUE.

            integer, save :: logdev
            integer, external :: setup_logdev

            IF ( FIRSTIME )  THEN
               FIRSTIME = .FALSE.
               logdev = setup_logdev()

               MET_CRO_2D  = MC2NAME

               CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                         STRTCOL, ENDCOL, STRTROW, ENDROW )

!              IF ( .NOT. XBND( 1 ) ) STRTCOL = STRTCOL - 1
               IF ( .NOT. XBND( 2 ) ) ENDCOL  = ENDCOL + 1
!              IF ( .NOT. YBND( 1 ) ) STRTROW = STRTROW - 1
               IF ( .NOT. YBND( 2 ) ) ENDROW  = ENDROW + 1

               LAY1 = 1; LAY2 = 1

!              write( logdev,* ) 'file: ', met_cro_2d, 'var: ', vname
!              write( logdev,* ) 'cols: ', endcol-strtcol+1
!              write( logdev,* ) 'rows: ', endrow-strtrow+1
!              write( logdev,* ) 'lays: ', lay2-lay1+1
!              write( logdev,* ) 'read size: ', ( endcol-strtcol+1 )
!    &                              * ( endrow-strtrow+1 )
!    &                              * ( lay2-lay1+1 )
!!             write( logdev,* ) 'xbuf size: ', size( xbuf )

            END IF

            IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOL,ENDCOL, STRTROW,ENDROW, LAY1,LAY2,
     &                          JDATE, JTIME, XBUF ) ) THEN

               L1 = LEN_TRIM( VNAME )
               XMSG = 'Could not read ' // VNAME( 1:L1 ) //
     &                ' from MET_CRO_2D'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            RETURN

         END SUBROUTINE READMC2

C-----------------------------------------------------------------------

         SUBROUTINE READMC3 ( VNAME, JDATE, JTIME, XBUF )

            USE VGRD_DEFN             ! vertical layer specifications
            USE PTBILIN, ONLY: XBND, YBND
            USE UTILIO_DEFN

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID  ! file name parameters

            CHARACTER( * ) :: VNAME     ! variable name
            INTEGER           JDATE     ! Julian date
            INTEGER           JTIME     ! time
            REAL              XBUF( :,: ) ! read buffer

            INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
            INTEGER, SAVE   :: STRTCOL, ENDCOL, STRTROW, ENDROW

            INTEGER, SAVE   :: LAY1, LAY2
            INTEGER            L1
            CHARACTER( 16 ) :: PNAME = 'READMC3'   ! procedure name
            CHARACTER( 96 ) :: XMSG = ' '
            LOGICAL         :: FIRSTIME = .TRUE.

            integer, save :: logdev
            integer, external :: setup_logdev

            IF ( FIRSTIME )  THEN
               FIRSTIME = .FALSE.
               logdev = setup_logdev()

               MET_CRO_3D  = MC3NAME

               CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                         STRTCOL, ENDCOL, STRTROW, ENDROW )

!              IF ( .NOT. XBND( 1 ) ) STRTCOL = STRTCOL - 1
               IF ( .NOT. XBND( 2 ) ) ENDCOL  = ENDCOL + 1
!              IF ( .NOT. YBND( 1 ) ) STRTROW = STRTROW - 1
               IF ( .NOT. YBND( 2 ) ) ENDROW  = ENDROW + 1

!              LAY1 = 1; LAY2 = NLAYS
               LAY1 = 1; LAY2 = SIZE( XBUF,2 )

!              write( logdev,* ) 'file: ', MET_CRO_3D, 'var: ', vname
!              write( logdev,* ) 'cols: ', endcol-strtcol+1
!              write( logdev,* ) 'rows: ', endrow-strtrow+1
!              write( logdev,* ) 'lays: ', lay2-lay1+1
!              write( logdev,* ) 'read size: ', ( endcol-strtcol+1 )
!    &                              * ( endrow-strtrow+1 )
!    &                              * ( lay2-lay1+1 )
!!             write( logdev,* ) 'xbuf size: ', size( xbuf )

            END IF

            IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                          STRTCOL,ENDCOL, STRTROW,ENDROW, LAY1,LAY2,
     &                          JDATE, JTIME, XBUF ) ) THEN

               L1 = LEN_TRIM( VNAME )
               XMSG = 'Could not read ' // VNAME( 1:L1 ) //
     &                ' from file MET_CRO_3D'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            RETURN

         END SUBROUTINE READMC3

C-----------------------------------------------------------------------

         SUBROUTINE READMD3( VNAME, JDATE, JTIME, DBUF )

            USE VGRD_DEFN             ! vertical layer specifications
            USE PTBILIN, ONLY: XBND, YBND
            USE UTILIO_DEFN

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID  ! file name parameters

            CHARACTER( * ) :: VNAME     ! variable name
            INTEGER           JDATE     ! Julian date
            INTEGER           JTIME     ! time
            REAL              DBUF( :,: ) ! read buffer

            INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
            INTEGER, SAVE   :: STRTCOL, ENDCOL, STRTROW, ENDROW

            INTEGER, SAVE   :: LAY1, LAY2
            INTEGER            L1
            CHARACTER( 16 ) :: PNAME = 'READMD3'   ! procedure name
            CHARACTER( 96 ) :: XMSG = ' '
            LOGICAL         :: FIRSTIME = .TRUE.
            CHARACTER( 16 ) :: LOC_VNAME

            integer, save :: logdev
            integer, external :: setup_logdev

            IF ( FIRSTIME )  THEN
               FIRSTIME = .FALSE.
               logdev = setup_logdev()

               MET_DOT_3D  = MD3NAME

               CALL SUBHFILE ( MET_DOT_3D, GXOFF, GYOFF,
     &                         STRTCOL, ENDCOL, STRTROW, ENDROW )

!              IF ( .NOT. XBND( 1 ) ) STRTCOL = STRTCOL - 1
               IF ( .NOT. XBND( 2 ) ) ENDCOL  = ENDCOL + 1
!              IF ( .NOT. YBND( 1 ) ) STRTROW = STRTROW - 1
               IF ( .NOT. YBND( 2 ) ) ENDROW  = ENDROW + 1

!              LAY1 = 1; LAY2 = NLAYS
               LAY1 = 1; LAY2 = SIZE( DBUF,2 )

!              write( logdev,* ) 'file: ', met_dot_3d, 'var: ', vname
!              write( logdev,* ) 'cols: ', endcol-strtcol+1
!              write( logdev,* ) 'rows: ', endrow-strtrow+1
!              write( logdev,* ) 'lays: ', lay2-lay1+1
!              write( logdev,* ) 'read size: ', ( endcol-strtcol+1 )
!    &                              * ( endrow-strtrow+1 )
!    &                              * ( lay2-lay1+1 )
!!             write( logdev,* ) 'xbuf size: ', size( xbuf )

            END IF

            IF ( ( VNAME .EQ. 'UWIND' ) .OR. ( VNAME .EQ. 'VWIND' ) ) THEN
!cccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc
               IF (       INTERPX( MET_DOT_3D, VNAME, PNAME,
     &                             STRTCOL,ENDCOL, STRTROW,ENDROW, LAY1,LAY2,
     &                             JDATE, JTIME, DBUF ) ) THEN
               ELSE
                  XMSG = 'Could not read ' // TRIM( VNAME ) //
     &                   ' from file MET_DOT_3D'
                  LOC_VNAME = TRIM( VNAME ) // 'C'
                  IF ( .NOT. INTERPX( MET_DOT_3D, LOC_VNAME, PNAME,
     &                                STRTCOL,ENDCOL, STRTROW,ENDROW, LAY1,LAY2,
     &                                JDATE, JTIME, DBUF ) ) THEN
                     L1 = LEN_TRIM( LOC_VNAME )
                     XMSG = 'Could not read ' // VNAME( 1:L1 ) //
     &                      ' from file MET_DOT_3D'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
               END IF 
            ELSE
               IF ( .NOT. INTERPX( MET_DOT_3D, VNAME, PNAME,
     &                             STRTCOL,ENDCOL, STRTROW,ENDROW, LAY1,LAY2,
     &                             JDATE, JTIME, DBUF ) ) THEN
                  L1 = LEN_TRIM( VNAME )
                  XMSG = 'Could not read ' // VNAME( 1:L1 ) //
     &                   ' from file MET_DOT_3D'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF 
            END IF

            RETURN

         END SUBROUTINE READMD3

C-----------------------------------------------------------------------

      END MODULE PTMET
