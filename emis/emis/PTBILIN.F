
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/PTBILIN.F,v 1.4 2011/10/21 16:10:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PTBILIN

C-----------------------------------------------------------------------
C Function: 3d point source bi-linear interpolation data

C Revision History:
C     20 Nov 2007 J.Young: initial implementation
C     13 Apr 2010 D. Wong: parallel bug fix - in subroutine ungridb2
C                          calculations use the lower left corner of the
C                          original domain
C     27 Jul 2011 David Wong: - fixed the description of XBND
C     12 Aug 2015 David Wong: - Based on the condition of MY_NSRC ( > 0 )
C                               to determine execution of code section
C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: BILIN_TYPE

      IMPLICIT NONE

c     TYPE :: BILIN_TYPE
c Allocatable un-gridding matrix variables (uses BMATVEC bilinear interpolation)
c Dimensioned 4 by NSRC
c        INTEGER          :: LEN1, LEN2
c        INTEGER, POINTER :: ND( :,: )    ! dot-point, cell indexes
c        INTEGER, POINTER :: NX( :,: )    ! cross-point, cell indexes
c        REAL,    POINTER :: CD( :,: )    ! dot-point, coefficients
c        REAL,    POINTER :: CX( :,: )    ! cross-point, coefficients
c     END TYPE BILIN_TYPE

      TYPE( BILIN_TYPE ), ALLOCATABLE, SAVE :: BILIN_DATA( : )

C Arrays which record .true. if local processor contains an exterior boundary
      LOGICAL, SAVE :: XBND( 2 )    ! west-east
      LOGICAL, SAVE :: YBND( 2 )    ! south-north

      REAL( 8 )     :: METCXORIG   ! cross point grid X-coord origin of met grid
      REAL( 8 )     :: METCYORIG   ! cross point grid Y-coord origin of met grid
      REAL( 8 )     :: XCELLCG     ! cross point grid X-coordinate cell dimension
      REAL( 8 )     :: YCELLCG     ! cross point grid Y-coordinate cell dimension
      REAL( 8 )     :: METDXORIG   ! dot point grid X-coord origin of met grid
      REAL( 8 )     :: METDYORIG   ! dot point grid Y-coord origin of met grid
      REAL( 8 )     :: XCELLDG     ! dot point grid X-coordinate cell dimension
      REAL( 8 )     :: YCELLDG     ! dot point grid Y-coordinate cell dimension
      REAL( 8 )     :: MY_CXORIG   ! local processor cross point X-coord origin
      REAL( 8 )     :: MY_CYORIG   ! local processor cross point Y-coord origin
      REAL( 8 )     :: MY_DXORIG   ! local processor dot point X-coord origin
      REAL( 8 )     :: MY_DYORIG   ! local processor dot point Y-coord origin

C Allocatable cross- OR dot-point meteorology input buffers
      REAL,    ALLOCATABLE, SAVE :: XBUF( :,: )  ! cross-point
      REAL,    ALLOCATABLE, SAVE :: DBUF( :,: )  ! dot-point

      INTEGER       :: METCGRID    ! cross point grid number of cells
      INTEGER       :: METDGRID    ! dot point grid number of cells

      CONTAINS

C----------------------------------------------------------------------

         FUNCTION PTBILIN_INIT ( NPTGRPS ) RESULT( SUCCESS )

         USE HGRD_DEFN             ! horizontal domain specifications
         USE VGRD_DEFN             ! vertical domain specifications
!        USE GRID_CONF             ! horizontal & vertical domain specifications
         USE STK_PRMS, ONLY: MY_NSRC, MY_XLOCA, MY_YLOCA

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: NPTGRPS
         LOGICAL               :: SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local variables:
         INTEGER MY_CCOLS    ! local processor cross point number of columns
         INTEGER MY_CROWS    ! local processor cross point number of rows
         INTEGER MY_DCOLS    ! local processor dot point number of columns
         INTEGER MY_DROWS    ! local processor dot point number of rows

         INTEGER I, J, M, N, MSRC, IOS, LOGDEV
         CHARACTER( 16 ) :: PNAME = 'PTBILIN_INIT'

C----------------------------------------------------------------------
         SUCCESS = .TRUE.
         LOGDEV = SETUP_LOGDEV()

         XBND( : ) = .FALSE.; YBND( : ) = .FALSE.
         M = MYPE + 1
         I = MOD( M, NPCOL )
         IF ( I .EQ. 1 .OR. NPCOL .EQ. 1 ) XBND( 1 ) = .TRUE.
         IF ( I .EQ. 0 ) XBND( 2 ) = .TRUE.
         I = 1 + ( M - 1 ) / NPCOL
         IF ( MOD( I, NPROW ) .EQ. 1 .OR. NPROW .EQ. 1 ) YBND( 1 ) = .TRUE.
         IF ( MOD( I, NPROW ) .EQ. 0 ) YBND( 2 ) = .TRUE.

C Set processor coordinate origins
!        MY_CXORIG = METCXORIG + XCELLCG * ( COLSX_PE( 1,1 ) - 1 )
!        MY_CYORIG = METCYORIG + YCELLCG * ( ROWSX_PE( 1,1 ) - 1 )
!        MY_DXORIG = METDXORIG + XCELLDG * ( COLSD_PE( 1,1 ) - 1 )
!        MY_DYORIG = METDYORIG + YCELLDG * ( ROWSD_PE( 1,1 ) - 1 )

         MY_CXORIG = METCXORIG
         MY_CYORIG = METCYORIG
         MY_DXORIG = METDXORIG
         MY_DYORIG = METDYORIG

         MY_CCOLS = MY_NCOLS
         MY_CROWS = MY_NROWS
         MY_DCOLS = MY_NCOLS + 1
         MY_DROWS = MY_NROWS + 1

!        IF ( .NOT. XBND( 1 ) ) THEN    ! extend cells west by 1
!             MY_CCOLS = MY_CCOLS + 1
!             MY_DCOLS = MY_DCOLS + 1
!        END IF
         IF ( .NOT. XBND( 2 ) ) THEN    ! extend cells east by 1
            MY_CCOLS = MY_CCOLS + 1
            MY_DCOLS = MY_DCOLS + 1
         END IF
!        IF ( .NOT. YBND( 1 ) ) THEN    ! extend cells south by 1
!           MY_CROWS = MY_CROWS + 1
!           MY_DROWS = MY_DROWS + 1
!        END IF
         IF ( .NOT. YBND( 2 ) ) THEN    ! extend cells north by 1
            MY_CROWS = MY_CROWS + 1
            MY_DROWS = MY_DROWS + 1
         END IF

         METCGRID = MY_CCOLS * MY_CROWS
         METDGRID = MY_DCOLS * MY_DROWS

!......................................................................
!        write( logdev,'(5x, "MYPE ", i8)' )            mype
!        write( logdev,'(5x, "XCELLCG ", 1pd20.12)' )   xcellcg
!        write( logdev,'(5x, "YCELLCG ", 1pd20.12)' )   ycellcg
!        write( logdev,'(5x, "XCELLDG ", 1pd20.12)' )   xcelldg
!        write( logdev,'(5x, "YCELLDG ", 1pd20.12)' )   ycelldg
!        write( logdev,'(5x, "MY_CXORIG ", 1pd20.12)' ) my_cxorig
!        write( logdev,'(5x, "MY_CYORIG ", 1pd20.12)' ) my_cyorig
!        write( logdev,'(5x, "MY_DXORIG ", 1pd20.12)' ) my_dxorig
!        write( logdev,'(5x, "MY_DYORIG ", 1pd20.12)' ) my_dyorig
!        write( logdev,'(5x, "MY_CCOLS ", i8)' )        my_ccols
!        write( logdev,'(5x, "MY_CROWS ", i8)' )        my_crows
!        write( logdev,'(5x, "MY_DCOLS ", i8)' )        my_dcols
!        write( logdev,'(5x, "MY_DROWS ", i8)' )        my_drows
!        write( logdev,'(5x, "METCGRID ", i8)' )        metcgrid
!        write( logdev,'(5x, "METDGRID ", i8)' )        metdgrid
!......................................................................

C Allocate read buffers for gridded, layered cross- and dot-point met data
         ALLOCATE( XBUF( METCGRID,NLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'XBUF', PNAME )
         ALLOCATE( DBUF( METDGRID,NLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'DBUF', PNAME )

         ALLOCATE( BILIN_DATA( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'BILIN_DATA', PNAME )

         DO N = 1, NPTGRPS

            MSRC = MY_NSRC( N )
            BILIN_DATA( N )%LEN1 = 4
            BILIN_DATA( N )%LEN2 = MSRC

            IF ( MY_NSRC( N ) .GT. 0 ) THEN
C Allocate ungridding arrays
               ALLOCATE( BILIN_DATA( N )%NX( 4,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'NX', PNAME )
               ALLOCATE( BILIN_DATA( N )%ND( 4,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ND', PNAME )
               ALLOCATE( BILIN_DATA( N )%CX( 4,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'CX', PNAME )
               ALLOCATE( BILIN_DATA( N )%CD( 4,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'CD', PNAME )

C Compute un-gridding matrices for cross- and dot-point met data
               CALL UNGRIDB2( 'X', MY_CCOLS, MY_CROWS, MY_CXORIG, MY_CYORIG,
     &                        XCELLCG, YCELLCG, MSRC,
     &                        MY_XLOCA( N )%ARRY, MY_YLOCA( N )%ARRY,
     &                        BILIN_DATA( N )%NX,
     &                        BILIN_DATA( N )%CX, I )
               CALL UNGRIDB2( 'D', MY_DCOLS, MY_DROWS, MY_DXORIG, MY_DYORIG,
     &                        XCELLDG, YCELLDG, MSRC,
     &                        MY_XLOCA( N )%ARRY, MY_YLOCA( N )%ARRY,
     &                        BILIN_DATA( N )%ND,
     &                        BILIN_DATA( N )%CD, J )
               WRITE( LOGDEV,94030 ) I, J
            END IF ! MY_NSRC( N ) > 0

         END DO

94030    FORMAT( / 5X,
     &   'Number of emissions point sources in my Cross subdomain:', i8 / 5X,
     &   'Number of emissions point sources in my Dot   subdomain:', i8 / )

         END FUNCTION PTBILIN_INIT

      END MODULE PTBILIN
