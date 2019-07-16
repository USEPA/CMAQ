
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
C $Header: /project/yoj/arc/BCON/src/common/HGRD_DEFN.F,v 1.2 2011/10/21 16:52:31 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE HGRD_DEFN

C Define the horizontal domain, globally and for each processor, if parallel
C Revision History: David Wong 18 Feb 01: created
C                   Jeff Young 23 Feb 01: generalize
C                              31 Mar 01: add BLKPRM.EXT
C                              10 Nov 01: change to use GRIDDESC, env vars
C                   J Gipson   01 Sep 04: change block size to 50
C                   J Young    07 Dec 04: remove layer dependency (for MXCELLS,
C                                         MXBLKS) to implement vertical layer
C                                         dyn alloc appropriately
C                   J Young    22 Dec 05: move proc. anayl. stuff to PAGRD_DEFN
C                   J.Young    13 Jul 11: Replaced I/O API include files
C                                         with M3UTILIO; simplified for no parallel
C.......................................................................

      IMPLICIT NONE

C grid name selected from GRIDDESC
      CHARACTER( 16 ), SAVE :: GRID_NAME

C returned coordinate system (projection)
      CHARACTER( 16 ), SAVE :: COORD_SYS_NAME

C map projection type (should be named PRTYP_GD!)
      INTEGER, SAVE :: GDTYP_GD = 2 ! LAMGRD3

C first map projection parameter (degrees)
      REAL( 8 ), SAVE :: P_ALP_GD = 30.0

C second map projection parameter (degrees)
      REAL( 8 ), SAVE :: P_BET_GD = 60.0

C third map projection parameter (degrees)
      REAL( 8 ), SAVE :: P_GAM_GD = -90.0

C longitude for coord-system center (degrees)
      REAL( 8 ), SAVE :: XCENT_GD = -90.0

C latitude for coord-system center (degrees)
      REAL( 8 ), SAVE :: YCENT_GD = 40.0

      REAL( 8 ), SAVE :: XORIG_GD ! X-coordinate origin of computational grid
      REAL( 8 ), SAVE :: YORIG_GD ! Y-coordinate origin of computational grid

      REAL( 8 ), SAVE :: XCELL_GD ! X-coordinate cell width (M)
      REAL( 8 ), SAVE :: YCELL_GD ! Y-coordinate cell width (M)

      INTEGER, SAVE :: NCOLS      ! grid columns array dimension
      INTEGER, SAVE :: NROWS      ! grid rows array dimension
      INTEGER, SAVE :: NBNDY      ! no. of cells in one layer of local boundary
 
      INTEGER, SAVE :: NTHIK      ! boundary thickness (cells)
 
      CONTAINS

         FUNCTION HGRD_INIT () RESULT ( SUCCESS )

         USE M3UTILIO   ! IOAPI module

         LOGICAL :: SUCCESS

         INTEGER, SAVE :: LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 96 ) :: XMSG = ' '

C environment variable grid name to select from GRIDDESC
         CHARACTER( 16 ) :: HGRD_NAME = 'GRID_NAME'
         INTEGER, EXTERNAL :: SETUP_LOGDEV
         INTEGER :: STATUS

         CHARACTER( 48 ) :: VARDESC

C-----------------------------------------------------------------------

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
!           LOGDEV = SETUP_LOGDEV()
            LOGDEV = INIT3()
            SUCCESS = .TRUE.

            VARDESC = 'Horizontal Domain Definition '
            CALL ENVSTR( HGRD_NAME, VARDESC, 'GRID_NAME', GRID_NAME, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default'
               WRITE( LOGDEV, '(5X, A)' ) XMSG
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default'
               WRITE( LOGDEV, '(5X, A)' ) XMSG
            END IF

C With GRID_NAME (only input) retrieve all horizontal grid parameters from
C the grid description file pointed to by the GRIDDESC env var:

            IF ( .NOT. DSCGRID ( GRID_NAME,
     &                           COORD_SYS_NAME, GDTYP_GD, 
     &                           P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                           XCENT_GD, YCENT_GD,
     &                           XORIG_GD, YORIG_GD, XCELL_GD, YCELL_GD,
     &                           NCOLS, NROWS, NTHIK ) ) THEN
               XMSG = 'Failure retrieving horizontal grid parameters'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            NBNDY = 2*NTHIK * ( NCOLS + NROWS + 2*NTHIK )

         ELSE
            XMSG = 'Horizontal domain decomposition already defined'
            CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN

         END IF   ! FIRSTIME

         RETURN
         END FUNCTION HGRD_INIT
 
      END MODULE HGRD_DEFN
