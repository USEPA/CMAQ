
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
C                   S. Roselle 29 Mar 11: Replaced I/O API include files
C                                         with UTILIO_DEFN
C                   D. Wong    11 May 11: incorporated twoway model implementation
C                   D. Wong    Aug 15:    Added a new logical variable, IO_PE_INCLUSIVE
C                                           to facilitate parallel I/O implementation
C                   D. Wong    01 Feb 19: removed all MY_N clauses
C.......................................................................
      USE RUNTIME_VARS

      IMPLICIT NONE

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

      INTEGER, SAVE :: GL_NCOLS   ! no. of columns in global grid
      INTEGER, SAVE :: GL_NROWS   ! no. of rows in global grid
      INTEGER, SAVE :: GL_NBNDY   ! no. of cells in one layer of global boundary
 
      INTEGER, SAVE :: NCOLS      ! grid columns array dimension
      INTEGER, SAVE :: NROWS      ! grid rows array dimension
      INTEGER, SAVE :: NBNDY      ! no. of cells in one layer of local boundary
 
!     INTEGER, PARAMETER :: NTHIK = 1     ! boundary thickness (cells)
      INTEGER, SAVE :: NTHIK      ! boundary thickness (cells)
 
      INTEGER, SAVE :: MY_NCOLS_DOT ! local no. of dot file grid columns
      INTEGER, SAVE :: MY_NROWS_DOT ! local no. of dot file grid rows
C column range for each processor
      INTEGER, ALLOCATABLE, SAVE :: COLSX_PE( :,: )
      INTEGER, ALLOCATABLE, SAVE :: COLSD_PE( :,: )  ! dot file
C row range for each processor
      INTEGER, ALLOCATABLE, SAVE :: ROWSX_PE( :,: )
      INTEGER, ALLOCATABLE, SAVE :: ROWSD_PE( :,: )  ! dot file

C maximum stencil displacement in the north, east, south, and west direction
      INTEGER, PARAMETER :: MNDIS = 2
      INTEGER, PARAMETER :: MEDIS = 2
      INTEGER, PARAMETER :: MSDIS = 2
      INTEGER, PARAMETER :: MWDIS = 2

      LOGICAL :: IO_PE_INCLUSIVE

C BLKPRM

!     INTEGER, PARAMETER :: BLKSIZE = 500
!     INTEGER, PARAMETER :: BLKSIZE = 50
!     INTEGER, SAVE :: MXCELLS
!     INTEGER, SAVE :: MXBLKS

C Integral average conc

      CONTAINS

         FUNCTION HGRD_INIT ( NPROCS, MYID ) RESULT ( SUCCESS )

         USE UTILIO_DEFN

!        INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters

         INTEGER, INTENT( IN ) :: NPROCS  ! total worker processors
         INTEGER, INTENT( IN ) :: MYID
         LOGICAL :: SUCCESS

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 96 ) :: XMSG = ' '

C environment variable grid name to select from GRIDDESC
         INTEGER :: STATUS, ALST

         INTEGER, ALLOCATABLE :: NCOLS_PE( : )  ! Column range for each PE
         INTEGER, ALLOCATABLE :: NROWS_PE( : )  ! Row range for each PE

         INTEGER I

C-----------------------------------------------------------------------

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SUCCESS = .TRUE.

            MYPE = MYID

#ifndef mpas
C With GRID_NAME (only input) retrieve all horizontal grid parameters from
C the grid description file pointed to by the GRIDDESC env var:
            WRITE( LOGDEV, * )
            CALL LOG_HEADING( LOGDEV, "Retrieve Horizontal Grid" )
            IF ( .NOT. DSCGRID ( GRID_NAME,
     &                           COORD_SYS_NAME, GDTYP_GD, 
     &                           P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                           XCENT_GD, YCENT_GD,
     &                           XORIG_GD, YORIG_GD, XCELL_GD, YCELL_GD,
     &                           GL_NCOLS, GL_NROWS, NTHIK ) ) THEN
               XMSG = 'Failure retrieving horizontal grid parameters'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
#endif
 
            IF ( GL_NROWS .LE. 1 .AND. GL_NCOLS .LE. 1 ) COLUMN_MODEL = .TRUE.

C Check NPROCS against NPCOL*NPROW
            IF ( NPROCS .NE. NPCOL*NPROW ) THEN
               WRITE( LOGDEV,* ) ' --- Nprocs, NProw, NPcol ',
     &                                 NPROCS, NPROW, NPCOL
               XMSG = 'NPROCS is not equal to NPCOL*NPROW'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( NPROCS .GT. GL_NCOLS*GL_NROWS ) THEN

               WRITE( LOGDEV,* ) ' --- Nprocs, TOTAL_NCOLS, TOTAL_NROWS ',
     &                                 NPROCS, GL_NCOLS, GL_NROWS
               IF ( MYPE .EQ. 0 ) THEN
                  WRITE( OUTDEV,'(A,4(1X,I4))') ' --- Nprocs, TOTAL_NCOLS, TOTAL_NROWS ',
     &                                                NPROCS, GL_NCOLS, GL_NROWS
               END IF

               XMSG = 'NPROCS must be less than or equal to TOTAL_NCOLS*TOTAL_NROWS'
               IF ( MYPE .EQ. 0 ) THEN
                  WRITE(OUTDEV,'(A)')TRIM( XMSG )
               END IF 
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )

               XMSG = 'Change NPROCS, NPCOL, NPROW in runscript to fit TOTAL_NCOLS and TOTAL_NROWS'
               IF ( MYPE .EQ. 0 ) THEN
                  WRITE(OUTDEV,'(A,/)')TRIM( XMSG )
               END IF 
               WRITE(LOGDEV,'(A)')TRIM( XMSG )

               SUCCESS = .FALSE.; RETURN

            END IF

            ALLOCATE ( COLSX_PE( 2,NPROCS ),
     &                 ROWSX_PE( 2,NPROCS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** COLSX_PE or ROWSX_PE Memory allocation failed'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( COLSD_PE( 2,NPROCS ),
     &                 ROWSD_PE( 2,NPROCS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** COLSD_PE or ROWSD_PE  Memory allocation failed'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( NCOLS_PE( NPROCS ),
     &                 NROWS_PE( NPROCS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** NCOLS_PE or NROWS_PE Memory allocation failed'
               CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Construct the processor-to-subdomain map for main domain
            CALL SUBHDOMAIN( NPROCS, NPCOL, NPROW, GL_NCOLS, GL_NROWS,
     &                       NCOLS_PE, NROWS_PE, COLSX_PE, ROWSX_PE )

C Get corresponding dot file map

            DO I = 1, NPROCS
               COLSD_PE( 1,I ) = COLSX_PE( 1,I )
               COLSD_PE( 2,I ) = COLSX_PE( 2,I )
               ROWSD_PE( 1,I ) = ROWSX_PE( 1,I )
               ROWSD_PE( 2,I ) = ROWSX_PE( 2,I )
            END DO

            DO I = NPCOL, NPROCS, NPCOL
               COLSD_PE( 2,I ) = COLSD_PE( 2,I ) + 1
            END DO

            DO I = NPROCS, NPROCS - NPCOL + 1, -1
               ROWSD_PE( 2,I ) = ROWSD_PE( 2,I ) + 1
            END DO

            NCOLS = NCOLS_PE( MYPE+1 )
            NROWS = NROWS_PE( MYPE+1 )
            NBNDY = 2*NTHIK * ( NCOLS + NROWS + 2*NTHIK )
            GL_NBNDY = 2*NTHIK * ( GL_NCOLS + GL_NROWS + 2*NTHIK )

            MY_NCOLS_DOT = COLSD_PE( 2,MYPE+1 ) - COLSD_PE( 1,MYPE+1 ) + 1
            MY_NROWS_DOT = ROWSD_PE( 2,MYPE+1 ) - ROWSD_PE( 1,MYPE+1 ) + 1

!           MXCELLS = NCOLS * NROWS * NLAYS
!           MXBLKS  = 1 + ( MXCELLS - 1 ) / BLKSIZE

            DEALLOCATE ( NCOLS_PE )
            DEALLOCATE ( NROWS_PE )

         ELSE
            XMSG = 'Horizontal domain decomposition already defined'
            CALL M3WARN ( 'HGRD_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN

         END IF   ! FIRSTIME

         RETURN
         END FUNCTION HGRD_INIT
 
      END MODULE HGRD_DEFN
