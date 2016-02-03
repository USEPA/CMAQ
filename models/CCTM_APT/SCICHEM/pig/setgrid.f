      SUBROUTINE SETGRID (JDATE, JTIME, TSTEP)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Set-up the SCICHEM grids based on the host model grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated April 2005 for CMAQ-APT-PM (PKK, AER)
!    Updated Oct 2005 for MADRID-Hg (PKK, AER)
!    Updated May 2006 for CMAQ v4.5 (PKK, AER)
!    Updated June 2006 to use compile flag to activate/deactivate Hg (PKK, AER)
!    Updated July 2006 to calculate zw3d for top of domain (PKK, AER)
!
!******************************************************************************

! --- MODULES AND INCLUDES

! May 2006, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
      USE GRID_CONF             ! horizontal and vertical domain specifications

      USE COMMON_PUF
      USE COMMON_MET
      USE COMMON_GRD
      USE HOST_INC

      IMPLICIT NONE
 
! --- ARGUMENTS
 
      INTEGER      JDATE                   ! current date (YYYYDDD)
      INTEGER      JTIME                   ! current time (HHMMSS)
      INTEGER      TSTEP                   ! model time step (HHMMSS)

! --- LOCALS
      CHARACTER( 16 ) :: VARNM

      CHARACTER( 16 ) :: PNAME = 'SETGRID'
      CHARACTER( 120 ) :: XMSG = ' '

C.......   Nonlayered variables

      REAL, DIMENSION(:,:),ALLOCATABLE :: LAT  ! latitude of cross pt. grid
      REAL, DIMENSION(:,:),ALLOCATABLE :: LON  ! longitude of cross pt. grid
      REAL, DIMENSION(:,:),ALLOCATABLE :: HT   ! terrain height in meters

C.......   Layered variables
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: ZH  ! mid-layer ht above
                                                ! ground (meters)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: ZF  ! layer ht above
                                                ! ground (meters)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: Z2  ! Layer-top height

C.......   Variables for SCICHEM Vertical grid
      REAL         TERR, ZFTOP, HTOP, ZRATIO

      INTEGER      COL, ROW, LVL, IJ, JJ  ! loop counters
      REAL         DENOM
      INTEGER :: IOS

C.......   Horizontal Grid

      IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open ' // GRID_CRO_2D // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
         XMSG = 'Could not get ' // GRID_CRO_2D // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      ALLOCATE( LAT( GL_NCOLS, GL_NROWS ), STAT = IOS )
      IF (IOS /= 0) THEN
         XMSG = 'Failure allocating LAT array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      VARNM = 'LAT'
      IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    JDATE, JTIME, LAT ) ) THEN
         XMSG = 'Could not read LAT from ' // GRID_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if INTERPX failed

      ALLOCATE( LON( GL_NCOLS, GL_NROWS ), STAT = IOS )
      IF (IOS /= 0) THEN
         XMSG = 'Failure allocating LON array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      VARNM = 'LON'
      IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    JDATE, JTIME, LON ) ) THEN
         XMSG = 'Could not read LON from ' // GRID_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if INTERPX failed

      ALLOCATE( HT( GL_NCOLS, GL_NROWS ), STAT = IOS )
      IF (IOS /= 0) THEN
         XMSG = 'Failure allocating HT array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      VARNM = 'HT'
      IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    JDATE, JTIME, HT ) ) THEN
         XMSG = 'Could not read HT from ' // GRID_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if INTERP3 failed

      NXB  = GL_NCOLS
      NYB  = GL_NROWS

      NXYB = NXB * NYB

      DX   = XCELL3D
      DY   = YCELL3D

      XMIN = XORIG3D + 0.5 * DX   ! center of first grid cell
      YMIN = YORIG3D + 0.5 * DY   ! center of first grid cell

      XMAX = XMIN + ( NCOLS3D - 1 ) * DX ! center of last grid cell
      YMAX = YMIN + ( NROWS3D - 1 ) * DY ! center of last grid cell

      XREF = XMIN
      YREF = YMIN

      LON0 = LON( 1, 1 )
      LAT0 = LAT( 1, 1 )

C...... Set terrain arrays to zero if not using terrain
      IF ( .NOT. LTER ) THEN
         CALL ZERO_TERRAIN
      ELSE

C...... Terrain arrays

         DO ROW = 1, NYB
            JJ = ( ROW - 1 ) * NXB
            DO COL = 1, NXB
               IJ = JJ + COL
               HS( IJ ) = HT( COL, ROW )
            END DO
         END DO

C------ Find and subtract minimum terrain height
         HMIN  = HS( 1 )

         DO IJ = 1, NXYB
            HMIN = MIN( HMIN, HS( IJ ) )
         END DO

         HS = HS - HMIN

!        DO IJ = 1, NXYB
!           HS( IJ ) = HS( IJ ) - HMIN
!        END DO

      END IF

C.......   Vertical grid
      NZB = NLAYS
C
C  Obtain ZH and ZF
C
      ALLOCATE( ZH( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS)
      IF (IOS /= 0) THEN

         XMSG = 'Failure allocating ZH array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      VARNM = 'ZH'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    JDATE, JTIME, ZH ) ) THEN
!     IF ( .NOT. INTERP3( MET_CRO_3D, 'ZH', PNAME,
!    &     JDATE, JTIME, GL_NCOLS*GL_NROWS*NLAYS, ZH ) ) THEN

         XMSG = 'Could not read ZH from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF                 !  if not interp3() for ZH

      ALLOCATE( Z2( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS)
      IF (IOS /= 0) THEN

         XMSG = 'Failure allocating Z2 array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      VARNM = 'ZF'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    JDATE, JTIME, Z2 ) ) THEN
!     IF ( .NOT. INTERP3( MET_CRO_3D, 'ZF', PNAME,
!    &     JDATE, JTIME, GL_NCOLS*GL_NROWS*NLAYS, Z2 ) ) THEN

         XMSG = 'Could not read ZF from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF                 !  if not interp3() for ZF

      ALLOCATE( ZF( GL_NCOLS, GL_NROWS, 0:NLAYS ), STAT = IOS)
      IF (IOS /= 0) THEN

         XMSG = 'Failure allocating ZF array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      DO ROW = 1, GL_NROWS
         DO COL = 1, GL_NCOLS
            ZF( COL, ROW, 0 ) = 0.0
         END DO
      END DO
C
      DO LVL = 1, NLAYS
         DO ROW = 1, GL_NROWS
            DO COL = 1, GL_NCOLS
               ZF( COL, ROW, LVL ) = Z2( COL, ROW, LVL )
            END DO
         END DO
      END DO
C
C  Get SCICHEM vertical grid
C
      DO ROW = 1, NYB
         JJ = ( ROW - 1 ) * NXB
         DO COL = 1, NXB
            IJ = JJ + COL
            TERR  = HS( IJ )
            ZFTOP = ZF( COL, ROW, NLAYS )
            HTOP  = ZFTOP + TERR
            ZRATIO = HTOP / ZFTOP
            DO LVL = 1, NLAYS
               Z3D ( COL, ROW, LVL) = ZH( COL, ROW, LVL )
               ZW3D( COL, ROW, LVL) = ZF( COL, ROW, LVL - 1 )
               ZB3D( COL, ROW, LVL) = ZH( COL, ROW, LVL ) * ZRATIO
            END DO
            ZW3D( COL, ROW, NLAYS + 1 ) = ZF( COL, ROW, NLAYS )
         END DO
      END DO

C ...... Calculate average zb for domain
      DENOM = FLOAT( NXYB )
      ZB = 0.
      DO LVL = 1, NLAYS
         DO ROW = 1, NYB
            DO COL = 1,NXB
               ZB( LVL ) = ZB( LVL ) + ZB3D( COL, ROW, LVL )
            END DO
         END DO
         ZB( LVL ) = ZB( LVL ) / DENOM
      END DO

      ZBW( 1 ) = 0.
      DO LVL = 2, NZB
         ZBW( LVL ) = 2.0 * ZB( LVL - 1 ) - ZBW( LVL - 1 )
      END DO

      ZMAX = ZBW( NZB )
!      VRES = 250.     ! DEFAULT VERTICAL RESOLUTION (read from namelist now)
      ZBTOP = ZMAX

C...... Lat-lon arrays

      DO ROW = 1, NYB
         JJ = ( ROW - 1 ) * NXB
         DO COL = 1, NXB
            IJ = JJ + COL
            LAT2D( IJ ) = LAT( COL, ROW )
            LON2D( IJ ) = LON( COL, ROW )
         END DO
      END DO

! --- Release memory
      DEALLOCATE(ZF)
      DEALLOCATE(Z2)
      DEALLOCATE(ZH)
      DEALLOCATE(HT)
      DEALLOCATE(LON)
      DEALLOCATE(LAT)

      RETURN
      END
