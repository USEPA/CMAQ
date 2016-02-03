SUBROUTINE xy2ll_ps (xx, yy, phi1, lambda0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Polar Stereographic Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and polar stereographic projection information.
! Notes:    Adapted from equations found at http://starbase.jpl.nasa.gov/
!           mgn-v-rdrs-5-dvdr-v1.0/gvdr0001/catalog/dsmp.lbl.
! Revised:  18 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const, ONLY: rearth

  IMPLICIT NONE

  REAL(8)                      :: cc
  REAL(8)                      :: deg2rad    ! convert degrees to radians
  REAL(8)                      :: drearth    ! earth radius [m]
  REAL(8)                      :: hemi       ! +/-1 for Northern/Southern Hemis
  REAL,          INTENT(OUT)   :: lambda     ! longitude [deg]
  REAL(8)                      :: lambdarad  ! longitude [rad]
  REAL,          INTENT(IN)    :: lambda0    ! standard longitude [deg]
  REAL(8)                      :: lambda0rad ! standard longitude [rad]
  REAL,          INTENT(OUT)   :: phi        ! latitude [deg]
  REAL(8)                      :: phirad     ! latitude [rad]
  REAL,          INTENT(IN)    :: phi1       ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad    ! true latitude 1 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover2    ! pi/2
  REAL(8)                      :: piover4    ! pi/4
  REAL(8)                      :: rad2deg
  REAL(8)                      :: rho
  REAL(8)                      :: scalefac
  REAL(8)                      :: sigma
  REAL,          INTENT(IN)    :: xx         ! X-coordinate from origin
  REAL(8)                      :: xxd
  REAL,          INTENT(IN)    :: yy         ! Y-coordinate from origin
  REAL(8)                      :: yyd

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  piover2 = 2.0d0 * piover4
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2
  rad2deg = 1.8d2 / pi

  drearth = DBLE(rearth)

!-------------------------------------------------------------------------------
! Set up geometric constants.
!-------------------------------------------------------------------------------

  hemi = DSIGN (1.0d0, DBLE(phi1))

  xxd  = DBLE(xx)
  yyd  = DBLE(yy)

  phi1rad = DBLE(phi1) * deg2rad

!!!TLO  sigma = (1.0d0 + DSIN(phi1rad)) / (1.0d0 + DSIN(hemi*pi))  ! at pole
  sigma = (1.0d0 + DSIN(phi1rad)) / 2.0d0 * hemi
  scalefac = drearth / sigma

  rho  = DSQRT ( xxd*xxd + yyd*yyd )
  cc   = 2.0d0 * DATAN2 ( rho, 2.0d0 * scalefac )

!-------------------------------------------------------------------------------
! Compute latitude (PHI).
!-------------------------------------------------------------------------------

  phirad  = ( piover2 - cc ) * hemi
  phi     = REAL( phirad * rad2deg )

!-------------------------------------------------------------------------------
! Compute longitude (LAMBDA).
!-------------------------------------------------------------------------------

  lambda0rad = DBLE(lambda0) * deg2rad
  lambdarad  = lambda0rad + DATAN2 ( xxd, -hemi*yyd )
  lambda     = REAL( lambdarad * rad2deg )

END SUBROUTINE xy2ll_ps
