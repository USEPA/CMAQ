SUBROUTINE xy2ll_merc (xx, yy, lambda0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Polar Stereographic Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and polar stereographic projection information.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 190-192.
! Revised:  18 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const, ONLY: rearth

  IMPLICIT NONE

  REAL(8)                      :: deg2rad    ! convert degrees to radians
  REAL(8)                      :: drearth    ! earth radius [m]
  REAL,          INTENT(OUT)   :: lambda     ! longitude [deg]
  REAL(8)                      :: lambdarad  ! longitude [rad]
  REAL,          INTENT(IN)    :: lambda0    ! center longitude [deg]
  REAL(8)                      :: lambda0rad ! center longitude [rad]
  REAL,          INTENT(OUT)   :: phi        ! latitude [deg]
  REAL(8)                      :: phirad     ! latitude [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover2    ! pi/2
  REAL(8)                      :: piover4    ! pi/4
  REAL(8)                      :: rad2deg
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

  xxd  = DBLE(xx)
  yyd  = DBLE(yy)

!-------------------------------------------------------------------------------
! Compute latitude (PHI).
!-------------------------------------------------------------------------------

  phirad  = ( 2.0d0 * DATAN ( DEXP(yyd/drearth) ) ) - piover2
  phi     = REAL( phirad * rad2deg )

!-------------------------------------------------------------------------------
! Compute longitude (LAMBDA).
!-------------------------------------------------------------------------------

  lambda0rad = DBLE(lambda0) * deg2rad
  lambdarad  = lambda0rad + xxd/drearth
  lambda     = REAL( lambdarad * rad2deg )

END SUBROUTINE xy2ll_merc
