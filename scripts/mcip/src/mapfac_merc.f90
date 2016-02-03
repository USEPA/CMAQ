REAL FUNCTION mapfac_merc (phi)

!-------------------------------------------------------------------------------
! Name:     Map-Scale Factor for Mercator projection
! Purpose:  Calculates map-scale factors for Mercator projection from latitude.
! Notes:    Equation taken from "Numerical Prediction and Dynamic Meteorology",
!           Second Edition, by George J. Haltiner and Roger Terry Williams
!           (1980), pp. 13.
! Revised:  15 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

!-------------------------------------------------------------------------------
! Compute map-scale factor, MAPFAC.
!
! M(phi) = COS phi0 / COS phi
!
!   where phi0 is the latitude of the "true" projection (assumed to be equator)
!-------------------------------------------------------------------------------

  phirad = DBLE(phi) * deg2rad  ! convert PHI from degrees to radians

  mapfac_merc = REAL( 1.0d0 / DCOS(phirad) )

END FUNCTION mapfac_merc
