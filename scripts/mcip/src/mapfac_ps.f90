REAL FUNCTION mapfac_ps (phi, phi1)

!-------------------------------------------------------------------------------
! Name:     Map-Scale Factor for polar stereographic projection.
! Purpose:  Calculates map-scale factors for polar stereographic projection
!           from latitude.
! Notes:    Equation taken from "Numerical Prediction and Dynamic Meteorology",
!           Second Edition, by George J. Haltiner and Roger Terry Williams
!           (1980), pp. 11-13.
! Revised:  24 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8)                      :: hemi    ! +/-1 for Northern/Southern Hemis
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
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
! M(phi) = hemi * 2 / (1 + SIN phi)
!-------------------------------------------------------------------------------

  hemi      = DSIGN( 1.0d0, DBLE(phi1) )
  phirad    = DBLE(phi) * deg2rad  ! convert PHI from degrees to radians

  mapfac_ps = REAL( hemi * 2.0d0 / (1.0d0 + DSIN(phirad)) )

END FUNCTION mapfac_ps
