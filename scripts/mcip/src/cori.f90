REAL FUNCTION cori (phi)

!-------------------------------------------------------------------------------
! Name:     Coriolis parameter
! Purpose:  Calculates Coriolis parameter (vertical component of the earth's
!           vorticity) from latitude.
! Revised:  03 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8),       PARAMETER     :: omega   = 7.2921d-5  ! sidereal day
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
! Compute Coriolis parameter, CORI.
!-------------------------------------------------------------------------------

  phirad = DBLE(phi) * deg2rad
  cori   = REAL( 2.0d0 * omega * DSIN(phirad) )

END FUNCTION cori
