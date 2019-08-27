!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Calculates (X,Y) from origin for a given latitude-longitude pair
!           and Lambert conformal projection information for secant case.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 181-182.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           04 Dec 2008  Added argument for reference latitude, PHI0.
!                        Changed routine so it is no longer hard-wired to
!                        have a reference latitude at the first true
!                        latitude.  (T. Otte and J. Pleim)
!           17 Sep 2009  Corrected inline comments associated with definitions
!                        of RHO and RHO0.  Corrected calculation of PSI (with
!                        no impact on results).  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const, ONLY: rearth

  IMPLICIT NONE

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8)                      :: dlambda ! delta lambda
  REAL(8)                      :: drearth ! double-precision radius of earth [m]
  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL,          INTENT(IN)    :: phi0    ! reference latitude [deg]
  REAL(8)                      :: phi0rad ! reference latitude [rad]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad ! true latitude 1 [rad]
  REAL,          INTENT(IN)    :: phi2    ! true latitude 2 [deg]
  REAL(8)                      :: phi2rad ! true latitude 2 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4
  REAL(8)                      :: psi     ! auxiliary function
  REAL(8)                      :: rho     ! polar radius to latitude phi
  REAL(8)                      :: rho0    ! polar radius to origin
  REAL(8)                      :: term
  REAL(8)                      :: term0
  REAL(8)                      :: term1
  REAL(8)                      :: term2
  REAL(8)                      :: theta   ! polar angle
  REAL(8)                      :: sinphi0 ! cone constant
  REAL,          INTENT(OUT)   :: xx      ! X-coordinate from origin
  REAL,          INTENT(OUT)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = DBLE(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
! Note:  PHI0 is the reference latitude, which is user-defined.  It is NOT
!        used in the calculation of SINPHI0, which is the cone constant.
!-------------------------------------------------------------------------------

  phi0rad = DBLE(phi0) * deg2rad  ! convert PHI0 from degrees to radians
  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = DBLE(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term0 = DTAN (piover4 - phi0rad/2.0d0)
  term1 = DTAN (piover4 - phi1rad/2.0d0)
  term2 = DTAN (piover4 - phi2rad/2.0d0)

  sinphi0 = DLOG ( DCOS(phi1rad) / DCOS(phi2rad) )
  sinphi0 = sinphi0 / DLOG (term1 / term2)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  dlambda = DBLE(lambda - lambda0) * deg2rad
  theta   = dlambda * sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  psi  = drearth * DCOS(phi1rad) / sinphi0 / (term1**sinphi0)
  rho0 = psi * (term0**sinphi0)

!-------------------------------------------------------------------------------
! Compute polar radius to latitude PHI, RHO.
!-------------------------------------------------------------------------------

  phirad = DBLE(phi) * deg2rad  ! convert PHI from degrees to radians
  term   = DTAN (piover4 - phirad/2.0d0)
  rho    = psi * (term**sinphi0)

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx = REAL(        rho * DSIN(theta) )
  yy = REAL( rho0 - rho * DCOS(theta) )

END SUBROUTINE ll2xy_lam_sec
