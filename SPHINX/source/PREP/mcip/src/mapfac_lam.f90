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

REAL FUNCTION mapfac_lam (phi, phi1, phi2)

!-------------------------------------------------------------------------------
! Name:     Map-Scale Factor for Lambert conformal projection.
! Purpose:  Calculates map-scale factors for secant Lambert conformal projection
!           from latitude and true latitudes.
! Notes:    Equations taken from "Numerical Prediction and Dynamic Meteorology",
!           Second Edition, by George J. Haltiner and Roger Terry Williams
!           (1980), pp. 13-14.  (Equations modified; see comments in code.)
! Revised:  03 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad ! true latitude 1 [rad]
  REAL,          INTENT(IN)    :: phi2    ! true latitude 2 [deg]
  REAL(8)                      :: phi2rad ! true latitude 2 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4
  REAL(8)                      :: sinphi0 ! cone constant
  REAL(8)                      :: term1
  REAL(8)                      :: term2
  REAL(8)                      :: term3
  REAL(8)                      :: term4
  REAL(8)                      :: term5

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.  ("K" in Haltiner and Williams, Eqn. 1-40.)
!-------------------------------------------------------------------------------

  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = DBLE(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term1 = DTAN (piover4 - phi1rad/2.0d0)
  term2 = DTAN (piover4 - phi2rad/2.0d0)

  sinphi0 = DLOG ( DCOS(phi1rad) / DCOS(phi2rad) )
  sinphi0 = sinphi0 / DLOG (term1 / term2)

!-------------------------------------------------------------------------------
! Compute map-scale factor, MAPFAC.
!
! M(phi) = (COS phi / COS phi1)**(K - 1) * ((1 + SIN phi1) / (1 + SIN phi))**K

! Note:  Original equation in Haltiner and Williams (1-40) is incorrect because
!        K-1 and K are exponents rather than multiplicative terms.
!-------------------------------------------------------------------------------

  phirad = DBLE(phi) * deg2rad  ! convert PHI from degrees to radians

  term3  = DCOS(phirad) / DCOS(phi1rad)
  term4  = sinphi0 - 1.0
  term5  = (1.0d0 + DSIN(phi1rad)) / (1.0d0 + DSIN(phirad))

  mapfac_lam = REAL(term3**term4 * term5**sinphi0)

END FUNCTION mapfac_lam
