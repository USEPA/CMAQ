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

SUBROUTINE xy2ll_lam (xx, yy, phi1, phi2, lambda0, phi0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Lambert Conformal Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and Lambert conformal projection information.
! Notes:    Equations adapted from http://mathworld.wolfram.com.
! Revised:  12 Dec 2007  Original version.  (T. Otte)
!           18 Sep 2009  Added reference latitude (PHI0) as an argument.
!                        Converted to double-precision.  Corrected comments
!                        associated with RHO0.  Corrected calculation of PSI
!                        (with no impact on results).  (T. Otte)
!           02 Oct 2009  Changed algorithm to follow Wolfram to eliminate the
!                        divide-by-zero condition for computing latitude along
!                        the standard longitude.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const, ONLY: rearth

  IMPLICIT NONE

  REAL(8)                      :: deg2rad    ! convert degrees to radians
  REAL(8)                      :: drearth    ! earth radius [m]
  REAL,          INTENT(OUT)   :: lambda     ! longitude [deg]
  REAL(8)                      :: lambdarad  ! longitude [rad]
  REAL,          INTENT(IN)    :: lambda0    ! standard longitude [deg]
  REAL(8)                      :: lambda0rad ! standard longitude [rad]
  REAL,          INTENT(OUT)   :: phi        ! latitude [deg]
  REAL(8)                      :: phirad     ! latitude [rad]
  REAL,          INTENT(IN)    :: phi0       ! reference latitude [deg]
  REAL(8)                      :: phi0rad    ! reference latitude [rad]
  REAL,          INTENT(IN)    :: phi1       ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad    ! true latitude 1 [rad]
  REAL,          INTENT(IN)    :: phi2       ! true latitude 2 [deg]
  REAL(8)                      :: phi2rad    ! true latitude 2 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover2    ! pi/2
  REAL(8)                      :: piover4    ! pi/4
  REAL(8)                      :: psi        ! auxiliary function
  REAL(8)                      :: rad2deg
  REAL(8)                      :: rho
  REAL(8)                      :: rho0       ! polar radius to origin
  REAL(8)                      :: term0
  REAL(8)                      :: term1
  REAL(8)                      :: term2
  REAL(8)                      :: theta      ! polar angle
  REAL(8)                      :: sinphi0    ! cone constant
  REAL(8)                      :: sinphi0inv ! 1/sinphi0
  REAL,          INTENT(IN)    :: xx         ! X-coordinate from origin
  REAL,          INTENT(IN)    :: yy         ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  piover2 = 2.0d0 * piover4
  deg2rad = pi / 1.8d2
  rad2deg = 1.8d2 / pi

  drearth = DBLE(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
!-------------------------------------------------------------------------------

  phi0rad = DBLE(phi0) * deg2rad  ! convert PHI0 from degrees to radians
  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = DBLE(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term0 = DTAN ( piover4 + phi0rad/2.0d0 )
  term1 = DTAN ( piover4 + phi1rad/2.0d0 )
  term2 = DTAN ( piover4 + phi2rad/2.0d0 )

  sinphi0 = DLOG ( DCOS(phi1rad) / DCOS(phi2rad) )
  sinphi0 = sinphi0 / DLOG (term2 / term1)

  sinphi0inv = 1.0d0 / sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  psi  = drearth * DCOS(phi1rad) * sinphi0inv * (term1**sinphi0)
  rho0 = psi / (term0**sinphi0)

!-------------------------------------------------------------------------------
! Compute longitude, LAMBDA.
!-------------------------------------------------------------------------------

  lambda0rad = lambda0 * deg2rad

  theta     = DATAN( DBLE(xx) / (rho0 - DBLE(yy)) )
  lambdarad = lambda0rad + theta * sinphi0inv
  lambda    = REAL(lambdarad * rad2deg)

!-------------------------------------------------------------------------------
! Compute latitude, PHI.
!-------------------------------------------------------------------------------

  rho = DSQRT( DBLE(xx)*DBLE(xx) + (rho0-DBLE(yy))*(rho0-DBLE(yy)) )
  rho = DSIGN(1.0d0, sinphi0) * rho

  phirad = (psi / rho)**sinphi0inv
  phirad = 2.0d0 * DATAN(phirad) - piover2
  phi    = REAL(phirad * rad2deg)

END SUBROUTINE xy2ll_lam
