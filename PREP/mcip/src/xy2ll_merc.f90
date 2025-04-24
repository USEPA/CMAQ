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

SUBROUTINE xy2ll_merc (xx, yy, truelat1, lambda0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Polar Stereographic Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and polar stereographic projection information.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 190-192.
! Revised:  18 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
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
  REAL(8)                      :: k0
  REAL,          INTENT(IN)    :: truelat1 
!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  piover2 = 2.0d0 * piover4
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2
  rad2deg = 1.8d2 / pi

  drearth = DBLE(rearth)
  k0=DCOS(DBLE(truelat1*deg2rad))
!-------------------------------------------------------------------------------
! Set up geometric constants.
!-------------------------------------------------------------------------------

  xxd  = DBLE(xx)
  yyd  = DBLE(yy)

!-------------------------------------------------------------------------------
! Compute latitude (PHI).
!-------------------------------------------------------------------------------
  phirad  = piover2 - 2.0d0 * DATAN ( DEXP(- yyd/drearth/k0) ) 
  phi     = REAL( phirad * rad2deg )

!-------------------------------------------------------------------------------
! Compute longitude (LAMBDA).
!-------------------------------------------------------------------------------

  lambda0rad = DBLE(lambda0) * deg2rad
  lambdarad  = lambda0rad + xxd/drearth/k0
  lambda     = REAL( lambdarad * rad2deg )

END SUBROUTINE xy2ll_merc
