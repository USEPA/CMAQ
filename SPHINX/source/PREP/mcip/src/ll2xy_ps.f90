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

SUBROUTINE ll2xy_ps (phi, lambda, phi1, lambda0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Polar Stereographic Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and polar stereographic projection information.
! Notes:    Adapted from equations found at http://starbase.jpl.nasa.gov/
!           mgn-v-rdrs-5-dvdr-v1.0/gvdr0001/catalog/dsmp.lbl.
! Revised:  28 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const, ONLY: rearth

  IMPLICIT NONE

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8)                      :: drearth ! earth radius [m]
  REAL(8)                      :: hemi    ! +/-1 for Northern/Southern Hemis
  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad ! true latitude 1 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4
  REAL(8)                      :: scalefac
  REAL(8)                      :: sigma   ! image scale
  REAL(8)                      :: theta   ! polar angle
  REAL(8)                      :: tt
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
! Compute image scale, SIGMA.
!-------------------------------------------------------------------------------

  hemi = DSIGN (1.0d0, DBLE(phi1))

  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phirad  = DBLE(phi)  * deg2rad  ! convert PHI  from degrees to radians

  sigma   = (1.0d0 + DSIN(phi1rad)) / 2.0d0 * hemi

  scalefac = drearth / sigma

  tt = DTAN ( piover4 - phirad/2.0d0)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  theta = DBLE(lambda - lambda0) * deg2rad

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx = REAL(         2.0d0 * scalefac * tt * DSIN(theta) )
  yy = REAL( -hemi * 2.0d0 * scalefac * tt * DCOS(theta) )

END SUBROUTINE ll2xy_ps
