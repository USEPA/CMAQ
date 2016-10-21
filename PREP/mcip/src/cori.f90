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

REAL FUNCTION cori (phi)

!-------------------------------------------------------------------------------
! Name:     Coriolis parameter
! Purpose:  Calculates Coriolis parameter (vertical component of the earth's
!           vorticity) from latitude.
! Revised:  03 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
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
