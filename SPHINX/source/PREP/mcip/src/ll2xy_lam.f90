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

SUBROUTINE ll2xy_lam (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Determines secant or tangent Lambert conformal case, and calls
!           appropriate routine.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           26 Nov 2008  Added argument for reference latitude, PHI0.
!                        Prevent users from having tangent Lambert conformal
!                        case until it can be tested with the Spatial
!                        Allocator.  (Known problem is that the Spatial
!                        Allocator does not work properly when the
!                        reference latitude is equal to the first true
!                        latitude.  Work-around is to set reference latitude
!                        to average of true latitudes for Lambert conformal.
!                        But average of true latiudes for tangent Lambert
!                        conformal case is the first true latitude, which
!                        will result in the same problem as solution used
!                        in MCIPv3.4.)  (T. Otte)
!           01 Sep 2011  Improved error handling.  Changed XX and YY from
!                        double-precision to single-precision reals.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,               INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,               INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,               INTENT(IN)    :: phi     ! latitude [deg]
  REAL,               INTENT(IN)    :: phi0    ! reference latitude [deg]
  REAL,               INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL,               INTENT(IN)    :: phi2    ! true latitude 2 [deg]
  REAL,               PARAMETER     :: phitol  = 0.001  ! tolerance [deg]
  CHARACTER(LEN=16),  PARAMETER     :: pname   = 'LL2XY_LAM'
  REAL,               INTENT(OUT)   :: xx      ! X-coordinate from origin
  REAL,               INTENT(OUT)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   TANGENT LAMBERT CONFORMAL PROJECTION DETECTED', &
    & /, 1x, '***   TRUE LATITUDES = ', f8.3, 2x, f8.3, &
    & /, 1x, '***   MAY NOT WORK PROPERLY IN SPATIAL ALLOCATOR', &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Determine whether Lambert conformal is tangent or secant.
!-------------------------------------------------------------------------------

  IF ( ABS( phi1 - phi2 ) < phitol ) THEN  ! tangent case
    WRITE (*,f9000) TRIM(pname), phi1, phi2
    CALL graceful_stop (pname)
!   CALL ll2xy_lam_tan (phi, lambda, phi1, lambda0, xx, yy)
  ELSE  ! secant case
    CALL ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)
  ENDIF

END SUBROUTINE ll2xy_lam
