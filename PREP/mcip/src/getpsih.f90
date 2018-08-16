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

SUBROUTINE getpsih (z1, z2, ustar, amol, psih)

!-------------------------------------------------------------------------------
! Name:     Surface Layer
! Purpose:  Compute PSIH using a mosaic USTAR to get aerodynamic resistance.
!           Uses similarity based on Hogstrom (1988).
! Notes:    Adapted from sfclayer.f90 from MCIPv4.4.
! Revised:  10 Feb 2018  Original version.  (T. Spero)
!-------------------------------------------------------------------------------

  USE const_pbl

  IMPLICIT NONE

  REAL                         :: alogz1z2
  REAL,          INTENT(IN)    :: amol       ! Monin-Obukhov length [m]
  REAL,          INTENT(OUT)   :: psih
  REAL                         :: psih0
  REAL,          INTENT(IN)    :: ustar      ! friction velocity [m/s]
  REAL                         :: x1
  REAL                         :: x2
  REAL,          INTENT(IN)    :: z1         ! height [m]
  REAL                         :: z1ol
  REAL,          INTENT(IN)    :: z2         ! height [m]
  REAL                         :: z2ol

  ! Compute psi functions from aerodynamic resistance.

  z1ol = z1 / amol
  z2ol = z2 / amol

  alogz1z2 = ALOG(z1/z2)

  IF ( z1ol >= 0.0 ) THEN

    IF ( z1ol > 1.0 ) THEN
       psih0 = 1.0 - betah - z1ol
    ELSE
       psih0 = - betah * z1ol
    ENDIF

    IF ( z2ol > 1.0 ) THEN
       psih = psih0 - (1.0 - betah - z2ol)
    ELSE
       psih = psih0 + betah * z2ol
    ENDIF

  ELSE

    psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - gamah*z1ol)) /  &
                       (1.0 + SQRT(1.0 - gamah*z2ol)) )

  ENDIF
      
END SUBROUTINE getpsih
