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

SUBROUTINE sfclayer (theta1, theta2, u1, u2, z1, z2, hfx, ustar, amol)

!-------------------------------------------------------------------------------
! Name:     Surface Layer
! Purpose:  Interpolate temperature to a given height using surface layer
!           similarity based on Hogstrom (1988).
! Revised:  13 Oct 1998  Original version.  (J. Pleim)
!           20 Sep 2001  Converted to free-form f90.  (T. Otte)
!           25 Jan 2002  Corrected error in calculation of PSIM.  (T. Otte)
!           04 Aug 2004  Removed unused variable, RA, from calling argument
!                        list.  (T. Otte)
!           20 Jun 2006  Removed unused variable ALOGZ1Z0.  (T. Otte)
!           12 Feb 2010  Removed unused argument Z0.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const_pbl

  IMPLICIT NONE

  REAL                         :: alogz1z2
  REAL,          INTENT(IN)    :: amol       ! Monin-Obukhov length [m]
  REAL,          INTENT(IN)    :: hfx        ! sensible heat flux [W/m^2]
  REAL                         :: psih
  REAL                         :: psih0
  REAL                         :: psim
  REAL                         :: psim0
  REAL,          INTENT(IN)    :: theta1     ! pot'l temp at z1 [K]
  REAL,          INTENT(OUT)   :: theta2     ! pot'l temp at z2 [K]
  REAL,          INTENT(IN)    :: u1         ! wind speed at z1 [m/s]
  REAL,          INTENT(OUT)   :: u2         ! wind speed at z2 [m/s]
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
       psim0 = 1.0 - betam - z1ol
    ELSE
       psih0 = - betah * z1ol
       psim0 = - betam * z1ol
    ENDIF

    IF ( z2ol > 1.0 ) THEN
       psih = psih0 - (1.0 - betah - z2ol)
       psim = psim0 - (1.0 - betam - z2ol)
    ELSE
       psih = psih0 + betah * z2ol
       psim = psim0 + betam * z2ol
    ENDIF

  ELSE

    psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - gamah*z1ol)) /  &
                       (1.0 + SQRT(1.0 - gamah*z2ol)) )
    x1   = (1.0 - gamam * z1ol)**0.25
    x2   = (1.0 - gamam * z2ol)**0.25
    psim = 2.0 * ALOG( (1.0+x1) / (1.0+x2) ) +        &
                 ALOG( (1.0+x1*x1) / (1.0+x2*x2)) -   &
                 2.0 * ATAN(x1) + 2.0 * ATAN(x2)

  ENDIF
      
  theta2 = theta1 - pro*hfx / (vkar*ustar) * (alogz1z2-psih)

  u2     = u1     - ustar/vkar * (alogz1z2-psim)
  u2     = MAX (u2, 0.1)
      
END SUBROUTINE sfclayer
