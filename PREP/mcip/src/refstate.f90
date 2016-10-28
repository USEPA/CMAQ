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

SUBROUTINE refstate

!-------------------------------------------------------------------------------
! Name:     Reference State
! Purpose:  Calculate non-hydrostatic reference state from meteorology input.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!           30 Mar 2005  Updated layer height calculations to use formulae
!                        defined by NCAR for MM5 base state rather than
!                        subroutine LAYHT.  Removed unused variables XDENSAM_REF
!                        and XENTRP.  (T. Otte)
!           31 Jul 2007  Captured PSTAR0 in global array XPSTAR0.  (T. Otte)
!           28 Apr 2008  Changed to use gravitational constant and dry gas
!                        constant values from MM5 system rather than from
!                        CMAQ system.  Removed explicit recalculation of P-star
!                        and use value read in from MM5 to fill XPSTAR0.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metinfo, aa => met_tlp, p00 => met_p00, ts0 => met_ts0
  USE metvars

  IMPLICIT NONE

  REAL,          PARAMETER     :: g_mm5      = 9.81    ! [m/s^2]
  REAL,          PARAMETER     :: r_mm5      = 287.04  ! [J/kg/K]
  REAL,          PARAMETER     :: gor        = g_mm5 / r_mm5
  REAL,          PARAMETER     :: rog        = r_mm5 / g_mm5

  REAL                         :: alogpf
  REAL                         :: alogpm
  INTEGER                      :: c
  INTEGER                      :: ec
  INTEGER                      :: er
  REAL                         :: g2ora
  INTEGER                      :: k
  INTEGER                      :: lvl
  REAL                         :: presf
  REAL                         :: presm
  REAL                         :: press
  INTEGER                      :: r
  REAL                         :: rao2g
  REAL                         :: rogt
  INTEGER                      :: sc
  INTEGER                      :: sr
  REAL                         :: tempf
  REAL                         :: tempm
  REAL                         :: ts0oa
  REAL                         :: tsfc

!-------------------------------------------------------------------------------
! Extract and define constants for reference calculations.
!-------------------------------------------------------------------------------

  ts0oa  = ts0 / aa
  g2ora  = 2.0 * gor / aa
  rao2g  = 0.5 * rog * aa
  rogt   = rog * ts0

!-------------------------------------------------------------------------------
! Calculate reference density, reference Jacobian, and reference height.
! Formulae for reference pressure, temperature, and height from MM5
! documentation (e.g., on-line tutorial notes in INTERPF section).
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xpstar0(:,:) = psa(sc:ec,sr:er)

  DO c = 1, ncols_x
    DO r = 1, nrows_x

      ! Comment out explicit calculation of P-star, and use value directly from
      ! MM5 instead.  Leave original calculation here in case someone wants to
      ! double-check values from MM5 against the calculation.

!!!   press  = p00 * EXP (- ts0oa + SQRT(ts0oa**2 - g2ora * xtopo(c,r)))
!!!   xpstar0 (c,r)   = press - x3top

      press  = xpstar0(c,r) + x3top
      tsfc   = ts0 + aa * LOG( press / p00 )

      xdenss  (c,r)   = press / ( r_mm5 * tsfc )
      xdensaf (c,r,0) = xdenss(c,r)
      x3jacobf(c,r,0) = xpstar0(c,r) / ( g_mm5 * xdenss(c,r) )  ! for JACOBS0
      x3htf   (c,r,0) = 0.0

      DO lvl = 1, metlay

        presm = x3top + xpstar0(c,r) * ( 1.0 - xx3midl(lvl) )
        presf = x3top + xpstar0(c,r) * ( 1.0 - xx3face(lvl) )

        alogpm = ALOG(presm/p00)
        alogpf = ALOG(presf/p00)

        tempm = ts0 + aa * alogpm
        tempf = ts0 + aa * alogpf

        xdensam(c,r,lvl)  = presm / ( r_mm5 * tempm )
        xdensaf(c,r,lvl)  = presf / ( r_mm5 * tempf )

        x3jacobm(c,r,lvl) = xpstar0(c,r) / ( g_mm5 * xdensam(c,r,lvl) )
        x3jacobf(c,r,lvl) = xpstar0(c,r) / ( g_mm5 * xdensaf(c,r,lvl) )

        x3htm(c,r,lvl)    = - (rao2g * alogpm**2 + rogt * alogpm)
        x3htm(c,r,lvl)    = x3htm(c,r,lvl) - xtopo(c,r)

        x3htf(c,r,lvl)    = - (rao2g * alogpf**2 + rogt * alogpf)
        x3htf(c,r,lvl)    = x3htf(c,r,lvl) - xtopo(c,r)

      ENDDO

    ENDDO
  ENDDO

  xdensaf_ref(:,:,:) = xdensaf(:,:,:)

  DO k = 1, metlay
    xdx3htf(:,:,k) = x3htf(:,:,k) - x3htf(:,:,k-1)  ! X3HTF vert starts at 0
  ENDDO

END SUBROUTINE refstate
