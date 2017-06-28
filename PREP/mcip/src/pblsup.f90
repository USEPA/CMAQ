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

SUBROUTINE pblsup

!-------------------------------------------------------------------------------
! Name:     PBL Supplement
! Purpose:  Computes 2-D parameters needed for CMAQ but not provided by
!           the meteorology model (i.e., XWSTAR, XTEMP10, XTEMP1P5, XWIND10,
!           and XALBEDO).
! Revised:  07 May 1998  Original version.  (J. Pleim)
!           20 Sep 2001  Converted to free-form f90 and integrated into MCIP
!                        for general distribution.  (T. Otte and J. Pleim)
!           23 Jan 2002  Changed missing value on XMOL to BADVAL3.  (T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           18 Mar 2003  Enabled last row and column of XUU and XVV to be
!                        used in calculation of UNS and VNS.  (T. Otte)
!           04 Aug 2004  Added provision to set minimum PBL heights when
!                        Mellor-Yamada (Eta) PBL scheme is used.  Changed local
!                        array allocation to occur only on initial call to
!                        subroutine to avoid memory fragmentation.  Removed
!                        XFLAGS.  Limited calculation of wind speed for initial
!                        time to the cross-point domain.  Removed unused
!                        variable, RA, from calling argument list to SFCLAYER.
!                        Changed WIND10 to WSPD10.  (T. Otte)
!           14 Apr 2005  Added changes for WRF.  Changed wind speed calculation
!                        to use wind components native to input meteorology
!                        model.  Added capability to use input 10-m wind
!                        components (rather than diagnosed) to calculate 10-m
!                        wind speed and direction.  Removed unnecessary 1-D
!                        arrays.  Corrected theta-v-star (TSTV) calculation.
!                        (T. Otte)
!           21 Jul 2005  Added provision to alternatively use XPBL to check for
!                        meteorology model initialization time.  (T. Otte)
!           19 Aug 2005  Rearranged K loops on calculation of winds to avoid
!                        segmentation fault on Linux PGF90v5 compilers at
!                        optimization of O2 and higher.  (T. Otte)
!           31 Jul 2007  Modified calculation of XWSTAR to remove dependency
!                        on function CVMGP.  Removed calculation of fractional
!                        USTAR for RADMdry.  Changed USTAR to a 2D array
!                        without a dimension for fractional land use that was
!                        required for RADMdry.  Removed 1.5-m and 10-m
!                        temperature calculations.  Compute 2-m temperature if
!                        it is unavailable in input meteorology.  Removed
!                        logical variable "PX" and introduced new logicals for
!                        specific fields.  Added call to RESISTCALC.  Simplified
!                        calculation of QL1.  (T. Otte)
!           29 Oct 2009  Changed real number comparisons from "equivalences" to
!                        "less than tolerances".  (T. Otte)
!           12 Feb 2010  Removed unused variable ILU.  Removed roughness length
!                        from argument list for subroutine SFCLAYER, and
!                        removed ZRUF from this routine.  (T. Otte)
!           11 Aug 2011  Replaced module PARMS3 with I/O API module M3UTILIO.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           03 Nov 2011  Added provision for calculating Monin-Obukhov length
!                        when the sensible heat flux is 0.  (T. Otte)
!           24 Aug 2015  Changed latent heat flux from QFX to LH.  Added
!                        calculation of Monin-Obukhov length for WRF/ACM2 so
!                        "corrector" part of predictor-corrector equation is
!                        used here; MOL output by WRF for ACM2 is "predictor"
!                        step.  MOL issue in WRF-CMAQ identified by H. Foroutan.
!                        Changed exponent in WSTAR calculation to remove the 7th
!                        decimal place to be consistent with the ACM2 use of
!                        that exponent in WRF.  Eliminated local variable UST.
!                        Changed Rd and Cp in calculation of CPAIR to match
!                        value used by WRF (from 1004.67 to 1004.5); note that
!                        both values differ from CPD used by CMAQ (in CONST.EXT)
!                        and given in MCIP's const_mod.f90.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           01 Mar 2017  Corrected the reference longitude for the wind
!                        direction calculation for both MM5 and WRF.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE const
  USE const_pbl
  USE metinfo
  USE m3utilio, ONLY: badval3

  IMPLICIT NONE

  REAL,          PARAMETER     :: amolmini   = 1.0 / amolmin
  INTEGER                      :: c
  INTEGER                      :: cp1
  REAL                         :: cpair
  REAL,          PARAMETER     :: ep1        = 0.608
  REAL                         :: hfx
  INTEGER                      :: k
  REAL                         :: lv
  REAL                         :: p2
  REAL                         :: ql1
  REAL                         :: qst
  INTEGER                      :: r
  INTEGER                      :: rp1
  REAL,          PARAMETER     :: smallnum   = 1.0e-7
  REAL                         :: thv1
  REAL                         :: tmpvtcon
  REAL                         :: theta1
  REAL                         :: theta2
  REAL                         :: thetav1
  REAL                         :: tst
  REAL                         :: tstv
  REAL                         :: u2
  REAL,    SAVE, ALLOCATABLE   :: ul         ( : )
  REAL                         :: ulev1
  REAL                         :: uns
  REAL                         :: vlev1
  REAL                         :: vns
  REAL                         :: wvflx
  REAL                         :: ztemp

  INTERFACE

    SUBROUTINE getpblht (c, r, wspd)
      IMPLICIT NONE
      INTEGER,       INTENT(IN)    :: c
      INTEGER,       INTENT(IN)    :: r
      REAL,          INTENT(IN)    :: wspd       ( : )
    END SUBROUTINE getpblht

  END INTERFACE

!-------------------------------------------------------------------------------
! For the time period that corresponds to a meteorology model initialization
! time, many PBL variables are not defined.  At the initialization time for the
! meteorology model, the XUSTAR array may contain all 0.0 values or the XPBL
! array may contain all 0.0 values.  In either case, set place-holder values for
! variables that would otherwise be calculated in this routine.
!-------------------------------------------------------------------------------

  IF ( ( ABS(MAXVAL(xustar)) < smallnum ) .OR.  &
       ( ABS(MAXVAL(xpbl))   < smallnum ) ) THEN  ! assume initialization period

    xwstar  (:,:) = 0.0
    xmol    (:,:) = badval3   ! inverse taken in metcro.F

    ! Compute 10-m wind speed and direction on scalar points.

    IF ( met_model == 1 ) THEN  ! MM5: use native dot-point winds

      DO c = 1, ncols_x
        cp1 = c + 1
        DO r = 1, nrows_x
          rp1 = r + 1
          uns = 0.25 * ( xuu_d(c,r,  1) + xuu_d(cp1,r,  1) +   &
                         xuu_d(c,rp1,1) + xuu_d(cp1,rp1,1) )
          vns = 0.25 * ( xvv_d(c,r,  1) + xvv_d(cp1,r,  1) +   &
                         xvv_d(c,rp1,1) + xvv_d(cp1,rp1,1) )
          xwspd10(c,r) = SQRT((uns * uns) + (vns * vns))
          CALL wind (uns, vns, xwspd10(c,r), xwdir10(c,r),  &
                     xlonc(c,r), met_proj_clon, met_cone_fac)
        ENDDO
      ENDDO

    ELSE IF ( met_model == 2 ) THEN  ! WRF: use native flux-point winds

      DO c = 1, ncols_x
        cp1 = c + 1
        DO r = 1, nrows_x
          rp1 = r + 1
          uns = 0.5 * ( xuu_s(c,r,1) + xuu_s(cp1,r,1) )
          vns = 0.5 * ( xvv_t(c,r,1) + xvv_t(c,rp1,1) )
          xwspd10(c,r) = SQRT((uns * uns) + (vns * vns))
          CALL wind (uns, vns, xwspd10(c,r), xwdir10(c,r),  &
                     xlonc(c,r), met_proj_clon, met_cone_fac)
        ENDDO
      ENDDO

    ENDIF

  ELSE

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

    IF ( .NOT. ALLOCATED ( ul ) ) ALLOCATE ( ul (metlay) )

!-------------------------------------------------------------------------------
! Compute additional variables.
!-------------------------------------------------------------------------------

    DO c = 1, ncols_x
      cp1 = c + 1

      DO r = 1, nrows_x
        rp1 = r + 1

        ! Compute wind speed profile on scalar points.

        IF ( met_model == 1 ) THEN  ! MM5: use native dot-point winds

          DO k = 1, metlay

            uns = 0.25 * ( xuu_d(c,r,  k) + xuu_d(cp1,r,  k) +   &
                           xuu_d(c,rp1,k) + xuu_d(cp1,rp1,k) )

            vns = 0.25 * ( xvv_d(c,r,  k) + xvv_d(cp1,r,  k) +   &
                           xvv_d(c,rp1,k) + xvv_d(cp1,rp1,k) )

            ul(k) = MAX( 0.5, SQRT( (uns * uns) + (vns * vns) ) )

            IF ( k == 1 ) THEN
              IF ( ifw10m ) THEN
                ulev1 = xu10(c,r)  ! 10-m wind components already on scalar pts
                vlev1 = xv10(c,r)
              ELSE
                ulev1 = uns
                vlev1 = vns
              ENDIF
            ENDIF

          ENDDO

        ELSE IF ( met_model == 2 ) THEN  ! WRF: use native flux-point winds

          DO k = 1, metlay

            uns = 0.5 * ( xuu_s(c,r,k) + xuu_s(cp1,r,k) )
            vns = 0.5 * ( xvv_t(c,r,k) + xvv_t(c,rp1,k) )

            ul(k) = MAX( 0.5, SQRT( (uns * uns) + (vns * vns) ) )

            IF ( k == 1 ) THEN
              IF ( ifw10m ) THEN
                ulev1 = xu10(c,r)  ! 10-m wind components already on scalar pts
                vlev1 = xv10(c,r)
              ELSE
                ulev1 = uns
                vlev1 = vns
              ENDIF
            ENDIF

          ENDDO

        ENDIF

        ! Estimate near-surface variables.

        ql1    = xwvapor(c,r,1)
        cpair  = cp * (1.0 + 0.84 * ql1)   ! [J / kg K]
        hfx    = -xhfx(c,r) / (xdensam(c,r,1) * cpair)
        theta1 = xtempm(c,r,1) * (100000.0/xpresm(c,r,1))**0.286

        ! Calculate Monin-Obukhov length if unavailable in input meteorology,
        ! except if missing from WRF/ACM2 simulation...which is done, below.

        IF ( .NOT. ifmol .AND. .NOT. ifmolpx ) THEN
          thetav1   = theta1 * (1.0 + ep1 * ql1)            
          lv        = lv0 - dlvdt * (xtempg(c,r) -  stdtemp) 
          wvflx     = xlh(c,r) / lv
          tstv      = ( hfx * (1.0 + ep1 * ql1) +  &
                        (ep1 * theta1 * wvflx)/xdensam(c,r,1) ) / xustar(c,r)
          IF ( ABS(tstv) > 0.000001 ) THEN
            xmol(c,r) = thetav1 * xustar(c,r) * xustar(c,r) /  &
                        (vkar * grav * tstv)
          ELSE
            xmol(c,r) = 1.0e7  ! small number for inverse MOL when HFX = 0.0
          ENDIF
        ENDIF

        ! Recalculate Monin-Obukhov length when ACM2 PBL is used in WRF.
        ! MOL output by WRF is the "predictor" from the predictor-corrector
        ! equation.  Recalculation here uses algorithms and constants from
        ! WRF phys/module_bl_acm.f90 and share/module_model_constants.f90
        ! as of WRFv3.7.

        IF ( ifmolpx ) THEN
          tmpvtcon = 1.0 + ep_1 * ql1
          tst      = hfx / xustar(c,r)
          qst      = -xqfx(c,r) / (xustar(c,r) * xdensam(c,r,1))
          thv1     = tmpvtcon * xtheta(c,r,1)
          tstv     = tst*tmpvtcon + thv1*ep_1*qst
          IF ( ABS(tstv) < 1.0e-6 ) THEN
            tstv = SIGN( 1.0e-6, tstv )
          ENDIF
          xmol(c,r) = thv1 * xustar(c,r) * xustar(c,r) / (vkar * grav * tstv)
        ENDIF

        ! Limit MOL.
        ! (Note that this is not part of ACM2 in WRF, but  per J. Pleim,
        ! we will apply it for all MOL coming out of MCIP.  TLS 24 Aug 2015)

        xmol(c,r) = SIGN( MAX(ABS(xmol(c,r)), amolmini), xmol(c,r) )

        ! Ensure that PBL heights from the Mellor-Yamada Eta PBL scheme in MM5
        ! are set to a minimum of the height of the lowest model layer to ensure
        ! that very small values and negative values are not used by CMAQ.

        IF ( ( met_model == 1 ) .AND. ( met_pbl == 4 ) ) THEN  ! M-Y Eta PBL
          xpbl(c,r) = MAX( xpbl(c,r), x3htf(c,r,1) )
        ENDIF

        ! Need to supply PBL height when Blackadar, MRF, or Gayno-Seaman PBL
        ! schemes in MM5 suggest PBL height of 0.0.  (Blackadar and MRF regimes
        ! 1 and 2, and G-S regime 1.)  Also need to specify PBL height for
        ! schemes in WRF when it is input as lower than the height of the
        ! lowest mid-layer.

        IF ( xpbl(c,r) <= x3htm(c,r,1) ) THEN
          CALL getpblht (c, r, ul)
        ENDIF

        ! Calculate near-surface temperature and wind.  If 10-m wind components
        ! are not provided in met model file, then 10-m wind speed will be
        ! calculated using similarity theory in sfclayer, and 10-m wind
        ! direction is calculated from layer-1 wind components (i.e., no
        ! directional shear assumed between layer-1 and surface).

        CALL wind (ulev1, vlev1, xwspd10(c,r), xwdir10(c,r),  &
                   xlonc(c,r), met_proj_clon, met_cone_fac)

        IF ( .NOT. ifw10m ) THEN
          ztemp = 10.0  ! [m]
          CALL sfclayer (theta1, theta2, ul(1), u2, x3htm(c,r,1), ztemp,  &
                         hfx, xustar(c,r), xmol(c,r))
          xwspd10(c,r) = u2
        ENDIF

        IF ( .NOT. ift2m ) THEN
          ztemp = 2.0  ! [m]
          p2    = xpresm(c,r,1) + grav * xdensam(c,r,1) * (x3htm(c,r,1) - ztemp)

          CALL sfclayer (theta1, theta2, ul(1), u2, x3htm(c,r,1), ztemp,  &
                         hfx, xustar(c,r), xmol(c,r))

          xtemp2(c,r) = theta2 * (p2 / 100000.0)**0.286
        ENDIF

        ! Compute W-star.

        IF ( xmol(c,r) < 0.0 ) THEN
          xwstar(c,r) = xustar(c,r) * ( xpbl(c,r) /   & 
                        ( vkar * ABS( xmol(c,r) ) ) )**0.333333
        ELSE
          xwstar(c,r) = 0.0
        ENDIF

      ENDDO
    ENDDO

!-------------------------------------------------------------------------------
! Dellocate variables.
!-------------------------------------------------------------------------------

!   DEALLOCATE ( ul )  ! commented out to avoid memory fragmentation

  ENDIF

!-------------------------------------------------------------------------------
! Compute aerodynamic and stomatal resistances if they are not in input
! meteorology.
!-------------------------------------------------------------------------------

  IF ( .NOT. ifresist ) THEN
    CALL resistcalc
  ENDIF

END SUBROUTINE pblsup
