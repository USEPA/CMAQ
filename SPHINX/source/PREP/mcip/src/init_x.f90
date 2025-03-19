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

SUBROUTINE init_x

!-------------------------------------------------------------------------------
! Name:     Initialize X arrays.
! Purpose:  Initializes X arrays.
! Revised:  26 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           30 Apr 1999  Replaced PSTAR with PRSFC.  (D. Byun)
!           19 Sep 2001  Converted to free-form f90.  Removed SDATE and STIME
!                        from routine.  Changed routine name from INITX to
!                        INIT_X.  (T. Otte)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           23 Jan 2002  Changed initialization of X-variables from 0.0 to
!                        BADVAL3 to avoid confusion.  (T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           09 Jun 2003  Added XF2DEF, XSNOCOV, XDELTA, XLSTWET, XRH.  Added
!                        new dry deposition species:  N2O5, NO3, and generic
!                        aldehyde.  (D. Schwede, T. Otte, and J. Pleim)
!                        Removed extraneous variables from output.  (T. Otte)
!           09 Aug 2004  Added XQGRAUP, XWSPD10, XWDIR10, and XT2.  Removed
!                        XFLAGS, XINDEX, and XNAMES.  (T. Otte and D. Schwede)
!           01 Dec 2004  Added XPURB.  (T. Otte)
!           04 Apr 2005  Removed unused variables XREGIME and XRTOLD.  Added
!                        initialization of WRF variables.  Changed XUU and XVV
!                        to XUU_D and XVV_D, and changed XUHAT and XVHAT to
!                        XUU_S and XVV_T.  Added pointer indices for optional
!                        chlorine and mercury species.  Removed XENTRP.  Added
!                        XU10 and XV10.  (T. Otte, S.-B. Kim, G. Sarwar, and
!                        R. Bullock)
!           19 Aug 2005  Removed initialization of XDEPIDX and XVD.  Moved
!                        XDEPSPC to INIT_DEPV.  Removed unused variables XCAPG,
!                        XMMPASS, and XFSOIL.  Removed array XRH and made it a
!                        local scalar in M3DRY.  (T. Otte and W. Hutzell)
!           14 Jul 2006  Removed XDELTA and XLSTWET to be local variables in
!                        M3DRY.  Added XLWMASK.  (T. Otte)
!           30 Jul 2007  Changed XUSTAR and XRADYN to 2D arrays without a
!                        dimension for fractional land use that was required
!                        for RADMdry.  Removed XRBNDY, XCFRACH, XCFRACM,
!                        XCFRACL, XTEMP1P5, and XTEMP10.  Create 2-m
!                        temperature array even if it is not part of input
!                        meteorology.  Changed 2-m temperature from XT2 to
!                        XTEMP2.  Removed internal variables for emissivity
!                        and net radiation.  Removed XF2DEF and XRSTMIN to be
!                        local variables in RESISTCALC.  Added XPSTAR0.  Added
!                        initialization for XDENSAF_REF.  (T. Otte)
!           21 Apr 2008  Added 2-m mixing ratio (XQ2) and turbulent kinetic
!                        energy (XTKE) arrays.  (T. Otte)
!           29 Oct 2009  Added potential vorticity (XPVC), Coriolis (XCORL),
!                        and potential temperature (XTHETA).  Added map-scale
!                        factors squared (on cross points, XMAPC2).  Added
!                        XLATU, XLONU, XMAPU, XLATV, XLONV, and XMAPV.  Allow
!                        output variable PURB to be created with urban model
!                        in WRF.  (T. Otte)
!           14 Dec 2010  Added sea ice.  (T. Otte)
!           11 Aug 2011  Replaced module PARMS3 with I/O API module M3UTILIO.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           10 Apr 2015  Added new array XCFRAC3D to pass 3D resolved cloud
!                        fraction to output.  (T. Spero)
!           21 Aug 2015  Changed latent heat flux from QFX to LH.  Fill THETA
!                        and add moisture flux (QFX) for IFMOLACM.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           16 Mar 2018  Added SNOWH to output.  Added XMUHYB to support hybrid
!                        vertical coordinate in WRF output.  Added XLUFRAC2,
!                        XMOSCATIDX, XLAI_MOS, XRA_MOS, XRS_MOS, XTSK_MOS, and
!                        XZNT_MOS to support NOAH Mosaic land-surface model.
!                        Added XZSOIL to define soil layer depths, and added
!                        3D soil arrays, XSOIT3D and XSOIM3D.  Added
!                        XWSPDSFC and XXLAIDYN for Noah.  (T. Spero)
!           26 Jun 2018  Now use netCDF tokens for missing data.  (T. Spero)
!           14 Sep 2018  Changed condition to enable hybrid vertical coordinate
!                        in WRF.  Removed support for MM5v3 input.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metinfo

  IMPLICIT NONE
 
!-------------------------------------------------------------------------------
! Initialize X-arrays.
!-------------------------------------------------------------------------------

  xx3face (:)     = fillreal  ;    xx3midl (:)     = fillreal

  xalbedo (:,:)   = fillreal  ;    xcfract (:,:)   = fillreal
  xcldbot (:,:)   = fillreal  ;    xcldtop (:,:)   = fillreal
  xdenss  (:,:)   = fillreal  ;    xdluse  (:,:)   = fillreal
  xglw    (:,:)   = fillreal  ;    xgsw    (:,:)   = fillreal
  xhfx    (:,:)   = fillreal  ;    xlai    (:,:)   = fillreal
  xlatc   (:,:)   = fillreal  ;    xlatd   (:,:)   = fillreal
  xlatu   (:,:)   = fillreal  ;    xlatv   (:,:)   = fillreal
  xlh     (:,:)   = fillreal  ;    xlonc   (:,:)   = fillreal
  xlond   (:,:)   = fillreal  ;    xlonu   (:,:)   = fillreal
  xlonv   (:,:)   = fillreal  ;    xlwmask (:,:)   = fillreal
  xmapc   (:,:)   = fillreal  ;    xmapc2  (:,:)   = fillreal
  xmapd   (:,:)   = fillreal  ;    xmapu   (:,:)   = fillreal
  xmapv   (:,:)   = fillreal  ;    xmol    (:,:)   = fillreal
  xpbl    (:,:)   = fillreal  ;    xprsfc  (:,:)   = fillreal
  xq2     (:,:)   = fillreal  ;    xradyn  (:,:)   = fillreal
  xrainc  (:,:)   = fillreal  ;    xrainn  (:,:)   = fillreal
  xrgrnd  (:,:)   = fillreal  ;    xrib    (:,:)   = fillreal
  xrstom  (:,:)   = fillreal  ;    xseaice (:,:)   = fillreal
  xsnocov (:,:)   = fillreal  ;    xsnowh  (:,:)   = fillreal
  xtemp2  (:,:)   = fillreal  ;    xtempg  (:,:)   = fillreal
  xtopo   (:,:)   = fillreal  ;    xustar  (:,:)   = fillreal
  xveg    (:,:)   = fillreal  ;    xwbar   (:,:)   = fillreal
  xwdir10 (:,:)   = fillreal  ;    xwr     (:,:)   = fillreal
  xwspd10 (:,:)   = fillreal  ;    xwstar  (:,:)   = fillreal
  xzruf   (:,:)   = fillreal

  IF ( met_hybrid >= 0 ) THEN
    xmuhyb(:,:)   = fillreal
  ENDIF

  IF ( ifw10m ) THEN
    xu10  (:,:)   = fillreal  ;    xv10    (:,:)   = fillreal
  ENDIF

  IF ( ( iflufrc ) .OR. ( met_urban_phys >= 1 ) ) THEN
    xpurb (:,:)   = fillreal
  ENDIF

  IF ( lpv > 0 ) THEN
    xcorl (:,:)   = fillreal
  ENDIF

  IF ( ifmolpx ) THEN
    xqfx  (:,:)   = fillreal
  ENDIF

  IF ( ifsoil ) THEN
    xsltyp(:,:)   = fillreal
    xt2a  (:,:)   = fillreal
    xtga  (:,:)   = fillreal 
    xw2a  (:,:)   = fillreal 
    xwga  (:,:)   = fillreal
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    xmu   (:,:)   = fillreal
    xgeof (:,:,:) = fillreal
  ENDIF

  x3htf   (:,:,:) = fillreal  ;    x3htm   (:,:,:) = fillreal
  x3jacobf(:,:,:) = fillreal  ;    x3jacobm(:,:,:) = fillreal
  xcldwtr (:,:,:) = fillreal  ;    xdensam (:,:,:) = fillreal
  xdenswm (:,:,:) = fillreal  ;    xdx3htf (:,:,:) = fillreal
  xluse   (:,:,:) = fillreal  ;    xpresm  (:,:,:) = fillreal
  xqgraup (:,:,:) = fillreal  ;    xqice   (:,:,:) = fillreal
  xqsnow  (:,:,:) = fillreal  ;    xranwtr (:,:,:) = fillreal
  xtempm  (:,:,:) = fillreal  ;    xuu_d   (:,:,:) = fillreal
  xuu_s   (:,:,:) = fillreal  ;    xvv_d   (:,:,:) = fillreal
  xvv_t   (:,:,:) = fillreal  ;    xwhat   (:,:,:) = fillreal
  xwvapor (:,:,:) = fillreal  ;    xwwind  (:,:,:) = fillreal

  IF ( iftke ) THEN
    xtke  (:,:,:) = fillreal
  ENDIF

  IF ( lpv > 0 ) THEN
    xpvc  (:,:,:) = fillreal
  ENDIF

  IF ( lpv > 0 .OR. ifmolpx ) THEN
    xtheta(:,:,:) = fillreal
  ENDIF

  IF ( ifcld3d ) THEN
    xcfrac3d(:,:,:) = fillreal
  ENDIF

  IF ( ( ifsoil ) .AND. ( metsoi > 0 ) ) THEN
    xzsoil (:)     = fillreal
    xsoit3d(:,:,:) = fillreal
    xsoim3d(:,:,:) = fillreal
  ENDIF

  IF ( nummosaic > 0 ) THEN
    xlufrac2  (:,:,:) = fillreal
    xmoscatidx(:,:,:) = fillreal
    xlai_mos  (:,:,:) = fillreal
    xra_mos   (:,:,:) = fillreal
    xrs_mos   (:,:,:) = fillreal
    xtsk_mos  (:,:,:) = fillreal
    xznt_mos  (:,:,:) = fillreal
    xwspdsfc  (:,:)   = fillreal
    xxlaidyn  (:,:)   = fillreal
  ENDIF

  IF ( ifpxwrf41 ) THEN
    xwsat_px  (:,:) = fillreal
    xwfc_px   (:,:) = fillreal
    xwwlt_px  (:,:) = fillreal
    xcsand_px (:,:) = fillreal
    xfmsand_px(:,:) = fillreal
    xclay_px  (:,:) = fillreal
  ENDIF

  IF ( ifkfradextras ) THEN
    xqc_cu  (:,:,:) = fillreal
    xqi_cu  (:,:,:) = fillreal
    xcldfrad(:,:,:) = fillreal
    xcldfras(:,:,:) = fillreal
  ENDIF

END SUBROUTINE init_x
