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

SUBROUTINE init_met

!-------------------------------------------------------------------------------
! Name:     Initialize Meteorology Arrays
! Purpose:  Initializes meteorology arrays.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Removed unused variables REGIME and MAVAIL.  Added PH,
!                        PHB, PB, MU, and MUB for WRF.  Added U10 and V10.
!                        (T. Otte and S.-B. Kim)
!           11 Aug 2005  Removed unused variable FSOIL.  (T. Otte)
!           19 Jun 2006  Corrected initial values for ISLTYP and LANDUSE.
!                        (T. Otte)
!           25 Jul 2007  Removed internal variables for emissivity and net
!                        radiation.  Eliminated logical variable "PX" to make
!                        code more general.  (T. Otte)
!           05 May 2008  Added 2-m mixing ratio (Q2) and turbulent kinetic
!                        energy (TKE) arrays.  Added urban fraction (FRC_URB)
!                        and urban roughness length (Z0C_URB2D) for
!                        MET_UCMCALL=1.  (T. Otte)
!           29 Sep 2009  Added THETA and CORIOLIS for when potential vorticity
!                        is needed.  Added LATU, LONU, MAPU, LATV, LONV, and
!                        MAPV.  Removed Z0C_URB2D.  (T. Otte)
!           15 Dec 2010  Added sea ice.  Added tipping buckets for convective
!                        and non-convective precipitation.  (T. Otte)
!           01 Sep 2011  Corrected initialization of ISLTYP to match variable
!                        type.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           11 Sep 2012  Added LANDMASK to be read from WRF.  (T. Otte)
!           10 Apr 2015  Added new array CLDFRA to pass 3D resolved cloud
!                        fraction to output.  (T. Spero)
!           21 Aug 2015  Changed latent heat flux from QFX to LH.  Added
!                        moisture flux (QFX) for IFMOLACM.  (T. Spero)
!           16 Mar 2018  Added SNOWH to output.  Added C1H, C2H, C1F, and C2F to
!                        support hybrid vertical coordinate in WRF.  Added
!                        LUFRAC2, MOSCATIDX, LAI_MOS, RA_MOS, RS_MOS, TSK_MOS,
!                        and ZNT_MOS to support NOAH Mosaic land-surface model.
!                        Added DZS to capture soil layers, and added 3D soil
!                        arrays, SOIT3D and SOIM3D.  Added WSPDSFC and XLAIDYN
!                        for Noah.  (T. Spero)
!           14 Sep 2018  Changed condition to enable hybrid vertical coordinate
!                        in WRF.  Removed support for MM5v3 input.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Initialize meteorology arrays.
!-------------------------------------------------------------------------------

  IF ( met_hybrid >= 0 ) THEN  ! using hybrid vertical coordinate in WRF
    c1f    (:)     = 0.0      ;      c1h      (:)     = 0.0
    c2f    (:)     = 0.0      ;      c2f      (:)     = 0.0
  ENDIF

  sigmaf   (:)     = 0.0      ;      sigmah   (:)     = 0.0

  IF ( met_ns > 0 ) THEN  ! using multi-layer land-surface model
    dzs    (:)     = 0.0
  ENDIF

  albedo   (:,:)   = 0.0      ;      glw      (:,:)   = 0.0
  groundt  (:,:)   = 0.0      ;      hfx      (:,:)   = 0.0
  i_rainc  (:,:)   = 0        ;      i_rainnc (:,:)   = 0
  ircold   (:,:)   = 0        ;      irnold   (:,:)   = 0
  landmask (:,:)   = 0.0      ;      landuse  (:,:)   = 0
  latcrs   (:,:)   = 0.0      ;      latdot   (:,:)   = 0.0
  latu     (:,:)   = 0.0      ;      latv     (:,:)   = 0.0
  lh       (:,:)   = 0.0      ;      loncrs   (:,:)   = 0.0
  londot   (:,:)   = 0.0      ;      lonu     (:,:)   = 0.0
  lonv     (:,:)   = 0.0      ;      mapcrs   (:,:)   = 0.0
  mapdot   (:,:)   = 0.0      ;      mapu     (:,:)   = 0.0
  mapv     (:,:)   = 0.0      ;      psa      (:,:)   = 0.0
  raincon  (:,:)   = 0.0      ;      rainnon  (:,:)   = 0.0
  rcold    (:,:)   = 0.0      ;      rgrnd    (:,:)   = 0.0
  rnold    (:,:)   = 0.0      ;      seaice   (:,:)   = 0.0
  snowcovr (:,:)   = 0.0      ;      snowh    (:,:)   = 0.0
  terrain  (:,:)   = 0.0      ;      ust      (:,:)   = 0.0
  znt      (:,:)   = 0.0      ;      zpbl     (:,:)   = 0.0

  IF ( ift2m ) THEN  ! 2-m temperature available
    t2     (:,:)   = 0.0
  ENDIF

  IF ( ifq2m ) THEN  ! 2-m mixing ratio available
    q2     (:,:)   = 0.0
  ENDIF

  IF ( ifw10m ) THEN  ! 10-m wind components available
    u10    (:,:)   = 0.0
    v10    (:,:)   = 0.0
  ENDIF

  pp       (:,:,:) = 0.0      ;      qca      (:,:,:) = 0.0
  qga      (:,:,:) = 0.0      ;      qia      (:,:,:) = 0.0
  qra      (:,:,:) = 0.0      ;      qsa      (:,:,:) = 0.0
  qva      (:,:,:) = 0.0      ;      ta       (:,:,:) = 0.0
  ua       (:,:,:) = 0.0      ;      va       (:,:,:) = 0.0
  wa       (:,:,:) = 0.0

  IF ( iflufrc ) THEN  ! fractional land use fields available
    lufrac   (:,:,:) = 0.0
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    mu     (:,:)   = 0.0      ;      mub      (:,:)   = 0.0
    pb     (:,:,:) = 0.0
    ph     (:,:,:) = 0.0      ;      phb      (:,:,:) = 0.0
  ENDIF

  IF ( ALLOCATED ( coriolis  ) )  coriolis (:,:) = 0.0
  IF ( ALLOCATED ( isltyp    ) )  isltyp   (:,:) = 0
  IF ( ALLOCATED ( lai       ) )  lai      (:,:) = 0.0
  IF ( ALLOCATED ( mol       ) )  mol      (:,:) = 0.0
  IF ( ALLOCATED ( qfx       ) )  qfx      (:,:) = 0.0
  IF ( ALLOCATED ( ra        ) )  ra       (:,:) = 0.0
  IF ( ALLOCATED ( rstom     ) )  rstom    (:,:) = 0.0
  IF ( ALLOCATED ( soilt1    ) )  soilt1   (:,:) = 0.0
  IF ( ALLOCATED ( soilt2    ) )  soilt2   (:,:) = 0.0
  IF ( ALLOCATED ( veg       ) )  veg      (:,:) = 0.0
  IF ( ALLOCATED ( w2        ) )  w2       (:,:) = 0.0
  IF ( ALLOCATED ( wg        ) )  wg       (:,:) = 0.0
  IF ( ALLOCATED ( wr        ) )  wr       (:,:) = 0.0

  IF ( ALLOCATED ( tke       ) )  tke      (:,:,:) = 0.0
  IF ( ALLOCATED ( theta     ) )  theta    (:,:,:) = 0.0

  IF ( ALLOCATED ( frc_urb   ) )  frc_urb  (:,:) = 0.0

  IF ( ALLOCATED ( cldfra    ) )  cldfra   (:,:,:) = 0.0

  IF ( ALLOCATED ( soim3d    ) )  soim3d   (:,:,:) = 0.0
  IF ( ALLOCATED ( soit3d    ) )  soit3d   (:,:,:) = 0.0

  IF ( ALLOCATED ( lufrac2   ) )  lufrac2  (:,:,:) = 0.0
  IF ( ALLOCATED ( moscatidx ) )  moscatidx(:,:,:) = 0
  IF ( ALLOCATED ( lai_mos   ) )  lai_mos  (:,:,:) = 0.0
  IF ( ALLOCATED ( ra_mos    ) )  ra_mos   (:,:,:) = 0.0
  IF ( ALLOCATED ( rs_mos    ) )  rs_mos   (:,:,:) = 0.0
  IF ( ALLOCATED ( tsk_mos   ) )  tsk_mos  (:,:,:) = 0.0
  IF ( ALLOCATED ( znt_mos   ) )  znt_mos  (:,:,:) = 0.0

  IF ( ALLOCATED ( wspdsfc   ) )  wspdsfc  (:,:)   = 0.0
  IF ( ALLOCATED ( xlaidyn   ) )  xlaidyn  (:,:)   = 0.0

  IF ( ALLOCATED ( lai_px    ) )  lai_px   (:,:)   = 0.0
  IF ( ALLOCATED ( wwlt_px   ) )  wwlt_px  (:,:)   = 0.0
  IF ( ALLOCATED ( wsat_px   ) )  wsat_px  (:,:)   = 0.0
  IF ( ALLOCATED ( wfc_px    ) )  wfc_px   (:,:)   = 0.0
  IF ( ALLOCATED ( csand_px  ) )  csand_px (:,:)   = 0.0
  IF ( ALLOCATED ( fmsand_px ) )  fmsand_px(:,:)   = 0.0
  IF ( ALLOCATED ( clay_px   ) )  clay_px  (:,:)   = 0.0

  IF ( ALLOCATED ( qc_cu     ) )  qc_cu    (:,:,:) = 0.0
  IF ( ALLOCATED ( qi_cu     ) )  qi_cu    (:,:,:) = 0.0
  IF ( ALLOCATED ( cldfra_dp ) )  cldfra_dp(:,:,:) = 0.0
  IF ( ALLOCATED ( cldfra_sh ) )  cldfra_sh(:,:,:) = 0.0

END SUBROUTINE init_met
