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

SUBROUTINE alloc_met

!-------------------------------------------------------------------------------
! Name:     Allocate Meteorology Variables
! Purpose:  Allocate arrays for input meteorology variables.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Changed array dimensions from I,J to X,Y to make code
!                        more general.  Now all meteorology arrays will have
!                        the east-west index first, as opposed to the standard
!                        MM5 convention.  For MM5 input, arrays are transposed
!                        to the new convention in either RDMM5V2 or RDMM5V3.
!                        Removed unused variables REGIME and MAVAIL.  Added PH,
!                        PHB, PB, MU, and MUB for WRF.  Added U10 and V10.
!                        (T. Otte and S.-B. Kim)
!           11 Aug 2005  Removed unused variable FSOIL.  (T. Otte)
!           25 Jul 2007  Removed internal variables for emissivity and net
!                        radiation.  Eliminated logical variable "PX" to make
!                        code more general.  (T. Otte)
!           05 May 2008  Added 2-m mixing ratio (Q2) and turbulent kinetic
!                        energy (TKE) arrays.  Added urban fraction (FRC_URB)
!                        and urban roughness length (Z0C_URB2D) for
!                        MET_UCMCALL=1.  (T. Otte)
!           29 Oct 2009  Changed MET_UCMCALL to MET_URBAN_PHYS, and allowed
!                        for variable to be set to be greater than 1.  Added
!                        THETA and CORIOLIS for when potential vorticity is
!                        needed.  Added LATU, LONU, MAPU, LATV, LONV, and
!                        and MAPV.  Removed Z0C_URB2D.  (T. Otte)
!           15 Dec 2010  Added sea ice.  Added tipping buckets for convective
!                        and non-convective precipitation.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           11 Sep 2012  Added LANDMASK to be read from WRF.  (T. Otte)
!           10 Apr 2015  Added new array CLDFRA to pass 3D resolved cloud
!                        fraction to output.  (T. Spero)
!           21 Aug 2015  Changed latent heat flux from QFX to LH.  Fill THETA
!                        and add moisture flux (QFX) for IFMOLACM.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           16 Mar 2018  Added SNOWH to output.  Added C1H, C2H, C1F, and C2F to
!                        support hybrid vertical coordinate in WRF.  Added
!                        LUFRAC2, MOSCATIDX, ZNT_MOS, TSK_MOS, RA_MOS, RS_MOS,
!                        and LAI_MOS for NOAH Mosaic land-surface model.
!                        Added DZS, SOIT3D, and SOIM3D.  Added WSPDSFC and
!                        XLAIDYN for Noah.  (T. Spero)
!           27 Jun 2018  Removed local aliases for dimensions of input
!                        meteorological fields.  (T. Spero)
!           14 Sep 2018  Changed condition to enable hybrid vertical coordinate
!                        from WRF.  Removed support for MM5v3 input.  (T. Spero)
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
! Allocate time-invariant fields.
!-------------------------------------------------------------------------------

  ALLOCATE ( albedo   (met_nx, met_ny) )   ! time varying in P-X LSM
  ALLOCATE ( landmask (met_nx, met_ny) )   ! time varying in NOAH LSM
  ALLOCATE ( landuse  (met_nx, met_ny) )
  ALLOCATE ( latcrs   (met_nx, met_ny) )
  ALLOCATE ( latdot   (met_nx, met_ny) )
  ALLOCATE ( latu     (met_nx, met_ny) )
  ALLOCATE ( latv     (met_nx, met_ny) )
  ALLOCATE ( loncrs   (met_nx, met_ny) )
  ALLOCATE ( londot   (met_nx, met_ny) )
  ALLOCATE ( lonu     (met_nx, met_ny) )
  ALLOCATE ( lonv     (met_nx, met_ny) )
  ALLOCATE ( mapcrs   (met_nx, met_ny) )
  ALLOCATE ( mapdot   (met_nx, met_ny) )
  ALLOCATE ( mapu     (met_nx, met_ny) )
  ALLOCATE ( mapv     (met_nx, met_ny) )
  ALLOCATE ( sigmaf                   (met_nz+1) )
  ALLOCATE ( sigmah                   (met_nz) )
  ALLOCATE ( terrain  (met_nx, met_ny) )
  ALLOCATE ( znt      (met_nx, met_ny) )

  IF ( iflufrc ) THEN
    ALLOCATE ( lufrac (met_nx, met_ny, nummetlu) )
    IF ( ifmosaic ) THEN
      ALLOCATE ( lufrac2   (met_nx, met_ny, nummetlu) )
      ALLOCATE ( moscatidx (met_nx, met_ny, nummetlu) )
    ENDIF
  ENDIF

  IF ( lpv > 0 ) THEN  ! potential vorticity; get Coriolis
    ALLOCATE ( coriolis (met_nx, met_ny) )
  ENDIF

  IF ( met_hybrid >= 0 ) THEN
    ALLOCATE ( c1f (met_nz+1) )
    ALLOCATE ( c1h (met_nz)   )
    ALLOCATE ( c2f (met_nz+1) )
    ALLOCATE ( c2h (met_nz)   )
  ENDIF

  IF ( met_ns > 0 ) THEN
    ALLOCATE ( dzs (met_ns) )
  ENDIF

!-------------------------------------------------------------------------------
! Allocate time-varying fields.
!-------------------------------------------------------------------------------

  ALLOCATE ( glw     (met_nx, met_ny) )
  ALLOCATE ( groundt (met_nx, met_ny) )
  ALLOCATE ( hfx     (met_nx, met_ny) )
  ALLOCATE ( i_rainc (met_nx, met_ny) )
  ALLOCATE ( i_rainnc(met_nx, met_ny) )
  ALLOCATE ( ircold  (met_nx, met_ny) )
  ALLOCATE ( irnold  (met_nx, met_ny) )
  ALLOCATE ( lh      (met_nx, met_ny) )
  ALLOCATE ( pp      (met_nx, met_ny, met_nz) )
  ALLOCATE ( psa     (met_nx, met_ny) )
  ALLOCATE ( qca     (met_nx, met_ny, met_nz) )
  ALLOCATE ( qga     (met_nx, met_ny, met_nz) )
  ALLOCATE ( qia     (met_nx, met_ny, met_nz) )
  ALLOCATE ( qra     (met_nx, met_ny, met_nz) )
  ALLOCATE ( qsa     (met_nx, met_ny, met_nz) )
  ALLOCATE ( qva     (met_nx, met_ny, met_nz) )
  ALLOCATE ( raincon (met_nx, met_ny) )
  ALLOCATE ( rainnon (met_nx, met_ny) )
  ALLOCATE ( rcold   (met_nx, met_ny) )   ! save this variable on each call
  ALLOCATE ( rgrnd   (met_nx, met_ny) )
  ALLOCATE ( rnold   (met_nx, met_ny) )   ! save this variable on each call
  ALLOCATE ( seaice  (met_nx, met_ny) )
  ALLOCATE ( snowcovr(met_nx, met_ny) )
  ALLOCATE ( snowh   (met_nx, met_ny) )
  ALLOCATE ( ta      (met_nx, met_ny, met_nz) )
  ALLOCATE ( ua      (met_nx, met_ny, met_nz) )
  ALLOCATE ( ust     (met_nx, met_ny) )
  ALLOCATE ( va      (met_nx, met_ny, met_nz) )
  ALLOCATE ( wa      (met_nx, met_ny, met_nz+1) )
  ALLOCATE ( zpbl    (met_nx, met_ny) )

  IF ( ift2m ) THEN  ! 2-m temperature available
    ALLOCATE ( t2    (met_nx, met_ny) )
  ENDIF

  IF ( ifq2m ) THEN  ! 2-m mixing ratio available
    ALLOCATE ( q2    (met_nx, met_ny) )
  ENDIF

  IF ( ifw10m ) THEN  ! 10-m wind components available
    ALLOCATE ( u10   (met_nx, met_ny) )
    ALLOCATE ( v10   (met_nx, met_ny) )
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    ALLOCATE ( mu    (met_nx, met_ny) )
    ALLOCATE ( mub   (met_nx, met_ny) )
    ALLOCATE ( pb    (met_nx, met_ny, met_nz)   )
    ALLOCATE ( ph    (met_nx, met_ny, met_nz+1) )
    ALLOCATE ( phb   (met_nx, met_ny, met_nz+1) )
  ENDIF

  IF ( iflai ) THEN  ! leaf area index available
    ALLOCATE ( lai    (met_nx, met_ny) )
  ENDIF

  IF ( ifmol ) THEN  ! Monin-Obukhov length available
    ALLOCATE ( mol    (met_nx, met_ny) )
  ENDIF

  IF ( ifresist ) THEN  ! aerodynamic and stomatal resistances available
    ALLOCATE ( ra     (met_nx, met_ny) )
    ALLOCATE ( rstom  (met_nx, met_ny) )
  ENDIF

  IF ( ifveg ) THEN  ! vegetation fraction available
    ALLOCATE ( veg    (met_nx, met_ny) )
  ENDIF

  IF ( ifwr ) THEN  ! canopy wetness available
    ALLOCATE ( wr     (met_nx, met_ny) )
  ENDIF

  IF ( ifsoil ) THEN  ! soil moisture, temperature, and type available
    ALLOCATE ( isltyp (met_nx, met_ny) )
    ALLOCATE ( soilt1 (met_nx, met_ny) )
    ALLOCATE ( soilt2 (met_nx, met_ny) )
    ALLOCATE ( w2     (met_nx, met_ny) )
    ALLOCATE ( wg     (met_nx, met_ny) )
    ALLOCATE ( soim3d (met_nx, met_ny, met_ns) )
    ALLOCATE ( soit3d (met_nx, met_ny, met_ns) )
  ENDIF

  IF ( iftke ) THEN  ! turbulent kinetic energy available
    IF ( iftkef ) THEN  ! TKE on full-levels
      ALLOCATE ( tke   (met_nx, met_ny, met_nz+1) )
    ELSE  ! TKE on half-levels
      ALLOCATE ( tke   (met_nx, met_ny, met_nz) )
    ENDIF
  ENDIF

  IF ( lpv > 0 .OR. ifmolpx ) THEN  ! need potential temperature
    ALLOCATE ( theta (met_nx, met_ny, met_nz) )
  ENDIF

  IF ( ifmolpx ) THEN  ! recalculate Monin-Obukhov length for WRF-ACM2
    ALLOCATE ( qfx   (met_nx, met_ny) )
  ENDIF

  IF ( met_urban_phys >= 1 ) THEN  ! urban canopy model in WRF
    ALLOCATE ( frc_urb   (met_nx, met_ny) )
  ENDIF

  IF ( ifcld3d ) THEN
    ALLOCATE ( cldfra (met_nx, met_ny, met_nz) )
  ENDIF

  IF ( ifmosaic ) THEN
    ALLOCATE ( lai_mos (met_nx, met_ny, nummosaic) )
    ALLOCATE ( ra_mos  (met_nx, met_ny, nummosaic) )
    ALLOCATE ( rs_mos  (met_nx, met_ny, nummosaic) )
    ALLOCATE ( tsk_mos (met_nx, met_ny, nummosaic) )
    ALLOCATE ( znt_mos (met_nx, met_ny, nummosaic) )
    ALLOCATE ( wspdsfc (met_nx, met_ny) )
    ALLOCATE ( xlaidyn (met_nx, met_ny) )
  ENDIF

  IF ( ifpxwrf41 ) THEN
    ALLOCATE ( lai_px    (met_nx, met_ny) )
    ALLOCATE ( wsat_px   (met_nx, met_ny) )
    ALLOCATE ( wfc_px    (met_nx, met_ny) )
    ALLOCATE ( wwlt_px   (met_nx, met_ny) )
    ALLOCATE ( csand_px  (met_nx, met_ny) )
    ALLOCATE ( fmsand_px (met_nx, met_ny) )
    ALLOCATE ( clay_px   (met_nx, met_ny) )
  ENDIF

  IF ( ifkfradextras ) THEN
    ALLOCATE ( qc_cu     (met_nx, met_ny, met_nz) )
    ALLOCATE ( qi_cu     (met_nx, met_ny, met_nz) )
    ALLOCATE ( cldfra_dp (met_nx, met_ny, met_nz) )
    ALLOCATE ( cldfra_sh (met_nx, met_ny, met_nz) )
  ENDIF

END SUBROUTINE alloc_met
