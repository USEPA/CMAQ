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

MODULE metvars

!-------------------------------------------------------------------------------
! Name:     Meteorology Variables
! Purpose:  Contains input meteorology arrays.
! Revised:  19 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Removed unused variables REGIME and MAVAIL.  Added PH,
!                        PHB, PB, MU, and MUB for WRF.  Added U10 and V10.
!                        (T. Otte and S.-B. Kim)
!           11 Aug 2005  Removed unused variable FSOIL.  (T. Otte)
!           26 Jul 2007  Added IMPLICIT NONE.  Removed internal variables for
!                        emissivity and net radiation.  (T. Otte)
!           05 May 2008  Added 2-m mixing ratio (Q2) and turbulent kinetic
!                        energy (TKE) arrays.  Added urban fraction (FRC_URB)
!                        and urban roughness length (Z0C_URB2D) for
!                        MET_UCMCALL=1.  (T. Otte)
!           29 Sep 2009  Added THETA and CORIOLIS for when potential vorticity
!                        is needed.  Added LATU, LONU, MAPU, LATV, LONV, and
!                        MAPV.  Removed Z0C_URB2D.  (T. Otte)
!           15 Dec 2010  Added sea ice.  Added tipping buckets for convective
!                        and non-convective precipitation.  (T. Otte)
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
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          ALLOCATABLE   :: albedo     ( : , : )
  REAL,          ALLOCATABLE   :: c1f        ( : )
  REAL,          ALLOCATABLE   :: c1h        ( : )
  REAL,          ALLOCATABLE   :: c2f        ( : )
  REAL,          ALLOCATABLE   :: c2h        ( : )
  REAL,          ALLOCATABLE   :: clay_px    ( : , : )
  REAL,          ALLOCATABLE   :: cldfra     ( : , : , : )
  REAL,          ALLOCATABLE   :: cldfra_dp  ( : , : , : )
  REAL,          ALLOCATABLE   :: cldfra_sh  ( : , : , : )
  REAL,          ALLOCATABLE   :: coriolis   ( : , : )
  REAL,          ALLOCATABLE   :: csand_px   ( : , : )
  REAL,          ALLOCATABLE   :: dzs        ( : )
  REAL,          ALLOCATABLE   :: fmsand_px  ( : , : )
  REAL,          ALLOCATABLE   :: frc_urb    ( : , : )
  REAL,          ALLOCATABLE   :: glw        ( : , : )
  REAL,          ALLOCATABLE   :: groundt    ( : , : )
  REAL,          ALLOCATABLE   :: hfx        ( : , : )
  INTEGER,       ALLOCATABLE   :: i_rainc    ( : , : )
  INTEGER,       ALLOCATABLE   :: i_rainnc   ( : , : )
  INTEGER,       ALLOCATABLE   :: ircold     ( : , : )
  INTEGER,       ALLOCATABLE   :: irnold     ( : , : )
  INTEGER,       ALLOCATABLE   :: isltyp     ( : , : )
  REAL,          ALLOCATABLE   :: lai        ( : , : )
  REAL,          ALLOCATABLE   :: lai_mos    ( : , : , : )
  REAL,          ALLOCATABLE   :: lai_px     ( : , : )
  REAL,          ALLOCATABLE   :: landmask   ( : , : )
  INTEGER,       ALLOCATABLE   :: landuse    ( : , : )
  REAL,          ALLOCATABLE   :: latcrs     ( : , : )
  REAL,          ALLOCATABLE   :: latdot     ( : , : )
  REAL,          ALLOCATABLE   :: latu       ( : , : )
  REAL,          ALLOCATABLE   :: latv       ( : , : )
  REAL,          ALLOCATABLE   :: lh         ( : , : )
  REAL,          ALLOCATABLE   :: loncrs     ( : , : )
  REAL,          ALLOCATABLE   :: londot     ( : , : )
  REAL,          ALLOCATABLE   :: lonu       ( : , : )
  REAL,          ALLOCATABLE   :: lonv       ( : , : )
  REAL,          ALLOCATABLE   :: lufrac     ( : , : , : )
  REAL,          ALLOCATABLE   :: lufrac2    ( : , : , : )
  REAL,          ALLOCATABLE   :: mapcrs     ( : , : )
  REAL,          ALLOCATABLE   :: mapdot     ( : , : )
  REAL,          ALLOCATABLE   :: mapu       ( : , : )
  REAL,          ALLOCATABLE   :: mapv       ( : , : )
  REAL,          ALLOCATABLE   :: mol        ( : , : )
  INTEGER,       ALLOCATABLE   :: moscatidx  ( : , : , : )
  REAL,          ALLOCATABLE   :: mu         ( : , : )
  REAL,          ALLOCATABLE   :: mub        ( : , : )
  REAL,          ALLOCATABLE   :: pb         ( : , : , : )
  REAL,          ALLOCATABLE   :: ph         ( : , : , : )
  REAL,          ALLOCATABLE   :: phb        ( : , : , : )
  REAL,          ALLOCATABLE   :: pp         ( : , : , : )
  REAL,          ALLOCATABLE   :: psa        ( : , : )
  REAL,          ALLOCATABLE   :: q2         ( : , : )
  REAL,          ALLOCATABLE   :: qca        ( : , : , : )
  REAL,          ALLOCATABLE   :: qc_cu      ( : , : , : )
  REAL,          ALLOCATABLE   :: qfx        ( : , : )
  REAL,          ALLOCATABLE   :: qga        ( : , : , : )
  REAL,          ALLOCATABLE   :: qia        ( : , : , : )
  REAL,          ALLOCATABLE   :: qi_cu      ( : , : , : )
  REAL,          ALLOCATABLE   :: qra        ( : , : , : )
  REAL,          ALLOCATABLE   :: qsa        ( : , : , : )
  REAL,          ALLOCATABLE   :: qva        ( : , : , : )
  REAL,          ALLOCATABLE   :: ra         ( : , : )
  REAL,          ALLOCATABLE   :: ra_mos     ( : , : , : )
  REAL,          ALLOCATABLE   :: raincon    ( : , : )
  REAL,          ALLOCATABLE   :: rainnon    ( : , : )
  REAL,          ALLOCATABLE   :: rcold      ( : , : )
  REAL,          ALLOCATABLE   :: rgrnd      ( : , : )
  REAL,          ALLOCATABLE   :: rnold      ( : , : )
  REAL,          ALLOCATABLE   :: rs_mos     ( : , : , : )
  REAL,          ALLOCATABLE   :: rstom      ( : , : )
  REAL,          ALLOCATABLE   :: seaice     ( : , : )
  REAL,          ALLOCATABLE   :: sigmaf     ( : )
  REAL,          ALLOCATABLE   :: sigmah     ( : )
  REAL,          ALLOCATABLE   :: snowcovr   ( : , : )
  REAL,          ALLOCATABLE   :: snowh      ( : , : )
  REAL,          ALLOCATABLE   :: soilt1     ( : , : )
  REAL,          ALLOCATABLE   :: soilt2     ( : , : )
  REAL,          ALLOCATABLE   :: soim3d     ( : , : , : )
  REAL,          ALLOCATABLE   :: soit3d     ( : , : , : )
  REAL,          ALLOCATABLE   :: t2         ( : , : )
  REAL,          ALLOCATABLE   :: ta         ( : , : , : )
  REAL,          ALLOCATABLE   :: terrain    ( : , : )
  REAL,          ALLOCATABLE   :: theta      ( : , : , : )
  REAL,          ALLOCATABLE   :: tke        ( : , : , : )
  REAL,          ALLOCATABLE   :: tsk_mos    ( : , : , : )
  REAL,          ALLOCATABLE   :: u10        ( : , : )
  REAL,          ALLOCATABLE   :: ua         ( : , : , : )
  REAL,          ALLOCATABLE   :: ust        ( : , : )
  REAL,          ALLOCATABLE   :: v10        ( : , : )
  REAL,          ALLOCATABLE   :: va         ( : , : , : )
  REAL,          ALLOCATABLE   :: veg        ( : , : )
  REAL,          ALLOCATABLE   :: w2         ( : , : )
  REAL,          ALLOCATABLE   :: wa         ( : , : , : )
  REAL,          ALLOCATABLE   :: wfc_px     ( : , : )
  REAL,          ALLOCATABLE   :: wg         ( : , : )
  REAL,          ALLOCATABLE   :: wr         ( : , : )
  REAL,          ALLOCATABLE   :: wsat_px    ( : , : )
  REAL,          ALLOCATABLE   :: wspdsfc    ( : , : )
  REAL,          ALLOCATABLE   :: wwlt_px    ( : , : )
  REAL,          ALLOCATABLE   :: xlaidyn    ( : , : )
  REAL,          ALLOCATABLE   :: znt        ( : , : )
  REAL,          ALLOCATABLE   :: znt_mos    ( : , : , : )
  REAL,          ALLOCATABLE   :: zpbl       ( : , : )

END MODULE metvars
