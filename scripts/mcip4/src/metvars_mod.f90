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
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          ALLOCATABLE   :: albedo     ( : , : )
  REAL,          ALLOCATABLE   :: coriolis   ( : , : )
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
  INTEGER,       ALLOCATABLE   :: landuse    ( : , : )
  REAL,          ALLOCATABLE   :: latcrs     ( : , : )
  REAL,          ALLOCATABLE   :: latdot     ( : , : )
  REAL,          ALLOCATABLE   :: latu       ( : , : )
  REAL,          ALLOCATABLE   :: latv       ( : , : )
  REAL,          ALLOCATABLE   :: loncrs     ( : , : )
  REAL,          ALLOCATABLE   :: londot     ( : , : )
  REAL,          ALLOCATABLE   :: lonu       ( : , : )
  REAL,          ALLOCATABLE   :: lonv       ( : , : )
  REAL,          ALLOCATABLE   :: lufrac     ( : , : , : )
  REAL,          ALLOCATABLE   :: mapcrs     ( : , : )
  REAL,          ALLOCATABLE   :: mapdot     ( : , : )
  REAL,          ALLOCATABLE   :: mapu       ( : , : )
  REAL,          ALLOCATABLE   :: mapv       ( : , : )
  REAL,          ALLOCATABLE   :: mol        ( : , : )
  REAL,          ALLOCATABLE   :: mu         ( : , : )
  REAL,          ALLOCATABLE   :: mub        ( : , : )
  REAL,          ALLOCATABLE   :: pb         ( : , : , : )
  REAL,          ALLOCATABLE   :: ph         ( : , : , : )
  REAL,          ALLOCATABLE   :: phb        ( : , : , : )
  REAL,          ALLOCATABLE   :: pp         ( : , : , : )
  REAL,          ALLOCATABLE   :: psa        ( : , : )
  REAL,          ALLOCATABLE   :: q2         ( : , : )
  REAL,          ALLOCATABLE   :: qca        ( : , : , : )
  REAL,          ALLOCATABLE   :: qfx        ( : , : )
  REAL,          ALLOCATABLE   :: qga        ( : , : , : )
  REAL,          ALLOCATABLE   :: qia        ( : , : , : )
  REAL,          ALLOCATABLE   :: qra        ( : , : , : )
  REAL,          ALLOCATABLE   :: qsa        ( : , : , : )
  REAL,          ALLOCATABLE   :: qva        ( : , : , : )
  REAL,          ALLOCATABLE   :: ra         ( : , : )
  REAL,          ALLOCATABLE   :: raincon    ( : , : )
  REAL,          ALLOCATABLE   :: rainnon    ( : , : )
  REAL,          ALLOCATABLE   :: rcold      ( : , : )
  REAL,          ALLOCATABLE   :: rgrnd      ( : , : )
  REAL,          ALLOCATABLE   :: rnold      ( : , : )
  REAL,          ALLOCATABLE   :: rstom      ( : , : )
  REAL,          ALLOCATABLE   :: seaice     ( : , : )
  REAL,          ALLOCATABLE   :: sigmah     ( : )
  REAL,          ALLOCATABLE   :: sigmaf     ( : )
  REAL,          ALLOCATABLE   :: snowcovr   ( : , : )
  REAL,          ALLOCATABLE   :: soilt1     ( : , : )
  REAL,          ALLOCATABLE   :: soilt2     ( : , : )
  REAL,          ALLOCATABLE   :: t2         ( : , : )
  REAL,          ALLOCATABLE   :: ta         ( : , : , : )
  REAL,          ALLOCATABLE   :: terrain    ( : , : )
  REAL,          ALLOCATABLE   :: theta      ( : , : , : )
  REAL,          ALLOCATABLE   :: tke        ( : , : , : )
  REAL,          ALLOCATABLE   :: u10        ( : , : )
  REAL,          ALLOCATABLE   :: ua         ( : , : , : )
  REAL,          ALLOCATABLE   :: ust        ( : , : )
  REAL,          ALLOCATABLE   :: v10        ( : , : )
  REAL,          ALLOCATABLE   :: va         ( : , : , : )
  REAL,          ALLOCATABLE   :: veg        ( : , : )
  REAL,          ALLOCATABLE   :: vegold     ( : , : )
  REAL,          ALLOCATABLE   :: w2         ( : , : )
  REAL,          ALLOCATABLE   :: wa         ( : , : , : )
  REAL,          ALLOCATABLE   :: wg         ( : , : )
  REAL,          ALLOCATABLE   :: wr         ( : , : )
  REAL,          ALLOCATABLE   :: znt        ( : , : )
  REAL,          ALLOCATABLE   :: zpbl       ( : , : )

END MODULE metvars
