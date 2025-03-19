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

SUBROUTINE init_ctm

!-------------------------------------------------------------------------------
! Name:     Initialize CTM data structures.
! Purpose:  Initializes CTM data structures.
! Revised:  13 Dec 2018  Initial version.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Changed variable
!                        LUVCOUT to LUVBOUT to reflect that the default 3D wind
!                        components are on the Arakawa-C staggered grid, and
!                        the optional additional 3D winds are on the Arakawa-B
!                        staggered grid.  Added optional variables from KF
!                        convective scheme with radiative feedback.  (T. Spero)
!           08 Oct 2020  Updated description and units for potential vorticity
!                        to adhere to constraints on character field in
!                        I/O API. Note that units do not conform to MKS, but
!                        full explanation is given in the description field.
!                        (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE ctmvars

  IMPLICIT NONE
 
!-------------------------------------------------------------------------------
! Initialize output dimensions.
!-------------------------------------------------------------------------------

  nx = ncols
  ny = nrows
  nz = nlays

  nxp1 = nx + 1
  nyp1 = ny + 1
  nzp1 = nz + 1

  nperim = nbndy

  nlucat = nummetlu
  nmos   = nummosaic
  nsoi   = metsoi

!-------------------------------------------------------------------------------
! Time-independent 2d fields at cell centers.
!-------------------------------------------------------------------------------

  g_lat%fld = fillreal
  g_lat%bdy = fillreal
  g_lat%fldname = 'LAT'
  g_lat%long_name = 'latitude at cell centers'
  g_lat%units = 'degrees_north'
  g_lat%dimnames(1) = 'nx'
  g_lat%dimnames(2) = 'ny'
  g_lat%istart(1) = 1
  g_lat%istart(2) = 1
  g_lat%iend(1) = nx
  g_lat%iend(2) = ny
  g_lat%dimnames_b(1) = 'nperim'
  g_lat%istart_b(1) = 1
  g_lat%iend_b(1) = nperim

  g_lon%fld = fillreal
  g_lon%bdy = fillreal
  g_lon%fldname = 'LON'
  g_lon%long_name = 'longitude at cell centers'
  g_lon%units = 'degrees_east'
  g_lon%dimnames(1) = 'nx'
  g_lon%dimnames(2) = 'ny'
  g_lon%istart(1) = 1
  g_lon%istart(2) = 1
  g_lon%iend(1) = nx
  g_lon%iend(2) = ny
  g_lon%dimnames_b(1) = 'nperim'
  g_lon%istart_b(1) = 1
  g_lon%iend_b(1) = nperim

  g_msfx2%fld = fillreal
  g_msfx2%bdy = fillreal
  g_msfx2%fldname = 'MSFX2'
  g_msfx2%long_name = 'map-scale factor squared'
  g_msfx2%units = 'm2 m-2'
  g_msfx2%dimnames(1) = 'nx'
  g_msfx2%dimnames(2) = 'ny'
  g_msfx2%istart(1) = 1
  g_msfx2%istart(2) = 1
  g_msfx2%iend(1) = nx
  g_msfx2%iend(2) = ny
  g_msfx2%dimnames_b(1) = 'nperim'
  g_msfx2%istart_b(1) = 1
  g_msfx2%iend_b(1) = nperim

  g_ht%fld = fillreal
  g_ht%bdy = fillreal
  g_ht%fldname = 'HT'
  g_ht%long_name = 'terrain elevation'
  g_ht%units = 'm'
  g_ht%dimnames(1) = 'nx'
  g_ht%dimnames(2) = 'ny'
  g_ht%istart(1) = 1
  g_ht%istart(2) = 1
  g_ht%iend(1) = nx
  g_ht%iend(2) = ny
  g_ht%dimnames_b(1) = 'nperim'
  g_ht%istart_b(1) = 1
  g_ht%iend_b(1) = nperim

  g_dluse%fld = fillreal
  g_dluse%bdy = fillreal
  g_dluse%fldname = 'DLUSE'
  g_dluse%long_name = 'dominant land use category'
  g_dluse%units = '1'
  g_dluse%dimnames(1) = 'nx'
  g_dluse%dimnames(2) = 'ny'
  g_dluse%istart(1) = 1
  g_dluse%istart(2) = 1
  g_dluse%iend(1) = nx
  g_dluse%iend(2) = ny
  g_dluse%dimnames_b(1) = 'nperim'
  g_dluse%istart_b(1) = 1
  g_dluse%iend_b(1) = nperim

  g_lwmask%fld = fillreal
  g_lwmask%bdy = fillreal
  g_lwmask%fldname = 'LWMASK'
  g_lwmask%long_name = 'land-water mask (1=land, 0=water)'
  g_lwmask%units = '1'
  g_lwmask%dimnames(1) = 'nx'
  g_lwmask%dimnames(2) = 'ny'
  g_lwmask%istart(1) = 1
  g_lwmask%istart(2) = 1
  g_lwmask%iend(1) = nx
  g_lwmask%iend(2) = ny
  g_lwmask%dimnames_b(1) = 'nperim'
  g_lwmask%istart_b(1) = 1
  g_lwmask%iend_b(1) = nperim

  IF ( iflufrc ) THEN
    g_purb%fld = fillreal
    g_purb%bdy = fillreal
    g_purb%fldname = 'PURB'
    g_purb%long_name = 'urban percent of cell based on land'
    g_purb%units = 'percent'
    g_purb%dimnames(1) = 'nx'
    g_purb%dimnames(2) = 'ny'
    g_purb%istart(1) = 1
    g_purb%istart(2) = 1
    g_purb%iend(1) = nx
    g_purb%iend(2) = ny
    g_purb%dimnames_b(1) = 'nperim'
    g_purb%istart_b(1) = 1
    g_purb%iend_b(1) = nperim
  ENDIF

!-------------------------------------------------------------------------------
! Time-independent 2d fields at cell corners and cell faces.
!-------------------------------------------------------------------------------

  g_latd%fld = fillreal
  g_latd%fldname = 'LATD'
  g_latd%long_name = 'latitude at cell corners'
  g_latd%units = 'degrees_north'
  g_latd%dimnames(1) = 'nxp1'
  g_latd%dimnames(2) = 'nyp1'
  g_latd%istart(1) = 1
  g_latd%istart(2) = 1
  g_latd%iend(1) = nxp1
  g_latd%iend(2) = nyp1

  g_lond%fld = fillreal
  g_lond%fldname = 'LOND'
  g_lond%long_name = 'longitude at cell corners'
  g_lond%units = 'degrees_east'
  g_lond%dimnames(1) = 'nxp1'
  g_lond%dimnames(2) = 'nyp1'
  g_lond%istart(1) = 1
  g_lond%istart(2) = 1
  g_lond%iend(1) = nxp1
  g_lond%iend(2) = nyp1

  g_msfd2%fld = fillreal
  g_msfd2%fldname = 'MSFD2'
  g_msfd2%long_name = 'squared map-scale factor at cell corners'
  g_msfd2%units = 'm2 m-2'
  g_msfd2%dimnames(1) = 'nxp1'
  g_msfd2%dimnames(2) = 'nyp1'
  g_msfd2%istart(1) = 1
  g_msfd2%istart(2) = 1
  g_msfd2%iend(1) = nxp1
  g_msfd2%iend(2) = nyp1

  g_latu%fld = fillreal
  g_latu%fldname = 'LATU'
  g_latu%long_name = 'latitude at cell U faces'
  g_latu%units = 'degrees_north'
  g_latu%dimnames(1) = 'nxp1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_latu%dimnames(2) = 'nyp1'
  ELSE  ! preserve Arakawa-C staggering
    g_latu%dimnames(2) = 'ny'
  ENDIF
  g_latu%istart(1) = 1
  g_latu%istart(2) = 1
  g_latu%iend(1) = nxp1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_latu%iend(2) = nyp1
  ELSE  ! preserve Arakawa-C staggering
    g_latu%iend(2) = ny
  ENDIF

  g_lonu%fld = fillreal
  g_lonu%fldname = 'LONU'
  g_lonu%long_name = 'longitude at cell U faces'
  g_lonu%units = 'degrees_east'
  g_lonu%dimnames(1) = 'nxp1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_lonu%dimnames(2) = 'nyp1'
  ELSE  ! preserve Arakawa-C staggering
    g_lonu%dimnames(2) = 'ny'
  ENDIF
  g_lonu%istart(1) = 1
  g_lonu%istart(2) = 1
  g_lonu%iend(1) = nxp1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_lonu%iend(2) = nyp1
  ELSE  ! preserve Arakawa-C staggering
    g_lonu%iend(2) = ny
  ENDIF

  g_msfu2%fld = fillreal
  g_msfu2%fldname = 'MSFU2'
  g_msfu2%long_name = 'squared map-scale factor at cell U faces'
  g_msfu2%units = 'm2 m-2'
  g_msfu2%dimnames(1) = 'nxp1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_msfu2%dimnames(2) = 'nyp1'
  ELSE  ! preserve Arakawa-C staggering
    g_msfu2%dimnames(2) = 'ny'
  ENDIF
  g_msfu2%istart(1) = 1
  g_msfu2%istart(2) = 1
  g_msfu2%iend(1) = nxp1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_msfu2%iend(2) = nyp1
  ELSE  ! preserve Arakawa-C staggering
    g_msfu2%iend(2) = ny
  ENDIF

  g_latv%fld = fillreal
  g_latv%fldname = 'LATV'
  g_latv%long_name = 'latitude at cell V faces'
  g_latv%units = 'degrees_north'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_latv%dimnames(1) = 'nxp1'
  ELSE  ! preserve Arakawa-C staggering
    g_latv%dimnames(1) = 'nx'
  ENDIF
  g_latv%dimnames(2) = 'nyp1'
  g_latv%istart(1) = 1
  g_latv%istart(2) = 1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_latv%iend(1) = nxp1
  ELSE  ! preserve Arakawa-C staggering
    g_latv%iend(1) = nx
  ENDIF
  g_latv%iend(2) = nyp1

  g_lonv%fld = fillreal
  g_lonv%fldname = 'LONV'
  g_lonv%long_name = 'longitude at cell V faces'
  g_lonv%units = 'degrees_east'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_lonv%dimnames(1) = 'nxp1'
  ELSE  ! preserve Arakawa-C staggering
    g_lonv%dimnames(1) = 'nx'
  ENDIF
  g_lonv%dimnames(2) = 'nyp1'
  g_lonv%istart(1) = 1
  g_lonv%istart(2) = 1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_lonv%iend(1) = nxp1
  ELSE  ! preserve Arakawa-C staggering
    g_lonv%iend(1) = nx
  ENDIF
  g_lonv%iend(2) = nyp1

  g_msfv2%fld = fillreal
  g_msfv2%fldname = 'MSFV2'
  g_msfv2%long_name = 'squared map-scale factor at cell V faces'
  g_msfv2%units = 'm2 m-2'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_msfv2%dimnames(1) = 'nxp1'
  ELSE  ! preserve Arakawa-C staggering
    g_msfv2%dimnames(1) = 'nx'
  ENDIF
  g_msfv2%dimnames(2) = 'nyp1'
  g_msfv2%istart(1) = 1
  g_msfv2%istart(2) = 1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    g_msfv2%iend(1) = nxp1
  ELSE  ! preserve Arakawa-C staggering
    g_msfv2%iend(1) = nx
  ENDIF
  g_msfv2%iend(2) = nyp1

!-------------------------------------------------------------------------------
! Time-independent 3d fields (fractional land use) at cell centers.
!-------------------------------------------------------------------------------

  IF ( iflufrc ) THEN
    g_lufrac%fld = fillreal
    g_lufrac%fldname = 'LUFRAC'
    g_lufrac%long_name = 'fractional land use'
    g_lufrac%units = 'percent'
    g_lufrac%dimnames(1) = 'nx'
    g_lufrac%dimnames(2) = 'ny'
    g_lufrac%dimnames(3) = 'nlucat'
    g_lufrac%istart(1) = 1
    g_lufrac%istart(2) = 1
    g_lufrac%istart(3) = 1
    g_lufrac%iend(1) = nx
    g_lufrac%iend(2) = ny
    g_lufrac%iend(3) = nummetlu
  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 2d fields at cell centers.
!-------------------------------------------------------------------------------

  c_prsfc%fld = fillreal
  c_prsfc%fldname = 'PRSFC'
  c_prsfc%long_name = 'surface pressure'
  c_prsfc%units = 'Pa'
  c_prsfc%dimnames(1) = 'nx'
  c_prsfc%dimnames(2) = 'ny'
  c_prsfc%istart(1) = 1
  c_prsfc%istart(2) = 1
  c_prsfc%iend(1) = nx
  c_prsfc%iend(2) = ny

  c_ustar%fld = fillreal
  c_ustar%fldname = 'USTAR'
  c_ustar%long_name = 'cell averaged friction velocity'
  c_ustar%units = 'm s-1'
  c_ustar%dimnames(1) = 'nx'
  c_ustar%dimnames(2) = 'ny'
  c_ustar%istart(1) = 1
  c_ustar%istart(2) = 1
  c_ustar%iend(1) = nx
  c_ustar%iend(2) = ny

  c_wstar%fld = fillreal
  c_wstar%fldname = 'WSTAR'
  c_wstar%long_name = 'convective velocity scale'
  c_wstar%units = 'm s-1'
  c_wstar%dimnames(1) = 'nx'
  c_wstar%dimnames(2) = 'ny'
  c_wstar%istart(1) = 1
  c_wstar%istart(2) = 1
  c_wstar%iend(1) = nx
  c_wstar%iend(2) = ny

  c_pbl%fld = fillreal
  c_pbl%fldname = 'PBL'
  c_pbl%long_name = 'PBL height'
  c_pbl%units = 'm'
  c_pbl%dimnames(1) = 'nx'
  c_pbl%dimnames(2) = 'ny'
  c_pbl%istart(1) = 1
  c_pbl%istart(2) = 1
  c_pbl%iend(1) = nx
  c_pbl%iend(2) = ny

  c_zruf%fld = fillreal
  c_zruf%fldname = 'ZRUF'
  c_zruf%long_name = 'surface roughness length'
  c_zruf%units = 'm'
  c_zruf%dimnames(1) = 'nx'
  c_zruf%dimnames(2) = 'ny'
  c_zruf%istart(1) = 1
  c_zruf%istart(2) = 1
  c_zruf%iend(1) = nx
  c_zruf%iend(2) = ny

  c_moli%fld = fillreal
  c_moli%fldname = 'MOLI'
  c_moli%long_name = 'inverse of Monin-Obukhov length'
  c_moli%units = 'm-1'
  c_moli%dimnames(1) = 'nx'
  c_moli%dimnames(2) = 'ny'
  c_moli%istart(1) = 1
  c_moli%istart(2) = 1
  c_moli%iend(1) = nx
  c_moli%iend(2) = ny

  c_hfx%fld = fillreal
  c_hfx%fldname = 'HFX'
  c_hfx%long_name = 'sensible heat flux'
  c_hfx%units = 'W m-2'
  c_hfx%dimnames(1) = 'nx'
  c_hfx%dimnames(2) = 'ny'
  c_hfx%istart(1) = 1
  c_hfx%istart(2) = 1
  c_hfx%iend(1) = nx
  c_hfx%iend(2) = ny

  c_lh%fld = fillreal
  c_lh%fldname = 'LH'
  c_lh%long_name = 'latent heat flux'
  c_lh%units = 'W m-2'
  c_lh%dimnames(1) = 'nx'
  c_lh%dimnames(2) = 'ny'
  c_lh%istart(1) = 1
  c_lh%istart(2) = 1
  c_lh%iend(1) = nx
  c_lh%iend(2) = ny

  c_radyni%fld = fillreal
  c_radyni%fldname = 'RADYNI'
  c_radyni%long_name = 'inverse of aerodynamic resistance'
  c_radyni%units = 'm s-1'
  c_radyni%dimnames(1) = 'nx'
  c_radyni%dimnames(2) = 'ny'
  c_radyni%istart(1) = 1
  c_radyni%istart(2) = 1
  c_radyni%iend(1) = nx
  c_radyni%iend(2) = ny

  c_rstomi%fld = fillreal
  c_rstomi%fldname = 'RSTOMI'
  c_rstomi%long_name = 'inverse of stomatic resistance'
  c_rstomi%units = 'm s-1'
  c_rstomi%dimnames(1) = 'nx'
  c_rstomi%dimnames(2) = 'ny'
  c_rstomi%istart(1) = 1
  c_rstomi%istart(2) = 1
  c_rstomi%iend(1) = nx
  c_rstomi%iend(2) = ny

  c_tempg%fld = fillreal
  c_tempg%fldname = 'TEMPG'
  c_tempg%long_name = 'skin temperature at ground'
  c_tempg%units = 'K'
  c_tempg%dimnames(1) = 'nx'
  c_tempg%dimnames(2) = 'ny'
  c_tempg%istart(1) = 1
  c_tempg%istart(2) = 1
  c_tempg%iend(1) = nx
  c_tempg%iend(2) = ny

  c_temp2%fld = fillreal
  c_temp2%fldname = 'TEMP2'
  c_temp2%long_name = 'temperature at 2 m'
  c_temp2%units = 'K'
  c_temp2%dimnames(1) = 'nx'
  c_temp2%dimnames(2) = 'ny'
  c_temp2%istart(1) = 1
  c_temp2%istart(2) = 1
  c_temp2%iend(1) = nx
  c_temp2%iend(2) = ny

  c_q2%fld = fillreal
  c_q2%fldname = 'Q2'
  c_q2%long_name = 'mixing ratio at 2 m'
  c_q2%units = 'kg kg-1'
  c_q2%dimnames(1) = 'nx'
  c_q2%dimnames(2) = 'ny'
  c_q2%istart(1) = 1
  c_q2%istart(2) = 1
  c_q2%iend(1) = nx
  c_q2%iend(2) = ny

  c_wspd10%fld = fillreal
  c_wspd10%fldname = 'WSPD10'
  c_wspd10%long_name = 'wind speed at 10 m'
  c_wspd10%units = 'm s-1'
  c_wspd10%dimnames(1) = 'nx'
  c_wspd10%dimnames(2) = 'ny'
  c_wspd10%istart(1) = 1
  c_wspd10%istart(2) = 1
  c_wspd10%iend(1) = nx
  c_wspd10%iend(2) = ny

  c_wdir10%fld = fillreal
  c_wdir10%fldname = 'WDIR10'
  c_wdir10%long_name = 'wind direction at 10 m'
  c_wdir10%units = 'degree'
  c_wdir10%dimnames(1) = 'nx'
  c_wdir10%dimnames(2) = 'ny'
  c_wdir10%istart(1) = 1
  c_wdir10%istart(2) = 1
  c_wdir10%iend(1) = nx
  c_wdir10%iend(2) = ny

  c_glw%fld = fillreal
  c_glw%fldname = 'GLW'
  c_glw%long_name = 'longwave radiation at ground'
  c_glw%units = 'W m-2'
  c_glw%dimnames(1) = 'nx'
  c_glw%dimnames(2) = 'ny'
  c_glw%istart(1) = 1
  c_glw%istart(2) = 1
  c_glw%iend(1) = nx
  c_glw%iend(2) = ny

  c_gsw%fld = fillreal
  c_gsw%fldname = 'GSW'
  c_gsw%long_name = 'solar radiation absorbed at ground'
  c_gsw%units = 'W m-2'
  c_gsw%dimnames(1) = 'nx'
  c_gsw%dimnames(2) = 'ny'
  c_gsw%istart(1) = 1
  c_gsw%istart(2) = 1
  c_gsw%iend(1) = nx
  c_gsw%iend(2) = ny

  c_rgrnd%fld = fillreal
  c_rgrnd%fldname = 'RGRND'
  c_rgrnd%long_name = 'solar radiation reaching ground'
  c_rgrnd%units = 'W m-2'
  c_rgrnd%dimnames(1) = 'nx'
  c_rgrnd%dimnames(2) = 'ny'
  c_rgrnd%istart(1) = 1
  c_rgrnd%istart(2) = 1
  c_rgrnd%iend(1) = nx
  c_rgrnd%iend(2) = ny

  c_rn%fld = fillreal
  c_rn%fldname = 'RN'
  c_rn%long_name = 'nonconvective precipitation in interval'
  c_rn%units = 'cm'
  c_rn%dimnames(1) = 'nx'
  c_rn%dimnames(2) = 'ny'
  c_rn%istart(1) = 1
  c_rn%istart(2) = 1
  c_rn%iend(1) = nx
  c_rn%iend(2) = ny

  c_rc%fld = fillreal
  c_rc%fldname = 'RC'
  c_rc%long_name = 'convective precipitation in interval'
  c_rc%units = 'cm'
  c_rc%dimnames(1) = 'nx'
  c_rc%dimnames(2) = 'ny'
  c_rc%istart(1) = 1
  c_rc%istart(2) = 1
  c_rc%iend(1) = nx
  c_rc%iend(2) = ny

  c_cfrac%fld = fillreal
  c_cfrac%fldname = 'CFRAC'
  c_cfrac%long_name = 'total cloud fraction'
  c_cfrac%units = '1'
  c_cfrac%dimnames(1) = 'nx'
  c_cfrac%dimnames(2) = 'ny'
  c_cfrac%istart(1) = 1
  c_cfrac%istart(2) = 1
  c_cfrac%iend(1) = nx
  c_cfrac%iend(2) = ny

  c_cldt%fld = fillreal
  c_cldt%fldname = 'CLDT'
  c_cldt%long_name = 'cloud top layer height'
  c_cldt%units = 'm'
  c_cldt%dimnames(1) = 'nx'
  c_cldt%dimnames(2) = 'ny'
  c_cldt%istart(1) = 1
  c_cldt%istart(2) = 1
  c_cldt%iend(1) = nx
  c_cldt%iend(2) = ny

  c_cldb%fld = fillreal
  c_cldb%fldname = 'CLDB'
  c_cldb%long_name = 'cloud bottom layer height'
  c_cldb%units = 'm'
  c_cldb%dimnames(1) = 'nx'
  c_cldb%dimnames(2) = 'ny'
  c_cldb%istart(1) = 1
  c_cldb%istart(2) = 1
  c_cldb%iend(1) = nx
  c_cldb%iend(2) = ny

  c_wbar%fld = fillreal
  c_wbar%fldname = 'WBAR'
  c_wbar%long_name = 'average liquid water content of cloud'
  c_wbar%units = 'g m-3'
  c_wbar%dimnames(1) = 'nx'
  c_wbar%dimnames(2) = 'ny'
  c_wbar%istart(1) = 1
  c_wbar%istart(2) = 1
  c_wbar%iend(1) = nx
  c_wbar%iend(2) = ny

  c_snocov%fld = fillreal
  c_snocov%fldname = 'SNOCOV'
  c_snocov%long_name = 'snow cover'
  c_snocov%units = '1'
  c_snocov%dimnames(1) = 'nx'
  c_snocov%dimnames(2) = 'ny'
  c_snocov%istart(1) = 1
  c_snocov%istart(2) = 1
  c_snocov%iend(1) = nx
  c_snocov%iend(2) = ny

  c_veg%fld = fillreal
  c_veg%fldname = 'VEG'
  c_veg%long_name = 'vegetation coverage'
  c_veg%units = '1'
  c_veg%dimnames(1) = 'nx'
  c_veg%dimnames(2) = 'ny'
  c_veg%istart(1) = 1
  c_veg%istart(2) = 1
  c_veg%iend(1) = nx
  c_veg%iend(2) = ny

  c_lai%fld = fillreal
  c_lai%fldname = 'LAI'
  IF ( ifpxwrf41 ) THEN
    c_lai%long_name = 'leaf-area index from PX LSM'
  ELSE
    c_lai%long_name = 'leaf-area index'
  ENDIF
  c_lai%units = 'm2 m-2'
  c_lai%dimnames(1) = 'nx'
  c_lai%dimnames(2) = 'ny'
  c_lai%istart(1) = 1
  c_lai%istart(2) = 1
  c_lai%iend(1) = nx
  c_lai%iend(2) = ny

  c_seaice%fld = fillreal
  c_seaice%fldname = 'SEAICE'
  c_seaice%long_name = 'sea ice'
  c_seaice%units = '1'
  c_seaice%dimnames(1) = 'nx'
  c_seaice%dimnames(2) = 'ny'
  c_seaice%istart(1) = 1
  c_seaice%istart(2) = 1
  c_seaice%iend(1) = nx
  c_seaice%iend(2) = ny

  c_snowh%fld = fillreal
  c_snowh%fldname = 'SNOWH'
  c_snowh%long_name = 'snow height'
  c_snowh%units = 'm'
  c_snowh%dimnames(1) = 'nx'
  c_snowh%dimnames(2) = 'ny'
  c_snowh%istart(1) = 1
  c_snowh%istart(2) = 1
  c_snowh%iend(1) = nx
  c_snowh%iend(2) = ny

  IF ( ifwr ) THEN
    c_wr%fld = fillreal
    c_wr%fldname = 'WR'
    c_wr%long_name = 'canopy moisture content'
    c_wr%units = 'm'
    c_wr%dimnames(1) = 'nx'
    c_wr%dimnames(2) = 'ny'
    c_wr%istart(1) = 1
    c_wr%istart(2) = 1
    c_wr%iend(1) = nx
    c_wr%iend(2) = ny
  ENDIF

  IF ( ifsoil ) THEN
    c_soim1%fld = fillreal
    c_soim1%fldname = 'SOIM1'
    c_soim1%long_name = 'volumetric soil moisture in top cm'
    c_soim1%units = 'm3 m-3'
    c_soim1%dimnames(1) = 'nx'
    c_soim1%dimnames(2) = 'ny'
    c_soim1%istart(1) = 1
    c_soim1%istart(2) = 1
    c_soim1%iend(1) = nx
    c_soim1%iend(2) = ny

    c_soim2%fld = fillreal
    c_soim2%fldname = 'SOIM2'
    c_soim2%long_name = 'volumetric soil moisture in top m'
    c_soim2%units = 'm3 m-3'
    c_soim2%dimnames(1) = 'nx'
    c_soim2%dimnames(2) = 'ny'
    c_soim2%istart(1) = 1
    c_soim2%istart(2) = 1
    c_soim2%iend(1) = nx
    c_soim2%iend(2) = ny

    c_soit1%fld = fillreal
    c_soit1%fldname = 'SOIT1'
    c_soit1%long_name = 'soil temperature in top cm'
    c_soit1%units = 'K'
    c_soit1%dimnames(1) = 'nx'
    c_soit1%dimnames(2) = 'ny'
    c_soit1%istart(1) = 1
    c_soit1%istart(2) = 1
    c_soit1%iend(1) = nx
    c_soit1%iend(2) = ny

    c_soit2%fld = fillreal
    c_soit2%fldname = 'SOIT2'
    c_soit2%long_name = 'soil temperature in top m'
    c_soit2%units = 'K'
    c_soit2%dimnames(1) = 'nx'
    c_soit2%dimnames(2) = 'ny'
    c_soit2%istart(1) = 1
    c_soit2%istart(2) = 1
    c_soit2%iend(1) = nx
    c_soit2%iend(2) = ny

    c_sltyp%fld = fillreal
    c_sltyp%fldname = 'SLTYP'
    c_sltyp%long_name = 'soil texture type by USDA category'
    c_sltyp%units = '1'
    c_sltyp%dimnames(1) = 'nx'
    c_sltyp%dimnames(2) = 'ny'
    c_sltyp%istart(1) = 1
    c_sltyp%istart(2) = 1
    c_sltyp%iend(1) = nx
    c_sltyp%iend(2) = ny
  ENDIF  ! ifsoil

  IF ( ifpxwrf41 ) THEN
    c_wsat_px%fld = fillreal
    c_wsat_px%fldname = 'WSAT_PX'
    c_wsat_px%long_name = 'soil saturation from PX LSM'
    c_wsat_px%units = 'm3 m-3'
    c_wsat_px%dimnames(1) = 'nx'
    c_wsat_px%dimnames(2) = 'ny'
    c_wsat_px%istart(1) = 1
    c_wsat_px%istart(2) = 1
    c_wsat_px%iend(1) = nx
    c_wsat_px%iend(2) = ny

    c_wfc_px%fld = fillreal
    c_wfc_px%fldname = 'WFC_PX'
    c_wfc_px%long_name = 'soil field capacity from PX LSM'
    c_wfc_px%units = 'm3 m-3'
    c_wfc_px%dimnames(1) = 'nx'
    c_wfc_px%dimnames(2) = 'ny'
    c_wfc_px%istart(1) = 1
    c_wfc_px%istart(2) = 1
    c_wfc_px%iend(1) = nx
    c_wfc_px%iend(2) = ny

    c_wwlt_px%fld = fillreal
    c_wwlt_px%fldname = 'WWLT_PX'
    c_wwlt_px%long_name = 'soil wilting point from PX LSM'
    c_wwlt_px%units = 'm3 m-3'
    c_wwlt_px%dimnames(1) = 'nx'
    c_wwlt_px%dimnames(2) = 'ny'
    c_wwlt_px%istart(1) = 1
    c_wwlt_px%istart(2) = 1
    c_wwlt_px%iend(1) = nx
    c_wwlt_px%iend(2) = ny

    c_csand_px%fld = fillreal
    c_csand_px%fldname = 'CSAND_PX'
    c_csand_px%long_name = 'coarse sand from PX LSM'
    c_csand_px%units = '1'
    c_csand_px%dimnames(1) = 'nx'
    c_csand_px%dimnames(2) = 'ny'
    c_csand_px%istart(1) = 1
    c_csand_px%istart(2) = 1
    c_csand_px%iend(1) = nx
    c_csand_px%iend(2) = ny

    c_fmsand_px%fld = fillreal
    c_fmsand_px%fldname = 'FMSAND_PX'
    c_fmsand_px%long_name = 'fine-medium sand from PX LSM'
    c_fmsand_px%units = '1'
    c_fmsand_px%dimnames(1) = 'nx'
    c_fmsand_px%dimnames(2) = 'ny'
    c_fmsand_px%istart(1) = 1
    c_fmsand_px%istart(2) = 1
    c_fmsand_px%iend(1) = nx
    c_fmsand_px%iend(2) = ny

    c_clay_px%fld = fillreal
    c_clay_px%fldname = 'CLAY_PX'
    c_clay_px%long_name = 'clay from PX LSM'
    c_clay_px%units = '1'
    c_clay_px%dimnames(1) = 'nx'
    c_clay_px%dimnames(2) = 'ny'
    c_clay_px%istart(1) = 1
    c_clay_px%istart(2) = 1
    c_clay_px%iend(1) = nx
    c_clay_px%iend(2) = ny
  ENDIF  ! ifpxwrf41

!-------------------------------------------------------------------------------
! Time-varying 3d fields at cell centers.
!-------------------------------------------------------------------------------

  c_jacobf%fld = fillreal
  c_jacobf%bdy = fillreal
  c_jacobf%fldname = 'JACOBF'
  c_jacobf%long_name = 'Jacobian at layer face scaled by MSFX2'
  c_jacobf%units = 'm'
  c_jacobf%dimnames(1) = 'nx'
  c_jacobf%dimnames(2) = 'ny'
  c_jacobf%dimnames(3) = 'nz'
  c_jacobf%istart(1) = 1
  c_jacobf%istart(2) = 1
  c_jacobf%istart(3) = 1
  c_jacobf%iend(1) = nx
  c_jacobf%iend(2) = ny
  c_jacobf%iend(3) = nz
  c_jacobf%dimnames_b(1) = 'nperim'
  c_jacobf%dimnames_b(2) = 'nz'
  c_jacobf%istart_b(1) = 1
  c_jacobf%istart_b(2) = 1
  c_jacobf%iend_b(1) = nperim
  c_jacobf%iend_b(2) = nz

  c_jacobm%fld = fillreal
  c_jacobm%bdy = fillreal
  c_jacobm%fldname = 'JACOBM'
  c_jacobm%long_name = 'Jacobian at layer middle scaled by MSFX2'
  c_jacobm%units = 'm'
  c_jacobm%dimnames(1) = 'nx'
  c_jacobm%dimnames(2) = 'ny'
  c_jacobm%dimnames(3) = 'nz'
  c_jacobm%istart(1) = 1
  c_jacobm%istart(2) = 1
  c_jacobm%istart(3) = 1
  c_jacobm%iend(1) = nx
  c_jacobm%iend(2) = ny
  c_jacobm%iend(3) = nz
  c_jacobm%dimnames_b(1) = 'nperim'
  c_jacobm%dimnames_b(2) = 'nz'
  c_jacobm%istart_b(1) = 1
  c_jacobm%istart_b(2) = 1
  c_jacobm%iend_b(1) = nperim
  c_jacobm%iend_b(2) = nz

  c_densa_j%fld = fillreal
  c_densa_j%bdy = fillreal
  c_densa_j%fldname = 'DENSA_J'
  c_densa_j%long_name = 'J-weighted air density (dry) scaled by MSFX2'
  c_densa_j%units = 'kg m-2'
  c_densa_j%dimnames(1) = 'nx'
  c_densa_j%dimnames(2) = 'ny'
  c_densa_j%dimnames(3) = 'nz'
  c_densa_j%istart(1) = 1
  c_densa_j%istart(2) = 1
  c_densa_j%istart(3) = 1
  c_densa_j%iend(1) = nx
  c_densa_j%iend(2) = ny
  c_densa_j%iend(3) = nz
  c_densa_j%dimnames_b(1) = 'nperim'
  c_densa_j%dimnames_b(2) = 'nz'
  c_densa_j%istart_b(1) = 1
  c_densa_j%istart_b(2) = 1
  c_densa_j%iend_b(1) = nperim
  c_densa_j%iend_b(2) = nz

  c_what_jd%fld = fillreal
  c_what_jd%bdy = fillreal
  c_what_jd%fldname = 'WHAT_JD'
  c_what_jd%long_name = 'J- and density weighted vert contravariant-W'
  c_what_jd%units = 'kg m-1 s-1'
  c_what_jd%dimnames(1) = 'nx'
  c_what_jd%dimnames(2) = 'ny'
  c_what_jd%dimnames(3) = 'nz'
  c_what_jd%istart(1) = 1
  c_what_jd%istart(2) = 1
  c_what_jd%istart(3) = 1
  c_what_jd%iend(1) = nx
  c_what_jd%iend(2) = ny
  c_what_jd%iend(3) = nz
  c_what_jd%dimnames_b(1) = 'nperim'
  c_what_jd%dimnames_b(2) = 'nz'
  c_what_jd%istart_b(1) = 1
  c_what_jd%istart_b(2) = 1
  c_what_jd%iend_b(1) = nperim
  c_what_jd%iend_b(2) = nz

  c_ta%fld = fillreal
  c_ta%bdy = fillreal
  c_ta%fldname = 'TA'
  c_ta%long_name = 'air temperature'
  c_ta%units = 'K'
  c_ta%dimnames(1) = 'nx'
  c_ta%dimnames(2) = 'ny'
  c_ta%dimnames(3) = 'nz'
  c_ta%istart(1) = 1
  c_ta%istart(2) = 1
  c_ta%istart(3) = 1
  c_ta%iend(1) = nx
  c_ta%iend(2) = ny
  c_ta%iend(3) = nz
  c_ta%dimnames_b(1) = 'nperim'
  c_ta%dimnames_b(2) = 'nz'
  c_ta%istart_b(1) = 1
  c_ta%istart_b(2) = 1
  c_ta%iend_b(1) = nperim
  c_ta%iend_b(2) = nz

  c_qv%fld = fillreal
  c_qv%bdy = fillreal
  c_qv%fldname = 'QV'
  c_qv%long_name = 'water vapor mixing ratio'
  c_qv%units = 'kg kg-1'
  c_qv%dimnames(1) = 'nx'
  c_qv%dimnames(2) = 'ny'
  c_qv%dimnames(3) = 'nz'
  c_qv%istart(1) = 1
  c_qv%istart(2) = 1
  c_qv%istart(3) = 1
  c_qv%iend(1) = nx
  c_qv%iend(2) = ny
  c_qv%iend(3) = nz
  c_qv%dimnames_b(1) = 'nperim'
  c_qv%dimnames_b(2) = 'nz'
  c_qv%istart_b(1) = 1
  c_qv%istart_b(2) = 1
  c_qv%iend_b(1) = nperim
  c_qv%iend_b(2) = nz

  c_pres%fld = fillreal
  c_pres%bdy = fillreal
  c_pres%fldname = 'PRES'
  c_pres%long_name = 'pressure'
  c_pres%units = 'Pa'
  c_pres%dimnames(1) = 'nx'
  c_pres%dimnames(2) = 'ny'
  c_pres%dimnames(3) = 'nz'
  c_pres%istart(1) = 1
  c_pres%istart(2) = 1
  c_pres%istart(3) = 1
  c_pres%iend(1) = nx
  c_pres%iend(2) = ny
  c_pres%iend(3) = nz
  c_pres%dimnames_b(1) = 'nperim'
  c_pres%dimnames_b(2) = 'nz'
  c_pres%istart_b(1) = 1
  c_pres%istart_b(2) = 1
  c_pres%iend_b(1) = nperim
  c_pres%iend_b(2) = nz

  c_dens%fld = fillreal
  c_dens%bdy = fillreal
  c_dens%fldname = 'DENS'
  c_dens%long_name = 'density of air (dry)'
  c_dens%units = 'kg m-3'
  c_dens%dimnames(1) = 'nx'
  c_dens%dimnames(2) = 'ny'
  c_dens%dimnames(3) = 'nz'
  c_dens%istart(1) = 1
  c_dens%istart(2) = 1
  c_dens%istart(3) = 1
  c_dens%iend(1) = nx
  c_dens%iend(2) = ny
  c_dens%iend(3) = nz
  c_dens%dimnames_b(1) = 'nperim'
  c_dens%dimnames_b(2) = 'nz'
  c_dens%istart_b(1) = 1
  c_dens%istart_b(2) = 1
  c_dens%iend_b(1) = nperim
  c_dens%iend_b(2) = nz

  c_zh%fld = fillreal
  c_zh%bdy = fillreal
  c_zh%fldname = 'ZH'
  c_zh%long_name = 'mid-layer height above ground'
  c_zh%units = 'm'
  c_zh%dimnames(1) = 'nx'
  c_zh%dimnames(2) = 'ny'
  c_zh%dimnames(3) = 'nz'
  c_zh%istart(1) = 1
  c_zh%istart(2) = 1
  c_zh%istart(3) = 1
  c_zh%iend(1) = nx
  c_zh%iend(2) = ny
  c_zh%iend(3) = nz
  c_zh%dimnames_b(1) = 'nperim'
  c_zh%dimnames_b(2) = 'nz'
  c_zh%istart_b(1) = 1
  c_zh%istart_b(2) = 1
  c_zh%iend_b(1) = nperim
  c_zh%iend_b(2) = nz

  c_zf%fld = fillreal
  c_zf%bdy = fillreal
  c_zf%fldname = 'ZF'
  c_zf%long_name = 'full-layer height above ground'
  c_zf%units = 'm'
  c_zf%dimnames(1) = 'nx'
  c_zf%dimnames(2) = 'ny'
  c_zf%dimnames(3) = 'nz'
  c_zf%istart(1) = 1
  c_zf%istart(2) = 1
  c_zf%istart(3) = 1
  c_zf%iend(1) = nx
  c_zf%iend(2) = ny
  c_zf%iend(3) = nz
  c_zf%dimnames_b(1) = 'nperim'
  c_zf%dimnames_b(2) = 'nz'
  c_zf%istart_b(1) = 1
  c_zf%istart_b(2) = 1
  c_zf%iend_b(1) = nperim
  c_zf%iend_b(2) = nz

  IF ( iftke ) THEN
    c_tke%fld = fillreal
    c_tke%bdy = fillreal
    IF ( iftkef ) THEN
      c_tke%fldname = 'TKEF'
      c_tke%long_name = 'turbulent kinetic energy on full-levels'
    ELSE
      c_tke%fldname = 'TKE'
      c_tke%long_name = 'turbulent kinetic energy on half-layers'
    ENDIF
    c_tke%units = 'J kg-1'
    c_tke%dimnames(1) = 'nx'
    c_tke%dimnames(2) = 'ny'
    c_tke%dimnames(3) = 'nz'
    c_tke%istart(1) = 1
    c_tke%istart(2) = 1
    c_tke%istart(3) = 1
    c_tke%iend(1) = nx
    c_tke%iend(2) = ny
    c_tke%iend(3) = nz
    c_tke%dimnames_b(1) = 'nperim'
    c_tke%dimnames_b(2) = 'nz'
    c_tke%istart_b(1) = 1
    c_tke%istart_b(2) = 1
    c_tke%iend_b(1) = nperim
    c_tke%iend_b(2) = nz
  ENDIF

  IF ( lpv > 0 ) THEN
    c_pv%fld = fillreal
    c_pv%bdy = fillreal
    c_pv%fldname = 'PV'
    c_pv%long_name = 'potential vorticity (m2 K kg-1 s-1 * E-6)'
    c_pv%units = 'm2 K mg-1 s-1'  ! <-- use scale_factor to change mg to kg
    c_pv%dimnames(1) = 'nx'
    c_pv%dimnames(2) = 'ny'
    c_pv%dimnames(3) = 'nz'
    c_pv%istart(1) = 1
    c_pv%istart(2) = 1
    c_pv%istart(3) = 1
    c_pv%iend(1) = nx
    c_pv%iend(2) = ny
    c_pv%iend(3) = nz
    c_pv%dimnames_b(1) = 'nperim'
    c_pv%dimnames_b(2) = 'nz'
    c_pv%istart_b(1) = 1
    c_pv%istart_b(2) = 1
    c_pv%iend_b(1) = nperim
    c_pv%iend_b(2) = nz
  ENDIF

  IF ( lwout > 0 ) THEN
    c_wwind%fld = fillreal
    c_wwind%bdy = fillreal
    c_wwind%fldname = 'WWIND'
    c_wwind%long_name = 'vertical velocity'
    c_wwind%units = 'm s-1'
    c_wwind%dimnames(1) = 'nx'
    c_wwind%dimnames(2) = 'ny'
    c_wwind%dimnames(3) = 'nz'
    c_wwind%istart(1) = 1
    c_wwind%istart(2) = 1
    c_wwind%istart(3) = 1
    c_wwind%iend(1) = nx
    c_wwind%iend(2) = ny
    c_wwind%iend(3) = nz
    c_wwind%dimnames_b(1) = 'nperim'
    c_wwind%dimnames_b(2) = 'nz'
    c_wwind%istart_b(1) = 1
    c_wwind%istart_b(2) = 1
    c_wwind%iend_b(1) = nperim
    c_wwind%iend_b(2) = nz
  ENDIF

  IF ( ifcld3d ) THEN
    c_cfrac_3d%fld = fillreal
    c_cfrac_3d%bdy = fillreal
    c_cfrac_3d%fldname = 'CFRAC_3D'
    c_cfrac_3d%long_name = '3D resolved cloud fraction'
    c_cfrac_3d%units = '1'
    c_cfrac_3d%dimnames(1) = 'nx'
    c_cfrac_3d%dimnames(2) = 'ny'
    c_cfrac_3d%dimnames(3) = 'nz'
    c_cfrac_3d%istart(1) = 1
    c_cfrac_3d%istart(2) = 1
    c_cfrac_3d%istart(3) = 1
    c_cfrac_3d%iend(1) = nx
    c_cfrac_3d%iend(2) = ny
    c_cfrac_3d%iend(3) = nz
    c_cfrac_3d%dimnames_b(1) = 'nperim'
    c_cfrac_3d%dimnames_b(2) = 'nz'
    c_cfrac_3d%istart_b(1) = 1
    c_cfrac_3d%istart_b(2) = 1
    c_cfrac_3d%iend_b(1) = nperim
    c_cfrac_3d%iend_b(2) = nz
  ENDIF

  IF ( nqspecies >= 2 ) THEN

    c_qc%fld = fillreal
    c_qc%bdy = fillreal
    c_qc%fldname = 'QC'
    c_qc%long_name = 'cloud water mixing ratio'
    c_qc%units = 'kg kg-1'
    c_qc%dimnames(1) = 'nx'
    c_qc%dimnames(2) = 'ny'
    c_qc%dimnames(3) = 'nz'
    c_qc%istart(1) = 1
    c_qc%istart(2) = 1
    c_qc%istart(3) = 1
    c_qc%iend(1) = nx
    c_qc%iend(2) = ny
    c_qc%iend(3) = nz
    c_qc%dimnames_b(1) = 'nperim'
    c_qc%dimnames_b(2) = 'nz'
    c_qc%istart_b(1) = 1
    c_qc%istart_b(2) = 1
    c_qc%iend_b(1) = nperim
    c_qc%iend_b(2) = nz

    c_qr%fld = fillreal
    c_qr%bdy = fillreal
    c_qr%fldname = 'QR'
    c_qr%long_name = 'rain water mixing ratio'
    c_qr%units = 'kg kg-1'
    c_qr%dimnames(1) = 'nx'
    c_qr%dimnames(2) = 'ny'
    c_qr%dimnames(3) = 'nz'
    c_qr%istart(1) = 1
    c_qr%istart(2) = 1
    c_qr%istart(3) = 1
    c_qr%iend(1) = nx
    c_qr%iend(2) = ny
    c_qr%iend(3) = nz
    c_qr%dimnames_b(1) = 'nperim'
    c_qr%dimnames_b(2) = 'nz'
    c_qr%istart_b(1) = 1
    c_qr%istart_b(2) = 1
    c_qr%iend_b(1) = nperim
    c_qr%iend_b(2) = nz

    IF ( nqspecies >= 4 ) THEN
      c_qi%fld = fillreal
      c_qi%bdy = fillreal
      c_qi%fldname = 'QI'
      c_qi%long_name = 'ice mixing ratio'
      c_qi%units = 'kg kg-1'
      c_qi%dimnames(1) = 'nx'
      c_qi%dimnames(2) = 'ny'
      c_qi%dimnames(3) = 'nz'
      c_qi%istart(1) = 1
      c_qi%istart(2) = 1
      c_qi%istart(3) = 1
      c_qi%iend(1) = nx
      c_qi%iend(2) = ny
      c_qi%iend(3) = nz
      c_qi%dimnames_b(1) = 'nperim'
      c_qi%dimnames_b(2) = 'nz'
      c_qi%istart_b(1) = 1
      c_qi%istart_b(2) = 1
      c_qi%iend_b(1) = nperim
      c_qi%iend_b(2) = nz

      c_qs%fld = fillreal
      c_qs%bdy = fillreal
      c_qs%fldname = 'QS'
      c_qs%long_name = 'snow mixing ratio'
      c_qs%units = 'kg kg-1'
      c_qs%dimnames(1) = 'nx'
      c_qs%dimnames(2) = 'ny'
      c_qs%dimnames(3) = 'nz'
      c_qs%istart(1) = 1
      c_qs%istart(2) = 1
      c_qs%istart(3) = 1
      c_qs%iend(1) = nx
      c_qs%iend(2) = ny
      c_qs%iend(3) = nz
      c_qs%dimnames_b(1) = 'nperim'
      c_qs%dimnames_b(2) = 'nz'
      c_qs%istart_b(1) = 1
      c_qs%istart_b(2) = 1
      c_qs%iend_b(1) = nperim
      c_qs%iend_b(2) = nz

      IF ( nqspecies >= 5 ) THEN
        c_qg%fld = fillreal
        c_qg%bdy = fillreal
        c_qg%fldname = 'QG'
        c_qg%long_name = 'graupel mixing ratio'
        c_qg%units = 'kg kg-1'
        c_qg%dimnames(1) = 'nx'
        c_qg%dimnames(2) = 'ny'
        c_qg%dimnames(3) = 'nz'
        c_qg%istart(1) = 1
        c_qg%istart(2) = 1
        c_qg%istart(3) = 1
        c_qg%iend(1) = nx
        c_qg%iend(2) = ny
        c_qg%iend(3) = nz
        c_qg%dimnames_b(1) = 'nperim'
        c_qg%dimnames_b(2) = 'nz'
        c_qg%istart_b(1) = 1
        c_qg%istart_b(2) = 1
        c_qg%iend_b(1) = nperim
        c_qg%iend_b(2) = nz
      ENDIF

      IF ( ifkfradextras ) THEN

        c_qc_cu%fld = fillreal
        c_qc_cu%bdy = fillreal
        c_qc_cu%fldname = 'QC_CU'
        c_qc_cu%long_name = 'subgrid cloud water mixing ratio from KF'
        c_qc_cu%units = 'kg kg-1'
        c_qc_cu%dimnames(1) = 'nx'
        c_qc_cu%dimnames(2) = 'ny'
        c_qc_cu%dimnames(3) = 'nz'
        c_qc_cu%istart(1) = 1
        c_qc_cu%istart(2) = 1
        c_qc_cu%istart(3) = 1
        c_qc_cu%iend(1) = nx
        c_qc_cu%iend(2) = ny
        c_qc_cu%iend(3) = nz
        c_qc_cu%dimnames_b(1) = 'nperim'
        c_qc_cu%dimnames_b(2) = 'nz'
        c_qc_cu%istart_b(1) = 1
        c_qc_cu%istart_b(2) = 1
        c_qc_cu%iend_b(1) = nperim
        c_qc_cu%iend_b(2) = nz

        c_qi_cu%fld = fillreal
        c_qi_cu%bdy = fillreal
        c_qi_cu%fldname = 'QI_CU'
        c_qi_cu%long_name = 'subgrid cloud ice mixing ratio from KF'
        c_qi_cu%units = 'kg kg-1'
        c_qi_cu%dimnames(1) = 'nx'
        c_qi_cu%dimnames(2) = 'ny'
        c_qi_cu%dimnames(3) = 'nz'
        c_qi_cu%istart(1) = 1
        c_qi_cu%istart(2) = 1
        c_qi_cu%istart(3) = 1
        c_qi_cu%iend(1) = nx
        c_qi_cu%iend(2) = ny
        c_qi_cu%iend(3) = nz
        c_qi_cu%dimnames_b(1) = 'nperim'
        c_qi_cu%dimnames_b(2) = 'nz'
        c_qi_cu%istart_b(1) = 1
        c_qi_cu%istart_b(2) = 1
        c_qi_cu%iend_b(1) = nperim
        c_qi_cu%iend_b(2) = nz

        c_cldfra_dp%fld = fillreal
        c_cldfra_dp%bdy = fillreal
        c_cldfra_dp%fldname = 'CLDFRA_DP'
        c_cldfra_dp%long_name = 'subgrid deep cloud fraction'
        c_cldfra_dp%units = '1'
        c_cldfra_dp%dimnames(1) = 'nx'
        c_cldfra_dp%dimnames(2) = 'ny'
        c_cldfra_dp%dimnames(3) = 'nz'
        c_cldfra_dp%istart(1) = 1
        c_cldfra_dp%istart(2) = 1
        c_cldfra_dp%istart(3) = 1
        c_cldfra_dp%iend(1) = nx
        c_cldfra_dp%iend(2) = ny
        c_cldfra_dp%iend(3) = nz
        c_cldfra_dp%dimnames_b(1) = 'nperim'
        c_cldfra_dp%dimnames_b(2) = 'nz'
        c_cldfra_dp%istart_b(1) = 1
        c_cldfra_dp%istart_b(2) = 1
        c_cldfra_dp%iend_b(1) = nperim
        c_cldfra_dp%iend_b(2) = nz

        c_cldfra_sh%fld = fillreal
        c_cldfra_sh%bdy = fillreal
        c_cldfra_sh%fldname = 'CLDFRA_SH'
        c_cldfra_sh%long_name = 'subgrid shallow cloud fraction'
        c_cldfra_sh%units = '1'
        c_cldfra_sh%dimnames(1) = 'nx'
        c_cldfra_sh%dimnames(2) = 'ny'
        c_cldfra_sh%dimnames(3) = 'nz'
        c_cldfra_sh%istart(1) = 1
        c_cldfra_sh%istart(2) = 1
        c_cldfra_sh%istart(3) = 1
        c_cldfra_sh%iend(1) = nx
        c_cldfra_sh%iend(2) = ny
        c_cldfra_sh%iend(3) = nz
        c_cldfra_sh%dimnames_b(1) = 'nperim'
        c_cldfra_sh%dimnames_b(2) = 'nz'
        c_cldfra_sh%istart_b(1) = 1
        c_cldfra_sh%istart_b(2) = 1
        c_cldfra_sh%iend_b(1) = nperim
        c_cldfra_sh%iend_b(2) = nz

      ENDIF  ! kfradextras

    ENDIF  ! nqspecies >= 4

  ENDIF ! nqspecies >= 2

!-------------------------------------------------------------------------------
! Time-varying 3d fields at cell corners and cell faces.
!-------------------------------------------------------------------------------

  c_uwindc%fld = fillreal
  c_uwindc%fldname = 'UWINDC'
  c_uwindc%long_name = 'U-comp. of true wind at W-E faces'
  c_uwindc%units = 'm s-1'
  c_uwindc%dimnames(1) = 'nxp1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_uwindc%dimnames(2) = 'nyp1'
  ELSE  ! preserve Arakawa-C staggering
    c_uwindc%dimnames(2) = 'ny'
  ENDIF
  c_uwindc%dimnames(3) = 'nz'
  c_uwindc%istart(1) = 1
  c_uwindc%istart(2) = 1
  c_uwindc%istart(3) = 1
  c_uwindc%iend(1) = nxp1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_uwindc%iend(2) = nyp1
  ELSE  ! preserve Arakawa-C staggering
    c_uwindc%iend(2) = ny
  ENDIF
  c_uwindc%iend(3) = nz

  c_vwindc%fld = fillreal
  c_vwindc%fldname = 'VWINDC'
  c_vwindc%long_name = 'V-comp. of true wind at S-N faces'
  c_vwindc%units = 'm s-1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_vwindc%dimnames(1) = 'nxp1'
  ELSE  ! preserve Arakawa-C staggering
    c_vwindc%dimnames(1) = 'nx'
  ENDIF
  c_vwindc%dimnames(2) = 'nyp1'
  c_vwindc%dimnames(3) = 'nz'
  c_vwindc%istart(1) = 1
  c_vwindc%istart(2) = 1
  c_vwindc%istart(3) = 1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_vwindc%iend(1) = nxp1
  ELSE  ! preserve Arakawa-C staggering
    c_vwindc%iend(1) = nx
  ENDIF
  c_vwindc%iend(2) = nyp1
  c_vwindc%iend(3) = nz

  c_uhat_jd%fld = fillreal
  c_uhat_jd%fldname = 'UHAT_JD'
  c_uhat_jd%long_name = '(contravariant_U*Jacobian*Density) at square pt'
  c_uhat_jd%units = 'kg m-1 s-1'
  c_uhat_jd%dimnames(1) = 'nxp1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_uhat_jd%dimnames(2) = 'nyp1'
  ELSE  ! preserve Arakawa-C staggering
    c_uhat_jd%dimnames(2) = 'ny'
  ENDIF
  c_uhat_jd%dimnames(3) = 'nz'
  c_uhat_jd%istart(1) = 1
  c_uhat_jd%istart(2) = 1
  c_uhat_jd%istart(3) = 1
  c_uhat_jd%iend(1) = nxp1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_uhat_jd%iend(2) = nyp1
  ELSE  ! preserve Arakawa-C staggering
    c_uhat_jd%iend(2) = ny
  ENDIF
  c_uhat_jd%iend(3) = nz

  c_vhat_jd%fld = fillreal
  c_vhat_jd%fldname = 'VHAT_JD'
  c_vhat_jd%long_name = '(contravariant_V*Jacobian*Density) at triangle pt'
  c_vhat_jd%units = 'kg m-1 s-1'
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_vhat_jd%dimnames(1) = 'nxp1'
  ELSE  ! preserve Arakawa-C staggering
    c_vhat_jd%dimnames(1) = 'nx'
  ENDIF
  c_vhat_jd%dimnames(2) = 'nyp1'
  c_vhat_jd%dimnames(3) = 'nz'
  c_vhat_jd%istart(1) = 1
  c_vhat_jd%istart(2) = 1
  c_vhat_jd%istart(3) = 1
  IF ( ioform == 1 ) THEN  ! M3IO -- keep false dot points
    c_vhat_jd%iend(1) = nxp1
  ELSE  ! preserve Arakawa-C staggering
    c_vhat_jd%iend(1) = nx
  ENDIF
  c_vhat_jd%iend(2) = nyp1
  c_vhat_jd%iend(3) = nz

  IF ( luvbout > 0 ) THEN

    c_uwind%fld = fillreal
    c_uwind%fldname = 'UWIND'
    c_uwind%long_name = 'U-comp. of true wind at dot point'
    c_uwind%units = 'm s-1'
    c_uwind%dimnames(1) = 'nxp1'
    c_uwind%dimnames(2) = 'nyp1'
    c_uwind%dimnames(3) = 'nz'
    c_uwind%istart(1) = 1
    c_uwind%istart(2) = 1
    c_uwind%istart(3) = 1
    c_uwind%iend(1) = nxp1
    c_uwind%iend(2) = nyp1
    c_uwind%iend(3) = nz

    c_vwind%fld = fillreal
    c_vwind%fldname = 'VWIND'
    c_vwind%long_name = 'V-comp. of true wind at dot point'
    c_vwind%units = 'm s-1'
    c_vwind%dimnames(1) = 'nxp1'
    c_vwind%dimnames(2) = 'nyp1'
    c_vwind%dimnames(3) = 'nz'
    c_vwind%istart(1) = 1
    c_vwind%istart(2) = 1
    c_vwind%istart(3) = 1
    c_vwind%iend(1) = nxp1
    c_vwind%iend(2) = nyp1
    c_vwind%iend(3) = nz

  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 3d fields (soil layers) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifsoil ) THEN

    c_soit3d%fld = fillreal
    c_soit3d%fldname = 'SOIT3D'
    c_soit3d%long_name = 'soil temperature'
    c_soit3d%units = 'K'
    c_soit3d%dimnames(1) = 'nx'
    c_soit3d%dimnames(2) = 'ny'
    c_soit3d%dimnames(3) = 'nsoi'
    c_soit3d%istart(1) = 1
    c_soit3d%istart(2) = 1
    c_soit3d%istart(3) = 1
    c_soit3d%iend(1) = nx
    c_soit3d%iend(2) = ny
    c_soit3d%iend(3) = nsoi

    c_soim3d%fld = fillreal
    c_soim3d%fldname = 'SOIM3D'
    c_soim3d%long_name = 'soil moisture'
    c_soim3d%units = 'm3 m-3'
    c_soim3d%dimnames(1) = 'nx'
    c_soim3d%dimnames(2) = 'ny'
    c_soim3d%dimnames(3) = 'nsoi'
    c_soim3d%istart(1) = 1
    c_soim3d%istart(2) = 1
    c_soim3d%istart(3) = 1
    c_soim3d%iend(1) = nx
    c_soim3d%iend(2) = ny
    c_soim3d%iend(3) = nsoi

  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 3d fields (mosaic land use categories) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifmosaic ) THEN

    c_lufrac2%fld = fillreal
    c_lufrac2%fldname = 'LUFRAC2'
    c_lufrac2%long_name = 'ranked fractional land use'
    c_lufrac2%units = 'percent'
    c_lufrac2%dimnames(1) = 'nx'
    c_lufrac2%dimnames(2) = 'ny'
    c_lufrac2%dimnames(3) = 'nmos'
    c_lufrac2%istart(1) = 1
    c_lufrac2%istart(2) = 1
    c_lufrac2%istart(3) = 1
    c_lufrac2%iend(1) = nx
    c_lufrac2%iend(2) = ny
    c_lufrac2%iend(3) = nmos

    c_moscat%fld = fillreal
    c_moscat%fldname = 'MOSCAT'
    c_moscat%long_name = 'land use category for LUFRAC2'
    c_moscat%units = '1'
    c_moscat%dimnames(1) = 'nx'
    c_moscat%dimnames(2) = 'ny'
    c_moscat%dimnames(3) = 'nmos'
    c_moscat%istart(1) = 1
    c_moscat%istart(2) = 1
    c_moscat%istart(3) = 1
    c_moscat%iend(1) = nx
    c_moscat%iend(2) = ny
    c_moscat%iend(3) = nmos

    c_lai_mos%fld = fillreal
    c_lai_mos%fldname = 'LAI_MOS'
    c_lai_mos%long_name = 'leaf area index (mosaic)'
    c_lai_mos%units = 'm2 m-2'
    c_lai_mos%dimnames(1) = 'nx'
    c_lai_mos%dimnames(2) = 'ny'
    c_lai_mos%dimnames(3) = 'nmos'
    c_lai_mos%istart(1) = 1
    c_lai_mos%istart(2) = 1
    c_lai_mos%istart(3) = 1
    c_lai_mos%iend(1) = nx
    c_lai_mos%iend(2) = ny
    c_lai_mos%iend(3) = nmos

    c_rai_mos%fld = fillreal
    c_rai_mos%fldname = 'RAI_MOS'
    c_rai_mos%long_name = 'inverse of aerodynamic resistance (mosaic)'
    c_rai_mos%units = 'm s-1'
    c_rai_mos%dimnames(1) = 'nx'
    c_rai_mos%dimnames(2) = 'ny'
    c_rai_mos%dimnames(3) = 'nmos'
    c_rai_mos%istart(1) = 1
    c_rai_mos%istart(2) = 1
    c_rai_mos%istart(3) = 1
    c_rai_mos%iend(1) = nx
    c_rai_mos%iend(2) = ny
    c_rai_mos%iend(3) = nmos

    c_rsi_mos%fld = fillreal
    c_rsi_mos%fldname = 'RSI_MOS'
    c_rsi_mos%long_name = 'inverse of stomatal resistance (mosaic)'
    c_rsi_mos%units = 'm s-1'
    c_rsi_mos%dimnames(1) = 'nx'
    c_rsi_mos%dimnames(2) = 'ny'
    c_rsi_mos%dimnames(3) = 'nmos'
    c_rsi_mos%istart(1) = 1
    c_rsi_mos%istart(2) = 1
    c_rsi_mos%istart(3) = 1
    c_rsi_mos%iend(1) = nx
    c_rsi_mos%iend(2) = ny
    c_rsi_mos%iend(3) = nmos

    c_tsk_mos%fld = fillreal
    c_tsk_mos%fldname = 'TSK_MOS'
    c_tsk_mos%long_name = 'vegetation temperature (mosaic)'
    c_tsk_mos%units = 'K'
    c_tsk_mos%dimnames(1) = 'nx'
    c_tsk_mos%dimnames(2) = 'ny'
    c_tsk_mos%dimnames(3) = 'nmos'
    c_tsk_mos%istart(1) = 1
    c_tsk_mos%istart(2) = 1
    c_tsk_mos%istart(3) = 1
    c_tsk_mos%iend(1) = nx
    c_tsk_mos%iend(2) = ny
    c_tsk_mos%iend(3) = nmos

    c_znt_mos%fld = fillreal
    c_znt_mos%fldname = 'ZNT_MOS'
    c_znt_mos%long_name = 'roughness length (mosaic)'
    c_znt_mos%units = 'm'
    c_znt_mos%dimnames(1) = 'nx'
    c_znt_mos%dimnames(2) = 'ny'
    c_znt_mos%dimnames(3) = 'nmos'
    c_znt_mos%istart(1) = 1
    c_znt_mos%istart(2) = 1
    c_znt_mos%istart(3) = 1
    c_znt_mos%iend(1) = nx
    c_znt_mos%iend(2) = ny
    c_znt_mos%iend(3) = nmos

  ENDIF

END SUBROUTINE init_ctm
