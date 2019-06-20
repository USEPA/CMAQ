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

MODULE ctmvars

!-------------------------------------------------------------------------------
! Name:     Meteorology Variables Output for CTM
! Purpose:  Contains output meteorology arrays.
! Revised:  22 Jun 2018  Original version created by combining modules GROUTCOM,
!                        LUOUTCOM, MCOUTCOM, MDOUTCOM, MOSOUTCOM, and SOIOUTCOM.
!                        (T. Spero)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           13 Dec 2018  Built new data structures.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Define data structures.
!-------------------------------------------------------------------------------

  TYPE fld2ddata
    REAL,        POINTER       :: fld        ( : , : )
    REAL,        POINTER       :: bdy        ( : )
    CHARACTER(LEN=16)          :: fldname
    CHARACTER(LEN=80)          :: long_name
    CHARACTER(LEN=16)          :: units
    CHARACTER(LEN=16)          :: dimnames   ( 4 )
    INTEGER                    :: istart     ( 4 )
    INTEGER                    :: iend       ( 4 )
    CHARACTER(LEN=16)          :: dimnames_b ( 4 )
    INTEGER                    :: istart_b   ( 4 )
    INTEGER                    :: iend_b     ( 4 )
  END TYPE fld2ddata

  TYPE fld3ddata
    REAL,        POINTER       :: fld        ( : , : , : )
    REAL,        POINTER       :: bdy        ( : , : )
    CHARACTER(LEN=16)          :: fldname
    CHARACTER(LEN=80)          :: long_name
    CHARACTER(LEN=16)          :: units
    CHARACTER(LEN=16)          :: dimnames   ( 4 )
    INTEGER                    :: istart     ( 4 )
    INTEGER                    :: iend       ( 4 )
    CHARACTER(LEN=16)          :: dimnames_b ( 4 )
    INTEGER                    :: istart_b   ( 4 )
    INTEGER                    :: iend_b     ( 4 )
  END TYPE fld3ddata

!-------------------------------------------------------------------------------
! Time-independent 2d fields at cell centers.
!-------------------------------------------------------------------------------

  TYPE(fld2ddata), ALLOCATABLE, TARGET :: fld2dxy ( : )

  TYPE(fld2ddata), POINTER     :: g_lat
  TYPE(fld2ddata), POINTER     :: g_lon
  TYPE(fld2ddata), POINTER     :: g_msfx2
  TYPE(fld2ddata), POINTER     :: g_ht
  TYPE(fld2ddata), POINTER     :: g_dluse
  TYPE(fld2ddata), POINTER     :: g_lwmask
  TYPE(fld2ddata), POINTER     :: g_purb

!-------------------------------------------------------------------------------
! Time independent 2d fields at cell corners and cell faces.
!-------------------------------------------------------------------------------

  TYPE(fld2ddata), ALLOCATABLE, TARGET :: fld2dxy_d ( : )

  TYPE(fld2ddata), POINTER     :: g_latd
  TYPE(fld2ddata), POINTER     :: g_lond
  TYPE(fld2ddata), POINTER     :: g_msfd2
  TYPE(fld2ddata), POINTER     :: g_latu
  TYPE(fld2ddata), POINTER     :: g_lonu
  TYPE(fld2ddata), POINTER     :: g_msfu2
  TYPE(fld2ddata), POINTER     :: g_latv
  TYPE(fld2ddata), POINTER     :: g_lonv
  TYPE(fld2ddata), POINTER     :: g_msfv2

!-------------------------------------------------------------------------------
! Time-independent 3d fields (fractional land use) at cell centers.
!-------------------------------------------------------------------------------

  TYPE(fld3ddata), ALLOCATABLE, TARGET :: fld3dxyl ( : )

  TYPE(fld3ddata), POINTER     :: g_lufrac

!-------------------------------------------------------------------------------
! Time-varying 2d fields at cell centers.
!-------------------------------------------------------------------------------

  TYPE(fld2ddata), ALLOCATABLE, TARGET :: fld2dxyt ( : )

  TYPE(fld2ddata), POINTER     :: c_prsfc
  TYPE(fld2ddata), POINTER     :: c_ustar
  TYPE(fld2ddata), POINTER     :: c_wstar
  TYPE(fld2ddata), POINTER     :: c_pbl
  TYPE(fld2ddata), POINTER     :: c_zruf
  TYPE(fld2ddata), POINTER     :: c_moli
  TYPE(fld2ddata), POINTER     :: c_hfx
  TYPE(fld2ddata), POINTER     :: c_lh
  TYPE(fld2ddata), POINTER     :: c_radyni
  TYPE(fld2ddata), POINTER     :: c_rstomi
  TYPE(fld2ddata), POINTER     :: c_tempg
  TYPE(fld2ddata), POINTER     :: c_temp2
  TYPE(fld2ddata), POINTER     :: c_q2
  TYPE(fld2ddata), POINTER     :: c_wspd10
  TYPE(fld2ddata), POINTER     :: c_wdir10
  TYPE(fld2ddata), POINTER     :: c_glw
  TYPE(fld2ddata), POINTER     :: c_gsw
  TYPE(fld2ddata), POINTER     :: c_rgrnd
  TYPE(fld2ddata), POINTER     :: c_rn
  TYPE(fld2ddata), POINTER     :: c_rc
  TYPE(fld2ddata), POINTER     :: c_cfrac
  TYPE(fld2ddata), POINTER     :: c_cldt
  TYPE(fld2ddata), POINTER     :: c_cldb
  TYPE(fld2ddata), POINTER     :: c_wbar
  TYPE(fld2ddata), POINTER     :: c_snocov
  TYPE(fld2ddata), POINTER     :: c_veg
  TYPE(fld2ddata), POINTER     :: c_lai
  TYPE(fld2ddata), POINTER     :: c_seaice
  TYPE(fld2ddata), POINTER     :: c_snowh
  TYPE(fld2ddata), POINTER     :: c_wr
  TYPE(fld2ddata), POINTER     :: c_soim1
  TYPE(fld2ddata), POINTER     :: c_soim2
  TYPE(fld2ddata), POINTER     :: c_soit1
  TYPE(fld2ddata), POINTER     :: c_soit2
  TYPE(fld2ddata), POINTER     :: c_sltyp
  TYPE(fld2ddata), POINTER     :: c_wsat_px
  TYPE(fld2ddata), POINTER     :: c_wfc_px
  TYPE(fld2ddata), POINTER     :: c_wwlt_px
  TYPE(fld2ddata), POINTER     :: c_csand_px
  TYPE(fld2ddata), POINTER     :: c_fmsand_px
  TYPE(fld2ddata), POINTER     :: c_clay_px

!-------------------------------------------------------------------------------
! Time-varying 3d fields at cell centers.
!-------------------------------------------------------------------------------

  TYPE(fld3ddata), ALLOCATABLE, TARGET :: fld3dxyzt ( : )

  TYPE(fld3ddata), POINTER     :: c_jacobf
  TYPE(fld3ddata), POINTER     :: c_jacobm
  TYPE(fld3ddata), POINTER     :: c_densa_j
  TYPE(fld3ddata), POINTER     :: c_what_jd
  TYPE(fld3ddata), POINTER     :: c_ta
  TYPE(fld3ddata), POINTER     :: c_qv
  TYPE(fld3ddata), POINTER     :: c_pres
  TYPE(fld3ddata), POINTER     :: c_dens
  TYPE(fld3ddata), POINTER     :: c_zh
  TYPE(fld3ddata), POINTER     :: c_zf
  TYPE(fld3ddata), POINTER     :: c_tke
  TYPE(fld3ddata), POINTER     :: c_pv
  TYPE(fld3ddata), POINTER     :: c_wwind
  TYPE(fld3ddata), POINTER     :: c_cfrac_3d


  TYPE(fld3ddata), ALLOCATABLE, TARGET :: fld3dxyzt_q ( : )

  TYPE(fld3ddata), POINTER     :: c_qc
  TYPE(fld3ddata), POINTER     :: c_qr
  TYPE(fld3ddata), POINTER     :: c_qi
  TYPE(fld3ddata), POINTER     :: c_qs
  TYPE(fld3ddata), POINTER     :: c_qg
  TYPE(fld3ddata), POINTER     :: c_qc_cu
  TYPE(fld3ddata), POINTER     :: c_qi_cu
  TYPE(fld3ddata), POINTER     :: c_cldfra_dp
  TYPE(fld3ddata), POINTER     :: c_cldfra_sh

!-------------------------------------------------------------------------------
! Time-varying 3d fields at cell corners and cell faces.
!-------------------------------------------------------------------------------

  TYPE(fld3ddata), ALLOCATABLE, TARGET :: fld3dxyzt_d ( : )

  TYPE(fld3ddata), POINTER     :: c_uwindc
  TYPE(fld3ddata), POINTER     :: c_vwindc
  TYPE(fld3ddata), POINTER     :: c_uhat_jd
  TYPE(fld3ddata), POINTER     :: c_vhat_jd
  TYPE(fld3ddata), POINTER     :: c_uwind
  TYPE(fld3ddata), POINTER     :: c_vwind

!-------------------------------------------------------------------------------
! Time-varying 3d fields (soil layers) at cell centers.
!-------------------------------------------------------------------------------

  TYPE(fld3ddata), ALLOCATABLE, TARGET :: fld3dxyst ( : )

  TYPE(fld3ddata), POINTER     :: c_soit3d
  TYPE(fld3ddata), POINTER     :: c_soim3d

!-------------------------------------------------------------------------------
! Time-varying 3d fields (mosaic land use categories) at cell centers.
!-------------------------------------------------------------------------------

  TYPE(fld3ddata), ALLOCATABLE, TARGET :: fld3dxymt ( : )

  TYPE(fld3ddata), POINTER     :: c_lufrac2
  TYPE(fld3ddata), POINTER     :: c_moscat
  TYPE(fld3ddata), POINTER     :: c_lai_mos
  TYPE(fld3ddata), POINTER     :: c_rai_mos
  TYPE(fld3ddata), POINTER     :: c_rsi_mos
  TYPE(fld3ddata), POINTER     :: c_tsk_mos
  TYPE(fld3ddata), POINTER     :: c_znt_mos

END MODULE ctmvars
