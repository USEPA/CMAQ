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

MODULE metinfo

!-------------------------------------------------------------------------------
! Name:     Meteorology Information
! Purpose:  Contains information about the input meteorology fields.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added MM5 season.  (T. Otte and J. Pleim)
!           05 Aug 2004  Added MM5 snow option (MET_SNOW_OPT), cone factor
!                        (MET_CONE_FAC), and true latitude 1 (MET_TRU1).
!                        Removed unused variables MET_SDATE and MET_STIME.
!                        Added IFT2M.  (T. Otte)
!           31 Mar 2005  Changed variable names from I,J,K representation
!                        to Y,X,Z representation to make code more general.
!                        Added new variable MET_MODEL.  Allowed MET_IVERSION
!                        to be more generic to support MM5 and WRF.  Removed
!                        unused variables MET_MDATE, MET_IEXPAND, MET_IOFFSET,
!                        and MET_JOFFSET.  Added MET_TRU2 and IFW10M.  (T. Otte)
!           10 May 2006  Added new variables MET_PROJ_CLAT and MET_PROJ_CLON.
!                        (T. Otte)
!           30 Jul 2007  Added IMPLICIT NONE.  Changed comment related to
!                        MET_IVERSION to remove indicator for MM5v2-formatted
!                        data.  Removed variable MET_INHYD.  Moved IFT2M and
!                        IFW10M to module MCIPPARM.  Added MET_LU_SRC, MET_NS,
!                        MET_RELEASE, MET_FDDA_3DAN, MET_FDDA_SFAN, and
!                        MET_FDDA_OBS.  Changed MET_RADIATION into MET_LW_RAD
!                        and MET_SW_RAD.  (T. Otte)
!           05 May 2008  Added nudging coefficients, earth radius, and whether
!                        or not the urban canopy model was invoked (WRF only).
!                        (T. Otte)
!           22 Sep 2009  Changed MET_UCMCALL to MET_URBAN_PHYS, and allowed
!                        for variable to be set to be greater than 1.  Added
!                        variables MET_RICTR_DOT, MET_RJCTR_DOT, MET_CEN_LAT,
!                        MET_CEN_LON, MET_XXCTR, MET_YYCTR, and MET_REF_LAT.
!                        (T. Otte)
!           15 Dec 2010  Added MET_RAIN_BUCKET.  (T. Otte)
!           30 Aug 2011  Added MET_SHAL_CU.  Changed F77 character declarations
!                        to F90 standard.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           07 Dec 2011  Added MET_FDDA_GPH3D and changed descriptions of
!                        MET_FDDA_Gx3D variables.  (T. Otte)
!           21 Aug 2012  Added MET_PCP_INCR to accommodate WRFv3.2 option to
!                        output incremental (rather than accumulated)
!                        precipitation.  (T. Otte)
!           26 Nov 2014  Added variables for land use indices associated with
!                        ice, lake, and urban (MET_LU_ICE, MET_LU_LAKE, and
!                        MET_LU_URBAN).  (T. Spero)
!           22 Jun 2017  Added MET_HYBRID.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  ! Input data format.

  INTEGER           :: met_model       ! 1=MM5, 2=WRF
  INTEGER           :: met_iversion    ! MM5: 3=v3; WRF: 2=EM/ARW
  CHARACTER(LEN=8)  :: met_release     ! release version

  ! Grid information.

  REAL              :: met_cen_lat     ! center latitude of this domain
  REAL              :: met_cen_lon     ! center longitude of this domain
  REAL              :: met_cone_fac    ! cone factor
  INTEGER           :: met_gratio      ! grid ratio w.r.t. coarse dom
  INTEGER           :: met_mapproj     ! map proj: 1-lamcon, 2-pol, 3-utm
  INTEGER           :: met_ns          ! number of soil layers
  INTEGER           :: met_nx          ! this domain X (E-W) dimension
  INTEGER           :: met_nxcoarse    ! coarse dom X (E-W) dimension
  INTEGER           :: met_ny          ! this domain Y (N-S) dimension
  INTEGER           :: met_nycoarse    ! coarse dom Y (N-S) dimension
  INTEGER           :: met_nz          ! number of half-sigma layers
  REAL              :: met_p_alp_d     ! projection alpha
  REAL              :: met_p_bet_d     ! projection beta
  REAL              :: met_p_gam_d     ! projection gamma
  REAL              :: met_proj_clat   ! projection center latitude [degrees]
  REAL              :: met_proj_clon   ! projection center longitude [degrees]
  REAL              :: met_ptop        ! model top [Pa]
  REAL              :: met_ref_lat     ! ref latitude for Lam con [degrees]
  REAL              :: met_resoln      ! horiz grid spacing [m]
  REAL              :: met_rictr_dot   ! I coord of ctr point in dot-point-space
  REAL              :: met_rjctr_dot   ! J coord of ctr point in dot-point-space
  REAL              :: met_tapfrq      ! output interval [min]
  REAL              :: met_tru1        ! true latitude 1 [degrees]
  REAL              :: met_tru2        ! true latitude 2 [degrees]
  REAL              :: met_x_11        ! coarse dom loc of this dom's x=1
  REAL              :: met_x_centd     ! reference center longitude
  REAL              :: met_xxctr       ! distance [m] from origin to center in X
  REAL              :: met_y_11        ! coarse dom loc of this dom's y=1
  REAL              :: met_y_centd     ! reference center latitude
  REAL              :: met_yyctr       ! distance [m] from origin to center in Y

  ! Non-hydrostatic base state variables.

  REAL              :: met_p00         ! non-hyd ref pressure [Pa]
  REAL              :: met_tiso        ! non-hyd ref isothermal temp [K]
  REAL              :: met_tlp         ! non-hyd ref lapse [K/500 hPa]
  REAL              :: met_ts0         ! non-hyd ref sfc temp [K]

  ! Physics options and other user input.

  INTEGER           :: met_cumulus     ! cumulus parameterization scheme
  INTEGER           :: met_expl_moist  ! explicit moist physics scheme
  INTEGER           :: met_hybrid      ! hybrid vertical coordinate in met?
  INTEGER           :: met_lu_ice      ! ice index in land use
  INTEGER           :: met_lu_lake     ! lake index in land use
  INTEGER           :: met_lu_urban    ! urban index in land use
  INTEGER           :: met_lu_water    ! water index in land use
  CHARACTER(LEN=19) :: met_lu_src      ! source of land use categorization
  INTEGER           :: met_lw_rad      ! longwave radiation scheme
  INTEGER           :: met_pbl         ! PBL scheme
  INTEGER           :: met_pcp_incr    ! Time increment [min] for precip
  REAL              :: met_rain_bucket ! Tipping bucket for precipitation
  INTEGER           :: met_sfc_lay     ! surface layer scheme
  INTEGER           :: met_shal_cu     ! shallow convection option
  INTEGER           :: met_soil_lsm    ! surface/soil scheme or LSM
  INTEGER           :: met_snow_opt    ! snow option
  INTEGER           :: met_sw_rad      ! shortwave radiation scheme
  INTEGER           :: met_urban_phys  ! urban canopy model (WRF only)

  ! FDDA options.

  INTEGER           :: met_fdda_3dan   ! 3d nudging?  (0=no, 1=GRID, 2=SPEC)
  INTEGER           :: met_fdda_obs    ! obs nudging?  (0=no, 1=yes)
  INTEGER           :: met_fdda_sfan   ! sfc analysis nudging?  (0=no, 1=GRID)

  REAL              :: met_fdda_gv3d   ! 3d nudging coeff. for wind
  REAL              :: met_fdda_gt3d   ! 3d nudging coeff. for temperature
  REAL              :: met_fdda_gq3d   ! 3d nudging coeff. for moisture
  REAL              :: met_fdda_gph3d  ! 3d nudging coeff. for geopotential

  REAL              :: met_fdda_gvsfc  ! sfc analysis nudging coeff. for wind
  REAL              :: met_fdda_gtsfc  ! sfc analysis nudging coeff. for temp.
  REAL              :: met_fdda_gqsfc  ! sfc analysis nudging coeff. for mois.

  REAL              :: met_fdda_giv    ! obs nudging coeff. for wind
  REAL              :: met_fdda_git    ! obs nudging coeff. for temp.
  REAL              :: met_fdda_giq    ! obs nudging coeff. for mois.

  ! Date and time information.

  INTEGER           :: met_restart     ! "restarted" run?  (1=yes, 0=no)
  INTEGER           :: met_season      ! 1=summer, 2=winter
  CHARACTER(LEN=24) :: met_startdate   ! YYYY-MM-DD-HH:MM:SS.SSSS

END MODULE metinfo
