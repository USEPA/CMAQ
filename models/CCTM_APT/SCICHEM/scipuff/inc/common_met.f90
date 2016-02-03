module common_met

!******************************************************************************
!Feb 2005: Added aerodynamic resistance as a variable, PK, AER
!Apr 2005: Additional modifications for CMAQ-APT-PM, PK, AER
!Oct 2005: Calculate and store dry and wet dep separately, PK, AER
!May 2006: Updated for CMAQ v4.5, PK, AER
!May 2006: Add variables for extracting cloud fields for a grid cell
!          containing a puff, PK, AER
!Jul 2006: Updated to handle deposition by convective precipitation correctly
!          and to consider ratio of water in puff to total water in grid
!          column, PK, AER
!Aug 2007: Add new module common_mc_met for multiprocessor code, -PK, AER
!          based on modifications to stand-alone SCICHEM by BC, L3COM
!******************************************************************************

use met_param_inc
use param_inc
use stimet_inc
use common_metn
use common_mc_met
save

!------ met background common

!------ upper air met fields

!Horizontal met grids
real, dimension(:),allocatable :: xb, yb

!Vertical met grids (staggered)
! May 2006, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
real, dimension( : ),allocatable :: zb, zbw

!---- 3D met fields
real, dimension( : ),allocatable :: u_ua    !u-component at current time, t
real, dimension( : ),allocatable :: v_ua    !v-component
real, dimension( : ),allocatable :: w_ua    !w-component
real, dimension( : ),allocatable :: t_ua    !Potential temperature
real, dimension( : ),allocatable :: h_ua    !Humidity (g/g)
real, dimension( : ),allocatable :: p_ua    !Pressure
real, dimension(:,:),allocatable :: cld_ua  !Cloud liquid water content
real, dimension(:,:),allocatable :: cldt_ua !Cloud total water content
real, dimension(:,:),allocatable :: cldp_ua !Precipitation water content

! --- 2D fields for total water in grid column
real, dimension( : ),allocatable :: cldmasst !Mass of cloud water in column
real, dimension( : ),allocatable :: cldmassp !Mass of precip. water in column

!------ 1D turbulence profiles (BL=PROF)

real uu_ua(MAX1D)            !Shear-driven horizontal velocity variance
real vv_ua(MAX1D)            !Buoyancy-driven horizontal velocity variance
real ww_ua(MAX1D)            !Vertical velocity variance
real wt_ua(MAX1D)            !Vertical heat flux
real sl_ua(MAX1D)            !Horizontal length scale
real sz_ua(MAX1D)            !Vertical length scale
real qq_ua(MAX1D)            !TKE (*2) - computed
real aa_ua(MAX1D), bb_ua(MAX1D)  !Stability parameters - computed

!------ boundary layer / surface fields

real, dimension(:),allocatable :: xbl, ybl        !BL horizontal grid
real, dimension(:),allocatable :: lon_bl, lat_bl  !Lat/lon grid

real, dimension( : ),allocatable :: u_bl         !BL u-component
real, dimension( : ),allocatable :: v_bl         !BL v-component
real, dimension( : ),allocatable :: t_bl         !BL potential temperature
real, dimension( : ),allocatable :: zi_bl        !Mixing layer height
real, dimension( : ),allocatable :: hflx_bl      !Surface heat flux
real, dimension( : ),allocatable :: fcc_bl       !Cloud cover fraction
real, dimension( : ),allocatable :: fprcpc_bl    !fraction of convective precip.
real, dimension( : ),allocatable :: pgt_bl       !PGT class
real, dimension( : ),allocatable :: h_bl         !BL humidity
real, dimension(:,:),allocatable :: cld_bl       !Cloud water content
real, dimension( : ),allocatable :: prcp_bl      !Precipitation class
real, dimension( : ),allocatable :: prate_bl     !Precipitation rate
real, dimension( : ),allocatable :: p_bl         !BL pressure

real uu_bl(MAX3DB)                 !Shear-driven horizontal velocity variance
real vv_bl(MAX3DB)                 !Buoyancy-driven horizontal velocity variance
real ww_bl(MAX3DB)                 !Vertical velocity variance
real qq_bl(MAX3DB)                 !TKE (*2)
real sz_bl(MAX3DB)                 !Vertical length scale
real sl_bl(MAX3DB)                 !Horizontal length scale
real aa_bl(MAX3DB), bb_bl(MAX3DB)  !Stability parameters 

real, dimension( : ),allocatable :: z_sl             !Surface layer depth
real, dimension( : ),allocatable :: u_sl, v_sl       !Surface layer velocity
real, dimension( : ),allocatable :: ustr2            !u*-squared
real, dimension( : ),allocatable :: wstr2            !w*-squared
real, dimension( : ),allocatable :: xmol2            !Monin-obukhov length
real, dimension( : ),allocatable :: dtdz2            !Surface temperature gradient
real, dimension( : ),allocatable :: zruf2            !Surface roughness
real, dimension(:,:),allocatable :: vdep2d           !Deposition velocities (by species)
real, dimension( : ),allocatable :: sr, ss           !Sunrise/sunset times
real, dimension( : ),allocatable :: radyn2           !Aerodynamic resistance
real, dimension( : ),allocatable :: lwc2             !Avg. cloud lwc
real, dimension( : ),allocatable :: cldbot2, cldtop2 !Cloud base and top

real cldbot,cldtop      !current vals for photolysis routine

real xmol_pgt(7)        !PGT lookup table for M-O lengths
real size_cld(MAX_CLD)  !Cloud size bins

!------ ensemble turbulence fields (large-scale variability and met uncertainty)

!Grid definitions
real, dimension(:),allocatable :: xbe, ybe
! May 2006, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
real, dimension(:),allocatable :: zbe

real xbec(MAXXC),  ybec(MAXXC),  zbec(MAXZC)  ! Not used 

real, dimension(:),allocatable :: uue_ua      !LSV u-component variance
real, dimension(:),allocatable :: vve_ua      !LSV v-component variance
real, dimension(:),allocatable :: uve_ua      !LSV uv-correlation
real, dimension(:),allocatable :: sle_ua      !LSV length scale

!BL values
real, dimension(:),allocatable :: uue_bl
real, dimension(:),allocatable :: vve_bl
real, dimension(:),allocatable :: uve_bl
real, dimension(:),allocatable :: sle_bl

!real, dimension(:),allocatable :: de     !Not used

real uu_climo(MAX3C), vv_climo(MAX3C), uv_climo(MAX3C), sl_climo(MAX3C) !Not used

!Latitude-dependent Gifford spectrum
real, dimension(:),allocatable :: uu_lsv, sl_lsv

real sl_haz        !Not used

type ( sTimeT ) time_anlys    !Not used

!------ terrain fields

real, dimension(:),allocatable :: hs        !Terrain heights
real, dimension(:),allocatable :: d         !Terrain d = 1 - hs/H
real, dimension(:),allocatable :: ddx, ddy  !Terrain slopes
real, dimension(:),allocatable :: d1, d2    !Staggered grid values of d

! May 2006, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
real, dimension(:),allocatable :: zz               !Vertical grid function

!------ logical flags

logical lua        !Upper air data available
logical lbl        !BL present
logical lensm      !LSV on
logical wflag      !Vertical velocity available
logical lsfc 
logical tflag      !Temperature data available
logical hflag      !Humidity data available
logical cldflag    !Cloud data available

logical lfirst                      !Flag for first time through
logical lfound                      !Local - not used
logical lter_prj, lzi_prj, lswift_prj  !Flags for restart from old project
logical lpgt                        !Heat flux from input PGT class
logical lzi                         !Zi from input data
logical lhflx                       !Heat flux from input data
logical lfcc                        !Cloud cover from met  input
logical lprcp                       !Precip class from met input
logical lprate                      !Precip rate from met input
logical update_ua, update_bl        !Update upper air and sfc fields
logical update_swift, next_swift    !Not used
logical lmc_ua                      !Mass consistency flag
logical lmc_sfc                     !Not used
logical l2d_sfc                     !Not used
logical l3d_obs                     !Multiple obs - need horizontal interpolation
logical lswift                      !Not used
logical local_met                   !Met time  is local
logical lbl_from_ua                 !Not used
logical lout_met, lout_2d, lout_3d  !Met output flags
logical lstagger                    !Velocities on staggered grid
logical lformat                     !Medoc file ASCII flag
logical l3dclimo                    !Not used
logical lsv_oper                    !Use operational LSV
logical lhaz_obs, lhaz_sfc          !Not used

logical do_ua,  do_ua_bl,  do_ua_prcp, do_ua_prate
logical do_sfc, do_sfc_bl, do_sfc_prcp, do_sfc_prate
logical lset_sfc, lset_ua, lzruf2d
logical do_ua_fcc, do_sfc_fcc, lprcap

!------ character variables

character*80 met_type, bl_type, ensm_type   !MSC namelist input strings
character*80 pr_type                        !Precip type input string

!------ miscellaneous real/integer variables

real  tmet_offset, dt_swift, tbin_met, tout_met
real  time_ua, time_bl, time_fc, time_ensm
real  time_met, time_met_old, time_swift, time_swift_old
real  time_ua_old, time_ua_last, time_bl_old, time_bl_last
real  time_eua, time_eua_last, time_ebl, time_ebl_last

real  dxb, dyb, dzb        !Met grid sizes
real  dxbl, dybl, dzbl     !BL grid sizes
real  dxe, dye             !LSV grid
real  dxec, dyec           !Not used

real  fixed_spd,fixed_dir                              !Fixed wind params
real  albedo, bowen, cloud_cover                       !Landuse params
real  zimin, zimax, hconst, hdiur, hconst_c, hdiur_c   !Simple BL params

real  hmin, zbtop                         !Min terrain height and top of grid
real  zref                                !Reference height for surface wind data
real  uu_calm, sl_calm                    !Min turbulence under light winds
real  uu_ensm, sl_ensm                    !LSV inputs

!---- GET_MET return values

real  ub, vb, wb                              !Velocity at get_met point
real  clwc(MAX_CLD)                           !Cloud lwc at get_met point
real  uub, vvb, uvb, uubl, vvbl, wwbl, wwbh   !Turbulence components
real  wtbl                                    !Heat flux

real  uu_haz, vv_haz, uv_haz              !Not used
real  uuz_haz, vvz_haz, uvz_haz, sb_haz   !Not used
real  sb_lsv                              !LSV length scale
real  qqs, qqb, qqsh, qql                 !Total energy in each component
real  uubz, vvbz, uvbz                    !Vertical gradients of LSV
real  sby, sbl, sbz, sbls                 !Length scales - L,B, vertical and S
real  dudx, dvdy, dudy, dvdx, dwdx, dwdy  !horizontal velocity gradients
real  dudz, dvdz, dwdz                    !vertical velocity gradients
real  dtdz, dhdz                          !temp and humidity gradients
real  difb, dddz                          !Equilibrium diffusivity and gradient
real  zinv                                !Mixing layer height
real  dtdzs                               !Surface dtdz

real  xbl_cnv, ybl_cnv, xbl_off, ybl_off
real  tprcp, tprcpn
real  xmin_met, xmax_met, ymin_met, ymax_met

real  ua_fac, age_fac_ua, sfc_fac, age_fac_sfc  !Interpolation factors

integer n_obs_min, n_sfc_min      !Min obs for usable input to interpolation (should be 1)
integer n_obs_max, n_sfc_max      !Max obs used in interpolation

integer nxb,  nyb,  nzb           !Met grid dimensions
integer nxbl, nybl, nzbl, nxybl   !BL grid dimensions
integer nxe,  nye,  nzbe, nxye    !LSV grid dimensions
integer nxec, nyec, nzec, nxyec   !Not used

integer iday_met, jul_met, yr_met, nday_yr

integer unit_spd                  !Wind speed units flag

integer obs_flag, obs_type, nout_met, ncld

real, dimension(:,:),allocatable :: ddepos2d  !Dry deposition array
real, dimension(:,:),allocatable :: wdepos2d  !Wet deposition array

real x0m, y0m             !Origin of grid (lower left)
real xmaxm,ymaxm,zmaxm    !max x,y and z grid points

end module common_met

