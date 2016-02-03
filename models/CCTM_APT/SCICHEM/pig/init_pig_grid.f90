subroutine init_pig_grid
!******************************************************************************
! Developed by Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Initialize SCICHEM horizontal grid parameters based on CMAQ
!            grid and allocate dynamic arrays
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: Version 1.0, January 2004 (PKK, AER)
!                   Apr 2005: Updated for CMAQ-APT-PM (PKK, AER)
!                   Oct 2005: Updated for MADRID-Hg (PKK, AER)
!                   Nov 2005: Updated for CMAQ v4.5 (PKK, AER)
!                   May 2006: Updated for MADRID-Hg (PKK, AER)
!                   Jun 2006: Compile flag to activate/deactivate Hg (PKK, AER)
!                   Jul 2006: Increased dimension of zw3d and added more
!                             cloud variables, PKK, AER
!                   Aug 2007: Used pname for routine name, PKK, AER
!                   Feb 2015: Bug-fix for uu_lsv and sl_lsv array dimensions,
!                             PKK, ENVIRON
!******************************************************************************

! --- MODULES

! November 2005, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
use grid_conf             ! horizontal and vertical domain specifications

use error_inc
use common_met
use common_grd
use diagnostics

implicit none

integer :: ios

character( 16 ) :: pname = 'init_pig_grid'

!.......   Horizontal Grid

MAXXB = GL_NCOLS
MAXYB = GL_NROWS

! November 2005, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
!.......   Vertical grid
MAXZB  = NLAYS     ! max z-grid size (ua)
MAXZBP = MAXZB + 1 ! max z-grid size plus 1 (ua)

! November 2005, PK, AER: Check MAXZB vs MAXZP
if (MAXZB > MAXZP) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'No. of vertical layers in CMAQ > allocated for plume rise calc'
  write(eAction,*) 'Increase MAXZP in met_param_pig_inc to ',MAXZB
  go to 9999
end if
      
MAX3D = MAXXB*MAXYB*MAXZBP ! max 3d met field size
MAX2D = MAXXB*MAXYB        ! max 2d met field size

MAXGRD = 4*MAX2D             !max total horizontal input field dimension

! --- Allocate 1-D horizontal arrays
allocate(xb(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate XB array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(yb(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate YB array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if
allocate(xbl(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate XBL array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(ybl(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate YBL array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if
allocate(lon_bl(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate LON_BL array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(lat_bl(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate LAT_BL array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if
allocate(sr(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate SR array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(ss(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate SS array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(xbe(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate XBE array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(ybe(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate YBE array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if
allocate(uu_lsv(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate UU_LSV array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if
allocate(sl_lsv(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate SL_LSV array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if
allocate(xbs(MAXXB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate XBS array'
  write(eInform,*) 'bytes requested =',MAXXB*4
  go to 9999
end if
allocate(ybs(MAXYB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate YBS array'
  write(eInform,*) 'bytes requested =',MAXYB*4
  go to 9999
end if

! November 2005, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
! --- Allocate 1-D vertical arrays
allocate(zb(MAXZB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZB array'
  write(eInform,*) 'bytes requested =',MAXZB*4
  go to 9999
end if
allocate(zbw(MAXZB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZBW array'
  write(eInform,*) 'bytes requested =',MAXZB*4
  go to 9999
end if
allocate(zbe(MAXZB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZBE array'
  write(eInform,*) 'bytes requested =',MAXZB*4
  go to 9999
end if
allocate(zz(MAXZBP), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZZ array'
  write(eInform,*) 'bytes requested =',MAXZBP*4
  go to 9999
end if

! --- Allocate 2-D arrays
allocate(xmap2d(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate XMAP2D array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(ymap2d(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate YMAP2D array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(lat2d(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate LAT2D array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(lon2d(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate LON2D array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if

allocate(u_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate U_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(v_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate V_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(t_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate T_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(zi_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZI_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(hflx_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate HFLX_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(fcc_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate FCC_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(pgt_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate PGT_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(h_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate H_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(prcp_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate PRCP_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(prate_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate PRATE_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(cldmasst(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLDMASST array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(cldmassp(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLDMASSP array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(fprcpc_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate FPRCPC_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(p_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate P_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(z_sl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate Z_SL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(u_sl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate U_SL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(v_sl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate V_SL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(ustr2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate USTR2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(wstr2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate WSTR2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(xmol2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate XMOL2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(dtdz2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate DTDZ2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(zruf2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZRUF2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(cldbot2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLDBOT2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(cldtop2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLDTOP2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(lwc2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate LWC2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(radyn2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate RADYN2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(uue_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate UUE_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(vve_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate VVE_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(uve_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate UVE_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(sle_bl(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate SLE_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(hs(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate HS array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(d(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate D array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(ddx(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate DDX array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(ddy(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate DDY array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(d1(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate D1 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(d2(MAX2D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate D2 array'
  write(eInform,*) 'bytes requested =',MAX2D*4
  go to 9999
end if
allocate(wrk(MAXGRD), stat = ios)
  if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate WRK array'
  write(eInform,*) 'bytes requested =',MAXGRD*4
  go to 9999
end if

! --- Allocate 3-D arrays
allocate(zb3d(MAXXB,MAXYB,MAXZB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZB3D array'
  write(eInform,*) 'bytes requested =',MAX2D*MAXZB*4
  go to 9999
end if
allocate(z3d(MAXXB,MAXYB,MAXZB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate Z3D array'
  write(eInform,*) 'bytes requested =',MAX2D*MAXZB*4
  go to 9999
end if
allocate(zw3d(MAXXB,MAXYB,MAXZBP), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate ZW3D array'
  write(eInform,*) 'bytes requested =',MAX2D*MAXZBP*4
  go to 9999
end if
allocate(cdump(MAXXB,MAXYB,MAXZB), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CDUMP array'
  write(eInform,*) 'bytes requested =',MAX2D*MAXZB*4
  go to 9999
end if
allocate(u_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate U_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(v_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate V_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(w_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate W_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(t_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate T_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(h_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate H_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(p_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate P_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(uue_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate UUE_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(vve_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate VVE_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(uve_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate UVE_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(sle_ua(MAX3D), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate SLE_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*4
  go to 9999
end if
allocate(cld_bl(MAX2D,MAX_CLD), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLD_BL array'
  write(eInform,*) 'bytes requested =',MAX2D*MAX_CLD*4
  go to 9999
end if
allocate(vdep2d(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate VDEP2D array'
  write(eInform,*) 'bytes requested =',MAX2D*MAX_MC*4
  go to 9999
end if
allocate(ddepos2d(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate DDEPOS2D array'
  write(eInform,*) 'bytes requested =',MAX2D*MAX_MC*4
  go to 9999
end if
allocate(wdepos2d(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate WDEPOS2D array'
  write(eInform,*) 'bytes requested =',MAX2D*MAX_MC*4
  go to 9999
end if

! --- Allocate 4-D arrays
allocate(cld_ua(MAX3D,MAX_CLD), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLD_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*MAX_CLD*4
  go to 9999
end if
allocate(cldt_ua(MAX3D,MAX_CLD), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLDT_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*MAX_CLD*4
  go to 9999
end if
allocate(cldp_ua(MAX3D,MAX_CLD), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate CLDP_UA array'
  write(eInform,*) 'bytes requested =',MAX3D*MAX_CLD*4
  go to 9999
end if
allocate(wrk1(MAXGRD,N2), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = pname
  eMessage = 'Insufficient memory to allocate WRK1 array'
  write(eInform,*) 'bytes requested =',MAXGRD*N2*4
  go to 9999
end if

9999  return
end
