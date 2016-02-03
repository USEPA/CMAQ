!*******************************************************************************
!$RCSfile: set_bl_from_ua.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine set_bl_from_ua
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Set bl grids using ua grids if no sfc obs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 bl_grid
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- LOCALS

integer i, nzbl_max

nxbl  = nxb
nybl  = nyb
nxybl = nxyb
dxbl  = dxb
dybl  = dyb

do i = 1,nxbl
  xbl(i) = xb(i)
end do

do i = 1,nybl
  ybl(i) = yb(i)
end do

zref = zb(1)

if (bl_type == 'CALC' .or. multicomp .or. &
   (bl_type == 'OPER' .and. &
   .not.(lpgt .or. (lzi .and. lhflx))) ) then
  call bl_grid
  if (nError /= NO_ERROR) go to 9999
end if

if (lbl) then

  nzbl_max = MAX3DB / nxyb
  if (nzbl_max < nzbl) then
    write(lun_log,*) 'BL grid too big; reducing vertical resolution'
    write(lun_log,*) 'nzbl (old,new) = ',nzbl,nzbl_max
    nzbl = nzbl_max
  end if

  if (nzbl <= 2) then
    nError   = SZ_ERROR
    eRoutine = 'set_bl_from_ua'
    eMessage = 'Met grid size too big for boundary layer arrays'
    write(eInform,'(a,i5,a)') 'Only have ',nzbl, &
                                      'levels in boundary layer'
    go to 9999
  end if

  if (bl_type /= 'PROF') then
    dzbl = 1.0/float(nzbl-1)
  end if

end if

9999    continue

return

end


subroutine bl_from_ua
!*******************************************************************************
!
! FUNCTION:  Set bl arrays using lowest level of ua arrays
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_met

implicit none
 
! --- LOCALS

integer i, j, i0, ix, icld

if (lstagger) then
  do i = 1,nxybl
    u_bl(i) = u_ua(i+nxyb)
    v_bl(i) = v_ua(i+nxyb)
    t_bl(i) = t_ua(i+nxyb)
    p_bl(i) = p_ua(i+nxyb)
    h_bl(i) = h_ua(i+nxyb)
    do icld = 1, ncld
      cld_bl(i,icld) = cld_ua(i+nxyb,icld)
    end do
  end do
else
  do i = 1,nxybl
    u_bl(i) = u_ua(i)
    v_bl(i) = v_ua(i)
    t_bl(i) = t_ua(i)
    p_bl(i) = p_ua(i)
    h_bl(i) = h_ua(i)
    do icld = 1, ncld
      cld_bl(i,icld) = cld_ua(i,icld)
    end do
  end do
end if

if (ensm_type == 'OBS' .or. lhaz_obs) then
  do j = 1,nye
    i0 = (j-1)*nxe
    do ix = 1,nxe
      i = i0 + ix
      uue_bl(i) = uue_ua(i)
      vve_bl(i) = vve_ua(i)
      uve_bl(i) = uve_ua(i)
      sle_bl(i) = sle_ua(i)
    end do
  end do
end if

return

end

subroutine bl_grid
!*******************************************************************************
!
! FUNCTION:  Define lat/lon grid for BL_TYPE=CALC
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 map_loc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- LOCALS

real dxx, dyy

integer i


if (lmap == I_LATLON) then

  do i = 1,nxbl
    lon_bl(i) = xbl(i)
  end do

  do i = 1,nybl
    lat_bl(i) = ybl(i)
  end do

else

  call map_loc(lmap,xbl(1),ybl(1), &
                             I_LATLON,lon_bl(1),lat_bl(1),dxx,dyy)
  if (nError /= NO_ERROR) then
    nError   = UK_ERROR
    eRoutine = 'bl_grid'
    eMessage = 'Must set map reference location'
    eInform  = 'Run is Cartesian; BL_TYPE=CALC requires Lat/Lon'
    go to 9999
  end if

  dxx = dxbl*dxx
  dyy = dybl*dyy

  do i = 2,nxbl
    lon_bl(i) = lon_bl(i-1) + dxx
  end do

  do i = 2,nybl
    lat_bl(i) = lat_bl(i-1) + dyy
  end do

end if

9999    continue

return

end


