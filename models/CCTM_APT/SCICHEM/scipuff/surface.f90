!*******************************************************************************
!$RCSfile: surface.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine create_surface
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Initialize surface files
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!        write_srf_totnam               create_it             GetMultiNum
!           output_groups              IsMultiDep              IsMultiDos
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use srfparam_inc
use files_inc

implicit none

! --- LOCALS

integer ityp, jtyp, nvs, i, j, k, nsg, output_groups
integer ipg, ipd, ipn, nspecies, GetMultiNum, nhaz, ihaz

character*1  namx(3)
character*4  ctmp

logical IsMultiDep, IsMultiDos

!------ - NB dezone_lev relies on this order
!       (specifically - must have M,V,S and D,X consecutively)

namx(1) = 'M'
namx(2) = 'V'
namx(3) = 'S'

nspecies = GetMultiNum()

if (hazard == IHAZ_COMB) then
  nhaz = 2
else
  nhaz = 1
end if

!------ Setup deposition file

ipg = 1
ipd = 0
ipn = 0
if(surface)then
  srfdep%nvar = -999

  srfdep%ipgrd = ipg
  srfdep%ipdat = ipd
  srfdep%ipnam = ipn

  nvs = srfdep%ipnam                                    !was 0
  do ityp = 1,ntypm
    nsg = output_groups(material(ityp),mat_aux)
    if(material(ityp)%ioffs /= NOT_SET_I)then
      jtyp = material(ityp)%ioffp

      do ihaz = 1,nhaz          !set up for combined run

        if(material(ityp)%lsrfg)then
          do i = 1,nsg
            jtyp = jtyp + 1
            do j = 1,NV_BASIC
              nvs = nvs + 1
              write(ctmp,1000)namx(j),jtyp
              srfnam(nvs) = ctmp
            end do
          end do
        end if
        if(material(ityp)%lsrft)then
          do j = 1,NV_BASIC
            nvs = nvs + 1
            call write_srf_totnam(namx(j),ityp,ihaz,ctmp)
            srfnam(nvs) = ctmp
          end do
        end if
      end do            ! end loop for hazard combination
    end if
  end do

  do ityp = 1,ntypm
    if(material(ityp)%ioffs_mc /= NOT_SET_I)then
      do k = 1,nspecies
        if(IsMultiDep(k))then
          do j = 1,NV_BASIC
            nvs = nvs + 1
            write(ctmp,1002)namx(j),char(64+ityp),k
            srfnam(nvs) = ctmp
          end do
        end if
      end do
    end if
  end do
  if(nvs <= 0)then
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'No deposition variables to output'
    go to 9999
  end if
  srfdep%nvart  = nvs - srfdep%ipnam
  srfdep%ncells = 0
  srfdep%record = 0
  call create_it( lun_ddp,file_ddp,srfdep )
  if (nError /= NO_ERROR) go to 9999
  ipg = ipg + 1
  ipd = ipd + srfdep%nvart
  ipn = ipn + srfdep%nvart
end if

!------ Setup dose file

if(dose)then
  srfdos%nvar  = -999

  srfdos%ipgrd = ipg
  srfdos%ipdat = ipd
  srfdos%ipnam = ipn

  nvs = srfdos%ipnam                                    !was 0
  do ityp = 1,ntypm
    nsg = output_groups(material(ityp),mat_aux)
    if(material(ityp)%ioffd /= NOT_SET_I)then
      jtyp = material(ityp)%ioffp

      do ihaz = 1,nhaz

        if(material(ityp)%ldosg)then
          do i = 1,nsg
            jtyp = jtyp + 1
            do j = 1,NV_BASIC
              nvs = nvs + 1
              write(ctmp,1000)namx(j),jtyp
              srfnam(nvs) = ctmp
            end do
          end do
        end if
        if(material(ityp)%ldost)then
          do j = 1,NV_BASIC
            nvs = nvs + 1
            call write_srf_totnam(namx(j),ityp,ihaz,ctmp)
            srfnam(nvs) = ctmp
          end do
        end if
      end do
    end if
  end do
  do ityp = 1,ntypm
    if(material(ityp)%ioffd_mc /= NOT_SET_I)then
      do k = 1,nspecies
        if(IsMultiDos(k))then
          do j = 1,NV_BASIC
            nvs = nvs + 1
            write(ctmp,1002)namx(j),char(64+ityp),k
            srfnam(nvs) = ctmp
          end do
        end if
      end do
    end if
  end do
  if(nvs <= 0)then
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'No dose variables to output'
    go to 9999
  end if
  srfdos%nvart  = nvs - srfdos%ipnam
  srfdos%record = 0
  call create_it( lun_dos,file_dos,srfdos )
  if (nError /= NO_ERROR) go to 9999
  ipg = ipg + 1
  ipd = ipd + srfdos%nvart
  ipn = ipn + srfdos%nvart
end if

9999    continue

return

!------ formats

1000    format(a,i3.3)
1002    format(a,a,i2.2)

end


subroutine write_srf_totnam(name,ityp,ihaz,ctmp)
!*******************************************************************************
!
! FUNCTION:    Create surface variable names
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- ARGUMENTS
 
character*(*) name,ctmp !Partial name and full name

integer ityp, ihaz      !Material type and hazard flag

if (ihaz == 1) then
  write(ctmp,1001)name,ityp,'T'
else
  write(ctmp,1001)name,ityp,'H'
end if

1001    format(a,i2.2,a)

return
end


subroutine create_it(lun, file, srf)
!*******************************************************************************
!
! FUNCTION:    Create surface file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf

implicit none

! --- ARGUMENTS
 
type ( grid_str ) srf  !Surface structure
integer lun            !Surface file unit number
character*128 file     !Surface file name

! --- PARAMETERS
 
real       FAST_FRAC
parameter (FAST_FRAC = 0.006) !normally have 10 cells -> limits to 4 levels

! --- LOCALS
integer ios, nxs, nys, ngrds, nvs, i, ivar, mlev

real tx, dxs, dys, xmap, ymap, xbar, ybar


!------ Create surface file

srf%nunit = lun
open(unit=lun, file=file, status='NEW', access='DIRECT', &
          recl=512, iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'create_it'
  eMessage = 'Error opening SCIPUFF surface output file'
  eInform  = 'File='//TRIM(file)
  eAction  = 'Make sure file does not already exist'
  go to 9999
end if

!------ Write initial header

tx    = 0.0
ngrds = 0
dxs   = float(nx)*dxg
dys   = float(ny)*dyg
if(dxs > dys)then
  nxs = 10
  nys = max0(1,nint(float(nxs)*dys/dxs))
else
  nys = 10
  nxs = max0(1,nint(float(nys)*dxs/dys))
end if
dxs = dxs/float(nxs)
dys = dys/float(nys)
if (delmin == DEF_VAL_R) then
  if(run_mode == FAST_MODE)then
    xbar = xmin + 0.5*(nxs-1)*dxs
    ybar = ymin + 0.5*(nys-1)*dys
    call mapfac( xbar , ybar , xmap , ymap )
    mlev = max0(int(alog(FAST_FRAC*amin0(nxs,nys))/alog(0.5)),0)
  else
    mlev = 99
  end if
else if (delmin > 0.) then
  xbar = xmin + 0.5*(nxs-1)*dxs
  ybar = ymin + 0.5*(nys-1)*dys
  call mapfac( xbar , ybar , xmap , ymap )
  mlev = max0(int(alog(delmin/max(dxs/xmap,dys/ymap))/alog(0.5)),0)
else
  mlev = 99
end if
mlev = -mlev
nvs  = srf%nvart
write(lun,rec=1,iostat=ios) tx, ngrds, nxs, nys, xmin, ymin, &
      dxs, dys, nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversion, mlev
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'create_it'
  eMessage = 'Error writing SCIPUFF surface output file'
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

!-----  Initialize adaptive grid

ngrds = nxs*nys

do i = 1,ngrds
  srfgrd(i,srf%ipgrd) = 0
  do ivar = 1,nvs
    srfdat(i,srf%ipdat+ivar) = 0.0
  end do
end do

srf%record = 1
srf%ncells = ngrds
srf%nx     = nxs
srf%ny     = nys
srf%xmin   = xmin
srf%ymin   = ymin
srf%dx     = dxs
srf%dy     = dys
srf%maxlev = mlev
if(delmin == DEF_VAL_R)then
  if(run_mode == FAST_MODE)then
    srf%delmin = min(srf%dx/xmap,srf%dy/ymap)*(0.5**iabs(mlev))
  else
    srf%delmin = 0.
  end if
else if(delmin == 0.)then
  srf%delmin = 0.
else
  srf%delmin = min(srf%dx/xmap,srf%dy/ymap)*(0.5**iabs(mlev))
end if

9999    continue

return

end


subroutine read_surface(lun, file, srf)
!*******************************************************************************
!
! FUNCTION:  Read the surface file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac          WarningMessage
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf

implicit none

! --- ARGUMENTS
 
type ( grid_str ) srf  !Surface structure
integer lun            !Surface file unit number
character*128 file     !Surface file name

! --- LOCALS

integer ios, ir, iro, nskip, i, j, ii, ivar
integer nxs, nys, nvs, iversionx, ngrds, irecs
integer j1, nvx, mlev

real tx, dxs, dys, xmins, ymins, xmap, ymap, xbar, ybar


!------ Open surface file

srf%nunit = lun
open(unit=lun, file=file, status='OLD', access='DIRECT', &
          recl=512, iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'read_surface'
  eMessage = 'Error opening SCIPUFF surface output file'
  eInform  = 'File='//TRIM(file)
  eAction  = 'Make sure file does not already exist'
  go to 9999
end if

!------ Read last time break

ir  = 1
iro = 1
read(lun,rec=ir,err=9998) tx, ngrds, nxs, nys, xmins, ymins, &
       dxs, dys,nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversionx

do while (ngrds > 0)

  nskip = ((ngrds-1)/128 + 1)*(nvs+1) + 1
  iro   = ir
  ir    = ir + nskip
  read(lun,rec=ir,err=9998) tx, ngrds, nxs, nys, xmins, ymins, &
         dxs, dys, nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversionx

end do

read(lun,rec=iro,err=9998) tx, ngrds, nxs, nys, xmins, ymins, &
       dxs, dys, nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversionx

xbar = xmins + 0.5*(nxs-1)*dxs
ybar = ymins + 0.5*(nys-1)*dys
call mapfac( xbar , ybar , xmap , ymap )

if (iversionx >= 650) then
  read(lun,rec=iro,err=9998) tx, ngrds, nxs, nys, xmins, ymins, &
        dxs, dys, nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversionx, mlev
else
  if (delmin == DEF_VAL_R) then
    mlev = 99
  else if (delmin > 0.) then
    mlev = max0(int(alog(delmin/max(dxs/xmap,dys/ymap))/alog(0.5)),0)
  else
    mlev = 99
  end if
  mlev = -mlev
end if

irecs = ir

ir = iro + 1
if (ngrds > 0) then
  do ii = 1,ngrds,128
    j1 = min0(ii+127,ngrds)
    read(lun,rec=ir,err=9998) (srfgrd(j,srf%ipgrd), j=ii,j1)
    ir = ir + 1
  end do
  do ivar = 1,nvs
    do ii = 1,ngrds,128
      j1 = min0(ii+127,ngrds)
      read(lun,rec=ir,err=9998) (srfdat(j,srf%ipdat+ivar), j=ii,j1)
      ir = ir + 1
    end do
  end do
else
  ngrds = nxs*nys
  do i = 1,ngrds
    srfgrd(i,srf%ipgrd) = 0
    do ivar = 1,nvs
      srfdat(i,srf%ipdat+ivar) = 0.0
    end do
  end do
end if

!------ Get variable parameters

nvx = -999

!------ check grid parameters

!       if(xminx /= xmin .or. yminx /= ymin)then
!         nError   = UK_ERROR
!         eRoutine = 'read_surface'
!         eMessage = 'Incompatible domain on SCIPUFF surface output file'
!         eInform  = 'File='//TRIM(file)
!         go to 9999
!       end if

if(iversionx /= iversion)then
  nError   = WN_ERROR
  eRoutine = 'read_surface'
  eMessage = 'Different SCIPUFF version number on surface file'
  eInform  = 'File='//TRIM(file)
  call WarningMessage(0,.true.)
  if (nError /= NO_ERROR) go to 9999
end if

!------ Set grid structures

srf%record = irecs
srf%nvart  = nvs
srf%nvar   = nvx
srf%ncells = ngrds
srf%nx     = nxs
srf%ny     = nys
srf%xmin   = xmins
srf%ymin   = ymins
srf%dx     = dxs
srf%dy     = dys
srf%maxlev = mlev
if(mlev == -99)then
  srf%delmin = 0.
else
  srf%delmin = min(srf%dx/xmap,srf%dy/ymap)*(0.5**iabs(mlev))
end if

9999    continue

return

!------ set read error and go to return

9998    continue
nError   = RD_ERROR
eRoutine = 'read_surface'
eMessage = 'Error reading SCIPUFF surface output file'
eInform  = 'File='//TRIM(file)
go to 9999

end


subroutine close_surface(srf)
!*******************************************************************************
!
! FUNCTION:  Close the surface file
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
 
use surface_inc

implicit none

! --- ARGUMENTS
 
type ( grid_str ) srf  !Surface structure

! --- LOCALS
integer ios


close(unit=srf%nunit,iostat=ios)

return

end


subroutine output_surface ( file, srf )
!*******************************************************************************
!
! FUNCTION:  Output the surface fields
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  dezone
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use files_inc

implicit none

! --- ARGUMENTS
 
character*128 file     !Surface file name
type ( grid_str ) srf  !Surface structure

! --- LOCALS

integer ncell, ir, nwrt, nvs, ngrds, nxs, nys
integer ivar, i, j, ii, j1, ntmp, ios, mlev

real tmp, dxs, dys, xmins, ymins

!------ dezone

ncell = srf%ncells
call dezone(srf)
if (ncell /= srf%ncells) then
  write(lun_log,600,iostat=ios) ncell-srf%ncells,srf%ncells
600       format('Deleted ',i5,' cells from surface file on output. ', &
                                             i5,' remaining.')
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'outpuf_surface'
    eMessage = 'Error writing SCIPUFF log file'
    eInform  = 'File='//TRIM(file_log)
    go to 9999
  end if
end if

!------ Output surface data

tmp  = t/3600.
ir    = srf%record
nwrt  = srf%nunit
nvs   = srf%nvart
ngrds = srf%ncells
nxs   = srf%nx
nys   = srf%ny
xmins = srf%xmin
ymins = srf%ymin
dxs   = srf%dx
dys   = srf%dy
mlev  = srf%maxlev

write(nwrt,rec=ir,err=9998) tmp, ngrds, nxs, nys, xmins, ymins, &
       dxs, dys, nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversion, mlev

if(ngrds <= 0)return

ir = ir + 1
do ii = 1,ngrds,128
  j1 = min0(ii+127,ngrds)
  write(nwrt,rec=ir,err=9998) (srfgrd(j,srf%ipgrd), j=ii,j1)
  ir = ir + 1
end do
do ivar = 1,nvs
  do ii = 1,ngrds,128
    j1 = min0(ii+127,ngrds)
    write(nwrt,rec=ir,err=9998) (srfdat(j,srf%ipdat+ivar), j=ii,j1)
    ir = ir + 1
  end do
end do

ntmp  = 0
srf%record = ir

write(nwrt,rec=ir,err=9998) tmp, ntmp, nxs, nys, xmins, ymins, &
       dxs, dys, nvs, (srfnam(srf%ipnam+i),i=1,nvs), iversion, mlev

9999    continue

return

!------ set read error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'output_surface'
eMessage = 'Error writing SCIPUFF surface output file'
eInform  = 'File='//TRIM(file)
go to 9999

end


subroutine surface_dose(ipuf,sdat,srf,zsrf,idep)
!*******************************************************************************
!
! FUNCTION:  Calculate the surface dose
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!        get_smooth_topog            puff_reflect                puff_dos
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use refl_inc
use srfparam_inc
use srfdos_inc

implicit none

! --- ARGUMENTS
 
type ( grid_str ) srf !Surface structure
integer ipuf          !Puff number
real    sdat(*)       !Surface data array
real    zsrf          !Dosage height
integer idep          !Dose or deposition flag

! --- LOCALS

logical lpuf
real    zs

!------ calculate cdep at grid locations and increment dose

nxs   = srf%nx
nys   = srf%ny
dxs   = srf%dx
dys   = srf%dy

lpuf = .false.
p    = puff(ipuf)

ityp = typeID(p%ityp)%imat
if(idep == 1)then
  ioff = max0(material(ityp)%ioffs,material(ityp)%ioffs_mc)
else if (idep == 0) then
  ioff = max0(material(ityp)%ioffd,material(ityp)%ioffd_mc)
else
  ioff = 0
end if

if(ioff /= NOT_SET_I)then

  if (sdat(ISRF_C) > 0.) then
    if (lter) then
      call get_smooth_topog(p%xbar,p%ybar,h,hx,hy, &
                                     sqrt(p%sxx),sqrt(p%syy))
      zp = p%zbar - (zsrf+h)
      zs = p%zbar - h
      call puff_reflect(zs,zp,p,hx,hy,.false.,xr,vfac)
      voli = sqrt(deth)/pi
    else
      det   = p%axx*p%ayy - p%axy**2
      zp    = 2.*(p%zbar-zsrf)/det
      xr(1) = zp*(p%axz*p%ayy - p%ayz*p%axy)
      xr(2) = zp*(p%ayz*p%axx - p%axz*p%axy)
      voli  = sqrt(det)/pi
    end if
    lpuf = voli*sdat(ISRF_C) > 0.
  end if

  if (lpuf) then
    call puff_dos(ipuf,sdat,srf,idep)
    if (nError /= NO_ERROR) go to 9999
  end if
end if

9999    continue

return

end


subroutine puff_dos(ipuf,sdat,srf,idep)
!*******************************************************************************
!
! FUNCTION:  Prepares to add the surface dose from a puff onto the surface grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac             set_srf_var                  get_mc
!             get_totalcc              inter_self              dezone_lev
!               grid_puff
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use refl_inc
use srfparam_inc
use srfdos_inc

implicit none

! --- ARGUMENTS
 
integer ipuf           !Puff number
real    sdat(*)        !Surface data array
type ( grid_str ) srf  !Surface structure
integer idep           !Dose or deposition flag

! --- LOCALS

type ( puff_mc ) pm
type ( puff_totalcc  ) pt

integer k, indx, ipnt, ncell

real crat
real facv0, facv, volg, volp, pmass, ccs

!------ Set surface grid level

call mapfac( p%xbar , p%ybar , xmap , ymap )

xbar  = p%xbar + 0.5*xr(1)*xmap - srf%xmin
ybar  = p%ybar + 0.5*xr(2)*ymap - srf%ymin

if (lter) then
  axx = b_rfl(1,1)
  axy = b_rfl(1,2)
  axz = b_rfl(1,3)
  ayy = b_rfl(2,2)
  ayz = b_rfl(2,3)
  azz = b_rfl(3,3)
  rrr = 1./sqrt(1.+hx*hx+hy*hy)
  bxx = axx*a_rfl(1,1)**2 + 2.*axy*a_rfl(1,1)*a_rfl(2,1) &
           + ayy*a_rfl(2,1)**2
  byy = axx*a_rfl(1,2)**2 + 2.*axy*a_rfl(1,2)*a_rfl(2,2) &
           + ayy*a_rfl(2,2)**2
  bxy = axx*a_rfl(1,1)*a_rfl(1,2) + ayy*a_rfl(2,1)*a_rfl(2,2) &
           + axy*(a_rfl(1,1)*a_rfl(2,2)+a_rfl(1,2)*a_rfl(2,1))
else
  axx = p%axx
  axy = p%axy
  axz = p%axz
  ayy = p%ayy
  ayz = p%ayz
  azz = p%azz
  rrr = 1.
  bxx = axx
  byy = ayy
  bxy = axy
end if

xlam = 0.5* (axx + ayy &
             + sqrt((axx-ayy)**2+4.*axy**2))
del  = sqrt(0.25/xlam) * rrr
mlev = max0(int(alog(del/max(dxs/xmap,dys/ymap))/alog(0.5)),0)

volp = voli

1000    continue

!------ set grid parameters; reset if dezoning

if(mlev > iabs(srf%maxlev))then
  srf%maxlev = iabs(srf%maxlev)
  mlev       = srf%maxlev
end if

nlev = 2**mlev
dfac = 1.0/float(nlev)
delx = dxs*dfac
dely = dys*dfac

volg  = xmap*ymap/(delx*dely)
if (volp > volg) then
  facv0 = (volp/volg-1.0)
  voli = volg
else
  voli = volp
  facv0 = 0.
end if
facv = facv0

!------ Set surface variables

call set_srf_var(material(ityp),p%ityp,ig,ng,igf,idep,nvx)

if(ng < 0)then
  ng = -ng
  call get_mc(p,pm)
end if
do k = 1,ng
  cfac(k) = 0.0
end do

argfac = 1.0

k = 1

pmass = sdat(ISRF_C )*(p%cfo)
cmax  = sdat(ISRF_C0)
conc_min = material(ityp)%prop(3)

do while (k < ng)

  cfac(k) = voli*sdat(ISRF_C )*(p%cfo)                  !g/m**2

  if(igf(k) < 0)then
    cfac(k+1) = voli*sdat(ISRF_CCT)*((p%cfo)**2)        !g**2/m**4
    if (facv0 > 0.0) then
      call get_totalcc(p,pt)
      call inter_self(p,ccs)
      facv = facv0*min(1.0e8,pt%cctb/max(ccs,1.e-30))
    end if
  else if(igf(k) == 0)then
    cfac(k+1) = voli*sdat(ISRF_CC)*((p%cfo)**2)         !g**2/m**4
    if (facv0 > 0.0) then
      call inter_self(p,ccs)
      facv = facv0*min(1.0e8,p%ccb/max(ccs,1.e-30))
    end if
  else
    if(igf(k) > MAX_MC)then
      indx = ISRF_CCT
      ipnt = igf(k) - MAX_MC
    else
      indx = ISRF_CC
      ipnt = igf(k)
    end if
    cfac(k+1) = voli*sdat(indx)*((p%cfo)**2)            !g**2/m**4
    if(p%c > 0.)then
      crat      = pm%mc(ipnt)/p%c
    else
      crat = 0.0
    end if
    cfac(k  ) = cfac(k  )*crat
    cfac(k+1) = cfac(k+1)*crat*crat
  if(k == 1)pmass = pmass*crat
  end if
  cfac(k+1) = cfac(k+1) + facv*cfac(k)**2

  cfac(k+2) = cfac(k+1)*sdat(ISRF_SL)                   !m-g**2/m**4


  k = k + nvx
end do

!------ Set parameters for grid sweep; check if dezoning required

xfac = argfac*ARGMAX / (1.0 - bxy*bxy/bxx/byy)
yp   = ymap*sqrt(xfac/byy)

m1 = nxs*nlev
n1 = nys*nlev

j1  = max0(int((ybar - yp)/dely),1)
j2  = min0(int((ybar + yp)/dely) + 1,n1)

if (j2 >= j1) then

  xp   = sqrt(argfac*ARGMAX/bxx) * xmap
  i2   = min0(m1,2*int(xp/delx)+1)

  ncell = i2*(j2-j1+1)* int((4.-0.5**(2*mlev))/3.)

  do while (srf%ncells+ncell > MAXSG)
    call dezone_lev(srf)
    if (srf%maxlev < mlev) go to 1000
  end do

  call grid_puff(ipuf,srf,idep,nvx,pmass)
  if (nError /= NO_ERROR) go to 9999

end if

9999    continue

return
end


subroutine grid_puff(ipuf,srf,idep,nv,pmass)
!*******************************************************************************
!
! FUNCTION:   Create adaptive grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              accum_surf              get_delmin
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use srfdos_inc
use files_inc

implicit none

! --- ARGUMENTS
 
integer ipuf           !Puff number
type ( grid_str ) srf  !Surface structure
integer idep           !Dose or dep flag
integer nv             !Number of variables
real pmass             !Mass to be transferred to surface grid

! --- LOCALS

integer nvt, ncell, k, icell
real darea, csum

logical interior


nvt   = srf%nvart
ncell = srf%ncells
do i = 1,ng
  ccell(i) = 0.0
end do

xplus  = xbar + xp
xminus = xbar - xp
cxy  = bxy/bxx

csum  = 0.

interior = (j1 > 1) .and. (j2 < n1)

do j = j1,j2
  y  = (float(j)-0.5)*dfac
  yp = (y*dys-ybar)/ymap
  x  = -cxy * yp * xmap
  i1 = max0( int((xminus + x)/delx)    , 1 )
  i2 = min0( int((xplus  + x)/delx) + 1, m1)
  interior = interior .and. (i1 > 1) .and. (i2 < m1)

  if (i2 >= i1) then

    icell = 0

    do i = i1,i2
      x  = (float(i)-0.5)*dfac
      xp = (x*dxs-xbar)/xmap
      arg  = bxx*xp*xp + 2.*bxy*xp*yp + byy*yp*yp

      if( arg < argfac*ARGMAX )then

        fac  = exp(-arg)

        do k = 1,ng
          ccell(k) = fac*cfac(k)
        end do

        csum = csum + ccell(1)

        if (abs(fac*cmax) > conc_min) then
          call accum_surf(x,y,srfgrd(1,srf%ipgrd), &
                                   srfdat(1,srf%ipdat+1),ig,ng, &
                       nv,nvt,ccell,mlev,ncell,nxs,MAXSG,icell)
          if (nError /= NO_ERROR) go to 9998
        else
          icell = 0
        end if
      else
        icell = 0
      end if                  ! if arg < ARGMAX
    end do                  ! do i=i1,i2

    if (i2-i1 <= 1) then
      do k = 1,ng
        ccell(k) = 0.0
      end do
      i = max0(i1-1,1)
      x  = (float(i)-0.5)*dfac
      icell = 0
      call accum_surf(x,y,srfgrd(1,srf%ipgrd), &
                               srfdat(1,srf%ipdat+1),ig,ng, &
                       nv,nvt,ccell,mlev,ncell,nxs,MAXSG,icell)
      if (nError /= NO_ERROR) go to 9998
      i = min0(i2+1,m1)
      x  = (float(i)-0.5)*dfac
      icell = 0
      call accum_surf(x,y,srfgrd(1,srf%ipgrd), &
                               srfdat(1,srf%ipdat+1),ig,ng, &
                       nv,nvt,ccell,mlev,ncell,nxs,MAXSG,icell)
      if (nError /= NO_ERROR) go to 9998
    end if
  end if
end do                  ! do j=j1,j2

if (j2-j1 <= 1) then
  do k = 1,ng
    ccell(k) = 0.0
  end do
  if (i2-i1 <= 1) then
    i1 = max0(i1-1,1)
    i2 = min0(i2+1,m1)
  end if

  j  = max0(j1-1,1)
  ym = (float(j)-0.5)*dfac
  j  = min0(j2+1,n1)
  yp = (float(j)-0.5)*dfac

  do i = i1,i2
    x  = (float(i)-0.5)*dfac
    icell = 0
    call accum_surf(x,ym,srfgrd(1,srf%ipgrd), &
                              srfdat(1,srf%ipdat+1),ig,ng, &
                       nv,nvt,ccell,mlev,ncell,nxs,MAXSG,icell)
    if (nError /= NO_ERROR) go to 9998
    x  = (float(i)-0.5)*dfac
    icell = 0
    call accum_surf(x,yp,srfgrd(1,srf%ipgrd), &
                              srfdat(1,srf%ipdat+1),ig,ng, &
                       nv,nvt,ccell,mlev,ncell,nxs,MAXSG,icell)
    if (nError /= NO_ERROR) go to 9998
  end do
end if

!------ Adjust mass deposition if necessary

if (interior) then
  darea = delx*dely/(xmap*ymap)
  if (abs(csum) < 0.9*abs(pmass)/darea) then
    j  = max0(nint(ybar/dely),1)
    y  = (float(j)-0.5)*dfac
    i  = max0(nint(xbar/delx),1)
    x  = (float(i)-0.5)*dfac

    fac = (abs(pmass)/darea - abs(csum)) / abs(cfac(1))

    do k = 1,ng
      ccell(k) = fac*cfac(k)
    end do

    icell = 0
    call accum_surf(x,y,srfgrd(1,srf%ipgrd), &
                   srfdat(1,srf%ipdat+1),ig,ng, &
                       nv,nvt,ccell,mlev,ncell,nxs,MAXSG,icell)
    if (nError /= NO_ERROR) go to 9998
  end if
end if

9999    continue

srf%ncells = ncell

return

9998    continue
eMessage = 'Too many surface cells'
call get_delmin(srf,srfgrd(1,srf%ipgrd),del,k)
write(eInform,*) 'Current minimum grid size is ',del,'m'
eAction  = 'Try increasing DELMIN in '//TRIM(file_inp)
go to 9999

end


subroutine srf_decay
!*******************************************************************************
!
! FUNCTION:  Decay material on the surface
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           output_groups                 GetNdep
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf

implicit none

! --- LOCALS

integer nvar, imat, icell, nsg, output_groups
integer ig0, igt, isg, ig

real dcfac(MAXMTYP)
real fac, fac2

integer GetNdep

!------ decay factors

do imat = 1,ntypm
  dcfac(imat) = exp(-decay_rate(imat)*delt)
end do

!------ loop over all surface cells, material types and subgroups

do imat = 1,ntypm

  if (decay_rate(imat) > 0.) then

    nsg = output_groups(material(imat),mat_aux)
    nvar = GetNdep(material(imat)%icls)
    if(material(imat)%lsrfg)then
      ig0  = material(imat)%ioffs + 1
      if(material(imat)%lsrft)then
        igt = ig0 + nsg*nvar
       else
        igt = -1
      end if
    else
      ig0  = -1
      if(material(imat)%lsrft)then
        igt = material(imat)%ioffs + 1
      else
        igt = -1
      end if
    end if

    fac  = dcfac(imat)
    fac2 = fac*fac

    if (ig0 > 0) then

      do isg = 1,nsg

        do icell = 1,srfdep%ncells

          ig = ig0 + srfdep%ipdat
          srfdat(icell,ig) = srfdat(icell,ig)*fac

          ig = ig + 1
          srfdat(icell,ig) = srfdat(icell,ig)*fac2

          ig = ig + 1
          srfdat(icell,ig) = srfdat(icell,ig)*fac2

        end do

        ig0 = ig0 + nvar

      end do

    end if

    if (igt > 0) then

      do icell = 1,srfdep%ncells

        ig = igt + srfdep%ipdat
        srfdat(icell,ig) = srfdat(icell,ig)*fac

        ig = ig + 1
        srfdat(icell,ig) = srfdat(icell,ig)*fac2

        ig = ig + 1
        srfdat(icell,ig) = srfdat(icell,ig)*fac2

      end do

    end if

  end if

end do


return

end


subroutine set_srf_var(matl,ityp,ig,ng,igf,idep,nvx)
!*******************************************************************************
!
! FUNCTION:   Set-up the variables to be written out
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 GetNdep                 GetNdos           output_groups
!                IsHazard          num_srf_fields             GetMultiNum
!          GetMultiSrfPos
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use srfparam_inc

implicit none

! --- ARGUMENTS
 
integer ityp    !Material type
integer ig(*)   !Array of fields that this puff contributes to 
integer igf(*)  !Array of field types
integer ng      !Number of fields that this puff contributes to
integer idep    !Dose or dep flag
integer nvx     !Number of variables to be written out

type ( material_str ) matl   !Material structure

! --- LOCALS

logical lgrp, ltot, IsHazard
integer GetNdep,GetNdos, num_srf_fields

integer k, kk, ioff, iv, nsg, output_groups, nspecies
integer iadd, ihaz, GetMultiNum, GetMultiSrfPos

ng = 0

!====== Surface Deposition

if(idep == 1)then
  lgrp = matl%lsrfg
  ltot = matl%lsrft
  ioff = matl%ioffs
  nvx = GetNdep(matl%icls)

!====== Surface Dose

else if (idep == 0) then
  lgrp = matl%ldosg
  ltot = matl%ldost
  ioff = matl%ioffd
  nvx = GetNdos(matl%icls)

!====== Slice

else
  ng = NV_BASIC
  do k = 1,ng
    ig(k)  = k
    if(idep < -MAX_MC)then
      igf(k) = -1
    else
      igf(k) = -idep
    end if
  end do
  if(idep >= -MAX_MC)then
    ng  = -ng
  end if
  nvx = NV_BASIC
  go to 9999
end if

!====== Group Output

nsg = output_groups(matl,mat_aux)

ihaz = 0
if (hazard == IHAZ_COMB) then
  if (IsHazard(typeID(ityp)%icls))then
    ihaz = num_srf_fields(matl,idep)
  end if
end if

if(lgrp)then
  iv = typeID(ityp)%igrp
  do k = 1,nvx
    ng = ng + 1
    ig(ng) = ihaz + ioff + (iv-1)*nvx + k
    igf(ng) = 0
  end do
  ioff = ioff + nsg*nvx
end if

!====== Total Output

if(ltot)then
  do k = 1,nvx
    ng = ng + 1
    ig(ng) = ihaz + ioff + k
    igf(ng) = -1
  end do
end if

!====== Multi-component Output

if(idep == 1)then
  ltot = matl%lsrf_mc
  ioff = matl%ioffs_mc
else if (idep == 0) then
  ltot = matl%ldos_mc
  ioff = matl%ioffd_mc
else
  go to 9999
end if

if(ltot)then
  nspecies = GetMultiNum()
  if(typeID(ityp)%ltot)then
    iadd = MAX_MC
  else
    iadd = 0
  end if
  do kk = 1,nspecies
    iv = GetMultiSrfPos(kk,idep)
    if(iv > 0)then
      do k = 1,nvx
        ng = ng + 1
        ig(ng) = ioff + (iv-1)*nvx + k
        igf(ng) = kk + iadd
      end do
    end if
  end do
  ng = -ng
end if

!====== Finished

9999    continue

return
end


integer function num_srf_fields(matl,idep)
!*******************************************************************************
!
! FUNCTION:  Find the number of surface fields
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 GetNdep                 GetNdos           output_groups
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
                           
integer idep               !Dose or deposition flag

type ( material_str ) matl !Material structure

! --- LOCALS

logical lgrp, ltot
integer GetNdep, GetNdos, ng, nvx, nsg, output_groups

ng = 0

!====== Surface Deposition

if(idep == 1)then
  lgrp = matl%lsrfg
  ltot = matl%lsrft
  nvx = GetNdep(matl%icls)

!====== Surface Dose

else if (idep == 0) then
  lgrp = matl%ldosg
  ltot = matl%ldost
  nvx = GetNdos(matl%icls)

end if

!====== Group Output

nsg = output_groups(matl,mat_aux)

if(lgrp) ng = nsg*nvx
if(ltot) ng = ng + nvx

num_srf_fields = ng

return
end


subroutine get_delmin(srf,irg,dels,mxlev)
!*******************************************************************************
!
! FUNCTION:   Find the minimum grid size
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             get_max_lev                  mapfac
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use surface_inc

implicit none

! --- ARGUMENTS
 
type ( grid_str ) srf   !Surface structure
integer irg(*)          !Surface grid
real    dels            !Minimum grid size
integer mxlev           !Maximum refinement level for the grid

! --- LOCALS

real xx, yy, xmap, ymap

call get_max_lev(srf,irg,mxlev)

xx = srf%xmin + 0.5*float(srf%nx-1)*srf%dx
yy = srf%ymin + 0.5*float(srf%ny-1)*srf%dy
call mapfac(xx , yy , xmap , ymap)
dels = min(srf%dx/xmap,srf%dy/ymap)*(0.5**mxlev)

return
end

subroutine get_max_lev(srf,irg,mxlev)
!*******************************************************************************
!
! FUNCTION:   Find the maximum refinement level
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  limget
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use surface_inc

implicit none

! --- ARGUMENTS
 
type ( grid_str ) srf   !Surface structure
integer irg(*)          !Surface grid
integer mxlev           !Maximum refinement level for the grid

! --- LOCALS

integer ix, iy, icell0, mlevi, nrfm

nrfm  = 3
mxlev = 0

do ix = 1,srf%nx
  do iy = 1,srf%ny

    icell0 = (iy-1)*srf%nx + ix
    mlevi   = 0
    call limget(irg,nrfm,icell0,mlevi)
    mxlev  = max0(mxlev,mlevi)

  end do
end do

return
end


recursive subroutine limget(irg,nrfm,icell0,mlev0)
!*******************************************************************************
!
! FUNCTION:  Finds maximum refinement level in the cell
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  limget
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS
 
integer irg(*)         !Surface grid
integer nrfm           !Refinement type
integer icell0         !Current grid cell 
integer mlev0          !Current refinement level

! --- LOCALS

integer mlevx, mlev, i, icell

if( irg(icell0) /= 0)then
  mlevx = mlev0 + 1
  do i = 0,nrfm
    icell = irg(icell0) + i
    mlev  = mlevx
    call limget(irg,nrfm,icell,mlev)
    mlev0 = max0(mlev,mlev0)
  end do
end if

return

end
