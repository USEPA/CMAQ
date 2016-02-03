subroutine init_amb_3d
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION: Initialize the 3D ambient
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_met
use multcomp_inc
use amb_data

implicit none

if (.not. lamb3d) return

!=== set grid to host model grid
nxa = nxb
nya = nyb
nza = nzb
dxa = dxb
dya = dyb
x0a = xb(1) - 0.5*dxa  ! left edge of domain
y0a = yb(1) - 0.5*dya  ! bottom edge of domain

xmaxa = x0a + dxa*nxa  ! right edge of domain
ymaxa = y0a + dya*nya  ! top edge of domain
zmaxa = zb(nzb)

9999 return
end

subroutine setup_wrt_amb
!******************************************************************************
!
! FUNCTION:  Set-up for writing out the ambient
!            (in order to have GUI plot the actual 3D ambient)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         restart_wrt_amb            init_wrt_amb
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use amb_data

implicit none

lwrt_amb = .false.  !wire this as you like (false for APT)

if (lwrt_amb) then
  if (restart) then
    call restart_wrt_amb
  else
    call init_wrt_amb
  end if
end if

return
end

subroutine init_wrt_amb
!******************************************************************************
!
! FUNCTION:  Initialize writing to the 3D ambient file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc
use error_inc
use amb_data

implicit none

! --- LOCALS

integer ios, i, j, k, is

!====   Open 3D ambient file

open(unit=lun_amb,file=amb_file,status='new',&
                  form='unformatted',iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'init_amb'
  eMessage = 'Error opening ambient species file'
  eInform  = 'File='//TRIM(amb_file)
  eAction  = 'Make sure file does not already exist'
  go to 9999
end if

!===   write header data

write(6,*) '3D ambient header:',int(tend_hr/3.),nxa,nya,nza,dxa,dya,x0a,y0a
write(lun_amb,iostat=ios) int(tend_hr/3.),nxa,nya,nza,dxa,dya,x0a,y0a
 100    format(4(i4,1x),5(f12.2,1x))
if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'init_amb'
  eMessage = 'Error reading ambient species input file header'
  eInform  = 'File='//TRIM(amb_file)
  go to 9999
end if

9999 return
end

subroutine get_amb(x,y,z,tdum,cgrid)
!******************************************************************************
!
! FUNCTION:  Get the ambient concentrations at location x,y,z
!            (finds nearest grid point)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!    Updated April 2005 to handle particle species (PKK, AER)
!******************************************************************************
 
! --- MODULES
 
use param_inc
use multcomp_inc
USE CGRID_SPCS, only: nspcsd
use common_met
use common_puf
use amb_data
use code_defines_inc

implicit none

! --- ARGUMENTS
 
real x, y, z  !Location
real tdum     !Time (used in regular PC code)
REAL :: CGRID( :,:,:,: )  !3D ambient array

! --- LOCALS

real xp,yp,zp
integer i, j, k, jj, ii, is, it, ix, iy, iz
real x1,y1,z1
real ctem(MAX_MC)
real dum1, dum2, dp, hp

logical IsMCParticle

! -- initialize working array
ctem = 0.

! -- transform z
if (lter) then
  call get_topog(x,y,hp,dum1,dum2)
  dp = 1. - hp/zbtop
  zp = (z - hp)/dp
else
  zp = z
end if

! -- limit coordinates
xp = min(xmaxa,max(x0a,x))
yp = min(ymaxa,max(y0a,y))
zp = min(zmaxa,max(0.,zp))

! -- find spatial indices
ix = min(max(1,int((xp - x0a)/dxa) + 1),nxa)
iy = min(max(1,int((yp - y0a)/dya) + 1),nya)

do i = 1,nzb - 1
   if (zp >= zbw(i) .and. zp < zbw(i+1)) then
      iz = i
      go to 45
   end if
end do

iz = nzb

45   continue

! -- get nearest ambient concentrations
do is = 1, NSPCSD
   ctem(is) = max(0.,cgrid(ix,iy,iz,is))
end do

! -- load ambient arrays
do i = 1, nspectot
  ii = i_spec_list(i)
  ps(i)%a = ctem(ii)
end do

! -- scale ambient concentrations to STP (gases only)
if (ic_units == UNIT_PPM .and. CODEPARAM /= STDAMODEL) then
  if (tb > 0.0 .and. pb > 0.0) then 
    tab = tb*(pb**0.285714)
    do i = 1, nspectot
      if ( .not. IsMCParticle(i)) then
        ps(i)%a = (ps(i)%a*298.*pb)/tab
      end if
    end do
  end if
end if

return
end

subroutine write_amb_data(cgrid)
!******************************************************************************
!
! FUNCTION:  Write out the 3D ambient data to a file 
!            that the GUI can read
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use multcomp_inc
use files_inc
use amb_data
USE CGRID_SPCS, only: nspcsd

implicit none

! --- ARGUMENTS
 
REAL :: CGRID( :,:,:,: )    !3D ambient data

! --- LOCALS

integer is, i, j, k, ios, ii

write(lun_amb) t/3600.
do is = 1, NSPCSD - 1
  ii = i_spec_list(is)
  write(lun_amb,iostat=ios) &
           (((cgrid(i,j,k,ii),i= 1,nxa),j=1,nya),k=1,nza)
  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_amb_data'
    eMessage = 'Error writing ambient species input file'
    eInform  = 'File='//TRIM(amb_file)
    go to 9999
  end if
end do

9999    return
end

subroutine restart_wrt_amb 
!******************************************************************************
!
! FUNCTION:  Advance the ambient file on a restart
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc
use error_inc
USE CGRID_SPCS, only: nspcsd

implicit none

! --- LOCALS

integer ios, is
real tem

!====   Open 3D ambient file

open(unit=lun_amb,file=amb_file,status='old',&
                  form='unformatted',iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'restart_wrt_amb'
  eMessage = 'Error opening ambient species file'
  eInform  = 'File='//TRIM(amb_file)
  eAction  = 'Make sure file exists'
  go to 9999
end if

!===   advance 3d-ambient file

read(lun_amb)
tem = -999.
do while (tem /= trst/3600.)
  read(lun_amb,err=9998,end=9998)tem
  do is = 1, NSPCSD - 1
    read(lun_amb)
  end do
end do

9999 return

9998   nError   = RD_ERROR
       eRoutine = 'restart_wrt_amb'
       eMessage = 'Error reading ambient species file'
       eInform  = 'File='//TRIM(amb_file)
       go to 9999

end
