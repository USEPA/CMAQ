!******************************************************************************
!Changed read_met_pig for CMAQ-APT-PM
!PK, AER, April 2005
!******************************************************************************
subroutine init_met_pig(stdate,sttime,tstep)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Initialize the meteorology for a plume-in-grid project
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         init_met_params             set_msc_pig          build_grid_pig
!                check_bl         init_met_fields          set_bl_from_ua
!           init_ensm_obs             init_amb_3d           setup_wrt_amb
!            days_in_year             init_met_3d
!
! REVISION HISTORY: 
!   May 2006, PK, AER: Add call to new routine init_met_3d
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use multcomp_inc
use files_inc

implicit none

! --- ARGUMENTS
 
integer stdate  !Start date, coded YYYYDDD
integer sttime  !start time, coded HHMMSS
integer tstep   !Model time step,  coded HHMMSS

! --- LOCALS

integer k, ios, jstop, days_in_year,i

!------ initialize

call init_met_params

!------ read met scenario file

call set_msc_pig(file_inp)
if (nError /= NO_ERROR) go to 9999

obs_flag = 0

call build_grid_pig(stdate,sttime,tstep)
if (nError /= NO_ERROR) go to 9999

!------ Initialize julian day for met and chemistry sun-angle

if (lymd) then
  iday_met = int(t/3600./24.) - 1
  jul_met  = jul_start + iday_met
  yr_met   = year_start
  if (jul_met <= 0) then
    yr_met  = yr_met - 1
    nday_yr = days_in_year(yr_met)
  else
    nday_yr = days_in_year(yr_met)
    if (jul_met > nday_yr) then
      yr_met  = yr_met + 1
      jul_met = jul_met - nday_yr
      nday_yr = days_in_year(yr_met)
    end if
  end if
end if

!------ checks for boundary layer files, variables, compatibility, etc.

if (lbl) then

  call check_bl
  if (nError /= NO_ERROR) then
    if (index(eMessage,'ZRUF') /= 0) then
      nError = NO_ERROR
    else
      go to 9999
    end if
  end if

end if

!------ initialized met fields

call init_met_fields

call init_met_3d

!------ set boundary layer grid parameters

call set_bl_from_ua
if (nError /= NO_ERROR) go to 9999

!------ setup ensemble turb. (large-scale variability and/or met uncertainty)

if (lensm .or. hazard /= IHAZ_OFF) then

  call init_ensm_obs
  if (nError /= NO_ERROR) go to 9999

end if

!------ setup ensemble turb. times

if (lensm .or. hazard /= IHAZ_OFF) then

  time_ensm     = time_met_old
  time_eua      = time_met
  time_eua_last = time_met_old

end if

amb_file       = 'nashville.amb'   !wired for PIG
lamb3d = .true.
call init_amb_3d
call setup_wrt_amb

lfirst = .false.

!------ write out met grids to log file

if (lua .or. lmc_ua) then
  write(lun_log,100,iostat=ios)
  write(lun_log,101,iostat=ios) xb(1),xb(nxb),dxb,nxb
  write(lun_log,102,iostat=ios) yb(1),yb(nyb),dyb,nyb
  write(lun_log,103,iostat=ios) (zb(k),k=1,nzb)
end if

if (lbl) then
  write(lun_log,200,iostat=ios)
  write(lun_log,101,iostat=ios) xbl(1),xbl(nxbl),dxbl,nxbl
  write(lun_log,102,iostat=ios) ybl(1),ybl(nybl),dybl,nybl
  write(lun_log,104,iostat=ios) dzbl,nzbl
end if

9999    continue

return

!------ formats

100     format(/,'Meteorology grid')
101     format('xmin,xmax,dx,nx: ',1p,3e12.4,0p,i4)
102     format('ymin,ymax,dy,ny: ',1p,3e12.4,0p,i4)
103     format('z: ',1p,6e12.4,5(/,3x,1p,6e12.4))
104     format('dzbl,nzbl: ',1p,e12.4,0p,i4)
200     format(/,'Boundary layer grid')

end

subroutine set_msc_pig(file)
!*******************************************************************************
!
! FUNCTION:  Set-up the meteorological flags
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            read_met_pig                  cupper               check_msc
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- ARGUMENTS
 
character*(*) file  !Plume-in-grid input file

! --- LOCALS

integer ios

open(unit=lun_inp,file=file,status='OLD',action="read",iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'set_msc_pig'
  eMessage = 'Error opening SCICHEM met scenario input file'
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

call read_met_pig(lun_inp)

call cupper( met_type)
call cupper(  bl_type)
call cupper(ensm_type)
call cupper(pr_type)

call check_msc(file)
if (nError /= NO_ERROR) go to 9999

9999    continue

return

end

subroutine read_met_pig(iunit)
!*******************************************************************************
!
! FUNCTION:  Read the meteorological namelist
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
! April 2005: Updated for CMAQ-APT-PM, PKK, AER
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- ARGUMENTS
 
integer iunit  !Input file unit number

! --- LOCALS

integer nearest_sfc, nearest_prf

namelist / met / ensm_type, uu_ensm, sl_ensm, sl_haz

!---Set default and PIG parameters

lzi         = .true.
lhflx       = .true.
tflag       = .true.
hflag       = .true.
wflag       = .true.
cldflag     = .true.
lter        = .true.
lstagger    = .true.
lfcc        = .true.
lprate      = .true.
lwash       = .true.
ncld        = 1
pr_type     = 'METFILE'
met_type    = 'MEDOC'
bl_type     = 'MEDOC'
ensm_type   = 'OPER3.1'   ! ('OPER3.1' to have operational LSV on)
local_met   = .true.
sl_ensm     = 100000.
uu_ensm     = 0.
nearest_sfc = DEF_VAL_I
nearest_prf = DEF_VAL_I
h_cnp       = -1.
tout_met    = NOT_SET_R
lua         = .true.
lsfc        = .false.

n_obs_max = max0(1,nearest_prf)
n_sfc_max = max0(1,nearest_sfc)

200   read(iunit,met,err=200,end=201)

9999  return

201   nError   = RD_ERROR
      eMessage = 'Error reading MET namelist'
      go to 9999

end

subroutine build_grid_pig(stdate,sttime,tstep)
!*******************************************************************************
!
! FUNCTION:  Build the meteorological grid (for plume-in-grid projects)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 setgrid                 initmap       set_topog_gridded
!                read_smp
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_grd
use files_inc

implicit none

! --- ARGUMENTS
 
integer stdate  !Start date, coded YYYYDDD
integer sttime  !start time, coded HHMMSS
integer tstep   !Model time step,  coded HHMMSS

! --- LOCALS

integer i, idum
real dz

lmap_met = I_PIG
lmap     = I_PIG

call setgrid(stdate,sttime,tstep)

if (nxyb <= 0 .or. nxyb > MAX2D &
         .or. nxb > MAXXB .or. nyb > MAXYB) then
  nError = IV_ERROR
  eMessage = 'Error setting pig grid'
  eRoutine = 'build_grid_pig'
  write(eInform,'(a,3i8)') 'nx,ny,nxy=',nxb,nyb,nxyb
  go to 9999
end if

dxb = dx
dyb = dy

!------ build horizontal met grids

xb(1) = xmin
xbs(1) = xmin + 0.5*dxb
do i = 2,nxb
  xb(i)  = xmin  + float(i-1)*dxb
  xbs(i) = xbs(1) + float(i-1)*dxb
end do

yb(1) = ymin
ybs(1) = ymin + 0.5*dyb
do i = 2,nyb
  yb(i)  = ymin  + float(i-1)*dyb
  ybs(i) = ybs(1) + float(i-1)*dyb
end do

xmin_met = xb(1)
ymin_met = yb(1)
xmax_met = xb(nxb)
ymax_met = yb(nyb)
taflag   = .false.

!------ initialize map factors and roughness lengths

call initmap(stdate,sttime,tstep)

call set_topog_gridded(idum)

!------ Set sampler flags and read data if necessary

call read_smp
if (nError /= NO_ERROR) go to 9999

9999    continue

return
end
