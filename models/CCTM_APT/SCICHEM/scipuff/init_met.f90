!*******************************************************************************
!$RCSfile: init_met.f90,v $
!$Revision: 1.10 $
!$Date: 2010/12/06 03:23:30 $
!*******************************************************************************
function days_in_year(yr)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Give the number of days in the year
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
integer yr    !4-digit year

! --- LOCALS
integer days_in_year

if (mod(yr,4) == 0) then
  days_in_year = 366
else
  days_in_year = 365
end if

return
end


subroutine check_bl
!*******************************************************************************
!
! FUNCTION:   Check that the boundary layer inputs are compatible
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          WarningMessage
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

logical lbl_met

!------ make sure zruf is set (since it is always required if lbl is true)

if (zruf == NOT_SET_R .or. zruf == 0.) then
  nError   = IV_ERROR
  eRoutine = 'check_bl'
  eMessage = 'ZRUF must be greater than zero with any BL_TYPE'
  eInform  = 'File='//TRIM(file_msc)
  go to 9999
end if

if (h_cnp == 0.) then
  nError   = IV_ERROR
  eRoutine = 'check_bl'
  eMessage = 'Canopy height cannot equal zero'
  eInform  = 'File='//TRIM(file_msc)
  go to 9999
end if

!------ set lzi and lhflx to false for calculated bl
!------ only used for operational bl
if (bl_type == 'CALC' .or. bl_type == 'SBL') then
  lzi   = .false.
  lhflx = .false.
  lpgt  = .false.
end if

!------ check for boundary layer variables on met input files

lbl_met = (lzi .and. lhflx) .or. lpgt

!------ set actual bl_type for OPER

if (bl_type == 'OPER') then

  if (lbl_met) then
    if (met_type == 'OBS') then
      bl_type = 'OBS'
    else if (met_type == 'MEDOC') then
      bl_type = 'MEDOC'
    else
      bl_type = 'CALC'                             ! should never get here
    end if
  else
    bl_type = 'CALC'
  end if

  write(lun_log,1000) bl_type
  if (lzi) write(lun_log,1001)
  if (lhflx) write(lun_log,1002)
1000      format(/,'**************** Operational BL Type = ',a)
1001      format('                 Using observed mixing heights')
1002      format('                 Using observed surface heat flux')

  if (lprcp .or. lprate) then
    lwash = .true.
    pr_type = 'METFILE'
  end if

  write(lun_log,1003) pr_type
1003      format(/,'**************** Operational Precip. Type = ',a)

end if

!------ check combinations with boundary layer types

if (bl_type == 'OBS' .and. .not.lbl_met) then
  nError   = UK_ERROR
  eRoutine = 'check_bl'
  eMessage = 'BL_TYPE=OBS requires ZI and HFLUX or PGT on obs file'
  eInform  = 'These may be given on surface and/or profile files'
  eAction  = 'Modify obs file(s) or change BL_TYPE in '//TRIM(file_msc)
  go to 9999
else if (bl_type == 'MEDOC' .and. .not.lbl_met) then
  nError   = UK_ERROR
  eRoutine = 'check_bl'
  eMessage = 'BL_TYPE=MEDOC requires ZI and HFLX on MEDOC file'
  eAction  = 'Use different MEDOC file or change BL_TYPE in '//TRIM(file_msc)
  go to 9999
else if (bl_type == 'PROF' .and. lsfc) then
  nError   = UK_ERROR
  eRoutine = 'check_bl'
  eMessage = 'No met surface obs allowed for BL_TYPE=PROF'
  eInform  = 'Delete reference to surface obs file or change BL_TYPE'
  eAction  = 'File='//TRIM(file_msc)
  go to 9999
end if

if (bl_type == 'PROF' .and. lsplitz) then
  nError   = WN_ERROR
  eRoutine = 'check_bl'
  eMessage = 'BL_TYPE=PROF and LSPLITZ=TRUE'
  eInform  = 'LSPLITZ should be FALSE when using turbulence profiles'
  call WarningMessage(0,.false.)
  if (nError /= NO_ERROR) go to 9999
  lsplitz = .false.
end if

if (pr_type == 'METFILE' .and. .not.(lprcp .or. lprate) ) then
  nError   = UK_ERROR
  eRoutine = 'check_bl'
  eMessage = 'Precip. type = METFILE but no precip. on file(s)'
  go to 9999
end if

if (lprcp .and. lprate .and. pr_type == 'METFILE') then
  nError   = WN_ERROR
  eRoutine = 'check_bl'
  eMessage = 'Precip. type specified both as index (PRCP) and as a rate (PRATE)'
  eInform  = 'Will use precipitiation rate (PRATE)'
  lprcp = .false.
  call WarningMessage(0,.true.)
  if (nError /= NO_ERROR) go to 9999
end if

!------ check some variables for bl_type=CALC

if (bl_type == 'CALC') then

  if (bowen == NOT_SET_R) then
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set BOWEN ratio for boundary layer calculation'
    eInform  = 'File='//TRIM(file_msc)
    go to 9999
  end if

  if (albedo == NOT_SET_R) then
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set ALBEDO for boundary layer calculation'
    eInform  = 'File='//TRIM(file_msc)
    go to 9999
  end if

  if (cloud_cover == NOT_SET_R) then
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set CLOUD COVER for boundary layer calculation'
    eInform  = 'File='//TRIM(file_msc)
    go to 9999
  end if

!-------- check if lat/lon info is available

  if ( ( (lmap == I_CARTESIAN) .or. &
              (lmap == I_UTM      ) .or. &
              (lmap == I_METERS   ) ) .and. &
                (lat0 == NOT_SET_R .or. lon0 == NOT_SET_R)) then
    nError   = UK_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must define map reference location for a UTM'
    eMessage = TRIM(eMessage)//' or Cartesian run when using BL_TYPE='//TRIM(bl_type)
    eInform  = 'File='//TRIM(file_msc)
    go to 9999
  end if

!-------- need year-month-day-hour for calculated boundary layer

  if (.not.lymd) then
    nError   = UK_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Incorrect time specification for BL_TYPE='//TRIM(bl_type)
    eInform  = 'Must use year-month-day-hour'
    eAction  = 'File='//TRIM(file_msc)
    go to 9999
  end if

end if

!-------- check if lat/lon info is available

if ( multicomp) then
  if ( ( (lmap == I_CARTESIAN) .or. &
              (lmap == I_UTM      ) .or. &
              (lmap == I_METERS   ) ) .and. &
                (lat0 == NOT_SET_R .or. lon0 == NOT_SET_R)) then
    nError   = UK_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must define map reference location for a UTM'
    eMessage = TRIM(eMessage)//' or Cartesian run when using multicomponent'
    eInform  = 'File='//TRIM(file_inp)
    go to 9999
  end if
end if

!------ check SBL variables

if (bl_type == 'SBL') then

!------ check zi

  if (zimin == NOT_SET_R .or. zimax == NOT_SET_R) then
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set ZIMIN, ZIMAX for boundary layer'
    eInform  = 'File='//TRIM(file_msc)
    go to 9999
  end if

!------ check heat flux

  if (hconst == NOT_SET_R .or. hdiur == NOT_SET_R) then
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set HCONST, HDIUR for boundary layer'
    eInform  = 'File='//TRIM(file_msc)
    go to 9999

  else

!------ convert hflux parameters to temperature flux K-m/s

    hconst_c = hconst / rhocp
    hdiur_c  = (hdiur-hconst) / rhocp

  end if

end if

9999    continue

return

end


subroutine check_msc(file)
!*******************************************************************************
!
! FUNCTION:  Check that inputs on the MSC file are compatible
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

use common_puf
use common_met

implicit none

! --- ARGUMENTS
 
character*(*) file !MSC file name

!------ check that required "types" are set

if ( met_type == 'CLIMO' &
        .or. met_type == 'CLI3D') then
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Incorrect MET_TYPE='//TRIM(met_type)
  eInform  = 'Not allowed in this version of SCICHEM'
  go to 9999
end if

if (met_type == NOT_SET_C) then
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Must set MET_TYPE'
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

if (bl_type == NOT_SET_C) then
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Must set BL_TYPE'
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

if (ensm_type == NOT_SET_C) then
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Must set ENSM_TYPE'
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

if (met_type /= 'OBS' .and. met_type /= 'MEDOC' &
        .and. met_type /= 'MRF' .and. met_type /= 'GRIDDED' &
        .and. met_type /= 'FIXED') then
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Incorrect MET_TYPE='//TRIM(met_type)
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

if (hazard /= IHAZ_OFF) then
  if (met_type == 'FIXED') then
    nError   = UK_ERROR
    eRoutine = 'check_msc'
    eMessage = 'Cannot do MET_TYPE=FIXED with hazard areas'
    eInform  = 'File='//TRIM(file)
    go to 9999
  end if
end if

!------ check BL types

if ( (bl_type == 'OBS'   ) .or. &
          (bl_type == 'SBL'   ) .or. &
          (bl_type == 'PROF'  ) .or. &
          (bl_type == 'OPER'  ) .or. &
          (bl_type == 'MEDOC' ) .or. &
          (bl_type == 'CALC') ) then
  lbl     = .true.
else if ( (bl_type == 'OFF'   ) .or. &
               (bl_type == 'NONE'  ) .or. &
               (bl_type == 'FALSE' ) ) then
  lbl = .false.
else
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Incorrect BL_TYPE='//TRIM(bl_type)
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

if (lbl) then
  nzbl = max0(3,nzbl)
else
  nzbl = 1
  lsplitz = .false.             ! turn-off lumped BL flag
end if

!------ check ENSM input

if (ensm_type == 'OPER3.1') then
  ensm_type = 'MODEL'
  lsv_oper = .true.
end if

if (ensm_type == 'OBS') then
  lensm = .true.
  if (sl_ensm == NOT_SET_R .or. sl_ensm <= 0.) then
    nError   = UK_ERROR
    eRoutine = 'check_msc'
    eMessage = 'Must set SL_ENSM'
    eInform  = 'File='//TRIM(file)
    go to 9999
  end if
  uu_ensm = 0.
else if (ensm_type == 'MODEL') then
  lensm = .true.
  sl_ensm = 1.e+10
  uu_ensm = 0.
else if (ensm_type == 'INPUT') then
  lensm = .true.
  if (sl_ensm == NOT_SET_R .or. sl_ensm <= 0.) then
    nError   = UK_ERROR
    eRoutine = 'check_msc'
    eMessage = 'Must set SL_ENSM'
    eInform  = 'File='//TRIM(file)
    go to 9999
  end if
  if (uu_ensm == NOT_SET_R) then
    nError   = UK_ERROR
    eRoutine = 'check_msc'
    eMessage = 'Must set UU_ENSM'
    eInform  = 'File='//TRIM(file)
    go to 9999
  end if
else if ( (ensm_type == 'OFF'   ) .or. &
             (ensm_type == 'NONE'  ) .or. &
             (ensm_type == 'FALSE' ) ) then
  lensm = .false.
  uu_ensm = 0.
  sl_ensm = 1.e+6
else if ( (ensm_type == 'OPER' ) )then !Just turn off if LSV=OPER - needed by GUI
  lensm = .false.
  uu_ensm = 0.
  sl_ensm = 1.e+6
else
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Incorrect ENSM_TYPE='//TRIM(ensm_type)
  eInform  = 'File='//TRIM(file)
  go to 9999
end if

!------ check for precipitation specifications

if (pr_type == 'NONE') then
  prbl    = 0.
  pratebl = 0.
  lwash   = .false.
else if (pr_type == 'METFILE') then
  prbl    = 0.
  pratebl = 0.
else if (pr_type == 'LGTRAIN') then
  prbl    = 1.
  pratebl = 0.5
else if (pr_type == 'MODRAIN') then
  prbl    = 2.
  pratebl = 3.5
else if (pr_type == 'HVYRAIN') then
  prbl    = 3.
  pratebl = 25.
else if (pr_type == 'LGTSNOW') then
  prbl    = 4.
  pratebl = 0.
else if (pr_type == 'MODSNOW') then
  prbl    = 5.
  pratebl = 0.
else if (pr_type == 'HVYSNOW') then
  prbl    = 6.
  pratebl = 0.
else
  nError   = UK_ERROR
  eRoutine = 'check_msc'
  eMessage = 'Incorrect precipitation type'
  eInform  = 'Check available precipitation types'
  eAction  = 'File='//TRIM(file)
  go to 9999
end if

9999    continue

return

end


subroutine check_time
!*******************************************************************************
!
! FUNCTION:  Check time conventions for met files
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

use common_puf
use common_met
use files_inc

implicit none

if (tzone == DEF_VAL_R) then
  if ( (local .neqv. local_met) .or. &
           (bl_type == 'SBL' .and. .not.local) .or. &
           (bl_type == 'CALC' .and. local) ) then
    nError   = UK_ERROR
    eRoutine = 'check_time'
    eMessage = 'Must set time zone [=local time of 0000Z (UTC)]'
    eInform  = 'File='//TRIM(file_msc)
    go to 9999
  end if
end if

if (local .eqv. local_met) then
  tmet_offset = tstart*3600.
else
  if (local) then
    tmet_offset = (tstart - tzone)*3600.
  else
    tmet_offset = (tstart + tzone)*3600.
  end if
end if
write(lun_log,'(a,f8.1)') 'tmet_offset ',tmet_offset/3600.

9999    continue

return
end


subroutine init_met_params
!*******************************************************************************
!
! FUNCTION:  Initialize all met field parameters
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!             
! 26 NOV 2003  :  Removed initialisation to 0 of zi_bl to start.f90. This
!                 is to avoid initialisation for restarted projects. -BC
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use files_inc

implicit none

time_ua        = 0.0
time_bl        = 0.0
time_ua_old    = 0.0
time_bl_old    = 0.0
time_met       = NOT_SET_R
time_met_old   = NOT_SET_R
time_swift     = NOT_SET_R
time_swift_old = NOT_SET_R
dt_swift       = NOT_SET_R
tbin_met       = NOT_SET_R
lua          = .false.
lbl          = .false.
lensm        = .false.
lsv_oper     = .false.
lsfc         = .false.
l3d_obs      = .false.
l2d_sfc      = .false.
lter         = .false.
lformat      = .false.
update_ua    = .false.
update_bl    = .false.
update_swift = .false.
next_swift   = .false.
lmc_ua       = .false.
lswift       = .false. 
lfirst       = .true.
local_met    = .false.
lset_ua      = .false.
lset_sfc     = .false.
wflag        = .false.
tflag        = .false.
hflag        = .false.
cldflag      = .false.
lzi          = .false.
lhflx        = .false.
lfcc         = .false.
lpgt         = .false.
lprcp        = .false.
lprate       = .false.
do_sfc       = .false.
do_sfc_bl    = .false.
do_sfc_fcc   = .false.
do_sfc_prcp  = .false.
do_sfc_prate = .false.
do_ua        = .false.
do_ua_bl     = .false.
do_ua_fcc    = .false.
do_ua_prcp   = .false.
do_ua_prate  = .false.
lhaz_obs     = .false.
lhaz_sfc     = .false.
sl_haz       = NOT_SET_R
met_type     = NOT_SET_C
bl_type      = NOT_SET_C
ensm_type    = NOT_SET_C
obs_type     = NOT_SET_I
nxe          = 0
nye          = 0
nxb          = 1
nyb          = 1
nxyb         = 1
nxbl         = 1
nybl         = 1
dxb          = 1.e+10
dyb          = 1.e+10
dxbl         = 1.e+10
dybl         = 1.e+10
zimin        = NOT_SET_R
zimax        = NOT_SET_R
hconst       = NOT_SET_R
hdiur        = NOT_SET_R
h_cnp        = -1.0
zref         = NOT_SET_R
zruf         = NOT_SET_R
bowen        = NOT_SET_R
albedo       = NOT_SET_R
cloud_cover  = NOT_SET_R
pr_type      = 'NONE'
tprcp        = NOT_SET_R
tprcpn       = NOT_SET_R
lwash        = .true.
hmin         = 0.
lstagger     = .false.
nzb          = NOT_SET_I
zb           = NOT_SET_R
time_anlys%Year = NOT_SET_I
ua_fac       = 1.
sfc_fac      = 1.
age_fac_ua   = 1.
age_fac_sfc  = 1.
file_met     = ' '
file_sfc     = ' '
lout_met     = .false.
tout_met     = NOT_SET_R
lout_3d      = .true.
lout_2d      = .true.
lzruf2d      = .false.
xbl          = 0.
ybl          = 0.
tb           = NOT_SET_R
tab          = NOT_SET_R
pb           = NOT_SET_R

!------ initialize lsv arrays
uu_lsv = 0.
sl_lsv = 1.e+6

return
end


subroutine init_met_fields
!*******************************************************************************
!
! FUNCTION:  Initialize upper air and boundary layer met fields
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              stnd_atmos
!
! REVISION HISTORY: 
!     Updated March 2005 - whole array operations (PKK, AER)
!
!*******************************************************************************
 
! --- MODULES
 
use common_met

implicit none

! --- LOCALS

integer i, i0, k, icld

real t_usa, pfac, ztem, dum

!------ initialize upper air fields

u_ua    = 0.
v_ua    = 0.
w_ua    = 0.
h_ua    = 0.
uue_ua  = 0.
vve_ua  = 0.
uve_ua  = 0.

cld_ua  = 0.
radyn2  = 1.0E30
lwc2    = 0.     

do k = 1,nzb
  i0 = (k-1)*nxyb
  do i = 1,nxyb
    ztem = hmin + hs(i) + zb(k)*d(i)
    call stnd_atmos(ztem,pfac,t_usa,dum,0)
    p_ua(i0+i)  = pfac
    t_ua(i0+i)  = t_usa
  end do
end do
do i = 1,nxyb
  k = nzb*nxyb
  i0 = (nzb-1)*nxyb
  t_ua(k+i) = t_ua(i0+i)
  p_ua(k+i) = p_ua(i0+i)
end do

!------ initialize bl fields

u_bl      = 0.
v_bl      = 0.
h_bl      = 0.
hflx_bl   = 0.
pgt_bl    = 0.
fcc_bl    = 0.
fprcpc_bl = 0.
prcp_bl   = 0.
prate_bl  = 0.
t_bl      = 288.

cld_bl    = 0.

if (.not.lzi_prj) then
  do i = 1,nxyb
    zi_bl(i)  = 50.
  end do
end if

return
end

subroutine zero_terrain
!*******************************************************************************
!
! FUNCTION:  Zero-out terrain arrays
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!     Updated March 2005 - whole array operations (PKK, AER)
!
!*******************************************************************************
 
! --- MODULES

use common_met

implicit none

hs  = 0.
ddx = 0.
ddy = 0.
d   = 1.
d1  = 1.
d2  = 1.

return

end
