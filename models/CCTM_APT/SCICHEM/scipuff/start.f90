!*******************************************************************************
!$RCSfile: start.f90,v $
!$Revision: 1.8 $
!$Date: 2010/10/28 20:13:41 $
!*******************************************************************************
!  Oct 2005, PK, AER: Dry and wet dep treated separately
subroutine new_start
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Set-up for a new project
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   FDATE      set_version_string               set_param
!                set_vdep                set_tlev        init_diagnostics
!            init_depos2d
!
! REVISION HISTORY: 
!
! 18-MAY-2005  Check for error from init_depos2d - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

!-----  set audit create time and version

call FDATE(audit_date)
call set_version_string(iversion_code,audit_version)

!-----  set/read program parameters

call set_param
if (nError /= NO_ERROR) go to 9999

!-----  compute gravitational settling velocities

call set_vdep
if (nError /= NO_ERROR) go to 9999

!------ Initialize time-level lists

call set_tlev

!------ Initialize multicomponent diagnostics and deposition files

if (multicomp) then
  call init_diagnostics
  if (nError /= NO_ERROR) go to 9999

  call init_depos2d
  if (nError /= NO_ERROR) go to 9999
end if

9999    continue

return

end

subroutine set_param
!*******************************************************************************
!
! FUNCTION:   Read run parameters
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              init_param              read_flags             read_domain
!            read_options          WarningMessage               fix_tzone
!              get_matdef          set_buoy_flags                 read_mc
!              set_depint              julian_day
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use multcomp_inc
use files_inc

implicit none

! --- LOCALS

integer julian_day
integer i, ios

real xmid,res,resmin,defdom

!------ initialize parameters, set defaults, etc.

call init_param

!-----  program input  -----

call read_flags(lun_inp)
if(nError /= NO_ERROR)go to 9997

call read_domain(lun_inp)
if(nError /= NO_ERROR)go to 9997

call read_options(lun_inp)
if(nError /= NO_ERROR)go to 9997

!------ set default vertical grid if required

if(run_mode == FAST_MODE)then
  res    = 7.
  resmin = 1000.
  defdom = 5000.
  simrge = 1.50
else
  res    = 15.
  resmin = 250.
  defdom = 2500.
  simrge = 1.25
end if
if (zmax == DEF_VAL_R) zmax = defdom
if (vres == DEF_VAL_R) vres = max(resmin,zmax/res)

!------ set dynamic flag if dense gas flag is set

dynamic = dense_gas .or. dynamic

!------ met input parameter checks

if (hazard /= IHAZ_OFF) then
  if ( ( (lmap == I_CARTESIAN) .or. &
            (lmap == I_UTM      ) .or. &
            (lmap == I_METERS   ) ) .and. &
          (lat0 == NOT_SET_R .or. lon0 == NOT_SET_R)) then
    nError   = UK_ERROR
    eRoutine = 'check_haz'
    eMessage = 'Must define map reference location for a UTM' &
                   //' or Cartesian run when doing a hazard area run'
    eInform  = 'File='//TRIM(file_inp)
    go to 9999
  end if
end if

if (sl_calm <= 0.) then
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'SL_CALM must be greater than 0'
  eInform  = 'Resetting to 1000m'
  call WarningMessage(0,.false.)
  if (nError /= NO_ERROR) go to 9999
  sl_calm = 1000.
end if

if (uu_calm < 0.) then
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'UU_CALM cannot be less than 0'
  eInform  = 'Resetting to (0.5m/s)**2'
  call WarningMessage(0,.false.)
  if (nError /= NO_ERROR) go to 9999
  uu_calm = 0.25
end if

if (sltrop <= 0.) then
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'SLTROP must be greater than 0'
  eInform  = 'Resetting to 10m'
  call WarningMessage(0,.false.)
  if (nError /= NO_ERROR) go to 9999
  sltrop = 10.
end if

if (wwtrop < 0.) then
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'WWTROP cannot be less than 0'
  eInform  = 'Resetting to (0.1m/s)**2'
  call WarningMessage(0,.false.)
  if (nError /= NO_ERROR) go to 9999
  wwtrop = 0.01
end if

if (epstrop < 0.) then
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'EPSTROP cannot be less than 0'
  eInform  = 'Resetting to 4e-4 m**2/s**3'
  call WarningMessage(0,.false.)
  if (nError /= NO_ERROR) go to 9999
  epstrop = 4.e-4
end if

!------ Check start year-month-day

if (year_start /= NOT_SET_I .and. month_start /= NOT_SET_I &
         .and. day_start /= NOT_SET_I) then
  lymd = .true.
  if (year_start < 50) then
    year_start = year_start + 2000
  else if (year_start < 100) then
    year_start = year_start + 1900
  end if
  jul_start = julian_day(month_start,day_start,year_start)

else if (year_start /= NOT_SET_I .or. month_start /= NOT_SET_I &
              .or.day_start /= NOT_SET_I) then
  nError   = UK_ERROR
  eMessage = 'Must set YEAR_START, MONTH_START and DAY_START'
  go to 9997

else

  lymd = .false.

end if

!------ Check time zone

if (tzone /= DEF_VAL_R .and. tzone /= NOT_SET_R) then
  call fix_tzone(tzone)
else if (((lmap == I_CARTESIAN) .or. (lmap == I_UTM)) .and. &
            lon0 /= NOT_SET_R) then
  tzone = float(int(lon0+7.5)/15)
  if (lon0 < -7.5) tzone = tzone - 1.
  write(lun_log,*)'**** Reset time zone to ',tzone
else if (lmap == I_LATLON .and. xmin /= DEF_VAL_R .and. &
                                     xmax /= DEF_VAL_R) then
  xmid  = 0.5*(xmin+xmax)
  tzone = float(int(xmid+7.5)/15)
  if (xmid < -7.5) tzone = tzone - 1.
  write(lun_log,*)'**** Reset time zone to ',tzone
else
  tzone = DEF_VAL_R
end if

!------ Initialize run time, puffs and other parameters

npaux    = 1
npuf     = 0
t        = 0.
t_save   = 0.
metname  = ' '
t_old_r  = -999.
surface  = .false.

!------ Initialize time if restarting from another project

if (file_rst /= ' ') then
  if (time_rst < 0.) then
    nError = UK_ERROR
    eMessage = 'Restart time cannot be negative'
    go to 9997
  end if
  t = time_rst
  t_save = time_rst
  t_old_r = time_rst
end if

!------ Read material description data

call get_matdef

!------ Close input file

close(lun_inp,iostat=ios)

!------ Set buoyant gas flags

call set_buoy_flags

!------ adjust puff auxiliary count for buoyant gases

if (buoy_gas) then
  do i = 1,ntypp
    typeID(i)%npaux = typeID(i)%npaux + NAUX_BUOY
  end do
end if

!------ Read multi-component data, if necessary

if (multicomp) then
  call read_mc(0)
else
  nstage   = 1
  istage   = 1
  nspecies = 0
  nspectot = 0
  ncorrm   = 0
  ncorrt   = 0
  nzenith  = 0
  nkrad    = 0
  nreacts  = 0
  nslow    = 0
  nfast    = 0
  nequilibrium = 0
  neq_s    = 0
  nambient = 0
  nvoc     = 0
  lamb3d   = .false.
  amb_file = NOT_SET_C
  lstep_amb = .false.
end if

surface  = ntyps > 0
dose     = ntypd > 0

hascal = .false.

call set_depint

9999    continue

return

!------ set namelist error and go to return

9997    continue
eRoutine = 'set_param'
if(eInform == char(0))eInform  = 'File='//TRIM(file_inp)
go to 9999

end

subroutine set_vdep
!*******************************************************************************
!
! FUNCTION:  Initialize deposition velocities for particle type materials
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!       get_puff_material       put_puff_material              IsParticle
!                   ufall
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- LOCALS

type ( part_material ) pmatpart

type ( puff_material ) pmat

equivalence( pmat, pmatpart )

integer i, j, ityp, jtyp, nsg

real rp(13)
real dbp(13)
real rhodst, dlb, dub, dm, vmin, vbar, vmax, fmin, fbar, fmax, fsum
real vdep, vd2, svdep, db_dust, rate, dbpx, ufall

logical off_end, less_than
logical IsParticle

rp(1)  =  0.0005e-6
rp(2)  =  0.0010e-6
rp(3)  =  0.0025e-6
rp(4)  =  0.0050e-6
rp(5)  =  0.0100e-6
rp(6)  =  0.0250e-6
rp(7)  =  0.0500e-6
rp(8)  =  0.2500e-6
rp(9)  =  0.5000e-6
rp(10) =  2.5000e-6
rp(11) =  5.0000e-6
rp(12) = 25.0000e-6
rp(13) = 50.0000e-6

dbp(1)  = 5.1e-6
dbp(2)  = 1.3e-6
dbp(3)  = 2.1e-7
dbp(4)  = 5.2e-8
dbp(5)  = 1.3e-8
dbp(6)  = 2.4e-9
dbp(7)  = 6.7e-10
dbp(8)  = 6.3e-11
dbp(9)  = 2.8e-11
dbp(10) = 4.9e-12
dbp(11) = 2.4e-12
dbp(12) = 1.e-20
dbp(13) = 1.e-20

do ityp = 1,ntypm
  jtyp = material(ityp)%ioffp + 1
  call get_puff_material(jtyp,material,mat_aux,pmat)
  if(IsParticle(material(ityp)%icls))then
    nsg    = pmatpart%nsg
    rhodst = pmatpart%rho
    do i = 1,nsg
      jtyp = material(ityp)%ioffp + i
      call get_puff_material(jtyp,material,mat_aux,pmat)

      if(pmatpart%dbar <= 0.0)then
        pmatpart%dbar = 0.5*(pmatpart%dmin+pmatpart%dmax)
      end if

      dlb =  pmatpart%dmin
      dub =  pmatpart%dmax
      dm  =  pmatpart%dbar

      if(dlb > 0.0)then
        vmin = ufall(rhoair,rhodst,rmuair,dlb)
        vbar = ufall(rhoair,rhodst,rmuair,dm)
        vmax = ufall(rhoair,rhodst,rmuair,dub)
        fmin = 1.0/dlb
        fbar = 1.0/dm
        fmax = 1.0/dub
        fsum = fmin + fbar + fmax
        vdep = (fmin*vmin    + fbar*vbar    + fmax*vmax)/fsum
        vd2  = (fmin*vmin**2 + fbar*vbar**2 + fmax*vmax**2)/fsum
        svdep= sqrt(abs(vd2 - vdep**2))
        if (svdep > vdep/3.0) then
          svdep = vdep/3.0
          write(lun_log,*,err=9998) &
                                'Resetting svdep for group ',ityp,i
        end if
        pmatpart%vd    = vdep
        pmatpart%sigvd = svdep

        off_end   = .false.
        less_than = .true.
        j = 2
        do while (less_than)
          if (2.0*rp(j) >= dm) then
            less_than = .false.
          else
            j = j + 1
            if (j > 13) then
              off_end = .true.
              less_than = .false.
            end if
          end if
        end do

        if (off_end) then
          db_dust = 1.e-20
        else
          rate = (0.5*dm-rp(j-1))/(rp(j)-rp(j-1))
          dbpx = alog(dbp(j-1)) + rate*(alog(dbp(j))-alog(dbp(j-1)))
          dbpx = exp(dbpx)
          db_dust = min(dbpx,rnu)
        end if
        pmatpart%diff = db_dust
        write(lun_log,*,err=9998)'Particle(diam,rho,diff,vs):', &
                           ityp,i,dm*1.e6,rhodst,db_dust,vdep,svdep
      else
        pmatpart%vd    = 0.
        pmatpart%sigvd = 0.
        pmatpart%diff  = 1.e-20
      end if
      call put_puff_material(jtyp,material,mat_aux,pmat)
    end do

  end if

end do

9999    continue

return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'set_vdep'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

end

subroutine create_output
!*******************************************************************************
!
! FUNCTION:  Initialize output files
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               write_prj
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc
use multcomp_inc
use amb_data

implicit none

! --- LOCALS

integer i,is,ios

!------ write out saved common variables to project file

call write_prj
if (nError /= NO_ERROR) go to 9999

!------ open puff file

open(unit=lun_puf,file=file_puf,status='NEW',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'create_output'
  eMessage = 'Error opening SCICHEM puff file'
  eInform  = 'File='//TRIM(file_puf)
  eAction  = 'Make sure file does not already exist'
  go to 9999
end if

! Following commented out for PiG version
!if (multicomp) then
!  if (lstep_amb3d) then
!    open(unit=lun_amb,file=file_amb,status='NEW',form='UNFORMATTED', &
!         iostat=ios)
!    if (ios /= 0) then
!      nError   = OP_ERROR
!      eRoutine = 'create_output'
!      eMessage = 'Error opening SCICHEM amb file'
!      eInform  = 'File='//TRIM(file_amb)
!      eAction  = 'Make sure file does not already exist'
!      go to 9999
!    end if
!    write(lun_amb,iostat =ios)nta,nxa,nya,nza,dxa,dya,dza,x0a,y0a, &
!          lamb_metgrid,mc_units,(species(is)%name,is = 1,nspecies),&
!         (ambient(is)%name,is = 1,nambient)
!    if (lamb_metgrid) then
!      write(lun_amb,iostat =ios)(zbw(i), i= 1,nza)
!    end if
  
!    if (ios /= 0) then
!      nError   = WR_ERROR
!      eRoutine = 'create_output'
!      eMessage = 'Error writing header in SCICHEM amb file'
!      eInform  = 'File='//TRIM(file_amb)
!      eAction  = ''
!      go to 9999
!    end if
!  end if
!end if

9999    continue

return

end

subroutine fix_tzone(tz)
!*******************************************************************************
!
! FUNCTION:  Changes local time of 0000Z (UTC) to time zone
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
 
! --- ARGUMENTS
 
! --- LOCALS

implicit none

real tz    !Time zone


if ( abs(tz) > 12.0 ) then
  tz = tz - sign(24.,tz)
end if

return

end

subroutine init_param
!*******************************************************************************
!
! FUNCTION:  Initialize run parameters (set defaults)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         init_matl_param       init_status_flags
!
! REVISION HISTORY: 
!
! 26 Nov 2003 : Add initialisation of zi_bl to zero in revision 1.14. -BC
! 31 Jan 2007 : Use constants from constants_fd. -BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

!------ code version

iversion  = iversion_code

!------ puff variable names

names(1)  = 'X   '
names(2)  = 'Y   '
names(3)  = 'Z   '
names(4)  = 'SXX '
names(5)  = 'SXY '
names(6)  = 'SXZ '
names(7)  = 'SYY '
names(8)  = 'SYZ '
names(9)  = 'SZZ '
names(10) = 'AXX '
names(11) = 'AXY '
names(12) = 'AXZ '
names(13) = 'AYY '
names(14) = 'AYZ '
names(15) = 'AZZ '
names(16) = 'DET '
names(17) = 'C   '
names(18) = 'CC  '
names(19) = 'XUC '
names(20) = 'XVC '
names(21) = 'YVC '
names(22) = 'YVSC'
names(23) = 'YVBC'
names(24) = 'ZWC '
names(25) = 'WC  '
names(26) = 'CCB '
names(27) = 'SI  '
names(28) = 'SI2 '
names(29) = 'SV  '
names(30) = 'SR  '
names(31) = 'CFO '
names(32) = 'ZI  '
names(33) = 'ZC  '
names(34) = 'U   '
names(35) = 'V   '
names(36) = 'W   '
names(37) = 'TYPE'
names(38) = 'NEXT'
names(39) = 'PREV'
names(40) = 'GRID'
names(41) = 'TLEV'
names(42) = 'NXTL'
names(43) = 'IAUX'

!------ turbulence parameters

sle_fac   = 1.4

!------ puff parameters

simrge    = 1.25
rrmrge    = 0.75  !  0.1
asplt     = 0.75  !  0.45                ! must be < 0.999
asplt2    = asplt*asplt
aspltc    = 1.0-asplt2
dxsplt    = 0.5
dzsplt    = 0.5
fac_rfl   = 3.0

!------ default parameter values

rmuair  = 1.6e-5
rhoair  = 1.2
rnu     = rmuair/rhoair

cmin    = 0.
delmin  = DEF_VAL_R
grdmin  = 0.
z_dosage = 0.
delx2   = 0.
delz2   = 0.
title   = 'SCICHEM'
lmap    = I_LATLON
create  = .false.
hascal  = .false.
dynamic = .false.
dense_gas = .false.
buoy_gas  = .false.
lsplitz = .false.
static  = .true.
run_mode= STANDARD_MODE
t_avg   = DEF_VAL_R
mgrd    = 2
hres    = DEF_VAL_R
vres    = DEF_VAL_R
xmin    = DEF_VAL_R
xmax    = DEF_VAL_R
ymin    = DEF_VAL_R
ymax    = DEF_VAL_R
zmax    = DEF_VAL_R
utm_zone = NOT_SET_I
nx      = 0
ny      = 0
nz      = 0
dxg     = 0.
dyg     = 0.
dzg     = 0.
xref    = 0.
yref    = 0.
lon0    = NOT_SET_R
lat0    = NOT_SET_R
nzbl    = 11
zi_bl   = 0.
epstrop = 4.0e-4
sltrop  = 10.
wwtrop  = 0.01
uu_calm = 0.25
sl_calm = 1000.
audit_class   = 'Not specified'
audit_analyst = 'Anonymous'
audit_space = ' '
smpfile = ' '
lzi_prj  = .false.
lter_prj = .false.
lswift_prj = .true.
lwash    = .true.
hazard   = IHAZ_OFF

!------ material definitions

call init_matl_param
call init_status_flags

return

end

subroutine init_matl_param
!*******************************************************************************
!
! FUNCTION:  Set material defaults
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

implicit none

! --- LOCALS

integer i

do i = 1,MAXCLS                         !namec no longer used
  namec(i) = ' '
end do

ntypm   = 0
ntypp   = 0
ntyps   = 0
ntypd   = 0
nmaux   = 0
nsrcaux = 0

return

end

subroutine init_status_flags
!*******************************************************************************
!
! FUNCTION: 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none
 
! --- LOCALS

integer i

do i = 1,TIME_STATUS_ARRAY_SIZE
  time_status(i) = 1
end do

do i = 1,DOMAIN_STATUS_ARRAY_SIZE
  domain_status(i) = 1
end do

return

end

subroutine puff_rst
!*******************************************************************************
!
! FUNCTION:  Restart a puff file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!    restart_static_puffs          start_met_grid          init_interp_2d
!               interp_2d        get_topog_interp               get_topog
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc
use multcomp_inc
use cont_rel_inc

implicit none

! --- LOCALS

integer nblank, nch, nchp, ios, ipuf, i
integer nxb_old, nyb_old, nxyb_old

real tx, ttt, dum, dum2, h,amb(MAX_MC)
real dxb_old, dyb_old
real xb_old(MAXXB), yb_old(MAXYB)
real zi_old(MAX2D), hs_old(MAX2D)
real xmin_old, xmax_old, ymin_old, ymax_old
real hmin_old 

logical lter_old, ldum

logical interp
integer n_rst,nn
common / rst_cmn / interp, n_rst
save   / rst_cmn /

!------ open puff file from old project

nch  = nblank(file_rst)
nchp = nblank(path_rst)

file_tmp = path_rst(1:nchp)//file_rst(1:nch)//'.puf'

open(unit=lun_tmp,file=file_tmp,status='OLD',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Error opening puff file for restarting'
  eInform  = 'File='//TRIM(file_tmp)
  eAction  = 'Make sure file exists'
  go to 9999
end if

!------ read first timebreak

read(lun_tmp,err=9998) tx
n_rst = 1

!------ use last timebreak if time_rst = DEF_VAL_R; otherwise check first
!       time and use if restart time is equal or earlier

if (time_rst == DEF_VAL_R) then
1         read(lun_tmp,iostat=ios) tx
  if (ios == -1) then
    backspace(lun_tmp)
    go to 22
  else if (ios /= 0) then
    go to 9998
  end if
  n_rst = n_rst + 1
  go to 1
else
  if (tx >= time_rst) go to 22
end if

!------ read until time on file is greater than restart time

do while (tx < time_rst)
  ttt = tx
  read(lun_tmp,iostat=ios) tx
  if (ios == -1) then
    backspace(lun_tmp)
    go to 22
  else if (ios /= 0) then
    go to 9998
  end if
  n_rst = n_rst + 1
end do

!------ use closest timebreak

if (abs(ttt-time_rst) < abs(tx-time_rst)) then
  backspace(lun_tmp)
  n_rst = n_rst - 1
end if

22      backspace(lun_tmp)
read(lun_tmp,err=9998) tx,npuf &
                ,(puff(ipuf),ipuf=1,npuf) &
                ,npaux,(puff_aux(i),i=1,npaux-1) &
                ,nn,(amb(i),i=1,nn) &
                ,nxb_old,nyb_old,dxb_old,dyb_old &
                ,(xb_old(i),i=1,nxb_old),(yb_old(i),i=1,nyb_old) &
                ,(zi_old(i),i=1,nxb_old*nyb_old) &
                ,ncrel,(c_plen(i),i=1,ncrel) &
                ,nsrcaux,(src_aux(i),i=1,nsrcaux)

close (lun_tmp)

if(multicomp)then
  if (.not. lamb3d) then
    do i = 1, nspectot
      ps(i)%a = amb(i)
    end do
  end if
end if

!------ Clear all static puffs for restart

call restart_static_puffs

!------ check old grid dimensions

if (nxb_old > MAXXB .or. nyb_old > MAXYB &
                                .or. nxb*nyb > MAX2D) then
  nError   = SZ_ERROR
  eRoutine = 'puff'
  eMessage = 'Met grid dimensions too large for restarting'
  eInform  = 'File='//TRIM(file_tmp)
  go to 9999
end if

!------ build horizontal grid for obs met w/o terrain

call start_met_grid(nxb_old)

!------ check time

if (abs(tx-time_rst) > 1.) then      ! 1 sec. tolerance
  nError = UK_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Restart time mismatch'
  write(eInform,100)  time_rst,tx
100       format('Specified time =',f8.1,' Nearest time =',f8.1)
  eAction  = 'Restart Puff File='//TRIM(file_tmp)
  go to 9999
end if

!------ interpolate zi if necessary

interp = nxb_old /= nxb .or. nyb_old /= nyb
interp = dxb_old /= dxb .or. dyb_old /= dyb .or. interp
interp = xb_old(1) /= xb(1) .or. yb_old(1) /= yb(1) .or. interp

if (interp) then
  call init_interp_2d
  ios = 1
  call interp_2d(xb_old,nxb_old,yb_old,nyb_old,zi_old,nxb_old, &
                      xb,nxb,yb,nyb,zi_bl,nxb,ios)
  if (ios < 0) then
    nError = UK_ERROR
    eRoutine = 'interp_2d'
    eMessage = 'Error interpolating mixed-layer height for restart'
    write(eInform,'(a,i3)') 'ios = ',ios
    go to 9999
  end if
else
  do i = 1,nxyb
    zi_bl(i) = zi_old(i)
  end do
end if

lzi_prj = .true.

!------ open old project file

file_tmp = path_rst(1:nchp)//file_rst(1:nch)//'.prj'

open(unit=lun_tmp,file=file_tmp,status='OLD',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Error opening project file for restarting'
  eInform  = 'File='//TRIM(file_tmp)
  eAction  = 'Make sure file exists and is a valid project file'
  go to 9999
end if

!------ skip over useless records; read old grid and lter

read(lun_tmp,iostat=ios,err=9997)               ! header
read(lun_tmp,iostat=ios,err=9997)               ! constants
read(lun_tmp,iostat=ios,err=9997)               ! turbulence
read(lun_tmp,iostat=ios,err=9997)               ! puff parameters

read(lun_tmp,iostat=ios,err=9997) xmin_old,xmax_old,ymin_old,ymax_old

read(lun_tmp,iostat=ios,err=9997)            ! materials
read(lun_tmp,iostat=ios,err=9997)            ! surface materials

read(lun_tmp,iostat=ios,err=9997) dum,ldum,lter_old

read(lun_tmp,iostat=ios,err=9997)            !ext

!------ check if surface files must be interpolated due to different domains

interp = xmin /= xmin_old .or. ymin /= ymin_old
interp = xmax /= xmax_old .or. ymax /= ymax_old .or. interp

!------ read old terrain and adjust puff heights

if (lter_old) then

  nxyb_old = nxb_old*nyb_old
  if (multicomp) read(lun_tmp,iostat=ios,err=9997)  ! multicomponent
  read(lun_tmp,iostat=ios,err=9997)            ! grid

  read(lun_tmp,iostat=ios,err=9997) (hs_old(i),i=1,nxyb_old), (dum,i=1,nxyb_old) &
                                 ,(dum,i=1,nxyb_old),hmin_old

  do ipuf = 1,npuf
    call get_topog_interp(xb_old,nxb_old,yb_old,nyb_old,hs_old, &
                      puff(ipuf)%xbar,puff(ipuf)%ybar,h)
    puff(ipuf)%zbar = max(puff(ipuf)%zbar-h,0.)
  end do

end if

!------ adjust puff heights for current terrain

if (lter) then
  do ipuf = 1,npuf
    call get_topog(puff(ipuf)%xbar,puff(ipuf)%ybar,h,dum,dum2)
    puff(ipuf)%zbar = puff(ipuf)%zbar + h
  end do
end if

close(lun_tmp,iostat=ios,err=9997)

9999    continue

close(lun_tmp,iostat=ios)

return

!------ set puff fileread error and return

9998    continue

nError = RD_ERROR
eRoutine = 'puff_rst'
eMessage = 'Error reading puff file for restarting'
eInform  = 'File='//TRIM(file_tmp)
go to 9999

!------ set project file read error and return

9997    continue

nError = RD_ERROR
eRoutine = 'puff_rst'
eMessage = 'Error reading project file for restarting'
eInform  = 'File='//TRIM(file_tmp)
go to 9999

end

subroutine get_topog_interp(xb,nxb,yb,nyb,hs,x,y,h)
!*******************************************************************************
!
! FUNCTION: Find topography height at puff location
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
 
integer nxb, nyb  !Number of grid points in met grid

real xb(nxb), yb(nyb) !x and y coord of met grid
real hs(nxb*nyb)      !New terrain array
real x,y          !Puff x and y
real h            !New terrain height at x and y

! --- LOCALS

real dxb, dyb
real ratx, rxm1, raty, rym1
real cc1, cc2, cc3, cc4
real frac

integer ig, jg, i

dxb = xb(2) - xb(1)
dyb = yb(2) - yb(1)

frac = (x-xb(1))/dxb
frac = max(frac,0.0)
frac = min(frac,float(nxb-1)-1.e-3)
ig   = int(frac) + 1
ratx = frac - float(ig-1)
rxm1 = 1.0 - ratx

frac = (y-yb(1))/dyb
frac = max(frac,0.0)
frac = min(frac,float(nyb-1)-1.e-3)
jg   = int(frac) + 1
raty = frac - float(jg-1)
rym1 = 1.0 - raty

cc1 = rxm1*rym1
cc2 = rxm1*raty
cc3 = ratx*raty
cc4 = ratx*rym1

i   = (jg-1)*nxb + ig
h   = cc1*hs(i) + cc2*hs(i+nxb) + cc3*hs(i+nxb+1) + cc4*hs(i+1)

return
end

subroutine init_diagnostics
!*******************************************************************************
!
! FUNCTION:  Initialize diagnostics for chemistry runs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           init_dmp_file
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use error_inc
use files_inc
use diagnostics
use multcomp_inc

implicit none

! --- PARAMETERS
 
integer, parameter :: NDIAG = 9

! --- LOCALS

character*10, dimension(:), allocatable :: nametmp
integer is, nvar, nch, ios, ioff

nvar =  NDIAG*(nspectot+1) + 2
allocate(nametmp(nvar), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = 'output'
  eMessage = 'Insufficient memory to allocate temporary name array'
  write(eInform,*) 'Bytes requested =',(NDIAG*nspectot+1)*4
  go to 9999
end if

ngd_chem = 0
nbad_chem = 0

ndump = 0
ndump_old = 0

do is = 1, MAX_MC
  emiss(is)  = 0.
  emiss_old(is)  = 0.
  statics(is)= 0.
  statics_old(is)= 0.
  bndry(is)  = 0.
  bndry_old(is)  = 0.
  trans(is)  = 0.
  trans_old(is)  = 0.
  ddepos(is)  = 0.
  ddepos_old(is)  = 0.
  wdepos(is)  = 0.
  wdepos_old(is)  = 0.
  chem(is)   = 0.
  chem_old(is)   = 0.
  remvd(is)  = 0.
  ppmfac(is)  = 0.
end do

!------ open diagnostics file

call open_new_mcfile(lun_dgn,file_dgn)
if (nError /= NO_ERROR) go to 9999

nametmp(1) = 't        '
nametmp(2) = 'ndump    '
ioff = 2
do is = 1,nspectot
  nch = 8
  write(nametmp(is+ioff+(is-1)*(NDIAG-1)),45)species(is)%name(1:nch)
  write(nametmp(is+ioff+1+(is-1)*(NDIAG-1)),50)species(is)%name(1:nch)
  write(nametmp(is+ioff+2+(is-1)*(NDIAG-1)),55)species(is)%name(1:nch)
  write(nametmp(is+ioff+3+(is-1)*(NDIAG-1)),60)species(is)%name(1:nch)
  write(nametmp(is+ioff+4+(is-1)*(NDIAG-1)),65)species(is)%name(1:nch)
  write(nametmp(is+ioff+5+(is-1)*(NDIAG-1)),70)species(is)%name(1:nch)
  write(nametmp(is+ioff+6+(is-1)*(NDIAG-1)),75)species(is)%name(1:nch)
  write(nametmp(is+ioff+7+(is-1)*(NDIAG-1)),80)species(is)%name(1:nch)
  write(nametmp(is+ioff+8+(is-1)*(NDIAG-1)),85)species(is)%name(1:nch)
end do
write(nametmp(nspectot+ioff+9+(nspectot-1)*(NDIAG-1)),90)'e_TRAC'
write(nametmp(nspectot+ioff+10+(nspectot-1)*(NDIAG-1)),90)'s_TRAC'
write(nametmp(nspectot+ioff+11+(nspectot-1)*(NDIAG-1)),90)'b_TRAC'
write(nametmp(nspectot+ioff+12+(nspectot-1)*(NDIAG-1)),90)'t_TRAC'
write(nametmp(nspectot+ioff+13+(nspectot-1)*(NDIAG-1)),90)'d_TRAC'
write(nametmp(nspectot+ioff+14+(nspectot-1)*(NDIAG-1)),90)'c_TRAC'
write(nametmp(nspectot+ioff+15+(nspectot-1)*(NDIAG-1)),90)'a_TRAC'
write(nametmp(nspectot+ioff+16+(nspectot-1)*(NDIAG-1)),90)'r_TRAC'
write(nametmp(nspectot+ioff+17+(nspectot-1)*(NDIAG-1)),90)'p_TRAC'
45 format('e_',a8)
50 format('s_',a8)
55 format('b_',a8)
60 format('t_',a8)
65 format('d_',a8)
70 format('c_',a8)
75 format('a_',a8)
80 format('r_',a8)
85 format('p_',a8)
90 format(a6)

write(lun_dgn) (nametmp(is),is=1,nvar)

call init_dmp_file

9999 continue

deallocate(nametmp)

return
end

subroutine init_depos2d
!*******************************************************************************
!
! FUNCTION:  Initialize 2D deposition arrays and files (for chemistry runs)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
! 18-MAY-2005  Check for write error - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc
use common_met
use multcomp_inc

implicit none

! --- LOCALS

integer is, ij, ios

!do ij = 1, MAX2D
!  do is = 1, MAX_MC
!    ddepos2d(ij,is) = 0.
!    wdepos2d(ij,is) = 0.
!  end do
!end do

ddepos2d = 0.
wdepos2d = 0.

!------ open deposition files

call open_new_mcfile(lun_ddp,file_ddp)
if (nError /= NO_ERROR) go to 9999

write(lun_ddp,IOSTAT=ios) nspecies, MAX2D
if (ios == 0) write(lun_ddp,IOSTAT=ios) (species(is)%name,is=1,nspecies)
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'init_depos2d'
  eMessage = 'Error writing deposition file header'
  write(eInform,'(a,2i10)') 'Species, rec size',nspecies,MAX2D
  eAction  = 'File='//TRIM(file_dep)
  go to 9999
end if

call open_new_mcfile(lun_wdp,file_wdp)
if (nError /= NO_ERROR) go to 9999

write(lun_wdp,IOSTAT=ios) nspecies, MAX2D
if (ios == 0) write(lun_wdp,IOSTAT=ios) (species(is)%name,is=1,nspecies)
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'init_depos2d'
  eMessage = 'Error writing wet deposition file header'
  write(eInform,'(a,2i10)') 'Species, rec size',nspecies,MAX2D
  eAction  = 'File='//TRIM(file_dep)
  go to 9999
end if

9999 return
end
