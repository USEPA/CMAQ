subroutine init_pig_input(stdate,sttime,tendhr,cgrid)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  More set-up for a SCICHEM run (called by initial)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               SplitName          write_progress              init_clock
!            read_control               set_mxgrd               old_start
!              update_met              init_time1              init_time2
!               new_start            set_end_timeinit_wash !does not need
!                  output
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!    Updated October 2005 to read Cl2 and HCl concs for Hg chemistry (PKK, AER)
!    Updated June 2006 to use compile flag to activate/deactivate Hg (PKK, AER)
!    Updated May 2007 to read Br and BrO concs for Hg chemistry (PKK, AER)
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_srf
use files_inc
use multcomp_inc

implicit none

! --- ARGUMENTS
 
integer stdate    !Start date, coded YYYYDDD
integer sttime    !Start time, coded HHMMSS
real    tendhr    !Final hour of simulation (relative to start)
REAL :: CGRID( :,:,:,: ) !3D ambient concentrations from host model

! --- LOCALS

character*128 cmsg, cmsg2, cmsg3

integer ios, i, j, nch, nblank

!------ get name

call SplitName(file_inp,name,cmsg)
i = index(name,'.')
if (i > 1) then
  name = name(1:i-1)
else
  nError   = IV_ERROR
  eRoutine = 'init_pig_input'
  eMessage = 'Invalid project name '//TRIM(name)
  eInform  = 'File='//TRIM(file_inp)
  go to 9999
end if

!------ initialize

open(unit=lun_inp,file=file_inp,status='OLD',action="read",iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'init_pig_input'
  eMessage = 'Error opening SCICHEM main input file'
  eInform  = 'File='//TRIM(file_inp)
  eAction  = 'Make sure file exists'
  go to 9999
end if

nch = nblank(name)
cmsg=name(1:nch)//' : Initializing SCICHEM'
cmsg2=char(0)
cmsg3=char(0)
call write_progress(cmsg,cmsg2,cmsg3)
if(nError /= NO_ERROR) go to 9999

call init_clock
if(nError /= NO_ERROR) go to 9999

call read_control(lun_inp)
if(nError /= NO_ERROR) go to 9997

call set_mxgrd(99)

!------ open/create/read files

if(restart)then

  cmsg=char(0)
  cmsg2='Reading project data files'
  cmsg3=char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR) go to 9999
  create = .false.

  call old_start(stdate,sttime)
  if (nError /= NO_ERROR) go to 9999

  tend_hr = tendhr + t/3600.

  cmsg=char(0)
  cmsg2='Initializing data'
  cmsg3=char(0)
  call write_progress(cmsg,cmsg2,cmsg3)

else

  cmsg=char(0)
  cmsg2='Creating project data files'
  cmsg3='Reading input data'
  call write_progress(cmsg,cmsg2,cmsg3)

  tend_hr = tendhr

  call init_time1(stdate,sttime)

  call init_time2

  if(nError /= NO_ERROR) go to 9997

  call new_start
  if (nError /= NO_ERROR) go to 9999

  cmsg=char(0)
  cmsg2=char(0)
  cmsg3='Initializing data'
  call write_progress(cmsg,cmsg2,cmsg3)

end if

!------ End time

call set_end_time
if (nError /= NO_ERROR) go to 9999

!------ Set washout time scale if precipitation

if (lwash) then
  call init_wash !does not need init_met first
end if

!------ Setup cloudshine functions

lcldshine = .false.

if (.not.restart) then

!------ check if restarting from existing project

  if (file_rst /= ' ') then

    nError = IV_ERROR
    eMessage = 'No restarts from other projects'
    go to 9999

  end if

  if (surface .or. dose) then
    nError = IV_ERROR
    eMessage = 'No surface or dose for PiG'
    go to 9999
  end if

end if

!------ Stop if only creating files

if(create)then

  cmsg=char(0)
  cmsg2=char(0)
  cmsg3='Writing output file(s)'
  call write_progress(cmsg,cmsg2,cmsg3)
  call output(cgrid)
  if (nError /= NO_ERROR) go to 9999

  write(lun_log,111,err=9998)'Setup completed at t =',t/3600.,'hrs.'
111 format(a,f7.2,a,i4,a,i5,a,a)

  go to 9999

end if

ldecay = .false.

9999  return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'init_pig_input'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

!------ set namelist error and go to return

9997    continue
eRoutine = 'init_pig_input'
eInform  = 'File='//TRIM(file_inp)
go to 9999

end

subroutine write_init_info
!*******************************************************************************
!
! FUNCTION:  Write initialization information to log file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          write_progress                  xsetun
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_srf
use files_inc
use multcomp_inc

implicit none

! --- LOCALS

character*128 cmsg, cmsg2, cmsg3

integer i

!------ Report status

if(npuf > 0)then
  if(restart)then
    write(lun_log,111,err=9998)'Run  restarted   at t =',t/3600., &
                     'hrs. with NCREL = ',ncrel,' and NPUFF = ',npuf
111         format(a,f7.2,a,i4,a,i5,a,a)
  else
    write(lun_log,111,err=9998)'Run    started   at t =',t/3600., &
                     'hrs. with NCREL = ',ncrel,' and NPUFF = ',npuf
  end if
end if

cmsg=char(0)
write(cmsg2,'(a,i5,a)')'Beginning run with',npuf,' puffs'
cmsg3=char(0)
call write_progress(cmsg,cmsg2,cmsg3)

!------ Redirect LSODE messages to log file

if(multicomp) then
  call xsetun(lun_log)
end if

9999 return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'write_init_info'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

end

subroutine init_clock
!*******************************************************************************
!
! FUNCTION:  Initialize clock
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: System subroutine date_and_time()
!
! REVISION HISTORY: 
!
! 30-JUL-2001: Replaced fdate by date_and_time (BC)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- LOCALS

CHARACTER*10 :: date,time,zone
INTEGER      :: values(8)
character*24 :: clock
integer      :: ios

if (nError /= NO_ERROR) go to 9999

!------ Get date and time

CALL date_and_time(date,time,zone,values)

WRITE(clock,100)values(2),values(3),values(1),values(5),values(6),values(7)
100 format (I2.2,'/',I2.2,'/',I4.4,' at ',I2.2,':',I2.2,':',I2.2)

write(lun_log,*,iostat=ios)'Starting run setup on ',clock
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'init_clock'
  eMessage = 'Error writing SCICHEM log file'
  eInform  = 'File='//TRIM(file_log)
  go to 9999
end if

!------ Set Defaults

trel    = -1.e+36
ncrel   = 0
ncaux   = 1

9999    continue

return

end

subroutine init_time1(stdate,sttime)
!*******************************************************************************
!
! FUNCTION:  Initialize the SCIPUFF start date from the host model's
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!      get_md_from_julian
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer stdate, sttime   !Host model start date and start time

!   STDATE is stored in the form   YYYYDDD = YEAR*1000  +  DAY
!   STTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS

! --- LOCALS

integer iyr, jday, imo, iday, ihr, imin, isec

iyr = stdate/1000
jday = mod(stdate,1000)

ihr = sttime/10000
imin = mod(sttime/100,100)
isec = mod(sttime,100)

write(6,*) 'stdate,sttime:',stdate,sttime
write(6,*) 'yr,jday,hr,min,sec:',iyr,jday,ihr,imin,isec

tstart      = float(ihr) + float(imin)/60. + float(isec)/3600.
year_start  = iyr
call get_md_from_julian(jday,iyr,imo,iday)
month_start = imo
day_start   = iday
write(6,*) 'scipuff setting:',year_start,month_start,day_start,tstart

local       = .false.
tzone       = DEF_VAL_R

return

end

subroutine init_time2
!*******************************************************************************
!
! FUNCTION: Initialize the SCIPUFF end date
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

!------ SCICHEM will use "tendhr" instead of these end time parameters

tend        = NOT_SET_R
year_end    = NOT_SET_I
month_end   = NOT_SET_I
day_end     = NOT_SET_I

return

end

subroutine set_end_time
!*******************************************************************************
!
! FUNCTION:  Set the SCICHEM end time
!            Uses either year_end, month_end, tend, etc.  OR  tendhr
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          year_month_day          WarningMessage              julian_day
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- LOCALS

integer jul_end, jyy, julian_day

logical lymd_end

real tx, tend_hx

lymd_end = year_end /= NOT_SET_I .or. month_end /= NOT_SET_I .or. &
                 day_end /= NOT_SET_I

if (lymd_end .and. .not.lymd) then
  nError   = RD_ERROR
  eRoutine = 'set_end_time'
  eMessage = 'Start/end time specifications not consistent'
  eInform  = 'File='//TRIM(file_inp)
  go to 9999
end if

if (lymd) then

  if (lymd_end) then

    if (year_end == NOT_SET_I) then
      year_end = year_start
    end if

    if (month_end == NOT_SET_I) then
      month_end = month_start
    end if

    if (day_end == NOT_SET_I) then
      day_end = day_start
    end if

    if (tend == NOT_SET_R) then
      nError   = RD_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set end time'
      eInform  = 'File='//TRIM(file_inp)
      go to 9999
    end if

    if (year_end < 50) then
      year_end = year_end + 2000
    else if (year_end < 100) then
      year_end = year_end + 1900
    end if

    if (year_end < year_start) then
      nError   = RD_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Cannot set end year before start year'
      eInform  = 'File='//TRIM(file_inp)
      go to 9999
    end if

    jul_end = julian_day(month_end,day_end,year_end)
    if (jul_end == -999) then
      nError   = UK_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Incorrect end day, month, year'
      eInform  = 'File='//TRIM(file_inp)
      go to 9999
    end if

    if(year_end /= year_start) then
      do jyy = year_start,year_end-1
          if (mod(jyy,4) == 0) then
             jul_end = jul_end + 366
          else
             jul_end = jul_end + 365
          end if
      end do
    end if

    tend_hx = tend - tstart + (jul_end - jul_start)*24.

  else

    if (tend /= NOT_SET_R) then
      tend_hx = tend - tstart
    else
      if (tend_hr == NOT_SET_R) then
        nError   = WN_ERROR
        eRoutine = 'set_end_time'
        eMessage = 'Must set run time (duration in hrs) or ' &
                               //'end time'
        go to 9999
      else if (tend_hr <= 0.) then
        nError   = WN_ERROR
        eRoutine = 'set_end_time'
        eMessage = 'Must set run time (duration in hrs) > 0 '
        go to 9999
      end if
      tend    = tstart + tend_hr
      tend_hx = NOT_SET_R
    end if

    call year_month_day(tend,year_start,month_start,day_start &
                            ,tx  ,year_end  ,month_end  ,day_end  )

    tend = tx

  end if

else                    ! .not. lymd

  if (tend /= NOT_SET_R) then
    tend_hx = tend - tstart
  else
    if (tend_hr == NOT_SET_R) then
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set run time (duration in hrs) or ' &
                             //'end time'
      go to 9999
    else if (tend_hr <= 0.) then
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set run time (duration in hrs) > 0 '
      go to 9999
    end if
    tend    = tstart + tend_hr
    tend_hx = NOT_SET_R
  end if

end if

if (tend_hx /= NOT_SET_R) then
  if (tend_hr /= NOT_SET_R) then
    if (abs(tend_hr-tend_hx) > 0.01) then
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Run time (duration in hrs) inconsistent with ' &
                             //'end time'
      eInform  = 'Run time will be ignored'
      call WarningMessage(0,.true.)
      if (nError /= NO_ERROR) go to 9999
    end if
  end if
  tend_hr = tend_hx
end if

tend_r = tend_hr*3600.

if (tend_r < t) then
  nError   = UK_ERROR
  eRoutine = 'set_end_time'
  eMessage = 'End time is less than current time'
  eInform  = 'File='//TRIM(file_inp)
  go to 9999
end if

9999    continue

return

end
