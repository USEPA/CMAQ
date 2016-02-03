!*******************************************************************************
!$RCSfile: time_cnv.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine time_cnv(ti,local_out,lymd_out,to, &
                            hour,min,sec,year,month,day,string)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Convert "time since start" to local or UTC
!           give year, month, day, hour, min, sec if appropriate
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          year_month_day
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

real ti                  !Time since start
logical local_out        !Project is in local time
logical lymd_out         !Project has year, month and day specified
real to                  !Time including start time and time zone
integer hour, min, sec   !Current time information
integer year, month, day !Current time information
character*(*) string     !Current time information written to character 
                         !to be displayed by model

! --- LOCALS

real tx, rmin, rsec
character*1   symbol, day_thing
character*3   name_month(12)
data name_month / 'JAN','FEB','MAR','APR','MAY','JUN', &
                       'JUL','AUG','SEP','OCT','NOV','DEC' /


to = ti + 0.5

if (local_out) then
  if (local) then
    to = to/3600. + tstart
  else
    to = to/3600. + tstart + tzone
  end if
  symbol = 'L'
else
  if (local) then
    to = to/3600. + tstart - tzone
  else
    to = to/3600. + tstart
  end if
  symbol = 'Z'
end if

if (lymd_out) then

  if (year_start == NOT_SET_I .or. month_start == NOT_SET_I .or. &
                                     day_start == NOT_SET_I) then
    nError   = UK_ERROR
    eRoutine = 'time_cnv'
    eMessage = 'Cannot output time in year-month-day format'
    eInform  = 'Start time must be set in year-month-day format'
    go to 9999
  end if

  call year_month_day(to,year_start,month_start,day_start &
                          ,tx,year      ,month      ,day      )

  if (year >= 2000) then
    year = year - 2000
  else if (year >= 1900) then
    year = year - 1900
  end if

  hour = int(tx)
  rmin = 60.*(tx-float(hour))
  min  = int(rmin)
  rsec = 60.*(rmin-float(min))
  sec  = int(rsec)

  write(string,100) day,name_month(month),year,hour,min,sec,symbol
100       format(i2.2,'-',a3,'-',i2.2,' ',i2.2,':',i2.2,':',i2.2,a1)

else

  year  = NOT_SET_I
  month = NOT_SET_I

  day  = int(to/24.)
  tx   = to - float(day)*24.
  hour = int(tx)
  rmin = 60.*(tx-float(hour))
  min  = int(rmin)
  rsec = int(60.*(rmin-float(min)))
  sec  = nint(rsec)

  if (day == 0) then
    day_thing = '-'
  else
    day_thing = '+'
  end if

  if (day < 10) then
    write(string,101) day_thing,day,hour,min,sec,symbol
  else if (day < 100) then
    write(string,102) day_thing,day,hour,min,sec,symbol
  else if (day < 1000) then
    write(string,103) day_thing,day,hour,min,sec,symbol
  end if

101       format('DAY',a1,i1.1,' ',i2.2,':',i2.2,':',i2.2,a1)
102       format('DAY',a1,i2.2,' ',i2.2,':',i2.2,':',i2.2,a1)
103       format('DAY',a1,i3.3,' ',i2.2,':',i2.2,':',i2.2,a1)

end if

9999    continue

return

end

subroutine time_message(cmsg,tx)
!*******************************************************************************
!
! FUNCTION: Write the time message
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                time_cnv                c_format
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

real tx              !Time since start
character*80 cmsg    !Time in year, month, day, hour, min, sec
                     !or just time (if not lymd)

! --- LOCALS

character*32 ctem

logical local_out, lymd_out

integer hour, min, sec, year, month, day
integer nch, nch1, nblank

real to


local_out = local
lymd_out  = lymd
call time_cnv(tx,local_out,lymd_out,to, &
                             hour,min,sec,year,month,day,ctem)
cmsg = ctem
nch1 = nblank(cmsg)
if(tx == 0. .or. abs(tx) > 1800.)then
  call c_format(tx/3600.,nch,ctem)
  write(cmsg,'(a)') cmsg(1:nch1)//' ('//ctem(1:nch)//' hr)'
else if(abs(tx) > 60.)then
  call c_format(tx/60.,nch,ctem)
  write(cmsg,'(a)') cmsg(1:nch1)//' ('//ctem(1:nch)//' min)'
else
  call c_format(tx,nch,ctem)
  write(cmsg,'(a)') cmsg(1:nch1)//' ('//ctem(1:nch)//' sec)'
end if

return
end

subroutine year_month_day(t,yr,mnth,day,to,yro,mntho,dayo)
!*******************************************************************************
!
! FUNCTION: Set year, month and day
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              julian_day               leap_year
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

real t, to               !Time in hours
integer yr, mnth ,day    !Known year, month and day
integer yro, mntho, dayo !To be determined

! --- LOCALS

integer julian_day, jul, julo
integer iday, days
logical leap_year, leap

integer nday(12)

data nday / 0,31,59,90,120,151,181,212,243,273,304,334 /


iday = int(t/24.)
to   = t - 24.*float(iday)
if(to < 0.)then
  iday = iday - 1
  to = to + 24.
end if

jul  = julian_day(mnth,day,yr)
julo = jul + iday
yro = yr

leap = leap_year(yro)
if(leap)then
  days = 366
else
  days = 365
end if

do while (julo <= 0)
  julo = julo + days
  yro  = yro - 1
  leap = leap_year(yro)
  if(leap)then
    days = 366
  else
    days = 365
  end if
end do

leap = leap_year(yro)
if(leap)then
  days = 366
else
  days = 365
end if

do while (julo > days)
  julo = julo - days
  yro  = yro + 1
  leap = leap_year(yro)
  if(leap)then
    days = 366
  else
    days = 365
  end if
end do

do mntho = 12,1,-1
  if(mntho >= 3 .and. leap)then
    days = nday(mntho) + 1
  else
    days = nday(mntho)
  end if
  if (julo > days) go to 3
end do

3       continue

dayo = julo - days

return
end
