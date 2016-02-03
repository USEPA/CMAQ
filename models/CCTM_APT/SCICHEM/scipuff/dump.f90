!*******************************************************************************
!$RCSfile: dump.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine dump
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Enable dump of surface file information
!            Used only when debugging (not actually called by main code)
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

implicit none

! --- LOCALS

logical ldump

common / dmpcmn / ldump
save   / dmpcmn /

ldump = .true.

return
end

subroutine write_dump
!*******************************************************************************
!
! FUNCTION:  Dump surface file information to file_dmp
!            Used only when debugging (not actually called by main code)
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
use common_met
use common_srf
use files_inc

implicit none

! --- LOCALS

character*8  cdep,cdos

integer year_cur, month_cur, day_cur
integer year_enx, month_enx, day_enx
integer ios, nch, ndp, nds, nchm, nch_sfc, nblank

real    tc_hr, te_hr, tr_hr, tcur, tenx

open(unit=lun_dmp,file=file_dmp,status='UNKNOWN',iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'write_dump'
  eMessage = 'Error opening SCICHEM dump output file'
  eInform  = 'File='//TRIM(file_dmp)
  go to 9999
end if

nch = nblank(name)
if(surface)then
  write(cdep,'(i7)')srfdep%ncells
  ndp = 7
else
  cdep = ' off'
  ndp = 4
end if
if(dose)then
  write(cdos,'(i7)')srfdos%ncells
  nds = 7
else
  cdos = ' off'
  nds = 4
end if

tc_hr = t/3600.
te_hr = tend_r/3600.
tr_hr = trel/3600.

write(lun_dmp,1100,iostat=ios)name(1:nch)
if (ios /= 0) go to 9998

if (lymd) then
  call year_month_day(tc_hr,year_start,month_start,day_start &
                          ,tcur,year_cur  ,month_cur  ,day_cur)
  call year_month_day(te_hr,year_start,month_start,day_start &
                          ,tenx,year_enx  ,month_enx  ,day_enx)

  write(lun_dmp,1101,iostat=ios)tcur,year_cur,month_cur,day_cur, &
                  tenx,year_enx,month_enx,day_enx
  if (ios /= 0) go to 9998
else
  write(lun_dmp,1102,iostat=ios)tc_hr,te_hr
  if (ios /= 0) go to 9998
end if

nchm = nblank(file_met)
write(lun_dmp,1103,iostat=ios)file_met(1:nchm)
if (ios /= 0) go to 9998
if (lsfc) then
  nch_sfc = nblank(file_sfc)
  write(lun_dmp,1104,iostat=ios)file_sfc(1:nch_sfc)
end if
write(lun_dmp,1105,iostat=ios)npuf,ncrel,mxtlev,cdep(1:ndp), &
                      cdos(1:nds),tr_hr
if (ios /= 0) go to 9998

close(lun_dmp,iostat=ios)

9999    continue

close(lun_dmp,iostat=ios)

return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'write_dump'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

!------ format statements

1100    format(' Run Progress : Run Name      = ',a)
1101    format('                Current Time   =',1p,e11.4,/, &
            '                Year-Month-Day =',i5,'-',i2.2,'-',i2.2,/, &
            '                Stop Time      =',1p,e11.4,/, &
            '                Year-Month-Day =',i5,'-',i2.2,'-',i2.2)
1102    format('                Current Time  =',1p,e11.4,/, &
            '                Stop    Time  =',1p,e11.4)
1103    format('                Met. File     = ',a)
1104    format('                Sfc. File     = ',a)
1105    format('                No. Puffs     =',i7,/, &
            '                No. C-sources =',i7,/, &
            '                Max. Time Lev =',i7,/, &
            '                No. Dep Cells =',a,/, &
            '                No. Dos Cells =',a,/, &
            '                Next Release  =',1p,e11.4)

end
