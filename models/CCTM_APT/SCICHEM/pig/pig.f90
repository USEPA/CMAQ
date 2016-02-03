subroutine output_all(cgrid)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Write to all output files
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          write_progress                  output               write_prj
!             end_scichem                    exit
!          System subroutine date_and_time ()
!
! REVISION HISTORY: 
!
!     30-JUL-2001: Replaced fdate by date_and_time (BC)
!     Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_srf
use files_inc
use interface_definitions, only: OUTPUT

implicit none

! --- ARGUMENTS
 
REAL :: CGRID( :,:,:,: ) !3D ambient concentration from host model

! --- LOCALS

character*80 cmsg, cmsg2, cmsg3
character*24   clock
CHARACTER*10 :: date,time,zone
INTEGER      :: values(8)

integer ios

cmsg=char(0)
cmsg2 = char(0)
cmsg3 = 'Writing puff file'
call write_progress(cmsg,cmsg2,cmsg3)

call output(cgrid)
if (nError /= NO_ERROR) go to 9999

cmsg3 = 'Writing project file'
call write_progress(cmsg,cmsg2,cmsg3)

call write_prj
if (nError /= NO_ERROR) go to 9999
write(lun_log,*)'finished writing project file'
call flush(lun_log)

cmsg3 = ' '
call write_progress(cmsg,cmsg2,cmsg3)

!------ Get date and time

CALL date_and_time(date,time,zone,values)
WRITE(clock,100)values(2),values(3),values(1),values(5),values(6),values(7)
100 format (I2.2,'/',I2.2,'/',I4.4,' at ',I2.2,':',I2.2,':',I2.2)
write(lun_log,111,iostat=ios)'Output completed at t =',t/3600., &
              'hrs. with NCREL = ',ncrel,' and NPUFF = ', &
              npuf,' on ',clock
111     format(a,f7.2,a,i4,a,i5,a,a)
call flush(lun_log)

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'output_all'
  eMessage = 'Error writing SCICHEM log file'
  eInform  = 'File='//TRIM(file_log)
  go to 9999
end if

return

9999    continue

call end_scichem

call exit

end

SUBROUTINE end_scichem
!******************************************************************************
!
! FUNCTION:  Report any errors, close all files and end SCICHEM
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            report_error
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_srf
use files_inc

implicit none

! --- LOCALS

integer ios, nch, nblank

if (nError /= NO_ERROR) then
  call report_error
end if

eRoutine = 'end_scichem'
write(eInform,'(a,I5)')  'No. Puffs = ',npuf
write(eMessage,'(a,f7.2,a)')'Stopping run at Time = ',t/3600.,' hrs'

if(nError == S0_ERROR)then
  write(lun_log,'(a)',iostat=ios)'************HALT***************'
else if(nError == WN_ERROR)then
  write(lun_log,'(a)',iostat=ios)'**********WARNING**************'
else if(nError /= NO_ERROR)then
  write(lun_log,'(a)',iostat=ios)'***********ERROR***************'
else
  write(lun_log,'(a)',iostat=ios)'************DONE***************'
end if

nch = nblank(eRoutine)
if (nch > 1) then
  write(lun_log,'(a)',iostat=ios)TRIM(eRoutine)
end if
write(lun_log,'(a)',iostat=ios)TRIM(eMessage)
write(lun_log,'(a)',iostat=ios)TRIM(eInform)
nch = nblank(eAction)
if (nch > 1) then
  write(lun_log,'(a)',iostat=ios)TRIM(eAction)
end if
write(lun_log,'(a)',iostat=ios)'*********************************'

if (ios /= 0) go to 9998

9997  continue
open(unit=lun_err,file=file_err,status='UNKNOWN',iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'end_scichem'
  eMessage = 'Error opening SCICHEM error file'
  eInform  = 'File='//TRIM(file_err)
else
  write(lun_err,'(i6,a)',iostat=ios)nError,' :SCICHEM Exit Status'
  write(lun_err,'(a)',iostat=ios)TRIM(eRoutine)
  write(lun_err,'(a)',iostat=ios)TRIM(eMessage)
  write(lun_err,'(a)',iostat=ios)TRIM(eInform)
  write(lun_err,'(a)',iostat=ios)TRIM(eAction)
  close(unit=lun_err,iostat=ios)
end if

close(unit=lun_puf,iostat=ios)
close(unit=lun_prj,iostat=ios)
close(unit=lun_dos,iostat=ios)
close(unit=lun_dep,iostat=ios)
close(unit=lun_ddp,iostat=ios)
close(unit=lun_wdp,iostat=ios)
close(unit=lun_dgn,iostat=ios)
close(unit=lun_dmp,iostat=ios)
close(unit=lun_amb,iostat=ios)
close(unit=lun_tmp,iostat=ios)
close(unit=lun_inp,iostat=ios)
close(unit=lun_scn,iostat=ios)
close(unit=lun_mcw,iostat=ios)
close(unit=lun_msc,iostat=ios)
close(unit=lun_met,iostat=ios)
close(unit=lun_sfc,iostat=ios)
close(unit=lun_ter,iostat=ios)
close(unit=lun_smp,iostat=ios)
close(unit=lun_rad,iostat=ios)
close(unit=lun_ATP,iostat=ios)
close(unit=lun_log,iostat=ios)

return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'end_scichem'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9997

end
