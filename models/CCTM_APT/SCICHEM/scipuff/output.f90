!*******************************************************************************
!$RCSfile: output.f90,v $
!$Revision: 1.6 $
!$Date: 2010/08/31 20:57:32 $
!******************************************************************************
subroutine output(cgrid)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: This subroutine writes to the output files             
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                time_cnv         setall_amb_keqm                 get_amb
!                  fflush                  get_mc          write_progress
!          write_dmp_file          write_amb_data
!
! REVISION HISTORY: 
! 18-MAY-2005  Change deposition output to block<=10000 and check IOSTAT - RIS
! 18-MAY-2005  Change min to mins to avoid conflict with intrinsic function - RIS
! 22-MAR-2006  Move ndump write message inside multcomp(as undefined otherwise) - BC
! Aug 2010 : Include following update made by PKK, AER
!             a) Updated for Oct. 2004 CMAQ release - BC(SAGE-MGT)
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc
use multcomp_inc
use cont_rel_inc
use diagnostics
use amb_data
use interface_definitions, only: get_amb, setall_amb_keqm, write_amb_data

implicit none

! --- ARGUMENTS
 
REAL :: CGRID( :,:,:,: )  !3-D ambient species concentrations

! --- LOCALS
integer  j ,k , is

integer ipuf, i, ios, hour, mins, sec, year, month, day, isp, ij
integer ndat, nsp, nwrt, iwrt
integer, dimension(:,:),allocatable :: ntmp
real,    dimension(:),allocatable :: tmp

real t0, amb_tmp(MAX_MC), xamb, yamb, zamb
real hp,dum1,dum2
real xmap, ymap
type(puff_mc) pm

character*32 ctem
character*80 cmsg,cmsg2,cmsg3

! ---- allocate temporary arrays

allocate(ntmp(MAX2D*nspectot,2), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = 'output'
  eMessage = 'Insufficient memory to allocate temporary array ntmp'
  write(eInform,*) 'Bytes requested =',MAX2D*nspectot*8
   go to 9999
end if

allocate(tmp(MAX2D*nspectot), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = 'output'
  eMessage = 'Insufficient memory to allocate temporary array tmp'
  write(eInform,*) 'Bytes requested =',MAX2D*nspectot*4
   go to 9999
end if

!---------- get time

if (lymd) then
  call time_cnv(t,local,lymd,t0,hour,mins,sec,year,month,day,ctem)
  t0 = t/3600.
  write(lun_log,1,err=9998)'Writing puff output at t =',t0, &
                              'hrs. (',TRIM(ctem),')'
1       format(a,f7.2,a,a,a)
else
  t0 = t/3600.
  write(lun_log,1,err=9998)'Writing puff output at t =',t0,'hrs.'
end if

if(multicomp) then
  if (ndump > 0) &
       write(lun_log,*) 'Total number of puffs dumped to host =',ndump

  if (.not. lamb3d) then
    if (.not. lstep_amb) then
      !----update ambient equilibrium species if not stepping the ambient
      if (.not. create) then
        call setall_amb_keqm(cgrid)
        if (nError /= NO_ERROR)  go to 9999
      end if
    end if
  else
    xamb = 0.5*(xmin+xmax)  !get ambient at center of domain
    yamb = 0.5*(ymin+ymax)
    if (lter) then
      call get_topog(xamb,yamb,hp,dum1,dum2)
      zamb = zref + hp
    else 
      zamb = zref
    end if
    if ( .not. create) then
       call mapfac( xamb, yamb, xmap , ymap )
       call get_met(xamb, yamb, zref, 0.0, 0.0, xmap, ymap, 1, .false.)
    end if
    call get_amb(xamb, yamb, zamb, t, cgrid)
  end if
end if
do i = 1, nspectot
  amb_tmp(i) = ps(i)%a
end do

!-----  write puff information

write(lun_puf,iostat=ios) t,npuf &
                ,(puff(ipuf),ipuf=1,npuf) &
                ,npaux,(puff_aux(i),i=1,npaux-1) &
                ,nspectot,(amb_tmp(i),i=1,nspectot) &
                ,nxb,nyb,dxb,dyb &
                ,(xbl(i),i=1,nxb),(ybl(i),i=1,nyb) &
                ,(zi_bl(i),i=1,nxb*nyb) &
                ,ncrel,(c_plen(i),i=1,ncrel) &
                ,nsrcaux,(src_aux(i),i=1,nsrcaux)

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'output'
  eMessage = 'Error writing puff file'
  write(eInform,'(a,f10.3)') 'Time=',t0
  eAction  = 'File='//TRIM(file_puf)
  go to 9999
end if
!#ifndef INTEL
!call fflush(lun_puf)
!#endif

do i = 1, nspectot+1
  active(i) = 0.
end do

do i = 1, npuf
  if (puff(i)%idtl >= 0) then
    call get_mc(puff(i),pm)
    do isp = 1, nspectot
      active(isp) = active(isp) + pm%mc(isp)
    end do
    active(nspectot+1) = active(nspectot+1) + puff(i)%c
  end if
end do

if(multicomp) then
  cmsg='Writing diagnostics file'
  cmsg2 = char(0)
  cmsg3 = char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR) go to 9999

  write(lun_dgn) t/3600.,ndump,(emiss(i),statics(i),bndry(i),trans(i),&
                 ddepos(i),wdepos(i),chem(i),active(i),remvd(i),&
                 ppmfac(i),i=1,nspectot+1)
!#ifndef INTEL
!  call fflush(lun_dgn)
!#endif

  do i = 1, nspectot+1
    emiss_current(i) = emiss(i) - emiss_old(i)
    statics_current(i) = statics(i) - statics_old(i)
    bndry_current(i) = bndry(i) - bndry_old(i)
    trans_current(i) = trans(i) - trans_old(i)
    ddepos_current(i) = ddepos(i) - ddepos_old(i)
    wdepos_current(i) = wdepos(i) - wdepos_old(i)
    chem_current(i) = chem(i) - chem_old(i)

    emiss_old(i) = emiss(i)
    statics_old(i) = statics(i)
    bndry_old(i) = bndry(i)
    trans_old(i) = trans(i)
    ddepos_old(i) = ddepos(i)
    wdepos_old(i) = wdepos(i)
    chem_old(i) = chem(i)
  end do

  ndump_current = ndump - ndump_old
  ndump_old = ndump

  cmsg='Writing deposition files'
  cmsg2 = char(0)
  cmsg3 = char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR) go to 9999

  ndat = 0
  do i = 1, nspecies
    do ij = 1,nxyb
      if (ddepos2d(ij,i) /= 0.) then
        ndat = ndat + 1
        tmp(ndat) = ddepos2d(ij,i)
        ntmp(ndat,1) = ij
        ntmp(ndat,2) = i
      end if
    end do
  end do
  write(lun_ddp) t/3600.,ndat
  nwrt = ndat
  is   = 0
  do while (nwrt > 0 .and. ios == 0)
    iwrt = min(nwrt,10000)
    write(lun_ddp,iostat=ios) (ntmp(i,1),ntmp(i,2),tmp(i),i=is+1,is+iwrt)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'output'
    eMessage = 'Error writing dry deposition file'
    write(eInform,'(a,f10.3,i10)') 'Time, rec size',t0,ndat
    eAction  = 'File='//TRIM(file_ddp)
    go to 9999
  end if
!#ifndef INTEL
!  call fflush(lun_ddp)
!#endif

  ndat = 0
  do i = 1, nspecies
    do ij = 1,nxyb
      if (wdepos2d(ij,i) /= 0.) then
        ndat = ndat + 1
        tmp(ndat) = wdepos2d(ij,i)
        ntmp(ndat,1) = ij
        ntmp(ndat,2) = i
      end if
    end do
  end do
  write(lun_wdp,iostat=ios) t/3600.,ndat
  nwrt = ndat
  is   = 0
  do while (nwrt > 0 .and. ios == 0)
    iwrt = min(nwrt,10000)
    write(lun_wdp,iostat=ios) (ntmp(i,1),ntmp(i,2),tmp(i),i=is+1,is+iwrt)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'output'
    eMessage = 'Error writing wet deposition file'
    write(eInform,'(a,f10.3,i10)') 'Time, rec size',t0,ndat
    eAction  = 'File='//TRIM(file_wdp)
    go to 9999
  end if
!#ifndef INTEL
!  call fflush(lun_wdp)
!#endif

!------ Write out good and bad chemistry steps

  write(lun_log,90)'Number of successful chemistry steps: ', ngd_chem
  write(lun_log,90)'Number of    aborted chemistry steps: ', nbad_chem
  write(cmsg,90)   'Number of successful chemistry steps: ', ngd_chem
  write(cmsg2,90)  'Number of    aborted chemistry steps: ', nbad_chem
  90 format(A40,i6)
  cmsg3 = char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR) go to 9999
  ngd_chem = 0
  nbad_chem = 0

!------ Write "dump" file (for plume-in-grid applications)

  call write_dmp_file
  if (nError /= NO_ERROR) go to 9999
  
  if (lwrt_amb) then
    cmsg='Writing ambient file'
    cmsg2 = char(0)
    cmsg3 = char(0)
    call write_progress(cmsg,cmsg2,cmsg3)
    if(nError /= NO_ERROR) go to 9999
    call write_amb_data(cgrid)
  end if

! Following commented out for PiG version:
 
! if (lstep_amb3d) then
!   cmsg='Writing ambient data file'
!   cmsg2 = char(0)
!   cmsg3 = char(0)
!   call write_progress(cmsg,cmsg2,cmsg3)
!   if(nError /= NO_ERROR) go to 9999
!   nsp = nspecies + nambient
!   write(lun_amb) t/3600., &
!        ((((amb3d(i,j,k,1,is),i=1,nxa),j=1,nya),k=1,nza),is=1,nsp)
!#ifndef INTEL
!   call fflush(lun_amb)
!#endif   
! end if
 
end if

9999    continue

if (allocated(ntmp)) deallocate(ntmp)
if (allocated(tmp))  deallocate(tmp)

return

!------ set log write error and  go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'output'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

end
