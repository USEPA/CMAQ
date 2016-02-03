!*******************************************************************************
!$RCSfile: restart.f90,v $
!$Revision: 1.5 $
!$Date: 2010/09/03 16:01:22 $
!*******************************************************************************
! Aug 2010: Include updates made by PKK, AER for reading dry and wet dep files 
!           instead of total dep file - BC(SAGE-MGT)
subroutine old_start(stdate,sttime)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Restart an old project
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             read_output            read_surface                 zero_ip
!                  set_ip              init_time2           read_end_time
!                set_tlev
!
! REVISION HISTORY: 
!
! 18 MAY 2005 : Avoid using uninitialized release puffs for restart calculations - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use files_inc

implicit none

! --- ARGUMENTS
 
integer stdate, sttime  !used in plume-in-grid version only (start date and start time)

! --- LOCALS

real delt_old
!integer i, j, ilev, mcrel
integer i, j, ilev

!-----  open output file & read first records

call read_output(stdate,sttime)
if (nError /= NO_ERROR) go to 9999

!-----  open/read surface dose file

srfdep%ipnam = 0
srfdep%ipdat = 0
if (surface)then
  srfdep%ipgrd = 1
  call read_surface( lun_ddp, file_ddp, srfdep )
  if (nError /= NO_ERROR) go to 9999
else
  srfdep%ipgrd = 0
  srfdep%nvart = 0
end if

srfdos%ipdat = srfdep%ipdat + srfdep%nvart
srfdos%ipnam = srfdep%ipnam + srfdep%nvart
if (dose   )then
  srfdos%ipgrd = srfdep%ipgrd + 1
  call read_surface( lun_dos, file_dos, srfdos )
  if (nError /= NO_ERROR) go to 9999
else
  srfdos%ipgrd = 0
  srfdos%nvart = 0
end if

!------ Zero ipgrd

call zero_ip

!------ Set ipgrd

call set_ip(1,npuf)
if (nError /= NO_ERROR) go to 9999

!------ Read new time input

delt_old = delt

call init_time2

call read_end_time(lun_inp)
if(nError /= NO_ERROR) then
  eRoutine = 'old_start'
  eInform  = 'File='//TRIM(file_inp)
  go to 9999
end if

!-----  Temporarily set ncrel to zero, since release puffs have not been initialized

!mcrel = ncrel
!ncrel = 0

!-----  Check for change in time step

if (delt_old < delt) then
  ilev = 0
  do while (delt_old < delt .and. ilev < MAXTLV)
    ilev = ilev + 1
    delt_old = 2.0*delt_old
  end do
  do i = 1,npuf
    if (puff(i)%idtl >= 0.) then
      puff(i)%idtl = puff(i)%idtl + ilev
      if (puff(i)%idtl > MAXTLV) go to 9998
    end if
  end do
!  do j = 1,ncrel
!    i = MAXPUF + 2*(1 - j)
!    puff(i)%idtl = puff(i)%idtl + ilev
!    if (puff(i)%idtl > MAXTLV) go to 9998
!  end do
end if

!-----  Set puff and release time step lists

call set_tlev

!ncrel = mcrel

9999    continue

return

9998    nError   = SZ_ERROR
eRoutine = 'old_start'
eMessage = 'Too many time-step refinement levels'
write(eInform,'(a,i5)') 'Maximum number is ',MAXTLV
eAction = 'Use a smaller maximum time step'
go to 9999

end

subroutine read_output(stdate,sttime)
!******************************************************************************
!
! FUNCTION:   Read old output files
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!       init_status_flags                read_prj                get_trst
!                init_mcp          write_progress        restart_dmp_file
!    restart_static_puffs
!
! REVISION HISTORY: 
! 18-MAY-2005  Change deposition output to block<=10000 and check IOSTAT - RIS
!  May 2006, PK, AER: Use separate files for a restarted run, not
!                     cumulative files (to reduce file size)
! Aug 2010   : Include following update made by PKK, AER
!              a) Read dry and wet dep files instead of total dep file - BC(SAGE-MGT)
!******************************************************************************

! --- MODULES
 
use common_puf
use common_met
use files_inc
use multcomp_inc
use cont_rel_inc
use diagnostics

implicit none

! --- ARGUMENTS
 
integer stdate, sttime  !used in plume-in-grid version only (start date and start time)

! --- PARAMETERS
 
integer, parameter :: NDIAG = 9

! --- LOCALS

integer ios, i, ipuf, nn, ij, ndat, is, iwrt, nwrt
real      amb(MAX_MC), tem
character*80 cmsg,cmsg2,cmsg3
character*10, dimension(:), allocatable :: nametmp
integer nvar
integer, dimension(:,:),allocatable :: ntmp
real,    dimension(:),allocatable :: tmp

real, parameter :: small = 1.E-5

!------ initialize operational status flags

call init_status_flags

!------ read saved common blocks on project file

call read_prj
if (nError /= 0) go to 9999

call get_trst(stdate,sttime)

!------ read restart puff file

open(unit=lun_puf,file=file_pufr,status='OLD',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'read_output'
  eMessage = 'Error opening SCICHEM restart puff file'
  eInform  = 'File='//TRIM(file_pufr)
  eAction  = 'Make sure file exists'
  go to 9999
end if

!-----  find last dump and read

tem = -999.
do while (tem*(1.+ small) < trst)
  read(lun_puf,end=9998,err=9998) tem,npuf &
               ,(puff(ipuf),ipuf=1,npuf) &
               ,npaux,(puff_aux(i),i=1,npaux-1) &
               ,nn,(amb(i),i=1,nn) &
               ,nxbl,nybl,dxbl,dybl &
               ,(xbl(i),i=1,nxbl),(ybl(i),i=1,nybl) &
               ,(zi_bl(i),i=1,nxbl*nybl) &
               ,ncrel,(c_plen(i),i=1,ncrel) &
               ,nsrcaux,(src_aux(i),i=1,nsrcaux) 
end do

close(unit=lun_puf,iostat=ios)

! open & write current time stamp to new puff file

open(unit=lun_puf,file=file_puf,status='NEW',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'read_output'
  eMessage = 'Error opening SCICHEM output puff file'
  eInform  = 'File='//TRIM(file_puf)
  eAction  = 'Make sure file does not already exist'
  go to 9999
end if

write(lun_puf,iostat=ios) tem,npuf &
               ,(puff(ipuf),ipuf=1,npuf) &
               ,npaux,(puff_aux(i),i=1,npaux-1) &
               ,nn,(amb(i),i=1,nn) &
               ,nxbl,nybl,dxbl,dybl &
               ,(xbl(i),i=1,nxbl),(ybl(i),i=1,nybl) &
               ,(zi_bl(i),i=1,nxbl*nybl) &
               ,ncrel,(c_plen(i),i=1,ncrel) &
               ,nsrcaux,(src_aux(i),i=1,nsrcaux) 
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'read_output'
  eMessage = 'Error writing puff file'
  write(eInform,'(a,f10.3)') 'Time=',tem/3600.
  eAction  = 'File='//TRIM(file_puf)
  go to 9999
end if

if(multicomp)then
  if (.not. lamb3d) then
    do i = 1, nspectot
      ps(i)%a = amb(i)
    end do
  end if
  if (trst > 0.0)then
    tambient = trst - 0.5*delt 
  else
    tambient = 0.0
  end if
  call init_mcp
  if(nError /= NO_ERROR)go to 9999

  !------ open & read diagnostics restart file

  cmsg='Restarting diagnostics file'
  cmsg2 = char(0)
  cmsg3 = char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR)go to 9999

  nvar =  NDIAG*(nspectot+1) + 2
  allocate(nametmp(nvar), stat = ios)
  if (ios /= 0) then
    nError   = SZ_ERROR
    eRoutine = 'read_output'
    eMessage = 'Insufficient memory to allocate temporary name array'
    write(eInform,*) 'Bytes requested =',(NDIAG*nspectot+1)*4
    go to 9999
  end if

  call open_old_mcfile(lun_dgn,file_dgnr)
  if (nError /= NO_ERROR) go to 9999

  tem = -999.
  if (iversion >= 1800) then
  
    read(lun_dgn,end=9997,err=9997) (nametmp(i),i=1,nvar)
    do while (tem*(1.+small) < trst/3600.)
      read(lun_dgn,end=9997,err=9997) tem, ndump,(emiss(i),statics(i),&
           bndry(i),trans(i),ddepos(i),wdepos(i),chem(i),active(i),remvd(i),&
           ppmfac(i),i=1,nspectot+1)
    end do

  else if (iversion >= 1219) then
  
    read(lun_dgn,end=9997,err=9997)
    do while (tem*(1.+small) < trst/3600.)
      read(lun_dgn,end=9997,err=9997) tem, ndump,(emiss(i),statics(i),&
           bndry(i),trans(i),ddepos(i),chem(i),active(i),remvd(i),&
           ppmfac(i),i=1,nspectot+1)
    end do

    wdepos = 0.

  else

    read(lun_dgn,*,end=9997,err=9997)
    do while (tem*(1.+small) < trst/3600.)
      read(lun_dgn,*,end=9997,err=9997) tem, ndump,(emiss(i),statics(i),&
           bndry(i),trans(i),ddepos(i),chem(i),active(i),remvd(i),&
           ppmfac(i),i=1,nspectot+1)
    end do

    wdepos = 0.

  end if

  do i = 1, nspectot + 1
    emiss_old(i) = emiss(i)
    statics_old(i) = statics(i)
    bndry_old(i) = bndry(i)
    trans_old(i) = trans(i)
    ddepos_old(i) = ddepos(i)
    wdepos_old(i) = wdepos(i)
    chem_old(i) = chem(i)
  end do

  ndump_old = ndump

  close(unit=lun_dgn,iostat=ios)

!------ open and write current time stamp to new diagnostics file

  call open_new_mcfile(lun_dgn,file_dgn)
  if (nError /= NO_ERROR) go to 9999
  write(lun_dgn) (nametmp(i),i=1,nvar)
  write(lun_dgn) tem, ndump,(emiss(i),statics(i),&
           bndry(i),trans(i),ddepos(i),wdepos(i),chem(i),active(i),remvd(i),&
           ppmfac(i),i=1,nspectot+1)

  deallocate(nametmp)

!------ open & read deposition files

  cmsg='Restarting deposition files'
  cmsg2 = char(0)
  cmsg3 = char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR)go to 9999

!--- Dry deposition file
  call open_old_mcfile(lun_ddp,file_ddpr)
  if (nError /= NO_ERROR) go to 9999

  if (iversion >= 1219) then
    read(lun_ddp,end=9996,err=9996)
    read(lun_ddp,end=9996,err=9996)
  else
    read(lun_ddp,*,end=9996,err=9996)
    read(lun_ddp,*,end=9996,err=9996)
  end if

! ---- allocate temporary arrays

  allocate(ntmp(MAX2D*nspectot,2), stat = ios)
  if (ios /= 0) then
    nError   = SZ_ERROR
    eRoutine = 'read_output'
    eMessage = 'Insufficient memory to allocate temporary array ntmp'
    write(eInform,*) 'Bytes requested =',MAX2D*nspectot*8
     go to 9999
  end if

  allocate(tmp(MAX2D*nspectot), stat = ios)
  if (ios /= 0) then
    nError   = SZ_ERROR
    eRoutine = 'read_output'
    eMessage = 'Insufficient memory to allocate temporary array tmp'
    write(eInform,*) 'Bytes requested =',MAX2D*nspectot*4
     go to 9999
  end if

  tem = -999.
  do while (tem*(1.+small) < trst/3600.)
    do i = 1, MAX_MC 
      do ij = 1, MAX2D
        ddepos2d(ij,i) = 0.
      end do
    end do

    if (iversion >= 1900) then
      read(lun_ddp,end=9996,err=9996) tem,ndat
      nwrt = ndat
      is   = 0
      do while (nwrt > 0)
        iwrt = MIN(nwrt,10000)
        read(lun_ddp,end=9996,err=9996) (ij,i,ddepos2d(ij,i),nn=is+1,is+iwrt)
        backspace(lun_ddp)
        read(lun_ddp) (ntmp(nn,1),ntmp(nn,2),tmp(nn),nn=is+1,is+iwrt)
        nwrt = nwrt - iwrt
        is   = is   + iwrt
      end do
    else if (iversion >= 1219) then
      read(lun_ddp,end=9996,err=9996) tem,ndat
      read(lun_ddp) (ij,i,ddepos2d(ij,i),nn=1,ndat)
      backspace(lun_ddp)
      read(lun_ddp) (ntmp(nn,1),ntmp(nn,2),tmp(nn),nn=1,ndat)
    else  
      read(lun_ddp,*,end=9996,err=9996) tem,ndat
      read(lun_ddp,*) (ij,i,ddepos2d(ij,i),nn=1,ndat)
      backspace(lun_ddp)
      read(lun_ddp,*) (ntmp(nn,1),ntmp(nn,2),tmp(nn),nn=1,ndat)
    end if
  end do

  close(unit=lun_ddp,iostat=ios)

!------ open & write current time stamp to new dry deposition file

  call open_new_mcfile(lun_ddp,file_ddp)
  if (nError /= NO_ERROR) go to 9999

  write(lun_ddp) nspecies, MAX2D
  write(lun_ddp) (species(i)%name,i=1,nspecies)
  write(lun_ddp) tem,ndat
  nwrt = ndat
  is   = 0
  do while (nwrt > 0 .and. ios == 0)
    iwrt = min(nwrt,10000)
    write(lun_ddp,iostat=ios) (ntmp(i,1),ntmp(i,2),tmp(i),i=is+1,is+iwrt)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do

!--- Wet deposition file
  if (iversion >= 1800) then
    call open_old_mcfile(lun_wdp,file_wdpr)
    if (nError /= NO_ERROR) go to 9999
    read(lun_wdp,end=9995,err=9995)
    read(lun_wdp,end=9995,err=9995)

    tem = -999.
    do while (tem*(1.+small) < trst/3600.)

      wdepos2d = 0.

      read(lun_wdp,end=9995,err=9995) tem,ndat
      if (iversion >= 1900) then
        nwrt = ndat
        is   = 0
        do while (nwrt > 0)
          iwrt = MIN(nwrt,10000)
          read(lun_wdp,end=9996,err=9996) (ij,i,wdepos2d(ij,i),nn=is+1,is+iwrt)
          backspace(lun_wdp)
          read(lun_wdp) (ntmp(nn,1),ntmp(nn,2),tmp(nn),nn=is+1,is+iwrt)
          nwrt = nwrt - iwrt
          is   = is   + iwrt
        end do

      else

        read(lun_wdp) (ij,i,wdepos2d(ij,i),nn=1,ndat)
        backspace(lun_wdp)
        read(lun_wdp) (ntmp(nn,1),ntmp(nn,2),tmp(nn),nn=1,ndat)

      end if

    end do

    close(unit=lun_wdp,iostat=ios)

!------ open & write current time stamp to new wet deposition file

    call open_new_mcfile(lun_wdp,file_wdp)
    if (nError /= NO_ERROR) go to 9999

    write(lun_wdp) nspecies, MAX2D
    write(lun_wdp) (species(i)%name,i=1,nspecies)
    write(lun_wdp) tem,ndat
    nwrt = ndat
    is   = 0
    do while (nwrt > 0 .and. ios == 0)
      iwrt = min(nwrt,10000)
      write(lun_wdp,iostat=ios) (ntmp(i,1),ntmp(i,2),tmp(i),i=is+1,is+iwrt)
      nwrt = nwrt - iwrt
      is   = is   + iwrt
    end do
  end if

  if (allocated(ntmp)) deallocate(ntmp)
  if (allocated(tmp))  deallocate(tmp)

!------ Restart "dump" file (for plume-in-grid applications)

  call restart_dmp_file

!------ Initialize bad and good chemistry steps (zeroed on output)

  ngd_chem = 0
  nbad_chem = 0

end if

!------ Clear all static puffs for restart

call restart_static_puffs

t = trst

9999    continue

return

!------ set error finding timebreak on puff file and go to return

9998    continue
nError   = RD_ERROR
eRoutine = 'read_output'
eMessage = 'Error finding timebreak on puff file'
write(eAction,*) 'Time_rst = ',trst, 'Last puff time = ',tem
eInform  = 'File='//TRIM(file_puf)
go to 9999

9997    continue
nError   = RD_ERROR
eRoutine = 'read_output'
eMessage = 'Error finding timebreak on diagnostics file'
write(eAction,*) 'Time_rst = ',trst/3600., 'Last diagnostics time(hr) = ',tem
eInform  = 'File='//TRIM(file_dgn)
go to 9999

9996    continue
nError   = RD_ERROR
eRoutine = 'read_output'
if (iversion >= 1800) then
  eMessage = 'Error finding timebreak on dry deposition file'
else
  eMessage = 'Error finding timebreak on deposition file'
end if
write(eAction,*) 'Time_rst = ',trst/3600., 'Last depositions time(hr) = ',tem
eInform  = 'File='//TRIM(file_ddp)
go to 9999

9995    continue
nError   = RD_ERROR
eRoutine = 'read_output'
eMessage = 'Error finding timebreak on wet deposition file'
write(eAction,*) 'Time_rst = ',trst/3600., 'Last depositions time(hr) = ',tem
eInform  = 'File='//TRIM(file_wdp)
go to 9999

end
