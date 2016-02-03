subroutine set_files(lflag,namex,nch_nx)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Setup all SCICHEM file names and unit numbers
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  getenv
!
! REVISION HISTORY: 
!   PK, AER, May 2006: Remove namelist type file for Cl2 and HCl concs and
!                      use different names for environment variables
!                      representing input and output directories
!   PK, AER, May 2006: Allow new output files for a restarted simulation
!                      rather than cumulative output files (to reduce
!                      file sizes for long simulations)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- ARGUMENTS
 
logical lflag          ! Restart flag from host model
character*(*) namex    ! Project name
integer nch_nx         ! Number of characters in project name

! --- LOCALS

character(120) :: indir = ''         ! Pathname of dir with input files
character(120) :: outdir = ''        ! Pathname of dir with current output files
character(120) :: rstdir = ''        ! Pathname of dir with previous output files
integer, external :: nblank     ! SCICHEM function that returns index of
                                ! last non-blank character in string

call getenv('SCI_INDIR',  indir)   ! Env. variable for input directory
call getenv('SCI_OUTDIR', outdir)  ! Env. variable for output directory

if (lflag) then
  call getenv('SCI_RSTDIR', rstdir)  ! Env. variable for restart directory
end if

! Make sure you don't overwrite restart files by giving the same names
! for the output and restart directories
!debug
write(*,*)'indir: ',indir
write(*,*)'outdir: ',outdir
write(*,*)'rstdir: ',rstdir
!debug
if (outdir == rstdir) then
  write(*,*)'Error in set_files'
  write(*,*)'Restart directory name is same as output directory name'
  stop
end if

nch_n = nch_nx
name  = namex

! Input files
file_imc = indir(1:nblank(indir))//'/'//name(1:nch_n)//'.imc'
file_inp = indir(1:nblank(indir))//'/'//name(1:nch_n)//'.inp'
file_msc = indir(1:nblank(indir))//'/'//name(1:nch_n)//'.msc'
file_scn = indir(1:nblank(indir))//'/'//name(1:nch_n)//'.scn'

! Additional input files for restarted run
if (lflag) then
  file_ddpr = rstdir(1:nblank(rstdir))//'/'//name(1:nch_n)//'.ddp'
  file_wdpr = rstdir(1:nblank(rstdir))//'/'//name(1:nch_n)//'.wdp'
  file_dmpr = rstdir(1:nblank(rstdir))//'/'//name(1:nch_n)//'.dmp'
  file_prjr = rstdir(1:nblank(rstdir))//'/'//name(1:nch_n)//'.prj'
  file_pufr = rstdir(1:nblank(rstdir))//'/'//name(1:nch_n)//'.puf'
  file_dgnr = rstdir(1:nblank(rstdir))//'/'//name(1:nch_n)//'.dgn'
end if

! Output files
file_smp = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.smp'
file_dep = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.dep'
file_ddp = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.ddp'
file_wdp = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.wdp'
file_dmp = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.dmp'
file_dos = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.dos'
file_err = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.err'
file_log = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.log'
file_prj = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.prj'
file_puf = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.puf'
file_dgn = outdir(1:nblank(outdir))//'/'//name(1:nch_n)//'.dgn'

file_dbg = name(1:nch_n)//'.dbg'
file_mcw = name(1:nch_n)//'.mcw'
file_pal = name(1:nch_n)//'.pal'
file_rad = name(1:nch_n)//'.rad'
file_sfc = name(1:nch_n)//'.sfc'
file_ATP = name(1:nch_n)//'.atp'

file_dat = 'DOSCAL.DAT'
file_def = 'scichem.def'
file_src = 'SRCOUT.TMP'
file_usr = 'USERDATA.TMP'
file_ind = 'INDEXR.DAT'

lun_tmp = 10

lun_dbg = 11
lun_dep = 12
lun_dmp = 13
lun_dos = 14
lun_err = 15
lun_inp = 16
lun_met = 17
lun_msc = 18
lun_pal = 19
lun_prj = 20
lun_puf = 21
lun_rad = 22
lun_scn = 23
lun_sfc = 24
lun_ter = 25

lun_dat = 26
lun_def = 27
lun_src = 28
lun_usr = 29
lun_log = 30
lun_smp = 31
lun_mcw = 32
lun_ATP = 33
lun_dgn = 34
lun_amb = 35

lun_ddp = 36
lun_wdp = 37

return

end

subroutine init_error
!*******************************************************************************
!
! FUNCTION:   Initialize the error parameters
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
 
use error_inc

implicit none

eRoutine = ' '
eMessage = ' '
eInform  = ' '
eAction  = ' '

nError = NO_ERROR

return
end

subroutine report_error
!*******************************************************************************
!
! FUNCTION:  Report the error
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
 
use error_inc
use files_inc

implicit none

! --- LOCALS

integer, external :: nblank
integer nch

write(6,*) '******ERROR******'
write(lun_log,*) '******ERROR******'
nch = nblank(eRoutine)
write(6,*) 'Routine=',eRoutine(1:nch)
write(lun_log,*) 'Routine=',eRoutine(1:nch)
nch = nblank(eMessage)
write(6,*) eMessage(1:nch)
write(lun_log,*) eMessage(1:nch)
nch = nblank(eInform)
write(6,*) eInform(1:nch)
write(lun_log,*) eInform(1:nch)
nch = nblank(eAction)
write(6,*) eAction(1:nch)
write(lun_log,*) eAction(1:nch)

return
end

subroutine WarningMessage(iparam,def)
!*******************************************************************************
!
! FUNCTION:   Report a warning message (does not stop code)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            report_error              init_error
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS
 
integer  iparam  !Not used here (used in GUI routine)
logical  def     !Flag that says whether to clear the error or not

write(6,*) '**** WARNING *****'
call report_error
if(def)then
  call init_error
end if

return
end

subroutine InfoMessage(iparam)
!*******************************************************************************
!
! FUNCTION:  Report information
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            report_error              init_error
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS
 
integer  iparam  !Not used here (used in GUI routine)

write(6,*) '**** INFORMATION *****'
call report_error
call init_error

return
end

subroutine AddPath(file,path)
!*******************************************************************************
!
! FUNCTION:   Add the path to the file name
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
 
character*(*) file  !File name
character*(*) path  !Path name

! --- LOCALS

integer, external :: nblank       
integer nchf, nchp

nchf = nblank(file)
nchp = nblank(path)

file = path(1:nchp)//'/'//file(1:nchf)

return

end

subroutine SplitName(name,file,path)
!*******************************************************************************
!
! FUNCTION:  Get the file and path out of the name
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
 
use error_inc

implicit none

! --- ARGUMENTS

character*(*) file  !File name
character*(*) path  !Path name
character*(*) name  !Full name
 
! --- LOCALS

character*1 BACK_SLASH
parameter  (BACK_SLASH = '/')

integer np

if(nError /= NO_ERROR)return

np = len(TRIM(name))
if(ichar(name(np:np)) == 0)np = np - 1
do while ( name(np:np)/=BACK_SLASH .and. np > 1)
  np = np - 1
end do

if(np <= 1)then
  file = TRIM(name)
  path = ' '
else
  file = TRIM(name(np+1:))
  path = TRIM(name(1:np-1))
end if

return
end
