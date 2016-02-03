subroutine init_scichem(namein,lflag,stdate,sttime,tstep,tendhr, &
                        dtsave,cgrid)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Initialize SCICHEM plume-in-grid run
!            Called by the host model
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_files              init_error             set_version
!                 initial            report_error           init_pig_grid
!               System subroutine date_and_time ()
!
! REVISION HISTORY: 
!
! 30-JUL-2001: Replaced fdate by date_and_time (BC)
! Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
! Updated May 2006 to allow new output files for a restarted simulation
!    rather than cumulative files, to reduce file sizes for long
!    simulations, PKK, AER
! Add calls to MPI routine for multiprocessor code, August 2007, PK, AER 
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc
use interface_definitions, only: INITIAL

#ifdef parallel
! use mpi
#endif

use common_mpi

implicit none

include 'mpif.h'

! --- ARGUMENTS

character(40) :: namein  !SCICHEM project file names
logical lflag        !Restart flag from host model
integer stdate       !Start date, coded YYYYDDD
integer sttime       !Start time, coded HHMMSS
integer tstep        !Model time step,  coded HHMMSS
real    tendhr       !Final hour of simulation (relative to start)
real    dtsave       !Time interval to save puff information
REAL :: CGRID( :,:,:,: )    !3D ambient concentrations from host model

! --- LOCALS

CHARACTER*10 :: date,time,zone
INTEGER      :: values(8)
character*24   clock
integer ios, nblank

logical ldump

integer rc, nch

character(128) process_log  ! SCICHEM log file output for secondary processes

common / dmpcmn / ldump
save  / dmpcmn / 

name = namein
nch_n = nblank(name)

dt_save = dtsave

write(*,*)'name: ',name
write(*,*)'nch_n: ',nch_n

#ifdef parallel
call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
#else
myid = 0
#endif

ios  = 0

write(*,*)'Process ', myid, ' of ', numprocs, ' is alive'

if (myid == 0) then

  call set_files(lflag,name,nch_n)

  file_imc = file_inp    !all input comes from inp file
  file_msc = file_inp

  call init_error

  open(unit=lun_log,file=file_log,status='UNKNOWN',iostat=ios)
  if (ios /= 0) then
    nError   = OP_ERROR
    eRoutine = 'init_scichem'
    eMessage = 'Error opening SCICHEM log output file'
    eInform  = 'File='//TRIM(file_log)
    go to 9999
  end if

#ifdef parallel
  call MPI_BCAST(file_log, 128, MPI_CHARACTER, 0, &
           MPI_COMM_WORLD, ierr )
#endif

!------ set code version number

  call set_version(iversion_code)

!------ initialization

! --- Initialize SCICHEM horizontal grid parameters and allocate
! --- dynamic arrays
  call init_pig_grid
  if (nError /= NO_ERROR) go to 9999

  call initial(lflag,stdate,sttime,tstep,tendhr,cgrid)
  if (nError /= NO_ERROR) go to 9999

  lsplit_report = .true.
  ldump     = .false.

!------ exit if only doing CREATE

  if (create) go to 9999

!------ setup for integration

!------ Get date and time

  CALL date_and_time(date,time,zone,values)
  WRITE(clock,100)values(2),values(3),values(1),values(5),values(6),values(7)
100 format (I2.2,'/',I2.2,'/',I4.4,' at ',I2.2,':',I2.2,':',I2.2)

  write(lun_log,111,iostat=ios)'Starting run at t =',t/3600., &
                'hrs. with NPUFF = ',npuf,' on ',clock
111     format(a,f7.2,a,i5,a,a)
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'init_scichem'
    eMessage = 'Error writing SCICHEM log file'
    eInform  = 'File='//TRIM(file_log)
    go to 9999
  end if

#ifdef parallel
else

  lun_log = 118

  call MPI_BCAST(file_log, 128, MPI_CHARACTER, 0, &
           MPI_COMM_WORLD, ierr )

  nch = LEN_TRIM(file_log)

  write(process_log,'(A,"_",I3.3,".log")')file_log(1:nch-4),myid

  open(unit=lun_log,file=process_log,status='UNKNOWN',iostat=ios)
  if (ios /= 0) then
    nError   = OP_ERROR
    eRoutine = 'scipuff'
    eMessage = 'Error opening SCIPUFF log output file'
    eInform  = 'File='//TRIM(file_log)
    go to 9999
  end if
#endif

end if

9999    continue

if (nError /= NO_ERROR) then
  call report_error
  stop
end if

return
end
