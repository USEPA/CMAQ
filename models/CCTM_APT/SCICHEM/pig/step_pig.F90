subroutine step_pig(cgrid,depv,wvel,jdate,jtime,mstep, &
                    tstep,outstep,nsteps,diagflg)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Performs one complete timestep for SCICHEM
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              update_met           set_cgrid_eqm              bl_from_ua
!              set_blturb      reset_static_puffs            puff_release
!               decay_fac               step_time          write_progress
!             end_scichem                    exit
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!    Minor updates (Fortran 90), February 2004 (PKK, AER)
!    Updated for parallel version, August 2007(PKK, AER)
!******************************************************************************
 
! --- MODULES

use common_puf
use common_date
use code_defines_inc
use interface_definitions, only: OUTPUT_ALL, SET_CGRID_EQM, STEP_TIME, &
                                 UPDATE_MET
use common_mpi

#ifdef parallel
! use mpi
#endif

!   Performs one complete timestep for SCICHEM

implicit none

include 'mpif.h'

! --- ARGUMENTS

REAL :: CGRID( :,:,:,: )             ! host model 3D ambient 
REAL, INTENT(IN) :: DEPV( :,:,: )    ! host model 2D dry deposition velocities
REAL, INTENT(IN) :: WVEL( :,:,: )    ! host model derived vertical velocities

integer         JDATE     ! current model date, coded YYYYDDD
integer         JTIME     ! current model time, coded HHMMSS
integer      MSTEP( 3 )   ! time step vector (HHMMSS)
                          ! MSTEP(1) = local output step
                          ! MSTEP(2) = sciproc sync. step (chem)
                          ! MSTEP(3) = twoway model time step w.r.t. wrf time
                          !            step and wrf/cmaq call frequency
integer         NSTEPS    ! number of output time steps

real tstep                ! simulation timestep (seconds)
real outstep              ! output timestep (seconds)

logical DIAGFLG           ! flag for saving diagnostics output as
                          ! netcdf files (process analysis)

! --- LOCALS

character*80 cmsg,cmsg2,cmsg3

integer NCH, NBLANK

real, save :: telaps = 0.
integer, save :: istep = 0

#ifdef parallel
integer errcode
#endif

MDATE = JDATE
MTIME = JTIME

!debug
!      write(*,*)'cgrid,depv,wvel: ',cgrid(63,61,1,1),depv(6,63,61), &
!                 wvel(63,61,2)
!      write(*,*)'jdate,jtime: ',jdate,jtime
!      write(*,*)'tstep: ',mstep
!      write(*,*)'tmstep,dtsave,nsteps,diagflg: ', &
!                 tstep,outstep,nsteps,diagflg 
!debug

delt = tstep

if (myid == 0) then

!------ Update meteorological fields

  call update_met( JDATE, JTIME, MSTEP( 2 ), DEPV, WVEL )

  if (nError /= NO_ERROR) go to 9999

  if (CODEPARAM == STDAMODEL) then
! -- set equilibrium for cgrid
    if (restart) then
      if (t == trst) then
        call set_cgrid_eqm(cgrid)
      end if
    else
      if ( t == 0.) then
        call set_cgrid_eqm(cgrid)
      end if
    end if
  end if

  if (restart) restart = .false.

!------ bl_from_ua always true

  call bl_from_ua
  if (nError /= NO_ERROR) go to 9999

!------ set turbulence

  call set_blturb
  if (nError /= NO_ERROR) go to 9999

!------ Delete existing static puffs for rebuilding with new met

  call reset_static_puffs

!------ Release puffs

  call puff_release( JDATE, JTIME, MSTEP( 1 ) )
  if (nError /= NO_ERROR) go to 9999

!------ compute decay factors for each material

  if (ldecay) then
    call decay_fac
  end if

!------ Step puffs

  call step_time(cgrid)
  if (nError /= NO_ERROR) go to 9999

  telaps = telaps + tstep

!------ increment output time once every output time step
  IF (telaps+1.E-6 >= outstep) THEN
     telaps = 0.
     istep = istep + 1
     t_save = t_save + dt_save
!-------  output

    nch = nblank(name)
    cmsg=name(1:nch)//' : Writing output'
    cmsg2 = char(0)
    cmsg3 = char(0)
    call write_progress(cmsg,cmsg2,cmsg3)

    CALL output_all( CGRID )
    if (nError /= NO_ERROR) go to 9999

!  save diagnostics output to netcdf files if user selects this option
    if ( DIAGFLG ) call output_dgn( JDATE, JTIME, MSTEP )
    if (nError /= NO_ERROR) go to 9999

  end if

  if (istep == nsteps) then
     call end_scichem
  end if

  return

9999    continue

  call end_scichem
#ifdef parallel
  errcode = myid + 1
  call MPI_ABORT(MPI_COMM_WORLD, errcode, ierr)
#else
  call exit
#endif

#ifdef parallel
else

  call step_time(cgrid)
  if (nError /= NO_ERROR) then
     errcode = myid + 1
     call MPI_ABORT(MPI_COMM_WORLD, errcode, ierr)
  else
     return
  end if
#endif

end if

end
