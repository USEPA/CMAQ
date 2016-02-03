!*******************************************************************************
!$RCSfile: step.F90,v $
!$Revision: 1.22 $
!$Date: 2010/08/26 22:06:19 $
!*******************************************************************************
subroutine step_time(cgrid)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Main loop over large time step
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                step_amb            time_message                c_format
!          write_progress      write_progress_bar               c_release
!                  settle                  step_p                   split
!          check_progress                   merge                set_tlev
!                add_tlev                  tmerge      compress_puff_list
!              reset_tlev                   inter               write_smp
!              write_dump                 set_lev                 step_mc
!
! REVISION HISTORY: 
! 01/31/2007 : Add calls to MPI routine for multiprocessor code. Move step_mc 
!              to here from step_p_dyn -BC
!*******************************************************************************
 
! --- MODULES

use common_puf
use files_inc
use multcomp_mc_inc
use common_met, only: ddepos2d, wdepos2d
use diagnostics, only: ddepos, wdepos, chem
use common_mpi
#ifdef parallel
! use mpi
#endif
use interface_definitions, only: c_release, merge, split, step_amb, step_p, &
                                 tmerge, inter, write_smp

implicit none

  include 'mpif.h'
! --- ARGUMENTS
 
REAL :: CGRID( :,:,:,: )   !3-D ambient concentration (for chemistry runs)

! --- LOCALS

type ( puff_mc ) pm        ! -- multicomponent structure

integer mstep, istep, lev, lev2, npufo, nchg
integer ipuf, ilev, nch, nblank, set_lev
integer mchg, nxtlev, ntpuf
integer ntem, ntemx, ios, i, j, k, ij

real dts, dt, t0_step, t_step

character*80 cmsg, cmsg2, cmsg3
character*32 ctem

logical ldump, compress, lsplit

common / dmpcmn / ldump
save  / dmpcmn / 

integer            :: IsDone
#ifdef parallel
integer            :: stat(MPI_STATUS_SIZE)
integer            :: tag
integer, parameter :: tag_chunk = 1
integer, parameter :: tag_putmc = 2
integer, parameter :: tag_getmc = 3

character, dimension(MPI_MAX_ERROR_STRING)  :: eString

call build_mpi_type()

call pack_mpi_mchead()

call MPI_BCAST( MPI_MCHEAD, size_mchead, MPI_PACKED, 0,&
                MPI_COMM_WORLD, ierr)
#endif

IsDone = 1

#ifdef parallel
if (myid == 0) then
#endif

!---step ambient half a time step
  if (multicomp) then
    if (tambient < t + 0.5*delt ) then
      call step_amb(t + 0.5*delt - tambient ,cgrid)
      if (nError /= NO_ERROR) go to 9999
      tambient = t + 0.5*delt
    end if
  end if

  if(npuf == 0 .and. ncrel == 0) then
    t = t + delt
    nch = nblank(name)
    cmsg=name(1:nch)//' : Calculating '
    call time_message(cmsg2,t)
    if(nError /= NO_ERROR)go to 9999
    call c_format(delt,nch,ctem)
    write(cmsg3,'(i5,a)')npuf,' Puffs  '//ctem(1:nch)//'s timestep'
    call write_progress(cmsg,cmsg2,cmsg3)
    if(nError /= NO_ERROR)go to 9999
    ntem  = 0
    call write_progress_bar(ntem)
    return
  end if

!------ initialize for large timestep

  ntpuf    = npuf
  nxtlev   = mxtlev
  compress = .false.
  lsplit   = .true.

  mstep = 2**nxtlev          !total number of steps
  dts   = delt/float(mstep)  !smallest timestep needed

!------ report run progress

  nch = nblank(name)
  cmsg=name(1:nch)//' : Calculating '
  call time_message(cmsg2,t)
  if(nError /= NO_ERROR)go to 9999
  call c_format(dts,nch,ctem)
  write(cmsg3,'(i5,a)')npuf,' Puffs  '//ctem(1:nch)//'s timestep'
  call write_progress(cmsg,cmsg2,cmsg3)
  if(nError /= NO_ERROR)go to 9999

  ntemx = 64
  ntem  = 0
  call write_progress_bar(ntem)

!-----  Loop over small timesteps

  t0_step = t
  t_step  = 0.
  istep   = 0

  do while (istep < mstep)

    istep = istep + 1
    lev   = nxtlev - set_lev(istep)
    lev2  = nxtlev

!-------- continuous releases

    call c_release(istep,dts,lev,lev2,cgrid)
    if(nError /= NO_ERROR)go to 9999

!-------- adjust minimum step if necessary after c_release

    if (lev2 > nxtlev) then
      mchg   = 2**(lev2-nxtlev)
      mstep  = mstep*mchg
      istep  = (istep-1)*mchg + 1
      dts    = delt/float(mstep)
      write(lun_log,6000,err=9998)'Release reducing dts at t=',t/3600., &
                      'hrs to ',dts,'s (',lev2,nxtlev,istep,mstep,')'
      nxtlev = lev2
      lev  = nxtlev - set_lev(istep)
    end if

    npufo = npuf
    nchg  = 0

!-------- Check if any puffs are being stepped

    nspuf = 0
    do ilev = lev,nxtlev
      nspuf = nspuf + ntlev(ilev)
    end do
    compress = compress .and. (nspuf>0)

    if (nspuf> 0) then
      allocate(StepMCdat(nspuf),Splitdat(nspuf),STAT=ios)
      if (ios /= 0) then
        nError   = UK_ERROR
        eRoutine = 'step'
        eMessage = 'Error allocating StepMCdat'
        write(eInform,*)'nspuf = ',nspuf
      end if
    end if

!-------- Loop over time levels

    nspuf = 0

    do ilev = lev,nxtlev

      dt = dts*float(2**(nxtlev-ilev))
      ipuf = itfrst(ilev)

      do while (ipuf > 0)

!------------ compute settling rate (z=0.0)

        call settle(ipuf,0.0,puff(ipuf)%sr,fac_rfl)

!-----------  integrate

        nspuf = nspuf + 1

        call step_p(dt,puff(ipuf),ipuf,lev,lev2,nchg,1.0,1.0,cgrid)

        if (nError /= NO_ERROR) go to 9999
      
        ipuf = puff(ipuf)%idtn
         
      end do
      
    end do

#ifdef parallel
! Scatter StepMCdat

    allocate(nchunk(numprocs), ipuf_start(numprocs), stat = ios)

    nchunk(1:numprocs) = nspuf/numprocs

    ipuf_start(1) = 1

    source = 0

    do j = 1,numprocs
 
      if( j <= (nspuf - numprocs*nchunk(numprocs)) )&
          nchunk(j) = nchunk(j) + 1

      if( j > 1) then

        dest = j-1

        ipuf_start(j) = ipuf_start(dest) + nchunk(dest)

        tag  = tag_chunk

        call MPI_Send ( nchunk(j), 1, MPI_INTEGER, dest, &
                        tag, MPI_COMM_WORLD, ierr )

        if ( nchunk(j) > 0 ) then

          tag  = tag_putmc

          call MPI_Send ( StepMCdat(ipuf_start(j)), nchunk(j), MPI_MCDAT, dest, &
                           tag, MPI_COMM_WORLD, ierr )
        end if
      end if 

    end do

    ! call step mc on root (duplicate in cont_rel_dyn)
    do ipuf = 1,nchunk(1)
#else
    do ipuf = 1,nspuf
#endif

      pStepMCdat => StepMCdat(ipuf)
 
      call step_mc()

      if (nError /= NO_ERROR) then
        j = pStepMCdat%ipuf
        call dump_puff(j,puff(j))
        IsDone = -1
        exit
      end if

    end do

#ifdef parallel
    tag = tag_getmc
    ! gather StepMCdat 
    do j = 2,numprocs
 
      source = j-1

      if ( nchunk(j) < 1 ) cycle
      
      call MPI_Recv ( StepMCdat(ipuf_start(j)), nchunk(j), MPI_MCDAT, source, &
        tag, MPI_COMM_WORLD, stat, ierr )

      pStepMCdat => StepMCdat(ipuf_start(j))

      if (pStepMCdat%ipuf < 0) then
        nError = UK_ERROR
        k = ABS(pStepMCdat%ipuf)
        write(lun_err,*)'Error from process id:',j
        write(lun_log,*)'Error from process id:',j
        call dump_puff(k,puff(k))
        IsDone = -1
      end if

    end do
#endif
    if ( istep == mstep .and. IsDone == 1 )  IsDone = 0
    
    if ( IsDone >= 0) then 

#ifdef parallel
      call MPI_Bcast ( IsDone, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr )
#endif
      ! update deposition and pm 
      do ipuf = 1,nspuf
  
        pStepMCdat => StepMCdat(ipuf)

        !====   deposition
        ij       = pStepMCdat%ij
        do i = 1, nspecies
          ddepos2d(ij,i) = ddepos2d(ij,i) + pStepMCdat%ddepos(i) !2D dry deposition field
          wdepos2d(ij,i) = wdepos2d(ij,i) + pStepMCdat%wdepos(i) !2D wet deposition field
          chem(i)   = chem(i)   + pStepMCdat%chem(i)  
          ddepos(i) = ddepos(i) + pStepMCdat%ddepos(i) !2D dry deposition field
          wdepos(i) = wdepos(i) + pStepMCdat%wdepos(i) !2D dry deposition field
        end do

        j                       = pStepMCdat%ipuf
        ps(1:nspectot+nambient) = pStepMCdat%ps(1:nspectot+nambient)
        nbad_chem               = nbad_chem + pStepMCdat%nbd 
        ngd_chem                = ngd_chem + pStepMCdat%ngd 

        call set_mc_from_ps(pm)
    
        call get_mc_ps(puff(j),pm)

        !====   Put puff multi-component info
        if (SplitDat(ipuf)%frac > 0.) then
          k = SplitDat(ipuf)%npuf
          call put_mc_frac(puff(k),pm,SplitDat(ipuf)%frac)
          call put_mc_frac(puff(j),pm,1.-SplitDat(ipuf)%frac)
        else
          call put_mc(puff(j),pm)
        end if

      end do

    end if

    if (allocated(StepMCdat)) then 
      deallocate(StepMCdat,Splitdat,STAT=ios)
      if (ios /= 0) then
        nError   = UK_ERROR
        eRoutine = 'step'
        eMessage = 'Error deallocating StepMCdat'
        write(eInform,*)'nspuf = ',nspuf
      end if
    end if

#ifdef parallel
    if (allocated(nchunk)) then
      deallocate(nchunk,ipuf_start,STAT = ios)
      if (ios /= 0) then
        nError   = UK_ERROR
        eRoutine = 'step'
        eMessage = 'Error deallocating nchunk'
        write(eInform,*)'nspuf = ',nspuf
      end if
    end if
#endif
    if ( IsDone < 0 .or. nError /= NO_ERROR) go to 9999

  !-------- Loop over time levels

    do ilev = lev,nxtlev

      dt = dts*float(2**(nxtlev-ilev))
      ipuf = itfrst(ilev)

      do while (ipuf > 0)
      
        if (lsplit) then
          call split(ipuf,lsplit,cgrid)
          if (nError /= NO_ERROR) go to 9999
        end if

        ipuf = puff(ipuf)%idtn

      end do

      call check_progress
      if (nError /= NO_ERROR) go to 9999

    end do

!---------Modified accumulation of time to eliminate deactivation/static source problems
!         due to round-off errors when making a long run with a small time step.  (SFP 6/3/98)
    t_step = t_step + dts
    t      = t0_step + t_step

    ntem = nint(64.*t_step/delt)
    call write_progress_bar(ntem)

!-------  check for merge and puffs off grid

    if (istep == mstep) then

      call merge(cgrid)
      if (nError /= NO_ERROR) go to 9999

      call set_tlev

    else

      if( npuf > npufo )then
        call add_tlev(npufo+1,npuf)
      end if

      call tmerge(lev,lev2,cgrid)
      if (nError /= NO_ERROR) go to 9999

      if (compress) call compress_puff_list

      call reset_tlev

    end if

!-------  compute interaction terms

    call inter(lev,lev2,cgrid)
    if (nError /= NO_ERROR) go to 9999

!-------  compute sampler output

    if (lsmp) then
      if (lev <= mxlev_smp) then
        call write_smp(cgrid)
        if (nError /= NO_ERROR) go to 9999
      end if
    end if

!-------  check for puff cleanup

    if (istep /= mstep) then
      ntpuf = 0
      do ilev = 0,lev2
        ntpuf = ntpuf + ntlev(ilev)
      end do
      compress = ((npuf > MAXPUF/3) .or. (npaux > MAXPAUX/3)) &
              .and. ((npuf-ntpuf) > npuf/4)
    else
      compress = .false.
    end if

!-------  check for dump output

    if (ldump) then
      call write_dump
      ldump = .false.
    end if

    call check_progress
    if (nError /= NO_ERROR) go to 9999

!-------- adjust minimum step if necessary

    if (lev2 > nxtlev) then
      mchg   = 2**(lev2-nxtlev)
      mstep  = mstep*mchg
      istep  = istep*mchg
      dts    = delt/float(mstep)
      write(lun_log,6000,err=9998)'Step reducing dts at t=',t/3600.,'hrs to ' &
                             ,dts,'s (',lev2,nxtlev,istep,mstep,')'
6000    format(a,f7.2,a,1p,e12.4,a,0p,4i6,a)
      nxtlev = lev2
    end if

  end do

  call check_progress
  if (nError /= NO_ERROR) go to 9999

#ifdef parallel
else   ! myid != 0

  call unpack_mpi_mchead()

  do
         
    tag  = tag_chunk
    call MPI_Recv ( npuf_chunk, 1, MPI_INTEGER, 0, &
                    tag, MPI_COMM_WORLD, stat, ierr )

    if ( npuf_chunk > 0 ) then

      allocate(StepMCdat(npuf_chunk),STAT=ios)
      if (ios /= 0) then
        nError   = UK_ERROR
        eRoutine = 'step'
        eMessage = 'Error allocating StepMCdat'
        write(eInform,*)'npuf_chunk = ',npuf_chunk
      end if

      tag  = tag_putmc
      call MPI_Recv ( StepMCdat, npuf_chunk, MPI_MCDAT, 0, &
                      tag, MPI_COMM_WORLD, stat, ierr )

      do ipuf = 1,npuf_chunk

        pStepMCdat => StepMCdat(ipuf)

        call step_mc()

        if (nError /= NO_ERROR) then
          pStepMCdat%ipuf = -pStepMCdat%ipuf
          exit
        end if

      end do  

      dest = 0
   
      tag  = tag_getmc
      call MPI_Send ( StepMCdat, npuf_chunk, MPI_MCDAT, dest, &
                    tag, MPI_COMM_WORLD, ierr )
 
      if(allocated(StepMCdat))deallocate(StepMCdat,STAT=ios)
      if (ios /= 0) then
        nError   = UK_ERROR
        eRoutine = 'step'
        eMessage = 'Error deallocating StepMCdat'
        write(eInform,*)'npuf_chunk = ',npuf_chunk
      end if
 
    end if
     
    call MPI_Bcast ( IsDone, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr )

    if ( IsDone <= 0 )  exit

  end do

end if
#endif

9999    continue

#ifdef parallel
if (myid == 0 .and. nError /= NO_ERROR) then
  IsDone = -1
  call MPI_Bcast ( IsDone, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr )
end if
#endif

return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'step'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

end
