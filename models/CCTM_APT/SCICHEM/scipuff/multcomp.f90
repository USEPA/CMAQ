!*******************************************************************************
!$RCSfile: multcomp.F90,v $
!$Revision: 1.26 $
!$Date: 2010/12/07 20:12:34 $
!*******************************************************************************
!******************************************************************************
!Subroutine step_mc changed for new aerosol/aqueous routines for SCICHEM-CMAQ
!Subroutine set_ps_from_mc changed to account for new particle types (no.
!concs and surface area) and to calculate default aerosol dry dep and washout
!only if chemical aerosol option is not selected
!Subroutine set_amb_keqm updated for new call to step_aerosol_chem
!PK, AER, July 2005
! 01/31/07: Removed redundant variable O3Max. 
!           Use StepMCdat array for multiprocessor code -BC
!******************************************************************************

module cstar_inc
  save

  real, dimension(:,:), allocatable  :: species_max  ! Maximum cstar for each puff and species
  integer i0_smax  ! start index number for smax 

end module

subroutine step_mc()
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Advance multi-component species
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          set_chem_stage       step_gas_phase
!         set_equilibrium       step_aerosol_chem            step_aqueous
!            sum_nitrogen       sum_nitrogen_conc
!
! REVISION HISTORY: 
!
! 08 JAN 2001 : Disabled warning messages when switching equilibrium to
!               fast solver. - RIS
! 24 JUL 2001 : Corrected pointers for turbulent correlations  - BC
! 06 AUG 2003 : Add step_gas_phase subroutine to setup and call step_ode/
!               step_ynb. Also add logical lstep_tot for step total and
!               ambient instead of perturbation. - BC
! 16 DEC 2004 : (A) Add nfast_sav to argument of step_gas_phase as it needs
!                   to be updated when setting slow species to fast permanently
!               (B) Set ambient concentration to initial value after 
!                   stepping ambient concentration, when stepping total 
!                   concentrations - BC
!
! 21 MAR 2005  : (A) Set reaction volume ratios faca and facb to 1.
!                (B) Limit the number of diagnostic messages to 10 - BC
! Jun/Jul 2005 : Updated for new calls to step_aerosol_chem, step_aqueous
!                and set_vdry - PK, AER
! Oct 2005     : Calculate and store dry and wet dep separately - PK, AER
! Jun/Jul 2006 : Reverse order of calls to aerosol and aqueous - PK, AER
! Jul 2006     : Updated to treat wet deposition by convective precipitation
!                correctly and to consider ratio of water in puff to total
!                water in grid column- PK, AER
! Jan 2007     : Removed redundant variable O3Max -BC
! August 2007  : Use StepMCdat array for multiprocessor code -BC
! August 2010  : Include updates made by PKK, AER for Oct. 2004 CMAQ release
!                -BC(SAGE-MGT)
! August 2011  : Updated for CMAQ 5.0 beta, PK, ENVIRON 
! March  2012  : Further updates for CMAQ 5.0 final, PK, ENVIRON
!******************************************************************************

! --- MODULES

use multcomp_inc
use common_mc_met
use common_mc_puf
use files_inc
use diagnostics
use aero_species_inc
use aqueous_species_inc
use common_mpi

implicit none

! --- ARGUMENTS (passed through structure StepMCdat)

type ( puff_str ) p   ! -- puff structure
real dt               ! -- timestep (sec)
real dtmc             ! -- timestep for equilibrium species
real csav             ! -- original puff tracer mass
real fac_diag         ! -- whether or not to include in diagnostics
real area_fac         ! -- area used for deposition

! --- LOCALS

real cA, carea
real msav(MAX_MC), conc(MAX_MC), concamb(MAX_MC), scavcoef(MAX_MC)
real corrsav(MAX_NCORR)
integer i, j, nn, ioff, istage_sav, nfast_sav, neq_sav, neq_tot, ir
real volp, depspec, len
real nitr1, nitr2, diffn, fac_dist(MAX_MC)

real f1, f2

logical  leqm_set

real pratepuf

real tauc

p              = pStepMCdat%p
ps             = pStepMCdat%ps
istage         = pStepMCdat%istage
t              = pStepMCdat%t
dt             = pStepMCdat%dt
dtmc           = pStepMCdat%dtmc 
csav           = pStepMCdat%csav
vol            = pStepMCdat%vol
fac_diag       = pStepMCdat%fac_diag
zk             = pStepMCdat%zk
tab            = pStepMCdat%tab
pb             = pStepMCdat%pb
hb             = pStepMCdat%hb
cldall         = pStepMCdat%cldall
cldallt        = pStepMCdat%cldallt
cldallp        = pStepMCdat%cldallp
cmassp         = pStepMCdat%cmassp
cmasst         = pStepMCdat%cmasst
pratebl        = pStepMCdat%pratebl
fcc            = pStepMCdat%fcc
fprcpc         = pStepMCdat%fprcpc
radyn          = pStepMCdat%radyn
us2            = pStepMCdat%us2
ws2            = pStepMCdat%ws2
lflag_dark     = pStepMCdat%lflag_dark
kamb           = pStepMCdat%kamb
corr           = pStepMCdat%corr

!debug
!write(*,*)'lun_log: ',lun_log
!write(lun_log,*)'kamb: ',kamb
!write(*,*)'zk,tab,pb,hb: ',zk,tab,pb,hb
!write(lun_log,*)'tab,pb,hb: ',tab,pb,hb
!write(lun_log,*)'cldall,cldallt,cldallp: ',cldall,cldallt,cldallp
!write(lun_log,*)'cmassp,cmasst,pratebl,fcc,fprcpc: ',cmassp,cmasst,pratebl,fcc,fprcpc
!write(lun_log,*)'radyn,us2,ws2: ',radyn,us2,ws2
!write(lun_log,*)'lflag_dark: ',lflag_dark
!call flush(lun_log)
!debug

do i = 1,nspectot
  psamb(i)  = ps(i)
  pswork(i) = ps(i)
end do

tk   = tab
pk   = pb
hk   = hb
cldk = cldall

if (lstep_tot) then
  do i = 1,nspectot
    psamb(i)%c = ps(i)%a 
    psamb(i)%a = 0.
    psamb(i)%m = 0.
    pswork(i)%c = ps(i)%c + ps(i)%a 
    pswork(i)%a = 0.
  end do
end if

!===  Set limits on concentrations
do i = 1,nspectot
  if(IsStar(i))then
    if (lstep_tot) then
      conc_lim(i) = 0.  
    else
      conc_lim(i) = -ps(i)%a
    end if
  else
    if (lstep_tot) then
      conc_lim(i) = 0. 
    else
      conc_lim(i) = -ps(i)%a*vol
    end if
  end if
end do

!====   Set flag used to step total versus perturbation concentrations

lflag_amb = .false.

!====   Load working arrays

!debug
!write(lun_log,*)'vol: ',vol
!debug
volp   = pi3*sqrt(p%det)
if (ncorrm == 0) vol = volp
!debug
!write(lun_log,*)'ncorrm,vol,volp: ',ncorrm,vol,volp
!debug

carea = p%sr/csav

istage_sav = istage     !save stage in case changed to set eqm
leqm_set   = .false.

!====   Save initial puff masses and concentrations

do i = 1, nspectot
  msav(i) = ps(i)%m
  if(IsStar(i))then
    concsav(i) = ps(i)%c
  end if
end do
!debug
!write(lun_log,*)'concsav(4),ps(4)%c,ps(4)%a,tot: ',concsav(4),ps(4)%c,ps(4)%a,ps(4)%c+ps(4)%a
!write(lun_log,*)'msav(4),ps(4)%c*vol: ',msav(4),ps(4)%c*vol
!debug

if (lbalance) then
  call sum_nitrogen(nitr1)
end if

!====   Set-up reaction volume ratios

do ir = 1, nreacts
  reaction(ir)%k = kamb(ir)
end do

!====   Set puff chemical stage

if (lstage) then
  istage = istage_sav
  call set_chem_stage(leqm_set)
end if

nfast_sav = nfast
neq_sav   = neq_s

!====   Advance gas-phase chemistry for total concentration

!debug
!write(lun_log,*)'before gas-phase chemistry'
!write(lun_log,*)'SO2,SULF,SULRXN: ',pswork(60)%c,pswork(61)%c,pswork(62)%c
!debug
call step_gas_phase(dt, leqm_set, pswork, nfast_sav)
!debug
!write(lun_log,*)'after gas-phase chemistry'
!write(lun_log,*)'SO2,SULF,SULRXN: ',pswork(60)%c,pswork(61)%c,pswork(62)%c
!debug
if (nError /= NO_ERROR) go to 9998

!====   Set equilibrium species for total concentration

if (neq_s > 0) then
  call set_equilibrium(.false.)
  if (nError /= NO_ERROR) then
    nError = NO_ERROR
    write(lun_log,*) 'Error setting equilibrium in step_mc-1 - will continue'
  end if
end if

if (lstep_tot) then

  !====   Save turbulent species correlations

  do i = 1,ncorrt
    corrsav(i) = corr(i)
    corr(i) = 0.0
  end do
  
  do i = ncorrt+1,MAX_NCORR
    corrsav(i) = 0.
    corr(i) = 0.0
  end do

  !====   Save advanced total concentrations and masses

  do i = 1,nspectot
    pswork(i)%c = ps(i)%c
    pswork(i)%a = psamb(i)%c
    pswork(i)%m = ps(i)%m
  end do

  !====   Advance gas-phase chemistry for ambient concentrations

  neq_tot = neq_s
  neq_s   = neq_sav

  call step_gas_phase(dt, leqm_set, psamb, nfast_sav)
  if (nError /= NO_ERROR) go to 9998

  if (neq_s > 0) then
    call set_equilibrium(.false.)
    if (nError /= NO_ERROR) then
      nError = NO_ERROR
      write(lun_log,*) 'Error setting equilibrium in step_mc-2 - will continue'
    end if
  end if

  !====   Set advanced perturbation concentrations

  do i = 1,nspectot
    ps(i)%c = pswork(i)%c - ps(i)%c
    ps(i)%a = pswork(i)%a
    ps(i)%m = pswork(i)%m
  end do

  do i = 1,ncorrt
    corr(i) = corrsav(i)
  end do

  neq_s    = neq_tot
  leqm_set = .true.

end if

!====   Aerosol and aqueous-phase chemistry calculations

if (laerosol) then

!--- first do aqueous-phase chemistry

!debug
!write(lun_log,*)'before aqueous, cldall: ',cldall
!cldall = 0.5
!write(lun_log,*)'for debugging only; cldall set to ',cldall
!call flush(lun_log)
!debug
  if (laqueous) then

    if (cldall > 0.) then

!load aqueous working array (species after aerosols)
      concamb = 0.
      conc = 0.
      do i = 1, naqueous
        j = index_aqueous(i)
        concamb(i) = MAX(0.,ps(j)%a)
        conc(i)    = MAX(0.,ps(j)%c + ps(j)%a)
        ps(j)%c    = conc(i) !save to report errors
      end do

! --  units:
! --  dt(sec),conc(ppm or mol/cc),humidity(g H2O/g dry air),
! --  cloud liquid water content(g/m3),precip rate (mm/hr),temp(k),press(atm)

! call first with ambient

      len = sqrt(pi2*p%szz)

! --- calculate precipitation rate for puff volume
      if ( cmassp > 0. ) then
        pratepuf = pratebl * vol * cldallp / cmassp
      else if ( cmasst > 0. ) then
        pratepuf = pratebl * vol * cldallt / cmasst
      else
        pratepuf = 0.
      end if

      pratepuf = MIN(pratepuf,pratebl)

!debug
!  write(lun_log,*)'calling aqueous with ambient'
!  write(lun_log,*)'dt,naqchem,naqueous,len,ic_units,hb,tab,pb: ', &
!             dt,naqchem,naqueous,len,ic_units,hb,tab,pb
!  write(lun_log,*)'cldall,pratepuf,fcc,fprcpc: ',cldall,pratepuf,fcc,fprcpc
!  do i = 1, naqueous
!     write(lun_log,*)'i,species,conc: ',i,aqueous_names(i),concamb(i)
!  end do
!  pratepuf = 1.0
!  write(lun_log,*)'for debugging only: pratepuf = ',pratepuf
!  write(lun_log,*)'for debugging only, skipping ambient aqueous'
  call flush(lun_log)
!debug
      call step_aqueous(dt,concamb,ic_units,cldall,pratepuf,fcc,fprcpc, &
                        tab,pb,naqchem,naqueous,aqueous_names,len,scavcoef)
!debug
!  write(lun_log,*)'after aqueous with ambient'
!  write(lun_log,*)'dt,naqchem,naqueous,len,ic_units,hb,tab,pb: ', &
!             dt,naqchem,naqueous,len,ic_units,hb,tab,pb
!  write(lun_log,*)'cldall,pratepuf,fcc,fprcpc: ',cldall,pratepuf,fcc,fprcpc
!  do i = 1, naqueous
!     write(lun_log,*)'i,species,conc,scav: ',i,aqueous_names(i),concamb(i),scavcoef(i)
!  end do
!  call flush(lun_log)
!debug
      if (nError /= NO_ERROR) then
        write(lun_log,*) 'Aqueous module called with the following'
        write(lun_log,*) '(solving for the ambient concentrations)'
        write(lun_log,*)'dt(s), clouds (g/m3), precip(mm/hr), fcc, fprcpc:'
        write(lun_log,*) dt,cldall,pratepuf,fcc,fprcpc
        write(lun_log,*)'P (atm), T(K):'
        write(lun_log,*) pb,tab
        write(lun_log,*)'Conc (ppm or mol/cc for gas, ug/m3 for aerosols):'
        do i = 1, naqueous
          j = index_aqueous(i)
          write(lun_log,*) species(j)%name,ps(j)%a
        end do
        go to 9998
      end if

! call again with total
!debug
!  write(lun_log,*)'calling aqueous with total'
!  write(lun_log,*)'dt,naqchem,naqueous,len,ic_units,hb,tab,pb: ', &
!             dt,naqchem,naqueous,len,ic_units,hb,tab,pb
!  write(lun_log,*)'cldall,pratepuf,fcc,fprcpc: ',cldall,pratepuf,fcc,fprcpc
!  write(lun_log,*)'shape(aqueous_names): ',shape(aqueous_names)
!  do i = 1, naqueous
!     write(lun_log,*)'i,species,conc: ',i,aqueous_names(i),conc(i)
!  end do
!  call flush(lun_log)
!debug
      call step_aqueous(dt,conc,ic_units,cldall,pratepuf,fcc,fprcpc, &
                        tab,pb,naqchem,naqueous,aqueous_names,len,scavcoef)
!debug
!  write(lun_log,*)'after aqueous with total'
!  write(lun_log,*)'dt,naqchem,naqueous,len,ic_units,hb,tab,pb: ', &
!             dt,naqchem,naqueous,len,ic_units,hb,tab,pb
!  write(lun_log,*)'cldall,pratepuf,fcc,fprcpc: ',cldall,pratepuf,fcc,fprcpc
!  do i = 1, naqueous
!     write(lun_log,*)'i,species,conc,scav: ',i,aqueous_names(i),conc(i),scavcoef(i)
!  end do
!  call flush(lun_log)
!debug
      if (nError /= NO_ERROR) then
        write(lun_log,*)'Aqueous module called with the following:'
        write(lun_log,*)'(solving for the plume concentrations)'
        write(lun_log,*)'dt(s), clouds (g/m3), precip(mm/hr), fcc, fprcpc:'
        write(lun_log,*) dt,cldall,pratepuf,fcc,fprcpc
        write(lun_log,*)'P (atm), T(K):'
        write(lun_log,*) pb,tab
        write(lun_log,*) 'Conc (ppm or mol/cc for gas, ug/m3 for aerosols):'
        do i = 1, naqueous
          j = index_aqueous(i)
          write(lun_log,*) species(j)%name,ps(j)%c
        end do
        go to 9998
      end if

!unload aqueous working arrays
      do i = 1, naqueous
        j = index_aqueous(i)
        conc(i) = MAX(conc(i),0.)
        ps(j)%c = conc(i) - concamb(i)
        ps(j)%tauwet = scavcoef(i)
      end do

    else

      do i = 1, naqueous
        j = index_aqueous(i)
        ps(j)%tauwet = 0.
      end do

    end if  ! if cldall > 0.

  end if  ! if laqueous > 0.

! -- Aerosol calculations

! --  units:
! --  conc(ppm or mol/cc),humidity(g H2O/g dry air),
! --  temp(k),press(atm)

!load aerosol working array
  concamb = 0.
  conc = 0.
  do i = 1, naero
    j = index_aero(i)
    concamb(i) = MAX(0.,ps(j)%a)
    conc(i)    = MAX(0.,ps(j)%c + ps(j)%a)
    select case ( TRIM( species(j)%name ) )
      case ('NUMATKN','NUMACC','NUMCOR','SRFATKN','SRFACC','SRFCOR' )
!DDDdebug
!write(lun_log,*)'i,j,species(j)%name,aero_names(i):', &
!                 i,j,species(j)%name,aero_names(i)
!write(lun_log,*)'before conc,ambient: ',conc(i),ps(j)%a
!DDDdebug
        conc(i)    = MAX(conc(i), ps(j)%a)
!DDDdebug
!DDDwrite(lun_log,*)'after conc,ambient: ',conc(i),ps(j)%a
!DDDdebug
    end select
  end do

!call first with ambient
!debug
!write(lun_log,*)'calling aerosol with ambient'
!write(lun_log,*)'dt,naero,ic_units,hb,tab,pb: ',dt,naero,ic_units,hb,tab,pb
!write(lun_log,*)'shape(aero_names): ',shape(aero_names)
!  do i = 1, naero
!     write(lun_log,*)'i,species,conc: ',i,aero_names(i),concamb(i)
!  end do
!call flush(lun_log)
!debug
  call step_aerosol_chem(dt,concamb,naero,aero_names,ic_units,hb,tab,pb)
!debug
!write(lun_log,*)'after aerosol with ambient'
!write(lun_log,*)'dt,naero,ic_units,hb,tab,pb: ',dt,naero,ic_units,hb,tab,pb
!  do i = 1, naero
!     write(lun_log,*)'i,species,conc: ',i,aero_names(i),concamb(i)
!  end do
!  write(lun_log,*)'nError,NO_ERROR: ',nError,NO_ERROR
!call flush(lun_log)
!debug
  if (nError /= NO_ERROR) go to 9998

!call next with total
!debug
!write(lun_log,*)'calling aerosol with total'
!write(lun_log,*)'dt,naero,ic_units,hb,tab,pb: ',dt,naero,ic_units,hb,tab,pb
!  do i = 1, naero
!     write(lun_log,*)'i,species,conc: ',i,aero_names(i),conc(i)
!  end do
!call flush(lun_log)
!debug
  call step_aerosol_chem(dt,conc,naero,aero_names,ic_units,hb,tab,pb)
!debug
!write(lun_log,*)'after aerosol with total'
!write(lun_log,*)'dt,naero,ic_units,hb,tab,pb: ',dt,naero,ic_units,hb,tab,pb
!  do i = 1, naero
!     write(lun_log,*)'i,species,conc: ',i,aero_names(i),conc(i)
!  end do
!  write(lun_log,*)'nError: ',nError
!call flush(lun_log)
!debug
  if (nError /= NO_ERROR) go to 9998

!unload aerosol working array
  do i = 1, naero
    j = index_aero(i)
    conc(i) = MAX(conc(i),0.)
    ps(j)%c = conc(i) - concamb(i)
  end do

  leqm_set = .false.

end if

!debug
!stop
!debug
!====   Limit volume

do i = 1, nlim
  nn = index_lim(i)
  if (IsStar(nn) .and. (concsav(nn) - ps(nn)%c) > 0. &
                .and.  ps(nn)%m > 0 .and. ps(nn)%c > 0.  ) then
    
  vol = MIN(vol,ps(nn)%m/(concsav(nn) - ps(nn)%c))
    
  end if
end do

do i = 1, nspecies
  if (IsStar(i)) then
    ps(i)%m = ps(i)%m + (ps(i)%c - concsav(i))*vol
  end if
end do

if (.not. leqm_set .and. neq_s /= 0 .and. istage >= nstage-1 ) then
  call set_equilibrium(.false.)
  if (nError /= NO_ERROR) then
    nError = NO_ERROR
    write(lun_log,*) 'Error setting equilibrium in step_mc - will continue'
  end if
end if

j = nspecies
do i = 1, nequilibrium
  j = j + 1
  ps(j)%m = ps(j)%m + (ps(j)%c - concsav(j))*vol
end do

!====   Force species balance

if (lbalance) then
  call sum_nitrogen(nitr2)
  diffn = nitr2-nitr1
  call sum_nitrogen_conc(fac_dist)
  do i = 1, nspectot
    ps(i)%m = ps(i)%m - diffn*fac_dist(i)
  end do
end if
  
!====   chemistry diagnostic

do i = 1, nspectot
  pStepMCdat%chem(i) = pStepMCdat%chem(i) + fac_diag*(ps(i)%m - msav(i))
end do

!====   deposition

do i = 1, nspecies
  tauc = ps(i)%taudry + ps(i)%tauwet
  if (tauc > 0.) then
    cA = max(ps(i)%c,-ps(i)%a)
    ddepspec = cA*ps(i)%taudry*dt*vol
    wdepspec = cA*ps(i)%tauwet*dt*vol
    depspec = ddepspec + wdepspec
    if (ps(i)%m >= -ps(i)%a*vol) then
      if (ps(i)%m - depspec < -ps(i)%a*vol) then
        f1 = ddepspec/depspec
        f2 = 1. - f1
        depspec = ps(i)%a*vol + ps(i)%m
        ddepspec = f1 * depspec
        wdepspec = f2 * depspec
      end if
    else
      if (depspec > 0.) then
        ddepspec = 0.
        wdepspec = 0.
        depspec = 0.
      end if
    end if
    pStepMCdat%ddepos(i) = pStepMCdat%ddepos(i) + fac_diag*ddepspec   !dry deposition diagnostic
    pStepMCdat%wdepos(i) = pStepMCdat%wdepos(i) + fac_diag*wdepspec   !wet deposition diagnostic
    ps(i)%m = ps(i)%m - depspec
  end if
end do

pStepMCdat%ps(1:nspectot+nambient) = ps(1:nspectot+nambient)
pStepMCdat%istage                  = istage

10 continue

nfast = nfast_sav

9999 return

9998    continue

write(lun_log,*) 'Error stepping chemistry'
write(lun_log,*) 'Concentrations at the beginning of step_mc:'
write(lun_log,*) 'Species  ps(i)%c   ps(i)%a   ps(i)%c + ps(i)%a  ps(i)%m'
do i = 1, nspectot
  write(lun_log,300)TRIM(species(i)%name),concsav(i),ps(i)%a,&
                    concsav(i)+ps(i)%a,msav(i)
end do
300 format(A8,1p,4e12.4)  
write(lun_log,*) 'Vol = ', vol
write(lun_log,*) 'Reaction rate constants:'
do i = 1, nreacts
  write(lun_log,310) i,kamb(i),reaction(i)%k
end do
310 format(i6,1p,2e12.4)  

nError = NO_ERROR
pStepMCdat%nbd = pStepMCdat%nbd + 1
!debug
!stop
!debug
go to 10

end

subroutine step_gas_phase(dt, leqm_set, ps_gas, nfast_sav)
!*******************************************************************************
!
! FUNCTION:  Step gas phase chemistry
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

use multcomp_inc
use diagnostics
use files_inc
use common_mc_puf
use common_mpi

implicit none

real                 dt
logical              leqm_set
type (work_species)  ps_gas(MAX_MC)

integer i, nfast_sav, io3
real    o3lim

do i = 1,nspectot
  ps(i) = ps_gas(i)
end do

if (isolve == ID_YNB) then
  io3 = ikey_spec(O3)
  if (io3 == NOT_SET_I .or. lstep_tot) then
    o3lim  = 0.0
  else
    o3lim  = -ps(io3)%a
  end if
  call step_ynb(dt, leqm_set, o3lim)
else
  call step_ode(dt, leqm_set)
end if

if (nError /= NO_ERROR) then  
   
  if (TRIM(eRoutine) == 'get_prodloss' ) then
    ierr = 1                                     !error with equilibrium species
  else if (TRIM(eRoutine) == 'set_dtsm') then
    ierr = 2                                     !error with slow species
  else if (lstage .and. istage < nstage  ) then
    ierr = 3                                     !error with staged chemistry
  else
    ierr = 4                                     !some other error
  end if

  if (ierr < 4) then  
    nError = NO_ERROR
    write(lun_log,*) 'Second try to step gas-phase chemistry'
    
! - reset concentrations

    do i = 1,nspectot
      ps(i) = ps_gas(i)
    end do
    
    if (ierr == 1) then
      istage = nstage
      call set_equilibrium(.false.)
      if (nError /= NO_ERROR) nError = NO_ERROR
    
      ! - set equilibrium to fast species (temporarily)
      do i = 1, neq_s
        indxf(i+nfast) = indx_eq_s(i,istage) + nspecies
      end do
      nfast = nfast + neq_s
      neq_s = 0

    else if (ierr == 2) then
      nfast_sav = nfast  !change slow to fast permanently
    else
      istage = nstage    !change stage to max # of stages
    end if

! - restep gas-phase chemistry

    leqm_set = .true.

    if (isolve == ID_YNB) then
      call step_ynb(dt, leqm_set, o3lim)
    else
      call step_ode(dt, leqm_set)
    end if
    if (nError /= NO_ERROR) then
      go to 9999
    else
      pStepMCdat%ngd = pStepMCdat%ngd + 1
    end if
      
  else 
          
    go to 9999
  
  end if
else

  pStepMCdat%ngd = pStepMCdat%ngd + 1
  
end if

9999 continue

return
end

subroutine sum_nitrogen(ntotm)
!*******************************************************************************
!
! FUNCTION:  Sum nitrogen species masses
!           (can be any type of species, since it is user-specified)
!
! PRECONDITIONS REQUIRED: sparam(i)%nit must have # of N molecules
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

real ntotm  ! -- total mass of nitrogen

! --- LOCALS

integer i

ntotm = 0.

do i = 1, nspectot
  ntotm = ntotm + sparam(i)%nit*ps(i)%m
end do    

return
end

subroutine sum_nitrogen_conc(fac)
!*******************************************************************************
!
! FUNCTION:  Sum nitrogen species concentrations and calculate
!            factors used to redistribute excess or missing nitrogen
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

real fac(*)  ! -- factor used to redistribute excess nitrogen

! --- LOCALS

real ntotc, factot
integer i

ntotc = 0.
factot = 0.

do i = 1, nspectot
  ntotc = ntotc + sparam(i)%nit*max((ps(i)%c+ps(i)%a),0.)
  factot = factot + sparam(i)%nit
end do    
if (ntotc /= 0.) then
  do i = 1, nspectot  ! distribute according to total concentrations
    fac(i) = sparam(i)%nit*max(0.,(ps(i)%c+ps(i)%a))/ntotc
  end do    
else
  do i = 1, nspectot  ! distribute evenly
    fac(i) = sparam(i)%nit/factot  
  end do    
end if

return
end

subroutine step_ynb(dt, leqm_set, o3lim)
!*******************************************************************************
!
! FUNCTION:  Advance gas-phase chemistry
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         set_equilibrium            load_yeq_ynb               solve_ynb
!             update_conc
!
! REVISION HISTORY: 
!
! APR 2001: Limit NO plus O3 reaction to prevent negative O3 concentration
!           in the Gaussian distribution - PK
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc
use files_inc

implicit none

! --- ARGUMENTS

real dt          ! -- timestep (sec)
logical leqm_set ! -- flag to indicate if equilibrium species are set already

! --- LOCALS

integer i, nn, neq, nfast_sav
real yeq(MAX_MC), yeq_lim(MAX_MC)

integer ido3,idno,idno2 ! --- species ids for O3, NO, and NO2
real o3lim ! --- lower limit for O3 conc

!===== Set equilibrium species initially (if not already set)

nfast_sav = nfast
if (.not. leqm_set .and. neq_s > 0 .and. istage >= nstage-1 ) then
  call set_equilibrium(.false.)
  if (nError /= NO_ERROR) then
    nError = NO_ERROR
    write(lun_log,*) 'Error setting equilibrium in step_ynb'
    do i = 1, neq_s
      indxf(i+nfast) = indx_eq_s(i,istage) + nspecies
    end do
    nfast = nfast + neq_s
    neq_s = 0
  else
    leqm_set = .true.
  end if
end if

!===== Save initial equilibrium concentrations

do i = 1, nequilibrium
  nn = i + nspecies
  concsav(nn) = ps(nn)%c
end do

!===== Load yeq array

call load_yeq_ynb(yeq,yeq_lim,neq,ido3,idno,idno2)

call solve_ynb(yeq,yeq_lim,neq,dt,ido3,idno,idno2,o3lim)
if (nError /= NO_ERROR) go to 9999

!===== Unload yeq array and set tolerances

call update_conc(yeq)

nfast = nfast_sav

9999    return
end

subroutine load_yeq_ynb(yeq,lim,neq,ido3,idno,idno2)
!*******************************************************************************
!
! FUNCTION:  Load the yeq array with either ps%c or ps%m to be solved by
!            the Young & Boris method
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  
!
! REVISION HISTORY: 
!
! APR 2001: Limit NO plus O3 reaction to prevent negative O3 concentration
!           in the Gaussian distribution - PK
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

real yeq(*), lim(*)  ! -- yeq and lim arrays to be loaded
integer neq          ! -- number of equations to be solved by Young & Boris method

integer ido3,idno,idno2 ! --- species ids for O3, NO, and NO2

! --- LOCALS

integer i, nn, iA, iB
real cA

!===== Load yeq array

neq = nfast
do i = 1,nfast
  nn = indxf(i)
  if (nn == ikey_spec(O3)) idO3 = i
  if (nn == ikey_spec(NO)) idNO = i
  if (nn == ikey_spec(NO2)) idNO2 = i
  if(IsStar(nn))then
    yeq(i) = ps(nn)%c
  else
    yeq(i) = ps(nn)%m
  end if
  lim(i) = conc_lim(nn)
end do
do i = 1,nslow
  neq = neq + 1
  nn = indxs(i)
  if (nn == ikey_spec(O3)) idO3 = neq
  if (nn == ikey_spec(NO)) idNO = neq
  if (nn == ikey_spec(NO2)) idNO2 = neq
  if(IsStar(nn))then
    yeq(neq) = ps(nn)%c
  else
    yeq(neq) = ps(nn)%m
  end if
  lim(neq) = conc_lim(nn)
end do
do i = 1, ncorrt
  neq = neq + 1
  yeq(neq) = corr(i)
  lim(neq) = conc_lim(i+nspectot)
end do

return
end

subroutine solve_ynb(yeq,yeq_lim,neq,tout,ido3,idno,idno2,o3lim)
!*******************************************************************************
!
! FUNCTION: Advance chemistry using the Young & Boris Method from:
!           Young, T.R. and J.P. Boris (1997), A numerical technique for
!           solving stiff ordinary differential equations associated with the
!           kinetics of reactive-flow problems, J. Phys. Chem.,81,2424-2427 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             update_conc               load_conc            get_prodloss
!                 load_ab                   ldark
!
! REVISION HISTORY: 
!
! APR 2001: Limit NO plus O3 reaction to prevent negative O3 concentration
!           in the Gaussian distribution - PK
!
!*******************************************************************************

! --- MODULES

use param_inc
use multcomp_inc
use error_inc

implicit none

! --- ARGUMENTS

real yeq(*)      ! -- initial conditions
real yeq_lim(*)  ! -- limits they should not go below
integer neq      ! -- number of species/equations
real tout        ! -- in: time step (sec)
integer ido3,idno,idno2 ! --- species ids for O3, NO, and NO2
real o3lim       ! -- limit below which O3 concentration should not go

! --- LOCALS

real conc(MAX_MC), prod(MAX_MC), loss(MAX_MC)
real dum, dtmin, dtmax, eden, enum, dthalf
real timremain, dt, tmnow
real dtsub(2), dtmx(2)
real tepsn, tepss, stiff
real ersums, ersumn, errorn, errors, er
real small, one, epsn, epss

integer i, nstep, nfe, nnorm, nstiff
integer k, kiter, iconv, nstmax
integer niter, nstchk

logical ldark

real diffo3  ! o3 correction

! ... initialize parameters

nError = NO_ERROR

dtsub(1) = 12.0
dtsub(2) = 120.
dtmx(1) = 300.
dtmx(2) = 900.
niter = 3
dtmin = 1.0e-20
small = 1.e-30
one = 1.0
nstchk=1

! ... epsn = relative error control criteria for non-stiff species
! ... epss = relative error control criteria for stiff species
epsn =.001
epss =.001
nstmax = 5000
tepsn = 4.*epsn
tepss = 4.*epss

do i = 1,neq
  yeq(i) = max(yeq(i),yeq_lim(i))
end do

! ... select initial time step

if (ldark()) then
  dt = dtsub(2)
  dtmax = dtmx(2)
else
  dt = dtsub(1)
  dtmax = dtmx(1)
end if

timremain = tout

dt = min(dt,timremain)

tmnow = 0.
nstep = 0
nfe = 0

! ... loop over time steps

18    continue

! ... evaluate function for predictor step

call update_conc(yeq)
call load_conc(conc)
call get_prodloss(conc,prod,loss)
if (nError /= NO_ERROR) go to 9999
call load_ab(prod,loss,apre,bpre)

do i = 1, neq
  ydpre(i) = apre(i) - bpre(i)*yeq(i)
end do

nfe = nfe + 1

! ... test for stiffness every nstchk steps

if (nstep==0 .or. mod(nstep,nstchk)==0) then
  nnorm = 0
  nstiff = 0
  do i = 1,neq
    istiff(i) = 0
    stiff = abs(dt * bpre(i))
    if (stiff > 0.9) istiff(i) = 100
    if (istiff(i) == 0) then
      nnorm = nnorm + 1
    else
      nstiff = nstiff + 1
    end if
  end do
end if

! ... integration loop starts here

! ... integrate with predictor step

nstep = nstep + 1
if (nstep > nstmax) then
  nError = IV_ERROR
  eRoutine = 'solve_ynb'
  eMessage = 'Error solving chemistry rate equations'
  eAction  = 'Too many iterations'
  write(eInform,'(a,i4,a)') &
     'Chemistry solver did not converge in ', nstmax,' iterations'
  go to 9999
end if

23    continue

do i = 1,neq
  if (istiff(i) == 0) then

! ... normal species

    ypre(i) = yeq(i) + dt * ydpre(i)

  else

! ... stiff species

    dum = 2.0/(bpre(i) + small)
    eden = dum + dt
    enum = yeq(i) * (dum - dt) +  dum * dt * apre(i)
    ypre(i) = enum/(eden + small)
  end if

!   ypre(i) = max(ypre(i),yeq_lim(i)) !these limits cause problems

end do

! ... integrate with corrector step

do i = 1,neq
  yc1(i)  = ypre(i)
end do

dthalf = 0.5 * dt

do k = 1,niter
  ersums = 0.0
  ersumn = 0.0

! ... evaluate function for each corrector iteration

  call update_conc(yc1)
  call load_conc(conc)
  call get_prodloss(conc,prod,loss)
  if (nError /= NO_ERROR) go to 9999
  call load_ab(prod,loss,ac1,bc1)

  do i = 1, neq
    ydc1(i) = ac1(i) - bc1(i)*yc1(i)
  end do
  
  nfe = nfe + 1

  errorn = - 1.0
  errors = - 1.0

  do i = 1,neq
    if (istiff(i) == 0) then

! ... normal species

      yc2(i) = yeq(i) + dthalf * (ydc1(i) + ydpre(i))
!     yc2(i) = max(yc2(i),yeq_lim(i))

! ... evaluate normal species error

      eden = min(yc1(i),yc2(i))
      eden = max(eden,1.e-5)
      enum = yc2(i) - yc1(i)
      er =  abs(enum/(eden + small))
      ersumn = ersumn + er
      errorn = max(errorn,er)
    else

! ... stiff species

      dum  = one/(bc1(i) + small) + one/(bpre(i) + small)
      eden = dum + dt
      enum = dthalf*dum*(ac1(i) + apre(i)) + yeq(i)*(dum - dt)
      yc2(i) = enum/(eden + small)
!     yc2(i) = max(yc2(i),yeq_lim(i))

! ... evaluate stiff species error

      eden = min(yc1(i),yc2(i))
      eden = max(eden,1.e-5)
      enum = yc2(i) - yc1(i)
      er =  abs(enum/(eden + small))
      ersums = ersums + er
      errors = max(errors,er)
    end if
  end do

! ... convergence test

  if (nnorm > 0) ersumn = ersumn/nnorm
  if (nstiff > 0) ersums = ersums/nstiff
  kiter = k

! ... error control method is: max error < 4*eps
!    avg error < eps
  iconv = 1
  if (nnorm /= 0) then
    if (errorn > tepsn) iconv = -1
    if (ersumn > epsn)  iconv = -2
  end if
  if (nstiff/=0) then
    if (errors > tepss) iconv = -3
    if (ersums > epss)  iconv = -4
  end if
  if (iconv > 0) go to 80

  do i = 1,neq
    yc1(i)  = yc2(i)
  end do
end do

! ... no convergence after niter iterations

! ... select time step for next step

dt = max(dt*0.50,dtmin)

if (dt <= dtmin) then

! ... integration fails

  nError = IV_ERROR
  eRoutine = 'solve_ynb'
  eMessage = 'Error solving chemistry rate equations'
  eAction  = 'Time step too small'
  write(eInform,'(a,e12.4)') 'Dt = ', dt
  go to 9999
else
  go to 23
end if

! ... convergence obtained

80    continue

!pk
! === make sure O3 conc. does not go below specified limit
if (yc2(ido3) < o3lim) then
   diffo3 = o3lim - yc2(ido3)
   yc2(ido3) = o3lim
   yc2(idno) = yc2(idno) + diffo3
   yc2(idno2) = yc2(idno2) - diffo3
end if
!pk

do i = 1,neq
   yeq(i) = max(yc2(i),yeq_lim(i))
end do

tmnow = tmnow + dt
if (tmnow < tout) then

! ... select time step for next step

  if (kiter <= 2) then
    if (dt < 0.001) then
      if (kiter == 1) then
        dt = 10.*dt
      else
        dt =  3.*dt
      end if
    else if (dt < 0.10) then
      if (kiter == 1) then
        dt = 3.*dt
      else
        dt = 1.5*dt
      end if
    else
      if (kiter == 1) then
        dt = 1.30*dt
      else
        dt = 1.15*dt
      end if
    end if
  end if

  timremain = tout - tmnow
  dt = min(dt,timremain,dtmax)
  go to 18

else

! ... integration is completed

  go to 9999

end if

9999  continue

return
end

subroutine step_ode(dt,leqm_set)
!*******************************************************************************
!
! FUNCTION: Advance the gas-phase chemistry using LSODE
!           Hindmarsh, A.C. (1983), ODEPACK: A systemized collection of ODE 
!           solvers, in Numerical Methods for Scientific Computing (edited by
!           Stepleman, R.S., et al.), pp. 55-64, North-Holand, New York.
!
!           or using VODE
!
! PRECONDITIONS REQUIRED: Local arrays must be filled (ps%c and ps%m) 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         set_equilibrium              deriv_slow                set_dtsm
!                   lsode         get_lsode_error                  
!                   svode_
!
! REVISION HISTORY: 
! 21 MAR 2005  :  Limit the number of diagnostic messages to 10 - BC
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc
use files_inc
use diagnostics

implicit none

! --- ARGUMENTS

real dt           ! -- time step (sec)
logical leqm_set  ! -- whether or not equilibriums have been set already

! --- PARAMETERS

integer, parameter :: MAX_EQN_ODE  = MAX_MC
integer, parameter :: MAX_RWRK_ODE = 22 + 9*MAX_EQN_ODE + MAX_EQN_ODE**2
integer, parameter :: MAX_IWRK_ODE = 30 + MAX_EQN_ODE

! --- LOCALS

character*128 cmsg
real atols(MAX_EQN_ODE)
real yeq(MAX_MC)
real dtsm, dttotl, dtsmin
real ydots1(MAX_MC), ydots2(MAX_MC)
real tx, dely
real atol(MAX_EQN_ODE), rwork(MAX_RWRK_ODE)
real rpar(1)

integer i, neq, itol, itask, istate, mf, iopt, lrw, liw
integer ii, nn, maxstep, ir, iA, iB, nfast_sav
integer iwork(MAX_IWRK_ODE), ipar(1), neq_lsode(1)

external deriv_fast, deriv_fast_svode
external jacob, jacob_svode

!====   Set up LSODE

tx     = 0.0
itol   = 2
itask  = 1
istate = 1
iopt   = 1
lrw    = MAX_RWRK_ODE
liw    = MAX_IWRK_ODE
!mf     = 22 !internally generated jacobian
mf     = 21  !user-supplied jacobian
do ii = 5,10
  rwork(ii) = 0.0
  iwork(ii) = 0
end do
iwork(6) = 1000
rwork(6) = dt

!====   Update steady state species

nfast_sav = nfast
if (.not. leqm_set .and. neq_s > 0 .and. istage >= nstage-1 ) then
  call set_equilibrium(.false.)
  if (nError /= NO_ERROR) then
    nError = NO_ERROR
    write(lun_log,*) 'Error setting equilibrium in step_ode'
    do i = 1, neq_s
      indxf(i+nfast) = indx_eq_s(i,istage) + nspecies
    end do
    nfast = nfast + neq_s
    neq_s = 0
  else
    leqm_set = .true.
  end if
end if

!===== Save initial equilibrium concentrations

do i = 1, nequilibrium
  nn = i + nspecies
  concsav(nn) = ps(nn)%c
end do

!====   Find atol for fast species

neq = nfast
do i = 1,nfast
  nn = indxf(i)
  atol(i) = species(nn)%tol
  if(.not.IsStar(nn) .and. vol /= 0.)then
    atol(i) = atol(i)*vol
  end if
end do

!====   Find atol for turbulent correlations
do i = 1, ncorrt
  ir = indx_corr(i)
  iA  = reaction(ir)%iA
  iB  = reaction(ir)%iB
  neq = neq + 1
  if (vol /= 0.) then
    atol(neq) = species(iA)%tol*species(iB)%tol*vol
  else
    atol(neq) = species(iA)%tol*species(iB)%tol
  end if
end do

!====   Find atol for slow species
do i = 1,nslow
  nn = indxs(i)
  atols(i) = species(nn)%tol
  if(.not.IsStar(nn) .and. vol /= 0.)then
    atols(i) = atols(i)*vol
  end if
end do

!====  Initialize time stepping

maxstep = 100
dtsmin  = dt/float(maxstep)
dttotl  = 0.


!====== Beginning of time loop

Timeloop: do while (dttotl < dt)
  dtsm = dt


!====== Advance slow species

  SlowSpecies: if(nslow > 0) then

! calculate derivative

    call deriv_slow(ydots1)
    if (nError /= NO_ERROR) go to 9999

! set time step

    call set_dtsm(ydots1, atols, dt, dttotl, dtsmin, dtsm)
    if (nError /= NO_ERROR) go to 9999

! step

    do i = 1,nslow
      nn = indxs(i)
      if(IsStar(nn))then
        ps(nn)%c = ps(nn)%c + dtsm*ydots1(i)
      else
        ps(nn)%m = ps(nn)%m + dtsm*ydots1(i)
      end if
    end do

  end if SlowSpecies

!====== Advance fast species

  FastSpecies: if (nfast + ncorrt > 0) then

! load LSODE array

    rwork(6) = dtsm
    neq = nfast
    do i = 1,nfast
      nn = indxf(i)
      if(IsStar(nn))then
        yeq(i) = ps(nn)%c
      else
        yeq(i) = ps(nn)%m
      end if
    end do
    do i = 1, ncorrt
      neq = neq + 1
      yeq(neq) = corr(i)
    end do

    if (isolve == ID_LSODE) then
      
      neq_lsode(1) = neq 
      call lsode(deriv_fast,neq_lsode,yeq,tx,tx+dtsm,itol,rtol,atol, &
                itask,istate,iopt,rwork,lrw,iwork,liw,jacob,mf)

    else
      
      call SVODE_(deriv_fast_svode,neq,yeq,tx,tx+dtsm,itol,rtol,atol, &
                itask,istate,iopt,rwork,lrw,iwork,liw,jacob_svode,mf,rpar,ipar)
    
    end if

    if( istate < 0 ) then

      nError = IV_ERROR
      eRoutine = 'step_ode'
      eMessage = 'Negative value of istate returned from lsode or vode'
      eAction = 'See the log file for lsode/vode messages'
      write(eInform,'(a,i4)') 'Istate is',istate
      call get_lsode_error(istate,cmsg)
      write(lun_log,*) TRIM(cmsg)
      if ( nbad_chem < 10) then
        write(lun_log,*) 'Values in last solution (yeq,atol):'
        do i = 1, nfast
          nn = indxf(i)
          write(lun_log,332) species(nn)%name,yeq(i),atol(i)
        end do
        if (vol /= 0.) then
          do i = 1, ncorrt
            ir = indx_corr(i)
            iA  = reaction(ir)%iA
            iB  = reaction(ir)%iB
            write(lun_log,332) TRIM(species(iA)%name)//'-'//species(iB)%name, &
           yeq(nfast+i)/vol,atol(nfast+i)/vol
          end do
        else
          do i = 1, ncorrt
            ir = indx_corr(i)
            iA  = reaction(ir)%iA
            iB  = reaction(ir)%iB
            write(lun_log,332) TRIM(species(iA)%name)//'-'//species(iB)%name, &
           yeq(nfast+i),atol(nfast+i)
          end do
          write(lun_log,*) 'Stepping the ambient'
        end if
      end if
      go to 9999

    end if
332 format(a9,2x,1p,2e12.4)

! unload LSODE array

    neq = nfast
    do i = 1,nfast
      nn = indxf(i)
      if(IsStar(nn))then
        ps(nn)%c = max(conc_lim(nn),yeq(i))
      else
        ps(nn)%m = yeq(i)
      end if
    end do
    do i = 1, ncorrt
      neq = neq + 1
      corr(i) = yeq(neq)
    end do
  end if FastSpecies

!====== Correct slow species

  if(nslow > 0) then

! calculate derivative

    call deriv_slow(ydots2)
    if (nError /= NO_ERROR) go to 9999

! correct
    do i = 1, nslow
      nn = indxs(i)
      dely = dtsm*0.5*(ydots2(i)-ydots1(i))
      if(IsStar(nn))then
        ps(nn)%c = max(conc_lim(nn),ps(nn)%c + dely)
      else
        ps(nn)%m = ps(nn)%m + dely
      end if
    end do

  end if

!====== Advance time

  dttotl = dttotl + dtsm
end do  Timeloop

!===== End  of time loop

nfast = nfast_sav

9999    return
end

subroutine set_chem_stage(leqm_set)
!*******************************************************************************
!
! FUNCTION:  Set the puff chemical stage
!
! PRECONDITIONS REQUIRED: Must be using staged chemistry (lstage = .true.)
!
! SUBROUTINES AND FUNCTIONS CALLED:
!         set_equilibrium                   ldark
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc
use files_inc

implicit none

! --- ARGUMENTS

logical leqm_set  ! -- whether or not the equilibriums are already set

! --- LOCALS

integer i, nn, iA, iB
logical lchange, ldark
real lim, kr1, kr2, to3, toh, tno3, tho2, cA, cB, denom

! == stage 3 (no change)

if (istage == 3) go to 9998

! == set total ozone concentration

to3  = max(0.,ps(ikey_spec(O3))%c  + ps(ikey_spec(O3))%a)

! == stage 1 to stage 2 criteria  (acid formation important)

Stage1:  if (istage == 1) then
  istage = 2
  if (.not. leqm_set) then
    call set_equilibrium(.false.)
    if (nError /= NO_ERROR) then
      nError = NO_ERROR
      istage = 3
      write(lun_log,*) 'Error setting equilibrium in set_chem_stage'
      write(lun_log,*) 'Advanced stage 1 to stage 3'
      go to 9998
    end if
    leqm_set = .true.
  end if
  lim = phno3/reaction(ikey_rxn(IPHNO3))%k
  toh  = max(0.,ps(ikey_spec(OH))%c  + ps(ikey_spec(OH))%a)
  lchange = (toh >= lim)
  if (.not. lchange .and. ldark()) then  !night
    lim = pno3/reaction(ikey_rxn(IPNO3))%k
    tno3 = max(0.,ps(ikey_spec(NO3))%c + ps(ikey_spec(NO3))%a)
    lchange =  (to3 >= lim .and. tno3 >= cno3)
  end if
  if (.not. lchange) istage = 1
end if Stage1

! == stage 2 to stage 3 criteria  (ozone formation important)

Stage2 : if (istage == 2) then
  if (to3 > co3) then
    if (.not. leqm_set) then
      call set_equilibrium(.false.)
      if (nError /= NO_ERROR) then
        nError = NO_ERROR
        istage = 3
        write(lun_log,*) 'Error setting equilibrium in set_chem_stage'
        write(lun_log,*) 'Advanced stage 2 to stage 3'
        go to 9998
      end if
      leqm_set = .true.
    end if
    kr1 = reaction(ikey_rxn(INOHO2))%k
    kr2 = reaction(ikey_rxn(INOO3))%k
    tho2 = max(0.,ps(ikey_spec(HO2))%c + ps(ikey_spec(HO2))%a)
    denom = kr2*to3
    if (denom > 0.) then
      lim = kr1*tho2/denom
      lchange = (lim >= rho2o3)
    else
      lchange = .true.
    end if  
  else
    lchange = .false.
  end if
  if (.not. lchange .and. ldark()) then  !night
    if (.not. leqm_set) then
      call set_equilibrium(.false.)
      if (nError /= NO_ERROR) then
        nError = NO_ERROR
        istage = 3
        write(lun_log,*) 'Error setting equilibrium in set_chem_stage'
        write(lun_log,*) 'Advanced stage 2 to stage 3'
        go to 9998
      end if
      leqm_set = .true.
    end if
    tno3 = max(0.,ps(ikey_spec(NO3))%c + ps(ikey_spec(NO3))%a)
    if (tno3 >= cno3) then
      lim = 0.
      do i = 1, nreact_voc  !VOC + NO3 -->
        nn = indx_react_voc(i)
        kr1 = reaction(nn)%fB*reaction(nn)%k
        iA = reaction(nn)%iA
        iB = reaction(nn)%iB
        cA = max(0.,ps(iA)%c+ps(iA)%a)
        cB = max(0.,ps(iB)%c+ps(iB)%a)
        lim = lim + kr1*cA*cB
      end do
      kr2 = reaction(ikey_rxn(INO3NO2))%k
      iA  = ikey_spec(NO2)
      iB  = ikey_spec(NO3)
      cA = max(0.,ps(iA)%c+ps(iA)%a)
      cB = max(0.,ps(iB)%c+ps(iB)%a)
      denom = kr2*cA*cB
      if (denom > 0.) then
        lchange = (lim > rvocno2*denom)
      else
        lchange = .true.
      end if
    else
      lchange = .false.
    end if
  end if
  if (lchange) then
    istage = 3
    leqm_set = .false.
  end if
end if Stage2

9998  continue    

if (lstage) then
  nrxn_curr = 0
  do i = 1, nreact_s(istage)
    if (reaction(i)%k > 0.) then
      nrxn_curr = nrxn_curr + 1
      indx_rxns(nrxn_curr) = i
    end if
  end do
  nfast = nfast_s(istage)
  nslow = nslow_s(istage)
  neq_s = neqm_s(istage)
  do i = 1, nfast
    indxf(i) = indxf_s(i,istage)
  end do
  do i = 1, nslow
    indxs(i) = indxs_s(i,istage)
  end do
end if

return
end

subroutine set_dtsm(ydot, atol, dt, dttotl, dtsmin, dtsm)
!*******************************************************************************
!
! FUNCTION:   Set the small timestep for step_ode for the slow species
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          WarningMessage                  set_staged_species
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use param_inc
use multcomp_inc
use files_inc
use error_inc

implicit none

! --- ARGUMENTS

real ydot(*), atol(*)  ! -- derivatives and tolerances for the slow species
real dt                ! -- large time step (sec)
real dttotl            ! -- total time stepped so far (sec), goes to dt
real dtsmin            ! -- minimum small time step allowed (sec)
real dtsm              ! -- small time step for slow species (sec)

! --- LOCALS

real tchem(MAX_MC), deriv(MAX_MC), tol_tmp(MAX_MC)
real yval, eps
integer Indx_too_fast(MAX_MC), ifast, nn, i, j

ifast = 0
do i = 1, nslow
  nn    = indxs(i)
  if (ydot(i) /= 0.0) then
    if (IsStar(nn))then
      yval = ps(nn)%c
    else
      yval = ps(nn)%m
    end if
    eps   = rtol(1)*abs(yval) + atol(i)
    tchem(nn)  = 10.*eps/(abs(ydot(i)))
    if( tchem(nn) <= dtsmin ) then
      ifast = ifast + 1
      Indx_too_fast(ifast) = nn
      deriv(nn) = ydot(i)
      tol_tmp(nn)   = atol(i)
    end if
    dtsm  = min(dtsm, tchem(nn))
  end if
end do

dtsm = max(dtsmin, dtsm)

if( dtsm == dtsmin ) then

  write(lun_log, *)'The following species are now modeled as fast:'
  do i = 1, ifast
    nn = Indx_too_fast(i)
    write(lun_log, '(A8,/, 6(A8, 1p,e12.4,/) )') species(nn)%name, &
         'tchem = ', tchem(nn), 'dt = ', dtsm,'rate = ',deriv(nn), &
         'mass = ', ps(nn)%m,'conc = ', ps(nn)%c &
         , 'atol = ',tol_tmp(nn)
    if (IsStar(nn)) then
      write(lun_log,*) 'Determined by concentration rate of change'
    else
      write(lun_log,*) 'Determined by mass rate of change'
    end if
    species(nn)%class = ID_SPECIES_FAST !reset to fast
  end do
  nfast = 0
  nslow = 0
  ! -- reset the species classes
  do i = 1, nspecies
    if(species(i)%class == ID_SPECIES_FAST)then
      nfast = nfast + 1
      indxf(nfast) = i
    else if(species(i)%class == ID_SPECIES_SLOW)then
      nslow = nslow + 1
      indxs(nslow) = i
    end if
  end do
  
  ! -- reset the reaction classes 
  do i = 1, nreacts
    if (reaction(i)%iA == ID_SPECIES_FAST .or. &
        reaction(i)%iB == ID_SPECIES_FAST) then
      call SetFast(reaction(i)%class)
    else if (reaction(i)%iA == ID_SPECIES_SLOW .or. &
             reaction(i)%iB == ID_SPECIES_SLOW) then
      call SetSlow(reaction(i)%class)
    end if
    do j = 1, reaction(i)%nP
      if (reaction(i)%iP(j) == ID_SPECIES_FAST) then
        call SetFast(reaction(i)%class)
      else if (reaction(i)%iP(j) == ID_SPECIES_SLOW) then
        call SetSlow(reaction(i)%class)
      end if
    end do
  end do
   
  if (lstage) then
    call set_staged_species
  end if

  nError   = IV_ERROR
  eRoutine = 'set_dtsm'
  eMessage = 'Fast species modeled as slow in the plume'
  eInform  = 'Species changed permanently to fast (see log file)'
  eAction  = char(0)

end if

if (dttotl + dtsm > dt) dtsm = dt - dttotl

return
end

subroutine deriv_fast_svode(neq, tdum, yeq, ydot, rpar, ipar)
!*******************************************************************************
!
! FUNCTION:  Set the derivatives for the fast species
!            called from SVODE
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               deriv_fast 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

implicit none

! --- ARGUMENTS

integer neq          ! -- number of equations/species
real    tdum         ! -- not used
real yeq(*), ydot(*) ! -- species concentrations and derivatives
real rpar(*)         ! -- real array for extra info (not used)
integer ipar(*)      ! -- integer array for extra info (not used)

call deriv_fast(neq, tdum, yeq, ydot)

return
end

subroutine deriv_fast(neq, tdum, yeq, ydot)
!*******************************************************************************
!
! FUNCTION:  Set the derivatives for the fast species
!            called from LSODE
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               load_conc            get_prodloss               
!
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

integer neq          ! -- number of equations/species
real    tdum         ! -- not used
real yeq(*), ydot(*) ! -- species concentrations and derivatives

! --- LOCALS

integer i, nn, j
real prod(MAX_MC), loss(MAX_MC), conc(MAX_MC)

if (nError /= NO_ERROR) go to 9998

!====   Update working arrays from LSODE species array

j = nfast
do i = 1,nfast
  nn = indxf(i)
  if(IsStar(nn))then
    ps(nn)%c = yeq(i)
  else
    ps(nn)%m = yeq(i)
  end if
end do
do i = 1, ncorrt
  j = j + 1
  corr(i) = yeq(j)
end do

!===== Load conc array

call load_conc(conc)

!====   Compute derivatives

call get_prodloss(conc,prod,loss)
if (nError /= NO_ERROR) go to 9998

!====   Load LSODE derivative array

j = nfast
do i = 1,nfast
  nn = indxf(i)
  ydot(i) = prod(nn) - loss(nn)*conc(nn)
end do
do i = 1, ncorrt
  j = j + 1
  nn = nspectot+nambient+i
  ydot(j) =  prod(nn) - loss(nn)*conc(nn)
end do

9999    return

9998  do i = 1, neq
  ydot(i) = 0.
end do
go to 9999

end

subroutine jacob_svode(neq, tdum, yeq, ml, mu, pd, ndim1, rpar, ipar)
!*******************************************************************************
!
! FUNCTION:  Calculate fast species jacobian, called from LSODE
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                    jacob
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS

integer neq      ! -- number of ODEs solved
integer ml, mu   ! -- half-band width parameters
integer ndim1    ! -- dimension for jacobian
real    tdum     ! -- time
real yeq(*)      ! -- concentrations
real pd(ndim1,*) ! -- jacobian
real rpar(*)     ! -- real array for extra info (not used)
integer ipar(*)  ! -- integer array for extra info (not used)

call jacob(neq, tdum, yeq, ml, mu, pd, ndim1)

return
end

subroutine jacob(neq, tdum, yeq, ml, mu, pd, ndim1)
!*******************************************************************************
!
! FUNCTION:  Calculate fast species jacobian, called from LSODE
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                    jacl                jacq_amb                    jacq
!                            
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

integer neq      ! -- number of ODEs solved
integer ml, mu   ! -- half-band width parameters
integer ndim1    ! -- dimension for jacobian
real    tdum     ! -- time
real yeq(*)      ! -- concentrations
real pd(ndim1,*) ! -- jacobian

! --- LOCALS

integer i, j, nn, mm, ir, ip
integer iA, iB, ii, jj
real    pdm(MAX_MC,MAX_MC)
real    rk

!====   Clear working jacobian arrays

do i = 1, nspectot+nambient
  do j = 1, nspectot+nambient
    pdm(i, j) = 0.
  end do
  pdm(i, i) = 0.
end do

j = nfast
do i = 1,nfast
  nn = indxf(i)
  if(IsStar(nn))then
    ps(nn)%c = yeq(i)
  else
    ps(nn)%m = yeq(i)
  end if
end do
do i = 1, ncorrt
  j = j + 1
  corr(i) = yeq(j)
end do

!====   Compute jacobian
do ir = 1,nrxn_curr
  i = indx_rxns(ir)

  if (btest(reaction(i)%class,ID_REACT_FAST)) then
    if (IsLinear(i))then
      call jacl(reaction(i), pdm)
    else
      if (lflag_amb) then
        call jacq_amb(reaction(i), pdm)
      else
        call jacq(reaction(i), pdm)
      end if
    end if
  end if
end do

!====   Load LSODE jacobian array

do i = 1,nfast
  nn = indxf(i)
  do j = 1, nfast
    mm = indxf(j)
    pd(i,j) = pdm(nn,mm)
  end do
end do

!====   If correlations - add to jacobian array

if (ncorrt > 0 .and. vol /= 0.) then

!---   load correlations column of pd
  do j = 1, ncorrt
    ir = indx_corr(j)
    iA = reaction(ir)%iA
    iB = reaction(ir)%iB
    rk = reaction(ir)%k
    jj = j + nfast
    do i = 1, nfast
      if (indxf(i) == iA) then
        pd(i, jj) = -rk/vol
      else if (indxf(i) == iB) then
        pd(i, jj) = -reaction(ir)%fB*rk/vol
      else
        do ip = 1, reaction(ir)%nP
          if (indxf(i) == reaction(ir)%iP(ip)) then
            pd(i,jj) = reaction(ir)%fP(ip)*rk/vol
          end if
        end do
      end if
    end do
  end do

!---     load correlation rows of pd
  do i = 1, ncorrt
    ii = i + nfast
    do j = 1, ncorrt
      jj = nfast + j
      if (i == j) pd(ii, jj) = -chem_damp(i)
    end do
  end do

end if


return
end

subroutine jacl(react, pdm)
!*******************************************************************************
!
! FUNCTION: Calculate linear reaction jacobian, called by jacob
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

use param_inc
use multcomp_inc

implicit none

! --- ARGUMENTS

type (chem_reaction) react  ! -- reaction structure
real  pdm(MAX_MC,MAX_MC)    ! -- array to store the jacobian (dA/dB, etc.)

! --- LOCALS

real dc
integer i

!====   Set deriv with respect to A

dc = react%k

!====   Adjust derivative

pdm(react%iA, react%iA) = pdm(react%iA, react%iA) - dc

do i = 1,react%nP
  pdm(react%iP(i), react%iA) = pdm(react%iP(i), react%iA) + &
                               dc*react%fP(i)
end do

return
end


subroutine jacq(react, pdm)
!*******************************************************************************
!
! FUNCTION: Calculate quadratic reaction jacobian, called by jacob
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

use param_inc
use multcomp_inc

implicit none

! --- ARGUMENTS

type (chem_reaction) react  ! -- reaction structure
real  pdm(MAX_MC,MAX_MC)    ! -- array to store the jacobian (dA/dB, etc.)

! --- LOCALS

integer iA, iB, i
real    dcadca, dcadcb

!====   Set local pointer

iA = react%iA
iB = react%iB

if (iA==iB .and. ps(iA)%c + ps(iA)%a < 0.) return 

!====   Set derivatives with respect to A and iB

dcadca = react%k*(ps(iB)%c + ps(iB)%a)
dcadcb = react%k*(ps(iA)%c + ps(iA)%a)

!====   Adjust jacobian

pdm(iA,iA)  = pdm(iA,iA) - dcadca
pdm(iA,iB)  = pdm(iA,iB) - dcadcb
pdm(iB,iA)  = pdm(iB,iA) - dcadca*react%fB
pdm(iB,iB)  = pdm(iB,iB) - dcadcb*react%fB

do i = 1,react%nP
  pdm(react%iP(i), iA) = pdm(react%iP(i), iA) + dcadca*react%fP(i)
  pdm(react%iP(i), iB) = pdm(react%iP(i), iB) + dcadcb*react%fP(i)
end do

return
end

subroutine update_conc(yeq)
!*******************************************************************************
!
! FUNCTION:  Load conc array from Young & Boris yeq array
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

use multcomp_inc

implicit none

! --- ARGUMENTS

real    yeq(*)  ! -- Solver array of concentrations

! --- LOCALS

integer i, neq, nn

neq = nfast
do i = 1,nfast
  nn = indxf(i)
  if (IsStar(nn)) then
    ps(nn)%c = yeq(i)
  else
    ps(nn)%m = yeq(i)
  end if
end do

do i = 1,nslow
  neq = neq + 1
  nn = indxs(i)
  if (IsStar(nn)) then
    ps(nn)%c = yeq(neq)
  else
    ps(nn)%m = yeq(neq)
  end if
end do

if (ncorrt /= 0) then
  do i = 1, ncorrt
    neq = neq + 1
    corr(i) = yeq(neq)
  end do
end if

return
end

subroutine load_conc(conc)
!*******************************************************************************
!
! FUNCTION:  Load the Young & Boris conc array from ps%m and ps%c
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

use multcomp_inc

implicit none

! --- ARGUMENTS

real    conc(*)  ! -- Values being used
                 ! -- either mass or conc, followed by ambient and correlations

! --- LOCALS

integer i, j

!===== Load conc array

do i = 1,nspectot
  if (IsStar(i)) then
    conc(i) = ps(i)%c
  else
    conc(i) = ps(i)%m
  end if
end do

j = nspectot
do i = 1, nambient
  j = j + 1
  conc(j) = ps(j)%c
end do
do i = 1, ncorrt
  j = j + 1
  conc(j) = corr(i)
end do


return
end

subroutine load_ab(prod,loss,ayeq,byeq)
!*******************************************************************************
!
! FUNCTION: Load a and b from prod and loss for Young & Boris solver
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

real    prod(*), loss(*) ! -- dc/dt = prod - loss*c
real    ayeq(*), byeq(*) ! -- prod and loss in order of yeq 

! --- LOCALS

integer i, j, nn

j = nfast
do i = 1, nfast
  nn = indxf(i)
  ayeq(i) = prod(nn)
  byeq(i) = loss(nn)
end do
do i = 1, nslow
  j = j + 1
  nn = indxs(i)
  ayeq(j) = prod(nn)
  byeq(j) = loss(nn)
end do
do i = 1, ncorrt
  j = j + 1
  nn = nspectot+nambient+i
  ayeq(j) = prod(nn)
  byeq(j) = loss(nn)
end do

return
end

subroutine deriv_slow(ydot)
!*******************************************************************************
!
! FUNCTION: Set the derivatives for the slow species
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!               load_conc            get_prodloss
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

real    ydot(*)  ! -- derivative for slow species

! --- LOCALS

integer i, nn
real conc(MAX_MC),prod(MAX_MC),loss(MAX_MC)

!===== Load conc array

call load_conc(conc)

!====   Compute derivatives

call get_prodloss(conc,prod,loss)
if (nError /= NO_ERROR) go to 9999

do i = 1,nslow
  nn = indxs(i)
  ydot(i) = prod(nn) - loss(nn)*conc(nn)
end do

9999    return
end

subroutine get_prodloss(conc, prod ,loss)
!*******************************************************************************
!
! FUNCTION: Calculate production and loss terms for all species
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               reactl_pl           reactq_pl_amb               reactq_pl
!                
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_mc_puf
use multcomp_inc
use step_p_inc
use files_inc
use common_mpi

implicit none

! --- ARGUMENTS

real prod(*), loss(*)  ! -- dc/dt = prod - loss*c
real conc(*)           ! -- species concentrations

! --- LOCALS

integer i, ir, j, iA, iB, nn, ii
real tchem, cA, cB

!====   Update steady state species  (proper but time-consuming)

!call set_equilibrium(.false.)
!if (nError /= NO_ERROR) go to 9999

!====   Clear working derivative arrays

do i = 1,nspectot+nambient
  prod(i) = 0.
  loss(i) = 0.
end do

!====   Compute production and loss terms

do ir = 1, nrxn_curr
  i = indx_rxns(ir)
  if(IsLinear(i))then
    call reactl_pl(conc,reaction(i),prod,loss)
  else
    if (lflag_amb) then
      call reactq_pl_amb(conc,reaction(i),prod,loss)
    else
      call reactq_pl(conc,reaction(i),prod,loss)
    end if
  end if
end do

if (ncorrt /= 0 .and. vol /= 0.) then
  j = nspectot + nambient
  do i = 1, ncorrt
    j = j + 1
    prod(j) = 0.
    ir = indx_corr(i)
    iA = reaction(ir)%iA
    iB = reaction(ir)%iB
    loss(j) = qosi_cc
    if (conc(j) > 0. ) then
      cA = max(conc(iA)+ps(iA)%a,species(iA)%tol)
      cB = max(conc(iB)+ps(iB)%a,species(iB)%tol)
      tchem = abs((prod(iA)/cA) - loss(iA)) &
            + abs((prod(iB)/cB) - loss(iB))
      loss(j) = loss(j) + tchem
    end if
    chem_damp(i) = loss(j)
  end do
end if

if (isolve == ID_YNB) then

!====   Update steady state species

  if (neq_s > 0) then
    do i = 1, neq_s
      nn = indx_eq_s(i,istage) + nspecies
      if (abs(loss(nn)) >= 1.e-30) then
        ps(nn)%c = prod(nn)/loss(nn)
      else
        write(lun_log,*) 't = ',t
        write(lun_log,*) 'lflag_amb = ',lflag_amb
        write(lun_log,*) 'neq_s, istage = ',neq_s,istage
        write(lun_log,*) 'Unable to solve for:  ',TRIM(species(nn)%name)
        write(lun_log,190) ' Species  ','     Conc   ','  Production' &
           ,'    Loss    '
        do ii = 1, nspectot
          write(lun_log,200) species(ii)%name,ps(ii)%c,prod(ii),loss(ii)
        end do
190     format(a10,3a12)
200     format(a10,1p,3e12.4)
        nError = IV_ERROR
        eRoutine = 'get_prodloss'
        eMessage = 'Zero destruction rate : unable to solve for '//TRIM(species(nn)%name)
        eInform = 'Changing from equilibrium to fast species'
        eAction = char(0)
        go to 9999
      end if
    end do
  end if

end if

9999  continue

return
end

subroutine reactl_pl(conc, react, prod ,loss)
!*******************************************************************************
!
! FUNCTION:  Calculate the production and loss terms for a linear reaction
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

real prod(*), loss(*)      ! -- dc/dt = prod - loss*c
real conc(*)               ! -- species concentrations
type (chem_reaction) react ! -- reaction structure

! --- LOCALS

real kr
integer i, iP, iA

!====   Set rate

kr = react%k

!====   Adjust derivative

iA = react%iA
loss(iA) = loss(iA) + kr

do i = 1,react%nP
  iP = react%iP(i)
  prod(iP) = prod(iP) + kr*react%fP(i)*conc(iA)
end do

return
end

subroutine reactq_pl(conc, react, prod, loss)
!*******************************************************************************
!
! FUNCTION: Calculate the production and loss terms for a quadratic reaction
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
! 18 OCT 2006 : Correct sign of cr for reaction of same species. -BC  
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none

! --- ARGUMENTS

real prod(*), loss(*)      ! -- dc/dt = prod - loss*c
real conc(*)               ! -- species concentrations
type (chem_reaction) react ! -- reaction structure

! --- LOCALS

integer iA, iB, i, ioff
real    mr, cr, kr, ft

!====   Set local pointer

iA = react%iA
iB = react%iB

!====   Set rate

kr = react%k

if (react%icorr /= 0) then
  mr = vol*(conc(iA)+ps(iA)%a)*(conc(iB)+ps(iB)%a)
  if(mr > 0.) then
  ioff = react%icorr + nspectot + nambient
    ft = 1. + conc(ioff)/mr
    ft = max(ft, 1.e-6)
    kr = ft*kr
  end if
end if

!====   Adjust production and loss terms

! dAp/dt = -k(ApBp + ApBa + AaBp) = -kAp(Bp+Ba) - kAaBp
!   = -loss*Ap    + prod

if (iA==iB .and. conc(iA) + ps(iA)%a < 0.) then 
  cr       =  -kr*ps(iA)%a*ps(iA)%a
  prod(iA) =  prod(iA) - 2.*cr
else
  loss(iA) = loss(iA) + kr*(conc(iB) + ps(iB)%a)
  prod(iA) = prod(iA) - kr*ps(iA)%a*conc(iB)

  loss(iB) = loss(iB) + kr*react%fB*(conc(iA) + ps(iA)%a)
  prod(iB) = prod(iB) - kr*react%fB*conc(iA)*ps(iB)%a

  cr = kr*(conc(iA)*conc(iB) + ps(iA)%a*conc(iB) + conc(iA)*ps(iB)%a)
end if

do i = 1,react%nP
  prod(react%iP(i)) = prod(react%iP(i)) + cr*react%fP(i)
end do

return
end

subroutine inter_mc(pmi,pmj,ipuf,jpuf)
!*******************************************************************************
!
! FUNCTION:  Set Multicomponent interactions
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
! 21 MAR 2005 : Remove restriction based on mass in inter_mc -BC
!
!*******************************************************************************

! --- MODULES

use common_puf
use multcomp_inc
use inter_inc
use cstar_inc

implicit none

! --- ARGUMENTS

type ( puff_mc  ) pmi, pmj  ! -- puff multicomponents structures
integer ipuf, jpuf          ! -- puff numbers

! --- LOCALS

integer ism, is, ispuf, jspuf
real    fmaxi, fmaxj, pvol

!====   Loop over multicomponents species and set correlations

pvol  = pi3*sqrt(puff(jpuf)%det)
if (pvol ==  0.) then
  fmaxj = 0.0
else
  CALL get_fmax(ipuf,jpuf,fmaxj)
  fmaxj = fmaxj/pvol
end if
pvol  = pi3*sqrt(puff(ipuf)%det)
if (pvol ==  0.) then
  fmaxi = 0.0
else
  CALL get_fmax(jpuf,ipuf,fmaxi)
  fmaxi = fmaxi/pvol
end if

ispuf = ipuf - i0_smax
jspuf = jpuf - i0_smax

do is = 1,nspecies
  ism = species(is)%star
  if(ism > 0)then
    pmi%mc(ism) = pmi%mc(ism) + fac*pmj%mc(is)
    species_max(ispuf,is) = species_max(ispuf,is) + fmaxj*pmj%mc(is)
    if(.not.lstatic)then
      pmj%mc(ism) = pmj%mc(ism) + fac*pmi%mc(is)
      species_max(jspuf,is) = species_max(jspuf,is) + fmaxi*pmi%mc(is)
    end if
  end if
end do

!===    Save inverse volume at end of mc array

is = 2*nspectot+ncorrm+ncorrt+1
pmi%mc(is) = pmi%mc(is) + fac
if(.not.lstatic)then
  pmj%mc(is) = pmj%mc(is) + fac
end if

return
end

subroutine clear_mc(pm)
!*******************************************************************************
!
! FUNCTION: Clear Multicomponent interactions
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
! 07/24/2001 : Corrected pointers for turbulent correlations (BC)
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

type ( puff_mc  ) pm  ! -- puff multicomponent structure

! --- LOCALS

integer i

!====   Loop over multicomponents interactions

do i = 1,ncorrm
  pm%mc(nspectot+ncorrt+i) = 0.0
end do

!===    Clear inverse volume
i = 2*nspectot+ncorrm+ncorrt+1
pm%mc(i) = 0.0

return
end

subroutine chk_chem_split(pm,lspltch)
!*******************************************************************************
!
! FUNCTION: Check if puffs should be split based on chemical criteria
!
! PRECONDITIONS REQUIRED: Only used with staged chemistry
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

type ( puff_mc ) pm   ! -- puff multicomponent structure
logical lspltch       ! -- flag that specifies if puff should be split

! --- PARAMETERS

real, parameter :: FRAC1 = 0.05, FRAC2 = 0.20

! --- LOCALS

integer ioff
real sO3, sNO, sNO2

lspltch = .false.

ioff = 2*nspectot + ncorrm + ncorrt + 2
istage = nint(pm%mc(ioff) + 0.45)
if ( .not. (istage == 1 .or. istage == 2 &
 .or. istage == 3) ) then
  nError = IV_ERROR
  eRoutine = 'chk_chem_split'
  eMessage = 'Istage is not 1, 2, or  3'
  eAction = ' Alert model developer'
  write(eInform,'(a,i4)') 'Istage is',istage
  go to 9999
end if

if(istage == 3) then
  ioff  = nspectot + ncorrm + ncorrt    !end of chemistry concentrations
  sO3   = pm%mc(ioff + ikey_spec(O3))
  sNO   = pm%mc(ioff + ikey_spec(NO)) - ps(ikey_spec(NO))%a
  sNO2  = pm%mc(ioff + ikey_spec(NO2)) - ps(ikey_spec(NO2))%a
  lspltch = (sNO + sNO2 > FRAC1*ps(ikey_spec(O3))%a) &
            .and. (sO3 < (1. + FRAC2)*ps(ikey_spec(O3))%a) &
            .and. (sO3 > (1. - FRAC2)*ps(ikey_spec(O3))%a)
end if

9999    return
end

subroutine set_ps_from_mc(pm,carea,area_fac)
!*******************************************************************************
!
! FUNCTION:  Set up working arrays
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         set_equilibrium   set_vdry_aerosol_chem   set_wash_aerosol_chem
!       set_wash_gas_chem                  
!
! REVISION HISTORY: 
!
! 07/24/2001   : Corrected pointers for turbulent correlations (BC)
! JAN/FEB 2005 : Default aerosol dry dep and washout calculations only if
!                chemical aerosol option is not selected (PK, AER)
!                Also account for additional particle types (PK, AER)
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc
use files_inc
use common_mpi

implicit none

! --- ARGUMENTS

type ( puff_mc ) pm   ! -- puff multicomponent structure
real carea, area_fac  ! -- puff area and area factor

! --- LOCALS

integer i, ioff, j, isec, nn, iA, iB
real vdry_aer(MAX_SEC), taur(MAX_SEC), taur_gas
real vfall, vdtot, cA, cB

!===  Initialize ambient equilibrium
!===  in order to set perturbation eqm conc
istage = nstage
call set_equilibrium(.true.)
if (nError /= NO_ERROR) then
  write(lun_log,*) 'Error setting ambient equilibrium in set_ps_from_mc'
  write(lun_log,*) 'Using previously set concentrations'
  nError = NO_ERROR
end if

do i = 1,nspectot+nambient
  ps(i)%m      = 0.
  ps(i)%c      = 0.
  ps(i)%taudry = 0.
  ps(i)%tauwet = 0.
end do

!===  Load ps%c with perturbation concentrations
do i = 1,nspecies
  ps(i)%m = pm%mc(i)
  if(IsStar(i))then
    ps(i)%c = max(pm%mc(species(i)%star),-ps(i)%a)
  else
    ps(i)%c = 0.
  end if
  ps(i)%taudry = species(i)%vdep*carea
end do

!===  Default wash out and dry dep for aerosol particle materials
if (.not. laerosol) then
  if (naerp > 0) then
    do isec = 1, nsec_aer
      call set_vdry_aerosol_chem(carea,isec,vdry_aer(isec),vfall)
      call set_wash_aerosol_chem(isec,taur(isec))
    end do
    do i = 1, naerp
      j    = index_aerp(i)
      isec = index_sec(i)
      vdtot = vfall*area_fac + vdry_aer(isec)
      ps(j)%taudry = vdtot*carea
      ps(j)%tauwet = taur(isec)
    end do
  end if
end if

if (nequilibrium > 0) then
  ioff = nspecies
  do i = 1,nequilibrium
    ps(i+ioff)%m = pm%mc(i+ioff)
    ps(i+ioff)%c = 0.
    ps(i+ioff)%taudry = species(i+ioff)%vdep*carea
  end do
end if

if (nambient > 0) then
  ioff = nspectot
  do i = 1,nambient
    ps(i+ioff)%m = 0.
    ps(i+ioff)%c = 0.
  end do
end if

!====   Set corr (A'B') from MC array

do i = 1, MAX_NCORR
  corr(i) = 0.0
end do

if (ncorrt /= 0) then
  
  ioff = nspectot 
  do i = 1, ncorrt
    nn   = indx_corr(i)
    iA   = reaction(nn)%iA
    iB   = reaction(nn)%iB
    cA   = ps(iA)%c
    cB   = ps(iB)%c
    corr(i) = pm%mc(i+ioff) - (0.5*(ps(iA)%m*cB + ps(iB)%m*cA))
  end do

!===    Limit A'B' to -A*B

  do i = 1, ncorrt
    nn   = indx_corr(i)
    iA   = reaction(nn)%iA
    iB   = reaction(nn)%iB
    cA   = ps(iA)%c + ps(iA)%a
    cB   = ps(iB)%c + ps(iB)%a
    if (corr(i) < -vol*cA*cB) then
      corr (i) = -vol*cA*cB
    end if
  end do

end if

!===   Wash out for gases (if not done in aqueous-phase chemistry)
if (.not. laqueous) then
  if (lwash) then
    do i = 1, nspectot
      call set_wash_gas_chem(i,taur_gas)
      if (species(i)%class /= ID_SPECIES_PARTICLE .and. &
          species(i)%class /= ID_SPECIES_NUMBER .and. &
          species(i)%class /= ID_SPECIES_SURFACE) then
        ps(i)%tauwet = taur_gas
      end if
    end do
  end if
end if

ioff = 2*nspectot + ncorrm + ncorrt + 1
if (pm%mc(ioff) /= 0.) then
  vol  = 1./pm%mc(ioff)
else
  vol  = 0.
end if

if (ncorrt > 0) then
  ioff = nspectot
  do i = 1, ncorrt
    nn   = indx_corr(i)
    iA   = reaction(nn)%iA
    iB   = reaction(nn)%iB
    cA   = ps(iA)%c + ps(iA)%a
    cB   = ps(iB)%c + ps(iB)%a
    conc_lim(ioff+i) = -cA*cB*vol
  end do
end if

!===  Set chemical stage
if (lstage) then
  ioff = 2*nspectot + ncorrm + ncorrt + 2
  istage = nint(pm%mc(ioff) + 0.45)
  if ( .not. (istage == 1 .or. istage == 2 &
       .or. istage == 3) ) then
    nError = IV_ERROR
    eRoutine = 'set_ps_from_mc'
    eMessage = 'Istage is not 1, 2, or  3'
    eAction = ' Alert model developer'
    write(eInform,'(a,i4)') 'Istage is',istage
    go to 9999
  end if
end if

9999    return
end

subroutine set_mc_from_ps(pm)
!*******************************************************************************
!
! FUNCTION:  Reset puff storage from  working arrays
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

use common_mc_puf
use multcomp_inc
use common_mc_met

implicit none

! --- ARGUMENTS

type ( puff_mc ) pm   ! -- puff multicomponent structure

! --- LOCALS

integer i, nn, ioff, iA, iB
real cA, cB
logical  leqm_set
 
leqm_set = .true.

do i = 1,nspectot
  pm%mc(i) = ps(i)%m
end do

!-------save end of chemistry concentrations

ioff = nspectot + ncorrm + ncorrt
do i = 1,nspectot
  pm%mc(ioff+i) = ps(i)%c + ps(i)%a
end do

!-------save chemical stage

ioff = 2*nspectot + ncorrm + ncorrt + 2  
if (lstage) then
  pm%mc(ioff) = istage
else if (ldump_chem .or. lchem_split) then
  istage = nint(pm%mc(ioff) + 0.45)
  if ( .not. (istage == 1 .or. istage == 2 &
   .or. istage == 3) ) then
    nError = IV_ERROR
    eRoutine = 'chk_chem_split'
    eMessage = 'Istage is not 1, 2, or  3'
    eAction = ' Alert model developer'
    write(eInform,'(a,i4)') 'Istage is',istage
    go to 9999
  end if
  call set_chem_stage(leqm_set)
  pm%mc(ioff) = istage
  istage = nstage
else
  pm%mc(ioff) = istage
end if

!-------save temperature and pressure

ioff = 2*nspectot + ncorrm + ncorrt + 3
pm%mc(ioff) = tab

ioff = ioff + 1
pm%mc(ioff) = pb

if (ncorrt > 0) then
  ioff = nspectot 
  do i = 1, ncorrt
    nn  = indx_corr(i)
    iA  = reaction(nn)%iA
    iB  = reaction(nn)%iB
    cA   = max(ps(iA)%c, -ps(iA)%a)
    cB   = max(ps(iB)%c, -ps(iB)%a)
    pm%mc(ioff+i) = corr(i) + 0.5*(ps(iA)%m*cB + ps(iB)%m*cA)
  end do
end if

9999 return
end

subroutine scale_static_mc(pm,rat,flag)
!*******************************************************************************
!
! FUNCTION:  Rescale multicomponents for continuous release static puff
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

type ( puff_mc  ) pm  ! -- puff multicomponent structure
real rat              ! -- scale factor
logical flag          ! -- flag to set volume

! --- LOCALS

integer i, j

!---masses
do i = 1,nspectot
  pm%mc(i) = rat*pm%mc(i)
end do

!---correlations
if (ncorrt > 0) then
  i = nspectot !+ ncorrm
  do j=1,ncorrt
    i = i + 1
    pm%mc(i) = pm%mc(i)*rat
  end do
end if

!---volume
if (flag) then
  j = 2*nspectot + ncorrm + ncorrt + 1
  pm%mc(j) = rat/pm%mc(j)
end if

return
end

subroutine inter_static_mc(pm,pm0,fac)
!*******************************************************************************
!
! FUNCTION:  Scale multicomponents for static interactions
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

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

type ( puff_mc  ) pm, pm0   ! -- puff multicomponent structures
real fac                    ! -- scale factor

! --- LOCALS

integer i, j

!---volume
j = 2*nspectot + ncorrm + ncorrt + 1
pm%mc(j) = fac*pm0%mc(j)

!----species concentrations

if (ncorrm > 0) then
  i = nspectot + ncorrt
  do j = 1,ncorrm
    i = i + 1
    pm%mc(i) = fac*pm0%mc(i)
  end do
end if

return
end

subroutine setall_amb_keqm(cgrid)
!******************************************************************************
!
! FUNCTION:  Set concentrations and update k for ambient equilibrium species
!            for all ambient points.
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!             set_amb_keqm         get_topog         set_amb3d
!
!
! REVISION HISTORY:
! August 2010: Include updates made by PKK, AER  for Oct. 2004 CMAQ release -BC(SAGE-MGT)
!******************************************************************************

! --- MODULES

use common_puf
use common_met
use multcomp_inc
use amb_data
use interface_definitions, only: set_amb_keqm

implicit none

! --- ARGUMENTS

REAL :: CGRID( :,:,:,: )  ! -- 3D ambient concentrations

! --- LOCALS

real xamb , yamb , zamb
real hp, dp, dum1, dum2
integer i, j, k

! Following commented out for PiG version:

!xamb  = x0a
!yamb  = y0a
!zamb  = z0a           

!if (nxyb == 0) then
!  call set_amb_keqm(xamb,yamb,zamb,cgrid)
!else 
!  if (lstep_amb3d) then
!    do i = 1,nxa
!      xamb = x0a + dxa*(i-1)
!      do j = 1,nya 
!        yamb = y0a + dya*(j-1)
!        if (lter) then
!          call get_topog(xamb,yamb,hp,dum1,dum2)
!          dp = 1. - hp/zbtop
!        else 
!          hp = 0.0
!          dp = 1.0
!        end if          
!        do k = 1,nza
!          if (lamb_metgrid) then
!            zamb = zbw(k)*dp + hp
!          else 
!            zamb = z0a + dza*(k-1) + hp
!          end if
!          call set_amb_keqm(xamb,yamb,zamb,cgrid)
!          if (nError /= NO_ERROR) go to 9999
!          call set_amb3d(i,j,k)
!          if (nError /= NO_ERROR) go to 9999          
!        end do
!      end do
!    end do
!  else
!    xamb = 0.5*(xmin+xmax)
!    yamb = 0.5*(ymin+ymax)
!    if (lter) then
!      call get_topog(xamb,yamb,hp,dum1,dum2)
!      zamb = zref + hp
!    else 
!      zamb = zref
!    end if
!    call set_amb_keqm(xamb,yamb,zamb,cgrid)
!    if (nError /= NO_ERROR) go to 9999
!  end if
!endif

9999    return
end

subroutine set_amb_keqm(xamb,yamb,zamb,cgrid)
!******************************************************************************
!
! FUNCTION:  Set concentrations and update k for ambient equilibrium species
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                  mapfac                 get_met                 get_amb
!                update_k         set_equilibrium       step_aerosol_chem
!
!
! REVISION HISTORY:
! Jan 2005 : Updated for new call to step_aerosol_chem - PK, AER
! Aug 2010  : Include following update made by PKK, AER
!             a) Updated for Oct. 2004 CMAQ release
!             - BC(SAGE-MGT)
!******************************************************************************

! --- MODULES

use common_mc_puf
use common_mc_met
use multcomp_inc
use files_inc
use interface_definitions, only: get_amb

implicit none

! --- ARGUMENTS

REAL :: CGRID( :,:,:,: )  ! -- 3D ambient concentration
real xamb , yamb , zamb

! --- LOCALS

real xmap , ymap
real conc(MAX_MC)
integer i, j
real dt

tk   = NOT_SET_R
pk   = NOT_SET_R
zk   = NOT_SET_R
hk   = NOT_SET_R
cldk = NOT_SET_R

if (nxyb == 0) then

  tb     = 298.0  !default values
  pb     = 1.0    !met not initialized on a create
  hb     = 0.0
  cldall = 0.0
 
else

  call mapfac( xamb , yamb , xmap , ymap )
  call get_met( xamb , yamb , zamb , 0.0 , 0.0, xmap, ymap, 1, .false.)

endif

if (lamb3d) call get_amb(xamb, yamb, zamb, t, cgrid)

istage = nstage

call update_k( .true., xamb, yamb, zamb )

if (nError /= NO_ERROR) go to 9999
call set_equilibrium(.true.)
if (nError /= NO_ERROR) then
  write(lun_log,*) 'Error setting equilibrium in set_amb_keqm'
  go to 9999
end if

if (laerosol) then

  dt = 1.0E-20  ! negligible time step (seconds)
  !load aerosol working array
  do i = 1, naero
    j = index_aero(i)
    conc(i) = ps(j)%a
  end do

  call step_aerosol_chem(dt,conc,naero,aero_names,ic_units,hb,tab,pb)
  if (nError /= NO_ERROR) go to 9999

  !unload aerosol working array
  do i = 1, naero
    j = index_aero(i)
    ps(j)%a = conc(i)
  end do

end if

9999    return
end

subroutine set_equilibrium(lAmbient)
!*******************************************************************************
!
! FUNCTION:  Equilibrium concentration solver
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   mnewt           check_equilib                  ludcmp
!                  lubksb                       
!
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

logical lAmbient  ! -- whether or not it is setting the ambient (versus plume)

! --- LOCALS

real ctem(MAX_MC), aa, bb
integer i, j, k, iA, iB, iP, iwrk(MAX_EQ)
integer iiA, iiB, ii, id, is, jd, js, i1, i2
real Cq(MAX_EQ, MAX_EQ, MAX_EQ), Cl(MAX_EQ, MAX_EQ), Cc(MAX_EQ)
real fjac(MAX_EQ, MAX_EQ), fvec(MAX_EQ), rwk(MAX_EQ), vwrk(MAX_EQ)
real x(MAX_EQ), tolx(MAX_EQ)
real kr

external f_conc
logical lA, lB, nonlinear, lturb

real arg, fP

!====   Check for equilibrium species

if(nequilibrium <= 0)return

neq_s = neqm_s(istage)
if (lstage .and. istage == 1) return

nsolve_eq = nsolve_eq_s(istage)
ndir_eq  = ndir_eq_s(istage)
nsubs_eq = nsubs_eq_s(istage)
nlin_eq  = nlin_eq_s(istage)
do i = 1, neq_s
  indx_eq(i) = indx_eq_s(i,istage)
  irow_eq(indx_eq(i)) = irow_eq_s(indx_eq(i),istage)
end do
do i = 1, nequilibrium
  itype_eq(i) = itype_eq_s(i,istage)
end do

!====   Set Ambient/Pertubation concentration switch
! (uses total concentration)

if(lAmbient)then
  do i = 1, nspecies
    ctem(i) = max(ps(i)%a,species(i)%tol)
  end do
else
  do i = 1, nspecies
    ctem(i) = max(ps(i)%c+ps(i)%a,species(i)%tol)
  end do
end if

! ===  Set first guess to tolerance
do i = nspecies+1, nspectot
  ctem(i) = species(i)%tol
end do
if (nambient > 0) then
  do i = 1, nambient
    ctem(i+nspectot) = ps(i+nspectot)%a
  end do
end if

!====   Clear solver arrays

do i = 1,nequilibrium
  Cc(i) = 0.0
  do j = 1,nequilibrium
    Cl(i,j) = 0.0
    do k = 1,nequilibrium
      Cq(i,j,k) = 0.0
    end do
  end do
  ii = irow_eq(i)
  Cl(ii,ii) = 0.
end do

!====   Loop over reactions and set solver coefficients

nonlinear = .false.

do i = 1, nreact_s(istage)

!====== Reaction has equilibrium reactants/products

  lturb = .false.

  if( btest(reaction(i)%class,ID_REACT_EQUILIBRIUM) )then

    if (lAmbient) then
      kr = kamb(i)
    else
      kr = reaction(i)%k
    end if

    iA  = reaction(i)%iA
    lA = ps(iA)%equil

    if( lA )iA = iA - nspecies

!======== Linear Reactions

    if( IsLinear(i) )then

!==========     Equilibrium reactant


      if( lA ) then

        iiA = irow_eq(iA)
        arg = kr
        Cl(iiA,iiA) = Cl(iiA,iiA) - arg
        do j = 1, reaction(i)%nP
          iP = reaction(i)%iP(j)
          if( ps(iP)%equil ) then
            iP = irow_eq(iP - nspecies)
            fP = reaction(i)%fP(j)
            Cl(iP,iiA) = Cl(iP,iiA) + fP*arg
          end if
        end do
!==========     Non Equilibrium reactant

      else

        do j = 1, reaction(i)%nP
          iP = reaction(i)%iP(j)
          if( ps(iP)%equil ) then
            iP = irow_eq(iP - nspecies)
            fP = reaction(i)%fP(j)
            Cc(iP) = Cc(iP) - fP*kr*ctem(iA)
          end if
        end do
      end if

!======== Quadratic reaction

    else

      iB  = reaction(i)%iB
      lB  = ps(iB)%equil

!  of A'B' on the equilibrium concentrations
! if (reaction(i)%icorr /= 0 .and. .not. lAmbient) then
!   lturb = .true.
!   kAB = kr*corr(reaction(i)%icorr)/vol
! end if

      if( lB )iB = iB - nspecies

!==========     Equilibrium A : Non equilibrium B

      if( lA .and. .not.lB )then

        iiA = irow_eq(iA)
        arg = kr*ctem(iB)
        Cl(iiA,iiA) = Cl(iiA,iiA) - arg
!   if (lturb) Cc(iiA) = Cc(iiA) + kAB
        do j = 1, reaction(i)%nP
          iP = reaction(i)%iP(j)
          if( ps(iP)%equil ) then
            iP = irow_eq(iP - nspecies)
            fP = reaction(i)%fP(j)
            Cl(iP,iiA) = Cl(iP,iiA) + fP*arg
! if (lturb) Cc(iP) = Cc(iP) - fP*kAB
          end if
        end do


!==========     Equilibrium B : Non equilibrium A

      else if( lB .and. .not.lA)then

        iiB = irow_eq(iB)
        arg = kr*ctem(iA)
        Cl(iiB,iiB) = Cl(iiB,iiB) - reaction(i)%fB*arg
!   if (lturb) Cc(iiB) = Cc(iiB) + reaction(i)%fB*kAB
        do j = 1, reaction(i)%nP
          iP = reaction(i)%iP(j)
          if( ps(iP)%equil ) then
            iP = irow_eq(iP - nspecies)
            fP = reaction(i)%fP(j)
            Cl(iP,iiB) = Cl(iP,iiB) + fP*arg
! if (lturb) Cc(iP) = Cc(iP) - fP*kAB
          end if
        end do

!==========     Equilibrium A : Equilibrium B -> Nonlinear terms

      else if( lB .and. lA)then

        iiA = irow_eq(iA)
        iiB = irow_eq(iB)
        if (iiA < iiB) then
          i1 = iiB
          i2 = iiA
        else
          i1 = iiA
          i2 = iiB
        end if
        Cq(iiA,i1,i2) = Cq(iiA,i1,i2) - kr
        Cq(iiB,i1,i2) = Cq(iiB,i1,i2) - reaction(i)%fB*kr
!   if (lturb) then
!     Cc(iiA) = Cc(iiA) + kAB
!     Cc(iiB) = Cc(iiB) + reaction(i)%fB*kAB
!   end if
        do j = 1, reaction(i)%nP
          iP = reaction(i)%iP(j)
          fP = reaction(i)%fP(j)
          if( ps(iP)%equil ) then
            iP = irow_eq(iP - nspecies)
            Cq(iP,i1,i2) = Cq(iP,i1,i2) + fP*kr
! if (lturb) Cc(iP) = Cc(iP) - fP*kAB
          end if
        end do
        nonlinear = .true.

!==========     Non equilibrium A : Non equilibrium B

      else

        do j = 1, reaction(i)%nP
          iP = reaction(i)%iP(j)
          if( ps(iP)%equil ) then
            iP = irow_eq(iP - nspecies)
            fP = reaction(i)%fP(j)
            Cc(iP) = Cc(iP) - fP*kr*ctem(iA)*ctem(iB)
! if (lturb) Cc(iP) = Cc(iP) - fP*kAB
          end if
        end do

      end if
    end if
  end if

end do

!====   Preprocess matrices

if (ndir_eq > 0) then
  do i = 1, ndir_eq
    id = neq_s - i + 1
    if (Cl(id,id) == 0.) then
      nError = IV_ERROR
      eRoutine = 'set_equilibrium'
      eMessage = 'Zero destruction rate'
      eInform = 'Unable to solve for '//TRIM(equilibrium(indx_eq(id))%name)
      if (lAmbient) then
        eAction = 'Solving for ambient radicals'
      else
        eAction = 'Solving for plume radicals'
      end if
      go to 9999
    end if
    x(id) = Cc(id)/Cl(id,id)
    do j = 1, id - 1
      Cc(j) = Cc(j)  - x(id)*Cl(j,id)
    end do
  end do
end if

if (nlin_eq > 0) then
  do i = 1, nlin_eq
    id = nsolve_eq + nlin_eq - i + 1
    js = itype_eq(indx_eq(id))
    jd = irow_eq(js)
    if (Cl(id,id) == 0.) then
      nError = IV_ERROR
      eRoutine = 'set_equilibrium'
      eMessage = 'Zero destruction rate'
      eInform = 'Unable to solve for '//TRIM(equilibrium(indx_eq(id))%name)
      if (lAmbient) then
        eAction = 'Solving for ambient radicals'
      else
        eAction = 'Solving for plume radicals'
      end if
      go to 9999
    end if
    Cl(id,jd) = Cl(id,jd)/Cl(id,id)
    Cc(id)    = Cc(id)/Cl(id,id)
    aa = -Cl(id,jd)
    bb = Cc(id)
    do j = 1, id - 1
      if (Cl(j,id) /= 0. ) then
        Cl(j,jd) = Cl(j,jd) + aa*Cl(j,id)
        Cc(j)    = Cc(j)  - bb*Cl(j,id)
      end if
    end do
  end do
end if


!====   Solve - nonlinear

if (nonlinear) then

!====   Use previous values as initial guesses

  do is = 1, nsolve_eq
    i = indx_eq(is)
    x(is) = ctem(i+nspecies)

!====   Find tolx for equilibrium species

    tolx(is) = equilibrium(i)%tol
  end do

!====   Call Newton-Raphson solver

  call mnewt(Cq, Cl, Cc, x, nsolve_eq, MAX_EQ, iwrk, f_conc, &
               tolx, fjac, fvec, rwk, vwrk)

  if(nError /= NO_ERROR) then
    call check_equilib(Cq, Cl, x, lAmbient)
    go to 9999
  end if

  do i = 1, nsolve_eq
    Cc(i) = x(i)
  end do

!====   Solve - linear (LU Decomposition and Backsubstitution)

else

  call ludcmp(Cl, nsolve_eq, MAX_EQ, iwrk, arg, vwrk)
  if(nError /= NO_ERROR) then
    call check_equilib(Cq, Cl, x, lAmbient)
    go to 9999
  end if
  call lubksb(Cl, nsolve_eq, MAX_EQ, iwrk, Cc )
end if

!====   Complete solution

do i = 1, nsolve_eq
  x(i) = Cc(i)
end do
if (nlin_eq > 0) then
  do i = 1, nlin_eq
    id = i+nsolve_eq
    js = itype_eq(indx_eq(id))
    jd = irow_eq(js)
    x(id) = -Cl(id,jd)*x(jd) + Cc(id)
  end do
end if
if (nsubs_eq > 0) then
  do i = 1, nsubs_eq
    id = i + nsolve_eq + nlin_eq
    x(id) = -Cc(id)
    do j = 1, nsolve_eq + nlin_eq
      x(id) = x(id) + Cl(id,j)*x(j)
    end do
    if (Cl(id,id) == 0.) then
      nError = IV_ERROR
      eRoutine = 'set_equilibrium'
      eMessage = 'Zero destruction rate'
      eInform  = 'Unable to solve for '//TRIM(equilibrium(indx_eq(id))%name)
      if (lAmbient) then
        eAction = 'Solving for ambient radicals'
      else
        eAction = 'Solving for plume radicals'
      end if
      go to 9999
    end if
    x(id) = -x(id)/Cl(id,id)
  end do
end if

!====   Put solution into working arrays

!====   Ambient equilibrium concentration only

if(lAmbient) then
  do i = 1,neq_s
    ii = indx_eq_s(i,istage)
    id = irow_eq(ii)
    ps(ii+nspecies)%a = max(0., x(id))
  end do

!====   Equilibrium species concentration and mass

else
  do i = 1,neq_s
    ii = indx_eq_s(i,istage)
    id = irow_eq(ii)
    j = ii+nspecies
    ps(j)%c = max( 0., x(id) ) - ps(j)%a
    ps(j)%m = ps(j)%c*vol
  end do
end if

9999    return
end

subroutine check_equilib(Cq, Cl, x, lAmbient)
!*******************************************************************************
!
! FUNCTION:  Checks equilibrium species timescales if mnewt does not converge
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

use multcomp_inc
use files_inc
use error_inc

implicit none

! --- ARGUMENTS

real Cq(MAX_EQ, MAX_EQ, MAX_EQ)  ! -- quadratic terms
real Cl(MAX_EQ, MAX_EQ)          ! -- linear terms
real x(*)                        ! -- values in solution
logical lAmbient                 ! -- whether it is solving for the ambient

! --- LOCALS

integer i, j, ii, jj, idi, idj
real tchem(MAX_EQ)
character*16 equilib

!====   Set Ambient/Pertubation concentration switch

if(lAmbient)then
  equilib = 'the ambient'
else
  equilib = 'the plume'
end if

do i = 1, nequilibrium
  tchem(i) = 0.
end do

do i = 1, nsolve_eq
    idi = indx_eq(i)
    ii  = irow_eq(idi)
end do

do ii = 1, nsolve_eq
  idi = indx_eq(ii)
  i = irow_eq(idi)
  do jj = 1, nsolve_eq
    idj = indx_eq(jj)
    j = irow_eq(idj)
    if (i == j) then
      tchem(idi) = tchem(idi) + Cq(i,i,j)*x(j)
    else
      tchem(idi) = tchem(idi) + (Cq(i,i,j) + Cq(i,j,i))*x(j)
    end if
  end do
  tchem(idi) = tchem(idi) + Cl(i, i)
end do

write(lun_log, *) &
     'The following are reaction timescales for equilibrium species:'
  do ii = 1, nsolve_eq
    i = indx_eq(ii)
    if (tchem(i) /= 0.) then
      write(lun_log, *) equilibrium(i)%name, abs(1./tchem(i))
    else
      write(lun_log, *) equilibrium(i)%name, 'Infinity'
    end if
  end do
  if (nzenith > 0) write(lun_log,*) 'Zenith angle = ',zk
  write(lun_log,*) 'Temperature  = ',tk
  write(lun_log,*) 'Pressure     = ',pk
  if (H2O > 0)     write(lun_log,*) 'H2O (ppm)    = ',ps(H2O)%a
  nError = IV_ERROR
  eMessage = 'Nonconverging equilibrium species for ' // equilib
  eAction  = 'See log file for more information'

return
end

subroutine lubksb(a, n, np, indx, b)
!*******************************************************************************
!
! FUNCTION:   Linear solver for equilibriums - generate the solution vector (X)
!             A*X = B
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

implicit none

! --- ARGUMENTS

integer n          ! - number of equations
integer np         ! - dimension
integer indx(n)    ! - permutation vector returned by ludcmp
real    a(np, np)  ! - LU decomposed matirx
real    b(n)       ! - right hand side on input, solution (X) on output

! --- LOCALS

integer i, ii, j, ll
real sum

!====   Forward substitution

ii = 0
do i = 1, n
  ll = indx(i)
  sum = b(ll)
  b(ll) = b(i)
  if (ii /= 0) then
    do j = ii, i-1
      sum = sum - a(i,j)*b(j)
    end do
  else if (sum /= 0.) then
    ii = i
  end if
  b(i) = sum
end do

!====   backward substitution

do i = n, 1, -1
  sum = b(i)
  do j = i+1, n
    sum = sum - a(i,j)*b(j)
  end do
  b(i) = sum/a(i,i)
end do

return
end

subroutine ludcmp(a, n, np, indx, d, vv)
!*******************************************************************************
!
! FUNCTION: Linear solver for equilibriums - decomposition
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

integer n         ! - number of equations
integer np        ! - dimension
integer indx(n)   ! - records row permuation affected by partial pivoting
real    d         ! - number of row interchanges (even = 1, odd = -1)
real    a(np, np) ! - matrix to be decomposed on input, decomposed matrix on output
real    vv(np)    ! - stores implicit scaling of each row

! --- PARAMETERS

real, parameter :: TINY = 1.0e-20

! --- LOCALS

real aamax, dum, sum
integer i, imax, j, k

!====   Get implicit scaling and check for singular matrix

d = 1.
do i = 1, n
  aamax=0.
  do j = 1, n
    aamax = max(aamax,abs(a(i,j)))
  end do
  if( aamax == 0. ) then
    nError = IV_ERROR
    eRoutine = 'ludcmp'
    eMessage = 'Singular matrix in ludcmp'
    go to 9999
  end if
  vv(i) = 1./aamax
end do

!====   Loop over columns of Crout's method

do j = 1,n

!====== Step 1

  do i = 1, j-1
    sum = a(i,j)
    do k=1, i-1
      sum = sum - a(i,k)*a(k,j)
    end do
    a(i,j) = sum
  end do

!====== Step 2 + find largets pivotal element

  aamax = 0.
  do i = j, n
    sum = a(i, j)
    do k = 1, j-1
      sum = sum - a(i,k)*a(k,j)
    end do
    a(i,j) = sum
    dum = vv(i)*abs(sum)
    if (dum >= aamax) then
      imax = i
      aamax = dum
    end if
  end do

!====== Step 3 - interchange rows if necessary

  if (j /= imax) then
    do k = 1, n
      dum = a(imax, k)
      a(imax,k)=a(j,k)
      a(j,k) = dum
    end do
    d = -d
    vv(imax) = vv(j)
  end if

!====== Step 4 - divide by pivotal element

  indx(j) = imax
  if(a(j,j) == 0.) a(j,j) = TINY
  if(j /= n) then
    dum = 1./a(j, j)
    do i = j+1, n
      a(i,j) = a(i,j)*dum
    end do
  end if
end do

9999    return
end

subroutine mnewt(q, l, c, x, n, np, indx, usrfunc, tolx, &
                                       fjac, fvec, p, vwrk)
!*******************************************************************************
!
! FUNCTION:  Nonlinear solver for equilibriums (Newton-Raphson Method)
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                 usrfunc                  ludcmp                  lubksb
!
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use error_inc
implicit none

! --- ARGUMENTS

integer n                             ! - number of equations
integer np                            ! - dimension
real q(np, np, np), l(np, np), c(np)  ! - quadratic, linear and constant terms of eqs
integer indx(np)                      ! - work array used in lubcksb and ludcmp
real tolx(n)                          ! - tolerances
real x(n)                             ! - initial guess and then final solution
real fjac(np, np)                     ! - jacobian
real fvec(np)                         ! - right hand side of eqns
real p(np)                            ! - work array
real vwrk(np)                         ! - work array
external usrfunc                      ! - calculated right hand side and jacobian

! --- PARAMETERS

real, parameter :: RTOL = .0001
integer, parameter :: NTRIAL = 20

! --- LOCALS

integer i, k
real d
logical converged

do k = 1, NTRIAL
  call usrfunc(q, l, c, x, n, np, fvec, fjac)
  do i = 1, n
    p(i) = -fvec(i)
  end do
  call ludcmp(fjac, n, np, indx, d, vwrk)
  if(nError /= NO_ERROR)go to 9999
  call lubksb(fjac, n, np, indx, p)
  converged = .true.
  do i = 1, n
    x(i) = x(i) + p(i)
    if ( abs(p(i)) > RTOL*abs(x(i))+tolx(i) ) then
      converged = .false.
    end if
  end do
  if (converged) go to 9999
end do

!-------Did not converge in ntrial iterations
nError = IV_ERROR
eRoutine = 'mnewt'
9999    return
end

subroutine f_conc(q, l, c, x, n, np, fvec, fjac)
!*******************************************************************************
!
! FUNCTION:  Calculates fvec and fjac for mnewt
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
integer  n                      ! -- number of equations
integer  np                     ! -- dimension
real  q(np, np, np), l(np, np)  ! -- quadratic and linear terms
real  c(np)                     ! -- constant terms
real  x(np)                     ! -- concentrations
real  fvec(np)                  ! -- right hand side of equations
real  fjac(np, np)              ! -- jacobian

! --- LOCALS

integer  i, j, k


!-------initial fvec and fjac to zero
do i = 1, n
  fvec(i) = 0.
  do j = 1, n
    fjac(i, j) = 0.
  end do
end do

!-------calculate fvec and fjac
do i = 1, n
  do j = 1, n
    do k = j, n
      if (q(i,j,k) /= 0.) then
        fvec(i) = fvec(i) + q(i,j,k)*x(j)*x(k)
        if (j == k) then
          fjac(i, j) = fjac(i, j) + 2.*q(i,j,k)*x(k)
        else
          fjac(i, j) = fjac(i, j) + q(i,j,k)*x(k)
        end if
      end if
    end do
    fvec(i) =  fvec(i) + l(i,j)*x(j)
    fjac(i, j) =  fjac(i, j) + l(i,j)
  end do
  fvec(i) = fvec(i) - c(i)
end do

return
end

subroutine get_lsode_error(i,cmsg)
!*******************************************************************************
!
! FUNCTION:  Interpret LSODE error (istate)
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

integer i          ! -- istate from lsode
character*(*) cmsg ! -- what istate means

if (i == -1) then
  cmsg = 'Excess work done on this call (perhaps wrong mf).'
else if (i == -2) then
  cmsg = 'Excess accuracy requested (tolerances too small).'
else if (i == -3) then
  cmsg = 'Illegal input detected (see printed message).'
else if (i == -4) then
  cmsg = 'Repeated error test failures (check all inputs).'
else if (i == -5) then
  cmsg = 'Repeated convergence failures (perhaps bad jacobian '
  cmsg = cmsg//'supplied or wrong choice of mf or tolerances).'
else if (i == -6) then
  cmsg = 'Error weight became zero during problem (solution'
  cmsg = cmsg//' component i vanished, and atol or atol(i) = 0.)'
else
  cmsg = ' '
end if

return
end

subroutine set_vdry_aerosol_chem(srtmp,isec,vdry,vfall)
!*******************************************************************************
!
! FUNCTION:  Set the dry deposition rates for aerosol particles
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                vdep_dry                   ufall
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use common_mc_met
use multcomp_inc

implicit none

! --- ARGUMENTS

integer isec    ! -- particle size section number
real srtmp      ! -- puff area close to ground
real vdry       ! -- dry deposition velocity
real vfall      ! -- settling velocity

! --- LOCALS

real rhoa, ufall

rhoa  = rhoair*(pb**0.715)*273.2/tb
vfall  = ufall(rhoa,rho_aer,rmuair,dm_aer(isec))
if (srtmp > 0.) then
  call vdep_dry(ustdep,h_cnp,zruf,vfall, &
                dm_aer(isec),diff_aer(isec),vdry)
else
  vdry = 0.
end if

return
end

subroutine set_wash_aerosol_chem(isec,taur_aer)
!*******************************************************************************
!
! FUNCTION:  Set the washout timescale for aerosol particles
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_met
use multcomp_inc

implicit none

! --- ARGUMENTS

integer isec    ! -- particle size section number
real taur_aer   ! -- washout timescale

! --- LOCALS

integer ityppr

!-----  Look up Raindrop Fall Velocity and Scavenging Coefficient
!-----  Interpolate Precipitation Rate from Specified Index
!-----  I = 0  --> PR =   0.0 mm/hr     !RAIN
!-----  I = 1  --> PR =   0.5 mm/hr
!-----  I = 2  --> PR =   3.5 mm/hr
!-----  I = 3  --> PR =  25.0 mm/hr
!-----  I = 4  --> PR =   5.0 mm/hr     !SNOW
!-----  I = 5  --> PR =  20.0 mm/hr
!-----  I = 6  --> PR = 100.0 mm/hr

ityppr = nint(prbl)  !always initialized
taur_aer = twash_aer(ityppr,isec)

return
end

subroutine set_wash_gas_chem(ispec,taur_gas)
!*******************************************************************************
!
! FUNCTION:  Set the washout timescale for a gas
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_met
use multcomp_inc

implicit none

! --- ARGUMENTS

integer ispec    ! -- species number
real taur_gas    ! -- washout timescale

taur_gas = pratebl*sparam(ispec)%scav

return
end

subroutine set_emission_conv
!*******************************************************************************
!
! FUNCTION:  Set conversion factor for the multicomponent emission rate
!            Emissions released at STP
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
! 30 NOV 2004 : Corrected check for MOLECULE_PPM to use ie_conv not em_conv - BC
!
!*******************************************************************************

! --- MODULES

use multcomp_inc

implicit none


if(iabs(ie_conv) == MOLECULE_PPM)then !molecules/cm3 <-> PPM
   !Av(molecules/mole)*P0(N/m2)/R0(J/mole/K)/10**6
  em_conv = 7.35e+15*(1./298.)   !Av=6.022e23  P0=1.103e5  R0=8.314e6 at STP

else if(ie_conv == G_PPM) then
   !g/m3 -> PPM
  em_conv = 82.06*298.     !RT/P * 10+6 (multiply by 1/MW later) at STP

else if(ie_conv == G_MOLECULE) then
   !g/m3 -> molecules/cm3
  em_conv = 6.022e17 !Av   * 10+6 (multiply by 1/MW later)

else

  em_conv = 1.

end if

if(ie_conv < 0)then
  em_conv = 1./em_conv
end if

return
end

subroutine sum_diagnostics(diag,p)
!*******************************************************************************
!
! FUNCTION:  Sum a multicomponent species diagnostic
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                  get_mc
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

! --- ARGUMENTS

type ( puff_str ) p      ! -- puff structure
double precision diag(*) ! -- diagnostics array

! --- LOCALS
type ( puff_mc ) pm
integer isp

diag(nspectot+1) = p%c + diag(nspectot+1)

call get_mc(p,pm)
do isp = 1, nspectot
  diag(isp) = pm%mc(isp) + diag(isp)
end do

return
end

subroutine sum_src_diagnostics(diag,p,dt)
!*******************************************************************************
!
! FUNCTION: Sum the multicomponent species source diagnostic
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                  get_mc
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_mc_puf
use multcomp_inc

! --- ARGUMENTS

double precision diag(*)  ! -- diagnostics array
type ( puff_str ) p       ! -- puff structure
real dt                   ! -- time step

! --- LOCALS

type ( puff_mc ) pm
integer isp

diag(nspectot+1) = p%c*dt + diag(nspectot+1)

call get_mc(p,pm)
do isp = 1, nspectot
  diag(isp) = pm%mc(isp)*dt + diag(isp)
end do

return
end

subroutine get_fmax(ipuf,jpuf,fmax)
!*******************************************************************************
!
! FUNCTION:  Find factor for contribution to species max concentration at 
!            "i" puff  centroid from "j" puff 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics    get_mc          mapfac
!            get_topog       get_asig        grnd_reflect     zi_reflect
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer ipuf,jpuf                  !puff number
real  fmax

! --- PARAMETERS
 
real, parameter :: ARGMAX = 20.0

! --- LOCALS

real xr(3), xnrm(3), asig(7)
real xmap, ymap, xbar, ybar, vbar
real deth, rat, hz, hx, hy, zp, znrm, zfac, xs, ys, vs
real xp, yp, faci, arg, zr, facs
logical lset

!------ calculate cdep at grid locations and increment dose

fmax = 0.0
  
call mapfac( puff(jpuf)%xbar , puff(jpuf)%ybar , xmap , ymap )

xbar  = puff(jpuf)%xbar
ybar  = puff(jpuf)%ybar
vbar  = puff(jpuf)%zbar

deth = puff(jpuf)%axx*puff(jpuf)%ayy - puff(jpuf)%axy**2
rat  = 0.5/(puff(jpuf)%det*deth)

xs = puff(ipuf)%xbar
ys = puff(ipuf)%ybar
vs = puff(ipuf)%zbar
xp = (xs-xbar)/xmap
yp = (ys-ybar)/ymap
zp = (vs-vbar)

call zi_reflect(vbar,puff(jpuf)%zc,puff(jpuf)%zc,vs,rat,faci)  
if (lter) then
  call get_topog(puff(jpuf)%xbar,puff(jpuf)%ybar,hz,hx,hy)
  call get_asig(puff(jpuf),asig)
  call grnd_reflect(vbar-hz,asig,hx,hy,xr,xnrm,deth,znrm)
  zfac = 0.5*znrm/(puff(jpuf)%det*deth)
  call get_topog(xs,ys,hz,hx,hy)
end if
if (lter) then
  lset =  hz <= vs
else
  lset = .true.
end if
if (lset) then
  arg = puff(jpuf)%axx*xp*xp+2.*puff(jpuf)%axy*xp*yp+2.*puff(jpuf)%axz*xp*zp &
         + puff(jpuf)%ayy*yp*yp+2.*puff(jpuf)%ayz*yp*zp+puff(jpuf)%azz*zp*zp
  if ( arg < ARGMAX )then
    if (lter) then
      zr   = xnrm(1)*(xp-xr(1)) + xnrm(2)*(yp-xr(2)) &
              + xnrm(3)*(zp-xr(3))
      zr   = max(zr,0.)
      facs = exp(zfac*zr)
    else
      facs = exp(-vs*vbar*rat)    
    end if
    fmax  = exp(-arg)*(1.+facs)*(1.+faci)
  end if
end if

return
end

subroutine allocate_smax(lallocate,nn,ioffset)
!*******************************************************************************
!
! FUNCTION:  Allocate and initialise array for maximum species concentration for
!            each puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
use error_inc
use multcomp_inc
use cstar_inc

implicit none

integer ios, nn, ioffset
logical lallocate

ios = 0
if (allocated(species_max)) then
  deallocate(species_max,STAT = ios)
  if (ios /= 0) then
    nError   = SZ_ERROR
    eRoutine = 'allocate_smax'
    eMessage = 'Error deallocating species_max array'
    write(eInform,*) ' '
  end if
end if


if (lallocate) then
  i0_smax = ioffset - 1
  allocate(species_max(nn,nspecies),STAT = ios)
  if (ios /= 0) then
   nError   = SZ_ERROR
   eRoutine = 'allocate_smax'
   eMessage = 'Error allocating species_max array'
   write(eInform,*) 'Number of bytes = ',nn*nspecies*4
  end if
  species_max(:,:) = 0.0
end if

return
end

subroutine reset_cstar(p,ipuf,cgrid)
!******************************************************************************
!
! FUNCTION:  Reset species overlap concentration of each species based on 
!            ambient species concentration.
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics           set_dense_gas            put_dynamics
!             get_totalcc             put_totalcc                  get_mc
!                clear_mc                  put_mc                   IsGas
!              IsParticle                 IsMulti
!
! REVISION HISTORY: 
!
! 21 MAR 2005 : Set species overlap to top2gauss*species_max instead of -ambient
!               to allow the reaction to continue. -BC
! Aug 2010  : Include following update made by PKK, AER
!             a) Updated for Oct. 2004 CMAQ release
!             - BC(SAGE-MGT)
!******************************************************************************

! --- MODULES
 
use common_puf
use multcomp_inc
use cstar_inc
use interface_definitions, only: get_amb

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
integer :: ipuf
REAL :: CGRID( :,:,:,: )  ! -- 3D ambient concentration

! --- LOCALS

type ( puff_mc ) pm
integer :: is, ism, ispuf

real, parameter :: top2gauss = 0.8, r0 = 0.75

real :: rat

logical  IsMulti

if(IsMulti(typeID(p%ityp)%icls))then
    call get_mc(p,pm)
    if (lamb3d) call get_amb(p%xbar, p%ybar, p%zbar, t, cgrid)
    ispuf = ipuf - i0_smax
    do is = 1,nspecies
      ism = species(is)%star
      if(ism > 0 .and. ps(is)%a /= 0.0 )then
        rat = -top2gauss*species_max(ispuf,is)
        if ( rat > r0*ps(is)%a ) then
          if (rat < ps(is)%a) then 
            rat = rat/ps(is)%a
            pm%mc(ism) = pm%mc(ism) + (r0 - rat)*(pm%mc(ism) + ps(is)%a)/(1. - r0)
          else
            pm%mc(ism) = -rat 
          end if
        end if
      end if
    end do
    call put_mc(p,pm)
end if

return

end

subroutine static_scale_smax(fac,ipuf)
!*******************************************************************************
!
! FUNCTION:  Rescale species max for static puffs.
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use multcomp_inc
use cstar_inc

implicit none

! --- ARGUMENTS
 
real    fac    !scale factor
integer ipuf   !puff number

integer j

do j = 1,nspecies
  species_max(ipuf-i0_smax,j) = fac*species_max(ipuf-i0_smax,j)
end do

return
end
