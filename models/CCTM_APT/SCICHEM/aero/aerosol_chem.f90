subroutine step_aerosol_chem(dt,conc,naero,aero_names,ic_units,hb,tk,patm)
!******************************************************************************
!
! FUNCTION:  Driver for aerosol routines
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!               sat_humid            aeroproc              getpar
!
! REVISION HISTORY:
!      Version 1.0, by Prakash Karamchandani, January 2005
!      Atmospheric and Environmental Research, Inc.
!      2682 Bishop Drive, Suite 120, San Ramon, CA 94583  
!      Portions adapted from aero_driver.F in CMAQ Oct 2004 release
!
!      Minor modifications for CMAQ-APT, March 2005, PK, AER
! REVISION HISTORY:
!   Updated Jan 2008 to include Hg treatment (PK, AER)
!   Updated Oct 2010 for consistency with CMAQ 4.7.1 (PK, ENVIRON)
!   Updated Aug 2011 for CMAQ 5 beta  (PK, ENVIRON)
!   Updated Mar 2012 for CMAQ 5 final (PK, ENVIRON)
!******************************************************************************

! --- MODULES

use files_inc
use error_inc
use aero_consts_inc
use aero_species_inc
use units_inc

use host_inc, only: INDEX1
use AEROMET_DATA, only: airtemp, airpres, airrh, MIN_GASCONC

implicit none

! --- ARGUMENTS

real dt              ! time step (seconds)
real conc(*)         ! array of species concentrations
integer naero        ! number of species participating in aerosol calculations
character*(*) aero_names(*)
integer ic_units     ! gas concentration units
real hb, tk, patm    ! humidity, temperature and pressure
REAL GAMMA_N2O5      ! N2O5 heterogeneous reaction probability [ ]

! Minimum time step (for equilibrium only calculations)
REAL, PARAMETER :: DTMIN = 1.E-20

! Minimum and Maximum RH
REAL, PARAMETER :: RHMIN = 0.005, RHMAX = 0.99

! Atmosphere to millibars
REAL, PARAMETER :: ATM_TO_MB = 1013.25

! *** aerosol properties: 

! *** variables to set up for "dry transport "
real m3_wet, m3_dry   ! third moment with and without water
real m2_wet, m2_dry   ! second moment with and without water
real m3subt           ! temp variable for dry 3rd moment calcs

integer spc, vv, imode, i   ! loop variables
integer j, ii
real hsx, pmb, factor, factor1

real vtmp

! dummy variables for consistency with CMAQ routines
integer col, row, layer

LOGICAL, SAVE :: FIRSTTIME = .TRUE.

!debug
!write(lun_log,*)'in step_aero; lun_log: ',lun_log
!write(lun_log,*)'firsttime: ',firsttime
!call flush(lun_log)
!debug
if (FIRSTTIME) then

  FIRSTTIME = .FALSE.
! Set species mapping to CMAQ aerosol module species

  aerospc_map = 0 ! note this is local to scichem not same as in AERO_DATA
  aeronum_map = 0
  aerosrf_map = 0
  precursor_map = 0
  vapor_map = 0
  orgprod_map = 0

!debug
  write(lun_log,*)'aero_names: ',(aero_names(spc),spc=1,naero)
  call flush(lun_log)
!debug
! First do particle species
  do i = 1, N_AEROSPC
outer1:    do imode = 1, N_MODE
      if (LEN_TRIM(aerospc(i)%name(imode)) == 0) CYCLE

! Find matching species
      do spc = 1, naero
        if (TRIM(aero_names(spc)) == TRIM(aerospc(i)%name(imode))) then
          aerospc_map(i,imode) = spc
          CYCLE outer1  ! go to next mode
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aerosol_chem'
      eMessage = 'No match for ' // aerospc(i)%name(imode)
      call report_error
      stop
    end do outer1
  end do

! Next do modal properties
outer2:  do imode = 1,N_MODE
    do spc = 1, naero
      if (TRIM(aero_names(spc)) == TRIM(aeromode(imode)%num_name)) then
        aeronum_map(imode) = spc
        CYCLE outer2  ! go to next mode
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aerosol_chem'
    eMessage = 'No match for ' // aeromode(imode)%num_name
    call report_error
    stop
  end do outer2

outer3:  do imode = 1,N_MODE
    do spc = 1, naero
      if (TRIM(aero_names(spc)) == TRIM(aeromode(imode)%srf_name)) then
        aerosrf_map(imode) = spc
        CYCLE outer3  ! go to next mode
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aerosol_chem'
    eMessage = 'No match for ' // aeromode(imode)%srf_name
    call report_error
    stop
  end do outer3

! Precursors
outer4:  do i = 1,N_PRECURSOR
    j = INDEX1(precursor(i)%name, n_gc_g2ae, gc_g2ae)
    if (j /= 0) then
      do spc = 1, naero
        if (TRIM(aero_names(spc)) == TRIM(gc_spc(gc_g2ae_map(j)))) then
          precursor_map(i) = spc
          CYCLE outer4  ! go to next precursor
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aerosol_chem'
      eMessage = 'No match for ' // precursor(i)%name
      call report_error
      stop
    else
      j = INDEX1(precursor(i)%name, n_nr_n2ae, nr_n2ae)
      if (j /= 0) then
        do spc = 1, naero
          if (TRIM(aero_names(spc)) == TRIM(nr_spc(nr_n2ae_map(j)))) then
            precursor_map(i) = spc
            CYCLE outer4  ! go to next precursor
          end if
        end do
        nerror = UK_ERROR   ! not an error for orgprod
        eRoutine = 'step_aerosol_chem'
        eMessage = 'No match for ' // precursor(i)%name
        call report_error
        stop
      else
        nerror = WN_ERROR
        eRoutine = 'step_aerosol_chem'
        eMessage = 'Species ' // Trim( precursor( i )%name ) &
                    // ' in vapor name is not in GC_G2AE or NR_N2AE tables'
        eInform = 'Warning only; not a required species'
        eAction = 'No action needed; run will continue'
        call WarningMessage(0,.true.)
      end if
    end if
  end do outer4

! Vapor species
outer5:  do i = 1,N_VAPOR
    j = INDEX1(vaporspc(i)%name, n_nr_n2ae, nr_n2ae)
    if (j /= 0) then
      do spc = 1, naero
        if (TRIM(aero_names(spc)) == TRIM(nr_spc(nr_n2ae_map(j)))) then
          vapor_map(i) = spc
          CYCLE outer5  ! go to next vapor
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aerosol_chem'
      eMessage = 'No match for ' // vaporspc(i)%name
      call report_error
      stop
    else
      nerror = UK_ERROR
      eRoutine = 'step_aerosol_chem'
      eMessage = 'Species ' // Trim( vaporspc( spc )%name ) &
                  // ' in vapor name is not in non-reactives table'
      call report_error
      stop
    end if
  end do outer5

! Organic products
outer6:  do i = 1,N_ORGPROD
    j = INDEX1(orgprod(i)%name, n_gc_g2ae, gc_g2ae)
    if (j /= 0) then ! j == 0 is not an error
      do spc = 1, naero
        if (TRIM(aero_names(spc)) == TRIM(gc_spc(gc_g2ae_map(j)))) then
          orgprod_map(i) = spc
          CYCLE outer6  ! go to next product
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aerosol_chem'
      eMessage = 'No match for ' // orgprod(i)%name
      call report_error
      stop
    else
      nerror = WN_ERROR
      eRoutine = 'step_aerosol_chem'
      eMessage = 'Species ' // Trim( orgprod( i )%name ) &
                    // ' in organic products is not in GC_G2AE tables'
      eInform = 'Warning only; not a required species'
      eAction = 'No action needed; run will continue'
      call WarningMessage(0,.true.)
    end if
  end do outer6

!debug  
  write(lun_log,*)'n_aerospc, n_mode: ',n_aerospc,n_mode
  do i = 1,n_aerospc
    do imode =1,n_mode
      if ( LEN_TRIM( aerospc(i)%name(imode)) == 0 ) CYCLE
      write(lun_log,*)'map for ',aerospc(i)%name(imode),' = ',aerospc_map(i,imode)
    end do
  end do

  do imode = 1,n_mode
    write(lun_log,*)'map for ',aeromode(imode)%num_name,' = ',aeronum_map(imode)
    write(lun_log,*)'map for ',aeromode(imode)%srf_name,' = ',aerosrf_map(imode)
  end do

  write(lun_log,*)'n_precursor: ',n_precursor
  do i = 1,n_precursor
    write(lun_log,*)'map for ',precursor(i)%name,' = ',precursor_map(i)
  end do 
  write(lun_log,*)'n_vapor: ',n_vapor
  do i = 1,n_vapor
    write(lun_log,*)'map for ', vaporspc( i )%name,' = ',vapor_map(i)
  end do 
  write(lun_log,*)'n_orgprod: ',n_orgprod
  do i = 1,n_orgprod
    write(lun_log,*)'map for ', orgprod( i )%name,' = ',orgprod_map(i)
  end do 
!debug

end if  !FIRSTTIME

! --- Assign met data
airtemp = tk

! --- calculate humidity mixing ratio at saturation
pmb = patm*ATM_TO_MB
call sat_humid(tk,pmb,hsx,1)
airrh = MAX(RHMIN, MIN(RHMAX, hb/hsx))

! --- pressure in pascals
airpres = patm * STDATMPA

!debug
!write(lun_log,*)'airtemp,pmb,hsx,hb,airrh,airpres: ',airtemp,pmb,hsx,hb,airrh,airpres
!debug

! --- Set conversion factors for concentrations
! --- ppm or molecules/cc to umoles/m3
if (ic_units == UNIT_PPM) then
 factor = 1.E3/(298.*RGAS)
!   factor = patm*1.E3/(tk*RGAS)
else if (ic_units == UNIT_MOLECULE) then
 factor = 1.3634E-13/(RGAS)
end if

!debug
!write(lun_log,*)'factor: ',factor
!debug
! Extract concentrations of aero species
aerospc_conc = 0.0
do imode = 1, N_MODE
  do spc = 1, N_AEROSPC
    j = aerospc_map(spc,imode)
    if ( j /= 0 ) then
      aerospc_conc(spc,imode) = MAX(conc(j), aerospc(spc)%min_conc(imode)) ! [ug/m^3]
    end if
  end do
end do

! Extract concentrations of aero # and surf area
! Convert and assign to moment0_conc and moment2_conc
moment0_conc = 0.0
moment2_conc = 0.0
do imode = 1, N_MODE
  j = aeronum_map(imode)
  moment0_conc(imode) = MAX(conc(j), aeromode_minNum(imode))
  j = aerosrf_map(imode)
  moment2_conc(imode) = MAX(conc(j), aeromode_minM2(imode) * PI)
end do

! Extract concentrations of precursor species
precursor_conc = 0.0
do spc = 1, N_PRECURSOR
  j = precursor_map(spc )
  if ( j /= 0 ) then
    vtmp = factor*precursor_mw(spc)
    precursor_conc(spc) = MAX(conc(j)*vtmp, MIN_GASCONC)
  end if
end do

! Extract concentrations of vapor species
vapor_conc = 0.0
do spc = 1, N_VAPOR
  j = vapor_map(spc)
  if ( j /= 0 ) then
    vtmp = factor*vapor_mw(spc)
    vapor_conc(spc) = MAX(conc(j)*vtmp, MIN_GASCONC)
  end if
end do

! Extract concentrations of ORGPROD species
orgprod_conc = 0.0
do spc = 1, N_ORGPROD
  j = orgprod_map(spc)
  if ( j /= 0 ) then
    orgprod_conc(spc) = MAX(conc(j), MIN_GASCONC)
  end if
end do

! *** Fetch gas-phase production rates.

dt = MAX( DTMIN, dt )

! *** sulfate
! *** sulfate (ug/m3/s)
if ( dt > DTMIN ) then
! *** Calculate SO4RATE stored in module
  so4rate = precursor_conc(sulprd_idx)/DT
else
  so4rate = 0.
end if

!debug
!write(lun_log,*)'dt,so4rate: ',dt,so4rate
!call flush(lun_log)
!debug
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     call aerosol process routines

! Assign dummy variables (not used) for consistency with CMAQ aeroproc
col = 1
row = 1
layer = 1

!debug
!write(lun_log,*)'conc before aeroproc: ',(conc(j),j=1,naero)
!call flush(lun_log)
!debug
call aeroproc( dt, col, row, layer, GAMMA_N2O5 )

! *** Update aerosol variables conc back to main conc array (set minimum)
do imode = 1, N_MODE
  do spc = 1, N_AEROSPC
    j = aerospc_map(spc,imode)
    if ( j /= 0 ) then
      conc(j) = MAX(aerospc_conc(spc,imode), aerospc(spc)%min_conc(imode)) ! [ug/m^3]
    end if
  end do
end do

! Copy aero number and surface area back to main conc array
do imode = 1, N_MODE
  j = aeronum_map(imode)
  conc(j) = MAX(moment0_conc(imode), aeromode_minNum(imode))
  j = aerosrf_map(imode)
  conc(j) = PI * MAX(moment2_conc(imode), aeromode_minM2(imode))
end do

! copy precursor_conc back to main conc array
do spc = 1, N_PRECURSOR
  j = precursor_map(spc )
  if ( j /= 0 ) then
    if (precursor(spc)%update) then
      if (precursor(spc)%rxncounter) then
        conc(j) = 0.0
      else
        vtmp = factor*precursor_mw(spc)
        conc(j) = MAX(REAL(precursor_conc(spc),4)/vtmp, MIN_GASCONC)
      end If
    end If
  end if
end do

! Copy vapor_conc back to main conc array
do spc = 1, N_VAPOR
  j = vapor_map(spc)
  if ( j /= 0 ) then
    vtmp = factor*vapor_mw(spc)
    conc(j) = MAX(vapor_conc(spc)/vtmp, MIN_GASCONC)
  end if
end do

!debug
!write(lun_log,*)'conc after aeroproc: ',(conc(j),j=1,naero)
!call flush(lun_log)
!debug

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! *** Calculate 2nd and 3rd moments of the "dry" aerosol distribution
!     NOTE! "dry" aerosol excludes both H2O and SOA  (January 2004 --SJR)
!     EXCEPT!  nonvolatile SOA is part of dry aerosol (Oct 2007 --PVB)

! Aitken mode.
m3_wet = moment3_conc(1)
m3subt = (1.0E-9*F6DPI/aerospc(ah2o_idx)%density) * aerospc_conc(ah2o_idx,1)
m3_dry = m3_wet - m3subt
m2_wet = moment2_conc(1)
m2_dry = m2_wet * (m3_dry/m3_wet) ** TWOTHIRDS

moment3_conc(1) = MAX(CONMIN, m3_dry)
moment2_conc(1) = MAX(CONMIN, m2_dry)

! Accumulation mode.
m3_wet = moment3_conc(2)
m3subt = (1.0E-9*F6DPI/aerospc(ah2o_idx)%density) * aerospc_conc(ah2o_idx,2)
do spc = 1, N_VAPOR
  m3subt = m3subt + (1.0e-9*f6dpi/aerospc(apoc_idx)%density) &
                      * aerospc_conc(soa_aeromap(SPC),2)
end do
m3_dry = m3_wet - m3subt
m2_wet = moment2_conc(2)
m2_dry = m2_wet * (m3_dry/m3_wet) ** TWOTHIRDS

moment3_conc(2) = MAX(CONMIN, m3_dry)
moment2_conc(2) = MAX(CONMIN, m2_dry)

!     coarse mode
m3_wet = moment3_conc(3)
m3subt = (1.0E-9*F6DPI/aerospc(ah2o_idx)%density) * aerospc_conc(ah2o_idx,3)
m3_dry = m3_wet - m3subt
m2_wet = moment2_conc(3)
m2_dry = m2_wet * (m3_dry/m3_wet) ** TWOTHIRDS

moment3_conc(3) = MAX(conmin, m3_dry)
moment2_conc(3) = MAX(conmin, m2_dry)

! *** Calculate geometric mean diameters and standard deviations of the
!     "dry" size distribution
call getpar( M3_WET_FLAG, LIMIT_Sg )

! *** Calculate aerosol surface area from the dry 2nd moment.  Dry value is
!     used in transport routines.  Put aerosrf values back to main conc array
do imode = 1, N_MODE
  j = aerosrf_map(imode)
  conc(j) = PI * moment2_conc(imode)
end do

!debug
!write(lun_log,*)'conc after surface area correction: ',(conc(j),j=1,naero)
!call flush(lun_log)
!debug

return
end
