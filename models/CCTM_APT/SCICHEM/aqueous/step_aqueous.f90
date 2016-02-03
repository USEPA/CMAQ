!*********************************************************************** 
! This is the SCICHEM driver routine for the RADM aqueous-phase        *
! chemistry module in CMAQ (October 2004 version)                      *
! This code has been developed for Southern Company Services under     *
! subcontract to EPRI, 3412 Hillview Ave., Palo Alto, CA 94304         *
! Contract EP-P14638/C7185                                             * 
!                                                                      * 
! Developed by Prakash Karamchandani                                   * 
! Atmospheric and Environmental Research, Inc., 2682 Bishop Drive,     * 
! Suite 120, San Ramon, CA 94583                                       * 
!                                                                      * 
! CMAQ 4.6 compatibility updates, Feb 2008, PK, AER                    *
!                                                                      * 
! Updated Feb 2008 to include scavenging of species that do not        *
! participate in aqueous chemistry (for consistency with CMAQ call to  *
! SCAVWDEP, but done here without a separate call); PK, AER            *
!                                                                      *
! Completely rewritten for CMAQ 5.0, PK, ENVIRON, Mar 2012             *
!*********************************************************************** 
subroutine step_aqueous(dt,conc,ic_units,lwc,prate,fcc,fpc, &
                        tk,patm,naqchem,naqueous,aqueous_names,len,scav)

use aqueous_consts_inc
use aqueous_species_inc
use units_inc
use error_inc

!debug
use files_inc
!debug

implicit none

! --- ARGUMENTS

real dt              ! time step (seconds)
real conc(*)         ! array of species concentrations
integer ic_units     ! gas concentration units
real lwc, prate      ! liquid water content and precip. rate
                     ! (g/m3 and mm/hr)
real fcc             ! fractional cloud cover (convective)
real fpc             ! fraction of precip. that is convective
real tk, patm        ! temperature and pressure
integer naqchem      ! number of aqueous chemistry species
integer naqueous     ! number of aqueous species

character*(*) aqueous_names(*) ! names of SCICHEM species in aqueous

! --- length scale (for scavenging coefficient calculations) (m)
real len

real scav(*)         ! scavenging coefficients

! --- Local variables
real fpn             ! fraction of precip. that is non-convective

! --- Minimum cloud water content for aqueous calculations
REAL, PARAMETER :: LWCMIN = 0.00001   ! kg/m3

! --- Minimum time step (for scavenging coefficient calculations)
REAL, PARAMETER :: DTMIN = 1.E-20

integer i,j, spc, var  ! loop variables

real factorg, factorp, factorn
real lwckg
real rhoair     ! Air density (mole/m3)

real          alfa                ! scavenging coefficient (1/s)
real          kh                  ! Henry's law constant (mol/l/atm)

real alfa0       ! scavenging coefficient for number [ 1/s ]
real alfa2       ! scavenging coefficient for surface area [ 1/s ]
real alfa3       ! scavenging coefficient for mass [1/s]

real rhoairkg    ! Air density (kg/m3)

real pbar        ! Pressure in Pa

! Number of species for aqueous chemistry
Integer :: n_cgrid2aq = 0
Integer, Allocatable, save :: cgrid2aq_map( : ) ! mapping of aqueous species
                                                ! to main conc array
! Number of species scavenged
integer :: n_cgrid_scav = 0
integer, allocatable, save :: cgrid_scav_map( : ) ! cgrid map to scavenged spc
character( 16 ), allocatable, save :: cgrid_scav( : )  ! cgrid species scavenged
real, allocatable,    save :: cgrid_scav_fac( : )  ! CGRID scav coef factors

integer, allocatable, save :: l_numakn( : ) ! pointers to aitken aerosol #
integer, allocatable, save :: l_masakn( : ) ! pointers to aitken aerosols
integer, allocatable, save :: l_srfakn( : ) ! pntrs to aitken aerosol surface area

integer, save :: n_numakn            ! # aitken aerosol number species
integer, save :: n_masakn            ! # aitken aerosol mass species
integer, save :: n_srfakn            ! # aitken aerosol sfc area species

real          numakn              ! Aitken mode aerosol # (#/m3)
real          masakn              ! Total Aitken mode mass (ug/m3)
real          srfakn              ! Aitken mode total surface area

Integer :: ios
CHARACTER( 16 ) :: spname
integer       iaer                ! aerosol loop counter
integer       imode               ! aerosol mode loop counter
integer       igas                ! gas loop counter
integer       isrg                ! surrogate loop counter
integer       pntr                ! relative pointer variable

!REAL( 8 )  :: GAS    ( NGAS )               ! gas phase conc (mol/mol)
!REAL( 8 )  :: AEROSOL( NAER,NMODES )        ! aerosol conc (mol/mol)
!REAL( 8 )  :: WSRGGAS( NGAS,MXSRG )         ! weights for surrogate
!REAL( 8 )  :: WSRGAER( NAER,NMODES,MXSRG )  ! weights for surrogate

real  :: gas    ( NGAS )               ! gas phase conc (mol/mol)
real  :: aerosol( NAER,NMODES )        ! aerosol conc (mol/mol)
real  :: wsrggas( NGAS,MXSRG )         ! weights for surrogate
real  :: wsrgaer( NAER,NMODES,MXSRG )  ! weights for surrogate

real  :: caero  ( N_AEROSPC, N_MODE )  ! aerosol conc for calculating
                                       ! modal mass
integer, save :: c2_aero_map( N_AEROSPC, N_MODE )   ! cgrid map to aero species
integer, save :: c2_srf_map( N_MODE )               ! cgrid map to surface area

real          rtch          ! chemical gas const times temp (liter atm/mol)
real          twash         ! washout time for clouds (sec) with low liq wat content
real          one_over_twash      ! 1 / twash
real          twf                 ! washout scaling factor (mol/l/atm)

real,   parameter :: HPLUS = 1.0E-4   ! typical value hydrogen ion concentration [mol/l]

real :: hplusaq  ! Actual hydrogen ion conc from aqchem [mol/l]

real, dimension( N_MODE ) :: m3old    ! modal mass at time 0
real, dimension( N_MODE ) :: m3new    ! modal mass at time t

logical, save :: FIRSTTIME = .TRUE.

!...........External Functions:

INTEGER, EXTERNAL :: INDEXN
REAL,    EXTERNAL :: HLCONST

real scavmax

!debug
!  write(lun_log,*)'lun_log: ',lun_log
!  write(lun_log,*)'Firsttime = ',firsttime
!debug
if (FIRSTTIME) then

  FIRSTTIME = .FALSE.

! Set species mapping to aqueous chemistry module species
!...Check number of species in conc and used in aqueous module
  n_cgrid2aq = n_gc_g2aq + n_ae_a2aq + n_nr_n2aq + n_tr_t2aq
!debug
  write(*,*)'lun_log: ',lun_log
  write(lun_log,*)'n_cgrid2aq: ',n_cgrid2aq
  call flush(lun_log)
!debug
  if (n_cgrid2aq > naqueous) then
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'Too many aqueous chemistry species '
    return
  end if
  if (n_cgrid2aq /= naqchem) then
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'Mismatch in number of aqueous chemistry species'
    return
  end if

! debug
  write(lun_log,*)'Allocating cgrid2aq_map'
  call flush(lun_log)
!debug
  ALLOCATE(cgrid2aq_map(n_cgrid2aq), stat = ios)
  if (ios /= 0) then
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = '*** Error allocating cgrid2aq_map'
    return
  end if

  cgrid2aq_map = 0 
! Reactive gases
  spc = 0
!debug
!  write(lun_log,*)'shape(aqueous_names): ',shape(aqueous_names)
  write(lun_log,*)'aqueous_names: ',(aqueous_names(j),j=1,naqchem)
  write(lun_log,*)'n_gc_g2aq: ',n_gc_g2aq
  call flush(lun_log)
!debug
outer1: do i = 1, n_gc_g2aq
    spc = spc + 1
    spname = gc_spc(gc_g2aq_map(i))
! Find matching species
!debug
    write(lun_log,*)'i,spc,spname: ',i,spc,spname
    call flush(lun_log)
!debug
    do j = 1, naqchem
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        cgrid2aq_map(spc) = j
        CYCLE outer1  ! go to next aqueous chemistry species
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'No match for ' // spname
    call report_error
    stop
  end do outer1

! Aerosols
outer2: do i = 1, n_ae_a2aq
    spc = spc + 1
    spname = ae_spc(ae_a2aq_map(i))
!debug
    write(lun_log,*)'i,spc,spname: ',i,spc,spname
    call flush(lun_log)
!debug
! Find matching species
    do j = 1, naqchem
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        cgrid2aq_map(spc) = j
        CYCLE outer2  ! go to next aqueous chemistry species
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'No match for ' // spname
    call report_error
    stop
  end do outer2

! Non-reactives
outer3: do i = 1, n_nr_n2aq
    spc = spc + 1
    spname = nr_spc(nr_n2aq_map(i))
!debug
    write(lun_log,*)'i,spc,spname: ',i,spc,spname
    call flush(lun_log)
!debug
! Find matching species
    do j = 1, naqchem
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        cgrid2aq_map(spc) = j
        CYCLE outer3  ! go to next aqueous chemistry species
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'No match for ' // spname
    call report_error
    stop
  end do outer3

! Tracers
outer4: do i = 1, n_tr_t2aq
    spc = spc + 1
    spname = tr_spc(tr_t2aq_map(i))
! Find matching species
    do j = 1, naqchem
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        cgrid2aq_map(spc) = j
        CYCLE outer4  ! go to next aqueous chemistry species
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'No match for ' // spname
    return
  end do outer4

!debug  
  do i = 1, n_cgrid2aq
    write(lun_log,*)'map for aqueous species',i,' = ',cgrid2aq_map(i)
  end do
  call flush(lun_log)
!debug

! Now do mapping for scavenged species
  n_cgrid_scav = n_gc_scav + n_ae_scav + n_nr_scav + n_tr_scav
!debug
  write(lun_log,*)'n_cgrid_scav: ',n_cgrid_scav
  call flush(lun_log)
!debug
  if ( n_cgrid_scav <= 0 ) then
    eRoutine = 'step_aqueous'
    eMessage = 'No species were specified for scavenging by cloud ' // &
                'or rain water...SCAVENGING WILL NOT BE PERFORMED!'
  else
    ALLOCATE(cgrid_scav(n_cgrid_scav), &
             cgrid_scav_map(n_cgrid_scav), &
             cgrid_scav_fac(n_cgrid_scav), stat = ios)
    if (ios /= 0) then
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = '*** Error allocating cgrid_scav, cgrid_scav_map or cgrid_scav_fac'
      return
    end if
    ALLOCATE (l_numakn(MAX_MC),l_masakn(MAX_MC),l_srfakn(MAX_MC), stat = ios)
    if (ios /= 0) then
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = '*** Error allocating l_numakn,l_masakn,l_srfakn'
      return
    end if

!... load the CGRID to scavenged species pointers
    cgrid_scav_map = 0
    cgrid_scav_fac = 0
! Reactive gases
    spc = 0
outer5:   do i = 1, n_gc_scav
      spc = spc + 1
      spname = gc_spc(gc_scav_map(i))
      cgrid_scav(spc) = gc_scav(i)
      cgrid_scav_fac(spc) = gc_scav_fac(i)
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          cgrid_scav_map(spc) = j
          CYCLE outer5  ! go to next scavenged species
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = 'No match for ' // spname
      return
    end do outer5

! Aerosols
outer6:   do i = 1, n_ae_scav
      spc = spc + 1
      spname = ae_spc(ae_scav_map(i))
      cgrid_scav(spc) = ae_scav(i)
      cgrid_scav_fac(spc) = ae_scav_fac(i)
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          cgrid_scav_map(spc) = j
          CYCLE outer6  ! go to next scavenged species
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = 'No match for ' // spname
      return
    end do outer6

! Non-reactives
outer7:   do i = 1, n_nr_scav
      spc = spc + 1
      cgrid_scav(spc) = nr_scav(i)
      cgrid_scav_fac(spc) = nr_scav_fac(i)
      spname = nr_spc(nr_scav_map(i))
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          cgrid_scav_map(spc) = j
          CYCLE outer7  ! go to next scavenged species
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = 'No match for ' // spname
      return
    end do outer7

! Tracers
outer8:   do i = 1, n_tr_scav
      spc = spc + 1
      spname = tr_spc(tr_scav_map(i))
      cgrid_scav(spc) = tr_scav(i)
      cgrid_scav_fac(spc) = tr_scav_fac(i)
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          cgrid_scav_map(spc) = j
          CYCLE outer8  ! go to next scavenged species
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = 'No match for ' // spname
      return
    end do outer8
!...create the pointers from CGRID to the species needed by AQCHEM

!debug  
  do i = 1, n_cgrid_scav
    write(lun_log,*)'map for scavenged species',i,' = ',cgrid_scav_map(i)
  end do
  call flush(lun_log)
!debug

    n_numakn = INDEXN( 'NUM_AITKEN      ', n_cgrid_scav, cgrid_scav, l_numakn )
    n_masakn = INDEXN( 'AITKEN', n_cgrid_scav, cgrid_scav, l_masakn )
    n_srfakn = INDEXN( 'SRF_AITKEN      ', n_cgrid_scav, cgrid_scav, l_srfakn )

!debug
    write(lun_log,*)'n_numakn, n_masakn, n_srfakn: ',n_numakn, n_masakn, n_srfakn
    call flush(lun_log)
!debug
  end if

!...mapping to aero species
  c2_aero_map = 0
  do var = 1, N_AEROSPC
outer9:    do imode = 1, N_MODE
      spname = aerospc(var)%name(imode)
      if (spname == ' ') CYCLE ! go to next mode
      do spc = 1, naqueous
        if (TRIM(spname) == TRIM(aqueous_names(spc))) then
          c2_aero_map(var,imode) = spc
          CYCLE outer9 ! go to next mode
        end if
      end do
      nerror = UK_ERROR
      eRoutine = 'step_aqueous'
      eMessage = 'No match for ' // spname
      return
    end do outer9
  end do

!debug  
  do var = 1, N_AEROSPC
    do imode = 1, N_MODE
      write(lun_log,*)'map for aero species',var,'; mode ',imode,' = ',c2_aero_map(var,imode)
    end do
  end do
  call flush(lun_log)
!debug

  c2_srf_map = 0
outer10:  do imode = 1, N_MODE
    spname = aeromode(imode)%srf_name
    do spc = 1, naqueous
      if (TRIM(spname) == TRIM(aqueous_names(spc))) then
        c2_srf_map(imode) = spc
        CYCLE outer10 ! go to next mode
      end if
    end do
    nerror = UK_ERROR
    eRoutine = 'step_aqueous'
    eMessage = 'No match for ' // spname
    return
  end do outer10
!debug  
  do imode = 1, N_MODE
    write(lun_log,*)'map for srf mode ',imode,' = ',c2_srf_map(imode)
  end do
  call flush(lun_log)
!debug

end if  !FIRSTTIME

!...for subsequent calls, check to make sure some species are specified
!...for aqueous chemistry or scavenging

if (n_cgrid2aq == 0 .and. n_cgrid_scav == 0) then
  return
end if

lwckg = lwc * 1.E-3  ! g/m3 water to kg/m3

!debug
!write(lun_log,*)'lwckg: ',lwckg
!call flush(lun_log)
!debug
! --- Scavenging coefficient calculations for Aitken mode variables
! --- Aitken mode number concentration (#/m3)
numakn = 0.0
do i = 1, n_numakn
  pntr = cgrid_scav_map(l_numakn(i))
  numakn = numakn + conc(pntr)
end do

! --- Aitken mode surface area (m2/m3)
srfakn = 0.0
do i = 1, n_srfakn
  pntr = cgrid_scav_map(l_srfakn(i))
  srfakn = srfakn + conc(pntr)
end do
!debug
!write(lun_log,*)'numakn, srfakn: ',numakn,srfakn
!call flush(lun_log)
!debug

! --- Total Aitken mode mass (ug/m3)
masakn = 0.0
do i = 1, n_masakn
  pntr = cgrid_scav_map(l_masakn(i))
!debug
!  write(lun_log,*)'i, pntr: ',i,pntr
!  call flush(lun_log)
!debug
  if ((index(cgrid_scav(l_masakn(i)), 'NUM') == 0 ) .and. &
      (index(cgrid_scav(l_masakn(i)), 'SRF') == 0 ) .and. &
      (index(cgrid_scav(l_masakn(i)), 'H2O') == 0 ) .and. &
      (index(cgrid_scav(l_masakn(i)), 'TRACER') == 0 ) ) then
    masakn = masakn + conc(pntr)
!debug
!    write(lun_log,*)'cgrid_scav,masakn: ',cgrid_scav(l_masakn(i)),masakn
!    call flush(lun_log)
!debug
  end if
end do
!debug
!write(lun_log,*)'masakn: ',masakn
!call flush(lun_log)
!debug

rhoair = patm * 1.E3 / (RGAS * tk)   ! Air density in moles/m3
rhoairkg = rhoair * MWAIR * 1.E-3    ! Air density in kg/m3
pbar = patm * STDATMPA               ! Pressure in Pa

! --- calculate in-cloud scavenging coefficients
call getalpha (numakn, masakn, srfakn, lwckg, tk, pbar, &
     &         rhoairkg, alfa0, alfa2, alfa3 )

!debug
!write(lun_log,*)'alfa0,alfa2,alfa3: ',alfa0,alfa2,alfa3
!call flush(lun_log)
!debug
!
!     ZERO SPECIES MATRICES

!scavgas = 0.0
!scavaer = 0.0

! Aqueous-phase chemistry
if (lwckg > LWCMIN) then

!
! --- Set conversion factors for gas-phase species concentrations
! --- (convert to moles/mole of air)
  if (ic_units == UNIT_PPM) then
    factorg = 1.0E-06 * tk / ( 298. * patm )
  else if (ic_units == UNIT_MOLECULE) then
    factorg = 1.3634E-22 * tk / patm
  end if

! --- Set conversion factors for particle-phase species concentrations
! --- (convert to moles/mole of air)
  factorp = 1.E-6 / rhoair

! --- Set conversion factors for particle number and surface area
! --- concentrations (convert to #/mole of air and m2/mole of air)
  factorn = 1. / rhoair

!debug
!write(lun_log,*)'ic_units,factorg,factorp,factorn: ',ic_units,factorg,factorp,factorn
!call flush(lun_log)
!debug

!...load gas-phase concentrations (mol/mol air)
  gas = 0.0
  wsrggas = 0.0
!debug
!  write(lun_log,*)'NGAS: ',NGAS
!  call flush(lun_log)
!debug
  do igas = 1, NGAS

!debug
!     write(lun_log,*)'igas,nsrggas,name,backgnd: ', &
!                igas,nsrggas(igas),srggas(igas)%name,srggas(igas)%backgnd
!debug
    do isrg = 1, nsrggas(igas)
      pntr = cgrid2aq_map(lsrggas(igas,isrg))
!     gas(igas) = gas(igas) + real(MAX(conc(pntr)*factorg,CONMIN),8)
      gas(igas) = gas(igas) + MAX(conc(pntr)*factorg,CONMIN)
!debug
!    write(lun_log,*)'isrg,pntr: ',isrg,pntr
!debug
    end do
!    if (gas(igas) > 0.0d0) then
!debug
!    write(lun_log,*)'gas(',igas,'): ',gas(igas)
!debug
    if (gas(igas) > 0.0) then
      do isrg = 1, nsrggas(igas)
        pntr = cgrid2aq_map(lsrggas(igas,isrg))
!       wsrggas(igas,isrg) = REAL(MAX(conc(pntr)*factorg,CONMIN),8) / gas(igas)
        wsrggas(igas,isrg) = MAX(conc(pntr)*factorg,CONMIN) / gas(igas)
      end do
    else
      do isrg = 1, nsrggas(igas)
!       wsrggas(igas,isrg) = 1.0d0 / REAL(nsrggas(igas),8)
        wsrggas(igas,isrg) = 1.0 / nsrggas(igas)
      end do
    end if
!debug
!    do isrg = 1, nsrggas(igas)
!       write(lun_log,*)'wsrg(',igas,',',isrg,'): ',wsrggas(igas,isrg)
!    end do
!    call flush(lun_log)
!debug

!...set background values for gases if no surrogates were specified

    if (nsrggas(igas) == 0) then
!     gas(igas) = REAL(srggas(igas)%backgnd,8) * 1.0d-6
      gas(igas) = srggas(igas)%backgnd * 1.0d-6
    end if
!debug
!    write(lun_log,*)'gas(',igas,'): ',gas(igas)
!    call flush(lun_log)
!debug

  end do

!...load aerosol concentrations
  aerosol = 0.0
  wsrgaer = 0.0

!debug
!  write(lun_log,*)'NAER,NMODES: ',NAER,NMODES
!  call flush(lun_log)
!debug
  do iaer = 1, NAER
    do imode = 1, NMODES

!debug
!      write(lun_log,*)'iaer,imode,nsrgaer,name,backgnd: ', &
!                 iaer,imode,nsrgaer(iaer,imode),srgaer(iaer)%name(imode), &
!                 srgaer(iaer)%backgnd
!debug
    
      if (srgaer(iaer)%name(imode) /= ' ' ) then
        do isrg = 1, nsrgaer(iaer,imode)
          pntr = cgrid2aq_map(lsrgaer(iaer,imode,isrg))
!debug
!          write(lun_log,*)'isrg,imode,pntr: ',isrg,imode,pntr
!debug
          if (srgaer(iaer)%name(imode)(1:3) /= 'NUM' .AND. &
              srgaer(iaer)%name(imode)(1:3) /= 'SRF' ) then
!...aerosol mass concentrations (mol/mol air)
            aerosol(iaer,imode) = aerosol(iaer,imode) + &
!                REAL(MAX(conc(pntr)*factorp/srgaer(iaer)%molwt,CONMIN),8)
                 MAX(conc(pntr)*factorp/srgaer(iaer)%molwt,CONMIN)
!debug
!            write(lun_log,*)'aerosol(',iaer,',',imode,'): ',aerosol(iaer,imode)
!debug
          else
!...aerosol no. concentrations and surface area (#/mol air and m2/mol of air)
            aerosol(iaer,imode) = aerosol(iaer,imode) + &
!                REAL(MAX(conc(pntr)*factorn,CONMIN),8)
                 MAX(conc(pntr)*factorn,CONMIN)
!debug
!            write(lun_log,*)'aerosol(',iaer,',',imode,'): ',aerosol(iaer,imode)
!debug
          end if
        end do
!       if (aerosol(iaer,imode) > 0.0d0) then
        if (aerosol(iaer,imode) > 0.0) then
          do isrg = 1, nsrgaer(iaer,imode)
            pntr = cgrid2aq_map(lsrgaer(iaer,imode,isrg))
            if (srgaer(iaer)%name(imode)(1:3) /= 'NUM' .AND. &
                srgaer(iaer)%name(imode)(1:3) /= 'SRF' ) then
!             wsrgaer(iaer,imode,isrg ) = REAL(MAX(conc(pntr)*factorp/ &
!                   srgaer(iaer)%molwt,CONMIN),8) / aerosol(iaer,imode)
              wsrgaer(iaer,imode,isrg ) = MAX(conc(pntr)*factorp/ &
                    srgaer(iaer)%molwt,CONMIN) / aerosol(iaer,imode)
            else
              wsrgaer(iaer,imode,isrg ) = MAX(conc(pntr)*factorn,CONMIN) &
                                              / aerosol(iaer,imode)
            end if
          end do
        else
          do isrg = 1, nsrgaer(iaer,imode)
!             wsrgaer(iaer,imode,isrg) = 1.0d0 / REAL(nsrgaer(iaer,imode),8)
            wsrgaer(iaer,imode,isrg) = 1.0 / nsrgaer(iaer,imode)
          end do
        end if

!debug
!        do isrg = 1, nsrgaer(iaer,imode)
!          write(lun_log,*)'wsrg(',iaer,',',imode,',',isrg,'): ', &
!                     wsrgaer(iaer,imode,isrg)
!        end do
!debug

!...set background values for aerosols if no surrogates were specified

        if (nsrgaer(iaer,imode) == 0) then
          if (srgaer(iaer)%molwt > 0.0) then
!           aerosol(iaer,imode) = REAL(srgaer(iaer)%backgnd*factorp/ &
!                                      srgaer(iaer)%molwt,8)
            aerosol(iaer,imode) = srgaer(iaer)%backgnd*factorp/ &
                                         srgaer(iaer)%molwt
          else
!           aerosol(iaer,imode) = REAL(srgaer(iaer)%backgnd/rhoair,8)
            aerosol(iaer,imode) = srgaer(iaer)%backgnd/rhoair
          end if
        end if

      end if
    end do
  end do
!debug
!  call flush(lun_log)
!debug

! *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)

  m3old = 0.0
  DO imode = 2, N_MODE

    DO spc = 1, N_AEROSPC
      IF ( (aerospc( spc )%name( imode ) /= ' ') .AND. &
           (.NOT. aerospc( spc )%no_m2wet)) THEN
        m3old(imode) = m3old(imode) &
                     + (conc(c2_aero_map(spc,imode)) / aerospc(spc)%density)
      END IF
    END DO

  END DO

! call aqradm(tk, patm, dt, prate, lwckg,
!debug
!  write(lun_log,*)'hplusaq before aqradm (not assigned): ',hplusaq
!  call flush(lun_log)
!debug
  CALL aqradm(tk, patm, dt, lwckg, &
              alfa0, alfa2, alfa3, &
              gas, aerosol, rhoair, hplusaq )
!, len, 
!     &            scavgas, scavaer)
!debug
!  write(lun_log,*)'hplusaq after aqradm: ',hplusaq
!  call flush(lun_log)
!debug

  if (nError /= NO_ERROR) go to 9999
!
! --- Assign aqueous-species concentrations back to main concentration array
! --- Gases
  factorg = 1. / factorg
  do igas = 1, NGAS

    do isrg = 1, nsrggas(igas)
      pntr = cgrid2aq_map(lsrggas(igas,isrg))
      conc(pntr) = gas(igas) * wsrggas(igas,isrg) * factorg
    end do

  end do

! --- Particles
  factorp = 1. / factorp
  factorn = 1. / factorn

  do iaer = 1, NAER
    do imode = 1, NMODES

      if (srgaer(iaer)%name(imode) /= ' ' ) then
        do isrg = 1, nsrgaer(iaer,imode)
          pntr = cgrid2aq_map(lsrgaer(iaer,imode,isrg))
          if (srgaer(iaer)%name(imode)(1:3) /= 'NUM' .AND. &
              srgaer(iaer)%name(imode)(1:3) /= 'SRF' ) then

!...aerosol mass concentrations (ug/m3 air)
            conc(pntr) = aerosol(iaer,imode) * wsrgaer(iaer,imode,isrg) * &
                         factorp * srgaer(iaer)%molwt
          else
!...aerosol no. concentrations and surface area (#/mol air and m2/mol of air)
            conc(pntr) = aerosol(iaer,imode) * wsrgaer(iaer,imode,isrg) * &
                         factorn
          end if
        end do
      end if
    end do
  end do

! *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)

  m3new = 0.0
  DO imode = 2, N_MODE

    DO spc = 1, N_AEROSPC
      IF ( (aerospc( spc )%name( imode ) /= ' ') .AND. &
           (.NOT. aerospc( spc )%no_m2wet)) THEN
        m3new(imode) = m3new(imode) &
                     + (conc(c2_aero_map(spc,imode)) / aerospc(spc)%density)
      END IF
    END DO

  END DO

!...check for minimums

  DO imode = 2, N_MODE
    m3old( imode ) = MAX( m3old( imode ), CONMIN )
    m3new( imode ) = MAX( m3new( imode ), CONMIN )
  END DO

!...Update surface areas for accumulation and coarse modes
!...Aitken mode already done in aqradm
  DO imode = 2, N_MODE
    conc(c2_srf_map(imode)) = conc(c2_srf_map(imode)) * &
                              (m3new(imode) / m3old(imode)) ** TWOTHIRDS
  END DO

else

  hplusaq = hplus  ! Default if no cloud chemistry

end if

! --- Scavenging coefficients
do i = 1, naqueous
  scav(i) = 0.
end do

if (prate <= 0.0 .OR. lwckg/rhoairkg <= 0.00005 ) return

rtch = (MOLVOL / STDTEMP) * tk
twash = lwckg*1000.0*len*3600.0 / ( H2ODENS*MAX( 1.0E-20, prate ) )
twash = MAX( twash, dt )
one_over_twash = 1.0 / twash
twf = H2ODENS / ( lwckg * rtch )

!...gas scavenging

spc = 0

do var = 1, n_gc_scav
  spc = spc + 1
  pntr = cgrid_scav_map( spc )
  kh = HLCONST( cgrid_scav( spc ), tk, .true., hplusaq )
  if ( kh > 0.0 ) then
    alfa = cgrid_scav_fac( spc ) * one_over_twash / ( 1.0 + twf / kh )
  else
    alfa = 0.0
  end if
  scav(pntr) = alfa
!debug
!  write(lun_log,*)'var,pntr,name,kh,scav: ',var,pntr,cgrid_scav(spc),kh,alfa
!  call flush(lun_log)
!debug
end do

!...aerosol scavenging
do var = 1, n_ae_scav
  spc = spc + 1
  pntr = cgrid_scav_map( spc )

  IF ( INDEX( cgrid_scav(spc), 'AITKEN' ) > 0 ) CYCLE
! if ( INDEX( cgrid_scav(spc), 'AITKEN' ) > 0 ) THEN
!   if ( INDEX( cgrid_scav(spc), 'NUM' )  > 0 ) THEN
!     alfa = cgrid_scav_fac(spc) * alfa0
!   else if ( INDEX( cgrid_scav(spc), 'SRF' ) > 0 ) THEN
!     alfa = cgrid_scav_fac(spc) * alfa2
!   else
!     alfa = cgrid_scav_fac(spc) * alfa3
!   end if
! else
!   alfa = cgrid_scav_fac(spc) * one_over_twash
!  end if
  scav(pntr) = cgrid_scav_fac(spc) * one_over_twash           !alfa
!debug
!  write(lun_log,*)'var,pntr,name,scav: ',var,pntr,cgrid_scav(spc),scav(pntr)
!  call flush(lun_log)
!debug
end do

!...non-reactive scavenging
do var = 1, n_nr_scav
  spc = spc + 1
  pntr = cgrid_scav_map( spc )
  kh = HLCONST( cgrid_scav( spc ), tk, .true., hplusaq )
  if ( kh > 0.0 ) then
    alfa = cgrid_scav_fac( spc ) * one_over_twash / ( 1.0 + twf / kh )
  else
    alfa = 0.0
  end if
  scav(pntr) = alfa
!debug
!  write(lun_log,*)'var,pntr,name,kh,scav: ',var,pntr,cgrid_scav(spc),kh,alfa
!  call flush(lun_log)
!debug
end do

!...tracer scavenging
do var = 1, n_tr_scav
  spc = spc + 1
  pntr = cgrid_scav_map( spc )
  kh = HLCONST( cgrid_scav( spc ), tk, .true., hplusaq )
  if ( kh > 0.0 ) then
    alfa = cgrid_scav_fac( spc ) * one_over_twash / ( 1.0 + twf / kh )
  else
    alfa = 0.0
  end if
  scav(pntr) = alfa
end do

! --- Limit scavenging coefficients
if ( dt > DTMIN ) then
  scavmax = 1. / dt
! scavmax = MIN( scavmax, 2.78E-4 )
else
  scavmax = 0.
end if
!debug
!write(lun_log,*)'scavmax: ',scavmax
!call flush(lun_log)
!debug
do i = 1, naqueous
  scav(i) = MIN( scav(i), scavmax )
end do

! --- Adjust scavenging coefficents for convective precipitation
fpn = 1. - fpc
!debug
!write(lun_log,*)'fpc,fpn,fcc: ',fpc,fpn,fcc
!call flush(lun_log)
!debug
do i = 1, naqueous
  scav(i) = ( fpn + fcc * fpc ) * scav(i)
end do

9999  return
      end
