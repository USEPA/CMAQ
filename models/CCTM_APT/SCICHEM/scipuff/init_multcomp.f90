!******************************************************************************
!Changed read_mc_species to read additional particle types (number concs. and
!surface area
!Changed read_mc_eqn to read additional reaction types
!Changed set_mc_rel to read emissions of particle number concs and surface
!area
!Changed init_mcp initialization of aerosol & aqueous chemistry
!Changed function isMCParticle for additional particle types
!PK, AER, June 2005
!PK, AER, January and February 2005
!New water-dependent reaction type added, March 22, 2005, PK, AER
!Changed species names from C*8 to C*16 for CMAQ-APT-PM, April 2005, PK AER
!Remove redundant variable O3Max, use MAX_MC2 and constants from constant_fd, Jan 2007, BC
!Updates for parallel version Jan 2007, BC
!Updated for CMAQ 5.0 final, March 2012, PK, ENVIRON
!******************************************************************************

subroutine read_mc(read_mode)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Reads multi-component definitions from project.imc
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         read_mc_control                  cupper         read_mc_species
!           read_mc_table             read_mc_eqn            setup_ktable
!       set_species_lists                init_mcp             set_mc_corr
!             set_mc_matl             read_mc_bal             read_mc_lim
!
! REVISION HISTORY: 
!
! 07/24/2001 : Corrected pointers for turbulent correlations (BC)
! 05/23/2005 : Added release list pointers to account for moving equilibrium
!              species to end of species list (RIS)
!*******************************************************************************
 
! --- MODULES

use common_puf
use multcomp_inc
use files_inc

implicit none

! --- ARGUMENTS

integer read_mode  ! = 0 when called during a normal run (reads all data)
                   ! = MODE_S or MODE_CTRL when called by GUI for plotting

! --- PARAMETERS
 
character*1 SET_MODE,    SPECIES_MODE,    EQUATION_MODE
parameter ( SET_MODE='#',SPECIES_MODE='S',EQUATION_MODE='E')
character*1 TABLE_MODE,    CONTROL_MODE, AQUEOUS_MODE
parameter ( TABLE_MODE='T',CONTROL_MODE='C',AQUEOUS_MODE='A')
character*1 BALANCE_MODE, LIMIT_MODE
parameter ( BALANCE_MODE='B', LIMIT_MODE='L')

! --- LOCALS

integer ios, imode, jmode, i, j, k
integer is, ie, iA, iB, ir, nblank

character*300 line
real  eqmparam(MAX_EQ,5)

!====   Open IMC file

open(unit=lun_tmp,file=file_imc,status='OLD',iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'read_mc'
  eMessage = 'Error opening Multi-component input file'
  eInform  = 'File='//TRIM(file_imc)
  eAction  = 'Make sure file exists'
  go to 9999
end if

!====   Initialize

imode    = MODE_NA
nstage   = 1
istage   = 1
nspecies = 0
nspectot = 0
ncorrm   = 0
ncorrt   = 0
nzenith  = 0
nkrad    = 0
nreacts  = 0
nslow    = 0
nfast    = 0
nequilibrium = 0
neq_s    = 0
nambient = 0
nvoc     = 0
lamb3d   = .false.
amb_file = NOT_SET_C
lstep_amb = .false.
ks_conv   = 1.
kt_conv   = 1.

do i = 1, MAX_STAGES
  nreact_s(i) = 0
end do
do i = 1,MAX_KTAB
  nreadkr(i) = 0
end do
lbalance = .false.

!====   Set Read modes

if(read_mode == 0)then
  is = min0(MODE_BAL,MODE_LIM,MODE_S,MODE_T,MODE_EQ,MODE_CTRL)
  ie = max0(MODE_BAL,MODE_LIM,MODE_S,MODE_T,MODE_EQ,MODE_CTRL)
else
  is = read_mode
  ie = read_mode
end if

!====   Loop over read modes

line = NOT_SET_C

do jmode = is,ie

!====   Advance input to IMC Control line
  
  do while (line(1:1) /= SET_MODE)
    read(lun_tmp,1000,iostat=ios) line
    if (ios /= 0) then
      nError   = RD_ERROR
      eRoutine = 'read_mc'
      eMessage = 'Error reading Multi-component input file'
      eInform  = 'File='//TRIM(file_imc)
      go to 9999
    end if
  end do
  backspace(lun_tmp)
  line = NOT_SET_C

!====== Control Mode read - Namelist

  if(jmode == MODE_CTRL)then
    call read_mc_control(lun_tmp)
    if (nError /= NO_ERROR) go to 9999
    go to 200
  end if

!====== Loop over lines in file

100       continue

  read(lun_tmp,1000,end=200,iostat=ios) line
1000      format(a)
  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_mc'
    eMessage = 'Error reading Multi-component input file'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

!====== Section change - Set file mode and go get next line

  call cupper(line)
  if (line(1:1) == SET_MODE) then
    if (line(2:2) == SPECIES_MODE) then
      imode = MODE_S
    else if (line(2:2) == TABLE_MODE) then
      imode = MODE_T
    else if (line(2:2) == EQUATION_MODE) then
      imode = MODE_EQ
    else if (line(2:2) == CONTROL_MODE) then
      imode = MODE_CTRL
    else if (line(2:2) == AQUEOUS_MODE) then
      imode = MODE_AQ
    else if (line(2:2) == BALANCE_MODE) then
      imode = MODE_BAL
    else if (line(2:2) == LIMIT_MODE) then
      imode = MODE_LIM
    else
      imode = MODE_NA
    end if
    go to 100
  end if

!====== Decipher line if read mode = file mode

  if (imode == MODE_S) then       !species mode
    if (imode == jmode) then
      call read_mc_species(line, eqmparam)
      if (nError /= NO_ERROR) go to 9999
    end if
  else if (imode == MODE_T) then  !table mode
    if (imode == jmode) then
      call read_mc_table(line)
      if (nError /= NO_ERROR) go to 9999
    end if
  else if (imode == MODE_EQ) then !equation mode
    if (imode == jmode) then
      call read_mc_eqn(line)
      if (nError /= NO_ERROR) go to 9999
    end if
  else if (imode == MODE_BAL) then !balance mode
    if (imode == jmode) then
      call read_mc_bal(line, eqmparam)
      if (nError /= NO_ERROR) go to 9999
    end if
  else if (imode == MODE_LIM) then !limit mode
    if (imode == jmode) then
      call read_mc_lim(line, eqmparam)
      if (nError /= NO_ERROR) go to 9999
    end if
  else if (imode == MODE_AQ) then   !aqueous - read in init_aqueous
  else if (imode == MODE_CTRL) then !control - already read
  else
    nError   = RD_ERROR
    eRoutine = 'read_mc'
    eMessage = 'Error setting Multi-component input mode'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

!====== Go get next line

  go to 100

!====== EOF reached

200       continue

!====== Rewind file and go to next read mode

  rewind(unit=lun_tmp,iostat=ios)

end do

if(read_mode /= 0)go to 9999

!====   Write number of reactions to log

if (.not. lstage) write(lun_log,*) 'No. of total reactions: ',nreacts

!====   Check zenith table for completness and set up reaction index

call setup_ktable

!====   check chemistry stages

if (lstage) nstage = nstage - 1
if (nstage /= MAX_STAGES .and. lstage) then
  nError   = IV_ERROR
  eRoutine = 'read_mc'
  eMessage = 'Incorrect number of chemical stages'
  write(eAction,*)'Number of stages should be: ', &
                         MAX_STAGES
  eInform  = 'File='//TRIM(file_imc)
  go to 9999
end if

!====   set equilibrium species release indices

j = 0
do i = 1, nspectot
  if (i_rel_list(i) == -1 ) then
    j = j + 1
    i_rel_list(i) = nspecies + j
  end if
end do

!====   expand species structure to contain equilibrium

do i = nspecies+1, nspectot
  species(i)%class = ID_SPECIES_EQUILIBRIUM
  species(i)%star = 0
  species(i)%dos = 0
  species(i)%dep = 0
  species(i)%pmode = 0
  species(i)%amb = equilibrium(i-nspecies)%amb
  species(i)%tol = equilibrium(i-nspecies)%tol
  species(i)%vdep = eqmparam(i-nspecies,1)
  species(i)%emis_split = 0.
  species(i)%density = 0.
  species(i)%name = equilibrium(i-nspecies)%name
  species(i)%nameemit = ' '
  sparam(i)%scav = eqmparam(i-nspecies,2)
  sparam(i)%mwt  = eqmparam(i-nspecies,3)
  sparam(i)%nit  = eqmparam(i-nspecies,4)
  sparam(i)%lim  = eqmparam(i-nspecies,5)
  sparam(i)%param = 0.
end do

close(unit=lun_tmp,iostat=ios)

!====   Set species names lists

call set_species_lists
if(nError /= NO_ERROR)go to 9999

!====   Post read Initialization

call init_mcp
if(nError /= NO_ERROR)go to 9999

!====   Set Pointers

call set_mc_corr
if(nError /= NO_ERROR)go to 9999

!====   Write mass and concentration names and order to log file

write(lun_log,*)
write(lun_log, *) 'Order of species masses in puff file:'
do j = 1, nspectot
  write(lun_log,'(''MC'',i3.3, 8x,A)') j, species(j)%name
end do

if (ncorrm > 0) then
  write(lun_log,*)
  write(lun_log, *) 'Order of species concentrations in puff file:'
  do i = 1, ncorrm
    do j = 1, nspectot
      if (nspectot+ncorrt+i == species(j)%star) then
        write(lun_log,'(''CM'',i3.3, 8x, A)') i, species(j)%name
      end if
    end do
  end do
end if
if (ncorrt > 0) then
  write(lun_log,*)
  write(lun_log, *) 'Order of species correlations in puff file:'
  do j = 1, ncorrt
    ir = indx_corr(j)
    iA = reaction(ir)%iA
    iB = reaction(ir)%iB
    i = nblank(species(iA)%name)
    k = nblank(species(iB)%name)
    write(lun_log,'(''CT'',i3.3, 8x,A)') j, species(iA)%name(1:i) // &
             '-' // species(iB)%name(1:k)
  end do
end if
write(lun_log,*)

!====   Set material and typeID structures for multi-components

call set_mc_matl

9999    continue

return
end

subroutine read_mc_names(nsp,snames,sunits)
!*******************************************************************************
!
! FUNCTION: Reads multi-component names only from project.imc - used by GUI
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 read_mc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
integer nsp               !number of species
character*(*) snames(1)   !species names
character*(*) sunits      !species units

! --- LOCALS

integer i

nsp = 0
call read_mc(MODE_S)
if(nError /= NO_ERROR)go to 9999

nsp = nspecies
do i = 1,nspecies
  snames(i) = species(i)%name
end do

call read_mc(MODE_CTRL)
if(nError /= NO_ERROR)go to 9999
sunits   = mc_units

9999    return
end

subroutine read_mc_control(lun)
!*******************************************************************************
!
! FUNCTION:  Reads multi-component control definitions from project.imc
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           set_imc_flags                  cupper
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

integer lun  !unit number of IMC file
 
! --- LOCALS

integer i

logical       step_ambient       !flag to step the ambient
logical       staged_chem        !flag for staged chemistry
logical       solve_ynb          !flag for Young and Boris solver (no longer used)
logical       chem_split         !flag for chemistry criteria splitting
logical       aqueous            !flag for aqueous chemistry
logical       aerosol            !flag for aerosol thermodynamics
logical       dump_chem          !flag for plume-in-grid chemical dumping criteria
character*16  emission_units     !g/s or ppm-m3/s
character*16  species_units      !ppm or molecules/cm3
character*16  rate_species_units !ppm or molecules/cm3
character*16  rate_time_units    !min or sec
character*128 ambient_file       !file name for 3D and/or time-dep ambient data
character*8   solver             !name of solver for rate equations

namelist / control / step_ambient, staged_chem, species_units, &
                        emission_units, rate_species_units, &
                        rate_time_units, ambient_file, rtol, &
                        voc_names, phno3, pno3, cno3, rho2o3, co3, rvocno2, &
                        solver, solve_ynb, chem_split, aqueous, aerosol, &
                        secbnds_aer, rho_aer, nsec_aer, &
                        dump_chem, param_chem

!===== Set default values

step_ambient       = .false.
staged_chem        = .false.
solve_ynb          = .false.
chem_split         = .false.
aqueous            = .false.
aerosol            = .false.
dump_chem          = .false.
species_units      = 'PPM'
emission_units     = 'PPM-M3/S'
rate_species_units = 'molecules/cm3'
rate_time_units    = 'sec'
ambient_file       = NOT_SET_C
solver             = 'YNB'
rtol               = 1.e-2
tambient           = 0.0

do i = 1, MAX_VOC
  voc_names(i) = TRIM(NOT_SET_C)
end do

phno3 = 0.1
pno3 = 0.1
cno3 = 1.e-8
rho2o3 = 0.1
co3 = 1.e-3
rvocno2 = 1.0

nsec_aer = 1
secbnds_aer(1) = 0.4e-6
secbnds_aer(2) = 0.4e-6
rho_aer        = 1000.
param_chem     = 0.01

200   read(lun,control,err=200,end=201)

lstage      =  staged_chem
lsolve_ynb  =  solve_ynb
lchem_split =  chem_split
laqueous    =  aqueous
laerosol    =  aerosol
ldump_chem  =  dump_chem
lstep_amb   =  step_ambient

! if aqueous is selected, then aerosol needs to be selected too.
if (laqueous) then
  if (.not. laerosol) then
    nError   = IV_ERROR
    eRoutine = 'ReadMCcontrol'
    eMessage = 'Aqueous chemistry requires aerosol module'
    write(eAction,*)'Set aerosol to true in imc file'
    go to 9999
  end if
end if

call set_imc_flags(species_units,rate_species_units, &
                   rate_time_units,ambient_file)

phno3   = phno3*1.e-2/3600. !convert from %/hr to frac/sec
pno3    = pno3*1.e-2/3600.
rho2o3  = rho2o3*1.e-2      !convert from % to frac
rvocno2 = rvocno2*1.e-2

!===== Set reaction rate coefficient conversion (to sec)

call cupper(rate_time_units)
if(rate_time_units(1:1) == 'S')then
  kt_conv = 1.0
else if(rate_time_units(1:1) == 'M')then
  kt_conv = 1./60.0
else if(rate_time_units(1:1) == 'H')then
  kt_conv = 1./3600.0
else
  nError   = IV_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage = 'Invalid Rate time units'
  eInform  = 'Units='//TRIM(rate_time_units)
  go to 9999
end if

!===== Save rate species units in i_units

mc_units = species_units
call cupper(rate_species_units)
call cupper(species_units)
if(TRIM(rate_species_units) == 'MOLECULES/CM3')then
  i_units = UNIT_MOLECULE
else if(TRIM(rate_species_units) == 'PPM')then
  i_units = UNIT_PPM
else
  nError   = IV_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage = 'Invalid Rate species units'
  eInform  = 'Units='//TRIM(rate_species_units)
  go to 9999
end if

!===== Save species units in i_units

if(TRIM(species_units) == 'MOLECULES/CM3')then
  i_units = i_units + 16*UNIT_MOLECULE
else if(TRIM(species_units) == 'PPM')then
  i_units = i_units + 16*UNIT_PPM
else
  nError   = IV_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage = 'Invalid Rate species units'
  eInform  = 'Units='//TRIM(species_units)
  go to 9999
end if

!===== Save emissions units in ie_units

call cupper(emission_units)
if(TRIM(emission_units) =='PPM-M3/S' )then
  ie_units = UNIT_PPM
else if(TRIM(emission_units) =='MOLECULES-M3/CM3-S')then
  ie_units = UNIT_MOLECULE
else if(TRIM(emission_units) =='G/S')then
  ie_units = UNIT_G
else
  nError   = IV_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage = 'Invalid Emission species units'
  eInform  = 'Units='//TRIM(emission_units)
  go to 9999
end if

if (ambient_file /= ' ') then
  amb_file = ambient_file
else
  amb_file = NOT_SET_C
end if

!===== Set solver type

call cupper(solver)
if (TRIM(solver) == 'LSODE') then
  isolve = ID_LSODE
else if (TRIM(solver) == 'VODE') then
  !isolve = ID_VODE  !disable VODE
  isolve = ID_YNB
  nError   = WN_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage='VODE solver is disabled'
  eInform='The solver will be Young & Boris'
  call WarningMessage(0,.true.)
else if (TRIM(solver) == 'YNB') then
  isolve = ID_YNB
else
  nError   = IV_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage = 'Do not recognize solver type'
  eInform  = 'Type='//TRIM(solver)
!  eAction  = 'Choices are: LSODE, VODE, or YNB'
  eAction  = 'Choices are: LSODE or YNB'
  go to 9999
end if

!===== Perform checks

if (lsolve_ynb .and. isolve /= ID_YNB) then
  nError   = WN_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage='lsolve_ynb will be ignored'
  eInform='The solver is set using the solver flag'
  if (isolve == ID_LSODE) then
    eAction='Using LSODE to solve the rate equations'
  else if (isolve == ID_VODE) then
    eAction='Using VODE to solve the rate equations'
  end if
  call WarningMessage(0,.true.)
end if

if (nsec_aer > MAX_SEC) then
  nError   = IV_ERROR
  eRoutine = 'ReadMCcontrol'
  eMessage = 'Too many aerosol particle sections'
  write(eAction,'(a,i2)') 'Maximum number of sections: ',MAX_SEC
  go to 9999
end if

9999    return
201     nError   = RD_ERROR
        eRoutine = 'ReadMCcontrol'
        eMessage = 'Error reading multicomponent CONTROL namelist'
        go to 9999
end

subroutine read_mc_species(line, eqmparam)
!*******************************************************************************
!
! FUNCTION: Reads multi-component species definitions from project.imc
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_value                   get_c                  cupper
!
! REVISION HISTORY: 
!
! 08 JAN 2001 :Reset TAB to CHAR(9) (lost in change to F90) . - RIS
! Jan. 2005   :Read additional particle types (number concs. and surface
!              area - PK, AER
! 23 MAY 2005: Added release list pointers to account for moving equilibrium
!              species to end of species list (RIS)
! 21 JUL 2005: Change namex to CHAR(16) - PKK
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

character*(*) line           !line read from the IMC file
real      eqmparam(MAX_EQ,*) !to save the equilibrium parameters

! --- PARAMETERS

character*1 BLANK    ,TAB
parameter  (BLANK=' ',TAB=CHAR(9))

! --- LOCALS

character*16 namex
character*8 ctype
integer  nblank, nch, ncc, ios
logical  lerr, ldep, ldos
real     amb, tol_ode, vdep, scav, mwt

!====   Change TABS to Blanks for reading

nch = nblank(line)
call get_value(line,nch,TAB,BLANK,lerr)

!====   Read name

call get_c(line,nch,BLANK,namex,ncc,lerr)

if (lerr) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_species'
  eMessage = 'Error reading Multi-component species name'
  eInform  = 'Line='//TRIM(line)
  go to 9999
end if

!====   Read type - Fast,Slow,Equilibrium

call get_c(line,nch,BLANK,ctype,ncc,lerr)

if (lerr) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_species'
  eMessage = 'Error reading Multi-component species type'
  eInform  = 'Line='//TRIM(line)
  go to 9999
end if

!====   Read rest of the data

amb = 0.
tol_ode = 0.
vdep = 0.
scav = 0.
mwt  = 0.
read(line,*,iostat=ios) amb,tol_ode,vdep,scav,mwt
ldep = .false.
ldos = .false.
if (tol_ode .le. 0.) then
  nError   = IV_ERROR
  eRoutine = 'read_mc_species'
  eMessage = 'Absolute tolerance should be a positive, non-zero value'
  eInform  = 'Species='//TRIM(namex)
  write(eAction,'(A10,1p,e12.4)')'Tolerance=',tol_ode
  go to 9999
end if

!====   Set species structure

if(ctype(1:1) == 'A')then

  nambient = nambient + 1
  if (nambient >= MAX_AMB) then
    nError   = RD_ERROR
    eRoutine = 'read_mc_species'
    eMessage = 'Too many multi-component ambient species'
    write(eInform,'(a,i7)') 'Maximum number is ',MAX_AMB
    go to 9999
  end if

  ambient(nambient)%class = ID_SPECIES_AMBIENT
  ambient(nambient)%name  = namex
  ambient(nambient)%amb   = amb

else if(ctype(1:1) == 'E')then

  nspectot     = nspectot + 1
  nequilibrium = nequilibrium + 1
  if (nequilibrium >= MAX_EQ) then
    nError   = RD_ERROR
    eRoutine = 'read_mc_species'
    eMessage = 'Too many multi-component equilibrium species'
    write(eInform,'(a,i7)') 'Maximum number is ',MAX_EQ
    go to 9999
  end if

  if (nspectot > MAX_MC) then
    nError   = RD_ERROR
    eRoutine = 'read_mc_species'
    eMessage = 'Too many multi-component reacting species'
    write(eInform,'(a,i7)') 'Maximum number is ',MAX_MC
    go to 9999
  end if
  i_rel_list(nspectot) = -1

  equilibrium(nequilibrium)%class = ID_SPECIES_EQUILIBRIUM
  equilibrium(nequilibrium)%name  = namex
  equilibrium(nequilibrium)%star  = 0       !zero out star
  equilibrium(nequilibrium)%amb   = amb
  equilibrium(nequilibrium)%tol   = tol_ode
  eqmparam(nequilibrium,1)        = vdep
  eqmparam(nequilibrium,2)        = scav
  eqmparam(nequilibrium,3)        = mwt
  eqmparam(nequilibrium,4)        = 0.      !zero out nitrogen balance
  eqmparam(nequilibrium,5)        = 0.      !zero out volume limit

else

  nspecies = nspecies + 1
  nspectot = nspectot + 1
  if (nspectot > MAX_MC) then
    nError   = RD_ERROR
    eRoutine = 'read_mc_species'
    eMessage = 'Too many multi-component reacting species'
    write(eInform,'(a,i7)') 'Maximum number is ',MAX_MC
    go to 9999
  end if

  i_rel_list(nspectot) = nspecies

  species(nspecies)%name = namex

  species(nspecies)%nameemit = ' '
  species(nspecies)%density = 0.
  species(nspecies)%emis_split = 0.
  species(nspecies)%pmode = 0

  call cupper(ctype)
  if(ctype(1:1) == 'S') then
    species(nspecies)%class = ID_SPECIES_SLOW
  else if(ctype(1:1) == 'F')then
    species(nspecies)%class = ID_SPECIES_FAST
  else if(ctype(1:1) == 'P')then
    species(nspecies)%class = ID_SPECIES_PARTICLE
  else if(ctype(1:1) == 'N')then
    species(nspecies)%class = ID_SPECIES_NUMBER
  else if(ctype(1:1) == 'X')then
    species(nspecies)%class = ID_SPECIES_SURFACE
  else
    nError   = RD_ERROR
    eRoutine = 'read_mc_species'
    eMessage = 'Invalid species type'
    eInform  = 'Type ='//TRIM(ctype)
    go to 9999
  end if

  if (ldos) then
    species(nspecies)%dos = 1
  else
    species(nspecies)%dos = 0
  end if

  if (ldep) then
    species(nspecies)%dep = 1
  else
    species(nspecies)%dep = 0
  end if

  species(nspecies)%amb  = amb
  species(nspecies)%tol  = tol_ode
  species(nspecies)%vdep = vdep
  species(nspecies)%star = 0    !zero out star

  sparam(nspecies)%scav  = scav
  sparam(nspecies)%mwt   = mwt
  sparam(nspecies)%nit   = 0.   !zero out nitrogen balance
  sparam(nspecies)%lim   = 0.   !zero out volume limit
  sparam(nspecies)%param = 0.

end if

if (nspectot+nambient >= MAX_MC) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_species'
  eMessage = 'Too many multi-component species'
  write(eInform,'(a,i7)') 'Maximum number is ',MAX_MC
  go to 9999
end if

9999    continue

return
end

subroutine read_mc_bal(line, eqmparam)
!*******************************************************************************
!
! FUNCTION: Reads multi-component species that must be balanced
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_value                   get_c
!
! REVISION HISTORY: 
!
! 08 JAN 2001 :Reset TAB to CHAR(9) (lost in change to F90) . - RIS
! 21 JUL 2005: Change namex to CHAR(16) - PKK
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

character*(*) line           !line read from the IMC file
real      eqmparam(MAX_EQ,*) !to save the equilibrium parameters

! --- PARAMETERS

character*1 BLANK    ,TAB
parameter  (BLANK=' ',TAB=CHAR(9))

! --- LOCALS

character*16 namex
integer ios, nblank, nch, ncc, i
logical   lfound, lerr
real      nit

!====   Change TABS to Blanks for reading

nch = nblank(line)
call get_value(line,nch,TAB,BLANK,lerr)

!====   Read name

call get_c(line,nch,BLANK,namex,ncc,lerr)

if (lerr) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_bal'
  eMessage = 'Error reading Multi-component species name'
  eInform  = 'Line='//TRIM(line)
  go to 9999
end if

!====   Read number of molecules in species

nit = 0.
read(line,*,iostat=ios) nit

!====   Save in species parameter structure

lfound = .false.
do i = 1, nspecies
  if(namex == TRIM(species(i)%name))then
    sparam(i)%nit  = nit
    lfound = .true.
  end if
end do
do i = 1, nequilibrium
  if(namex == TRIM(equilibrium(i)%name))then
    eqmparam(i,4)  = nit
    lfound = .true.
  end if
end do

if (.not. lfound) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_bal'
  eMessage = 'Species for balance not in species list'
  write(eInform,'(a,a)') 'Species name :',TRIM(namex)
  go to 9999
end if

9999    continue

return
end

subroutine read_mc_lim(line, eqmparam)
!*******************************************************************************
!
! FUNCTION: Reads multi-component species that must be limited
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_value                   get_c
!
! REVISION HISTORY: 
!
! 08 JAN 2001 :Reset TAB to CHAR(9) (lost in change to F90) . - RIS
! 21 JUL 2005: Change namex to CHAR(16) - PKK
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc

implicit none

! --- ARGUMENTS

character*(*) line           !line read from the IMC file
real      eqmparam(MAX_EQ,*) !to save the equilibrium parameters

! --- PARAMETERS

character*1 BLANK    ,TAB
parameter  (BLANK=' ',TAB=CHAR(9))

! --- LOCALS

character*16 namex
integer  nblank, nch, ncc, i
logical   lfound, lerr

!====   Change TABS to Blanks for reading

nch = nblank(line)
call get_value(line,nch,TAB,BLANK,lerr)

!====   Read name

call get_c(line,nch,BLANK,namex,ncc,lerr)

if (lerr) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_lim'
  eMessage = 'Error reading Multi-component species name'
  eInform  = 'Line='//TRIM(line)
  go to 9999
end if

!====   Save in species parameter structure

lfound = .false.
do i = 1, nspecies
  if(namex == TRIM(species(i)%name))then
    sparam(i)%lim  = 1.0
    lfound = .true.
  end if
end do
do i = 1, nequilibrium
  if(namex == TRIM(equilibrium(i)%name))then
    eqmparam(i,5)  = 0.
    write(lun_log,*) 'Equilibrium species may not limit reaction volume: ',&
                      TRIM(equilibrium(i)%name)
    write(lun_log,*) 'Species will not determine reaction volume'
    lfound = .true.
  end if
end do

if (.not. lfound) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_lim'
  eMessage = 'Species for limit not in species list'
  write(eInform,'(a,a)') 'Species name :',TRIM(namex)
  go to 9999
end if

9999    continue

return
end

subroutine read_mc_eqn(line)
!*******************************************************************************
!
! FUNCTION: Reads multi-component equation definitions from project.imc
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_value                   get_i             get_mc_spec
!                   get_c       set_reaction_type        SetReactionClass
!               SetLinear                
!
! REVISION HISTORY: 
!
! 08 JAN 2001 : Reset TAB to CHAR(9) (lost in change to F90) . - RIS
! 30 NOV 2004 : Remove interchange of reactants A and B as it was not
!               done properly - BC
! JUNE   2005 : Read additional reaction types - PK, AER
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
character*(*) line           !line read from the IMC file

! --- PARAMETERS

character*1 BLANK    ,TAB    ,SEMI
parameter  (BLANK=' ',TAB=CHAR(9),SEMI=';')
character*2 PLUS     ,EQLS
parameter  (PLUS='+ ',EQLS='->')

! --- LOCALS

integer nch, ncc, nblank, ieqls, isemi, itype
integer ios, i, nP, ik, is, iA, iB
real    fac

character*4  string
character*80 kstring,original

logical lerr, turb, repeat

!====   Check for end of stage

if (lstage) then
  if(line(1:1) == '-') then
    if (nstage > MAX_STAGES) then
      nError   = RD_ERROR
      eRoutine = 'read_mc_eqn'
      eMessage = 'Too many reaction stages'
      write(eInform,*)'Required no. of stages = ', MAX_STAGES
      go to 9998
    end if
    nreact_s(nstage) = nreacts
    nstage = nstage + 1
    return
  end if
end if

!====   Change TABS to Blanks for reading

nch = nblank(line)
call get_value(line,nch,TAB,BLANK,lerr)
original = TRIM(line)
if(nch > len(original))then
  original(len(original)-2:) = '...'
end if

!====   Locate special characters

ieqls = index(line,EQLS)
isemi = index(line,SEMI)
if (ieqls <= 0) go to 9999
if (isemi <= 0) go to 9999

!====   Strip off rate data

kstring      = line(isemi+1:)
line(isemi:) = BLANK

!====   Check for turbulent reaction

if(line(1:1) == 't' .or. line(1:1) == 'T') then
  turb = .true.
  is = 2
else
  turb = .false.
  is = 1
end if
line = line(is:)

!====   Read reaction ID

call get_i(line,nch,ik,lerr)
if (lerr) go to 9999

!====   Check ID for uniqueness

do i = 1,nreacts
  if(reaction(i)%ID == ik)then
    nError   = RD_ERROR
    eRoutine = 'read_mc_eqn'
    eMessage = 'Non unique Multi-component equation ID'
    eInform  = 'Line='//TRIM(original)
    go to 9998
  end if
end do

!====   Increment number of reactions

nreacts = nreacts + 1
if(nreacts > MAX_REACTIONS)then
  nError   = RD_ERROR
  eRoutine = 'read_mc_eqn'
  eMessage = 'Too many reactions'
  write(eInform,*)'Max = ', MAX_REACTIONS
  go to 9998
end if

reaction(nreacts)%ID = ik

! Initialize reaction structure
reaction(nreacts)%class = 0
reaction(nreacts)%iA    = 0
reaction(nreacts)%iB    = 0
reaction(nreacts)%nP    = 0
reaction(nreacts)%icorr = 0
reaction(nreacts)%ktype = 0
reaction(nreacts)%iP    = 0
reaction(nreacts)%k     = 0.0
reaction(nreacts)%fB    = 0.0
reaction(nreacts)%fP    = 0.0
reaction(nreacts)%kdata = 0.0

!====   Get first reactant

call get_mc_spec(line,nch,fac,reaction(nreacts)%iA)
if (nError /= NO_ERROR) go to 9998

!====   Get second reactant [optional]

call get_c(line,nch,BLANK,string,ncc,lerr)
if (lerr) go to 9999
if (string(1:2) == EQLS)then
  reaction(nreacts)%iB = 0
  reaction(nreacts)%fB = 0.
else if (string(1:1) == PLUS)then
  call get_mc_spec(line,nch,reaction(nreacts)%fB,reaction(nreacts)%iB)
  if (nError /= NO_ERROR) go to 9998
  call get_c(line,nch,BLANK,string,ncc,lerr)
  if (lerr .or. string(1:2) /= EQLS) go to 9999
else
  go to 9999
end if

!====   Get product names

nP = 0
do while (nch > 0)
  nP = nP + 1
  if(nP > MAX_PRODUCTS)then
    eAction  = 'Too many products.'
  go to 9999
  end if
  call get_mc_spec(line,nch,reaction(nreacts)%fP(nP), &
                             reaction(nreacts)%iP(nP))
  if (nError /= NO_ERROR) go to 9998
  if(nch > 0)then
    call get_c(line,nch,BLANK,string,ncc,lerr)
    if (lerr .or. string(1:2) /= PLUS) go to 9999
  end if
end do
reaction(nreacts)%nP = nP

!====   Get Rate coefficient

nch = nblank(kstring)
call get_i(kstring,nch,itype,lerr)

if (lerr) go to 9999
if(itype == ID_K_RAD)then
  reaction(nreacts)%kdata(1) = 0.0
else if(itype == ID_K_CONST)then
  read(kstring,*,iostat=ios) reaction(nreacts)%kdata(1)
else if(itype == ID_K_TEMP)then
  reaction(nreacts)%kdata(3) = 0.  !for older imc files (now T-n)
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,3)
else if(itype == ID_K_EQM)then
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,3)
  if(reaction(nreacts)%kdata(3) <= 0.)then
    nError   = RD_ERROR
    eRoutine = 'read_mc_eqn'
    eMessage = '3rd coefficent for reaction type ID_K_EQM must be > 0'
    eInform  = 'Line='//TRIM(original)
    go to 9998
  end if
  if(INT(reaction(nreacts)%kdata(3)) > nreacts-1)then
    nError   = RD_ERROR
    eRoutine = 'read_mc_eqn'
    eMessage = '3rd coefficent for reaction type ID_K_EQM must be the ID '// &
               'of a previous reaction (preferably immediately previous)'
    eInform  = 'Line='//TRIM(original)
    go to 9998
  end if
else if(itype == ID_K_PRES)then
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,4)
else if(itype == ID_K_PRES2)then
  read(kstring,*,iostat=ios) reaction(nreacts)%kdata(1)
else if(itype == ID_K_H2O .or. itype == ID_K_H2OB)then
  reaction(nreacts)%kdata(3) = 0.
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,3)
else if(itype == ID_K_M  .or. itype == ID_K_O2 .or. &
        itype == ID_K_N2 .or. itype == ID_K_CH4)then
  reaction(nreacts)%kdata(3) = 0.
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,3)
else if(itype == ID_K_LWC)then
  read(kstring,*,iostat=ios) reaction(nreacts)%kdata(1)

else if(itype == ID_K_FALLOFF1 .or. itype == ID_K_FALLOFF2)then
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,4)
else if(itype == ID_K_FALLOFF3)then
  read(kstring,*,iostat=ios) (reaction(nreacts)%kdata(i),i=1,6)

else
  go to 9999
end if

!====   Set Reaction classes

IsLinear(nreacts) = .false.

call set_reaction_type(itype)

reaction(nreacts)%ktype = itype
reaction(nreacts)%class = 0
call SetReactionClass(reaction(nreacts)%class,reaction(nreacts)%iA)
if(reaction(nreacts)%iB == 0)then
  call SetLinear(nreacts,reaction(nreacts)%class)
else
  reaction(nreacts)%fB = reaction(nreacts)%fB/fac
  call SetReactionClass(reaction(nreacts)%class,reaction(nreacts)%iB)
end if
do i = 1,nP
  reaction(nreacts)%fP(i) = reaction(nreacts)%fP(i)/fac
  call SetReactionClass(reaction(nreacts)%class,reaction(nreacts)%iP(i))
end do

!====   Set turbulent correlation ID for reaction

if ( turb .and. .not. IsLinear(nreacts)) then
  iA = reaction(nreacts)%iA
  iB = reaction(nreacts)%iB
  repeat = .false.
  do i = 1, nreacts-1
    if(reaction(i)%icorr /= 0)then
      if( (iA == reaction(i)%iA .and. &
                iB == reaction(i)%iB )  ) then
        repeat = .true.
        reaction(nreacts)%icorr = reaction(i)%icorr
      end if
    end if
  end do
  if (.not. repeat) then
    ncorrt = ncorrt + 1
    if (ncorrt > MAX_NCORR) then
      nError   = RD_ERROR
      eRoutine = 'read_mc_eqn'
      eMessage = 'Too many turbulent reactions'
      write(eInform,*)'Max = ', MAX_NCORR
      go to 9998
    end if
    reaction(nreacts)%icorr = ncorrt
    indx_corr(ncorrt) = nreacts
  end if
else
  reaction(nreacts)%icorr = 0
end if


9998    if(nError /= NO_ERROR)then
          nreacts = max0(0,nreacts-1)
        end if
return

9999    continue
nError   = RD_ERROR
eRoutine = 'read_mc_eqn'
eMessage = 'Error reading Multi-component equation'
eInform  = 'Line='//TRIM(original)
go to 9998

end

subroutine read_mc_table(line)
!*******************************************************************************
!
! FUNCTION:   Reads multi-component radiation dependent rate table 
!             (zenith vs K) from project.imc
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_value                   get_i                   get_r
!
! REVISION HISTORY: 
!
! 08 JAN 2001 :Reset TAB to CHAR(9) (lost in change to F90) . - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

character*(*) line    !line read from the IMC file

! --- PARAMETERS

character*1 BLANK    ,TAB
parameter  (BLANK=' ',TAB=CHAR(9))

! --- LOCALS

integer nblank, nch, ik, i, icol
real      ang
logical   lerr

!====   Change TABS to Blanks for reading

eInform  = 'Line='//TRIM(line)

nch = nblank(line)
call get_value(line,nch,TAB,BLANK,lerr)

!====   Read reaction ID (0->zenith data)

call get_i(line,nch,ik,lerr)
if (lerr) then
  nError   = RD_ERROR
  eRoutine = 'read_mc_table'
  eMessage = 'Error reading Multi-component table'
  go to 9999
end if

!====   Read zenith data

if(ik == 0)then

!====== Read data

100       call get_r(line,nch,ang,lerr)
  if(lerr)then
    nError   = RD_ERROR
    eRoutine = 'read_mc_table'
    eMessage = 'Error reading Multi-component table'
    go to 9999
  end if

!====== Save data

  nzenith = nzenith + 1
  zenith(nzenith) = ang

!====== Get next data value if any

  if(nch > 0)go to 100

!====   Read rate data

else

!====== Find index in list

  icol = 0
  do i = 1,nkrad
    if(indxkr(i) == ik)icol = i
  end do

!====== New index - add to list

  if(icol == 0)then
    nkrad = nkrad + 1
    indxkr(nkrad) = ik
    nreadkr(nkrad)= 0
    icol = nkrad
  end if

!====== Set number of values previously read

  i = nreadkr(icol)

!====== Read data

200       call get_r(line,nch,ang,lerr)
  if(lerr)then
    nError   = RD_ERROR
    eRoutine = 'read_mc_table'
    eMessage = 'Error reading Multi-component table'
    go to 9999
  end if

!====== Save data
  i = i + 1
  ktable(i,icol) = ang

!====== Get next data value if any

  if(nch > 0)go to 200

!====== Done with line - save number of values read

  nreadkr(icol) = i
  if(i > nzenith)then
    nError   = RD_ERROR
    eRoutine = 'read_mc_table'
    eMessage = 'table data out of order'
    go to 9999
  end if

end if

!====   Done

eInform = ' '
9999    continue

return
end

subroutine SetReactionClass(class,ispec)
!*******************************************************************************
!
! FUNCTION:  Set the reaction class
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 SetFast                 SetSlow          SetEquilibrium
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use multcomp_inc

!====   Set reaction class from species class

implicit none

! --- ARGUMENTS
 
integer class  !species class
integer ispec  !species number


if(ispec <= nspecies)then
  if(species(ispec)%class == ID_SPECIES_FAST)then
    call SetFast(class)
  else if(species(ispec)%class == ID_SPECIES_SLOW)then
    call SetSlow(class)
  end if
else if(ispec <= nspectot)then
  call SetEquilibrium(class)
end if

return
end

subroutine get_mc_spec(line,nch,fac,ispec)
!*******************************************************************************
!
! FUNCTION: Read next species and stoichiometric coefficient from reaction line
!           and return pointer to species in species list
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   get_c
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
character*(*) line  !line from IMC file
integer ispec       !species pointer
real    fac         !stochiometric factor
integer nch         !number of characters in the line

! --- PARAMETERS

character*1 LBRK    ,RBRK    ,LPRN    ,RPRN
parameter  (LBRK='[',RBRK=']',LPRN='(',RPRN=')')
character*1 BLANK
parameter  (BLANK=' ')

! --- LOCALS

integer ncc, nblank, ios, i, ioff

character*32 string

logical lerr

!====   Read coefficient if present

nch = nblank(line)
call get_c(line,nch,BLANK,string,ncc,lerr)
if (string(1:1) == LPRN) then
  read(string(2:ncc-1),*,iostat=ios) fac
  call get_c(line,nch,BLANK,string,ncc,lerr)
else
  fac = 1.0
end if

!====   Read species

if (string(1:1) == LBRK) then
  ispec = 0
  if (string(2:2) == RBRK)then
    nError   = RD_ERROR
    eRoutine = 'get_mc_spec'
    eMessage = 'Error reading Multi-component input file'
    eInform  = 'Invalid species = '//TRIM(string)
    go to 9999
  end if
else
  nError   = RD_ERROR
  eRoutine = 'get_mc_spec'
  eMessage = 'Error reading Multi-component input file'
  eInform  = 'Line = '//TRIM(string)//' '//TRIM(line)
  go to 9999
end if

!====   Find species in species list

!       Reacting species

ioff = 0
do i = 1,nspecies
  if (string(2:ncc-1) == species(i)%name) ispec = i + ioff
end do

!       Equilibrium species

ioff = ioff + nspecies
do i = 1,nequilibrium
  if (string(2:ncc-1) == equilibrium(i)%name) ispec = i + ioff
end do

!       Ambient species

ioff = ioff + nequilibrium
do i = 1,nambient
  if (string(2:ncc-1) == ambient(i)%name) ispec = i + ioff
end do

if ( ispec == 0) then
  nError   = RD_ERROR
  eRoutine = 'get_mc_spec'
  eMessage = 'Error reading Multi-component input file'
  eInform  = 'Invalid species = '//TRIM(string)
  go to 9999
end if

9999    continue
return

end

subroutine set_mc_matl
!*******************************************************************************
!
! FUNCTION:  Set species, material, typeID structures
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 IsMulti                 GetNdep                 GetNdos
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- LOCALS

integer i, imat
integer ndos, nsrf
integer nmc
integer nmc2(2)

logical ldos,lsrf,ldost,lsrft,IsMulti
integer GetNdep,GetNdos,nvx

!====   Check species for surface output and reset pointer

ldost = .false.
lsrft = .false.
ndos = 0
nsrf = 0
do i = 1,nspecies
  ldos = species(i)%dos > 0
  ldost = ldost .or. ldos
  if(ldos)then
    ndos = ndos + 1
    species(i)%dos = ndos
  end if
  lsrf = species(i)%dep > 0
  lsrft = lsrft .or. lsrf
  if(lsrf)then
    nsrf = nsrf + 1
    species(i)%dep = nsrf
  end if
end do

!====   Check each material for multi-component

nmc2(2) = nspectot + ncorrt         !masses, conc correl
nmc2(1) = ncorrm   + nspectot + 4   !overlap conc, end of chemistry conc, 
                                    !vol-1, stage, temp and press
nmc = nmc2(1)*65536 + nmc2(2)
if (nmc2(1) + nmc2(2) + nambient > MAX_MC2) then
  nError = SZ_ERROR
  eRoutine = 'set_mc_matl'
  eMessage = 'Too many multi-component variables'
  write(eAction,'(a,i5,a,i5)') 'Maximum number is ',MAX_MC2,&
                               ', Minimum required = ',(nmc2(1)+nmc2(2)+nambient)/2+1
  go to 9999
end if

do i = 1,ntypm
  if (IsMulti(material(i)%icls)) then
    material(i)%nmc     = nmc2(1)+nmc2(2)
    material(i)%ldos_mc = ldost
    material(i)%lsrf_mc = lsrft
    if(material(i)%lsrf_mc)then
      nvx = GetNdep(material(i)%icls)*nsrf
      material(i)%ioffs_mc = ntyps
      ntyps = ntyps + nvx
    end if
    if(material(i)%ldos_mc)then
      nvx = GetNdos(material(i)%icls)*ndos
      material(i)%ioffd_mc = ntypd
      ntypd = ntypd + nvx
    end if
    if(ntyps+ntypd > MAXSTYP)then
      nError   = SZ_ERROR
      eRoutine = 'set_mc_matl'
      eMessage = 'Too many deposition/dose fields'
      write(eAction,'(a,i5)') 'Maximum number is ',MAXSTYP
      go to 9999
    end if
  end if
end do

!====   Check each puff type for multi-component

do i = 1,ntypp
  imat = typeID(i)%imat
  if (IsMulti(material(imat)%icls)) then
    typeID(i)%nmc   = nmc
    typeID(i)%ipmc  = typeID(i)%npaux + 1
    typeID(i)%npaux = typeID(i)%npaux + nmc2(1) + nmc2(2)
  else
    typeID(i)%nmc   = 0
    typeID(i)%ipmc  = 0
  end if
end do

9999    return
end

subroutine set_mc_corr
!*******************************************************************************
!
! FUNCTION:  Set the species that must be stepped using overlap concentrations
!            (if only involved in linear reactions, masses can be used)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 SetStar 
!
! REVISION HISTORY: 
!
! 08 JAN 2001 : Fixed bugs initializing IsStar) . - RIS
!*******************************************************************************
 
! --- MODULES
 
use multcomp_inc

implicit none

! --- LOCALS

integer i, j, nP

logical NewSpecies

NewSpecies = .false.

!====   Loop over species

do i = 1, nspecies
  IsStar(i) = species(i)%star /= 0
end do
do i = nspecies+1, nspectot
  IsStar(i) = equilibrium(i-nspecies)%star /= 0
end do
do i = nspectot+1, nspectot+nambient
  IsStar(i) = .false.
end do

!====   Loop over reactions

do i = 1, nreacts

!====== Quadratic reactions only

  if(.not.IsLinear(i))then
!========       Reactant A

    call SetStar(reaction(i)%iA,NewSpecies)

!========       Reactant B

    call SetStar(reaction(i)%iB,NewSpecies)

!========       Products

    nP = reaction(i)%nP
    do j = 1,nP
      call SetStar(reaction(i)%iP(j),NewSpecies)
    end do

  end if

end do

!====   Check linear equations for influence

do while (NewSpecies)
  NewSpecies = .false.
  do i = 1,nreacts
    if(IsLinear(i))then
      nP = reaction(i)%nP
      do j = 1,nP
        if(IsStar(reaction(i)%iP(j)))then
          call SetStar(reaction(i)%iA,NewSpecies)
        end if
      end do
    end if
  end do
end do

do i = 1, nequilibrium
  call SetStar(i+nspecies,NewSpecies)
end do

return
end

subroutine c_init_mc(pm,fac,vel)
!*******************************************************************************
!
! FUNCTION: Initialize multicomponents for continuous release puff
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
 
use common_puf
use multcomp_inc
use common_met

implicit none

! --- ARGUMENTS
 
real fac              !2*pi*sigy*sigz (or *sigx*sigy depending on source)
real vel              !puff release velocity
type ( puff_mc  ) pm  !multicomponent species structure

! --- LOCALS

integer i, j

!----species end of chem concentrations (only used for output in puff file)

do i = 1, nspectot
  j = nspectot + ncorrm + ncorrt
  pm%mc(i+j) = 0.  !just initialize to zero
end do

!------initialize volume

j = 2*nspectot + ncorrm + ncorrt + 1
pm%mc(j) = fac*vel

!------initialize chemical stage

j = j + 1
pm%mc(j) = 1

!-------initialize temperature and pressure

j = j + 1
pm%mc(j) = tab

j = j + 1
pm%mc(j) = pb

return
end

subroutine set_mc_rel(p,pm)
!*******************************************************************************
!
! FUNCTION:  Initialize multicomponent release
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
! JAN.   2005 : Read releases of particle number concs and surface
!               areas - PK, AER
! 23 MAY 2005: Added release list pointers to account for moving equilibrium
!              species to end of species list (RIS)
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use multcomp_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str  ) p
type ( puff_mc  ) pm  !multicomponent species structure

! --- LOCALS

integer i, j, ioff, nmc, nzero
integer nmc2(2)

nmc   = typeID(p%ityp)%nmc
nmc2(1) = nmc/65536
nmc2(2) = nmc - 65536*nmc2(1)

do i= 1,nmc2(1)+nmc2(2)
  pm%mc(i) = 0.
end do

!-----species masses

do i = 1,nspectot
  j = i_rel_list(i)
  if (j > nspecies) cycle
  if (species(j)%class == ID_SPECIES_PARTICLE) then
! emission units always assumed to be g/s for particles
    pm%mc(j) = rel_mc(i)*1.e+6  ! convert to ug/s for aerosol particles

! emission units always assumed to be #/s for particle number and
! m2/s for particle surface area
  else if (species(j)%class == ID_SPECIES_NUMBER .or. &
           species(j)%class == ID_SPECIES_SURFACE) then
    pm%mc(j) = rel_mc(i)

  else   ! gases
    pm%mc(j) = rel_mc(i)*em_conv
  end if                        
end do

if (ie_conv >= G_PPM) then
  do i = 1,nspecies
    if (species(i)%class /= ID_SPECIES_PARTICLE .and. &
        species(i)%class /= ID_SPECIES_NUMBER .and. &
        species(i)%class /= ID_SPECIES_SURFACE) then
      pm%mc(i) = pm%mc(i)/sparam(i)%mwt
    end if
  end do
end if

!-----puff chemical stage
ioff = 2*nspectot + ncorrm + ncorrt + 2
pm%mc(ioff) = 1

!-------save temperature and pressure

ioff = ioff + 1
pm%mc(ioff) = tab

ioff = ioff + 1
pm%mc(ioff) = pb

return
end

subroutine c_set_mc(pm,dt)
!*******************************************************************************
!
! FUNCTION:  Initialize multicomponents for continuous release puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
! 04/16/2001 : Corrected pointers for turbulent correlations (BC)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS

real dt               !timestep (sec)
type ( puff_mc  ) pm  !multicomponent species structure

! --- LOCALS

integer i, j

do i=1,nspectot
  pm%mc(i) = pm%mc(i)*dt
end do

if(ncorrt > 0)then
  i = nspectot
  do j=1,ncorrt
    i = i + 1
    pm%mc(i) = pm%mc(i)*dt
  end do
end if

j = 2*nspectot + ncorrm + ncorrt + 1
if (pm%mc(j) /= 0.) pm%mc(j) = 1./(pm%mc(j)*dt)

return
end

subroutine set_mc_tcorr(pm)
!*******************************************************************************
!
! FUNCTION: Initialize turbulent correlations for multicomponent release
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          set_ps_from_mc
!
! REVISION HISTORY: 
!
! 07/24/2001 : Corrected pointers for turbulent correlations (BC)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
type ( puff_mc  ) pm !multicomponent species structure

! --- LOCALS

integer i, ic, ir, iA, iB
real cA, cB

!-----added to set tcorr for eqm species

do i = 1, MAX_NCORR
  corr(i) = 0.
end do

if (ncorrt > 0) then

  call set_ps_from_mc(pm,0.,0.)
  do i = 1, ncorrt
    corr(i) = 0.
  end do

!-----added to set tcorr for eqm species

  do i = 1, ncorrt
    ic = nspectot  + i !+ ncorrm
    ir = indx_corr(i)
    iA = reaction(ir)%iA
    iB = reaction(ir)%iB
    cA   = ps(iA)%c - ps(iA)%a
    cB   = ps(iB)%c - ps(iB)%a
    pm%mc(ic) = 0.5*(ps(iA)%m*cB + cA*ps(iB)%m)
  end do

end if

return
end

subroutine init_mcp
!*******************************************************************************
!
! FUNCTION: Initialize multi-component
!           This routine is called in start via read_mc 
!           and directly from restart
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            WarningMessage    init_equilibrium
!            set_vdep_aer      init_wash_aerosol_chem
!            init_aero         init_aqueous  SetStar        init_staged_chem
!            set_staged_species
!
! REVISION HISTORY: 
! JUNE 2005 : Changed initialization of aerosol & aqueous chemistry
!             - PK, AER
! JULY 2005 : Changed species names from C*8 to C*16 for CMAQ-MADRID-APT
!             - PK, AER
! AUG  2010 : Update to changes on Aug 2007 by PK,AER. Initialize ioh for parallel version) -BC(SAGE-MGT)
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc

implicit none

! --- LOCALS

character*16 nameA, nameB, nametmp(MAX_MC)

integer i, ioff, j, k, jj, nn, nblank, isp
integer iA, iB, iP, n
integer ispec_stage(MAX_MC)

logical lflag, lneed, IsMCParticle


!====   expand species array to include the ambient

do i = nspectot+1, nspectot+nambient
  species(i)%class = ID_SPECIES_AMBIENT
  species(i)%star = 0
  species(i)%dos = 0
  species(i)%dep = 0
  species(i)%amb = ambient(i-nspectot)%amb
  species(i)%tol = 0.
  species(i)%vdep = 0.
  species(i)%name = ambient(i-nspectot)%name
end do

!====   if a restart, set IsStar and IsLinear array

if (restart) then

  do i = 1, nspecies
    if(species(i)%star /= 0) then
      IsStar(i) = .true.
    else
      IsStar(i) = .false.
    end if
  end do      

  do i = 1, nequilibrium
    IsStar(i+nspecies) = .true.
  end do      

  do i = 1, nreacts
    if(reaction(i)%iB == 0)then
      IsLinear(i) = btest(reaction(i)%class,ID_REACT_LINEAR)
    else
      IsLinear(i) = .false.
    end if
  end do
  
end if
  
!====   write solution method to log file

if (isolve == ID_YNB) then
  write(lun_log,*) 'Using Young & Boris method ' &
       //'to solve chemistry ODEs'
else if (isolve == ID_LSODE) then
  write(lun_log,*) 'Using LSODE to solve chemistry ODEs'
else if (isolve == ID_VODE) then
  write(lun_log,*) 'Using VODE to solve chemistry ODEs'
end if

!====   Set flag used to step total and ambient instead of perturbations
lstep_tot = .false.
if (lstep_tot) then
  write(lun_log,*) 'Using stepping of total concentration instead of perturbations'
end if

!====   Set species stages

do i = 1, nstage
  nspec_s(i)  = 0
  neqm_s(i)   = 0
  nslow_s(i)  = 0
  nfast_s(i)  = 0
end do

if (.not. lstage) then

  istage = nstage
  nspec_s(nstage) = nspectot
  nreact_s(nstage) = nreacts
  do i = 1, nspectot
    indx_spec(i,nstage) = i
  end do
  nrxn_curr = 0
  do i = 1, nreacts
    if ( (reaction(i)%ktype /= ID_K_CONST)  &
          .or. (reaction(i)%kdata(1) /= 0.) ) then
       nrxn_curr = nrxn_curr + 1
       indx_rxns(nrxn_curr) = i
    end if
  end do

else

  do i = 1, nspectot
    ispec_stage(i) = 0
  end do
  do j = 1, nstage
    do i = 1, nreact_s(j)
      iA = reaction(i)%iA
      ispec_stage(iA) = max0(ispec_stage(iA),j)
      iB = reaction(i)%iB
      if (iB > 0) ispec_stage(iB) = max0(ispec_stage(iB),j)
      do k = 1, reaction(i)%nP
        iP = reaction(i)%iP(k)
        ispec_stage(iP) = max0(ispec_stage(iP),j)
      end do
    end do
    n = 0
    do i = 1, nspectot
      if(ispec_stage(i) /= 0) then
        n = n + 1
        nspec_s(j) = n
        indx_spec(n,j) = i
      end if
    end do
  end do

end if

!====   Set slow, fast, and equlibrium indices

call set_staged_species

!====   Check that correlation species are set fast or equilibrium

do k = 1, nreacts
  if (reaction(k)%icorr /= 0) then
    iA = reaction(k)%iA
    iB = reaction(k)%iB
    if (iB > nspecies) then
      j  = nblank(species(iB)%name)
      nError   = IV_ERROR
      eRoutine = 'init_mcp'
      if (iB > nspectot) then
        eMessage = 'Turbulent reactions may not '// &
                    'involve ambient species'
      else
        eMessage = 'Turbulent reactions may not '// &
                    'involve equilibrium species'
      end if
      if (iA > nspecies) then
        i  = nblank(species(iA)%name)
        write(eAction, '(a, i4)') 'The following reaction '// &
            'may not be turbulent(involves '//species(iA)%name(1:i)// &
            ' and '//species(iB)%name(1:j)//'):', reaction(k)%ID
      else
        write(eAction, '(a, i4)') 'The following reaction '// &
                                  'may not be turbulent(involves ' &
                    //species(iB)%name(1:j)//'):', reaction(k)%ID
      end if
      go to 9999
    end if
    if ( species(iA)%class == ID_SPECIES_SLOW .or. &
                       species(iB)%class == ID_SPECIES_SLOW ) then
      i = nblank(species(iA)%name)
      nameA = species(iA)%name
      j = nblank(species(iB)%name)
      nameB = species(iB)%name
      nError   = IV_ERROR
      eRoutine = 'init_mcp'
      eMessage = 'Turbulent reactants must be modeled as fast'
      eAction  = 'Both the following species must be fast: ' // &
                     nameA(1:i) // ' and ' // nameB(1:j)
      go to 9999
    end if
  end if
end do

!====   Initialize H2O pointer

H2O = -999
ioff = 0
do i = 1,nspecies
  if(TRIM(species(i)%name) == 'H2O')H2O = i + ioff
end do
ioff = ioff + nspecies
do i = 1,nequilibrium
  if(TRIM(equilibrium(i)%name) == 'H2O')H2O = i + ioff
end do
ioff = ioff + nequilibrium
do i = 1,nambient
  if(TRIM(ambient(i)%name) == 'H2O')H2O = i + ioff
end do
if(H2O < 0)then
  do i = 1,nreacts
    if(reaction(i)%ktype == ID_K_H2O .or. &
       reaction(i)%ktype == ID_K_H2OB)then
      nError = IV_ERROR
      eRoutine = 'init_mcp'
      eMessage = 'Reaction needs H2O but H2O not in species list'
      write(eInform,*)'Reaction ',i
      go to 9999
    end if
  end do
end if

!====   Move ambient value in working structure

ioff = 0
do i = 1,nspecies
  ps(i+ioff)%equil = .false.
  if (.not. restart) ps(i+ioff)%a = species(i)%amb
end do
ioff = nspecies
do i = 1,nequilibrium
  ps(i+ioff)%equil = .true.
  ps(i+ioff)%a = equilibrium(i)%amb
end do
ioff = nspectot
do i = 1,nambient
  ps(i+ioff)%equil = .false.
  ps(i+ioff)%a = ambient(i)%amb
end do

!--- Initialize remaining ps values
ioff = nspectot + nambient
do i = ioff+1,MAX_MC
  ps(i)%equil  = .false.
  ps(i)%a      = 0.
  ps(i)%m      = 0.
  ps(i)%c      = 0.
  ps(i)%taudry = 0.
  ps(i)%tauwet = 0.
end do

if (amb_file /= TRIM(NOT_SET_C)) then
  lamb3d = .true.
else
  lamb3d = .false.
end if

!====   Initialize rate parameters

tk = NOT_SET_R
pk = NOT_SET_R
zk = NOT_SET_R
hk = NOT_SET_R
cldk = NOT_SET_R
lchng_rad = .false.
lflag_dark = .false.

!====   Set unit conversion flag

! -- rate constant conversion
ic_units = i_units/16
ik_units = i_units - 16*ic_units
if (ic_units == ik_units) then
  ik_conv = NO_CONVERSION
else if(ic_units == UNIT_PPM) then
  ik_conv = MOLECULE_PPM
else
  ik_conv = -MOLECULE_PPM
end if

! -- emission rate conversion
if (ie_units == ic_units) then
  ie_conv = NO_CONVERSION
else if(ie_units == UNIT_MOLECULE) then
  ie_conv = MOLECULE_PPM
else if(ie_units == UNIT_PPM) then
  ie_conv = -MOLECULE_PPM
else if(ie_units == UNIT_G) then
  if (ic_units == UNIT_PPM)      ie_conv = G_PPM
  if (ic_units == UNIT_MOLECULE) ie_conv = G_MOLECULE
end if

call set_emission_conv  !emissions released at STP

!====   Check that molecular weights are nonzero, if needed

if (ie_conv >= G_PPM) then
  do i = 1,nspecies
    if (species(i)%class /= ID_SPECIES_PARTICLE .and. &
        species(i)%class /= ID_SPECIES_NUMBER .and. &
        species(i)%class /= ID_SPECIES_SURFACE) then
      if (sparam(i)%mwt <= 0.) then
        nError = IV_ERROR
        eRoutine='init_mcp'
        eMessage='Nonphysical molecular weight'
        write(eAction,'(3a,f9.2)')'Species ',TRIM(species(i)%name), &
                           ' has a molecular weight of ',sparam(i)%mwt
        eInform = 'File = '//TRIM(file_imc)
        go to 9999
      end if
    end if
  end do
end if

!====   Warn if scavenging coefficient is set for particles

do i = 1,nspecies
  if (species(i)%class == ID_SPECIES_PARTICLE .or. &
      species(i)%class == ID_SPECIES_NUMBER .or. &
      species(i)%class == ID_SPECIES_SURFACE) then
    if (sparam(i)%scav /= 0.) then
      nError = WN_ERROR
      eRoutine='init_mcp'
      eMessage='Particle scavenging coefficients' &
                   // ' are calculated internally'
      write(eInform,*)'Coefficient will be ignored for species:' &
                            ,TRIM(species(i)%name)
      call WarningMessage(0,.true.)
      if (nError /= NO_ERROR) go to 9999
    end if
  end if
end do

!====   Initialize equilibrium calculation

call init_equilibrium

!====   Initialize aqueous and aerosol chemistry

! - still set default deposition and washout for aerosol particles
! - even if aerosol equilibrium is not turned on
if (.not. laerosol) then
  call set_vdep_aer
  call init_wash_aerosol_chem
end if

if (laerosol) then

  write(lun_log, &
        '("AEROSOL CALCULATIONS TURNED ON (MODULE 1.0)")')

  do i = 1, nspectot + nambient
    nametmp(i) = species(i)%name
  end do

  index_aero = 0
  aero_names = ' '
  call init_aero(nametmp,nspectot+nambient,lun_log,file_imc, &
                 index_aero,naero,naerp,aero_names)

  if (nError /= NO_ERROR) go to 9999

  do i = 1, naero
    j = index_aero(i)
    call SetStar(j,lflag)  !use concentrations to step
  end do

  do i = 1, naerp
    j = index_aero(i)
    if (species(j)%class /= ID_SPECIES_PARTICLE .and. &
        species(j)%class /= ID_SPECIES_NUMBER .and. &
        species(j)%class /= ID_SPECIES_SURFACE) then
      nError = IV_ERROR
      eRoutine='init_mcp'
      eMessage='Species should be specified as a particle'
      write(eInform,*)'Species :',species(j)%name
      go to 9999
    end if
  end do

  do isp = 1, nspecies
    if (species(isp)%class == ID_SPECIES_PARTICLE .or. &
        species(isp)%class == ID_SPECIES_NUMBER .or. &
        species(isp)%class == ID_SPECIES_SURFACE) then
      lflag = .false.
      do i = 1, naerp
        j = index_aero(i)
        if (j == isp) then
          lflag = .true.
          go to 1
        end if
      end do
1     if (.not. lflag) then
        nError   = WN_ERROR
        eRoutine='init_mcp'
        eMessage='Species should not be specified as a particle'
        eInform='Species :'//species(isp)%name
        eAction='Species will not undergo gas-phase reactions'
        call WarningMessage(0,.true.)
      end if
    end if
  end do

else    ! if (.not. laerosol) use default SCICHEM aerosol treatment

  naerp = 0
  do i = 1, nspecies
    if (IsMCParticle(i)) then
      naerp = naerp + 1
      index_aerp(naerp) = i
      index_sec(naerp)  = 1
    end if
  end do

end if

if (laqueous) then

  write(lun_log, &
         '("AQUEOUS CHEMISTRY TURNED ON (MODULE 1.0)")')

  do i = 1, nspectot + nambient
    nametmp(i) = species(i)%name
  end do

  index_aqueous = 0
  aqueous_names = ' '
  call init_aqueous(nametmp,nspectot+nambient,lun_log,file_imc, &
                    index_aqueous,naqchem,naqueous,aqueous_names)

  if (nError /= NO_ERROR) go to 9999

  do i = 1, naqueous
    j = index_aqueous(i)
    call SetStar(j,lflag)  !use concentrations to step
  end do

end if

!====   Check if there is a species balance requirement

do i = 1, nspectot
  if (sparam(i)%nit /= 0.) then
    lbalance = .true.
  end if
end do

if (lbalance) then
  write(lun_log,*) 'Conserving the following set of species:'
  do i = 1, nspectot
    if (sparam(i)%nit /= 0.) then
      write(lun_log, *) species(i)%name, sparam(i)%nit
    end if
  end do
end if

!====   Save species limit requirement to an index

nlim = 0
do i = 1, nspecies  ! - wire to be NO and NO2 for now
!  if (sparam(i)%lim /= 0.) then   
  if (TRIM(species(i)%name) .eq. 'NO' .or. &
      TRIM(species(i)%name) .eq. 'NO2' ) then
    nlim = nlim + 1
    index_lim(nlim) = i
  end if
end do

if (nlim > 0) write(lun_log,*) 'Limiting reaction volumes for:'
do i = 1, nlim
  j = index_lim(i)
  write(lun_log,*) species(j)%name
end do
      
! -- Special, for wiring to NO and NO2
if (nlim /= 2) then
  write(lun_log,*) '****************** WARNING **********************'
  write(lun_log,*) 'Unable to locate NO and/or NO2 in the species list'
end if

!====   Write unrequired species to log file

do i = 1, nspectot+nambient
  lneed = .false.
  do j = 1, nreacts
    iA = reaction(j)%iA
    iB = reaction(j)%iB
    if (i == iA .or. i == iB ) lneed = .true.
    do jj = 1, reaction(j)%nP
      iP = reaction(j)%iP(jj)
      if (i == iP) lneed = .true.
    end do
  end do
  if (laerosol) then
    do j = 1, naero
      if (i == index_aero(j)) lneed = .true.
    end do
  end if
  if (laqueous) then
    do j = 1, naqueous
      if (i == index_aqueous(j)) lneed = .true.
    end do
  end if
  if(.not. lneed) write(lun_log,*) 'Not needed in species list: ', &
                                       species(i)%name
end do

!====   Initialize staged chemistry

nfast = nfast_s(nstage)
nslow = nslow_s(nstage)
do i = 1, nfast
  indxf(i) = indxf_s(i,nstage)
end do
do i = 1, nslow
  indxs(i) = indxs_s(i,nstage)
end do

if (lstage .or. ldump_chem .or. lchem_split) then
  call init_staged_chem
  if (nError /= NO_ERROR) go to 9999
else

! -- Locate key species for o3lim in YNB

  do i = 1, NKEY_SPEC
    ikey_spec(i) = NOT_SET_I
  end do

  do i = 1,nspectot
    if(TRIM(species(i)%name) == 'NO')   ikey_spec(NO)   = i
    if(TRIM(species(i)%name) == 'NO2')  ikey_spec(NO2)  = i
    if(TRIM(species(i)%name) == 'O3')   ikey_spec(O3)   = i
    if(TRIM(species(i)%name) == 'OH') then
      ikey_spec(OH)   = i
      ioh = i
    end if
  end do

  if (nequilibrium > 0) then

    write(lun_log,*)'Equilibrium species solution'
    write(lun_log,*)'----------------------------'
    j = nstage
    do i = 1, neqm_s(j)
      nn = indx_eq_s(i,j)
      jj = nblank(equilibrium(nn)%name)
      if (itype_eq_s(nn,j) == 0) then
        write(lun_log,200)equilibrium(nn)%name(1:jj) &
                                       ,'fully coupled solution'
      else if (itype_eq_s(nn,j) == -1) then
        write(lun_log,200)equilibrium(nn)%name(1:jj),'solved at the end'
      else if (itype_eq_s(nn,j) == -2) then
        write(lun_log,200)equilibrium(nn)%name(1:jj),'solved directly'
      else
        k = nblank(equilibrium(itype_eq_s(nn,j))%name)
        write(lun_log,205)equilibrium(nn)%name(1:jj), &
               'eliminated in favor of ', &
               equilibrium(itype_eq_s(nn,j))%name(1:k)
      end if
    end do
  end if
end if
200 format(A,' ----> ',A)
205 format(A,' ----> ',2A)

9999    return
end

subroutine init_staged_chem
!*******************************************************************************
!
! FUNCTION: Initialize staged chemistry for multi-component runs
!           This routine is called in start via read_mc and directly from restart
!           This routine is also called for non-staged plume-in-grid runs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
! AUG  2010 : Update to changes on Aug 2007 by PK,AER. Initialize ioh for parallel version) -BC(SAGE-MGT)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc

implicit none

! --- LOCALS

integer i, j, k, jj, nn, nblank, iA, iB, nP, iVOC, id
character*3 plus
logical lno2, loh

! -- Set number of VOCs

if (nvoc < 0 .and. restart) then
    nError   = IV_ERROR
    eRoutine = 'init_staged_chem'
    eMessage = 'This project was created with version 1.1.103'
    eAction  = 'There was an error writing VOC names and '// &
                             'it may not be re-started'
    go to 9999
end if

if (nvoc == 0) then
  nreact_voc = 0
  do i = 1, MAX_VOC
    if (voc_names(i) /= NOT_SET_C) then
      nvoc = nvoc + 1
    end if
  end do
  if (nvoc == 0) then
    nError = IV_ERROR
    eRoutine = 'init_staged_chem'
    eMessage = 'VOCs not defined'
    go to 9999
  end if

  do i = 1, nvoc
    do j = i+1, nvoc
      if (voc_names(i) == voc_names(j)) then
        nError = IV_ERROR
        eRoutine = 'init_staged_chem'
        eMessage = 'VOC repeated in VOC_NAMES'
        write(eInform,*) 'Do not repeat: ',TRIM(voc_names(i))
        go to 9999
      end if
    end do
  end do
end if

! -- Locate key species

do i = 1, NKEY_SPEC
  ikey_spec(i) = NOT_SET_I
end do

do i = 1,nspectot
  if(TRIM(species(i)%name) == 'NO')   ikey_spec(NO)   = i
  if(TRIM(species(i)%name) == 'NO2')  ikey_spec(NO2)  = i
  if(TRIM(species(i)%name) == 'O3')   ikey_spec(O3)   = i
  if(TRIM(species(i)%name) == 'OH') then
    ikey_spec(OH)   = i
    ioh = i
  end if
  if(TRIM(species(i)%name) == 'HO') then
    ikey_spec(OH)   = i
    ioh = i
  end if
  if(TRIM(species(i)%name) == 'HO2')  ikey_spec(HO2)  = i
  if(TRIM(species(i)%name) == 'NO3')  ikey_spec(NO3)  = i
  if(TRIM(species(i)%name) == 'HNO3') ikey_spec(HNO3) = i
  if(TRIM(species(i)%name) == 'N2O5') ikey_spec(N2O5) = i
  if(TRIM(species(i)%name) == 'O1D')  ikey_spec(O1D)  = i
  if(TRIM(species(i)%name) == 'C2O3') ikey_spec(C2O3) = i
  do j = 1, nvoc
    if(TRIM(species(i)%name) == TRIM(voc_names(j))) &
                     indx_voc(j) = i
  end do
end do

do i = 1, NKEY_SPEC
  if ( ikey_spec(i) == NOT_SET_I ) then
    nError   = IV_ERROR
    eRoutine = 'init_staged_chem'
    eMessage = 'Necessary chemical species for staged chemistry' &
                     // ' not found in imc file'
    if (i == NO) then
      eAction = 'Missing species: NO'
    else if (i == NO2) then
      eAction = 'Missing species: NO2'
    else if (i == O3) then
      eAction = 'Missing species: O3'
    else if (i == OH) then
      eAction = 'Missing species: OH'
    else if (i == HO2) then
      eAction = 'Missing species: HO2'
    else if (i == NO3) then
      eAction = 'Missing species: NO3'
    else if (i == HNO3) then
      eAction = 'Missing species: HNO3'
    else if (i == N2O5) then
      eAction = 'Missing species: N2O5'
    else if (i == O1D) then
      eAction = 'Missing species: O1D'
    else if (i == C2O3) then
      eAction = 'Missing species: C2O3'
    end if
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if
end do

if (lstage) then

  if (species(ikey_spec(NO))%class == ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='NO must not be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(NO2))%class == ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='NO2 must not be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(O3))%class == ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='O3 must not be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(HNO3))%class == ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
      eMessage ='HNO3 must not be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(OH))%class /= ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='OH must be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(HO2))%class /= ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='HO2 must be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(NO3))%class /= ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='NO3 must be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(O1D))%class /= ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='O1D must be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

  if (species(ikey_spec(C2O3))%class /= ID_SPECIES_EQUILIBRIUM) then
    nError   = IV_ERROR
    eRoutine ='init_staged_chem'
    eMessage ='C2O3 must be set as equilibrium with staged chemistry'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if

end if

! -- Locate key reactions

do i = 1, NKEY_RXNS
  ikey_rxn(i) = NOT_SET_I
end do

do i = 1, nreacts
  iA = reaction(i)%iA
  iB = reaction(i)%iB
  nP = reaction(i)%nP
! -- NO2 + OH --> HNO3
  if ( (iA == ikey_spec(NO2) .and. iB == ikey_spec(OH) .or. &
             iA == ikey_spec(OH)  .and. iB == ikey_spec(NO2) ) &
             .and. reaction(i)%fB == 1. ) then
    do j = 1, nP
      if ( reaction(i)%iP(j) == ikey_spec(HNO3) .and. &
                 reaction(i)%fP(j) == 1. ) &
                              ikey_rxn(IPHNO3) = i
    end do
  end if
! -- NO2 + O3 --> NO3
  if ( (iA == ikey_spec(NO2) .and. iB == ikey_spec(O3) .or. &
             iA == ikey_spec(O3)  .and. iB == ikey_spec(NO2)) &
             .and. reaction(i)%fB == 1. ) then
    do j = 1, nP
      if ( (reaction(i)%iP(j) == ikey_spec(NO3)) .and. &
               (reaction(i)%fP(j) == 1.) ) ikey_rxn(IPNO3) = i
    end do
  end if
! -- NO + O3 --> NO2
  if ( (iA == ikey_spec(NO) .and. iB == ikey_spec(O3) .or. &
             iA == ikey_spec(O3) .and. iB == ikey_spec(NO)).and. &
             reaction(i)%fB == 1. ) then
    do j = 1, nP
      if ( (reaction(i)%iP(j) == ikey_spec(NO2)) .and. &
               (reaction(i)%fP(j) == 1.) ) ikey_rxn(INOO3) = i
    end do
  end if
! -- NO3 + NO2 --> N2O5
  if ( (iA == ikey_spec(NO3) .and. iB == ikey_spec(NO2) .or. &
             iA == ikey_spec(NO2) .and. iB == ikey_spec(NO3)) &
             .and. reaction(i)%fB == 1. ) then
    do j = 1, nP
      if ( (reaction(i)%iP(j) == ikey_spec(N2O5 )) .and. &
               (reaction(i)%fP(j) == 1.) ) ikey_rxn(INO3NO2) = i
    end do
  end if
! -- NO + HO2 --> NO2 + OH
  if ( (iA == ikey_spec(NO) .and. iB == ikey_spec(HO2) .or. &
             iA == ikey_spec(HO2) .and. iB == ikey_spec(NO)) .and. &
             reaction(i)%fB == 1. ) then
    lno2 = .false.
    loh  = .false.
    do j = 1, nP
      if ( reaction(i)%iP(j) == ikey_spec(NO2) .and. &
               reaction(i)%fP(j) == 1. ) lno2 = .true.
      if ( reaction(i)%iP(j) == ikey_spec(OH)  .and. &
               reaction(i)%fP(j) == 1. ) loh = .true.
    end do
    if (lno2 .and. loh)  ikey_rxn(INOHO2) = i
  end if
! -- NO3 + VOC --> products
  do j = 1, nvoc
    iVOC = indx_voc(j)
    if (iA == ikey_spec(NO3) .and. iB == iVOC .or. &
           iA == iVOC .and. iB == ikey_spec(NO3)) then
      nreact_voc = nreact_voc + 1
      indx_react_voc(nreact_voc) = i
    end if
  end do
end do

do i = 1, NKEY_RXNS
  if ( ikey_rxn(i) == NOT_SET_I ) then
    nError   = IV_ERROR
    eRoutine = 'init_staged_chem'
    eMessage = 'Necessary reaction for staged chemistry' &
                     // ' not found in imc file'
    if (i == IPHNO3) then
      eAction = 'NO2 + OH --> HNO3'
    else if (i == IPNO3) then
      eAction = 'NO2 + O3 --> NO3'
    else if (i == INOO3) then
      eAction = 'NO + O3 --> NO2'
    else if (i == INO3NO2) then
      eAction = 'NO3 + NO2 --> N2O5'
    else if (i == INOHO2) then
      eAction = 'NO + HO2 --> NO2 + OH'
    end if
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if
end do
if (nreact_voc == 0) then
  nError = IV_ERROR
  eRoutine = 'init_staged_chem'
  eMessage = 'Necessary reaction for staged chemistry' &
                     //' not found in imc file'
  eAction = 'No VOC reaction with NO3'
  go to 9999
end if

if (lstage) then
  write(lun_log,'(25("="),"STAGED CHEMISTRY",25("="))')
end if
do j = 1, nstage
  if (lstage) then
    write(lun_log,'(56("-"))')
    write(lun_log,'(A6,2x,i2,2(A20,i3))') &
           'Stage:',j,'No. of reactions:',nreact_s(j), &
           'No. of species:',nspec_s(j)
    write(lun_log,'(4A)') &
           (species(indx_spec(i,j))%name,i=1,nspec_s(j))
  else
    if (neqm_s(nstage) > 0) then
      write(lun_log,*)'Equilibrium species solution'
      write(lun_log,*)'----------------------------'
    end if
  end if
  do i = 1, neqm_s(j)
    nn = indx_eq_s(i,j)
    jj = nblank(equilibrium(nn)%name)
    if (itype_eq_s(nn,j) == 0) then
      write(lun_log,200)equilibrium(nn)%name(1:jj) &
                                     ,'fully coupled solution'
    else if (itype_eq_s(nn,j) == -1) then
      write(lun_log,200)equilibrium(nn)%name(1:jj),'solved at the end'
    else if (itype_eq_s(nn,j) == -2) then
      write(lun_log,200)equilibrium(nn)%name(1:jj),'solved directly'
    else
      k = nblank(equilibrium(itype_eq_s(nn,j))%name)
      write(lun_log,205)equilibrium(nn)%name(1:jj), &
             'eliminated in favor of ', &
             equilibrium(itype_eq_s(nn,j))%name(1:k)
    end if
  end do
end do

write(lun_log,'(56("-"))')
200 format(A,' ----> ',A)
205 format(A,' ----> ',2A,A)

write(lun_log,*)'VOC reactions with NO3 found on imc file:'
do i = 1, nreact_voc
  j = indx_react_voc(i)
  iA = reaction(j)%iA
  iB = reaction(j)%iB
  id = reaction(j)%ID
  write(lun_log,300) id, species(iA)%name, species(iB)%name
end do
300 format(i3,3x,A,' + ',A,' -->    products')

write(lun_log,*)
write(lun_log,*)'Other key reactions found on imc file:'
do i = 1, NKEY_RXNS
  j = ikey_rxn(i)
  iA = reaction(j)%iA
  iB = reaction(j)%iB
  nP = reaction(j)%nP
  id = reaction(j)%ID
  plus = ' + '
  write(lun_log,305) id, species(iA)%name, plus, species(iB)%name &
        ,(species(reaction(j)%iP(jj))%name,plus,jj = 1, nP-1) &
        ,species(reaction(j)%iP(nP))%name
end do
305 format(i3,3x,3A,' --> ',10(A8,A3))

9999    return
end

subroutine set_staged_species
!*******************************************************************************
!
! FUNCTION: This routine sets the indices for fast and slow species for the 
!           different stages of chemistry from the definition of species classes
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

! --- LOCALS

integer i, j, nn

do j = 1, nstage
  do i = 1,nspec_s(j)
    nn = indx_spec(i,j)
    if(species(nn)%class == ID_SPECIES_FAST) then
      nfast_s(j) = nfast_s(j) + 1
      indxf_s(nfast_s(j),j) = nn
    else if(species(nn)%class == ID_SPECIES_SLOW) then
      nslow_s(j) = nslow_s(j) + 1
      indxs_s(nslow_s(j),j) = nn
    else if(species(nn)%class == ID_SPECIES_EQUILIBRIUM) then
      neqm_s(j) = neqm_s(j) + 1
      indx_eq_s(neqm_s(j),j) = nn - nspecies
    end if
  end do
end do

return
end

subroutine init_equilibrium
!*******************************************************************************
!
! FUNCTION: Equilibrium solver initialization
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             set_indx_eq           IsEquilibrium 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use multcomp_inc

implicit none

! --- LOCALS

integer i, j, k, iA, iB, iP, ii
real Cq(MAX_EQ, MAX_EQ, MAX_EQ), Cl(MAX_EQ, MAX_EQ)

integer nlin, elim, nsolve, is, js, ist, item(MAX_EQ)
integer  nquad(MAX_EQ), jj, kk

logical lA, lB, check

!====   Check for equilibrium species

if(nequilibrium <= 0)return

do ist = 1, nstage

  neq_s = neqm_s(ist)

!====     Clear solver arrays

  do i = 1,nequilibrium
    do j = 1,nequilibrium
      Cl(i,j) = 0.0
      do k = 1,nequilibrium
        Cq(i,j,k) = 0.0
      end do
    end do
  end do

!====     Loop over reactions and set solver coefficients

  do i = 1, nreact_s(ist)

!======   Reaction has equilibrium reactants/products

    check = (reaction(i)%ktype == ID_K_CONST) &
              .and. (reaction(i)%kdata(1) == 0.)

    if( btest(reaction(i)%class,ID_REACT_EQUILIBRIUM) .and. .not. check )then

      iA  = reaction(i)%iA
      lA = ps(iA)%equil

      if( lA )iA = iA - nspecies
!========         Linear Reactions

      if( IsLinear(i) )then

!==========       Equilibrium reactant

        if( lA ) then

          Cl(iA,iA) = 1.
          do j = 1, reaction(i)%nP
            iP = reaction(i)%iP(j)
            if( ps(iP)%equil ) then
              iP = iP - nspecies
              Cl(iP,iA) = 1.
            end if
          end do

        end if

!========         Quadratic reaction

      else

        iB = reaction(i)%iB
        lB = ps(iB)%equil

        if( lB )iB = iB - nspecies

!==========       Equilibrium A : Non equilibrium B

        if( lA .and. .not.lB )then

          Cl(iA,iA) = 1.
          do j = 1, reaction(i)%nP
            iP = reaction(i)%iP(j)
            if( ps(iP)%equil ) then
              iP = iP - nspecies
              Cl(iP,iA) = 1.
            end if
          end do

!==========       Equilibrium B : Non equilibrium A

        else if( lB .and. .not.lA)then

          Cl(iB,iB) = 1.
          do j = 1, reaction(i)%nP
            iP = reaction(i)%iP(j)
            if( ps(iP)%equil ) then
              iP = iP - nspecies
              Cl(iP,iB) = 1.
            end if
          end do

!==========       Equilibrium A : Equilibrium B -> Nonlinear terms

        else if( lB .and. lA)then

          Cq(iA,iA,iB) = 1.
          Cq(iB,iA,iB) = 1.
          do j = 1, reaction(i)%nP
            iP = reaction(i)%iP(j)
            if( ps(iP)%equil ) then
              iP = iP - nspecies
              Cq(iP,iA,iB) = 1.
            end if
          end do

        end if
      end if
    end if

  end do

  nsolve   = neq_s
  ndir_eq  =  0
  nsubs_eq =  0
  nlin_eq  =  0
  do i = 1, neq_s
    is  = indx_eq_s(i,ist)
    indx_eq(i)   = is
  end do
  do i = 1, nequilibrium
    itype_eq(i) = ISOLVE_EQ
    nquad(i) = 0
  end do

! --- find species that can be solved directly

  check = .true.

  do while (check)

    do i = 1, nsolve
      item(i) = indx_eq(i)
    end do
    check = .false.
    do is = 1, nsolve
      i = item(is)
      nlin = 0
      do j = 1, neq_s
        jj = indx_eq_s(j,ist)
        if (Cl(i,jj) /= 0. .and. jj /= i) then
          nlin = nlin + 1
        end if
        do k = 1, neq_s
          kk = indx_eq_s(k,ist)
          if (Cq(i,jj,kk) /= 0.) then
            nquad(i) = nquad(i) + 1
          end if
        end do
      end do
      if (nquad(i) == 0 .and. nlin == 0) then
        indx_eq(nsolve) = i
        nsolve = nsolve - 1
        ndir_eq = ndir_eq + 1
        itype_eq(i) = IDIR_EQ
        check = (.true. .and. nsolve > 0)
        do k = 1, neq_s
          kk = indx_eq_s(k,ist)
          Cl(kk,i) = 0.
        end do
      end if
    end do
    call set_indx_eq(ist)
  end do

!---check for species that can substituted at the end
  do i = 1, neq_s
    ii = indx_eq_s(i,ist)
    if (itype_eq(ii) == ISOLVE_EQ) then
      check = .true.
      do j = 1, neq_s
        jj = indx_eq_s(j,ist)
        if (Cl(jj,ii) /= 0. .and. jj /= ii) then
          check = .false.
        end if
        do k = 1, neq_s
          kk = indx_eq_s(k,ist)
          if (Cq(jj,ii,kk) /= 0. .or. Cq(jj,kk,ii) /= 0.) then
            check = .false.
          end if
        end do
      end do
      if (check) then
        indx_eq(nsolve) = ii
        nsolve = nsolve - 1
        itype_eq(ii) = ISUBS_EQ
        nsubs_eq = nsubs_eq + 1
      end if
    end if
  end do

  call set_indx_eq(ist)

!---check for species that can be eliminated in favor of another

  check = .true.
  do while (check)

    do i = 1, nsolve
      item(i) = indx_eq(i)
    end do
    check = .false.
    do is = 1, nsolve
      i = item(is)
      nlin = 0
      do js = 1, nsolve
        j = indx_eq(js)
        if (Cl(i,j) /= 0.0 .and. i /= j) then
          nlin = nlin + 1
          elim = j
        end if
      end do
      if (nlin == 1 .and. nquad(i) == 0 .and. nsolve > 1) then
        itype_eq(i) = elim
        indx_eq(nsolve) = i
        nsolve = nsolve - 1
        nlin_eq = nlin_eq + 1
        check = .true.
        do k = 1, neq_s
          kk = indx_eq_s(k,ist)
          if (Cl(kk,i) /= 0.) Cl(k,elim) = 1.
        end do
      end if
      call set_indx_eq(ist)
    end do
  end do

  nsolve_eq = nsolve

  do i = 1, neq_s
    irow_eq(indx_eq(i)) = i
  end do

!----save equilibrium stage variables

  nsolve_eq_s(ist) = nsolve_eq
  ndir_eq_s(ist)   =  ndir_eq
  nsubs_eq_s(ist)  =  nsubs_eq
  nlin_eq_s(ist)   =  nlin_eq
  do i = 1, nequilibrium
    itype_eq_s(i,ist) = itype_eq(i)
  end do
  do i = 1, neq_s
    indx_eq_s(i,ist)  = indx_eq(i)
    irow_eq_s(indx_eq(i),ist) = i
  end do

end do  ! end of stages

return
end

subroutine set_indx_eq(ist)
!*******************************************************************************
!
! FUNCTION:  Save index for equilibrium
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

integer ist  !chemical stage

! --- LOCALS

integer is, i, ii

is = 0
do i = 1,neqm_s(ist)
  ii = indx_eq_s(i,ist)
  if (itype_eq(ii) == ISOLVE_EQ) then
    is = is + 1
    indx_eq(is) = ii
  end if
end do

return
end

subroutine SetFast(class)
!*******************************************************************************
!
! FUNCTION:  Set fast species
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

integer class  !species class


class = ibset(class,ID_REACT_FAST)

return
end

subroutine SetEquilibrium(class)
!*******************************************************************************
!
! FUNCTION:  Set equilibrium species
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
 
integer class  !species class


class = ibset(class,ID_REACT_EQUILIBRIUM)

return
end

subroutine SetSlow(class)
!*******************************************************************************
!
! FUNCTION:  Set slow species
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

integer class  !species class


class = ibset(class,ID_REACT_SLOW)

return
end

subroutine SetLinear(i,class)
!*******************************************************************************
!
! FUNCTION: Set linear reaction
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
 
integer i      !reaction number
integer class  !reaction class

class = ibset(class,ID_REACT_LINEAR)
IsLinear(i) = .true.

return
end

subroutine SetStar(jA,lset)
!*******************************************************************************
!
! FUNCTION: Set mean correlation pointer
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

integer jA   !species number
logical lset !whether or not it is already set

! --- LOCALS

integer iA

iA = jA

if( iA <= nspecies )then

  if(species(iA)%star == 0)then
    ncorrm = ncorrm + 1
    species(iA)%star = nspectot + ncorrt + ncorrm
    lset = .true.
    IsStar(iA) = .true.
  end if

else if (iA <= nspectot)then

  iA = iA - nspecies
  if(equilibrium(iA)%star == 0)then
    equilibrium(iA)%star = iA
    lset = .true.
    IsStar(iA+nspecies) = .true.
  end if

end if

return

end

logical function IsMCParticle(i)
!*******************************************************************************
!
! FUNCTION: Check for particle species
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
! Jan. 2005   :Check additional particle types (number concs. and surface
!              area - PK, AER
!*******************************************************************************
 
! --- MODULES
 
use multcomp_inc

implicit none

! --- ARGUMENTS

integer i  !species number

IsMCParticle = species(i)%class == ID_SPECIES_PARTICLE .or. &
               species(i)%class == ID_SPECIES_NUMBER .or. &
               species(i)%class == ID_SPECIES_SURFACE

return
end

logical function IsFast(class)
!*******************************************************************************
!
! FUNCTION: Check for fast reaction
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

integer class  !reaction class


IsFast = btest(class,ID_REACT_FAST)

return
end

logical function IsEquilibrium(class)
!*******************************************************************************
!
! FUNCTION: Check for equilibrium species in reaction
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

integer class   !reaction class


IsEquilibrium = btest(class,ID_REACT_EQUILIBRIUM)

return
end

logical function IsSlow(class)
!*******************************************************************************
!
! FUNCTION: Check for slow species in reaction
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
 
integer class    !reaction class

IsSlow = btest(class,ID_REACT_SLOW)

return
end

integer function GetMultiNum()
!*******************************************************************************
!
! FUNCTION:  Get number of multicomponent reacting species
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

GetMultiNum = nspectot

return
end

integer function GetMultiSrfPos(ispecies,idep)
!*******************************************************************************
!
! FUNCTION: Get pointer to multicomponent species's surface array
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

! --- ARGUMENTS

implicit none

integer ispecies  !species number
integer idep      !deposition or dose flag

if(idep == 0)then
  GetMultiSrfPos = species(ispecies)%dos
else if(idep == 1)then
  GetMultiSrfPos = species(ispecies)%dep
else
  GetMultiSrfPos = ispecies
end if

return
end

logical function IsMultiDep(ispecies)
!*******************************************************************************
!
! FUNCTION: Check for surface deposition of multicomponent
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

integer ispecies

IsMultiDep = species(ispecies)%dep > 0

return
end

logical function IsMultiDos(ispecies)
!*******************************************************************************
!
! FUNCTION: Check for surface dos of multicomponent
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
 
integer ispecies

IsMultiDos = species(ispecies)%dos > 0

return
end

logical function IsMultiStar()
!*******************************************************************************
!
! FUNCTION: Check multicomponent mean correlation (S*)
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

IsMultiStar = ncorrm > 0

return
end

subroutine set_vdep_aer
!*******************************************************************************
!
! FUNCTION:  Set the deposition velocity parameters for the aerosol particles
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   ufall
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc

implicit none

! --- LOCALS

integer j, isec

real rp(13)
real dbp(13)
real dlb, dub
real vmin, vbar, vmax, fmin, fbar, fmax, fsum
real db_dust, rate, dbpx, ufall

logical off_end, less_than

rp(1)  =  0.0005e-6
rp(2)  =  0.0010e-6
rp(3)  =  0.0025e-6
rp(4)  =  0.0050e-6
rp(5)  =  0.0100e-6
rp(6)  =  0.0250e-6
rp(7)  =  0.0500e-6
rp(8)  =  0.2500e-6
rp(9)  =  0.5000e-6
rp(10) =  2.5000e-6
rp(11) =  5.0000e-6
rp(12) = 25.0000e-6
rp(13) = 50.0000e-6

dbp(1)  = 5.1e-6
dbp(2)  = 1.3e-6
dbp(3)  = 2.1e-7
dbp(4)  = 5.2e-8
dbp(5)  = 1.3e-8
dbp(6)  = 2.4e-9
dbp(7)  = 6.7e-10
dbp(8)  = 6.3e-11
dbp(9)  = 2.8e-11
dbp(10) = 4.9e-12
dbp(11) = 2.4e-12
dbp(12) = 1.e-20
dbp(13) = 1.e-20

do isec = 1, nsec_aer

  dub = secbnds_aer(isec+1)
  dlb = secbnds_aer(isec)

  dm_aer(isec) = sqrt(dub*dlb)

  vmin = ufall(rhoair,rho_aer,rmuair,dlb)
  vbar = ufall(rhoair,rho_aer,rmuair,dm_aer(isec))
  vmax = ufall(rhoair,rho_aer,rmuair,dub)
  fmin = 1.0/dlb
  fbar = 1.0/dm_aer(isec)
  fmax = 1.0/dub
  fsum = fmin + fbar + fmax
  vs_aer(isec) = (fmin*vmin    + fbar*vbar    + fmax*vmax)/fsum

  off_end   = .false.
  less_than = .true.
  j = 2
  do while (less_than)
   if (2.0*rp(j) >= dm_aer(isec)) then
      less_than = .false.
    else
      j = j + 1
      if (j > 13) then
        off_end = .true.
        less_than = .false.
      end if
    end if
  end do

  if (off_end) then
    db_dust = 1.e-20
  else
    rate = (0.5*dm_aer(isec)-rp(j-1))/(rp(j)-rp(j-1))
    dbpx = alog(dbp(j-1)) + rate*(alog(dbp(j))-alog(dbp(j-1)))
    dbpx = exp(dbpx)
    db_dust = min(dbpx,rnu)
  end if
  diff_aer(isec) = db_dust 
  write(lun_log,*,err=9998)'Aerosol (diam,rho,diff,vs):',isec, &
            dm_aer(isec),rho_aer,diff_aer(isec),vs_aer(isec)

end do

9999    continue

return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'set_vdep_aer'
eMessage = 'Error writing SCIPUFF log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

end

subroutine init_wash_aerosol_chem
!*******************************************************************************
!
! FUNCTION:  Initialize washout for the aerosol particles
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          init_wash_chem                   ufall
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc

implicit none

! --- PARAMETERS

real       A_RAIN
parameter (A_RAIN = 7.317e-04)
real       B_RAIN
parameter (B_RAIN = 0.21)
real       A_SNOW
parameter (A_SNOW = 5.215e-04)
real       B_SNOW
parameter (B_SNOW = 0.25)
real       RHOW
parameter (RHOW   = 1.00e+03)
real       V_SNOW
parameter (V_SNOW = 1.1)

! --- LOCALS

integer i, isec

character*7 nametmp(NWASH)
real pr(NWASH), dr(NWASH), washtmp(NWASH), ufall

!-----  Set Rain Precipitation Groups (mm/hr)
!-----  RAIN

pr(1)  =  0.5
pr(2)  =  3.5
pr(3)  = 25.0

!-----  SNOW

pr(4)  =   5.0
pr(5)  =  20.0
pr(6)  = 100.0

nametmp(1) = 'LGTRAIN'
nametmp(2) = 'MODRAIN'
nametmp(3) = 'HVYRAIN'
nametmp(4) = 'LGTSNOW'
nametmp(5) = 'MODSNOW'
nametmp(6) = 'HVYSNOW'

!-----  Set Precipitation fall velocity (m/s)

do isec = 1, nsec_aer
  twash_aer(0,isec) =  0.0  !No precipitation
end do

do i = 1,NRAIN
  dr(i)      =  A_RAIN*pr(i)**B_RAIN
  washtmp(i) =  ufall(rhoair,RHOW,rmuair,dr(i))
end do
do i = NRAIN+1,NWASH
  dr(i)      =  A_SNOW*pr(i)**B_SNOW
  washtmp(i) =  V_SNOW
end do

!-----  Calculate Scavenging Coefficients (s)

write(lun_log,*) 'Aerosol wash-out timescale (s):'
do isec = 1, nsec_aer
  write(lun_log,*) 'Size group: ',dm_aer(isec)
  do i = 1,NWASH
    call init_wash_chem(i,isec,pr(i),dr(i),washtmp(i),twash_aer(i,isec))
    write(lun_log,500) nametmp(i),twash_aer(i,isec)
  end do
end do
500   format(a7,' = ',1p,e12.4)
return

end

subroutine init_wash_chem(ipr,isec,pr,dr,vr,tauwo)
!*******************************************************************************
!
! FUNCTION: Initialize washout for an individual aerosol particle 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
integer ipr, isec
real pr, dr, vr, tauwo

! --- PARAMETERS

real       MUW
parameter (MUW    = 1.00e-03)
real       CNVFAC
parameter (CNVFAC = 4.167e-07)

! --- LOCALS

real sc3, tau
real re, re2, sc, st, sts, wi, h, e, e1, e2, e3, sc2

!-----  Calculate dimensionless groups

tau  = vs_aer(isec)/G0
re   = (0.5*dr*vr*rhoair)/rmuair
sc   = rmuair/(rhoair*diff_aer(isec))
st   = 2.*tau*(vr - vs_aer(isec))/dr
if (ipr <= NRAIN) then
  wi = rmuair/MUW                               !RAIN
else
  wi = 0.                                       !SNOW
end if
h    = dm_aer(isec)/dr

re2  = sqrt(re)
sc2  = sqrt(sc)
sc3  = sc**(1./3.)
sts  = 0.0

e1   = 4.*h*(wi + (1.+2.*re2)*h)
e2   = ((st-sts)/(st-sts+0.666667))**1.5
e3   = 4.*(1. + 0.4*re2*sc3 + 0.16*re2*sc2)/(re*sc)

e    = (e1 + e2 + e3)/3.0

tauwo= CNVFAC*pr*e/dr

return
end
