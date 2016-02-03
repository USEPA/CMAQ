subroutine read_control(iunit)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Read control namelist parameters
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- ARGUMENTS
 
integer iunit   !Input file unit number

! --- LOCALS

integer ios

file_rst = ' '
path_rst = ' '
time_rst = DEF_VAL_R

if (file_rst == ' ') then
  time_rst = 0.0
end if

9999    return

end

subroutine read_end_time(iunit)
!******************************************************************************
!
! FUNCTION:   Read end time from input file
!             Dummy routine: This is not done for pig, 
!             since the info comes from the host model
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************

implicit none

! --- ARGUMENTS
 
integer iunit   !Input file unit number

! end time is set by the host model

return

end

subroutine read_flags(iunit)
!******************************************************************************
!
! FUNCTION:   Read flags namelist from input file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer iunit   !Input file unit number

! --- LOCALS

character*8 hazarea

!namelist / flags / title,create,hascal,audit_class,audit_analyst &
!                      ,dynamic,dense_gas,static,multicomp,hazarea &
!                      ,run_mode
!
! Note: "dynamic" now read in options flag

!200 read(iunit,flags,err=200,end=201)

title = 'SCICHEM Plume-in-Grid'
create = .false.
multicomp = .true.
static = .true.
hascal = .false. 
hazard = IHAZ_OFF

9999    return

!201     nError   = RD_ERROR
!        eMessage = 'Error reading FLAGS namelist'
!        go to 9999
end

subroutine read_options(iunit)
!******************************************************************************
!
! FUNCTION:   Read options namelist from input file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              check_real
!
! REVISION HISTORY: Eliminate lsplitz from interface. Leave in namelist for
!                   reading old files. -PK, AER, April 2007 based on BC's
!                   changes to namelist_pc.f90
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- ARGUMENTS
 
integer iunit   !Input file unit number

! --- LOCALS

integer ios

namelist / options / t_avg,cmin,lsplitz,delmin,wwtrop,epstrop,sltrop &
           ,uu_calm,sl_calm,nzbl,mgrd,grdmin,z_dosage,smpfile,dt_smp,&
           dynamic, vres

dynamic = .true.  !set default (was in "flags" namelist) 
vres    = 250.    !default vertical resolution (was in "domain" namelist)

! - Allow user to set "dynamic", false --> use plume rise formula
!                                 true --> compute dynamically

read(iunit,options,iostat=ios)
if (ios /= 0) then
  nError   = RD_ERROR
  eMessage = 'Error reading OPTIONS namelist'
  go to 9999
end if

!Eliminate lsplitz from interface. Leave in namelist for reading old files
!Just always set to FALSE
lsplitz = .FALSE.

call check_real(t_avg,0.0,1.e30,'Averaging time')
call check_real(cmin,0.0,1.e30,'Minimum puff mass')
call check_real(delmin,0.0,1.e30,'Minimum surface grid')
call check_real(wwtrop,0.0,1.e30,'wwtrop')
call check_real(sltrop,1.0e-30,1.e30,'sltrop')
call check_real(epstrop,0.0,1.e30,'epstrop')
call check_real(uu_calm,0.0,1.e30,'uu_calm')
call check_real(sl_calm,1.0e-30,1.e30,'sl_calm')

dt_smp = NOT_SET_R

9999    return

end

subroutine read_matdef(fname,read_v02,iunit,nmat,mat,naux,mataux,lok)
!******************************************************************************
!
! FUNCTION:  Set the PiG material definition (not read from file)
!
! PRECONDITIONS REQUIRED:  
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  cupper        check_nsubgroups       put_puff_material
!                SetClass           SetClassMulti                 IsMulti
!               naux_matl          SetClassHazard                   IsGas
!              IsParticle
!
! REVISION HISTORY: 
!
!  Updated Feb. 2004 for consistency with stand-alone SCICHEM V1601 (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc
use class_inc

implicit none

! --- ARGUMENTS
 
character*(*) fname  !Material name (not used)
logical read_v02     !Flag to read old version
logical lok          !Error flag
integer iunit        !Input file unit number
integer nmat         !Number of materials
integer naux         !Number of auxilliary variables

real    mataux(*)             !Material auxilliary array
type ( material_str ) mat(*)  !Material structure

! --- LOCALS

integer i, i0, ios, nsg, icls, ityp, j0 
integer naux_matl, maux,ieof, nmat_save

logical lseek

real gas_deposition
real psize(10*MAXSGP),pbounds(10*MAXSGP+1)
real density
real decay_amp
real decay_min
real antoine(3), mweight, liquid_density(2), surf_tension
real specific_heat_liq, specific_heat_vap
real spread_factor, conc_min, NWPN_decay

character*16  mname, class,units
character*128 file_name
character*128 file_path

logical group_deposition, total_deposition
logical group_dose, total_dose, ltot, multi_comp

!namelist / matdef / mname,class,density,gas_deposition, &
!                 antoine,mweight,liquid_density, &
!                 surf_tension,spread_factor,conc_min, &
!                 NWPN_decay, &
!                 nsg,psize,pbounds, &
!                 group_deposition,total_deposition, &
!                 group_dose,total_dose,units, &
!                 decay_amp, decay_min, multi_comp, &
!                 specific_heat_liq, specific_heat_vap, &
!                 file_name,file_path

type ( gas_material ) pmatgas
type ( part_material ) pmatpart
type ( puff_material ) pmat

equivalence ( pmat, pmatpart, pmatgas )

logical IsGas, IsParticle, IsMulti
integer SetClass, SetClassHazard
integer SetClassMulti

!---- Determine if looking for all materials or a specific material

call cupper(fname)
lseek = fname(1:1) /= char(0) .and. fname /= ' ' .and. &
           fname(1:4) /= 'ALL '

! ---   Set default values (for Plume-in-Grid)

lok = .false.

mname     = 'TRAC '
class     = MAT_GAS
units     = 'ppm-m3 '
nsg       = 1
density   = rhoair
group_dose = .false.       !if these are ever used,
total_dose = .false.       !must fix restart from particular time
group_deposition = .false. ! "
total_deposition = .false. ! "
multi_comp = .true.
gas_deposition = 0.
do i = 1,MAXSGP
  psize(i)   = 0.
  pbounds(i) = 0.
end do
pbounds(MAXSGP+1) = 0.
decay_amp = 0.
decay_min = 0.
antoine(1) = NOT_SET_R
antoine(2) = 0.
antoine(3) = 0.
mweight    = NOT_SET_R
surf_tension = NOT_SET_R
spread_factor = NOT_SET_R
conc_min      = 0.
NWPN_decay    = -1.3
liquid_density(1) = NOT_SET_R
liquid_density(2) = 0.
specific_heat_liq = 1000.
specific_heat_vap = 1000.
file_name = ' '
file_path = ' '

nmat_save = nmat

! - Do not read in PiG version

!  read(iunit,matdef,end=200,iostat=ios)

!if (ios /= 0) then
!  nError   = RD_ERROR
!  eMessage = 'Error reading MATDEF namelist'
!  go to 9999
!end if

call cupper(mname)
call cupper(class)

if(lseek .and. mname /= fname) go to 9999

! ----  Get material class

icls = SetClass(class)

if (multi_comp .and. multicomp) then
  icls = SetClassMulti(icls)
end if

if(icls == 0)then
  nError   = UK_ERROR
  eMessage = 'Unknown material class : '//TRIM(class)
  go to 9999
end if

! ----  Build material structure if name is OK

if(mname /= ' ')then

  if(nmat == 0)then
    nmat = 1
    ityp  = nmat
  else
    ityp = 0
    do i = 1,nmat
      if(mname == mat(i)%cmat)ityp = i
    end do
    if(ityp <= 0)then
      nmat = nmat + 1
      ityp  = nmat
      if(nmat > MAXMTYP)then
        nError   = SZ_ERROR
        eMessage = 'Too many materials'
        write(eAction,'(a,i5)') 'Maximum number is ',MAXMTYP
        go to 9999
      end if
    else
      nError   = NF_ERROR
      eMessage = 'Multiple material definition'
      go to 9999
    end if
  end if

  if (IsMulti(icls)) then
    i = -1
  else
    i = 0
  end if

  mat(ityp)%icls     = icls
  mat(ityp)%iaux     = naux + 1
  mat(ityp)%nmc      = i
  mat(ityp)%ipmc     = NOT_SET_I
  mat(ityp)%ioffp    = ntypp
  mat(ityp)%ioffs    = NOT_SET_I
  mat(ityp)%ioffd    = NOT_SET_I
  mat(ityp)%ioffs_mc = NOT_SET_I
  mat(ityp)%ioffd_mc = NOT_SET_I
  mat(ityp)%lsrf_mc  = .false.
  mat(ityp)%ldos_mc  = .false.

  mat(ityp)%prop(1)  = decay_amp
  mat(ityp)%prop(2)  = decay_min
  mat(ityp)%prop(3)  = conc_min
  mat(ityp)%prop(4:10)  = NOT_SET_R

! ----    Check subgroups

  call check_nsubgroups(nsg,mat(ityp))
  if (nError /= NO_ERROR) go to 9999

  ltot  = nsg > 1

! ----    Set typeID structure

  ntypp = ntypp + nsg
  if(ntypp > MAXPTYP)then
    nError   = SZ_ERROR
    eMessage = 'Too many puff types'
    write(eAction,'(a,i5)') 'Maximum number is ',MAXPTYP
    go to 9999
  end if

  i0 = mat(ityp)%ioffp
  do i = 1,nsg
    typeID(i0+i)%imat  = ityp
    typeID(i0+i)%igrp  = i
    typeID(i0+i)%ltot  = ltot
    typeID(i0+i)%icls  = icls
    typeID(i0+i)%npaux = naux_matl(icls,i,ltot)
    typeID(i0+i)%nmc   = 0
    typeID(i0+i)%ipmc   = 0
  end do

!------- Set hazard flags

  if (hazard == IHAZ_ON) then
    do i = 1,nsg
      typeID(i0+i)%icls = SetClassHazard(typeID(i0+i)%icls)
    end do
  else if (hazard == IHAZ_COMB) then
    ntypp = ntypp + nsg
    if(ntypp > MAXPTYP)then
      nError   = SZ_ERROR
      eMessage = 'Too many puff types'
      write(eAction,'(a,i5)') 'Maximum number is ',MAXPTYP
      go to 9999
    end if
    j0 = i0 + nsg
    do i = 1,nsg
      typeID(j0+i) = typeID(i0+i)
      typeID(j0+i)%icls = SetClassHazard(typeID(j0+i)%icls)
    end do
  end if

! ----    Set material auxiliary variables

! ------  GAS

  if(IsGas(icls))then
    if(naux+MAXGMAUX > MAXMAUX)then
      nError   = SZ_ERROR
      eMessage = 'Too many material Auxiliary variables'
      write(eAction,'(a,i5)') 'Maximum number is ',MAXMAUX
      go to 9999
    end if
    pmatgas%rho  = density
    pmatgas%vd   = gas_deposition
    call put_puff_material(i0+1,mat,mataux,pmat)
    naux = naux + MAXGMAUX

! ------  LIQUID


! ------  PARTICLES

  else if(IsParticle(icls))then
    if(naux+MAXPMAUXX+MAXPMAUX*nsg > MAXMAUX)then
      nError   = SZ_ERROR
      eMessage = 'Too many material Auxiliary variables'
      write(eAction,'(a,i5)') 'Maximum number is ',MAXMAUX
      go to 9999
    end if
    pmatpart%nsg   = nsg
    pmatpart%rho   = density
    pmatpart%vd    = 0.0
    pmatpart%sigvd = 0.0
    pmatpart%diff  = 0.0
    do i = 1,nsg
      pmatpart%dmin = pbounds(i)
      pmatpart%dbar = psize(i)
      pmatpart%dmax = pbounds(i+1)
      call put_puff_material(i0+i,mat,mataux,pmat)
    end do
    naux = naux + nsg*MAXPMAUX + MAXPMAUXX
  end if

! ----    Set deposition flags

  if(group_deposition .or. total_deposition)then
    mat(ityp)%lsrfg = group_deposition &
             .or. (total_deposition .and. (nsg==1) )
    mat(ityp)%lsrft = total_deposition .and. (nsg>1)
  else
    mat(ityp)%lsrfg = .false.
    mat(ityp)%lsrft = .false.
  end if
  if(group_dose .or. total_dose)then
    mat(ityp)%ldosg = group_dose &
             .or. (total_dose .and. (nsg==1) )
    mat(ityp)%ldost = total_dose .and. (nsg>1)
  else
    mat(ityp)%ldosg = .false.
    mat(ityp)%ldost = .false.
  end if

  mat(ityp)%cmat  = mname
  mat(ityp)%unit  = units
  mat(ityp)%ccls  = class
  mat(ityp)%file  = TRIM(file_name)
  mat(ityp)%path  = TRIM(file_path)

end if

lok = .true.

9999    continue

if(nError /= NO_ERROR)then
  nmat = nmat_save
end if

return

200     continue
nError = EOF_ERROR
go to 9999

end
