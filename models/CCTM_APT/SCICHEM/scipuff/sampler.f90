!*****************************************************************************
!subroutine write_smp changed for new aerosol driver for SCICHEM-CMAQ
!PK, AER, January 2005
!*****************************************************************************

subroutine read_smp
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:   Read sampler location file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   get_c                  cupper               get_topog
!                 set_smp              create_smp                   IsGas
!              IsParticle              sub_groups                 IsMulti
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_smp
use multcomp_inc
use files_inc

implicit none
 
! --- LOCALS

real    xs, ys, zs, hx, hy ,hz

integer ios, sub_groups
integer is, imat, i, nch, nblank, nchp

logical   IsGas, IsParticle, IsMulti, lerr

character*128 string

!------ check if sampler location file is specified

if (smpfile == ' ') then
  lsmp = .false.
  go to 9999
else
  lsmp = .true.
end if

!------ set max sampler time level

  mxlev_smp = 0

!------ Get sampler locations

open(unit=lun_tmp,file=smpfile, status='OLD', action="read",iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Error opening sampler file'
  eInform  = 'File= '//TRIM(smpfile)
  go to 9999
end if

read(lun_tmp,50,iostat=ios) string
50      format(a)
if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Error reading sampler file header'
  eInform  = 'File= '//TRIM(smpfile)
  go to 9999
end if

!----- Get material name

nch = nblank(string)
call get_c(string,nch,' ',matname,i,lerr)
if (lerr) then
  nError   = RD_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Error reading sampler material'
  eInform  = 'File= '//TRIM(smpfile)
  go to 9999
end if
call cupper(matname)

!----- Get material subgroup

if(nch > 0)then
  read(string,*,iostat=ios)isg
  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Error reading sampler subgroup'
    eInform  = 'File= '//TRIM(smpfile)
    go to 9999
  end if
else
  isg = 0
end if

!------ Get sampler locations

is = 1
do while (.true.)
  read(lun_tmp,*,iostat=ios,end=101) xs,ys,zs
  if (is <= MAXSMP) then
    xsmp(is) = xs
    ysmp(is) = ys
    if (lter) then
      call get_topog(xs,ys,hz,hx,hy)
    else
      hz = 0.
    end if
    zsmp(is) = zs + hz
  else
    nError   = SZ_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Too many sampler locations'
    write(eInform,'(a,i5)') 'Maximum number is ',MAXSMP
    eAction  = 'File= '//TRIM(smpfile)
    go to 9999
  end if
  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Error reading sampler locations'
    eInform  = 'File= '//TRIM(smpfile)
    go to 9999
  end if
  is = is + 1
end do
101     nsmp = is - 1

close(lun_tmp,iostat=ios)

!------ Find material types

imat = -1
do i = 1,ntypm
  if (material(i)%cmat == matname) then
    imat = i
  end if
end do

if (imat == -1) then
  nError   = UK_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Sampler material not found in materials list'
  write(eInform,'(a,a)') 'Material requested was ',TRIM(matname)
  eAction  = 'Make sure material is in materials list'
  go to 9999
end if

nch = nblank(matname)
string = 'Sampler time histories for '//matname(1:nch)
nch = nblank(string)

!------ Set subgroups

if (isg > 0) then

  itys = material(imat)%ioffp + isg
  itye = itys

  if (IsGas(material(imat)%icls)) then
    if (isg /= 1) then
      nError   = IV_ERROR
      eRoutine = 'read_smp'
      eMessage = 'Invalid subgroup for sampler output'
      write(eInform,'(a,a)') 'Material requested was ',TRIM(matname)
      eAction  = 'Gas material subgroup must be 0 or 1'
      go to 9999
    end if
  else if (IsParticle(material(imat)%icls)) then
    if (isg > sub_groups(material(imat),mat_aux)) then
      nError   = IV_ERROR
      eRoutine = 'read_smp'
      eMessage = 'Invalid subgroup for sampler output'
      write(eInform,'(a,a)') 'Material requested was ',TRIM(matname)
      eAction  = 'Particle material subgroup exceeds number of bins'
      go to 9999
    else
      string = string(1:nch)//' subgroup '
      write(string(nch+11:),'(i3)') isg
    end if
  end if

else if (isg == 0) then

  string = string(1:nch)//' total'
  itys = material(imat)%ioffp + 1
  itye = material(imat)%ioffp + &
                       max(1,sub_groups(material(imat),mat_aux))

else

  nError   = IV_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Invalid subgroup for sampler output'
  write(eInform,'(a,a)') 'Material requested was ',TRIM(matname)
  eAction  = 'Subgroup must be 0 or positive'
  go to 9999

end if

nch  = nblank(string)
nchp = nblank(name)
string = string(1:nch)//' for project '//name(1:nchp)
nvarsmp = 4*nsmp
lmcsmp  = .false.
if(IsMulti(material(imat)%icls))then
  nvarsmp = nvarsmp + (nspectot+ncorrt)*nsmp
  lmcsmp = .true.
end if

!------ Output file

if (restart) then
  call set_smp
else
  call create_smp(string)
end if

9999    return

end

subroutine create_smp(stitle)
!*******************************************************************************
!
! FUNCTION:  Initialize sampler output file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          write_smp_name
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_smp
use files_inc
use multcomp_inc

implicit none

! --- ARGUMENTS
 
character*(*) stitle  ! title of sampler file (written in header)

! --- PARAMETERS
 
integer, parameter :: NUMB_OFFSET = 48, ALPH_OFFSET = 55

! --- LOCALS

character*16 xnam(10)
character*3 stmp
integer     ios

integer   i1, i, j, nch, nblank, isp, ir, iA, iB
integer   base, base2, nb(3), ncha, nchb,ic

!------ Open sampler output file

open(unit=lun_smp,file=file_smp, status='NEW', iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'create_smp'
  eMessage = 'Error opening sampler output file'
  eInform  = 'File= '//TRIM(file_smp)
  go to 9999
end if

!------ Write sampler output file header

write(lun_smp,*,iostat=ios) nvarsmp+1
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'create_smp'
  eMessage = 'Error writing sampler output file'
  eInform  = 'File= '//TRIM(file_smp)
  go to 9999
end if
if(nsmp < 10**3)then                                          !Decimal numbering
  base = 10
else if(nsmp < 16**3)then                                     !Hexidecimal numbering
  base = 16
else                                                             !AlphaNumeric numbering
  base = 36
end if
base2 = base*base
xnam(1) = 'T'
i1 = 1
do i = 1,nsmp
  nb(1) = i/base2
  nb(2) = (i-nb(1)*base2)/base
  nb(3) = (i-nb(1)*base2-nb(2)*base)
  do j = 1,3
    if(nb(j) < 10)then
      stmp(j:j) = char(nb(j)+NUMB_OFFSET)
    else
      stmp(j:j) = char(nb(j)+ALPH_OFFSET)
    end if
  end do
  i1 = i1 + 1
  xnam(i1)='C'//stmp
  if (i1 == 5) call write_smp_name(i1,xnam)
  i1 = i1 + 1
  xnam(i1)='V'//stmp
  if (i1 == 5) call write_smp_name(i1,xnam)
  i1 = i1 + 1
  xnam(i1)='S'//stmp
  if (i1 == 5) call write_smp_name(i1,xnam)
  i1 = i1 + 1
  xnam(i1)='T'//stmp
  if (i1 == 5) call write_smp_name(i1,xnam)
  if (lmcsmp) then
    do isp = 1, nspectot
      i1 = i1 + 1
      nch = min(13,nblank(species(isp)%name))
      xnam(i1) = species(isp)%name(1:nch)//stmp
      if (i1 == 5) call write_smp_name(i1,xnam)
    end do
    do ic = 1, ncorrt
      i1 = i1 + 1
      ir = indx_corr(ic)
      iA = reaction(ir)%iA
      ncha = min(2,nblank(species(iA)%name))
      iB = reaction(ir)%iB
      nchb = min(3,nblank(species(iA)%name))
      xnam(i1) = species(iA)%name(1:ncha)//species(iB)%name(1:nchb)//stmp
      if (i1 == 5) call write_smp_name(i1,xnam)
    end do
  end if
end do
if (i1 > 0) call write_smp_name(i1,xnam)

nch = nblank(stitle)
write(lun_smp,110,iostat=ios) stitle(1:nch)
110     format(a)
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'create_smp'
  eMessage = 'Error writing sampler output file'
  eInform  = 'File= '//TRIM(file_smp)
  go to 9999
end if

9999    return
end

subroutine write_smp_name(n,xnam)
!*******************************************************************************
!
! FUNCTION:  Write to sampler name
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
use files_inc

implicit none

! --- ARGUMENTS
 
integer n             !number of names
character*16 xnam(*)   !sampler names

! --- LOCALS

integer i, ios

write(lun_smp,100,iostat=ios) (xnam(i),i=1,n)
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_smp_name'
  eMessage = 'Error writing sampler names'
  eInform  = 'File= '//TRIM(file_smp)
  go to 9999
end if
100     format(5a16)

n = 0

9999    return
end

subroutine set_smp
!*******************************************************************************
!
! FUNCTION:  Restart a sampler output file
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
use common_smp
use files_inc

implicit none

! --- LOCALS

integer ios
real tem
character*128 line

open(unit=lun_smp,file=file_smp, status='OLD', iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'set_smp'
  eMessage = 'Error opening sampler output file'
  eInform  = 'File= '//TRIM(file_smp)
  go to 9999
end if

read(lun_smp,*,err=9998) nvarsmp

line = ' '
do while (index(line,'Sampler') == 0)  
  read(lun_smp,'(A)',err=9998) line
end do

tem = -999.
do while (tem /= trst .and. trst /= 0.)
  read(lun_smp,*,iostat=ios,end=9998)tem
  if (ios /= 0) then
    go to 9998
  end if
end do

9999    return
9998    nError   = RD_ERROR
        eRoutine = 'set_smp'
        eMessage = 'Error reading sampler output file'
        eInform  = 'File= '//TRIM(file_smp)
        go to 9999
end

subroutine write_smp(cgrid)
!******************************************************************************
!
! FUNCTION:  Write to sampler output file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!   get_samp_val_allpuffs                  mapfac                 get_met
!                 get_amb                update_k         set_equilibrium
!       step_aerosol_chem                 out_smp            IsMCParticle
!
!
! REVISION HISTORY: 
!
! 26Aug03 : Change iflag to 0 for call to get_met as height is above ground - BC
! Jan 2005 : Updated for new calls to step_aerosol_chem - PK, AER
! Aug 2010 : Include following update made by PKK, AER
!             a) Updated for Oct. 2004 CMAQ release
! Aug 2011 : Updated for CMAQ 5, PK, ENVIRON
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_smp
use multcomp_inc
use interface_definitions, only: get_amb, get_samp_val_allpuffs

implicit none

! --- ARGUMENTS
 
REAL :: CGRID( :,:,:,: )  !3-D ambient species concentrations (for chemistry)

! --- LOCALS

integer i, i0, isp, ii, iaq, ir, iA, iB
real xmap, ymap, conc(MAX_MC), cAB, dum1, dum2, dum3, ppmfactor
logical, external :: IsMCParticle

! Minimum time step (for equilibrium only calculations)
REAL, PARAMETER :: DTMIN = 1.E-20

real dtzero               ! -- dummy timestep (sec)

dtzero = DTMIN

!------ zero sampler arrays

do i = 1,nvarsmp
  dsmp(i) = 0.
end do

!------ loop over samplers

istage = nstage
do i = 1,nsmp

  i0 = (i-1)*(nvarsmp/nsmp)

!------ get point concentration

  call get_samp_val_allpuffs(i,cgrid)

!------ Set time scale

  if (dsmp(i0+2) > 0.0) then
    dsmp(i0+3) = dsmp(i0+3)/dsmp(i0+2)
  end if

!------ Set plume temperature

  call  mapfac( xsmp(i) , ysmp(i) , xmap , ymap )
  call get_met( xsmp(i) , ysmp(i) , zsmp(i) , &
                           0.0 , 0.0, xmap,ymap,0,.false.)

  dsmp(i0+4) = dsmp(i0+4) + tb*(pb**0.285714) !absolute background temperature
                                              !with the plume temperature
                                              !(may be overwritten)

!------ Set multicomponent species concentrations

  if (lmcsmp) then

    tab = dsmp(i0+4)

!------ set or get ambient values

    if (lamb3d) call get_amb(xsmp(i),ysmp(i),zsmp(i),t,cgrid)   !always at STP
 
    if (nequilibrium > 0) then
      tk = NOT_SET_R
      pk = NOT_SET_R
      zk = NOT_SET_R
      hk = NOT_SET_R
      cldk = NOT_SET_R
      tb = tab*(pb**(-0.285714)) !new potential temperature (used in update_k)

      call update_k(.true.,xsmp(i),ysmp(i),zsmp(i))

      if (nError /= NO_ERROR) go to 9999
      call set_equilibrium(.true.)
      if (nError /= NO_ERROR) then
        nError   = IV_ERROR
        eRoutine = 'write_smp'
        eMessage = 'Error setting ambient equilibrium'
        go to 9999
      end if
    end if

!------ calculate aerosol equilibrium concentrations

    if (laerosol) then

! - load aerosol working array
      do iaq = 1, naero
        isp = index_aero(iaq)
        conc(iaq) = MAX(ps(isp)%a,0.)
      end do

      call step_aerosol_chem(dtzero,conc,naero,aero_names,ic_units, &
                             hb,tab,pb)
      if (nError /= NO_ERROR) go to 9999

    end if

!------ add ambient to species concentrations and set equilibrium species

    ppmfactor = tab/(298.*pb)    !factor to go from STP to actual T and P
    do isp = 1, nspecies
      if (IsMCParticle(isp)) then
        dsmp(i0+4+isp) = dsmp(i0+4+isp) + ps(isp)%a !total particle conc
      else
        dsmp(i0+4+isp) = ppmfactor*(dsmp(i0+4+isp) + ps(isp)%a) !total species conc
      end if
    end do

    if (laerosol) then
      do iaq = 1, naero
        isp = index_aero(iaq)
        if (isp <= nspectot) then
          if (IsMCParticle(isp)) then
            dsmp(i0+4+isp) = dsmp(i0+4+isp) - ps(isp)%a + conc(iaq)  !replace with aerosol equil
          else
            dsmp(i0+4+isp) = dsmp(i0+4+isp) - ppmfactor*(ps(isp)%a - conc(iaq))
          end if  
        end if
      end do
    end if

!------ calculate equilibrium concentrations

    if (nequilibrium > 0) then

      do isp = 1, nspecies
        if (IsMCParticle(isp)) then
          ps(isp)%c = dsmp(i0+4+isp) - ps(isp)%a    !save pert values to set equilibrium
        else
          ps(isp)%c = dsmp(i0+4+isp)/ppmfactor - ps(isp)%a    !save pert values to set equilibrium
        end if
      end do

      call set_equilibrium(.false.)
      if (nError /= NO_ERROR) then     !use masses for equilibrium conc
        nError = NO_ERROR
        !do isp = 1, nequilibrium
        !  ii = isp + nspecies
        !  dsmp(i0+4+ii) = dsmp(i0+4+ii) + ps(ii)%a)
        !end do
      else                             !use result from set_equilibrium
        do isp = 1, nequilibrium
          ii = isp + nspecies
          dsmp(i0+4+ii) = ppmfactor*(ps(ii)%c + ps(ii)%a)
        end do
      end if

    end if

!------ calculate aerosol equilibrium concentrations

    if (laerosol) then

! - load aerosol working array
      do iaq = 1, naero
        isp = index_aero(iaq)
        if (isp <= nspectot) then
          conc(iaq) = MAX(dsmp(i0+4+isp),0.)
        else
          conc(iaq) = MAX(ps(isp)%a,0.)
        end if
      end do

      call step_aerosol_chem(dtzero,conc,naero,aero_names,ic_units, &
                             hb,tab,pb)
      if (nError /= NO_ERROR) go to 9999

! - unload aerosol working array
      do iaq = 1, naero
        isp = index_aero(iaq)
        if (isp <= nspectot) dsmp(i0+4+isp) = conc(iaq)
      end do

    end if

! - do not output negatives - disabled
!    do isp = 1, nspectot
!      dsmp(i0+4+isp) = max(0.,dsmp(i0+4+isp))
!    end do
    do isp = 1, ncorrt
      ir = indx_corr(isp)
      iA = reaction(ir)%iA
      iB = reaction(ir)%iB
      cAB = -dsmp(i0+4+iA)*dsmp(i0+4+iB)
      dsmp(i0+4+nspectot+isp) = max(cAB,dsmp(i0+4+nspectot+isp))
    end do

  end if  ! end of multicomponents

end do  ! end of samplers

call out_smp

9999    return
end

subroutine get_samp_val_allpuffs(is,cgrid)
!******************************************************************************
!
! FUNCTION:  Get instantaneous concentration at sampler "is"
!            Include contribution from all puffs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_puff_val
!
! REVISION HISTORY: 
! Aug 2010  : Include following update made by PKK, AER
!             a) Updated for Oct. 2004 CMAQ release - BC(SAGE-MGT)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_smp
use interface_definitions, only: get_puff_val

implicit none

! --- ARGUMENTS
 
integer is  ! sampler index
REAL :: CGRID( :,:,:,: )  !3-D ambient species concentrations (for chemistry)

! --- LOCALS

integer jpuf

do jpuf = 1,npuf

  if(puff(jpuf)%ityp >= itys .and. &
         puff(jpuf)%ityp <= itye)then
    call get_puff_val(is,puff(jpuf),cgrid)  ! get contribution from jpuf
  end if

end do

return
end

subroutine limint2(x,d,i1,i2,nx)
!*******************************************************************************
!
! FUNCTION:  Find the indices that are + and - 3 grid points away from x
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
  
implicit none

! --- ARGUMENTS
 
integer i1, i2 !indices
integer nx     !max number of points

real    x, d   !puff centroid and grid size

! --- PARAMETERS
 
integer, parameter :: N = 3

! --- LOCALS

integer m

m = 0
do while (x-m*d >= 0. .and. m <= N)
  i1 = -m
  m  = m + 1
end do

m = 0
do while (x+m*d <= float(nx) .and. m <= N)
  i2 = m
  m  = m + 1
end do

return
end

subroutine get_puff_val(is,p,cgrid)
!******************************************************************************
!
! FUNCTION:  Find contribution to concentration at sampler "is" from puff "p"
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_amb         get_dynamics    get_mc           mapfac
!            get_topog       get_asig        grnd_reflect     zi_reflect
!
!
! REVISION HISTORY: 
! Aug 2010  : Include following update made by PKK, AER
!             a) Updated for Oct. 2004 CMAQ release - BC(SAGE-MGT)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use common_smp
use interface_definitions, only: get_amb

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !puff structure
integer is            !sampler index
REAL :: CGRID( :,:,:,: )  !3-D ambient species concentrations (for chemistry)

! --- PARAMETERS
 
real, parameter :: ARGMAX = 20.0, SPAR   = 0.3

! --- LOCALS

type ( puff_dynamics ) pd
type ( puff_mc ) pm

real xr(3), xnrm(3)
real cfac, ccoc, ccfac, slfac, xmap, ymap, xbar, ybar, vbar
real deth, rat, hz, hx, hy, zp, znrm, zfac, xs, ys, vs
real xp, yp, faci, arg, zr, facs, fac
real cfac_mc(MAX_MC), cstar(MAX_MC)
real r, facspec, tfac
real asig(7),tsig(6)
real alpha,beta,args,s
real xn,yn,zn,dy0

integer iv, nmc, isp, ioff, i
integer ir, iA, iB, jA, jB

logical lset

!------ calculate cdep at grid locations and increment dose

vol  = pi3*sqrt(p%det)
cfac = p%cfo*p%c/vol
if (dynamic) then
  call get_dynamics(p,pd)
  tfac = pd%ctp/p%c
else
  tfac = 0.
end if
ccoc = 0.

nmc   = typeID(p%ityp)%nmc
if (nmc > 0) then
  call get_mc(p,pm)
  ioff = nspectot + ncorrm + ncorrt
  do isp = 1, nspectot
    cfac_mc(isp) = pm%mc(isp)/vol
    cstar(isp)   = pm%mc(isp+ioff)
  end do
  if (ncorrt > 0) then
    ioff = nspectot
    do i = 1,ncorrt
      ir = indx_corr(i)
      iA = reaction(ir)%iA
      iB = reaction(ir)%iB
      jA = species(iA)%star
      jB = species(iB)%star
      cfac_mc(i+nspectot) = ( pm%mc(i+ioff) - &
         ( 0.5*(pm%mc(iA)*pm%mc(jB) + pm%mc(iB)*pm%mc(jA)) ) )/vol   !A'B'
    end do
  end if
end if

ccfac = (p%cfo**2)*(p%cc-p%ccb)/vol
ccfac = max( ccfac , (ccoc*cfac)**2 )
!slfac = p%si*ccfac
slfac = p%sr*ccfac

call mapfac( p%xbar , p%ybar , xmap , ymap )

xbar  = p%xbar
ybar  = p%ybar
vbar  = p%zbar

if (lamb3d)call get_amb(xbar,ybar,vbar,t,cgrid)

deth = p%axx*p%ayy - p%axy**2
rat  = 0.5/(p%det*deth)
call get_asig(p,asig)

iv = (nvarsmp/nsmp)*(is-1) + 1
vs = zsmp(is)
zp = (vs-vbar)
call zi_reflect(vbar,p%zc,p%zc,vs,rat,faci)
xs = xsmp(is)
xp = (xs-xbar)/xmap
ys = ysmp(is)
yp = (ys-ybar)/ymap

if (lter) then
  call get_topog(p%xbar,p%ybar,hz,hx,hy)
  call grnd_reflect(vbar-hz,asig,hx,hy,xr,xnrm,deth,znrm)
  zfac = 0.5*znrm/(p%det*deth)
  call get_topog(xs,ys,hz,hx,hy)
end if

if (lter) then
  lset =  hz <= vs
else
  lset = .true.
end if
if (lset) then
  arg = p%axx*xp*xp+2.*p%axy*xp*yp+2.*p%axz*xp*zp &
         + p%ayy*yp*yp+2.*p%ayz*yp*zp+p%azz*zp*zp
  if ( arg < ARGMAX )then
    if (lter) then
      zr   = xnrm(1)*(xp-xr(1)) + xnrm(2)*(yp-xr(2)) &
                 + xnrm(3)*(zp-xr(3))
      zr   = max(zr,0.)
      facs = exp(zfac*zr)
    else 
      facs = exp(-vs*vbar*rat)
    end if
    fac  = exp(-arg)*(1.+facs)*(1.+faci)
    dsmp(iv)   = dsmp(iv)   + fac*cfac
    dsmp(iv+1) = dsmp(iv+1) + fac*ccfac
    dsmp(iv+2) = dsmp(iv+2) + fac*slfac
    dsmp(iv+3) = dsmp(iv+3) + fac*tfac
    if (nmc > 0) then
      call puff_rot(xp,yp,zp,p%uo,p%vo,asig,tsig,xn,yn,zn,dy0)
      do isp = 1, nspectot
        facspec = fac  
        if (isp <= nspecies ) then             
          if (species(isp)%star /= 0 .and. ps(isp)%a /= 0.) then
            r    =  amin1(1.,cstar(isp)/ps(isp)%a)
          else
            r = 1.0
          end if
        else 
          if (pm%mc(isp) >= 0.0) then
            r = 1.0
          else
            r = 0.0
          end if
        end if
        if (r < 1.0) then
          s = 1.0 + SPAR*(1.0 - r)
          alpha = 0.5*s*(s**2 + 1.0)
          beta  = s*(s**2 - 1.0)
          args  = tsig(1)*xn*xn &
                + 2.*tsig(2)*xn*(dy0+s*(yn-dy0))+2.*tsig(3)*xn*zn &
                + tsig(4)*(dy0+s*(yn-dy0))**2 &
                + 2.*tsig(5)*(dy0+s*(yn-dy0))*zn + tsig(6)*zn*zn
          facspec = (alpha + beta*(tsig(4)* &
                   (s*(yn-dy0))**2 -1.0))*exp(-args)*(1.+facs)*(1.+faci)            
        end if
        dsmp(iv+3+isp) = dsmp(iv+3+isp) + facspec*cfac_mc(isp)
      end do
      ioff = iv+3+nspectot
      do i = 1, ncorrt
        dsmp(ioff+i) = dsmp(ioff+i) + fac*cfac_mc(i+nspectot)
      end do
    end if ! (nmc > 0)
  end if   ! (arg < ARGMAX)
end if     ! (lset)

return

end

subroutine out_smp
!*******************************************************************************
!
! FUNCTION:  Output the sampler data
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
! 22Mar06 : Change the output format to explicit using custom string instead of implicit
!           format. This is done as the record length is limited in Portland compiler.-BC
! 25Jan07 : Add logical lWrap to write wrapped/single line output.
!           lWrap is true only if run using GUI, due to Absoft 7.5 I/O issues.-BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_smp
use files_inc

implicit none
 
! --- LOCALS

integer ios
integer is
character*64 fstring

if( lWrap )then

  write(lun_smp,*,iostat=ios) t,(dsmp(is),is=1,nvarsmp)
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'out_smp'
    write(eMessage,'("error writing wrapped sampler output file with ios =",I6)')ios
    eInform  = 'File= '//TRIM(file_smp)
    go to 9999
  end if

else

  write(lun_smp,fmt='(ES12.4,1x)',advance='no',iostat=ios) t
  do is = 1,nvarsmp-1
    if (ios /= 0 )exit
    write(lun_smp,fmt='(ES12.5,1x)',advance='no',iostat=ios)dsmp(is)
  end do
  is = nvarsmp
  if (ios == 0 )write(lun_smp,fmt='(ES12.5,1x)',advance='yes',iostat=ios)dsmp(is)

  if( ios /= 0 )then
    nerror   = wr_error
    eroutine = 'out_smp'
    write(eMessage,'("error writing sampler output file with ios =",I6)')ios
    einform  = 'file= '//trim(file_smp)
    goto 9999
  end if

end if

9999    return
end

!=======================================================================

subroutine get_asig(p,asig)

use common_puf

type ( puff_str ) p

real asig(7)

asig(1) = p%axx
asig(2) = p%axy
asig(3) = p%axz
asig(4) = p%ayy
asig(5) = p%ayz
asig(6) = p%azz
asig(7) = p%det

return
end

!==============================================================================

subroutine puff_rot(xp,yp,zp,uo,vo,asig,tsig,xn,yn,zn,dy0)

implicit none

real  xp,yp,zp,uo,vo,xn,yn,zn,dy0
real asig(7),tsig(6)
real b(3,3),a(3,3),at(3,3),c(3,3),x(3),xr(3)

real sp,cn,sn

b(1,:) = asig(1:3)
b(2,2) = asig(4)
b(2,3) = asig(5)
b(3,3) = asig(6)
b(2,1) = b(1,2)
b(3,1) = b(1,3)
b(3,2) = b(2,3)

sp = sqrt(uo*uo +vo*vo)
if (sp /= 0.) then
  cn = uo/sp
  sn = vo/sp
else
  cn = 1.0
  sn = 0.0
end if

a = 0.0
a(1,1) = cn
a(1,2) = sn
a(2,1) = -sn
a(2,2) = cn
a(3,3) = 1.

call trnsps(a,at)
call xmatmul(a,b,c)
call xmatmul(c,at,b)
x(1) = xp
x(2) = yp
x(3) = zp
call matvmul(a,x,xr)
xn = xr(1)
yn = xr(2)
zn = xr(3)

if (b(2,2) /= 0.) then
  dy0 = - (b(1,2)*xn + b(2,3)*zn)/b(2,2)
else
  dy0 = 0.0
end if

tsig(1:3) = b(1,:)
tsig(4)   = b(2,2)
tsig(5)   = b(2,3)
tsig(6)   = b(3,3)

return
end
