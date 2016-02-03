!*******************************************************************************
!$RCSfile: read_prj.f90,v $
!$Revision: 1.6 $
!$Date: 2010/10/31 03:25:43 $
! REVISION HISTORY: 
!                  01/31/07 : use constants from constants_fd
!*******************************************************************************
subroutine read_prj
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:   Read the project file (ProjectName.prj)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            read_prj_hdr          read_prj_const           read_prj_turb
!           read_prj_pprm           read_prj_pgrd           read_prj_pmtl
!           read_prj_smtl          read_prj_cntrl            read_prj_ext
!             read_prj_mc            read_prj_ter           read_prj_xmap
!             read_prj_zb         read_prj_latlon          set_buoy_flags
!
! REVISION HISTORY: 
!  May 2006: Separate project file for restarted run (PK, AER)
!
!******************************************************************************
 
! --- MODULES

use common_puf
use multcomp_inc
use files_inc

implicit none

! --- PARAMETERS

integer  IVERSION_READ
parameter (IVERSION_READ = 200)
 
! --- LOCALS

integer ios

open(unit=lun_prj,file=file_prjr,status='OLD',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Error opening SCICHEM project file'
  eInform  = 'File='//TRIM(file_prjr)
  eAction  = 'Make sure file exists and is a valid project file'
  go to 9999
end if

!------ read version number of project file

read(lun_prj,iostat=ios) iversion

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Error reading version number on project file'
  eInform  = 'File='//TRIM(file_prjr)
  eAction  = 'Make sure file is a valid project file'
  go to 9999
end if

rewind(lun_prj,iostat=ios)

!---- read versions 0.Vx where 0.2 < 0.Vx < current revision level

if (iversion > iversion_code) then

  nError   = UK_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Project version number is newer than code version'
  write(eInform,*)'Version = ',iversion
  eAction  = 'Use a newer version of the code to read this project'
  go to 9999

end if

if ( (iversion/100 >= IVERSION_READ/100)  .and. &
        (iversion/100 <= iversion_code/100) ) then

  call read_prj_hdr
  if (nError /= NO_ERROR) go to 9999

  call read_prj_const
  if (nError /= NO_ERROR) go to 9999

  call read_prj_turb
  if (nError /= NO_ERROR) go to 9999

  call read_prj_pprm
  if (nError /= NO_ERROR) go to 9999

  call read_prj_pgrd
  if (nError /= NO_ERROR) go to 9999

  call read_prj_pmtl
  if (nError /= NO_ERROR) go to 9999

  call read_prj_smtl
  if (nError /= NO_ERROR) go to 9999

  call read_prj_cntrl
  if (nError /= NO_ERROR) go to 9999

  call read_prj_ext
  if (nError /= NO_ERROR) go to 9999

  if (.not. multicomp) then
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
  else  
    call read_prj_mc
    if (nError /= NO_ERROR) go to 9999
  end if

  if (lter) then
    call read_prj_ter
    if (nError /= NO_ERROR) go to 9999
  end if

  call read_prj_xmap
  if (nError /= NO_ERROR) go to 9999
  
  call read_prj_zb
  if (nError /= NO_ERROR) go to 9999
  
  call read_prj_latlon
  if (nError /= NO_ERROR) go to 9999

end if

!------ set buoyant gas flags, ( .false. for all old projects)

buoy_gas = .false.
if (iversion >= 340) then
  call set_buoy_flags
end if

!---- set Error flag if 0.Vx < current revision level - unable to continue run
!     because of possible file incompatibility upon write

if (iversion/100 /= iversion_code/100) then

  nError   = VN_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Incompatible project version'
  go to 9999

end if

9999    close(unit=lun_prj,iostat=ios)

return

end


subroutine read_prj_hdr
!*******************************************************************************
!
! FUNCTION:  Read the header from the project file
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

! --- PARAMETERS
 
integer  NP_ALLV02
parameter (NP_ALLV02  = 46)

! --- LOCALS

integer ios,i
real    dum

if(iversion >= 800)then
  read(lun_prj,iostat=ios) iversion,name,nch_n,names,title &
                               ,audit_class &
                               ,audit_analyst &
                               ,audit_date &
                               ,audit_version &
                               ,audit_space
else if(iversion >= 300)then
  read(lun_prj,iostat=ios) iversion,name,nch_n, &
         (names(i),i=1,21),(names(i),i=24,NP_ALL),title &
                               ,audit_class &
                               ,audit_analyst &
                               ,audit_date &
                               ,audit_version &
                               ,audit_space
else if(iversion >= 200)then
  read(lun_prj,iostat=ios) iversion,name,nch_n, &
                           (dum,i=1,NP_ALLV02),title &
                              ,audit_class &
                               ,audit_analyst &
                               ,audit_date &
                               ,audit_version &
                               ,audit_space
else
  ios = 0
  nError   = RD_ERROR
  eRoutine = 'read_prj_hdr'
  eMessage = 'Version error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_hdr'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

return

end


subroutine read_prj_const
!*******************************************************************************
!
! FUNCTION:   Read the constants from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!                  01/31/07 : use constants from constants_fd
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none
 
! --- LOCALS

integer ios
real dum

!read(lun_prj,iostat=ios) pi,pi2,pi3,pi180,sphfac,sphfacr &
!                        ,g,gt,f0,rhoair,rmuair,rnu,rhocp
read(lun_prj,iostat=ios) dum,dum,dum,dum,dum,dum &
                        ,dum,dum,dum,rhoair,rmuair,rnu,dum
if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_const'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

return

end

subroutine read_prj_turb
!*******************************************************************************
!
! FUNCTION:  Read the turbulence parameters from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!              01/31/07 : use constants from constants_fd
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none
 
! --- LOCALS

integer ios
real    dum

!read(lun_prj,iostat=ios) a,b,bs,vonk,cvrtx,cqb &
!                        ,csi1,csi2,wwtrop,sltrop,epstrop,sle_fac &
!                        ,uu_calm,sl_calm
read(lun_prj,iostat=ios) dum,dum,dum,dum,dum,dum &
                        ,dum,dum,wwtrop,sltrop,epstrop,sle_fac &
                        ,uu_calm,sl_calm

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_turb'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

return

end

subroutine read_prj_pprm
!*******************************************************************************
!
! FUNCTION:   Read the puff parameters from the project file
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
use files_inc

implicit none

! --- LOCALS

integer ios

read(lun_prj,iostat=ios) rrmrge,simrge,cmin,delmin,asplt,asplt2 &
                        ,aspltc,dxsplt,dzsplt,delx2,delz2,fac_rfl

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_pprm'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

return

end


subroutine read_prj_pgrd
!*******************************************************************************
!
! FUNCTION:  Read the puff grid from the project file
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
use files_inc

implicit none
 
! --- LOCALS

integer ios

read(lun_prj,iostat=ios) xmin,xmax,ymin,ymax,zmax,mgrd &
                         ,hres,vres,dxg,dyg,dzg,nx,ny,nz &
                         ,lon0,lat0,xref,yref
if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_pgrd'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

return

end


subroutine read_prj_pmtl
!*******************************************************************************
!
! FUNCTION:  Read the puff materials from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!       get_puff_material       put_puff_material              set_depint
!           SetClassMulti                 IsMulti              sub_groups
!                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- LOCALS

type ( gas_material ) pmatgas

type ( puff_material ) pmat

equivalence (pmat, pmatgas )

integer ios, i, nsg, sub_groups, i0, idum

logical IsGas,IsMulti
integer SetClassMulti

read(lun_prj,iostat=ios) nclass,idum,idum,ntypm,ntypp,mxsgp,nmaux

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
  go to 9999
end if

if (nclass > MAXCLS) then
  nError   = IV_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Incorrect number of project material classes'
  write(eInform,'(a,i2,a,i2)') 'Project number is ',nclass, &
                                     'Code number is ',MAXCLS
  go to 9999
end if

if (ntypm > MAXMTYP) then
  nError   = SZ_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Too many material types on project file'
  write(eInform,'(a,i2,a,i2)') 'Project number is ',ntypm, &
                                     'Code max is ',MAXMTYP
  go to 9999
end if

if (ntypp > MAXPTYP) then
  nError   = SZ_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Too many puff types on project file'
  write(eInform,'(a,i2,a,i2)') 'Project number is ',ntypp, &
                                     'Code max is ',MAXPTYP
  go to 9999
end if

if (nmaux > MAXMAUX) then
  nError   = SZ_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Too many material auxilary variables on project file'
  write(eInform,'(a,i2,a,i2)') 'Project number is ',nmaux, &
                                     'Code max is ',MAXMAUX
  go to 9999
end if

backspace(lun_prj)

!------ read MATERIAL description data

if (iversion >= 1040) then
  read(lun_prj,iostat=ios) nclass,idum,idum,ntypm,ntypp,mxsgp &
               ,nmaux &
               ,(material(i),i=1,ntypm) &
               ,(mat_aux(i),i=1,nmaux) &
               ,(typeID(i),i=1,ntypp) &
               ,(namec(i),i=1,MAXCLS)
  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Error reading project file'
    eInform  = 'File='//TRIM(file_prjr)
    go to 9999
  end if
else if (iversion >= 510) then
  read(lun_prj,iostat=ios) nclass,idum,idum,ntypm,ntypp,mxsgp &
               ,nmaux &
               ,(material(i),i=1,ntypm) &
               ,(mat_aux(i),i=1,nmaux) &
               ,(typeID(i),i=1,ntypp) &
               ,(namec(i),i=1,MAXCLS)
  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Error reading project file'
    eInform  = 'File='//TRIM(file_prjr)
    go to 9999
  end if
  do i = 1,ntypm
    if(material(i)%nmc > 0)then
      material(i)%icls = SetClassMulti(material(i)%icls)
    end if
  end do
  do i = 1,ntypp
    if(IsMulti(material(typeID(i)%imat)%icls))then
      typeID(i)%icls = SetClassMulti(typeID(i)%icls)
    end if
  end do
else
  nError   = RD_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Version error reading project file'
  eAction  = 'Version is no longer supported'
  eInform  = 'File='//TRIM(file_prjr)
  go to 9999
end if

!------ check number of subgroups

do i = 1,ntypm
  nsg = sub_groups(material(i),mat_aux)
  if (nsg > MAXSGP) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Too many particle size groups on project file'
    write(eInform,'(a,i2,a,i2)') 'Project number is ',nsg, &
                        'Code max is ',MAXSGP
    eInform  = 'File='//TRIM(file_prjr)
    go to 9999
  end if
!====== Version prior to v0.400 did not have dense gas effects and ignored gas density
  if(iversion < 400)then
    i0 = material(i)%ioffp + 1
    if(IsGas(material(i)%icls))then
      call get_puff_material(i0,material,mat_aux,pmat)
      pmatgas%rho = rhoair
      call put_puff_material(i0,material,mat_aux,pmat)
    end if
  end if
end do

!------ set flag for surface dose integral

call set_depint

9999    continue

return

end


subroutine read_prj_smtl
!*******************************************************************************
!
! FUNCTION:  Read surface material information from project file
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

! --- LOCALS

integer ios

read(lun_prj,iostat=ios) ntyps, ntypd

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_smtl'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
  go to 9999
end if

if (ntyps+ntypd > MAXSTYP) then
  nError   = SZ_ERROR
  eRoutine = 'read_prj_smtl'
  eMessage = 'Too many surface dose/deposition fields on project file'
  write(eInform,'(a,i2,'','',i2,a,i2)') 'Project numbers are ', &
                                     ntyps,ntypd, &
                                     '   Code max is ',MAXSTYP
  go to 9999
end if

9999    continue

return

end


subroutine read_prj_cntrl
!*******************************************************************************
!
! FUNCTION:  Read control parameters from the project file
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
use common_met
use files_inc

implicit none
 
! --- LOCALS

integer ios

read(lun_prj,iostat=ios) t_avg,hascal,lter,lmap,local,lsplitz &
                             ,lzi_prj,lymd,dose,surface,tzone,jul_start &
                             ,year_start,month_start,day_start &
                             ,year_end,month_end,day_end &
                             ,tstart,tend,tend_hr &
                             ,delt,t_save,dt_save,t_old_r

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_cntrl'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
end if

return

end


subroutine read_prj_ext
!*******************************************************************************
!
! FUNCTION: Read extensions for newer versions
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               naux_matl
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
  
use common_puf
use common_met
use files_inc

implicit none

! --- LOCALS

integer ios,i,naux_matl,idum
real    dum

nzbl      = 11
nmcaux    = 0
dynamic   = .false.
dense_gas = .false.
grdmin    = 0.0
!       nsrcaux   = 0
z_dosage  = 0.0
smpfile   = ' '
utm_zone  = NOT_SET_I
static    = .false.
run_mode  = STANDARD_MODE
multicomp = .false.
hazard    = IHAZ_OFF

if (iversion >= 1100) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,z_dosage, smpfile, utm_zone, static &
                              ,multicomp,hazard,run_mode

  if (ios /= 0) go to 9999

else if (iversion >= 900) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,z_dosage, smpfile, utm_zone, static &
                              ,multicomp,hazard

  if (ios /= 0) go to 9999

else if (iversion >= 700) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,idum,(dum,i=1,idum) &
                              ,z_dosage, smpfile, utm_zone, static &
                              ,multicomp

  if (ios /= 0) go to 9999

else if (iversion >= 651) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,nmcaux,(dum,i=1,nmcaux) &
                              ,idum,(dum,i=1,idum) &
                              ,z_dosage, smpfile, utm_zone, static &
                              ,multicomp

  if (ios /= 0) go to 9999

else if (iversion >= 650) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,nmcaux,(dum,i=1,nmcaux) &
                              ,idum,(dum,i=1,idum) &
                              ,z_dosage, smpfile, utm_zone, static

multicomp = nmcaux > 0

  if (ios /= 0) go to 9999

else if (iversion >= 420) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,nmcaux,(dum,i=1,nmcaux) &
                              ,idum,(dum,i=1,idum) &
                              ,z_dosage, smpfile, utm_zone

multicomp = nmcaux > 0

  if (ios /= 0) go to 9999

else if (iversion >= 400) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,nmcaux,(dum,i=1,nmcaux) &
                              ,idum,(dum,i=1,idum) &
                              ,z_dosage, smpfile

multicomp = nmcaux > 0

  if (ios /= 0) go to 9999

else if (iversion >= 340) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,nmcaux,(dum,i=1,nmcaux) &
                              ,idum,(dum,i=1,idum) &
                              ,z_dosage

multicomp = nmcaux > 0

  if (ios /= 0) go to 9999

else if (iversion >= 310) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                              ,nmcaux,(dum,i=1,nmcaux) &
                              ,idum,(dum,i=1,idum)

multicomp = nmcaux > 0

  if (ios /= 0) go to 9999

else if (iversion >= 300) then

  read(lun_prj,iostat=ios) nzbl,dynamic,dense_gas &
                              ,nmcaux,(dum,i=1,nmcaux)

multicomp = nmcaux > 0

  if (ios /= 0) go to 9999

else if (iversion >= 230) then

  read(lun_prj,iostat=ios) nzbl

  if (ios /= 0) go to 9999

  do i = 1,ntypp
    typeID(i)%npaux = naux_matl(typeID(i)%icls, &
                                   typeID(i)%igrp, &
                                   typeID(i)%ltot)
  end do

else if (iversion >= 200) then

  do i = 1,ntypp
    typeID(i)%npaux = naux_matl(typeID(i)%icls, &
                                   typeID(i)%igrp, &
                                   typeID(i)%ltot)
  end do

else

  go to 9999

end if

9998    return

9999    nError   = RD_ERROR
eRoutine = 'read_prj_ext'
eMessage = 'Error reading project file'
eInform  = 'File='//TRIM(file_prjr)
go to 9998

end


subroutine read_prj_ter
!*******************************************************************************
!
! FUNCTION:   Read the terrain from the project file
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
use common_met
use files_inc

implicit none

! --- LOCALS

integer ios, i

read(lun_prj,iostat=ios) nxb,nyb,dxb,dyb

if (nxb > MAXXB .or. nyb > MAXYB .or. nxb*nyb > MAX2D) then
  nError   = SZ_ERROR
  eRoutine = 'read_prj_ter'
  eMessage = 'Terrain grid dimensions too large'
  eInform  = 'File='//TRIM(file_prjr)
  go to 9999
end if

backspace(lun_prj)

if (iversion >= 610) then
  read(lun_prj,iostat=ios) nxb,nyb,dxb,dyb &
                             ,(xb(i),i=1,nxb),(yb(i),i=1,nyb) &
                             ,lter_prj,lswift_prj
else
  lswift_prj = .false.
  read(lun_prj,iostat=ios) nxb,nyb,dxb,dyb &
                             ,(xb(i),i=1,nxb),(yb(i),i=1,nyb) &
                             ,lter_prj
endif

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'read_prj_ter(1)'
  eMessage = 'Error reading project file'
  eInform  = 'File='//TRIM(file_prjr)
  go to 9999
end if

if (lter_prj) then

  nxyb = nxb*nyb

  if (iversion >= 411) then
    read(lun_prj,iostat=ios) (hs(i),i=1,nxyb), (ddx(i),i=1,nxyb) &
                                 ,(ddy(i),i=1,nxyb),hmin
  else
    read(lun_prj,iostat=ios) (hs(i),i=1,nxyb), (ddx(i),i=1,nxyb) &
                                 ,(ddy(i),i=1,nxyb)
    hmin = 0.0
  end if

  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_ter(2)'
    eMessage = 'Error reading project file'
    eInform  = 'File='//TRIM(file_prjr)
    go to 9999
  end if

end if

9999    continue

return

end


subroutine set_buoy_flags
!*******************************************************************************
!
! FUNCTION:  Sets flags for buoyant gas materials (non-neutral densities)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!       get_puff_material                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- LOCALS

type ( gas_material ) pmatgas

type ( puff_material ) pmat

equivalence ( pmat, pmatgas )

logical   IsGas
integer i
real      rhog

!------ loop over puff types and check densities

if (.not. dynamic) return

do i = 1,ntypp

  buoy_flag(i) = .false.

  if(IsGas(typeID(i)%icls))then
    call get_puff_material(i,material,mat_aux,pmat)
    rhog = pmatgas%rho
    if (rhog /= rhoair) then
      buoy_gas     = .true.
      buoy_flag(i) = .true.
    end if
    buoy_fac(i)  = (rhog-rhoair)/(rhog*rhoair)
  end if

end do

return
end


subroutine set_depint
!*******************************************************************************
!
! FUNCTION:  Sets flag for integration of surface integrals
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

implicit none

! --- LOCALS

depint = .false.

return
end
