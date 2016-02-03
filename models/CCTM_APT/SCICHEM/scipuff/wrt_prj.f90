!*******************************************************************************
!$RCSfile: wrt_prj.f90,v $
!$Revision: 1.4 $
!$Date: 2010/10/28 20:13:41 $
!*******************************************************************************
subroutine write_prj
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Write to the project file (ProjectName.prj)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              init_error           write_prj_hdr         write_prj_const
!          write_prj_turb          write_prj_pprm          write_prj_pgrd
!          write_prj_pmtl          write_prj_smtl         write_prj_cntrl
!           write_prj_ext            write_prj_mc           write_prj_ter
!          write_prj_xmap            write_prj_zb        write_prj_latlon
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- LOCALS

integer ios, itry


itry = 0
100     itry = itry + 1

open(unit=lun_prj,file=file_prj,status='UNKNOWN',form='UNFORMATTED', &
          iostat=ios)
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'write_prj'
  eMessage = 'Error opening SCICHEM project file'
  eInform  = 'File='//TRIM(file_prj)
!         eAction  = 'Make sure file does not already exist'
  write(eAction,*)'IOSTAT=',ios
!         go to 9999
if(itry < 10)then
  write(11,*)'*********** OPEN ERROR'
  write(11,*)itry,ios
  call init_error
  go to 100
else
  go to 9999
end if
end if

!debug
write(*,*)'writing project header'
call flush(6)
!debug
call write_prj_hdr
!debug
write(*,*)'finished writing project header; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project const'
call flush(6)
!debug
call write_prj_const
!debug
write(*,*)'finished writing project const; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project turb'
call flush(6)
!debug
call write_prj_turb
!debug
write(*,*)'finished writing project turb; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project pprm'
call flush(6)
!debug
call write_prj_pprm
!debug
write(*,*)'finished writing project pprm; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project pgrd'
call flush(6)
!debug
call write_prj_pgrd
!debug
write(*,*)'finished writing project pgrd; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project pmtl'
call flush(6)
!debug
call write_prj_pmtl
!debug
write(*,*)'finished writing project pmtl; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project smtl'
call flush(6)
!debug
call write_prj_smtl
!debug
write(*,*)'finished writing project smtl; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project cntrl'
call flush(6)
!debug
call write_prj_cntrl
!debug
write(*,*)'finished writing project cntrl; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project ext'
call flush(6)
!debug
call write_prj_ext
!debug
write(*,*)'finished writing project ext; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

if (multicomp) then
!debug
write(*,*)'writing project mc'
call flush(6)
!debug
  call write_prj_mc
!debug
write(*,*)'finished writing project mc; nerror = ',nError
call flush(6)
!debug
  if (nError /= NO_ERROR) go to 9999
end if

!debug
write(*,*)'writing project ter'
call flush(6)
!debug
if (lter) then
  call write_prj_ter
!debug
write(*,*)'finished writing project ter; nerror = ',nError
call flush(6)
!debug
  if (nError /= NO_ERROR) go to 9999
end if

!debug
write(*,*)'writing project xmap'
call flush(6)
!debug
call write_prj_xmap
!debug
write(*,*)'finished writing project xmap; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project zb'
call flush(6)
!debug
call write_prj_zb
!debug
write(*,*)'finished writing project zb; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

!debug
write(*,*)'writing project latlon'
call flush(6)
!debug
call write_prj_latlon
!debug
write(*,*)'finished writing project zb; nerror = ',nError
call flush(6)
!debug
if (nError /= NO_ERROR) go to 9999

close(unit=lun_prj,iostat=ios)

9999    continue

return

end


subroutine write_prj_hdr
!*******************************************************************************
!
! FUNCTION:  Write the header of the project file
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

write(lun_prj,iostat=ios) iversion,name,nch_n,names,title &
                              ,audit_class &
                              ,audit_analyst &
                              ,audit_date &
                              ,audit_version &
                              ,audit_space

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_hdr'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end


subroutine write_prj_const
!*******************************************************************************
!
! FUNCTION: Write the constants to the project file
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

write(lun_prj,iostat=ios) pi,pi2,pi3,pi180,sphfac,sphfacr &
                         ,g0,gt,fcor0,rhoair,rmuair,rnu,rhocp

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_const'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end

subroutine write_prj_turb
!*******************************************************************************
!
! FUNCTION: Write the turbulence parameters to the project file
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

write(lun_prj,iostat=ios) a,b,bs,vonk,cvrtx,cqb &
                          ,csi1,csi2,wwtrop,sltrop,epstrop,sle_fac &
                          ,uu_calm,sl_calm

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_turb'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end

subroutine write_prj_pprm
!*******************************************************************************
!
! FUNCTION:  Write the puff parameters to the project file
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

write(lun_prj,iostat=ios) rrmrge,simrge,cmin,delmin,asplt,asplt2 &
                          ,aspltc,dxsplt,dzsplt,delx2,delz2,fac_rfl

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_pprm'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end


subroutine write_prj_pgrd
!*******************************************************************************
!
! FUNCTION:  Write the puff grid to the project file
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

write(lun_prj,iostat=ios) xmin,xmax,ymin,ymax,zmax,mgrd &
                          ,hres,vres,dxg,dyg,dzg,nx,ny,nz &
                          ,lon0,lat0,xref,yref

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_pgrd'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end


subroutine write_prj_pmtl
!*******************************************************************************
!
! FUNCTION:  Write the puff materials to the project file
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

integer ios, i, igas, iprt

nclass  = MAXCLS
igas = -999
iprt = -999
mxsgp   = MAXSGP
write(lun_prj,iostat=ios) nclass,igas,iprt,ntypm,ntypp,mxsgp &
                          ,nmaux &
                          ,(material(i),i=1,ntypm) &
                          ,(mat_aux(i),i=1,nmaux) &
                          ,(typeID(i),i=1,ntypp) &
                          ,(namec(i),i=1,MAXCLS)
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_pmtl'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end


subroutine write_prj_smtl
!*******************************************************************************
!
! FUNCTION:  Write surface material information to the project file
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

write(lun_prj,iostat=ios) ntyps, ntypd

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_smtl'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
  go to 9999
end if

9999    continue

return

end


subroutine write_prj_cntrl
!*******************************************************************************
!
! FUNCTION: Write control parameters to the project file
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

integer ios

write(lun_prj,iostat=ios) t_avg,hascal,lter,lmap,local,lsplitz &
                          ,lzi_prj,lymd,dose,surface,tzone,jul_start &
                          ,year_start,month_start,day_start &
                          ,year_end,month_end,day_end &
                          ,tstart,tend,tend_hr &
                          ,delt,t_save,dt_save,t_old_r

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_cntrl'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end


subroutine write_prj_ext
!*******************************************************************************
!
! FUNCTION:  Write extensions for new versions
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

integer ios

write(lun_prj,iostat=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpfile, utm_zone, static &
                          ,multicomp,hazard,run_mode
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_ext'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end


subroutine write_prj_mc
!*******************************************************************************
!
! FUNCTION:   Write multicomponent parameters for new versions
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! 23 MAY 2005 :Added release list pointers to account for moving equilibrium species
!              to end of species list (RIS)
!*******************************************************************************
 
! --- MODULES
 
! --- ARGUMENTS
 
! --- LOCALS
use common_puf
use multcomp_inc
use files_inc

implicit none


real ktmp(MAX_ZENITH*MAX_KTAB)
integer ios,i,j,ij

! -- fix for the SUN, copy ktable to tmp array

do i = 1, nzenith
  do j = 1, nkrad
    ij = (i-1)*nkrad + j
    ktmp(ij) = ktable(i,j)
  end do
end do

write(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, isolve, lstep_amb,  &
                   amb_file, lstage, lchem_split, laqueous, &
                   laerosol, ldump_chem, param_chem, &
                   rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot), &
                  (i_hostspec_list(i),i=1,nspectot),&
                  (i_eq_list(i),i=1,nequilibrium), &
                  (i_rel_list(i),i=1,nspectot)

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_mc'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end

subroutine write_prj_ter
!*******************************************************************************
!
! FUNCTION:  Write the terrain to the project file
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

integer ios, i


write(lun_prj,iostat=ios) nxb,nyb,dxb,dyb &
                           ,(xb(i),i=1,nxb),(yb(i),i=1,nyb) &
                           ,lter_prj,lswift_prj

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_ter(1)'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

if (lter_prj) then

  nxyb = nxb*nyb

  write(lun_prj,iostat=ios) (hs(i),i=1,nxyb), (ddx(i),i=1,nxyb) &
                               ,(ddy(i),i=1,nxyb),hmin

  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'write_prj_ter(2)'
    eMessage = 'Error writing project file'
    eInform  = 'File='//TRIM(file_prj)
  end if

end if

return

end
