subroutine init_aero(name,nsp,lun,file_imc,index_aero,naero,naerp,aero_names)

!====  Initialize aerosol module for multi-component runs
!      This routine is called by init_mcp, which is called in start via
!      read_mc and directly from restart
!
!      Updated for CMAQ 5 beta,  PK, ENVIRON, August 2011
!      Updated for CMAQ 5 final, PK, ENVIRON, March 2012
!

use error_inc
use aero_species_inc

implicit none

! Arguments
character*(*) name(*), file_imc, aero_names(*)
integer nsp,lun,index_aero(*)
integer naero,naerp

! Locals
integer i, j, spc, imode ! loop variables

! Initialize required species for aerosol module

! First do particle species
aero_names(1:n_ae_spc) = ae_spc(1:n_ae_spc)

spc = n_ae_spc
naerp = spc
  
! Next do reactive gases
do i = 1, n_gc_g2ae
  spc = spc + 1
  aero_names(spc) = gc_spc(gc_g2ae_map(i))
end do

! Next do non-reactive gases and tracers
do i = 1, n_nr_n2ae
  spc = spc + 1
  aero_names(spc) = nr_spc(nr_n2ae_map(i))
end do
do i = 1, n_tr_t2ae
  spc = spc + 1
  aero_names(spc) = tr_spc(tr_t2ae_map(i))
end do
  
naero   = spc

!debug
write(*,*)'naero,naerp: ',naero,naerp
write(*,*)'species names: '
do i = 1, naero
  write(*,*)'spc, name: ',i,aero_names(i)
end do
!debug

! ---  identify all species that are passed to aerosol module     
outer: do j = 1,naero
  do i = 1,nsp
    if (TRIM(name(i)) == TRIM(aero_names(j))) then
      index_aero(j)    = i        !aerosol module species pointer
      CYCLE outer ! search for next species
    end if
  end do
  nError   = IV_ERROR
  eRoutine = 'init_aero'
  eMessage = 'Necessary species for aerosol module: ' // &
             TRIM(aero_names(j)) // ',  not found in imc file'
  eInform  = 'File='//TRIM(file_imc)
  go to 9999
end do outer

! below is redundant since check is above
! kept for the time being for the debug statement in the loop
do j = 1, naero
  if ( index_aero(j) == 0 ) then
    nError   = IV_ERROR
    eRoutine = 'init_aero'
    eMessage = 'Necessary species for aerosol module: ' // &
               TRIM(aero_names(j)) // ',  not found in imc file'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if
!debug
  write(*,*)'j, index: ',j,index_aero(j)
!debug
end do

!debug
write(*,*)'init_aero'
do j = 1, naero
   write(*,*)'Aero species, index-local, model species: ', &
             TRIM(aero_names(j)),index_aero(j), &
             TRIM(name(index_aero(j)))
end do
!debug

9999  return
end
