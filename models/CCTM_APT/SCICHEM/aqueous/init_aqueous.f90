!*********************************************************************** 
! This is the SCICHEM initialization routine for the RADM aqueous-phase*
! chemistry module in CMAQ (October 2004 version)                      *
! This code has been developed for Southern Company Services under     *
! subcontract to EPRI, 3412 Hillview Ave., Palo Alto, CA 94304         *
! Contract EP-P14638/C7185                                             * 
!                                                                      * 
! Developed by Prakash Karamchandani                                   * 
! Atmospheric and Environmental Research, Inc., 2682 Bishop Drive,     * 
! Suite 120, San Ramon, CA 94583                                       * 
!                                                                      * 
! Revisions:                                                           *
!    August 2011: Updated for CMAQ 5 beta,  PK, ENVIRON                *
!    March  2012: Updated for CMAQ 5 final, PK, ENVIRON                *
!*********************************************************************** 
subroutine init_aqueous(name,nsp,lun,file_imc,index_aqueous, &
                        naqchem,naqueous,aqueous_names)

!====  Initialize aqueous module for multi-component runs
!      This routine is called by init_mcp, which is called in start via
!      read_mc and directly from restart
!
!      This version is compatible with the CMAQ 2004 aerosol/aqueous modules
!      (modal approach)
!
!      Version 1.0, by Prakash Karamchandani, January 2005
!      Atmospheric and Environmental Research, Inc.
!      2682 Bishop Drive, Suite 120, San Ramon, CA 94583  
!
!      Updated Feb 2008 for CMAQ 4.6 and Hg treatment
!      Updated November 2010 by PK, ENVIRON for consistency with
!      CMAQ 4.7.1                                       
!      Updated August 2011 by PK, ENVIRON for CMAQ 5.0 beta
!      Updated March 2012  by PK, ENVIRON for CMAQ 5.0 final
!***********************************************************************

use error_inc
use aqueous_species_inc

implicit none

character*(*) name(*), file_imc, aqueous_names(*)
integer nsp,lun,index_aqueous(*)
integer naqchem,naqueous

integer i, j, spc ! loop variables
character*16 scav_name

! Initialize required species for aqueous module

! First do species participating in aqueous chemistry
! Reactive gases
spc = 0
do i = 1, n_gc_g2aq
  spc = spc + 1
  aqueous_names(spc) = gc_spc(gc_g2aq_map(i))
end do

! Next do particle species
do i = 1, n_ae_a2aq
  spc = spc + 1
  aqueous_names(spc) = ae_spc(ae_a2aq_map(i))
end do

! Next do non-reactive gases and tracers
do i = 1, n_nr_n2aq
  spc = spc + 1
  aqueous_names(spc) = nr_spc(nr_n2aq_map(i))
end do
do i = 1, n_tr_t2aq
  spc = spc + 1
  aqueous_names(spc) = tr_spc(tr_t2aq_map(i))
end do
  
naqchem = spc
!debug
write(*,*)'No. of aqueous chemistry species: ',naqchem
write(*,*)'species names: '
do i = 1, naqchem
  write(*,*)'spc, name: ',i,aqueous_names(i)
end do
!debug

! Next do unique scavenged species (i.e., those not participating in
! aqueous chemistry)
! Reactive gases
outer1: do i = 1, n_gc_scav
  scav_name = gc_spc(gc_scav_map(i))

! Check if this species is already in list
  do j = 1, naqchem
    if (TRIM(aqueous_names(j)) == TRIM(scav_name)) CYCLE outer1
  end do
  spc = spc + 1
  aqueous_names(spc) = scav_name
end do outer1

! Particles
outer2: do i = 1, n_ae_scav
  scav_name = ae_spc(ae_scav_map(i))

! Check if this species is already in list
  do j = 1, naqchem
    if (TRIM(aqueous_names(j)) == TRIM(scav_name)) CYCLE outer2
  end do
  spc = spc + 1
  aqueous_names(spc) = scav_name
end do outer2

! Non-reactive gases
outer3: do i = 1, n_nr_scav
  scav_name = nr_spc(nr_scav_map(i))

! Check if this species is already in list
  do j = 1, naqchem
    if (TRIM(aqueous_names(j)) == TRIM(scav_name)) CYCLE outer3
  end do
  spc = spc + 1
  aqueous_names(spc) = scav_name
end do outer3

! Tracers
outer4: do i = 1, n_tr_scav
  scav_name = tr_spc(tr_scav_map(i))

! Check if this species is already in list
  do j = 1, naqchem
    if (TRIM(aqueous_names(j)) == TRIM(scav_name)) CYCLE outer4
  end do
  spc = spc + 1
  aqueous_names(spc) = scav_name
end do outer4

naqueous   = spc

!debug
write(*,*)'Total no. of aqueous species, naqueous: ',naqueous
write(*,*)'species names (1-naqchem): '
do i = 1, naqchem
  write(*,*)'spc, name: ',i,aqueous_names(i)
end do
write(*,*)'species names (naqchem+1,naqueous): '
do i = naqchem+1, naqueous
  write(*,*)'spc, name: ',i,aqueous_names(i)
end do
!debug

! ---  identify all species that are passed to aqueous module     
outer5: do j = 1,naqueous
  do i = 1,nsp
    if (TRIM(name(i)) == TRIM(aqueous_names(j))) then
      index_aqueous(j)    = i        !aqueous module species pointer
      CYCLE outer5 ! search for next species
    end if
  end do
  nError   = IV_ERROR
  eRoutine = 'init_aqueous'
  eMessage = 'Necessary species for aqueous module: ' // &
             TRIM(aqueous_names(j)) // ',  not found in imc file'
  eInform  = 'File='//TRIM(file_imc)
  go to 9999
end do outer5

! below is redundant since check is above
! kept for the time being for the debug statement in the loop
do j = 1, naqueous
  if ( index_aqueous(j) == 0 ) then
    nError   = IV_ERROR
    eRoutine = 'init_aqueous'
    eMessage = 'Necessary species for aqueous module: ' // &
               TRIM(aqueous_names(j)) // ',  not found in imc file'
    eInform  = 'File='//TRIM(file_imc)
    go to 9999
  end if
!debug
  write(*,*)'j, index: ',j,index_aqueous(j)
!debug
end do

!debug
write(*,*)'init_aqueous'
do j = 1, naqueous
   write(*,*)'Aqueous species, index-local, model species: ', &
             TRIM(aqueous_names(j)),index_aqueous(j), &
             TRIM(name(index_aqueous(j)))
end do
!debug

9999  return
      end
