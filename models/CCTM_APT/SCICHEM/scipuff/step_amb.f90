subroutine step_amb(dt,cgrid)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Advance ambient multi-component species
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            set_amb_keqm         set_ps_from_amb                step_ynb
!                step_ode       step_aerosol_chem            step_aqueous
!         set_amb_from_ps         set_equilibrium
!
! REVISION HISTORY: 
! March 2005   : Updated for APT & Oct. 2004 CMAQ release (PKK, AER)
!                (Do nothing routine)
!******************************************************************************
 
! --- MODULES
 
!use common_puf
!use multcomp_inc
!use common_met
!use files_inc
!use amb_data
!use interface_definitions, only: SET_AMB_KEQM

implicit none

! --- ARGUMENTS
 
real dt       !Time step (sec)
REAL :: CGRID( :,:,:,: ) !3-D ambient concentration

! --- LOCALS

!real conc(MAX_MC), dum(MAX_MC), len
!integer i, is, it, it1, it2, ios, j, ix,iy,iz

!integer io3
!real o3lim

!logical leqm_set

!real dens
!real nitr1, nitr2, diffn, fac_dist(MAX_MC)
!real xamb, yamb, zamb, sflx,rat
!real hp,dum1,dum2,dp
!real,dimension(:),allocatable :: ambconc

!====   Check to see if really stepping ambients
! For plume-in-grid ambient is not stepped

!if(.not. lstep_amb) go to 9999

9999    return
end

subroutine reactq_pl_amb(conc, react, prod, loss)
!*******************************************************************************
!
! FUNCTION: Calculate quadratic reaction rate for the ambient
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
 
type (chem_reaction)react        !Reaction structure
real prod(*), loss(*), conc(*)   !Production and loss terms, species conc

! --- LOCALS

integer iA, iB, i
real    cr, kr

!====   Set local pointer

iA = react%iA
iB = react%iB

!====   Set rate

kr = react%k

!====   Adjust production and loss terms

loss(iA) = loss(iA) + kr*conc(iB)
loss(iB) = loss(iB) + kr*react%fB*conc(iA)

cr = kr*conc(iA)*conc(iB)
do i = 1,react%nP
  prod(react%iP(i)) = prod(react%iP(i)) + cr*react%fP(i)
end do

return
end

subroutine jacq_amb(react, pdm)
!*******************************************************************************
!
! FUNCTION: Calculate quadratic reaction jacobian for the ambient
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
 
type (chem_reaction)react  !Reaction structure
real pdm(MAX_MC, MAX_MC)   !Jacobian array

! --- LOCALS

integer iA, iB, i
real    dcadca, dcadcb

!====   Set local pointer

iA = react%iA
iB = react%iB

!====   Set derivatives with respect to A and iB

dcadca = react%k*ps(iB)%c
dcadcb = react%k*ps(iA)%c

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
