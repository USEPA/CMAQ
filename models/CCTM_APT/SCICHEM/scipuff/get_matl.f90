!*******************************************************************************
!$RCSfile: get_matl.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine get_puff_material(ityp,mat,mataux,pmatl)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Set density and deposition velocity in material structure
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           get_gas_param          get_part_param                   IsGas
!              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

integer ityp                 !puff type numer
type ( material_str ) mat(*) !material structure
real    mataux(*)            !material auxilliary array
type ( puff_material ) pmatl !puff material structure (gas or particle)

! --- LOCALS

integer imat, igrp, iaux, icls

logical IsGas, IsParticle

imat = typeID(ityp)%imat
igrp = typeID(ityp)%igrp

iaux = mat(imat)%iaux

icls = typeID(ityp)%icls

if(IsGas(icls))then

  call get_gas_param(pmatl,iaux,mataux)

else if(IsParticle(icls))then

  call get_part_param(pmatl,iaux,igrp,mataux)

else
  nError = IV_ERROR
  eMessage = 'Invalid Material Class'
  write(eInform,*)'Class =',icls
  eRoutine = 'GetPuffMaterial'
end if

return
end

subroutine get_gas_param(pmatl,iaux,mataux)
!*******************************************************************************
!
! FUNCTION:  Get density and deposition velocity for gas materials
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use struct_inc

implicit none

! --- ARGUMENTS

type ( puff_material ) pmatl  !gas material structure

real      mataux(*)          !material auxilliary array
integer   iaux               !pointer to start of gas data in aux array

pmatl%param(1) = mataux(iaux)   !rho
pmatl%param(2) = mataux(iaux+1) !vd

return
end

subroutine get_part_param(pmatl,iaux,igrp,mataux)
!*******************************************************************************
!
! FUNCTION:  Get density and number of subgroups for particle materials
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use param_inc
use struct_inc

implicit none

! --- ARGUMENTS

type ( puff_material ) pmatl   !particle material structure

real      mataux(*)            !material auxiliary array
integer   iaux                 !pointer to start of particle data in aux array
integer   igrp                 !particle size group

! --- LOCALS

integer i

pmatl%nsg = nint(mataux(iaux))

iaux = iaux + 1
pmatl%param(1) = mataux(iaux) !rho

iaux = iaux + (igrp-1)*MAXPMAUX

do i = 2,MAXPMAUX+2
  pmatl%param(i) = mataux(iaux+i)
end do

return
end

subroutine put_puff_material(ityp,mat,mataux,pmatl)
!*******************************************************************************
!
! FUNCTION:  Load the material auxiliary arry from the material structure
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           put_gas_param          put_part_param                   IsGas
!              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

integer ityp                 !puff type numer
type ( material_str ) mat(*) !material structure
real    mataux(*)            !material auxilliary array
type ( puff_material ) pmatl !puff material structure (gas or particle)

! --- LOCALS


integer imat, igrp, iaux, icls

logical IsGas, IsParticle

imat = typeID(ityp)%imat
igrp = typeID(ityp)%igrp

iaux = mat(imat)%iaux

icls = typeID(ityp)%icls

if(IsGas(icls))then

  call put_gas_param(pmatl,iaux,mataux)

else if(IsParticle(icls))then

  call put_part_param(pmatl,iaux,igrp,mataux)

else
  nError = IV_ERROR
  eMessage = 'Invalid Material Class'
  write(eInform,*)'Class =',icls
  eRoutine = 'PutPuffMaterial'
end if

return
end

subroutine put_gas_param(pmatl,iaux,mataux)
!*******************************************************************************
!
! FUNCTION:  Load the material auxiliary arry from a gas material structure
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use struct_inc

implicit none

! --- ARGUMENTS

type ( puff_material ) pmatl  !gas material structure

real      mataux(*)          !material auxilliary array
integer   iaux               !pointer to start of gas data in aux array

mataux(iaux)   = pmatl%param(1)  !rho
mataux(iaux+1) = pmatl%param(2)  !vd

return
end

subroutine put_part_param(pmatl,iaux,igrp,mataux)
!*******************************************************************************
!
! FUNCTION:  Load the material auxiliary arry from a particle material structure
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
use struct_inc

implicit none

! --- ARGUMENTS

type ( puff_material ) pmatl  !gas material structure

real      mataux(*)            !material auxilliary array
integer   iaux                 !pointer to start of particle data in aux array
integer   igrp                 !particle size group

! --- LOCALS

integer i

if(igrp == 1)then
  mataux(iaux)   = float(pmatl%nsg)
  mataux(iaux+1) = pmatl%param(1)       !rho
end if

iaux = iaux + (igrp-1)*MAXPMAUX + 1

do i = 2,MAXPMAUX+2
  mataux(iaux+i) = pmatl%param(i)
end do

return
end

integer function output_groups(mat,mataux)
!*******************************************************************************
!
! FUNCTION:  Returns the number of groups associated with the material
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

type ( material_str ) mat  !material structure
real    mataux(*)          !material auxilliary array

! --- LOCALS

logical IsGas, IsParticle

if(IsGas(mat%icls))then
  output_groups = 1
else if(IsParticle(mat%icls))then
  output_groups = nint(mataux(mat%iaux))
end if

return

end

real function material_density(mat,mataux)
!*******************************************************************************
!
! FUNCTION:  Returns the material density
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

type ( material_str ) mat  !material structure
real    mataux(*)          !material auxilliary array

! --- LOCALS

logical IsGas, IsParticle

if(IsGas(mat%icls))then
  material_density = mataux(mat%iaux)
else if(IsParticle(mat%icls))then
  material_density = mataux(mat%iaux+1)
end if

return

end

integer function sub_groups(mat,mataux)
!*******************************************************************************
!
! FUNCTION:  Returns the number of subgroups associated with the material
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use files_inc

implicit none

! --- ARGUMENTS

type ( material_str ) mat  !material structure
real    mataux(*)          !material auxilliary array

! --- LOCALS

logical IsGas, IsParticle

if(IsGas(mat%icls))then
  sub_groups = 0
else if(IsParticle(mat%icls))then
  sub_groups = nint(mataux(mat%iaux))
end if

return

end

integer function num_puff_types(mat,mataux)
!*******************************************************************************
!
! FUNCTION:  Returns the number of puff types
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

type ( material_str ) mat  !material structure
real    mataux(*)          !material auxilliary array

! --- LOCALS

logical IsGas, IsParticle

if(IsGas(mat%icls))then
  num_puff_types = 1
else if(IsParticle(mat%icls))then
  num_puff_types = nint(mataux(mat%iaux))
end if

return

end

subroutine get_bounds(mat,mataux,nsg,pbounds)
!*******************************************************************************
!
! FUNCTION:  Set the particles bin boundaries
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

type ( material_str ) mat  !material structure
real    mataux(*)          !material auxilliary array
integer nsg                !number of particle size groups
real    pbounds(*)         !particle bun boundaries

! --- LOCALS

integer k, j

logical IsParticle

if(IsParticle(mat%icls))then
  nsg = nint(mataux(mat%iaux))
  do k = 1,nsg+1
    j = mat%iaux + (k-1)*MAXPMAUX + PMAUX_BOUNDS
    pbounds(k) = mataux(j)
  end do
else
  nsg = 0
end if

return

end

subroutine get_sizes(mat,mataux,nsg,psize)
!*******************************************************************************
!
! FUNCTION:  Set the particles sizes
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

type ( material_str ) mat  !material structure
real    mataux(*)          !material auxilliary array
integer nsg                !number of particle size groups
real    psize(*)           !particle sizes

! --- LOCALS

integer k, j

logical IsParticle

if(IsParticle(mat%icls))then
  nsg = nint(mataux(mat%iaux))
  do k = 1,nsg
    j = mat%iaux + (k-1)*MAXPMAUX + PMAUX_MEAN
    psize(k) = mataux(j)
  end do
else
  nsg = 0
end if

return

end
