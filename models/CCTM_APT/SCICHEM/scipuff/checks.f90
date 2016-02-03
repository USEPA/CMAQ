!*******************************************************************************
!$RCSfile: checks.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine check_lon(x,xmin,xmax)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Check that longitude x is between xmin and xmax
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use error_inc

implicit none

! --- ARGUMENTS

real x, xmin, xmax  !longitude checked, min and max longitude

! --- PARAMETERS

integer MAXN
parameter (MAXN = 3)

! --- LOCALS

integer icount
character*80 xsav

icount = 0
write(xsav,*) x

if (x < xmin) then
  do while (xmin - x > 180. .and. icount < MAXN)
    x = x + 360.
    icount = icount + 1
  end do
else if (x > xmax) then
  do while (x - xmax > 180. .and. icount < MAXN)
    x = x - 360.
    icount = icount + 1
  end do
end if

if (icount >= MAXN) then
  nError = UK_ERROR
  eRoutine = 'check_lon'
  eMessage = 'Invalid longitude='//TRIM(xsav)
  go to 9999
end if

9999    continue

return
end

subroutine check_nsubgroups(nsg,mat)
!*******************************************************************************
!
! FUNCTION: Check the number of material subgroups
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

integer nsg               !number of subgroups
type ( material_str ) mat !material structure

! --- LOCALS

integer icls, mxsgp_matl
logical IsParticle

icls = mat%icls

if(IsParticle(icls))then
  mxsgp_matl = MAXSGP
else
  mxsgp_matl = 1
end if

if(nsg <= 0 .or. nsg > mxsgp_matl)then
  nError   = IV_ERROR
  eMessage = 'Invalid number of size bins for '//TRIM(mat%cmat)
  write(eAction,'(a,i5)') 'Range is 1 -',mxsgp_matl
end if

return
end

subroutine check_real(x,xmin,xmax,string)
!*******************************************************************************
!
! FUNCTION:   Check if a number falls in the range that is expected
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

use default_inc
use error_inc

implicit none

! --- ARGUMENTS

real x, xmin, xmax   !value being checked, min and max expected
character*(*) string !message passed back


if (x /= DEF_VAL_R .and. x /= NOT_SET_R) then

  if (x < xmin .or. x > xmax) then
    nError = IV_ERROR
    eMessage = TRIM(string)//' outside allowable range'
    write(eInform,*) 'Range = ',xmin,' to',xmax
  end if

end if

return
end

subroutine check_units(nmat,mat,mataux,ldense_gas)
!*******************************************************************************
!
! FUNCTION: Check that the material has necessary units
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  clower           get_gas_param                check_kg
!                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

logical ldense_gas           !whether or not material has same density as air
type ( material_str ) mat(*) !material structure
integer nmat                 !number of material
real    mataux(*)            !material auxilliary array

! --- LOCALS

real    denrat
integer i,j,iaux

type ( gas_material ) gmat

type ( puff_material ) pmat

equivalence ( pmat, gmat )

logical IsGas

character*80 string1,string2
character*16 units

do i = 1,nmat

  string1 = 'Material='//TRIM(mat(i)%cmat)
  units   = TRIM(mat(i)%unit)
  string2 = 'Material='//TRIM(mat(i)%cmat)//' : units=' &
                                                        //TRIM(units)
  call clower(units)
  j    = mat(i)%icls
  iaux = mat(i)%iaux

  if(IsGas(j))then
    call get_gas_param(pmat,iaux,mataux)
    denrat = gmat%rho/rhoair
    call check_kg(units,denrat,ldense_gas)
  end if

  if(nError /= NO_ERROR)then
    eRoutine='CheckUnits'
    if(nError == UK_ERROR)then
      eInform = TRIM(string1)
    else
      eInform = TRIM(string2)
    end if
    go to 9999
  end if

end do

9999    return
end

subroutine check_kg(units,denrat,ldense_gas)
!*******************************************************************************
!
! FUNCTION: Check that the material has necessary units
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                  clower
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

logical ldense_gas   !whether or not material has same density as air
real    denrat       !ratio of the density of material to the density of air
character*(*) units  !material mass units

! --- LOCALS

character*16  munit

if(denrat /= 0.0)then
  if(ldense_gas)then
    munit = units
    call clower(munit)

    if (denrat /= 1.0) then

      if (munit /= 'kg') then
        nError = IV_ERROR
        eMessage = 'Mass release units must be ''kg'' for '// &
                      'buoyant gases and liquid vapors'
      end if

    end if
  end if
else
  munit = units
  call clower(munit)

  if (munit /= 'kg') then
    nError = IV_ERROR
    eMessage = 'Mass release units must be ''kg'' for aerosols'
  else if(.not.ldense_gas)then
    nError = UK_ERROR
    eMessage = 'Invalid material class/dynamics combination'
    eAction  = 'Aerosol materials require dense gas dynamics'
  end if

end if

return
end
