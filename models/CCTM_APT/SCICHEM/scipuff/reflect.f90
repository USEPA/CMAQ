!*******************************************************************************
!$RCSfile: reflect.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine reflect(zsrf,zp,asig,hx,hy,lsrf,xr,vfac)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: rotate moment tensor into local coordinate system defined by
!       the surface normal and tangent plane; find position of reflected
!       puff in this system; rotate back into original coordinate system;
!       find surface integral factor
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              set_matrot                  matmul                  trnsps
!                 matvmul             check_slope
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use refl_inc

implicit none

! --- ARGUMENTS

real zsrf     !puff height relative to the ground
real zp       !puff height relative to the dose height
real asig     !inverse spread tensor
real hx, hy   !terrain gradients
real xr       !reflected position
real vfac     !refelction factor
logical lsrf  !compute surface integral factor

dimension asig(7), xr(3)
 
! --- LOCALS

integer i
real xs, ys, zs, arg, tem, facs
real denr, x, c
dimension x(3), c(3,3)
logical check_slope

b_rfl(1,:) = asig(1:3)
b_rfl(2,2) = asig(4)
b_rfl(2,3) = asig(5)
b_rfl(3,3) = asig(6)
b_rfl(2,1) = b_rfl(1,2)
b_rfl(3,1) = b_rfl(1,3)
b_rfl(3,2) = b_rfl(2,3)

!------ rotate into local coordinates

if (check_slope(hx,hy)) then

  call set_matrot(hx,hy,a_rfl)

  call xmatmul( a_rfl,b_rfl,c )
  call trnsps( a_rfl,at_rfl )
  call xmatmul( c,at_rfl,b_rfl )

else

  a_rfl = 0.0
  do i = 1,3
   a_rfl(i ,i) = 1.
  end do
  at_rfl = a_rfl

end if

denr = at_rfl(3,3)

!------ find reflected puff position in local coordinates

zs = -zp*denr

deth = b_rfl(1,1)*b_rfl(2,2) - b_rfl(1,2)*b_rfl(1,2)
xs   = -zs*(b_rfl(1,3)*b_rfl(2,2)-b_rfl(2,3)*b_rfl(1,2))/deth
ys   = -zs*(b_rfl(2,3)*b_rfl(1,1)-b_rfl(1,3)*b_rfl(1,2))/deth

x(1) = 2.*xs
x(2) = 2.*ys
x(3) = 2.*zs

if (lsrf) then

!------ compute surface integral factor

  arg = -(b_rfl(3,3)*zs*zs-b_rfl(1,1)*xs*xs-b_rfl(2,2)*ys*ys &
                     -2.*b_rfl(1,2)*xs*ys)
  if (arg > -30.0) then
    tem = exp(arg)
  else
    tem = 0.0
  end if

  vfac = tem/sqrt(deth)                 ! * denr

!------ Add surface reflection

  if(zsrf == zp)then
    facs = 1.
  else
    zs = -zsrf*denr
    facs = exp(0.5*zs*(zsrf-zp)/(asig(7)*deth*denr))
  end if
  vfac = vfac * (1.+facs)

end if

!------ rotate back into global coordinates

call matvmul(at_rfl,x,xr)

return
end

subroutine xmatmul(a,b,c)
!*******************************************************************************
!
! FUNCTION :   Multiply 2 matrices
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

real a(3,3), b(3,3), c(3,3)
real sum

integer i, j, k

do j = 1,3
  do i = 1,3
    sum = 0.
    do k = 1,3
      sum = sum + a(i,k)*b(k,j)
    end do
    c(i,j) = sum
  end do
end do

return
end

subroutine trnsps(a,b)
!*******************************************************************************
!
! SUBROUTINE: Transpose of a matrix
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

real    a(3,3), b(3,3)

integer i, j

do j = 1,3
  do i = 1,3
    b(i,j) = a(j,i)
  end do
end do

return
end

real function det(a)
!*******************************************************************************
!
! FUNCTION:   Determinant of a matrix
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real a(3,3)  !in matrix

! --- LOCALS

real d11, d12, d13

d11 = a(2,2)*a(3,3) - a(3,2)*a(2,3)
d12 = a(2,1)*a(3,3) - a(3,1)*a(2,3)
d13 = a(2,1)*a(3,2) - a(3,1)*a(2,2)
det = a(1,1)*d11 - a(1,2)*d12 + a(1,3)*d13

return
end

subroutine matvmul( a,x,y )
!*******************************************************************************
!
! subroutine:   multiply a vector and a matrix
!
! preconditions required: 
!
! subroutines and functions called: 
!
!
! revision history: 
!
!*******************************************************************************

implicit none

integer i, j

real x(3), y(3), a(3,3), sum

do i = 1,3
  sum = 0.
  do j = 1,3
    sum = sum + a(i,j)*x(j)
  end do
  y(i) = sum
end do

return
end


subroutine zi_reflect(zbar,zinv,zrefl,zs,rat,faci)
!*******************************************************************************
!
! FUNCTION:  Set inversion reflection factor
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS 

real zbar     !puff position
real zinv     !inversion location - now capping height
real zrefl    !reflection height
real zs       !slice location
real rat      !gaussain factor
real faci     !reflection factor

! --- LOCALS

real arg

!------ Inversion reflection factor

if( zbar > zinv )then        !PUFF above inversion - now
  faci = 0.
else
  if(zs > zrefl)then        !PUFF Below - SLICE Above
    faci = -1.
  else                      !PUFF Below - SLICE Below
    arg = (zrefl-zs)*(zrefl-zbar)*rat
    faci = exp(-arg)
  end if
end if

return
end

subroutine grnd_reflect(zp,asig,hx,hy,xr,x,deth,zs)
!*******************************************************************************
!
! FUNCTION:  Ground reflections
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              set_matrot                  matmul                  trnsps
!                 matvmul             check_slope
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none
 
! --- ARGUMENTS

real zp              !PUFF height above terrain
real asig(7)         !PUFF sigma
real hx              !terrain gradient
real hy              !terrain gradient
real xr(3)           !reflected position
real x(3)            !ground normals
real deth            !horizontal determinate
real zs              !Z normal

! --- LOCALS

integer i
real a(3,3), b(3,3), c(3,3), at(3,3)
real xs, ys

logical check_slope

!---- ground reflection

b(1,:) = asig(1:3)
b(2,2) = asig(4)
b(2,3) = asig(5)
b(3,3) = asig(6)
b(2,1) = b(1,2)
b(3,1) = b(1,3)
b(3,2) = b(2,3)

!---- rotate into local coordinates

if (check_slope(hx,hy)) then

  call set_matrot(hx,hy,a)

  call xmatmul(a,b,c)
  call trnsps(a,at)
  call xmatmul(c,at,b)

else
  a = 0.0
  do i = 1,3
    a(i,i) = 1.
  end do
  at = a
end if

!---- find reflected puff position in local coordinates

zs = -zp*at(3,3)

deth = b(1,1)*b(2,2) - b(1,2)*b(1,2)
xs   = -zs*(b(1,3)*b(2,2)-b(2,3)*b(1,2))/deth
ys   = -zs*(b(2,3)*b(1,1)-b(1,3)*b(1,2))/deth

x(1) = xs
x(2) = ys
x(3) = zs

!------ rotate back into global coordinates; save surface normal vector

CALL matvmul(at,x,xr)

x  = a(3,:)

return
end

subroutine set_matrot(hx,hy,amat)
!*******************************************************************************
!
! FUNCTION: Setup for rotation into local coordinates
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
 
real hx, hy    !terrain gradients
real amat(3,3) !rotation matrix

! --- LOCALS

real denr, denx

denr = 1./sqrt(1.+ hx*hx + hy*hy)
denx = 1./sqrt(1.+ hx*hx)

amat(1,1) = denx
amat(1,2) = 0.
amat(1,3) = hx*denx
amat(2,1) = -hx*hy*denx*denr
amat(2,2) = denr/denx
amat(2,3) = hy*denr*denx
amat(3,1) = -hx*denr
amat(3,2) = -hy*denr
amat(3,3) = denr

return
end
