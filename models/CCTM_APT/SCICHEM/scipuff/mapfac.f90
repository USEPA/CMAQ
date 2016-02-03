!*******************************************************************************
!$RCSfile: mapfac.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine mapfac( x , y , xmap , ymap )
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:     Horizontal coordinate transform function
!               (x,y) are puff coordinates
!               If dx is in meters then  dx*xmap is in puff coords
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              mapfac_pig
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
      
implicit none

! --- ARGUMENTS
 
real x, y        !Puff coordinates
real xmap, ymap  !Conversion factor for meters to puff coordinates

!---------

if (lmap == I_LATLON) then

  xmap = sphfacr/cos(y*pi180)
  ymap = sphfacr

else if (lmap == I_METERS) then

  xmap = 1.
  ymap = 1.

else if (lmap == I_CARTESIAN .or. lmap == I_UTM) then

  xmap = 1.0e-3
  ymap = 1.0e-3

else if (lmap == I_PIG) then

  call mapfac_pig( x , y , xmap , ymap )

end if

return

end
