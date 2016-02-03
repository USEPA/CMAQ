!*******************************************************************************
!$RCSfile: set_tlev.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine set_tlev
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Setup multi-timestep linked lists
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              c_set_tlev
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- LOCALS

integer ilev, ipuf

do ilev = 0,MAXTLV
  itfrst(ilev) = 0
  itlast(ilev) = 0
  ntlev(ilev)  = 0
end do

mxtlev = 0

if(npuf > 0)then
  do ipuf = 1,npuf
    ilev = puff(ipuf)%idtl
    if (ilev >= 0) then
      puff(ipuf)%idtn = 0
      mxtlev = max0(ilev,mxtlev)
      if(itfrst(ilev) == 0)then
        itfrst(ilev) = ipuf
      else
        puff(itlast(ilev))%idtn = ipuf
      end if
      itlast(ilev) = ipuf
      ntlev(ilev) = ntlev(ilev) + 1
    end if
  end do
end if

call c_set_tlev

return
end

subroutine reset_tlev
!*******************************************************************************
!
! FUNCTION: Setup multi-timestep linked lists
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

implicit none

! --- LOCALS

integer ilev, ipuf


do ilev = 0,MAXTLV
  itfrst(ilev) = 0
  itlast(ilev) = 0
  ntlev(ilev)  = 0
end do

do ipuf = 1,npuf
  ilev = puff(ipuf)%idtl
  if (ilev >= 0) then
    puff(ipuf)%idtn = 0
    if(itfrst(ilev) == 0)then
      itfrst(ilev) = ipuf
    else
      puff(itlast(ilev))%idtn = ipuf
    end if
    itlast(ilev) = ipuf
    ntlev(ilev) = ntlev(ilev) + 1
  end if
end do

return
end

subroutine add_tlev(n1,n2)
!*******************************************************************************
!
! FUNCTION: Add new puffs to multi-timestep linked lists
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

implicit none

! --- ARGUMENTS
 
integer n1, n2   !puff numbers to add, from n1 to n2

! --- LOCALS

integer ipuf, ilev


do ipuf = n1,n2
  ilev = puff(ipuf)%idtl
  if (ilev >= 0) then
    puff(ipuf)%idtn = 0
    if(itfrst(ilev) == 0)then
      itfrst(ilev) = ipuf
    else
      puff(itlast(ilev))%idtn = ipuf
    end if
    itlast(ilev) = ipuf
    ntlev(ilev) = ntlev(ilev) + 1
  end if
end do

return
end

subroutine remove_tlev(lev,ipuf,iprv,ipufn)
!*******************************************************************************
!
! FUNCTION: Remove a puff from multi-timestep linked lists
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

implicit none

! --- ARGUMENTS
 
integer lev   !puff time level
integer ipuf  !puff number
integer iprv  !previous puff in linked list
integer ipufn !next puff in linked list


if(iprv == 0)then
  itfrst(lev) = ipufn
else
  puff(iprv)%idtn = ipufn
end if
if (ipufn == 0) itlast(lev) = iprv
ntlev(lev) = ntlev(lev) - 1

return
end
