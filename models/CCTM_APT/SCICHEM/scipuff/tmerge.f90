!*******************************************************************************
!$RCSfile: tmerge.f90,v $
!$Revision: 1.4 $
!$Date: 2010/08/27 20:30:44 $
!*******************************************************************************
subroutine tmerge(ilev,jlev,cgrid)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Merge puffs, setup ipgrd and linked lists
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            remove_ipgrd                  mapfac               dump_puff
!               puff_grid           get_grid_cell               get_ipgrd
!               set_ipgrd        search_and_merge                    grid
!            chkgrd_merge
!
! REVISION HISTORY: 
! Aug 2010: Include updates form May 2005 for CMAQ-APT-PM made by PKK, AER - BC(SAGE-MGT)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use interface_definitions, only: chkgrd_merge

implicit none

! --- ARGUMENTS
 
integer ilev, jlev  !Max and min time levels that are being stepped
REAL :: CGRID( :,:,:,: )  !3-D ambient concentration (for chemistry)

! --- LOCALS

logical lok

integer grid
integer lev, ipuf, igrd, k, ip, jpuf
integer in, jn, id, jd, i, j
integer k0, kl, ku, ip0, jpuf0
integer nk

real xmap, ymap, fmass, rxx, ryy, xx, yy
real xc, yc, dfac

!------ set inverse moments and remove puffs from ip list

do lev = jlev,ilev,-1

  ipuf = itfrst(lev)

  do while (ipuf > 0)
    call remove_ipgrd(ipuf)
    ipuf = puff(ipuf)%idtn
  end do

end do

!------ loop over puffs

do lev = ilev,jlev
 ipuf  = itfrst(lev)
 do while (ipuf > 0)

  call mapfac(puff(ipuf)%xbar , puff(ipuf)%ybar , xmap , ymap)

  fmass  = puff(ipuf)%c

!------ find appropriate ipgrd location

  igrd = grid(puff(ipuf)%sxx,puff(ipuf)%syy,xmap,ymap)
  if(nError /= NO_ERROR)then
    eRoutine = 'TMerge'
    call dump_puff(ipuf,puff(ipuf))
    go to 9999
  end if

  call puff_grid(puff(ipuf),xx,yy,k0)

  rxx  = puff(ipuf)%sxx*xmap*xmap/(dxg*dxg)
  ryy  = puff(ipuf)%syy*ymap*ymap/(dyg*dyg)

!------- check for other puffs in grid

  lok = chkgrd_merge(xx,yy,rxx,ryy,puff(ipuf),cgrid)
  if(nError /= NO_ERROR) go to 9999

  if ( lok .and. fmass > cmin) then

    if ( puff(ipuf)%szz > delz2 )then
      nk = int(2.*sqrt(puff(ipuf)%szz)/dzg)
      ku = min(k0 + nk,nz)
      kl = max(k0 - nk,1)
    else
      ku = k0
      kl = k0
    end if

    dfac = 0.5**igrd

    xc = xx/dfac
    xc = xc - INT(xc)
    if (xc > 0.5) then
      if (xx+dfac > float(nx) ) then
        in = 0
      else
        in = 1
      end if
      id = 1
    else
      if (xx-dfac < 0.0) then
        in = 0
      else
        in = -1
      end if
      id = -1
    end if

    yc = yy/dfac
    yc = yc - INT(yc)
    if (yc > 0.5) then
      if (yy+dfac > float(ny) ) then
        jn = 0
      else
        jn = 1
      end if
      jd = 1
    else
      if (yy-dfac < 0.0) then
        jn = 0
      else
        jn = -1
      end if
      jd = -1
    end if

    Xloop: do i = 0,in,id
      xc = xx + float(i)*dfac

      Yloop: do j = 0,jn,jd
        yc = yy + float(j)*dfac

        Zloop: do k = kl,ku

          call find_grid_cell(xc,yc,k,igrd,ip)

          if (ip > 0) then
            call get_ipgrd(ip,2,k,jpuf)
          else
            jpuf = 0
          end if

!------- check other puffs in box

          if (jpuf > 0) then
            call search_and_merge(ipuf,jpuf,igrd,xmap,ymap,ilev,k,ip)
            if( jpuf == 0 )then
              jpuf0 = -1
              exit Xloop
            end if
          end if

          if( k == k0 .and. i==0 .and. j==0 )then   !Save local cell info
            jpuf0 = jpuf
            ip0   = ip
          end if

        end do Zloop

      end do Yloop

    end do Xloop

    if( jpuf0 == 0 )then   ! First puff in box

      if (ip0 == 0) then
        call get_grid_cell(xx,yy,k0,igrd,ip0)
        if (nError /= NO_ERROR) goto 9999
      end if

      call set_ipgrd( ip0,2,k0,ipuf )
      puff(ipuf)%inxt = 0
      puff(ipuf)%iprv = -(1000*ip0 + k0)
      puff(ipuf)%ipgd = igrd

    else if( jpuf0 > 0 )then ! No merge - add to list

      puff(jpuf0)%inxt = ipuf
      puff(ipuf)%inxt  = 0
      puff(ipuf)%iprv  = jpuf0
      puff(ipuf)%ipgd  = igrd

    else ! merged - mark for removal

      puff(ipuf)%idtl = I_REMOVE

    end if

  else ! failed chkgrd - remove

    puff(ipuf)%idtl = I_REMOVE 
      
  end if

  ipuf  = puff(ipuf)%idtn

 end do
end do

9999    return
end

subroutine compress_puff_list
!*******************************************************************************
!
! FUNCTION:  Remove dead puffs and fix IPGRD linked list
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               copy_puff               set_ipgrd  update_static_pointers
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- LOCALS

integer ipuf, inxt, iprv, iout, ipaux
integer ip, jprv, k

iout  = 0
ipaux = 1

do ipuf = 1,npuf

  if (puff(ipuf)%idtl /= I_REMOVE) then

    iout = iout + 1
    call copy_puff(puff(ipuf),puff(iout),ipaux)

    if (ipuf/=iout) then

      iprv = puff(ipuf)%iprv
      inxt = puff(ipuf)%inxt

      if (inxt > 0) then
        puff(inxt)%iprv = iout
      end if

      if (iprv > 0) then
        puff(iprv)%inxt = iout
      else
        jprv = iabs(iprv)
        ip   = jprv/1000
        k    = jprv - 1000*ip
        call set_ipgrd(ip,2,k,iout)
      end if

      if (puff(ipuf)%idtl == I_STATIC) then
        call update_static_pointers(iout)
      end if

    end if                         ! ipuf .ne. iout

  end if                           ! marked for removal

end do

npuf  = iout
npaux = ipaux

return
end
