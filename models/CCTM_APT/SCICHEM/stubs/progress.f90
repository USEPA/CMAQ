!*******************************************************************************
!$RCSfile: progress.f90,v $
!$Revision: 1.3 $
!$Date: 2003/11/25 22:59:01 $
!*******************************************************************************
subroutine write_progress(m1,m2,m3)
use files_inc

character*(*) m1,m2,m3
integer, external :: nblank


integer nch

if(m1(1:1) /= char(0))then
  nch = nblank(m1)
  if(nch > 1)then
    write(6,*)m1(1:nch)
  end if
end if

if(m2(1:1) /= char(0))then
  nch = nblank(m2)
  if(nch > 1)then
    write(6,*)m2(1:nch)
  end if
end if

if(m3(1:1) /= char(0))then
  nch = nblank(m3)
  if(nch > 1)then
    write(6,*)m3(1:nch)
  end if
end if

call flush(6)

return
end

!===============================================================================

subroutine check_progress

return
end

!===============================================================================

subroutine write_progress_bar(n)
integer n

return
end

!===============================================================================

subroutine enable_halt(n)
integer n

return
end

!===============================================================================

subroutine disable_halt(n)
integer n

return
end
