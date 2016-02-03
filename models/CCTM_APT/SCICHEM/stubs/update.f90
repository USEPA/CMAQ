!*******************************************************************************
!$RCSfile: update.f90,v $
!$Revision: 1.2 $
!$Date: 2003/11/25 22:59:13 $
!*******************************************************************************
subroutine update_release(opidx,i,ipuf)
use error_inc

implicit none

integer opidx,i,ipuf


nError = IV_ERROR
eRoutine = 'UpdateRelease'
eMessage = 'Unable to update release data'
eInform  = 'UNIX/SGI Version does not have access to the DLL'

return
end
