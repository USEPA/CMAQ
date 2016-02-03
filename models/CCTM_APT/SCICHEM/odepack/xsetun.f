!*******************************************************************************
!$RCSfile: xsetun.f,v $
!$Revision: 1.3 $
!$Date: 2003/11/25 22:04:41 $
!*******************************************************************************
      subroutine xsetun (lun)
c
c this routine resets the logical unit number for messages.
c
      integer lun, mesflg, lunit
      common /eh0001/ mesflg, lunit
      save /eh0001/ 
c
      if (lun .gt. 0) lunit = lun
      return
c----------------------- end of subroutine xsetun ----------------------
      end
