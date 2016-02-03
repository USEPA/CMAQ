!*******************************************************************************
!$RCSfile: xsetf.f,v $
!$Revision: 1.3 $
!$Date: 2003/11/25 22:04:25 $
!*******************************************************************************
      subroutine xsetf (mflag)
c
c this routine resets the print control flag mflag.
c
      integer mflag, mesflg, lunit
      common /eh0001/ mesflg, lunit
      save /eh0001/
c
      if (mflag .eq. 0 .or. mflag .eq. 1) mesflg = mflag
      return
c----------------------- end of subroutine xsetf -----------------------
      end
