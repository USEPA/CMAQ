!*******************************************************************************
!$RCSfile: r1mach.f,v $
!$Revision: 1.2 $
!$Date: 2003/11/25 21:59:05 $
!*******************************************************************************
      real function r1mach (idum)
      integer idum
c-----------------------------------------------------------------------
c this routine computes the unit roundoff of the machine.
c this is defined as the smallest positive machine number
c u such that  1.0 + u .ne. 1.0
c-----------------------------------------------------------------------
      real u, comp
      u    = 1.0e0
      comp = 1.0e0 + u
c sfp
c Original version didn't work because the registers are double precision and
c the loop is so simple it is all done with registers.  By making a subrouitne
c call we force the data to be stored in a 32bit word and then the comparison is
c done as single precision
c
      do while (comp .ne. 1.0e0)
        call update_r1mach(u,comp)
      end do
      r1mach = u*2.0e0
c sfp
c      u = 1.0e0
c 10   u = u*0.5e0
c      comp = 1.0e0 + u
c      if (comp .ne. 1.0e0) go to 10
c      r1mach = u*2.0e0
      return
c----------------------- end of function r1mach ------------------------
      end

      subroutine update_r1mach(u,comp)
      real*4 u,comp
      u    = 0.5e0*u
      comp = 1.0e0 + u
      return
      end
