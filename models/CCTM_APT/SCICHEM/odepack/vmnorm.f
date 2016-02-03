!*******************************************************************************
!$RCSfile: vmnorm.f,v $
!$Revision: 1.2 $
!$Date: 2003/11/25 22:03:40 $
!*******************************************************************************
      real function vmnorm (n, v, w)
clll. optimize
c-----------------------------------------------------------------------
c this function routine computes the weighted max-norm
c of the vector of length n contained in the array v, with weights
c contained in the array w of length n..
c   vmnorm = max(i=1,...,n) abs(v(i))*w(i)
c-----------------------------------------------------------------------
      integer n,   i
      real v, w,   vm
      dimension v(n), w(n)
      vm = 0.0e0
      do 10 i = 1,n
 10     vm = amax1(vm,abs(v(i))*w(i))
      vmnorm = vm
      return
c----------------------- end of function vmnorm ------------------------
      end
