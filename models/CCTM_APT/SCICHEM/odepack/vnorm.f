!*******************************************************************************
!$RCSfile: vnorm.f,v $
!$Revision: 1.2 $
!$Date: 2003/11/25 22:03:55 $
!*******************************************************************************
      real function vnorm (n, v, w)
clll. optimize
c-----------------------------------------------------------------------
c this function routine computes the weighted root-mean-square norm
c of the vector of length n contained in the array v, with weights
c contained in the array w of length n..
c   vnorm = sqrt( (1/n) * sum( v(i)*w(i) )**2 )
c-----------------------------------------------------------------------
      integer n,   i
      real v, w,   sum
      dimension v(n), w(n)
      sum = 0.0e0
      do 10 i = 1,n
 10     sum = sum + (v(i)*w(i))**2
      vnorm = sqrt(sum/float(n))
      return
c----------------------- end of function vnorm -------------------------
      end
