!*******************************************************************************
!$RCSfile: isamax.f,v $
!$Revision: 1.3 $
!$Date: 2003/11/25 21:54:05 $
!*******************************************************************************
      integer function isamax(n,sx,incx)
clll. optimize
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 6/17/77.
c
      real sx(*),smax
      integer i,incx,ix,n
c
      isamax = 1
      if(n.le.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      smax = abs(sx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(abs(sx(ix)).le.smax) go to 5
         isamax = i
         smax = abs(sx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 smax = abs(sx(1))
      do 30 i = 2,n
         if(abs(sx(i)).le.smax) go to 30
         isamax = i
         smax = abs(sx(i))
   30 continue
      return
      end
