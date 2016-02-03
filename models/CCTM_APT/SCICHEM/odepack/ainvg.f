!*******************************************************************************
!$RCSfile: ainvg.f,v $
!$Revision: 1.3 $
!$Date: 2003/11/25 21:52:08 $
!*******************************************************************************
      subroutine ainvg (res, adda, neq, t, y, ydot, miter,
     1                   ml, mu, pw, ipvt, ier )
clll. optimize
      external res, adda
      integer neq, miter, ml, mu, ipvt, ier
      integer i, lenpw, mlp1, nrowpw
      real t, y, ydot, pw
      dimension y(*), ydot(*), pw(*), ipvt(*)
c-----------------------------------------------------------------------
c this subroutine computes the initial value
c of the vector ydot satisfying
c     a * ydot = g(t,y)
c when a is nonsingular.  it is called by lsodi for
c initialization only, when istate = 0 .
c ainvg returns an error flag ier..
c   ier  =  0  means ainvg was successful.
c   ier .ge. 2 means res returned an error flag ires = ier.
c   ier .lt. 0 means the a-matrix was found to be singular.
c-----------------------------------------------------------------------
c
      if (miter .ge. 4)  go to 100
c
c full matrix case -----------------------------------------------------
c
      lenpw = neq*neq
      do 10  i = 1, lenpw
   10    pw(i) = 0.0e0
c
      ier = 1
      call res ( neq, t, y, pw, ydot, ier )
      if (ier .gt. 1) return
c
      call adda ( neq, t, y, 0, 0, pw, neq )
      call sgefa ( pw, neq, neq, ipvt, ier )
      if (ier .eq. 0) go to 20
         ier = -ier
         return
   20 call sgesl ( pw, neq, neq, ipvt, ydot, 0 )
      return
c
c band matrix case -----------------------------------------------------
c
  100 continue
      nrowpw = 2*ml + mu + 1
      lenpw = neq * nrowpw
      do 110  i = 1, lenpw
  110    pw(i) = 0.0e0
c
      ier = 1
      call res ( neq, t, y, pw, ydot, ier )
      if (ier .gt. 1) return
c
      mlp1 = ml + 1
      call adda ( neq, t, y, ml, mu, pw(mlp1), nrowpw )
      call sgbfa ( pw, nrowpw, neq, ml, mu, ipvt, ier )
      if (ier .eq. 0) go to 120
         ier = -ier
         return
  120 call sgbsl ( pw, nrowpw, neq, ml, mu, ipvt, ydot, 0 )
      return
c-------------------- end of subroutine ainvg --------------------------
      end
