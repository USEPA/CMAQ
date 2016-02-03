!*******************************************************************************
!$RCSfile: solbt.f,v $
!$Revision: 1.2 $
!$Date: 2003/11/25 22:01:50 $
!*******************************************************************************
      subroutine solbt (m, n, a, b, c, y, ip)
      integer m, n, ip(m,n)
      real a(m,m,n), b(m,m,n), c(m,m,n), y(m,n)
clll. optimize
c-----------------------------------------------------------------------
c solution of block-tridiagonal linear system.
c coefficient matrix must have been previously processed by decbt.
c m, n, a, b, c, and ip  must not have been changed since call to decbt.
c written by a. c. hindmarsh.
c input..
c     m = order of each block.
c     n = number of blocks in each direction of matrix.
c a,b,c = m by m by n arrays containing block lu decomposition
c         of coefficient matrix from decbt.
c    ip = m by n integer array of pivot information from decbt.
c     y = array of length m*n containg the right-hand side vector
c         (treated as an m by n array here).
c output..
c     y = solution vector, of length m*n.
c
c external routines required.. sgesl (linpack) and sdot (blas).
c-----------------------------------------------------------------------
c
      integer nm1, nm2, i, k, kb, km1, kp1
      real dp, sdot
      nm1 = n - 1
      nm2 = n - 2
c forward solution sweep. ----------------------------------------------
      call sgesl (a, m, m, ip, y, 0)
      do 30 k = 2,nm1
        km1 = k - 1
        do 20 i = 1,m
          dp = sdot (m, c(i,1,k), m, y(1,km1), 1)
          y(i,k) = y(i,k) - dp
 20       continue
        call sgesl (a(1,1,k), m, m, ip(1,k), y(1,k), 0)
 30     continue
      do 50 i = 1,m
        dp = sdot (m, c(i,1,n), m, y(1,nm1), 1)
     1     + sdot (m, b(i,1,n), m, y(1,nm2), 1)
        y(i,n) = y(i,n) - dp
 50     continue
      call sgesl (a(1,1,n), m, m, ip(1,n), y(1,n), 0)
c backward solution sweep. ---------------------------------------------
      do 80 kb = 1,nm1
        k = n - kb
        kp1 = k + 1
        do 70 i = 1,m
          dp = sdot (m, b(i,1,k), m, y(1,kp1), 1)
          y(i,k) = y(i,k) - dp
 70       continue
 80     continue
      do 100 i = 1,m
        dp = sdot (m, c(i,1,1), m, y(1,3), 1)
        y(i,1) = y(i,1) - dp
 100    continue
      return
c-----------------------  end of subroutine solbt  ---------------------
      end
