
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero5/Attic/SOA_NEWT.f,v 1.1 2010/07/20 11:55:06 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      module soa_NEWT

c  REVISION HISTORY:
c FSB This module contains all of the information and subroutines 
c     used in subroutine NEWT. 
c           
c YOJ 07/31/02 changed FUNCTION FMIN to SUBROUTINE FMINV to avoid errors
c     with (some) compilers
c
c CGN 01/12/04 removed ITS from NEWT call vector, added documentation, 
c     and removed extraneous lines of code -- Dr. Chris G. Nolte
c
c SLN 09/18/07 updated NP and NPREC for compatibility with new SOA module
c
c PVB 11/19/07 renamed NP to NCVAP for consistency with ORGAER5 subroutine
c
c  REFERENCES:
c   1. Schell, B., I. J. Ackermann, H. Hass, F. S. Binkowski, and
c      A. Abel, Modeling the formation of secondary organic aerosol
c      within a comprehensive air quality modeling system, J. Geophys.
c      Res., Vol 106, No D22, 28275-28293, 2001.

      implicit none      

      INTEGER MAXITS            !bs maximum number of iterations
       PARAMETER (MAXITS = 100)

      REAL TOLF                 !bs convergence criterion on function values
       PARAMETER (TOLF = 1.E-09)

      REAL TOLMIN                  !bs criterion whether spurious convergence to
       PARAMETER (TOLMIN = 1.E-12) !bs a minimum of fminv has occurred

      REAL TOLX                 !bs convergence criterion on delta_x
       PARAMETER (TOLX = 1.E-10)

      REAL STPMX                !bs scaled maximum step length allowed
       PARAMETER (STPMX = 100.)
      
      INTEGER NCVAP             ! number of partitioning SVOCs
       PARAMETER( NCVAP = 12 )      
      INTEGER nprec             ! this variable must be equal to NPSPCS
       PARAMETER( nprec = 10 ) 
            
      contains
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      
!     the following subroutines constitute Dr. Benedikt Schell's SOA
!     model      

!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      SUBROUTINE NEWT(
     &                LAYER, X, N, CHECK,
     &                CTOT, CSAT, IMWCV, MINITW )
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.   !
!bs                                                                    !
!bs  Given an initial guess X(1:N) for a root in N dimensions, find    !
!bs  the root by a globally convergent Newton's method. The vector of  !
!bs  functions to be zeroed, called FVEC(1:N) in the routine below. is !
!bs  returned by a user-supplied subroutine that must be called FUNCV  !
!ba  and have the declaration SUBROUTINE FUNCV(N,X,FVEC). The output   !
!bs  quantity CHECK is false on a normal return and true if the        !
!bs  routine has converged to a local minimum of the function FMINV    !
!bs  defined below. In this case should try restarting from a          !
!bs  different initial guess.                                          !
!bs                                                                    !
!bs  PARAMETERS                                                        !
!bs  NCVAP  : maximum expected value of N                              !
!bs  MAXITS : maximum number of iterations                             !
!bs  TOLF   : convergence criterion on function values                 !
!bs  TOLMIN : criterion for deciding whether spurious convergence to a !
!bs           minimum of FMINV has ocurred                             !
!bs  TOLX   : convergence criterion on delta_X                         !
!bs  STPMX  : scaled maximum step length allowed in line searches      !
!bs                                                                    !
!bs  Called by:       ORGAER3                                          !
!bs                                                                    !
!bs  Calls:           FDJAC                                            !
!bs                   FMINV                                            !
!bs                   LNSRCH                                           !
!bs                   LUBKSB                                           !
!bs                   LUDCMP                                           !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
c  REVISION HISTORY:
c
c CGN 01/12/04  Removed ITS from call vector, changed B1 & B2 to 
c      scalars, added documentation, removed extraneous lines of code

      
      IMPLICIT NONE
!bs
!bs * includes
!bs
!bs
!bs * input variables
!bs
      INTEGER LAYER           !bs model layer
      INTEGER N               !bs dimension of problem
      REAL X(N)               !bs initial guess of CAER
      LOGICAL CHECK
      REAL CTOT(N)            !bs total concentration GAS + AER + PROD
      REAL CSAT(N)            !bs saturation conc. of cond. vapor [ug/m^3]
      REAL IMWCV(N)           !bs inverse molecular weights
      REAL MINITW             !bs weighted initial mass
!bs
!bs * following Numerical recipes
!bs
      INTEGER NN
      REAL FVEC                 !bs vector of functions to be zeroed
!bs
      COMMON /NEWTV/ FVEC(NCVAP), NN
      SAVE /NEWTV/
!bs
      REAL CT
      REAL CS
      REAL IMW
      REAL M
      COMMON /NEWTINP/ CT(NCVAP), CS(NCVAP), IMW(NCVAP), M
      SAVE /NEWTINP/
!bs
      INTEGER I, ITS, J, INDX(NCVAP)
      REAL D, DEN, F, FOLD, STPMAX, SUM, TEMP, TEST
      REAL FJAC(NCVAP,NCVAP)
      REAL G(NCVAP), P(NCVAP), XOLD(NCVAP) 
!     EXTERNAL FDJAC


!bs
 !bs
!bs * begin code
!bs
      CHECK = .FALSE.
      M = MINITW
      DO I = 1, N
         CT(I) = CTOT(I)
         CS(I) = CSAT(I)
         IMW(I) = IMWCV(I)
      ENDDO
!bs
      NN = N
      CALL FMINV( X,F )       ! The vector FVEC is also computed by this call
      TEST = 0.               ! Test for initial guess being a root. Use more
                              ! stringent test than simply TOLF.      
      DO I = 1, N
         IF (ABS(FVEC(I)) .GT. TEST) TEST = ABS(FVEC(I))
      ENDDO
            
      IF (TEST .LT. 0.01*TOLF) RETURN  ! initial guess is a root
      SUM = 0.                  !Calculate STPMAX for line searches
      DO I = 1, N
         SUM = SUM + X(I)**2
      ENDDO
      STPMAX = STPMX * MAX(SQRT(SUM), FLOAT(N))
      DO ITS = 1, MAXITS        !start of iteration loop
         CALL FDJAC(N, X, FJAC) !get Jacobian
         DO I = 1, N            !compute Delta f for line search
            SUM = 0.
            DO J = 1, N
               SUM = SUM + FJAC(J,I) * FVEC(J)
            ENDDO
            G(I) = SUM
         ENDDO
         DO I  =1, N            !store X
            XOLD(I) = X(I)
         ENDDO
         FOLD = F               !store F
         DO I = 1, N            !right-hand side for linear equations
            P(I) = -FVEC(I)
         ENDDO
         CALL LUDCMP(FJAC,N,INDX,D) !solve linear equations by LU decomposition
         CALL LUBKSB(FJAC,N,INDX,P)
         CALL LNSRCH(CTOT,
     &               N, XOLD, FOLD, G, !LNSRCH returns new X and F. It also
     &               P, X, F, STPMAX,  !calculates FVEC at the new X when it
     &               CHECK)            !calls FMINV
         TEST = 0.
         DO I = 1, N
            IF (ABS(FVEC(I)) .GT. TEST) TEST = ABS(FVEC(I))
         ENDDO
         IF (TEST .LT. TOLF) THEN
            CHECK = .FALSE.
            RETURN
         ENDIF
         IF (CHECK) THEN        !Check for gradient of F zero,
            TEST = 0.           !i.e., spurious convergence.
            DEN = MAX(F,0.5*N)
            DO I = 1, N
               TEMP = ABS(G(I)) * MAX(ABS(X(I)),1.) / DEN
               IF (TEMP .GT. TEST) TEST = TEMP
            ENDDO
            IF (TEST .LT. TOLMIN) THEN
               check = .TRUE.
            ELSE
               CHECK = .FALSE.
            ENDIF
            RETURN
         ENDIF
         TEST = 0.              !Test for convergence on delta_x
         DO I = 1, N
            TEMP = (ABS(X(I)-XOLD(I))) / MAX(ABS(X(I)),1.)
            IF (TEMP .GT. TEST) TEST = TEMP
         ENDDO
         IF (TEST .LT. TOLX) RETURN
      ENDDO
      WRITE(6,'(a,i2)') 'MAXITS exceeded in NEWT ! Layer: ',LAYER
      END SUBROUTINE NEWT

     
            
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      subroutine fdjac( N, X, FJAC )
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Get the Jacobian of the function                                  !
!bs                                                                    !
!bs         ( a1 * X1^2 + b1 * X1 + c1 )                               !
!bs         ( a2 * X2^2 + b2 * X2 + c2 )                               !
!bs         ( a3 * X3^2 + b3 * X3 + c3 )                               !
!bs  F(X) = ( a4 * X4^2 + b4 * X4 + c4 ) = 0.                          !
!bs         ( ........................ )                               !
!bs         ( aN * XN^2 + bN * XN + cN )                               !
!bs                                                                    !
!bs   a_i = IMW_i                                                      !
!bs   b_i = SUM(X_j * IMW_j)_j.NE.i + CSAT_i * IMW_i  + M              !
!bs         - CTOT_i * IMW_i                                           !
!bs                                                                    !
!bs   c_i = - CTOT_i * [ SUM(X_j * IMW_j)_j.NE.i + M ]                 !
!bs                                                                    !
!bs          delta F_i    ( 2. * a_i * X_i + b_i          if i .EQ. j  !
!bs  J_ij = ----------- = (                                            !
!bs          delta X_j    ( ( X_i  - CTOT_i ) * IMW_j     if i .NE. j  !
!bs                                                                    !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
      IMPLICIT NONE
!bs
!bs
      INTEGER N                 !dimension of problem
      REAL X(N)                 !bs initial guess of CAER
!bs
      REAL CT
      REAL CS
      REAL IMW
      REAL M
      COMMON /NEWTINP/ CT(NCVAP), CS(NCVAP), IMW(NCVAP), M
!bs
      REAL FJAC(N,N)
!bs
      INTEGER I, J              !bs loop index
      REAL A(NCVAP)
      REAL B(NCVAP)
      REAL B1
      REAL B2
      REAL SUM_JNEI
!bs
      DO I = 1, N
         A(I) = IMW(I)
         SUM_JNEI = 0.
         DO J = 1, N
            SUM_JNEI = SUM_JNEI + X(J) * IMW(J)
         ENDDO
         B1 = SUM_JNEI - (X(I) * IMW(I))
         B2 = (CS(I) - CT(I)) * IMW(I) + M
         B(I) = B1 + B2
      ENDDO
      DO J = 1, N
         DO I = 1, N
            IF (I .EQ. J) THEN
               FJAC(I,J) = 2. * A(I) * X(I) + B(I)
            ELSE
               FJAC(I,J) = ( X(I) - CT(I) ) * IMW(J)
            ENDIF
         ENDDO
      ENDDO
!bs
      RETURN
      END SUBROUTINE fdjac
      
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      SUBROUTINE FMINV( X,F )
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.   !
!bs                                                                    !
!bs  Returns f = 0.5 * F*F at X. SR FUNCV(N,X,F) is a fixed-name,      !
!bs  user-supplied routine that returns the vector of functions at X.  !
!bs  The common block NEWTV communicates the function values back to   !
!bs  NEWT.                                                             !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs  Calls:           FUNCV                                            !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
C
      IMPLICIT NONE
C
!bs
!bs
      INTEGER N
      
      REAL X(*), F
      REAL FVEC
C
      COMMON /NEWTV/ FVEC(NCVAP), N
      SAVE /NEWTV/
C
      INTEGER I
      REAL SUM
      CALL FUNCV(N, X, FVEC)
      SUM = 0.
      DO I = 1, N
         SUM = SUM + FVEC(I)**2
      ENDDO
      F = 0.5 * SUM
      RETURN
      END SUBROUTINE FMINV
      
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      SUBROUTINE FUNCV( N, X, FVEC )
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Called by:       FMINV                                            !
!bs                                                                    !
!bs  Calls:           None                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
C
C   From Equation (8) of Schell et al., 2001:
C     Caer,i = Ctot,i - Csat,i * (Caer,i/MWi) / 
C                             ( sum_j (Caer,j/MWj) + Cinit/MWinit)
C   Let Xi  = Caer,i
C       a_i = 1 / MWi
C       M   = Cinit/MWinit
C       CTi = Ctot,i
C       CSi = Csat,i
C   Then,       
C       Xi  = CTi - CSi * (a_i * Xi) / ( sum_j (a_j * Xj) + M )
C
C   Multiply above equation by sum_j(a_j*Xj) + M and group terms
C       a_i Xi^2 + ( sum_jnei (a_j*Xj) + M + CSi*a_i - CTi*a_i ) Xi
C                - CTi * ( sum_jnei (a_j*Xj) + M ) = 0
C
C   This equation is of the form F(X) = a_i*Xi^2 + b_i*Xi + c_i = 0.
C     F(X) is stored as FVEC in this subroutine.
C
C   See also FDJAC.

C  REVISION HISTORY:
C
C CGN 01/12/04  Added documentation, removed extraneous lines of code


      IMPLICIT NONE
!bs
!bs
      INTEGER N
      REAL X(*)
      REAL FVEC(N)
!bs
      REAL CT
      REAL CS
      REAL IMW
      REAL M
      COMMON /NEWTINP/ CT(NCVAP), CS(NCVAP), IMW(NCVAP), M
      SAVE /NEWTINP/
!bs
      INTEGER I, J
      REAL SUM_JNEI
      REAL A(NCVAP)
      REAL B(NCVAP)
      REAL C(NCVAP)
!bs
      DO I = 1, N
         A(I) = IMW(I)
         SUM_JNEI = 0.
         DO J  = 1, N
            SUM_JNEI = SUM_JNEI + X(J) * IMW(J)
         ENDDO
         SUM_JNEI = SUM_JNEI - (X(I) * IMW(I))
         B(I) = SUM_JNEI + M + (CS(I) - CT(I)) * IMW(I)
         C(I) = -CT(I) * (SUM_JNEI + M)
         FVEC(I) = X(I) * (A(I) * X(I) + B(I)) + C(I)
         
      ENDDO
!bs
      RETURN
      END SUBROUTINE FUNCV
      
      SUBROUTINE LNSRCH(
     &                  CTOT,
     &                  N, XOLD, FOLD, G, P,
!    &                  X, F, STPMAX, CHECK, FUNC)
     &                  X, F, STPMAX, CHECK)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.   !
!bs                                                                    !
!bs  Given an n-dimensional point XOLD(1:N), the value of the function !
!bs  and gradient there, FOLD and G(1:N), and a direction P(1:N),      !
!bs  finds a new point X(1:N) along the direction P from XOLD where    !
!bs  the function FUNC has decreased 'sufficiently'. The new function  !
!bs  value is returned in F. STPMAX is an input quantity that limits   !
!bs  the length of the steps so that you do not try to evaluate the    !
!bs  function in regions where it is undefined or subject to overflow. !
!bs  P is usually the Newton direction. The output quantity CHECK is   !
!bs  false on a normal exit. It is true when X is too close to XOLD.   !
!bs  In a minimization algorithm, this usually signals convergence and !
!bs  can be ignored. However, in a zero-finding algorithm the calling  !
!bs  program should check whether the convergence is spurious.         !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs  Calls:           FUNCV                                            !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
C
      IMPLICIT NONE
!bs
!bs
      INTEGER N
      LOGICAL CHECK
      REAL F, FOLD, STPMAX
      REAL G(N), P(N), X(N), XOLD(N)
      REAL CTOT(N)
      REAL ALF
      PARAMETER (ALF = 1.E-04)
      REAL CONMIN
      PARAMETER (CONMIN = 1.E-30)
C
      INTEGER I
      REAL A, ALAM, ALAM2, ALAMIN, B, DISC
      REAL F2, FOLD2, RHS1, RHS2, SLOPE
      REAL SUM, TEMP, TEST, TMPLAM
C
      CHECK = .FALSE.
      SUM = 0.
      DO I = 1, N
         SUM = SUM + P(I)*P(I)
      ENDDO
      SUM = SQRT(SUM)
      IF (SUM .GT. STPMAX) THEN
         DO I = 1, N
            P(I) = P(I) * STPMAX / SUM
         ENDDO
      ENDIF
      SLOPE = 0.
      DO I = 1, N
         SLOPE = SLOPE + G(I) * P(I)
      ENDDO
      TEST = 0.
      DO I = 1, N
         TEMP = ABS(P(I)) / MAX(ABS(XOLD(I)),1.)
         IF (TEMP .GT. TEST) TEST = TEMP
      ENDDO
      ALAMIN = TOLX / TEST
      ALAM = 1.
C
 1    CONTINUE
C
!bs
!bs * avoid negative concentrations and set upper limit given by CTOT.
!bs
         DO I = 1, N
            X(I) = XOLD(I) + ALAM * P(I)
            IF (X(I) .LE. 0.)      X(I) = CONMIN
            IF (X(I) .GT. CTOT(I)) X(I) = CTOT(I)
         ENDDO
         CALL FMINV( X,F )
         IF (ALAM .LT. ALAMIN) THEN
            DO I = 1, N
               X(I) = XOLD(I)
            ENDDO
            CHECK = .TRUE.
            RETURN
         ELSE IF (F .LE. FOLD + ALF * ALAM * SLOPE) THEN
            RETURN
         ELSE
            IF (ALAM .EQ. 1.) THEN
               TMPLAM = -SLOPE / (2. * (F - FOLD - SLOPE))
            ELSE
               RHS1 = F - FOLD - ALAM * SLOPE
               RHS2 = F2 - FOLD2 - ALAM2 * SLOPE
               A = (RHS1 / ALAM**2 - RHS2 / ALAM2**2) / (ALAM - ALAM2)
               B = (-ALAM2 * RHS1 / ALAM**2 + ALAM * RHS2 / ALAM2**2)
     &            /
     &             (ALAM - ALAM2)
               IF (A .EQ. 0.) THEN
                  TMPLAM = -SLOPE / (2. * B)
               ELSE
                  DISC  = B * B - 3. * A * SLOPE
                  TMPLAM = (-B + SQRT(DISC)) / (3. * A)
               ENDIF
               IF (TMPLAM .GT. 0.5*ALAM) TMPLAM = 0.5 * ALAM
            ENDIF
         ENDIF
         ALAM2 = ALAM
         F2 = F
         FOLD2 = FOLD
         ALAM = MAX(TMPLAM, 0.1 * ALAM)
      GOTO 1
C
      END SUBROUTINE LNSRCH
      
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      SUBROUTINE LUBKSB(
     &                  A, N, INDX, B
     &                 )
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 2.3, 2nd ed.   !
!bs                                                                    !
!bs  Solves the set of N linear equations A * X = B. Here A is input,  !
!bs  not as the matrix A but rather as its LU decomposition,           !
!bs  determined by the routine LUDCMP. B(1:N) is input as the right-   !
!bs  hand side vector B, and returns with the solution vector X. A, N, !
!bs  and INDX are not modified by this routine and can be left in      !
!bs  place for successive calls with different right-hand sides B.     !
!bs  This routine takes into account the possibility that B will begin !
!bs  with many zero elements, so it is efficient for use in matrix     !
!bs  inversion.                                                        !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
c    call vector modified to remove NCVAP and set dimensions to N.
      IMPLICIT NONE
!bs
      INTEGER N, INDX(N)
      REAL A(N,N), B(N) ! A now has dimension NxN.
C
      INTEGER I, II, J, LL
      REAL SUM
C
      II = 0
      DO I = 1, N
         LL = INDX(I)
         SUM = B(LL)
         B(LL) = B(I)
         IF (II .NE. 0) THEN
            DO J = II, I-1
               SUM = SUM - A(I,J) * B(J)
            ENDDO
         ELSE IF (SUM .NE. 0) THEN
            II = I
         ENDIF
         B(I) = SUM
      ENDDO
      DO I = N, 1, -1
         SUM = B(I)
         DO J = I+1, N
            SUM = SUM - A(I,J) * B(J)
         ENDDO
         B(I) = SUM / A(I,I)
      ENDDO
C
      RETURN
      END SUBROUTINE LUBKSB
      
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
      SUBROUTINE LUDCMP(
     &                  A, N, INDX, D
     &                 )
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 2.3, 2nd ed.   !
!bs                                                                    !
!bs  Equation (2.3.14) Numerical Recipes, p 36:                        !
!bs   | b_11 b_12 b_13 b_14 |                                          !
!bs   | a_21 b_22 b_23 b_24 |                                          !
!bs   | a_31 a_32 b_33 b_34 |                                          !
!bs   | a_41 a_42 a_43 b_44 |                                          !
!bs                                                                    !
!bs  Given a matrix A(1:N,1:N), with physical dimension N by N, this !
!bs  routine replaces it by the LU decomposition of a rowwise          !
!bs  permutation of itself. A and N are input. A is output arranged as !
!bs  in equation (2.3.14) above; INDX(1:N) is an output vector that    !
!bs  records vector that records the row permutation effected by the   !
!bs  partial pivoting; D is output as +-1 depending on whether the     !
!bs  number of row interchanges was even or odd, respectively. This    !
!bs  routine is used in combination with SR LUBKSB to solve linear     !
!bs  equations or invert a matrix.                                     !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
      IMPLICIT NONE
!bs
cc   call vector modified to remove NCVAP. 
cc   all dimensions now depend upon N only.
      INTEGER N, INDX(N)
cc      INTEGER NMAX
cc      PARAMETER (NMAX = 10)      !largest expected N
      REAL D, A(N,N) ! note that A now has dimension NxN
                     ! NCVAP is ignored
      REAL TINY
      PARAMETER (TINY = 1.0E-20)
C
      INTEGER I, IMAX, J, K
      REAL AAMAX, DUM, SUM, VV(N)
C
      D = 1
      DO I =1, N
         AAMAX = 0.
         DO J = 1, N
            IF (ABS(A(I,J)) .GT. AAMAX) AAMAX = ABS(A(I,J))
         ENDDO
         IF (AAMAX .EQ. 0) THEN
            WRITE(6,'(a)') 'Singular matrix in ludcmp!'
cc            STOP
         ENDIF
         VV(I) = 1. / AAMAX
      ENDDO
      DO J = 1, N
         DO I = 1, J-1
            SUM = A(I,J)
            DO K = 1, I-1
               SUM = SUM - A(I,K) * A(K,J)
            ENDDO
            A(I,J) = sum
         ENDDO
         AAMAX = 0.
         DO I = J, N
            SUM = A(I,J)
            DO K = 1, J-1
               SUM = SUM - A(I,K) * A(K,J)
            ENDDO
            A(I,J) = SUM
            DUM = VV(I) * ABS(SUM)
            IF (DUM .GE. AAMAX) THEN
               IMAX = I
               AAMAX = DUM
            ENDIF
         ENDDO
         IF (J .NE. IMAX) THEN
            DO K = 1, N
               DUM = A(IMAX,K)
               A(IMAX,K) = A(J,K)
               A(J,K) = DUM
            ENDDO
            D = -D
            VV(IMAX) = VV(J)
         ENDIF
         INDX(J) = IMAX
         IF (A(J,J) .EQ. 0.) A(J,J) = TINY
         IF (J .NE. N) THEN
            DUM = 1. / A(J,J)
            DO I = J+1, N
               A(I,J) = A(I,J) * DUM
            ENDDO
         ENDIF
      ENDDO
C
      RETURN
      END SUBROUTINE LUDCMP
      
      end module soa_NEWT     
      
