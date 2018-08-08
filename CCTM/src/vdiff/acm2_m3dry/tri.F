
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/tri.F,v 1.4 2011/10/21 16:11:46 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TRI ( L, D, U, B, X )

C-----------------------------------------------------------------------
C Function:
C    Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C    ( M3ERR ) if first pivot is zero.  In that case, rewrite the
C    equation as a set of order KMAX-1, with X(2) trivially eliminated.
C The associated tri-diagonal system is stored in 3 arrays
C   D : diagonal
C   L : sub-diagonal
C   U : super-diagonal
C   B : right hand side function
C   X : return solution from tridiagonal solver

C   [ D(1) U(1) 0    0    0 ...       0     ]
C   [ L(2) D(2) U(2) 0    0 ...       .     ]
C   [ 0    L(3) D(3) U(3) 0 ...       .     ]
C   [ .       .     .     .           .     ] X(i) = B(i)
C   [ .             .     .     .     0     ]
C   [ .                   .     .     .     ]
C   [ 0                           L(n) D(n) ]

C   where n = NLAYS

C Preconditions:

C Subroutines and Functions Called:

C Revision History:
C    No.   Date      Who     What
C    __    ____      ___     ____
C    6     Jun 10    yoj  convert for Namelist redesign
C    5     Apr 06    JEP  adapted tridiag for use with ACM2
C    4     Aug 96    yoj  cleaner
C    3     8/16/94   XKX  configuration management include statements
C    2     3/15/92   CJC  For use in Models-3 LCM.
C    1     10/19/89  JKV  converted for use on IBM
C    0      3/89     BDX  Initial version
C-----------------------------------------------------------------------

      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species

      IMPLICIT NONE

C Includes: None

C Arguments:

      REAL, INTENT( IN )  :: L( : )        ! subdiagonal
      REAL, INTENT( IN )  :: D( : )        ! diagonal
      REAL, INTENT( IN )  :: U( : )        ! superdiagonal
      REAL, INTENT( IN )  :: B( :,: )      ! R.H. side
      REAL, INTENT( OUT ) :: X( :,: )      ! solution

C Local Variables:

      INTEGER, SAVE :: N_SPC_DIFF          ! global diffusion species
      REAL        GAM( NLAYS )
      REAL        BET
      INTEGER     V, K
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         N_SPC_DIFF = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_DIFF
      END IF   ! FIRSTIME

C Decomposition and forward substitution:
      BET = 1.0 / D( 1 )
      DO V = 1, N_SPC_DIFF
         X( V,1 ) = BET * B( V,1 )
      END DO

      DO K = 2, NLAYS
         GAM( K ) = BET * U( K-1 )
         BET = 1.0 / ( D( K ) - L( K ) * GAM( K ) )
         DO V = 1, N_SPC_DIFF
            X( V,K ) = BET * ( B( V,K ) - L( K ) * X( V,K-1 ) )
         END DO
      END DO

C Back-substitution:

      DO K = NLAYS - 1, 1, -1
         DO V = 1, N_SPC_DIFF
            X( V,K ) = X( V,K ) - GAM( K+1 ) * X( V,K+1 )
         END DO
      END DO

      RETURN
      END
