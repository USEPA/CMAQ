
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/tridiag.f,v 1.5 2011/10/29 01:03:56 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)tridiag.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.tridiag.F 23 May 1997 12:44:32

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TRIDIAG ( A, B, E, D, U, N )
C-----------------------------------------------------------------------
C
C  FUNCTION:
C    Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C    if first pivot is zero.  In that case, rewrite the
C    equation as a set of order N-1, with U(2) trivially eliminated.
C The associated tri-diagonal system is stored in 3 arrays
C   B: diagonal
C   A: sub-diagonal
C   E: super-diagonal
C   D: right hand side function
C   U : return solution from tridiagonal solver
C
C     [ B(1) E(1) 0    0    0 ...       0     ]
C     [ A(2) B(2) E(2) 0    0 ...       .     ]
C     [ 0    A(3) B(3) E(3) 0 ...       .     ]
C     [ .       .     .     .           .     ] U(i) = D(i)
C     [ .             .     .     .     0     ]
C     [ .                   .     .     .     ]
C     [ 0                           A(n) B(n) ]
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION HISTORY:
C    NO.   DATE     WHO      WHAT
C    __    ____     ___      ____
C    4     4/3/96    SJR  copied code and modified for use in JPROC
C    3     8/16/94   XKX  configuration management include statements
C    2     3/15/92   CJC  For use in Models-3 LCM.
C    1     10/19/89  JKV  converted for use on IBM
C    0      3/89     BDX  Initial version
C                    yoj
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, PARAMETER :: NMAX = 400

C...ARGUMENTS and their descriptions:

      INTEGER     N                ! number of rows in matrix

      REAL        A( NMAX )        ! subdiagonal
      REAL        B( NMAX )        ! diagonal
      REAL        E( NMAX )        ! superdiagonal
      REAL        D( NMAX )        ! R.H. side
      REAL        U( NMAX )        ! solution

C SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER     J                ! loop index

      REAL        BET              !
      REAL        GAM( NMAX )      ! 

C...begin body of subroutine  TRIDIAG
C...  Decomposition and forward substitution:

      BET = 1.0 / B( 1 )
      U( 1 ) = BET * D( 1 )

      DO J = 2, N   
        GAM( J ) = BET * E( J - 1 )
        BET = 1.0 / ( B( J ) - A( J ) * GAM( J ) )
        U( J ) = BET * ( D( J ) - A( J ) * U( J - 1) )
      END DO

C...Back-substitution:

      DO J = N - 1, 1, -1  
        U( J ) = U( J ) - GAM( J + 1 ) * U( J + 1 )
      END DO

      RETURN
      END
