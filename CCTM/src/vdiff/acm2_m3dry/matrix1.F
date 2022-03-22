
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/matrix.F,v 1.5 2011/10/21 16:11:45 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE MATRIX1 ( KL, A, B, E, D, X )

C Rather than solving the ACM2 banded tridiagonal matrix using LU decomposition,
C it is much faster to split the solution into the ACM1 convective solver followed
C by the tridiagonal solver
C MATRIX1 is the ACM1 solver. When the PBL is convective, this solver is called
C followed by TRI. If not convective, only TRI is called.

C-- ACM1 Matrix is in this form (there is no subdiagonal:
C   B1 E2                     <- note E2 (flux from layer above), not E1
C   A2 B2 E3
C   A3    B3 E4
C   A4       B4 E5
C   A5          B5 E6
C   A6             B6

      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE

C Includes:
      INTEGER, SAVE :: N_SPC_DIFF    ! global diffusion species

C Arguments:
      INTEGER, INTENT( IN )  :: KL         ! CBL sigma height
      REAL,    INTENT( IN )  :: A( : )     ! matrix column one
      REAL,    INTENT( IN )  :: B( : )     ! diagonal
      REAL,    INTENT( IN )  :: E( : )     ! superdiagonal
      REAL,    INTENT( IN )  :: D( :,: )   ! R.H.S
      REAL,    INTENT( OUT ) :: X( :,: )   ! returned solution

C Locals:
      REAL, ALLOCATABLE, SAVE :: BETA( : )
      REAL  ALPHA, GAMA 

      INTEGER L, V, IOS

      CHARACTER( 120 ) :: XMSG = ' '
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         N_SPC_DIFF = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_DIFF
         ALLOCATE ( BETA( N_SPC_DIFF ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            XMSG = 'Failure allocating BETA'
            CALL M3EXIT( 'MATRIX', 0, 0, XMSG, XSTAT1 )
         END IF
      END IF   ! FIRSTIME

C-- ACM1 matrix solver

      DO V = 1, N_SPC_DIFF
         BETA( V ) = D( V,1 )
      END DO
      GAMA = B( 1 )
      ALPHA = 1.0

      DO L = 2, KL
         ALPHA = -ALPHA * E( L ) / B( L )
         DO V = 1, N_SPC_DIFF
            BETA( V ) = ALPHA * D( V,L ) + BETA( V )
         END DO
         GAMA = GAMA + ALPHA * A( L )
      END DO

      DO V = 1, N_SPC_DIFF
         X( V,1 )  = BETA( V ) / GAMA
         X( V,KL ) = ( D( V,KL ) - A( KL ) * X( V,1 ) ) / B( KL )
      END DO

C-- Back sub for Ux=y

      DO L = KL-1, 2, -1
         DO V = 1, N_SPC_DIFF
            X( V,L ) = ( D( V,L ) - A( L ) * X( V,1 ) - E( L+1 ) * X( V,L+1 ) ) / B( L )
         END DO
      END DO

      RETURN
      END

