
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/matrix.F,v 1.4 2011/10/21 16:11:42 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE MATRIX ( A, B, C, D, E, X )

C---------------------------------------------------------
C-- Bordered band diagonal matrix solver for ACM2

C-- ACM2 Matrix is in this form:
C   B1 E1
C   A2 B2 E2
C   A3 C3 B3 E3
C   A4    C4 B4 E4
C   A5       C5 B5 E5
C   A6          C6 B6

C--Upper Matrix is
C  U11 U12
C      U22 U23
C          U33 U34
C              U44 U45
C                  U55 U56
C                      U66

C--Lower Matrix is:
C  1
C L21  1
C L31 L32  1
C L41 L42 L43  1
C L51 L52 L53 L54  1
C L61 L62 L63 L64 L65 1
C---------------------------------------------------------

      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE

C Includes:

      INTEGER, SAVE :: N_SPC_DIFF    ! global diffusion species

C Arguments:

      REAL, INTENT( IN )  :: A( : )     ! matrix column one
      REAL, INTENT( IN )  :: B( : )     ! diagonal
      REAL, INTENT( IN )  :: C( : )     ! subdiagonal
      REAL, INTENT( IN )  :: E( : )     ! superdiagonal
      REAL, INTENT( IN )  :: D( :,: )   ! R.H.S
      REAL, INTENT( OUT ) :: X( :,: )   ! returned solution

C Locals:

      REAL, ALLOCATABLE, SAVE :: Y( :,: )
      REAL L  ( NLAYS,NLAYS )
      REAL U  ( NLAYS )
      REAL UP1( NLAYS )
      REAL RU ( NLAYS )
      REAL DD, DD1, YSUM

      INTEGER I, J, V, IOS

      CHARACTER( 120 ) :: XMSG = ' '
      LOGICAL :: FIRSTIME = .TRUE.

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         N_SPC_DIFF = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_DIFF
         ALLOCATE ( Y( N_SPC_DIFF,NLAYS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            XMSG = 'Failure allocating Y'
            CALL M3EXIT( 'MATRIX', 0, 0, XMSG, XSTAT1 )
         END IF
      END IF   ! FIRSTIME

C-- Define Upper and Lower matrices

      L( 1,1 ) = 1.0
      U( 1 ) = B( 1 )
      RU( 1 ) = 1.0 / B( 1 )

      DO I = 2, NLAYS
         L( I,I ) = 1.0
         L( I,1 ) = A( I ) / B( 1 )
         UP1( I-1 ) = E( I-1 )
      END DO

      DO I = 3, NLAYS
         DO J = 2, I - 2
            DD = B( J ) - L( J,J-1 ) * E( J-1 )
            L( I,J ) = - L( I,J-1 ) * E( J-1 ) / DD
         END DO
         J = I - 1
         DD = B( J ) - L( J,J-1 ) * E( J-1 )
         L( I,J ) = ( C( I ) - L( I,J-1 ) * E( J-1 ) ) / DD
      END DO

      DO I = 2, NLAYS
         U( I ) = B( I ) - L( I,I-1 ) * E( I-1 )
         RU( I ) = 1.0 / U( I )
      END DO

C-- Forward sub for Ly=d

      DO V = 1, N_SPC_DIFF
         Y( V,1 ) = D( V,1 )
         DO I = 2, NLAYS
            YSUM = D( V,I )
            DO J = 1, I-1
               YSUM = YSUM - L( I,J ) * Y( V,J )
            END DO
            Y( V,I ) = YSUM
         END DO
      END DO

C-- Back sub for Ux=y

      DO V= 1, N_SPC_DIFF
         X( V,NLAYS ) = Y( V,NLAYS ) * RU( NLAYS )
      END DO

      DO I = NLAYS - 1, 1, -1
         DD = RU( I )
         DD1 = UP1( I )
         DO V = 1, N_SPC_DIFF
            X( V,I ) = ( Y( V,I ) - DD1 * X( V,I+1 ) ) * DD
         END DO
      END DO

      RETURN
      END

