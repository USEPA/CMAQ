
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
C $Header: /project/yoj/arc/ICON/src/common/lr_interp.F,v 1.2 2011/10/21 16:41:52 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      
       SUBROUTINE LR_INTERP ( L_RATINT, XA, YA, N, X, Y, DELY )

C***********************************************************************
 
C  Function: Interpolates a value Y for a given X from the arrays XA and
C            YA. The flag L_RATINT determines whether linear or rational
C            function interpolation is done.
 
C  Preconditions: Extrapolation will be performed unless controlled by 
C                 the calling routine
  
C  Key Subroutines/Functions Called: None
 
C  Revision History:
C     Prototype created by Jerry Gipson, January, 1998
C     Rational Function Interpolation is from Numerical Recipes
C     (Press et al., 19??)
C     Linear interpolation equation modified by JG 6/1/99 to better treat
C     large conc gradients
C     Improved Linear interpolation algorithm by JG 4/18/00 for interpolants
C     close to interval end points
C     UTILIO_DEFN for M3EXIT by J.Young 6/9/11
C     replaced UTILIO_DEFN with M3UTILIO

C***********************************************************************

      USE M3UTILIO   ! IOAPI module

      IMPLICIT NONE 

      
C Arguments:
      LOGICAL, INTENT( IN ) :: L_RATINT  ! Flag for rational function interpolation

      REAL, INTENT( IN )  :: XA( : )     ! Independent variable array
      REAL, INTENT( IN )  :: YA( : )     ! Dependent variable array
      REAL, INTENT( IN )  :: X           ! Value of independent variable to be interpolated
      REAL, INTENT( OUT ) :: Y           ! Interpolated value of dependent variable
      REAL, INTENT( OUT ) :: DELY        ! Error estimate for rational function interpolation

      INTEGER, INTENT( IN ) :: N         ! Number of values in arrays XA and YA
                                           
C Parameters:
      INTEGER, PARAMETER :: NMAX = 100  ! Maximum number of points in arrays AX and YA
      REAL,    PARAMETER :: TINY = 1.0E-35   ! Tiny number
      REAL,    PARAMETER :: EPS  = 1.0E-05   ! Small number

C External Functions: None

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'LR_INTERP'    ! Procedure Name
      CHARACTER( 80 ) :: MSG      ! Log message

      INTEGER I, M           ! Loop indices
      INTEGER NS             ! Rat Func temporary variable

      REAL    DX             ! Incremental delta of independent variable
!     REAL    DY             ! Incremental delta of dependent variable
      REAL    SX             ! Incremental independent value for interpolation 
      REAL    SLP            ! Slope for linear interpolation

      REAL    H, HH, T, DD, W   ! Rat Func temporary variables

      REAL    :: C( NMAX )   ! Rat Func temporary variable
      REAL    :: D( NMAX )   ! Rat Func temporary variable

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Linear interpolation section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. L_RATINT ) THEN

         DELY = 0.0

         IF ( ( XA( 1 ) .LT. XA( 2 )  .AND. X .LE. XA( 1 ) ) .OR.
     &        ( XA( 1 ) .GT. XA( 2 )  .AND. X .GE. XA( 1 ) ) ) THEN 

            DX = XA( 2 ) - XA( 1 )

            IF ( DX .EQ. 0.0 ) THEN
               MSG = 'Invalid Independent variables for interpolation'
               CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
            END IF

            Y = YA( 1 ) + ( ( X - XA( 1 ) ) / DX ) * YA( 1 )

            RETURN

         END IF

         IF ( ( XA( N ) .GT. XA( N - 1 ) .AND. X .GE. XA( N ) ) .OR.
     &        ( XA( N ) .LT. XA( N - 1 ) .AND. X .LE. XA( N ) ) ) THEN 

            DX = XA( N ) - XA( N - 1 )

            IF ( DX .EQ. 0.0 ) THEN
               MSG = 'Invalid Independent variables for interpolation'
               CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
            END IF

            Y = YA( N ) + ( ( X - XA( N ) ) / DX ) * YA( N - 1 )

            RETURN

         END IF

         DO I = 1, N - 1

            DX = ABS( XA( I + 1 ) - XA( I ) )
            IF ( DX .EQ. 0.0 ) THEN
               MSG = 'Invalid Independent variables for interpolation'
               CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
            END IF
!           DY = YA( I + 1 ) - YA( I )
            SX = ABS( X - XA( I ) )

            IF ( SX - DX .LT. EPS ) THEN

!              Y = YA( I ) + ( ( X - XA( I ) ) / 
!     &            ( XA( I + 1 ) - XA( I ) ) ) * DY

               SLP = ( X - XA( I ) ) / ( XA( I + 1 ) - XA( I ) )
               IF ( SLP .GT. 0.99999 ) SLP = 1.0
               IF ( SLP .LT. 0.00001 ) SLP = 0.0

               Y = ( 1.0 - SLP ) * YA( I ) + SLP * YA( I+1 )

               RETURN

            END IF

         END DO

         MSG = 'No interval found for linear interpolation'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )

      END IF
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Rational function interpolation section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NS = 1
      HH = ABS( X - XA( 1 ) )

      DO I = 1, N
         H = ABS( X -XA( I ) )
         IF ( H .EQ. 0.0 ) THEN
            Y = YA( I )
            DELY = 0.0
            RETURN
         ELSE IF ( H .LT. HH ) THEN
            NS = I
            HH = H
         END IF
         C( I ) = YA( I )
         D( I ) = YA( I ) + TINY
      END DO

      Y = YA( NS )
      NS = NS - 1

      DO M = 1, N - 1
         DO I = 1, N - M
            W = C( I + 1 ) - D( I )
            H = XA( I + M ) - X
            T = ( XA( I ) - X ) * D( I ) / H
            DD = T - C( I + 1 )

            IF ( DD .EQ. 0.0 ) THEN
               MSG = 'Rational function interpolation error'
               CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
            END IF
            DD = W / DD
            D( I ) = C( I + 1 ) * DD
            C( I ) = T * DD
         END DO

         IF ( 2 * NS .LT. N - M ) THEN
            DELY = C( NS + 1 )
         ELSE
            DELY = D( NS )
            NS = NS - 1
         END IF

         Y = Y + DELY

      END DO

      RETURN

      END
