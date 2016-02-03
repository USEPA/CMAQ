SUBROUTINE ll2xy_lam (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Determines secant or tangent Lambert conformal case, and calls
!           appropriate routine.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           26 Nov 2008  Added argument for reference latitude, PHI0.
!                        Prevent users from having tangent Lambert conformal
!                        case until it can be tested with the Spatial
!                        Allocator.  (Known problem is that the Spatial
!                        Allocator does not work properly when the
!                        reference latitude is equal to the first true
!                        latitude.  Work-around is to set reference latitude
!                        to average of true latitudes for Lambert conformal.
!                        But average of true latiudes for tangent Lambert
!                        conformal case is the first true latitude, which
!                        will result in the same problem as solution used
!                        in MCIPv3.4.)  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL,          INTENT(IN)    :: phi0    ! reference latitude [deg]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL,          INTENT(IN)    :: phi2    ! true latitude 2 [deg]
  REAL,          PARAMETER     :: phitol  = 0.001  ! tolerance [deg]
  CHARACTER*16,  PARAMETER     :: pname   = 'LL2XY_LAM'
  REAL(8),       INTENT(OUT)   :: xx      ! X-coordinate from origin
  REAL(8),       INTENT(OUT)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Determine whether Lambert conformal is tangent or secant.
!-------------------------------------------------------------------------------

  IF ( ABS( phi1 - phi2 ) < phitol ) THEN  ! tangent case
    WRITE (6,9000) phi1, phi2
    GOTO 1001
!   CALL ll2xy_lam_tan (phi, lambda, phi1, lambda0, xx, yy)
  ELSE  ! secant case
    CALL ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                                  &
              /, 1x, '*** SUBROUTINE: LL2XY_LAM',                              &
              /, 1x, '***   TANGENT LAMBERT CONFORMAL PROJECTION DETECTED',    &
              /, 1x, '***   TRUE LATITUDES = ', f8.3, 2x, f8.3,                &
              /, 1x, '***   MAY NOT WORK PROPERLY IN SPATIAL ALLOCATOR',       &
              /, 1x, '***   ...PLEASE SUBMIT BUGZILLA TICKET TO INVESTIGATE',  &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE ll2xy_lam
