!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE collapx (aa, vertin, vertout)

!-------------------------------------------------------------------------------
! Name:     Collapse X Arrays
! Purpose:  Collapses a meteorological 3D array AA with layers defined in
!           VERTIN down to layers defined in VERTOUT.  Result is in AA.
! Notes:    Array AA is used for both input/output to save memory of the
!           calling routines (GRIDOUT.F, METCRO.F, METDOT.F)   
! Revised:  22 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           23 Feb 1998  Corrected collapsing routine.  (R. Tang and D. Byun)
!           10 Mar 1998  Corrected collapsing routine.  (D. Byun)
!           21 Apr 2000  Added if-then-elseif for preventing array index
!                        out of scope problem when the top layer is NOT
!                        collapsed.  (D. Byun)
!           20 Sep 2001  Converted to free-form f90.  Restructured argument
!                        list.  Removed dependence on all modules.  (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           08 Jul 2004  Added interface block for subroutine ratint.  (T. Otte)
!           19 Aug 2005  Removed option to call subroutine ratint, and modified
!                        arguments to reflect that the only option for layer
!                        collapsing is linear interpolation.  (T. Otte)
!           19 Jun 2006  Removed unused variables DY, K, and Y.  (T. Otte)
!           29 Aug 2011  Improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,               INTENT(INOUT) :: aa         ( : , : , : )
  INTEGER                           :: i
  INTEGER,            ALLOCATABLE   :: ilays      ( : )
  INTEGER                           :: imax
  INTEGER                           :: j
  INTEGER                           :: jmax
  INTEGER                           :: kin
  INTEGER                           :: kout
  INTEGER                           :: lay
  INTEGER                           :: lbnd
  INTEGER                           :: ll
  INTEGER                           :: nlend
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'COLLAPX'
  REAL,               INTENT(IN)    :: vertin     ( : )
  REAL,               INTENT(IN)    :: vertout    ( : )
  REAL,               ALLOCATABLE   :: wgt        ( : )
  REAL,               ALLOCATABLE   :: workb      ( : )

  INTERFACE

    SUBROUTINE locate (xx, x, j)
      IMPLICIT NONE
      INTEGER,            INTENT(OUT)   :: j
      REAL,               INTENT(IN)    :: x
      REAL,               INTENT(IN)    :: xx         ( : )
    END SUBROUTINE locate

  END INTERFACE

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   INPUT ARRAY SIZES DO NOT MATCH', &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Get array dimensions to use as loop bounds.
!-------------------------------------------------------------------------------

  IF ( ( SIZE(aa,3) /= SIZE(vertin,1)  ) .AND.  &
       ( SIZE(aa,3) /= SIZE(vertout,1) ) ) THEN
    WRITE (*,f9000) TRIM(pname)
    CALL graceful_stop (pname)
  ENDIF

  imax  = SIZE(aa,1)
  jmax  = SIZE(aa,2)
  kin   = SIZE(vertin,1)
  kout  = SIZE(vertout,1)

  lbnd  = LBOUND(aa,3)
  nlend = lbnd + kout - 1

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( ilays ( kout ) )
  ALLOCATE ( wgt   ( kout ) )
  ALLOCATE ( workb ( kout ) )

!-------------------------------------------------------------------------------
! Compute output layer coordinates.
!-------------------------------------------------------------------------------

  DO lay = 1, kout

    CALL locate (vertin, vertout(lay), ll)
    IF ( ll >= kin ) THEN
      wgt(lay)   = 1.0
      ilays(lay) = kin - 1
    ELSE
      wgt(lay)   = ( vertout(lay) - vertin(ll) ) /  &
                   ( vertin(ll+1) - vertin(ll) )
      ilays(lay) = ll
    ENDIF

  ENDDO

!-------------------------------------------------------------------------------
! Linear interpolation:  locate nearest points then interpolate linearly.
!-------------------------------------------------------------------------------

  DO i = 1, imax
    DO j = 1, jmax

      DO lay = 1, kout
        ll = ilays(lay)
        workb(lay) = (1.0-wgt(lay)) * aa(i,j,lbnd+ll-1) +  &
                          wgt(lay)  * aa(i,j,lbnd+ll)
      ENDDO

      aa(i,j,lbnd:nlend) = workb
      aa(i,j,nlend+1:)   = 0.0

    ENDDO
  ENDDO
            
!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( ilays )
  DEALLOCATE ( wgt   )
  DEALLOCATE ( workb )

END SUBROUTINE collapx
