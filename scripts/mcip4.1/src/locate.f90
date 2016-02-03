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

SUBROUTINE locate (xx, x, j)

!-------------------------------------------------------------------------------
! Name:     Locate
! Purpose:  Locates index of value X for given monotonic array XX.
! Input:    XX:  array of monotonic values
!                For our application, it represents vertical coordinate values
!           X:   Value to bracket in XX
! Output:   J:   Index where X is located in XX
!                            XX(J) <= X < XX(J+1)  
!                 ***  I hope this is correct - test it DWB    
! Notes:    This routine is adapted from the Numerical Recipe.
! Revised:  ?? ??? 19??  Original version.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Removed N from
!                        argument list.  (T. Otte)
!           19 Mar 2002  Corrected upper limit on DO loop.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       INTENT(OUT)   :: j
  INTEGER                      :: lbnd
  REAL,          PARAMETER     :: small      = 1.0e-6
  INTEGER                      :: ubnd
  REAL,          INTENT(IN)    :: x
  REAL,          INTENT(IN)    :: xx         ( : )

  ! Step thru array 1 element at a time.

  lbnd = LBOUND(xx,1)
  ubnd = UBOUND(xx,1)

  DO j = lbnd, ubnd-1
    IF ( ( x+small >= xx(j) ) .AND. ( x < xx(j+1) ) ) EXIT
  ENDDO

END SUBROUTINE locate
