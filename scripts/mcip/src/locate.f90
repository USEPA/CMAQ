
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/locate.F,v 1.2 2002/03/20 12:33:40 yoj Exp $ 


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
