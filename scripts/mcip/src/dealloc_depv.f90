
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
! $Header: /project/work/rep/MCIP2/src/mcip2/dealloc_depv.F,v 1.4 2007/08/03 20:50:36 tlotte Exp $


SUBROUTINE dealloc_depv

!-------------------------------------------------------------------------------
! Name:     Deallocate Dry Deposition Variables
! Purpose:  Deallocate arrays for dry deposition.
! Revised:  19 Aug 2005  Original version.  (T. Otte and W. Hutzell)
!           27 Feb 2006  Added variable MESO.  (D. Schwede)
!           16 Jun 2006  Added variable SCC_PR_23.  (T. Otte)
!           24 Jul 2007  Removed variables for RADMdry.  Added molecular weights
!                        for dry deposition species.  (T. Otte)
!-------------------------------------------------------------------------------

  USE depvvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Deallocate dry deposition arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xdepspc   )
  DEALLOCATE ( xvd       )
  DEALLOCATE ( vd_c      )

  DEALLOCATE ( a         )
  DEALLOCATE ( alphs     )
  DEALLOCATE ( dif0      )
  DEALLOCATE ( dkhor     )
  DEALLOCATE ( kh        )
  DEALLOCATE ( meso      )
  DEALLOCATE ( molwt     )
  DEALLOCATE ( scc_pr_23 )
  DEALLOCATE ( subname   )

END SUBROUTINE dealloc_depv
