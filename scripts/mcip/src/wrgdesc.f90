
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
! $Header: /project/work/rep/MCIP2/src/mcip2/wrgdesc.F,v 1.2 2004/08/30 16:43:31 tlotte Exp $ 


SUBROUTINE wrgdesc

!-------------------------------------------------------------------------------
! Name:     Write Grid Description
! Purpose:  Writes grid description (GRIDDESC) file.
! Revised:  03 Oct 2001  Original version.  (T. Otte)
!           07 Jan 2002  Added OPEN and CLOSE statements for output file.
!                        (S. Howard and T. Otte)
!           30 Jan 2002  Changed the creation of the GRIDDESC file from
!                        a list-directed write to a formatted write to
!                        improve portability.  (T. Otte)
!           01 Jul 2004  Corrected minor formatting bugs.  (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE file
  USE mcipparm

  IMPLICIT NONE

  CHARACTER*1,   PARAMETER     :: blank      = ' '
  CHARACTER*1,   PARAMETER     :: quote      = CHAR(39)

!-------------------------------------------------------------------------------
! Write grid description in two sets.  Reader uses two list-directed READ
! statements to capture information.
!-------------------------------------------------------------------------------

  OPEN (iutgd, FILE=file_gd)

  WRITE (iutgd, 100) quote // blank // quote

  WRITE (iutgd, 100) quote // TRIM(coordnam_gd) // quote
  WRITE (iutgd, 200) gdtyp_gd, p_alp_gd, p_bet_gd, p_gam_gd, xcent_gd, ycent_gd
  WRITE (iutgd, 100) quote // blank // quote

  WRITE (iutgd, 100) quote // TRIM(gdname_gd) // quote
  WRITE (iutgd, 300) quote // TRIM(coordnam_gd) // quote, xorig_gd, yorig_gd,  &
                     xcell_gd, ycell_gd, ncols, nrows, nthik
  WRITE (iutgd, 100) quote // blank // quote

  CLOSE (iutgd)

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 100 FORMAT (a)
 200 FORMAT (i3, 5(1x, f13.3))
 300 FORMAT ( a, 4(1x, f13.3), 3(1x, i3))

END SUBROUTINE wrgdesc
