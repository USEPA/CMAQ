
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
! $Header: /project/work/rep/MCIP2/src/mcip2/comheader.F,v 1.2 2007/08/03 20:52:47 tlotte Exp $ 


SUBROUTINE comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Common Header
! Purpose:  Builds a common header part for I/O API output.
! Revised:  27 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  LSM include nonglobal changed.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           30 Jul 2007  Fill FDESC3D to create metadata.  (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE fdesc3
  USE mcipparm

  IMPLICIT NONE

  INTEGER,       INTENT(IN)    :: sdate       ! YYYYDDD
  INTEGER,       INTENT(IN)    :: stime       ! HHMMSS

!-------------------------------------------------------------------------------
! Fill common headers from MODULE COORD.
!-------------------------------------------------------------------------------

  sdate3d = sdate
  stime3d = stime

  gdnam3d = gdname_gd
  gdtyp3d = gdtyp_gd
  p_alp3d = p_alp_gd
  p_bet3d = p_bet_gd
  p_gam3d = p_gam_gd

  xcent3d = xcent_gd
  ycent3d = ycent_gd
  xorig3d = xorig_gd
  yorig3d = yorig_gd
  xcell3d = xcell_gd
  ycell3d = ycell_gd

  vgtyp3d = vgtyp_gd
  vgtop3d = vgtop_gd

  ! Layer defined in standard met. coordinate.

  vglvs3d(1:nlays+1) = vglvs_gd(1:nlays+1)
 
  ! Initialize FDESC3D and UPDESC3D array.

  fdesc3d(1:mxdesc3) = ' '
  updsc3d(1:mxdesc3) = ' '

  fdesc3d(:) = fdesc(:)
  updsc3d(:) = fdesc(:)

END SUBROUTINE comheader
