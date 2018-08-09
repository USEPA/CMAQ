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

SUBROUTINE comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Common Header
! Purpose:  Builds a common header part for I/O API output.
! Revised:  27 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  LSM include nonglobal changed.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           30 Jul 2007  Fill FDESC3D to create metadata.  (T. Otte)
!           11 Aug 2011  Replaced module FDESC3 with I/O API module M3UTILIO.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           10 Feb 2018  Reinitialize VGLVS3D on each call to accommodate
!                        additional 3D I/O API output files where the third
!                        dimension is not atmospheric layers.  (T. Spero)
!-------------------------------------------------------------------------------

  USE coord
  USE m3utilio
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

  vglvs3d(:)         = 0.0  ! initialized to ensure monotonicity
  vglvs3d(1:nlays+1) = vglvs_gd(1:nlays+1)
 
  ! Initialize FDESC3D and UPDESC3D array.

  fdesc3d(1:mxdesc3) = ' '
  updsc3d(1:mxdesc3) = ' '

  fdesc3d(:) = fdesc(:)
  updsc3d(:) = fdesc(:)

END SUBROUTINE comheader
