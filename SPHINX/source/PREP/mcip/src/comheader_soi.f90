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

SUBROUTINE comheader_soi (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Common Header (Soil)
! Purpose:  Builds a common header part for I/O API output.
! Revised:  10 Feb 2018  Initial version adapted from comheader.f90 in
!                        MCIPv4.4.  (T. Spero)
!-------------------------------------------------------------------------------

  USE coord
  USE m3utilio
  USE mcipparm
  USE xvars, ONLY: xzsoil

  IMPLICIT NONE

  INTEGER                      :: i
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

  vgtyp3d = vghval3
  vgtop3d = 0.0

  ! Layer defined in soil depths.

  vglvs3d(:) = -999.9  ! initialized to ensure monotonicity

  DO i = 1, metsoi
    vglvs3d(i) = xzsoil(i)
  ENDDO
 
  ! Initialize FDESC3D and UPDESC3D array.

  fdesc3d(1:mxdesc3) = ' '
  updsc3d(1:mxdesc3) = ' '

  fdesc3d(:) = fdesc(:)
  updsc3d(:) = fdesc(:)

END SUBROUTINE comheader_soi
