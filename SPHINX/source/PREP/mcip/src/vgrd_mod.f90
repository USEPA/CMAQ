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

MODULE vgrd

!-------------------------------------------------------------------------------
! Name:     Vertical Grid Dimension Parameters
! Purpose:  Contains vertical grid dimension parameters.
! Revised:  ?? ??? 19??  Original version.  (???)
!           10 Sep 2001  Converted to free-form f90.  Removed explicit
!                        definition of NLAYS, and changed X3FACE and X3MIDL
!                        to allocatable arrays.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          ALLOCATABLE   :: x3face     ( : )  ! vert coord lyr sfc
  REAL,          ALLOCATABLE   :: x3midl     ( : )  ! vert coord lyr ctr

END MODULE vgrd
