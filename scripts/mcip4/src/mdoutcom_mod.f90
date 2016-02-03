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

MODULE mdoutcom

!-------------------------------------------------------------------------------
! Name:     Meteorology Dot-Point Output Common Blocks
! Purpose:  Contains MCIP meteorology dot-point output common blocks.
! Revised:  27 Jan 1997  Original version.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Changed grid-dependent
!                        arrays to allocatable.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!           23 Sep 2009  Add user option to output u- and v-component winds on
!                        C-staggered grid.  (T. Otte)
!           31 Aug 2011  Changed F77 character declarations to F90 standard.
!                        Replaced DATA statements with parameters.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Time dependent dot 3D arrays for CTM domain.  (MET_DOT_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: md3index = 4

  REAL, ALLOCATABLE, TARGET :: md3        ( : , : , : , : )

  REAL, POINTER :: uu_d       ( : , : , : )  ! U-comp of true wind [m/s]
                                             ! at dot point
  REAL, POINTER :: vv_d       ( : , : , : )  ! V-comp of true wind [m/s]
                                             ! at dot point
  REAL, POINTER :: uhat_s     ( : , : , : )  ! contravar-U multiplied by
                                             ! DENSITY*JACOBIAN at X-dir
                                             ! flux pt (square point)
  REAL, POINTER :: vhat_t     ( : , : , : )  ! contravar-V multiplied by
                                             ! DENSITY*JACOBIAN at Y-dir
                                             ! flux pt (triangle point)

  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: md3vname ( md3index ) =  &
    (/ 'UWIND  ',  'VWIND  ',  'UHAT_JD',  'VHAT_JD'  /)

  CHARACTER(LEN=16), PARAMETER :: md3units ( md3index ) =  &
    (/ 'M/S     ', 'M/S     ', 'KG/(M*S)', 'KG/(M*S)' /)

  CHARACTER(LEN=80), PARAMETER :: md3vdesc ( md3index ) =   &
    (/ 'U-comp. of true wind at dot point                ', &  !  1
       'V-comp. of true wind at dot point                ', &  !  2
       '(contravariant_U*Jacobian*Density) at square pt  ', &  !  3
       '(contravariant_V*Jacobian*Density) at triangle pt' /) !  4

  REAL, ALLOCATABLE :: uu_s ( : , : , : )  ! u-component wind on C grid
  REAL, ALLOCATABLE :: vv_t ( : , : , : )  ! v-component wind on C grid

END MODULE mdoutcom
