
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
! $Header: /project/work/rep/MCIP2/src/mcip2/mdoutcom_mod.F,v 1.2 2007/08/03 20:48:25 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Time dependent dot 3D arrays for CTM domain.  (MET_DOT_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: md3index   = 4

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

  ! For header information.

  CHARACTER*16 :: md3vname ( md3index ) 
  CHARACTER*16 :: md3units ( md3index ) 
  CHARACTER*80 :: md3vdesc ( md3index ) 

  ! Header description.

  DATA md3vname / 'UWIND',   'VWIND',   'UHAT_JD',  'VHAT_JD'  /

  DATA md3units / 'M/S',     'M/S',     'KG/(M*S)', 'KG/(M*S)' /

  DATA md3vdesc(1) / 'U-comp. of true wind at dot point'          /   
  DATA md3vdesc(2) / 'V-comp. of true wind at dot point'          /   
  DATA md3vdesc(3) / '(contra_U*Jacobian*Density) at square pt'   /   
  DATA md3vdesc(4) / '(contra_V*Jacobian*Density) at triangle pt' /   

  REAL, ALLOCATABLE :: uu_s ( : , : , : )  ! u-component wind on C grid
  REAL, ALLOCATABLE :: vv_t ( : , : , : )  ! v-component wind on C grid

END MODULE mdoutcom
