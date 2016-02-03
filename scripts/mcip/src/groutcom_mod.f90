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

MODULE groutcom

!-------------------------------------------------------------------------------
! Name:     Grid Output Common Blocks
! Purpose:  Contains MCIP grid output common blocks.
! Revised:  27 Jan 1997  Original version.  (D. Byun)
!           20 May 1997  For Models-3 Beta Version.  (???)
!           30 Apr 1999  Replaced PSTAR with PRSFC.  (???)
!           02 May 2000  Replace NBNDY with NBDNYD.  (???)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           09 Jun 2003  Removed extraneous variables from output.  (T. Otte)
!           01 Jul 2004  Restored GRIDBDY2D file to output.  (T. Otte)
!           01 Dec 2004  Added PURB.  (T. Otte)
!           14 Jul 2006  Added LWMASK.  (T. Otte)
!           11 Apr 2007  Added IMPLICIT NONE.  Added fractional land use.
!                        Added description of source of land use classification
!                        to DLUSE.  (T. Otte)
!           07 Apr 2008  Added space in variable description of DLUSE to set
!                        land-use classification apart from generic
!                        description.  (T. Otte)
!           17 Sep 2009  Added latitude, longitude, and map-scale factors
!                        squared to GRIDDOT2D.  (T. Otte)
!           01 Sep 2011  Changed F77 character declarations to F90 standard.
!                        Replaced DATA statements with parameters.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Time independent dot 2D arrays for CTM domain.  (GRID_DOT_2D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: gd2index   = 9

  REAL, ALLOCATABLE, TARGET :: gd2        ( : , : , : )

  REAL, POINTER :: glat_d     ( : , : )  ! lat dot (S- ; N+) [deg]
  REAL, POINTER :: glon_d     ( : , : )  ! lon dot (W- ; E+) [deg]
  REAL, POINTER :: gmsfsq_d   ( : , : )  ! sq map scale factor on dot
  REAL, POINTER :: glatu_d    ( : , : )  ! lat U (S- ; N+) [deg]
  REAL, POINTER :: glonu_d    ( : , : )  ! lon U (W- ; E+) [deg]
  REAL, POINTER :: gmsfusq_d  ( : , : )  ! sq map scale factor on U
  REAL, POINTER :: glatv_d    ( : , : )  ! lat V (S- ; N+) [deg]
  REAL, POINTER :: glonv_d    ( : , : )  ! lon V (W- ; E+) [deg]
  REAL, POINTER :: gmsfvsq_d  ( : , : )  ! sq map scale factor on V

  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: gd2vname ( gd2index ) = &
    (/ 'LATD ',   'LOND ',   'MSFD2',     'LATU ',        &
       'LONU ',   'MSFU2',   'LATV ',     'LONV ',        &
       'MSFV2' /)

  CHARACTER(LEN=16), PARAMETER :: gd2units ( gd2index ) = &
    (/ 'DEGREES ','DEGREES ','(M/M)**2',  'DEGREES ',     &
       'DEGREES ','(M/M)**2','DEGREES ',  'DEGREES ',     &
       '(M/M)**2' /)

  CHARACTER(LEN=80), PARAMETER :: gd2vdesc ( gd2index ) = &
    (/ 'latitude (south negative) -- dot point         ', &  !  1
       'longitude (west negative) -- dot point         ', &  !  2
       'squared map-scale factor (DOT)                 ', &  !  3
       'latitude (south negative) -- U face            ', &  !  4
       'longitude (west negative) -- U face            ', &  !  5
       'squared map-scale factor (U FACE)              ', &  !  6
       'latitude (south negative) -- V face            ', &  !  7
       'longitude (west negative) -- V face            ', &  !  8
       'squared map-scale factor (V FACE)              '  /) !  9

!-------------------------------------------------------------------------------
! Time independent cross 2d arrays for CTM domain.  (GRID_CRO_2D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: gc2index   = 6

  REAL, ALLOCATABLE, TARGET :: gc2        ( : , : , : )

  REAL, POINTER :: glat_c     ( : , : )     ! lat (S- ; N+) [deg]
  REAL, POINTER :: glon_c     ( : , : )     ! lon (W- ; E+) [deg]
  REAL, POINTER :: gmsfsq_c   ( : , : )     ! sq map scale factor
  REAL, POINTER :: gtopo_c    ( : , : )     ! elevation AGL [m]
  REAL, POINTER :: gdluse_c   ( : , : )     ! dominant land use category
  REAL, POINTER :: glwmask_c  ( : , : )     ! land-water mask (1=land, 0=water)

  ! Optional output variables, if fractional land use data are available
  REAL, POINTER :: gpurb_c    ( : , : )     ! percentage of urban area
  REAL, POINTER :: glufrac_c  ( : , : , : ) ! fractional land use by category

  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: gc2vname ( gc2index ) = &
    (/ 'LAT   ',  'LON   ',  'MSFX2 ',    'HT    ',       &
       'DLUSE ',  'LWMASK' /)

  CHARACTER(LEN=16), PARAMETER :: gc2units ( gc2index ) = &
    (/ 'DEGREES ','DEGREES ','(M/M)**2',  'M       ',     &
       'CATEGORY','CATEGORY' /)

  CHARACTER(LEN=80), PARAMETER :: gc2vdesc ( gc2index ) = &
    (/ 'latitude (south negative)                      ', &  !  1
       'longitude (west negative)                      ', &  !  2
       'squared map-scale factor (CROSS)               ', &  !  3
       'terrain elevation                              ', &  !  4
       'dominant land use category from classification ', &  !  5
       'land-water mask (1=land, 0=water)              '  /) !  6

!-------------------------------------------------------------------------------
! Time independent boundary 2d arrays for CTM domain.  (GRID_BDY_2D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: gb2index   = gc2index

  REAL, ALLOCATABLE, TARGET :: gb2        ( : , : )

  REAL, POINTER :: glat_b     ( : )      ! latitude  (S- ; N+) [deg]
  REAL, POINTER :: glon_b     ( : )      ! longitude (W- ; E+) [deg]
  REAL, POINTER :: gmsfsq_b   ( : )      ! square map scale factor 
  REAL, POINTER :: gtopo_b    ( : )      ! elevation AGL [m]
  REAL, POINTER :: gdluse_b   ( : )      ! dominant land use category
  REAL, POINTER :: glwmask_b  ( : )      ! land-water mask (1=land, 0=water)

  ! Optional output variables, if fractional land use data are available
  REAL, POINTER :: gpurb_b    ( : )      ! percentage of urban area
  REAL, POINTER :: glufrac_b  ( : , : )  ! fractional land use by category

!-------------------------------------------------------------------------------
! Time independent cross 3D arrays for CTM domain.  (GRID_CRO_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: gc3index   = 2

  REAL, ALLOCATABLE, TARGET :: gc3        ( : , : , : , : )

  REAL, POINTER :: gx3htf_c   ( : , : , : )  ! lyr-top hgt abv grnd [m]
  REAL, POINTER :: gx3htm_c   ( : , : , : )  ! mid-lyr hgt abv grnd [m]

  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: gc3vname ( gc3index ) = &
    (/ 'X3HT0F',  'X3HT0M' /)

  CHARACTER(LEN=16), PARAMETER :: gc3units ( gc3index ) = &
    (/ 'M',       'M' /)

  CHARACTER(LEN=80), PARAMETER :: gc3vdesc ( gc3index ) = &
    (/ 'height of layer face (top) above ground        ', &  !  1
       'height of layer middle above ground            '  /) !  2

END MODULE groutcom
