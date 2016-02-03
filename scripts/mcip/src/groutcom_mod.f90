
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
! $Header: /project/work/rep/MCIP2/src/mcip2/groutcom_mod.F,v 1.6 2007/08/03 20:49:38 tlotte Exp $ 


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

  ! For header information

  CHARACTER*16 :: gd2vname ( gd2index ) 
  CHARACTER*16 :: gd2units ( gd2index ) 
  CHARACTER*80 :: gd2vdesc ( gd2index ) 

  ! Header description

  DATA gd2vname / 'LATD',    'LOND',    'MSFD2',     'LATU',    &
                  'LONU',    'MSFU2',   'LATV',      'LONV',    &
                  'MSFV2' /

  DATA gd2units / 'DEGREES', 'DEGREES', '(M/M)**2',  'DEGREES', &
                  'DEGREES', '(M/M)**2','DEGREES',   'DEGREES', &
                  '(M/M)**2' /

  DATA gd2vdesc(1) / 'latitude (south negative) -- dot point'          /
  DATA gd2vdesc(2) / 'longitude (west negative) -- dot point'          /
  DATA gd2vdesc(3) / 'squared map-scale factor (DOT)'                  /
  DATA gd2vdesc(4) / 'latitude (south negative) -- U face'             /
  DATA gd2vdesc(5) / 'longitude (west negative) -- U face'             /
  DATA gd2vdesc(6) / 'squared map-scale factor (U FACE)'               /
  DATA gd2vdesc(7) / 'latitude (south negative) -- V face'             /
  DATA gd2vdesc(8) / 'longitude (west negative) -- V face'             /
  DATA gd2vdesc(9) / 'squared map-scale factor (V FACE)'               /

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

  ! For header information

  CHARACTER*16 :: gc2vname ( gc2index ) 
  CHARACTER*16 :: gc2units ( gc2index ) 
  CHARACTER*80 :: gc2vdesc ( gc2index ) 

  ! Header description

  DATA gc2vname / 'LAT',     'LON',     'MSFX2',     'HT',      &
                  'DLUSE',   'LWMASK' /

  DATA gc2units / 'DEGREES', 'DEGREES', '(M/M)**2',  'M',       &
                  'CATEGORY','CATEGORY' /

  DATA gc2vdesc(1) / 'latitude (south negative)'                       /
  DATA gc2vdesc(2) / 'longitude (west negative)'                       /
  DATA gc2vdesc(3) / 'squared map-scale factor (CROSS)'                /
  DATA gc2vdesc(4) / 'terrain elevation'                               /
  DATA gc2vdesc(5) / 'dominant land use category from classification'  /
  DATA gc2vdesc(6) / 'land-water mask (1=land, 0=water)'               /

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

  ! For header information

  CHARACTER*16 :: gc3vname ( gc3index ) 
  CHARACTER*16 :: gc3units ( gc3index ) 
  CHARACTER*80 :: gc3vdesc ( gc3index ) 

  ! Header description

  DATA gc3vname / 'X3HT0F',  'X3HT0M' /

  DATA gc3units / 'M',       'M' /

  DATA gc3vdesc(1) / 'height of layer face (top) above ground'/   
  DATA gc3vdesc(2) / 'height of layer middle above ground'    /   

END MODULE groutcom
