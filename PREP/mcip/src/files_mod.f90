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

MODULE files

!-------------------------------------------------------------------------------
! Name:     Files
! Purpose:  Contains FORTRAN units and file names.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           03 Oct 2001  Added unit for GRIDDESC file.  (T. Otte)
!           07 Jan 2002  Added explicit file names for input meteorology
!                        file, namelist, grid description, and meteorology
!                        header.  (T. Otte)
!           09 Jun 2003  Removed GRIDBDY2D, GRIDBDY3D, and METBDY2D from
!                        output.  (T. Otte)
!           01 Jul 2004  Restored GRIDBDY2D.  Fixed lengths of output file
!                        names to be strictly CHARACTER*16.  Added flag to
!                        create static output (grid) files.  (T. Otte)
!           29 Nov 2004  Added optional input MM5 "TERRAIN" file to get
!                        fractional land use fields.  (T. Otte)
!           26 May 2005  Removed unused (input) variables GRIDCRO2D, GRIDCRO3D,
!                        LANDCRO2D, METCRO2D, METCRO3D, and METDOT3D.  Changed
!                        names for output files by removing "_G1".  Added new
!                        parameter to define maximum number of input meteorology
!                        files, and increased default maximum to 100.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!           02 May 2008  Increased MAX_MM to 300 to accommodate 5-minute output
!                        for one day.  (T. Otte)
!           30 Aug 2011  Changed name of routine from FILE to FILES to avoid
!                        conflict with F90 protected intrinsic names.  Changed
!                        F77 character declarations to F90 standard.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           31 May 2012  Increased MAX_MM from 300 to 367 to allow for a full
!                        year to be run (including leap year and previous day
!                        for precipitation calculation).  (T. Otte)
!           10 Feb 2018  Added new optional output files for fractional land
!                        use, layered soil data, and mosaic data.  (T. Spero)
!           18 Dec 2018  Removed support for MM5v3 input files.  Added file
!                        names for netCDF output.  Removed runtime option
!                        to not output time-independent files.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                           :: iutmm
  INTEGER,            PARAMETER     :: iutgd      =  4
  INTEGER,            PARAMETER     :: iutmmi     = 10
  INTEGER,            PARAMETER     :: iutnml     =  8

  INTEGER,            PARAMETER     :: max_mm     = 367

  CHARACTER(LEN=256)                :: file_gd
  CHARACTER(LEN=256)                :: file_mm    ( max_mm )
  CHARACTER(LEN=256), PARAMETER     :: file_nml   = 'namelist.mcip'
  CHARACTER(LEN=256)                :: file_geo

  CHARACTER(LEN=16),  PARAMETER     :: gridbdy2d  = 'GRID_BDY_2D     '
  CHARACTER(LEN=16),  PARAMETER     :: gridcro2d  = 'GRID_CRO_2D     '
  CHARACTER(LEN=16),  PARAMETER     :: griddot2d  = 'GRID_DOT_2D     '
  CHARACTER(LEN=16),  PARAMETER     :: lufraccro  = 'LUFRAC_CRO      '
  CHARACTER(LEN=16),  PARAMETER     :: metbdy3d   = 'MET_BDY_3D      '
  CHARACTER(LEN=16),  PARAMETER     :: metcro2d   = 'MET_CRO_2D      '
  CHARACTER(LEN=16),  PARAMETER     :: metcro3d   = 'MET_CRO_3D      '
  CHARACTER(LEN=16),  PARAMETER     :: metdot3d   = 'MET_DOT_3D      '
  CHARACTER(LEN=16),  PARAMETER     :: mosaiccro  = 'MOSAIC_CRO      '
  CHARACTER(LEN=16),  PARAMETER     :: soicro     = 'SOI_CRO         ' 

  CHARACTER(LEN=16),  PARAMETER     :: mcipbdyncf = 'mcip_bdy.nc     '
  CHARACTER(LEN=16),  PARAMETER     :: mcipncf    = 'mcip.nc         '

  INTEGER                           :: cdfid_b
  INTEGER                           :: cdfid_m

END MODULE files
