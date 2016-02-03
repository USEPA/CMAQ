
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
! $Header: /project/work/rep/MCIP2/src/mcip2/file_mod.F,v 1.5 2007/08/03 20:48:12 tlotte Exp $ 


MODULE file

!-------------------------------------------------------------------------------
! Name:     File
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
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: iutmm
  INTEGER,       PARAMETER     :: iutgd      =  4
  INTEGER,       PARAMETER     :: iuthdr     =  2
  INTEGER,       PARAMETER     :: iutmmi     = 10
  INTEGER,       PARAMETER     :: iutnml     =  8
  INTEGER,       PARAMETER     :: iutter     =  9

  INTEGER,       PARAMETER     :: max_mm     = 300

  CHARACTER*256                :: file_gd
  CHARACTER*256                :: file_hdr
  CHARACTER*256                :: file_mm    ( max_mm )
  CHARACTER*256, PARAMETER     :: file_nml   = 'namelist.mcip'
  CHARACTER*256                :: file_ter
  LOGICAL                      :: makegrid

  CHARACTER*16,  PARAMETER     :: gridbdy2d  = 'GRID_BDY_2D     '
  CHARACTER*16,  PARAMETER     :: gridcro2d  = 'GRID_CRO_2D     '
  CHARACTER*16,  PARAMETER     :: gridcro3d  = 'GRID_CRO_3D     '
  CHARACTER*16,  PARAMETER     :: griddot2d  = 'GRID_DOT_2D     '
  CHARACTER*16,  PARAMETER     :: metbdy3d   = 'MET_BDY_3D      '
  CHARACTER*16,  PARAMETER     :: metcro2d   = 'MET_CRO_2D      '
  CHARACTER*16,  PARAMETER     :: metcro3d   = 'MET_CRO_3D      '
  CHARACTER*16,  PARAMETER     :: metdot3d   = 'MET_DOT_3D      '

END MODULE file
