
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
! $Header: /project/work/rep/MCIP2/src/mcip2/parms3_mod.F,v 1.4 2007/08/03 20:48:32 tlotte Exp $ 


MODULE parms3

!-------------------------------------------------------------------------------
! Name:     Parameters for Models-3
! Purpose:  Contains dimensioning parameters, standard file-type, grid-type,
!           (etc.) token values for FORTRAN Models-3 I/O System API.
! Notes:    Dependent upon  M3INT, M3REAL, M3DBLE consistent with NETCDF.EXT
! Revised:  ?? Mar 1992  Prototype.  (C. Coats)
!           ?? Dec 1992  New map projection type STEGRD3.  (C. Coats)
!           ?? Jun 1994  I/O API revisions.  (C. Coats)
!           ?? Dec 1996  Support for new file types.  (C. Coats)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           08 Jul 2004  Updated for consistency with I/O API.  (T. Otte)
!           17 Feb 2006  Updated for consistency with I/O API.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!           03 Nov 2008  Changed module to explicitly include free-form
!                        F90-compliant I/O API PARMS3.EXT rather than
!                        replicate its contents here.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'

END MODULE parms3
