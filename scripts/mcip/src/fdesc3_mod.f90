
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
! $Header: /project/work/rep/MCIP2/src/mcip2/fdesc3_mod.F,v 1.2 2007/08/03 20:48:05 tlotte Exp $ 


MODULE fdesc3

!-------------------------------------------------------------------------------
! Name:     Models-3 File Descriptors
! Purpose:  Contains Fortran data structures for a MODELS 3 file description.
!           Used to pass data between RDDICT3, WRDICT3, OPEN3, DESC3,
!           and their callers.  Common BDESC3 is used to store the
!           non-character-string data, and CDESC3 is used to store
!           the character-string data (recall that FORTRAN 77 prohibits
!           character and non-character data in the same common)
! Notes:    SHOULD ONLY BE USED AS A NAME BASED ARGUMENT PASSING MECHANISM.
!           The user should have local variables to/from which this data
!           structure is copied immediately prior to or immediately after
!           calls which set or use these COMMONs, since their values are
!           subject to change at any time by the IOAPI.
!
! Set By:
!          DESC3:    Everything in FDESC3.EXT
!
!          RDDICT3:  FTYPE3D, TSTEP3D, NCOLS3D, NROWS3D, NLAYS3D, NVARS3D,
!                    NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D, P_GAM3D,
!                    XORIG3D, YORIG3D, XCELL3D, YCELL3D, GDNAM3D,
!                    XCENT3D, YCENT3D, VNAME3D, UNITS3D, VDESC3D
!
! Referenced By:
!          OPEN3:    FTYPE3D, SDATE3D, STIME3D, TSTEP3D, NCOLS3D, NROWS3D,
!                    NLAYS3D, NVARS3D, NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D,
!                    P_GAM3D, XORIG3D, YORIG3D, XCELL3D, YCELL3D, GDNAM3D,
!                    XCENT3D, YCENT3D, VNAME3D, UNITS3D, VDESC3D
!
!          WRDICT3:  FTYPE3D, TSTEP3D, NCOLS3D, NROWS3D, NLAYS3D, NVARS3D,
!                    NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D, P_GAM3D, XORIG3D,
!                    YORIG3D, XCELL3D, YCELL3D, XCELL3D, YCELL3D, GDNAM3D, 
!                    VNAME3D, UNITS3D, VDESC3D
!
! Revised:  ?? May 1991  Prototype.  (C. Coats)
!           ?? Mar 1992  For netCDF FORTRAN implementation of Models-3.
!                        (C. Coats)
!           ?? Dec 1992  Map projection descriptive parameters P_ALP, P_BET,
!                        P_GAM.  (C. Coats)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!           03 Nov 2008  Changed module to explicitly include free-form
!                        F90-compliant I/O API FDESC3.EXT rather than
!                        replicate its contents here.  (T. Otte)
!-------------------------------------------------------------------------------

  USE parms3

  IMPLICIT NONE

  INCLUDE 'FDESC3.EXT'

END MODULE fdesc3
