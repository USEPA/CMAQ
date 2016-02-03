
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
! $Header: /project/work/rep/MCIP2/src/mcip2/vstamp.F,v 1.12 2007/08/03 20:53:27 tlotte Exp $ 


SUBROUTINE vstamp

!-------------------------------------------------------------------------------
! Name:     Version Stamp
! Purpose:  Writes version and release identification to log file.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           18 Oct 2001  Updated for second beta release of MCIPv2.  (T. Otte)
!           20 Nov 2001  Updated for third beta release of MCIPv2.  (T. Otte)
!           19 Mar 2002  Updated for release of MCIP Version 2.0.  (T. Otte)
!           27 Mar 2003  Updated for release of MCIP Version 2.1.  (T. Otte)
!           11 Jun 2003  Updated for release of MCIP Version 2.2.  (T. Otte)
!           18 Aug 2004  Updated for release of MCIP Version 2.3.  (T. Otte)
!           19 Aug 2005  Updated for release of MCIP Version 3.0   (T. Otte)
!           27 Feb 2006  Updated for release of MCIP Version 3.1.  (T. Otte)
!           18 Aug 2006  Updated for release of MCIP Version 3.2.  (T. Otte)
!           31 Jul 2007  Moved version descriptors to MCIPPARM.  Changed
!                        "RELEASED" to "FROZEN" in the prints to be more
!                        technically correct.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm

  IMPLICIT NONE

  WRITE (*, "(/, 1x, 78('='), /)")
  WRITE (*, "(18x, a)") 'US EPA COMMUNITY MULTISCALE AIR QUALITY MODEL'
  WRITE (*, "(20x, a, /)") 'METEOROLOGY-CHEMISTRY INTERFACE PROCESSOR'
  WRITE (*, "(27x, a, 1x, a, ' FROZEN ', a)")  &
                                     TRIM(progname), TRIM(ver), vdate
  WRITE (*, "(/, 1x, 78('='), ///)")

END SUBROUTINE vstamp
