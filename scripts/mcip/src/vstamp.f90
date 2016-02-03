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
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
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
