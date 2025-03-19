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

SUBROUTINE dynflds

!-------------------------------------------------------------------------------
! Name:     Dynamic Fields
! Purpose:  Maps and calculates time-variant fields on MCIP X grids.
! Revised:  12 Sep 2001  Original version.  (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           28 Feb 2005  Allowed new user options for LDDEP = 3 or 4 for dry
!                        deposition velocity calculations for additional
!                        chlorine and mercury species in M3DRY.  Removed call
!                        to MET3DSUP since that subroutine was combined into
!                        METVARS2CTM.  (T. Otte)
!           27 Feb 2006  Removed unnecessary dependence on module METINFO.
!                        (T. Otte)
!           30 Jun 2006  Added SDATE and STIME to calling argument list
!                        for M3DRY.  (T. Otte)
!           24 Jul 2007  Added option to bypass dry deposition velocity
!                        calculations in MCIP so that they can be performed
!                        in the CCTM, and changed code so that M3Dry with
!                        chlorine and mercury is the only option to compute
!                        dry deposition velocities in MCIP.  Removed obsolescent
!                        user-definable run time options for recalculating PBL,
!                        cloud, and radiation fields.  (T. Otte)
!           29 Oct 2009  Added optional call to compute potential vorticity.
!                        (T. Otte)
!           12 Feb 2010  Removed unused argument GMT.  (T. Otte)
!           09 Sep 2010  Removed option to compute dry deposition velocities
!                        in MCIP.  Removed input arguments SDATE and
!                        STIME.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Put input meteorology variables on MCIP (X) grid and calculate required
! state variables for CTM.
!-------------------------------------------------------------------------------

  CALL metvars2ctm

!-------------------------------------------------------------------------------
! Calculate supplemental planetary boundary layer fields.
!-------------------------------------------------------------------------------

  CALL pblsup

!-------------------------------------------------------------------------------
! Calculate cloud fields.
!-------------------------------------------------------------------------------

  CALL bcldprc_ak

!-------------------------------------------------------------------------------
! Compute potential vorticity.
!-------------------------------------------------------------------------------

  IF ( lpv > 0 ) THEN
    CALL pvs
  ENDIF

END SUBROUTINE dynflds
