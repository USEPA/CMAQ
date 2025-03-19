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

SUBROUTINE wrgdesc

!-------------------------------------------------------------------------------
! Name:     Write Grid Description
! Purpose:  Writes grid description (GRIDDESC) file.
! Revised:  03 Oct 2001  Original version.  (T. Otte)
!           07 Jan 2002  Added OPEN and CLOSE statements for output file.
!                        (S. Howard and T. Otte)
!           30 Jan 2002  Changed the creation of the GRIDDESC file from
!                        a list-directed write to a formatted write to
!                        improve portability.  (T. Otte)
!           01 Jul 2004  Corrected minor formatting bugs.  (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Changed name of internal
!                        variable BLANK to BLNK to avoid conflict with F90
!                        protected intrinsic.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           30 Jun 2021  Changed format for GRIDDESC to accommodate modeling
!                        domains with more than 999 grid cells on a side.
!                        (T. Spero)
!-------------------------------------------------------------------------------

  USE coord
  USE files
  USE mcipparm

  IMPLICIT NONE

  CHARACTER(LEN=1),   PARAMETER     :: blnk       = ' '
  CHARACTER(LEN=1),   PARAMETER     :: quote      = CHAR(39)

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f100 = "(a)"
  CHARACTER(LEN=256), PARAMETER :: f200 = "(i3, 5(1x, f13.3))"
  CHARACTER(LEN=256), PARAMETER :: f300 = "( a, 4(1x, f13.3), 3(1x, i4))"

!-------------------------------------------------------------------------------
! Write grid description in two sets.  Reader uses two list-directed READ
! statements to capture information.
!-------------------------------------------------------------------------------

  OPEN (iutgd, FILE=file_gd)

  WRITE (iutgd,f100) quote // blnk // quote

  WRITE (iutgd,f100) quote // TRIM(coordnam_gd) // quote
  WRITE (iutgd,f200) gdtyp_gd, p_alp_gd, p_bet_gd, p_gam_gd, xcent_gd, ycent_gd
  WRITE (iutgd,f100) quote // blnk // quote

  WRITE (iutgd,f100) quote // TRIM(gdname_gd) // quote
  WRITE (iutgd,f300) quote // TRIM(coordnam_gd) // quote, xorig_gd, yorig_gd,  &
                     xcell_gd, ycell_gd, ncols, nrows, nthik
  WRITE (iutgd,f100) quote // blnk // quote

  CLOSE (iutgd)

END SUBROUTINE wrgdesc
