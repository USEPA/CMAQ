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

SUBROUTINE getmet (mcip_now)

!-------------------------------------------------------------------------------
! Name:     Get Meteorology (from MM5)
! Purpose:  Get input meteorology information from MM5v2 or MM5v3 files.
! Revised:  04 Feb 1997  Original version.  (Y.-H. Li)
!           04 Mar 1997  Added include file 'LRADMDAT.EXT'.  Replaced loop
!                        indices with METCOL, METROW, METLAY.  (D. Byun)
!           29 May 1997  Modified SUBST include for beta version.  (D. Byun)
!           05 Nov 1997  Added nonhydrostatic and hydrostatic output
!                        functions.  (D. Byun)
!           04 Feb 1998  Changed include method for nonglobals.  (D. Byun)
!           04 Mar 1998  Allow high resolution land use data.  (R. Tang)
!           19 Sep 2001  Rewrote entire section of code in and called from this
!                        routine and organized it into logical subroutines.
!                        All code is now in free-form f90.  This routine and
!                        others called within replace getmet_mm5.F.  (T. Otte)
!           12 Jan 2005  Added option to call WRF input routine.  (T. Otte)
!           19 Jun 2006  Updated error-handling if unknown meteorology model
!                        is encountered.  (T. Otte)
!           09 Apr 2007  Changed routine to directly call RDMM5V3 rather than
!                        RDMM5 because MM5v2-formatted data are no longer
!                        suppported.  (T. Otte)
!           29 Aug 2011  Improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo, ONLY: met_model

  IMPLICIT NONE

  CHARACTER(LEN=24),  INTENT(IN)    :: mcip_now
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'GETMET'

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNKNOWN INPUT MET MODEL SOURCE', &
    & /, 1x, '***   MUST BE WRF-ARW', &
    & /, 1x, '***   MET_MODEL = ', i4, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Read meteorology information for this time period.
!-------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN  ! WRF-ARW
    CALL rdwrfem (mcip_now)
  ELSE
    WRITE (*,f9000) TRIM(pname), met_model
    CALL graceful_stop (pname)
  ENDIF

END SUBROUTINE getmet
