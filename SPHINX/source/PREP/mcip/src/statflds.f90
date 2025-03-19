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

SUBROUTINE statflds

!-------------------------------------------------------------------------------
! Name:     Static Fields
! Purpose:  Maps and calculates time-invariant fields on MCIP X grids.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           09 Jun 2003  Restricted output vertical structure so that layer
!                        collapsing is not used to create a thinner lowest or
!                        top layer than input meteorology.  (T. Otte)
!           07 Jul 2004  Refined error-checking on vertical structure to
!                        avoid machine truncation errors.  (T. Otte)
!           08 Apr 2005  Changed MET_KX to MET_NZ to make code more general.
!                        Added condition that REFSTATE is only called for
!                        non-hydrostatic MM5 input to MCIP.  (T. Otte)
!           18 Aug 2005  Changed internal variable EPSILON to EPSILONS to
!                        avoid confusion with F90 intrinsic function.  (T. Otte)
!           30 Jul 2007  Removed use of MET_INHYD to limit MM5 processing.
!                        Added call to BLDDESC to create metadata.  (T, Otte)
!           22 Dec 2010  Changed print statements from "f8.6" to "f9.6" to
!                        take Intel compiler's recommendations and eliminate
!                        warning messages.  (T. Otte)
!           29 Aug 2011  Improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           14 Sep 2018  Removed support for MM5v3.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE coord
  USE metinfo
  USE metvars

  IMPLICIT NONE

  REAL,               PARAMETER     :: epsilons     = 1.0e-6
  CHARACTER(LEN=16),  PARAMETER     :: pname        = 'STATFLDS'

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   CANNOT COLLAPSE TO THINNER BOTTOM OR TOP LAYER THAN INPUT',&
    & /, 1x, '***   BOTTOM CTM, MET LAYERS:  ', 2(2x, f9.6), &
    & /, 1x, '***   TOP CTM, MET LAYERS:  ', 2(2x, f9.6), &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Verify that user input VGLVS_GD (from namelist variable CTMLAYS) is not
! trying to create thinner lowest layer or thinner top layer than input
! meteorology.
!-------------------------------------------------------------------------------

  IF ( (   vglvs_gd(2)     - sigmaf(2)       > epsilons )   .OR.  &
       ( -(vglvs_gd(nlays) - sigmaf(met_nz)) > epsilons ) ) THEN
    WRITE (*,f9000) TRIM(pname), vglvs_gd(2), sigmaf(2), vglvs_gd(nlays),  &
                   sigmaf(met_nz)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Get land use categories.
!-------------------------------------------------------------------------------

  CALL getluse

!-------------------------------------------------------------------------------
! Put time-invariant fields on MCIP grid.
!-------------------------------------------------------------------------------

  CALL metgrid2ctm

!-------------------------------------------------------------------------------
! Create metadata for output files.
!-------------------------------------------------------------------------------

  CALL blddesc

END SUBROUTINE statflds
