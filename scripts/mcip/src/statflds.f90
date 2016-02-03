
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
! $Header: /project/work/rep/MCIP2/src/mcip2/statflds.F,v 1.5 2007/08/03 20:52:19 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  USE mcipparm
  USE coord
  USE metinfo
  USE metvars

  IMPLICIT NONE

  REAL,          PARAMETER     :: epsilons     = 1.0e-6
  CHARACTER*16,  PARAMETER     :: pname        = 'STATFLDS'

!-------------------------------------------------------------------------------
! Verify that user input VGLVS_GD (from namelist variable CTMLAYS) is not
! trying to create thinner lowest layer or thinner top layer than input
! meteorology.
!-------------------------------------------------------------------------------

  IF ( (   vglvs_gd(2)     - sigmaf(2)       > epsilons )   .OR.  &
       ( -(vglvs_gd(nlays) - sigmaf(met_nz)) > epsilons ) ) THEN
    WRITE (6,9000) vglvs_gd(2), sigmaf(2), vglvs_gd(nlays), sigmaf(met_nz)
    GOTO 1001
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
! Calculate non-hydrostatic reference state for MM5v3.
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN  ! non-hydrostatic MM5v3
    CALL refstate
  ENDIF

!-------------------------------------------------------------------------------
! Create metadata for output files.
!-------------------------------------------------------------------------------

  CALL blddesc

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: STATFLDS',                              &
              /, 1x, '***   MCIP WILL NOT ALLOW COLLAPSING TO THINNER',       &
              /, 1x, '***     BOTTOM OR TOP LAYER THAN INPUT METEOROLOGY',    &
              /, 1x, '***   BOTTOM CTM, MET LAYERS:  ', 2(2x, f8.6),          &
              /, 1x, '***   TOP CTM, MET LAYERS:  ', 2(2x, f8.6),             &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE statflds
