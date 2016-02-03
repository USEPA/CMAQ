
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
! $Header: /project/work/rep/MCIP2/src/mcip2/getmet.F,v 1.4 2007/08/03 20:49:21 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  USE metinfo, ONLY: met_model

  IMPLICIT NONE

  CHARACTER*24,  INTENT(IN)    :: mcip_now
  CHARACTER*16,  PARAMETER     :: pname      = 'GETMET'

!-------------------------------------------------------------------------------
! Read meteorology information for this time period.
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN       ! MM5v3
    CALL rdmm5v3 (mcip_now)
  ELSE IF ( met_model == 2 ) THEN  ! WRF-ARW
    CALL rdwrfem (mcip_now)
  ELSE
    WRITE (6,9000) met_model
    GOTO 1001
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETMET',                             &
              /, 1x, '***   UNKNOWN INPUT MET MODEL SOURCE',               &
              /, 1x, '***   MUST BE EITHER MM5v3 OR WRF-ARW',              &
              /, 1x, '***   MET_MODEL = ', i4,                             &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getmet
