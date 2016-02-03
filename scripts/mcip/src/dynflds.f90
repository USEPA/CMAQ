
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
! $Header: /project/work/rep/MCIP2/src/mcip2/dynflds.F,v 1.5 2007/08/03 20:50:11 tlotte Exp $ 


SUBROUTINE dynflds (sdate, stime)

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
!-------------------------------------------------------------------------------

  USE mcipparm

  IMPLICIT NONE

  CHARACTER*16,  PARAMETER     :: pname      = 'DYNFLDS'
  INTEGER,       INTENT(IN)    :: sdate      ! YYYYDDD
  INTEGER,       INTENT(IN)    :: stime      ! HHMISS

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
! Calculate dry deposition velocities.
!-------------------------------------------------------------------------------

  SELECT CASE ( lddep )

    CASE (  0  )  ! Do not perform dry deposition velocity calculation in MCIP

    CASE (  4  )  ! Use Models-3 (Pleim) dry deposition
      CALL m3dry (sdate, stime)

    CASE DEFAULT
      WRITE (6,9000) lddep
      GOTO 1001

  END SELECT

!-------------------------------------------------------------------------------
! Compute potential vorticity.
!-------------------------------------------------------------------------------

  IF ( lpv > 0 ) THEN
    CALL pvs
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: DYNFLDS',           &
              /, 1x, '***   IMPROPER VALUE FOR LDDEP',    &
              /, 1x, '***   LDDEP MUST BE 0 OR 4',        &
              /, 1x, '***   LDDEP = ', i2,                &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE dynflds
