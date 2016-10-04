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

PROGRAM mcip

!-------------------------------------------------------------------------------
! Name:     Meteorology-Chemistry Interface Processor
! Purpose:  Generates a complete set of meteorological data for transport
!           processors of Models-3 CTM.
! Revised:  25 Jan 1997  Original version.  (D. Byun)
!           20 May 1997  Adapted for Models-3 BETA system.  (D. Byun)
!           05 Nov 1997  Nonhydrostatic/hydrostatic output function.  (D. Byun)
!           04 Feb 1998  Changed include method for nonglobals.  (D. Byun)
!           13 Mar 1998  Get JUDATE in YYYYDDD.  (J. Young)
!           24 Mar 1998  Added SOLAR to calculate radiation.  (A. Bourgeois)
!           20 Sep 2001  Rewrote and restructured entire program.
!                        Converted to free-form f90.  Added, deleted, and
!                        modified several subroutines, variables, and
!                        modules.  (T. Otte)
!           03 Oct 2001  Added call to WRGDESC to write GRIDDESC file. (T. Otte)
!           29 Jan 2002  Created module DATE_TIME for SDATE and STIME.  Altered
!                        format for successful end-of-program.  (T. Otte)
!           03 Aug 2004  Added initialization of I/O API.  Added flag to create
!                        static output (grid) files.  (T. Otte)
!           26 May 2005  Changed replaced call to SETUPMM5 with call to SETUP
!                        to make code more general.  (T. Otte)
!           11 Aug 2005  Added calls to new routines ALLOC_DEPV, INIT_DEPV,
!                        and DEALLOC_DEPV.  (T. Otte)
!           09 Apr 2007  Added option to bypass dry deposition velocity
!                        calculations in MCIP so that they can be performed
!                        in the CCTM.  (T. Otte)
!           28 Apr 2008  Added user option to process observed cloud fields
!                        for satellite photolysis adjustment.  Requires
!                        additional data sets and preprocessing package
!                        available from University of Alabama at Huntsville.
!                        Contributed by University of Alabama at Huntsville.
!                        (A. Biazar and T. Otte)
!           25 Aug 2009  Added calls to ALLOC_LU, INIT_LU, and DEALLOC_LU to
!                        set up land-use classification information.  (T. Otte)
!           12 Feb 2010  Removed unused variable GMT from subroutines GETSDT
!                        and DYNFLDS.  (T. Otte)
!           19 Mar 2010  Added echo of metadata summary to MCIP log file.
!                        (T. Otte)
!           09 Sep 2010  Removed option to compute dry deposition velocities
!                        in MCIP.  Removed arguments SDATE and STIME from
!                        subroutine DYNFLDS.  (T. Otte)
!           30 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Removed calls to
!                        ALLOC_LU, DEALLOC_LU, and INIT_LU.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE date_pack
  USE date_time
  USE files
  USE sat2mcip

  IMPLICIT NONE

  REAL                              :: ctmlays    ( maxlays )
  LOGICAL                           :: first      = .TRUE.
  CHARACTER(LEN=24)                 :: mcip_next  ! YYYY-MO-DD-HH:MI:SS.SSSS
  CHARACTER(LEN=24)                 :: mcip_now   ! YYYY-MO-DD-HH:MI:SS.SSSS 

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f100 = "(//, 1x, 78('~'), &
    & /,  1x, '~~~ Processing meteorology for time = ', a,  &
    & /,  1x, 78('~'), /)"

  CHARACTER(LEN=256), PARAMETER :: f200 = "(//, 1x, 78('~'), &
    & /,  1x, '~~~ Metadata summary (from I/O API header)', &
    & /,  1x, 78('~'), /)"

!-------------------------------------------------------------------------------
! Initialize I/O API.
!-------------------------------------------------------------------------------

  CALL init_io

!-------------------------------------------------------------------------------
! Read user options from namelist.
!-------------------------------------------------------------------------------

  CALL vstamp
  CALL readnml (ctmlays)

  mcip_now = mcip_start
  CALL getsdt (mcip_now, sdate, stime)

!-------------------------------------------------------------------------------
! Set up input meteorology.
!-------------------------------------------------------------------------------

  CALL setup (ctmlays)

!-------------------------------------------------------------------------------
! Set up grid definitions from input meteorology and user input.
!-------------------------------------------------------------------------------

  CALL setgriddefs

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  CALL alloc_met
  CALL alloc_x
  CALL alloc_ctm

!-------------------------------------------------------------------------------
! Initialize arrays.
!-------------------------------------------------------------------------------

  CALL init_met
  CALL init_x

!-------------------------------------------------------------------------------
! Fill vertical arrays.
!-------------------------------------------------------------------------------

  CALL vertarys (ctmlays)

!-------------------------------------------------------------------------------
! Loop over time to get input, process fields, and write output.
!-------------------------------------------------------------------------------

  timeloop: DO

    WRITE (*,f100) mcip_now

    CALL getmet (mcip_now)            ! Read input meteorology file.

    IF ( first ) THEN
      CALL statflds                   ! Put time-invariant fields on MCIP grid.
      IF ( makegrid ) THEN
        CALL gridout (sdate, stime)   ! Output to GRID files.
        CALL wrgdesc                  ! Write GRIDDESC file.
      ENDIF
      first = .FALSE.
    ENDIF

    CALL dynflds                      ! Put time-varying fields on MCIP grid.

    IF ( lsat == 1 ) THEN
      CALL getsat (mcip_now)          ! Read input satellite file.
      CALL satvars2ctm (mcip_now)     ! Put time-variant sat data on MCIP grid
    ENDIF

    CALL metcro (sdate, stime)        ! Output to MET_CRO and MET_BDY files.
    CALL metdot (sdate, stime)        ! Output to MET_DOT files.


    ! Update SDATE and STIME for next I/O API header.

    CALL geth_newdate (mcip_next, mcip_now, intvl*60)
    IF ( mcip_next > mcip_end ) EXIT timeloop
    mcip_now = mcip_next
    CALL getsdt (mcip_now, sdate, stime)

  ENDDO timeloop

  WRITE (*,f200)
  WRITE (*,'(a)') fdesc(:)

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  CALL dealloc_met
  CALL dealloc_x
  CALL dealloc_ctm

!-------------------------------------------------------------------------------
! Close output files.
!-------------------------------------------------------------------------------

  CALL close_files

  WRITE (*,'(//, a)') 'NORMAL TERMINATION'

END PROGRAM mcip
