
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


SUBROUTINE init_io

!-------------------------------------------------------------------------------
! Name:     Initializes I/O API
! Purpose:  Initialize I/O API.
! Revised:  18 Aug 2004  Original version.  (T. Otte)
!           08 Mar 2005  Enabled/mandated use of I/O API 3 or beyond.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_time
  USE iodecl3
  USE parms3

  IMPLICIT NONE

  INTEGER                      :: funit
  INTEGER                      :: mxatts
  INTEGER                      :: mxdesc
  INTEGER                      :: mxdlen
  INTEGER                      :: mxfile
  INTEGER                      :: mxlays
  INTEGER                      :: mxvars
  INTEGER                      :: namlen
  LOGICAL                      :: ok         = .TRUE.
  CHARACTER*16,  PARAMETER     :: pname      = 'INIT_IO'

!-------------------------------------------------------------------------------
! Initialize I/O API.
!-------------------------------------------------------------------------------

  funit = init3()

!-------------------------------------------------------------------------------
! Verify that the I/O API library and the parameters in PARMS3 match.
!-------------------------------------------------------------------------------

  CALL ioparms3 (mxdlen, namlen, mxfile, mxvars, mxdesc, mxlays, mxatts)

  ok = ok .AND. ( mxdlen == mxdlen3 )
  ok = ok .AND. ( namlen == namlen3 )
  ok = ok .AND. ( mxfile == mxfile3 )
  ok = ok .AND. ( mxvars == mxvars3 )
  ok = ok .AND. ( mxdesc == mxdesc3 )
  ok = ok .AND. ( mxlays == mxlays3 )
  ok = ok .AND. ( mxatts == mxatts3 )

  IF ( .NOT. ok ) THEN
    WRITE (6,9000) mxdlen,  namlen,  mxfile,  mxvars,  mxdesc,  mxlays,  mxatts, &
                   mxdlen3, namlen3, mxfile3, mxvars3, mxdesc3, mxlays3, mxatts3
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Initialize current date and time to missing values.
!-------------------------------------------------------------------------------

  sdate = imiss3
  stime = imiss3

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: INIT_IO',                        &
              /, 1x, '***   PARAMETER MISMATCH IN IO API AND PARMS3',  &
              /, 1x, '***   FROM IO API:',                             &
              /, 1x, '***     MXDLEN = ', i4, '  NAMLEN = ', i4,       &
              /, 1x, '***     MXFILE = ', i4, '  MXVARS = ', i4,       &
              /, 1x, '***     MXDESC = ', i4, '  MXLAYS = ', i4,       &
              /, 1x, '***     MXATTS = ', i4,                          &
              /, 1x, '***   FROM PARMS3:',                             &
              /, 1x, '***     MXDLEN = ', i4, '  NAMLEN = ', i4,       &
              /, 1x, '***     MXFILE = ', i4, '  MXVARS = ', i4,       &
              /, 1x, '***     MXDESC = ', i4, '  MXLAYS = ', i4,       &
              /, 1x, '***     MXATTS = ', i4,                          &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE init_io
