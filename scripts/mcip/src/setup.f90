
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

SUBROUTINE setup (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the Input Meteorology Domain Attributes
! Purpose:  Establishes bounds for MM5 or WRF post-processing.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           07 Jan 2002  Changed file name to explicit file rather than
!                        Fortran unit to improve portability.  (S. Howard
!                        and T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           26 May 2005  Added WRF capability.  Changed routine name from
!                        SETUPMM5 to SETUP to make code more general.  (T. Otte)
!           09 Apr 2007  Removed option to handle MM5v2-formatted data.
!                        (T. Otte)
!           22 Apr 2008  Set WRF DYN_OPT to 2 (mass core) for WRFv3 and
!                        beyond because support for other cores within WRF-ARW
!                        was discontinued in WRFv3.  (T. Otte)
!           17 Mar 2010  Changed all calls to netCDF routines to use the
!                        Fortran interface rather than the C interface.
!                        Rearranged subroutine to improve efficiency.  Removed
!                        dependency on module WRF_NETCDF.  Improved clarity
!                        in some error-handling messages.  Added CDFID to the
!                        argument list for subroutine SETUP_WRFEM.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE metinfo
  USE file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                      :: cdfid
  REAL,          INTENT(INOUT) :: ctmlays   ( maxlays )
  CHARACTER*19                 :: gridtype
  INTEGER                      :: istat
  CHARACTER*16,  PARAMETER     :: pname     = 'SETUP'
  INTEGER                      :: rcode
  CHARACTER*80                 :: wrfversion

!-------------------------------------------------------------------------------
! Try to determine if input meteorology file is in NetCDF format or not.
! If NetCDF format, it is probably WRF.  Otherwise, assume it is MM5.
!-------------------------------------------------------------------------------

  rcode = nf_open (file_mm(1), nf_nowrite, cdfid)

  IF ( rcode == nf_noerr ) THEN  ! successfully opened NetCDF file; assume WRF

    !---------------------------------------------------------------------------
    ! If WRF, determine whether or not the Advanced Research WRF, ARW, formerly
    ! known as Eulerian mass, EM) version was used.
    !---------------------------------------------------------------------------

    met_model = 2

    rcode = nf_get_att_int (cdfid, nf_global, 'DYN_OPT', met_iversion)
    IF ( rcode /= nf_noerr ) THEN
      rcode = nf_get_att_text (cdfid, nf_global, 'TITLE', wrfversion)
      IF ( rcode /= nf_noerr ) THEN
        WRITE (6,9300) TRIM(pname), 'TITLE', rcode
        GOTO 1001
      ENDIF
      IF ( wrfversion(18:19) == "V3" ) THEN
        met_iversion = 2  ! NCAR only supports mass core in WRFv3 and beyond
      ELSE
        WRITE (6,9400) TRIM(pname), TRIM(wrfversion)
        GOTO 1001
      ENDIF
    ENDIF

    rcode = nf_get_att_text (cdfid, nf_global, 'GRIDTYPE', gridtype)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,9300) TRIM(pname), 'GRIDTYPE', rcode
      GOTO 1001
    ENDIF

    IF ( ( met_iversion == 2 ) .AND. ( gridtype(1:1) == "C" ) ) THEN
      CALL setup_wrfem (cdfid, ctmlays)
    ELSE
      GOTO 8200
    ENDIF

    rcode = nf_close (cdfid)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,9500) TRIM(pname), rcode
      GOTO 1001
    ENDIF

  ELSE  ! error opening file as NetCDF; assume MM5

    !---------------------------------------------------------------------------
    ! Set up Fortran unit for (first) MM5 input file.
    ! Call subroutine for set-up based on version of MM5 output format.
    !---------------------------------------------------------------------------

    met_model = 1

    iutmm = iutmmi

    OPEN (UNIT=iutmmi,  FILE=file_mm(1), FORM='UNFORMATTED', STATUS='OLD',  &
          IOSTAT=istat, ERR=8000)

    CALL getversion

    IF ( met_iversion == 3 ) THEN
      CALL setup_mm5v3 (ctmlays)
    ELSE
      GOTO 8100
    ENDIF

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) TRIM(pname), iutmmi, TRIM(file_mm(1)), istat
      GOTO 1001

 8100 WRITE (6,9100) TRIM(pname), met_iversion
      GOTO 1001

 8200 WRITE (6,9200) TRIM(pname), met_iversion, gridtype
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: ', a,                            &
              /, 1x, '***   ERROR OPENING MM5 FILE ON UNIT ', i3,      &
              /, 1x, '***   MM5 FILE NAME = ', a,                      &
              /, 1x, '***   IOSTAT = ', i4,                            &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: ', a,                            &
              /, 1x, '***   UNKNOWN OR UNSUPPORTED MM5 OUTPUT VERSION',&
              /, 1x, '***   IVERSION = ', i3,                          &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: ', a,                            &
              /, 1x, '***   UNKNOWN WRF OUTPUT VERSION',               &
              /, 1x, '***   IVERSION = ', i3,                          &
              /, 1x, '***   GRIDTYPE = ', a,                           &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: ', a,                            &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',  &
              /, 1x, '***   VARIABLE = ', a,                           &
              /, 1x, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: ', a,                            &
              /, 1x, '***   UNKNOWN OR UNSUPPORTED WRF OUTPUT VERSION',&
              /, 1x, '***   VERSION = ', a,                            &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: ', a,                            &
              /, 1x, '***   ERROR CLOSING WRF FILE',                   &
              /, 1x, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setup
