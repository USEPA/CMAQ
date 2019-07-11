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
!           31 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Updated netCDF commands
!                        to F90, and improved error handling.  Changed F77
!                        character declarations to F90 standard.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           15 Nov 2018  Allow WRFv4.0 input to be used.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE metinfo
  USE files
  USE netcdf

  IMPLICIT NONE

  INTEGER                           :: cdfid
  REAL,               INTENT(INOUT) :: ctmlays   ( maxlays )
  CHARACTER(LEN=19)                 :: gridtype
  INTEGER                           :: istat
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'SETUP'
  INTEGER                           :: rcode
  CHARACTER(LEN=80)                 :: wrfversion

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING WRF NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   NCF:  ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNKNOWN WRF OUTPUT VERSION', &
    & /, 1x, '***   IVERSION = ', i3, &
    & /, 1x, '***   GRIDTYPE = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNKNOWN OR UNSUPPORTED WRF OUTPUT VERSION', &
    & /, 1x, '***   VERSION = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING WRF FILE', &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Try to determine if input meteorology file is in NetCDF format or not.
! If NetCDF format, it is probably WRF.
!-------------------------------------------------------------------------------

  rcode = nf90_open (file_mm(1), nf90_nowrite, cdfid)

  IF ( rcode == nf90_noerr ) THEN  ! successfully opened NetCDF file; assume WRF

    !---------------------------------------------------------------------------
    ! If WRF, determine whether or not the Advanced Research WRF, ARW, formerly
    ! known as Eulerian mass, EM) version was used.
    !---------------------------------------------------------------------------

    met_model = 2

    rcode = nf90_get_att (cdfid, nf90_global, 'DYN_OPT', met_iversion)
    IF ( rcode /= nf90_noerr ) THEN
      rcode = nf90_get_att (cdfid, nf90_global, 'TITLE', wrfversion)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9300) TRIM(pname), 'TITLE', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      IF ( wrfversion(18:19) >= "V3" ) THEN
        met_iversion = 2  ! NCAR only supports mass core in WRFv3 and beyond
      ELSE
        WRITE (*,f9400) TRIM(pname), TRIM(wrfversion)
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

    rcode = nf90_get_att (cdfid, nf90_global, 'GRIDTYPE', gridtype)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9300) TRIM(pname), 'GRIDTYPE', rcode
      CALL graceful_stop (pname)
    ENDIF

    IF ( ( met_iversion == 2 ) .AND. ( gridtype(1:1) == "C" ) ) THEN
      CALL setup_wrfem (cdfid, ctmlays)
    ELSE
      WRITE (*,f9200) TRIM(pname), met_iversion, gridtype
      CALL graceful_stop (pname)
    ENDIF

    rcode = nf90_close (cdfid)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9500) TRIM(pname), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ELSE  ! error opening file as NetCDF

    WRITE (*,f9000) TRIM(pname), TRIM(file_mm(1)), TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)

  ENDIF

END SUBROUTINE setup
