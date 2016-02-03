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

SUBROUTINE chkwpshdr (fl, cdfid)

!-------------------------------------------------------------------------------
! Name:     Check WPS Header
! Purpose:  Check WPS header variables from a WPS output file against the
!           "base" WRF output file used for this MCIP run to ensure that the
!           WRF output files are from the same domain.
! Notes:    This routine is not thorough, but it should be enough to spot-check
!           key variables that would indicate a different WRF simulation.
!           This routine assumes that FL (input argument) is already opened.
! Revised:  23 Sep 2009  Original version.  (T. Otte)
!           12 Feb 2010  Removed unused variables CDFID, DATE_INIT, DX, DY, 
!                        N_TIMES, and VARID, and removed unused format
!                        statements 9600 and 9700.  Changed RTOL to 1.0e-4.
!                        Corrected PNAME to "CHKWPSHDR".  (T. Otte)
!           17 Mar 2010  Added input argument CDFID.  Removed dependency on
!                        module WRF_NETCDF.  (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Updated netCDF commands
!                        to F90, and improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE files
  USE mcipparm
  USE metinfo
  USE netcdf

  IMPLICIT NONE

  INTEGER,            INTENT(IN)    :: cdfid
  CHARACTER(LEN=80)                 :: cval
  CHARACTER(LEN=4)                  :: cval4
  CHARACTER(LEN=256), INTENT(IN)    :: fl
  CHARACTER(LEN=256)                :: fl1
  INTEGER                           :: ival
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'CHKWPSHDR'
  INTEGER                           :: rcode
  REAL,               PARAMETER     :: rtol      = 1.0e-4
  REAL                              :: rval

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME DOMAIN', &
    & /, 1x, '***   VARIABLE = ', a)"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "( &
    & /, 1x, '***   FIRST FILE = ', a, &
    & /, 1x, '***   VALUE IN FIRST FILE = ', i4, &
    & /, 1x, '***   NEW FILE = ', a, &
    & /, 1x, '***   VALUE IN NEW FILE = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "( &
    & /, 1x, '***   FIRST FILE = ', a, &
    & /, 1x, '***   VALUE IN FIRST FILE = ', f13.3, &
    & /, 1x, '***   NEW FILE = ', a, &
    & /, 1x, '***   VALUE IN NEW FILE = ', f13.3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "( &
    & /, 1x, '***   FIRST FILE = ', a, &
    & /, 1x, '***   VALUE IN FIRST FILE = ', a, &
    & /, 1x, '***   NEW FILE = ', a, &
    & /, 1x, '***   VALUE IN NEW FILE = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WPS FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9800 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR EXTRACTING WPS VERSION FROM HEADER', &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Check NX, NY, and NZ.
!-------------------------------------------------------------------------------

  fl1 = file_mm(1)

  rcode = nf90_get_att (cdfid, nf90_global, 'WEST-EAST_GRID_DIMENSION', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_nx ) THEN
      WRITE (*,f9000) TRIM(pname), 'WEST-EAST_GRID_DIMENSION'
      WRITE (*,f9100) TRIM(fl1), met_nx, TRIM(fl),  ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'WEST-EAST_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'SOUTH-NORTH_GRID_DIMENSION', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_ny ) THEN
      WRITE (*,f9000) TRIM(pname), 'SOUTH-NORTH_GRID_DIMENSION'
      WRITE (*,f9100) TRIM(fl1), met_ny, TRIM(fl),  ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SOUTH-NORTH_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Check domain attributes.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'DX', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_resoln ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'DX'
      WRITE (*,f9200) TRIM(fl1), met_resoln, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'DX', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'DY', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_resoln ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'DY'
      WRITE (*,f9200) TRIM(fl1), met_resoln, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'DY', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'MAP_PROJ', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_mapproj ) THEN
      WRITE (*,f9000) TRIM(pname), 'MAP_PROJ'
      WRITE (*,f9100) TRIM(fl1), met_mapproj, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MAP_PROJ', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'STAND_LON', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_proj_clon ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'STAND_LON'
      WRITE (*,f9200) TRIM(fl1), met_proj_clon, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'STAND_LON', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'MOAD_CEN_LAT', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_proj_clat ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'MOAD_CEN_LAT'
      WRITE (*,f9200) TRIM(fl1), met_proj_clat, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MOAD_CEN_LAT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'CEN_LON', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_x_centd ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'CEN_LON'
      WRITE (*,f9200) TRIM(fl1), met_x_centd, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'CEN_LON', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'CEN_LAT', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_y_centd ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'CEN_LAT'
      WRITE (*,f9200) TRIM(fl1), met_y_centd, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'CEN_LAT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'TRUELAT1', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_tru1 ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'TRUELAT1'
      WRITE (*,f9200) TRIM(fl1), met_tru1, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'TRUELAT1', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'TRUELAT2', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_tru2 ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'TRUELAT2'
      WRITE (*,f9200) TRIM(fl1), met_tru2, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'TRUELAT2', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'MMINLU', cval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( cval(1:3) /= met_lu_src(1:3) ) THEN
      WRITE (*,f9000) TRIM(pname), 'MMINLU'
      WRITE (*,f9300) TRIM(fl1), TRIM(met_lu_src), TRIM(fl), TRIM(cval)
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MMINLU', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'TITLE', cval)
  IF ( rcode == nf90_noerr ) THEN
    cval4 = '    '
    IF ( cval(21:21) == "V" ) THEN
      cval4(1:2) = cval(21:22)
    ELSE
      WRITE (*,f9800) TRIM(pname), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    IF ( cval(23:23) == "." ) THEN
      cval4(3:4) = cval(23:24)
    ENDIF
    IF ( cval4 >= "V3.1" ) THEN
      rcode = nf90_get_att (cdfid, nf90_global, 'NUM_LAND_CAT', ival)
      IF ( rcode == nf90_noerr ) THEN
        IF ( ival /= nummetlu ) THEN
          WRITE (*,f9000) TRIM(pname), 'NUM_LAND_CAT'
          WRITE (*,f9100) TRIM(fl1), nummetlu, TRIM(fl), ival
          CALL graceful_stop (pname)
        ENDIF
      ELSE
        WRITE (*,f9400) TRIM(pname), 'NUM_LAND_CAT', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'TITLE', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

END SUBROUTINE chkwpshdr
