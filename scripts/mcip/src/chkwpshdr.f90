
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
!-------------------------------------------------------------------------------

  USE file
  USE mcipparm
  USE metinfo

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: cdfid
  CHARACTER*80                 :: cval
  CHARACTER*4                  :: cval4
  CHARACTER*256, INTENT(IN)    :: fl
  CHARACTER*256                :: fl1
  INTEGER                      :: ival
  CHARACTER*16,  PARAMETER     :: pname     = 'CHKWPSHDR'
  INTEGER                      :: rcode
  REAL,          PARAMETER     :: rtol      = 1.0e-4
  REAL                         :: rval

!-------------------------------------------------------------------------------
! Check NX, NY, and NZ.
!-------------------------------------------------------------------------------

  fl1 = file_mm(1)

  rcode = nf_get_att_int (cdfid, nf_global, 'WEST-EAST_GRID_DIMENSION', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_nx ) THEN
      WRITE (6,9100) 'WEST-EAST_GRID_DIMENSION', TRIM(fl1), met_nx,  &
                                                 TRIM(fl),  ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'WEST-EAST_GRID_DIMENSION', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'SOUTH-NORTH_GRID_DIMENSION', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_ny ) THEN
      WRITE (6,9100) 'SOUTH-NORTH_GRID_DIMENSION', TRIM(fl1), met_ny,  &
                                                   TRIM(fl),  ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'SOUTH-NORTH_GRID_DIMENSION', rcode
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Check domain attributes.
!-------------------------------------------------------------------------------

  rcode = nf_get_att_real (cdfid, nf_global, 'DX', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_resoln ) > rtol ) THEN
      WRITE (6,9200) 'DX', TRIM(fl1), met_resoln, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'DX', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'DY', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_resoln ) > rtol ) THEN
      WRITE (6,9200) 'DY', TRIM(fl1), met_resoln, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'DY', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'MAP_PROJ', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_mapproj ) THEN
      WRITE (6,9100) 'MAP_PROJ', TRIM(fl1), met_mapproj, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'MAP_PROJ', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'STAND_LON', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_proj_clon ) > rtol ) THEN
      WRITE (6,9200) 'STAND_LON', TRIM(fl1), met_proj_clon, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'STAND_LON', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'MOAD_CEN_LAT', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_proj_clat ) > rtol ) THEN
      WRITE (6,9200) 'MOAD_CEN_LAT', TRIM(fl1), met_proj_clat, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'MOAD_CEN_LAT', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'CEN_LON', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_x_centd ) > rtol ) THEN
      WRITE (6,9200) 'CEN_LON', TRIM(fl1), met_x_centd, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'CEN_LON', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'CEN_LAT', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_y_centd ) > rtol ) THEN
      WRITE (6,9200) 'CEN_LAT', TRIM(fl1), met_y_centd, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'CEN_LAT', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'TRUELAT1', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_tru1 ) > rtol ) THEN
      WRITE (6,9200) 'TRUELAT1', TRIM(fl1), met_tru1, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'TRUELAT1', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'TRUELAT2', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_tru2 ) > rtol ) THEN
      WRITE (6,9200) 'TRUELAT2', TRIM(fl1), met_tru2, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'TRUELAT2', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_text (cdfid, nf_global, 'MMINLU', cval)
  IF ( rcode == nf_noerr ) THEN
    IF ( cval(1:3) /= met_lu_src(1:3) ) THEN
      WRITE (6,9300) 'MMINLU', TRIM(fl1), TRIM(met_lu_src), TRIM(fl), TRIM(cval)
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'MMINLU', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_text (cdfid, nf_global, 'TITLE', cval)
  IF ( rcode == nf_noerr ) THEN
    cval4 = '    '
    IF ( cval(21:21) == "V" ) THEN
      cval4(1:2) = cval(21:22)
    ELSE
      WRITE (6,9800), rcode
      GOTO 1001
    ENDIF
    IF ( cval(23:23) == "." ) THEN
      cval4(3:4) = cval(23:24)
    ENDIF
    IF ( cval4 >= "V3.1" ) THEN
      rcode = nf_get_att_int (cdfid, nf_global, 'NUM_LAND_CAT', ival)
      IF ( rcode == nf_noerr ) THEN
        IF ( ival /= nummetlu ) THEN
          WRITE (6,9100) 'NUM_LAND_CAT', TRIM(fl1), nummetlu, TRIM(fl), ival
          GOTO 1001
        ENDIF
      ELSE
        WRITE (6,9400) 'NUM_LAND_CAT', rcode
        GOTO 1001
      ENDIF
    ENDIF
  ELSE
    WRITE (6,9400) 'TITLE', rcode
    GOTO 1001
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWPSHDR',                          &
              /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME DOMAIN', &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   FIRST FILE = ', a,                             &
              /, 1x, '***   VALUE IN FIRST FILE = ', i4,                   &
              /, 1x, '***   NEW FILE = ', a,                               &
              /, 1x, '***   VALUE IN NEW FILE = ', i4,                     &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWPSHDR',                          &
              /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME DOMAIN', &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   FIRST FILE = ', a,                             &
              /, 1x, '***   VALUE IN FIRST FILE = ', f13.3,                &
              /, 1x, '***   NEW FILE = ', a,                               &
              /, 1x, '***   VALUE IN NEW FILE = ', f13.3,                  &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWPSHDR',                          &
              /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME DOMAIN', &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   FIRST FILE = ', a,                             &
              /, 1x, '***   VALUE IN FIRST FILE = ', a,                    &
              /, 1x, '***   NEW FILE = ', a,                               &
              /, 1x, '***   VALUE IN NEW FILE = ', a,                      &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWPSHDR',                          &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WPS FILE',      &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   RCODE = ', i3,                                 &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWPSHDR',                          &
              /, 1x, '***   ERROR EXTRACTING WPS VERSION FROM HEADER',     &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE chkwpshdr
