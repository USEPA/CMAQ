
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

SUBROUTINE chkwrfhdr (fl, cdfid)

!-------------------------------------------------------------------------------
! Name:     Check WRF Header
! Purpose:  Check WRF header variables from one WRF output file against the
!           "base" WRF output file used for this MCIP run to ensure that the
!           WRF output files are from the same simulation.
! Notes:    This routine is not thorough, but it should be enough to spot-check
!           key variables that would indicate a different WRF simulation.
!           This routine assumes that FL (input argument) is already opened.
! Revised:  15 May 2008  Original version.  (T. Otte)
!           11 May 2009  Correct bug in checking surface layer scheme in
!                        subsequent files.  (T. Otte)
!           25 Sep 2009  Removed netCDF file opening to prevent condition with
!                        too many open files for long simulations.  Added
!                        check on urban physics option and surface analysis
!                        nudging options.  Changed code to allow for GRID_FDDA
!                        to be greater than 1.  Corrected bug in checking to
!                        ensure that data are from same simulation (i.e.,
!                        restarted rather than reinitialized).  Corrected bug
!                        in checking observation nudging coefficient for
!                        temperature.  (T. Otte)
!           12 Feb 2010  Removed unused variables CDFID, DX, DY, N_TIMES, and
!                        VARID, and removed unused format statements 9600 and
!                        9700.  Changed RTOL to 1.0e-4.  (T. Otte)
!           18 Mar 2010  Added CDFID as an input argument.  Removed dependency
!                        on module WRF_NETCDF.  (T. Otte)
!-------------------------------------------------------------------------------

  USE file
  USE metinfo

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: cdfid
  CHARACTER*80                 :: cval
  CHARACTER*8                  :: cval8
  CHARACTER*19                 :: date_init
  INTEGER                      :: dimid
  CHARACTER*256, INTENT(IN)    :: fl
  CHARACTER*256                :: fl1
  INTEGER                      :: ival
  CHARACTER*16,  PARAMETER     :: pname     = 'CHKWRFHDR'
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

  rcode = nf_get_att_int (cdfid, nf_global, 'BOTTOM-TOP_GRID_DIMENSION', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival-1 /= met_nz ) THEN
      WRITE (6,9100) 'BOTTOM-TOP_GRID_DIMENSION', TRIM(fl1), met_nz,  &
                                                  TRIM(fl),  ival-1
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'BOTTOM-TOP_GRID_DIMENSION', rcode
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Check domain attributes.
!-------------------------------------------------------------------------------

  rcode = nf_get_att_text (cdfid, nf_global, 'TITLE', cval)
  IF ( rcode == nf_noerr ) THEN
    cval8 = '        '
    IF ( cval(18:18) == "V" ) THEN
      cval8(1:2) = cval(18:19)
    ENDIF
    IF ( cval(20:20) == '.' ) THEN
      cval8(3:4) = cval(20:21)
    ENDIF
    IF ( cval(22:22) == '.' ) THEN
      cval8(5:6) = cval(22:23)
    ENDIF
    IF ( cval(24:24) == '.' ) THEN
      cval8(7:8) = cval(24:25)
    ENDIF
    IF ( cval8 /= met_release ) THEN
      WRITE (6,9300) 'WRF VERSION', TRIM(fl1), met_release, TRIM(fl), cval8
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'TITLE', rcode
    GOTO 1001
  ENDIF

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

!-------------------------------------------------------------------------------
! Extract model run options.
!-------------------------------------------------------------------------------

  rcode = nf_get_att_text (cdfid, nf_global, 'MMINLU', cval)
  IF ( rcode == nf_noerr ) THEN
    IF ( cval(1:4) /= met_lu_src(1:4) ) THEN
      WRITE (6,9300) 'MMINLU', TRIM(fl1), met_lu_src(1:4), TRIM(fl), cval(1:4)
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'MMINLU', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'ISWATER', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_lu_water ) THEN
      WRITE (6,9100) 'ISWATER', TRIM(fl1), met_lu_water, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'ISWATER', rcode
    GOTO 1001
  ENDIF

  rcode = nf_inq_dimid (cdfid, 'soil_layers_stag', dimid)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,9400) 'ID for soil_layers_stag', rcode
    GOTO 1001
  ENDIF
  rcode = nf_inq_dimlen (cdfid, dimid, ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_ns ) THEN
      WRITE (6,9100) 'soil_layers_stag', TRIM(fl1), met_ns, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'value for soil_layers_stag', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'RA_LW_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_lw_rad ) THEN
      WRITE (6,9100) 'RA_LW_PHYSICS', TRIM(fl1), met_lw_rad, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'RA_LW_PHYSICS', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'RA_SW_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_sw_rad ) THEN
      WRITE (6,9100) 'RA_SW_PHYSICS', TRIM(fl1), met_sw_rad, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'RA_SW_PHYSICS', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'CU_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_cumulus ) THEN
      WRITE (6,9100) 'CU_PHYSICS', TRIM(fl1), met_cumulus, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'CU_PHYSICS', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'MP_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_expl_moist ) THEN
      WRITE (6,9100) 'MP_PHYSICS', TRIM(fl1), met_expl_moist, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'MP_PHYSICS', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'BL_PBL_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_pbl ) THEN
      WRITE (6,9100) 'BL_PBL_PHYSICS', TRIM(fl1), met_pbl, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'BL_PBL_PHYSICS', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'SF_SFCLAY_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_sfc_lay ) THEN
      WRITE (6,9100) 'SF_SFCLAY_PHYSICS', TRIM(fl1), met_sfc_lay, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'SF_SFCLAY_PHYSICS', rcode
    GOTO 1001
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'SF_SURFACE_PHYSICS', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_soil_lsm ) THEN
      WRITE (6,9100) 'SF_SURFACE_PHYSICS', TRIM(fl1), met_soil_lsm,  &
                                           TRIM(fl),  ival
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'SF_SURFACE_PHYSICS', rcode
    GOTO 1001
  ENDIF

  IF ( TRIM(met_release) >= 'V3.1' ) THEN
    rcode = nf_get_att_int (cdfid, nf_global, 'SF_URBAN_PHYSICS', ival)
    IF ( rcode == nf_noerr ) THEN
      IF ( ival /= met_urban_phys ) THEN
        WRITE (6,9100) 'SF_URBAN_PHYSICS', TRIM(fl1), met_urban_phys,  &
                                           TRIM(fl),  ival
        GOTO 1001
      ENDIF
    ELSE
      WRITE (6,9400) 'SF_URBAN_PHYSICS', rcode
      GOTO 1001
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Extract WRF start date and time information.
!-------------------------------------------------------------------------------

  rcode = nf_get_att_text (cdfid, nf_global, 'SIMULATION_START_DATE', date_init)
  IF ( rcode == nf_noerr ) THEN
    date_init(11:11) = "-"  ! change from "_" to "-" for consistency
    IF ( date_init /= met_startdate(1:19) ) THEN
      WRITE (6,9300) 'SIMULATION_START_DATE', TRIM(fl1), met_startdate(1:19),  &
                                              TRIM(fl),  date_init
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9400) 'SIMULATION_START_DATE', rcode
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Determine FDDA options.
!-------------------------------------------------------------------------------

  rcode = nf_get_att_int (cdfid, nf_global, 'GRID_FDDA', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_fdda_3dan ) THEN
      WRITE (6,9100) 'GRID_FDDA', TRIM(fl1), met_fdda_3dan, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    IF ( TRIM(met_release) >= 'V2.2' ) THEN
      WRITE (6,9400) 'GRID_FDDA', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'GUV', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_gv3d ) > rtol ) THEN
      WRITE (6,9200) 'GUV', TRIM(fl1), met_fdda_gv3d, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_3dan == 1 ) ) THEN
      WRITE (6,9400) 'GUV', rcode
      GOTO 1001
    ELSE IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_3dan == 2 ) ) THEN
      WRITE (6,9400) 'GUV', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'GT', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_gt3d ) > rtol ) THEN
      WRITE (6,9200) 'GT', TRIM(fl1), met_fdda_gt3d, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_3dan == 1 ) ) THEN
      WRITE (6,9400) 'GT', rcode
      GOTO 1001
    ELSE IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_3dan == 2 ) ) THEN
      WRITE (6,9400) 'GT', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'GQ', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_gq3d ) > rtol ) THEN
      WRITE (6,9200) 'GQ', TRIM(fl1), met_fdda_gq3d, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_3dan == 1 ) ) THEN
      WRITE (6,9400) 'GQ', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'GRID_SFDDA', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_fdda_sfan ) THEN
      WRITE (6,9100) 'GRID_SFDDA', TRIM(fl1), met_fdda_sfan, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    IF ( TRIM(met_release) >= 'V3.1' ) THEN
      WRITE (6,9400) 'GRID_SFDDA', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'GUV_SFC', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_gvsfc ) > rtol ) THEN
      WRITE (6,9200) 'GUV_SFC', TRIM(fl1), met_fdda_gvsfc, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_sfan == 1 ) ) THEN
      WRITE (6,9400) 'GUV_SFC', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'GT_SFC', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_gtsfc ) > rtol ) THEN
      WRITE (6,9200) 'GT_SFC', TRIM(fl1), met_fdda_gtsfc, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_sfan == 1 ) ) THEN
      WRITE (6,9400) 'GT_SFC', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'GQ_SFC', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_gqsfc ) > rtol ) THEN
      WRITE (6,9200) 'GQ_SFC', TRIM(fl1), met_fdda_gqsfc, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_sfan == 1 ) ) THEN
      WRITE (6,9400) 'GQ_SFC', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, 'OBS_NUDGE_OPT', ival)
  IF ( rcode == nf_noerr ) THEN
    IF ( ival /= met_fdda_obs ) THEN
      WRITE (6,9100) 'OBS_NUDGE_OPT', TRIM(fl1), met_fdda_obs, TRIM(fl), ival
      GOTO 1001
    ENDIF
  ELSE
    IF ( TRIM(met_release) >= 'V2.2' ) THEN
      WRITE (6,9400) 'OBS_NUDGE_OPT', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'OBS_COEF_WIND', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_giv ) > rtol ) THEN
      WRITE (6,9200) 'OBS_COEF_WIND', TRIM(fl1), met_fdda_giv, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_obs == 1 ) ) THEN
      WRITE (6,9400) 'OBS_COEF_WIND', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'OBS_COEF_TEMP', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_git ) > rtol ) THEN
      WRITE (6,9200) 'OBS_COEF_TEMP', TRIM(fl1), met_fdda_git, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_obs == 1 ) ) THEN
      WRITE (6,9400) 'OBS_COEF_TEMP', rcode
      GOTO 1001
    ENDIF
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, 'OBS_COEF_MOIS', rval)
  IF ( rcode == nf_noerr ) THEN
    IF ( ABS( rval - met_fdda_giq ) > rtol ) THEN
      WRITE (6,9200) 'OBS_COEF_MOIS', TRIM(fl1), met_fdda_giq, TRIM(fl), rval
      GOTO 1001
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_obs == 1 ) ) THEN
      WRITE (6,9400) 'OBS_COEF_MOIS', rcode
      GOTO 1001
    ENDIF
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWRFHDR',                          &
              /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME RUN',    &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   FIRST FILE = ', a,                             &
              /, 1x, '***   VALUE IN FIRST FILE = ', i4,                   &
              /, 1x, '***   NEW FILE = ', a,                               &
              /, 1x, '***   VALUE IN NEW FILE = ', i4,                     &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWRFHDR',                          &
              /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME RUN',    &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   FIRST FILE = ', a,                             &
              /, 1x, '***   VALUE IN FIRST FILE = ', f13.3,                &
              /, 1x, '***   NEW FILE = ', a,                               &
              /, 1x, '***   VALUE IN NEW FILE = ', f13.3,                  &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWRFHDR',                          &
              /, 1x, '***   WRF FILES DO NOT SEEM TO BE FROM SAME RUN',    &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   FIRST FILE = ', a,                             &
              /, 1x, '***   VALUE IN FIRST FILE = ', a,                    &
              /, 1x, '***   NEW FILE = ', a,                               &
              /, 1x, '***   VALUE IN NEW FILE = ', a,                      &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHKWRFHDR',                          &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',      &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   RCODE = ', i3,                                 &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE chkwrfhdr
