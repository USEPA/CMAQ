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
!           23 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Updated netCDF commands
!                        to F90, and improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE files
  USE metinfo
  USE netcdf

  IMPLICIT NONE

  INTEGER,            INTENT(IN)    :: cdfid
  CHARACTER(LEN=80)                 :: cval
  CHARACTER(LEN=8)                  :: cval8
  CHARACTER(LEN=19)                 :: date_init
  INTEGER                           :: dimid
  CHARACTER(LEN=256), INTENT(IN)    :: fl
  CHARACTER(LEN=256)                :: fl1
  INTEGER                           :: ival
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'CHKWRFHDR'
  INTEGER                           :: rcode
  REAL,               PARAMETER     :: rtol      = 1.0e-4
  REAL                              :: rval

!-------------------------------------------------------------------------------
! Error-handling section.
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
    & /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
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
      WRITE (*,f9100) TRIM(fl1), met_nx, TRIM(fl), ival
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
      WRITE (*,f9100) TRIM(fl1), met_ny, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SOUTH-NORTH_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'BOTTOM-TOP_GRID_DIMENSION', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival-1 /= met_nz ) THEN
      WRITE (*,f9000) TRIM(pname), 'BOTTOM-TOP_GRID_DIMENSION'
      WRITE (*,f9100) TRIM(fl1), met_nz, TRIM(fl), ival-1
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'BOTTOM-TOP_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Check domain attributes.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'TITLE', cval)
  IF ( rcode == nf90_noerr ) THEN
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
      WRITE (*,f9000) TRIM(pname), 'WRF VERSION'
      WRITE (*,f9300) TRIM(fl1), met_release, TRIM(fl), cval8
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'TITLE', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

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

!-------------------------------------------------------------------------------
! Extract model run options.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'MMINLU', cval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( cval(1:4) /= met_lu_src(1:4) ) THEN
      WRITE (*,f9000) TRIM(pname), 'MMINLU'
      WRITE (*,f9300) TRIM(fl1), met_lu_src(1:4), TRIM(fl), cval(1:4)
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MMINLU', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'ISWATER', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_lu_water ) THEN
      WRITE (*,f9000) TRIM(pname), 'ISWATER'
      WRITE (*,f9100) TRIM(fl1), met_lu_water, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'ISWATER', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_inq_dimid (cdfid, 'soil_layers_stag', dimid)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'ID for soil_layers_stag',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  rcode = nf90_inquire_dimension (cdfid, dimid, len=ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_ns ) THEN
      WRITE (*,f9000) TRIM(pname), 'soil_layers_stag'
      WRITE (*,f9100) TRIM(fl1), met_ns, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'value for soil_layers_stag',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'RA_LW_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_lw_rad ) THEN
      WRITE (*,f9000) TRIM(pname), 'RA_LW_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_lw_rad, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'RA_LW_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'RA_SW_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_sw_rad ) THEN
      WRITE (*,f9000) TRIM(pname), 'RA_SW_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_sw_rad, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'RA_SW_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'CU_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_cumulus ) THEN
      WRITE (*,f9000) TRIM(pname), 'CU_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_cumulus, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'CU_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'MP_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_expl_moist ) THEN
      WRITE (*,f9000) TRIM(pname), 'MP_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_expl_moist, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MP_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'BL_PBL_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_pbl ) THEN
      WRITE (*,f9000) TRIM(pname), 'BL_PBL_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_pbl, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'BL_PBL_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'SF_SFCLAY_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_sfc_lay ) THEN
      WRITE (*,f9000) TRIM(pname), 'SF_SFCLAY_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_sfc_lay, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SF_SFCLAY_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'SF_SURFACE_PHYSICS', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_soil_lsm ) THEN
      WRITE (*,f9000) TRIM(pname), 'SF_SURFACE_PHYSICS'
      WRITE (*,f9100) TRIM(fl1), met_soil_lsm, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SF_SURFACE_PHYSICS',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( TRIM(met_release) >= 'V3.1' ) THEN
    rcode = nf90_get_att (cdfid, nf90_global, 'SF_URBAN_PHYSICS', ival)
    IF ( rcode == nf90_noerr ) THEN
      IF ( ival /= met_urban_phys ) THEN
        WRITE (*,f9000) TRIM(pname), 'SF_URBAN_PHYSICS'
        WRITE (*,f9100) TRIM(fl1), met_urban_phys, TRIM(fl), ival
        CALL graceful_stop (pname)
      ENDIF
    ELSE
      WRITE (*,f9400) TRIM(pname), 'SF_URBAN_PHYSICS',  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Extract WRF start date and time information.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'SIMULATION_START_DATE', date_init)
  IF ( rcode == nf90_noerr ) THEN
    date_init(11:11) = "-"  ! change from "_" to "-" for consistency
    IF ( date_init /= met_startdate(1:19) ) THEN
      WRITE (*,f9000) TRIM(pname), 'SIMULATION_START_DATE'
      WRITE (*,f9300) TRIM(fl1), met_startdate(1:19), TRIM(fl),  date_init
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SIMULATION_START_DATE',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Determine FDDA options.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'GRID_FDDA', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_fdda_3dan ) THEN
      WRITE (*,f9000) TRIM(pname), 'GRID_FDDA'
      WRITE (*,f9100) TRIM(fl1), met_fdda_3dan, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( TRIM(met_release) >= 'V2.2' ) THEN
      WRITE (*,f9400) TRIM(pname), 'GRID_FDDA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GUV', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_gv3d ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'GUV'
      WRITE (*,f9200) TRIM(fl1), met_fdda_gv3d, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_3dan == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GUV', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ELSE IF ( ( TRIM(met_release) >= 'V3.1' ) .AND.  &
              ( met_fdda_3dan == 2 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GUV', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GT', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_gt3d ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'GT'
      WRITE (*,f9200) TRIM(fl1), met_fdda_gt3d, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_3dan == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ELSE IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_3dan == 2 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GQ', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_gq3d ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'GQ'
      WRITE (*,f9200) TRIM(fl1), met_fdda_gq3d, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_3dan == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GQ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GRID_SFDDA', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_fdda_sfan ) THEN
      WRITE (*,f9000) TRIM(pname), 'GRID_SFDDA'
      WRITE (*,f9100) TRIM(fl1), met_fdda_sfan, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( TRIM(met_release) >= 'V3.1' ) THEN
      WRITE (*,f9400) TRIM(pname), 'GRID_SFDDA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GUV_SFC', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_gvsfc ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'GUV_SFC'
      WRITE (*,f9200) TRIM(fl1), met_fdda_gvsfc, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_sfan == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GUV_SFC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GT_SFC', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_gtsfc ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'GT_SFC'
      WRITE (*,f9200) TRIM(fl1), met_fdda_gtsfc, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_sfan == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GT_SFC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GQ_SFC', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_gqsfc ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'GQ_SFC'
      WRITE (*,f9200) TRIM(fl1), met_fdda_gqsfc, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V3.1' ) .AND. ( met_fdda_sfan == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'GQ_SFC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_NUDGE_OPT', ival)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ival /= met_fdda_obs ) THEN
      WRITE (*,f9000) TRIM(pname), 'OBS_NUDGE_OPT'
      WRITE (*,f9100) TRIM(fl1), met_fdda_obs, TRIM(fl), ival
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( TRIM(met_release) >= 'V2.2' ) THEN
      WRITE (*,f9400) TRIM(pname), 'OBS_NUDGE_OPT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_COEF_WIND', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_giv ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'OBS_COEF_WIND'
      WRITE (*,f9200) TRIM(fl1), met_fdda_giv, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_obs == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'OBS_COEF_WIND', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_COEF_TEMP', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_git ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'OBS_COEF_TEMP'
      WRITE (*,f9200) TRIM(fl1), met_fdda_git, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_obs == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'OBS_COEF_TEMP', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_COEF_MOIS', rval)
  IF ( rcode == nf90_noerr ) THEN
    IF ( ABS( rval - met_fdda_giq ) > rtol ) THEN
      WRITE (*,f9000) TRIM(pname), 'OBS_COEF_MOIS'
      WRITE (*,f9200) TRIM(fl1), met_fdda_giq, TRIM(fl), rval
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    IF ( ( TRIM(met_release) >= 'V2.2' ) .AND. ( met_fdda_obs == 1 ) ) THEN
      WRITE (*,f9400) TRIM(pname), 'OBS_COEF_MOIS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

END SUBROUTINE chkwrfhdr
