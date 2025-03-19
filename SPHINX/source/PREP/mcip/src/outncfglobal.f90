SUBROUTINE outncfglobal (cdfid_in, fl)

!-------------------------------------------------------------------------------
! Name:     Output netCDF Global Attributes
! Purpose:  Write netCDF global attributes.
! Revised:  19 Dec 2018  Original version.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo
  USE mcipparm
  USE netcdf
  USE coord  !  <-- these are I/O API coordinate variables

  IMPLICIT NONE

  INTEGER,            INTENT(IN)  :: cdfid_in
  CHARACTER(LEN=32)               :: cstr
  CHARACTER(LEN=256), INTENT(IN)  :: fl
  CHARACTER(LEN=16),  PARAMETER   :: pname      = 'OUTNCFGLOBAL'
  INTEGER                         :: rcode
  CHARACTER(LEN=32)               :: var

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING ATTRIBUTE FOR', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Define global attributes.
!-------------------------------------------------------------------------------

  var = "PROGNAME"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, progname)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "VERSION"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ver)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "CODE_DATE"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vdate)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "INPUT_MODEL"
  IF ( met_model == 2 ) THEN
    cstr = "WRF ARW " // TRIM(met_release)
  ELSE
    cstr = " "
  ENDIF
  rcode = nf90_put_att (cdfid_in, nf90_global, var, cstr)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "NCOLS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ncols)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "NROWS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, nrows)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "NLAYS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, nlays)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "NTHIK"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, nthik)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "GDTYP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, gdtyp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "P_ALP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, p_alp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "P_BET"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, p_bet_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "P_GAM"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, p_gam_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "XCENT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, xcent_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "YCENT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ycent_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "XORIG"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, xorig_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "YORIG"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, yorig_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "XCELL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, xcell_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "YCELL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ycell_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "VGTYP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vgtyp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "VGTOP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vgtop_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "VGTOP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vgtop_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "VGLVLS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vglvs_gd(:))
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "CEN_LAT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_cen_lat)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "CEN_LON"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_cen_lon)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "TRUELAT1"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_tru1)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "TRUELAT2"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_tru2)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MOAD_CEN_LAT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_proj_clat)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_REF_LAT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_ref_lat)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "STAND_LON"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_proj_clon)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "DX"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_resoln)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "DY"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_resoln)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "PTOP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_ptop)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_CUMULUS" 
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_cumulus)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_SHAL_CU"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_shal_cu)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_MICROPHYS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_expl_moist)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_LW_RAD"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_lw_rad)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_SW_RAD"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_sw_rad)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_PBL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_pbl)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_SFC_LAY"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_sfc_lay)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_LSM"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_soil_lsm)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_URBAN_PHYS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_urban_phys)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "LAND_USE_SOURCE"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_lu_src)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "MET_FDDA_3DAN"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_3dan)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( met_fdda_3dan > 0 ) THEN  ! 3d nudging (any variety)

    var = "MET_FDDA_3DAN_WIND"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gv3d)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "MET_FDDA_3DAN_TEMP"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gt3d)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "MET_FDDA_3DAN_MOIS"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gq3d)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_fdda_3dan == 2 ) THEN  ! spectral nudging only
      var = "MET_FDDA_3DAN_GEOP"
      rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gph3d)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

  ENDIF  ! 3d nudging

  var = "MET_FDDA_SFAN"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_sfan)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( met_fdda_sfan > 0 ) THEN  ! surface nudging

    var = "MET_FDDA_SFAN_WIND"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gvsfc)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "MET_FDDA_SFAN_TEMP"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gtsfc)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "MET_FDDA_SFAN_MOIS"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_gqsfc)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! surface nudging

  var = "MET_FDDA_OBS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_obs)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( met_fdda_obs > 0 ) THEN  ! observation nudging

    var = "MET_FDDA_OBS_WIND"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_giv)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "MET_FDDA_OBS_TEMP"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_git)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "MET_FDDA_OBS_MOIS"
    rcode = nf90_put_att (cdfid_in, nf90_global, var, met_fdda_giq)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! surface nudging

  var = "MET_HYBRID"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, met_hybrid)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  var = "EARTH_RADIUS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, eradm)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

END SUBROUTINE outncfglobal
