
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


SUBROUTINE blddesc

!-------------------------------------------------------------------------------
! Name:     Build File Description
! Purpose:  Builds file description used as metadata in output files.
! Revised:  31 Jul 2007  Original version.  (T. Otte)
!           29 May 2008  Added meteorology model's nudging coefficients and
!                        earth radius (assumed in MCIP) to metadata.  (T. Otte)
!           26 Aug 2009  Added urban model option to output metadata.  Added
!                        spectral nudging to GRID_FDDA options.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE metinfo

  IMPLICIT NONE

  CHARACTER*16                 :: coeff_g
  CHARACTER*16                 :: coeff_q
  CHARACTER*16                 :: coeff_t
  CHARACTER*16                 :: coeff_v
  CHARACTER*16                 :: text
  CHARACTER*30                 :: txt_cupa
  CHARACTER*30                 :: txt_lsm
  CHARACTER*30                 :: txt_lu        = ' '
  CHARACTER*30                 :: txt_lwrad
  CHARACTER*30                 :: txt_microphys
  CHARACTER*30                 :: txt_pbl
  CHARACTER*30                 :: txt_sflay
  CHARACTER*30                 :: txt_swrad
  CHARACTER*30                 :: txt_urban

!-------------------------------------------------------------------------------
! Write primary header for output.
!-------------------------------------------------------------------------------

  fdesc(:)(1:) = ' '  ! initialize

  fdesc( 1)  = 'US EPA COMMUNITY MULTISCALE AIR QUALITY MODEL'
  fdesc( 2)  = 'METEOROLOGY-CHEMISTRY INTERFACE PROCESSOR'

  fdesc( 4)  = TRIM(progname) // ' ' // TRIM(ver) // '  FROZEN ' // vdate

  IF ( ( met_model == 1 ) .AND. ( met_iversion == 3 ) ) THEN
    text = 'MM5'
  ELSE IF ( ( met_model == 2 ) .AND. ( met_iversion == 2 ) ) THEN
    text = 'WRF ARW'
  ELSE
    text = 'UNKNOWN SOURCE'
  ENDIF

  fdesc( 7)  = 'INPUT METEOROLOGY DATA FROM ' // TRIM(text) // ' ' // TRIM(met_release)
  fdesc( 8)  = 'INPUT RUN INITIALIZED:  ' // TRIM(met_startdate)

  IF ( ( met_model == 1 ) .AND. ( met_iversion == 3 ) ) THEN
    CALL mm5v3opts (txt_cupa, txt_microphys, txt_lwrad, txt_swrad,  &
                    txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_lu)
  ELSE IF ( ( met_model == 2 ) .AND. ( met_iversion == 2 ) ) THEN
    CALL wrfemopts (txt_cupa, txt_microphys, txt_lwrad, txt_swrad,  &
                    txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_lu)
  ENDIF

  fdesc(10)  = 'CUMULUS PARAMETERIZATION:  ' // TRIM(txt_cupa)

  fdesc(12)  = 'MICROPHYSICS:  ' // TRIM(txt_microphys)

  fdesc(14)  = 'LONGWAVE RADIATION:  ' // TRIM(txt_lwrad)

  fdesc(16)  = 'SHORTWAVE RADIATION:  ' // TRIM(txt_swrad)

  fdesc(18)  = 'PBL SCHEME:  ' // TRIM(txt_pbl)

  fdesc(20)  = 'SURFACE LAYER SCHEME:  ' // TRIM(txt_sflay)

  fdesc(22)  = 'LAND-SURFACE SCHEME:  ' // TRIM(txt_lsm)

  fdesc(24)  = 'URBAN MODEL:  ' // TRIM(txt_urban)

  fdesc(26)  = 'LAND USE CLASSIFICATION:  ' // TRIM(txt_lu)

  IF ( met_fdda_3dan == 1 ) THEN
    text = 'GRID'
    IF ( met_fdda_gv3d >= 0.0 ) THEN
      WRITE ( coeff_v, '(es9.3, a)' ) met_fdda_gv3d, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_gt3d >= 0.0 ) THEN
      IF ( ( met_model == 1 ) .AND. ( met_fdda_gt3d == 0.0 ) .AND.  &
           ( met_fdda_gq3d > 0.0 ) ) THEN  ! Bug in MM5 header for GT?
        WRITE ( coeff_t, '(es9.3, a)' ) met_fdda_gt3d, ' ? BUG?'
      ELSE
        WRITE ( coeff_t, '(es9.3, a)' ) met_fdda_gt3d, ' s-1'
      ENDIF
    ELSE
      coeff_t = 'unknown'
    ENDIF
    IF ( met_fdda_gq3d >= 0.0 ) THEN
      WRITE ( coeff_q, '(es9.3, a)' ) met_fdda_gq3d, ' s-1'
    ELSE
      coeff_q = 'unknown'
    ENDIF
    coeff_g = 'not applicable'
  ELSE IF ( met_fdda_3dan == 2 ) THEN
    text = 'SPECTRAL'
    IF ( met_fdda_gv3d >= 0.0 ) THEN
      WRITE ( coeff_v, '(es9.3, a)' ) met_fdda_gv3d, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_gt3d >= 0.0 ) THEN
      WRITE ( coeff_t, '(es9.3, a)' ) met_fdda_gt3d, ' s-1'
    ELSE
      coeff_t = 'unknown'
    ENDIF
    coeff_q = 'not applicable'
    coeff_g = 'unknown'  ! pertains to spectral nudging but not in V3.1 header
  ELSE
    text    = 'OFF'
    coeff_v = 'not applicable'
    coeff_t = 'not applicable'
    coeff_q = 'not applicable'
    coeff_g = 'not applicable'
  ENDIF

  fdesc(28)  = '3D ANALYSIS NUDGING:  ' // TRIM(text)
  fdesc(29)  = '   WIND COEFF:  ' // TRIM(coeff_v)
  fdesc(30)  = '   TEMP COEFF:  ' // TRIM(coeff_t)
  fdesc(31)  = '   MOIS COEFF:  ' // TRIM(coeff_q)
  fdesc(32)  = '   GEOP COEFF:  ' // TRIM(coeff_g)

  IF ( met_fdda_sfan == 1 ) THEN
    text = 'ON'
    IF ( met_fdda_gvsfc >= 0.0 ) THEN
      WRITE ( coeff_v, '(es9.3, a)' ) met_fdda_gvsfc, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_gtsfc >= 0.0 ) THEN
      WRITE ( coeff_t, '(es9.3, a)' ) met_fdda_gtsfc, ' s-1'
    ELSE
      coeff_t = 'unknown'
    ENDIF
    IF ( met_fdda_gqsfc >= 0.0 ) THEN
      WRITE ( coeff_q, '(es9.3, a)' ) met_fdda_gqsfc, ' s-1'
    ELSE
      coeff_q = 'unknown'
    ENDIF
  ELSE
    text    = 'OFF'
    coeff_v = 'not applicable'
    coeff_t = 'not applicable'
    coeff_q = 'not applicable'
  ENDIF

  fdesc(34)  = 'SFC ANALYSIS NUDGING:  ' // TRIM(text)
  fdesc(35)  = '   WIND COEFF:  ' // TRIM(coeff_v)
  fdesc(36)  = '   TEMP COEFF:  ' // TRIM(coeff_t)
  fdesc(37)  = '   MOIS COEFF:  ' // TRIM(coeff_q)

  IF ( met_fdda_obs == 1 ) THEN
    text = 'ON'
    IF ( met_fdda_giv >= 0.0 ) THEN
      WRITE ( coeff_v, '(es9.3, a)' ) met_fdda_giv, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_git >= 0.0 ) THEN
      WRITE ( coeff_t, '(es9.3, a)' ) met_fdda_git, ' s-1'
    ELSE
      coeff_t = 'unknown'
    ENDIF
    IF ( met_fdda_giq >= 0.0 ) THEN
      WRITE ( coeff_q, '(es9.3, a)' ) met_fdda_giq, ' s-1'
    ELSE
      coeff_q = 'unknown'
    ENDIF
  ELSE
    text    = 'OFF'
    coeff_v = 'not applicable'
    coeff_t = 'not applicable'
    coeff_q = 'not applicable'
  ENDIF

  fdesc(39)  = 'OBS NUDGING:  ' // TRIM(text)
  fdesc(40)  = '   WIND COEFF:  ' // TRIM(coeff_v)
  fdesc(41)  = '   TEMP COEFF:  ' // TRIM(coeff_t)
  fdesc(42)  = '   MOIS COEFF:  ' // TRIM(coeff_q)

  WRITE ( text, '(f11.3, a)' ) eradm, ' m'
  fdesc(44)  = 'EARTH RADIUS ASSUMED IN MCIP:  ' // TRIM(text)

END SUBROUTINE blddesc
