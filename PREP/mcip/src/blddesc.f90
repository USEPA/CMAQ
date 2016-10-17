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

SUBROUTINE blddesc

!-------------------------------------------------------------------------------
! Name:     Build File Description
! Purpose:  Builds file description used as metadata in output files.
! Revised:  31 Jul 2007  Original version.  (T. Otte)
!           29 May 2008  Added meteorology model's nudging coefficients and
!                        earth radius (assumed in MCIP) to metadata.  (T. Otte)
!           26 Aug 2009  Added urban model option to output metadata.  Added
!                        spectral nudging to GRID_FDDA options.  (T. Otte)
!           22 Dec 2010  Changed format of print statements from "es9.3" to
!                        "es10.3" to take Intel compiler's recommendations
!                        and eliminate warning messages.  (T. Otte)
!           01 Sep 2011  Added shallow convection option (available in WRFv3.3)
!                        to output metadata.  Replace F77 character declarations
!                        with F90 standard.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           07 Dec 2011  Added MET_FDDA_GPH3D for spectral nudging coefficient
!                        toward geopotential.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE metinfo

  IMPLICIT NONE

  CHARACTER(LEN=16)                 :: coeff_g
  CHARACTER(LEN=16)                 :: coeff_q
  CHARACTER(LEN=16)                 :: coeff_t
  CHARACTER(LEN=16)                 :: coeff_v
  CHARACTER(LEN=16)                 :: text
  CHARACTER(LEN=30)                 :: txt_cupa
  CHARACTER(LEN=30)                 :: txt_lsm
  CHARACTER(LEN=30)                 :: txt_lu        = ' '
  CHARACTER(LEN=30)                 :: txt_lwrad
  CHARACTER(LEN=30)                 :: txt_microphys
  CHARACTER(LEN=30)                 :: txt_pbl
  CHARACTER(LEN=30)                 :: txt_sflay
  CHARACTER(LEN=30)                 :: txt_shcu
  CHARACTER(LEN=30)                 :: txt_swrad
  CHARACTER(LEN=30)                 :: txt_urban

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
                    txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_shcu, txt_lu)
  ELSE IF ( ( met_model == 2 ) .AND. ( met_iversion == 2 ) ) THEN
    CALL wrfemopts (txt_cupa, txt_microphys, txt_lwrad, txt_swrad,  &
                    txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_shcu, txt_lu)
  ENDIF

  fdesc(10)  = 'CUMULUS PARAMETERIZATION:  ' // TRIM(txt_cupa)

  fdesc(12)  = 'SHALLOW CONVECTION:  ' // TRIM(txt_shcu)

  fdesc(14)  = 'MICROPHYSICS:  ' // TRIM(txt_microphys)

  fdesc(16)  = 'LONGWAVE RADIATION:  ' // TRIM(txt_lwrad)

  fdesc(18)  = 'SHORTWAVE RADIATION:  ' // TRIM(txt_swrad)

  fdesc(20)  = 'PBL SCHEME:  ' // TRIM(txt_pbl)

  fdesc(22)  = 'SURFACE LAYER SCHEME:  ' // TRIM(txt_sflay)

  fdesc(24)  = 'LAND-SURFACE SCHEME:  ' // TRIM(txt_lsm)

  fdesc(26)  = 'URBAN MODEL:  ' // TRIM(txt_urban)

  fdesc(28)  = 'LAND USE CLASSIFICATION:  ' // TRIM(txt_lu)

  IF ( met_fdda_3dan == 1 ) THEN
    text = 'GRID'
    IF ( met_fdda_gv3d >= 0.0 ) THEN
      WRITE ( coeff_v, '(es10.3, a)' ) met_fdda_gv3d, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_gt3d >= 0.0 ) THEN
      IF ( ( met_model == 1 ) .AND. ( met_fdda_gt3d == 0.0 ) .AND.  &
           ( met_fdda_gq3d > 0.0 ) ) THEN  ! Bug in MM5 header for GT?
        WRITE ( coeff_t, '(es10.3, a)' ) met_fdda_gt3d, ' ? BUG?'
      ELSE
        WRITE ( coeff_t, '(es10.3, a)' ) met_fdda_gt3d, ' s-1'
      ENDIF
    ELSE
      coeff_t = 'unknown'
    ENDIF
    IF ( met_fdda_gq3d >= 0.0 ) THEN
      WRITE ( coeff_q, '(es10.3, a)' ) met_fdda_gq3d, ' s-1'
    ELSE
      coeff_q = 'unknown'
    ENDIF
    coeff_g = 'not applicable'
  ELSE IF ( met_fdda_3dan == 2 ) THEN
    text = 'SPECTRAL'
    IF ( met_fdda_gv3d >= 0.0 ) THEN
      WRITE ( coeff_v, '(es10.3, a)' ) met_fdda_gv3d, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_gt3d >= 0.0 ) THEN
      WRITE ( coeff_t, '(es10.3, a)' ) met_fdda_gt3d, ' s-1'
    ELSE
      coeff_t = 'unknown'
    ENDIF
    coeff_q = 'not applicable'
    IF ( met_fdda_gph3d >= 0.0 ) THEN
      WRITE ( coeff_g, '(es10.3, a)' ) met_fdda_gph3d, ' s-1'
    ELSE
      coeff_g = 'unknown'
    ENDIF
  ELSE
    text    = 'OFF'
    coeff_v = 'not applicable'
    coeff_t = 'not applicable'
    coeff_q = 'not applicable'
    coeff_g = 'not applicable'
  ENDIF

  fdesc(30)  = '3D ANALYSIS NUDGING:  ' // TRIM(text)
  fdesc(31)  = '   WIND COEFF:  ' // TRIM(coeff_v)
  fdesc(32)  = '   TEMP COEFF:  ' // TRIM(coeff_t)
  fdesc(33)  = '   MOIS COEFF:  ' // TRIM(coeff_q)
  fdesc(34)  = '   GEOP COEFF:  ' // TRIM(coeff_g)

  IF ( met_fdda_sfan == 1 ) THEN
    text = 'ON'
    IF ( met_fdda_gvsfc >= 0.0 ) THEN
      WRITE ( coeff_v, '(es10.3, a)' ) met_fdda_gvsfc, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_gtsfc >= 0.0 ) THEN
      WRITE ( coeff_t, '(es10.3, a)' ) met_fdda_gtsfc, ' s-1'
    ELSE
      coeff_t = 'unknown'
    ENDIF
    IF ( met_fdda_gqsfc >= 0.0 ) THEN
      WRITE ( coeff_q, '(es10.3, a)' ) met_fdda_gqsfc, ' s-1'
    ELSE
      coeff_q = 'unknown'
    ENDIF
  ELSE
    text    = 'OFF'
    coeff_v = 'not applicable'
    coeff_t = 'not applicable'
    coeff_q = 'not applicable'
  ENDIF

  fdesc(36)  = 'SFC ANALYSIS NUDGING:  ' // TRIM(text)
  fdesc(37)  = '   WIND COEFF:  ' // TRIM(coeff_v)
  fdesc(38)  = '   TEMP COEFF:  ' // TRIM(coeff_t)
  fdesc(39)  = '   MOIS COEFF:  ' // TRIM(coeff_q)

  IF ( met_fdda_obs == 1 ) THEN
    text = 'ON'
    IF ( met_fdda_giv >= 0.0 ) THEN
      WRITE ( coeff_v, '(es10.3, a)' ) met_fdda_giv, ' s-1'
    ELSE
      coeff_v = 'unknown'
    ENDIF
    IF ( met_fdda_git >= 0.0 ) THEN
      WRITE ( coeff_t, '(es10.3, a)' ) met_fdda_git, ' s-1'
    ELSE
      coeff_t = 'unknown'
    ENDIF
    IF ( met_fdda_giq >= 0.0 ) THEN
      WRITE ( coeff_q, '(es10.3, a)' ) met_fdda_giq, ' s-1'
    ELSE
      coeff_q = 'unknown'
    ENDIF
  ELSE
    text    = 'OFF'
    coeff_v = 'not applicable'
    coeff_t = 'not applicable'
    coeff_q = 'not applicable'
  ENDIF

  fdesc(41)  = 'OBS NUDGING:  ' // TRIM(text)
  fdesc(42)  = '   WIND COEFF:  ' // TRIM(coeff_v)
  fdesc(43)  = '   TEMP COEFF:  ' // TRIM(coeff_t)
  fdesc(44)  = '   MOIS COEFF:  ' // TRIM(coeff_q)

  WRITE ( text, '(f11.3, a)' ) eradm, ' m'
  fdesc(46)  = 'EARTH RADIUS ASSUMED IN MCIP:  ' // TRIM(text)

END SUBROUTINE blddesc
