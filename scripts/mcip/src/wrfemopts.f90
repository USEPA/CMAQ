
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


SUBROUTINE wrfemopts (txt_cupa, txt_microphys, txt_lwrad, txt_swrad,  &
                      txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_lu)

!-------------------------------------------------------------------------------
! Name:     WRF ARW (EM) Options
! Purpose:  Writes WRF options to an array that will form metadata for this run.
! Revised:  31 Jul 2007  Original version.  (T. Otte)
!           15 Apr 2008  Updated for WRFv3 options.  (T. Otte)
!           25 Aug 2009  Updated for WRFv3.1 options.  Added urban model
!                        to physics descriptions.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo

  IMPLICIT NONE

  CHARACTER*1,   PARAMETER     :: blank0    = ' '
  INTEGER                      :: n
  CHARACTER*1,   PARAMETER     :: null0     = CHAR(0)
  CHARACTER*30,  INTENT(OUT)   :: txt_cupa
  CHARACTER*30,  INTENT(OUT)   :: txt_lsm
  CHARACTER*30,  INTENT(OUT)   :: txt_lu
  CHARACTER*30,  INTENT(OUT)   :: txt_lwrad
  CHARACTER*30,  INTENT(OUT)   :: txt_microphys
  CHARACTER*30,  INTENT(OUT)   :: txt_pbl
  CHARACTER*30,  INTENT(OUT)   :: txt_sflay
  CHARACTER*30,  INTENT(OUT)   :: txt_swrad
  CHARACTER*30,  INTENT(OUT)   :: txt_urban

!-------------------------------------------------------------------------------
! Determine cumulus parameterization scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_cumulus )
    CASE ( 0 )
      txt_cupa = 'None'
    CASE ( 1 )
      txt_cupa = 'Kain-Fritsch (new Eta)'
    CASE ( 2 )
      txt_cupa = 'Betts-Miller-Janjic'
    CASE ( 3 )
      txt_cupa = 'Grell-Devenyi'
    CASE ( 5 )
      txt_cupa = 'Grell G3'
    CASE ( 99 )
      txt_cupa = 'old Kain-Fritsch'
    CASE DEFAULT
      txt_cupa = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine explicit moisture (microphysics) scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_expl_moist )
    CASE ( 0 )
      txt_microphys = 'None'
    CASE ( 1 )
      txt_microphys = 'Kessler'
    CASE ( 2 )
      txt_microphys = 'Lin et al.'
    CASE ( 3 )
      txt_microphys = 'WSM 3-Class Simple Ice'
    CASE ( 4 )
      txt_microphys = 'WSM 5-Class'
    CASE ( 5 )
      txt_microphys = 'Ferrier (new Eta)'
    CASE ( 6 )
      txt_microphys = 'WSM 6-Class'
    CASE ( 7 )
      txt_microphys = 'Goddard GCE'
    CASE ( 8 )
      txt_microphys = 'Thompson'
    CASE ( 10 )
      txt_microphys = 'Morrison 2-moment'
    CASE ( 14 )
      txt_microphys = 'WDM 5-class'
    CASE ( 16 )
      txt_microphys = 'WDM 6-class'
    CASE ( 98 )
      IF ( met_release(1:4) >= "V3.1" ) THEN
        txt_microphys = 'old Thompson scheme'
      ELSE
        txt_microphys = 'NCEP 3-Class Simple Ice'
      ENDIF
    CASE ( 99 )
      txt_microphys = 'NCEP 5-Class'
    CASE DEFAULT
      txt_microphys = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine longwave radiation scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_lw_rad )
    CASE ( 0 )
      txt_lwrad = 'None'
    CASE ( 1 )
      txt_lwrad = 'RRTM'
    CASE ( 3 )
      txt_lwrad = 'CAM'
    CASE ( 4 )
      txt_lwrad = 'RRTMg'
    CASE ( 31 )
      txt_lwrad = 'Earth Held-Suarez'
    CASE ( 99 )
      txt_lwrad = 'GFDL (Eta)'
    CASE DEFAULT
      txt_lwrad = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine shortwave radiation scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_sw_rad )
    CASE ( 0 )
      txt_swrad = 'None'
    CASE ( 1 )
      txt_swrad = 'Dudhia'
    CASE ( 2 )
      txt_swrad = 'Goddard'
    CASE ( 3 )
      txt_swrad = 'CAM'
    CASE ( 4 )
      txt_swrad = 'RRTMg'
    CASE ( 99 )
      txt_swrad = 'GFDL (Eta)'
    CASE DEFAULT
      txt_swrad = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine planetary boundary layer scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_pbl )
    CASE ( 0 )
      txt_pbl = 'None'
    CASE ( 1 )
      txt_pbl = 'YSU'
    CASE ( 2 )
      txt_pbl = 'Mellor-Yamada-Janjic (Eta) TKE'
    CASE ( 4 )
      txt_pbl = 'QNSE'
    CASE ( 5 )
      txt_pbl = 'MYNN 2.5 level TKE'
    CASE ( 6 )
      txt_pbl = 'MYNN 3.0 level TKE'
    CASE ( 7 )
      txt_pbl = 'ACM2 (Pleim)'
    CASE ( 8 )
      txt_pbl = 'Bougeault and Lacarrere'
    CASE ( 99 )
      txt_pbl = 'MRF'
    CASE DEFAULT
      txt_pbl = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine surface-layer scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_sfc_lay )
    CASE ( 0 )
      txt_sflay = 'None'
    CASE ( 1 )
      txt_sflay = 'Monin-Obukhov'
    CASE ( 2 )
      txt_sflay = 'Monin-Obukhov (Janjic Eta)'
    CASE ( 4 )
      txt_sflay = 'QNSE'
    CASE ( 5 )
      txt_sflay = 'MYNN'
    CASE ( 7 )
      txt_sflay = 'Pleim'
    CASE DEFAULT
      txt_sflay = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine surface scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_soil_lsm )
    CASE ( 0 )
      txt_lsm = 'No Sfc Temperature Prediction'
    CASE ( 1 )
      txt_lsm = 'Thermal Diffusion'
    CASE ( 2 )
      txt_lsm = 'NOAH Land-Surface Model'
    CASE ( 3 )
      txt_lsm = 'RUC Land-Surface Model'
    CASE ( 7 )
      txt_lsm = 'Pleim-Xiu Land-Surface Model'
    CASE DEFAULT
      txt_lsm = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine urban model.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_urban_phys )
    CASE ( 0 )
      txt_urban = 'No urban physics'
    CASE ( 1 )
      txt_urban = 'Single-Layer UCM (Kusaka)'
    CASE ( 2 )
      txt_urban = 'Multi-Layer BEP (Martilli)'
    CASE DEFAULT
      txt_urban = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine land use categorization.
!-------------------------------------------------------------------------------

  txt_lu(1:LEN_TRIM(met_lu_src)) = met_lu_src(1:LEN_TRIM(met_lu_src))

  DO n = 1, LEN_TRIM(txt_lu)
    IF ( txt_lu(n:n) == null0 ) THEN
      txt_lu(n:n) = blank0
    ENDIF
  ENDDO

END SUBROUTINE wrfemopts
