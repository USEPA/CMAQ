
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


SUBROUTINE mm5v3opts (txt_cupa, txt_microphys, txt_lwrad, txt_swrad,  &
                      txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_lu)

!-------------------------------------------------------------------------------
! Name:     MM5 Version 3 Options
! Purpose:  Writes MM5 options to an array that will form metadata for this run.
! Revised:  30 Jul 2007  Original version.  (T. Otte)
!           25 Aug 2009  Added urban model to physics descriptions.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo

  IMPLICIT NONE

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
    CASE ( 1 )
      txt_cupa = 'None'
    CASE ( 2 )
      txt_cupa = 'Anthes-Kuo'
    CASE ( 3 )
      txt_cupa = 'Grell'
    CASE ( 4 )
      txt_cupa = 'Arakawa-Schubert'
    CASE ( 5 )
      txt_cupa = 'Fritsch-Chappell'
    CASE ( 6 )
      txt_cupa = 'Kain-Fritsch'
    CASE ( 7 )
      txt_cupa = 'Betts-Miller'
    CASE ( 8 )
      txt_cupa = 'Kain-Fritsch 2'
    CASE DEFAULT
      txt_cupa = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine explicit moisture (microphysics) scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_expl_moist )
    CASE ( 1 )
      txt_microphys = 'Dry'
    CASE ( 2 )
      txt_microphys = 'Stable Precip'
    CASE ( 3 )
      txt_microphys = 'Warm Rain (Hsie)'
    CASE ( 4 )
      txt_microphys = 'Simple Ice (Dudhia)'
    CASE ( 5 )
      txt_microphys = 'Mixed-Phase (Reisner 1)'
    CASE ( 6 )
      txt_microphys = 'Goddard'
    CASE ( 7 )
      txt_microphys = 'Reisner Graupel (Reisner 2)'
    CASE ( 8 )
      txt_microphys = 'Schultz'
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
      txt_lwrad = 'Simple Cooling'
    CASE ( 2 )
      txt_lwrad = 'Cloud-Radiation (Dudhia)'
    CASE ( 3 )
      txt_lwrad = 'CCM2'
    CASE ( 4 )
      txt_lwrad = 'RRTM'
    CASE DEFAULT
      txt_lwrad = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine shortwave radiation scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_sw_rad )
    CASE ( 1 )
      txt_swrad = 'Dudhia'
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
      txt_pbl = 'Bulk'
    CASE ( 2 )
      txt_pbl = 'Blackadar'
    CASE ( 3 )
      txt_pbl = 'Burk-Thompson'
    CASE ( 4 )
      txt_pbl = 'Mellor-Yamada (Eta)'
    CASE ( 5 )
      txt_pbl = 'MRF (Hong and Pan)'
    CASE ( 6 )
      txt_pbl = 'Gayno-Seaman'
    CASE ( 7 )
      txt_pbl = 'Pleim-Chang (ACM)'
    CASE DEFAULT
      txt_pbl = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine surface-layer scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_sfc_lay )
    CASE ( 1 )
      txt_sflay = 'Standard'
    CASE DEFAULT
      txt_sflay = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine surface scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_soil_lsm )
    CASE ( 0 )
      txt_lsm = 'Force-Restore (Blackadar) Slab'
    CASE ( 1 )
      txt_lsm = 'Five-Layer Soil Model'
    CASE ( 2 )
      txt_lsm = 'OSU or NOAH Land-Surface Model'
    CASE ( 3 )
      txt_lsm = 'Pleim-Xiu Land-Surface Model'
    CASE DEFAULT
      txt_lsm = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine urban model.
!-------------------------------------------------------------------------------

  txt_urban = 'No urban physics'

!-------------------------------------------------------------------------------
! Determine land use categorization.
!-------------------------------------------------------------------------------

  txt_lu(1:LEN(met_lu_src)) = TRIM(met_lu_src)

END SUBROUTINE mm5v3opts
