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

SUBROUTINE wrfemopts (txt_cupa, txt_microphys, txt_lwrad, txt_swrad,  &
                      txt_pbl, txt_sflay, txt_lsm, txt_urban, txt_shcu, txt_lu)

!-------------------------------------------------------------------------------
! Name:     WRF ARW (EM) Options
! Purpose:  Writes WRF options to an array that will form metadata for this run.
! Revised:  31 Jul 2007  Original version.  (T. Otte)
!           15 Apr 2008  Updated for WRFv3 options.  (T. Otte)
!           25 Aug 2009  Updated for WRFv3.1 options.  Added urban model
!                        to physics descriptions.  (T. Otte)
!           31 Aug 2011  Updated for WRFv3.2 and WRFv3.3. options.  Added
!                        shallow convection option to physics descriptions.
!                        Changed F77 character declarations to F90 standard.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           11 May 2012  Updated for WRFv3.4 options.  (T. Otte)
!           23 Aug 2013  Updated for WRFv3.4.1 and WRFv3.5 optoins.  (T. Otte)
!           27 Apr 2015  Updated for WRFv3.6, WRFv3.6.1, and WRFv3.7.1
!                        options.  (T. Spero)
!           20 Jun 2017  Updated for WRFv3.8, WRFv3.8.1, and WRFv3.9
!                        options.  (T. Spero)
!           10 Feb 2018  Annotated LSM option if NOAH Mosaic is used. (T. Spero)
!           23 Nov 2018  Updated for WRFv4.0 options.  (T. Spero)
!           18 Jun 2019  Updated for WRFv4.1 options.  Improved metadata to
!                        annotate if radiative feedbacks were included in the
!                        convective scheme.  (T. Spero)
!           09 Apr 2024  Updated for options through WRFv4.5.2. Pulled options
!                        from WRF Users Guide for v4.5. Also included options
!                        planned for WRFv4.6 from their Git packages. (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo
  USE mcipparm, ONLY: ifmosaic, ifcuradfdbk

  IMPLICIT NONE

  CHARACTER(LEN=1),   PARAMETER     :: blank0    = ' '
  INTEGER                           :: n
  CHARACTER(LEN=1),   PARAMETER     :: null0     = CHAR(0)
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_cupa
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_lsm
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_lu
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_lwrad
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_microphys
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_pbl
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_sflay
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_shcu
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_swrad
  CHARACTER(LEN=30),  INTENT(OUT)   :: txt_urban

!-------------------------------------------------------------------------------
! Determine cumulus parameterization scheme.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_cumulus )
    CASE ( 0 )
      txt_cupa = 'None'
    CASE ( 1 )
      IF ( ifcuradfdbk ) THEN  ! feedback with radiation
        txt_cupa = 'KF with radiative feedback'
      ELSE
        txt_cupa = 'Kain-Fritsch'
      ENDIF
    CASE ( 2 )
      txt_cupa = 'Betts-Miller-Janjic'
    CASE ( 3 )
      IF ( met_release(1:4) >= "V3.5" ) THEN
        IF ( ifcuradfdbk ) THEN
          txt_cupa = 'GF with radiative feedback'
        ELSE
          txt_cupa = 'Grell-Freitas'
        ENDIF
      ELSE
        txt_cupa = 'Grell-Devenyi'
      ENDIF
    CASE ( 4 )
      txt_cupa = 'Old Simpl. Arakawa-Schubert'
    CASE ( 5 )
      IF ( ifcuradfdbk ) THEN
        txt_cupa = 'Grell G3 w radiative feedback'
      ELSE
        txt_cupa = 'Grell G3'
      ENDIF
    CASE ( 6 )
      txt_cupa = 'Tiedtke'
    CASE ( 7 )
      txt_cupa = 'Zhang-McFarlane (CESM)'
    CASE ( 10 )
      txt_cupa = 'Modified K-F with PDF trigger'
    CASE ( 11 )
      txt_cupa = 'MSKF with radiative feedback'
    CASE ( 14 )
      txt_cupa = 'KIAPS Simpl. Arakawa-Schubert'
    CASE ( 16 )
      txt_cupa = 'Newer Tiedtke'
    CASE ( 84 )
      txt_cupa = 'New Simpl. Ara-Schu HWRF'
    CASE ( 93 )
      txt_cupa = 'Grell-Devenyi'
    CASE ( 96 )
      txt_cupa = 'New Simpl. Ara-Schu (NSAS)'
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
      txt_microphys = 'Purdue Lin et al.'
    CASE ( 3 )
      txt_microphys = 'WSM 3-Class Simple Ice'
    CASE ( 4 )
      txt_microphys = 'WSM 5-Class'
    CASE ( 5 )
      txt_microphys = 'Ferrier (Eta) Hi-Res'
    CASE ( 6 )
      txt_microphys = 'WSM 6-Class'
    CASE ( 7 )
      txt_microphys = 'Goddard GCE (4 ice)'
    CASE ( 8 )
      txt_microphys = 'Thompson'
    CASE ( 9 )
      txt_microphys = 'Millbrandt-Yau'
    CASE ( 10 )
      txt_microphys = 'Morrison 2-moment'
    CASE ( 11 )
      txt_microphys = 'CAM 5.1'
    CASE ( 12 )
      txt_microphys = 'Millbrandt'
    CASE ( 13 )
      txt_microphys = 'Stony Brook-Lin'
    CASE ( 14 )
      txt_microphys = 'WDM 5-class'
    CASE ( 15 )
      txt_microphys = 'Ferrier (Eta) Hi-Res Advect'
    CASE ( 16 )
      txt_microphys = 'WDM 6-class'
    CASE ( 17 )
      txt_microphys = 'NSSL 2-moment (const CCN)'
    CASE ( 18 )
      txt_microphys = 'NSSL 2-moment (dyn CCN)'
    CASE ( 19 )
      txt_microphys = 'NSSL 1-moment 7-class'
    CASE ( 21 )
      txt_microphys = 'NSSL 1-moment 6-class'
    CASE ( 22 )
      txt_microphys = 'NSSL 2-moment (no hail)'
    CASE ( 24 )
      txt_microphys = 'WSM 7-Class'
    CASE ( 26 )
      txt_microphys = 'WDM 7-Class'
    CASE ( 28 )
      txt_microphys = 'Aerosol-Aware Thompson'
    CASE ( 30 )
      txt_microphys = 'HUJI spectral bin -- fast'
    CASE ( 32 )
      txt_microphys = 'HUJI spectral bin -- full'
    CASE ( 38 )
      txt_microphys = 'Thompson Hail/Graupel/Aero'
    CASE ( 40 )
      txt_microphys = 'Morrison+CESM aerosol'
    CASE ( 50 )
      txt_microphys = 'P3 1-category'
    CASE ( 51 )
      txt_microphys = 'P3 1-cat + dbl-mom cld water'
    CASE ( 52 )
      txt_microphys = 'P3 2 ice'
    CASE ( 53 )
      txt_microphys = 'P3 3 ice'
    CASE ( 55 )
      txt_microphys = 'Jensen ISHMAEL'
    CASE ( 56 )
      txt_microphys = 'NTU'
    CASE ( 95 )
      txt_microphys = 'Ferrier (old Eta) NAM'
    CASE ( 96 )
      txt_microphys = 'MAD WRF'
    CASE ( 97 )
      txt_microphys = 'GSFC GCE'
    CASE ( 98 )
      IF ( met_release(1:4) >= "V3.1" ) THEN
        txt_microphys = 'old Thompson scheme'
      ELSE
        txt_microphys = 'NCEP 3-Class Simple Ice'
      ENDIF
    CASE ( 99 )
      txt_microphys = 'NCEP 5-Class'
    CASE ( 106 )
      txt_microphys = 'WSM 6-Class R'
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
      txt_lwrad = 'RRTMG'
    CASE ( 5 )
      txt_lwrad = 'New Goddard'
    CASE ( 7 )
      txt_lwrad = 'Fu-Liou-Gu UCLA'
    CASE ( 14 )
      txt_lwrad = 'RRTMG-K'
    CASE ( 24 )
      txt_lwrad = 'Fast RRTMg'
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
      txt_swrad = 'RRTMG'
    CASE ( 5 )
      txt_swrad = 'New Goddard'
    CASE ( 7 )
      txt_swrad = 'Fu-Liou-Gu UCLA'
    CASE ( 14 )
      txt_swrad = 'RRTMG-K'
    CASE ( 24 )
      txt_swrad = 'Fast RRTMg'
    CASE ( 31 )
      txt_swrad = 'Earth Held-Suarez'
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
      txt_pbl = 'QNSE-EDMF'
    CASE ( 5 )
      txt_pbl = 'MYNN 2.5 level TKE'
    CASE ( 6 )
      txt_pbl = 'MYNN 3.0 level TKE'
    CASE ( 7 )
      txt_pbl = 'ACM2 (Pleim)'
    CASE ( 8 )
      txt_pbl = 'Bougeault and Lacarrere'
    CASE ( 9 )
      txt_pbl = 'UW Bretherton and Park (CESM)'
    CASE ( 10 )
      txt_pbl = 'Total Energy-Mass Flux (TEMF)'
    CASE ( 11 )
      txt_pbl = 'Shin-Hong scale-aware scheme'
    CASE ( 12 )
      txt_pbl = 'Grenier-Bretherton-McCaa'
    CASE ( 16 )
      txt_pbl = 'EEPS'
    CASE ( 17 )
      txt_pbl = 'KEPS'
    CASE ( 94 )
      txt_pbl = 'Quasi-Normal Scale Elim'
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
      IF ( met_release(1:4) >= "V3.6" ) THEN
        txt_sflay = 'Revised MM5 (Jimenez)'
      ELSE
        txt_sflay = 'Monin-Obukhov'
      ENDIF
    CASE ( 2 )
      txt_sflay = 'Monin-Obukhov (Janjic Eta)'
    CASE ( 4 )
      txt_sflay = 'QNSE'
    CASE ( 5 )
      txt_sflay = 'MYNN'
    CASE ( 7 )
      txt_sflay = 'Pleim'
    CASE ( 10 )
      txt_sflay = 'Total Energy-Mass Flux (TEMF)'
    CASE ( 11 )
      txt_sflay = 'Revised MM5 (Jimenez)'
    CASE ( 91 )
      txt_sflay = 'MM5 Similarity'
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
      IF ( ifmosaic ) THEN
        txt_lsm = 'NOAH Mosaic Land-Surface Model'
      ELSE
        txt_lsm = 'NOAH Land-Surface Model'
      ENDIF
    CASE ( 3 )
      txt_lsm = 'RUC Land-Surface Model'
    CASE ( 4 )
      txt_lsm = 'NOAH-MP'
    CASE ( 5 )
      txt_lsm = 'CLM4'
    CASE ( 7 )
      txt_lsm = 'Pleim-Xiu Land-Surface Model'
    CASE ( 8 )
      txt_lsm = 'Simplified SiB'
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
    CASE ( 3 )
      txt_urban = 'Multi-Layer Bldg Energy Model'
    CASE DEFAULT
      txt_urban = '*** Unknown ***'
  END SELECT

!-------------------------------------------------------------------------------
! Determine shallow convection option.
!-------------------------------------------------------------------------------

  SELECT CASE ( met_shal_cu )
    CASE ( -1 )
      txt_shcu = 'Grell 3D -- unknown'
    CASE ( 0 )
      txt_shcu = 'No shallow convection'
    CASE ( 1 )
      txt_shcu = 'Grell 3D shallow'
    CASE ( 2 )
      txt_shcu = 'UW Bretherton and Park (CESM)'
    CASE ( 3 )
      txt_shcu = 'GRIMS (YSU)'
    CASE ( 4 )
      txt_shcu = 'NSAS Shallow Scheme'
    CASE ( 5 )
      txt_shcu = 'Deng Shallow Scheme'
    CASE DEFAULT
      txt_shcu = '*** Unknown ***'
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
