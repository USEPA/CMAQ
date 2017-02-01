
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module lus_defn
C Land Use/Cover Schema

C Revision History:
C   10 Nov 2011: J.Young - Created
C   10 Dec 2011: J.Young - fix for two-way application (Tanya Spero`s help)

      implicit none

      real,    allocatable, save :: ladut( :,:,: ) ! % of desertland
      real,    allocatable, save :: uland( :,:,: ) ! % of 4 land categories groups
      real,    allocatable, save :: dmsk( : )      ! desert fraction mask
      integer, allocatable, save :: dmap( : )      ! desert land map to BELD3
      integer, save              :: n_lucat        ! number of landuse categories
      integer, save              :: n_dlcat        ! number of desert land categories

      type lus_type
         character( 16 ) :: name
         character( 64 ) :: desc
      end type lus_type

      type( lus_type ), allocatable :: vnmld( : )

      public ladut, uland, dmsk, dmap, n_lucat, n_dlcat, lus_init, lus_type, vnmld
      private

C BELD - Biogenic Emission Landcover Database
C USGS - United States Geological Survey
C NOAH - A community land-surface model [N=NCEP, O=Oregon State Univ.,
C        A=Air Force, H=Hydrologic Research Lab - NWS(now OHD: Office of
C        Hydrologic Devel.)]
C MODIS - MODerate resolution Imaging Spectroradiometer
C NLCD - National Land Cover Dataset
C IGBP - International Geosphere-Biosphere Programme

C BELD3 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     read files: DUST_LU_1 & DUST_LU_2

      integer, parameter :: n_dlcat_beld3 = 3   ! number of desert land categories
      type( lus_type ), parameter :: vnmld_beld3( n_dlcat_beld3 ) = (/
     & lus_type( 'USGS_shrubland ', 'shrub_land    ' ),
     & lus_type( 'USGS_shrubgrass', 'shrub_grass   ' ),
     & lus_type( 'USGS_sprsbarren', 'sparse_barren ' ) /)

      real :: dmsk_beld3( n_dlcat_beld3 ) = ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.25,    ! shrubgrass
     &              0.75 /)  ! barrenland

      integer :: dmap_beld3( n_dlcat_beld3+1 ) = ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              2,       ! shrubgrass
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

      integer, parameter :: n_lucat_beld3 = 20   ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_beld3( n_lucat_beld3 ) = (/
     & lus_type( 'USGS_urban      ', 'urban               ' ),
     & lus_type( 'USGS_drycrop    ', 'non_irrigated crops ' ),
     & lus_type( 'USGS_irrcrop    ', 'irrigated crops     ' ),
     & lus_type( 'USGS_cropgrass  ', 'crop_grass          ' ),
     & lus_type( 'USGS_cropwdlnd  ', 'crop_wood land      ' ),
     & lus_type( 'USGS_grassland  ', 'grass land          ' ),
     & lus_type( 'USGS_shrubland  ', 'shrub land          ' ),
     & lus_type( 'USGS_shrubgrass ', 'shrub_grass         ' ),
     & lus_type( 'USGS_savanna    ', 'savanna             ' ),
     & lus_type( 'USGS_decidforest', 'deciduous forest    ' ),
     & lus_type( 'USGS_evbrdleaf  ', 'evergreen_broad_leaf' ),
     & lus_type( 'USGS_coniferfor ', 'coniferous forest   ' ),
     & lus_type( 'USGS_mxforest   ', 'mixed forest        ' ),
     & lus_type( 'USGS_water      ', 'water               ' ),
     & lus_type( 'USGS_wetwoods   ', 'wet_woods           ' ),
     & lus_type( 'USGS_sprsbarren ', 'sparse_barren       ' ),
     & lus_type( 'USGS_woodtundr  ', 'wood_tundra         ' ),
     & lus_type( 'USGS_mxtundra   ', 'mixed_tundra        ' ),
     & lus_type( 'USGS_snowice    ', 'snow_ice            ' ),
     & lus_type( 'FOREST          ', 'total forest        ' ) /)  ! DUST_LU_2

C BELD4 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     read file: BELD4_LU

      integer, parameter :: n_dlcat_beld4 = 6   ! number of desert land categories
      type( lus_type ), parameter :: vnmld_beld4( n_dlcat_beld4 ) = (/
     & lus_type( 'NLCD_52 ', 'Shrub/Scrub                 ' ),  ! shrubland?
     & lus_type( 'MODIS_6 ', 'closed shrublands           ' ),
     & lus_type( 'MODIS_7 ', 'open shrublands             ' ),
     & lus_type( 'NLCD_51 ', 'Dwarf Scrub                 ' ),  ! shrubgrass?
     & lus_type( 'NLCD_31 ', 'Barren Land (Rock/Sand/Clay)' ),
     & lus_type( 'MODIS_16', 'barren or sparsely vegetated' ) /)

      real :: dmsk_beld4( n_dlcat_beld4 ) = ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.50,    ! shrubland
     &              0.50,    ! shrubland
     &              0.25,    ! shrubgrass
     &              0.75,    ! barrenland
     &              0.75 /)  ! barrenland

      integer :: dmap_beld4( n_dlcat_beld4+1 ) = ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              1,       ! shrubland
     &              1,       ! shrubland
     &              2,       ! shrubgrass
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

      integer, parameter :: n_lucat_beld4 = 50  ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_beld4( n_lucat_beld4 ) = (/
     & lus_type( 'NLCD_11  ', 'Open Water                                  '),   !  1
     & lus_type( 'NLCD_12  ', 'Perennial Ice/Snow                          '),   !  2
     & lus_type( 'NLCD_21  ', 'Developed Open Space (not used)             '),   !  3 ?
     & lus_type( 'NLCD_22  ', 'Developed Low Intensity                     '),   !  4
     & lus_type( 'NLCD_23  ', 'Developed Medium Intensity                  '),   !  5
     & lus_type( 'NLCD_24  ', 'Developed High Intensity                    '),   !  6
     & lus_type( 'NLCD_31  ', 'Barren Land (Rock/Sand/Clay)                '),   !  7
     & lus_type( 'NLCD_32  ', 'Unconsolidated Shore (not used)             '),   !  8 ?
     & lus_type( 'NLCD_41  ', 'Deciduous Forest                            '),   !  9
     & lus_type( 'NLCD_42  ', 'Evergreen Forest                            '),   ! 10
     & lus_type( 'NLCD_43  ', 'Mixed Forest                                '),   ! 11
     & lus_type( 'NLCD_51  ', 'Dwarf Scrub                                 '),   ! 12
     & lus_type( 'NLCD_52  ', 'Shrub/Scrub                                 '),   ! 13
     & lus_type( 'NLCD_71  ', 'Grassland/Herbaceous                        '),   ! 14
     & lus_type( 'NLCD_72  ', 'Sedge/Herbaceous                            '),   ! 15
     & lus_type( 'NLCD_73  ', 'Lichens                                     '),   ! 16
     & lus_type( 'NLCD_74  ', 'Moss                                        '),   ! 17
     & lus_type( 'NLCD_75  ', 'Tundra                                      '),   ! 18
     & lus_type( 'NLCD_81  ', 'Pasture/Hay                                 '),   ! 19
     & lus_type( 'NLCD_82  ', 'Cultivated Crops                            '),   ! 20
     & lus_type( 'NLCD_90  ', 'Woody Wetlands                              '),   ! 21
     & lus_type( 'NLCD_91  ', 'Palustrine Forested Wetland                 '),   ! 22
     & lus_type( 'NLCD_92  ', 'Palustrine Scrub/Shrub Wetland (not used)   '),   ! 23 ?
     & lus_type( 'NLCD_93  ', 'Estuarine Forested Wetland                  '),   ! 24
     & lus_type( 'NLCD_94  ', 'Estuarine Scrub_Shrub Wetland (not used)    '),   ! 25 ?
     & lus_type( 'NLCD_95  ', 'Emergent Herbaceous Wetlands (not used)     '),   ! 26 ?
     & lus_type( 'NLCD_96  ', 'Palustrine Emergent Wetland (not used)      '),   ! 27 ?
     & lus_type( 'NLCD_97  ', 'Estuarine Emergent Wetland (not used)       '),   ! 28 ?
     & lus_type( 'NLCD_98  ', 'Palustrine Aquatic Bed (not used)           '),   ! 29 ?
     & lus_type( 'NLCD_99  ', 'Estuarine Aquatic Bed  (not used)           '),   ! 30 ?
     & lus_type( 'MODIS_0  ', 'water                                       '),   ! 31
     & lus_type( 'MODIS_1  ', 'evergreen needleleaf forest                 '),   ! 32
     & lus_type( 'MODIS_2  ', 'evergreen broadleaf forest                  '),   ! 33
     & lus_type( 'MODIS_3  ', 'deciduous needleleaf forest                 '),   ! 34
     & lus_type( 'MODIS_4  ', 'deciduous broadleaf forest                  '),   ! 35
     & lus_type( 'MODIS_5  ', 'mixed forests                               '),   ! 36
     & lus_type( 'MODIS_6  ', 'closed shrublands                           '),   ! 37
     & lus_type( 'MODIS_7  ', 'open shrublands                             '),   ! 38
     & lus_type( 'MODIS_8  ', 'woody savannas                              '),   ! 39
     & lus_type( 'MODIS_9  ', 'savannas                                    '),   ! 40
     & lus_type( 'MODIS_10 ', 'grasslands                                  '),   ! 41
     & lus_type( 'MODIS_11 ', 'permanent wetlands                          '),   ! 42
     & lus_type( 'MODIS_12 ', 'croplands                                   '),   ! 43
     & lus_type( 'MODIS_13 ', 'urban and built up                          '),   ! 44
     & lus_type( 'MODIS_14 ', 'cropland / natural vegetation mosaic        '),   ! 45
     & lus_type( 'MODIS_15 ', 'permanent snow and ice                      '),   ! 46
     & lus_type( 'MODIS_16 ', 'barren or sparsely vegetated                '),   ! 47
     & lus_type( 'MODIS_17 ', 'IGBP water                                  '),   ! 48
     & lus_type( 'MODIS_254', 'unclassified (not used)                     '),   ! 49 ?
     & lus_type( 'MODIS_255', 'fill value (normally ocean water) (not used)') /) ! 50 ?

C USGS24 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     read file: GRID_CRO_2D

      integer, parameter :: n_dlcat_usgs24 = 4   ! number of desert land categories
      type( lus_type ), parameter :: vnmld_usgs24( n_dlcat_usgs24 ) = (/
     & lus_type( 'LUFRAC_08', 'Shrubland                   ' ),
     & lus_type( 'LUFRAC_19', 'Barren or Sparsely Vegetated' ),
     & lus_type( 'LUFRAC_22', 'Mixed Tundra                ' ),
     & lus_type( 'LUFRAC_23', 'Bare Ground Tundra          ' ) /)

!!!!!    Is some 'tundra' desert land?

      real :: dmsk_usgs24( n_dlcat_usgs24 ) =   ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.75,    ! barrenland
     &              0.75,    ! barrenland
     &              0.75 /)  ! barrenland

      integer :: dmap_usgs24( n_dlcat_usgs24+1 ) =   ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

      integer, parameter :: n_lucat_usgs24 = 24   ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_usgs24( n_lucat_usgs24 ) = (/
     & lus_type( 'LUFRAC_01', 'Urban Land                                      ' ),
     & lus_type( 'LUFRAC_02', 'Dryland Cropland and Pasture                    ' ),
     & lus_type( 'LUFRAC_03', 'Irrigated Cropland and Pasture                  ' ),
     & lus_type( 'LUFRAC_04', 'Mixed Dryland and Irrigated Cropland and Pasture' ),
     & lus_type( 'LUFRAC_05', 'Cropland-Grassland Mosaic                       ' ),
     & lus_type( 'LUFRAC_06', 'Cropland-Woodland Mosaic                        ' ),
     & lus_type( 'LUFRAC_07', 'Grassland                                       ' ),
     & lus_type( 'LUFRAC_08', 'Shrubland                                       ' ),
     & lus_type( 'LUFRAC_09', 'Mixed Shrubland-Grassland                       ' ),
     & lus_type( 'LUFRAC_10', 'Savannah                                        ' ),
     & lus_type( 'LUFRAC_11', 'Deciduous Broadleaf Forest                      ' ),
     & lus_type( 'LUFRAC_12', 'Deciduous Needleleaf Forest                     ' ),
     & lus_type( 'LUFRAC_13', 'Evergreen Broadleaf Forest                      ' ),
     & lus_type( 'LUFRAC_14', 'Evergreen Needleleaf Forest                     ' ),
     & lus_type( 'LUFRAC_15', 'Mixed Forest                                    ' ),
     & lus_type( 'LUFRAC_16', 'Water                                           ' ),
     & lus_type( 'LUFRAC_17', 'Herbaceous Wetland                              ' ),
     & lus_type( 'LUFRAC_18', 'Wooded Wetland                                  ' ),
     & lus_type( 'LUFRAC_19', 'Barren or Sparsely Vegetated                    ' ),
     & lus_type( 'LUFRAC_20', 'Herbaceous Tundra                               ' ),
     & lus_type( 'LUFRAC_21', 'Wooded Tundra                                   ' ),
     & lus_type( 'LUFRAC_22', 'Mixed Tundra                                    ' ),
     & lus_type( 'LUFRAC_23', 'Bare Ground Tundra                              ' ),
     & lus_type( 'LUFRAC_24', 'Snow or Ice                                     ' ) /)

C MODIS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     read file: GRID_CRO_2D

      integer, parameter :: n_dlcat_modis = 1   ! number of desert land categories
      type( lus_type ), parameter :: vnmld_modis( n_dlcat_modis ) = (/
     & lus_type( 'nada', 'nada' ) /)
      real :: dmsk_modis( n_dlcat_modis ) = 0.0  ! land use type desert fraction
      integer :: dmap_modis( n_dlcat_modis+1 ) = 0 ! land use type desert map to BELD3
      integer, parameter :: n_lucat_modis = 1   ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_modis( n_lucat_modis ) = (/
     & lus_type( 'nada', 'nada' ) /)

! 1  MODIS_0:    31. MODIS: 0 water
! 4  MODIS_1:    32. MODIS: 1 evergreen needleleaf forest
! 4  MODIS_2:    33. MODIS: 2 evergreen broadleaf forest
! 4  MODIS_3:    34. MODIS: 3 deciduous needleleaf forest
! 4  MODIS_4:    35. MODIS: 4 deciduous broadleaf forest
! 4  MODIS_5:    36. MODIS: 5 mixed forests
! 2  MODIS_6:    37. MODIS: 6 closed shrublands
! 2  MODIS_7:    38. MODIS: 7 open shrublands
! 4  MODIS_8:    39. MODIS: 8 woody savannas
! 2  MODIS_9:    40. MODIS: 9 savannas
! 2  MODIS_10:   41. MODIS: 10 grasslands
! 4  MODIS_11:   42. MODIS: 11 permanent wetlands
! 2  MODIS_12:   43. MODIS: 12 croplands
! 3  MODIS_13:   44. MODIS: 13 urban and built up
! 2  MODIS_14:   45. MODIS: 14 cropland / natural vegetation mosaic
! 1  MODIS_15:   46. MODIS: 15 permanent snow and ice
! 1  MODIS_16:   47. MODIS: 16 barren or sparsely vegetated
! 1  MODIS_17:   48. MODIS: 17 IGBP water
! -  MODIS_254:  49. MODIS: 254 unclassified
! -  MODIS_255:  50. MODIS: 255 fill value (normally ocean water)

C MODIS_NOAH
C     read file: GRID_CRO_2D

      integer, parameter :: n_dlcat_modis_noah = 4   ! number of desert land categories
      type( lus_type ), parameter :: vnmld_modis_noah( n_dlcat_modis_noah ) = (/
     & lus_type( 'LUFRAC_06', 'Closed Shrublands           ' ),
     & lus_type( 'LUFRAC_07', 'Open Shrublands             ' ),
     & lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated' ),
     & lus_type( 'LUFRAC_20', 'Barren Tundra               ' ) /)

      real :: dmsk_modis_noah( n_dlcat_modis_noah ) =   ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.50,    ! shrubland
     &              0.75,    ! barrenland
     &              0.75 /)  ! barrenland

      integer :: dmap_modis_noah( n_dlcat_modis_noah+1 ) =  ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              1,       ! shrubland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

      integer, parameter :: n_lucat_modis_noah = 20   ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_modis_noah( n_lucat_modis_noah ) = (/
     & lus_type( 'LUFRAC_01', 'Evergreen Needleleaf Forest       ' ),
     & lus_type( 'LUFRAC_02', 'Evergreen Broadleaf Forest        ' ),
     & lus_type( 'LUFRAC_03', 'Deciduous Needleleaf Forest       ' ),
     & lus_type( 'LUFRAC_04', 'Deciduous Broadleaf Forest        ' ),
     & lus_type( 'LUFRAC_05', 'Mixed Forests                     ' ),
     & lus_type( 'LUFRAC_06', 'Closed Shrublands                 ' ),
     & lus_type( 'LUFRAC_07', 'Open Shrublands                   ' ),
     & lus_type( 'LUFRAC_08', 'Woody Savannas                    ' ),
     & lus_type( 'LUFRAC_09', 'Savannas                          ' ),
     & lus_type( 'LUFRAC_10', 'Grasslands                        ' ),
     & lus_type( 'LUFRAC_11', 'Permanent Wetlands                ' ),
     & lus_type( 'LUFRAC_12', 'Croplands                         ' ),
     & lus_type( 'LUFRAC_13', 'Urban and Built-Up                ' ),
     & lus_type( 'LUFRAC_14', 'Cropland-Natural Vegetation Mosaic' ),
     & lus_type( 'LUFRAC_15', 'Snow and Ice                      ' ),
     & lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated      ' ),
     & lus_type( 'LUFRAC_17', 'Water                             ' ),
     & lus_type( 'LUFRAC_18', 'Wooded Tundra                     ' ),
     & lus_type( 'LUFRAC_19', 'Mixed Tundra                      ' ),
     & lus_type( 'LUFRAC_20', 'Barren Tundra                     ' ) /)

C NLCD_MODIS or NLCD50 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     read file: GRID_CRO_2D
C includes both nlcd and MODIS versions of shrublands
      integer, parameter :: n_dlcat_nlcd_modis = 7   ! number of desert land types
      type( lus_type ), parameter :: vnmld_nlcd_modis( n_dlcat_nlcd_modis ) = (/
     & lus_type( 'LUFRAC_37', 'Closed Shrublands           ' ),
     & lus_type( 'LUFRAC_38', 'Open Shrublands             ' ),
     & lus_type( 'LUFRAC_07', 'Barren Land (Rock-Sand-Clay)' ),
     & lus_type( 'LUFRAC_12', 'Dwarf Scrub                 ' ),
     & lus_type( 'LUFRAC_13', 'Shrub-Scrub                 ' ),
     & lus_type( 'LUFRAC_18', 'Tundra                      ' ),
     & lus_type( 'LUFRAC_47', 'Barren or Sparsely Vegetated' ) /)

      real :: dmsk_nlcd_modis( n_dlcat_nlcd_modis ) =   ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.50,    ! shrubland
     &              0.75,    ! barrenland
     &              0.75,    ! barrenland
     &              0.75,    ! barrenland
     &              0.75,    ! barrenland
     &              0.75 /)  ! barrenland

      integer :: dmap_nlcd_modis( n_dlcat_nlcd_modis+1 ) = ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              1,       ! shrubland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

      integer, parameter :: n_lucat_nlcd_modis = 50   ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_nlcd_modis( n_lucat_nlcd_modis ) = (/
     & lus_type( 'LUFRAC_01', 'Open Water' ),
     & lus_type( 'LUFRAC_02', 'Perennial Ice-Snow                ' ),
     & lus_type( 'LUFRAC_03', 'Developed Open Space              ' ),  ! ?
     & lus_type( 'LUFRAC_04', 'Developed Low Intensity           ' ),
     & lus_type( 'LUFRAC_05', 'Developed Medium Intensity        ' ),
     & lus_type( 'LUFRAC_06', 'Developed High Intensity          ' ),
     & lus_type( 'LUFRAC_07', 'Barren Land (Rock-Sand-Clay)      ' ),
     & lus_type( 'LUFRAC_08', 'Unconsolidated Shore              ' ),  ! ?
     & lus_type( 'LUFRAC_09', 'Deciduous Forest                  ' ),
     & lus_type( 'LUFRAC_10', 'Evergreen Forest                  ' ),
     & lus_type( 'LUFRAC_11', 'Mixed Forest                      ' ),
     & lus_type( 'LUFRAC_12', 'Dwarf Scrub                       ' ),
     & lus_type( 'LUFRAC_13', 'Shrub-Scrub                       ' ),
     & lus_type( 'LUFRAC_14', 'Grassland-Herbaceous              ' ),
     & lus_type( 'LUFRAC_15', 'Sedge-Herbaceous                  ' ),
     & lus_type( 'LUFRAC_16', 'Lichens                           ' ),
     & lus_type( 'LUFRAC_17', 'Moss                              ' ),
     & lus_type( 'LUFRAC_18', 'Tundra                            ' ),
     & lus_type( 'LUFRAC_19', 'Pasture-Hay                       ' ),
     & lus_type( 'LUFRAC_20', 'Cultivated Crops                  ' ),
     & lus_type( 'LUFRAC_21', 'Woody Wetlands                    ' ),
     & lus_type( 'LUFRAC_22', 'Palustrine Forested Wetland       ' ),
     & lus_type( 'LUFRAC_23', 'Palustrine Scrub-Shrub Wetland    ' ),  ! ?
     & lus_type( 'LUFRAC_24', 'Estuarine Forested Wetland        ' ),
     & lus_type( 'LUFRAC_25', 'Estuarine Scrub_Shrub Wetland     ' ),  ! ?
     & lus_type( 'LUFRAC_26', 'Emergent Herbaceous Wetlands      ' ),  ! ?
     & lus_type( 'LUFRAC_27', 'Palustrine Emergent Wetland       ' ),  ! ?
     & lus_type( 'LUFRAC_28', 'Estuarine Emergent Wetland        ' ),  ! ?
     & lus_type( 'LUFRAC_29', 'Palustrine Aquatic Bed            ' ),  ! ?
     & lus_type( 'LUFRAC_30', 'Estuarine Aquatic Bed             ' ),  ! ?
     & lus_type( 'LUFRAC_31', 'Water                             ' ),
     & lus_type( 'LUFRAC_32', 'Evergreen Needleleaf Forest       ' ),
     & lus_type( 'LUFRAC_33', 'Evergreen Broadleaf Forest        ' ),
     & lus_type( 'LUFRAC_34', 'Deciduous Needleleaf Forest       ' ),
     & lus_type( 'LUFRAC_35', 'Deciduous Broadleaf Forest        ' ),
     & lus_type( 'LUFRAC_36', 'Mixed Forests                     ' ),
     & lus_type( 'LUFRAC_37', 'Closed Shrublands                 ' ),
     & lus_type( 'LUFRAC_38', 'Open Shrublands                   ' ),
     & lus_type( 'LUFRAC_39', 'Woody Savannas                    ' ),
     & lus_type( 'LUFRAC_40', 'Savannas                          ' ),
     & lus_type( 'LUFRAC_41', 'Grasslands                        ' ),
     & lus_type( 'LUFRAC_42', 'Permanent Wetlands                ' ),
     & lus_type( 'LUFRAC_43', 'Croplands                         ' ),
     & lus_type( 'LUFRAC_44', 'Urban and Built Up                ' ),
     & lus_type( 'LUFRAC_45', 'Cropland-Natural Vegetation Mosaic' ),
     & lus_type( 'LUFRAC_46', 'Permanent Snow and Ice            ' ),
     & lus_type( 'LUFRAC_47', 'Barren or Sparsely Vegetated      ' ),
     & lus_type( 'LUFRAC_48', 'IGBP Water                        ' ),
     & lus_type( 'LUFRAC_49', 'unclassified                      ' ),  ! ?
     & lus_type( 'LUFRAC_50', 'fill value                        ' ) /)  ! ?

C NLCD40 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     read file: GRID_CRO_2D
C includes both nlcd and MODIS versions of shrublands
      integer, parameter :: n_dlcat_nlcd40 = 6   ! number of desert land types
      type( lus_type ), parameter :: vnmld_nlcd40( n_dlcat_nlcd40 ) = (/
     & lus_type( 'LUFRAC_06', 'Closed Shrublands           ' ),
     & lus_type( 'LUFRAC_07', 'Open Shrublands             ' ),
     & lus_type( 'LUFRAC_27', 'Barren Land (Rock-Sand-Clay)' ),
     & lus_type( 'LUFRAC_31', 'Dwarf Scrub                 ' ),
     & lus_type( 'LUFRAC_32', 'Shrub-Scrub                 ' ),
     & lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated' ) /)

      real :: dmsk_nlcd40( n_dlcat_nlcd40 ) =   ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.50,    ! shrubland
     &              0.75,    ! barrenland
     &              0.75,    ! barrenland
     &              0.75,    ! barrenland
     &              0.75 /)  ! barrenland

      integer :: dmap_nlcd40( n_dlcat_nlcd40+1 ) = ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              1,       ! shrubland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

      integer, parameter :: n_lucat_nlcd40 = 40   ! number of landuse categories
      type( lus_type ), parameter :: vnmlu_nlcd40( n_lucat_nlcd40 ) = (/
     & lus_type( 'LUFRAC_01', 'Evergreen Needleleaf Forest       ' ),
     & lus_type( 'LUFRAC_02', 'Evergreen Broadleaf Forest        ' ),
     & lus_type( 'LUFRAC_03', 'Deciduous Needleleaf Forest       ' ),
     & lus_type( 'LUFRAC_04', 'Deciduous Broadleaf Forest        ' ),
     & lus_type( 'LUFRAC_05', 'Mixed Forests                     ' ),
     & lus_type( 'LUFRAC_06', 'Closed Shrublands                 ' ),
     & lus_type( 'LUFRAC_07', 'Open Shrublands                   ' ),
     & lus_type( 'LUFRAC_08', 'Woody Savannas                    ' ),
     & lus_type( 'LUFRAC_09', 'Savannas                          ' ),
     & lus_type( 'LUFRAC_10', 'Grasslands                        ' ),
     & lus_type( 'LUFRAC_11', 'Permanent Wetlands                ' ),
     & lus_type( 'LUFRAC_12', 'Croplands                         ' ),
     & lus_type( 'LUFRAC_13', 'Urban and Built Up                ' ),
     & lus_type( 'LUFRAC_14', 'Cropland-Natural Vegetation Mosaic' ),
     & lus_type( 'LUFRAC_15', 'Permanent Snow and Ice            ' ),
     & lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated      ' ),
     & lus_type( 'LUFRAC_17', 'IGBP Water                        ' ),
     & lus_type( 'LUFRAC_18', 'unclassified                      ' ),  ! ?
     & lus_type( 'LUFRAC_19', 'fill value                        ' ),
     & lus_type( 'LUFRAC_20', 'unclassified                      ' ),  ! ?
     & lus_type( 'LUFRAC_21', 'Open Water                        ' ),
     & lus_type( 'LUFRAC_22', 'Perennial Ice-Snow                ' ),
     & lus_type( 'LUFRAC_23', 'Developed Open Space              ' ),  ! ?
     & lus_type( 'LUFRAC_24', 'Developed Low Intensity           ' ),
     & lus_type( 'LUFRAC_25', 'Developed Medium Intensity        ' ),
     & lus_type( 'LUFRAC_26', 'Developed High Intensity          ' ),
     & lus_type( 'LUFRAC_27', 'Barren Land (Rock-Sand-Clay)      ' ),
     & lus_type( 'LUFRAC_28', 'Deciduous Forest                  ' ),
     & lus_type( 'LUFRAC_29', 'Evergreen Forest                  ' ),
     & lus_type( 'LUFRAC_30', 'Mixed Forest                      ' ),
     & lus_type( 'LUFRAC_31', 'Dwarf Scrub                       ' ),
     & lus_type( 'LUFRAC_32', 'Shrub-Scrub                       ' ),
     & lus_type( 'LUFRAC_33', 'Grassland-Herbaceous              ' ),
     & lus_type( 'LUFRAC_34', 'Sedge-Herbaceous                  ' ),
     & lus_type( 'LUFRAC_35', 'Lichens                           ' ),
     & lus_type( 'LUFRAC_36', 'Moss                              ' ),
     & lus_type( 'LUFRAC_37', 'Pasture-Hay                       ' ),
     & lus_type( 'LUFRAC_38', 'Cultivated Crops                  ' ),
     & lus_type( 'LUFRAC_39', 'Woody Wetlands                    ' ),
     & lus_type( 'LUFRAC_40', 'Emergent Herbaceous Wetlands      ' )  /)  ! ?

      contains

         function lus_init( jdate, jtime ) result ( success )

         use hgrd_defn           ! horizontal domain specifications
#ifdef twoway
         use twoway_data_module, only : mminlu, num_land_cat
#endif
         use utilio_defn

         implicit none

C Includes:
         include SUBST_FILES_ID  ! file name parameters

         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         logical :: success

         integer, external :: setup_logdev

         type( lus_type ), allocatable :: vnmlu( : )
         real, allocatable :: lut( :,:,: ) ! landuse category fraction

         integer :: gxoff, gyoff             ! global origin offset from file
         integer, save :: strtcol_01, endcol_01, strtrow_01, endrow_01
         integer, save :: strtcol_02, endcol_02, strtrow_02, endrow_02

         character( 16 ) :: land_scheme
         character( 16 ) :: ctm_wbdust_beld = 'CTM_WBDUST_BELD'
         character( 16 ) :: lufile( 2 ) = ' '
         character( 16 ) :: pname = 'LUS_INIT'
         character( 128 ) :: vardesc = ' '
         character( 128 ) :: xmsg = ' '
         character( 24 ) :: strg = 'incorrect num_land_cat, '
         real( 8 ) :: tland
         logical isbeld
         integer logdev
         integer i, c, r, err

         logdev = setup_logdev()
         success = .true.

         vardesc = 'Flag for Wind Blown Dust - Land Use from BELD'
         call envstr( 'CTM_WBDUST_BELD', vardesc, 'UNKNOWN', land_scheme, err )
         if ( err .gt. 0 ) then
            write( logdev, '(5x, a, i4)' ) vardesc // ' err:', err
            xmsg = 'Environment variable not set or empty or improperly formatted'
            call m3warn ( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

         if ( land_scheme .eq. 'BELD3' ) then
            isbeld = .true.
            lufile( 1 ) = dust_lu_1 
            lufile( 2 ) = dust_lu_2    ! total forest coverage
         else if ( land_scheme .eq. 'BELD4' ) then
            isbeld = .true.
            lufile( 1 ) = beld4_lu 
!           lufile( 2 ) = beld4_lu_forest )
            lufile( 2 ) = beld4_lu 
         else
            isbeld = .false.
            lufile( 1 ) = grid_cro_2d
         end if

         if ( .not. open3( lufile( 1 ), fsread3, pname ) ) then
            xmsg = 'could not open ' // trim( lufile( 1 ) )
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if
         call subhfile ( lufile( 1 ), gxoff, gyoff,   ! Get domain window info
     &                   strtcol_01, endcol_01, strtrow_01, endrow_01 )

         if ( isbeld ) then  ! get the 2nd file (currently same as 1st for BELD4)

            if ( .not. open3( lufile( 2 ), fsread3, pname ) ) then
               xmsg = 'could not open ' // trim( lufile( 2 ) )
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            call subhfile ( lufile( 2 ), gxoff, gyoff,   ! Get domain window info
     &                      strtcol_02, endcol_02, strtrow_02, endrow_02 )

         end if

         if ( .not. isbeld ) then   ! determine land_scheme from GRID_CRO_2D

            if ( .not. desc3( lufile( 1 ) ) ) then
               xmsg = 'Could not get GRID_CRO_2D file description '
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

#ifdef twoway

C mminlu and num_land_cat are WRF global variables
            
            select case( mminlu )

               case( 'USGS24' )
                  if ( num_land_cat .ne. 24 ) then
                     write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                        ' for ' // trim( mminlu )
                     call m3warn( pname, jdate, jtime, xmsg )
                     success = .false.; return
                  end if
                  land_scheme = 'USGS24'
               case( 'NLCD40' )
                  if ( num_land_cat .ne. 40 ) then
                     write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                        ' for ' // trim( mminlu )
                     call m3warn( pname, jdate, jtime, xmsg )
                     success = .false.; return
                  end if
                  land_scheme = 'NLCD40'
               case( 'NLCD-MODIS' )
                  if ( num_land_cat .ne. 50 ) then
                     write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                        ' for ' // trim( mminlu )
                     call m3warn( pname, jdate, jtime, xmsg )
                     success = .false.; return
                  end if
                  land_scheme = 'NLCD-MODIS'
               case( 'MODIFIED_IGBP_MODIS_NOAH' )
                  if ( num_land_cat .ne. 20 ) then
                     write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                        ' for ' // trim( mminlu )
                     call m3warn( pname, jdate, jtime, xmsg )
                     success = .false.; return
                  end if
                  land_scheme = 'MODIS_NOAH'
               case( 'MODIS' )
                  if ( num_land_cat .ne. 20 ) then
                     write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                        ' for ' // trim( mminlu )
                     call m3warn( pname, jdate, jtime, xmsg )
                     success = .false.; return
                  end if
                  land_scheme = 'MODIS'
               case default
                  xmsg = 'Land use scheme not supported'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return

            end select

#else
            l_s: do i = 1, nvars3d   ! determine land scheme from GRID_CRO_2D
               if ( vname3d( i ) .eq. 'DLUSE' ) then
                  if ( index( vdesc3d( i ), 'USGS24' ) .ne. 0 ) then
                     land_scheme = 'USGS24'; exit l_s
                  else if ( index( vdesc3d( i ), 'NLCD40' ) .ne. 0 ) then
                     land_scheme = 'NLCD40'; exit l_s
                  else if ( index( vdesc3d( i ), 'NLCD50' ) .ne. 0 ) then
                     land_scheme = 'NLCD50'; exit l_s
                  else if ( index( vdesc3d( i ), 'MODIS' ) .ne. 0 ) then
                     if ( index( vdesc3d( i ), 'NLCD-MODIS' ) .ne. 0 ) then
                        land_scheme = 'NLCD-MODIS'; exit l_s
                     else if ( index( vdesc3d( i ), 'MODIS NOAH' ) .ne. 0 ) then
                        land_scheme = 'MODIS_NOAH'; exit l_s
                     else
                        land_scheme = 'MODIS'; exit l_s
                     end if
                  end if
               end if
            end do l_s
#endif

         end if   ! .not. isbeld

         select case( land_scheme )

            case( 'BELD3' )
               n_lucat = n_lucat_beld3
               n_dlcat = n_dlcat_beld3
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_beld3   ! array assignment
               vnmld = vnmld_beld3   ! array assignment
               dmsk = dmsk_beld3     ! array assignment
               dmap = dmap_beld3     ! array assignment

            case( 'BELD4' )
               n_lucat = n_lucat_beld4
               n_dlcat = n_dlcat_beld4
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_beld4   ! array assignment
               vnmld = vnmld_beld4   ! array assignment
               dmsk = dmsk_beld4     ! array assignment
               dmap = dmap_beld4     ! array assignment

            case( 'USGS24' )
               n_lucat = n_lucat_usgs24
               n_dlcat = n_dlcat_usgs24
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_usgs24   ! array assignment
               vnmld = vnmld_usgs24   ! array assignment
               dmsk = dmsk_usgs24     ! array assignment
               dmap = dmap_usgs24     ! array assignment

            case( 'MODIS' )
               n_lucat = n_lucat_modis
               n_dlcat = n_dlcat_modis
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_modis   ! array assignment
               vnmld = vnmld_modis   ! array assignment
               dmsk = dmsk_modis     ! array assignment
               dmap = dmap_modis     ! array assignment

            case( 'NLCD40' )
               n_lucat = n_lucat_nlcd40
               n_dlcat = n_dlcat_nlcd40
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_nlcd40   ! array assignment
               vnmld = vnmld_nlcd40   ! array assignment
               dmsk = dmsk_nlcd40     ! array assignment
               dmap = dmap_nlcd40     ! array assignment

            case( 'NLCD-MODIS', 'NLCD50' )
               n_lucat = n_lucat_nlcd_modis
               n_dlcat = n_dlcat_nlcd_modis
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_nlcd_modis   ! array assignment
               vnmld = vnmld_nlcd_modis   ! array assignment
               dmsk = dmsk_nlcd_modis     ! array assignment
               dmap = dmap_nlcd_modis     ! array assignment

            case( 'MODIS_NOAH' )
               n_lucat = n_lucat_modis_noah
               n_dlcat = n_dlcat_modis_noah
               allocate( vnmlu( n_lucat ),
     &                   vnmld( n_dlcat ),
     &                   dmsk( n_dlcat ),
     &                   dmap( n_dlcat+1 ), stat = err )
               if ( err .ne. 0 ) then
                  xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                  call m3warn( pname, jdate, jtime, xmsg )
                  success = .false.; return
               end if
               vnmlu = vnmlu_modis_noah   ! array assignment
               vnmld = vnmld_modis_noah   ! array assignment
               dmsk = dmsk_modis_noah     ! array assignment
               dmap = dmap_modis_noah     ! array assignment

            case default
               xmsg = 'Land use scheme not supported'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return

         end select

         write( logdev,* ) ' '
         write( logdev,* ) '    Land use scheme is ', trim( land_scheme )
         write( logdev,* ) '    n_lucat,n_dlcat: ', n_lucat, n_dlcat
         write( logdev,* ) '    desert land categories ------------------------'
         do i = 1, n_dlcat
            write( logdev,* ) '    ', trim( vnmld( i )%name ), ' ', trim( vnmld( i )%desc )
         end do
         write( logdev,* ) '    land use categories ---------------------------'
         do i = 1, n_lucat
            write( logdev,* ) '    ', trim( vnmlu( i )%name ), ' ', trim( vnmlu( i )%desc )
         end do
         write( logdev,* ) ' '

         allocate( ladut( ncols,nrows,n_dlcat ),
     &               lut( ncols,nrows,n_lucat ),
     &             uland( ncols,nrows,4 ), stat = err )
         if ( err .ne. 0 ) then
            xmsg = '*** Error allocating ladut, lut or uland'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

C Get desert land (fraction) data (assume if BELD, all desert types are in file 1)
         do i = 1, n_dlcat
            if ( .not. xtract3( lufile( 1 ), vnmld( i )%name, 1,1,
     &                          strtrow_01,endrow_01, strtcol_01,endcol_01,
     &                          jdate, jtime, ladut( 1,1,i ) ) ) then
               xmsg = 'Could not read ' // trim( vnmld( i )%name )
     &              // ' from ' // trim( lufile( 1 ) )
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
         end do

C Get landuse (fraction) data
         do i = 1, n_lucat-1
            if ( .not. xtract3( lufile( 1 ), vnmlu( i )%name, 1,1,
     &                          strtrow_01,endrow_01, strtcol_01,endcol_01,
     &                          jdate, jtime, lut( 1,1,i ) ) ) then
               xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &              // ' from ' // trim( lufile( 1 ) )
               call m3warn ( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
         end do

         i = n_lucat
         if ( .not. isbeld ) then
            if ( .not. xtract3( lufile( 1 ), vnmlu( i )%name, 1,1,
     &                          strtrow_01,endrow_01, strtcol_01,endcol_01,
     &                          jdate, jtime, lut( 1,1,i ) ) ) then
               xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &              // ' from ' // trim( lufile( 1 ) )
               call m3warn ( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
         else
            if ( .not. xtract3( lufile( 2 ), vnmlu( i )%name, 1,1,
     &                          strtrow_02,endrow_02, strtcol_02,endcol_02,
     &                          jdate, jtime, lut( 1,1,i ) ) ) then
               xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &              // ' from ' // trim( lufile( 2 ) )
               call m3warn ( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
         end if
         
         if ( .not. isbeld ) then   ! transform ladut & lut to %
            ladut = 100.0 * ladut   ! array assignment
            lut   = 100.0 * lut     ! array assignment
         end if

C Merge fractional landuse categories into 4 groups for dust transport factors
         select case( land_scheme )

            case( 'BELD3' )
            do r = 1, my_nrows
            do c = 1, my_ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,7 )   ! USGS_shrubland
     &                        + lut( c,r,14 )  ! USGS_water
     &                        + lut( c,r,16 )  ! USGS_sprsbarren
     &                        + lut( c,r,18 )  ! USGS_mxtundra
     &                        + lut( c,r,19 )  ! USGS_snowice
C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,6 )   ! USGS_grassland
     &                        + lut( c,r,8 )   ! USGS_shrubgrass
     &                        + lut( c,r,9 )   ! USGS_savanna
     &                        + lut( c,r,17 )  ! USGS_woodtundr
     &                        + lut( c,r,2 )   ! USGS_drycrop
     &                        + lut( c,r,3 )   ! USGS_irrcrop
     &                        + lut( c,r,4 )   ! USGS_cropgrass
     &                        + lut( c,r,5 )   ! USGS_cropwdlnd
C           building surrounding
               uland( c,r,3 ) = lut( c,r,1 )   ! USGS_urban
C           forest surrounding
               uland( c,r,4 ) = lut( c,r,10 )  ! USGS_decidforest
     &                        + lut( c,r,11 )  ! USGS_evbrdleaf
     &                        + lut( c,r,12 )  ! USGS_coniferfor
     &                        + lut( c,r,13 )  ! USGS_mxforest
     &                        + lut( c,r,15 )  ! USGS_wetwoods
     &                        + lut( c,r,20 )  ! FOREST  (dust_lu_2)
            end do
            end do

            case( 'BELD4' )   ! NLCD is national, MODIS is outside U.S.
            do r = 1, my_nrows
            do c = 1, my_ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,1 )   ! NLCD_11 Open Water
     &                        + lut( c,r,2 )   ! NLCD_12 Perennial Ice/Snow
     &                        + lut( c,r,7 )   ! NLCD_31 Barren Land (Rock/Sand/Clay)
     &                        + lut( c,r,12 )  ! NLCD_51 Dwarf Scrub
     &                        + lut( c,r,13 )  ! NLCD_52 Shrub/Scrub
     &                        + lut( c,r,18 )  ! NLCD_75 Tundra
     &                        + lut( c,r,31 )  ! MODIS_0 water
     &                        + lut( c,r,46 )  ! MODIS_15 permanent snow and ice
     &                        + lut( c,r,47 )  ! MODIS_16 barren or sparsely vegetated
     &                        + lut( c,r,48 )  ! MODIS_17 IGBP water

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,14 )  ! NLCD_71 Grassland/Herbaceous
     &                        + lut( c,r,15 )  ! NLCD_72 Sedge/Herbaceous
     &                        + lut( c,r,16 )  ! NLCD_73 Lichens
     &                        + lut( c,r,17 )  ! NLCD_74 Moss
     &                        + lut( c,r,19 )  ! NLCD_81 Pasture/Hay
     &                        + lut( c,r,20 )  ! NLCD_82 Cultivated Crops
     &                        + lut( c,r,37 )  ! MODIS_6 closed shrublands
     &                        + lut( c,r,38 )  ! MODIS_7 open shrublands
     &                        + lut( c,r,40 )  ! MODIS_9 savannas
     &                        + lut( c,r,41 )  ! MODIS_10 grasslands
     &                        + lut( c,r,43 )  ! MODIS_12 croplands
     &                        + lut( c,r,45 )  ! MODIS_14 cropland / natural vegetation mosaic

C           building surrounding
               uland( c,r,3 ) = lut( c,r,4 )   ! NLCD_22 Developed Low Intensity
     &                        + lut( c,r,5 )   ! NLCD_23 Developed Medium Intensity
     &                        + lut( c,r,6 )   ! NLCD_24 Developed High Intensity
     &                        + lut( c,r,44 )  ! MODIS_13 urban and built up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,9 )   ! NLCD_41 Deciduous Forest
     &                        + lut( c,r,10 )  ! NLCD_42 Evergreen Forest
     &                        + lut( c,r,11 )  ! NLCD_43 Mixed Forest
     &                        + lut( c,r,21 )  ! NLCD_90 Woody Wetlands
     &                        + lut( c,r,22 )  ! NLCD_91 Palustrine Forested Wetland
     &                        + lut( c,r,24 )  ! NLCD_93 Estuarine Forested Wetland
     &                        + lut( c,r,32 )  ! MODIS_1 evergreen needleleaf forest
     &                        + lut( c,r,33 )  ! MODIS_2 evergreen broadleaf forest
     &                        + lut( c,r,34 )  ! MODIS_3 deciduous needleleaf forest
     &                        + lut( c,r,35 )  ! MODIS_4 deciduous broadleaf forest
     &                        + lut( c,r,36 )  ! MODIS_5 mixed forests
     &                        + lut( c,r,39 )  ! MODIS_8 woody savannas
     &                        + lut( c,r,42 )  ! MODIS_11 permanent wetlands

C Not used:
C               3  NLCD_21 Developed Open Space
C               8  NLCD_32 Unconsolidated Shore
C              23  NLCD_92 Palustrine Scrub/Shrub Wetland
C              25  NLCD_94 Estuarine Scrub/Shrub Wetland
C              26  NLCD_95 Emergent Herbaceous Wetlands
C              27  NLCD_96 Palustrine Emergent Wetland
C              28  NLCD_97 Estuarine Emergent Wetland
C              29  NLCD_98 Palustrine Aquatic Bed
C              30  NLCD_99 Estuarine Aquatic Bed
C              49  MODIS_254 unclassified
C              50  MODIS_255 fill value (normally ocean water)

            end do
            end do

            case( 'USGS24' )
            do r = 1, my_nrows
            do c = 1, my_ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,8 )   ! Shrubland
     &                        + lut( c,r,16 )  ! Water
     &                        + lut( c,r,19 )  ! Barren or Sparsely Vegetated
     &                        + lut( c,r,22 )  ! Mixed Tundra
     &                        + lut( c,r,23 )  ! Bare Ground Tundra
     &                        + lut( c,r,24 )  ! Snow and Ice

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,2 )   ! Dryland Cropland and Pasture
     &                        + lut( c,r,3 )   ! Irrigated Cropland and Pasture
     &                        + lut( c,r,4 )   ! Mixed Dryland and Irrigated Cropland and Pasture
     &                        + lut( c,r,5 )   ! Cropland-Grassland Mosaic
     &                        + lut( c,r,6 )   ! Cropland-Woodland Mosaic
     &                        + lut( c,r,7 )   ! Grassland
     &                        + lut( c,r,9 )   ! Mixed Shrubland-Grassland
     &                        + lut( c,r,10 )  ! Savannah
     &                        + lut( c,r,17 )  ! Herbaceous Wetland
     &                        + lut( c,r,20 )  ! Herbaceous Tundra

C           building surrounding
               uland( c,r,3 ) = lut( c,r,1 )   ! Urban Land

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,11 )  ! Deciduous Broadleaf Forest
     &                        + lut( c,r,12 )  ! Deciduous Needleleaf Forest
     &                        + lut( c,r,13 )  ! Evergreen Broadleaf Forest
     &                        + lut( c,r,14 )  ! Evergreen Needleleaf Forest
     &                        + lut( c,r,15 )  ! Mixed Forest
     &                        + lut( c,r,18 )  ! Wooded Wetland
     &                        + lut( c,r,21 )  ! Wooded Tundra
            end do
            end do

!           case( 'MODIS' )
!           do r = 1, my_nrows
!           do c = 1, my_ncols
!              uland( c,r,1 ) = lut( c,r,> )
!           end do
!           end do

            case( 'NLCD40' )
            do r = 1, my_nrows
            do c = 1, my_ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,21 )   ! Open Water
     &                        + lut( c,r,22 )   ! Perennial Ice-Snow
     &                        + lut( c,r,27 )   ! Barren Land (Rock-Sand-Clay)
     &                        + lut( c,r,31 )   ! Dwarf Scrub
     &                        + lut( c,r,32 )   ! Shrub-Scrub
     &                        + lut( c,r,15 )   ! Permanent Snow and Ice
     &                        + lut( c,r,16 )   ! Barren or Sparsely Vegetated
     &                        + lut( c,r,17 )   ! IGBP Water

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,33 )  ! Grassland-Herbaceous
     &                        + lut( c,r,34 )  ! Sedge-Herbaceous
     &                        + lut( c,r,35 )  ! Lichens
     &                        + lut( c,r,36 )  ! Moss
     &                        + lut( c,r,37 )  ! Pasture-Hay
     &                        + lut( c,r,38 )  ! Cultivated Crops
     &                        + lut( c,r,6  )  ! Closed Shrublands
     &                        + lut( c,r,7  )  ! Open Shrublands
     &                        + lut( c,r,9  )  ! Savannas
     &                        + lut( c,r,10 )  ! Grasslands
     &                        + lut( c,r,12 )  ! Croplands
     &                        + lut( c,r,14 )  ! Cropland-Natural Vegetation Mosaic

C           building surrounding
               uland( c,r,3 ) = lut( c,r,24 )   ! Developed Low Intensity
     &                        + lut( c,r,25 )   ! Developed Medium Intensity
     &                        + lut( c,r,26 )   ! Developed High Intensity
     &                        + lut( c,r,13 )   ! Urban and Built Up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,28 )  ! Deciduous Forest
     &                        + lut( c,r,29 )  ! Evergreen Forest
     &                        + lut( c,r,30 )  ! Mixed Forest
     &                        + lut( c,r,39 )  ! Woody Wetlands
     &                        + lut( c,r,40 )  ! Emergent Herbaceous Wetlands
     &                        + lut( c,r,1  )  ! Evergreen Needleleaf Forest
     &                        + lut( c,r,2  )  ! Evergreen Broadleaf Forest
     &                        + lut( c,r,3  )  ! Deciduous Needleleaf Forest
     &                        + lut( c,r,4  )  ! Deciduous Broadleaf Forest
     &                        + lut( c,r,5  )  ! Mixed Forests
     &                        + lut( c,r,8  )  ! Woody Savannas
     &                        + lut( c,r,11 )  ! Permanent Wetlands
            end do
            end do

            case( 'NLCD-MODIS', 'NLCD50' )
            do r = 1, my_nrows
            do c = 1, my_ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,1 )   ! Open Water
     &                        + lut( c,r,2 )   ! Perennial Ice-Snow
     &                        + lut( c,r,7 )   ! Barren Land (Rock-Sand-Clay)
     &                        + lut( c,r,12 )  ! Dwarf Scrub
     &                        + lut( c,r,13 )  ! Shrub-Scrub
     &                        + lut( c,r,18 )  ! Tundra
     &                        + lut( c,r,31 )  ! Water
     &                        + lut( c,r,46 )  ! Permanent Snow and Ice
     &                        + lut( c,r,47 )  ! Barren or Sparsely Vegetated
     &                        + lut( c,r,48 )  ! IGBP Water

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,14 )  ! Grassland-Herbaceous
     &                        + lut( c,r,15 )  ! Sedge-Herbaceous
     &                        + lut( c,r,16 )  ! Lichens
     &                        + lut( c,r,17 )  ! Moss
     &                        + lut( c,r,19 )  ! Pasture-Hay
     &                        + lut( c,r,20 )  ! Cultivated Crops
     &                        + lut( c,r,37 )  ! Closed Shrublands
     &                        + lut( c,r,38 )  ! Open Shrublands
     &                        + lut( c,r,40 )  ! Savannas
     &                        + lut( c,r,41 )  ! Grasslands
     &                        + lut( c,r,43 )  ! Croplands
     &                        + lut( c,r,45 )  ! Cropland-Natural Vegetation Mosaic

C           building surrounding
               uland( c,r,3 ) = lut( c,r,4 )   ! Developed Low Intensity
     &                        + lut( c,r,5 )   ! Developed Medium Intensity
     &                        + lut( c,r,6 )   ! Developed High Intensity
     &                        + lut( c,r,44 )  ! Urban and Built Up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,9 )   ! Deciduous Forest
     &                        + lut( c,r,10 )  ! Evergreen Forest
     &                        + lut( c,r,11 )  ! Mixed Forest
     &                        + lut( c,r,21 )  ! Woody Wetlands
     &                        + lut( c,r,22 )  ! Palustrine Forested Wetland
     &                        + lut( c,r,24 )  ! Estuarine Forested Wetland
     &                        + lut( c,r,32 )  ! Evergreen Needleleaf Forest
     &                        + lut( c,r,33 )  ! Evergreen Broadleaf Forest
     &                        + lut( c,r,34 )  ! Deciduous Needleleaf Forest
     &                        + lut( c,r,35 )  ! Deciduous Broadleaf Forest
     &                        + lut( c,r,36 )  ! Mixed Forests
     &                        + lut( c,r,39 )  ! Woody Savannas
     &                        + lut( c,r,42 )  ! Permanent Wetlands
            end do
            end do

            case( 'MODIS_NOAH' )
            do r = 1, my_nrows
            do c = 1, my_ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,6 )   ! Closed Shrublands
     &                        + lut( c,r,7 )   ! Open Shrublands
     &                        + lut( c,r,15 )  ! Snow and Ice
     &                        + lut( c,r,16 )  ! Barren or Sparsely Vegetated
     &                        + lut( c,r,17 )  ! Water
     &                        + lut( c,r,19 )  ! Mixed Tundra
     &                        + lut( c,r,20 )  ! Barren Tundra

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,8 )   ! Woody Savannas
     &                        + lut( c,r,9 )   ! Savannas
     &                        + lut( c,r,10 )  ! Grasslands
     &                        + lut( c,r,12 )  ! Croplands
     &                        + lut( c,r,14 )  ! Cropland-Natural Vegetation Mosaic
     &                        + lut( c,r,18 )  ! Wooded Tundra

C           building surrounding
               uland( c,r,3 ) = lut( c,r,13 )  ! Urban and Built-Up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,1 )   ! Evergreen Needleleaf Forest
     &                        + lut( c,r,2 )   ! Evergreen Broadleaf Forest
     &                        + lut( c,r,3 )   ! Deciduous Needleleaf Forest
     &                        + lut( c,r,4 )   ! Deciduous Broadleaf Forest
     &                        + lut( c,r,5 )   ! Mixed Forests
     &                        + lut( c,r,11 )  ! Permanent Wetlands
            end do
            end do

         end select

         do r = 1, my_nrows
         do c = 1, my_ncols

            tland = 0.0d0
            do i = 1, 4
               uland( c,r,i ) = max( 0.0, min( 100.0, uland( c,r,i ) ) )
               tland = tland + real( uland( c,r,i ), 8 )
            end do

            if ( tland .gt. 100.0d0 ) then
#ifdef dbg_wbdust
               write( logdev,* ) ' >>> tland:', c, r, tland
#endif
               do i = 1, 4
                  uland( c,r,i ) = uland( c,r,i ) * real( 100.0d0 / tland, 4 )
               end do
            end if

         end do
         end do

         deallocate( lut )

         return
         end function lus_init

      end module lus_defn
