
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

!------------------------------------------------------------------------!
! This module contains key component from various modules and packaged
! in the following modules:
!
!         module beld_data_module
!         module crop_data_module
!         module lus_data_module
!         module stack_group_data_module
!         module biog_emis_param_module
!         module em_param_module
!
! to facilitate centralized I/O implementation by avoiding cyclic data
! dependence

! Revision History:
!  03 Oct 2018 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module beld_data_module

            implicit none

            integer, parameter :: e2c_cats = 42   ! number of crop catigories

! Some compilers do not pad with blank characters in data initialization
!              '12345678901234567890'
            character( 20 ), parameter :: Beld_Names( e2c_cats ) =
     &        (/ 'Hay                 ',  !  1
     &           'Hay_ir              ',  !  2
     &           'Alfalfa             ',  !  3
     &           'Alfalfa_ir          ',  !  4
     &           'Other_Grass         ',  !  5
     &           'Other_Grass_ir      ',  !  6
     &           'Barley              ',  !  7
     &           'Barley_ir           ',  !  8
     &           'BeansEdible         ',  !  9
     &           'BeansEdible_ir      ',  ! 10
     &           'CornGrain           ',  ! 11
     &           'CornGrain_ir        ',  ! 12
     &           'CornSilage          ',  ! 13
     &           'CornSilage_ir       ',  ! 14
     &           'Cotton              ',  ! 15
     &           'Cotton_ir           ',  ! 16
     &           'Oats                ',  ! 17
     &           'Oats_ir             ',  ! 18
     &           'Peanuts             ',  ! 19
     &           'Peanuts_ir          ',  ! 20
     &           'Potatoes            ',  ! 21
     &           'Potatoes_ir         ',  ! 22
     &           'Rice                ',  ! 23
     &           'Rice_ir             ',  ! 24
     &           'Rye                 ',  ! 25
     &           'Rye_ir              ',  ! 26
     &           'SorghumGrain        ',  ! 27
     &           'SorghumGrain_ir     ',  ! 28
     &           'SorghumSilage       ',  ! 29
     &           'SorghumSilage_ir    ',  ! 30
     &           'Soybeans            ',  ! 31
     &           'Soybeans_ir         ',  ! 32
     &           'Wheat_Spring        ',  ! 33
     &           'Wheat_Spring_ir     ',  ! 34
     &           'Wheat_Winter        ',  ! 35
     &           'Wheat_Winter_ir     ',  ! 36
     &           'Other_Crop          ',  ! 37
     &           'Other_Crop_ir       ',  ! 38
     &           'Canola              ',  ! 39
     &           'Canola_ir           ',  ! 40
     &           'Beans               ',  ! 41
     &           'Beans_ir            '   ! 42
     &           /)

            real, allocatable :: Beld_ag ( :,:,: )
!   for E2C_SOIL file
            real, allocatable :: pHs1    ( :,:,: )
            real, allocatable :: pHs2    ( :,:,: )
            real, allocatable :: por1    ( :,:,: )
            real, allocatable :: por2    ( :,:,: )
            real, allocatable :: wp1     ( :,:,: )
            real, allocatable :: wp2     ( :,:,: )
            real, allocatable :: cec1    ( :,:,: )
            real, allocatable :: cec2    ( :,:,: )
!   for E2C_FERT file
            real, Allocatable :: NH4ps1  ( :,:,: ) ! Total Soil NH4 from EPIC
            real, Allocatable :: NH4ps2  ( :,:,: )
            real, Allocatable :: wep1    ( :,:,: )
            real, Allocatable :: wep2    ( :,:,: )
            real, Allocatable :: dep2    ( :,:,: )

          end module beld_data_module

! ------------------------------------------------------------------------
          module crop_data_module

            implicit none

            integer, parameter :: nlcrp = 17
            character( 16 ), parameter :: crop_vnmld( nlcrp ) =  ! beld01
     &         ( / 'Alfalfa  ', 'Barley   ', 'Corn     ',
     &             'Cotton   ', 'Grass    ', 'Hay      ',
     &             'Misc_crop', 'Oats     ', 'Pasture  ',
     &             'Peanuts  ', 'Potatoes ', 'Rice     ',
     &             'Rye      ', 'Sorghum  ', 'Soybeans ',
     &             'Tobacco  ', 'Wheat    ' / )

C Conventional tillage (0-15% residue) fraction for 17 crops as VNMLD list
            real, parameter :: ctilf( nlcrp ) =
     &         ( / 0.0,  0.37, 0.40, 0.70,  ! Alfalfa, Barley, Corn, Cotton,
     &             0.0,  0.0,  0.70, 0.47,  ! Grass, Hay, Misc_crop, Oats,
     &             0.36, 0.80, 0.74, 0.79,  ! Pasture, Peanuts, Potatoes, Rice,
     &             0.52, 0.46, 0.24, 0.96,  ! Rye, Sorghum, Soybeans, Tobacco, 
     &             0.40 / )                 ! Wheat

C No tillage fraction for 17 crops as VNMLD list
            real, parameter :: ntilf( nlcrp ) =
     &         ( / 0.80, 0.10, 0.20, 0.14,  ! Alfalfa, Barley, Corn, Cotton,
     &             0.80, 0.80, 0.05, 0.08,  ! Grass, Hay, Misc_crop, Oats,
     &             0.47, 0.09, 0.0,  0.05,  ! Pasture, Peanuts, Potatoes, Rice,
     &             0.11, 0.14, 0.33, 0.01,  ! Rye, Sorghum, Soybeans, Tobacco, 
     &             0.13 / )                 ! Wheat

            real, allocatable :: crop_ladut ( :,:,: )   ! cropland fraction from BELD1
            real, allocatable :: crplnd( :,:,: )        ! each erodible crop land fraction

            integer, parameter :: ncrop = 18
            character( 16 ), parameter :: vcrop( ncrop ) =  ! crop calendar )CROPCAL)
     &         ( / 'Alfalfa     ', 'BarleySpring', 'BarleyFall  ',
     &             'Corn        ', 'Cotton      ', 'Hay         ',
     &             'OatsSpring  ', 'OatsFall    ', 'Peanuts     ',
     &             'Potatoes    ', 'Rice        ', 'Rye         ',
     &             'Sorghum     ', 'Soybeans    ', 'Sugerbeets  ',
     &             'Tobacco     ', 'WheatSpring ', 'WheatWinter ' / )

            character( 16 ), parameter :: crname( 3 ) = ! crop calendar NetCDF file name
     &         ( / 'CROPMAP01',        ! begin planting
     &             'CROPMAP04',        ! end planting
     &             'CROPMAP08' / )     ! end harvesting

            real, allocatable :: cropdt( :,:,:,: ) ! cropland calendar for species and 3 activities

          end module crop_data_module

! ------------------------------------------------------------------------
          module lus_data_module

            implicit none

            type lus_type
               character( 16 ) :: name
               character( 64 ) :: desc
            end type lus_type

            type( lus_type ), allocatable :: vnmld( : )
            type( lus_type ), allocatable :: vnmlu( : )

            real, allocatable    :: ladut( :,:,: ) ! % of desertland
            real, allocatable    :: lut( :,:,: )   ! landuse category fraction
            real, allocatable    :: dmsk( : )      ! desert fraction mask
            real, allocatable    :: uland( :,:,: ) ! % of 4 land categories groups
            integer, allocatable :: dmap( : )      ! desert land map to BELD3

            character( 16 ) :: lufile( 2 ) = ' '
            logical         :: isbeld
            integer         :: n_dlcat        ! number of desert land categories
            integer         :: n_lucat        ! number of landuse categories

! BELD - Biogenic Emission Landcover Database
! USGS - United States Geological Survey
! NOAH - A community land-surface model [N=NCEP, O=Oregon State Univ.,
!        A=Air Force, H=Hydrologic Research Lab - NWS(now OHD: Office of
!        Hydrologic Devel.)]
! MODIS - MODerate resolution Imaging Spectroradiometer
! NLCD - National Land Cover Dataset
! IGBP - International Geosphere-Biosphere Programme

! BELD3 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     read files: DUST_LU_1 & DUST_LU_2

            integer, parameter :: n_dlcat_beld3 = 3   ! number of desert land categories
            type( lus_type ), parameter :: vnmld_beld3( n_dlcat_beld3 ) = (/
     &            lus_type( 'USGS_shrubland ', 'shrub_land    ' ),
     &            lus_type( 'USGS_shrubgrass', 'shrub_grass   ' ),
     &            lus_type( 'USGS_sprsbarren', 'sparse_barren ' ) /)

            real :: dmsk_beld3( n_dlcat_beld3 ) = ! land use type desert fraction
     &               (/ 0.50,    ! shrubland
     &                  0.25,    ! shrubgrass
     &                  0.75 /)  ! barrenland

            integer :: dmap_beld3( n_dlcat_beld3+1 ) = ! land use type desert map to BELD3
     &           (/ 1,       ! shrubland
     &              2,       ! shrubgrass
     &              3,       ! barrenland
     &              3 /)     ! ag landuse surrogate

            integer, parameter :: n_lucat_beld3 = 20   ! number of landuse categories
            type( lus_type ), parameter :: vnmlu_beld3( n_lucat_beld3 ) = (/
     &            lus_type( 'USGS_urban      ', 'urban               ' ),
     &            lus_type( 'USGS_drycrop    ', 'non_irrigated crops ' ),
     &            lus_type( 'USGS_irrcrop    ', 'irrigated crops     ' ),
     &            lus_type( 'USGS_cropgrass  ', 'crop_grass          ' ),
     &            lus_type( 'USGS_cropwdlnd  ', 'crop_wood land      ' ),
     &            lus_type( 'USGS_grassland  ', 'grass land          ' ),
     &            lus_type( 'USGS_shrubland  ', 'shrub land          ' ),
     &            lus_type( 'USGS_shrubgrass ', 'shrub_grass         ' ),
     &            lus_type( 'USGS_savanna    ', 'savanna             ' ),
     &            lus_type( 'USGS_decidforest', 'deciduous forest    ' ),
     &            lus_type( 'USGS_evbrdleaf  ', 'evergreen_broad_leaf' ),
     &            lus_type( 'USGS_coniferfor ', 'coniferous forest   ' ),
     &            lus_type( 'USGS_mxforest   ', 'mixed forest        ' ),
     &            lus_type( 'USGS_water      ', 'water               ' ),
     &            lus_type( 'USGS_wetwoods   ', 'wet_woods           ' ),
     &            lus_type( 'USGS_sprsbarren ', 'sparse_barren       ' ),
     &            lus_type( 'USGS_woodtundr  ', 'wood_tundra         ' ),
     &            lus_type( 'USGS_mxtundra   ', 'mixed_tundra        ' ),
     &            lus_type( 'USGS_snowice    ', 'snow_ice            ' ),
     &            lus_type( 'FOREST          ', 'total forest        ' ) /)  ! DUST_LU_2

! BELD4 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     read file: BELD4_LU

            integer, parameter :: n_dlcat_beld4 = 6   ! number of desert land categories
            type( lus_type ), parameter :: vnmld_beld4( n_dlcat_beld4 ) = (/
     &            lus_type( 'NLCD_52 ', 'Shrub/Scrub                 ' ),  ! shrubland?
     &            lus_type( 'MODIS_6 ', 'closed shrublands           ' ),
     &            lus_type( 'MODIS_7 ', 'open shrublands             ' ),
     &            lus_type( 'NLCD_51 ', 'Dwarf Scrub                 ' ),  ! shrubgrass?
     &            lus_type( 'NLCD_31 ', 'Barren Land (Rock/Sand/Clay)' ),
     &            lus_type( 'MODIS_16', 'barren or sparsely vegetated' ) /)

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
     &            lus_type( 'NLCD_11  ', 'Open Water                                  '),   !  1
     &            lus_type( 'NLCD_12  ', 'Perennial Ice/Snow                          '),   !  2
     &            lus_type( 'NLCD_21  ', 'Developed Open Space (not used)             '),   !  3 ?
     &            lus_type( 'NLCD_22  ', 'Developed Low Intensity                     '),   !  4
     &            lus_type( 'NLCD_23  ', 'Developed Medium Intensity                  '),   !  5
     &            lus_type( 'NLCD_24  ', 'Developed High Intensity                    '),   !  6
     &            lus_type( 'NLCD_31  ', 'Barren Land (Rock/Sand/Clay)                '),   !  7
     &            lus_type( 'NLCD_32  ', 'Unconsolidated Shore (not used)             '),   !  8 ?
     &            lus_type( 'NLCD_41  ', 'Deciduous Forest                            '),   !  9
     &            lus_type( 'NLCD_42  ', 'Evergreen Forest                            '),   ! 10
     &            lus_type( 'NLCD_43  ', 'Mixed Forest                                '),   ! 11
     &            lus_type( 'NLCD_51  ', 'Dwarf Scrub                                 '),   ! 12
     &            lus_type( 'NLCD_52  ', 'Shrub/Scrub                                 '),   ! 13
     &            lus_type( 'NLCD_71  ', 'Grassland/Herbaceous                        '),   ! 14
     &            lus_type( 'NLCD_72  ', 'Sedge/Herbaceous                            '),   ! 15
     &            lus_type( 'NLCD_73  ', 'Lichens                                     '),   ! 16
     &            lus_type( 'NLCD_74  ', 'Moss                                        '),   ! 17
     &            lus_type( 'NLCD_75  ', 'Tundra                                      '),   ! 18
     &            lus_type( 'NLCD_81  ', 'Pasture/Hay                                 '),   ! 19
     &            lus_type( 'NLCD_82  ', 'Cultivated Crops                            '),   ! 20
     &            lus_type( 'NLCD_90  ', 'Woody Wetlands                              '),   ! 21
     &            lus_type( 'NLCD_91  ', 'Palustrine Forested Wetland                 '),   ! 22
     &            lus_type( 'NLCD_92  ', 'Palustrine Scrub/Shrub Wetland (not used)   '),   ! 23 ?
     &            lus_type( 'NLCD_93  ', 'Estuarine Forested Wetland                  '),   ! 24
     &            lus_type( 'NLCD_94  ', 'Estuarine Scrub_Shrub Wetland (not used)    '),   ! 25 ?
     &            lus_type( 'NLCD_95  ', 'Emergent Herbaceous Wetlands (not used)     '),   ! 26 ?
     &            lus_type( 'NLCD_96  ', 'Palustrine Emergent Wetland (not used)      '),   ! 27 ?
     &            lus_type( 'NLCD_97  ', 'Estuarine Emergent Wetland (not used)       '),   ! 28 ?
     &            lus_type( 'NLCD_98  ', 'Palustrine Aquatic Bed (not used)           '),   ! 29 ?
     &            lus_type( 'NLCD_99  ', 'Estuarine Aquatic Bed  (not used)           '),   ! 30 ?
     &            lus_type( 'MODIS_0  ', 'water                                       '),   ! 31
     &            lus_type( 'MODIS_1  ', 'evergreen needleleaf forest                 '),   ! 32
     &            lus_type( 'MODIS_2  ', 'evergreen broadleaf forest                  '),   ! 33
     &            lus_type( 'MODIS_3  ', 'deciduous needleleaf forest                 '),   ! 34
     &            lus_type( 'MODIS_4  ', 'deciduous broadleaf forest                  '),   ! 35
     &            lus_type( 'MODIS_5  ', 'mixed forests                               '),   ! 36
     &            lus_type( 'MODIS_6  ', 'closed shrublands                           '),   ! 37
     &            lus_type( 'MODIS_7  ', 'open shrublands                             '),   ! 38
     &            lus_type( 'MODIS_8  ', 'woody savannas                              '),   ! 39
     &            lus_type( 'MODIS_9  ', 'savannas                                    '),   ! 40
     &            lus_type( 'MODIS_10 ', 'grasslands                                  '),   ! 41
     &            lus_type( 'MODIS_11 ', 'permanent wetlands                          '),   ! 42
     &            lus_type( 'MODIS_12 ', 'croplands                                   '),   ! 43
     &            lus_type( 'MODIS_13 ', 'urban and built up                          '),   ! 44
     &            lus_type( 'MODIS_14 ', 'cropland / natural vegetation mosaic        '),   ! 45
     &            lus_type( 'MODIS_15 ', 'permanent snow and ice                      '),   ! 46
     &            lus_type( 'MODIS_16 ', 'barren or sparsely vegetated                '),   ! 47
     &            lus_type( 'MODIS_17 ', 'IGBP water                                  '),   ! 48
     &            lus_type( 'MODIS_254', 'unclassified (not used)                     '),   ! 49 ?
     &            lus_type( 'MODIS_255', 'fill value (normally ocean water) (not used)') /) ! 50 ?

! USGS24 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     read file: GRID_CRO_2D

            integer, parameter :: n_dlcat_usgs24 = 4   ! number of desert land categories
            type( lus_type ), parameter :: vnmld_usgs24( n_dlcat_usgs24 ) = (/
     &            lus_type( 'LUFRAC_08', 'Shrubland                   ' ),
     &            lus_type( 'LUFRAC_19', 'Barren or Sparsely Vegetated' ),
     &            lus_type( 'LUFRAC_22', 'Mixed Tundra                ' ),
     &            lus_type( 'LUFRAC_23', 'Bare Ground Tundra          ' ) /)

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
     &            lus_type( 'LUFRAC_01', 'Urban Land                                      ' ),
     &            lus_type( 'LUFRAC_02', 'Dryland Cropland and Pasture                    ' ),
     &            lus_type( 'LUFRAC_03', 'Irrigated Cropland and Pasture                  ' ),
     &            lus_type( 'LUFRAC_04', 'Mixed Dryland and Irrigated Cropland and Pasture' ),
     &            lus_type( 'LUFRAC_05', 'Cropland-Grassland Mosaic                       ' ),
     &            lus_type( 'LUFRAC_06', 'Cropland-Woodland Mosaic                        ' ),
     &            lus_type( 'LUFRAC_07', 'Grassland                                       ' ),
     &            lus_type( 'LUFRAC_08', 'Shrubland                                       ' ),
     &            lus_type( 'LUFRAC_09', 'Mixed Shrubland-Grassland                       ' ),
     &            lus_type( 'LUFRAC_10', 'Savannah                                        ' ),
     &            lus_type( 'LUFRAC_11', 'Deciduous Broadleaf Forest                      ' ),
     &            lus_type( 'LUFRAC_12', 'Deciduous Needleleaf Forest                     ' ),
     &            lus_type( 'LUFRAC_13', 'Evergreen Broadleaf Forest                      ' ),
     &            lus_type( 'LUFRAC_14', 'Evergreen Needleleaf Forest                     ' ),
     &            lus_type( 'LUFRAC_15', 'Mixed Forest                                    ' ),
     &            lus_type( 'LUFRAC_16', 'Water                                           ' ),
     &            lus_type( 'LUFRAC_17', 'Herbaceous Wetland                              ' ),
     &            lus_type( 'LUFRAC_18', 'Wooded Wetland                                  ' ),
     &            lus_type( 'LUFRAC_19', 'Barren or Sparsely Vegetated                    ' ),
     &            lus_type( 'LUFRAC_20', 'Herbaceous Tundra                               ' ),
     &            lus_type( 'LUFRAC_21', 'Wooded Tundra                                   ' ),
     &            lus_type( 'LUFRAC_22', 'Mixed Tundra                                    ' ),
     &            lus_type( 'LUFRAC_23', 'Bare Ground Tundra                              ' ),
     &            lus_type( 'LUFRAC_24', 'Snow or Ice                                     ' ) /)

! MODIS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     read file: GRID_CRO_2D

            integer, parameter :: n_dlcat_modis = 1   ! number of desert land categories
            type( lus_type ), parameter :: vnmld_modis( n_dlcat_modis ) = (/
     &            lus_type( 'nada', 'nada' ) /)
            real :: dmsk_modis( n_dlcat_modis ) = 0.0  ! land use type desert fraction
            integer :: dmap_modis( n_dlcat_modis+1 ) = 0 ! land use type desert map to BELD3
            integer, parameter :: n_lucat_modis = 1   ! number of landuse categories
            type( lus_type ), parameter :: vnmlu_modis( n_lucat_modis ) = (/
     &            lus_type( 'nada', 'nada' ) /)

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

! MODIS_NOAH
!     read file: GRID_CRO_2D

            integer, parameter :: n_dlcat_modis_noah = 4   ! number of desert land categories
            type( lus_type ), parameter :: vnmld_modis_noah( n_dlcat_modis_noah ) = (/
     &            lus_type( 'LUFRAC_06', 'Closed Shrublands           ' ),
     &            lus_type( 'LUFRAC_07', 'Open Shrublands             ' ),
     &            lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated' ),
     &            lus_type( 'LUFRAC_20', 'Barren Tundra               ' ) /)

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
     &            lus_type( 'LUFRAC_01', 'Evergreen Needleleaf Forest       ' ),
     &            lus_type( 'LUFRAC_02', 'Evergreen Broadleaf Forest        ' ),
     &            lus_type( 'LUFRAC_03', 'Deciduous Needleleaf Forest       ' ),
     &            lus_type( 'LUFRAC_04', 'Deciduous Broadleaf Forest        ' ),
     &            lus_type( 'LUFRAC_05', 'Mixed Forests                     ' ),
     &            lus_type( 'LUFRAC_06', 'Closed Shrublands                 ' ),
     &            lus_type( 'LUFRAC_07', 'Open Shrublands                   ' ),
     &            lus_type( 'LUFRAC_08', 'Woody Savannas                    ' ),
     &            lus_type( 'LUFRAC_09', 'Savannas                          ' ),
     &            lus_type( 'LUFRAC_10', 'Grasslands                        ' ),
     &            lus_type( 'LUFRAC_11', 'Permanent Wetlands                ' ),
     &            lus_type( 'LUFRAC_12', 'Croplands                         ' ),
     &            lus_type( 'LUFRAC_13', 'Urban and Built-Up                ' ),
     &            lus_type( 'LUFRAC_14', 'Cropland-Natural Vegetation Mosaic' ),
     &            lus_type( 'LUFRAC_15', 'Snow and Ice                      ' ),
     &            lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated      ' ),
     &            lus_type( 'LUFRAC_17', 'Water                             ' ),
     &            lus_type( 'LUFRAC_18', 'Wooded Tundra                     ' ),
     &            lus_type( 'LUFRAC_19', 'Mixed Tundra                      ' ),
     &            lus_type( 'LUFRAC_20', 'Barren Tundra                     ' ) /)

! NLCD_MODIS or NLCD50 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     read file: GRID_CRO_2D
! includes both nlcd and MODIS versions of shrublands
            integer, parameter :: n_dlcat_nlcd_modis = 7   ! number of desert land types
            type( lus_type ), parameter :: vnmld_nlcd_modis( n_dlcat_nlcd_modis ) = (/
     &            lus_type( 'LUFRAC_37', 'Closed Shrublands           ' ),
     &            lus_type( 'LUFRAC_38', 'Open Shrublands             ' ),
     &            lus_type( 'LUFRAC_07', 'Barren Land (Rock-Sand-Clay)' ),
     &            lus_type( 'LUFRAC_12', 'Dwarf Scrub                 ' ),
     &            lus_type( 'LUFRAC_13', 'Shrub-Scrub                 ' ),
     &            lus_type( 'LUFRAC_18', 'Tundra                      ' ),
     &            lus_type( 'LUFRAC_47', 'Barren or Sparsely Vegetated' ) /)

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
     &            lus_type( 'LUFRAC_01', 'Open Water' ),
     &            lus_type( 'LUFRAC_02', 'Perennial Ice-Snow                ' ),
     &            lus_type( 'LUFRAC_03', 'Developed Open Space              ' ),  ! ?
     &            lus_type( 'LUFRAC_04', 'Developed Low Intensity           ' ),
     &            lus_type( 'LUFRAC_05', 'Developed Medium Intensity        ' ),
     &            lus_type( 'LUFRAC_06', 'Developed High Intensity          ' ),
     &            lus_type( 'LUFRAC_07', 'Barren Land (Rock-Sand-Clay)      ' ),
     &            lus_type( 'LUFRAC_08', 'Unconsolidated Shore              ' ),  ! ?
     &            lus_type( 'LUFRAC_09', 'Deciduous Forest                  ' ),
     &            lus_type( 'LUFRAC_10', 'Evergreen Forest                  ' ),
     &            lus_type( 'LUFRAC_11', 'Mixed Forest                      ' ),
     &            lus_type( 'LUFRAC_12', 'Dwarf Scrub                       ' ),
     &            lus_type( 'LUFRAC_13', 'Shrub-Scrub                       ' ),
     &            lus_type( 'LUFRAC_14', 'Grassland-Herbaceous              ' ),
     &            lus_type( 'LUFRAC_15', 'Sedge-Herbaceous                  ' ),
     &            lus_type( 'LUFRAC_16', 'Lichens                           ' ),
     &            lus_type( 'LUFRAC_17', 'Moss                              ' ),
     &            lus_type( 'LUFRAC_18', 'Tundra                            ' ),
     &            lus_type( 'LUFRAC_19', 'Pasture-Hay                       ' ),
     &            lus_type( 'LUFRAC_20', 'Cultivated Crops                  ' ),
     &            lus_type( 'LUFRAC_21', 'Woody Wetlands                    ' ),
     &            lus_type( 'LUFRAC_22', 'Palustrine Forested Wetland       ' ),
     &            lus_type( 'LUFRAC_23', 'Palustrine Scrub-Shrub Wetland    ' ),  ! ?
     &            lus_type( 'LUFRAC_24', 'Estuarine Forested Wetland        ' ),
     &            lus_type( 'LUFRAC_25', 'Estuarine Scrub_Shrub Wetland     ' ),  ! ?
     &            lus_type( 'LUFRAC_26', 'Emergent Herbaceous Wetlands      ' ),  ! ?
     &            lus_type( 'LUFRAC_27', 'Palustrine Emergent Wetland       ' ),  ! ?
     &            lus_type( 'LUFRAC_28', 'Estuarine Emergent Wetland        ' ),  ! ?
     &            lus_type( 'LUFRAC_29', 'Palustrine Aquatic Bed            ' ),  ! ?
     &            lus_type( 'LUFRAC_30', 'Estuarine Aquatic Bed             ' ),  ! ?
     &            lus_type( 'LUFRAC_31', 'Water                             ' ),
     &            lus_type( 'LUFRAC_32', 'Evergreen Needleleaf Forest       ' ),
     &            lus_type( 'LUFRAC_33', 'Evergreen Broadleaf Forest        ' ),
     &            lus_type( 'LUFRAC_34', 'Deciduous Needleleaf Forest       ' ),
     &            lus_type( 'LUFRAC_35', 'Deciduous Broadleaf Forest        ' ),
     &            lus_type( 'LUFRAC_36', 'Mixed Forests                     ' ),
     &            lus_type( 'LUFRAC_37', 'Closed Shrublands                 ' ),
     &            lus_type( 'LUFRAC_38', 'Open Shrublands                   ' ),
     &            lus_type( 'LUFRAC_39', 'Woody Savannas                    ' ),
     &            lus_type( 'LUFRAC_40', 'Savannas                          ' ),
     &            lus_type( 'LUFRAC_41', 'Grasslands                        ' ),
     &            lus_type( 'LUFRAC_42', 'Permanent Wetlands                ' ),
     &            lus_type( 'LUFRAC_43', 'Croplands                         ' ),
     &            lus_type( 'LUFRAC_44', 'Urban and Built Up                ' ),
     &            lus_type( 'LUFRAC_45', 'Cropland-Natural Vegetation Mosaic' ),
     &            lus_type( 'LUFRAC_46', 'Permanent Snow and Ice            ' ),
     &            lus_type( 'LUFRAC_47', 'Barren or Sparsely Vegetated      ' ),
     &            lus_type( 'LUFRAC_48', 'IGBP Water                        ' ),
     &            lus_type( 'LUFRAC_49', 'unclassified                      ' ),  ! ?
     &            lus_type( 'LUFRAC_50', 'fill value                        ' ) /)  ! ?

! NLCD40 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     read file: GRID_CRO_2D
! includes both nlcd and MODIS versions of shrublands
            integer, parameter :: n_dlcat_nlcd40 = 6   ! number of desert land types
            type( lus_type ), parameter :: vnmld_nlcd40( n_dlcat_nlcd40 ) = (/
     &            lus_type( 'LUFRAC_06', 'Closed Shrublands           ' ),
     &            lus_type( 'LUFRAC_07', 'Open Shrublands             ' ),
     &            lus_type( 'LUFRAC_27', 'Barren Land (Rock-Sand-Clay)' ),
     &            lus_type( 'LUFRAC_31', 'Dwarf Scrub                 ' ),
     &            lus_type( 'LUFRAC_32', 'Shrub-Scrub                 ' ),
     &            lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated' ) /)

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
     &            lus_type( 'LUFRAC_01', 'Evergreen Needleleaf Forest       ' ),
     &            lus_type( 'LUFRAC_02', 'Evergreen Broadleaf Forest        ' ),
     &            lus_type( 'LUFRAC_03', 'Deciduous Needleleaf Forest       ' ),
     &            lus_type( 'LUFRAC_04', 'Deciduous Broadleaf Forest        ' ),
     &            lus_type( 'LUFRAC_05', 'Mixed Forests                     ' ),
     &            lus_type( 'LUFRAC_06', 'Closed Shrublands                 ' ),
     &            lus_type( 'LUFRAC_07', 'Open Shrublands                   ' ),
     &            lus_type( 'LUFRAC_08', 'Woody Savannas                    ' ),
     &            lus_type( 'LUFRAC_09', 'Savannas                          ' ),
     &            lus_type( 'LUFRAC_10', 'Grasslands                        ' ),
     &            lus_type( 'LUFRAC_11', 'Permanent Wetlands                ' ),
     &            lus_type( 'LUFRAC_12', 'Croplands                         ' ),
     &            lus_type( 'LUFRAC_13', 'Urban and Built Up                ' ),
     &            lus_type( 'LUFRAC_14', 'Cropland-Natural Vegetation Mosaic' ),
     &            lus_type( 'LUFRAC_15', 'Permanent Snow and Ice            ' ),
     &            lus_type( 'LUFRAC_16', 'Barren or Sparsely Vegetated      ' ),
     &            lus_type( 'LUFRAC_17', 'IGBP Water                        ' ),
     &            lus_type( 'LUFRAC_18', 'unclassified                      ' ),  ! ?
     &            lus_type( 'LUFRAC_19', 'fill value                        ' ),
     &            lus_type( 'LUFRAC_20', 'unclassified                      ' ),  ! ?
     &            lus_type( 'LUFRAC_21', 'Open Water                        ' ),
     &            lus_type( 'LUFRAC_22', 'Perennial Ice-Snow                ' ),
     &            lus_type( 'LUFRAC_23', 'Developed Open Space              ' ),  ! ?
     &            lus_type( 'LUFRAC_24', 'Developed Low Intensity           ' ),
     &            lus_type( 'LUFRAC_25', 'Developed Medium Intensity        ' ),
     &            lus_type( 'LUFRAC_26', 'Developed High Intensity          ' ),
     &            lus_type( 'LUFRAC_27', 'Barren Land (Rock-Sand-Clay)      ' ),
     &            lus_type( 'LUFRAC_28', 'Deciduous Forest                  ' ),
     &            lus_type( 'LUFRAC_29', 'Evergreen Forest                  ' ),
     &            lus_type( 'LUFRAC_30', 'Mixed Forest                      ' ),
     &            lus_type( 'LUFRAC_31', 'Dwarf Scrub                       ' ),
     &            lus_type( 'LUFRAC_32', 'Shrub-Scrub                       ' ),
     &            lus_type( 'LUFRAC_33', 'Grassland-Herbaceous              ' ),
     &            lus_type( 'LUFRAC_34', 'Sedge-Herbaceous                  ' ),
     &            lus_type( 'LUFRAC_35', 'Lichens                           ' ),
     &            lus_type( 'LUFRAC_36', 'Moss                              ' ),
     &            lus_type( 'LUFRAC_37', 'Pasture-Hay                       ' ),
     &            lus_type( 'LUFRAC_38', 'Cultivated Crops                  ' ),
     &            lus_type( 'LUFRAC_39', 'Woody Wetlands                    ' ),
     &            lus_type( 'LUFRAC_40', 'Emergent Herbaceous Wetlands      ' )  /)  ! ?

          end module lus_data_module

! ------------------------------------------------------------------------
          module stack_group_data_module

            use UDTYPES, ONLY: IARRY1, RARRY1

            real, allocatable    :: XLOCA(:,:),
     &                              YLOCA(:,:)
            integer, ALLOCATABLE :: NSRC( : )
            integer, allocatable :: STKID(:,:)

            TYPE( RARRY1 ), ALLOCATABLE :: STKDIAM( : )
            TYPE( RARRY1 ), ALLOCATABLE :: STKHT  ( : )
            TYPE( RARRY1 ), ALLOCATABLE :: STKTK  ( : )
            TYPE( RARRY1 ), ALLOCATABLE :: STKVEL ( : )
            TYPE( RARRY1 ), ALLOCATABLE :: ACRES_BURNED( : )

            LOGICAL, ALLOCATABLE :: FIRE_ON( : )

          end module stack_group_data_module

! ------------------------------------------------------------------------
!         module bidi_data_module

!           Integer, Parameter :: HG_AQ_CONC   = 3
!           Integer, Parameter :: HG_GAS_CONC  = 1
!           Integer, Parameter :: HG_SOL_CONC  = 2

!           Integer, Parameter :: HG_TOT = HG_AQ_CONC + HG_GAS_CONC + HG_SOL_CONC       

!           Real, Allocatable :: CMEDIA ( :,:,: ) ! surface layer concs 

!           Character( 16 ), Save , Allocatable :: MEDIA_NAMES( : )

!         end module bidi_data_module

! ------------------------------------------------------------------------
          module biog_emis_param_module

! Former include file  B3V12DIMS3.EXT
! dimensioning parameters for BEIS v3.12 

            INTEGER, PARAMETER :: NSEF     = 35 ! # of em class in Summer emis factor table
            INTEGER, PARAMETER :: NSEASONS = 2  ! number of seasons modeled (2 for now)
            INTEGER, PARAMETER :: NLAI     = 3  ! no. of different LAIs (ISOP and MBO)
            INTEGER, PARAMETER :: NSUMMER  = 1  ! summer flag
            INTEGER, PARAMETER :: NWINTER  = 2  ! winter flag

! Names for BEIS species, emitted model species
! DO NOT CHANGE THE ORDER OF THESE ARRAYS

            CHARACTER( 5 ), PARAMETER :: BIOTYPES( NSEF ) =
     &                          (/ 'ISOP ', 'MBO  ', 'METH ',
     &                             'APIN ', 'BPIN ', 'D3CAR', 'DLIM ',
     &                             'CAMPH', 'MYRC ', 'ATERP', 'BPHE ',
     &                             'SABI ', 'PCYM ', 'OCIM ', 'ATHU ',
     &                             'TRPO ', 'GTERP', 'ETHE ', 'PROPE',
     &                             'ETHO ', 'ACET ', 'HEXA ', 'HEXE ',
     &                             'HEXY ', 'FORM ', 'ACTAL', 'BUTE ',
     &                             'ETHA ', 'FORAC', 'ACTAC', 'BUTO ',
     &                             'CO   ', 'ORVOC', 'SESQT',
     &                             'NO   ' /)

            CHARACTER( 5 ), PARAMETER :: LAITYPES( NLAI ) =
     &                          (/ 'ISOP ', 'MBO  ', 'METH ' /)

            CHARACTER( 1 ), PARAMETER :: SEASON( NSEASONS ) =
     &                          (/ 'S', 'W' /)

          end module biog_emis_param_module

! ------------------------------------------------------------------------
          module em_param_module

            INTEGER :: N_EM_SRM   ! global number of emissions streams
            INTEGER :: N_EMIS_ISTR! global emissions instructions
            INTEGER :: N_EM_RGN   ! no. of region masks

            ! Define Data Structure for Storing Emission Control Namelist Options  
            TYPE EM_REG
                CHARACTER( 32 ) :: LABEL
                CHARACTER( 32 ) :: FILE
                CHARACTER( 32 ) :: VAR
                INTEGER         :: FILENUM
            END TYPE EM_REG

            TYPE( EM_REG ), ALLOCATABLE :: EM_REGIONS( : )
            REAL, ALLOCATABLE           :: EM_REG_FAC( :,:,: )

            TYPE EM_RULE_REG
                CHARACTER( 32 ) :: REGION
                CHARACTER( 32 ) :: STREAM
                CHARACTER( 16 ) :: SURR
                CHARACTER( 16 ) :: SPEC
                CHARACTER( 16 ) :: PHASE
                REAL            :: FAC
                CHARACTER( 4  ) :: BASIS
                CHARACTER( 1  ) :: OP
            END TYPE EM_RULE_REG
            INTEGER, PARAMETER  :: N_EM_RULE_REG = 2000
            TYPE( EM_RULE_REG ), ALLOCATABLE :: EM_NML( : )

            TYPE REGIONS_REG
                CHARACTER( 32 ) :: LABEL
                CHARACTER( 32 ) :: FILE
                CHARACTER( 32 ) :: VAR
            END TYPE REGIONS_REG
            INTEGER, PARAMETER  :: N_REGIONS_REG = 200
            TYPE( REGIONS_REG ), ALLOCATABLE :: RGN_NML( : )

            ! Define Structure for Size Distribution Rule      
            TYPE SD_RULE
                CHARACTER( 32 ) :: STREAM
                CHARACTER( 16 ) :: MODE
                CHARACTER( 16 ) :: MODE_REF
            END TYPE SD_RULE
            INTEGER, PARAMETER :: N_SD_REG = 200
            TYPE( SD_RULE ), ALLOCATABLE :: SD_NML( : )

            LOGICAL :: Guard_BiogenicVOC   = .FALSE.
            LOGICAL :: Guard_MarineGas     = .FALSE.
            LOGICAL :: Guard_LightningNO   = .FALSE.
            LOGICAL :: Guard_WindBlownDust = .FALSE.
            LOGICAL :: Guard_SeaSpray      = .FALSE.

          end module em_param_module

