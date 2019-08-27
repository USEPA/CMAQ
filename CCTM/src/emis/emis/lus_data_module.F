
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
! This module contains key component from LUS_DEFN.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation
!  12 Aug 2019 F.Sidi Moved vnmld to its on type, and created a integer entry 
!                     to map dust categories from LUFRAC. **NOTE land use type
!                     with no mapped index has a dummy -999 value.** 
!------------------------------------------------------------------------!

          module lus_data_module

            implicit none

            type lus_type
               character( 16 ) :: name
               character( 64 ) :: desc
            end type lus_type
            
            type lus_type_2
               character( 16 ) :: name
               character( 64 ) :: desc
               integer :: lu_idx
            end type lus_type_2


            type( lus_type_2 ), allocatable :: vnmld( : )
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
            type( lus_type_2 ), parameter :: vnmld_beld3( n_dlcat_beld3 ) = (/
     &            lus_type_2( 'USGS_shrubland ', 'shrub_land    ',-999 ),
     &            lus_type_2( 'USGS_shrubgrass', 'shrub_grass   ',-999 ),
     &            lus_type_2( 'USGS_sprsbarren', 'sparse_barren ',-999 ) /)

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
!     read file: E2C_LU

            integer, parameter :: n_dlcat_beld4 = 6   ! number of desert land categories
            type( lus_type_2 ), parameter :: vnmld_beld4( n_dlcat_beld4 ) = (/
     &            lus_type_2( 'NLCD_52 ', 'Shrub/Scrub                 ',-999 ),  ! shrubland?
     &            lus_type_2( 'MODIS_6 ', 'closed shrublands           ',-999 ),
     &            lus_type_2( 'MODIS_7 ', 'open shrublands             ',-999 ),
     &            lus_type_2( 'NLCD_51 ', 'Dwarf Scrub                 ',-999 ),  ! shrubgrass?
     &            lus_type_2( 'NLCD_31 ', 'Barren Land (Rock/Sand/Clay)',-999 ),
     &            lus_type_2( 'MODIS_16', 'barren or sparsely vegetated',-999 ) /)

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
            type( lus_type_2 ), parameter :: vnmld_usgs24( n_dlcat_usgs24 ) = (/
     &            lus_type_2( 'LUFRAC_08', 'Shrubland                   ',8  ),
     &            lus_type_2( 'LUFRAC_19', 'Barren or Sparsely Vegetated',19 ),
     &            lus_type_2( 'LUFRAC_22', 'Mixed Tundra                ',22 ),
     &            lus_type_2( 'LUFRAC_23', 'Bare Ground Tundra          ',23 ) /)

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
            type( lus_type_2 ), parameter :: vnmld_modis( n_dlcat_modis ) = (/
     &            lus_type_2( 'nada', 'nada',-999 ) /)
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
            type( lus_type_2 ), parameter :: vnmld_modis_noah( n_dlcat_modis_noah ) = (/
     &            lus_type_2( 'LUFRAC_06', 'Closed Shrublands           ',6  ),
     &            lus_type_2( 'LUFRAC_07', 'Open Shrublands             ',7  ),
     &            lus_type_2( 'LUFRAC_16', 'Barren or Sparsely Vegetated',16 ),
     &            lus_type_2( 'LUFRAC_20', 'Barren Tundra               ',20 ) /)

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
            type( lus_type_2 ), parameter :: vnmld_nlcd_modis( n_dlcat_nlcd_modis ) = (/
     &            lus_type_2( 'LUFRAC_37', 'Closed Shrublands           ',37 ),
     &            lus_type_2( 'LUFRAC_38', 'Open Shrublands             ',38 ),
     &            lus_type_2( 'LUFRAC_07', 'Barren Land (Rock-Sand-Clay)',7  ),
     &            lus_type_2( 'LUFRAC_12', 'Dwarf Scrub                 ',12 ),
     &            lus_type_2( 'LUFRAC_13', 'Shrub-Scrub                 ',13 ),
     &            lus_type_2( 'LUFRAC_18', 'Tundra                      ',18 ),
     &            lus_type_2( 'LUFRAC_47', 'Barren or Sparsely Vegetated',47 ) /)

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
            type( lus_type_2 ), parameter :: vnmld_nlcd40( n_dlcat_nlcd40 ) = (/
     &            lus_type_2( 'LUFRAC_06', 'Closed Shrublands           ',6  ),
     &            lus_type_2( 'LUFRAC_07', 'Open Shrublands             ',7  ),
     &            lus_type_2( 'LUFRAC_27', 'Barren Land (Rock-Sand-Clay)',27 ),
     &            lus_type_2( 'LUFRAC_31', 'Dwarf Scrub                 ',31 ),
     &            lus_type_2( 'LUFRAC_32', 'Shrub-Scrub                 ',32 ),
     &            lus_type_2( 'LUFRAC_16', 'Barren or Sparsely Vegetated',16 ) /)

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
