
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
!  31 Mar 2022 J. Willison Removed BELD as wbdust input 
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
            integer         :: n_dlcat        ! number of desert land categories
            integer         :: n_lucat        ! number of landuse categories

! USGS - United States Geological Survey
! NOAH - A community land-surface model [N=NCEP, O=Oregon State Univ.,
!        A=Air Force, H=Hydrologic Research Lab - NWS(now OHD: Office of
!        Hydrologic Devel.)]
! MODIS - MODerate resolution Imaging Spectroradiometer
! NLCD - National Land Cover Dataset
! IGBP - International Geosphere-Biosphere Programme


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
