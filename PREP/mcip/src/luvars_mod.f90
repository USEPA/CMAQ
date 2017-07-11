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

MODULE luvars

!-------------------------------------------------------------------------------
! Name:     Land Use Variables
! Purpose:  Contains input land use classification arrays.
! Revised:  25 Aug 2009  Original version.  (T. Otte)
!           01 Sep 2011  Include all land use information as parameter
!                        statements rather than allocatable and filled later.
!                        Eliminates need for routines ALLOC_LU, DEALLOC_LU,
!                        and INIT_LU.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           07 Sep 2012  Added handling for 40-category 2006 NLCD-MODIS land
!                        use classification as "NLCD40".  Added alternate name
!                        for 50-category 2001 NLCD-MODIS land use classification
!                        as "NLCD50".  Changed LUCATNLCD to LUCATNLCD50 and
!                        added new field LUCATNLCD40.  (T. Otte)
!           21 Apr 2017  Updated MODIS category 21 to be "Lake".  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  CHARACTER(LEN=48), PARAMETER :: lucatold ( 13 ) =        &  ! MM5 "old"
    (/ "Urban Land                                      ", &  !  1
       "Agriculture                                     ", &  !  2
       "Range-Grassland                                 ", &  !  3
       "Deciduous Forest                                ", &  !  4
       "Coniferous Forest                               ", &  !  5
       "Mixed Forest and Wetland                        ", &  !  6
       "Water                                           ", &  !  7
       "Marsh or Wetland                                ", &  !  8
       "Desert                                          ", &  !  9
       "Tundra                                          ", &  ! 10
       "Permanent Ice                                   ", &  ! 11
       "Tropical or Subtropical Forest                  ", &  ! 12
       "Savannah                                        "  /) ! 13

  CHARACTER(LEN=48), PARAMETER :: lucatusgs24 ( 24 ) =     &  ! USGS 24-category
    (/ "Urban Land                                      ", &  !  1
       "Dryland Cropland and Pasture                    ", &  !  2
       "Irrigated Cropland and Pasture                  ", &  !  3
       "Mixed Dryland and Irrigated Cropland and Pasture", &  !  4
       "Cropland-Grassland Mosaic                       ", &  !  5
       "Cropland-Woodland Mosaic                        ", &  !  6
       "Grassland                                       ", &  !  7
       "Shrubland                                       ", &  !  8
       "Mixed Shrubland-Grassland                       ", &  !  9
       "Savannah                                        ", &  ! 10
       "Deciduous Broadleaf Forest                      ", &  ! 11
       "Deciduous Needleleaf Forest                     ", &  ! 12
       "Evergreen Broadleaf Forest                      ", &  ! 13
       "Evergreen Needleleaf Forest                     ", &  ! 14
       "Mixed Forest                                    ", &  ! 15
       "Water                                           ", &  ! 16
       "Herbaceous Wetland                              ", &  ! 17
       "Wooded Wetland                                  ", &  ! 18
       "Barren or Sparsely Vegetated                    ", &  ! 19
       "Herbaceous Tundra                               ", &  ! 20
       "Wooded Tundra                                   ", &  ! 21
       "Mixed Tundra                                    ", &  ! 22
       "Bare Ground Tundra                              ", &  ! 23
       "Snow or Ice                                     "  /) ! 24

  CHARACTER(LEN=48), PARAMETER :: lucatsib ( 16 ) =        &  ! SiB
    (/ "Evergreen Broadleaf Trees                       ", &  !  1
       "Broadleaf Deciduous Trees                       ", &  !  2
       "Deciduous and Evergreen Trees                   ", &  !  3
       "Evergreen Needleleaf Trees                      ", &  !  4
       "Deciduous Needleleaf Trees                      ", &  !  5
       "Ground Cover with Trees and Shrubs              ", &  !  6
       "Ground Cover Only                               ", &  !  7
       "Broadleaf Shrubs with Perennial Ground Cover    ", &  !  8
       "Broadleaf Shrubs with Bare Soil                 ", &  !  9
       "Ground Cover with Dwarf Trees and Shrubs        ", &  ! 10
       "Bare Soil                                       ", &  ! 11
       "Agriculture or C3 Grassland                     ", &  ! 12
       "Persistent Wetland                              ", &  ! 13
       "Dry Coastal Complexes                           ", &  ! 14
       "Water                                           ", &  ! 15
       "Ice Cap and Glacier                             "  /) ! 16

  CHARACTER(LEN=48), PARAMETER :: lucatusgs33 ( 33 ) =     &  ! USGS 33-category
    (/ lucatusgs24(1:24),                                  &  ! 1-24
       "Playa                                           ", &  ! 25
       "Lava                                            ", &  ! 26
       "White Sand                                      ", &  ! 27
       "~~~unassigned~~~                                ", &  ! 28
       "~~~unassigned~~~                                ", &  ! 29
       "~~~unassigned~~~                                ", &  ! 30
       "Low Intensity Residential                       ", &  ! 31
       "High Intensity Residential                      ", &  ! 32
       "Industrial or Commercial                        "  /) ! 33

  CHARACTER(LEN=48), PARAMETER :: lucatmod ( 33 ) =        &  ! MODIS-NOAH 33
    (/ "Evergreen Needleleaf Forest                     ", &  !  1
       "Evergreen Broadleaf Forest                      ", &  !  2
       "Deciduous Needleleaf Forest                     ", &  !  3
       "Deciduous Broadleaf Forest                      ", &  !  4
       "Mixed Forests                                   ", &  !  5
       "Closed Shrublands                               ", &  !  6
       "Open Shrublands                                 ", &  !  7
       "Woody Savannas                                  ", &  !  8
       "Savannas                                        ", &  !  9
       "Grasslands                                      ", &  ! 10
       "Permanent Wetlands                              ", &  ! 11
       "Croplands                                       ", &  ! 12
       "Urban and Built-Up                              ", &  ! 13
       "Cropland-Natural Vegetation Mosaic              ", &  ! 14
       "Snow and Ice                                    ", &  ! 15
       "Barren or Sparsely Vegetated                    ", &  ! 16
       "Water                                           ", &  ! 17
       "Wooded Tundra                                   ", &  ! 18
       "Mixed Tundra                                    ", &  ! 19
       "Barren Tundra                                   ", &  ! 20
       "~~~unassigned~~~                                ", &  ! 21
       "~~~unassigned~~~                                ", &  ! 22
       "~~~unassigned~~~                                ", &  ! 23
       "~~~unassigned~~~                                ", &  ! 24
       "~~~unassigned~~~                                ", &  ! 25
       "~~~unassigned~~~                                ", &  ! 26
       "~~~unassigned~~~                                ", &  ! 27
       "~~~unassigned~~~                                ", &  ! 28
       "~~~unassigned~~~                                ", &  ! 29
       "~~~unassigned~~~                                ", &  ! 30
       "Low Intensity Residential                       ", &  ! 31
       "High Intensity Residential                      ", &  ! 32
       "Industrial or Commercial                        "  /) ! 33

  CHARACTER(LEN=48), PARAMETER :: lucatnlcd50 ( 50 ) =     &  ! NLCD-MODIS 50
    (/ "Open Water                                      ", &  !  1
       "Perennial Ice-Snow                              ", &  !  2
       "Developed Open Space                            ", &  !  3
       "Developed Low Intensity                         ", &  !  4
       "Developed Medium Intensity                      ", &  !  5
       "Developed High Intensity                        ", &  !  6
       "Barren Land (Rock-Sand-Clay)                    ", &  !  7
       "Unconsolidated Shore                            ", &  !  8
       "Deciduous Forest                                ", &  !  9
       "Evergreen Forest                                ", &  ! 10
       "Mixed Forest                                    ", &  ! 11
       "Dwarf Scrub                                     ", &  ! 12
       "Shrub-Scrub                                     ", &  ! 13
       "Grassland-Herbaceous                            ", &  ! 14
       "Sedge-Herbaceous                                ", &  ! 15
       "Lichens                                         ", &  ! 16
       "Moss                                            ", &  ! 17
       "Tundra                                          ", &  ! 18
       "Pasture-Hay                                     ", &  ! 19
       "Cultivated Crops                                ", &  ! 20
       "Woody Wetlands                                  ", &  ! 21
       "Palustrine Forested Wetland                     ", &  ! 22
       "Palustrine Scrub-Shrub Wetland                  ", &  ! 23
       "Estuarine Forested Wetland                      ", &  ! 24
       "Estuarine Scrub-Shrub Wetland                   ", &  ! 25
       "Emergent Herbaceous Wetlands                    ", &  ! 26
       "Palustrine Emergent Wetland                     ", &  ! 27
       "Estuarine Emergent Wetland                      ", &  ! 28
       "Palustrine Aquatic Bed                          ", &  ! 29
       "Estuarine Aquatic Bed                           ", &  ! 30
       "Water                                           ", &  ! 31
       "Evergreen Needleleaf Forest                     ", &  ! 32
       "Evergreen Broadleaf Forest                      ", &  ! 33
       "Deciduous Needleleaf Forest                     ", &  ! 34
       "Deciduous Broadleaf Forest                      ", &  ! 35
       "Mixed Forests                                   ", &  ! 36
       "Closed Shrublands                               ", &  ! 37
       "Open Shrublands                                 ", &  ! 38
       "Woody Savannas                                  ", &  ! 39
       "Savannas                                        ", &  ! 40
       "Grasslands                                      ", &  ! 41
       "Permanent Wetlands                              ", &  ! 42
       "Croplands                                       ", &  ! 43
       "Urban and Built Up                              ", &  ! 44
       "Cropland-Natural Vegetation Mosaic              ", &  ! 45
       "Permanent Snow and Ice                          ", &  ! 46
       "Barren or Sparsely Vegetated                    ", &  ! 47
       "IGBP Water                                      ", &  ! 48
       "unclassified                                    ", &  ! 49
       "fill value                                      "  /) ! 50

  CHARACTER(LEN=48), PARAMETER :: lucatnlcd40 ( 40 ) =     &  ! NLCD-MODIS 40
    (/ "Evergreen Needleleaf Forest                     ", &  !  1
       "Evergreen Broadleaf Forest                      ", &  !  2
       "Deciduous Needleleaf Forest                     ", &  !  3
       "Deciduous Broadleaf Forest                      ", &  !  4
       "Mixed Forest                                    ", &  !  5
       "Closed Shrublands                               ", &  !  6
       "Open Shrublands                                 ", &  !  7
       "Woody Savanna                                   ", &  !  8
       "Savanna                                         ", &  !  9
       "Grasslands                                      ", &  ! 10
       "Permanent Wetlands                              ", &  ! 11
       "Croplands                                       ", &  ! 12
       "Urban and Built-up                              ", &  ! 13
       "Cropland-Natural Vegetation Mosaic              ", &  ! 14
       "Snow and Ice                                    ", &  ! 15
       "Barren or Sparsely Vegetated                    ", &  ! 16
       "IGBP water                                      ", &  ! 17
       "unclassified                                    ", &  ! 18
       "fill value (normally ocean water)               ", &  ! 19
       "unclassified                                    ", &  ! 20
       "Open water                                      ", &  ! 21
       "Perrenial Ice/snow                              ", &  ! 22
       "Developed open space                            ", &  ! 23
       "Developed Low Intensity                         ", &  ! 24
       "Developed Medium Intensity                      ", &  ! 25
       "Developed High Intensity                        ", &  ! 26
       "Barren Land                                     ", &  ! 27
       "Deciduous Forest                                ", &  ! 28
       "Evergreen Forest                                ", &  ! 29
       "Mixed Forest                                    ", &  ! 30
       "Dwarf Scrub                                     ", &  ! 31
       "Shrub/Scrub                                     ", &  ! 32
       "Grassland/Herbaceous                            ", &  ! 33
       "Sedge/Herbaceous                                ", &  ! 34
       "Lichens                                         ", &  ! 35
       "Moss                                            ", &  ! 36
       "Pasture/Hay                                     ", &  ! 37
       "Cultivated Crops                                ", &  ! 38
       "Woody Wetland                                   ", &  ! 39
       "Emergent Herbaceous Wetland                     "  /) ! 40

END MODULE luvars
