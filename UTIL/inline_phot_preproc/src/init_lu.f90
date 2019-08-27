
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
! $Header: /project/work/rep/MCIP2/src/mcip2/init_lu.F


SUBROUTINE init_lu

!-------------------------------------------------------------------------------
! Name:     Initialize Land Use Classification Arrays
! Purpose:  Initializes land use classification arrays.
! Notes:    Land use classifications are based off systems used in MM5 and WRF.
! Revised:  25 Aug 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE luvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Initialize "Old" MM5 13-category classification.
!-------------------------------------------------------------------------------

  lucatold( 1) = "Urban Land"
  lucatold( 2) = "Agriculture"
  lucatold( 3) = "Range-Grassland"
  lucatold( 4) = "Deciduous Forest"
  lucatold( 5) = "Coniferous Forest"
  lucatold( 6) = "Mixed Forest and Wetland"
  lucatold( 7) = "Water"
  lucatold( 8) = "Marsh or Wetland"
  lucatold( 9) = "Desert"
  lucatold(10) = "Tundra"
  lucatold(11) = "Permanent Ice"
  lucatold(12) = "Tropical or Subtropical Forest"
  lucatold(13) = "Savannah"

!-------------------------------------------------------------------------------
! Initialize USGS 24-category classification.
!-------------------------------------------------------------------------------

  lucatusgs24( 1) = "Urban Land"
  lucatusgs24( 2) = "Dryland Cropland and Pasture"
  lucatusgs24( 3) = "Irrigated Cropland and Pasture"
  lucatusgs24( 4) = "Mixed Dryland and Irrigated Cropland and Pasture"
  lucatusgs24( 5) = "Cropland-Grassland Mosaic"
  lucatusgs24( 6) = "Cropland-Woodland Mosaic"
  lucatusgs24( 7) = "Grassland"
  lucatusgs24( 8) = "Shrubland"
  lucatusgs24( 9) = "Mixed Shrubland-Grassland"
  lucatusgs24(10) = "Savannah"
  lucatusgs24(11) = "Deciduous Broadleaf Forest"
  lucatusgs24(12) = "Deciduous Needleleaf Forest"
  lucatusgs24(13) = "Evergreen Broadleaf Forest"
  lucatusgs24(14) = "Evergreen Needleleaf Forest"
  lucatusgs24(15) = "Mixed Forest"
  lucatusgs24(16) = "Water"
  lucatusgs24(17) = "Herbaceous Wetland"
  lucatusgs24(18) = "Wooded Wetland"
  lucatusgs24(19) = "Barren or Sparsely Vegetated"
  lucatusgs24(20) = "Herbaceous Tundra"
  lucatusgs24(21) = "Wooded Tundra"
  lucatusgs24(22) = "Mixed Tundra"
  lucatusgs24(23) = "Bare Ground Tundra"
  lucatusgs24(24) = "Snow or Ice"

!-------------------------------------------------------------------------------
! Initialize SiB 16-category classification.
!-------------------------------------------------------------------------------

  lucatsib( 1) = "Evergreen Broadleaf Trees"
  lucatsib( 2) = "Broadleaf Deciduous Trees"
  lucatsib( 3) = "Deciduous and Evergreen Trees"
  lucatsib( 4) = "Evergreen Needleleaf Trees"
  lucatsib( 5) = "Deciduous Needleleaf Trees"
  lucatsib( 6) = "Ground Cover with Trees and Shrubs"
  lucatsib( 7) = "Ground Cover Only"
  lucatsib( 8) = "Broadleaf Shrubs with Perennial Ground Cover"
  lucatsib( 9) = "Broadleaf Shrubs with Bare Soil"
  lucatsib(10) = "Ground Cover with Dwarf Trees and Shrubs"
  lucatsib(11) = "Bare Soil"
  lucatsib(12) = "Agriculture or C3 Grassland"
  lucatsib(13) = "Persistent Wetland"
  lucatsib(14) = "Dry Coastal Complexes"
  lucatsib(15) = "Water"
  lucatsib(16) = "Ice Cap and Glacier"

!-------------------------------------------------------------------------------
! Initialize USGS 33-category classification.
!-------------------------------------------------------------------------------

  lucatusgs33(1:24) = lucatusgs24(1:24)
  lucatusgs33(25)   = "Playa"
  lucatusgs33(26)   = "Lava"
  lucatusgs33(27)   = "White Sand"
  lucatusgs33(28)   = "~~~unassigned~~~"
  lucatusgs33(29)   = "~~~unassigned~~~"
  lucatusgs33(30)   = "~~~unassigned~~~"
  lucatusgs33(31)   = "Low Intensity Residential"
  lucatusgs33(32)   = "High Intensity Residential"
  lucatusgs33(33)   = "Industrial or Commercial"

!-------------------------------------------------------------------------------
! Initialize Modified IGBP MODIS NOAH 33-category classification.
!-------------------------------------------------------------------------------

  lucatmod( 1) = "Evergreen Needleleaf Forest"
  lucatmod( 2) = "Evergreen Broadleaf Forest"
  lucatmod( 3) = "Deciduous Needleleaf Forest"
  lucatmod( 4) = "Deciduous Broadleaf Forest"
  lucatmod( 5) = "Mixed Forests"
  lucatmod( 6) = "Closed Shrublands"
  lucatmod( 7) = "Open Shrublands"
  lucatmod( 8) = "Woody Savannas"
  lucatmod( 9) = "Savannas"
  lucatmod(10) = "Grasslands"
  lucatmod(11) = "Permanent Wetlands"
  lucatmod(12) = "Croplands"
  lucatmod(13) = "Urban and Built-Up"
  lucatmod(14) = "Cropland-Natural Vegetation Mosaic"
  lucatmod(15) = "Snow and Ice"
  lucatmod(16) = "Barren or Sparsely Vegetated"
  lucatmod(17) = "Water"
  lucatmod(18) = "Wooded Tundra"
  lucatmod(19) = "Mixed Tundra"
  lucatmod(20) = "Barren Tundra"
  lucatmod(21) = "~~~unassigned~~~"
  lucatmod(22) = "~~~unassigned~~~"
  lucatmod(23) = "~~~unassigned~~~"
  lucatmod(24) = "~~~unassigned~~~"
  lucatmod(25) = "~~~unassigned~~~"
  lucatmod(26) = "~~~unassigned~~~"
  lucatmod(27) = "~~~unassigned~~~"
  lucatmod(28) = "~~~unassigned~~~"
  lucatmod(29) = "~~~unassigned~~~"
  lucatmod(30) = "~~~unassigned~~~"
  lucatmod(31) = "Low Intensity Residential"
  lucatmod(32) = "High Intensity Residential"
  lucatmod(33) = "Industrial or Commercial"

!-------------------------------------------------------------------------------
! Initialize NLCD-MODIS 50-category classification.
!-------------------------------------------------------------------------------

  lucatnlcd( 1) = "Open Water"
  lucatnlcd( 2) = "Perennial Ice-Snow"
  lucatnlcd( 3) = "Developed Open Space"
  lucatnlcd( 4) = "Developed Low Intensity"
  lucatnlcd( 5) = "Developed Medium Intensity"
  lucatnlcd( 6) = "Developed High Intensity"
  lucatnlcd( 7) = "Barren Land (Rock-Sand-Clay)"
  lucatnlcd( 8) = "Unconsolidated Shore"
  lucatnlcd( 9) = "Deciduous Forest"
  lucatnlcd(10) = "Evergreen Forest"
  lucatnlcd(11) = "Mixed Forest"
  lucatnlcd(12) = "Dwarf Scrub"
  lucatnlcd(13) = "Shrub-Scrub"
  lucatnlcd(14) = "Grassland-Herbaceous"
  lucatnlcd(15) = "Sedge-Herbaceous"
  lucatnlcd(16) = "Lichens"
  lucatnlcd(17) = "Moss"
  lucatnlcd(18) = "Tundra"
  lucatnlcd(19) = "Pasture-Hay"
  lucatnlcd(20) = "Cultivated Crops"
  lucatnlcd(21) = "Woody Wetlands"
  lucatnlcd(22) = "Palustrine Forested Wetland"
  lucatnlcd(23) = "Palustrine Scrub-Shrub Wetland"
  lucatnlcd(24) = "Estuarine Forested Wetland"
  lucatnlcd(25) = "Estuarine Scrub-Shrub Wetland"
  lucatnlcd(26) = "Emergent Herbaceous Wetlands"
  lucatnlcd(27) = "Palustrine Emergent Wetland"
  lucatnlcd(28) = "Estuarine Emergent Wetland"
  lucatnlcd(29) = "Palustrine Aquatic Bed"
  lucatnlcd(30) = "Estuarine Aquatic Bed"
  lucatnlcd(31) = "Water"
  lucatnlcd(32) = "Evergreen Needleleaf Forest"
  lucatnlcd(33) = "Evergreen Broadleaf Forest"
  lucatnlcd(34) = "Deciduous Needleleaf Forest"
  lucatnlcd(35) = "Deciduous Broadleaf Forest"
  lucatnlcd(36) = "Mixed Forests"
  lucatnlcd(37) = "Closed Shrublands"
  lucatnlcd(38) = "Open Shrublands"
  lucatnlcd(39) = "Woody Savannas"
  lucatnlcd(40) = "Savannas"
  lucatnlcd(41) = "Grasslands"
  lucatnlcd(42) = "Permanent Wetlands"
  lucatnlcd(43) = "Croplands"
  lucatnlcd(44) = "Urban and Built Up"
  lucatnlcd(45) = "Cropland-Natural Vegetation Mosaic"
  lucatnlcd(46) = "Permanent Snow and Ice"
  lucatnlcd(47) = "Barren or Sparsely Vegetated"
  lucatnlcd(48) = "IGBP Water"
  lucatnlcd(49) = "unclassified"
  lucatnlcd(50) = "fill value"

END SUBROUTINE init_lu
