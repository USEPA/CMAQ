      MODULE ALBEDO_REFER_DATA
          
        IMPLICIT NONE

          INTEGER, PARAMETER  :: NUMB_BANDS_MODIS   = 15
          INTEGER, PARAMETER  :: NUMB_LANDUSE_MODIS = 20

          REAL( 4 ) :: WVBAND( NUMB_BANDS_MODIS )
          DATA WVBAND / 
     &     200.05,  234.3,  264.8,   292.1,  310.5,
     &      340.0,  397.5,  467.5,  546.25,  642.3,
     &     1000.0, 1550.0, 2150.0,  3000.0, 3750.0 /

          INTEGER, PRIVATE :: IWL 
          REAL( 4 ) :: SPECTRAL_ALBEDO(NUMB_BANDS_MODIS,NUMB_LANDUSE_MODIS)
          DATA (SPECTRAL_ALBEDO(IWL, 1), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.032, 0.032, 0.032, 0.032, 0.032,  ! (1) EVERGREEN NEEDLE FOREST
     &    0.032, 0.032, 0.032, 0.046, 0.046,  
     &    0.235, 0.096, 0.038, 0.038, 0.038 /

          DATA (SPECTRAL_ALBEDO(IWL, 2), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.044, 0.044, 0.044, 0.044, 0.044,  ! ( 2) EVERGREEN BROADLEAF FOR
     &    0.044, 0.044, 0.044, 0.044, 0.044,  !      (Tropical Forest)
     &    0.234, 0.193, 0.112, 0.112, 0.112  /

          DATA (SPECTRAL_ALBEDO(IWL, 3), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.032, 0.032, 0.032, 0.032, 0.032,  ! ( 3) DECIDUOUS NEEDLE FOREST
     &    0.032, 0.032, 0.032, 0.046, 0.046, 
     &    0.235, 0.096, 0.038, 0.038, 0.038  / 

          DATA (SPECTRAL_ALBEDO(IWL, 4), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.034, 0.034, 0.034, 0.034, 0.034,  ! ( 4) DECIDUOUS BROADLEAF FOR
     &    0.034, 0.034, 0.034, 0.066, 0.067,  
     &    0.312, 0.276, 0.160, 0.160, 0.160  / 

          DATA (SPECTRAL_ALBEDO(IWL, 5), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.033, 0.033, 0.033, 0.033, 0.033,  ! ( 5) MIXED FOREST
     &    0.033, 0.033, 0.033, 0.056, 0.057,  
     &    0.274, 0.186, 0.099, 0.099, 0.099  /

          DATA (SPECTRAL_ALBEDO(IWL, 6), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017,  ! ( 6) CLOSED SHRUBS
     &    0.020, 0.036, 0.045, 0.055, 0.156,   
     &    0.350, 0.239, 0.101, 0.101, 0.101  /

          DATA (SPECTRAL_ALBEDO(IWL, 7), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.095, 0.095, 0.095, 0.095, 0.095,  ! ( 7) OPEN/SHRUBS
     &    0.095, 0.098, 0.104, 0.122, 0.157,    
     &    0.231, 0.330, 0.311, 0.150, 0.150  /

          DATA (SPECTRAL_ALBEDO(IWL, 8), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.020, 0.020, 0.020, 0.023, 0.024,  ! ( 8) WOODY SAVANNA (Decid Broadleaf*0.4 + Grass*0.6)
     &    0.026, 0.035, 0.041, 0.102, 0.104, 
     &    0.366, 0.291, 0.151, 0.107, 0.107  /

          DATA (SPECTRAL_ALBEDO(IWL, 9), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017,  ! ( 9) SAVANNA  
     &    0.020, 0.036, 0.045, 0.126, 0.129,  
     &    0.402, 0.301, 0.145, 0.071, 0.071  /

          DATA (SPECTRAL_ALBEDO(IWL, 10), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017,  ! (10) GRASSLAND 
     &    0.020, 0.036, 0.045, 0.126, 0.129,  
     &    0.402, 0.301, 0.145, 0.071, 0.071  /

          DATA (SPECTRAL_ALBEDO(IWL, 11), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.039, 0.039, 0.039, 0.039, 0.039,  ! (11) PERMANENT WETLANDS
     &    0.039, 0.039, 0.039, 0.051, 0.071,  
     &    0.164, 0.100, 0.056, 0.056, 0.056  /

          DATA (SPECTRAL_ALBEDO(IWL, 12), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017,  ! (12) CROPLAND 
     &    0.020, 0.036, 0.045, 0.115, 0.099,  
     &    0.442, 0.271, 0.122, 0.059, 0.059  / 

          DATA (SPECTRAL_ALBEDO(IWL, 13), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.052, 0.052, 0.052, 0.052, 0.052,  ! (13) URBAN
     &    0.052, 0.052, 0.066, 0.104, 0.114,  
     &    0.304, 0.258, 0.258, 0.258, 0.258  /

          DATA (SPECTRAL_ALBEDO(IWL, 14), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017,  ! (14) CROP MOSAIC
     &    0.020, 0.036, 0.045, 0.090, 0.083,  
     &    0.377, 0.273, 0.141, 0.110, 0.110  /

          DATA (SPECTRAL_ALBEDO(IWL, 15), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.910, 0.910, 0.910, 0.916, 0.921,  ! (15) PERMANENT SNOW 
     &    0.931, 0.947, 0.964, 0.953, 0.920,   
     &    0.635, 0.013, 0.006, 0.009, 0.014  / 

          DATA (SPECTRAL_ALBEDO(IWL, 16), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.144, 0.144, 0.144, 0.144, 0.144,  ! (16) BARREN/DESERT
     &    0.144, 0.144, 0.179, 0.263, 0.331,  
     &    0.405, 0.390, 0.390, 0.390, 0.390  /

          DATA (SPECTRAL_ALBEDO(IWL, 17), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.066, 0.066, 0.066, 0.070, 0.073,  ! (17) OCEAN WATER
     &    0.082, 0.094, 0.091, 0.078, 0.072,  
     &    0.066, 0.062, 0.055, 0.044, 0.069  /

          DATA (SPECTRAL_ALBEDO(IWL, 18), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.180, 0.180, 0.180, 0.180, 0.180,  ! (18) TUNDRA  (modified with Lundberg et al)
     &    0.180, 0.180, 0.180, 0.180, 0.180,  
     &    0.247, 0.265, 0.265, 0.265, 0.265  /

          DATA (SPECTRAL_ALBEDO(IWL, 19), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.979, 0.979, 0.979, 0.980, 0.982,  ! (19) FRESH SNOW 
     &    0.984, 0.988, 0.992, 0.989, 0.982,  
     &    0.902, 0.143, 0.168, 0.019, 0.015  /  

          DATA (SPECTRAL_ALBEDO(IWL, 20), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.778, 0.778, 0.778, 0.778, 0.778,  ! (20) SEA ICE
     &    0.778, 0.778, 0.778, 0.778, 0.752,  
     &    0.393, 0.055, 0.054, 0.036, 0.036  /
     
! The following scale factors are applied to the spectral albedos to bring
! them into closer agreement (in the visible spectrum) with the MODIS fixed albedos      

          REAL(4) :: MODIS2SPECTRAL(NUMB_LANDUSE_MODIS)
          DATA MODIS2SPECTRAL /
     &       3.5,   ! (1)
     &       2.5,   ! (2)
     &       4.0,   ! (3)
     &       2.5,   ! (4)
     &       3.0,   ! (5)
     &       3.0,   ! (6)
     &       2.0,   ! (7)
     &       2.5,   ! (8)
     &       1.5,   ! (9)
     &       2.0,   ! (10)
     &       2.0,   ! (11)
     &       1.5,   ! (12)
     &       1.5,   ! (13)
     &       2.5,   ! (14)
     &       1.0,   ! (15)
     &       1.0,   ! (16)
     &       1.0,   ! (17)
     &       1.0,   ! (18)
     &       1.0,   ! (19)
     &       1.0 /  ! (20)

     
 
          CHARACTER(30) LANDUSE(20)
          
          DATA LANDUSE /
     &     'EVERGREEN NEEDLE FOREST'
     &    ,'EVERGREEN BROADLEAF FOREST'
     &    ,'DECIDUOUS NEEDLE FOREST'
     &    ,'DECIDUOUS BROADLEAF FOREST'
     &    ,'MIXED FOREST'
     &    ,'CLOSED SHRUBS'
     &    ,'OPEN / SHRUBS'
     &    ,'WOODY SAVANNA'
     &    ,'SAVANNA'
     &    ,'GRASSLAND'
     &    ,'PERMANENT WETLANDS'
     &    ,'CROPLAND'
     &    ,'URBAN'
     &    ,'CROP MOSAIC'
     &    ,'PERMANENT SNOW'
     &    ,'BARREN / DESSERT'
     &    ,'OCEAN WATER'
     &    ,'TUNDRA'
     &    ,'FRESH SNOW'
     &    ,'SEA ICE' /
                
          INTEGER, PARAMETER :: INDEX_GRASSLAND    = 10
          INTEGER, PARAMETER :: INDEX_OCEAN_WATER  = 17
          INTEGER, PARAMETER :: INDEX_SEA_ICE      = 20
          
          REAL ZENITH_COEFF( NUMB_LANDUSE_MODIS )
          DATA ZENITH_COEFF /
     &                   0.40,   ! ( 1) EVERGREEN NEEDLE FOR 
     &                   0.44,   ! ( 2) EVERGREEN BROAD FOR 
     &                   0.32,   ! ( 3) DECIDUOUS NEEDLE FOR
     &                   0.39,   ! ( 4) DECIDUOUS BROAD FOR
     &                   0.22,   ! ( 5) MIXED FOREST
     &                   0.28,   ! ( 6) CLOSED SHRUBS
     &                   0.40,   ! ( 7) OPEN/SHRUBS
     &                   0.47,   ! ( 8) WOODY SAVANNA
     &                   0.53,   ! ( 9) SAVANNA
     &                   0.53,   ! (10) GRASSLAND
     &                   0.35,   ! (11) WETLAND
     &                   0.41,   ! (12) CROPLAND (CAGEX-APR)
     &                   0.10,   ! (13) URBAN
     &                   0.40,   ! (14) CROP MOSAIC
     &                   0.10,   ! (15) ANTARCTIC SNOW
     &                   0.40,   ! (16) BARREN/DESERT
     &                   0.41,   ! (17) OCEAN WATER
     &                   0.58,   ! (18) TUNDRA
     &                   0.10,   ! (19) FRESH SNOW
     &                   0.10 /  ! (20) SEA ICE

          REAL SEASON_COEFF( NUMB_LANDUSE_MODIS )
          DATA SEASON_COEFF   /
     &                   0.73,    ! EVERGREEN NEEDLE FOR
     &                   0.84,    ! EVERGREEN BROADLEAF FOR
     &                   0.77,    ! DECIDUOUS NEEDLE FOR
     &                   0.73,    ! DECIDOUS BROAD FOR
     &                   0.78,    ! MIXED FOREST
     &                   0.73,    ! CLOSED SHRUBS
     &                   0.77,    ! OPEN/SHRUBS
     &                   0.78,    ! WOODY SAVANNA
     &                   0.78,    ! SAVANNA
     &                   0.79,    ! GRASSLAND
     &                   0.78,    ! WETLAND
     &                   0.80,    ! CROPLAND
     &                   0.67,    ! URBAN
     &                   0.80,    ! CROP MOSAIC
     &                   1.00,    ! PERMANENT SNOW
     &                   0.96,    ! BARREN/DESERT
     &                   1.00,    ! OCEAN WATER
     &                   0.25,    ! TUNDRA		(modified with Lundberg et al (2005))
     &                   1.00,    ! FRESH SNOW
     &                   1.00 /   ! SEA ICE

! values used for CMAQ version 5.0 and 5.01
!
!          REAL SNOW_COEFF( NUMB_LANDUSE_MODIS )
!          DATA SNOW_COEFF  /
!     &                   3.0,   ! EVERGREEN NEEDLE FOR
!     &                   4.1,   ! EVERGREEN BROADLEAF FOR
!     &                   3.1,   ! DECIDUOUS NEEDLE FOR
!     &                   2.7,   ! DECIDUOUS BROADLEAF FOR
!     &                   3.0,   ! MIXED FOREST
!     &                   2.2,   ! CLOSED SHRUBS
!     &                   3.6,   ! OPEN/SHRUBS
!     &                   2.1,   ! WOODY SAVANNA
!     &                   3.0,   ! SAVANNA
!     &                   3.8,   ! GRASSLAND
!     &                   4.9,   ! WETLAND
!     &                   4.5,   ! CROPLAND
!     &                   3.6,   ! URBAN
!     &                   3.6,   ! CROP MOSAIC
!     &                   1.7,   ! PERMANENT SNOW
!     &                   3.5,   ! BARREN/DESERT
!     &                   1.0,   ! OCEAN WATER
!     &                   4.0,   ! TUNDRA		(modified with Lundberg et al (2005))
!     &                   1.0,   ! FRESH SNOW
!     &                   1.3 / ! SEA ICE

! Aug 27, 2013 B. Hutzell: Updated WOODY SAVANNA, SAVANNA, GRASSLAND, CROPLAND, \
! CROP MOSAIC, and BARREN/DESERT to increase albedo in UV spectrum. Based on
!    1) Jin et. al (2002), GRL, Vol 29(10), pg 1374-7.
!    2) Tanskanen and Manninen (2007), Atmos. Chem. Phys., Vol 7, pg 2759-2764.

          REAL SNOW_COEFF( NUMB_LANDUSE_MODIS )
          DATA SNOW_COEFF  /
     &                   3.0,   ! EVERGREEN NEEDLE FOR
     &                   4.1,   ! EVERGREEN BROADLEAF FOR
     &                   3.1,   ! DECIDUOUS NEEDLE FOR
     &                   2.7,   ! DECIDUOUS BROADLEAF FOR
     &                   3.0,   ! MIXED FOREST
     &                   8.6,   ! CLOSED SHRUBS
     &                   3.6,   ! OPEN/SHRUBS
     &                   7.8,   ! WOODY SAVANNA
     &                  25.9,   ! SAVANNA
     &                  19.6,   ! GRASSLAND
     &                   4.9,   ! WETLAND
     &                  26.5,   ! CROPLAND
     &                   3.6,   ! URBAN
     &                  15.9,   ! CROP MOSAIC
     &                   1.7,   ! PERMANENT SNOW
     &                   5.7,   ! BARREN/DESERT
     &                   1.0,   ! OCEAN WATER
     &                   4.0,   ! TUNDRA		(modified with Lundberg et al (2005))
     &                   1.0,   ! FRESH SNOW
     &                   1.3 / ! SEA ICE
	     
          INTEGER, PARAMETER :: NUMB_USGS =   24
          
          CHARACTER(60) USGS_LANDUSE( NUMB_USGS )
          DATA USGS_LANDUSE /
     &     "Urban Land", !                                        'Urban or Built-up Land',
     &     "Dryland Cropland and Pasture", !                      'Dryland Cropland and Pasture',
     &     "Irrigated Cropland and Pasture", !                    'Irrigated Cropland and Pasture',
     &     "Mixed Dryland and Irrigated Cropland and Pasture", !  'Mixed Dry/Irr Crop and Past',
     &     "Cropland-Grassland Mosaic", !                         'Grassland/Cropland Mosaic',
     &     "Cropland-Woodland Mosaic", !                          'Woodland/Cropland Mosaic',
     &     "Grassland", !                                         'Grassland',
     &     "Shrubland", !                                         'Shrubland',
     &     "Mixed Shrubland-Grassland", !                         'Mixed Shrubland/Grassland',
     &     "Savannah", !                                          'Savanna',
     &     "Deciduous Broadleaf Forest", !                        'Broadleaf Deciduous Forest',
     &     "Deciduous Needleleaf Forest", !                       'Deciduous Coniferous Forest',
     &     "Evergreen Broadleaf Forest", !                        'Evergreen Broadleaf Forest (Palm?)',
     &     "Evergreen Needleleaf Forest", !                       'Evergreen Coniferous Forest',
     &     "Mixed Forest", !                                      'Mixed forest',
     &     "Water", !                                             'Water',
     &     "Herbaceous Wetland", !                                'Herbaceous Wetland (none in east)',
     &     "Wooded Wetland", !                                    'Forested Wetlands (e.g. Everglades)',
     &     "Barren or Sparsely Vegetated", !                      'Barren or Sparsely Vegetated',
     &     "Herbaceous Tundra", !                                 'Herbaceous Tundra',
     &     "Wooded Tundra", !                                     'Shrub and Brush Tundra',
     &     "Mixed Tundra", !                                      'Mixed Tundra',
     &     "Bare Ground Tundra", !                                'Bare Ground Tundra',
     &     "Snow or Ice" / !                                       'Perennial Snowfields or Glaciers'  /

! values used for CMAQ version 5.0 and 5.01
!          
!          REAL               :: ALBMAP_REF2USGS( NUMB_USGS )
!          DATA ALBMAP_REF2USGS /
!     &       13.0000,    12.0000,    12.0000,    12.0000,    14.0000,
!     &       14.0000,    10.0000,     6.0000,     7.0000,     9.0000,
!     &        4.0000,     3.0000,     2.0000,     1.0000,     5.0000,
!     &       17.0000,    11.0000,    11.0000,    16.0000,    18.0000,
!     &       18.0000,    18.0000,    18.0000,    15.0000/


! Aug 27, 2013 B. Hutzell: changed albmap for ALBMAP_REF2USGS Shrubland from 7 (CLODSED SHRUBS) to
! 6 (OPEN SHRUB) because descriptions of Shrubland allow large fractions of 
! non-woody plants (up to 70%). 

          REAL               :: ALBMAP_REF2USGS( NUMB_USGS )
          DATA ALBMAP_REF2USGS /
     &       13.0000,    12.0000,    12.0000,    12.0000,    14.0000,
     &       14.0000,    10.0000,     7.0000,     7.0000,     9.0000,
     &        4.0000,     3.0000,     2.0000,     1.0000,     5.0000,
     &       17.0000,    11.0000,    11.0000,    16.0000,    18.0000,
     &       18.0000,    18.0000,    18.0000,    15.0000/

          REAL               :: ALBFAC_REF2USGS( NUMB_USGS )
          DATA ALBFAC_REF2USGS /
     &             1.0000,     1.0000,     1.0588,     1.0588,     1.0000,
     &             0.8889,     1.0000,     1.0000,     1.0000,     1.0000,
     &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &             1.0000,     1.0000,     1.0000,     1.0000/
          
!         INTEGER, PARAMETER :: NUMB_MODIS =   20
!         
!         REAL               :: ALBMAP_REF2MODIS( NUMB_MODIS )
!         DATA ALBMAP_REF2MODIS /
!    &             1.0000,     2.0000,     3.0000,     4.0000,     5.0000,
!    &             6.0000,     7.0000,     8.0000,     9.0000,    10.0000,
!    &            11.0000,    12.0000,    13.0000,    14.0000,    15.0000,
!    &            16.0000,    17.0000,    18.0000,    18.0000,    18.0000/
          
!         REAL               :: ALBFAC_REF2MODIS( NUMB_MODIS )
!         DATA ALBFAC_REF2MODIS /
!    &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
!    &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
!    &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
!    &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000/

          INTEGER, PARAMETER :: NUMB_MODIS_NOAH =   33


          CHARACTER(60) MODIS_LANDUSE( NUMB_MODIS_NOAH )
          DATA MODIS_LANDUSE /
     &      "Evergreen Needleleaf Forest", !                  'EVERGREEN NEEDLE FOREST',
     &      "Evergreen Broadleaf Forest", !                   'EVERGREEN BROADLEAF FOREST',
     &      "Deciduous Needleleaf Forest", !                  'DECIDUOUS NEEDLE FOREST',
     &      "Deciduous Broadleaf Forest", !                   'DECIDUOUS BROADLEAF FOREST',
     &      "Mixed Forests", !                                'MIXED FOREST',
     &      "Closed Shrublands", !                            'CLOSED SHRUBS',
     &      "Open Shrublands", !                              'OPEN / SHRUBS',
     &      "Woody Savannas", !                               'WOODY SAVANNA',
     &      "Savannas", !                                     'SAVANNA',
     &      "Grasslands", !                                   'GRASSLAND',
     &      "Permanent Wetlands", !                           'PERMANENT WETLANDS',
     &      "Croplands", !                                    'CROPLAND',
     &      "Urban and Built-Up", !                           'URBAN',
     &      "Cropland-Natural Vegetation Mosaic", !           'CROP MOSAIC',
     &      "Snow and Ice", !                                 'PERMANENT SNOW',
     &      "Barren or Sparsely Vegetated", !                 'BARREN / DESSERT',
     &      "Water", !                                        'OCEAN WATER',
     &      "Wooded Tundra", !                                'TUNDRA',
     &      "Mixed Tundra", !                                 'FRESH SNOW',
     &      "Barren Tundra",  !                              'SEA ICE' / 
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "~~~unassigned~~~",
     &      "Low Intensity Residential",
     &      "High Intensity Residential",
     &      "Industrial or Commercial"  /

          REAL               :: ALBMAP_REF2MODIS( NUMB_MODIS_NOAH )
          DATA ALBMAP_REF2MODIS /
     &             1.0000,     2.0000,     3.0000,     4.0000,     5.0000,
     &             6.0000,     7.0000,     8.0000,     9.0000,    10.0000,
     &            11.0000,    12.0000,    13.0000,    14.0000,    15.0000,
     &            16.0000,    17.0000,    18.0000,    18.0000,    18.0000,
     &            -1.0000,    -1.0000,    -1.0000,    -1.0000,    -1.0000,
     &            -1.0000,    -1.0000,    -1.0000,    -1.0000,    -1.0000,
     &            13.0000,    13.0000,    13.0000/
          
          REAL               :: ALBFAC_REF2MODIS( NUMB_MODIS_NOAH )
          DATA ALBFAC_REF2MODIS /
     &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &             0.0000,     0.0000,     0.0000,     0.0000,     0.0000,
     &             0.0000,     0.0000,     0.0000,     0.0000,     0.0000,
     &             1.0000,     1.0000,     1.0000 /

          
          INTEGER, PARAMETER :: NUMB_NLCD_MODIS =   50
          
          CHARACTER(60) NLCD_LANDUSE( NUMB_NLCD_MODIS )
          DATA NLCD_LANDUSE /
     &     "Open Water", !                             'Open water',
     &     "Perennial Ice-Snow", !                     'Perennial Ice/snow ',
     &     "Developed Open Space", !                   'Developed, Open space ',
     &     "Developed Low Intensity", !                'Developed, Low Intensity',
     &     "Developed Medium Intensity", !             'Developed, Medium Intensity',
     &     "Developed High Intensity", !               'Developed, High Intensity',
     &     "Barren Land (Rock-Sand-Clay)", !           'Barren land',
     &     "Unconsolidated Shore", !                   'Unconsolidated Shore',
     &     "Deciduous Forest", !                       'Deciduous Forest',
     &     "Evergreen Forest", !                       'Evergreen Forest',
     &     "Mixed Forest", !                           'Mixed Forest',
     &     "Dwarf Scrub", !                            'Dwarf Scrub',
     &     "Shrub-Scrub", !                            'Shrub/Scrub',
     &     "Grassland-Herbaceous", !                   'Grassland/Herbaceous',
     &     "Sedge-Herbaceous", !                       'Sedge/Herbaceous',
     &     "Lichens", !                                'Lichens',
     &     "Moss", !                                   'Moss',
     &     "Tundra", !                                 'Tundra',
     &     "Pasture-Hay", !                            'Pasture/Hay',
     &     "Cultivated Crops", !                       'Cultivated Crops',
     &     "Woody Wetlands", !                         'Woody Wetland',
     &     "Palustrine Forested Wetland", !            'Palustrine Forested Wetland',
     &     "Palustrine Scrub-Shrub Wetland", !         'Palustrine Scrub/Shrub Wetland',
     &     "Estuarine Forested Wetland", !             'Estuarine Forested Wetland',
     &     "Estuarine Scrub-Shrub Wetland", !          'Estuarine Scrub/Shrub Wetland',
     &     "Emergent Herbaceous Wetlands", !           'Emergent Herbaceous Wetland',
     &     "Palustrine Emergent Wetland", !            'Palustrine Emergent Wetland',
     &     "Estuarine Emergent Wetland", !             'Estuarine Emergent Wetland',
     &     "Palustrine Aquatic Bed", !                 'Palustrine Aquatic Bed',
     &     "Estuarine Aquatic Bed", !                  'Estuarine Aquatic Bed',
     &     "Water", !                                  'Open water (mapped to 1)',
     &     "Evergreen Needleleaf Forest", !            'Evergreen Needleleaf Forest',
     &     "Evergreen Broadleaf Forest", !             'Evergreen Broadleaf Forest',
     &     "Deciduous Needleleaf Forest", !            'Deciduous Needleleaf Forest',
     &     "Deciduous Broadleaf Forest", !             'Deciduous Broadleaf Forest',
     &     "Mixed Forests", !                          'Mixed Forest',
     &     "Closed Shrublands", !                      'Closed Shrublands',
     &     "Open Shrublands", !                        'Open Shrublands',
     &     "Woody Savannas", !                         'Woody Savanna',
     &     "Savannas", !                               'Savanna',
     &     "Grasslands", !                             'Grasslands',
     &     "Permanent Wetlands", !                     'Perminent Wetlands',
     &     "Croplands", !                              'Croplands',
     &     "Urban and Built Up", !                     'Urban andBuilt-up',
     &     "Cropland-Natural Vegetation Mosaic", !     'Cropland/Natural Vegetation Mosaic',
     &     "Permanent Snow and Ice", !                 'Snow and Ice',
     &     "Barren or Sparsely Vegetated", !           'Barren or Sparsely Vegetated',
     &     "IGBP Water", !                             'IGBP water',
     &     "unclassified", !                           'unclassified',
     &     "fill value" / !                             'fill value (normally ocean water)' /
          
          REAL               :: ALBMAP_REF2NLCD( NUMB_NLCD_MODIS )
          DATA ALBMAP_REF2NLCD /
     O            17.0000,    15.0000,    13.0000,    13.0000,    13.0000,
     +            13.0000,    16.0000,    16.0000,     4.0000,     1.0000,
     1             5.0000,     7.0000,     7.0000,     8.0000,     8.0000,
     +            10.0000,    10.0000,    18.0000,    14.0000,    12.0000,
     2            11.0000,    11.0000,    11.0000,    11.0000,    11.0000,
     +            11.0000,    11.0000,    11.0000,    17.0000,    17.0000,
     3            17.0000,     1.0000,     2.0000,     3.0000,     4.0000,
     +             5.0000,     6.0000,     7.0000,     8.0000,     9.0000,
     4            10.0000,    11.0000,    12.0000,    13.0000,    14.0000,
     +            15.0000,    16.0000,    17.0000,    17.0000,    17.0000/
          
          REAL               :: ALBFAC_REF2NLCD( NUMB_NLCD_MODIS )
          DATA ALBFAC_REF2NLCD /
     O             1.0000,     1.0000,     1.0667,     1.1333,     1.0667,
     +             1.1333,     0.8000,     1.4000,     0.9375,     0.8333,
     1             1.0000,     1.0000,     1.0000,     1.1364,     1.0455,
     +             1.0526,     1.0526,     1.0000,     1.2778,     1.1765,
     2             1.0714,     1.0714,     1.0714,     1.0714,     1.0714,
     +             1.2857,     1.2857,     1.2857,     1.2500,     1.2500,
     3             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     +             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     4             1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     +             1.0000,     1.0000,     1.0000,     1.0000,     1.0000/
          
          INTEGER, PARAMETER :: NUMB_NLCD40_MODIS =   40
          
          CHARACTER(60) NLCD40_LANDUSE( NUMB_NLCD40_MODIS )
          DATA NLCD40_LANDUSE /
     &     "Evergreen Needleleaf Forest        ",
     &     "Evergreen Broadleaf Forest         ",
     &     "Deciduous Needleleaf Forest        ",
     &     "Deciduous Broadleaf Forest         ",
     &     "Mixed Forests                      ",
     &     "Closed Shrublands                  ",
     &     "Open Shrublands                    ",
     &     "Woody Savannas                     ",
     &     "Savannas                           ",
     &     "Grasslands                         ",
     &     "Permanent Wetlands                 ",
     &     "Croplands                          ",
     &     "Urban and Built Up                 ",
     &     "Cropland-Natural Vegetation Mosaic ",
     &     "Permanent Snow and Ice             ",
     &     "Barren or Sparsely Vegetated       ",
     &     "IGBP Water                         ",
     &     "unclassified                       ",
     &     "fill value                         ",
     &     "unclassified                       ",
     &     "Open Water                         ",
     &     "Perennial Ice-Snow                 ",
     &     "Developed Open Space               ",
     &     "Developed Low Intensity            ",
     &     "Developed Medium Intensity         ",
     &     "Developed High Intensity           ",
     &     "Barren Land (Rock-Sand-Clay)       ",
!    &     "Unconsolidated Shore               ",
     &     "Deciduous Forest                   ",
     &     "Evergreen Forest                   ",
     &     "Mixed Forest                       ",
     &     "Dwarf Scrub                        ",
     &     "Shrub-Scrub                        ",
     &     "Grassland-Herbaceous               ",
     &     "Sedge-Herbaceous                   ",
     &     "Lichens                            ",
     &     "Moss                               ",
     &     "Pasture-Hay                        ",
     &     "Cultivated Crops                   ",
     &     "Woody Wetlands                     ",
     &     "Emergent Herbaceous Wetlands       " /
          
          REAL               :: ALBMAP_REF2NLCD40( NUMB_NLCD40_MODIS )
          DATA ALBMAP_REF2NLCD40 /
     &      1.000,  2.000,  3.000,  4.000,  5.000,  6.000,  7.000,  8.000, 
     &      9.000, 10.000, 11.000, 12.000, 13.000, 14.000, 15.000, 16.000,
     &     17.000, 17.000, 17.000, 17.000, 17.000, 15.000, 13.000, 13.000,
!    &     17.000, 17.000, 17.000, 17.000, 15.000, 13.000, 13.000, 13.000, 
     &     13.000, 13.000, 16.000,  4.000,  1.000,  5.000,  7.000,  7.000, 
!    &     13.000, 16.000, 16.000,  4.000,  1.000,  5.000,  7.000,  7.000, 
     &      8.000,  8.000, 10.000, 10.000, 14.000, 12.000, 11.000, 11.000 / 


!         REAL               :: ALBFAC_REF2NLCD40( NUMB_NLCD40_MODIS )
!         DATA ALBFAC_REF2NLCD40 /
!    &      1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
!    &      1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
!    &      1.000, 1.000, 1.000, 1.000, 1.067, 1.133, 1.067, 1.133,
!    &      0.800, 1.400, 0.938, 0.833, 1.000, 1.000, 1.000, 1.136,
!    &      1.046, 1.053, 1.053, 1.278, 1.176, 1.071, 1.286  /
          
          REAL               :: ALBFAC_REF2NLCD40( NUMB_NLCD40_MODIS )
          DATA ALBFAC_REF2NLCD40 /
     &      1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
     &      1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
     &      1.000, 1.000, 1.000, 1.000, 1.000, 1.067, 1.133, 1.067,
!    &      1.000, 1.000, 1.000, 1.000, 1.067, 1.133, 1.067, 1.133,
     &      1.133, 0.800, 0.938, 0.833, 1.000, 1.000, 1.000, 1.136,
!    &      0.800, 1.400, 0.938, 0.833, 1.000, 1.000, 1.000, 1.136,
     &      1.046, 1.053, 1.053, 1.278, 1.176, 1.071, 1.286  /


      END MODULE ALBEDO_REFER_DATA
          
          
          
          
          

          
