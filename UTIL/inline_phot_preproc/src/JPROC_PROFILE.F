       MODULE JPROC_PROFILE

#ifndef mech_includes
        USE RXNS_DATA
#endif	   

        IMPLICIT NONE

#ifdef mech_includes
           INCLUDE SUBST_RXCMMN    ! to get mech name
           INCLUDE SUBST_RXDATA    ! to get photolysis rate names
#endif	   

!...........PARAMETERS from JPROC and their descriptions
!    MXWL     = maximum number of wavelength bands to process
!    MXWLIN   = maximum number of wavelength bands on input files
!    NJPROC   = maximum number of vertical levels in radiative transfer
!               calculation
!    MXLEV    = maximum number of vertical levels in base atmospheric 
!               structure

           INTEGER, PARAMETER :: MXWL   = 601
           INTEGER, PARAMETER :: MXWLIN = 2000
           INTEGER, PARAMETER :: NJPROC = 200
           INTEGER, PARAMETER :: MXLEV  = 51

          REAL               :: Z_JPROC(    MXLEV )      ! altitudes for JPROC level, km
          REAL               :: O3_JPROC(   MXLEV )      ! JPROC ozone profile
          REAL               :: T_JPROC(    MXLEV )      ! JPROC temperature profile
          REAL               :: DENS_JPROC(   MXLEV )    ! JPROC number density profile, cm^-3

          LOGICAL, PARAMETER :: USE_JPROC = .TRUE.

! all rate listed in CB05, SAPRC99, SAPRC07T, plus seven additional rates 
! that copy SAPRC07T and SAPRC99 rates. Why? Rates hardwired in PHOT_MOD.F
! multiple occurrances will be eliminated in CREATE_MODULE subroutine

          INTEGER, PARAMETER :: NPHOTAB_ALL =  94
          CHARACTER( 16 )    :: PHOTAB_ALL( NPHOTAB_ALL )
          CHARACTER( 32 )    :: MECHNAME_ALL = 'ALL_MECHANISM'

          INTEGER            :: IRXXN_ALL


!          INTEGER, PARAMETER :: NPHOTAB =  94
!          CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

!          CHARACTER( 32 )    :: MECHNAME = 'ALL_MECHANISM'

!          INTEGER            :: IRXXN

          DATA ( PHOTAB_ALL( IRXXN_ALL ), IRXXN_ALL = 1, NPHOTAB_ALL ) /
!          DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) /
     &   'NO2-06          ', 'NO3NO-06        ', 'NO3NO2-6        ',
     &   'O3O1D-06        ', 'O3O3P-06        ', 'HONO-06         ',
     &   'HNO3            ', 'HNO4-06         ', 'H2O2            ',
     &   'NO2EX           ', 'PAN             ', 'HCHOR-06        ',
     &   'HCHOM-06        ', 'CCHO_R          ', 'C2CHO           ',
     &   'ACET-06         ', 'MEK-06          ', 'COOH            ',
     &   'GLY-07R         ', 'GLY-07M         ', 'MGLY-06         ',
     &   'BACL-07         ', 'BALD-06         ', 'AFG1            ',
     &   'MACR-06         ', 'MVK-06          ', 'IC3ONO2         ',
     &   'HOCCHO_IUPAC    ', 'ACRO-09         ', 'PAA             ',
     &   'CL2             ', 'CLNO-06         ', 'CLONO           ',
     &   'CLNO2           ', 'CLONO2-1        ', 'CLONO2-2        ',
     &   'HOCL-06         ', 'CLCCHO          ', 'CLACET          ',
     &   'NO2_SAPRC99     ', 'NO3NO_SAPRC99   ', 'NO3NO2_SAPRC99  ',
     &   'O3O3P_SAPRC99   ', 'O3O1D_SAPRC99   ', 'HONO_NO_SAPRC99 ',
     &   'HONO_NO2_SAPRC99', 'HNO3_SAPRC99    ', 'HO2NO2_SAPRC99  ',
     &   'H2O2_SAPRC99    ', 'HCHO_R_SAPRC99  ', 'HCHO_M_SAPRC99  ',
     &   'CCHO_R_SAPRC99  ', 'C2CHO_SAPRC99   ', 'ACETONE_SAPRC99 ',
     &   'KETONE_SAPRC99  ', 'COOH_SAPRC99    ', 'GLY_R_SAPRC99   ',
     &   'GLY_ABS_SAPRC99 ', 'MGLY_ADJ_SAPRC99', 'BACL_ADJ_SAPRC99',
     &   'BZCHO_SAPRC99   ', 'ACROLEIN_SAPRC99', 'IC3ONO2_SAPRC99 ',
     &   'MGLY_ABS_SAPRC99',
     &   'NO2_SAPRC99     ', 'O3_O3P_IUPAC04  ', 'O3_O1D_IUPAC04  ',
     &   'NO3NO2_SAPRC99  ', 'NO3NO_SAPRC99   ', 'HONO_IUPAC04    ',
     &   'H2O2_SAPRC99    ', 'HO2NO2_IUPAC04  ', 'HNO3_IUPAC04    ',
     &   'N2O5_IUPAC04    ', 'NTR_IUPAC04     ', 'COOH_SAPRC99    ',
     &   'HCHO_R_SAPRC99  ', 'HCHO_M_SAPRC99  ', 'CCHO_R_SAPRC99  ',
     &   'PAN_IUPAC04     ', 'PACD_CB05       ', 'C2CHO_SAPRC99   ',
     &   'MGLY_IUPAC04    ', 'ACROLEIN_SAPRC99', 'CL2_IUPAC04     ',
     &   'HOCL_IUPAC04    ', 'FMCL_IUPAC04    ', 'NO2             ',
     &   'O3O1D           ', 'O3O3P           ', 'KETONE          ',
     &   'MGLY_ABS        ', 'MGLY_ADJ        ', 'ACETONE         ' /

	    LOGICAL, PARAMETER  :: USE_TUV_JVALUE  = .TRUE.
 
          INTEGER, SAVE                         :: N_USE_PHOTAB 
          CHARACTER( 16 ), ALLOCATABLE, SAVE    :: USE_PHOTAB( : )

!          DATA ( USE_PHOTAB( IRXXN ), IRXXN = 1, N_USE_PHOTAB ) /
!     &   'BALD-06         ', 'AFG1            ', 'IC3ONO2         ',
!     &   'CLNO-06         ', 'CLONO           ', 'CLNO2           ', 
!     &   'HOCL-06         ', 'CLCCHO          ', 'CLACET          '/

          LOGICAL, ALLOCATABLE, SAVE  :: USE_JPROC_CSQY ( : )
          INTEGER, ALLOCATABLE, SAVE  :: TUV_TO_JPROC( : )

          INTEGER, PARAMETER       :: N_TEMP_STRAT   = 6
          INTEGER, PARAMETER       :: N_TEMPERATURE  = 6

          INTEGER, SAVE            :: MODULE_UNIT = 75
          INTEGER, SAVE            :: JTABLE_UNIT = 105

          CHARACTER(16), SAVE      :: PHOT_DONE( NPHOTAB )
          INTEGER,       SAVE      :: NPHOT_DONE
          LOGICAL,       SAVE      :: PHOT_PROCESS( NPHOTAB )
          REAL, ALLOCATABLE, SAVE  :: WV_FASTJ( : ), XO3_FASTJ( : )

          INTEGER, SAVE            :: CSQY_UNIT  =  76

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
 
      CHARACTER(30) LANDUSE(20)

      DATA LANDUSE /
     & 'EVERGREEN NEEDLE FOREST'
     &,'EVERGREEN BROADLEAF FOREST'
     &,'DECIDUOUS NEEDLE FOREST'
     &,'DECIDUOUS BROADLEAF FOREST'
     &,'MIXED FOREST'
     &,'CLOSED SHRUBS'
     &,'OPEN / SHRUBS'
     &,'WOODY SAVANNA'
     &,'SAVANNA'
     &,'GRASSLAND'
     &,'PERMANENT WETLANDS'
     &,'CROPLAND'
     &,'URBAN'
     &,'CROP MOSAIC'
     &,'PERMANENT SNOW'
     &,'BARREN / DESSERT'
     &,'OCEAN WATER'
     &,'TUNDRA'
     &,'FRESH SNOW'
     &,'SEA ICE' /
            
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

          REAL SNOW_COEFF( NUMB_LANDUSE_MODIS )
          DATA SNOW_COEFF  /
     &                   3.0,   ! EVERGREEN NEEDLE FOR
     &                   4.1,   ! EVERGREEN BROADLEAF FOR
     &                   3.1,   ! DECIDUOUS NEEDLE FOR
     &                   2.7,   ! DECIDUOUS BROADLEAF FOR
     &                   3.0,   ! MIXED FOREST
     &                   2.2,   ! CLOSED SHRUBS
     &                   3.6,   ! OPEN/SHRUBS
     &                   2.1,   ! WOODY SAVANNA
     &                   3.0,   ! SAVANNA
     &                   3.8,   ! GRASSLAND
     &                   4.9,   ! WETLAND
     &                   4.5,   ! CROPLAND
     &                   3.6,   ! URBAN
     &                   3.6,   ! CROP MOSAIC
     &                   1.7,   ! PERMANENT SNOW
     &                   3.5,   ! BARREN/DESERT
     &                   1.0,   ! OCEAN WATER
     &                   4.0,   ! TUNDRA		(modified with Lundberg et al (2005))
     &                   1.0,   ! FRESH SNOW
     &                   1.3 / ! SEA ICE
	     
      INTEGER, PARAMETER :: NUMB_USGS =   24

      CHARACTER(60) USGS_LANDUSE( NUMB_USGS )
      DATA USGS_LANDUSE /
     & "Urban Land", !                                        'Urban or Built-up Land',
     & "Dryland Cropland and Pasture", !                      'Dryland Cropland and Pasture',
     & "Irrigated Cropland and Pasture", !                    'Irrigated Cropland and Pasture',
     & "Mixed Dryland and Irrigated Cropland and Pasture", !  'Mixed Dry/Irr Crop and Past',
     & "Cropland-Grassland Mosaic", !                         'Grassland/Cropland Mosaic',
     & "Cropland-Woodland Mosaic", !                          'Woodland/Cropland Mosaic',
     & "Grassland", !                                         'Grassland',
     & "Shrubland", !                                         'Shrubland',
     & "Mixed Shrubland-Grassland", !                         'Mixed Shrubland/Grassland',
     & "Savannah", !                                          'Savanna',
     & "Deciduous Broadleaf Forest", !                        'Broadleaf Deciduous Forest',
     & "Deciduous Needleleaf Forest", !                       'Deciduous Coniferous Forest',
     & "Evergreen Broadleaf Forest", !                        'Evergreen Broadleaf Forest (Palm?)',
     & "Evergreen Needleleaf Forest", !                       'Evergreen Coniferous Forest',
     & "Mixed Forest", !                                      'Mixed forest',
     & "Water", !                                             'Water',
     & "Herbaceous Wetland", !                                'Herbaceous Wetland (none in east)',
     & "Wooded Wetland", !                                    'Forested Wetlands (e.g. Everglades)',
     & "Barren or Sparsely Vegetated", !                      'Barren or Sparsely Vegetated',
     & "Herbaceous Tundra", !                                 'Herbaceous Tundra',
     & "Wooded Tundra", !                                     'Shrub and Brush Tundra',
     & "Mixed Tundra", !                                      'Mixed Tundra',
     & "Bare Ground Tundra", !                                'Bare Ground Tundra',
     & "Snow or Ice" / !                                       'Perennial Snowfields or Glaciers'  /
     
      REAL               :: ALBMAP_REF2USGS( NUMB_USGS )
      DATA ALBMAP_REF2USGS /
     &        13.0000,    12.0000,    12.0000,    12.0000,    14.0000,
     &        14.0000,    10.0000,     6.0000,     7.0000,     9.0000,
     &         4.0000,     3.0000,     2.0000,     1.0000,     5.0000,
     &        17.0000,    11.0000,    11.0000,    16.0000,    18.0000,
     &        18.0000,    18.0000,    18.0000,    15.0000/

      REAL               :: ALBFAC_REF2USGS( NUMB_USGS )
      DATA ALBFAC_REF2USGS /
     &         1.0000,     1.0000,     1.0588,     1.0588,     1.0000,
     &         0.8889,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000/

!     INTEGER, PARAMETER :: NUMB_MODIS =   20
!
!     REAL               :: ALBMAP_REF2MODIS( NUMB_MODIS )
!     DATA ALBMAP_REF2MODIS /
!    &         1.0000,     2.0000,     3.0000,     4.0000,     5.0000,
!    &         6.0000,     7.0000,     8.0000,     9.0000,    10.0000,
!    &        11.0000,    12.0000,    13.0000,    14.0000,    15.0000,
!    &        16.0000,    17.0000,    18.0000,    18.0000,    18.0000/

!     REAL               :: ALBFAC_REF2MODIS( NUMB_MODIS )
!     DATA ALBFAC_REF2MODIS /
!    &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
!    &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
!    &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
!    &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000/

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
     &         1.0000,     2.0000,     3.0000,     4.0000,     5.0000,
     &         6.0000,     7.0000,     8.0000,     9.0000,    10.0000,
     &        11.0000,    12.0000,    13.0000,    14.0000,    15.0000,
     &        16.0000,    17.0000,    18.0000,    18.0000,    18.0000,
     &        -1.0000,    -1.0000,    -1.0000,    -1.0000,    -1.0000,
     &        -1.0000,    -1.0000,    -1.0000,    -1.0000,    -1.0000,
     &        13.0000,    13.0000,    13.0000/

      REAL               :: ALBFAC_REF2MODIS( NUMB_MODIS_NOAH )
      DATA ALBFAC_REF2MODIS /
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         0.0000,     0.0000,     0.0000,     0.0000,     0.0000,
     &         0.0000,     0.0000,     0.0000,     0.0000,     0.0000,
     &         1.0000,     1.0000,     1.0000 /


      INTEGER, PARAMETER :: NUMB_NLCD_MODIS =   50

      CHARACTER(60) NLCD_LANDUSE( NUMB_NLCD_MODIS )
      DATA NLCD_LANDUSE /
     & "Open Water", !                             'Open water',
     & "Perennial Ice-Snow", !                     'Perennial Ice/snow ',
     & "Developed Open Space", !                   'Developed, Open space ',
     & "Developed Low Intensity", !                'Developed, Low Intensity',
     & "Developed Medium Intensity", !             'Developed, Medium Intensity',
     & "Developed High Intensity", !               'Developed, High Intensity',
     & "Barren Land (Rock-Sand-Clay)", !           'Barren land',
     & "Unconsolidated Shore", !                   'Unconsolidated Shore',
     & "Deciduous Forest", !                       'Deciduous Forest',
     & "Evergreen Forest", !                       'Evergreen Forest',
     & "Mixed Forest", !                           'Mixed Forest',
     & "Dwarf Scrub", !                            'Dwarf Scrub',
     & "Shrub-Scrub", !                            'Shrub/Scrub',
     & "Grassland-Herbaceous", !                   'Grassland/Herbaceous',
     & "Sedge-Herbaceous", !                       'Sedge/Herbaceous',
     & "Lichens", !                                'Lichens',
     & "Moss", !                                   'Moss',
     & "Tundra", !                                 'Tundra',
     & "Pasture-Hay", !                            'Pasture/Hay',
     & "Cultivated Crops", !                       'Cultivated Crops',
     & "Woody Wetlands", !                         'Woody Wetland',
     & "Palustrine Forested Wetland", !            'Palustrine Forested Wetland',
     & "Palustrine Scrub-Shrub Wetland", !         'Palustrine Scrub/Shrub Wetland',
     & "Estuarine Forested Wetland", !             'Estuarine Forested Wetland',
     & "Estuarine Scrub-Shrub Wetland", !          'Estuarine Scrub/Shrub Wetland',
     & "Emergent Herbaceous Wetlands", !           'Emergent Herbaceous Wetland',
     & "Palustrine Emergent Wetland", !            'Palustrine Emergent Wetland',
     & "Estuarine Emergent Wetland", !             'Estuarine Emergent Wetland',
     & "Palustrine Aquatic Bed", !                 'Palustrine Aquatic Bed',
     & "Estuarine Aquatic Bed", !                  'Estuarine Aquatic Bed',
     & "Water", !                                  'Open water (mapped to 1)',
     & "Evergreen Needleleaf Forest", !            'Evergreen Needleleaf Forest',
     & "Evergreen Broadleaf Forest", !             'Evergreen Broadleaf Forest',
     & "Deciduous Needleleaf Forest", !            'Deciduous Needleleaf Forest',
     & "Deciduous Broadleaf Forest", !             'Deciduous Broadleaf Forest',
     & "Mixed Forests", !                          'Mixed Forest',
     & "Closed Shrublands", !                      'Closed Shrublands',
     & "Open Shrublands", !                        'Open Shrublands',
     & "Woody Savannas", !                         'Woody Savanna',
     & "Savannas", !                               'Savanna',
     & "Grasslands", !                             'Grasslands',
     & "Permanent Wetlands", !                     'Perminent Wetlands',
     & "Croplands", !                              'Croplands',
     & "Urban and Built Up", !                     'Urban andBuilt-up',
     & "Cropland-Natural Vegetation Mosaic", !     'Cropland/Natural Vegetation Mosaic',
     & "Permanent Snow and Ice", !                 'Snow and Ice',
     & "Barren or Sparsely Vegetated", !           'Barren or Sparsely Vegetated',
     & "IGBP Water", !                             'IGBP water',
     & "unclassified", !                           'unclassified',
     & "fill value" / !                             'fill value (normally ocean water)' /

      REAL               :: ALBMAP_REF2NLCD( NUMB_NLCD_MODIS )
      DATA ALBMAP_REF2NLCD /
     O        17.0000,    15.0000,    13.0000,    13.0000,    13.0000,
     +        13.0000,    16.0000,    16.0000,     4.0000,     1.0000,
     1         5.0000,     7.0000,     7.0000,     8.0000,     8.0000,
     +        10.0000,    10.0000,    18.0000,    14.0000,    12.0000,
     2        11.0000,    11.0000,    11.0000,    11.0000,    11.0000,
     +        11.0000,    11.0000,    11.0000,    17.0000,    17.0000,
     3        17.0000,     1.0000,     2.0000,     3.0000,     4.0000,
     +         5.0000,     6.0000,     7.0000,     8.0000,     9.0000,
     4        10.0000,    11.0000,    12.0000,    13.0000,    14.0000,
     +        15.0000,    16.0000,    17.0000,    17.0000,    17.0000/

      REAL               :: ALBFAC_REF2NLCD( NUMB_NLCD_MODIS )
      DATA ALBFAC_REF2NLCD /
     O         1.0000,     1.0000,     1.0667,     1.1333,     1.0667,
     +         1.1333,     0.8000,     1.4000,     0.9375,     0.8333,
     1         1.0000,     1.0000,     1.0000,     1.1364,     1.0455,
     +         1.0526,     1.0526,     1.0000,     1.2778,     1.1765,
     2         1.0714,     1.0714,     1.0714,     1.0714,     1.0714,
     +         1.2857,     1.2857,     1.2857,     1.2500,     1.2500,
     3         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     +         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     4         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     +         1.0000,     1.0000,     1.0000,     1.0000,     1.0000/

      CONTAINS
        SUBROUTINE INIT_CXQY_MODULE()

          USE BIN_DATA

          IMPLICIT NONE 

          LOGICAL, SAVE :: FIRSTCALL = .TRUE.
          INTEGER       :: I, J, K, ITTR
          CHARACTER(16) :: SAFE_NAME( NPHOTAB )
          CHARACTER(16) :: PHOT_NAME( NPHOTAB )
          CHARACTER(64) :: JTABLE_NAME
          CHARACTER(32) :: WORD
          CHARACTER(255)   EQNAME

          IF( FIRSTCALL )THEN

              CALL INIT_BIN_DATA
              FIRSTCALL = .FALSE.

          ELSE
       
              RETURN

          ENDIF

          WORD = MECHNAME
          

          EQNAME = 'OUT_DIR'

 
          CALL CONVERT_CASE ( WORD, .FALSE. )
          
          JTABLE_NAME = EQNAME( 1:LEN_TRIM( EQNAME ) )
     &               // '/' 
     &               // 'CSQY_DATA_' // TRIM( WORD )


          OPEN(JTABLE_UNIT, FILE = JTABLE_NAME, STATUS = 'UNKNOWN')

          PHOT_DONE   = ' '
          SAFE_NAME   = ' '
          NPHOT_DONE  = 0
          PHOT_PROCESS  = .TRUE.
          ITTR = 0
          
          DO J = 1, NPHOTAB
             DO I = 1, NPHOT_DONE
                IF( PHOTAB(J) .EQ. PHOT_DONE(I) )THEN
                    PHOT_PROCESS( J ) = .FALSE.
                    EXIT
                ENDIF
             ENDDO
             IF( PHOT_PROCESS( J ) )THEN
                 NPHOT_DONE = NPHOT_DONE + 1
                 PHOT_DONE(NPHOT_DONE) = PHOTAB(J)
                 SAFE_NAME(NPHOT_DONE) = PHOT_DONE(NPHOT_DONE)
                 DO I = 1, LEN(SAFE_NAME(NPHOT_DONE))
                    IF( SAFE_NAME(NPHOT_DONE)(I:I) .EQ. '-' ) THEN
                        SAFE_NAME(NPHOT_DONE)(I:I) = '_'
                    ENDIF
                 ENDDO
 
             ELSE
                 ITTR = ITTR + 1
                 WRITE(6,'(I3,1X,A,1X,A,I3,1X,A)')ITTR, TRIM(PHOTAB(J)),
     &                ' already treated by ', I, TRIM(PHOT_DONE(I))
             ENDIF
         ENDDO

         DO I = 1, NPHOT_DONE
            WRITE(JTABLE_UNIT,'(A16)')PHOT_DONE(I)
         ENDDO

         PHOT_DONE   = ' '
         NPHOT_DONE  = 0

1996     FORMAT(6X,'INTEGER, PARAMETER :: NPHOT_REF = ',I3,' ! # ref phot reactions ')
1997     FORMAT(6X,'INTEGER, PARAMETER :: NTEMP_REF = ',I3,' ! # ref temperatures ')
1998     FORMAT(6X,'INTEGER, PARAMETER :: NWL_REF   = ',I3,' ! # ref wavelengths ')
1999     FORMAT(6X,'INTEGER, PARAMETER :: I',A16, ' = ', I3, ' ! pointer to ', A16)
2040     FORMAT(6X,'CHARACTER(16), SAVE :: PNAME_REF( NPHOT_REF )')
2000     FORMAT(6X,'DATA PNAME_REF( I',A16, ' ) / ''', A16, ''' /')
2027     FORMAT(6X,'INTEGER, PARAMETER :: NPHOT_MAP = ',I3,' ! #  phot mapped reactions ')
2025     FORMAT(6X,'CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )')
2026     FORMAT(6X,'INTEGER, SAVE       :: PHOT_MAP( NPHOT_MAP )')
2010     FORMAT(6X,'DATA PNAME_MAP( ', I3, ' ), ', ' PHOT_MAP( ', I3, ' )  / ''',
     &             A16, ''', I', A16, ' / ')
2021     FORMAT(6X,'REAL, SAVE :: TEMP_REF( NTEMP_REF, NPHOT_REF )    ! reference temperatures')
2022     FORMAT(6X,'REAL, SAVE :: CS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective cross sections')
2023     FORMAT(6X,'REAL, SAVE :: QY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective quantum yields')
2024     FORMAT(6X,'REAL, SAVE :: ECS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! CS*QY averaged UCI Solar Flux')
2031     FORMAT(6X,'REAL, SAVE :: EQY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! eCS/CS averaged 77 bins in UCI Model')
2032     FORMAT('C...  quantum yields')
2033     FORMAT('C...    effective quantum yields were computed by performing separate')
2034     FORMAT('C...    interval integrations for the cross sections and for the') 
2035     FORMAT('C...    effective cross sections (cs*qy) (calculated on the finer')
2036     FORMAT('C...    wavelength grid.  The effective quantum yield values')
2037     FORMAT('C...    were then calculated for the 7 wavelength intervals by ')
2038     FORMAT('C...    dividing the effective cross sections by the interval average')
2039     FORMAT('C...    cross sections (eQY=eCS/CS).')
2003     FORMAT(1x)

          RETURN
        END SUBROUTINE INIT_CXQY_MODULE
	END MODULE JPROC_PROFILE
