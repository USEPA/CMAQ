      program veg

! PURPOSE:      For specified photolysis module ("INLINE", or "SPHERAD"),
!               and for specified solar zenith angle (sza), Julian day (jd), and
!               snow flag (snowflag), returns reflectances (spectral albedos) for 
!               each of 3 landuse categorization schemes (NLCD, USGS, and MODIS) 
!               that are specific to the solar zenith angle, Julian day, snow 
!               conditions,and the spectral bin structure of the photolysis module.

! ADVISORY:	This module should be called when sza, jd, or snowflag changes

! DEVELOPED:	August 2010. John Streicher USEPA/ORD/NERL/AMAD

! INPUT: 	rtmodule,sza[deg],jd [integer],snowflag [0=no;1=yes]
! OUTPUT:	reflectance
! CALLS:	reflectances,season_correction,spectralalbedo,mapalbedo
! CALLED BY:	main

      implicit none
      character*7 rtmodule
      integer jd,snowflag,n,isza,i
     
      real*4 sza,specalbsza(20,15,2),specalbszassn(20,15,2)
     &      ,bin_spectra(50,50,2),nlcd_spectra(50,50,2)
     &      ,usgs_spectra(50,50,2),modis_spectra(50,50,2)

! The following (sza; rtmodule='INLINE', or 'SPHERAD'; jd; snowflag) 
! are temporarily hardcoded.  
! Replace with user-selected values when incorporated into MCIP or WRF-CMAQ.


      CHARACTER(80)           :: LAND_SCHEME       ! land use scheme used 
      INTEGER, PARAMETER      :: NUMB_REF     = 20 ! # of land categories in reference
      INTEGER, PARAMETER      :: NUMB_SCHEME  = 50 ! # of land categories in LAND_SCHEME
      REAL, ALLOCATABLE       :: ALBEDO_REF_TO_SCHEME( :, :)

      INTERFACE
        SUBROUTINE ALBEDO_MAP(LAND_SCHEME, NUMB_REF, NUMB_SCHEME, ALBEDO_REF_TO_SCHEME)
          IMPLICIT NONE
          CHARACTER(80), INTENT( IN )  :: LAND_SCHEME   ! land use scheme used 
          INTEGER,       INTENT( IN )  :: NUMB_REF      ! # of land categories in reference
          INTEGER,       INTENT( IN )  :: NUMB_SCHEME   ! # of land categories in LAND_SCHEME
          REAL,          INTENT( OUT ) :: ALBEDO_REF_TO_SCHEME( :, :)
        END SUBROUTINE ALBEDO_MAP
      END INTERFACE

      rtmodule = 'INLINE'
      sza = 30.     
      jd = 180
      snowflag = 0
  
      call reflectances(sza
     &            ,specalbsza)

      call season_correction(specalbsza,jd,snowflag
     &                      ,specalbszassn)

      call spectralalbedo(specalbszassn,rtmodule
     &                   ,n,bin_spectra )
     
      bin_spectra = 0.0

      call mapalbedo(n,bin_spectra
     &              ,nlcd_spectra,usgs_spectra,modis_spectra )

      ALLOCATE( ALBEDO_REF_TO_SCHEME( NUMB_SCHEME, NUMB_REF) )


      LAND_SCHEME  = 'NLCD-MODIS'         
      CALL ALBEDO_MAP(LAND_SCHEME, NUMB_REF, NUMB_SCHEME, ALBEDO_REF_TO_SCHEME)

      LAND_SCHEME = 'USGS24'
      CALL ALBEDO_MAP(LAND_SCHEME, NUMB_REF, NUMB_SCHEME, ALBEDO_REF_TO_SCHEME)

      LAND_SCHEME = 'MODIS'
      CALL ALBEDO_MAP(LAND_SCHEME, NUMB_REF, NUMB_SCHEME, ALBEDO_REF_TO_SCHEME)

      
      stop
      end
!***********************************************************************            
      SUBROUTINE SPECTRAL_REFLECT_DRAFT(NWAVE, WAVE, NUSES, ALBEDO, LANDUSE, 
     &                            ZENITH_COEFF, SEASON_COEFF, SNOW_COEFF)

! PURPOSE: Populate array ALBEDO with wavelength dependent reflectances (i.e.
!	     spectral albedos) for 20 MODIS land use types.  Also returns correction factors
!          for solar zenith angle and season. Note that an adjustment is applied to 
!          better match spectral estimates for each land use type.

! REFERENCE:  CERES/SARB Surface Properties database, May 2006
!             http://www-surf/larc.nasa.gov/surf/pages/data-page.html

! INPUT:  	SOLAR_ZENITH [deg]
! OUTPUT: 	NWAVE       
!             ALBEDO(20,15)
! CALLS:  	none
! CALLED BY: 	veg
      


      implicit none
      INTEGER, PARAMETER  :: NUMB_LANDUSE_MODIS = 20
      INTEGER, PARAMETER  :: NUMB_BANDS_MODIS   = 15


      INTEGER NWAVE
      REAL    WAVE(NUMB_BANDS_MODIS)

      INTEGER NUSES
      REAL    ALBEDO(NUMB_LANDUSE_MODIS,NUMB_BANDS_MODIS)

      CHARACTER(30) LANDUSE(NUMB_LANDUSE_MODIS)

      REAL ZENITH_COEFF( NUMB_LANDUSE_MODIS )
      REAL SNOW_COEFF( NUMB_LANDUSE_MODIS )
      REAL SEASON_COEFF( NUMB_LANDUSE_MODIS )

      REAL(4) SPECTRAL_ALBEDO(NUMB_BANDS_MODIS,NUMB_LANDUSE_MODIS),
     &        dterm(NUMB_LANDUSE_MODIS),
     &        ALBEDO_WAVE(NUMB_BANDS_MODIS),
     &        specenhancement,u0,SOLAR_ZENITH,MODIS2SPECTRAL(NUMB_LANDUSE_MODIS)

      REAL( 8 ) :: PI

      REAL( 4 ) :: ZENITH_CORRECTION

      REAL( 4 ) :: WVBAND( NUMB_BANDS_MODIS )
      DATA WVBAND / 
     &     200.05,  234.3,  264.8,   292.1,  310.5,
     &      340.0,  397.5,  467.5,  546.25,  642.3,
     &     1000.0, 1550.0, 2150.0,  3000.0, 3750.0 /

      integer i,j,k


! *** Spectral Reflectances

      DATA (SPECTRAL_ALBEDO(IWL, 1), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.032, 0.032, 0.032, 0.032, 0.032  ! (1) EVERGREEN NEEDLE FOREST
     &  , 0.032, 0.032, 0.032, 0.046, 0.046  
     &  , 0.235, 0.096, 0.038, 0.038, 0.038 /

      DATA (SPECTRAL_ALBEDO(IWL, 2), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.044, 0.044, 0.044, 0.044, 0.044  ! ( 2) EVERGREEN BROADLEAF FOR
     &  , 0.044, 0.044, 0.044, 0.044, 0.044  !      (Tropical Forest)
     &  , 0.234, 0.193, 0.112, 0.112, 0.112  /

      DATA (SPECTRAL_ALBEDO(IWL, 3), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.032, 0.032, 0.032, 0.032, 0.032  ! ( 3) DECIDUOUS NEEDLE FOREST
     &  , 0.032, 0.032, 0.032, 0.046, 0.046 
     &  , 0.235, 0.096, 0.038, 0.038, 0.038  / 

      DATA (SPECTRAL_ALBEDO(IWL, 4), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.034, 0.034, 0.034, 0.034, 0.034  ! ( 4) DECIDUOUS BROADLEAF FOR
     &  , 0.034, 0.034, 0.034, 0.066, 0.067  
     &  , 0.312, 0.276, 0.160, 0.160, 0.160  / 

      DATA (SPECTRAL_ALBEDO(IWL, 5), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.033, 0.033, 0.033, 0.033, 0.033  ! ( 5) MIXED FOREST
     &  , 0.033, 0.033, 0.033, 0.056, 0.057  
     &  , 0.274, 0.186, 0.099, 0.099, 0.099  /

      DATA (SPECTRAL_ALBEDO(IWL, 6), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017  ! ( 6) CLOSED SHRUBS
     &  , 0.020, 0.036, 0.045, 0.055, 0.156   
     &  , 0.350, 0.239, 0.101, 0.101, 0.101  /

      DATA (SPECTRAL_ALBEDO(IWL, 7), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.095, 0.095, 0.095, 0.095, 0.095  ! ( 7) OPEN/SHRUBS
     &  , 0.095, 0.098, 0.104, 0.122, 0.157    
     &  , 0.231, 0.330, 0.311, 0.150, 0.150  /

      DATA (SPECTRAL_ALBEDO(IWL, 8), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.020, 0.020, 0.020, 0.023, 0.024  ! ( 8) WOODY SAVANNA (Decid Broadleaf*0.4 + Grass*0.6)
     &  , 0.026, 0.035, 0.041, 0.102, 0.104 
     &  , 0.366, 0.291, 0.151, 0.107, 0.107  /

      DATA (SPECTRAL_ALBEDO(IWL, 9), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017  ! ( 9) SAVANNA  
     &  , 0.020, 0.036, 0.045, 0.126, 0.129  
     &  , 0.402, 0.301, 0.145, 0.071, 0.071  /

      DATA (SPECTRAL_ALBEDO(IWL, 10), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017  ! (10) GRASSLAND 
     &  , 0.020, 0.036, 0.045, 0.126, 0.129  
     &  , 0.402, 0.301, 0.145, 0.071, 0.071  /

      DATA (SPECTRAL_ALBEDO(IWL, 11), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.039, 0.039, 0.039, 0.039, 0.039  ! (11) PERMANENT WETLANDS
     &  , 0.039, 0.039, 0.039, 0.051, 0.071  
     &  , 0.164, 0.100, 0.056, 0.056, 0.056  /

      DATA (SPECTRAL_ALBEDO(IWL, 12), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017  ! (12) CROPLAND 
     &  , 0.020, 0.036, 0.045, 0.115, 0.099  
     &  , 0.442, 0.271, 0.122, 0.059, 0.059  / 

      DATA (SPECTRAL_ALBEDO(IWL, 13), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.052, 0.052, 0.052, 0.052, 0.052  ! (13) URBAN
     &  , 0.052, 0.052, 0.066, 0.104, 0.114  
     &  , 0.304, 0.258, 0.258, 0.258, 0.258  /

      DATA (SPECTRAL_ALBEDO(IWL, 14), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.010, 0.010, 0.010, 0.015, 0.017  ! (14) CROP MOSAIC
     &  , 0.020, 0.036, 0.045, 0.090, 0.083  
     &  , 0.377, 0.273, 0.141, 0.110, 0.110  /

      DATA (SPECTRAL_ALBEDO(IWL, 15), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.910, 0.910, 0.910, 0.916, 0.921  ! (15) PERMANENT SNOW 
     &  , 0.931, 0.947, 0.964, 0.953, 0.920   
     &  , 0.635, 0.013, 0.006, 0.009, 0.014  / 

      DATA (SPECTRAL_ALBEDO(IWL, 16), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.144, 0.144, 0.144, 0.144, 0.144  ! (16) BARREN/DESERT
     &  , 0.144, 0.144, 0.179, 0.263, 0.331  
     &  , 0.405, 0.390, 0.390, 0.390, 0.390  /

      DATA (SPECTRAL_ALBEDO(IWL, 17), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.066, 0.066, 0.066, 0.070, 0.073  ! (17) OCEAN WATER
     &  , 0.082, 0.094, 0.091, 0.078, 0.072  
     &  , 0.066, 0.062, 0.055, 0.044, 0.069  /

      DATA (SPECTRAL_ALBEDO(IWL, 18), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.180, 0.180, 0.180, 0.180, 0.180  ! (18) TUNDRA  (modified with Lundberg et al)
     &  , 0.180, 0.180, 0.180, 0.180, 0.180  
     &  , 0.247, 0.265, 0.265, 0.265, 0.265  /

      DATA (SPECTRAL_ALBEDO(IWL, 19), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.979, 0.979, 0.979, 0.980, 0.982  ! (19) FRESH SNOW 
     &  , 0.984, 0.988, 0.992, 0.989, 0.982  
     &  , 0.902, 0.143, 0.168, 0.019, 0.015  /  

      DATA (SPECTRAL_ALBEDO(IWL, 20), IWL = 1, NUMB_BANDS_MODIS)  /                       
     &    0.778, 0.778, 0.778, 0.778, 0.778  ! (20) SEA ICE
     &  , 0.778, 0.778, 0.778, 0.778, 0.752  
     &  , 0.393, 0.055, 0.054, 0.036, 0.036  /


! *** Land-use type-dependent solar zenith adjustment factor 

     
! The following scale factors are applied to the spectral albedos to bring
! them into closer agreement (in the visible spectrum) with the MODIS fixed albedos      
     
      data MODIS2SPECTRAL /
     &   3.5,   ! (1)
     &   2.5,   ! (2)
     &   4.0,   ! (3)
     &   2.5,   ! (4)
     &   3.0,   ! (5)
     &   3.0,   ! (6)
     &   2.0,   ! (7)
     &   2.5,   ! (8)
     &   1.5,   ! (9)
     &   2.0,   ! (10)
     &   2.0,   ! (11)
     &   1.5,   ! (12)
     &   1.5,   ! (13)
     &   2.5,   ! (14)
     &   1.0,   ! (15)
     &   1.0,   ! (16)
     &   1.0,   ! (17)
     &   1.0,   ! (18)
     &   1.0,   ! (19)
     &   1.0 /  ! (20)

      INTEGER   :: IWL

      NWAVE = NUMB_BANDS_MODIS
      NUSES = NUMB_LANDUSE_MODIS

      ALBEDO = 0.0

      pi = dacos(-1.d0)


       LANDUSE( 1:NUSES ) = ( /
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
     &,'SEA ICE' / )
 

       SEASON_COEFF( 1:NUSES )  = (/
     &   0.73    ! EVERGREEN NEEDLE FOR
     &,  0.84    ! EVERGREEN BROADLEAF FOR
     &,  0.77    ! DECIDUOUS NEEDLE FOR
     &,  0.73    ! DECIDOUS BROAD FOR
     &,  0.78    ! MIXED FOREST
     &,  0.73    ! CLOSED SHRUBS
     &,  0.77    ! OPEN/SHRUBS
     &,  0.78    ! WOODY SAVANNA
     &,  0.78    ! SAVANNA
     &,  0.79    ! GRASSLAND
     &,  0.78    ! WETLAND
     &,  0.80    ! CROPLAND
     &,  0.67    ! URBAN
     &,  0.80    ! CROP MOSAIC
     &,  1.00    ! PERMANENT SNOW
     &,  0.96    ! BARREN/DESERT
     &,  1.00    ! OCEAN WATER
     &,  0.25    ! TUNDRA		(modified with Lundberg et al (2005))
     &,  1.00    ! FRESH SNOW
     &,  1.00 /) ! SEA ICE

        ZENITH_COEFF( 1:NUSES )  = (/
     &  0.40,   ! ( 1) EVERGREEN NEEDLE FOR 
     &  0.44,   ! ( 2) EVERGREEN BROAD FOR 
     &  0.32,   ! ( 3) DECIDUOUS NEEDLE FOR
     &  0.39,   ! ( 4) DECIDUOUS BROAD FOR
     &  0.22,   ! ( 5) MIXED FOREST
     &  0.28,   ! ( 6) CLOSED SHRUBS
     &  0.40,   ! ( 7) OPEN/SHRUBS
     &  0.47,   ! ( 8) WOODY SAVANNA
     &  0.53,   ! ( 9) SAVANNA
     &  0.53,   ! (10) GRASSLAND
     &  0.35,   ! (11) WETLAND
     &  0.41,   ! (12) CROPLAND (CAGEX-APR)
     &  0.10,   ! (13) URBAN
     &  0.40,   ! (14) CROP MOSAIC
     &  0.10,   ! (15) ANTARCTIC SNOW
     &  0.40,   ! (16) BARREN/DESERT
     &  0.41,   ! (17) OCEAN WATER
     &  0.58,   ! (18) TUNDRA
     &  0.10,   ! (19) FRESH SNOW
     &  0.10 /) ! (20) SEA ICE

       SNOW_COEFF( 1:NUSES ) =  (/
     &   3.0   ! EVERGREEN NEEDLE FOR
     &,  4.1   ! EVERGREEN BROADLEAF FOR
     &,  3.1   ! DECIDUOUS NEEDLE FOR
     &,  2.7   ! DECIDUOUS BROADLEAF FOR
     &,  3.0   ! MIXED FOREST
     &,  2.2   ! CLOSED SHRUBS
     &,  3.6   ! OPEN/SHRUBS
     &,  2.1   ! WOODY SAVANNA
     &,  3.0   ! SAVANNA
     &,  3.8   ! GRASSLAND
     &,  4.9   ! WETLAND
     &,  4.5   ! CROPLAND
     &,  3.6   ! URBAN
     &,  3.6   ! CROP MOSAIC
     &,  1.7   ! PERMANENT SNOW
     &,  3.5   ! BARREN/DESERT
     &,  1.0   ! OCEAN WATER
     &,  4.0   ! TUNDRA		(modified with Lundberg et al (2005))
     &,  1.0   ! FRESH SNOW
     &,  1.3 /)! SEA ICE
     
!The folowing loops indicate use LUC (as "j), and band (as "i")

       u0           = 0.5 ! dcos((pi/180.d0)*SOLAR_ZENITH)

       SOLAR_ZENITH = PI/180.0D0*DACOS(0.5D0)

       DO 270 J = 1,20
       
        DO 250 I = 1,15

         ZENITH_CORRECTION = (1.0 + ZENITH_COEFF(j))
     &                     / (1.0 + 2.0*u0*ZENITH_COEFF(j))

         ZENITH_CORRECTION = MAX(0.8,ZENITH_CORRECTION)

         ALBEDO_WAVE(I) = WVBAND(I)
         ALBEDO(J, I) = MIN(1.0, (ZENITH_CORRECTION*SPECTRAL_ALBEDO(I, J)))

         IF((modis2spectral(j)*ALBEDO(J, I)).LE. 1.0)THEN
          ALBEDO(J, I) = modis2spectral(j)*ALBEDO(J, I)
         ELSE
          ALBEDO(J ,I) = 1.0
         ENDIF
	 
250     continue
270    continue

!Output is specalbsza(j,i,2): 20 spectral albedos x 15 bands

      return
      end
!***********************************************************************            
      subroutine reflectances(sza
     &                  ,specalbsza)

! PURPOSE: Populate array specalbsza with sza-specific reflectances (i.e.
!	   spectral albedos) for each of 20 land use types.  Correct for
!          solar zenith angle, and adjust spectral albedos to better match
!          MODIS non-spectral estimates for each MODIS land use type.

! REFERENCE:  CERES/SARB Surface Properties database, May 2006
!             http://www-surf/larc.nasa.gov/surf/pages/data-page.html

! INPUT:  	sza[deg]
! OUTPUT: 	specalbsza(20,15,2)
! CALLS:  	none
! CALLED BY: 	veg
      


      implicit none
      character*30 landuse(20)
      integer i,j,k
      real*4 bands(15),specalb(15,20),dterm(20),specalbsza(20,15,2)
     &      ,pi,specenhancement,u0,sza,modis2spectral(20)

      specalbsza(1:20,1:15,1:2) = 0.0

      pi = dacos(-1.d0)


      data bands / 
     &     200.05, 234.3,  264.8,  292.1,  310.5
     &  ,  340.0,  397.5,  467.5,  546.25, 642.3
     &  , 1000.0, 1550.0, 2150.0, 3000.0, 3750.0 /

! *** Spectral Reflectances

      data specalb /                       
     &    0.032, 0.032, 0.032, 0.032, 0.032  ! (1) EVERGREEN NEEDLE FOREST
     &  , 0.032, 0.032, 0.032, 0.046, 0.046  
     &  , 0.235, 0.096, 0.038, 0.038, 0.038 
     &  , 0.044, 0.044, 0.044, 0.044, 0.044  ! ( 2) EVERGREEN BROADLEAF FOR
     &  , 0.044, 0.044, 0.044, 0.044, 0.044  !      (Tropical Forest)
     &  , 0.234, 0.193, 0.112, 0.112, 0.112  
     &  , 0.032, 0.032, 0.032, 0.032, 0.032  ! ( 3) DECIDUOUS NEEDLE FOREST
     &  , 0.032, 0.032, 0.032, 0.046, 0.046 
     &  , 0.235, 0.096, 0.038, 0.038, 0.038
     &  , 0.034, 0.034, 0.034, 0.034, 0.034  ! ( 4) DECIDUOUS BROADLEAF FOR
     &  , 0.034, 0.034, 0.034, 0.066, 0.067  
     &  , 0.312, 0.276, 0.160, 0.160, 0.160 
     &  , 0.033, 0.033, 0.033, 0.033, 0.033  ! ( 5) MIXED FOREST
     &  , 0.033, 0.033, 0.033, 0.056, 0.057  
     &  , 0.274, 0.186, 0.099, 0.099, 0.099
     &  , 0.010, 0.010, 0.010, 0.015, 0.017  ! ( 6) CLOSED SHRUBS
     &  , 0.020, 0.036, 0.045, 0.055, 0.156   
     &  , 0.350, 0.239, 0.101, 0.101, 0.101
     &  , 0.095, 0.095, 0.095, 0.095, 0.095  ! ( 7) OPEN/SHRUBS
     &  , 0.095, 0.098, 0.104, 0.122, 0.157    
     &  , 0.231, 0.330, 0.311, 0.150, 0.150
     &  , 0.020, 0.020, 0.020, 0.023, 0.024  ! ( 8) WOODY SAVANNA (Decid Broadleaf*0.4 + Grass*0.6)
     &  , 0.026, 0.035, 0.041, 0.102, 0.104 
     &  , 0.366, 0.291, 0.151, 0.107, 0.107
     &  , 0.010, 0.010, 0.010, 0.015, 0.017  ! ( 9) SAVANNA  
     &  , 0.020, 0.036, 0.045, 0.126, 0.129  
     &  , 0.402, 0.301, 0.145, 0.071, 0.071
     &  , 0.010, 0.010, 0.010, 0.015, 0.017  ! (10) GRASSLAND 
     &  , 0.020, 0.036, 0.045, 0.126, 0.129  
     &  , 0.402, 0.301, 0.145, 0.071, 0.071
     &  , 0.039, 0.039, 0.039, 0.039, 0.039  ! (11) PERMANENT WETLANDS
     &  , 0.039, 0.039, 0.039, 0.051, 0.071  
     &  , 0.164, 0.100, 0.056, 0.056, 0.056
     &  , 0.010, 0.010, 0.010, 0.015, 0.017  ! (12) CROPLAND 
     &  , 0.020, 0.036, 0.045, 0.115, 0.099  
     &  , 0.442, 0.271, 0.122, 0.059, 0.059 
     &  , 0.052, 0.052, 0.052, 0.052, 0.052  ! (13) URBAN
     &  , 0.052, 0.052, 0.066, 0.104, 0.114  
     &  , 0.304, 0.258, 0.258, 0.258, 0.258
     &  , 0.010, 0.010, 0.010, 0.015, 0.017  ! (14) CROP MOSAIC
     &  , 0.020, 0.036, 0.045, 0.090, 0.083  
     &  , 0.377, 0.273, 0.141, 0.110, 0.110
     &  , 0.910, 0.910, 0.910, 0.916, 0.921  ! (15) PERMANENT SNOW 
     &  , 0.931, 0.947, 0.964, 0.953, 0.920   
     &  , 0.635, 0.013, 0.006, 0.009, 0.014 
     &  , 0.144, 0.144, 0.144, 0.144, 0.144  ! (16) BARREN/DESERT
     &  , 0.144, 0.144, 0.179, 0.263, 0.331  
     &  , 0.405, 0.390, 0.390, 0.390, 0.390
     &  , 0.066, 0.066, 0.066, 0.070, 0.073  ! (17) OCEAN WATER
     &  , 0.082, 0.094, 0.091, 0.078, 0.072  
     &  , 0.066, 0.062, 0.055, 0.044, 0.069
     &  , 0.180, 0.180, 0.180, 0.180, 0.180  ! (18) TUNDRA  (modified with Lundberg et al)
     &  , 0.180, 0.180, 0.180, 0.180, 0.180  
     &  , 0.247, 0.265, 0.265, 0.265, 0.265
     &  , 0.979, 0.979, 0.979, 0.980, 0.982  ! (19) FRESH SNOW 
     &  , 0.984, 0.988, 0.992, 0.989, 0.982  
     &  , 0.902, 0.143, 0.168, 0.019, 0.015  
     &  , 0.778, 0.778, 0.778, 0.778, 0.778  ! (20) SEA ICE
     &  , 0.778, 0.778, 0.778, 0.778, 0.752  
     &  , 0.393, 0.055, 0.054, 0.036, 0.036 /

      data landuse /
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


! *** Land-use type-dependent solar zenith adjustment factor 


      data dterm /
     &  0.40   ! ( 1) EVERGREEN NEEDLE FOR 
     &, 0.44   ! ( 2) EVERGREEN BROAD FOR 
     &, 0.32   ! ( 3) DECIDUOUS NEEDLE FOR
     &, 0.39   ! ( 4) DECIDUOUS BROAD FOR
     &, 0.22   ! ( 5) MIXED FOREST
     &, 0.28   ! ( 6) CLOSED SHRUBS
     &, 0.40   ! ( 7) OPEN/SHRUBS
     &, 0.47   ! ( 8) WOODY SAVANNA
     &, 0.53   ! ( 9) SAVANNA
     &, 0.53   ! (10) GRASSLAND
     &, 0.35   ! (11) WETLAND
     &, 0.41   ! (12) CROPLAND (CAGEX-APR)
     &, 0.10   ! (13) URBAN
     &, 0.40   ! (14) CROP MOSAIC
     &, 0.10   ! (15) ANTARCTIC SNOW
     &, 0.40   ! (16) BARREN/DESERT
     &, 0.41   ! (17) OCEAN WATER
     &, 0.58   ! (18) TUNDRA
     &, 0.10   ! (19) FRESH SNOW
     &, 0.10 / ! (20) SEA ICE
     
! The following scale factors are applied to the spectral albedos to bring
! them into closer agreement (in the visible spectrum) with the MODIS fixed albedos      
     
      data modis2spectral /
     &   3.5   ! (1)
     &,  2.5   ! (2)
     &,  4.0   ! (3)
     &,  2.5   ! (4)
     &,  3.0   ! (5)
     &,  3.0   ! (6)
     &,  2.0   ! (7)
     &,  2.5   ! (8)
     &,  1.5   ! (9)
     &,  2.0   ! (10)
     &,  2.0   ! (11)
     &,  1.5   ! (12)
     &,  1.5   ! (13)
     &,  2.5   ! (14)
     &,  1.0   ! (15)
     &,  1.0   ! (16)
     &,  1.0   ! (17)
     &,  1.0   ! (18)
     &,  1.0   ! (19)
     &,  1.0 / ! (20)
     
!The folowing loops indicate use LUC (as "j), and band (as "i")

       u0 = dcos((pi/180.d0)*sza)

       do 270 j = 1,20
       
        do 250 i = 1,15

         specenhancement = (1.0 + dterm(j))/(1.0 + 2.0*u0*dterm(j))
         specenhancement = max(0.8,specenhancement)

         specalbsza(j,i,1) = bands(i)
	 specalbsza(j,i,2) = min(1.0,(specenhancement*specalb(i,j)))
         if((modis2spectral(j)*specalbsza(j,i,2)).le.1.0)then
          specalbsza(j,i,2) = modis2spectral(j)*specalbsza(j,i,2)
         else
	  specalbsza(j,i,2) = 1.0
	 endif
	 
250     continue
270    continue

!Output is specalbsza(j,i,2): 20 spectral albedos x 15 bands

      return
      end
!***********************************************************************
      subroutine season_correction(specalbsza,jd,snowflag
     &                            ,specalbszassn)

!  PURPOSE:    Modify the sza-specific spectral albedos to be further
!              specific to Julian day

!  REFERENCE:  Moody et al (2005); Moody et al (2007); Jin et al (2002)

!  INPUT:      specalbsza,jd,snowflag
!  OUTPUT:     specalbszassn
!  CALLS:      none
!  CALLED BY:  veg

      implicit none
      integer jd,i,j,snowflag
      real*4 specalbsza(20,15,2),specalbszassn(20,15,2)
     &       ,ssn_correction(20),jd_correction(20),pi,scale,temp
     &       ,snow_correction(20)

      specalbszassn(1:20,1:15,1:2) = 0.0
      jd_correction(1:20) = 0.0

      pi = dacos(-1.d0)

      data ssn_correction /
     &   0.73  ! EVERGREEN NEEDLE FOR
     &,  0.84  ! EVERGREEN BROADLEAF FOR
     &,  0.77  ! DECIDUOUS NEEDLE FOR
     &,  0.73  ! DECIDOUS BROAD FOR
     &,  0.78  ! MIXED FOREST
     &,  0.73  ! CLOSED SHRUBS
     &,  0.77  ! OPEN/SHRUBS
     &,  0.78  ! WOODY SAVANNA
     &,  0.78  ! SAVANNA
     &,  0.79  ! GRASSLAND
     &,  0.78  ! WETLAND
     &,  0.80  ! CROPLAND
     &,  0.67  ! URBAN
     &,  0.80  ! CROP MOSAIC
     &,  1.00  ! PERMANENT SNOW
     &,  0.96  ! BARREN/DESERT
     &,  1.00  ! OCEAN WATER
     &,  0.25  ! TUNDRA		(modified with Lundberg et al (2005))
     &,  1.00  ! FRESH SNOW
     &,  1.00 /! SEA ICE

! scale ranges from [0,1]; scale = 0 on ~Jun21; = 1 on Dec21
      temp = cos(2.0*pi*((real(jd)+11.0)/365.))
      if(temp.ge.0.d0)then
       scale = 0.5*(1.+ sqrt(temp))
      else
       scale = 0.5*(1.- sqrt(abs(temp)))
      endif

      do 90 i = 1,20
       jd_correction(i) = scale*ssn_correction(i) - (scale-1.0)*1.0
90    continue
   
      do 190 i = 1,20
       do 170 j = 1,15
        specalbszassn(i,j,1) = specalbsza(i,j,1)
     
         specalbszassn(i,j,2) = specalbsza(i,j,2)/jd_correction(i)
     
170    continue
190   continue

      if(snowflag.eq.0)goto 999

      data snow_correction /
     &   3.0  ! EVERGREEN NEEDLE FOR
     &,  4.1  ! EVERGREEN BROADLEAF FOR
     &,  3.1  ! DECIDUOUS NEEDLE FOR
     &,  2.7  ! DECIDUOUS BROADLEAF FOR
     &,  3.0  ! MIXED FOREST
     &,  2.2  ! CLOSED SHRUBS
     &,  3.6  ! OPEN/SHRUBS
     &,  2.1  ! WOODY SAVANNA
     &,  3.0  ! SAVANNA
     &,  3.8  ! GRASSLAND
     &,  4.9  ! WETLAND
     &,  4.5  ! CROPLAND
     &,  3.6  ! URBAN
     &,  3.6  ! CROP MOSAIC
     &,  1.7  ! PERMANENT SNOW
     &,  3.5  ! BARREN/DESERT
     &,  1.0  ! OCEAN WATER
     &,  4.0  ! TUNDRA		(modified with Lundberg et al (2005))
     &,  1.0  ! FRESH SNOW
     &,  1.3 /! SEA ICE

      do 290 i = 1,20
       do 270 j = 1,15
        specalbszassn(i,j,2) = snow_correction(i)*specalbszassn(i,j,2)
        if(specalbszassn(i,j,2).gt.1.)then
         specalbszassn(i,j,2) = 1.0
        endif

270    continue
290   continue

999   return
      end        
!***********************************************************************      
      subroutine spectralalbedo(specalbszassn,rtmodule
     &                         ,n,bin_spectra )
!   PURPOSE:   Bin the sza-,season-specific spectral albedo data into the wavelength
!              bins of the selected photolysis module

!   INPUT:     specalbszassn,rtmodule
!   OUTPUT:    bin_spectra
!   CALLS:     get_bin_data,cutpoints,assign
!   CALLED BY: veg

      implicit none
      character rtmodule*7
    

      integer choice,n,ncat,countforlabel(50)

      real*4 wavebin(50),cutpoint(50),specalbszassn(20,15,2)
     &      ,nlcd_spectra(50,200,2)
     &      ,bin_spectra(50,50,2),n2,sza

      bin_spectra(1:50,1:50,1:2) = 0.0


      call get_bin_data(rtmodule
     &                 ,wavebin,n)

      call cutpoints(wavebin,n,rtmodule
     &              ,cutpoint)

      call assign(cutpoint,n,specalbszassn,ncat,countforlabel,wavebin
     &           ,bin_spectra)

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine assign(cutpoint,n,specalbszassn,ncat,countforlabel
     &                 ,wavebin
     &                 ,bin_spectra)
      implicit none
      integer n,ncat,countforlabel(50),i,j,k,kcount,jj,zeroflag
      real*4 cutpoint(50),specalbszassn(20,15,2),wavebin(50)
     &      ,bin_spectra(50,50,2)
     &      ,accumulate1,accumulate2
      do 190 i = 1,20
       do 170 j = 2,n+1
        kcount = 0
        accumulate1 = 0.d0
        accumulate2 = 0.d0
        do 150 k = 1,15
         if((specalbszassn(i,k,1).le.cutpoint(j)).and.
     &      (specalbszassn(i,k,1).gt.cutpoint(j-1)))then
          kcount = kcount + 1
          accumulate2 = accumulate2 + specalbszassn(i,k,2)
         endif
150     continue
        if(kcount.ne.0)then
         accumulate2 = accumulate2/real(kcount)
         bin_spectra(i,j,1) = wavebin(j-1)
         bin_spectra(i,j,2) = accumulate2      
        elseif(kcount.eq.0)then
         bin_spectra(i,j,1) = wavebin(j-1)
         bin_spectra(i,j,2) = 0.d0
        endif

160     format(2f12.3,i6,f12.3)
170    continue
190   continue

!     First bin
      do 230 i = 1,20
       if(bin_spectra(i,2,2).le.1.d-5)then
        do 220 k = 1,15
         if(specalbszassn(i,k,1).gt.cutpoint(1))then
          bin_spectra(i,2,2) = specalbszassn(i,k-1,2)
          goto 225
         endif
220     continue
225     if(bin_spectra(i,3,2).ne.0.d0)then
         bin_spectra(i,2,2) = (bin_spectra(i,2,2)
     &                        +bin_spectra(i,3,2))/2.d0
        endif
       endif
230   continue

!     Last bin
      do 250 i = 1,20
       if(bin_spectra(i,n+1,2).le.1.d-5)then
        do 240 k = 15,1,-1
         if(specalbszassn(i,k,1).lt.cutpoint(n))then
          bin_spectra(i,n+1,2) = specalbszassn(i,k+1,2)
          goto 245
         endif
240     continue
245     if(bin_spectra(i,n,2).ne.0.d0)then
         bin_spectra(i,n+1,2) = (bin_spectra(i,n+1,2)
     &                          +bin_spectra(i,n,2))/2.d0
        endif
       endif      
250   continue

      do 290 i = 1,20
255    zeroflag = 0
       do 270 j = 2,n-1  
        if(bin_spectra(i,j,2).eq.0.d0)then
         zeroflag = 1
         if((bin_spectra(i,j-1,2).ne.0.d0).and.
     &      (bin_spectra(i,j+1,2).ne.0.d0))then
          bin_spectra(i,j,2)=(bin_spectra(i,j-1,2)
     &                       +bin_spectra(i,j+1,2))/2.d0
          goto 270
         elseif(bin_spectra(i,j-1,2).ne.0.d0)then
          bin_spectra(i,j,2) = bin_spectra(i,j-1,2)
          goto 270
         elseif(bin_spectra(i,j+1,2).ne.0.d0)then
          bin_spectra(i,j,2) = bin_spectra(i,j+1,2)
          goto 270
         endif
        
        endif
270    continue
       if(zeroflag.eq.1)goto 255
290   continue

      do 390 i = 1,20
       do 370 j = 2,n+1
        bin_spectra(i,j-1,1) = bin_spectra(i,j,1)
        bin_spectra(i,j-1,2) = bin_spectra(i,j,2)

360     format(2i6,2f12.3)
370    continue
390   continue

      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cutpoints(wavebin,n,rtmodule
     &                    ,cutpoint)
      implicit none
      character rtmodule*7
      integer n,i
      real*4 wavebin(50),cutpoint(50)
      cutpoint(1) = wavebin(1) - (wavebin(2) - wavebin(1))/2.d0
 
      do 190 i = 2,n
       cutpoint(i) = (wavebin(i-1) + wavebin(i))/2.d0
! Exception:
       if((rtmodule.eq.'SPHERAD').and.(i.eq.24))then
        cutpoint(i) = 402.5
       endif
    
190   continue
      cutpoint(n+1) = wavebin(n) + (wavebin(n) - wavebin(n-1))/2.d0

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_bin_data(rtmodule
     &                       ,wavebin,n)
      implicit none
      character rtmodule*7
     
      integer n,i
      real*4 wavebin(50),xwavebin(30),ywavebin(30)
    
      wavebin(1:50) = 0.d0
 

     
       data xwavebin / 
     &                295.0, 303.0, 310.0, 316.0, 333.0
     &              , 380.0, 574.0,   0.0,   0.0,   0.0
     &              ,   0.0,   0.0,   0.0,   0.0,   0.0
     &              ,   0.0,   0.0,   0.0,   0.0,   0.0
     &              ,   0.0,   0.0,   0.0,   0.0,   0.0
     &              ,   0.0,   0.0,   0.0,   0.0,   0.0   /


       data ywavebin / 
     &                290.0, 295.0, 300.0, 305.0, 310.0
     &              , 315.0, 320.0, 325.0, 330.0, 335.0
     &              , 340.0, 345.0, 350.0, 355.0, 360.0
     &              , 365.0, 370.0, 375.0, 380.0, 385.0
     &              , 390.0, 395.0, 400.0, 450.0, 550.0
     &              , 650.0, 750.0,   0.0,   0.0,   0.0   /



      if(rtmodule.eq.'INLINE')then
       n = 7
       do 190 i = 1,n
        wavebin(i) = xwavebin(i)
190    continue
      elseif(rtmodule.eq.'SPHERAD')then
       n = 27 
       do 195 i = 1,n
        wavebin(i) = ywavebin(i)
195    continue
      endif

      return     
      end
!***********************************************************************
!***********************************************************************
      subroutine mapalbedo(n,bin_spectra
     &                    ,nlcd_spectra,usgs_spectra,modis_spectra)
! PURPOSE:    Maps the 50 NLCD and 24 USGS landuse categories to the closest
!             20 MODIS categories - with a weighting factor applied to the mapping
!             for continuity of albedo of NLCD/USGS to MODIS.  17 of the 20 MODIS 
!             categories are then mapped to 17 uniquely corresponding spectral albedo  
!             categories; 3 MODIS categories (Wooded Tundra, Mixed Tundra, and Barren 
!             TUNDRA) are mapped to spectral "Tundra". 


! INPUT:      n,bin_spectra
! OUTPUT:     nlcd_spectra,usgs_spectra,modis_spectra
! CALLS:      nlcdusgsmodis
! CALLED BY:  veg

      implicit none
      real*4 nlcdmap(65,2),usgsmap(65,2),modismap(65,2)
     &      ,a(65),b(65),y,nlcdtype(50,3),usgstype(50,3),modistype(50,3)
     &      ,bin_spectra(50,50,2),nlcd_spectra(50,50,2)
     &      ,usgs_spectra(50,50,2),modis_spectra(50,50,2)
     &      ,albedomap(6,64),temparray(500,3)
     
      integer c(65),d(65),e(65),f(65),icount,jcount,kcount,i,ix,iz
     &       ,n,ii,jj,nn

      INTEGER, SAVE :: IO_INSPECT

      CHARACTER(30) SPECTRAL_LANDUSE(20)

      DATA SPECTRAL_LANDUSE /
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


      CHARACTER(40) USGS_LANDUSE(24)
      DATA USGS_LANDUSE /
     &   'Urban or Built-up Land',
     &   'Dryland Cropland and Pasture',
     &   'Irrigated Cropland and Pasture',
     &   'Mixed Dry/Irr Crop and Past',
     &   'Grassland/Cropland Mosaic',
     &   'Woodland/Cropland Mosaic',
     &   'Grassland',
     &   'Shrubland',
     &   'Mixed Shrubland/Grassland',
     &   'Savanna',
     &   'Broadleaf Deciduous Forest',
     &   'Deciduous Coniferous Forest',
     &   'Evergreen Broadleaf Forest (Palm?)',
     &   'Evergreen Coniferous Forest',
     &   'Mixed forest',
     &   'Water',
     &   'Herbaceous Wetland (none in east)',
     &   'Forested Wetlands (e.g. Everglades)',
     &   'Barren or Sparsely Vegetated',
     &   'Herbaceous Tundra',
     &   'Shrub and Brush Tundra',
     &   'Mixed Tundra',
     &   'Bare Ground Tundra',
     &   'Perennial Snowfields or Glaciers'  /
     
      CHARACTER(40) MODIS_LANDUSE(20)
      DATA MODIS_LANDUSE /
     &   'Evergreen Needleleaf Forest',
     &   'Evergreen Broadleaf Forest',
     &   'Deciduous Needleleaf Forest',
     &   'Deciduous Broadleaf Forest',
     &   'Mixed Forest',
     &   'Closed Shrublands',
     &   'Open Shrublands',
     &   'Woody Savanna',
     &   'Savanna',
     &   'Grasslands',
     &   'Perminent Wetlands',
     &   'Croplands',
     &   'Urban andBuilt-up',
     &   'Cropland/Natural Vegetation',
     &   'Snow and Ice',
     &   'Barren or Sparsely Vegetated',
     &   'IGBP water',
     &   'Mixed Tundra',
     &   'Wooded Tundra',
     &   'Barren Tundra'   /

      CHARACTER(40) NLCD_LANDUSE(50)
      DATA NLCD_LANDUSE /
     &   'Open water',
     &   'Perennial Ice/snow ',
     &   'Developed, Open space ',
     &   'Developed, Low Intensity',
     &   'Developed, Medium Intensity',
     &   'Developed, High Intensity',
     &   'Barren land',
     &   'Unconsolidated Shore',
     &   'Deciduous Forest',
     &   'Evergreen Forest',
     &   'Mixed Forest',
     &   'Dwarf Scrub',
     &   'Shrub/Scrub',
     &   'Grassland/Herbaceous',
     &   'Sedge/Herbaceous',
     &   'Lichens',
     &   'Moss',
     &   'Tundra',
     &   'Pasture/Hay',
     &   'Cultivated Crops',
     &   'Woody Wetland',
     &   'Palustrine Forested Wetland',
     &   'Palustrine Scrub/Shrub Wetland',
     &   'Estuarine Forested Wetland',
     &   'Estuarine Scrub/Shrub Wetland',
     &   'Emergent Herbaceous Wetland',
     &   'Palustrine Emergent Wetland',
     &   'Estuarine Emergent Wetland',
     &   'Palustrine Aquatic Bed',
     &   'Estuarine Aquatic Bed',
     &   'Open water (mapped to 1)',
     &   'Evergreen Needleleaf Forest',
     &   'Evergreen Broadleaf Forest',
     &   'Deciduous Needleleaf Forest',
     &   'Deciduous Broadleaf Forest',
     &   'Mixed Forest',
     &   'Closed Shrublands',
     &   'Open Shrublands',
     &   'Woody Savanna',
     &   'Savanna',
     &   'Grasslands',
     &   'Perminent Wetlands',
     &   'Croplands',
     &   'Urban andBuilt-up',
     &   'Cropland/Natural Vegetation Mosaic',
     &   'Snow and Ice',
     &   'Barren or Sparsely Vegetated',
     &   'IGBP water',
     &   'unclassified',
     &   'fill value (normally ocean water)' /

       CHARACTER(40) NAMEA, NAMEB

      icount = 0
      jcount = 0
      kcount = 0
      nlcdtype(1:50,1:3) = 0.d0
      usgstype(1:50,1:3) = 0.d0
      modistype(1:50,1:3) = 0.d0
      nlcd_spectra(1:50,1:50,1:2) = 0.0
      usgs_spectra(1:50,1:50,1:2) = 0.0
      modis_spectra(1:50,1:50,1:2) = 0.0
      albedomap(1:6,1:64) = 0.0
      temparray(1:500,1:3) = 0.0

    
      IO_INSPECT = 13
      IF( IO_INSPECT .NE. 6 )THEN
          OPEN(IO_INSPECT, FILE='ALBEDO_MAPS.dat',STATUS='UNKNOWN')
      ENDIF

      call nlcdusgsmodis(albedomap)
     
      do 190 i = 1,64
       a(i) = albedomap(1,i)
       b(i) = albedomap(2,i)
       c(i) = int(albedomap(3,i))
       d(i) = int(albedomap(4,i))
       e(i) = int(albedomap(5,i))
       f(i) = int(albedomap(6,i))
190   continue

      do 290 i = 1,64
       if(a(i).ne.0)then
        icount = icount + 1
        temparray(icount,1) = real(c(i))
	temparray(icount,2) = a(i)
	temparray(icount,3) = real(f(i))

       endif
290   continue
      do 390 i = 1,64
       if(b(i).ne.0)then
        jcount = jcount + 1
        temparray(jcount+icount,1) = real(d(i))
	temparray(jcount+icount,2) = b(i)
	temparray(jcount+icount,3) = real(f(i))

       endif
390   continue
      do 490 i = 1,64
       if(e(i).ne.0)then
        kcount = kcount + 1
        temparray(kcount+icount+jcount,1) = real(e(i))
	temparray(kcount+icount+jcount,2) = 1.
	temparray(kcount+icount+jcount,3) = real(f(i))

       endif
490   continue
      rewind(8)
      do 590 i = 1,icount

       ix = int(temparray(i,1))
       y  =     temparray(i,2)
       iz = int(temparray(i,3))

       nlcdmap(ix,1)=iz
       nlcdmap(ix,2)=y
590   continue
      do 690 i = 1,jcount

       ix = int(temparray(i+icount,1))
       y  =     temparray(i+icount,2)
       iz = int(temparray(i+icount,3))

       usgsmap(ix,1)=iz
       usgsmap(ix,2)=y
690   continue
      do 790 i = 1,kcount

       ix = int(temparray(i+icount+jcount,1))
       y  =     temparray(i+icount+jcount,2)
       iz = int(temparray(i+icount+jcount,3))

       modismap(ix,1)=iz
       modismap(ix,2)=y
790   continue
      do 890 i = 1,icount
       nlcdtype(i,1) = real(i)
       nlcdtype(i,2) = nlcdmap(i,1)
       nlcdtype(i,3) = nlcdmap(i,2)

890   continue      
      do 990 i = 1,jcount
       usgstype(i,1) = real(i)
       usgstype(i,2) = usgsmap(i,1)
       usgstype(i,3) = usgsmap(i,2)

990   continue
      do 1090 i = 1,kcount
       modistype(i,1) = real(i)
       modistype(i,2) = modismap(i,1)
       modistype(i,3) = modismap(i,2)

1090  continue

! NLCD Map
      do 2190 i = 1,icount
       do 2170 nn = 1,n
        nlcd_spectra(i,nn,1) = bin_spectra(int(nlcdtype(i,2)),nn,1)
        nlcd_spectra(i,nn,2) = bin_spectra(int(nlcdtype(i,2)),nn,2)
     &                     *nlcdtype(i,3)
        nlcd_spectra(i,nn,2) = min(nlcd_spectra(i,nn,2),1.00)
        nlcd_spectra(i,nn,2) = max(nlcd_spectra(i,nn,2),0.00)
!!        write(19,2000)i,nn,nlcd_spectra(i,nn,1),nlcd_spectra(i,nn,2)      
!!2000    format(2i6,2f12.4)
        IF(IO_INSPECT .GT. 0)THEN
           NAMEA = NLCD_LANDUSE(I)(1:LEN_TRIM(NLCD_LANDUSE(I)))
           NAMEB = SPECTRAL_LANDUSE(int(nlcdtype(i,2)))(1:LEN_TRIM(SPECTRAL_LANDUSE(int(nlcdtype(i,2)))))
           write(IO_INSPECT,2000)i,NAMEA,nn,nlcdtype(i,2),nlcdtype(i,3),nlcd_spectra(i,nn,1),
     &     nlcd_spectra(i,nn,2),bin_spectra(int(nlcdtype(i,2)),nn,2),
     &     NAMEB
        ENDIF
2170   continue
2190  continue

! USGS MAP
      do 3190 i = 1,jcount
       do 3170 nn = 1,n
        usgs_spectra(i,nn,1) = bin_spectra(int(usgstype(i,2)),nn,1)
        usgs_spectra(i,nn,2) = bin_spectra(int(usgstype(i,2)),nn,2)
     &                     *usgstype(i,3)
        usgs_spectra(i,nn,2) = min(usgs_spectra(i,nn,2),1.00)
        usgs_spectra(i,nn,2) = max(usgs_spectra(i,nn,2),0.00)
!!        write(19,2000)i,nn,usgs_spectra(i,nn,1),usgs_spectra(i,nn,2)
        IF(IO_INSPECT .GT. 0)THEN
           NAMEA = USGS_LANDUSE(I)(1:LEN_TRIM(USGS_LANDUSE(I)))
           NAMEB = SPECTRAL_LANDUSE(int(usgstype(i,2)))(1:LEN_TRIM(SPECTRAL_LANDUSE(int(usgstype(i,2)))))
           write(IO_INSPECT,2000)i,NAMEA,nn,usgstype(i,2),usgstype(i,3),usgs_spectra(i,nn,1),
     &     usgs_spectra(i,nn,2),bin_spectra(int(usgstype(i,2)),nn,2),
     &     NAMEB
        ENDIF
3170   continue
3190  continue

! MODIS Map       
      do 4190 i = 1,kcount
       do 4170 nn = 1,n
        modis_spectra(i,nn,1) = bin_spectra(int(modistype(i,2)),nn,1)
        modis_spectra(i,nn,2) = bin_spectra(int(modistype(i,2)),nn,2)
     &                      *modistype(i,3)
        modis_spectra(i,nn,2) = min(modis_spectra(i,nn,2),1.00)
        modis_spectra(i,nn,2) = max(modis_spectra(i,nn,2),0.00)
!!        write(19,2000)i,nn,modis_spectra(i,nn,1),modis_spectra(i,nn,2)
        IF(IO_INSPECT .GT. 0)THEN
           NAMEA = MODIS_LANDUSE(I)(1:LEN_TRIM(MODIS_LANDUSE(I)))
           NAMEB = SPECTRAL_LANDUSE(int(modistype(i,2)))(1:LEN_TRIM(SPECTRAL_LANDUSE(int(modistype(i,2)))))
           write(IO_INSPECT,2000)i,NAMEA,nn,modistype(i,2),modistype(i,3),modis_spectra(i,nn,1),
     &     modis_spectra(i,nn,2),bin_spectra(int(modistype(i,2)),nn,2),
     &     NAMEB
        ENDIF
4170   continue
4190  continue

2000    format(i2,1X,a30,1X,i2,3(f7.3,1X),2(f8.4,1X),a30)
       CLOSE(IO_INSPECT)
      return
      end          
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine nlcdusgsmodis(
     &                         albedomap)

       implicit none
       integer i,j
       real*4 categorymap(6,64),albedomap(6,64)
       
!   NLCD2MODIS,USGS2MODIS,NLCD2SPEC,USGS2SPEC,MODIS2SPEC,SPEC

      data categorymap  /
     &  0,	0,	0,	0,	0,	0
     &, 0,	0,	0,	0,	0,	0
     &, 1,	0,	1,	0,	0,	17
     &, 1.0,	0,	2,	0,	0,	15
     &, 1.0667,	0,	3,	0,	0,	13
     &, 1.1333,	0,	4,	0,	0,	13
     &, 1.0667,	0,	5,	0,	0,	13
     &, 1.1333,	0,	6,	0,	0,	13
     &, 0.8,	0,	7,	0,	0,	16
     &, 1.4,	0,	8,	0,	0,	16
     &, 0.9375,	0,	9,	0,	0,	4
     &, 0.8333,	0,	10,	0,	0,	1
     &, 1,	0,	11,	0,	0,	5
     &, 1,	0,	12,	0,	0,	7
     &, 1,	0,	13,	0,	0,	7
     &, 1.1364,	0,	14,	0,	0,	8
     &, 1.0455,	0,	15,	0,	0,	8
     &, 1.0526,	0,	16,	0,	0,	10
     &, 1.0526,	0,	17,	0,	0,	10
     &, 1,	1,	18,	22,	0,	18
     &, 1.2778,	0,	19,	0,	0,	14
     &, 1.1765,	1.0588,	20,	4,	0,	12
     &, 1.0714,	1,	21,	17,	0,	11
     &, 1.0714,	0,	22,	0,	0,	11
     &, 1.0714,	0,	23,	0,	0,	11
     &, 1.0714,	0,	24,	0,	0,	11
     &, 1.0714,	0,	25,	0,	0,	11
     &, 1.2857,	0,	26,	0,	0,	11
     &, 1.2857,	0,	27,	0,	0,	11
     &, 1.2857,	0,	28,	0,	0,	11
     &, 1.25,	0,	29,	0,	0,	17
     &, 1.25,	0,	30,	0,	0,	17
     &, 1,	0,	31,	0,	0,	17
     &, 1,	1,	32,	14,	1,	1
     &, 1,	1,	33,	13,	2,	2
     &, 1,	1,	34,	12,	3,	3
     &, 1,	1,	35,	11,	4,	4
     &, 1,	1,	36,	15,	5,	5
     &, 1,	1,	37,	8,	6,	6
     &, 1,	1,	38,	9,	7,	7
     &, 1,	0,	39,	0,	8,	8
     &, 1,	1,	40,	10,	9,	9
     &, 1,	1,	41,	7,	10,	10
     &, 1,	1,	42,	18,	11,	11
     &, 1,	1,	43,	2,	12,	12
     &, 1,	1,	44,	1,	13,	13
     &, 1,	0,	45,	0,	14,	14
     &, 1,	1,	46,	24,	15,	15
     &, 1,	1,	47,	19,	16,	16
     &, 1,	1,	48,	16,	17,	17
     &, 1,	0,	49,	0,	0,	17
     &, 1,	0,	50,	0,	0,	17
     &, 0,	0,	0,	0,	0,	0
     &, 0,	1,	0,	20,	19,	18
     &, 0,	1,	0,	21,	18,	18
     &, 0,	0,	0,	0,	0,	0
     &, 0,	1,	0,	23,	20,	18
     &, 0,	0,	0,	0,	0,	0
     &, 0,	1.0588,	0,	3,	0,	12
     &, 0,	0,	0,	0,	0,	0
     &, 0,	1,	0,	5,	0,	14
     &, 0,	0.8889,	0,	6,	0,	14
     &, 0,	0,	0,	0,	0,	19
     &, 0,	0,	0,	0,	0,	20  /

       do 190 i = 1,64
        do 170 j = 1,6
	 albedomap(j,i) = categorymap(j,i)
170     continue
190    continue 
       return
       end
!***********************************************************************
      SUBROUTINE ALBEDO_MAP(LAND_SCHEME, NUMB_REF, NUMB_SCHEME, ALBEDO_REF_TO_SCHEME)
! PURPOSE:    returns a map between the surface albedo of the refererence and
!             input land characterization scheme donated by variable LAND_SCHEME
!             Algorithm only accepts three values for LAND_SCHEME: USGS24, MODIS
!             and NLCD-MODIS. 
!             MODIS exactly matches the reference. NLCD-MOSIS and USGS map to closest
!             20 MODIS categories - with a weighting factor applied to the mapping
!             for continuity of albedo of NLCD/USGS to MODIS.  17 of the 20 MODIS 
!             categories are then mapped to 17 uniquely corresponding spectral albedo  
!             categories; 3 MODIS categories (Wooded Tundra, Mixed Tundra, and Barren 
!             TUNDRA) are mapped to spectral "Tundra". 

      IMPLICIT NONE

! INPUT:      LAND_SCHEME
! OUTPUT:     ALBEDO_REF_TO_SCHEME

      CHARACTER(80)           :: LAND_SCHEME   ! land use scheme used 
      INTEGER                 :: NUMB_REF      ! # of land categories in reference
      INTEGER                 :: NUMB_SCHEME   ! # of land categories in LAND_SCHEME
      REAL                    :: ALBEDO_REF_TO_SCHEME( :, :)

! CALLS:    subroutine  nlcdusgsmodis
! LOCAL:

      REAL nlcdmap(65,2)
      REAL usgsmap(65,2)
      REAL modismap(65,2)
      REAL a(65)
      REAL b(65)
      REAL y
      REAL nlcdtype(50,3)
      REAL usgstype(50,3)
      REAL modistype(50,3)
      REAL, ALLOCATABLE :: BUFFER( : )
      REAL bin_spectra(50,50,2),nlcd_spectra(50,50,2)
      REAL usgs_spectra(50,50,2)
      REAL modis_spectra(50,50,2)
      REAL albedomap(6,64)
      REAL temparray(500,3)

      INTEGER, PARAMETER :: N_ALBEDO_REF = 20
     
      INTEGER N

      INTEGER c(65)
      INTEGER d(65)
      INTEGER e(65)
      INTEGER f(65)

      INTEGER icount
      INTEGER jcount
      INTEGER kcount

      INTEGER i,ix,iz
      INTEGER ii,jj,nn

      INTEGER, SAVE :: IO_INSPECT
      LOGICAL, SAVE :: FIRSTCALL = .TRUE.

      CHARACTER(30) SPECTRAL_LANDUSE(20)

      DATA SPECTRAL_LANDUSE /
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


      CHARACTER(40) USGS_LANDUSE(24)
      DATA USGS_LANDUSE /
     &   'Urban or Built-up Land',
     &   'Dryland Cropland and Pasture',
     &   'Irrigated Cropland and Pasture',
     &   'Mixed Dry/Irr Crop and Past',
     &   'Grassland/Cropland Mosaic',
     &   'Woodland/Cropland Mosaic',
     &   'Grassland',
     &   'Shrubland',
     &   'Mixed Shrubland/Grassland',
     &   'Savanna',
     &   'Broadleaf Deciduous Forest',
     &   'Deciduous Coniferous Forest',
     &   'Evergreen Broadleaf Forest (Palm?)',
     &   'Evergreen Coniferous Forest',
     &   'Mixed forest',
     &   'Water',
     &   'Herbaceous Wetland (none in east)',
     &   'Forested Wetlands (e.g. Everglades)',
     &   'Barren or Sparsely Vegetated',
     &   'Herbaceous Tundra',
     &   'Shrub and Brush Tundra',
     &   'Mixed Tundra',
     &   'Bare Ground Tundra',
     &   'Perennial Snowfields or Glaciers'  /
     
      CHARACTER(40) MODIS_LANDUSE(20)
      DATA MODIS_LANDUSE /
     &   'Evergreen Needleleaf Forest',
     &   'Evergreen Broadleaf Forest',
     &   'Deciduous Needleleaf Forest',
     &   'Deciduous Broadleaf Forest',
     &   'Mixed Forest',
     &   'Closed Shrublands',
     &   'Open Shrublands',
     &   'Woody Savanna',
     &   'Savanna',
     &   'Grasslands',
     &   'Perminent Wetlands',
     &   'Croplands',
     &   'Urban andBuilt-up',
     &   'Cropland/Natural Vegetation',
     &   'Snow and Ice',
     &   'Barren or Sparsely Vegetated',
     &   'IGBP water',
     &   'Mixed Tundra',
     &   'Wooded Tundra',
     &   'Barren Tundra'   /

      CHARACTER(40) NLCD_LANDUSE(50)
      DATA NLCD_LANDUSE /
     &   'Open water',
     &   'Perennial Ice/snow ',
     &   'Developed, Open space ',
     &   'Developed, Low Intensity',
     &   'Developed, Medium Intensity',
     &   'Developed, High Intensity',
     &   'Barren land',
     &   'Unconsolidated Shore',
     &   'Deciduous Forest',
     &   'Evergreen Forest',
     &   'Mixed Forest',
     &   'Dwarf Scrub',
     &   'Shrub/Scrub',
     &   'Grassland/Herbaceous',
     &   'Sedge/Herbaceous',
     &   'Lichens',
     &   'Moss',
     &   'Tundra',
     &   'Pasture/Hay',
     &   'Cultivated Crops',
     &   'Woody Wetland',
     &   'Palustrine Forested Wetland',
     &   'Palustrine Scrub/Shrub Wetland',
     &   'Estuarine Forested Wetland',
     &   'Estuarine Scrub/Shrub Wetland',
     &   'Emergent Herbaceous Wetland',
     &   'Palustrine Emergent Wetland',
     &   'Estuarine Emergent Wetland',
     &   'Palustrine Aquatic Bed',
     &   'Estuarine Aquatic Bed',
     &   'Open water (mapped to 1)',
     &   'Evergreen Needleleaf Forest',
     &   'Evergreen Broadleaf Forest',
     &   'Deciduous Needleleaf Forest',
     &   'Deciduous Broadleaf Forest',
     &   'Mixed Forest',
     &   'Closed Shrublands',
     &   'Open Shrublands',
     &   'Woody Savanna',
     &   'Savanna',
     &   'Grasslands',
     &   'Perminent Wetlands',
     &   'Croplands',
     &   'Urban andBuilt-up',
     &   'Cropland/Natural Vegetation Mosaic',
     &   'Snow and Ice',
     &   'Barren or Sparsely Vegetated',
     &   'IGBP water',
     &   'unclassified',
     &   'fill value (normally ocean water)' /

       CHARACTER(40) NAMEA, NAMEB
       INTEGER, PARAMETER :: NUMB_USGS =   24

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

      INTEGER, PARAMETER :: NUMB_MODIS =   20

      REAL               :: ALBMAP_REF2MODIS( NUMB_MODIS )
      DATA ALBMAP_REF2MODIS /
     &         1.0000,     2.0000,     3.0000,     4.0000,     5.0000,
     &         6.0000,     7.0000,     8.0000,     9.0000,    10.0000,
     &        11.0000,    12.0000,    13.0000,    14.0000,    15.0000,
     &        16.0000,    17.0000,    18.0000,    18.0000,    18.0000/

      REAL               :: ALBFAC_REF2MODIS( NUMB_MODIS )
      DATA ALBFAC_REF2MODIS /
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000,
     &         1.0000,     1.0000,     1.0000,     1.0000,     1.0000/

      INTEGER, PARAMETER :: NUMB_NLCD_MODIS =   50

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

      INTEGER  INDEX_REF
      REAL     FACTOR

      icount = 0
      jcount = 0
      kcount = 0
      nlcdtype(1:50,1:3) = 0.d0
      usgstype(1:50,1:3) = 0.d0
      modistype(1:50,1:3) = 0.d0
      nlcd_spectra(1:50,1:50,1:2) = 0.0
      usgs_spectra(1:50,1:50,1:2) = 0.0
      modis_spectra(1:50,1:50,1:2) = 0.0
      albedomap(1:6,1:64) = 0.0
      temparray(1:500,1:3) = 0.0

    
      

      IF( FIRSTCALL )THEN
          IO_INSPECT = 13
          IF(IO_INSPECT .NE. 6)OPEN(IO_INSPECT, FILE='ALBEDO_MAPS2.dat',STATUS='UNKNOWN')
          FIRSTCALL = .FALSE.
      ENDIF

      N           = 7
      BIN_SPECTRA = 0.0
      
      call nlcdusgsmodis(albedomap)
     
      do 190 i = 1,64
       a(i) = albedomap(1,i)
       b(i) = albedomap(2,i)
       c(i) = int(albedomap(3,i))
       d(i) = int(albedomap(4,i))
       e(i) = int(albedomap(5,i))
       f(i) = int(albedomap(6,i))
190   continue

      do 290 i = 1,64
       if(a(i).ne.0)then
        icount = icount + 1
        temparray(icount,1) = real(c(i))
	temparray(icount,2) = a(i)
	temparray(icount,3) = real(f(i))

       endif
290   continue
      do 390 i = 1,64
       if(b(i).ne.0)then
        jcount = jcount + 1
        temparray(jcount+icount,1) = real(d(i))
	temparray(jcount+icount,2) = b(i)
	temparray(jcount+icount,3) = real(f(i))

       endif
390   continue
      do 490 i = 1,64
       if(e(i).ne.0)then
        kcount = kcount + 1
        temparray(kcount+icount+jcount,1) = real(e(i))
	temparray(kcount+icount+jcount,2) = 1.
	temparray(kcount+icount+jcount,3) = real(f(i))

       endif
490   continue
      rewind(8)
      do 590 i = 1,icount

       ix = int(temparray(i,1))
       y  =     temparray(i,2)
       iz = int(temparray(i,3))

       nlcdmap(ix,1)=iz
       nlcdmap(ix,2)=y
590   continue
      do 690 i = 1,jcount

       ix = int(temparray(i+icount,1))
       y  =     temparray(i+icount,2)
       iz = int(temparray(i+icount,3))

       usgsmap(ix,1)=iz
       usgsmap(ix,2)=y
690   continue
      do 790 i = 1,kcount

       ix = int(temparray(i+icount+jcount,1))
       y  =     temparray(i+icount+jcount,2)
       iz = int(temparray(i+icount+jcount,3))

       modismap(ix,1)=iz
       modismap(ix,2)=y
790   continue
      do 890 i = 1,icount
       nlcdtype(i,1) = real(i)
       nlcdtype(i,2) = nlcdmap(i,1)
       nlcdtype(i,3) = nlcdmap(i,2)

890   continue      
      do 990 i = 1,jcount
       usgstype(i,1) = real(i)
       usgstype(i,2) = usgsmap(i,1)
       usgstype(i,3) = usgsmap(i,2)

990   continue
      do 1090 i = 1,kcount
       modistype(i,1) = real(i)
       modistype(i,2) = modismap(i,1)
       modistype(i,3) = modismap(i,2)

1090  continue

      print*,LAND_SCHEME
      print*,INDEX(LAND_SCHEME, 'NLCD-MODIS')
      print*,INDEX(LAND_SCHEME, 'USGS24')
      print*,INDEX(LAND_SCHEME, 'MODIS')

      IF( INDEX(LAND_SCHEME, 'NLCD-MODIS') .GT. 0)THEN
! NLCD Map
           do 2190 i = 1,icount

            INDEX_REF = INT( ALBMAP_REF2NLCD( i ) )
            FACTOR    = ALBFAC_REF2NLCD( i )

            do 2170 nn = 1,n
             nlcd_spectra(i,nn,1) = bin_spectra(int(nlcdtype(i,2)),nn,1)


             nlcd_spectra(i,nn,2) = bin_spectra(int(nlcdtype(i,2)),nn,2)
     &                            * nlcdtype(i,3)

             nlcd_spectra(i,nn,2) = bin_spectra(INDEX_REF,nn,2)
     &                            * FACTOR

             ALBEDO_REF_TO_SCHEME( i, int(nlcdtype(i,2))) = nlcdtype(i,3)

             nlcd_spectra(i,nn,2) = min(nlcd_spectra(i,nn,2),1.00)
             nlcd_spectra(i,nn,2) = max(nlcd_spectra(i,nn,2),0.00)
!!             write(19,2000)i,nn,nlcd_spectra(i,nn,1),nlcd_spectra(i,nn,2)      
!!2000         format(2i6,2f12.4)
             IF(IO_INSPECT .GT. 0)THEN
                NAMEA = NLCD_LANDUSE(I)(1:LEN_TRIM(NLCD_LANDUSE(I)))
                NAMEB = SPECTRAL_LANDUSE(int(nlcdtype(i,2)))(1:LEN_TRIM(SPECTRAL_LANDUSE(int(nlcdtype(i,2)))))
                write(IO_INSPECT,2000)i,NAMEA,nn,nlcdtype(i,2),nlcdtype(i,3),nlcd_spectra(i,nn,1),
     &          nlcd_spectra(i,nn,2),bin_spectra(int(nlcdtype(i,2)),nn,2),
     &          NAMEB
             ENDIF
2170        continue
2190       continue

           IF(IO_INSPECT .GT. 0)THEN
               WRITE(IO_INSPECT,5001)'NUMB_NLCD_MODIS', icount

               ALLOCATE( BUFFER(icount) )
               BUFFER( 1:icount ) = nlcdtype(1:icount,2)

               CALL WRBF12D_HEADERB(IO_INSPECT, 5, icount,BUFFER,'NUMB_NLCD_MODIS',
     &                              'ALBMAP_REF2NLCD', 'F')

               BUFFER( 1:icount ) = nlcdtype(1:icount,3)

               CALL WRBF12D_HEADERB(IO_INSPECT, 5, icount,BUFFER,'NUMB_NLCD_MODIS',
     &                              'ALBFAC_REF2NLCD', 'F')

               DEALLOCATE( BUFFER )

           ENDIF

           RETURN
      ENDIF

! USGS MAP
      IF( INDEX(LAND_SCHEME, 'USGS24') .GT. 0)THEN
           do 3190 i = 1,jcount


            INDEX_REF = INT( ALBMAP_REF2USGS( i ) )
            FACTOR    = ALBFAC_REF2USGS( i )

            do 3170 nn = 1,n
             usgs_spectra(i,nn,1) = bin_spectra(int(usgstype(i,2)),nn,1)
             usgs_spectra(i,nn,2) = bin_spectra(int(usgstype(i,2)),nn,2)
     &                            * usgstype(i,3)

             usgs_spectra(i,nn,2) = bin_spectra(INDEX_REF,nn,2)
     &                            * FACTOR

             usgs_spectra(i,nn,2) = min(usgs_spectra(i,nn,2),1.00)
             usgs_spectra(i,nn,2) = max(usgs_spectra(i,nn,2),0.00)

             ALBEDO_REF_TO_SCHEME( i, int(USGStype(i,2))) = USGStype(i,3)

!!        write(19,2000)i,nn,usgs_spectra(i,nn,1),usgs_spectra(i,nn,2)
             IF(IO_INSPECT .GT. 0)THEN
                NAMEA = USGS_LANDUSE(I)(1:LEN_TRIM(USGS_LANDUSE(I)))
                NAMEB = SPECTRAL_LANDUSE(int(usgstype(i,2)))(1:LEN_TRIM(SPECTRAL_LANDUSE(int(usgstype(i,2)))))
                write(IO_INSPECT,2000)i,NAMEA,nn,usgstype(i,2),usgstype(i,3),usgs_spectra(i,nn,1),
     &          usgs_spectra(i,nn,2),bin_spectra(int(usgstype(i,2)),nn,2),
     &          NAMEB
             ENDIF
3170        continue
3190       continue

          IF(IO_INSPECT .GT. 0)THEN
               WRITE(IO_INSPECT,5001)'NUMB_USGS', jcount

               ALLOCATE( BUFFER(jcount) )
               BUFFER( 1:jcount ) = usgstype(1:jcount,2)

               CALL WRBF12D_HEADERB(IO_INSPECT, 5, jcount,BUFFER,'NUMB_USGS',
     &                              'ALBMAP_REF2USGS', 'F')

               BUFFER( 1:jcount ) = usgstype(1:jcount,3)

               CALL WRBF12D_HEADERB(IO_INSPECT, 5, jcount,BUFFER,'NUMB_USGS',
     &                              'ALBFAC_REF2USGS', 'F')

               DEALLOCATE( BUFFER )

           ENDIF

           RETURN
      ENDIF

! MODIS Map
      IF( INDEX(LAND_SCHEME, 'MODIS') .GT. 0 .AND. INDEX(LAND_SCHEME, 'NLCD') .EQ. 0)THEN
           do 4190 i = 1,kcount

            INDEX_REF = INT( ALBMAP_REF2MODIS( i ) )
            FACTOR    = ALBFAC_REF2MODIS( i )
      
            do 4170 nn = 1,n
             modis_spectra(i,nn,1) = bin_spectra(int(modistype(i,2)),nn,1)
             modis_spectra(i,nn,2) = bin_spectra(int(modistype(i,2)),nn,2)
     &                             * modistype(i,3)

             modis_spectra(i,nn,2) = bin_spectra(INDEX_REF,nn,2)
     &                             * FACTOR

             ALBEDO_REF_TO_SCHEME( i, int(MODIStype(i,2))) = MODIStype(i,3)

             modis_spectra(i,nn,2) = min(modis_spectra(i,nn,2),1.00)
             modis_spectra(i,nn,2) = max(modis_spectra(i,nn,2),0.00)
!!             write(19,2000)i,nn,modis_spectra(i,nn,1),modis_spectra(i,nn,2)
             IF(IO_INSPECT .GT. 0)THEN
                NAMEA = MODIS_LANDUSE(I)(1:LEN_TRIM(MODIS_LANDUSE(I)))
                NAMEB = SPECTRAL_LANDUSE(int(modistype(i,2)))(1:LEN_TRIM(SPECTRAL_LANDUSE(int(modistype(i,2)))))
                write(IO_INSPECT,2000)i,NAMEA,nn,modistype(i,2),modistype(i,3),modis_spectra(i,nn,1),
     &          modis_spectra(i,nn,2),bin_spectra(int(modistype(i,2)),nn,2),
     &          NAMEB
             ENDIF
4170        continue
4190       continue

          IF(IO_INSPECT .GT. 0)THEN
               WRITE(IO_INSPECT,5001)'NUMB_MODIS', kcount

               ALLOCATE( BUFFER(kcount) )
               BUFFER( 1:kcount ) = modistype(1:kcount,2)

               CALL WRBF12D_HEADERB(IO_INSPECT, 5, kcount,BUFFER,'NUMB_MODIS',
     &                              'ALBMAP_REF2MODIS', 'F')

               BUFFER( 1:kcount ) = modistype(1:kcount,3)

               CALL WRBF12D_HEADERB(IO_INSPECT, 5, kcount,BUFFER,'NUMB_MODIS',
     &                              'ALBFAC_REF2MODIS', 'F')

               DEALLOCATE( BUFFER )

           ENDIF
           RETURN

      ENDIF

      ALBEDO_REF_TO_SCHEME = -1.0  ! returned result produces error flag

2000  FORMAT(i2,1X,a30,1X,i2,3(f7.3,1X),2(f8.4,1X),a30)
5001  FORMAT(7X,'INTEGER, PARAMETER :: ',A,' = ',I4)

!      CLOSE(IO_INSPECT)
      return
      end          
!***********************************************************************
!1	Asner, G.  Biophysical and biochemical sources of variability in canopy reflectance.  
!	Remote Sens. Environ.  64:234-253, 1998.
!
!2	Belward, A., and T. Loveland. The DIS 1km Land Cover Data Set, Global Change, The 
!	IGBP Newsletter, #27, Sep., 1996. 
!
!3	Betts, A. K., and J. H. Ball. Albedo over the Boreal Forest, 
!	J. Geophys. Res., 102, 28901-28909, 1997. 
!
!4	Blumthaler, M., and W. Ambach.  Solar UVB-albedo of various surfaces.  
!	Photochem. Photobiol.  48(1) 85-88,  1988.
!
!5	Bowker, D. E., R. E. Davis, D. L. Myrick, K. Stacy, and W. T. Jones. 
!	Spectral Reflectances of Natural Targets for use in Remote Sensing Studies, 
!	NASA Ref. Pub., 1139, June 1985.
!
!6	Briegleb, B. P., P. Minnis, V. Ramanathan, and E. Harrison. Comparison of Regional 
!	Clear-Sky Albedos Inferred from Satellite Observations and Models Comparisons, 
!	J. Clim. Appl. Meteor., 25, 214-226, 1986. 
!
!7	Briegleb, B. P., and V. Ramanathan. Spectral and Diurnal Variations in Clear Sky 
!	Planetary Albedo, J. Appl. Meteor., 21, 1160-1171, 1982. 
!
!8	Coddington, Odele, K. S. Schmidt, P. Pilewskie, W. J. Gore, R. W. Bergstrom, 
!	M. Roman, J. Redemann, P. B.Russell, J. Liu, and C. C. Schaaf.  Aircraft measurements of 
!	spectral surface albedo and its consistency with ground-based and space-borne observations.  
!	J. Geophys. Res.  113 (D17209), 2008.
!
!9	Dozier, J.  Spectral signature of alpine snow cover from the Landsat Thematic Mapper.  
!	Remote Sens. Environ.  28:9-22, 1989.
!
!10	Dozier, J., R. E. Davis, and R. Perla.  On the objective analysis of snow microstructure.  
!	Avalanche Formation, Movement and Effects (proceedings of the Davos Symposium, Sept. 1986.  
!	IAHS Publ. no. 162, 1987.
!
!11	Dozier, J. and T. H. Painter.  Multispectral and hyperspectral remote sensing of alpine 
!	snow properties.  Annu. Rev. Earth Planet. Sci. 32:465-94, 2004.
!
!12	Dozier, J, and S. G. Warren.  Effect of viewing angle on the infrared brightness 
!	temperature of snow.  Water Resources Research, 18(5) 1424-1434, 1982.
!
!13	Feister, U., and R. Grewe. Spectral albedo measurements in the uv and visible region 
!	over different types of surfaces.  Photochem.Photbiol. 62(4) 736-744,  1995.
!
!14	Fu, Qiang, and K.N. Liou.  ON the Correlated k-Distribution Moethod for Radiative 
!	Transfer in Nonhomogeneous Atmospheres.  J. Atmos. Sci.  49(22) 2139-2156, 1992.
!
!15	Grenfell, T. C. and D. K. Perovich.  Spectral albedos of sea ice and incident solar 
!	irradiance in the Southern Beaufort Sea, J. Geophys. Res., 89, 3573-3580.  1984.
!
!16	Grenfell, T. C., S. G. Warren, and P. C. Mullen.  Reflection of Solar Radiation By 
!	The Antarctic Snow Surface at Ultraviolet, Visible, and Near-Infrared Wavelengths, J. Geophys. Res., 99, 18669-18684, 1994. 
!
!17	Hansen, F. V.  Albedos.  Army Research Laboratory.  ARL-TR-57, 1993.
!
!18	Jin, Y, C.B. Schaaf, F. Gao, X. Li, and A.H. Strahler.  How does snow impact
!	the albedo of vegetated land surfaces as analysized with MODIS data?  Geophys. Res.
!	Lett.  29(10) 1374.  2002
!19	Koelemeijer, R. B. A., J. F. de Haan, and P. Stammes.  A database of spectral surface 
!	reflectivity in the range 335-772 nm derived from 5.5 years of GOME observations.  
!	J. Geophys. Res. 108(D2) 4070, 2003.
!
!20	Laepple, T. and M. G. Schultz.  Improved albedo formulation for chemistry transport 
!	models based on satellite observations and assimilated snow data and its impact on tropospheric 
!	photochemistry.  J. Geophys. Res.  110(D) 11308,  2005.
!
!21	Lundberg, A, and J. Beringer.  Albedo and snowmelt rates across a trundra-to-forest 
!	transition.  15th Inter. Northern Research Basins Symposium and Workshop, Lulea, Sweden, 
!	29 Aug ? 2 Sep, 2005.
!
!22	Michalsky, J. J., Q. L. Min, J. C. Barnard, R. T.Marchand, P. Pilewskie.  Simultaneous 
!	spectral albedo measurements near the ARM SGP central facility.  Twelfth ARM Science Team Meeting 
!	Proceedings, St. Petersburg, FL, USA.  April 8-12, 2002.
!
!23	Moody, E.G, M.D. King, S. Platnick, C.B. Schaaf, F. Gao.  Spatially complete global spectral 
!	surface albedos:  value-added datasets derived from Terra MODIS land products.  IEEE Trans. 
!	On Geoscience and Remote Sens.  43(1) 144-158.  2005.  
!
!24	Moody, E.G, M.D. King, C.B. Schaaf, D.K. Hall, and S. Platnick.  Northern Hemisphere 
!	five-year average (2000-2004) spectral albedos of surfaces in the presence of snow: Statistics 
!	computed from Terra MODIS land products.  Remote Sens. Environment. 111, 337-345.  2007.
!
!25	Payne, R. E., Albedo of the Sea Surface, J. Atmos. Sci., 29, 959-970, 1972. 
!
!26	Petzold, D. E., and A. N. Rencz.  The albedo of selected subarctic surfaces.  Arctic and 
!	Alpine Res. 7(4) 393-8,  1975.
!
!27	Pinker, R. T., and A. Karnieli. Characteristic Spectral Reflectance of a semi-arid environment, 
!	Int. J. Rem. Sens., 1995. 
!
!28	Privette, J. L, T. F. Eck, D. W. Deering.  Estimating spectral albedo and nadir reflectance 
!	through inversion of simple BRDF models with AVHRR/MODIS-like data.  
!	J. Geophys. Res. 102(D24)  29,529-29,542,  1997.
!
!29	Roesch, A., C. Schaaf, and F. Gao.  Use of Moderate-Resolution Imaging Spectroradiometer 
!	bidirectional reflectance distribution function products to enhance simulated surface albedos.  
!	J. Geophys. Res. 109(D) 12,105, 2004.
!
!30	Staylor W. F. and A.C. Wilber. Global surface albedos estimated from ERBE data, 
!	Proceedings, 7th Conf. on Atmos. Rad., San Francisco, CA, 1990. 
!
!31	Taberner, M. B. Pinty, Y. Govaerts, S. Liang, M.M. Verstraete, N. Gobron, and J.L. Widlowski.  
!	Comparison of MISR and MODIS land surface albedos: methodology.  
!	J. Geophys. Res.  115(D) 05101, 2010.
!
!32	Tsuyuzaki, S., K Kushida, Y Kodama.   Recovery of surface albedo and plant cover
!	after wildfire in a Picea mariana forest in interior Alaska.  Climatic Change 93:517?525, 2009.
!
!33	Wilber, A.C., D. P. Kratz, S. K. Gupta, Surface Emissivity Maps for Use of Satellite 
!	Retrievals of Longwave Radiation, NASA Tech. Pub., TP-99-209362, 1999.
!
!34	Wendisch, M. P. Pilewskie, E. Jakel, S. Schmidt, J. Pommier, S. Howard, H.H. Jonsson, 
!	H Guan, M. Schroder, and B. Mayer.  Airborne measurements of areal spectral surface albedo over 
!	different sea and land surfaces.  J. Geophys. Res. 109(D) 08203,  2004.
!
!35	Warren, S. G., and W. J. Wiscombe.  A model for the spectral albedo of snow.  
!	II: Snow containing atmospheric aerosols.  J. Atmosph. Sci.  37(12) 2734-45, 1980.
!
