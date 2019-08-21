
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
C $Header: /project/work/rep/arc/CCTM/src/depv/m3dry/LSM_MOD.F,v 1.5 2012/01/19 14:23:58 yoj Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module LSM_Mod
       
C------------------------------------------------------------------------------
C Revision History: 
C      June 16 11  Created by J. Bash
C      April 19 12  J. Bash    Updated the LU_FAC data arrays to be a function
C                              of the annual total N deposition following 
C                              Massad et al 2010 doi:10.5194/acp-10-10359-2010
C                              The annual 2002 deposition filed from a previous bi-
C                              directional model run using values ~ 100 was used. 
C                              The model is not very sensitive to this paramater
C                              and using different annual deposition fileds would
C                              result in small changes. 
C      Sept 12 12  D. Schwede  Added NLCD40 land use classification.
C                              Also some changes made to values in tables for NLCD50.
C                              Maintain compatability with previous versions of MCIP and detect NLCD-MODIS
C                              as the same as NLCD50
C      Nov  5  12  D. Schwede  Modified albedo for NLCD pasture and grass categories so that they are more
C                              consistent with the MODIS and USGS values
C      Apr  4  13 J. Bash      Added general water, forest, shrub, grass, snow/ice, 
C                              agricultural land use classification in a land use type 
C                              to simplify how bidirecitonal NH3 code handles different 
C                              land use data sets. 
C      Aug  22 14 J. Bash      Moved all the data to defined data types for easier updates and data access. 
C      June 2018  j. pleim     Removed all LSM specific data
C      Feb. 2019  D. Wong      Implemented centralized I/O approach
C      25 Jul 19 D.Wong        Based on logical variable WRF_V4P defined in RUNTIME_VARS to handle
C                              various number of soil type from different WRF version
C------------------------------------------------------------------------------
       
      Implicit None

      INTEGER, PARAMETER :: N_SOIL_TYPE_WRFV4P = 16
      INTEGER, PARAMETER :: N_SOIL_TYPE_WRFV3  = 11
      INTEGER :: N_SOIL_TYPE
      
      REAL, ALLOCATABLE  :: wwlt (:)
      REAL, ALLOCATABLE  :: wsat (:)
      REAL, ALLOCATABLE  :: bslp (:)
      REAL, ALLOCATABLE  :: wres (:)
      REAL, ALLOCATABLE  :: wfc  (:)
      REAL, ALLOCATABLE  :: rhob (:)

      REAL,   SAVE, ALLOCATABLE  :: LUF_FAC   ( : )
      REAL,   SAVE, ALLOCATABLE  :: HGLU_FAC  ( : )
      INTEGER, SAVE              :: n_lufrac
      CHARACTER( 80 ), SAVE      :: LAND_SCHEME
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: cat_lu( : )
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: name_lu( : )


      INTEGER, PRIVATE, PARAMETER :: n_modis   = 20
      INTEGER, PRIVATE, PARAMETER :: n_nlcd50  = 50
      INTEGER, PRIVATE, PARAMETER :: n_nlcd40  = 40
      INTEGER, PRIVATE, PARAMETER :: n_usgs    = 24
      INTEGER, PRIVATE            :: ALLOCSTAT
      
      TYPE lu_type
         CHARACTER( 16 ) :: name
         CHARACTER( 16 ) :: lu_cat
         REAL            :: LUF_FAC ! NH3 bidi stomatal gamma
         REAL            :: HGLU_FAC ! Hg bidi soil conc
      END TYPE lu_type


C-------------------------------------------------------------------------------
C Soil Characteristics by Type for WRFV38
C
C   #  SOIL TYPE  WSAT  WFC  WWLT  BSLP  CGSAT   JP   AS   C2R  C1SAT  WRES
C   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____  ____
C   1  SAND       .395 .135  .068  4.05  3.222    4  .387  3.9  .082   .020
C   2  LOAMY SAND .410 .150  .075  4.38  3.057    4  .404  3.7  .098   .035
C   3  SANDY LOAM .435 .195  .114  4.90  3.560    4  .219  1.8  .132   .041
C   4  SILT LOAM  .485 .255  .179  5.30  4.418    6  .105  0.8  .153   .015
C   5  LOAM       .451 .240  .155  5.39  4.111    6  .148  0.8  .191   .027
C   6  SND CLY LM .420 .255  .175  7.12  3.670    6  .135  0.8  .213   .068
C   7  SLT CLY LM .477 .322  .218  7.75  3.593    8  .127  0.4  .385   .040
C   8  CLAY LOAM  .476 .325  .250  8.52  3.995   10  .084  0.6  .227   .075
C   9  SANDY CLAY .426 .310  .219 10.40  3.058    8  .139  0.3  .421   .109
C  10  SILTY CLAY .482 .370  .283 10.40  3.729   10  .075  0.3  .375   .056
C  11  CLAY       .482 .367  .286 11.40  3.600   12  .083  0.3  .342   .090
C
C-------------------------------------------------------------------------------
!-- WSAT is saturated soil moisture (M^3/M^3) (JN90)
      REAL, PARAMETER :: WSAT_WRFV3(N_SOIL_TYPE_WRFV3) =  
     &      (/ 0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477,
     &         0.476, 0.426, 0.482, 0.482 /)
!-- WFC is soil field capacity (Rawls et al 1982)     
      REAL, PARAMETER :: WFC_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/ 0.135, 0.150, 0.195, 0.255, 0.240, 0.255, 0.322,
     &         0.325, 0.310, 0.370, 0.367 /)
!-- WWLT is wilting point (M^3/M^3) (JN90)
      REAL, PARAMETER :: WWLT_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/ 0.068, 0.075, 0.114, 0.179, 0.155, 0.175, 0.218,
     &         0.250, 0.219, 0.283, 0.286 /)
!-- B is slop of the retention curve (NP89)
      REAL, PARAMETER :: BSLP_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/  4.05,  4.38,  4.90,  5.30,  5.39,  7.12,  7.75,
     &          8.52, 10.40, 10.40, 11.40 /)
!-- WRES is residual soil moisture (Rawls et al 1982)
      REAL, PARAMETER :: WRES_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/  0.020, 0.035, 0.041, 0.015, 0.027, 0.068, 0.040,
     &          0.075, 0.109, 0.056, 0.090 /)
! -- RHOB is the soil bulk dnesity following Lyons et al 1997   
      REAL, PARAMETER :: RHOB_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/ 1.59e6, 1.55e6, 1.53e6, 1.53e6, 1.55e6, 1.62e6, 1.67e6,
     &         1.66e6, 1.83e6, 1.78e6, 1.83e6 /)

C-------------------------------------------------------------------------------
C Soil Characteristics by Type for WRF4+
C
C   #  SOIL TYPE  WSAT  WFC  WWLT  BSLP  CGSAT   JP   AS   C2R  C1SAT  WRES
C   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____  ____
C   1  SAND       .395 .135  .068  4.05  3.222    4  .387  3.9  .082   .020
C   2  LOAMY SAND .410 .150  .075  4.38  3.057    4  .404  3.7  .098   .035
C   3  SANDY LOAM .435 .195  .114  4.90  3.560    4  .219  1.8  .132   .041
C   4  SILT LOAM  .485 .255  .179  5.30  4.418    6  .105  0.8  .153   .015
C   5  SILT       .480 .260  .150  5.30  4.418    6  .105  0.8  .153   .020
C   6  LOAM       .451 .240  .155  5.39  4.111    6  .148  0.8  .191   .027
C   7  SND CLY LM .420 .255  .175  7.12  3.670    6  .135  0.8  .213   .068
C   8  SLT CLY LM .477 .322  .218  7.75  3.593    8  .127  0.4  .385   .040
C   9  CLAY LOAM  .476 .325  .250  8.52  3.995   10  .084  0.6  .227   .075
C  10  SANDY CLAY .426 .310  .219 10.40  3.058    8  .139  0.3  .421   .109
C  11  SILTY CLAY .482 .370  .283 10.40  3.729   10  .075  0.3  .375   .056
C  12  CLAY       .482 .367  .286 11.40  3.600   12  .083  0.3  .342   .090
C  13  ORGANICMAT .451 .240  .155  5.39  4.111    6  .148  0.8  .191   .027
C  14  WATER      .482 .367  .286 11.40  3.600   12  .083  0.3  .342   .090
C  15  BEDROCK    .482 .367  .286 11.40  3.600   12  .083  0.3  .342   .090
C  16  OTHER      .420 .255  .175  7.12  3.670    6  .135  0.8  .213   .068
C-------------------------------------------------------------------------------

!-- WSAT is saturated soil moisture (M^3/M^3) (JN90)
      REAL, PARAMETER :: WSAT_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  0.395, 0.410, 0.435, 0.485, 0.480, 0.451, 0.420, 0.477,
     &          0.476, 0.426, 0.482, 0.482, 0.451, 0.482, 0.482, 0.420 /)
!-- WFC is soil field capacity (Rawls et al 1982)     
      REAL, PARAMETER :: WFC_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  0.135, 0.150, 0.195, 0.255, 0.260, 0.240, 0.255, 0.322,
     &          0.325, 0.310, 0.370, 0.367, 0.240, 0.367, 0.367, 0.255 /)
!-- WWLT is wilting point (M^3/M^3) (JN90)
      REAL, PARAMETER :: WWLT_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  0.068, 0.075, 0.114, 0.179, 0.150, 0.155, 0.175, 0.218,
     &          0.250, 0.219, 0.283, 0.286, 0.155, 0.286, 0.286, 0.175 /)
!-- B is slop of the retention curve (NP89)
      REAL, PARAMETER :: BSLP_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,  7.75,
     &          8.52, 10.40, 10.40, 11.40,  5.39, 11.40, 11.40,  7.12 /)
!-- WRES is residual soil moisture (Rawls et al 1982)
      REAL, PARAMETER :: WRES_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  0.020, 0.035, 0.041, 0.015, 0.020, 0.027, 0.068, 0.040,
     &          0.075, 0.109, 0.056, 0.090, 0.027, 0.090, 0.090, 0.068 /)
! -- RHOB is the soil bulk dnesity following Lyons et al 1997   
      REAL, PARAMETER :: RHOB_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  1.59e6, 1.55e6, 1.53e6, 1.53e6, 1.53e6, 1.55e6, 1.62e6, 1.67e6,
     &          1.66e6, 1.83e6, 1.78e6, 1.83e6, 1.62e6, 1.83e6, 1.83e6, 1.67e6 /)


!*****************************************************************************
!*****************************************************************************
! MODIS-ONLY (1XX)/MODIS (2XX) LU characterization ... reclassified by NCEP
!-------------------------------------------------------------------------------
      
      TYPE( lu_type ),PRIVATE, PARAMETER :: MODIS( n_modis ) =  (/
!               -Name---    -LU cat - LUF_FAC HGLU_FAC     Cat Desc.
     & lu_type('NCEP_1 ', 'EVEFOR ', 246.0,  40.0),  ! 1 Evergreen Needleleaf Forest
     & lu_type('NCEP_2 ', 'EVEFOR ', 247.0, 130.0),  ! 2 Evergreen Broadleaf Forest
     & lu_type('NCEP_3 ', 'DECFOR ', 267.0,  81.0),  ! 3 Deciduous Needleleaf Forest
     & lu_type('NCEP_4 ', 'DECFOR ', 268.0,  81.0),  ! 4 Deciduous Broadleaf Forest  
     & lu_type('NCEP_5 ', 'MIXFOR ', 252.0, 139.0),  ! 5 Mixed Forest
     & lu_type('NCEP_6 ', 'SHRUB  ', 246.0, 125.0),  ! 6 Closed Shrublands
     & lu_type('NCEP_7 ', 'SHRUB  ', 246.0, 125.0),  ! 7 Open Shrublands
     & lu_type('NCEP_8 ', 'MIXFOR ', 246.0, 125.0),  ! 8 Woody Savanna
     & lu_type('NCEP_9 ', 'MIXFOR ', 246.0, 125.0),  ! 9 Savanna
     & lu_type('NCEP_10', 'GRASS  ', 247.0,  20.0),  ! 10 Grasslands
     & lu_type('NCEP_11', 'WETLAND', 260.0, 125.0),  ! 11 Perminent Wetlands
     & lu_type('NCEP_12', 'AG     ', 340.0,  81.0),  ! 12 Croplands  
     & lu_type('NCEP_13', 'BUILT  ', 338.0, 190.0),  ! 13 Urban and Built-up    
     & lu_type('NCEP_14', 'AGMOS  ', 298.0,  50.0),  ! 14 Crop/Natural Mosaic
     & lu_type('NCEP_15', 'SNOWICE',   0.0,   0.0),  ! 15 Snow and Ice   
     & lu_type('NCEP_16', 'BARREN ', 246.0, 125.0),  ! 16 Sparsely Vegetated
     & lu_type('NCEP_17', 'WATER  ',   0.0,   0.0),  ! 17 IGBP water
     & lu_type('NCEP_18', 'EVEFOR ', 246.0,  62.0),  ! 18 Wooded Tundra   
     & lu_type('NCEP_19', 'SHRUB  ', 246.0,  62.0),  ! 19 Mixed Tundra
     & lu_type('NCEP_20', 'GRASS  ', 246.0,  18.0)/) ! 20 Barren Tundra

!****************************************************************************************
!****************************************************************************************
! 50 CLASS NLCD (US only, cats 1-30)/MODIS (Outside US, cats 31-50) LU characterization
!---------------------------------------------------------------------------------------

      TYPE( lu_type ),PRIVATE, PARAMETER :: NLCD50( n_nlcd50 ) =  (/
!               -Name---    -LU cat - LUF_FAC HGLU_FAC     Cat Desc.
     & lu_type('NLCD_11  ', 'WATER  ',   0.0,   0.0),  ! 1 Open water
     & lu_type('NLCD_12  ', 'SNOWICE',   0.0,   0.0),  ! 2 Perennial Ice/snow
     & lu_type('NLCD_21  ', 'BUILT  ', 312.0, 130.0),  ! 3 Developed, Open space
     & lu_type('NLCD_22  ', 'BUILT  ', 305.0, 150.0),  ! 4 Developed, Low Intensity
     & lu_type('NLCD_23  ', 'BUILT  ', 331.0, 170.0),  ! 5 Developed, Medium Intensity
     & lu_type('NLCD_24  ', 'BUILT  ', 402.0, 190.0),  ! 6 Developed, High Intensity
     & lu_type('NLCD_31  ', 'BARREN ', 246.0, 125.0),  ! 7 Barren land
     & lu_type('NLCD_32  ', 'BARREN ', 248.0,   0.0),  ! 8 Unconsolidated Shore
     & lu_type('NLCD_41  ', 'DECFOR ', 274.0,  81.0),  ! 9 Deciduous Forest
     & lu_type('NLCD_42  ', 'EVEFOR ', 246.0,  81.0),  ! 10 Evergreen Forest
     & lu_type('NLCD_43  ', 'MIXFOR ', 254.0, 139.0),  ! 11 Mixed Forest
     & lu_type('NLCD_51  ', 'SHRUB  ', 246.0,  73.0),  ! 12 Dwarf Scrub
     & lu_type('NLCD_52  ', 'SHRUB  ', 246.0, 125.0),  ! 13 Shrub/Scrub
     & lu_type('NLCD_71  ', 'GRASS  ', 247.0,  20.0),  ! 14 Grassland/Herbaceous
     & lu_type('NLCD_72  ', 'GRASS  ', 246.0, 125.0),  ! 15 Sedge/Herbaceous
     & lu_type('NLCD_73  ', 'BARREN ',   0.0,  18.0),  ! 16 Lichens
     & lu_type('NLCD_74  ', 'BARREN ',   0.0,  18.0),  ! 17 Moss
     & lu_type('NLCD_75  ', 'BARREN ', 246.0,  62.0),  ! 18 Tundra
     & lu_type('NLCD_81  ', 'AG     ', 344.0,  50.0),  ! 19 Pasture/Hay
     & lu_type('NLCD_82  ', 'AG     ', 340.0,  80.0),  ! 20 Cultivated Crops   
     & lu_type('NLCD_90  ', 'WETLAND', 271.0, 125.0),  ! 21 Woody Wetland
     & lu_type('NLCD_91  ', 'WETLAND', 267.0, 125.0),  ! 22 Palustrine Forested Wetland   
     & lu_type('NLCD_92  ', 'WETLAND', 254.0, 125.0),  ! 23 Palustrine Shrub Wetland   
     & lu_type('NLCD_93  ', 'WETLAND', 267.0, 125.0),  ! 24 Estuarine Forested Wetland   
     & lu_type('NLCD_94  ', 'WETLAND', 267.0, 125.0),  ! 25 Estuarine Shrub Wetland   
     & lu_type('NLCD_95  ', 'WETLAND', 248.0, 125.0),  ! 26 Emergent Herbaceous Wetland   
     & lu_type('NLCD_96  ', 'WETLAND', 269.0, 125.0),  ! 27 Palustrine Emergent Wetland   
     & lu_type('NLCD_97  ', 'WETLAND', 262.0, 125.0),  ! 28 Estuarine Emergent Wetland   
     & lu_type('NLCD_98  ', 'WETLAND', 277.0, 125.0),  ! 29 Palustrine Aquatic Bed
     & lu_type('NLCD_99  ', 'WETLAND', 247.0, 125.0),  ! 30 Estuarine Aquatic Bed   
     & lu_type('MODIS_0  ', 'WATER  ',   0.0,   0.0),  ! 31 Open water (mapped to 1)
     & lu_type('MODIS_1  ', 'EVEFOR ', 246.0,  40.0),  ! 32 Evergreen Needleleaf Forest
     & lu_type('MODIS_2  ', 'EVEFOR ', 247.0, 130.0),  ! 33 Evergreen Broadleaf Forest
     & lu_type('MODIS_3  ', 'DECFOR ', 267.0,  81.0),  ! 34 Deciduous Needleleaf Forest
     & lu_type('MODIS_4  ', 'DECFOR ', 268.0,  81.0),  ! 35 Deciduous Broadleaf Forest  
     & lu_type('MODIS_5  ', 'MIXFOR ', 252.0, 139.0),  ! 36 Mixed Forest
     & lu_type('MODIS_6  ', 'SHRUB  ', 246.0, 125.0),  ! 37 Closed Shrublands
     & lu_type('MODIS_7  ', 'SHRUB  ', 246.0, 125.0),  ! 38 Open Shrublands
     & lu_type('MODIS_8  ', 'MIXFOR ', 246.0, 125.0),  ! 39 Woody Savanna
     & lu_type('MODIS_9  ', 'MIXFOR ', 246.0, 125.0),  ! 40 Savanna
     & lu_type('MODIS_10 ', 'GRASS  ', 247.0,  20.0),  ! 41 Grasslands
     & lu_type('MODIS_11 ', 'WETLAND', 260.0, 125.0),  ! 42 Perminent Wetlands
     & lu_type('MODIS_12 ', 'AG     ', 340.0,  81.0),  ! 43 Croplands  
     & lu_type('MODIS_13 ', 'BUILT  ', 338.0, 190.0),  ! 44 Urban and Built-up    
     & lu_type('MODIS_14 ', 'AGMOS  ', 298.0,  50.0),  ! 45 Crop/Natural Mosaic
     & lu_type('MODIS_15 ', 'SNOWICE',   0.0,   0.0),  ! 46 Snow and Ice   
     & lu_type('MODIS_16 ', 'BARREN ', 246.0, 125.0),  ! 47 Sparsely Vegetated
     & lu_type('MODIS_0  ', 'WATER  ',   0.0,   0.0),  ! 48 IGBP water
     & lu_type('MODIS_255', 'SNOWICE',   0.0,   0.0),  ! 49 unclassified   
     & lu_type('MODIS_255', 'WATER  ',   0.0,   0.0)/) ! 50 fill (normally ocean water)                                               
                                               
!****************************************************************************************
!****************************************************************************************
! 40 CLASS MODIS (Outside US, cats 1-20)/NLCD (US only, cats 21-40) LU characterization
!-------------------------------------------------------------------------------

       TYPE( lu_type ),PRIVATE, PARAMETER :: NLCD40( n_nlcd40 ) =  (/
!               -Name---    -LU cat - LUF_FAC HGLU_FAC     Cat Desc.
     & lu_type('MODIS_1  ', 'EVEFOR ', 246.0,  40.0),  ! 1 Evergreen Needleleaf Forest
     & lu_type('MODIS_2  ', 'EVEFOR ', 247.0, 130.0),  ! 2 Evergreen Broadleaf Forest
     & lu_type('MODIS_3  ', 'DECFOR ', 267.0,  81.0),  ! 3 Deciduous Needleleaf Forest
     & lu_type('MODIS_4  ', 'DECFOR ', 268.0,  81.0),  ! 4 Deciduous Broadleaf Forest  
     & lu_type('MODIS_5  ', 'MIXFOR ', 252.0, 139.0),  ! 5 Mixed Forest
     & lu_type('MODIS_6  ', 'SHRUB  ', 246.0, 125.0),  ! 6 Closed Shrublands
     & lu_type('MODIS_7  ', 'SHRUB  ', 246.0, 125.0),  ! 7 Open Shrublands
     & lu_type('MODIS_8  ', 'MIXFOR ', 246.0, 125.0),  ! 8 Woody Savanna
     & lu_type('MODIS_9  ', 'MIXFOR ', 246.0, 125.0),  ! 9 Savanna
     & lu_type('MODIS_10 ', 'GRASS  ', 247.0,  20.0),  ! 10 Grasslands
     & lu_type('MODIS_11 ', 'WETLAND', 260.0, 125.0),  ! 11 Perminent Wetlands
     & lu_type('MODIS_12 ', 'AG     ', 340.0,  81.0),  ! 12 Croplands  
     & lu_type('MODIS_13 ', 'BUILT  ', 338.0, 190.0),  ! 13 Urban and Built-up    
     & lu_type('MODIS_14 ', 'AGMOS  ', 298.0,  50.0),  ! 14 Crop/Natural Mosaic
     & lu_type('MODIS_15 ', 'SNOWICE',   0.0,   0.0),  ! 15 Snow and Ice   
     & lu_type('MODIS_16 ', 'BARREN ', 246.0, 125.0),  ! 16 Sparsely Vegetated
     & lu_type('MODIS_0  ', 'WATER  ',   0.0,   0.0),  ! 17 IGBP water
     & lu_type('MODIS_255', 'WATER  ',   0.0,   0.0),  ! 18 unclassified   
     & lu_type('MODIS_255', 'WATER  ',   0.0,   0.0),  ! 19 fill (normally ocean water)
     & lu_type('MODIS_255', 'WATER  ',   0.0,   0.0),  ! 20 unclassified   
     & lu_type('NLCD_11  ', 'WATER  ',   0.0,   0.0),  ! 21 Open water
     & lu_type('NLCD_12  ', 'SNOWICE',   0.0,   0.0),  ! 22 Perennial Ice/snow
     & lu_type('NLCD_21  ', 'BUILT  ', 312.0, 130.0),  ! 23 Developed, Open space
     & lu_type('NLCD_22  ', 'BUILT  ', 305.0, 150.0),  ! 24 Developed, Low Intensity
     & lu_type('NLCD_23  ', 'BUILT  ', 331.0, 170.0),  ! 25 Developed, Medium Intensity
     & lu_type('NLCD_24  ', 'BUILT  ', 402.0, 190.0),  ! 26 Developed, High Intensity
     & lu_type('NLCD_31  ', 'BARREN ', 246.0, 125.0),  ! 27 Barren land
     & lu_type('NLCD_41  ', 'DECFOR ', 274.0,  81.0),  ! 28 Deciduous Forest
     & lu_type('NLCD_42  ', 'EVEFOR ', 246.0,  81.0),  ! 29 Evergreen Forest
     & lu_type('NLCD_43  ', 'MIXFOR ', 254.0, 139.0),  ! 30 Mixed Forest
     & lu_type('NLCD_51  ', 'SHRUB  ', 246.0,  73.0),  ! 31 Dwarf Scrub
     & lu_type('NLCD_52  ', 'SHRUB  ', 246.0, 125.0),  ! 32 Shrub/Scrub
     & lu_type('NLCD_71  ', 'GRASS  ', 247.0,  20.0),  ! 33 Grassland/Herbaceous
     & lu_type('NLCD_72  ', 'GRASS  ', 246.0, 125.0),  ! 34 Sedge/Herbaceous
     & lu_type('NLCD_73  ', 'BARREN ',   0.0,  18.0),  ! 35 Lichens
     & lu_type('NLCD_74  ', 'BARREN ',   0.0,  18.0),  ! 36 Moss
     & lu_type('NLCD_81  ', 'AG     ', 344.0,  50.0),  ! 37 Pasture/Hay
     & lu_type('NLCD_82  ', 'AG     ', 340.0,  80.0), ! 38 Cultivated Crops   
     & lu_type('NLCD_90  ', 'WETLAND', 271.0, 125.0),  ! 39 Woody Wetland 
     & lu_type('NLCD_95  ', 'WETLAND', 248.0, 125.0)/) ! 40 Emergent Herbaceous Wetland

!*****************************************************************************
!*****************************************************************************
!  USGS LU characterization  --  note below may not match those for the same category in a diff
!                                classification system.  This was done to maintain compatibility with MM5.               
!-----------------------------------------------------------------------------                   

      TYPE( lu_type ), PRIVATE, PARAMETER :: USGS24( n_usgs ) =  (/
!               ---Name---  -LU cat - LUF_FAC  HGLU_FAC    Cat Desc.
     & lu_type('USGS_Urban', 'BUILT   ', 338.0, 190.0),  ! 1 Urban or Built-up Land
     & lu_type('USGS_DrCrp', 'AG      ', 340.0,  80.0),  ! 2 Dryland Crop and Pasture
     & lu_type('USGS_IrCrp', 'AG      ', 340.0,  32.0),  ! 3 Irr. Cropland and Pasture
     & lu_type('USGS_MixCp', 'AG      ', 340.0,  56.0),  ! 4 Mixed Dry/Irr Crop and Past
     & lu_type('USGS_CrGrM', 'AGMOS   ', 298.0,  50.0),  ! 5 Grassland/Cropland Mosaic
     & lu_type('USGS_CrWdM', 'AGMOS   ', 298.0,  26.0),  ! 6 Woodland/Cropland Mosaic
     & lu_type('USGS_GrsLd', 'GRASS   ', 247.0,  20.0),  ! 7 Grassland
     & lu_type('USGS_ShrLd', 'SHRUB   ', 246.0, 125.0),  ! 8 Shrubland
     & lu_type('USGS_ShrGr', 'SHRUB   ', 246.0,  73.0),  ! 9 Mixed Shrubland/Grassland
     & lu_type('USGS_Savan', 'MIXFOR  ', 246.0,  51.0),  ! 10 Savanna
     & lu_type('USGS_DBFst', 'DECFOR  ', 268.0,  81.0),  ! 11 Broadleaf Deciduous Forest
     & lu_type('USGS_DNFst', 'DECFOR  ', 267.0,  81.0),  ! 12 Deciduous Coniferous Forest
     & lu_type('USGS_EBFst', 'EVEFOR  ', 247.0, 130.0),  ! 13 Evergreen Broadleaf Forest
     & lu_type('USGS_ENFst', 'EVEFOR  ', 246.0,  40.0),  ! 14 Evergreen Coniferous Forest
     & lu_type('USGS_MxFst', 'MIXFOR  ', 252.0, 139.0),  ! 15 Mixed forest
     & lu_type('USGS_Water', 'WATER   ',   0.0,   0.0),  ! 16 Water
     & lu_type('USGS_HWtld', 'WETLAND ', 248.0, 125.0),  ! 17 Herbaceous Wetland
     & lu_type('USGS_WWtld', 'WETLAND ', 260.0, 125.0),  ! 18 Forested Wetlands
     & lu_type('USGS_BarSp', 'BARREN  ', 246.0,  26.0),  ! 19 Barren or Sparsely Vegetated
     & lu_type('USGS_HrTun', 'GRASS   ', 246.0,  62.0),  ! 20 Herbaceous Tundra
     & lu_type('USGS_WdTun', 'SHRUB   ', 246.0,  62.0),  ! 21 Shrub and Brush Tundra
     & lu_type('USGS_MxTun', 'SHRUB   ', 246.0,  62.0),  ! 22 Mixed Tundra
     & lu_type('USGS_BGTun', 'BARREN  ', 246.0,  18.0),  ! 23 Bare Ground Tundra
     & lu_type('USGS_SnwIc', 'SNOWICE ',   0.0,   0.0)/) ! 24 Snowfields or Glaciers

      CONTAINS                                 
         SUBROUTINE INIT_LSM( JDate, JTime )   
                                               
         USE HGRD_DEFN                         
         USE UTILIO_DEFN                       
         USE RUNTIME_VARS, ONLY : WRF_V4P
#ifdef twoway                                  
         USE twoway_data_module, ONLY : num_land_cat
#endif                                         
                                               
         IMPLICIT NONE                         
                                               
         INCLUDE SUBST_FILES_ID  ! file name parameters         
                                               
         INTEGER, INTENT( In )  :: jdate       
         INTEGER, INTENT( In )  :: jtime  
         CHARACTER( 240 )       :: XMSG = ' '  
         CHARACTER(  16 ), SAVE :: PNAME = 'Init_LSM'
         INTEGER l
         INTEGER :: STAT

         LOGICAL, SAVE :: INITIALIZED = .FALSE.

         IF( INITIALIZED ) RETURN
         INITIALIZED = .TRUE.
                                             
         IF (WRF_V4P) THEN
            N_SOIL_TYPE = N_SOIL_TYPE_WRFV4P
         ELSE
            N_SOIL_TYPE = N_SOIL_TYPE_WRFV3
         END IF

         ALLOCATE (WSAT(N_SOIL_TYPE),
     &             WFC(N_SOIL_TYPE),
     &             WWLT(N_SOIL_TYPE),
     &             BSLP(N_SOIL_TYPE),
     &             WRES(N_SOIL_TYPE),
     &             RHOB(N_SOIL_TYPE),
     &             STAT=STAT)

         IF (WRF_V4P) THEN
            WSAT = WSAT_WRFV4P
            WFC  = WFC_WRFV4P
            WWLT = WWLT_WRFV4P
            BSLP = BSLP_WRFV4P
            WRES = WRES_WRFV4P
            RHOB = RHOB_WRFV4P
         ELSE
            WSAT = WSAT_WRFV3
            WFC  = WFC_WRFV3
            WWLT = WWLT_WRFV3
            BSLP = BSLP_WRFV3
            WRES = WRES_WRFV3
            RHOB = RHOB_WRFV3
         END IF

#ifdef twoway                                  
         IF ( NUM_LAND_CAT .EQ. 24 ) THEN      
            LAND_SCHEME = 'USGS24'             
         ELSE IF ( NUM_LAND_CAT .EQ. 20 ) THEN 
            LAND_SCHEME = 'MODIS'              
         ELSE IF ( NUM_LAND_CAT .EQ. 50 ) THEN 
            LAND_SCHEME = 'NLCD50'         
         ELSE IF ( NUM_LAND_CAT .EQ. 40 ) THEN 
            LAND_SCHEME = 'NLCD40'         
         END IF                                
#endif                                         
!       Write( *,* ) 'Land use scheme is ', TRIM(LAND_SCHEME) 
                                               
         SELECT CASE( LAND_SCHEME )            
            CASE( 'USGS24' )                   
               n_lufrac    = n_usgs
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = USGS24%lu_cat
               name_lu  = USGS24%name
               LUF_FAC  = USGS24%LUF_FAC
               HGLU_FAC = USGS24%HGLU_FAC
            CASE( 'MODIS' )                    
               n_lufrac    = n_modis 
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = MODIS%lu_cat
               name_lu  = MODIS%name
               LUF_FAC  = MODIS%LUF_FAC
               HGLU_FAC = MODIS%HGLU_FAC
            CASE( 'NLCD50' )               
               n_lufrac    = n_nlcd50  
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = NLCD50%lu_cat
               name_lu  = NLCD50%name
               LUF_FAC  = NLCD50%LUF_FAC
               HGLU_FAC = NLCD50%HGLU_FAC
            CASE( 'NLCD40' )               
               n_lufrac    = n_nlcd40   
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = NLCD40%lu_cat
               name_lu  = NLCD40%name
               LUF_FAC  = NLCD40%LUF_FAC
               HGLU_FAC = NLCD40%HGLU_FAC
            CASE DEFAULT                       
               xmsg = 'Land use scheme not supported'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END SELECT                            
                                               
         END SUBROUTINE Init_LSM               
                                               
      END MODULE LSM_Mod                       
