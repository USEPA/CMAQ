
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/LSM_MOD.F,v 1.1 2011/09/30 13:06:39 sjr Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module LSM_Mod
       
C
C Revision History: J. Bash June 16 11: Created
       
      Implicit None
      
      REAL,   SAVE               :: wwlt (11)
      REAL,   SAVE               :: wsat (11)
      REAL,   SAVE               :: bslp (11)
      REAL,   SAVE               :: wres (11)
      Real,   SAVE               :: wfc  (11)
      Real,   SAVE               :: rhob (11)
      REAL,   SAVE, ALLOCATABLE  :: lwmask    ( :,: )
      Integer, Save              :: n_lufrac
      Character( 80 ), Save      :: LAND_SCHEME
      

C-------------------------------------------------------------------------------
C Soil Characteristics by Type
C
C   #  SOIL TYPE  WSAT  WFC  WWLT    B   CGSAT   JP   AS   C2R  C1SAT  WRES
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
      DATA WSAT  /  0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477,    
     &              0.476, 0.426, 0.482, 0.482/
!-- WWLT is wilting point (M^3/M^3) (JN90)
      DATA WWLT  /  0.068, 0.075, 0.114, 0.179, 0.155, 0.175, 0.218,    
     &              0.250, 0.219, 0.283, 0.286/
!-- B is slop of the retention curve (NP89)
      DATA BSLP  /  4.05,  4.38,  4.90,  5.30,  5.39,  7.12,  7.75,   
     &              8.52, 10.40, 10.40, 11.40/
!-- WRES is residual soil moisture (Rawls et al 1982)
      DATA WRES  /  0.020, 0.035, 0.041, 0.015, 0.027, 0.068, 0.040,    
     &              0.075, 0.109, 0.056, 0.090/
!-- WFC is soil field capacity (Rawls et al 1982)     
      DATA WFC   /  0.135, 0.150, 0.195, 0.255, 0.240, 0.255, 0.322,    
     &              0.325, 0.310, 0.370, 0.367/  
! -- RHOB  is the soil bulk dnesity following Lyons et al 1997   
      DATA RHOB  /  1.59e6, 1.55e6, 1.53e6, 1.53e6, 1.55e6, 1.62e6, 1.67e6,    
     &              1.66e6, 1.83e6, 1.78e6, 1.83e6/     

!*****************************************************************************
! MODIS-ONLY (1XX)/MODIS (2XX) LU characterization
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!  1      175.   100.   90.  80.  5.0  3.0    Evergreen Needleleaf Forest
!  2      120.   90.    95.  85.  5.0  4.0    Evergreen Broadleaf Forest
!  3      175.   100.   95.  50.  5.0  1.0    Deciduous Needleleaf Forest
!  4      200.   100.   95.  50.  5.0  1.0    Deciduous Broadleaf Forest'
!  5      200.   100.   95.  60.  5.0  2.0    Mixed Forest
!  6      200.   30.    90.  50.  3.0  1.0    Closed Shrublands
!  7      200.   20.    75.  50.  2.0  1.0    Open Shrublands
!  8      150.   25.    80.  60.  2.5  1.0    Woody Savanna
!  9      120.   15.    70.  50   2.0  1.0    Savanna
!-------------------------------------------------------------------------------
! 10      100.    7.    85.  60.  2.5  1.0    Grasslands
! 11      160    20.    75.  45.  3.0  1.0    Perminent Wetlands
! 12       70.   10.    95.  10.  3.0  0.5    Croplands
! 13      150.   80.    40.  20.  3.0  1.0    Urban andBuilt-up'
! 14      100.   30.    95.  40.  3.0  1.0    Cropland/Natural Vegetation Mosaic
! 15     9999.   1.2     5.  02.  0.1  0.1    Snow and Ice
! 16      100.    5.    20.   5.  1.0  0.5    Barren or Sparsely Vegetated
! 17     9999.   0.1    00.  00.  0.0  0.0    IGBP water
! 18     9999.   0.1    00.  00.  0.0  0.0    Wooded Tundra
! 19     9999.   0.1    00.  00.  0.0  0.0    Mixed Tundra
! 20     9999.   0.1    00.  00.  0.0  0.0    Barren Tundra
!------------------------------------------------------------------------------------
      
      Integer, Parameter :: n_modis = 20
      Integer, Parameter :: n_nlcd  = 50
      Integer, Parameter :: n_usgs  = 24
      REAL, DIMENSION(n_modis), TARGET  :: RSMIN_MODIS, Z00_MODIS,     
     &                                 VEG0_MODIS, VEGMN0_MODIS,   
     &                                 LAI0_MODIS, LAIMN0_MODIS,   
     &                                 SNUP0_MODIS, ALBF_MODIS, 
     &                                 LUF_FAC_MODIS, HGLU_FAC_MODIS

      DATA RSMIN_MODIS                                    
     &       /  175.0,  120.0,  175.0,  200.0,  200.0,    
     &          200.0,  200.0,  150.0,  120.0,  100.0,    
     &          160.0,   70.0,  150.0,  100.0, 9999.0,    
     &          100.0, 9999.0,  175.0,  120.0,  100.0  /

      DATA Z00_MODIS                                      
     &       /   50.0,   50.0,   50.0,   50.0,   50.0,    
     &            5.0,    6.0,    5.0,   15.0,   12.0,    
     &           12.0,   30.0,   15.0,   50.0,   14.0,    
     &            0.1,   0.01,   30.0,   15.0,   10.0  /

      DATA VEG0_MODIS                                     
     &       /   90.0,   95.0,   95.0,   95.0,   95.0,    
     &           90.0,   75.0,   80.0,   70.0,   85.0,    
     &           75.0,   95.0,   40.0,   95.0,    5.0,    
     &           20.0,    0.0,   70.0,   40.0,   20.0  /

      DATA VEGMN0_MODIS                                   
     &       /   80.0,   85.0,   50.0,   50.0,   60.0,    
     &           50.0,   50.0,   60.0,   50.0,   60.0,    
     &           45.0,   10.0,   20.0,   40.0,    2.0,    
     &            5.0,    0.0,   50.0,   20.0,    5.0  /

      DATA LAI0_MODIS                                     
     &       /    5.0,    5.0,    5.0,    5.0,    5.0,     
     &            3.0,    2.0,    2.5,    2.0,    2.5,    
     &            3.0,    3.0,    3.0,    3.0,    0.1,    
     &            1.0,    0.0,    3.4,    2.4,    1.4  /

      DATA LAIMN0_MODIS                                   
     &       /    3.0,    4.0,    1.0,    1.0,    2.0,     
     &            1.0,    1.0,    1.0,    1.0,    1.0,    
     &            1.0,    0.5,    1.0,    1.0,    0.1,     
     &            0.5,    0.0,    2.0,    1.0,    0.1  /

      DATA SNUP0_MODIS                                    
     &       /   0.08,   0.08,   0.08,   0.08,   0.08,     
     &           0.03,   0.035,  0.03,   0.04,   0.04,    
     &           0.015,  0.04,   0.04,   0.04,   0.02,      
     &           0.02,   0.01,   0.80,   0.40,   0.015  /

      DATA ALBF_MODIS                                     
     &       /   12.0,   12.0,   14.0,   16.0,   13.0,     
     &           22.0,   20.0,   22.0,   20.0,   19.0,    
     &           14.0,   17.0,   15.0,   18.0,   55.0,     
     &           25.0,    8.0,   15.0,   15.0,   25.0  /
     
      DATA LUF_FAC_MODIS
     &       /  100.0,  100.0,  100.0,  100.0,  100.0, 
     &          100.0,  100.0,  100.0,  100.0,  100.0, 
     &          100.0,  160.0,   50.0,  130.0,    0.0, 
     &           10.0,    0.0,   10.0,   10.0,    0.0  /  
     
      DATA HGLU_FAC_MODIS
     &       /   40.0,  130.0,   81.0,   81.0,  139.0, 
     &          125.0,  125.0,  125.0,  125.0,   20.0, 
     &          125.0,   80.0,  190.0,   50.0,    0.0, 
     &          125.0,    0.0,   62.0,   62.0,   18.0  /  


!*****************************************************************************
! NLCD (1XX)/MODIS (2XX) LU characterization
!-------------------------------------------------------------------------------
!    Index  Rstmin  Zo    Mxfr MnFr  MxLA MnLA
! 1  111    9999.   0.1    00.  00.  0.0  0.0    Open water
! 2  112    9999.   1.2     5.  02.  0.1  0.1    Perennial Ice/snow
! 3  121     120.   30.    90.  80.  3.0  1.0    Developed, Open space
! 4  122     120.   40.    70   60.  3.0  1.0    Developed, Low Intensity
! 5  123     140.   60.    40.  30.  3.0  1.0    Developed, Medium Intensity
! 6  124     160.   100.   15.   5.  3.0  1.0    Developed, High Intensity
! 7  131     100.    5.    20.   5.  1.0  0.5    Barren land
! 8  132     100.    5.    15.   5.  0.5  0.2    Unconsolidated Shore
! 9  141     200.   100.   95.  50.  5.0  1.0    Deciduous Forest
! 10 142     175.   100.   90.  80.  4.0  2.5    Evergreen Forest
!-------------------------------------------------------------------------------
! 11 143     200.   100.   95.  60.  5.0  2.0    Mixed Forest
! 12 151     200.   10.    50.  20.  2.0  1.0    Dwarf Scrub
! 13 152     200.   25.    75.  50.  2.5  1.0    Shrub/Scrub
! 14 171     100.    7.    85.  60.  2.5  1.0    Grassland/Herbaceous
! 15 172     100.    7.    80.  20.  2.0  1.0    Sedge/Herbaceous
! 16 173     100.    5.    80.  20.  1.0  1.0    Lichens
! 17 174     100.    5.    80.  20.  1.0  1.0    Moss
! 18 175     100.    5.    50.  20.  1.0  1.0    Tundra
! 19 181      80.    7.    95.  80.  3.0  1.0    Pasture/Hay'
! 20 182      70.   10.    95.  10.  3.0  0.5    Cultivated Crops
!-------------------------------------------------------------------------------
! 21 190     200.   55.    90.  80.  5.0  2.0    Woody Wetland
! 22 191     200.   80.    90.  80.  5.0  2.0    Palustrine Forested Wetland
! 23 192     164.   30.    90.  80.  3.0  1.0    Palustrine Scrub/Shrub Wetland
! 24 193     200.   60.    90.  80.  5.0  2.0    Estuarine Forested Wetland
! 25 194     164.   30.    90.  80.  3.0  1.0    Estuarine Scrub/Shrub Wetland
! 26 195     120.   11.    60.  40.  2.0  1.0    Emergent Herbaceous Wetland
! 27 196     120.   11.    80.  40.  2.0  1.0    Palustrine Emergent Wetland
! 28 197     120.   11.    80.  40.  2.0  1.0    Estuarine Emergent Wetland
! 29 198     100.    5.    60.  20.  1.0  0.5    Palustrine Aquatic Bed
! 30 199     100.    5.    60.  20.  1.0  0.5    Estuarine Aquatic Bed
!-------------------------------------------------------------------------------
! 31 200    9999.   0.1    00.  00.  0.0  0.0    Open water (mapped to 1)
! 32 201     175.   100.   90.  80.  5.0  3.0    Evergreen Needleleaf Forest
! 33 202     120.   90.    95.  85.  5.0  4.0    Evergreen Broadleaf Forest
! 34 203     175.   100.   95.  50.  5.0  1.0    Deciduous Needleleaf Forest
! 35 204     200.   100.   95.  50.  5.0  1.0    Deciduous Broadleaf Forest
! 36 205     200.   100.   95.  60.  5.0  2.0    Mixed Forest
! 37 206     200.   30.    90.  50.  3.0  1.0    Closed Shrublands
! 38 207     200.   20.    75.  50.  2.0  1.0    Open Shrublands
! 39 208     150.   25.    80.  60.  2.5  1.0    Woody Savanna
! 40 209     120.   15.    70.  50   2.0  1.0    Savanna
!-------------------------------------------------------------------------------
! 41 210     100.    7.    85.  60.  2.5  1.0    Grasslands
! 42 211     160    20.    75.  45.  3.0  1.0    Perminent Wetlands
! 43 212      70.   10.    95.  10.  3.0  0.5    Croplands
! 44 213     150.   80.    40.  20.  3.0  1.0    Urban andBuilt-up'
! 45 214     100.   30.    95.  40.  3.0  1.0    Cropland/Natural Vegetation Mosaic
! 46 215    9999.   1.2     5.  02.  0.1  0.1    Snow and Ice
! 47 216     100.    5.    20.   5.  1.0  0.5    Barren or Sparsely Vegetated
! 48 217    9999.   0.1    00.  00.  0.0  0.0    IGBP water
! 49 2254   9999.   0.1    00.  00.  0.0  0.0    unclassified
! 50 2255   9999.   0.1    00.  00.  0.0  0.0    fill value (normally ocean water)
!------------------------------------------------------------------------------------

      REAL, DIMENSION(n_nlcd), TARGET  :: RSMIN_NLCD, Z00_NLCD,      
     &                                    VEG0_NLCD, VEGMN0_NLCD,    
     &                                    LAI0_NLCD, LAIMN0_NLCD,    
     &                                    SNUP0_NLCD, ALBF_NLCD, 
     &                                    LUF_FAC_NLCD, HGLU_FAC_NLCD

      DATA RSMIN_NLCD                                        
     &       / 9999.0,  9999.0,   120.0,   120.0,   140.0,   
     &          160.0,   100.0,   100.0,   200.0,   175.0,   
     &          200.0,   200.0,   200.0,   100.0,   100.0,   
     &          100.0,   100.0,   100.0,    80.0,    70.0,   
     &          200.0,   200.0,   164.0,   200.0,   164.0,   
     &          120.0,   120.0,   120.0,   100.0,   100.0,   
     &         9999.0,   175.0,   120.0,   175.0,   200.0,   
     &          200.0,   200.0,   200.0,   150.0,   120.0,   
     &          100.0,   160.0,    70.0,   150.0,   100.0,   
     &         9999.0,   100.0,  9999.0,  9999.0,  9999.0  /

      DATA Z00_NLCD                                          
     &       /   0.1,     1.2,    30.0,    40.0,    60.0,    
     &         100.0,     5.0,     5.0,   100.0,   100.0,    
     &         100.0,    10.0,    25.0,     7.0,     7.0,    
     &           5.0,     5.0,     5.0,     7.0,    10.0,    
     &          55.0,    80.0,    30.0,    60.0,    30.0,    
     &          11.0,    11.0,    11.0,     5.0,     5.0,    
     &           0.1,   100.0,    90.0,   100.0,   100.0,    
     &         100.0,    30.0,    20.0,    25.0,    15.0,    
     &           7.0,    20.0,    10.0,    80.0,    30.0,    
     &           1.2,     5.0,     0.1,     0.1,     0.1   /
                   
      DATA VEG0_NLCD                                         
     &       /   0.0,     5.0,    90.0,    70.0,    40.0,    
     &          15.0,    20.0,    15.0,    95.0,    90.0,    
     &          95.0,    50.0,    75.0,    85.0,    80.0,    
     &          80.0,    80.0,    50.0,    95.0,    95.0,    
     &          90.0,    90.0,    90.0,    90.0,    90.0,    
     &          60.0,    80.0,    80.0,    60.0,    60.0,    
     &           0.0,    90.0,    95.0,    95.0,    95.0,    
     &          95.0,    90.0,    75.0,    80.0,    70.0,    
     &          85.0,    75.0,    95.0,    40.0,    95.0,    
     &           5.0,    20.0,     0.0,     0.0,     0.0   /
                   
      DATA VEGMN0_NLCD                                       
     &       /  00.0,     2.0,    80.0,    60.0,    30.0,    
     &          05.0,    05.0,     5.0,    50.0,    80.0,    
     &          60.0,    20.0,    50.0,    60.0,    20.0,    
     &          20.0,    20.0,    20.0,    80.0,    10.0,    
     &          80.0,    80.0,    80.0,    80.0,    80.0,    
     &          40.0,    40.0,    40.0,    20.0,    20.0,    
     &           0.0,    80.0,    85.0,    50.0,    50.0,    
     &          60.0,    50.0,    50.0,    60.0,    50.0,    
     &          60.0,    45.0,    10.0,    20.0,    40.0,    
     &           2.0,     5.0,     0.0,     0.0,     0.0   /
                     
      DATA LAI0_NLCD                                         
     &       /   0.0,     0.1,     3.0,     3.0,     3.0,    
     &           3.0,     1.0,     0.5,     5.0,     4.0,    
     &           5.0,     2.0,     2.5,     2.5,     2.0,    
     &           1.0,     1.0,     1.0,     3.0,     3.0,    
     &           5.0,     5.0,     3.0,     5.0,     3.0,    
     &           2.0,     2.0,     2.0,     1.0,     1.0,    
     &           0.0,     5.0,     5.0,     5.0,     5.0,    
     &           5.0,     3.0,     2.0,     2.5,     2.0,    
     &           2.5,     3.0,     3.0,     3.0,     3.0,    
     &           0.1,     1.0,     0.0,     0.0,     0.0   /
                    
      DATA LAIMN0_NLCD                                       
     &       /   0.0,     0.1,     1.0,     1.0,     1.0,    
     &           1.0,     0.5,     0.2,     1.0,     2.5,    
     &           2.0,     1.0,     1.0,     1.0,     1.0,    
     &           1.0,     1.0,     1.0,     1.0,     0.5,    
     &           2.0,     2.0,     1.0,     2.0,     1.0,    
     &           1.0,     1.0,     1.0,     0.5,     0.5,    
     &           0.0,     3.0,     4.0,     1.0,     1.0,    
     &           2.0,     1.0,     1.0,     1.0,     1.0,    
     &           1.0,     1.0,     0.5,     1.0,     1.0,    
     &           0.1,     0.5,     0.0,     0.0,     0.0   /

      DATA SNUP0_NLCD                                        
     &       /   0.01,    0.02,    0.04,    0.04,    0.04,   
     &           0.04,    0.02,    0.02,    0.08,    0.08,   
     &           0.08,    0.04,    0.04,    0.04,    0.01,   
     &           0.01,    0.01,    0.02,    0.04,    0.04,   
     &           0.08,    0.08,    0.04,    0.08,    0.04,   
     &           0.04,    0.06,    0.06,    0.02,    0.02,   
     &           0.08,    0.08,    0.08,    0.08,    0.08,   
     &           0.08,    0.03,    0.035,   0.03,    0.04,   
     &           0.04,   0.015,    0.04,    0.04,    0.04,   
     &           0.02,    0.02,    0.01,    0.01,    0.01  /

      DATA ALBF_NLCD                                         
     &       /   8.0,    60.0,    16.0,    17.0,    16.0,    
     &          17.0,    20.0,    35.0,    15.0,    10.0,    
     &          13.0,    20.0,    20.0,    25.0,    23.0,    
     &          20.0,    20.0,    15.0,    23.0,    20.0,    
     &          15.0,    15.0,    15.0,    15.0,    15.0,    
     &          18.0,    18.0,    18.0,    10.0,    10.0,    
     &           8.0,    12.0,    12.0,    14.0,    16.0,    
     &          13.0,    22.0,    20.0,    22.0,    20.0,    
     &          19.0,    14.0,    17.0,    15.0,    18.0,    
     &          55.0,    25.0,     8.0,     8.0,     8.0   /
     
      DATA LUF_FAC_NLCD
     &       /    0.0,    0.0,   160.0,   160.0,   100.0, 
     &           50.0,   10.0,     0.0,   100.0,   100.0, 
     &          100.0,  100.0,   100.0,   100.0,   100.0, 
     &            0.0,    0.0,    10.0,   160.0,   160.0,
     &          100.0,  100.0,   100.0,   100.0,   100.0, 
     &          100.0,  100.0,   100.0,    50.0,    50.0, 
     &            0.0,  100.0,   100.0,   100.0,   100.0, 
     &          100.0,  100.0,   100.0,   100.0,   100.0, 
     &          100.0,  100.0,   160.0,    50.0,   130.0, 
     &            0.0,   10.0,     0.0,     0.0,     0.0 /
     
      DATA HGLU_FAC_NLCD
     &       /    0.0,    0.0,   130.0,   150.0,   170.0, 
     &          190.0,  125.0,     0.0,    81.0,    81.0, 
     &          139.0,   73.0,   125.0,    20.0,   125.0, 
     &           18.0,   18.0,    62.0,    50.0,    80.0,
     &          125.0,  125.0,   125.0,   125.0,   125.0, 
     &          125.0,  125.0,   125.0,   125.0,   125.0, 
     &            0.0,   40.0,   130.0,    81.0,    81.0, 
     &          139.0,  125.0,   125.0,   125.0,   125.0, 
     &           20.0,  125.0,    81.0,   190.0,    50.0, 
     &            0.0,  125.0,     0.0,     0.0,     0.0 /


!*****************************************************************************
!  USGS LU characterization
!---------------------------
!     Name  Rstmin  Zo  Mxfr MnFr  MxLA MnLA
! 1   Urban  150.   50.  40.  20.  2.0  0.5  Urban or Built-up Land
! 2   DrCrp   70.   10.  95.  15.  3.0  0.5  Dryland Cropland and Pasture
! 3   IrCrp   60.   10.  95.  10.  3.0  0.5  Irrigated Cropland and Pasture
! 4   MixCp   70.   10.  95.  15.  3.0  0.5  Mixed Dry/Irr Crop and Past
! 5   CrGrM   80.   10.  95.  35.  2.5  1.0  Grassland/Cropland Mosaic
! 6   CrWdM  180.   40.  95.  40.  4.0  1.5  Woodland/Cropland Mosaic
! 7   GrsLd  100.    7.  95.  70.  2.5  1.0  Grassland
! 8   ShrLd  200.   20.  70.  50.  3.0  1.0  Shrubland
! 9   ShrGr  150.   20.  85.  60.  3.0  1.0  Mixed Shrubland/Grassland
! 10  Savan  120.   20.  80.  60.  2.0  1.0  Savanna
! 11  DBFst  200.   50.  95.  50.  5.0  1.0  Broadleaf Deciduous Forest
! 12  DNFst  175.   50.  95.  50.  5.0  1.0  Deciduous Coniferous Forest
! 13  EBFst  120.   40.  95.  85.  5.0  4.0  Evergreen Broadleaf Forest (Palm?)
! 14  ENFst  175.   50.  90.  80.  4.0  3.0  Evergreen Coniferous Forest
! 15  MxFst  200.   50.  95.  60.  5.0  2.0  Mixed forest
! 16  Water 9999.   0.1  00.  00.  0.0  0.0  Water
! 17  HWtld  164.   15.  60.  40.  2.0  1.0  Herbaceous Wetland (none in east)
! 18  WWtld  200.   45.  90.  80.  5.0  3.0  Forested Wetlands (e.g. Everglades)
! 19  BarSp  100.    5.  10.  05.  0.5  0.2  Barren or Sparsely Vegetated
! 20  HrTun  150.   10.  20.  10.  1.0  0.5  Herbaceous Tundra
! 21  WdTun  200.   10.  30.  10.  1.0  0.5  Shrub and Brush Tundra
! 22  MxTun  150.    5.  20.  05.  1.0  0.5  Mixed Tundra
! 23  BGTun  100.    5.   5.  02.  0.1  0.1  Bare Ground Tundra
! 24  SnwIc  300.    5.   5.  02.  0.1  0.1  Perennial Snowfields or Glaciers
!-----------------------------------------------------------------------------

      REAL, DIMENSION(n_usgs), TARGET  :: RSMIN_USGS, Z00_USGS,   
     &                                    VEG0_USGS, VEGMN0_USGS, 
     &                                    LAI0_USGS, LAIMN0_USGS, 
     &                                    SNUP0_USGS, ALBF_USGS, 
     &                                    LUF_FAC_USGS, HGLU_FAC_USGS

      DATA RSMIN_USGS                                         
     &       /  150.0,    70.0,    60.0,    70.0,    80.0,   
     &          180.0,   100.0,   200.0,   150.0,   120.0,   
     &          200.0,   175.0,   120.0,   175.0,   200.0,   
     &         9999.0,   164.0,   200.0,   100.0,   150.0,   
     &          200.0,   150.0,   100.0,   300.0           /
      DATA Z00_USGS                                           
     &       /   50.0,    10.0,    10.0,    10.0,    10.0,   
     &           40.0,     7.0,    20.0,    20.0,    20.0,   
     &           50.0,    50.0,    40.0,    50.0,    50.0,   
     &            0.1,    15.0,    45.0,     5.0,    10.0,   
     &           10.0,     5.0,     5.0,     5.0           /
      DATA VEG0_USGS                                          
     &       /   40.0,    95.0,    95.0,    95.0,    95.0,   
     &           95.0,    95.0,    70.0,    85.0,    80.0,   
     &           95.0,    95.0,    95.0,    90.0,    95.0,   
     &            0.00,   60.0,    90.0,    10.0,    20.0,   
     &           30.0,    20.0,     5.0,     5.0           /

      DATA VEGMN0_USGS                                        
     &       /   20.0,    15.0,    10.0,    15.0,    35.0,   
     &           40.0,    70.0,    50.0,    60.0,    60.0,   
     &           50.0,    50.0,    85.0,    80.0,    60.0,   
     &            0.0,    40.0,    80.0,     5.0,    10.0,   
     &           10.0,     5.0,     2.0,     2.0           /

      DATA LAI0_USGS                                          
     &       /    2.0,     3.0,     3.0,     3.0,     2.5,   
     &            4.0,     2.5,     3.0,     3.0,     2.0,   
     &            5.0,     5.0,     5.0,     4.0,     5.0,   
     &            0.0,     2.0,     5.0,     0.50,    1.0,   
     &            1.0,     1.0,     0.1,     0.1           /

      DATA LAIMN0_USGS                                        
     &       /    0.50,    0.50,    0.50,    0.50,    1.0,   
     &            1.5,     1.0,     1.0,     1.0,     1.0,   
     &            1.0,     1.0,     4.0,     3.0,     2.0,   
     &            0.0,     1.0,     3.0,     0.20,    0.50,  
     &            0.50,    0.50,    0.10,    0.10          /

      DATA SNUP0_USGS                                         
     &       /    0.04,    0.04,    0.04,    0.04,    0.04,  
     &            0.04,    0.04,    0.03,    0.035,   0.04,  
     &            0.08,    0.08,    0.08,    0.08,    0.08,  
     &            0.01,    0.01,    0.01,    0.02,    0.02,  
     &            0.025,   0.025,   0.025,   0.02          /

      DATA ALBF_USGS                                          
     &       /    15.0,   17.0,    18.0,    18.0,    18.0,   
     &            16.0,   19.0,    22.0,    20.0,    20.0,   
     &            16.0,   14.0,    12.0,    12.0,    13.0,   
     &             8.0,   14.0,    14.0,    25.0,    15.0,   
     &            15.0,   15.0,    25.0,    55.0           /
     
      DATA LUF_FAC_USGS
     &       /   50.0,  160.0,   160.0,   160.0,   130.0, 
     &          130.0,  100.0,   100.0,   100.0,   100.0, 
     &          100.0,  100.0,   100.0,   100.0,   100.0, 
     &            0.0,  100.0,   100.0,    10.0,     0.0,
     &            0.0,    0.0,     0.0,     0.0            /
     
      DATA LUF_FAC_USGS
     &       /  190.0,   80.0,    32.0,    56.0,    50.0, 
     &           26.0,   20.0,   125.0,    73.0,    51.0, 
     &           81.0,   81.0,   130.0,    40.0,   139.0, 
     &            0.0,  125.0,   125.0,    26.0,    62.0,
     &           62.0,   62.0,    18.0,     0.0            /
      Contains 
         Subroutine INIT_LSM( JDate, JTime )
         
         Use HGRD_DEFN
         Use UTILIO_DEFN
#ifdef twoway
         USE twoway_data_module, only : num_land_cat
#endif
 
         Implicit None
         
         INCLUDE SUBST_FILES_ID  ! file name parameters         
         
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime  
         CHARACTER( 240 )       :: XMSG = ' '
         CHARACTER(  16 ), SAVE :: PNAME = 'Init_LSM'
         INTEGER      :: GXOFF, GYOFF            ! global origin offset from file
         INTEGER      :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
         Integer l
         
         LAND_SCHEME = 'UNKNOWN'
         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
       
         IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
            XMSG = 'COULD NOT GET GRID_CRO_2D FILE DESCRIPTION '
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF        
         
#ifdef twoway
         IF ( NUM_LAND_CAT == 24) THEN
            LAND_SCHEME = 'USGS24'
         ELSE IF ( NUM_LAND_CAT == 20) THEN
            LAND_SCHEME = 'MODIS'
         ELSE IF ( NUM_LAND_CAT == 50) THEN
            LAND_SCHEME = 'NLCD-MODIS'
         END IF
#else
         DO l = 1, NVARS3D ! determine land character
            IF ( VNAME3D( l ) .EQ. 'DLUSE' ) THEN
               IF ( INDEX( VDESC3D( l ), 'USGS24' ) .NE. 0 ) LAND_SCHEME = 'USGS24'
               IF ( INDEX( VDESC3D( l ), 'MODIS' ) .NE. 0 )  LAND_SCHEME = 'MODIS'
C...overwrite if NLCD-MODIS scheme
               IF ( INDEX( VDESC3D( l ), 'NLCD-MODIS' ) .NE. 0 ) LAND_SCHEME = 'NLCD-MODIS'
            END IF
         END DO         
#endif
!       Write(Logdev,*) 'Land use scheme is ', TRIM(LAND_SCHEME) 
       
         SELECT CASE( LAND_SCHEME )
            CASE( 'USGS24' )
               n_lufrac = n_usgs              
            CASE( 'MODIS' )
               n_lufrac = n_modis        
            CASE( 'NLCD-MODIS' )
               n_lufrac = n_nlcd            
            CASE DEFAULT
               xmsg = 'Land use scheme not supported'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END SELECT 
         
         End Subroutine Init_LSM
      
      End Module LSM_Mod
