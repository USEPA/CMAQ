
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
C------------------------------------------------------------------------------
       
      Implicit None
      
      REAL,   SAVE               :: wwlt_px (11)
      REAL,   SAVE               :: wsat_px (11)
      REAL,   SAVE               :: bslp_px (11)
      REAL,   SAVE               :: wres_px (11)
      REAL,   SAVE               :: wfc_px  (11)
      REAL,   SAVE               :: rhob_px (11)
      REAL,   SAVE               :: wwlt_clm (19)
      REAL,   SAVE               :: wsat_clm (19)
      REAL,   SAVE               :: bslp_clm (19)
      REAL,   SAVE               :: wres_clm (19)
      REAL,   SAVE               :: wfc_clm  (19)
      REAL,   SAVE               :: rhob_clm (19)
      REAL,   SAVE               :: wwlt_noah (19)
      REAL,   SAVE               :: wsat_noah (19)
      REAL,   SAVE               :: bslp_noah (19)
      REAL,   SAVE               :: wres_noah (19)
      REAL,   SAVE               :: wfc_noah  (19)
      REAL,   SAVE               :: rhob_noah (19)
      REAL,   SAVE, ALLOCATABLE  :: RSMIN     ( : )
      REAL,   SAVE, ALLOCATABLE  :: Z00       ( : )
      REAL,   SAVE, ALLOCATABLE  :: VEG0      ( : )
      REAL,   SAVE, ALLOCATABLE  :: VEGMN0    ( : )
      REAL,   SAVE, ALLOCATABLE  :: LAI0      ( : )
      REAL,   SAVE, ALLOCATABLE  :: LAIMN0    ( : )
      REAL,   SAVE, ALLOCATABLE  :: SNUP0     ( : )
      REAL,   SAVE, ALLOCATABLE  :: ALBF      ( : )
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
         REAL            :: RSMIN
         REAL            :: Z00
         REAL            :: VEG0
         REAL            :: VEGMN0
         REAL            :: LAI0
         REAL            :: LAIMN0
         REAL            :: SNUP0
         REAL            :: ALBF
         REAL            :: LUF_FAC ! NH3 bidi stomatal gamma
         REAL            :: HGLU_FAC ! Hg bidi soil conc
      END TYPE lu_type


C-------------------------------------------------------------------------------
C Soil Characteristics by Type from WRF 3.8.1 PX
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
      DATA WSAT_PX  /  0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477,
     &                 0.476, 0.426, 0.482, 0.482 /
!-- WFC is soil field capacity (Rawls et al 1982)     
      DATA WFC_PX   /  0.135, 0.150, 0.195, 0.255, 0.240, 0.255, 0.322,
     &                 0.325, 0.310, 0.370, 0.367 /
!-- WWLT is wilting point (M^3/M^3) (JN90)
      DATA WWLT_PX  /  0.068, 0.075, 0.114, 0.179, 0.155, 0.175, 0.218,
     &                 0.250, 0.219, 0.283, 0.286 /
!-- B is slop of the retention curve (NP89)
      DATA BSLP_PX  /  4.05,  4.38,  4.90,  5.30,  5.39,  7.12,  7.75,
     &                 8.52, 10.40, 10.40, 11.40 /
!-- WRES is residual soil moisture (Rawls et al 1982)
      DATA WRES_PX  /  0.020, 0.035, 0.041, 0.015, 0.027, 0.068, 0.040,
     &                 0.075, 0.109, 0.056, 0.090 /
! -- RHOB is the soil bulk dnesity following Lyons et al 1997   
      DATA RHOB_PX  /  1.59e6, 1.55e6, 1.53e6, 1.53e6, 1.55e6, 1.62e6, 1.67e6,
     &                 1.66e6, 1.83e6, 1.78e6, 1.83e6 /

C-------------------------------------------------------------------------------
C Soil hydrolic properties as calculcated from soil sand and clay fractions 
C in WRF-CLM in WRF 3.7.1-3.8.1. 
C WRES is calculated as WSAT*(psi_air_dry/psi_sat)^(-1.0/BSLP) following CLM soil hydrology 
C relationships. Note that this is a common paramterization, e.g. Campbel and Norman (1998)
C    where psi_air_dry = -15000 kPa
C
C   #  SOIL TYPE  WSAT  WFC  WWLT  BSLP  CGSAT   JP   AS   C2R  C1SAT  WRES
C   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____  ____
C   1  SAND       .373 .135  .029  3.30  3.222    4  .387  3.9  .082   .016
C   2  LOAMY SAND .388 .156  .042  3.65  3.057    4  .404  3.7  .098   .025
C   3  SANDY LOAM .406 .192  .071  4.47  3.560    4  .219  1.8  .132   .049
C   4  SILT LOAM  .464 .269  .138  5.40  4.418    6  .105  0.8  .153   .108
C   5  SILT       .483 .250  .096  3.87                                .075
C   6  LOAM       .435 .248  .127  5.80  4.111    6  .148  0.8  .191   .096
C   7  SND CLY LM .413 .249  .143  7.16  3.670    6  .135  0.8  .213   .109
C   8  SLT CLY LM .476 .331  .225  8.25  3.593    8  .127  0.4  .385   .185
C   9  CLAY LOAM  .449 .299  .195  8.19  3.995   10  .084  0.6  .227   .157
C  10  SANDY CLAY .425 .288  .195  9.38  3.058    8  .139  0.3  .421   .156
C  11  SILTY CLAY .481 .360  .270 10.46  3.729   10  .075  0.3  .375   .227
C  12  CLAY       .461 .351  .270 12.14  3.600   12  .083  0.3  .342   .227
C  13  ORGANIC    .439 .241  .115  5.29                                .086
C  14  WATER      .489 .229  .066  3.10  1.0      0  .0    0.0  .0     .052
C  15  BEDROCK    .363 .114  .017  2.80                                .008
C  16  OTHER      .421 .200  .073  4.27  3.222    4  .387  3.9  .082   .051
C  17  PLAYA      .468 .353  .296 11.53                                .227 
C  18  LAVA       .363 .114  .017  2.80                                .001
C  19  WHITE SAND .373 .135  .029  3.30                                .016
C-------------------------------------------------------------------------------
!-- WSAT is saturated soil moisture (M^3/M^3)
      DATA WSAT_CLM  /  0.373, 0.388, 0.406, 0.464, 0.483, 0.435, 0.413, 0.476,
     &                  0.449, 0.425, 0.481, 0.461, 0.439, 0.489, 0.363, 0.421, 
     &                  0.468, 0.363, 0.373 /
!-- WFC is soil field capacity      
      DATA WFC_CLM   /  0.135, 0.156, 0.192, 0.269, 0.250, 0.248, 0.249, 0.331,
     &                  0.299, 0.288, 0.360, 0.351, 0.241, 0.229, 0.114, 0.200,
     &                  0.353, 0.114, 0.135 /
!-- WWLT is wilting point (M^3/M^3)
      DATA WWLT_CLM  /  0.029, 0.042, 0.071, 0.138, 0.096, 0.127, 0.143, 0.225,
     &                  0.195, 0.195, 0.270, 0.270, 0.115, 0.066, 0.017, 0.073,
     &                  0.269, 0.017, 0.029 /
!-- B is slop of the retention curve
      DATA BSLP_CLM  /  3.30,  3.65,  4.47,  5.40,  3.87, 5.80,  7.16,  8.25,
     &                  8.19,  9.38, 10.46, 12.14,  5.29, 3.10,  2.80,  4.27,
     &                 11.53,  2.80,  3.30 /
!-- WRES is residual soil moisture
      DATA WRES_CLM  /  0.016, 0.025, 0.049, 0.108, 0.075, 0.096, 0.109, 0.185,
     &                  0.157, 0.156, 0.227, 0.227, 0.086, 0.052, 0.008, 0.051,
     &                  0.227, 0.008, 0.016 /
! -- RHOB is the soil bulk dnesity
      DATA RHOB_CLM  /  1.69e6, 1.65e6, 1.60e6, 1.45e6, 1.40e6, 1.53e6, 1.58e6, 1.41e6,
     &                  1.49e6, 1.55e6, 1.40e6, 1.45e6, 1.51e6, 1.38e6, 1.72e6, 1.56e6,
     &                  1.44e6, 1.72e6, 1.69e6 /

C-------------------------------------------------------------------------------
C Soil hydrolic properties as calculcated from soil sand and clay fractions 
C in WRF-Noah LSM in WRF 3.8.1. 
C WRES is calculated as WSAT*(psi_air_dry/psi_sat)^(-1.0/BSLP) following CLM soil hydrology 
C relationships. Note that this is a common paramterization, e.g. Campbel and Norman (1998)
C    where psi_air_dry = -3000 kPa
C
C   #  SOIL TYPE  WSAT  WFC  WWLT  BSLP  CGSAT   JP   AS   C2R  C1SAT  WRES
C   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____  ____
C   1  SAND       .339 .129  .021  2.79  3.222    4  .387  3.9  .082   .009
C   2  LOAMY SAND .421 .192  .059  4.26  3.057    4  .404  3.7  .098   .034
C   3  SANDY LOAM .434 .286  .099  4.74  3.560    4  .219  1.8  .132   .061
C   4  SILT LOAM  .476 .360  .176  5.33  4.418    6  .105  0.8  .153   .114
C   5  SILT       .476 .360  .176  5.33                                .114
C   6  LOAM       .439 .286  .138  5.25  4.111    6  .148  0.8  .191   .089
C   7  SND CLY LM .404 .300  .143  6.77  3.670    6  .135  0.8  .213   .102
C   8  SLT CLY LM .464 .382  .247  8.72  3.593    8  .127  0.4  .385   .189
C   9  CLAY LOAM  .465 .340  .213  8.17  3.995   10  .084  0.6  .227   .161
C  10  SANDY CLAY .406 .292  .205 10.73  3.058    8  .139  0.3  .421   .165
C  11  SILTY CLAY .468 .374  .259 10.39  3.729   10  .075  0.3  .375   .207
C  12  CLAY       .468 .394  .283 11.55  3.600   12  .083  0.3  .342   .232
C  13  ORGANIC    .439 .286  .138  5.25                                .089
C  14  WATER      .489 .229  .066  3.10  1.0      0  .0    0.0  .0     .052
C  15  BEDROCK    .200 .050  .013  2.79                                .006
C  16  OTHER      .421 .145  .059  4.26  3.222    4  .387  3.9  .082   .034
C  17  PLAYA      .468 .394  .283 11.55                                .232 
C  18  LAVA       .200 .050  .013  2.79                                .006
C  19  WHITE SAND .339 .129  .021  2.79                                .009
C-------------------------------------------------------------------------------
!!-- WSAT is saturated soil moisture (M^3/M^3)
!      DATA WSAT_NOAH /  0.339, 0.421, 0.434, 0.476, 0.476, 0.439, 0.404, 0.464, 
!     &                  0.465, 0.406, 0.468, 0.468, 0.439, 0.489, 0.200, 0.421,
!     &                  0.468, 0.200, 0.339 /
!-- WFC is soil field capacity      
!      DATA WFC_NOAH  /  0.129, 0.192, 0.286, 0.360, 0.360, 0.286, 0.300, 0.382,
!     &                  0.340, 0.292, 0.374, 0.394, 0.286, 0.229, 0.050, 0.145, 
!     &                  0.394, 0.050, 0.129 /
!!-- WWLT is wilting point (M^3/M^3)
!      DATA WWLT_NOAH /  0.021, 0.059, 0.099, 0.176, 0.176, 0.138, 0.143, 0.247, 
!     &                  0.213, 0.205, 0.259, 0.283, 0.138, 0.066, 0.013, 0.059,
!     &                  0.283, 0.013, 0.021 /
!!-- B is slop of the retention curve
!     DATA BSLP_NOAH /   2.79,  4.26,  4.74,  5.33,  5.33,  5.25,  6.77,  8.72,
!     &                   8.17, 10.73, 10.39, 11.55,  5.25,  3.10,  2.79,  4.26,
!     &                  11.55,  2.79,  2.79 /
!!-- WRES is residual soil moisture
!      DATA WRES_NOAH /  0.009, 0.034, 0.061, 0.114, 0.114, 0.089, 0.102, 0.189,
!     &                  0.161, 0.165, 0.207, 0.232, 0.089, 0.052, 0.006, 0.034,
!     &                  0.232, 0.006, 0.009 / 
!! -- RHOB is the soil bulk dnesity
!      DATA RHOB_NOAH /  1.69e6, 1.65e6, 1.60e6, 1.45e6, 1.40e6, 1.53e6, 1.58e6, 1.41e6,
!     &                  1.49e6, 1.55e6, 1.40e6, 1.45e6, 1.51e6, 1.38e6, 1.72e6, 1.56e6,
!     &                  1.44e6, 1.72e6, 1.69e6 /

C-------------------------------------------------------------------------------
C Soil hydraulic properties updated (Patrick Campbell & Jesse Bash,Dec 2016) using obs/models in Kishne et al. (2017):
C "Evaluation and improvement of the default soil hydraulic parameters for the Noah Land Surface Model"
C The updated variables should match with representative run/SOILPARM.TBL for WRF NOAH, such that -->
C WSAT_NOAH = MAXSMC, WFC_NOAH = REFSMC, WWLT_NOAH = WLTSMC, and BSLP_NOAH = BB  
C Note:  Categories of Organic material, Water, Bedrock, Other (land-ice), Playa, and White sand 
C are not updated because no soil characterization data or insufficient number of samples are available. 
C WRES is calculated as (psi_air_dry/psi_sat)^(-1.0/BSLP)*WSAT following CLM soil hydrology 
C relationships, but with updated NOAH values. Note that this is a common paramterization, e.g. Campbel and Norman (1998)
C    where psi_air_dry = -3000 kPa
C   #  SOIL TYPE  WSAT  WFC  WWLT  BSLP  CGSAT   JP   AS   C2R  C1SAT  WRES
C   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____  ____
C   1  SAND       .402 .086  .024  3.36  3.222    4  .387  3.9  .082   .004
C   2  LOAMY SAND .396 .142  .057  4.06  3.057    4  .404  3.7  .098   .010
C   3  SANDY LOAM .413 .213  .081  4.85  3.560    4  .219  1.8  .132   .016
C   4  SILT LOAM  .456 .303  .123  5.72  4.418    6  .105  0.8  .153   .023
C   5  SILT       .438 .346  .064  4.18                                .010
C   6  LOAM       .440 .274  .128  6.01  4.111    6  .148  0.8  .191   .022
C   7  SND CLY LM .416 .288  .168  7.03  3.670    6  .135  0.8  .213   .029
C   8  SLT CLY LM .457 .350  .212  8.49  3.593    8  .127  0.4  .385   .039
C   9  CLAY LOAM  .449 .335  .196  8.20  3.995   10  .084  0.6  .227   .036
C  10  SANDY CLAY .425 .355  .239  8.98  3.058    8  .139  0.3  .421   .037
C  11  SILTY CLAY .467 .392  .264 10.24  3.729   10  .075  0.3  .375   .052
C  12  CLAY       .506 .428  .285 11.56  3.600   12  .083  0.3  .342   .058
C  13  ORGANIC    .439 .286  .118  5.25                                .003
C  14  WATER      .489 .229  .066  3.10  1.0      0  .0    0.0  .0     .052
C  15  BEDROCK    .200 .050  .009  2.79                                .001
C  16  OTHER      .421 .145  .049  4.26  3.222    4  .387  3.9  .082   .010
C  17  PLAYA      .468 .395  .264 11.55                                .147 
C  18  LAVA       .200 .050  .009  2.79                                .001
C  19  WHITE SAND .339 .084  .015  2.79                                .001
C-------------------------------------------------------------------------------
!-- WSAT is saturated soil moisture (M^3/M^3)
      DATA WSAT_NOAH /  0.402, 0.396, 0.413, 0.456, 0.438, 0.440, 0.416, 0.457, 
     &                  0.449, 0.425, 0.467, 0.506, 0.439, 0.489, 0.200, 0.421,
     &                  0.468, 0.200, 0.339 /
!-- WFC is soil field capacity      
      DATA WFC_NOAH  /  0.086, 0.142, 0.213, 0.303, 0.346, 0.274, 0.288, 0.350,
     &                  0.335, 0.355, 0.392, 0.428, 0.286, 0.229, 0.050, 0.145, 
     &                  0.395, 0.050, 0.084 /
!-- WWLT is wilting point (M^3/M^3)
      DATA WWLT_NOAH /  0.024, 0.057, 0.081, 0.123, 0.064, 0.128, 0.168, 0.212, 
     &                  0.196, 0.239, 0.264, 0.285, 0.118, 0.066, 0.009, 0.049,
     &                  0.264, 0.009, 0.015 /
!-- B is slop of the retention curve
      DATA BSLP_NOAH /   3.36,  4.06,  4.85,  5.72,  4.18,  6.01,  7.03,  8.49,
     &                   8.20, 8.98, 10.24, 11.56,  5.25,  3.10,  2.79,  4.26,
     &                  11.55,  2.79,  2.79 /
!-- WRES is residual soil moisture
      DATA WRES_NOAH /  0.004, 0.010, 0.016, 0.023, 0.010, 0.022, 0.029, 0.039,
     &                  0.036, 0.037, 0.052, 0.058, 0.003, 0.052, 0.001, 0.010,
     &                  0.147, 0.001, 0.001 / 
! -- RHOB is the soil bulk dnesity
      DATA RHOB_NOAH /  1.69e6, 1.65e6, 1.60e6, 1.45e6, 1.40e6, 1.53e6, 1.58e6, 1.41e6,
     &                  1.49e6, 1.55e6, 1.40e6, 1.45e6, 1.51e6, 1.38e6, 1.72e6, 1.56e6,
     &                  1.44e6, 1.72e6, 1.69e6 /

!*****************************************************************************
! Vegetation parameters are adapted from WRF 3.8.1 PX LSM
!*****************************************************************************
! MODIS-ONLY (1XX)/MODIS (2XX) LU characterization ... reclassified by NCEP
!-------------------------------------------------------------------------------
      
      TYPE( lu_type ),PRIVATE, PARAMETER :: MODIS( n_modis ) =  (/
!                                                       VEGMN0     LAIMN0             LUF_FAC
!               -name-    -LU cat -  RSMIN   Z00   VEG0   |   LAI0   |  SNUP0   ALBF    |   HGLU_FAC     Cat Desc.
     & lu_type('NCEP_1 ', 'EVEFOR ', 175.0, 100.0, 90.0, 80.0, 4.0, 3.0, 0.08,  12.0, 246.0,  40.0),  ! 1 Evergreen Needleleaf Forest
     & lu_type('NCEP_2 ', 'EVEFOR ', 120.0,  90.0, 95.0, 85.0, 5.0, 4.0, 0.08,  12.0, 247.0, 130.0),  ! 2 Evergreen Broadleaf Forest
     & lu_type('NCEP_3 ', 'DECFOR ', 175.0, 100.0, 95.0, 50.0, 5.0, 1.0, 0.08,  14.0, 267.0,  81.0),  ! 3 Deciduous Needleleaf Forest
     & lu_type('NCEP_4 ', 'DECFOR ', 200.0, 100.0, 95.0, 50.0, 5.0, 1.0, 0.08,  16.0, 268.0,  81.0),  ! 4 Deciduous Broadleaf Forest  
     & lu_type('NCEP_5 ', 'MIXFOR ', 200.0, 100.0, 95.0, 60.0, 5.0, 2.0, 0.08,  13.0, 252.0, 139.0),  ! 5 Mixed Forest
     & lu_type('NCEP_6 ', 'SHRUB  ', 200.0,  15.0, 90.0, 50.0, 3.0, 1.0, 0.03,  22.0, 246.0, 125.0),  ! 6 Closed Shrublands
     & lu_type('NCEP_7 ', 'SHRUB  ', 200.0,  15.0, 75.0, 50.0, 2.5, 1.0, 0.035, 20.0, 246.0, 125.0),  ! 7 Open Shrublands
     & lu_type('NCEP_8 ', 'MIXFOR ', 150.0,  25.0, 80.0, 60.0, 2.5, 1.0, 0.03,  22.0, 246.0, 125.0),  ! 8 Woody Savanna
     & lu_type('NCEP_9 ', 'MIXFOR ', 120.0,  15.0, 70.0, 50.0, 2.0, 1.0, 0.04,  20.0, 246.0, 125.0),  ! 9 Savanna
     & lu_type('NCEP_10', 'HERB   ', 100.0,   7.0, 85.0, 60.0, 2.5, 1.0, 0.04,  19.0, 247.0,  20.0),  ! 10 Grasslands
     & lu_type('NCEP_11', 'WETLAND', 160.0,  20.0, 75.0, 45.0, 3.0, 1.0, 0.015, 14.0, 260.0, 125.0),  ! 11 Perminent Wetlands
     & lu_type('NCEP_12', 'AG     ',  70.0,  10.0, 95.0, 10.0, 3.16, 0.5, 0.04,  18.0, 340.0,  81.0),  ! 12 Croplands  
     & lu_type('NCEP_13', 'URBAN  ', 150.0,  80.0, 40.0, 20.0, 3.0, 1.0, 0.04,  11.0, 338.0, 190.0),  ! 13 Urban and Built-up    
     & lu_type('NCEP_14', 'AG     ', 100.0,  30.0, 95.0, 40.0, 3.0, 1.0, 0.04,  18.0, 298.0,  50.0),  ! 14 Crop/Natural Mosaic
     & lu_type('NCEP_15', 'SNOWICE',9999.0,   1.2,  5.0,  2.0, 0.1, 0.1, 0.02,  60.0,   0.0,   0.0),  ! 15 Snow and Ice   
     & lu_type('NCEP_16', 'BARREN ', 100.0,   5.0, 20.0,  5.0, 1.0, 0.5, 0.02,  25.0, 246.0, 125.0),  ! 16 Sparsely Vegetated
     & lu_type('NCEP_17', 'WATER  ',9999.0,   0.1,  0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 17 IGBP water
     & lu_type('NCEP_18', 'EVEFOR ', 175.0,  30.0, 70.0, 50.0, 3.4, 2.0, 0.80,  15.0, 246.0,  62.0),  ! 18 Wooded Tundra   
     & lu_type('NCEP_19', 'SHRUB  ', 120.0,  15.0, 40.0, 20.0, 2.4, 1.0, 0.40,  15.0, 246.0,  62.0),  ! 19 Mixed Tundra
     & lu_type('NCEP_20', 'HERB   ', 100.0,  10.0, 20.0,  5.0, 1.4, 0.1, 0.015, 25.0, 246.0,  18.0)/) ! 20 Barren Tundra

!****************************************************************************************
!****************************************************************************************
! 50 CLASS NLCD (US only, cats 1-30)/MODIS (Outside US, cats 31-50) LU characterization
!---------------------------------------------------------------------------------------

      TYPE( lu_type ),PRIVATE, PARAMETER :: NLCD50( n_nlcd50 ) =  (/
!                                                           VEGMN0     LAIMN0             LUF_FAC
!               -Name-      -LU cat -   RSMIN    Z00   VEG0   |   LAI0   |  SNUP0   ALBF    |   HGLU_FAC    Cat Desc.
     & lu_type('NLCD_11  ', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 1 Open water
     & lu_type('NLCD_12  ', 'SNOWICE',9999.0,   1.2,   5.0,  2.0, 0.1, 0.1, 0.02,  60.0,   0.0,   0.0),  ! 2 Perennial Ice/snow
     & lu_type('NLCD_21  ', 'URBAN  ', 120.0,  30.0,  90.0, 80.0, 3.0, 1.0, 0.04,  12.0, 312.0, 130.0),  ! 3 Developed, Open space
     & lu_type('NLCD_22  ', 'URBAN  ', 120.0,  40.0,  70.0, 60.0, 3.0, 1.0, 0.04,  11.0, 305.0, 150.0),  ! 4 Developed, Low Intensity
     & lu_type('NLCD_23  ', 'URBAN  ', 140.0,  60.0,  40.0, 30.0, 3.0, 1.0, 0.04,  10.0, 331.0, 170.0),  ! 5 Developed, Medium Intensity
     & lu_type('NLCD_24  ', 'URBAN  ', 160.0, 100.0,  15.0,  5.0, 3.0, 1.0, 0.04,  10.0, 402.0, 190.0),  ! 6 Developed, High Intensity
     & lu_type('NLCD_31  ', 'BARREN ', 100.0,   5.0,  20.0,  5.0, 1.0, 0.5, 0.02,  20.0, 246.0, 125.0),  ! 7 Barren land
     & lu_type('NLCD_32  ', 'BARREN ', 100.0,   5.0,  15.0,  5.0, 0.5, 0.2, 0.02,  35.0, 248.0,   0.0),  ! 8 Unconsolidated Shore
     & lu_type('NLCD_41  ', 'DECFOR ', 200.0, 100.0,  95.0, 50.0, 5.0, 1.0, 0.08,  15.0, 274.0,  81.0),  ! 9 Deciduous Forest
     & lu_type('NLCD_42  ', 'EVEFOR ', 175.0, 100.0,  90.0, 80.0, 4.0, 3.0, 0.08,  10.0, 246.0,  81.0),  ! 10 Evergreen Forest
     & lu_type('NLCD_43  ', 'MIXFOR ', 200.0, 100.0,  95.0, 60.0, 5.0, 2.0, 0.08,  13.0, 254.0, 139.0),  ! 11 Mixed Forest
     & lu_type('NLCD_51  ', 'SHRUB  ', 200.0,  10.0,  50.0, 20.0, 2.0, 1.0, 0.04,  20.0, 246.0,  73.0),  ! 12 Dwarf Scrub
     & lu_type('NLCD_52  ', 'SHRUB  ', 200.0,  15.0,  75.0, 50.0, 2.5, 1.0, 0.04,  20.0, 246.0, 125.0),  ! 13 Shrub/Scrub
     & lu_type('NLCD_71  ', 'HERB   ', 100.0,   7.0,  85.0, 60.0, 2.5, 1.0, 0.04,  19.0, 247.0,  20.0),  ! 14 Grassland/Herbaceous
     & lu_type('NLCD_72  ', 'HERB   ', 100.0,   7.0,  80.0, 20.0, 2.0, 1.0, 0.01,  23.0, 246.0, 125.0),  ! 15 Sedge/Herbaceous
     & lu_type('NLCD_73  ', 'HERB   ', 100.0,   5.0,  80.0, 20.0, 1.0, 1.0, 0.01,  20.0,   0.0,  18.0),  ! 16 Lichens
     & lu_type('NLCD_74  ', 'HERB   ', 100.0,   5.0,  80.0, 20.0, 1.0, 1.0, 0.01,  20.0,   0.0,  18.0),  ! 17 Moss
     & lu_type('NLCD_75  ', 'HERB   ', 100.0,   5.0,  50.0, 20.0, 1.0, 1.0, 0.02,  15.0, 246.0,  62.0),  ! 18 Tundra
     & lu_type('NLCD_81  ', 'AG     ',  80.0,   7.0,  95.0, 80.0, 3.0, 1.0, 0.04,  18.0, 344.0,  50.0),  ! 19 Pasture/Hay
     & lu_type('NLCD_82  ', 'AG     ',  70.0,  10.0,  95.0, 10.0, 3.16, 0.5, 0.04,  18.0, 340.0,  80.0),  ! 20 Cultivated Crops   
     & lu_type('NLCD_90  ', 'WETLAND', 200.0,  55.0,  90.0, 80.0, 5.0, 2.0, 0.08,  15.0, 271.0, 125.0),  ! 21 Woody Wetland
     & lu_type('NLCD_91  ', 'WETLAND', 200.0,  80.0,  90.0, 80.0, 5.0, 2.0, 0.08,  15.0, 267.0, 125.0),  ! 22 Palustrine Forested Wetland   
     & lu_type('NLCD_92  ', 'WETLAND', 164.0,  30.0,  90.0, 80.0, 3.0, 1.0, 0.04,  15.0, 254.0, 125.0),  ! 23 Palustrine Shrub Wetland   
     & lu_type('NLCD_93  ', 'WETLAND', 200.0,  60.0,  90.0, 80.0, 5.0, 2.0, 0.08,  15.0, 267.0, 125.0),  ! 24 Estuarine Forested Wetland   
     & lu_type('NLCD_94  ', 'WETLAND', 164.0,  30.0,  90.0, 80.0, 3.0, 1.0, 0.04,  15.0, 267.0, 125.0),  ! 25 Estuarine Shrub Wetland   
     & lu_type('NLCD_95  ', 'WETLAND', 120.0,  11.0,  60.0, 40.0, 2.0, 1.0, 0.04,  18.0, 248.0, 125.0),  ! 26 Emergent Herbaceous Wetland   
     & lu_type('NLCD_96  ', 'WETLAND', 120.0,  11.0,  80.0, 40.0, 2.0, 1.0, 0.06,  18.0, 269.0, 125.0),  ! 27 Palustrine Emergent Wetland   
     & lu_type('NLCD_97  ', 'WETLAND', 120.0,  11.0,  80.0, 40.0, 2.0, 1.0, 0.06,  18.0, 262.0, 125.0),  ! 28 Estuarine Emergent Wetland   
     & lu_type('NLCD_98  ', 'WETLAND', 100.0,   5.0,  60.0, 20.0, 1.0, 0.5, 0.02,  10.0, 277.0, 125.0),  ! 29 Palustrine Aquatic Bed
     & lu_type('NLCD_99  ', 'WETLAND', 100.0,   5.0,  60.0, 20.0, 1.0, 0.5, 0.02,  10.0, 247.0, 125.0),  ! 30 Estuarine Aquatic Bed   
     & lu_type('MODIS_0  ', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.08,   8.0,   0.0,   0.0),  ! 31 Open water (mapped to 1)
     & lu_type('MODIS_1  ', 'EVEFOR ', 175.0, 100.0,  90.0, 80.0, 4.0, 3.0, 0.08,  12.0, 246.0,  40.0),  ! 32 Evergreen Needleleaf Forest
     & lu_type('MODIS_2  ', 'EVEFOR ', 120.0,  90.0,  95.0, 85.0, 5.0, 4.0, 0.08,  12.0, 247.0, 130.0),  ! 33 Evergreen Broadleaf Forest
     & lu_type('MODIS_3  ', 'DECFOR ', 175.0, 100.0,  95.0, 50.0, 5.0, 1.0, 0.08,  14.0, 267.0,  81.0),  ! 34 Deciduous Needleleaf Forest
     & lu_type('MODIS_4  ', 'DECFOR ', 200.0, 100.0,  95.0, 50.0, 5.0, 1.0, 0.08,  16.0, 268.0,  81.0),  ! 35 Deciduous Broadleaf Forest  
     & lu_type('MODIS_5  ', 'MIXFOR ', 200.0, 100.0,  95.0, 60.0, 5.0, 2.0, 0.08,  13.0, 252.0, 139.0),  ! 36 Mixed Forest
     & lu_type('MODIS_6  ', 'SHRUB  ', 200.0,  15.0,  90.0, 50.0, 3.0, 1.0, 0.03,  22.0, 246.0, 125.0),  ! 37 Closed Shrublands
     & lu_type('MODIS_7  ', 'SHRUB  ', 200.0,  15.0,  75.0, 50.0, 2.5, 1.0, 0.035, 20.0, 246.0, 125.0),  ! 38 Open Shrublands
     & lu_type('MODIS_8  ', 'MIXFOR ', 150.0,  25.0,  80.0, 60.0, 2.5, 1.0, 0.03,  22.0, 246.0, 125.0),  ! 39 Woody Savanna
     & lu_type('MODIS_9  ', 'MIXFOR ', 120.0,  15.0,  70.0, 50.0, 2.0, 1.0, 0.04,  20.0, 246.0, 125.0),  ! 40 Savanna
     & lu_type('MODIS_10 ', 'HERB   ', 100.0,   7.0,  85.0, 60.0, 2.5, 1.0, 0.04,  19.0, 247.0,  20.0),  ! 41 Grasslands
     & lu_type('MODIS_11 ', 'WETLAND', 160.0,  20.0,  75.0, 45.0, 3.0, 1.0, 0.015, 14.0, 260.0, 125.0),  ! 42 Perminent Wetlands
     & lu_type('MODIS_12 ', 'AG     ',  70.0,  10.0,  95.0, 10.0, 3.16, 0.5, 0.04,  18.0, 340.0,  81.0),  ! 43 Croplands  
     & lu_type('MODIS_13 ', 'URBAN  ', 150.0,  80.0,  40.0, 20.0, 3.0, 1.0, 0.04,  11.0, 338.0, 190.0),  ! 44 Urban and Built-up    
     & lu_type('MODIS_14 ', 'AG     ', 100.0,  30.0,  95.0, 40.0, 3.0, 1.0, 0.04,  18.0, 298.0,  50.0),  ! 45 Crop/Natural Mosaic
     & lu_type('MODIS_15 ', 'SNOWICE',9999.0,   1.2,   5.0,  2.0, 0.1, 0.1, 0.02,  60.0,   0.0,   0.0),  ! 46 Snow and Ice   
     & lu_type('MODIS_16 ', 'BARREN ', 100.0,   5.0,  20.0,  5.0, 1.0, 0.5, 0.02,  25.0, 246.0, 125.0),  ! 47 Sparsely Vegetated
     & lu_type('MODIS_0  ', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 48 IGBP water
     & lu_type('MODIS_255', 'SNOWICE',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 49 unclassified   
     & lu_type('MODIS_255', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0)/) ! 50 fill (normally ocean water)                                               
                                               
!****************************************************************************************
!****************************************************************************************
! 40 CLASS MODIS (Outside US, cats 1-20)/NLCD (US only, cats 21-40) LU characterization
!-------------------------------------------------------------------------------

       TYPE( lu_type ),PRIVATE, PARAMETER :: NLCD40( n_nlcd40 ) =  (/
!                                                        VEGMN0     LAIMN0             LUF_FAC
!               -Name-      -LU cat -  RSMIN   Z00   VEG0   |   LAI0   |  SNUP0   ALBF    |   HGLU_FAC       Cat Desc.
     & lu_type('MODIS_1  ', 'EVEFOR ', 175.0, 100.0,  90.0, 80.0, 4.0, 3.0, 0.08,  12.0, 246.0,  40.0),  ! 1 Evergreen Needleleaf Forest
     & lu_type('MODIS_2  ', 'EVEFOR ', 120.0,  90.0,  95.0, 85.0, 5.0, 4.0, 0.08,  12.0, 247.0, 130.0),  ! 2 Evergreen Broadleaf Forest
     & lu_type('MODIS_3  ', 'DECFOR ', 175.0, 100.0,  95.0, 50.0, 5.0, 1.0, 0.08,  14.0, 267.0,  81.0),  ! 3 Deciduous Needleleaf Forest
     & lu_type('MODIS_4  ', 'DECFOR ', 200.0, 100.0,  95.0, 50.0, 5.0, 1.0, 0.08,  16.0, 268.0,  81.0),  ! 4 Deciduous Broadleaf Forest  
     & lu_type('MODIS_5  ', 'MIXFOR ', 200.0, 100.0,  95.0, 60.0, 5.0, 2.0, 0.08,  13.0, 252.0, 139.0),  ! 5 Mixed Forest
     & lu_type('MODIS_6  ', 'SHRUB  ', 200.0,  15.0,  90.0, 50.0, 3.0, 1.0, 0.03,  22.0, 246.0, 125.0),  ! 6 Closed Shrublands
     & lu_type('MODIS_7  ', 'SHRUB  ', 200.0,  15.0,  75.0, 50.0, 2.5, 1.0, 0.035, 20.0, 246.0, 125.0),  ! 7 Open Shrublands
     & lu_type('MODIS_8  ', 'MIXFOR ', 150.0,  25.0,  80.0, 60.0, 2.5, 1.0, 0.03,  22.0, 246.0, 125.0),  ! 8 Woody Savanna
     & lu_type('MODIS_9  ', 'MIXFOR ', 120.0,  15.0,  70.0, 50.0, 2.0, 1.0, 0.04,  20.0, 246.0, 125.0),  ! 9 Savanna
     & lu_type('MODIS_10 ', 'GRASS  ', 100.0,   7.0,  85.0, 60.0, 2.5, 1.0, 0.04,  19.0, 247.0,  20.0),  ! 10 Grasslands
     & lu_type('MODIS_11 ', 'WETLAND', 160.0,  20.0,  75.0, 45.0, 3.0, 1.0, 0.015, 14.0, 260.0, 125.0),  ! 11 Perminent Wetlands
     & lu_type('MODIS_12 ', 'AG     ',  70.0,  10.0,  95.0, 10.0, 3.16, 0.5, 0.04,  18.0, 340.0,  81.0),  ! 12 Croplands  
     & lu_type('MODIS_13 ', 'URBAN  ', 150.0,  80.0,  40.0, 20.0, 3.0, 1.0, 0.04,  11.0, 338.0, 190.0),  ! 13 Urban and Built-up    
     & lu_type('MODIS_14 ', 'AG     ', 100.0,  30.0,  95.0, 40.0, 3.0, 1.0, 0.04,  18.0, 298.0,  50.0),  ! 14 Crop/Natural Mosaic
     & lu_type('MODIS_15 ', 'SNOWICE',9999.0,   1.2,   5.0,  2.0, 0.1, 0.1, 0.02,  60.0,   0.0,   0.0),  ! 15 Snow and Ice   
     & lu_type('MODIS_16 ', 'BARREN ', 100.0,   5.0,  20.0,  5.0, 1.0, 0.5, 0.02,  25.0, 246.0, 125.0),  ! 16 Sparsely Vegetated
     & lu_type('MODIS_0  ', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 17 IGBP water
     & lu_type('MODIS_255', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 18 unclassified   
     & lu_type('MODIS_255', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 19 fill (normally ocean water)
     & lu_type('MODIS_255', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 20 unclassified   
     & lu_type('NLCD_11  ', 'WATER  ',9999.0,   0.1,   0.0,  0.0, 0.0, 0.0, 0.01,   8.0,   0.0,   0.0),  ! 21 Open water
     & lu_type('NLCD_12  ', 'SNOWICE',9999.0,   1.2,   5.0,  2.0, 0.1, 0.1, 0.02,  60.0,   0.0,   0.0),  ! 22 Perennial Ice/snow
     & lu_type('NLCD_21  ', 'URBAN  ', 120.0,  30.0,  90.0, 80.0, 3.0, 1.0, 0.04,  12.0, 312.0, 130.0),  ! 23 Developed, Open space
     & lu_type('NLCD_22  ', 'URBAN  ', 120.0,  40.0,  70.0, 60.0, 3.0, 1.0, 0.04,  11.0, 305.0, 150.0),  ! 24 Developed, Low Intensity
     & lu_type('NLCD_23  ', 'URBAN  ', 140.0,  60.0,  40.0, 30.0, 3.0, 1.0, 0.04,  10.0, 331.0, 170.0),  ! 25 Developed, Medium Intensity
     & lu_type('NLCD_24  ', 'URBAN  ', 160.0, 100.0,  15.0,  5.0, 3.0, 1.0, 0.04,  10.0, 402.0, 190.0),  ! 26 Developed, High Intensity
     & lu_type('NLCD_31  ', 'BARREN ', 100.0,   5.0,  20.0,  5.0, 1.0, 0.5, 0.02,  20.0, 246.0, 125.0),  ! 27 Barren land
     & lu_type('NLCD_41  ', 'DECFOR ', 200.0, 100.0,  95.0, 50.0, 5.0, 1.0, 0.08,  15.0, 274.0,  81.0),  ! 28 Deciduous Forest
     & lu_type('NLCD_42  ', 'EVEFOR ', 175.0, 100.0,  90.0, 80.0, 4.0, 3.0, 0.08,  10.0, 246.0,  81.0),  ! 29 Evergreen Forest
     & lu_type('NLCD_43  ', 'MIXFOR ', 200.0, 100.0,  95.0, 60.0, 5.0, 2.0, 0.08,  13.0, 254.0, 139.0),  ! 30 Mixed Forest
     & lu_type('NLCD_51  ', 'SHRUB  ', 200.0,  10.0,  50.0, 20.0, 2.0, 1.0, 0.04,  20.0, 246.0,  73.0),  ! 31 Dwarf Scrub
     & lu_type('NLCD_52  ', 'SHRUB  ', 200.0,  15.0,  75.0, 50.0, 2.5, 1.0, 0.04,  20.0, 246.0, 125.0),  ! 32 Shrub/Scrub
     & lu_type('NLCD_71  ', 'GRASS  ', 100.0,   7.0,  85.0, 60.0, 2.5, 1.0, 0.04,  19.0, 247.0,  20.0),  ! 33 Grassland/Herbaceous
     & lu_type('NLCD_72  ', 'HERB   ', 100.0,   7.0,  80.0, 20.0, 2.0, 1.0, 0.01,  23.0, 246.0, 125.0),  ! 34 Sedge/Herbaceous
     & lu_type('NLCD_73  ', 'HERB   ', 100.0,   5.0,  80.0, 20.0, 1.0, 1.0, 0.01,  20.0,   0.0,  18.0),  ! 35 Lichens
     & lu_type('NLCD_74  ', 'HERB   ', 100.0,   5.0,  80.0, 20.0, 1.0, 1.0, 0.01,  20.0,   0.0,  18.0),  ! 36 Moss
     & lu_type('NLCD_81  ', 'HAY     ',  80.0,   7.0,  95.0, 80.0, 3.0, 1.0, 0.04,  18.0, 344.0,  50.0),  ! 37 Pasture/Hay
     & lu_type('NLCD_82  ', 'AG     ',  70.0,  10.0,  95.0, 10.0, 3.16, 0.5, 0.04,  18.0, 340.0,  80.0), ! 38 Cultivated Crops   
     & lu_type('NLCD_90  ', 'WETLAND', 200.0,  55.0,  90.0, 80.0, 5.0, 2.0, 0.08,  15.0, 271.0, 125.0),  ! 39 Woody Wetland 
     & lu_type('NLCD_95  ', 'WETLAND', 120.0,  11.0,  60.0, 40.0, 2.0, 1.0, 0.04,  18.0, 248.0, 125.0)/) ! 40 Emergent Herbaceous Wetland

!*****************************************************************************
!*****************************************************************************
!  USGS LU characterization  --  note below may not match those for the same category in a diff
!                                classification system.  This was done to maintain compatibility with MM5.               
!-----------------------------------------------------------------------------                   

      TYPE( lu_type ), PRIVATE, PARAMETER :: USGS24( n_usgs ) =  (/
!                                                          VEGMN0     LAIMN0             LUF_FAC
!               ---Name---   -LU cat -   RSMIN    Z00  VEG0   |   LAI0   |  SNUP0   ALBF    |   HGLU_FAC     Cat Desc.
     & lu_type('USGS_Urban', 'URBAN   ', 150.0,  50.0, 40.0, 20.0, 2.0,  0.5, 0.04,  15.0, 338.0, 190.0),  ! 1 Urban or Built-up Land
     & lu_type('USGS_DrCrp', 'AG      ',  70.0,  10.0, 95.0, 15.0, 3.16,  0.5, 0.04,  17.0, 340.0,  80.0),  ! 2 Dryland Crop and Pasture
     & lu_type('USGS_IrCrp', 'AG      ',  60.0,  10.0, 95.0, 10.0, 3.16,  0.5, 0.04,  18.0, 340.0,  32.0),  ! 3 Irr. Cropland and Pasture
     & lu_type('USGS_MixCp', 'AG      ',  70.0,  10.0, 95.0, 15.0, 3.16,  0.5, 0.04,  18.0, 340.0,  56.0),  ! 4 Mixed Dry/Irr Crop and Past
     & lu_type('USGS_CrGrM', 'AG      ',  80.0,  10.0, 95.0, 35.0, 2.5,  1.0, 0.04,  18.0, 298.0,  50.0),  ! 5 Grassland/Cropland Mosaic
     & lu_type('USGS_CrWdM', 'AG      ', 180.0,  40.0, 95.0, 40.0, 4.0,  1.5, 0.04,  16.0, 298.0,  26.0),  ! 6 Woodland/Cropland Mosaic
     & lu_type('USGS_GrsLd', 'HERB    ', 100.0,   7.0, 95.0, 70.0, 2.5,  1.0, 0.04,  19.0, 247.0,  20.0),  ! 7 Grassland
     & lu_type('USGS_ShrLd', 'SHRUB   ', 200.0,  20.0, 70.0, 50.0, 3.0,  1.0, 0.03,  22.0, 246.0, 125.0),  ! 8 Shrubland
     & lu_type('USGS_ShrGr', 'SHRUB   ', 150.0,  20.0, 85.0, 60.0, 3.0,  1.0, 0.035, 20.0, 246.0,  73.0),  ! 9 Mixed Shrubland/Grassland
     & lu_type('USGS_Savan', 'MIXFOR  ', 120.0,  20.0, 80.0, 60.0, 2.0,  1.0, 0.04,  20.0, 246.0,  51.0),  ! 10 Savanna
     & lu_type('USGS_DBFst', 'DECFOR  ', 200.0,  50.0, 95.0, 50.0, 5.0,  1.0, 0.08,  16.0, 268.0,  81.0),  ! 11 Broadleaf Deciduous Forest
     & lu_type('USGS_DNFst', 'DECFOR  ', 175.0,  50.0, 95.0, 50.0, 5.0,  1.0, 0.08,  14.0, 267.0,  81.0),  ! 12 Deciduous Coniferous Forest
     & lu_type('USGS_EBFst', 'EVEFOR  ', 120.0,  40.0, 95.0, 85.0, 5.0,  4.0, 0.08,  12.0, 247.0, 130.0),  ! 13 Evergreen Broadleaf Forest
     & lu_type('USGS_ENFst', 'EVEFOR  ', 175.0,  50.0, 90.0, 80.0, 4.0,  3.0, 0.08,  12.0, 246.0,  40.0),  ! 14 Evergreen Coniferous Forest
     & lu_type('USGS_MxFst', 'MIXFOR  ', 200.0,  50.0, 95.0, 60.0, 5.0,  2.0, 0.08,  13.0, 252.0, 139.0),  ! 15 Mixed forest
     & lu_type('USGS_Water', 'WATER   ',9999.0,   0.1,  0.0,  0.0, 0.0,  0.0, 0.01,   8.0,   0.0,   0.0),  ! 16 Water
     & lu_type('USGS_HWtld', 'WETLAND ', 164.0,  15.0, 60.0, 40.0, 2.0,  1.0, 0.01,  14.0, 248.0, 125.0),  ! 17 Herbaceous Wetland
     & lu_type('USGS_WWtld', 'WETLAND ', 200.0,  45.0, 90.0, 80.0, 5.0,  3.0, 0.01,  14.0, 260.0, 125.0),  ! 18 Forested Wetlands
     & lu_type('USGS_BarSp', 'BARREN  ', 100.0,   5.0, 10.0,  5.0, 0.5,  0.2, 0.02,  25.0, 246.0,  26.0),  ! 19 Barren or Sparsely Vegetated
     & lu_type('USGS_HrTun', 'HERB    ', 150.0,  10.0, 20.0, 10.0, 1.0,  0.5, 0.02,  15.0, 246.0,  62.0),  ! 20 Herbaceous Tundra
     & lu_type('USGS_WdTun', 'SHRUB   ', 200.0,  10.0, 30.0, 10.0, 1.0,  0.5, 0.025, 15.0, 246.0,  62.0),  ! 21 Shrub and Brush Tundra
     & lu_type('USGS_MxTun', 'SHRUB   ', 150.0,   5.0, 20.0,  5.0, 1.0,  0.5, 0.025, 15.0, 246.0,  62.0),  ! 22 Mixed Tundra
     & lu_type('USGS_BGTun', 'BARREN  ', 100.0,   5.0,  5.0,  2.0, 0.1,  0.1, 0.025, 25.0, 246.0,  18.0),  ! 23 Bare Ground Tundra
     & lu_type('USGS_SnwIc', 'SNOWICE ', 300.0,   5.0,  5.0,  2.0, 0.1,  0.1, 0.02,  55.0,   0.0,   0.0)/) ! 24 Snowfields or Glaciers

      CONTAINS                                 
         SUBROUTINE INIT_LSM( JDate, JTime )   
                                               
         USE HGRD_DEFN                         
         USE UTILIO_DEFN                       
#ifdef twoway                                  
         USE twoway_data_module, ONLY : num_land_cat
#endif                                         
                                               
         IMPLICIT NONE                         
                                               
         INCLUDE SUBST_FILES_ID  ! file name parameters         
                                               
         INTEGER, INTENT( In )  :: jdate       
         INTEGER, INTENT( In )  :: jtime  
         CHARACTER( 240 )       :: XMSG = ' '  
         CHARACTER(  16 ), SAVE :: PNAME = 'Init_LSM'
         INTEGER      :: GXOFF, GYOFF            ! global origin offset from file
         INTEGER      :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
         INTEGER l                             
                                               
         LAND_SCHEME = 'UNKNOWN'               
         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
                                               
         IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
            XMSG = 'COULD NOT GET GRID_CRO_2D FILE DESCRIPTION '
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
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
#else                                          
         DO l = 1, NVARS3D ! determine land character
            IF ( VNAME3D( l ) .EQ. 'DLUSE' ) THEN
               IF ( INDEX( VDESC3D( l ), 'USGS24' ) .NE. 0 )     LAND_SCHEME = 'USGS24'
               IF ( INDEX( VDESC3D( l ), 'MODIS' ) .NE. 0 )      LAND_SCHEME = 'MODIS'
               IF ( INDEX( VDESC3D( l ), 'NLCD-MODIS' ) .NE. 0 ) LAND_SCHEME = 'NLCD50'
               IF ( INDEX( VDESC3D( l ), 'NLCD50' ) .NE. 0 )     LAND_SCHEME = 'NLCD50'
               IF ( INDEX( VDESC3D( l ), 'NLCD40' ) .NE. 0 )     LAND_SCHEME = 'NLCD40'
            END IF                             
         END DO                                
#endif                                         
!       Write( *,* ) 'Land use scheme is ', TRIM(LAND_SCHEME) 
                                               
         SELECT CASE( LAND_SCHEME )            
            CASE( 'USGS24' )                   
               n_lufrac    = n_usgs
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       RSMIN    ( n_lufrac ),
     &                       LAI0     ( n_lufrac ),
     &                       LAIMN0   ( n_lufrac ),
     &                       VEG0     ( n_lufrac ),
     &                       VEGMN0   ( n_lufrac ),
     &                       Z00      ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = USGS24%lu_cat
               name_lu  = USGS24%name
               RSMIN    = USGS24%RSMIN
               LAI0     = USGS24%LAI0
               LAIMN0   = USGS24%LAIMN0
               VEG0     = USGS24%VEG0
               VEGMN0   = USGS24%VEGMN0
               Z00      = USGS24%Z00/100.0
               LUF_FAC  = USGS24%LUF_FAC
               HGLU_FAC = USGS24%HGLU_FAC
            CASE( 'MODIS' )                    
               n_lufrac    = n_modis 
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       RSMIN    ( n_lufrac ),
     &                       LAI0     ( n_lufrac ),
     &                       LAIMN0   ( n_lufrac ),
     &                       VEG0     ( n_lufrac ),
     &                       VEGMN0   ( n_lufrac ), 
     &                       Z00      ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = MODIS%lu_cat
               name_lu  = MODIS%name
               RSMIN    = MODIS%RSMIN
               LAI0     = MODIS%LAI0
               LAIMN0   = MODIS%LAIMN0
               VEG0     = MODIS%VEG0
               VEGMN0   = MODIS%VEGMN0
               Z00      = MODIS%Z00/100.0
               LUF_FAC  = MODIS%LUF_FAC
               HGLU_FAC = MODIS%HGLU_FAC
            CASE( 'NLCD50' )               
               n_lufrac    = n_nlcd50  
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       RSMIN    ( n_lufrac ),
     &                       LAI0     ( n_lufrac ),
     &                       LAIMN0   ( n_lufrac ),
     &                       VEG0     ( n_lufrac ),
     &                       VEGMN0   ( n_lufrac ), 
     &                       Z00      ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = NLCD50%lu_cat
               name_lu  = NLCD50%name
               RSMIN    = NLCD50%RSMIN
               LAI0     = NLCD50%LAI0
               LAIMN0   = NLCD50%LAIMN0
               VEG0     = NLCD50%VEG0
               VEGMN0   = NLCD50%VEGMN0
               Z00      = NLCD50%Z00/100.0
               LUF_FAC  = NLCD50%LUF_FAC
               HGLU_FAC = NLCD50%HGLU_FAC
            CASE( 'NLCD40' )               
               n_lufrac    = n_nlcd40   
               If ( .Not. Allocated ( cat_lu ) ) Then
                  Allocate ( cat_lu   ( n_lufrac ),
     &                       name_lu  ( n_lufrac ),
     &                       RSMIN    ( n_lufrac ),
     &                       LAI0     ( n_lufrac ),
     &                       LAIMN0   ( n_lufrac ),
     &                       VEG0     ( n_lufrac ),
     &                       VEGMN0   ( n_lufrac ), 
     &                       Z00      ( n_lufrac ),
     &                       LUF_FAC  ( n_lufrac ),
     &                       HGLU_FAC ( n_lufrac ), STAT = ALLOCSTAT )
                  IF ( ALLOCSTAT .NE. 0 ) THEN
                     XMSG = 'Failure allocating land use data'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               End If
               cat_lu   = NLCD40%lu_cat
               name_lu  = NLCD40%name
               RSMIN    = NLCD40%RSMIN
               LAI0     = NLCD40%LAI0
               LAIMN0   = NLCD40%LAIMN0
               VEG0     = NLCD40%VEG0
               VEGMN0   = NLCD40%VEGMN0
               Z00      = NLCD40%Z00/100.0
               LUF_FAC  = NLCD40%LUF_FAC
               HGLU_FAC = NLCD40%HGLU_FAC
            CASE DEFAULT                       
               xmsg = 'Land use scheme not supported'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END SELECT                            
                                               
         END SUBROUTINE Init_LSM               
                                               
      END MODULE LSM_Mod                       
