
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
C      Feb. 2019  D. Wong      Implemented centralized I/O approach
C      25 Jul 19 D.Wong        Based on logical variable WRF_V4P defined in RUNTIME_VARS to handle
C                              various number of soil type from different WRF version
C------------------------------------------------------------------------------
       
      Implicit None
      
      INTEGER, PARAMETER :: N_SOIL_TYPE_WRFV4P = 16
      INTEGER, PARAMETER :: N_SOIL_TYPE_WRFV3  = 11
      INTEGER :: N_SOIL_TYPE

      REAL, ALLOCATABLE          :: wwlt_px (:)
      REAL, ALLOCATABLE          :: wsat_px (:)
      REAL, ALLOCATABLE          :: bslp_px (:)
      REAL, ALLOCATABLE          :: wres_px (:)
      REAL, ALLOCATABLE          :: wfc_px  (:)
      REAL, ALLOCATABLE          :: rhob_px (:)
      REAL, ALLOCATABLE          :: psi_sat (:)
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

      INTEGER, SAVE               :: n_xref_lu
      INTEGER, SAVE               :: n_stage_lu
      INTEGER, PRIVATE, PARAMETER :: N_Map_Max = 200
      INTEGER, PRIVATE            :: ALLOCSTAT
      
      TYPE DEP_MOD_LU_DATA
         Character( 16 ) :: LU_Name
         CHARACTER( 16 ) :: lu_cat
         Real            :: RSMIN
         Real            :: Z00
         Real            :: VEG0
         Real            :: VEGMN0
         Real            :: LAI0
         Real            :: LAIMN0
         Real            :: ALBF
         Real            :: Gamma_NH3_st
         Real            :: HGLU_FAC
         Integer         :: LU_Index
      END TYPE
      TYPE( DEP_MOD_LU_DATA ) STAGE_LU_DATA( N_Map_Max )

      TYPE MET_MOD_LU_DATA
         Character(20) :: Met_LU_Name
         Integer       :: Met_Index
         Character(20) :: Dep_LU_Name
         Integer       :: Dep_Index
         Real          :: Factor
         Character(30) :: Description
      END TYPE
      TYPE( MET_MOD_LU_DATA ) MET_TO_STAGE_LU( N_Map_Max )

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
      REAL, PARAMETER :: WSAT_PX_WRFV3(N_SOIL_TYPE_WRFV3) =  
     &      (/ 0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477,
     &         0.476, 0.426, 0.482, 0.482 /)
!-- WWLT is wilting point (M^3/M^3) (JN90)
      REAL, PARAMETER :: WWLT_PX_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/ 0.068, 0.075, 0.114, 0.179, 0.155, 0.175, 0.218,
     &         0.250, 0.219, 0.283, 0.286 /)
!-- B is slop of the retention curve (NP89)
      REAL, PARAMETER :: BSLP_PX_WRFV3(N_SOIL_TYPE_WRFV3) =
     &      (/  4.05,  4.38,  4.90,  5.30,  5.39,  7.12,  7.75,
     &          8.52, 10.40, 10.40, 11.40 /)
! -- RHOB is the soil bulk dnesity following Lyons et al 1997   
      REAL, PARAMETER :: RHOB_PX_WRFV3(N_SOIL_TYPE_WRFV3) =
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
      REAL, PARAMETER :: WSAT_PX_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  0.395, 0.410, 0.435, 0.485, 0.480, 0.451, 0.420, 0.477,
     &          0.476, 0.426, 0.482, 0.482, 0.451, 0.482, 0.482, 0.420 /)
!-- WWLT is wilting point (M^3/M^3) (JN90)
      REAL, PARAMETER :: WWLT_PX_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  0.068, 0.075, 0.114, 0.179, 0.150, 0.155, 0.175, 0.218,
     &          0.250, 0.219, 0.283, 0.286, 0.155, 0.286, 0.286, 0.175 /)
!-- B is slop of the retention curve (NP89)
      REAL, PARAMETER :: BSLP_PX_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,  7.75,
     &          8.52, 10.40, 10.40, 11.40,  5.39, 11.40, 11.40,  7.12 /)
! -- RHOB is the soil bulk dnesity following Lyons et al 1997   
      REAL, PARAMETER :: RHOB_PX_WRFV4P(N_SOIL_TYPE_WRFV4P) =  
     &      (/  1.59e6, 1.55e6, 1.53e6, 1.53e6, 1.53e6, 1.55e6, 1.62e6, 1.67e6,
     &          1.66e6, 1.83e6, 1.78e6, 1.83e6, 1.62e6, 1.83e6, 1.83e6, 1.67e6 /)

C-------------------------------------------------------------------------------
C Soil hydrolic properties as calculcated from soil sand and clay fractions 
C in WRF-CLM in WRF 3.7.1-3.8.1. 
C WRES is calculated as WSAT*(psi_air_dry/psi_sat)^(-1.0/BSLP) following CLM soil hydrology 
C relationships. Note that this is a common paramterization, e.g. Campbel and Norman (1998)
C    where psi_air_dry = -300,000 kPa
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
C Soil hydraulic properties updated (Patrick Campbell & Jesse Bash,Dec 2016) using obs/models in Kishne et al. (2017):
C "Evaluation and improvement of the default soil hydraulic parameters for the Noah Land Surface Model"
C The updated variables should match with representative run/SOILPARM.TBL for WRF NOAH, such that -->
C WSAT_NOAH = MAXSMC, WFC_NOAH = REFSMC, WWLT_NOAH = WLTSMC, and BSLP_NOAH = BB  
C Note:  Categories of Organic material, Water, Bedrock, Other (land-ice), Playa, and White sand 
C are not updated because no soil characterization data or insufficient number of samples are available. 
C WRES is calculated as (psi_air_dry/psi_sat)^(-1.0/BSLP)*WSAT following CLM soil hydrology 
C relationships, but with updated NOAH values. Note that this is a common paramterization, e.g. Campbel and Norman (1998)
C    where psi_air_dry = -300,000 kPa
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

         ALLOCATE (WSAT_PX(N_SOIL_TYPE),
     &             WFC_PX(N_SOIL_TYPE),
     &             WWLT_PX(N_SOIL_TYPE),
     &             BSLP_PX(N_SOIL_TYPE),
     &             WRES_PX(N_SOIL_TYPE),
     &             RHOB_PX(N_SOIL_TYPE),
     &             PSI_SAT(N_SOIL_TYPE),
     &             STAT=STAT)

         IF (WRF_V4P) THEN
            WSAT_PX = WSAT_PX_WRFV4P
            WWLT_PX = WWLT_PX_WRFV4P
            BSLP_PX = BSLP_PX_WRFV4P
            RHOB_PX = RHOB_PX_WRFV4P
         ELSE
            WSAT_PX = WSAT_PX_WRFV3
            WWLT_PX = WWLT_PX_WRFV3
            BSLP_PX = BSLP_PX_WRFV3
            RHOB_PX = RHOB_PX_WRFV3
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
         Call STAGE_LU_MAPPER                           
                                               
         END SUBROUTINE Init_LSM               
         
         SUBROUTINE STAGE_LU_MAPPER
            
            USE RUNTIME_VARS, ONLY: EMISSCTRL, LOGDEV!, log_message, log_subheading
            use UTILIO_DEFN
      
            IMPLICIT NONE

            Logical            :: n_stage_end
            Logical            :: n_xref_end
            CHARACTER( 200 )   :: XMSG
            INTEGER            :: EMCTRL_NML
            INTEGER            :: STAT
       
            NAMELIST / STAGE_LU         / STAGE_LU_DATA
            NAMELIST / STAGE_MODIS_20   / MET_TO_STAGE_LU
            NAMELIST / STAGE_CUSTUM     / MET_TO_STAGE_LU
            NAMELIST / STAGE_NLCD_40    / MET_TO_STAGE_LU
            
            STAGE_LU_DATA%LU_Name      = 'N/A'
            STAGE_LU_DATA%LU_Cat       = 'N/A'
            STAGE_LU_DATA%RSMIN        = 9999.0
            STAGE_LU_DATA%Z00          = 0.0
            STAGE_LU_DATA%VEG0         = 0.0
            STAGE_LU_DATA%VEGMN0       = 0.0
            STAGE_LU_DATA%LAI0         = 0.0
            STAGE_LU_DATA%LAIMN0       = 0.0
            STAGE_LU_DATA%ALBF         = 0.0
            STAGE_LU_DATA%Gamma_NH3_st = 0.0
            STAGE_LU_DATA%HGLU_FAC     = 0.0
            STAGE_LU_DATA%LU_Index     = 0

            MET_TO_STAGE_LU%Met_LU_Name = 'N/A'
            MET_TO_STAGE_LU%Met_Index   = 0
            MET_TO_STAGE_LU%Dep_LU_Name = 'N/A'
            MET_TO_STAGE_LU%Dep_Index   = 0
            MET_TO_STAGE_LU%Factor      = 0.0
            MET_TO_STAGE_LU%Description = 'N/A'                  

      ! Retrieve the Name of the Emission Control File
            IF ( EMISSCTRL .EQ. "EMISSCTRL_NML" ) THEN
               WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &           'You have chosen not to indicate the location of an',
     &           'Emission Control namelist file. Default settings ',
     &           'will be assumed.'
               RETURN
            END IF

      ! Open Emission Control Namelist File
            EMCTRL_NML = JUNIT()
            OPEN( FILE = EMISSCTRL, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &            POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
            IF ( STAT .NE. 0 ) THEN
               WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &                 'emissions control namelist file: ',TRIM( EMISSCTRL )
               CALL M3EXIT( 'STAGE_LU_MAPPER',0,0,XMSG,1 )
            END IF

            REWIND( EMCTRL_NML )
            READ( NML = STAGE_LU, UNIT = EMCTRL_NML, IOSTAT=STAT )
            IF ( STAT .NE. 0 ) THEN
               WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'STAGE land use section of the Emissions ',
     &           'Control Namelist. Default values for this section ',
     &           'will be assumed.'
            END IF
!            Write(logdev,*) 'N_STAGE_LU', N_STAGE_LU
!            n_stage_lu   = 1
            n_stage_lu = maxval(STAGE_LU_DATA%LU_Index)
!            n_stage_end = .TRUE. ! rename
!           Do While( n_stage_end )
!               If( STAGE_LU_DATA(n_stage_lu)%LU_Name .Eq. 'N/A' ) Then
!                  n_stage_end = .FALSE.
!                  Write(logdev,*) 'STAGE_LU          :', STAGE_LU_DATA(n_stage_lu)%LU_Name
!                  Write(logdev,*) 'STAGE LU Cat      :', STAGE_LU_DATA(n_stage_lu)%LU_Cat
!                  Write(logdev,*) 'STAGE RSMIN       :', STAGE_LU_DATA(n_stage_lu)%RSMIN
!                  Write(logdev,*) 'STAGE Z00         :', STAGE_LU_DATA(n_stage_lu)%Z00
!                  Write(logdev,*) 'STAGE VEG0        :', STAGE_LU_DATA(n_stage_lu)%VEG0
!                  Write(logdev,*) 'STAGE VEGMN0      :', STAGE_LU_DATA(n_stage_lu)%VEGMN0
!                  Write(logdev,*) 'STAGE LAI0        :', STAGE_LU_DATA(n_stage_lu)%LAI0
!                  Write(logdev,*) 'STAGE LAIMN0      :', STAGE_LU_DATA(n_stage_lu)%LAIMN0
!                  Write(logdev,*) 'STAGE ALBF        :', STAGE_LU_DATA(n_stage_lu)%ALBF
!                  Write(logdev,*) 'STAGE Gamma_NH3_st:', STAGE_LU_DATA(n_stage_lu)%Gamma_NH3_st
!                  Write(logdev,*) 'STAGE HGLU_FAC    :', STAGE_LU_DATA(n_stage_lu)%HGLU_FAC
!                  Write(logdev,*) 'STAGE LU Index    :', STAGE_LU_DATA(n_stage_lu)%LU_Index
!                  n_stage_lu = n_stage_lu - 1
!               Else
!                  Write(logdev,*) 'STAGE_LU          :', STAGE_LU_DATA(n_stage_lu)%LU_Name
!                  Write(logdev,*) 'STAGE LU Cat      :', STAGE_LU_DATA(n_stage_lu)%LU_Cat
!                  Write(logdev,*) 'STAGE RSMIN       :', STAGE_LU_DATA(n_stage_lu)%RSMIN
!                  Write(logdev,*) 'STAGE Z00         :', STAGE_LU_DATA(n_stage_lu)%Z00
!                  Write(logdev,*) 'STAGE VEG0        :', STAGE_LU_DATA(n_stage_lu)%VEG0
!                  Write(logdev,*) 'STAGE VEGMN0      :', STAGE_LU_DATA(n_stage_lu)%VEGMN0
!                  Write(logdev,*) 'STAGE LAI0        :', STAGE_LU_DATA(n_stage_lu)%LAI0
!                  Write(logdev,*) 'STAGE LAIMN0      :', STAGE_LU_DATA(n_stage_lu)%LAIMN0
!                  Write(logdev,*) 'STAGE ALBF        :', STAGE_LU_DATA(n_stage_lu)%ALBF
!                  Write(logdev,*) 'STAGE Gamma_NH3_st:', STAGE_LU_DATA(n_stage_lu)%Gamma_NH3_st
!                  Write(logdev,*) 'STAGE HGLU_FAC    :', STAGE_LU_DATA(n_stage_lu)%HGLU_FAC
!                  Write(logdev,*) 'STAGE LU Index    :', STAGE_LU_DATA(n_stage_lu)%LU_Index
!                  n_stage_lu = n_stage_lu + 1
!               End If
!            End Do

            REWIND( EMCTRL_NML )            
            SELECT CASE( LAND_SCHEME )            
               CASE( 'MODIS' )       
                  READ( NML = STAGE_MODIS_20, UNIT = EMCTRL_NML, IOSTAT=STAT )
                  IF ( STAT .NE. 0 ) THEN
                     WRITE( LOGDEV, "(A,A,A)" ),
     &                 'ERROR: Something went wrong while reading the ',
     &                 'STAGE land use mapping section for MODIS 20 data ',
     &                 'of the Emissions Control Namelist.', TRIM( EMISSCTRL )
                     CALL M3EXIT( 'STAGE_LU_MAPPER',0,0,XMSG,1 )
                  END IF

                  n_xref_lu   = 1
                  n_xref_end = .TRUE. ! rename
                  Do While( n_xref_end )
                     If( MET_TO_STAGE_LU(n_xref_lu)%Met_LU_Name .Eq. 'N/A' ) Then
                        n_xref_end = .FALSE.
!                        Write(logdev,*) 'MET_LU     :', MET_TO_STAGE_LU(n_xref_lu)%Met_LU_Name
!                        Write(logdev,*) 'Met Index  :', MET_TO_STAGE_LU(n_xref_lu)%Met_Index
!                        Write(logdev,*) 'Met Lu Name:', MET_TO_STAGE_LU(n_xref_lu)%Dep_LU_Name
!                        Write(logdev,*) 'Dep Index  :', MET_TO_STAGE_LU(n_xref_lu)%Dep_Index
!                        Write(logdev,*) 'Factor     :', MET_TO_STAGE_LU(n_xref_lu)%Factor
!                        Write(logdev,*) 'Description:', MET_TO_STAGE_LU(n_xref_lu)%Description
                        n_xref_lu = n_xref_lu - 1
                     Else
                        Write(logdev,*) 'MET_LU     :', MET_TO_STAGE_LU(n_xref_lu)%Met_LU_Name
                        Write(logdev,*) 'Met Index  :', MET_TO_STAGE_LU(n_xref_lu)%Met_Index
                        Write(logdev,*) 'Met Lu Name:', MET_TO_STAGE_LU(n_xref_lu)%Dep_LU_Name
                        Write(logdev,*) 'Dep Index  :', MET_TO_STAGE_LU(n_xref_lu)%Dep_Index
                        Write(logdev,*) 'Factor     :', MET_TO_STAGE_LU(n_xref_lu)%Factor
                        Write(logdev,*) 'Description:', MET_TO_STAGE_LU(n_xref_lu)%Description
                        n_xref_lu = n_xref_lu + 1
                     End If
                  End Do

               CASE( 'NLCD40' )  
                  READ( NML = STAGE_NLCD_40, UNIT = EMCTRL_NML, IOSTAT=STAT )
                  IF ( STAT .NE. 0 ) THEN
                     WRITE( LOGDEV, "(A,A,A)" ),
     &                 'ERROR: Something went wrong while reading the ',
     &                 'STAGE land use mapping section for NLCD 40 data ',
     &                 'of the Emissions Control Namelist.', TRIM( EMISSCTRL )
                     CALL M3EXIT( 'STAGE_LU_MAPPER',0,0,XMSG,1 )
                  END IF

                  n_xref_lu   = 1
                  n_xref_end = .TRUE. ! rename
                  Do While( n_xref_end )
                     If( MET_TO_STAGE_LU(n_xref_lu)%Met_LU_Name .Eq. 'N/A' ) Then
                        n_xref_end = .FALSE.
!                        Write(logdev,*) 'MET_LU     :', MET_TO_STAGE_LU(n_xref_lu)%Met_LU_Name
!                        Write(logdev,*) 'Met Index  :', MET_TO_STAGE_LU(n_xref_lu)%Met_Index
!                        Write(logdev,*) 'Met Lu Name:', MET_TO_STAGE_LU(n_xref_lu)%Dep_LU_Name
!                        Write(logdev,*) 'Dep Index  :', MET_TO_STAGE_LU(n_xref_lu)%Dep_Index
!                        Write(logdev,*) 'Factor     :', MET_TO_STAGE_LU(n_xref_lu)%Factor
!                        Write(logdev,*) 'Description:', MET_TO_STAGE_LU(n_xref_lu)%Description
                        n_xref_lu = n_xref_lu - 1
                     Else
                        Write(logdev,*) 'MET_LU     :', MET_TO_STAGE_LU(n_xref_lu)%Met_LU_Name
                        Write(logdev,*) 'Met Index  :', MET_TO_STAGE_LU(n_xref_lu)%Met_Index
                        Write(logdev,*) 'Met Lu Name:', MET_TO_STAGE_LU(n_xref_lu)%Dep_LU_Name
                        Write(logdev,*) 'Dep Index  :', MET_TO_STAGE_LU(n_xref_lu)%Dep_Index
                        Write(logdev,*) 'Factor     :', MET_TO_STAGE_LU(n_xref_lu)%Factor
                        Write(logdev,*) 'Description:', MET_TO_STAGE_LU(n_xref_lu)%Description
                        n_xref_lu = n_xref_lu + 1
                     End If
                  End Do

               CASE( 'NLCD50' )
               CASE( 'USGS24' )      
               CASE DEFAULT      
            END SELECT                                                                                       
            CLOSE( UNIT = EMCTRL_NML )  
            n_lufrac = maxval(MET_TO_STAGE_LU%Met_Index)
         END SUBROUTINE STAGE_LU_MAPPER
                                               
      END MODULE LSM_Mod                       
