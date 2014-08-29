
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE HRRATES

C***********************************************************************
C
C  FUNCTION: To calculate rates of reactions
C
C  PRECONDITIONS: For the SAPRC99 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, July 18, 2014
C
C***********************************************************************
      USE HRDATA

      IMPLICIT NONE

C.....INCLUDES: NONE


C.....ARGUMENTS: NONE


C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS: NONE


C.....SAVED VARIABLES: NONE
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRRATES'   ! Program name


C.....LOCAL VARIABLES: None

C***********************************************************************
C..NOTE: RXRAT commented out are not referenced by any routine
!     RXRAT( NCELL,   1 ) = RKI( NCELL,   1 ) * YC( NCELL, NO2 )
!     RXRAT( NCELL,   2 ) = RKI( NCELL,   2 ) * YC( NCELL, O3P )
!     RXRAT( NCELL,   3 ) = RKI( NCELL,   3 ) * YC( NCELL, O3P ) * YC( NCELL, O3 )
!     RXRAT( NCELL,   4 ) = RKI( NCELL,   4 ) * YC( NCELL, O3P ) * YC( NCELL, NO )
!     RXRAT( NCELL,   5 ) = RKI( NCELL,   5 ) * YC( NCELL, O3P ) * YC( NCELL, NO2 )
      RXRAT( NCELL,   6 ) = RKI( NCELL,   6 ) * YC( NCELL, O3P ) * YC( NCELL, NO2 )
!     RXRAT( NCELL,   7 ) = RKI( NCELL,   7 ) * YC( NCELL, O3 ) * YC( NCELL, NO )
      RXRAT( NCELL,   8 ) = RKI( NCELL,   8 ) * YC( NCELL, O3 ) * YC( NCELL, NO2 )
      RXRAT( NCELL,   9 ) = RKI( NCELL,   9 ) * YC( NCELL, NO ) * YC( NCELL, NO3 )
!     RXRAT( NCELL,  10 ) = RKI( NCELL,  10 ) * YC( NCELL, NO ) * YC( NCELL, NO )
!     RXRAT( NCELL,  11 ) = RKI( NCELL,  11 ) * YC( NCELL, NO2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  12 ) = RKI( NCELL,  12 ) * YC( NCELL, N2O5 )
      RXRAT( NCELL,  13 ) = RKI( NCELL,  13 ) * YC( NCELL, N2O5 )
      RXRAT( NCELL,  14 ) = RKI( NCELL,  14 ) * YC( NCELL, NO2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  15 ) = RKI( NCELL,  15 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  16 ) = RKI( NCELL,  16 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  17 ) = RKI( NCELL,  17 ) * YC( NCELL, O3 )
      RXRAT( NCELL,  18 ) = RKI( NCELL,  18 ) * YC( NCELL, O3 )
!     RXRAT( NCELL,  19 ) = RKI( NCELL,  19 ) * YC( NCELL, O1D2 )
!     RXRAT( NCELL,  20 ) = RKI( NCELL,  20 ) * YC( NCELL, O1D2 )
!     RXRAT( NCELL,  21 ) = RKI( NCELL,  21 ) * YC( NCELL, HO ) * YC( NCELL, NO )
      RXRAT( NCELL,  22 ) = RKI( NCELL,  22 ) * YC( NCELL, HONO )
      RXRAT( NCELL,  23 ) = RKI( NCELL,  23 ) * YC( NCELL, HONO )
      RXRAT( NCELL,  24 ) = RKI( NCELL,  24 ) * YC( NCELL, HO ) * YC( NCELL, HONO )
      RXRAT( NCELL,  25 ) = RKI( NCELL,  25 ) * YC( NCELL, HO ) * YC( NCELL, NO2 )
      RXRAT( NCELL,  26 ) = RKI( NCELL,  26 ) * YC( NCELL, HO ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  27 ) = RKI( NCELL,  27 ) * YC( NCELL, HO ) * YC( NCELL, HNO3 )
      RXRAT( NCELL,  28 ) = RKI( NCELL,  28 ) * YC( NCELL, HNO3 )
      RXRAT( NCELL,  29 ) = RKI( NCELL,  29 ) * YC( NCELL, HO ) * YC( NCELL, CO )
!     RXRAT( NCELL,  30 ) = RKI( NCELL,  30 ) * YC( NCELL, HO ) * YC( NCELL, O3 )
!     RXRAT( NCELL,  31 ) = RKI( NCELL,  31 ) * YC( NCELL, HO2 ) * YC( NCELL, NO )
!     RXRAT( NCELL,  32 ) = RKI( NCELL,  32 ) * YC( NCELL, HO2 ) * YC( NCELL, NO2 )
      RXRAT( NCELL,  33 ) = RKI( NCELL,  33 ) * YC( NCELL, HNO4 )
      RXRAT( NCELL,  34 ) = RKI( NCELL,  34 ) * YC( NCELL, HNO4 )
      RXRAT( NCELL,  35 ) = RKI( NCELL,  35 ) * YC( NCELL, HNO4 ) * YC( NCELL, HO )
!     RXRAT( NCELL,  36 ) = RKI( NCELL,  36 ) * YC( NCELL, HO2 ) * YC( NCELL, O3 )
      RXRAT( NCELL,  37 ) = RKI( NCELL,  37 ) * YC( NCELL, HO2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  38 ) = RKI( NCELL,  38 ) * YC( NCELL, HO2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  39 ) = RKI( NCELL,  39 ) * YC( NCELL, NO3 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  40 ) = RKI( NCELL,  40 ) * YC( NCELL, NO3 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  41 ) = RKI( NCELL,  41 ) * YC( NCELL, HO2H )
      RXRAT( NCELL,  42 ) = RKI( NCELL,  42 ) * YC( NCELL, HO2H ) * YC( NCELL, HO )
!     RXRAT( NCELL,  43 ) = RKI( NCELL,  43 ) * YC( NCELL, HO ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  44 ) = RKI( NCELL,  44 ) * YC( NCELL, HO ) * YC( NCELL, SO2 )
!     RXRAT( NCELL,  45 ) = RKI( NCELL,  45 ) * YC( NCELL, HO )
      RXRAT( NCELL,  46 ) = RKI( NCELL,  46 ) * YC( NCELL, C_O2 ) * YC( NCELL, NO )
      RXRAT( NCELL,  47 ) = RKI( NCELL,  47 ) * YC( NCELL, C_O2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  48 ) = RKI( NCELL,  48 ) * YC( NCELL, C_O2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  49 ) = RKI( NCELL,  49 ) * YC( NCELL, C_O2 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  50 ) = RKI( NCELL,  50 ) * YC( NCELL, C_O2 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  51 ) = RKI( NCELL,  51 ) * YC( NCELL, RO2_R ) * YC( NCELL, NO )
      RXRAT( NCELL,  52 ) = RKI( NCELL,  52 ) * YC( NCELL, RO2_R ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  53 ) = RKI( NCELL,  53 ) * YC( NCELL, RO2_R ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  54 ) = RKI( NCELL,  54 ) * YC( NCELL, RO2_R ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  55 ) = RKI( NCELL,  55 ) * YC( NCELL, RO2_R ) * YC( NCELL, RO2_R )
      RXRAT( NCELL,  56 ) = RKI( NCELL,  56 ) * YC( NCELL, R2O2 ) * YC( NCELL, NO )
      RXRAT( NCELL,  57 ) = RKI( NCELL,  57 ) * YC( NCELL, R2O2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  58 ) = RKI( NCELL,  58 ) * YC( NCELL, R2O2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  59 ) = RKI( NCELL,  59 ) * YC( NCELL, R2O2 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  60 ) = RKI( NCELL,  60 ) * YC( NCELL, R2O2 ) * YC( NCELL, RO2_R )
      RXRAT( NCELL,  61 ) = RKI( NCELL,  61 ) * YC( NCELL, R2O2 ) * YC( NCELL, R2O2 )
      RXRAT( NCELL,  62 ) = RKI( NCELL,  62 ) * YC( NCELL, RO2_N ) * YC( NCELL, NO )
      RXRAT( NCELL,  63 ) = RKI( NCELL,  63 ) * YC( NCELL, RO2_N ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  64 ) = RKI( NCELL,  64 ) * YC( NCELL, RO2_N ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  65 ) = RKI( NCELL,  65 ) * YC( NCELL, RO2_N ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  66 ) = RKI( NCELL,  66 ) * YC( NCELL, RO2_N ) * YC( NCELL, RO2_R )
      RXRAT( NCELL,  67 ) = RKI( NCELL,  67 ) * YC( NCELL, RO2_N ) * YC( NCELL, R2O2 )
      RXRAT( NCELL,  68 ) = RKI( NCELL,  68 ) * YC( NCELL, RO2_N ) * YC( NCELL, RO2_N )
!     RXRAT( NCELL,  69 ) = RKI( NCELL,  69 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, NO2 )
      RXRAT( NCELL,  70 ) = RKI( NCELL,  70 ) * YC( NCELL, PAN )
      RXRAT( NCELL,  71 ) = RKI( NCELL,  71 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, NO )
      RXRAT( NCELL,  72 ) = RKI( NCELL,  72 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  73 ) = RKI( NCELL,  73 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  74 ) = RKI( NCELL,  74 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  75 ) = RKI( NCELL,  75 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, RO2_R )
      RXRAT( NCELL,  76 ) = RKI( NCELL,  76 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, R2O2 )
      RXRAT( NCELL,  77 ) = RKI( NCELL,  77 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, RO2_N )
      RXRAT( NCELL,  78 ) = RKI( NCELL,  78 ) * YC( NCELL, CCO_O2 ) * YC( NCELL, CCO_O2 )
      RXRAT( NCELL,  79 ) = RKI( NCELL,  79 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, NO2 )
      RXRAT( NCELL,  80 ) = RKI( NCELL,  80 ) * YC( NCELL, PAN2 )
      RXRAT( NCELL,  81 ) = RKI( NCELL,  81 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, NO )
      RXRAT( NCELL,  82 ) = RKI( NCELL,  82 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  83 ) = RKI( NCELL,  83 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  84 ) = RKI( NCELL,  84 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  85 ) = RKI( NCELL,  85 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, RO2_R )
      RXRAT( NCELL,  86 ) = RKI( NCELL,  86 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, R2O2 )
      RXRAT( NCELL,  87 ) = RKI( NCELL,  87 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, RO2_N )
      RXRAT( NCELL,  88 ) = RKI( NCELL,  88 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, CCO_O2 )
      RXRAT( NCELL,  89 ) = RKI( NCELL,  89 ) * YC( NCELL, RCO_O2 ) * YC( NCELL, RCO_O2 )
      RXRAT( NCELL,  90 ) = RKI( NCELL,  90 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, NO2 )
      RXRAT( NCELL,  91 ) = RKI( NCELL,  91 ) * YC( NCELL, PBZN )
      RXRAT( NCELL,  92 ) = RKI( NCELL,  92 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, NO )
      RXRAT( NCELL,  93 ) = RKI( NCELL,  93 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL,  94 ) = RKI( NCELL,  94 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL,  95 ) = RKI( NCELL,  95 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL,  96 ) = RKI( NCELL,  96 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, RO2_R )
      RXRAT( NCELL,  97 ) = RKI( NCELL,  97 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, R2O2 )
      RXRAT( NCELL,  98 ) = RKI( NCELL,  98 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, RO2_N )
      RXRAT( NCELL,  99 ) = RKI( NCELL,  99 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, CCO_O2 )
      RXRAT( NCELL, 100 ) = RKI( NCELL, 100 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, RCO_O2 )
      RXRAT( NCELL, 101 ) = RKI( NCELL, 101 ) * YC( NCELL, BZCO_O2 ) * YC( NCELL, BZCO_O2 )
      RXRAT( NCELL, 102 ) = RKI( NCELL, 102 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, NO2 )
      RXRAT( NCELL, 103 ) = RKI( NCELL, 103 ) * YC( NCELL, MA_PAN )
      RXRAT( NCELL, 104 ) = RKI( NCELL, 104 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, NO )
      RXRAT( NCELL, 105 ) = RKI( NCELL, 105 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 106 ) = RKI( NCELL, 106 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 107 ) = RKI( NCELL, 107 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, C_O2 )
      RXRAT( NCELL, 108 ) = RKI( NCELL, 108 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, RO2_R )
      RXRAT( NCELL, 109 ) = RKI( NCELL, 109 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, R2O2 )
      RXRAT( NCELL, 110 ) = RKI( NCELL, 110 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, RO2_N )
      RXRAT( NCELL, 111 ) = RKI( NCELL, 111 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, CCO_O2 )
      RXRAT( NCELL, 112 ) = RKI( NCELL, 112 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, RCO_O2 )
      RXRAT( NCELL, 113 ) = RKI( NCELL, 113 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, BZCO_O2 )
      RXRAT( NCELL, 114 ) = RKI( NCELL, 114 ) * YC( NCELL, MA_RCO3 ) * YC( NCELL, MA_RCO3 )
      RXRAT( NCELL, 115 ) = RKI( NCELL, 115 ) * YC( NCELL, TBU_O ) * YC( NCELL, NO2 )
      RXRAT( NCELL, 116 ) = RKI( NCELL, 116 ) * YC( NCELL, TBU_O )
      RXRAT( NCELL, 117 ) = RKI( NCELL, 117 ) * YC( NCELL, BZ_O ) * YC( NCELL, NO2 )
      RXRAT( NCELL, 118 ) = RKI( NCELL, 118 ) * YC( NCELL, BZ_O ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 119 ) = RKI( NCELL, 119 ) * YC( NCELL, BZ_O )
      RXRAT( NCELL, 120 ) = RKI( NCELL, 120 ) * YC( NCELL, BZNO2_O ) * YC( NCELL, NO2 )
      RXRAT( NCELL, 121 ) = RKI( NCELL, 121 ) * YC( NCELL, BZNO2_O ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 122 ) = RKI( NCELL, 122 ) * YC( NCELL, BZNO2_O )
      RXRAT( NCELL, 123 ) = RKI( NCELL, 123 ) * YC( NCELL, HCHO )
      RXRAT( NCELL, 124 ) = RKI( NCELL, 124 ) * YC( NCELL, HCHO )
      RXRAT( NCELL, 125 ) = RKI( NCELL, 125 ) * YC( NCELL, HCHO ) * YC( NCELL, HO )
      RXRAT( NCELL, 126 ) = RKI( NCELL, 126 ) * YC( NCELL, HCHO ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 127 ) = RKI( NCELL, 127 ) * YC( NCELL, HOCOO )
      RXRAT( NCELL, 128 ) = RKI( NCELL, 128 ) * YC( NCELL, HOCOO ) * YC( NCELL, NO )
      RXRAT( NCELL, 129 ) = RKI( NCELL, 129 ) * YC( NCELL, HCHO ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 130 ) = RKI( NCELL, 130 ) * YC( NCELL, CCHO ) * YC( NCELL, HO )
      RXRAT( NCELL, 131 ) = RKI( NCELL, 131 ) * YC( NCELL, CCHO )
      RXRAT( NCELL, 132 ) = RKI( NCELL, 132 ) * YC( NCELL, CCHO ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 133 ) = RKI( NCELL, 133 ) * YC( NCELL, RCHO ) * YC( NCELL, HO )
      RXRAT( NCELL, 134 ) = RKI( NCELL, 134 ) * YC( NCELL, RCHO )
      RXRAT( NCELL, 135 ) = RKI( NCELL, 135 ) * YC( NCELL, RCHO ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 136 ) = RKI( NCELL, 136 ) * YC( NCELL, ACET ) * YC( NCELL, HO )
      RXRAT( NCELL, 137 ) = RKI( NCELL, 137 ) * YC( NCELL, ACET )
      RXRAT( NCELL, 138 ) = RKI( NCELL, 138 ) * YC( NCELL, MEK ) * YC( NCELL, HO )
      RXRAT( NCELL, 139 ) = RKI( NCELL, 139 ) * YC( NCELL, MEK )
      RXRAT( NCELL, 140 ) = RKI( NCELL, 140 ) * YC( NCELL, MEOH ) * YC( NCELL, HO )
      RXRAT( NCELL, 141 ) = RKI( NCELL, 141 ) * YC( NCELL, COOH ) * YC( NCELL, HO )
      RXRAT( NCELL, 142 ) = RKI( NCELL, 142 ) * YC( NCELL, COOH )
      RXRAT( NCELL, 143 ) = RKI( NCELL, 143 ) * YC( NCELL, ROOH ) * YC( NCELL, HO )
      RXRAT( NCELL, 144 ) = RKI( NCELL, 144 ) * YC( NCELL, ROOH )
      RXRAT( NCELL, 145 ) = RKI( NCELL, 145 ) * YC( NCELL, GLY )
      RXRAT( NCELL, 146 ) = RKI( NCELL, 146 ) * YC( NCELL, GLY )
      RXRAT( NCELL, 147 ) = RKI( NCELL, 147 ) * YC( NCELL, GLY ) * YC( NCELL, HO )
      RXRAT( NCELL, 148 ) = RKI( NCELL, 148 ) * YC( NCELL, GLY ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 149 ) = RKI( NCELL, 149 ) * YC( NCELL, MGLY )
      RXRAT( NCELL, 150 ) = RKI( NCELL, 150 ) * YC( NCELL, MGLY ) * YC( NCELL, HO )
      RXRAT( NCELL, 151 ) = RKI( NCELL, 151 ) * YC( NCELL, MGLY ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 152 ) = RKI( NCELL, 152 ) * YC( NCELL, BACL )
      RXRAT( NCELL, 153 ) = RKI( NCELL, 153 ) * YC( NCELL, PHEN ) * YC( NCELL, HO )
      RXRAT( NCELL, 154 ) = RKI( NCELL, 154 ) * YC( NCELL, PHEN ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 155 ) = RKI( NCELL, 155 ) * YC( NCELL, CRES ) * YC( NCELL, HO )
      RXRAT( NCELL, 156 ) = RKI( NCELL, 156 ) * YC( NCELL, CRES ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 157 ) = RKI( NCELL, 157 ) * YC( NCELL, NPHE ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 158 ) = RKI( NCELL, 158 ) * YC( NCELL, BALD ) * YC( NCELL, HO )
      RXRAT( NCELL, 159 ) = RKI( NCELL, 159 ) * YC( NCELL, BALD )
      RXRAT( NCELL, 160 ) = RKI( NCELL, 160 ) * YC( NCELL, BALD ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 161 ) = RKI( NCELL, 161 ) * YC( NCELL, METHACRO ) * YC( NCELL, HO )
      RXRAT( NCELL, 162 ) = RKI( NCELL, 162 ) * YC( NCELL, METHACRO ) * YC( NCELL, O3 )
      RXRAT( NCELL, 163 ) = RKI( NCELL, 163 ) * YC( NCELL, METHACRO ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 164 ) = RKI( NCELL, 164 ) * YC( NCELL, METHACRO ) * YC( NCELL, O3P )
      RXRAT( NCELL, 165 ) = RKI( NCELL, 165 ) * YC( NCELL, METHACRO )
      RXRAT( NCELL, 166 ) = RKI( NCELL, 166 ) * YC( NCELL, MVK ) * YC( NCELL, HO )
      RXRAT( NCELL, 167 ) = RKI( NCELL, 167 ) * YC( NCELL, MVK ) * YC( NCELL, O3 )
      RXRAT( NCELL, 168 ) = RKI( NCELL, 168 ) * YC( NCELL, MVK ) * YC( NCELL, O3P )
      RXRAT( NCELL, 169 ) = RKI( NCELL, 169 ) * YC( NCELL, MVK )
      RXRAT( NCELL, 170 ) = RKI( NCELL, 170 ) * YC( NCELL, ISOPROD ) * YC( NCELL, HO )
      RXRAT( NCELL, 171 ) = RKI( NCELL, 171 ) * YC( NCELL, ISOPROD ) * YC( NCELL, O3 )
      RXRAT( NCELL, 172 ) = RKI( NCELL, 172 ) * YC( NCELL, ISOPROD ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 173 ) = RKI( NCELL, 173 ) * YC( NCELL, ISOPROD )
      RXRAT( NCELL, 174 ) = RKI( NCELL, 174 ) * YC( NCELL, PROD2 ) * YC( NCELL, HO )
      RXRAT( NCELL, 175 ) = RKI( NCELL, 175 ) * YC( NCELL, PROD2 )
      RXRAT( NCELL, 176 ) = RKI( NCELL, 176 ) * YC( NCELL, RNO3 ) * YC( NCELL, HO )
      RXRAT( NCELL, 177 ) = RKI( NCELL, 177 ) * YC( NCELL, RNO3 )
      RXRAT( NCELL, 178 ) = RKI( NCELL, 178 ) * YC( NCELL, DCB1 ) * YC( NCELL, HO )
      RXRAT( NCELL, 179 ) = RKI( NCELL, 179 ) * YC( NCELL, DCB1 ) * YC( NCELL, O3 )
      RXRAT( NCELL, 180 ) = RKI( NCELL, 180 ) * YC( NCELL, DCB2 ) * YC( NCELL, HO )
      RXRAT( NCELL, 181 ) = RKI( NCELL, 181 ) * YC( NCELL, DCB2 )
      RXRAT( NCELL, 182 ) = RKI( NCELL, 182 ) * YC( NCELL, DCB3 ) * YC( NCELL, HO )
      RXRAT( NCELL, 183 ) = RKI( NCELL, 183 ) * YC( NCELL, DCB3 )
      RXRAT( NCELL, 184 ) = RKI( NCELL, 184 ) * YC( NCELL, HO )
      RXRAT( NCELL, 185 ) = RKI( NCELL, 185 ) * YC( NCELL, ETHENE ) * YC( NCELL, HO )
      RXRAT( NCELL, 186 ) = RKI( NCELL, 186 ) * YC( NCELL, ETHENE ) * YC( NCELL, O3 )
      RXRAT( NCELL, 187 ) = RKI( NCELL, 187 ) * YC( NCELL, ETHENE ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 188 ) = RKI( NCELL, 188 ) * YC( NCELL, ETHENE ) * YC( NCELL, O3P )
      RXRAT( NCELL, 189 ) = RKI( NCELL, 189 ) * YC( NCELL, ISOPRENE ) * YC( NCELL, HO )
      RXRAT( NCELL, 190 ) = RKI( NCELL, 190 ) * YC( NCELL, ISOPRENE ) * YC( NCELL, O3 )
      RXRAT( NCELL, 191 ) = RKI( NCELL, 191 ) * YC( NCELL, ISOPRENE ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 192 ) = RKI( NCELL, 192 ) * YC( NCELL, ISOPRENE ) * YC( NCELL, O3P )
      RXRAT( NCELL, 193 ) = RKI( NCELL, 193 ) * YC( NCELL, TRP1 ) * YC( NCELL, HO )
      RXRAT( NCELL, 194 ) = RKI( NCELL, 194 ) * YC( NCELL, TRP1 ) * YC( NCELL, O3 )
      RXRAT( NCELL, 195 ) = RKI( NCELL, 195 ) * YC( NCELL, TRP1 ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 196 ) = RKI( NCELL, 196 ) * YC( NCELL, TRP1 ) * YC( NCELL, O3P )
      RXRAT( NCELL, 197 ) = RKI( NCELL, 197 ) * YC( NCELL, ALK1 ) * YC( NCELL, HO )
      RXRAT( NCELL, 198 ) = RKI( NCELL, 198 ) * YC( NCELL, ALK2 ) * YC( NCELL, HO )
      RXRAT( NCELL, 199 ) = RKI( NCELL, 199 ) * YC( NCELL, ALK3 ) * YC( NCELL, HO )
      RXRAT( NCELL, 200 ) = RKI( NCELL, 200 ) * YC( NCELL, ALK4 ) * YC( NCELL, HO )
      RXRAT( NCELL, 201 ) = RKI( NCELL, 201 ) * YC( NCELL, ALK5 ) * YC( NCELL, HO )
      RXRAT( NCELL, 202 ) = RKI( NCELL, 202 ) * YC( NCELL, ARO1 ) * YC( NCELL, HO )
      RXRAT( NCELL, 203 ) = RKI( NCELL, 203 ) * YC( NCELL, ARO1RO2 ) * YC( NCELL, NO )
      RXRAT( NCELL, 204 ) = RKI( NCELL, 204 ) * YC( NCELL, ARO1RO2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 205 ) = RKI( NCELL, 205 ) * YC( NCELL, ARO2 ) * YC( NCELL, HO )
      RXRAT( NCELL, 206 ) = RKI( NCELL, 206 ) * YC( NCELL, ARO2RO2 ) * YC( NCELL, NO )
      RXRAT( NCELL, 207 ) = RKI( NCELL, 207 ) * YC( NCELL, ARO2RO2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 208 ) = RKI( NCELL, 208 ) * YC( NCELL, BENZENE ) * YC( NCELL, HO )
      RXRAT( NCELL, 209 ) = RKI( NCELL, 209 ) * YC( NCELL, BENZRO2 ) * YC( NCELL, NO )
      RXRAT( NCELL, 210 ) = RKI( NCELL, 210 ) * YC( NCELL, BENZRO2 ) * YC( NCELL, HO2 )
      RXRAT( NCELL, 211 ) = RKI( NCELL, 211 ) * YC( NCELL, OLE1 ) * YC( NCELL, HO )
      RXRAT( NCELL, 212 ) = RKI( NCELL, 212 ) * YC( NCELL, OLE1 ) * YC( NCELL, O3 )
      RXRAT( NCELL, 213 ) = RKI( NCELL, 213 ) * YC( NCELL, OLE1 ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 214 ) = RKI( NCELL, 214 ) * YC( NCELL, OLE1 ) * YC( NCELL, O3P )
      RXRAT( NCELL, 215 ) = RKI( NCELL, 215 ) * YC( NCELL, OLE2 ) * YC( NCELL, HO )
      RXRAT( NCELL, 216 ) = RKI( NCELL, 216 ) * YC( NCELL, OLE2 ) * YC( NCELL, O3 )
      RXRAT( NCELL, 217 ) = RKI( NCELL, 217 ) * YC( NCELL, OLE2 ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 218 ) = RKI( NCELL, 218 ) * YC( NCELL, OLE2 ) * YC( NCELL, O3P )
      RXRAT( NCELL, 219 ) = RKI( NCELL, 219 ) * YC( NCELL, HCOOH ) * YC( NCELL, HO )
      RXRAT( NCELL, 220 ) = RKI( NCELL, 220 ) * YC( NCELL, CCO_OH ) * YC( NCELL, HO )
      RXRAT( NCELL, 221 ) = RKI( NCELL, 221 ) * YC( NCELL, RCO_OH ) * YC( NCELL, HO )
      RXRAT( NCELL, 222 ) = RKI( NCELL, 222 ) * YC( NCELL, SESQ ) * YC( NCELL, O3 )
      RXRAT( NCELL, 223 ) = RKI( NCELL, 223 ) * YC( NCELL, SESQ ) * YC( NCELL, HO )
      RXRAT( NCELL, 224 ) = RKI( NCELL, 224 ) * YC( NCELL, SESQ ) * YC( NCELL, NO3 )
      RXRAT( NCELL, 225 ) = RKI( NCELL, 225 ) * YC( NCELL, N2O5 )
      RXRAT( NCELL, 226 ) = RKI( NCELL, 226 ) * YC( NCELL, NO2 )
      RXRAT( NCELL, 227 ) = RKI( NCELL, 227 ) * YC( NCELL, AALKJ )
      RXRAT( NCELL, 228 ) = RKI( NCELL, 228 ) * YC( NCELL, AXYL1J )
      RXRAT( NCELL, 229 ) = RKI( NCELL, 229 ) * YC( NCELL, AXYL2J )
      RXRAT( NCELL, 230 ) = RKI( NCELL, 230 ) * YC( NCELL, ATOL1J )
      RXRAT( NCELL, 231 ) = RKI( NCELL, 231 ) * YC( NCELL, ATOL2J )
      RXRAT( NCELL, 232 ) = RKI( NCELL, 232 ) * YC( NCELL, ABNZ1J )
      RXRAT( NCELL, 233 ) = RKI( NCELL, 233 ) * YC( NCELL, ABNZ2J )
      RXRAT( NCELL, 234 ) = RKI( NCELL, 234 ) * YC( NCELL, ATRP1J )
      RXRAT( NCELL, 235 ) = RKI( NCELL, 235 ) * YC( NCELL, ATRP2J )
      RXRAT( NCELL, 236 ) = RKI( NCELL, 236 ) * YC( NCELL, AISO1J )
      RXRAT( NCELL, 237 ) = RKI( NCELL, 237 ) * YC( NCELL, AISO2J )
      RXRAT( NCELL, 238 ) = RKI( NCELL, 238 ) * YC( NCELL, ASQTJ )


      RETURN

      END
