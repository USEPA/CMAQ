
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

      MODULE DEGRADE_PARAMETERS
C**********************************************************************
C
C  FUNCTION:  Define arrays that identify species within CGRID used
C             based on input arrays
C
C  REVISION HISTORY: 07/29/05 : B.Hutzell - Initial version
C                    06 May 11: B.Hutzell: convert for Namelist redesign
C                    09 May 11: B.Hutzell: enabled a degraded species to
C                               be missing from namelists
C
C**********************************************************************

      IMPLICIT NONE

C.....INCLUDES:


C..declare and define variables used by maps and data

      INTEGER, PARAMETER :: N_REACT       = 34   ! number of  species being degraded
      INTEGER, PARAMETER :: N_UNI_LOSS    = 1     ! Number of Unimolecular Loss Processes
      INTEGER, PARAMETER :: N_BI_LOSS     = 5     ! Number of Bimolcular Loss processes
      INTEGER, PARAMETER :: N_TRI_LOSS    = 2     ! Number of Bimolcular Loss processes
      INTEGER, PARAMETER :: N_PHOTO_LOSS  = 2     ! Number of Photolysis Loss processes
      INTEGER, PARAMETER :: N_LANHIN_LOSS = 1     ! Number of Langmuir-Hinshelwood Loss Processes

! Total number of Loss processes      
      INTEGER, PARAMETER :: N_PROCESSES  = N_UNI_LOSS
     &                                   + N_BI_LOSS
     &                                   + N_PHOTO_LOSS
     &                                   + N_TRI_LOSS
     &                                   + N_LANHIN_LOSS

      CHARACTER(16), PARAMETER :: BLANK = ' '  ! default value for characters

      INTEGER, SAVE :: N_PHOTO_TAB    ! Number of photolysis rates in mechanism
      INTEGER, SAVE :: N_REACT_FOUND  ! Number ofreact species found in namelists

C..looping data

      INTEGER :: UNI_START
      INTEGER :: BI_START
      INTEGER :: TRI_START
      INTEGER :: PHOTO_START
      INTEGER :: LANHIN_START

      INTEGER :: UNI_STOP
      INTEGER :: BI_STOP
      INTEGER :: TRI_STOP
      INTEGER :: PHOTO_STOP
      INTEGER :: LANHIN_STOP

      CHARACTER(16), ALLOCATABLE :: REACT( : )         ! names of species being degraded
      CHARACTER(16), ALLOCATABLE :: BICAUSE( :,: )     ! species name that cause degradation
      CHARACTER(16), ALLOCATABLE :: LHCAUSE( :,: )     ! species name that cause degradation
      CHARACTER(16), ALLOCATABLE :: TRICAUSE( :,:,: )  ! species name that cause degradation
      CHARACTER(16), ALLOCATABLE :: PHOTO_NAME( :,: )  ! name of photolysis rate for react(i)

      CHARACTER(16), ALLOCATABLE :: BI_PROD   ( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: TRI_PROD  ( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: PHOTO_PROD( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: UNI_PROD  ( :,: )  ! name of daughter product for react(i)
      CHARACTER(16), ALLOCATABLE :: LH_PROD  ( :,: )   ! name of daughter product for react(i)

      REAL(8), ALLOCATABLE :: UNIRATE  ( :,: ) ! rate for unimolecular decay for react(i) [molecules/sec^1]
      REAL(8), ALLOCATABLE :: UNI_ACT  ( :,: ) ! activation energy for UNIRATE(I) [K]. Positive if exothermic
      REAL(8), ALLOCATABLE :: UNI_TEXP ( :,: ) ! exponent of Temperature
      REAL,    ALLOCATABLE :: UNI_YIELD( :,: ) ! production yield

      REAL(8), ALLOCATABLE :: BIRATE  ( :,: )  ! degradation rates for bimolecular reactions,  [cm^3/(sec*molecules)]
      REAL(8), ALLOCATABLE :: BI_ACT  ( :,: )  ! activation energy for BIRATE(I) [K]. Positive if exothermic
      REAL(8), ALLOCATABLE :: BI_TEXP ( :,: )  ! exponent of Temperature
      REAL,    ALLOCATABLE :: BI_YIELD( :,: )  ! production yield

      REAL(8), ALLOCATABLE :: TRIRATE  ( :,: ) ! degradation rates for trimolecular reactions,  [cm^3/(sec*molecules)]
      REAL(8), ALLOCATABLE :: TRI_ACT  ( :,: ) ! activation energy for TRIRATE(I) [K]. Positive if exothermic
      REAL(8), ALLOCATABLE :: TRI_TEXP ( :,: ) ! exponent of Temperature
      REAL,    ALLOCATABLE :: TRI_YIELD( :,: ) ! production yield

      REAL(8), ALLOCATABLE :: A_PHOTO    ( :,: ) ! multiplier of photolysis rates
      REAL,    ALLOCATABLE :: PHOTO_YIELD( :,: ) ! production yield

! Heterogeneous loss of aerosol species with Relative Humidity and Temperature cut-off. It uses an
! Langmuir-Hinshelwood isotherm to calculate the rate 
      REAL(8), ALLOCATABLE :: LHRATE  ( :,: )  ! degradation rates for Langmuir-Hinshelwood reactions,  [1/sec]
      REAL(8), ALLOCATABLE :: LH_EQU  ( :,: )  ! reactant adsorption and diffusion equilibruim constant, [cm^3/molecules]
      REAL,    ALLOCATABLE :: LH_YIELD( :,: )  ! production yield
      REAL,    ALLOCATABLE :: LH_RHMIN( :,: )  ! Minimum Relative Humidity for nonzero rate [fractional]
      REAL,    ALLOCATABLE :: LH_TAMIN( :,: )  ! Minimum Air temperature for nonzero rate   [K]
      
      INTEGER :: NDEGRADED_FOUND = 0
      CHARACTER( 16 ), ALLOCATABLE :: DEGRADED_SPECIES( : )

C**********************************************************************
      CONTAINS
      
         SUBROUTINE DEGRADE_DATA()
C**********************************************************************
C
C  FUNCTION:  Allocate arrays and define data used by degradation routines
C
C    react(i) does not need the maximum number of causes for degradation.
C    Set "un-used" causes to "' '".
C
C    Note, OH and HO both refer to hydroxyl concentrations. The redundancy exists because
C    current mechanisms for gas chemistry use OH or HO to refer to the radical.
C
C  CALLED BY: INIT_DEGRADE
C
C  REVISION HISTORY: : 07/29/05 : B.Hutzell - Initial version
C                      10/23/07 : B.Hutzell - added CL reactions for specific HAPs
C                      04/14/08 : B.Hutzell - Removed Benzene for AERO5 module
C                      08/31/11 : B.Hutzell - Removed removed capabilities with previous 
C                                             HAPs mechanisms
C**********************************************************************

            USE CGRID_SPCS           ! CGRID species number and offsets
            
            IMPLICIT NONE
            
            LOGICAL, SAVE :: INITIALIZED = .FALSE.

            INTEGER :: MARKER, N, M       ! indexes
            INTEGER :: I, J, K, L         ! loop counters
            
            INTEGER, ALLOCATABLE :: INDEX_FOUND( : )

C**********************************************************************

            IF( INITIALIZED )RETURN
      
            INITIALIZED = .TRUE.

C..looping data

            UNI_START    = 1
            BI_START     = UNI_START + N_UNI_LOSS
            TRI_START    = BI_START  + N_BI_LOSS
            PHOTO_START  = TRI_START + N_TRI_LOSS
            LANHIN_START = PHOTO_START + N_PHOTO_LOSS
            
            UNI_STOP    = BI_START     - 1
            BI_STOP     = TRI_START    - 1
            TRI_STOP    = PHOTO_START  - 1
            PHOTO_STOP  = LANHIN_START - 1
            LANHIN_STOP = N_PROCESSES
            
            ALLOCATE( REACT     ( N_REACT ) )                 ! names of species being degraded
            ALLOCATE( BICAUSE   ( N_BI_LOSS, N_REACT ) )      ! species name that cause degradation
            ALLOCATE( TRICAUSE  ( 2, N_TRI_LOSS, N_REACT ) )  ! species name that cause degradation
            ALLOCATE( PHOTO_NAME( N_PHOTO_LOSS, N_REACT ) )   ! name of photolysis rate for react(i)
            ALLOCATE( LHCAUSE   ( N_LANHIN_LOSS, N_REACT ) )  ! species name that cause degradation
            
            ALLOCATE( UNIRATE  ( N_UNI_LOSS, N_REACT ) )  ! rate for unimolecular decay for react(i) [molecules/sec^1]
            ALLOCATE( UNI_ACT  ( N_UNI_LOSS, N_REACT ) )  ! activation energy for UNIRATE(I) [K]. Positive if exothermic
            ALLOCATE( UNI_TEXP ( N_UNI_LOSS, N_REACT ) )  ! exponent of Temperature and UNIRATE(I) factored by (1/298)**UNI_TEXP
            ALLOCATE( UNI_PROD ( N_UNI_LOSS, N_REACT ) )  ! name of daughter product for react(i)
            ALLOCATE( UNI_YIELD( N_UNI_LOSS, N_REACT ) )  ! yield of daughter product for react(i)
            
            ALLOCATE( BIRATE  ( N_BI_LOSS, N_REACT ) )  ! degradation rates for bimolecular reactions,  [cm^3/(sec*molecules)]
            ALLOCATE( BI_ACT  ( N_BI_LOSS, N_REACT ) )  ! activation energy for BIRATE(I) [K]. Positive if exothermic
            ALLOCATE( BI_TEXP ( N_BI_LOSS, N_REACT ) )  ! exponent of Temperature and BIRATE(I) factored by (1/298)**BI_TEXP
            ALLOCATE( BI_PROD ( N_BI_LOSS, N_REACT ) )  ! name of daughter product for react(i)
            ALLOCATE( BI_YIELD( N_BI_LOSS, N_REACT ) )  ! yield of daughter product for react(i)
            
            ALLOCATE( TRIRATE  ( N_TRI_LOSS, N_REACT ) )  ! degradation rates for trimolecular reactions,  [cm^3/(sec*molecules)]
            ALLOCATE( TRI_ACT  ( N_TRI_LOSS, N_REACT ) )  ! activation energy for TRIRATE(I) [K]. Positive if exothermic
            ALLOCATE( TRI_TEXP ( N_TRI_LOSS, N_REACT ) )  ! exponent of Temperature and TRIRATE(I) factored by (1/298)**TRI_TEXP
            ALLOCATE( TRI_PROD ( N_TRI_LOSS, N_REACT ) )  ! name of daughter product for react(i)
            ALLOCATE( TRI_YIELD( N_TRI_LOSS, N_REACT ) )  ! yield of daughter product for react(i)
            
            ALLOCATE( A_PHOTO    ( N_PHOTO_LOSS, N_REACT ) )  ! multiplier of photolysis rates
            ALLOCATE( PHOTO_PROD ( N_PHOTO_LOSS, N_REACT  ) )  ! name of daughter product for react(i)
            ALLOCATE( PHOTO_YIELD( N_PHOTO_LOSS, N_REACT  ) )  ! yield of daughter product for react(i)
            
            
            ALLOCATE( LHRATE  ( N_LANHIN_LOSS, N_REACT ) )  
            ALLOCATE( LH_EQU  ( N_LANHIN_LOSS, N_REACT ) )  
            ALLOCATE( LH_PROD ( N_LANHIN_LOSS, N_REACT ) )  
            ALLOCATE( LH_YIELD( N_LANHIN_LOSS, N_REACT ) )  
            ALLOCATE( LH_RHMIN( N_LANHIN_LOSS, N_REACT ) )
            ALLOCATE( LH_TAMIN( N_LANHIN_LOSS, N_REACT ) )
                        

            ALLOCATE( INDEX_FOUND( N_REACT ) )
            
            INDEX_FOUND = -1 
            REACT       = BLANK
            PHOTO_NAME  = BLANK
            BICAUSE     = BLANK
            TRICAUSE    = BLANK
            LHCAUSE     = BLANK
            
            UNI_ACT   = 0.0D0
            UNIRATE   = 0.0D0
            UNI_TEXP  = 0.0D0
            UNI_YIELD = 0.0
            
            BI_ACT   = 0.0D0
            BIRATE   = 0.0D0
            BI_TEXP  = 0.0D0
            BI_YIELD = 0.0
            
            TRI_ACT   = 0.0D0
            TRIRATE   = 0.0D0
            TRI_TEXP  = 0.0D0
            TRI_YIELD = 0.0
            
            A_PHOTO     = 0.0D0
            PHOTO_YIELD = 0.0
            
            LH_EQU   = 0.0D0
            LHRATE   = 0.0D0
            LH_YIELD = 0.0
            LH_RHMIN = 0.0
            LH_TAMIN = 0.0
            
! product are unused aspect and results not returned cgrid 
            UNI_PROD   = BLANK
            BI_PROD    = BLANK
            TRI_PROD   = BLANK
            PHOTO_PROD = BLANK
            LH_PROD    = BLANK


C..toxic VOC's

C.. ACRYLONITRILE

            REACT(1)       = 'ACRY_NITRILE'
            BICAUSE(1:5,1) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,1)  = (/ 4.8D-12, 4.8D-12, 1.9D-19, 6.3D-18, 5.2D-14/)
            BI_ACT(1:5,1)  = (/   0.0D0,   0.0D0, 0.0D0, 0.0D0, -1.4D+3/)

C.. CARBON TETRACHLORIDE

            REACT(2)       = 'CARBONTET'
            BICAUSE(1:5,2) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,2)  = (/ 1.0D-12, 1.0D-12, 0.0D0, 0.0D0, 9.55D-11/)
            BI_ACT(1:5,2)  = (/2300.0D0,2300.0D0, 0.0D0, 0.0D0, 7.80D+03/)

C.. PROPYLENE DICHLORIDE

            REACT(3)       = 'PROPYL_DICL'
            BICAUSE(1:5,3) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,3)  = (/ 2.2D-13, 2.2D-13, 0.0D0, 0.0D0, 1.1D-11/)
            BI_ACT(1:5,3)  = (/   0.0D0,   0.0D0, 0.0D0, 0.0D0, 0.0D0/)

C.. DICHLORIPROPENE

            REACT(4)       = 'DICL_PROPENE'
            BICAUSE(1:4,4) = (/'OH ','HO ','O3 ','NO3'/)
            BIRATE(1:4,4)  = (/1.04D-11, 1.04D-11, 4.10D-19, 0.0D0/)
            BI_ACT(1:4,4)  = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0/)

C.. 1,1,2,2TETRACHLOROETHANE

            REACT(5)       = 'CL4_ETHANE'
            BICAUSE(1:5,5) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,5)  = (/2.46D-13, 2.46D-13, 0.0D0, 0.0D0, 1.91D-13/)
            BI_ACT(1:5,5)  = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0, 0.00D0/)

C.. CHLOROFORM

            REACT(6)       = 'CHCL3'
            BICAUSE(1:5,6) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,6)  = (/6.30D-18, 6.30D-18, 0.0D0, 0.0D0, 4.9D-12/)
            BI_ACT(1:5,6)  = (/  504.D0,   504.D0, 0.0D0, 0.0D0, 1.24D+3/)
            BI_TEXP(1:2,6) = (/   2.0D0,    2.0D0/)

C.. 1,2DIBROMOETHANE

            REACT(7)       = 'BR2_C2_12'
            BICAUSE(1:4,7) = (/'OH ','HO ','O3 ','NO3'/)
            BIRATE(1:4,7)  = (/1.03D-17, 1.03D-17, 0.0D0, 0.0D0/)
            BI_ACT(1:4,7)  = (/  422.D0,   422.D0, 0.0D0, 0.0D0/)
            BI_TEXP(1:2,7) = (/   2.0D0,    2.0D0/)

C.. 1,2DICHLOROTHANE

            REACT(8)       = 'CL2_C2_12'
            BICAUSE(1:5,8) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,8)  = (/1.10D-17, 1.10D-17, 0.0D0, 0.0D0, 4.93D-11/)
            BI_ACT(1:5,8)  = (/  409.D0,   409.D0, 0.0D0, 0.0D0, 1.09D+3/)
            BI_TEXP(1:2,8) = (/   2.0D0,    2.0D0/)

C.. ETHYLENE OXIDE

            REACT(9)       = 'ETOX'
            BICAUSE(1:5,9) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,9)  = (/7.60D-14, 7.60D-14, 0.0D0, 0.0D0, 5.0D-12/)
            BI_ACT(1:5,9)  = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0, 0.0D0/)

C.. METHYLENE CHLORIDE

            REACT(10)       = 'CL2_ME'
            BICAUSE(1:5,10) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,10)  = (/8.54D-18, 8.54D-18, 0.0D0, 0.0D0, 2.81D-11/)
            BI_ACT(1:5,10)  = (/  500.D0,   500.D0, 0.0D0, 0.0D0, 1.3D+3/)
            BI_TEXP(1:2,10) = (/   2.0D0,    2.0D0/)

C.. PERCHLOROETHYLENE

            REACT(11)       = 'CL4_ETHE'
            BICAUSE(1:5,11) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,11)  = (/9.64D-12, 9.64D-12, 0.0D0, 0.0D0, 3.72D-8/)
            BI_ACT(1:5,11)  = (/ 1209.D0,  1209.D0, 0.0D0, 0.0D0, 0.0D0/)
            BI_TEXP(1:5,11) = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0,-1.2D0/)

C.. TRICHLOROETHYLENE

            REACT(12)       = 'CL3_ETHE'
            BICAUSE(1:5,12) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,12)  = (/5.63D-13, 5.63D-13, 0.0D0, 0.0D0, 8.07D-11/)
            BI_ACT(1:5,12)  = (/ -427.D0,  -427.D0, 0.0D0, 0.0D0, 0.0D0/)

C.. VINYL CHLORIDE

            REACT(13)       = 'CL_ETHE'
            BICAUSE(1:5,13) = (/'OH ','HO ','O3 ','NO3', 'CL '/)
            BIRATE(1:5,13)  = (/1.69D-12, 1.69D-12, 0.0D0,1.84D-13, 1.27D-10/)
            BI_ACT(1:5,13)  = (/ -422.D0,  -422.D0, 0.0D0, 1764.D0, 0.0D0/)

C.. NAPHTHALENE

            REACT(14)       = 'NAPHTHALENE'
            BICAUSE(1:3,14) = (/'OH ','HO ','O3 '/)
            BIRATE(1:3,14)  = (/1.05D-12, 1.05D-12, 2.01D-19/)
            BI_ACT(1:3,14)  = (/ -901.D0,  -901.D0,    0.0D0/)
            TRICAUSE(1:2,1,14) = (/'NO3 ','NO2 '/)
            TRIRATE( 1,14)     =  3.94D-34
            TRI_ACT( 1,14)     = -4.10D+03

C.. QUINOLINE

            REACT(15)       = 'QUINOLINE'
            BICAUSE(1:4,15) = (/'OH ','HO ','O3 ','NO2'/)
            BIRATE(1:4,15)  = (/1.16D-11, 1.16D-11, 1.00D-19, 1.60D-20/)

C.. HEXAMETHYLENE DIISOCAYNATE

            REACT(16)       = 'HEXMETH_DIS'
            BICAUSE(1:2,16) = (/'OH ','HO '/)
            BIRATE(1:2,16)  = (/7.94D-12, 7.94D-12/)

C.. HYDRAZINE

            REACT(17)       = 'HYDRAZINE'
            BICAUSE(1:3,17) = (/'OH ','HO ','O3 '/)
            BIRATE(1:3,17)  = (/3.60D-11, 3.60D-11, 3.00D-17/)

C.. MALEIC ANHYDRIDE

            REACT(18)       = 'MAL_ANHYDRID'
            BICAUSE(1:3,18) = (/'OH  ','HO  ','NO3 '/)
            BIRATE(1:3,18)  = (/1.45D-12, 1.45D-12, 1.02D-13/)

C.. TOULENE DIISOCYANATE 

            REACT(19)       = 'TOL_DIIS'
            BICAUSE(1:2,19) = (/'OH ','HO '/)
            BIRATE(1:2,19)  = (/7.41D-12, 7.41D-12/)

C.. TRIETHYLAMINE

            REACT(20)       = 'TRIETHYLAMIN'
            BICAUSE(1:5,20) = (/'OH  ','HO  ','O   ','O3P ', 'O3  '/)
            BIRATE(1:5,20)  = (/3.60D-11, 3.60D-11, 1.09D-11, 1.09D-11, 7.84D-18/)

C.. P-DICHLOROBENZENE 

            REACT(21)       = 'DICL_BENZENE'
            BICAUSE(1:2,21) = (/'OH ','HO '/)
            BIRATE(1:2,21)  = (/3.60D-13, 3.60D-13/)

C.. XYLENE

            REACT(22)       = 'XYLENE         '
            BICAUSE(1:3,22) = (/'OH','HO','CL'/)
            BIRATE(1:3,22)  = (/1.717D-11, 1.717D-11, 1.433D-10/)

C.. STYRENE

            REACT(23)       = 'STYRENE        '
            BICAUSE(1:4,23) = (/'OH','HO','O3','CL'/)
            BIRATE(1:4,23)  = (/1.02D-11, 1.02D-11, 1.51D-13, 3.6D-10/)
            BI_ACT(1:2,23)  = (/    -532.0D0,   -532.0D0/)

C.. ACETONITRILE

            REACT(24)       = 'ACET_NITRILE   '
            BICAUSE(1:3,24) = (/'OH','HO','CL'/)
            BIRATE(1:3,24)  = (/          8.31D-13,          8.31D-13,          1.60D-11/)
            BI_ACT(1:3,24)  = (/          1080.0D0,          1080.0D0,          2140.0D0/)

C.. ACRYACID

            REACT(25)       = 'ACRYACID       '
            BICAUSE(1:4,25) = (/'OH','HO','O3','CL'/)
            BIRATE(1:4,25)  = (/1.75D-11, 1.75D-11, 6.50D-19, 4.00D-10/)

C.. HEXANE 
 
            REACT(26)       = 'HEXANE         '
            BICAUSE(1:3,26) = (/'OH','HO','CL'/)
            BIRATE(1:3,26)  = (/          1.98D-11,          1.91D-11,          3.30D-10/)
            BI_ACT(1:2,26)  = (/           394.0D0,           394.0D0/)

C.. METHCHLORIDE
 
            REACT(27)       = 'METHCHLORIDE   '
            BICAUSE(1:3,27) = (/'OH','HO','CL'/)
            BIRATE(1:3,27)  = (/          2.10D-12,          2.10D-12,          1.80D-11/)
            BI_ACT(1:3,27)  = (/          1210.0D0,          1210.0D0,            1081D0/)

C.. CARBSULFIDE

            REACT(28)       = 'CARBSULFIDE    '
            BICAUSE(1:3,28) = (/'OH','HO','O3'/)
            BIRATE(1:3,28)  = (/          1.10D-13,          1.10D-13,           3.01D-16/)
            BI_ACT(1:2,28)  = (/          1200.0D0,          1200.0D0/)

C.. CHLOROPRENE

           REACT(29)       = 'CHLOROPRENE    '
           BICAUSE(1:5,29) = (/'OH ','HO ','O3 ','NO3','CL '/)
           BIRATE(1:5,29)  = (/          6.20D-11,          6.20D-11,          1.40D-16,
     &                                   3.60D-13,          3.62D-10/)

C.. ETHYLBENZENE

           REACT(30)       = 'ETHYLBENZENE   '
           BICAUSE(1:4,30) = (/'OH ','HO ','NO3','CL '/)
           BIRATE(1:4,30)  = (/          7.00D-12,          7.00D-12,          5.70D-16,
     &                                   1.15D-10/)

C.. BENZO[A]PYRENE    

           REACT(31)       = 'BENAPY         '
           BICAUSE(1:2,31) = (/'OH ','HO '/)
           BIRATE(1:2,31)  = (/ 5.00D-11,   5.00D-11/)

C.. BENZO[A]PYRENE, accumulation mode

           REACT(32)       = 'ABENAPYJ       '
           LHCAUSE(1,32) = 'O3 '
!           LHRATE(1,32)  =  5.40D-02  ! average between 0% and 70%  Rel. Hum. Points in Owususa et al. (2004)
!           LH_EQU(32)  =  2.00D-15  ! average between 0% and 70%  Rel. Hum. Points in Owususa et al. (2004)
           LHRATE(1,32)  = 3.10D-02   ! average between 50% and 70% Rel. Hum. Points in Zhou et al. (2013) for organic shell around NH4SO4 core
           LH_EQU(1,32)  = 10.1D-15   ! average between 50% and 70% Rel. Hum. Points in Zhou et al. (2013) for organic shell around NH4SO4 core
           LH_TAMIN(1,32)  =  275.15  ! based on New-Shield BaP heteorogeneous oxidation treatment in Shrivastava et al. (2017)
           LH_RHMIN(1,32)  =  0.50    ! based on New-Shield BaP heteorogeneous oxidation treatment in Shrivastava et al. (2017)

C.. BENZO[A]PYRENE, Aitken mode

           REACT(33)       = 'ABENAPYI       '
           LHCAUSE(1,33) = 'O3 '
!           LHRATE(1,33)  = 5.40D-02 ! average between 0% and 70% Rel. Hum. Points in Owususa et al. (2004)
!           LH_EQU(1,33)  = 2.00D-15 ! average between 0% and 70% Rel. Hum. Points in Owususa et al. (2004)
           LHRATE(1,33)  = 3.10D-02  ! average between 50% and 70% Rel. Hum. Points in Zhou et al. (2013)
           LH_EQU(1,33)  = 10.1D-15  ! average between 50% and 70% Rel. Hum. Points in Zhou et al. (2013)
           LH_TAMIN(1,33)  =  275.15 ! based on New-Shield BaP heteorogeneous oxidation treatment in Shrivastava et al. (2017)
           LH_RHMIN(1,33)  =  0.50   ! based on New-Shield BaP heteorogeneous oxidation treatment in Shrivastava et al. (2017)

C.. CARBON TETRACHLORIDE without boundary conditions

            REACT(34)       = 'CARB_TET_NBC'
            BICAUSE(1:5,34) = (/'OH ','HO ','O3 ','NO3','CL '/)
            BIRATE(1:5,34)  = (/ 1.0D-12, 1.0D-12, 0.0D0, 0.0D0, 9.55D-11/)
            BI_ACT(1:5,34)  = (/2300.0D0,2300.0D0, 0.0D0, 0.0D0, 7.80D+03/)

           NDEGRADED_FOUND = 0
           DO I = 1, N_REACT
               N = 0
               DO J = 1, N_GC_SPC
                  IF ( TRIM( REACT( I ) ) == TRIM( GC_SPC( J ) ) ) THEN
                     N = J
                     EXIT
                  END IF
               END DO
               IF ( N == 0 ) THEN  ! search aerosol them  non-reactive species for index
                  DO J = 1, N_AE_SPC
                     IF ( TRIM( REACT( I ) ) ==  TRIM( AE_SPC( J ) ) ) THEN
                       N = J
                       EXIT
                     END IF
                  END DO
                  IF ( N == 0 ) THEN
                     DO J = 1, N_NR_SPC
                        IF ( TRIM( REACT( I ) ) == TRIM( NR_SPC( J ) ) ) THEN
                          N = J
                          EXIT
                        END IF
                     END DO
                  ENDIF
                ENDIF
                IF ( N == 0 ) CYCLE
                NDEGRADED_FOUND = NDEGRADED_FOUND + 1 
                INDEX_FOUND( NDEGRADED_FOUND ) = I
           END DO
           IF ( NDEGRADED_FOUND .GT. 0 ) THEN
              ALLOCATE( DEGRADED_SPECIES( NDEGRADED_FOUND ) )
              DO I = 1, NDEGRADED_FOUND 
                 DEGRADED_SPECIES( I ) = REACT( INDEX_FOUND( I ) )
              END DO
           END IF
           
           DEALLOCATE( INDEX_FOUND )
                  
         RETURN
         END SUBROUTINE DEGRADE_DATA
      
      END MODULE DEGRADE_PARAMETERS

