
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

      USE RXNS_DATA
      USE DEGRADE_SETUP_TOX

      IMPLICIT NONE


C**********************************************************************


C..looping data

      UNI_START   = 1
      BI_START    = UNI_START + N_UNI_LOSS
      TRI_START   = BI_START  + N_BI_LOSS
      PHOTO_START = TRI_START + N_TRI_LOSS

      UNI_STOP   = BI_START    - 1
      BI_STOP    = TRI_START   - 1
      TRI_STOP   = PHOTO_START - 1
      PHOTO_STOP = N_PROCESSES

      ALLOCATE( REACT     ( N_REACT ) )                 ! names of species being degraded
      ALLOCATE( BICAUSE   ( N_REACT, N_BI_LOSS ) )      ! species name that cause degradation
      ALLOCATE( TRICAUSE  ( N_REACT, N_TRI_LOSS, 2 ) )  ! species name that cause degradation
      ALLOCATE( PHOTO_NAME( N_REACT, N_PHOTO_LOSS ) )   ! name of photolysis rate for react(i)

      ALLOCATE( UNIRATE  ( N_REACT, N_UNI_LOSS ) )  ! rate for unimolecular decay for react(i) [molecules/sec^1]
      ALLOCATE( UNI_ACT  ( N_REACT, N_UNI_LOSS ) )  ! activation energy for UNIRATE(I) [K]. Positive if exothermic
      ALLOCATE( UNI_TEXP ( N_REACT, N_UNI_LOSS ) )  ! exponent of Temperature and UNIRATE(I) factored by (1/298)**UNI_TEXP
      ALLOCATE( UNI_PROD ( N_REACT, N_UNI_LOSS ) )  ! name of daughter product for react(i)
      ALLOCATE( UNI_YIELD( N_REACT, N_UNI_LOSS ) )  ! yield of daughter product for react(i)

      ALLOCATE( BIRATE  ( N_REACT, N_BI_LOSS ) )  ! degradation rates for bimolecular reactions,  [cm^3/(sec*molecules)]
      ALLOCATE( BI_ACT  ( N_REACT, N_BI_LOSS ) )  ! activation energy for BIRATE(I) [K]. Positive if exothermic
      ALLOCATE( BI_TEXP ( N_REACT, N_BI_LOSS ) )  ! exponent of Temperature and BIRATE(I) factored by (1/298)**BI_TEXP
      ALLOCATE( BI_PROD ( N_REACT, N_BI_LOSS ) )  ! name of daughter product for react(i)
      ALLOCATE( BI_YIELD( N_REACT, N_BI_LOSS ) )  ! yield of daughter product for react(i)

      ALLOCATE( TRIRATE  ( N_REACT, N_TRI_LOSS ) )  ! degradation rates for trimolecular reactions,  [cm^3/(sec*molecules)]
      ALLOCATE( TRI_ACT  ( N_REACT, N_TRI_LOSS ) )  ! activation energy for TRIRATE(I) [K]. Positive if exothermic
      ALLOCATE( TRI_TEXP ( N_REACT, N_TRI_LOSS ) )  ! exponent of Temperature and TRIRATE(I) factored by (1/298)**TRI_TEXP
      ALLOCATE( TRI_PROD ( N_REACT, N_TRI_LOSS ) )  ! name of daughter product for react(i)
      ALLOCATE( TRI_YIELD( N_REACT, N_TRI_LOSS ) )  ! yield of daughter product for react(i)

      ALLOCATE( A_PHOTO    ( N_REACT, N_PHOTO_LOSS ) )  ! multiplier of photolysis rates
      ALLOCATE( PHOTO_PROD ( N_REACT, N_PHOTO_LOSS ) )  ! name of daughter product for react(i)
      ALLOCATE( PHOTO_YIELD( N_REACT, N_PHOTO_LOSS ) )  ! yield of daughter product for react(i)

      ALLOCATE( RATE_CONST( N_REACT, N_PROCESSES ) )
      ALLOCATE( RATE_YIELD( N_REACT, N_PROCESSES ) )

C..arrays to store indices to CGRID

      ALLOCATE( RXTANT_MAP( N_REACT ) )
      ALLOCATE( PROD_MAP  ( N_REACT, N_PROCESSES ) )
      ALLOCATE( RAD_MAP   ( N_REACT, N_BI_LOSS ) )
      ALLOCATE( RAD2_MAP  ( N_REACT, N_TRI_LOSS, 2 ) )
      ALLOCATE( PHOTO_MAP ( N_REACT, N_PHOTO_LOSS ) )

      REACT      = BLANK
      PHOTO_NAME = BLANK
      BICAUSE    = BLANK
      TRICAUSE   = BLANK

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

      RATE_YIELD = 1.0D0

! product are unused aspect and results not returned cgrid 
      UNI_PROD   = BLANK
      BI_PROD    = BLANK
      TRI_PROD   = BLANK
      PHOTO_PROD = BLANK

C..Initialize maps

      RXTANT_MAP = -1
      RAD_MAP    = -1
      RAD2_MAP   = -1
      PROD_MAP   = -1
      PHOTO_MAP  = -1

C..toxic VOC's

C.. ACRYLONITRILE

      REACT(1)       = 'ACRYLONITRILE'
      UNIRATE( 1, 1) = 0.0D0
      UNI_ACT( 1, 1) = 0.0D0
      UNI_TEXP(1, 1) = 0.0
      BICAUSE(1,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(1,1:5)  = (/ 4.8D-12, 4.8D-12, 1.9D-19, 6.3D-18, 5.2D-14/)
      BI_ACT(1,1:5)  = (/   0.0D0,   0.0D0, 0.0D0, 0.0D0, -1.4D+3/)

C.. CARBON TETRACHLORIDE

      REACT(2)       = 'CARBONTET'
      UNIRATE( 2, 1) = 0.0D0
      UNI_ACT( 2, 1) = 0.0D0
      UNI_TEXP(2, 1) = 0.0
      BICAUSE(2,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(2,1:5)  = (/ 1.0D-12, 1.0D-12, 0.0D0, 0.0D0, 9.55D-11/)
      BI_ACT(2,1:5)  = (/2300.0D0,2300.0D0, 0.0D0, 0.0D0, 7.80D+03/)

C.. PROPYLENE DICHLORIDE

      REACT(3)       = 'PROPDICHLORIDE'
      UNIRATE( 3, 1) = 0.0D0
      UNI_ACT( 3, 1) = 0.0D0
      UNI_TEXP(3, 1) = 0.0
      BICAUSE(3,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(3,1:5)  = (/ 2.2D-13, 2.2D-13, 0.0D0, 0.0D0, 1.1D-11/)
      BI_ACT(3,1:5)  = (/   0.0D0,   0.0D0, 0.0D0, 0.0D0, 0.0D0/)

C.. DICHLORIPROPENE

      REACT(4)       = 'DICHLOROPROPENE'
      UNIRATE( 4, 1) = 0.0D0
      UNI_ACT( 4, 1) = 0.0D0
      UNI_TEXP(4, 1) = 0.0
      BICAUSE(4,1:4) = (/'OH ','HO ','O3 ','NO3'/)
      BIRATE(4,1:4)  = (/1.04D-11, 1.04D-11, 4.10D-19, 0.0D0/)
      BI_ACT(4,1:4)  = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0/)

C.. 1,1,2,2TETRACHLOROETHANE

      REACT(5)       = 'CL4_ETHANE1122'
      UNIRATE( 5, 1) = 0.0D0
      UNI_ACT( 5, 1) = 0.0D0
      UNI_TEXP(5, 1) = 0.0
      BICAUSE(5,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(5,1:5)  = (/2.46D-13, 2.46D-13, 0.0D0, 0.0D0, 1.91D-13/)
      BI_ACT(5,1:5)  = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0, 0.00D0/)

C.. CHLOROFORM

      REACT(6)       = 'CHCL3'
      UNIRATE( 6, 1) = 0.0D0
      UNI_ACT( 6, 1) = 0.0D0
      UNI_TEXP(6, 1) = 0.0
      BICAUSE(6,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(6,1:5)  = (/6.30D-18, 6.30D-18, 0.0D0, 0.0D0, 4.9D-12/)
      BI_ACT(6,1:5)  = (/  504.D0,   504.D0, 0.0D0, 0.0D0, 1.24D+3/)
      BI_TEXP(6,1:2) = (/   2.0D0,    2.0D0/)

C.. 1,2DIBROMOETHANE

      REACT(7)       = 'BR2_C2_12'
      UNIRATE( 7, 1) = 0.0D0
      UNI_ACT( 7, 1) = 0.0D0
      UNI_TEXP(7, 1) = 0.0
      BICAUSE(7,1:4) = (/'OH ','HO ','O3 ','NO3'/)
      BIRATE(7,1:4)  = (/1.03D-17, 1.03D-17, 0.0D0, 0.0D0/)
      BI_ACT(7,1:4)  = (/  422.D0,   422.D0, 0.0D0, 0.0D0/)
      BI_TEXP(7,1:2) = (/   2.0D0,    2.0D0/)

C.. 1,2DICHLOROTHANE

      REACT(8)       = 'CL2_C2_12'
      UNIRATE( 8, 1) = 0.0D0
      UNI_ACT( 8, 1) = 0.0D0
      UNI_TEXP(8, 1) = 0.0
      BICAUSE(8,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(8,1:5)  = (/1.10D-17, 1.10D-17, 0.0D0, 0.0D0, 4.93D-11/)
      BI_ACT(8,1:5)  = (/  409.D0,   409.D0, 0.0D0, 0.0D0, 1.09D+3/)
      BI_TEXP(8,1:2) = (/   2.0D0,    2.0D0/)

C.. ETHYLENE OXIDE

      REACT(9)       = 'ETOX'
      UNIRATE( 9, 1) = 0.0D0
      UNI_ACT( 9, 1) = 0.0D0
      UNI_TEXP(9, 1) = 0.0
      BICAUSE(9,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(9,1:5)  = (/7.60D-14, 7.60D-14, 0.0D0, 0.0D0, 5.0D-12/)
      BI_ACT(9,1:5)  = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0, 0.0D0/)

C.. METHYLENE CHLORIDE

      REACT(10)       = 'CL2_ME'
      UNIRATE( 10, 1) = 0.0D0
      UNI_ACT( 10, 1) = 0.0D0
      UNI_TEXP(10, 1) = 0.0
      BICAUSE(10,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(10,1:5)  = (/8.54D-18, 8.54D-18, 0.0D0, 0.0D0, 2.81D-11/)
      BI_ACT(10,1:5)  = (/  500.D0,   500.D0, 0.0D0, 0.0D0, 1.3D+3/)
      BI_TEXP(10,1:2) = (/   2.0D0,    2.0D0/)

C.. PERCHLOROETHYLENE

      REACT(11)       = 'CL4_ETHE'
      UNIRATE(11, 1)  = 0.0D0
      UNI_ACT( 11, 1) = 0.0D0
      UNI_TEXP(11, 1) = 0.0
      BICAUSE(11,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(11,1:5)  = (/9.64D-12, 9.64D-12, 0.0D0, 0.0D0, 3.72D-8/)
      BI_ACT(11,1:5)  = (/ 1209.D0,  1209.D0, 0.0D0, 0.0D0, 0.0D0/)
      BI_TEXP(11,1:5) = (/   0.0D0,    0.0D0, 0.0D0, 0.0D0,-1.2D0/)

C.. TRICHLOROETHYLENE

      REACT(12)       = 'CL3_ETHE'
      UNIRATE( 12, 1) = 0.0D0
      UNI_ACT( 12, 1) = 0.0D0
      UNI_TEXP(12, 1) = 0.0
      BICAUSE(12,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(12,1:5)  = (/5.63D-13, 5.63D-13, 0.0D0, 0.0D0, 8.07D-11/)
      BI_ACT(12,1:5)  = (/ -427.D0,  -427.D0, 0.0D0, 0.0D0, 0.0D0/)

C.. VINYL CHLORIDE

      REACT(13)       = 'CL_ETHE'
      UNIRATE(13, 1)  = 0.0D0
      UNI_ACT( 13, 1) = 0.0D0
      UNI_TEXP(13, 1) = 0.0
      BICAUSE(13,1:5) = (/'OH ','HO ','O3 ','NO3', 'CL '/)
      BIRATE(13,1:5)  = (/1.69D-12, 1.69D-12, 0.0D0,1.84D-13, 1.27D-10/)
      BI_ACT(13,1:5)  = (/ -422.D0,  -422.D0, 0.0D0, 1764.D0, 0.0D0/)

C.. NAPHTHALENE

      REACT(14)       = 'NAPHTHALENE'
      BICAUSE(14,1:3) = (/'OH ','HO ','O3 '/)
      BIRATE(14,1:3)  = (/1.05D-12, 1.05D-12, 2.01D-19/)
      BI_ACT(14,1:3)  = (/ -901.D0,  -901.D0,    0.0D0/)
      TRICAUSE(14,1,1:2) = (/'NO3 ','NO2 '/)
      TRIRATE( 14,1)     =  3.94D-34
      TRI_ACT( 14,1)     = -4.10D+03

C.. QUINOLINE

      REACT(15)       = 'QUINOLINE'
      BICAUSE(15,1:4) = (/'OH ','HO ','O3 ','NO2'/)
      BIRATE(15,1:4)  = (/1.16D-11, 1.16D-11, 1.00D-19, 1.60D-20/)

C.. HEXAMETHYLENE DIISOCAYNATE

      REACT(16)       = 'HEXAMETHY_DIIS'
      BICAUSE(16,1:2) = (/'OH ','HO '/)
      BIRATE(16,1:2)  = (/7.94D-12, 7.94D-12/)

C.. HYDRAZINE

      REACT(17)       = 'HYDRAZINE'
      BICAUSE(17,1:3) = (/'OH ','HO ','O3 '/)
      BIRATE(17,1:3)  = (/3.60D-11, 3.60D-11, 3.00D-17/)

C.. MALEIC ANHYDRIDE

      REACT(18)       = 'MAL_ANHYDRIDE'
      BICAUSE(18,1:3) = (/'OH  ','HO  ','NO3 '/)
      BIRATE(18,1:3)  = (/1.45D-12, 1.45D-12, 1.02D-13/)

C.. TOULENE DIISOCYANATE 

      REACT(19)       = 'TOL_DIIS'
      BICAUSE(19,1:2) = (/'OH ','HO '/)
      BIRATE(19,1:2)  = (/7.41D-12, 7.41D-12/)

C.. TRIETHYLAMINE

      REACT(20)       = 'TRIETHYLAMINE'
      BICAUSE(20,1:5) = (/'OH  ','HO  ','O   ','O3P ', 'O3  '/)
      BIRATE(20,1:5)  = (/3.60D-11, 3.60D-11, 1.09D-11, 1.09D-11, 7.84D-18/)

C.. P-DICHLOROBENZENE 

      REACT(21)       = 'DICHLOROBENZENE'
      BICAUSE(21,1:2) = (/'OH ','HO '/)
      BIRATE(21,1:2)  = (/3.60D-13, 3.60D-13/)

C.. XYLENE

      REACT(22)       = 'XYLENE         '
      BICAUSE(22,1:3) = (/'OH','HO','CL'/)
      BIRATE(22,1:3)  = (/1.717D-11,                  1.717D-11,         1.433D-10/)

C.. STYRENE

      REACT(23)       = 'STYRENE        '
      BICAUSE(23,1:4) = (/'OH','HO','O3','CL'/)
      BIRATE(23,1:4)  = (/1.02D-11, 1.02D-11, 1.51D-13, 3.6D-10/)
      BI_ACT(23,1:2)  = (/   -532.0D0,  -532.0D0/)

C.. ACETONITRILE

      REACT(24)       = 'ACETONITRILE   '
      BICAUSE(24,1:3) = (/'OH','HO','CL'/)
      BIRATE(24,1:3)  = (/          8.31D-13,          8.31D-13,          1.60D-11/)
      BI_ACT(24,1:3)  = (/          1080.0D0,          1080.0D0,          2140.0D0/)

C.. ACRYACID

      REACT(25)       = 'ACRYACID       '
      BICAUSE(25,1:4) = (/'OH','HO','O3','CL'/)
      BIRATE(25,1:4)  = (/1.75D-11, 1.75D-11, 6.50D-19, 4.00D-10/)

C.. HEXANE 
 
      REACT(26)       = 'HEXANE         '
      BICAUSE(26,1:3) = (/'OH','HO','CL'/)
      BIRATE(26,1:3)  = (/          1.98D-11,          1.91D-11,          3.30D-10/)
      BI_ACT(26,1:2)  = (/           394.0D0,           394.0D0/)

C.. METHCHLORIDE
 
      REACT(27)       = 'METHCHLORIDE   '
      BICAUSE(27,1:3) = (/'OH','HO','CL'/)
      BIRATE(27,1:3)  = (/          2.10D-12,          2.10D-12,          1.80D-11/)
      BI_ACT(27,1:3)  = (/          1210.0D0,          1210.0D0,            1081D0/)

C.. CARBSULFIDE

      REACT(28)       = 'CARBSULFIDE    '
      BICAUSE(28,1:3) = (/'OH','HO','O3'/)
      BIRATE(28,1:3)  = (/          1.10D-13,          1.10D-13,           3.01D-16/)
      BI_ACT(28,1:2)  = (/          1200.0D0,          1200.0D0/)

C.. CHLOROPRENE

      REACT(29)       = 'CHLOROPRENE    '
      BICAUSE(29,1:5) = (/'OH ','HO ','O3 ','NO3','CL '/)
      BIRATE(29,1:5)  = (/          6.20D-11,          6.20D-11,          1.40D-16,
     &                              3.60D-13,          3.62D-10/)

C.. ETHYLBENZENE

      REACT(30)       = 'ETHYLBENZENE   '
      BICAUSE(30,1:4) = (/'OH ','HO ','NO3','CL '/)
      BIRATE(30,1:4)  = (/          7.00D-12,          7.00D-12,          5.70D-16,
     &                              1.15D-10/)

      RETURN
      END
