C ===================================================================
C This subroutine is the include file for DDM of ISORROPIAII
C 
C Written by Wenxian Zhang in August 2011
C
C Reference: 
C Zhang, W., Capps, S. L., Hu, Y., Nenes, A., Napelenok, S. L., & 
C     Russell, A. G. (2012). Development of the high-order decoupled 
C     direct method in three dimensions for particulate matter: 
C     enabling advanced sensitivity analysis in air quality models. 
C     Geoscientific Model Development, 5(2), 355-368. 
C     doi: 10.5194/gmd-5-355-2012
C ===================================================================

      INTEGER NSEN,NDIM
      PARAMETER ( NSEN = 33 )
      INTEGER NIONSPC
      PARAMETER ( NIONSPC = NIONS + 1)
      DOUBLE PRECISION LN10
      PARAMETER        (LN10=2.30258509299404568402D0)
 
      INTEGER    jTNA, jTSO4, jTNH4, jTNO3, jTCL, jTCA, jTK,jTMG
      PARAMETER( jTNA    =  1, 
     &           jTSO4   =  2,
     &           jTNH4   =  3,
     &           jTNO3   =  4,
     &           jTCL    =  5,
     &           jTCA    =  6,
     &           jTK     =  7,
     &           jTMG    =  8) 

      INTEGER    jH     , jNA    , jNH4   , jCL    , jSO4   , jHSO4  ,
     &           jNO3   , jCA    , jK     , jMG    , jH2O   , jNH3   , 
     &           jHCL   , jHNO3  , jNH42S4, jNH4HS4, jNACL  , jNA2SO4, 
     &           jNANO3 , jNH4NO3, jNH4CL , jNAHSO4, jLC    , jCASO4 ,
     &           jCANO32, jCACL2 , jK2SO4 , jKHSO4 , jKNO3  , jKCL   ,
     &           jMGSO4 , jMGNO32, jMGCL2 


      PARAMETER( jH      =  1, ! IONS
     &           jNA     =  2,
     &           jNH4    =  3,
     &           jCL     =  4,
     &           jSO4    =  5,
     &           jHSO4   =  6,
     &           jNO3    =  7,
     &           jCA     =  8,
     &           jK      =  9,
     &           jMG     = 10,
     &           jH2O    = 11, ! WATER
     &           jNH3    = 12, ! GASES
     &           jHCL    = 13,
     &           jHNO3   = 14,
     &           jNH42S4 = 15, ! SOLIDS
     &           jNH4HS4 = 16,
     &           jNACL   = 17,
     &           jNA2SO4 = 18,
     &           jNANO3  = 19,
     &           jNH4NO3 = 20,
     &           jNH4CL  = 21,
     &           jNAHSO4 = 22,
     &           jLC     = 23,
     &           jCASO4  = 24,
     &           jCANO32 = 25,
     &           jCACL2  = 26,
     &           jK2SO4  = 27,
     &           jKHSO4  = 28,
     &           jKNO3   = 29,
     &           jKCL    = 30,
     &           jMGSO4  = 31,
     &           jMGNO32 = 32,
     &           jMGCL2  = 33)  

      INTEGER    mNACL  , mNA2SO4, mNANO3 , mNH42S4, mNH4NO3, mNH4CL ,
     &           mH2SO4 , mHHSO4 , mNH4HS4, mHNO3  , mHCL   , mNAHSO4,
     &           mLC    , mCASO4 , mCANO32, mCACL2 , mK2SO4 , mKHSO4 ,
     &           mKNO3  , mKCL   , mMGSO4 , mMGNO32, mMGCL2
      PARAMETER( mNACL   =  1, ! ION PAIRS
     &           mNA2SO4 =  2,
     &           mNANO3  =  3,
     &           mNH42S4 =  4,
     &           mNH4NO3 =  5,
     &           mNH4CL  =  6,
     &           mH2SO4  =  7,
     &           mHHSO4  =  8,
     &           mNH4HS4 =  9,
     &           mHNO3   = 10,
     &           mHCL    = 11,
     &           mNAHSO4 = 12,
     &           mLC     = 13,
     &           mCASO4  = 14,
     &           mCANO32 = 15,
     &           mCACL2  = 16,
     &           mK2SO4  = 17,
     &           mKHSO4  = 18,
     &           mKNO3   = 19,
     &           mKCL    = 20,
     &           mMGSO4  = 21,
     &           mMGNO32 = 22,
     &           mMGCL2  = 23 )

      INTEGER    iK1    , iK2    , iK3    , iK4    , iK5    , iK6    ,
     &           iK7    , iK8    , iK9    , iK10   , iK11   , iK12   ,
     &           iK13   , iMBNA  , iMBSO4 , iMBNH4 , iMBNO3 , iMBCL  ,
     &           iCB    , iZSR   , iK14   , iK15   , iK16   , iK17   ,
     &           iK18   , iK19   , iK20   , iK21   , iK22   , iK23   ,
     &           iMBCA  , iMBK   , iMBMG 
      PARAMETER( iK1     =  1, ! HSO4(aq) <==> H(aq) + SO4(aq)
     &           iK2     =  2, ! NH3(aq)  <==> NH4(aq) + OH(aq)
     &           iK3     =  3, ! HCL(g)   <==> H(aq) + CL(aq)
     &           iK4     =  4, ! HNO3(g)  <==> H(aq) + NO3(aq)
     &           iK5     =  5, ! NA2SO4(s)<==> 2*NA(aq) + SO4(aq)
     &           iK6     =  6, ! NH4CL(s) <==> NH3(g) + HCL(g)
     &           iK7     =  7, ! (NH4)2SO4(s) <==> 2*NH4(aq) + SO4(aq)
     &           iK8     =  8, ! NACL(s) <==> NA(aq) + CL(aq) 
     &           iK9     =  9, ! NANO3(s) <==> NA(aq) + NO3(aq)
     &           iK10    = 10, ! NH4NO3(s) <==> NH3(g) + HNO3(g)
     &           iK11    = 11, ! NAHSO4(s) <==> NA(aq) + HSO4(aq)
     &           iK12    = 12, ! NH4HSO4(s) <==> NH4(aq) + HSO4(aq)
     &           iK13    = 13, ! LC <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
     &           iK14    = 14, ! CA(NO3)2(s) <==> CA(aq) + 2NO3(aq)
     &           iK15    = 15, ! CACL2(s) <==> CA(aq) + 2CL(aq)
     &           iK16    = 16, ! K2SO4(s) <==> 2K(aq) + SO4(aq)
     &           iK17    = 17, ! KHSO4(s) <==> K(aq) + HSO4(aq)
     &           iK18    = 18, ! KNO3(s) <==> K(aq) + NO3(aq)
     &           iK19    = 19, ! KCL(s) <==> K(aq) + CL(aq)
     &           iK20    = 20, ! MGS04(s) <==> MG(aq) + SO4(aq)
     &           iK21    = 21, ! MG(NO3)2(s) <==> MG(aq) + 2NO3(aq)
     &           iK22    = 22, ! MGCL2(s) <==> MG(aq) + 2CL(aq)
     &           iK23    = 23,
     &           iMBNA   = 24, ! Mass Balances
     &           iMBSO4  = 25,
     &           iMBNH4  = 26,
     &           iMBNO3  = 27,
     &           iMBCL   = 28,
     &           iMBCA   = 29,
     &           iMBK    = 30,
     &           iMBMG   = 31,
     &           iCB     = 32, ! Charge Balance
     &           iZSR    = 33 )! ZSR 

      CHARACTER        CC*1
      DOUBLE PRECISION SINI(NSEN)          !INITIAL SENSITIVITIES
      DOUBLE PRECISION SGAMA(NPAIR)
      DOUBLE PRECISION SI1, SH1, SY1(6,4),SX1(6,4),SG01(NPAIR)
      DOUBLE PRECISION SI2, SH2, SY2(6,4),SX2(6,4),SG02(NPAIR)
      COMMON /DDMISO/ CC, SINI, SGAMA, NDIM,
     &                SI1, SH1, SY1, SX1, SG01,
     &                SI2, SH2, SY2, SX2, SG02
      
c      DOUBLE PRECISION MOLALD
c      COMMON /MINOR/ MOLALD(NIONS)
c      INTEGER NONPYS,SAT
c      COMMON /FILT/ NONPYS,SAT





     
