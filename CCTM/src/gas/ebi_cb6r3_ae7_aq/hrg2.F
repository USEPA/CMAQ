
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


      SUBROUTINE HRG2( DTC )
C**********************************************************************
C
C  FUNCTION: To solve for the concentration of HO, HO2, HONO, and
C            HNO4 alebraically.
C
C  PRECONDITIONS: For the CB6R3_AE7_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: None


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC                      ! Time step

C..PARAMETERS: None


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
!     CHARACTER( 16 ), SAVE :: PNAME = 'HRG2'             ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) ::    O1D_S                 ! sum of O1D loss frequencies
      REAL( 8 ) ::    OH_S                  ! stoich coeff for OH from O1D+H2O or H2
      REAL( 8 ) ::    HO2_S                 ! stoich coeff for HO2 from O1D+H2 rxn
      REAL( 8 ) ::    XOH_S                 ! stoich coeff for OH & HONO from NO2EX loss rxn
      REAL( 8 ) ::    R4_19                 ! production of OH from HONO
      REAL( 8 ) ::    R19_4                 ! production of HONO from OH
      REAL( 8 ) ::    R4_5                  ! production of OH from HO2
      REAL( 8 ) ::    R5_4                  ! production of HO2 from OH
      REAL( 8 ) ::    R5_21                 ! production of HO2 from HNO4
      REAL( 8 ) ::    R21_5                 ! production of HNO4 from HO2
      REAL( 8 ) ::    P4, P5, P19, P21      ! species production form other paths
      REAL( 8 ) ::    L4, L5, L19, L21      ! species loss terms

      REAL( 8 ) ::    A, B, C               ! coeffs of quadratic eq. for HO2
      REAL( 8 ) ::    Q                     ! intermediate term

      REAL( 8 ) ::    T1, T2, T3            ! intermediate terms

      REAL( 8 ) ::    L21_INV               ! reciprocal of HNO4 loss term

C**********************************************************************


c..stoichiometric coefficient for production of HOX from O3 via O1D
      O1D_S =                 RKI(    10 )                         ! O1D=O
     &      +                 RKI(    11 )                         ! O1D=0.2000D+01*OH

      OH_S =    2.0000D+00 * RKI(    11 )                         ! O1D=0.2000D+01*OH

      OH_S  = OH_S / O1D_S


c..Production of HO from HO2 (r4,5 terms )
      R4_5 =                 RKI(    13 ) * YC ( O3           )   ! HO2+O3=OH
     &     +                 RKI(    15 ) * YC ( O            )   ! HO2+O=OH
     &     +                 RKI(    25 ) * YC ( NO           )   ! HO2+NO=OH+NO2
     &     +                 RKI(    33 ) * YC ( NO3          )   ! HO2+NO3=OH+NO2
     &     +    4.4000D-01 * RKI(    57 ) * YC ( C2O3         )   ! HO2+C2O3=0.4400D+...
     &     +    4.4000D-01 * RKI(    65 ) * YC ( CXO3         )   ! HO2+CXO3=0.4400D+...
     &     +    2.0000D-01 * RKI(   104 ) * YC ( HCO3         )   ! HO2+HCO3=0.2000D+...
     &     +    1.2000D-01 * RKI(   152 ) * YC ( ISO2         )   ! HO2+ISO2=0.1200D+...
     &     +    1.7500D-01 * RKI(   166 ) * YC ( EPX2         )   ! HO2+EPX2=0.1125D+...
     &     +    4.4000D-01 * RKI(   215 ) * YC ( OPO3         )   ! HO2+OPO3=0.4400D+...

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =                 RKI(    43 )                         ! HONO=OH+NO

      R4_19 = R4_19 * DTC

c..Remaining HO production
      P4 =   OH_S  *       RXRAT(     9 )      ! O1D=0.2000D+01*OH
     &   +    2.0000D+00 * RXRAT(    21 )      ! H2O2=0.2000D+01*OH
     &   +                 RXRAT(    23 )      ! H2O2+O=OH+HO2
     &   +                 RXRAT(    47 )      ! HNO3=OH+NO2
     &   +    4.1000D-01 * RXRAT(    50 )      ! PNA=0.4100D+00*OH+0.5900D+...
     &   +                 RXRAT(    88 )      ! MEPX=OH+RO2+MEO2
     &   +                 RXRAT(    90 )      ! ROOH=OH+HO2
     &   +                 RXRAT(    99 )      ! FORM+O=OH+HO2+CO
     &   +                 RXRAT(   105 )      ! ALD2+O=OH+C2O3
     &   +                 RXRAT(   109 )      ! ALDX+O=OH+CXO3
     &   +    1.9000D-01 * RXRAT(   114 )      ! GLYD=0.1900D+00*OH+0.1400D+...
     &   +    3.0000D-01 * RXRAT(   137 )      ! ETH+O=0.3000D+00*OH+HO2+CO+...
     &   +    1.6000D-01 * RXRAT(   139 )      ! ETH+O3=0.1600D+00*OH+...
     &   +    1.0000D-01 * RXRAT(   141 )      ! OLE+O=0.1000D+00*OH+...
     &   +    3.3400D-01 * RXRAT(   143 )      ! OLE+O3=0.3340D+00*OH+...
     &   +    5.0000D-01 * RXRAT(   147 )      ! IOLE+O3=0.5000D+00*OH+...
     &   +    2.6600D-01 * RXRAT(   156 )      ! ISOP+O3=0.2660D+00*OH+...
     &   +    4.6100D-01 * RXRAT(   159 )      ! ISPD+O3=0.4610D+00*OH+...
     &   +                 RXRAT(   163 )      ! HPLD=OH+ISPD
     &   +    9.5000D-01 * RXRAT(   166 )      ! HO2+EPX2=0.1125D+01*OH+...
     &   +    1.2500D-01 * RXRAT(   167 )      ! EPX2+NO=0.1250D+00*OH+...
     &   +    1.0000D-01 * RXRAT(   168 )      ! EPX2+C2O3=0.1000D+00*OH+...
     &   +    1.2500D-01 * RXRAT(   169 )      ! EPX2+RO2=0.1250D+00*OH+...
     &   +    5.7000D-01 * RXRAT(   173 )      ! TERP+O3=0.5700D+00*OH+...
     &   +    5.7000D-01 * RXRAT(   177 )      ! APIN+O3=0.5700D+00*OH+...
     &   +    5.0000D-01 * RXRAT(   204 )      ! XOPN+O3=0.5000D+00*OH+...
     &   +    5.0000D-01 * RXRAT(   208 )      ! OPEN+O3=0.5000D+00*OH+...
     &   +                 RXRAT(   226 )      ! HOCL=OH+CL
     &   +    1.0000D-01 * RXRAT(   248 )      ! CL+TOL=0.1000D+00*OH+...
     &   +    2.4400D-01 * RXRAT(   249 )      ! CL+XYLMN=0.2440D+00*OH+...
     &   +    2.4400D-01 * RXRAT(   250 )      ! CL+NAPH=0.2440D+00*OH+...

      P4 = YC0( OH ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =                 RKI(    12 ) * YC ( O3           )   ! OH+O3=HO2
     &     +                 RKI(    14 ) * YC ( O            )   ! OH+O=HO2
     &     +                 RKI(    22 ) * YC ( H2O2         )   ! OH+H2O2=HO2
     &     +                 RKI(    32 ) * YC ( NO3          )   ! OH+NO3=HO2+NO2
     &     +                 RKI(    52 ) * YC ( SO2          )   ! OH+SO2=HO2+SULF+...
     &     +                 RKI(    93 ) * YC ( FACD         )   ! OH+FACD=HO2
     &     +                 RKI(    96 ) * YC ( FORM         )   ! OH+FORM=HO2+CO
     &     +    2.0000D-01 * RKI(   113 ) * YC ( GLYD         )   ! OH+GLYD=0.2000D+...
     &     +                 RKI(   116 ) * YC ( GLY          )   ! OH+GLY=HO2+...
     &     +                 RKI(   122 )                         ! OH=HO2
     &     +                 RKI(   123 ) * YC ( CO           )   ! OH+CO=HO2
     &     +                 RKI(   126 ) * YC ( MEOH         )   ! OH+MEOH=HO2+FORM
     &     +    9.0000D-01 * RKI(   127 ) * YC ( ETOH         )   ! OH+ETOH=0.9000D+...
     &     +    3.0000D-01 * RKI(   136 ) * YC ( ETHY         )   ! OH+ETHY=0.3000D+...
     &     +    1.3700D-01 * RKI(   158 ) * YC ( ISPD         )   ! OH+ISPD=0.1370D+...
     &     +    5.3000D-01 * RKI(   179 ) * YC ( BENZENE      )   ! OH+BENZENE=...
     &     +    1.8000D-01 * RKI(   184 ) * YC ( TOL          )   ! OH+TOL=0.1800D+...
     &     +    1.5500D-01 * RKI(   189 ) * YC ( XYLMN        )   ! OH+XYLMN=0.1550D+...
     &     +    1.5500D-01 * RKI(   190 ) * YC ( NAPH         )   ! OH+NAPH=0.1550D+...
     &     +                 RKI(   195 ) * YC ( CRES         )   ! OH+CRES=HO2+...
     &     +    2.0000D-01 * RKI(   210 ) * YC ( CAT1         )   ! OH+CAT1=0.2000D+...

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =                 RKI(    49 )                         ! PNA=HO2+NO2
     &      +    5.9000D-01 * RKI(    50 )                         ! PNA=0.5900D+...

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5 =                 RXRAT(    23 )      ! H2O2+O=HO2+OH
     &   +                 RXRAT(    71 )      ! MEO2+NO=HO2+FORM+NO2
     &   +    9.0000D-01 * RXRAT(    73 )      ! MEO2+C2O3=0.9000D+00*HO2+...
     &   +    3.7000D-01 * RXRAT(    74 )      ! MEO2+RO2=0.3700D+00*HO2+...
     &   +                 RXRAT(    75 )      ! XO2H+NO=HO2+NO2
     &   +    8.0000D-01 * RXRAT(    77 )      ! XO2H+C2O3=0.8000D+00*HO2+...
     &   +    6.0000D-01 * RXRAT(    78 )      ! XO2H+RO2=0.6000D+00*HO2+RO2
     &   +    8.0000D-01 * RXRAT(    85 )      ! XO2N+C2O3=0.8000D+00*HO2+...
     &   +                 RXRAT(    90 )      ! ROOH=HO2+OH
     &   +    2.0000D+00 * RXRAT(    97 )      ! FORM=0.2000D+01*HO2+CO
     &   +                 RXRAT(    99 )      ! FORM+O=HO2+OH+CO
     &   +                 RXRAT(   100 )      ! FORM+NO3=HO2+HNO3+CO
     &   +                 RXRAT(   102 )      ! HCO3=HO2+FORM
     &   +                 RXRAT(   103 )      ! HCO3+NO=HO2+NO2+FACD
     &   +                 RXRAT(   108 )      ! ALD2=HO2+RO2+CO+MEO2
     &   +                 RXRAT(   112 )      ! ALDX=HO2+XO2H+RO2+CO+ALD2
     &   +    1.4000D+00 * RXRAT(   114 )      ! GLYD=0.1400D+01*HO2+...
     &   +    2.0000D+00 * RXRAT(   117 )      ! GLY=0.2000D+01*HO2+0.2000D+...
     &   +                 RXRAT(   118 )      ! GLY+NO3=HO2+0.1500D+01*CO+...
     &   +                 RXRAT(   119 )      ! MGLY=HO2+C2O3+CO
     &   +                 RXRAT(   134 )      ! ROR=HO2+KET
     &   +                 RXRAT(   137 )      ! ETH+O=HO2+0.3000D+00*OH+CO+...
     &   +    1.6000D-01 * RXRAT(   139 )      ! ETH+O3=0.1600D+00*HO2+...
     &   +    1.0000D-01 * RXRAT(   141 )      ! OLE+O=0.1000D+00*HO2+...
     &   +    8.0000D-02 * RXRAT(   143 )      ! OLE+O3=0.8000D-01*HO2+...
     &   +    2.5000D-01 * RXRAT(   150 )      ! ISOP+O=0.2500D+00*HO2+...
     &   +    8.1800D-01 * RXRAT(   151 )      ! ISO2+NO=0.8180D+00*HO2+...
     &   +    7.2800D-01 * RXRAT(   153 )      ! ISO2+C2O3=0.7280D+00*HO2+...
     &   +    7.2800D-01 * RXRAT(   154 )      ! ISO2+RO2=0.7280D+00*HO2+...
     &   +                 RXRAT(   155 )      ! ISO2=HO2+HPLD
     &   +    6.6000D-02 * RXRAT(   156 )      ! ISOP+O3=0.6600D-01*HO2+...
     &   +    3.9800D-01 * RXRAT(   159 )      ! ISPD+O3=0.3980D+00*HO2+...
     &   +    7.6000D-01 * RXRAT(   161 )      ! ISPD=0.7600D+00*HO2+...
     &   +    8.2500D-01 * RXRAT(   167 )      ! EPX2+NO=0.8250D+00*HO2+...
     &   +    6.6000D-01 * RXRAT(   168 )      ! EPX2+C2O3=0.6600D+00*HO2+...
     &   +    8.2500D-01 * RXRAT(   169 )      ! EPX2+RO2=0.8250D+00*HO2+...
     &   +    9.1800D-01 * RXRAT(   180 )      ! BZO2+NO=0.9180D+00*HO2+...
     &   +                 RXRAT(   181 )      ! BZO2+C2O3=HO2+OPEN+GLY+...
     &   +                 RXRAT(   183 )      ! BZO2+RO2=HO2+OPEN+GLY+RO2
     &   +    8.6000D-01 * RXRAT(   185 )      ! TO2+NO=0.8600D+00*HO2+...
     &   +                 RXRAT(   186 )      ! TO2+C2O3=HO2+0.5200D+...
     &   +                 RXRAT(   188 )      ! TO2+RO2=HO2+0.5200D+...
     &   +    8.6000D-01 * RXRAT(   191 )      ! XLO2+NO=0.8600D+00*HO2+...
     &   +                 RXRAT(   193 )      ! XLO2+C2O3=HO2+0.7700D+...
     &   +                 RXRAT(   194 )      ! XLO2+RO2=HO2+0.7700D+...
     &   +                 RXRAT(   201 )      ! CRON=HO2+HONO+FORM+OPEN
     &   +    7.0000D-01 * RXRAT(   202 )      ! XOPN=0.7000D+00*HO2+XO2H+...
     &   +                 RXRAT(   206 )      ! OPEN=HO2+OPO3+CO
     &   +    5.6000D-01 * RXRAT(   208 )      ! OPEN+O3=0.5600D+00*HO2+...
     &   +    8.0000D-01 * RXRAT(   212 )      ! OPO3+NO=0.8000D+00*HO2+...
     &   +                 RXRAT(   231 )      ! CLO+MEO2=HO2+FORM+CL
     &   +                 RXRAT(   233 )      ! FMCL=HO2+CO+CL
     &   +                 RXRAT(   238 )      ! CL+ETH=HO2+0.2000D+01*XO2+...
     &   +                 RXRAT(   239 )      ! CL+OLE=HO2+0.3300D+00*ALD2+...
     &   +                 RXRAT(   240 )      ! CL+IOLE=HO2+0.7000D+...
     &   +                 RXRAT(   242 )      ! CL+FORM=HO2+HCL+CO
     &   +                 RXRAT(   245 )      ! CL+MEOH=HO2+HCL+FORM
     &   +                 RXRAT(   246 )      ! CL+ETOH=HO2+HCL+ALD2
     &   +    1.8000D-01 * RXRAT(   248 )      ! CL+TOL=0.1800D+00*HO2+...
     &   +    1.5500D-01 * RXRAT(   249 )      ! CL+XYLMN=0.1550D+00*HO2+...
     &   +    1.5500D-01 * RXRAT(   250 )      ! CL+NAPH=0.1550D+00*HO2+...

      P5 = YC0( HO2 ) + P5 * DTC

c..Production of HONO from OH (r19,4 terms )

      R19_4 =   RKI(    40 ) * YC( NO           ) * DTC      ! OH+NO=HONO

c..Remaining HONO production terms
      P19 =    2.0000D+00 * RXRAT(    41 )      ! NO+NO2=0.2000D+01*HONO
     &    +                 RXRAT(   201 )      ! CRON=HONO+HO2+FORM+OPEN
     &    +    5.0000D-01 * RXRAT(   276 )      ! NO2=0.5000D+00*HONO+...

      P19 = YC0( HONO ) + P19 * DTC

c..Production of PNA from HO2 (r21,5 term )

      R21_5 =   RKI(    48 ) * YCP( NO2          ) * DTC      ! HO2+NO2=HNO4

c..Remaining PNA production terms
      P21   =   YC0( PNA )

c..OH loss terms not in R5_4 & R19_4
      L4 =    2.0000D+00 * RKI(    16 ) * YC ( OH           )   ! OH+OH=O
     &   +    2.0000D+00 * RKI(    17 ) * YC ( OH           )   ! OH+OH=H2O2
     &   +                 RKI(    18 ) * YC ( HO2          )   ! OH+HO2=
     &   +                 RKI(    44 ) * YC ( HONO         )   ! OH+HONO=NO2
     &   +                 RKI(    45 ) * YC ( NO2          )   ! OH+NO2=HNO3
     &   +                 RKI(    46 ) * YC ( HNO3         )   ! OH+HNO3=NO3
     &   +                 RKI(    51 ) * YC ( PNA          )   ! OH+PNA=NO2
     &   +    6.0000D-01 * RKI(    87 ) * YC ( MEPX         )   ! OH+MEPX=0.6000D+...
     &   +    6.0000D-01 * RKI(    89 ) * YC ( ROOH         )   ! OH+ROOH=0.5400D+...
     &   +                 RKI(    91 ) * YC ( NTR1         )   ! OH+NTR1=NTR2
     &   +                 RKI(    94 ) * YC ( AACD         )   ! OH+AACD=MEO2+RO2
     &   +                 RKI(    95 ) * YC ( PACD         )   ! OH+PACD=C2O3
     &   +                 RKI(   106 ) * YC ( ALD2         )   ! OH+ALD2=C2O3
     &   +                 RKI(   110 ) * YC ( ALDX         )   ! OH+ALDX=CXO3
     &   +    8.0000D-01 * RKI(   113 ) * YC ( GLYD         )   ! OH+GLYD=0.2000D+...
     &   +                 RKI(   121 ) * YC ( MGLY         )   ! OH+MGLY=C2O3+CO
     &   +                 RKI(   124 )                         ! OH=MEO2+RO2
     &   +                 RKI(   125 ) * YC ( ETHA         )   ! OH+ETHA=0.9910D+...
     &   +    1.0000D-01 * RKI(   127 ) * YC ( ETOH         )   ! OH+ETOH=0.9000D+...
     &   +                 RKI(   130 ) * YC ( ACET         )   ! OH+ACET=FORM+...
     &   +                 RKI(   131 ) * YC ( PRPA         )   ! OH+PRPA=XPRP
     &   +                 RKI(   132 ) * YC ( PAR          )   ! OH+PAR=XPAR
     &   +                 RKI(   138 ) * YC ( ETH          )   ! OH+ETH=XO2H+RO2+...
     &   +                 RKI(   142 ) * YC ( OLE          )   ! OH+OLE=0.7810D+...
     &   +                 RKI(   146 ) * YC ( IOLE         )   ! OH+IOLE=0.1300D+...
     &   +                 RKI(   149 ) * YC ( ISOP         )   ! OH+ISOP=ISO2+RO2+...
     &   +    8.6300D-01 * RKI(   158 ) * YC ( ISPD         )   ! OH+ISPD=0.1370D+...
     &   +    6.7000D-02 * RKI(   162 ) * YC ( ISPX         )   ! OH+ISPX=0.9040D+...
     &   +                 RKI(   165 ) * YC ( EPOX         )   ! OH+EPOX=EPX2+RO2
     &   +                 RKI(   170 ) * YC ( INTR         )   ! OH+INTR=0.6300D+...
     &   +                 RKI(   172 ) * YC ( TERP         )   ! OH+TERP=0.7500D+...
     &   +                 RKI(   176 ) * YC ( APIN         )   ! OH+APIN=0.7500D+...
     &   +    3.5200D-01 * RKI(   179 ) * YC ( BENZENE      )   ! OH+BENZENE=...
     &   +    7.2000D-01 * RKI(   184 ) * YC ( TOL          )   ! OH+TOL=0.1800D+...
     &   +    6.0100D-01 * RKI(   189 ) * YC ( XYLMN        )   ! OH+XYLMN=0.1550D+...
     &   +    6.0100D-01 * RKI(   190 ) * YC ( NAPH         )   ! OH+NAPH=0.1550D+...
     &   +                 RKI(   199 ) * YC ( CRON         )   ! OH+CRON=NTR2+...
     &   +                 RKI(   203 ) * YC ( XOPN         )   ! OH+XOPN=MGLY+...
     &   +                 RKI(   207 ) * YC ( OPEN         )   ! OH+OPEN=0.6000D+...
     &   +    8.0000D-01 * RKI(   210 ) * YC ( CAT1         )   ! OH+CAT1=0.2000D+...
     &   +                 RKI(   218 ) * YC ( OPAN         )   ! OH+OPAN=0.5000D+...
     &   +                 RKI(   219 ) * YC ( PANX         )   ! OH+PANX=ALD2+NO2
     &   +                 RKI(   220 ) * YC ( ECH4         )   ! OH+ECH4=MEO2+RO2
     &   +                 RKI(   232 ) * YC ( FMCL         )   ! OH+FMCL=CL+CO
     &   +                 RKI(   247 ) * YC ( HCL          )   ! OH+HCL=CL

      L4    = 1.0D0 + L4 * DTC + R5_4 + R19_4

c..HO2 loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5 =                 RKI(    18 ) * YC ( OH           )   ! HO2+OH=
     &   +    5.6000D-01 * RKI(    57 ) * YC ( C2O3         )   ! HO2+C2O3=0.4400D+...
     &   +    5.6000D-01 * RKI(    65 ) * YC ( CXO3         )   ! HO2+CXO3=0.4400D+...
     &   +                 RKI(    72 ) * YC ( MEO2         )   ! HO2+MEO2=0.9000D+...
     &   +                 RKI(    76 ) * YC ( XO2H         )   ! HO2+XO2H=ROOH
     &   +                 RKI(    80 ) * YC ( XO2          )   ! HO2+XO2=ROOH
     &   +                 RKI(    84 ) * YC ( XO2N         )   ! HO2+XO2N=ROOH
     &   +                 RKI(   101 ) * YC ( FORM         )   ! HO2+FORM=HCO3
     &   +    6.0000D-01 * RKI(   104 ) * YC ( HCO3         )   ! HO2+HCO3=0.2000D+...
     &   +    7.6000D-01 * RKI(   152 ) * YC ( ISO2         )   ! HO2+ISO2=0.1200D+...
     &   +                 RKI(   182 ) * YC ( BZO2         )   ! HO2+BZO2=
     &   +                 RKI(   187 ) * YC ( TO2          )   ! HO2+TO2=
     &   +                 RKI(   192 ) * YC ( XLO2         )   ! HO2+XLO2=
     &   +                 RKI(   198 ) * YC ( CRO          )   ! HO2+CRO=CRES
     &   +    5.6000D-01 * RKI(   215 ) * YC ( OPO3         )   ! HO2+OPO3=0.4400D+...
     &   +                 RKI(   230 ) * YC ( CLO          )   ! HO2+CLO=HOCL

      L5    = 1.0D0 + L5 * DTC + R4_5 + R21_5

c..HONO loss terms not included in R4_19
      L19 =    2.0000D+00 * RKI(    42 ) * YC ( HONO         )   ! HONO+HONO=NO+NO2
     &    +                 RKI(    44 ) * YC ( OH           )   ! HONO+OH=NO2

      L19   = 1.0D0 + L19 * DTC + R4_19

c..PNA loss terms not included in R5_21
      L21 =    4.1000D-01 * RKI(    50 )                         ! PNA=0.5900D+...
     &    +                 RKI(    51 ) * YC ( OH           )   ! PNA+OH=NO2

      L21   = 1.0D0 + L21 * DTC + R5_21

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute terms used to calculate a,b & c
      L21_INV = 1.0D0 / L21
      T1 = 1.0D0 / ( L4 * L19 - R4_19 * R19_4 )
      T2 = R5_4 * T1
      T3 = R5_21 * L21_INV

c..solve quadratic equation for HO2
      A = 2.0D0 * ( RKI(    19 ) + RKI(    20 ) ) * DTC

      B = L5 - T3 * R21_5 - T2 * R4_5 * L19

      C = P5 + T3 * P21 + T2 * ( P4 * L19 + P19 * R4_19 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( HO2 ) = MAX( Q / A , -C / Q  )

c..compute remaining species concentrations
      YCP( OH ) = ( ( P4 + R4_5 * YCP( HO2 ) ) * L19 + R4_19 * P19 ) * T1

      YCP( PNA ) = ( P21 + R21_5 * YCP( HO2 ) ) * L21_INV

      YCP( HONO ) = ( P19 + R19_4 * YCP( OH ) ) / L19

      RETURN

      END


