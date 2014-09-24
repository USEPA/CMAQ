
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

      SUBROUTINE HRG2( DTC )
C**********************************************************************
C
C  FUNCTION: To solve for the concentration of HO, HO2, HONO, and
C            HNO4 alebraically.
C
C  PRECONDITIONS: For the CB05CL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, July 18, 2014
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
      CHARACTER( 16 ), SAVE :: PNAME = 'HRG2'             ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) ::    O1D_S                 ! sum of O1D loss frequencies
      REAL( 8 ) ::    OH_S                  ! stoich coeff for OH from O1D+H2O or H2
      REAL( 8 ) ::    HO2_S                 ! stoich coeff for HO2 from O1D+H2 rxn
      REAL( 8 ) ::    EXN_S                 ! sum of NO2EX loss frequencies
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
      O1D_S =                 RKI( NCELL,  10 )                   ! O1D=O
     &      +                 RKI( NCELL,  11 )                   ! O1D=0.2000D+01*OH
     &      +                 RKI( NCELL,  38 )                   ! O1D=OH+HO2

      OH_S =    2.0000D+00 * RKI( NCELL,  11 )                   ! O1D=0.2000D+01*OH
     &     +                 RKI( NCELL,  38 )                   ! O1D=OH+HO2

      OH_S  = OH_S / O1D_S

      HO2_S =                 RKI( NCELL,  38 )                   ! O1D=HO2+OH

      HO2_S  = HO2_S / O1D_S

c..Production of HO from HO2 (r4,5 terms )
      R4_5 =                 RKI( NCELL,  13 ) * YC( NCELL, O3      )   ! HO2+O3=OH
     &     +                 RKI( NCELL,  30 ) * YC( NCELL, NO      )   ! HO2+NO=OH+NO2
     &     +                 RKI( NCELL,  44 ) * YC( NCELL, O       )   ! HO2+O=OH

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =                 RKI( NCELL,  25 )                   ! HONO=OH+NO

      R4_19 = R4_19 * DTC

c..Remaining HO production
      P4 =   OH_S  *       RXRAT( NCELL,   9 )      ! O1D=0.2000D+01*OH
     &   +    2.0000D+00 * RXRAT( NCELL,  36 )      ! H2O2=0.2000D+01*OH
     &   +                 RXRAT( NCELL,  45 )      ! H2O2+O=OH+HO2
     &   +    3.9000D-01 * RXRAT( NCELL,  51 )      ! PNA=0.3900D+00*OH+0.6100D+...
     &   +                 RXRAT( NCELL,  52 )      ! HNO3=OH+NO2
     &   +                 RXRAT( NCELL,  64 )      ! ROOH=OH+HO2+0.5000D+...
     &   +                 RXRAT( NCELL,  71 )      ! MEPX=OH+HO2+FORM
     &   +                 RXRAT( NCELL,  76 )      ! FORM+O=OH+HO2+CO
     &   +                 RXRAT( NCELL,  83 )      ! ALD2+O=OH+C2O3
     &   +                 RXRAT( NCELL,  96 )      ! PACD=OH+MEO2
     &   +                 RXRAT( NCELL,  98 )      ! ALDX+O=OH+CXO3
     &   +    1.0000D-01 * RXRAT( NCELL, 116 )      ! O+OLE=0.1000D+00*OH+...
     &   +    1.0000D-01 * RXRAT( NCELL, 118 )      ! O3+OLE=0.1000D+00*OH+...
     &   +    3.0000D-01 * RXRAT( NCELL, 120 )      ! O+ETH=0.3000D+00*OH+...
     &   +    1.3000D-01 * RXRAT( NCELL, 122 )      ! O3+ETH=0.1300D+00*OH+...
     &   +    5.0000D-01 * RXRAT( NCELL, 126 )      ! IOLE+O3=0.5000D+00*OH+...
     &   +    8.0000D-02 * RXRAT( NCELL, 137 )      ! OPEN+O3=0.8000D-01*OH+...
     &   +    2.6600D-01 * RXRAT( NCELL, 143 )      ! O3+ISOP=0.2660D+00*OH+...
     &   +    2.6800D-01 * RXRAT( NCELL, 146 )      ! O3+ISPD=0.2680D+00*OH+...
     &   +    5.7000D-01 * RXRAT( NCELL, 151 )      ! TERP+O3=0.5700D+00*OH+...
     &   +                 RXRAT( NCELL, 158 )      ! HOCL=OH+CL

      P4 = YC0( NCELL, OH ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =                 RKI( NCELL,  12 ) * YC( NCELL, O3      )   ! OH+O3=HO2
     &     +                 RKI( NCELL,  37 ) * YC( NCELL, H2O2    )   ! OH+H2O2=HO2
     &     +                 RKI( NCELL,  39 )                   ! OH=HO2
     &     +                 RKI( NCELL,  40 ) * YC( NCELL, O       )   ! OH+O=HO2
     &     +                 RKI( NCELL,  47 ) * YC( NCELL, NO3     )   ! OH+NO3=HO2+NO2
     &     +                 RKI( NCELL,  61 ) * YC( NCELL, NTR     )   ! OH+NTR=HO2+HNO3+...
     &     +                 RKI( NCELL,  65 ) * YC( NCELL, CO      )   ! OH+CO=HO2
     &     +    3.0000D-01 * RKI( NCELL,  70 ) * YC( NCELL, MEPX    )   ! OH+MEPX=0.3000D+...
     &     +                 RKI( NCELL,  72 ) * YC( NCELL, MEOH    )   ! OH+MEOH=HO2+FORM
     &     +                 RKI( NCELL,  73 ) * YC( NCELL, FORM    )   ! OH+FORM=HO2+CO
     &     +                 RKI( NCELL,  82 ) * YC( NCELL, FACD    )   ! OH+FACD=HO2
     &     +    1.1000D-01 * RKI( NCELL, 112 ) * YC( NCELL, PAR     )   ! OH+PAR=0.1100D+...
     &     +    9.5000D-01 * RKI( NCELL, 117 ) * YC( NCELL, OLE     )   ! OH+OLE=0.9500D+...
     &     +                 RKI( NCELL, 121 ) * YC( NCELL, ETH     )   ! OH+ETH=HO2+...
     &     +                 RKI( NCELL, 125 ) * YC( NCELL, IOLE    )   ! OH+IOLE=HO2+...
     &     +    4.4000D-01 * RKI( NCELL, 128 ) * YC( NCELL, TOL     )   ! OH+TOL=0.4400D+...
     &     +    6.0000D-01 * RKI( NCELL, 131 ) * YC( NCELL, CRES    )   ! OH+CRES=0.6000D+...
     &     +                 RKI( NCELL, 136 ) * YC( NCELL, OPEN    )   ! OH+OPEN=0.2000D+...
     &     +    7.0000D-01 * RKI( NCELL, 138 ) * YC( NCELL, XYL     )   ! OH+XYL=0.7000D+...
     &     +    9.1200D-01 * RKI( NCELL, 142 ) * YC( NCELL, ISOP    )   ! OH+ISOP=0.9120D+...
     &     +    5.0300D-01 * RKI( NCELL, 145 ) * YC( NCELL, ISPD    )   ! OH+ISPD=0.5030D+...
     &     +    7.5000D-01 * RKI( NCELL, 150 ) * YC( NCELL, TERP    )   ! OH+TERP=0.7500D+...
     &     +                 RKI( NCELL, 153 ) * YC( NCELL, SO2     )   ! OH+SO2=HO2+SULF+...
     &     +                 RKI( NCELL, 154 ) * YC( NCELL, ETOH    )   ! OH+ETOH=HO2+...
     &     +                 RKI( NCELL, 155 ) * YC( NCELL, ETHA    )   ! OH+ETHA=HO2+...

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =                 RKI( NCELL,  32 )                   ! PNA=HO2+NO2
     &      +    6.1000D-01 * RKI( NCELL,  51 )                   ! PNA=0.6100D+...

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5 =   HO2_S  *      RXRAT( NCELL,   9 )      ! O1D=HO2+OH
     &   +                 RXRAT( NCELL,  45 )      ! H2O2+O=HO2+OH
     &   +                 RXRAT( NCELL,  62 )      ! NTR=HO2+NO2+0.3300D+...
     &   +                 RXRAT( NCELL,  64 )      ! ROOH=HO2+OH+0.5000D+...
     &   +                 RXRAT( NCELL,  67 )      ! MEO2+NO=HO2+FORM+NO2
     &   +    7.4000D-01 * RXRAT( NCELL,  69 )      ! MEO2+MEO2=0.7400D+00*HO2+...
     &   +                 RXRAT( NCELL,  71 )      ! MEPX=HO2+OH+FORM
     &   +    2.0000D+00 * RXRAT( NCELL,  74 )      ! FORM=0.2000D+01*HO2+CO
     &   +                 RXRAT( NCELL,  76 )      ! FORM+O=HO2+OH+CO
     &   +                 RXRAT( NCELL,  77 )      ! FORM+NO3=HO2+HNO3+CO
     &   +                 RXRAT( NCELL,  79 )      ! HCO3=HO2+FORM
     &   +                 RXRAT( NCELL,  80 )      ! HCO3+NO=HO2+NO2+FACD
     &   +                 RXRAT( NCELL,  86 )      ! ALD2=HO2+CO+MEO2
     &   +    9.0000D-01 * RXRAT( NCELL,  92 )      ! C2O3+MEO2=0.9000D+00*HO2+...
     &   +                 RXRAT( NCELL, 101 )      ! ALDX=HO2+CO+MEO2
     &   +                 RXRAT( NCELL, 102 )      ! CXO3+NO=HO2+NO2+ALD2+XO2
     &   +                 RXRAT( NCELL, 108 )      ! CXO3+MEO2=HO2+0.9000D+...
     &   +    2.0000D+00 * RXRAT( NCELL, 110 )      ! CXO3+CXO3=0.2000D+01*HO2+...
     &   +                 RXRAT( NCELL, 111 )      ! CXO3+C2O3=HO2+XO2+MEO2+ALD2
     &   +    9.4000D-01 * RXRAT( NCELL, 113 )      ! ROR=0.9400D+00*HO2+0.6000D+...
     &   +                 RXRAT( NCELL, 114 )      ! ROR=HO2
     &   +    3.0000D-01 * RXRAT( NCELL, 116 )      ! O+OLE=0.3000D+00*HO2+...
     &   +    4.4000D-01 * RXRAT( NCELL, 118 )      ! O3+OLE=0.4400D+00*HO2+...
     &   +    1.7000D+00 * RXRAT( NCELL, 120 )      ! O+ETH=0.1700D+01*HO2+...
     &   +    1.3000D-01 * RXRAT( NCELL, 122 )      ! O3+ETH=0.1300D+00*HO2+...
     &   +    1.0000D-01 * RXRAT( NCELL, 124 )      ! IOLE+O=0.1000D+00*HO2+...
     &   +    5.0000D-01 * RXRAT( NCELL, 126 )      ! IOLE+O3=0.5000D+00*HO2+...
     &   +                 RXRAT( NCELL, 127 )      ! IOLE+NO3=HO2+0.6400D+...
     &   +    9.0000D-01 * RXRAT( NCELL, 129 )      ! TO2+NO=0.9000D+00*HO2+...
     &   +                 RXRAT( NCELL, 130 )      ! TO2=HO2+CRES
     &   +                 RXRAT( NCELL, 135 )      ! OPEN=HO2+C2O3+CO
     &   +                 RXRAT( NCELL, 136 )      ! OPEN+OH=0.2000D+01*HO2+...
     &   +    7.6000D-01 * RXRAT( NCELL, 137 )      ! OPEN+O3=0.7600D+00*HO2+...
     &   +                 RXRAT( NCELL, 140 )      ! MGLY=HO2+C2O3+CO
     &   +    2.5000D-01 * RXRAT( NCELL, 141 )      ! O+ISOP=0.2500D+00*HO2+...
     &   +    6.6000D-02 * RXRAT( NCELL, 143 )      ! O3+ISOP=0.6600D-01*HO2+...
     &   +    8.0000D-01 * RXRAT( NCELL, 144 )      ! NO3+ISOP=0.8000D+00*HO2+...
     &   +    1.5400D-01 * RXRAT( NCELL, 146 )      ! O3+ISPD=0.1540D+00*HO2+...
     &   +    9.2500D-01 * RXRAT( NCELL, 147 )      ! NO3+ISPD=0.9250D+00*HO2+...
     &   +    1.0330D+00 * RXRAT( NCELL, 148 )      ! ISPD=0.1033D+01*HO2+...
     &   +    7.0000D-02 * RXRAT( NCELL, 151 )      ! TERP+O3=0.7000D-01*HO2+...
     &   +    2.8000D-01 * RXRAT( NCELL, 152 )      ! TERP+NO3=0.2800D+00*HO2+...
     &   +    8.0000D-01 * RXRAT( NCELL, 156 )      ! NO2+ISOP=0.8000D+00*HO2+...
     &   +                 RXRAT( NCELL, 164 )      ! FMCL=HO2+CO+CL
     &   +    1.1000D-01 * RXRAT( NCELL, 166 )      ! CL+PAR=0.1100D+00*HO2+...
     &   +                 RXRAT( NCELL, 167 )      ! CL+ETHA=HO2+0.9910D+...
     &   +                 RXRAT( NCELL, 168 )      ! CL+ETH=HO2+0.2000D+01*XO2+...
     &   +                 RXRAT( NCELL, 169 )      ! CL+OLE=HO2+0.3300D+00*ALD2+...
     &   +                 RXRAT( NCELL, 170 )      ! CL+IOLE=HO2+0.7000D+...
     &   +                 RXRAT( NCELL, 171 )      ! CL+ISOP=HO2+XO2+0.1500D+...
     &   +                 RXRAT( NCELL, 172 )      ! CL+FORM=HO2+HCL+CO
     &   +                 RXRAT( NCELL, 175 )      ! CL+MEOH=HO2+HCL+FORM
     &   +                 RXRAT( NCELL, 176 )      ! CL+ETOH=HO2+HCL+ALD2

      P5 = YC0( NCELL, HO2 ) + P5 * DTC

c..Production of HONO from OH (r19,4 terms )

      R19_4 =   RKI( NCELL,  24 ) * YC( NCELL, NO      ) * DTC      ! OH+NO=HONO

c..Remaining HONO production terms
      P19 =    2.0000D+00 * RXRAT( NCELL,  23 )      ! NO+NO2=0.2000D+01*HONO
     &    +    5.0000D-01 * RXRAT( NCELL, 189 )      ! NO2=0.5000D+00*HONO+...

      P19 = YC0( NCELL, HONO ) + P19 * DTC

c..Production of PNA from HO2 (r21,5 term )

      R21_5 =   RKI( NCELL,  31 ) * YC( NCELL, NO2     ) * DTC      ! HO2+NO2=HNO4

c..Remaining PNA production terms
      P21   =   YC0( NCELL, PNA )

c..OH loss terms not in R5_4 & R19_4
      L4 =                 RKI( NCELL,  26 ) * YC( NCELL, HONO    )   ! OH+HONO=NO2
     &   +                 RKI( NCELL,  28 ) * YC( NCELL, NO2     )   ! OH+NO2=HNO3
     &   +                 RKI( NCELL,  29 ) * YC( NCELL, HNO3    )   ! OH+HNO3=NO3
     &   +                 RKI( NCELL,  33 ) * YC( NCELL, PNA     )   ! OH+PNA=NO2
     &   +    2.0000D+00 * RKI( NCELL,  41 ) * YC( NCELL, OH      )   ! OH+OH=O
     &   +    2.0000D+00 * RKI( NCELL,  42 ) * YC( NCELL, OH      )   ! OH+OH=H2O2
     &   +                 RKI( NCELL,  43 ) * YC( NCELL, HO2     )   ! OH+HO2=
     &   +                 RKI( NCELL,  63 ) * YC( NCELL, ROOH    )   ! OH+ROOH=XO2+...
     &   +                 RKI( NCELL,  66 )                   ! OH=MEO2
     &   +    7.0000D-01 * RKI( NCELL,  70 ) * YC( NCELL, MEPX    )   ! OH+MEPX=0.3000D+...
     &   +                 RKI( NCELL,  84 ) * YC( NCELL, ALD2    )   ! OH+ALD2=C2O3
     &   +                 RKI( NCELL,  95 ) * YC( NCELL, PACD    )   ! OH+PACD=C2O3
     &   +                 RKI( NCELL,  97 ) * YC( NCELL, AACD    )   ! OH+AACD=MEO2
     &   +                 RKI( NCELL,  99 ) * YC( NCELL, ALDX    )   ! OH+ALDX=CXO3
     &   +                 RKI( NCELL, 106 ) * YC( NCELL, PANX    )   ! OH+PANX=ALD2+NO2
     &   +    8.9000D-01 * RKI( NCELL, 112 ) * YC( NCELL, PAR     )   ! OH+PAR=0.1100D+...
     &   +    5.0000D-02 * RKI( NCELL, 117 ) * YC( NCELL, OLE     )   ! OH+OLE=0.9500D+...
     &   +    5.6000D-01 * RKI( NCELL, 128 ) * YC( NCELL, TOL     )   ! OH+TOL=0.4400D+...
     &   +    4.0000D-01 * RKI( NCELL, 131 ) * YC( NCELL, CRES    )   ! OH+CRES=0.6000D+...
     &   +    3.0000D-01 * RKI( NCELL, 138 ) * YC( NCELL, XYL     )   ! OH+XYL=0.7000D+...
     &   +                 RKI( NCELL, 139 ) * YC( NCELL, MGLY    )   ! OH+MGLY=XO2+C2O3
     &   +    8.8000D-02 * RKI( NCELL, 142 ) * YC( NCELL, ISOP    )   ! OH+ISOP=0.9120D+...
     &   +    4.9700D-01 * RKI( NCELL, 145 ) * YC( NCELL, ISPD    )   ! OH+ISPD=0.5030D+...
     &   +    2.5000D-01 * RKI( NCELL, 150 ) * YC( NCELL, TERP    )   ! OH+TERP=0.7500D+...
     &   +                 RKI( NCELL, 163 ) * YC( NCELL, FMCL    )   ! OH+FMCL=CL+CO
     &   +                 RKI( NCELL, 177 ) * YC( NCELL, HCL     )   ! OH+HCL=CL

      L4    = 1.0D0 + L4 * DTC + R5_4 + R19_4

c..HO2 loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5 =                 RKI( NCELL,  43 ) * YC( NCELL, OH      )   ! HO2+OH=
     &   +                 RKI( NCELL,  48 ) * YC( NCELL, NO3     )   ! HO2+NO3=HNO3
     &   +                 RKI( NCELL,  56 ) * YC( NCELL, XO2     )   ! HO2+XO2=ROOH
     &   +                 RKI( NCELL,  57 ) * YC( NCELL, XO2N    )   ! HO2+XO2N=ROOH
     &   +                 RKI( NCELL,  68 ) * YC( NCELL, MEO2    )   ! HO2+MEO2=MEPX
     &   +                 RKI( NCELL,  78 ) * YC( NCELL, FORM    )   ! HO2+FORM=HCO3
     &   +                 RKI( NCELL,  81 ) * YC( NCELL, HCO3    )   ! HO2+HCO3=MEPX
     &   +                 RKI( NCELL,  91 ) * YC( NCELL, C2O3    )   ! HO2+C2O3=0.8000D+...
     &   +                 RKI( NCELL, 107 ) * YC( NCELL, CXO3    )   ! HO2+CXO3=0.8000D+...
     &   +                 RKI( NCELL, 134 ) * YC( NCELL, CRO     )   ! HO2+CRO=CRES
     &   +                 RKI( NCELL, 162 ) * YC( NCELL, CLO     )   ! HO2+CLO=HOCL

      L5    = 1.0D0 + L5 * DTC + R4_5 + R21_5

c..HONO loss terms not included in R4_19
      L19 =                 RKI( NCELL,  26 ) * YC( NCELL, OH      )   ! HONO+OH=NO2
     &    +    2.0000D+00 * RKI( NCELL,  27 ) * YC( NCELL, HONO    )   ! HONO+HONO=NO+NO2

      L19   = 1.0D0 + L19 * DTC + R4_19

c..PNA loss terms not inluded in R5_21
      L21 =                 RKI( NCELL,  33 ) * YC( NCELL, OH      )   ! PNA+OH=NO2
     &    +    3.9000D-01 * RKI( NCELL,  51 )                   ! PNA=0.6100D+...

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
      A = 2.0D0 * ( RKI( NCELL,  34 ) + RKI( NCELL,  35 ) ) * DTC

      B = L5 - T3 * R21_5 - T2 * R4_5 * L19

      C = P5 + T3 * P21 + T2 * ( P4 * L19 + P19 * R4_19 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( NCELL, HO2 ) = MAX( Q / A , -C / Q  )

c..compute remaining species concentrations
      YCP( NCELL, OH ) = ( ( P4 + R4_5 * YCP( NCELL, HO2 ) ) * L19 + R4_19 * P19 ) * T1

      YCP( NCELL, PNA ) = ( P21 + R21_5 * YCP( NCELL, HO2 ) ) * L21_INV

      YCP( NCELL, HONO ) = ( P19 + R19_4 * YCP( NCELL, OH ) ) / L19

      RETURN

      END


