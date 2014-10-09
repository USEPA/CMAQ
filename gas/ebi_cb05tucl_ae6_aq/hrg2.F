
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
C  PRECONDITIONS: For the CB05TUCL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Sept. 10, 2014
C
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
      REAL    O1D_S                 ! sum of O1D loss frequencies
      REAL    OH_S                  ! stoich coeff for OH from O1D+H2O or H2
      REAL    HO2_S                 ! stoich coeff for HO2 from O1D+H2 rxn
      REAL    EXN_S                 ! sum of NO2EX loss frequencies
      REAL    XOH_S                 ! stoich coeff for OH & HONO from NO2EX loss rxn
      REAL    R4_19                 ! production of OH from HONO
      REAL    R19_4                 ! production of HONO from OH
      REAL    R4_5                  ! production of OH from HO2
      REAL    R5_4                  ! production of HO2 from OH
      REAL    R5_21                 ! production of HO2 from HNO4
      REAL    R21_5                 ! production of HNO4 from HO2
      REAL    P4, P5, P19, P21      ! species production form other paths
      REAL    L4, L5, L19, L21      ! species loss terms

      REAL    A, B, C               ! coeffs of quadratic eq. for HO2
      REAL    Q                     ! intermediate term

      REAL    T1, T2, T3            ! intermediate terms

      REAL    L21_INV               ! reciprocal of HNO4 loss term

C**********************************************************************


c..stoichiometric coefficient for production of HOX from O3 via O1D
      O1D_S =           RKI( NCELL,  10 )                    ! O1D=O
     &      +           RKI( NCELL,  11 )                    ! O1D=2*OH
     &      +           RKI( NCELL,  38 )                    ! O1D=OH+HO2

      OH_S =   2.000 * RKI( NCELL,  11 )                    ! O1D=2*OH
     &     +           RKI( NCELL,  38 )                    ! O1D=OH+HO2

      OH_S  = OH_S / O1D_S

      HO2_S =           RKI( NCELL,  38 )                    ! O1D=HO2+OH

      HO2_S  = HO2_S / O1D_S

c..Production of HO from HO2 (r4,5 terms )
      R4_5 =           RKI( NCELL,  13 ) * YC( NCELL, O3       )   ! HO2+O3=OH
     &     +           RKI( NCELL,  30 ) * YC( NCELL, NO       )   ! HO2+NO=OH+NO2
     &     +           RKI( NCELL,  44 ) * YC( NCELL, O        )   ! HO2+O=OH

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =           RKI( NCELL,  25 )                    ! HONO=OH+NO

      R4_19 = R4_19 * DTC

c..Remaining HO production
      P4 =   OH_S  * RXRAT( NCELL,   9 )      ! O1D=2*OH
     &   +   2.000 * RXRAT( NCELL,  36 )      ! H2O2=2*OH
     &   +           RXRAT( NCELL,  45 )      ! H2O2+O=OH+HO2
     &   +   0.390 * RXRAT( NCELL,  51 )      ! PNA=0.39*OH+0.61*HO2+...
     &   +           RXRAT( NCELL,  52 )      ! HNO3=OH+NO2
     &   +           RXRAT( NCELL,  64 )      ! ROOH=OH+HO2+0.5*ALD2+0.5*ALDX
     &   +           RXRAT( NCELL,  71 )      ! MEPX=OH+HO2+FORM
     &   +           RXRAT( NCELL,  76 )      ! FORM+O=OH+HO2+CO
     &   +           RXRAT( NCELL,  83 )      ! ALD2+O=OH+C2O3
     &   +           RXRAT( NCELL,  96 )      ! PACD=OH+MEO2
     &   +           RXRAT( NCELL,  98 )      ! ALDX+O=OH+CXO3
     &   +   0.100 * RXRAT( NCELL, 116 )      ! O+OLE=0.1*OH+0.3*HO2+...
     &   +   0.100 * RXRAT( NCELL, 118 )      ! O3+OLE=0.1*OH+0.44*HO2+...
     &   +   0.300 * RXRAT( NCELL, 120 )      ! O+ETH=0.3*OH+1.7*HO2+CO+...
     &   +   0.130 * RXRAT( NCELL, 122 )      ! O3+ETH=0.13*OH+0.13*HO2+...
     &   +   0.500 * RXRAT( NCELL, 126 )      ! IOLE+O3=0.5*OH+0.5*HO2+...
     &   +           RXRAT( NCELL, 141 )      ! CRPX=OH+CRNO
     &   +   0.080 * RXRAT( NCELL, 145 )      ! OPEN+O3=0.08*OH+0.76*HO2+...
     &   +   0.266 * RXRAT( NCELL, 159 )      ! O3+ISOP=0.266*OH+0.066*HO2+...
     &   +   0.268 * RXRAT( NCELL, 162 )      ! O3+ISPD=0.268*OH+0.154*HO2+...
     &   +   0.570 * RXRAT( NCELL, 167 )      ! TERP+O3=0.57*OH+0.07*HO2+...
     &   +           RXRAT( NCELL, 174 )      ! HOCL=OH+CL

      P4 = YC0( NCELL, OH ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =           RKI( NCELL,  12 ) * YC( NCELL, O3       )   ! OH+O3=HO2
     &     +           RKI( NCELL,  37 ) * YC( NCELL, H2O2     )   ! OH+H2O2=HO2
     &     +           RKI( NCELL,  39 )                    ! OH=HO2
     &     +           RKI( NCELL,  40 ) * YC( NCELL, O        )   ! OH+O=HO2
     &     +           RKI( NCELL,  47 ) * YC( NCELL, NO3      )   ! OH+NO3=HO2+NO2
     &     +           RKI( NCELL,  61 ) * YC( NCELL, NTR      )   ! OH+NTR=HO2+HNO3+...
     &     +           RKI( NCELL,  65 ) * YC( NCELL, CO       )   ! OH+CO=HO2
     &     +   0.300 * RKI( NCELL,  70 ) * YC( NCELL, MEPX     )   ! OH+MEPX=0.3*HO2+...
     &     +           RKI( NCELL,  72 ) * YC( NCELL, MEOH     )   ! OH+MEOH=HO2+FORM
     &     +           RKI( NCELL,  73 ) * YC( NCELL, FORM     )   ! OH+FORM=HO2+CO
     &     +           RKI( NCELL,  82 ) * YC( NCELL, FACD     )   ! OH+FACD=HO2
     &     +   0.110 * RKI( NCELL, 112 ) * YC( NCELL, PAR      )   ! OH+PAR=0.11*HO2+...
     &     +   0.950 * RKI( NCELL, 117 ) * YC( NCELL, OLE      )   ! OH+OLE=0.95*HO2+...
     &     +           RKI( NCELL, 121 ) * YC( NCELL, ETH      )   ! OH+ETH=HO2+...
     &     +           RKI( NCELL, 125 ) * YC( NCELL, IOLE     )   ! OH+IOLE=HO2+...
     &     +   0.280 * RKI( NCELL, 128 ) * YC( NCELL, TOL      )   ! OH+TOL=0.28*HO2+...
     &     +           RKI( NCELL, 131 ) * YC( NCELL, CRES     )   ! OH+CRES=1.12*HO2+...
     &     +   0.700 * RKI( NCELL, 154 ) * YC( NCELL, XYL      )   ! OH+XYL=0.7*HO2+...
     &     +   0.912 * RKI( NCELL, 158 ) * YC( NCELL, ISOP     )   ! OH+ISOP=...
     &     +   0.503 * RKI( NCELL, 161 ) * YC( NCELL, ISPD     )   ! OH+ISPD=...
     &     +   0.750 * RKI( NCELL, 166 ) * YC( NCELL, TERP     )   ! OH+TERP=0.75*HO2+...
     &     +           RKI( NCELL, 169 ) * YC( NCELL, SO2      )   ! OH+SO2=HO2+SULF+...
     &     +           RKI( NCELL, 170 ) * YC( NCELL, ETOH     )   ! OH+ETOH=HO2+...
     &     +           RKI( NCELL, 171 ) * YC( NCELL, ETHA     )   ! OH+ETHA=HO2+...

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =           RKI( NCELL,  32 )                    ! PNA=HO2+NO2
     &      +   0.610 * RKI( NCELL,  51 )                    ! PNA=0.61*HO2+...

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5 =   HO2_S  *RXRAT( NCELL,   9 )      ! O1D=HO2+OH
     &   +           RXRAT( NCELL,  45 )      ! H2O2+O=HO2+OH
     &   +           RXRAT( NCELL,  62 )      ! NTR=HO2+NO2+0.33*FORM+...
     &   +           RXRAT( NCELL,  64 )      ! ROOH=HO2+OH+0.5*ALD2+0.5*ALDX
     &   +           RXRAT( NCELL,  67 )      ! MEO2+NO=HO2+FORM+NO2
     &   +   0.740 * RXRAT( NCELL,  69 )      ! MEO2+MEO2=0.74*HO2+...
     &   +           RXRAT( NCELL,  71 )      ! MEPX=HO2+OH+FORM
     &   +   2.000 * RXRAT( NCELL,  74 )      ! FORM=2*HO2+CO
     &   +           RXRAT( NCELL,  76 )      ! FORM+O=HO2+OH+CO
     &   +           RXRAT( NCELL,  77 )      ! FORM+NO3=HO2+HNO3+CO
     &   +           RXRAT( NCELL,  79 )      ! HCO3=HO2+FORM
     &   +           RXRAT( NCELL,  80 )      ! HCO3+NO=HO2+NO2+FACD
     &   +           RXRAT( NCELL,  86 )      ! ALD2=HO2+CO+MEO2
     &   +   0.900 * RXRAT( NCELL,  92 )      ! C2O3+MEO2=0.9*HO2+0.9*MEO2+...
     &   +           RXRAT( NCELL, 101 )      ! ALDX=HO2+CO+MEO2
     &   +           RXRAT( NCELL, 102 )      ! CXO3+NO=HO2+NO2+ALD2+XO2
     &   +           RXRAT( NCELL, 108 )      ! CXO3+MEO2=HO2+0.9*XO2+...
     &   +   2.000 * RXRAT( NCELL, 110 )      ! CXO3+CXO3=2*HO2+2*XO2+2*ALD2
     &   +           RXRAT( NCELL, 111 )      ! CXO3+C2O3=HO2+XO2+MEO2+ALD2
     &   +   0.940 * RXRAT( NCELL, 113 )      ! ROR=0.94*HO2+0.6*ALD2+...
     &   +           RXRAT( NCELL, 114 )      ! ROR=HO2
     &   +   0.300 * RXRAT( NCELL, 116 )      ! O+OLE=0.3*HO2+0.1*OH+...
     &   +   0.440 * RXRAT( NCELL, 118 )      ! O3+OLE=0.44*HO2+0.1*OH+...
     &   +   1.700 * RXRAT( NCELL, 120 )      ! O+ETH=1.7*HO2+0.3*OH+CO+...
     &   +   0.130 * RXRAT( NCELL, 122 )      ! O3+ETH=0.13*HO2+0.13*OH+...
     &   +   0.100 * RXRAT( NCELL, 124 )      ! IOLE+O=0.1*HO2+0.66*ALDX+...
     &   +   0.500 * RXRAT( NCELL, 126 )      ! IOLE+O3=0.5*HO2+0.5*OH+...
     &   +           RXRAT( NCELL, 127 )      ! IOLE+NO3=HO2+0.64*ALDX+...
     &   +   1.200 * RXRAT( NCELL, 129 )      ! TO2+NO=1.2*HO2+0.86*NO2+...
     &   +   0.120 * RXRAT( NCELL, 131 )      ! OH+CRES=1.12*HO2+0.12*XO2+...
     &   +   0.360 * RXRAT( NCELL, 132 )      ! CRES+NO3=0.36*HO2+HNO3+...
     &   +           RXRAT( NCELL, 143 )      ! OPEN=HO2+OPO3+CO
     &   +   0.760 * RXRAT( NCELL, 145 )      ! OPEN+O3=0.76*HO2+0.08*OH+...
     &   +   1.200 * RXRAT( NCELL, 149 )      ! CAO2+NO=1.2*HO2+0.86*NO2+...
     &   +           RXRAT( NCELL, 151 )      ! OPO3+NO=HO2+XO2+NO2+ALDX
     &   +           RXRAT( NCELL, 156 )      ! MGLY=HO2+C2O3+CO
     &   +   0.250 * RXRAT( NCELL, 157 )      ! O+ISOP=0.25*HO2+0.5*FORM+...
     &   +   0.066 * RXRAT( NCELL, 159 )      ! O3+ISOP=0.066*HO2+0.266*OH+...
     &   +   0.800 * RXRAT( NCELL, 160 )      ! NO3+ISOP=0.8*HO2+0.8*NTR+...
     &   +   0.154 * RXRAT( NCELL, 162 )      ! O3+ISPD=0.154*HO2+0.268*OH+...
     &   +   0.925 * RXRAT( NCELL, 163 )      ! NO3+ISPD=0.925*HO2+...
     &   +   1.033 * RXRAT( NCELL, 164 )      ! ISPD=1.033*HO2+0.067*ALD2+...
     &   +   0.070 * RXRAT( NCELL, 167 )      ! TERP+O3=0.07*HO2+0.57*OH+...
     &   +   0.280 * RXRAT( NCELL, 168 )      ! TERP+NO3=0.28*HO2+0.47*NO2+...
     &   +   0.800 * RXRAT( NCELL, 172 )      ! NO2+ISOP=0.8*HO2+0.8*NTR+...
     &   +           RXRAT( NCELL, 180 )      ! FMCL=HO2+CO+CL
     &   +   0.110 * RXRAT( NCELL, 182 )      ! CL+PAR=0.11*HO2+0.87*XO2+...
     &   +           RXRAT( NCELL, 183 )      ! CL+ETHA=HO2+0.991*ALD2+...
     &   +           RXRAT( NCELL, 184 )      ! CL+ETH=HO2+2*XO2+FMCL+FORM
     &   +           RXRAT( NCELL, 185 )      ! CL+OLE=HO2+0.33*ALD2+...
     &   +           RXRAT( NCELL, 186 )      ! CL+IOLE=HO2+0.7*FMCL+...
     &   +           RXRAT( NCELL, 187 )      ! CL+ISOP=HO2+XO2+0.15*HCL+...
     &   +           RXRAT( NCELL, 188 )      ! CL+FORM=HO2+HCL+CO
     &   +           RXRAT( NCELL, 191 )      ! CL+MEOH=HO2+HCL+FORM
     &   +           RXRAT( NCELL, 192 )      ! CL+ETOH=HO2+HCL+ALD2
     &   +   0.880 * RXRAT( NCELL, 194 )      ! CL+TOL=0.88*HO2+0.88*XO2+...
     &   +   0.840 * RXRAT( NCELL, 195 )      ! CL+XYL=0.84*HO2+0.84*XO2+...

      P5 = YC0( NCELL, HO2 ) + P5 * DTC

c..Production of HONO from OH (r19,4 terms )

      R19_4 =   RKI( NCELL,  24 ) * YC( NCELL, NO       ) * DTC      ! OH+NO=HONO

c..Remaining HONO production terms
      P19 =   2.000 * RXRAT( NCELL,  23 )      ! NO+NO2=2*HONO
     &    +   0.500 * RXRAT( NCELL, 215 )      ! NO2=0.5*HONO+0.5*HNO3

      P19 = YC0( NCELL, HONO ) + P19 * DTC

c..Production of PNA from HO2 (r21,5 term )

      R21_5 =   RKI( NCELL,  31 ) * YC( NCELL, NO2      ) * DTC      ! HO2+NO2=HNO4

c..Remaining PNA production terms
      P21   =   YC0( NCELL, PNA )

c..OH loss terms not in R5_4 & R19_4
      L4 =           RKI( NCELL,  26 ) * YC( NCELL, HONO     )   ! OH+HONO=NO2
     &   +           RKI( NCELL,  28 ) * YC( NCELL, NO2      )   ! OH+NO2=HNO3
     &   +           RKI( NCELL,  29 ) * YC( NCELL, HNO3     )   ! OH+HNO3=NO3
     &   +           RKI( NCELL,  33 ) * YC( NCELL, PNA      )   ! OH+PNA=NO2
     &   +   2.000 * RKI( NCELL,  41 ) * YC( NCELL, OH       )   ! OH+OH=O
     &   +   2.000 * RKI( NCELL,  42 ) * YC( NCELL, OH       )   ! OH+OH=H2O2
     &   +           RKI( NCELL,  43 ) * YC( NCELL, HO2      )   ! OH+HO2=
     &   +           RKI( NCELL,  63 ) * YC( NCELL, ROOH     )   ! OH+ROOH=XO2+...
     &   +           RKI( NCELL,  66 )                    ! OH=MEO2
     &   +   0.700 * RKI( NCELL,  70 ) * YC( NCELL, MEPX     )   ! OH+MEPX=0.3*HO2+...
     &   +           RKI( NCELL,  84 ) * YC( NCELL, ALD2     )   ! OH+ALD2=C2O3
     &   +           RKI( NCELL,  95 ) * YC( NCELL, PACD     )   ! OH+PACD=C2O3
     &   +           RKI( NCELL,  97 ) * YC( NCELL, AACD     )   ! OH+AACD=MEO2
     &   +           RKI( NCELL,  99 ) * YC( NCELL, ALDX     )   ! OH+ALDX=CXO3
     &   +           RKI( NCELL, 106 ) * YC( NCELL, PANX     )   ! OH+PANX=ALD2+NO2
     &   +   0.890 * RKI( NCELL, 112 ) * YC( NCELL, PAR      )   ! OH+PAR=0.11*HO2+...
     &   +   0.050 * RKI( NCELL, 117 ) * YC( NCELL, OLE      )   ! OH+OLE=0.95*HO2+...
     &   +   0.648 * RKI( NCELL, 128 ) * YC( NCELL, TOL      )   ! OH+TOL=0.28*HO2+...
     &   +           RKI( NCELL, 135 ) * YC( NCELL, CRON     )   ! OH+CRON=CRNO
     &   +           RKI( NCELL, 142 ) * YC( NCELL, CRPX     )   ! OH+CRPX=CRN2
     &   +           RKI( NCELL, 144 ) * YC( NCELL, OPEN     )   ! OH+OPEN=0.6*OPO3+...
     &   +           RKI( NCELL, 147 ) * YC( NCELL, CAT1     )   ! OH+CAT1=CAO2
     &   +   0.300 * RKI( NCELL, 154 ) * YC( NCELL, XYL      )   ! OH+XYL=0.7*HO2+...
     &   +           RKI( NCELL, 155 ) * YC( NCELL, MGLY     )   ! OH+MGLY=XO2+C2O3
     &   +   0.088 * RKI( NCELL, 158 ) * YC( NCELL, ISOP     )   ! OH+ISOP=...
     &   +   0.497 * RKI( NCELL, 161 ) * YC( NCELL, ISPD     )   ! OH+ISPD=...
     &   +   0.250 * RKI( NCELL, 166 ) * YC( NCELL, TERP     )   ! OH+TERP=0.75*HO2+...
     &   +           RKI( NCELL, 179 ) * YC( NCELL, FMCL     )   ! OH+FMCL=CL+CO
     &   +           RKI( NCELL, 193 ) * YC( NCELL, HCL      )   ! OH+HCL=CL

      L4    = 1.0 + L4 * DTC + R5_4 + R19_4

c..HO2 loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5 =           RKI( NCELL,  43 ) * YC( NCELL, OH       )   ! HO2+OH=
     &   +           RKI( NCELL,  48 ) * YC( NCELL, NO3      )   ! HO2+NO3=HNO3
     &   +           RKI( NCELL,  56 ) * YC( NCELL, XO2      )   ! HO2+XO2=ROOH
     &   +           RKI( NCELL,  57 ) * YC( NCELL, XO2N     )   ! HO2+XO2N=ROOH
     &   +           RKI( NCELL,  68 ) * YC( NCELL, MEO2     )   ! HO2+MEO2=MEPX
     &   +           RKI( NCELL,  78 ) * YC( NCELL, FORM     )   ! HO2+FORM=HCO3
     &   +           RKI( NCELL,  81 ) * YC( NCELL, HCO3     )   ! HO2+HCO3=MEPX
     &   +           RKI( NCELL,  91 ) * YC( NCELL, C2O3     )   ! HO2+C2O3=...
     &   +           RKI( NCELL, 107 ) * YC( NCELL, CXO3     )   ! HO2+CXO3=...
     &   +           RKI( NCELL, 130 ) * YC( NCELL, TO2      )   ! HO2+TO2=
     &   +           RKI( NCELL, 134 ) * YC( NCELL, CRO      )   ! HO2+CRO=CRES
     &   +           RKI( NCELL, 140 ) * YC( NCELL, CRN2     )   ! HO2+CRN2=CRPX
     &   +           RKI( NCELL, 150 ) * YC( NCELL, CAO2     )   ! HO2+CAO2=
     &   +           RKI( NCELL, 178 ) * YC( NCELL, CLO      )   ! HO2+CLO=HOCL

      L5    = 1.0 + L5 * DTC + R4_5 + R21_5

c..HONO loss terms not included in R4_19
      L19 =           RKI( NCELL,  26 ) * YC( NCELL, OH       )   ! HONO+OH=NO2
     &    +   2.000 * RKI( NCELL,  27 ) * YC( NCELL, HONO     )   ! HONO+HONO=NO+NO2

      L19   = 1.0 + L19 * DTC + R4_19

c..PNA loss terms not inluded in R5_21
      L21 =           RKI( NCELL,  33 ) * YC( NCELL, OH       )   ! PNA+OH=NO2
     &    +   0.390 * RKI( NCELL,  51 )                    ! PNA=0.61*HO2+...

      L21   = 1.0 + L21 * DTC + R5_21

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute terms used to calculate a,b & c
      L21_INV = 1.0 / L21
      T1 = 1.0 / ( L4 * L19 - R4_19 * R19_4 )
      T2 = R5_4 * T1
      T3 = R5_21 * L21_INV

c..solve quadratic equation for HO2
      A = 2.0 * ( RKI( NCELL,  34 ) + RKI( NCELL,  35 ) ) * DTC

      B = L5 - T3 * R21_5 - T2 * R4_5 * L19

      C = P5 + T3 * P21 + T2 * ( P4 * L19 + P19 * R4_19 )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP( NCELL, HO2 ) = MAX( Q / A , -C / Q  )

c..compute remaining species concentrations
      YCP( NCELL, OH ) = ( ( P4 + R4_5 * YCP( NCELL, HO2 ) ) * L19 + R4_19 * P19 ) * T1

      YCP( NCELL, PNA ) = ( P21 + R21_5 * YCP( NCELL, HO2 ) ) * L21_INV

      YCP( NCELL, HONO ) = ( P19 + R19_4 * YCP( NCELL, OH ) ) / L19

      RETURN

      END


