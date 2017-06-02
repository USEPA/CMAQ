
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
C  REVISION HISTORY: Created by EBI solver program, May 26, 2017
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
      O1D_S =                 RKI(  10 )                    ! O1D=O
     &      +                 RKI(  11 )                    ! O1D=0.2000D+01*OH
     &      +                 RKI(  38 )                    ! O1D=OH+HO2

      OH_S =    2.0000D+00 * RKI(  11 )                    ! O1D=0.2000D+01*OH
     &     +                 RKI(  38 )                    ! O1D=OH+HO2

      OH_S  = OH_S / O1D_S

      HO2_S =                 RKI(  38 )                    ! O1D=HO2+OH

      HO2_S  = HO2_S / O1D_S

c..Production of HO from HO2 (r4,5 terms )
      R4_5 =                 RKI(  13 ) * YC( O3       )   ! HO2+O3=OH
     &     +                 RKI(  30 ) * YC( NO       )   ! HO2+NO=OH+NO2
     &     +                 RKI(  44 ) * YC( O        )   ! HO2+O=OH

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =                 RKI(  25 )                    ! HONO=OH+NO

      R4_19 = R4_19 * DTC

c..Remaining HO production
      P4 =   OH_S  *       RXRAT(   9 )      ! O1D=0.2000D+01*OH
     &   +    2.0000D+00 * RXRAT(  36 )      ! H2O2=0.2000D+01*OH
     &   +                 RXRAT(  45 )      ! H2O2+O=OH+HO2
     &   +    3.9000D-01 * RXRAT(  51 )      ! PNA=0.3900D+00*OH+0.6100D+...
     &   +                 RXRAT(  52 )      ! HNO3=OH+NO2
     &   +                 RXRAT(  64 )      ! ROOH=OH+HO2+0.5000D+...
     &   +                 RXRAT(  71 )      ! MEPX=OH+HO2+FORM
     &   +                 RXRAT(  76 )      ! FORM+O=OH+HO2+CO
     &   +                 RXRAT(  83 )      ! ALD2+O=OH+C2O3
     &   +                 RXRAT(  96 )      ! PACD=OH+MEO2
     &   +                 RXRAT(  98 )      ! ALDX+O=OH+CXO3
     &   +    1.0000D-01 * RXRAT( 116 )      ! O+OLE=0.1000D+00*OH+...
     &   +    1.0000D-01 * RXRAT( 118 )      ! O3+OLE=0.1000D+00*OH+...
     &   +    3.0000D-01 * RXRAT( 120 )      ! O+ETH=0.3000D+00*OH+...
     &   +    1.3000D-01 * RXRAT( 122 )      ! O3+ETH=0.1300D+00*OH+...
     &   +    5.0000D-01 * RXRAT( 126 )      ! IOLE+O3=0.5000D+00*OH+...
     &   +                 RXRAT( 141 )      ! CRPX=OH+CRNO
     &   +    8.0000D-02 * RXRAT( 145 )      ! OPEN+O3=0.8000D-01*OH+...
     &   +    2.6600D-01 * RXRAT( 160 )      ! O3+ISOP=0.2660D+00*OH+...
     &   +    2.6800D-01 * RXRAT( 163 )      ! O3+ISPD=0.2680D+00*OH+...
     &   +    5.7000D-01 * RXRAT( 168 )      ! TERP+O3=0.5700D+00*OH+...
     &   +                 RXRAT( 175 )      ! HOCL=OH+CL

      P4 = YC0( OH ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =                 RKI(  12 ) * YC( O3       )   ! OH+O3=HO2
     &     +                 RKI(  37 ) * YC( H2O2     )   ! OH+H2O2=HO2
     &     +                 RKI(  39 )                    ! OH=HO2
     &     +                 RKI(  40 ) * YC( O        )   ! OH+O=HO2
     &     +                 RKI(  47 ) * YC( NO3      )   ! OH+NO3=HO2+NO2
     &     +                 RKI(  61 ) * YC( NTR      )   ! OH+NTR=HO2+HNO3+...
     &     +                 RKI(  65 ) * YC( CO       )   ! OH+CO=HO2
     &     +    3.0000D-01 * RKI(  70 ) * YC( MEPX     )   ! OH+MEPX=0.3000D+...
     &     +                 RKI(  72 ) * YC( MEOH     )   ! OH+MEOH=HO2+FORM
     &     +                 RKI(  73 ) * YC( FORM     )   ! OH+FORM=HO2+CO
     &     +                 RKI(  82 ) * YC( FACD     )   ! OH+FACD=HO2
     &     +    1.1000D-01 * RKI( 112 ) * YC( PAR      )   ! OH+PAR=0.1100D+...
     &     +    9.5000D-01 * RKI( 117 ) * YC( OLE      )   ! OH+OLE=0.9500D+...
     &     +                 RKI( 121 ) * YC( ETH      )   ! OH+ETH=HO2+...
     &     +                 RKI( 125 ) * YC( IOLE     )   ! OH+IOLE=HO2+...
     &     +    2.8000D-01 * RKI( 128 ) * YC( TOL      )   ! OH+TOL=0.2800D+...
     &     +                 RKI( 131 ) * YC( CRES     )   ! OH+CRES=0.1120D+...
     &     +    7.0000D-01 * RKI( 154 ) * YC( XYLMN    )   ! OH+XYLMN=0.7000D+...
     &     +    7.0000D-01 * RKI( 155 ) * YC( NAPH     )   ! OH+NAPH=0.7000D+...
     &     +    9.1200D-01 * RKI( 159 ) * YC( ISOP     )   ! OH+ISOP=0.9120D+...
     &     +    5.0300D-01 * RKI( 162 ) * YC( ISPD     )   ! OH+ISPD=0.5030D+...
     &     +    7.5000D-01 * RKI( 167 ) * YC( TERP     )   ! OH+TERP=0.7500D+...
     &     +                 RKI( 170 ) * YC( SO2      )   ! OH+SO2=HO2+SULF+...
     &     +                 RKI( 171 ) * YC( ETOH     )   ! OH+ETOH=HO2+...
     &     +                 RKI( 172 ) * YC( ETHA     )   ! OH+ETHA=HO2+...

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =                 RKI(  32 )                    ! PNA=HO2+NO2
     &      +    6.1000D-01 * RKI(  51 )                    ! PNA=0.6100D+...

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5 =   HO2_S  *      RXRAT(   9 )      ! O1D=HO2+OH
     &   +                 RXRAT(  45 )      ! H2O2+O=HO2+OH
     &   +                 RXRAT(  62 )      ! NTR=HO2+NO2+0.3300D+...
     &   +                 RXRAT(  64 )      ! ROOH=HO2+OH+0.5000D+...
     &   +                 RXRAT(  67 )      ! MEO2+NO=HO2+FORM+NO2
     &   +    7.4000D-01 * RXRAT(  69 )      ! MEO2+MEO2=0.7400D+00*HO2+...
     &   +                 RXRAT(  71 )      ! MEPX=HO2+OH+FORM
     &   +    2.0000D+00 * RXRAT(  74 )      ! FORM=0.2000D+01*HO2+CO
     &   +                 RXRAT(  76 )      ! FORM+O=HO2+OH+CO
     &   +                 RXRAT(  77 )      ! FORM+NO3=HO2+HNO3+CO
     &   +                 RXRAT(  79 )      ! HCO3=HO2+FORM
     &   +                 RXRAT(  80 )      ! HCO3+NO=HO2+NO2+FACD
     &   +                 RXRAT(  86 )      ! ALD2=HO2+CO+MEO2
     &   +    9.0000D-01 * RXRAT(  92 )      ! C2O3+MEO2=0.9000D+00*HO2+...
     &   +                 RXRAT( 101 )      ! ALDX=HO2+CO+MEO2
     &   +                 RXRAT( 102 )      ! CXO3+NO=HO2+NO2+ALD2+XO2
     &   +                 RXRAT( 108 )      ! CXO3+MEO2=HO2+0.9000D+...
     &   +    2.0000D+00 * RXRAT( 110 )      ! CXO3+CXO3=0.2000D+01*HO2+...
     &   +                 RXRAT( 111 )      ! CXO3+C2O3=HO2+XO2+MEO2+ALD2
     &   +    9.4000D-01 * RXRAT( 113 )      ! ROR=0.9400D+00*HO2+0.6000D+...
     &   +                 RXRAT( 114 )      ! ROR=HO2
     &   +    3.0000D-01 * RXRAT( 116 )      ! O+OLE=0.3000D+00*HO2+...
     &   +    4.4000D-01 * RXRAT( 118 )      ! O3+OLE=0.4400D+00*HO2+...
     &   +    1.7000D+00 * RXRAT( 120 )      ! O+ETH=0.1700D+01*HO2+...
     &   +    1.3000D-01 * RXRAT( 122 )      ! O3+ETH=0.1300D+00*HO2+...
     &   +    1.0000D-01 * RXRAT( 124 )      ! IOLE+O=0.1000D+00*HO2+...
     &   +    5.0000D-01 * RXRAT( 126 )      ! IOLE+O3=0.5000D+00*HO2+...
     &   +                 RXRAT( 127 )      ! IOLE+NO3=HO2+0.6400D+...
     &   +    1.2000D+00 * RXRAT( 129 )      ! TO2+NO=0.1200D+01*HO2+...
     &   +    1.2000D-01 * RXRAT( 131 )      ! OH+CRES=0.1120D+01*HO2+...
     &   +    3.6000D-01 * RXRAT( 132 )      ! CRES+NO3=0.3600D+00*HO2+...
     &   +                 RXRAT( 143 )      ! OPEN=HO2+OPO3+CO
     &   +    7.6000D-01 * RXRAT( 145 )      ! OPEN+O3=0.7600D+00*HO2+...
     &   +    1.2000D+00 * RXRAT( 149 )      ! CAO2+NO=0.1200D+01*HO2+...
     &   +                 RXRAT( 151 )      ! OPO3+NO=HO2+XO2+NO2+ALDX
     &   +                 RXRAT( 157 )      ! MGLY=HO2+C2O3+CO
     &   +    2.5000D-01 * RXRAT( 158 )      ! O+ISOP=0.2500D+00*HO2+...
     &   +    6.6000D-02 * RXRAT( 160 )      ! O3+ISOP=0.6600D-01*HO2+...
     &   +    8.0000D-01 * RXRAT( 161 )      ! NO3+ISOP=0.8000D+00*HO2+...
     &   +    1.5400D-01 * RXRAT( 163 )      ! O3+ISPD=0.1540D+00*HO2+...
     &   +    9.2500D-01 * RXRAT( 164 )      ! NO3+ISPD=0.9250D+00*HO2+...
     &   +    1.0330D+00 * RXRAT( 165 )      ! ISPD=0.1033D+01*HO2+...
     &   +    7.0000D-02 * RXRAT( 168 )      ! TERP+O3=0.7000D-01*HO2+...
     &   +    2.8000D-01 * RXRAT( 169 )      ! TERP+NO3=0.2800D+00*HO2+...
     &   +    8.0000D-01 * RXRAT( 173 )      ! NO2+ISOP=0.8000D+00*HO2+...
     &   +                 RXRAT( 181 )      ! FMCL=HO2+CO+CL
     &   +    1.1000D-01 * RXRAT( 183 )      ! CL+PAR=0.1100D+00*HO2+...
     &   +                 RXRAT( 184 )      ! CL+ETHA=HO2+0.9910D+...
     &   +                 RXRAT( 185 )      ! CL+ETH=HO2+0.2000D+01*XO2+...
     &   +                 RXRAT( 186 )      ! CL+OLE=HO2+0.3300D+00*ALD2+...
     &   +                 RXRAT( 187 )      ! CL+IOLE=HO2+0.7000D+...
     &   +                 RXRAT( 188 )      ! CL+ISOP=HO2+XO2+0.1500D+...
     &   +                 RXRAT( 189 )      ! CL+FORM=HO2+HCL+CO
     &   +                 RXRAT( 192 )      ! CL+MEOH=HO2+HCL+FORM
     &   +                 RXRAT( 193 )      ! CL+ETOH=HO2+HCL+ALD2
     &   +    8.8000D-01 * RXRAT( 195 )      ! CL+TOL=0.8800D+00*HO2+...
     &   +    8.4000D-01 * RXRAT( 196 )      ! CL+XYLMN=0.8400D+00*HO2+...

      P5 = YC0( HO2 ) + P5 * DTC

c..Production of HONO from OH (r19,4 terms )

      R19_4 =   RKI(  24 ) * YC( NO       ) * DTC      ! OH+NO=HONO

c..Remaining HONO production terms
      P19 =    2.0000D+00 * RXRAT(  23 )      ! NO+NO2=0.2000D+01*HONO
     &    +    5.0000D-01 * RXRAT( 219 )      ! NO2=0.5000D+00*HONO+...

      P19 = YC0( HONO ) + P19 * DTC

c..Production of PNA from HO2 (r21,5 term )

      R21_5 =   RKI(  31 ) * YC( NO2      ) * DTC      ! HO2+NO2=HNO4

c..Remaining PNA production terms
      P21   =   YC0( PNA )

c..OH loss terms not in R5_4 & R19_4
      L4 =                 RKI(  26 ) * YC( HONO     )   ! OH+HONO=NO2
     &   +                 RKI(  28 ) * YC( NO2      )   ! OH+NO2=HNO3
     &   +                 RKI(  29 ) * YC( HNO3     )   ! OH+HNO3=NO3
     &   +                 RKI(  33 ) * YC( PNA      )   ! OH+PNA=NO2
     &   +    2.0000D+00 * RKI(  41 ) * YC( OH       )   ! OH+OH=O
     &   +    2.0000D+00 * RKI(  42 ) * YC( OH       )   ! OH+OH=H2O2
     &   +                 RKI(  43 ) * YC( HO2      )   ! OH+HO2=
     &   +                 RKI(  63 ) * YC( ROOH     )   ! OH+ROOH=XO2+...
     &   +                 RKI(  66 )                    ! OH=MEO2
     &   +    7.0000D-01 * RKI(  70 ) * YC( MEPX     )   ! OH+MEPX=0.3000D+...
     &   +                 RKI(  84 ) * YC( ALD2     )   ! OH+ALD2=C2O3
     &   +                 RKI(  95 ) * YC( PACD     )   ! OH+PACD=C2O3
     &   +                 RKI(  97 ) * YC( AACD     )   ! OH+AACD=MEO2
     &   +                 RKI(  99 ) * YC( ALDX     )   ! OH+ALDX=CXO3
     &   +                 RKI( 106 ) * YC( PANX     )   ! OH+PANX=ALD2+NO2
     &   +    8.9000D-01 * RKI( 112 ) * YC( PAR      )   ! OH+PAR=0.1100D+...
     &   +    5.0000D-02 * RKI( 117 ) * YC( OLE      )   ! OH+OLE=0.9500D+...
     &   +    6.4800D-01 * RKI( 128 ) * YC( TOL      )   ! OH+TOL=0.2800D+...
     &   +                 RKI( 135 ) * YC( CRON     )   ! OH+CRON=CRNO
     &   +                 RKI( 142 ) * YC( CRPX     )   ! OH+CRPX=CRN2
     &   +                 RKI( 144 ) * YC( OPEN     )   ! OH+OPEN=0.6000D+...
     &   +                 RKI( 147 ) * YC( CAT1     )   ! OH+CAT1=CAO2
     &   +    3.0000D-01 * RKI( 154 ) * YC( XYLMN    )   ! OH+XYLMN=0.7000D+...
     &   +    3.0000D-01 * RKI( 155 ) * YC( NAPH     )   ! OH+NAPH=0.7000D+...
     &   +                 RKI( 156 ) * YC( MGLY     )   ! OH+MGLY=XO2+C2O3
     &   +    8.8000D-02 * RKI( 159 ) * YC( ISOP     )   ! OH+ISOP=0.9120D+...
     &   +    4.9700D-01 * RKI( 162 ) * YC( ISPD     )   ! OH+ISPD=0.5030D+...
     &   +    2.5000D-01 * RKI( 167 ) * YC( TERP     )   ! OH+TERP=0.7500D+...
     &   +                 RKI( 180 ) * YC( FMCL     )   ! OH+FMCL=CL+CO
     &   +                 RKI( 194 ) * YC( HCL      )   ! OH+HCL=CL

      L4    = 1.0D0 + L4 * DTC + R5_4 + R19_4

c..HO2 loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5 =                 RKI(  43 ) * YC( OH       )   ! HO2+OH=
     &   +                 RKI(  48 ) * YC( NO3      )   ! HO2+NO3=HNO3
     &   +                 RKI(  56 ) * YC( XO2      )   ! HO2+XO2=ROOH
     &   +                 RKI(  57 ) * YC( XO2N     )   ! HO2+XO2N=ROOH
     &   +                 RKI(  68 ) * YC( MEO2     )   ! HO2+MEO2=MEPX
     &   +                 RKI(  78 ) * YC( FORM     )   ! HO2+FORM=HCO3
     &   +                 RKI(  81 ) * YC( HCO3     )   ! HO2+HCO3=MEPX
     &   +                 RKI(  91 ) * YC( C2O3     )   ! HO2+C2O3=0.8000D+...
     &   +                 RKI( 107 ) * YC( CXO3     )   ! HO2+CXO3=0.8000D+...
     &   +                 RKI( 130 ) * YC( TO2      )   ! HO2+TO2=
     &   +                 RKI( 134 ) * YC( CRO      )   ! HO2+CRO=CRES
     &   +                 RKI( 140 ) * YC( CRN2     )   ! HO2+CRN2=CRPX
     &   +                 RKI( 150 ) * YC( CAO2     )   ! HO2+CAO2=
     &   +                 RKI( 179 ) * YC( CLO      )   ! HO2+CLO=HOCL

      L5    = 1.0D0 + L5 * DTC + R4_5 + R21_5

c..HONO loss terms not included in R4_19
      L19 =                 RKI(  26 ) * YC( OH       )   ! HONO+OH=NO2
     &    +    2.0000D+00 * RKI(  27 ) * YC( HONO     )   ! HONO+HONO=NO+NO2

      L19   = 1.0D0 + L19 * DTC + R4_19

c..PNA loss terms not inluded in R5_21
      L21 =                 RKI(  33 ) * YC( OH       )   ! PNA+OH=NO2
     &    +    3.9000D-01 * RKI(  51 )                    ! PNA=0.6100D+...

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
      A = 2.0D0 * ( RKI(  34 ) + RKI(  35 ) ) * DTC

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


