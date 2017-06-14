
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
C  PRECONDITIONS: For the CB05e51 family of mechanisms
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
      O1D_S =                 RKI(  10 )                        ! O1D=O
     &      +                 RKI(  11 )                        ! O1D=0.2000D+01*OH
     &      +                 RKI(  39 )                        ! O1D=OH+HO2

      OH_S =    2.0000D+00 * RKI(  11 )                        ! O1D=0.2000D+01*OH
     &     +                 RKI(  39 )                        ! O1D=OH+HO2

      OH_S  = OH_S / O1D_S

      HO2_S =                 RKI(  39 )                        ! O1D=HO2+OH

      HO2_S  = HO2_S / O1D_S

c..Production of HO from HO2 (r4,5 terms )
      R4_5 =                 RKI(  13 ) * YC( O3           )   ! HO2+O3=OH
     &     +                 RKI(  30 ) * YC( NO           )   ! HO2+NO=OH+NO2
     &     +                 RKI(  45 ) * YC( O            )   ! HO2+O=OH
     &     +    1.7500D-01 * RKI(  66 ) * YC( IEPXO2       )   ! HO2+IEPXO2=...
     &     +    4.4000D-01 * RKI(  95 ) * YC( C2O3         )   ! HO2+C2O3=0.4400D+...
     &     +    4.4000D-01 * RKI( 111 ) * YC( CXO3         )   ! HO2+CXO3=0.4400D+...
     &     +    4.4000D-01 * RKI( 229 ) * YC( MACO3        )   ! HO2+MACO3=...

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =                 RKI(  25 )                        ! HONO=OH+NO

      R4_19 = R4_19 * DTC

c..Remaining HO production
      P4 =   OH_S  *       RXRAT(   9 )      ! O1D=0.2000D+01*OH
     &   +    2.0000D+00 * RXRAT(  37 )      ! H2O2=0.2000D+01*OH
     &   +                 RXRAT(  46 )      ! H2O2+O=OH+HO2
     &   +    3.9000D-01 * RXRAT(  52 )      ! PNA=0.3900D+00*OH+0.6100D+...
     &   +                 RXRAT(  53 )      ! HNO3=OH+NO2
     &   +                 RXRAT(  63 )      ! ROOH=OH+HO2+0.5000D+...
     &   +    9.5000D-01 * RXRAT(  66 )      ! HO2+IEPXO2=0.1125D+01*OH+...
     &   +    1.2500D-01 * RXRAT(  67 )      ! IEPXO2+NO=0.1250D+00*OH+...
     &   +    1.0000D-01 * RXRAT(  68 )      ! IEPXO2+C2O3=0.1000D+00*OH+...
     &   +                 RXRAT(  75 )      ! MEPX=OH+HO2+FORM
     &   +                 RXRAT(  80 )      ! FORM+O=OH+HO2+CO
     &   +                 RXRAT(  87 )      ! ALD2+O=OH+C2O3
     &   +                 RXRAT( 100 )      ! PACD=OH+MEO2
     &   +                 RXRAT( 102 )      ! ALDX+O=OH+CXO3
     &   +    1.0000D-01 * RXRAT( 120 )      ! O+OLE=0.1000D+00*OH+...
     &   +    1.0000D-01 * RXRAT( 122 )      ! O3+OLE=0.1000D+00*OH+...
     &   +    3.0000D-01 * RXRAT( 124 )      ! O+ETH=0.3000D+00*OH+...
     &   +    1.3000D-01 * RXRAT( 126 )      ! O3+ETH=0.1300D+00*OH+...
     &   +    5.0000D-01 * RXRAT( 130 )      ! IOLE+O3=0.5000D+00*OH+...
     &   +                 RXRAT( 145 )      ! CRPX=OH+CRNO
     &   +    8.0000D-02 * RXRAT( 149 )      ! OPEN+O3=0.8000D-01*OH+...
     &   +    2.6600D-01 * RXRAT( 167 )      ! O3+ISOP=0.2660D+00*OH+...
     &   +    2.6800D-01 * RXRAT( 170 )      ! O3+ISPD=0.2680D+00*OH+...
     &   +    5.7000D-01 * RXRAT( 179 )      ! TERP+O3=0.5700D+00*OH+...
     &   +                 RXRAT( 186 )      ! HOCL=OH+CL

      P4 = YC0( OH ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =                 RKI(  12 ) * YC( O3           )   ! OH+O3=HO2
     &     +                 RKI(  38 ) * YC( H2O2         )   ! OH+H2O2=HO2
     &     +                 RKI(  40 )                        ! OH=HO2
     &     +                 RKI(  41 ) * YC( O            )   ! OH+O=HO2
     &     +                 RKI(  48 ) * YC( NO3          )   ! OH+NO3=HO2+NO2
     &     +                 RKI(  69 ) * YC( CO           )   ! OH+CO=HO2
     &     +    3.0000D-01 * RKI(  74 ) * YC( MEPX         )   ! OH+MEPX=0.3000D+...
     &     +                 RKI(  76 ) * YC( MEOH         )   ! OH+MEOH=HO2+FORM
     &     +                 RKI(  77 ) * YC( FORM         )   ! OH+FORM=HO2+CO
     &     +                 RKI(  86 ) * YC( FACD         )   ! OH+FACD=HO2
     &     +    1.1000D-01 * RKI( 116 ) * YC( PAR          )   ! OH+PAR=0.1100D+...
     &     +    9.5000D-01 * RKI( 121 ) * YC( OLE          )   ! OH+OLE=0.9500D+...
     &     +                 RKI( 125 ) * YC( ETH          )   ! OH+ETH=HO2+...
     &     +                 RKI( 129 ) * YC( IOLE         )   ! OH+IOLE=HO2+...
     &     +    2.8000D-01 * RKI( 132 ) * YC( TOL          )   ! OH+TOL=0.2800D+...
     &     +                 RKI( 135 ) * YC( CRES         )   ! OH+CRES=0.1120D+...
     &     +    7.0000D-01 * RKI( 158 ) * YC( XYLMN        )   ! OH+XYLMN=0.7000D+...
     &     +    7.0000D-01 * RKI( 159 ) * YC( NAPH         )   ! OH+NAPH=0.7000D+...
     &     +    5.0300D-01 * RKI( 169 ) * YC( ISPD         )   ! OH+ISPD=0.5030D+...
     &     +    7.5000D-01 * RKI( 174 ) * YC( TERP         )   ! OH+TERP=0.7500D+...
     &     +                 RKI( 181 ) * YC( SO2          )   ! OH+SO2=HO2+SULF+...
     &     +                 RKI( 182 ) * YC( ETOH         )   ! OH+ETOH=HO2+...
     &     +                 RKI( 183 ) * YC( ETHA         )   ! OH+ETHA=HO2+...

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =                 RKI(  33 )                        ! PNA=HO2+NO2
     &      +    6.1000D-01 * RKI(  52 )                        ! PNA=0.6100D+...

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5 =   HO2_S  *      RXRAT(   9 )      ! O1D=HO2+OH
     &   +                 RXRAT(  46 )      ! H2O2+O=HO2+OH
     &   +                 RXRAT(  63 )      ! ROOH=HO2+OH+0.5000D+...
     &   +    8.2500D-01 * RXRAT(  67 )      ! IEPXO2+NO=0.8250D+00*HO2+...
     &   +    6.6000D-01 * RXRAT(  68 )      ! IEPXO2+C2O3=0.6600D+00*HO2+...
     &   +                 RXRAT(  71 )      ! MEO2+NO=HO2+FORM+NO2
     &   +    7.4000D-01 * RXRAT(  73 )      ! MEO2+MEO2=0.7400D+00*HO2+...
     &   +                 RXRAT(  75 )      ! MEPX=HO2+OH+FORM
     &   +    2.0000D+00 * RXRAT(  78 )      ! FORM=0.2000D+01*HO2+CO
     &   +                 RXRAT(  80 )      ! FORM+O=HO2+OH+CO
     &   +                 RXRAT(  81 )      ! FORM+NO3=HO2+HNO3+CO
     &   +                 RXRAT(  83 )      ! HCO3=HO2+FORM
     &   +                 RXRAT(  84 )      ! HCO3+NO=HO2+NO2+FACD
     &   +                 RXRAT(  90 )      ! ALD2=HO2+CO+MEO2
     &   +    9.0000D-01 * RXRAT(  96 )      ! C2O3+MEO2=0.9000D+00*HO2+...
     &   +                 RXRAT( 105 )      ! ALDX=HO2+CO+MEO2
     &   +                 RXRAT( 106 )      ! CXO3+NO=HO2+NO2+ALD2+XO2
     &   +    4.0000D-01 * RXRAT( 109 )      ! PANX=0.4000D+00*HO2+...
     &   +                 RXRAT( 112 )      ! CXO3+MEO2=HO2+0.9000D+...
     &   +    2.0000D+00 * RXRAT( 114 )      ! CXO3+CXO3=0.2000D+01*HO2+...
     &   +                 RXRAT( 115 )      ! CXO3+C2O3=HO2+XO2+MEO2+ALD2
     &   +    9.4000D-01 * RXRAT( 117 )      ! ROR=0.9400D+00*HO2+0.6000D+...
     &   +                 RXRAT( 118 )      ! ROR=HO2
     &   +    3.0000D-01 * RXRAT( 120 )      ! O+OLE=0.3000D+00*HO2+...
     &   +    4.4000D-01 * RXRAT( 122 )      ! O3+OLE=0.4400D+00*HO2+...
     &   +    1.7000D+00 * RXRAT( 124 )      ! O+ETH=0.1700D+01*HO2+...
     &   +    1.3000D-01 * RXRAT( 126 )      ! O3+ETH=0.1300D+00*HO2+...
     &   +    1.0000D-01 * RXRAT( 128 )      ! IOLE+O=0.1000D+00*HO2+...
     &   +    5.0000D-01 * RXRAT( 130 )      ! IOLE+O3=0.5000D+00*HO2+...
     &   +                 RXRAT( 131 )      ! IOLE+NO3=HO2+0.6400D+...
     &   +    1.2000D+00 * RXRAT( 133 )      ! TO2+NO=0.1200D+01*HO2+...
     &   +    1.2000D-01 * RXRAT( 135 )      ! OH+CRES=0.1120D+01*HO2+...
     &   +    3.6000D-01 * RXRAT( 136 )      ! CRES+NO3=0.3600D+00*HO2+...
     &   +                 RXRAT( 147 )      ! OPEN=HO2+OPO3+CO
     &   +    7.6000D-01 * RXRAT( 149 )      ! OPEN+O3=0.7600D+00*HO2+...
     &   +    1.2000D+00 * RXRAT( 153 )      ! CAO2+NO=0.1200D+01*HO2+...
     &   +                 RXRAT( 155 )      ! OPO3+NO=HO2+XO2+NO2+ALDX
     &   +                 RXRAT( 161 )      ! MGLY=HO2+C2O3+CO
     &   +    2.5000D-01 * RXRAT( 162 )      ! O+ISOP=0.2500D+00*HO2+...
     &   +    9.0000D-01 * RXRAT( 164 )      ! ISOPO2+NO=0.9000D+00*HO2+...
     &   +    8.0000D-01 * RXRAT( 165 )      ! ISOPO2+C2O3=0.8000D+00*HO2+...
     &   +    6.6000D-02 * RXRAT( 167 )      ! O3+ISOP=0.6600D-01*HO2+...
     &   +    8.0000D-01 * RXRAT( 168 )      ! NO3+ISOP=0.8000D+00*HO2+...
     &   +    1.5400D-01 * RXRAT( 170 )      ! O3+ISPD=0.1540D+00*HO2+...
     &   +    9.2500D-01 * RXRAT( 171 )      ! NO3+ISPD=0.9250D+00*HO2+...
     &   +    1.0330D+00 * RXRAT( 172 )      ! ISPD=0.1033D+01*HO2+...
     &   +    7.0000D-02 * RXRAT( 179 )      ! TERP+O3=0.7000D-01*HO2+...
     &   +    2.8000D-01 * RXRAT( 180 )      ! TERP+NO3=0.2800D+00*HO2+...
     &   +    8.0000D-01 * RXRAT( 184 )      ! NO2+ISOP=0.8000D+00*HO2+...
     &   +                 RXRAT( 192 )      ! FMCL=HO2+CO+CL
     &   +    1.1000D-01 * RXRAT( 194 )      ! CL+PAR=0.1100D+00*HO2+...
     &   +                 RXRAT( 195 )      ! CL+ETHA=HO2+0.9910D+...
     &   +                 RXRAT( 196 )      ! CL+ETH=HO2+0.2000D+01*XO2+...
     &   +                 RXRAT( 197 )      ! CL+OLE=HO2+0.3300D+00*ALD2+...
     &   +                 RXRAT( 198 )      ! CL+IOLE=HO2+0.7000D+...
     &   +                 RXRAT( 199 )      ! CL+ISOP=HO2+XO2+0.1500D+...
     &   +                 RXRAT( 200 )      ! CL+FORM=HO2+HCL+CO
     &   +                 RXRAT( 203 )      ! CL+MEOH=HO2+HCL+FORM
     &   +                 RXRAT( 204 )      ! CL+ETOH=HO2+HCL+ALD2
     &   +    8.8000D-01 * RXRAT( 206 )      ! CL+TOL=0.8800D+00*HO2+...
     &   +    8.4000D-01 * RXRAT( 207 )      ! CL+XYLMN=0.8400D+00*HO2+...
     &   +    8.4000D-01 * RXRAT( 208 )      ! CL+NAPH=0.8400D+00*HO2+...
     &   +    9.0000D-01 * RXRAT( 230 )      ! MACO3+MEO2=0.9000D+00*HO2+...
     &   +                 RXRAT( 232 )      ! MACO3+CXO3=HO2+XO2+ALD2+...
     &   +    6.8000D-01 * RXRAT( 248 )      ! NTRMO2+NO=0.6800D+00*HO2+...
     &   +                 RXRAT( 253 )      ! NTRM=HO2+NO2+ALD2+IOLE
     &   +                 RXRAT( 254 )      ! NTRI=HO2+NO2+0.3300D+...
     &   +                 RXRAT( 255 )      ! NTRALK=HO2+NO2+0.3300D+...
     &   +                 RXRAT( 256 )      ! NTROH=HO2+NO2+0.3300D+...
     &   +                 RXRAT( 257 )      ! NTRCN=HO2+NO2+0.3300D+...
     &   +                 RXRAT( 258 )      ! NTRCNOH=HO2+NO2+0.3300D+...
     &   +                 RXRAT( 259 )      ! NTRPX=HO2+NO2+ROOH

      P5 = YC0( HO2 ) + P5 * DTC

c..Production of HONO from OH (r19,4 terms )

      R19_4 =   RKI(  24 ) * YC( NO           ) * DTC      ! OH+NO=HONO

c..Remaining HONO production terms
      P19 =    2.0000D+00 * RXRAT(  23 )      ! NO+NO2=0.2000D+01*HONO
     &    +    5.0000D-01 * RXRAT( 274 )      ! NO2=0.5000D+00*HONO+...

      P19 = YC0( HONO ) + P19 * DTC

c..Production of PNA from HO2 (r21,5 term )

      R21_5 =   RKI(  32 ) * YC( NO2          ) * DTC      ! HO2+NO2=HNO4

c..Remaining PNA production terms
      P21   =   YC0( PNA )

c..OH loss terms not in R5_4 & R19_4
      L4 =                 RKI(  26 ) * YC( HONO         )   ! OH+HONO=NO2
     &   +                 RKI(  28 ) * YC( NO2          )   ! OH+NO2=HNO3
     &   +                 RKI(  29 ) * YC( HNO3         )   ! OH+HNO3=NO3
     &   +                 RKI(  34 ) * YC( PNA          )   ! OH+PNA=NO2
     &   +    2.0000D+00 * RKI(  42 ) * YC( OH           )   ! OH+OH=O
     &   +    2.0000D+00 * RKI(  43 ) * YC( OH           )   ! OH+OH=H2O2
     &   +                 RKI(  44 ) * YC( HO2          )   ! OH+HO2=
     &   +                 RKI(  62 ) * YC( ROOH         )   ! OH+ROOH=XO2+...
     &   +    6.7000D-02 * RKI(  64 ) * YC( ISOPX        )   ! OH+ISOPX=0.9040D+...
     &   +                 RKI(  65 ) * YC( IEPOX        )   ! OH+IEPOX=IEPXO2
     &   +                 RKI(  70 )                        ! OH=MEO2
     &   +    7.0000D-01 * RKI(  74 ) * YC( MEPX         )   ! OH+MEPX=0.3000D+...
     &   +                 RKI(  88 ) * YC( ALD2         )   ! OH+ALD2=C2O3
     &   +                 RKI(  99 ) * YC( PACD         )   ! OH+PACD=C2O3
     &   +                 RKI( 101 ) * YC( AACD         )   ! OH+AACD=MEO2
     &   +                 RKI( 103 ) * YC( ALDX         )   ! OH+ALDX=CXO3
     &   +                 RKI( 110 ) * YC( PANX         )   ! OH+PANX=ALD2+NO2
     &   +    8.9000D-01 * RKI( 116 ) * YC( PAR          )   ! OH+PAR=0.1100D+...
     &   +    5.0000D-02 * RKI( 121 ) * YC( OLE          )   ! OH+OLE=0.9500D+...
     &   +    6.4800D-01 * RKI( 132 ) * YC( TOL          )   ! OH+TOL=0.2800D+...
     &   +                 RKI( 139 ) * YC( CRON         )   ! OH+CRON=CRNO
     &   +                 RKI( 146 ) * YC( CRPX         )   ! OH+CRPX=CRN2
     &   +                 RKI( 148 ) * YC( OPEN         )   ! OH+OPEN=0.6000D+...
     &   +                 RKI( 151 ) * YC( CAT1         )   ! OH+CAT1=CAO2
     &   +    3.0000D-01 * RKI( 158 ) * YC( XYLMN        )   ! OH+XYLMN=0.7000D+...
     &   +    3.0000D-01 * RKI( 159 ) * YC( NAPH         )   ! OH+NAPH=0.7000D+...
     &   +                 RKI( 160 ) * YC( MGLY         )   ! OH+MGLY=XO2+C2O3
     &   +                 RKI( 163 ) * YC( ISOP         )   ! OH+ISOP=ISOPO2+...
     &   +    4.9700D-01 * RKI( 169 ) * YC( ISPD         )   ! OH+ISPD=0.5030D+...
     &   +    2.5000D-01 * RKI( 174 ) * YC( TERP         )   ! OH+TERP=0.7500D+...
     &   +                 RKI( 191 ) * YC( FMCL         )   ! OH+FMCL=CL+CO
     &   +                 RKI( 205 ) * YC( HCL          )   ! OH+HCL=CL
     &   +                 RKI( 228 ) * YC( MAPAN        )   ! OH+MAPAN=ALD2+CO+NO2
     &   +                 RKI( 234 ) * YC( NTRALK       )   ! OH+NTRALK=NALKO2
     &   +                 RKI( 237 ) * YC( NTROH        )   ! OH+NTROH=NOHO2
     &   +                 RKI( 240 ) * YC( NTRCN        )   ! OH+NTRCN=NCNO2
     &   +                 RKI( 243 ) * YC( NTRCNOH      )   ! OH+NTRCNOH=NCNOHO2
     &   +                 RKI( 247 ) * YC( NTRM         )   ! OH+NTRM=NTRMO2
     &   +                 RKI( 250 ) * YC( NTRI         )   ! OH+NTRI=NTRIO2

      L4    = 1.0D0 + L4 * DTC + R5_4 + R19_4

c..HO2 loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5 =                 RKI(  31 ) * YC( NO           )   ! HO2+NO=HNO3
     &   +                 RKI(  44 ) * YC( OH           )   ! HO2+OH=
     &   +                 RKI(  49 ) * YC( NO3          )   ! HO2+NO3=HNO3
     &   +                 RKI(  57 ) * YC( XO2          )   ! HO2+XO2=ROOH
     &   +                 RKI(  58 ) * YC( XO2N         )   ! HO2+XO2N=ROOH
     &   +                 RKI(  72 ) * YC( MEO2         )   ! HO2+MEO2=MEPX
     &   +                 RKI(  82 ) * YC( FORM         )   ! HO2+FORM=HCO3
     &   +                 RKI(  85 ) * YC( HCO3         )   ! HO2+HCO3=MEPX
     &   +    5.6000D-01 * RKI(  95 ) * YC( C2O3         )   ! HO2+C2O3=0.4400D+...
     &   +    5.6000D-01 * RKI( 111 ) * YC( CXO3         )   ! HO2+CXO3=0.4400D+...
     &   +                 RKI( 134 ) * YC( TO2          )   ! HO2+TO2=
     &   +                 RKI( 138 ) * YC( CRO          )   ! HO2+CRO=CRES
     &   +                 RKI( 144 ) * YC( CRN2         )   ! HO2+CRN2=CRPX
     &   +                 RKI( 154 ) * YC( CAO2         )   ! HO2+CAO2=
     &   +                 RKI( 166 ) * YC( ISOPO2       )   ! HO2+ISOPO2=ISOPX
     &   +                 RKI( 176 ) * YC( XO2T         )   ! HO2+XO2T=ROOH
     &   +                 RKI( 190 ) * YC( CLO          )   ! HO2+CLO=HOCL
     &   +    5.6000D-01 * RKI( 229 ) * YC( MACO3        )   ! HO2+MACO3=...
     &   +                 RKI( 236 ) * YC( NALKO2       )   ! HO2+NALKO2=NTRPX
     &   +                 RKI( 239 ) * YC( NOHO2        )   ! HO2+NOHO2=NTRPX
     &   +                 RKI( 242 ) * YC( NCNO2        )   ! HO2+NCNO2=NTRPX
     &   +                 RKI( 245 ) * YC( NCNOHO2      )   ! HO2+NCNOHO2=NTRPX
     &   +                 RKI( 249 ) * YC( NTRMO2       )   ! HO2+NTRMO2=NTRI
     &   +                 RKI( 252 ) * YC( NTRIO2       )   ! HO2+NTRIO2=NTRI

      L5    = 1.0D0 + L5 * DTC + R4_5 + R21_5

c..HONO loss terms not included in R4_19
      L19 =                 RKI(  26 ) * YC( OH           )   ! HONO+OH=NO2
     &    +    2.0000D+00 * RKI(  27 ) * YC( HONO         )   ! HONO+HONO=NO+NO2

      L19   = 1.0D0 + L19 * DTC + R4_19

c..PNA loss terms not inluded in R5_21
      L21 =                 RKI(  34 ) * YC( OH           )   ! PNA+OH=NO2
     &    +    3.9000D-01 * RKI(  52 )                        ! PNA=0.6100D+...

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
      A = 2.0D0 * ( RKI(  35 ) + RKI(  36 ) ) * DTC

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


