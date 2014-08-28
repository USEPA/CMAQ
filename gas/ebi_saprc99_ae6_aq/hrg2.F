
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
C  PRECONDITIONS: For the SAPRC99 family of mechanisms
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
      O1D_S =                 RKI( NCELL,  19 )                    ! O1D2=0.2000D+01*HO
     &      +                 RKI( NCELL,  20 )                    ! O1D2=O3P

      OH_S =    2.0000D+00 * RKI( NCELL,  19 )                    ! O1D2=0.2000D+01*HO

      OH_S  = OH_S / O1D_S


c..Production of HO from HO2 (r4,5 terms )
      R4_5 =                 RKI( NCELL,  31 ) * YC( NCELL, NO       )   ! HO2+NO=HO+NO2
     &     +                 RKI( NCELL,  36 ) * YC( NCELL, O3       )   ! HO2+O3=HO
     &     +    8.0000D-01 * RKI( NCELL,  39 ) * YC( NCELL, NO3      )   ! HO2+NO3=0.8000D+...

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =                 RKI( NCELL,  22 )                    ! HONO=HO+NO

      R4_19 = R4_19 * DTC

c..Remaining HO production
      P4 =   OH_S  *       RXRAT( NCELL,  18 )      ! O1D2=0.2000D+01*HO
     &   +                 RXRAT( NCELL,  28 )      ! HNO3=HO+NO2
     &   +    3.9000D-01 * RXRAT( NCELL,  34 )      ! HNO4=0.3900D+00*HO+0.6100D+...
     &   +    2.0000D+00 * RXRAT( NCELL,  41 )      ! HO2H=0.2000D+01*HO
     &   +                 RXRAT( NCELL, 142 )      ! COOH=HO+HO2+HCHO
     &   +                 RXRAT( NCELL, 144 )      ! ROOH=HO+HO2+RCHO
     &   +    2.0800D-01 * RXRAT( NCELL, 162 )      ! METHACRO+O3=0.2080D+00*HO+...
     &   +    3.3000D-01 * RXRAT( NCELL, 165 )      ! METHACRO=0.3300D+00*HO+...
     &   +    1.6400D-01 * RXRAT( NCELL, 167 )      ! MVK+O3=0.1640D+00*HO+...
     &   +    2.8500D-01 * RXRAT( NCELL, 171 )      ! ISOPROD+O3=0.2850D+00*HO+...
     &   +    5.0000D-01 * RXRAT( NCELL, 179 )      ! DCB1+O3=0.5000D+00*HO+...
     &   +    1.2000D-01 * RXRAT( NCELL, 186 )      ! ETHENE+O3=0.1200D+00*HO+...
     &   +    2.6600D-01 * RXRAT( NCELL, 190 )      ! ISOPRENE+O3=0.2660D+00*HO+...
     &   +    5.6700D-01 * RXRAT( NCELL, 194 )      ! TRP1+O3=0.5670D+00*HO+...
     &   +    1.5500D-01 * RXRAT( NCELL, 212 )      ! OLE1+O3=0.1550D+00*HO+...
     &   +    3.7800D-01 * RXRAT( NCELL, 216 )      ! OLE2+O3=0.3780D+00*HO+...

      P4 = YC0( NCELL, HO ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =                 RKI( NCELL,  26 ) * YC( NCELL, NO3      )   ! HO+NO3=HO2+NO2
     &     +                 RKI( NCELL,  29 ) * YC( NCELL, CO       )   ! HO+CO=HO2
     &     +                 RKI( NCELL,  30 ) * YC( NCELL, O3       )   ! HO+O3=HO2
     &     +                 RKI( NCELL,  42 ) * YC( NCELL, HO2H     )   ! HO+HO2H=HO2
     &     +                 RKI( NCELL,  44 ) * YC( NCELL, SO2      )   ! HO+SO2=HO2+SULF+...
     &     +                 RKI( NCELL,  45 )                    ! HO=HO2
     &     +                 RKI( NCELL, 125 ) * YC( NCELL, HCHO     )   ! HO+HCHO=HO2+CO
     &     +                 RKI( NCELL, 140 ) * YC( NCELL, MEOH     )   ! HO+MEOH=HO2+HCHO
     &     +    6.3000D-01 * RKI( NCELL, 147 ) * YC( NCELL, GLY      )   ! HO+GLY=0.6300D+...
     &     +    3.7900D-01 * RKI( NCELL, 174 ) * YC( NCELL, PROD2    )   ! HO+PROD2=0.3790D+...
     &     +    1.1300D-01 * RKI( NCELL, 176 ) * YC( NCELL, RNO3     )   ! HO+RNO3=0.1130D+...
     &     +    1.2100D-01 * RKI( NCELL, 198 ) * YC( NCELL, ALK2     )   ! HO+ALK2=0.1210D+...
     &     +    2.2400D-01 * RKI( NCELL, 202 ) * YC( NCELL, ARO1     )   ! HO+ARO1=0.2240D+...
     &     +    1.8700D-01 * RKI( NCELL, 205 ) * YC( NCELL, ARO2     )   ! HO+ARO2=0.1870D+...
     &     +    2.3600D-01 * RKI( NCELL, 208 ) * YC( NCELL, BENZENE  )   ! HO+BENZENE=...
     &     +                 RKI( NCELL, 219 ) * YC( NCELL, HCOOH    )   ! HO+HCOOH=HO2

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =                 RKI( NCELL,  33 )                    ! HNO4=HO2+NO2
     &      +    6.1000D-01 * RKI( NCELL,  34 )                    ! HNO4=0.6100D+...

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5 =                 RXRAT( NCELL,  23 )      ! HONO=HO2+NO2
     &   +                 RXRAT( NCELL,  46 )      ! C_O2+NO=HO2+HCHO+NO2
     &   +                 RXRAT( NCELL,  48 )      ! C_O2+NO3=HO2+HCHO+NO2
     &   +    2.0000D+00 * RXRAT( NCELL,  50 )      ! C_O2+C_O2=0.2000D+01*HO2+...
     &   +                 RXRAT( NCELL,  51 )      ! RO2_R+NO=HO2+NO2
     &   +                 RXRAT( NCELL,  53 )      ! RO2_R+NO3=HO2+NO2
     &   +                 RXRAT( NCELL,  54 )      ! RO2_R+C_O2=HO2+0.7500D+...
     &   +                 RXRAT( NCELL,  55 )      ! RO2_R+RO2_R=HO2
     &   +                 RXRAT( NCELL,  64 )      ! RO2_N+C_O2=HO2+0.2500D+...
     &   +                 RXRAT( NCELL,  65 )      ! RO2_N+NO3=HO2+NO2+MEK
     &   +                 RXRAT( NCELL,  66 )      ! RO2_N+RO2_R=HO2+0.5000D+...
     &   +                 RXRAT( NCELL,  68 )      ! RO2_N+RO2_N=HO2+MEK+PROD2
     &   +    2.0000D+00 * RXRAT( NCELL, 123 )      ! HCHO=0.2000D+01*HO2+CO
     &   +                 RXRAT( NCELL, 127 )      ! HOCOO=HO2+HCHO
     &   +                 RXRAT( NCELL, 128 )      ! HOCOO+NO=HO2+NO2+HCOOH
     &   +                 RXRAT( NCELL, 129 )      ! HCHO+NO3=HO2+HNO3+CO
     &   +                 RXRAT( NCELL, 131 )      ! CCHO=HO2+CO+C_O2
     &   +                 RXRAT( NCELL, 134 )      ! RCHO=HO2+RO2_R+CO+CCHO
     &   +                 RXRAT( NCELL, 142 )      ! COOH=HO2+HO+HCHO
     &   +                 RXRAT( NCELL, 144 )      ! ROOH=HO2+HO+RCHO
     &   +    2.0000D+00 * RXRAT( NCELL, 145 )      ! GLY=0.2000D+01*HO2+0.2000D+...
     &   +    6.3000D-01 * RXRAT( NCELL, 148 )      ! GLY+NO3=0.6300D+00*HO2+...
     &   +                 RXRAT( NCELL, 149 )      ! MGLY=HO2+CO+CCO_O2
     &   +    8.0000D-03 * RXRAT( NCELL, 162 )      ! METHACRO+O3=0.8000D-02*HO2+...
     &   +    3.4000D-01 * RXRAT( NCELL, 165 )      ! METHACRO=0.3400D+00*HO2+...
     &   +    6.4000D-02 * RXRAT( NCELL, 167 )      ! MVK+O3=0.6400D-01*HO2+...
     &   +    4.0000D-01 * RXRAT( NCELL, 171 )      ! ISOPROD+O3=0.4000D+00*HO2+...
     &   +    1.2330D+00 * RXRAT( NCELL, 173 )      ! ISOPROD=0.1233D+01*HO2+...
     &   +    3.4100D-01 * RXRAT( NCELL, 177 )      ! RNO3=0.3410D+00*HO2+NO2+...
     &   +    1.5000D+00 * RXRAT( NCELL, 179 )      ! DCB1+O3=0.1500D+01*HO2+...
     &   +    5.0000D-01 * RXRAT( NCELL, 181 )      ! DCB2=0.5000D+00*HO2+...
     &   +    5.0000D-01 * RXRAT( NCELL, 183 )      ! DCB3=0.5000D+00*HO2+...
     &   +    1.2000D-01 * RXRAT( NCELL, 186 )      ! ETHENE+O3=0.1200D+00*HO2+...
     &   +    5.0000D-01 * RXRAT( NCELL, 188 )      ! ETHENE+O3P=0.5000D+00*HO2+...
     &   +    3.3000D-02 * RXRAT( NCELL, 194 )      ! TRP1+O3=0.3300D-01*HO2+...
     &   +    5.6000D-02 * RXRAT( NCELL, 212 )      ! OLE1+O3=0.5600D-01*HO2+...
     &   +    3.0000D-03 * RXRAT( NCELL, 216 )      ! OLE2+O3=0.3000D-02*HO2+...
     &   +    1.3000D-02 * RXRAT( NCELL, 218 )      ! OLE2+O3P=0.1300D-01*HO2+...

      P5 = YC0( NCELL, HO2 ) + P5 * DTC

c..Production of HONO from OH (r19,4 terms )

      R19_4 =   RKI( NCELL,  21 ) * YC( NCELL, NO       ) * DTC      ! OH+NO=HONO

c..Remaining HONO production terms
      P19 =    5.0000D-01 * RXRAT( NCELL, 226 )      ! NO2=0.5000D+00*HONO+...

      P19 = YC0( NCELL, HONO ) + P19 * DTC

c..Production of HNO4 from HO2 (r21,5 term )

      R21_5 =   RKI( NCELL,  32 ) * YC( NCELL, NO2      ) * DTC      ! HO2+NO2=HNO4

c..Remaining HNO4 production terms
      P21   =   YC0( NCELL, HNO4 )

c..HO loss terms not in R5_4 & R19_4
      L4 =                 RKI( NCELL,  24 ) * YC( NCELL, HONO     )   ! HO+HONO=NO2
     &   +                 RKI( NCELL,  25 ) * YC( NCELL, NO2      )   ! HO+NO2=HNO3
     &   +                 RKI( NCELL,  27 ) * YC( NCELL, HNO3     )   ! HO+HNO3=NO3
     &   +                 RKI( NCELL,  35 ) * YC( NCELL, HNO4     )   ! HO+HNO4=NO2
     &   +                 RKI( NCELL,  43 ) * YC( NCELL, HO2      )   ! HO+HO2=
     &   +                 RKI( NCELL, 130 ) * YC( NCELL, CCHO     )   ! HO+CCHO=CCO_O2
     &   +                 RKI( NCELL, 133 ) * YC( NCELL, RCHO     )   ! HO+RCHO=0.3400D-...
     &   +                 RKI( NCELL, 136 ) * YC( NCELL, ACET     )   ! HO+ACET=HCHO+...
     &   +                 RKI( NCELL, 138 ) * YC( NCELL, MEK      )   ! HO+MEK=0.3700D+...
     &   +    6.5000D-01 * RKI( NCELL, 141 ) * YC( NCELL, COOH     )   ! HO+COOH=0.3500D+...
     &   +    3.4000D-01 * RKI( NCELL, 143 ) * YC( NCELL, ROOH     )   ! HO+ROOH=RCHO+...
     &   +    3.7000D-01 * RKI( NCELL, 147 ) * YC( NCELL, GLY      )   ! HO+GLY=0.6300D+...
     &   +                 RKI( NCELL, 150 ) * YC( NCELL, MGLY     )   ! HO+MGLY=CO+CCO_O2
     &   +                 RKI( NCELL, 153 ) * YC( NCELL, PHEN     )   ! HO+PHEN=0.2400D+...
     &   +                 RKI( NCELL, 155 ) * YC( NCELL, CRES     )   ! HO+CRES=0.2400D+...
     &   +                 RKI( NCELL, 158 ) * YC( NCELL, BALD     )   ! HO+BALD=BZCO_O2
     &   +                 RKI( NCELL, 161 ) * YC( NCELL, METHACRO )   ! HO+METHACRO=...
     &   +                 RKI( NCELL, 166 ) * YC( NCELL, MVK      )   ! HO+MVK=0.3000D+...
     &   +                 RKI( NCELL, 170 ) * YC( NCELL, ISOPROD  )   ! HO+ISOPROD=...
     &   +    6.2100D-01 * RKI( NCELL, 174 ) * YC( NCELL, PROD2    )   ! HO+PROD2=0.3790D+...
     &   +    8.8700D-01 * RKI( NCELL, 176 ) * YC( NCELL, RNO3     )   ! HO+RNO3=0.1130D+...
     &   +                 RKI( NCELL, 178 ) * YC( NCELL, DCB1     )   ! HO+DCB1=RCHO+...
     &   +                 RKI( NCELL, 180 ) * YC( NCELL, DCB2     )   ! HO+DCB2=R2O2+...
     &   +                 RKI( NCELL, 182 ) * YC( NCELL, DCB3     )   ! HO+DCB3=R2O2+...
     &   +                 RKI( NCELL, 184 )                    ! HO=C_O2
     &   +                 RKI( NCELL, 185 ) * YC( NCELL, ETHENE   )   ! HO+ETHENE=RO2_R+...
     &   +                 RKI( NCELL, 189 ) * YC( NCELL, ISOPRENE )   ! HO+ISOPRENE=...
     &   +                 RKI( NCELL, 193 ) * YC( NCELL, TRP1     )   ! HO+TRP1=0.7500D+...
     &   +                 RKI( NCELL, 197 ) * YC( NCELL, ALK1     )   ! HO+ALK1=RO2_R+CCHO
     &   +    6.3300D-01 * RKI( NCELL, 198 ) * YC( NCELL, ALK2     )   ! HO+ALK2=0.1210D+...
     &   +                 RKI( NCELL, 199 ) * YC( NCELL, ALK3     )   ! HO+ALK3=0.6950D+...
     &   +                 RKI( NCELL, 200 ) * YC( NCELL, ALK4     )   ! HO+ALK4=0.8350D+...
     &   +                 RKI( NCELL, 201 ) * YC( NCELL, ALK5     )   ! HO+ALK5=0.6530D+...
     &   +    7.7600D-01 * RKI( NCELL, 202 ) * YC( NCELL, ARO1     )   ! HO+ARO1=0.2240D+...
     &   +    8.1300D-01 * RKI( NCELL, 205 ) * YC( NCELL, ARO2     )   ! HO+ARO2=0.1870D+...
     &   +    7.6400D-01 * RKI( NCELL, 208 ) * YC( NCELL, BENZENE  )   ! HO+BENZENE=...
     &   +                 RKI( NCELL, 211 ) * YC( NCELL, OLE1     )   ! HO+OLE1=0.9100D+...
     &   +                 RKI( NCELL, 215 ) * YC( NCELL, OLE2     )   ! HO+OLE2=0.9180D+...
     &   +                 RKI( NCELL, 220 ) * YC( NCELL, CCO_OH   )   ! HO+CCO_OH=...
     &   +                 RKI( NCELL, 221 ) * YC( NCELL, RCO_OH   )   ! HO+RCO_OH=RO2_R+...

      L4    = 1.0D0 + L4 * DTC + R5_4 + R19_4

c..HO2 loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5 =    2.0000D-01 * RKI( NCELL,  39 ) * YC( NCELL, NO3      )   ! HO2+NO3=0.8000D+...
     &   +                 RKI( NCELL,  43 ) * YC( NCELL, HO       )   ! HO2+HO=
     &   +                 RKI( NCELL,  47 ) * YC( NCELL, C_O2     )   ! HO2+C_O2=COOH
     &   +                 RKI( NCELL,  52 ) * YC( NCELL, RO2_R    )   ! HO2+RO2_R=ROOH
     &   +                 RKI( NCELL,  63 ) * YC( NCELL, RO2_N    )   ! HO2+RO2_N=ROOH
     &   +                 RKI( NCELL,  72 ) * YC( NCELL, CCO_O2   )   ! HO2+CCO_O2=...
     &   +                 RKI( NCELL,  82 ) * YC( NCELL, RCO_O2   )   ! HO2+RCO_O2=...
     &   +                 RKI( NCELL,  93 ) * YC( NCELL, BZCO_O2  )   ! HO2+BZCO_O2=...
     &   +                 RKI( NCELL, 105 ) * YC( NCELL, MA_RCO3  )   ! HO2+MA_RCO3=...
     &   +                 RKI( NCELL, 118 ) * YC( NCELL, BZ_O     )   ! HO2+BZ_O=PHEN
     &   +                 RKI( NCELL, 121 ) * YC( NCELL, BZNO2_O  )   ! HO2+BZNO2_O=NPHE
     &   +                 RKI( NCELL, 126 ) * YC( NCELL, HCHO     )   ! HO2+HCHO=HOCOO

      L5    = 1.0D0 + L5 * DTC + R4_5 + R21_5

c..HONO loss terms not included in R4_19
      L19 =                 RKI( NCELL,  23 )                    ! HONO=HO2+NO2
     &    +                 RKI( NCELL,  24 ) * YC( NCELL, HO       )   ! HONO+HO=NO2

      L19   = 1.0D0 + L19 * DTC + R4_19

c..HNO4 loss terms not inluded in R5_21
      L21 =    3.9000D-01 * RKI( NCELL,  34 )                    ! HNO4=0.6100D+...
     &    +                 RKI( NCELL,  35 ) * YC( NCELL, HO       )   ! HNO4+HO=NO2

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
      A = 2.0D0 * ( RKI( NCELL,  37 ) + RKI( NCELL,  38 ) ) * DTC

      B = L5 - T3 * R21_5 - T2 * R4_5 * L19

      C = P5 + T3 * P21 + T2 * ( P4 * L19 + P19 * R4_19 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( NCELL, HO2 ) = MAX( Q / A , -C / Q  )

c..compute remaining species concentrations
      YCP( NCELL, HO ) = ( ( P4 + R4_5 * YCP( NCELL, HO2 ) ) * L19 + R4_19 * P19 ) * T1

      YCP( NCELL, HNO4 ) = ( P21 + R21_5 * YCP( NCELL, HO2 ) ) * L21_INV

      YCP( NCELL, HONO ) = ( P19 + R19_4 * YCP( NCELL, HO ) ) / L19

      RETURN

      END


