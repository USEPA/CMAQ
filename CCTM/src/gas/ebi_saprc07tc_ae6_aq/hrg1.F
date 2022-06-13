
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


       SUBROUTINE HRG1( DTC )

C**********************************************************************
C
C  FUNCTION: To solve for the concentration of NO2, NO, O3, and O3P
C            algebraically.
C
C  PRECONDITIONS: For the SAPRC07TC_AE6_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables
C   01 Jun 18 B.Hutzell: replaced steady solution for O1D with backward Euler
C                        approximation. To match conditions where the initial
C                        concentration cannot be neglected.
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: None


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC                      ! Time step


C..PARAMETERS: None


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
!     CHARACTER( 16 ), SAVE  :: PNAME = 'HRG1'   ! Prgram Name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) :: O1D_S               ! sum of O1D loss frequencies
      REAL( 8 ) :: O3P_S               ! stoich coeff for O3P from O1D



      REAL( 8 ) :: R1_2                ! production term for NO from NO2
      REAL( 8 ) :: R2_1                ! production term for NO2 from NO
      REAL( 8 ) :: P1, P2, P3, P12     ! production terms for NO, NO2, O3, & O3P
      REAL( 8 ) :: L1, L2, L3, L12     ! loss terms for NO, NO2, O3, O3P
      REAL( 8 ) :: L1_INV, L2_INV,
     &             L3_INV, L12_INV     ! inverse of loss terms

      REAL( 8 ) :: T1, T2, T3, T4, T5  ! intermerdiate terms
      REAL( 8 ) :: F1, F2, F3          ! intermerdiate terms
      REAL( 8 ) :: A, B, C             ! coefficients for quadratic equation
      REAL( 8 ) :: Q, XX, S1, S2       ! intermerdiate terms

      REAL( 8 ) :: RK1, RK2, RK3       ! rate constants

      REAL( 8 ) :: PO3                 ! temp variable for O3

C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O1D Section
c    1) sum of the rate constants for all O1D loss reactions
c    2) get fractional yield of O3P from O1D loss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      O1D_S =                 RKI(    20 )                         ! O1D=0.2000D+01*OH
     &      +                 RKI(    21 )                         ! O1D=O3P

      O3P_S =                 RKI(    21 )                         ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(     1 )                         ! NO2=NO+O3P
     &     +                 RKI(     5 ) * YC ( O3P          )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(    15 )      ! NO2+NO3=NO+NO2
     &   +                 RXRAT(    16 )      ! NO3=NO
     &   +                 RXRAT(    23 )      ! HONO=NO+OH
     &   +                 RXRAT(   592 )      ! CLNO=NO+CL
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(    22 ) * YC ( OH           )   ! NO+OH=HONO
     &   +                 RKI(    57 ) * YC ( RO2XC        )   ! NO+RO2XC=
     &   +                 RKI(   591 ) * YC ( CL           )   ! NO+CL=CLNO
      L1    = 1.0D0 + L1 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO2 Section
c    R2_1 = production of NO2 from NO ( rates of form k[NO][x] )
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 ( 1/2 of NO2 formation rate included )
c            c)  NO3+NO2=NO+NO2 is not included for NO2
c    P2 =  remaining NO2 production terms 
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 (1/2 of NO2 formation rate included )
c    L2 = loss of NO2 (except rxns producing NO2 - they are in R1_2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R2_1 =                 RKI(     4 ) * YC ( O3P          )   ! NO+O3P=NO2
     &     +                 RKI(     9 ) * YC ( NO3          )   ! NO+NO3=0.2000D+...
     &     +    2.0000D+00 * RKI(    10 ) * YC ( NO           )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(    31 ) * YC ( HO2          )   ! NO+HO2=NO2+OH
     &     +                 RKI(    46 ) * YC ( MEO2         )   ! NO+MEO2=NO2+HCHO+HO2
     &     +                 RKI(    52 ) * YC ( RO2C         )   ! NO+RO2C=NO2
     &     +                 RKI(    66 ) * YC ( MECO3        )   ! NO+MECO3=NO2+CO2+...
     &     +                 RKI(    76 ) * YC ( RCO3         )   ! NO+RCO3=NO2+RO2C+...
     &     +                 RKI(    87 ) * YC ( BZCO3        )   ! NO+BZCO3=NO2+CO2+...
     &     +                 RKI(    99 ) * YC ( MACO3        )   ! NO+MACO3=NO2+CO2+...
     &     +                 RKI(   283 ) * YC ( HCOCO3       )   ! NO+HCOCO3=NO2+CO+...
     &     +                 RKI(   548 ) * YC ( IEPOXOO      )   ! NO+IEPOXOO=NO2+...
     &     +                 RKI(   601 ) * YC ( CLO          )   ! NO+CLO=NO2+CL
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(     9 )      ! NO+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    12 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    17 )      ! NO3=NO2+O3P
     &   +                 RXRAT(    24 )      ! OH+HONO=NO2
     &   +                 RXRAT(    26 )      ! OH+NO3=NO2+HO2
     &   +                 RXRAT(    28 )      ! HNO3=NO2+OH
     &   +                 RXRAT(    33 )      ! HNO4=NO2+HO2
     &   +    6.1000D-01 * RXRAT(    34 )      ! HNO4=0.6100D+00*NO2+...
     &   +                 RXRAT(    35 )      ! HNO4+OH=NO2
     &   +    8.0000D-01 * RXRAT(    39 )      ! NO3+HO2=0.8000D+...
     &   +    2.0000D+00 * RXRAT(    40 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    49 )      ! MEO2+NO3=NO2+HO2+HCHO
     &   +                 RXRAT(    54 )      ! RO2C+NO3=NO2
     &   +                 RXRAT(    59 )      ! RO2XC+NO3=NO2
     &   +                 RXRAT(    64 )      ! PAN=NO2+MECO3
     &   +    6.0000D-01 * RXRAT(    65 )      ! PAN=0.6000D+00*NO2+...
     &   +                 RXRAT(    68 )      ! MECO3+NO3=NO2+CO2+MEO2
     &   +                 RXRAT(    74 )      ! PAN2=NO2+RCO3
     &   +    6.0000D-01 * RXRAT(    75 )      ! PAN2=0.6000D+00*NO2+...
     &   +                 RXRAT(    78 )      ! RCO3+NO3=NO2+RO2C+...
     &   +                 RXRAT(    85 )      ! PBZN=NO2+BZCO3
     &   +    6.0000D-01 * RXRAT(    86 )      ! PBZN=0.6000D+00*NO2+...
     &   +                 RXRAT(    89 )      ! BZCO3+NO3=NO2+CO2+BZO+...
     &   +                 RXRAT(    97 )      ! MAPAN=NO2+MACO3
     &   +    6.0000D-01 * RXRAT(    98 )      ! MAPAN=0.6000D+00*NO2+...
     &   +                 RXRAT(   101 )      ! MACO3+NO3=NO2+CO2+...
     &   +                 RXRAT(   134 )      ! NO+xNO2=NO2+NO
     &   +                 RXRAT(   136 )      ! xNO2+NO3=NO2+NO3
     &   +    5.0000D-01 * RXRAT(   137 )      ! xNO2+MEO2=0.5000D+...
     &   +    5.0000D-01 * RXRAT(   138 )      ! xNO2+RO2C=0.5000D+...
     &   +    5.0000D-01 * RXRAT(   139 )      ! xNO2+RO2XC=0.5000D+...
     &   +                 RXRAT(   140 )      ! xNO2+MECO3=NO2+MECO3
     &   +                 RXRAT(   141 )      ! xNO2+RCO3=NO2+RCO3
     &   +                 RXRAT(   142 )      ! xNO2+BZCO3=NO2+BZCO3
     &   +                 RXRAT(   143 )      ! xNO2+MACO3=NO2+MACO3
     &   +    1.9000D-02 * RXRAT(   269 )      ! RNO3+OH=0.1900D-...
     &   +                 RXRAT(   270 )      ! RNO3=NO2+0.5540D+...
     &   +                 RXRAT(   595 )      ! CLONO=NO2+CL
     &   +                 RXRAT(   596 )      ! CLNO2=NO2+CL
     &   +                 RXRAT(   600 )      ! CL+NO3=NO2+CLO
     &   +                 RXRAT(   603 )      ! CLONO2=NO2+CLO
     &   +                 RXRAT(   605 )      ! CLONO2=NO2+CLO
     &   +    3.8000D-02 * RXRAT(   618 )      ! RNO3+CL=0.3800D-...
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(     6 ) * YC ( O3P          )   ! NO2+O3P=NO3
     &   +                 RKI(     8 ) * YC ( O3           )   ! NO2+O3=NO3
     &   +                 RKI(    11 ) * YC ( NO3          )   ! NO2+NO3=N2O5
     &   +                 RKI(    25 ) * YC ( OH           )   ! NO2+OH=HNO3
     &   +                 RKI(    32 ) * YC ( HO2          )   ! NO2+HO2=HNO4
     &   +                 RKI(    63 ) * YC ( MECO3        )   ! NO2+MECO3=PAN
     &   +                 RKI(    73 ) * YC ( RCO3         )   ! NO2+RCO3=PAN2
     &   +                 RKI(    84 ) * YC ( BZCO3        )   ! NO2+BZCO3=PBZN
     &   +                 RKI(    96 ) * YC ( MACO3        )   ! NO2+MACO3=MAPAN
     &   +                 RKI(   109 ) * YC ( TBUO         )   ! NO2+TBUO=RNO3
     &   +                 RKI(   111 ) * YC ( BZO          )   ! NO2+BZO=NPHE
     &   +                 RKI(   284 ) * YC ( HCOCO3       )   ! NO2+HCOCO3=HO2+...
     &   +                 RKI(   593 ) * YC ( CL           )   ! NO2+CL=CLONO
     &   +                 RKI(   594 ) * YC ( CL           )   ! NO2+CL=CLNO2
     &   +                 RKI(   602 ) * YC ( CLO          )   ! NO2+CLO=CLONO2
     &   +                 RKI(   713 )                         ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =    1.5000D-01 * RXRAT(    67 )      ! MECO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT(    77 )      ! RCO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT(    88 )      ! BZCO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT(   100 )      ! MACO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT(   285 )      ! HCOCO3+HO2=0.1500D+...
      P3 = YC0( O3 ) + P3 * DTC


      L3 =                 RKI(     3 ) * YC ( O3P          )   ! O3+O3P=
     &   +                 RKI(     8 ) * YC ( NO2          )   ! O3+NO2=NO3
     &   +                 RKI(    18 )                         ! O3=O1D
     &   +                 RKI(    19 )                         ! O3=O3P
     &   +                 RKI(    30 ) * YC ( OH           )   ! O3+OH=HO2
     &   +                 RKI(    36 ) * YC ( HO2          )   ! O3+HO2=OH
     &   +                 RKI(   247 ) * YC ( AFG1         )   ! O3+AFG1=0.8260D+...
     &   +                 RKI(   250 ) * YC ( AFG2         )   ! O3+AFG2=0.8260D+...
     &   +                 RKI(   253 ) * YC ( AFG3         )   ! O3+AFG3=0.4710D+...
     &   +                 RKI(   255 ) * YC ( MACR         )   ! O3+MACR=0.2080D+...
     &   +                 RKI(   260 ) * YC ( MVK          )   ! O3+MVK=0.1640D+...
     &   +                 RKI(   264 ) * YC ( IPRD         )   ! O3+IPRD=0.2850D+...
     &   +                 RKI(   275 ) * YC ( ACROLEIN     )   ! O3+ACROLEIN=...
     &   +                 RKI(   528 ) * YC ( ETHENE       )   ! O3+ETHENE=...
     &   +                 RKI(   532 ) * YC ( PROPENE      )   ! O3+PROPENE=...
     &   +                 RKI(   536 ) * YC ( BUTADIENE13  )   ! O3+BUTADIENE13=...
     &   +                 RKI(   540 ) * YC ( ISOPRENE     )   ! O3+ISOPRENE=...
     &   +                 RKI(   553 ) * YC ( APIN         )   ! O3+APIN=0.9000D-...
     &   +                 RKI(   557 ) * YC ( ACETYLENE    )   ! O3+ACETYLENE=...
     &   +                 RKI(   572 ) * YC ( OLE1         )   ! O3+OLE1=0.9500D-...
     &   +                 RKI(   576 ) * YC ( OLE2         )   ! O3+OLE2=0.9400D-...
     &   +                 RKI(   583 ) * YC ( TERP         )   ! O3+TERP=0.7800D-...
     &   +                 RKI(   587 ) * YC ( SESQ         )   ! O3+SESQ=0.7800D-...
     &   +                 RKI(   599 ) * YC ( CL           )   ! O3+CL=CLO
     &   +                 RKI(   721 )                         ! O3=
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT(    17 )      ! NO3=O3P+NO2
     &    +   O3P_S *       RXRAT(    18 )      ! O3=O1D
     &    +                 RXRAT(    19 )      ! O3=O3P
      P12 = YC0( O3P ) + P12 * DTC


      L12 =                 RKI(     2 )                         ! O3P=O3
     &    +                 RKI(     3 ) * YC ( O3           )   ! O3P+O3=
     &    +                 RKI(     4 ) * YC ( NO           )   ! O3P+NO=NO2
     &    +                 RKI(     5 ) * YC ( NO2          )   ! O3P+NO2=NO
     &    +                 RKI(     6 ) * YC ( NO2          )   ! O3P+NO2=NO3
     &    +                 RKI(   257 ) * YC ( MACR         )   ! O3P+MACR=RCHO
     &    +                 RKI(   261 ) * YC ( MVK          )   ! O3P+MVK=0.4500D+...
     &    +                 RKI(   277 ) * YC ( ACROLEIN     )   ! O3P+ACROLEIN=RCHO
     &    +                 RKI(   530 ) * YC ( ETHENE       )   ! O3P+ETHENE=0.8000D+...
     &    +                 RKI(   534 ) * YC ( PROPENE      )   ! O3P+PROPENE=0.4500D+...
     &    +                 RKI(   538 ) * YC ( BUTADIENE13  )   ! O3P+BUTADIENE13=...
     &    +                 RKI(   542 ) * YC ( ISOPRENE     )   ! O3P+ISOPRENE=0.2500D+...
     &    +                 RKI(   555 ) * YC ( APIN         )   ! O3P+APIN=PRD2+TRPRXN
     &    +                 RKI(   574 ) * YC ( OLE1         )   ! O3P+OLE1=0.4500D+...
     &    +                 RKI(   578 ) * YC ( OLE2         )   ! O3P+OLE2=0.7900D-...
     &    +                 RKI(   585 ) * YC ( TERP         )   ! O3P+TERP=0.2370D+...
     &    +                 RKI(   589 ) * YC ( SESQ         )   ! O3P+SESQ=0.2370D+...
      L12   = 1.0D0 + L12 * DTC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute reciprocal of loss terms
      L1_INV  = 1.0D0 / L1
      L2_INV  = 1.0D0 / L2
      L3_INV  = 1.0D0 / L3
      L12_INV = 1.0D0 / L12

c..compute specific k*delta t terms
      RK1 = RKI(   1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI(   2 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(   7 ) * DTC            ! k1_3  (NO+O3=NO2)

c..compute terms that are used to calulate a,b & c
      T1 = RK1  * L2_INV                ! J1   / ( 1.0 + Lno2 * dt )
      T2 = R1_2 * L2_INV                ! r1,2 / ( 1.0 + Lno2 * dt)
      T3 = R2_1 * L1_INV                ! r2,1 / ( 1.0 + Lno  * dt)
      T4 = RK2  * L12_INV               ! J2   / ( 1.0 + Lo3p * dt )
      T5 = T3   * P1 - T2 * P2          ! T3 * Pno - T2 * Pno2

      F1 = 1.0D0 + T2 + T3                ! factor in calculating a & b
      F2 = T1 * T4                      ! factor in calculating a & b
      F3 = L3 * L1 + RK3 * P1           ! (1 + Lo3 * dt) (1 + lno * dt )
                                        ! + k1,3 * dt * Pno

      PO3 = P3 + P12 * T4

      A = RK3 * ( F1  - F2 )

      B = F1 * F3 +  RK3 * ( F2 * ( P2 - P1 ) + PO3 +  T5 )

      C = RK3 * P1 * ( PO3 + P2 * F2 ) + F3 * T5

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B - 4.0D0 * A * C ) )

      XX = MAX( Q / A , C / Q  )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Species solutions
c       [NO]   = ( P1 + x ) / ( 1 + L1 )
c       [NO2]  = ( P2 - x ) / ( 1 + L2 )
c       [O3 ]  = ( P3 + Ko3p->O3 ) / (1 + K1,3 * [NO] + L3 )
c       [O3P]  = ( P12 + J1 * [NO2] ) / ( 1 + L12 )
c       [O1D] = ( yc0(o1d) + Ko3->o1d * [O3] *dtc) / ( 1 + O1D_S*dtc )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      YCP( NO ) = MAX( 0.0D0, ( P1 + XX ) * L1_INV )

      YCP( NO2 ) = MAX( 0.0D0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NO2 )

      S2 = T4 * S1

      YCP( O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NO ) )

      YCP( O3P ) = S1 * L12_INV

      YCP( O1D ) = ( YC0( O1D ) + RKI( 18 ) * YCP( O3 ) * DTC ) 
     &           / ( 1.0D0 + O1D_S * DTC )

      RETURN

      END


