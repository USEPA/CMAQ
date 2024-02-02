
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
C  PRECONDITIONS: For the CB6R5_AE7_AQ mechanism
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
      O1D_S =                 RKI(    10 )                         ! O1D=O
     &      +                 RKI(    11 )                         ! O1D=0.2000D+01*OH

      O3P_S =                 RKI(    10 )                         ! O1D=O

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(     1 )                         ! NO2=NO+O
     &     +                 RKI(     5 ) * YC ( O            )   ! NO2+O=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(    28 )      ! NO3=NO
     &   +                 RXRAT(    30 )      ! NO2+NO3=NO+NO2
     &   +                 RXRAT(    42 )      ! HONO+HONO=NO+NO2
     &   +                 RXRAT(    43 )      ! HONO=NO+OH
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(    40 ) * YC ( OH           )   ! NO+OH=HONO
     &   +                 RKI(    41 ) * YC ( NO2          )   ! NO+NO2=0.2000D+...
     &   +                 RKI(    83 ) * YC ( XO2N         )   ! NO+XO2N=0.5000D+...
     &   +    1.0000D-01 * RKI(   151 ) * YC ( ISO2         )   ! NO+ISO2=0.1000D+...
     &   +    8.2000D-02 * RKI(   180 ) * YC ( BZO2         )   ! NO+BZO2=0.9180D+...
     &   +    1.4000D-01 * RKI(   185 ) * YC ( TO2          )   ! NO+TO2=0.8600D+...
     &   +    1.4000D-01 * RKI(   191 ) * YC ( XLO2         )   ! NO+XLO2=0.8600D+...
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
      R2_1 =                 RKI(     4 ) * YC ( O            )   ! NO+O=NO2
     &     +    2.0000D+00 * RKI(    24 ) * YC ( NO           )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(    25 ) * YC ( HO2          )   ! NO+HO2=NO2+OH
     &     +                 RKI(    29 ) * YC ( NO3          )   ! NO+NO3=0.2000D+...
     &     +                 RKI(    53 ) * YC ( C2O3         )   ! NO+C2O3=NO2+MEO2+RO2
     &     +                 RKI(    61 ) * YC ( CXO3         )   ! NO+CXO3=NO2+ALD2+...
     &     +                 RKI(    71 ) * YC ( MEO2         )   ! NO+MEO2=NO2+HO2+FORM
     &     +                 RKI(    75 ) * YC ( XO2H         )   ! NO+XO2H=NO2+HO2
     &     +                 RKI(    79 ) * YC ( XO2          )   ! NO+XO2=NO2
     &     +                 RKI(   103 ) * YC ( HCO3         )   ! NO+HCO3=NO2+FACD+HO2
     &     +    9.0000D-01 * RKI(   151 ) * YC ( ISO2         )   ! NO+ISO2=0.9000D+...
     &     +                 RKI(   167 ) * YC ( EPX2         )   ! NO+EPX2=NO2+...
     &     +    9.1800D-01 * RKI(   180 ) * YC ( BZO2         )   ! NO+BZO2=0.9180D+...
     &     +    8.6000D-01 * RKI(   185 ) * YC ( TO2          )   ! NO+TO2=0.8600D+...
     &     +    8.6000D-01 * RKI(   191 ) * YC ( XLO2         )   ! NO+XLO2=0.8600D+...
     &     +                 RKI(   212 ) * YC ( OPO3         )   ! NO+OPO3=NO2+...
     &     +                 RKI(   230 ) * YC ( CLO          )   ! NO+CLO=NO2+CL
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(    27 )      ! NO3=NO2+O
     &   +                 RXRAT(    29 )      ! NO+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    31 )      ! NO3+O=NO2
     &   +                 RXRAT(    32 )      ! NO3+OH=NO2+HO2
     &   +                 RXRAT(    33 )      ! NO3+HO2=NO2+OH
     &   +                 RXRAT(    34 )      ! NO3+O3=NO2
     &   +    2.0000D+00 * RXRAT(    35 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    37 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    38 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    42 )      ! HONO+HONO=NO2+NO
     &   +                 RXRAT(    44 )      ! HONO+OH=NO2
     &   +                 RXRAT(    47 )      ! HNO3=NO2+OH
     &   +                 RXRAT(    49 )      ! PNA=NO2+HO2
     &   +    5.9000D-01 * RXRAT(    50 )      ! PNA=0.5900D+00*NO2+...
     &   +                 RXRAT(    51 )      ! PNA+OH=NO2
     &   +                 RXRAT(    55 )      ! PAN=NO2+C2O3
     &   +    6.0000D-01 * RXRAT(    56 )      ! PAN=0.6000D+00*NO2+...
     &   +                 RXRAT(    63 )      ! PANX=NO2+CXO3
     &   +    6.0000D-01 * RXRAT(    64 )      ! PANX=0.6000D+00*NO2+...
     &   +                 RXRAT(    92 )      ! NTR1=NO2
     &   +    5.0000D-01 * RXRAT(   140 )      ! ETH+NO3=0.5000D+...
     &   +    5.0000D-01 * RXRAT(   144 )      ! OLE+NO3=0.5000D+...
     &   +    5.0000D-01 * RXRAT(   148 )      ! IOLE+NO3=0.5000D+...
     &   +    3.5000D-01 * RXRAT(   157 )      ! ISOP+NO3=0.3500D+...
     &   +    1.4200D-01 * RXRAT(   160 )      ! ISPD+NO3=0.1420D+...
     &   +    4.4400D-01 * RXRAT(   170 )      ! INTR+OH=0.4440D+...
     &   +    4.7000D-01 * RXRAT(   174 )      ! TERP+NO3=0.4700D+...
     &   +    4.7000D-01 * RXRAT(   178 )      ! APIN+NO3=0.4700D+...
     &   +    5.0000D-01 * RXRAT(   205 )      ! XOPN+NO3=0.5000D+...
     &   +                 RXRAT(   214 )      ! OPAN=NO2+OPO3
     &   +    5.0000D-01 * RXRAT(   218 )      ! OPAN+OH=0.5000D+...
     &   +                 RXRAT(   219 )      ! PANX+OH=NO2+ALD2
     &   +                 RXRAT(   252 )      ! CLNO2=NO2+CL
     &   +                 RXRAT(   254 )      ! CLNO3=NO2+CLO
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(     6 ) * YC ( O            )   ! NO2+O=NO3
     &   +                 RKI(    26 ) * YC ( O3           )   ! NO2+O3=NO3
     &   +                 RKI(    36 ) * YC ( NO3          )   ! NO2+NO3=N2O5
     &   +                 RKI(    41 ) * YC ( NO           )   ! NO2+NO=0.2000D+...
     &   +                 RKI(    45 ) * YC ( OH           )   ! NO2+OH=HNO3
     &   +                 RKI(    48 ) * YC ( HO2          )   ! NO2+HO2=PNA
     &   +                 RKI(    54 ) * YC ( C2O3         )   ! NO2+C2O3=PAN
     &   +                 RKI(    62 ) * YC ( CXO3         )   ! NO2+CXO3=PANX
     &   +                 RKI(   135 ) * YC ( ROR          )   ! NO2+ROR=NTR1
     &   +                 RKI(   197 ) * YC ( CRO          )   ! NO2+CRO=CRON
     &   +                 RKI(   213 ) * YC ( OPO3         )   ! NO2+OPO3=OPAN
     &   +                 RKI(   225 ) * YC ( OH           )   ! NO2+OH=HNO3
     &   +                 RKI(   253 ) * YC ( CLO          )   ! NO2+CLO=CLNO3
     &   +                 RKI(   278 )                         ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =    1.3000D-01 * RXRAT(    57 )      ! C2O3+HO2=0.1300D+...
     &   +    1.3000D-01 * RXRAT(    65 )      ! CXO3+HO2=0.1300D+...
     &   +    1.3000D-01 * RXRAT(   215 )      ! OPO3+HO2=0.1300D+...
      P3 = YC0( O3 ) + P3 * DTC


      L3 =                 RKI(     7 ) * YC ( O            )   ! O3+O=
     &   +                 RKI(     8 )                         ! O3=O
     &   +                 RKI(     9 )                         ! O3=O1D
     &   +                 RKI(    12 ) * YC ( OH           )   ! O3+OH=HO2
     &   +                 RKI(    13 ) * YC ( HO2          )   ! O3+HO2=OH
     &   +                 RKI(    26 ) * YC ( NO2          )   ! O3+NO2=NO3
     &   +                 RKI(    34 ) * YC ( NO3          )   ! O3+NO3=NO2
     &   +                 RKI(   139 ) * YC ( ETH          )   ! O3+ETH=FORM+...
     &   +                 RKI(   143 ) * YC ( OLE          )   ! O3+OLE=0.2950D+...
     &   +                 RKI(   147 ) * YC ( IOLE         )   ! O3+IOLE=0.7320D+...
     &   +                 RKI(   156 ) * YC ( ISOP         )   ! O3+ISOP=0.6000D+...
     &   +                 RKI(   159 ) * YC ( ISPD         )   ! O3+ISPD=0.4000D-...
     &   +                 RKI(   173 ) * YC ( TERP         )   ! O3+TERP=0.5700D+...
     &   +                 RKI(   177 ) * YC ( APIN         )   ! O3+APIN=0.5700D+...
     &   +                 RKI(   204 ) * YC ( XOPN         )   ! O3+XOPN=0.1200D+...
     &   +                 RKI(   208 ) * YC ( OPEN         )   ! O3+OPEN=0.1400D+...
     &   +                 RKI(   228 ) * YC ( CL           )   ! O3+CL=CLO
     &   +                 RKI(   279 )                         ! O3=
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT(     8 )      ! O3=O
     &    +   O3P_S *       RXRAT(     9 )      ! O3=O1D
     &    +                 RXRAT(    16 )      ! OH+OH=O
     &    +                 RXRAT(    27 )      ! NO3=O+NO2
      P12 = YC0( O ) + P12 * DTC


      L12 =                 RKI(     2 )                         ! O=O3
     &    +                 RKI(     4 ) * YC ( NO           )   ! O+NO=NO2
     &    +                 RKI(     5 ) * YC ( NO2          )   ! O+NO2=NO
     &    +                 RKI(     6 ) * YC ( NO2          )   ! O+NO2=NO3
     &    +                 RKI(     7 ) * YC ( O3           )   ! O+O3=
     &    +                 RKI(    14 ) * YC ( OH           )   ! O+OH=HO2
     &    +                 RKI(    15 ) * YC ( HO2          )   ! O+HO2=OH
     &    +                 RKI(    23 ) * YC ( H2O2         )   ! O+H2O2=OH+HO2
     &    +                 RKI(    31 ) * YC ( NO3          )   ! O+NO3=NO2
     &    +                 RKI(    99 ) * YC ( FORM         )   ! O+FORM=OH+HO2+CO
     &    +                 RKI(   105 ) * YC ( ALD2         )   ! O+ALD2=C2O3+OH
     &    +                 RKI(   109 ) * YC ( ALDX         )   ! O+ALDX=CXO3+OH
     &    +                 RKI(   137 ) * YC ( ETH          )   ! O+ETH=FORM+HO2+CO+...
     &    +                 RKI(   141 ) * YC ( OLE          )   ! O+OLE=0.2000D+00*ALD2+...
     &    +                 RKI(   145 ) * YC ( IOLE         )   ! O+IOLE=0.1240D+...
     &    +                 RKI(   150 ) * YC ( ISOP         )   ! O+ISOP=0.7500D+...
     &    +                 RKI(   171 ) * YC ( TERP         )   ! O+TERP=0.1500D+...
     &    +                 RKI(   175 ) * YC ( APIN         )   ! O+APIN=0.1500D+...
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
      RK3 = RKI(   3 ) * DTC            ! k1_3  (NO+O3=NO2)

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

      YCP( O ) = S1 * L12_INV

      YCP( O1D ) = ( YC0( O1D ) + RKI( 9 ) * YCP( O3 ) * DTC ) 
     &           / ( 1.0D0 + O1D_S * DTC )

      RETURN

      END


