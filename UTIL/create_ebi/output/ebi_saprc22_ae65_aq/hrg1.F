
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
C  PRECONDITIONS: For the SAPRC22_AE65_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Dec 19, 2024
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
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(    22 ) * YC ( OH           )   ! NO+OH=HONO
     &   +                 RKI(    31 ) * YC ( HO2          )   ! NO+HO2=HNO3
     &   +                 RKI(    32 ) * YC ( HO2          )   ! NO+HO2=HNO3
     &   +                 RKI(    62 ) * YC ( RO2XC        )   ! NO+RO2XC=
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
     &     +                 RKI(    30 ) * YC ( HO2          )   ! NO+HO2=NO2+OH
     &     +                 RKI(    57 ) * YC ( RO2C         )   ! NO+RO2C=NO2
     &     +                 RKI(    67 ) * YC ( MEO2         )   ! NO+MEO2=NO2+HCHO+HO2
     &     +                 RKI(    72 ) * YC ( ETO2         )   ! NO+ETO2=NO2+HO2+...
     &     +                 RKI(    77 ) * YC ( BZO2         )   ! NO+BZO2=NO2+BZO
     &     +                 RKI(    82 ) * YC ( MECO3        )   ! NO+MECO3=NO2+...
     &     +                 RKI(    89 ) * YC ( BZCO3        )   ! NO+BZCO3=NO2+CO2+...
     &     +                 RKI(   455 ) * YC ( R2CO3        )   ! NO+R2CO3=NO2+...
     &     +                 RKI(   462 ) * YC ( MACO3        )   ! NO+MACO3=NO2+...
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(     9 )      ! NO+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    12 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    17 )      ! NO3=NO2+O3P
     &   +                 RXRAT(    24 )      ! OH+HONO=NO2
     &   +                 RXRAT(    26 )      ! OH+NO3=NO2+HO2
     &   +                 RXRAT(    28 )      ! HNO3=NO2+OH
     &   +                 RXRAT(    34 )      ! HNO4=NO2+HO2
     &   +    8.0000D-01 * RXRAT(    35 )      ! HNO4=0.8000D+00*NO2+...
     &   +                 RXRAT(    36 )      ! HNO4+OH=NO2
     &   +                 RXRAT(    40 )      ! NO3+HO2=NO2+OH
     &   +    2.0000D+00 * RXRAT(    41 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    59 )      ! RO2C+NO3=NO2
     &   +                 RXRAT(    64 )      ! RO2XC+NO3=NO2
     &   +                 RXRAT(    69 )      ! MEO2+NO3=NO2+HO2+HCHO
     &   +                 RXRAT(    74 )      ! ETO2+NO3=NO2+HO2+MECHO
     &   +                 RXRAT(    79 )      ! BZO2+NO3=NO2+BZO
     &   +                 RXRAT(    84 )      ! MECO3+NO3=NO2+MEO2+...
     &   +                 RXRAT(    91 )      ! BZCO3+NO3=NO2+CO2+...
     &   +                 RXRAT(   121 )      ! PAN=NO2+MECO3+SumRCO3
     &   +    6.0000D-01 * RXRAT(   122 )      ! PAN=0.6000D+00*NO2+...
     &   +                 RXRAT(   130 )      ! PBZN=NO2+BZCO3+SumRCO3
     &   +    6.0000D-01 * RXRAT(   131 )      ! PBZN=0.6000D+00*NO2+...
     &   +                 RXRAT(   132 )      ! NPHE+OH=NO2+BZO
     &   +                 RXRAT(   160 )      ! NO+xNO2=NO2+NO
     &   +                 RXRAT(   162 )      ! xNO2+NO3=NO2+NO3
     &   +    5.0000D-01 * RXRAT(   163 )      ! xNO2+SumRO2=0.5000D+...
     &   +                 RXRAT(   164 )      ! xNO2+SumRCO3=NO2+SumRCO3
     &   +                 RXRAT(   457 )      ! R2CO3+NO3=NO2+0.9500D+...
     &   +                 RXRAT(   464 )      ! MACO3+NO3=NO2+MEO2+...
     &   +    1.2000D-01 * RXRAT(   490 )      ! ISOP_N3=0.1200D+...
     &   +    1.0000D-02 * RXRAT(   653 )      ! OLEA2_N3=0.1000D-...
     &   +    3.4000D-01 * RXRAT(   687 )      ! RCNO3+OH=0.3400D+...
     &   +    7.7000D-01 * RXRAT(   690 )      ! RCNO3=0.7700D+00*NO2+...
     &   +    6.0000D-01 * RXRAT(   693 )      ! RHNO3+OH=0.6000D+...
     &   +                 RXRAT(   694 )      ! RHNO3=NO2+0.9400D+...
     &   +    4.2000D-01 * RXRAT(   695 )      ! RANO3+OH=0.4200D+...
     &   +    3.7000D-01 * RXRAT(   697 )      ! RPNO3+OH=0.3700D+...
     &   +    3.0000D-02 * RXRAT(   698 )      ! RPNO3_OH=0.3000D-...
     &   +    8.1000D-01 * RXRAT(   700 )      ! RPNO3=0.8100D+00*NO2+...
     &   +    3.0000D-02 * RXRAT(   701 )      ! RPNO3_HV=0.3000D-...
     &   +    5.3000D-01 * RXRAT(   703 )      ! RDNO3+OH=0.5300D+...
     &   +    1.8600D+00 * RXRAT(   704 )      ! RDNO3=0.1860D+01*NO2+...
     &   +    4.0000D-02 * RXRAT(   705 )      ! RDNO3_HV=0.4000D-...
     &   +    1.7000D-01 * RXRAT(   707 )      ! R1NO3+OH=0.1700D+...
     &   +                 RXRAT(   708 )      ! R1NO3=NO2+0.1900D+...
     &   +    6.0000D-02 * RXRAT(   709 )      ! R2NO3+OH=0.6000D-...
     &   +                 RXRAT(   710 )      ! R2NO3=NO2+R2NO3_HV+...
     &   +                 RXRAT(   744 )      ! PAN2=NO2+R2CO3+SumRCO3
     &   +                 RXRAT(   746 )      ! PAN2=NO2+R2CO3+SumRCO3
     &   +                 RXRAT(   747 )      ! APANS=NO2+MACO3+SumRCO3
     &   +    5.0000D-02 * RXRAT(   749 )      ! APANS+O3=0.5000D-...
     &   +    6.0000D-01 * RXRAT(   751 )      ! APANS=0.6000D+00*NO2+...
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(     6 ) * YC ( O3P          )   ! NO2+O3P=NO3
     &   +                 RKI(     8 ) * YC ( O3           )   ! NO2+O3=NO3
     &   +                 RKI(    11 ) * YC ( NO3          )   ! NO2+NO3=N2O5
     &   +                 RKI(    25 ) * YC ( OH           )   ! NO2+OH=HNO3
     &   +                 RKI(    33 ) * YC ( HO2          )   ! NO2+HO2=HNO4
     &   +                 RKI(    83 ) * YC ( MECO3        )   ! NO2+MECO3=PAN
     &   +                 RKI(    88 ) * YC ( BZCO3        )   ! NO2+BZCO3=PBZN
     &   +                 RKI(    94 ) * YC ( TBUO         )   ! NO2+TBUO=R1NO3
     &   +                 RKI(    96 ) * YC ( BZO          )   ! NO2+BZO=NPHE
     &   +                 RKI(   100 ) * YC ( NPRAD        )   ! NO2+NPRAD=NPHE
     &   +                 RKI(   103 ) * YC ( PNAMIN       )   ! NO2+PNAMIN=NAMIN
     &   +                 RKI(   106 ) * YC ( HCHO2        )   ! NO2+HCHO2=HCHO+NO3
     &   +                 RKI(   109 ) * YC ( MECHO2       )   ! NO2+MECHO2=MECHO+NO3
     &   +                 RKI(   112 ) * YC ( RCHO2        )   ! NO2+RCHO2=RCHO+NO3
     &   +                 RKI(   456 ) * YC ( R2CO3        )   ! NO2+R2CO3=PAN2
     &   +                 RKI(   463 ) * YC ( MACO3        )   ! NO2+MACO3=APANS
     &   +                 RKI(   761 )                         ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =    1.3000D-01 * RXRAT(    85 )      ! MECO3+HO2=0.1300D+...
     &   +    1.3000D-01 * RXRAT(    90 )      ! BZCO3+HO2=0.1300D+...
     &   +    1.3000D-01 * RXRAT(   458 )      ! R2CO3+HO2=0.1300D+...
     &   +    1.3000D-01 * RXRAT(   465 )      ! MACO3+HO2=0.1300D+...
      P3 = YC0( O3 ) + P3 * DTC


      L3 =                 RKI(     3 ) * YC ( O3P          )   ! O3+O3P=
     &   +                 RKI(     8 ) * YC ( NO2          )   ! O3+NO2=NO3
     &   +                 RKI(    18 )                         ! O3=O1D
     &   +                 RKI(    19 )                         ! O3=O3P
     &   +                 RKI(    29 ) * YC ( OH           )   ! O3+OH=HO2
     &   +                 RKI(    37 ) * YC ( HO2          )   ! O3+HO2=OH
     &   +                 RKI(    98 ) * YC ( BZO          )   ! O3+BZO=BZO2+SumRO2
     &   +                 RKI(   472 ) * YC ( ETHEN        )   ! O3+ETHEN=0.1700D+...
     &   +                 RKI(   478 ) * YC ( PROPE        )   ! O3+PROPE=...
     &   +                 RKI(   486 ) * YC ( ISOP         )   ! O3+ISOP=ISOP_O3+...
     &   +                 RKI(   496 ) * YC ( BUT13        )   ! O3+BUT13=...
     &   +                 RKI(   504 ) * YC ( APINE        )   ! O3+APINE=0.6900D+...
     &   +                 RKI(   510 ) * YC ( BPINE        )   ! O3+BPINE=0.3900D+...
     &   +                 RKI(   514 ) * YC ( ACETL        )   ! O3+ACETL=0.2600D+...
     &   +                 RKI(   546 ) * YC ( ACRO         )   ! O3+ACRO=0.1500D+...
     &   +                 RKI(   558 ) * YC ( MACR         )   ! O3+MACR=0.1900D+...
     &   +                 RKI(   564 ) * YC ( MVK          )   ! O3+MVK=0.1800D+...
     &   +                 RKI(   583 ) * YC ( OLE1         )   ! O3+OLE1=0.2600D+...
     &   +                 RKI(   587 ) * YC ( OLE2         )   ! O3+OLE2=OLE2_O3+...
     &   +                 RKI(   593 ) * YC ( OLE3         )   ! O3+OLE3=0.5800D+...
     &   +                 RKI(   597 ) * YC ( OLE4         )   ! O3+OLE4=OLE4_O3+...
     &   +                 RKI(   605 ) * YC ( TERP         )   ! O3+TERP=TERP_O3+...
     &   +                 RKI(   615 ) * YC ( SESQ         )   ! O3+SESQ=SESQ_O3+...
     &   +                 RKI(   628 ) * YC ( FURNS        )   ! O3+FURNS=0.3900D+...
     &   +                 RKI(   631 ) * YC ( STYRS        )   ! O3+STYRS=0.8000D-...
     &   +                 RKI(   633 ) * YC ( AMINS        )   ! O3+AMINS=0.6100D+...
     &   +                 RKI(   641 ) * YC ( OLEA1        )   ! O3+OLEA1=0.6200D+...
     &   +                 RKI(   649 ) * YC ( OLEA2        )   ! O3+OLEA2=...
     &   +                 RKI(   663 ) * YC ( LVKS         )   ! O3+LVKS=LVKS_O3+...
     &   +                 RKI(   670 ) * YC ( OLEP         )   ! O3+OLEP=OLEP_O3+...
     &   +                 RKI(   749 ) * YC ( APANS        )   ! O3+APANS=0.5000D-...
     &   +                 RKI(   766 )                         ! O3=
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
     &    +                 RKI(   474 ) * YC ( ETHEN        )   ! O3P+ETHEN=ETHEN_OP+...
     &    +                 RKI(   482 ) * YC ( PROPE        )   ! O3P+PROPE=0.2500D+...
     &    +                 RKI(   492 ) * YC ( ISOP         )   ! O3P+ISOP=0.2300D+...
     &    +                 RKI(   500 ) * YC ( BUT13        )   ! O3P+BUT13=0.2500D+...
     &    +                 RKI(   506 ) * YC ( APINE        )   ! O3P+APINE=0.5000D+...
     &    +                 RKI(   512 ) * YC ( BPINE        )   ! O3P+BPINE=0.5000D+...
     &    +                 RKI(   585 ) * YC ( OLE1         )   ! O3P+OLE1=0.2500D+...
     &    +                 RKI(   591 ) * YC ( OLE2         )   ! O3P+OLE2=0.2100D+...
     &    +                 RKI(   595 ) * YC ( OLE3         )   ! O3P+OLE3=0.5000D+...
     &    +                 RKI(   601 ) * YC ( OLE4         )   ! O3P+OLE4=0.5000D+...
     &    +                 RKI(   611 ) * YC ( TERP         )   ! O3P+TERP=0.1600D+...
     &    +                 RKI(   621 ) * YC ( SESQ         )   ! O3P+SESQ=0.1300D+...
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


