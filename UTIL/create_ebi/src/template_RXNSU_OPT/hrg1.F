
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
R1  PRECONDITIONS: For SAPRC99 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2003
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

RE      REAL( 8 ) :: EXN_S               ! sum of NO2EX loss frequencies
RE      REAL( 8 ) :: NO2_S               ! stoich coeff for NO2 from NO2EX


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
S1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O1D2 Section
c    1) sum of the rate constants for all O1D loss reactions
c    2) get fractional yield of O3P from O1D loss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      O1D_DNM = RKI(   19 ) + RKI(   20 )

      O3P_S   = RKI(   20 ) / O1D_DNM

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      R1_2 =           RKI(    1 )                   ! NO2+hv=NO 
     &       +         RKI(    5 ) * YC( O3P  )      ! NO2+O3P=NO  
      R1_2 = R1_2 * DTC   

      P1  =            RXRAT(  14 )                 ! NO2+NO3=NO+NO2
     &       +         RXRAT(  15 )                 ! NO3+hv=NO
     &       +         RXRAT(  22 )                 ! HONO+hv=NO
      P1 = YC0( NCELL,   NO ) + P1 * DTC

      L1  =            RKI(   21 ) * YC(    HO )     ! NO+HO=HONO
     &       +         RKI(   23 ) * YC( RO2_N )     ! NO+RO2_N=RNO3
      L1 = 1.0D0 + L1 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO2 Section
c    R2_1 = production of NO2 from NO ( rates of form k[NO][x] )
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 ( 1/2 of rate included )
c            c)  NO3+NO2=NO+NO2 is not included for NO2
c    P2 =  remaining NO2 production terms 
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 (1/2 of rate included )
c    L2 = loss of NO2 (except rxns producing NO2 - they are in R1_2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      R2_1 =           RKI(    4 ) * YC(     O3P )       ! NO+O3P=NO2 
     &       +         RKI(    9 ) * YC(     NO3 )       ! NO+NO3=2*NO2 (1/2)
     &       + 2.000D0 * RKI(   10 ) * YC(      NO )       ! NO+NO=2*NO2
     &       +         RKI(   31 ) * YC(     HO2 )       ! NO+HO2=NO2
     &       +         RKI(   46 ) * YC(    C_O2 )       ! NO+C_O2=NO2
     &       +         RKI(   51 ) * YC(   RO2_R )       ! NO+RO2_R=NO2
     &       +         RKI(   56 ) * YC(    R2O2 )       ! NO+R2O2=NO2
     &       +         RKI(   62 ) * YC(   RO2_N )       ! NO+RO2_N=NO2
     &       +         RKI(   71 ) * YC(  CCO_O2 )       ! NO+CCO_O2=NO2
     &       +         RKI(   81 ) * YC(  RCO_O2 )  ! NO+RCO_O2=NO2
     &       +         RKI(   92 ) * YC( BZCO_O2 )  ! NO+BZCO_O2=NO2
     &       +         RKI(  104 ) * YC( MA_RCO3 )  ! NO+MA_RCO3=NO2
     &       +         RKI(  128 ) * YC(   HOCOO )  ! NO+HOCOO=NO2
      R2_1 = R2_1 * DTC

      P2  =           RXRAT(  10 )                 ! NO+NO3=2*NO2 (1/2)
     &      +         RXRAT(  12 )                 ! N2O5=NO2
     &      +         RXRAT(  16 )                 ! NO3+hv=NO2
     &      +         RXRAT(  23 )                 ! HONO+hv=NO2
     &      +         RXRAT(  24 )                 ! HONO+HO=NO2
     &      +         RXRAT(  26 )                 ! NO3+HO=NO2
     &      +         RXRAT(  28 )                 ! HNO3+hv=NO2
     &      +         RXRAT(  33 )                 ! HNO4=NO2
     &      + 0.610D0 * RXRAT(  34 )                 ! HNO4+hv=0.610*NO2
     &      +         RXRAT(  35 )                 ! HNO4+HO=NO2
     &      + 0.800D0 * RXRAT(  39 )                 ! NO3+HO2=0.800*NO2
     &      + 2.000D0 * RXRAT(  40 )                 ! NO3+NO3=2*NO2
     &      +         RXRAT(  48 )                 ! NO3+C_O2=NO2
     &      +         RXRAT(  58 )                 ! NO3+RO2_R=NO2
     &      +         RXRAT(  65 )                 ! NO3+RO2_N=NO2
     &      +         RXRAT(  70 )                 ! PAN=NO2
     &      +         RXRAT(  73 )                 ! NO3+CCO_O2=NO2
     &      +         RXRAT(  80 )                 ! PAN2=NO2
     &      +         RXRAT(  83 )                 ! NO3+RCO_O2=NO2
     &      +         RXRAT(  91 )                 ! PBZN=NO2
     &      +         RXRAT(  94 )                 ! NO3+BZCO_O2=NO2
     &      +         RXRAT( 103 )                 ! MA_PAN=NO2
     &      +         RXRAT( 106 )                 ! NO3+MA_RCO3=NO2
     &      + 0.338 * RXRAT( 176 )                 ! RNO3+HO=0.338*NO2
     &      +         RXRAT( 177 )                 ! RNO3+hv=NO2
     &      + 0.187 * RXRAT( 191 )                 ! NO3+ISOPRENE=0.187*NO2
     &      + 0.474 * RXRAT( 195 )                 ! NO3+TRP1=0.474*NO2
     &      + 0.391D0 * RXRAT( 210 )                 ! NO3+OLE2=0.391*NO2
      P2 = YC0( NCELL,   NO2 ) + P2 * DTC

      L2  =           RKI(    6 ) * YC(     O3P )   ! NO2+O3P=NO3
     &      +         RKI(    9 ) * YC(      O3 )   ! NO2+O3=NO3
     &      +         RKI(   12 ) * YC(     NO3 )   ! NO2+NO3=N2O5
     &      +         RKI(   25 ) * YC(      NO )   ! NO2+OH=HNO3
     &      +         RKI(   32 ) * YC(     HO2 )   ! NO2+HO2=HNO4
     &      +         RKI(   69 ) * YC(  CCO_O2 )   ! NO2+CCO_O2=PAN
     &      +         RKI(   79 ) * YC(  RCO_O2 )   ! NO2+RCO_O2=PAN2
     &      +         RKI(   90 ) * YC( BZCO_O2 )   ! NO2+BZCO_O2=PBZN
     &      +         RKI(  102 ) * YC( MA_RCO3 )   ! NO2+MA_RCO3=MA_PAN
     &      + 0.800D0 * RKI(  115 ) * YC(   TBU_O )   ! NO2+TBU_O=RNO3
     &      +         RKI(  117 ) * YC(    BZ_O )   ! NO2+BZ_O=NPHE
     &      +         RKI(  120 ) * YC( BZNO2_O )   ! NO2+BZNO2_O=
      L2 = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      P3  =   0.250D0 * RXRAT(  72 )                  ! CCO_O2+HO2=0.250*O3
     &      + 0.250D0 * RXRAT(  82 )                  ! RCO_O2+HO2=0.250*O3
     &      + 0.250D0 * RXRAT(  93 )                  ! BZCO_O2+HO2=0.250*O3
     &      + 0.250D0 * RXRAT( 105 )                  ! MA_RCO3+HO2=0.250*O3
      P3 = YC0( NCELL,  O3 ) + P3 * DTC

      L3 =            RKI(    3 ) * YC(      O3P )   ! O3+O3P=
     &      +         RKI(    8 ) * YC(      NO2 )   ! O3+NO2=NO3
     &      +         RKI(   17 )                    ! O3+hv=O3P
     &      +         RKI(   18 )                    ! O3+hv=O1D2
     &      +         RKI(   30 ) * YC(       HO )   ! O3+OH=HO2
     &      +         RKI(   36 ) * YC(      HO2 )   ! O3+HO2=OH
     &      +         RKI(  162 ) * YC( METHACRO )   ! O3+METHACRO=
     &      +         RKI(  167 ) * YC(      MVK )   ! O3+MVK=
     &      +         RKI(  171 ) * YC(  ISOPROD )   ! O3+ISOPROD=
     &      +         RKI(  179 ) * YC(     DCB1 )   ! O3+DCB1=
     &      +         RKI(  186 ) * YC(   ETHENE )   ! O3+ETHENE=
     &      +         RKI(  190 ) * YC( ISOPRENE )   ! O3+ISOPRENE=
     &      +         RKI(  194 ) * YC(     TRP1 )   ! O3+TRP1=
     &      +         RKI(  205 ) * YC(     OLE1 )   ! O3+OLE1=
     &      +         RKI(  209 ) * YC(     OLE2 )   ! O3+OLE2=
      L3 = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      P12 =           RXRAT(  16 )                  ! NO3+hv=O3P
     &      +         RXRAT(  17 )                  ! O3+hv=O3P
     &      + O3P_S * RXRAT(  18 )                  ! O3+hv=O1D2=>O3P
      P12 = YC0( NCELL, O ) + P12 * DTC 

      L12 =           RKI(    2 )                    ! O3P=O3
     &      +         RKI(    4 ) * YC(       NO )   ! O3P+NO=NO2
     &      +         RKI(    5 ) * YC(      NO2 )   ! O3P+NO2=NO
     &      +         RKI(    6 ) * YC(      NO2 )   ! O3P+NO=NO3
     &      +         RKI(  164 ) * YC( METHACRO )   ! O3P+METHACRO=
     &      +         RKI(  168 ) * YC(      MVK )   ! O3P+MVK=
     &      +         RKI(  188 ) * YC(   ETHENE )   ! O3P+ETHENE=
     &      +         RKI(  192 ) * YC( ISOPRENE )   ! O3P+ISOPRENE=
     &      +         RKI(  196 ) * YC(     TRP1 )   ! O3P+TRP1=
     &      +         RKI(  207 ) * YC(     OLE1 )   ! O3P+OLE1=
     &      +         RKI(  211 ) * YC(     OLE2 )   ! O3P+OLE2=
      L12 = 1.0D0 + L12 * DTC
S1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute reciprocal of loss terms
      L1_INV  = 1.0D0 / L1
      L2_INV  = 1.0D0 / L2
      L3_INV  = 1.0D0 / L3
      L12_INV = 1.0D0 / L12

c..compute specific k*delta t terms
R3      RK1 = RKI(    1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
R3      RK2 = RKI(    2 ) * DTC            ! J2    (O3P+O2=O3)
R3      RK3 = RKI(    3 ) * DTC            ! k1_3  (NO+O3=NO2)

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
S2

      YCP( NCELL, NO  ) = MAX( 0.0D0, ( P1 + XX ) * L1_INV )

      YCP( NCELL, NO2 ) = MAX( 0.0D0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NCELL, NO2 )

      S2 = T4 * S1

      YCP( O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NO ) )

      YCP( O3P ) = S1 * L12_INV

      YCP( O1D2 ) = ( YC0( O1D2 ) + RKI(  18 ) * YCP( O3 ) * DTC )
     &            / ( 1.0D0 + O1D_DNM * DTC )

      RETURN

      END
     















      
