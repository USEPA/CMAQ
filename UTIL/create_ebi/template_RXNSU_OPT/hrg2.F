
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
R1  PRECONDITIONS: For SAPRC99 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, September, 2003
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
RE     REAL( 8 ) ::    EXN_S                 ! sum of NO2EX loss frequencies
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
S1

c..stoichiometric coefficient for production of HO from O3 via O1D
      OH_S = 2.0D0D0 * RKI(   19 ) / ( RKI(   19 ) + RKI(   20 ) )


c..Production of HO from HO2 (r4,5 terms )
      R4_5   =          RKI(   31 ) * YC(   NO )   ! HO2+NO=HO+NO2
     &        +         RKI(   36 ) * YC(   O3 )   ! HO2+O3=HO
     &        + 0.800D0 * RKI(   39 ) * YC(  NO3 )   ! HO2+NO3=0.8*HO

      R4_5  = R4_5 * DTC

c..Production of HO from HONO (r4,19 terms )
      R4_19 =            RKI(   22 ) * DTC         ! HONO+hv=HO+NO

c..Remaining HO production
      P4 =  OH_S  * RXRAT(  18 )                  ! O3+Hv=>O1D2=2*HO
     &    +         RXRAT(  28 )                  ! HNO3+hv=HO
     &    + 0.390D0 * RXRAT(  34 )                  ! HNO4+hv=0.39*HO
     &    + 2.000D0 * RXRAT(  41 )                  ! HO2H+hv=2*HO
     &    +         RXRAT( 142 )                  ! COOH=HO
     &    +         RXRAT( 144 )                  ! ROOH=HO
     &    + 0.208D0 * RXRAT( 162 )                  ! METHACRO+O3=0.208*HO
     &    + 0.330D0 * RXRAT( 165 )                  ! METHACRO+hv=0.330*HO
     &    + 0.164D0 * RXRAT( 167 )                  ! MVK+O3=0.164*HO
     &    + 0.285D0 * RXRAT( 171 )                  ! ISOPROD+O3=0.285*HO
     &    + 0.500D0 * RXRAT( 179 )                  ! DCB1+O3=0.500*HO
     &    + 0.120D0 * RXRAT( 186 )                  ! ETHENE+O3=0.120*HO
     &    + 0.266D0 * RXRAT( 190 )                  ! ISOPRENE+O3=0.266*HO
     &    + 0.567D0 * RXRAT( 194 )                  ! TRP1+O3=0.285*HO
     &    + 0.155D0 * RXRAT( 205 )                  ! OLE1+O3=0.155*HO
     &    + 0.378D0 * RXRAT( 209 )                  ! OLE2+O3=0.378*HO

      P4 = YC0( NCELL,   HO ) + P4 * DTC

   

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =            RKI(   26 ) * YC(   NO3 )  ! HO+NO3=HO2
     &        +         RKI(   29 ) * YC(    CO )  ! HO+CO=HO2
     &        +         RKI(   30 ) * YC(    O3 )  ! HO+O3=HO2
     &        +         RKI(   42 ) * YC(  HO2H )  ! HO+HO2H=HO2
     &        +         RKI(   44 ) * YC(   SO2 )  ! HO+SO2=HO2
     &        +         RKI(   45 )                ! HO+{H2}=HO2
     &        +         RKI(  125 ) * YC(  HCHO )  ! HO+HCHO=HO2
     &        +         RKI(  140 ) * YC(  MEOH )  ! HO+MEOH=HO2
     &        + 0.630D0 * RKI(  147 ) * YC(   GLY )  ! HO+GLY=0.63*HO2
     &        + 0.379D0 * RKI(  174 ) * YC( PROD2 )  ! HO+PROD2=0.379*HO2
     &        + 0.113D0 * RKI(  176 ) * YC(  RNO3 )  ! HO+RNO3=0.113*HO2
     &        + 0.121D0 * RKI(  198 ) * YC(  ALK2 )  ! HO+ALK2=0.121*HO2+0.246*HO
     &        + 0.224D0 * RKI(  202 ) * YC(  ARO1 )  ! HO+ARO1=0.224*HO2
     &        + 0.187D0 * RKI(  203 ) * YC(  ARO2 )  ! HO+ARO2=0.187*HO2
     &        +         RKI(  212 ) * YC( HCOOH )  ! HO+HCOOH=HO2

      R5_4  = R5_4 * DTC

c..Production of HO2 from HNO4 (r5,21 term )
      R5_21 =           RKI(   33 )                ! HNO4=HO2
     &        + 0.610D0 * RKI(   34 )                ! HNO4+hv=0.61*HO2

      R5_21 = R5_21 * DTC

c..Remaining HO2 production terms
      P5   =            RXRAT(  23 )              ! HONO+hv=HO2
     &       +          RXRAT(  46 )              ! C_O2+NO=HO2
     &       +          RXRAT(  48 )              ! C_O2+NO3=HO2
     &       + 2.000D0 *  RXRAT(  50 )              ! C_O2+C_O2=2*HO2
     &       +          RXRAT(  51 )              ! RO2_R+NO=HO2
     &       +          RXRAT(  53 )              ! RO2_R+NO3=HO2
     &       +          RXRAT(  54 )              ! RO2_R+C_O2=HO2
     &       +          RXRAT(  55 )              ! RO2_R+RO2_R=HO2
     &       +          RXRAT(  64 )              ! RO2_N+C_O2=HO2
     &       +          RXRAT(  65 )              ! RO2_N+NO3=HO2
     &       +          RXRAT(  66 )              ! RO2_N+RO2_R=HO2
     &       +          RXRAT(  68 )              ! RO2_N+RO2_N=HO2
     &       + 2.000D0 *  RXRAT( 123 )              ! HCHO+hv=2*HO2
     &       +          RXRAT( 127 )              ! HOCOO=HO2
     &       +          RXRAT( 128 )              ! HOCOO+NO=HO2
     &       +          RXRAT( 129 )              ! HCHO+NO3=HO2
     &       +          RXRAT( 131 )              ! CCHO=HO2
     &       +          RXRAT( 134 )              ! RCHO=HO2
     &       +          RXRAT( 142 )              ! COOH+hv=HO2
     &       +          RXRAT( 144 )              ! ROOH+hv=HO2
     &       + 2.000D0 *  RXRAT( 145 )              ! GLY+hv=2*HO2
     &       + 0.630D0 *  RXRAT( 148 )              ! GLY+NO3=0.63*HO2
     &       +          RXRAT( 149 )              ! MGLY+hv=HO2
     &       + 0.008D0 *  RXRAT( 162 )              ! METHACRO+O3=0.008*HO2
     &       + 0.340D0 *  RXRAT( 165 )              ! METHACRO+hv=0.340*HO2
     &       + 0.06D04 *  RXRAT( 167 )              ! MVK+O3=0.064*HO2
     &       + 0.400D0 *  RXRAT( 171 )              ! ISOPROD+O3=0.400*HO2
     &       + 1.233D0 *  RXRAT( 173 )              ! ISOPROD+hv=1.233*HO2
     &       + 0.341D0 *  RXRAT( 177 )              ! RNO3+hv=0.341*HO2
     &       + 1.500D0 *  RXRAT( 179 )              ! DCB1+O3=1.500*HO2
     &       + 0.500D0 *  RXRAT( 181 )              ! DCB2+hv=0.500*HO2
     &       + 0.500D0 *  RXRAT( 183 )              ! DCB3+hv=0.500*HO2
     &       + 0.120D0 *  RXRAT( 186 )              ! ETHENE+O3=0.120*HO2
     &       + 0.500D0 *  RXRAT( 188 )              ! ETHENE+O3P=0.500*HO2
     &       + 0.033D0 *  RXRAT( 194 )              ! TRP1+O3=0.033*HO2
     &       + 0.056D0 *  RXRAT( 205 )              ! OLE1+O3=0.056*HO2
     &       + 0.003D0 *  RXRAT( 209 )              ! OLE2+O3=0.003*HO2
     &       + 0.013D0 *  RXRAT( 211 )              ! OLE2+O3P=0.013*HO2

      P5 = YC0( NCELL,  HO2 ) + P5 * DTC


c..Production of HONO from OH (r19,4 terms )
      R19_4 =   RKI(   21 ) * YC(   NO ) * DTC     ! HO+NO=HONO


c..Remaining HONO production terms
      P19  =  YC0( NCELL, HONO ) 


c..Production of HNO4 from HO2 (r21,5 term )
      R21_5 =   RKI(   32 ) * YC(  NO2 ) * DTC     ! HO2+NO2=HNO4

c..Remaining HNO4 production terms
      P21  =  YC0( NCELL,  HNO4 ) 


c..OH Loss terms not in R5_4 & R19_4
      L4   =            RKI(   24 ) * YC(     HONO )  ! HO+HONO=NO2
     &        +         RKI(   25 ) * YC(      NO2 )  ! HO+NO2=HNO3
     &        +         RKI(   27 ) * YC(     HNO3 )  ! HO+HNO3=NO3
     &        +         RKI(   35 ) * YC(     HNO4 )  ! HO+HNO4=NO2
     &        +         RKI(   43 ) * YC(      HO2 )  ! HO+HO2=
     &        +         RKI(  130 ) * YC(     CCHO )  ! HO+CCHO=
     &        +         RKI(  133 ) * YC(     RCHO )  ! HO+RCHO=
     &        +         RKI(  136 ) * YC(     ACET )  ! HO+ACET=
     &        +         RKI(  138 ) * YC(      MEK )  ! HO+MEK=
     &        + 0.650D0 * RKI(  141 ) * YC(     COOH )  ! HO+COOH=0.35*HO
     &        + 0.340D0 * RKI(  143 ) * YC(     ROOH )  ! HO+ROOH=0.66*HO
     &        + 0.370D0 * RKI(  147 ) * YC(      GLY )  ! HO+GLY=0.63*HO2
     &        +         RKI(  150 ) * YC(     MGLY )  ! HO+MGLY=
     &        +         RKI(  153 ) * YC(     PHEN )  ! HO+PHEN=
     &        +         RKI(  155 ) * YC(     CRES )  ! HO+CRES=
     &        +         RKI(  158 ) * YC(     BALD )  ! HO+BALD=
     &        +         RKI(  161 ) * YC( METHACRO )  ! HO+METHACRO=
     &        +         RKI(  166 ) * YC(      MVK )  ! HO+MVK=
     &        +         RKI(  170 ) * YC(  ISOPROD )  ! HO+ISOPROD=
     &        + 0.621D0 * RKI(  174 ) * YC(    PROD2 )  ! HO+PROD2=0.379*HO2
     &        + 0.887D0 * RKI(  176 ) * YC(     RNO3 )  ! HO+RNO3=0.113*HO2
     &        +         RKI(  178 ) * YC(     DCB1 )  ! HO+DCB1=
     &        +         RKI(  180 ) * YC(     DCB2 )  ! HO+DCB2=
     &        +         RKI(  182 ) * YC(     DCB3 )  ! HO+DCB3=
     &        +         RKI(  184 )                   ! HO+{CH4}=
     &        +         RKI(  185 ) * YC(   ETHENE )  ! HO+ETHENE=
     &        +         RKI(  189 ) * YC( ISOPRENE )  ! HO+ISOPRENE=
     &        +         RKI(  193 ) * YC(     TRP1 )  ! HO+TRP1=
     &        +         RKI(  197 ) * YC(     ALK1 )  ! HO+ALK1=
     &        + 0.633D0 * RKI(  198 ) * YC(     ALK2 )  ! HO+ALK2=0.246HO+0.121HO2
     &        +         RKI(  199 ) * YC(     ALK3 )  ! HO+ALK3=
     &        +         RKI(  200 ) * YC(     ALK4 )  ! HO+ALK4=
     &        +         RKI(  201 ) * YC(     ALK5 )  ! HO+ALK5=
     &        + 0.776D0 * RKI(  202 ) * YC(     ARO1 )  ! HO+ARO1=0.224*HO2
     &        + 0.813D0 * RKI(  203 ) * YC(     ARO2 )  ! HO+ARO2=0.187*HO2
     &        +         RKI(  204 ) * YC(     OLE1 )  ! HO+OLE1=
     &        +         RKI(  208 ) * YC(     OLE2 )  ! HO+OLE2=
     &        +         RKI(  213 ) * YC(   CCO_OH )  ! HO+CCO_OH=
     &        +         RKI(  214 ) * YC(   RCO_OH )  ! HO+RCO_OH=

      L4 = 1.0D0 + L4 * DTC + R5_4 + R19_4



c..HO2 Loss terms not included in R4_5 & R21_5 (except for HO2+HO2 )
      L5   =    0.200D0 * RKI(   39 ) * YC(     NO3 )     ! HO2+NO3=0.8*HO    
     &        +         RKI(   43 ) * YC(      HO )     ! HO2+HO=
     &        +         RKI(   47 ) * YC(    C_O2 )     ! HO2+C_O2=
     &        +         RKI(   52 ) * YC(   RO2_R )     ! HO2+RO2_R=
     &        +         RKI(   63 ) * YC(   RO2_N )     ! HO2+RO2_N=
     &        +         RKI(   72 ) * YC(  CCO_O2 )     ! HO2+CCO_O2=
     &        +         RKI(   82 ) * YC(  RCO_O2 )     ! HO2+RCO_O2=
     &        +         RKI(   93 ) * YC( BZCO_O2 )     ! HO2+BZCO_O2=
     &        +         RKI(  105 ) * YC( MA_RCO3 )     ! HO2+MA_RCO3=
     &        +         RKI(  118 ) * YC(    BZ_O )     ! HO2+BZ_O=
     &        +         RKI(  121 ) * YC( BZNO2_O )     ! HO2+BZNO2_O=
     &        +         RKI(  126 ) * YC(    HCHO )     ! HO2+HCHO=

      L5 = 1.0D0 + L5 * DTC + R4_5 + R21_5


c..HONO loss terms not included in R4_19
      L19  =  +         RKI(   23 )                     ! HONO+hv=HO2
     &        +         RKI(   24 ) * YC(  HO )         ! HONO+HO=NO2

      L19 = 1.0D0 + L19 * DTC + R4_19


c..HNO4 loss terms not inluded in R5_21
      L21  =    0.390D0 * RKI(   34 )                     ! HNO4+hv=0.61HO2+0.39HO
     &        +         RKI(   35 ) * YC(  HO )         ! HNO4+HO=NO2

      L21 = 1.0D0 + L21 * DTC + R5_21

S1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c..compute terms used to calculate a,b & c
      L21_INV = 1.0D0 / L21
      T1 = 1.0D0 / ( L4 * L19 - R4_19 * R19_4 )
      T2 = R5_4 * T1
      T3 = R5_21 * L21_INV

R3c..solve quadratic equation for HO2
R4      A = 2.0D0 * ( RKI(   37 ) + RKI(   38 ) ) * DTC

      B = L5 - T3 * R21_5 - T2 * R4_5 * L19

      C = P5 + T3 * P21 + T2 * ( P4 * L19 + P19 * R4_19 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

R5      YCP( NCELL,  HO2 ) = MAX( Q / A , -C / Q  )

c..compute remaining species concentrations
R6      YCP( NCELL,   HO ) = ( ( P4 + R4_5 * YCP( NCELL,   HO2 ) ) * L19 + R4_19 * P19 ) * T1

R7      YCP( NCELL, HNO4 ) = ( P21 + R21_5 * YCP( NCELL,  HO2 ) ) * L21_INV

R8      YCP( NCELL, HONO ) = ( P19 + R19_4 * YCP( NCELL,   HO ) ) / L19

      RETURN

      END


