
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
C  PRECONDITIONS: For the CRACMM1_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Nov 17, 2021
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
      O1D_S =                 RKI(    41 )                      ! O1D=O3P
     &      +                 RKI(    42 )                      ! O1D=O3P
     &      +                 RKI(    43 )                      ! O1D=0.2000D+01*HO

      O3P_S =                 RKI(    41 )                      ! O1D=O3P
     &      +                 RKI(    42 )                      ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(     4 )                      ! NO2=NO+O3P
     &     +                 RKI(    55 ) * YC ( O3P       )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(     5 )      ! NO3=NO
     &   +                 RXRAT(     7 )      ! HONO=NO+HO
     &   +                 RXRAT(    62 )      ! NO2+NO3=NO+NO2
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(    50 ) * YC ( HO        )   ! NO+HO=HONO
     &   +                 RKI(    52 ) * YC ( HO2       )   ! NO+HO2=HNO3
     &   +    6.5000D-02 * RKI(   171 ) * YC ( HC3P      )   ! NO+HC3P=0.6600D+...
     &   +    1.3600D-01 * RKI(   172 ) * YC ( HC5P      )   ! NO+HC5P=0.2000D+...
     &   +    2.6100D-01 * RKI(   173 ) * YC ( HC8P      )   ! NO+HC8P=0.6060D+...
     &   +    3.0000D-02 * RKI(   175 ) * YC ( OLTP      )   ! NO+OLTP=0.7800D+...
     &   +    5.0000D-02 * RKI(   176 ) * YC ( OLIP      )   ! NO+OLIP=0.8300D+...
     &   +    8.2000D-02 * RKI(   177 ) * YC ( BENP      )   ! NO+BENP=0.9180D+...
     &   +    5.0000D-02 * RKI(   179 ) * YC ( TOLP      )   ! NO+TOLP=0.9500D+...
     &   +    5.0000D-02 * RKI(   180 ) * YC ( PER1      )   ! NO+PER1=0.5000D+...
     &   +    5.0000D-02 * RKI(   182 ) * YC ( XYLP      )   ! NO+XYLP=0.9500D+...
     &   +    5.0000D-02 * RKI(   183 ) * YC ( PER2      )   ! NO+PER2=0.9500D+...
     &   +    5.0000D-02 * RKI(   184 ) * YC ( XYOP      )   ! NO+XYOP=0.9500D+...
     &   +    1.2000D-01 * RKI(   185 ) * YC ( ISOP      )   ! NO+ISOP=0.8800D+...
     &   +    1.8000D-01 * RKI(   186 ) * YC ( APIP      )   ! NO+APIP=0.8200D+...
     &   +    8.0000D-02 * RKI(   381 ) * YC ( FURANO2   )   ! NO+FURANO2=...
     &   +    2.4700D-01 * RKI(   395 ) * YC ( SESQRO2   )   ! NO+SESQRO2=...
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
      R2_1 =                 RKI(    49 ) * YC ( O3P       )   ! NO+O3P=NO2
     &     +                 RKI(    51 ) * YC ( HO2       )   ! NO+HO2=NO2+HO
     &     +    2.0000D+00 * RKI(    53 ) * YC ( NO        )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(    61 ) * YC ( NO3       )   ! NO+NO3=0.2000D+...
     &     +                 RKI(   169 ) * YC ( MO2       )   ! NO+MO2=NO2+HO2+HCHO
     &     +                 RKI(   170 ) * YC ( ETHP      )   ! NO+ETHP=NO2+HO2+ACD
     &     +    9.3500D-01 * RKI(   171 ) * YC ( HC3P      )   ! NO+HC3P=0.9350D+...
     &     +    8.6400D-01 * RKI(   172 ) * YC ( HC5P      )   ! NO+HC5P=0.8640D+...
     &     +    7.3900D-01 * RKI(   173 ) * YC ( HC8P      )   ! NO+HC8P=0.7390D+...
     &     +                 RKI(   174 ) * YC ( ETEP      )   ! NO+ETEP=NO2+HO2+...
     &     +    9.7000D-01 * RKI(   175 ) * YC ( OLTP      )   ! NO+OLTP=0.9700D+...
     &     +    9.5000D-01 * RKI(   176 ) * YC ( OLIP      )   ! NO+OLIP=0.9500D+...
     &     +    9.1800D-01 * RKI(   177 ) * YC ( BENP      )   ! NO+BENP=0.9180D+...
     &     +                 RKI(   178 ) * YC ( TLP1      )   ! NO+TLP1=NO2+BALD
     &     +    9.5000D-01 * RKI(   179 ) * YC ( TOLP      )   ! NO+TOLP=0.9500D+...
     &     +    9.5000D-01 * RKI(   180 ) * YC ( PER1      )   ! NO+PER1=0.9500D+...
     &     +                 RKI(   181 ) * YC ( XYL1      )   ! NO+XYL1=NO2+BALD
     &     +    9.5000D-01 * RKI(   182 ) * YC ( XYLP      )   ! NO+XYLP=0.9500D+...
     &     +    9.5000D-01 * RKI(   183 ) * YC ( PER2      )   ! NO+PER2=0.9500D+...
     &     +    9.5000D-01 * RKI(   184 ) * YC ( XYOP      )   ! NO+XYOP=0.9500D+...
     &     +    8.8000D-01 * RKI(   185 ) * YC ( ISOP      )   ! NO+ISOP=0.8800D+...
     &     +    8.2000D-01 * RKI(   186 ) * YC ( APIP      )   ! NO+APIP=0.8200D+...
     &     +                 RKI(   187 ) * YC ( LIMP      )   ! NO+LIMP=NO2+...
     &     +                 RKI(   188 ) * YC ( ACO3      )   ! NO+ACO3=NO2+MO2
     &     +                 RKI(   189 ) * YC ( RCO3      )   ! NO+RCO3=NO2+ETHP
     &     +                 RKI(   190 ) * YC ( ACTP      )   ! NO+ACTP=NO2+ACO3+...
     &     +                 RKI(   191 ) * YC ( MEKP      )   ! NO+MEKP=NO2+...
     &     +                 RKI(   192 ) * YC ( KETP      )   ! NO+KETP=NO2+...
     &     +                 RKI(   193 ) * YC ( MACP      )   ! NO+MACP=NO2+...
     &     +                 RKI(   194 ) * YC ( MCP       )   ! NO+MCP=NO2+...
     &     +                 RKI(   195 ) * YC ( MVKP      )   ! NO+MVKP=NO2+...
     &     +                 RKI(   196 ) * YC ( UALP      )   ! NO+UALP=NO2+HO2+...
     &     +                 RKI(   197 ) * YC ( BALP      )   ! NO+BALP=NO2+BAL1
     &     +                 RKI(   198 ) * YC ( BAL1      )   ! NO+BAL1=NO2+BAL2
     &     +                 RKI(   199 ) * YC ( ADDC      )   ! NO+ADDC=NO2+HO2+...
     &     +                 RKI(   200 ) * YC ( MCTP      )   ! NO+MCTP=NO2+MCTO
     &     +                 RKI(   201 ) * YC ( ORAP      )   ! NO+ORAP=NO2+GLY+HO2
     &     +                 RKI(   202 ) * YC ( OLNN      )   ! NO+OLNN=NO2+HO2+ONIT
     &     +                 RKI(   203 ) * YC ( OLND      )   ! NO+OLND=0.2000D+...
     &     +                 RKI(   204 ) * YC ( ADCN      )   ! NO+ADCN=0.2000D+...
     &     +                 RKI(   205 ) * YC ( XO2       )   ! NO+XO2=NO2
     &     +    9.2000D-01 * RKI(   381 ) * YC ( FURANO2   )   ! NO+FURANO2=...
     &     +                 RKI(   389 ) * YC ( SESQNRO2  )   ! NO+SESQNRO2=...
     &     +    7.5300D-01 * RKI(   395 ) * YC ( SESQRO2   )   ! NO+SESQRO2=...
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(     6 )      ! NO3=NO2+O3P
     &   +                 RXRAT(     8 )      ! HNO3=NO2+HO
     &   +    8.0000D-01 * RXRAT(     9 )      ! HNO4=0.8000D+00*NO2+...
     &   +                 RXRAT(    32 )      ! ONIT=NO2+HO2+0.2000D+...
     &   +                 RXRAT(    33 )      ! PAN=NO2+ACO3
     &   +                 RXRAT(    54 )      ! HONO+HO=NO2
     &   +                 RXRAT(    59 )      ! NO3+HO=NO2+HO2
     &   +    7.0000D-01 * RXRAT(    60 )      ! NO3+HO2=0.7000D+...
     &   +                 RXRAT(    61 )      ! NO+NO3=0.2000D+01*NO2
     &   +    2.0000D+00 * RXRAT(    63 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    65 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    68 )      ! HNO4=NO2+HO2
     &   +                 RXRAT(    69 )      ! HNO4+HO=NO2
     &   +                 RXRAT(   121 )      ! MPAN+HO=NO2+HKET
     &   +                 RXRAT(   122 )      ! ONIT+HO=NO2+HC3P
     &   +                 RXRAT(   123 )      ! NALD+HO=NO2+XO2+HKET
     &   +    6.8000D-01 * RXRAT(   148 )      ! MACR+NO3=0.6800D+...
     &   +    5.0000D-01 * RXRAT(   154 )      ! EPX+NO3=0.5000D+...
     &   +                 RXRAT(   156 )      ! MPAN+NO3=NO2+MACP
     &   +                 RXRAT(   164 )      ! PAN=NO2+ACO3
     &   +                 RXRAT(   166 )      ! PPN=NO2+RCO3
     &   +                 RXRAT(   168 )      ! MPAN=NO2+MACP
     &   +                 RXRAT(   203 )      ! NO+OLND=0.2000D+...
     &   +                 RXRAT(   204 )      ! NO+ADCN=0.2000D+...
     &   +                 RXRAT(   270 )      ! MCP+MO2=NO2+HO2+...
     &   +    5.0000D-01 * RXRAT(   279 )      ! OLND+MO2=0.5000D+...
     &   +    7.0000D-01 * RXRAT(   280 )      ! ADCN+MO2=0.7000D+...
     &   +                 RXRAT(   306 )      ! MCP+ACO3=NO2+0.5000D+...
     &   +                 RXRAT(   315 )      ! OLND+ACO3=NO2+0.5000D+...
     &   +    7.0000D-01 * RXRAT(   316 )      ! ADCN+ACO3=0.7000D+...
     &   +                 RXRAT(   319 )      ! MO2+NO3=NO2+HCHO+HO2
     &   +                 RXRAT(   320 )      ! ETHP+NO3=NO2+HO2+ACD
     &   +                 RXRAT(   321 )      ! HC3P+NO3=NO2+0.1400D+...
     &   +                 RXRAT(   322 )      ! HC5P+NO3=NO2+0.5500D-...
     &   +                 RXRAT(   323 )      ! HC8P+NO3=NO2+0.1800D+...
     &   +                 RXRAT(   324 )      ! ETEP+NO3=NO2+HO2+...
     &   +                 RXRAT(   325 )      ! OLTP+NO3=NO2+0.7900D+...
     &   +                 RXRAT(   326 )      ! OLIP+NO3=NO2+0.7200D+...
     &   +                 RXRAT(   327 )      ! BENP+NO3=NO2+HO2+...
     &   +                 RXRAT(   328 )      ! TLP1+NO3=NO2+BALD
     &   +                 RXRAT(   329 )      ! TOLP+NO3=NO2+HO2+DCB2
     &   +                 RXRAT(   330 )      ! PER1+NO3=NO2+0.5000D+...
     &   +                 RXRAT(   331 )      ! XYL1+NO3=NO2+BALD
     &   +                 RXRAT(   332 )      ! XYLP+NO3=NO2+HO2+DCB3
     &   +                 RXRAT(   333 )      ! PER2+NO3=NO2+HO2+MGLY+...
     &   +                 RXRAT(   334 )      ! XYOP+NO3=NO2+HO2+...
     &   +                 RXRAT(   335 )      ! ISOP+NO3=NO2+HO2+...
     &   +                 RXRAT(   336 )      ! APIP+NO3=NO2+HO2+ALD+KET
     &   +                 RXRAT(   337 )      ! LIMP+NO3=NO2+HO2+...
     &   +                 RXRAT(   338 )      ! ACO3+NO3=NO2+MO2
     &   +                 RXRAT(   339 )      ! RCO3+NO3=NO2+ETHP
     &   +                 RXRAT(   340 )      ! ACTP+NO3=NO2+ACO3+HCHO
     &   +                 RXRAT(   341 )      ! MEKP+NO3=NO2+0.6700D+...
     &   +                 RXRAT(   342 )      ! KETP+NO3=NO2+HO2+DCB1
     &   +                 RXRAT(   343 )      ! MACP+NO3=NO2+0.5380D+...
     &   +                 RXRAT(   344 )      ! MCP+NO3=NO2+HO2+HCHO+HKET
     &   +                 RXRAT(   345 )      ! MVKP+NO3=NO2+0.7000D+...
     &   +                 RXRAT(   346 )      ! UALP+NO3=NO2+HO2+...
     &   +                 RXRAT(   347 )      ! BALP+NO3=NO2+BAL1
     &   +                 RXRAT(   348 )      ! BAL1+NO3=NO2+BAL2
     &   +                 RXRAT(   349 )      ! ADDC+NO3=NO2+HO2+...
     &   +                 RXRAT(   350 )      ! MCTP+NO3=NO2+MCTO
     &   +                 RXRAT(   351 )      ! ORAP+NO3=NO2+GLY+HO2
     &   +                 RXRAT(   352 )      ! OLNN+NO3=NO2+HO2+ONIT
     &   +    2.0000D+00 * RXRAT(   353 )      ! OLND+NO3=0.2000D+...
     &   +    2.0000D+00 * RXRAT(   354 )      ! ADCN+NO3=0.2000D+...
     &   +    5.0000D-01 * RXRAT(   356 )      ! OLNN+OLND=0.5000D+...
     &   +                 RXRAT(   357 )      ! OLND+OLND=NO2+0.5040D+...
     &   +                 RXRAT(   358 )      ! XO2+NO3=NO2
     &   +                 RXRAT(   385 )      ! FURAN+NO3=NO2+0.8000D+...
     &   +                 RXRAT(   389 )      ! NO+SESQNRO2=0.2000D+...
     &   +    2.0000D+00 * RXRAT(   390 )      ! SESQNRO2+NO3=0.2000D+...
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(    38 ) * YC ( O3        )   ! NO2+O3=NO3
     &   +                 RKI(    56 ) * YC ( O3P       )   ! NO2+O3P=NO3
     &   +                 RKI(    57 ) * YC ( HO        )   ! NO2+HO=HNO3
     &   +                 RKI(    64 ) * YC ( NO3       )   ! NO2+NO3=N2O5
     &   +                 RKI(    67 ) * YC ( HO2       )   ! NO2+HO2=HNO4
     &   +                 RKI(   163 ) * YC ( ACO3      )   ! NO2+ACO3=PAN
     &   +                 RKI(   165 ) * YC ( RCO3      )   ! NO2+RCO3=PPN
     &   +                 RKI(   167 ) * YC ( MACP      )   ! NO2+MACP=MPAN
     &   +                 RKI(   206 ) * YC ( BAL2      )   ! NO2+BAL2=ONIT
     &   +                 RKI(   207 ) * YC ( CHO       )   ! NO2+CHO=ONIT
     &   +                 RKI(   208 ) * YC ( MCTO      )   ! NO2+MCTO=ONIT
     &   +                 RKI(   399 )                      ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 = YC0( O3 ) 


      L3 =                 RKI(     1 )                      ! O3=O3P
     &   +                 RKI(     2 )                      ! O3=O1D
     &   +                 RKI(    35 ) * YC ( HO        )   ! O3+HO=HO2
     &   +                 RKI(    36 ) * YC ( HO2       )   ! O3+HO2=HO
     &   +                 RKI(    38 ) * YC ( NO2       )   ! O3+NO2=NO3
     &   +                 RKI(    40 ) * YC ( O3P       )   ! O3+O3P=
     &   +                 RKI(   125 ) * YC ( ETE       )   ! O3+ETE=0.8000D-...
     &   +                 RKI(   126 ) * YC ( OLT       )   ! O3+OLT=0.2200D+...
     &   +                 RKI(   127 ) * YC ( OLI       )   ! O3+OLI=0.4600D+...
     &   +                 RKI(   128 ) * YC ( ISO       )   ! O3+ISO=0.2500D+...
     &   +                 RKI(   129 ) * YC ( API       )   ! O3+API=0.8500D+...
     &   +                 RKI(   130 ) * YC ( LIM       )   ! O3+LIM=0.8500D+...
     &   +                 RKI(   131 ) * YC ( MACR      )   ! O3+MACR=0.1900D+...
     &   +                 RKI(   132 ) * YC ( MVK       )   ! O3+MVK=0.1600D+...
     &   +                 RKI(   133 ) * YC ( UALD      )   ! O3+UALD=0.1000D+...
     &   +                 RKI(   134 ) * YC ( DCB1      )   ! O3+DCB1=0.5000D-...
     &   +                 RKI(   135 ) * YC ( DCB2      )   ! O3+DCB2=0.5000D-...
     &   +                 RKI(   136 ) * YC ( DCB3      )   ! O3+DCB3=0.5000D-...
     &   +                 RKI(   137 ) * YC ( EPX       )   ! O3+EPX=0.5000D-...
     &   +                 RKI(   138 ) * YC ( MCTO      )   ! O3+MCTO=MCTP
     &   +                 RKI(   384 ) * YC ( FURAN     )   ! O3+FURAN=0.2000D-...
     &   +                 RKI(   391 ) * YC ( SESQ      )   ! O3+SESQ=0.9820D+...
     &   +                 RKI(   400 )                      ! O3=
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT(     1 )      ! O3=O3P
     &    +   O3P_S *       RXRAT(     2 )      ! O3=O1D
     &    +                 RXRAT(     6 )      ! NO3=O3P+NO2
      P12 = YC0( O3P ) + P12 * DTC


      L12 =                 RKI(    39 )                      ! O3P=O3
     &    +                 RKI(    40 ) * YC ( O3        )   ! O3P+O3=
     &    +                 RKI(    49 ) * YC ( NO        )   ! O3P+NO=NO2
     &    +                 RKI(    55 ) * YC ( NO2       )   ! O3P+NO2=NO
     &    +                 RKI(    56 ) * YC ( NO2       )   ! O3P+NO2=NO3
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
      RK1 = RKI(   4 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI(  39 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(  37 ) * DTC            ! k1_3  (NO+O3=NO2)

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

      YCP( O1D ) = ( YC0( O1D ) + RKI( 2 ) * YCP( O3 ) * DTC ) 
     &           / ( 1.0D0 + O1D_S * DTC )

      RETURN

      END


