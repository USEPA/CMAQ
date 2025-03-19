
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
C  PRECONDITIONS: For the RACM2_AE6_AQ mechanism
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
      O1D_S =                 RKI(    40 )                     ! O1D=O3P
     &      +                 RKI(    41 )                     ! O1D=O3P
     &      +                 RKI(    42 )                     ! O1D=0.2000D+01*HO

      O3P_S =                 RKI(    40 )                     ! O1D=O3P
     &      +                 RKI(    41 )                     ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(     4 )                     ! NO2=NO+O3P
     &     +                 RKI(    54 ) * YC ( O3P      )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(     5 )      ! NO3=NO
     &   +                 RXRAT(     7 )      ! HONO=NO+HO
     &   +                 RXRAT(    61 )      ! NO2+NO3=NO+NO2
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(    49 ) * YC ( HO       )   ! NO+HO=HONO
     &   +                 RKI(    51 ) * YC ( HO2      )   ! NO+HO2=HNO3
     &   +    6.5000D-02 * RKI(   174 ) * YC ( HC3P     )   ! NO+HC3P=0.6600D+...
     &   +    1.3600D-01 * RKI(   175 ) * YC ( HC5P     )   ! NO+HC5P=0.2000D+...
     &   +    2.6100D-01 * RKI(   176 ) * YC ( HC8P     )   ! NO+HC8P=0.6060D+...
     &   +    3.0000D-02 * RKI(   178 ) * YC ( OLTP     )   ! NO+OLTP=0.7800D+...
     &   +    5.0000D-02 * RKI(   179 ) * YC ( OLIP     )   ! NO+OLIP=0.8300D+...
     &   +    8.2000D-02 * RKI(   180 ) * YC ( BENP     )   ! NO+BENP=0.9180D+...
     &   +    5.0000D-02 * RKI(   182 ) * YC ( TOLP     )   ! NO+TOLP=0.9500D+...
     &   +    5.0000D-02 * RKI(   183 ) * YC ( PER1     )   ! NO+PER1=0.5000D+...
     &   +    5.0000D-02 * RKI(   185 ) * YC ( XYLP     )   ! NO+XYLP=0.9500D+...
     &   +    5.0000D-02 * RKI(   186 ) * YC ( PER2     )   ! NO+PER2=0.9500D+...
     &   +    5.0000D-02 * RKI(   187 ) * YC ( XYOP     )   ! NO+XYOP=0.9500D+...
     &   +    1.2000D-01 * RKI(   188 ) * YC ( ISOP     )   ! NO+ISOP=0.8800D+...
     &   +    1.8000D-01 * RKI(   189 ) * YC ( APIP     )   ! NO+APIP=0.8200D+...
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
      R2_1 =                 RKI(    48 ) * YC ( O3P      )   ! NO+O3P=NO2
     &     +                 RKI(    50 ) * YC ( HO2      )   ! NO+HO2=NO2+HO
     &     +    2.0000D+00 * RKI(    52 ) * YC ( NO       )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(    60 ) * YC ( NO3      )   ! NO+NO3=0.2000D+...
     &     +                 RKI(   172 ) * YC ( MO2      )   ! NO+MO2=NO2+HO2+HCHO
     &     +                 RKI(   173 ) * YC ( ETHP     )   ! NO+ETHP=NO2+HO2+ACD
     &     +    9.3500D-01 * RKI(   174 ) * YC ( HC3P     )   ! NO+HC3P=0.9350D+...
     &     +    8.6400D-01 * RKI(   175 ) * YC ( HC5P     )   ! NO+HC5P=0.8640D+...
     &     +    7.3900D-01 * RKI(   176 ) * YC ( HC8P     )   ! NO+HC8P=0.7390D+...
     &     +                 RKI(   177 ) * YC ( ETEP     )   ! NO+ETEP=NO2+HO2+...
     &     +    9.7000D-01 * RKI(   178 ) * YC ( OLTP     )   ! NO+OLTP=0.9700D+...
     &     +    9.5000D-01 * RKI(   179 ) * YC ( OLIP     )   ! NO+OLIP=0.9500D+...
     &     +    9.1800D-01 * RKI(   180 ) * YC ( BENP     )   ! NO+BENP=0.9180D+...
     &     +                 RKI(   181 ) * YC ( TLP1     )   ! NO+TLP1=NO2+BALD
     &     +    9.5000D-01 * RKI(   182 ) * YC ( TOLP     )   ! NO+TOLP=0.9500D+...
     &     +    9.5000D-01 * RKI(   183 ) * YC ( PER1     )   ! NO+PER1=0.9500D+...
     &     +                 RKI(   184 ) * YC ( XYL1     )   ! NO+XYL1=NO2+BALD
     &     +    9.5000D-01 * RKI(   185 ) * YC ( XYLP     )   ! NO+XYLP=0.9500D+...
     &     +    9.5000D-01 * RKI(   186 ) * YC ( PER2     )   ! NO+PER2=0.9500D+...
     &     +    9.5000D-01 * RKI(   187 ) * YC ( XYOP     )   ! NO+XYOP=0.9500D+...
     &     +    8.8000D-01 * RKI(   188 ) * YC ( ISOP     )   ! NO+ISOP=0.8800D+...
     &     +    8.2000D-01 * RKI(   189 ) * YC ( APIP     )   ! NO+APIP=0.8200D+...
     &     +                 RKI(   190 ) * YC ( LIMP     )   ! NO+LIMP=NO2+...
     &     +                 RKI(   191 ) * YC ( ACO3     )   ! NO+ACO3=NO2+MO2
     &     +                 RKI(   192 ) * YC ( RCO3     )   ! NO+RCO3=NO2+ETHP
     &     +                 RKI(   193 ) * YC ( ACTP     )   ! NO+ACTP=NO2+ACO3+...
     &     +                 RKI(   194 ) * YC ( MEKP     )   ! NO+MEKP=NO2+...
     &     +                 RKI(   195 ) * YC ( KETP     )   ! NO+KETP=NO2+...
     &     +                 RKI(   196 ) * YC ( MACP     )   ! NO+MACP=NO2+...
     &     +                 RKI(   197 ) * YC ( MCP      )   ! NO+MCP=NO2+...
     &     +                 RKI(   198 ) * YC ( MVKP     )   ! NO+MVKP=NO2+...
     &     +                 RKI(   199 ) * YC ( UALP     )   ! NO+UALP=NO2+HO2+...
     &     +                 RKI(   200 ) * YC ( BALP     )   ! NO+BALP=NO2+BAL1
     &     +                 RKI(   201 ) * YC ( BAL1     )   ! NO+BAL1=NO2+BAL2
     &     +                 RKI(   202 ) * YC ( ADDC     )   ! NO+ADDC=NO2+HO2+...
     &     +                 RKI(   203 ) * YC ( MCTP     )   ! NO+MCTP=NO2+MCTO
     &     +                 RKI(   204 ) * YC ( ORAP     )   ! NO+ORAP=NO2+GLY+HO2
     &     +                 RKI(   205 ) * YC ( OLNN     )   ! NO+OLNN=NO2+HO2+ONIT
     &     +                 RKI(   206 ) * YC ( OLND     )   ! NO+OLND=0.2000D+...
     &     +                 RKI(   207 ) * YC ( ADCN     )   ! NO+ADCN=0.2000D+...
     &     +                 RKI(   208 ) * YC ( XO2      )   ! NO+XO2=NO2
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(     6 )      ! NO3=NO2+O3P
     &   +                 RXRAT(     8 )      ! HNO3=NO2+HO
     &   +    8.0000D-01 * RXRAT(     9 )      ! HNO4=0.8000D+00*NO2+...
     &   +                 RXRAT(    31 )      ! ONIT=NO2+HO2+0.2000D+...
     &   +                 RXRAT(    32 )      ! PAN=NO2+ACO3
     &   +                 RXRAT(    53 )      ! HONO+HO=NO2
     &   +                 RXRAT(    58 )      ! NO3+HO=NO2+HO2
     &   +    7.0000D-01 * RXRAT(    59 )      ! NO3+HO2=0.7000D+...
     &   +                 RXRAT(    60 )      ! NO+NO3=0.2000D+01*NO2
     &   +    2.0000D+00 * RXRAT(    62 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    64 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    67 )      ! HNO4=NO2+HO2
     &   +                 RXRAT(    68 )      ! HNO4+HO=NO2
     &   +                 RXRAT(   122 )      ! MPAN+HO=NO2+HKET
     &   +                 RXRAT(   123 )      ! ONIT+HO=NO2+HC3P
     &   +                 RXRAT(   124 )      ! NALD+HO=NO2+XO2+HKET
     &   +    6.8000D-01 * RXRAT(   151 )      ! MACR+NO3=0.6800D+...
     &   +    5.0000D-01 * RXRAT(   157 )      ! EPX+NO3=0.5000D+...
     &   +                 RXRAT(   159 )      ! MPAN+NO3=NO2+MACP
     &   +                 RXRAT(   167 )      ! PAN=NO2+ACO3
     &   +                 RXRAT(   169 )      ! PPN=NO2+RCO3
     &   +                 RXRAT(   171 )      ! MPAN=NO2+MACP
     &   +                 RXRAT(   206 )      ! NO+OLND=0.2000D+...
     &   +                 RXRAT(   207 )      ! NO+ADCN=0.2000D+...
     &   +                 RXRAT(   273 )      ! MCP+MO2=NO2+HO2+...
     &   +    5.0000D-01 * RXRAT(   282 )      ! OLND+MO2=0.5000D+...
     &   +    7.0000D-01 * RXRAT(   283 )      ! ADCN+MO2=0.7000D+...
     &   +                 RXRAT(   309 )      ! MCP+ACO3=NO2+0.5000D+...
     &   +                 RXRAT(   318 )      ! OLND+ACO3=NO2+0.5000D+...
     &   +    7.0000D-01 * RXRAT(   319 )      ! ADCN+ACO3=0.7000D+...
     &   +                 RXRAT(   322 )      ! MO2+NO3=NO2+HCHO+HO2
     &   +                 RXRAT(   323 )      ! ETHP+NO3=NO2+HO2+ACD
     &   +                 RXRAT(   324 )      ! HC3P+NO3=NO2+0.1400D+...
     &   +                 RXRAT(   325 )      ! HC5P+NO3=NO2+0.5500D-...
     &   +                 RXRAT(   326 )      ! HC8P+NO3=NO2+0.1800D+...
     &   +                 RXRAT(   327 )      ! ETEP+NO3=NO2+HO2+...
     &   +                 RXRAT(   328 )      ! OLTP+NO3=NO2+0.7900D+...
     &   +                 RXRAT(   329 )      ! OLIP+NO3=NO2+0.7200D+...
     &   +                 RXRAT(   330 )      ! BENP+NO3=NO2+HO2+...
     &   +                 RXRAT(   331 )      ! TLP1+NO3=NO2+BALD
     &   +                 RXRAT(   332 )      ! TOLP+NO3=NO2+HO2+DCB2
     &   +                 RXRAT(   333 )      ! PER1+NO3=NO2+0.5000D+...
     &   +                 RXRAT(   334 )      ! XYL1+NO3=NO2+BALD
     &   +                 RXRAT(   335 )      ! XYLP+NO3=NO2+HO2+DCB3
     &   +                 RXRAT(   336 )      ! PER2+NO3=NO2+HO2+MGLY+...
     &   +                 RXRAT(   337 )      ! XYOP+NO3=NO2+HO2+...
     &   +                 RXRAT(   338 )      ! ISOP+NO3=NO2+HO2+...
     &   +                 RXRAT(   339 )      ! APIP+NO3=NO2+HO2+ALD+KET
     &   +                 RXRAT(   340 )      ! LIMP+NO3=NO2+HO2+...
     &   +                 RXRAT(   341 )      ! ACO3+NO3=NO2+MO2
     &   +                 RXRAT(   342 )      ! RCO3+NO3=NO2+ETHP
     &   +                 RXRAT(   343 )      ! ACTP+NO3=NO2+ACO3+HCHO
     &   +                 RXRAT(   344 )      ! MEKP+NO3=NO2+0.6700D+...
     &   +                 RXRAT(   345 )      ! KETP+NO3=NO2+HO2+DCB1
     &   +                 RXRAT(   346 )      ! MACP+NO3=NO2+0.5380D+...
     &   +                 RXRAT(   347 )      ! MCP+NO3=NO2+HO2+HCHO+HKET
     &   +                 RXRAT(   348 )      ! MVKP+NO3=NO2+0.7000D+...
     &   +                 RXRAT(   349 )      ! UALP+NO3=NO2+HO2+...
     &   +                 RXRAT(   350 )      ! BALP+NO3=NO2+BAL1
     &   +                 RXRAT(   351 )      ! BAL1+NO3=NO2+BAL2
     &   +                 RXRAT(   352 )      ! ADDC+NO3=NO2+HO2+...
     &   +                 RXRAT(   353 )      ! MCTP+NO3=NO2+MCTO
     &   +                 RXRAT(   354 )      ! ORAP+NO3=NO2+GLY+HO2
     &   +                 RXRAT(   355 )      ! OLNN+NO3=NO2+HO2+ONIT
     &   +    2.0000D+00 * RXRAT(   356 )      ! OLND+NO3=0.2000D+...
     &   +    2.0000D+00 * RXRAT(   357 )      ! ADCN+NO3=0.2000D+...
     &   +    5.0000D-01 * RXRAT(   359 )      ! OLNN+OLND=0.5000D+...
     &   +                 RXRAT(   360 )      ! OLND+OLND=NO2+0.5040D+...
     &   +                 RXRAT(   361 )      ! XO2+NO3=NO2
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(    37 ) * YC ( O3       )   ! NO2+O3=NO3
     &   +                 RKI(    55 ) * YC ( O3P      )   ! NO2+O3P=NO3
     &   +                 RKI(    56 ) * YC ( HO       )   ! NO2+HO=HNO3
     &   +                 RKI(    63 ) * YC ( NO3      )   ! NO2+NO3=N2O5
     &   +                 RKI(    66 ) * YC ( HO2      )   ! NO2+HO2=HNO4
     &   +                 RKI(   166 ) * YC ( ACO3     )   ! NO2+ACO3=PAN
     &   +                 RKI(   168 ) * YC ( RCO3     )   ! NO2+RCO3=PPN
     &   +                 RKI(   170 ) * YC ( MACP     )   ! NO2+MACP=MPAN
     &   +                 RKI(   209 ) * YC ( BAL2     )   ! NO2+BAL2=ONIT
     &   +                 RKI(   210 ) * YC ( CHO      )   ! NO2+CHO=ONIT
     &   +                 RKI(   211 ) * YC ( MCTO     )   ! NO2+MCTO=ONIT
     &   +                 RKI(   379 )                     ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 = YC0( O3 ) 


      L3 =                 RKI(     1 )                     ! O3=O3P
     &   +                 RKI(     2 )                     ! O3=O1D
     &   +                 RKI(    34 ) * YC ( HO       )   ! O3+HO=HO2
     &   +                 RKI(    35 ) * YC ( HO2      )   ! O3+HO2=HO
     &   +                 RKI(    37 ) * YC ( NO2      )   ! O3+NO2=NO3
     &   +                 RKI(    39 ) * YC ( O3P      )   ! O3+O3P=
     &   +                 RKI(   126 ) * YC ( ETE      )   ! O3+ETE=0.8000D-...
     &   +                 RKI(   127 ) * YC ( OLT      )   ! O3+OLT=0.2200D+...
     &   +                 RKI(   128 ) * YC ( OLI      )   ! O3+OLI=0.4600D+...
     &   +                 RKI(   129 ) * YC ( DIEN     )   ! O3+DIEN=0.9000D-...
     &   +                 RKI(   130 ) * YC ( ISO      )   ! O3+ISO=0.2500D+...
     &   +                 RKI(   131 ) * YC ( API      )   ! O3+API=0.8500D+...
     &   +                 RKI(   132 ) * YC ( LIM      )   ! O3+LIM=0.8500D+...
     &   +                 RKI(   133 ) * YC ( MACR     )   ! O3+MACR=0.1900D+...
     &   +                 RKI(   134 ) * YC ( MVK      )   ! O3+MVK=0.1600D+...
     &   +                 RKI(   135 ) * YC ( UALD     )   ! O3+UALD=0.1000D+...
     &   +                 RKI(   136 ) * YC ( DCB1     )   ! O3+DCB1=0.5000D-...
     &   +                 RKI(   137 ) * YC ( DCB2     )   ! O3+DCB2=0.5000D-...
     &   +                 RKI(   138 ) * YC ( DCB3     )   ! O3+DCB3=0.5000D-...
     &   +                 RKI(   139 ) * YC ( EPX      )   ! O3+EPX=0.5000D-...
     &   +                 RKI(   140 ) * YC ( MCTO     )   ! O3+MCTO=MCTP
     &   +                 RKI(   380 )                     ! O3=
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT(     1 )      ! O3=O3P
     &    +   O3P_S *       RXRAT(     2 )      ! O3=O1D
     &    +                 RXRAT(     6 )      ! NO3=O3P+NO2
     &    +    9.0000D-02 * RXRAT(   129 )      ! DIEN+O3=0.9000D-...
      P12 = YC0( O3P ) + P12 * DTC


      L12 =                 RKI(    38 )                     ! O3P=O3
     &    +                 RKI(    39 ) * YC ( O3       )   ! O3P+O3=
     &    +                 RKI(    48 ) * YC ( NO       )   ! O3P+NO=NO2
     &    +                 RKI(    54 ) * YC ( NO2      )   ! O3P+NO2=NO
     &    +                 RKI(    55 ) * YC ( NO2      )   ! O3P+NO2=NO3
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
      RK2 = RKI(  38 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(  36 ) * DTC            ! k1_3  (NO+O3=NO2)

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


