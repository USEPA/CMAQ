
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
C  PRECONDITIONS: For the CRACMM2 mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Mar 15, 2024
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
      O1D_S =                 RKI(    47 )                        ! O1D=O3P
     &      +                 RKI(    48 )                        ! O1D=O3P
     &      +                 RKI(    49 )                        ! O1D=0.2000D+01*HO

      O3P_S =                 RKI(    47 )                        ! O1D=O3P
     &      +                 RKI(    48 )                        ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(     4 )                        ! NO2=NO+O3P
     &     +                 RKI(    61 ) * YC ( O3P         )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(     5 )      ! NO3=NO
     &   +                 RXRAT(     7 )      ! HONO=NO+HO
     &   +                 RXRAT(    68 )      ! NO2+NO3=NO+NO2
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(    56 ) * YC ( HO          )   ! NO+HO=HONO
     &   +                 RKI(    58 ) * YC ( HO2         )   ! NO+HO2=HNO3
     &   +    1.3000D-01 * RKI(    94 ) * YC ( ISOP        )   ! NO+ISOP=0.1300D+...
     &   +    6.5000D-02 * RKI(   185 ) * YC ( HC3P        )   ! NO+HC3P=0.6600D+...
     &   +    1.3600D-01 * RKI(   186 ) * YC ( HC5P        )   ! NO+HC5P=0.2000D+...
     &   +    3.0000D-02 * RKI(   188 ) * YC ( OLTP        )   ! NO+OLTP=0.7800D+...
     &   +    5.0000D-02 * RKI(   189 ) * YC ( OLIP        )   ! NO+OLIP=0.8300D+...
     &   +    2.0000D-03 * RKI(   190 ) * YC ( BENP        )   ! NO+BENP=0.0000D+...
     &   +    2.0000D-03 * RKI(   191 ) * YC ( TOLP        )   ! NO+TOLP=0.2000D-...
     &   +    2.0000D-03 * RKI(   192 ) * YC ( XYLP        )   ! NO+XYLP=0.1000D-...
     &   +    2.0000D-03 * RKI(   193 ) * YC ( EBZP        )   ! NO+EBZP=0.2000D-...
     &   +    2.3700D-01 * RKI(   194 ) * YC ( APIP1       )   ! NO+APIP1=0.2370D+...
     &   +    1.8000D-01 * RKI(   195 ) * YC ( APIP2       )   ! NO+APIP2=0.8200D+...
     &   +    1.8000D-01 * RKI(   197 ) * YC ( APINP2      )   ! NO+APINP2=...
     &   +    2.3000D-01 * RKI(   198 ) * YC ( LIMP1       )   ! NO+LIMP1=0.2300D+...
     &   +    2.3000D-01 * RKI(   199 ) * YC ( LIMP2       )   ! NO+LIMP2=0.7700D+...
     &   +    2.3000D-01 * RKI(   201 ) * YC ( LIMNP2      )   ! NO+LIMNP2=...
     &   +    3.6000D-01 * RKI(   202 ) * YC ( PINALP      )   ! NO+PINALP=...
     &   +    6.4000D-01 * RKI(   203 ) * YC ( LIMALP      )   ! NO+LIMALP=...
     &   +    3.2000D-02 * RKI(   398 ) * YC ( BDE13P      )   ! NO+BDE13P=...
     &   +    8.0000D-02 * RKI(   406 ) * YC ( FURANO2     )   ! NO+FURANO2=...
     &   +    2.4700D-01 * RKI(   420 ) * YC ( SESQRO2     )   ! NO+SESQRO2=...
     &   +    2.8000D-01 * RKI(   440 ) * YC ( VROCP6ALKP  )   ! NO+VROCP6ALKP=...
     &   +    2.8000D-01 * RKI(   441 ) * YC ( VROCP5ALKP  )   ! NO+VROCP5ALKP=...
     &   +    2.8000D-01 * RKI(   442 ) * YC ( VROCP4ALKP  )   ! NO+VROCP4ALKP=...
     &   +    2.8000D-01 * RKI(   443 ) * YC ( VROCP3ALKP  )   ! NO+VROCP3ALKP=...
     &   +    2.8000D-01 * RKI(   444 ) * YC ( VROCP2ALKP  )   ! NO+VROCP2ALKP=...
     &   +    2.8000D-01 * RKI(   445 ) * YC ( VROCP1ALKP  )   ! NO+VROCP1ALKP=...
     &   +    2.6000D-01 * RKI(   446 ) * YC ( HC10P       )   ! NO+HC10P=0.7400D+...
     &   +    1.4000D-01 * RKI(   468 ) * YC ( VROCP6ALKP2 )   ! NO+VROCP6ALKP2=...
     &   +    1.4000D-01 * RKI(   469 ) * YC ( VROCP5ALKP2 )   ! NO+VROCP5ALKP2=...
     &   +    1.4000D-01 * RKI(   470 ) * YC ( VROCP4ALKP2 )   ! NO+VROCP4ALKP2=...
     &   +    1.4000D-01 * RKI(   471 ) * YC ( VROCP3ALKP2 )   ! NO+VROCP3ALKP2=...
     &   +    1.4000D-01 * RKI(   472 ) * YC ( VROCP2ALKP2 )   ! NO+VROCP2ALKP2=...
     &   +    1.4000D-01 * RKI(   473 ) * YC ( VROCP1ALKP2 )   ! NO+VROCP1ALKP2=...
     &   +    1.2000D-01 * RKI(   474 ) * YC ( HC10P2      )   ! NO+HC10P2=...
     &   +    2.0000D-03 * RKI(   491 ) * YC ( VROCP6AROP  )   ! NO+VROCP6AROP=...
     &   +    2.0000D-03 * RKI(   497 ) * YC ( VROCP5AROP  )   ! NO+VROCP5AROP=...
     &   +    2.0000D-03 * RKI(   503 ) * YC ( NAPHP       )   ! NO+NAPHP=0.5950D-...
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
      R2_1 =                 RKI(    55 ) * YC ( O3P         )   ! NO+O3P=NO2
     &     +                 RKI(    57 ) * YC ( HO2         )   ! NO+HO2=NO2+HO
     &     +    2.0000D+00 * RKI(    59 ) * YC ( NO          )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(    67 ) * YC ( NO3         )   ! NO+NO3=0.2000D+...
     &     +    8.7000D-01 * RKI(    94 ) * YC ( ISOP        )   ! NO+ISOP=0.8700D+...
     &     +                 RKI(    98 ) * YC ( INO2        )   ! NO+INO2=0.1300D+...
     &     +                 RKI(   183 ) * YC ( MO2         )   ! NO+MO2=NO2+HO2+HCHO
     &     +                 RKI(   184 ) * YC ( ETHP        )   ! NO+ETHP=NO2+HO2+ACD
     &     +    9.3500D-01 * RKI(   185 ) * YC ( HC3P        )   ! NO+HC3P=0.9350D+...
     &     +    8.6400D-01 * RKI(   186 ) * YC ( HC5P        )   ! NO+HC5P=0.8640D+...
     &     +                 RKI(   187 ) * YC ( ETEP        )   ! NO+ETEP=NO2+HO2+...
     &     +    9.7000D-01 * RKI(   188 ) * YC ( OLTP        )   ! NO+OLTP=0.9700D+...
     &     +    9.5000D-01 * RKI(   189 ) * YC ( OLIP        )   ! NO+OLIP=0.9500D+...
     &     +    9.9800D-01 * RKI(   190 ) * YC ( BENP        )   ! NO+BENP=0.9980D+...
     &     +    9.9800D-01 * RKI(   191 ) * YC ( TOLP        )   ! NO+TOLP=0.9980D+...
     &     +    9.9800D-01 * RKI(   192 ) * YC ( XYLP        )   ! NO+XYLP=0.9980D+...
     &     +    9.9800D-01 * RKI(   193 ) * YC ( EBZP        )   ! NO+EBZP=0.9980D+...
     &     +    7.6300D-01 * RKI(   194 ) * YC ( APIP1       )   ! NO+APIP1=0.7630D+...
     &     +    8.2000D-01 * RKI(   195 ) * YC ( APIP2       )   ! NO+APIP2=0.8200D+...
     &     +                 RKI(   196 ) * YC ( APINP1      )   ! NO+APINP1=...
     &     +    8.2000D-01 * RKI(   197 ) * YC ( APINP2      )   ! NO+APINP2=...
     &     +    7.7000D-01 * RKI(   198 ) * YC ( LIMP1       )   ! NO+LIMP1=0.7700D+...
     &     +    7.7000D-01 * RKI(   199 ) * YC ( LIMP2       )   ! NO+LIMP2=0.7700D+...
     &     +                 RKI(   200 ) * YC ( LIMNP1      )   ! NO+LIMNP1=...
     &     +    7.7000D-01 * RKI(   201 ) * YC ( LIMNP2      )   ! NO+LIMNP2=...
     &     +    6.4000D-01 * RKI(   202 ) * YC ( PINALP      )   ! NO+PINALP=...
     &     +    3.6000D-01 * RKI(   203 ) * YC ( LIMALP      )   ! NO+LIMALP=...
     &     +                 RKI(   204 ) * YC ( ACO3        )   ! NO+ACO3=NO2+MO2
     &     +                 RKI(   205 ) * YC ( RCO3        )   ! NO+RCO3=NO2+ETHP
     &     +                 RKI(   206 ) * YC ( ACTP        )   ! NO+ACTP=NO2+ACO3+...
     &     +                 RKI(   207 ) * YC ( MEKP        )   ! NO+MEKP=NO2+...
     &     +                 RKI(   208 ) * YC ( KETP        )   ! NO+KETP=NO2+...
     &     +                 RKI(   209 ) * YC ( MACP        )   ! NO+MACP=NO2+...
     &     +                 RKI(   210 ) * YC ( MCP         )   ! NO+MCP=NO2+...
     &     +                 RKI(   211 ) * YC ( MVKP        )   ! NO+MVKP=NO2+...
     &     +                 RKI(   212 ) * YC ( UALP        )   ! NO+UALP=NO2+HO2+...
     &     +                 RKI(   213 ) * YC ( BALP        )   ! NO+BALP=NO2+BAL1
     &     +                 RKI(   214 ) * YC ( BAL1        )   ! NO+BAL1=NO2+BAL2
     &     +                 RKI(   215 ) * YC ( ADDC        )   ! NO+ADDC=NO2+HO2+...
     &     +                 RKI(   216 ) * YC ( MCTP        )   ! NO+MCTP=NO2+MCTO
     &     +                 RKI(   217 ) * YC ( ORAP        )   ! NO+ORAP=NO2+GLY+HO2
     &     +                 RKI(   218 ) * YC ( OLNN        )   ! NO+OLNN=NO2+HO2+ONIT
     &     +                 RKI(   219 ) * YC ( OLND        )   ! NO+OLND=0.2000D+...
     &     +                 RKI(   220 ) * YC ( ADCN        )   ! NO+ADCN=0.2000D+...
     &     +                 RKI(   221 ) * YC ( XO2         )   ! NO+XO2=NO2
     &     +    9.6800D-01 * RKI(   398 ) * YC ( BDE13P      )   ! NO+BDE13P=...
     &     +    9.2000D-01 * RKI(   406 ) * YC ( FURANO2     )   ! NO+FURANO2=...
     &     +                 RKI(   414 ) * YC ( SESQNRO2    )   ! NO+SESQNRO2=...
     &     +    7.5300D-01 * RKI(   420 ) * YC ( SESQRO2     )   ! NO+SESQRO2=...
     &     +    7.2000D-01 * RKI(   440 ) * YC ( VROCP6ALKP  )   ! NO+VROCP6ALKP=...
     &     +    7.2000D-01 * RKI(   441 ) * YC ( VROCP5ALKP  )   ! NO+VROCP5ALKP=...
     &     +    7.2000D-01 * RKI(   442 ) * YC ( VROCP4ALKP  )   ! NO+VROCP4ALKP=...
     &     +    7.2000D-01 * RKI(   443 ) * YC ( VROCP3ALKP  )   ! NO+VROCP3ALKP=...
     &     +    7.2000D-01 * RKI(   444 ) * YC ( VROCP2ALKP  )   ! NO+VROCP2ALKP=...
     &     +    7.2000D-01 * RKI(   445 ) * YC ( VROCP1ALKP  )   ! NO+VROCP1ALKP=...
     &     +    7.4000D-01 * RKI(   446 ) * YC ( HC10P       )   ! NO+HC10P=0.7400D+...
     &     +    8.6000D-01 * RKI(   468 ) * YC ( VROCP6ALKP2 )   ! NO+VROCP6ALKP2=...
     &     +    8.6000D-01 * RKI(   469 ) * YC ( VROCP5ALKP2 )   ! NO+VROCP5ALKP2=...
     &     +    8.6000D-01 * RKI(   470 ) * YC ( VROCP4ALKP2 )   ! NO+VROCP4ALKP2=...
     &     +    8.6000D-01 * RKI(   471 ) * YC ( VROCP3ALKP2 )   ! NO+VROCP3ALKP2=...
     &     +    8.6000D-01 * RKI(   472 ) * YC ( VROCP2ALKP2 )   ! NO+VROCP2ALKP2=...
     &     +    8.6000D-01 * RKI(   473 ) * YC ( VROCP1ALKP2 )   ! NO+VROCP1ALKP2=...
     &     +    8.8000D-01 * RKI(   474 ) * YC ( HC10P2      )   ! NO+HC10P2=...
     &     +    9.9800D-01 * RKI(   491 ) * YC ( VROCP6AROP  )   ! NO+VROCP6AROP=...
     &     +    9.9800D-01 * RKI(   497 ) * YC ( VROCP5AROP  )   ! NO+VROCP5AROP=...
     &     +    9.9800D-01 * RKI(   503 ) * YC ( NAPHP       )   ! NO+NAPHP=0.9980D+...
     &     +                 RKI(   528 ) * YC ( STYP        )   ! NO+STYP=NO2+HO2+...
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(     6 )      ! NO3=NO2+O3P
     &   +                 RXRAT(     8 )      ! HNO3=NO2+HO
     &   +    8.0000D-01 * RXRAT(     9 )      ! HNO4=0.8000D+00*NO2+...
     &   +                 RXRAT(    36 )      ! ONIT=NO2+HO2+0.2000D+...
     &   +                 RXRAT(    37 )      ! PAN=NO2+ACO3
     &   +                 RXRAT(    39 )      ! TRPN=NO2+0.6700D+...
     &   +                 RXRAT(    40 )      ! HONIT=NO2+HKET
     &   +                 RXRAT(    60 )      ! HONO+HO=NO2
     &   +                 RXRAT(    65 )      ! NO3+HO=NO2+HO2
     &   +    7.0000D-01 * RXRAT(    66 )      ! NO3+HO2=0.7000D+...
     &   +                 RXRAT(    67 )      ! NO+NO3=0.2000D+01*NO2
     &   +    2.0000D+00 * RXRAT(    69 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(    71 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(    74 )      ! HNO4=NO2+HO2
     &   +                 RXRAT(    75 )      ! HNO4+HO=NO2
     &   +    4.0000D-01 * RXRAT(    91 )      ! ISO+NO3=0.4000D+...
     &   +    3.0000D-01 * RXRAT(    98 )      ! NO+INO2=0.1300D+...
     &   +    6.5000D-01 * RXRAT(    99 )      ! ISON+HO=0.6500D+...
     &   +                 RXRAT(   100 )      ! HO+INALD=NO2+CO+...
     &   +                 RXRAT(   140 )      ! MPAN+HO=NO2+HKET
     &   +                 RXRAT(   141 )      ! ONIT+HO=NO2+HC3P
     &   +    6.7000D-01 * RXRAT(   142 )      ! TRPN+HO=0.6700D+...
     &   +    6.7000D-01 * RXRAT(   150 )      ! TRPN+O3=0.6700D+...
     &   +    6.7000D-01 * RXRAT(   163 )      ! TRPN+NO3=0.6700D+...
     &   +    6.8000D-01 * RXRAT(   167 )      ! MACR+NO3=0.6800D+...
     &   +                 RXRAT(   174 )      ! MPAN+NO3=NO2+MACP
     &   +                 RXRAT(   178 )      ! PAN=NO2+ACO3
     &   +                 RXRAT(   180 )      ! PPN=NO2+RCO3
     &   +                 RXRAT(   182 )      ! MPAN=NO2+MACP
     &   +    6.9550D-01 * RXRAT(   196 )      ! NO+APINP1=0.1696D+...
     &   +    3.6000D-01 * RXRAT(   200 )      ! NO+LIMNP1=0.1360D+...
     &   +                 RXRAT(   219 )      ! NO+OLND=0.2000D+...
     &   +                 RXRAT(   220 )      ! NO+ADCN=0.2000D+...
     &   +    5.3900D-01 * RXRAT(   238 )      ! APINP1+HO2=0.5390D+...
     &   +    2.3000D-01 * RXRAT(   242 )      ! LIMNP1+HO2=0.2300D+...
     &   +    6.5900D-01 * RXRAT(   277 )      ! APINP1+MO2=0.6590D+...
     &   +    7.5000D-01 * RXRAT(   278 )      ! APINP2+MO2=0.7500D+...
     &   +    3.1000D-01 * RXRAT(   281 )      ! LIMNP1+MO2=0.3100D+...
     &   +    7.5000D-01 * RXRAT(   282 )      ! LIMNP2+MO2=0.7500D+...
     &   +                 RXRAT(   289 )      ! MCP+MO2=NO2+HO2+...
     &   +    5.0000D-01 * RXRAT(   298 )      ! OLND+MO2=0.5000D+...
     &   +    7.0000D-01 * RXRAT(   299 )      ! ADCN+MO2=0.7000D+...
     &   +    8.2500D-01 * RXRAT(   314 )      ! APINP1+ACO3=0.8250D+...
     &   +    5.0000D-01 * RXRAT(   315 )      ! APINP2+ACO3=0.5000D+...
     &   +    4.6000D-01 * RXRAT(   318 )      ! LIMNP1+ACO3=0.4600D+...
     &   +    5.0000D-01 * RXRAT(   319 )      ! LIMNP2+ACO3=0.5000D+...
     &   +                 RXRAT(   326 )      ! MCP+ACO3=NO2+0.5000D+...
     &   +                 RXRAT(   335 )      ! OLND+ACO3=NO2+0.5000D+...
     &   +    7.0000D-01 * RXRAT(   336 )      ! ADCN+ACO3=0.7000D+...
     &   +                 RXRAT(   339 )      ! MO2+NO3=NO2+HCHO+HO2
     &   +                 RXRAT(   340 )      ! ETHP+NO3=NO2+HO2+ACD
     &   +                 RXRAT(   341 )      ! HC3P+NO3=NO2+0.1400D+...
     &   +                 RXRAT(   342 )      ! HC5P+NO3=NO2+0.5500D-...
     &   +                 RXRAT(   343 )      ! ETEP+NO3=NO2+HO2+...
     &   +                 RXRAT(   344 )      ! OLTP+NO3=NO2+0.7900D+...
     &   +                 RXRAT(   345 )      ! OLIP+NO3=NO2+0.7200D+...
     &   +                 RXRAT(   346 )      ! BENP+NO3=NO2+HO2+...
     &   +                 RXRAT(   347 )      ! TOLP+NO3=NO2+0.9146D+...
     &   +                 RXRAT(   348 )      ! XYLP+NO3=NO2+0.9518D+...
     &   +                 RXRAT(   349 )      ! EBZP+NO3=NO2+0.9146D+...
     &   +                 RXRAT(   350 )      ! ISOP+NO3=NO2+HO2+...
     &   +                 RXRAT(   351 )      ! APIP1+NO3=NO2+HO2+...
     &   +                 RXRAT(   352 )      ! LIMP1+NO3=NO2+HO2+...
     &   +    1.8250D+00 * RXRAT(   353 )      ! APINP1+NO3=0.1825D+...
     &   +    1.4600D+00 * RXRAT(   354 )      ! LIMNP1+NO3=0.1460D+...
     &   +                 RXRAT(   355 )      ! ACO3+NO3=NO2+MO2
     &   +                 RXRAT(   356 )      ! RCO3+NO3=NO2+ETHP
     &   +                 RXRAT(   357 )      ! ACTP+NO3=NO2+ACO3+HCHO
     &   +                 RXRAT(   358 )      ! MEKP+NO3=NO2+0.6700D+...
     &   +                 RXRAT(   359 )      ! KETP+NO3=NO2+HO2+DCB1
     &   +                 RXRAT(   360 )      ! MACP+NO3=NO2+0.5380D+...
     &   +                 RXRAT(   361 )      ! MCP+NO3=NO2+HO2+HCHO+HKET
     &   +                 RXRAT(   362 )      ! MVKP+NO3=NO2+0.7000D+...
     &   +                 RXRAT(   363 )      ! UALP+NO3=NO2+HO2+...
     &   +                 RXRAT(   364 )      ! BALP+NO3=NO2+BAL1
     &   +                 RXRAT(   365 )      ! BAL1+NO3=NO2+BAL2
     &   +                 RXRAT(   366 )      ! ADDC+NO3=NO2+HO2+...
     &   +                 RXRAT(   367 )      ! MCTP+NO3=NO2+MCTO
     &   +                 RXRAT(   368 )      ! ORAP+NO3=NO2+GLY+HO2
     &   +                 RXRAT(   369 )      ! OLNN+NO3=NO2+HO2+ONIT
     &   +    2.0000D+00 * RXRAT(   370 )      ! OLND+NO3=0.2000D+...
     &   +    2.0000D+00 * RXRAT(   371 )      ! ADCN+NO3=0.2000D+...
     &   +    5.0000D-01 * RXRAT(   373 )      ! OLNN+OLND=0.5000D+...
     &   +                 RXRAT(   374 )      ! OLND+OLND=NO2+0.5040D+...
     &   +                 RXRAT(   375 )      ! XO2+NO3=NO2
     &   +    4.8000D-01 * RXRAT(   384 )      ! APINP2+APIP1=0.4800D+...
     &   +    4.8000D-01 * RXRAT(   385 )      ! APINP2+LIMP1=0.4800D+...
     &   +    4.8000D-01 * RXRAT(   386 )      ! APINP2+ISOP=0.4800D+...
     &   +    4.8000D-01 * RXRAT(   387 )      ! LIMNP2+APIP1=0.4800D+...
     &   +    4.8000D-01 * RXRAT(   388 )      ! LIMNP2+LIMP1=0.4800D+...
     &   +    4.8000D-01 * RXRAT(   389 )      ! LIMNP2+ISOP=0.4800D+...
     &   +    6.8000D-01 * RXRAT(   395 )      ! ACRO+NO3=0.6800D+...
     &   +                 RXRAT(   399 )      ! BDE13P+NO3=NO2+HO2+...
     &   +                 RXRAT(   410 )      ! FURAN+NO3=NO2+0.8000D+...
     &   +                 RXRAT(   414 )      ! NO+SESQNRO2=0.2000D+...
     &   +    2.0000D+00 * RXRAT(   415 )      ! SESQNRO2+NO3=0.2000D+...
     &   +                 RXRAT(   447 )      ! VROCP6ALKP+NO3=NO2+...
     &   +                 RXRAT(   448 )      ! VROCP5ALKP+NO3=NO2+...
     &   +                 RXRAT(   449 )      ! VROCP4ALKP+NO3=NO2+...
     &   +                 RXRAT(   450 )      ! VROCP3ALKP+NO3=NO2+...
     &   +                 RXRAT(   451 )      ! VROCP2ALKP+NO3=NO2+...
     &   +                 RXRAT(   452 )      ! VROCP1ALKP+NO3=NO2+...
     &   +                 RXRAT(   453 )      ! HC10P+NO3=NO2+HC10P2
     &   +                 RXRAT(   475 )      ! VROCP6ALKP2+NO3=NO2+...
     &   +                 RXRAT(   476 )      ! VROCP5ALKP2+NO3=NO2+...
     &   +                 RXRAT(   477 )      ! VROCP4ALKP2+NO3=NO2+...
     &   +                 RXRAT(   478 )      ! VROCP3ALKP2+NO3=NO2+...
     &   +                 RXRAT(   479 )      ! VROCP2ALKP2+NO3=NO2+...
     &   +                 RXRAT(   480 )      ! VROCP1ALKP2+NO3=NO2+...
     &   +                 RXRAT(   481 )      ! HC10P2+NO3=NO2+KET+HO2
     &   +                 RXRAT(   492 )      ! VROCP6AROP+NO3=NO2+...
     &   +                 RXRAT(   498 )      ! VROCP5AROP+NO3=NO2+...
     &   +                 RXRAT(   504 )      ! NAPHP+NO3=NO2+0.9405D+...
     &   +                 RXRAT(   529 )      ! STYP+NO3=NO2+HO2+HCHO+...
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(    44 ) * YC ( O3          )   ! NO2+O3=NO3
     &   +                 RKI(    62 ) * YC ( O3P         )   ! NO2+O3P=NO3
     &   +                 RKI(    63 ) * YC ( HO          )   ! NO2+HO=HNO3
     &   +                 RKI(    70 ) * YC ( NO3         )   ! NO2+NO3=N2O5
     &   +                 RKI(    73 ) * YC ( HO2         )   ! NO2+HO2=HNO4
     &   +                 RKI(   177 ) * YC ( ACO3        )   ! NO2+ACO3=PAN
     &   +                 RKI(   179 ) * YC ( RCO3        )   ! NO2+RCO3=PPN
     &   +                 RKI(   181 ) * YC ( MACP        )   ! NO2+MACP=MPAN
     &   +                 RKI(   222 ) * YC ( BAL2        )   ! NO2+BAL2=ONIT
     &   +                 RKI(   223 ) * YC ( CHO         )   ! NO2+CHO=ONIT
     &   +                 RKI(   224 ) * YC ( MCTO        )   ! NO2+MCTO=ONIT
     &   +                 RKI(   424 )                        ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 = YC0( O3 ) 


      L3 =                 RKI(     1 )                        ! O3=O3P
     &   +                 RKI(     2 )                        ! O3=O1D
     &   +                 RKI(    41 ) * YC ( HO          )   ! O3+HO=HO2
     &   +                 RKI(    42 ) * YC ( HO2         )   ! O3+HO2=HO
     &   +                 RKI(    44 ) * YC ( NO2         )   ! O3+NO2=NO3
     &   +                 RKI(    46 ) * YC ( O3P         )   ! O3+O3P=
     &   +                 RKI(    90 ) * YC ( ISO         )   ! O3+ISO=0.2500D+...
     &   +                 RKI(   144 ) * YC ( ETE         )   ! O3+ETE=0.8000D-...
     &   +                 RKI(   145 ) * YC ( OLT         )   ! O3+OLT=0.2200D+...
     &   +                 RKI(   146 ) * YC ( OLI         )   ! O3+OLI=0.4600D+...
     &   +                 RKI(   147 ) * YC ( API         )   ! O3+API=0.6055D+...
     &   +                 RKI(   148 ) * YC ( LIM         )   ! O3+LIM=0.6600D+...
     &   +                 RKI(   149 ) * YC ( LIMAL       )   ! O3+LIMAL=0.9000D-...
     &   +                 RKI(   150 ) * YC ( TRPN        )   ! O3+TRPN=0.3300D+...
     &   +                 RKI(   151 ) * YC ( MACR        )   ! O3+MACR=0.1900D+...
     &   +                 RKI(   152 ) * YC ( MVK         )   ! O3+MVK=0.1600D+...
     &   +                 RKI(   153 ) * YC ( UALD        )   ! O3+UALD=0.1000D+...
     &   +                 RKI(   154 ) * YC ( DCB1        )   ! O3+DCB1=0.5000D-...
     &   +                 RKI(   155 ) * YC ( DCB2        )   ! O3+DCB2=0.5000D-...
     &   +                 RKI(   156 ) * YC ( DCB3        )   ! O3+DCB3=0.5000D-...
     &   +                 RKI(   157 ) * YC ( MCTO        )   ! O3+MCTO=MCTP
     &   +                 RKI(   394 ) * YC ( ACRO        )   ! O3+ACRO=0.8400D+...
     &   +                 RKI(   403 ) * YC ( BDE13       )   ! O3+BDE13=0.6200D+...
     &   +                 RKI(   409 ) * YC ( FURAN       )   ! O3+FURAN=0.2000D-...
     &   +                 RKI(   416 ) * YC ( SESQ        )   ! O3+SESQ=0.9820D+...
     &   +                 RKI(   427 )                        ! O3=
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


      L12 =                 RKI(    45 )                        ! O3P=O3
     &    +                 RKI(    46 ) * YC ( O3          )   ! O3P+O3=
     &    +                 RKI(    55 ) * YC ( NO          )   ! O3P+NO=NO2
     &    +                 RKI(    61 ) * YC ( NO2         )   ! O3P+NO2=NO
     &    +                 RKI(    62 ) * YC ( NO2         )   ! O3P+NO2=NO3
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
      RK2 = RKI(  45 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(  43 ) * DTC            ! k1_3  (NO+O3=NO2)

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


