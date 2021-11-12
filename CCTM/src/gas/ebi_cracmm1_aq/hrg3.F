
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

       SUBROUTINE HRG3( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of C2O3 and PAN
C
C  PRECONDITIONS: For the CRACMM1_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Oct 14, 2021
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE

C..INCLUDES:  NONE

C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC              ! Time step


C..PARAMETERS: NONE


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
!     CHARACTER( 16 ), SAVE  :: PNAME = 'HRG3'      ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) ::   A, B, C, Q   ! Quadratic equation terms
      REAL( 8 ) ::   CMN          ! Temp scalar
      REAL( 8 ) ::   L8           ! Loss of CCO_O2
      REAL( 8 ) ::   L9           ! Loss of PAN
      REAL( 8 ) ::   P8           ! Production of CCO_O2

      REAL( 8 ) ::   K8_8         ! Kaco3+aco3 * delta t
      REAL( 8 ) ::   R8_9         ! Kpan-->aco3 * delta t
      REAL( 8 ) ::   R9_8         ! Kaco3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of ACO3 (except from PAN )
      P8 =                 RXRAT(    14 )      ! ACT=ACO3+MO2
     &   +    7.8400D-01 * RXRAT(    15 )      ! UALD=0.7840D+00*ACO3+...
     &   +                 RXRAT(    16 )      ! MEK=ACO3+0.5000D+00*ETHP+...
     &   +                 RXRAT(    17 )      ! KET=ACO3+ETHP
     &   +                 RXRAT(    18 )      ! HKET=ACO3+HO2+HCHO
     &   +    6.7000D-01 * RXRAT(    19 )      ! MACR=0.6700D+00*ACO3+...
     &   +                 RXRAT(    24 )      ! MGLY=ACO3+HO2+CO
     &   +    2.5000D-01 * RXRAT(    25 )      ! DCB1=0.2500D+00*ACO3+...
     &   +    2.5000D-01 * RXRAT(    26 )      ! DCB2=0.2500D+00*ACO3+...
     &   +                 RXRAT(    88 )      ! ACD+HO=ACO3
     &   +    3.1300D-01 * RXRAT(    96 )      ! UALD+HO=0.3130D+00*ACO3+...
     &   +                 RXRAT(    98 )      ! MGLY+HO=ACO3+CO
     &   +    6.5000D-01 * RXRAT(   117 )      ! PAA+HO=0.6500D+00*ACO3+...
     &   +    9.0000D-02 * RXRAT(   126 )      ! OLI+O3=0.9000D-01*ACO3+...
     &   +    1.0000D-01 * RXRAT(   127 )      ! ISO+O3=0.1000D+00*ACO3+...
     &   +    1.0000D-01 * RXRAT(   130 )      ! MACR+O3=0.1000D+00*ACO3+...
     &   +    2.8000D-01 * RXRAT(   131 )      ! MVK+O3=0.2800D+00*ACO3+...
     &   +    2.0000D-03 * RXRAT(   132 )      ! UALD+O3=0.2000D-02*ACO3+...
     &   +                 RXRAT(   145 )      ! ACD+NO3=ACO3+HNO3
     &   +                 RXRAT(   150 )      ! MGLY+NO3=ACO3+CO+HNO3
     &   +                 RXRAT(   189 )      ! ACTP+NO=ACO3+NO2+HCHO
     &   +    2.3000D-01 * RXRAT(   191 )      ! KETP+NO=0.2300D+00*ACO3+...
     &   +    3.5000D-01 * RXRAT(   192 )      ! MACP+NO=0.3500D+00*ACO3+...
     &   +    7.0000D-01 * RXRAT(   194 )      ! MVKP+NO=0.7000D+00*ACO3+...
     &   +    1.5000D-01 * RXRAT(   229 )      ! ACTP+HO2=0.1500D+00*ACO3+...
     &   +    5.0000D-01 * RXRAT(   265 )      ! ACTP+MO2=0.5000D+00*ACO3+...
     &   +    2.6900D-01 * RXRAT(   268 )      ! MACP+MO2=0.2690D+00*ACO3+...
     &   +    1.1600D+00 * RXRAT(   270 )      ! MVKP+MO2=0.1160D+01*ACO3+...
     &   +    1.6000D-01 * RXRAT(   306 )      ! MVKP+ACO3=0.1160D+01*ACO3+...
     &   +                 RXRAT(   339 )      ! ACTP+NO3=ACO3+NO2+HCHO
     &   +    5.3800D-01 * RXRAT(   342 )      ! MACP+NO3=0.5380D+00*ACO3+...
     &   +    7.0000D-01 * RXRAT(   344 )      ! MVKP+NO3=0.7000D+00*ACO3+...

c..Loss frequency of ACO3 ( not including ACO3 + ACO3 )
      L8 =                 RKI(   162 ) * YC ( NO2      )   ! ACO3+NO2=PAN
     &   +                 RKI(   187 ) * YC ( NO       )   ! ACO3+NO=MO2+NO2
     &   +                 RKI(   227 ) * YC ( HO2      )   ! ACO3+HO2=0.4400D+...
     &   +                 RKI(   263 ) * YC ( MO2      )   ! ACO3+MO2=0.9000D+...
     &   +                 RKI(   281 ) * YC ( ETHP     )   ! ACO3+ETHP=...
     &   +                 RKI(   282 ) * YC ( HC3P     )   ! ACO3+HC3P=...
     &   +                 RKI(   283 ) * YC ( HC5P     )   ! ACO3+HC5P=...
     &   +                 RKI(   284 ) * YC ( HC8P     )   ! ACO3+HC8P=...
     &   +                 RKI(   285 ) * YC ( ETEP     )   ! ACO3+ETEP=...
     &   +                 RKI(   286 ) * YC ( OLTP     )   ! ACO3+OLTP=...
     &   +                 RKI(   287 ) * YC ( OLIP     )   ! ACO3+OLIP=...
     &   +                 RKI(   288 ) * YC ( BENP     )   ! ACO3+BENP=...
     &   +                 RKI(   289 ) * YC ( TLP1     )   ! ACO3+TLP1=MO2+BALD
     &   +                 RKI(   290 ) * YC ( TOLP     )   ! ACO3+TOLP=HO2+...
     &   +                 RKI(   291 ) * YC ( PER1     )   ! ACO3+PER1=HO2+...
     &   +                 RKI(   292 ) * YC ( XYL1     )   ! ACO3+XYL1=MO2+BALD
     &   +                 RKI(   293 ) * YC ( XYLP     )   ! ACO3+XYLP=HO2+...
     &   +                 RKI(   294 ) * YC ( PER2     )   ! ACO3+PER2=HO2+...
     &   +                 RKI(   295 ) * YC ( XYOP     )   ! ACO3+XYOP=HO2+...
     &   +                 RKI(   296 ) * YC ( ISOP     )   ! ACO3+ISOP=...
     &   +                 RKI(   297 ) * YC ( APIP     )   ! ACO3+APIP=...
     &   +                 RKI(   298 ) * YC ( LIMP     )   ! ACO3+LIMP=...
     &   +                 RKI(   300 ) * YC ( RCO3     )   ! ACO3+RCO3=MO2+ETHP
     &   +    5.0000D-01 * RKI(   301 ) * YC ( ACTP     )   ! ACO3+ACTP=...
     &   +                 RKI(   302 ) * YC ( MEKP     )   ! ACO3+MEKP=...
     &   +                 RKI(   303 ) * YC ( KETP     )   ! ACO3+KETP=...
     &   +    7.3100D-01 * RKI(   304 ) * YC ( MACP     )   ! ACO3+MACP=...
     &   +                 RKI(   305 ) * YC ( MCP      )   ! ACO3+MCP=NO2+...
     &   +                 RKI(   307 ) * YC ( UALP     )   ! ACO3+UALP=...
     &   +                 RKI(   308 ) * YC ( BALP     )   ! ACO3+BALP=MO2+BAL1
     &   +                 RKI(   309 ) * YC ( BAL1     )   ! ACO3+BAL1=MO2+BAL2
     &   +                 RKI(   310 ) * YC ( ADDC     )   ! ACO3+ADDC=...
     &   +                 RKI(   311 ) * YC ( MCTP     )   ! ACO3+MCTP=HO2+...
     &   +                 RKI(   312 ) * YC ( ORAP     )   ! ACO3+ORAP=MO2+GLY
     &   +                 RKI(   313 ) * YC ( OLNN     )   ! ACO3+OLNN=HO2+...
     &   +                 RKI(   314 ) * YC ( OLND     )   ! ACO3+OLND=...
     &   +                 RKI(   315 ) * YC ( ADCN     )   ! ACO3+ADCN=HO2+...
     &   +                 RKI(   316 ) * YC ( XO2      )   ! ACO3+XO2=MO2
     &   +                 RKI(   337 ) * YC ( NO3      )   ! ACO3+NO3=MO2+NO2

c..Loss frequency of PAN
      L9 =                 RKI(    32 )                     ! PAN=ACO3+NO2
     &   +                 RKI(    33 )                     ! PAN=MO2+NO3
     &   +                 RKI(   118 ) * YC ( HO       )   ! PAN+HO=XO2+NO3+HCHO
     &   +                 RKI(   163 )                     ! PAN=ACO3+NO2

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI(   299 ) * DTC

      R8_9  = ( RKI(    32 )
     &      +   RKI(   163 ) ) * DTC 

      R9_8  = ( RKI(   162 ) * YC( NO2 ) ) * DTC 

c..Solution of quadratic equation to get ACO3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0D0 * K8_8 * CMN
      B = CMN * ( 1.0D0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( ACO3 ) + P8 * DTC ) +  R8_9 * YC0( PAN )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( ACO3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) +  R9_8 * YCP( ACO3 ) ) / CMN

      RETURN

      END
