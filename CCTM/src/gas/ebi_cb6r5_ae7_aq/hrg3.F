
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
C  PRECONDITIONS: For the CB6R5_AE7_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
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

      REAL( 8 ) ::   K8_8         ! Kc2o3+c2o3 * delta t
      REAL( 8 ) ::   R8_9         ! Kpan-->c2o3 * delta t
      REAL( 8 ) ::   R9_8         ! Kc2o3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of C2O3 (except from PAN )
      P8 =                 RXRAT(    95 )      ! PACD+OH=C2O3
     &   +                 RXRAT(   105 )      ! ALD2+O=C2O3+OH
     &   +                 RXRAT(   106 )      ! ALD2+OH=C2O3
     &   +                 RXRAT(   107 )      ! ALD2+NO3=C2O3+HNO3
     &   +    8.0000D-01 * RXRAT(   113 )      ! GLYD+OH=0.8000D+00*C2O3+...
     &   +                 RXRAT(   115 )      ! GLYD+NO3=C2O3+HNO3
     &   +                 RXRAT(   119 )      ! MGLY=C2O3+HO2+CO
     &   +                 RXRAT(   120 )      ! MGLY+NO3=C2O3+HNO3+XO2+RO2
     &   +                 RXRAT(   121 )      ! MGLY+OH=C2O3+CO
     &   +    5.0000D-01 * RXRAT(   128 )      ! KET=0.5000D+00*C2O3+...
     &   +    6.2000D-01 * RXRAT(   129 )      ! ACET=0.6200D+00*C2O3+...
     &   +                 RXRAT(   130 )      ! ACET+OH=C2O3+FORM+XO2+RO2
     &   +    2.6900D-01 * RXRAT(   158 )      ! ISPD+OH=0.2690D+00*C2O3+...
     &   +    1.4300D-01 * RXRAT(   159 )      ! ISPD+O3=0.1430D+00*C2O3+...
     &   +    2.0800D-01 * RXRAT(   161 )      ! ISPD=0.2080D+00*C2O3+...
     &   +    3.0000D-01 * RXRAT(   202 )      ! XOPN=0.3000D+00*C2O3+XO2H+...
     &   +    6.0000D-01 * RXRAT(   204 )      ! XOPN+O3=0.6000D+00*C2O3+...
     &   +    1.2000D-01 * RXRAT(   208 )      ! OPEN+O3=0.1200D+00*C2O3+...
     &   +                 RXRAT(   244 )      ! CL+ALD2=C2O3+HCL

c..Loss frequency of C2O3 ( not including C2O3 + C2O3 )
      L8 =                 RKI(    53 ) * YC ( NO           )   ! C2O3+NO=NO2+MEO2+RO2
     &   +                 RKI(    54 ) * YC ( NO2          )   ! C2O3+NO2=PAN
     &   +                 RKI(    57 ) * YC ( HO2          )   ! C2O3+HO2=0.3700D+...
     &   +                 RKI(    58 ) * YC ( RO2          )   ! C2O3+RO2=MEO2
     &   +                 RKI(    60 ) * YC ( CXO3         )   ! C2O3+CXO3=MEO2+...
     &   +                 RKI(    73 ) * YC ( MEO2         )   ! C2O3+MEO2=FORM+...
     &   +                 RKI(    77 ) * YC ( XO2H         )   ! C2O3+XO2H=...
     &   +                 RKI(    81 ) * YC ( XO2          )   ! C2O3+XO2=0.8000D+...
     &   +                 RKI(    85 ) * YC ( XO2N         )   ! C2O3+XO2N=...
     &   +                 RKI(   153 ) * YC ( ISO2         )   ! C2O3+ISO2=...
     &   +                 RKI(   168 ) * YC ( EPX2         )   ! C2O3+EPX2=...
     &   +                 RKI(   181 ) * YC ( BZO2         )   ! C2O3+BZO2=GLY+...
     &   +                 RKI(   186 ) * YC ( TO2          )   ! C2O3+TO2=0.4800D+...
     &   +                 RKI(   193 ) * YC ( XLO2         )   ! C2O3+XLO2=...
     &   +                 RKI(   216 ) * YC ( OPO3         )   ! C2O3+OPO3=MEO2+...

c..Loss frequency of PAN
      L9 =                 RKI(    55 )                         ! PAN=NO2+C2O3
     &   +                 RKI(    56 )                         ! PAN=0.6000D+...

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI(    59 ) * DTC

      R8_9  = ( RKI(    55 )
     &      +   RKI(    56 ) ) * DTC 

      R9_8  = ( RKI(    54 ) * YC( NO2 ) ) * DTC 

c..Solution of quadratic equation to get C2O3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0D0 * K8_8 * CMN
      B = CMN * ( 1.0D0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( C2O3 ) + P8 * DTC ) +  R8_9 * YC0( PAN )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( C2O3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) +  R9_8 * YCP( C2O3 ) ) / CMN

      RETURN

      END
