
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
C  PRECONDITIONS: For the SAPRC07TIC_AE7I_AQ mechanism
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

      REAL( 8 ) ::   K8_8         ! Kmeco3+meco3 * delta t
      REAL( 8 ) ::   R8_9         ! Kpan-->meco3 * delta t
      REAL( 8 ) ::   R9_8         ! Kmeco3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of MECO3 (except from PAN )
      P8 =    4.0000D-01 * RXRAT(    98 )      ! MAPAN=0.4000D+00*MECO3+...
     &   +                 RXRAT(   144 )      ! xMECO3+NO=MECO3+NO
     &   +                 RXRAT(   146 )      ! xMECO3+NO3=MECO3+NO3
     &   +    5.0000D-01 * RXRAT(   147 )      ! xMECO3+MEO2=0.5000D+...
     &   +    5.0000D-01 * RXRAT(   148 )      ! xMECO3+RO2C=0.5000D+...
     &   +    5.0000D-01 * RXRAT(   149 )      ! xMECO3+RO2XC=0.5000D+...
     &   +                 RXRAT(   150 )      ! xMECO3+MECO3=0.2000D+01*MECO3
     &   +                 RXRAT(   151 )      ! xMECO3+RCO3=MECO3+RCO3
     &   +                 RXRAT(   152 )      ! xMECO3+BZCO3=MECO3+BZCO3
     &   +                 RXRAT(   153 )      ! xMECO3+MACO3=MECO3+MACO3
     &   +                 RXRAT(   198 )      ! CCHO+OH=MECO3
     &   +                 RXRAT(   200 )      ! CCHO+NO3=MECO3+HNO3
     &   +    6.2000D-01 * RXRAT(   205 )      ! ACETONE=0.6200D+00*MECO3+...
     &   +                 RXRAT(   207 )      ! MEK=MECO3+RO2C+xHO2+xCCHO+...
     &   +                 RXRAT(   224 )      ! MGLY=MECO3+CO+HO2
     &   +                 RXRAT(   225 )      ! MGLY+OH=MECO3+CO
     &   +                 RXRAT(   226 )      ! MGLY+NO3=MECO3+CO+HNO3
     &   +    2.0000D+00 * RXRAT(   227 )      ! BACL=0.2000D+01*MECO3
     &   +    3.0500D-01 * RXRAT(   238 )      ! AFG1=0.3050D+00*MECO3+...
     &   +    1.3000D-02 * RXRAT(   243 )      ! AFG3+O3=0.1300D-01*MECO3+...
     &   +    4.6700D-01 * RXRAT(   252 )      ! IPRD=0.4670D+00*MECO3+...
     &   +    4.0000D-01 * RXRAT(   254 )      ! PRD2=0.4000D+00*MECO3+...
     &   +                 RXRAT(   258 )      ! HOCCHO+NO3=MECO3+HNO3
     &   +    9.8000D-01 * RXRAT(   264 )      ! CCOOOH+OH=0.9800D+00*MECO3+...
     &   +                 RXRAT(   580 )      ! TERPNRO2+MACO3=MECO3+...
     &   +                 RXRAT(   581 )      ! TERPNRO2+IMACO3=MECO3+...
     &   +                 RXRAT(   610 )      ! CCHO+CL=MECO3+HCL
     &   +                 RXRAT(   618 )      ! MGLY+CL=MECO3+CO+HCL
     &   +                 RXRAT(   630 )      ! CLACET=MECO3+RO2C+xCL+...
     &   +    3.5000D-01 * RXRAT(   733 )      ! HC5+O3=0.3500D+00*MECO3+...
     &   +    6.2500D-01 * RXRAT(   775 )      ! MVKOO+NO=0.6250D+00*MECO3+...
     &   +    3.5000D-01 * RXRAT(   777 )      ! MVKOO+MEO2=0.3500D+...
     &   +    3.5000D-01 * RXRAT(   778 )      ! MVKOO+RO2C=0.3500D+...
     &   +    4.4000D-01 * RXRAT(   786 )      ! MACO3+HO2=0.4400D+00*MECO3+...
     &   +                 RXRAT(   798 )      ! HACET=MECO3+HO2+HCHO
     &   +                 RXRAT(   803 )      ! PROPNN=MECO3+HCHO+NO2
     &   +                 RXRAT(   804 )      ! ISOPNN=MECO3+HCHO+0.2000D+...
     &   +                 RXRAT(   806 )      ! MVKN=MECO3+NO2+HOCCHO
     &   +    3.4000D-01 * RXRAT(   825 )      ! MACR=0.3400D+00*MECO3+...
     &   +    4.0000D-01 * RXRAT(   841 )      ! IMAPAN=0.4000D+00*MECO3+...
     &   +                 RXRAT(   846 )      ! xMECO3+IMACO3=MECO3+IMACO3

c..Loss frequency of MECO3 ( not including MECO3 + MECO3 )
      L8 =                 RKI(    63 ) * YC ( NO2          )   ! MECO3+NO2=PAN
     &   +                 RKI(    66 ) * YC ( NO           )   ! MECO3+NO=MEO2+...
     &   +                 RKI(    67 ) * YC ( HO2          )   ! MECO3+HO2=...
     &   +                 RKI(    68 ) * YC ( NO3          )   ! MECO3+NO3=MEO2+...
     &   +                 RKI(    69 ) * YC ( MEO2         )   ! MECO3+MEO2=...
     &   +                 RKI(    70 ) * YC ( RO2C         )   ! MECO3+RO2C=MEO2+CO2
     &   +                 RKI(    71 ) * YC ( RO2XC        )   ! MECO3+RO2XC=MEO2+CO2
     &   +                 RKI(    82 ) * YC ( RCO3         )   ! MECO3+RCO3=...
     &   +                 RKI(    93 ) * YC ( BZCO3        )   ! MECO3+BZCO3=...
     &   +                 RKI(   577 ) * YC ( TERPNRO2     )   ! MECO3+TERPNRO2=...
     &   +                 RKI(   715 ) * YC ( ISOPO2       )   ! MECO3+ISOPO2=...
     &   +                 RKI(   726 ) * YC ( NISOPO2      )   ! MECO3+NISOPO2=...
     &   +                 RKI(   732 ) * YC ( HC5OO        )   ! MECO3+HC5OO=MEO2+...
     &   +                 RKI(   739 ) * YC ( ISOPNOOD     )   ! MECO3+ISOPNOOD=...
     &   +                 RKI(   746 ) * YC ( ISOPNOOB     )   ! MECO3+ISOPNOOB=...
     &   +                 RKI(   755 ) * YC ( NIT1NO3OOA   )   ! MECO3+NIT1NO3OOA=...
     &   +                 RKI(   761 ) * YC ( NIT1NO3OOB   )   ! MECO3+NIT1NO3OOB=...
     &   +                 RKI(   768 ) * YC ( NIT1OHOO     )   ! MECO3+NIT1OHOO=...
     &   +                 RKI(   773 ) * YC ( DIBOO        )   ! MECO3+DIBOO=HO2+...
     &   +    3.0000D-01 * RKI(   779 ) * YC ( MVKOO        )   ! MECO3+MVKOO=MEO2+...
     &   +                 RKI(   784 ) * YC ( MACROO       )   ! MECO3+MACROO=...
     &   +                 RKI(   791 ) * YC ( MACO3        )   ! MECO3+MACO3=...
     &   +                 RKI(   818 ) * YC ( IEPOXOO      )   ! MECO3+IEPOXOO=...
     &   +                 RKI(   833 ) * YC ( IMACO3       )   ! MECO3+IMACO3=...

c..Loss frequency of PAN
      L9 =                 RKI(    64 )                         ! PAN=MECO3+NO2
     &   +                 RKI(    65 )                         ! PAN=0.6000D+...

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI(    72 ) * DTC

      R8_9  = ( RKI(    64 )
     &      +   RKI(    65 ) ) * DTC 

      R9_8  = ( RKI(    63 ) * YC( NO2 ) ) * DTC 

c..Solution of quadratic equation to get MECO3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0D0 * K8_8 * CMN
      B = CMN * ( 1.0D0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( MECO3 ) + P8 * DTC ) +  R8_9 * YC0( PAN )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( MECO3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) +  R9_8 * YCP( MECO3 ) ) / CMN

      RETURN

      END
