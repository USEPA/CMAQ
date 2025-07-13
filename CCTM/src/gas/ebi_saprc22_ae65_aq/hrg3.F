
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
C  PRECONDITIONS: For the SAPRC22_AE65_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 20, 2025
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
      P8 =    6.0000D-01 * RXRAT(   122 )      ! PAN=0.6000D+00*MECO3+...
     &   +                 RXRAT(   360 )      ! xMECO3+NO=MECO3+NO+SumRCO3
     &   +                 RXRAT(   362 )      ! xMECO3+NO3=MECO3+NO3+SumRCO3
     &   +    5.0000D-01 * RXRAT(   363 )      ! xMECO3+SumRO2=0.5000D+...
     &   +                 RXRAT(   364 )      ! xMECO3+SumRCO3=MECO3+...
     &   +    1.6000D-01 * RXRAT(   486 )      ! ISOP+O3=0.1600D+00*MECO3+...
     &   +    9.5000D-01 * RXRAT(   529 )      ! MECHO+OH=0.9500D+00*MECO3+...
     &   +                 RXRAT(   532 )      ! MECHO+NO3=MECO3+HNO3+SumRCO3
     &   +    1.0000D-01 * RXRAT(   533 )      ! MECHO=0.1000D+00*MECO3+...
     &   +                 RXRAT(   552 )      ! ACET=MECO3+MEO2+SumRO2+SumRCO3
     &   +    8.5000D-01 * RXRAT(   554 )      ! MEK=0.8500D+00*MECO3+...
     &   +    1.0000D-02 * RXRAT(   558 )      ! MACR+O3=0.1000D-01*MECO3+...
     &   +    1.0000D-02 * RXRAT(   641 )      ! OLEA1+O3=0.1000D-01*MECO3+...
     &   +    2.0000D-02 * RXRAT(   658 )      ! KET2+OH=0.2000D-01*MECO3+...
     &   +    5.1000D-01 * RXRAT(   659 )      ! KET2=0.5100D+00*MECO3+...
     &   +    1.3000D-01 * RXRAT(   660 )      ! LVKS+OH=0.1300D+00*MECO3+...
     &   +    1.0000D-02 * RXRAT(   663 )      ! LVKS+O3=0.1000D-01*MECO3+...
     &   +    2.6000D-01 * RXRAT(   675 )      ! PACID+OH=0.2600D+00*MECO3+...
     &   +    9.9000D-01 * RXRAT(   677 )      ! MGLY+OH=0.9900D+00*MECO3+...
     &   +                 RXRAT(   678 )      ! MGLY+NO3=MECO3+HNO3+CO+SumRCO3
     &   +                 RXRAT(   679 )      ! MGLY=MECO3+HO2+CO+SumRCO3
     &   +    2.0000D+00 * RXRAT(   680 )      ! BACL=0.2000D+01*MECO3+...
     &   +    1.1000D-01 * RXRAT(   690 )      ! RCNO3=0.1100D+00*MECO3+...

c..Loss frequency of MECO3 ( not including MECO3 + MECO3 )
      L8 =                 RKI(    82 ) * YC ( NO           )   ! MECO3+NO=NO2+...
     &   +                 RKI(    83 ) * YC ( NO2          )   ! MECO3+NO2=PAN
     &   +                 RKI(    84 ) * YC ( NO3          )   ! MECO3+NO3=NO2+...
     &   +                 RKI(    85 ) * YC ( HO2          )   ! MECO3+HO2=...
     &   +                 RKI(    86 ) * YC ( SumRO2       )   ! MECO3+SumRO2=...
     &   +                 RKI(    87 ) * YC ( SumRCO3      )   ! MECO3+SumRCO3=...

c..Loss frequency of PAN
      L9 =                 RKI(   121 )                         ! PAN=NO2+MECO3+...
     &   +                 RKI(   122 )                         ! PAN=0.6000D+...

c..K8_8, R8_9, and R9_8 terms
      K8_8  = 1.0D-18

      R8_9  = ( RKI(   121 ) ) * DTC 

      R9_8  = ( RKI(    83 ) * YC( NO2 ) ) * DTC 

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
