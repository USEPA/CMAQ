
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE HRG3( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of C2O3 and PAN
C
C  PRECONDITIONS: For the SAPRC07TB family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, June 1, 2012
C
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE

C..INCLUDES:  NONE

C..ARGUMENTS:
      REAL DTC              ! Time step


C..PARAMETERS: NONE


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
      CHARACTER( 16 ), SAVE  :: PNAME = 'HRG3'      ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL   A, B, C, Q   ! Quadratic equation terms
      REAL   CMN          ! Temp scalar
      REAL   L8           ! Loss of CCO_O2
      REAL   L9           ! Loss of PAN
      REAL   P8           ! Production of CCO_O2

      REAL   K8_8         ! Kmeco3+meco3 * delta t
      REAL   R8_9         ! Kpan-->meco3 * delta t
      REAL   R9_8         ! Kmeco3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of MECO3 (except from PAN )
      P8 =   0.400 * RXRAT( 102 )      ! MAPAN=0.4*MECO3+0.6*NO2+...
     &    +          RXRAT( 103 )      ! MACO3+NO=MECO3+CO2+HCHO+NO2
     &    +          RXRAT( 105 )      ! MACO3+NO3=MECO3+CO2+HCHO+NO2
     &    +          RXRAT( 106 )      ! MACO3+MEO2=MECO3+HO2+CO2+...
     &    +          RXRAT( 107 )      ! MACO3+RO2C=MECO3+HCHO+CO2
     &    +          RXRAT( 108 )      ! MACO3+RO2XC=MECO3+HCHO+CO2
     &    +          RXRAT( 110 )      ! MACO3+RCO3=MECO3+HCHO+RO2C+...
     &    +          RXRAT( 111 )      ! MACO3+BZCO3=MECO3+HCHO+BZO+...
     &    +  2.000 * RXRAT( 112 )      ! MACO3+MACO3=2*MECO3+2*HCHO+...
     &    +          RXRAT( 126 )      ! xMECO3=MECO3
     &    +          RXRAT( 140 )      ! CCHO+OH=MECO3
     &    +          RXRAT( 142 )      ! CCHO+NO3=MECO3+HNO3
     &    +  0.620 * RXRAT( 147 )      ! ACETONE=0.62*MECO3+...
     &    +          RXRAT( 149 )      ! MEK=MECO3+RO2C+xHO2+xCCHO+...
     &    +          RXRAT( 166 )      ! MGLY=MECO3+CO+HO2
     &    +          RXRAT( 167 )      ! MGLY+OH=MECO3+CO
     &    +          RXRAT( 168 )      ! MGLY+NO3=MECO3+CO+HNO3
     &    +  2.000 * RXRAT( 169 )      ! BACL=2*MECO3
     &    +  0.305 * RXRAT( 180 )      ! AFG1=0.305*MECO3+...
     &    +  0.013 * RXRAT( 185 )      ! AFG3+O3=0.013*MECO3+...
     &    +  0.340 * RXRAT( 190 )      ! MACR=0.34*MECO3+0.67*HO2+...
     &    +  0.467 * RXRAT( 198 )      ! IPRD=0.467*MECO3+1.233*HO2+...
     &    +  0.400 * RXRAT( 200 )      ! PRD2=0.4*MECO3+0.913*xHO2+...
     &    +          RXRAT( 203 )      ! HOCCHO+OH=MECO3
     &    +          RXRAT( 205 )      ! HOCCHO+NO3=MECO3+HNO3
     &    +  0.980 * RXRAT( 211 )      ! CCOOOH+OH=0.98*MECO3+...
     &    +          RXRAT( 341 )      ! CCHO+CL=MECO3+HCL
     &    +          RXRAT( 349 )      ! MGLY+CL=MECO3+CO+HCL
     &    +          RXRAT( 362 )      ! CLACET=MECO3+RO2C+xCL+...

c..Loss frequency of MECO3 ( not including MECO3 + MECO3 )
      L8 =           RKI(  67 ) * YC( NO2              )   ! MECO3+NO2=PAN
     &    +          RKI(  70 ) * YC( NO               )   ! MECO3+NO=MEO2+...
     &    +          RKI(  71 ) * YC( HO2              )   ! MECO3+HO2=...
     &    +          RKI(  72 ) * YC( NO3              )   ! MECO3+NO3=MEO2+...
     &    +          RKI(  73 ) * YC( MEO2             )   ! MECO3+MEO2=...
     &    +          RKI(  74 ) * YC( RO2C             )   ! MECO3+RO2C=MEO2+CO2
     &    +          RKI(  75 ) * YC( RO2XC            )   ! MECO3+RO2XC=MEO2+CO2
     &    +          RKI(  86 ) * YC( RCO3             )   ! MECO3+RCO3=2*CO2+...
     &    +          RKI(  97 ) * YC( BZCO3            )   ! MECO3+BZCO3=...

c..Loss frequency of PAN
      L9 =           RKI(  68 )                            ! PAN=MECO3+NO2
     &    +          RKI(  69 )                            ! PAN=0.6*MECO3+...

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI(  76 ) * DTC

      R8_9  = ( RKI(  68 )
     &      +   RKI(  69 ) ) * DTC 

      R9_8  = ( RKI(  67 ) * YC( NO2 ) ) * DTC 

c..Solution of quadratic equation to get MECO3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0 * K8_8 * CMN
      B = CMN * ( 1.0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( MECO3 ) + P8 * DTC ) +  R8_9 * YC0( PAN )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP( MECO3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) +  R9_8 * YCP( MECO3 ) ) / CMN

      RETURN

      END
