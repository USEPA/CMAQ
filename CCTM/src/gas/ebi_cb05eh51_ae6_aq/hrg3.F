
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
C  PRECONDITIONS: For the CB05eh51 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 26, 2017
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
      P8 =                 RXRAT(  87 )      ! ALD2+O=C2O3+OH
     &   +                 RXRAT(  88 )      ! ALD2+OH=C2O3
     &   +                 RXRAT(  89 )      ! ALD2+NO3=C2O3+HNO3
     &   +                 RXRAT(  99 )      ! PACD+OH=C2O3
     &   +                 RXRAT( 160 )      ! OH+MGLY=C2O3+XO2
     &   +                 RXRAT( 161 )      ! MGLY=C2O3+HO2+CO
     &   +    3.3000D-01 * RXRAT( 169 )      ! OH+ISPD=0.3300D+00*C2O3+...
     &   +    1.1400D-01 * RXRAT( 170 )      ! O3+ISPD=0.1140D+00*C2O3+...
     &   +    2.6700D-01 * RXRAT( 172 )      ! ISPD=0.2670D+00*C2O3+...
     &   +                 RXRAT( 201 )      ! CL+ALD2=C2O3+HCL
     &   +    4.0000D-01 * RXRAT( 227 )      ! MAPAN=0.4000D+00*C2O3+...
     &   +    1.5000D-01 * RXRAT( 229 )      ! MACO3+HO2=0.1500D+00*C2O3+...
     &   +    3.0000D-01 * RXRAT( 230 )      ! MACO3+MEO2=0.3000D+00*C2O3+...
     &   +    3.0000D-01 * RXRAT( 231 )      ! MACO3+XO2=0.3000D+00*C2O3+...
     &   +    3.5000D-01 * RXRAT( 232 )      ! MACO3+CXO3=0.3500D+00*C2O3+...
     &   +                 RXRAT( 415 )      ! BR+ALD2=C2O3+HBR

c..Loss frequency of C2O3 ( not including C2O3 + C2O3 )
      L8 =                 RKI(  68 ) * YC( IEPXO2       )   ! C2O3+IEPXO2=...
     &   +                 RKI(  91 ) * YC( NO           )   ! C2O3+NO=MEO2+NO2
     &   +                 RKI(  92 ) * YC( NO2          )   ! C2O3+NO2=PAN
     &   +                 RKI(  95 ) * YC( HO2          )   ! C2O3+HO2=0.4100D+...
     &   +                 RKI(  96 ) * YC( MEO2         )   ! C2O3+MEO2=...
     &   +                 RKI(  97 ) * YC( XO2          )   ! C2O3+XO2=0.9000D+...
     &   +                 RKI( 115 ) * YC( CXO3         )   ! C2O3+CXO3=MEO2+...
     &   +                 RKI( 165 ) * YC( ISOPO2       )   ! C2O3+ISOPO2=...
     &   +    6.5000D-01 * RKI( 233 ) * YC( MACO3        )   ! C2O3+MACO3=...
     &   +                 RKI( 419 ) * YC( BRO          )   ! C2O3+BRO=BR+MEO2

c..Loss frequency of PAN
      L9 =                 RKI(  93 )                        ! PAN=C2O3+NO2
     &   +                 RKI(  94 )                        ! PAN=0.6000D+...

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI(  98 ) * DTC

      R8_9  = ( RKI(  93 )
     &      +   RKI(  94 ) ) * DTC 

      R9_8  = ( RKI(  92 ) * YC( NO2 ) ) * DTC 

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
