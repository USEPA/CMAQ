
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

R3      REAL( 8 ) ::   K8_8       ! Kcco_o2+cco_o2 x delta t
R3      REAL( 8 ) ::   R8_9       ! Kpan-->cco_o2 x delta t
R3      REAL( 8 ) ::   R9_8       ! Kcco_o2+no2-->pan

C**********************************************************************

S1

c..Production rate of CCO_O2 (except from PAN )

      PCCO_O2 =          RXRAT( 104 ) +         RXRAT( 106 ) +         
     &                                          RXRAT( 112 ) +
     &                   RXRAT( 113 ) + 2.000D0 * RXRAT( 114 ) +
     &                   RXRAT( 130 ) +         RXRAT( 132 ) +
     &                   RXRAT( 136 ) +         RXRAT( 137 ) +
     &           0.492D0 * RXRAT( 138 ) +         RXRAT( 139 ) +
     &                   RXRAT( 149 ) +         RXRAT( 150 ) +
     &                   RXRAT( 151 ) + 2.000D0 * RXRAT( 152 ) +
     &           0.670D0 * RXRAT( 165 ) + 0.675D0 * RXRAT( 166 ) +
     &           0.467D0 * RXRAT( 173 ) + 0.029D0 * RXRAT( 174 ) +
     &           0.667D0 * RXRAT( 175 ) +         RXRAT( 180 ) +
     &           0.500D0 * RXRAT( 181 ) +         RXRAT( 182 ) +
     &           0.500D0 * RXRAT( 183 ) + 0.123D0 * RXRAT( 194 ) +
     &           0.011D0 * RXRAT( 200 ) + 0.137D0 * RXRAT( 209 ) 


 
c..Loss frequency of CCO_O2 ( not including CCO_O2 + CCO_O2 )
      LCCO_O2 =  RKI(   69 ) * YC( NO2      ) + 
     &           RKI(   71 ) * YC( NO       ) +
     &           RKI(   72 ) * YC( HO2      ) +
     &           RKI(   73 ) * YC( NO3      ) +
     &           RKI(   74 ) * YC( C_O2     ) +
     &           RKI(   75 ) * YC( RO2_R    ) +
     &           RKI(   77 ) * YC( RO2_N    ) +
     &           RKI(   88 ) * YC( RCO_O2   ) +
     &           RKI(   99 ) * YC( BZCO_O2  ) 


c..Loss frequency of PAN
      LPAN  =  RKI(   70 ) 

c....R8_8 = K for CCO_O2+CCO_O2= times delta t
c....R8_9 = K for CCO_O2+NO2=PAN times delta t times [NO2]
c....R8_9 = K for PAN=C2O3+NO2 times delta t

      R8_8 = RKI(   78 ) * DTC
      R8_9 = RKI(   70 ) * DTC
      R9_8 = RKI(   69 ) * YC( NO2 ) * DTC

S1

R4..Solution of quadratic equation to get CCO_O2 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0D0 * K8_8 * CMN
      B = CMN * ( 1.0D0 + L8 * DTC ) - R8_9 * R9_8
R5      C = CMN * ( YC0( NCELL, CCO_O2 ) + P8 * DTC ) + R8_9 * YC0( NCELL, PAN )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

R6      YCP( NCELL, CCO_O2 ) = MAX( Q / A , -C / Q  )

R7      YCP( NCELL, PAN ) = ( YC0( NCELL, PAN ) + R8_9 * YCP( NCELL, CCO_O2 ) ) / CMN

      RETURN

      END 
