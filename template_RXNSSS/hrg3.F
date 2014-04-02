
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
R1  PRECONDITIONS: For SAPRC99 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, September, 2003
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

R3      REAL   K8_8       ! Kcco_o2+cco_o2 x delta t
R3      REAL   R8_9       ! Kpan-->cco_o2 x delta t
R3      REAL   R9_8       ! Kcco_o2+no2-->pan

C**********************************************************************

S1

c..Production rate of CCO_O2 (except from PAN )

      PCCO_O2 =          RXRAT( 104 ) +         RXRAT( 106 ) +         
     &                                          RXRAT( 112 ) +
     &                   RXRAT( 113 ) + 2.000 * RXRAT( 114 ) +
     &                   RXRAT( 130 ) +         RXRAT( 132 ) +
     &                   RXRAT( 136 ) +         RXRAT( 137 ) +
     &           0.492 * RXRAT( 138 ) +         RXRAT( 139 ) +
     &                   RXRAT( 149 ) +         RXRAT( 150 ) +
     &                   RXRAT( 151 ) + 2.000 * RXRAT( 152 ) +
     &           0.670 * RXRAT( 165 ) + 0.675 * RXRAT( 166 ) +
     &           0.467 * RXRAT( 173 ) + 0.029 * RXRAT( 174 ) +
     &           0.667 * RXRAT( 175 ) +         RXRAT( 180 ) +
     &           0.500 * RXRAT( 181 ) +         RXRAT( 182 ) +
     &           0.500 * RXRAT( 183 ) + 0.123 * RXRAT( 194 ) +
     &           0.011 * RXRAT( 200 ) + 0.137 * RXRAT( 209 ) 


 
c..Loss frequency of CCO_O2 ( not including CCO_O2 + CCO_O2 )
      LCCO_O2 =  RKI( NCELL,   69 ) * YC( NO2      ) + 
     &           RKI( NCELL,   71 ) * YC( NO       ) +
     &           RKI( NCELL,   72 ) * YC( HO2      ) +
     &           RKI( NCELL,   73 ) * YC( NO3      ) +
     &           RKI( NCELL,   74 ) * YC( C_O2     ) +
     &           RKI( NCELL,   75 ) * YC( RO2_R    ) +
     &           RKI( NCELL,   77 ) * YC( RO2_N    ) +
     &           RKI( NCELL,   88 ) * YC( RCO_O2   ) +
     &           RKI( NCELL,   99 ) * YC( BZCO_O2  ) 


c..Loss frequency of PAN
      LPAN  =  RKI( NCELL,   70 ) 

c....R8_8 = K for CCO_O2+CCO_O2= times delta t
c....R8_9 = K for CCO_O2+NO2=PAN times delta t times [NO2]
c....R8_9 = K for PAN=C2O3+NO2 times delta t

      R8_8 = RKI( NCELL,   78 ) * DTC
      R8_9 = RKI( NCELL,   70 ) * DTC
      R9_8 = RKI( NCELL,   69 ) * YC( NO2 ) * DTC

S1

R4..Solution of quadratic equation to get CCO_O2 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0 * K8_8 * CMN
      B = CMN * ( 1.0 + L8 * DTC ) - R8_9 * R9_8
R5      C = CMN * ( YC0( CCO_O2 ) + P8 * DTC ) + R8_9 * YC0( PAN )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

R6      YCP( CCO_O2 ) = MAX( Q / A , -C / Q  )

R7      YCP( PAN ) = ( YC0( PAN ) + R8_9 * YCP( CCO_O2 ) ) / CMN

      RETURN

      END 
