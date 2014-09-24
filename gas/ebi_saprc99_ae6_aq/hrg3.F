
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
C  PRECONDITIONS: For the SAPRC99 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, July 18, 2014
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
      CHARACTER( 16 ), SAVE  :: PNAME = 'HRG3'      ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) ::   A, B, C, Q   ! Quadratic equation terms
      REAL( 8 ) ::   CMN          ! Temp scalar
      REAL( 8 ) ::   L8           ! Loss of CCO_O2
      REAL( 8 ) ::   L9           ! Loss of PAN
      REAL( 8 ) ::   P8           ! Production of CCO_O2

      REAL( 8 ) ::   K8_8         ! Kcco_o2+cco_o2 * delta t
      REAL( 8 ) ::   R8_9         ! Kpan-->cco_o2 * delta t
      REAL( 8 ) ::   R9_8         ! Kcco_o2+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of CCO_O2 (except from PAN )
      P8 =                 RXRAT( NCELL, 104 )      ! MA_RCO3+NO=CCO_O2+HCHO+NO2
     &   +                 RXRAT( NCELL, 106 )      ! MA_RCO3+NO3=CCO_O2+HCHO+NO2
     &   +                 RXRAT( NCELL, 112 )      ! MA_RCO3+RCO_O2=CCO_O2+HCHO+...
     &   +                 RXRAT( NCELL, 113 )      ! MA_RCO3+BZCO_O2=CCO_O2+...
     &   +    2.0000D+00 * RXRAT( NCELL, 114 )      ! MA_RCO3+MA_RCO3=0.2000D+...
     &   +                 RXRAT( NCELL, 130 )      ! CCHO+HO=CCO_O2
     &   +                 RXRAT( NCELL, 132 )      ! CCHO+NO3=CCO_O2+HNO3
     &   +                 RXRAT( NCELL, 136 )      ! ACET+HO=CCO_O2+HCHO+R2O2
     &   +                 RXRAT( NCELL, 137 )      ! ACET=CCO_O2+C_O2
     &   +    4.9200D-01 * RXRAT( NCELL, 138 )      ! MEK+HO=0.4920D+00*CCO_O2+...
     &   +                 RXRAT( NCELL, 139 )      ! MEK=CCO_O2+CCHO+RO2_R
     &   +                 RXRAT( NCELL, 149 )      ! MGLY=CCO_O2+CO+HO2
     &   +                 RXRAT( NCELL, 150 )      ! MGLY+HO=CCO_O2+CO
     &   +                 RXRAT( NCELL, 151 )      ! MGLY+NO3=CCO_O2+CO+HNO3
     &   +    2.0000D+00 * RXRAT( NCELL, 152 )      ! BACL=0.2000D+01*CCO_O2
     &   +    6.7000D-01 * RXRAT( NCELL, 165 )      ! METHACRO=0.6700D+00*CCO_O2+...
     &   +    6.7500D-01 * RXRAT( NCELL, 166 )      ! MVK+HO=0.6750D+00*CCO_O2+...
     &   +    4.6700D-01 * RXRAT( NCELL, 173 )      ! ISOPROD=0.4670D+00*CCO_O2+...
     &   +    2.9000D-02 * RXRAT( NCELL, 174 )      ! PROD2+HO=0.2900D-01*CCO_O2+...
     &   +    6.6700D-01 * RXRAT( NCELL, 175 )      ! PROD2=0.6670D+00*CCO_O2+...
     &   +                 RXRAT( NCELL, 180 )      ! DCB2+HO=CCO_O2+RCHO+R2O2
     &   +    5.0000D-01 * RXRAT( NCELL, 181 )      ! DCB2=0.5000D+00*CCO_O2+...
     &   +                 RXRAT( NCELL, 182 )      ! DCB3+HO=CCO_O2+RCHO+R2O2
     &   +    5.0000D-01 * RXRAT( NCELL, 183 )      ! DCB3=0.5000D+00*CCO_O2+...
     &   +    1.2300D-01 * RXRAT( NCELL, 194 )      ! TRP1+O3=0.1230D+00*CCO_O2+...
     &   +    1.1000D-02 * RXRAT( NCELL, 200 )      ! ALK4+HO=0.1100D-01*CCO_O2+...
     &   +    1.3700D-01 * RXRAT( NCELL, 216 )      ! OLE2+O3=0.1370D+00*CCO_O2+...

c..Loss frequency of CCO_O2 ( not including CCO_O2 + CCO_O2 )
      L8 =                 RKI( NCELL,  69 ) * YC( NCELL, NO2      )   ! CCO_O2+NO2=PAN
     &   +                 RKI( NCELL,  71 ) * YC( NCELL, NO       )   ! CCO_O2+NO=C_O2+NO2
     &   +                 RKI( NCELL,  72 ) * YC( NCELL, HO2      )   ! CCO_O2+HO2=...
     &   +                 RKI( NCELL,  73 ) * YC( NCELL, NO3      )   ! CCO_O2+NO3=C_O2+NO2
     &   +                 RKI( NCELL,  74 ) * YC( NCELL, C_O2     )   ! CCO_O2+C_O2=...
     &   +                 RKI( NCELL,  75 ) * YC( NCELL, RO2_R    )   ! CCO_O2+RO2_R=CCO_OH
     &   +                 RKI( NCELL,  77 ) * YC( NCELL, RO2_N    )   ! CCO_O2+RO2_N=...
     &   +                 RKI( NCELL,  88 ) * YC( NCELL, RCO_O2   )   ! CCO_O2+RCO_O2=...
     &   +                 RKI( NCELL,  99 ) * YC( NCELL, BZCO_O2  )   ! CCO_O2+BZCO_O2=...

c..Loss frequency of PAN
      L9 =                 RKI( NCELL,  70 )                    ! PAN=CCO_O2+NO2

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI( NCELL,  78 ) * DTC

      R8_9  = ( RKI( NCELL,  70 ) ) * DTC 

      R9_8  = ( RKI( NCELL,  69 ) * YC( NCELL, NO2 ) ) * DTC 

c..Solution of quadratic equation to get CCO_O2 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0D0 * K8_8 * CMN
      B = CMN * ( 1.0D0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( NCELL, CCO_O2 ) + P8 * DTC ) +  R8_9 * YC0( NCELL, PAN )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( NCELL, CCO_O2 ) = MAX( Q / A , -C / Q  )

      YCP( NCELL, PAN ) = ( YC0( NCELL, PAN ) +  R9_8 * YCP( NCELL, CCO_O2 ) ) / CMN

      RETURN

      END
