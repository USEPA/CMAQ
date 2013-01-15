
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE HRG3( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of C2O3 and PAN
C
C  PRECONDITIONS: For the SAPRC07TIC family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jan. 9, 2013
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
     &    +          RXRAT( 148 )      ! xMECO3+NO=MECO3+NO
     &    +          RXRAT( 150 )      ! xMECO3+NO3=MECO3+NO3
     &    +  0.500 * RXRAT( 151 )      ! xMECO3+MEO2=0.5*MECO3+MEO2
     &    +  0.500 * RXRAT( 152 )      ! xMECO3+RO2C=0.5*MECO3+RO2C
     &    +  0.500 * RXRAT( 153 )      ! xMECO3+RO2XC=0.5*MECO3+RO2XC
     &    +          RXRAT( 154 )      ! xMECO3+MECO3=MECO3+MECO3
     &    +          RXRAT( 155 )      ! xMECO3+RCO3=MECO3+RCO3
     &    +          RXRAT( 156 )      ! xMECO3+BZCO3=MECO3+BZCO3
     &    +          RXRAT( 157 )      ! xMECO3+MACO3=MECO3+MACO3
     &    +          RXRAT( 202 )      ! CCHO+OH=MECO3
     &    +          RXRAT( 204 )      ! CCHO+NO3=MECO3+HNO3
     &    +  0.620 * RXRAT( 209 )      ! ACETONE=0.62*MECO3+...
     &    +          RXRAT( 211 )      ! MEK=MECO3+RO2C+xHO2+xCCHO+...
     &    +          RXRAT( 228 )      ! MGLY=MECO3+CO+HO2
     &    +          RXRAT( 229 )      ! MGLY+OH=MECO3+CO
     &    +          RXRAT( 230 )      ! MGLY+NO3=MECO3+CO+HNO3
     &    +  2.000 * RXRAT( 231 )      ! BACL=2*MECO3
     &    +  0.305 * RXRAT( 242 )      ! AFG1=0.305*MECO3+...
     &    +  0.013 * RXRAT( 247 )      ! AFG3+O3=0.013*MECO3+...
     &    +  0.467 * RXRAT( 256 )      ! IPRD=0.467*MECO3+1.233*HO2+...
     &    +  0.400 * RXRAT( 258 )      ! PRD2=0.4*MECO3+0.913*xHO2+...
     &    +          RXRAT( 262 )      ! HOCCHO+NO3=MECO3+HNO3
     &    +  0.980 * RXRAT( 268 )      ! CCOOOH+OH=0.98*MECO3+...
     &    +          RXRAT( 576 )      ! CCHO+CL=MECO3+HCL
     &    +          RXRAT( 584 )      ! MGLY+CL=MECO3+CO+HCL
     &    +          RXRAT( 596 )      ! CLACET=MECO3+RO2C+xCL+...
     &    +  0.350 * RXRAT( 696 )      ! HC5+O3=0.35*MECO3+0.35*GLY+...
     &    +  0.625 * RXRAT( 738 )      ! MVKOO+NO=0.625*MECO3+...
     &    +  0.350 * RXRAT( 740 )      ! MVKOO+MEO2=0.35*MECO3+...
     &    +  0.350 * RXRAT( 741 )      ! MVKOO+RO2C=0.35*MECO3+...
     &    +          RXRAT( 761 )      ! HACET=MECO3+HO2+HCHO
     &    +          RXRAT( 766 )      ! PROPNN=MECO3+HCHO+NO2
     &    +          RXRAT( 767 )      ! PROPNNB=MECO3+HCHO+NO2
     &    +          RXRAT( 769 )      ! MVKN=MECO3+NO2+HOCCHO
     &    +  0.340 * RXRAT( 788 )      ! MACR=0.34*MECO3+0.67*HO2+...
     &    +  0.400 * RXRAT( 803 )      ! IMAPAN=0.4*MECO3+0.6*NO2+...
     &    +          RXRAT( 808 )      ! xMECO3+IMACO3=MECO3+IMACO3

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
     &    +          RKI( 678 ) * YC( ISOPO2           )   ! MECO3+ISOPO2=...
     &    +          RKI( 689 ) * YC( NISOPO2          )   ! MECO3+NISOPO2=...
     &    +          RKI( 695 ) * YC( HC5OO            )   ! MECO3+HC5OO=MEO2+...
     &    +          RKI( 702 ) * YC( ISOPNOOD         )   ! MECO3+ISOPNOOD=...
     &    +          RKI( 709 ) * YC( ISOPNOOB         )   ! MECO3+ISOPNOOB=...
     &    +          RKI( 718 ) * YC( NIT1NO3OOA       )   ! MECO3+NIT1NO3OOA=...
     &    +          RKI( 724 ) * YC( NIT1NO3OOB       )   ! MECO3+NIT1NO3OOB=...
     &    +          RKI( 731 ) * YC( NIT1OHOO         )   ! MECO3+NIT1OHOO=...
     &    +          RKI( 736 ) * YC( DIBOO            )   ! MECO3+DIBOO=HO2+...
     &    +  0.300 * RKI( 742 ) * YC( MVKOO            )   ! MECO3+MVKOO=MEO2+...
     &    +          RKI( 747 ) * YC( MACROO           )   ! MECO3+MACROO=...
     &    +          RKI( 754 ) * YC( MACO3            )   ! MECO3+MACO3=CO2+...
     &    +          RKI( 781 ) * YC( IEPOXOO          )   ! MECO3+IEPOXOO=...
     &    +          RKI( 796 ) * YC( IMACO3           )   ! MECO3+IMACO3=CO2+...

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
