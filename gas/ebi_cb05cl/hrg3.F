
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
C $Header: /project/yoj/arc/CCTM/src/gas/ebi_cb05cl/hrg3.F,v 1.1 2010/07/20 12:33:22 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE HRG3( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of C2O3 and PAN
C
C  PRECONDITIONS: For the CB05CL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jan. 31, 2008
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

      REAL   K8_8         ! Kc2o3+c2o3 * delta t
      REAL   R8_9         ! Kpan-->c2o3 * delta t
      REAL   R9_8         ! Kc2o3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of C2O3 (except from PAN )
      P8 =           RXRAT(  83 )      ! ALD2+O=C2O3+OH
     &    +          RXRAT(  84 )      ! ALD2+OH=C2O3
     &    +          RXRAT(  85 )      ! ALD2+NO3=C2O3+HNO3
     &    +          RXRAT(  95 )      ! PACD+OH=C2O3
     &    +          RXRAT( 135 )      ! OPEN=C2O3+HO2+CO
     &    +          RXRAT( 136 )      ! OPEN+OH=C2O3+2*CO+2*HO2+...
     &    +  0.620 * RXRAT( 137 )      ! OPEN+O3=0.62*C2O3+...
     &    +          RXRAT( 139 )      ! OH+MGLY=C2O3+XO2
     &    +          RXRAT( 140 )      ! MGLY=C2O3+HO2+CO
     &    +  0.210 * RXRAT( 145 )      ! OH+ISPD=0.21*C2O3+...
     &    +  0.114 * RXRAT( 146 )      ! O3+ISPD=0.114*C2O3+...
     &    +  0.967 * RXRAT( 148 )      ! ISPD=0.967*C2O3+0.067*ALD2+...
     &    +          RXRAT( 173 )      ! CL+ALD2=C2O3+HCL

c..Loss frequency of C2O3 ( not including C2O3 + C2O3 )
      L8    =            RKI(  87 ) * YC( NO      )   ! C2O3+NO=MEO2+NO2
     &        +          RKI(  88 ) * YC( NO2     )   ! C2O3+NO2=PAN
     &        +          RKI(  91 ) * YC( HO2     )   ! C2O3+HO2=...
     &        +          RKI(  92 ) * YC( MEO2    )   ! C2O3+MEO2=...
     &        +          RKI(  93 ) * YC( XO2     )   ! C2O3+XO2=...
     &        +          RKI( 111 ) * YC( CXO3    )   ! C2O3+CXO3=MEO2+...

c..Loss frequency of PAN
      L9    =            RKI(  89 )                   ! PAN=C2O3+NO2
     &        +          RKI(  90 )                   ! PAN=C2O3+NO2

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI(  94 ) * DTC

      R8_9  = ( RKI(  89 )
     &      +   RKI(  90 ) ) * DTC 

      R9_8  = ( RKI(  88 ) * YC( NO2 ) ) * DTC 

c..Solution of quadratic equation to get C2O3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0 * K8_8 * CMN
      B = CMN * ( 1.0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( C2O3 ) + P8 * DTC ) +  R8_9 * YC0( PAN )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP( C2O3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) +  R9_8 * YCP( C2O3 ) ) / CMN

      RETURN

      END
