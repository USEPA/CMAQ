
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
C  PRECONDITIONS: For the RACM2 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, March 4, 2013
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

      REAL   K8_8         ! Kaco3+aco3 * delta t
      REAL   R8_9         ! Kpan-->aco3 * delta t
      REAL   R9_8         ! Kaco3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of ACO3 (except from PAN )
      P8 =           RXRAT(  14 )      ! ACT=ACO3+MO2
     &    +  0.784 * RXRAT(  15 )      ! UALD=0.784*ACO3+1.22*HO2+...
     &    +          RXRAT(  16 )      ! MEK=ACO3+0.5*ETHP+0.5*MO2
     &    +          RXRAT(  17 )      ! KET=ACO3+ETHP
     &    +          RXRAT(  18 )      ! HKET=ACO3+HO2+HCHO
     &    +  0.670 * RXRAT(  19 )      ! MACR=0.67*ACO3+0.66*HO2+...
     &    +          RXRAT(  24 )      ! MGLY=ACO3+HO2+CO
     &    +  0.250 * RXRAT(  25 )      ! DCB1=0.25*ACO3+1.5*HO2+...
     &    +  0.250 * RXRAT(  26 )      ! DCB2=0.25*ACO3+1.5*HO2+...
     &    +          RXRAT(  90 )      ! ACD+HO=ACO3
     &    +  0.313 * RXRAT(  98 )      ! UALD+HO=0.313*ACO3+0.687*UALP
     &    +          RXRAT( 100 )      ! MGLY+HO=ACO3+CO
     &    +  0.650 * RXRAT( 119 )      ! PAA+HO=0.65*ACO3+0.35*HO+...
     &    +  0.090 * RXRAT( 128 )      ! OLI+O3=0.09*ACO3+0.07*HO2+...
     &    +  0.150 * RXRAT( 129 )      ! DIEN+O3=0.15*ACO3+0.28*HO+...
     &    +  0.100 * RXRAT( 130 )      ! ISO+O3=0.1*ACO3+0.25*HO2+...
     &    +  0.100 * RXRAT( 133 )      ! MACR+O3=0.1*ACO3+0.14*HO2+...
     &    +  0.280 * RXRAT( 134 )      ! MVK+O3=0.28*ACO3+0.11*HO2+...
     &    +  0.002 * RXRAT( 135 )      ! UALD+O3=0.002*ACO3+...
     &    +          RXRAT( 149 )      ! ACD+NO3=ACO3+HNO3
     &    +          RXRAT( 154 )      ! MGLY+NO3=ACO3+CO+HNO3
     &    +          RXRAT( 193 )      ! ACTP+NO=ACO3+NO2+HCHO
     &    +  0.230 * RXRAT( 195 )      ! KETP+NO=0.23*ACO3+0.77*HO2+...
     &    +  0.350 * RXRAT( 196 )      ! MACP+NO=0.35*ACO3+0.65*MO2+...
     &    +  0.700 * RXRAT( 198 )      ! MVKP+NO=0.7*ACO3+0.3*HO2+...
     &    +  0.150 * RXRAT( 233 )      ! ACTP+HO2=0.15*ACO3+0.15*HO+...
     &    +  0.500 * RXRAT( 269 )      ! ACTP+MO2=0.5*ACO3+0.5*HO2+...
     &    +  0.269 * RXRAT( 272 )      ! MACP+MO2=0.269*ACO3+...
     &    +  1.160 * RXRAT( 274 )      ! MVKP+MO2=1.16*ACO3+HO2+...
     &    +  0.160 * RXRAT( 310 )      ! MVKP+ACO3=1.16*ACO3+...
     &    +          RXRAT( 343 )      ! ACTP+NO3=ACO3+NO2+HCHO
     &    +  0.538 * RXRAT( 346 )      ! MACP+NO3=0.538*ACO3+HCHO+...
     &    +  0.700 * RXRAT( 348 )      ! MVKP+NO3=0.7*ACO3+0.3*HO2+...

c..Loss frequency of ACO3 ( not including ACO3 + ACO3 )
      L8 =           RKI( 166 ) * YC( NO2     )   ! ACO3+NO2=PAN
     &    +          RKI( 191 ) * YC( NO      )   ! ACO3+NO=MO2+NO2
     &    +          RKI( 231 ) * YC( HO2     )   ! ACO3+HO2=0.44*HO+...
     &    +          RKI( 267 ) * YC( MO2     )   ! ACO3+MO2=0.9*HO2+...
     &    +          RKI( 285 ) * YC( ETHP    )   ! ACO3+ETHP=...
     &    +          RKI( 286 ) * YC( HC3P    )   ! ACO3+HC3P=...
     &    +          RKI( 287 ) * YC( HC5P    )   ! ACO3+HC5P=...
     &    +          RKI( 288 ) * YC( HC8P    )   ! ACO3+HC8P=...
     &    +          RKI( 289 ) * YC( ETEP    )   ! ACO3+ETEP=...
     &    +          RKI( 290 ) * YC( OLTP    )   ! ACO3+OLTP=...
     &    +          RKI( 291 ) * YC( OLIP    )   ! ACO3+OLIP=...
     &    +          RKI( 292 ) * YC( BENP    )   ! ACO3+BENP=...
     &    +          RKI( 293 ) * YC( TLP1    )   ! ACO3+TLP1=MO2+BALD
     &    +          RKI( 294 ) * YC( TOLP    )   ! ACO3+TOLP=HO2+...
     &    +          RKI( 295 ) * YC( PER1    )   ! ACO3+PER1=HO2+...
     &    +          RKI( 296 ) * YC( XYL1    )   ! ACO3+XYL1=MO2+BALD
     &    +          RKI( 297 ) * YC( XYLP    )   ! ACO3+XYLP=HO2+...
     &    +          RKI( 298 ) * YC( PER2    )   ! ACO3+PER2=HO2+...
     &    +          RKI( 299 ) * YC( XYOP    )   ! ACO3+XYOP=HO2+...
     &    +          RKI( 300 ) * YC( ISOP    )   ! ACO3+ISOP=...
     &    +          RKI( 301 ) * YC( APIP    )   ! ACO3+APIP=...
     &    +          RKI( 302 ) * YC( LIMP    )   ! ACO3+LIMP=...
     &    +          RKI( 304 ) * YC( RCO3    )   ! ACO3+RCO3=MO2+ETHP
     &    +  0.500 * RKI( 305 ) * YC( ACTP    )   ! ACO3+ACTP=...
     &    +          RKI( 306 ) * YC( MEKP    )   ! ACO3+MEKP=...
     &    +          RKI( 307 ) * YC( KETP    )   ! ACO3+KETP=...
     &    +  0.731 * RKI( 308 ) * YC( MACP    )   ! ACO3+MACP=...
     &    +          RKI( 309 ) * YC( MCP     )   ! ACO3+MCP=NO2+...
     &    +          RKI( 311 ) * YC( UALP    )   ! ACO3+UALP=...
     &    +          RKI( 312 ) * YC( BALP    )   ! ACO3+BALP=MO2+BAL1
     &    +          RKI( 313 ) * YC( BAL1    )   ! ACO3+BAL1=MO2+BAL2
     &    +          RKI( 314 ) * YC( ADDC    )   ! ACO3+ADDC=2*HO2+...
     &    +          RKI( 315 ) * YC( MCTP    )   ! ACO3+MCTP=HO2+...
     &    +          RKI( 316 ) * YC( ORAP    )   ! ACO3+ORAP=MO2+GLY
     &    +          RKI( 317 ) * YC( OLNN    )   ! ACO3+OLNN=HO2+...
     &    +          RKI( 318 ) * YC( OLND    )   ! ACO3+OLND=...
     &    +          RKI( 319 ) * YC( ADCN    )   ! ACO3+ADCN=HO2+...
     &    +          RKI( 320 ) * YC( XO2     )   ! ACO3+XO2=MO2
     &    +          RKI( 341 ) * YC( NO3     )   ! ACO3+NO3=MO2+NO2

c..Loss frequency of PAN
      L9 =           RKI(  32 )                   ! PAN=ACO3+NO2
     &    +          RKI(  33 )                   ! PAN=MO2+NO3
     &    +          RKI( 120 ) * YC( HO      )   ! PAN+HO=XO2+NO3+HCHO
     &    +          RKI( 167 )                   ! PAN=ACO3+NO2

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI( 303 ) * DTC

      R8_9  = ( RKI(  32 )
     &      +   RKI( 167 ) ) * DTC 

      R9_8  = ( RKI( 166 ) * YC( NO2 ) ) * DTC 

c..Solution of quadratic equation to get ACO3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0 * K8_8 * CMN
      B = CMN * ( 1.0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( ACO3 ) + P8 * DTC ) +  R8_9 * YC0( PAN )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP( ACO3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) +  R9_8 * YCP( ACO3 ) ) / CMN

      RETURN

      END
