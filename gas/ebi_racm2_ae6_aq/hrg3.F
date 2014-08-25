
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

      REAL( 8 ) ::   K8_8         ! Kaco3+aco3 * delta t
      REAL( 8 ) ::   R8_9         ! Kpan-->aco3 * delta t
      REAL( 8 ) ::   R9_8         ! Kaco3+no2-->pan * [NO2] * delta t

C**********************************************************************


c..Production of ACO3 (except from PAN )
      P8 =                 RXRAT( NCELL,  14 )      ! ACT=ACO3+MO2
     &   +    7.8400D-01 * RXRAT( NCELL,  15 )      ! UALD=0.7840D+00*ACO3+...
     &   +                 RXRAT( NCELL,  16 )      ! MEK=ACO3+0.5000D+00*ETHP+...
     &   +                 RXRAT( NCELL,  17 )      ! KET=ACO3+ETHP
     &   +                 RXRAT( NCELL,  18 )      ! HKET=ACO3+HO2+HCHO
     &   +    6.7000D-01 * RXRAT( NCELL,  19 )      ! MACR=0.6700D+00*ACO3+...
     &   +                 RXRAT( NCELL,  24 )      ! MGLY=ACO3+HO2+CO
     &   +    2.5000D-01 * RXRAT( NCELL,  25 )      ! DCB1=0.2500D+00*ACO3+...
     &   +    2.5000D-01 * RXRAT( NCELL,  26 )      ! DCB2=0.2500D+00*ACO3+...
     &   +                 RXRAT( NCELL,  90 )      ! ACD+HO=ACO3
     &   +    3.1300D-01 * RXRAT( NCELL,  98 )      ! UALD+HO=0.3130D+00*ACO3+...
     &   +                 RXRAT( NCELL, 100 )      ! MGLY+HO=ACO3+CO
     &   +    6.5000D-01 * RXRAT( NCELL, 119 )      ! PAA+HO=0.6500D+00*ACO3+...
     &   +    9.0000D-02 * RXRAT( NCELL, 128 )      ! OLI+O3=0.9000D-01*ACO3+...
     &   +    1.5000D-01 * RXRAT( NCELL, 129 )      ! DIEN+O3=0.1500D+00*ACO3+...
     &   +    1.0000D-01 * RXRAT( NCELL, 130 )      ! ISO+O3=0.1000D+00*ACO3+...
     &   +    1.0000D-01 * RXRAT( NCELL, 133 )      ! MACR+O3=0.1000D+00*ACO3+...
     &   +    2.8000D-01 * RXRAT( NCELL, 134 )      ! MVK+O3=0.2800D+00*ACO3+...
     &   +    2.0000D-03 * RXRAT( NCELL, 135 )      ! UALD+O3=0.2000D-02*ACO3+...
     &   +                 RXRAT( NCELL, 149 )      ! ACD+NO3=ACO3+HNO3
     &   +                 RXRAT( NCELL, 154 )      ! MGLY+NO3=ACO3+CO+HNO3
     &   +                 RXRAT( NCELL, 193 )      ! ACTP+NO=ACO3+NO2+HCHO
     &   +    2.3000D-01 * RXRAT( NCELL, 195 )      ! KETP+NO=0.2300D+00*ACO3+...
     &   +    3.5000D-01 * RXRAT( NCELL, 196 )      ! MACP+NO=0.3500D+00*ACO3+...
     &   +    7.0000D-01 * RXRAT( NCELL, 198 )      ! MVKP+NO=0.7000D+00*ACO3+...
     &   +    1.5000D-01 * RXRAT( NCELL, 233 )      ! ACTP+HO2=0.1500D+00*ACO3+...
     &   +    5.0000D-01 * RXRAT( NCELL, 269 )      ! ACTP+MO2=0.5000D+00*ACO3+...
     &   +    2.6900D-01 * RXRAT( NCELL, 272 )      ! MACP+MO2=0.2690D+00*ACO3+...
     &   +    1.1600D+00 * RXRAT( NCELL, 274 )      ! MVKP+MO2=0.1160D+01*ACO3+...
     &   +    1.6000D-01 * RXRAT( NCELL, 310 )      ! MVKP+ACO3=0.1160D+01*ACO3+...
     &   +                 RXRAT( NCELL, 343 )      ! ACTP+NO3=ACO3+NO2+HCHO
     &   +    5.3800D-01 * RXRAT( NCELL, 346 )      ! MACP+NO3=0.5380D+00*ACO3+...
     &   +    7.0000D-01 * RXRAT( NCELL, 348 )      ! MVKP+NO3=0.7000D+00*ACO3+...

c..Loss frequency of ACO3 ( not including ACO3 + ACO3 )
      L8 =                 RKI( NCELL, 166 ) * YC( NCELL, NO2     )   ! ACO3+NO2=PAN
     &   +                 RKI( NCELL, 191 ) * YC( NCELL, NO      )   ! ACO3+NO=MO2+NO2
     &   +                 RKI( NCELL, 231 ) * YC( NCELL, HO2     )   ! ACO3+HO2=0.4400D+...
     &   +                 RKI( NCELL, 267 ) * YC( NCELL, MO2     )   ! ACO3+MO2=0.9000D+...
     &   +                 RKI( NCELL, 285 ) * YC( NCELL, ETHP    )   ! ACO3+ETHP=...
     &   +                 RKI( NCELL, 286 ) * YC( NCELL, HC3P    )   ! ACO3+HC3P=...
     &   +                 RKI( NCELL, 287 ) * YC( NCELL, HC5P    )   ! ACO3+HC5P=...
     &   +                 RKI( NCELL, 288 ) * YC( NCELL, HC8P    )   ! ACO3+HC8P=...
     &   +                 RKI( NCELL, 289 ) * YC( NCELL, ETEP    )   ! ACO3+ETEP=...
     &   +                 RKI( NCELL, 290 ) * YC( NCELL, OLTP    )   ! ACO3+OLTP=...
     &   +                 RKI( NCELL, 291 ) * YC( NCELL, OLIP    )   ! ACO3+OLIP=...
     &   +                 RKI( NCELL, 292 ) * YC( NCELL, BENP    )   ! ACO3+BENP=...
     &   +                 RKI( NCELL, 293 ) * YC( NCELL, TLP1    )   ! ACO3+TLP1=MO2+BALD
     &   +                 RKI( NCELL, 294 ) * YC( NCELL, TOLP    )   ! ACO3+TOLP=HO2+...
     &   +                 RKI( NCELL, 295 ) * YC( NCELL, PER1    )   ! ACO3+PER1=HO2+...
     &   +                 RKI( NCELL, 296 ) * YC( NCELL, XYL1    )   ! ACO3+XYL1=MO2+BALD
     &   +                 RKI( NCELL, 297 ) * YC( NCELL, XYLP    )   ! ACO3+XYLP=HO2+...
     &   +                 RKI( NCELL, 298 ) * YC( NCELL, PER2    )   ! ACO3+PER2=HO2+...
     &   +                 RKI( NCELL, 299 ) * YC( NCELL, XYOP    )   ! ACO3+XYOP=HO2+...
     &   +                 RKI( NCELL, 300 ) * YC( NCELL, ISOP    )   ! ACO3+ISOP=...
     &   +                 RKI( NCELL, 301 ) * YC( NCELL, APIP    )   ! ACO3+APIP=...
     &   +                 RKI( NCELL, 302 ) * YC( NCELL, LIMP    )   ! ACO3+LIMP=...
     &   +                 RKI( NCELL, 304 ) * YC( NCELL, RCO3    )   ! ACO3+RCO3=MO2+ETHP
     &   +    5.0000D-01 * RKI( NCELL, 305 ) * YC( NCELL, ACTP    )   ! ACO3+ACTP=...
     &   +                 RKI( NCELL, 306 ) * YC( NCELL, MEKP    )   ! ACO3+MEKP=...
     &   +                 RKI( NCELL, 307 ) * YC( NCELL, KETP    )   ! ACO3+KETP=...
     &   +    7.3100D-01 * RKI( NCELL, 308 ) * YC( NCELL, MACP    )   ! ACO3+MACP=...
     &   +                 RKI( NCELL, 309 ) * YC( NCELL, MCP     )   ! ACO3+MCP=NO2+...
     &   +                 RKI( NCELL, 311 ) * YC( NCELL, UALP    )   ! ACO3+UALP=...
     &   +                 RKI( NCELL, 312 ) * YC( NCELL, BALP    )   ! ACO3+BALP=MO2+BAL1
     &   +                 RKI( NCELL, 313 ) * YC( NCELL, BAL1    )   ! ACO3+BAL1=MO2+BAL2
     &   +                 RKI( NCELL, 314 ) * YC( NCELL, ADDC    )   ! ACO3+ADDC=...
     &   +                 RKI( NCELL, 315 ) * YC( NCELL, MCTP    )   ! ACO3+MCTP=HO2+...
     &   +                 RKI( NCELL, 316 ) * YC( NCELL, ORAP    )   ! ACO3+ORAP=MO2+GLY
     &   +                 RKI( NCELL, 317 ) * YC( NCELL, OLNN    )   ! ACO3+OLNN=HO2+...
     &   +                 RKI( NCELL, 318 ) * YC( NCELL, OLND    )   ! ACO3+OLND=...
     &   +                 RKI( NCELL, 319 ) * YC( NCELL, ADCN    )   ! ACO3+ADCN=HO2+...
     &   +                 RKI( NCELL, 320 ) * YC( NCELL, XO2     )   ! ACO3+XO2=MO2
     &   +                 RKI( NCELL, 341 ) * YC( NCELL, NO3     )   ! ACO3+NO3=MO2+NO2

c..Loss frequency of PAN
      L9 =                 RKI( NCELL,  32 )                   ! PAN=ACO3+NO2
     &   +                 RKI( NCELL,  33 )                   ! PAN=MO2+NO3
     &   +                 RKI( NCELL, 120 ) * YC( NCELL, HO      )   ! PAN+HO=XO2+NO3+HCHO
     &   +                 RKI( NCELL, 167 )                   ! PAN=ACO3+NO2

c..K8_8, R8_9, and R9_8 terms
      K8_8  = RKI( NCELL, 303 ) * DTC

      R8_9  = ( RKI( NCELL,  32 )
     &      +   RKI( NCELL, 167 ) ) * DTC 

      R9_8  = ( RKI( NCELL, 166 ) * YC( NCELL, NO2 ) ) * DTC 

c..Solution of quadratic equation to get ACO3 & PAN
      CMN = 1.0 + L9 * DTC
      A = 2.0D0 * K8_8 * CMN
      B = CMN * ( 1.0D0 + L8 * DTC ) - R8_9 * R9_8
      C = CMN * ( YC0( NCELL, ACO3 ) + P8 * DTC ) +  R8_9 * YC0( NCELL, PAN )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )

      YCP( NCELL, ACO3 ) = MAX( Q / A , -C / Q  )

      YCP( NCELL, PAN ) = ( YC0( NCELL, PAN ) +  R9_8 * YCP( NCELL, ACO3 ) ) / CMN

      RETURN

      END
