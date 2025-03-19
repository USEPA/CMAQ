
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

       SUBROUTINE HRG4( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of NO3 and N2O5
C
C  PRECONDITIONS: For the CB6R3_AE7_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: NONE


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC             ! Time step


C..PARAMETERS: NONE


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
!     CHARACTER( 16 ), SAVE   ::  PNAME = 'HRG4'    ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) ::   A, B, C, Q   ! Quadratic equation terms
      REAL( 8 ) ::   CMN          ! Temp scalar
      REAL( 8 ) ::   L15          ! Loss of NO3
      REAL( 8 ) ::   L16          ! Loss of N2O5
      REAL( 8 ) ::   P15          ! Production of NO3
      REAL( 8 ) ::   K15_15       ! Kno3+no3 * delta t
      REAL( 8 ) ::   R15_16       ! Kn2o5-->no3 * delta t
      REAL( 8 ) ::   R16_15       ! Kno3+no2-->n2o5[NO2] * delta t


C**********************************************************************


c..Production of NO3 (except from N2O5 )
      P15 =                 RKI(     6 ) * YCP( O            ) * YCP( NO2          )      ! O+NO2=NO3
     &    +                 RKI(    26 ) * YCP( NO2          ) * YCP( O3           )      ! NO2+O3=NO3
     &    +                 RKI(    46 ) * YC ( HNO3         ) * YCP( OH           )      ! HNO3+OH=NO3
     &    +    4.1000D-01 * RKI(    50 ) * YC ( PNA          )                            ! PNA=0.4100D+00*NO3+0.5900D+...
     &    +    4.0000D-01 * RKI(    56 ) * YC ( PAN          )                            ! PAN=0.4000D+00*NO3+0.6000D+...
     &    +    4.0000D-01 * RKI(    64 ) * YC ( PANX         )                            ! PANX=0.4000D+00*NO3+...
     &    +    1.8500D-01 * RKI(   170 ) * YC ( INTR         ) * YCP( OH           )      ! INTR+OH=0.1850D+00*NO3+...
     &    +                 RKI(   254 ) * YC ( CLNO3        )                            ! CLNO3=NO3+CL

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI(    27 )                         ! NO3=NO2+O
     &    +                 RKI(    28 )                         ! NO3=NO
     &    +                 RKI(    29 ) * YCP( NO           )   ! NO3+NO=0.2000D+...
     &    +                 RKI(    30 ) * YCP( NO2          )   ! NO3+NO2=NO+NO2
     &    +                 RKI(    31 ) * YCP( O            )   ! NO3+O=NO2
     &    +                 RKI(    32 ) * YCP( OH           )   ! NO3+OH=HO2+NO2
     &    +                 RKI(    33 ) * YCP( HO2          )   ! NO3+HO2=OH+NO2
     &    +                 RKI(    34 ) * YCP( O3           )   ! NO3+O3=NO2
     &    +                 RKI(    36 ) * YCP( NO2          )   ! NO3+NO2=N2O5
     &    +                 RKI(   100 ) * YC ( FORM         )   ! NO3+FORM=HNO3+HO2+CO
     &    +                 RKI(   107 ) * YC ( ALD2         )   ! NO3+ALD2=C2O3+HNO3
     &    +                 RKI(   111 ) * YC ( ALDX         )   ! NO3+ALDX=CXO3+HNO3
     &    +                 RKI(   115 ) * YC ( GLYD         )   ! NO3+GLYD=HNO3+C2O3
     &    +                 RKI(   118 ) * YC ( GLY          )   ! NO3+GLY=HNO3+...
     &    +                 RKI(   120 ) * YC ( MGLY         )   ! NO3+MGLY=HNO3+...
     &    +                 RKI(   140 ) * YC ( ETH          )   ! NO3+ETH=0.5000D+...
     &    +                 RKI(   144 ) * YC ( OLE          )   ! NO3+OLE=0.5000D+...
     &    +                 RKI(   148 ) * YC ( IOLE         )   ! NO3+IOLE=0.5000D+...
     &    +                 RKI(   157 ) * YC ( ISOP         )   ! NO3+ISOP=0.3500D+...
     &    +                 RKI(   160 ) * YC ( ISPD         )   ! NO3+ISPD=0.7170D+...
     &    +                 RKI(   164 ) * YC ( HPLD         )   ! NO3+HPLD=HNO3+ISPD
     &    +                 RKI(   174 ) * YC ( TERP         )   ! NO3+TERP=0.4700D+...
     &    +                 RKI(   178 ) * YC ( APIN         )   ! NO3+APIN=0.4700D+...
     &    +                 RKI(   196 ) * YC ( CRES         )   ! NO3+CRES=0.3000D+...
     &    +                 RKI(   200 ) * YC ( CRON         )   ! NO3+CRON=NTR2+...
     &    +                 RKI(   205 ) * YC ( XOPN         )   ! NO3+XOPN=0.5000D+...
     &    +                 RKI(   209 ) * YC ( OPEN         )   ! NO3+OPEN=OPO3+HNO3
     &    +                 RKI(   211 ) * YC ( CAT1         )   ! NO3+CAT1=CRO+HNO3

c..Loss frequency of N2O5
      L16 =                 RKI(    37 )                         ! N2O5=NO3+NO2
     &    +                 RKI(    38 )                         ! N2O5=NO2+NO3
     &    +                 RKI(    39 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(   269 )                         ! N2O5=HNO3+H2NO3PIJ
     &    +                 RKI(   270 )                         ! N2O5=HNO3+H2NO3PK

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI(    35 ) * DTC

      R15_16  = ( RKI(    37 )
     &        +   RKI(    38 ) ) * DTC 


      R16_15  = RKI(    36 ) * YCP( NO2 ) * DTC

c..Solution of quadratic equation to get NO3 & N2O5
      CMN = 1.0D0 + L16 * DTC
      A = 2.0D0 * K15_15 * CMN
      B = CMN * ( 1.0D0 + L15 * DTC ) - R15_16 * R16_15
      C = CMN * ( YC0( NO3 ) + P15 * DTC ) +  R15_16 * YC0( N2O5 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )
      YCP( NO3 ) = MAX( Q / A , -C / Q  )
      YCP( N2O5 ) = ( YC0( N2O5 ) + R16_15 * YCP( NO3 ) ) / CMN

      RETURN

      END
