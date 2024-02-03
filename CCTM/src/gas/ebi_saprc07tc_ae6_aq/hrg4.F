
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
C  PRECONDITIONS: For the SAPRC07TC_AE6_AQ mechanism
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
      P15 =                 RKI(     6 ) * YCP( O3P          ) * YCP( NO2          )      ! O3P+NO2=NO3
     &    +                 RKI(     8 ) * YCP( O3           ) * YCP( NO2          )      ! O3+NO2=NO3
     &    +                 RKI(    27 ) * YCP( OH           ) * YC ( HNO3         )      ! OH+HNO3=NO3
     &    +    3.9000D-01 * RKI(    34 ) * YC ( HNO4         )                            ! HNO4=0.3900D+00*NO3+...
     &    +    4.0000D-01 * RKI(    65 ) * YC ( PAN          )                            ! PAN=0.4000D+00*NO3+0.6000D+...
     &    +    4.0000D-01 * RKI(    75 ) * YC ( PAN2         )                            ! PAN2=0.4000D+00*NO3+...
     &    +    4.0000D-01 * RKI(    86 ) * YC ( PBZN         )                            ! PBZN=0.4000D+00*NO3+...
     &    +    4.0000D-01 * RKI(    98 ) * YC ( MAPAN        )                            ! MAPAN=0.4000D+00*NO3+...
     &    +                 RKI(   284 ) * YC ( HCOCO3       ) * YCP( NO2          )      ! HCOCO3+NO2=NO3+CO+CO2+HO2
     &    +                 RKI(   604 ) * YC ( CLONO2       )                            ! CLONO2=NO3+CL
     &    +                 RKI(   606 ) * YC ( CL           ) * YC ( CLONO2       )      ! CL+CLONO2=NO3+CL2

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI(     9 ) * YCP( NO           )   ! NO3+NO=0.2000D+...
     &    +                 RKI(    11 ) * YCP( NO2          )   ! NO3+NO2=N2O5
     &    +                 RKI(    15 ) * YCP( NO2          )   ! NO3+NO2=NO+NO2
     &    +                 RKI(    16 )                         ! NO3=NO
     &    +                 RKI(    17 )                         ! NO3=NO2+O3P
     &    +                 RKI(    26 ) * YCP( OH           )   ! NO3+OH=HO2+NO2
     &    +                 RKI(    39 ) * YCP( HO2          )   ! NO3+HO2=0.8000D+...
     &    +                 RKI(    49 ) * YC ( MEO2         )   ! NO3+MEO2=HCHO+...
     &    +                 RKI(    54 ) * YC ( RO2C         )   ! NO3+RO2C=NO2
     &    +                 RKI(    59 ) * YC ( RO2XC        )   ! NO3+RO2XC=NO2
     &    +                 RKI(    68 ) * YC ( MECO3        )   ! NO3+MECO3=MEO2+...
     &    +                 RKI(    78 ) * YC ( RCO3         )   ! NO3+RCO3=NO2+...
     &    +                 RKI(    89 ) * YC ( BZCO3        )   ! NO3+BZCO3=NO2+...
     &    +                 RKI(   101 ) * YC ( MACO3        )   ! NO3+MACO3=NO2+...
     &    +                 RKI(   207 ) * YC ( HCHO         )   ! NO3+HCHO=HNO3+HO2+CO
     &    +                 RKI(   210 ) * YC ( CCHO         )   ! NO3+CCHO=HNO3+MECO3
     &    +                 RKI(   213 ) * YC ( RCHO         )   ! NO3+RCHO=HNO3+RCO3
     &    +                 RKI(   233 ) * YC ( GLY          )   ! NO3+GLY=HNO3+...
     &    +                 RKI(   236 ) * YC ( MGLY         )   ! NO3+MGLY=HNO3+CO+...
     &    +                 RKI(   239 ) * YC ( CRES         )   ! NO3+CRES=HNO3+BZO
     &    +                 RKI(   245 ) * YC ( BALD         )   ! NO3+BALD=HNO3+BZCO3
     &    +                 RKI(   256 ) * YC ( MACR         )   ! NO3+MACR=0.5000D+...
     &    +                 RKI(   265 ) * YC ( IPRD         )   ! NO3+IPRD=0.1500D+...
     &    +                 RKI(   273 ) * YC ( HOCCHO       )   ! NO3+HOCCHO=HNO3+...
     &    +                 RKI(   276 ) * YC ( ACROLEIN     )   ! NO3+ACROLEIN=...
     &    +                 RKI(   529 ) * YC ( ETHENE       )   ! NO3+ETHENE=xHO2+...
     &    +                 RKI(   533 ) * YC ( PROPENE      )   ! NO3+PROPENE=...
     &    +                 RKI(   537 ) * YC ( BUTADIENE13  )   ! NO3+BUTADIENE13=...
     &    +                 RKI(   541 ) * YC ( ISOPRENE     )   ! NO3+ISOPRENE=...
     &    +                 RKI(   554 ) * YC ( APIN         )   ! NO3+APIN=0.5600D-...
     &    +                 RKI(   573 ) * YC ( OLE1         )   ! NO3+OLE1=0.7720D+...
     &    +                 RKI(   577 ) * YC ( OLE2         )   ! NO3+OLE2=0.4000D+...
     &    +                 RKI(   584 ) * YC ( TERP         )   ! NO3+TERP=0.2270D+...
     &    +                 RKI(   588 ) * YC ( SESQ         )   ! NO3+SESQ=0.2270D+...
     &    +                 RKI(   600 ) * YC ( CL           )   ! NO3+CL=CLO+NO2

c..Loss frequency of N2O5
      L16 =                 RKI(    12 )                         ! N2O5=NO2+NO3
     &    +                 RKI(    13 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(    14 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(   714 )                         ! N2O5=HNO3+H2NO3PIJ
     &    +                 RKI(   715 )                         ! N2O5=HNO3+H2NO3PK

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI(    40 ) * DTC

      R15_16  = ( RKI(    12 ) ) * DTC 


      R16_15  = RKI(    11 ) * YCP( NO2 ) * DTC

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
