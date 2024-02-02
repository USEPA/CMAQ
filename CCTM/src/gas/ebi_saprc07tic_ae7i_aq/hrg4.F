
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
C  PRECONDITIONS: For the SAPRC07TIC_AE7I_AQ mechanism
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
     &    +                 RKI(   269 ) * YC ( HCOCO3       ) * YCP( NO2          )      ! HCOCO3+NO2=NO3+CO+CO2+HO2
     &    +                 RKI(   601 ) * YC ( CLONO2       )                            ! CLONO2=NO3+CL
     &    +                 RKI(   603 ) * YC ( CL           ) * YC ( CLONO2       )      ! CL+CLONO2=NO3+CL2
     &    +    3.1000D-01 * RKI(   735 ) * YC ( ISOPNOOD     ) * YC ( NO           )      ! ISOPNOOD+NO=0.3100D+00*NO3+...
     &    +    1.5500D-01 * RKI(   737 ) * YC ( ISOPNOOD     ) * YC ( MEO2         )      ! ISOPNOOD+MEO2=0.1550D+...
     &    +    1.5500D-01 * RKI(   738 ) * YC ( ISOPNOOD     ) * YC ( RO2C         )      ! ISOPNOOD+RO2C=0.1550D+...
     &    +    3.1000D-01 * RKI(   739 ) * YC ( ISOPNOOD     ) * YC ( MECO3        )      ! ISOPNOOD+MECO3=0.3100D+...
     &    +                 RKI(   805 ) * YC ( MVKN         ) * YCP( OH           )      ! MVKN+OH=NO3+0.6500D+...
     &    +    1.5000D-01 * RKI(   807 ) * YC ( MACRN        ) * YCP( OH           )      ! MACRN+OH=0.1500D+00*NO3+...
     &    +    4.0000D-01 * RKI(   841 ) * YC ( IMAPAN       )                            ! IMAPAN=0.4000D+00*NO3+...
     &    +    8.1000D-01 * RKI(   877 ) * YC ( IMAPAN       ) * YCP( OH           )      ! IMAPAN+OH=0.8100D+00*NO3+...

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
     &    +                 RKI(   197 ) * YC ( HCHO         )   ! NO3+HCHO=HNO3+HO2+CO
     &    +                 RKI(   200 ) * YC ( CCHO         )   ! NO3+CCHO=HNO3+MECO3
     &    +                 RKI(   203 ) * YC ( RCHO         )   ! NO3+RCHO=HNO3+RCO3
     &    +                 RKI(   223 ) * YC ( GLY          )   ! NO3+GLY=HNO3+...
     &    +                 RKI(   226 ) * YC ( MGLY         )   ! NO3+MGLY=HNO3+CO+...
     &    +                 RKI(   229 ) * YC ( CRES         )   ! NO3+CRES=HNO3+BZO
     &    +                 RKI(   235 ) * YC ( BALD         )   ! NO3+BALD=HNO3+BZCO3
     &    +                 RKI(   251 ) * YC ( IPRD         )   ! NO3+IPRD=0.1500D+...
     &    +                 RKI(   258 ) * YC ( HOCCHO       )   ! NO3+HOCCHO=HNO3+...
     &    +                 RKI(   261 ) * YC ( ACROLEIN     )   ! NO3+ACROLEIN=...
     &    +                 RKI(   526 ) * YC ( ETHENE       )   ! NO3+ETHENE=xHO2+...
     &    +                 RKI(   530 ) * YC ( PROPENE      )   ! NO3+PROPENE=...
     &    +                 RKI(   534 ) * YC ( BUTADIENE13  )   ! NO3+BUTADIENE13=...
     &    +                 RKI(   540 ) * YC ( APIN         )   ! NO3+APIN=0.5600D-...
     &    +                 RKI(   559 ) * YC ( OLE1         )   ! NO3+OLE1=0.7720D+...
     &    +                 RKI(   563 ) * YC ( OLE2         )   ! NO3+OLE2=0.4000D+...
     &    +                 RKI(   570 ) * YC ( TERP         )   ! NO3+TERP=TERPNRO2
     &    +                 RKI(   573 ) * YC ( TERPNRO2     )   ! NO3+TERPNRO2=...
     &    +                 RKI(   585 ) * YC ( SESQ         )   ! NO3+SESQ=0.2270D+...
     &    +                 RKI(   597 ) * YC ( CL           )   ! NO3+CL=CLO+NO2
     &    +                 RKI(   719 ) * YC ( ISOPRENE     )   ! NO3+ISOPRENE=NISOPO2
     &    +                 RKI(   720 ) * YC ( NISOPO2      )   ! NO3+NISOPO2=...
     &    +                 RKI(   748 ) * YC ( NIT1         )   ! NO3+NIT1=0.6000D+...
     &    +                 RKI(   749 ) * YC ( NIT1NO3OOA   )   ! NO3+NIT1NO3OOA=...
     &    +                 RKI(   756 ) * YC ( NIT1NO3OOB   )   ! NO3+NIT1NO3OOB=...
     &    +                 RKI(   787 ) * YC ( MACO3        )   ! NO3+MACO3=NO2+CO+...
     &    +                 RKI(   824 ) * YC ( MACR         )   ! NO3+MACR=0.5000D+...
     &    +                 RKI(   829 ) * YC ( IMACO3       )   ! NO3+IMACO3=NO2+...
     &    +                 RKI(   904 )                         ! NO3=HNO3

c..Loss frequency of N2O5
      L16 =                 RKI(    12 )                         ! N2O5=NO2+NO3
     &    +                 RKI(    13 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(    14 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(   885 )                         ! N2O5=HNO3+H2NO3PIJ
     &    +                 RKI(   886 )                         ! N2O5=HNO3+H2NO3PK

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
