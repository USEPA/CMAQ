
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
C  PRECONDITIONS: For the SAPRC22_AE65_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Dec 19, 2024
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
     &    +    2.0000D-01 * RKI(    35 ) * YC ( HNO4         )                            ! HNO4=0.2000D+00*NO3+...
     &    +                 RKI(   106 ) * YC ( HCHO2        ) * YCP( NO2          )      ! HCHO2+NO2=NO3+HCHO
     &    +                 RKI(   109 ) * YC ( MECHO2       ) * YCP( NO2          )      ! MECHO2+NO2=NO3+MECHO
     &    +                 RKI(   112 ) * YC ( RCHO2        ) * YCP( NO2          )      ! RCHO2+NO2=NO3+RCHO
     &    +    4.0000D-01 * RKI(   122 ) * YC ( PAN          )                            ! PAN=0.4000D+00*NO3+0.6000D+...
     &    +    4.0000D-01 * RKI(   131 ) * YC ( PBZN         )                            ! PBZN=0.4000D+00*NO3+...
     &    +                 RKI(   165 ) * YC ( xNO3         ) * YC ( NO           )      ! xNO3+NO=NO3+NO
     &    +                 RKI(   167 ) * YC ( xNO3         ) * YC ( NO3          )      ! xNO3+NO3=0.2000D+01*NO3
     &    +    5.0000D-01 * RKI(   168 ) * YC ( xNO3         ) * YC ( SumRO2       )      ! xNO3+SumRO2=0.5000D+00*NO3+...
     &    +                 RKI(   169 ) * YC ( xNO3         ) * YC ( SumRCO3      )      ! xNO3+SumRCO3=NO3+SumRCO3
     &    +    5.0000D-02 * RKI(   745 ) * YC ( PAN2         ) * YCP( OH           )      ! PAN2+OH=0.5000D-01*NO3+...
     &    +    7.4000D-01 * RKI(   748 ) * YC ( APANS        ) * YCP( OH           )      ! APANS+OH=0.7400D+00*NO3+...
     &    +    4.0000D-01 * RKI(   751 ) * YC ( APANS        )                            ! APANS=0.4000D+00*NO3+...

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI(     9 ) * YCP( NO           )   ! NO3+NO=0.2000D+...
     &    +                 RKI(    11 ) * YCP( NO2          )   ! NO3+NO2=N2O5
     &    +                 RKI(    15 ) * YCP( NO2          )   ! NO3+NO2=NO+NO2
     &    +                 RKI(    16 )                         ! NO3=NO
     &    +                 RKI(    17 )                         ! NO3=NO2+O3P
     &    +                 RKI(    26 ) * YCP( OH           )   ! NO3+OH=HO2+NO2
     &    +                 RKI(    40 ) * YCP( HO2          )   ! NO3+HO2=OH+NO2
     &    +                 RKI(    59 ) * YC ( RO2C         )   ! NO3+RO2C=NO2
     &    +                 RKI(    64 ) * YC ( RO2XC        )   ! NO3+RO2XC=NO2
     &    +                 RKI(    69 ) * YC ( MEO2         )   ! NO3+MEO2=HCHO+...
     &    +                 RKI(    74 ) * YC ( ETO2         )   ! NO3+ETO2=NO2+HO2+...
     &    +                 RKI(    79 ) * YC ( BZO2         )   ! NO3+BZO2=BZO+NO2
     &    +                 RKI(    84 ) * YC ( MECO3        )   ! NO3+MECO3=NO2+...
     &    +                 RKI(    91 ) * YC ( BZCO3        )   ! NO3+BZCO3=NO2+...
     &    +                 RKI(   120 ) * YC ( HCHO         )   ! NO3+HCHO=HNO3+HO2+CO
     &    +                 RKI(   126 ) * YC ( GLY          )   ! NO3+GLY=HNO3+...
     &    +                 RKI(   129 ) * YC ( BALD         )   ! NO3+BALD=HNO3+...
     &    +                 RKI(   136 ) * YC ( CATL3        )   ! NO3+CATL3=HNO3+OTHN
     &    +                 RKI(   138 ) * YC ( NAPPRD       )   ! NO3+NAPPRD=HNO3+OTHN
     &    +                 RKI(   457 ) * YC ( R2CO3        )   ! NO3+R2CO3=NO2+...
     &    +                 RKI(   464 ) * YC ( MACO3        )   ! NO3+MACO3=NO2+...
     &    +                 RKI(   473 ) * YC ( ETHEN        )   ! NO3+ETHEN=...
     &    +                 RKI(   481 ) * YC ( PROPE        )   ! NO3+PROPE=...
     &    +                 RKI(   489 ) * YC ( ISOP         )   ! NO3+ISOP=ISOP_N3+...
     &    +                 RKI(   499 ) * YC ( BUT13        )   ! NO3+BUT13=...
     &    +                 RKI(   505 ) * YC ( APINE        )   ! NO3+APINE=...
     &    +                 RKI(   511 ) * YC ( BPINE        )   ! NO3+BPINE=...
     &    +                 RKI(   532 ) * YC ( MECHO        )   ! NO3+MECHO=HNO3+...
     &    +                 RKI(   536 ) * YC ( GLCHO        )   ! NO3+GLCHO=HNO3+...
     &    +                 RKI(   541 ) * YC ( ETCHO        )   ! NO3+ETCHO=HNO3+...
     &    +                 RKI(   547 ) * YC ( ACRO         )   ! NO3+ACRO=0.9400D+...
     &    +                 RKI(   559 ) * YC ( MACR         )   ! NO3+MACR=MACR_N3+...
     &    +                 RKI(   571 ) * YC ( PHEN         )   ! NO3+PHEN=HNO3+BZO
     &    +                 RKI(   584 ) * YC ( OLE1         )   ! NO3+OLE1=0.9000D-...
     &    +                 RKI(   590 ) * YC ( OLE2         )   ! NO3+OLE2=0.8000D+...
     &    +                 RKI(   594 ) * YC ( OLE3         )   ! NO3+OLE3=0.8600D+...
     &    +                 RKI(   600 ) * YC ( OLE4         )   ! NO3+OLE4=0.9200D+...
     &    +                 RKI(   608 ) * YC ( TERP         )   ! NO3+TERP=TERP_N3+...
     &    +                 RKI(   618 ) * YC ( SESQ         )   ! NO3+SESQ=SESQ_N3+...
     &    +                 RKI(   629 ) * YC ( FURNS        )   ! NO3+FURNS=...
     &    +                 RKI(   636 ) * YC ( RCHO         )   ! NO3+RCHO=HNO3+...
     &    +                 RKI(   642 ) * YC ( OLEA1        )   ! NO3+OLEA1=...
     &    +                 RKI(   652 ) * YC ( OLEA2        )   ! NO3+OLEA2=...
     &    +                 RKI(   673 ) * YC ( OLEP         )   ! NO3+OLEP=0.7400D+...
     &    +                 RKI(   678 ) * YC ( MGLY         )   ! NO3+MGLY=HNO3+...
     &    +                 RKI(   682 ) * YC ( CRES         )   ! NO3+CRES=HNO3+BZO
     &    +                 RKI(   684 ) * YC ( XYNL         )   ! NO3+XYNL=HNO3+BZO
     &    +                 RKI(   686 ) * YC ( CATL         )   ! NO3+CATL=HNO3+BZO
     &    +                 RKI(   750 ) * YC ( APANS        )   ! NO3+APANS=...
     &    +                 RKI(   767 )                         ! NO3=HNO3

c..Loss frequency of N2O5
      L16 =                 RKI(    12 )                         ! N2O5=NO2+NO3
     &    +                 RKI(    13 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(    14 )                         ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(   762 )                         ! N2O5=HNO3+H2NO3PIJ
     &    +                 RKI(   763 )                         ! N2O5=HNO3+H2NO3PK

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI(    41 ) * DTC

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
