
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

       SUBROUTINE HRG4( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of NO3 and N2O5
C
C  PRECONDITIONS: For the CB05CL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, July 18, 2014
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
      CHARACTER( 16 ), SAVE   ::  PNAME = 'HRG4'    ! Program name


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
      P15 =                 RXRAT( NCELL,   5 )      ! O+NO2=NO3
     &    +                 RXRAT( NCELL,   7 )      ! NO2+O3=NO3
     &    +                 RXRAT( NCELL,  29 )      ! OH+HNO3=NO3
     &    +    3.9000D-01 * RXRAT( NCELL,  51 )      ! PNA=0.3900D+00*NO3+0.6100D+...

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI( NCELL,  14 )                   ! NO3=NO2+O
     &    +                 RKI( NCELL,  15 )                   ! NO3=NO
     &    +                 RKI( NCELL,  16 ) * YC( NCELL, NO      )   ! NO3+NO=0.2000D+...
     &    +                 RKI( NCELL,  17 ) * YC( NCELL, NO2     )   ! NO3+NO2=NO+NO2
     &    +                 RKI( NCELL,  18 ) * YC( NCELL, NO2     )   ! NO3+NO2=N2O5
     &    +                 RKI( NCELL,  46 ) * YC( NCELL, O       )   ! NO3+O=NO2
     &    +                 RKI( NCELL,  47 ) * YC( NCELL, OH      )   ! NO3+OH=HO2+NO2
     &    +                 RKI( NCELL,  48 ) * YC( NCELL, HO2     )   ! NO3+HO2=HNO3
     &    +                 RKI( NCELL,  49 ) * YC( NCELL, O3      )   ! NO3+O3=NO2
     &    +                 RKI( NCELL,  77 ) * YC( NCELL, FORM    )   ! NO3+FORM=HNO3+HO2+CO
     &    +                 RKI( NCELL,  85 ) * YC( NCELL, ALD2    )   ! NO3+ALD2=C2O3+HNO3
     &    +                 RKI( NCELL, 100 ) * YC( NCELL, ALDX    )   ! NO3+ALDX=CXO3+HNO3
     &    +                 RKI( NCELL, 119 ) * YC( NCELL, OLE     )   ! NO3+OLE=NO2+FORM+...
     &    +                 RKI( NCELL, 123 ) * YC( NCELL, ETH     )   ! NO3+ETH=NO2+XO2+...
     &    +                 RKI( NCELL, 127 ) * YC( NCELL, IOLE    )   ! NO3+IOLE=0.1180D+...
     &    +                 RKI( NCELL, 132 ) * YC( NCELL, CRES    )   ! NO3+CRES=CRO+HNO3
     &    +                 RKI( NCELL, 144 ) * YC( NCELL, ISOP    )   ! NO3+ISOP=0.2000D+...
     &    +                 RKI( NCELL, 147 ) * YC( NCELL, ISPD    )   ! NO3+ISPD=0.3570D+...
     &    +                 RKI( NCELL, 152 ) * YC( NCELL, TERP    )   ! NO3+TERP=0.4700D+...

c..Loss frequency of N2O5
      L16 =                 RKI( NCELL,  19 )                   ! N2O5=0.2000D+01*HNO3
     &    +                 RKI( NCELL,  20 )                   ! N2O5=0.2000D+01*HNO3
     &    +                 RKI( NCELL,  21 )                   ! N2O5=NO3+NO2
     &    +                 RKI( NCELL,  53 )                   ! N2O5=NO2+NO3
     &    +                 RKI( NCELL, 188 )                   ! N2O5=0.2000D+01*HNO3

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI( NCELL,  50 ) * DTC

      R15_16  = ( RKI( NCELL,  21 )
     &        +   RKI( NCELL,  53 ) ) * DTC 


      R16_15  = RKI( NCELL,  18 ) * YC( NCELL, NO2 ) * DTC

c..Solution of quadratic equation to get NO3 & N2O5
      CMN = 1.0D0 + L16 * DTC
      A = 2.0D0 * K15_15 * CMN
      B = CMN * ( 1.0D0 + L15 * DTC ) - R15_16 * R16_15
      C = CMN * ( YC0( NCELL, NO3 ) + P15 * DTC ) +  R15_16 * YC0( NCELL, N2O5 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )
      YCP( NCELL, NO3 ) = MAX( Q / A , -C / Q  )
      YCP( NCELL, N2O5 ) = ( YC0( NCELL, N2O5 ) + R16_15 * YCP( NCELL, NO3 ) ) / CMN

      RETURN

      END
