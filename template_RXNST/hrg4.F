
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
R1  PRECONDITIONS: For SAPRC99 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, September, 2003
C                    
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
R3      REAL   A, B, C, Q   ! Quadratic equation terms
R3      REAL   CMN          ! Temp scalar
R3      REAL   L15          ! Loss of NO3
R3      REAL   L16          ! Loss of N2O5
R3      REAL   P15          ! Production of NO3

R3      REAL   R040DT       ! Kno3+no3 x delta t
R3      REAL   R012DT       ! Kn2o5-->no3 x delta t
R3      REAL   R011DT       ! Kno3+no2-->n2o5

C**********************************************************************

S1

c..Production of NO3 (except from N2O5 )
      PNO3  = RXRAT( NCELL,   6 ) + RXRAT( NCELL,   8 ) + RXRAT( NCELL,  27 ) +
     &         0.390 * RXRAT( NCELL,  34 )

c..Loss frequncy of NO3 ( excluding NO3 + NO3 ) 
      LNO3 =  RKI( NCELL,    9 ) * YC( NCELL, NO       ) + RKI( NCELL,   11 ) * YC( NCELL, NO2      ) +
     &        RKI( NCELL,   14 ) * YC( NCELL, NO2      ) + RKI( NCELL,   15 )                  +
     &        RKI( NCELL,   16 )                  + RKI( NCELL,   26 ) * YC( NCELL, HO       ) +
     &        RKI( NCELL,   39 ) * YC( NCELL, HO2      ) + RKI( NCELL,   48 ) * YC( NCELL, C_O2     ) +
     &        RKI( NCELL,   53 ) * YC( NCELL, RO2_R    ) + RKI( NCELL,   58 ) * YC( NCELL, R2O2     ) + 
     &        RKI( NCELL,   65 ) * YC( NCELL, RO2_N    ) + RKI( NCELL,   73 ) * YC( NCELL, CCO_O2   ) + 
     &        RKI( NCELL,   83 ) * YC( NCELL, RCO_O2   ) + RKI( NCELL,   94 ) * YC( NCELL, BZCO_O2  ) + 
     &        RKI( NCELL,  106 ) * YC( NCELL, MA_RCO3  ) + RKI( NCELL,  129 ) * YC( NCELL, HCHO     ) + 
     &        RKI( NCELL,  132 ) * YC( NCELL, CCHO     ) + RKI( NCELL,  135 ) * YC( NCELL, RCHO     ) +
     &        RKI( NCELL,  148 ) * YC( NCELL, GLY      ) + RKI( NCELL,  151 ) * YC( NCELL, MGLY     ) +
     &        RKI( NCELL,  154 ) * YC( NCELL, PHEN     ) + RKI( NCELL,  156 ) * YC( NCELL, CRES     ) +
     &        RKI( NCELL,  157 ) * YC( NCELL, NPHE     ) + RKI( NCELL,  160 ) * YC( NCELL, BALD     ) +
     &        RKI( NCELL,  163 ) * YC( NCELL, METHACRO ) + RKI( NCELL,  172 ) * YC( NCELL, ISOPROD  ) +
     &        RKI( NCELL,  187 ) * YC( NCELL, ETHENE   ) + RKI( NCELL,  191 ) * YC( NCELL, ISOPRENE ) +
     &        RKI( NCELL,  195 ) * YC( NCELL, TRP1     ) + RKI( NCELL,  206 ) * YC( NCELL, OLE1     ) +
     &        RKI( NCELL,  210 ) * YC( NCELL, OLE2     )


c..Loss frequency of  N2O5 
      LN2O5 = RKI( NCELL,   12 ) + RKI( NCELL,   13 )


c..Solution of quadratic equation to get NO3 & N2O5
c....R040DT = K for NO3+NO3= times delta t
c....R011DT = K for NO3+NO2=N2O5 times delta t times [NO2]
c....R012DT = K for N2O5=NO3+NO2 times delta t

      K15_15 = RKI( NCELL,   40 ) * DTC
      R16_15 = RKI( NCELL,   11 ) * YC( NCELL, NO2 ) * DTC
      R15_16 = RKI( NCELL,   12 ) * DTC

      CMN = 1.0 + L16 * DTC
      A = 2.0 * K15_15 * CMN
      B = CMN * ( 1.0 + LNO3 * DTC ) - R15_16 * R16_15
      C = CMN * ( YC0( NCELL, NO3 ) + PNO3 * DTC ) + R15_16 * YC0( NCELL, N2O5 )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP( NCELL, NO3 ) = MAX( Q / A , -C / Q  )

      YCP( NCELL, N2O5 ) = ( YC0( NCELL, N2O5 ) + R16_15 * YCP( NCELL, NO3 ) ) / CMN

S1

      RETURN


      END
