
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/beis3.f,v 1.1 2010/07/20 11:26:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE BEIS3( JDATE, JTIME, NX, NY, MSPCS, COSZEN, SEMIS,
     &                  SLAI, TA, RSOLAR, PRES, EMPOL )

C-----------------------------------------------------------------------
C Description:
 
C   Uses PAR and sfc temperature data to calculate
C   biogenic ISOP and MBO emissions.  Other emissions are
C   calculated using the temperature data only.
 
C Preconditions:
C   PAR and Surface Temperature
 
C Subroutines and Functions Called:
 
C Revision History:
C   4/01 : Prototype by JMV
C   6/05 : updates for BEIS3.3 by D. Schwede (BEIS3.13)
C   8/05 : additional diagnostic messages for PAR out of bounds (G. Pouliot)
C  10/06 : yoj
C   1/10 : yoj remove ck & report if TAIR > 315
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C              System
C File: @(#)$Id: beis3.f,v 1.1 2010/07/20 11:26:47 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/beis3.f,v $
C Last updated: $Date: 2010/07/20 11:26:47 $
C-----------------------------------------------------------------------

      USE BIOG_EMIS, ONLY: NSEF, NLAI, LAITYPES

      IMPLICIT NONE

C Includes:

C Arguments:
      INTEGER, INTENT( IN ) :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN ) :: NX      ! no. columns
      INTEGER, INTENT( IN ) :: NY      ! no. rows
      INTEGER, INTENT( IN ) :: MSPCS   ! no. of output species

      REAL,    INTENT( IN ) :: COSZEN( NX,NY )        ! cosine of zenith angle
      REAL,    INTENT( IN ) :: SEMIS ( NX,NY,NSEF-1 ) ! normalized emissions
      REAL,    INTENT( IN ) :: SLAI  ( NX,NY,NLAI )   ! leaf area indices
      REAL,    INTENT( IN ) :: TA    ( NX,NY )        ! surface air temperature [K]
      REAL,    INTENT( IN ) :: RSOLAR( NX,NY )        ! surface radiation [w/m**2]
      REAL,    INTENT( IN ) :: PRES  ( NX,NY )        ! surface pressure [Pa]

      REAL,    INTENT( OUT ) :: EMPOL( NX,NY,NSEF )   ! output emissions

C Local Variables:
      INTEGER        R, C, L, I   ! counters

      REAL           CFOTHR       ! isop corr fac -- non-forest
      REAL           CFCLAI       ! isop corr fac -- LAI
      REAL           CFNO         ! NO correction factor
      REAL           CFOVOC       ! non-isop corr fac
      REAL           CFSESQT      ! sesquiterpene corr fac
      REAL           PAR          ! photo. actinic flux (UE/M**2-S) (UE=micro-einsteins)
      REAL           CT, DT       ! temperature correction
      REAL           TAIR         ! local surface temperature
      REAL           RK           ! k from Geron and Guenther
      REAL           CSUBL        ! C sub l
      REAL           TLAI         ! local LAI
      REAL           SOLRAD       ! local solar radiation [W/m**2]
      REAL           PSFC         ! local sfc pressure (mb)
      REAL           ZEN          ! zenith angle
      REAL           PARDB        ! PAR direct beam
      REAL           PARDIF       ! PAR diffuse
      REAL           COSZ         ! local cosine of zenith angle

      CHARACTER( 5 )   :: BTMP    ! temporary variable name
      CHARACTER( 256 ) :: MESG    ! message buffer

      CHARACTER( 16 )  :: PROGNAME = 'BEIS3'   ! procedure name

C-----------------------------------------------------------------------

C Loop through cells
      DO R = 1, NY
         DO C = 1, NX

            TAIR = TA( C,R )         ! unit in degree K
            COSZ = COSZEN( C,R )

C Check min bounds for temperature
C Note we no longer cap temperature for isoprene
            IF ( TAIR .LT. 200.0 ) THEN
               WRITE( MESG, 94010 ) 'TAIR=', TAIR,
     &              'out of range at (C,R)=', C, R
               CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
            END IF

C Calculate temperature correction term
            DT = 28668.514 / TAIR
            CT = EXP( 37.711 - 0.398570815 * DT ) /
     &               ( 1.0 + EXP( 91.301 - DT ) )

            SOLRAD = RSOLAR( C,R )

C Cosine of zenith angle to zenith angle (radians)
            ZEN =  ACOS( COSZ )
            PSFC = PRES( C,R )

C Direct and diffuse photosynthetically active radiation
            CALL GETPARB( SOLRAD, PSFC, COSZ, PARDB, PARDIF )

            PAR = PARDB + PARDIF

C Check max/min bounds of PAR and calculate biogenic ISOP
            IF ( PAR .LT. 0.0 .OR. PAR .GT. 2600.0 ) THEN
               
               WRITE( MESG, 94030 ) 'PAR=', PAR,
     &              'out of range at (C,R)=', C, R,
     &              'PARDB  = ', PARDB,
     &              'PARDIF = ', PARDIF,
     &              'SOLRAD = ', SOLRAD,
     &              'PSFC   = ', PSFC,
     &              'ZEN    = ', ZEN
    
                CALL M3MSG2( MESG )
!               CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
            END IF

C Compute ISOP and MBO and METH emissions first
C Note assumption that these are the first 3 species in LAITYPE and BIOTYPE
C arrays
            DO I = 1, NLAI

               BTMP = LAITYPES( I )
               TLAI = SLAI( C,R,I )

C Adjust methanol based on T. Pierce recommendation (1-16-03)
               IF ( TRIM( BTMP ) == 'METH' ) THEN
                  TLAI = MAX( 3.0, TLAI )
               END IF

               IF ( TLAI .GT. 10.0 ) THEN
                  WRITE( MESG, 94010 ) 'LAI=', TLAI,
     &              'out of range at (C,R)=', C, R
                  CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
               END IF

C Initialize csubl
               CSUBL = 0.0

               IF ( PAR .LE. 0.01 .OR.
     &              COSZ .LE. 0.02079483 ) THEN
                  EMPOL( C,R,I ) = 0.0
               ELSE
                  IF ( TLAI .GT. 0.1 ) THEN
                     CSUBL = CLNEW( ZEN, PARDB, PARDIF, TLAI )
                  ELSE  ! keep this or not?
                     CSUBL = CGUEN( PAR )
                  END IF
                  EMPOL( C,R,I ) = SEMIS( C,R,I ) * CT * CSUBL
               END IF

            END DO ! end ISOP and MBO calculations loop

C Calculate other biogenic emissions except NO
C Note not speciated here
C Limit temerature to 315 K for monoterpenes and other VOCs
            TAIR = MIN( TAIR, 315.0 )

            CFOVOC  = EXP( 0.09 * ( TAIR - 303.0 ) )
            CFSESQT = EXP( 0.17 * ( TAIR - 303.0 ) )

            DO I = NLAI + 1, NSEF - 2
               EMPOL( C,R,I ) = SEMIS( C,R,I ) * CFOVOC
            END DO

            I = NSEF - 1
            EMPOL( C,R,I ) = SEMIS( C,R,I ) * CFSESQT

         END DO ! end loop over columns
      END DO ! end loop over rows

      RETURN

C-----------------------------------------------------------------------

94010 FORMAT( 1X, A, F10.2, 1X, A, I3, ',', I3 )
94020 FORMAT( 1X, A, F10.2, 1X, A, I3, ',', I3, A )
94030 FORMAT( 1X, A, F10.2, 1X, A, I3, ',', I3, 1X, 5(A, F10.2) )

C-----------------------------------------------------------------------
      CONTAINS

C Function to calculate csubl based on zenith angle, par, and lai
         REAL FUNCTION CLNEW( ZEN, PARDB, PARDIF, TLAI )

         IMPLICIT NONE

C Function arguments:
         REAL, INTENT( IN ) :: PARDB    ! direct beam PAR( umol/m2-s)
         REAL, INTENT( IN ) :: PARDIF   ! diffuse PAR ( umol/m2-s)
         REAL, INTENT( IN ) :: ZEN      ! solar zenith angle (radians)
         REAL, INTENT( IN ) :: TLAI     ! leaf area index for grid cell

C Parameters:
         REAL, PARAMETER :: ALPHA = 0.8 ! leaf absorptivity
         REAL, PARAMETER :: KD = 0.68   ! extinction coefficient for diffuse radiation
         
         
C Local variables:
         REAL, SAVE :: SQALPHA ! square root of alpha
         REAL KBE              ! extinction coefficient for direct beam
         REAL CANPARSCAT       ! exponentially wtd scattered PAR (umol/m2-s)
         REAL CANPARDIF        ! exponentially wtd diffuse PAR (umol/m2-s)
         REAL PARSHADE         ! PAR on shaded leaves (umol/m2-s)
         REAL PARSUN           ! PAR on sunlit leaves (umol/m2-s)
         REAL LAISUN           ! LAI that is sunlit
         REAL FRACSUN          ! fraction of leaves that are sunlit
         REAL FRACSHADE        ! fraction of leaves that are shaded

         LOGICAL, SAVE :: FIRSTIME = .TRUE.

C-----------------------------------------------------------------------
         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SQALPHA = SQRT( ALPHA )
         END IF

C CN98 - eqn 15.4, assume x=1
         KBE = 0.5 * SQRT( 1.0 + TAN( ZEN ) * TAN( ZEN ) )

C CN98 - p. 261 (this is usually small)
         CANPARSCAT = 0.5 * PARDB * ( EXP( -1.0 * SQALPHA * KBE * TLAI )
     &              - EXP( -1.0 * KBE * TLAI ) )

C CN98 - p. 261 (assume exponentially wtd avg)
         CANPARDIF  = PARDIF * ( 1.0 - EXP( -1.0 * SQALPHA * KD * TLAI ) )
     &            / ( SQALPHA * KD * TLAI )

C CN98 - p. 261 (for next 3 eqns)
C note that we use the incoming (not absorbed) PAR
         PARSHADE   = CANPARDIF + CANPARSCAT
         PARSUN     = KBE * PARDB + PARSHADE
         LAISUN     = ( 1.0 - EXP( -1.0 * KBE * TLAI ) ) / KBE
         FRACSUN    = LAISUN / TLAI
         FRACSHADE  = 1.0 - FRACSUN

C cguen is Guenther's eqn for computing light correction as a function of
C PAR...fracSun should probably be higher since sunlit leaves tend to be
C thicker than shaded leaves. But since we need to make crude assumptions
C regarding leaf orientation (x=1), we will not attempt to fix at the moment.

         CLNEW = FRACSUN * CGUEN( PARSUN )
     &         + FRACSHADE * CGUEN( PARSHADE )

         RETURN

         END FUNCTION CLNEW

C-----------------------------------------------------------------------

C Function to calculate Guenther's equation for computing light correction
         REAL FUNCTION CGUEN( PAR )

         IMPLICIT NONE

C Function arguments:
         REAL, INTENT( IN ) :: PAR

C Parameters:
         REAL, PARAMETER :: ALPHA = 0.001
         REAL, PARAMETER :: CL = 1.42

C-----------------------------------------------------------------------

         IF ( PAR .LE. 0.01 ) THEN
            CGUEN = 0.0
         ELSE
            CGUEN = ( ALPHA * CL * PAR )
     &            / SQRT( 1.0 + ALPHA * ALPHA * PAR * PAR )
         END IF

         RETURN

         END FUNCTION CGUEN

C-----------------------------------------------------------------------

      END SUBROUTINE BEIS3

