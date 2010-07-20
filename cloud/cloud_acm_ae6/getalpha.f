
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
C $Header: /project/yoj/arc/CCTM/src/cloud/cloud_acm_ae6/Attic/getalpha.f,v 1.1 2010/07/20 11:56:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE GETALPHA ( NUMI, MASSI, SURFI,
     &                      LWC, T, P, RHOAIR, ALFA0, ALFA2, ALFA3 )

C-----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Calculate the in-cloud scavenging coefficients for number (alfa0),
C    surface area (alfa2) and mass (alfa3). This code combines the RPM
C    codes getcld.f, and pandkcd.f
C
C  Revision History:
C      Date       Who                      What
C    -------- -------------------------  -------------------------------
C    12/16/97 Dr. Francis S. Binkowski.  Coded
C    12/22/97 S.Roselle                  Revised to comply with M3 Coding
C                                           standards
C    02/10/99 Binkowski                  Added provision for 2nd moment
C                                           and variable geometric
C                                           standard deviation
C    09/25/00 S.Roselle                  Modified to enable backwards
C                                           compatibility with aerosol
C                                           model version 1
C    08/23/2005  Binkowski               Changed check on MASSI to
C                                           be 1.0e-6 [ug/M**3] rather than
C                                           to avoid difficulties causing NAN's
C                                           as found by Bonyoung Koo
C                                           I have also commented out some lines 
C                                           code that are unnecessary in this 
C                                           implementation
C-----------------------------------------------------------------------

      IMPLICIT NONE

C...........PARAMETERS and their descriptions:

      REAL          SIGC                ! geometric standard deviation for cloud droplets
      PARAMETER   ( SIGC = 1.2 )

      REAL          P0                  ! Standard pressure [ Pa ]
      PARAMETER   ( P0 = 101325.0 )

      REAL          T0                  !
      PARAMETER   ( T0 = 288.0 )        ! [ K ]

      REAL          PI                  !
      PARAMETER   ( PI = 3.14159265 )


      REAL          ONE3
      PARAMETER   ( ONE3 = 1.0 / 3.0 )

      REAL          TWO3
      PARAMETER   ( TWO3 = 2.0 / 3.0 )

      REAL          KBOLTZ              !
      PARAMETER   ( KBOLTZ = 1.38066E-23 ) ! [ J/K ]

      REAL          RHOPART             ! particle density
      PARAMETER   ( RHOPART = 1.0E3 )   ! [ kg/ m**3 ]

      REAL          AERCONST            !
csjr      PARAMETER   ( AERCONST = 6.0E-9 / ( RHOPART *  PI / 6.0 ) )
      PARAMETER   ( AERCONST = 1.0E-9 / ( RHOPART *  PI / 6.0 ) )

      REAL          GRAV                !
      PARAMETER   ( GRAV = 9.8 )        ! [ m/s**2 ]

      REAL          RHO_WATER           ! density of water at 20 C and 1 ATM
      PARAMETER   ( RHO_WATER = 1.0E3 ) ! [ kg/m**3 ]

C...........ARGUMENTS and their descriptions

      REAL          NUMI                ! total number concentration for the Aitken mode [ #/m**3 ]
      REAL          MASSI               ! total mass concentration in AItken mode  [ ug/m**3 ]
      REAL          SURFI               ! total surface area in Aitken mode [ m**2 / m**3 ]
      REAL          LWC                 ! cloud liquid water content [ kg/m**3 ]
      REAL          T                   ! average cloud temperature [ K ]
      REAL          P                   ! average cloud pressure [ Pa ]
      REAL          RHOAIR              ! average cloud density [ kg/m**3 ]
      REAL          ALFA0               ! scavenging coefficient for number [ 1/s ]
      REAL          ALFA2               ! scavenging coefficient for surface area [ 1/s ]
      REAL          ALFA3               ! scavenging coefficient for mass   [ 1/s ]

C...........LOCAL VARIABLES (scalars) and their descriptions:

      REAL          DGNI                ! geometric mean diameter for AItken mode [ m ]
      REAL          L2SGI               ! square of the log of the
                                        ! Aitken mode geometric standard deviation
c      REAL          XXFN                ! fraction for computing L2SGI

c *** ranges for acceptable values of LOG( sigma_g).

      REAL          MINL2SG             ! minimum value of L2SG
      PARAMETER   ( MINL2SG = 2.380480e-3 )  ! minimum sigma_g = 1.05

      REAL          MAXL2SG             ! maximum value of L2SG
      PARAMETER   ( MAXL2SG = 8.395887e-1 )  ! maximum sigma_g = 2.5

      REAL          DEFL2SG             ! default value of L2SG if no surface area
      PARAMETER   ( DEFL2SG = 2.815663e-1 )  ! default sigma_g = 1.7

      REAL          NC                  ! total cloud droplet number concentration  [ #/m**3 ]
      REAL          DGC                 ! geometric mean diameter for cloud droplet [ m ]
      REAL          ESG4                ! exp(0.5 * L2SGI )
      REAL          ESG16               ! exp(2.0 * L2SGI )
      REAL          ESG36               ! exp(4.5 * L2SGI )
      REAL          DG2                 ! square of geometic mean diameter
      REAL          AMM2                ! -2nd moment of the Aitken mode distribution
      REAL          AMM1                ! -1st moment of the Aitken mode distribution
      REAL          AM1                 ! 1st moment of the Aitken mode distribution
      REAL          AM0                 ! alternative name for Aitken mode number  [ #/m**3]
      REAL          AM2                 ! 2nd moment of the Aitken mode distribution
      REAL          AM3                 ! 3rd moment of the Aitken mode distribution
      REAL          M0C                 ! moments of the cloud droplet distribution
      REAL          M1C                 ! moments of the cloud droplet distribution
      REAL          M2C                 ! moments of the cloud droplet distribution

      REAL          LSGC, LSGC2         ! log and square of log of SIGC
      REAL          ESGC4               !
      SAVE          ESGC4
      REAL          ESGC16              !
      SAVE          ESGC16

      REAL          COEF                !
      REAL          DIFF0               ! aerosol diffusivities
      REAL          DIFF2               ! aerosol diffusivities
      REAL          DIFF3               ! aerosol diffusivities
      REAL          PE0                 ! Peclet numbers
      REAL          PE2                 ! Peclet numers
      REAL          PE3                 ! Peclet numbers
      REAL          COEF2               !
      REAL          COEF3               !
      REAL          COEF4               !
      REAL          VSETTL              ! settling velocity for cloud drops
      REAL          LAMDA               ! mean free path of air [ m ]
      REAL          MU                  ! dynamic viscosity [ kg/(m*s) ]
      REAL          NU                  ! kinematic viscosity [ m**2/s ]

      REAL          CUBRT               ! cube root
      REAL          XX                  ! dummy arguement for cube root

      REAL          CLCONST             !
      SAVE          CLCONST             !

      LOGICAL       FIRSTIME            ! flag for first pass thru
      DATA          FIRSTIME / .TRUE. /
      SAVE          FIRSTIME

C...........STATEMENT FUNCTIONS:

      CUBRT( XX ) = EXP( ONE3 * LOG( XX ) )

C*********************************************************************
C     begin body of subroutine GETALPHA

      IF ( FIRSTIME ) THEN

C...compute special variables for moments
C...  of cloud droplet distribution

        LSGC   = LOG( SIGC )
        LSGC2  = LSGC * LSGC
        ESGC4  = EXP( 0.5 * LSGC2 )
        ESGC16 = EXP( 2.0 * LSGC2 )

C...compute constant for obtaining dgc

        CLCONST = 6.0 /
     &     ( RHO_WATER * PI * EXP( 4.5 * LOG( SIGC ) ** 2 ) )

        FIRSTIME = .FALSE.

      END IF ! check on firstime

C...check to make sure that there is aerosol mass and number before
C... proceeding, if not set alphas to zero

cccc FSB      IF ( ( NUMI .LE. 0.0 ) .OR. ( MASSI .LE. 0.0 ) .OR.
cccc FSB    &     ( LWC  .LE. 0.0 ) ) THEN

C FSB mass check changed to be 1.0e-6 [ug/m**3] - 08/23/2005
C     to avoid AM3 becomming too small.  With this change
C     AM3 is no smaller than 1.0e-18

      IF ( ( NUMI .LE. 0.0 ) .OR. ( MASSI .LE. 1.0e-6 ) .OR.
     &     ( LWC  .LE. 0.0 ) ) THEN
cccc     &     ( SURFI .LE. 0.0 ) .OR. ( LWC  .LE. 0.0 ) ) THEN
        ALFA0 = 0.0
        ALFA2 = 0.0
        ALFA3 = 0.0
        RETURN
      END IF

C...get cloud characteristics
C...  reference: Bower, K.N. and T.W. Choularton, 1992,
C...  " A parameterisation of the effective radius of ice
C...  free clouds for use in global climate models"
C...  Atmospheric Research, 27, 305-339, figures 10 & 11.
C...
C...  data in Bower & Choularton fit with quadratic function
C...  for droplet number as a function of lwc

C...calculate the total number concentration of cloud droplets.

      IF ( LWC .LE. 3.0E-3 ) THEN
        NC = 4.80E11 * LWC - 8.0E13 * LWC * LWC
      ELSE
        NC = 7.20E8
      END IF

C...calculate dgc

      DGC =  CUBRT( CLCONST * LWC / NC )

C...get m0c cloud droplet number and 1st and 2nd moments
C...  of cloud droplet distribution

      M0C = NC
      M1C = M0C * DGC * ESGC4
      M2C = M0C * DGC * DGC * ESGC16

C...set values for the 0th and 3rd moments

      AM3   = AERCONST * MASSI
      AM0   = NUMI

C...Check to see if surface area exists and process accordingly

      IF ( SURFI .GT. 0.0 ) THEN

C...set the 2nd moment value

        AM2   = SURFI / PI

C...get square of the log of the geometric standard deviation L2SGI
C...  The following method is much more robust, even if it uses three log calls.

        L2SGI = ONE3 * LOG( AM0 ) +
     &          TWO3 * LOG( AM3 ) -
     &                 LOG( AM2 )

        L2SGI = MAX( MINL2SG, L2SGI )
        L2SGI = MIN( MAXL2SG, L2SGI )

      ELSE

C...set the standard deviation to a fixed value (e.g. 1.7)

        L2SGI = DEFL2SG

C...calculate the 2nd moment based on the fixed standard deviation

      	AM2 = EXP( TWO3 * LOG( AM3 ) +
     &             ONE3 * LOG( AM0 ) -
     &             L2SGI )

      END IF

C...compute special variables for moments
C...  of Aitken mode distribution

      ESG4  = EXP( 0.5 * L2SGI )
      ESG16 = EXP( 2.0 * L2SGI )
      ESG36 = ESG4 * ESG16 * ESG16

C...calculate dgni from numi and m3i

      DGNI = CUBRT( AM3 / ( AM0 * ESG36 ) )
      DG2  = DGNI  * DGNI

C...now get other aerosol moments

      AMM2  = AM0  * ESG16 / DG2
      AMM1  = AM0  * ESG4  / DGNI
      AM1   = AM0  * DGNI  * ESG4

C...calculate:
C...  lamda     mean free path of air [ m ]
C...  mu        dynamic viscosity [ kg/(m*s) ]
C...  nu        kinematic viscosity [ m**2/s ]

      MU = 1.458E-6 * T * SQRT( T ) / ( T + 110.4 )
      LAMDA = 6.6328E-8 * ( P0 / P ) * ( T / T0 )
      NU = MU / RHOAIR

C...implement Pruppacher and Klett method for scavenging coefficients
C...  calculates scavenging coefficients for number and mass
C...  of interstitial aerosol with a variable sigma_g.
C...  this method uses Brownian diffusion with convective enhancement.
C...  reference:
C...      Pruppacher & Klett "Microphysics of Clouds & Precipitation"
C...                             pp 380 - 384, 1978, D. Reidel

      COEF = KBOLTZ * ( T / ( 3.0 * PI * MU ) )

C...get aerosol diffusivities

cccc FSB these values are not used
cccc      DIFF0 = COEF * ( AMM1 + 2.492 * LAMDA * AMM2 ) / AM0
cccc      DIFF2 = COEF * ( AM1  + 2.492 * LAMDA * AM0  ) / AM2
      DIFF3 = COEF * ( ( AM2  + 2.492 * LAMDA * AM1  ) / AM3 )

C...set up coefficient coef2 for settling velocity of cloud drops

      COEF2 = ( GRAV / ( 18.0 * NU ) ) * ( RHOPART / RHOAIR )

C...get settling velocity for cloud drops

      VSETTL = COEF2 * M2C / M0C

C...set up for Peclet numbers

      COEF3 = VSETTL * DGC

C...calculate Peclet numbers

cccc FSB because these Peclet numbers are not used, I have commented 
cccc     these lines of code - Binkowski 08/23/2005
cccc      PE0 = COEF3 / DIFF0
cccc      PE2 = COEF3 / DIFF2
      PE3 = COEF3 / DIFF3

C...now get in-cloud scavenging coefficients
C...  including convective enhancement

      COEF4  = 2.0 * PI * M1C

cccc FSB use a common value, that for 3rd moment.
cccc
cccc      ALFA0 = COEF4 * DIFF0 * ( 1.0 + 0.5 * CUBRT( PE0 ) )
cccc      ALFA2 = COEF4 * DIFF2 * ( 1.0 + 0.5 * CUBRT( PE2 ) )
      ALFA3 = COEF4 * DIFF3 * ( 1.0 + 0.5 * CUBRT( PE3 ) )
      ALFA0 = ALFA3
      ALFA2 = ALFA3

      RETURN
      END
