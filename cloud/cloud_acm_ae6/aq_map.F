
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
C $Header: /project/yoj/arc/CCTM/src/cloud/cloud_acm_ae6/aq_map.F,v 1.4 2011/04/01 15:41:32 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AQ_MAP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                    CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                    REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )

C-----------------------------------------------------------------------
C  DESCRIPTION:  This is an interface processor between the cloud dynamics
C     module(s) and the aqueous chemistry module.  It uses indices to
C     map species from their order in the array POLC, CEND, REMOV to
C     the species that are expected for AQCHEM, ie GAS and AEROSOL.
 
C  Revision History:
C      No   Date   Who	What
C      -- -------- ---  -----------------------------------------
C       0 01/15/98 sjr  created program
C       1 02/13/98 sjr  modified/generalized the mapping procedure
C         Dec 00   Jeff move CGRID_MAP into f90 module
C       3 06/07/05 sjr  added logic for coarse sulfate
C       4 04/11/08 jtk  added logic for coarse ammonium
C       5 10/10/10 yoj  update to use aero_reeng by Steve Howard, Prakash Bhave,
C                       Jeff Young, Sergey Napelenok, and Shawn Roselle
C       6 03/01/11 sjr  replaced I/O API include files with UTILIO_DEFN;
 
C  Called by:  RADMCLD and RESCLD
 
C  Calls the following subroutines:  AQCHEM
 
C  ARGUMENTS    TYPE      I/O       DESCRIPTION
C  ---------   -------  ------  --------------------------------
C    JDATE     integer   input  current model julian date (yyyyddd)
C    JTIME     integer   input  current model time (hhmmss)
C    WTBAR      real     input  avg total water content (kg/m3)
C    WCBAR      real     input  avg liquid water content (kg/m3)
C    TBARC      real     input  avg cloud temperature (K)
C    PBARC      real     input  avg cloud pressure (Pa)
C    CTHK1      real     input  cloud thickness (m)
C    AIRM       real     input  total air mass (moles/m2) in cloudy air
C    PRATE1     real     input  precipitation rate (mm/hr)
C    TAUCLD     real     input  cloud lifetime (s)
C    POLC       real     input  ave vert conc incloud (moles sp/ mole air)
C    CEND       real    output  ending incloud conc (moles/mole)
C    REMOV      real    output  moles/m2 or mm*mol/lit scavenged
C    REMOVAC    real    output  variable storing H+ deposition
C    ALFA0      real     input  scav coef for aitken aerosol number
C    ALFA2      real     input  scav coef for aitken aerosol surface area
C    ALFA3      real     input  scav coef for aitken aerosol mass
C-----------------------------------------------------------------------

      USE CGRID_SPCS     ! CGRID mechanism species
      USE AQ_DATA        ! aqueous chemistry shared parameters
      USE AERO_DATA      ! aerosol shared parameters
      USE UTILIO_DEFN

      IMPLICIT NONE

      CHARACTER( 120 ) :: XMSG = ' '    ! Exit status message

C Parameters:

      INTEGER, SAVE :: MXSPCS   ! Number of species in CGRID

      REAL, PARAMETER :: ONETHIRD  = 1.0 / 3.0
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

C Arguments:

      INTEGER, INTENT( IN )    :: JDATE     ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )    :: JTIME     ! current model time, coded HHMMSS

      REAL,    INTENT( IN )    :: WTBAR     ! total wat cont (kg/m2) int. thru cld
      REAL,    INTENT( IN )    :: WCBAR     ! liq water content of cloud (kg/m3)
      REAL,    INTENT( IN )    :: TBARC     ! mean cloud temp (K)
      REAL,    INTENT( IN )    :: PBARC     ! mean cloud pressure (Pa)
      REAL,    INTENT( IN )    :: CTHK1     ! aq chem calc cloud thickness
      REAL,    INTENT( IN )    :: AIRM      ! total air mass (moles/m2) in cloudy air
      REAL,    INTENT( IN )    :: PRATE1    ! storm rainfall rate (mm/hr)
      REAL,    INTENT( IN )    :: TAUCLD    ! cloud lifetime
      REAL,    INTENT( IN )    :: POLC ( : )  ! avg vert conc incloud (moles/mole)
      REAL,    INTENT( OUT )   :: CEND ( : )  ! ending incloud conc (moles/mole)
      REAL,    INTENT( OUT )   :: REMOV( : )  ! moles/m2 or mm*mol/lit scavenged
      REAL,    INTENT( INOUT ) :: REMOVAC   ! variable storing H+ deposition
      REAL,    INTENT( IN )    :: ALFA0     ! scav coef for aitken aerosol number
      REAL,    INTENT( IN )    :: ALFA2     ! scav coef for aitken aerosol sfc area
      REAL,    INTENT( IN )    :: ALFA3     ! scav coef for aitken aerosol mass

C Local Variables (scalars):

      LOGICAL, SAVE :: FIRSTIME = .TRUE.      ! flag for first pass thru

      CHARACTER(16), SAVE :: PNAME = 'AQ_MAP' ! program name

      INTEGER       IAER                ! aerosol loop counter
      INTEGER       IMODE               ! aerosol mode loop counter
      INTEGER       IGAS                ! gas loop counter
      INTEGER       ISRG                ! surrogate loop counter
      INTEGER       PNTR                ! relative pointer variable
      INTEGER       SPC                 ! liquid species loop counter

      REAL( 8 )  :: BETASO4
      REAL       :: EALFA2T             ! EXP( -ALFA2 * TAUCLD )
      REAL( 8 )  :: M3NEW( NMODES )     ! modal mass at time t
      REAL( 8 )  :: M3OLD( NMODES )     ! modal mass at time 0

      REAL       :: HPWDEP                        ! hydrogen wet dep (mm mol/liter)
      REAL( 8 )  :: GAS    ( NGAS )               ! gas phase conc (mol/mol)
      REAL( 8 )  :: GASWDEP( NGAS )               ! gas phase wet dep array (mm mol/liter)
      REAL( 8 )  :: AEROSOL( NAER,NMODES )        ! aerosol conc (mol/mol)
      REAL( 8 )  :: AERWDEP( NAER,NMODES )        ! aerosol wet dep array (mm mol/liter)
      REAL( 8 )  :: WSRGGAS( NGAS,MXSRG )         ! weights for surrogate
      REAL( 8 )  :: WSRGAER( NAER,NMODES,MXSRG )  ! weights for surrogate

C External Functions:

      INTEGER, EXTERNAL :: INDEXN           ! external func to get species pointers

C-----------------------------------------------------------------------

C...Initialization
C...  event-statistics variables.

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        CALL AQ_DATA_INIT()      

      END IF

C...for subsequent calls, check to make sure some surrogates were
C...  specified, otherwise there is no need to perform aqueous chemistry

      IF ( SIZE( CGRID2AQ_MAP ) .EQ. 0 ) THEN
        RETURN
      END IF

C...load gas-phase concentrations

      DO IGAS = 1, NGAS

        GAS( IGAS ) = 0.0D0

        DO ISRG = 1, NSRGGAS( IGAS )
          PNTR = CGRID2AQ_MAP( LSRGGAS( IGAS, ISRG ) )
          GAS( IGAS ) = GAS( IGAS ) + REAL( POLC( PNTR ), 8 )
        END DO

        IF ( GAS( IGAS ) .GT. 0.0D0 ) THEN
          DO ISRG = 1, NSRGGAS( IGAS )
            PNTR = CGRID2AQ_MAP( LSRGGAS( IGAS, ISRG ) )
            WSRGGAS( IGAS, ISRG ) = REAL( POLC( PNTR ), 8 ) / GAS( IGAS )
          END DO
        ELSE
          DO ISRG = 1, NSRGGAS( IGAS )
            WSRGGAS( IGAS, ISRG ) = 1.0D0 / REAL( NSRGGAS( IGAS ), 8 )
          END DO
        END IF

C...set background values for gases if no surrogates were specified

        IF ( NSRGGAS( IGAS ) .EQ. 0 ) THEN
          GAS( IGAS ) = REAL( SRGGAS( IGAS )%BACKGND, 8 ) * 1.0D-6
        END IF

      END DO

C...load aerosol concentrations

      DO IAER = 1, NAER
        DO IMODE = 1, NMODES

          IF ( SRGAER( IAER )%NAME( IMODE ) .NE. ' ' ) THEN
            AEROSOL( IAER, IMODE ) = 0.0D0

            DO ISRG = 1, NSRGAER( IAER, IMODE )
              PNTR = CGRID2AQ_MAP( LSRGAER( IAER, IMODE, ISRG ) )
              AEROSOL( IAER, IMODE ) = AEROSOL( IAER, IMODE ) + REAL( POLC( PNTR ), 8 )
            END DO

            IF ( AEROSOL( IAER, IMODE ) .GT. 0.0D0 ) THEN
              DO ISRG = 1, NSRGAER( IAER, IMODE )
                PNTR = CGRID2AQ_MAP( LSRGAER( IAER, IMODE, ISRG ) )
                WSRGAER( IAER, IMODE, ISRG ) = REAL( POLC( PNTR ), 8 )
     &                                       / REAL( AEROSOL( IAER, IMODE ), 8 )
              END DO
            ELSE
              DO ISRG = 1, NSRGAER( IAER, IMODE )
                WSRGAER( IAER, IMODE, ISRG ) = 1.0D0 / REAL( NSRGAER( IAER, IMODE ), 8 )
              END DO
            END IF

C...set background values for aerosols if no surrogates were specified

            IF ( NSRGAER( IAER, IMODE ) .EQ. 0 ) THEN
              IF ( SRGAER( IAER )%MOLWT .GT. 0.0 ) THEN
                AEROSOL( IAER, IMODE ) = REAL( SRGAER( IAER )%BACKGND * 1.0E-6 * CTHK1, 8 )
     &                                 / REAL( SRGAER( IAER )%MOLWT * AIRM, 8 )
              ELSE
                AEROSOL( IAER, IMODE ) = REAL( SRGAER( IAER )%BACKGND * CTHK1 / AIRM, 8 )
              END IF
            END IF
          END IF
        END DO
      END DO

C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module

      CALL EXTRACT_AERO ( POLC, .FALSE. )

C *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)

      M3OLD = 0.0
      DO IMODE = 2, N_MODE

        DO SPC = 1, N_AEROSPC
          IF ( ( AEROSPC( SPC )%NAME( IMODE ) .NE. ' ' ) .AND.
     &         ( .NOT. AEROSPC( SPC )%NO_M2WET ) ) THEN
             M3OLD( IMODE ) = M3OLD( IMODE )
     &                      + ( AEROSPC_CONC( SPC,IMODE ) * AEROSPC_MW( SPC )
     &                      / AEROSPC( SPC )%DENSITY )
          END IF
        END DO
      END DO

C...perform aqueous-phase chemistry calculations

      CALL AQCHEM ( JDATE, JTIME, TBARC, PBARC, TAUCLD, PRATE1,
     &              WCBAR, WTBAR, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &              AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4 )

C...  compute the scavenging coefficient
      EALFA2T = EXP( -ALFA2 * TAUCLD )

C...store the amount of hydrogen deposition

      REMOVAC = HPWDEP

C...Now, re-apportion mass back into cend/remov (cgrid-type) array

      DO IGAS = 1, NGAS
        DO ISRG = 1, NSRGGAS( IGAS )
          PNTR = CGRID2AQ_MAP( LSRGGAS( IGAS, ISRG ) )
          CEND ( PNTR ) = GAS    ( IGAS ) * WSRGGAS( IGAS, ISRG )
          REMOV( PNTR ) = GASWDEP( IGAS ) * WSRGGAS( IGAS, ISRG )
        END DO
      END DO

      DO IAER = 1, NAER
        DO IMODE = 1, NMODES
          IF( SRGAER( IAER )%NAME( IMODE ) .NE. ' ' ) THEN
            DO ISRG = 1, NSRGAER( IAER, IMODE )
              PNTR = CGRID2AQ_MAP( LSRGAER( IAER, IMODE, ISRG ) )
              CEND ( PNTR ) = AEROSOL( IAER, IMODE ) * WSRGAER( IAER, IMODE, ISRG )
              REMOV( PNTR ) = AERWDEP( IAER, IMODE ) * WSRGAER( IAER, IMODE, ISRG )
            END DO
          ENDIF 
        END DO
      END DO

C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module

      CALL EXTRACT_AERO ( CEND, .FALSE. )

C *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)

      M3NEW = 0.0
      DO IMODE = 2, N_MODE

        DO SPC = 1, N_AEROSPC
          IF ( ( AEROSPC( SPC )%NAME( IMODE ) .NE. ' ' ) .AND.
     &         ( .NOT. AEROSPC( SPC )%NO_M2WET ) ) THEN
             M3NEW( IMODE ) = M3NEW( IMODE )
     &                      + ( AEROSPC_CONC( SPC,IMODE ) * AEROSPC_MW( SPC )
     &                      / AEROSPC( SPC )%DENSITY )
          END IF
        END DO
      END DO

C...check for minimums

      DO IMODE = 2, N_MODE
        M3OLD( IMODE ) = MAX( M3OLD( IMODE ), CONMIN )
        M3NEW( IMODE ) = MAX( M3NEW( IMODE ), CONMIN )
      END DO

C...Update surface area

      CEND( AEROSRF_MAP( 1 ) ) = POLC( AEROSRF_MAP( 1 ) ) * EALFA2T
      
      CEND( AEROSRF_MAP( 2 ) ) = POLC( AEROSRF_MAP( 2 ) )
     &                         * EXP( -BETASO4 * TAUCLD * ONETHIRD )
     &                         * ( M3NEW( 2 ) / M3OLD( 2 ) ) ** TWOTHIRDS

      CEND( AEROSRF_MAP( 3 ) ) = POLC( AEROSRF_MAP( 3 ) )
     &                         * ( CEND( AERONUM_MAP( 3 ) )
     &                             / POLC( AERONUM_MAP( 3 ) ) ) ** ONETHIRD
     &                         * ( M3NEW(3) / M3OLD( 3 ) ) ** TWOTHIRDS

      RETURN
      END

