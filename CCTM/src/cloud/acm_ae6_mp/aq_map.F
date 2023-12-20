
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AQ_MAP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                    CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                    REMOV, REMOVAC, ALFA0, ALFA2, ALFA3, DARK )

C-----------------------------------------------------------------------
C  DESCRIPTION:  This is an interface processor between the cloud dynamics
C     module(s) and the aqueous chemistry module.  It uses indices to
C     map species from their order in the array POLC, CEND, REMOV to
C     the species that are expected for AQCHEM, ie GAS and AEROSOL.
 
C  Revision History:
C      No   Date   Who  What
C      -- -------- ---  -----------------------------------------
C       0 01/15/98 sjr  created program
C       1 02/13/98 sjr  modified/generalized the mapping procedure
C         Dec 00   Jeff move CGRID_MAP into f90 module
C       3 06/07/05 sjr  added logic for coarse sulfate
C       4 04/11/08 jtk  added logic for coarse ammonium
C       5 10/10/10 yoj  update to use aero_reeng by Steve Howard, Prakash Bhave,
C                       Jeff Young, Sergey Napelenok, and Shawn Roselle
C       6 03/01/11 sjr  replaced I/O API include files with UTILIO_DEFN;
C       7 07/01/01 gs   calculate zenith angle to determine daytime and nightime 
C                       needed for sulfur oxidation via metal catalysis
C       8 09/10/11 wth  adapted for multiple pollutant model, i.e., Gas phase mercury 
C                       PM mercury and tracer species
C       07 Jul 14  wth: replaced mechanism include file(s) with fortran module
C       10 Feb 19 D.Wong: replaced run time dynamic arrays with allocatable arrays
 
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
C    DARK       logical  input DARK = TRUE is night,  DARK = FALSE is day
C-----------------------------------------------------------------------

      USE CGRID_SPCS     ! CGRID mechanism species
      USE AERO_DATA      ! aerosol shared parameters
      USE AQ_DATA        ! aqueous chemistry shared parameters
      USE UTILIO_DEFN

      IMPLICIT NONE
      INCLUDE SUBST_CONST   ! for PI

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
      REAL,    INTENT( INOUT ) :: CEND ( : )  ! ending incloud conc (moles/mole)
      REAL,    INTENT( INOUT ) :: REMOV( : )  ! moles/m2 or mm*mol/lit scavenged
      REAL,    INTENT( INOUT ) :: REMOVAC   ! variable storing H+ deposition
      REAL,    INTENT( IN )    :: ALFA0     ! scav coef for aitken aerosol number
      REAL,    INTENT( IN )    :: ALFA2     ! scav coef for aitken aerosol sfc area
      REAL,    INTENT( IN )    :: ALFA3     ! scav coef for aitken aerosol mass
      LOGICAL, INTENT( IN )    :: DARK      ! DARK = TRUE is night,  DARK = FALSE is day

C Local Variables (scalars):

      LOGICAL, SAVE :: FIRSTIME = .TRUE.      ! flag for first pass thru

      CHARACTER(16), SAVE :: PNAME = 'AQ_MAP' ! program name

      INTEGER       IAER                ! aerosol loop counter
      INTEGER       IMODE               ! aerosol mode loop counter
      INTEGER       IGAS                ! gas loop counter
      INTEGER       ISRG                ! surrogate loop counter
      INTEGER       PNTR                ! relative pointer variable
      INTEGER       SPC                 ! liquid species loop counter
      INTEGER ::    STAT

      REAL( 8 )  :: BETASO4
      REAL       :: EALFA2T             ! EXP( -ALFA2 * TAUCLD )
      REAL( 8 )  :: M3NEW( NMODES )     ! modal mass at time t
      REAL( 8 )  :: M3OLD( NMODES )     ! modal mass at time 0
      REAL( 8 )  :: Dens_wmean_old( NMODES ) ! concentration weighted molecular weight g/mol
      REAL( 8 )  :: Dens_wmean_new( NMODES ) ! concentration weighted molecular weight g/mol

      REAL       :: HPWDEP                                 ! hydrogen wet dep (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE  :: GAS    ( : )        ! gas phase conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE  :: GASWDEP( : )        ! gas phase wet dep array (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE  :: AEROSOL( :, :)      ! aerosol conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE  :: AERWDEP( :, :)      ! aerosol wet dep array (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE  :: WSRGGAS( :, :)      ! weights for surrogate
      REAL( 8 ), ALLOCATABLE, SAVE  :: WSRGAER( :, :, :)   ! weights for surrogate
      
      REAL( 8 ) :: FRACTR, FRACPOA, FRACPRI, FRACSOA
      INTEGER IAERI

C External Functions:

      INTEGER, EXTERNAL :: INDEXN           ! external func to get species pointers


      INTERFACE
        SUBROUTINE AQCHEM ( JDATE, JTIME, TEMP, PRES_PA, TAUCLD, PRCRATE,
     &                      WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &                      AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, DARK,
     &                      FRACTR, FRACPOA, FRACPRI, FRACSOA )
           INTEGER,   INTENT( IN )  :: JDATE  ! current model date, coded YYYYDDD
           INTEGER,   INTENT( IN )  :: JTIME  ! current model time, coded HHMMSS
           REAL,      INTENT( IN )  :: AIRM      ! total air mass in cloudy layers (mol/m2)
           REAL,      INTENT( IN )  :: ALFA0     ! scav coef for aitken aerosol number
           REAL,      INTENT( IN )  :: ALFA2     ! scav coef for aitken aerosol sfc area
           REAL,      INTENT( IN )  :: ALFA3     ! scav coef for aitken aerosol mass
           REAL,      INTENT( OUT ) :: HPWDEP    ! hydrogen wet deposition (mm mol/liter)
           REAL( 8 ), INTENT( OUT ) :: BETASO4  
           REAL,      INTENT( IN )  :: PRCRATE   ! precip rate (mm/hr)
           REAL,      INTENT( IN )  :: PRES_PA   ! pressure (Pa)
           REAL,      INTENT( IN )  :: TAUCLD    ! timestep for cloud (s)
           REAL,      INTENT( IN )  :: TEMP      ! temperature (K)
           REAL,      INTENT( IN )  :: WCAVG     ! liquid water content (kg/m3)
           REAL,      INTENT( IN )  :: WTAVG     ! total water content (kg/m3)
           REAL( 8 ), INTENT( INOUT ) :: GAS    ( : )   ! gas phase concentrations (mol/molV)
           REAL( 8 ), INTENT( INOUT ) :: AEROSOL( :,: ) ! aerosol concentrations (mol/molV)
           REAL( 8 ), INTENT( INOUT ) :: GASWDEP( : )   ! gas phase wet deposition array (mm mol/liter)
           REAL( 8 ), INTENT( INOUT ) :: AERWDEP( :,: ) ! aerosol wet deposition array (mm mol/liter)
	   REAL( 8 ), INTENT( OUT ) :: FRACTR    ! Fraction of J mode tracer scavenged from I mode
	   REAL( 8 ), INTENT( OUT ) :: FRACPOA   ! Fraction of J mode POA scavenged from I mode
	   REAL( 8 ), INTENT( OUT ) :: FRACPRI   ! Fraction of J mode PRI scavenged from I mode
	   REAL( 8 ), INTENT( OUT ) :: FRACSOA   ! Fraction of J mode SOA scavenged from I mode
           LOGICAL,   INTENT( IN )    :: DARK           ! DARK = TRUE is night,  DARK = FALSE is day
        END SUBROUTINE AQCHEM
      END INTERFACE      

C-----------------------------------------------------------------------

C...Initialization
C...  event-statistics variables.

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        ALLOCATE ( GAS    ( NGAS ),
     &             GASWDEP( NGAS ),
     &             AEROSOL( MAX_NAER, NMODES),
     &             AERWDEP( MAX_NAER, NMODES),
     &             WSRGGAS( NGAS, MXSRG),
     &             WSRGAER( MAX_NAER, NMODES, MXSRG),
     &             STAT = STAT)
        IF (STAT .NE. 0) THEN
           XMSG = 'Memory allocation failure'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        CALL AQ_DATA_INIT()      

      END IF

C...for subsequent calls, check to make sure some surrogates were
C...  specified, otherwise there is no need to perform aqueous chemistry

      IF ( SIZE( CGRID2AQ_MAP ) .EQ. 0 ) THEN
        RETURN
      END IF

C...load gas-phase concentrations

      GAS     = 0.0D0
      WSRGGAS = 0.0D0
      GASWDEP = 0.0D0

      DO IGAS = 1, NGAS

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

      AEROSOL = 0.0D0
      WSRGAER = 0.0D0
      AERWDEP = 0.0D0

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
C ... M3OLD is in units of m3/kmol_air
      M3OLD = 0.0
      Dens_wmean_old = 0.0
      DO IMODE = 2, N_MODE

        DO SPC = 1, N_AEROSPC
          IF (  AEROSPC( SPC )%TRACER ) CYCLE 
          IF ( ( AEROSPC( SPC )%NAME( IMODE ) .NE. ' ' ) .AND.
     &         ( .NOT. AEROSPC( SPC )%NO_M2WET ) ) THEN
             M3OLD( IMODE ) = M3OLD( IMODE )
     &                      + ( AEROSPC_CONC( SPC,IMODE ) * AEROSPC_MW( SPC )
     &                      / AEROSPC( SPC )%DENSITY )
             Dens_wmean_old( IMODE ) = Dens_wmean_old( IMODE )  
     &                      + AEROSPC_CONC( SPC,IMODE ) * AEROSPC_MW( SPC )
          END IF
        END DO
      END DO
      Dens_wmean_old(2:N_MODE) = Dens_wmean_old(2:N_MODE) / M3OLD(2:N_MODE)

C...perform aqueous-phase chemistry calculations

      CALL AQCHEM ( JDATE, JTIME, TBARC, PBARC, TAUCLD, PRATE1,
     &              WCBAR, WTBAR, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &              AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, DARK,
     &              FRACTR, FRACPOA, FRACPRI, FRACSOA )

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
	
	  IF(IAER .eq. LTRACER_ACC) THEN
	     IAERI = LTRACER_AKN 
          ELSE
	     IAERI = IAER
          END IF
	  
          IF( SRGAER( IAER )%NAME( IMODE ) .NE. ' ' ) THEN
            DO ISRG = 1, NSRGAER( IAER, IMODE )
              PNTR = CGRID2AQ_MAP( LSRGAER( IAER, IMODE, ISRG ) )
              CEND ( PNTR ) = AEROSOL( IAER, IMODE ) * WSRGAER( IAER, IMODE, ISRG )
              REMOV( PNTR ) = AERWDEP( IAER, IMODE ) * WSRGAER( IAER, IMODE, ISRG )
	      
	      IF( IMODE .EQ. 2 ) THEN
			
                 IF( IAER .EQ. LPOA ) THEN
		    IF( AKNSRG( IAER,ISRG ) .GT. 0 ) THEN
		       CEND ( PNTR ) = (1.d0 - FRACPOA) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACPOA * 
     &                               AEROSOL( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                       REMOV( PNTR ) = (1.d0 - FRACPOA) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACPOA * 
     &                               AERWDEP( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                    ELSE
		    
		       CEND ( PNTR ) = (1.d0 - FRACPOA) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
     
                       REMOV( PNTR ) = (1.d0 - FRACPOA) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
		    END IF
		    
                 ELSE IF ( IAER .EQ. LPRI ) THEN
		    IF(AKNSRG(IAER,ISRG) .GT. 0) THEN
		       CEND ( PNTR ) = (1.d0 - FRACPRI) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACPRI * 
     &                               AEROSOL( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                       REMOV( PNTR ) = (1.d0 - FRACPRI) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACPRI * 
     &                               AERWDEP( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
		    ELSE
		    
		       CEND ( PNTR ) = (1.d0 - FRACPRI) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
     
                       REMOV( PNTR ) = (1.d0 - FRACPRI) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
		    END IF
		   
                 ELSE IF ( IAER .EQ. LTRACER_ACC ) THEN
		    
		    IF(AKNSRG(IAER,ISRG) .GT. 0) THEN
		       CEND ( PNTR ) = (1.d0 - FRACTR) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACTR * 
     &                               AEROSOL( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                       REMOV( PNTR ) = (1.d0 - FRACTR) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACTR * 
     &                               AERWDEP( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                    ELSE
		    
		       CEND ( PNTR ) = (1.d0 - FRACTR) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
     
                       REMOV( PNTR ) = (1.d0 - FRACTR) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
                    END IF
		    
                  ELSE IF ( IAER .EQ. LSOA ) THEN
		    
		    IF(AKNSRG(IAER,ISRG) .GT. 0) THEN
		       CEND ( PNTR ) = (1.d0 - FRACSOA) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACSOA * 
     &                               AEROSOL( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                       REMOV( PNTR ) = (1.d0 - FRACSOA) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) + FRACSOA * 
     &                               AERWDEP( IAER, IMODE ) * WSRGAER( IAERI, 1, AKNSRG(IAER,ISRG) )
     
                    ELSE
		    
		       CEND ( PNTR ) = (1.d0 - FRACSOA) * AEROSOL( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
     
                       REMOV( PNTR ) = (1.d0 - FRACSOA) * AERWDEP( IAER, IMODE ) * 
     &                               WSRGAER( IAER, IMODE, ISRG ) 
                    END IF   		    
                 END IF 		    
	      END IF
	      
            END DO
          END IF 
        END DO
      END DO

C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module

      CALL EXTRACT_AERO ( CEND, .FALSE. )

C *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)
C ... M3NEW is in units of m3/kmol_air
      M3NEW = 0.0
      Dens_wmean_new = 0.0
      DO IMODE = 2, N_MODE

        DO SPC = 1, N_AEROSPC
          IF (  AEROSPC( SPC )%TRACER ) CYCLE 
          IF ( ( AEROSPC( SPC )%NAME( IMODE ) .NE. ' ' ) .AND.
     &         ( .NOT. AEROSPC( SPC )%NO_M2WET ) ) THEN
             M3NEW( IMODE ) = M3NEW( IMODE )
     &                      + ( AEROSPC_CONC( SPC,IMODE ) * AEROSPC_MW( SPC )
     &                      / AEROSPC( SPC )%DENSITY )
             Dens_wmean_new( IMODE ) = Dens_wmean_new( IMODE )  
     &                      + AEROSPC_CONC( SPC,IMODE ) * AEROSPC_MW( SPC )
          END IF
        END DO
      END DO

C...Update aerosol number 
      CEND( AERONUM_MAP( 1 ) ) = MAX( CEND( AERONUM_MAP( 1 ) ), aeromode_minNum( 1 )/ (AIRM/CTHK1) )
      CEND( AERONUM_MAP( 2 ) ) = MAX( CEND( AERONUM_MAP( 2 ) ), aeromode_minNum( 2 )/ (AIRM/CTHK1) )
      CEND( AERONUM_MAP( 3 ) ) = MAX( CEND( AERONUM_MAP( 3 ) ), aeromode_minNum( 3 )/ (AIRM/CTHK1) )

C...check for minimums
C...ug/m3*m3/mol_air*1e3 mol_air/kmol_air*1e-9kg/ug*m3/kg=> m3/kmol_air
      DO IMODE = 2, N_MODE
        IF ( M3NEW( IMODE ) .GT. 0.0D0 ) THEN  ! Dens_wmean_new will be .GT. 0 also
           Dens_wmean_new( IMODE ) = Dens_wmean_new( IMODE ) / M3NEW( IMODE )
           M3OLD( IMODE ) = MAX( M3OLD( IMODE ), CONMIND * 1.0d-6 / ( Dens_wmean_old( IMODE ) * AIRM / CTHK1 ) )
           M3NEW( IMODE ) = MAX( M3NEW( IMODE ), CONMIND * 1.0d-6 / ( Dens_wmean_new( IMODE ) * AIRM / CTHK1 ) )
        END IF
      END DO

C...Update surface area

      CEND( AEROSRF_MAP( 1 ) ) = POLC( AEROSRF_MAP( 1 ) ) * EALFA2T
      CEND( AEROSRF_MAP( 1 ) ) = MAX( CEND( AEROSRF_MAP( 1 ) ), aeromode_minM2( 1 ) * PI / (AIRM/CTHK1) )
      
      CEND( AEROSRF_MAP( 2 ) ) = POLC( AEROSRF_MAP( 2 ) )
     &                         * EXP( -BETASO4 * TAUCLD * ONETHIRD )
     &                         * ( M3NEW( 2 ) / M3OLD( 2 ) ) ** TWOTHIRDS
      CEND( AEROSRF_MAP( 2 ) ) = MAX( CEND( AEROSRF_MAP( 2 ) ), aeromode_minM2( 2 ) * PI / (AIRM/CTHK1) )

      CEND( AEROSRF_MAP( 3 ) ) = POLC( AEROSRF_MAP( 3 ) )
     &                         * ( CEND( AERONUM_MAP( 3 ) )
     &                             / POLC( AERONUM_MAP( 3 ) ) ) ** ONETHIRD
     &                         * ( M3NEW( 3 ) / M3OLD( 3 ) ) ** TWOTHIRDS
      CEND( AEROSRF_MAP( 3 ) ) = MAX( CEND( AEROSRF_MAP( 3 ) ), aeromode_minM2( 3 ) * PI / (AIRM/CTHK1) )

      RETURN
      END

