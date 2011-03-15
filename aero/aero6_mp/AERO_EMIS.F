
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero6_mp/AERO_EMIS.F,v 1.2 2011/03/15 19:16:50 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE AERO_EMIS

C  Emissions data and code required for the modal aerosol module in CMAQ
C  Based on original codes by Dr. Francis S. Binkowski and J. Young

C  Dependent Upon:  NONE

C  Revision History:

C   30 Aug 01 J.Young:  dyn alloc - Use HGRD_DEFN
C   09 Oct 03 J.Gipson: added MW array for AE emis species to module contents
C   31 Jan 05 J.Young:  dyn alloc - establish both horizontal & vertical
C                       domain specifications in one module, GRID_CONF
C   26 Apr 05 P.Bhave:  removed code supporting the "old type" of emission
C                        files that had unspeciated PM10 and PM2.5 only
C                       removed need for 'AERO_SPC.EXT' by declaring the
C                        required variables locally
C   13 Jun 05 P.Bhave:  added vars needed for sea-salt emission processing
C                       inherit N_AE_EMIS,AE_EMIS,AE_EMIS_MAP from AE_EMIS.EXT
C                       moved RHO* parameters from RDEMIS_AE to this module
C                        for use by SSEMIS routine
C   24 Aug 07 J.Young:  Modified to enable in-line plume rise calculation for
C                       3D pt source emissions. Distinguish between PM (primary,
C                       unspeciated, file data) and AE (model speciated). Re-
C                       named RDEMIS_AE to GET_AERO_EMIS.
C   11 Apr 08 J.Kelly:  added code to emit coarse surface area
C    4 Jan 10 J.Young:  restructure; eliminate ref to older AERO versions
C   21 Feb 10 J.Young:  move sea salt emissions to its own module (SSEMIS)
C   23 Apr 10 J.Young:  replace include files with mechanism namelists
C   30 Apr 10 J.Young:  update to use aero_reeng by Steve Howard, Prakash Bhave,
C                       Jeff Young, and Sergey Napelenok
C   23 Jul 10 D.Wong:   remove CLOSE3 and BARRIER
C   24 Feb 11 J.Young:  Reorganized module with initialization and timestepping
C                       procedures
C   25 Feb 11 J.Young:  add wind-blown dust module
C-----------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE

      IMPLICIT NONE

C aerosol emissions: [ppmv/s] for mass & number spcs, [m2/mol/s] for surface area spcs
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_AE( :,:,:,: )

C Variables for converting mass emissions rate to number emissions rate
      REAL,    PRIVATE, SAVE :: FACNUM( N_MODE )

C Variables for converting mass emissions rate to 2nd moment emissions rate
      REAL,    PRIVATE, SAVE :: FACSRF( N_MODE )

C Variables for the thickness and volume of each grid cell
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: CELLHGT( : ) ! grid-cell height [sigma]
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: CELLVOL( : ) ! grid-cell volume [m2*sigma]

C Emission rate of all aerosol species interpolated to current time
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: EMBUFF  ( :,:,:,: ) ! in all grid cells
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: PM_EM   ( : )       ! in one grid cell
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: EM_RATE ( : )       !
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: EM_PART ( :,: )     !
      INTEGER, PRIVATE, ALLOCATABLE, SAVE :: EM_MAP  ( :,: )     !
      INTEGER, PRIVATE, ALLOCATABLE, SAVE :: NUM_MAP ( : )       !
      INTEGER, PRIVATE, ALLOCATABLE, SAVE :: SRF_MAP ( : )       !
      INTEGER, PRIVATE, ALLOCATABLE, SAVE :: SS_MAP  ( : )       !
#ifdef Dust
      INTEGER, PRIVATE, ALLOCATABLE, SAVE :: DUST_MAP( : )       !
#endif

C EMBUFF     mass emission rate input global array
C PM_EM      mass emission rate input localized to one grid cell
C EM_RATE    scaled PM_EM
C EM_PART    particulate mass emission rate in one grid cell
C EM_MAP     particulate mass emission rate index in VDEMIS_AE (CGRID)
C NUM_MAP    particulate number emission rate index in VDEMIS_AE (CGRID)
C SRF_MAP    particulate sfc area emission rate index in VDEMIS_AE (CGRID)
C SS_MAP     sea salt emissions rate index in VDEMIS_AE (CGRID)
C DUST_MAP   wind-blown dust emissions rate index in VDEMIS_AE (CGRID)

C Factor for converting aerosol emissions from input units ...
      REAL,   PRIVATE, ALLOCATABLE, SAVE :: CONVEM_PM( : ) ! into [ug/m2/sec]

C Variables for converting emission rates into molar-mixing-ratio units
      REAL,   PRIVATE, PARAMETER :: GPKG = 1.0E+03     ! g/kg
      REAL,   PRIVATE, PARAMETER :: MGPG = 1.0E+06     ! ug/g

C Domain decomposition info from emission and meteorology files
      INTEGER, PRIVATE, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER, PRIVATE, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C For handling vapor-phase sulfuric acid emissions
      INTEGER, PRIVATE, SAVE :: VSULF        ! index to H2SO4 in VDEMIS array
      INTEGER, PRIVATE, SAVE :: VPSO4_IDX    ! index to PSO4 in EM_RATE array

      INTEGER :: PM_EMLYRS   ! number of vertical layers on emis file

C Miscellaneous variables
#ifdef Dust
      LOGICAL, PRIVATE, SAVE :: WB_DUST   ! flag for wind-blown dust option
#endif
      INTEGER, PRIVATE :: IDX
      CHARACTER( 96 ), PRIVATE :: XMSG = ' '

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION  AERO_EMIS_INIT ( JDATE, JTIME, TSTEP, NEMIS_AE ) RESULT ( SUCCESS)

C  Revision History:

C   30 Aug 01 J.Young:  dynamic allocation - Use INTERPX
C   29 Jul 03 P.Bhave:  added compatibility with emission files that contain
C                       PM10, PEC, POA, PNO3, PSO4, and PMF, but do not
C                       contain PMC
C   20 Aug 03 J.Young:  return aero emissions in molar mixing ratio, ppm units
C   09 Oct 03 J.Gipson: added MW array for AE emis species to module contents
C   01 Sep 04 P.Bhave:  changed MW for primary organics from 120 to 220 g/mol,
C                       to match MWPOA in subroutine ORGAER3.
C   31 Jan 05 J.Young:  dyn alloc - removed HGRD_ID, VGRID_ID, and COORD_ID
C                       include files because those parameters are now
C                       inherited from the GRID_CONF module
C   26 Apr 05 P.Bhave:  removed code supporting the "old type" of emission
C                        files that had unspeciated PM10 and PM2.5 only
C                       removed need for 'AERO_CONST.EXT' by declaring the
C                        required variables locally
C                       simplified the CONVM, CONVN, CONVS calculations
C                       updated and enhanced in-line documentation
C   03 May 05 P.Bhave:  fixed bug in the H2SO4 unit conversion, initially
C                        identified by Jinyou Liang of CARB
C   13 Jun 05 P.Bhave:  calculate sea-salt emissions; execute if MECHNAME = AE4
C                        read input fields from new OCEAN_1 file
C                        read extra input fields from MET_CRO_2D and MET_CRO_3D
C                        write diagnostic sea-salt emission file
C                        added TSTEP to call vector for diagnostic output file
C                       inherit MWs from AE_SPC.EXT instead of hardcoding
C                       find pointers to CGRID indices instead of hardcoding
C   08 Mar 07 P.Bhave&   added capability for emission files that contain
C             S.Roselle:  POC or POA
C   30 Jan 08 P.Bhave:  added compatibility with AE5 mechanisms
C   23 Mar 08 J.Young:  modifications to allow for in-line point source emissions
C   11 Apr 08 J.Kelly:  added code to emit coarse surface area
C   09 Sep 08 P.Bhave:  backward compatibility with AE4 mechanisms
C   20 Feb 10 J.Young:  move ssemis out to its own F90 module
C   24 Feb 11 J.Young:  add wind-blown dust emissions option

C  References:
C    CRC76,        "CRC Handbook of Chemistry and Physics (76th Ed)",
C                   CRC Press, 1995
C    Hobbs, P.V.   "Basic Physical Chemistry for the Atmospheric Sciences",
C                   Cambridge Univ. Press, 206 pp, 1995.
C    Snyder, J.P.  "Map Projections-A Working Manual", U.S. Geological Survey
C                   Paper 1395 U.S.GPO, Washington, DC, 1987.
C    Binkowski & Roselle  Models-3 Community Multiscale Air Quality (CMAQ)
C                   model aerosol component 1: Model Description.
C                   J. Geophys. Res., Vol 108, No D6, 4183
C                   doi:10.1029/2001JD001409, 2003
C-----------------------------------------------------------------------

         USE GRID_CONF           ! horizontal & vertical domain configuration
         USE SSEMIS                               ! sea-salt emissions
#ifdef Dust
         USE DUST_EMIS                            ! wind-blown dust emissions
#endif
         USE AERO_DATA
         USE PRECURSOR_DATA
         USE CGRID_SPCS                           ! CGRID mechanism species

         INCLUDE SUBST_RXCMMN    ! to get mech name
         INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
!        INCLUDE SUBST_IODECL    ! I/O definitions and declarations
#include      SUBST_IODECL      # I/O definitions and declarations
         INCLUDE SUBST_IOFDESC   ! file header data structure
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP( 2 ) ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
         INTEGER, INTENT( IN ) :: NEMIS_AE   ! number of aerosol emissions species
         LOGICAL SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: ENVYN
         INTEGER, EXTERNAL :: INDEX1

C Local Variables:

#ifdef Dust
         CHARACTER( 16 ), SAVE :: CTM_WB_DUST = 'CTM_WB_DUST' ! env var for wind-blown dust
#endif

C Geometric mean diameter by volume (or mass) of emitted particles in
C each mode [ m ].  See paragraph #14 of Binkowski & Roselle (2003)
         REAL, PARAMETER :: DGVEM( N_MODE ) = (/ 0.03E-6, 0.3E-6, 6.0E-6 /)

C Geometric standard deviation of emitted particles in each mode, as
C described in paragraph #14 of Binkowski & Roselle (2003)
         REAL, PARAMETER :: SGEM( N_MODE ) = (/ 1.7, 2.0, 2.2 /)

C Variables for calculating the volume of each grid cell
         REAL  DX1, DX2                            ! grid-cell width and length [m]
         REAL  CELLAREA                            ! grid-cell area [m2]

C Factor for converting aerosol emissions from input units ...
         REAL CONVEM_PM_MASS                       ! into [ug/sec]

C Domain decomposition info from emission and meteorology files
         INTEGER GXOFF, GYOFF          ! origin offset

C Miscellaneous variables
         INTEGER, SAVE     :: LOGDEV
         INTEGER STATUS                   ! ENV..., ALLOCATE status
         CHARACTER( 16 ), SAVE :: PNAME = 'AERO_EMIS_INIT'
         CHARACTER( 16 ) :: VNAME         ! temp var for species names
         INTEGER L, N, S                  ! Loop indices

C ----------------------------------------------------------------------

         LOGDEV = INIT3()

C *** set up for sea-salt emission processing
         IF ( .NOT. SSEMIS_INIT( JDATE, JTIME, TSTEP( 1 ) ) ) THEN
            XMSG = 'Failure initializing sea-salt emission processing'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

#ifdef Dust
C *** wind-blown dust emissions?
         WB_DUST = ENVYN( 'CTM_WB_DUST', 'Flag for wind-blown dust emissions',
     &                   .TRUE., STATUS )

         IF ( WB_DUST ) THEN
C *** set up for dust emission processing
            IF ( .NOT. DUST_EMIS_INIT( JDATE, JTIME, TSTEP( 1 ) ) ) THEN
               XMSG = 'Failure initializing dust emission processing'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
         END IF
#endif

         IF ( NEMIS_AE .GT. 0 ) THEN   ! all EMLAYS
            ALLOCATE ( VDEMIS_AE( NEMIS_AE,PM_EMLYRS,MY_NCOLS,MY_NROWS ),
     &                 STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'VDEMIS_AE memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            VDEMIS_AE = 0.0   ! array assignment
         END IF

C *** Calculate factors for converting 3rd moment emission rates into number
C     emission rates.  See Equation 7b of Binkowski & Roselle (2003)
         DO N = 1, N_MODE
            FACNUM( N )  = EXP( 4.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N ) ** 3
         END DO

C *** Calculate factors for converting 3rd moment emission rates into 2nd
C     moment emission rates.  See Equation 7c of Binkowski & Roselle (2003)
         DO N = 1, N_MODE
!           FACTM2( N )  = EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
            FACSRF( N )  = PI * EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
         END DO

C *** Find location of sulfuric acid vapor in VDEMIS array
         VNAME = 'SULF'
         IDX = INDEX1 ( VNAME, N_GC_EMIS, GC_EMIS )
         IF ( IDX .NE. 0 ) THEN
            VSULF = IDX  ! index for vapor-phase H2SO4 emissions
         ELSE
            XMSG = 'Could not find ' // VNAME // 'in gas table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

C *** Allocate memory for PM_EM, EMBUFF, CELLHGT, CELLVOL, and CONVEM_PM
         ALLOCATE ( PM_EM( N_EMIS_PM ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** PM_EM memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( EMBUFF( NCOLS,NROWS,PM_EMLYRS,N_EMIS_PM ),
     &              STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** EMBUFF memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CONVEM_PM( PM_EMLYRS ),
     &              CELLHGT( PM_EMLYRS ),
     &              CELLVOL( PM_EMLYRS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** CONVEM_PM, CELLHGT or CELLVOL memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C *** Allocate memory for EM_RATE, EM_PART, EM_MAP, NUM_MAP, SRF_MAP, SS_MAP and DUST_MAP
         ALLOCATE ( EM_RATE( N_EMIS_PM ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** EM_RATE memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( EM_PART( N_AEROSPC, N_MODE ),
     &              EM_MAP ( N_AEROSPC, N_MODE ),
     &              NUM_MAP(            N_MODE ),
     &              SRF_MAP(            N_MODE ),
#ifdef Dust
     &              SS_MAP  ( N_AEROSPC ),
     &              DUST_MAP( N_AEROSPC ), STAT = STATUS )
#else
     &              SS_MAP ( N_AEROSPC ), STAT = STATUS )
#endif
         IF ( STATUS .NE. 0 ) THEN
#ifdef Dust
            XMSG = '*** memory allocation failed for '
     &           // 'EM_PART, EM_MAP, NUM_MAP, SRF_MAP, SS_MAP or DUST_MAP'
#else
            XMSG = '*** memory allocation failed for '
     &           // 'EM_PART, EM_MAP, NUM_MAP, SRF_MAP, or SS_MAP'
#endif
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C *** Map indices of emission species
         EM_MAP = 0   ! array assignment
         DO S = 1, N_AEROSPC
            IF ( AEROSPC( S )%EMIS .NE. ' ' ) THEN
               DO N = 1, N_MODE
                  IF ( AEROSPC( S )%NAME( N ) .NE. ' ' ) THEN
                     EM_MAP( S,N ) = INDEX1( AEROSPC( S )%NAME( N ), N_AE_EMIS, AE_EMIS )
                  END IF
               END DO
            END IF
         END DO

C *** Map aerosol number emission species
         NUM_MAP = 0
         DO N = 1, N_MODE
            NUM_MAP( N ) = INDEX1( AEROMODE( N )%NUM_NAME, N_AE_EMIS, AE_EMIS )
            IF ( NUM_MAP( N ) .EQ. 0 ) THEN
               XMSG = 'Could not find ' // TRIM( AEROMODE( N )%NUM_NAME )
     &              // ' in AE_EMIS table'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
         END DO

C *** Map aerosol surface emission species
         SRF_MAP = 0
         DO N = 1, N_MODE
            SRF_MAP( N ) = INDEX1( AEROMODE( N )%SRF_NAME, N_AE_EMIS, AE_EMIS )
            IF ( SRF_MAP( N ) .EQ. 0 ) THEN
               XMSG = 'Could not find ' // TRIM( AEROMODE( N )%SRF_NAME )
     &              // ' in AE_EMIS table'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
         END DO

C *** Map sea salt emission species
         SS_MAP = 0
         DO S = 1, NSSSPC
            IDX = FINDAERO( WRSS_SPC( S ), .FALSE. )
            IF ( IDX .NE. 0 ) THEN
               SS_MAP( IDX ) = S
               DO N = 1, N_MODE   ! add SEASALT indices to EM_MAP
                  IF ( AEROSPC( IDX )%NAME( N ) .NE. ' ' ) THEN
                     EM_MAP( IDX,N ) = INDEX1( AEROSPC( IDX )%NAME( N ), N_AE_EMIS, AE_EMIS )
                  END IF
               END DO
            END IF
         END DO

#ifdef Dust
         IF ( WB_DUST ) THEN
C *** Map wind-blown dust emission species
         DUST_MAP = 0
            DO S = 1, NDUST_SPC
               IDX = FINDAERO( DUST_SPC( S )%NAME, .FALSE. )
               IF ( IDX .NE. 0 ) THEN
                  DUST_MAP( IDX ) = S
                  DO N = 1, N_MODE   ! add dust_map indices to EM_MAP
                     IF ( AEROSPC( IDX )%NAME( N ) .NE. ' ' ) THEN
                        EM_MAP( IDX,N ) = INDEX1( AEROSPC( IDX )%NAME( N ), N_AE_EMIS, AE_EMIS )
                     END IF
                  END DO
               END IF
            END DO
         END IF
#endif

C *** Map data modules
         CALL MAP_PRECURSOR()
         CALL MAP_AERO()

C *** find index of SO4 emissions
         VPSO4_IDX = 0
         DO S = 1, N_EMIS_PM
            IF ( PMEM_MAP( S ) .EQ. ASO4_IDX ) VPSO4_IDX = S
         END DO
         IF ( VPSO4_IDX .EQ. 0 ) THEN
            XMSG = 'Cannot define index for SO4 emissions: '
     &           // 'aerospc%emis='' '' in AERO_DATA.F'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

C *** Get length and width of each grid cell
C     note: crude estimate is made for LAT/LONG coordinate systems
         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD
     &          * FLOAT( GL_NROWS/2 ) ) ) ! in m
         ELSE
            DX1 = XCELL_GD        ! in m
            DX2 = YCELL_GD        ! in m
         END IF

C *** Get height of grid cell in each layer in sigma coordinates
C     Multiply by grid area [m2] to obtain grid volume
         CELLAREA = DX1 * DX2
         DO L = 1, PM_EMLYRS
            CELLHGT( L ) = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            CELLVOL( L ) = CELLHGT( L ) * CELLAREA
         END DO

C *** Get scaling factor for converting aerosol emissions from
C     their input units to [ug/s] and then to [ug/m2/s] using layer-
C     specific grid-cell volume.  Calling DESC3( EMIS_1 ) assumes EMIS_1
C     already opened.

         IF ( .NOT. DESC3( EMIS_1 ) ) THEN
            XMSG = 'Could not get '// EMIS_1 // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         IF ( PMEM_UNITS .EQ. 'G/S' .OR.
     &        PMEM_UNITS .EQ. 'g/s' ) THEN
            CONVEM_PM_MASS = MGPG                  ! (g/s) -> (ug/s)
         ELSE IF ( PMEM_UNITS .EQ. 'KG/HR' .OR.
     &             PMEM_UNITS .EQ. 'kg/hr' ) THEN
            CONVEM_PM_MASS = GPKG * MGPG / 3600.0  ! (kg/hr) -> (ug/s)
         ELSE
            XMSG = 'Units incorrect on ' // EMIS_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         DO L = 1, PM_EMLYRS
            CONVEM_PM( L ) = CONVEM_PM_MASS / CELLVOL( L )
         END DO

C *** Get domain decomposition info from input files

         CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF,
     &                   STARTCOL, ENDCOL, STARTROW, ENDROW )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         SUCCESS = .TRUE.; RETURN

         END FUNCTION  AERO_EMIS_INIT

C-----------------------------------------------------------------------

         SUBROUTINE GET_AERO_EMIS ( JDATE, JTIME, TSTEP, RJACM, VDEMIS )

C  GET_AERO_EMIS reads PM emissions from gridded input file and
C  optionally from plume rise calculated point source emissions (via VDEMIS
C  array) and converts into molar-mixing-ratio units, for use in the vertical
C  diffusion routines

C  Key Subroutines/Functions Called: SSEMIS_INIT, GET_SSEMIS, DUST_INIT, GET_DUST_EMIS

C ----------------------------------------------------------------------
         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE PT3D_DEFN, ONLY: PT3DEMIS, PMEMIS_PT ! from plume rise
         USE PTMAP, ONLY: N_SPC_PTPM, PTPM_MAP    ! defines emissions file(s) PM mapping
         USE AERO_DATA
         USE PRECURSOR_DATA
         USE SSEMIS                               ! sea-salt emissions
#ifdef Dust
         USE DUST_EMIS                            ! wind-blown dust emissions
#endif

         INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
!        INCLUDE SUBST_IODECL    ! I/O definitions and declarations
#include      SUBST_IODECL      # I/O definitions and declarations
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP( 2 ) ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
         REAL,    INTENT( IN ) :: RJACM( NCOLS,NROWS,NLAYS ) ! recip of mid-layer
                                                             ! Jacobian [1/m]
         REAL,    INTENT( INOUT ) :: VDEMIS( :,:,:,: ) ! gas emissions [ppmv/s]

C Geometric Constants
         REAL, PARAMETER :: F6DPI = 6.0 / PI
         REAL, PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI

C Local variables for sea salt emission rates (grid cell value)
         REAL :: CR_SSM( N_AEROSPC,NSSMODE )   ! mass
         REAL :: CR_SSN( NSSMODE )             ! number
         REAL :: CR_SSS( NSSMODE )             ! surface area

#ifdef Dust
C Local variables for wind-blown dust emission rates (grid cell value)
         REAL :: CR_DUSTM( N_AEROSPC,NDUST_MODE )   ! mass
         REAL :: CR_DUSTN( NDUST_MODE )             ! number
         REAL :: CR_DUSTS( NDUST_MODE )             ! surface area
#endif

         REAL( 8 ) :: GSFAC                    ! into [ug/m3/sec]

C Variables interpolated from the meteorological input files
         REAL DENS( NCOLS,NROWS,NLAYS )        ! air density [kg/m3]

C Variables for converting emission rates into molar-mixing-ratio units
         REAL, PARAMETER :: RAVO = 1.0 / AVO   ! reciprocal of Avogadro
         REAL( 8 ) :: CONVM     ! conversion factor for mass emissions [m3/mol]
         REAL( 8 ) :: CONVN     ! conversion factor for number emissions [1e6*m3]
         REAL( 8 ) :: CONVS     ! conversion factor for surface area emissions [m3/mol]

C Third moment emissions rates [m3/m3/s]
         REAL( 8 ) :: EMISM3( N_MODE )

C Number emissions rates [1/m3/s]
         REAL( 8 ) :: EM_NUM( N_MODE )

C Surface area emission rates [m2/m3/s]
         REAL( 8 ) :: EM_SRF( N_MODE )

C Miscellaneous variables
         INTEGER, EXTERNAL :: TRIMLEN
         CHARACTER( 16 ), SAVE :: PNAME = 'GET_AERO_EMIS'
         INTEGER C, R, L, N, V, S         ! Loop indices
         REAL( 8 ) :: FINETOT
#ifdef Verbose
         real sumem
#endif

C ----------------------------------------------------------------------

C *** Read aerosol emission rates from file and interpolate to the current
C     time.  Store result in EMBUFF array.

         IF ( PT3DEMIS ) THEN   ! in-line plume rise

            EMBUFF = 0.0   ! array assignment

            DO S = 1, N_EMIS_PM   ! read 2d file
               IDX = PMEM_MAP( S )
               IF ( .NOT. INTERPX( EMIS_1, AEROSPC( IDX )%EMIS, PNAME,
     &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,1,
     &                             JDATE, JTIME, EMBUFF( 1,1,1,S ) ) ) THEN
                  XMSG = 'Could not read '
     &                 // TRIM( AEROSPC( IDX )%EMIS )
     &                 // ' from ' // EMIS_1
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG  )
               END IF
            END DO

            DO S = 1, N_SPC_PTPM   ! merge inline point src PM species
               V = PTPM_MAP( S )   ! set in PT3D_DEFN
               DO L = 1, PM_EMLYRS
                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                     EMBUFF( C,R,L,S ) = EMBUFF( C,R,L,S ) + PMEMIS_PT( C,R,L,V )
                     END DO
                  END DO
               END DO
            END DO

         ELSE   ! read 3d file

            DO S = 1, N_EMIS_PM
               IDX = PMEM_MAP( S )
               IF ( .NOT. INTERPX( EMIS_1, AEROSPC( IDX )%EMIS, PNAME,
     &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,PM_EMLYRS,
     &                             JDATE, JTIME, EMBUFF( 1,1,1,S ) ) ) THEN
                  XMSG = 'Could not read '
     &                 // TRIM( AEROSPC( IDX )%EMIS )
     &                 // ' from ' // EMIS_1
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG  )
               END IF
            END DO

         END IF

C *** Read air density [kg/m3], atmospheric pressure [Pa], air temperature
C     [K], specific humidity [kg H2O / kg air], and 10m wind speed [m/s]
C     from meteorology file.  Interpolate to the current time.  Store results
C     in DENS, PRES, TA, QV, and WSPD10 arrays.
         IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                       1,NLAYS, JDATE, JTIME, DENS ) ) THEN
            XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C *** Get sea salt emissions
         CALL GET_SSEMIS ( JDATE, JTIME, TSTEP,
     &                     CELLVOL( 1 ), RJACM( :,:,1 ), CELLHGT( 1 ) )

#ifdef Dust
         IF ( WB_DUST ) THEN
C *** Get dust emissions
            CALL GET_DUST_EMIS ( JDATE, JTIME )
         END IF
#endif

C *** Loop over each grid cell in layer 1

         L = 1
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

C *** Store aerosol emission rates for this grid cell in a 1D array
               DO S = 1, N_EMIS_PM
                  PM_EM( S ) = EMBUFF( C,R,L,S )
               END DO

C *** Get grid cell values for sea salt emission rates
C     Convert units of SSOUTM from [g/m3/s] to [ug/m3/s].
               CR_SSM = 0.0   ! array assignment
               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     IF ( SS_MAP( S ) .NE. 0 )
     &                  CR_SSM( S,N ) = SSOUTM( SS_MAP( S ),N,C,R ) * MGPG
                  END DO
                  CR_SSN( N ) = SSOUTN( N,C,R )
                  CR_SSS( N ) = SSOUTS( N,C,R )
               END DO

#ifdef Dust
               IF ( WB_DUST ) THEN
C *** Get grid cell values for wind-blown dust emission rates
C     Convert units of DUSTOUTM from [g/m3/s] to [ug/m3/s]. g/s -> ug/s ???
                  CR_DUSTM = 0.0   ! array assignment
                  DO N = 1, N_MODE
                     DO S = 1, N_AEROSPC
                        IF ( DUST_MAP( S ) .NE. 0 )
     &                     CR_DUSTM( S,N ) = DUSTOUTM( DUST_MAP( S ),N,C,R ) * MGPG
                     END DO
                     CR_DUSTN( N ) = DUSTOUTN( N,C,R )
                     CR_DUSTS( N ) = DUSTOUTS( N,C,R )
                  END DO
               END IF
#endif

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
               GSFAC = CONVEM_PM( L ) * RJACM( C,R,L )
               CONVM = MWAIR / GPKG / DENS( C,R,L )   ! [m3/mol]
               CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
               CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
               DO S = 1, N_EMIS_PM
                  EM_RATE( S ) = PM_EM( S ) * GSFAC
               END DO

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
               EM_RATE( VPSO4_IDX ) = EM_RATE( VPSO4_IDX ) + VDEMIS( VSULF,L,C,R )
     &                              * PRECURSOR_MW( SULF_IDX ) / CONVM
               VDEMIS( VSULF,L,C,R ) = 0.0

C *** Assign fine-particle emission rates to the fine modes.
C     Assume all non-seasalt-emissions of sulfate, nitrate, and unspeciated
C     fine PM are in the accumulation mode.  Split the carbon emissions
C     between the Aitken and accumulation modes, as described in paragraph
C     #12 of Binkowski & Roselle (2003).

C *** Calculate emissions rate for third moments [m3/m3/s] of each mode
C     (excluding sea salt), as in Equation 7a of Binkowski & Roselle (2003).

               EM_PART = 0.0   ! array assignment
               EMISM3 = 0.0    ! array assignment
               DO S = 1, N_EMIS_PM
                  IDX = PMEM_MAP( S )
                  DO N = 1, N_MODE
                     EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N )
     &                                * EM_RATE( S )
                     IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                        EMISM3( N ) = EMISM3( N ) + EM_PART( IDX,N )
     &                              * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                     END IF
                  END DO
               END DO

C *** Calculate the number emissions rate for each mode [1/m3/s], using
C     Equation 7b of Binkowski & Roselle (2003).
               DO N = 1, N_MODE
                  EM_NUM( N ) = FACNUM( N ) * EMISM3( N )
               END DO

C *** Calculate the surface area emissions rate for the fine modes [m2/m3/s],
C     using Equation 7c of Binkowski & Roselle (2003).  Multiplying by PI
C     converts 2nd moment to surface area.
               DO N = 1, N_MODE
                  EM_SRF( N ) = FACSRF( N ) * EMISM3( N )
               END DO

C *** Units of CR_SSM are ug/m3/s].  Transfer Sea-salt mass emissions
C     to mode- and species-specific variables.  Sea-salt sulfate in the fine
C     modes must be added to the anthropogenic sulfate emissions.  Remaining
C     sea-salt species are not in the standard inventory.
               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     EM_PART( S,N ) = EM_PART( S,N ) + CR_SSM( S,N )
                  END DO
               END DO

C *** Add sea salt to the total emissions of particle number and surface area
               DO N = 1, N_MODE
                  EM_NUM( N ) = EM_NUM( N ) + CR_SSN( N )
                  EM_SRF( N ) = EM_SRF( N ) + CR_SSS( N )
               END DO

#ifdef Dust
               IF ( WB_DUST ) THEN
C *** Units of CR_DUSTM are ug/m3/s].  Transfer wind-blown dust mass emissions
C     to mode- and species-specific variables.  wind-blown dust sulfate in the fine
C     modes must be added to the anthropogenic sulfate emissions.  Remaining
C     wind-blown dust species are not in the standard inventory.
                  DO N = 1, N_MODE
                     DO S = 1, N_AEROSPC
                        EM_PART( S,N ) = EM_PART( S,N ) + CR_DUSTM( S,N )
                     END DO
                  END DO

C *** Add wind-blown dust to the total emissions of particle number and surface area
                  DO N = 1, N_MODE
                     EM_NUM( N ) = EM_NUM( N ) + CR_DUSTN( N )
                     EM_SRF( N ) = EM_SRF( N ) + CR_DUSTS( N )
                  END DO
               END IF
#endif

C *** Convert emission rates into molar-mixing-ratio units, as required by
C     the vertical diffusion routines.  Mass and number emissions are
C     converted to [ppmv/s].  Surface area emissions are converted to
C     [m2/mol/s].  Save results in the VDEMIS_AE array.

               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     IF ( EM_MAP( S,N ) .GT. 0 ) THEN
                        VDEMIS_AE( EM_MAP( S,N ),L,C,R ) = EM_PART( S,N )
     &                                                   * ( CONVM / AEROSPC_MW( S ) )
                     END IF
                  END DO
                     VDEMIS_AE( NUM_MAP( N ),L,C,R ) = EM_NUM( N ) * CONVN
                     VDEMIS_AE( SRF_MAP( N ),L,C,R ) = EM_SRF( N ) * CONVS
               END DO

            END DO   ! loop on MY_NCOLS
         END DO   ! loop on MY_NROWS

         DO L = 2, PM_EMLYRS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS

C *** Store aerosol emission rates for this grid cell in a 1D array
                  DO S = 1, N_EMIS_PM
                     PM_EM( S ) = EMBUFF( C,R,L,S )
                  END DO

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
                  GSFAC = CONVEM_PM( L ) * RJACM( C,R,L )
                  CONVM = MWAIR / GPKG / DENS( C,R,L )   ! [m3/mol]
                  CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
                  CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
                  DO S = 1, N_EMIS_PM
                     EM_RATE( S ) = PM_EM( S ) * GSFAC
                  END DO

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
                  EM_RATE( VPSO4_IDX ) = EM_RATE( VPSO4_IDX ) + VDEMIS( VSULF,L,C,R )
     &                                 * PRECURSOR_MW( SULF_IDX ) / CONVM
                  VDEMIS( VSULF,L,C,R ) = 0.0

C *** Calculate emissions rate for third moments [m3/m3/s] of each mode
C     (excluding sea salt), as in Equation 7a of Binkowski & Roselle (2003).

                  EM_PART = 0.0   ! array assignment
                  EMISM3 = 0.0    ! array assignment
                  DO S = 1, N_EMIS_PM
                     IDX = PMEM_MAP( S )
                     DO N = 1, N_MODE
                        EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N )
     &                                   * EM_RATE( S )
                        IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                           EMISM3( N ) = EMISM3( N ) + EM_PART( IDX,N )
     &                                 * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                        END IF
                     END DO
                  END DO

C *** Calculate the number emissions rate for each mode [1/m3/s], using
C     Equation 7b of Binkowski & Roselle (2003).
                  DO N = 1, N_MODE
                     EM_NUM( N ) = FACNUM( N ) * EMISM3( N )
                  END DO

C *** Calculate the surface area emissions rate for the fine modes [m2/m3/s],
C     using Equation 7c of Binkowski & Roselle (2003).  Multiplying by PI
C     converts 2nd moment to surface area.
                  DO N = 1, N_MODE
                     EM_SRF( N ) = FACSRF( N ) * EMISM3( N )
                  END DO

C *** Convert emission rates into molar-mixing-ratio units, as required by
C     the vertical diffusion routines.  Mass and number emissions are
C     converted to [ppmv/s].  Surface area emissions are converted to
C     [m2/mol/s].  Save results in the VDEMIS_AE array.

                  DO N = 1, N_MODE
                     DO S = 1, N_AEROSPC
                        IF ( EM_MAP( S,N ) .GT. 0 ) THEN
                           VDEMIS_AE( EM_MAP( S,N ),L,C,R ) = EM_PART( S,N )
     &                                                      * CONVM / AEROSPC_MW( S )
                        END IF
                     END DO
                        VDEMIS_AE( NUM_MAP( N ),L,C,R ) = EM_NUM( N ) * CONVN
                        VDEMIS_AE( SRF_MAP( N ),L,C,R ) = EM_SRF( N ) * CONVS
                  END DO

               END DO   ! loop on MY_NCOLS
            END DO   ! loop on MY_NROWS
         END DO   ! loop on PM_EMLYRS

#ifdef Verbose
         write( logdev,'(/"@@@",i8,i7.6)') jdate, jtime
         do s = 1, n_ae_emis
            sumem = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumem = sumem + vdemis_ae( s,1,c,r )
               end do
            end do
            write( logdev,'("@@@ ",a,g16.6)') ae_emis( s ), sumem
         end do
#endif

         RETURN

         END SUBROUTINE GET_AERO_EMIS

      END MODULE AERO_EMIS

