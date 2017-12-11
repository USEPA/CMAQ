
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/AERO_EMIS.F,v 1.11 2012/01/19 13:20:08 yoj Exp $

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
C   25 Feb 11 J.Young:  add windblown dust module
C   25 Mar 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   18 Aug 11 David Wong: In the merge inline point source PM species calculation,
C                         arrays EMBUFF and PMEMIS_PT have incorrect index values
C   17 Apr 13 J.Young: replace "SPFC ASO4" (found by Havala Pye) with "SPFC_ASO4"
C   07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C-----------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE
      USE EMIS_GLOBAL

      IMPLICIT NONE
      
C aerosol emissions: [ppmv/s] for mass & number spcs, [m2/mol/s] for surface area spcs
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_AE( :,:,:,: )
      CHARACTER( 16 ),      SAVE :: CTM_WB_DUST = 'CTM_WB_DUST' ! env var for windblown dust
      LOGICAL,              SAVE :: WB_DUST   ! flag for windblown dust option

      PUBLIC EMISS_SIZE_DIST, VDEMIS_AE, AERO_EMIS_INIT, GET_AERO_EMIS,
     &       MAP_AEROtoEM, MAP_EMtoAERO, MAP_EMtoMODE, MAP_NUMtoEM, MAP_SRFtoEM,
     &       MAP_EMtoNUM, MAP_EMtoSRF,
     &       CTM_WB_DUST, WB_DUST
      PRIVATE

C Variables for converting mass emissions rate to number emissions rate
      REAL,    SAVE :: FACNUM( N_MODE )

C Variables for converting mass emissions rate to 2nd moment emissions rate
      REAL,    SAVE :: FACSRF( N_MODE )

C Variables for the thickness and volume of each grid cell
      REAL,    ALLOCATABLE, SAVE :: CELLHGT( : ) ! grid-cell height [sigma]
      REAL,    ALLOCATABLE, SAVE :: CELLVOL( : ) ! grid-cell volume [m2*sigma]
 
C Emission rate of all aerosol species interpolated to current time
      INTEGER, ALLOCATABLE, SAVE :: MAP_AEROtoEM( :,: )   !
      INTEGER, ALLOCATABLE, SAVE :: MAP_EMtoAERO( : )   !
      INTEGER, ALLOCATABLE, SAVE :: MAP_EMtoMODE( : )   !
      INTEGER, ALLOCATABLE, SAVE :: MAP_NUMtoEM ( : )  
      INTEGER, ALLOCATABLE, SAVE :: MAP_SRFtoEM ( : )   !
      INTEGER, ALLOCATABLE, SAVE :: MAP_EMtoNUM ( : )  
      INTEGER, ALLOCATABLE, SAVE :: MAP_EMtoSRF ( : )   !

C DUST_MAP   windblown dust emission rate index in DUSTOUTM

C Factor for converting aerosol emissions from input units ...
      REAL,    ALLOCATABLE, SAVE :: CONVEM_PM( : ) ! into [ug/m2/sec]

C *** PVB: temporary hack to speciate PMC
      REAL,    SAVE :: SPFC_ACORS
      REAL,    SAVE :: SPFC_ASO4
      REAL,    SAVE :: SPFC_ANO3
      REAL,    SAVE :: SPFC_ACL
      REAL,    SAVE :: SPFC_AH2O

C Variables for converting emission rates into molar-mixing-ratio units
      REAL,    PARAMETER :: GPKG = 1.0E+03     ! g/kg
      REAL,    PARAMETER :: MGPG = 1.0E+06     ! ug/g

C Domain decomposition info from emission and meteorology files
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C Miscellaneous variables
      INTEGER :: IDX
      CHARACTER( 200 ) :: XMSG = ' '
      INTEGER, SAVE :: LOGDEV
      INTEGER, EXTERNAL :: SETUP_LOGDEV

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION  AERO_EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS)

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
C   24 Feb 11 J.Young:  add windblown dust emissions option
C   25 Mar 11 S.Roselle: Replaced I/O API include files with UTILIO_DEFN
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C   17 Sep 14 K.Fahey:  Changed geometric mean diameter and geometric
C                       standard deviation of emitted particles according to 
C                       Elleman and Covert (2010)
C   15 Apr 16 J.Young: Use aerosol factors from the AERO_DATA module's named constants;
C                      Moved K.Fahey's mods to geometric mean diameter and standard
C                      deviation to the AERO_DATA module
 
C  References:
C    CRC76,        "CRC Handbook of Chemistry and Physics (76th Ed)",
C                   CRC Press, 1995
C    Elleman & Covert, "Aerosol size distribution modeling with the Community
C                   Multiscale Air Quality modeling system in the Pacific
C                   Northwest: 3. Size distribution of particles emitted
C                   into a mesoscale model", J. Geophys. Res., Vol 115,
C                   No D3, doi:10.1029/2009JD012401, 2010
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
         USE RXNS_DATA           ! chemical mechanism data
         USE CGRID_SPCS          ! CGRID mechanism species
         USE SSEMIS              ! sea-salt emissions
         USE DUST_EMIS           ! wind-blown dust emissions
         USE AERO_DATA
         USE PRECURSOR_DATA
         USE UTILIO_DEFN
     
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP      ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
         LOGICAL SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: FINDEX       !  looks up number in table.

C Local Variables:
         LOGICAL, SAVE :: ZERO_PCSOA
         CHARACTER( 16 ), SAVE :: CTM_ZERO_PCSOA = 'CTM_ZERO_PCSOA' ! env var for toggling
                                                                    ! pcSOA modeling

C Variables for calculating the volume of each grid cell
         REAL  DX1, DX2                            ! grid-cell width and length [m]
         REAL  CELLAREA                            ! grid-cell area [m2]

C Factor for converting aerosol emissions from input units ...
         REAL CONVEM_PM_MASS                       ! into [ug/sec]

C Domain decomposition info from emission and meteorology files
         INTEGER GXOFF, GYOFF          ! origin offset

C Miscellaneous variables
         INTEGER STATUS                   ! ENV..., ALLOCATE status
         CHARACTER( 16 ), SAVE :: PNAME = 'AERO_EMIS_INIT  '
         CHARACTER( 16 ) :: VNAME         ! temp var for species names
         CHARACTER( 50 ) :: VARDESC       ! variable for reading environ. variables
         INTEGER L, N, S, V, IPOA         ! Loop indices

C ----------------------------------------------------------------------

         LOGDEV = SETUP_LOGDEV()
         SUCCESS = .TRUE.

C *** Map data modules
         CALL MAP_AERO()
         CALL MAP_PRECURSOR()

C *** set up for sea-salt emission processing
         IF ( .NOT. SSEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing sea-salt emission processing'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C *** set up for dust emission processing
         IF ( WB_DUST ) THEN
            IF ( .NOT. DUST_EMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
               XMSG = 'Failure initializing dust emission processing'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
         END IF

         IF ( N_AE_EMIS .GT. 0 ) THEN   ! all EMLAYS
            ALLOCATE ( VDEMIS_AE( N_EMIS_SPC,EMLAYS,MY_NCOLS,MY_NROWS ),
     &                 STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'VDEMIS_AE memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.
            ELSE
               VDEMIS_AE = 0.0   ! array assignment
            END IF
         END IF

C *** Calculate factors for converting 3rd moment emission rates into number
C     emission rates.  See Equation 7b of Binkowski & Roselle (2003)
         DO N = 1, N_MODE
            FACNUM( N ) = EXP( 4.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N ) ** 3
         END DO

C *** Calculate factors for converting 3rd moment emission rates into 2nd 
C     moment emission rates.  See Equation 7c of Binkowski & Roselle (2003)
         DO N = 1, N_MODE
            FACSRF( N ) = PI * EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
         END DO

C *** Allocate memory for EMBUFF, CELLHGT, CELLVOL, and CONVEM_PM
         ALLOCATE ( CONVEM_PM( EMLAYS ),
     &              CELLHGT( EMLAYS ),
     &              CELLVOL( EMLAYS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** CONVEM_PM, CELLHGT or CELLVOL memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( MAP_AEROtoEM( N_AEROSPC, N_MODE ),
     &              MAP_NUMtoEM ( N_MODE ),
     &              MAP_SRFtoEM ( N_MODE ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** memory allocation failed for '
     &           // 'NUM_MAP, NUM_FAC, SRF_MAP, SRF_FAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C *** Semivolatile POA  
         IF ( INDEX( MECHNAME,"NVPOA" ) .EQ. 0 ) THEN
           ! Let the user set the pcVOC species (which makes pcSOA) emissions
           ! scaling to 0.0
           ! Retrieve the Environment Variable allowing the User to Toggle
           ! the pcVOC emissions
           ZERO_PCSOA = .FALSE.         ! default
           VARDESC = 'Flag for Ignoring the pcSOA species'
           ZERO_PCSOA = ENVYN( CTM_ZERO_PCSOA, VARDESC, ZERO_PCSOA, STATUS )

                        
           V = INDEX1( 'PCVOC', N_GC_SPC, GC_SPC )
           IF ( V .LT. 1 ) THEN
             WRITE(LOGDEV,99901)'PCVOC','PCVOC',TRIM( MECHNAME )
             SUCCESS = .FALSE.
           END IF
           IF ( ZERO_PCSOA ) THEN
             ! Find the index for the pcVOC species in the emissions
             ! scale-factor vector
             IF ( V .GT. 0 ) THEN
                S = FINDEX( V, N_GC_EMIS, GC_EMIS_MAP )
                IF ( S .GT. 0 )THEN
                   GC_EMIS_FAC( S ) = 0.0
                ELSE
                  WRITE(LOGDEV,99902)'PCVOC'
!                  SUCCESS = .FALSE.
                END IF
             END IF
           END IF
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
         DO L = 1, EMLAYS
            CELLHGT( L ) = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            CELLVOL( L ) = CELLHGT( L ) * CELLAREA
         END DO

C *** Get scaling factor for converting aerosol emissions from
C     their input units to [ug/s] and then to [ug/m2/s] using layer-
C     specific grid-cell volume.  
         write( LOGDEV, '(A,A)'),'PMEM_UNITS=',PMEM_UNITS

         IF ( PMEM_UNITS .EQ. 'G/S' .OR.
     &        PMEM_UNITS .EQ. 'g/s' ) THEN
            CONVEM_PM_MASS = MGPG                  ! (g/s) -> (ug/s)
         ELSE IF ( PMEM_UNITS .EQ. 'KG/HR' .OR.
     &             PMEM_UNITS .EQ. 'kg/hr' ) THEN
            CONVEM_PM_MASS = GPKG * MGPG / 3600.0  ! (kg/hr) -> (ug/s)
         ELSE
            XMSG = PMEM_UNITS
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.
         END IF

         DO L = 1, EMLAYS
            CONVEM_PM( L ) = CONVEM_PM_MASS / CELLVOL( L )
         END DO

99901     FORMAT(/'FATAL: GC namelist used does not include species, ', A, 
     &         / 'Check this namelist contains the same information '
     &         / 'for ', A,' as the repository version of the GC namelist for '
     &         / 'the ', A, ' mechanism.' )
99902     FORMAT(/'WARNING: GC namelist line for ', A, ' does not include an emissions '
     &         / 'surrogate. Simulation cannot evaluate the effect from zeroing out its '
     &         / 'emissions.')

         END FUNCTION  AERO_EMIS_INIT

C-----------------------------------------------------------------------

         SUBROUTINE GET_AERO_EMIS ( JDATE, JTIME, TSTEP )

C  GET_AERO_EMIS reads PM emissions from gridded input file and
C  optionally from plume rise calculated point source emissions (via VDEMIS
C  array) and converts into molar-mixing-ratio units, for use in the vertical
C  diffusion routines
 
C  Key Subroutines/Functions Called: SSEMIS_INIT, GET_SSEMIS, DUST_INIT, GET_DUST_EMIS

C  Revision History:

C   18 Aug 11 David Wong: In the merge inline point source PM species calculation,
C                         arrays EMBUFF and PMEMIS_PT have incorrect index values
 
C ----------------------------------------------------------------------
         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE CGRID_SPCS          ! CGRID mechanism species
         USE PT3D_DEFN, ONLY: PT3DEMIS ! from plume rise
         USE AERO_DATA
         USE PRECURSOR_DATA
         USE SSEMIS                               ! sea-salt emissions
         USE UTILIO_DEFN
         USE DUST_EMIS                            ! windblown dust emissions
         USE ASX_DATA_MOD, ONLY: MET_DATA

         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
                                             ! TSTEP(3) = twoway model time step w.r.t.
                                             ! wrf time step and wrf/cmaq call frequency

C Local variables for sea salt emission rates (grid cell value)
         REAL :: CR_SSM( N_AEROSPC,N_MODE )   ! mass
         REAL :: CR_SSN( N_MODE )             ! number
         REAL :: CR_SSS( N_MODE )             ! surface area
         
C Local variables for windblown dust emission rates (grid cell value)
         REAL :: CR_DUSTM( N_AEROSPC,N_MODE )  ! mass
         REAL :: CR_DUSTN( N_MODE )            ! number
         REAL :: CR_DUSTS( N_MODE )            ! surface area
         
C Miscellaneous variables
         CHARACTER( 16 ), SAVE :: PNAME = 'GET_AERO_EMIS'
         INTEGER C, R, L, N, V, S, ISPC, IAERO, IM   ! Loop indices
         INTEGER ISEA, IDUST
#ifdef Verbose2
         real sumem
#endif

C ----------------------------------------------------------------------
         VDEMIS_AE = 0.0

C *** Get sea salt emissions
         CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( 1 ), CELLHGT( 1 ) )

C *** Get grid cell values for sea salt emission rates
C     Convert units of SSOUTM from [g/m3/s] to [umol/m3/s].
C     Pass emission rates back to aerosol emission master array.
         DO ISPC = 1, N_EMIS_SPC
            ISEA = MAP_EMtoSEA( ISPC )
            IF ( ISEA .GT. 0 ) THEN
               IM    = MAP_EMtoMODE( ISPC )
               IAERO = MAP_SEAtoAERO( ISEA )
               VDEMIS_AE( ISPC,1,:,: ) = SSOUTM( ISEA,IM,:,: )
     &                    * EM_FAC( ISPC,ISEASRC ) * MGPG / AEROSPC_MW( IAERO )
            END IF
         END DO
         DO N = 1, N_MODE
            VDEMIS_AE( MAP_NUMtoEM( N ),1,:,: ) = SSOUTN( N,:,: )
            VDEMIS_AE( MAP_SRFtoEM( N ),1,:,: ) = SSOUTS( N,:,: )
         END DO
 
C *** Get dust emissions
         IF ( WB_DUST ) THEN
            CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), CELLHGT( 1 ) )

C *** Get grid cell values for windblown dust emission rates
C     Convert units of DUSTOUTM from [g/m3/s] to [umol/m3/s].
C     Pass emission rates back to aerosol emission master array.
            DO ISPC = 1, N_EMIS_SPC
               IDUST = MAP_EMtoDUST( ISPC )
               IF ( IDUST .GT. 0 ) THEN
                  IM    = MAP_EMtoMODE( ISPC )
                  IAERO = MAP_DUSTtoAERO( IDUST )
                  VDEMIS_AE( ISPC,1,:,: ) = VDEMIS_AE( ISPC,1,:,: ) 
     &                       + DUSTOUTM( IDUST,IM,:,: ) * EM_FAC( ISPC,IDUSTSRC ) 
     &                          * MGPG / AEROSPC_MW( IAERO )
               END IF
            END DO
            DO N = 1, N_MODE
               VDEMIS_AE( MAP_NUMtoEM( N ),1,:,: ) = 
     &                    VDEMIS_AE( MAP_NUMtoEM( N ),1,:,: )
     &                      + DUSTOUTN( N,:,: )
               VDEMIS_AE( MAP_SRFtoEM( N ),1,:,: ) = 
     &                    VDEMIS_AE( MAP_SRFtoEM( N ),1,:,: )
     &                      + DUSTOUTS( N,:,: )
            END DO
  
         END IF

         RETURN

         END SUBROUTINE GET_AERO_EMIS

C-----------------------------------------------------------------------

         SUBROUTINE EMISS_SIZE_DIST ( VDEMIS_AE )

C  EMISS_SIZE_DIST distributes bulk aerosol emissions into size space
C  using parameters precompiled in the AERO_DATA module. 
C
C  Revision History:

C   16 AUG 17 BMURPHY: Created
C                      
C ----------------------------------------------------------------------

         USE GRID_CONF
         USE AERO_DATA, ONLY : AEROSPC, EMWT_PROF,
     &                         ACORS_IDX, ASO4_IDX, ANO3_IDX, ACL_IDX, AH2O_IDX,
     &                         ACORSEM_ASO4_FAC, ACORSEM_ANO3_FAC, ACORSEM_ACL_FAC,
     &                         ACORSEM_AH2O_FAC, ACORSEM_RENORM, AEROSPC_MW 
         USE AEROMET_DATA
         USE ASX_DATA_MOD, ONLY: MET_DATA

         REAL,    INTENT( INOUT )  :: VDEMIS_AE( :,:,:,: )

         INTEGER :: EMLAYS
         INTEGER :: N, S, IAERO, IM, ISPC   ! Looping Variables
         INTEGER :: N_EMIS_SPC, ROW, COL, LAY
         REAL    :: WT
         REAL, ALLOCATABLE :: EMISM3( :,:,:,: ), GSFAC( :,:,: )
         REAL, ALLOCATABLE :: EMIS_BUFF( :,:,: )
         REAL, PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI  ! 1.0E-9 = Kg/ug


C *** Initialize Variables  
         N_EMIS_SPC = SIZE( VDEMIS_AE, 1 ) - 1
         EMLAYS     = SIZE( VDEMIS_AE, 2 ) 
         
         ALLOCATE( EMISM3( EMLAYS,MY_NCOLS,MY_NROWS,N_MODE ) )
         ALLOCATE( GSFAC( EMLAYS,MY_NCOLS,MY_NROWS ) )
         ALLOCATE( EMIS_BUFF( EMLAYS,MY_NCOLS,MY_NROWS ) )
         EMISM3 = 0.0

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
         DO LAY = 1,EMLAYS
           DO COL = 1,MY_NCOLS
             DO ROW = 1,MY_NROWS
               GSFAC( LAY,COL,ROW ) = CONVEM_PM( LAY ) 
     &                              * Met_Data%RJACM( COL,ROW,LAY ) ![g/s] or [kg/hr] to [ug/m3/s]
             END DO
           END DO
         END DO
 
C *** Apply Aerosol Size Distribution
         DO ISPC = 1,N_EMIS_SPC
             IAERO = MAP_EMtoAERO( ISPC )
             IM    = MAP_EMtoMODE( ISPC )
             IF ( IAERO .EQ. 0 ) CYCLE
             
             ! Apply Size Distirbution Coefficient [ug/m3/s]
             EMIS_BUFF( :,:,: ) = EMWT_PROF( IAERO,IM ) * VDEMIS_AE( ISPC,:,:,: ) * GSFAC( :,:,: )

             ! Add Aerosol Emissions to Total Emission Buffer
             VDEMIS_AE( ISPC,:,:,: ) = VDEMIS_AE( ISPC,:,:,: ) + EMIS_BUFF
               
             ! Sum Total Volume of Mode N [m3/m3/s]
             IF ( .NOT. AEROSPC( IAERO )%TRACER ) THEN
                   EMISM3( :,:,:,IM ) = EMISM3( :,:,:,IM ) + 
     &                   EMIS_BUFF * ( F6DPIM9 / AEROSPC( IAERO )%DENSITY )
             END IF

             ! Apply Speciation to Coarse Aerosol Mode Emission
             IF ( IAERO .EQ. ACORS_IDX .AND. IM .EQ. N_MODE ) THEN
                VDEMIS_AE( MAP_AEROtoEM( ASO4_IDX,IM ),:,:,: ) = ACORSEM_ASO4_FAC * EMIS_BUFF
                VDEMIS_AE( MAP_AEROtoEM( ANO3_IDX,IM ),:,:,: ) = ACORSEM_ANO3_FAC * EMIS_BUFF 
                VDEMIS_AE( MAP_AEROtoEM( ACL_IDX,IM  ),:,:,: ) = ACORSEM_ACL_FAC  * EMIS_BUFF 
                VDEMIS_AE( MAP_AEROtoEM( AH2O_IDX,IM ),:,:,: ) = ACORSEM_AH2O_FAC * EMIS_BUFF 
                VDEMIS_AE( MAP_AEROtoEM( ACORS_IDX,IM),:,:,: ) = ACORSEM_RENORM   * EMIS_BUFF 
              
                ! Adjust 3rd Moment Summation to Account for Density
                ! Change in dust speciation
                EMISM3( :,:,:,IM ) = EMISM3( :,:,:,IM ) 
     &             - EMIS_BUFF( :,:,: ) * ( F6DPIM9 / AEROSPC( ACORS_IDX )%DENSITY )
     &             + VDEMIS_AE( MAP_AEROtoEM( ASO4_IDX,IM ),:,:,: )* (F6DPIM9 / AEROSPC( ASO4_IDX )%DENSITY )
     &             + VDEMIS_AE( MAP_AEROtoEM( ANO3_IDX,IM ),:,:,: )* (F6DPIM9 / AEROSPC( ANO3_IDX )%DENSITY )
     &             + VDEMIS_AE( MAP_AEROtoEM( ACL_IDX,IM  ),:,:,: )* (F6DPIM9 / AEROSPC( ACL_IDX )%DENSITY )
     &             + VDEMIS_AE( MAP_AEROtoEM( AH2O_IDX,IM ),:,:,: )* (F6DPIM9 / AEROSPC( AH2O_IDX )%DENSITY )
     &             + VDEMIS_AE( MAP_AEROtoEM( ACORS_IDX,IM),:,:,: )* (F6DPIM9 / AEROSPC( ACORS_IDX )%DENSITY )
             END IF

             ! Convert Mass Emission Rates from [ug/m3/s] to [umol/m3/s]
             VDEMIS_AE( ISPC,:,:,: ) = VDEMIS_AE( ISPC,:,:,: ) / AEROSPC_MW( IAERO )
         END DO          

C *** Calculate the number emissions rate for each mode [1/m3/s], using 
C     Equation 7b of Binkowski & Roselle (2003).
C     Calculate the surface area emissions rate for the fine modes [m2/m3/s],
C     using Equation 7c of Binkowski & Roselle (2003).  Multiplying by PI 
C     converts 2nd moment to surface area.
         DO IM = 1,N_MODE
            VDEMIS_AE( MAP_NUMtoEM(IM),:,:,: ) = EMISM3( :,:,:,IM ) * FACNUM( IM ) 
            VDEMIS_AE( MAP_NUMtoEM(IM),:,:,: ) = EMISM3( :,:,:,IM ) * FACSRF( IM ) 
         END DO
 
         END SUBROUTINE EMISS_SIZE_DIST
 
      END MODULE AERO_EMIS

