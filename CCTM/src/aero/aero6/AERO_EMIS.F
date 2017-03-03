
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
       
      IMPLICIT NONE
      
C aerosol emissions: [ppmv/s] for mass & number spcs, [m2/mol/s] for surface area spcs
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_AE( :,:,:,: )

      INTEGER :: PM_EMLYRS   ! number of vertical layers on emis file (set in EMIS_DEFN)

      PUBLIC VDEMIS_AE, PM_EMLYRS, AERO_EMIS_INIT, GET_AERO_EMIS
      PRIVATE

C Variables for converting mass emissions rate to number emissions rate
      REAL,    SAVE :: FACNUM( N_MODE )

C Variables for converting mass emissions rate to 2nd moment emissions rate
      REAL,    SAVE :: FACSRF( N_MODE )

C Variables for the thickness and volume of each grid cell
      REAL,    ALLOCATABLE, SAVE :: CELLHGT( : ) ! grid-cell height [sigma]
      REAL,    ALLOCATABLE, SAVE :: CELLVOL( : ) ! grid-cell volume [m2*sigma]
 
C Emission rate of all aerosol species interpolated to current time
      REAL,    ALLOCATABLE, SAVE :: EMBUFF  ( :,:,:,: ) ! in all grid cells
      REAL,    ALLOCATABLE, SAVE :: EMTMP   ( :,:,:,: ) ! in all grid cells
      REAL,    ALLOCATABLE, SAVE :: PM_EM   ( : )       ! in one grid cell
      REAL,    ALLOCATABLE, SAVE :: EM_RATE ( : )       !
      REAL,    ALLOCATABLE, SAVE :: EM_PART ( :,: )     !
      INTEGER, ALLOCATABLE, SAVE :: EM_MAP  ( :,: )     ! index in namelist table
      REAL,    ALLOCATABLE, SAVE :: EM_FAC  ( :,: )     ! Namelist emissions scaling factor
      INTEGER, ALLOCATABLE, SAVE :: NUM_MAP ( : )       !
      REAL,    ALLOCATABLE, SAVE :: NUM_FAC ( : )       !
      INTEGER, ALLOCATABLE, SAVE :: SRF_MAP ( : )       !
      REAL,    ALLOCATABLE, SAVE :: SRF_FAC ( : )       !
      INTEGER, ALLOCATABLE, SAVE :: SS_MAP  ( :,: )     !
      INTEGER, ALLOCATABLE, SAVE :: DUST_MAP( :,: )     !

C EMBUFF     mass emission rate input global array
C EMTMP      temporary array for reading and summing emissions into EMBUFF
C PM_EM      mass emission rate input localized to one grid cell
C EM_RATE    scaled PM_EM
C EM_PART    particulate mass emission rate in one grid cell
C EM_MAP                 mass emission rate index in VDEMIS_AE (CGRID)
C EM_FAC                 mass emission Namelist scaling factor for VDEMIS_AE (CGRID)
C NUM_MAP                number emission rate index in VDEMIS_AE (CGRID)
C NUM_FAC
C SRF_MAP                sfc area emission rate index in VDEMIS_AE (CGRID)
C SRF_FAC
C SS_MAP     sea salt emission rate index in SSOUTM
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

C For handling vapor-phase sulfuric acid emissions
      INTEGER, SAVE :: VSULF               ! index to H2SO4 in VDEMIS array
      INTEGER, SAVE :: VPSO4               ! index to PSO4 in EM_RATE array

C Miscellaneous variables
      LOGICAL, SAVE :: WB_DUST   ! flag for windblown dust option
      INTEGER :: IDX
      CHARACTER( 96 ) :: XMSG = ' '
      INTEGER, SAVE :: LOGDEV
      INTEGER, EXTERNAL :: SETUP_LOGDEV

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
         INTEGER, INTENT( IN ) :: NEMIS_AE   ! number of aerosol emissions species
         LOGICAL SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: FINDEX       !  looks up number in table.

C Local Variables:
         CHARACTER( 16 ), SAVE :: CTM_WB_DUST = 'CTM_WB_DUST' ! env var for windblown dust
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
         CALL MAP_PMEMIS()

C *** set up for sea-salt emission processing
         IF ( .NOT. SSEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing sea-salt emission processing'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C *** windblown dust emissions?
         WB_DUST = ENVYN( 'CTM_WB_DUST', 'Flag for windblown dust emissions',
     &                   .TRUE., STATUS )

         IF ( WB_DUST ) THEN
C *** set up for dust emission processing
            IF ( .NOT. DUST_EMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
               XMSG = 'Failure initializing dust emission processing'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
         END IF

         IF ( NEMIS_AE .GT. 0 ) THEN   ! all EMLAYS
            ALLOCATE ( VDEMIS_AE( NEMIS_AE,PM_EMLYRS,MY_NCOLS,MY_NROWS ),
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
!           FACTM2( N ) = EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
            FACSRF( N ) = PI * EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
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
    
         ALLOCATE ( EMTMP( NCOLS,NROWS,PM_EMLYRS,1 ),
     &              STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** EMTMP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CONVEM_PM( PM_EMLYRS ),
     &              CELLHGT( PM_EMLYRS ),
     &              CELLVOL( PM_EMLYRS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** CONVEM_PM, CELLHGT or CELLVOL memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
C *** Allocate memory for EM_RATE, EM_PART, EM_MAP, EM_FAC, NUM_MAP, NUM_FAC, SRF_MAP, SRF_FAC, SS_MAP and DUST_MAP
         ALLOCATE ( EM_RATE( N_EMIS_PM ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** EM_RATE memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( EM_PART ( N_AEROSPC, N_MODE ),
     &              EM_MAP  ( N_AEROSPC, N_MODE ),
     &              EM_FAC  ( N_AEROSPC, N_MODE ),
     &              NUM_MAP (            N_MODE ),
     &              NUM_FAC (            N_MODE ),
     &              SRF_MAP (            N_MODE ),
     &              SRF_FAC (            N_MODE ),
     &              SS_MAP  ( N_AEROSPC, N_MODE ),
     &              DUST_MAP( N_AEROSPC, N_MODE ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** memory allocation failed for '
     &           // 'EM_PART, EM_MAP, EM_FAC, NUM_MAP, NUM_FAC, SRF_MAP, SRF_FAC, SS_MAP or DUST_MAP'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C *** Map indices of emission species
      write( logdev,* ) '=== aerospc em_map'
         EM_MAP = 0  
         DO S = 1, N_AEROSPC
            IF ( AEROSPC( S )%EMIS .NE. ' ' ) THEN
               ! Verify that aerospc( S )%emis is a pm emis
               DO N = 1, N_MODE
                  IF ( .NOT. AERO_MISSING(S,N) .AND.
     &                 AEROSPC( S )%EMIS_SPLIT( N ) .NE. 0.0 ) THEN
                     EM_MAP( S,N ) = INDEX1( AEROSPC( S )%NAME( N ), N_AE_EMIS, AE_EMIS )
                     write( logdev,* ) ' ', s, n, aerospc( s )%name( n ), em_map( s,n )
!                    if ( em_map( s,n ) .eq. 0 ) then
!                       write( logdev,* ) '*** aerospc em_map: ', s, n, em_map( s,n )
!                    end if
                  END IF
               END DO
            END IF
         END DO

C *** set across-the-board adjustment factors (from Namelist file)
C     Strategy: first check Namelist Emis_Fac for antrhop. emis, then check for non-zero split factor
      write( logdev,* ) 'modified aerospc em_fac'
         EM_FAC = 1.0 ! array assignment
         DO S = 1, N_AEROSPC
            IF ( AEROSPC( S )%EMIS .NE. ' ' ) THEN
               DO N = 1, N_MODE
                  IF ( .NOT. AERO_MISSING(S,N) ) THEN
                     L = INDEX1( AEROSPC( S )%NAME( N ), N_AE_EMIS, AE_EMIS )
                     IF ( L .GT. 0 ) THEN
                        EM_FAC( S,N ) = AE_EMIS_FAC( L )
                        ! Test for POA Species and Aggregate Volatility
                        ! Parameters
                        IF ( AEROSPC( S )%EMIS .EQ. 'POC' ) THEN
                            DO IPOA = 1,N_VBS_BIN
                               IF ( AEROSPC( S )%NAME( N )(2:6) .EQ. POA_NAME( IPOA ) ) THEN
                                  EM_FAC( S,N ) = EM_FAC( S,N ) * POA_OP_VF( IPOA ) * POA_AMF( IPOA )
                               ENDIF
                            END DO
                        ENDIF

                        if ( em_fac( s,n ) .ne. 1.0 ) then
                           write( logdev,* ) ' ', s, n, aerospc( s )%name( n ), em_fac( s,n )
                        end if
                     END IF
                  END IF
               END DO
            ELSE
               DO N = 1, N_MODE
                  IF ( AEROSPC( S )%EMIS_SPLIT( N ) .NE. 0.0 ) THEN
                     L = INDEX1( AEROSPC( S )%NAME( N ), N_AE_EMIS, AE_EMIS )
                     IF ( AE_EMIS( L ) .NE. ' ' ) THEN
                        EM_FAC( S,N ) = AE_EMIS_FAC( L )
                        if ( em_fac( s,n ) .ne. 1.0 ) then
                           write( logdev,* ) ' ', s, n, aerospc( s )%name( n ), em_fac( s,n )
                        end if
                     END IF
                  END IF
               END DO
            END IF
         END DO


         IF ( INDEX( MECHNAME,"NVPOA" ) .EQ. 0 ) THEN
           ! Semivolatile POA  
C *** Let the user set the pcVOC species (which makes pcSOA) emissions
C *** scaling to 0.0
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

C *** Assign Volaitlity Distribution to Primary Orgnaic Vapor Species
           ! Find the Index for Each Vapor and alter the emissions scaling
           ! to reflect the parameters in AERO_DATA. The GC_EMIS_FAC field
           ! from the GC namelist should account for the conversion from
           ! g/s (POC) to mol/s (Vapors). That factor is then modified
           ! by the volatility distribution itself.
           DO IPOA = 1,N_VBS_BIN
             VNAME = 'V' // POA_NAME( IPOA )
             V = INDEX1( VNAME, N_GC_SPC, GC_SPC )
             IF ( V .LT. 1 ) THEN
                WRITE(LOGDEV,99901)TRIM( VNAME ), TRIM( VNAME ), TRIM( MECHNAME )
                SUCCESS = .FALSE.
             ELSE
                S = FINDEX( V, N_GC_EMIS, GC_EMIS_MAP )
                IF ( S .GT. 0 )THEN
                   GC_EMIS_FAC( S ) = GC_EMIS_FAC( S ) * POA_OP_VF( IPOA ) * 
     &                                (1.0 - POA_AMF( IPOA ) ) / GC_MOLWT( V )
                ELSE
                   WRITE(LOGDEV,99902)TRIM( VNAME )
!                  SUCCESS = .FALSE.
                END IF
             END IF
           END DO
         ENDIF

C *** Map aerosol number emission species and set across-the-board adjustment factors
         NUM_MAP = 0   ! array assignment
         NUM_FAC = 1.0 ! array assignment
         DO N = 1, N_MODE
            NUM_MAP( N ) = INDEX1( AEROMODE( N )%NUM_NAME, N_AE_EMIS, AE_EMIS )
            NUM_FAC( N ) = AE_EMIS_FAC( NUM_MAP( N ) )
#ifdef Verbose1
            write( logdev,* ) 'num_map: ', n, num_map( n ), aeromode( n )%num_name, num_fac( n )
#endif
            IF ( NUM_MAP( N ) .EQ. 0 ) THEN
               XMSG = 'FATAL: Cannot find species and emission surrogate for ' 
     &             // TRIM( AEROMODE( N )%NUM_NAME )
     &             // ' in AE namelist. Check against repository version of '
     &             // ' the file.'
!               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.
            END IF
         END DO

C *** Map aerosol surface emission species and set across-the-board adjustment factors
         SRF_MAP = 0   ! array assignment
         SRF_FAC = 1.0 ! array assignment
         DO N = 1, N_MODE
            SRF_MAP( N ) = INDEX1( AEROMODE( N )%SRF_NAME, N_AE_EMIS, AE_EMIS )
            SRF_FAC( N ) = AE_EMIS_FAC( SRF_MAP( N ) )
#ifdef Verbose1
            write( logdev,* ) 'srf_map: ', n, srf_map( n ), aeromode( n )%srf_name, srf_fac( n )
#endif
            IF ( SRF_MAP( N ) .EQ. 0 ) THEN
               XMSG = 'FATAL: Cannot find species and emission surrogate for ' 
     &             // TRIM( AEROMODE( N )%SRF_NAME )
     &             // ' in AE namelist. Check against repository version of '
     &             // ' the file.'
!               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.
            END IF
         END DO

C *** PVB: speciate PMC (from AERO_DATA)
         N = N_MODE
         SPFC_ASO4  = REAL( ACORSEM_ASO4_FAC ) * AEROSPC( ASO4_IDX )%EMIS_SPLIT( N )
         SPFC_ANO3  = REAL( ACORSEM_ANO3_FAC ) * AEROSPC( ANO3_IDX )%EMIS_SPLIT( N )
         SPFC_ACL   = REAL( ACORSEM_ACL_FAC ) * AEROSPC( ACL_IDX )%EMIS_SPLIT( N )
         SPFC_AH2O  = REAL( ACORSEM_AH2O_FAC ) * AEROSPC( AH2O_IDX )%EMIS_SPLIT( N )
         SPFC_ACORS = REAL( ACORS_RENORM ) * AEROSPC( ACORS_IDX )%EMIS_SPLIT( N )

C *** Map sea salt emission species
         SS_MAP = 0   ! array assignment
         DO S = 1, NSSSPC
            IDX = SSSPC_MAP( S )
#ifdef Verbose1
            write( logdev,* ) 'ssspc_map: ', s, idx, ssspc_map( s )
#endif
            DO N = 2, N_MODE   ! add SEASALT indices to EM_MAP
               IF ( .NOT. AERO_MISSING( IDX,N ) ) THEN
                  EM_MAP( IDX,N ) = INDEX1( AEROSPC( IDX )%NAME( N ), N_AE_EMIS, AE_EMIS )
                  SS_MAP( IDX,N ) = S
#ifdef Verbose1
               write( logdev,* ) 'ss_map:    ', s, n, idx, ss_map( idx,n ), aerospc( idx )%name( n )
#endif
               END IF
            END DO
         END DO

         IF ( WB_DUST ) THEN
C *** Map windblown dust emission species
            DUST_MAP = 0   ! array assignment
            DO N = 2, N_MODE
               DO S = 1, NDUST_SPC
                  IF ( DUST_SPC( S )%NAME( N ) .NE. ' ' ) THEN
                     IDX = FINDAERO( DUST_SPC( S )%NAME( N ), .FALSE. )
                     IF ( IDX .NE. 0 ) THEN
                        EM_MAP( IDX,N ) = INDEX1( AEROSPC( IDX )%NAME( N ), N_AE_EMIS, AE_EMIS )
                        DUST_MAP( IDX,N ) = S
                     END IF
#ifdef Verbose1
                  write( logdev,* ) 'dust_map: ', n, s, dust_map( idx,n ), aerospc( idx )%name( n )
#endif
                  END IF
               END DO
            END DO
         END IF

#ifdef Verbose1
         do s = 1, n_aerospc
            do n = 1, n_mode
               if ( em_map( s,n ) .ne. 0 ) then
                  write( logdev,* ) 'em_map: ', n, s, em_map( s,n ), aerospc( s )%name( n ), em_fac( s,n )
               end if
            end do
         end do
#endif

C *** get index of SO4 emissions
         VPSO4 = 0
         DO S = 1, N_EMIS_PM
            IF ( PMEM_MAP( S ) .EQ. ASO4_IDX ) VPSO4 = S
#ifdef Verbose1
               write( logdev,* ) 'vpso4: ', s, pmem_map( s ), aso4_idx, vpso4
#endif
         END DO

         IF ( VPSO4 .EQ. 0 ) THEN
            XMSG = 'Cannot define index for SO4 emissions: '
     &           // 'aerospc%emis='' '' in AERO_DATA.F. Check if AE '
     &           // 'namelist is missing lines for ASO4I, ASO4J, and ASO4K'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.
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

         IF ( PMEM_UNITS .EQ. 'G/S' .OR.
     &        PMEM_UNITS .EQ. 'g/s' ) THEN
            CONVEM_PM_MASS = MGPG                  ! (g/s) -> (ug/s)
         ELSE IF ( PMEM_UNITS .EQ. 'KG/HR' .OR.
     &             PMEM_UNITS .EQ. 'kg/hr' ) THEN
            CONVEM_PM_MASS = GPKG * MGPG / 3600.0  ! (kg/hr) -> (ug/s)
         ELSE
            XMSG = 'FATAL: Units incorrect on ' // EMIS_1 //
     &             ' for aerosol species.'
!            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.
         END IF

         DO L = 1, PM_EMLYRS
            CONVEM_PM( L ) = CONVEM_PM_MASS / CELLVOL( L )
         END DO

C *** Get domain window info from input files

         CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF,
     &                   STARTCOL, ENDCOL, STARTROW, ENDROW )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

99901     FORMAT(/'FATAL: GC namelist used does not include species, ', A, 
     &         / 'Check this namelist contains the same information '
     &         / 'for ', A,' as the repository version of the GC namelist for '
     &         / 'the ', A, ' mechanism.' )
99902     FORMAT(/'WARNING: GC namelist line for ', A, ' does not include an emissions '
     &         / 'surrogate. Simulation cannot evaluate the effect from zeroing out its '
     &         / 'emissions.')

         END FUNCTION  AERO_EMIS_INIT

C-----------------------------------------------------------------------

         SUBROUTINE GET_AERO_EMIS ( JDATE, JTIME, TSTEP, VDEMIS )

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
         USE PT3D_DEFN, ONLY: PT3DEMIS, PMEMIS_PT ! from plume rise
         USE PTMAP, ONLY: N_SPC_PTPM, PTPM_MAP    ! defines emissions file(s) PM mapping
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

         REAL,    INTENT( INOUT ) :: VDEMIS( :,:,:,: ) ! gas emissions [ppmv/s]

C Geometric Constants
         REAL, PARAMETER :: F6DPI = 6.0 / PI
         REAL, PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI   ! 1.0E-9 = Kg/ug

C Local variables for sea salt emission rates (grid cell value)
         REAL :: CR_SSM( N_AEROSPC,N_MODE )   ! mass
         REAL :: CR_SSN( N_MODE )             ! number
         REAL :: CR_SSS( N_MODE )             ! surface area
         
C Local variables for windblown dust emission rates (grid cell value)
         REAL :: CR_DUSTM( N_AEROSPC,N_MODE )  ! mass
         REAL :: CR_DUSTN( N_MODE )            ! number
         REAL :: CR_DUSTS( N_MODE )            ! surface area
         
         REAL( 8 ) :: GSFAC                        ! into [ug/m3/sec]

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
         CHARACTER( 16 ), SAVE :: PNAME = 'GET_AERO_EMIS'
         INTEGER C, R, L, N, V, S, ISPC       ! Loop indices
#ifdef Verbose2
         real sumem
#endif

C ----------------------------------------------------------------------

C *** Read aerosol emission rates from file and interpolate to the current
C     time.  Store result in EMBUFF array.

         IF ( PT3DEMIS ) THEN   ! in-line plume rise

            EMBUFF = 0.0   ! array assignment
            EMTMP  = 0.0

            DO S = 1, N_EMIS_PM   ! read 2d file
               IDX = PMEM_MAP( S )
               IF ( .NOT. INTERPX( EMIS_1, PMEM_MAP_NAME( S ), PNAME,
     &                           STARTCOL,ENDCOL, STARTROW,ENDROW, 1,1,
     &                           JDATE, JTIME, EMBUFF( 1,1,1,S ) ) ) THEN
                  XMSG = 'Could not read '
     &               // TRIM( AEROSPC( IDX )%EMIS ) // ' from ' // EMIS_1
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG  )
               END IF
            END DO

            DO S = 1, N_SPC_PTPM   ! merge inline point src PM species
               V = PTPM_MAP( S )   ! set in PT3D_DEFN
               DO L = 1, PM_EMLYRS
                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
!                       EMBUFF( C,R,L,S ) = EMBUFF( C,R,L,S ) + PMEMIS_PT( C,R,L,V )
                        EMBUFF( C,R,L,V ) = EMBUFF( C,R,L,V ) + PMEMIS_PT( C,R,L,S )
                     END DO
                  END DO
               END DO
            END DO

         ELSE   ! read 3d file

            DO S = 1, N_EMIS_PM
               IDX = PMEM_MAP( S )
               IF ( .NOT. INTERPX( EMIS_1, PMEM_MAP_NAME( S ), PNAME,
     &                           STARTCOL,ENDCOL, STARTROW,ENDROW, 1,PM_EMLYRS,
     &                           JDATE, JTIME, EMBUFF( 1,1,1,S ) ) ) THEN
                 XMSG = 'Could not read '
     &             // TRIM( AEROSPC( IDX )%EMIS ) // ' from ' // EMIS_1
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
         CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( 1 ), CELLHGT( 1 ) )

         IF ( WB_DUST ) THEN
C *** Get dust emissions
            CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), CELLHGT( 1 ) )
         END IF

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
                     IF ( SS_MAP( S,N ) .NE. 0 )
     &                  CR_SSM( S,N ) = SSOUTM( SS_MAP( S,N ),N,C,R ) * MGPG
                  END DO
                  CR_SSN( N ) = SSOUTN( N,C,R )
                  CR_SSS( N ) = SSOUTS( N,C,R )
               END DO

               IF ( WB_DUST ) THEN
C *** Get grid cell values for windblown dust emission rates
C     Convert units of DUSTOUTM from [g//m3/s] to [ug/m3/s]
                  CR_DUSTM = 0.0   ! array assignment
                  DO N = 1, N_MODE
                     DO S = 1, N_AEROSPC
                        IF ( DUST_MAP( S,N ) .NE. 0 )
     &                     CR_DUSTM( S,N ) = DUSTOUTM( DUST_MAP( S,N ),N,C,R ) * MGPG
                     END DO
                     CR_DUSTN( N ) = DUSTOUTN( N,C,R )
                     CR_DUSTS( N ) = DUSTOUTS( N,C,R )
                  END DO
               END IF

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
               GSFAC = CONVEM_PM( L ) * Met_Data%RJACM( C,R,L )
               CONVM = MWAIR / GPKG / DENS( C,R,L )   ! [m3/mol]
               CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
               CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
               DO S = 1, N_EMIS_PM
                  EM_RATE( S ) = PM_EM( S ) * GSFAC
               END DO

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
               EM_RATE( VPSO4 ) = EM_RATE( VPSO4 ) + VDEMIS( VSULF,L,C,R )
     &                          * REAL( PRECURSOR_MW( SULF_IDX ), 4 ) / CONVM
               VDEMIS( VSULF,L,C,R ) = 0.0

C *** Assign fine-particle emission rates to the fine modes.
C     Assume all non-seasalt-emissions of sulfate, nitrate, and unspeciated
C     fine PM are in the accumulation mode.  Split the carbon emissions 
C     between the Aitken and accumulation modes, as described in paragraph 
C     #12 of Binkowski & Roselle (2003).

C *** Calculate emissions rate for third moments [m3/m3/s] of each mode 
C     (excluding sea salt and windblown dust), as in Equation 7a of
C     Binkowski & Roselle (2003).
               EM_PART = 0.0   ! array assignment
               EMISM3 = 0.0    ! array assignment
               DO S = 1, N_EMIS_PM
                  IDX = PMEM_MAP( S )
                  DO N = 1, N_MODE - 1
                     EM_PART( IDX,N ) = EM_PART( IDX,N ) + 
     &                        AEROSPC( IDX )%EMIS_SPLIT( N ) * EM_RATE( S )
     &                            * EM_FAC( IDX,N )
                     IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                        EMISM3( N ) = EMISM3( N ) + 
     &                        AEROSPC( IDX )%EMIS_SPLIT( N ) * EM_RATE( S )
     &                          * EM_FAC( IDX,N ) * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                     END IF
                  END DO

                  !Apply Emissions to Coarse Aerosol Mode
                  N = N_MODE
                  IF ( IDX .EQ. ASO4_IDX ) THEN
                     EM_PART( IDX,N ) = SPFC_ASO4 * EM_RATE( S ) * EM_FAC( IDX,N )
                  ELSE IF ( IDX .EQ. ANO3_IDX ) THEN
                     EM_PART( IDX,N ) = SPFC_ANO3 * EM_RATE( S ) * EM_FAC( IDX,N )
                  ELSE IF ( IDX .EQ. ACL_IDX ) THEN
                     EM_PART( IDX,N ) = SPFC_ACL * EM_RATE( S ) * EM_FAC( IDX,N )
                  ELSE IF ( IDX .EQ. AH2O_IDX ) THEN
                     EM_PART( IDX,N ) = SPFC_AH2O * EM_RATE( S ) * EM_FAC( IDX,N )
                  ELSE IF ( IDX .EQ. ACORS_IDX ) THEN
                     EM_PART( IDX,N ) = SPFC_ACORS * EM_RATE( S ) * EM_FAC( IDX,N )
                  ELSE
                     EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N ) 
     &                                    * EM_RATE( S ) * EM_FAC( IDX,N )
                  END IF
                  IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                     EMISM3( N ) = EMISM3( N ) + EM_PART( IDX,N )
     &                           * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                  END IF
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

C *** Add Sea-salt mass emissions to mode- and species-specific emissions
C     in layer 1.  Units of CR_SSM are [ug/m3/s].  Sea-salt sulfate in the fine
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

               IF ( WB_DUST ) THEN

C *** Add windblown dust mass emissions to mode- and species-specific
C     emissions in layer 1.  Units of CR_DUSTM are [ug/m3/s].
                  DO N = 1, N_MODE
                     DO S = 1, N_AEROSPC
                        EM_PART( S,N ) = EM_PART( S,N ) + CR_DUSTM( S,N )
                     END DO
                  END DO

C *** Add windblown dust to the total emissions of particle number and surface area
                  DO N = 1, N_MODE
                     EM_NUM( N ) = EM_NUM( N ) + CR_DUSTN( N )
                     EM_SRF( N ) = EM_SRF( N ) + CR_DUSTS( N )
                  END DO

               END IF

C *** Convert emission rates into molar-mixing-ratio units, as required by
C     the vertical diffusion routines.  Mass and number emissions are converted
C     to [ppmv/s].  Note: EM_PART has units of [ug/m3/s] and mol(s)/mol(air) is
C     1.0e6 ppmv; so 1.0e-6 ug/g cancels the 1.0e6 ppmv/[mol(s)/mol(air)].
C     Surface area emissions are converted to [m2/mol/s].
C     Save results in the VDEMIS_AE array.
               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     IF ( EM_MAP( S,N ) .GT. 0 ) THEN
                        VDEMIS_AE( EM_MAP( S,N ),L,C,R ) = EM_PART( S,N )
     &                                      * ( CONVM / AEROSPC_MW( S ) )
                     END IF
                  END DO
                  VDEMIS_AE( NUM_MAP( N ),L,C,R ) = EM_NUM( N ) * NUM_FAC( N ) * CONVN
                  VDEMIS_AE( SRF_MAP( N ),L,C,R ) = EM_SRF( N ) * SRF_FAC( N ) * CONVS
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
                  GSFAC = CONVEM_PM( L ) * Met_Data%RJACM( C,R,L )
                  CONVM = MWAIR / GPKG / DENS( C,R,L )   ! [m3/mol]
                  CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
                  CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
                  DO S = 1, N_EMIS_PM
                     EM_RATE( S ) = PM_EM( S ) * GSFAC
                  END DO

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
                  EM_RATE( VPSO4 ) = EM_RATE( VPSO4 ) + VDEMIS( VSULF,L,C,R )
     &                             * REAL( PRECURSOR_MW( SULF_IDX ), 4 ) / CONVM
                  VDEMIS( VSULF,L,C,R ) = 0.0

C *** Calculate emissions rate for third moments [m3/m3/s] of each mode 
C     (excluding sea salt), as in Equation 7a of Binkowski & Roselle (2003).
                  EM_PART = 0.0   ! array assignment
                  EMISM3 = 0.0    ! array assignment
                  DO S = 1, N_EMIS_PM
                     IDX = PMEM_MAP( S )
                     DO N = 1, N_MODE - 1
                        EM_PART( IDX,N ) = EM_PART( IDX,N ) + 
     &                      AEROSPC( IDX )%EMIS_SPLIT( N ) * EM_RATE( S ) * EM_FAC( IDX,N )
                        IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                           EMISM3( N ) = EMISM3( N ) + 
     &                     AEROSPC( IDX )%EMIS_SPLIT( N ) * EM_RATE( S ) * EM_FAC( IDX,N )
     &                        * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                        END IF
                     END DO

                     !Compute Coarse Mode Properties
                     N = N_MODE
                     IF ( IDX .EQ. ASO4_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ASO4 * EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. ANO3_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ANO3 * EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. ACL_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ACL * EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. AH2O_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_AH2O * EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. ACORS_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ACORS * EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE
                        EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N ) * EM_RATE( S ) * EM_FAC( IDX,N )
                     END IF
                     IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                        EMISM3( N ) = EMISM3( N ) + EM_PART( IDX,N )
     &                              * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                     END IF
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
     &                                             * CONVM / AEROSPC_MW( S )
                        END IF
                     END DO
                     VDEMIS_AE( NUM_MAP( N ),L,C,R ) = EM_NUM( N ) * NUM_FAC( N ) * CONVN
                     VDEMIS_AE( SRF_MAP( N ),L,C,R ) = EM_SRF( N ) * SRF_FAC( N ) * CONVS
                  END DO

               END DO   ! loop on MY_NCOLS
            END DO   ! loop on MY_NROWS
         END DO   ! loop on PM_EMLYRS

#ifdef Verbose2
         write( logdev,'(/"@@@1", i8, i7.6)') jdate, jtime
         l = 1
         do s = 1, n_ae_emis
            sumem = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumem = sumem + vdemis_ae( s,l,c,r )
               end do
            end do
            write( logdev,'("@@@1 ", a, g16.6)') ae_emis( s ), sumem
         end do
#endif

         RETURN

         END SUBROUTINE GET_AERO_EMIS

      END MODULE AERO_EMIS

