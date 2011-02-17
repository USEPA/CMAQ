
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/Attic/AERO_EMIS.F,v 1.4 2011/02/17 14:17:45 sjr Exp $

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
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
       
      IMPLICIT NONE
      
C Molar masses (molecular wt.) reciprocals of each aerosol species
      REAL, ALLOCATABLE, SAVE :: R_AE_EM_MW( : )
      REAL, ALLOCATABLE, SAVE :: AE_EM_MW( : )

      CONTAINS

C-----------------------------------------------------------------------

         SUBROUTINE GET_AERO_EMIS ( JDATE, JTIME, TSTEP, EMLYRS, RJACM,
     &                              VDEMIS, VDEMIS_AE )

C  GET_AERO_EMIS reads PM emissions from gridded input file and
C  optionally from plume rise calculated point source emissions (via VDEMIS
C  array) and converts into molar-mixing-ratio units, for use in the vertical
C  diffusion routines
 
C  Key Subroutines/Functions Called:  SSEMIS_INIT, GET_SSEMIS
 
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

         USE PT3D_DEFN, ONLY: PT3DEMIS, PMEMIS_PT ! from plume rise
         USE PMEM_DEFN                            ! defines PM emissions species
         USE AERO_DATA
         USE PRECURSOR_DATA
         USE PTMAP, ONLY: N_SPC_PTPM, PTPM_MAP    ! defines emissions file(s) PM mapping
         USE SSEMIS                               ! sea-salt emissions
         USE CGRID_SPCS                           ! CGRID mechanism species
     
         INCLUDE SUBST_RXCMMN    ! to get mech name
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
!        INCLUDE SUBST_IODECL    ! I/O definitions and declarations
#include      SUBST_IODECL      # I/O definitions and declarations
         INCLUDE SUBST_IOFDESC   ! file header data structure
         INCLUDE SUBST_FILES_ID  ! file name parameters

C *** Arguments:

         INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP( 2 ) ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
         INTEGER, INTENT( IN ) :: EMLYRS   ! number of vertical layers on emis file
         REAL,    INTENT( IN ) :: RJACM( NCOLS,NROWS,NLAYS ) ! recip of mid-layer
                                                             ! Jacobian [1/m]
         REAL,    INTENT( INOUT ) :: VDEMIS   ( :,:,:,: ) ! gas emissions [ppmv/s]
         REAL,    INTENT( OUT )   :: VDEMIS_AE( :,:,:,: ) ! aerosol emissions
                                                       ! [ppmv/s] for mass & number spcs
                                                       ! [m2/mol/s] for surface area spcs

C *** Local Variables:

C     Geometric Constants
         REAL, PARAMETER :: F6DPI = 6.0 / PI
         REAL, PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI

C     Aerosol version name
         CHARACTER( 16 ), SAVE :: AE_VRSN

C     Geometric mean diameter by volume (or mass) of emitted particles in 
C     each mode [ m ].  See paragraph #14 of Binkowski & Roselle (2003)
         REAL, PARAMETER :: DGVEM( N_MODE ) = (/ 0.03E-6, 0.3E-6, 6.0E-6 /)

C     Geometric standard deviation of emitted particles in each mode, as
C     described in paragraph #14 of Binkowski & Roselle (2003)
         REAL, PARAMETER :: SGEM( N_MODE ) = (/ 1.7, 2.0, 2.2 /)

C     Variables for converting mass emissions rate to number emissions rate
         REAL, SAVE :: FACNUM( N_MODE )

C     Variables for converting mass emissions rate to 2nd moment emissions rate
         REAL, SAVE :: FACSRF( N_MODE )

C     Variables for calculating the volume of each grid cell
         REAL  DX1, DX2                             ! grid-cell width and length [m]
         REAL  CELLAREA                             ! grid-cell area [m2]
         REAL,    ALLOCATABLE, SAVE :: CELLHGT( : ) ! grid-cell height [sigma]
         REAL,    ALLOCATABLE, SAVE :: CELLVOL( : ) ! grid-cell volume [m2*sigma]

C     Emission rate of all aerosol species interpolated to current time
         REAL,    ALLOCATABLE, SAVE :: EMBUFF  ( :,:,:,: ) ! in all grid cells
         REAL,    ALLOCATABLE, SAVE :: PM_EM   ( : )       ! in one grid cell
         REAL,    ALLOCATABLE, SAVE :: EM_RATE ( : )       !
         REAL,    ALLOCATABLE, SAVE :: EP_PART ( :,: )     !
         INTEGER, ALLOCATABLE, SAVE :: EM_MAP  ( :,: )     !
         INTEGER, ALLOCATABLE, SAVE :: NUM_MAP ( : )       !
         INTEGER, ALLOCATABLE, SAVE :: SRF_MAP ( : )       !
         INTEGER, ALLOCATABLE, SAVE :: WRSS_MAP( : )       !

C Local variables for sea salt emission rates
         REAL :: CR_SSM( N_AEROSPC,NSSMOD )   ! mass
         REAL :: CR_SSN( NSSMOD )             ! number
         REAL :: CR_SSS( NSSMOD )             ! surface area
         
C     Factor for converting aerosol emissions from input units ...
         REAL CONVEM_PM_MASS                       ! into [ug/sec]
         REAL, ALLOCATABLE, SAVE :: CONVEM_PM( : ) ! into [ug/m2/sec]
         REAL( 8 ) :: GSFAC                        ! into [ug/m3/sec]

C     Variables interpolated from the meteorological input files
         REAL DENS( NCOLS,NROWS,NLAYS )        ! air density [kg/m3]

C     Variables for converting emission rates into molar-mixing-ratio units
         REAL, PARAMETER :: GPKG = 1.0E+03     ! g/kg
         REAL, PARAMETER :: MGPG = 1.0E+06     ! ug/g
         REAL, PARAMETER :: RAVO = 1.0 / AVO   ! reciprocal of Avogadro
         REAL( 8 ) :: CONVM     ! conversion factor for mass emissions [m3/mol]
         REAL( 8 ) :: CONVN     ! conversion factor for number emissions [1e6*m3]
         REAL( 8 ) :: CONVS     ! conversion factor for surface area emissions [m3/mol]

C     Domain decomposition info from emission and meteorology files
         INTEGER  GXOFF, GYOFF          ! origin offset
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
         INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C     Variables for handling vapor-phase sulfuric acid emissions
         INTEGER, SAVE :: VSULF               ! index to H2SO4 in VDEMIS array

C     Third moment emissions rates [m3/m3/s]
         REAL( 8 ) :: EMISM3( N_MODE )

C     Number emissions rates [1/m3/s]
         REAL( 8 ) :: EM_NUM( N_MODE )

C     Surface area emission rates [m2/m3/s]
         REAL( 8 ) :: EM_SRF( N_MODE )

C     Miscellaneous variables
         LOGICAL, SAVE     :: FIRSTIME = .TRUE.
         INTEGER, SAVE     :: LOGDEV
         INTEGER, EXTERNAL :: INDEX1, TRIMLEN
         INTEGER INDX
         INTEGER STATUS                   ! ENV..., ALLOCATE status
         CHARACTER( 80 ) :: VARDESC       ! environment variable description
         CHARACTER( 96 ) :: XMSG = ' '
         CHARACTER( 16 ), SAVE :: PNAME = 'GET_AERO_EMIS'
         CHARACTER( 16 ) :: VNAME         ! temp var for species names
         INTEGER C, R, L, N, V, S         ! Loop indices
         INTEGER IDX
         REAL( 8 ) :: FINETOT
#ifdef Verbose
         real sumem
#endif

C ----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            LOGDEV = INIT3()

            IF ( INDEX ( MECHNAME, 'AE4' ) .GT. 0 ) THEN
               AE_VRSN  = 'AE4'
            ELSE IF ( INDEX ( MECHNAME, 'AE5' ) .GT. 0 ) THEN
               AE_VRSN  = 'AE5'
            ELSE
               XMSG = 'This version of the emission processing code '
     &             // 'can only be used with the AE4 or AE5 '
     &             // 'aerosol mechanisms.'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF ! check on MECHNAME

C *** Set indices for the VDEMIS_AE array using the AE_EMIS table
C     This is the subset of the AE_SPC table that have emissions
!           INCLUDE 'aero_index.ext'

C *** set up for sea-salt emission processing
            IF ( .NOT. SSEMIS_INIT( JDATE, JTIME, TSTEP( 1 ) ) ) THEN
               XMSG = 'Failure initializing sea-salt emission processing'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            ALLOCATE ( AE_EM_MW  ( N_AE_EMIS ),
     &                 R_AE_EM_MW( N_AE_EMIS ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = '*** AE_EM_MW or R_AE_EM_MW memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

C *** Save array of MW reciprocals [R_AE_EM_MW( V ) required for pa_update.F]
            DO V = 1, N_AE_EMIS
               AE_EM_MW( V ) = AE_MOLWT( AE_EMIS_MAP( V ) )
               R_AE_EM_MW( V ) = 1.0 / AE_EM_MW( V )
            END DO

C *** Calculate factors for converting 3rd moment emission rates into number
C     emission rates.  See Equation 7b of Binkowski & Roselle (2003)
            DO N = 1, N_MODE
               FACNUM( N )  = EXP( 4.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N ) ** 3
            END DO

C *** Calculate factors for converting 3rd moment emission rates into 2nd 
C     moment emission rates.  See Equation 7c of Binkowski & Roselle (2003)
            DO N = 1, N_MODE
!              FACTM2( N )  = EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
               FACSRF( N )  = PI * EXP( 0.5 * LOG( SGEM( N ) ) ** 2 ) / DGVEM( N )
            END DO

C *** Find location of sulfuric acid vapor in VDEMIS array
            VNAME = 'SULF'
            INDX = INDEX1 ( VNAME, N_GC_EMIS, GC_EMIS )
            IF ( INDX .NE. 0 ) THEN
               VSULF = INDX  ! index for vapor-phase H2SO4 emissions
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
            
            ALLOCATE ( EMBUFF( NCOLS,NROWS,EMLYRS,N_EMIS_PM ),
     &                 STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** EMBUFF memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( CONVEM_PM( EMLYRS ),
     &                 CELLHGT( EMLYRS ),
     &                 CELLVOL( EMLYRS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** CONVEM_PM, CELLHGT or CELLVOL memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            
C *** Allocate memory for EM_RATE, EP_PART, EM_MAP, NUM_MAP, SRF_MAP, and WRSS_MAP
            ALLOCATE ( EM_RATE( N_EMIS_PM ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** EM_RATE memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( EP_PART( N_AEROSPC, N_MODE ),
     &                 EM_MAP(  N_AEROSPC, N_MODE ),
     &                 NUM_MAP( N_MODE ),
     &                 SRF_MAP( N_MODE ),
     &                 WRSS_MAP( N_AEROSPC ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** memory allocation failed for EP_PART, EM_MAP, NUM_MAP, SRF_MAP, or WRSS_MAP'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

C *** Map indices of emission species
            EM_MAP = 0   ! array assignment
            DO S = 1, N_AEROSPC
               IF ( AEROSPC( S )%EMIS .NE. ' ' ) THEN
                  ! Verify that aerospc( S )%emis is a pm_emis
                  IDX = FINDPMEMIS( AEROSPC( S )%EMIS )
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
     &                 // ' in AE_EMIS table'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               END IF
            END DO

C *** Map aerosol surface emission species
            SRF_MAP = 0
            DO N = 1, N_MODE
               SRF_MAP( N ) = INDEX1( AEROMODE( N )%SRF_NAME, N_AE_EMIS, AE_EMIS )
               IF ( SRF_MAP( N ) .EQ. 0 ) THEN
                  XMSG = 'Could not find ' // TRIM( AEROMODE( N )%SRF_NAME )
     &                 // ' in AE_EMIS table'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               END IF
            END DO

C *** Map sea salt emission species
            WRSS_MAP = 0
            DO S = 1, NSSSPC
               IDX = FINDAERO( WRSS_SPC( S ), .FALSE. )
               IF ( IDX .NE. 0 ) THEN
                  WRSS_MAP( IDX ) = S
                  DO N = 1, N_MODE   ! add SEASALT indices to EM_MAP
                     IF ( AEROSPC( IDX )%NAME( N ) .NE. ' ' ) THEN
                        EM_MAP( IDX,N ) = INDEX1( AEROSPC( IDX )%NAME( N ), N_AE_EMIS, AE_EMIS )
                     END IF
                  END DO
               END IF
            END DO

C *** Map data modules
            CALL MAP_PRECURSOR()
            CALL MAP_AERO()

C *** Calculate length and width of each grid cell
C     note: crude estimate is made for LAT/LONG coordinate systems
            IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
               DX1 = DG2M * XCELL_GD ! in m
               DX2 = DG2M * YCELL_GD
     &             * COS( PI180*( YORIG_GD + YCELL_GD
     &             * FLOAT( GL_NROWS/2 ) ) ) ! in m
            ELSE
               DX1 = XCELL_GD        ! in m
               DX2 = YCELL_GD        ! in m
            END IF

C *** Calculate height of grid cell in each layer in sigma coordinates
C     Multiply by grid area [m2] to obtain grid volume
            CELLAREA = DX1 * DX2
            DO L = 1, EMLYRS
               CELLHGT( L ) = X3FACE_GD( L ) - X3FACE_GD( L-1 )
               CELLVOL( L ) = CELLHGT( L ) * CELLAREA
            END DO

C *** Calculate scaling factor for converting aerosol emissions from
C     their input units to [ug/s] and then to [ug/m2/s] using layer-
C     specific grid-cell volume
            IF ( UNITSPM( 1 ) .EQ. 'G/S' .OR.
     &           UNITSPM( 1 ) .EQ. 'g/s' ) THEN
               CONVEM_PM_MASS = MGPG                  ! (g/s) -> (ug/s)
            ELSE IF ( UNITSPM( 1 ) .EQ. 'KG/HR' .OR.
     &                UNITSPM( 1 ) .EQ. 'kg/hr' ) THEN
               CONVEM_PM_MASS = GPKG * MGPG / 3600.0  ! (kg/hr) -> (ug/s)
            ELSE
               XMSG = 'Units incorrect on ' // EMIS_1
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            DO L = 1, EMLYRS
               CONVEM_PM( L ) = CONVEM_PM_MASS / CELLVOL( L )
            END DO

C *** Get domain decomposition info from input files

            CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF,
     &                      STARTCOL, ENDCOL, STARTROW, ENDROW )
            CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                      STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         END IF    ! FIRSTIME

C ----------------------------------------------------------------------

C *** Read aerosol emission rates from file and interpolate to the current
C     time.  Store result in EMBUFF array.

         IF ( PT3DEMIS ) THEN   ! in-line plume rise

            EMBUFF = 0.0   ! array assignment

            DO N = 1, N_EMIS_PM   ! read 2d file
               IF ( .NOT. INTERPX( EMIS_1, PM_EMIS( N )%NAME, PNAME,
     &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,1,
     &                             JDATE, JTIME, EMBUFF( 1,1,1,N ) ) ) THEN
                  XMSG = 'Could not read '
     &                 // PM_EMIS( N )%NAME( 1:TRIMLEN( PM_EMIS( N )%NAME ) )
     &                 // ' from ' // EMIS_1
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG  )
               END IF
            END DO

            DO N = 1, N_SPC_PTPM   ! merge inline point src PM species
               V = PTPM_MAP( N )   ! set in PT3D_DEFN
               DO L = 1, EMLYRS
                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                     EMBUFF( C,R,L,N ) = EMBUFF( C,R,L,N ) + PMEMIS_PT( C,R,L,V )
                     END DO
                  END DO
               END DO
            END DO

         ELSE   ! read 3d file

            DO N = 1, N_EMIS_PM
               IF ( .NOT. INTERPX( EMIS_1, PM_EMIS( N )%NAME, PNAME,
     &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                             JDATE, JTIME, EMBUFF( 1,1,1,N ) ) ) THEN
                  XMSG = 'Could not read '
     &                 // PM_EMIS( N )%NAME( 1:TRIMLEN( PM_EMIS( N )%NAME ) )
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

         VDEMIS_AE = 0.0   ! array assignment

C *** Loop over each grid cell in layer 1

         L = 1
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

C *** Store aerosol emission rates for this grid cell in a 1D array

               DO N = 1, N_EMIS_PM
                  PM_EM( N ) = EMBUFF( C,R,L,N )
               END DO

C Get grid cell values for sea salt emission rates
C *** Convert units of SSOUTM from [g/m3/s] to [ug/m3/s].
               CR_SSM = 0.0   ! array assignment
               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     IF ( WRSS_MAP( S ) .NE. 0 )
     &                  CR_SSM( S,N ) = SSOUTM( WRSS_MAP( S ),N,C,R ) * MGPG
                  END DO
                  CR_SSN( N ) = SSOUTN( N,C,R )
                  CR_SSS( N ) = SSOUTS( N,C,R )
               END DO

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
               GSFAC = CONVEM_PM( L ) * RJACM( C,R,L )
               CONVM = MWAIR / GPKG / DENS( C,R,L )   ! [m3/mol]
               CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
               CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
               DO N = 1, N_EMIS_PM
                  EM_RATE( N ) = PM_EM( N ) * GSFAC
               END DO

C *** If INDX10 is non-zero, the emission file did not contain PMC so 
C     PM_EM(VPMC) holds PM10 emissions.  Substract the PM2.5 species
C     from this so PM_EM(VPMC) holds only the coarse emission rate.
               IF ( INDX10 .NE. 0 ) THEN
                  FINETOT = 0.0
                  DO N = 1, N_EMIS_PM
                     IF ( PM_EMIS( N )%EMIS_SPLIT( 2 ) .GT. 0.0 )
     &                  FINETOT = FINETOT + EM_RATE( N )
                  END DO
                  DO N = 1, N_EMIS_PM
                     IF ( PM_EMIS( N )%EMIS_SPLIT( N_MODE ) .GT. 0.0 )
     &                  EM_RATE( N ) = MAX( 0.0, EM_RATE( N ) - FINETOT )
                  END DO
               END IF
                  

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
               EM_RATE( VPSO4 ) = EM_RATE( VPSO4 ) + VDEMIS( VSULF,L,C,R )
     &                          * PRECURSOR_MW( SULF_IDX ) / CONVM
               VDEMIS( VSULF,L,C,R ) = 0.0

C *** Assign fine-particle emission rates to the fine modes.
C     Assume all non-seasalt-emissions of sulfate, nitrate, and unspeciated
C     fine PM are in the accumulation mode.  Split the carbon emissions 
C     between the Aitken and accumulation modes, as described in paragraph 
C     #12 of Binkowski & Roselle (2003).


C *** Calculate emissions rate for third moments [m3/m3/s] of each mode 
C     (excluding sea salt), as in Equation 7a of Binkowski & Roselle (2003).

               EP_PART = 0.0   ! array assignment
               EMISM3 = 0.0    ! array assignment
               DO S = 1, N_AEROSPC
                  IF ( AEROSPC( S )%EMIS .NE. ' ' ) THEN
                     IDX = FINDPMEMIS( AEROSPC( S )%EMIS )
                     DO N = 1, N_MODE
                        EP_PART( S,N ) = PM_EMIS( IDX )%EMIS_SPLIT( N )
     &                                 * EM_RATE( IDX )
                        IF ( AEROSPC( S )%TRACER ) CYCLE
                        EMISM3( N ) = EMISM3( N ) + EP_PART( S,N )
     &                              * ( F6DPIM9 / AEROSPC( S )%DENSITY )
                     END DO
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

C *** Units of CR_SSM are ug/m3/s].  Transfer Sea-salt mass emissions
C     to mode- and species-specific variables.  Sea-salt sulfate in the fine
C     modes must be added to the anthropogenic sulfate emissions.  Remaining
C     sea-salt species are not in the standard inventory.
               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     EP_PART( S,N ) = EP_PART( S,N ) + CR_SSM( S,N )
                  END DO
               END DO

C *** Add sea salt to the total emissions of particle number and surface area
               DO N = 1, N_MODE
                  EM_NUM( N ) = EM_NUM( N ) + CR_SSN( N )
                  EM_SRF( N ) = EM_SRF( N ) + CR_SSS( N )
               END DO

C *** Convert emission rates into molar-mixing-ratio units, as required by
C     the vertical diffusion routines.  Mass and number emissions are 
C     converted to [ppmv/s].  Surface area emissions are converted to 
C     [m2/mol/s].  Save results in the VDEMIS_AE array.

               DO N = 1, N_MODE
                  DO S = 1, N_AEROSPC
                     IF ( EM_MAP( S,N ) .GT. 0 ) THEN
                        VDEMIS_AE( EM_MAP( S,N ),L,C,R ) = EP_PART( S,N )
     &                                                   * ( CONVM / AEROSPC_MW( S ) )
                     END IF
                  END DO
                     VDEMIS_AE( NUM_MAP( N ),L,C,R ) = EM_NUM( N ) * CONVN
                     VDEMIS_AE( SRF_MAP( N ),L,C,R ) = EM_SRF( N ) * CONVS
               END DO

            END DO   ! loop on MY_NCOLS
         END DO   ! loop on MY_NROWS

         DO L = 2, EMLYRS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS

C *** Store aerosol emission rates for this grid cell in a 1D array
                  DO N = 1, N_EMIS_PM
                     PM_EM( N ) = EMBUFF( C,R,L,N )
                  END DO

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
                  GSFAC = CONVEM_PM( L ) * RJACM( C,R,L )
                  CONVM = MWAIR / GPKG / DENS( C,R,L )   ! [m3/mol]
                  CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
                  CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
                  DO N = 1, N_EMIS_PM
                     EM_RATE( N ) = PM_EM( N ) * GSFAC
                  END DO

C *** If INDX10 is non-zero, the emission file did not contain PMC so 
C     PM_EM(VPMC) holds PM10 emissions.  Substract the PM2.5 species
C     from this so PM_EM(VPMC) holds only the coarse emission rate.
                  IF ( INDX10 .NE. 0 ) THEN
                     FINETOT = 0.0
                     DO N = 1, N_EMIS_PM
                        IF ( PM_EMIS( N )%EMIS_SPLIT( 2 ) .GT. 0.0 )
     &                     FINETOT = FINETOT + EM_RATE( N )
                     END DO
                     DO N = 1, N_EMIS_PM
                        IF ( PM_EMIS( N )%EMIS_SPLIT( N_MODE ) .GT. 0.0 )
     &                     EM_RATE( N ) = MAX( 0.0, EM_RATE( N ) - FINETOT )
                     END DO
                  END IF
                  

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
                  EM_RATE( VPSO4 ) = EM_RATE( VPSO4 ) + VDEMIS( VSULF,L,C,R )
     &                             * PRECURSOR_MW( SULF_IDX ) / CONVM
                  VDEMIS( VSULF,L,C,R ) = 0.0

C *** Calculate emissions rate for third moments [m3/m3/s] of each mode 
C     (excluding sea salt), as in Equation 7a of Binkowski & Roselle (2003).

                  EP_PART = 0.0   ! array assignment
                  EMISM3 = 0.0    ! array assignment
                  DO S = 1, N_AEROSPC
                     IF ( AEROSPC( S )%EMIS .NE. ' ' ) THEN
                        IDX = FINDPMEMIS( AEROSPC( S )%EMIS )
                        DO N = 1, N_MODE
                           EP_PART( S,N ) = PM_EMIS( IDX )%EMIS_SPLIT( N )
     &                                    * EM_RATE( IDX )
                           IF ( AEROSPC( S )%TRACER ) CYCLE
                           EMISM3( N ) = EMISM3( N ) + EP_PART( S,N )
     &                                 * ( F6DPIM9 / AEROSPC( S )%DENSITY )
                        END DO
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
                           VDEMIS_AE( EM_MAP( S,N ),L,C,R ) = EP_PART( S,N )
     &                                                      * CONVM / AEROSPC_MW( S )
                        END IF
                     END DO
                        VDEMIS_AE( NUM_MAP( N ),L,C,R ) = EM_NUM( N ) * CONVN
                        VDEMIS_AE( SRF_MAP( N ),L,C,R ) = EM_SRF( N ) * CONVS
                  END DO

               END DO   ! loop on MY_NCOLS
            END DO   ! loop on MY_NROWS
         END DO   ! loop on EMLYRS

#ifdef Verbose
         write( logdev,'(/"@@@",i8,i7.6)') jdate, jtime
         do n = 1, n_ae_emis
            sumem = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumem = sumem + vdemis_ae( n,1,c,r )
               end do
            end do
            write( logdev,'("@@@ ",a,g16.6)') ae_emis( n ), sumem
         end do
#endif

         RETURN

         END SUBROUTINE GET_AERO_EMIS

      END MODULE AERO_EMIS
      
