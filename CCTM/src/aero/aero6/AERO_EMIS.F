
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

      USE AERO_DATA
      USE EMIS_VARS
      USE UDTYPES, ONLY: IARRY1

      IMPLICIT NONE
      SAVE 
C aerosol emissions: [ppmv/s] for mass & number spcs, [m2/mol/s] for surface area spcs
      CHARACTER( 16 )      :: CTM_WB_DUST = 'CTM_WB_DUST' ! env var for windblown dust
      LOGICAL              :: WB_DUST   ! flag for windblown dust option

      PUBLIC EMISS_SIZE_DIST, AERO_EMIS_INIT,
     &       MAP_AEROtoEM, MAP_EMtoAERO, MAP_EMtoMODE, MAP_NUMtoEM, MAP_SRFtoEM,
     &       MAP_EMtoNUM, MAP_EMtoSRF, MAP_EMtoSD, EM_SRC_SD, SD_SPLIT,
     &       CTM_WB_DUST, WB_DUST, CELLVOL, CELLHGT, EM_SD_INIT, SD_RULE
      PRIVATE

C Variables for converting mass emissions rate to number emissions rate
      REAL   :: FACNUM( N_EM_AERO_REF,N_MODE )

C Variables for converting mass emissions rate to 2nd moment emissions rate
      REAL    :: FACSRF( N_EM_AERO_REF,N_MODE )

C Variables for Saving split factors between emission modes
      REAL, ALLOCATABLE :: SD_SPLIT( :,: )

C Define Structure for Holding Emissions Aerosol Mode Information
C Globally
      TYPE SRC_MODE
         INTEGER  :: LEN
         INTEGER,         ALLOCATABLE :: REF( : )
         CHARACTER( 16 ), ALLOCATABLE :: NAME( : )
         REAL,            ALLOCATABLE :: FACNUM( :,: )
         REAL,            ALLOCATABLE :: FACSRF( :,: )
      END TYPE SRC_MODE
      TYPE( SRC_MODE ), ALLOCATABLE :: EM_SRC_SD( : )

C Define Structure for Size Distribution Rule      
      TYPE SD_RULE
             CHARACTER( 32 ) :: SRC
             CHARACTER( 16 ) :: MODE
             CHARACTER( 16 ) :: MODE_REF
      END TYPE SD_RULE

C Variables for the thickness and volume of each grid cell
      REAL,    ALLOCATABLE :: CELLHGT( : ) ! grid-cell height [sigma]
      REAL,    ALLOCATABLE :: CELLVOL( : ) ! grid-cell volume [m2*sigma]
 
C Emission rate of all aerosol species interpolated to current time
      INTEGER, ALLOCATABLE :: MAP_AEROtoEM( :,: )   
      INTEGER, ALLOCATABLE :: MAP_EMtoAERO( : )   
      INTEGER, ALLOCATABLE :: MAP_EMtoMODE( : )   
      INTEGER, ALLOCATABLE :: MAP_NUMtoEM ( : )  
      INTEGER, ALLOCATABLE :: MAP_SRFtoEM ( : )   
      INTEGER, ALLOCATABLE :: MAP_EMtoNUM ( : )  
      INTEGER, ALLOCATABLE :: MAP_EMtoSRF ( : )   
      INTEGER, ALLOCATABLE :: MAP_EMtoSD  ( :,: ) 
      INTEGER, ALLOCATABLE :: MAP_AEROtoDIFF( :,: )     ! indices of aero species to CGRID

C DUST_MAP   windblown dust emission rate index in DUSTOUTM

C Factor for converting aerosol emissions from input units ...
      REAL,    ALLOCATABLE :: CONVEM_PM( :,: ) ! into [ug/m2/sec]

C Domain decomposition info from emission and meteorology files
      INTEGER :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

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
         USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_SPC
         USE EMIS_VARS, ONLY : MAP_EMtoSURR
     
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
         REAL  DGV, SG, SPLIT_ACCUM

C Variables for calculating the volume of each grid cell
         REAL  DX1, DX2                            ! grid-cell width and length [m]
         REAL  CELLAREA                            ! grid-cell area [m2]

C Domain decomposition info from emission and meteorology files
         INTEGER GXOFF, GYOFF          ! origin offset

C Miscellaneous variables
         INTEGER STATUS                   ! ENV..., ALLOCATE status
         CHARACTER( 16 ), SAVE :: PNAME = 'AERO_EMIS_INIT  '
         CHARACTER( 16 ) :: VNAME         ! temp var for species names
         CHARACTER( 50 ) :: VARDESC       ! variable for reading environ. variables
         INTEGER L, N, S, V, IAERO, ISRC, ! Loop indices
     &           IEM, IDIFF, ISPC

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

C *** Set up emissions size distribution arrays
      ! Calculate factors for converting 3rd moment emission rates into 
      ! number and surface area emission rates.  See Equation 7b of 
      ! Binkowski & Roselle (2003)
         DO IEM = 1,N_EM_AERO_REF
           DO N = 1, N_MODE
              DGV = EM_AERO_REF( IEM )%DGVEM( N )
              SG  = EM_AERO_REF( IEM )%SGEM ( N )

              IF ( EM_AERO_REF( IEM )%SPLIT( N ) .GT. 0.0 ) THEN
                FACNUM( IEM,N ) = EXP( 4.5 * LOG( SG ) ** 2 ) / DGV ** 3
                FACSRF( IEM,N ) = PI * EXP( 0.5 * LOG( SG ) ** 2 ) / DGV
              ELSE
                FACNUM( IEM,N ) = 0.0
                FACSRF( IEM,N ) = 0.0
              END IF
           END DO

         END DO

       ! Map the Modal-Dependent Names to Transported Species
       ALLOCATE ( MAP_AEROtoDIFF( N_AEROSPC, N_MODE ) )
       DO ISPC = 1,N_AEROSPC
         DO N = 1,N_MODE
           MAP_AEROtoDIFF( ISPC, N ) = INDEX1( AEROSPC( ISPC )%name( N ), 
     &                                 N_SPC_DIFF, DIFF_SPC )
         END DO
       END DO

     
       ! Modify the reference emissions splits based on what transported 
       ! aerosol species are actually available. For exmaple, if the aerosol
       ! namelist only includes the accumulation mode (J) but not the 
       ! Aitken mode (I) for a particular species, then the split for
       ! Aitken mode should be added to the Accumulation mode. Save
       ! these scale factors as a function of transported species and
       ! mode. Note that this 
       ALLOCATE( SD_SPLIT( N_SPC_DIFF, N_EM_AERO_REF ) )
       DO ISPC = 1,N_AEROSPC
         DO IEM = 1,N_EM_AERO_REF
           ! Gather the split factors going from the smallest mode to
           ! the largest
           SPLIT_ACCUM = 0.0
           DO N = 1,N_MODE
              IF ( AERO_MISSING( ISPC,N ) ) THEN
                SPLIT_ACCUM = SPLIT_ACCUM + EM_AERO_REF( IEM )%SPLIT( N )
                SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 0.0
              ELSE
                SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 
     &             EM_AERO_REF( IEM )%SPLIT( N ) + SPLIT_ACCUM
                SPLIT_ACCUM = 0.0
              END IF
           END DO
           ! Now gather going from the largest to smallest mode
           SPLIT_ACCUM = 0.0
           DO N = N_MODE,1,-1
              IF ( AERO_MISSING( ISPC,N ) ) THEN
                SPLIT_ACCUM = SPLIT_ACCUM + EM_AERO_REF( IEM )%SPLIT( N )
                SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 0.0
              ELSE
                SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 
     &            EM_AERO_REF( IEM )%SPLIT( N ) + SPLIT_ACCUM
                SPLIT_ACCUM = 0.0
              END IF
           END DO 

         END DO
       END DO

C *** Allocate memory for EMBUFF, CELLHGT, CELLVOL, and CONVEM_PM
         ALLOCATE ( CONVEM_PM( EMLAYS, N_EM_SRC ),
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

            DO ISRC = 1,N_EM_SRC
              CONVEM_PM( L,ISRC ) = CONVEM_PM_MASS( ISRC ) / CELLVOL( L )
            END DO
         END DO

         

         END FUNCTION  AERO_EMIS_INIT

C-----------------------------------------------------------------------

       SUBROUTINE EM_SD_INIT ( SD_NML )

C  EM_SD_INIT initializes the structures that map modes and sources to
C  reference modes including splits, diameters, and standard deviations.

C-----------------------------------------------------------------------
       USE EMIS_VARS
       USE AERO_DATA
       USE UTILIO_DEFN
         
       IMPLICIT NONE

       INTEGER ISRC
       
       INTEGER                          :: N_SD_RULE
       TYPE( SD_RULE ), INTENT( IN )    :: SD_NML( : )
       CHARACTER( 20 )                  :: SD_FINE = 'FINE_REF'
       CHARACTER( 20 )                  :: SD_COARSE = 'COARSE_REF'
       INTEGER                          :: N_SD( N_EM_SRC )
       CHARACTER( 16 )                  :: SD_NAME( N_EM_SRC, 10 )
       INTEGER                          :: SD( N_EM_SRC, 10 )
       LOGICAL                          :: EM_SRC_RULE( N_EM_SRC )
       CHARACTER( 16 )                  :: CSUR

       INTEGER IRULE, ISUR, N, NLEN, ISD, IM, IEM
       
       N_SD_RULE = SIZE( SD_NML )
       CALL UPCASE( SD_FINE )
       CALL UPCASE( SD_COARSE )

       ! First Load all of the Sources with the Default FINE and COARSE
       ! Mode references
       DO ISRC = 1,N_EM_SRC
         N_SD( ISRC ) = 2
         SD_NAME( ISRC,1 ) = 'FINE'
         SD( ISRC,1 ) = INDEX1( SD_FINE, N_EM_AERO_REF, EM_AERO_REF( : )%NAME )
         SD_NAME( ISRC,2 ) = 'COARSE'
         SD( ISRC,2 ) = INDEX1( SD_COARSE, N_EM_AERO_REF, EM_AERO_REF( : )%NAME )
       END DO
       
       ! Now Modify those defaults or add new modes to desired sources
       DO IRULE = 1,N_SD_RULE
         ! Expand Size Distribution Rule to All Sources if Requested
         CALL INTERPRET_EM_RULE_SRC( SD_NML( IRULE )%SRC, EM_SRC_RULE )

         ! Loop through sources, set defaults, and build map array
         DO ISRC = 1,N_EM_SRC
            IF ( EM_SRC_RULE( ISRC ) ) THEN
               ! This Source is Being Modified by a Size Distribution
               ! rule
               IF ( SD_NML( IRULE )%MODE .EQ. 'FINE' ) THEN
                   ! Overwrite the FINE mode
                   SD( ISRC,1 ) = INDEX1( SD_NML( IRULE )%MODE_REF,
     &                               N_EM_AERO_REF, EM_AERO_REF( : )%NAME )
               ELSEIF ( SD_NML( IRULE )%MODE .EQ. 'COARSE' ) THEN
                   ! Overwrite the COARSE mode
                   SD( ISRC,2 ) = INDEX1( SD_NML( IRULE )%MODE_REF,
     &                               N_EM_AERO_REF, EM_AERO_REF( : )%NAME )
               ELSE
                   ! Add a New Available Mode
                   N_SD( ISRC ) = N_SD( ISRC ) + 1
                   SD_NAME( ISRC,N_SD( ISRC ) ) = SD_NML( IRULE )%MODE
                   SD( ISRC,N_SD( ISRC ) ) = INDEX1( SD_NML( IRULE )%MODE_REF,
     &                                 N_EM_AERO_REF, EM_AERO_REF( : )%NAME )

               END IF
           END IF
         END DO
      END DO
 
      ! Finally, transfer this data to a global variable which
      ! captures and organizes the modes of each source
      ALLOCATE( EM_SRC_SD( N_EM_SRC ) )
      DO ISRC = 1,N_EM_SRC
          N = N_SD( ISRC )
          EM_SRC_SD( ISRC )%LEN = N + 1
          ALLOCATE( EM_SRC_SD( ISRC )%NAME( N+1 ) )
          ALLOCATE( EM_SRC_SD( ISRC )%REF( N+1 ) )
          ALLOCATE( EM_SRC_SD( ISRC )%FACNUM( N+1,N_MODE ) )
          ALLOCATE( EM_SRC_SD( ISRC )%FACSRF( N+1,N_MODE ) )

          EM_SRC_SD( ISRC )%NAME( 2:N+1 ) = SD_NAME( ISRC,1:N )
          EM_SRC_SD( ISRC )%REF( 2:N+1 )  = SD( ISRC,1:N )
          EM_SRC_SD( ISRC )%NAME( 1 ) = 'GAS'
          EM_SRC_SD( ISRC )%REF( 1 )  = 0

          ! Map Factors for Converting Aerosol Mass to Number and
          ! Surface Area to each Emission Source
          DO ISD = 1,N
             IEM = EM_SRC_SD( ISRC )%REF( ISD )
             DO IM = 1,N_MODE
                 EM_SRC_SD( ISRC )%FACNUM( ISD+1,IM ) = FACNUM( IEM,IM ) 
                 EM_SRC_SD( ISRC )%FACSRF( ISD+1,IM ) = FACSRF( IEM,IM )
             END DO
          END DO
      END DO

      END SUBROUTINE EM_SD_INIT    


C-----------------------------------------------------------------------

         SUBROUTINE EMISS_SIZE_DIST ( VDEMIS_AE, ISRC )

C  EMISS_SIZE_DIST distributes bulk aerosol emissions into size space
C  using parameters precompiled in the AERO_DATA module. 
C
C  Revision History:

C   16 AUG 17 BMURPHY: Created
C                      
C ----------------------------------------------------------------------

         USE GRID_CONF
         USE AERO_DATA, ONLY : AEROSPC 
         USE AEROMET_DATA
         USE ASX_DATA_MOD, ONLY: MET_DATA
         USE EMIS_VARS

         REAL,    INTENT( INOUT )  :: VDEMIS_AE( :,:,:,: )
         INTEGER, INTENT( IN    )  :: ISRC

         INTEGER :: N, S, IAERO, IM, ISPC, ISD, ISTR   ! Looping Variables
         INTEGER :: ROW, COL, LAY, N_SD
         REAL    :: WT
         REAL, ALLOCATABLE :: EMISM3( :,:,:,:,: ), GSFAC( :,:,: )
         REAL, PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI  ! 1.0E-9 = Kg/ug


C *** Initialize Variables  
         N_SD        = EM_SRC_SD( ISRC )%LEN 

         ALLOCATE( EMISM3( EMLAYS,MY_NCOLS,MY_NROWS,N_MODE,N_SD ) )
         ALLOCATE( GSFAC( EMLAYS,MY_NCOLS,MY_NROWS ) )
         EMISM3 = 0.0

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
         DO LAY = 1,EMLAYS
           DO COL = 1,MY_NCOLS
             DO ROW = 1,MY_NROWS
               GSFAC( LAY,COL,ROW ) = CONVEM_PM( LAY,ISRC ) / CONVEM( ISRC )
     &                              * Met_Data%RJACM( COL,ROW,LAY ) ![g/s] or [kg/hr] to [ug/m3/s]
             END DO
           END DO
         END DO
 
C *** Apply Aerosol Size Distribution
         DO ISTR = 1,N_EMIS_ISTR
             IAERO = MAP_EMtoAERO( ISTR )   !This maps to the CMAQ aerosol
                                            !  species so we can retrieve density
             IM    = MAP_EMtoMODE( ISTR )   !This maps to the internal CMAQ modes (ie. I, J, and K)
             ISD   = MAP_EMtoSD( ISTR,ISRC )!This maps to the emissions modes defined for each source
             IF ( IAERO .EQ. 0 ) CYCLE
             
             ! Convert Aerosol from [kg/m3/s] to [ug/m3/s]
             VDEMIS_AE( ISPC,:,:,: ) = VDEMIS_AE( ISPC,:,:,: ) * GSFAC( :,:,: )

             ! Sum Total Volume of Mode N [m3/m3/s]
             IF ( .NOT. AEROSPC( IAERO )%TRACER ) 
     &             EMISM3( :,:,:,IM,ISD ) = EMISM3( :,:,:,IM,ISD ) + 
     &                  VDEMIS_AE( ISPC,:,:,: ) * ( F6DPIM9 / AEROSPC( IAERO )%DENSITY )

             ! Convert Mass Emission Rates from [ug/m3/s] to [umol/m3/s]
             VDEMIS_AE( ISPC,:,:,: ) = VDEMIS_AE( ISPC,:,:,: ) / AEROSPC_MW( IAERO )
         END DO          

C *** Calculate the number emissions rate for each mode [1/m3/s], using 
C     Equation 7b of Binkowski & Roselle (2003).
C     Calculate the surface area emissions rate for the fine modes [m2/m3/s],
C     using Equation 7c of Binkowski & Roselle (2003).  Multiplying by PI 
C     converts 2nd moment to surface area.
         IF ( ISRC .EQ. ISEASRC ) THEN

           DO IM = 1,N_MODE
              VDEMIS_AE( MAP_NUMtoEM(IM),1,:,: ) = VDEMIS_AE( MAP_NUMtoEM(IM),1,:,: ) 
     &                + SSOUTN( IM,:,: )   
              VDEMIS_AE( MAP_SRFtoEM(IM),1,:,: ) = VDEMIS_AE( MAP_SRFtoEM(IM),1,:,: ) 
     &                + SSOUTS( IM,:,: )   
           END DO

         ELSE

           DO ISD = 2,N_SD   ! Skip the Index for the Gas Phase
           DO IM = 1,N_MODE
              VDEMIS_AE( MAP_NUMtoEM(IM),:,:,: ) = VDEMIS_AE( MAP_NUMtoEM(IM),:,:,: ) 
     &                + EMISM3( :,:,:,IM,ISD ) * EM_SRC_SD( ISRC )%FACNUM( ISD,IM ) 
              VDEMIS_AE( MAP_SRFtoEM(IM),:,:,: ) = VDEMIS_AE( MAP_SRFtoEM(IM),:,:,: ) 
     &                + EMISM3( :,:,:,IM,ISD ) * EM_SRC_SD( ISRC )%FACSRF( ISD,IM ) 
           END DO
           END DO
         END IF
 
         END SUBROUTINE EMISS_SIZE_DIST
 
      END MODULE AERO_EMIS

