
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

      USE AERO_DATA, ONLY: N_EM_AERO_REF, N_MODE
      USE EMIS_VARS, ONLY: EMLAYS, EM_STREAM_SIZE, N_EM_SRM

      IMPLICIT NONE
      SAVE 
C aerosol emissions: [ppmv/s] for mass & number spcs, [m2/mol/s] for surface area spcs
      PUBLIC EMISS_SIZE_DIST, AERO_EMIS_INIT,
     &       MAP_EMtoAERO, MAP_EMtoMODE, MAP_NUMtoEM, MAP_SRFtoEM,
     &       MAP_EMtoNUM, MAP_EMtoSRF, MAP_EMtoSD, EM_STREAM_SIZE, SD_SPLIT,
     &       CELLVOL, CELLHGT, EM_SD_INIT
      PRIVATE

C Variables for converting mass emissions rate to number emissions rate
      REAL   :: FACNUM( N_EM_AERO_REF,N_MODE )

C Variables for converting mass emissions rate to 2nd moment emissions rate
      REAL    :: FACSRF( N_EM_AERO_REF,N_MODE )

C Variables for Saving split factors between emission modes
      REAL, ALLOCATABLE :: SD_SPLIT( :,: )

C Variables for the thickness and volume of each grid cell
      REAL,    ALLOCATABLE :: CELLHGT( : ) ! grid-cell height [sigma]
      REAL,    ALLOCATABLE :: CELLVOL( : ) ! grid-cell volume [m2*sigma]
 
C Emission rate of all aerosol species interpolated to current time
      INTEGER, ALLOCATABLE :: MAP_EMtoAERO( : )   
      INTEGER, ALLOCATABLE :: MAP_EMtoMODE( : )   
      INTEGER, ALLOCATABLE :: MAP_NUMtoEM ( : )  
      INTEGER, ALLOCATABLE :: MAP_SRFtoEM ( : )   
      INTEGER, ALLOCATABLE :: MAP_EMtoNUM ( : )  
      INTEGER, ALLOCATABLE :: MAP_EMtoSRF ( : )   
      INTEGER, ALLOCATABLE :: MAP_EMtoSD  ( :,: ) 
      INTEGER, ALLOCATABLE :: MAP_AEROtoDIFF( :,: )     ! indices of aero species to CGRID

C DUST_MAP   windblown dust emission rate index in DUSTOUTM

C Domain decomposition info from emission and meteorology files
      INTEGER :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C Miscellaneous variables
      INTEGER :: IDX
      CHARACTER( 200 ) :: XMSG = ' '

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

         USE AERO_DATA, ONLY: EM_AERO_REF, N_AEROSPC, AEROSPC, 
     &                        AERO_MISSING, MAP_AERO
         USE GRID_CONF, ONLY: GDTYP_GD, XCELL_GD, YCELL_GD, YORIG_GD, GL_NROWS, X3FACE_GD
         USE DUST_EMIS, ONLY: DUST_EMIS_INIT
         USE EMIS_VARS, ONLY: MAP_EMtoSURR
         USE PRECURSOR_DATA, ONLY: MAP_PRECURSOR
         USE RUNTIME_VARS, ONLY:  OCEAN_CHEM, WB_DUST
         USE SSEMIS, ONLY:    SSEMIS_INIT
         USE UTILIO_DEFN, ONLY: INDEX1, M3EXIT, LATGRD3, XSTAT1, XSTAT2
         USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_SPC
     
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
         INTEGER L, N, S, V, IAERO, ISRM, ! Loop indices
     &           IEM, IDIFF, ISPC

C ----------------------------------------------------------------------

         SUCCESS = .TRUE.

C *** Map data modules
         CALL MAP_AERO()
         CALL MAP_PRECURSOR()

C *** set up for sea-spray emission processing
         IF ( OCEAN_CHEM ) THEN
            IF ( .NOT. SSEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
               XMSG = 'Failure initializing sea-spray emission processing'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
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
       ! aerosol species are actually available. For example, if the aerosol
       ! namelist only includes the accumulation mode (J) but not the 
       ! Aitken mode (I) for a particular species, then the split for
       ! Aitken mode should be added to the Accumulation mode. Save
       ! these scale factors as a function of transported species and
       ! mode. 
       ALLOCATE( SD_SPLIT( N_SPC_DIFF, N_EM_AERO_REF ) )
       SD_SPLIT = 0.0
       DO IEM = 1,N_EM_AERO_REF
         ! For the Fine Mode Reference Distribution, lump Aitken
         ! with Accumulation mode if Aitken Mode does not exist
         IF ( EM_AERO_REF( IEM )%NAME .EQ. 'FINE_REF' ) THEN
            DO ISPC = 1,N_AEROSPC
              SPLIT_ACCUM = 0.0
              DO N = 1,N_MODE-1
                IF ( AERO_MISSING( ISPC,N ) ) THEN
                  SPLIT_ACCUM = SPLIT_ACCUM + EM_AERO_REF( IEM )%SPLIT( N )
                ELSE
                  SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 
     &               SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) +
     &               EM_AERO_REF( IEM )%SPLIT( N ) + SPLIT_ACCUM
                  SPLIT_ACCUM = 0.0
                END IF
              END DO
            END DO
         ELSE 
            ! Arbitrary Distribution -> Apply factor to species
            ! if it exists in each mode
            DO ISPC = 1, N_AEROSPC
              DO N = 1, N_MODE
                IF ( .NOT. AERO_MISSING( ISPC,N ) ) THEN
                  SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 
     &               EM_AERO_REF( IEM )%SPLIT( N ) 
                END IF
              END DO
            END DO
         END IF
       END DO

C *** Allocate memory for CELLHGT and CELLVOL
         ALLOCATE ( CELLHGT( EMLAYS ),
     &              CELLVOL( EMLAYS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** CELLHGT or CELLVOL memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( MAP_NUMtoEM ( N_MODE ),
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
         END DO

         END FUNCTION  AERO_EMIS_INIT

C-----------------------------------------------------------------------

       SUBROUTINE EM_SD_INIT ( JDATE, JTIME )

C  EM_SD_INIT initializes the structures that map modes and streams to
C  reference modes including splits, diameters, and standard deviations.

C-----------------------------------------------------------------------
       USE AERO_DATA, ONLY: EM_AERO_REF, N_EM_AERO_REF
       USE EMIS_VARS, ONLY: SD_NML, INTERPRET_EM_RULE_STREAM
       USE UTILIO_DEFN, ONLY: INDEX1, XSTAT1
         
       IMPLICIT NONE

       INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
       INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
       INTEGER ISRM
       
       INTEGER                          :: N_SD_RULE
       INTEGER                          :: N_SD( N_EM_SRM )
       CHARACTER( 16 )                  :: SD_NAME( N_EM_SRM, 10 )
       INTEGER                          :: SD( N_EM_SRM, 10 )
       LOGICAL                          :: EM_STREAM_RULE( N_EM_SRM )
       CHARACTER( 16 )                  :: CSUR
       CHARACTER( 16 ), SAVE            :: PNAME = 'EM_SD_INIT  '
       CHARACTER( 20 )                  :: EM_AERO_REF_CAPS( N_EM_AERO_REF )

       INTEGER IRULE, ISUR, N, NLEN, ISD, IM, IEM, NRULE
       LOGICAL   :: LREMOVE
       
       ! Find Total Number of Size Distribution Registries
       N_SD_RULE = SIZE( SD_NML )
       NRULE = 0
       DO IRULE = 1,N_SD_RULE
          IF ( SD_NML( IRULE )%STREAM .EQ. '' ) EXIT
          NRULE = NRULE + 1
       END DO
       N_SD_RULE = NRULE

       ! First Load all of the Streams with the Default FINE, COARSE, and
       ! AERO Mode references
       SD = 0
       SD_NAME = ''

       ! Capitalize EM_AERO_REF(:)%NAME
       DO IM = 1,N_EM_AERO_REF
          EM_AERO_REF_CAPS( IM ) = EM_AERO_REF( IM )%NAME
          CALL UPCASE( EM_AERO_REF_CAPS( IM ) )
       ENDDO

       DO ISRM = 1,N_EM_SRM
         N_SD( ISRM ) = 2
         SD_NAME( ISRM,1 ) = 'FINE'
         SD( ISRM,1 ) = INDEX1( 'FINE_REF',   N_EM_AERO_REF, EM_AERO_REF_CAPS( : ) )
         SD_NAME( ISRM,2 ) = 'COARSE'
         SD( ISRM,2 ) = INDEX1( 'COARSE_REF', N_EM_AERO_REF, EM_AERO_REF_CAPS( : ) )
       END DO
       
       ! Now Modify those defaults or add new modes to desired streams
       DO IRULE = 1, N_SD_RULE
         ! Expand Size Distribution Rule to All Streams if Requested
         LREMOVE = .FALSE.
         IF ( SD_NML( IRULE )%STREAM .EQ. '' ) CYCLE
         CALL INTERPRET_EM_RULE_STREAM( SD_NML( IRULE )%STREAM, IRULE, 
     &                 EM_STREAM_RULE, LREMOVE )
         IF ( LREMOVE ) CYCLE

         ! Loop through streams, set defaults, and build map array
         DO ISRM = 1, N_EM_SRM
            IF ( EM_STREAM_RULE( ISRM ) ) THEN
               ! This Stream is Being Modified by a Size Distribution
               ! rule
               CALL UPCASE( SD_NML( IRULE )%MODE_REF )
               IF ( SD_NML( IRULE )%MODE .EQ. 'FINE' ) THEN
                   ! Overwrite the FINE mode. All fine particle species
                   ! will go to this mode by default
                   SD( ISRM,1 ) = INDEX1( SD_NML( IRULE )%MODE_REF,
     &                               N_EM_AERO_REF, EM_AERO_REF_CAPS( : ) )
                   IF ( SD( ISRM,1 ) .EQ. 0 ) THEN
                      WRITE( XMSG,'(A,A,A,/,A,I2,A)' ), '*** Reference Aerosol Mode (', 
     &                       SD_NML( IRULE )%MODE_REF, 'Specified in Emissions Size ',
     &                       'Dist Rule ',IRULE,' does not exist in AERO_DATA.'
                      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                   END IF

               ELSEIF ( SD_NML( IRULE )%MODE .EQ. 'COARSE' ) THEN
                   ! Overwrite the COARSE mode. All coarse particle
                   ! species will go to this mode by default
                   SD( ISRM,2 ) = INDEX1( SD_NML( IRULE )%MODE_REF,
     &                               N_EM_AERO_REF, EM_AERO_REF_CAPS( : ) )
                   IF ( SD( ISRM,2 ) .EQ. 0 ) THEN
                      WRITE( XMSG,'(A,A,A,/,A,I2,A)' ), '*** Reference Aerosol Mode (', 
     &                       SD_NML( IRULE )%MODE_REF, 'Specified in Emissions Size ',
     &                       'Dist Rule ',IRULE,' does not exist in AERO_DATA.'
                      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                   END IF

               ELSE
                   ! Add a New Available Mode. For example, add a mode
                   ! just for BC, call it PUREBC, and make sure the AEC
                   ! for this stream is pointing to this mode. Also make
                   ! sure you set AEC for FINE mode aerosol to 0.0 if
                   ! you have default mapping turned on.
                   N_SD( ISRM ) = N_SD( ISRM ) + 1
                   SD_NAME( ISRM,N_SD( ISRM ) ) = SD_NML( IRULE )%MODE
                   SD( ISRM,N_SD( ISRM ) ) = INDEX1( SD_NML( IRULE )%MODE_REF,
     &                                 N_EM_AERO_REF, EM_AERO_REF_CAPS( : ) )
                   IF ( SD( ISRM,N_SD( ISRM )) .EQ. 0 ) THEN
                      WRITE( XMSG,'(A,A,A,/,A,I2,A)' ), '*** Reference Aerosol Mode (', 
     &                       SD_NML( IRULE )%MODE_REF, 'Specified in Emissions Size ',
     &                       'Dist Rule ',IRULE,' does not exist in AERO_DATA.'
                      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                   END IF

               END IF
           END IF
         END DO
      END DO
 
      ! Finally, transfer this data to a global variable which
      ! captures and organizes the modes of each stream
      ALLOCATE( EM_STREAM_SIZE( N_EM_SRM ) )
      DO ISRM = 1,N_EM_SRM
          N = N_SD( ISRM )
          EM_STREAM_SIZE( ISRM )%LEN = N + 1
          ALLOCATE( EM_STREAM_SIZE( ISRM )%NAME( N+1 ) )
          ALLOCATE( EM_STREAM_SIZE( ISRM )%REF( N+1 ) )
          ALLOCATE( EM_STREAM_SIZE( ISRM )%FACNUM( N+1,N_MODE ) )
          ALLOCATE( EM_STREAM_SIZE( ISRM )%FACSRF( N+1,N_MODE ) )

          EM_STREAM_SIZE( ISRM )%NAME( 2:N+1 ) = SD_NAME( ISRM,1:N )
          EM_STREAM_SIZE( ISRM )%REF( 2:N+1 )  = SD( ISRM,1:N )
          EM_STREAM_SIZE( ISRM )%NAME( 1 ) = 'GAS'
          EM_STREAM_SIZE( ISRM )%REF( 1 )  = 0

          ! Map Factors for Converting Aerosol Mass to Number and
          ! Surface Area to each Emission Stream
          EM_STREAM_SIZE( ISRM )%FACNUM( :,: ) = 0.0
          EM_STREAM_SIZE( ISRM )%FACSRF( :,: ) = 0.0
          DO ISD = 2,N+1
             IEM = EM_STREAM_SIZE( ISRM )%REF( ISD )
             DO IM = 1,N_MODE
                 EM_STREAM_SIZE( ISRM )%FACNUM( ISD,IM ) = FACNUM( IEM,IM ) 
                 EM_STREAM_SIZE( ISRM )%FACSRF( ISD,IM ) = FACSRF( IEM,IM )
             END DO
          END DO
      END DO

      END SUBROUTINE EM_SD_INIT    


C-----------------------------------------------------------------------

         SUBROUTINE EMISS_SIZE_DIST ( ISRM, VDEMIS, NL )

C  EMISS_SIZE_DIST distributes bulk aerosol emissions into size space
C  using parameters precompiled in the AERO_DATA module. 
C
C  Revision History:

C   16 AUG 17 BMURPHY: Created
C                      
C ----------------------------------------------------------------------

         USE AERO_DATA, ONLY: AEROSPC, N_AEROSPC, AEROSPC_MWINV 
         USE AEROMET_DATA, ONLY: F6DPI
         USE ASX_DATA_MOD, ONLY: MET_DATA
         USE EMIS_VARS, ONLY: N_EMIS_ISTR, IDUSTSRM, ISEASRM
         USE GRID_CONF, ONLY: NCOLS, NROWS
         USE SSEMIS, ONLY: SEA_FACTNUM, SEA_FACTSRF

         INTEGER, INTENT( IN ) :: ISRM, NL
         REAL, INTENT( INOUT ) :: VDEMIS ( :,:,:,: ) 

         INTEGER :: N, S, IAERO, IM, ISD, ISTR   ! Looping Variables
         INTEGER :: ROW, COL, LAY, N_SD, INUM, ISRF
         REAL    :: FACNUM, FACSRF, MW_FAC
         REAL, ALLOCATABLE       :: EMISM3( :,:,:,:,: ) 
         REAL, ALLOCATABLE, SAVE :: GSFAC( :,:,: )
         REAL, ALLOCATABLE, SAVE :: DENS_FAC( : )
         REAL, PARAMETER   :: F6DPIM9 = 1.0E-9 * F6DPI  ! 1.0E-9 = Kg/ug
         LOGICAL, SAVE     :: FIRST_TIME = .TRUE.

C *** Initialize Variables  
         IF ( FIRST_TIME ) THEN
             FIRST_TIME = .FALSE.
             ALLOCATE( GSFAC ( EMLAYS,NCOLS,NROWS ) )

             ALLOCATE( DENS_FAC( N_AEROSPC ) )
             DO IAERO = 1,N_AEROSPC
                 DENS_FAC( IAERO ) = F6DPIM9 / AEROSPC( IAERO )%DENSITY
             END DO

         END IF

         N_SD   = EM_STREAM_SIZE( ISRM )%LEN 
         ALLOCATE( EMISM3( EMLAYS,NCOLS,NROWS,N_MODE,N_SD ) )
         EMISM3 = 0.0
         !NL = EMLAYS

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
         DO LAY = 1,NL
           GSFAC( LAY,:,: ) = Met_Data%RJACM( :,:,LAY ) / CELLVOL( LAY ) ![ug/s] to [ug/m3/s]
         END DO
 
C *** Apply Aerosol Size Distribution
         DO ISTR = 1, N_EMIS_ISTR
             ! Find which Size Distribution or Phase this emissions species belongs 
             ! to for this stream. If the value is a 0, then there are no emissions 
             ! for this species from this stream. If it is a 1, then this species is
             ! a gas and the following aerosol conversions should be skipped.
             ISD   = MAP_EMtoSD( ISTR,ISRM )
             IF ( ISD .LE. 1 ) CYCLE
             
             ! Look up Aerosol Species and Mode of Interest
             IAERO = MAP_EMtoAERO( ISTR )   !This maps to the CMAQ aerosol
                                            !  species so we can retrieve density
             IM    = MAP_EMtoMODE( ISTR )   !This maps to the internal CMAQ modes 
                                            !  (ie. I, J, and K)
             !DENS_FAC = F6DPIM9 / AEROSPC( IAERO )%DENSITY 

             ! Convert Aerosol from [g/s] to [ug/m3/s] for all streams
             ! except Dust and Sea Spray. For those streams, convert
             ! [g/m3/s] to [ug/m3/s]
             VDEMIS( ISTR,1:NL,:,: ) = VDEMIS( ISTR,1:NL,:,: )  * 1.0E6
             IF ( ISRM .NE. ISEASRM .AND. ISRM .NE. IDUSTSRM ) THEN
                VDEMIS( ISTR,1:NL,:,: ) = VDEMIS( ISTR,1:NL,:,: ) * GSFAC( 1:NL,:,: ) 
             END IF

             ! Sum Total Volume of Mode N [m3/m3/s]
             IF ( .NOT. AEROSPC( IAERO )%TRACER ) 
     &             EMISM3( 1:NL,:,:,IM,ISD ) = EMISM3( 1:NL,:,:,IM,ISD ) + 
     &                  VDEMIS( ISTR,1:NL,:,: ) * DENS_FAC( IAERO )

             ! Convert Mass Emission Rates from [ug/m3/s] to [umol/m3/s]
             VDEMIS( ISTR,1:NL,:,: ) = VDEMIS( ISTR,1:NL,:,: ) * AEROSPC_MWINV( IAERO )
         END DO          

C *** Calculate the number emissions rate for each mode [1/m3/s], using 
C     Equation 7b of Binkowski & Roselle (2003).
C     Calculate the surface area emissions rate for the fine modes [m2/m3/s],
C     using Equation 7c of Binkowski & Roselle (2003).  Multiplying by PI 
C     converts 2nd moment to surface area.
            
         DO ISD = 2, N_SD   ! Skip the Index for the Gas Phase
            !IF ( EM_AERO_REF( EM_STREAM_SIZE( ISRM )%REF( ISD ) )%NAME .EQ. 'SEASPRAY' ) THEN
            IF ( ISRM .EQ. ISEASRM ) THEN
               ! Apply Spatially-Dependent Number and Surface Area Scale Factors
               DO IM = 1, N_MODE
                  VDEMIS( MAP_NUMtoEM(IM),1,:,: ) = VDEMIS( MAP_NUMtoEM(IM),1,:,: ) 
     &                     + EMISM3( 1,:,:,IM,ISD ) * SEA_FACTNUM( IM,:,: ) 
                  VDEMIS( MAP_SRFtoEM(IM),1,:,: ) = VDEMIS( MAP_SRFtoEM(IM),1,:,: ) 
     &                     + EMISM3( 1,:,:,IM,ISD ) * SEA_FACTSRF( IM,:,: ) 
               END DO
            ELSE
               ! Apply Homogeneous Scale Factors Consistent with this Stream
               DO IM = 1, N_MODE
                  INUM = MAP_NUMtoEM(IM)
                  FACNUM = EM_STREAM_SIZE( ISRM )%FACNUM( ISD,IM )
                  VDEMIS( INUM,1:NL,:,: ) = VDEMIS( INUM,1:NL,:,: ) + EMISM3( 1:NL,:,:,IM,ISD ) * FACNUM
                  ISRF = MAP_SRFtoEM(IM)
                  FACSRF = EM_STREAM_SIZE( ISRM )%FACSRF( ISD,IM )
                  VDEMIS( ISRF,1:NL,:,: ) = VDEMIS( ISRF,1:NL,:,: ) + EMISM3( 1:NL,:,:,IM,ISD ) * FACSRF
               END DO
            END IF
         END DO
 
         END SUBROUTINE EMISS_SIZE_DIST
 
      END MODULE AERO_EMIS

