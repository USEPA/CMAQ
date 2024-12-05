
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

      USE AERO_DATA, ONLY: DESID_N_AERO_REF, N_MODE
      USE DESID_VARS, ONLY: DESID_LAYS, DESID_STREAM_AERO, DESID_N_SRM, CELLVOL

      IMPLICIT NONE
      SAVE 
C aerosol emissions: [ppmv/s] for mass & number spcs, [m2/mol/s] for surface area spcs
      PUBLIC DESID_SIZE_DIST, AERO_EMIS_INIT, DESID_INIT_SIZE_DIST,
     &       MAP_ISTRtoAERO, MAP_ISTRtoMODE, MAP_NUMtoISTR, MAP_SRFtoISTR,
     &       MAP_ISTRtoNUM,  MAP_ISTRtoSRF,  MAP_ISTRtoSD,  DESID_STREAM_AERO, 
     &       SD_SPLIT
      PRIVATE

C Variables for converting mass emissions rate to number emissions rate
      REAL   :: FACNUM( DESID_N_AERO_REF,N_MODE )

C Variables for converting mass emissions rate to 2nd moment emissions rate
      REAL   :: FACSRF( DESID_N_AERO_REF,N_MODE )

C Variables for Saving split factors between emission modes
      REAL, ALLOCATABLE :: SD_SPLIT( :,: )

C Emission rate of all aerosol species interpolated to current time
      INTEGER, ALLOCATABLE :: MAP_ISTRtoAERO( : )   
      INTEGER, ALLOCATABLE :: MAP_ISTRtoMODE( : )   
      INTEGER, ALLOCATABLE :: MAP_NUMtoISTR ( : )  
      INTEGER, ALLOCATABLE :: MAP_SRFtoISTR ( : )   
      INTEGER, ALLOCATABLE :: MAP_ISTRtoNUM ( : )  
      INTEGER, ALLOCATABLE :: MAP_ISTRtoSRF ( : )   
      INTEGER, ALLOCATABLE :: MAP_ISTRtoSD  ( :,: ) 
      INTEGER, ALLOCATABLE :: MAP_AEROtoDIFF( :,: )     ! indices of aero species to CGRID

C Miscellaneous variables
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

         USE AERO_DATA, ONLY: DESID_AERO_REF, N_AEROSPC, AEROSPC, 
     &                        AERO_MISSING, MAP_AERO
         USE GRID_CONF, ONLY: GDTYP_GD, XCELL_GD, YCELL_GD, YORIG_GD, GL_NROWS, X3FACE_GD
         USE DUST_EMIS, ONLY: DUST_EMIS_INIT
         USE DESID_VARS, ONLY: MAP_ISTRtoEMVAR
         USE PRECURSOR_DATA, ONLY: MAP_PRECURSOR
         USE RUNTIME_VARS, ONLY:  OCEAN_CHEM, WB_DUST
         USE SSEMIS, ONLY:    SSEMIS_INIT
         USE UTILIO_DEFN
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
         DO IEM = 1,DESID_N_AERO_REF
           DO N = 1, N_MODE
              DGV = DESID_AERO_REF( IEM )%DGVEM( N )
              SG  = DESID_AERO_REF( IEM )%SGEM ( N )

              IF ( DESID_AERO_REF( IEM )%SPLIT( N ) .GT. 0.0 ) THEN
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
       ALLOCATE( SD_SPLIT( N_SPC_DIFF, DESID_N_AERO_REF ) )
       SD_SPLIT = 0.0
       DO IEM = 1,DESID_N_AERO_REF
         ! For the Fine Mode Reference Distribution, lump Aitken
         ! with Accumulation mode if Aitken Mode does not exist
         IF ( DESID_AERO_REF( IEM )%NAME .EQ. 'FINE_REF' ) THEN
            DO ISPC = 1,N_AEROSPC
              SPLIT_ACCUM = 0.0
              DO N = 1,N_MODE-1
                IF ( AERO_MISSING( ISPC,N ) ) THEN
                  SPLIT_ACCUM = SPLIT_ACCUM + DESID_AERO_REF( IEM )%SPLIT( N )
                ELSE
                  SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) = 
     &               SD_SPLIT( MAP_AEROtoDIFF( ISPC,N ),IEM ) +
     &               DESID_AERO_REF( IEM )%SPLIT( N ) + SPLIT_ACCUM
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
     &               DESID_AERO_REF( IEM )%SPLIT( N ) 
                END IF
              END DO
            END DO
         END IF
       END DO
                  
       ALLOCATE ( MAP_NUMtoISTR ( N_MODE ),
     &            MAP_SRFtoISTR ( N_MODE ), STAT = STATUS )
       CALL CHECKMEM( STATUS, 'MAP_NUMtoEM', PNAME )
       CALL CHECKMEM( STATUS, 'MAP_SRFtoEM', PNAME )
 
       END FUNCTION  AERO_EMIS_INIT

C-----------------------------------------------------------------------

       SUBROUTINE DESID_INIT_SIZE_DIST ( JDATE, JTIME )

C  EM_SD_INIT initializes the structures that map modes and streams to
C  reference modes including splits, diameters, and standard deviations.

C-----------------------------------------------------------------------
       USE AERO_DATA, ONLY: DESID_AERO_REF, DESID_N_AERO_REF
       USE DESID_VARS, ONLY: DESID_SD_NML
       USE DESID_UTIL, ONLY: DESID_GET_RULE_STREAMS
       USE UTILIO_DEFN, ONLY: INDEX1, XSTAT1, M3EXIT, UPCASE
         
       IMPLICIT NONE

       INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
       INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
       INTEGER ISRM
       
       INTEGER                          :: N_SD_RULE
       INTEGER                          :: N_SD( DESID_N_SRM )
       CHARACTER( 16 )                  :: SD_NAME( DESID_N_SRM, 10 )
       INTEGER                          :: SD( DESID_N_SRM, 10 )
       LOGICAL                          :: RULE_STREAM( DESID_N_SRM )
       CHARACTER( 16 )                  :: CSUR
       CHARACTER( 16 ), SAVE            :: PNAME = 'EM_SD_INIT  '
       CHARACTER( 20 )                  :: DESID_AERO_REF_CAPS( DESID_N_AERO_REF )

       INTEGER IRULE, ISUR, N, NLEN, ISD, IM, IEM, NRULE
       LOGICAL   :: LREMOVE, LERROR
       
       ! Find Total Number of Size Distribution Registries
       N_SD_RULE = 0
       DO IRULE = 1,SIZE( DESID_SD_NML )
          IF ( DESID_SD_NML( IRULE )%STREAM .EQ. '' ) EXIT
          N_SD_RULE = IRULE
       END DO

       ! First Load all of the Streams with the Default FINE, COARSE, and
       ! AERO Mode references
       SD = 0
       SD_NAME = ''

       ! Capitalize EM_AERO_REF(:)%NAME
       DO IM = 1,DESID_N_AERO_REF
          DESID_AERO_REF_CAPS( IM ) = DESID_AERO_REF( IM )%NAME
          CALL UPCASE( DESID_AERO_REF_CAPS( IM ) )
       ENDDO

       DO ISRM = 1,DESID_N_SRM
         N_SD( ISRM ) = 2
         SD_NAME( ISRM,1 ) = 'FINE'
         SD( ISRM,1 ) = INDEX1( 'FINE_REF',   DESID_N_AERO_REF, DESID_AERO_REF_CAPS( : ) )
         SD_NAME( ISRM,2 ) = 'COARSE'
         SD( ISRM,2 ) = INDEX1( 'COARSE_REF', DESID_N_AERO_REF, DESID_AERO_REF_CAPS( : ) )
       END DO
       
       ! Now Modify those defaults or add new modes to desired streams
       DO IRULE = 1, N_SD_RULE
         ! Expand Size Distribution Rule to All Streams if Requested
         LREMOVE = .FALSE.
         IF ( DESID_SD_NML( IRULE )%STREAM .EQ. '' ) CYCLE
         CALL DESID_GET_RULE_STREAMS( DESID_SD_NML( IRULE )%STREAM, IRULE, 
     &                 RULE_STREAM, LREMOVE, LERROR )
         IF ( LREMOVE ) CYCLE

         ! Loop through streams, set defaults, and build map array
         DO ISRM = 1, DESID_N_SRM
            IF ( RULE_STREAM( ISRM ) ) THEN
               ! This Stream is Being Modified by a Size Distribution
               ! rule
               CALL UPCASE( DESID_SD_NML( IRULE )%MODE_REF )
               IF ( DESID_SD_NML( IRULE )%MODE .EQ. 'FINE' ) THEN
                   ! Overwrite the FINE mode. All fine particle species
                   ! will go to this mode by default
                   SD( ISRM,1 ) = INDEX1( DESID_SD_NML( IRULE )%MODE_REF,
     &                               DESID_N_AERO_REF, DESID_AERO_REF_CAPS( : ) )
                   IF ( SD( ISRM,1 ) .EQ. 0 ) THEN
                      WRITE( XMSG,'(A,A,A,/,A,I2,A)' ), '*** Reference Aerosol Mode (', 
     &                       DESID_SD_NML( IRULE )%MODE_REF, 'Specified in Emissions Size ',
     &                       'Dist Rule ',IRULE,' does not exist in AERO_DATA.'
                      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                   END IF

               ELSEIF ( DESID_SD_NML( IRULE )%MODE .EQ. 'COARSE' ) THEN
                   ! Overwrite the COARSE mode. All coarse particle
                   ! species will go to this mode by default
                   SD( ISRM,2 ) = INDEX1( DESID_SD_NML( IRULE )%MODE_REF,
     &                               DESID_N_AERO_REF, DESID_AERO_REF_CAPS( : ) )
                   IF ( SD( ISRM,2 ) .EQ. 0 ) THEN
                      WRITE( XMSG,'(A,A,A,/,A,I2,A)' ), '*** Reference Aerosol Mode (', 
     &                       DESID_SD_NML( IRULE )%MODE_REF, 'Specified in Emissions Size ',
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
                   SD_NAME( ISRM,N_SD( ISRM ) ) = DESID_SD_NML( IRULE )%MODE
                   SD( ISRM,N_SD( ISRM ) ) = INDEX1( DESID_SD_NML( IRULE )%MODE_REF,
     &                                DESID_N_AERO_REF, DESID_AERO_REF_CAPS( : ) )
                   IF ( SD( ISRM,N_SD( ISRM )) .EQ. 0 ) THEN
                      WRITE( XMSG,'(A,A,A,/,A,I2,A)' ), '*** Reference Aerosol Mode (', 
     &                       DESID_SD_NML( IRULE )%MODE_REF, 'Specified in Emissions Size ',
     &                       'Dist Rule ',IRULE,' does not exist in AERO_DATA.'
                      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                   END IF

               END IF
           END IF
         END DO
      END DO
 
      ! Finally, transfer this data to a global variable which
      ! captures and organizes the modes of each stream
      ALLOCATE( DESID_STREAM_AERO( DESID_N_SRM ) )
      DO ISRM = 1,DESID_N_SRM
          N = N_SD( ISRM )
          DESID_STREAM_AERO( ISRM )%LEN = N + 1
          ALLOCATE( DESID_STREAM_AERO( ISRM )%NAME( N+1 ) )
          ALLOCATE( DESID_STREAM_AERO( ISRM )%REF( N+1 ) )
          ALLOCATE( DESID_STREAM_AERO( ISRM )%FACNUM( N+1,N_MODE ) )
          ALLOCATE( DESID_STREAM_AERO( ISRM )%FACSRF( N+1,N_MODE ) )

          DESID_STREAM_AERO( ISRM )%NAME( 2:N+1 ) = SD_NAME( ISRM,1:N )
          DESID_STREAM_AERO( ISRM )%REF( 2:N+1 )  = SD( ISRM,1:N )
          DESID_STREAM_AERO( ISRM )%NAME( 1 ) = 'GAS'
          DESID_STREAM_AERO( ISRM )%REF( 1 )  = 0

          ! Map Factors for Converting Aerosol Mass to Number and
          ! Surface Area to each Emission Stream
          DESID_STREAM_AERO( ISRM )%FACNUM( :,: ) = 0.0
          DESID_STREAM_AERO( ISRM )%FACSRF( :,: ) = 0.0
          DO ISD = 2,N+1
             IEM = DESID_STREAM_AERO( ISRM )%REF( ISD )
             DO IM = 1,N_MODE
                 DESID_STREAM_AERO( ISRM )%FACNUM( ISD,IM ) = FACNUM( IEM,IM ) 
                 DESID_STREAM_AERO( ISRM )%FACSRF( ISD,IM ) = FACSRF( IEM,IM )
             END DO
          END DO
      END DO

      END SUBROUTINE DESID_INIT_SIZE_DIST    


C-----------------------------------------------------------------------

         SUBROUTINE DESID_SIZE_DIST ( ISRM, VDEMIS, NL )

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
         USE DESID_VARS, ONLY: DESID_N_ISTR, IDUSTSRM, ISEASRM
         USE GRID_CONF, ONLY: NCOLS, NROWS
         USE SSEMIS, ONLY: SEA_FACTNUM, SEA_FACTSRF

         INTEGER, INTENT( IN ) :: ISRM, NL
         REAL, INTENT( INOUT ) :: VDEMIS ( :,:,:,: ) 

         INTEGER :: N, S, IAERO, IM, ISD, ISTR   ! Looping Variables
         INTEGER :: ROW, COL, LAY, N_SD, INUM, ISRF, MAX_N_SD
         REAL    :: FACNUM, FACSRF, MW_FAC
         REAL, ALLOCATABLE, SAVE :: EMISM3( :,:,:,:,: ) 
         REAL, ALLOCATABLE, SAVE :: GSFAC( :,:,: )
         REAL, ALLOCATABLE, SAVE :: DENS_FAC( : )
         REAL, PARAMETER   :: F6DPIM9 = 1.0E-9 * F6DPI  ! 1.0E-9 = Kg/ug
         LOGICAL, SAVE     :: FIRST_TIME = .TRUE.

C *** Initialize Variables  
         
         IF ( FIRST_TIME ) THEN
             FIRST_TIME = .FALSE.
             ALLOCATE( GSFAC ( DESID_LAYS,NCOLS,NROWS ) )

             ALLOCATE( DENS_FAC( N_AEROSPC ) )
             DO IAERO = 1,N_AEROSPC
                 DENS_FAC( IAERO ) = F6DPIM9 / AEROSPC( IAERO )%DENSITY
             END DO
             
             MAX_N_SD = MAXVAL( DESID_STREAM_AERO(:)%LEN )

             ALLOCATE( EMISM3( DESID_LAYS,NCOLS,NROWS,N_MODE,MAX_N_SD ) )

         END IF
         N_SD   = DESID_STREAM_AERO( ISRM )%LEN 
         EMISM3 = 0.0

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
         DO LAY = 1,NL
           GSFAC( LAY,:,: ) = Met_Data%RJACM( :,:,LAY ) / CELLVOL( :,:,LAY ) ![ug/s] to [ug/m3/s]
         END DO
 
C *** Apply Aerosol Size Distribution
         DO ISTR = 1, DESID_N_ISTR
             ! Find which Size Distribution or Phase this emissions species belongs 
             ! to for this stream. If the value is a 0, then there are no emissions 
             ! for this species from this stream. If it is a 1, then this species is
             ! a gas and the following aerosol conversions should be skipped.
             ISD   = MAP_ISTRtoSD( ISTR,ISRM )
             IF ( ISD .LE. 1 ) CYCLE
             
             ! Look up Aerosol Species and Mode of Interest
             IAERO = MAP_ISTRtoAERO( ISTR )   !This maps to the CMAQ aerosol
                                              !  species so we can retrieve density
             IM    = MAP_ISTRtoMODE( ISTR )   !This maps to the internal CMAQ modes 
                                              !  (ie. I, J, and K)

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
            IF ( ISRM .EQ. ISEASRM ) THEN
               ! Apply Spatially-Dependent Number and Surface Area Scale Factors
               DO IM = 1, N_MODE
                  INUM = MAP_NUMtoISTR(IM)
                  VDEMIS( INUM,1,:,: ) = VDEMIS( INUM,1,:,: ) 
     &                     + EMISM3( 1,:,:,IM,ISD ) * SEA_FACTNUM( IM,:,: ) 

                  ISRF = MAP_SRFtoISTR(IM)
                  VDEMIS( ISRF,1,:,: ) = VDEMIS( ISRF,1,:,: ) 
     &                     + EMISM3( 1,:,:,IM,ISD ) * SEA_FACTSRF( IM,:,: ) 
               END DO
            ELSE
               ! Apply Homogeneous Scale Factors Consistent with this Stream
               DO IM = 1, N_MODE
                  INUM = MAP_NUMtoISTR(IM)
                  FACNUM = DESID_STREAM_AERO( ISRM )%FACNUM( ISD,IM )
                  VDEMIS( INUM,1:NL,:,: ) = VDEMIS( INUM,1:NL,:,: ) + EMISM3( 1:NL,:,:,IM,ISD ) * FACNUM

                  ISRF = MAP_SRFtoISTR(IM)
                  FACSRF = DESID_STREAM_AERO( ISRM )%FACSRF( ISD,IM )
                  VDEMIS( ISRF,1:NL,:,: ) = VDEMIS( ISRF,1:NL,:,: ) + EMISM3( 1:NL,:,:,IM,ISD ) * FACSRF
               END DO
            END IF
         END DO
 
         END SUBROUTINE DESID_SIZE_DIST
 
      END MODULE AERO_EMIS

