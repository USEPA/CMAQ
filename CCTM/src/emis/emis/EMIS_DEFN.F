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
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/EMIS_DEFN.F,v 1.10 2011/10/21 16:10:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE EMIS_DEFN

C-----------------------------------------------------------------------
C Function: emissions interface to the chemistry-transport model

C Revision History:
C     28 Jul 2006 J.Young: initial implementation
C     18 Aug 2007 J.Young: move beis part to separate module; add plume rise
C     23 Sep 2009 B.Hutzell: modified algorithm that loads gas emissions from point
C                 sources into VDEMIS array to enable multi-use of an emission species
C     26 Jan 2010 J.Young: fix bug overwriting point source layer 1 NH3; inline rdemis
C     07 Jan 2011 B.Hutzell: updated for namelist definition of model species
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                            removed deprecated TRIMLEN
C      6 Apr 2011 J.Young, R.Pinder: add lightning NO emissions capability
C     11 May 2011 D.Wong: incorporated twoway model implementation
C      5 Jun 2012 J.Bash: Added support for NH3 bidirectional exchange. Fertilizer
C                         sector emissions are subtracted from the total NH3 emissions
C                         if the CTM_ABFLUX flag is set
C     07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C     24 Feb 16 B.Murphy: Generalize scaling of point source species based on
C                         mapping model species, not point source species
C     03 Mar 16 B.Gantt/G. Sarwar: incorporated halogen emissions
C     08 Aug 2016 B.Murphy: Neglect fire emissions for pcVOC
C     12 Jan 2017 B.Murphy: Remove warning when model species are not
C                           read in correctly. Invoke error and model stop when model 
C                           species are not found on any emission file
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE EMIS_GLOBAL

      IMPLICIT NONE

      PUBLIC EMIS_INIT, GET_EMIS, EM_GRID_LAYS
      
      PRIVATE
      
      LOGICAL,              SAVE :: EMISCHK ! Let model proceed even if Emis Surrogates 
                                            ! are not properly defined
      
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_TMP( :,:,:,: ) ! total emissions array
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_GR ( :,:,:,: ) ! total emissions array
      INTEGER, ALLOCATABLE, SAVE :: EM_GRID_LAYS( : ) ! no. of area emission layers
      
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_SURR( :,: ) ! Emissions Surrogate to be read ( species, source )
      
      LOGICAL, ALLOCATABLE, SAVE :: EM_MASK_AERO( : )! Store the location of aerosol species in master 
                                                     ! emissions vector
                                     
      LOGICAL, SAVE :: EM_TRAC    ! do tracer emissions?
      REAL,    SAVE :: CNVTE      ! combined conversion factor
      REAL,    SAVE :: DT         ! TSTEP (output) in sec

      REAL,    ALLOCATABLE, SAVE :: BUFF( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: BUFF2( :,:,: )       ! read buffer
      REAL,    ALLOCATABLE, SAVE :: FERT( :,:,: )        ! read buffer

      INTEGER, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                 STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, SAVE :: NDATE, NTIME  ! step next date/time (beis)

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE BEIS_DEFN           ! biogenic emissions
         USE MGEMIS              ! marine gas emissions
         USE LTNG_DEFN           ! NO emissions from lightning strikes
         USE PT3D_DEFN           ! plume rise emissions
         USE UTILIO_DEFN         ! I/O API
         USE AERO_EMIS           ! inherits GRID_CONF
         USE AERO_DATA, ONLY:   ! access subroutine map_pmemis
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP   ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

C Parameters:
         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio
                                        
C Local Variables:
         REAL    CONVEM          ! conversion for emissions rates to Kg/s
         REAL    DX1, DX2        ! CX x1- and x2-cell widths

         CHARACTER( 16 ), SAVE :: CTM_EMLAYS = 'CTM_EMLAYS'    ! env var for no. of
                                                               ! emiss. layers
         
         CHARACTER( 16 ) :: PNAME = 'EMIS_INIT'
         CHARACTER( 80 ) :: VARDESC   ! env variable description
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER V, LOGDEV, STATUS, ISRC

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = INIT3()

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
         ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
         END IF

C Retrieve Number of Emission Files of Various Types (sectors)
         CALL EM_FILE_INIT( JDATE, JTIME )

C Initialize Online Biogenic Emissions
         IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Online Marine Gas Emissions
         IF ( .NOT. MGEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing marine gas emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Open Area Emissions files
         CALL OPEMIS ( JDATE, JTIME, EM_TRAC, CONVEM )
         CNVTE = CMLMR * CONVEM * MWAIR / ( DX1 * DX2 ) !Conv. Factor for Gases

C Get number of emissions layers
         VARDESC = 'Number of emission layers'
         EMLAYS = ENVINT( CTM_EMLAYS, VARDESC, EMLAYS, STATUS )
         IF ( STATUS .NE. 0 ) THEN
            WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE
               XMSG = 'Environment variable not set or empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF
         END IF 
         
         EMLAYS = MIN ( EMLAYS, NLAYS )  !Emission Layers will be at most the 
                                         !layers in the grid

         ! Cut the number of emission layers of any 3D gridded files to
         ! equal EMLAYS
         DO ISRC = 1, SIZE( EM_GRID_LAYS ) 
             IF ( EM_GRID_LAYS( ISRC ) .GT. 0 ) 
     &            EM_GRID_LAYS( ISRC ) = MIN( EM_GRID_LAYS( ISRC ), EMLAYS )
         END DO

         WRITE( LOGDEV,1009 ) EMLAYS, NLAYS
1009     FORMAT( / 10X, 'Number of Emissions Layers:         ', I3
     &           / 10X, 'out of total Number of Model Layers:', I3 )

C Initialize Online Lightning NOx Emissions
         IF ( .NOT. LTNG_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing lightning emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize 3D Point Source Emissions 
         IF ( .NOT. PT3D_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Aerosol Emissions         
         IF ( .NOT. AERO_EMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing aerosol emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
 
                   
C Map the Emissions Species Available on the Input Files To the
C Surrogates Identified by the User via the Namelists and Stop the model
C or Print Warnings if Mistakes Are Made.
         CALL EMIS_SPC_MAP( JDATE, JTIME )

C Allocate Space for Master Emissions Computation
         ALLOCATE ( VDEMIS( N_EMIS_SPC+1,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_TMP( N_EMIS_SPC+1,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_TMP memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_GR( N_EMIS_SPC+1,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_GR memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
 
C Allocate Buffer space for Reading Emissions
         ALLOCATE ( BUFF( NCOLS,NROWS,EMLAYS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'BUFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         BUFF = 0.0   

         ALLOCATE ( BUFF2( NCOLS,NROWS,EMLAYS ), STAT = STATUS )
          IF ( STATUS .NE. 0 ) THEN
            XMSG = 'BUFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF 
         BUFF2= 0.0   

C Return From Initialization
         SUCCESS = .TRUE.; RETURN

         END FUNCTION EMIS_INIT

C-----------------------------------------------------------------------
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA, CGRID )

C NEW APPROACH:
C Apply NML factors to all *input* emissions and inline plume rise, aero emis
C biog, SeaSalt, Dust, and Lightning NO
C GET_AERO_EMIS has to apply factors - don`t do it at this level.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE MGEMIS              ! marine gas emissions
         USE PT3D_DEFN           ! plume rise emissions
         USE LTNG_DEFN           ! lightning NO emissions
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME
         USE PRECURSOR_DATA, ONLY : SULF_IDX, PRECURSOR_MW
         USE AERO_DATA, ONLY : AERONUM_MAP, AEROSRF_MAP

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL,    INTENT( IN ) :: CONVPA        ! conversion factor to pressure in Pascals
         REAL, POINTER :: CGRID( :,:,:,: )

C Local Variables:
         REAL             CNVTC         ! combined conversion factor
         REAL             CNVTI         ! intermediate combined conv. factor
         REAL,    SAVE :: CNVTP         ! intermediate combined conv. factor
         REAL             CONVM         ! Aerosol Mass conversion factor
         REAL             CONVN         ! Aerosol Number conversion factor
         REAL             CONVS         ! Aerosol Surface Area conversion factor
         
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V, ISPC, ISRC ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables
         
         REAL, PARAMETER :: GPKG = 1.0E+03     ! g/kg
         REAL, PARAMETER :: MWAIR = 28.9628 
         REAL, PARAMETER :: AVO  = 6.0221367E23
         REAL, PARAMETER :: RAVO = 1.0 / AVO

         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
C Get domain decomp info from the emissions file
            CALL SUBHFILE ( EM_FILE_NAME( 1 ), GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
            CNVTP = CONVPA * CNVTE

         END IF    !FirstTime

C Initialize Emissions Array
         VDEMIS = 0.0


C Retrieve Emissions from All Source Types
         DO ISRC = 1,N_EM_SRC

           SELECT CASE ( EM_FILE_TYPE( ISRC ) )
           
             ! Retrieve Gridded Emissions
             CASE ( "GRID" )
               CALL GRIDEMIS ( JDATE, JTIME, EM_FILE_NAME( ISRC ), 
     &                         EM_GRID_LAYS( ISRC ), ISRC )
               VDEMIS = VDEMIS + VDEMIS_GR

             ! Retrieve Point Source Emissions
             CASE ( "POINT" )
               CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, EM_SURR( :,ISRC ), ISRC )
               VDEMIS = VDEMIS + VDEMIS_PT

             ! Retrieve Biogenic Emissions from BEIS
             CASE ( "BIOG" )
               CALL GET_BEIS ( JDATE, JTIME, TSTEP )
               DO ISPC = 1, MSPCS
                  V = BEIS_MAP( ISPC )
                  IF ( V .GT. 0 ) VDEMIS( V,1,:,: ) = VDEMIS( V,1,:,: )
     &                          + EM_FAC( V,ISRC ) * VDEMIS_BI( ISPC,:,: )
               END DO

             ! Retrieve Marine Gas Emissions
             CASE ( "MGEM" )
               CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )
               DO ISPC = 1, NMGSPC
                  V = MGSPC_MAP( ISPC )
                  IF ( V .GT. 0 ) VDEMIS( V,1,:,: ) = VDEMIS( V,1,:,: )
     &                          + EM_FAC( V,ISRC ) * VDEMIS_MG( ISPC,:,: )
               END DO

             ! Retrieve Lightning NO Emissions
             CASE ( "LTNG" )
               CALL GET_LTNG ( JDATE, JTIME, TSTEP )
               DO ISPC = 1,N_EMIS_SPC
                  IF ( EM_FAC( ISPC,ISRC ) .NE. 0.0 )
     &               VDEMIS( ISPC,:,:,: ) = VDEMIS( ISPC,:,:,: )
     &                 +  EM_FAC( ISPC,ISRC ) * VDEMIS_LT( :,:,: )
               END DO
           END SELECT

         END DO  

C *** Distribute Aerosol Emissions in Size Space
         CALL EMISS_SIZE_DIST( VDEMIS )

C Aerosol Emissions - Add Sea Spray and Dust
         IF ( N_AE_SPC .GT. 0 ) THEN
            CALL GET_AERO_EMIS ( JDATE, JTIME, TSTEP )
            VDEMIS = VDEMIS + VDEMIS_AE
         END IF          

C Convert All Emissions to Units Appropriate for the Dispersion Solver
         DO L = 1, EMLAYS
            CNVTI = CNVTP * Grid_Data%RDX3F( L ) 
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  CNVTC = CNVTI * Met_Data%RRHOJ( C,R,L )  ! Gas mol/s to ppmv/s
                  CONVM = MWAIR / GPKG / Met_Data%DENS( C,R,L ) ! Aer. Mass [m3/mol]
                  CONVN = CONVM * RAVO * 1.0E+06           ! Aer. Num 10^6 * [m3]
                  CONVS = CONVM                            ! Aer. Srf [m3/mol]
                  
                  !MAKE SURE TO APPLY RELEVANT UNIT CONVERSION TO EACH
                  !TYPE OF VARIABLE (GAS, AEROSOL MASS, AEROSOL NUMBER,
                  !AND AEROSOL SURFACE AREA)
                  WHERE( MAP_EMtoNUM .EQ. 0 .AND. MAP_EMtoSRF .EQ. 0 .AND.
     &                   MAP_EMtoAERO .EQ. 0 )
     &                   VDEMIS( :,L,C,R ) = CNVTC * VDEMIS( :,L,C,R ) 
                  WHERE( MAP_EMtoAERO .NE. 0 )
     &                   VDEMIS( :,L,C,R ) = CONVM * VDEMIS( :,L,C,R ) 
                  WHERE( MAP_EMtoNUM .NE. 0 )
     &                   VDEMIS( :,L,C,R ) = CONVN * VDEMIS( :,L,C,R ) 
                  WHERE( MAP_EMtoSRF .NE. 0 )
     &                   VDEMIS( :,L,C,R ) = CONVS * VDEMIS( :,L,C,R ) 

               END DO
            END DO
         END DO
 
C zero out emissions values for species not included in diffused list
C ...accounts for emissions species names as a subset of the vert. diffused species list

         VDEMIS( N_EMIS_SPC+1,:,:,: ) = 0.0

         RETURN

         END SUBROUTINE GET_EMIS

C-----------------------------------------------------------------------
         SUBROUTINE GRIDEMIS ( JDATE, JTIME, EMIS_FNAME, LAYS, 
     &                         FSRC )

C NEW APPROACH:
C Apply NML factors to all *input* emissions and inline plume rise, aero emis
C biog, SeaSalt, Dust, and Lightning NO
C GET_AERO_EMIS has to apply factors - don`t do it at this level.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME
         USE PRECURSOR_DATA, ONLY : SULF_IDX, PRECURSOR_MW

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: FSRC
         CHARACTER( 100 ), INTENT( IN ) :: EMIS_FNAME

C Local Variables:
         INTEGER          C, R, L, N, S, V, ISPC ! loop induction variables
         INTEGER          S_STRT, S_END, LAYS

         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GRIDEMIS'
         CHARACTER( 120 ) :: XMSG = ' '
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
C Get domain decomp info from the emissions file
            CALL SUBHFILE ( EMIS_FNAME, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
         END IF    !FirstTime
 
C Read & Interpolate Emissions 

          DO ISPC = 1, N_EMIS_SPC 
            VNAME = EM_SURR( ISPC, FSRC )
            IF ( VNAME .EQ. '' ) CYCLE

            BUFF = 0.0   ! array assignment
C                                       vvvvvvv - emis surrogate names
            IF ( .NOT. INTERPX( EMIS_FNAME, VNAME, PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW,1,LAYS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read '
     &              // TRIM( VNAME ) // ' from ' // EMIS_FNAME
               IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF

            ! READ Non-Carbon Organic Matter From Gas Emissions
            IF ( ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 )
     &           .AND. VNAME .EQ. 'POC' ) THEN
                 BUFF2 = 0.0
                 IF ( .NOT. INTERPX( EMIS_FNAME, 'PNCOM', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW,1,LAYS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                 XMSG = 'Could not read '
     &               // TRIM( VNAME ) // ' from ' // EMIS_FNAME
                 IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              END IF  
              BUFF = BUFF + BUFF2
            ENDIF
 
            ! READ Sulfuric Acid From Gas Emissions and Assign to Aerosols
            IF ( VNAME .EQ. 'PSO4' ) THEN
                 BUFF2 = 0.0
                 IF ( .NOT. INTERPX( EMIS_FNAME, 'SULF', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW,1,LAYS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                 XMSG = 'Could not read '
     &               // TRIM( VNAME ) // ' from ' // EMIS_FNAME
                 IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              END IF  
              BUFF = BUFF + BUFF2 * REAL( PRECURSOR_MW( SULF_IDX ), 4 )
            ENDIF
 
            ! Zero Out Sulfuric Acid Emissions Since they have been
            ! added to particulate sulfate
            IF ( VNAME .EQ. 'SULF' ) THEN
              BUFF = 0.0
            END IF

            ! Subtract Fertilizer Emissions from NH3 source if running
            ! BIDIrectional Exchange Module
            IF ( ABFLUX .AND. VNAME .EQ. 'NH3' ) THEN
               BUFF2 = 0.0   ! array assignment
               IF ( .NOT. INTERPX( EMIS_FNAME, 'NH3_FERT', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW,1,LAYS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                  XMSG = 'Could not read NH3_FERT from' // EMIS_FNAME
                  IF ( .NOT. EMISCHK ) 
     &                 CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               END IF
               BUFF( :,:,1 ) = BUFF( :,:,1 ) - BUFF2( :,:,1 )
               IF ( ANY( BUFF .LT. 0.0 ) ) THEN
                    XMSG = 'Negative values in NH3 emissions' 
     &                       //'Error in NH3 Bidi emissions'                          
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF ! ABFLUX & Ammonia
 
C Store all emissions in mol/sec or g/sec and convert to ppmv/s later
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, LAYS
                     VDEMIS_GR( ISPC,L,C,R ) = EM_FAC( ISPC,FSRC ) * BUFF( C,R,L )
                  END DO
               END DO
            END DO

           END DO   ! ISPC

           RETURN

      END SUBROUTINE GRIDEMIS

C-----------------------------------------------------------------------
         SUBROUTINE EMIS_SPC_MAP( JDATE, JTIME )

C Check the chemical species from the namelists and AERO_DATA against
C the species that are available on the actual emissions input files. If
C they do not agree, print warngings or crash the program depending on
C how severe the error is.
C
C    16 Mar 2017  B.Murphy     Created Subroutine
C    10 Sep 2017  B.Murphy     Revised Emissions Mapping Approach
C    08 Nov 2017  B.Murphy     Vectorized Emission Maps to allow for 
C                                unlimited emissions sources
C-----------------------------------------------------------------------

         USE BEIS_DEFN, only : BIOGEMIS, ! Online Biogenic Emissions Env Variable
     &                         BEIS_MAP  ! Map from BEIS Species to EM_SPEC
         USE BIOG_EMIS, only : EMSPC,    ! Emitted CMAQ-Species from BEIS
     &                         MSPCS     ! Number of Emitted Species
         USE MGEMIS,    only : MGEM,     ! Flag for Marine Gas Emissions
     &                         MG_SPC,   ! Emitted Marine Gas CMAQ-Species
     &                         MGSPC_MAP ! Emitted Marine Gas CMAQ-Species
         USE LTNG_DEFN, only : LTNG_NO,  ! Flag for Lightning Emissions
     &                         LTSPC     ! Lightning Emitted Species (i.e. 'NO')
         USE PT3D_DEFN, only : PT3DEMIS, ! Flag for Point Source Emissions
     &                         NPTGRPS
         USE STK_EMIS,  only : STKSPC    ! Species from Point Source Emissions
         USE UTILIO_DEFN
         USE CGRID_SPCS, only: N_GC_EMIS, N_AE_EMIS, N_NR_EMIS, N_TR_EMIS,
     &                         GC_EMIS,   AE_EMIS,   NR_EMIS,   TR_EMIS,
     &                         GC_SPC,    AE_SPC,    NR_SPC,    TR_SPC,
     &                         GC_EMIS_MAP, AE_EMIS_MAP, NR_EMIS_MAP, TR_EMIS_MAP,
     &                         GC_EMIS_FAC, AE_EMIS_FAC, NR_EMIS_FAC, TR_EMIS_FAC,
     &                         GC_MOLWT, N_GC_SPC
         USE AERO_DATA, only : N_MODE, AEROSPC,  ! Aerosol Properties Table
     &                         N_AEROSPC, AEROMODE, POA_NAME, POA_OP_VF, POA_AMF,
     &                         EMWT_PROF, N_VBS_BIN
         USE AERO_EMIS, only : MAP_NUMtoEM, MAP_SRFtoEM, MAP_EMtoAERO, MAP_EMtoMODE, 
     &                         MAP_AEROtoEM,MAP_EMtoNUM, MAP_EMtoSRF, WB_DUST

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME

         INTEGER    :: NTRSPC, NARSPC, NBIOSPC, 
     &                 NPTSPC, NMGSPC, NLTSPC, NDSTSPC
         INTEGER    :: NEMIS_SPC, NEMIS_SPC_UNIQUE, N_UNUSED, N_EMBUFF

         CHARACTER( 16 ), ALLOCATABLE  :: EMIS_SPCS( : ), EMIS_SPC_UNIQUE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: GC_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: AE_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: NR_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: TR_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: AERO_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: BG_SPC_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: EMIS_SPC_UNUSED( : )
         CHARACTER( 16 ), ALLOCATABLE  :: EM_SURR_BUFF( : )
         REAL(8),         ALLOCATABLE  :: EM_FAC_BUFF( : )

         INTEGER    :: STRT, PTSTRT, ISPC, IPT, JPT, IDX, LOGDEV, IX,
     &                 IGC, IAE, INR, ITR, N, IM, IAERO, IPOA, V, ISRC,NSPC

         LOGICAL    :: LERROR
         LOGICAL, ALLOCATABLE :: L_EMIS_SPC( : ), 
     &                           GC_EMIS_MISSING( : ), AE_EMIS_MISSING( : ),
     &                           NR_EMIS_MISSING( : ), TR_EMIS_MISSING( : ),
     &                           AERO_EMIS_MISSING( : ), TMP( : ),
     &                           EMIS_MASK( : ), BIO_IND( : )

         INTEGER, ALLOCATABLE  :: EMIS_IND( : )

         CHARACTER( 16 )  :: CTM_EMISCHK = 'CTM_EMISCHK'    ! Let model proceed even if
                                                                 ! Emissions surrogates are
                                                                 ! not properly defined
         CHARACTER( 16 )  :: PNAME = 'EMIS_SPC_CHECK'
         CHARACTER( 80 )  :: VARDESC
         INTEGER          :: STATUS
         CHARACTER( 200 ) :: XMSG
 
 
C Retrieve Environment Variable Letting User Ignore this Check
C and allowing the model to proceed.
         EMISCHK = .TRUE.  ! Default True for Emissions Check
         VARDESC    = "Let model proceed even if there are emission"
     &              //" surrogates that do not appear on any input file."
         LOGDEV = INIT3()
         EMISCHK = ENVYN( CTM_EMISCHK, VARDESC, EMISCHK, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty...Using default:'
            WRITE( LOGDEV, '(5X,A,I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set...Using default:'
            WRITE( LOGDEV, '(5X,A,I9)' ) XMSG, JTIME
         END IF

C Find Numbers of Species Available From Emissions Sources
         NEMIS_SPC  = SUM( EM_FILE_SPEC( : )%LEN )
         ALLOCATE( EMIS_SPCS( NEMIS_SPC ) )
         EMIS_SPCS( 1:NEMIS_SPC ) = ""

         ! Build the master list of all area, point, bio, sea spray
         ! Print a Table of Species from Every Emissions Source
         WRITE( LOGDEV, '(/,A31,A42)' ),'CHECKING COMPATABILITY BETWEEN ',
     &                  'INTERNAL SPECIES LIST AND EMISSIONS INPUTS' 
         WRITE( LOGDEV, '(A55,/,A58)' ),
     &                  'The following tables list the chemical species present ',
     &                  'on each emission file or from each online emission source.' 

         ! Print Out the Species From Each Emissions Source
         STRT = 0
         DO ISRC = 1,N_EM_SRC
            NSPC = EM_FILE_SPEC( ISRC )%LEN
            EMIS_SPCS( STRT+1:STRT+NSPC ) = EM_FILE_SPEC( ISRC )%ARRY(1:NSPC)

            WRITE( LOGDEV, '(/,A,I3,A8,/,4(2x,A16))' ),
     &           TRIM( EM_FILE_DESC( ISRC ) ),NSPC,' Species',
     &           EMIS_SPCS( STRT+1:STRT+NSPC )
            STRT = STRT + NSPC
         END DO

C Reduce EMIS_SPCS to just unique values
         ALLOCATE( EMIS_MASK( NEMIS_SPC ) )
         EMIS_MASK = .TRUE.
         ! Determine which surrogates to keep
         DO ISPC = NEMIS_SPC,2,-1
             EMIS_MASK( ISPC ) = .NOT.( ANY( EMIS_SPCS(:ISPC-1) == EMIS_SPCS( ISPC ) ) )
         END DO

         ! Define indices of Surrogates to Keep
         ALLOCATE( EMIS_IND( COUNT( EMIS_MASK ) ) )   
         EMIS_IND = PACK([(ix, ix=1,NEMIS_SPC ) ],EMIS_MASK )
         
         ! Reduce the EMIS_SPCS vector down to just the unique values
         NEMIS_SPC_UNIQUE = SIZE( EMIS_IND,DIM=1 )
         ALLOCATE( EMIS_SPC_UNIQUE( NEMIS_SPC_UNIQUE ) )
         EMIS_SPC_UNIQUE = EMIS_SPCS( EMIS_IND )
         
C Print out the Tables of CMAQ Emissions Surrogate Species for reference
         WRITE( LOGDEV, '(/A57,/,A54)' ),
     &                  'The following tables list the emission surrogate species ',
     &                  'present in each namelist.' 
         
         IF ( N_GC_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'GC Namelist: ',N_GC_EMIS,' Species',GC_EMIS(1:N_GC_EMIS)
         
         IF ( N_AEROSPC .GT. 0 ) 
     &        WRITE( LOGDEV, '(/,A16,I3,A8,/,4(2x,A16))' ),
     &               'AE Namelist: ',N_AE_EMIS,' Species',AE_EMIS(1:N_AE_EMIS)
         
         IF ( N_NR_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'NR Namelist: ',N_NR_EMIS,' Species',NR_EMIS(1:N_NR_EMIS)
         
         IF ( N_TR_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'TR Namelist: ',N_TR_EMIS,' Species',TR_EMIS(1:N_TR_EMIS)


C Warn the User if all of the files have no species present         
         IF ( N_EMIS_SPC .LE. 0 ) THEN
            XMSG = 'There are no emissions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

C Populate Useful Masks
         ! Populate EM_SPEC Array with CMAQ Internal Species that will have
         ! Emissions
         ALLOCATE( EM_SPEC( N_EMIS_SPC ) )
         ALLOCATE( EM_MASK_AERO( N_EMIS_SPC ) )
         EM_SPEC = ''
         EM_MASK_AERO = .FALSE.

         ALLOCATE( MAP_EMtoSEA( N_EMIS_SPC ) )
         ALLOCATE( MAP_EMtoDUST( N_EMIS_SPC ) )
         MAP_EMtoSEA = 0
         MAP_EMtoDUST = 0

         ISPC = 0
         DO IGC = 1,N_GC_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = GC_SPC( GC_EMIS_MAP( IGC ) )
         END DO
         DO IAE = 1,N_AE_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = AE_SPC( AE_EMIS_MAP( IAE ) )
             EM_MASK_AERO( ISPC ) = .TRUE.
             MAP_EMtoSEA( ISPC ) = MAP_AEtoSEA( IAE )
             IF ( WB_DUST ) MAP_EMtoDUST( ISPC ) = MAP_AEtoDUST( IAE )
         END DO
         DO INR = 1,N_NR_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = NR_SPC( NR_EMIS_MAP( INR ) )
         END DO
         DO ITR = 1,N_TR_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = TR_SPC( TR_EMIS_MAP( ITR ) )
         END DO


         ! Determine which aerosol species emissions are for number and 
         ! surface area. Create maps from Emission Surrogate Species to 
         ! the number and surface area of each mode. 
         ALLOCATE( MAP_EMtoNUM( N_EMIS_SPC ) )
         ALLOCATE( MAP_EMtoSRF( N_EMIS_SPC ) )
         ALLOCATE( MAP_EMtoAERO( N_EMIS_SPC ) )
         ALLOCATE( MAP_EMtoMODE( N_EMIS_SPC ) )
         MAP_NUMtoEM = 0
         MAP_SRFtoEM = 0
         MAP_EMtoNUM = 0
         MAP_EMtoSRF = 0
         MAP_EMtoAERO = 0
         MAP_EMtoMODE = 0
         MAP_EMtoAERO = 0

         DO IM = 1,N_MODE
             IDX = INDEX1( AEROMODE( IM )%NUM_NAME, N_EMIS_SPC, EM_SPEC )
             IF ( IDX .GT. 0 ) THEN
                 ! Found Aerosol Number for this Mode
                 MAP_NUMtoEM( IM ) = IDX
                 MAP_EMtoNUM( IDX ) = IM
             ELSE
                 WRITE( XMSG, '(A,I1)'),'Cannot find Number Emissions for Mode ',IM 
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             IDX = INDEX1( AEROMODE( IM )%SRF_NAME, N_EMIS_SPC, EM_SPEC )
             IF ( IDX .GT. 0 ) THEN
                 ! Found Aerosol Number for this Mode
                 MAP_SRFtoEM( IM ) = IDX
                 MAP_EMtoSRF( IDX ) = IM
             ELSE
                 WRITE( XMSG, '(A,I1)'),'Cannot find Surface Area Emissions for Mode ',IM 
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF 

             ! Map Aerosol Species to Emissions List
             DO IAERO = 1,N_AEROSPC
                 IDX = INDEX1( AEROSPC( IAERO )%NAME( IM ), N_EMIS_SPC, EM_SPEC )
                 IF ( IDX .GT. 0 ) THEN
                     ! Load Maps from EM_SPEC to Aerosol Type and Mode
                     MAP_EMtoAERO( IDX ) = IAERO
                     MAP_EMtoMODE( IDX ) = IM
                     MAP_AEROtoEM( IAERO,IM ) = IDX
                 END IF
                 ! Load Size Distribution Factors
                 EMWT_PROF( IAERO,IM ) = AEROSPC( IAERO )%EMIS_SPLIT( IM )
            END DO
         END DO

         ! Populate EM_SURR_BUFF Array with Master List of All Requested
         ! Emissions Surrogate Species. These lists will be distributed
         ! to the individual sources in the EM_SURR ARRAY.
         ALLOCATE( EM_SURR_BUFF( N_EMIS_SPC ) )
         ALLOCATE( EM_FAC_BUFF( N_EMIS_SPC ) )
         EM_SURR_BUFF = ''
         EM_FAC_BUFF = 0

         EM_SURR_BUFF( 1:N_GC_EMIS ) = GC_EMIS( 1:N_GC_EMIS )     !Gases
         EM_FAC_BUFF ( 1:N_GC_EMIS ) = GC_EMIS_FAC( 1:N_GC_EMIS )     !Gases
         N = N_GC_EMIS
         EM_SURR_BUFF( N+1:N+N_AE_EMIS ) = AE_EMIS( 1:N_AE_EMIS ) !Aerosols
         EM_FAC_BUFF ( N+1:N+N_AE_EMIS ) = AE_EMIS_FAC( 1:N_AE_EMIS ) !Aerosols
         N = N + N_AE_EMIS
         EM_SURR_BUFF( N+1:N+N_NR_EMIS ) = NR_EMIS( 1:N_NR_EMIS ) !Non-Reactives
         EM_FAC_BUFF ( N+1:N+N_NR_EMIS ) = NR_EMIS_FAC( 1:N_NR_EMIS ) !Non-Reactives
         N = N + N_NR_EMIS
         EM_SURR_BUFF( N+1:N+N_TR_EMIS ) = TR_EMIS( 1:N_TR_EMIS ) !Tracers
         EM_FAC_BUFF ( N+1:N+N_TR_EMIS ) = TR_EMIS_FAC( 1:N_TR_EMIS ) !Tracers
 
         ! Apply Uniform Volatility Distribution Across All Sources
         ! This algorithm distributes the factors across affected
         ! particles and vapors using parameters from AERO_DATA.
         DO ISPC = 1,N_EMIS_SPC
           IF ( EM_SURR_BUFF( ISPC ) .EQ. 'POC' ) THEN
             DO IPOA = 1,N_VBS_BIN
               IF ( EM_SPEC( ISPC )(2:6) .EQ. POA_NAME( IPOA ) ) THEN
                 IF ( EM_MASK_AERO( ISPC ) ) THEN
                   EM_FAC_BUFF( ISPC ) = EM_FAC_BUFF( ISPC )
     &                                 * POA_OP_VF( IPOA ) * POA_AMF( IPOA ) 
                 ELSE
                   V = INDEX1( EM_SPEC( ISPC ), N_GC_SPC, GC_SPC )  
                   EM_FAC_BUFF( ISPC ) = EM_FAC_BUFF( ISPC ) * POA_OP_VF( IPOA )
     &                                 * (1.0 - POA_AMF( IPOA ) ) / GC_MOLWT( V )
                 END IF
               END IF
             END DO
           END IF
         END DO    
 
         ! Populate Arrays Mapping CMAQ Species to Species on Files
         ! For each emission source, check to see which Emissions
         ! species are actually represented. Overwrite the entry with a
         ! blank if the species is not present from the source/file. If
         ! there is a species not found on any file or a species on a
         ! file that is never used, trigger an error (if EMISCHK is
         ! TRUE).
         ALLOCATE( EM_SURR( N_EMIS_SPC,N_EM_SRC ) )
         EM_SURR = ''
         ALLOCATE( EM_FAC( N_EMIS_SPC,N_EM_SRC ) )
         EM_FAC = 0.0

         ALLOCATE( L_EMIS_SPC( NEMIS_SPC_UNIQUE ) )
         L_EMIS_SPC = .FALSE.

         ! Populate the Emissions Surrogate Species 
         DO ISRC = 1,N_EM_SRC
           LERROR = .TRUE.
           DO ISPC = 1,N_EMIS_SPC
             ! Test if Surrogate Matches the Species Names on the
             ! Emission File
             IDX = INDEX1( EM_SURR_BUFF( ISPC ), 
     &                     EM_FILE_SPEC( ISRC )%LEN, 
     &                     EM_FILE_SPEC( ISRC )%ARRY )
             IF ( IDX .GT. 0 ) THEN
                 ! A match for this surrogate is found. Avoid Printing
                 ! a warning for this source
                 LERROR = .FALSE.

                 ! Match the surrogate and scale factor arrays for this
                 ! source to the correct buffer entry.
                 EM_SURR( ISPC,ISRC ) = EM_SURR_BUFF( ISPC )
                 EM_FAC ( ISPC,ISRC ) = EM_FAC_BUFF ( ISPC )

                 ! For Biogenic and Marine Gas Emissions, build mapping
                 ! arrays to enable processing
                 IF ( EM_FILE_TYPE( ISRC ) .EQ. "BIOG" ) THEN
                   BEIS_MAP( IDX ) = ISPC

                 ELSE IF ( EM_FILE_TYPE( ISRC ) .EQ. "MGEM" ) THEN
                   MGSPC_MAP( IDX ) = ISPC

                 END IF     

                 ! Mark this species as found on the master list
                 IDX = INDEX1( EM_SURR( ISPC,ISRC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                 IF ( IDX .GT. 0 ) L_EMIS_SPC( IDX ) = .TRUE.
             END IF

             ! Test Specifically for Aerosol Model Species instead of
             ! surrogate names for the dust module
             IF ( EM_FILE_TYPE( ISRC ) .EQ. "DUST" ) THEN
                IF ( MAP_EMtoDUST( ISPC ) .GT. 0 ) THEN
                   ! This aerosol is represented by the dust module
                   LERROR = .FALSE.
                   EM_SURR( ISPC,ISRC ) = EM_SURR_BUFF( ISPC )
                   EM_FAC ( ISPC,ISRC ) = EM_FAC_BUFF ( ISPC )

                   ! Mark this species as found on the master list
                   IDX = INDEX1( EM_FILE_SPEC( ISRC )%ARRY( MAP_EMtoDUST( ISPC ) ), 
     &                           NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                   IF ( IDX .GT. 0 ) L_EMIS_SPC( IDX ) = .TRUE.
                END IF
             END IF
              
             ! Test Specifically for Aerosol Model Species instead of
             ! surrogate names for the sea spray module
             IF ( EM_FILE_TYPE( ISRC ) .EQ. "ASEA" ) THEN
                IF ( MAP_EMtoSEA( ISPC ) .GT. 0 ) THEN
                   ! This aerosol is represented by the sea spray module
                   LERROR = .FALSE.
                   EM_SURR( ISPC,ISRC ) = EM_SURR_BUFF( ISPC )
                   EM_FAC ( ISPC,ISRC ) = EM_FAC_BUFF ( ISPC )

                   ! Mark this species as found on the master list
                   IDX = INDEX1( EM_FILE_SPEC( ISRC )%ARRY( MAP_EMtoSEA( ISPC ) ), 
     &                           NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                   IF ( IDX .GT. 0 ) L_EMIS_SPC( IDX ) = .TRUE.
                END IF
             END IF         

           END DO
           IF ( LERROR ) THEN
              WRITE( LOGDEV, '(/,A,I2,A2,A)'),
     &         'There are no matching surrogates for Source ',ISRC,': ',EM_FILE_DESC( ISRC )
           END IF
         END DO

C Compare with Gas, Aerosol, Nonreactive, and Tracer Species
         ! Initialize variables for tracking surrogate matches
         LERROR = .FALSE.

         ! Check Compatability with Gas Emissions Surrogates
         IF ( N_GC_EMIS .GT. 0 ) THEN
             ALLOCATE( GC_EMIS_MISSING( N_GC_EMIS ) )
             GC_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_GC_EMIS
                IDX = INDEX1( GC_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .EQ. 0 ) GC_EMIS_MISSING( ISPC ) = .TRUE.
             END DO
             IF ( ANY( GC_EMIS_MISSING ) ) THEN
                ALLOCATE( GC_EMIS_WRITE( COUNT( GC_EMIS_MISSING ) ))
                GC_EMIS_WRITE = PACK( GC_EMIS(1:N_GC_EMIS), GC_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A48,/,2x,A51,/,2x,A42,/,5(5x,A16))'),
     &             'The following GAS emission surrogate species are',
     &             'not present from any of the Area, Point, Biogenic,',
     &             'Marine Gas, or Lightning emissions sources:',
     &             GC_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
          
         ! Check Compatability with Aerosol Emissions Surrogates
         IF ( N_AEROSPC .GT. 0 ) THEN 
             ALLOCATE( AERO_EMIS_MISSING( N_AE_EMIS ) )
             AERO_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_AE_EMIS
                IDX = INDEX1( AE_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .EQ. 0 ) AERO_EMIS_MISSING( ISPC ) = .TRUE.
             END DO 
             IF ( ANY( AERO_EMIS_MISSING ) ) THEN
                ALLOCATE( AERO_EMIS_WRITE( COUNT( AERO_EMIS_MISSING ) ) ) 
                AERO_EMIS_WRITE = PACK( AE_EMIS(1:N_AE_EMIS), AERO_EMIS_MISSING )

                WRITE( LOGDEV, '(/A40,/,2x,A52,/,2x,A53,/,5(5x,A16))'),
     &             'The following AEROSOL emission surrogate',
     &             'species are not present from any of the Area, Point,',
     &             'Biogenic, Marine Gas, or Lightning emissions sources:',
     &             AERO_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
          
         ! Check Compatability with Nonreactive Emissions Surrogates
         IF ( N_NR_EMIS .GT. 0 ) THEN
             ALLOCATE( NR_EMIS_MISSING( N_NR_EMIS ) )
             NR_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_NR_EMIS
                IDX = INDEX1( NR_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .EQ. 0 ) NR_EMIS_MISSING( ISPC ) = .TRUE.
             END DO 
             IF ( ANY( NR_EMIS_MISSING ) ) THEN
                ALLOCATE( NR_EMIS_WRITE( COUNT( NR_EMIS_MISSING ) ) ) 
                NR_EMIS_WRITE = PACK( NR_EMIS(1:N_NR_EMIS), NR_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A45,/,A53,/,A54,/,5(5x,A16))'),
     &             'The following NONREACTIVE emission surrogate ',
     &             'species are not present from any of the Area, Point, ',
     &             'Biogenic, Marine Gas, or Lightning emissions sources: ',
     &             NR_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
         
         ! Check Compatability with Tracer Emissions Surrogates
         IF ( N_TR_EMIS .GT. 0 ) THEN
             ALLOCATE( TR_EMIS_MISSING( N_TR_EMIS ) )
             TR_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_TR_EMIS
                IDX = INDEX1( TR_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .GT. 0 ) TR_EMIS_MISSING( ISPC ) = .TRUE.
             END DO 
             IF ( ANY( TR_EMIS_MISSING ) ) THEN
                ALLOCATE( TR_EMIS_WRITE( COUNT( TR_EMIS_MISSING ) ) ) 
                TR_EMIS_WRITE = PACK( TR_EMIS(1:N_TR_EMIS), TR_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A40,/,A53,/,A54,/,5(5x,A16))'),
     &             'The following TRACER emission surrogate ',
     &             'species are not present from any of the Area, Point, ',
     &             'Biogenic, Marine Gas, or Lightning emissions sources: ',
     &             TR_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF

C Print a warning for each emissions file species that is not used.
         N_UNUSED = COUNT( .NOT. L_EMIS_SPC )
         IF ( N_UNUSED .GT. 0 ) THEN 
           ALLOCATE( EMIS_SPC_UNUSED( N_UNUSED ) )
           EMIS_SPC_UNUSED = PACK( EMIS_SPC_UNIQUE,( .NOT. L_EMIS_SPC ) ) 
         END IF
        
C Stop the Model if Any Emissions Surrogate Species are
C Missing from All Emission Input Sources
         IF ( LERROR ) THEN
           IF ( .NOT. EMISCHK ) THEN
              WRITE( LOGDEV, '(5(/,A))') 
     &               '************************WARNING***********************:',
     &               'One or more emissions surrogates assigned to model species ',
     &               'are not found in emissions sources but the CTM_EMISCHK ',
     &               'environment variable set to False so simulation will proceed.',
     &               '*******************************************************'

              WRITE( LOGDEV, '(/,A55,/,A34,I3,A8,/,4(2x,A16))')
     &              'ATTENTION: The following emission species are available',
     &              'from the inputs but are not used: ',N_UNUSED,' Species',
     &              EMIS_SPC_UNUSED

              WRITE( LOGDEV, * )
              WRITE( LOGDEV, * )
              XMSG = 'For optimal predictions, species with the missing surrogates '
     &             //'should have a surrogate found in at least one source.'
              CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
           ELSE     
              WRITE( LOGDEV, '(3(/,A),/,5(/,A),/,A,4(/,A))') 
     &               '*************************ERROR**********************:',
     &               'One or more emission surrogates assigned to model species ',
     &               'are not found in emission sources.',
     &               'Use one of the below options to continue.', 
     &               '1) Change or remove the missing GC, NR and TR emissions surrogates from the',
     &               'namelists where they are defined.', 
     &               'or',
     &               '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &               'if model predictions are acceptable without using the above emissions.',
     &               '*****************************************************'

              WRITE( LOGDEV, '(/,A55,/,A34,I3,A8,/,4(2x,A16))')
     &             'ATTENTION: The following emission species are available',
     &             'from the inputs but are not used: ',N_UNUSED,' Species',
     &              EMIS_SPC_UNUSED
              WRITE( LOGDEV, * )
              WRITE( LOGDEV, * )
              XMSG = 'Species with the missing surrogates ' 
     &             //'must have a surrogate found in at least one source.'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF
         END IF



         END SUBROUTINE EMIS_SPC_MAP

C-----------------------------------------------------------------------
         SUBROUTINE EM_FILE_INIT( JDATE, JTIME )

C Initialize the counter for the total nbumber of emissions files. Also
C allocate memory for the vectors storing the labels of emission files
C and the maps from master ID number to the relative ID number for each
C gridded and point source file, i.e. Emissions File 10 is also known as
C Point Source file 2.
C
         USE AERO_EMIS, only : WB_DUST
         USE BEIS_DEFN, only : BIOGEMIS ! Online Biogenic Emissions Env Variable
         USE LTNG_DEFN, only : LTNG_NO  ! Flag for Lightning Emissions
         USE UTILIO_DEFN         ! I/O API
         USE RXNS_DATA, ONLY: MECHNAME
         USE MGEMIS,    only : MGEM     ! Flag for Marine Gas Emissions

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME
         INTEGER N_EMIS_GR, NPTGRPS, N_EMIS_TR

         INTEGER IOS, LOGDEV, ISRC, N

         CHARACTER( 16 )  :: CTM_BIOGEMIS = 'CTM_BIOGEMIS'    ! Let model proceed even if
         CHARACTER( 16 )  :: CTM_LTNG_NO = 'CTM_LTNG_NO'    ! Let model proceed even if
         CHARACTER( 16 )  :: PNAME = 'EM_FILE_INIT'
         CHARACTER( 80 )  :: VARDESC
         INTEGER          :: STATUS
         CHARACTER( 200 ) :: XMSG
         LOGICAL :: SUCCESS

         LOGDEV = INIT3()
         SUCCESS = .TRUE.

C Retrieve the number of each type of emission file             
         N_EMIS_GR = ENVINT( 'N_EMIS_GR',
     &                     'Number of Gridded Area Emission Files',1, IOS )
         NPTGRPS   = ENVINT( 'N_EMIS_PT',
     &                     'Number of file groups',0, IOS )
         N_EMIS_TR = ENVINT( 'N_EMIS_TR',
     &                     'Number of Tracer Emission Files',0, IOS )
         N_EM_SRC = N_EMIS_GR + NPTGRPS + N_EMIS_TR
 
C Are Biogenic Emissions Calculated On-Line
         BIOGEMIS = .FALSE.
         VARDESC = 'Flag for in-line biogenic emissions'
         BIOGEMIS = ENVYN( CTM_BIOGEMIS, VARDESC, BIOGEMIS, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF
         IF ( BIOGEMIS ) N_EM_SRC = N_EM_SRC + 1

C Marine gas emissions; use in-line marine gas option only if CB05EH51 is used
         IF ( INDEX( MECHNAME, 'CB05EH51' ) .GT. 0 ) then
            MGEM = .TRUE.
         ELSE
            MGEM = .FALSE.
            RETURN
         END IF
         IF ( MGEM ) N_EM_SRC = N_EM_SRC + 1

C Determine if Lightning NO is Requested
         VARDESC = 'Flag for lightning emissions'
         LTNG_NO = ENVYN( CTM_LTNG_NO, VARDESC, .FALSE., STATUS )
         IF ( LTNG_NO ) N_EM_SRC = N_EM_SRC + 1
 
C Determine if WindBlown Dust is Requested
         WB_DUST = ENVYN( 'CTM_WB_DUST', 'Flag for windblown dust emissions',
     &                   .TRUE., STATUS )
         IF ( WB_DUST ) N_EM_SRC = N_EM_SRC + 1

C Sea Spray Aerosol Emissions Are Always Utilized
         N_EM_SRC = N_EM_SRC + 1

C Allocate Emission File Structure Variables
         Allocate( EM_FILE_NAME( N_EM_SRC ) )
         Allocate( EM_FILE_LAB ( N_EM_SRC ) )
         Allocate( EM_FILE_TYPE( N_EM_SRC ) )
         Allocate( EM_FILE_DESC( N_EM_SRC ) )
         Allocate( IGSRC( N_EM_SRC ) )
         Allocate( IPSRC( N_EM_SRC ) )
         Allocate( ITSRC( N_EM_SRC ) )
         Allocate( EM_FILE_SPEC( N_EM_SRC ) )
         Allocate( EM_GRID_LAYS( N_EM_SRC ) )
 
C Assign Attributes to Emission File Records. Other records will be 
C populated in individual subroutines. For example, opemis and
C stkemis_init.
         ! Gridded Emission Files
         EM_FILE_TYPE( 1:N_EMIS_GR ) = 'GRID'
         DO ISRC = 1, N_EMIS_GR
            N = N + 1
            IGSRC( N ) = ISRC
            WRITE( EM_FILE_DESC( N ), '(A,I2)' ),
     &         'Gridded Area Emissions File ',IGSRC(N)
         END DO
         
         ! In-Line Point Source Files
         IF ( NPTGRPS .GT. 0 ) THEN 
            EM_FILE_TYPE( N+1:N+NPTGRPS ) = 'POINT'
            DO ISRC = 1, NPTGRPS
               N = N + 1
               IPSRC( N ) = ISRC
               IF ( NPTGRPS .GT. 0 ) THEN
                  WRITE( EM_FILE_DESC( N ), '(A,I2)' ),
     &            'Point Emissions File ', IPSRC(N)
               END IF
            END DO
         END IF
         
         ! Tracer Emissions
         IF ( N_EMIS_TR .GT. 0 ) THEN 
            EM_TRAC = .TRUE.
            EM_FILE_TYPE( N+1:N+N_EMIS_TR ) = 'TRAC'
            DO ISRC = 1, N_EMIS_TR
               N = N + 1
               ITSRC( N ) = ISRC
               WRITE( EM_FILE_DESC( N ), '(A,I2)' ),
     &           'Gridded Tracer Emissions File ', ITSRC(N)
            END DO
         ELSE
            EM_TRAC = .FALSE.
         END IF
         
         ! Online Biogenic Emissions (BEIS)
         IF ( BIOGEMIS ) THEN
             N = N + 1
             EM_FILE_TYPE( N ) = 'BIOG'
             EM_FILE_DESC( N ) = 'Biogenic Emissions'
             IBIOSRC = N
         END IF

         ! Online Marine Gas Emissions
         IF ( MGEM ) THEN
             N = N + 1
             EM_FILE_TYPE( N ) = 'MGEM'
             EM_FILE_DESC( N ) = 'Marine Gas Emissions'
             IMGSRC = N
         END IF

         ! Online Lightning NO Emissions
         IF ( LTNG_NO ) THEN
             N = N + 1
             EM_FILE_TYPE( N ) = 'LTNG'
             EM_FILE_DESC( N ) = 'Lightning NO Emissions'
             ILTSRC = N
         END IF

         ! Sea Spray Aerosol Emissions
         N = N + 1
         EM_FILE_TYPE( N ) = 'ASEA'
         EM_FILE_DESC( N ) = 'Sea Spray Aerosol Emissions'
         ISEASRC = N

         ! Wind-Blown Dust Emissions
         IF ( WB_DUST ) THEN
             N = N + 1
             EM_FILE_TYPE( N ) = 'DUST'
             EM_FILE_DESC( N ) = 'Wind-Blown Dust Emissions'
             IDUSTSRC = N
         END IF
          


         END SUBROUTINE EM_FILE_INIT    

      END MODULE EMIS_DEFN
