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
      USE EMIS_VARS
      USE VDIFF_MAP, ONLY : N_SPC_DIFF

      IMPLICIT NONE

      PUBLIC EMIS_INIT, GET_EMIS, EM_GRID_LAYS
      
      PRIVATE
      
      LOGICAL,              SAVE :: EMISCHK ! Let model proceed even if Emis Surrogates 
                                            ! are not properly defined
      
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_TMP( :,:,:,: ) ! total emissions array
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_GR ( :,:,:,: ) ! total emissions array
      INTEGER, ALLOCATABLE, SAVE :: EM_GRID_LAYS( : ) ! no. of area emission layers
      
      LOGICAL, ALLOCATABLE, SAVE :: EM_MASK_AERO( : )! Store the location of aerosol species in master 
                                                     ! emissions vector
                                     
      LOGICAL, SAVE :: EM_TRAC    ! do tracer emissions?
      REAL,    SAVE :: DT         ! TSTEP (output) in sec

      REAL,    ALLOCATABLE, SAVE :: BUFF( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: BUFF2( :,:,: )       ! read buffer
      REAL,    ALLOCATABLE, SAVE :: FERT( :,:,: )        ! read buffer

      INTEGER, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                 STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, SAVE :: NDATE, NTIME  ! step next date/time (beis)

      REAL(8), SAVE   :: DX1, DX2          ! CX x1- and x2-cell widths

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
                                        
C Local Variables:

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

C Open Area Emissions files
         CALL OPEMIS ( JDATE, JTIME, EM_TRAC )

C Get number of emissions layers
         VARDESC = 'Number of emission layers'
         EMLAYS = ENVINT( CTM_EMLAYS, VARDESC, NLAYS, STATUS )
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

C Initialize 3D Point Source Emissions 
         IF ( .NOT. PT3D_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

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
 
C Initialize Online Lightning NOx Emissions
         IF ( .NOT. LTNG_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing lightning emissions module'
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
         ALLOCATE ( VDEMIS( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_DIFF( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_TMP( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_TMP memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_GR( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
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
         INTEGER          C, R, L, N, S, V, ISTR, ISRC ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables
         
         REAL, PARAMETER :: GPKG = 1.0E+03       ! g/kg
         REAL, PARAMETER :: MWAIR = 28.9628      ! g/mol
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
            CNVTP = CONVPA * 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 

         END IF    !FirstTime

C Initialize Emissions Array
         VDEMIS = 0.0


C Retrieve Emissions from All Source Types
         DO ISRC = 1,N_EM_SRC
           VDEMIS_TMP = 0.0

           SELECT CASE ( EM_FILE_TYPE( ISRC ) )
           ! Each Case Returns Emission Rates for gases in kmol/s and
           ! for aerosols in kg/s

             ! Retrieve Gridded Emissions
             CASE ( "GRID" )
               CALL GRIDEMIS ( JDATE, JTIME, EM_FILE_NAME( ISRC ), 
     &                         EM_GRID_LAYS( ISRC ), ISRC )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS_TMP( ISTR,1,:,: ) = EM_FAC( ISTR,ISRC ) * CONVEM( ISRC )
     &                  * VDEMIS_GR( ISTR,:,:,: ) 
               END FORALL
               CALL EMISS_SIZE_DIST( VDEMIS_TMP, ISRC )
               VDEMIS( :,:,:,: ) = VDEMIS( :,:,:,: ) + VDEMIS_TMP( :,:,:,: )

             ! Retrieve Point Source Emissions
             CASE ( "POINT" )
               CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, EM_SURR( :,ISRC ), ISRC )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS_TMP( ISTR,1,:,: ) = EM_FAC( ISTR,ISRC ) * CONVEM( ISRC )
     &                  * VDEMIS_PT( ISTR,:,:,: ) 
               END FORALL
               CALL EMISS_SIZE_DIST( VDEMIS_TMP, ISRC )
               VDEMIS( :,:,:,: ) = VDEMIS( :,:,:,: ) + VDEMIS_TMP( :,:,:,: )

             ! Retrieve Biogenic Emissions from BEIS
             CASE ( "BIOG" )
               CALL GET_BEIS ( JDATE, JTIME, TSTEP )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS( ISTR,1,:,: ) = VDEMIS( ISTR,1,:,: ) + EM_FAC( ISTR,ISRC ) 
     &                  * VDEMIS_BI( MAP_EMtoSURR( ISTR,ISRC ),:,: ) * CONVEM( ISRC )
               END FORALL

             ! Retrieve Marine Gas Emissions
             CASE ( "MGEM" )
               CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS( ISTR,1,:,: ) = VDEMIS( ISTR,1,:,: ) + EM_FAC( ISTR,ISRC ) 
     &                  * VDEMIS_MG( MAP_EMtoSURR( ISTR,ISRC ),:,: ) * CONVEM( ISRC )
               END FORALL

             ! Retrieve Lightning NO Emissions
             CASE ( "LTNG" )
               CALL GET_LTNG ( JDATE, JTIME, TSTEP )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS( ISTR,:,:,: ) = VDEMIS( ISTR,:,:,: ) + EM_FAC( ISTR,ISRC ) 
     &                  * VDEMIS_LT( :,:,: ) * CONVEM( ISRC )
               END FORALL

             ! Retrieve Sea Spray Aerosol Emissions (g/m3/s)
             CASE ( "ASEA" )
               CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( 1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS_TMP( ISTR,1,:,: ) = EM_FAC( ISTR,ISRC ) * CONVEM( ISRC )
     &                  * SSOUTM( MAP_EMtoSURR( ISTR,ISRC ),MAP_EMtoMODE( ISTR ):,: ) 
               END FORALL
               CALL EMISS_SIZE_DIST( VDEMIS_TMP, ISRC )
               VDEMIS( :,:,:,: ) = VDEMIS( :,:,:,: ) + VDEMIS_TMP( :,:,:,: )

             ! Retrieve Wind-Blown Dust Emissions (g/m3/s)
             CASE ( "DUST" )
               CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS_TMP( ISTR,1,:,: ) = EM_FAC( ISTR,ISRC ) * CONVEM( ISRC )
     &                  * DUSTOUTM( MAP_EMtoSURR( ISTR,ISRC ),MAP_EMtoMODE( ISTR ):,: ) 
               END FORALL
               CALL EMISS_SIZE_DIST( VDEMIS_TMP, ISRC )
               VDEMIS( :,:,:,: ) = VDEMIS( :,:,:,: ) + VDEMIS_TMP( :,:,:,: )

           END SELECT

         END DO  

C Convert All Emissions to Units Appropriate for the Dispersion Solver
         DO L = 1, EMLAYS
            CNVTI = CNVTP * Grid_Data%RDX3F( L ) 
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  CNVTC = CNVTI * Met_Data%RRHOJ( C,R,L )       ! Gas:       mol/s -> ppmv/s
                  CONVM = MWAIR / GPKG / Met_Data%DENS( C,R,L ) ! Aer. Mass: umol/m3/s -> ppmv/s
                  CONVN = CONVM * RAVO * 1.0E+06                ! Aer. Num:  N/m3/s -> N/molec/s
                  CONVS = CONVM                                 ! Aer. Srf:  m2/m3/s -> m2/mol/s
                  
                  !MAKE SURE TO APPLY RELEVANT UNIT CONVERSION TO EACH
                  !TYPE OF VARIABLE (GAS, AEROSOL MASS, AEROSOL NUMBER,
                  !AND AEROSOL SURFACE AREA)
                  WHERE( MAP_EMtoNUM .EQ. 0 .AND. MAP_EMtoSRF .EQ. 0 .AND.
     &                   MAP_EMtoAERO .EQ. 0 )
     &                   VDEMIS( :,L,C,R ) = CNVTC * VDEMIS( :,L,C,R ) 
                  WHERE( MAP_EMtoAERO .NE. 0 .AND. MAP_EMtoNUM .EQ. 0 .AND.
     &                   MAP_EMtoSRF .EQ. 0 )
     &                   VDEMIS( :,L,C,R ) = CONVM * VDEMIS( :,L,C,R ) 
                  WHERE( MAP_EMtoNUM .NE. 0 )
     &                   VDEMIS( :,L,C,R ) = CONVN * VDEMIS( :,L,C,R ) 
                  WHERE( MAP_EMtoSRF .NE. 0 )
     &                   VDEMIS( :,L,C,R ) = CONVS * VDEMIS( :,L,C,R ) 

               END DO
            END DO
         END DO
 
C zero out emissions values for diffused species not included in emissions list.
C ...accounts for emissions species names as a subset of the vert. diffused species list

         VDEMIS_DIFF = 0.0
         DO ISTR = 1,N_EMIS_ISTR
           VDEMIS_DIFF( MAP_EMtoDIFF( ISTR ),:,:,: ) = 
     &        VDEMIS_DIFF( MAP_EMtoDIFF( ISTR ),:,:,: ) + VDEMIS( ISTR,:,:,: )
         END DO

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
         CHARACTER( 200 ) :: XMSG = ' '
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

          DO ISPC = 1, N_EMIS_ISTR 
            VNAME = EM_SURR( ISPC, FSRC )
            IF ( VNAME .EQ. '' ) CYCLE

            BUFF = 0.0   ! array assignment
            IF ( .NOT. INTERPX( EMIS_FNAME, VNAME, PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW,1,LAYS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read '
     &              // TRIM( VNAME ) // ' from ' // EMIS_FNAME
               IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF

C Store all emissions in mol/sec or g/sec and convert to ppmv/s later
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, LAYS
                     VDEMIS_GR( ISPC,L,C,R ) = BUFF( C,R,L )
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

         USE VDIFF_MAP, only : DIFF_SPC, ! Name for every Diffusion Module Species                                  
     &                         DIFF_MASK_GAS, DIFF_MASK_AERO, DIFF_MASK_NR, 
     &                         DIFF_MASK_TRAC
         USE UTILIO_DEFN
         USE AERO_DATA, only : N_MODE, AEROSPC,  ! Aerosol Properties Table
     &                         N_AEROSPC, AEROMODE, N_MAP_AEROtoPTCL, MAP_AEROtoPTCL,
     &                         MODESUFF
         USE AERO_EMIS, only : MAP_NUMtoEM, MAP_SRFtoEM, MAP_EMtoAERO, MAP_EMtoMODE, 
     &                         MAP_AEROtoEM,MAP_EMtoNUM, MAP_EMtoSRF, WB_DUST
         USE UDTYPES,   only : CARRY1, LARRY1

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME
         INTEGER    :: N_UNUSED

         INTEGER    :: STRT, PTSTRT, ISPC, IDX, LOGDEV, IX, ISUR, IRULE,
     &                 N, IM, IAERO, V, ISRC,NSPC, IA, IDIFF, JDX, JM,
     &                 ISTR, ISUR0, ISRC0

         LOGICAL    :: LERROR, LFOUND
         
         TYPE( CARRY1 ),  ALLOCATABLE, SAVE :: EM_FILE_SURR_2( : ) !Species to be read from each source file

         TYPE( LARRY1 ),  ALLOCATABLE, SAVE :: EM_FILE_SURR_USED( : ) 
         CHARACTER( 16 )  :: SPECNAME, SN

         CHARACTER( 16 )  :: CTM_EMISCHK = 'CTM_EMISCHK'    ! Let model proceed even if
                                                                 ! Emissions surrogates are
                                                                 ! not properly defined
         CHARACTER( 16 )  :: PNAME = 'EMIS_SPC_CHECK'
         CHARACTER( 100)  :: VARDESC
         INTEGER          :: STATUS
         CHARACTER( 500 ) :: XMSG
         CHARACTER( 16 )  :: B
         
         INTEGER, PARAMETER :: NISTR0 = 3000
         LOGICAL, ALLOCATABLE, SAVE :: EM_SRC_RULE( : ), EM_SPEC_RULE( : )
         TYPE( LARRY1 ), ALLOCATABLE, SAVE :: EM_SURR_RULE( : )
        
         INTEGER          :: IC, N_COMMANDS
         INTEGER          :: RULE_ISTR_IDIFF( NISTR0 )
         INTEGER          :: RULE_ISTR_ISRC ( NISTR0 )
         INTEGER          :: RULE_ISTR_ISUR ( NISTR0 )
         CHARACTER( 16 )  :: RULE_ISTR_SPEC ( NISTR0 )
         CHARACTER( 16 )  :: RULE_ISTR_SURR ( NISTR0 )

! Define General Parameters in Emission Control Namelist
         CHARACTER( 300 ) :: EQNAME
         INTEGER          :: EMCTRL_NML

         LOGICAL :: DefaultScaling      = .FALSE.
         LOGICAL :: Guard_BiogenicVOC   = .FALSE.
         LOGICAL :: Guard_MarineGas     = .FALSE.
         LOGICAL :: Guard_LightningNO   = .FALSE.
         LOGICAL :: Guard_WindBlownDust = .FALSE.
         LOGICAL :: Guard_SeaSpray      = .FALSE.
         NAMELIST / GeneralSpecs / DefaultScaling, Guard_BiogenicVOC,
     &              Guard_MarineGas, Guard_LightningNO, Guard_WindBlownDust,
     &              Guard_SeaSpray

! Define Emissions Rules Input from Emissions Control Namelist
         TYPE EM_RULE
             CHARACTER( 16 ) :: SPEC
             CHARACTER( 32 ) :: SRC
             CHARACTER( 16 ) :: PHASE
             CHARACTER( 16 ) :: SURR
             REAL            :: FAC
             CHARACTER( 1  ) :: OP
         END TYPE EM_RULE
         INTEGER, PARAMETER  :: N_EM_RULE = 200
         TYPE( EM_RULE ) :: EM_NML( N_EM_RULE )
         NAMELIST / EmissionScaling / EM_NML

         TYPE SD_RULE
             CHARACTER( 32 ) :: SRC
             CHARACTER( 16 ) :: MODE
             CHARACTER( 16 ) :: MODE_REF
         END TYPE SD_RULE
         INTEGER, PARAMETER  :: N_SD_RULE = 200
         TYPE( SD_RULE ) :: SD_NML( N_SD_RULE )
         NAMELIST / SizeDistRules / SD_NML 

! Retrieve Environment Variable Letting User Ignore this Check
! and allowing the model to proceed.
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

! Open and Begin Reading The Emission Control Namelist
         CALL NAMEVAL( "EMISSCTRL_NML", EQNAME )
         
         EMCTRL_NML = JUNIT()
         OPEN( FILE = EQNAME, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &         POSITION = 'REWIND' )
         READ( NML = GeneralSpecs, UNIT = EMCTRL_NML )


! Retrieve the Surrogates Available From Emissions Sources
         WRITE( LOGDEV, '(/,A31,A42)' ),'CHECKING COMPATABILITY BETWEEN ',
     &                  'INTERNAL SPECIES LIST AND EMISSIONS INPUTS' 
         WRITE( LOGDEV, '(A58,/,A58)' ),
     &                  'The following tables list the chemical surrogates present ',
     &                  'on each emission file or from each online emission source.' 

         ! Print Out the Species From Each Emissions Source
         DO ISRC = 1,N_EM_SRC
            NSPC = EM_FILE_SURR( ISRC )%LEN

            WRITE( LOGDEV, '(/,4A,I3,A8,/,4(2x,A16))' ),
     &           TRIM( EM_FILE_DESC( ISRC ) ),' -- ',TRIM(EM_FILE_LAB( ISRC )),
     &           ' -- ',NSPC,' Species',
     &           EM_FILE_SURR( ISRC )%ARRY
         END DO

         ! For aerosols, which may differ in name from the surrogates, 
         ! try using the common name transformations (e.g. PNO3 --> 
         ! ANO3). These are located in AERO_DATA for reference. Store
         ! the new names in EM_FILE_SURR_2
         ALLOCATE( EM_FILE_SURR_2( N_EM_SRC ) )
         DO ISRC = 1,N_EM_SRC
            EM_FILE_SURR_2( ISRC )%LEN = EM_FILE_SURR( ISRC )%LEN
            ALLOCATE( EM_FILE_SURR_2( ISRC )%ARRY( EM_FILE_SURR_2( ISRC )%LEN ) )
            ! Translate Common Aerosol Surrogate Names to CMAQ Particle
            ! Names Using the map provided in AERO_DATA (MAP_AEROtoPTCL).
            DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
               B = EM_FILE_SURR( ISRC )%ARRY( ISUR )
               IA  = INDEX1( B, N_MAP_AEROtoPTCL, MAP_AEROtoPTCL( : )%PTCL )
               IF ( IA .GT. 0 ) THEN
                  EM_FILE_SURR_2( ISRC )%ARRY( ISUR ) = MAP_AEROtoPTCL( IA )%AERO
               ELSE
                  EM_FILE_SURR_2( ISRC )%ARRY( ISUR ) = B
               END IF
            END DO
         END DO

         ! Initialize Structure Used to Keep track of which surrogates
         ! are used and which are ignored
         ALLOCATE( EM_FILE_SURR_USED( N_EM_SRC ) )
         DO ISRC = 1,N_EM_SRC
           N = EM_FILE_SURR( ISRC )%LEN
           EM_FILE_SURR_USED( ISRC )%LEN = N
           ALLOCATE( EM_FILE_SURR_USED( ISRC )%ARRY( N ) )
           EM_FILE_SURR_USED( ISRC )%ARRY( : ) = .FALSE.
         END DO

! Set up Source <-> Size Distribution relationship. This routine
! populates the EM_SRC_MODES structure which tells the logic below which
! modes are present on which sources.
         CALL EM_SD_INIT( SD_NML )

! Process Default Emissions Mapping (if requested in namelist; i.e. 
! DefaultScaling = TRUE) 
         ALLOCATE( EM_SPEC( NISTR0 ) )                ! CMAQ Species Names
         EM_SPEC = ""
         ALLOCATE( EM_SURR( NISTR0,N_EM_SRC ) )       ! Surrogate Name
         EM_SURR = ""
         ALLOCATE( EM_FAC ( NISTR0,N_EM_SRC ) )       ! Scale Factor
         EM_FAC = 0.0
         ALLOCATE( MAP_EMtoDIFF( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoDIFF = 0                             !   to Diffusion Vector
         ALLOCATE( MAP_EMtoSURR( NISTR0,N_EM_SRC ) )  ! Map from Emissions Species 
         MAP_EMtoSURR = 0                             !   to Surrogate Locatino on File
         ALLOCATE( MAP_EMtoAERO( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoAERO = 0                             !   to aerosol table
         ALLOCATE( MAP_EMtoMODE( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoMODE = 0                             !   to aerosol mode
         ALLOCATE( MAP_EMtoNUM ( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoNUM  = 0                             !   to aerosol number
         ALLOCATE( MAP_EMtoSRF ( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoSRF  = 0                             !   to aerosol surface area
         
         ! Find all matches between the transported species list and the
         ! available surrogates from each source. Apply a scale factor
         ! of 1 to these matches. For aerosols, the CMAQ species name
         ! may or may not include the mode suffix (eg. i, j, or k).
         ! Equivalence tests should be performed without a suffix on the
         ! surrogate name and with each suffix added in turn.

         N_EMIS_ISTR = 0
         SPECNAME = ''

         IF ( DefaultScaling ) THEN
            DO IDIFF = 1,N_SPC_DIFF
              DO ISRC = 1,N_EM_SRC

                 ! For Non-Aerosols, Search For Transported Species 
                 ! Name in Emission Surrogate List
                 IDX = 0
                 IF ( .NOT. DIFF_MASK_AERO( IDIFF ) ) THEN
                   IDX = INDEX1( DIFF_SPC( IDIFF ), EM_FILE_SURR_2( ISRC )%LEN, 
     &                           EM_FILE_SURR_2( ISRC )%ARRY )
                 END IF

                 ! For Aerosols, Loop through each surrogate and add
                 ! suffixes
                 JDX = 0
                 IF ( DIFF_MASK_AERO( IDIFF ) ) THEN
                   DO ISUR = 1,EM_FILE_SURR_2( ISRC )%LEN
                     SN = EM_FILE_SURR_2( ISRC )%ARRY( ISUR )
                     DO IM = 1,N_MODE
                       IF ( TRIM( SN )//MODESUFF( IM ) .EQ. 
     &                      DIFF_SPC( IDIFF ) ) THEN 
                          JDX = ISUR
                          JM = IM     ! Save this mode value for mapping below
                       END IF
                     END DO
                   END DO
                 END IF

                 ! Populate Entry in Emissions Instructions if a Match was Found
                 IF ( IDX + JDX .GT. 0 ) THEN
                    ! A Surrogate Matches a Transported Species.
                    ! If both the Species and Surrogate Match a
                    ! Previously Defined Instruction, then add the
                    ! scale factor to that entry under the relevant
                    ! source number.
                    SPECNAME = DIFF_SPC( IDIFF )
                    IF ( N_EMIS_ISTR .NE. 0 ) SPECNAME = EM_SPEC( N_EMIS_ISTR )
                    IF ( DIFF_SPC( IDIFF ) .EQ. SPECNAME .AND. 
     &                   N_EMIS_ISTR .NE. 0 ) THEN
                       ! Update the Surrogate of an Existing Instruction
                       EM_SURR( N_EMIS_ISTR, ISRC ) = EM_FILE_SURR( ISRC )%ARRY( IDX + JDX ) 
                       MAP_EMtoSURR( N_EMIS_ISTR, ISRC ) = IDX + JDX
                       MAP_EMtoSD( N_EMIS_ISTR, ISRC ) =
     &                             EM_FILE_SURR_MODE( ISRC )%ARRY( IDX + JDX )
                       EM_FILE_SURR_USED( ISRC )%ARRY( IDX + JDX ) = .TRUE.
                       EM_FAC ( N_EMIS_ISTR, ISRC ) = 1.0         

                    ELSE
                       ! Create A New Instruction
                       N_EMIS_ISTR = N_EMIS_ISTR + 1
                       EM_SPEC( N_EMIS_ISTR ) = DIFF_SPC( IDIFF )
                       MAP_EMtoDIFF( N_EMIS_ISTR ) = IDIFF


                       EM_SURR( N_EMIS_ISTR, ISRC ) = EM_FILE_SURR( ISRC )%ARRY( IDX + JDX )
                       MAP_EMtoSURR( N_EMIS_ISTR, ISRC ) = IDX + JDX
                       MAP_EMtoSD( N_EMIS_ISTR, ISRC ) =
     &                             EM_FILE_SURR_MODE( ISRC )%ARRY( IDX + JDX )
                       EM_FILE_SURR_USED( ISRC )%ARRY( IDX + JDX ) = .TRUE.
                       IEM = EM_SRC_MODES( ISRC )%SD_REF( MAP_EMtoSD( N_EMIS_ISTR, ISRC )
 
                       ! Link Row to the Aerosol Module. These CMAQ
                       ! species names should already include aerosol
                       ! mode suffixes (like I, J, or K) so search for
                       ! the entire name.
                       AERO_SPLIT = 0.0
                       IF ( DIFF_MASK_AERO( IDIFF ) ) THEN
                         IAERO = INDEX1( EM_SPEC( N_EMIS_ISTR ), N_AEROSPC, 
     &                                   AEROSPC%NAME( JM ) )
                         MAP_EMtoAERO( N_EMIS_ISTR ) = IAERO
                         MAP_EMtoMODE( N_EMIS_ISTR ) = JM
                         MAP_AEROtoEM( IAERO,JM ) = N_EMIS_ISTR

                         AERO_SPLIT = SD_SPLIT( IDIFF, IEM )
                       END IF
 
                       EM_FAC ( N_EMIS_ISTR, ISRC ) = 1.0 * AERO_SPLIT
                    END IF
                 END IF
               END DO
            END DO
         END IF
         
! Insert Emissions Instructions for Aerosol Number and Surface Area,
! if at least one aerosol species is being transported
         IF ( COUNT( DIFF_MASK_AERO ) .GT. 0 ) THEN
            DO IM = 1,N_MODE
               ! Aerosol Number
               EM_SPEC( N_EMIS_ISTR+1 ) = AEROMODE( IM )%NUM_NAME
               EM_FAC ( N_EMIS_ISTR+1, : ) = 1.0
               MAP_NUMtoEM( IM ) = N_EMIS_ISTR+1
               MAP_EMtoNUM( N_EMIS_ISTR+1 ) = IM
               MAP_EMtoDIFF( N_EMIS_ISTR+1 ) = 
     &            INDEX1( EM_SPEC( N_EMIS_ISTR+1 ), N_SPC_DIFF, DIFF_SPC )
               MAP_EMtoSD( N_EMIS_ISTR+1, : ) = 0

               ! Aerosol Surface Area
               EM_SPEC( N_EMIS_ISTR+2 ) = AEROMODE( IM )%SRF_NAME
               EM_FAC ( N_EMIS_ISTR+2, : ) = 1.0
               MAP_SRFtoEM( IM ) = N_EMIS_ISTR+2
               MAP_EMtoSRF( N_EMIS_ISTR+2 ) = IM
               MAP_EMtoDIFF( N_EMIS_ISTR+2 ) = 
     &            INDEX1( EM_SPEC( N_EMIS_ISTR+2 ), N_SPC_DIFF, DIFF_SPC )
               MAP_EMtoSD( N_EMIS_ISTR+2, : ) = 0

               N_EMIS_ISTR = N_EMIS_ISTR + 2
            END DO
         END IF

! Process User-Defined Emissions Scaling Rules. 
         EM_NML%SPEC = ''
         EM_NML%SRC  = ''
         EM_NML%PHASE= ''
         EM_NML%SURR = ''
         EM_NML%FAC  = 0.
         EM_NML%OP   = ''
         READ( NML = EmissionScaling, UNIT = EMCTRL_NML )
         CLOSE( UNIT = EMCTRL_NML )

         ! Add a rule For Removing Fertilizer Ammonia if BiDi is
         ! activated and the user has requested the correction
         IF ( ABFLUX .AND. SUBTRACT_FERTILIZER_NH3 ) THEN
            EM_NML(N_EM_RULE + 1)%SPEC  = 'NH3'
            EM_NML(N_EM_RULE + 1)%SRC   = 'ALL'
            EM_NML(N_EM_RULE + 1)%PHASE = 'GAS'
            EM_NML(N_EM_RULE + 1)%SURR  = 'NH3_FERT'
            EM_NML(N_EM_RULE + 1)%FAC   = -1.0
            EM_NML(N_EM_RULE + 1)%OP    = 'a'
         END IF

         ! Allocate Rule->Instruction Transform Masks
         ALLOCATE( EM_SRC_RULE  ( N_EM_SRC   ) )
         ALLOCATE( EM_SPEC_RULE ( N_SPC_DIFF ) )
         ALLOCATE( EM_SURR_RULE ( N_EM_SRC ) )
         ALLOCATE( EM_PHASE_RULE( N_EM_SRC ) )
         DO ISRC = 1,N_EM_SRC
           N = EM_FILE_SURR( ISRC )%LEN
           EM_SURR_RULE( ISRC )%LEN = N
           ALLOCATE( EM_SURR_RULE( ISRC )%ARRY( N ) )

           N = EM_SRC_MODES( ISRC )%LEN
           EM_PHASE_RULE( ISRC )%LEN = N
           ALLOCATE( EM_PHASE_RULE( ISRC )%ARRY( N ) )
         END DO
 
         ! Loop Through Emission Rules, Test for Fidelity, expand if necessary
         ! and Apply them to the instruction set that currently exists.
         DO IRULE = 1,N_EM_RULE 
            ! Exit this loop if the rule is blank
            IF ( EM_NML( IRULE )%SPEC .EQ. '' ) EXIT 

            ! Expand Rule To Individual Instructions. If the CMAQ
            ! Species, Source Label, and Surrogate are all single
            ! components, then there will just be one instruction. If
            ! any of them equal 'All' the number of instructions will
            ! grow correspondingly.

            !------   ------   ------   ------   ------   ------   -----
            ! First error check and expand the source field
            ! This subroutine returns a logical vector, EM_SRC_RULE,
            ! which identifies which sources are affected by this rule.
            CALL INTERPRET_EM_RULE_SRC( EM_NML( IRULE )%SRC, EM_SRC_RULE )

            !------   ------   ------   ------   ------   ----  
            ! Now error check and expand the surrogate field
            CALL UPCASE( EM_NML( IRULE )%SURR )

            !Initialize Surrogate Array for every Source
            DO ISRC = 1,N_EM_SRC
              EM_SURR_RULE( ISRC )%ARRY = .FALSE.
            END DO

            LERROR = .TRUE.
            DO ISRC = 1,N_EM_SRC 
              ! Skip this source if it is not identified
              IF ( .NOT. EM_SRC_RULE( ISRC ) ) CYCLE 

              IF ( EM_NML( IRULE )%SURR .EQ. 'ALL' ) THEN
                 ! Expand the Rule to Apply to All Surrogates
                 EM_SURR_RULE( ISRC )%ARRY = .TRUE.
                 LERROR = .FALSE.
              ELSE
                 ! Find the Specific Surrogate this Rule Identifies
                 IDX = INDEX1( EM_NML( IRULE )%SURR, EM_FILE_SURR( ISRC )%LEN, 
     &                         EM_FILE_SURR( ISRC )%ARRY )
                 IF ( IDX .NE. 0 ) THEN
                   EM_SURR_RULE( ISRC )%ARRY( IDX ) = .TRUE.
                   LERROR = .FALSE.
                 END IF
              END IF
            END DO
            
            IF ( LERROR ) THEN
              IF ( .NOT. EMISCHK ) THEN
                 WRITE( LOGDEV, '(/,A,/,2A,3(/,A))') 
     &                  '****************************WARNING**************************:',
     &                  'The emission surrogate ',TRIM( EM_NML( IRULE )%SURR ),
     &                  ' was not found in emissions sources but the CTM_EMISCHK ',
     &                  'environment variable set to False so simulation will proceed.',
     &                  '**************************************************************'
                 WRITE( LOGDEV, * )
                 WRITE( LOGDEV, * )
                 XMSG = 'For optimal predictions, all surrogates '
     &                //'should be found on at least one source.'
                 CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              ELSE     
                 WRITE( LOGDEV, '(A,/,2A,/,8(/,A))') 
     &                  '*****************************ERROR***************************************:',
     &                  'The emission surrogate ',TRIM( EM_NML( IRULE )%SURR ),
     &                  ' is not found in any of the emission sources.',
     &                  'Use one of the below options to continue.', 
     &                  '1) Change or remove the missing GC, NR and TR emissions surrogates from the',
     &                  'namelists where they are defined.', 
     &                  'or',
     &                  '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &                  'if model predictions are acceptable without using the above emissions.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
                 XMSG = 'Species with the missing surrogates ' 
     &                //'must have a surrogate found in at least one source.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF
            END IF
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check and Expand the CMAQ Species Field
            CALL UPCASE( EM_NML( IRULE )%SPEC )

            ! Initialize CMAQ Species Array
            EM_SPEC_RULE = .FALSE.

            IF ( EM_NML( IRULE )%SPEC .EQ. 'ALL' ) THEN
               ! Expand the Rule to Apply to All Species
                EM_SPEC_RULE = .TRUE.
            ELSE
               ! Find the Specific Species this Rule Identifies
               IDX = INDEX1( EM_NML( IRULE )%SPEC, N_SPC_DIFF, DIFF_SPC )
               JDX = INDEX1( EM_NML( IRULE )%SPEC, N_AEROSPC,  AEROSPC( : )%BULKNAME )
               IF ( IDX .NE. 0 ) THEN
                 EM_SPEC_RULE( IDX ) = .TRUE.
               ELSE IF ( JDX .NE. 0 ) THEN
                  ! This is an aerosol species, and it is being
                  ! identified with a bulk name (no mode suffix). 
                  ! We need to allow for all possible DIFF_SPC with
                  ! all used suffixes
                  SN = EM_NML( IRULE )%SPEC
                  DO IM = 1,N_MODE
                    KDX = INDEX1( TRIM( SN )//MODESUFF( IM ), N_SPC_DIFF, DIFF_SPC )
                    IF ( KDX .NE. 0 ) EM_SPC_RULE( KDX ) = .TRUE.
                  END DO
               ELSE
                 WRITE( LOGDEV, * )
                 WRITE( LOGDEV, '(3A,/,A,/,A,/,A)' ),
     &               'Species ',TRIM(EM_NML( IRULE )%SPEC),' was used in the Emissions',
     &               ' Control Instructions Namelist but it is not a CMAQ ',
     &               'transported species. Please add it to one of the ',
     &               'input chemical namelists (ie. GC, AE, etc)'
                 XMSG = 'Error in Emissions Map Processing.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF 
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check and Expand the Phase Field
            CALL UPCASE( EM_NML( IRULE )%PHASE )

            !Initialize Surrogate Array for every Source
            DO ISRC = 1,N_EM_SRC
              EM_PHASE_RULE( ISRC )%ARRY = .FALSE.
            END DO

            LERROR = .TRUE.
            DO ISRC = 1,N_EM_SRC 
              ! Skip this source if it is not identified
              IF ( .NOT. EM_SRC_RULE( ISRC ) ) CYCLE 

              IF ( EM_NML( IRULE )%PHASE .EQ. 'ALL' ) THEN
                 ! Expand the Rule to Apply to All Phases and Modes
                 EM_PHASE_RULE( ISRC )%ARRY = .TRUE.
                 LERROR = .FALSE.
              ELSE
                 ! Find the Specific Surrogate this Rule Identifies
                 IDX = INDEX1( EM_NML( IRULE )%PHASE, EM_SRC_MODES( ISRC )%LEN, 
     &                         EM_SRC_MODES( ISRC )%SD_NAME )
                 IF ( IDX .NE. 0 ) THEN
                   EM_PHASE_RULE( ISRC )%ARRY( IDX ) = .TRUE.
                   LERROR = .FALSE.
                 END IF
              END IF
            END DO
            
            IF ( LERROR ) THEN
                 WRITE( LOGDEV, '(A,/,2A,/,A,/,A,I3,A1,/,A)') 
     &                  '*****************************ERROR***************************************:',
     &                  'The phase oe mode ',TRIM( EM_NML( IRULE )%PHASE ),
     &                  ' is not found in any of the emission sources you''re requesting for ',
     &                  'emission rule ',IRULE,'.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
                 XMSG = 'Species with the missing surrogates ' 
     &                //'must have a surrogate found in at least one source.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
 
            !------   ------   ------   ------   ------
            ! Check the Operation Identifier for errors
            IF ( EM_NML( IRULE )%OP .NE. 'a' .AND.
     &           EM_NML( IRULE )%OP .NE. 'm' .AND.
     &           EM_NML( IRULE )%OP .NE. 'o'       ) THEN
               WRITE( LOGDEV, * )
               WRITE( XMSG, '(/,A,A,A,I3,A,A,A)' ),
     &             'The Emissions Operator (',EM_NML( IRULE )%OP,
     &             ') applied for Rule ',IRULE,' in the Emissions Control ',
     &             'Namelist does not match any of the allowed values (a, m, or o)',
     &             '. Please check the your emissions control inputs.'
               WRITE( LOGDEV, * )XMSG
               XMSG = 'Error in Emissions Map Processing.'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            
            !------   ------   ------   ------   ------   ------ ------
            ! Order the indivdual commands in this rule into one
            ! vector of instructions. The operator and scale factor for
            ! each of these commands will be uniform because they apply
            ! to the entire rule
            N_COMMANDS = 0
            DO IDIFF = 1,N_SPC_DIFF
            IF ( EM_SPEC_RULE( IDIFF ) ) THEN
              DO ISRC = 1,N_EM_SRC
              IF ( EM_SRC_RULE( ISRC ) ) THEN
                DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN 
                IF ( EM_SURR_RULE( ISRC )%ARRY( ISUR ) ) THEN
                  DO ISD = 1,EM_SRC_MODES( ISRC )%LEN
                  IF ( EM_PHASE_RULE( ISRC )%ARRY( ISD ) ) THEN
                    N_COMMANDS = N_COMMANDS + 1
                    RULE_ISTR_IDIFF( N_COMMANDS ) = IDIFF
                    RULE_ISTR_ISRC ( N_COMMANDS ) = ISRC
                    RULE_ISTR_ISUR ( N_COMMANDS ) = ISUR
                    RULE_ISTR_SPEC ( N_COMMANDS ) = DIFF_SPC( IDIFF )
                    RULE_ISTR_SURR ( N_COMMANDS ) = 
     &                   EM_FILE_SURR( ISRC )%ARRY( ISUR )
                    RULE_ISTR_PHASE( N_COMMANDS ) = 
     &                   EM_SRC_MODES( ISRC )%ARRY( ISUR )
                END IF
                END DO
              END IF
              END DO
            END IF
            END DO
            
            !------   ------   ------   ------   ------   ------
            ! Modify the Emissions Instruction Set Based on this Rule
            IF ( EM_NML( IRULE )%OP .EQ. 'a' ) THEN
               ! Add this rule to existing instructions
               DO IC = 1,N_COMMANDS
                  ! This entry needs to be created, but first we need
                  ! to check whether to add it as a new row or add it
                  ! to a previous row. We can add to a previous row
                  ! if the CMAQ species matches exactly but this
                  ! surrogate is not present
                  LFOUND = .FALSE.
                  ! Look For a suitable previous instruction to add to.
                  IF ( N_EMIS_ISTR .GT. 0 ) THEN
                  DO ISTR = 1,N_EMIS_ISTR
                    IF ( EM_SPEC( ISTR ) .EQ. RULE_ISTR_SPEC( IC ) .AND.
     &                   EM_SURR( ISTR,RULE_ISTR_ISRC( IC ) ) .EQ. '' ) THEN
                      ! Add This Command to Instruction number ISTR
                      ISRC = RULE_ISTR_ISRC( IC )
                      EM_SURR( N_EMIS_ISTR, ISRC ) = RULE_ISTR_SURR( IC )
                      EM_FAC ( N_EMIS_ISTR, ISRC ) = EM_NML( IRULE )%FAC
                      MAP_EMtoSURR( N_EMIS_ISTR, ISRC ) = RULE_ISTR_ISUR( IC )
                      EM_FILE_SURR_USED( ISRC )%ARRY( ISUR ) = .TRUE.
                      MAP_EMtoSD( N_EMIS_ISTR, ISRC ) = RULE_ISTR_PHASE( IC )
                      LFOUND = .TRUE.
                    END IF
                  END DO
                  END IF

                  ! If no suitable instruction was found to add to, add a new
                  ! instruction. This means either there was no previous
                  ! instruction with the same CMAQ species or there was
                  ! an instruction with this CMAQ species but it already
                  ! had a surrogate and scale factor associated with this source.
                  IF ( .NOT. LFOUND ) THEN
                    N_EMIS_ISTR = N_EMIS_ISTR + 1

                    EM_SPEC( N_EMIS_ISTR ) = RULE_ISTR_SPEC( IC )
                    MAP_EMtoDIFF( N_EMIS_ISTR ) = RULE_ISTR_IDIFF( IC )
                  
                    ISRC = RULE_ISTR_ISRC( IC )
                    EM_SURR( N_EMIS_ISTR, ISRC ) = RULE_ISTR_SURR( IC )
                    MAP_EMtoSURR( N_EMIS_ISTR, ISRC ) = RULE_ISTR_ISUR( IC )
                    EM_FILE_SURR_USED( ISRC )%ARRY( ISUR ) = .TRUE.
                    MAP_EMtoSD( N_EMIS_ISTR, ISRC ) = RULE_ISTR_PHASE( IC )
                    IEM = EM_SRC_MODES( ISRC )%SD_REF( MAP_EMtoSD( N_EMIS_ISTR, ISRC )
                  
                    ! Link this row to the Aerosol Module.
                    AERO_SPLIT = 0.0
                    IF ( DIFF_MASK_AERO( RULE_ISTR_IDIFF( IC ) ) ) THEN
                      DO IAERO = 1,N_AEROSPC
                        JM = INDEX1( EM_SPEC( N_EMIS_ISTR ), N_MODE, AEROSPC( IAERO )%NAME( : ) )
                        IF ( JM .GT. 0 ) THEN
                          MAP_EMtoAERO( N_EMIS_ISTR ) = IAERO
                          MAP_EMtoMODE( N_EMIS_ISTR ) = JM
                          MAP_AEROtoEM( IAERO,JM ) = N_EMIS_ISTR
                          AERO_SPLIT = SD_SPLIT( RULE_ISTR_IDIFF( IC ), IEM )
                        END IF
                      END DO
                    END IF
                    
                    EM_FAC ( N_EMIS_ISTR, ISRC ) = EM_NML( IRULE )%FAC * AERO_SPLIT

                  END IF
               END DO
            ELSE
               ! Modify Existing Instructions
               DO IC = 1,N_COMMANDS
                 ! Loop through existing instructions and find matches
                 IF ( N_EMIS_ISTR .GT. 0 ) THEN
                 DO ISTR = 1,N_EMIS_ISTR
                 IF ( EM_SPEC( ISTR ) .EQ. RULE_ISTR_SPEC( IC ) ) THEN
                   DO ISRC = 1,N_EM_SRC
                   DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
                     IF ( EM_SURR( ISTR, ISRC ) .EQ. RULE_ISTR_SURR( IC ) .AND.
                          MAP_EMtoSD( ISTR,ISRC ) .EQ. RULE_ISTR_PHASE( IC ) ) THEN
                       ! Multiply the Scale Factor if Desired
                       IF ( EM_NML( IRULE )%OP .EQ. 'm' ) 
     &                      EM_FAC ( ISTR, ISRC ) = 
     &                         EM_FAC( ISTR, ISRC ) *
     &                         EM_NML( IRULE )%FAC
                       ! Overwrite the Scale Factor if Desired
                       IF ( EM_NML( IRULE )%OP .EQ. 'o' ) 
     &                      EM_FAC ( ISTR, ISRC ) = 
     &                         EM_NML( IRULE )%FAC
                     END IF
                   END DO
                   END DO
                 END IF
                 END DO
                 ELSE
                   WRITE( LOGDEV, '(5(/,A))' ),
     &              'ATTENTION: The emissions control file is ',
     &              'attempting to modify an existing instruction, but ',
     &              'there are no compatible existing instructions. ',
     &              'Please check the configuration of the emission ',
     &              'control file.'
                 END IF
               END DO
            END IF  ! Operator
         END DO !IRULE
! Warn the User if there are no emissions instructions provided        
         IF ( N_EMIS_ISTR .LE. 0 ) THEN
            XMSG = 'There are no emissions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

C Print a message for each emissions source surrogate that is not used.
         WRITE( LOGDEV, '(/,A,/)' ) 'Checking for unused Emissions Surrogates'

         LERROR = .FALSE.
         DO ISRC = 1,N_EM_SRC
             N_UNUSED = COUNT( .NOT. EM_FILE_SURR_USED( ISRC )%ARRY( : ) )
             WRITE( LOGDEV, '(A,A,I2,A)' ), TRIM(EM_FILE_DESC( ISRC )),': ',
     &              N_UNUSED,' unused surrogates.'
             IF ( N_UNUSED .GT. 0 ) THEN
                 LERROR = .TRUE.

                 DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
                   IF ( .NOT. EM_FILE_SURR_USED( ISRC )%ARRY( ISUR ) ) 
     &               WRITE( LOGDEV, '(A)'), EM_FILE_SURR( ISRC )%ARRY( ISUR )
                 END DO
                 WRITE ( LOGDEV, * )
             END IF
         END DO
         IF ( LERROR ) WRITE( LOGDEV, '(A,A,A,A)' )'ATTENTION: Some Emissions Surrogates',
     &       ' are unused by your current emission control configuration. ',
     &       'You may want to include these emissions if they are relevant ',
     &       'to your application.'

! Resize Important Arrays
         MAP_EMtoDIFF = MAP_EMtoDIFF( 1:N_EMIS_ISTR )


! An Emissions Scaling Map Now Exists as a 2D Array (NSPECIES x NSOURCES).
! For every element, there is an associated surrogate and scale factor
! to be applied. For the aerosols, the scale factor will be modified
! later in order to split the mass into the appropriate modes.


! Print out the Tables of CMAQ Emissions Instructions organized by each 
! emission source and then by CMAQ internal species.
         WRITE( LOGDEV, '(/,A)' ),'|> EMISSIONS SCALING DIAGNOSTIC:'
         WRITE( LOGDEV, '(A)'   ),'=================================='

         DO ISRC = 1,N_EM_SRC
           WRITE( LOGDEV,'(/,3x,A,A,A,A)' ),'Source: ',TRIM(EM_FILE_DESC( ISRC )),
     &            '; Label: ',TRIM(EM_FILE_LAB( ISRC ) )
           WRITE( LOGDEV, '(6x,A,3x,A,3x,A,3x,A)' ),
     &            'CMAQ Species ','Surrogate','Scale Factor','Phase/Mode'

           DO IDIFF = 1,N_SPC_DIFF
             DO ISTR = 1,N_EMIS_ISTR
               IF ( EM_SPEC( ISTR ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &              EM_SURR( ISTR,ISRC ) .NE. '' )
     &           WRITE( LOGDEV, '(7x,A,3x,A,F7.4)' ),EM_SPEC( ISTR ),
     &                  EM_SURR( ISTR, ISRC ), EM_FAC_BULK( ISTR, ISRC ),
     &                  EM_SRC_MODES( ISRC )%SD_NAME
             END DO
           END DO
         END DO
 

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

         INTEGER IOS, LOGDEV, ISRC, N

         CHARACTER( 16 )  :: CTM_BIOGEMIS = 'CTM_BIOGEMIS'    ! Let model proceed even if
         CHARACTER( 16 )  :: CTM_LTNG_NO = 'CTM_LTNG_NO'    ! Let model proceed even if
         CHARACTER( 16 )  :: PNAME = 'EM_FILE_INIT'
         CHARACTER( 80 )  :: VARDESC
         CHARACTER( 16 )  :: VLAB
         CHARACTER( 16 )  :: VNAME
         INTEGER          :: STATUS
         CHARACTER( 200 ) :: XMSG
         LOGICAL :: SUCCESS

         LOGDEV = INIT3()
         SUCCESS = .TRUE.

C Retrieve the number of each type of emission file             
         N_FILE_GR = ENVINT( 'N_EMIS_GR',
     &                     'Number of Gridded Area Emission Files',1, IOS )
         NPTGRPS   = ENVINT( 'N_EMIS_PT',
     &                     'Number of file groups',0, IOS )
         N_FILE_TR = ENVINT( 'N_EMIS_TR',
     &                     'Number of Tracer Emission Files',0, IOS )
         N_EM_SRC = N_FILE_GR + NPTGRPS + N_FILE_TR
 
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
         Allocate( MAP_PTtoISRC( NPTGRPS ) )
         Allocate( EM_FILE_SURR( N_EM_SRC ) )
         Allocate( EM_GRID_LAYS( N_EM_SRC ) )
         Allocate( CONVEM( N_EM_SRC ) )
         Allocate( CONVEM_PM_MASS( N_EM_SRC ) )
         Allocate( EMIS_GAS_UNITS( N_EM_SRC ) )
         Allocate( EMIS_AER_UNITS( N_EM_SRC ) )
 
C Assign Attributes to Emission File Records. Other records will be 
C populated in individual subroutines. For example, opemis and
C stkemis_init.
         ISRC = 0
         EMIS_GAS_UNITS = "NONE"
         EMIS_AER_UNITS = "NONE"

         ! Gridded Emission Files
         EM_FILE_TYPE( 1:N_FILE_GR ) = 'GRID'
         DO N = 1, N_FILE_GR
            ISRC = ISRC + 1
            IGSRC( ISRC ) = N
            WRITE( EM_FILE_DESC( ISRC ), '(A,I2)' ),
     &         'Gridded Area Emissions File ', N
             
            WRITE( VLAB,'( "GR_EMIS_LAB_",I2.2 )' ) N
            CALL ENVSTR( VLAB, 'Gridded Emission File Label',VLAB,
     &                   EM_FILE_LAB( ISRC ), IOS )

            WRITE( EM_FILE_NAME( ISRC ),'( "GR_EMIS_",I2.2 )' ) N
            CONVEM( ISRC ) = 1.0
            CONVEM_PM_MASS ( ISRC ) = 1.0
         END DO
         
         ! In-Line Point Source Files
         IF ( NPTGRPS .GT. 0 ) THEN 
            EM_FILE_TYPE( ISRC+1:ISRC+NPTGRPS ) = 'POINT'
            DO N = 1, NPTGRPS
               ISRC = ISRC + 1
               IPSRC( ISRC ) = N
               MAP_PTtoISRC( N ) = ISRC
               WRITE( EM_FILE_DESC( ISRC ), '(A,I2)' ),
     &         'Point Emissions File ', IPSRC( ISRC )
              
               WRITE( VLAB,'( "STK_EMIS_LAB_",I2.2 )' ) N
               CALL ENVSTR( VLAB, 'Stack Emission File Label',VLAB,
     &                      EM_FILE_LAB( ISRC ), IOS )
 
               WRITE( EM_FILE_NAME( ISRC ),'( "STK_EMIS_",I2.2 )' ) N
               CONVEM( ISRC ) = 1.0
               CONVEM_PM_MASS ( ISRC ) = 1.0
            END DO
         END IF
         
         ! Tracer Emissions
         IF ( N_FILE_TR .GT. 0 ) THEN 
            EM_TRAC = .TRUE.
            EM_FILE_TYPE( ISRC+1:ISRC+N_FILE_TR ) = 'TRAC'
            DO N = 1, N_FILE_TR
               ISRC = ISRC + 1
               ITSRC( ISRC ) = N
               WRITE( EM_FILE_DESC( ISRC ), '(A,I2)' ),
     &           'Gridded Tracer Emissions File ', N

               WRITE( VLAB,'( "TR_EMIS_LAB_",I2.2 )' ) N
               CALL ENVSTR( VLAB, 'Tracer Emission File Label',VLAB,
     &                      EM_FILE_LAB( ISRC ), IOS )

               WRITE( EM_FILE_NAME( ISRC ),'( "TR_EMIS_",I2.2 )' ) N
               CONVEM( ISRC ) = 1.0
               CONVEM_PM_MASS ( ISRC ) = 1.0
            END DO
         ELSE
            EM_TRAC = .FALSE.
         END IF
         
         ! Online Biogenic Emissions (BEIS)
         IF ( BIOGEMIS ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'BIOG'
             EM_FILE_LAB ( ISRC ) = 'BIOG'
             EM_FILE_DESC( ISRC ) = 'Biogenic Emissions'
             CONVEM( ISRC ) = 1.0E-3
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_GAS_UNITS( ISRC ) = "Unknown"

             IBIOSRC = ISRC
         END IF

         ! Online Marine Gas Emissions
         IF ( MGEM ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'MGEM'
             EM_FILE_LAB ( ISRC ) = 'MGEM'
             EM_FILE_DESC( ISRC ) = 'Marine Gas Emissions'
             CONVEM( ISRC ) = 1.0E-3
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_GAS_UNITS( ISRC ) = "Unknown"

             IMGSRC = ISRC
         END IF

         ! Online Lightning NO Emissions
         IF ( LTNG_NO ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'LTNG'
             EM_FILE_LAB ( ISRC ) = 'LTNG'
             EM_FILE_DESC( ISRC ) = 'Lightning NO Emissions'
             CONVEM( ISRC ) = 1.0E-3
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_GAS_UNITS( ISRC ) = "Unknown"

             ILTSRC = ISRC
         END IF

         ! Sea Spray Aerosol Emissions
         ISRC = ISRC + 1
         EM_FILE_TYPE( ISRC ) = 'ASEA'
         EM_FILE_LAB ( ISRC ) = 'SEASPRAY'
         EM_FILE_DESC( ISRC ) = 'Sea Spray Aerosol Emissions'
         CONVEM( ISRC ) = 1.0E-3
         CONVEM_PM_MASS ( ISRC ) = 1.0
         EMIS_AER_UNITS( ISRC ) = "g/m3/s"
         ISEASRC = ISRC

         ! Wind-Blown Dust Emissions
         IF ( WB_DUST ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'DUST'
             EM_FILE_LAB ( ISRC ) = 'WBDUST'
             EM_FILE_DESC( ISRC ) = 'Wind-Blown Dust Emissions'
             CONVEM( ISRC ) = 1.0E-3
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_AER_UNITS( ISRC ) = "g/m3/s"
             IDUSTSRC = ISRC
         END IF
          


         END SUBROUTINE EM_FILE_INIT    

C-----------------------------------------------------------------------
         SUBROUTINE INTERPRET_EM_RULE_SRC( RULE_SRC, EM_SRC_RULE )

! This subroutine analyzes the part of a rule identifying which sources
! to affect and searches for the individual source, or, if the rule uses
! the 'ALL' keyword, this routine identifies every source as impacted.

      IMPLICIT NONE

      CHARACTER(32), INTENT( INOUT ) :: RULE_SRC
      LOGICAL, INTENT( INOUT )       :: EM_SRC_RULE( N_EM_SRC )

      INTEGER ISRC

      ! Initialize the output vector
      EM_SRC_RULE = .FALSE.
      
      ! First Capitalize the Rule so it is easier to error-check
      CALL UPCASE( EM_NML( IRULE )%SRC )

      IF ( EM_NML( IRULE )%SRC .EQ. 'ALL' ) THEN
         ! Special Case Where the Source Identifier in the rule equals
         ! equals all 'ALL'. Expand the Rule to Apply to All Sources
         ISRC = 0
         IF ( N_FILE_GR .GT. 0 ) THEN
           EM_SRC_RULE( ISRC+1:N_FILE_GR ) = .TRUE.
           ISRC = ISRC + N_FILE_GR
         END IF

         IF ( NPTGRPS .GT. 0 ) THEN
           EM_SRC_RULE( ISRC+1:ISRC+NPTGRPS ) = .TRUE.
           ISRC = ISRC + NPTGRPS
         END IF
         IF ( N_FILE_TR .GT. 0 ) EM_SRC_RULE( ISRC+1:ISRC+N_FILE_TR ) = .TRUE.
         IF ( IBIOSRC .GT.0 .AND. .NOT. Guard_BiogenicVOC   ) EM_SRC_RULE( IBIOSRC  ) = .TRUE.
         IF ( IMGSRC  .GT.0 .AND. .NOT. Guard_MarineGas     ) EM_SRC_RULE( IMGSRC   ) = .TRUE.
         IF ( ILTSRC  .GT.0 .AND. .NOT. Guard_LightningNO   ) EM_SRC_RULE( ILTSRC   ) = .TRUE.
         IF ( ISEASRC .GT.0 .AND. .NOT. Guard_SeaSpray      ) EM_SRC_RULE( ISEASRC  ) = .TRUE.
         IF ( IDUSTSRC.GT.0 .AND. .NOT. Guard_WindBlownDust ) EM_SRC_RULE( IDUSTSRC ) = .TRUE.

      ELSE
         ! Find the Specific Source this Rule Identifies
         IDX = INDEX1( EM_NML( IRULE )%SRC, N_EM_SRC, EM_FILE_LAB ) 
         IF ( IDX .NE. 0 ) THEN
           EM_SRC_RULE( IDX ) = .TRUE.
         ELSE
           WRITE( LOGDEV, * )
           WRITE( LOGDEV, '(/,3A,I3,/,2A,/,2A)' ),
     &         'The Emissions Source Label (',TRIM(EM_NML( IRULE )%SRC),
     &         ') applied for Rule ',IRULE,' in the Emissions Control ',
     &         'Namelist does not match any of the emissions source ',
     &         'labels provided to CMAQ. Please check the runscript against ',
     &         'your emissions control inputs.'
           XMSG = 'Error in Emissions Map Processing.'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
 
      END SUBROUTINE INTERPRET_EM_RULE_SRC

      END MODULE EMIS_DEFN
