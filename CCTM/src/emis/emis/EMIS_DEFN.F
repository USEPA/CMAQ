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
      USE UTILIO_DEFN

      IMPLICIT NONE

      PUBLIC EMIS_INIT, GET_EMIS, EM_GRID_LAYS
      
      PRIVATE
      
      LOGICAL,              SAVE :: EMISCHK ! Let model proceed even if Emis Surrogates 
                                            ! are not properly defined
     
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
         IF ( N_EM_SRC .EQ. 0 ) THEN 
            XMSG = 'No Emissions Sources Have Been Selected.'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .TRUE.; RETURN
         END IF

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
         ALLOCATE ( VDEMIS_CONV( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_CONV memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_SCLD( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_SCLD memory allocation failed'
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
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CGRID )

C NEW APPROACH:
C Apply NML factors to all *input* emissions and inline plume rise, aero emis
C biog, SeaSalt, Dust, and Lightning NO
C GET_AERO_EMIS has to apply factors - don`t do it at this level.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE SSEMIS
         USE DUST_EMIS
         USE MGEMIS              ! marine gas emissions
         USE PT3D_DEFN           ! plume rise emissions
         USE LTNG_DEFN           ! lightning NO emissions
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME
         USE AERO_DATA, ONLY : AERONUM_MAP, AEROSRF_MAP, DUSTOUTM

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL, POINTER :: CGRID( :,:,:,: )

C Local Variables:
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V, ISTR, ISRC ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables
         

         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '

C-----------------------------------------------------------------------


C Initialize Emissions Array
         VDEMIS = 0.0
         VDEMIS_DIFF = 0.0


C Retrieve Emissions from All Source Types
         IF ( N_EM_SRC .GT. 0 ) THEN
         DO ISRC = 1,N_EM_SRC
           VDEMIS_GR = 0.0

           SELECT CASE ( EM_FILE_ITYPE( ISRC ) )
           ! Each Case Returns Emission Rates for gases in kmol/s and
           ! for aerosols in kg/s

             ! Retrieve Gridded Emissions
             CASE ( 1 )
               CALL GRIDEMIS ( JDATE, JTIME, EM_FILE_NAME( ISRC ), 
     &                         EM_GRID_LAYS( ISRC ), ISRC )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 .AND. 
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_GR( ISTR,:,:,: ) = CONVEM_GAS( ISRC ) * VDEMIS_GR( ISTR,:,:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC )
               CALL EMISS_SIZE_DIST( VDEMIS_SCLD, ISRC )
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

             ! Retrieve Point Source Emissions
             CASE ( 2 )
               CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, EM_SURR( :,ISRC ), ISRC )
               VDEMIS_GR = VDEMIS_PT
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 .AND.
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_GR( ISTR,:,:,: ) = CONVEM_GAS( ISRC ) * VDEMIS_GR( ISTR,:,:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC )
               CALL EMISS_SIZE_DIST( VDEMIS_SCLD, ISRC )
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

             ! Retrieve Biogenic Emissions from BEIS
             CASE ( 4 )
               CALL GET_BEIS ( JDATE, JTIME, TSTEP )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 .AND.
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_GR( ISTR,1,:,: ) = CONVEM_GAS( ISRC ) *
     &                         VDEMIS_BI( MAP_EMtoSURR( ISTR,ISRC ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC )
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

             ! Retrieve Marine Gas Emissions
             CASE ( 5 )
               CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 .AND. 
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_GR( ISTR,1,:,: ) = CONVEM_GAS( ISRC ) *
     &                          VDEMIS_MG( MAP_EMtoSURR( ISTR,ISRC ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC )
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

             ! Retrieve Lightning NO Emissions
             CASE ( 6 )
               CALL GET_LTNG ( JDATE, JTIME, TSTEP )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 .AND. 
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_GR( ISTR,:,:,: ) = VDEMIS_LT( :,:,: ) * CONVEM_GAS( ISRC )
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC )
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

             ! Retrieve Sea Spray Aerosol Emissions 
             CASE ( 7 )
               CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( 1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS_GR( ISTR,1,:,: ) = 
     &                  SSOUTM( MAP_EMtoSURR( ISTR,ISRC ),MAP_EMtoMODE( ISTR ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC ) 
               CALL EMISS_SIZE_DIST( VDEMIS_SCLD, ISRC )
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

             ! Retrieve Wind-Blown Dust Emissions 
             CASE ( 8 )
               CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRC ) .GT. 0 )
                  VDEMIS_GR( ISTR,1,:,: ) = 
     &                  DUSTOUTM( MAP_EMtoSURR( ISTR,ISRC ),MAP_EMtoMODE( ISTR ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_GR, ISRC ) 
               CALL EMISS_SIZE_DIST( VDEMIS_SCLD, ISRC ) 
               CALL EMISS_CONV_DIFF()
               VDEMIS_DIFF( :,:,:,: ) = VDEMIS_DIFF( :,:,:,: ) + VDEMIS_CONV( :,:,:,: )

           END SELECT

         END DO  
         END IF

         RETURN

         END SUBROUTINE GET_EMIS

C-----------------------------------------------------------------------
         SUBROUTINE GRIDEMIS ( JDATE, JTIME, EMIS_FNAME, LAYS, 
     &                         FSRC )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: FSRC
         CHARACTER( 100 ), INTENT( IN ) :: EMIS_FNAME

C Local Variables:
         INTEGER   C, R, L, N, S, V, ISTR ! loop induction variables
         INTEGER   S_STRT, S_END, LAYS

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

          DO ISTR = 1, N_EMIS_ISTR 
            VNAME = EM_SURR( ISTR, FSRC )
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
                     VDEMIS_GR( ISTR,L,C,R ) = BUFF( C,R,L )
                  END DO
               END DO
            END DO

           END DO   ! ISPC

           RETURN

      END SUBROUTINE GRIDEMIS

C-----------------------------------------------------------------------
      SUBROUTINE EMISS_SCALING( VDEMISRD, ISRC )

C     Apply region-dependent scaling of emissions rules.
C-----------------------------------------------------------------------
          
      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: ISRC
      REAL, INTENT( IN )    :: VDEMISRD( :,:,:,: )

      INTEGER ISTR, IRGN, IFAC, L, NL


      VDEMIS_SCLD = 0.0
      NL = SIZE( VDEMISRD,2 )
      
      ! Loop through instructions and sources
      DO ISTR = 1,N_EMIS_ISTR
        ! Do Not bother with this row if there is no surrogate to map
        IF ( MAP_EMtoSURR( ISTR,ISRC ) .EQ. 0 ) CYCLE

        ! Loop through Instructions Based on Region-Dependent Scaling
        DO IFAC = 1,EM_FAC_ST( ISTR,ISRC )%LEN
              !Apply scaling using EM_FAC_ST and VDEMIS_RGN
              SELECT CASE ( EM_FAC_ST( ISTR,ISRC )%OP( IFAC ) )
              CASE ( 1 )  ! Add scaled emissions
                  IF ( EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) .EQ. 1 ) THEN
                     ! Add Emissions for the Entire Domain
                     VDEMIS_SCLD( ISTR,:,:,: ) = VDEMIS_SCLD( ISTR,:,:,: ) +
     &                   VDEMISRD( ISTR,:,:,: ) * EM_FAC_ST( ISTR,ISRC )%FAC( IFAC )
                  ELSE
                     ! Add Emissions just for a subdomain
                     IRGN = EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) 
                     DO L = 1,NL
                       VDEMIS_SCLD( ISTR,L,:,: ) = VDEMIS_SCLD( ISTR,L,:,: ) +
     &                   VDEMISRD( ISTR,L,:,: ) * EM_FAC_ST( ISTR,ISRC )%FAC( IFAC ) *
     &                   EM_REG_FAC( :,:,IRGN )
                     END DO
                  END IF
              CASE ( 2 )  ! Modify scaled emissions
                  IF ( EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) .EQ. 1 ) THEN
                     ! Scale Emissions for the Entire Domain
                     VDEMIS_SCLD( ISTR,:,:,: ) = VDEMIS_SCLD( ISTR,:,:,: )
     &                   * EM_FAC_ST( ISTR,ISRC )%FAC( IFAC )
                  ELSE
                     ! Scale Emissions just for a subdomain
                     IRGN = EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) 
                     DO L = 1,NL
                       VDEMIS_SCLD( ISTR,L,:,: ) = VDEMIS_SCLD( ISTR,L,:,: ) * 
     &                     (1.0 - EM_REG_FAC( :,:,IRGN ) ) +
     &                   VDEMIS_SCLD( ISTR,L,:,: ) * EM_FAC_ST( ISTR,ISRC )%FAC( IFAC ) *
     &                   EM_REG_FAC( :,:,IRGN )
                     END DO
                  END IF
              CASE ( 3 )  ! Overwrite emissions
                  IF ( EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) .EQ. 1 ) THEN
                     ! Overwrite Emissions for the Entire Domain
                     VDEMIS_SCLD( ISTR,:,:,: ) = VDEMISRD( ISTR,:,:,: ) *
     &                  EM_FAC_ST( ISTR,ISRC )%FAC( IFAC )
                  ELSE
                     ! Overwrite Emissions just for a subdomain
                     IRGN = EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) 
                     DO L = 1,NL
                       VDEMIS_SCLD( ISTR,L,:,: ) = VDEMIS_SCLD( ISTR,L,:,: ) *
     &                     (1.0 - EM_REG_FAC( :,:,IRGN ) ) +
     &                   VDEMISRD( ISTR,L,:,: ) * EM_FAC_ST( ISTR,ISRC )%FAC( IFAC ) *
     &                   EM_REG_FAC( :,:,IRGN )
                     END DO
                  END IF
              END SELECT

        END DO
      END DO

      RETURN

      END SUBROUTINE EMISS_SCALING
 
C-----------------------------------------------------------------------
      SUBROUTINE EMISS_CONV_DIFF( )

C     Convert rate and map to Diffusivity module species order
C-----------------------------------------------------------------------
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA, CONVPA
      USE AERO_EMIS           ! inherits GRID_CONF

      IMPLICIT NONE

      ! Local Variables
      REAL             CNVTC         ! combined conversion factor
      REAL             CNVTI         ! intermediate combined conv. factor
      REAL,    SAVE :: CNVTP         ! intermediate combined conv. factor
      REAL             CONVM         ! Aerosol Mass conversion factor
      REAL             CONVN         ! Aerosol Number conversion factor
      REAL             CONVS         ! Aerosol Surface Area conversion factor
      REAL, PARAMETER :: GPKG = 1.0E+03       ! g/kg
      REAL, PARAMETER :: MWAIR = 28.9628      ! g/mol
      REAL, PARAMETER :: AVO  = 6.0221367E23
      REAL, PARAMETER :: RAVO = 1.0 / AVO
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER          GXOFF, GYOFF         ! global origin offset from file
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER       :: C, R, L, ISTR

      ! Get domain decomp info from the emissions file
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         CALL SUBHFILE ( EM_FILE_NAME( 1 ), GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
         CNVTP = CONVPA * 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 

      END IF    !FirstTime
      
      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
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
     &                MAP_EMtoAERO .EQ. 0 )
     &                VDEMIS_SCLD( :,L,C,R ) = CNVTC * VDEMIS_SCLD( :,L,C,R ) 
               WHERE( MAP_EMtoAERO .NE. 0 .AND. MAP_EMtoNUM .EQ. 0 .AND.
     &                MAP_EMtoSRF .EQ. 0 )
     &                VDEMIS_SCLD( :,L,C,R ) = CONVM * VDEMIS_SCLD( :,L,C,R ) 
               WHERE( MAP_EMtoNUM .NE. 0 )
     &                VDEMIS_SCLD( :,L,C,R ) = CONVN * VDEMIS_SCLD( :,L,C,R ) 
               WHERE( MAP_EMtoSRF .NE. 0 )
     &                VDEMIS_SCLD( :,L,C,R ) = CONVS * VDEMIS_SCLD( :,L,C,R ) 

            END DO
         END DO
      END DO
 
      ! zero out emissions values for diffused species not included in emissions list.
      ! ...accounts for emissions species names as a subset of the vert. diffused species list
      VDEMIS_CONV = 0.0
      DO ISTR = 1,N_EMIS_ISTR
        VDEMIS_CONV( MAP_EMtoDIFF( ISTR ),:,:,: ) = 
     &     VDEMIS_CONV( MAP_EMtoDIFF( ISTR ),:,:,: ) + VDEMIS_SCLD( ISTR,:,:,: )
      END DO
 

      RETURN
      END SUBROUTINE EMISS_CONV_DIFF


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
     &                         DIFF_MASK_TRAC, DIFF_MW
         USE UTILIO_DEFN
         USE AERO_DATA, only : N_MODE, AEROSPC,  ! Aerosol Properties Table
     &                         N_AEROSPC, AEROMODE, MODESUFF, EM_AERO_REF
         USE AERO_EMIS, only : MAP_NUMtoEM, MAP_SRFtoEM, MAP_EMtoAERO, MAP_EMtoMODE, 
     &                         MAP_EMtoNUM, MAP_EMtoSRF, WB_DUST, MAP_EMtoSD,
     &                         EM_SRC_SD, SD_SPLIT, EM_SD_INIT, SD_RULE
         USE UDTYPES,   only : CARRY1, LARRY1

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME
         INTEGER    :: N_UNUSED, REGNUM

         INTEGER    :: STRT, PTSTRT, ISPC, IDX, LOGDEV, IX, ISUR, IRULE,
     &                 N, IM, IAERO, V, ISRC,NSPC, IA, IDIFF, JDX, JM,
     &                 ISTR, ISUR0, ISRC0, IEM, KDX, JSD, JEM, ISD,
     &                 IFAC, IRGN, ISTRN, NFAC
         REAL       :: AERO_SPLIT, UNIT_FAC_1, UNIT_FAC_2, SURR_MW

         LOGICAL    :: LERROR, LFOUND, L_WDIFF, L_WISD, LTEST
         LOGICAL    :: LGAS_DIFF, LGAS_SURR
         
         CHARACTER( 16 )  :: SPECNAME, SN, SM

         CHARACTER( 16 )  :: CTM_EMISCHK = 'CTM_EMISCHK'    ! Let model proceed even if
                                                            ! Emissions surrogates are
                                                            ! not properly defined
         CHARACTER( 16 )  :: PNAME = 'EMIS_SPC_CHECK'
         CHARACTER( 100)  :: VARDESC
         INTEGER          :: STATUS
         CHARACTER( 500 ) :: XMSG
         CHARACTER( 16 )  :: B
         CHARACTER( 20 )  :: REFNAME
         
         INTEGER, PARAMETER :: NISTR0 = 3000
         LOGICAL, ALLOCATABLE, SAVE :: EM_SRC_RULE( : ), EM_SPEC_RULE( : )
         TYPE( LARRY1 ), ALLOCATABLE, SAVE :: EM_SURR_RULE( : ), EM_PHASE_RULE( : )
         INTEGER, PARAMETER :: N_SCALEFAC = 3000
        
         INTEGER            :: IC, N_COMMANDS
         INTEGER, PARAMETER :: NCOMM0 = 4000000
         INTEGER            :: RULE_ISTR_IDIFF( NCOMM0 )
         INTEGER            :: RULE_ISTR_ISRC ( NCOMM0 )
         INTEGER            :: RULE_ISTR_ISUR ( NCOMM0 )
         CHARACTER( 16 )    :: RULE_ISTR_SPEC ( NCOMM0 )
         CHARACTER( 16 )    :: RULE_ISTR_SURR ( NCOMM0 )
         INTEGER            :: RULE_ISTR_PHASE( NCOMM0 )

         REAL, ALLOCATABLE :: EM_FAC( :,:,: )
         REAL, ALLOCATABLE :: EM_FAC_BULK( :,:,: )
         CHARACTER( 1 ), ALLOCATABLE :: EM_OP( :,:,: )
         INTEGER, ALLOCATABLE :: EM_REG( :,:,: )


! Define General Parameters in Emission Control Namelist
         CHARACTER( 300 ) :: EQNAME
         INTEGER          :: EMCTRL_NML

         LOGICAL :: DefaultScaling      = .FALSE.
         CHARACTER( 16 ) :: DefaultFineSurrogates( 50 )
         CHARACTER( 16 ) :: DefaultCoarseSurrogates( 50 )
         NAMELIST / GeneralSpecs / DefaultScaling, Guard_BiogenicVOC,
     &              Guard_MarineGas, Guard_LightningNO, Guard_WindBlownDust,
     &              Guard_SeaSpray

! Define Emissions Rules Input from Emissions Control Namelist
         INTEGER             :: N_EM_RULE
         TYPE( EM_RULE_REG ) :: EM_NML( N_EM_RULE_REG )
         NAMELIST / EmissionScaling / EM_NML

         INTEGER, PARAMETER  :: N_SD_RULE = 200
         TYPE( SD_RULE ) :: SD_NML( N_SD_RULE )
         NAMELIST / SizeDistReg / SD_NML 

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
         WRITE( LOGDEV, '(/,/,5x,A)' ), REPEAT( '=', 75 )
         WRITE( LOGDEV, '(5x,A,A)' ), '|> SCALING EMISSIONS CONSISTENT WITH ',
     &                  'EMISSIONS CONTROL FILE SUPPLIED BY USER' 
         WRITE( LOGDEV, '(5x,A)' ), REPEAT( '=', 75 )
         !WRITE( LOGDEV, '(A58,/,A58)' ),
        ! &                  'The following tables list the chemical surrogates present ',
        ! &                  'on each emission file or from each online emission source.' 

         ! Print Out the Species From Each Emissions Source
        ! DO ISRC = 1,N_EM_SRC
        !    NSPC = EM_FILE_SURR( ISRC )%LEN
        !
        !    WRITE( LOGDEV, '(/,4A,I3,A8,/,4(2x,A16))' ),
        !&           TRIM( EM_FILE_DESC( ISRC ) ),' -- ',TRIM(EM_FILE_LAB( ISRC )),
        !&           ' -- ',NSPC,' Species',
        !&           EM_FILE_SURR( ISRC )%ARRY
        ! END DO

         ! Map the Emission Surrogate Species to Default CMAQ Internal
         ! Species, default size distributions, and default molecular
         ! weights based on SMOKE/MOVES/SPECIATE guidelines
         DO ISRC = 1,N_EM_SRC
            IF ( ISRC .EQ. ISEASRC .OR. ISRC .EQ. IBIOSRC .OR. 
     &           ISRC .EQ. IDUSTSRC.OR. ISRC .EQ. ILTSRC  .OR.
     &           ISRC .EQ. IMGSRC ) CYCLE

            ALLOCATE( EM_FILE_SURR( ISRC )%CMAQ ( EM_FILE_SURR( ISRC )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRC )%PHASE( EM_FILE_SURR( ISRC )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRC )%MW   ( EM_FILE_SURR( ISRC )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRC )%USED ( EM_FILE_SURR( ISRC )%LEN ) )
            EM_FILE_SURR( ISRC )%USED = .FALSE.

            DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
               B = EM_FILE_SURR( ISRC )%ARRY( ISUR )
               IA  = INDEX1( B, N_MAP_EMIStoCMAQ, MAP_EMIStoCMAQ( : )%SURR )
               IF ( IA .GT. 0 ) THEN
                  EM_FILE_SURR( ISRC )%CMAQ ( ISUR ) = MAP_EMIStoCMAQ( IA )%CMAQ
                  EM_FILE_SURR( ISRC )%PHASE( ISUR ) = MAP_EMIStoCMAQ( IA )%PHASE
                  EM_FILE_SURR( ISRC )%MW   ( ISUR ) = MAP_EMIStoCMAQ( IA )%MW
               ELSE
                  ! Surrogate is not calculated online nor does it
                  ! belong to the default list of commonly used
                  ! surrogates.
                  EM_FILE_SURR( ISRC )%CMAQ ( ISUR ) = B
                  EM_FILE_SURR( ISRC )%PHASE( ISUR ) = 'N/A'
                  EM_FILE_SURR( ISRC )%MW   ( ISUR ) = 1.0

                  !WRITE( LOGDEV, '(/,5x,A,A,A,A,/,5x,A,/,5x,A,/,5x,A,/,5x,A)' ) 
      !&                'Emissions Surrogate ',TRIM(B),' on source ',TRIM(EM_FILE_LAB( ISRC )),
      !&                'is not a recognized surrogate. If you would like for it to have a',
      !&                'default scaling rule, please add it, along with a default ',
      !&                'CMAQ internal species, a destination phase or mode, and a ',
      !&                'molecular weight to EMIS_VARS.'
               END IF
            END DO
         END DO

! Set up Source <-> Size Distribution relationship. This routine
! populates the EM_SRC_SD structure which tells the logic below which
! modes are present on which sources.
         SD_NML%SRC      = ''
         SD_NML%MODE     = ''
         SD_NML%MODE_REF = ''

         REWIND( EMCTRL_NML )
         READ( NML = SizeDistReg, UNIT = EMCTRL_NML )
         CALL EM_SD_INIT( SD_NML )
         
! Process Default Emissions Mapping (if requested in namelist; i.e. 
! DefaultScaling = TRUE) 
         ALLOCATE( EM_SPEC( NISTR0 ) )                ! CMAQ Species Names
         EM_SPEC = ""
         ALLOCATE( EM_SURR( NISTR0,N_EM_SRC ) )       ! Surrogate Name
         EM_SURR = ""
         ALLOCATE( EM_FAC ( NISTR0,N_EM_SRC,N_SCALEFAC ) ) ! Scale Factor
         EM_FAC = 0.0
         ALLOCATE( EM_FAC_BULK( NISTR0,N_EM_SRC,N_SCALEFAC ) ) ! Bulk Scale Factor For Printing to Diagnostic
         EM_FAC_BULK = 0.0                                        !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_OP( NISTR0,N_EM_SRC,N_SCALEFAC ) )  ! Bulk Scale Factor For Printing to Diagnostic
         EM_OP = ""                                          !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_REG( NISTR0,N_EM_SRC,N_SCALEFAC ) ) ! Bulk Scale Factor For Printing to Diagnostic
         EM_REG = 1                                          !   Output. Ignores aero_split and unit conversion
         ALLOCATE( MAP_EMtoDIFF( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoDIFF = 0                             !   to Diffusion Vector
         ALLOCATE( MAP_EMtoSURR( NISTR0,N_EM_SRC ) )  ! Map from Emissions Species 
         MAP_EMtoSURR = 0                             !   to Surrogate Locatino on File
         ALLOCATE( MAP_EMtoGAS( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoGAS = 0                             !   to aerosol table
         ALLOCATE( MAP_EMtoAERO( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoAERO = 0                             !   to aerosol table
         ALLOCATE( MAP_EMtoMODE( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoMODE = 0                             !   to CMAQ aerosol mode
         ALLOCATE( MAP_EMtoNUM ( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoNUM  = 0                             !   to aerosol number
         ALLOCATE( MAP_EMtoSRF ( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoSRF  = 0                             !   to aerosol surface area
         ALLOCATE( MAP_EMtoSD  ( NISTR0,N_EM_SRC ) )  ! Map from Emissions Species 
         MAP_EMtoSD  = 0                              !   to emissions aerosol mode
         
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

                 ! First Look for The Transported Species Name in the
                 ! Emissions Surrogate List. Then look for it on the
                 ! alternate list.
                 JDX = 0; JM  = 0; JSD = 0; JEM = 0

                 DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
                   IF ( EM_FILE_SURR( ISRC )%PHASE( ISUR ) .EQ. 'N/A' ) CYCLE

                   SN = EM_FILE_SURR( ISRC )%ARRY( ISUR )
                   SM = EM_FILE_SURR( ISRC )%CMAQ( ISUR )
                   ! Find Out which CMAQ aerosol modes are available
                   ! for this surrogate as a function of its source
                   ! and emission mode
                   IF ( EM_FILE_SURR( ISRC )%PHASE( ISUR ) .EQ. 'GAS' ) THEN ! Gas Surrogate
                     IF ( .NOT.DIFF_MASK_AERO( IDIFF )       .AND.           ! Gas CMAQ Species
     &                   ( TRIM( SN ) .EQ. DIFF_SPC( IDIFF ) .OR.            ! Name Match
     &                     TRIM( SM ) .EQ. DIFF_SPC( IDIFF ) )    ) THEN
                       JDX = ISUR
                       JSD = 1    ! 1 is Always the Gas Phase in the Sector Size Distribution Vector
                       JEM = 0    ! 0 is Always the gas phase in the Size Dist Reference List
                       ! Update Molecular Weight of the surrogate if it
                       ! is not already set
                       IF ( EM_FILE_SURR( ISRC )%MW( ISUR ) .EQ. 0. )
     &                      EM_FILE_SURR( ISRC )%MW( ISUR ) = DIFF_MW( IDIFF )
                     END IF
                   ELSEIF ( DIFF_MASK_AERO( IDIFF ) ) THEN  ! Aerosol CMAQ Species and Surrogate
                     ISD = INDEX1( EM_FILE_SURR( ISRC )%PHASE( ISUR ), 
     &                             EM_SRC_SD( ISRC )%LEN, EM_SRC_SD( ISRC )%NAME )
                     IEM = EM_SRC_SD( ISRC )%REF( ISD )
                     
                     ! Cycle through model aerosol modes and find ones that apply 
                     ! to this size distribution
                     DO IM = 1,N_MODE
                       IF ( EM_SRC_SD( ISRC )%FACNUM( ISD,IM ) .GT. 0.0 ) THEN
                         ! The surrogate mode ISD may be mapped to
                         ! CMAQ Model Mode IM
                         IF ( TRIM( SN )//MODESUFF( IM ) .EQ. DIFF_SPC( IDIFF ) .OR.
     &                        TRIM( SM )//MODESUFF( IM ) .EQ. DIFF_SPC( IDIFF ) .OR.
     &                        TRIM( SN ) .EQ. DIFF_SPC( IDIFF )                 .OR.
     &                        TRIM( SM ) .EQ. DIFF_SPC( IDIFF )             ) THEN 
                           JDX = ISUR   ! Save the surrogate index for mapping below
                           JM  = IM     ! Save this mode value for mapping below
                           JSD = ISD    ! Save the surrogate mode for mapping below
                           JEM = IEM    ! Save the reference size distribution index 
                                        !   for mapping below
                           ! Update Molecular Weight of the surrogate if it
                           ! is not already set
                           IF ( EM_FILE_SURR( ISRC )%MW( ISUR ) .EQ. 0. )
     &                          EM_FILE_SURR( ISRC )%MW( ISUR ) = DIFF_MW( IDIFF )
                         END IF
                       END IF
                     END DO
                   END IF
                 END DO

                 ! Populate Entry in Emissions Instructions if a Match was Found
                 IF ( JDX .GT. 0 ) THEN
                    ! A Surrogate Matches a Transported Species. If both the Species 
                    ! and Surrogate Match a Previously Defined Instruction, then add 
                    ! the scale factor to that entry under the relevant source number.
                    SPECNAME = DIFF_SPC( IDIFF )
                    IF ( N_EMIS_ISTR .NE. 0 ) SPECNAME = EM_SPEC( N_EMIS_ISTR )
                    IF ( DIFF_SPC( IDIFF ) .NE. SPECNAME .OR. 
     &                   N_EMIS_ISTR .EQ. 0 ) THEN
                       
                       ! Create A New Instruction
                       N_EMIS_ISTR = N_EMIS_ISTR + 1
                       EM_SPEC( N_EMIS_ISTR ) = DIFF_SPC( IDIFF )
                       MAP_EMtoDIFF( N_EMIS_ISTR ) = IDIFF

                       ! Link Row to the Aerosol Module. These CMAQ
                       ! species names should already include aerosol
                       ! mode suffixes (like I, J, or K) so search for
                       ! the entire name.
                       IF ( DIFF_MASK_AERO( IDIFF ) ) THEN 
                         IAERO = INDEX1( EM_SPEC( N_EMIS_ISTR ), N_AEROSPC, 
     &                                   AEROSPC%NAME( JM ) )
                         MAP_EMtoAERO( N_EMIS_ISTR ) = IAERO
                         MAP_EMtoMODE( N_EMIS_ISTR ) = JM
                       ELSE 
                         MAP_EMtoGAS( N_EMIS_ISTR ) = IDIFF
                       END IF
                    END IF
 
                    ! Only apply an aerosol size-split parameter if this
                    ! species is an aerosol and not from dust or sea spray
                    ! sectors
                    AERO_SPLIT = 1.0
                    IF ( DIFF_MASK_AERO( IDIFF ) .AND. ISRC .NE. IDUSTSRC .AND.
     &                       ISRC .NE. ISEASRC ) 
     &                   AERO_SPLIT = SD_SPLIT( IDIFF, JEM )

                    ! Update the Surrogate of an Existing Instruction
                    EM_FILE_SURR( ISRC )%USED( JDX ) = .TRUE.
                    EM_SURR( N_EMIS_ISTR, ISRC ) = EM_FILE_SURR( ISRC )%ARRY( JDX )
                    MAP_EMtoSURR( N_EMIS_ISTR, ISRC ) = JDX
                    MAP_EMtoSD( N_EMIS_ISTR, ISRC ) = JSD

                    EM_FAC ( N_EMIS_ISTR, ISRC, 1 ) = 1.0 * AERO_SPLIT
                    EM_FAC_BULK ( N_EMIS_ISTR, ISRC, 1 ) = 1.0         
                    EM_REG( N_EMIS_ISTR, ISRC, 1 ) = 1
                    EM_OP( N_EMIS_ISTR, ISRC, 1 )  = 'a'

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
               EM_FAC ( N_EMIS_ISTR+1,:,1 ) = 1.0
               EM_FAC_BULK ( N_EMIS_ISTR+1,:,1 ) = 1.0         
               EM_OP ( N_EMIS_ISTR+1,:,1 ) = "a"
               EM_REG ( N_EMIS_ISTR+1,:,1 ) = 1
               MAP_NUMtoEM( IM ) = N_EMIS_ISTR+1
               MAP_EMtoNUM( N_EMIS_ISTR+1 ) = IM
               MAP_EMtoDIFF( N_EMIS_ISTR+1 ) = 
     &            INDEX1( EM_SPEC( N_EMIS_ISTR+1 ), N_SPC_DIFF, DIFF_SPC )
               MAP_EMtoSD( N_EMIS_ISTR+1, : ) = 0

               ! Aerosol Surface Area
               EM_SPEC( N_EMIS_ISTR+2 ) = AEROMODE( IM )%SRF_NAME
               EM_FAC ( N_EMIS_ISTR+2,:,1 ) = 1.0
               EM_FAC_BULK ( N_EMIS_ISTR+2,:,1 ) = 1.0         
               EM_OP ( N_EMIS_ISTR+1,:,1 ) = "a"
               EM_REG ( N_EMIS_ISTR+1,:,1 ) = 1
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
         REWIND( EMCTRL_NML )
         READ( NML = EmissionScaling, UNIT = EMCTRL_NML )
         CLOSE( UNIT = EMCTRL_NML )
         
         ! Find Total Number of Rules
         DO IRULE = 1,N_EM_RULE_REG
            IF( EM_NML( IRULE )%SPEC .EQ. '' ) EXIT
            N_EM_RULE = IRULE
         END DO

         ! Implement Default Scaling Rules that will Always Be Applied
         IF ( DefaultScaling ) CALL CUSTOM_EM_RULES( N_EM_RULE, EM_NML )


         ! Allocate Rule->Instruction Transform Masks
         ALLOCATE( EM_SRC_RULE  ( N_EM_SRC   ) )
         ALLOCATE( EM_SPEC_RULE ( N_SPC_DIFF ) )
         ALLOCATE( EM_SURR_RULE ( N_EM_SRC ) )
         ALLOCATE( EM_PHASE_RULE( N_EM_SRC ) )
         DO ISRC = 1,N_EM_SRC
           N = EM_FILE_SURR( ISRC )%LEN
           EM_SURR_RULE( ISRC )%LEN = N
           ALLOCATE( EM_SURR_RULE( ISRC )%ARRY( N ) )

           N = EM_SRC_SD( ISRC )%LEN
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
            CALL INTERPRET_EM_RULE_SRC( EM_NML( IRULE )%SRC, IRULE, EM_SRC_RULE )

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
                    IF ( KDX .NE. 0 ) EM_SPEC_RULE( KDX ) = .TRUE.
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
                 ! Find the Specific Phase/Mode this Rule Identifies
                 IDX = INDEX1( EM_NML( IRULE )%PHASE, EM_SRC_SD( ISRC )%LEN, 
     &                         EM_SRC_SD( ISRC )%NAME )
                 IF ( IDX .NE. 0 ) THEN
                   EM_PHASE_RULE( ISRC )%ARRY( IDX ) = .TRUE.
                   LERROR = .FALSE.
                 END IF
              END IF
            END DO
            
            IF ( LERROR ) THEN
                 WRITE( LOGDEV, '(A,/,2A,/,A,/,A,I3,A1,/,A)') 
     &                  '*****************************ERROR***************************************:',
     &                  'The phase or mode ',TRIM( EM_NML( IRULE )%PHASE ),
     &                  ' is not found in any of the emission sources you''re requesting for ',
     &                  'emission rule ',IRULE,'.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
                 XMSG = 'Species with the missing surrogates ' 
     &                //'must have a surrogate found in at least one source.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check the Region Field
            CALL UPCASE( EM_NML( IRULE )%REGION )

            ! Check that the Region has been defined
            LERROR = .TRUE.
            DO IRGN = 1,N_EM_RGN 
              IF ( EM_NML( IRULE )%REGION .EQ. 
     &             EM_REGIONS( IRGN )%LABEL ) THEN 
                 REGNUM = IRGN
                 LERROR = .FALSE.
              END IF
            END DO
            
            IF ( LERROR ) THEN
                 WRITE( LOGDEV, '(A,/,2A,/,A,/,A,/,A)') 
     &                  '*****************************ERROR***************************************:',
     &                  'The Region ',TRIM( EM_NML( IRULE )%REGION ),
     &                  ' is not found in any of the regions defined',
     &                  ' in the Emission Control File.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
                 XMSG = 'Regions used in the Emissions Scaling must ' 
     &                //'be defined on the Emission Control File.'
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
            ! vector of instructions. The operator, scale factor, and
            ! region for each of these commands will be uniform because 
            ! they apply to the entire rule
            N_COMMANDS = 0
            DO IDIFF = 1,N_SPC_DIFF
            IF ( EM_SPEC_RULE( IDIFF ) ) THEN
              DO ISRC = 1,N_EM_SRC
              IF ( EM_SRC_RULE( ISRC ) ) THEN
                DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN 
                IF ( EM_SURR_RULE( ISRC )%ARRY( ISUR ) ) THEN
                  DO ISD = 1,EM_SRC_SD( ISRC )%LEN
                  IF ( EM_PHASE_RULE( ISRC )%ARRY( ISD ) ) THEN

                    ! Test to make sure if this is an aerosol species
                    ! that the size distirbution will actually populate it
                    LTEST = .TRUE.
                    IF ( DIFF_MASK_AERO( IDIFF ) ) THEN
                        DO IAERO = 1,N_AEROSPC
                           IM = INDEX1( DIFF_SPC( IDIFF ), N_MODE,
     &                                  AEROSPC( IAERO )%NAME( : ) )
                           IF ( IM .GT. 0 ) THEN
                             IF ( EM_SRC_SD( ISRC )%FACNUM( ISD,IM ) 
     &                              .LE. 1.0e-10 ) LTEST = .FALSE.
                           END IF
                        END DO
                    END IF

                    ! Add an Instruction for this combination of CMAQ
                    ! Species, Source, Surrogate, and Phase
                    IF ( LTEST ) THEN
                      N_COMMANDS = N_COMMANDS + 1
                      RULE_ISTR_IDIFF( N_COMMANDS ) = IDIFF
                      RULE_ISTR_ISRC ( N_COMMANDS ) = ISRC
                      RULE_ISTR_ISUR ( N_COMMANDS ) = ISUR
                      RULE_ISTR_SPEC ( N_COMMANDS ) = DIFF_SPC( IDIFF )
                      RULE_ISTR_SURR ( N_COMMANDS ) = 
     &                     EM_FILE_SURR( ISRC )%ARRY( ISUR )
                      RULE_ISTR_PHASE( N_COMMANDS ) = ISD
                    END IF
                  END IF
                  END DO
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
                  DO ISTRN = 1,N_EMIS_ISTR
                    IF ( EM_SPEC( ISTRN ) .EQ. RULE_ISTR_SPEC( IC ) .AND.
     &                   EM_SURR( ISTRN,RULE_ISTR_ISRC( IC ) ) .EQ. '' ) THEN
                      ! Add This Command to Instruction number ISTR
                      ISTR = ISTRN
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
                    ISTR = N_EMIS_ISTR

                    EM_SPEC( ISTR ) = RULE_ISTR_SPEC( IC )
                    MAP_EMtoDIFF( ISTR ) = RULE_ISTR_IDIFF( IC )

                    ! Link this row to the Aerosol Module.
                    IF ( DIFF_MASK_AERO( RULE_ISTR_IDIFF( IC ) ) ) THEN
                      DO IAERO = 1,N_AEROSPC
                        JM = INDEX1( EM_SPEC( ISTR ), N_MODE, AEROSPC( IAERO )%NAME( : ) )
                        IF ( JM .GT. 0 ) THEN
                          MAP_EMtoAERO( ISTR ) = IAERO
                          MAP_EMtoMODE( ISTR ) = JM
                        END IF
                      END DO
                    ELSE
                      MAP_EMtoGAS( ISTR ) = RULE_ISTR_IDIFF( IC )
                    END IF
                  END IF
                          
                  ! Now that the instruction location has either been
                  ! found or created, populate it.
                  ISRC = RULE_ISTR_ISRC( IC )
                  IEM  = EM_SRC_SD( ISRC )%REF( RULE_ISTR_PHASE( IC ) )
                  ISUR = RULE_ISTR_ISUR( IC )
                    
                  EM_SURR( ISTR, ISRC ) = RULE_ISTR_SURR( IC )
                  MAP_EMtoSURR( ISTR, ISRC ) = ISUR
                  EM_FILE_SURR( ISRC )%USED( ISUR ) = .TRUE.
                  MAP_EMtoSD( ISTR, ISRC ) = RULE_ISTR_PHASE( IC )

                  ! Only apply an aerosol size-split parameter if this
                  ! species is an aerosol and if it is not from a dust 
                  ! or sea spray sector
                  AERO_SPLIT = 1.0
                  IF ( DIFF_MASK_AERO( RULE_ISTR_IDIFF( IC ) ) .AND. 
     &                 ISRC .NE. IDUSTSRC .AND. ISRC .NE. ISEASRC    )
     &                 AERO_SPLIT = SD_SPLIT( RULE_ISTR_IDIFF( IC ), IEM )

                  ! Determine Next Free Location in Scale Factor Space
                  ! (IFAC) so that the scale factor can be added.
                  DO IFAC = 1,N_SCALEFAC
                      IF ( EM_OP( ISTR, ISRC, IFAC ) .EQ. '' ) THEN
                           EM_FAC ( ISTR, ISRC, IFAC ) = EM_NML( IRULE )%FAC * AERO_SPLIT
                        EM_FAC_BULK ( ISTR, ISRC, IFAC ) = EM_NML( IRULE )%FAC
                        EM_REG( ISTR, ISRC, IFAC ) = REGNUM
                        EM_OP( ISTR, ISRC, IFAC )  = EM_NML( IRULE )%OP
                        EXIT
                     END IF
                  END DO
               END DO
            ELSE
               ! Modify All Existing Instructions that Match this
               ! rule's parameters.
               DO IC = 1,N_COMMANDS
                 ! Loop through existing instructions and find matches
                 IF ( N_EMIS_ISTR .GT. 0 ) THEN
                   ISRC = RULE_ISTR_ISRC( IC )
                   DO ISTR = 1,N_EMIS_ISTR
                   IF ( EM_SPEC( ISTR ) .EQ. RULE_ISTR_SPEC( IC ) ) THEN
                      IF ( EM_SURR( ISTR, ISRC ) .EQ. RULE_ISTR_SURR( IC ) .AND.
     &                     MAP_EMtoSD( ISTR,ISRC ) .EQ. RULE_ISTR_PHASE( IC ) ) THEN
                  
                        IEM = EM_SRC_SD( ISRC )%REF( RULE_ISTR_PHASE( IC ) )
                        AERO_SPLIT = 1.0
                        IF ( DIFF_MASK_AERO( RULE_ISTR_IDIFF( IC ) ) .AND.
     &                       EM_NML( IRULE )%OP .EQ. 'o' ) 
     &                       AERO_SPLIT = SD_SPLIT( RULE_ISTR_IDIFF( IC ), IEM )

                        ! Determine Next Free Location in Scale Factor Space (IFAC) so 
                        ! that the scale factor can be added.
                        DO IFAC = 1,N_SCALEFAC
                          IF ( EM_OP( ISTR, ISRC, IFAC ) .EQ. '' ) THEN
                              EM_FAC ( ISTR, ISRC, IFAC )      = EM_NML( IRULE )%FAC * AERO_SPLIT
                             EM_FAC_BULK ( ISTR, ISRC, IFAC ) = EM_NML( IRULE )%FAC
                             EM_REG( ISTR, ISRC, IFAC )       = REGNUM
                             EM_OP( ISTR, ISRC, IFAC )        = EM_NML( IRULE )%OP
                             EXIT
                          END IF
                        END DO
 
                      END IF
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

! Inspect all of the Emissions Instructions for conversion between gases
! and particles. If there is a conversion, apply the correct factor
! based on species moleculer weights.
         ALLOCATE( EM_FAC_ST( N_EMIS_ISTR,N_EM_SRC ) )
         DO ISTR = 1,N_EMIS_ISTR
            DO ISRC = 1,N_EM_SRC
               ! First load emission factor structure
               NFAC = 1
               DO IFAC = 2,N_SCALEFAC
                   IF ( EM_OP( ISTR,ISRC,IFAC ) .EQ. '' ) EXIT
                 NFAC = NFAC + 1  
               END DO

        
               EM_FAC_ST( ISTR,ISRC )%LEN = NFAC
               ALLOCATE( EM_FAC_ST( ISTR,ISRC )%FAC( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRC )%BULK( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRC )%REG( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRC )%OP( NFAC ) )
                 
               EM_FAC_ST( ISTR,ISRC )%FAC  = 0.
               EM_FAC_ST( ISTR,ISRC )%BULK = 0.
               EM_FAC_ST( ISTR,ISRC )%REG  = 1
               EM_FAC_ST( ISTR,ISRC )%OP   = 0
        
               DO IFAC = 1,NFAC
                 EM_FAC_ST( ISTR,ISRC )%FAC( IFAC )    = EM_FAC ( ISTR, ISRC, IFAC ) 
                 EM_FAC_ST( ISTR,ISRC )%BULK( IFAC )   = EM_FAC_BULK( ISTR, ISRC, IFAC )
                 EM_FAC_ST( ISTR,ISRC )%REG( IFAC )    = EM_REG( ISTR, ISRC, IFAC )
                      
                   SELECT CASE ( EM_OP( ISTR, ISRC, IFAC ) )
                   CASE ( 'a' )
                       EM_FAC_ST( ISTR,ISRC )%OP( IFAC )    = 1
                   CASE ( 'm' )
                       EM_FAC_ST( ISTR,ISRC )%OP( IFAC )    = 2
                   CASE ( 'o' )
                       EM_FAC_ST( ISTR,ISRC )%OP( IFAC )    = 3
                 END SELECT
               END DO
            END DO

            ! Now Calculate Conversion Factors needed for going from gas moles
            ! to particle mass or vice-versa.
            ! Do Not Modify Number or Surface Area Emissions
            IF ( MAP_EMtoNUM( ISTR ) .GT. 0 .OR. MAP_EMtoSRF( ISTR ) .GT. 0 ) CYCLE

            UNIT_FAC_1 = 1.0

            ! First Get Phase of CMAQ Species
            IDIFF = MAP_EMtoDIFF( ISTR )
            LGAS_DIFF = .NOT. DIFF_MASK_AERO( IDIFF )
            IF ( LGAS_DIFF ) UNIT_FAC_1 = 1.0 / DIFF_MW( IDIFF )

            ! Now Get Phase of Emission Surrogate
            DO ISRC = 1, N_EM_SRC
              ISUR  = MAP_EMtoSURR( ISTR,ISRC )
              IF ( ISUR .EQ. 0 ) CYCLE

              LGAS_SURR = ( EM_FILE_SURR( ISRC )%PHASE( ISUR ) .EQ. 'GAS' )
              SURR_MW   = EM_FILE_SURR( ISRC )%MW( ISUR ) ! MW [g mol-1]

              UNIT_FAC_2 = UNIT_FAC_1
              IF ( LGAS_SURR ) UNIT_FAC_2 = UNIT_FAC_2 * SURR_MW

              DO IFAC = 1,EM_FAC_ST( ISTR,ISRC )%LEN
                 EM_FAC_ST( ISTR,ISRC )%FAC( IFAC ) = 
     &                  EM_FAC_ST( ISTR,ISRC )%FAC( IFAC ) * UNIT_FAC_2
              END DO
            END DO
         END DO

! Warn the User if there are no emissions instructions provided        
         IF ( N_EMIS_ISTR .LE. 0 ) THEN
            XMSG = 'There are no emissions instructions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

! Print a message for each emissions source surrogate that is not used.
         WRITE( LOGDEV, '(/,5x,A)' ) '|> Checking for unused Emissions Surrogates'
         WRITE( LOGDEV, '(5x,A)'   ),'==========================================='

         LERROR = .FALSE.
         DO ISRC = 1,N_EM_SRC
             N_UNUSED = COUNT( .NOT. EM_FILE_SURR( ISRC )%USED( : ) )
             WRITE( LOGDEV, '(5x,4A,I2,A)' ), TRIM( EM_FILE_LAB( ISRC )),' | ',
     &              TRIM(EM_FILE_DESC( ISRC )),': ', N_UNUSED,' unused surrogates.'
             IF ( N_UNUSED .GT. 0 ) THEN
                 LERROR = .TRUE.

                 DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
                   IF ( .NOT. EM_FILE_SURR( ISRC )%USED( ISUR ) ) 
     &               WRITE( LOGDEV, '(10x,A)'), EM_FILE_SURR( ISRC )%ARRY( ISUR )
                 END DO
             END IF
             WRITE( LOGDEV, '()' )
         END DO
         IF ( LERROR ) WRITE( LOGDEV, '(5x,A,/,5x,A,/,5x,A)' ),
     &       'ATTENTION: Some Emissions Surrogates are unused by your current', 
     &       'emission control configuration. You may want to include these ',
     &       'emissions if they are relevant to your application.'

! Resize Important Arrays
         MAP_EMtoDIFF = MAP_EMtoDIFF( 1:N_EMIS_ISTR )
         MAP_EMtoNUM  = MAP_EMtoNUM ( 1:N_EMIS_ISTR )
         MAP_EMtoSRF  = MAP_EMtoSRF ( 1:N_EMIS_ISTR )
         MAP_EMtoAERO = MAP_EMtoAERO( 1:N_EMIS_ISTR )


! An Emissions Scaling Map Now Exists as a 2D Array (NSPECIES x NSOURCES).
! For every element, there is an associated surrogate and scale factor
! to be applied. For the aerosols, the scale factor will be modified
! later in order to split the mass into the appropriate modes.


! Print out the Tables of CMAQ Emissions Instructions organized by each 
! emission source and then by CMAQ internal species.
         WRITE( LOGDEV, '(/,5x,A)' ),'|> Regions Available for Scaling:'
         WRITE( LOGDEV, '(5x,A)'   ),'================================='

         ! First Print Information about All the Available Regions for Scaling
         WRITE( LOGDEV,'(8x,A,2x,A,8x,A,10x,A)' ),'Number','Region Label','File Label','Variable'
         WRITE( LOGDEV,'(8x,A,2x,A,8x,A,10x,A)' ),'------','------------','----------','--------'
         DO IRGN = 1,N_EM_RGN
            WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),IRGN, EM_REGIONS(IRGN)%LABEL( 1:18 ),
     &             EM_REGIONS( IRGN )%File( 1:18 ), TRIM(EM_REGIONS( IRGN )%VAR )
         END DO

         WRITE( LOGDEV, '(/,/,5x,A)' ),'|> EMISSIONS SCALING DIAGNOSTIC:'
         WRITE( LOGDEV, '(5x,A)'   ),'=================================='
         WRITE( LOGDEV,'(7x,A,/,13x,A)' ),'Note: these are populated using rules from the',
     &                              'Emission Control File Supplied by the User.'
         DO ISRC = 1,N_EM_SRC
           
           ! Print summary information about each Sector including
           ! the label and the available aerosol modes
           WRITE( LOGDEV, '(/,5x,A,A)'   ),'>',REPEAT('-',80 )
           WRITE( LOGDEV,'(5x,A,A,A,A,A2,I2.2,A1)' ),
     &            'Source Type: "',TRIM(EM_FILE_DESC( ISRC )),
     &            '" | Sector Label: ',TRIM(EM_FILE_LAB( ISRC ) ),' (',ISRC,')'

           WRITE( LOGDEV, '(/8x,A)' ),'Table of Aerosol Size Distributions Available for Use Sector-Wide.'
           WRITE( LOGDEV, '(8x,A)' ),'Note that Mode 1 is reserved for gas-phase species and surrogates.'
           WRITE( LOGDEV, '(10x,A,2x,A,2x,A)' ), 'Number','Surrogate Mode','Reference Mode (see AERO_DATA.F)'
           WRITE( LOGDEV, '(10x,A,2x,A,2x,A)' ), '------','--------------','--------------------------------'
           DO ISD = 2,EM_SRC_SD( ISRC )%LEN
              IEM = EM_SRC_SD( ISRC )%REF( ISD )
              REFNAME = ''
              IF ( IEM .GT. 0 ) THEN
                  REFNAME = EM_AERO_REF( IEM )%NAME
              END IF

              WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),ISD, 
     &               EM_SRC_SD( ISRC )%NAME( ISD )( 1:16 ), TRIM( REFNAME )
           END DO


           ! Finally Print Every Instruction for this Source
           WRITE( LOGDEV, '(/,8x,A,5x,A,2x,A,9x,A,13x,A,1x,A)' ),
     &            'CMAQ Species','Phase/Mode','Surrogate','Region','Op','Scale Factor'
           WRITE( LOGDEV, '(  8x,A,5x,A,2x,A,9x,A,13x,A,1x,A)' ),
     &            '------------','----------','---------','------','--','------------'

           DO IDIFF = 1,N_SPC_DIFF
             L_WDIFF = .TRUE.
             DO ISD = 1,EM_SRC_SD( ISRC )%LEN
               L_WISD = .TRUE.
               DO ISUR = 1,EM_FILE_SURR( ISRC )%LEN
                 DO ISTR = 1,N_EMIS_ISTR
                   IF ( EM_SPEC( ISTR ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &                  MAP_EMtoSD( ISTR,ISRC ) .EQ. ISD       .AND.
     &                  MAP_EMtoSURR( ISTR,ISRC ) .EQ. ISUR ) THEN
                     IF ( L_WDIFF ) THEN
                        WRITE( LOGDEV, '(10x,A,1x,A,1x,A,1x,A,1x,A,2x,F6.3)' ),
     &                         EM_SPEC( ISTR ),
     &                         EM_SRC_SD( ISRC )%NAME( ISD )(1:10),
     &                         EM_SURR( ISTR, ISRC ),
     &                         EM_REGIONS( EM_FAC_ST( ISTR,ISRC )%REG( 1 ) )%LABEL(1:18),
     &                         EM_OP( ISTR,ISRC,1 ), EM_FAC_ST( ISTR, ISRC )%BULK( 1 )
                        L_WDIFF = .FALSE.
                        L_WISD  = .FALSE.
                      ELSE IF ( L_WISD ) THEN
                        WRITE( LOGDEV, '(27x,A,1x,A,1x,A,1x,A,2x,F6.3)' ),
     &                         EM_SRC_SD( ISRC )%NAME( ISD )(1:10),
     &                         EM_SURR( ISTR, ISRC ),
     &                         EM_REGIONS( EM_FAC_ST( ISTR,ISRC )%REG( 1 ) )%LABEL(1:18),
     &                         EM_OP( ISTR,ISRC,1 ), EM_FAC_ST( ISTR, ISRC )%BULK( 1 )
                        L_WISD = .FALSE.
                      ELSE 
                        WRITE( LOGDEV, '(38x,A,1x,A,1x,A,2x,F6.3)' ),
     &                         EM_SURR( ISTR, ISRC ),
     &                         EM_REGIONS( EM_FAC_ST( ISTR,ISRC )%REG( 1 ) )%LABEL(1:18),
     &                         EM_OP( ISTR,ISRC,1 ), EM_FAC_ST( ISTR, ISRC )%BULK( 1 )
                      END IF    

                      IF ( EM_FAC_ST( ISTR,ISRC )%LEN .GT. 1 ) THEN
                      DO IFAC = 2,EM_FAC_ST( ISTR,ISRC )%LEN
                         WRITE( LOGDEV, '(55x,A,1x,A,2x,F6.3)' ),
     &                          EM_REGIONS( EM_FAC_ST( ISTR,ISRC )%REG( IFAC ) )%LABEL(1:18),
     &                          EM_OP( ISTR,ISRC,IFAC ), EM_FAC_ST( ISTR, ISRC )%BULK( IFAC )
                      END DO
                      END IF

                   END IF
                 END DO
               END DO
             END DO
           END DO
         END DO

         ! End Emissions Scaling Preparation and Diagnostic Output
         WRITE( LOGDEV, '(/,5x,A)' ), REPEAT( '=',80 )
         WRITE( LOGDEV, '(5x,A,/,/)' ),
     &          '|> END EMISSIONS SCALING PREPARTION AND DIAGNOSTIC OUTPUT'

      END SUBROUTINE EMIS_SPC_MAP

C-----------------------------------------------------------------------
         SUBROUTINE EM_FILE_INIT( JDATE, JTIME )

C Initialize the counter for the total nbumber of emissions files. Also
C allocate memory for the vectors storing the labels of emission files
C and the maps from master ID number to the relative ID number for each
C gridded and point source file, i.e. Emissions File 10 is also known as
C Point Source file 2.
C
         USE AERO_DATA, only : MGPG, GPKG
         USE AERO_EMIS, only : WB_DUST, SS_AERO
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
         CHARACTER( 300 ) :: EQNAME
         LOGICAL :: SUCCESS
         INTEGER :: EMCTRL_NML

         TYPE( REGIONS_REG ) :: REGIONS_NML( N_REGIONS_REG )
         NAMELIST / Regions_Registry / REGIONS_NML

         LOGDEV = INIT3()
         SUCCESS = .TRUE.

C Initialize Regions for Mapping Scale Factors Geographically for Base
C Model (eventually apply this to DDM-3D and ISAM)
         CALL NAMEVAL( "EMISSCTRL_NML", EQNAME )
         
         REGIONS_NML%LABEL = ''
         REGIONS_NML%FILE  = ''
         REGIONS_NML%VAR   = ''

         EMCTRL_NML = JUNIT()
         OPEN( FILE = EQNAME, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &         POSITION = 'REWIND' )
         READ( NML = Regions_Registry, UNIT = EMCTRL_NML )
         CLOSE( EMCTRL_NML )
         CALL INIT_EMIS_REGIONS( REGIONS_NML )

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
 
C Determine if Sea Spray Aerosol is Requested
         SS_AERO = ENVYN( 'CTM_SS_AERO', 'Flag for Sea Spray Aerosol emissions',
     &                   .TRUE., STATUS )
         IF ( SS_AERO ) N_EM_SRC = N_EM_SRC + 1

C Determine if WindBlown Dust is Requested
         WB_DUST = ENVYN( 'CTM_WB_DUST', 'Flag for windblown dust emissions',
     &                   .TRUE., STATUS )
         IF ( WB_DUST ) N_EM_SRC = N_EM_SRC + 1

C Turn Back Now if N_EM_SRC Equals Zero (i.e. there are no emissions
         IF ( N_EM_SRC .EQ. 0 ) RETURN

C Allocate Emission File Structure Variables
         Allocate( EM_FILE_NAME( N_EM_SRC ) )
         Allocate( EM_FILE_LAB ( N_EM_SRC ) )
         Allocate( EM_FILE_TYPE( N_EM_SRC ) )
         Allocate( EM_FILE_ITYPE( N_EM_SRC ) )
         Allocate( EM_FILE_DESC( N_EM_SRC ) )
         Allocate( IGSRC( N_EM_SRC ) )
         Allocate( IPSRC( N_EM_SRC ) )
         Allocate( ITSRC( N_EM_SRC ) )
         Allocate( MAP_PTtoISRC( NPTGRPS ) )
         Allocate( EM_FILE_SURR( N_EM_SRC ) )
         Allocate( EM_GRID_LAYS( N_EM_SRC ) )
         Allocate( CONVEM_GAS( N_EM_SRC ) )
         Allocate( CONVEM_PM_MASS( N_EM_SRC ) )
         Allocate( EMIS_GAS_UNITS( N_EM_SRC ) )
         Allocate( EMIS_AER_UNITS( N_EM_SRC ) )
 
C Assign Attributes to Emission File Records. Other records will be 
C populated in individual subroutines. For example, opemis and
C stkemis_init.
         ISRC = 0
         EMIS_GAS_UNITS = "Unknown"
         EMIS_AER_UNITS = "Unknown"

         ! Gridded Emission Files
         IF ( N_FILE_GR .GT. 0 ) THEN
           EM_FILE_TYPE( ISRC+1:ISRC+N_FILE_GR ) = 'GRID'
           EM_FILE_ITYPE( ISRC+1:ISRC+N_FILE_GR ) = 1
           DO N = 1, N_FILE_GR
              ISRC = ISRC + 1
              IGSRC( ISRC ) = N
              WRITE( EM_FILE_DESC( ISRC ), '(A,I2)' ),
     &           'Gridded Area Emissions File ', N
             
              WRITE( VLAB,'( "GR_EMIS_LAB_",I2.2 )' ) N
              CALL ENVSTR( VLAB, 'Gridded Emission File Label',VLAB,
     &                     EM_FILE_LAB( ISRC ), IOS )

              WRITE( EM_FILE_NAME( ISRC ),'( "GR_EMIS_",I2.2 )' ) N
              CONVEM_GAS( ISRC ) = 1.0
              CONVEM_PM_MASS ( ISRC ) = 1.0
           END DO
         END IF
         
         ! In-Line Point Source Files
         IF ( NPTGRPS .GT. 0 ) THEN 
            EM_FILE_TYPE( ISRC+1:ISRC+NPTGRPS ) = 'POINT'
            EM_FILE_ITYPE( ISRC+1:ISRC+NPTGRPS ) = 2
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
               CONVEM_GAS( ISRC ) = 1.0
               CONVEM_PM_MASS ( ISRC ) = 1.0
            END DO
         END IF
         
         ! Tracer Emissions
         IF ( N_FILE_TR .GT. 0 ) THEN 
            EM_TRAC = .TRUE.
            EM_FILE_TYPE( ISRC+1:ISRC+N_FILE_TR ) = 'TRAC'
            EM_FILE_ITYPE( ISRC+1:ISRC+N_FILE_TR ) = 3
            DO N = 1, N_FILE_TR
               ISRC = ISRC + 1
               ITSRC( ISRC ) = N
               WRITE( EM_FILE_DESC( ISRC ), '(A,I2)' ),
     &           'Gridded Tracer Emissions File ', N

               WRITE( VLAB,'( "TR_EMIS_LAB_",I2.2 )' ) N
               CALL ENVSTR( VLAB, 'Tracer Emission File Label',VLAB,
     &                      EM_FILE_LAB( ISRC ), IOS )

               WRITE( EM_FILE_NAME( ISRC ),'( "TR_EMIS_",I2.2 )' ) N
               CONVEM_GAS( ISRC ) = 1.0
               CONVEM_PM_MASS ( ISRC ) = 1.0
            END DO
         ELSE
            EM_TRAC = .FALSE.
         END IF
         
         ! Online Biogenic Emissions (BEIS)
         IF ( BIOGEMIS ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'BIOG'
             EM_FILE_ITYPE( ISRC ) = 4
             EM_FILE_LAB ( ISRC ) = 'BIOG'
             EM_FILE_DESC( ISRC ) = 'Biogenic Emissions'
             CONVEM_GAS( ISRC ) = 1.0E-3
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_GAS_UNITS( ISRC ) = "Unknown"

             IBIOSRC = ISRC
         END IF

         ! Online Marine Gas Emissions
         IF ( MGEM ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'MGEM'
             EM_FILE_ITYPE( ISRC ) = 5
             EM_FILE_LAB ( ISRC ) = 'MGEM'
             EM_FILE_DESC( ISRC ) = 'Marine Gas Emissions'
             CONVEM_GAS( ISRC ) = 1.0E-3        ! mol/s -> kmol/s
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_GAS_UNITS( ISRC ) = "Unknown"

             IMGSRC = ISRC
         END IF

         ! Online Lightning NO Emissions
         IF ( LTNG_NO ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'LTNG'
             EM_FILE_ITYPE( ISRC ) = 6
             EM_FILE_LAB ( ISRC ) = 'LTNG'
             EM_FILE_DESC( ISRC ) = 'Lightning NO Emissions'
             CONVEM_GAS( ISRC ) = 1.0E-3
             CONVEM_PM_MASS ( ISRC ) = 1.0
             EMIS_GAS_UNITS( ISRC ) = "Unknown"

             ILTSRC = ISRC
         END IF

         ! Sea Spray Aerosol Emissions
         IF ( SS_AERO ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'ASEA'
             EM_FILE_ITYPE( ISRC ) = 7
             EM_FILE_LAB ( ISRC ) = 'SEASPRAY'
             EM_FILE_DESC( ISRC ) = 'Sea Spray Aerosol Emissions'
             CONVEM_GAS( ISRC ) = 1.0
             CONVEM_PM_MASS ( ISRC ) = MGPG
             EMIS_AER_UNITS( ISRC ) = "g m-3 s-1"
             ISEASRC = ISRC
         END IF

         ! Wind-Blown Dust Emissions
         IF ( WB_DUST ) THEN
             ISRC = ISRC + 1
             EM_FILE_TYPE( ISRC ) = 'DUST'
             EM_FILE_ITYPE( ISRC ) = 8
             EM_FILE_LAB ( ISRC ) = 'WBDUST'
             EM_FILE_DESC( ISRC ) = 'Wind-Blown Dust Emissions'
             CONVEM_GAS( ISRC ) = 1.0
             CONVEM_PM_MASS ( ISRC ) = MGPG
             EMIS_AER_UNITS( ISRC ) = "g m-3 s-1"
             IDUSTSRC = ISRC
         END IF
          


         END SUBROUTINE EM_FILE_INIT    

      END MODULE EMIS_DEFN
