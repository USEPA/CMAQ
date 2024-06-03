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
      MODULE DESID_MODULE

C-----------------------------------------------------------------------
C Function: Detailed Emissions, Scaling, Isolation and Diagnostic Module
C           (DESID) for CMAQ. 

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
C     16 NOv 2018 S.Napelenok: ISAM implementation
C     01 Feb 2019 D.Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses, replaced IOS with LOGDEV when calling GET_ENV
C     15 May 2019 D.Wong: Put the check for using marine gas emission or not in RUNTIME_VAR.F
C     4 Oct 2019 D.Wong: fixed the time advancement, NEXTIME, for a multi-day run in subroutine
C                        GRIDEMIS
C     22 Nov 2019 F. Sidi: Re-introduced date override variable for representative
C                          Day Emissions
C     16 Dec 2019 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C     6 Jul 2021 D.Wong: fixed time marching through a day line during simulation w.r.t.
C                        representative Day Emissions in subroutine GR3D
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE DESID_VARS
      USE DESID_UTIL
      USE VDIFF_MAP, ONLY : N_SPC_DIFF
      USE UTILIO_DEFN

      IMPLICIT NONE

      PUBLIC DESID_INIT, DESID_DRIVER
      
      PRIVATE
      
      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION DESID_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE BEIS_DEFN           ! biogenic emissions
         USE MEGAN_DEFN          ! biogenic emissions from MEGAN
         USE MGEMIS              ! marine gas emissions
         USE LTNG_DEFN           ! NO emissions from lightning strikes
         USE PT3D_DEFN           ! plume rise emissions
         USE UTILIO_DEFN         ! I/O API
         USE AERO_EMIS           ! inherits GRID_CONF
         !USE AERO_DATA           ! access subroutine map_pmemis
         USE CENTRALIZED_IO_MODULE, only : interpolate_var
         USE ASX_DATA_MOD, only: MET_DATA

#ifdef isam
         USE SA_DEFN, ONLY : SA_VDEMIS_DIFF, NTAG_SA, SA_VDEMIS_CONV, NSPC_SA,
     &                       SA_VDEMIS_CONV_OTHER
#endif

#ifdef sens
        USE DDM3D_DEFN, ONLY: NP, NPMAX, SVDEMIS_DIFF
#endif

#ifdef mpas
        USE COUPLER_MODULE, ONLY : cell_thickness, cell_vol, MPAS_CELL_AREA => CELL_AREA
#endif
         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP   ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

C Parameters:
                                        
C Local Variables:

         CHARACTER( 16 ) :: PNAME = 'DESID_INIT'
         CHARACTER( 80 ) :: VARDESC   ! env variable description
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER V, L, STATUS, ISRM

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         ! Initialize Emission Variable Molecular Weights
         CALL INIT_DESID_EMVAR_MW()

         ! Define Projected Grid Cell Areas
         ALLOCATE( CELLAREA( NCOLS,NROWS ),STAT=STATUS )
         CALL CHECKMEM( STATUS, 'CELLAREA', PNAME )
         ALLOCATE( CELLHGT( NCOLS, NROWS, NLAYS ),STAT=STATUS )
         CALL CHECKMEM( STATUS, 'CELLHGT', PNAME )
         ALLOCATE( CELLVOL( NCOLS,NROWS,NLAYS ),STAT=STATUS )
         CALL CHECKMEM( STATUS, 'CELLVOL', PNAME )
#ifdef mpas
         CELLAREA(:,:) = MPAS_CELL_AREA(:,:)
         CELLHGT( :,1,: ) = cell_thickness(:,1,:)
         CELLVOL( :,1,: ) = cell_vol(:,1,:)
#else
         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + 
     &            YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
         ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
         END IF
         CELLAREA(:,:) = REAL( DX1 * DX2, 4 )
         ! Get height of grid cell in each layer in sigma coordinates
         ! Multiply by grid area [m2] to obtain grid volume

         !cellhgt = Met_Data%DZF ! for spatially varying height

         DO L = 1, NLAYS
            CELLHGT( :,:,L ) = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            CELLVOL( :,:,L ) = CELLHGT( :,:,L ) * CELLAREA(:,:)
         END DO
#endif
 
         ! REMINDER: CELLAREA is projected grid cell area (e.g. 12 x 12
         ! km^2). To get real area on the Earth for a conformal grid, you
         ! should divide CELLAREA by the MAP SCALE FACTOR squared, typically
         ! stored in CMAQ as MSFX2

C Retrieve Number of Emission Streams of Various Types (sectors)
         CALL DESID_INIT_STREAMS( JDATE, JTIME )
         IF ( DESID_N_SRM .EQ. 0 ) THEN 
            XMSG = 'No Emissions Streams Have Been Selected.'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .TRUE.; RETURN
         END IF

C Open Area Emissions files
         CALL DESID_OPEN_GR3D ( JDATE, JTIME )

C Get number of emissions layers
         IF ( DESID_MAXLAYS .LE. 0 ) THEN
            ! Find The Largest Gridded Emission Layer And Let That be
            ! the initial top.
            DESID_LAYS = MAXVAL( DESID_GRID_LAYS(:) ) 
            
            ! If there are 3D (inline point or Lightning) sources, 
            ! revise the top to be the model top.
            IF ( NPTGRPS .GT. 0 .OR. LTNG_NO ) DESID_LAYS = NLAYS

            ! Make sure the top is not greater than the model top
            DESID_LAYS = MAX( MIN( DESID_LAYS, NLAYS ), 1 )
         ELSE
           ! Make sure the top is not greater than the model top
           DESID_LAYS = MIN( DESID_MAXLAYS, NLAYS )

         END IF

         WRITE( LOGDEV,1009 ) DESID_LAYS, NLAYS
1009     FORMAT(    5X, 'Number of Emissions Layers:         ', I3
     &           /  5X, 'out of total Number of Model Layers:', I3 )


C Initialize 3D Point Source Emissions 
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Point Emissions' )
         IF ( .NOT. PT3D_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( BIOGEMIS_MEGAN ) THEN
C Initialize Online Biogenic Emissions, MEGAN
          CALL LOG_SUBHEADING( LOGDEV, 'Initialize BEIS Biogenic Emissions' )
          IF ( .NOT. MEGAN_INIT( JDATE, JTIME, TSTEP ) ) THEN
             XMSG = 'Failure initializing biogenics emissions module'
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
         END IF
         IF ( BIOGEMIS_BEIS ) THEN 
C Initialize Online Biogenic Emissions, BEIS
          CALL LOG_SUBHEADING( LOGDEV, 'Initialize MEGAN Biogenic Emissions' )
          IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
         END IF
C Initialize Online Marine Gas Emissions
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Marine Gas Emissions' )
         IF ( .NOT. MGEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing marine gas emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Online Lightning NOx Emissions
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Lightning NO Emissions' )
         IF ( .NOT. LTNG_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing lightning emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Aerosol Emissions         
         CALL LOG_SUBHEADING( LOGDEV,'Process Aerosol Emissions' )
         IF ( .NOT. AERO_EMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing aerosol emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Map the Emission Variables Available on the Input Files To the
C CMAQ Species Identified by the User via the Namelists and Stop the model
C or Print Warnings if Mistakes Are Made.

         CALL DESID_PROCESS_RULES( JDATE, JTIME )
         CALL DESID_INIT_DIAG
#ifndef mpas
         IF ( IO_PE_INCLUSIVE ) CALL DESID_OPEN_DIAG( JDATE, JTIME, TSTEP )
#else
         CALL DESID_OPEN_DIAG( JDATE, JTIME, TSTEP )
#endif

C Allocate Space for Master Emissions Computation
         ALLOCATE ( VDEMIS_DIFF( N_SPC_DIFF,DESID_LAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

#ifdef isam
         ALLOCATE ( SA_VDEMIS_DIFF( NSPC_SA,   DESID_LAYS,NCOLS,NROWS, NTAG_SA ),
     &              SA_VDEMIS_CONV( N_SPC_DIFF,DESID_LAYS,NCOLS,NROWS, NTAG_SA ), 
     &              SA_VDEMIS_CONV_OTHER( N_SPC_DIFF,DESID_LAYS,NCOLS,NROWS ), 
     &                                                  STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'SA_VDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
#endif

#ifdef sens
         ALLOCATE ( SVDEMIS_DIFF( N_SPC_DIFF,DESID_LAYS,NCOLS,NROWS,NPMAX ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'SVDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
#endif

C Return From Initialization
         SUCCESS = .TRUE.; RETURN

         END FUNCTION DESID_INIT

!-----------------------------------------------------------------------
         SUBROUTINE DESID_DRIVER ( JDATE, JTIME, TSTEP, CGRID )
!-----------------------------------------------------------------------
!        DESID_DRIVER controls whether DESID_GET_EMIS will be 
!        executed to process emissions for input back to vdiff or if 
!        it is being run in diagnostic mode to populate DESID diagnostic
!        output files. 
!        
!        In typical operation, emissions used by CMAQ are calculated at 
!        1/2 time step before the output time step, meaning that any
!        diagnostics output using these values would be quantitatively 
!        inconsistent with the emissions inputs, which are usually
!        tabulated at the output time step (e.g., hourly). By allowing
!        DESID_GET_EMIS to be called in diagnostic mode, with
!        L_DESID_DIAG = .TRUE., DESID can recalculate emissions at
!        exactly the the output time step and populate diagnostic output
!        files with these values. These values are not actually used for
!        emissions calculations within CMAQ. 
!-----------------------------------------------------------------------

         USE UTILIO_DEFN
         USE centralized_io_module
#ifdef mpas
         use util_module, only : time2sec, nextime
#endif
         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL, POINTER :: CGRID( :,:,:,: )

         LOGICAL         :: L_DESID_DIAG
         LOGICAL         :: FIRST_TIME = .TRUE.
         INTEGER, SAVE   :: WSTEP, MDATE, MTIME
         LOGICAL         :: WRTIME
         CHARACTER( 200 ):: XMSG

         INTEGER :: l

         CALL LOG_MESSAGE( LOGDEV, 'Beginning Emissions' )
         
         IF ( FIRST_TIME ) THEN
            FIRST_TIME = .FALSE.

            ! Initialize Diagnostic Write Counter
            WSTEP = 0 
            MDATE = STDATE
            MTIME = STTIME
           
            ! Execute DESID Emission Processor in Diagnostic Mode
            ! for the Initial Hour of the Simulation (usually 0Z)
            VDEMIS_DIAG = 0.0
            WRITE( XMSG, '(A,I10,A,I10)' ),
     &           '  Calling Diagnostic Emissions at Date: ',MDATE,
     &           '  and time: ',MTIME
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            CALL DESID_GET_EMIS( MDATE, MTIME, TSTEP, CGRID, .TRUE. )
         END IF
         
         ! Increment Diagnostic Counter
         WRTIME = .FALSE.
         WSTEP  = WSTEP + TIME2SEC( TSTEP( 2 ) )
         
         ! Determine whether or not this is a write step
         WRTIME = ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) )
         IF ( WRTIME ) THEN 
           WSTEP = 0
           CALL NEXTIME( MDATE, MTIME,  TSTEP( 1 ) )

           ! Execute DESID Emission Processor in Diagnostic Mode
           VDEMIS_DIAG = 0.0
           WRITE( XMSG, '(A,I10,A,I10)' ),
     &          '  Calling Diagnostic Emissions at Date: ',MDATE,
     &          '  and time: ',MTIME
           CALL LOG_MESSAGE( LOGDEV, XMSG )
           CALL LOG_MESSAGE( LOGDEV, '' )
           CALL DESID_GET_EMIS( MDATE, MTIME, TSTEP, CGRID, .TRUE. )

         END IF
 
         ! Execute DESID Emission Processor in Real Mode
           WRITE( XMSG, '(A,I10,A,I10)' ),
     &          '  Calling Real Emissions at Date: ',JDATE,
     &          '  and time: ',JTIME
           CALL LOG_MESSAGE( LOGDEV, XMSG )
           CALL LOG_MESSAGE( LOGDEV, '' )

         CALL DESID_GET_EMIS( JDATE, JTIME, TSTEP, CGRID, .FALSE. )

         END SUBROUTINE DESID_DRIVER

!-----------------------------------------------------------------------
         SUBROUTINE DESID_GET_EMIS ( JDATE, JTIME, TSTEP, CGRID, L_DESID_DIAG )

         ! Step through all emission sub-modules for each stream
         ! (gridded and point offline, biog, SeaSalt, Dust, Lightning
         ! NO, etc.) and apply scaling factors, unit conversions, etc.

         ! If L_DESID_DIAG = .FALSE. then this is a standard call of
         ! DESID_GET_EMIS and emissions will be returned to VDIFF to
         ! supply the CMAQ system with emissions. If L_DESID_DIAG =
         ! .TRUE., then emissions are calculated and written out to
         ! diagnostic files but not used by VDIFF or any other part of
         ! CMAQ.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE MEGAN_DEFN          ! biogenic emissions from MEGAN
         USE BIOG_EMIS, ONLY: MSPCS
         USE SSEMIS
         USE DUST_EMIS
         USE MGEMIS              ! marine gas emissions
         USE PT3D_DEFN           ! plume rise emissions
         USE LTNG_DEFN           ! lightning NO emissions
         USE UTILIO_DEFN
         USE HGRD_DEFN
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE AERO_DATA, ONLY : AERONUM_MAP, AEROSRF_MAP, DUSTOUTM

#ifdef isam
         USE SA_DEFN, ONLY : SA_VDEMIS_DIFF, ITAG, NTAG_SA, TAGSTREAMS,
     &                       STREAM_TO_TAG, OTHRTAG, TAGSTREAMS_TEMP,
     &                       TAGSTREAMS_NUM, SA_VDEMIS_CONV,
     &                       SA_VDEMIS_CONV_OTHER,
     &                       MAP_DIFFtoSA, ISAM_SPEC, NSPC_SA,
     &                       ISAMRGN_TEMP, ISAMRGN, ISAMRGN_NUM,
     &                       ISAMRGN_MAP, TAGS_PER_STREAM,
     &                       ISAM_PVO3_MAP, L_OZONE
         USE VDIFF_MAP, ONLY : DIFF_MAP, DIFF_SPC
#endif

#ifdef sens
         USE DDM3D_DEFN, ONLY: NP, NPMAX, SVDEMIS_DIFF, IPT, SENNUM,
     &                         S_STREAMLBL, S_NSTREAMS, IPARM, IREGION,
     &                         STREAM_TO_SENS, SENS_PER_STREAM
         USE VDIFF_MAP, ONLY : DIFF_MAP
#endif
#ifdef mpas
         use util_module, only : index1
#endif
         USE centralized_io_module

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL, POINTER :: CGRID( :,:,:,: )
         LOGICAL, INTENT( IN ) :: L_DESID_DIAG

C Local Variables:
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V, ISTR, ISRM, NL, I, J ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables
         REAL, ALLOCATABLE, SAVE :: VDEMIS_READ  ( :,:,:,: ) ! Emissions as they are provided by each stream
         REAL, ALLOCATABLE, SAVE :: VDEMIS_SCALED( :,:,:,: ) ! Emissions after user scaling, and spatial scaling
         REAL, ALLOCATABLE, SAVE :: VDEMIS_CONV  ( :,:,:,: ) ! Emissions after converting to units for VDIFF
         

         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 300 ):: XMSG = ' '
         INTEGER         :: ERROR_NEG
         INTEGER, SAVE   :: WSTEP
         LOGICAL, SAVE   :: FIRST_TIME = .TRUE.
         LOGICAL         :: EFLAG

#ifdef isam
         INTEGER                    :: IDX, SPC, LAYER, RGN, IRGN
         CHARACTER(96)              :: TXTSTRING
         REAL                       :: MINCHECK, TOTAL
#endif
C-----------------------------------------------------------------------

         EFLAG = .FALSE.

         IF ( FIRST_TIME ) THEN
            FIRST_TIME = .FALSE.

            ALLOCATE( VDEMIS_READ  ( DESID_N_ISTR,DESID_LAYS,NCOLS,NROWS ),
     &                VDEMIS_SCALED( DESID_N_ISTR,DESID_LAYS,NCOLS,NROWS ),
     &                VDEMIS_CONV  ( N_SPC_DIFF, DESID_LAYS,NCOLS,NROWS ) )

#ifdef isam
C Map Emissions streams to ISAM tags
            ALLOCATE( STREAM_TO_TAG( DESID_N_SRM, NTAG_SA ),
     &                TAGS_PER_STREAM( DESID_N_SRM ),
     &                MAP_DIFFtoSA ( NSPC_SA  ),
     &                TAGSTREAMS( NTAG_SA, DESID_N_SRM ) )
            STREAM_TO_TAG   = 0
            TAGS_PER_STREAM = 0
            MAP_DIFFtoSA    = 0
            TAGSTREAMS      = 'empty'

            IF ( LPVO3 ) THEN
               ALLOCATE ( ISAM_PVO3_MAP( NTAG_SA ) )
               ISAM_PVO3_MAP = 0
            END IF

            DO ITAG = 1, NTAG_SA-3

C Count the number of streams going to each ISAM tag
              TXTSTRING = TRIM(TAGSTREAMS_TEMP( ITAG ))
              TAGSTREAMS_NUM( ITAG ) = 1 + COUNT(TRANSFER(TXTSTRING, 'A', LEN(TXTSTRING)) == "," )

C Parse out the stream names the user wants tagged
              READ(TXTSTRING, *) TAGSTREAMS(ITAG,1:TAGSTREAMS_NUM( ITAG ))

C Find emissions stream in the list of available streams
              DO ISRM = 1, TAGSTREAMS_NUM( ITAG )
                 IF ( TAGSTREAMS(ITAG,ISRM) .EQ. 'PVO3' ) THEN
                    IF ( LPVO3 ) THEN 
                       IF ( L_OZONE ) THEN
                         ISAM_PVO3_MAP(ITAG) = 1
                         CYCLE
                       ELSE
                         XMSG = " Must specifiy OZONE ISAM TAG CLASS" //
     &                          " in isam_control to track PVO3 "
                         CALL M3EXIT( 'ISAM_PVO3', 1, 1, XMSG, XSTAT1 )
                       ENDIF
                    ELSE
                       XMSG = " To track PVO3, run-script must set CTM_LPVO3" //
     &                        " to Y or T for Yes or True. Check run-script."
                       CALL M3EXIT( 'ISAM_PVO3', 1, 1, XMSG, XSTAT1 )
                    END IF 
                 ENDIF

                 IF ( TAGSTREAMS(ITAG,ISRM) .EQ. 'BIDIRECTIONALNH3' )THEN
                   IF ( ABFLUX ) THEN
                     CYCLE ! don't need a stream for this
                   ELSE
                     XMSG = " BIDIRECTIONALNH3 specified," //
     &                      " but ABLUX set to FALSE "
                     CALL M3EXIT( 'ISAM_PVO3', 1, 1, XMSG, XSTAT1 )
                   ENDIF           
                 END IF

                 IDX = INDEX1(TAGSTREAMS(ITAG,ISRM),DESID_N_SRM,DESID_STREAM_LAB )
                 IF ( IDX .EQ. 0 ) THEN
                   XMSG = " User specified ISAM tag - " //
     &                    TRIM( TAGSTREAMS(ITAG,ISRM) )// 
     &                    " - not found in available emissions streams "
                   CALL M3MESG(XMSG)
                   EFLAG = .TRUE.
                 ELSE
                   TAGS_PER_STREAM( IDX ) = TAGS_PER_STREAM( IDX ) + 1
                   STREAM_TO_TAG(IDX,TAGS_PER_STREAM( IDX )) = ITAG
                 END IF
              END DO

            END DO
            IF( EFLAG )THEN
                WRITE(LOGDEV,'(A)')'Available Emissions Streams'
                DO ITAG = 1,DESID_N_SRM
                   WRITE(LOGDEV,'(4X,A)')DESID_STREAM_LAB(ITAG)
                END DO
                XMSG = 'INCORRECT Emissions Stream used in ISAM control file'
                CALL M3EXIT( 'ISAM_STREAMS', 1, 1, XMSG, XSTAT1 )
            END IF

C Develop a map of emitted/diffused ISAM species
            MAP_DIFFtoSA = 0
            DO SPC = 1, NSPC_SA
              MAP_DIFFtoSA( SPC ) = INDEX1(TRIM(ISAM_SPEC(SPC,1)), N_SPC_DIFF, DIFF_SPC )
            END DO

C Initialize geographic regions for ISAM tagging
            ALLOCATE( ISAMRGN( NTAG_SA, DESID_N_REG ) )
            ISAMRGN = 'EVERYWHERE'
            ALLOCATE( ISAMRGN_MAP( NTAG_SA, DESID_N_REG ) )
            ISAMRGN_MAP = 0
            ISAMRGN_NUM = 0

            DO ITAG = 1, NTAG_SA-3

C Check if the 'EVERYWHERE' keyword is present
              IDX = INDEX(ISAMRGN_TEMP(ITAG),'EVERYWHERE')
              IF ( IDX .NE. 0 ) THEN
                ISAMRGN_NUM( ITAG ) = 0
                CYCLE
              ENDIF

C Count the number of regions for each tag
              TXTSTRING = TRIM(ISAMRGN_TEMP( ITAG ))
              ISAMRGN_NUM( ITAG ) = 1 + COUNT(TRANSFER(TXTSTRING,'A', LEN(TXTSTRING)) == "," )

C Parse out the region names the user wants tagged
              READ(TXTSTRING, *) ISAMRGN(ITAG,1:ISAMRGN_NUM( ITAG ))

C Map the user specified ISAM regions to available CMAQ regions
              DO ISRM = 1, ISAMRGN_NUM(ITAG)
                IDX = INDEX1(ISAMRGN(ITAG,ISRM),DESID_N_REG, DESID_REG%LABEL )
                IF ( IDX .EQ. 0 ) THEN
                  XMSG = " User specified ISAM region - " //
     &                    TRIM( ISAMRGN(ITAG,ISRM) ) //
     &                    " - not found in available emissions regions "
                   CALL M3EXIT( 'ISAM_STREAMS', 1, 1, XMSG, XSTAT1 )
                ELSE
                   ISAMRGN_MAP(ITAG,ISRM) = IDX
                END IF
              END DO

            END DO

#endif

#ifdef sens
            ALLOCATE( STREAM_TO_SENS( DESID_N_SRM, NPMAX ),
     &                SENS_PER_STREAM( DESID_N_SRM ))
            STREAM_TO_SENS = 0
            SENS_PER_STREAM = 0

            DO NP = 1, NPMAX
              IF ( IPT ( NP ) .EQ. 3 ) THEN ! This is an emissions sensitivity
                IF ( S_NSTREAMS(NP) .EQ. 99 ) THEN ! all emissions streams
                  S_NSTREAMS(NP) = DESID_N_SRM
                  S_STREAMLBL(NP,1:DESID_N_SRM)=DESID_STREAM_LAB(1:DESID_N_SRM)
                END IF
                DO ISRM = 1, S_NSTREAMS(NP)
                  SENNUM = INDEX1(S_STREAMLBL(NP,ISRM),DESID_N_SRM,DESID_STREAM_LAB)
                  IF ( SENNUM .EQ. 0 ) THEN
                    XMSG = " User specified DDM Emissions Stream - " //
     &                    TRIM( S_STREAMLBL(NP,ISRM) )//
     &                    " - not found in available emissions streams "
                    CALL M3EXIT( 'ISAM_STREAMS', 1, 1, XMSG, XSTAT1 )
                  ELSE
                    SENS_PER_STREAM( SENNUM ) = SENS_PER_STREAM( SENNUM ) + 1
                    STREAM_TO_SENS(SENNUM,SENS_PER_STREAM( SENNUM )) = NP
                  END IF
                END DO
              END IF
            END DO
#endif
         END IF  ! First Time

C Initialize Emissions Array
         VDEMIS_DIFF = 0.0
         VDEMIS_CONV = 0.0

#ifdef isam
         SA_VDEMIS_DIFF = 0.0
         SA_VDEMIS_CONV = 0.0
#endif

#ifdef sens
        SVDEMIS_DIFF = 0.0
#endif

C Retrieve Emissions from All Stream Types
         IF ( DESID_N_SRM .GT. 0 ) THEN

         DO ISRM = 1,DESID_N_SRM
           DO R = 1,NROWS
           DO C = 1,NCOLS
           DO J = 1,DESID_LAYS
           DO I = 1,DESID_N_ISTR
             VDEMIS_READ(I,J,C,R) = 0.0
           END DO
           END DO
           END DO
           END DO

           SELECT CASE ( DESID_STREAM_ITYPE( ISRM ) )
             
             ! Retrieve Gridded and Tracer Emissions
             CASE ( 1, 3 )
               NL = DESID_GRID_LAYS( ISRM )
               CALL GR3D( JDATE, JTIME, TSTEP, DESID_STREAM_NAME( ISRM ), 
     &                        NL, ISRM, VDEMIS_READ, L_DESID_DIAG ) ! mol/s and g/s
               
               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               
               ! Calculate Number and Surface Area Emissions for this
               ! stream. Convert aerosol mass emissions from g/s to
               ! ug/m3/s.
               CALL DESID_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve Point Source Emissions Streams
             CASE ( 2 )
               NL = 1
               CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, LOCAL_EMVAR( :,ISRM ), 
     &                              ISRM, VDEMIS_READ, NL, L_DESID_DIAG )! mol/s and g/s

               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

               ! Calculate Number and Surface Area Emissions for this
               ! stream. Convert aerosol mass emissions from g/s to
               ! ug/m3/s.
               CALL DESID_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             CASE ( 4 )
               NL = 1
               CALL GET_BEIS ( JDATE, JTIME, TSTEP, L_DESID_DIAG ) ! mol/s
               FORALL( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoEMVAR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = VDEMIS_BI( MAP_ISTRtoEMVAR( ISTR,ISRM ),:,: ) 
               END FORALL
               
               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Marine Gas Emissions
             CASE ( 5 )
               NL = 1
               CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID, L_DESID_DIAG )! mol/s

               FORALL( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoEMVAR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = VDEMIS_MG( MAP_ISTRtoEMVAR( ISTR,ISRM ),:,: ) 
               END FORALL

               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Lightning NO Emissions
             CASE ( 6 )
               NL = DESID_LAYS
               CALL GET_LTNG ( JDATE, JTIME, TSTEP, L_DESID_DIAG )  ! mol/s
               DO L = 1,NL
                  FORALL( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoEMVAR( ISTR,ISRM ) .GT. 0 )
                     VDEMIS_READ( ISTR,L,:,: ) = VDEMIS_LT( :,:,L ) 
                  END FORALL
               END DO
               
               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Sea Spray Aerosol Emissions 
             CASE ( 7 )
               NL = 1
               CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( :,:,1 ), 
     &                           CELLHGT( :,:,1 ), L_DESID_DIAG )  ! g/m3/s

               FORALL( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoEMVAR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = SSOUTM( MAP_ISTRtoEMVAR( ISTR,ISRM ),:,: ) 
               END FORALL

               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

               ! Calculate Number and Surface Area Emissions for this
               ! stream. Convert aerosol mass emissions from g/m3/s to
               ! ug/m3/s.
               CALL DESID_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve Wind-Blown Dust Emissions
             CASE ( 8 )
               NL = 1
               CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), 
     &                              CELLHGT( :,:,1 ), L_DESID_DIAG ) ! g/m3/s
               FORALL( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoEMVAR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = DUSTOUTM( MAP_ISTRtoEMVAR( ISTR,ISRM ),:,: ) 
               END FORALL
               
               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               
               ! Calculate Number and Surface Area Emissions for this
               ! stream. Convert aerosol mass emissions from g/m3/s to
               ! ug/m3/s.
               CALL DESID_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve MEGAN Emissions
             CASE ( 9 )
               NL = 1
               CALL GET_MEGAN ( JDATE, JTIME, TSTEP, L_DESID_DIAG) ! mol/s
               FORALL( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoEMVAR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = VDEMIS_ME( MAP_ISTRtoEMVAR( ISTR,ISRM ),:,: ) 
               END FORALL
               
               ! Apply Rules from DESID_${mech} namelist
               CALL DESID_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

           END SELECT
           
           ! Convert All Emissions to VDIFF Units (ppmv/s for gas and
           ! aerosol masses, m2/mol/s for aerosol surface area, and
           ! N/mol/s for aerosol number)
           CALL DESID_CONV_UNITS( VDEMIS_SCALED, NL, VDEMIS_CONV )
           
           ! Write out diagnostic file (if requested in DESID control
           ! namelist file). This subroutine will only be executed if
           ! DESID is called in diagnostic mode, meaning all input,
           ! scaling, and unit conversions are performed for input to
           ! VDIFF, but values are converted back to mol/s (gas), g/s
           ! (aerosol mass), m2/s (aerosol surface area), and N/s 
           ! (aerosol number) before output.
           IF ( L_DESID_DIAG ) CALL DESID_CALC_DIAG( JDATE, JTIME, VDEMIS_CONV, ISRM, NL )

           ! Tools like DDM3D has a need to calculate emissions from 
           ! specific streams and then not apply them to VDIFF. They
           ! should only be added if DESID_STREAM_LAPPLY is true for
           ! this stream.
           IF ( DESID_STREAM_LAPPLY( ISRM ) ) 
     &          VDEMIS_DIFF( :,1:NL,:,: ) = VDEMIS_DIFF( :,1:NL,:,: ) + VDEMIS_CONV( :,1:NL,:,: )

#ifdef isam
           IF ( DESID_STREAM_LAPPLY( ISRM ) ) THEN
             ! Populate SA_VDEMIS_CONV, array for carrying tagged
             ! emissions contributions

             ! Initialize 
             DO ITAG = 1,NTAG_SA
               DO R = 1,NROWS
                 DO C = 1,NCOLS
                   DO LAYER = 1,NL
                     DO SPC = 1,N_SPC_DIFF
                       SA_VDEMIS_CONV( SPC,LAYER,C,R,ITAG ) = 0.0
                     END DO
                   END DO
                 END DO
               END DO
             END DO
 
             ! Determine whether this emission stream corresponds to 
             ! any ISAM tags
             IF ( TAGS_PER_STREAM( ISRM ) .EQ. 0 ) THEN 
               ! Dump all into 'OTHRTAG'
               SA_VDEMIS_CONV( :,1:NL,:,:,OTHRTAG ) = 
     &                             VDEMIS_CONV( :,1:NL,:,: )
             ELSE
               ! Initialize Array for Tracking Residual of Tagged
               ! Emissions  
               SA_VDEMIS_CONV_OTHER( :,1:NL,:,: ) = VDEMIS_CONV( :,1:NL,:,: )

               ! Loop through all of the ISAM tags associated with this
               ! emissions stream. Generally each stream is only
               ! associated with one tag. The exception is when
               ! there are multiple tags corresponding to separate regions.
               DO IDX = 1, TAGS_PER_STREAM( ISRM )

                 ! Retrieve A Tag Associated with this Stream
                 ITAG = STREAM_TO_TAG(ISRM,IDX)

                 IF ( ISAMRGN_NUM( ITAG ) .LT. 1 ) THEN  
                   ! This tag is capturing 100% of every grid cell in
                   ! the full domain for this stream. We need to assign
                   ! all of the emissions from this stream to this tag
                   ! and the residual is then set to 0.
                   SA_VDEMIS_CONV( :,1:NL,:,:,ITAG ) = 
     &                  SA_VDEMIS_CONV( :,1:NL,:,:,ITAG )
     &                        + VDEMIS_CONV(:,1:NL,:,: )
                   SA_VDEMIS_CONV_OTHER = 0.0

                 ELSE
                   ! This tag is capturing some fraction of the stream
                   ! in grid cells across the domain. These fractional
                   ! contributions are applied using the DESID_REG_FAC
                   ! array with corresponding map indices.
                   ! Loop Over Altitude, Species and Regions to Add
                   ! emissions to corresponding tag, ITAG.
                   DO IRGN = 1, ISAMRGN_NUM( ITAG )
                     DO LAYER = 1, NL
                       DO SPC = 1, N_SPC_DIFF
                          SA_VDEMIS_CONV( SPC,LAYER,:,:,ITAG ) = 
     &                              SA_VDEMIS_CONV( SPC,LAYER,:,:,ITAG )
     &                             + VDEMIS_CONV( SPC,LAYER,:,: )
     &                               * DESID_REG_FAC(:,:,ISAMRGN_MAP(ITAG,IRGN))

                          SA_VDEMIS_CONV_OTHER( SPC,LAYER,:,: ) = 
     &                              SA_VDEMIS_CONV_OTHER( SPC,LAYER,:,: )
     &                            - VDEMIS_CONV( SPC,LAYER,:,: )
     &                              * DESID_REG_FAC(:,:,ISAMRGN_MAP(ITAG,IRGN))

                       END DO
                     END DO
                   END DO
                 END IF
               END DO
               
               ! Now that emissions from this stream have been assigned
               ! to all available tags, assess the remainder and assign
               ! it to the "OTHR" tag if any exists.
               MINCHECK = MINVAL( SA_VDEMIS_CONV_OTHER( :,1:NL,:,: ) )
               IF ( MINCHECK .GE. 0.0 ) THEN
                  ! Remainder is Valid. Move it all to the "OTHR" Tag
                  SA_VDEMIS_CONV( :,1:NL,:,:,OTHRTAG ) = 
     &                                SA_VDEMIS_CONV_OTHER( :,1:NL,:,: )
               ELSE
                  ! Some Part of the Remainder ("OTHER") is Invalid
                  XMSG = " ISAM mass balance error for emissions stream " //
     &                     TRIM( DESID_STREAM_DESC( ISRM ) ) // 
     &                    " - check isam control file. "
                  CALL M3WARN( 'ISAM_EMISSIONS', 1, 1, XMSG )

                  ! Repair OTHER By Distributing Negative Among Populated Tags 
                  ! and assigning positive part to the "OTHR" Tag as
                  ! planned
                  DO R = 1,NROWS
                  DO C = 1,NCOLS
                  DO LAYER = 1,NL
                  DO SPC = 1,N_SPC_DIFF
                     IF ( SA_VDEMIS_CONV_OTHER( SPC,LAYER,C,R ) .LT. 0.0 ) THEN
                        ! Find the total alloted to this (SPC,LAYER,C,R)
                        ! and use it to distribute the error fractionally
                        TOTAL = SUM( SA_VDEMIS_CONV( SPC,LAYER,C,R,: ) )
                        DO ITAG = 1,NTAG_SA 
                          SA_VDEMIS_CONV( SPC,LAYER,C,R,ITAG ) =
     &                      SA_VDEMIS_CONV( SPC,LAYER,C,R,ITAG ) *
     &                      ( 1.0 + SA_VDEMIS_CONV_OTHER( SPC,LAYER,C,R ) / TOTAL )
                        END DO
                     ELSE
                        ! Assign the positive values to the "OTHR" Tag
                        SA_VDEMIS_CONV( SPC,LAYER,C,R,OTHRTAG ) = 
     &                                SA_VDEMIS_CONV_OTHER( SPC,LAYER,C,R )
                     END IF
                  END DO
                  END DO
                  END DO
                  END DO
               END IF

             END IF

             ! Subset the emissions array for ISAM traced species  
             DO SPC = 1, NSPC_SA
               IF ( MAP_DIFFtoSA( SPC ) .NE. 0 ) THEN
                 DO ITAG = 1,NTAG_SA
                   DO R = 1,NROWS
                     DO C = 1,NCOLS
                       DO LAYER = 1,NL
                         SA_VDEMIS_DIFF( SPC,LAYER,C,R,ITAG ) =  
     &                                SA_VDEMIS_DIFF( SPC,LAYER,C,R,ITAG )
     &                             +  SA_VDEMIS_CONV( MAP_DIFFtoSA( SPC ),LAYER,C,R,ITAG )
                       END DO
                     END DO
                   END DO
                 END DO
               END IF
             END DO

           END IF
#endif


#ifdef sens
           IF ( SENS_PER_STREAM( ISRM ) .GT. 0 ) THEN ! This stream is used by DDM3D

             DO SENNUM = 1, SENS_PER_STREAM( ISRM )
               NP = STREAM_TO_SENS( ISRM,SENNUM )
               SVDEMIS_DIFF( :,1:NL,:,:,NP ) = SVDEMIS_DIFF( :,1:NL,:,:,NP ) 
     &                                       + VDEMIS_CONV(:,1:NL,:,: )
             END DO
           END IF
#endif

         END DO  ! Master Loop for Emissions Streams

#ifdef sens
C Subset the emissions array for DDM-3D (by species and region)
         DO NP = 1, NPMAX
           DO S = 1, N_SPC_DIFF
             DO L = 1, DESID_LAYS
               SVDEMIS_DIFF( S,L,:,:,NP ) = 
     &                        SVDEMIS_DIFF( S,L,:,:,NP )
     &                      * IREGION( :,:,L,NP )
     &                      * REAL( IPARM( NP, DIFF_MAP( S ) ), 4 )
             END DO
           END DO
         END DO
#endif

         ! Check for Negative Emissions after scaling rules and unit
         ! conversions are applied.
         ERROR_NEG     = DESID_CHECK_NEG( VDEMIS_DIFF, 1, DESID_LAYS )
         
         ! Write Out Aggregate Diagnostic Emissions to Files
         IF ( L_DESID_DIAG ) CALL DESID_WRITE_DIAG( JDATE, JTIME )
         
         END IF  ! DESID_N_SRM

         RETURN

         END SUBROUTINE DESID_GET_EMIS

C------------------------------------------------------------------------------
 
      SUBROUTINE GR3D ( JDATE, JTIME, TSTEP, DESID_FNAME, STREAM_LAYS, 
     &                  FSTREAM, VDEMIS, L_DESID_DIAG )

      USE CGRID_SPCS          ! CGRID mechanism species
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      USE CENTRALIZED_IO_MODULE, ONLY: interpolate_var
#ifdef mpas
         use util_module, only : nextime
#endif

      !USE EMIS_UTIL
      !USE UTILIO_DEFN

      IMPLICIT NONE

      ! Includes:
      INCLUDE SUBST_FILES_ID  ! file name parameters

      ! Arguments:
      INTEGER, INTENT( IN ) :: JDATE, JTIME         ! date (YYYYDDD), time (HHMMSS)
      INTEGER, INTENT( IN ) :: FSTREAM              ! Stream Counter
      CHARACTER( 100 ), INTENT( IN ) :: DESID_FNAME ! Stream Filename
      INTEGER, INTENT( IN ) :: TSTEP( 3 )           ! time step vector (HHMMSS)
      LOGICAL, INTENT( IN ) :: L_DESID_DIAG         ! Flag for diagnostic operation

      ! Output:
      REAL, INTENT( OUT )  :: VDEMIS( :,:,:,: )     ! Emission Rate Array

      ! Local Variables:
      INTEGER   C, R, L, N, S, V, ISTR 
      INTEGER   S_STRT, S_END, STREAM_LAYS, NDATE, NTIME

      REAL, ALLOCATABLE, SAVE :: BUFF( :,:,: )
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: PNAME = 'GR3D'
      CHARACTER( 200 ) :: XMSG = ' '
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, ALLOCATABLE, SAVE :: LOC_STDATE(:)
      LOGICAL         :: L_NEWDAY

      real*8 :: lDBL_CKSUM
      real :: lsum
      logical :: found
      integer :: mycount

!-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ! Get Domain Information for Emissions Read Routine

         ! Allocate Persistent Variables
         ALLOCATE( BUFF( NCOLS,NROWS,MAXVAL( DESID_GRID_LAYS ) ) )
         ALLOCATE( LOC_STDATE( SIZE(DESID_STREAM_DATE) ) )

         LOC_STDATE = STDATE
      END IF    !FirstTime
 
      ! Ensure that the model and emissions timestamp dates stay synchronized
      L_NEWDAY = .FALSE.
      IF (LOC_STDATE(FSTREAM) .NE. JDATE ) THEN 
         NDATE = JDATE; NTIME = JTIME
         CALL NEXTIME( NDATE, NTIME, -TSTEP( 1 ) )       ! go back one output tstep
         CALL NEXTIME( DESID_STREAM_DATE( FSTREAM ), NTIME, TSTEP( 1 ) ) ! advance the start date one time step
         LOC_STDATE(FSTREAM) = JDATE
         L_NEWDAY = .TRUE.
      END IF

      ! Read & Interpolate Emissions 
      DO R = 1,NROWS
      DO C = 1,NCOLS
      DO L = 1,DESID_LAYS
      DO ISTR = 1,DESID_N_ISTR
        VDEMIS(ISTR,L,C,R) = 0.0
      END DO
      END DO
      END DO
      END DO

      DO ISTR = 1, DESID_N_ISTR 
        VNAME = LOCAL_EMVAR( ISTR, FSTREAM )
        IF ( VNAME .EQ. '' ) CYCLE

        CALL INTERPOLATE_VAR (VNAME, DESID_STREAM_DATE( FSTREAM ), JTIME, BUFF, DESID_FNAME)

        ! Store all emissions in mol/sec or g/sec and convert to ppmv/s later
        IF ( DESID_LAYS .GE. STREAM_LAYS ) THEN 
          DO L = 1, STREAM_LAYS
             VDEMIS( ISTR,L,:,: ) = BUFF( :,:,L )
          END DO
        ELSE
          DO L = 1, DESID_LAYS
             VDEMIS( ISTR,L,:,: ) = BUFF( :,:,L )
          END DO
          DO L = DESID_LAYS+1,STREAM_LAYS
             VDEMIS( ISTR,DESID_LAYS,:,: ) = VDEMIS( ISTR,DESID_LAYS,:,: ) + BUFF( :,:,L )
          END DO
        END IF
      END DO   ! ISTR

      ! Reset the date of the gridded file if this is diagnostic mode
      ! and the day has advanced
      IF ( L_NEWDAY .AND. L_DESID_DIAG ) THEN
         CALL NEXTIME( DESID_STREAM_DATE( FSTREAM ), NTIME, -TSTEP(1) )
         LOC_STDATE(FSTREAM) = NDATE
      END IF

      RETURN

      END SUBROUTINE GR3D
 
C-----------------------------------------------------------------------
      SUBROUTINE DESID_SCALING( VDEMIS0, ISRM, NL, VDEMIS )

C     Apply region-dependent scaling of emissions rules.
C-----------------------------------------------------------------------
      USE CENTRALIZED_IO_MODULE, ONLY: MSFX2
          
      IMPLICIT NONE

      INTEGER, INTENT( IN )   :: ISRM, NL
      REAL, INTENT( IN )      :: VDEMIS0( :,:,:,: )
      REAL, INTENT( OUT )     :: VDEMIS ( :,:,:,: )

      REAL, ALLOCATABLE, SAVE    :: VDEMIS1( :,:,: )
      INTEGER, ALLOCATABLE, SAVE :: NFAC( : )

      INTEGER ISTR, IFAC, L, NFAC_MAX, IRGN, OP, JSRM, ISTR_TMP, 
     &        NISTR, REG_UNQ, JRGN, K
      INTEGER, ALLOCATABLE :: MAP_FACtoISTR( : )
      REAL, ALLOCATABLE, SAVE :: FAC( :,: )
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL :: LAREA, LAREAADJ


      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ALLOCATE( NFAC( DESID_N_ISTR ) )
         ALLOCATE( VDEMIS1( NLAYS, NCOLS, NROWS ) )
         ALLOCATE( FAC( NCOLS,NROWS ) )
      END IF   ! First Time

      !! Time-Dependent Portion
      VDEMIS( :,:,:,: ) = 0.0
      NFAC( : ) = DESID_FAC( :,ISRM )%NFAC
      
      ! Skip Streams With no Emissions Whatsoever
      IF ( SUM( NFAC(:) ) .EQ. 0 ) RETURN

      ! Loop through Each Instruction 
      DO ISTR = 1,DESID_N_ISTR
        ! Do Not bother with this row if there is no emission variable to map
        IF ( NFAC( ISTR ) .EQ. 0 ) CYCLE

        ! Loop through each unique Region and Process Instruction's Stacks
        DO JRGN = 1,DESID_FAC( ISTR,ISRM )%NREG
          REG_UNQ = DESID_FAC( ISTR,ISRM )%REG_UNQ( JRGN )

          ! Initialize Region-Dependent Array for this Instruction
          VDEMIS1( :,:,: ) = 0.

          ! Calculate the scale factor needed for this region
          DO IFAC = 1,NFAC( ISTR )      

            FAC(:,:)= DESID_FAC( ISTR,ISRM )%FAC( IFAC )
            OP      = DESID_FAC( ISTR,ISRM )%OP ( IFAC )
            IRGN    = DESID_FAC( ISTR,ISRM )%REG( IFAC )
            LAREA   = DESID_FAC( ISTR,ISRM )%AREA(IFAC )
            LAREAADJ= DESID_FAC( ISTR,ISRM )%AREAADJ(IFAC )

            IF ( IRGN .EQ. REG_UNQ .OR.
     &          DESID_REG_SUB( IRGN,REG_UNQ ) ) THEN
              !Apply scaling for region IRGN using DESID_FAC 

              IF ( OP .EQ. 1 ) THEN ! Add New Emission
#ifndef mpas
                 IF ( LAREA ) FAC = FAC * CELLAREA
                 IF ( LAREAADJ ) FAC = FAC / MSFX2
#endif
                 DO L = 1,NL
                   VDEMIS1( L,:,: ) = VDEMIS1( L,:,: ) +
     &                  VDEMIS0( ISTR,L,:,: ) * FAC
                 END DO

              ELSE IF ( OP.EQ.2 ) THEN ! Multiply Emissions Rule
                 DO L = 1,NL
                   VDEMIS1( L,:,: ) = VDEMIS1( L,:,: ) * FAC
                 END DO

              ELSE IF ( OP.EQ.3 ) THEN ! Overwrite Existing Rule
#ifndef mpas
                 IF ( LAREA ) FAC = FAC * CELLAREA
                 IF ( LAREAADJ ) FAC = FAC / MSFX2
#endif
                 DO L = 1,NL
                   VDEMIS1( L,:,: ) = VDEMIS0( ISTR,L,:,: ) * FAC
                 END DO
             END IF
            END IF
          END DO  ! End Loop through instruction stack

          ! Determine Region Mask to use for this Instruction
          K = DESID_FAC( ISTR,ISRM)%REG_RMDR( JRGN )
          IF ( K .EQ. 0 ) THEN
             ! This region does not have active subsets for this instruction
             ! Integrate Emissions Accounting for Region-based contributions
             DO L = 1,NL
               VDEMIS( ISTR,L,:,: ) = VDEMIS( ISTR,L,:,: ) + 
     &             VDEMIS1( L,:,: ) * DESID_REG_FAC(:,:,REG_UNQ )
             END DO
          ELSE
             ! This region has active subsets for this instruction. Use the 
             ! remainder mask 
             ! Integrate Emissions Accounting for Region-based contributions
             DO L = 1,NL
               VDEMIS( ISTR,L,:,: ) = VDEMIS( ISTR,L,:,: ) + 
     &             VDEMIS1( L,:,: ) * DESID_REG_RMDR( K )%MASK(:,: )
             END DO
          END IF


        END DO    ! End loop through unique regions

      END DO      ! End loop through instructions

      RETURN

      END SUBROUTINE DESID_SCALING


C-----------------------------------------------------------------------
      SUBROUTINE DESID_CONV_UNITS( VDEMIS0, NL, VDEMIS )

C     Convert rate and map to Diffusivity module species order
C-----------------------------------------------------------------------
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      USE AERO_EMIS           ! inherits GRID_CONF
#ifdef mpas
      USE coupler_module, ONLY: inv_cell_vol
#endif

      IMPLICIT NONE

      ! Local Variables
      REAL,ALLOCATABLE, SAVE :: CNVTC( :,:,: ) ! combined conversion factor
      REAL,ALLOCATABLE, SAVE :: CNVTI( : )     ! intermediate combined conv. factor
      REAL,       SAVE :: CNVTP                ! intermediate combined conv. factor
      REAL,ALLOCATABLE, SAVE :: CONVM( :,:,: ) ! Aerosol Mass and Surface Area conversion factor         
      REAL,ALLOCATABLE, SAVE :: CONVN( :,:,: ) ! Aerosol Number conversion factor       
      REAL,ALLOCATABLE, SAVE :: CNVTI_M( :,:,: ) ! intermediate combined conv. factor
      REAL, PARAMETER  :: GPKG = 1.0E+03       ! g/kg
      REAL, PARAMETER  :: MWAIR = 28.9628      ! g/mol
      REAL, PARAMETER  :: AVO  = 6.0221367E23
      REAL, PARAMETER  :: RAVO = 1.0 / AVO
      
      INTEGER, INTENT( IN ) :: NL
      REAL, INTENT( INOUT ) :: VDEMIS0( :,:,:,: )
      REAL, INTENT( OUT ):: VDEMIS ( :,:,:,: )
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER       :: C, R, L, ISTR, INDX

      ! Get domain decomp info from the emissions file
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ! Populate Persistent Variables
#ifndef mpas
         CNVTP = 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 
#endif

         ALLOCATE( CNVTI( DESID_LAYS ), CNVTC( DESID_LAYS,NCOLS,NROWS ), 
#ifdef mpas
     &             CNVTI_M( NCOLS,NROWS,DESID_LAYS ),
#endif
     &             CONVM( DESID_LAYS,NCOLS,NROWS ),
     &             CONVN( DESID_LAYS,NCOLS,NROWS )  )
      END IF    !FirstTime
      
      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
#ifdef mpas
      CNVTI_M( :,1,1:NL ) = 1.0E+06 * MWAIR * inv_cell_vol(:,1,1:NL)
#else
      CNVTI( 1:NL ) = CNVTP * Grid_Data%RDX3F( 1:NL )
#endif

      DO L = 1,NL
#ifdef mpas
        CNVTC( L,:,: ) = 1.0E-3 * CNVTI_M( :,:,L ) * Met_Data%RRHOJ( :,:,L )  ! Gas Moles:   mol/s -> ppmv/s
#else
        CNVTC( L,:,: ) = 1.0E-3 * CNVTI( L ) * Met_Data%RRHOJ( :,:,L )        ! Gas Moles:   mol/s -> ppmv/s
#endif
        CONVM( L,:,: ) = MWAIR / GPKG / Met_Data%DENS( :,:,L ) !m3/mol  ! Aer. Mass:   umol/m3/s -> ppmv/s
      END DO                                                            ! Aer. Surf:   m2/m3/s -> m2/mol/s
      CONVN( 1:NL,:,: ) = CONVM( 1:NL,:,: )                             ! Aer. Num:    N/m3/s -> N/mol/s

      !MAKE SURE TO APPLY RELEVANT UNIT CONVERSION TO EACH
      !TYPE OF VARIABLE (GAS, AEROSOL MASS, AEROSOL NUMBER,
      !AND AEROSOL SURFACE AREA)
      FORALL ( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoGAS( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )
  
      FORALL ( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoAERO( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )
      
      FORALL ( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoSRF( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )
      
      FORALL ( ISTR = 1:DESID_N_ISTR, MAP_ISTRtoNUM( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CONVN( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )

 
      ! zero out emissions values for diffused species not included in emissions list.
      ! ...accounts for emissions species names as a subset of the vert. diffused species list
      DO L = 1,NL
        VDEMIS( :,L,:,: ) = 0.0

        DO ISTR = 1,DESID_N_ISTR 
          IF ( MAP_ISTRtoDIFF( ISTR ) .NE. 0 ) 
     &        VDEMIS( MAP_ISTRtoDIFF( ISTR ),L,:,: ) = 
     &                VDEMIS( MAP_ISTRtoDIFF( ISTR ),L,:,: ) + 
     &                VDEMIS0( ISTR,L,:,: )
        END DO
      END DO

      RETURN
      END SUBROUTINE DESID_CONV_UNITS


!-----------------------------------------------------------------------
      FUNCTION DESID_CHECK_NEG( VDEMIS, ISRM, NL ) RESULT( STAT )

!     Check Emissions Array for negative values and exit if negative values
!     exceed tolerance of 1.0E-7. Emissions that are negative but within 
!     tolerance are set to 0.0.
!     VDEMIS is the scaled emissions and VDEMIS0 is the emissions
!     read in from each stream.
!-----------------------------------------------------------------------
      USE UTILIO_DEFN
      USE VDIFF_MAP, only : DIFF_SPC ! Name for every Diffusion Module Species 

      IMPLICIT NONE

      REAL, INTENT( INOUT ) :: VDEMIS ( :,:,:,: )
      INTEGER, INTENT( IN ) :: NL
      INTEGER, INTENT( IN ) :: ISRM

      INTEGER :: ISPC
      INTEGER :: STAT
      INTEGER :: ALOC( 4 )
      REAL    :: EMIS_MIN
      CHARACTER(250) :: XMSG

      STAT = 0

      ! If All Values Are Positive then Return Right Away
      EMIS_MIN = MINVAL( VDEMIS( :,1:NL,:,: ) )
      IF ( EMIS_MIN .GE. 0.0 ) RETURN

      IF ( EMIS_MIN .LT. -1.0e-7 ) THEN

          STAT = 1

          ! Find Where the Most Negative Value Is and Exit
          ALOC = MINLOC( VDEMIS( :,1:NL,:,: ) )
          ISPC = ALOC( 1 )

          WRITE( LOGDEV, '(/,5x,A,ES10.3,A,/,5x,3A,/,5x,A,/,5x,A)' ),
     &      'ERROR: Invalid Negative emission rate ', EMIS_MIN, ' has been ',
     &      ' detected for CMAQ species ',TRIM(DIFF_SPC( ISPC )),'.',
     &      'Please inspect the Emission Control Namelist File (Search for ',
     &      '"Reading Emission Control Namelist" in this Log File).'
          XMSG = 'Negative Emissions Detected'
          CALL M3EXIT( 'EMISS_NEG_CHECK', 0, 0, XMSG, XSTAT1 )

      ELSE ! reset slightly negative values to 0.0
          WHERE ( VDEMIS .LT. 0.0 ) VDEMIS = 0.0
      END IF

      END FUNCTION DESID_CHECK_NEG    

!-----------------------------------------------------------------------
      SUBROUTINE DESID_CALC_DIAG( JDATE, JTIME, VDEMIS0, ISRM, NL ) 

!     Write out emissions diagnostic file
!-----------------------------------------------------------------------
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      USE AERO_EMIS           ! inherits GRID_CONF
      USE GRID_CONF
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_MASK_GAS, DIFF_MASK_AERO,
     &                      DIFF_MASK_NUM, DIFF_MASK_SRF, DIFF_MW, DIFF_SPC,
     &                      DIFF_MASK_NR, DIFF_MASK_TRAC
      USE AERO_DATA, ONLY : GPKG
#ifdef mpas
      USE coupler_module, ONLY: inv_cell_vol
#endif

      IMPLICIT NONE
         
      INCLUDE SUBST_CONST     ! constants

      INTEGER, INTENT( IN ) :: JDATE, JTIME, ISRM
      REAL, INTENT( In )    :: VDEMIS0( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: VDEMIS ( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: VDEMIS_OUT ( :,:,: )

      ! Local Variables
      REAL, PARAMETER :: RAVO = 1.0 / AVO
      REAL,ALLOCATABLE, SAVE :: CNVTC( :,:,: ) ! combined conversion factor
      REAL,ALLOCATABLE, SAVE :: CNVTI( : )     ! intermediate combined conv. factor
      REAL,ALLOCATABLE, SAVE :: CNVTI_M( :,:,: ) ! intermediate combined conv. factor for mpas
      REAL,       SAVE :: CNVTP                ! intermediate combined conv. factor
      REAL,ALLOCATABLE, SAVE :: CONVM( :,:,: ) ! Aerosol Mass and Surface Area conversion factor         
      REAL,ALLOCATABLE, SAVE :: CONVN( :,:,: ) ! Aerosol Number conversion factor       
 
      INTEGER       :: LAYS, L, C, R, IVAR, NL, I
      INTEGER       :: ISPEC, IDIAG, IP, JSPEC, JDIAG, KSPEC

      LOGICAL :: FIRST_TIME = .TRUE.
      CHARACTER( 586 ) :: XMSG

      ! Get pressure info from ASX_DATA_MOD
      IF ( FIRST_TIME ) THEN
         FIRST_TIME = .FALSE.
#ifndef mpas
         CNVTP = 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 
#endif

         ALLOCATE( CNVTI( DESID_LAYS ), CNVTC( DESID_LAYS,NCOLS,NROWS ), 
     &             CONVM( DESID_LAYS,NCOLS,NROWS ),
#ifdef mpas
     &             CNVTI_M( NCOLS,NROWS,DESID_LAYS ),
#endif
     &             CONVN( DESID_LAYS,NCOLS,NROWS ),
     &             VDEMIS( N_SPC_DIFF,DESID_LAYS,NCOLS,NROWS ),
     &             VDEMIS_OUT( NCOLS,NROWS,DESID_LAYS ) )
      END IF    !FirstTime
      
      VDEMIS     = 0.0
      VDEMIS_OUT = 0.0

      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
#ifdef mpas
      CNVTI_M( :, 1, 1:NL ) = 1.0E+06 * MWAIR * inv_cell_vol(:, 1, 1:NL)
#else
      CNVTI( 1:NL ) = CNVTP * Grid_Data%RDX3F( 1:NL )
#endif
      DO L = 1,NL
#ifdef mpas
        CNVTC( L,:,: ) = 1.0E3 / ( CNVTI_M( :,:,L ) * Met_Data%RRHOJ( :,:,L ) )   ! Gas Moles: ppmv/s -> mol/s
#else
        CNVTC( L,:,: ) = 1.0E3 / ( CNVTI( L ) * Met_Data%RRHOJ( :,:,L ) )         ! Gas Moles: ppmv/s -> mol/s
#endif
        CONVM( L,:,: ) = (1.0 / MWAIR) * GPKG * Met_Data%DENS( :,:,L )    ! Aer. Mass: ppmv/s -> umol/s
     &                   / Met_Data%RJACM( :,:,L ) * CELLVOL( :,:,L )     ! Aer. Surf: m2/mol/s -> m2/s
      END DO                                                              ! Aer. Num:  N/mol/s -> N/s  
      CONVN( 1:NL,:,: ) = CONVM( 1:NL,:,: )                               

      !MAKE SURE TO APPLY RELEVANT UNIT CONVERSION TO EACH
      !TYPE OF VARIABLE (GAS, AEROSOL MASS, AEROSOL NUMBER,
      !AND AEROSOL SURFACE AREA)
      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_GAS( I ) .OR.
     &                           DIFF_MASK_NR( I )  .OR.
     &                           DIFF_MASK_TRAC( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:)

      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_AERO( I ) .AND.
     &                           .NOT. DIFF_MASK_NUM( I ) .AND.
     &                           .NOT. DIFF_MASK_SRF( I )  )
     &    VDEMIS( I,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:) * DIFF_MW( I )

      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_SRF( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:)

      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_NUM( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CONVN( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:)

      ! Write out this stream to its diagnostic file or sum emissions
      ! for an aggregate diagnostic
      DO IDIAG = 1,DESID_N_DIAG
        IF ( DESID_DIAG_STREAM_MASK( ISRM,IDIAG ) ) THEN
           ! This Stream Contributes to IDIAG diagnostic
           LAYS = DESID_DIAG_LAYS( IDIAG )

           ! Loop over all diagnostic species
           DO ISPEC = 1,DESID_DIAG_SPEC( IDIAG )%NSPEC

              ! Sum up all of the diffused species relevant for this
              ! diagnostic species
              VDEMIS_OUT( :,:,: ) = 0.0
              DO IP = 1,DESID_DIAG_SPEC( IDIAG )%NPAIRS
                IF ( DESID_DIAG_SPEC( IDIAG )%MAP_toDIAG( IP )
     &                .EQ. ISPEC ) THEN
                  JSPEC = DESID_DIAG_SPEC( IDIAG )%MAP_toDIFF( IP )
                  DO L = 1,NL
                     VDEMIS_OUT( :,:,L ) = 
     &                   VDEMIS_OUT( :,:,L ) + VDEMIS( JSPEC,L,:,: )
                  END DO
                END IF
              END DO

              IF ( DESID_DIAG_FORMAT( IDIAG ) .EQ. 'COLSUM' )
     &             VDEMIS_OUT(:,:,1) = SUM( VDEMIS_OUT( :,:,1:NL ),3 )

              IF ( DESID_DIAG_N_STREAM( IDIAG ) .EQ. 1 ) THEN
                ! Only this stream contributes. Write emisison
                ! rates out directly
#ifndef mpas
                IF ( .NOT. WRITE3( DESID_DIAG_LOGICAL(IDIAG), DESID_DIAG_SPEC(IDIAG)%SPEC(ISPEC),
     &               JDATE, JTIME, VDEMIS_OUT( :,:,1:LAYS ) ) ) THEN
                   XMSG = 'Could not write ' // TRIM( DESID_DIAG_FILENAME(IDIAG) ) // ' file'
                   CALL M3EXIT( 'WRITE_EMISS_DIAG', JDATE, JTIME, XMSG, XSTAT1 )
                END IF
#endif
              ELSE
                ! Other streams contribute as well. Sum these emission
                ! rates to an aggregate array, VDEMIS_DIAG
                KSPEC = MAP_DIAGtoVDEMIS( ISPEC,IDIAG )
                VDEMIS_DIAG( KSPEC,:,:,1:LAYS ) = VDEMIS_DIAG( KSPEC,:,:,1:LAYS ) + 
     &                                            VDEMIS_OUT( :,:,1:LAYS )
              END IF
           END DO
        END IF
      END DO

      END SUBROUTINE DESID_CALC_DIAG
 
!-----------------------------------------------------------------------
      SUBROUTINE DESID_WRITE_DIAG( JDATE, JTIME ) 

!     Write out emissions diagnostic file
!-----------------------------------------------------------------------
          
          IMPLICIT NONE

          INTEGER IDIAG, ISPEC, JSPEC
          INTEGER LAYS
          INTEGER JDATE, JTIME
          CHARACTER(200) :: XMSG

#ifdef mpas
          CHARACTER (20) :: TIME_STAMP
          call mio_time_format_conversion (jdate, jtime, time_stamp)
#endif

          DO IDIAG = 1,DESID_N_DIAG

            IF ( DESID_DIAG_N_STREAM( IDIAG ) .GT. 1 ) THEN
               LAYS = DESID_DIAG_LAYS( IDIAG )


               DO ISPEC = 1,DESID_DIAG_SPEC( IDIAG )%NSPEC
                  ! Write out the emissions data for this aggregate
                  ! diagnostic
                  JSPEC = MAP_DIAGtoVDEMIS( ISPEC,IDIAG )
#ifndef mpas
                  IF ( .NOT. WRITE3( DESID_DIAG_LOGICAL(IDIAG), DESID_DIAG_SPEC( IDIAG )%SPEC( ISPEC ),
     &                     JDATE, JTIME, VDEMIS_DIAG( JSPEC,:,:,1:LAYS ) ) ) THEN
                    XMSG = 'Could not write ' // TRIM( DESID_DIAG_FILENAME(IDIAG) ) // ' file'
                    CALL M3EXIT( 'WRITE_EMISS_DIAG', JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
#else
             call mio_fwrite('EMIS_DIAG',DESID_DIAG_SPEC( IDIAG )%SPEC(ISPEC ), "write_emis",
     &                             VDEMIS_DIAG( JSPEC,:,1,1 ) ,time_stamp)
#endif
               END DO
            END IF
         END DO   


      END SUBROUTINE DESID_WRITE_DIAG

C-----------------------------------------------------------------------
      SUBROUTINE DESID_PROCESS_RULES( JDATE, JTIME )

C Check the chemical species from the namelists and AERO_DATA against
C the species that are available on the actual emissions input files. If
C they do not agree, print warnings or crash the program depending on
C how severe the error is.
C
C    16 Mar 2017  B.Murphy     Created Subroutine
C    10 Sep 2017  B.Murphy     Revised Emissions Mapping Approach
C    08 Nov 2017  B.Murphy     Vectorized Emission Maps to allow for 
C                                unlimited emissions streams
C-----------------------------------------------------------------------

         USE VDIFF_MAP, only : DIFF_SPC, ! Name for every Diffusion Module Species                                  
     &                         DIFF_MASK_GAS, DIFF_MASK_AERO, DIFF_MASK_NR, 
     &                         DIFF_MASK_TRAC, DIFF_MW
         USE UTILIO_DEFN
         USE AERO_DATA, only : N_MODE, AEROSPC,  ! Aerosol Properties Table
     &                         N_AEROSPC, AEROMODE, MODESUFF, DESID_AERO_REF
         USE AERO_EMIS, only : MAP_NUMtoISTR, MAP_SRFtoISTR, MAP_ISTRtoAERO, MAP_ISTRtoMODE, 
     &                         MAP_ISTRtoNUM, MAP_ISTRtoSRF, MAP_ISTRtoSD,
     &                         DESID_STREAM_AERO, SD_SPLIT, DESID_INIT_SIZE_DIST
         USE UDTYPES,   only : CARRY1, LARRY1
         USE UTIL_FAMILY_MODULE
#ifdef mpas
         use util_module, only : index1, upcase
#endif

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME
         INTEGER    :: N_UNUSED, REGNUM, N_USED

         INTEGER    :: STRT, PTSTRT, ISPC, IDX, IX, IEMVAR, IRULE, IAREA,
     &                 N, IM, IAERO, V, ISRM, NSPC, IA, IDIFF, JDX, JM,
     &                 ISTR, IEM, KDX, JSD, JEM, ISD,
     &                 IFAC, IRGN, ISTRN, NFAC, NREG, NCHEM, ICHEM, IFAM, 
     &                 IRGN2, N_EMVAR_CATCH, ICATCH, F, N_REG_RMDR, JRGN, K,
     &                 KRGN, I, J, JFAC
         REAL       :: AERO_SPLIT, UNIT_FAC_1, UNIT_FAC_2, EMVAR_MW, 
     &                 SPEC_MW, BASIS_FAC, FAC

         LOGICAL    :: LERROR, LFOUND, L_WDIFF, L_WISD, LTEST
         LOGICAL    :: LGAS_DIFF, LGAS_EMVAR, L_CATCH, LSUBSUB
         
         CHARACTER( 16 )  :: SPECNAME, SN, SM, EMVAR_CATCH( 200 )

         CHARACTER( 16 )  :: PNAME = 'DESID_PROCESS_RULES'
         CHARACTER( 100)  :: VARDESC
         INTEGER          :: STATUS

         CHARACTER( 500 ) :: XMSG
         CHARACTER( 16 )  :: B
         CHARACTER( 20 )  :: REFNAME
         
         INTEGER, PARAMETER :: NI0 = 3000
         LOGICAL, ALLOCATABLE, SAVE :: RULE_STREAM( : ), RULE_SPEC( : )
         TYPE( LARRY1 ), ALLOCATABLE, SAVE :: RULE_EMVAR( : ), RULE_PHASE( : )
         INTEGER, PARAMETER :: N_SCALEFAC = 100
         LOGICAL            :: LSPEC_KEY, LEMVAR_KEY
         CHARACTER( 16 )    :: CHEM_NAME( 150 )
         LOGICAL            :: LERROR2, LERROR3( 150 )
        
         INTEGER            :: IC, N_TASKS
         INTEGER, PARAMETER :: NT0 = 4000000
         INTEGER            :: TASK_IDIFF( NT0 )
         INTEGER            :: TASK_ISRM ( NT0 )
         INTEGER            :: TASK_IEMVAR( NT0)
         CHARACTER( 16 )    :: TASK_SPEC ( NT0 )
         CHARACTER( 16 )    :: TASK_EMVAR( NT0 )
         INTEGER            :: TASK_PHASE( NT0 )

         REAL, ALLOCATABLE :: LOCAL_FAC( :,:,: )
         REAL, ALLOCATABLE :: LOCAL_FAC_BULK( :,:,: )
         CHARACTER( 1 ), ALLOCATABLE :: LOCAL_OP( :,:,: )
         CHARACTER( 4 ), ALLOCATABLE :: LOCAL_BASIS( :,:,: )
         REAL,  ALLOCATABLE          :: LOCAL_CONV( :,:,: )
         INTEGER, ALLOCATABLE :: LOCAL_REG( :,:,: )
         INTEGER              :: N_RULE, N_AREA
         LOGICAL              :: LREMOVE, LERROR4
         INTEGER, ALLOCATABLE :: REG_UNQ( : )
         LOGICAL, ALLOCATABLE :: LSUB( : )
         LOGICAL              :: LREG_RMDR
         REAL, ALLOCATABLE    :: RMDR_MASK( :,: )

C Retrieve Environment Variable Letting User Ignore this Check
C and allowing the model to proceed.
         CALL LOG_SUBHEADING( LOGDEV, "Check Emissions Mapping" )

         WRITE( LOGDEV, '(/,/,5x,A)' ), REPEAT( '=', 77 )
         WRITE( LOGDEV, '(5x,A,A)' ), '|> SCALING EMISSIONS CONSISTENT WITH ',
     &                  'EMISSIONS CONTROL FILE SUPPLIED BY USER' 
         WRITE( LOGDEV, '(5x,A)' ), REPEAT( '=', 77 )
 
! Write Out Region Diagnostic Information
         WRITE( LOGDEV, '(/,5x,A)' ),'|> Regions Available for Scaling:'
         WRITE( LOGDEV, '(5x,A)'   ),'================================='

         ! Print Information about All the Available Regions for Scaling
         WRITE( LOGDEV,'(8x,A,2x,A,8x,A,10x,A)' ),'Number','Region Label','File Label','Variable'
         WRITE( LOGDEV,'(8x,A,2x,A,8x,A,10x,A)' ),'------','------------','----------','--------'
         DO IRGN = 1,DESID_N_REG
            WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),IRGN, DESID_REG(IRGN)%LABEL( 1:18 ),
     &             DESID_REG( IRGN )%File( 1:18 ), TRIM(DESID_REG( IRGN )%VAR ) 
            IF ( DESID_REG(IRGN)%FILE(1:6) .EQ. 'Family' ) THEN
                 F = INDEX1( DESID_REG(IRGN)%LABEL(1:18), DESID_N_REG_FAMS, REGIONFAMILYNAME )
                 DO IRGN2 = 1,REGIONFAMILYNUM( F )
                     WRITE( LOGDEV,'(56x,A)' ), REGIONFAMILYMEMBERS( F,IRGN2 )
                 END DO
            END IF
         END DO

! Retrieve the Emission Variables Available From Emissions Streams
         ! Load Default Molecular Weights and Units based on 
         ! SMOKE/MOVES/SPECIATE specifications

         WRITE( LOGDEV, '(/,5x,A)' ),'|> Map Available Emissions Variables to Defaults:'
         WRITE( LOGDEV, '(5x,A)'   ),'=================================================='
         N_EMVAR_CATCH = 0
         DO ISRM = 1,DESID_N_SRM

            IF ( ISRM .EQ. ISEASRM .OR. ISRM .EQ. IBIOSRM .OR. 
     &           ISRM .EQ. IDUSTSRM.OR. ISRM .EQ. ILTSRM  .OR.
     &           ISRM .EQ. IMGSRM  .OR. ISRM .EQ. IMIOGSRM ) CYCLE

            ALLOCATE( DESID_EMVAR( ISRM )%MW      ( DESID_EMVAR( ISRM )%LEN ) )
            ALLOCATE( DESID_EMVAR( ISRM )%USED    ( DESID_EMVAR( ISRM )%LEN ) )
            ALLOCATE( DESID_EMVAR( ISRM )%CONV    ( DESID_EMVAR( ISRM )%LEN ) )
            ALLOCATE( DESID_EMVAR( ISRM )%BASIS   ( DESID_EMVAR( ISRM )%LEN ) )
            ALLOCATE( DESID_EMVAR( ISRM )%LAREA   ( DESID_EMVAR( ISRM )%LEN ) )
            ALLOCATE( DESID_EMVAR( ISRM )%LAREAADJ( DESID_EMVAR( ISRM )%LEN ) )
            DESID_EMVAR( ISRM )%USED    = .FALSE.
            DESID_EMVAR( ISRM )%LAREA   = .FALSE.
            DESID_EMVAR( ISRM )%LAREAADJ= .FALSE.

            DO IEMVAR = 1,DESID_EMVAR( ISRM )%LEN
               ! Assign Default Molecular Weight to Each Emision Variable
               B = DESID_EMVAR( ISRM )%ARRY( IEMVAR )
               IA  = INDEX1( B, DESID_N_EMVAR_TABLE, DESID_EMVAR_TABLE( : )%NAME )
               IF ( IA .GT. 0 ) THEN
                  DESID_EMVAR( ISRM )%MW   ( IEMVAR ) = DESID_EMVAR_TABLE( IA )%MW
               ELSE
                  ! Emission Variable is not calculated online nor does it
                  ! belong to the default list of commonly used
                  ! variables.
                  DESID_EMVAR( ISRM )%MW   ( IEMVAR ) = 1.0

                  ! Only Write a Note to the User if this species hasn't
                  ! been caught from a different stream already
                  IF ( N_EMVAR_CATCH .EQ. 0 .OR.
     &                 INDEX1( B, 200, EMVAR_CATCH ) .EQ. 0 ) THEN
                     N_EMVAR_CATCH = N_EMVAR_CATCH + 1
                     EMVAR_CATCH( N_EMVAR_CATCH ) = B
                  END IF
               END IF
            END DO

         END DO

         WRITE( LOGDEV, '(/,5x,A)' ),'|> Checking Emissions Em. Var. Units: '
         WRITE( LOGDEV, '(5x,A)'   ),'======================================'
         DO ISRM = 1,DESID_N_SRM
            DO IEMVAR = 1,DESID_EMVAR( ISRM )%LEN
               ! Check Units and Assign Conversion Factors for
               ! translating [kmol or umol] -> mol, [kg or mg] -> g, and
               ! [s, min, hr] -> s
               CALL CHECK_EMIS_UNITS( ISRM, IEMVAR, 
     &                                DESID_EMVAR( ISRM )%ARRY ( IEMVAR ),
     &                                DESID_EMVAR( ISRM )%UNITS( IEMVAR ), 
     &                                DESID_EMVAR( ISRM )%CONV ( IEMVAR ),
     &                                DESID_EMVAR( ISRM )%BASIS( IEMVAR ),
     &                                DESID_EMVAR( ISRM )%LAREA( IEMVAR ) )
               ! Default for detecting area fluxes is to adjust to
               ! projected grid
               IF ( DESID_EMVAR( ISRM )%LAREA( IEMVAR ) ) 
     &              DESID_EMVAR( ISRM )%LAREAADJ( IEMVAR ) = .TRUE.
            END DO
         END DO

! Process Area Normalization User Controls. Override Automatic defaults
! with forced area normalization and/or projection adjustment.
         N_AREA = 0
         DO IAREA = 1,SIZE( DESID_AREA_NML )
            IF( DESID_AREA_NML( IAREA )%STREAM .EQ. '' ) EXIT
            N_AREA = IAREA
         END DO

         ALLOCATE( RULE_STREAM( DESID_N_SRM   ) )
         DO IAREA = 1,N_AREA
            LREMOVE = .FALSE.
            CALL DESID_GET_RULE_STREAMS( DESID_AREA_NML( IAREA )%STREAM, 
     &              IAREA, RULE_STREAM, LREMOVE, LERROR4 )
            IF ( LREMOVE ) CYCLE

            ! If the 'ALL' keyword was used, set all of the online
            ! sources to false.
            IF ( DESID_AREA_NML(IAREA)%STREAM .EQ. 'ALL' ) THEN
               IF ( ISEASRM .GT. 0 )  RULE_STREAM( ISEASRM  )  = .FALSE. 
               IF ( IBIOSRM .GT. 0 )  RULE_STREAM( IBIOSRM  )  = .FALSE. 
               IF ( IDUSTSRM .GT. 0 ) RULE_STREAM( IDUSTSRM  ) = .FALSE. 
               IF ( ILTSRM .GT. 0 )   RULE_STREAM( ILTSRM  )   = .FALSE. 
               IF ( IMGSRM .GT. 0 )   RULE_STREAM( IMGSRM  )   = .FALSE. 
               IF ( IMIOGSRM .GT. 0 ) RULE_STREAM( IMIOGSRM  ) = .FALSE. 
            ENDIF   
               
            DO ISRM = 1,DESID_N_SRM
               IF ( RULE_STREAM( ISRM ) ) THEN
                  DO IEMVAR = 1,DESID_EMVAR( ISRM )%LEN
                     ! Override Area Normalization if Requested
                     IF ( DESID_AREA_NML( IAREA )%AREA .EQ. 'TRUE' ) THEN
                         DESID_EMVAR( ISRM )%LAREA( IEMVAR ) = .TRUE.
                     ELSEIF (DESID_AREA_NML( IAREA )%AREA .EQ. 'FALSE' ) THEN
                         DESID_EMVAR( ISRM )%LAREA( IEMVAR ) = .FALSE.
                     END IF
                     ! Override Area Adjustment if Requested
                     IF ( DESID_AREA_NML( IAREA )%ADJ .EQ. 'TRUE' ) THEN
                         DESID_EMVAR( ISRM )%LAREAADJ( IEMVAR ) = .TRUE.
                     ELSEIF (DESID_AREA_NML( IAREA )%ADJ .EQ. 'FALSE' ) THEN
                         DESID_EMVAR( ISRM )%LAREAADJ( IEMVAR ) = .FALSE.
                     END IF
                  END DO
               END IF
            END DO

         END DO
 
! Write Out All Available Stream Families to the Log File
         WRITE( LOGDEV, '(/,5x,A)' ),'|> Emission Stream Family Definitions:'
         WRITE( LOGDEV, '(5x,A)'   ),'======================================'
         WRITE( LOGDEV,'(8x,A19,15x,A)' ),'Stream Family Label','Stream Family Members'
         WRITE( LOGDEV,'(8x,A19,15x,A)' ),'-------------------','---------------------'
         DO IFAM = 1,DESID_N_STREAM_FAMS
            WRITE( LOGDEV,'(8x,A32,2x,A)' ), STREAMFAMILYNAME( IFAM ), 
     &             STREAMFAMILYMEMBERS( IFAM,1 )
            IF ( STREAMFAMILYNUM( IFAM ) .GT. 1 ) THEN
                DO ISRM = 2,STREAMFAMILYNUM( IFAM )
                   WRITE( LOGDEV,'(42x,A)' ), STREAMFAMILYMEMBERS( IFAM,ISRM )
                END DO
            END IF
         END DO

! Write Out All Available Species Families to the Log File
         WRITE( LOGDEV, '(/,5x,A)' ),'|> CMAQ Species Family Definitions:'
         WRITE( LOGDEV, '(5x,A)'   ),'======================================'
         WRITE( LOGDEV,'(8x,A20,14x,A)' ),'Species Family Label','Species Family Members'
         WRITE( LOGDEV,'(8x,A20,14x,A)' ),'--------------------','---------------------'
         DO IFAM = 1,N_CHEM_FAMS
            WRITE( LOGDEV,'(8x,A32,2x,A)' ), CHEMFAMILYNAME( IFAM ), 
     &             CHEMFAMILYMEMBERS( IFAM,1 )
            IF ( CHEMFAMILYNUM( IFAM ) .GT. 1 ) THEN
                DO ISRM = 2,CHEMFAMILYNUM( IFAM )
                   WRITE( LOGDEV,'(42x,A)' ), CHEMFAMILYMEMBERS( IFAM,ISRM )
                END DO
            END IF
         END DO

! Set up Stream <-> Size Distribution relationship. This routine
! populates the DESID_STREAM_AERO structure which tells the logic below which
! modes are present on which streams.
         WRITE( LOGDEV, '(/,5x,A)' ),'|> Mapping Particle Size Distributions to Each Emission Stream:'
         WRITE( LOGDEV, '(5x,A)'   ),'==============================================================='
         CALL DESID_INIT_SIZE_DIST( JDATE, JTIME )

! Process Default Emissions Mapping (if requested in namelist; i.e. 
         ALLOCATE( LOCAL_SPEC( NI0 ) )                            ! CMAQ Species Names
         ALLOCATE( LOCAL_EMVAR( NI0,DESID_N_SRM ) )               ! Emission Variable Names
         ALLOCATE( LOCAL_FAC ( NI0,DESID_N_SRM,N_SCALEFAC ) )     ! Scale Factor
         ALLOCATE( LOCAL_FAC_BULK( NI0,DESID_N_SRM,N_SCALEFAC ) ) ! Bulk Scale Factor For Printing to Diagnostic
         ALLOCATE( LOCAL_OP( NI0,DESID_N_SRM,N_SCALEFAC ) )       ! Operator for scaling rule
         ALLOCATE( LOCAL_REG( NI0,DESID_N_SRM,N_SCALEFAC ) )      ! Region Index
         ALLOCATE( LOCAL_BASIS ( NI0,DESID_N_SRM,N_SCALEFAC ) )   ! Mass or Mole Basis for Conversion
         ALLOCATE( LOCAL_CONV ( NI0,DESID_N_SRM,N_SCALEFAC ) )    ! Conversion Factor
         ALLOCATE( MAP_ISTRtoDIFF( NI0 ) )                        ! Map from Instruction to Dispersed species 
         ALLOCATE( MAP_ISTRtoEMVAR(NI0,DESID_N_SRM ) )            ! Map from Instruction to Emission Variable  
         ALLOCATE( MAP_ISTRtoGAS( NI0 ) )                         ! Map from Instruction to Gas Index
         ALLOCATE( MAP_ISTRtoAERO( NI0 ) )                        ! Map from Instruction to Aerosol Index 
         ALLOCATE( MAP_ISTRtoMODE( NI0 ) )                        ! Map from Instruction to Aerosol Mode
         ALLOCATE( MAP_ISTRtoNUM ( NI0 ) )                        ! Map from Instruction to Number Index
         ALLOCATE( MAP_ISTRtoSRF ( NI0 ) )                        ! Map from Instruction to Surface Area Index
         ALLOCATE( MAP_ISTRtoSD  ( NI0,DESID_N_SRM ) )            ! Map from Instruction to Size Distribution Ref
         ALLOCATE( DESID_STREAM_DIFF( N_SPC_DIFF,DESID_N_SRM ) )    
         
         LOCAL_SPEC = ""
         LOCAL_EMVAR = ""
         LOCAL_FAC = 0.0
         LOCAL_FAC_BULK = 0.0      !   Output. Ignores aero_split and unit conversion
         LOCAL_OP = ""             !   Output. Ignores aero_split and unit conversion
         LOCAL_REG = 1             !   Output. Ignores aero_split and unit conversion
         LOCAL_BASIS = ""
         LOCAL_CONV = 1.0
         MAP_ISTRtoDIFF = 0        !   to Diffusion Vector
         MAP_ISTRtoEMVAR= 0        !   to EMission Variabl Location on File
         MAP_ISTRtoGAS = 0         !   to aerosol table
         MAP_ISTRtoAERO = 0        !   to aerosol table
         MAP_ISTRtoMODE = 0        !   to CMAQ aerosol mode
         MAP_ISTRtoNUM  = 0        !   to aerosol number
         MAP_ISTRtoSRF  = 0        !   to aerosol surface area
         MAP_ISTRtoSD  = 0         !   to emissions aerosol mode
         DESID_STREAM_DIFF = .FALSE.

         ! Find all matches between the transported species list and the
         ! available variables from each stream. Apply a scale factor
         ! of 1 to these matches. For aerosols, the CMAQ species name
         ! may or may not include the mode suffix (eg. i, j, or k).
         ! Equivalence tests should be performed without a suffix on the
         ! variable name and with each suffix added in turn.

         DESID_N_ISTR = 0
         SPECNAME = ''

! Insert Emissions Instructions for Aerosol Number and Surface Area,
! if at least one aerosol species is being transported
         IF ( COUNT( DIFF_MASK_AERO ) .GT. 0 ) THEN
            DO IM = 1,N_MODE
               ! Aerosol Number
               LOCAL_SPEC( DESID_N_ISTR+1 ) = AEROMODE( IM )%NUM_NAME
               LOCAL_FAC ( DESID_N_ISTR+1,:,1 ) = 1.0
               LOCAL_FAC_BULK ( DESID_N_ISTR+1,:,1 ) = 1.0         
               LOCAL_BASIS( DESID_N_ISTR+1,:,1 ) = 'UNIT'
               LOCAL_CONV( DESID_N_ISTR+1,:,1 ) = 1.0
               LOCAL_OP ( DESID_N_ISTR+1,:,1 ) = "a"
               LOCAL_REG ( DESID_N_ISTR+1,:,1 ) = 1
               MAP_NUMtoISTR( IM ) = DESID_N_ISTR+1
               MAP_ISTRtoNUM( DESID_N_ISTR+1 ) = IM
               MAP_ISTRtoDIFF( DESID_N_ISTR+1 ) = 
     &            INDEX1( LOCAL_SPEC( DESID_N_ISTR+1 ), N_SPC_DIFF, DIFF_SPC )
               DESID_STREAM_DIFF( MAP_ISTRtoDIFF( DESID_N_ISTR+1 ),: ) = .TRUE.
               MAP_ISTRtoSD( DESID_N_ISTR+1, : ) = 0

               ! Aerosol Surface Area
               LOCAL_SPEC( DESID_N_ISTR+2 ) = AEROMODE( IM )%SRF_NAME
               LOCAL_FAC ( DESID_N_ISTR+2,:,1 ) = 1.0
               LOCAL_FAC_BULK ( DESID_N_ISTR+2,:,1 ) = 1.0         
               LOCAL_BASIS( DESID_N_ISTR+2,:,1 ) = 'UNIT'
               LOCAL_CONV( DESID_N_ISTR+2,:,1 ) = 1.0
               LOCAL_OP ( DESID_N_ISTR+2,:,1 ) = "a"
               LOCAL_REG ( DESID_N_ISTR+2,:,1 ) = 1
               MAP_SRFtoISTR( IM ) = DESID_N_ISTR+2
               MAP_ISTRtoSRF( DESID_N_ISTR+2 ) = IM
               MAP_ISTRtoDIFF( DESID_N_ISTR+2 ) = 
     &            INDEX1( LOCAL_SPEC( DESID_N_ISTR+2 ), N_SPC_DIFF, DIFF_SPC )
               DESID_STREAM_DIFF( MAP_ISTRtoDIFF( DESID_N_ISTR+2 ),: ) = .TRUE.
               MAP_ISTRtoSD( DESID_N_ISTR+2, : ) = 0

               DESID_N_ISTR = DESID_N_ISTR + 2
            END DO
         END IF

! Process User-Defined Emissions Scaling Rules. 
         CALL LOG_SUBHEADING( LOGDEV, 'Reading and Storing Emission Scaling Rules' )

         ! Find Total Number of Rules
         N_RULE = 0
         DO IRULE = 1,SIZE( DESID_RULES_NML )
            IF( DESID_RULES_NML( IRULE )%SPEC .EQ. '' ) EXIT
            N_RULE = IRULE
         END DO

         ! Implement Online Scaling Rules that are less exposed to users
         CALL DESID_GET_ONLINE_RULES( N_RULE )


         ! Allocate Rule->Instruction Transform Masks
         ALLOCATE( RULE_SPEC  ( N_SPC_DIFF ) )
         ALLOCATE( RULE_EMVAR ( DESID_N_SRM ) )
         ALLOCATE( RULE_PHASE ( DESID_N_SRM ) )
         DO ISRM = 1,DESID_N_SRM
           N = DESID_EMVAR( ISRM )%LEN
           RULE_EMVAR( ISRM )%LEN = N
           ALLOCATE( RULE_EMVAR( ISRM )%ARRY( N ) )

           N = DESID_STREAM_AERO( ISRM )%LEN
           RULE_PHASE( ISRM )%LEN = N
           ALLOCATE( RULE_PHASE( ISRM )%ARRY( N ) )
         END DO
 
         ! Loop Through Emission Rules, Test for Fidelity, expand if necessary
         ! and Apply them to the instruction set that currently exists.
         DO IRULE = 1,N_RULE 
            ! Exit this loop if the rule is blank
            IF ( DESID_RULES_NML( IRULE )%SPEC .EQ. '' ) EXIT 

            ! Expand Rule To Individual Instructions. If the CMAQ
            ! Species, Stream Label, and Em. Variable are all single
            ! components, then there will just be one instruction. If
            ! any of them equal 'All' the number of instructions will
            ! grow correspondingly.

            !------   ------   ------   ------   ------   ------   -----
            ! First error check and expand the stream field
            ! This subroutine returns a logical vector, RULE_STREAM,
            ! which identifies which streams are affected by this rule.
            LREMOVE = .FALSE.
            CALL DESID_GET_RULE_STREAMS( DESID_RULES_NML( IRULE )%STREAM, 
     &              IRULE, RULE_STREAM, LREMOVE, LERROR4 )
            IF ( LREMOVE ) CYCLE

            !------   ------   ------   ------   ------   ----  
            ! Now error check and expand the emission variable field
            CALL UPCASE( DESID_RULES_NML( IRULE )%EMVAR )
 
            !Initialize Emission Variable Array for every Stream
            DO ISRM = 1,DESID_N_SRM
              RULE_EMVAR( ISRM )%ARRY = .FALSE.
            END DO
            LEMVAR_KEY = .FALSE.

            LERROR = .TRUE.
            IF ( DESID_RULES_NML( IRULE )%EMVAR .EQ. 'ALL' ) THEN
               ! Expand the Rule to Apply to All Emission Variables
               DO ISRM = 1,DESID_N_SRM
                 IF ( .NOT. RULE_STREAM( ISRM ) ) CYCLE
                 RULE_EMVAR( ISRM )%ARRY = .TRUE.
                 LEMVAR_KEY = .TRUE.
                 LERROR = .FALSE.
               END DO
            ELSE
               ! Determine if the Emission Variable Label Refers to A 
               ! Family and if So, Apply the Rule to all members of that 
               ! Family
               IFAM = INDEX1( DESID_RULES_NML( IRULE )%EMVAR, N_Chem_Fams, ChemFamilyName )
               IAERO = 0
               IF ( IFAM .GT. 0 ) 
     &            IAERO= INDEX1( ChemFamilyName(IFAM), N_AEROSPC, AEROSPC(:)%BULKNAME )
               IF ( IFAM .EQ. 0 .OR. IAERO .NE. 0 ) THEN
                  ! This Emission Variable Label Does not match any families or this is an
                  ! aerosol bulkname. Just look for one variable
                  DO ISRM = 1,DESID_N_SRM
                     IF ( .NOT.RULE_STREAM( ISRM ) ) CYCLE
                     IDX = INDEX1( DESID_RULES_NML( IRULE )%EMVAR, DESID_EMVAR( ISRM )%LEN, 
     &                             DESID_EMVAR( ISRM )%ARRY )
                     IF ( IDX .NE. 0 ) THEN
                        ! A matching Emission variable has been found
                        RULE_EMVAR( ISRM )%ARRY( IDX ) = .TRUE.
                        LERROR = .FALSE.
                     END IF
                 END DO
               END IF

               IF ( IFAM .NE. 0 .AND. IAERO .EQ. 0 ) THEN
                  ! This is not an aerosol bulkname. Loop through all the 
                  ! members to assign emission variables
                  NCHEM = ChemFamilyNum( IFAM )
                  CHEM_NAME(1:NCHEM) = ChemFamilyMembers( IFAM,1:NCHEM )
                  LEMVAR_KEY = .TRUE.
               
                  DO ICHEM = 1,NCHEM
                     LERROR3( ICHEM ) = .FALSE.
                     LERROR2 = .TRUE.
                     DO ISRM = 1,DESID_N_SRM
                        IF ( .NOT. RULE_STREAM( ISRM ) ) CYCLE
                        ! Find the Specific Species this Rule Identifies
                        IDX = INDEX1( CHEM_NAME( ICHEM ), DESID_EMVAR( ISRM )%LEN, 
     &                                DESID_EMVAR( ISRM )%ARRY )
                        IF ( IDX .NE. 0 ) THEN
                           RULE_EMVAR( ISRM )%ARRY( IDX ) = .TRUE.
                           LERROR2 = .FALSE.
                        END IF
                     END DO
                     IF ( LERROR2 ) THEN
                         ! Store the index of the variables not found on
                         ! any emission stream.
                         LERROR3( ICHEM ) = .TRUE.
                     END IF
                  END DO 
                  IF ( .NOT. ANY( LERROR3 ) ) LERROR = .FALSE.
               END IF
               
            END IF
            
            IF ( LERROR ) THEN
              IF ( .NOT. EMISCHK ) THEN
                 WRITE( LOGDEV, '(/,5x,A,/,5x,2A,/,5x,3A,3(/,5x,A))') 
     &                  '*** ATTENTION **********************************************:',
     &                  'The emission variable or member of family ',TRIM( DESID_RULES_NML( IRULE )%EMVAR ),
     &                  'was not found in the emission stream(s) ',TRIM(DESID_RULES_NML(IRULE)%STREAM),'  but the ',
     &                  'CTM_EMISCHK environment variable set to False so simulation ',
     &                  'will proceed.',
     &                  '*************************************************************'
                 IF ( IFAM .NE. 0 ) THEN
                    WRITE( LOGDEV, '(7x,A,/,7x,A,/,7x,A,1x,A )') 
     &                  'The emission variable field applied was a chemical family. The specific',
     &                  'family member(s) that did not appear on requested emission stream(s)',
     &                  'inputs was:'
                    DO ICHEM = 1,NCHEM
                        IF ( LERROR3( ICHEM ) ) WRITE( LOGDEV, '(20x,A)' ) TRIM( CHEM_NAME( ICHEM ) )
                    END DO
                    WRITE( LOGDEV, '(7x,A)' )
     &                  '*************************************************************'
                 END IF
                 CYCLE
              ELSE     
                 WRITE( LOGDEV, '(5x,A,/,5x,2A,/,5x,3A,7(/,5x,A))') 
     &                  '*** ERROR **************************************************************:',
     &                  'The emission variable or member of family ',TRIM( DESID_RULES_NML( IRULE )%EMVAR ),
     &                  ' is not found on the stream(s) ',TRIM(DESID_RULES_NML(IRULE)%STREAM),'.',
     &                  'Use one of the below options to continue.', 
     &                  '1) Change or remove this emission rule so that it refers to an existing emission variable.', 
     &                  'or',
     &                  '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &                  'if model predictions are acceptable without using the above emissions.',
     &                  '*************************************************************************'
                 IF ( IFAM .NE. 0 ) THEN
                    WRITE( LOGDEV, '(7x,A,/,7x,A,/,7x,A,1x,A )') 
     &                  'The emission variable field applied was a chemical family. The specific',
     &                  'family member(s) that did not appear on requested emission stream(s)',
     &                  'inputs was:'
                    DO ICHEM = 1,NCHEM
                        IF ( LERROR3( ICHEM ) ) WRITE( LOGDEV, '(20x,A)' ) TRIM( CHEM_NAME( ICHEM ) )
                    END DO
                    WRITE( LOGDEV, '(7x,A)' )
     &                  '*************************************************************'
                 END IF
                 XMSG = 'Species with the missing emission variable ' 
     &                //'must have a variable found in at least one stream.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF
            END IF
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check and Expand the CMAQ Species Field
            CALL UPCASE( DESID_RULES_NML( IRULE )%SPEC )

            ! Initialize CMAQ Species Array
            RULE_SPEC = .FALSE.
            LSPEC_KEY = .FALSE.

            IF ( DESID_RULES_NML( IRULE )%SPEC .EQ. 'ALL' ) THEN
               ! Expand the Rule to Apply to All Species
               RULE_SPEC = .TRUE.
               LSPEC_KEY = .TRUE.
            ELSE     
               ! Determine if the Species Label Refers to A Family and if So, 
               ! Apply the Rule to all members of that Family
               IFAM = INDEX1( DESID_RULES_NML( IRULE )%SPEC, N_Chem_Fams, ChemFamilyName )
               IF ( IFAM .EQ. 0 ) THEN
                  NCHEM = 1
                  CHEM_NAME(1) = DESID_RULES_NML( IRULE )%SPEC
               ELSE
                  NCHEM = ChemFamilyNum( IFAM )
                  CHEM_NAME(1:NCHEM) = ChemFamilyMembers( IFAM,1:NCHEM )
                  LSPEC_KEY = .TRUE.
               END IF

               DO ICHEM = 1,NCHEM
                 ! Find the Specific Species this Rule Identifies
                 IDX = INDEX1( CHEM_NAME( ICHEM ), N_SPC_DIFF, DIFF_SPC )
                 JDX = INDEX1( CHEM_NAME( ICHEM ), N_AEROSPC,  AEROSPC( : )%BULKNAME )
                 IF ( IDX .NE. 0 ) THEN
                   RULE_SPEC( IDX ) = .TRUE.
                 ELSE IF ( JDX .NE. 0 ) THEN
                   ! This is an aerosol species, and it is being
                   ! identified with a bulk name (no mode suffix). 
                   ! We need to allow for all possible DIFF_SPC with
                   ! all used suffixes
                   SN = CHEM_NAME( ICHEM )
                   DO IM = 1,N_MODE
                     KDX = INDEX1( TRIM( SN )//MODESUFF( IM ), N_SPC_DIFF, DIFF_SPC )
                     IF ( KDX .NE. 0 ) RULE_SPEC( KDX ) = .TRUE.
                   END DO
                 ELSE
                   WRITE( LOGDEV, '(/,5A,/,A,/,A,/,A,/,A)' ),
     &               'Species ',TRIM(DESID_RULES_NML( IRULE )%SPEC),':',
     &               TRIM(CHEM_NAME(ICHEM)),' was used in the Emissions',
     &               ' Control Instructions Namelist but it is not a valid CMAQ ',
     &               'transported species or family. Please add it to one of the ',
     &               'input chemical namelists (ie. GC, AE, etc). Note that aerosol',
     &               'Number and Surface Area Species are not valid for scaling.'
                   XMSG = 'Error in Emissions Map Processing.'
                   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                 END IF
               END DO
           END IF 
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check and Expand the Phase Field
            CALL UPCASE( DESID_RULES_NML( IRULE )%PHASE )

            !Initialize Emission Variable Array for every Stream
            DO ISRM = 1,DESID_N_SRM
              RULE_PHASE( ISRM )%ARRY = .FALSE.
            END DO

            LERROR = .TRUE.
            DO ISRM = 1,DESID_N_SRM 
              ! Skip this stream if it is not identified
              IF ( .NOT. RULE_STREAM( ISRM ) ) CYCLE 

              IF ( DESID_RULES_NML( IRULE )%PHASE .EQ. 'ALL' ) THEN
                 ! Expand the Rule to Apply to All Phases and Modes
                 RULE_PHASE( ISRM )%ARRY = .TRUE.
                 LERROR = .FALSE.
              ELSE IF ( DESID_RULES_NML( IRULE )%PHASE .EQ. 'AERO' ) THEN
                 ! Expand the Rule to Apply to All Aerosol Modes
                 RULE_PHASE( ISRM )%ARRY(2:) = .TRUE.
                 LERROR = .FALSE.
              ELSE
                 ! Find the Specific Phase/Mode this Rule Identifies
                 IDX = INDEX1( DESID_RULES_NML( IRULE )%PHASE, DESID_STREAM_AERO( ISRM )%LEN, 
     &                         DESID_STREAM_AERO( ISRM )%NAME )
                 IF ( IDX .NE. 0 ) THEN
                   RULE_PHASE( ISRM )%ARRY( IDX ) = .TRUE.
                   LERROR = .FALSE.
                 END IF
              END IF
            END DO
            
            IF ( LERROR ) THEN
                 WRITE( LOGDEV, '(A,/,2A,/,A,/,A,I3,A1,/,A)') 
     &                  '*****************************ERROR***************************************:',
     &                  'The phase or mode ',TRIM( DESID_RULES_NML( IRULE )%PHASE ),
     &                  ' is not found in any of the emission streams you are requesting for ',
     &                  'emission rule ',IRULE,'.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
                 XMSG = 'Species with the missing mode ' 
     &                //'must have a mode found in at least one stream.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check the Region Field
            CALL UPCASE( DESID_RULES_NML( IRULE )%REGION )

            ! Check that the Region has been defined
            LERROR = .TRUE.
            DO IRGN = 1,DESID_N_REG 
              IF ( DESID_RULES_NML( IRULE )%REGION .EQ. 
     &             DESID_REG( IRGN )%LABEL ) THEN 
                 REGNUM = IRGN
                 LERROR = .FALSE.
              END IF
            END DO
            
            IF ( LERROR ) THEN
                 WRITE( LOGDEV, '(A,/,2A,/,A,/,A,/,A)') 
     &                  '*****************************ERROR***************************************:',
     &                  'The Region ',TRIM( DESID_RULES_NML( IRULE )%REGION ),
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
            IF ( DESID_RULES_NML( IRULE )%OP .NE. 'a' .AND.
     &           DESID_RULES_NML( IRULE )%OP .NE. 'm' .AND.
     &           DESID_RULES_NML( IRULE )%OP .NE. 'o'       ) THEN
               WRITE( LOGDEV, * )
               WRITE( XMSG, '(/,A,A,A,I3,A,A,A)' ),
     &             'The Emissions Operator (',DESID_RULES_NML( IRULE )%OP,
     &             ') applied for Rule ',IRULE,' in the Emissions Control ',
     &             'Namelist does not match any of the allowed values (a, m, or o)',
     &             '. Please check the your emissions control inputs.'
               WRITE( LOGDEV, * )TRIM( XMSG )
               XMSG = 'Error in Emissions Map Processing.'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            
            !------   ------   ------   ------   ------   ------ ------
            ! Order the indivdual tasks in this rule into one
            ! vector of instructions. The operator, scale factor, and
            ! region for each of these tasks will be uniform because 
            ! they apply to the entire rule
            N_TASKS = 0
            DO IDIFF = 1,N_SPC_DIFF
            IF ( RULE_SPEC( IDIFF ) ) THEN
              DO ISRM = 1,DESID_N_SRM
              IF ( RULE_STREAM( ISRM ) ) THEN
                DO IEMVAR = 1,DESID_EMVAR( ISRM )%LEN 
                IF ( RULE_EMVAR( ISRM )%ARRY( IEMVAR ) ) THEN
                  DO ISD = 1,DESID_STREAM_AERO( ISRM )%LEN
                  IF ( RULE_PHASE( ISRM )%ARRY( ISD ) ) THEN

                    LTEST = .TRUE.
                    
                    ! If this task applies to a Gas CMAQ species, make
                    ! sure the aerosol phases are not invoked so that 
                    ! double-counting is avoided
                    IF ( .NOT. DIFF_MASK_AERO( IDIFF ) .AND. 
     &                   DESID_STREAM_AERO( ISRM )%NAME( ISD ) .NE. 'GAS' )
     &                 LTEST = .FALSE.

                    ! If this task applies to an aerosol CMAQ species,
                    ! then make sure that the 'mode' selection will apply
                    ! mass to this CMAQ species, DIFF_SPC( IDIFF ).
                    ! Because CMAQ species are resolved in modes, it
                    ! could be that this 'mode' selection will not put
                    ! any mass in this species. For example, if this
                    ! species is for the Aitken mode, but the selection
                    ! was meant to populate the coarse mode.
                    IF ( DIFF_MASK_AERO( IDIFF ) ) THEN
                        DO IAERO = 1,N_AEROSPC
                           IM = INDEX1( DIFF_SPC( IDIFF ), N_MODE,
     &                                  AEROSPC( IAERO )%NAME( : ) )
                           IF ( IM .GT. 0 ) THEN
                             IF ( DESID_STREAM_AERO( ISRM )%FACNUM( ISD,IM ) 
     &                              .LE. 1.0e-10 ) LTEST = .FALSE.
                           END IF
                        END DO
                    END IF

                    ! If any family name was used for the Emission Variable 
                    ! and Species fields and those family or keyword
                    ! names match, then only exact matches or existing 
                    ! relationships should be populated.
                    IF ( (LEMVAR_KEY .AND. LSPEC_KEY ) .AND.
     &                   (DESID_EMVAR( ISRM )%ARRY( IEMVAR ) .NE. 
     &                      DIFF_SPC( IDIFF ) ) ) THEN
                      ! Look for this relationship in at least one
                      ! of the existing instructions
                      LTEST = .FALSE.
                      DO ISTRN = 1,DESID_N_ISTR
                          IF ( LOCAL_SPEC( ISTRN ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &                         LOCAL_EMVAR( ISTRN,ISRM ) .EQ. 
     &                           DESID_EMVAR( ISRM )%ARRY( IEMVAR ) ) THEN
                            LTEST = .TRUE.
                          END IF
                      END DO
                    END IF

                    ! Add a Task for this combination of CMAQ
                    ! Species, Stream, Em. Variable, and Phase 
                    ! if the test for validity (LTEST) is still 
                    ! TRUE
                    IF ( LTEST ) THEN
                      N_TASKS = N_TASKS + 1
                      TASK_IDIFF( N_TASKS ) = IDIFF
                      TASK_ISRM ( N_TASKS ) = ISRM
                      TASK_IEMVAR(N_TASKS ) = IEMVAR
                      TASK_SPEC ( N_TASKS ) = DIFF_SPC( IDIFF )
                      TASK_EMVAR( N_TASKS ) = DESID_EMVAR(ISRM)%ARRY(IEMVAR)
                      TASK_PHASE( N_TASKS ) = ISD
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
            IF ( DESID_RULES_NML( IRULE )%OP .EQ. 'a' ) THEN
               ! Add this rule to existing instructions
               DO IC = 1,N_TASKS
                  ! This entry needs to be created, but first we need
                  ! to check whether to add it as a new row or add it
                  ! to a previous row. We can add to a previous row
                  ! if the CMAQ species matches exactly, there is no 
                  ! em. var present for this stream, and the same
                  ! Emissions Variable is being used for another 
                  ! stream.
                  LFOUND = .FALSE.
                  ! Look For a suitable previous instruction to add to.
                  IF ( DESID_N_ISTR .GT. 0 ) THEN
                  DO ISTRN = 1,DESID_N_ISTR
                    IF ( ! The CMAQ species for this instruction matches
                         ! the CMAQ species in the rule
     &                   LOCAL_SPEC( ISTRN ) .EQ. TASK_SPEC( IC ) .AND.
                         ! The instruction emission file variable
                         ! matches that of the rule
     &                   ( ( LOCAL_EMVAR( ISTRN, TASK_ISRM( IC ) ) .EQ.
     &                       TASK_EMVAR( IC ) ) .OR.
                         ! The instruction has no emission file variable assigned 
                         ! yet and one is available on this stream.
     &                   ( LOCAL_EMVAR( ISTRN, TASK_ISRM( IC ) ) .EQ. '' .AND.
     &                   ANY( LOCAL_EMVAR( ISTRN,: ) .EQ. TASK_EMVAR( IC ))) )
     &                 ) THEN
                      ! Add This Command to Instruction number ISTR
                      ISTR = ISTRN
                      LFOUND = .TRUE.
                    END IF
                  END DO
                  END IF

                  ! If no suitable instruction was found to add to, add a new
                  ! instruction. This means either there was no previous
                  ! instruction with the same CMAQ species and emission variable 
                  ! or there was an instruction with this CMAQ species but it 
                  ! already had an emission variable and scale factor associated 
                  ! with this stream.
                  IF ( .NOT. LFOUND ) THEN
                    DESID_N_ISTR = DESID_N_ISTR + 1
                    ISTR = DESID_N_ISTR

                    LOCAL_SPEC( ISTR )     = TASK_SPEC( IC )
                    MAP_ISTRtoDIFF( ISTR ) = TASK_IDIFF( IC )

                    ! Link this row to the Aerosol or Gas Species
                    IF ( DIFF_MASK_AERO( TASK_IDIFF( IC ) ) ) THEN
                      DO IAERO = 1,N_AEROSPC
                        JM = INDEX1( LOCAL_SPEC( ISTR ), N_MODE, AEROSPC( IAERO )%NAME( : ) )
                        IF ( JM .GT. 0 ) THEN
                          MAP_ISTRtoAERO( ISTR ) = IAERO
                          MAP_ISTRtoMODE( ISTR ) = JM
                        END IF
                      END DO
                    ELSE
                      MAP_ISTRtoGAS( ISTR ) = TASK_IDIFF( IC )
                    END IF
                  END IF
                          
                  ! Now that the instruction location has either been
                  ! found or created, populate it.
                  ISRM   = TASK_ISRM( IC )
                  IEM    = DESID_STREAM_AERO( ISRM )%REF( TASK_PHASE( IC ) )
                  IEMVAR = TASK_IEMVAR( IC )
                    
                  LOCAL_EMVAR( ISTR, ISRM )    = TASK_EMVAR( IC )
                  MAP_ISTRtoEMVAR( ISTR, ISRM ) = IEMVAR
                  DESID_EMVAR( ISRM )%USED( IEMVAR ) = .TRUE.
                  MAP_ISTRtoSD( ISTR, ISRM )         = TASK_PHASE( IC )
                  DESID_STREAM_DIFF( TASK_IDIFF( IC ),ISRM ) = .TRUE.

                  ! Only apply an aerosol size-split parameter if this
                  ! species is an aerosol and if it is not from a dust 
                  ! or sea spray sector
                  AERO_SPLIT = 1.0
                  IF ( DIFF_MASK_AERO( TASK_IDIFF( IC ) ) .AND. 
     &                 ISRM .NE. IDUSTSRM .AND. ISRM .NE. ISEASRM    )
     &                 AERO_SPLIT = SD_SPLIT( TASK_IDIFF( IC ), IEM )

                  ! Determine Next Free Location in Scale Factor Space
                  ! (IFAC) so that the scale factor can be added.
                  DO IFAC = 1,N_SCALEFAC
                      IF ( LOCAL_OP( ISTR, ISRM, IFAC ) .EQ. '' ) THEN

                        CALL CHECK_OP( DESID_RULES_NML( IRULE )%OP, IRULE )
                        CALL CHECK_BASIS( DESID_RULES_NML( IRULE )%BASIS, IRULE )

                        LOCAL_FAC ( ISTR, ISRM, IFAC ) = DESID_RULES_NML( IRULE)%FAC * AERO_SPLIT
                        LOCAL_FAC_BULK(ISTR,ISRM,IFAC )= DESID_RULES_NML( IRULE)%FAC
                        LOCAL_REG( ISTR, ISRM, IFAC )  = REGNUM
                        LOCAL_OP( ISTR, ISRM, IFAC )   = DESID_RULES_NML( IRULE)%OP
                        LOCAL_BASIS( ISTR, ISRM, IFAC )= DESID_RULES_NML( IRULE)%BASIS
                        LOCAL_CONV ( ISTR, ISRM, IFAC )= DESID_EMVAR( ISRM )%CONV( IEMVAR )
                        EXIT
                     END IF
                  END DO
               END DO
            ELSE
               ! Modify All Existing Instructions that Match this
               ! rule's parameters.
               DO IC = 1,N_TASKS
                 ! Loop through existing instructions and find matches
                 IF ( DESID_N_ISTR .GT. 0 ) THEN
                   ISRM = TASK_ISRM( IC )
                   DO ISTR = 1,DESID_N_ISTR
                   IF ( LOCAL_SPEC( ISTR ) .EQ. TASK_SPEC( IC ) ) THEN
                      IF ( LOCAL_EMVAR( ISTR, ISRM ) .EQ. TASK_EMVAR( IC ) .AND.
     &                     MAP_ISTRtoSD( ISTR,ISRM ) .EQ. TASK_PHASE( IC ) ) THEN
                  
                        IEMVAR = TASK_IEMVAR( IC )
                        IEM = DESID_STREAM_AERO( ISRM )%REF( TASK_PHASE( IC ) )
                        AERO_SPLIT = 1.0
                        IF ( DIFF_MASK_AERO( TASK_IDIFF( IC ) ) .AND.
     &                       DESID_RULES_NML( IRULE )%OP .EQ. 'o' ) 
     &                       AERO_SPLIT = SD_SPLIT( TASK_IDIFF( IC ), IEM )

                        ! Determine Next Free Location in Scale Factor Space (IFAC) so 
                        ! that the scale factor can be added.
                        DO IFAC = 1,N_SCALEFAC
                          IF ( LOCAL_OP( ISTR, ISRM, IFAC ) .EQ. '' ) THEN

                             CALL CHECK_OP( DESID_RULES_NML( IRULE )%OP, IRULE )
                             CALL CHECK_BASIS( DESID_RULES_NML( IRULE )%BASIS, IRULE )

                             LOCAL_FAC ( ISTR, ISRM, IFAC )      = DESID_RULES_NML( IRULE )%FAC * AERO_SPLIT
                             LOCAL_FAC_BULK ( ISTR, ISRM, IFAC ) = DESID_RULES_NML( IRULE )%FAC
                             LOCAL_REG( ISTR, ISRM, IFAC )       = REGNUM
                             LOCAL_OP( ISTR, ISRM, IFAC )        = DESID_RULES_NML( IRULE )%OP
                             LOCAL_BASIS( ISTR, ISRM, IFAC )     = DESID_RULES_NML( IRULE )%BASIS
                             LOCAL_CONV( ISTR, ISRM, IFAC )      = DESID_EMVAR( ISRM )%CONV( IEMVAR )
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

         ! Create Summarized Emission Scale Factor Structure that
         ! can be processed in the Emiss_Scaling subroutine every time
         ! step
         ALLOCATE( DESID_FAC( DESID_N_ISTR,DESID_N_SRM ) )
         ALLOCATE( REG_UNQ( N_SCALEFAC ) )
         ALLOCATE( DESID_REG_RMDR( 1000 ) )
         N_REG_RMDR = 0
         ALLOCATE( RMDR_MASK( NCOLS,NROWS ))
         ALLOCATE( LSUB( DESID_N_REG ) )

         DO ISTR = 1,DESID_N_ISTR
            DO ISRM = 1,DESID_N_SRM
               DESID_FAC( ISTR,ISRM )%NFAC = 0
               IF ( MAP_ISTRtoEMVAR( ISTR,ISRM ) .NE. 0 ) THEN

               ! First determine number of emission scale factors (NFAC) and
               ! number of unique regions (NREG) that are specified for this 
               ! instruction and stream combination
               NFAC = 1
               NREG = 1
               REG_UNQ(:) = 1
               DO IFAC = 2,N_SCALEFAC
                 IF ( LOCAL_OP( ISTR,ISRM,IFAC ) .EQ. '' ) EXIT
                 NFAC = NFAC + 1 
               
                 ! Determine whether the region for this factor is
                 ! unique for this combination of ISTR and ISRM
                 IF ( COUNT( REG_UNQ(1:NREG) .eq. 
     &                       LOCAL_REG( ISTR,ISRM,IFAC ) ) .eq. 0 ) THEN
                   NREG = NREG + 1
                   REG_UNQ( NREG ) = LOCAL_REG( ISTR,ISRM,IFAC )
                 END IF
               END DO

               ! Determine if two active regions contain one or more 
               ! identical Sub-Regions. If they do, then add that common
               ! sub-region as an active region.
               IF ( NREG .GT. 1 ) THEN
                 DO IRGN = 1,NREG-1
                 DO JRGN = IRGN+1,NREG
                   I = REG_UNQ( IRGN )
                   J = REG_UNQ( JRGN )
                   ! Skip if one of these Regions is a Sub-Region of the other
                   IF ( DESID_REG_SUB(I,J) .OR. DESID_REG_SUB(J,I) ) CYCLE
                   DO K = 1,DESID_N_REG
                     ! Check if IRGN and JRGN have a common Sub-Region 
                     ! and that that sub-region is not an active region
                     IF ( DESID_REG_SUB(I,K) .AND. DESID_REG_SUB(J,K) .AND.
#ifndef mpas
     &                    INDEXINT1( K, NREG, REG_UNQ(1:NREG) ) .EQ. 0 ) THEN
#else
     &                    INDEX1( K, NREG, REG_UNQ(1:NREG) ) .EQ. 0 ) THEN
#endif
                       ! Add Sub-Region K to Active Region List
                       NREG = NREG + 1
                       REG_UNQ( NREG ) = K
                       
                       ! Add a factor to zero out this Sub-Region
                       NFAC = NFAC + 1
                       LOCAL_REG( ISTR,ISRM,NFAC ) = K
                       LOCAL_OP ( ISTR,ISRM,NFAC ) = 'o'
                       LOCAL_FAC( ISTR,ISRM,NFAC ) = 0.
                       LOCAL_FAC_BULK( ISTR,ISRM,NFAC ) = 0.
                       LOCAL_BASIS( ISTR,ISRM,NFAC ) = 'UNIT'
                       LOCAL_CONV( ISTR,ISRM,NFAC ) = 1.0
                       
                       ! Add Factors for this Sub-Region to the
                       ! Instruction set
                       DO IFAC = 1,NFAC-1
                           IF ( LOCAL_REG( ISTR,ISRM,IFAC ) .EQ. K .OR.
     &                          DESID_REG_SUB( LOCAL_REG(ISTR,ISRM,IFAC), K) ) THEN
                           ! Region K is a Sub-Region of the Region in this instruction
                           NFAC = NFAC + 1
                           LOCAL_REG( ISTR,ISRM,NFAC )     = K
                           LOCAL_OP ( ISTR,ISRM,NFAC )     = LOCAL_OP ( ISTR,ISRM,IFAC ) 
                           LOCAL_FAC( ISTR,ISRM,NFAC )     = LOCAL_FAC( ISTR,ISRM,IFAC ) 
                           LOCAL_FAC_BULK( ISTR,ISRM,NFAC )= LOCAL_FAC_BULK( ISTR,ISRM,IFAC )
                           LOCAL_BASIS( ISTR,ISRM,NFAC )   = LOCAL_BASIS( ISTR,ISRM,IFAC )  
                           LOCAL_CONV( ISTR,ISRM,NFAC )    = LOCAL_CONV( ISTR,ISRM,IFAC )  
                         END IF    
                       END DO
                     END IF
                   END DO
                 END DO
                 END DO
               END IF

               ! Allocate and Prepopulate Emission Factor Structure
               DESID_FAC( ISTR,ISRM )%NFAC = NFAC
               DESID_FAC( ISTR,ISRM )%NREG = NREG
               ALLOCATE( DESID_FAC( ISTR,ISRM )%FAC( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%BULK( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%BASIS( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%AREA( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%AREAADJ( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%REG( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%OP( NFAC ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%REG_UNQ( NREG ) )
               ALLOCATE( DESID_FAC( ISTR,ISRM )%REG_RMDR( NREG ) )

               DESID_FAC( ISTR,ISRM )%FAC  = 0.
               DESID_FAC( ISTR,ISRM )%BULK = 0.
               DESID_FAC( ISTR,ISRM )%BASIS= 1.
               DESID_FAC( ISTR,ISRM )%AREA = .FALSE.
               DESID_FAC( ISTR,ISRM )%AREAADJ = .FALSE.
               DESID_FAC( ISTR,ISRM )%REG  = 1
               DESID_FAC( ISTR,ISRM )%OP   = 0
               DESID_FAC( ISTR,ISRM )%REG_UNQ= REG_UNQ( 1:NREG )
               DESID_FAC( ISTR,ISRM )%REG_RMDR= 0
        
               ! Populate remainder mask array of big regions by subtracting away 
               ! their subsets
               DO IRGN = 1,NREG 
                 ! Move to the next region if this one has no sub-regions
                 IF ( ALL( .NOT. DESID_REG_SUB( REG_UNQ(IRGN),: ) ) ) CYCLE

                 ! Build vector of active sub-regions of this region
                 LSUB(:) = .FALSE.
                 DO JRGN = 1,NREG
                   IF ( DESID_REG_SUB( REG_UNQ(IRGN),REG_UNQ(JRGN) ) )
     &               LSUB( REG_UNQ(JRGN) ) = .TRUE.
                     ! Region JRGN is a subdomain of Region IRGN 
                 END DO
                 IF ( ALL( .NOT. LSUB(:) )) CYCLE ! No active sub-regions

                 ! Check to see if this remainder mask has been calculated already.
                 LREG_RMDR = .FALSE.
                 DO K = 1,N_REG_RMDR
                   IF ( DESID_REG_RMDR( K )%REG .EQ. REG_UNQ(IRGN) .AND.
     &                  ALL( DESID_REG_RMDR( K )%SUB(:) .EQV. LSUB(:) ) ) THEN
                     ! Remainder mask already exists. Map to it.
                     LREG_RMDR = .TRUE.
                     DESID_FAC( ISTR,ISRM )%REG_RMDR( IRGN ) = K
                     EXIT
                   END IF
                 END DO
                 IF ( LREG_RMDR ) CYCLE

                 ! If there's no remainder mask already, add one
                 N_REG_RMDR = N_REG_RMDR + 1
                 ! Identify Large Region
                 DESID_REG_RMDR( N_REG_RMDR )%REG = REG_UNQ(IRGN)
                 
                 ! Identify active sub-regions
                 ALLOCATE( DESID_REG_RMDR( N_REG_RMDR )%SUB( DESID_N_REG ) )
                 DESID_REG_RMDR( N_REG_RMDR )%SUB(:) = LSUB(:)

                 ! Calculate remainder mask
                 ALLOCATE( DESID_REG_RMDR( N_REG_RMDR )%MASK( NCOLS,NROWS ))
                 RMDR_MASK(:,:) = DESID_REG_FAC(:,:,REG_UNQ(IRGN) )
                 DO JRGN = 1,NREG
                   ! Make sure this sub-region is not a sub-region of
                   ! another active sub-region
                   LSUBSUB = .FALSE.
                   DO KRGN = 1,NREG
                     IF ( KRGN .NE. IRGN .AND. LSUB(REG_UNQ(KRGN)) .AND. 
     &                    DESID_REG_SUB( REG_UNQ(KRGN),REG_UNQ(JRGN) ) )
     &                  LSUBSUB = .TRUE.
                   END DO
                   IF ( LSUB(REG_UNQ(JRGN)) .AND. .NOT. LSUBSUB )
     &               RMDR_MASK(:,:) = MAX( 0., RMDR_MASK(:,:) 
     &                         - DESID_REG_FAC(:,:,REG_UNQ(JRGN) ))
                 END DO
                 DESID_REG_RMDR( N_REG_RMDR )%MASK(:,:) = RMDR_MASK(:,:)
                 
                 ! Assign this remainder mask to an active region
                 ! for this factor stack
                 DESID_FAC( ISTR,ISRM )%REG_RMDR( IRGN ) = N_REG_RMDR
               END DO

               ! Populate Local Indices for Chemical Species
               IEMVAR  = MAP_ISTRtoEMVAR( ISTR,ISRM )
               EMVAR_MW= DESID_EMVAR( ISRM )%MW( IEMVAR ) ! MW [g mol-1]
               SPEC_MW = DIFF_MW( MAP_ISTRtoDIFF( ISTR ) )

               ! Populate Emission Factor Structure
               DO IFAC = 1,NFAC
                 DESID_FAC( ISTR,ISRM )%FAC( IFAC )    = LOCAL_FAC ( ISTR, ISRM, IFAC ) 
                 DESID_FAC( ISTR,ISRM )%BULK( IFAC )   = LOCAL_FAC_BULK( ISTR, ISRM, IFAC )
                 DESID_FAC( ISTR,ISRM )%REG( IFAC )    = LOCAL_REG( ISTR, ISRM, IFAC )
                 DESID_FAC( ISTR,ISRM )%AREA( IFAC )   = DESID_EMVAR( ISRM )%LAREA(IEMVAR)
                 DESID_FAC( ISTR,ISRM )%AREAADJ( IFAC )= DESID_EMVAR( ISRM )%LAREAADJ(IEMVAR)
                      
                   SELECT CASE ( LOCAL_OP( ISTR, ISRM, IFAC ) )
                   CASE ( 'a' )
                       DESID_FAC( ISTR,ISRM )%OP( IFAC )    = 1
                   CASE ( 'm' )
                       DESID_FAC( ISTR,ISRM )%OP( IFAC )    = 2
                   CASE ( 'o' )
                       DESID_FAC( ISTR,ISRM )%OP( IFAC )    = 3
                 END SELECT

                 ! Inspect all of the Emissions Instructions for conversion based on 
                 ! mass or moles. If there is a conversion, apply the correct factor
                 ! based on species molecular weights.
                 BASIS_FAC = 1.0
                 IF ( LOCAL_BASIS( ISTR, ISRM, IFAC ) .EQ. 'UNIT' ) THEN
                     BASIS_FAC = BASIS_FAC
                 ELSE IF ( LOCAL_BASIS( ISTR, ISRM, IFAC ) .EQ. 'MOLE' ) THEN
                     IF ( DESID_EMVAR( ISRM )%BASIS( IEMVAR ) .EQ. 'MOLE' ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE IF ( DESID_EMVAR( ISRM )%BASIS( IEMVAR ) .EQ. 'MASS' ) THEN
                        ! Convert emission variable rate to moles
                        BASIS_FAC = BASIS_FAC / EMVAR_MW
                     ELSE
                        ! Unknown Basis has been encountered. Need to exit.
                        WRITE( XMSG,'(A,A16,A,I3,A,A,A)' ),
     &                     'ERROR: Emission Variable ',TRIM(DESID_EMVAR(ISRM)%ARRY(IEMVAR) ),
     &                     ' on emission stream ',ISRM, ' has units which are not recognized ',
     &                     'as an emission rate. If you wish to use this variable for ',
     &                     'emissions, please correct the units (e.g. g/s or moles/s).'
                        CALL M3EXIT( 'EMIS_SPC_MAP', 0, 0, XMSG, 2 )
                     END IF
                     
                     ! Check to see if this is an aerosol (mass-based)
                     ! CMAQ species.
                     IF ( .NOT. DIFF_MASK_AERO( MAP_ISTRtoDIFF( ISTR ) ) ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE
                        ! Convert species emission rate to mass
                        BASIS_FAC = BASIS_FAC * SPEC_MW
                     END IF

                 ELSE IF ( LOCAL_BASIS( ISTR, ISRM, IFAC ) .EQ. 'MASS' ) THEN
                     IF ( DESID_EMVAR( ISRM )%BASIS( IEMVAR ) .EQ. 'MOLE' ) THEN
                        ! Convert emission variable rate to mass
                        BASIS_FAC = BASIS_FAC * EMVAR_MW
                     ELSE IF ( DESID_EMVAR( ISRM )%BASIS( IEMVAR ) .EQ. 'MASS' ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE
                        ! Unknown Basis has been encountered. Need to exit.
                        WRITE( XMSG,'(A,A16,A,I3,A,A,A)' ),
     &                     'ERROR: Emission Variable ',TRIM(DESID_EMVAR(ISRM)%ARRY(IEMVAR) ),
     &                     ' on emission stream ',ISRM, ' has units which are not recognized ',
     &                     'as an emission rate. If you wish to use this variable for ',
     &                     'emissions, please correct the units (e.g. g/s or moles/s).'
                        CALL M3EXIT( 'EMIS_SPC_MAP', 0, 0, XMSG, 2 )
                     END IF

                     ! Check to see if this is an aerosol (mass-based)
                     ! CMAQ species.
                     IF ( .NOT. DIFF_MASK_AERO( MAP_ISTRtoDIFF( ISTR ) ) ) THEN
                        ! Convert species emission rate to moles
                        BASIS_FAC = BASIS_FAC / SPEC_MW
                     ELSE
                        BASIS_FAC = BASIS_FAC
                     END IF
               
                 END IF
                 DESID_FAC( ISTR,ISRM )%BASIS = BASIS_FAC

                 IF ( DESID_FAC( ISTR,ISRM )%OP( IFAC ) .NE. 2 ) 
     &                DESID_FAC( ISTR,ISRM )%FAC( IFAC ) = 
     &                     DESID_FAC( ISTR,ISRM )%FAC( IFAC ) * BASIS_FAC
     &                       * LOCAL_CONV( ISTR, ISRM, IFAC )
              END DO

              END IF
            END DO

        END DO !End Rule Loop

        ! Reduce the size of the Region Remainder Mask
        DESID_REG_RMDR = DESID_REG_RMDR( 1:N_REG_RMDR )

         ! Warn the User if there are no emissions instructions provided        
         IF ( DESID_N_ISTR .LE. 0 ) THEN
            XMSG = 'There are no emissions instructions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         ! Print a message for each emissions stream variable that is not used.
         WRITE( LOGDEV, '(/,5x,A)' ) '|> Checking for unused Emissions Variables: '
         WRITE( LOGDEV, '(5x,A)'   ),'============================================'

         LERROR = .FALSE.
         DO ISRM = 1,DESID_N_SRM
             N = DESID_EMVAR( ISRM )%LEN
             N_UNUSED = COUNT( .NOT. DESID_EMVAR( ISRM )%USED( : )
     &                         .AND. DESID_EMVAR(ISRM)%ARRY( 1:N ) .NE. 'NOT_AVAILABLE'  )
             WRITE( LOGDEV, '(5x,4A,I2,A)' ), TRIM( DESID_STREAM_LAB( ISRM )),' | ',
     &              TRIM(DESID_STREAM_DESC( ISRM )),': ', N_UNUSED,' unused variables.'
             IF ( N_UNUSED .GT. 0 ) THEN
                 LERROR = .TRUE.

                 DO IEMVAR = 1,DESID_EMVAR( ISRM )%LEN
                   IF ( .NOT.DESID_EMVAR( ISRM )%USED( IEMVAR )
     &                  .AND.DESID_EMVAR( ISRM )%ARRY( IEMVAR ) .NE. 'NOT_AVAILABLE' ) 
     &               WRITE( LOGDEV, '(10x,A)'), DESID_EMVAR( ISRM )%ARRY( IEMVAR )
                 END DO
             END IF
             WRITE( LOGDEV, '()' )
         END DO
         IF ( LERROR ) WRITE( LOGDEV, '(5x,A,/,5x,A,/,5x,A)' ),
     &       'NOTE: Some Emissions Variables are unused by your current', 
     &       'emission control configuration. You may want to include these ',
     &       'emissions if they are relevant to your application.'

         ! Print a message for every used variable that is not recognized on 
         ! the DESID_EMVAR_TABLE                 
         WRITE( LOGDEV, '(/,5x,A)' ) '|> Checking status of used Emissions Variables:'
         WRITE( LOGDEV, '(5x,A)'   ),'================================================'

         DO ICATCH = 1,N_EMVAR_CATCH
             B = EMVAR_CATCH( ICATCH )
             N_USED = 0
             !L_CATCH = .FALSE.
             DO ISRM = 1,DESID_N_SRM
                 N = DESID_EMVAR( ISRM )%LEN
                 N_USED = N_USED + COUNT( DESID_EMVAR(ISRM)%ARRY( 1:N ) .EQ. B 
     &                                    .AND. DESID_EMVAR(ISRM)%USED( 1:N ) )

             END DO
             IF ( N_USED .GT. 0 )
     &           WRITE( LOGDEV, '(/,5x,3A,/,5x,A,/,5x,3A,/,5x,A,/,5x,A,/,5x,A)') 
     &                  'WARNING: Emission Variable ',TRIM(B),' is being used but does not have a',
     &                  '         default entry in the DESID_EMVAR_TABLE in DESID_VARS.F. Currently,',
     &                  '         the molecular weight of ',TRIM(B),' is assumed to be 1.0',
     &                  '         g/mol for any emissions scaling operations requiring mass-mole',
     &                  '         conversions. If you are only prescribing UNIT scaling, then this',
     &                  '         assumption will not be problematic.'
         END DO 

! Resize Important Arrays
         MAP_ISTRtoDIFF = MAP_ISTRtoDIFF( 1:DESID_N_ISTR )
         MAP_ISTRtoNUM  = MAP_ISTRtoNUM ( 1:DESID_N_ISTR )
         MAP_ISTRtoSRF  = MAP_ISTRtoSRF ( 1:DESID_N_ISTR )
         MAP_ISTRtoAERO = MAP_ISTRtoAERO( 1:DESID_N_ISTR )


! An Emissions Scaling Map Now Exists as a 2D Array (NSPECIES x NSTREAMS).
! For every element, there is an associated emission variable and scale factor
! to be applied. For the aerosols, the scale factor will be modified
! later in order to split the mass into the appropriate modes.


! Print out the Tables of CMAQ Emissions Instructions organized by each 
! emission stream and then by CMAQ internal species.

         WRITE( LOGDEV, '(/,/,5x,A)' ),'|> EMISSIONS SCALING REPORT:'
         WRITE( LOGDEV, '(5x,A)'   ),'=================================='
         WRITE( LOGDEV,'(7x,A,/,13x,A)' ),'Note: these are populated using rules from the',
     &                              'Emission Control File Supplied by the User.'
         DO ISRM = 1,DESID_N_SRM
           
           ! Print summary information about each Sector including
           ! the label and the available aerosol modes
           WRITE( LOGDEV, '(/,5x,A,A)'   ),'>',REPEAT('-',80 )
           WRITE( LOGDEV,'(5x,A,A,A,A,A2,I2.2,A1)' ),
     &            'Stream Type: "',TRIM(DESID_STREAM_DESC( ISRM )),
     &            '" | Sector Label: ',TRIM(DESID_STREAM_LAB( ISRM ) ),' (',ISRM,')'

           WRITE( LOGDEV, '(/8x,A)' ),'Table of Aerosol Size Distributions Available for Use Sector-Wide.'
           WRITE( LOGDEV, '(8x,A)' ),'Note that Mode 1 is reserved for gas-phase species and emission variable.'
           WRITE( LOGDEV, '(10x,A,2x,A,2x,A)' ), 'Number','Em. Var.  Mode','Reference Mode (see AERO_DATA.F)'
           WRITE( LOGDEV, '(10x,A,2x,A,2x,A)' ), '------','--------------','--------------------------------'
           DO ISD = 2,DESID_STREAM_AERO( ISRM )%LEN
              IEM = DESID_STREAM_AERO( ISRM )%REF( ISD )
              REFNAME = ''
              IF ( IEM .GT. 0 ) THEN
                  REFNAME = DESID_AERO_REF( IEM )%NAME
              END IF

              WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),ISD, 
     &               DESID_STREAM_AERO( ISRM )%NAME( ISD )( 1:16 ), TRIM( REFNAME )
           END DO


           ! Finally Print Every Instruction for this Stream
           WRITE( LOGDEV, '(/,8x,A,5x,A,2x,A,9x,A,13x,A,2x,A,4x,A,2x,A)' ),
     &            'CMAQ Species','Phase/Mode','Em. Var. ','Region','Op','ScaleFac','Basis','FinalFac'
           WRITE( LOGDEV, '(8x,A,5x,A,2x,A,9x,A,13x,A,2x,A,4x,A,2x,A)' ),
     &            '------------','----------','---------','------','--','--------','-----','--------'

           DO IDIFF = 1,N_SPC_DIFF
             L_WDIFF = .TRUE.
             DO ISD = 1,DESID_STREAM_AERO( ISRM )%LEN
               L_WISD = .TRUE.
               DO ISTR = 1,DESID_N_ISTR
                   IF ( LOCAL_SPEC( ISTR ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &                  MAP_ISTRtoSD( ISTR,ISRM ) .EQ. ISD     ) THEN 
                     IF ( L_WDIFF ) THEN
                        WRITE( LOGDEV, '(10x,A,1x,A,1x,A,1x,A,1x,A,2x,ES9.2,4x,A,2x,ES9.2)' ),
     &                         LOCAL_SPEC( ISTR ),
     &                         DESID_STREAM_AERO( ISRM )%NAME( ISD )(1:10),
     &                         LOCAL_EMVAR( ISTR, ISRM ),
     &                         DESID_REG( DESID_FAC( ISTR,ISRM )%REG( 1 ) )%LABEL(1:18),
     &                         LOCAL_OP( ISTR,ISRM,1 ), 
     &                         DESID_FAC( ISTR, ISRM )%BULK( 1 ), LOCAL_BASIS( ISTR,ISRM,1 ),
     &                         DESID_FAC( ISTR,ISRM )%FAC( 1 ) 
     &                         
                        L_WDIFF = .FALSE.
                        L_WISD  = .FALSE.
                      ELSE IF ( L_WISD ) THEN
                        WRITE( LOGDEV, '(27x,A,1x,A,1x,A,1x,A,2x,ES9.2,4x,A,2x,ES9.2)' ),
     &                         DESID_STREAM_AERO( ISRM )%NAME( ISD )(1:10),
     &                         LOCAL_EMVAR( ISTR, ISRM ),
     &                         DESID_REG( DESID_FAC( ISTR,ISRM )%REG( 1 ) )%LABEL(1:18),
     &                         LOCAL_OP( ISTR,ISRM,1 ),
     &                         DESID_FAC( ISTR, ISRM )%BULK( 1 ), LOCAL_BASIS( ISTR,ISRM,1 ),
     &                         DESID_FAC( ISTR,ISRM )%FAC( 1 )
                        L_WISD = .FALSE.
                      ELSE
                        WRITE( LOGDEV, '(38x,A,1x,A,1x,A,2x,ES9.2,4x,A,2x,ES9.2)' ),
     &                         LOCAL_EMVAR( ISTR, ISRM ),
     &                         DESID_REG( DESID_FAC( ISTR,ISRM )%REG( 1 ) )%LABEL(1:18),
     &                         LOCAL_OP( ISTR,ISRM,1 ), 
     &                         DESID_FAC( ISTR, ISRM )%BULK( 1 ), LOCAL_BASIS( ISTR,ISRM,1 ),
     &                         DESID_FAC( ISTR,ISRM )%FAC( 1 ) 
                      END IF    

                      IF ( DESID_FAC( ISTR,ISRM )%NFAC .GT. 1 ) THEN
                      DO IFAC = 2,DESID_FAC( ISTR,ISRM )%NFAC
                         ! Compute Final Factor taking into account all rules that apply to the
                         ! region associated with each factor.
                         FAC  = 0.0
                         IRGN = DESID_FAC( ISTR,ISRM )%REG( IFAC )
                         DO JFAC = 1,IFAC
                           JRGN = DESID_FAC( ISTR,ISRM )%REG( JFAC )
                           ! If region IRGN and JRGN are connected (i.e. one is a subregion of
                           ! the other) then the final_fac should account for JFAC
                           IF ( IRGN .EQ. JRGN .OR. DESID_REG_SUB( IRGN,JRGN ) 
     &                                         .OR. DESID_REG_SUB( JRGN,IRGN ) ) THEN
                              
                             ! Decide how to modify FAC to account for JFAC
                             IF ( DESID_FAC( ISTR,ISRM )%OP(JFAC) .EQ. 1 ) THEN
                                 ! Add Factor (a)
                                 FAC = FAC + DESID_FAC( ISTR,ISRM )%FAC( JFAC )
                             ELSE IF ( DESID_FAC( ISTR,ISRM )%OP(JFAC) .EQ. 2 ) THEN
                                 ! Multiply Factor (m)
                                 FAC = FAC * DESID_FAC( ISTR,ISRM )%FAC( JFAC )
                             ELSE IF ( DESID_FAC( ISTR,ISRM )%OP(JFAC) .EQ. 3 ) THEN
                                 ! Overwrite Factor (o)
                                 FAC = DESID_FAC( ISTR,ISRM )%FAC( JFAC )
                             END IF
                           END IF
                         END DO
                         WRITE( LOGDEV, '(55x,A,1x,A,2x,ES9.2,4x,A,2x,ES9.2)' ),
     &                          DESID_REG( DESID_FAC( ISTR,ISRM )%REG( IFAC ) )%LABEL(1:18),
     &                          LOCAL_OP( ISTR,ISRM,IFAC ), DESID_FAC( ISTR, ISRM )%BULK( IFAC ), 
     &                          LOCAL_BASIS( ISTR,ISRM,IFAC ), FAC
                      END DO
                      END IF

                   END IF
                 !END DO
               END DO
             END DO
           END DO
         END DO

         ! End Emissions Scaling Preparation and Diagnostic Output
         WRITE( LOGDEV, '(/,5x,A)' ), REPEAT( '=',80 )
         WRITE( LOGDEV, '(5x,A,/,/)' ),
     &          '|> END EMISSIONS SCALING PREPARATION AND DIAGNOSTIC OUTPUT'

        END SUBROUTINE DESID_PROCESS_RULES

!-----------------------------------------------------------------------
      SUBROUTINE DESID_INIT_STREAMS( JDATE, JTIME )

! Initialize the counter for the total nbumber of emissions files. Also
! allocate memory for the vectors storing the labels of emission files
! and the maps from master ID number to the relative ID number for each
! gridded and point stream file, i.e. Emissions File 10 is also known as
! Point Source file 2.
!
#ifdef mpas
         use util_module, only : upcase
#endif
  
      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: JDATE, JTIME
  
      INTEGER ISRM, N
  
      CHARACTER( 16 )  :: PNAME = 'DESID_INIT_STREAMS'
      CHARACTER( 32 )  :: VLAB
      INTEGER          :: STATUS
      LOGICAL :: SUCCESS
  
      SUCCESS = .TRUE.
  
      CALL LOG_SUBHEADING( LOGDEV,'Initialize Emissions Input Files' )
   
         ! Calculate the total number of Emission Streams based on the
         ! user options
         ! Add the number of total Emission Streams 
         DESID_N_SRM = N_FILE_GR + NPTGRPS + N_FILE_TR
  
         ! Online Biogenic Emissions
         IF ( BIOGEMIS_BEIS ) DESID_N_SRM = DESID_N_SRM + 1
  
         ! Online Biogenic Emissions from MEGAN
         IF ( BIOGEMIS_MEGAN ) DESID_N_SRM = DESID_N_SRM + 1
  
         ! Marine gas emissions; use online marine gas option only if CB6R5M is used
         IF ( USE_MARINE_GAS_EMISSION ) DESID_N_SRM = DESID_N_SRM + 1
  
         ! Lightning NO Emissions
         IF ( LTNG_NO ) DESID_N_SRM = DESID_N_SRM + 1
  
         ! Sea Spray Aerosol 
         IF ( OCEAN_CHEM ) DESID_N_SRM = DESID_N_SRM + 1
  
         ! Determine if WindBlown Dust is Requested
         IF ( WB_DUST ) DESID_N_SRM = DESID_N_SRM + 1
  
         ! Turn Back Now if DESID_N_SRM Equals Zero (i.e. there are no emissions
         IF ( DESID_N_SRM .EQ. 0 ) RETURN
  
         ! Allocate Emission File Structure Variables
         Allocate( DESID_STREAM_NAME  ( DESID_N_SRM ) )
         Allocate( DESID_STREAM_LAB   ( DESID_N_SRM ) )
         Allocate( DESID_STREAM_TYPE  ( DESID_N_SRM ) )
         Allocate( DESID_STREAM_ITYPE ( DESID_N_SRM ) )
         Allocate( DESID_STREAM_DESC  ( DESID_N_SRM ) )
         Allocate( DESID_STREAM_LAPPLY( DESID_N_SRM ) )
         Allocate( DESID_STREAM_DATE  ( DESID_N_SRM ) )
         Allocate( DESID_STREAM_SYM_DATE ( DESID_N_SRM ) ) 
         Allocate( DESID_STREAM_FIRE  ( DESID_N_SRM ) )
         Allocate( IGSRM         ( DESID_N_SRM ) )
         Allocate( IPSRM         ( DESID_N_SRM ) )
         Allocate( ITSRM         ( DESID_N_SRM ) )
         Allocate( MAP_PTtoISRM  ( NPTGRPS ) )
         Allocate( DESID_EMVAR  ( DESID_N_SRM ) )
         Allocate( DESID_GRID_LAYS  ( DESID_N_SRM ) )
  
C Assign Attributes to Emission File Records. Other records will be 
C populated in individual subroutines. For example, opemis and
C stkemis_init.
  
         CALL LOG_MESSAGE( LOGDEV, ' ' )
         CALL LOG_MESSAGE( LOGDEV, 'Retrieving Env. Variables for Gridded and Elevated Point Emission Inputs' )
         CALL LOG_MESSAGE( LOGDEV, ' ' )
  
         ISRM = 0
         DESID_STREAM_LAPPLY   = .TRUE.
         DESID_STREAM_DATE     = JDATE
         DESID_STREAM_SYM_DATE = EMIS_SYM_DATE ! representative day files (default: false)
         DESID_STREAM_FIRE     = .FALSE.
         DESID_GRID_LAYS       = 0
         
         ! Gridded Emission Files
         IF ( N_FILE_GR .GT. 0 ) THEN
           DESID_STREAM_TYPE( ISRM+1:ISRM+N_FILE_GR ) = 'GRID'
           DESID_STREAM_ITYPE( ISRM+1:ISRM+N_FILE_GR ) = 1
           DO N = 1, N_FILE_GR
              ISRM = ISRM + 1
              IGSRM( ISRM ) = N
  
              ! Create Description of this Emission File for output to
              ! diagnostics
              WRITE( DESID_STREAM_DESC( ISRM ), '(A,I3)' ),
     &           'Gridded Area Emissions File ', N
             
              ! Retrieve Short-Name Label for Each Gridded File
              WRITE( VLAB,'( "GR_EMIS_LAB_",I3.3 )' ) N
              CALL GET_ENV( DESID_STREAM_LAB( ISRM ), VLAB,
     &                      DESID_STREAM_LAB( ISRM ), LOGDEV )
              CALL UPCASE( DESID_STREAM_LAB( ISRM ) )
  
              ! Each Gridded File Name is stored already in IO-API as
              ! an object of the form GR_EMIS_XXX
              WRITE( DESID_STREAM_NAME( ISRM ),'( "GR_EMIS_",I3.3 )' ) N
  
              ! Retrieve Toggle for whether or not to apply these
              ! emissions 
              WRITE( VLAB,'( "GR_EMIS_APPLY_",I3.3 )' ) N
              CALL GET_ENV( DESID_STREAM_LAPPLY( ISRM ), VLAB, 
     &                      DESID_STREAM_LAPPLY( ISRM ), LOGDEV )
  
              ! Retrieve Toggle for overriding emissions file date with
              ! internal model date
              WRITE( VLAB,'( "GR_EM_SYM_DATE_",I3.3 )' ) N
              CALL GET_ENV( DESID_STREAM_SYM_DATE( ISRM ), VLAB, 
     &                      EMIS_SYM_DATE, LOGDEV )
  
           END DO
         END IF
         
         ! In-Line Point Source Files
         IF ( NPTGRPS .GT. 0 ) THEN 
           DESID_STREAM_TYPE( ISRM+1:ISRM+NPTGRPS ) = 'POINT'
           DESID_STREAM_ITYPE( ISRM+1:ISRM+NPTGRPS ) = 2
           DO N = 1, NPTGRPS
              ISRM = ISRM + 1
              IPSRM( ISRM ) = N
              MAP_PTtoISRM( N ) = ISRM
              
              ! Create Description of this Emission File for output to
              ! diagnostics
              WRITE( DESID_STREAM_DESC( ISRM ), '(A,I3)' ),
     &        'Point Emissions File ', IPSRM( ISRM )
             
              ! Retrieve Short-Name Label for Each Inline File
              WRITE( VLAB,'( "STK_EMIS_LAB_",I3.3 )' ) N
              CALL GET_ENV( DESID_STREAM_LAB( ISRM ), VLAB, 
     &                      DESID_STREAM_LAB( ISRM ), LOGDEV )
              CALL UPCASE( DESID_STREAM_LAB( ISRM ) )
  
              ! Each Inline File Name is stored already in IO-API as
              ! an object of the form STK_EMIS_XXX
              WRITE( DESID_STREAM_NAME( ISRM ),'( "STK_EMIS_",I3.3 )' ) N
              
              ! Retrieve Toggle for whether or not to apply these
              ! emissions 
              WRITE( VLAB,'( "STK_EMIS_APPLY_",I3.3 )' ) N
              CALL GET_ENV( DESID_STREAM_LAPPLY( ISRM ), VLAB, 
     &                      DESID_STREAM_LAPPLY( ISRM ), LOGDEV )
  
              ! Retrieve Toggle for overriding emissions file date with
              ! internal model date
              WRITE( VLAB,'( "STK_EM_SYM_DATE_",I3.3 )' ) N
              CALL GET_ENV( DESID_STREAM_SYM_DATE( ISRM ), VLAB, 
     &                      EMIS_SYM_DATE, LOGDEV )
  
           END DO
         END IF
         
         ! Tracer Emissions
         IF ( N_FILE_TR .GT. 0 ) THEN 
           DESID_STREAM_TYPE( ISRM+1:ISRM+N_FILE_TR ) = 'TRAC'
           DESID_STREAM_ITYPE( ISRM+1:ISRM+N_FILE_TR ) = 3
           DO N = 1, N_FILE_TR
              ISRM = ISRM + 1
              ITSRM( ISRM ) = N
  
              ! Create Description of this Emission File for output to
              ! diagnostics
              WRITE( DESID_STREAM_DESC( ISRM ), '(A,I2)' ),
     &          'Gridded Tracer Emissions File ', N
  
              ! Retrieve Short-Name Label for Each Tracer File
              WRITE( VLAB,'( "TR_EMIS_LAB_",I2.2 )' ) N
              CALL GET_ENV( DESID_STREAM_LAB( ISRM ), VLAB,
     &                      DESID_STREAM_LAB( ISRM ), LOGDEV )
              CALL UPCASE( DESID_STREAM_LAB( ISRM ) )
  
              ! Each Tracer File Name is stored already in IO-API as
              ! an object of the form TR_EMIS_XXX
              WRITE( DESID_STREAM_NAME( ISRM ),'( "TR_EMIS_",I2.2 )' ) N
  
           END DO
         END IF
  
         ! Online Biogenic Emissions (BEIS)
         IF ( BIOGEMIS_BEIS ) THEN
             ISRM = ISRM + 1
             DESID_STREAM_TYPE( ISRM ) = 'BIOG'
             DESID_STREAM_ITYPE( ISRM ) = 4
             DESID_STREAM_LAB ( ISRM ) = 'BIOG'
             DESID_STREAM_DESC( ISRM ) = 'Biogenic Emissions'
 
             IBIOSRM = ISRM
         END IF
         
         ! Online Biogenic Emissions (MEGAN)
         IF ( BIOGEMIS_MEGAN ) THEN
             ISRM = ISRM + 1
             DESID_STREAM_TYPE( ISRM ) = 'MIOG'
             DESID_STREAM_ITYPE( ISRM ) = 9
             DESID_STREAM_LAB ( ISRM ) = 'MIOG'
             DESID_STREAM_DESC( ISRM ) = 'Megan Biogenic Emissions'
 
             IMIOGSRM = ISRM
         END IF
 
 
         ! Online Marine Gas Emissions
         IF ( USE_MARINE_GAS_EMISSION ) THEN
             ISRM = ISRM + 1
             DESID_STREAM_TYPE( ISRM ) = 'MGEM'
             DESID_STREAM_ITYPE( ISRM ) = 5
             DESID_STREAM_LAB ( ISRM ) = 'MGEM'
             DESID_STREAM_DESC( ISRM ) = 'Marine Gas Emissions'
 
             IMGSRM = ISRM
         END IF
 
         ! Online Lightning NO Emissions
         IF ( LTNG_NO ) THEN
             ISRM = ISRM + 1
             DESID_STREAM_TYPE( ISRM ) = 'LTNG'
             DESID_STREAM_ITYPE( ISRM ) = 6
             DESID_STREAM_LAB ( ISRM ) = 'LTNG'
             DESID_STREAM_DESC( ISRM ) = 'Lightning NO Emissions'
 
             ILTSRM = ISRM
         END IF
 
         ! Sea Spray Aerosol Emissions
         IF ( OCEAN_CHEM ) THEN
             ISRM = ISRM + 1
             DESID_STREAM_TYPE( ISRM ) = 'ASEA'
             DESID_STREAM_ITYPE( ISRM ) = 7
             DESID_STREAM_LAB ( ISRM ) = 'SEASPRAY'
             DESID_STREAM_DESC( ISRM ) = 'Sea Spray Aerosol Emissions'
             
             ISEASRM = ISRM
         END IF
 
         ! Wind-Blown Dust Emissions
         IF ( WB_DUST ) THEN
             ISRM = ISRM + 1
             DESID_STREAM_TYPE( ISRM ) = 'DUST'
             DESID_STREAM_ITYPE( ISRM ) = 8
             DESID_STREAM_LAB ( ISRM ) = 'WBDUST'
             DESID_STREAM_DESC( ISRM ) = 'Wind-Blown Dust Emissions'
             
             IDUSTSRM = ISRM
         END IF
         
        END SUBROUTINE DESID_INIT_STREAMS 
 
!-----------------------------------------------------------------------
        SUBROUTINE DESID_INIT_DIAG
!       This subroutine processes the user input in the Emission Control
!       file and determines how the emission rates are to be output for
!       diagnostics. Rates may be summed, specific species may be 
!       selected or ignored, and rates may be summed into columns, among
!       other features.
!-----------------------------------------------------------------------

#ifdef mpas
         use util_module, only : index1, upcase
#endif
  
        IMPLICIT NONE
  
        CHARACTER( 200 ) :: PREFIX = 'CCTM_DESID'
        CHARACTER( 200 ) :: SUFFIX = '.nc'
        CHARACTER( 200 ) :: XMSG
        INTEGER          :: VALUE
        LOGICAL          :: stream_vector( DESID_N_SRM )
        INTEGER          :: N_Diag_Nml, N_Buff 
        INTEGER          :: IDIAG, NSTREAM, I, ISRM, J, JDIAG,
     &                      NSUM, NSPEC, LAYS, ISPEC, NPAIRS
        LOGICAL          :: LREMOVE, LFOUND, EXPAND_STREAM
        LOGICAL          :: STREAM_VECTOR_TMP( DESID_N_SRM ),
     &                      SPEC_VECTOR( N_SPC_DIFF )
        CHARACTER( 3)    :: CDIAG
        LOGICAL          :: LERROR
   
        N_Diag_Nml = SIZE( Desid_Diag_Fmt_Nml )
        N_Buff = N_Diag_Nml * 20

        ALLOCATE( DESID_DIAG_STREAM_MASK( DESID_N_SRM, N_Buff ) )
        ALLOCATE( DESID_DIAG_SPEC_BUFF ( N_Buff ) )
        ALLOCATE( DESID_DIAG_N_STREAM ( N_Buff ) )
        ALLOCATE( DESID_DIAG_FORMAT ( N_Buff ) )
        ALLOCATE( DESID_DIAG_FILENAME ( N_Buff ) )
        ALLOCATE( DESID_DIAG_LAB ( N_Buff ) )
        ALLOCATE( DESID_DIAG_SUM( N_Buff ) )
        ALLOCATE( DESID_DIAG_LAYS( N_Buff ) )
   
        CALL LOG_SUBHEADING( LOGDEV,'Initialize Emissions Diagnostic Files' )
   
        DESID_DIAG_STREAM_MASK = .FALSE.
        DESID_DIAG_FORMAT      = 'OFF'
        DESID_DIAG_FILENAME    = ''
        DESID_DIAG_N_STREAM    = 0
        DESID_DIAG_LAB         = ''
        DESID_DIAG_LAYS        = NLAYS
        DESID_DIAG_SUM         = 0
   
        ! Set the standard suffix for all Emissions Diagnostic Files
        IF ( APPL_NAME(1:8 ) .NE. 'CTM_APPL' ) SUFFIX = '_' // TRIM(APPL_NAME) // SUFFIX
   
        ! Find Number of Emissions Diagnostic Files Selected. 
        DESID_N_DIAG = 0
        NSUM = 0
        DO IDIAG = 1,N_Diag_Nml
           ! Error Check Value for Emission Diagnostic Format
           CALL UPCASE( Desid_Diag_Fmt_Nml( IDIAG ) )
           IF ( Desid_Diag_Fmt_Nml( IDIAG ) .NE. 'OFF' .AND. 
     &          Desid_Diag_Fmt_Nml( IDIAG ) .NE. '3D' .AND.
     &          Desid_Diag_Fmt_Nml( IDIAG ) .NE. 'COLSUM' .AND. 
     &          Desid_Diag_Fmt_Nml( IDIAG ) .NE. 'LAYER1' ) THEN
               WRITE( LOGDEV, * )
               WRITE( XMSG, '(A,I3,A,A)' ),
     &             'The format for Emission Diagnostic group ',IDIAG,
     &             ' is not allowed. Please correct:',TRIM( Desid_Diag_Fmt_Nml(IDIAG))
               CALL M3EXIT( 'Map_Emiss_Diag', 0, 0, XMSG, XSTAT1 )     
           END IF
   
           ! Determine number of stream labels to expand
           NSTREAM = INDEX1( '', Desid_Max_Diag_Streams, Desid_Diag_Streams_Nml(IDIAG,:) ) - 1
           IF ( NSTREAM .LE. 0 ) THEN
               WRITE( LOGDEV, * )
               WRITE( XMSG, '(A,I3,A)' ),
     &             'No Emission Streams have been selected for group ',IDIAG,
     &             ' of the emission diagnostic input. Please correct.'
               CALL M3EXIT( 'Map_Emiss_Diag', 0, 0, XMSG, XSTAT1 )     
           END IF
   
           ! Expand Desid_Diag_Streams_Nml to discover all matching streams
           STREAM_VECTOR = .FALSE.
           Expand_Stream = .FALSE.
           DO I = 1,NSTREAM
             IF ( Desid_Diag_Streams_Nml( IDIAG,I )(1:1) .EQ. '*' ) THEN
                 Expand_Stream = .TRUE.
                 Desid_Diag_Streams_Nml( IDIAG,I ) = 
     &                 Desid_Diag_Streams_Nml( IDIAG,I )(2:32)//' '
             END IF
             IF ( Desid_Diag_Streams_Nml( IDIAG,I ) .EQ. 'ALL' ) THEN
                 Expand_Stream = .TRUE.
             END IF
             CALL DESID_GET_RULE_STREAMS( Desid_Diag_Streams_Nml( IDIAG,I ),IDIAG,
     &                                    STREAM_VECTOR, LREMOVE, LERROR )

             ! Stop CMAQ if there was an error setting the emission streams
             IF ( LERROR ) THEN
               WRITE( LOGDEV, * )
               WRITE( XMSG, '(A,A,A)' ),
     &             'CMAQ will crash until an error specifying the ',
     &             'DESID diagnostics is fixed. Detailed information is ',
     &             'in the processor log files.'
               CALL M3EXIT( 'Map_Emiss_Diag', 0, 0, XMSG, XSTAT1 )     
             END IF

             ! Determine whether this diagnostic is a sum or not and
             ! assign name accordingly
             IF ( Expand_Stream ) THEN
                 ! Create a separate diagnostic file for every stream
                 ! in stream_vector
                 DO ISRM = 1,DESID_N_SRM
                     IF ( STREAM_VECTOR( ISRM ) ) THEN
                        ! Check for existing entries
                        DESID_N_DIAG = DESID_N_DIAG + 1
                        JDIAG = DESID_N_DIAG
            
                        ! Save the number of
                        ! streams selected for this diagnostic and a mask for
                        ! mapping each stream to each diagnostic
                        DESID_DIAG_N_STREAM( JDIAG ) = 1
                        DESID_DIAG_STREAM_MASK( ISRM,JDIAG ) = .TRUE.
                        DESID_DIAG_FORMAT( JDIAG ) = Desid_Diag_Fmt_Nml( IDIAG )
                        IF ( DESID_DIAG_FORMAT( JDIAG ) .EQ. 'COLSUM' .OR.
     &                       DESID_DIAG_FORMAT( JDIAG ) .EQ. 'LAYER1' ) 
     &                       DESID_DIAG_LAYS( JDIAG ) = 1
   
                        DESID_DIAG_LAB( JDIAG ) = DESID_STREAM_LAB( ISRM )
                        WRITE( CDIAG, '(I0)' ), IDIAG
                        DESID_DIAG_FILENAME( JDIAG ) = TRIM( PREFIX ) // TRIM(CDIAG) //
     &                        '_' // TRIM( DESID_DIAG_LAB( JDIAG ) ) // TRIM( SUFFIX )
                        DESID_DIAG_SUM( JDIAG ) = 0
            
                        ! Build Species Selection Array
                        SPEC_VECTOR = DESID_STREAM_DIFF( :,ISRM )
                        CALL DESID_DIAG_MAP_SPEC( Desid_Diag_Spec_Nml( IDIAG,: ), IDIAG, 
     &                             SPEC_VECTOR, JDIAG )
            
                     END IF
                 END DO
             ELSE
                 ! Check for existing entries. 
                 DESID_N_DIAG = DESID_N_DIAG + 1
                 JDIAG = DESID_N_DIAG
                 NSUM = NSUM + 1
                 DESID_DIAG_SUM( JDIAG ) = NSUM
                 
                 ! Save the number of
                 ! streams selected for this diagnostic and a mask for
                 ! mapping each stream to each diagnostic
                 DESID_DIAG_N_STREAM( JDIAG ) = COUNT( STREAM_VECTOR )
                 DESID_DIAG_STREAM_MASK( :,JDIAG ) = STREAM_VECTOR
                 DESID_DIAG_FORMAT( JDIAG ) = Desid_Diag_Fmt_Nml( IDIAG )
                 IF ( DESID_DIAG_FORMAT( JDIAG ) .EQ. 'COLSUM' .OR.
     &                DESID_DIAG_FORMAT( JDIAG ) .EQ. 'LAYER1' ) 
     &                DESID_DIAG_LAYS( JDIAG ) = 1
   
                 DESID_DIAG_LAB( JDIAG ) = Desid_Diag_Streams_Nml( IDIAG,I )
                 WRITE( CDIAG, '(I0)' ), IDIAG
                 DESID_DIAG_FILENAME( JDIAG ) = TRIM( PREFIX ) // TRIM(CDIAG) //
     &                 '_' // TRIM( DESID_DIAG_LAB( JDIAG ) ) // TRIM( SUFFIX )
   
                 ! Build Species Selection Array
                 SPEC_VECTOR = .FALSE.
                 DO ISRM = 1,DESID_N_SRM
                    IF ( STREAM_VECTOR( ISRM ) ) 
     &                   SPEC_VECTOR = SPEC_VECTOR .OR. DESID_STREAM_DIFF( :,ISRM )
                 END DO
                 CALL DESID_DIAG_MAP_SPEC( Desid_Diag_Spec_Nml( IDIAG,: ), IDIAG, 
     &                     SPEC_VECTOR, JDIAG )
             END IF
   
           END DO
        END DO
        
        ! Reduce size of each vector to length DESID_N_DIAG
        DESID_DIAG_N_STREAM    = DESID_DIAG_N_STREAM( 1:DESID_N_DIAG )
        DESID_DIAG_STREAM_MASK = DESID_DIAG_STREAM_MASK( :,1:DESID_N_DIAG )
        DESID_DIAG_FORMAT      = DESID_DIAG_FORMAT( 1:DESID_N_DIAG )
        DESID_DIAG_FILENAME    = DESID_DIAG_FILENAME( 1:DESID_N_DIAG )
        DESID_DIAG_LAB         = DESID_DIAG_LAB( 1:DESID_N_DIAG )
        DESID_DIAG_LAYS        = DESID_DIAG_LAYS( 1:DESID_N_DIAG )
        DESID_DIAG_SUM         = DESID_DIAG_SUM( 1:DESID_N_DIAG )
   
        ! Populate DESID_DIAG_SPEC from DESID_DIAG_SPEC_BUFF
        ALLOCATE( DESID_DIAG_SPEC( DESID_N_DIAG ) )
        DO IDIAG = 1,DESID_N_DIAG
           NSPEC  = DESID_DIAG_SPEC_BUFF(IDIAG)%NSPEC
           NPAIRS = DESID_DIAG_SPEC_BUFF(IDIAG)%NPAIRS
           DESID_DIAG_SPEC(IDIAG)%NSPEC  = NSPEC
           DESID_DIAG_SPEC(IDIAG)%NPAIRS = NPAIRS

           IF ( NSPEC .GT. 0 ) THEN
               ALLOCATE( DESID_DIAG_SPEC( IDIAG )%SPEC( NSPEC ) )
               ALLOCATE( DESID_DIAG_SPEC( IDIAG )%UNITS( NSPEC ) )
               ALLOCATE( DESID_DIAG_SPEC( IDIAG )%MAP_toDIFF( NPAIRS ) )
               ALLOCATE( DESID_DIAG_SPEC( IDIAG )%MAP_toDIAG( NPAIRS ) )
        
               DESID_DIAG_SPEC(IDIAG)%SPEC  = DESID_DIAG_SPEC_BUFF( IDIAG )%SPEC  
               DESID_DIAG_SPEC(IDIAG)%UNITS = DESID_DIAG_SPEC_BUFF( IDIAG )%UNITS 
               DESID_DIAG_SPEC(IDIAG)%MAP_toDIFF = DESID_DIAG_SPEC_BUFF( IDIAG )%MAP_toDIFF
               DESID_DIAG_SPEC(IDIAG)%MAP_toDIAG = DESID_DIAG_SPEC_BUFF( IDIAG )%MAP_toDIAG
           END IF
        END DO
        DEALLOCATE ( DESID_DIAG_SPEC_BUFF )

        ! Write Details of Diagnostic Output to Log File
        WRITE( LOGDEV, * )
        WRITE( LOGDEV, '(/,/,5x,A)' ),'|> EMISSIONS DIAGNOSTIC REPORT:'
        WRITE( LOGDEV, '(5x,A)'   ),'=================================='
        DO IDIAG = 1,DESID_N_DIAG
           WRITE( LOGDEV, * )
           WRITE( LOGDEV,'(5x,A19,2x,A)' ),'Diagnostic File: ',TRIM(DESID_DIAG_LAB( IDIAG ))
           WRITE( LOGDEV,'(10x,A)' ), 'Stream Members'
           DO I = 1,DESID_N_SRM
               IF ( DESID_DIAG_STREAM_MASK( I,IDIAG ) ) WRITE( LOGDEV,'(15x,A32)'),DESID_STREAM_LAB(I)
           END DO
           WRITE( LOGDEV,'(10x,A)' ), 'Diagnostic Species'
           DO I = 1,DESID_DIAG_SPEC( IDIAG )%NSPEC
               WRITE( LOGDEV,'(15x,A16)'),DESID_DIAG_SPEC( IDIAG )%SPEC( I )
           END DO
        END DO
        
        ! Create I/O Logicals and Populate with full Filepaths
        ALLOCATE( DESID_DIAG_LOGICAL( DESID_N_DIAG ) )
        DO IDIAG = 1,DESID_N_DIAG
           IF ( OUTDIR .NE. '' ) THEN
             DESID_DIAG_FILENAME(IDIAG) = TRIM( OUTDIR ) // '/' // DESID_DIAG_FILENAME(IDIAG)
           END IF
           WRITE( DESID_DIAG_LOGICAL( IDIAG ),'( "DESID_",I3.3 )' ) IDIAG
#ifndef mpas
           VALUE = SETENVVAR( DESID_DIAG_LOGICAL( IDIAG ), DESID_DIAG_FILENAME( IDIAG ) )
#endif
        END DO
   
        ! Allocate Minimal Necessary Space for VDEMIS_DIAG
        ALLOCATE( MAP_DIAGtoVDEMIS( N_SPC_DIFF,DESID_N_DIAG ) )
        MAP_DIAGtoVDEMIS = 0
        NSPEC = 0
        LAYS = 1
   
        DO IDIAG = 1,DESID_N_DIAG
            IF ( DESID_DIAG_SUM( IDIAG ) .GT. 0 ) THEN
              DO ISPEC = 1,DESID_DIAG_SPEC( IDIAG )%NSPEC
                 NSPEC = NSPEC + 1
                 MAP_DIAGtoVDEMIS( ISPEC,IDIAG ) = NSPEC
              END DO
              IF ( DESID_DIAG_FORMAT( IDIAG ) .EQ. '3D' ) LAYS = NLAYS
            ENDIF
        END DO
        ALLOCATE( VDEMIS_DIAG( NSPEC,NCOLS,NROWS,LAYS ) )
   
        END SUBROUTINE DESID_INIT_DIAG
   
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       SUBROUTINE DESID_OPEN_GR3D ( JDATE, JTIME )
   
C  7 Mar 02 - J.Young: add units string variations
C 29 Oct 05 - J.Young: dyn. layers
C 19 Feb 08 - David Wong: add DESID_TRAC = .TRUE. when EMIS_TRAC_1 exists
C 21 Jun 10 - J.Young: convert for Namelist redesign
C 16 Feb 11 - S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
  
      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE AERO_DATA, only : AEROMODE, N_MODE, MGPG, GPKG
      USE VDIFF_MAP, only : N_SPC_DIFF, DIFF_SPC, DIFF_MASK_AERO
#ifdef mpas
      use util_module, only : index1, upcase
      use mio_module
#endif
  
      IMPLICIT NONE
  
      INCLUDE SUBST_FILES_ID  ! file name parameters
  
C Arguments:
  
      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      INTEGER      NLAY_FILE  ! keep a running maximum of the layers from the gridded files
  
C Local variables:
  
      CHARACTER( 16 ) :: PNAME = 'OPEMIS'
      CHARACTER(200 ) :: XMSG
      CHARACTER( 16 ) :: UNITSCK
  
      LOGICAL ::   LAERO
      LOGICAL ::   WRFLG = .FALSE.
      INTEGER      STATUS, IOS
      INTEGER      V, N, S, ISRM, ITRAC, IGR, IVAR, X    

      integer :: floc, loc_nvars, loc_nlays, loc_date, loc_time
      character (16), allocatable :: loc_vnames(:), loc_units(:)
      logical :: file_exist

C-----------------------------------------------------------------------
  
      CALL LOG_SUBHEADING( LOGDEV,'Open Gridded Emissions' )
  
      DESID_GRID_LAYS = 0
  
C Open All Tracer Emission Files
      DO ISRM = 1,DESID_N_SRM
        IF ( DESID_STREAM_TYPE( ISRM ) .EQ. 'TRAC' ) THEN
           ITRAC = ITSRM( ISRM )
#ifdef mpas
           floc = mio_search (DESID_STREAM_NAME( ISRM ))
           if (floc > 0) then
              file_exist = .true.
              loc_nvars = mio_file_data(floc)%nvars
              loc_nlays = mio_file_data(floc)%nlays
              allocate (loc_vnames(loc_nvars), loc_units(loc_nvars), stat=status)
              loc_vnames = mio_file_data(floc)%var_name
              loc_units  = mio_file_data(floc)%units
           else
              file_exist = .false.
           end if
#else  
           IF ( .NOT. OPEN3( DESID_STREAM_NAME( ISRM ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open tracer file'
              CALL M3MESG( XMSG )
              file_exist = .false.
           ELSE
              file_exist = .true.
             ! Assign Tracer Emissions Species
             IF ( .NOT. DESC3( DESID_STREAM_NAME( ISRM ) ) ) THEN
                XMSG = 'Could not get '// DESID_STREAM_NAME( ISRM ) // ' file description'
                CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
             END IF
  
             loc_nvars = nvars3d
             loc_nlays = nlays3d
             allocate (loc_vnames(loc_nvars), loc_units(loc_nvars), stat=status)
             loc_vnames = VNAME3d(1:loc_nvars)
             loc_units  = UNITS3D(1:loc_nvars)
           END IF
#endif  

           IF (FILE_EXIST) THEN
             ! Save Tracer Variables for Use in Emissions Species Check
             ! Routine
             DESID_EMVAR( ISRM )%LEN = loc_nvars
             ALLOCATE ( DESID_EMVAR( ISRM )%ARRY( loc_nvars ), STAT = STATUS )
             ALLOCATE ( DESID_EMVAR( ISRM )%UNITS( loc_nvars ), STAT = STATUS )

             DESID_EMVAR( ISRM )%ARRY = loc_vnames
  
             ! Assign Layers to Common Vector
             DESID_GRID_LAYS( ISRM ) = loc_nlays
              
             ! Check Units For Consistency
             UNITSCK = 'BLANK'
             DO N = 1, loc_nvars
                V = INDEX1( loc_vnames( N ), N_SPC_DIFF, DIFF_SPC )
                IF ( V .NE. 0 ) THEN
                  IF ( UNITSCK .EQ. 'BLANK' ) UNITSCK = loc_units( N )
                  IF ( loc_units( N ) .NE. UNITSCK ) THEN
                     XMSG = 'Units not uniform on ' // DESID_STREAM_LAB( ISRM )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
                END IF
                DESID_EMVAR( ISRM )%UNITS( N ) = loc_units( N )
             END DO 
           END IF
           deallocate (loc_vnames, loc_units) 
        END IF   ! tracer emissions
      END DO
  
  
C Open Gridded Emission Files (for gas chem, aerosols and non-reactive species)
      DO ISRM = 1,DESID_N_SRM
        IF ( DESID_STREAM_TYPE( ISRM ) .EQ. 'GRID' ) THEN
          IGR = IGSRM( ISRM )

#ifdef mpas 
          floc = mio_search (DESID_STREAM_NAME( ISRM ))
          call mio_timestamp_to_julian (mio_file_data(floc)%timestamp(1), loc_date, loc_time)
          loc_nvars = mio_file_data(floc)%nvars
          loc_nlays = mio_file_data(floc)%nlays
          allocate (loc_vnames(loc_nvars), loc_units(loc_nvars), stat=status)
          loc_vnames = mio_file_data(floc)%var_name
          loc_units  = mio_file_data(floc)%units
#else
          IF ( .NOT. OPEN3( DESID_STREAM_NAME( ISRM ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open file ' // DESID_STREAM_NAME( ISRM )
              CALL M3MESG( XMSG )
          ELSE
  
            IF ( .NOT. DESC3( DESID_STREAM_NAME( ISRM ) ) ) THEN
              XMSG = 'Could not get '// DESID_STREAM_NAME( ISRM ) // ' file description'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
          END IF
          loc_nvars = NVARS3D
          loc_nlays = nlays3d
          allocate (loc_vnames(loc_nvars), loc_units(loc_nvars), stat=status)
          loc_vnames = VNAME3d(1:loc_nvars)
          loc_units  = UNITS3D(1:loc_nvars)
          loc_date   = sdate3d
#endif
            
          ! Let the date for this emission stream come from the model
          ! by default or the file if the user wishes to override the
          ! default for representative day files
          DESID_STREAM_DATE( ISRM ) = JDATE
          IF ( DESID_STREAM_SYM_DATE( ISRM ) )  DESID_STREAM_DATE( ISRM ) = loc_date
 
          ! Save Area Source Species Names For Use in Emissions Species
          ! Check Routine
          DESID_EMVAR( ISRM )%LEN = loc_nvars
          ALLOCATE ( DESID_EMVAR( ISRM )%ARRY( loc_nvars ), STAT = STATUS )
          ALLOCATE ( DESID_EMVAR( ISRM )%UNITS( loc_nvars ), STAT = STATUS )
          DESID_EMVAR( ISRM )%ARRY = loc_vnames
         
          ! Assign Area Source Emission Species
          DESID_GRID_LAYS( ISRM ) = loc_nlays

          ! Assign Units
          DO IVAR = 1,loc_nvars
                ! A match has been found
                UNITSCK = loc_units( IVAR )
                CALL UPCASE( UNITSCK )
                DESID_EMVAR( ISRM )%UNITS( IVAR ) = loc_units( IVAR )
          END DO
          deallocate (loc_vnames, loc_units)
        END IF    ! (gridded emission file)
      END DO
  
      RETURN
      END SUBROUTINE DESID_OPEN_GR3D
  
  
!-----------------------------------------------------------------------
      SUBROUTINE DESID_OPEN_DIAG( JDATE, JTIME, TSTEP )
!       This subroutine opens diagnostic files for printing the
!       emission rates input to CMAQ after scaling by user emission
!       rules. The rates are put on files dedicated to each individual
!       emissions stream. This adds user flexibility in being able to
!       toggle the diagnostic output for streams individually and
!       reduces the storage space needed for most use cases (presumably
!       users will rarely want to output the data from all of the
!       emissions streams).
!-----------------------------------------------------------------------
      USE UTILIO_DEFN
      USE GRID_CONF
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_SPC, DIFF_MASK_GAS, DIFF_MASK_NR,
     &                      DIFF_MASK_AERO, DIFF_MASK_NUM, DIFF_MASK_SRF
  
      IMPLICIT NONE
  
      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP
  
      CHARACTER( 16 ) :: PNAME = 'OPEN_EMISS_DIAG'
      CHARACTER( 200 ) :: XMSG
  
      INTEGER :: IDIAG, IVAR, NLAYERS, V, ISPEC
      
#ifndef mpas
      DO IDIAG = 1,DESID_N_DIAG
      
        ! Test whether or not each file should be opened
        IF ( DESID_DIAG_FORMAT( IDIAG ) .NE. 'OFF' ) THEN
  
          ! Make File 2D by Default, but 3D if the user requests it
          NLAYERS = 1
          IF ( DESID_DIAG_FORMAT( IDIAG ) .EQ. '3D' ) NLAYERS = DESID_LAYS
  
          ! Set output file characteristics based on COORD.EXT and 
          ! open diagnostic file
          FTYPE3D = GRDDED3
          SDATE3D = STDATE
          STIME3D = STTIME
          TSTEP3D = TSTEP
  
          NVARS3D = DESID_DIAG_SPEC( IDIAG )%NSPEC
          NCOLS3D = GL_NCOLS
          NROWS3D = GL_NROWS
          NLAYS3D = NLAYERS
          NTHIK3D = 1
          GDTYP3D = GDTYP_GD
          P_ALP3D = P_ALP_GD
          P_BET3D = P_BET_GD
          P_GAM3D = P_GAM_GD
          XORIG3D = XORIG_GD
          YORIG3D = YORIG_GD
          XCENT3D = XCENT_GD
          YCENT3D = YCENT_GD
          XCELL3D = XCELL_GD
          YCELL3D = YCELL_GD
          VGTYP3D = VGTYP_GD
          VGTOP3D = VGTOP_GD
          VGLVS3D( 1:NLAYS3D+1 ) = VGLVS_GD( 1:NLAYS3D+1 )
          GDNAM3D = GRID_NAME  ! from HGRD_DEFN
  
          V = 0
          DO IVAR = 1,DESID_DIAG_SPEC( IDIAG )%NSPEC
             V = V + 1
  
             VTYPE3D( V ) = M3REAL
             VNAME3D( V ) = DESID_DIAG_SPEC( IDIAG )%SPEC( IVAR )
             UNITS3D( V ) = DESID_DIAG_SPEC( IDIAG )%UNITS( IVAR )
             VDESC3D( V ) = 'Emission Rate of ' // TRIM( VNAME3D( V ) )
     &                       // ' from ' // TRIM(DESID_DIAG_LAB( IDIAG )) // ' emissions'
          END DO
          
          FDESC3D( 1 ) = 'Instantaneous pollutant emissions from stream: ' // 
     &                   TRIM(DESID_DIAG_LAB( IDIAG ) ) 
          FDESC3D( 2:MXDESC3 ) = ''
  
          ! Open emissions stream diagnostic file
          IF ( .NOT. OPEN3( DESID_DIAG_LOGICAL( IDIAG ), FSNEW3, PNAME ) ) THEN
             XMSG = 'Could not create the ' // TRIM( DESID_DIAG_FILENAME( IDIAG ) ) // ' file'
             CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF
  
        END IF   ! DESID_STREAM_LDIAG?
      END DO     ! IDIAG
#else
         call mio_fcreate ('EMIS_DIAG',512 )
#endif
  
  
      END SUBROUTINE DESID_OPEN_DIAG

  
C-----------------------------------------------------------------------
       END MODULE DESID_MODULE
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
