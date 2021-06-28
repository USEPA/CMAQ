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
C     16 NOv 2018 S.Napelenok: ISAM implementation
C     01 Feb 2019 D.Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses, replaced IOS with LOGDEV when calling GET_ENV
C     15 May 2019 D.Wong: Put the check for using marine gas emission or not in RUNTIME_VAR.F
C     4 Oct 2019 D.Wong: fixed the time advancement, NEXTIME, for a multi-day run in subroutine
C                        GRIDEMIS
C     22 Nov 2019 F. Sidi: Re-introduced date override variable for representative
C                          Day Emissions
C     16 Dec 2019 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE EMIS_VARS
      USE VDIFF_MAP, ONLY : N_SPC_DIFF
      USE UTILIO_DEFN

      IMPLICIT NONE

      PUBLIC EMIS_INIT, GET_EMIS, EM_GRID_LAYS
      
      PRIVATE
      
     
      INTEGER, ALLOCATABLE, SAVE :: EM_GRID_LAYS( : ) ! no. of area emission layers
      
      LOGICAL, ALLOCATABLE, SAVE :: EM_MASK_AERO( : )! Store the location of aerosol species in master 
                                                     ! emissions vector
      

      LOGICAL, SAVE :: EM_TRAC    ! do tracer emissions?
      REAL,    SAVE :: DT         ! TSTEP (output) in sec

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
         USE AERO_DATA           ! access subroutine map_pmemis

#ifdef isam
         USE SA_DEFN, ONLY : SA_VDEMIS_DIFF, NTAG_SA, SA_VDEMIS_CONV, NSPC_SA,
     &                       SA_VDEMIS_CONV_OTHER
#endif

#ifdef sens
        USE DDM3D_DEFN, ONLY: NP, NPMAX, SVDEMIS_DIFF
#endif

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP   ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

C Parameters:
                                        
C Local Variables:

         CHARACTER( 16 ) :: PNAME = 'EMIS_INIT'
         CHARACTER( 80 ) :: VARDESC   ! env variable description
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER V, STATUS, ISRM

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

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
         IF ( N_EM_SRM .EQ. 0 ) THEN 
            XMSG = 'No Emissions Streams Have Been Selected.'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .TRUE.; RETURN
         END IF

C Open Area Emissions files
         CALL OPEMIS ( JDATE, JTIME, EM_TRAC )

C Get number of emissions layers
         IF ( EMLAYS_MX .LE. 0 ) THEN
            ! Find The Largest Gridded Emission Layer And Let That be
            ! the initial top.
            EMLAYS = MAXVAL( EM_GRID_LAYS(:) ) 
            
            ! If there are 3D (inline point or Lightning) sources, 
            ! revise the top to be the model top.
            IF ( NPTGRPS .GT. 0 .OR. LTNG_NO ) EMLAYS = NLAYS

            ! Make sure the top is not greater than the model top
            EMLAYS = MAX( MIN( EMLAYS, NLAYS ), 1 )
         ELSE
           ! Make sure the top is not greater than the model top
           EMLAYS = MIN( EMLAYS_MX, NLAYS )

         END IF

         WRITE( LOGDEV,1009 ) EMLAYS, NLAYS
1009     FORMAT(    5X, 'Number of Emissions Layers:         ', I3
     &           /  5X, 'out of total Number of Model Layers:', I3 )


C Initialize 3D Point Source Emissions 
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Point Emissions' )
         IF ( .NOT. PT3D_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
 
C Initialize Online Biogenic Emissions
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Biogenic Emissions' )
         IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
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

C Map the Emissions Species Available on the Input Files To the
C Surrogates Identified by the User via the Namelists and Stop the model
C or Print Warnings if Mistakes Are Made.

         CALL EMIS_SPC_MAP( JDATE, JTIME )
         IF ( IO_PE_INCLUSIVE ) CALL OPEN_EMISS_DIAG( JDATE, JTIME, TSTEP )

C Allocate Space for Master Emissions Computation
         ALLOCATE ( VDEMIS_DIFF( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

#ifdef isam
         ALLOCATE ( SA_VDEMIS_DIFF( NSPC_SA,   EMLAYS,NCOLS,NROWS, NTAG_SA ),
     &              SA_VDEMIS_CONV( N_SPC_DIFF,EMLAYS,NCOLS,NROWS, NTAG_SA ), 
     &              SA_VDEMIS_CONV_OTHER( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ), 
     &                                                  STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'SA_VDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
#endif

#ifdef sens
         ALLOCATE ( SVDEMIS_DIFF( N_SPC_DIFF,EMLAYS,NCOLS,NROWS,NPMAX ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'SVDEMIS_DIFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
#endif

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
         USE HGRD_DEFN
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME
         USE AERO_DATA, ONLY : AERONUM_MAP, AEROSRF_MAP, DUSTOUTM

#ifdef isam
         USE SA_DEFN, ONLY : SA_VDEMIS_DIFF, ITAG, NTAG_SA, TAGSTREAMS,
     &                       STREAM_TO_TAG, OTHRTAG, TAGSTREAMS_TEMP,
     &                       TAGSTREAMS_NUM, SA_VDEMIS_CONV,
     &                       SA_VDEMIS_CONV_OTHER,
     &                       MAP_DIFFtoSA, SPC_NAME, NSPC_SA,
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

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL, POINTER :: CGRID( :,:,:,: )

C Local Variables:
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V, ISTR, ISRM, NL ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables
         REAL, ALLOCATABLE, SAVE :: VDEMIS_READ  ( :,:,:,: ) ! Emissions as they are provided by each stream
         REAL, ALLOCATABLE, SAVE :: VDEMIS_SCALED( :,:,:,: ) ! Emissions after user scaling, and spatial scaling
         REAL, ALLOCATABLE, SAVE :: VDEMIS_CONV  ( :,:,:,: ) ! Emissions after converting to units for VDIFF
         

         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ):: XMSG = ' '
         INTEGER         :: ERROR_NEG
         INTEGER, SAVE   :: WSTEP, MDATE, MTIME
         LOGICAL         :: WRTIME
         LOGICAL, SAVE   :: FIRST_TIME = .TRUE.

#ifdef isam
         INTEGER                    :: IDX, SPC, LAYER, RGN
         CHARACTER(96)              :: TXTSTRING
         REAL                       :: MINCHECK, TOTAL
#endif
C-----------------------------------------------------------------------

         IF ( FIRST_TIME ) THEN
            FIRST_TIME = .FALSE.

            ALLOCATE( VDEMIS_READ  ( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),
     &                VDEMIS_SCALED( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),
     &                VDEMIS_CONV  ( N_SPC_DIFF, EMLAYS,NCOLS,NROWS ) )

#ifdef isam
C Map Emissions streams to ISAM tags
            ALLOCATE( STREAM_TO_TAG( N_EM_SRM, NTAG_SA ),
     &                TAGS_PER_STREAM( N_EM_SRM ),
     &                MAP_DIFFtoSA ( NSPC_SA  ),
     &                TAGSTREAMS( NTAG_SA, N_EM_SRM ) )
            STREAM_TO_TAG   = 0
            TAGS_PER_STREAM = 0
            MAP_DIFFtoSA    = 0
            TAGSTREAMS      = 'empty'

#ifdef potvorto3
            ALLOCATE ( ISAM_PVO3_MAP( NTAG_SA ) )
            ISAM_PVO3_MAP = 0
#endif
            DO ITAG = 1, NTAG_SA-3

C Count the number of streams going to each ISAM tag
              TXTSTRING = TRIM(TAGSTREAMS_TEMP( ITAG ))
              TAGSTREAMS_NUM( ITAG ) = 1 + COUNT(TRANSFER(TXTSTRING, 'A', LEN(TXTSTRING)) == "," )

C Parse out the stream names the user wants tagged
              READ(TXTSTRING, *) TAGSTREAMS(ITAG,1:TAGSTREAMS_NUM( ITAG ))

C Find emissions stream in the list of available streams
              DO ISRM = 1, TAGSTREAMS_NUM( ITAG )
                 IF ( TAGSTREAMS(ITAG,ISRM) .EQ. 'PVO3' ) THEN
#ifdef potvorto3
                    IF ( L_OZONE ) THEN
                      ISAM_PVO3_MAP(ITAG) = 1
                      CYCLE
                    ELSE
                      XMSG = " Must specifiy OZONE ISAM TAG CLASS" //
     &                       " in isam_control to track PVO3 "
                      CALL M3EXIT( 'ISAM_PVO3', 1, 1, XMSG, XSTAT1 )
                    ENDIF
#else
                    XMSG = " To track PVO3, model must be compiled" //
     &                     " with -Dpotvorto3 option. Check Makefile" //
     &                     " and/or build script."
                    CALL M3EXIT( 'ISAM_PVO3', 1, 1, XMSG, XSTAT1 )
#endif
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

                 IDX = INDEX1(TAGSTREAMS(ITAG,ISRM),N_EM_SRM,EM_FILE_LAB )
                 IF ( IDX .EQ. 0 ) THEN
                   XMSG = " User specified ISAM tag - " //
     &                    TRIM( TAGSTREAMS(ITAG,ISRM) )// 
     &                    " - not found in available emissions streams "
                   CALL M3EXIT( 'ISAM_STREAMS', 1, 1, XMSG, XSTAT1 )
                 ELSE
                   TAGS_PER_STREAM( IDX ) = TAGS_PER_STREAM( IDX ) + 1
                   STREAM_TO_TAG(IDX,TAGS_PER_STREAM( IDX )) = ITAG
                 END IF
              END DO

            END DO

C Develop a map of emitted/diffused ISAM species
            MAP_DIFFtoSA = 0
            DO SPC = 1, NSPC_SA
              MAP_DIFFtoSA( SPC ) = INDEX1(TRIM(SPC_NAME(SPC,1)), N_SPC_DIFF, DIFF_SPC )
            END DO

C Initialize geographic regions for ISAM tagging
            ALLOCATE( ISAMRGN( NTAG_SA, N_EM_RGN ) )
            ISAMRGN = 'EVERYWHERE'
            ALLOCATE( ISAMRGN_MAP( NTAG_SA, N_EM_RGN ) )
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
                IDX = INDEX1(ISAMRGN(ITAG,ISRM),N_EM_RGN, EM_REGIONS%LABEL )
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
            ALLOCATE( STREAM_TO_SENS( N_EM_SRM, NPMAX ),
     &                SENS_PER_STREAM( N_EM_SRM ))
            STREAM_TO_SENS = 0
            SENS_PER_STREAM = 0

            DO NP = 1, NPMAX
              IF ( IPT ( NP ) .EQ. 3 ) THEN ! This is an emissions sensitivity
                IF ( S_NSTREAMS(NP) .EQ. 99 ) THEN ! all emissions streams
                  S_NSTREAMS(NP) = N_EM_SRM
                  S_STREAMLBL(NP,1:N_EM_SRM)=EM_FILE_LAB(1:N_EM_SRM)
                END IF
                DO ISRM = 1, S_NSTREAMS(NP)
                  SENNUM = INDEX1(S_STREAMLBL(NP,ISRM),N_EM_SRM,EM_FILE_LAB)
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
            ! Initialize Diagnostic Write Counter
            WSTEP = 0 
            MDATE = STDATE
            MTIME = STTIME
         END IF  ! First Time

C Initialize Emissions Array
         WRTIME = .FALSE.
         WSTEP  = WSTEP + TIME2SEC( TSTEP( 2 ) )
         WRTIME = ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) )
         IF ( WRTIME ) THEN 
           WSTEP = 0
           CALL NEXTIME( MDATE, MTIME,  TSTEP( 1 ) )
         END IF

#ifdef isam
         SA_VDEMIS_DIFF = 0.0
#endif

#ifdef sens
        SVDEMIS_DIFF = 0.0
#endif

C Retrieve Emissions from All Stream Types
         IF ( N_EM_SRM .GT. 0 ) THEN
         VDEMIS_DIFF = 0.0

         DO ISRM = 1,N_EM_SRM
           VDEMIS_READ = 0.0

           SELECT CASE ( EM_FILE_ITYPE( ISRM ) )
             
             ! Retrieve Gridded and Tracer Emissions
             CASE ( 1, 3 )
               NL = EM_GRID_LAYS( ISRM )
               CALL GRIDEMIS( JDATE, JTIME, TSTEP, EM_FILE_NAME( ISRM ), 
     &                        NL, ISRM, VDEMIS_READ )
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve Point Source Emissions Streams
             CASE ( 2 )
               NL = 1
               CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, EM_SURR( :,ISRM ), ISRM, VDEMIS_READ, NL )
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )
             
             CASE ( 4 )
               NL = 1
               CALL GET_BEIS ( JDATE, JTIME, TSTEP )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = VDEMIS_BI( MAP_EMtoSURR( ISTR,ISRM ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Marine Gas Emissions
             CASE ( 5 )
               NL = 1
               CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = VDEMIS_MG( MAP_EMtoSURR( ISTR,ISRM ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Lightning NO Emissions
             CASE ( 6 )
               NL = EMLAYS
               CALL GET_LTNG ( JDATE, JTIME, TSTEP )
               DO L = 1,NL
                  FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                     VDEMIS_READ( ISTR,L,:,: ) = VDEMIS_LT( :,:,L ) 
                  END FORALL
               END DO
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Sea Spray Aerosol Emissions 
             CASE ( 7 )
               NL = 1
               CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( 1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = SSOUTM( MAP_EMtoSURR( ISTR,ISRM ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve Wind-Blown Dust Emissions 
             CASE ( 8 )
               NL = 1
               CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = DUSTOUTM( MAP_EMtoSURR( ISTR,ISRM ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

           END SELECT
           
           ! Convert All Emissions to VDIFF Units, Check for negative
           ! values, write out diagnostic file (if requested) and apply
           ! emissions to output array
           CALL EMISS_CONV_DIFF( VDEMIS_SCALED, NL, VDEMIS_CONV )
           IF ( EM_FILE_LDIAG( ISRM ) .NE. 'FALSE' .AND. WRTIME ) 
     &                     CALL WRITE_EMISS_DIAG( MDATE, MTIME, VDEMIS_CONV, ISRM, NL )

           IF ( EM_FILE_LAPPLY( ISRM ) ) 
     &          VDEMIS_DIFF( :,1:NL,:,: ) = VDEMIS_DIFF( :,1:NL,:,: ) + VDEMIS_CONV( :,1:NL,:,: )

#ifdef isam
           IF ( EM_FILE_LAPPLY( ISRM ) ) THEN
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
                   ! contributions are applied using the EM_REG_FAC
                   ! array with corresponding map indices.
                   ! Loop Over Altitude, Species and Regions to Add
                   ! emissions to corresponding tag, ITAG.
                   DO RGN = 1, ISAMRGN_NUM( ITAG )
                     DO LAYER = 1, NL
                       DO SPC = 1, N_SPC_DIFF
                          SA_VDEMIS_CONV( SPC,LAYER,:,:,ITAG ) = 
     &                              SA_VDEMIS_CONV( SPC,LAYER,:,:,ITAG )
     &                             + VDEMIS_CONV( SPC,LAYER,:,: )
     &                               * EM_REG_FAC(:,:,ISAMRGN_MAP(ITAG,RGN))

                          SA_VDEMIS_CONV_OTHER( SPC,LAYER,:,: ) = 
     &                              SA_VDEMIS_CONV_OTHER( SPC,LAYER,:,: )
     &                            - VDEMIS_CONV( SPC,LAYER,:,: )
     &                              * EM_REG_FAC(:,:,ISAMRGN_MAP(ITAG,RGN))

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
                  XMSG = " ISAM mass balance error for emissions - " //
     &                     TRIM( EM_REGIONS(ISRM)%LABEL ) //
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
             DO L = 1, EMLAYS
               SVDEMIS_DIFF( S,L,:,:,NP ) = 
     &                        SVDEMIS_DIFF( S,L,:,:,NP )
     &                      * IREGION( :,:,L,NP )
     &                      * IPARM( NP, DIFF_MAP( S ) )
             END DO
           END DO
         END DO
#endif

         ERROR_NEG     = EMISS_NEG_CHECK( VDEMIS_DIFF, 1, EMLAYS )
         
         ! Write Out the Sum of All Emissions to Gridded Diagnostic File
         IF ( EM_FILE_LDIAG( N_EM_SRM+1 ) .NE. 'FALSE' .AND. WRTIME ) 
     &         CALL WRITE_EMISS_DIAG( MDATE, MTIME, VDEMIS_DIFF, N_EM_SRM+1, EMLAYS )

         END IF

         RETURN

         END SUBROUTINE GET_EMIS

!-----------------------------------------------------------------------
         SUBROUTINE GRIDEMIS ( JDATE, JTIME, TSTEP, EMIS_FNAME, FILE_LAYS, FSTREAM, VDEMIS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME
         USE CENTRALIZED_IO_MODULE, ONLY: interpolate_var

         IMPLICIT NONE

! Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

! Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: FSTREAM
         CHARACTER( 100 ), INTENT( IN ) :: EMIS_FNAME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)

! Output:
         REAL, INTENT( OUT )  :: VDEMIS( :,:,:,: )

! Local Variables:
         INTEGER   C, R, L, N, S, V, ISTR ! loop induction variables
         INTEGER   S_STRT, S_END, FILE_LAYS, NDATE, NTIME

         REAL, ALLOCATABLE, SAVE :: BUFF( :,:,: )
         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GRIDEMIS'
         CHARACTER( 200 ) :: XMSG = ' '
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, ALLOCATABLE, SAVE :: LOC_STDATE(:)

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            ! Get Domain Information for Emissions Read Routine

            ! Allocate Persistent Variables
            ALLOCATE( BUFF( NCOLS,NROWS,MAXVAL( EM_GRID_LAYS ) ) )
            ALLOCATE( LOC_STDATE( SIZE(EM_FILE_DATE) ) )

            LOC_STDATE = STDATE
         END IF    !FirstTime
 
C Ensure that the model and emissions timestamp dates stay synchronized
         IF (LOC_STDATE(FSTREAM) .NE. JDATE ) THEN
            NDATE = JDATE; NTIME = JTIME
            CALL NEXTIME( NDATE, NTIME, -TSTEP( 1 ) )       ! go back one output tstep
            CALL NEXTIME( EM_FILE_DATE( FSTREAM ), NTIME, TSTEP( 1 ) ) ! advance the start date one time step
            LOC_STDATE(FSTREAM) = EM_FILE_DATE( FSTREAM )
         END IF

C Read & Interpolate Emissions 
         VDEMIS = 0.0

         DO ISTR = 1, N_EMIS_ISTR 
           VNAME = EM_SURR( ISTR, FSTREAM )
           IF ( VNAME .EQ. '' ) CYCLE

           CALL INTERPOLATE_VAR (VNAME, EM_FILE_DATE( FSTREAM ), JTIME, BUFF, EMIS_FNAME)

C Store all emissions in mol/sec or g/sec and convert to ppmv/s later
           IF ( EMLAYS .GE. FILE_LAYS ) THEN 
             DO L = 1, FILE_LAYS
                VDEMIS( ISTR,L,:,: ) = BUFF( :,:,L )
             END DO
           ELSE
             DO L = 1, EMLAYS
                VDEMIS( ISTR,L,:,: ) = BUFF( :,:,L )
             END DO
             DO L = EMLAYS+1,FILE_LAYS
                VDEMIS( ISTR,L,:,: ) = VDEMIS( ISTR,L,:,: ) + BUFF( :,:,L )
             END DO
           END IF

         END DO   ! ISTR

         RETURN

      END SUBROUTINE GRIDEMIS

C-----------------------------------------------------------------------
      SUBROUTINE EMISS_SCALING( VDEMIS0, ISRM, NL, VDEMIS )

C     Apply region-dependent scaling of emissions rules.
C-----------------------------------------------------------------------
          
      IMPLICIT NONE

      INTEGER, INTENT( IN )   :: ISRM, NL
      REAL, INTENT( IN )      :: VDEMIS0( :,:,:,: )
      REAL, INTENT( OUT )     :: VDEMIS ( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: FAC1( :,:,:,: )
      INTEGER, ALLOCATABLE, SAVE :: NFAC( : )

      TYPE FAC_ST
          LOGICAL :: EXISTS  ! Does this source have any instructions with 
                             ! at least IFAC number of factors
          REAL, ALLOCATABLE    :: FAC( :,:,:,: ) ! Spatial-Dependent Scaling Factors 
          INTEGER              :: NISTR          ! Number of nonzero instructions for each stream
          INTEGER, ALLOCATABLE :: MAP_FACtoISTR( : )! Map the factors from the global 
                                                    ! structure to just the species 
                                                    ! relevant for this emission stream
      END TYPE FAC_ST
      TYPE ( FAC_ST ), ALLOCATABLE, SAVE :: EM_FAC1_ST( : )

      INTEGER ISTR, IFAC, L, NFAC_MAX, IRGN, OP, JSRM, ISTR_TMP, NISTR
      INTEGER, ALLOCATABLE :: MAP_FACtoISTR( : )
      REAL    FAC
      LOGICAL, SAVE :: FIRSTIME = .TRUE.


      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ALLOCATE( NFAC( N_EMIS_ISTR ) )

         ! Build Array for primary factor. This algorithm is optimized
         ! by assuming that most instructions only have one factor (i.e.
         ! the 'add' factor). So time will be saved by pre-populating
         ! the 5D array necessry to perform this operation.
         ALLOCATE( EM_FAC1_ST( N_EM_SRM ) )
         EM_FAC1_ST( : )%EXISTS = .FALSE.
         ALLOCATE( FAC1( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ) )
         ALLOCATE( MAP_FACtoISTR( N_EMIS_ISTR ) )

         DO JSRM = 1,N_EM_SRM
           NFAC( : ) = EM_FAC_ST( :,JSRM )%LEN
           IF ( ANY( NFAC( : ) .GE. 1 ) ) THEN
             ! Start Counting Instructions for this Emission Stream    
             FAC1 = 0.0
             ISTR_TMP = 0
             MAP_FACtoISTR = 0

             ! Loop through existing instructions
             DO ISTR = 1,N_EMIS_ISTR
               ! Determine whether Emissions for this combination exist
               IF ( EM_FAC_ST( ISTR,JSRM )%LEN .GT. 0 ) THEN
                 IRGN = EM_FAC_ST( ISTR,JSRM )%REG( 1 )
                 ISTR_TMP = ISTR_TMP + 1
                 MAP_FACtoISTR( ISTR_TMP ) = ISTR

                 DO L = 1,EMLAYS
                   IF ( IRGN .EQ. 1 ) THEN
                     ! Emissions are domain wide
                     FAC1( ISTR_TMP,L,:,: ) = EM_FAC_ST( ISTR,JSRM )%FAC( 1 )
                   ELSE
                     ! Regional-based scaling
                     FAC1( ISTR_TMP,L,:,: ) = EM_FAC_ST( ISTR,JSRM )%FAC( 1 )
     &                            * EM_REG_FAC( :,:,IRGN )
                   END IF
                 END DO
               END IF
             END DO

             ! Allocate factor array for the first factor
             EM_FAC1_ST( JSRM )%EXISTS = .TRUE.
             EM_FAC1_ST( JSRM )%NISTR = ISTR_TMP
             ALLOCATE( EM_FAC1_ST( JSRM )%FAC( ISTR_TMP,EMLAYS,NCOLS,NROWS ) )
             EM_FAC1_ST( JSRM )%FAC( 1:ISTR_TMP,:,:,: ) = FAC1( 1:ISTR_TMP,:,:,: )
             ALLOCATE( EM_FAC1_ST( JSRM )%MAP_FACtoISTR( ISTR_TMP ) )
             EM_FAC1_ST( JSRM )%MAP_FACtoISTR( 1:ISTR_TMP ) = MAP_FACtoISTR( 1:ISTR_TMP )
           
           END IF 
         END DO
         DEALLOCATE( MAP_FACtoISTR )

      END IF   ! First Time

      !! Time-Dependent Portion
      VDEMIS( :,1:NL,:,: ) = 0.0
      
      ! Skip Streams With no Emissions Whatsoever
      IF ( .NOT. EM_FAC1_ST( ISRM )%EXISTS ) RETURN

      ! For the Primary Scaling (IFAC = 1), apply the array operation
      NISTR = EM_FAC1_ST( ISRM )%NISTR
      DO ISTR = 1,NISTR
          VDEMIS( EM_FAC1_ST( ISRM )%MAP_FACtoISTR( ISTR ),1:NL,:,: ) = 
     &          VDEMIS0( EM_FAC1_ST( ISRM )%MAP_FACtoISTR( ISTR ),1:NL,:,: ) *
     &          EM_FAC1_ST( ISRM )%FAC( ISTR,1:NL,:,: )
      END DO

      
      ! For Additional Scaling Operations (IFAC > 1), Loop through Instructions 
      NFAC( : ) = EM_FAC_ST( :,ISRM )%LEN
      DO ISTR = 1,N_EMIS_ISTR
        ! Do Not bother with this row if there is no surrogate to map
        IF ( MAP_EMtoSURR( ISTR,ISRM ) .EQ. 0 .OR. NFAC( ISTR ) .EQ. 1 ) CYCLE

        DO IFAC = 2,NFAC( ISTR )      

          FAC  = EM_FAC_ST( ISTR,ISRM )%FAC( IFAC )
          OP   = EM_FAC_ST( ISTR,ISRM )%OP ( IFAC )
          IRGN = EM_FAC_ST( ISTR,ISRM )%REG( IFAC )

          IF ( IRGN .EQ. 1 ) THEN
            !Apply scaling using EM_FAC_ST 
            IF ( OP .EQ. 1 .AND. NFAC( ISTR ) .EQ. 1 ) THEN ! Add New Emission
                VDEMIS( ISTR,1:NL,:,: ) = VDEMIS0( ISTR,1:NL,:,: ) * FAC
            ELSE IF ( OP.EQ.1 ) THEN   ! Add Additional Emission 
                VDEMIS( ISTR,1:NL,:,: ) = VDEMIS( ISTR,1:NL,:,: ) +
     &                 VDEMIS0( ISTR,1:NL,:,: ) * FAC
            ELSE IF ( OP.EQ.2 ) THEN ! Multiply Emissions Rule
                VDEMIS( ISTR,1:NL,:,: ) = VDEMIS( ISTR,1:NL,:,: ) * FAC
            ELSE IF ( OP.EQ.3 ) THEN ! Overwrite Existing Rule
                VDEMIS( ISTR,1:NL,:,: ) = VDEMIS0( ISTR,1:NL,:,: ) * FAC
            END IF
          ELSE
              
            !Apply scaling using EM_FAC_ST and VDEMIS_RGN
            IF ( OP.EQ.1 ) THEN   ! Add Additional Emission 
              DO L = 1,NL
                VDEMIS( ISTR,L,:,: ) = VDEMIS( ISTR,L,:,: ) +
     &             VDEMIS0( ISTR,L,:,: ) * FAC * EM_REG_FAC( :,:,IRGN )
              END DO
            ELSE IF ( OP.EQ.2 ) THEN ! Multiply Emissions Rule
              DO L = 1,NL
                VDEMIS( ISTR,L,:,: ) = VDEMIS( ISTR,L,:,: ) * 
     &               (1.0 - EM_REG_FAC( :,:,IRGN ) ) +
     &             VDEMIS( ISTR,L,:,: ) * FAC * EM_REG_FAC( :,:,IRGN )
              END DO
            ELSE IF ( OP.EQ.3 ) THEN ! Overwrite Existing Rule
              DO L = 1,NL
                VDEMIS( ISTR,L,:,: ) = VDEMIS( ISTR,L,:,: ) *
     &               (1.0 - EM_REG_FAC( :,:,IRGN ) ) +
     &             VDEMIS0( ISTR,L,:,: ) * FAC * EM_REG_FAC( :,:,IRGN )
              END DO
            END IF

          END IF

        END DO
      END DO

      RETURN

      END SUBROUTINE EMISS_SCALING


C-----------------------------------------------------------------------
      SUBROUTINE EMISS_CONV_DIFF( VDEMIS0, NL, VDEMIS )

C     Convert rate and map to Diffusivity module species order
C-----------------------------------------------------------------------
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      USE AERO_EMIS           ! inherits GRID_CONF

      IMPLICIT NONE

      ! Local Variables
      REAL,ALLOCATABLE, SAVE :: CNVTC( :,:,: ) ! combined conversion factor
      REAL,ALLOCATABLE, SAVE :: CNVTI( : )     ! intermediate combined conv. factor
      REAL,       SAVE :: CNVTP                ! intermediate combined conv. factor
      REAL,ALLOCATABLE, SAVE :: CONVM( :,:,: ) ! Aerosol Mass and Surface Area conversion factor         
      REAL,ALLOCATABLE, SAVE :: CONVN( :,:,: ) ! Aerosol Number conversion factor       
      REAL, PARAMETER  :: GPKG = 1.0E+03       ! g/kg
      REAL, PARAMETER  :: MWAIR = 28.9628      ! g/mol
      REAL, PARAMETER  :: AVO  = 6.0221367E23
      REAL, PARAMETER  :: RAVO = 1.0 / AVO
      
      INTEGER, INTENT( IN ) :: NL
      REAL, INTENT( INOUT ) :: VDEMIS0( :,:,:,: )
      REAL, INTENT( OUT ):: VDEMIS ( :,:,:,: )
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER       :: EMtoDIFF(N_EMIS_ISTR)
      INTEGER       :: C, R, L, ISTR, INDX

      ! Get domain decomp info from the emissions file
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ! Populate Persistent Variables
         CNVTP = 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 

         ALLOCATE( CNVTI( EMLAYS ), CNVTC( EMLAYS,NCOLS,NROWS ), 
     &             CONVM( EMLAYS,NCOLS,NROWS ),
     &             CONVN( EMLAYS,NCOLS,NROWS )  )
      END IF    !FirstTime
      
      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
      CNVTI( 1:NL ) = CNVTP * Grid_Data%RDX3F( 1:NL )
      DO L = 1,NL
        CNVTC( L,:,: ) = 1.0E-3 * CNVTI( L ) * Met_Data%RRHOJ( :,:,L )  ! Gas Moles:   mol/s -> ppmv/s
        CONVM( L,:,: ) = MWAIR / GPKG / Met_Data%DENS( :,:,L ) !m3/mol  ! Aer. Mass:   umol/m3/s -> ppmv/s
      END DO                                                            ! m2/m3/s -> m2/mol/s
      CONVN( 1:NL,:,: ) = CONVM( 1:NL,:,: )                             ! Aer. Num:    N/m3/s -> N/mol/s

      !MAKE SURE TO APPLY RELEVANT UNIT CONVERSION TO EACH
      !TYPE OF VARIABLE (GAS, AEROSOL MASS, AEROSOL NUMBER,
      !AND AEROSOL SURFACE AREA)
      FORALL ( ISTR = 1:N_EMIS_ISTR, MAP_EMtoGAS( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )
  
      FORALL ( ISTR = 1:N_EMIS_ISTR, MAP_EMtoAERO( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )
      
      FORALL ( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSRF( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )
      
      FORALL ( ISTR = 1:N_EMIS_ISTR, MAP_EMtoNUM( ISTR ) .NE. 0 )
     &    VDEMIS0( ISTR,1:NL,:,: ) = CONVN( 1:NL,:,: ) * VDEMIS0( ISTR,1:NL,:,: )

 
      ! zero out emissions values for diffused species not included in emissions list.
      ! ...accounts for emissions species names as a subset of the vert. diffused species list
      VDEMIS( :,1:NL,:,: ) = 0.0
      FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoDIFF( ISTR ) .NE. 0 ) 
     &        VDEMIS( MAP_EMtoDIFF( ISTR ),1:NL,:,: ) = 
     &                VDEMIS( MAP_EMtoDIFF( ISTR ),1:NL,:,: ) + 
     &                VDEMIS0( ISTR,1:NL,:,: )

      RETURN
      END SUBROUTINE EMISS_CONV_DIFF


!-----------------------------------------------------------------------
      FUNCTION EMISS_NEG_CHECK( VDEMIS, ISRM, NL ) RESULT( STAT )

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

          WRITE( LOGDEV, '(/,5x,A,ES10.3,A,/,5x,3A,/,5x,A)' ),
     &      'ERROR: Invalid Negative emission rate ', EMIS_MIN, ' has been ',
     &      ' for CMAQ species ',TRIM(DIFF_SPC( ISPC )),'.',
     &      'Please inspect the Emission Control Namelist File.'
          XMSG = 'Negative Emissions Detected'
          CALL M3EXIT( 'EMISS_NEG_CHECK', 0, 0, XMSG, XSTAT1 )

      ELSE ! reset slightly negative values to 0.0
          WHERE ( VDEMIS .LT. 0.0 ) VDEMIS = 0.0
      END IF

      END FUNCTION EMISS_NEG_CHECK    

!-----------------------------------------------------------------------
      SUBROUTINE WRITE_EMISS_DIAG( MDATE, MTIME, VDEMIS0, ISRM, NL ) 

!     Write out emissions diagnostic file
!-----------------------------------------------------------------------
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      USE AERO_EMIS           ! inherits GRID_CONF
      USE GRID_CONF
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_MASK_GAS, DIFF_MASK_AERO,
     &                      DIFF_MASK_NUM, DIFF_MASK_SRF, DIFF_MW, DIFF_SPC,
     &                      DIFF_MASK_NR, DIFF_MASK_TRAC
      USE AERO_DATA, ONLY : GPKG

      IMPLICIT NONE
         
      INCLUDE SUBST_CONST     ! constants

      INTEGER, INTENT( IN ) :: MDATE, MTIME, ISRM
      REAL, INTENT( In )    :: VDEMIS0( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: VDEMIS ( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: VDEMIS_OUT ( :,:,: )

      ! Local Variables
      REAL, PARAMETER :: RAVO = 1.0 / AVO
      REAL,ALLOCATABLE, SAVE :: CNVTC( :,:,: ) ! combined conversion factor
      REAL,ALLOCATABLE, SAVE :: CNVTI( : )     ! intermediate combined conv. factor
      REAL,       SAVE :: CNVTP                ! intermediate combined conv. factor
      REAL,ALLOCATABLE, SAVE :: CONVM( :,:,: ) ! Aerosol Mass and Surface Area conversion factor         
      REAL,ALLOCATABLE, SAVE :: CONVN( :,:,: ) ! Aerosol Number conversion factor       
 
      INTEGER       :: LAYS, L, C, R, IVAR, NL, I
      CHARACTER( 6 ):: EM_LDIAG
      CHARACTER( 16):: EM_FDIAG

      LOGICAL :: FIRST_TIME = .TRUE.
      CHARACTER( 586 ) :: XMSG

      ! Get pressure info from ASX_DATA_MOD
      IF ( FIRST_TIME ) THEN
         FIRST_TIME = .FALSE.
         CNVTP = 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 

         ALLOCATE( CNVTI( EMLAYS ), CNVTC( EMLAYS,NCOLS,NROWS ), 
     &             CONVM( EMLAYS,NCOLS,NROWS ),
     &             CONVN( EMLAYS,NCOLS,NROWS ),
     &             VDEMIS( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ),
     &             VDEMIS_OUT( NCOLS,NROWS,EMLAYS ) )
      END IF    !FirstTime
      
      VDEMIS     = 0.0
      VDEMIS_OUT = 0.0
      LAYS       = 1

      EM_LDIAG = EM_FILE_LDIAG( ISRM )
      EM_FDIAG = EM_DIAG_FILE( ISRM )

      IF ( EM_LDIAG .EQ. '3D' .OR. 
     &     EM_LDIAG .EQ. '2DSUM' ) LAYS = NL

      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
      CNVTI( 1:NL ) = CNVTP * Grid_Data%RDX3F( 1:NL )
      DO L = 1,NL
        CNVTC( L,:,: ) = 1.0E3 / ( CNVTI( L ) * Met_Data%RRHOJ( :,:,L ) ) ! Gas Moles: ppmv/s -> mol/s
        CONVM( L,:,: ) = (1.0 / MWAIR) * GPKG * Met_Data%DENS( :,:,L )    ! Aer. Mass: ppmv/s -> umol/s
     &                   / Met_Data%RJACM( :,:,L ) * CELLVOL( L )         ! m2/mol/s -> m2/s
      END DO                                                                
      CONVN( 1:NL,:,: ) = CONVM( 1:NL,:,: )                               ! Aer. Num:  N/mol/s -> N/s

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

 
      ! Output to Emissions Diagnostic File
      DO IVAR = 1,N_SPC_DIFF
            ! Skip Writing this species if this source did not
            ! contribute to it
            IF ( ISRM .LT. (N_EM_SRM+1) ) THEN
              IF ( .NOT. EM_FILE_DIFF( IVAR,ISRM ) ) CYCLE
            ELSE
              IF ( .NOT. ANY(EM_FILE_DIFF( IVAR,: )) ) CYCLE
            END IF

            ! Sum the column if requested
            IF ( EM_LDIAG .EQ. '2DSUM' ) THEN
                 VDEMIS( IVAR,1,:,: ) = SUM( VDEMIS( IVAR,1:NL,:,: ),1 )
                 LAYS = 1
            END IF 
   
            DO L = 1,LAYS
                VDEMIS_OUT( :,:,L ) = VDEMIS( IVAR,L,:,: )
            END DO

            ! Write out the emissions data
            IF ( .NOT. WRITE3( EM_FDIAG, DIFF_SPC( IVAR ),
     &                 MDATE, MTIME, VDEMIS_OUT( :,:,1:LAYS ) ) ) THEN
                XMSG = 'Could not write ' // TRIM( EM_FDIAG ) // ' file'
                CALL M3EXIT( 'WRITE_EMISS_DIAG', MDATE, MTIME, XMSG, XSTAT1 )
            END IF
      END DO

      END SUBROUTINE WRITE_EMISS_DIAG
 
C-----------------------------------------------------------------------
      SUBROUTINE EMIS_SPC_MAP( JDATE, JTIME )

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
     &                         N_AEROSPC, AEROMODE, MODESUFF, EM_AERO_REF
         USE AERO_EMIS, only : MAP_NUMtoEM, MAP_SRFtoEM, MAP_EMtoAERO, MAP_EMtoMODE, 
     &                         MAP_EMtoNUM, MAP_EMtoSRF, MAP_EMtoSD,
     &                         EM_STREAM_SIZE, SD_SPLIT, EM_SD_INIT
         USE UDTYPES,   only : CARRY1, LARRY1

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME
         INTEGER    :: N_UNUSED, REGNUM, N_USED

         INTEGER    :: STRT, PTSTRT, ISPC, IDX, IX, ISUR, IRULE,
     &                 N, IM, IAERO, V, ISRM, NSPC, IA, IDIFF, JDX, JM,
     &                 ISTR, ISUR0, ISRM0, IEM, KDX, JSD, JEM, ISD,
     &                 IFAC, IRGN, ISTRN, NFAC, NCHEM, ICHEM, IFAM, F,
     &                 IRGN2, N_SURR_CATCH, ICATCH
         REAL       :: AERO_SPLIT, UNIT_FAC_1, UNIT_FAC_2, SURR_MW, 
     &                 SPEC_MW, BASIS_FAC

         LOGICAL    :: LERROR, LFOUND, L_WDIFF, L_WISD, LTEST
         LOGICAL    :: LGAS_DIFF, LGAS_SURR, L_CATCH
         
         CHARACTER( 16 )  :: SPECNAME, SN, SM, EM_SURR_CATCH( 200 )

         CHARACTER( 16 )  :: PNAME = 'EMIS_SPC_CHECK'
         CHARACTER( 100)  :: VARDESC
         INTEGER          :: STATUS

         CHARACTER( 500 ) :: XMSG
         CHARACTER( 16 )  :: B
         CHARACTER( 20 )  :: REFNAME
         
         INTEGER, PARAMETER :: NISTR0 = 3000
         LOGICAL, ALLOCATABLE, SAVE :: EM_STREAM_RULE( : ), EM_SPEC_RULE( : )
         TYPE( LARRY1 ), ALLOCATABLE, SAVE :: EM_SURR_RULE( : ), EM_PHASE_RULE( : )
         INTEGER, PARAMETER :: N_SCALEFAC = 100
         LOGICAL            :: LSPEC_KEY( NISTR0 ), LSURR_KEY( NISTR0 )
         CHARACTER( 16 )    :: CHEM_NAME( 150 )
         LOGICAL            :: LERROR2, LERROR3( 150 )
        
         INTEGER            :: IC, N_COMMANDS
         INTEGER, PARAMETER :: NCOMM0 = 4000000
         INTEGER            :: RULE_ISTR_IDIFF( NCOMM0 )
         INTEGER            :: RULE_ISTR_ISRM ( NCOMM0 )
         INTEGER            :: RULE_ISTR_ISUR ( NCOMM0 )
         CHARACTER( 16 )    :: RULE_ISTR_SPEC ( NCOMM0 )
         CHARACTER( 16 )    :: RULE_ISTR_SURR ( NCOMM0 )
         INTEGER            :: RULE_ISTR_PHASE( NCOMM0 )

         REAL, ALLOCATABLE :: EM_FAC( :,:,: )
         REAL, ALLOCATABLE :: EM_FAC_BULK( :,:,: )
         CHARACTER( 1 ), ALLOCATABLE :: EM_OP( :,:,: )
         CHARACTER( 4 ), ALLOCATABLE :: EM_BASIS( :,:,: )
         REAL,  ALLOCATABLE          :: EM_CONV( :,:,: )
         INTEGER, ALLOCATABLE :: EM_REG( :,:,: )
         INTEGER              :: N_EM_RULE
         LOGICAL              :: LREMOVE

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
         DO IRGN = 1,N_EM_RGN
            WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),IRGN, EM_REGIONS(IRGN)%LABEL( 1:18 ),
     &             EM_REGIONS( IRGN )%File( 1:18 ), TRIM(EM_REGIONS( IRGN )%VAR ) 
            IF ( EM_REGIONS(IRGN)%FILE(1:6) .EQ. 'Family' ) THEN
                 F = INDEX1( EM_REGIONS(IRGN)%LABEL(1:18), NREGIONFAMILIES, REGIONFAMILYNAME )
                 DO IRGN2 = 1,REGIONFAMILYNUM( F )
                     WRITE( LOGDEV,'(56x,A)' ), REGIONFAMILYMEMBERS( F,IRGN2 )
                 END DO
            END IF
         END DO

! Retrieve the Surrogates Available From Emissions Streams
         ! Load Default Molecular Weights and Units based on 
         ! SMOKE/MOVES/SPECIATE specifications

         WRITE( LOGDEV, '(/,5x,A)' ),'|> Map Available Emissions Surrogates to Defaults:'
         WRITE( LOGDEV, '(5x,A)'   ),'=================================================='
         N_SURR_CATCH = 0
         DO ISRM = 1,N_EM_SRM

            IF ( ISRM .EQ. ISEASRM .OR. ISRM .EQ. IBIOSRM .OR. 
     &           ISRM .EQ. IDUSTSRM.OR. ISRM .EQ. ILTSRM  .OR.
     &           ISRM .EQ. IMGSRM ) CYCLE

            ALLOCATE( EM_FILE_SURR( ISRM )%MW   ( EM_FILE_SURR( ISRM )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRM )%USED ( EM_FILE_SURR( ISRM )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRM )%CONV ( EM_FILE_SURR( ISRM )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRM )%BASIS( EM_FILE_SURR( ISRM )%LEN ) )
            EM_FILE_SURR( ISRM )%USED = .FALSE.

            DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
               ! Assign Default Molecular Weight to Each Surrogate Species
               B = EM_FILE_SURR( ISRM )%ARRY( ISUR )
               IA  = INDEX1( B, N_EMIS_SURR_TABLE, EMIS_SURR_TABLE( : )%NAME )
               IF ( IA .GT. 0 ) THEN
                  EM_FILE_SURR( ISRM )%MW   ( ISUR ) = EMIS_SURR_TABLE( IA )%MW
               ELSE
                  ! Surrogate is not calculated online nor does it
                  ! belong to the default list of commonly used
                  ! surrogates.
                  EM_FILE_SURR( ISRM )%MW   ( ISUR ) = 1.0

                  ! Only Write a Note to the User if this species hasn't
                  ! been caught from a different stream already
                  IF ( N_SURR_CATCH .EQ. 0 .OR.
     &                 INDEX1( B, 200, EM_SURR_CATCH ) .EQ. 0 ) THEN
                     N_SURR_CATCH = N_SURR_CATCH + 1
                     EM_SURR_CATCH( N_SURR_CATCH ) = B
                  END IF
               END IF
            END DO

         END DO

         WRITE( LOGDEV, '(/,5x,A)' ),'|> Checking Emissions Surrogate Units:'
         WRITE( LOGDEV, '(5x,A)'   ),'======================================'
         DO ISRM = 1,N_EM_SRM
            DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
               ! Check Units and Assign Conversion Factors for
               ! translating [kmol or umol] -> mol, [kg or mg] -> g, and
               ! [s, min, hr] -> s
               CALL CHECK_EMIS_UNITS( ISRM, ISUR, 
     &                                EM_FILE_SURR( ISRM )%ARRY ( ISUR ),
     &                                EM_FILE_SURR( ISRM )%UNITS( ISUR ), 
     &                                EM_FILE_SURR( ISRM )%CONV ( ISUR ),
     &                                EM_FILE_SURR( ISRM )%BASIS( ISUR ) )
            END DO
         END DO

! Write Out All Available Stream Families to the Log File
         WRITE( LOGDEV, '(/,5x,A)' ),'|> Emission Stream Family Definitions:'
         WRITE( LOGDEV, '(5x,A)'   ),'======================================'
         WRITE( LOGDEV,'(8x,A19,15x,A)' ),'Stream Family Label','Stream Family Members'
         WRITE( LOGDEV,'(8x,A19,15x,A)' ),'-------------------','---------------------'
         DO IFAM = 1,NSTREAMFAMILIES
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
         DO IFAM = 1,NCHEMFAMILIES
            WRITE( LOGDEV,'(8x,A32,2x,A)' ), CHEMFAMILYNAME( IFAM ), 
     &             CHEMFAMILYMEMBERS( IFAM,1 )
            IF ( CHEMFAMILYNUM( IFAM ) .GT. 1 ) THEN
                DO ISRM = 2,CHEMFAMILYNUM( IFAM )
                   WRITE( LOGDEV,'(42x,A)' ), CHEMFAMILYMEMBERS( IFAM,ISRM )
                END DO
            END IF
         END DO

! Set up Stream <-> Size Distribution relationship. This routine
! populates the EM_STREAM_SIZE structure which tells the logic below which
! modes are present on which streams.
         WRITE( LOGDEV, '(/,5x,A)' ),'|> Mapping Particle Size Distributions to Each Emission Stream:'
         WRITE( LOGDEV, '(5x,A)'   ),'==============================================================='
         CALL EM_SD_INIT( JDATE, JTIME )

! Process Default Emissions Mapping (if requested in namelist; i.e. 
         ALLOCATE( EM_SPEC( NISTR0 ) )                ! CMAQ Species Names
         EM_SPEC = ""
         ALLOCATE( EM_SURR( NISTR0,N_EM_SRM ) )       ! Surrogate Name
         EM_SURR = ""
         ALLOCATE( EM_FAC ( NISTR0,N_EM_SRM,N_SCALEFAC ) ) ! Scale Factor
         EM_FAC = 0.0
         ALLOCATE( EM_FAC_BULK( NISTR0,N_EM_SRM,N_SCALEFAC ) ) ! Bulk Scale Factor For Printing to Diagnostic
         EM_FAC_BULK = 0.0                                        !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_OP( NISTR0,N_EM_SRM,N_SCALEFAC ) )  ! Bulk Scale Factor For Printing to Diagnostic
         EM_OP = ""                                          !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_REG( NISTR0,N_EM_SRM,N_SCALEFAC ) ) ! Bulk Scale Factor For Printing to Diagnostic
         EM_REG = 1                                          !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_BASIS ( NISTR0,N_EM_SRM,N_SCALEFAC ) )   ! Mass or Mole Basis for Conversion
         EM_BASIS = ""
         ALLOCATE( EM_CONV ( NISTR0,N_EM_SRM,N_SCALEFAC ) )   ! Mass or Mole Basis for Conversion
         EM_CONV = 1.0
         ALLOCATE( MAP_EMtoDIFF( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoDIFF = 0                             !   to Diffusion Vector
         ALLOCATE( MAP_EMtoSURR( NISTR0,N_EM_SRM ) )  ! Map from Emissions Species 
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
         ALLOCATE( MAP_EMtoSD  ( NISTR0,N_EM_SRM ) )  ! Map from Emissions Species 
         MAP_EMtoSD  = 0                              !   to emissions aerosol mode
         ALLOCATE( EM_FILE_DIFF( N_SPC_DIFF,N_EM_SRM ) )
         EM_FILE_DIFF = .FALSE.

         ! Find all matches between the transported species list and the
         ! available surrogates from each stream. Apply a scale factor
         ! of 1 to these matches. For aerosols, the CMAQ species name
         ! may or may not include the mode suffix (eg. i, j, or k).
         ! Equivalence tests should be performed without a suffix on the
         ! surrogate name and with each suffix added in turn.

         N_EMIS_ISTR = 0
         SPECNAME = ''

! Insert Emissions Instructions for Aerosol Number and Surface Area,
! if at least one aerosol species is being transported
         IF ( COUNT( DIFF_MASK_AERO ) .GT. 0 ) THEN
            DO IM = 1,N_MODE
               ! Aerosol Number
               EM_SPEC( N_EMIS_ISTR+1 ) = AEROMODE( IM )%NUM_NAME
               EM_FAC ( N_EMIS_ISTR+1,:,1 ) = 1.0
               EM_FAC_BULK ( N_EMIS_ISTR+1,:,1 ) = 1.0         
               EM_BASIS( N_EMIS_ISTR+1,:,1 ) = 'UNIT'
               EM_CONV( N_EMIS_ISTR+1,:,1 ) = 1.0
               EM_OP ( N_EMIS_ISTR+1,:,1 ) = "a"
               EM_REG ( N_EMIS_ISTR+1,:,1 ) = 1
               MAP_NUMtoEM( IM ) = N_EMIS_ISTR+1
               MAP_EMtoNUM( N_EMIS_ISTR+1 ) = IM
               MAP_EMtoDIFF( N_EMIS_ISTR+1 ) = 
     &            INDEX1( EM_SPEC( N_EMIS_ISTR+1 ), N_SPC_DIFF, DIFF_SPC )
               EM_FILE_DIFF( MAP_EMtoDIFF( N_EMIS_ISTR+1 ),: ) = .TRUE.
               MAP_EMtoSD( N_EMIS_ISTR+1, : ) = 0

               ! Aerosol Surface Area
               EM_SPEC( N_EMIS_ISTR+2 ) = AEROMODE( IM )%SRF_NAME
               EM_FAC ( N_EMIS_ISTR+2,:,1 ) = 1.0
               EM_FAC_BULK ( N_EMIS_ISTR+2,:,1 ) = 1.0         
               EM_BASIS( N_EMIS_ISTR+2,:,1 ) = 'UNIT'
               EM_CONV( N_EMIS_ISTR+2,:,1 ) = 1.0
               EM_OP ( N_EMIS_ISTR+2,:,1 ) = "a"
               EM_REG ( N_EMIS_ISTR+2,:,1 ) = 1
               MAP_SRFtoEM( IM ) = N_EMIS_ISTR+2
               MAP_EMtoSRF( N_EMIS_ISTR+2 ) = IM
               MAP_EMtoDIFF( N_EMIS_ISTR+2 ) = 
     &            INDEX1( EM_SPEC( N_EMIS_ISTR+2 ), N_SPC_DIFF, DIFF_SPC )
               EM_FILE_DIFF( MAP_EMtoDIFF( N_EMIS_ISTR+2 ),: ) = .TRUE.
               MAP_EMtoSD( N_EMIS_ISTR+2, : ) = 0

               N_EMIS_ISTR = N_EMIS_ISTR + 2
            END DO
         END IF

! Process User-Defined Emissions Scaling Rules. 
        ! Find Total Number of Rules
         N_EM_RULE = 0
         DO IRULE = 1,N_EM_RULE_REG
            IF( EM_NML( IRULE )%SPEC .EQ. '' ) EXIT
            N_EM_RULE = IRULE
         END DO

         ! Implement Default Scaling Rules that will Always Be Applied
         CALL CUSTOM_EM_RULES( N_EM_RULE )


         ! Allocate Rule->Instruction Transform Masks
         ALLOCATE( EM_STREAM_RULE  ( N_EM_SRM   ) )
         ALLOCATE( EM_SPEC_RULE ( N_SPC_DIFF ) )
         ALLOCATE( EM_SURR_RULE ( N_EM_SRM ) )
         ALLOCATE( EM_PHASE_RULE( N_EM_SRM ) )
         DO ISRM = 1,N_EM_SRM
           N = EM_FILE_SURR( ISRM )%LEN
           EM_SURR_RULE( ISRM )%LEN = N
           ALLOCATE( EM_SURR_RULE( ISRM )%ARRY( N ) )

           N = EM_STREAM_SIZE( ISRM )%LEN
           EM_PHASE_RULE( ISRM )%LEN = N
           ALLOCATE( EM_PHASE_RULE( ISRM )%ARRY( N ) )
         END DO
 
         ! Loop Through Emission Rules, Test for Fidelity, expand if necessary
         ! and Apply them to the instruction set that currently exists.
         DO IRULE = 1,N_EM_RULE 
            ! Exit this loop if the rule is blank
            IF ( EM_NML( IRULE )%SPEC .EQ. '' ) EXIT 

            ! Expand Rule To Individual Instructions. If the CMAQ
            ! Species, Stream Label, and Surrogate are all single
            ! components, then there will just be one instruction. If
            ! any of them equal 'All' the number of instructions will
            ! grow correspondingly.

            !------   ------   ------   ------   ------   ------   -----
            ! First error check and expand the stream field
            ! This subroutine returns a logical vector, EM_STREAM_RULE,
            ! which identifies which streams are affected by this rule.
            LREMOVE = .FALSE.
            CALL INTERPRET_EM_RULE_STREAM( EM_NML( IRULE )%STREAM, IRULE, 
     &              EM_STREAM_RULE, LREMOVE )
            IF ( LREMOVE ) CYCLE

            !------   ------   ------   ------   ------   ----  
            ! Now error check and expand the surrogate field
            CALL UPCASE( EM_NML( IRULE )%SURR )
 
            !Initialize Surrogate Array for every Stream
            DO ISRM = 1,N_EM_SRM
              EM_SURR_RULE( ISRM )%ARRY = .FALSE.
            END DO
            LSURR_KEY = .FALSE.

            LERROR = .TRUE.
            IF ( EM_NML( IRULE )%SURR .EQ. 'ALL' ) THEN
               ! Expand the Rule to Apply to All Surrogates
               DO ISRM = 1,N_EM_SRM
                 IF ( .NOT. EM_STREAM_RULE( ISRM ) ) CYCLE
                 EM_SURR_RULE( ISRM )%ARRY = .TRUE.
                 LSURR_KEY( IRULE ) = .TRUE.
                 LERROR = .FALSE.
               END DO
            ELSE
               ! Determine if the Surrogate Label Refers to A Family and if So, 
               ! Apply the Rule to all members of that Family
               IFAM = INDEX1( EM_NML( IRULE )%SURR, NChemFamilies, ChemFamilyName )
               IF ( IFAM .EQ. 0 ) THEN
                  ! This Surrogate Label Does not match any families.
                  ! Just look for one surrogate
                  DO ISRM = 1,N_EM_SRM
                     IF ( .NOT.EM_STREAM_RULE( ISRM ) ) CYCLE
                     IDX = INDEX1( EM_NML( IRULE )%SURR, EM_FILE_SURR( ISRM )%LEN, 
     &                             EM_FILE_SURR( ISRM )%ARRY )
                     IF ( IDX .NE. 0 ) THEN
                        EM_SURR_RULE( ISRM )%ARRY( IDX ) = .TRUE.
                        LERROR = .FALSE.
                     END IF
                 END DO
               ELSE
                  ! Surrogate Label is for a family. Loop through all
                  ! the members to assign surrogates
                  NCHEM = ChemFamilyNum( IFAM )
                  CHEM_NAME(1:NCHEM) = ChemFamilyMembers( IFAM,1:NCHEM )
                  LSURR_KEY( IRULE ) = .TRUE.
 
                  LERROR = .FALSE.
                  DO ICHEM = 1,NCHEM
                     LERROR3( ICHEM ) = .FALSE.
                     LERROR2 = .TRUE.
                     DO ISRM = 1,N_EM_SRM
                        IF ( .NOT. EM_STREAM_RULE( ISRM ) ) CYCLE
                        ! Find the Specific Species this Rule Identifies
                        IDX = INDEX1( CHEM_NAME( ICHEM ), EM_FILE_SURR( ISRM )%LEN, 
     &                                EM_FILE_SURR( ISRM )%ARRY )
                        IF ( IDX .NE. 0 ) THEN
                           EM_SURR_RULE( ISRM )%ARRY( IDX ) = .TRUE.
                           LERROR2 = .FALSE.
                        END IF
                     END DO
                     IF ( LERROR2 ) THEN
                         LERROR3( ICHEM ) = .TRUE.
                         LERROR = .TRUE.
                     END IF
                  END DO          
               END IF
 
            END IF
            
            IF ( LERROR ) THEN
              IF ( .NOT. EMISCHK ) THEN
                 WRITE( LOGDEV, '(/,5x,A,/,5x,2A,4(/,5x,A))') 
     &                  '*** ATTENTION **********************************************:',
     &                  'The emission surrogate or member of family ',TRIM( EM_NML( IRULE )%SURR ),
     &                  'was not found in the requested emission streams but the ',
     &                  'CTM_EMISCHK environment variable set to False so simulation ',
     &                  'will proceed.',
     &                  '*************************************************************'
                 IF ( IFAM .NE. 0 ) THEN
                    WRITE( LOGDEV, '(7x,A,/,7x,A,/,7x,A,1x,A )') 
     &                  'The surrogate label applied was a chemical family. The specific',
     &                  'family member(s) that did not appear on any emission stream',
     &                  'inputs was:'
                    DO ICHEM = 1,NCHEM
                        IF ( LERROR3( ICHEM ) ) WRITE( LOGDEV, '(20x,A)' ) TRIM( CHEM_NAME( ICHEM ) )
                    END DO
                    WRITE( LOGDEV, '(7x,A)' )
     &                  '*************************************************************'
                 END IF
                 CYCLE
              ELSE     
                 WRITE( LOGDEV, '(5x,A,/,5x,2A,/,8(/,5x,A))') 
     &                  '*** ERROR **************************************************************:',
     &                  'The emission surrogate or member of family ',TRIM( EM_NML( IRULE )%SURR ),
     &                  ' is not found in any of the emission streams.',
     &                  'Use one of the below options to continue.', 
     &                  '1) Change or remove this emission rule so that it refers to an existing surrogate.', 
     &                  'or',
     &                  '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &                  'if model predictions are acceptable without using the above emissions.',
     &                  '*************************************************************************'
                 IF ( IFAM .NE. 0 ) THEN
                    WRITE( LOGDEV, '(7x,A,/,7x,A,/,7x,A,1x,A )') 
     &                  'The surrogate label applied was a chemical family. The specific',
     &                  'family member(s) that did not appear on any emission stream',
     &                  'inputs was:'
                    DO ICHEM = 1,NCHEM
                        IF ( LERROR3( ICHEM ) ) WRITE( LOGDEV, '(20x,A)' ) TRIM( CHEM_NAME( ICHEM ) )
                    END DO
                    WRITE( LOGDEV, '(7x,A)' )
     &                  '*************************************************************'
                 END IF
                 XMSG = 'Species with the missing surrogates ' 
     &                //'must have a surrogate found in at least one stream.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF
            END IF
 
            !------   ------   ------   ------   ------   ------
            ! Now Error Check and Expand the CMAQ Species Field
            CALL UPCASE( EM_NML( IRULE )%SPEC )

            ! Initialize CMAQ Species Array
            EM_SPEC_RULE = .FALSE.
            LSPEC_KEY = .FALSE.

            IF ( EM_NML( IRULE )%SPEC .EQ. 'ALL' ) THEN
               ! Expand the Rule to Apply to All Species
               EM_SPEC_RULE = .TRUE.
               LSPEC_KEY( IRULE ) = .TRUE.
            ELSE     
               ! Determine if the Species Label Refers to A Family and if So, 
               ! Apply the Rule to all members of that Family
               IFAM = INDEX1( EM_NML( IRULE )%SPEC, NChemFamilies, ChemFamilyName )
               IF ( IFAM .EQ. 0 ) THEN
                  NCHEM = 1
                  CHEM_NAME(1) = EM_NML( IRULE )%SPEC
               ELSE
                  NCHEM = ChemFamilyNum( IFAM )
                  CHEM_NAME(1:NCHEM) = ChemFamilyMembers( IFAM,1:NCHEM )
                  LSPEC_KEY( IRULE ) = .TRUE.
               END IF

               DO ICHEM = 1,NCHEM
                 ! Find the Specific Species this Rule Identifies
                 IDX = INDEX1( CHEM_NAME( ICHEM ), N_SPC_DIFF, DIFF_SPC )
                 JDX = INDEX1( CHEM_NAME( ICHEM ), N_AEROSPC,  AEROSPC( : )%BULKNAME )
                 IF ( IDX .NE. 0 ) THEN
                   EM_SPEC_RULE( IDX ) = .TRUE.
                 ELSE IF ( JDX .NE. 0 ) THEN
                   ! This is an aerosol species, and it is being
                   ! identified with a bulk name (no mode suffix). 
                   ! We need to allow for all possible DIFF_SPC with
                   ! all used suffixes
                   SN = CHEM_NAME( ICHEM )
                   DO IM = 1,N_MODE
                     KDX = INDEX1( TRIM( SN )//MODESUFF( IM ), N_SPC_DIFF, DIFF_SPC )
                     IF ( KDX .NE. 0 ) EM_SPEC_RULE( KDX ) = .TRUE.
                   END DO
                 ELSE
                   WRITE( LOGDEV, '(/,3A,/,A,/,A,/,A,/,A)' ),
     &               'Species ',TRIM(EM_NML( IRULE )%SPEC),' was used in the Emissions',
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
            CALL UPCASE( EM_NML( IRULE )%PHASE )

            !Initialize Surrogate Array for every Stream
            DO ISRM = 1,N_EM_SRM
              EM_PHASE_RULE( ISRM )%ARRY = .FALSE.
            END DO

            LERROR = .TRUE.
            DO ISRM = 1,N_EM_SRM 
              ! Skip this stream if it is not identified
              IF ( .NOT. EM_STREAM_RULE( ISRM ) ) CYCLE 

              IF ( EM_NML( IRULE )%PHASE .EQ. 'ALL' ) THEN
                 ! Expand the Rule to Apply to All Phases and Modes
                 EM_PHASE_RULE( ISRM )%ARRY = .TRUE.
                 LERROR = .FALSE.
              ELSE IF ( EM_NML( IRULE )%PHASE .EQ. 'AERO' ) THEN
                 ! Expand the Rule to Apply to All Aerosol Modes
                 EM_PHASE_RULE( ISRM )%ARRY(2:) = .TRUE.
                 LERROR = .FALSE.
              ELSE
                 ! Find the Specific Phase/Mode this Rule Identifies
                 IDX = INDEX1( EM_NML( IRULE )%PHASE, EM_STREAM_SIZE( ISRM )%LEN, 
     &                         EM_STREAM_SIZE( ISRM )%NAME )
                 IF ( IDX .NE. 0 ) THEN
                   EM_PHASE_RULE( ISRM )%ARRY( IDX ) = .TRUE.
                   LERROR = .FALSE.
                 END IF
              END IF
            END DO
            
            IF ( LERROR ) THEN
                 WRITE( LOGDEV, '(A,/,2A,/,A,/,A,I3,A1,/,A)') 
     &                  '*****************************ERROR***************************************:',
     &                  'The phase or mode ',TRIM( EM_NML( IRULE )%PHASE ),
     &                  ' is not found in any of the emission streams you''re requesting for ',
     &                  'emission rule ',IRULE,'.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
                 XMSG = 'Species with the missing surrogates ' 
     &                //'must have a surrogate found in at least one stream.'
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
               WRITE( LOGDEV, * )TRIM( XMSG )
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
              DO ISRM = 1,N_EM_SRM
              IF ( EM_STREAM_RULE( ISRM ) ) THEN
                DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN 
                IF ( EM_SURR_RULE( ISRM )%ARRY( ISUR ) ) THEN
                  DO ISD = 1,EM_STREAM_SIZE( ISRM )%LEN
                  IF ( EM_PHASE_RULE( ISRM )%ARRY( ISD ) ) THEN

                    LTEST = .TRUE.
                    
                    ! If this command applies to a Gas CMAQ species, make
                    ! sure the aerosol phases are not invoked so that 
                    ! double-counting is avoided
                    IF ( .NOT. DIFF_MASK_AERO( IDIFF ) .AND. 
     &                   EM_STREAM_SIZE( ISRM )%NAME( ISD ) .NE. 'GAS' )
     &                 LTEST = .FALSE.

                    ! If this command applies to an aerosol CMAQ species,
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
                             IF ( EM_STREAM_SIZE( ISRM )%FACNUM( ISD,IM ) 
     &                              .LE. 1.0e-10 ) LTEST = .FALSE.
                           END IF
                        END DO
                    END IF

                    ! If any family name was used for the Surrogate 
                    ! and Species fields and those family or keyword
                    ! names match, then only exact matches or existing 
                    ! relationships should be populated.
                    IF (   (LSURR_KEY( IRULE ) .AND. 
     &                      LSPEC_KEY( IRULE ) )
     &                  .AND.
     &                     (EM_FILE_SURR( ISRM )%ARRY( ISUR ) .NE. 
     &                      DIFF_SPC( IDIFF ) ) ) THEN
                      ! Look for this relationship in at least one
                      ! of the existing instructions
                      LTEST = .FALSE.
                      DO ISTRN = 1,N_EMIS_ISTR
                          IF ( EM_SPEC( ISTRN ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &                         EM_SURR( ISTRN,ISRM ) .EQ. 
     &                           EM_FILE_SURR( ISRM )%ARRY( ISUR ) ) THEN
                            LTEST = .TRUE.
                          END IF
                      END DO
                    END IF

                    ! Add an Instruction for this combination of CMAQ
                    ! Species, Stream, Surrogate, and Phase if the test
                    ! for validity (LTEST) is still TRUE
                    IF ( LTEST ) THEN
                      N_COMMANDS = N_COMMANDS + 1
                      RULE_ISTR_IDIFF( N_COMMANDS ) = IDIFF
                      RULE_ISTR_ISRM ( N_COMMANDS ) = ISRM
                      RULE_ISTR_ISUR ( N_COMMANDS ) = ISUR
                      RULE_ISTR_SPEC ( N_COMMANDS ) = DIFF_SPC( IDIFF )
                      RULE_ISTR_SURR ( N_COMMANDS ) = 
     &                     EM_FILE_SURR( ISRM )%ARRY( ISUR )
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
                  ! if the CMAQ species matches exactly, there is no 
                  ! surrogate present for this stream, and the same
                  ! Emissions Surrogate is being used for another 
                  ! stream.
                  LFOUND = .FALSE.
                  ! Look For a suitable previous instruction to add to.
                  IF ( N_EMIS_ISTR .GT. 0 ) THEN
                  DO ISTRN = 1,N_EMIS_ISTR
                    IF ( EM_SPEC( ISTRN ) .EQ. RULE_ISTR_SPEC( IC ) .AND.
     &                   EM_SURR( ISTRN,RULE_ISTR_ISRM( IC ) ) .EQ. '' .AND.
     &                   ANY( EM_SURR( ISTRN,: ) .EQ. RULE_ISTR_SURR( IC )) ) THEN
                      ! Add This Command to Instruction number ISTR
                      ISTR = ISTRN
                      LFOUND = .TRUE.
                    END IF
                  END DO
                  END IF

                  ! If no suitable instruction was found to add to, add a new
                  ! instruction. This means either there was no previous
                  ! instruction with the same CMAQ species and surrogate or there 
                  ! was an instruction with this CMAQ species but it already
                  ! had a surrogate and scale factor associated with this stream.
                  IF ( .NOT. LFOUND ) THEN
                    N_EMIS_ISTR = N_EMIS_ISTR + 1
                    ISTR = N_EMIS_ISTR

                    EM_SPEC( ISTR ) = RULE_ISTR_SPEC( IC )
                    MAP_EMtoDIFF( ISTR ) = RULE_ISTR_IDIFF( IC )

                    ! Link this row to the Aerosol or Gas Species
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
                  ISRM = RULE_ISTR_ISRM( IC )
                  IEM  = EM_STREAM_SIZE( ISRM )%REF( RULE_ISTR_PHASE( IC ) )
                  ISUR = RULE_ISTR_ISUR( IC )
                    
                  EM_SURR( ISTR, ISRM ) = RULE_ISTR_SURR( IC )
                  MAP_EMtoSURR( ISTR, ISRM ) = ISUR
                  EM_FILE_SURR( ISRM )%USED( ISUR ) = .TRUE.
                  MAP_EMtoSD( ISTR, ISRM ) = RULE_ISTR_PHASE( IC )
                  EM_FILE_DIFF( RULE_ISTR_IDIFF( IC ),ISRM ) = .TRUE.

                  ! Only apply an aerosol size-split parameter if this
                  ! species is an aerosol and if it is not from a dust 
                  ! or sea spray sector
                  AERO_SPLIT = 1.0
                  IF ( DIFF_MASK_AERO( RULE_ISTR_IDIFF( IC ) ) .AND. 
     &                 ISRM .NE. IDUSTSRM .AND. ISRM .NE. ISEASRM    )
     &                 AERO_SPLIT = SD_SPLIT( RULE_ISTR_IDIFF( IC ), IEM )

                  ! Determine Next Free Location in Scale Factor Space
                  ! (IFAC) so that the scale factor can be added.
                  DO IFAC = 1,N_SCALEFAC
                      IF ( EM_OP( ISTR, ISRM, IFAC ) .EQ. '' ) THEN
                           EM_FAC ( ISTR, ISRM, IFAC ) = EM_NML( IRULE)%FAC * AERO_SPLIT
                        EM_FAC_BULK ( ISTR, ISRM, IFAC ) = EM_NML( IRULE)%FAC
                        EM_REG( ISTR, ISRM, IFAC )    = REGNUM
                        CALL CHECK_OP( EM_NML( IRULE )%OP, IRULE )
                        EM_OP( ISTR, ISRM, IFAC )     = EM_NML( IRULE)%OP
                        CALL CHECK_BASIS( EM_NML( IRULE )%BASIS, IRULE )
                        EM_BASIS( ISTR, ISRM, IFAC )  = EM_NML( IRULE)%BASIS
                        EM_CONV( ISTR, ISRM, IFAC )  = EM_FILE_SURR( ISRM )%CONV( ISUR )
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
                   ISRM = RULE_ISTR_ISRM( IC )
                   DO ISTR = 1,N_EMIS_ISTR
                   IF ( EM_SPEC( ISTR ) .EQ. RULE_ISTR_SPEC( IC ) ) THEN
                      IF ( EM_SURR( ISTR, ISRM ) .EQ. RULE_ISTR_SURR( IC ) .AND.
     &                     MAP_EMtoSD( ISTR,ISRM ) .EQ. RULE_ISTR_PHASE( IC ) ) THEN
                  
                        ISUR = RULE_ISTR_ISUR( IC )
                        IEM = EM_STREAM_SIZE( ISRM )%REF( RULE_ISTR_PHASE( IC ) )
                        AERO_SPLIT = 1.0
                        IF ( DIFF_MASK_AERO( RULE_ISTR_IDIFF( IC ) ) .AND.
     &                       EM_NML( IRULE )%OP .EQ. 'o' ) 
     &                       AERO_SPLIT = SD_SPLIT( RULE_ISTR_IDIFF( IC ), IEM )

                        ! Determine Next Free Location in Scale Factor Space (IFAC) so 
                        ! that the scale factor can be added.
                        DO IFAC = 1,N_SCALEFAC
                          IF ( EM_OP( ISTR, ISRM, IFAC ) .EQ. '' ) THEN
                             EM_FAC ( ISTR, ISRM, IFAC )     = EM_NML( IRULE )%FAC * AERO_SPLIT
                             EM_FAC_BULK ( ISTR, ISRM, IFAC ) = EM_NML( IRULE )%FAC
                             EM_REG( ISTR, ISRM, IFAC )       = REGNUM
                             CALL CHECK_OP( EM_NML( IRULE )%OP, IRULE )
                             EM_OP( ISTR, ISRM, IFAC )        = EM_NML( IRULE )%OP
                             CALL CHECK_BASIS( EM_NML( IRULE )%BASIS, IRULE )
                             EM_BASIS( ISTR, ISRM, IFAC )     = EM_NML( IRULE )%BASIS
                             EM_CONV( ISTR, ISRM, IFAC )      = EM_FILE_SURR( ISRM )%CONV( ISUR )
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

! Inspect all of the Emissions Instructions for conversion based on 
! mass or moles. If there is a conversion, apply the correct factor
! based on species molecular weights.
         ALLOCATE( EM_FAC_ST( N_EMIS_ISTR,N_EM_SRM ) )
         DO ISTR = 1,N_EMIS_ISTR
            DO ISRM = 1,N_EM_SRM
               EM_FAC_ST( ISTR,ISRM )%LEN = 0
               IF ( MAP_EMtoSURR( ISTR,ISRM ) .NE. 0 ) THEN

               ! First load emission factor structure
               NFAC = 1
               DO IFAC = 2,N_SCALEFAC
                   IF ( EM_OP( ISTR,ISRM,IFAC ) .EQ. '' ) EXIT
                 NFAC = NFAC + 1  
               END DO

               ! Allocate and Prepopulate Emission Factor Structure
               EM_FAC_ST( ISTR,ISRM )%LEN = NFAC
               ALLOCATE( EM_FAC_ST( ISTR,ISRM )%FAC( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRM )%BULK( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRM )%BASIS( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRM )%REG( NFAC ) )
               ALLOCATE( EM_FAC_ST( ISTR,ISRM )%OP( NFAC ) )
                 
               EM_FAC_ST( ISTR,ISRM )%FAC  = 0.
               EM_FAC_ST( ISTR,ISRM )%BULK = 0.
               EM_FAC_ST( ISTR,ISRM )%BASIS= 1.
               EM_FAC_ST( ISTR,ISRM )%REG  = 1
               EM_FAC_ST( ISTR,ISRM )%OP   = 0
        
               ISUR  = MAP_EMtoSURR( ISTR,ISRM )
               SURR_MW = EM_FILE_SURR( ISRM )%MW( ISUR ) ! MW [g mol-1]
               SPEC_MW = DIFF_MW( MAP_EMtoDIFF( ISTR ) )


               ! Populate Emission Factor Structure
               DO IFAC = 1,NFAC
                 EM_FAC_ST( ISTR,ISRM )%FAC( IFAC )    = EM_FAC ( ISTR, ISRM, IFAC ) 
                 EM_FAC_ST( ISTR,ISRM )%BULK( IFAC )   = EM_FAC_BULK( ISTR, ISRM, IFAC )
                 EM_FAC_ST( ISTR,ISRM )%REG( IFAC )    = EM_REG( ISTR, ISRM, IFAC )
                      
                   SELECT CASE ( EM_OP( ISTR, ISRM, IFAC ) )
                   CASE ( 'a' )
                       EM_FAC_ST( ISTR,ISRM )%OP( IFAC )    = 1
                   CASE ( 'm' )
                       EM_FAC_ST( ISTR,ISRM )%OP( IFAC )    = 2
                   CASE ( 'o' )
                       EM_FAC_ST( ISTR,ISRM )%OP( IFAC )    = 3
                 END SELECT

                 BASIS_FAC = 1.0
                 IF ( EM_BASIS( ISTR, ISRM, IFAC ) .EQ. 'UNIT' ) THEN
                     BASIS_FAC = BASIS_FAC
                 ELSE IF ( EM_BASIS( ISTR, ISRM, IFAC ) .EQ. 'MOLE' ) THEN
                     IF ( EM_FILE_SURR( ISRM )%BASIS( ISUR ) .EQ. 'MOLE' ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE IF ( EM_FILE_SURR( ISRM )%BASIS( ISUR ) .EQ. 'MASS' ) THEN
                        ! Convert surrogate emission rate to moles
                        BASIS_FAC = BASIS_FAC / SURR_MW
                     ELSE
                        ! Unknown Basis has been encountered. Need to exit.
                        WRITE( XMSG,'(A,A16,A,I3,A,A,A)' ),
     &                     'ERROR: Surrogate Species ',TRIM(EM_FILE_SURR(ISRM)%ARRY(ISUR) ),
     &                     ' on emission stream ',ISRM, ' has units which are not recognized ',
     &                     'as an emission rate. If you wish to use this variable for ',
     &                     'emissions, please correct the units (e.g. g/s or moles/s).'
                        CALL M3EXIT( 'EMIS_SPC_MAP', 0, 0, XMSG, 2 )
                     END IF
                     
                     ! Check to see if this is an aerosol (mass-based)
                     ! CMAQ species.
                     IF ( .NOT. DIFF_MASK_AERO( MAP_EMtoDIFF( ISTR ) ) ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE
                        ! Convert species emission rate to mass
                        BASIS_FAC = BASIS_FAC * SPEC_MW
                     END IF

                 ELSE IF ( EM_BASIS( ISTR, ISRM, IFAC ) .EQ. 'MASS' ) THEN
                     IF ( EM_FILE_SURR( ISRM )%BASIS( ISUR ) .EQ. 'MOLE' ) THEN
                        ! Convert surrogate emission rate to mass
                        BASIS_FAC = BASIS_FAC * SURR_MW
                     ELSE IF ( EM_FILE_SURR( ISRM )%BASIS( ISUR ) .EQ. 'MASS' ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE
                        ! Unknown Basis has been encountered. Need to exit.
                        WRITE( XMSG,'(A,A16,A,I3,A,A,A)' ),
     &                     'ERROR: Surrogate Species ',TRIM(EM_FILE_SURR(ISRM)%ARRY(ISUR) ),
     &                     ' on emission stream ',ISRM, ' has units which are not recognized ',
     &                     'as an emission rate. If you wish to use this variable for ',
     &                     'emissions, please correct the units (e.g. g/s or moles/s).'
                        CALL M3EXIT( 'EMIS_SPC_MAP', 0, 0, XMSG, 2 )
                     END IF

                     ! Check to see if this is an aerosol (mass-based)
                     ! CMAQ species.
                     IF ( .NOT. DIFF_MASK_AERO( MAP_EMtoDIFF( ISTR ) ) ) THEN
                        ! Convert species emission rate to moles
                        BASIS_FAC = BASIS_FAC / SPEC_MW
                     ELSE
                        BASIS_FAC = BASIS_FAC
                     END IF
               
                 END IF
                 EM_FAC_ST( ISTR,ISRM )%BASIS = BASIS_FAC

                 IF ( EM_FAC_ST( ISTR,ISRM )%OP( IFAC ) .NE. 2 ) 
     &                EM_FAC_ST( ISTR,ISRM )%FAC( IFAC ) = 
     &                     EM_FAC_ST( ISTR,ISRM )%FAC( IFAC ) * BASIS_FAC
     &                       * EM_CONV( ISTR, ISRM, IFAC )
              END DO

              END IF
            END DO

        END DO !End Rule Loop

         ! Warn the User if there are no emissions instructions provided        
         IF ( N_EMIS_ISTR .LE. 0 ) THEN
            XMSG = 'There are no emissions instructions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         ! Print a message for each emissions stream surrogate that is not used.
         WRITE( LOGDEV, '(/,5x,A)' ) '|> Checking for unused Emissions Surrogates:'
         WRITE( LOGDEV, '(5x,A)'   ),'============================================'

         LERROR = .FALSE.
         DO ISRM = 1,N_EM_SRM
             N = EM_FILE_SURR( ISRM )%LEN
             N_UNUSED = COUNT( .NOT. EM_FILE_SURR( ISRM )%USED( : )
     &                         .AND. EM_FILE_SURR(ISRM)%ARRY( 1:N ) .NE. 'NOT_AVAILABLE'  )
             WRITE( LOGDEV, '(5x,4A,I2,A)' ), TRIM( EM_FILE_LAB( ISRM )),' | ',
     &              TRIM(EM_FILE_DESC( ISRM )),': ', N_UNUSED,' unused surrogates.'
             IF ( N_UNUSED .GT. 0 ) THEN
                 LERROR = .TRUE.

                 DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
                   IF ( .NOT.EM_FILE_SURR( ISRM )%USED( ISUR )
     &                  .AND.EM_FILE_SURR( ISRM )%ARRY( ISUR ) .NE. 'NOT_AVAILABLE' ) 
     &               WRITE( LOGDEV, '(10x,A)'), EM_FILE_SURR( ISRM )%ARRY( ISUR )
                 END DO
             END IF
             WRITE( LOGDEV, '()' )
         END DO
         IF ( LERROR ) WRITE( LOGDEV, '(5x,A,/,5x,A,/,5x,A)' ),
     &       'NOTE: Some Emissions Surrogates are unused by your current', 
     &       'emission control configuration. You may want to include these ',
     &       'emissions if they are relevant to your application.'

         ! Print a message for every used surrogate that is not recognized on 
         ! the EMIS_SURR_TABLE                 
         WRITE( LOGDEV, '(/,5x,A)' ) '|> Checking status of used Emissions Surrogates:'
         WRITE( LOGDEV, '(5x,A)'   ),'================================================'

         DO ICATCH = 1,N_SURR_CATCH
             B = EM_SURR_CATCH( ICATCH )
             N_USED = 0
             !L_CATCH = .FALSE.
             DO ISRM = 1,N_EM_SRM
                 N = EM_FILE_SURR( ISRM )%LEN
                 N_USED = N_USED + COUNT( EM_FILE_SURR(ISRM)%ARRY( 1:N ) .EQ. B 
     &                                    .AND. EM_FILE_SURR(ISRM)%USED( 1:N ) )

                 !N = INDEX1( B,EM_FILE_SURR(ISRM)%LEN, EM_FILE_SURR(ISRM)%ARRY )
                 !IF ( N .GT. 0 ) THEN
                 !  IF ( EM_FILE_SURR( ISRM )%USED( N ) ) L_CATCH = .TRUE.
                 !END IF
             END DO
             IF ( N_USED .GT. 0 )
     &           WRITE( LOGDEV, '(/,5x,3A,/,5x,A,/,5x,3A,/,5x,A,/,5x,A,/,5x,A)') 
     &                  'WARNING: Surrogate ',TRIM(B),' is being used but does not have a',
     &                  '         default entry in the EMIS_SURR_TABLE in EMIS_VARS.F. Currently,',
     &                  '         the molecular weight of ',TRIM(B),' is assumed to be 1.0',
     &                  '         g/mol for any emissions scaling operations requiring mass-mole',
     &                  '         conversions. If you are only prescribing UNIT scaling, then this',
     &                  '         assumption will not be problematic.'
         END DO 

! Resize Important Arrays
         MAP_EMtoDIFF = MAP_EMtoDIFF( 1:N_EMIS_ISTR )
         MAP_EMtoNUM  = MAP_EMtoNUM ( 1:N_EMIS_ISTR )
         MAP_EMtoSRF  = MAP_EMtoSRF ( 1:N_EMIS_ISTR )
         MAP_EMtoAERO = MAP_EMtoAERO( 1:N_EMIS_ISTR )


! An Emissions Scaling Map Now Exists as a 2D Array (NSPECIES x NSTREAMS).
! For every element, there is an associated surrogate and scale factor
! to be applied. For the aerosols, the scale factor will be modified
! later in order to split the mass into the appropriate modes.


! Print out the Tables of CMAQ Emissions Instructions organized by each 
! emission stream and then by CMAQ internal species.

         WRITE( LOGDEV, '(/,/,5x,A)' ),'|> EMISSIONS SCALING DIAGNOSTIC:'
         WRITE( LOGDEV, '(5x,A)'   ),'=================================='
         WRITE( LOGDEV,'(7x,A,/,13x,A)' ),'Note: these are populated using rules from the',
     &                              'Emission Control File Supplied by the User.'
         DO ISRM = 1,N_EM_SRM
           
           ! Print summary information about each Sector including
           ! the label and the available aerosol modes
           WRITE( LOGDEV, '(/,5x,A,A)'   ),'>',REPEAT('-',80 )
           WRITE( LOGDEV,'(5x,A,A,A,A,A2,I2.2,A1)' ),
     &            'Stream Type: "',TRIM(EM_FILE_DESC( ISRM )),
     &            '" | Sector Label: ',TRIM(EM_FILE_LAB( ISRM ) ),' (',ISRM,')'

           WRITE( LOGDEV, '(/8x,A)' ),'Table of Aerosol Size Distributions Available for Use Sector-Wide.'
           WRITE( LOGDEV, '(8x,A)' ),'Note that Mode 1 is reserved for gas-phase species and surrogates.'
           WRITE( LOGDEV, '(10x,A,2x,A,2x,A)' ), 'Number','Surrogate Mode','Reference Mode (see AERO_DATA.F)'
           WRITE( LOGDEV, '(10x,A,2x,A,2x,A)' ), '------','--------------','--------------------------------'
           DO ISD = 2,EM_STREAM_SIZE( ISRM )%LEN
              IEM = EM_STREAM_SIZE( ISRM )%REF( ISD )
              REFNAME = ''
              IF ( IEM .GT. 0 ) THEN
                  REFNAME = EM_AERO_REF( IEM )%NAME
              END IF

              WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),ISD, 
     &               EM_STREAM_SIZE( ISRM )%NAME( ISD )( 1:16 ), TRIM( REFNAME )
           END DO


           ! Finally Print Every Instruction for this Stream
           WRITE( LOGDEV, '(/,8x,A,5x,A,2x,A,9x,A,13x,A,1x,A,1x,A,1x,A)' ),
     &            'CMAQ Species','Phase/Mode','Surrogate','Region','Op','ScaleFac','Basis','FinalFac'
           WRITE( LOGDEV, '(8x,A,5x,A,2x,A,9x,A,13x,A,1x,A,1x,A,1x,A)' ),
     &            '------------','----------','---------','------','--','--------','-----','--------'

           DO IDIFF = 1,N_SPC_DIFF
             L_WDIFF = .TRUE.
             DO ISD = 1,EM_STREAM_SIZE( ISRM )%LEN
               L_WISD = .TRUE.
               DO ISTR = 1,N_EMIS_ISTR
             !    DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
                   IF ( EM_SPEC( ISTR ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &                  MAP_EMtoSD( ISTR,ISRM ) .EQ. ISD     ) THEN !  .AND.
   !  &                  MAP_EMtoSURR( ISTR,ISRM ) .EQ. ISUR ) THEN
                     IF ( L_WDIFF ) THEN
                        WRITE( LOGDEV, '(10x,A,1x,A,1x,A,1x,A,1x,A,2x,F6.3,4x,A,2x,F6.3)' ),
     &                         EM_SPEC( ISTR ),
     &                         EM_STREAM_SIZE( ISRM )%NAME( ISD )(1:10),
     &                         EM_SURR( ISTR, ISRM ),
     &                         EM_REGIONS( EM_FAC_ST( ISTR,ISRM )%REG( 1 ) )%LABEL(1:18),
     &                         EM_OP( ISTR,ISRM,1 ), 
     &                         EM_FAC_ST( ISTR, ISRM )%BULK( 1 ), EM_BASIS( ISTR,ISRM,1 ),
     &                         EM_FAC_ST( ISTR,ISRM )%FAC( 1 ) 
     &                         
                        L_WDIFF = .FALSE.
                        L_WISD  = .FALSE.
                      ELSE IF ( L_WISD ) THEN
                        WRITE( LOGDEV, '(27x,A,1x,A,1x,A,1x,A,2x,F6.3,4x,A,2x,F6.3)' ),
     &                         EM_STREAM_SIZE( ISRM )%NAME( ISD )(1:10),
     &                         EM_SURR( ISTR, ISRM ),
     &                         EM_REGIONS( EM_FAC_ST( ISTR,ISRM )%REG( 1 ) )%LABEL(1:18),
     &                         EM_OP( ISTR,ISRM,1 ),
     &                         EM_FAC_ST( ISTR, ISRM )%BULK( 1 ), EM_BASIS( ISTR,ISRM,1 ),
     &                         EM_FAC_ST( ISTR,ISRM )%FAC( 1 )
                        L_WISD = .FALSE.
                      ELSE
                        WRITE( LOGDEV, '(38x,A,1x,A,1x,A,2x,F6.3,4x,A,2x,F6.3)' ),
     &                         EM_SURR( ISTR, ISRM ),
     &                         EM_REGIONS( EM_FAC_ST( ISTR,ISRM )%REG( 1 ) )%LABEL(1:18),
     &                         EM_OP( ISTR,ISRM,1 ), 
     &                         EM_FAC_ST( ISTR, ISRM )%BULK( 1 ), EM_BASIS( ISTR,ISRM,1 ),
     &                         EM_FAC_ST( ISTR,ISRM )%FAC( 1 ) 
                      END IF    

                      IF ( EM_FAC_ST( ISTR,ISRM )%LEN .GT. 1 ) THEN
                      DO IFAC = 2,EM_FAC_ST( ISTR,ISRM )%LEN
                         WRITE( LOGDEV, '(55x,A,1x,A,2x,F6.3,4x,A,2x,F6.3)' ),
     &                          EM_REGIONS( EM_FAC_ST( ISTR,ISRM )%REG( IFAC ) )%LABEL(1:18),
     &                          EM_OP( ISTR,ISRM,IFAC ),
     &                          EM_FAC_ST( ISTR, ISRM )%BULK( IFAC ), EM_BASIS( ISTR,ISRM,IFAC ),
     &                          EM_FAC_ST( ISTR,ISRM )%FAC( IFAC )
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

      END SUBROUTINE EMIS_SPC_MAP

!-----------------------------------------------------------------------
         SUBROUTINE CHECK_EMIS_UNITS( ISRM, ISUR, SPEC, UNITS, CONV, BASIS )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         REAL, INTENT( OUT )              :: CONV
         CHARACTER( 4 ) , INTENT( OUT )   :: BASIS
         CHARACTER( 16 ), INTENT( INOUT ) :: UNITS
         INTEGER, INTENT( IN )            :: ISRM
         INTEGER, INTENT( IN )            :: ISUR
         CHARACTER( 16 ), INTENT( IN )    :: SPEC
         CHARACTER( 400 )      :: XMSG
         INTEGER               :: X, SLASH_IND

         CONV = 1.0

         CALL UPCASE( UNITS )

         ! Find Break between numerator and denominator. If it's not a
         ! slash, then it should be the first space
         SLASH_IND = INDEX( UNITS, '/' )
         IF ( SLASH_IND .EQ. 0 ) SLASH_IND = INDEX( UNITS, ' ' )
         X = SLASH_IND - 1

         ! Check for Molar or Mass Units
         IF ( UNITS(1:X) .EQ. 'MOLE' .OR. UNITS(1:X) .EQ. 'MOLES' .OR.
     &        UNITS(1:X) .EQ. 'MOL' ) THEN
            ! No Conversion Needed for Moles to Moles
            CONV = 1.0 
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'KMOLE' .OR. UNITS(1:X) .EQ. 'KMOLES' .OR.
     &             UNITS(1:X) .EQ. 'KMOL' ) THEN
            ! Convert kmol to mol
            CONV = 1000.0
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'MMOLE' .OR. UNITS(1:X) .EQ. 'MMOLES' .OR.
     &             UNITS(1:X) .EQ. 'MMOL' ) THEN
            ! Convert mmol to mol
            CONV = 1.0e-3
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'UMOLE' .OR. UNITS(1:X) .EQ. 'UMOLES' .OR.
     &             UNITS(1:X) .EQ. 'UMOL' ) THEN
            ! Convert umol to mol
            CONV = 1.0e-6
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'GRAM' .OR. UNITS(1:X) .EQ. 'GRAMS' .OR.
     &             UNITS(1:X) .EQ. 'G'    .OR. UNITS(1:X) .EQ. 'GM'    .OR.
     &             UNITS(1:X) .EQ. 'GMS'  .OR. UNITS(1:X) .EQ. 'GS'  ) THEN
            ! No Conversion Needed for Grams to Grams
            CONV = 1.0
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'KGRAM' .OR. UNITS(1:X) .EQ. 'KGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'KG'    .OR. UNITS(1:X) .EQ. 'KGM'    .OR.
     &             UNITS(1:X) .EQ. 'KGMS'  .OR. UNITS(1:X) .EQ. 'KGS' ) THEN
            ! Convert kg -> g
            CONV = 1000.0
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'MGRAM' .OR. UNITS(1:X) .EQ. 'MGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'MG'    .OR. UNITS(1:X) .EQ. 'MGM'    .OR.
     &             UNITS(1:X) .EQ. 'MGMS'  .OR. UNITS(1:X) .EQ. 'MGS' ) THEN
            ! Convert mg -> g
            CONV = 1.0e-3
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'UGRAM' .OR. UNITS(1:X) .EQ. 'UGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'UG'    .OR. UNITS(1:X) .EQ. 'UGM'    .OR.
     &             UNITS(1:X) .EQ. 'UGMS'  .OR. UNITS(1:X) .EQ. 'UGS' ) THEN
            ! Convert ug -> g
            CONV = 1.0e-6
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'NGRAM' .OR. UNITS(1:X) .EQ. 'NGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'NG'    .OR. UNITS(1:X) .EQ. 'NGM'    .OR.
     &             UNITS(1:X) .EQ. 'NGMS'  .OR. UNITS(1:X) .EQ. 'NGS' ) THEN
            ! Convert ng -> g
            CONV = 1.0e-9
            BASIS = 'MASS'
         ELSE
            WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A,A)' ),
     &              'WARNING: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &              'as an emission rate. If you wish to use this variable for ',
     &              'emissions, please correct the units. Otherwise grams will be ',
     &              'assumed for particles and moles for gases.'
            CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
            ! No Conversion and and Unknown Basis
            CONV = 1.0e0
            BASIS = 'UKNOWN'
         END IF
          
         ! Check for Time Units
         X = X + 2
         IF ( UNITS(X:) .EQ. 'S' .OR. UNITS(X:) .EQ. 'S-1' .OR.
     &        UNITS(X:) .EQ. 'SEC'.OR.UNITS(X:) .EQ. 'SEC-1' .OR.
     &        UNITS(X:) .EQ. 'SECOND'.OR.UNITS(X:) .EQ. 'SECOND-1' .OR.
     &        UNITS(X:) .EQ. 'SECONDS'.OR.UNITS(X:) .EQ. 'SECONDS-1' ) THEN
            ! No Conversion Necessary for seconds -> seconds
            CONV = CONV * 1.0
         ELSE IF ( UNITS(X:) .EQ. 'H' .OR. UNITS(X:) .EQ. 'H-1' .OR.
     &             UNITS(X:) .EQ. 'HR'.OR.UNITS(X:) .EQ. 'HR-1' .OR.
     &             UNITS(X:) .EQ. 'HOUR'.OR.UNITS(X:) .EQ. 'HOUR-1' .OR.
     &             UNITS(X:) .EQ. 'HOURS'.OR.UNITS(X:) .EQ. 'HOURS-1' ) THEN
            ! Convert hours -> seconds
            CONV = CONV * 3600.0
         ELSE IF ( UNITS(X:) .EQ. 'M' .OR. UNITS(X:) .EQ. 'M-1' .OR.
     &             UNITS(X:) .EQ. 'MIN'.OR.UNITS(X:) .EQ. 'MIN-1' .OR.
     &             UNITS(X:) .EQ. 'MINUTE'.OR.UNITS(X:) .EQ. 'MINUTE-1' .OR.
     &             UNITS(X:) .EQ. 'MINUTES'.OR.UNITS(X:) .EQ. 'MINUTES-1' ) THEN
            ! Convert hours -> seconds
            CONV = CONV * 60.0
         ELSE
            WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A,A)' ),
     &              'WARNING: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &              'as an emission rate. If you wish to use this variable for ',
     &              'emissions, please correct the units. Otherwise seconds will be ',
     &              'assumed for the time interval for these emissions.'
            CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
            ! No Conversion and and Unknown Basis
            CONV = CONV * 1.0e0
            BASIS = 'UNKNOWN'
         END IF

         END SUBROUTINE CHECK_EMIS_UNITS
 
!-----------------------------------------------------------------------
         SUBROUTINE CHECK_OP( OP, IRULE )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         CHARACTER( 1 )        :: OP
         INTEGER, INTENT( IN ) :: IRULE
         CHARACTER( 200 )      :: XMSG

         IF ( OP .EQ. 'A' .OR. OP .EQ. 'a' ) THEN
             OP = 'a'
         ELSE IF ( OP .EQ. 'M' .OR. OP .EQ. 'm' ) THEN
             OP = 'm'
         ELSE IF ( OP .EQ. 'O' .OR. OP .EQ. 'o' ) THEN
             OP = 'o'
         ELSE
             WRITE( XMSG,'(A,I3,A,A1)' ),
     &              'ERROR: OP parameter for rule ',IRULE,
     &              ' has invalid value: ',OP
             CALL M3EXIT( 'CHECK_OP (EMIS_DEFN)', 0, 0, XMSG, 2 )
         END IF

         END SUBROUTINE CHECK_OP

!-----------------------------------------------------------------------
         SUBROUTINE CHECK_BASIS( BASIS, IRULE )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         CHARACTER( 4 )        :: BASIS
         INTEGER, INTENT( IN ) :: IRULE
         CHARACTER( 200 )      :: XMSG

         IF ( BASIS .EQ. 'mole' .OR. BASIS .EQ. 'MOLE' .OR.
     &        BASIS .EQ. 'Mole' ) THEN
             BASIS = 'MOLE'
         ELSE IF ( BASIS .EQ. 'mass' .OR. BASIS .EQ. 'MASS' .OR.
     &             BASIS .EQ. 'Mass' ) THEN
             BASIS = 'MASS'
         ELSE IF ( BASIS .EQ. 'unit' .OR. BASIS .EQ. 'UNIT' .OR.
     &             BASIS .EQ. 'Unit' ) THEN
             BASIS = 'UNIT'
         ELSE
             WRITE( XMSG,'(A,I3,A,A4)' ),
     &              'ERROR: BASIS parameter for rule ',IRULE,
     &              ' has invalid value: ',BASIS
             CALL M3EXIT( 'CHECK_BASIS (EMIS_DEFN)', 0, 0, XMSG, 2 )
         END IF

         END SUBROUTINE CHECK_BASIS

!-----------------------------------------------------------------------
         SUBROUTINE EM_FILE_INIT( JDATE, JTIME )

! Initialize the counter for the total nbumber of emissions files. Also
! allocate memory for the vectors storing the labels of emission files
! and the maps from master ID number to the relative ID number for each
! gridded and point stream file, i.e. Emissions File 10 is also known as
! Point Source file 2.
!
         USE AERO_DATA, only : MGPG, GPKG
         USE UTILIO_DEFN         ! I/O API
         USE RXNS_DATA, ONLY: MECHNAME
         USE RUNTIME_VARS, only : LOGDEV

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME

         INTEGER ISRM, N

         CHARACTER( 16 )  :: PNAME = 'EM_FILE_INIT'
         CHARACTER( 32 )  :: VLAB
         CHARACTER( 16 )  :: VNAME
         INTEGER          :: STATUS
         CHARACTER( 200 ) :: XMSG
         LOGICAL :: SUCCESS
         CHARACTER( 200 ) :: EM_DIAG_PREFIX = 'CCTM_EMDIAG_'
         CHARACTER( 200 ) :: EM_DIAG_SUFFIX = ''
         LOGICAL        :: VALUE

         SUCCESS = .TRUE.

         ! Initialize Regions for Mapping Scale Factors Geographically for Base
         ! Model (eventually apply this to DDM-3D and ISAM)
!        CALL READ_EMISS_NAMELIST( )
!        CALL INIT_EMIS_REGIONS( )
    
       ! Calculate the total number of Emission Streams based on the
       ! user options
         ! Add the number of total Emission Streams 
         N_EM_SRM = N_FILE_GR + NPTGRPS + N_FILE_TR

         ! Online Biogenic Emissions
         IF ( BIOGEMIS ) N_EM_SRM = N_EM_SRM + 1

         ! Marine gas emissions; use online marine gas option only if CB6R3m is used
         IF ( USE_MARINE_GAS_EMISSION ) N_EM_SRM = N_EM_SRM + 1

         ! Lightning NO Emissions
         IF ( LTNG_NO ) N_EM_SRM = N_EM_SRM + 1
 
         ! Sea Spray Aerosol 
         IF ( OCEAN_CHEM ) N_EM_SRM = N_EM_SRM + 1

         ! Determine if WindBlown Dust is Requested
         IF ( WB_DUST ) N_EM_SRM = N_EM_SRM + 1

         ! Turn Back Now if N_EM_SRM Equals Zero (i.e. there are no emissions
         IF ( N_EM_SRM .EQ. 0 ) RETURN

       ! Allocate Emission File Structure Variables
         Allocate( EM_FILE_NAME  ( N_EM_SRM ) )
         Allocate( EM_FILE_LAB   ( N_EM_SRM+1 ) )
         Allocate( EM_FILE_TYPE  ( N_EM_SRM ) )
         Allocate( EM_FILE_ITYPE ( N_EM_SRM ) )
         Allocate( EM_FILE_DESC  ( N_EM_SRM ) )
         Allocate( EM_FILE_LAPPLY( N_EM_SRM ) )
         Allocate( EM_FILE_LDIAG ( N_EM_SRM+1 ) )
         Allocate( EM_DIAG_FILE  ( N_EM_SRM+1 ) )
         Allocate( EM_FILE_DATE  ( N_EM_SRM ) )
         Allocate( EM_FILE_SYM_DATE ( N_EM_SRM ) ) 
         Allocate( EM_FILE_FIRE  ( N_EM_SRM ) )
         Allocate( IGSRM         ( N_EM_SRM ) )
         Allocate( IPSRM         ( N_EM_SRM ) )
         Allocate( ITSRM         ( N_EM_SRM ) )
         Allocate( MAP_PTtoISRM  ( NPTGRPS ) )
         Allocate( EM_FILE_SURR  ( N_EM_SRM ) )
         Allocate( EM_GRID_LAYS  ( N_EM_SRM ) )
 
C Assign Attributes to Emission File Records. Other records will be 
C populated in individual subroutines. For example, opemis and
C stkemis_init.
         ISRM = 0
         EM_FILE_LAPPLY = .TRUE.
         EM_FILE_DATE   = JDATE
         EM_FILE_SYM_DATE = EMIS_SYM_DATE ! representative day files (default: false)
         EM_FILE_FIRE   = .FALSE.
         EM_GRID_LAYS   = 0
         
         EMISDIAG = RESOLVE_YN_TF_2D3D( EMISDIAG )
         EM_FILE_LDIAG   = EMISDIAG 

         ! Set the standard suffix for all Emissions Diagnostic Files
         IF ( OUTDIR(1:8 ) .NE. 'OUTDIR' ) EM_DIAG_PREFIX = TRIM( OUTDIR ) // '/' // EM_DIAG_PREFIX
         
         EM_DIAG_SUFFIX = '.nc'
         IF ( APPL_NAME(1:8 ) .NE. 'CTM_APPL' ) EM_DIAG_SUFFIX = '_' // TRIM(APPL_NAME) // '.nc'
 
         ! Set Diagnostic Type and Name for Total Emissions Diagnostic File
         CALL GET_ENV( EM_FILE_LDIAG(N_EM_SRM+1), "EMISDIAG_SUM", EMISDIAG, LOGDEV )
         EM_FILE_LDIAG( N_EM_SRM + 1 ) = RESOLVE_YN_TF_2D3D(EM_FILE_LDIAG(N_EM_SRM+1) )  
         EM_DIAG_FILE( N_EM_SRM+1 ) = 'SUM_EMDGFILE'
         VALUE = SETENVVAR( EM_DIAG_FILE( N_EM_SRM+1 ), TRIM( EM_DIAG_PREFIX ) // 
     &           'SUM' // TRIM( EM_DIAG_SUFFIX ) )  
         EM_FILE_LAB( N_EM_SRM+1 ) = 'TOTAL'

         ! Gridded Emission Files
         IF ( N_FILE_GR .GT. 0 ) THEN
           EM_FILE_TYPE( ISRM+1:ISRM+N_FILE_GR ) = 'GRID'
           EM_FILE_ITYPE( ISRM+1:ISRM+N_FILE_GR ) = 1
           DO N = 1, N_FILE_GR
              ISRM = ISRM + 1
              IGSRM( ISRM ) = N

              ! Create Description of this Emission File for output to
              ! diagnostics
              WRITE( EM_FILE_DESC( ISRM ), '(A,I3)' ),
     &           'Gridded Area Emissions File ', N
             
              ! Retrieve Short-Name Label for Each Gridded File
              WRITE( VLAB,'( "GR_EMIS_LAB_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LAB( ISRM ), VLAB,
     &                      EM_FILE_LAB( ISRM ), LOGDEV )
              CALL UPCASE( EM_FILE_LAB( ISRM ) )

              ! Each Gridded File Name is stored already in IO-API as
              ! an object of the form GR_EMIS_XXX
              WRITE( EM_FILE_NAME( ISRM ),'( "GR_EMIS_",I3.3 )' ) N

              ! Retrieve Toggle for whether or not to apply these
              ! emissions 
              WRITE( VLAB,'( "GR_EMIS_APPLY_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LAPPLY( ISRM ), VLAB, 
     &                      EM_FILE_LAPPLY( ISRM ), LOGDEV )
 
              ! Retrieve Toggle for overriding emissions file date with
              ! internal model date
              WRITE( VLAB,'( "GR_EM_SYM_DATE_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_SYM_DATE( ISRM ), VLAB, 
     &                      EMIS_SYM_DATE, LOGDEV )
 
              ! Retrieve Toggle for Outputing Gridded Emissions
              ! Diagnostic File
              WRITE( VLAB,'( "GR_EMIS_DIAG_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LDIAG( ISRM ), VLAB, EMISDIAG, LOGDEV )
              EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
              WRITE( EM_DIAG_FILE( ISRM ),'( "GR_EMDGFILE_",I3.3 )' ) N
              VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &            TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) ) 

           END DO
         END IF
         
         ! In-Line Point Source Files
         IF ( NPTGRPS .GT. 0 ) THEN 
           EM_FILE_TYPE( ISRM+1:ISRM+NPTGRPS ) = 'POINT'
           EM_FILE_ITYPE( ISRM+1:ISRM+NPTGRPS ) = 2
           DO N = 1, NPTGRPS
              ISRM = ISRM + 1
              IPSRM( ISRM ) = N
              MAP_PTtoISRM( N ) = ISRM
              
              ! Create Description of this Emission File for output to
              ! diagnostics
              WRITE( EM_FILE_DESC( ISRM ), '(A,I3)' ),
     &        'Point Emissions File ', IPSRM( ISRM )
             
              ! Retrieve Short-Name Label for Each Inline File
              WRITE( VLAB,'( "STK_EMIS_LAB_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LAB( ISRM ), VLAB, 
     &                      EM_FILE_LAB( ISRM ), LOGDEV )
              CALL UPCASE( EM_FILE_LAB( ISRM ) )
 
              ! Each Inline File Name is stored already in IO-API as
              ! an object of the form STK_EMIS_XXX
              WRITE( EM_FILE_NAME( ISRM ),'( "STK_EMIS_",I3.3 )' ) N
              
              ! Retrieve Toggle for whether or not to apply these
              ! emissions 
              WRITE( VLAB,'( "STK_EMIS_APPLY_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LAPPLY( ISRM ), VLAB, 
     &                      EM_FILE_LAPPLY( ISRM ), LOGDEV )
 
              ! Retrieve Toggle for overriding emissions file date with
              ! internal model date
              WRITE( VLAB,'( "STK_EM_SYM_DATE_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_SYM_DATE( ISRM ), VLAB, 
     &                      EMIS_SYM_DATE, LOGDEV )
 
              ! Retrieve Toggle for Outputing Inline Emissions
              ! Diagnostic File
              WRITE( VLAB,'( "STK_EMIS_DIAG_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LDIAG( ISRM ), VLAB, EMISDIAG, LOGDEV )
              EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
              WRITE( EM_DIAG_FILE( ISRM ),'( "STK_EMDGFILE_",I3.3 )' ) N
              VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &            TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

           END DO
         END IF
         
         ! Tracer Emissions
         IF ( N_FILE_TR .GT. 0 ) THEN 
           EM_TRAC = .TRUE.
           EM_FILE_TYPE( ISRM+1:ISRM+N_FILE_TR ) = 'TRAC'
           EM_FILE_ITYPE( ISRM+1:ISRM+N_FILE_TR ) = 3
           DO N = 1, N_FILE_TR
              ISRM = ISRM + 1
              ITSRM( ISRM ) = N

              ! Create Description of this Emission File for output to
              ! diagnostics
              WRITE( EM_FILE_DESC( ISRM ), '(A,I2)' ),
     &          'Gridded Tracer Emissions File ', N

              ! Retrieve Short-Name Label for Each Tracer File
              WRITE( VLAB,'( "TR_EMIS_LAB_",I2.2 )' ) N
              CALL GET_ENV( EM_FILE_LAB( ISRM ), VLAB,
     &                      EM_FILE_LAB( ISRM ), LOGDEV )
              CALL UPCASE( EM_FILE_LAB( ISRM ) )

              ! Each Tracer File Name is stored already in IO-API as
              ! an object of the form TR_EMIS_XXX
              WRITE( EM_FILE_NAME( ISRM ),'( "TR_EMIS_",I2.2 )' ) N

              ! Retrieve Toggle for Outputing Tracer Emissions
              ! Diagnostic File
              WRITE( VLAB,'( "TR_EMIS_DIAG_",I2.2 )' ) N
              CALL GET_ENV( EM_FILE_LDIAG( ISRM ), VLAB, EMISDIAG, LOGDEV )
              EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
              WRITE( EM_DIAG_FILE( ISRM ),'( "TR_EMDGFILE_",I2.2 )' ) N
              VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &            TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

           END DO
         ELSE
           EM_TRAC = .FALSE.
         END IF
         
         ! Online Biogenic Emissions (BEIS)
         IF ( BIOGEMIS ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'BIOG'
             EM_FILE_ITYPE( ISRM ) = 4
             EM_FILE_LAB ( ISRM ) = 'BIOG'
             EM_FILE_DESC( ISRM ) = 'Biogenic Emissions'

             ! Retrieve Toggle for Outputing Biogenic Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "BIOG_EMIS_DIAG", EMISDIAG, LOGDEV )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "BIOG_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             IBIOSRM = ISRM
         END IF

         ! Online Marine Gas Emissions
         IF ( USE_MARINE_GAS_EMISSION ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'MGEM'
             EM_FILE_ITYPE( ISRM ) = 5
             EM_FILE_LAB ( ISRM ) = 'MGEM'
             EM_FILE_DESC( ISRM ) = 'Marine Gas Emissions'

             ! Retrieve Toggle for Outputing Marine Gas Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "MG_EMIS_DIAG", EMISDIAG, LOGDEV )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "MG_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             IMGSRM = ISRM
         END IF

         ! Online Lightning NO Emissions
         IF ( LTNG_NO ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'LTNG'
             EM_FILE_ITYPE( ISRM ) = 6
             EM_FILE_LAB ( ISRM ) = 'LTNG'
             EM_FILE_DESC( ISRM ) = 'Lightning NO Emissions'

             ! Retrieve Toggle for Outputing Lightning NO Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "LTNG_EMIS_DIAG", EMISDIAG, LOGDEV )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "LTNG_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             ILTSRM = ISRM
         END IF

         ! Sea Spray Aerosol Emissions
         IF ( OCEAN_CHEM ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'ASEA'
             EM_FILE_ITYPE( ISRM ) = 7
             EM_FILE_LAB ( ISRM ) = 'SEASPRAY'
             EM_FILE_DESC( ISRM ) = 'Sea Spray Aerosol Emissions'
             
             ! Retrieve Toggle for Outputing Sea Spray Aerosol Emissions
             ! Diagnostic File
             CALL GET_ ENV( EM_FILE_LDIAG( ISRM ), "SEASPRAY_EMIS_DIAG", EMISDIAG, LOGDEV )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "ASEA_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             ISEASRM = ISRM
         END IF

         ! Wind-Blown Dust Emissions
         IF ( WB_DUST ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'DUST'
             EM_FILE_ITYPE( ISRM ) = 8
             EM_FILE_LAB ( ISRM ) = 'WBDUST'
             EM_FILE_DESC( ISRM ) = 'Wind-Blown Dust Emissions'
             
             ! Retrieve Toggle for Outputing Wind-Blown Dust Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "DUST_EMIS_DIAG", EMISDIAG, LOGDEV )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "DUST_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             IDUSTSRM = ISRM
         END IF
          
      END SUBROUTINE EM_FILE_INIT    


!-----------------------------------------------------------------------
      FUNCTION RESOLVE_YN_TF_2D3D( ARG ) RESULT( ARGOUT )

!-----------------------------------------------------------------------
        IMPLICIT NONE

        CHARACTER( 6 ) :: ARG, ARGOUT
 
         IF ( ARG .EQ. 'FALSE' .OR. ARG .EQ. 'F' .OR.
     &        ARG .EQ. 'NO'    .OR. ARG .EQ. 'N' ) THEN
            ARGOUT = 'FALSE'
         ELSEIF ( ARG .EQ. 'TRUE' .OR. ARG .EQ. 'T' .OR.
     &            ARG .EQ. 'YES'  .OR. ARG .EQ. 'Y' ) THEN
            ARGOUT = 'TRUE'
         ELSEIF ( ARG .EQ. '2D'  .OR. ARG .EQ. '2d' ) THEN
            ARGOUT = 'TRUE'
         ELSEIF   ( ARG .EQ. '3D'  .OR. ARG .EQ. '3d' ) THEN
            ARGOUT = '3D'
         ELSEIF   ( ARG .EQ. '2DSUM' .OR. ARG .EQ. '2dSUM' .OR.
     &              ARG .EQ. '2dsum' ) THEN
            ARGOUT = '2DSUM'
         END IF
 
         RETURN

      END FUNCTION RESOLVE_YN_TF_2D3D

!-----------------------------------------------------------------------
      SUBROUTINE OPEN_EMISS_DIAG( JDATE, JTIME, TSTEP )
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

      INTEGER :: ISRM, IVAR, NLAYERS, V
      CHARACTER( 2 ) :: CSRM
      
      DO ISRM = 1,N_EM_SRM+1
      
        ! Test whether or not each file should be opened
        IF ( EM_FILE_LDIAG( ISRM ) .NE. 'FALSE' ) THEN
 
          ! Make File 2D by Default, but 3D if the user requests it
          NLAYERS = 1
          IF ( EM_FILE_LDIAG( ISRM ) .EQ. '3D' ) NLAYERS = EMLAYS

          ! Set output file characteristics based on COORD.EXT and 
          ! open diagnostic file
          FTYPE3D = GRDDED3
          SDATE3D = JDATE
          STIME3D = JTIME
          TSTEP3D = TSTEP
          CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

          NVARS3D = 0
          DO IVAR = 1,N_SPC_DIFF
              IF ( ISRM .EQ. N_EM_SRM+1 ) THEN
                IF ( ANY(EM_FILE_DIFF( IVAR,: )) ) NVARS3D = NVARS3D + 1
              ELSE
                IF ( EM_FILE_DIFF( IVAR,ISRM ) ) NVARS3D = NVARS3D + 1
              END IF
          END DO
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
          DO IVAR = 1,N_SPC_DIFF
              IF ( ISRM .EQ. N_EM_SRM+1 ) THEN
                IF ( .NOT. ANY(EM_FILE_DIFF( IVAR,: )) ) CYCLE
              ELSE
                IF ( .NOT. EM_FILE_DIFF( IVAR,ISRM ) ) CYCLE
              END IF
             V = V + 1

             VTYPE3D( V ) = M3REAL
             VNAME3D( V ) = DIFF_SPC( IVAR )
             IF ( DIFF_MASK_GAS( IVAR ) ) THEN
                    ! GAS SPECIES
                    UNITS3D( V ) = 'mol s-1'
             ELSE IF ( DIFF_MASK_NR( IVAR ) ) THEN
                    ! NR SPECIES
                    UNITS3D( V ) = 'mol s-1'
             ELSE IF ( DIFF_MASK_NUM( IVAR ) ) THEN
                    ! AEROSOL NUMBER SPECIES
                    UNITS3D( V ) = 'particles s-1'
             ELSE IF ( DIFF_MASK_SRF( IVAR ) ) THEN
                    ! AEROSOL SURFACE AREA SPECIES
                    UNITS3D( V ) = 'm2 s-1'
             ELSE
                    ! AEROSOL MASS SPECIES
                    UNITS3D( V ) = 'g s-1'
             END IF
            
             VDESC3D( V ) = 'Emission Rate of ' // TRIM( VNAME3D( V ) )
     &                       // ' from ' // TRIM(EM_FILE_LAB( ISRM )) // ' emissions'
          END DO
          
          WRITE( CSRM, '(I2.2)' ), ISRM
          FDESC3D( 1 ) = 'Instantaneous pollutant emissions from stream ' //
     &               TRIM( CSRM ) // ': ' // TRIM(EM_FILE_LAB( ISRM ) ) 
          FDESC3D( 2:MXDESC3 ) = ''

          ! Open emissions stream diagnostic file
          IF ( .NOT. OPEN3( EM_DIAG_FILE( ISRM ), FSNEW3, PNAME ) ) THEN
             XMSG = 'Could not create the ' // TRIM( EM_DIAG_FILE( ISRM ) ) // ' file'
             CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF

        END IF   ! EM_FILE_LDIAG?
      END DO     ! ISRM


      END SUBROUTINE OPEN_EMISS_DIAG


C-----------------------------------------------------------------------
      END MODULE EMIS_DEFN
