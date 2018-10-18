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
         INTEGER         :: ERROR_NEG, MDATE, MTIME
         LOGICAL         :: WRTIME
         LOGICAL, SAVE   :: FIRST_TIME = .TRUE.

C-----------------------------------------------------------------------

         IF ( FIRST_TIME ) THEN
            FIRST_TIME = .FALSE.

            ALLOCATE( VDEMIS_READ  ( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),
     &                VDEMIS_SCALED( N_EMIS_ISTR,EMLAYS,NCOLS,NROWS ),
     &                VDEMIS_CONV  ( N_SPC_DIFF, EMLAYS,NCOLS,NROWS ) )

         END IF

C Initialize Emissions Array
         VDEMIS_DIFF = 0.0
         WRTIME = .FALSE.
         MTIME = JTIME
         MDATE = JDATE
         CALL NEXTIME( MDATE, MTIME,  SEC2TIME( TIME2SEC( TSTEP( 2 ) ) / 2 ) )
         IF ( MOD( TIME2SEC( MTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) WRTIME = .TRUE.



C Retrieve Emissions from All Stream Types
         IF ( N_EM_SRM .GT. 0 ) THEN
         DO ISRM = 1,N_EM_SRM
           VDEMIS_READ = 0.0

           SELECT CASE ( EM_FILE_ITYPE( ISRM ) )
             
             ! Retrieve Gridded and Tracer Emissions
             CASE ( 1, 3 )
               NL = EM_GRID_LAYS( ISRM )
               CALL GRIDEMIS( JDATE, JTIME, TSTEP, EM_FILE_NAME( ISRM ), 
     &                        NL, ISRM, VDEMIS_READ )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 .AND. 
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_READ( ISTR,1:NL,:,: ) = CONVEM_GAS( ISRM ) * VDEMIS_READ( ISTR,1:NL,:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve Point Source Emissions Streams
             CASE ( 2 )
               CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, EM_SURR( :,ISRM ), ISRM, VDEMIS_READ, NL )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 .AND.
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_READ( ISTR,1:NL,:,: ) = CONVEM_GAS( ISRM ) * VDEMIS_READ( ISTR,1:NL,:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )
             
             CASE ( 4 )
               NL = 1
               CALL GET_BEIS ( JDATE, JTIME, TSTEP )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 .AND.
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = CONVEM_GAS( ISRM ) *
     &                         VDEMIS_BI( MAP_EMtoSURR( ISTR,ISRM ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Marine Gas Emissions
             CASE ( 5 )
               NL = 1
               CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 .AND. 
     &                 MAP_EMtoGAS( ISTR ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = CONVEM_GAS( ISRM ) *
     &                          VDEMIS_MG( MAP_EMtoSURR( ISTR,ISRM ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Lightning NO Emissions
             CASE ( 6 )
               NL = EMLAYS
               CALL GET_LTNG ( JDATE, JTIME, TSTEP )
               DO ISTR = 1,N_EMIS_ISTR
                  IF ( MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 .AND. MAP_EMtoGAS( ISTR ) .GT. 0 ) THEN
                    DO L = 1,EMLAYS
                      VDEMIS_READ( ISTR,L,:,: ) = VDEMIS_LT( :,:,L ) * CONVEM_GAS( ISRM )
                    END DO
                  END IF
               END DO
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )

             ! Retrieve Sea Spray Aerosol Emissions 
             CASE ( 7 )
               NL = 1
               CALL GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL( 1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = 
     &                  SSOUTM( MAP_EMtoSURR( ISTR,ISRM ),MAP_EMtoMODE( ISTR ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

             ! Retrieve Wind-Blown Dust Emissions 
             CASE ( 8 )
               NL = 1
               CALL GET_DUST_EMIS ( JDATE, JTIME, TSTEP, Met_data%RJACM( :,:,1 ), CELLHGT( 1 ) )
               FORALL( ISTR = 1:N_EMIS_ISTR, MAP_EMtoSURR( ISTR,ISRM ) .GT. 0 )
                  VDEMIS_READ( ISTR,1,:,: ) = 
     &                  DUSTOUTM( MAP_EMtoSURR( ISTR,ISRM ),MAP_EMtoMODE( ISTR ),:,: ) 
               END FORALL
               CALL EMISS_SCALING( VDEMIS_READ, ISRM, NL, VDEMIS_SCALED )
               CALL EMISS_SIZE_DIST( ISRM, VDEMIS_SCALED, NL )

           END SELECT
           
           ! Convert All Emissions to VDIFF Units, Check for negative
           ! values, write out diagnostic file (if requested) and apply
           ! emissions to output array
           CALL EMISS_CONV_DIFF( VDEMIS_SCALED, NL, VDEMIS_CONV )
           ERROR_NEG     = EMISS_NEG_CHECK( VDEMIS_CONV, ISRM, NL )
           IF ( EM_FILE_LDIAG( ISRM ) .NE. 'FALSE' .AND. WRTIME ) 
     &                     CALL WRITE_EMISS_DIAG( MDATE, MTIME, VDEMIS_CONV, ISRM, NL )

           IF ( EM_FILE_LAPPLY( ISRM ) ) 
     &          VDEMIS_DIFF( :,1:NL,:,: ) = VDEMIS_DIFF( :,1:NL,:,: ) + VDEMIS_CONV( :,1:NL,:,: )
 
         END DO  
         END IF

         RETURN

         END SUBROUTINE GET_EMIS

!-----------------------------------------------------------------------
         SUBROUTINE GRIDEMIS ( JDATE, JTIME, TSTEP, EMIS_FNAME, FILE_LAYS, FSTREAM, VDEMIS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME

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
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            ! Get Domain Information for Emissions Read Routine
            CALL SUBHFILE ( EMIS_FNAME, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )

            ! Allocate Persistent Variables
            ALLOCATE( BUFF( NCOLS,NROWS,MAXVAL( EM_GRID_LAYS ) ) )
         END IF    !FirstTime
 
C Ensure that the model and emissions timestamp dates stay synchronized
         IF ( STDATE .NE. JDATE ) THEN
            NDATE = JDATE; NTIME = JTIME
            CALL NEXTIME( NDATE, NTIME, -TSTEP( 1 ) )       ! go back one output tstep
            CALL NEXTIME( EM_FILE_DATE( FSTREAM ), NTIME, TSTEP( 1 ) ) ! advance the start date one time step
         END IF

C Read & Interpolate Emissions 
         VDEMIS = 0.0

         DO ISTR = 1, N_EMIS_ISTR 
           VNAME = EM_SURR( ISTR, FSTREAM )
           IF ( VNAME .EQ. '' ) CYCLE

           BUFF = 0.0   ! array assignment
           IF ( .NOT. INTERPX( EMIS_FNAME, VNAME, PNAME,
     &                         STARTCOL,ENDCOL, STARTROW,ENDROW,1,FILE_LAYS,
     &                         EM_FILE_DATE( FSTREAM ), JTIME, BUFF ) ) THEN
              XMSG = 'Could not read '
     &             // TRIM( VNAME ) // ' from ' // EMIS_FNAME
              IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
           END IF

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
               IF ( EM_FAC_ST( ISTR,JSRM )%FAC( 1 ) .NE. 0.0 ) THEN
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
      ALLOCATE( MAP_FACtoISTR( NISTR ) )
      MAP_FACtoISTR = EM_FAC1_ST( ISRM )%MAP_FACtoISTR

      VDEMIS( MAP_FACtoISTR,1:NL,:,: ) = VDEMIS0( MAP_FACtoISTR,1:NL,:,: ) * 
     &        EM_FAC1_ST( ISRM )%FAC( 1:NISTR,1:NL,:,: )

      
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
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA, CONVPA
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
      INTEGER          GXOFF, GYOFF         ! global origin offset from file
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER       :: C, R, L, ISTR, INDX

      ! Get domain decomp info from the emissions file
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ! Populate Persistent Variables
         CNVTP = CONVPA * 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 

         ALLOCATE( CNVTI( EMLAYS ), CNVTC( EMLAYS,NCOLS,NROWS ), 
     &             CONVM( EMLAYS,NCOLS,NROWS ),
     &             CONVN( EMLAYS,NCOLS,NROWS )  )
      END IF    !FirstTime
      
      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
      CNVTI( 1:NL ) = CNVTP * Grid_Data%RDX3F( 1:NL ) 
      DO L = 1,NL
        CNVTC( L,:,: ) = CNVTI( L ) * Met_Data%RRHOJ( :,:,L )       ! Gas:       kmol/s -> ppmv/s
        CONVM( L,:,: ) = MWAIR / GPKG / Met_Data%DENS( :,:,L ) ! Aer. Mass: umol/m3/s -> ppmv/s
      END DO
      CONVN( 1:NL,:,: ) = CONVM( 1:NL,:,: ) * RAVO * 1.0E+06                ! Aer. Num:  N/m3/s -> N/molec/s
      
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

!     Check Emissions Array for Negative Values and Exit if one is
!     found. VDEMIS is the scaled emissions and VDEMIS0 is the emissions
!     read in from each stream.
!-----------------------------------------------------------------------
      USE GRID_CONF
      USE UTILIO_DEFN
         USE VDIFF_MAP, only : DIFF_SPC ! Name for every Diffusion Module Species 

      IMPLICIT NONE

      REAL, INTENT( IN )    :: VDEMIS ( :,:,:,: )
      INTEGER, INTENT( IN ) :: NL
      INTEGER ISRM, ISPC
      INTEGER ALOC( 4 )
      REAL EMIS_MIN

      CHARACTER(250) XMSG
      INTEGER STAT
      LOGICAL, SAVE :: FIRST_TIME = .TRUE.

      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          !ALLOCATE( VDEMIS( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ) )
      END IF

      STAT = 0

      ! If All Values Are Positive then Return Right Away
      EMIS_MIN = MINVAL( VDEMIS( :,1:NL,:,: ) )
      IF ( EMIS_MIN .GE. 0.0 ) RETURN

      ! Set Tolerance to 0.01% of Initial Emission Rate
      IF ( EMIS_MIN .LT. -1.0e-10 ) THEN

          STAT = 1

          ! Find Where the Most Negative Value Is and Exit
          ALOC = MINLOC( VDEMIS( :,1:NL,:,: ) )
          ISPC = ALOC( 1 )

          WRITE( LOGDEV, '(/,5x,A,/,5x,4A,/,5x,A)' ),
     &      'ERROR: Invalid Negative emission rates have been detected from ',
     &      TRIM(EM_FILE_LAB( ISRM )), ' for CMAQ species ',TRIM(DIFF_SPC( ISPC )),'.',
     &      'Please inspect the Emission Control Namelist File.'
          XMSG = 'Negative Emissions Detected'
          CALL M3EXIT( 'EMISS"NEG_CHECK', 1, 1, XMSG, XSTAT1 )

      END IF

      END FUNCTION EMISS_NEG_CHECK    

!-----------------------------------------------------------------------
      SUBROUTINE WRITE_EMISS_DIAG( MDATE, MTIME, VDEMIS0, ISRM, NL ) 

!     Write out emissions diagnostic file
!-----------------------------------------------------------------------
          
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA, CONVPA
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

      LOGICAL :: FIRST_TIME = .TRUE.
      CHARACTER( 200 ) :: XMSG

      ! Get pressure info from ASX_DATA_MOD
      IF ( FIRST_TIME ) THEN
         FIRST_TIME = .FALSE.
         CNVTP = CONVPA * 1.0E+06 * MWAIR / REAL( DX1 * DX2 ) !Conv. Factor for Gases 

         ALLOCATE( CNVTI( EMLAYS ), CNVTC( EMLAYS,NCOLS,NROWS ), 
     &             CONVM( EMLAYS,NCOLS,NROWS ),
     &             CONVN( EMLAYS,NCOLS,NROWS ),
     &             VDEMIS( N_SPC_DIFF,EMLAYS,NCOLS,NROWS ),
     &             VDEMIS_OUT( NCOLS,NROWS,EMLAYS ) )
      END IF    !FirstTime
      
      VDEMIS = 0.0
      LAYS = 1
      IF ( EM_FILE_LDIAG( ISRM ) .EQ. '3D' .OR. 
     &     EM_FILE_LDIAG( ISRM ) .EQ. '2DSUM' ) LAYS = NL

      ! Convert All Emissions to Units Appropriate for the Dispersion Solver
      CNVTI( 1:NL ) = CNVTP * Grid_Data%RDX3F( 1:NL ) 
      DO L = 1,NL
        CNVTC( L,:,: ) = 1.0E3 / ( CNVTI( L ) * Met_Data%RRHOJ( :,:,L ) )   ! Gas:       kmol/s -> ppmv/s
        CONVM( L,:,: ) = 1.0 / MWAIR * GPKG * Met_Data%DENS( :,:,L )        ! Aer. Mass: ppmv/s -> umol/m3/s
      END DO
      CONVN( 1:NL,:,: ) = CONVM( 1:NL,:,: ) / RAVO / 1.0E+12                ! Aer. Num:  N/molec/s -> N/cm3/s
      
      !MAKE SURE TO APPLY RELEVANT UNIT CONVERSION TO EACH
      !TYPE OF VARIABLE (GAS, AEROSOL MASS, AEROSOL NUMBER,
      !AND AEROSOL SURFACE AREA)
      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_GAS( I ) .OR. 
     &                           DIFF_MASK_NR( I )  .OR. 
     &                           DIFF_MASK_TRAC( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,: )
  
      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_AERO( I ) .AND. 
     &                           .NOT. DIFF_MASK_NUM( I ) .AND.
     &                           .NOT. DIFF_MASK_SRF( I )  )
     &    VDEMIS( I,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,: ) * DIFF_MW( I )
      
      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_SRF( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,: )
      
      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_NUM( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CONVN( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,: )

 
      ! Output to Emissions Diagnostic File
      DO IVAR = 1,N_SPC_DIFF
            ! Skip Writing this species if this source did not
            ! contribute to it
            IF ( .NOT. EM_FILE_DIFF( IVAR,ISRM ) ) CYCLE

            ! Sum the column if requested
            IF ( EM_FILE_LDIAG( ISRM ) .EQ. '2DSUM' ) THEN
                VDEMIS( IVAR,1,:,: ) = SUM( VDEMIS( IVAR,1:NL,:,: ),3 )
                LAYS = 1
            END IF

            DO L = 1,LAYS
                VDEMIS_OUT( :,:,L ) = VDEMIS( IVAR,L,:,: )
            END DO

            ! Write out the emissions data
            IF ( .NOT. WRITE3( EM_DIAG_FILE( ISRM ), DIFF_SPC( IVAR ),
     &                 MDATE, MTIME, VDEMIS_OUT( :,:,1:LAYS ) ) ) THEN
                XMSG = 'Could not write ' // EM_DIAG_FILE( ISRM ) // ' file'
                CALL M3EXIT( 'WRITE_EMISS_DIAG', MDATE, MTIME, XMSG, XSTAT1 )
            END IF
      END DO

      END SUBROUTINE WRITE_EMISS_DIAG
 
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
         INTEGER    :: N_UNUSED, REGNUM

         INTEGER    :: STRT, PTSTRT, ISPC, IDX, IX, ISUR, IRULE,
     &                 N, IM, IAERO, V, ISRM, NSPC, IA, IDIFF, JDX, JM,
     &                 ISTR, ISUR0, ISRM0, IEM, KDX, JSD, JEM, ISD,
     &                 IFAC, IRGN, ISTRN, NFAC
         REAL       :: AERO_SPLIT, UNIT_FAC_1, UNIT_FAC_2, SURR_MW, 
     &                 SPEC_MW, BASIS_FAC

         LOGICAL    :: LERROR, LFOUND, L_WDIFF, L_WISD, LTEST
         LOGICAL    :: LGAS_DIFF, LGAS_SURR
         
         CHARACTER( 16 )  :: SPECNAME, SN, SM

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
         INTEGER, ALLOCATABLE :: EM_REG( :,:,: )
         INTEGER             :: N_EM_RULE

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

         ! First Print Information about All the Available Regions for Scaling
         WRITE( LOGDEV,'(8x,A,2x,A,8x,A,10x,A)' ),'Number','Region Label','File Label','Variable'
         WRITE( LOGDEV,'(8x,A,2x,A,8x,A,10x,A)' ),'------','------------','----------','--------'
         DO IRGN = 1,N_EM_RGN
            WRITE( LOGDEV,'(8x,I3,5x,A18,2x,A18,2x,A)' ),IRGN, EM_REGIONS(IRGN)%LABEL( 1:18 ),
     &             EM_REGIONS( IRGN )%File( 1:18 ), TRIM(EM_REGIONS( IRGN )%VAR )
         END DO
 
! Retrieve the Surrogates Available From Emissions Streams
         ! Map the Emission Surrogate Species to Default CMAQ Internal
         ! Species, default size distributions, and default molecular
         ! weights based on SMOKE/MOVES/SPECIATE guidelines
         DO ISRM = 1,N_EM_SRM
            IF ( ISRM .EQ. ISEASRM .OR. ISRM .EQ. IBIOSRM .OR. 
     &           ISRM .EQ. IDUSTSRM.OR. ISRM .EQ. ILTSRM  .OR.
     &           ISRM .EQ. IMGSRM ) CYCLE

            ALLOCATE( EM_FILE_SURR( ISRM )%CMAQ ( EM_FILE_SURR( ISRM )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRM )%PHASE( EM_FILE_SURR( ISRM )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRM )%MW   ( EM_FILE_SURR( ISRM )%LEN ) )
            ALLOCATE( EM_FILE_SURR( ISRM )%USED ( EM_FILE_SURR( ISRM )%LEN ) )
            EM_FILE_SURR( ISRM )%USED = .FALSE.

            DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
               B = EM_FILE_SURR( ISRM )%ARRY( ISUR )
               IA  = INDEX1( B, N_MAP_EMIStoCMAQ, MAP_EMIStoCMAQ( : )%SURR )
               IF ( IA .GT. 0 ) THEN
                  EM_FILE_SURR( ISRM )%CMAQ ( ISUR ) = MAP_EMIStoCMAQ( IA )%CMAQ
                  EM_FILE_SURR( ISRM )%PHASE( ISUR ) = MAP_EMIStoCMAQ( IA )%PHASE
                  EM_FILE_SURR( ISRM )%MW   ( ISUR ) = MAP_EMIStoCMAQ( IA )%MW
               ELSE
                  ! Surrogate is not calculated online nor does it
                  ! belong to the default list of commonly used
                  ! surrogates.
                  EM_FILE_SURR( ISRM )%CMAQ ( ISUR ) = B
                  EM_FILE_SURR( ISRM )%PHASE( ISUR ) = 'N/A'
                  EM_FILE_SURR( ISRM )%MW   ( ISUR ) = 1.0
               END IF
            END DO
         END DO

! Set up Stream <-> Size Distribution relationship. This routine
! populates the EM_STREAM_SIZE structure which tells the logic below which
! modes are present on which streams.
         CALL EM_SD_INIT( JDATE, JTIME )
         
! Process Default Emissions Mapping (if requested in namelist; i.e. 
! DefaultScaling = TRUE) 
         ALLOCATE( EM_SPEC( NISTR0 ) )                ! CMAQ Species Names
         EM_SPEC = ""
         ALLOCATE( EM_SURR( NISTR0,N_EM_SRM ) )       ! Surrogate Name
         EM_SURR = ""
         ALLOCATE( EM_FAC ( NISTR0,N_EM_SRM,N_SCALEFAC ) ) ! Scale Factor
         EM_FAC = 0.0
         ALLOCATE( EM_FAC_BULK( NISTR0,N_EM_SRM,N_SCALEFAC ) ) ! Bulk Scale Factor For Printing to Diagnostic
         EM_FAC_BULK = 0.0                                     !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_OP( NISTR0,N_EM_SRM,N_SCALEFAC ) )       ! Bulk Scale Factor For Printing to Diagnostic
         EM_OP = ""                                            !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_REG( NISTR0,N_EM_SRM,N_SCALEFAC ) )      ! Bulk Scale Factor For Printing to Diagnostic
         EM_REG = 1                                            !   Output. Ignores aero_split and unit conversion
         ALLOCATE( EM_BASIS ( NISTR0,N_EM_SRM,N_SCALEFAC ) )   ! Mass or Mole Basis for Conversion
         EM_BASIS = ""
         ALLOCATE( MAP_EMtoDIFF( NISTR0 ) )           ! Map from Emissions Species 
         MAP_EMtoDIFF = 0                             !   to Diffusion Vector
         ALLOCATE( MAP_EMtoSURR( NISTR0,N_EM_SRM ) )  ! Map from Emissions Species 
         MAP_EMtoSURR = 0                             !   to Surrogate Locatino on File
         ALLOCATE( MAP_EMtoGAS( NISTR0 ) )            ! Map from Emissions Species 
         MAP_EMtoGAS = 0                              !   to aerosol table
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

         IF ( DefaultScaling ) THEN
            DO IDIFF = 1,N_SPC_DIFF
              DO ISRM = 1,N_EM_SRM

                 ! First Look for The Transported Species Name in the
                 ! Emissions Surrogate List. Then look for it on the
                 ! alternate list.
                 JDX = 0; JM  = 0; JSD = 0; JEM = 0

                 DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
                   IF ( EM_FILE_SURR( ISRM )%PHASE( ISUR ) .EQ. 'N/A' ) CYCLE

                   SN = EM_FILE_SURR( ISRM )%ARRY( ISUR )
                   SM = EM_FILE_SURR( ISRM )%CMAQ( ISUR )
                   ! Find Out which CMAQ aerosol modes are available
                   ! for this surrogate as a function of its streams
                   ! and emission mode
                   IF ( EM_FILE_SURR( ISRM )%PHASE( ISUR ) .EQ. 'GAS' ) THEN ! Gas Surrogate
                     IF ( .NOT.DIFF_MASK_AERO( IDIFF )       .AND.           ! Gas CMAQ Species
     &                   ( TRIM( SN ) .EQ. DIFF_SPC( IDIFF ) .OR.            ! Name Match
     &                     TRIM( SM ) .EQ. DIFF_SPC( IDIFF ) )    ) THEN
                       JDX = ISUR
                       JSD = 1    ! 1 is Always the Gas Phase in the Sector Size Distribution Vector
                       JEM = 0    ! 0 is Always the gas phase in the Size Dist Reference List
                       ! Update Molecular Weight of the surrogate if it
                       ! is not already set
                       IF ( EM_FILE_SURR( ISRM )%MW( ISUR ) .EQ. 0. )
     &                      EM_FILE_SURR( ISRM )%MW( ISUR ) = DIFF_MW( IDIFF )
                     END IF
                   ELSEIF ( DIFF_MASK_AERO( IDIFF ) ) THEN  ! Aerosol CMAQ Species and Surrogate
                     ISD = INDEX1( EM_FILE_SURR( ISRM )%PHASE( ISUR ), 
     &                             EM_STREAM_SIZE( ISRM )%LEN, EM_STREAM_SIZE( ISRM )%NAME )
                     IEM = EM_STREAM_SIZE( ISRM )%REF( ISD )
                     
                     ! Cycle through model aerosol modes and find ones that apply 
                     ! to this size distribution
                     DO IM = 1,N_MODE
                       IF ( EM_STREAM_SIZE( ISRM )%FACNUM( ISD,IM ) .GT. 0.0 ) THEN
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
                           IF ( EM_FILE_SURR( ISRM )%MW( ISUR ) .EQ. 0. )
     &                          EM_FILE_SURR( ISRM )%MW( ISUR ) = DIFF_MW( IDIFF )
                         END IF
                       END IF
                     END DO
                   END IF
                 END DO

                 ! Populate Entry in Emissions Instructions if a Match was Found
                 IF ( JDX .GT. 0 ) THEN
                    ! A Surrogate Matches a Transported Species. If both the Species 
                    ! and Surrogate Match a Previously Defined Instruction, then add 
                    ! the scale factor to that entry under the relevant stream number.
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
                    IF ( DIFF_MASK_AERO( IDIFF ) .AND. ISRM .NE. IDUSTSRM .AND.
     &                       ISRM .NE. ISEASRM ) 
     &                   AERO_SPLIT = SD_SPLIT( IDIFF, JEM )

                    ! Update the Surrogate of an Existing Instruction
                    EM_FILE_SURR( ISRM )%USED( JDX ) = .TRUE.
                    EM_SURR( N_EMIS_ISTR, ISRM ) = EM_FILE_SURR( ISRM )%ARRY( JDX )
                    MAP_EMtoSURR( N_EMIS_ISTR, ISRM ) = JDX
                    MAP_EMtoSD( N_EMIS_ISTR, ISRM ) = JSD
                    EM_FILE_DIFF( IDIFF,ISRM ) = .TRUE.

                    EM_FAC ( N_EMIS_ISTR, ISRM, 1 ) = 1.0 * AERO_SPLIT
                    EM_FAC_BULK ( N_EMIS_ISTR, ISRM, 1 ) = 1.0
                    EM_REG( N_EMIS_ISTR, ISRM, 1 ) = 1
                    EM_OP( N_EMIS_ISTR, ISRM, 1 )  = 'a'

                    IF ( DIFF_MASK_AERO( IDIFF ) ) THEN
                         EM_BASIS ( N_EMIS_ISTR, ISRM, 1 ) = 'MASS'
                    ELSE
                         EM_BASIS ( N_EMIS_ISTR, ISRM, 1 ) = 'MOLE'
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
               EM_FAC ( N_EMIS_ISTR+1,:,1 ) = 1.0
               EM_FAC_BULK ( N_EMIS_ISTR+1,:,1 ) = 1.0        
               EM_BASIS( N_EMIS_ISTR+1,:,1 ) = ''
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
               EM_BASIS( N_EMIS_ISTR+2,:,1 ) = ''
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
        ! Find Total Number of Rules
         N_EM_RULE = 0
         DO IRULE = 1,N_EM_RULE_REG
            IF( EM_NML( IRULE )%SPEC .EQ. '' ) EXIT
            N_EM_RULE = IRULE
         END DO

         ! Implement Default Scaling Rules that will Always Be Applied
         IF ( DefaultScaling ) CALL CUSTOM_EM_RULES( N_EM_RULE )


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
            CALL INTERPRET_EM_RULE_STREAM( EM_NML( IRULE )%STREAM, IRULE, EM_STREAM_RULE )

            !------   ------   ------   ------   ------   ----  
            ! Now error check and expand the surrogate field
            CALL UPCASE( EM_NML( IRULE )%SURR )

            !Initialize Surrogate Array for every Stream
            DO ISRM = 1,N_EM_SRM
              EM_SURR_RULE( ISRM )%ARRY = .FALSE.
            END DO

            LERROR = .TRUE.
            DO ISRM = 1,N_EM_SRM 
              ! Skip this stream if it is not identified
              IF ( .NOT. EM_STREAM_RULE( ISRM ) ) CYCLE 

              IF ( EM_NML( IRULE )%SURR .EQ. 'ALL' ) THEN
                 ! Expand the Rule to Apply to All Surrogates
                 EM_SURR_RULE( ISRM )%ARRY = .TRUE.
                 LERROR = .FALSE.
              ELSE
                 ! Find the Specific Surrogate this Rule Identifies
                 IDX = INDEX1( EM_NML( IRULE )%SURR, EM_FILE_SURR( ISRM )%LEN, 
     &                         EM_FILE_SURR( ISRM )%ARRY )
                 IF ( IDX .NE. 0 ) THEN
                   EM_SURR_RULE( ISRM )%ARRY( IDX ) = .TRUE.
                   LERROR = .FALSE.
                 END IF
              END IF
            END DO
            
            IF ( LERROR ) THEN
              IF ( .NOT. EMISCHK ) THEN
                 WRITE( LOGDEV, '(/,A,/,2A,3(/,A))') 
     &                  '****************************WARNING**************************:',
     &                  'The emission surrogate ',TRIM( EM_NML( IRULE )%SURR ),
     &                  ' was not found in emissions streams but the CTM_EMISCHK ',
     &                  'environment variable set to False so simulation will proceed.',
     &                  '**************************************************************'
                 WRITE( LOGDEV, * )
                 WRITE( LOGDEV, * )
                 XMSG = 'For optimal predictions, all surrogates '
     &                //'should be found on at least one stream.'
                 CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              ELSE     
                 WRITE( LOGDEV, '(A,/,2A,/,8(/,A))') 
     &                  '*****************************ERROR***************************************:',
     &                  'The emission surrogate ',TRIM( EM_NML( IRULE )%SURR ),
     &                  ' is not found in any of the emission streams.',
     &                  'Use one of the below options to continue.', 
     &                  '1) Change or remove the missing GC, NR and TR emissions surrogates from the',
     &                  'namelists where they are defined.', 
     &                  'or',
     &                  '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &                  'if model predictions are acceptable without using the above emissions.',
     &                  '*************************************************************************'
                 WRITE( LOGDEV, * )
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
                 WRITE( LOGDEV, '(3A,/,A,/,A,/,A,/,A)' ),
     &               'Species ',TRIM(EM_NML( IRULE )%SPEC),' was used in the Emissions',
     &               ' Control Instructions Namelist but it is not a valid CMAQ ',
     &               'transported species. Please add it to one of the ',
     &               'input chemical namelists (ie. GC, AE, etc). Note that aerosol',
     &               'Number and Surface Area Species are not valid for scaling.'
                 XMSG = 'Error in Emissions Map Processing.'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
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
              DO ISRM = 1,N_EM_SRM
              IF ( EM_STREAM_RULE( ISRM ) ) THEN
                DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN 
                IF ( EM_SURR_RULE( ISRM )%ARRY( ISUR ) ) THEN
                  DO ISD = 1,EM_STREAM_SIZE( ISRM )%LEN
                  IF ( EM_PHASE_RULE( ISRM )%ARRY( ISD ) ) THEN

                    ! Test to make sure if this is an aerosol species
                    ! that the size distirbution will actually populate it
                    LTEST = .TRUE.
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

                    ! Add an Instruction for this combination of CMAQ
                    ! Species, Stream, Surrogate, and Phase
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
                  ! if the CMAQ species matches exactly but this
                  ! surrogate is not present
                  LFOUND = .FALSE.
                  ! Look For a suitable previous instruction to add to.
                  IF ( N_EMIS_ISTR .GT. 0 ) THEN
                  DO ISTRN = 1,N_EMIS_ISTR
                    IF ( EM_SPEC( ISTRN ) .EQ. RULE_ISTR_SPEC( IC ) .AND.
     &                   EM_SURR( ISTRN,RULE_ISTR_ISRM( IC ) ) .EQ. '' ) THEN
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
                  ! had a surrogate and scale factor associated with this stream.
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
                           EM_FAC ( ISTR, ISRM, IFAC ) = EM_NML( IRULE )%FAC * AERO_SPLIT
                        EM_FAC_BULK ( ISTR, ISRM, IFAC ) = EM_NML( IRULE )%FAC
                        EM_REG( ISTR, ISRM, IFAC )    = REGNUM
                        CALL CHECK_OP( EM_NML( IRULE )%OP, IRULE )
                        EM_OP( ISTR, ISRM, IFAC )     = EM_NML( IRULE )%OP
                        CALL CHECK_BASIS( EM_NML( IRULE )%BASIS, IRULE )
                        EM_BASIS( ISTR, ISRM, IFAC )  = EM_NML( IRULE )%BASIS
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
! based on species moleculer weights.
         ALLOCATE( EM_FAC_ST( N_EMIS_ISTR,N_EM_SRM ) )
         DO ISTR = 1,N_EMIS_ISTR
            DO ISRM = 1,N_EM_SRM
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
                 IF ( EM_BASIS( ISTR, ISRM, IFAC ) .EQ. 'MOLE' ) THEN
                     IF ( EM_FILE_SURR( ISRM )%PHASE( MAP_EMtoSURR( ISTR,ISRM ) ) .EQ. 'GAS' ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE
                        ! Convert surrogate emission rate to moles
                        BASIS_FAC = BASIS_FAC / SURR_MW  
                     END IF
                     IF ( .NOT. DIFF_MASK_AERO( MAP_EMtoDIFF( ISTR ) ) ) THEN
                        BASIS_FAC = BASIS_FAC
                     ELSE
                        ! Convert species emission rate to mass
                        BASIS_FAC = BASIS_FAC * SPEC_MW
                     END IF
                 ELSE IF ( EM_BASIS( ISTR, ISRM, IFAC ) .EQ. 'MASS' ) THEN
                     IF ( EM_FILE_SURR( ISRM )%PHASE( MAP_EMtoSURR( ISTR,ISRM ) ) .EQ. 'GAS' ) THEN
                        ! Convert surrogate emission rate to mass
                        BASIS_FAC = BASIS_FAC * SURR_MW  
                     ELSE
                        BASIS_FAC = BASIS_FAC
                     END IF
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
               END DO
            END DO

         END DO

! Warn the User if there are no emissions instructions provided        
         IF ( N_EMIS_ISTR .LE. 0 ) THEN
            XMSG = 'There are no emissions instructions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

! Print a message for each emissions stream surrogate that is not used.
         WRITE( LOGDEV, '(/,5x,A)' ) '|> Checking for unused Emissions Surrogates'
         WRITE( LOGDEV, '(5x,A)'   ),'==========================================='

         LERROR = .FALSE.
         DO ISRM = 1,N_EM_SRM
             N_UNUSED = COUNT( .NOT. EM_FILE_SURR( ISRM )%USED( : ) )
             WRITE( LOGDEV, '(5x,4A,I2,A)' ), TRIM( EM_FILE_LAB( ISRM )),' | ',
     &              TRIM(EM_FILE_DESC( ISRM )),': ', N_UNUSED,' unused surrogates.'
             IF ( N_UNUSED .GT. 0 ) THEN
                 LERROR = .TRUE.

                 DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
                   IF ( .NOT. EM_FILE_SURR( ISRM )%USED( ISUR ) ) 
     &               WRITE( LOGDEV, '(10x,A)'), EM_FILE_SURR( ISRM )%ARRY( ISUR )
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
               DO ISUR = 1,EM_FILE_SURR( ISRM )%LEN
                 DO ISTR = 1,N_EMIS_ISTR
                   IF ( EM_SPEC( ISTR ) .EQ. DIFF_SPC( IDIFF ) .AND.
     &                  MAP_EMtoSD( ISTR,ISRM ) .EQ. ISD       .AND.
     &                  MAP_EMtoSURR( ISTR,ISRM ) .EQ. ISUR ) THEN
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
                 END DO
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
             CALL LOG_MESSAGE( OUTDEV, XMSG )
             STOP
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
         ELSE
             WRITE( XMSG,'(A,I3,A,A4)' ),
     &              'ERROR: BASIS parameter for rule ',IRULE,
     &              ' has invalid value: ',BASIS
             CALL LOG_MESSAGE( OUTDEV, XMSG )
             STOP
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
         USE MGEMIS,    only : MGEM     ! Flag for Marine Gas Emissions

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME

         INTEGER IOS, ISRM, N

         CHARACTER( 16 )  :: PNAME = 'EM_FILE_INIT'
         CHARACTER( 22 )  :: VLAB
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
         CALL READ_EMISS_NAMELIST( )
         CALL INIT_EMIS_REGIONS( )
    
       ! Calculate the total number of Emission Streams based on the
       ! user options
         ! Add the number of total Emission Streams 
         N_EM_SRM = N_FILE_GR + NPTGRPS + N_FILE_TR
 
         ! Online Biogenic Emissions
         IF ( BIOGEMIS ) N_EM_SRM = N_EM_SRM + 1

         ! Marine gas emissions; use online marine gas option only if CB6R3m is used
         IF ( INDEX( MECHNAME, 'CB6R3M' ) .GT. 0 ) then
            MGEM = .TRUE.
         ELSE
            MGEM = .FALSE.
         END IF        
         IF ( MGEM ) N_EM_SRM = N_EM_SRM + 1

         ! Lightning NO Emissions
         IF ( LTNG_NO ) N_EM_SRM = N_EM_SRM + 1
 
         ! Sea Spray Aerosol 
         IF ( SS_AERO ) N_EM_SRM = N_EM_SRM + 1

         ! Determine if WindBlown Dust is Requested
         IF ( WB_DUST ) N_EM_SRM = N_EM_SRM + 1

         ! Turn Back Now if N_EM_SRM Equals Zero (i.e. there are no emissions
         IF ( N_EM_SRM .EQ. 0 ) RETURN

       ! Allocate Emission File Structure Variables
         Allocate( EM_FILE_NAME  ( N_EM_SRM ) )
         Allocate( EM_FILE_LAB   ( N_EM_SRM ) )
         Allocate( EM_FILE_TYPE  ( N_EM_SRM ) )
         Allocate( EM_FILE_ITYPE ( N_EM_SRM ) )
         Allocate( EM_FILE_DESC  ( N_EM_SRM ) )
         Allocate( EM_FILE_LAPPLY( N_EM_SRM ) )
         Allocate( EM_FILE_LDIAG ( N_EM_SRM ) )
         Allocate( EM_DIAG_FILE  ( N_EM_SRM ) )
         Allocate( EM_FILE_DATE_OVRD ( N_EM_SRM ) )
         Allocate( EM_FILE_DATE  ( N_EM_SRM ) )
         Allocate( EM_FILE_FIRE  ( N_EM_SRM ) )
         Allocate( IGSRM         ( N_EM_SRM ) )
         Allocate( IPSRM         ( N_EM_SRM ) )
         Allocate( ITSRM         ( N_EM_SRM ) )
         Allocate( MAP_PTtoISRM  ( NPTGRPS ) )
         Allocate( EM_FILE_SURR  ( N_EM_SRM ) )
         Allocate( EM_GRID_LAYS  ( N_EM_SRM ) )
         Allocate( CONVEM_GAS    ( N_EM_SRM ) )
         Allocate( CONVEM_PM_MASS( N_EM_SRM ) )
         Allocate( EMIS_GAS_UNITS( N_EM_SRM ) )
         Allocate( EMIS_AER_UNITS( N_EM_SRM ) )
 
C Assign Attributes to Emission File Records. Other records will be 
C populated in individual subroutines. For example, opemis and
C stkemis_init.
         ISRM = 0
         EMIS_GAS_UNITS = "Unknown"
         EMIS_AER_UNITS = "Unknown"
         EM_FILE_LAPPLY = .TRUE.
         EM_FILE_DATE_OVRD = EMIS_DATE_OVRD
         EM_FILE_DATE   = JDATE
         EM_FILE_FIRE   = .FALSE.
         EM_GRID_LAYS   = 0
         
         EMISDIAG = RESOLVE_YN_TF_2D3D( EMISDIAG )
         EM_FILE_LDIAG   = EMISDIAG 

         ! Set the standard suffix for all Emissions Diagnostic Files
         IF ( OUTDIR(1:8 ) .NE. 'OUTDIR' ) EM_DIAG_PREFIX = TRIM( OUTDIR ) // '/' // EM_DIAG_PREFIX
         
         EM_DIAG_SUFFIX = '.nc'
         IF ( APPL_NAME(1:8 ) .NE. 'CTM_APPL' ) EM_DIAG_SUFFIX = '_' // TRIM(APPL_NAME) // '.nc'


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
     &                      EM_FILE_LAB( ISRM ), IOS )
              CALL UPCASE( EM_FILE_LAB( ISRM ) )

              ! Each Gridded File Name is stored already in IO-API as
              ! an object of the form GR_EMIS_XXX
              WRITE( EM_FILE_NAME( ISRM ),'( "GR_EMIS_",I3.3 )' ) N

              ! Retrieve Toggle for whether or not to apply these
              ! emissions 
              WRITE( VLAB,'( "GR_EMIS_APPLY_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LAPPLY( ISRM ), VLAB, 
     &                      EM_FILE_LAPPLY( ISRM ), IOS )
 
              ! Retrieve Toggle for overriding emissions file date with
              ! internal model date
              WRITE( VLAB,'( "GR_EM_DTOVRD_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_DATE_OVRD( ISRM ), VLAB, 
     &                      EMIS_DATE_OVRD, IOS )
 
              ! Retrieve Toggle for Outputing Gridded Emissions
              ! Diagnostic File
              WRITE( VLAB,'( "GR_EMIS_DIAG_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LDIAG( ISRM ), VLAB, EMISDIAG, IOS )
              EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
              WRITE( EM_DIAG_FILE( ISRM ),'( "GR_EMDGFILE_",I3.3 )' ) N
              VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &            TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) ) 

              ! Set Unit Conversions for Gas and Particle Phase Species
              CONVEM_GAS( ISRM ) = 1.0
              CONVEM_PM_MASS ( ISRM ) = 1.0
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
     &                      EM_FILE_LAB( ISRM ), IOS )
              CALL UPCASE( EM_FILE_LAB( ISRM ) )
 
              ! Each Inline File Name is stored already in IO-API as
              ! an object of the form STK_EMIS_XXX
              WRITE( EM_FILE_NAME( ISRM ),'( "STK_EMIS_",I3.3 )' ) N
              
              ! Retrieve Toggle for whether or not to apply these
              ! emissions 
              WRITE( VLAB,'( "STK_EMIS_APPLY_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LAPPLY( ISRM ), VLAB, 
     &                      EM_FILE_LAPPLY( ISRM ), IOS )
 
              ! Retrieve Toggle for overriding emissions file date with
              ! internal model date
              WRITE( VLAB,'( "STK_EM_DTOVRD_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_DATE_OVRD( ISRM ), VLAB, 
     &                      EMIS_DATE_OVRD, IOS )
 
              ! Retrieve Toggle for Outputing Inline Emissions
              ! Diagnostic File
              WRITE( VLAB,'( "STK_EMIS_DIAG_",I3.3 )' ) N
              CALL GET_ENV( EM_FILE_LDIAG( ISRM ), VLAB, EMISDIAG, IOS )
              EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
              WRITE( EM_DIAG_FILE( ISRM ),'( "STK_EMDGFILE_",I3.3 )' ) N
              VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &            TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

              ! Set Unit Conversions for Gas and Particle Phase Species
              CONVEM_GAS( ISRM ) = 1.0
              CONVEM_PM_MASS ( ISRM ) = 1.0
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
     &                      EM_FILE_LAB( ISRM ), IOS )
              CALL UPCASE( EM_FILE_LAB( ISRM ) )

              ! Each Tracer File Name is stored already in IO-API as
              ! an object of the form TR_EMIS_XXX
              WRITE( EM_FILE_NAME( ISRM ),'( "TR_EMIS_",I2.2 )' ) N

              ! Retrieve Toggle for Outputing Tracer Emissions
              ! Diagnostic File
              WRITE( VLAB,'( "TR_EMIS_DIAG_",I2.2 )' ) N
              CALL GET_ENV( EM_FILE_LDIAG( ISRM ), VLAB, EMISDIAG, IOS )
              EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
              WRITE( EM_DIAG_FILE( ISRM ),'( "TR_EMDGFILE_",I2.2 )' ) N
              VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &            TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

              ! Set Unit Conversions for Gas and Particle Phase Species
              CONVEM_GAS( ISRM ) = 1.0
              CONVEM_PM_MASS ( ISRM ) = 1.0
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
             CONVEM_GAS( ISRM ) = 1.0E-3
             CONVEM_PM_MASS ( ISRM ) = 1.0
             EMIS_GAS_UNITS( ISRM ) = "Unknown"

             ! Retrieve Toggle for Outputing Biogenic Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "BIOG_EMIS_DIAG", EMISDIAG, IOS )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "BIOG_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             IBIOSRM = ISRM
         END IF

         ! Online Marine Gas Emissions
         IF ( MGEM ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'MGEM'
             EM_FILE_ITYPE( ISRM ) = 5
             EM_FILE_LAB ( ISRM ) = 'MGEM'
             EM_FILE_DESC( ISRM ) = 'Marine Gas Emissions'
             CONVEM_GAS( ISRM ) = 1.0E-3        ! mol/s -> kmol/s
             CONVEM_PM_MASS ( ISRM ) = 1.0
             EMIS_GAS_UNITS( ISRM ) = "Unknown"

             ! Retrieve Toggle for Outputing Marine Gas Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "MG_EMIS_DIAG", EMISDIAG, IOS )
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
             CONVEM_GAS( ISRM ) = 1.0E-3
             CONVEM_PM_MASS ( ISRM ) = 1.0
             EMIS_GAS_UNITS( ISRM ) = "Unknown"

             ! Retrieve Toggle for Outputing Lightning NO Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "LTNG_EMIS_DIAG", EMISDIAG, IOS )
             EM_FILE_LDIAG( ISRM ) = RESOLVE_YN_TF_2D3D( EM_FILE_LDIAG( ISRM ) )
             EM_DIAG_FILE( ISRM ) = "LTNG_EMDGFILE"
             VALUE = SETENVVAR( EM_DIAG_FILE( ISRM ), TRIM( EM_DIAG_PREFIX ) // 
     &           TRIM( EM_FILE_LAB( ISRM ) ) // TRIM( EM_DIAG_SUFFIX ) )

             ILTSRM = ISRM
         END IF

         ! Sea Spray Aerosol Emissions
         IF ( SS_AERO ) THEN
             ISRM = ISRM + 1
             EM_FILE_TYPE( ISRM ) = 'ASEA'
             EM_FILE_ITYPE( ISRM ) = 7
             EM_FILE_LAB ( ISRM ) = 'SEASPRAY'
             EM_FILE_DESC( ISRM ) = 'Sea Spray Aerosol Emissions'
             CONVEM_GAS( ISRM ) = 1.0
             CONVEM_PM_MASS ( ISRM ) = MGPG
             EMIS_AER_UNITS( ISRM ) = "g m-3 s-1"
             
             ! Retrieve Toggle for Outputing Sea Spray Aerosol Emissions
             ! Diagnostic File
             CALL GET_ ENV( EM_FILE_LDIAG( ISRM ), "SEASPRAY_EMIS_DIAG", EMISDIAG, IOS )
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
             CONVEM_GAS( ISRM ) = 1.0
             CONVEM_PM_MASS ( ISRM ) = MGPG
             EMIS_AER_UNITS( ISRM ) = "g m-3 s-1"
             
             ! Retrieve Toggle for Outputing Wind-Blown Dust Emissions
             ! Diagnostic File
             CALL GET_ENV( EM_FILE_LDIAG( ISRM ), "DUST_EMIS_DIAG", EMISDIAG, IOS )
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
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_SPC, DIFF_MASK_GAS,
     &                      DIFF_MASK_AERO, DIFF_MASK_NUM, DIFF_MASK_SRF

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP

      CHARACTER( 16 ) :: PNAME = 'OPEN_EMISS_DIAG'
      CHARACTER( 200 ) :: XMSG

      INTEGER :: ISRM, IVAR, NLAYERS, V
      CHARACTER( 2 ) :: CSRM
      
      DO ISRM = 1,N_EM_SRM
      
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
              IF ( EM_FILE_DIFF( IVAR,ISRM ) ) NVARS3D = NVARS3D + 1
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
             IF ( .NOT. EM_FILE_DIFF( IVAR,ISRM ) ) CYCLE
             V = V + 1

             VTYPE3D( V ) = M3REAL
             VNAME3D( V ) = DIFF_SPC( IVAR )
             IF ( DIFF_MASK_GAS( IVAR ) ) THEN
                    ! GAS SPECIES
                    UNITS3D( V ) = 'mol/s'
             ELSE IF ( DIFF_MASK_NUM( IVAR ) ) THEN
                    ! AEROSOL NUMBER SPECIES
                    UNITS3D( V ) = 'particles/s'
             ELSE IF ( DIFF_MASK_SRF( IVAR ) ) THEN
                    ! AEROSOL SURFACE AREA SPECIES
                    UNITS3D( V ) = 'm2/s'
             ELSE
                    ! AEROSOL MASS SPECIES
                    UNITS3D( V ) = 'g/s'
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
