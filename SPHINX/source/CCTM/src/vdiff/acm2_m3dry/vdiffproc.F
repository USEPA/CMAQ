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
      SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Asymmetric Convective Model v2 (ACM2) -- Pleim(2006)
C Function:
C   calculates and writes dry deposition.
C   calculates vertical diffusion

C Subroutines and Functions Called:
C   SEC2TIME, TIME2SEC, WRITE3, NEXTIME,
C   M3EXIT, EDDYX, TRI, MATRIX, PA_UPDATE_EMIS, PA_UPDATE_DDEP

C Revision History:
C   Analogous to VDIFFIM (Eddy diffusion PBL scheme)

C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module (GRID_CONF)
C    7 Jun 05 P.Bhave: added call to OPSSEMIS if MECHNAME='AE4';
C                      added TSTEP to RDEMIS_AE call vector
C    Aug 05 J. Pleim Update to v4.5
C
C    Jan 06 J. Pleim ACM2 implementation
C    Jul 06 J.Young: emis module
C    Nov 06 B.Hutzell, J.Young: depv module
C    Mar 08 G.Sarwar: added heterogeneous reaction for HONO. It affects HONO, NO2 and HNO3
C    Apr 08 J.Young: optional inline beis and plume rise emissions
C 21 Jun 10 J.Young: convert for Namelist redesign
C 16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C 10 May 11 J.Bash: updated for NH3 bidirectional exchange
C 11 May 11 D.Wong: - incorporated twoway model implementation
C 20 May 11 D.Schwede: modified to include mosaic
C 15 Aug 11 D.Schwede and G. Sarwar: Modified HONO calculation so that deposition
C           velocity for NO2 that is output in DEPV file does not include the loss due
C           to the heterogeneous reaction.
C           This additional loss is now accounted for in vdiff.F
C 14 Sep 11 J.Young: fix TSTEP bug in call to DEPV_INIT
C 11 Apr 13 J.Young: fix double adjustment of conc for DDBF in heterogeneous HONO
C           if-then-else clauses; eliminate some white space
C 13 May 13 J.Young: access met data from VDIFF_MET module
C                    change CRANKP to THBAR, CRANKQ to THETA
C 25 May 13 J.Young: re-do the acm/eddy algorithm for computational efficiency
C 30 Apr 14 J.Young: switch THBAR and THETA
C  2 May 14 J.Pleim, J.Young: replace the banded tridiagonal matrix solver for the
C           convective PBL, with the ACM1 matrix solver followed by the tridiagonal
C           matrix solver
C 30 May 14 J.Young: split vdiff calculation out of vdiff proc.
C 31 Jul 14 J.Young: incorporate gravitational settling for J-,K-mode aerosols
C 07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C 03 Jul 15 J.Young: put sedi stuff into a module
C 13 Aug 15 J.Bash: updated for MP model
C 15 Aug 15 D.Wong: - Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O 
C                     implementation
C                   - Added a block of code to allow non I/O processors
C                     to open CTM_DRY_DEP_1, CTM_VSED_DIAG, CTM_VDIFF_DIAG,
C                     CTM_DRY_DEP_MOS, and CTM_DRY_DEP_FST
C 03 Mar 16 G.Sarwar: updated for halogen emissions
C 16 Sep 16 J.Young: update for inline procan (IPR)
C 02 Nov 2018: L.Zhou, S.Napelenok: isam implementation
C  1 Feb 19 D.Wong: removed all MY_N clauses
C 12 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE GRID_CONF
      USE DESID_VARS, ONLY : VDEMIS_DIFF, DESID_LAYS
      USE DESID_MODULE, ONLY : DESID_INIT, DESID_DRIVER
      USE DEPV_DEFN
      USE ASX_DATA_MOD
!      USE MPAS_DATA_MOD
      USE VDIFF_MAP
      USE UTILIO_DEFN
      USE BIDI_MOD
      USE HGSIM
!      USE LSM_MOD, Only: n_lufrac
      USE SEDIMENTATION
      USE VDIFF_DIAG
#ifdef mpas
      use mio_module
      use coupler_module
      USE util_module, only : sec2time, time2sec, nextime, index1
      USE centralized_io_module
#else
      USE PA_UPDATE
      USE PA_DEFN, ONLY : LIPR, BUDGET_DIAG
#endif

#ifdef isam
      USE SA_DEFN, Only: ITAG, N_SPCTAG, VNAM_SPCTAG, SA_DDEP, SA_BIDI, ISAM, NTAG_SA, 
     &                   S_SPCTAG, NSPC_SA, ISAM_SPEC, OTHRTAG
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY: NP, NPMAX, SENGRID, SPCNAME, S_DDEP, SNGRD, SENGRID_TEMP, SEN_PAR
#endif 

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' '

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )              !  concentrations

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

C Parameters:

C External Functions: None

C Local Variables: 
      CHARACTER( 16 ), SAVE :: PNAME = 'VDIFFPROC'
      CHARACTER( 80 ) :: VARDESC                ! env variable description
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: WRITE_FIRSTIME = .TRUE.
      INTEGER, SAVE :: WSTEP  = 0               ! local write counter
      INTEGER  STATUS                           ! ENV... status

      REAL             FCMSF                          ! loop local RMSFX4( C,R )

      REAL, ALLOCATABLE, SAVE :: CNGRD( :,:,:,: )     ! cgrid aero in mixing ratio
      REAL, ALLOCATABLE, SAVE :: DDEP     ( :,:,: )   ! ddep accumulator
      REAL, ALLOCATABLE, SAVE :: DDEP0    ( :,:,: )   ! ddep initial before call to vdiffacmx
      REAL, ALLOCATABLE, SAVE :: NH3_EMIS0( :,: )     ! NH3_EMIS0 initial before call to get_depv
      REAL, ALLOCATABLE, SAVE :: ICMP     ( :,:,: )   ! component flux accumlator 
      REAL     :: WRDD( NCOLS,NROWS )                 ! ddep write buffer

      REAL, ALLOCATABLE, SAVE :: DDEP_PA  ( :,:,: )   ! ddep for process analysis
      REAL, ALLOCATABLE, SAVE :: EMIS_PA( :,:,:,: )   ! emis for process analysis

      INTEGER, SAVE :: N_SPC_CGRID              ! no. of CGRID species

      REAL     :: EDDYV ( NCOLS,NROWS,NLAYS )   ! from EDYINTB
      REAL     :: SEDDY ( NLAYS,NCOLS,NROWS )   ! flipped EDDYV
      REAL        DTSEC                         ! model time step in seconds

      REAL     :: NH3_FLUX( NCOLS,NROWS )

      REAL, ALLOCATABLE, SAVE :: VSED_AE( :,:,:,: )
      INTEGER, SAVE :: NH3_HIT

C Local Variables
#ifdef mpas
      CHARACTER (20) :: TIME_STAMP
#endif


! this is for creating the output name list
      character (512) :: fname
      character (30), allocatable, save :: name_list(:)
      character (30)  :: oname
      character (120) :: buf, buf2
      integer, save :: loc_n
      integer :: loc_nvars, stat, n, k, fnum
      logical :: found

      INTEGER     ASTAT
      INTEGER     C, R, L, S, V, I, J, OFF      ! loop induction variables
      INTEGER     MDATE, MTIME, MSTEP           ! internal simulation date&time
      CHARACTER(LEN=8), PARAMETER :: NAME_NH3_EMIS = 'NH3_Emis'
      CHARACTER(LEN=8), PARAMETER :: NAME_NH3_FLUX = 'NH3_Flux'
      CHARACTER(LEN=16) :: SPCBASE

#ifdef isam
      REAL NH3_SUM
      REAL, ALLOCATABLE, SAVE :: NH3_TAG_FRACTION( : )
      INTEGER :: NH3_ISAM_IDX = 0
      INTEGER, ALLOCATABLE, SAVE :: ISAM_DEPV( : )
      INTEGER IBGN, JTAG
      CHARACTER( 16 ) :: ISAM_SPECIES

      INTEGER, SAVE :: NH3_DV2DF_SPC_IDX = 0

#endif

      INTERFACE
         SUBROUTINE CONV_CGRID ( CGRID, JDATE, JTIME, CNGRD )
            REAL, POINTER :: CGRID( :,:,:,: )
            INTEGER,        INTENT( IN )  :: JDATE, JTIME
            REAL,           INTENT( INOUT ) :: CNGRD( :,:,:,: )
         END SUBROUTINE CONV_CGRID
         SUBROUTINE REV_CGRID ( CNGRD, JDATE, JTIME, CGRID )
            REAL,           INTENT( INOUT ) :: CNGRD( :,:,:,: )
            INTEGER,        INTENT( IN )  :: JDATE, JTIME
            REAL, POINTER :: CGRID( :,:,:,: )
         END SUBROUTINE REV_CGRID
         SUBROUTINE EDDYX ( EDDYV )
            REAL,           INTENT( OUT ) :: EDDYV( :,:,: )
         END SUBROUTINE EDDYX
         SUBROUTINE VDIFFACMX( dtsec, seddy, ddep, icmp, cngrd)
            REAL, INTENT( IN )    :: dtsec
            REAL, INTENT( INOUT ) :: seddy( :,:,: )
            REAL, INTENT( INOUT ) :: ddep ( :,:,: )
            REAL, INTENT( INOUT ) :: icmp ( :,:,: )
            REAL, INTENT( INOUT ) :: cngrd( :,:,:,: )
         END SUBROUTINE VDIFFACMX
#ifdef isam
         SUBROUTINE SA_OPWDDEP ( SADEPFILE, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            CHARACTER( 16 ) SADEPFILE ! Dry or wet dep file for tags
            INTEGER      JDATE      ! current model date, coded YYYYDDD
            INTEGER      JTIME      ! current model time, coded HHMMSS
            INTEGER      TSTEP      ! output time step
         END SUBROUTINE SA_OPWDDEP
#endif
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         CALL LOG_HEADING( LOGDEV, "Initialize Surface Exchange Module" )

         IF ( .NOT. DEPV_INIT ( JDATE, JTIME, TSTEP, CGRID ) ) THEN
            XMSG = 'Failure initializing deposition velocities module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifndef mpas
         IF ( BUDGET_DIAG .OR. LIPR ) THEN
            ALLOCATE( NH3_EMIS0( NCOLS,NROWS), STAT = ASTAT )
            CALL CHECKMEM( ASTAT,'NH3_EMIS0',PNAME )
         END IF
#endif

C create global maps
         IF ( .NOT. VDIFF_MAP_INIT( ) ) THEN
            XMSG = 'Failure initializing index mapping module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize the met data
         CALL INIT_MET( JDATE, JTIME )

        IF ( HGBIDI ) THEN ! Initialize HGSIM module
           CALL INIT_HGSIM(JDATE, JTIME)
        END IF

C Set output file characteristics based on COORD.EXT and open the dry dep file
         IF ( IO_PE_INCLUSIVE ) THEN
            CALL OPDDEP ( JDATE, JTIME, TSTEP( 1 ), N_SPC_DDEP )
#ifdef isam
            CALL SA_OPWDDEP ( SA_DD_1, JDATE, JTIME,TSTEP( 1 ) )
#endif
            IF ( ABFLUX .OR. HGBIDI ) CALL OPASX_MEDIA( JDATE, JTIME, TSTEP( 1 ) )
         END IF

C Open vdiff diagnostics file (ioapi header from cgrd)
         IF ( VDIFFDIAG ) THEN
            IF ( .NOT. VDIFF_DIAG_INIT ( JDATE, JTIME, TSTEP( 1 ), GRAV_SETL ) ) THEN
               XMSG = 'Failure initializing vdiff diagnostics module'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

C Allocate and initialize dry deposition array

         ALLOCATE ( DDEP( N_SPC_DEPV,NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         DDEP = 0.0   ! array assignment
         
         ALLOCATE ( DDEP0( N_SPC_DEPV,NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DDEP0'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         DDEP0 = 0.0   ! array assignment
 
         ALLOCATE ( ICMP( LCMP,NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ICMP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         ICMP = 0.0   ! array assignment

         CALL LOG_HEADING( LOGDEV, "Initialize Emissions Module" )

C Allocate and initialize emissions
         IF ( .NOT. DESID_INIT ( JDATE, JTIME, TSTEP( 1 ) ) ) THEN
            XMSG = 'Failure initializing emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifdef isam
         ALLOCATE ( SA_DDEP( NCOLS,NROWS,N_SPCTAG ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SA_DDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         SA_DDEP = 0.0
#endif

C Set up for budget accumulation and process analysis
         CALL LOG_HEADING( LOGDEV, "Initialize Surface Exchange for Process Analysis" )

#ifndef mpas
         IF ( BUDGET_DIAG .OR. LIPR ) THEN
            ALLOCATE ( EMIS_PA( NCOLS,NROWS,NLAYS,N_CGRID_SPC ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'EMIS_PA memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( DDEP_PA( NCOLS,NROWS,N_CGRID_SPC ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'DDEP_PA memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF
#endif

C Set up for grav. settling
         IF ( GRAV_SETL ) THEN
            CALL LOG_HEADING( LOGDEV, "Set Up Gravitational Settling" )

            ALLOCATE ( VSED_AE( N_AE_SPC,NLAYS,NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating VSED_AE'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         N_SPC_CGRID = SIZE ( CGRID,4 )

         ALLOCATE ( CNGRD( N_SPC_CGRID,NLAYS,NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CNGRD'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         CNGRD = 0.0   ! array assignment
 
         NH3_HIT = 0
         DO V = 1, N_SPC_DDEP
            IF ( DDEP_SPC( V ) .EQ. 'NH3' ) NH3_HIT = V
         END DO

#ifdef sens
         ALLOCATE ( S_DDEP( N_SPC_DEPV,NCOLS,NROWS,NPMAX ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating S_DDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         S_DDEP = 0.0

         ALLOCATE ( SNGRD( N_SPC_CGRID,NLAYS,NCOLS,NROWS,NPMAX ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SNGRD'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         SNGRD = 0.0
#endif

#ifdef isam
        IF ( SA_BIDI ) THEN
          NH3_ISAM_IDX  = INDEX1( 'NH3', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
          ALLOCATE ( ISAM_DEPV( N_SPCTAG ), 
     &               NH3_TAG_FRACTION( NTAG_SA ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure Allocating ISAM variables in VDIFF'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
          ISAM_DEPV     = 0
          DO ITAG = 1, N_SPCTAG
            DO V = 1, N_SPC_DEPV
              ISAM_SPECIES =  ISAM_SPEC( S_SPCTAG(ITAG),1 )
              IF ( TRIM( ISAM_SPECIES ) .EQ. DV2DF_SPC( V ) ) THEN
                ISAM_DEPV( ITAG ) = V
              END IF
            END DO
          END DO

          DO V = 1, N_SPC_DEPV
            IF ( DV2DF_SPC( V ) .EQ.'NH3' ) THEN
              NH3_DV2DF_SPC_IDX = V
            ENDIF
          END DO
        ENDIF
#endif

#ifdef mpas
! this is for creating the output name list
         found = .false.
         fnum = 0
         do while ((.not. found) .and. (fnum < mio_outfile_def_info%num_of_file_definitions))
            fnum = fnum + 1
            if ('CTM_OUT' == mio_outfile_def_info%flist(fnum)%fname) then
               found = .true.
            end if
         end do
         if (.not. found) then
            write (logdev, *) ' Abort: file CTM_OUT not on the file_input.txt'
            stop
         end if

         loc_nvars = mio_outfile_def_info%flist(fnum)%nvars
         loc_n = 0
         if (found) then
            allocate (name_list(loc_nvars), stat=stat)
            do n = 1, loc_nvars
               buf = mio_outfile_def_info%flist(fnum)%vlist(n)
               found = .false.
               k = 0
               do while (.not. found)
                  k = k + 1
                  if (buf(k:k) == ' ') then
                     found = .true.
                  end if
               end do
               if (buf(1:3) == 'DD_') then
                  loc_n = loc_n + 1
                  name_list(loc_n) = buf(4:k-1)
               end if
            end do
         end if
#endif

      END IF   !  if Firstime

      MDATE = JDATE
      MTIME = JTIME
      MSTEP = TIME2SEC( TSTEP( 2 ) )
      DTSEC = FLOAT( MSTEP )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

C Convert non-molar mixing ratio species and re-order CGRID
      CALL CONV_CGRID ( CGRID, MDATE, MTIME, CNGRD )

#ifdef sens
      DO NP = 1, NPMAX
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DO S = 1, N_SPC_CGRID
                     SENGRID_TEMP( C,R,L,S ) = SENGRID( C,R,L,NP,S )
                  END DO
               END DO
            END DO
         END DO
         CALL CONV_CGRID ( SENGRID_TEMP,MDATE, MTIME, SNGRD(:,:,:,:,NP) )
      END DO
#endif

C read & interpolate met data
      CALL GET_MET ( MDATE, MTIME, MSTEP )

C read & interpolate deposition velocities
#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) NH3_EMIS0 = NH3_EMIS
#endif
      CALL GET_DEPV ( MDATE, MTIME, TSTEP, CGRID )      

      IF ( GRAV_SETL ) THEN
C Get gravitational settling velocity for the vsed aero species:
C AERO_SEDV assumes that every aero species is dry deposited and is diffused (trns)
C Calculate the changes in the layer J-,K-mode aerosol concentrations
         CALL SEDI( MDATE, MTIME, DTSEC, VSED_AE, CGRID, CNGRD )
      END IF

C read & interpolate emissions data => VDEMIS_DIFF from EMIS_DEFN module
      CALL DESID_DRIVER ( MDATE, MTIME, TSTEP, CGRID )

#ifdef mpas
      DO R = 1, NROWS
         DO C = 1, NCOLS
            DO L = 1, NLAYS
               SEDDY( L,C,R ) = g3ddata(C,R,L,eddy_ind)
            END DO
         END DO
      END DO

#else

      IF ( BUDGET_DIAG .OR. LIPR ) THEN
         EMIS_PA = 0.0
         DO S = 1, N_SPC_DIFF
            DO L = 1, DESID_LAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     EMIS_PA( C,R,L,DIFF_MAP(S) ) = VDEMIS_DIFF( S,L,C,R )
                  END DO
               END DO
            END DO
         END DO
         CALL PA_UPDATE_EMIS ( EMIS_PA, JDATE, JTIME, TSTEP )
      END IF

      CALL EDDYX ( EDDYV )

C EDDYV returned = Kz, where Kz is in m**2/sec

      DO R = 1, NROWS
         DO C = 1, NCOLS
!            FCMSF = Grid_Data%RMSFX4( C,R )
            DO L = 1, NLAYS
               SEDDY( L,C,R ) = EDDYV( C,R,L )
            END DO
         END DO
      END DO
#endif

      IF ( WSTEP .EQ. 0 ) THEN
         DDEP = 0.0                      ! array assignment
         ICMP = 0.0                      ! array assignment
#ifdef sens
         S_DDEP = 0.0
#endif
      END IF

C Store Deposition Change in Budget and Process Analysis Arrays

#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) DDEP0 = DDEP
#endif

C Calculate the change in concentration and dry dep from vertical diffusion and vsed
C Note: cngrd is the argument keyword (from the INTERFACE); CNGRD is the actual argument
      CALL VDIFFACMX( DTSEC, SEDDY, DDEP, ICMP, CNGRD )

      IF ( VDIFFDIAG ) THEN
         NTICS = NTICS + 1
         NLPCR_SUM = NLPCR_SUM + NLPCR_MEAN    ! array assignment
         DO R = 1, NROWS
            DO C = 1, NCOLS
               NLPCR_MAX( C,R ) = MAX( NLPCR_MEAN( C,R ), NLPCR_MAX( C,R ) )
               NLPCR_MIN( C,R ) = MIN( NLPCR_MEAN( C,R ), NLPCR_MIN( C,R ) )
            END DO
         END DO
         IF ( GRAV_SETL ) THEN
            DTCCR_SUM = DTCCR_SUM + DTCCR_MEAN    ! array assignment
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DTCCR_MAX( C,R ) = MAX( DTCCR_MEAN( C,R ), DTCCR_MAX( C,R ) )
                  DTCCR_MIN( C,R ) = MIN( DTCCR_MEAN( C,R ), DTCCR_MIN( C,R ) )
               END DO
            END DO
         END IF
      END IF
 
C Store Deposition Change in Budget and Process Analysis Arrays
#ifndef mpas
      IF ( BUDGET_DIAG .OR. LIPR ) THEN
         DDEP_PA = 0.0
         DO V = 1, N_SPC_DEPV
            IF ( ABFLUX .AND. V .EQ. DD2DV(NH3_HIT) ) THEN 
               ! For Ammonia, take Bidirectional Flux into account
               ! if those processes are implemented
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     DDEP_PA( C,R,MAP_DEPVtoCGRID(V)) = DDEP( V,C,R ) - DDEP0( V,C,R )
     &                                            + NH3_EMIS( C,R ) - NH3_EMIS0( C,R )
                  END DO
               END DO
            ELSE
               ! All other species
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     DDEP_PA( C,R,MAP_DEPVtoCGRID(V)) = DDEP( V,C,R ) - DDEP0( V,C,R )
                  END DO
               END DO
            END IF
         END DO
         CALL PA_UPDATE_DDEP ( DDEP_PA, JDATE, JTIME, TSTEP )
      END IF
#endif
 
C Revert non-molar mixing ratio species and re-order CGRID
      CALL REV_CGRID ( CNGRD, MDATE, MTIME, CGRID )

#ifdef sens
      DO NP = 1, NPMAX
         CALL REV_CGRID ( SNGRD(:,:,:,:,NP), MDATE, MTIME, SENGRID_TEMP )
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DO S = 1, N_SPC_CGRID
                     SENGRID( C,R,L,NP,S ) = SENGRID_TEMP( C,R,L,S )
                  END DO
               END DO
            END DO
         END DO
      END DO
#endif

C If last call this hour:  write accumulated depositions:

      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
         MDATE = JDATE
         MTIME = JTIME
         CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )
         WSTEP = 0

#ifndef mpas
#ifdef parallel_io
         IF ( WRITE_FIRSTIME ) THEN
            WRITE_FIRSTIME = .FALSE.

            IF ( .NOT. IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSREAD3, PNAME ) ) THEN
                  XMSG = 'Could not open ' // TRIM(CTM_DRY_DEP_1)
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

#ifdef sens
               IF ( .NOT. OPEN3( CTM_SDRYDEP_1, FSNONIO, PNAME ) ) THEN
                  XMSG = 'Could not open ' // TRIM(CTM_SDRYDEP_1)
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
#endif 

            END IF   ! .NOT. IO_PE_INCLUSIVE
         END IF
#endif
#endif
         IF ( ABFLUX ) THEN
           DO R = 1, NROWS
               DO C = 1, NCOLS
                  NH3_FLUX(C,R) = DDEP( DD2DV(NH3_HIT),C,R )
                  DDEP( DD2DV(NH3_HIT),C,R ) = NH3_DDEP(C,R)
                  NH3_EMIS(C,R) = DDEP( DD2DV(NH3_HIT),C,R ) - NH3_FLUX(C,R)
               END DO
            END DO
#ifdef isam
            IF ( SA_BIDI ) THEN
              DO R = 1, NROWS
                DO C = 1, NCOLS
                  NH3_SUM = 0.0
                  DO ITAG = 1, NTAG_SA
                    NH3_SUM = NH3_SUM + ISAM( C,R,1,NH3_ISAM_IDX,ITAG )
                  END DO
                  IF ( NH3_SUM .GT. 1.0E-25 ) THEN
                    DO ITAG = 1, NTAG_SA
                      NH3_TAG_FRACTION( ITAG ) = ISAM( C,R,1,NH3_ISAM_IDX,ITAG ) / NH3_SUM
                    END DO
                  ELSE
                    NH3_TAG_FRACTION = 0.0
                  END IF
 
                  JTAG = 0 
                  DO ITAG = 1, N_SPCTAG
                    V = ISAM_DEPV( ITAG )
                    IF ( V .EQ. NH3_DV2DF_SPC_IDX ) THEN
                      JTAG = JTAG + 1
                      SA_DDEP( C,R,ITAG ) = SA_DDEP( C,R,ITAG ) + NH3_TAG_FRACTION( JTAG ) * NH3_EMIS(C,R)
                    END IF
                  END DO
                END DO
              END DO
            END IF
#endif
         ENDIF

#ifdef mpas
         call mio_time_format_conversion (mdate, mtime, time_stamp)

         ctm_out_clock = time_stamp
#endif

         DO V = 1, N_SPC_DDEP
            S = DD2DV( V )
#ifdef mpas
            if ((loc_n > 0) .and. mpas_diag) then
               k = index1( DDEP_SPC( V ), loc_n, name_list)
               if (k .gt. 0) then
                  oname = 'DD_' // DDEP_SPC( V )
                  call mio_fwrite ('CTM_OUT', oname, pname, DDEP( S,:,1), TIME_STAMP)
               end if
            end if

#else

            DO R = 1, NROWS
               DO C = 1, NCOLS
                  WRDD( C,R ) = DDEP( S,C,R )
               END DO
            END DO

            IF ( .NOT. WRITE3( CTM_DRY_DEP_1, DDEP_SPC( V ),
     &                 MDATE, MTIME, WRDD ) ) THEN
               XMSG = 'Could not write ' // CTM_DRY_DEP_1 // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
#endif
         END DO 

#ifndef mpas
C Write extra arrays for NH3 emission and net flux for bidi
         IF ( ABFLUX ) THEN

            IF ( .NOT. WRITE3( CTM_DRY_DEP_1, NAME_NH3_FLUX,
     &                 MDATE, MTIME, NH3_FLUX ) ) THEN
               XMSG = 'Could not write ' // CTM_DRY_DEP_1 // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
            IF ( .NOT. WRITE3( CTM_DRY_DEP_1, NAME_NH3_EMIS,
     &                 MDATE, MTIME, NH3_EMIS ) ) THEN
               XMSG = 'Could not write ' // CTM_DRY_DEP_1 // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
! must ensure the col/row being written to log is not larger than 
! the subdomain size for this PE. 
!          WRITE( LOGDEV,*) ' NH3emis, drydep, flux=',NH3_EMIS(27,18),
!    &                     DDEP_NH3(27,18),DDEP( DD2DV(NH3_HIT),27,18)
         NH3_DDEP = 0.0
         END IF

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', CTM_DRY_DEP_1,
     &         'for date and time', MDATE, MTIME
         
C Write vdiff diagnostics
         IF ( VDIFFDIAG ) THEN
            IF ( GRAV_SETL ) THEN   ! Write vsed diagnostics

               DO V = 1, N_VSED
                  S = VSED_MAP( V )
                  DO L = 1, NLAYS
                     DO R = 1, NROWS
                        DO C = 1, NCOLS
                           VSED_BUF( C,R,L,V ) = VSED_AE( S,L,C,R )
                        END DO
                     END DO
                  END DO
                  IF ( .NOT. WRITE3( CTM_VSED_DIAG, VSED_NAME( V ),
     &                               MDATE, MTIME, VSED_BUF( :,:,:,V ) ) ) THEN
                     XMSG = 'Could not write ' // TRIM( VSED_NAME( V ) )
     &                    // ' to ' // CTM_VSED_DIAG
                     CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                  END IF
               END DO

               WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &               'Timestep written to', CTM_VSED_DIAG,
     &               'for date and time', MDATE, MTIME

            END IF   ! GRAV_SETL

C Write other diagnostics
            NLPCR_MEAN = NLPCR_SUM / FLOAT( NTICS )
            IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'NLP_MEAN',
     &                         MDATE, MTIME, NLPCR_MEAN ) ) THEN
               XMSG = 'Could not write ' //  'NLP_MEAN to ' // CTM_VDIFF_DIAG
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
            IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'NLP_MAX',
     &                         MDATE, MTIME, NLPCR_MAX ) ) THEN
               XMSG = 'Could not write ' //  'NLP_MAX to ' // CTM_VDIFF_DIAG
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
            IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'NLP_MIN',
     &                         MDATE, MTIME, NLPCR_MIN ) ) THEN
               XMSG = 'Could not write ' //  'NLP_MIN to ' // CTM_VDIFF_DIAG
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
            NLPCR_MAX = 0.0      ! array assignment
            NLPCR_MIN = 9.9E30   ! array assignment
            NLPCR_SUM = 0.0      ! array assignment

            IF ( GRAV_SETL ) THEN   ! Write vsed diagnostics
               DTCCR_MEAN = DTCCR_SUM / FLOAT( NTICS )
               IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'SEDI_DTC_MEAN',
     &                            MDATE, MTIME, DTCCR_MEAN ) ) THEN
                  XMSG = 'Could not write ' //  'SEDI_DTC_MEAN to ' // CTM_VDIFF_DIAG
                  CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               END IF
               IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'SEDI_DTC_MAX',
     &                            MDATE, MTIME, DTCCR_MAX ) ) THEN
                  XMSG = 'Could not write ' //  'SEDI_DTC_MAX to ' // CTM_VDIFF_DIAG
                  CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               END IF
               IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'SEDI_DTC_MIN',
     &                            MDATE, MTIME, DTCCR_MIN ) ) THEN
                  XMSG = 'Could not write ' //  'SEDI_DTC_MIN to ' // CTM_VDIFF_DIAG
                  CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               END IF
               DTCCR_MAX = 0.0      ! array assignment
               DTCCR_MIN = 9.9E30   ! array assignment
               DTCCR_SUM = 0.0      ! array assignment
            END IF

            CNVCT = 0.0   ! array assignment
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  IF ( Met_Data%CONVCT( C,R ) ) CNVCT( C,R ) = 1.0
               END DO
            END DO
            IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'CONVCT',
     &                         MDATE, MTIME, CNVCT ) ) THEN
               XMSG = 'Could not write ' //  'convct to ' // CTM_VDIFF_DIAG
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. WRITE3( CTM_VDIFF_DIAG, 'LPBL',
     &                         MDATE, MTIME, REAL( Met_Data%LPBL ) ) ) THEN
               XMSG = 'Could not write ' //  'lpbl to ' // CTM_VDIFF_DIAG
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6, I6 )' )
     &            'Timestep written to', CTM_VDIFF_DIAG,
     &            'for date and time (and ntics)', MDATE, MTIME, NTICS
            NTICS = 0

         END IF
#endif
         
         IF ( ABFLUX .OR. HGBIDI ) THEN    
            CALL WRASX_MEDIA( MDATE, MTIME )
         END IF 


#ifdef isam
         WRDD = 0.0
         DO ITAG = 1, N_SPCTAG
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  WRDD( C,R ) = SA_DDEP( C,R,ITAG )
               END DO
            END DO

            IF ( .NOT. WRITE3( SA_DD_1, VNAM_SPCTAG( ITAG ),
     &                         MDATE, MTIME, WRDD ) ) THEN
               XMSG = 'Could not write ' // SA_DD_1 // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
         END DO
         SA_DDEP = 0.0
#endif

C re-set dry deposition array to zero

         DDEP = 0.0
         ICMP = 0.0

#ifdef sens
C WT - write out dry deposition sensitivities
         DO V = 1, N_SPC_DDEP
            S = DD2DV( V )
            SPCBASE = DDEP_SPC( V )( 1:12 )                       ! <- these lines have to
!           WRITE( LOGDEV,'(A)')'DDEP_SPC value: ' // TRIM( SPCBASE )
            DO NP = 1, NPMAX
               SPCNAME = TRIM( SPCBASE ) //'_' // SEN_PAR( NP )   ! <- match what is in opddep.F
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     WRDD( C,R ) = S_DDEP( S, C, R, NP )
                  ENDDO
               ENDDO
!           WRITE( LOGDEV,'(A)')'WRITE3D for ' // TRIM( SPCNAME )

               IF ( .NOT. WRITE3( CTM_SDRYDEP_1, SPCNAME,
     &                 MDATE, MTIME, WRDD ) ) THEN
                  XMSG = 'Could not write ' // CTM_SDRYDEP_1 // ' file'
                  CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               END IF

            ENDDO
         ENDDO
         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', CTM_SDRYDEP_1,
     &         'for date and time', MDATE, MTIME

         S_DDEP = 0.0
#endif

      END IF

      RETURN
      END
