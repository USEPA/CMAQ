
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /disk40/EPRI_CMAQ/APT/cctm/BLD_apt/ping_apt.F,v 1.1 2009/07/07 01:00:08 cp194 Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PING ( JDATE, JTIME, TSTEP )

C   April  2005 - Prakash Karamchandani, AER: Updated for CMAQ-APT-PM
C   Oct.   2005 - Prakash Karamchandani, AER: Updated for CMAQ-MADRID-APT-Hg
C   May    2006 - Prakash Karamchandani, AER: Updated for CMAQ 4.5
C   Sep    2007 - Prakash Karamchandani, AER: Parallel version
C   April  2008 - Prakash Karamchandani, AER: CMAQ-AERO3 version
C   March  2012 - Prakash Karamchandani, ENVIRON: CMAQ 5.0 version
C-----------------------------------------------------------------------
      USE PCGRID_DEFN ! inherits GRID_CONF and CGRID_SPCS:NSPCSD
      USE UTILIO_DEFN           ! replace I/O API include with UTILIO_DEFN
Cjmg      USE CGRID_SPCS, ONLY: N_GC_SPC, N_AE_SPC, N_NR_SPC, N_TR_SPC,
Cjmg&                           GC_SPC, AE_SPC, NR_SPC, TR_SPC
      USE CGRID_SPCS
      USE DEPV_DEFN   ! dry dep velocities
      USE WVEL_DEFN   ! derived vertical velocities
      USE SUBST_MODULES         ! stenex

      IMPLICIT NONE

      INCLUDE SUBST_FILES_APT   ! APT diagnostic file names

#ifdef parallel
      INCLUDE SUBST_MPI
#endif

C Arguments:

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

C Local variables:

      REAL, POINTER     :: CGRID( :,:,:,: )

#ifdef parallel
! Global arrays
      REAL, ALLOCATABLE, SAVE :: GCGRID( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: GDEPV( :,:,: )
      REAL, ALLOCATABLE, SAVE :: GWVEL( :,:,: )

      INTEGER, SAVE :: GCSIZE      ! Message size of global conc array
      INTEGER, SAVE :: GDSIZE      ! Message size of global dep array
      INTEGER, SAVE :: GWSIZE      ! Message size of global wvel array

      INTEGER :: ALLOCSTAT

      INTEGER        MSGSIZE       ! Message size of subgrid to receive
      INTEGER        IPE           ! For loop over processors
      INTEGER        WHO           ! For identifying sending processor

      INTEGER        STATUS_MPI( MPI_STATUS_SIZE )   ! MPI status code

      INTEGER, PARAMETER :: TAG1 = 901 ! MPI message tag for processor ID
      INTEGER, PARAMETER :: TAG2 = 902 ! MPI message tag for CGRID data array.
      INTEGER, PARAMETER :: TAG3 = 903 ! MPI message tag for DEPV data array.
      INTEGER, PARAMETER :: TAG4 = 904 ! MPI message tag for WVEL data array.

      INTEGER  LOC

      INTEGER        IR            ! Loop counter over grid rows
      INTEGER        IC            ! Loop counter over grid columns
      INTEGER        IL            ! Loop counter over grid layers
      INTEGER        C0            ! First column in global grid
      INTEGER        R0            ! First row in global grid
      INTEGER        NC            ! Number of columns in local grid
      INTEGER        NR            ! Number of rows in local grid

      INTEGER, SAVE :: RECVBUF_SIZE = 0
      INTEGER :: RCSIZE, RVDSIZE, RWVSIZE, RSIZE
      REAL, ALLOCATABLE, DIMENSION( : ) :: RECVBUFC,RECVBUFD,RECVBUFW

      INTEGER, SAVE :: NPROCS      ! Total number of processors
      INTEGER IERROR               ! Error code: 0 for ok, 1 for fail

      INTEGER, ALLOCATABLE, SAVE :: NCOLS_PE( : )  ! Column range for each PE
      INTEGER, ALLOCATABLE, SAVE :: NROWS_PE( : )  ! Row range for each PE
#endif
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
 
      INTEGER, SAVE :: LOGDEV
      INTEGER ::       NFL              ! Loop index for no. of files
      INTEGER ::       SPC, V           ! loop counters
      INTEGER ::       STRT, FINI       ! loop counters

      INTEGER ::       STATUS           ! ENV... status
      CHARACTER( 80 ) :: VARDESC        ! environment variable description

      CHARACTER( 200 ) :: XMSG = ' '
      CHARACTER( 80 )  :: MSG = ' '
      CHARACTER( 16 )  :: PNAME = 'PING'
      CHARACTER( 16 )  :: CTM_RUNLEN = 'CTM_RUNLEN'  !  environment variable run duration
      CHARACTER( 16 )  :: APT_RESTART = 'APT_RESTART'  !  environment variable restart run
      CHARACTER( 16 )  :: APT_DIAG = 'APT_DIAG'  !  environment variable for APT diagnostics
      CHARACTER( 16 )  :: CTM_PROJNAME = 'CTM_PROJNAME' ! driver program name
      CHARACTER( 16 )  :: PROJNAME
      CHARACTER( 40 )  :: NAMEIN
      CHARACTER( 256 ) :: RET_VAL   ! Returned value of environment variable

      INTEGER      RUNLEN            ! run duration, HHMMSS
      INTEGER      STEPSECS          ! seconds per time step
      INTEGER      TOTSECS           ! run duration seconds

      LOGICAL   :: RSTFLG            ! flag for restart run, default = [F]
      LOGICAL, SAVE :: DIAGFLG       ! flag for APT diagnostics, default = [F]

      REAL TMSTEP
      REAL TENDHR

      INTEGER, SAVE :: NSTEPS    ! run duration, as number of output time steps
      REAL, SAVE :: DTSAVE

      INTERFACE
         SUBROUTINE STEP_PIG( CGRID, DEPV, WVEL, JDATE, JTIME, MSTEP,
     &                        TSTEP, OUTSTEP, NSTEPS, DIAGFLG )
         IMPLICIT NONE

         REAL :: CGRID( :,:,:,: )
         REAL, INTENT(IN) :: DEPV( :,:,: )
         REAL, INTENT(IN) :: WVEL( :,:,: )

         INTEGER :: JDATE, JTIME
         INTEGER :: MSTEP( 3 )
         REAL :: TSTEP, OUTSTEP
         INTEGER :: NSTEPS
         LOGICAL :: DIAGFLG

         END SUBROUTINE STEP_PIG

         SUBROUTINE INIT_SCICHEM(NAMEIN,LFLAG,STDATE,STTIME,TSTEP,TENDHR,
     &                           DTSAVE,CGRID)
         IMPLICIT NONE
         CHARACTER(40) :: NAMEIN
         LOGICAL LFLAG        !RESTART FLAG
         INTEGER, INTENT( IN ) :: STDATE, STTIME
         integer, INTENT( IN ) :: TSTEP        !Model time step,  coded HHMMSS
         REAL :: TENDHR       !Final hour of simulation (relative to start)
         REAL :: DTSAVE       !Time interval to save puff information
         REAL :: CGRID( :,:,:,: )

         END SUBROUTINE INIT_SCICHEM

      END INTERFACE

      CGRID => PCGRID( 1:MY_NCOLS,1:MY_NROWS,:,: )

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         WRITE( LOGDEV,* ) '    '
         WRITE( LOGDEV,* ) '    Using SCICHEM Plume-in-Grid process'
         WRITE( LOGDEV,* ) '    '

         IF ( .NOT. W_VEL ) THEN
             XMSG = 'Set save derived vertical velocity component to true'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( MYPE == 0 ) THEN
            RSTFLG = .FALSE.         ! default
            VARDESC = 'Continuation run flag'
            RSTFLG = ENVYN( APT_RESTART, VARDESC, RSTFLG, STATUS )
            IF ( STATUS /= 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS == 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            ELSE IF ( STATUS == -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS == -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

            DIAGFLG = .FALSE.         ! default
            VARDESC = 'Flag for saving APT Diagnostics'
            DIAGFLG = ENVYN( APT_DIAG, VARDESC, DIAGFLG, STATUS )
            IF ( STATUS /= 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS == 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            ELSE IF ( STATUS == -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS == -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

            VARDESC = 'Project Name'
            CALL ENVSTR( CTM_PROJNAME, VARDESC, PNAME, PROJNAME, STATUS )
            IF ( STATUS /= 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS == 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            ELSE IF ( STATUS == -1 ) THEN
               XMSG = 'Environment variable set, but empty ... no default:'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            ELSE IF ( STATUS == -2 ) THEN
               XMSG = 'Environment variable not set ... no default:'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            WRITE( LOGDEV, '( 5X, A)') 'Project ' // TRIM( PROJNAME )
            NAMEIN = PROJNAME

            RUNLEN = 480000         ! default
            VARDESC = 'Scenario Run Duration (HHMMSS)'
            RUNLEN = ENVINT( CTM_RUNLEN, VARDESC, RUNLEN, STATUS )
            IF ( STATUS /= 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS == 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            ELSE IF ( STATUS == -1 ) THEN
               MSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            ELSE IF ( STATUS == -2 ) THEN
               MSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
            END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the diagnostics files if needed
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( DIAGFLG ) THEN

C Set diagnostic file names

               DGNFNAME( 1 ) = EMIS_DGN
               DGNFNAME( 2 ) = STATICS_DGN
               DGNFNAME( 3 ) = BDRY_DGN
               DGNFNAME( 4 ) = DDEPOS_DGN
               DGNFNAME( 5 ) = WDEPOS_DGN
               DGNFNAME( 6 ) = CHEM_DGN
               DGNFNAME( 7 ) = ACTIVE_DGN
               DGNFNAME( 8 ) = DUMP_DGN

C Set output file characteristics based on COORD.EXT

               FTYPE3D = GRDDED3
               TSTEP3D = TSTEP( 1 )
               NVARS3D = N_GC_SPC + N_AE_SPC + N_NR_SPC + N_TR_SPC + 1
               NCOLS3D = 1
               NROWS3D = 1
               NLAYS3D = 1
               NTHIK3D = 1
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

               SDATE3D = JDATE
               STIME3D = JTIME
               CALL NEXTIME( SDATE3D, STIME3D, TSTEP( 1 ) )

               DO NFL = 1, NUMFLS

                  VARDESC = 'Diagnostics output file ' // DGNFNAME( NFL )
                  CALL ENVSTR( DGNFNAME( NFL ), VARDESC, PNAME, RET_VAL, STATUS )

                  IF ( STATUS /= 0 ) THEN

                     XMSG = 'Diagnostics output file ' // DGNFNAME( NFL ) //
     &                      ' not assigned'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )

                  ELSE

                     FDESC3D( 1 ) = 'APT Diagnostics Output File '
     &                               // DGNFNAME( NFL )
                     DO SPC = 2, MXDESC3
                        FDESC3D( SPC ) = ' '
                     END DO
                     V = 0
                     STRT = 1
                     FINI = N_GC_SPC
                     DO SPC = STRT, FINI
                        V = V + 1
                        VTYPE3D( SPC ) = M3REAL
                        VNAME3D( SPC ) = GC_SPC( V )
                        UNITS3D( SPC ) = 'ppmV'
                        VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
                     END DO

                     V = 0
                     STRT = FINI + 1     ! STRT = N_GC_SPC + 1
                     FINI = N_GC_SPC + N_AE_SPC
                     DO SPC = STRT, FINI
                        V = V + 1
                        VTYPE3D( SPC ) = M3REAL
                        VNAME3D( SPC ) = AE_SPC( V )
                        IF ( VNAME3D( SPC )(1:3) == 'NUM' ) THEN
                           UNITS3D( SPC ) = 'number/m**3'
                        ELSE IF ( VNAME3D( SPC )(1:3) == 'SRF' ) THEN
                           UNITS3D( SPC ) = 'm**2/m**3'
                        ELSE
                           UNITS3D( SPC ) = 'micrograms/m**3'
                        END IF
                        VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
                     END DO

                     V = 0
                     STRT = FINI + 1     ! STRT = N_GC_SPC + N_AE_SPC + 1
                     FINI = N_GC_SPC + N_AE_SPC + N_NR_SPC
                     DO SPC = STRT, FINI
                        V = V + 1
                        VTYPE3D( SPC ) = M3REAL
                        VNAME3D( SPC ) = NR_SPC( V )
                        UNITS3D( SPC ) = 'ppmV'
                        VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
                     END DO

                     V = 0
                     STRT = FINI + 1
                     FINI = N_GC_SPC + N_AE_SPC + N_NR_SPC + N_TR_SPC
                     DO SPC = STRT, FINI
                        V = V + 1
                        VTYPE3D( SPC ) = M3REAL
                        VNAME3D( SPC ) = TR_SPC( V )
                        UNITS3D( SPC ) = 'ppmV'
                        VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
                     END DO

C Last variable is SCICHEM tracer
                     VTYPE3D( NVARS3D ) = M3REAL
                     VNAME3D( NVARS3D ) = 'TRAC'
                     UNITS3D( NVARS3D ) = 'ppmV-m3'
                     VDESC3D( NVARS3D ) = 'Variable ' // VNAME3D( NVARS3D )

C Dump file contains extra field for no. of puffs dumped
                     IF ( NFL == NUMFLS ) THEN
                        NVARS3D = NVARS3D + 1
                        VTYPE3D( NVARS3D ) = M3INT
                        VNAME3D( NVARS3D ) = 'NDUMP'
                        UNITS3D( NVARS3D ) = ''
                        VDESC3D( NVARS3D ) = 'No. of puffs dumped '
                     END IF

                     IF ( OPEN3( DGNFNAME( NFL ), FSUNKN3, PNAME ) ) THEN
                        CALL M3MESG( 'Opened APT Diagnostics Output File '
     &                             //  DGNFNAME( NFL ) )
                     ELSE
                        CALL M3EXIT( PNAME, JDATE, JTIME,
     &                     'Could not open APT Diagnostics Output File ' 
     &                     // DGNFNAME( NFL ), XSTAT1 )
                     END IF

                  END IF

               END DO

            END IF    ! IF DIAGFLG

C Calculate number of output time steps for this model run:

            IF ( RUNLEN < 1000000 ) THEN
               TOTSECS  = TIME2SEC( RUNLEN )
            ELSE                          ! HH > 99
               RUNLEN = RUNLEN - 1000000
               TOTSECS  = TIME2SEC( RUNLEN )
               TOTSECS  = TOTSECS + 360000
            END IF
            STEPSECS = TIME2SEC( TSTEP( 1 )  )
            NSTEPS = TOTSECS / STEPSECS

            DTSAVE = FLOAT( STEPSECS )
            TENDHR = DTSAVE*NSTEPS/3600.

         END IF   ! IF MYPE = 0

#ifdef parallel
         CALL MPI_BCAST( DTSAVE, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERROR )
         CALL MPI_BCAST( NAMEIN, 40, MPI_CHARACTER, 0,
     &                   MPI_COMM_WORLD, IERROR )
#endif
         write(logdev,*)'mype ',mype,' calling init_scichem; dtsave = ',dtsave

#ifdef parallel
         ALLOCATE ( GCGRID( 1:GL_NCOLS,1:GL_NROWS,NLAYS,NSPCSD ),
     &              STAT = ALLOCSTAT )

         IF ( ALLOCSTAT /= 0 ) THEN
             XMSG = 'Failure allocating GCGRID'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         GCSIZE = GL_NCOLS * GL_NROWS * NLAYS * NSPCSD
         GCGRID = 0.0

!debug
         write(logdev,*)'gcsize: ',gcsize
!debug
         ALLOCATE ( GDEPV( N_SPC_DEPV+1, 1:GL_NCOLS,1:GL_NROWS ),
     &              STAT = ALLOCSTAT )

         IF ( ALLOCSTAT /= 0 ) THEN
             XMSG = 'Failure allocating GDEPV'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         GDSIZE = GL_NCOLS * GL_NROWS * (N_SPC_DEPV + 1)
         GDEPV = 0.0
!debug
         write(logdev,*)'gdsize: ',gdsize
!debug

         ALLOCATE ( GWVEL( 1:GL_NCOLS,1:GL_NROWS,NLAYS ),
     &              STAT = ALLOCSTAT )

         IF ( ALLOCSTAT /= 0 ) THEN
             XMSG = 'Failure allocating GWVEL'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         GWSIZE = GL_NCOLS * GL_NROWS * NLAYS
         GWVEL  = 0.0
!debug
         write(logdev,*)'gwsize: ',gwsize
!debug

C Get number of processors
         CALL MPI_COMM_SIZE( MPI_COMM_WORLD, NPROCS, IERROR )
         IF ( IERROR /= 0 ) THEN
            XMSG = 'Error in MPI_COMM_SIZE'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
!debug
      write(logdev,*)'NPROCS: ',NPROCS
!debug

         ALLOCATE ( NCOLS_PE( NPROCS ),
     &                 NROWS_PE( NPROCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT /= 0 ) THEN
             XMSG = 'Failure allocating NCOLS_PE or NROWS_PE'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         DO IPE = 1, NPROCS
            NCOLS_PE( IPE ) = COLSX_PE( 2,IPE ) - COLSX_PE( 1, IPE ) + 1
            NROWS_PE( IPE ) = ROWSX_PE( 2,IPE ) - ROWSX_PE( 1, IPE ) + 1
!debug
            write(logdev,*)'proc,ncols,nrows: ',ipe,ncols_pe(ipe),nrows_pe(ipe)
            write(logdev,*)'colsx_pe(1),colsx_pe(2): ',COLSX_PE( 1, IPE ),COLSX_PE( 2, IPE )
            write(logdev,*)'rowsx_pe(1),rowsx_pe(2): ',ROWSX_PE( 1, IPE ),ROWSX_PE( 2, IPE )
!debug
         END DO

!debug
      write(logdev,*)'calling init_scichem'
      write(logdev,*)'namein,rstflg,jdate,jtime,tstep(1),tendhr,dtsave: ',
     &           namein,rstflg,jdate,jtime,tstep(1),tendhr,dtsave
!debug
         CALL INIT_SCICHEM( NAMEIN,RSTFLG,JDATE,JTIME,TSTEP( 1 ),
     &                      TENDHR,DTSAVE,GCGRID )
#else
!debug
      write(logdev,*)'calling init_scichem'
      write(logdev,*)'namein,rstflg,jdate,jtime,tstep(1),tendhr,dtsave: ',
     &           namein,rstflg,jdate,jtime,tstep(1),tendhr,dtsave
!debug
         CALL INIT_SCICHEM( NAMEIN,RSTFLG,JDATE,JTIME,TSTEP( 1 ),
     &                      TENDHR,DTSAVE,CGRID )
#endif
         write(logdev,*)'processor ',mype,' finished init_scichem'
!debug
         call flush(logdev)
!debug

      END IF   ! IF FIRSTIME

#ifdef parallel

!debug
!      write(logdev,*)'gl_ncols,gl_nrows,nlays: ',gl_ncols,gl_nrows,nlays
!      call flush(logdev)
!debug
      IF ( .NOT. ALLOCATED ( GCGRID ) ) THEN

         ALLOCATE ( GCGRID( 1:GL_NCOLS,1:GL_NROWS,NLAYS,NSPCSD ),
     &              STAT = ALLOCSTAT )

         IF ( ALLOCSTAT /= 0 ) THEN
             XMSG = 'Failure allocating GCGRID'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         GCGRID = 0.0

      END IF

      IF ( MYPE == 0 ) THEN

! Main processor copies its own local CGRID, DEPV and WVEL arrays into
! global arrays

         C0 = COLSX_PE( 1,1 )
         R0 = ROWSX_PE( 1,1 )
         NC = NCOLS_PE( 1 )
         NR = NROWS_PE( 1 )

!debug
!         write(logdev,*)'processor = ',mype
!         write(logdev,*)'c0,r0,nc,nr: ',c0,r0,nc,nr
!         call flush(logdev)
!debug
! CGRID array
         DO SPC = 1, NSPCSD
            DO IL = 1, NLAYS
               DO IR = 1, NR
                  DO IC = 1, NC
                     GCGRID( C0+IC-1,R0+IR-1,IL,SPC ) = CGRID( IC,IR,IL,SPC )
                  END DO
               END DO
            END DO
         END DO

!debug
!       write(logdev,*)'finished assigning cgrid for processor ',mype
!       write(logdev,*)'gcgrid(15,40,1,4),cgrid(15,40,1,4): ',
!     &                 gcgrid(15,40,1,4),cgrid(15,40,1,4)
!       write(logdev,*)'gcgrid,cgrid(16,20,1,1): ',gcgrid(16,20,1,1),cgrid(16,20,1,1)
!       write(logdev,*)'gcgrid shape: ',shape(gcgrid) 
!       write(logdev,*)'cgrid shape: ',shape(cgrid) 
!       call flush(logdev)
!debug
! DEPV array
         DO IR = 1, NR
            DO IC = 1, NC
               DO SPC = 1, N_SPC_DEPV + 1
                  GDEPV( SPC,C0+IC-1,R0+IR-1 ) = DEPV( SPC,IC,IR )
               END DO
            END DO
         END DO

!debug
!       write(logdev,*)'finished assigning depv for processor ',mype
!       write(logdev,*)'gdepv shape: ',shape(gdepv) 
!       write(logdev,*)'depv shape: ',shape(depv) 
!D       DO SPC = 1, N_SPC_DEPV + 1
!D          write(logdev,*)'gdepv,depv(',spc,',15,40): ',
!D     &                    gdepv(spc,15,40),depv(spc,15,40)
!D          write(logdev,*)'gdepv,depv(',spc,',63,61): ',
!D     &               gdepv(spc,63,61),depv(spc,63,61)
!D       END DO
!       call flush(logdev)
!debug
! WVEL array
         DO IL = 1, NLAYS
            DO IR = 1, NR
               DO IC = 1, NC
                  GWVEL( C0+IC-1,R0+IR-1,IL ) = WVEL( IC,IR,IL )
               END DO
            END DO
         END DO
!debug
!       write(logdev,*)'finished assigning wvel for processor ',mype
!       write(logdev,*)'gwvel,wvel(15,40,2): ',gwvel(15,40,2),wvel(15,40,2)
!       write(logdev,*)'gwvel shape: ',shape(gwvel) 
!       write(logdev,*)'wvel shape: ',shape(wvel) 
!       call flush(logdev)
!debug
  
! Main processor receives local CGRID, DEPV and WVEL arrays from slave
! processors and copies them to global arrays
!debug
!       write(logdev,*)'nprocs: ',nprocs
!debug
         DO IPE = 1, NPROCS - 1

            CALL MPI_RECV( WHO, 1, MPI_INTEGER, MPI_ANY_SOURCE, 
     &                     TAG1, MPI_COMM_WORLD, STATUS_MPI, IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'MPI error receiving processor id WHO.'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            C0 = COLSX_PE( 1,WHO+1 )
            R0 = ROWSX_PE( 1,WHO+1 )
            NC = NCOLS_PE( WHO+1 )
            NR = NROWS_PE( WHO+1 )
!debug
!        write(logdev,*)'received id from processor ',who
!        write(logdev,*)'c0,r0,nc,nr: ',c0,r0,nc,nr
!        call flush(logdev)
!debug 
! Gather CGRIDs and DEPVs from slave processors to main processor global
! arrays
! Get size of local CGRID array for this processor
            RCSIZE  = NC * NR * NLAYS * NSPCSD
! Get size of local DEPV array for this processor
            RVDSIZE = NC * NR * ( N_SPC_DEPV + 1 )
! Get size of local WVEL array for this processor
            RWVSIZE = NC * NR * NLAYS

!      RSIZE = MAX( RCSIZE, RVDSIZE, RWVSIZE )

!debug
!      write(logdev,*)'who,rcsize,rvdsize,rwvsize: ',
!     &                who,rcsize,rvdsize,rwvsize
!      call flush(logdev)
!debug
            IF ( ALLOCATED ( RECVBUFC ) ) DEALLOCATE ( RECVBUFC )
            ALLOCATE ( RECVBUFC  ( RCSIZE ), STAT = IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'Failure allocating RECVBUFC'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            RECVBUFC = 0.

            IF ( ALLOCATED ( RECVBUFD ) ) DEALLOCATE ( RECVBUFD )
            ALLOCATE ( RECVBUFD  ( RVDSIZE ), STAT = IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'Failure allocating RECVBUFD'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            RECVBUFD = 0.

            IF ( ALLOCATED ( RECVBUFW ) ) DEALLOCATE ( RECVBUFW )
            ALLOCATE ( RECVBUFW  ( RWVSIZE ), STAT = IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'Failure allocating RECVBUFW'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            RECVBUFW = 0.

! CGRID array
            CALL MPI_RECV( RECVBUFC, RCSIZE, MPI_REAL, WHO,
     &                     TAG2, MPI_COMM_WORLD, STATUS_MPI, IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'MPI error receiving data array RECVBUF (CGRID).'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
!debug
!        write(logdev,*)'received cgrid from processor ',who
!        call flush(logdev)
!debug 
! DEPV array
            CALL MPI_RECV( RECVBUFD, RVDSIZE, MPI_REAL, WHO,
     &                     TAG3, MPI_COMM_WORLD, STATUS_MPI, IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'MPI error receiving data array RECVBUF (DEPV).'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
!debug
!        write(logdev,*)'received depv from processor ',who
!        call flush(logdev)
!debug 
! WVEL array
            CALL MPI_RECV( RECVBUFW, RWVSIZE, MPI_REAL, WHO,
     &                     TAG4, MPI_COMM_WORLD, STATUS_MPI, IERROR )
            IF ( IERROR /= 0 ) THEN
               XMSG = 'MPI error receiving data array RECVBUF (WVEL).'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
!debug
!        write(logdev,*)'received wvel from processor ',who
!        call flush(logdev)
!debug 

            LOC = 0
            DO SPC = 1, NSPCSD
               DO IL = 1, NLAYS
                  DO IR = 1, NR
                     DO IC = 1, NC
                        LOC = LOC + 1
                        GCGRID( C0+IC-1,R0+IR-1,IL,SPC ) = RECVBUFC( LOC )
                     END DO
                  END DO
               END DO
            END DO
!debug
!        write(logdev,*)'assigned cgrid from processor ',who
!        call flush(logdev)
!debug 

            LOC = 0
            DO IR = 1, NR
               DO IC = 1, NC
                  DO SPC = 1, N_SPC_DEPV + 1
                     LOC = LOC + 1
                     GDEPV( SPC,C0+IC-1,R0+IR-1 ) = RECVBUFD( LOC )
                  END DO
               END DO
            END DO
!debug
!        write(logdev,*)'assigned depv from processor ',who
!        call flush(logdev)
!debug 

            LOC = 0
            DO IL = 1, NLAYS
               DO IR = 1, NR
                  DO IC = 1, NC
                     LOC = LOC + 1
                     GWVEL( C0+IC-1,R0+IR-1,IL ) = RECVBUFW( LOC )
                  END DO
               END DO
            END DO
!debug
!        write(logdev,*)'assigned wvel from processor ',who
!        call flush(logdev)

!D        SELECT CASE(who)
!D          CASE(1)
!D            write(logdev,*)'gcgrid(45,40,1,4): ',gcgrid(45,40,1,4)
!D            write(logdev,*)'gdepv(1,45,40): ',gdepv(1,45,40)
!D            write(logdev,*)'gwvel(45,40,2): ',gwvel(45,40,2)
!D          CASE(2)
!D            write(logdev,*)'gcgrid(75,40,1,4): ',gcgrid(75,40,1,4)
!D            write(logdev,*)'gdepv(1,75,40): ',gdepv(1,75,40)
!D            write(logdev,*)'gwvel(75,40,2): ',gwvel(75,40,2)
!D          CASE(3)
!D            write(logdev,*)'gcgrid(105,40,1,4): ',gcgrid(105,40,1,4)
!D            write(logdev,*)'gdepv(1,105,40): ',gdepv(1,105,40)
!D            write(logdev,*)'gwvel(105,40,2): ',gwvel(105,40,2)
!D          CASE(4)
!D            write(logdev,*)'gcgrid(15,119,1,4): ',gcgrid(15,119,1,4)
!D            write(logdev,*)'gdepv(1,15,119): ',gdepv(1,15,119)
!D            write(logdev,*)'gwvel(15,119,2): ',gwvel(15,119,2)
!D          CASE(5)
!D            write(logdev,*)'gcgrid(45,119,1,4): ',gcgrid(45,119,1,4)
!D            write(logdev,*)'gdepv(1,45,119): ',gdepv(1,45,119)
!D            write(logdev,*)'gwvel(45,119,2): ',gwvel(45,119,2)
!D          CASE(6)
!D            write(logdev,*)'gcgrid(75,119,1,4): ',gcgrid(75,119,1,4)
!D            write(logdev,*)'gdepv(1,75,119): ',gdepv(1,75,119)
!D            write(logdev,*)'gwvel(75,119,2): ',gwvel(75,119,2)
!D          CASE(7)
!D            write(logdev,*)'gcgrid(105,119,1,4): ',gcgrid(105,119,1,4)
!D            write(logdev,*)'gdepv(1,105,119): ',gdepv(1,105,119)
!D            write(logdev,*)'gwvel(105,119,2): ',gwvel(105,119,2)
!!          CASE(8)
!!            write(logdev,*)'gcgrid(16,102,1,1): ',gcgrid(16,102,1,1)
!!            write(logdev,*)'gdepv(1,16,102): ',gdepv(1,16,102)
!!            write(logdev,*)'gwvel(16,102,2): ',gwvel(16,102,2)
!!          CASE(9)
!!            write(logdev,*)'gcgrid(48,102,1,1): ',gcgrid(48,102,1,1)
!!            write(logdev,*)'gdepv(1,48,102): ',gdepv(1,48,102)
!!            write(logdev,*)'gwvel(48,102,2): ',gwvel(48,102,2)
!!          CASE(10)
!!            write(logdev,*)'gcgrid(80,102,1,1): ',gcgrid(80,102,1,1)
!!            write(logdev,*)'gdepv(1,80,102): ',gdepv(1,80,102)
!!            write(logdev,*)'gwvel(80,102,2): ',gwvel(80,102,2)
!!          CASE(11)
!!            write(logdev,*)'gcgrid(112,102,1,1): ',gcgrid(112,102,1,1)
!!            write(logdev,*)'gdepv(1,112,102): ',gdepv(1,112,102)
!!            write(logdev,*)'gwvel(112,102,2): ',gwvel(112,102,2)
!D       END SELECT
!debug 

         END DO   ! End loop on slave processors

      ELSE      ! Slave processors send data

C Each slave processor sends its local array to the main processor.

         WHO = MYPE
!debug
!       write(logdev,*)'who,gcsize,gdsize,gwsize: ',who,gcsize,gdsize,gwsize
!       call flush(logdev)
!debug

         CALL MPI_SEND( WHO, 1, MPI_INTEGER, 0, TAG1,
     &                  MPI_COMM_WORLD, IERROR )

         IF ( IERROR /= 0 ) THEN
            XMSG = 'MPI error sending processor id WHO.'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
!debug
!        write(logdev,*)'sent id from processor ',who
!        call flush(logdev)
!debug 

! Send CGRID array
         MSGSIZE = NCOLS * NROWS * NLAYS * NSPCSD
         CALL MPI_SEND( CGRID, MSGSIZE, MPI_REAL, 0, TAG2,
     &                  MPI_COMM_WORLD, IERROR )

         IF ( IERROR /= 0 ) THEN
            XMSG = 'MPI error sending data array CGRID'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
!debug
!        write(logdev,*)'sent cgrid from processor ',who
!        write(logdev,*)'shape(cgrid): ',shape(cgrid)
!        write(logdev,*)'cgrid(15,40,1,4): ',cgrid(15,40,1,4)
!        call flush(logdev)
!debug 

! Send DEPV array
         MSGSIZE = NCOLS * NROWS * ( N_SPC_DEPV + 1 )
         CALL MPI_SEND( DEPV, MSGSIZE, MPI_REAL, 0, TAG3,
     &                  MPI_COMM_WORLD, IERROR )

         IF ( IERROR /= 0 ) THEN
            XMSG = 'MPI error sending data array DEPV'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
!debug
!        write(logdev,*)' sent depv from processor ',who
!        write(logdev,*)'shape(depv): ',shape(depv)
!        write(logdev,*)'depv(1,15,40): ',depv(1,15,40)
!        call flush(logdev)
!debug 

! Send WVEL array
         MSGSIZE = NCOLS * NROWS * NLAYS
         CALL MPI_SEND( WVEL, MSGSIZE, MPI_REAL, 0, TAG4,
     &                  MPI_COMM_WORLD, IERROR )

         IF ( IERROR /= 0 ) THEN
            XMSG = 'MPI error sending data array WVEL'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
!debug
!        write(logdev,*)'sent wvel from processor ',who
!        write(logdev,*)'shape(wvel): ',shape(wvel)
!        write(logdev,*)'wvel(15,40,2): ',wvel(15,40,2)
!        call flush(logdev)
!debug 

      END IF
! Propagate global arrays through all processors
!debug
!       write(logdev,*)'gcsize,gdsize,gwsize: ',gcsize,gdsize,gwsize
!debug
      CALL SUBST_BARRIER
      CALL MPI_BCAST( GCGRID, GCSIZE, MPI_REAL, 0, MPI_COMM_WORLD, IERROR )
      CALL MPI_BCAST( GDEPV, GDSIZE, MPI_REAL, 0, MPI_COMM_WORLD, IERROR )
      CALL MPI_BCAST( GWVEL, GWSIZE, MPI_REAL, 0, MPI_COMM_WORLD, IERROR )
#endif

      TMSTEP = FLOAT( TIME2SEC( TSTEP( 2 ) ) )

#ifdef parallel
!debug
!      IF ( MYPE == 0 ) THEN
!        write(logdev,*)'cgrid,wvel: ',gcgrid(63,61,1,1),gwvel(63,61,2)
!        DO SPC = 1, N_SPC_DEPV + 1
!           write(logdev,*)'gdepv(',spc,',63,61): ',gdepv(spc,63,61)
!        END DO
!      END IF
!      write(logdev,*)'jdate,jtime: ',jdate,jtime
!      write(logdev,*)'tstep: ',tstep
!      write(logdev,*)'tmstep,dtsave,nsteps,diagflg: ',
!     &           tmstep,dtsave,nsteps,diagflg 
!debug

      CALL SUBST_BARRIER
      write(logdev,*)'mype ',mype,' calling step_pig'
      call flush(logdev)
      CALL STEP_PIG( GCGRID, GDEPV, GWVEL, JDATE, JTIME, TSTEP,
     &               TMSTEP, DTSAVE, NSTEPS, DIAGFLG )

!debug
      write(logdev,*)'mype ',mype,' finished step_pig'
      write(*,*)'mype ',mype,' finished step_pig'
!      write(logdev,*)'gcgrid(15,40,1,4): ',gcgrid(15,40,1,4)
!      write(logdev,*)'gcgrid(45,40,1,4): ',gcgrid(45,40,1,4)
!      write(logdev,*)'gcgrid(75,40,1,4): ',gcgrid(75,40,1,4)
!      write(logdev,*)'gcgrid(105,40,1,4): ',gcgrid(105,40,1,4)
!      write(logdev,*)'gcgrid(15,119,1,4): ',gcgrid(15,119,1,4)
!      write(logdev,*)'gcgrid(45,119,1,4): ',gcgrid(45,119,1,4)
!      write(logdev,*)'gcgrid(75,119,1,4): ',gcgrid(75,119,1,4)
!      write(logdev,*)'gcgrid(105,119,1,4): ',gcgrid(105,119,1,4)
      call flush(logdev)
      call flush(6)
!debug
! Scatter global CGRID array to processor CGRID sub-arrays
! First send GCGRID array to all processors
!debug
      write(*,*)'mype ',mype,' calling mpi_barrier'
      call flush(logdev)
      call flush(6)
!debug
      CALL SUBST_BARRIER
!debug
      write(*,*)'mype ',mype,' finished calling mpi_barrier'
      call flush(logdev)
      call flush(6)
!debug

!debug
      write(*,*)'mype ',mype,' calling mpi_bcast'
      call flush(logdev)
      call flush(6)
!debug
      CALL MPI_BCAST( GCGRID, GCSIZE, MPI_REAL, 0, MPI_COMM_WORLD, IERROR )
!debug
      write(*,*)'mype ',mype,' finished calling mpi_bcast'
      call flush(logdev)
      call flush(6)
!debug

      C0 = COLSX_PE( 1,MYPE+1 )
      R0 = ROWSX_PE( 1,MYPE+1 )
      NC = NCOLS_PE( MYPE+1 )
      NR = NROWS_PE( MYPE+1 )
!debug
      write(*,*)'mype ',mype,' updating cgrid'
      write(*,*)'mype, c0,r0,nc,nr ',mype,c0,r0,nc,nr
      write(logdev,*)'mype, c0,r0,nc,nr ',mype,c0,r0,nc,nr
      write(logdev,*)'nspcsd,nlays ',nspcsd,nlays
      call flush(logdev)
      call flush(6)
!debug

! Update CGRID sub-arrays for all processors
      DO SPC = 1, NSPCSD
         DO IL = 1, NLAYS
            DO IR = 1, NR
               DO IC = 1, NC
                  CGRID( IC,IR,IL,SPC ) = GCGRID( C0+IC-1,R0+IR-1,IL,SPC )
               END DO
            END DO
         END DO
      END DO
!debug
      write(*,*)'mype ',mype,' finished updating cgrid'
      write(logdev,*)'mype ',mype,' finished updating cgrid'
      call flush(logdev)
      call flush(6)
!debug

      DEALLOCATE ( GCGRID )
      CALL SUBST_BARRIER

#else

!debug
      write(logdev,*)'mype ',mype,' calling step_pig'
      call flush(logdev)
!debug
      CALL STEP_PIG( CGRID, DEPV, WVEL, JDATE, JTIME, TSTEP,
     &               TMSTEP, DTSAVE, NSTEPS, DIAGFLG )
!debug
      write(logdev,*)'mype ',mype,' finished step_pig'
      call flush(logdev)
!debug

#endif

      RETURN
      END
