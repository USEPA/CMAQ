
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
C $Header: /project/yoj/arc/CCTM/src/init/yamo/initscen.F,v 1.7 2011/10/21 16:11:24 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INITSCEN ( CGRID, STDATE, STTIME, TSTEP, NSTEPS )

C-----------------------------------------------------------------------
C Function:
C   Initialize simulation time period and time stepping constants for
C   core model driver
C   Environment variable can reference a previous CONC file to use as
C   initial data.
C   Write initial conc data as step "0" on output conc file
 
C Preconditions:
 
C Subroutines and functions called:
C   INIT3, M3EXIT, OPEN3, CLOSE3, DESC3, ENVINT, TIME2SEC, HHMMSS
 
C Revision history:
C   prototype 6/92 by CJC for "concept model"
C   revised   2/93 by CJC for initial M3 Prototype Alpha
C   revised   6/14/94 by Dongming Hwang for configuration management prototype
C   revised   8/30/94 by Dongming Hwang: If CTM_CONC_1 file doesn't exist,
C             use header info from CHEMINIT3D file.
 
C Interim Version ... (Jeff)

C   revised  6/12/97 by Jerry Gipson: Get ICs by species name, by surrogate
C            name, or zero
C   Jeff - Aug 97 - fixed problems, cleaned up
C   Jeff - Dec 97 - add CMIN
C   Jeff - Dec 97 - put in aerosol sulfate inititalization
C   Jeff - Feb 98 - close init cond files after reading
C   2 October, 1998 by Al Bourgeois at LM: parallel implementation
C   revised 10/7/99 by Shawn Roselle: added surface area species to
C            aerosol species types
C   Jeff - Dec 00 - check if append, split out opconc and load_cgrid
C                 - move CGRID_MAP into f90 module
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; assumed shape arrays
C   17 Mar 03 D.Wong: move barrier to avoid race conditions
C   28 Aug 03 J.Young: following Zion Wang at CERT, only pe 0 closes
C   30 May 05 J.Young: add call to load RHOJ into CGRID
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   Aug 15 D.Wong:    Added two new subroutine calls to open ACONC and WDEP files
C   Dec 15 D.Wong: new routine wr_init to write the initial data to the CONC file
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

!     REAL      :: CGRID( :,:,:,: )  ! for initial CONC
      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC
      INTEGER      STDATE       ! starting date,    format YYYYDDD
      INTEGER      STTIME       ! starting time,    format HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency
      INTEGER      NSTEPS       ! run duration, as number of output time steps

C Parameters:

      REAL, PARAMETER :: CMIN = 1.0E-30

C Local Variables

C  environment variable start date
      CHARACTER( 16 ) :: CTM_STDATE = 'CTM_STDATE'
C  environment variable start time
      CHARACTER( 16 ) :: CTM_STTIME = 'CTM_STTIME'
C  environment variable run duration
      CHARACTER( 16 ) :: CTM_RUNLEN = 'CTM_RUNLEN'
C  environment variable output time step
      CHARACTER( 16 ) :: CTM_TSTEP = 'CTM_TSTEP'
C driver program name
      CHARACTER( 16 ) :: CTM_PROGNAME = 'CTM_PROGNAME'
C  environment variable description
      CHARACTER( 80 ) :: VARDESC

      CHARACTER( 16 ) :: PNAME = 'INITSCEN'
      CHARACTER( 16 ) :: FNAME
      CHARACTER( 16 ) :: PROGNAME
      CHARACTER( 96 ) :: MSG = ' '

      INTEGER      RUNLEN            ! run duration, HHMMSS
      INTEGER      STEPSECS          ! seconds per time step
      INTEGER      TOTSECS           ! run duration seconds
      INTEGER      LOGDEV            ! FORTRAN unit number for log file
      INTEGER      STATUS            !  ENVINT status

#ifdef Verbose3
      real sumcg
      integer c, r, l, s, v
#endif

      INTERFACE
         SUBROUTINE LOAD_CGRID ( FNAME, STDATE, STTIME, SPC_CAT, CMIN, CGRID )
            CHARACTER( 16 ), INTENT( IN ) :: FNAME
            INTEGER, INTENT( IN )         :: STDATE, STTIME
            CHARACTER(  2 ), INTENT( IN ) :: SPC_CAT
            REAL,    INTENT( IN )         :: CMIN
            REAL, POINTER                 :: CGRID( :,:,:,: )
         END SUBROUTINE LOAD_CGRID
         SUBROUTINE WR_INIT ( CGRID, STDATE, STTIME, TSTEP )
            REAL, POINTER                 :: CGRID(:,:,:,:)
            INTEGER, INTENT( IN )         :: STDATE, STTIME, TSTEP
         END SUBROUTINE WR_INIT
      END INTERFACE

C-----------------------------------------------------------------------

      LOGDEV = INIT3()

      TSTEP( 1 )  = 010000         ! default
      VARDESC = 'Scenario Output Time Step (HHMMSS)'
      TSTEP( 1 ) = ENVINT( CTM_TSTEP, VARDESC, TSTEP( 1 ), STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         MSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      ELSE IF ( STATUS .EQ. -2 ) THEN
         MSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      END IF

#ifndef twoway
      VARDESC = 'Main Program Name'
      CALL ENVSTR( CTM_PROGNAME, VARDESC, 'DRIVER', PROGNAME, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         MSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      ELSE IF ( STATUS .EQ. -2 ) THEN
         MSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      END IF

      STDATE = 1995192        ! default
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      STDATE = ENVINT( CTM_STDATE, VARDESC, STDATE, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         MSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      ELSE IF ( STATUS .EQ. -2 ) THEN
         MSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      END IF

      STTIME = 000000         ! default
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      STTIME = ENVINT( CTM_STTIME, VARDESC, STTIME, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         MSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      ELSE IF ( STATUS .EQ. -2 ) THEN
         MSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      END IF

      RUNLEN = 480000         ! default
      VARDESC = 'Scenario Run Duration (HHMMSS)'
      RUNLEN = ENVINT( CTM_RUNLEN, VARDESC, RUNLEN, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         MSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      ELSE IF ( STATUS .EQ. -2 ) THEN
         MSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
      END IF

C Calculate number of output time steps for this model run:
      IF ( RUNLEN .LT. 1000000 ) THEN
         TOTSECS  = TIME2SEC( RUNLEN )
      ELSE
         RUNLEN = RUNLEN - 1000000
         TOTSECS  = TIME2SEC( RUNLEN )
         TOTSECS  = TOTSECS + 360000
      END IF
      STEPSECS = TIME2SEC( TSTEP( 1 )  )

      IF ( MOD( TOTSECS, STEPSECS ) .EQ. 0 ) THEN
         NSTEPS = TOTSECS / STEPSECS
      ELSE
         MSG = 'Output time step ' // HHMMSS( TSTEP( 1 ) ) //
     &         ' does not divide duration ' // HHMMSS( RUNLEN )
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT1 )
      END IF
#endif

C Initialize the CGRID array
      CGRID = CMIN

C Load CGRID
      IF ( N_GC_SPC .GT. 0 ) THEN
         FNAME = INIT_GASC_1
         CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'GC', CMIN, CGRID )
#ifdef Verbose3
         write( logdev,'(/"@@@c ", i8, i7.6)') stdate, sttime
         l = 1
         do v = gc_strt, gc_fini
            s = v + 1 - gc_strt
            sumcg = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumcg = sumcg + cgrid( c,r,l,v )
               end do
            end do
            write( logdev,'("@@@c ", a, i5, g16.6)') gc_spc( s ), s, sumcg
         end do
#endif
      END IF  ! N_GC_SPC > 0

C load rhoj
      FNAME = MET_CRO_3D
      CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'RJ', CMIN, CGRID )

C Get aerosols IC's. Note: INIT_AERO_1 may be setenv'ed to INIT_GASC_1
      IF ( N_AE_SPC .GT. 0 ) THEN
         FNAME = INIT_AERO_1
         CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'AE', CMIN, CGRID )
#ifdef Verbose3
         write( logdev,'(/"@@@c ", i8, i7.6)') stdate, sttime
         l = 1
         do v = ae_strt, ae_fini
            s = v + 1 - ae_strt
            sumcg = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumcg = sumcg + cgrid( c,r,l,v )
               end do
            end do
            write( logdev,'("@@@c ", a, i5, g16.6)') ae_spc( s ), s, sumcg
         end do
#endif
      END IF  ! N_AE_SPC > 0

C Get non-reactives IC's. Note: INIT_NONR_1 may be setenv'ed to INIT_GASC_1
      IF ( N_NR_SPC .GT. 0 ) THEN
         FNAME = INIT_NONR_1
         CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'NR', CMIN, CGRID )
#ifdef Verbose3
         write( logdev,'(/"@@@c ", i8, i7.6)') stdate, sttime
         l = 1
         do v = nr_strt, nr_fini
            s = v + 1 - nr_strt
            sumcg = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumcg = sumcg + cgrid( c,r,l,v )
               end do
            end do
            write( logdev,'("@@@c ", a, i5, g16.6)') nr_spc( s ), s, sumcg
         end do
#endif
      END IF  ! N_NR_SPC > 0

C Get tracer IC's. Note: INIT_TRAC_1 may be setenv'ed to INIT_GASC_1
      IF ( N_TR_SPC .GT. 0 ) THEN
         FNAME = INIT_TRAC_1
         CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'TR', CMIN, CGRID )
#ifdef Verbose3
         write( logdev,'(/"@@@c ", i8, i7.6)') stdate, sttime
         l = 1
         do v = tr_strt, tr_fini
            s = v + 1 - tr_strt
            sumcg = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumcg = sumcg + cgrid( c,r,l,v )
               end do
            end do
            write( logdev,'("@@@c ", a, i5, g16.6)') tr_spc( s ), s, sumcg
         end do
#endif
      END IF  ! N_TR_SPC > 0

C Test opening existing conc file for update
      IF ( .NOT. OPEN3( CTM_CONC_1, FSRDWR3, PNAME ) ) THEN
         MSG = 'Could not open ' // TRIM( CTM_CONC_1 ) // ' for update - '
     &        // 'try to open new'
         CALL M3MESG( MSG )
         ! Open the file new
         CALL SUBST_BARRIER
         CALL OPCONC ( STDATE, STTIME, TSTEP( 1 ) )
      END IF

      CALL WR_INIT ( CGRID, STDATE, STTIME, TSTEP( 1 ) )
      CALL OPACONC ( STDATE, STTIME, TSTEP( 1 ) )
      CALL OPWDEP  ( STDATE, STTIME, TSTEP( 1 ) )

      CALL SUBST_BARRIER

      RETURN
      END
