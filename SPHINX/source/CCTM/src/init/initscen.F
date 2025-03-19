
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
      SUBROUTINE INITSCEN ( CGRID, TSTEP)

C-----------------------------------------------------------------------
C Function:
C   Initialize simulation time period and time stepping constants for
C   core model driver
C   Environment variable can reference a previous CONC file to use as
C   initial data.
C   Write initial conc data as step "0" on output conc file
 
C Preconditions:
 
C Subroutines and functions called:
C   M3EXIT, OPEN3, CLOSE3, DESC3, TIME2SEC, HHMMSS
 
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
C   1 Feb 19 David Wong: Implemented centralized I/O approach, removed
C                        all MY_N clauses
C   29 Oct 18 L.Zhou, S.Napelenok: isam implementation
C   26 Nov 19 S. Napelenok: DDM3D implementation for CMAQ v5.3.1
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE HGRD_DEFN             ! horizontal domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE ELMO_DATA, ONLY : L_ELMO, ELMO_NSTEP 
      USE ELMO_PROC, ONLY : OPEN_ELMO
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
      USE VERTEXT_MODULE
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

#ifdef isam
      USE SA_DEFN
#endif

#ifdef sens
      USE DDM3D_DEFN
      USE VGRD_DEFN, ONLY: NLAYS
#endif

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

!     REAL      :: CGRID( :,:,:,: )  ! for initial CONC
      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency
!     INTEGER      NSTEPS       ! run duration, as number of output time steps

C Parameters:

      REAL, PARAMETER :: CMIN = 1.0E-30

C Local Variables
      CHARACTER( 16 ) :: PNAME = 'INITSCEN'
      CHARACTER( 16 ) :: FNAME
      CHARACTER( 96 ) :: MSG = ' '

      INTEGER      STEPSECS          ! seconds per time step
      INTEGER      TOTSECS           ! run duration seconds

#ifdef Verbose3
      real sumcg
      integer c, r, l, s, v
#endif

#ifdef sens
      integer COL,ROW,LAY,SPC
#endif

      INTERFACE
         SUBROUTINE LOAD_CGRID ( FNAME, SPC_CAT, CMIN, CGRID )
            CHARACTER( 16 ), INTENT( IN ) :: FNAME
            CHARACTER(  2 ), INTENT( IN ) :: SPC_CAT
            REAL,    INTENT( IN )         :: CMIN
            REAL, POINTER                 :: CGRID( :,:,:,: )
         END SUBROUTINE LOAD_CGRID
         SUBROUTINE WR_INIT ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER                 :: CGRID(:,:,:,:)
            INTEGER, INTENT( IN )         :: JDATE
            INTEGER, INTENT( IN )         :: JTIME
            INTEGER, INTENT( IN )         :: TSTEP
         END SUBROUTINE WR_INIT
#ifdef isam
         SUBROUTINE SA_ARRAY_INIT ( CGRID )
            IMPLICIT NONE
            REAL, POINTER                 :: CGRID( :,:,:,: )
         END SUBROUTINE SA_ARRAY_INIT
         SUBROUTINE OP_SA ( JDATE, JTIME, TSTEP, NSTEPS, FNAME )
            IMPLICIT NONE
            INTEGER :: JDATE, JTIME
            INTEGER :: TSTEP(2)
            INTEGER :: NSTEPS
            CHARACTER( 16 ) :: FNAME
         END SUBROUTINE OP_SA
         SUBROUTINE SA_OPWDDEP ( SADEPFILE, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            CHARACTER( 16 ) :: SADEPFILE ! Dry or wet dep file for tags
            INTEGER         :: JDATE, JTIME, TSTEP
         END SUBROUTINE SA_OPWDDEP
#endif
#ifdef sens
         SUBROUTINE LOAD_SENGRID ( FNAME, STDATE, STTIME, SPC_CAT )
            IMPLICIT NONE
            CHARACTER( 16 ), INTENT( IN ) :: FNAME
            INTEGER, INTENT( IN )         :: STDATE, STTIME
            CHARACTER(  2 ), INTENT( IN ) :: SPC_CAT
         END SUBROUTINE LOAD_SENGRID
#endif
      END INTERFACE

C-----------------------------------------------------------------------

      TSTEP( 1 ) = LOCAL_TSTEP 

#ifdef isam
      IF ( MYPE .EQ. 0 ) THEN
         WRITE( LOGDEV,* ) 'Entered INITSCEN...'
         WRITE( LOGDEV,* ) 'NSPC_SA, NTAG_SA:', NSPC_SA, NTAG_SA
      END IF
#endif

C Initialize the CGRID array
      CGRID = CMIN

C Load CGRID
      IF ( N_GC_SPC .GT. 0 ) THEN
         FNAME = INIT_CONC_1
         CALL LOAD_CGRID ( FNAME, 'GC', CMIN, CGRID )
#ifdef Verbose3
         write( logdev,'(/"@@@c ", i8, i7.6)') stdate, sttime
         l = 1
         do v = gc_strt, gc_fini
            s = v + 1 - gc_strt
            sumcg = 0.0
            do r = 1, nrows
               do c = 1, ncols
                  sumcg = sumcg + cgrid( c,r,l,v )
               end do
            end do
            write( logdev,'("@@@c ", a, i5, g16.6)') gc_spc( s ), s, sumcg
         end do
#endif
      END IF  ! N_GC_SPC > 0

C load rhoj
      FNAME = MET_CRO_3D
      CALL LOAD_CGRID ( FNAME, 'RJ', CMIN, CGRID )

C Get aerosols IC's
      IF ( N_AE_SPC .GT. 0 ) THEN
         FNAME = INIT_CONC_1
         CALL LOAD_CGRID ( FNAME, 'AE', CMIN, CGRID )
      END IF  ! N_AE_SPC > 0

C Get non-reactives IC's
      IF ( N_NR_SPC .GT. 0 ) THEN
         FNAME = INIT_CONC_1
         CALL LOAD_CGRID ( FNAME, 'NR', CMIN, CGRID )
      END IF  ! N_NR_SPC > 0

C Get tracer IC's
      IF ( N_TR_SPC .GT. 0 ) THEN
         FNAME = INIT_CONC_1
         CALL LOAD_CGRID ( FNAME, 'TR', CMIN, CGRID )
      END IF  ! N_TR_SPC > 0

C Test opening existing conc file for update
      CALL LOG_HEADING( LOGDEV, "Open or Create Concentration File" )

C *** Open the aerosol parameters file (diameters and standard deviations).
      IF ( L_ELMO .AND. IO_PE_INCLUSIVE ) THEN
           CALL OPEN_ELMO ( STDATE, STTIME, TSTEP( 1 ) )
           ELMO_NSTEP = 0.0
      END IF

#ifndef mpas
      IF ( .NOT. OPEN3( CTM_CONC_1, FSRDWR3, PNAME ) ) THEN

#ifndef twoway
         BACKSPACE( LOGDEV )
#endif
         MSG = 'Could not open ' // TRIM( CTM_CONC_1 ) // ' for update - '
     &        // 'try to open new'
         CALL M3MESG( MSG )
         ! Open the file new
         CALL SUBST_BARRIER
         CALL OPCONC ( STDATE, STTIME, TSTEP( 1 ) )
#ifdef parallel
         CALL OPVEXT ( STDATE, STTIME, TSTEP( 1 ) )
#endif
      END IF

      CALL WR_INIT ( CGRID, STDATE, STTIME, TSTEP( 1 ) )
      
      CALL LOG_HEADING( LOGDEV, "Open or Create Average Concentration File" )
      CALL OPACONC ( STDATE, STTIME, TSTEP( 1 ) )
      
      CALL LOG_HEADING( LOGDEV, "Open or Create Wet Deposition File" )
      CALL OPWDEP  ( STDATE, STTIME, TSTEP( 1 ) )

#ifdef sens
      CALL OPASENS( STDATE, STTIME, TSTEP( 1 ) )
#endif

#ifdef isam
C Initialize ISAM array
      CALL SA_ARRAY_INIT ( CGRID )

C Open SA_CONC_1 and SA_CGRID_1
      CALL OP_SA ( STDATE, STTIME, TSTEP, 1, SA_CONC_1 )
      IF ( MYPE .EQ. 0 ) CALL SA_OPWDDEP ( SA_WD_1, STDATE, STTIME, TSTEP( 1 ) )
#endif

#ifdef sens
      IF ( RST ) THEN ! Load SENGRID from a file

         IF ( N_GC_SPC .GT. 0 ) THEN
            FNAME = INIT_SENS_1
            CALL LOAD_SENGRID ( FNAME, STDATE, STTIME, 'GC' )
         END IF  ! N_GC_SPC > 0

         FNAME = MET_CRO_3D
         CALL LOAD_SENGRID ( FNAME, STDATE, STTIME, 'RJ' )

         IF ( N_AE_SPC .GT. 0 ) THEN
            FNAME = INIT_SENS_1
            CALL LOAD_SENGRID ( FNAME, STDATE, STTIME, 'AE')
         END IF  ! N_AE_SPC > 0

         IF ( N_NR_SPC .GT. 0 ) THEN
            FNAME = INIT_SENS_1
            CALL LOAD_SENGRID ( FNAME, STDATE, STTIME, 'NR' )
         END IF  ! N_NR_SPC > 0

         IF ( N_TR_SPC .GT. 0 ) THEN
            FNAME = INIT_SENS_1
            CALL LOAD_SENGRID ( FNAME, STDATE, STTIME, 'TR' )
         END IF  ! N_TR_SPC > 0

      ELSE  ! Set to initial conditions or 0.0

         DO NP = 1, NPMAX
            IF ( IPT( NP ) .EQ. 1 ) THEN
               DO SPC = 1, NSPCSD
                  DO LAY = 1, NLAYS
                     DO ROW = 1, NROWS
                        DO COL = 1, NCOLS
                           SENGRID( COL, ROW, LAY, NP, SPC) =
     &                               CGRID( COL, ROW, LAY, SPC )
     &                             * REAL( IPARM( NP, SPC ) )
     &                             * IREGION( COL, ROW, LAY, NP )
                        END DO
                     END DO
                  END DO
               END DO
            ELSE
               DO SPC = 1, NSPCSD
                  DO LAY = 1, NLAYS
                     DO ROW = 1, NROWS
                        DO COL = 1, NCOLS
                           SENGRID( COL, ROW, LAY, NP, SPC) = 0.0
                        END DO
                     END DO
                  END DO
               END DO
            END IF
         END DO

      END IF
#endif

      CALL SUBST_BARRIER

      CALL LOG_HEADING( LOGDEV, "Check Initial Concentrations" )
#endif

      RETURN
      END
