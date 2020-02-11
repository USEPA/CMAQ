
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

!------------------------------------------------------------------------!
! This serves the main program for offline CMAQ mode

! Revision History:
!  02/01/19, D. Wong: initial implementation
!  02/10/20, D. Wong: Added new environmental variable MET_TSTEP
!                     to allow running with temporally fine 
!                     meteorology
!------------------------------------------------------------------------!

      PROGRAM CMAQ

        USE RUNTIME_VARS
        USE UTILIO_DEFN

        IMPLICIT NONE

#ifdef parallel
        INCLUDE 'mpif.h'

        INTEGER   PAR_ERR     ! Error code from parallel initialization
        REAL( 8 ) BEGTIME     ! Wall-clock time (sec) at MPI initialization
        REAL( 8 ) ENDTIME     ! Wall-clock time (sec) at MPI finish
        REAL( 8 ) ELAPTIME    ! ENDTIME-BEGTIME
#else
      REAL      BEGTIME     ! Wall-clock time (sec) at MPI initialization
      REAL      ENDTIME     ! Wall-clock time (sec) at MPI finish
      REAL      ELAPTIME    ! ENDTIME-BEGTIME
#endif
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: MODEL_TSTEP, NSTEPS, ISTEP,
     $             JDATE, JTIME, TOTSECS, STEPSECS
        CHARACTER( 96 ) :: MSG = ' '

        INTERFACE
          SUBROUTINE CMAQ_DRIVER ( MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP,
     $                             MODEL_JDATE, MODEL_JTIME, LAST_STEP,
     $                             COUPLE_TSTEP, NCOLS_IN, NLAYS_IN)
            INTEGER, INTENT( IN )  :: MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP
            INTEGER, INTENT( OUT ) :: MODEL_JDATE, MODEL_JTIME
            LOGICAL, INTENT( IN )  :: LAST_STEP
            INTEGER, INTENT( IN ), OPTIONAL :: COUPLE_TSTEP
            INTEGER, INTENT( IN ), OPTIONAL :: NCOLS_IN, NLAYS_IN
          END SUBROUTINE CMAQ_DRIVER
        END INTERFACE

#ifdef parallel
C Start up processor communication and retrieve number of compute
C processes (NPROCS)
        CALL MPCOMM_INIT( NPROCS, MYPE, BEGTIME, PAR_ERR )
#else
        NPROCS = 1
        MYPE = 0
        CALL CPU_TIME( BEGTIME )
#endif
        ! Initialize all runscript environmental variables
        CALL INIT_ENV_VARS( 0, 0 )

        MODEL_TSTEP = LOCAL_TSTEP

        TOTSECS  = TIME2SEC( RUNLEN )

        STEPSECS = TIME2SEC( MIN( MODEL_TSTEP, MET_TSTEP) )

        IF ( MOD( TOTSECS, STEPSECS ) .EQ. 0 ) THEN
           NSTEPS = TOTSECS / STEPSECS
        ELSE
           MSG = 'EXIT: Output time step ' // HHMMSS( MODEL_TSTEP ) //
     &           ' does not divide duration ' // HHMMSS( RUNLEN )
           CALL M3EXIT( 'CMAQ_MAIN', STDATE, STTIME, MSG, XSTAT1 )
        END IF
        
        ! Call to driver, for nsteps or how often the model synchronization step
        ! needs to be recalculated (RUNLEN = 60*(NSTEPS*NREPS*TSTEP(2))

        DO ISTEP = 1, NSTEPS
           CALL CMAQ_DRIVER (STDATE, STTIME, MODEL_TSTEP, JDATE, JTIME, (ISTEP .EQ. NSTEPS))
        END DO

#ifdef parallel
        ENDTIME = MPI_WTIME()   ! get final wall-clock time
#else
        CALL CPU_TIME( ENDTIME )   ! get final wall-clock time
#endif
        ELAPTIME = ENDTIME - BEGTIME
        WRITE( XMSG, '(A,F10.1,A)' ) 'The elapsed time for this simulation was', 
     &                               ELAPTIME, ' seconds.'
        CALL LOG_MESSAGE( LOGDEV, XMSG )
        IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * )
        IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
        IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * )

#ifdef parallel
C Shut down MPI
        CALL MPI_FINALIZE ( PAR_ERR )
        IF ( PAR_ERR .NE. 0 ) THEN
           MSG = 'Error terminating MPI'
           CALL M3EXIT( 'CMAQ_MAIN', STDATE, STTIME, MSG, XSTAT1 )
        END IF
#endif

      END PROGRAM CMAQ
