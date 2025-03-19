
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
C $Header: /project/yoj/arc/BCON/src/m3conc/m3_driver.F,v 1.2 2011/10/21 16:52:35 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE M3_INBNDY ( LOGUNIT )

C***********************************************************************
 
C  Function: Opens the Models-3 CTM conc file(s) and checks for 
C            data consistency, and calls the routine to 
C            compute the BCs and write the output BC file  
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: CK_CTM_FLS
C                                    M3_BCOUT  
 
C  Revision History:
C    Prototype created by Jerry Gipson, January, 1998          
C    Modified 4/22/99 by JG to provide default treatment for missing RUNLEN
C    Modified by JG May, 1999 to treat PinG concs
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    01/05/05 J.Young: vert dyn alloc - Use VGRD_DEFN
C    13 Jul 11 J.Young: Replaced I/O API include files with M3UTILIO and
C                       Namelist for species definitions
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
C    14 Sep 18 S.Roselle: Removed species mapping
C    10 June 19 F.Sidi  : Corrected data type mismatch between IOAPI and BCON
C    20 April 21 C. Hogrefe: Force height or pressure interpolation if either 
C                            grid uses hybrid vertical coordinates. This
C                            requires checking for MET_CRO_3D and MET_BDY_3 
                    
C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE BC_PARMS    ! BCON parameters

      IMPLICIT NONE     

C Arguments:
      INTEGER, INTENT( IN )    :: LOGUNIT  ! Unit number for output log

C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER(  16 ) :: ENV_DFLT       ! Environment variable default value
      CHARACTER(  80 ) :: ENV_DESC       ! Environment variable description
      CHARACTER(  16 ) :: FL_NAME        ! Input CTM file name
      CHARACTER(  80 ) :: MSG            ! Log message
      CHARACTER(  16 ) :: PNAME = 'M3_INBNDY' ! Procedure name
      CHARACTER( 256 ) :: RET_VAL        ! Returned value of environment variable

      CHARACTER(  16 ), ALLOCATABLE :: CTM_FL_NAME( : )  ! Names of CTM files
      CHARACTER(  16 ), ALLOCATABLE :: INFL_SP_NAME( : ) ! Names of CTM species
      CHARACTER(  16 ), ALLOCATABLE :: UNITS_IN( : ) ! Units for CTM species
      CHARACTER(  80 ), ALLOCATABLE :: VDESC_IN( : ) ! Variable description for CTM species

      INTEGER, ALLOCATABLE :: VTYPE_IN( : ) ! variable type for CTM species

      INTEGER :: RUNLEN = 0   ! Time duration for BC output (HHMMSS)
      INTEGER :: SDATE = 0    ! Date for BC output (YYYYJJJ)
      INTEGER :: STIME = 0    ! Time for BC output (HHMMSS)
      INTEGER :: JDATE        ! Current date
      INTEGER :: JTIME        ! Current time
      INTEGER :: N            ! File loop index
      INTEGER :: N_CTM_FLS    ! Number of input CTM files
      INTEGER :: NCOLS_IN     ! No. of columns in input conc file
      INTEGER :: NLAYS_IN     ! No. of layers in input conc file
      INTEGER :: NROWS_IN     ! No. of rows in input conc file
      INTEGER :: NSPCS_IN     ! CTM files species counter
      INTEGER :: VGTYP_IN     ! input conc file VGTYP
      INTEGER :: SDATE_IN     ! input conc file start date
      INTEGER :: STIME_IN     ! input conc file start time
      INTEGER :: EDATE_IN     ! input conc file end date
      INTEGER :: ETIME_IN     ! input conc file end time
      INTEGER :: TSTEP_IN     ! input conc file  timestep
      INTEGER :: NSTEPS_IN    ! Number time steps in input conc file
      INTEGER :: SECS         ! time variable (sec)
      INTEGER :: SECS_STRT    ! time variable (sec)
      INTEGER :: SECS_END     ! time variable (sec)
      INTEGER :: NSTINC       ! No. of steps on file before start of output
      INTEGER :: STATUS       ! Status code
      INTEGER :: ALLOCSTAT    ! Status returned from array allocation
      INTEGER :: STRTINC      ! No. of sec. on file before start of output
      INTEGER :: STEPSECS     ! Seconds per time step
      INTEGER :: TOTSECS      ! Time duration for BC file (seconds)
      INTEGER :: NSTEPS       ! Number time steps in BC output file
      INTEGER :: TSTEP        ! Time step BC output file (HHMMSS)
      INTEGER :: V            ! Variable loop index

      REAL :: VGTOP_IN     ! input conc file VGTOP

      INTERFACE

         SUBROUTINE CK_CTM_FLS ( LOGUNIT, N_CTM_FLS, CTM_FL_NAME )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: N_CTM_FLS
            CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )
         END SUBROUTINE CK_CTM_FLS

         SUBROUTINE CK_MET_FL ( LOGUNIT, SDATE, STIME, TSTEP, NSTEPS, CTM_FL_NAME )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: TSTEP
            INTEGER, INTENT( IN ) :: NSTEPS
            CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )
         END SUBROUTINE CK_MET_FL

         SUBROUTINE CK_BDY_FL ( LOGUNIT, SDATE, STIME, TSTEP, NSTEPS )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: TSTEP
            INTEGER, INTENT( IN ) :: NSTEPS
         END SUBROUTINE CK_BDY_FL

         SUBROUTINE M3_BCOUT ( LOGUNIT,
     &                         N_CTM_FLS,
     &                         SDATE, STIME, NSTEPS,
     &                         NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                         CTM_FL_NAME, INFL_SP_NAME,
     &                         VTYPE_IN, UNITS_IN, VDESC_IN )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: N_CTM_FLS
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: NSTEPS
            INTEGER, INTENT( IN ) :: NCOLS_IN
            INTEGER, INTENT( IN ) :: NLAYS_IN
            INTEGER, INTENT( IN ) :: NROWS_IN
            INTEGER, INTENT( IN ) :: NSPCS_IN
            CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )
            CHARACTER( 16 ), INTENT( IN ) :: INFL_SP_NAME( : )
            CHARACTER( 16 ), INTENT( IN ) :: UNITS_IN( : )
            CHARACTER( 80 ), INTENT( IN ) :: VDESC_IN( : )
            INTEGER, INTENT( IN ) :: VTYPE_IN( : )
         END SUBROUTINE M3_BCOUT

      END INTERFACE

C***********************************************************************

      WRITE( LOGUNIT, 92000 )

      SDATE = ENVINT( 'SDATE', 'START DATE', 0, STATUS )
      IF ( STATUS .NE. 0 ) SDATE = 0

      STIME = ENVINT( 'STIME', 'START TIME', 0, STATUS )
      IF ( STATUS .NE. 0 ) STIME = 0

      RUNLEN = ENVINT( 'RUNLEN', 'RUN LENGTH', 0, STATUS )
      IF ( STATUS .NE. 0 ) RUNLEN = 0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine the number of input CTM conc files that need to be read
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( CTM_FL_NAME( MXCTMS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating CTM_FL_NAME'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      N_CTM_FLS = 0

      WRITE( LOGUNIT, 92020 )

      DO N = 1, MXCTMS

         WRITE( FL_NAME, '( ''CTM_CONC_'', I1 )' ) N
         WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I1 )' ) N 
         ENV_DFLT = ' '        
         CALL ENVSTR ( FL_NAME, ENV_DESC, ENV_DFLT, RET_VAL, STATUS)

         IF ( STATUS .EQ. 0 ) THEN
            N_CTM_FLS = N_CTM_FLS + 1
            CTM_FL_NAME( N_CTM_FLS ) = FL_NAME
         END IF
     
      END DO

      IF ( N_CTM_FLS .EQ. 0 ) THEN
         MSG = 'No CTM CONC files found'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the input Models-3 CTM concentration file(s) and put the species 
c  names on each file in one contiguous array
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( INFL_SP_NAME( N_CTM_FLS*MXVARS3 ),
     &          UNITS_IN( N_CTM_FLS*MXVARS3 ),
     &          VDESC_IN( N_CTM_FLS*MXVARS3 ),
     &          VTYPE_IN( N_CTM_FLS*MXVARS3 ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating UNITS_IN, VDESC_IN, VTYPE_IN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      NSPCS_IN = 0

      DO N = 1, N_CTM_FLS

         IF ( .NOT. OPEN3( CTM_FL_NAME( N ), FSREAD3, PNAME ) ) THEN
            MSG = 'Could not open ' // CTM_FL_NAME( N ) // ' file'
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF

         IF ( .NOT. DESC3( CTM_FL_NAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // CTM_FL_NAME( N ) 
     &            // ' file'
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF

         NCOLS_IN  = NCOLS3D
         NROWS_IN  = NROWS3D
         NLAYS_IN  = NLAYS3D
         VGTYP_IN  = VGTYP3D
         VGTOP_IN  = VGTOP3D
         SDATE_IN  = SDATE3D
         STIME_IN  = STIME3D
         TSTEP_IN  = TSTEP3D
         NSTEPS_IN = MXREC3D

         DO V = 1, NVARS3D
            NSPCS_IN = NSPCS_IN + 1
            INFL_SP_NAME( NSPCS_IN ) = VNAME3D( V )
            VTYPE_IN( NSPCS_IN ) = VTYPE3D( V )
            UNITS_IN( NSPCS_IN ) = UNITS3D( V )
            VDESC_IN( NSPCS_IN ) = VDESC3D( V )
         END DO

      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check multiple input CTM files for consistency
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( N_CTM_FLS .GT. 1 ) CALL CK_CTM_FLS( LOGUNIT, N_CTM_FLS, CTM_FL_NAME )

      TSTEP  = TSTEP_IN
      NSTEPS = NSTEPS_IN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Check MET_CRO_3D and MET_BDY_3 if necessary
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( VGTYP_GD .NE. VGTYP_IN .OR. VGTOP_GD .NE. VGTOP_IN .OR. 
     &     VGTYP_GD .EQ. -9999 .OR. VGTYP_IN .EQ. -9999) THEN 
         CALL CK_MET_FL ( LOGUNIT, SDATE, STIME, TSTEP, NSTEPS, CTM_FL_NAME )
         CALL CK_BDY_FL ( LOGUNIT, SDATE, STIME, TSTEP, NSTEPS )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Reset start and runlen times if necessary
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      EDATE_IN = SDATE_IN
      ETIME_IN = STIME_IN
      CALL NEXTIME ( EDATE_IN, ETIME_IN,
     &               SEC2TIME( ( NSTEPS_IN - 1 ) * TIME2SEC( TSTEP_IN ) ) )

C for time-dependent concentration file
      IF ( TSTEP_IN .GT. 0 ) THEN

C inherit timestep from MET_BDY_3D_FIN
         IF ( .NOT. DESC3( MET_BDY_3D_FIN ) ) THEN
            MSG = 'Could not read DESC of  ' // MET_BDY_3D_FIN // ' file'
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF
         TSTEP = TSTEP3D

C if user did not specify SDATE, then inherit the SDATE from MET_BDY_3D_FIN
         IF ( SDATE .EQ. 0 ) THEN
            MSG = 'Input SDATE equals zero; SDATE ' //
     &            'reset to MET_BDY_3D_FIN file start date'
            CALL M3MESG ( MSG )
            SDATE = SDATE3D
         END IF

C if user did not specify STIME, then inherit the STIME from MET_BDY_3D_FIN
         IF ( STIME .EQ. 0 ) THEN
            MSG = 'Input STIME equals zero; STIME ' //
     &            'reset to MET_BDY_3D_FIN file start time'
            CALL M3MESG ( MSG )
            STIME = STIME3D
         END IF

         SECS_STRT = SECSDIFF( SDATE_IN, STIME_IN, SDATE, STIME )
         SECS_END  = SECSDIFF( EDATE_IN, ETIME_IN, SDATE, STIME )
         IF ( ( SECS_STRT .LT. 0 ) .OR. ( SECS_END .GT. 0 ) ) THEN
            MSG = 'Requested starting time is not in the ' //
     &            CTM_FL_NAME( 1 ) // ' file'
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF
      
c Find number of steps on file from starting point
         STEPSECS = TIME2SEC( TSTEP )
         STRTINC = SECSDIFF( SDATE3D, STIME3D, SDATE, STIME )
         NSTINC = STRTINC / STEPSECS

c Check and reset RUNLEN if necessary
         IF ( RUNLEN .NE. 0 ) THEN

            IF ( RUNLEN .LT. 1000000 ) THEN
               TOTSECS  = TIME2SEC( RUNLEN )
            ELSE                          ! HH > 99
               RUNLEN = RUNLEN - 1000000
               TOTSECS  = TIME2SEC( RUNLEN )
               TOTSECS  = TOTSECS + 360000
            END IF

            IF ( MOD( TOTSECS, STEPSECS ) .EQ. 0 ) THEN
               NSTEPS = TOTSECS / STEPSECS + 1
            ELSE
               MSG = 'Output time step ' // HHMMSS( TSTEP3D  ) //
     &               ' does not divide duration ' // HHMMSS( RUNLEN )
               CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
            END IF

            IF ( NSTEPS .GT. MXREC3D - NSTINC ) THEN
               MSG = 'Input RUNLEN exceeds time steps on input file. '
               WRITE( LOGUNIT, '( /5X, A )' ) MSG
               MSG = 'Resetting RUNLEN to correspond to CTM file ending ' //
     &               'date & time.'
               WRITE( LOGUNIT, '( 5X, A )' ) MSG
               NSTEPS = MXREC3D - NSTINC
               RUNLEN = SEC2TIME( ( NSTEPS - 1 ) * STEPSECS )
            END IF

         ELSE

            MSG = 'Input RUNLEN not set or equal to zero. '
            WRITE( LOGUNIT, '( /5X, A )' ) MSG
            MSG = 'Resetting RUNLEN to correspond to MET_BDY_3D_FIN file ' //
     &            'ending date & time.'
            WRITE( LOGUNIT, '( 5X, A )' ) MSG
            NSTEPS = MXREC3D - NSTINC

         END IF

c Check to make sure end date is on CTM file
         JDATE = SDATE
         JTIME = STIME
         CALL NEXTIME ( JDATE, JTIME, RUNLEN )

         SECS = SECSDIFF( JDATE, JTIME, EDATE_IN, ETIME_IN )
         IF ( SECS .LT. 0 ) THEN
            MSG = 'Requested ending time is not in the ' //
     &             CTM_FL_NAME( 1 ) // ' file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

      ELSE

         MSG = 'Input CTM file is time independent. ' //
     &         'Output BCON file will be time independent.'
         CALL M3MESG ( MSG )
         SDATE = 0
         STIME = 0
         TSTEP = 0
         NSTEPS = 1

      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  compute and outputs BCs from the input CTM conc file(s)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL M3_BCOUT ( LOGUNIT,
     &                N_CTM_FLS,
     &                SDATE, STIME, NSTEPS,
     &                NCOLS_IN, NROWS_IN, NLAYS_IN,
     &                NSPCS_IN, CTM_FL_NAME,
     &                INFL_SP_NAME, VTYPE_IN, UNITS_IN, VDESC_IN )

      RETURN

C************************* FORMAT Statements ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  Input section '
     &         / 1X, 79( '#' )
     &        // 5X, 'Boundary Concentrations from a CMAQ CTM ',
     &               'concentration file.' )

92020 FORMAT( // 5X, 'Input Models3 CTM file names: ' )

      END
