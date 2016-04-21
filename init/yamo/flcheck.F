
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
C $Header: /project/yoj/arc/CCTM/src/init/yamo/flcheck.F,v 1.4 2011/10/21 16:11:23 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE FLCHECK ( STDATE, STTIME, TSTEP )

C-----------------------------------------------------------------------
C Function: To check file header data for consistency with COORD.EXT
C            and runlength; only gridded and boundary files are checked.
 
C Preconditions: None
 
C Key Subroutines/Functions Called:
 
C Revision History: Prototype created by Jerry Gipson, September, 1997
C                   Modified Jun, 1998 by Jerry Gipson to remove PING
C                   and MEPSE files from being checked
C                   Jeff - Nov 2000 - ck only input files for start date/time
C   30 Mar 01 J.Young: elim reference to grid
C    6 Jul 10 J.Young: minor mods - F90-ize
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   11 Apr 16 D.Wong: formulated a robust way to handle a situation when
C                     ACONC file with STIME3 = starting time of the simulation,
C                     and the file is emtpy and requires to go through CHECK3
C-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE 

C Includes:
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE 'STATE3.EXT'
      
C Arguments:
      INTEGER, INTENT( IN ) :: STDATE   ! Current date (YYYYDDD)
      INTEGER, INTENT( IN ) :: STTIME   ! Current time (HHMMSS)
      INTEGER, INTENT( IN ) :: TSTEP    ! Current time step (HHMMSS)

C Parameters: None

C External Functions: None

C Saved Local Variables: None
      
C Local Variables:
      CHARACTER( 80 ) :: MSG                ! Message buffer
      CHARACTER( 16 ) :: PNAME = 'FLCHECK'  ! Procedure name
      
      INTEGER JDATE     ! Time step date (YYYYDDD)
      INTEGER JTIME     ! Time step time (HHMMSS)
      INTEGER N         ! Loop index
      INTEGER RUNLEN    ! Length of run
      INTEGER STATUS    ! Status of returned value
!     INTEGER LOGDEV    ! Log file device no.

      LOGICAL LERROFF   ! Flag to stop run if errors found
      LOGICAL LERROR    ! Flag to indicate error for 1 or more files
      LOGICAL LOK       ! Flag to indicate error for 1 file

      INTEGER :: FID
      INTEGER, EXTERNAL :: NAME2FID

C-----------------------------------------------------------------------

      LOGDEV = INIT3()

      WRITE( LOGDEV,2011 ) TRIM( PNAME )

      LERROFF = .TRUE.   ! default
      LERROFF = ENVYN( 'FL_ERR_STOP', 'Flag for stopping on errors',
     &                 LERROFF, STATUS )  

      LERROR = .FALSE.

      RUNLEN = 480000    ! default
      RUNLEN = ENVINT( 'CTM_RUNLEN', 'Scenario Run Duration (HHMMSS)',
     &                 RUNLEN, STATUS )

      DO N = 1, N_FILES      

C skip Ping and Mepse files
         IF ( INDEX( LGCL_NAME( N ), 'PING'  ) .EQ. 0 .AND. 
     &        INDEX( LGCL_NAME( N ), 'MEPSE' ) .EQ. 0 ) THEN

            LOK = OPEN3( LGCL_NAME( N ), FSREAD3, PNAME )

            IF ( LOK ) THEN

               IF ( DESC3( LGCL_NAME( N ) ) ) THEN
     
C only check gridded or boundary files
                  IF ( FTYPE3D .EQ. GRDDED3 .OR. FTYPE3D .EQ. BNDARY3 ) THEN

                     MSG = 'Checking header data for file: ' // LGCL_NAME( N )
                     CALL M3MESG( MSG )

                     CALL GRDCHECK ( LGCL_NAME( N ), LOK )

c check time steps for time dependent files
                     IF ( TSTEP3D .GT. 0 ) THEN
                        JDATE = STDATE
                        JTIME = STTIME

                        FID   = NAME2FID( LGCL_NAME( N ) )

                        IF ( MXREC3( FID ) .GT. 0 ) THEN
                           IF ( .NOT. CHECK3( LGCL_NAME( N ), ALLVAR3,
     &                                        JDATE, JTIME ) ) THEN
                              LOK = .FALSE.
                              MSG = 'Starting time not on file'
                              CALL M3MESG( MSG )
                           END IF
                        END IF
 
c skip checking ending time for output files
                        IF ( IN_MODE( N ) ) THEN
                           CALL NEXTIME ( JDATE, JTIME, RUNLEN )
                           IF ( .NOT. CHECK3( LGCL_NAME( N ), ALLVAR3,
     &                                        JDATE, JTIME ) ) THEN
                              LOK = .FALSE.
                              MSG = 'Ending time not on file'
                              CALL M3MESG( MSG )
                           END IF
                        END IF

                     END IF   ! if TSTEP3D .GT. 0

                     IF ( .NOT. LOK ) LERROR = .TRUE.

                  END IF   ! only check gridded or boundary files

               ELSE

                  LOK = .FALSE.
                  MSG = 'DESC3 failed for ' // LGCL_NAME( N )
                  CALL M3MESG( MSG )

               END IF   ! DESC3 successful

            END IF   ! successful open

!           IF ( .NOT. CLOSE3( LGCL_NAME( N ) ) ) THEN
!              MSG = '  >>> COULD NOT CLOSE ' // LGCL_NAME( N )
!              CALL M3WARN ( PNAME, STDATE, STTIME, MSG )
!           END IF

         END IF  ! skip Ping and Mepse files

      END DO

      IF ( LERROR ) THEN
         MSG = 'Inconsistent header data on input files'
         IF ( LERROFF ) THEN       
            CALL M3EXIT ( PNAME, STDATE, STTIME, MSG, XSTAT2 )
         ELSE
            CALL M3WARN ( PNAME, STDATE, STTIME, MSG )
            WRITE( LOGDEV,2015 ) TRIM( PNAME )
            RETURN
         END IF
      ELSE
         MSG = '   No inconsistent header data found on input files'
         CALL M3MESG( MSG )
         WRITE( LOGDEV,2015 ) TRIM( PNAME )
      END IF

      RETURN

2011  FORMAT( / 5X, 14( '= ' ), 'Start ', A, 14( ' =' ) / )
2015  FORMAT( / 5X, 14( '= ' ), ' End  ', A, 14( ' =' ) / )
      END
