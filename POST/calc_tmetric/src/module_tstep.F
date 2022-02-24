
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/module_tstep.F,v 1.4 2011/10/21 14:41:33 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  MODULE TIME_STEP:  defines a time class
C             
C*************************************************************************
      MODULE TIME_STEP

      INTEGER :: NSTEPS

      INTEGER , ALLOCATABLE  :: STEP_DATE( : )
      INTEGER , ALLOCATABLE  :: STEP_TIME( : )
      INTEGER , ALLOCATABLE  :: STEP_FILE( : )

      INTEGER :: TIMESTEP

      CONTAINS

         SUBROUTINE GET_TSTEPS
C*************************************************************************
C
C  FUNCTION: Set-up time step sequence for reading files and writing
C            output records
C             
C*************************************************************************
         USE M3FILES
         USE M3UTILIO

         IMPLICIT NONE     

C..ARGUMENTS: None

C..PARAMETERS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*80  MSG         ! Log message
         CHARACTER*16  PNAME       ! Program Name

         INTEGER D0, D1            ! Differences betwwen two times, seconds
         INTEGER INDX              ! Array location to insert new index
         INTEGER MXSTEPS           ! max possible no. of time steps
         INTEGER N, NFL, NS, N2    ! Loop indices
         INTEGER JDATE             ! Current date
         INTEGER JTIME             ! Current time
         INTEGER STATUS

   
C**********************************************************************
         DATA PNAME / 'GET_TSTEPS' /

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the maximum number of time steps
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         MXSTEPS = 0
         DO N = 1, N_M3FILES

            IF( .NOT. DESC3( M3_FLNAME( N ) ) ) THEN
               CALL M3EXIT( PNAME, 0, 0, 'Could not get ' //
     &                      M3_FLNAME( N ) // ' file description',
     &                      XSTAT1 )
            ENDIF

            MXSTEPS = MXSTEPS + MXREC3D
         ENDDO

         ALLOCATE( STEP_DATE( MXSTEPS ), STEP_TIME( MXSTEPS ), 
     &             STEP_FILE( MXSTEPS ), stat=status )

         IF ( status .NE. 0 ) THEN
           MSG = 'Failure allocating step_date, step_time, or step_file'
           CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Load all steps from first file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF( .NOT. DESC3( M3_FLNAME( 1 ) ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Could not get ' // 
     &                   M3_FLNAME( 1 ) // ' file description',
     &                   XSTAT1 )
         ENDIF

         JDATE = SDATE3D
         JTIME = STIME3D
         TIMESTEP = TSTEP3D

         NSTEPS = MXREC3D

         DO NS = 1, NSTEPS
            STEP_DATE( NS ) = JDATE
            STEP_TIME( NS ) = JTIME
            STEP_FILE( NS ) = 1
            CALL NEXTIME( JDATE, JTIME, TSTEP3D )
         ENDDO

         IF( N_M3FILES .EQ. 1 ) RETURN
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Insert non-duplicative time steps from remaining files in the array
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         MSG = 'Multiple CTM Concentration files being used' 
         CALL M3MESG( MSG )
         MSG = 'Duplicate time steps will be eliminated' 
         CALL M3MESG( MSG )

         DO NFL = 2, N_M3FILES

            IF ( .NOT. DESC3( M3_FLNAME( NFL ) ) ) THEN
               CALL M3EXIT( PNAME, 0, 0,
     &                     'Could not get ' // M3_FLNAME( NFL ) //
     &                     ' file description', XSTAT1 )
            ENDIF

            JDATE = SDATE3D
            JTIME = STIME3D

            DO NS = 1, MXREC3D

               DO N = 1, NSTEPS

                  INDX = 0
                  D1 = SECSDIFF( STEP_DATE( N ), STEP_TIME( N ),
     &                           JDATE, JTIME )

                  IF( N .EQ. 1 .AND. D1 .LT. 0 ) THEN
                     INDX = 1
                  ELSEIF( N .EQ. NSTEPS .AND. D1 .GT. 0 ) THEN
                     INDX = NSTEPS + 1
                  ELSEIF( N .GT. 1 ) THEN
                     D0 = SECSDIFF( STEP_DATE( N - 1 ), STEP_TIME( N - 1 ),
     &                              JDATE, JTIME )
                     IF( D0 .GT. 0 .AND. D1 .LT. 0 ) INDX = N
                  ENDIF
          
                  IF( INDX .GT. 0 ) THEN
                     DO N2 = NSTEPS, INDX, -1
                        STEP_DATE( N2 + 1 ) =  STEP_DATE( N2 )
                        STEP_TIME( N2 + 1 ) =  STEP_TIME( N2 )
                     ENDDO
                  
                     STEP_DATE( INDX ) = JDATE
                     STEP_TIME( INDX ) = JTIME
                     STEP_FILE( INDX ) = NFL
                     NSTEPS = NSTEPS + 1
                  ENDIF

               ENDDO

               IF( INDX .EQ. 0 ) WRITE( LOGUNIT, 93000) JDATE, JTIME,
     &                                  M3_FLNAME( NFL )
               CALL NEXTIME( JDATE, JTIME, TSTEP3D )

            ENDDO

         ENDDO

         RETURN


93000    FORMAT( 10X, 'Duplicate time step ignored: ', I7, 1X, I6, 1X, A )

         END SUBROUTINE GET_TSTEPS

      END MODULE TIME_STEP








