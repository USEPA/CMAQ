
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
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/module_file.F,v 1.4 2011/10/21 14:41:32 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  MODULE:  sets up file data
C             
C*************************************************************************
      MODULE M3FILES

      USE M3UTILIO

      INTEGER LOGUNIT                       ! Logdevice

      INTEGER N_M3FILES                     ! No. of input Models-3 files

      INTEGER, PARAMETER :: MXM3FLS = MXFILE3 - 1   ! Max no. of input files - use IOAPI parameter 
                                                    ! MXFILE3 - 1 to also allow one output file

      CHARACTER*16  M3_FLNAME( MXM3FLS )    ! Names of Models-3 file

      CHARACTER*256 M3FILE                  ! first filename

      CONTAINS


         SUBROUTINE OPEN_M3FILES
C*************************************************************************
C
C  FUNCTION:  To provide site data
C             
C*************************************************************************

         USE M3UTILIO

         IMPLICIT NONE 

      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    ENV_DFLT     ! Environment variable default value
         CHARACTER*16    FL_NAME      ! Input Models-3 file name
         CHARACTER*16    PNAME        ! Program Name
         CHARACTER*80    ENV_DESC     ! Environment variable description
         CHARACTER*80    MSG          ! Error message
         CHARACTER*256   RET_VAL      ! Returned value of environment variable

         INTEGER   N                  ! Loop index
         INTEGER   STATUS             ! Status code

C**********************************************************************
         DATA  PNAME       / 'OPEN_M3FILES'  /

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine the number of input CTM conc files that need to be read
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LOGUNIT = INIT3()

         N_M3FILES = 0

         WRITE( LOGUNIT, 92020 )

         DO N = 1, MXM3FLS

            IF( N .LT. 10 ) THEN
               WRITE( FL_NAME, '( ''M3_FILE_'', I1 )' ) N
               WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I1 )' ) N 
            ELSEIF( N .LT. 100 ) THEN
               WRITE( FL_NAME, '( ''M3_FILE_'', I2 )' ) N
               WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I2 )' ) N 
            ELSE
               WRITE( FL_NAME, '( ''M3_FILE_'', I3 )' ) N
               WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I3 )' ) N 
            ENDIF

            ENV_DFLT = ' '        
            CALL ENVSTR( FL_NAME, ENV_DESC, ENV_DFLT, RET_VAL, STATUS)

            IF( STATUS .EQ. 0 ) THEN
               If( N_M3FILES .eq. 0) M3FILE = RET_VAL
               N_M3FILES = N_M3FILES + 1
               M3_FLNAME( N_M3FILES ) = FL_NAME
            ELSE
               IF ( N_M3FILES .eq. 0) THEN !TRY CHECKING FOR 'INFILE' INSTEAD - BACKWARD COMPATIBILITY
                  ENV_DFLT = ' '        
                  CALL ENVSTR( 'INFILE', ENV_DESC, ENV_DFLT, RET_VAL, STATUS)
                  IF( STATUS .EQ. 0 ) THEN
                     N_M3FILES = N_M3FILES + 1
                     M3_FLNAME( N_M3FILES ) = 'INFILE'
                  ELSE
                     EXIT
                  ENDIF
               ELSE !NOT FIRST FILE, EXIT LOOP
                  EXIT
               ENDIF 
            ENDIF
     
         ENDDO

         IF( N_M3FILES .EQ. 0 ) THEN
            MSG = 'No CTM CONC files found'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the files ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO N = 1, N_M3FILES
            IF( .NOT. OPEN3( M3_FLNAME( N ), 1, PNAME) )THEN
               CALL M3EXIT( PNAME, 0 , 0,
     &              'Could not open input file ' // M3_FLNAME( N ),
     &               XSTAT1 )
            ENDIF
         ENDDO 

         RETURN

C******************  FORMAT  STATEMENTS   ******************************

92020  FORMAT( //5X, A, // )

         END SUBROUTINE OPEN_M3FILES

      
      END MODULE M3FILES
