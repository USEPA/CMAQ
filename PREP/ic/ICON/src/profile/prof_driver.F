
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
C $Header: /project/yoj/arc/ICON/src/profile/prof_driver.F,v 1.2 2011/10/21 16:41:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE INIC( LOGUNIT, SDATE, STIME, NML_IC_NAME )   

C***********************************************************************
 
C  Function: Opens the input profile data file, and calls the routine to 
C             compute the ICs and write the output IC file  
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: GC_SPC_MAP
C                                    NGC_SPC_MAP
C                                    LST_SPC_MAP
C                                    ICOUT
 
C  Revision History: Prototype created by Jerry Gipson, January, 1998
C                    Modified April 1998 by JG to remove ppb option for 
C                       input profile file
C  06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C  07 Jun 11 J.Young: Removed RADM2<->CB4 mechanism conversion feature
C  13 Jun 11 J.Young: Convert for Namelist redesign
C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
 
C***********************************************************************

      USE UTILIO_DEFN
      USE CGRID_SPCS   ! CGRID mechanism species
      USE IC_PARMS     ! ICON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments: 
      INTEGER LOGUNIT                 ! Unit number for output log
      INTEGER SDATE                   ! Date for IC Output
      INTEGER STIME                   ! Time for IC output
      CHARACTER( 16 ) :: NML_IC_NAME( * )  ! Names of species in *.NMLs

C Parameters: None

C Local Variables:
      CHARACTER(  16 ) :: ENV_DFLT   ! Environment variable default value
      CHARACTER(  80 ) :: ENV_DESC   ! Environment variable description
      CHARACTER( 256 ) :: LINEIN     ! Input line
      CHARACTER(  80 ) :: MSG        ! Log message
      CHARACTER(  16 ) :: PNAME = 'INIC' ! Procedure name
      CHARACTER(  16 ) :: IC_PROFILE = 'IC_PROFILE' ! Logical name of input profile file
      CHARACTER( 256 ) :: PROF_FL_NAME   ! Input profile file name
      CHARACTER(  16 ) :: PROF_UNITS = 'PROF_UNITS'  ! Environment variable for units
      CHARACTER(  16 ) :: PUNITS         ! Value of units Environment variable

      CHARACTER(  16 ) :: PROF_SP_NAME( MXCTMS*MXVARS3 ) ! Species names on profile file
!     CHARACTER(  16 ) :: SPNAME_OUT( NSPCSD )           ! Output file species names
      CHARACTER(  16 ), ALLOCATABLE :: SPNAME_OUT( : )   ! Output file species names
  
      INTEGER N, SPC       ! Loop indices
      INTEGER N_GC_OUT     ! No. of GC species
      INTEGER NLAYS_IN     ! No. of layers in input conc file
      INTEGER NSPCS_IN     ! CTM files species counter
      INTEGER NSPCS_OUT    ! Number of IC species on output file(s)
      INTEGER PFILE        ! Unit number of profile file    
      INTEGER STATUS       ! Status code
      INTEGER ALST         ! Allocate statu

!     INTEGER :: SPMAP( NSPCSD )    ! Map to input file species from IC species
      INTEGER, ALLOCATABLE :: SPMAP( : )  ! Map to input file species from IC species

      LOGICAL LERROR                ! Error flag

C***********************************************************************

      WRITE( LOGUNIT, 92000 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the input profile file, read the number of species and their 
c  names, and list the file on the output log
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENV_DFLT = ' '
      ENV_DESC = 'Input IC Profile file'        
      CALL ENVSTR( IC_PROFILE, ENV_DESC, ENV_DFLT, PROF_FL_NAME, STATUS )

      IF ( STATUS .NE. 0 ) THEN
         MSG = 'No IC profile file assigned to IC_PROFILE'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      END IF

      PFILE = JUNIT()

      OPEN( UNIT = PFILE, FILE = PROF_FL_NAME, ERR = 999 )

      WRITE( LOGUNIT, 92020 ) PROF_FL_NAME( 1:100 )

C Report the 3-line header
      DO N = 1, 3
         READ( PFILE, 94000 ) LINEIN
         WRITE( LOGUNIT, 92040 ) TRIM( LINEIN )
      END DO

      READ( PFILE, * ) NLAYS_IN, NSPCS_IN
!     READ( PFILE, * ) NSPCS_IN, NLAYS_IN
      BACKSPACE PFILE 
      READ( PFILE, 94000 ) LINEIN
      WRITE( LOGUNIT, 92040 ) TRIM( LINEIN )

      READ( PFILE, 94000 ) LINEIN
      WRITE( LOGUNIT, 92040 ) TRIM( LINEIN )

      LERROR = .FALSE.
      IF ( NSPCS_IN .GT. MX_INFL_SP ) THEN
         MSG = 'ERROR: No of layers on profile file exceeds  MXINFL_SP' 
         WRITE( LOGUNIT, 92080  ) MSG
         LERROR = .TRUE.
      END IF

      IF ( NLAYS_IN .GT. MXLAYS3 + 1 ) THEN
         MSG = 'ERROR: No of layers on profile file exceeds MXLAYS + 1' 
         WRITE( LOGUNIT, 92080  ) MSG
         LERROR = .TRUE.
      END IF

      IF ( LERROR ) THEN
         MSG = 'Dimension problem in IC profile file'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE.) 
      END IF

      DO SPC = 1, NSPCS_IN
         READ( PFILE, 94000 ) LINEIN
         WRITE( LOGUNIT, 92040 ) TRIM( LINEIN )
         BACKSPACE PFILE
         READ( PFILE, * ) PROF_SP_NAME( SPC )
      END DO

      WRITE( LOGUNIT, 92060 ) 

      CLOSE( PFILE )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Build a species map and report
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE ( SPMAP( NSPCSD ), SPNAME_OUT( NSPCSD ), STAT = ALST )
      IF ( ALST .NE. 0 ) THEN
         MSG = 'Error allocating SPMAP or SPNAME_OUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      CALL GC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, SPMAP,
     &                 PROF_SP_NAME, SPNAME_OUT )

      CALL NGC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, NSPCS_OUT,
     &                  SPMAP, PROF_SP_NAME, SPNAME_OUT )

      CALL LST_SPC_MAP( LOGUNIT, NSPCS_IN, NSPCS_OUT,
     &                  SPMAP, NML_IC_NAME, PROF_SP_NAME, SPNAME_OUT )
            
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute ICs from the input CTM conc file(s) and output 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL ICOUT( LOGUNIT,
     &            SDATE, STIME,
     &            NSPCS_IN, NSPCS_OUT, SPMAP,
     &            PROF_FL_NAME,
     &            PROF_SP_NAME,
     &            SPNAME_OUT )

      RETURN

999   CONTINUE

      MSG = 'Could not open ' // TRIM( IC_PROFILE )
      CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 ) 

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  Input file section '
     &         / 1X, 79( '#' )
     &        // 5X, 'Initial Concentrations from an input IC profile ',
     &               'file.' )

92020 FORMAT( / 5X, 'Profile file name: ', A 
     &        / 5X, 'File contents: '
     &        / 1X )

!2040 FORMAT(  5X, A100 )
92040 FORMAT(  5X, A )

92060 FORMAT( / 5X, 'End of file contents.')

92080 FORMAT( //1X, A )

92200 FORMAT( //5X, 'Profile concentration units assumed to be ppm' ) 

92220 FORMAT( //5X, 'Profile concentration units assumed to be ppb' ) 

94000 FORMAT( A )

      END
