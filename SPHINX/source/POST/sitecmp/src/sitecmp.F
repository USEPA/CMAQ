
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
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/sitecmp.F,v 1.4 2011/10/21 14:41:34 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      PROGRAM sitecmp

C**********************************************************************
C
C  PROGRAM: To compare model concentrations at an observed site
C
C
C**********************************************************************
      USE M3FILES
      USE ENV_VARS
      USE GRID_DATA
      USE TIME_STEP
      USE SITE_DATA
      USE SPECIES_DEF

      USE M3UTILIO

      IMPLICIT NONE 

C..ARGUMENTS:

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message
      CHARACTER*5     Version      ! Version number
      INTEGER   N                  ! Loop index

C**********************************************************************
      DATA  PNAME       / 'sitecmp'  /
      DATA  Version     / '1.0'  /

C... write program name and version number
      WRITE( LOGUNIT, 92020 )
     &    '>>---->  Program ' // TRIM(PNAME) // ' (Version ' // TRIM(Version) // ')  <----<<' 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Models-3 file(s) to process and the other environment   
c  variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL OPEN_M3FILES

      Call GET_SPECS
      CALL GET_ENVS

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check the file(s) for consistency and make sure the requested   
c  species is on the file(s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL CK_M3FLS()

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the grid definition and the tsteps from the M3 files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GET_M3GRID

      CALL GET_TSTEPS

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the site data 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL FL_TYP1

      CALL SET_SITE_LOC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call the routine to extract the data and generate the output file 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL PROCESS

      WRITE( LOGUNIT, 92020 )
     &    '>>---->  Program ' // TRIM(PNAME) // ' completed successfully  <----<<' 


      CALL EXIT( 0 )

C******************  FORMAT  STATEMENTS   ******************************


92020  FORMAT( //5X, A, // )

       END
       
       
