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


      PROGRAM SITECMP_DAILYO3

C**********************************************************************
C
C  program to generate a table comparing AIRS and modeled ozone values
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

C..INCLUDES:  
C      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C      INCLUDE SUBST_IOFDESC     ! IOAPI file description
C      INCLUDE SUBST_IODECL      ! IOAPI declarations
      
C..ARGUMENTS:

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message

      INTEGER   N                  ! Loop index

C**********************************************************************
      DATA  PNAME       / 'SITECMP_DAILYO3'  /

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Models-3 file(s) to process and the other environment   
c  variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL OPEN_M3FILES
      !Write(*,'('' M3FILES opened'')')

      Call GET_SPECS
      !Write(*,'('' Species defined'')')

      CALL GET_ENVS
      !Write(*,'('' Environment values defined'')')

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
      !Write(*,'('' M3FILE grid and time steps defined'')')

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the site data 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL LOAD_SITES
      CALL SET_SITE_LOC
      !Write(*,'('' Sites read'')')

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call the routine to extract the data and generate the output file 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      if( COMPUTE_SUM06 ) Then
!        Write(*,'('' extracting daily SUM06 values'')')
!        Call process3
!        CALL EXIT( 0 )
!        endif
        
!      if( COMPUTE_MAX ) Then
        Write(*,'('' extracting daily maximum values'')')
        Call process
        WRITE( LOGUNIT, 92020 )
     &    '>>---->  Program ' // PNAME // ' completed successfully  <----<<' 
        CALL EXIT( 0 )
!        endif

!      Write(*,'('' extracting hourly values'')')
!      Call process1
!      CALL EXIT( 0 )
   
      WRITE( LOGUNIT, 92020 )
     &    '>>---->  Program ' // PNAME // ' completed successfully  <----<<' 


      CALL EXIT( 0 )

C******************  FORMAT  STATEMENTS   ******************************


92020  FORMAT( //5X, A, // )

       END
       
       
