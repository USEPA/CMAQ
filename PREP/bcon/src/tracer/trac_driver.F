
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
C $Header: /project/yoj/arc/BCON/src/tracer/trac_driver.F,v 1.2 2011/10/21 16:52:37 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE TRAC_INBNDY( LOGUNIT, SDATE, STIME, RUNLEN )   

C***********************************************************************
 
C Function: Call the routine to compute catalogued special tracer species
C           BCs and write the Models-3 output file
              
C Preconditions: None
  
C Key Subroutines/Functions Called:
 
C Revision History:
C    Prototype created by Jerry Gipson, January, 1998          
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    19 Jul 11 J.Young: Convert for Namelist redesign + M3UTILIO
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
                    
C***********************************************************************

      USE HGRD_DEFN    ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN    ! vertical layer specifications
      USE M3UTILIO     ! IOAPI
      USE BC_PARMS     ! BCON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments: 
      INTEGER LOGUNIT                 ! Unit number for output log
      INTEGER SDATE                   ! Date for IC Output
      INTEGER STIME                   ! Time for IC output
      INTEGER RUNLEN                  ! No. of hours for BC output

C Parameters: None

C External Functions: 
      INTEGER FINDEX    ! Finds the index of a number in a list 
      
C Local Variables:
      CHARACTER( 16 ) :: BLANK  = ' '     ! String of blanks
      CHARACTER( 80 ) :: MSG              ! Log message
      CHARACTER( 16 ) :: PNAME = 'TRAC_INBNDY' ! Procedure name
      CHARACTER( 16 ) :: SUROUT           ! Surrogate output to log
      CHARACTER( 16 ) :: VNAME            ! Output variable name

      CHARACTER( 16 ) :: SPNAME_OUT( NTRACERS )    ! Output tracer names
      CHARACTER( 16 ) :: UNITS_OUT( NTRACERS )    ! Output tracer units

      CHARACTER( 16 ) :: TR_FNAME( MXCTMS )        ! Output file name
                            
      INTEGER FLN          ! Output file number
      INTEGER ICBC_IND     ! Index of surrogate in TR_ICBC
      INTEGER N            ! Loop index
      INTEGER NSP          ! Counter
      INTEGER NSPCS_OUT    ! Number of IC species on output file(s)
      INTEGER TRIND        ! Index for catalogued tracer
      INTEGER TSTEP        ! Time step for output file
      INTEGER ALST         ! Allocate status
      INTEGER :: VTYPE_OUT( NTRACERS )  ! Output tracer variable type

!     REAL    :: TCOUT( NBNDY,NLAYS )   ! Output tracer concs
      REAL, ALLOCATABLE :: TCOUT( :,: )   ! Output tracer concs

C***********************************************************************

      WRITE( LOGUNIT, 92000 )
 
      NSPCS_OUT = NTRACERS
      DO N = 1, NSPCS_OUT
         SPNAME_OUT( N ) = TRAC_NAME( N )
         VTYPE_OUT( N ) = M3REAL
         UNITS_OUT( N ) = "N/A"
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  open the Models3 IC output file(s)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      TSTEP = 0
      CALL OPN_BC_FILE( LOGUNIT, SDATE, STIME, TSTEP,
     &                  NSPCS_OUT, SPNAME_OUT, VTYPE_OUT, UNITS_OUT, TRAC_DESC,
     &                  TR_FNAME, 1 )  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  compute the BCs and then write the ooutput file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE ( TCOUT( NBNDY,NLAYS ), STAT = ALST )
      IF ( ALST .NE. 0 ) THEN
         MSG = 'Error allocating TCOUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      DO N = 1, NSPCS_OUT

         FLN = ( N - 1 ) / MXVARS3 + 1
         VNAME = SPNAME_OUT( N )

         CALL TRAC_BC( VNAME, TCOUT )

         IF ( .NOT. WRITE3( TR_FNAME( FLN ), VNAME, SDATE, STIME,
     &                      TCOUT ) ) THEN
            MSG =  'Could not WRITE species ' //  VNAME // 
     &             'to file ' // TR_FNAME( FLN ) 
            CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT1 )
         END IF

      END DO

      RETURN

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  TRACER species section '
     &         / 1X, 79( '#' )
     &        // 5X, 'The tracer species module has been invoked; ',
     &               'BCs will be generated for tracer species ONLY.' )

      END
