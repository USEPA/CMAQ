
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
C $Header: /project/yoj/arc/ICON/src/tracer/trac_driver.F,v 1.2 2011/10/21 16:41:57 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE TRAC_INIC ( LOGUNIT )

C***********************************************************************
 
C Function: Call the routine to compute catalogued special tracer species
C             ICs and write the Models-3 output file
              
C Preconditions: None
  
C Key Subroutines/Functions Called:
 
C Revision History:
C  Prototype created by Jerry Gipson, January, 1998          
C  01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C  19 Jul 11 J.Young: Convert for Namelist redesign + UTILIO_DEFN
C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C  05 Nov 18 S.Roselle: UTILIO_DEFN replaced with M3UTILIO

C***********************************************************************

      USE HGRD_DEFN    ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN    ! vertical layer specifications
      USE M3UTILIO     ! IOAPI
      USE IC_PARMS     ! ICON parameters

      IMPLICIT NONE     

C Arguments: 
      INTEGER, INTENT( IN ) :: LOGUNIT    ! Unit number for output log

C Parameters: None

C External Functions: None
      
C Local Variables:
      CHARACTER( 80 ) :: MSG              ! Log message
      CHARACTER( 16 ) :: PNAME = 'TRAC_INIC'  ! Procedure name
      CHARACTER( 16 ) :: VNAME            ! Output variable name

      CHARACTER( 16 ), ALLOCATABLE :: SPNAME_OUT( : )    ! Output tracer names
      CHARACTER( 16 ), ALLOCATABLE :: UNITS_OUT( : )     ! Output tracer units

      CHARACTER( 16 ), ALLOCATABLE :: TR_FNAME( : ) ! Output file name
                            
      INTEGER FLN          ! Output file number
      INTEGER N            ! Loop index
      INTEGER NSPCS_OUT    ! Number of IC species on output file(s)
      INTEGER ALLOCSTAT    ! Status returned from array allocation
      INTEGER SDATE        ! Date for IC Output
      INTEGER STIME        ! Time for IC output
      INTEGER TSTEP        ! Timestep for IC Output

      INTEGER, ALLOCATABLE :: VTYPE_OUT( : )    ! Output tracer variable type

      REAL, ALLOCATABLE :: TCOUT( :,:,: )  ! Output tracer concs

      INTERFACE

         SUBROUTINE OPN_IC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_OUT,
     &                            SPNAME_OUT, VTYPE_OUT, UNITS_OUT,
     &                            VDESC_OUT, IC_FNAME, RINDX )
            CHARACTER( 16 ), INTENT( OUT ) :: IC_FNAME( : )
            CHARACTER( 16 ), INTENT( IN )  :: SPNAME_OUT( : )
            CHARACTER( 16 ), INTENT( IN )  :: UNITS_OUT( : )
            CHARACTER( 80 ), INTENT( IN )  :: VDESC_OUT( : )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: NSPCS_OUT
            INTEGER, INTENT( IN ) :: RINDX
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: TSTEP
            INTEGER, INTENT( IN ) :: VTYPE_OUT( : )
         END SUBROUTINE OPN_IC_FILE

         SUBROUTINE TRAC_IC ( TRNAME, TCOUT )
            CHARACTER( 16 ), INTENT( IN ) :: TRNAME
            REAL, INTENT( OUT ) :: TCOUT( :,:,: )
         END SUBROUTINE TRAC_IC

      END INTERFACE

C***********************************************************************

      WRITE( LOGUNIT, 92000 )
 
      ALLOCATE( TR_FNAME( MXCTMS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating TR_FNAME'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( UNITS_OUT( NTRACERS ),
     &          SPNAME_OUT( NTRACERS ),
     &          VTYPE_OUT( NTRACERS ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating UNITS_OUT, SPNAME_OUT, VTYPE_OUT'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      NSPCS_OUT = NTRACERS
      DO N = 1, NSPCS_OUT
         SPNAME_OUT( N ) = TRAC_NAME( N )
         VTYPE_OUT( N ) = M3REAL
         UNITS_OUT( N ) = "N/A"
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  open the Models3 IC output file(s)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SDATE = 0
      STIME = 0
      TSTEP = 0
      CALL OPN_IC_FILE( LOGUNIT, SDATE, STIME, TSTEP,
     &                  NSPCS_OUT, SPNAME_OUT, VTYPE_OUT, UNITS_OUT, TRAC_DESC,
     &                  TR_FNAME, 1 )  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  compute the ICs and write the ooutput file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE ( TCOUT( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Error allocating TCOUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      DO N = 1, NSPCS_OUT

         FLN = ( N - 1 ) / MXVARS3 + 1
         VNAME = SPNAME_OUT( N )

         CALL TRAC_IC ( VNAME, TCOUT )

         IF ( .NOT. WRITE3( TR_FNAME( FLN ), VNAME, SDATE, STIME,
     &                      TCOUT ) ) THEN
            MSG = 'Could not WRITE species ' // TRIM( VNAME )
     &          // 'to file ' // TR_FNAME( FLN ) 
            CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT1 )
         END IF

      END DO

      RETURN

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  TRACER species section '
     &         / 1X, 79( '#' )
     &        // 5X, 'The tracer species module has been invoked; ',
     &               'ICs will be generated for tracer species ONLY.' )

      END
