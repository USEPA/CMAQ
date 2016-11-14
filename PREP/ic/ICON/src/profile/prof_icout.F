
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
C $Header: /project/yoj/arc/ICON/src/profile/prof_icout.F,v 1.2 2011/10/21 16:41:56 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE ICOUT( LOGUNIT,
     &                  SDATE, STIME,
     &                  NSPCS_IN, NSPCS_OUT, SPMAP,
     &                  PROF_FL_NAME,
     &                  PROF_SP_NAME,
     &                  SPNAME_OUT )

C***********************************************************************
 
C  Function: Reads the input profile concentration file and opens and
C            writes the output IC file
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: OPN_IC_FILE
C                                    PROF_VINTERP   
 
C  Revision History:
C  Prototype created by Jerry Gipson, January, 1998
C  Modified April 1998 by JG to remove ppb option for 
C     input profile file
C  Modified 4/22/99 by JG to provide default treatment
C     for missing SDATE and STIME
C  Modified 04/20/00 by David Wong, LM
C     -- enable parallelization
C  01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C  12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C  06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
 
C***********************************************************************

      USE HGRD_DEFN    ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN    ! vertical layer specifications
      USE UTILIO_DEFN
      USE IC_PARMS     ! ICON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments: 
      INTEGER :: LOGUNIT        ! Unit number for output log
      INTEGER :: SDATE          ! Date for IC Output
      INTEGER :: STIME          ! Time for IC output
      INTEGER :: NSPCS_IN       ! Total No. of species in input conc file(s)
      INTEGER :: NSPCS_OUT      ! Number of IC species on output file(s)
      INTEGER :: SPMAP( * )     ! Map to input file species from IC species
      CHARACTER( * ) :: PROF_FL_NAME        ! Input profile file name
      CHARACTER( * ) :: PROF_SP_NAME( * )   ! Profile species names
      CHARACTER( * ) :: SPNAME_OUT( * )     ! Ouput file species names

C Parameters: None

C Local Variables:
      CHARACTER( 80 ) :: MSG = ' '            ! Log message
      CHARACTER( 16 ) :: PNAME = 'PROF_ICOUT' ! Program Name
      CHARACTER( 16 ) :: PROF_SP              ! Species name on profile file
      CHARACTER( 16 ) :: VNAME                ! Species name on CTM conc file
      CHARACTER( 16 ) :: IC_FNAME( MXCTMS )   ! Logical names of IC Output file(s) 
      INTEGER  C             ! Column loop indices
      INTEGER  FLN           ! IC output file number
      INTEGER  IND           ! Array indices for species
      INTEGER  L             ! Layer loop index
      INTEGER  N, SPC        ! Loop indices for species
      INTEGER  NLEVS_IN      ! No. of layers in input conc file
      INTEGER  PFILE         ! Unit number of profile file   
      INTEGER  R             ! Row loop indices
      INTEGER  STATUS        ! Status code

      LOGICAL  LNEG          ! Flag for negative concentrations

      REAL :: VGLVS_IN( MXLAYS3 + 1 )       ! Input vertical levels
      REAL :: COUT( NCOLS,NROWS,NLAYS )     ! Substituted output IC conc
      REAL :: INPROF( MXLAYS3,MX_INFL_SP )  ! Input conc profiles  
      REAL :: VIPROF( NLAYS,MX_INFL_SP )    ! Vertically interpolated profiles
                            
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read the input profile file data  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PFILE = JUNIT()

      OPEN( UNIT = PFILE, FILE = PROF_FL_NAME, ERR = 999 )

C Consume the 3-line header
      DO N = 1, 3
         READ( PFILE,94000 ) 
      END DO

C Get file info (no. of species already retrieved)
      READ( PFILE,* ) NLEVS_IN, SPC, ( VGLVS_IN( L ), L = 1, NLEVS_IN + 1 )
!     READ( PFILE,* ) SPC, NLEVS_IN, ( VGLVS_IN( L ), L = 1, NLEVS_IN + 1 )

C Consume a date and time line
      READ( PFILE,* ) 

C Get file data
      DO SPC = 1, NSPCS_IN
         READ( PFILE, * ) PROF_SP, ( INPROF( L,SPC ), L = 1, NLEVS_IN )
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Possible vertical interpolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL PROF_VINTERP( LOGUNIT, NSPCS_IN, NLEVS_IN, VGLVS_IN, INPROF,
     &                   VIPROF )      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the Models3 IC output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SDATE = 0
      STIME = 0
      CALL OPN_IC_FILE( LOGUNIT, SDATE, STIME, NSPCS_OUT, SPNAME_OUT,
     &                  IC_FNAME, 1 )  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the output IC concentrations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LNEG = .FALSE.

      DO N = 1, NSPCS_OUT

         FLN = ( N - 1 ) / MXVARS3 + 1
         VNAME = SPNAME_OUT( N )
           
         IND = SPMAP( N )

         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  COUT( C,R,L ) = VIPROF( L,IND )
                  IF ( COUT( C,R,L ) .LT. 0.0 ) LNEG = .TRUE.
               END DO
            END DO
         END DO
       
         IF ( .NOT. WRITE3( IC_FNAME( FLN ), VNAME, SDATE, STIME,
     &                     COUT( 1,1,1 ) ) ) THEN
            MSG =  'Could not WRITE species ' //  VNAME // 
     &             'to file ' // IC_FNAME( FLN ) 
            CALL M3ERR( PNAME, SDATE, STIME, MSG, .TRUE. )
         END IF

      END DO

      IF ( LNEG ) THEN
         MSG = 'Negative ICs output'
         STATUS = 1
         CALL M3EXIT( PNAME, 0, 0,' ', STATUS )
      END IF

      RETURN

999   CONTINUE 

      MSG = 'Could not open file ' // PROF_FL_NAME
      STATUS = 2
      CALL M3EXIT( PNAME, 0, 0, ' ', STATUS )

C************************* FORMAT STATEMENTS ***************************

94000 FORMAT( 1X )

      END
