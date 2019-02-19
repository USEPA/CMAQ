
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
C $Header: /project/yoj/arc/ICON/src/m3conc/m3_driver.F,v 1.2 2011/10/21 16:41:54 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE M3_INIC( LOGUNIT, SDATE, STIME )

C***********************************************************************
 
C  Function: Opens the Models-3 CTM conc file(s) and checks for 
C            data consistency, and calls the routine to 
C            compute the ICs and write the output IC file  
              
C  Preconditions: None
C 
C  Key Subroutines/Functions Called: CK_CTM_FLS
C                                    GC_SPC_MAP
C                                    NGC_SPC_MAP
C                                    LST_SPC_MAP
C                                    ICOUT  
 
C  Revision History: Prototype created by Jerry Gipson, January, 1998          
C      12/12/00 David Wong, LM
C        -- initialized N_PING_FLS = 0
C      01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C      12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C      06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C      07 Jun 11 J.Young: Removed RADM2<->CB4 mechanism conversion feature
C      13 Jul 11 J.Young: Convert for Namelist redesign + UTILIO_DEFN
C      21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C      02 Nov 18 S.Roselle: Removed species mapping
C      06 Nov 18 S.Roselle: Replaced UTILIO_DEFN with M3UTILIO

C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE IC_PARMS    ! ICON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments:
      INTEGER LOGUNIT                 ! Unit number for output log
      INTEGER SDATE                   ! Date for IC output
      INTEGER STIME                   ! Time for IC output

C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER(  16 ) :: ENV_DFLT       ! Environment variable default value
      CHARACTER(  80 ) :: ENV_DESC       ! Environment variable description
      CHARACTER(  16 ) :: FL_NAME        ! Input CTM file name
      CHARACTER(  80 ) :: MSG            ! Log message
      CHARACTER(  16 ) :: PNAME = 'M3_INIC' ! Program name
      CHARACTER( 256 ) :: RET_VAL        ! Returned value of environment variable
      CHARACTER(  80 ) :: VARDESC        ! environment variable description

      CHARACTER(  16 ) :: CTM_FL_NAME( MXCTMS )           ! Name of CTM file
      CHARACTER(  16 ) :: INFL_SP_NAME( MXCTMS*MXVARS3 )  ! Names of CTM species
      CHARACTER(  16 ) :: UNITS_IN( MXCTMS*MXVARS3 ) ! Units for CTM species
      CHARACTER(  80 ) :: VDESC_IN( MXCTMS*MXVARS3 ) ! Variable description for CTM species

      INTEGER :: VTYPE_IN( MXCTMS*MXVARS3 ) ! variable type for CTM species

      INTEGER N            ! File loop index
      INTEGER N_GC_OUT     ! No. of GC species
      INTEGER N_CTM_FLS    ! Number of input CTM files
      INTEGER NCOLS_IN     ! No. of columns in input conc file
      INTEGER NLAYS_IN     ! No. of layers in input conc file
      INTEGER NROWS_IN     ! No. of rows in input conc file
      INTEGER NSPCS_IN     ! CTM files species counter
      INTEGER NSPCS        ! Number of NML species
      INTEGER STATUS       ! Status code
      INTEGER V            ! Variable loop index
      INTEGER L            ! Layer loop index
      INTEGER ALST         ! Allocate status

      LOGICAL LCHKL            ! Flag for layer consistency check
      LOGICAL LCHKD            ! Flag for met file checked

C***********************************************************************

      WRITE( LOGUNIT, 92000 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine the number of input CTM conc files that need to be read
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      N_CTM_FLS = 0

      WRITE( LOGUNIT, 92020 )

      DO N = 1, MXCTMS

         WRITE( FL_NAME, '( ''CTM_CONC_'', I1 )' ) N
         WRITE( ENV_DESC, '( ''CTM Concentration file no. '', I1 )' ) N 
         ENV_DFLT = ' '        
         CALL ENVSTR( FL_NAME, ENV_DESC, ENV_DFLT, RET_VAL, STATUS)

         IF ( STATUS .EQ. 0 ) THEN
            N_CTM_FLS = N_CTM_FLS + 1
            CTM_FL_NAME( N_CTM_FLS ) = FL_NAME
         END IF
     
      END DO

      IF ( N_CTM_FLS .EQ. 0 ) THEN
         MSG = 'No CTM CONC files found'
         CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the input Models-3 CTM concentration file(s) and check that the
c  required time step is available on each file; save the species 
c  names on each file in one contiguous array
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NSPCS_IN = 0

      DO N = 1, N_CTM_FLS

         IF ( .NOT. OPEN3( CTM_FL_NAME( N ), FSREAD3, PNAME ) ) THEN
            MSG = 'Could not open ' // CTM_FL_NAME( N ) // ' file'
            CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF

         IF ( .NOT. DESC3( CTM_FL_NAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // CTM_FL_NAME( N ) 
     &            // ' file'
            CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF

         DO V = 1, NVARS3D
            NSPCS_IN = NSPCS_IN + 1
            INFL_SP_NAME( NSPCS_IN ) = VNAME3D( V )
            VTYPE_IN( NSPCS_IN ) = VTYPE3D( V )
            UNITS_IN( NSPCS_IN ) = UNITS3D( V )
            VDESC_IN( NSPCS_IN ) = VDESC3D( V )
         END DO

      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check multiple input CTM files for consistency & set input domain
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( N_CTM_FLS .GT. 1 ) CALL CK_CTM_FLS( LOGUNIT, N_CTM_FLS,
     &                                         CTM_FL_NAME )

      IF ( .NOT. DESC3( CTM_FL_NAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // CTM_FL_NAME( N ) 
     &         // ' file'
         CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 )
      END IF
      NCOLS_IN = NCOLS3D
      NROWS_IN = NROWS3D
      NLAYS_IN = NLAYS3D

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Check MET_CRO_3D_CRS (for input CCTM file) and MET_CRO_3D_FIN (for
c   output IC file) if vertical grid types are different
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LCHKD = .FALSE.

      IF ( VGTYP_GD .NE. VGTYP3D .OR. VGTOP_GD .NE. VGTOP3D ) THEN 
         LCHKD = .TRUE.
         CALL CK_CTMMET_FL( LOGUNIT, SDATE, STIME, CTM_FL_NAME )
         CALL CK_ICMET_FL( LOGUNIT, SDATE, STIME  )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute and output ICs from the input CTM conc file(s)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL M3_ICOUT( LOGUNIT,
     &               N_CTM_FLS,
     &               SDATE, STIME,
     &               NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &               CTM_FL_NAME,
     &               INFL_SP_NAME, VTYPE_IN, UNITS_IN, VDESC_IN )

      RETURN

C************************* FORMAT Statements ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  Input file section '
     &         / 1X, 79( '#' )
     &        // 5X, 'Initial Concentrations from a CMAQ CTM ',
     &               'concentration file.' )

92020 FORMAT( // 5X, 'Input CMAQ CTM file names: ' )

      END
