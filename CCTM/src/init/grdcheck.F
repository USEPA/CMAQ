
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
C $Header: /project/yoj/arc/CCTM/src/init/yamo/grdcheck.F,v 1.3 2011/10/21 16:11:24 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE GRDCHECK ( FNAME, LOK )

C----------------------------------------------------------------------
C Function: To check files and COORD.EXT for consistency
 
C Preconditions: None
 
C Key Subroutines/Functions Called:
 
C Revision History:
C    Prototype created by Jerry Gipson, September, 1997
C    eliminate EMISLYRS, Jeff, Oct 97
C    2 October, 1998 by Al Bourgeois at LM: parallel implementation
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C    6 Sep 06 J.Young: ensure LNLAYSOK is initialized - Michael Bane's suggestion
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN

      IMPLICIT NONE 

C..INCLUDES:
      INCLUDE SUBST_FILES_ID    ! file name parameters
       
C..ARGUMENTS:
      CHARACTER*(*) FNAME     ! File Name
      LOGICAL LOK             ! Flag to indicate error found

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      
C..SCRATCH LOCAL VARIABLES:
      CHARACTER*20 CHR1      ! Value of variable 1 in character data
      CHARACTER*20 CHR2      ! Value of variable 1 in character data
      CHARACTER*80 MSG       ! Message

      INTEGER LAY       ! Loop index for layers
      INTEGER NCOLS_DOT ! No. of columns for a dot file
      INTEGER NLAYS_FL  ! No. of layers expected on 3D file
      INTEGER NLAYS2CK  ! No. of layers to check vertical levels
      INTEGER NROWS_DOT ! No. of rows for a dot file

      LOGICAL LDIFF     ! Flag for difference in vertical levels
      LOGICAL LNLAYSOK  ! Flag to indicate no. of vert. layers OK

      REAL*8 XORIG_FL   ! Computed XORIG for dot file
      REAL*8 YORIG_FL   ! Computed YORIG for dot file

C----------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Check horizontal grid structure
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifndef mpas
C...For DOT files only
      IF(INDEX( FNAME, '_DOT_' ) .NE. 0 ) THEN

         IF( GL_NCOLS + 1 .NE. NCOLS3D ) THEN
            NCOLS_DOT = GL_NCOLS + 1
            CALL INTDIFF( 'GL_NCOLS', NCOLS3D, NCOLS_DOT )
            LOK = .FALSE.
         END IF

         IF( GL_NROWS + 1 .NE. NROWS3D ) THEN
            NROWS_DOT = GL_NROWS + 1
            CALL INTDIFF( 'GL_NROWS', NROWS3D,  NROWS_DOT )
            LOK = .FALSE.
         END IF

C...Skip PDM files
      ELSEIF( INDEX( FNAME, 'PDM_' ) .NE. 0) THEN

C...Skip PING files
      ELSEIF( INDEX( FNAME, '_PING_' ) .NE. 0) THEN

C...Skip IPR files
      ELSEIF( INDEX( FNAME, '_IPR_' ) .NE. 0) THEN

C...Skip IRR files
      ELSEIF( INDEX( FNAME, '_IRR_' ) .NE. 0) THEN

C...All other files
      ELSE

         IF( GL_NCOLS .NE. NCOLS3D ) THEN
            CALL INTDIFF( 'GL_NCOLS', NCOLS3D, GL_NCOLS )
            LOK = .FALSE.
         END IF

         IF( GL_NROWS .NE. NROWS3D ) THEN
            CALL INTDIFF( 'GL_NROWS', NROWS3D, GL_NROWS )
            LOK = .FALSE.
         END IF
 
      END IF

C...Check Number of vertical layers only for true 3D files

      LNLAYSOK = .TRUE.

      IF( INDEX( FNAME,   'PDM_' ) .EQ. 0 .AND.
     &    INDEX( FNAME, '_PING_' ) .EQ. 0 .AND. NLAYS3D .GT. 1 )  THEN

         IF( INDEX( FNAME,  'EMIS' ) .GT. 0 .OR.
     &       INDEX( FNAME, 'MEPSE' ) .GT. 0 .OR.
     &       INDEX( FNAME, '_IPR_' ) .GT. 0 .OR.
     &       INDEX( FNAME, '_IRR_' ) .GT. 0 ) THEN
!           NLAYS_FL = EMISLYRS
            NLAYS_FL = NLAYS3D   ! don't check this file
         ELSE
            NLAYS_FL = NLAYS
         END IF

         IF( NLAYS_FL .NE. NLAYS3D ) THEN
            CALL INTDIFF( 'NLAYS', NLAYS3D, NLAYS_FL )
            LOK = .FALSE.
            LNLAYSOK = .FALSE.
         END IF

         IF( NTHIK .NE. NTHIK3D ) THEN
            CALL INTDIFF( 'NTHIK', NTHIK3D, NTHIK )
            LOK = .FALSE.
         END IF
             
      END IF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Check Map projection
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( GDTYP_GD .NE. GDTYP3D ) THEN
         CALL INTDIFF( 'GDTYP', GDTYP_GD, GDTYP3D )
         LOK = .FALSE.
      END IF

C...Check P_ALPHA only for Lambert, Mercator, Stereographic and UTM
      IF( GDTYP_GD .GE. 2 .AND. GDTYP_GD .LE. 5 ) THEN      
         WRITE( CHR1, 94020 ) P_ALP3D
         WRITE( CHR2, 94020 ) P_ALP_GD
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'P_ALP', CHR1, CHR2 )
            LOK = .FALSE.
        END IF
      END IF

C...Check P_BETA only for Lambert, Mercator, and Stereographic
      IF( GDTYP_GD .GE. 2 .AND. GDTYP_GD .LE. 4 ) THEN      
         WRITE( CHR1, 94020 ) P_BET3D
         WRITE( CHR2, 94020 ) P_BET_GD
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'P_BET', CHR1,  CHR2 )
            LOK = .FALSE.
        END IF
      END IF

C...Check P_GAMMA only for Lambert, Mercator, and Stereographic
      IF( GDTYP_GD .GE. 2 .AND. GDTYP_GD .LE. 4 ) THEN      
         WRITE( CHR1, 94020 ) P_GAM3D
         WRITE( CHR2, 94020 ) P_GAM_GD
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'P_GAM', CHR1,  CHR2 )
            LOK = .FALSE.
         END IF
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Check for horizontal grid location and size
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C...Check XORG & YORG only for Lambert, Mercator, Stereographic and UTM
      IF( GDTYP_GD .GE. 2 .AND. GDTYP_GD .LE. 5 ) THEN      
         IF( INDEX( FNAME, '_DOT_' ) .GT. 0 ) THEN
            XORIG_FL = XORIG_GD - 0.5 * XCELL_GD
            YORIG_FL = YORIG_GD - 0.5 * YCELL_GD
         ELSE
            XORIG_FL = XORIG_GD 
            YORIG_FL = YORIG_GD 
         END IF
         WRITE( CHR1, 94020 ) XORIG3D
         WRITE( CHR2, 94020 ) XORIG_FL
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'XORIG', CHR1,  CHR2 )
            LOK = .FALSE.
         END IF
         WRITE( CHR1, 94020 ) YORIG3D
         WRITE( CHR2, 94020 ) YORIG_FL
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'YORIG', CHR1,  CHR2 )
            LOK = .FALSE.
         END IF
      END IF

C...Check XCENT & YCENT only for Lambert, Mercator, and Stereographic
      IF( GDTYP_GD .GE. 2 .AND. GDTYP_GD .LE. 4 ) THEN      
         WRITE( CHR1, 94020 ) XCENT3D
         WRITE( CHR2, 94020 ) XCENT_GD
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'XCENT', CHR1,  CHR2 )
            LOK = .FALSE.
         END IF

         WRITE( CHR1, 94020 ) YCENT3D
         WRITE( CHR2, 94020 ) YCENT_GD
         IF( CHR1 .NE. CHR2 ) THEN
            CALL CHRDIFF( 'YCENT', CHR1,  CHR2 )
            LOK = .FALSE.
         END IF
      END IF

      WRITE( CHR1, 94020 ) XCELL3D
      WRITE( CHR2, 94020 ) XCELL_GD
      IF( CHR1 .NE. CHR2 ) THEN
         CALL CHRDIFF( 'XCELL', CHR1,  CHR2 )
         LOK = .FALSE.
      END IF

      WRITE( CHR1, 94020 ) YCELL3D
      WRITE( CHR2, 94020 ) YCELL_GD
      IF( CHR1 .NE. CHR2 ) THEN
         CALL CHRDIFF( 'YCELL', CHR1,  CHR2 )
         LOK = .FALSE.
      END IF
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Check vertical structure (only for "true" 3D files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( INDEX( FNAME,   'PDM_' ) .EQ. 0 .AND.
     &    INDEX( FNAME, '_PING_' ) .EQ. 0 .AND.
     &    NLAYS3D .GT. 1 .AND. LNLAYSOK )  THEN

         IF( VGTYP_GD .NE. VGTYP3D ) THEN
            CALL INTDIFF( 'VGTYP', VGTYP3D, VGTYP_GD )
            LOK = .FALSE.
         END IF

C..Check for vtop for sigma-p coordinates only
         IF( VGTYP3D .GE. 1 .AND. VGTYP3D .LE. 2 ) THEN  
            WRITE( CHR1, 94020 ) VGTOP3D
            WRITE( CHR2, 94020 ) VGTOP_GD
            IF( CHR1 .NE. CHR2 ) THEN
               CALL CHRDIFF( 'VGTOP', CHR1,  CHR2 )
               LOK = .FALSE.
            END IF
         END IF

C..Check vertical levels for 3D files only

         LDIFF = .FALSE.

         IF( INDEX( FNAME, 'EMIS' ) .GT. 0 ) THEN
!           NLAYS2CK = EMISLYRS
            NLAYS2CK = NLAYS3D
         ELSE
            NLAYS2CK = NLAYS
         END IF

         DO LAY = 1 , NLAYS2CK
            WRITE( CHR1, 94020 ) VGLVS3D( LAY )
            WRITE( CHR2, 94020 ) VGLVS_GD( LAY )
            IF( CHR1 .NE. CHR2 ) LDIFF = .TRUE.
         END DO

         IF( LDIFF ) THEN    
            MSG = '    Inconsistent value for vertical level'
            CALL M3MESG( MSG )
            LOK = .FALSE.
         END IF

      END IF
#endif

      RETURN

94020 FORMAT( 1PE20.4 )

      END
         
