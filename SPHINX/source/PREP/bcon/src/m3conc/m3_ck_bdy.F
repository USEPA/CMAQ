
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
C $Header: /project/yoj/arc/BCON/src/m3conc/m3_ck_bdy.F,v 1.2 2011/10/21 16:52:34 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE CK_BDY_FL ( LOGUNIT, SDATE, STIME, TSTEP, NSTEPS )

C***********************************************************************
 
C Function: To check input MET_BDY_3D file for consistency with
C           the LAYER_FILE/VGRD_DEFN data
              
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C    Prototype created by Jerry Gipson, May, 1999
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    01/05/05 J.Young: vert dyn alloc - Use VGRD_DEFN
C    13 JuL 11 J.Young: Replaced I/O API include files with M3UTILIO
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
C    10 June 19 F. Sidi : Commented Out LOGICAL LFIRST & INTEGER N because it is unused
                    
C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE BC_PARMS    ! BCON parameters

      IMPLICIT NONE     

C Include Files: None

C Arguments:
      INTEGER, INTENT( IN ) :: LOGUNIT   ! Unit number for output log
      INTEGER, INTENT( IN ) :: SDATE     ! Start date
      INTEGER, INTENT( IN ) :: STIME     ! Start time
      INTEGER, INTENT( IN ) :: TSTEP     ! Time step
      INTEGER, INTENT( IN ) :: NSTEPS    ! No. of time steps

C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 80 ) :: MSG = ' '    ! Log message
      CHARACTER( 16 ) :: PNAME = 'CK_BDY_FL'   ! Procedure Name
      CHARACTER( 15 ) :: VAL1         ! Character value of real
      CHARACTER( 15 ) :: VAL2         ! Character value of real

      INTEGER JDATE       ! Current date
      INTEGER JTIME       ! Current time
!      INTEGER N
      INTEGER L        ! Loop indices

      LOGICAL LERROR      ! Error Flag
!      LOGICAL LFIRST      ! First time flag
    
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write out report data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MSG = 'Checking MET_BDY_3D_FIN File for consistent ' //
     &      'header data'
      CALL M3MESG ( MSG )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get header data for MET_BDY_3D_FIN file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. OPEN3 ( MET_BDY_3D_FIN, FSREAD3, PNAME ) ) THEN
         MSG = 'Could not open ' // MET_BDY_3D_FIN // ' file'
         CALL M3EXIT ( PNAME, JDATE , JTIME, MSG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3 ( MET_BDY_3D_FIN ) ) THEN
         MSG = 'Could not read DESC of  ' // MET_BDY_3D_FIN 
     &         // ' file'
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check MET_BDY_3D_FIN file for consistency with GRIDDESC
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE.
         
      IF ( FTYPE3D .NE. BNDARY3 ) THEN
         WRITE( LOGUNIT, 94020 ) FTYPE3D, BNDARY3
         LERROR = .TRUE.
      END IF

      IF ( GDTYP3D .NE. GDTYP_GD ) THEN
         WRITE( LOGUNIT, 94200 ) GDTYP3D, GDTYP_GD
         LERROR = .TRUE.
      END IF

      IF ( NCOLS3D .NE. NCOLS ) THEN
          WRITE( LOGUNIT, 94040 ) NCOLS3D, NCOLS
          LERROR = .TRUE.
      END IF

      IF ( NROWS3D .NE. NROWS ) THEN
         WRITE( LOGUNIT, 94060 ) NROWS3D, NROWS
         LERROR = .TRUE.
      END IF

      IF ( NLAYS3D .NE. NLAYS ) THEN
         WRITE( LOGUNIT, 94080 ) NLAYS3D, NLAYS
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) P_ALP3D
      WRITE( VAL2, 94000 ) P_ALP_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94220 ) P_ALP3D, P_ALP_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) P_BET3D
      WRITE( VAL2, 94000 ) P_BET_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94240 ) P_BET3D, P_BET_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) P_GAM3D
      WRITE( VAL2, 94000 ) P_GAM_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94260 ) P_GAM3D, P_GAM_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) XORIG3D
      WRITE( VAL2, 94000 ) XORIG_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94280 ) XORIG3D, XORIG_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) YORIG3D
      WRITE( VAL2, 94000 ) YORIG_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94300 ) YORIG3D, YORIG_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) XCELL3D
      WRITE( VAL2, 94000 ) XCELL_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94320 ) XCELL3D, XCELL_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) YCELL3D
      WRITE( VAL2, 94000 ) YCELL_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94340 ) YCELL3D, YCELL_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) XCENT3D
      WRITE( VAL2, 94000 ) XCENT_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94360 ) XCENT3D, XCENT_GD
         LERROR = .TRUE.
      END IF

      WRITE( VAL1, 94000 ) YCENT3D
      WRITE( VAL2, 94000 ) YCENT_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94380 ) YCENT3D, YCENT_GD
         LERROR = .TRUE.
      END IF
 
      WRITE( VAL1, 94000 ) VGTOP3D
      WRITE( VAL2, 94000 ) VGTOP_GD
      IF ( VAL1 .NE. VAL2 ) THEN
         WRITE( LOGUNIT, 94420 ) VGTOP3D, VGTOP_GD
         LERROR = .TRUE.
      END IF

      DO L = 1 , NLAYS3D + 1
         WRITE( VAL1, 94000 ) VGLVS3D(  L )
         WRITE( VAL2, 94000 ) VGLVS_GD( L )
         IF ( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94440 ) L, VGLVS3D( L ),  L,
     &                              VGLVS_GD( L )      
            LERROR = .TRUE.
         END IF
      END DO
 
      JDATE = SDATE
      JTIME = STIME
!
!      LFIRST = .TRUE.
!      DO N = 1, NSTEPS
!         IF ( .NOT. CHECK3(  MET_BDY_3D_FIN, 'ALL',
!     &                      JDATE, JTIME ) ) THEN
!            LERROR = .TRUE.
!            IF ( LFIRST ) THEN
!               MSG = 'The following time steps are not on '
!     &             // MET_BDY_3D_FIN
!               WRITE( LOGUNIT, 94480 ) MSG
!               LFIRST = .FALSE.
!            END IF
!            WRITE( LOGUNIT,'( I7, ":", I6.7 )' ) JDATE, JTIME
!         END IF
!         CALL NEXTIME ( JDATE, JTIME, TSTEP )
!      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  If inconsistency found, then stop, else return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LERROR ) THEN
         WRITE( LOGUNIT, 94460 )
      ELSE
         MSG = 'MET_BDY_3D_FIN file inconsistent: stopping' 
         CALL M3EXIT ( PNAME, JDATE , JTIME, MSG, XSTAT2 )
      END IF
     
      RETURN

C************************* FORMAT STATEMENTS ***************************

94000 FORMAT( E15.5 )
94020 FORMAT( 10X, 'MET_BDY_3D_FIN FTYPE = ', I3, ' COORD FTYPE', ' = ', I3 )
94040 FORMAT( 10X, 'MET_BDY_3D_FIN NCOLS = ', I3, ' COORD NCOLS', ' = ', I3 )
94060 FORMAT( 10X, 'MET_BDY_3D_FIN NROWS = ', I3, ' COORD NROWS', ' = ', I3 )
94080 FORMAT( 10X, 'MET_BDY_3D_FIN NLAYS = ', I3, ' COORD NLAYS', ' = ', I3 )
94200 FORMAT( 10X, 'MET_BDY_3D_FIN GDTYP = ', I3, ' COORD GDTYP', ' = ', I3 )
94220 FORMAT( 10X, 'MET_BDY_3D_FIN P_ALP = ', 1PE12.5, ' COORD  P_ALP', ' = ', 1PE12.5 )
94240 FORMAT( 10X, 'MET_BDY_3D_FIN P_BET = ', 1PE12.5, ' COORD  P_BET', ' = ', 1PE12.5 )
94260 FORMAT( 10X, 'MET_BDY_3D_FIN P_GAM = ', 1PE12.5, ' COORD  P_GAM', ' = ', 1PE12.5 )
94280 FORMAT( 10X, 'MET_BDY_3D_FIN XORIG = ', 1PE12.5, ' COORD  XORIG', ' = ', 1PE12.5 )
94300 FORMAT( 10X, 'MET_BDY_3D_FIN YORIG = ', 1PE12.5, ' COORD  YORIG', ' = ', 1PE12.5 )
94320 FORMAT( 10X, 'MET_BDY_3D_FIN XCELL = ', 1PE12.5, ' COORD  XCELL', ' = ', 1PE12.5 )
94340 FORMAT( 10X, 'MET_BDY_3D_FIN YCELL = ', 1PE12.5, ' COORD  YCELL', ' = ', 1PE12.5 )
94360 FORMAT( 10X, 'MET_BDY_3D_FIN XCENT = ', 1PE12.5, ' COORD  XCENT', ' = ', 1PE12.5 )
94380 FORMAT( 10X, 'MET_BDY_3D_FIN YCENT = ', 1PE12.5, ' COORD  YCENT', ' = ', 1PE12.5 )
94420 FORMAT( 10X, 'MET_BDY_3D_FIN VGTOP = ', 1PE12.5, ' COORD  VGTOP', ' = ', 1PE12.5 )
94440 FORMAT( 10X, 'MET_BDY_3D_FIN VGLVS(', I2, ' ) = ', 1PE12.5, ' COORD VGLVS',
     &                                 '(', I2, ' ) = ', 1PE12.5 )
94460 FORMAT( 10X, 'No file header inconsistencies for MET_BDY_3D_FIN' )
94480 FORMAT( A )

      END
