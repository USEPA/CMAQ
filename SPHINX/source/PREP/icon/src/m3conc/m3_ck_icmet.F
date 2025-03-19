
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
C $Header: /project/yoj/arc/ICON/src/m3conc/m3_ck_icmet.F,v 1.2 2011/10/21 16:41:54 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE CK_ICMET_FL ( LOGUNIT, SDATE, STIME )

C***********************************************************************
 
C  Function: To check input MET_CRO_3D_FIN file for consistency with
C            the LAYER_FILE/VGRD_DEFN data
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: None
 
C  Revision History:
C     Prototype created by Jerry Gipson, May, 1999
C     01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C     12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C     09 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C     21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C     06 Nov 18 S.Roselle: Replaced UTILIO_DEFN with M3UTILIO
                    
C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE IC_PARMS    ! ICON parameters

      IMPLICIT NONE     

C Arguments:
      INTEGER, INTENT( IN ) :: LOGUNIT ! Unit number for output log
      INTEGER, INTENT( IN ) :: SDATE   ! Start date
      INTEGER, INTENT( IN ) :: STIME   ! Start time

C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 80 ) :: MSG          ! Log message
      CHARACTER( 16 ) :: PNAME = 'CK_ICMET_FL'   ! Procedure Name
      CHARACTER( 15 ) :: VAL1         ! Character value of real
      CHARACTER( 15 ) :: VAL2         ! Character value of real

      INTEGER JDATE       ! Current date
      INTEGER JTIME       ! Current time
      INTEGER L           ! Loop index

      LOGICAL LERROR      ! Error Flag
    
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write out report data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MSG = 'Checking MET_CRO_3D_FIN File for consistent ' //
     &      'header data' 
      CALL M3MESG ( MSG )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get header data for MET_CRO_3D_FIN file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. OPEN3 ( MET_CRO_3D_FIN, FSREAD3, PNAME ) ) THEN
         MSG = 'Could not open ' // MET_CRO_3D_FIN // ' file'
         CALL M3EXIT ( PNAME, JDATE , JTIME, MSG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3 ( MET_CRO_3D_FIN ) ) THEN
         MSG = 'Could not read DESC of  ' // MET_CRO_3D_FIN 
     &         // ' file'
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check MET_CRO_3D_FIN file for consistency with GRIDDESC
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE.
         
      IF ( FTYPE3D .NE. GRDDED3 ) THEN
         WRITE( LOGUNIT, 94020 ) FTYPE3D, GRDDED3
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
 
!      IF ( .NOT. CHECK3( MET_CRO_3D_FIN, ALLVAR3, SDATE, STIME ) ) THEN
!         MSG = 'Requested time is not on the ' // MET_CRO_3D_FIN
!     &       // ' file'
!         WRITE( LOGUNIT, 94480 ) MSG
!         LERROR = .TRUE.
!      END IF
             
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  If inconsistency found, then stop, else return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LERROR ) THEN
         WRITE( LOGUNIT, 94460 )
      ELSE
         MSG = 'MET_CRO_3D_FIN file inconsistent: stopping' 
         CALL M3EXIT ( PNAME, SDATE , STIME, MSG, XSTAT2 )
      END IF
     
      RETURN

C************************* FORMAT STATEMENTS ***************************

94000 FORMAT( E15.5 )
94020 FORMAT( 10X, 'MET_CRO_3D_FIN FTYPE = ', I3, ' COORD FTYPE', ' = ', I3 )
94040 FORMAT( 10X, 'MET_CRO_3D_FIN NCOLS = ', I3, ' COORD NCOLS', ' = ', I3 )
94060 FORMAT( 10X, 'MET_CRO_3D_FIN NROWS = ', I3, ' COORD NROWS', ' = ', I3 )
94080 FORMAT( 10X, 'MET_CRO_3D_FIN NLAYS = ', I3, ' COORD NLAYS', ' = ', I3 )
94200 FORMAT( 10X, 'MET_CRO_3D_FIN GDTYP = ', I3, ' COORD GDTYP', ' = ', I3 )
94220 FORMAT( 10X, 'MET_CRO_3D_FIN P_ALP = ', 1PE12.5, ' COORD  P_ALP', ' = ', 1PE12.5 )
94240 FORMAT( 10X, 'MET_CRO_3D_FIN P_BET = ', 1PE12.5, ' COORD  P_BET', ' = ', 1PE12.5 )
94260 FORMAT( 10X, 'MET_CRO_3D_FIN P_GAM = ', 1PE12.5, ' COORD  P_GAM', ' = ', 1PE12.5 )
94280 FORMAT( 10X, 'MET_CRO_3D_FIN XORIG = ', 1PE12.5, ' COORD  XORIG', ' = ', 1PE12.5 )
94300 FORMAT( 10X, 'MET_CRO_3D_FIN YORIG = ', 1PE12.5, ' COORD  YORIG', ' = ', 1PE12.5 )
94320 FORMAT( 10X, 'MET_CRO_3D_FIN XCELL = ', 1PE12.5, ' COORD  XCELL', ' = ', 1PE12.5 )
94340 FORMAT( 10X, 'MET_CRO_3D_FIN YCELL = ', 1PE12.5, ' COORD  YCELL', ' = ', 1PE12.5 )
94360 FORMAT( 10X, 'MET_CRO_3D_FIN XCENT = ', 1PE12.5, ' COORD  XCENT', ' = ', 1PE12.5 )
94380 FORMAT( 10X, 'MET_CRO_3D_FIN YCENT = ', 1PE12.5, ' COORD  YCENT', ' = ', 1PE12.5 )
94420 FORMAT( 10X, 'MET_CRO_3D_FIN VGTOP = ', 1PE12.5, ' COORD  VGTOP', ' = ', 1PE12.5 )
94440 FORMAT( 10X, 'MET_CRO_3D_FIN VGLVS(', I2, ' ) = ', 1PE12.5, ' COORD VGLVS',
     &                                 '(', I2, ' ) = ', 1PE12.5 )

94460 FORMAT( 10X, 'NONE' )
94480 FORMAT( A )

      END
