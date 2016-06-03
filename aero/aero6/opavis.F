
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

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPAVIS ( JDATE, JTIME, TSTEP )

C Revision history
C   10 May 16 D. Wong: Initial version
C   19 May 16 D. Wong: renamed ACONC_END_TIME to AVG_FILE_ENDTIME

        USE GRID_CONF           ! horizontal & vertical domain specifications
        USE UTILIO_DEFN

        IMPLICIT NONE
 
        INCLUDE SUBST_FILES_ID  ! file name parameters

C...Arguments:

        INTEGER, INTENT(IN) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT(IN) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT(IN) :: TSTEP      ! output time step

C...Local variables:

        CHARACTER( 16 ) :: PNAME = 'OPAVIS'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables

        INTEGER :: MDATE, MTIME, LOGDEV, STATUS
        LOGICAL :: END_TIME = .FALSE.
        CHARACTER( 80 ) :: VARDESC = ' '
        CHARACTER( 16 ) :: AVG_FILE_ENDTIME = 'AVG_FILE_ENDTIME'

C-----------------------------------------------------------------------

        LOGDEV = INIT3 ()

        VARDESC = 'Flag to Override default beginning time timestamp for PMDIAG'
        END_TIME = ENVYN( AVG_FILE_ENDTIME, VARDESC, END_TIME, STATUS )
        IF ( STATUS .EQ. 1 ) THEN
           XMSG = 'Environment variable improperly formatted'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        ELSE IF ( STATUS .NE. 0 ) THEN
           WRITE( LOGDEV, '(5X, A)' ) VARDESC
           XMSG = '... Using default:'
           WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
        END IF

        IF ( END_TIME ) THEN   ! ending time timestamp
           MDATE = JDATE; MTIME = JTIME
           CALL NEXTIME ( MDATE, MTIME, TSTEP )
        ELSE                   ! beginning time timestamp
           MDATE = JDATE; MTIME = JTIME
        END IF

C Try to open existing file for update

        IF ( .NOT. OPEN3( CTM_AVIS_1, FSRDWR3, PNAME ) ) THEN

           XMSG = 'Could not open ' // CTM_AVIS_1 // ' file for update - '
     &            // 'try to open new'
           CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open the dry dep file

           FTYPE3D = GRDDED3
           SDATE3D = MDATE
           STIME3D = MTIME
           TSTEP3D = TSTEP

           NVARS3D = 4
           NCOLS3D = GL_NCOLS
           NROWS3D = GL_NROWS
           NLAYS3D =     1
           NTHIK3D =     1
           GDTYP3D = GDTYP_GD
           P_ALP3D = P_ALP_GD
           P_BET3D = P_BET_GD 
           P_GAM3D = P_GAM_GD
           XORIG3D = XORIG_GD
           YORIG3D = YORIG_GD
           XCENT3D = XCENT_GD
           YCENT3D = YCENT_GD
           XCELL3D = XCELL_GD
           YCELL3D = YCELL_GD
           VGTYP3D = VGTYP_GD
           VGTOP3D = VGTOP_GD

           DO L = 1, NLAYS3D + 1
              VGLVS3D( L ) = VGLVS_GD( L )
           END DO

           GDNAM3D = GRID_NAME  ! from HGRD_DEFN

           VTYPE3D( 1 ) = M3REAL
           VNAME3D( 1 ) = 'A_DCV_Mie'
           UNITS3D( 1 ) = 'deciview'
           VDESC3D( 1 ) = 'average hourly visual range'

           VTYPE3D( 2 ) = M3REAL
           VNAME3D( 2 ) = 'A_EXT_Mie'
           UNITS3D( 2 ) = '1/km'
           VDESC3D( 2 ) = 'average hourly extinction coefficient'
               
           VTYPE3D( 3 ) = M3REAL
           VNAME3D( 3 ) = 'A_DCV_Recon'
           UNITS3D( 3 ) = 'deciview'
           VDESC3D( 3 ) = 'average hourly visual range'

           VTYPE3D( 4 ) = M3REAL
           VNAME3D( 4 ) = 'A_EXT_Recon'
           UNITS3D( 4 ) = '1/km'
           VDESC3D( 4 ) = 'average hourly extinction coefficient'
 
           FDESC3D( 1 ) = 'hourly 1-layer cross-point Visibility Information'
           DO L = 2, MXDESC3
              FDESC3D( L ) = ' '
           END DO

C Open visibility file new

           IF ( .NOT. OPEN3( CTM_AVIS_1, FSNEW3, PNAME ) ) THEN
              XMSG = 'Could not create '// CTM_AVIS_1 // ' file'
              CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
           END IF

        END IF

      END SUBROUTINE OPAVIS
