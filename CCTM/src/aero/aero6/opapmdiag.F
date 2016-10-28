
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
      SUBROUTINE OPAPMDIAG ( JDATE, JTIME, TSTEP )

C Revision history
C   09 May 16 D. Wong: Initial Implemnetation
C   19 May 16 D. Wong: renamed ACONC_END_TIME to AVG_FILE_ENDTIME

        USE GRID_CONF           ! horizontal & vertical domain specifications
        USE UTILIO_DEFN
        USE PMDIAG_DATA_MODULE

        IMPLICIT NONE
 
        INCLUDE SUBST_FILES_ID  ! file name parameters

C...Arguments:

        INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP      ! output time step

C...Local variables:

        CHARACTER( 16 ) :: PNAME = 'OPAPMDIAG'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables

        CHARACTER( 32 ) :: AVG_PMDIAG_SPCS   = 'AVG_PMDIAG_SPCS'
        CHARACTER( 32 ) :: APMDIAG_BLEV_ELEV = 'APMDIAG_BLEV_ELEV'
        CHARACTER( 16 ) :: V_LIST( 41 )
        CHARACTER( 80 ) :: VARDESC = ' '
        CHARACTER( 16 ) :: AVG_FILE_ENDTIME = 'AVG_FILE_ENDTIME'
        INTEGER :: BEG_LVL, END_LVL, N, V
        INTEGER :: MDATE, MTIME, LOGDEV, STATUS, LOC
        LOGICAL :: END_TIME = .FALSE.
        LOGICAL :: FOUND

        INTERFACE
           SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )
              CHARACTER( * ),  INTENT ( IN )  :: ENV_VAR
              INTEGER,         INTENT ( OUT ) :: NVARS
              CHARACTER( 16 ), INTENT ( OUT ) :: VAL_LIST( : )
           END SUBROUTINE GET_ENVLIST
        END INTERFACE

C-----------------------------------------------------------------------

! get begin and end level information from environment variable
        CALL GET_ENVLIST ( APMDIAG_BLEV_ELEV, N, V_LIST )
        IF ( N .EQ. 0 ) THEN
           BEG_LVL = 1
           END_LVL = NLAYS
        ELSE
           IF ( N .NE. 2 ) THEN
              XMSG = 'Environment variable error for ' // APMDIAG_BLEV_ELEV
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
           END IF
           READ( V_LIST( 1 ), '( I4 )' ) BEG_LVL
           READ( V_LIST( 2 ), '( I4 )' ) END_LVL
        END IF

        IF ( BEG_LVL .LE. 0 .OR. END_LVL .GT. NLAYS ) THEN
           WRITE( XMSG,'( "Layer range", 2I4, " invalid for this model")' )
     &          BEG_LVL, END_LVL
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
        END IF

        IF ( BEG_LVL .NE. 1 ) THEN
           WRITE( XMSG,'( "Layer", I3, " Not 1st layer in CGRID" )' ) BEG_LVL
           CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
        END IF

! get subset PMDIAG species list
        CALL GET_ENVLIST ( AVG_PMDIAG_SPCS, N, V_LIST )
        IF ( N .EQ. 0 .OR. V_LIST( 1 ) .EQ. 'ALL' ) THEN
           DO L = 1, NUM_PMDIAG_SPC
              VTYPE3D( L ) = PMDIAG_SPC_RECORD( L )%VAR_TYPE
              VNAME3D( L ) = PMDIAG_SPC_RECORD( L )%SPC_NAME
              UNITS3D( L ) = PMDIAG_SPC_RECORD( L )%SPC_UNIT
              VDESC3D( L ) = PMDIAG_SPC_RECORD( L )%SPC_DESC
              A_PMDIAG_SPC_MAP( L ) = 1
           END DO
           NVARS3D = NUM_PMDIAG_SPC 
        ELSE
           A_PMDIAG_SPC_MAP = 0
           DO L = 1, N
              FOUND = .FALSE.
              V = 0
              DO WHILE ( ( .NOT. FOUND ) .AND. ( V .LT. NUM_PMDIAG_SPC ) )
                 V = V + 1
                 IF ( V_LIST( L ) .EQ. PMDIAG_SPC_RECORD( V )%SPC_NAME ) THEN
                    FOUND = .TRUE.
                 END IF
              END DO
              IF ( FOUND ) THEN
                 A_PMDIAG_SPC_MAP( V ) = 1
                 VTYPE3D( L ) = PMDIAG_SPC_RECORD( V )%VAR_TYPE
                 VNAME3D( L ) = PMDIAG_SPC_RECORD( V )%SPC_NAME
                 UNITS3D( L ) = PMDIAG_SPC_RECORD( V )%SPC_UNIT
                 VDESC3D( L ) = PMDIAG_SPC_RECORD( V )%SPC_DESC
              ELSE
                 WRITE (XMSG, *) ' Warning: Invalid species ', TRIM( V_LIST( L ) )
                 CALL M3MESG( XMSG )
              END IF
           END DO
           NVARS3D = N 
        END IF

! get end time information from environment variable AVG_FILE_ENDTIME
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

        IF ( IO_PE_INCLUSIVE ) THEN

           IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSRDWR3, PNAME ) ) THEN

              XMSG = 'Could not open ' // CTM_APMDIAG_1 // ' file for update - '
     &                // 'try to open new'
              CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open the aerosol
C diagnostic file

              FTYPE3D = GRDDED3
              SDATE3D = MDATE
              STIME3D = MTIME
              TSTEP3D = TSTEP

              NCOLS3D = GL_NCOLS
              NROWS3D = GL_NROWS
              NLAYS3D = END_LVL
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

              DO L = 1, END_LVL + 1
                 VGLVS3D( L ) = VGLVS_GD( L )
              END DO

              GDNAM3D = GRID_NAME  ! from HGRD_DEFN

              FDESC3D( 1 ) = 'aerosol distribution and chemistry parameters'
              DO L = 2, MXDESC3
                 FDESC3D( L ) = ' '
              END DO

C Open the aerosol diagnostic file

              IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSNEW3, PNAME ) ) THEN
                 XMSG = 'Could not create '// CTM_APMDIAG_1 // ' file'
                 CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
              END IF

           END IF
        END IF

      END SUBROUTINE OPAPMDIAG
