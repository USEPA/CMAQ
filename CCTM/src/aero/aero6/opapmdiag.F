
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPAPMDIAG ( JDATE, JTIME, TSTEP )

C Revision history
C   09 May 16 D. Wong: Initial Implementation
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

        INTEGER :: N, V
        INTEGER :: MDATE, MTIME, STATUS
        LOGICAL :: FOUND

C-----------------------------------------------------------------------

! get begin and end level information from environment variable
        IF ( APMDIAG_ZHI .EQ. -1 ) APMDIAG_ZHI = NLAYS
        IF ( APMDIAG_ZLO .LE. 0 .OR. APMDIAG_ZHI .GT. NLAYS ) THEN
           WRITE( XMSG,'( "Layer range", 2I4, " invalid for this model")' )
     &          APMDIAG_ZLO, APMDIAG_ZHI
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
        END IF

! get subset PMDIAG species list
        IF ( N_APMDIAG_SPC .EQ. 0 .OR. APMDIAG_SPC( 1 ) .EQ. 'ALL' ) THEN
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
           DO L = 1, N_APMDIAG_SPC
              FOUND = .FALSE.
              V = 0
              DO WHILE ( ( .NOT. FOUND ) .AND. ( V .LT. NUM_PMDIAG_SPC ) )
                 V = V + 1
                 IF ( APMDIAG_SPC( L ) .EQ. PMDIAG_SPC_RECORD( V )%SPC_NAME ) THEN
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
                 WRITE (XMSG, *) ' Warning: Invalid species ', TRIM( APMDIAG_SPC( L ) )
                 CALL M3MESG( XMSG )
              END IF
           END DO
           NVARS3D = N_APMDIAG_SPC 
        END IF

! get end time information from environment variable AVG_FILE_ENDTIME
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
              NLAYS3D = APMDIAG_ZHI - APMDIAG_ZLO + 1
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

              DO L = APMDIAG_ZLO, APMDIAG_ZHI + 1
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
