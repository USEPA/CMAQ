
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPACONC ( JDATE, JTIME, TSTEP )

! Revision History:
!   D. Wong - July 15: initial version
!   D. Wong 19 May 2016 - renamed ACONC_END_TIME to AVG_FILE_ENDTIME
!   F. Sidi 09 Sep 2019 - Replaced L_ACONC_WVEL(depreciated) with W_VEL
!-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

      IMPLICIT NONE

! Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER, INTENT (IN ) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT (IN ) :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT (IN ) :: TSTEP        ! output timestep (HHMMSS)

! Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'OPACONC'
      CHARACTER( 80 ) :: VARDESC = ' '
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

      INTEGER      L, K, KD, VAR, SPC ! loop counters
      INTEGER      STATUS

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF
      REAL VGTOP_RF
!-----------------------------------------------------------------------

! Change output date/time to starting date/time - e.g. timestamp 1995196:090000
! represents data computed from time 1995196:090000 to 1995196:100000

#ifndef mpas 
! Override default beginning time timestamp for ACONC?      
      IF ( END_TIME ) THEN   ! ending time timestamp
         MDATE = JDATE; MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, TSTEP )
      ELSE                   ! beginning time timestamp
         MDATE = JDATE; MTIME = JTIME
      END IF
 

! Try to open existing file for update
      OK = OPEN3( A_CONC_1, FSRDWR3, PNAME )
      CALL SUBST_GLOBAL_LOGICAL( OK, 'AND' )
      IF ( .NOT. OK ) THEN
#ifndef twoway
         BACKSPACE( LOGDEV )
#endif
         XMSG = 'Could not open ' // TRIM( A_CONC_1 )
     &        // ' file for update - try to open new'
         CALL M3MESG( XMSG )

         IF ( IO_PE_INCLUSIVE ) THEN

! Get default file header attibutes from CONC file (assumes file already open)

            IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
               XMSG = 'Could not get '
     &              // TRIM( CTM_CONC_1 )
     &              // ' file description'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

! Set file header attributes that differ from CONC and open the file

            SDATE3D = MDATE
            STIME3D = MTIME
            TSTEP3D = TSTEP
            NVARS3D = N_ACONC_VARS
            NLAYS3D = A_NLYS

            L = 0
            DO K = ACONC_BLEV, ACONC_ELEV + 1
               L = L + 1
               VGLVS3D( L ) = VGLVS_GD( K )
            END DO
!           GDNAM3D = GDNAME_GD
            GDNAM3D = GRID_NAME  ! from HGRD_DEFN

            FDESC3D( 1 ) = 'Concentration file output '
            FDESC3D( 2 ) = 'Averaged over the synchronization time steps '
            FDESC3D( 3 ) = 'Timestamp represents beginning computed date/time '
            FDESC3D( 4 ) = 'Layer mapping (CGRID to AGRID):'
            KD = 4
            VAR = ACONC_BLEV
            L = 0
            DO K = KD + 1, MIN ( A_NLYS + KD, MXDESC3 )
               L = L + 1
               WRITE( FDESC3D( K ),'( "Layer", I3, " to", I3, " " )' )
     &         VAR + L - 1, L
            END DO
            IF ( ( KD + 1 + L ) .LT. MXDESC3 ) THEN
               DO K = KD + 1 + L, MXDESC3
                  FDESC3D( K ) = ' '
               END DO
            END IF

            WRITE( LOGDEV,* ) 
            WRITE( LOGDEV, '(5x,A)' ) 'Avg Conc File Header Description:'
            DO K = 1, KD + L
               WRITE( LOGDEV,'(7x,A)' ) TRIM( FDESC3D( K ) )
            END DO

            VAR = 0

            DO SPC = 1, N_A_GC_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_GC_SPC( SPC )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Average Molar Mixing Ratio of ' // VNAME3D( VAR )
            END DO

            DO SPC = 1, N_A_AE_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_AE_SPC( SPC )
               IF ( VNAME3D( VAR )(1:3) .EQ. 'NUM' ) THEN
                  UNITS3D( VAR ) = 'm-3'
               ELSE IF ( VNAME3D( VAR )(1:3) .EQ. 'SRF' ) THEN
                  UNITS3D( VAR ) = 'm2 m-3'
               ELSE
                  UNITS3D( VAR ) = 'ug m-3'
               END IF
               VDESC3D( VAR ) = 'Average Concentrations of ' // VNAME3D( VAR )
            END DO

            DO SPC = 1, N_A_NR_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_NR_SPC( SPC )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Average Molar Mixing Ratio of ' // VNAME3D( VAR )
            END DO

            DO SPC = 1, N_A_TR_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_TR_SPC( SPC )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Average Molar Mixing Ratio of ' // VNAME3D( VAR )
            END DO

            IF ( W_VEL ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'W_VEL'
               UNITS3D( VAR ) = 'm s-1'
               VDESC3D( VAR ) = 'Vertical Wind Velocity'
            END IF
             
            IF ( L_ACONC_RH ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'RH'
               UNITS3D( VAR ) = '1'
               VDESC3D( VAR ) = 'Fractional Relative Humidity'
            END IF
             
            IF ( L_ACONC_TA ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'TA'
               UNITS3D( VAR ) = 'K'
               VDESC3D( VAR ) = 'Air Temperature'
            END IF
          
            IF ( L_ACONC_PRES ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'PRES'
               UNITS3D( VAR ) = 'Pa'
               VDESC3D( VAR ) = 'Air Pressure'
            END IF

            DO SPC = 1, VAR
               WRITE( LOGDEV,'( 7X, "=> VNAME3D(", I3, " ): ", A )' )
     &                SPC, VNAME3D( SPC )
            END DO

            IF ( .NOT. OPEN3( A_CONC_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not open '
     &              // TRIM( A_CONC_1 ) // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         END IF   ! MYPE = 0

      ELSE

! File exists. Make sure it matches requested output.

         IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
            XMSG = 'Could not get '
     &           // TRIM( CTM_CONC_1 )
     &           // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         TSTEP_RF = TSTEP3D
         NTHIK_RF = NTHIK3D
         NCOLS_RF = NCOLS3D
         NROWS_RF = NROWS3D
         GDTYP_RF = GDTYP3D
         P_ALP_RF = P_ALP3D
         P_BET_RF = P_BET3D
         P_GAM_RF = P_GAM3D
         XCENT_RF = XCENT3D
         YCENT_RF = YCENT3D
         XORIG_RF = XORIG3D
         YORIG_RF = YORIG3D
         XCELL_RF = XCELL3D
         YCELL_RF = YCELL3D
         VGTYP_RF = VGTYP3D
         VGTOP_RF = VGTOP3D

         IF ( .NOT. DESC3( A_CONC_1 ) ) THEN
            XMSG = 'Could not get '
     &           // TRIM( A_CONC_1 )
     &           // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( N_ACONC_VARS .NE. NVARS3D ) THEN
            WRITE( XMSG, '( A, 2I6 )' )
     &      'Number of variables don''t match file: ', N_ACONC_VARS, NVARS3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         DO SPC = 1, N_ASPCS
            DO VAR = 1, NVARS3D
               IF ( ACONC_FILE_SPCS( SPC ) .EQ. VNAME3D( VAR ) ) GO TO 101
            END DO
            XMSG = 'Could not find ' // ACONC_FILE_SPCS( SPC )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
101         CONTINUE
         END DO

         IF ( A_NLYS .NE. NLAYS3D ) THEN
            WRITE( XMSG, '( A, 2I6 )' )
     &      'Number of layers don''t match file: ', A_NLYS, NLAYS3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

! Check other header data with CONC file as reference

         IF ( TSTEP_RF .NE. TSTEP3D .OR.
     &        NTHIK_RF .NE. NTHIK3D .OR.
     &        NCOLS_RF .NE. NCOLS3D .OR.
     &        NROWS_RF .NE. NROWS3D .OR.
     &        GDTYP_RF .NE. GDTYP3D ) THEN
              XMSG = 'Header inconsistent on existing A_CONC_1'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( P_ALP_RF .NE. P_ALP3D .OR.
     &        P_BET_RF .NE. P_BET3D .OR.
     &        P_GAM_RF .NE. P_GAM3D ) THEN
              XMSG = 'Header inconsistent on existing A_CONC_1'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( XCENT_RF .NE. XCENT3D .OR.
     &        YCENT_RF .NE. YCENT3D ) THEN
              XMSG = 'Header inconsistent on existing A_CONC_1'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( XORIG_RF .NE. XORIG3D .OR.
     &        YORIG_RF .NE. YORIG3D ) THEN
            XMSG = 'Header inconsistent on existing A_CONC_1'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( XCELL_RF .NE. XCELL3D .OR.
     &        YCELL_RF .NE. YCELL3D ) THEN
            XMSG = 'Header inconsistent on existing A_CONC_1'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( VGTYP_RF .NE. VGTYP3D ) THEN
            XMSG = 'Header inconsistent on existing A_CONC_1'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
         IF ( VGTOP_RF .NE. VGTOP3D ) THEN
            XMSG = 'Header inconsistent on existing A_CONC_1'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

      END IF   ! .NOT. OPEN A_CONC_1
#endif

      END SUBROUTINE OPACONC
