
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
C $Header: /project/work/rep/arc/CCTM/src/driver/wrf/wr_aconc.F,v 1.6 2011/10/21 16:10:43 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WR_AVG_SA ( JDATE, JTIME, TSTEP )

C20140428  Writes hourly averaged ISAM conc to SA_ACONC_1
C
C          Called by driver.F
C
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses

C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
c     USE SA_LAYERS             ! 20140428 for AISAM_BLEV, AISAM_ELEV, SA_NLAYS
      USE SA_DEFN
      USE RUNTIME_VARS

!     USE SUBST_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif


      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      REAL, ALLOCATABLE :: BUF4( :,:,:,: )
      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'WR_AVG_SA'
      CHARACTER( 16 ) :: ACONC_END_TIME = 'ACONC_END_TIME'
      CHARACTER( 80 ) :: VARDESC = ' '
      CHARACTER( 96 ) :: XMSG = ' '

c     INTEGER, SAVE :: LOGDEV       ! FORTRAN unit number for log file
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK
c     LOGICAL, SAVE :: END_TIME = .FALSE.

      INTEGER      L, K, KD, VAR, SPC ! loop counters
      INTEGER      STATUS

      INTEGER, SAVE :: A_NLYS

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF
      REAL VGTOP_RF
C-----------------------------------------------------------------------

C Change output date/time to starting date/time - e.g. timestamp 1995196:090000
C represents data computed from time 1995196:090000 to 1995196:100000

      IF ( FIRSTIME ) THEN
         LOGDEV = INIT3 ()

C Override default beginning time timestamp for ACONC?
c        VARDESC = 
c    &     'Flag to Override default beginning time timestamp for ACONC'
c        END_TIME = ENVYN( ACONC_END_TIME, VARDESC, END_TIME, STATUS )
c        IF ( STATUS .EQ. 1 ) THEN
c           XMSG = 'Environment variable improperly formatted'
c           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
c        ELSE IF ( STATUS .NE. 0 ) THEN
c           WRITE( LOGDEV, '(5X, A)' ) VARDESC
c           XMSG = '... Using default:'
c           WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
c        END IF

      END IF

      IF ( END_TIME ) THEN   ! ending time timestamp
         MDATE = JDATE; MTIME = JTIME
      ELSE                   ! beginning time timestamp
         MDATE = JDATE; MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, -TSTEP )
      END IF

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

C Try to open existing file for update

c        CALL SUBST_BARRIER

         IF ( AISAM_ELEV .EQ. -1 ) AISAM_ELEV = NLAYS

         A_NLYS = AISAM_ELEV - AISAM_BLEV + 1

         OK = OPEN3( SA_ACONC_1, FSRDWR3, PNAME )
         CALL SUBST_GLOBAL_LOGICAL( OK, 'AND' )
         IF ( .NOT. OK ) THEN

            XMSG = 'Could not open ' // TRIM( SA_ACONC_1 )
     &           // ' file for update - try to open new'
            CALL M3MESG( XMSG )

            IF ( MYPE .EQ. 0 ) THEN

C Get default file header attibutes from CONC file (assumes file already open)

               IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( SA_CONC_1 )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

C Set file header attributes that differ from CONC and open the file

               SDATE3D = MDATE
               STIME3D = MTIME
               TSTEP3D = TSTEP
!0711          NVARS3D = N_SPCTAG
               NLAYS3D = A_NLYS

               L = 0
               DO K = AISAM_BLEV, AISAM_ELEV
                  L = L + 1
                  VGLVS3D( L ) = VGLVS_GD( K )
               END DO
!0711          VGLVS3D( A_NLYS + 1 ) = VGLVS_GD( NLAYS + 1 )
!              GDNAM3D = GDNAME_GD
               GDNAM3D = GRID_NAME  ! from HGRD_DEFN

               FDESC3D( 1 ) = 'Src Apportionment file output '
               FDESC3D( 2 ) = 
     &                'Averaged over the synchronization time steps '
               FDESC3D( 3 ) = 
     &              'Timestamp represents beginning computed date/time '
               FDESC3D( 4 ) = 'Layer mapping (ISAM to AISAM):'
               KD = 4
               VAR = AISAM_BLEV
               L = 0
               DO K = KD + 1, MIN ( A_NLYS + KD, MXDESC3 )
                  L = L + 1
                  WRITE( FDESC3D( K ),'( "Layer", I3, " to", I3, " " )' )
     &            VAR + L - 1, L
               END DO
               IF ( ( KD + 1 + L ) .LT. MXDESC3 ) THEN
                  DO K = KD + 1 + L, MXDESC3
                     FDESC3D( K ) = ' '
                  END DO
               END IF

               WRITE( LOGDEV,* ) ' '
               WRITE( LOGDEV,* ) 
     &                  '      Avg ISAM File Header Description:'
               DO K = 1, KD + L
                  WRITE( LOGDEV,* ) '    => ',
     &            TRIM( FDESC3D( K ) )
               END DO


               IF ( .NOT. OPEN3( SA_ACONC_1, FSNEW3, PNAME ) ) THEN
                  XMSG = 'Could not open '
     &                 // TRIM( SA_ACONC_1 ) // ' file'
                  CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               END IF

            END IF   ! MYPE = 0

         ELSE

C File exists. Make sure it matches requested output.

            IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
               XMSG = 'Could not get '
     &              // TRIM( SA_CONC_1 )
     &              // ' file description'
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

            IF ( .NOT. DESC3( SA_ACONC_1 ) ) THEN
               XMSG = 'Could not get '
     &              // TRIM( SA_ACONC_1 )
     &              // ' file description'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( N_SPCTAG .NE. NVARS3D ) THEN
               WRITE( XMSG, '( A, 2I6 )' )
     &         'Number of variables don''t match file: ', N_SPCTAG, NVARS3D
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            DO SPC = 1, N_SPCTAG
               DO VAR = 1, NVARS3D
                  IF ( VNAM_SPCTAG( SPC ) .EQ. VNAME3D( VAR ) ) GO TO 101
               END DO
               XMSG = 'Could not find ' // VNAM_SPCTAG( SPC )
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
101            CONTINUE
            END DO

            IF ( A_NLYS .NE. NLAYS3D ) THEN
               WRITE( XMSG, '( A, 2I6 )' )
     &         'Number of layers don''t match file: ', A_NLYS, NLAYS3D
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

C Check other header data with CONC file as reference

            IF ( TSTEP_RF .NE. TSTEP3D .OR.
     &           NTHIK_RF .NE. NTHIK3D .OR.
     &           NCOLS_RF .NE. NCOLS3D .OR.
     &           NROWS_RF .NE. NROWS3D .OR.
     &           GDTYP_RF .NE. GDTYP3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_ACONC_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( P_ALP_RF .NE. P_ALP3D .OR.
     &           P_BET_RF .NE. P_BET3D .OR.
     &           P_GAM_RF .NE. P_GAM3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_ACONC_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( XCENT_RF .NE. XCENT3D .OR.
     &           YCENT_RF .NE. YCENT3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_ACONC_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( XORIG_RF .NE. XORIG3D .OR.
     &           YORIG_RF .NE. YORIG3D ) THEN
               XMSG = 'Header inconsistent on existing SA_ACONC_1'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( XCELL_RF .NE. XCELL3D .OR.
     &           YCELL_RF .NE. YCELL3D ) THEN
               XMSG = 'Header inconsistent on existing SA_ACONC_1'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( VGTYP_RF .NE. VGTYP3D ) THEN
               XMSG = 'Header inconsistent on existing SA_ACONC_1'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( VGTOP_RF .NE. VGTOP3D ) THEN
               XMSG = 'Header inconsistent on existing SA_ACONC_1'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

         END IF   ! .NOT. OPEN SA_ACONC_1

c        CALL SUBST_BARRIER

      END IF   ! FIRSTIME
    
      IF ( .NOT. ALLOCATED( BUF4 ) ) ALLOCATE( BUF4( NCOLS, NROWS, A_NLYS, N_SPCTAG ) )

    
      DO SPC = 1, N_SPCTAG
         L = 0
         DO K = AISAM_BLEV, AISAM_ELEV
          L = L + 1
          BUF4( :,:,L,SPC ) = AISAM( :,:,L, S_SPCTAG(SPC), T_SPCTAG(SPC) )
         ENDDO !
         IF ( .NOT. WRITE3( SA_ACONC_1, VNAM_SPCTAG( SPC ),
     &      MDATE, MTIME, BUF4( :,:,:,SPC ) ) ) THEN
            XMSG = 'Could not write '
     &           // VNAM_SPCTAG( SPC )
     &           // ' to ' // SA_ACONC_1
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
!     write( logdev,* ) ' <>var, A_GC_SPC: ', VAR, A_GC_SPC( SPC )
      END DO
 
      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', SA_ACONC_1,
     &      'for date and time', MDATE, MTIME

      IF ( ALLOCATED( BUF4 ) ) DEALLOCATE( BUF4 )

      RETURN 
      END
