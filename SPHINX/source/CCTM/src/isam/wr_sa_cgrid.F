
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

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c      [2] write tssa concentration data 
c     
C     Revision history
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SA_CGRID ( JDATE, JTIME, TSTEP )

C20140428  Writes full ISAM values to a restart file SA_CGRID_1 which would serve as
C          initial conditions for next day's run
C
C          Called by driver.F

      USE GRID_CONF
      USE UTILIO_DEFN           ! 20120615
c     USE SUBST_MODULES         ! stenex
      USE SA_DEFN               ! Mc06
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif


      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

C...Arguments Declaration
      INTEGER    JDATE          ! current model date, coded YYYYDDD
      INTEGER    JTIME          ! current model time, coded HHMMSS
      !INTEGER    TSTEP(2)       ! format 10000, 0, 0
      INTEGER    TSTEP          ! format 10000

C...End of argument declaration

C Local variables:
      
c     INTEGER          ALLOCSTAT, LOGDEV
      INTEGER          ALLOCSTAT

      CHARACTER( 16 ) :: PNAME = 'WR_SA_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '
      
c     INTEGER   C,R,S,K,N, L, ITAG, JSPC    ! loop induction variables
      INTEGER   C,R,S,K,N, L, JSPC    ! loop induction variables

      REAL, ALLOCATABLE :: ISAM_BUFF ( :,:,:,: )
!20140519
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF
      REAL VGTOP_RF

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
        
        FIRSTIME = .FALSE.

        CALL SUBST_BARRIER

!20140519 add subst_global_logical call as done in wr_cgrid
        OK = OPEN3( SA_CGRID_1, FSRDWR3, PNAME )
        CALL SUBST_GLOBAL_LOGICAL( OK, 'AND' )

        IF ( .NOT. OK ) THEN
          XMSG = 'Could not open ' // SA_CGRID_1 // 
     & ' file for update - try to open new'
          CALL M3MESG( XMSG )

          IF ( MYPE .EQ. 0 ) THEN
            ! open sa_conc_1
            IF ( .NOT. OPEN3( SA_CONC_1, FSRDWR3, PNAME ) ) THEN
              XMSG = 'Could not open ' // SA_CONC_1 // 
     & ' file for update '
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            ENDIF ! sa_conc_1 opened ?
      
            ! Get description of sa_conc_1
            IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
              XMSG = 'Could not get file description from ' // SA_CONC_1
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            ENDIF ! sa_conc_1 description obtained ?

            SDATE3D = JDATE
            STIME3D = JTIME
            NLAYS3D = NLAYS

            ! Create sa_cgrid_1
            IF ( .NOT. OPEN3( SA_CGRID_1, FSNEW3, PNAME ) ) THEN
              XMSG = 'Could not open' // SA_CGRID_1 
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            ENDIF !
          ENDIF ! mype0 ?

        ELSE ! 20140519 sa_cgrid already opened by any processor

          IF ( MYPE .EQ. 0 ) THEN

            IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
              XMSG = 'Could not get' // TRIM(SA_CONC_1) //
     &  'file description'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            ENDIF

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

            IF ( .NOT. DESC3( SA_CGRID_1 ) ) THEN
              XMSG = 'Could not get' // TRIM(SA_CGRID_1) // 
     &  'file description'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            ENDIF

            IF ( TSTEP_RF .NE. ABS( TSTEP3D ) .OR.
     &           NTHIK_RF .NE. NTHIK3D .OR.
     &           NCOLS_RF .NE. NCOLS3D .OR.
     &           NROWS_RF .NE. NROWS3D .OR.
     &           GDTYP_RF .NE. GDTYP3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( P_ALP_RF .NE. P_ALP3D .OR.
     &           P_BET_RF .NE. P_BET3D .OR.
     &           P_GAM_RF .NE. P_GAM3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( XCENT_RF .NE. XCENT3D .OR.
     &           YCENT_RF .NE. YCENT3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( XORIG_RF .NE. XORIG3D .OR.
     &           YORIG_RF .NE. YORIG3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( XCELL_RF .NE. XCELL3D .OR.
     &           YCELL_RF .NE. YCELL3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( VGTYP_RF .NE. VGTYP3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            IF ( VGTOP_RF .NE. VGTOP3D ) THEN
                 XMSG = 'Header inconsistent on existing SA_CGRID_1'
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

          ENDIF  ! mype 0

        END IF ! sa_cgrid_1 opened ?

        CALL SUBST_BARRIER

        ALLOCATE ( ISAM_BUFF( NCOLS, NROWS, NLAYS, N_SPCTAG ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating ISAM_BUFF'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        ENDIF

      ENDIF  !firstime

      ISAM_BUFF = 0.0
    
      DO S = 1, N_SPCTAG
        ISAM_BUFF( :,:,:,S ) = ISAM( :,:,:,S_SPCTAG(S),T_SPCTAG(S) )
!20140319        IF ( .NOT. WRITE3( SA_CGRID_1, VNAME3D(S), JDATE, JTIME,
        IF ( .NOT. WRITE3( SA_CGRID_1, VNAM_SPCTAG(S), JDATE, JTIME,
     &          ISAM_BUFF(:,:,:,S)) ) THEN
           XMSG = 'Could not write to ' 
     &           // SA_CGRID_1 
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
      END DO ! S
!      DEALLOCATE( ISAM_BUFF )
      
      return
      end

