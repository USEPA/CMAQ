
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

      SUBROUTINE WR_SA ( JDATE, JTIME, TSTEP, NSTEPS )

C20140428  Writes ISAM values to SA_CONC_1 file
C
C          Called by driver.F

!      USE HGRD_DEFN             ! horizontal domain specifications
      USE GRID_CONF
      USE UTILIO_DEFN           ! 20120615
c     USE SA_LAYERS
      USE SA_DEFN               ! Mc06
c     USE SUBST_MODULES         ! stenex
      USE RUNTIME_VARS

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
      INTEGER    TSTEP(3)       ! format 10000, 0, 0
cyqw
      INTEGER    NSTEPS         ! Number of time steps of 3D output
cyqw
!0711 CHARACTER( 16 )       :: FILE_NAME

C...End of argument declaration

C Local variables:
      
c     INTEGER          ALLOCSTAT, LOGDEV
      INTEGER          ALLOCSTAT
      LOGICAL, SAVE :: FIRSTIME = .TRUE.    !20140228

      CHARACTER( 16 ) :: PNAME = 'WR_SA'
      CHARACTER( 96 ) :: XMSG = ' '
      
      INTEGER   C,R,S,K,N, L, JSPC    ! loop induction variables

      REAL, ALLOCATABLE, SAVE :: ISAM_BUFF ( :,:,:,: )  ! save added on 20140306

C-----------------------------------------------------------------------


      CALL SUBST_BARRIER


Ckrt     print*,'N_SPCTAG, JDATE, JTIME '
Ckrt     print*, N_SPCTAG, JDATE, JTIME  

!0710    IF ( .NOT. OPEN3( FILE_NAME, FSRDWR3, PNAME ) ) THEN
!0710       XMSG = 'Could not open ' // FILE_NAME // ' file for update'
!0710       CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!0710    END IF

!0711    IF ( .NOT. DESC3( FILE_NAME ) ) THEN
!0711       XMSG = 'Could not get file description from ' // FILE_NAME
!0711       CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!0711    END IF

      IF ( FIRSTIME ) THEN  ! 20140228

        FIRSTIME = .FALSE.
        LOGDEV = INIT3 ()

        IF ( MYPE .NE. 0 ) THEN
          IF ( .NOT. OPEN3( SA_CONC_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // SA_CONC_1 // 'file for update'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF !open3
        END IF ! mype nonzero

        IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
          XMSG = 'Could not get file description from ' // SA_CONC_1
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( ISAM_ELEV .EQ. -1 ) ISAM_ELEV = NLAYS
        SA_NLAYS = ISAM_ELEV - ISAM_BLEV + 1

!20140306 relocated here the following five lines
         ALLOCATE ( ISAM_BUFF( NCOLS, NROWS, SA_NLAYS, N_SPCTAG ),
     &     STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ISAM_BUFF'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ENDIF

      IF ( ISAM_ELEV .EQ. -1 ) ISAM_ELEV = NLAYS

      END IF ! firstime

      ISAM_BUFF = 0.0
!Ckrt  print*,'S,VNAME3D(S),S_SPCTAG(S),T_SPCTAG(S)'
      DO S = 1, N_SPCTAG
       K = 0
       DO L = ISAM_BLEV, ISAM_ELEV
        K = K + 1
        DO R = 1, NROWS
         DO C = 1, NCOLS
          ISAM_BUFF( C,R,K,S ) = ISAM( C,R,L,S_SPCTAG(S),T_SPCTAG(S) )
         ENDDO
        ENDDO
       ENDDO


!20140306 Mentioning of vname3d is dangerous. Use vnam_spctag instead
!20140306       IF ( .NOT. WRITE3( SA_CONC_1, VNAME3D(S), JDATE, JTIME,
       IF ( .NOT. WRITE3( SA_CONC_1, VNAM_SPCTAG(S), JDATE, JTIME,
     &    ISAM_BUFF(:,:,:,S)) ) THEN
         XMSG = 'Could not write to ' 
!0711&        // FILE_NAME 
     &        // SA_CONC_1 
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF
      END DO
!20140306         IF ( ALLOCATED(ISAM_BUFF) ) DEALLOCATE( ISAM_BUFF )
      
c.....couple ISAM after writing

      return
      end

