
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
C $Header: /project/yoj/arc/CCTM/src/driver/yamo/wr_vext.F,v 1.6 2011/10/21 16:10:43 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WR_VEXT( JDATE, JTIME, TSTEP )

C Revision History:
C   24 Nov 17 B.Henderson: copied from WR_CONC
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE STD_CONC              ! standard CONC
      USE PIOMAPS_MODULE
      USE UTILIO_DEFN

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

!     REAL      :: SGRID( :,:,:,: )
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C External functions:
      LOGICAL, EXTERNAL :: PWRVEC ! Parallel write for vector data files.

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'WR_VEXT'
      CHARACTER( 96 ) :: XMSG = ' '

      REAL, ALLOCATABLE, SAVE :: JACF( :,:,: ) ! full-layer Jacobian
      REAL, ALLOCATABLE, SAVE :: MSFX2( :,: )  ! map scale factor ** 2
      REAL, ALLOCATABLE :: DBUFF ( :,: )     ! input for W_VEL
      INTEGER      ALLOCSTAT

      INTEGER, SAVE :: LOGDEV       ! FORTRAN unit number for log file
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER       :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2

      INTEGER      C, R, K, L, V,GI   ! loop induction variables
      INTEGER      C0, CN, R0, RN
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VNAME( : )
      INTEGER, SAVE :: NVARS

C-----------------------------------------------------------------------
      IF (.not.DOVEXT) RETURN
      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

C open vext file for update

         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( CTM_VEXT_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // CTM_VEXT_1 //
     &                ' file for update'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         IF ( .NOT. DESC3( CTM_VEXT_1 ) ) THEN
            XMSG = 'Could not get file description from ' // CTM_VEXT_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         NVARS = NVARS3D
         ALLOCATE ( VNAME( NVARS ), STAT=ALLOCSTAT )

         VNAME = VNAME3D( 1:NVARS )
         IF ( .NOT. ALLOCATED( DBUFF ) ) THEN
            ALLOCATE ( DBUFF( NVERTEXT,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DBUFF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

      END IF   ! firstime
      WRITE(LOGDEV, *) 'WR_VEXT:NVARS', NVARS
      
      DO V = 1, NVARS
         IF ( .NOT. PWRVEC(CTM_VEXT_1, VNAME(V), JDATE, JTIME,
     &                     SGRID(:, :, :, V), NCOLS3D, NROWS3D, NLAYS3D,
     &                     NCOLS, NROWS, NUMPROCS,
     &                     NVERTEXT,VERTEXTI,VERTEXTJ     ) ) THEN
                  XMSG = 'PWRGRDD failed writing variable '
     &                  // TRIM( VNAME(V) ) //
     &                  ' to file '// TRIM( CTM_VEXT_1 )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            RETURN
         END IF
      END DO

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', CTM_VEXT_1,
     &      'for date and time', JDATE, JTIME
      RETURN 
      END SUBROUTINE WR_VEXT
