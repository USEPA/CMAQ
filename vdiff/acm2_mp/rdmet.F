
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2_mp/rdmet.F,v 1.1 2010/07/20 11:50:38 yoj Exp $
 
C what( 1 ) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RDMET ( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ, DENS1 )

C-----------------------------------------------------------------------
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable RDEPVHT, RJACM, RRHOJ
C   14 Nov 03 J.Young: add reciprocal vertical Jacobian product for full and
C                      mid-layer
C   Tanya took JACOBF out of METCRO3D! Improvise
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE
 
!     INCLUDE SUBST_HGRD_ID   ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID   ! vertical dimensioning parameters
      INCLUDE SUBST_FILES_ID  ! file name parameters
      INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC   ! file header data structure
      INCLUDE SUBST_IODECL    ! I/O definitions and declarations

C Arguments:

      INTEGER      MDATE, MTIME                 ! internal simulation date&time
!     REAL         RDEPVHT( NCOLS,NROWS )       ! reciprocal dep vel ht
!     REAL         RJACM  ( NCOLS,NROWS,NLAYS ) ! reciprocal mid-layer Jacobian
!     REAL         RRHOJ  ( NCOLS,NROWS,NLAYS ) ! reciprocal density X Jacobian
      REAL      :: RDEPVHT( :,: )               ! air dens / dep vel ht
      REAL      :: RJACM  ( :,:,: )             ! reciprocal mid-layer Jacobian
      REAL      :: RVJACMF( :,:,: )             ! 1/ mid-full layer vert Jac
      REAL      :: RRHOJ  ( :,:,: )             ! reciprocal density X Jacobian
      REAL      :: DENS1  ( :,: )               ! layer 1 air density

C File variables:

!     REAL         ZF     ( NCOLS,NROWS,NLAYS ) ! layer surface ht above ground
      REAL, ALLOCATABLE, SAVE :: ZF  ( :,:,: ) ! layer surface ht above ground
      REAL, ALLOCATABLE, SAVE :: DENS( :,:,: ) ! air density
      REAL, ALLOCATABLE, SAVE :: JACF( :,:,: ) ! full-layer Jacobian
      REAL, ALLOCATABLE, SAVE :: RMSFX4( :,: ) ! inverse map scale factor ** 4

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'RDMET'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV

      INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER       :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2

      INTEGER      C, R, L                   ! loop induction variables
      INTEGER      ALLOCSTAT

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         ALLOCATE ( ZF( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ZF'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         ALLOCATE ( DENS( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENS'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         ALLOCATE ( JACF( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating JACF'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

C reciprical of msfx2**2

         ALLOCATE ( RMSFX4( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating RMSFX4'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         VNAME = 'MSFX2'
         IF ( .NOT. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       MDATE, MTIME, RMSFX4 ) ) THEN
            XMSG = 'Could not interpolate MSFX2 from ' // GRID_CRO_2D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         RMSFX4 = 1.0 / ( RMSFX4 * RMSFX4 )

         END IF   ! FIRSTIME

C Interpolate file input variables and format for output

      VNAME = 'ZF'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, ZF ) ) THEN
         XMSG = 'Could not interpolate ZF from '// MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS
            RDEPVHT( C,R ) = 1.0 / ZF( C,R,1 )
            END DO
         END DO

      VNAME = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT 1 )
         END IF

      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS
            DENS1( C,R ) = DENS( C,R,1 )
            END DO
         END DO

      VNAME = 'JACOBM'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, RJACM ) ) THEN
         XMSG = 'Could not interpolate JACOBM from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      VNAME = 'JACOBF'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, JACF ) ) THEN
         XMSG = 'Could not interpolate JACOBF from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               RJACM( C,R,L ) = 1.0 / RJACM( C,R,L )
               RVJACMF( C,R,L ) = RJACM( C,R,L ) * RMSFX4( C,R ) / JACF( C,R,L )
               END DO
            END DO
         END DO

      VNAME = 'DENSA_J'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, RRHOJ ) ) THEN
         XMSG = 'Could not interpolate DENSA_J from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               RRHOJ( C,R,L ) = 1.0 / RRHOJ( C,R,L )
               END DO
            END DO
         END DO

      RETURN
      END
