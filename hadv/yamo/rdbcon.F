
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
C $Header: /project/yoj/arc/CCTM/src/hadv/yamo/rdbcon.F,v 1.4 2011/09/15 15:42:34 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RDBCON ( JDATE, JTIME, TSTEP, LVL, BCON )

C-----------------------------------------------------------------------
C Function:
C   Read boundary concentrations data for advection and couple with
C   SqRDMT, Where SqRDMT = Sq. Root [det ( metric tensor )]
C                        = Vertical Jacobian / (map scale factor)**2
C   Load Air Density X SqRDMT = RHOJ into last BCON slot for advection

C Preconditions:

C Subroutines and Functions Called:
C   INTERPX, INTERPB, M3EXIT, TRIMLEN, ADVBC_MAP, TIME2SEC, SEC2TIME, NEXTIME

C Revision History:
C   Jeff - Aug 1997 Based on beta version, keep in ppmV units (not aerosols)
C   Jeff - Dec 97 - add CMIN
C   Jeff - Apr 98 - fix conversion/coupling for aerosol number species
C   Jeff - Apr 01 - dyn alloc - Use PINTERB for boundary data - assume the met
C                   data could come from a larger file, but not the conc cata
C   23 Jun 03 J.Young: for layer dependent advection tstep
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                      removed deprecated TRIMLEN
C-----------------------------------------------------------------------

      USE GRID_CONF            ! horizontal & vertical domain specifications
      USE CGRID_SPCS           ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE
 
C Includes:
 
      INCLUDE SUBST_CONST      ! constants
      INCLUDE SUBST_FILES_ID   ! file name parameters

C Arguments:
 
      INTEGER      JDATE       ! current model date, coded YYYYDDD
      INTEGER      JTIME       ! current model time, coded HHMMSS
      INTEGER      TSTEP       ! timestep
      INTEGER      LVL         ! layer
!     REAL         BCON( NBNDY,NLAYS,* )  ! boundary concentrations
      REAL      :: BCON( :,: ) ! boundary concentrations

C Parameters:
 
!     REAL, PARAMETER ( MAOAVO = MWAIR / AVO * 1.0E-03 ) ! Ma/avo X Kg/g
      REAL, PARAMETER :: KGPMG = 1.0E-09  ! Kg / micro-gram
 
      REAL, PARAMETER :: CMIN = 1.0E-30

      CHARACTER( 16 ) :: CONCMIN

C File variables:

!     REAL        SQRDMT_BND( NBNDY,NLAYS )        ! boundary Jacobian
!     REAL        RHOJ_BND  ( NBNDY,NLAYS )        ! mid-layer boundary RhoJ
      REAL, ALLOCATABLE, SAVE :: SQRDMT_BND( :,: ) ! boundary Jacobian
      REAL, ALLOCATABLE, SAVE :: RHOJ_BND  ( :,: ) ! mid-layer boundary RhoJ

      REAL, ALLOCATABLE, SAVE :: SQRDMT_BUF( :,: ) ! grid Jacobian
      REAL, ALLOCATABLE, SAVE :: RHOJ_BUF  ( :,: ) ! mid-layer grid RhoJ

      REAL, ALLOCATABLE, SAVE :: BBUF    ( :,:,: ) ! bcon file buffer

C External Functions: None

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'RDBCON'
      CHARACTER( 16 ) :: VNAME

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 16 ) :: BLNK = ' '
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: BCNAME( : )  ! BC name for adv species
      REAL,            ALLOCATABLE, SAVE :: BCFAC( : )   ! Scale factor for BCs

      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER, SAVE :: N_SPC_ADV

      INTEGER   MDATE             ! mid-advection date
      INTEGER   MTIME             ! mid-advection time
      INTEGER   STEP              ! advection time step in seconds

      INTEGER   BND, VAR, SPC     ! loop counters
      INTEGER   COL, ROW          ! loop counters
      INTEGER   STRT, FINI
      INTEGER   ALLOCSTAT

      INTEGER   COUNT             ! Counter for constructing boundary arrays

      INTEGER   GXOFF, GYOFF      ! global origin offset from file
      LOGICAL, SAVE :: WINDOW = .FALSE. ! posit same file and global
                                        ! processing domain
C for INTERPX
      INTEGER, SAVE :: STRTCOL,   ENDCOL,   STRTROW,   ENDROW
      INTEGER       :: STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC

      INTERFACE
         SUBROUTINE ADVBC_MAP( CONCMIN, BCNAME, BCFAC )
            USE CGRID_SPCS
            USE UTILIO_DEFN
            IMPLICIT NONE
            CHARACTER( 16 ), INTENT( IN )    :: CONCMIN
            CHARACTER( 16 ), INTENT( INOUT ) :: BCNAME( : )
            REAL,            INTENT( INOUT ) :: BCFAC ( : )
         END SUBROUTINE ADVBC_MAP
      END INTERFACE

      integer, save :: logdev
#ifdef Verbose3
      real sumem
      integer s
#endif
C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         logdev = init3()

         WRITE( CONCMIN,'(1PE8.2)' ) CMIN

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC )

C currently not implemented: case where only one origin component matches file's
         IF ( GXOFF .NE. 0 .AND. GYOFF .NE. 0 ) THEN
            WINDOW = .TRUE.       ! windowing from file
            STRTCOL = STRTCOLMC - 1
            ENDCOL  = ENDCOLMC  + 1
            STRTROW = STRTROWMC - 1
            ENDROW  = ENDROWMC  + 1
         ELSE
            STRTCOL = STRTCOLMC
            ENDCOL  = ENDCOLMC
            STRTROW = STRTROWMC
            ENDROW  = ENDROWMC
         END IF

         ALLOCATE ( BBUF( NBNDY,NLAYS,SIZE( BCON,2 ) ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BBUF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ

         ALLOCATE ( BCNAME( N_SPC_ADV ),
     &              BCFAC( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BCNAME or BCFAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C create advected species map to bc's
 
         CALL ADVBC_MAP ( CONCMIN, BCNAME, BCFAC )

         ALLOCATE ( SQRDMT_BND( NBNDY,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SQRDMT_BND'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( RHOJ_BND( NBNDY,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating RHOJ_BND'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( WINDOW ) THEN

            ALLOCATE ( SQRDMT_BUF( 0:MY_NCOLS+1,0:MY_NROWS+1 ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SQRDMT_BUF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( RHOJ_BUF( 0:MY_NCOLS+1,0:MY_NROWS+1 ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating RHOJ_BUF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END IF

      END IF                    ! if FIRSTIME

      MDATE  = JDATE
      MTIME  = JTIME
      STEP   = TIME2SEC( TSTEP )
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

C Read & interpolate boundary SqrDMT, and RhoJ

      IF ( WINDOW ) THEN

         VNAME = 'DENSA_J'
         IF ( .NOT. INTERPX ( MET_CRO_3D, VNAME, PNAME,
     &                        STRTCOL,ENDCOL, STRTROW,ENDROW, LVL,LVL,
     &                        MDATE, MTIME, RHOJ_BUF ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'JACOBM'
         IF ( .NOT. INTERPX ( MET_CRO_3D, VNAME, PNAME,
     &                        STRTCOL,ENDCOL, STRTROW,ENDROW, LVL,LVL,
     &                        MDATE, MTIME, SQRDMT_BUF ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

C Fill in DENSJ array for boundaries

         COUNT = 0
         DO ROW = 0, 0                                ! South
            DO COL = 1, MY_NCOLS+1
               COUNT = COUNT + 1
               SQRDMT_BND( COUNT,LVL ) = SQRDMT_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO
         DO ROW = 1, MY_NROWS+1                       ! East
            DO COL = MY_NCOLS+1, MY_NCOLS+1
               COUNT = COUNT + 1
               SQRDMT_BND( COUNT,LVL ) = SQRDMT_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO
         DO ROW = MY_NROWS+1, MY_NROWS+1              ! North
            DO COL = 0, MY_NCOLS
               COUNT = COUNT + 1
               SQRDMT_BND( COUNT,LVL ) = SQRDMT_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO
         DO ROW = 0, MY_NROWS                         ! West
            DO COL = 0, 0
               COUNT = COUNT + 1
               SQRDMT_BND( COUNT,LVL ) = SQRDMT_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO

      ELSE

!        write ( logdev,* ) '=0= mdate,mtime,nbndy,nlays: ',
!    &                           mdate,mtime,nbndy,nlays

         VNAME = 'JACOBM'
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        SQRDMT_BND ) ) THEN
            XMSG = 'Could not read' // VNAME // ' from ' // MET_BDY_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'DENSA_J'
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        RHOJ_BND ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_BDY_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      END IF   ! WINDOW

      BCON = 0.0

C Read & interpolate boundary concentrations

      SPC = 0
      STRT = 1
      FINI = N_GC_TRNS
      DO 141 VAR = STRT, FINI
         SPC = SPC + 1 
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN

            IF ( .NOT. INTERPB( BNDY_GASC_1, BCNAME( VAR ) , PNAME,
     &                          MDATE, MTIME, NBNDY*NLAYS,
     &                          BBUF( 1,1,VAR ) ) ) THEN
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) )
     &              // ' from ' // BNDY_GASC_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            ELSE   ! found bc's (PPM) on file; convert

               DO BND = 1, NBNDY
                  BCON( BND,VAR ) = BCFAC( VAR )
     &                            * BBUF( BND,LVL,VAR )
     &                            * RHOJ_BND( BND,LVL )
               END DO
            END IF
         ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
            END DO
         END IF
141   CONTINUE

#ifdef Verbose3
      if ( lvl .eq. 1 ) then
         write( logdev,'(/"@@@b ", i8, i7.6)') jdate, jtime
         do var = strt, fini
            sumem = 0.0
            do bnd = 1, nbndy
               sumem = sumem + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumem
         end do
      end if
#endif

      SPC = 0
      STRT = N_GC_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS
      DO 151 VAR = STRT, FINI
         SPC = SPC + 1
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN
            IF ( .NOT. INTERPB ( BNDY_AERO_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS,
     &                           BBUF( 1,1,VAR ) ) ) THEN
               XMSG = 'Could not read ' //
     &                 TRIM( BCNAME ( VAR ) ) //
     &                ' from ' // BNDY_AERO_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            ELSE   ! found bc's (microgram/m**3, m**2/m**3, or number/m**3)
                      ! on file; convert
               IF ( AE_TRNS( SPC )( 1:3 ) .EQ. 'NUM' ) THEN
                  DO BND = 1, NBNDY
                     BCON( BND,VAR ) = BCFAC( VAR )
!    &                               * BBUF( BND,LVL,VAR ) * MAOAVO
     &                               * BBUF( BND,LVL,VAR )
     &                               * SQRDMT_BND( BND,LVL )
                  END DO
               ELSE IF ( AE_TRNS( SPC )( 1:3 ) .EQ. 'SRF' ) THEN
                  DO BND = 1, NBNDY
                     BCON( BND,VAR ) = BCFAC( VAR )
     &                               * BBUF( BND,LVL,VAR )
     &                               * SQRDMT_BND( BND,LVL )
                  END DO
               ELSE
                  DO BND = 1, NBNDY
                     BCON( BND,VAR ) = BCFAC( VAR )
     &                               * BBUF( BND,LVL,VAR ) * KGPMG
     &                               * SQRDMT_BND( BND,LVL )
                  END DO
               END IF
            END IF
         ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
            END DO
         END IF
151   CONTINUE

#ifdef Verbose3
      if ( lvl .eq. 1 ) then
         do var = strt, fini
            sumem = 0.0
            do bnd = 1, nbndy
               sumem = sumem + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumem
         end do
      end if
#endif

      SPC = 0
      STRT = N_GC_TRNS + N_AE_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS
      DO 161 VAR = STRT, FINI
         SPC = SPC + 1 
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN

!           write ( logdev,* ) '=== spc,var,bcname: ', spc, var, bcname( var )

            IF ( .NOT. INTERPB ( BNDY_NONR_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS,
     &                           BBUF( 1,1,VAR ) ) ) THEN
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) )
     &              // ' from ' // BNDY_NONR_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

               ELSE   ! found bc's (PPM) on file; convert

               DO BND = 1, NBNDY
                  BCON( BND,VAR ) = BCFAC( VAR )
     &                            * BBUF( BND,LVL,VAR )
     &                            * RHOJ_BND( BND,LVL )
                  END DO
               END IF
            ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
               END DO
            END IF
161      CONTINUE

#ifdef Verbose3
      if ( lvl .eq. 1 ) then
         do var = strt, fini
            sumem = 0.0
            do bnd = 1, nbndy
               sumem = sumem + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumem
         end do
      end if
#endif

      SPC = 0
      STRT = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV
      DO 171 VAR = STRT, FINI
         SPC = SPC + 1 
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN

!           write ( logdev,* ) '====spc,var,bcname: ', spc, var, bcname( var )

            IF ( .NOT. INTERPB ( BNDY_TRAC_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS,
     &                           BBUF( 1,1,VAR ) ) ) THEN
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) )
     &              // ' from ' // BNDY_TRAC_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            ELSE   ! found bc's (PPM) on file; convert

               DO BND = 1, NBNDY
                  BCON( BND,VAR ) = BCFAC( VAR )
     &                            * BBUF( BND,LVL,VAR )
     &                            * RHOJ_BND( BND,LVL )
               END DO
            END IF
         ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
            END DO
         END IF
171   CONTINUE

#ifdef Verbose3
      if ( lvl .eq. 1 ) then
         do var = strt, fini
            sumem = 0.0
            do bnd = 1, nbndy
               sumem = sumem + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumem
         end do
      end if
#endif

C for advecting Air Density X Jacobian

      DO BND = 1, NBNDY
         BCON( BND,N_SPC_ADV ) = RHOJ_BND( BND,LVL )
      END DO

      RETURN
      END
