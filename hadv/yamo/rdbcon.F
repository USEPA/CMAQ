
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
C $Header: /project/yoj/arc/CCTM/src/hadv/yamo/rdbcon.F,v 1.7 2012/01/19 14:51:13 yoj Exp $
 
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
C   08 Jun 12 J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C-----------------------------------------------------------------------

      USE GRID_CONF            ! horizontal & vertical domain specifications
      USE CGRID_SPCS           ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE
 
C Includes:
      INCLUDE SUBST_CONST      ! constants
      INCLUDE SUBST_FILES_ID   ! file name parameters

C Arguments:
      INTEGER, INTENT( IN ) :: JDATE       ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME       ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP       ! timestep
      INTEGER, INTENT( IN ) :: LVL         ! layer
!     REAL           BCON( NBNDY,NLAYS,* )  ! boundary concentrations
      REAL,    INTENT( OUT ) :: BCON( :,: ) ! boundary concentrations

C Parameters:
      REAL, PARAMETER :: KGPMG = 1.0E-09  ! Kg / micro-gram
      REAL, PARAMETER :: CMIN = 1.0E-30

      CHARACTER( 16 ) :: CONCMIN

C File variables:
!     REAL        JACOBM_BND( NBNDY,NLAYS )        ! boundary Jacobian
!     REAL        RHOJ_BND  ( NBNDY,NLAYS )        ! mid-layer boundary RhoJ
      REAL, ALLOCATABLE, SAVE :: JACOBM_BND( :,: ) ! boundary Jacobian
      REAL, ALLOCATABLE, SAVE :: RHOJ_BND  ( :,: ) ! mid-layer boundary RhoJ

      REAL, ALLOCATABLE, SAVE :: JACOBM_BUF( :,: ) ! grid Jacobian
      REAL, ALLOCATABLE, SAVE :: RHOJ_BUF  ( :,: ) ! mid-layer grid RhoJ

      REAL, ALLOCATABLE, SAVE :: BBUF      ( :,: ) ! bcon file buffer

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
            CHARACTER( 16 ), INTENT( IN )  :: CONCMIN
            CHARACTER( 16 ), INTENT( OUT ) :: BCNAME( : )
            REAL,            INTENT( OUT ) :: BCFAC ( : )
         END SUBROUTINE ADVBC_MAP
      END INTERFACE

      integer, save :: logdev
#ifdef verbose_rdbcon
      real sumbc
      integer s
#endif
C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         logdev = init3()

         WRITE( CONCMIN,'(1PE8.2)' ) CMIN

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC )

C currently not implemented: case where only one origin component matches file`s
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

         ALLOCATE ( BBUF( NBNDY,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BBUF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ

!        write ( logdev,* ) '=0= window,n_spc_adv: ', window, n_spc_adv

         ALLOCATE ( BCNAME( N_SPC_ADV ),
     &              BCFAC( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BCNAME or BCFAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C create advected species map to bc`s
 
         CALL ADVBC_MAP ( CONCMIN, BCNAME, BCFAC )

         ALLOCATE ( JACOBM_BND( NBNDY,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating JACOBM_BND'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( RHOJ_BND( NBNDY,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating RHOJ_BND'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( WINDOW ) THEN

            ALLOCATE ( JACOBM_BUF( 0:MY_NCOLS+1,0:MY_NROWS+1 ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating JACOBM_BUF'
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
     &                        MDATE, MTIME, JACOBM_BUF ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

C Fill in DENSJ array for boundaries

         COUNT = 0
         DO ROW = 0, 0                                ! South
            DO COL = 1, MY_NCOLS+1
               COUNT = COUNT + 1
               JACOBM_BND( COUNT,LVL ) = JACOBM_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO
         DO ROW = 1, MY_NROWS+1                       ! East
            DO COL = MY_NCOLS+1, MY_NCOLS+1
               COUNT = COUNT + 1
               JACOBM_BND( COUNT,LVL ) = JACOBM_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO
         DO ROW = MY_NROWS+1, MY_NROWS+1              ! North
            DO COL = 0, MY_NCOLS
               COUNT = COUNT + 1
               JACOBM_BND( COUNT,LVL ) = JACOBM_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO
         DO ROW = 0, MY_NROWS                         ! West
            DO COL = 0, 0
               COUNT = COUNT + 1
               JACOBM_BND( COUNT,LVL ) = JACOBM_BUF( COL,ROW )
               RHOJ_BND  ( COUNT,LVL ) = RHOJ_BUF  ( COL,ROW )
            END DO
         END DO

      ELSE

!        write ( logdev,* ) '=0= mdate,mtime,nbndy,nlays: ',
!    &                           mdate,mtime,nbndy,nlays

         VNAME = 'JACOBM'
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        JACOBM_BND ) ) THEN
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

      STRT = 1
      FINI = N_GC_TRNS
      DO 141 VAR = STRT, FINI
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN

            bbuf = 0.0   ! array assignment

            IF ( .NOT. INTERPB( BNDY_GASC_1, BCNAME( VAR ), PNAME,
 !          IF ( .NOT. INTERPB( BNDY_GASC_1, BCNAME( VAR ), 'DBG',
     &                          MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) )
     &              // ' from ' // BNDY_GASC_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            ELSE   ! found bc's (PPM) on file; convert

 !    if ( lvl .eq. 1 )
 !   &   write ( logdev,'( a, i4, 1x, a )' ) '=b= var,bcname :', var, trim( bcname( var ) )

               DO BND = 1, NBNDY

 !    if ( lvl .eq. 1 ) then
 !       write ( logdev,'( a, i4, 3(1pe13.3 ) )' )
!    &                      '=0= var,bnd,bcfac,bbuf,rhoj_bnd: ',
 !   &                      '=b= bnd,bcfac,bbuf,rhoj_bnd: ',
 !   &                           bnd, bcfac( var ), bbuf( bnd,lvl ), rhoj_bnd( bnd,lvl )
 !    end if

                  BCON( BND,VAR ) = BCFAC( VAR ) * BBUF( BND,LVL )
     &                            * RHOJ_BND( BND,LVL )
               END DO
            END IF
         ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
            END DO
         END IF
141   CONTINUE

#ifdef verbose_rdbcon
      if ( lvl .eq. 1 ) then
         write( logdev,'(/"@@@b ", i8, i7.6)') jdate, jtime
         do var = strt, fini
            sumbc = 0.0
            do bnd = 1, nbndy
               sumbc = sumbc + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumbc
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
 !          IF ( .NOT. INTERPB ( BNDY_AERO_1, BCNAME( VAR ), 'DBG',
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
               XMSG = 'Could not read ' //
     &                 TRIM( BCNAME ( VAR ) ) //
     &                ' from ' // BNDY_AERO_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            ELSE   ! found bc's (microgram/m**3, m**2/m**3, or number/m**3)
                      ! on file; convert
               IF ( AE_TRNS( SPC )( 1:3 ) .EQ. 'NUM' ) THEN
                  DO BND = 1, NBNDY
                     BCON( BND,VAR ) = BCFAC( VAR ) * BBUF( BND,LVL )
     &                               * JACOBM_BND( BND,LVL )
                  END DO
               ELSE IF ( AE_TRNS( SPC )( 1:3 ) .EQ. 'SRF' ) THEN
                  DO BND = 1, NBNDY
                     BCON( BND,VAR ) = BCFAC( VAR ) * BBUF( BND,LVL )
     &                               * JACOBM_BND( BND,LVL )
                  END DO
               ELSE
                  DO BND = 1, NBNDY
                     BCON( BND,VAR ) = BCFAC( VAR ) * BBUF( BND,LVL ) * KGPMG
     &                               * JACOBM_BND( BND,LVL )
                  END DO
               END IF
            END IF
         ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
            END DO
         END IF
151   CONTINUE

#ifdef verbose_rdbcon
      if ( lvl .eq. 1 ) then
         do var = strt, fini
            sumbc = 0.0
            do bnd = 1, nbndy
               sumbc = sumbc + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumbc
         end do
      end if
#endif

      STRT = N_GC_TRNS + N_AE_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS
      DO 161 VAR = STRT, FINI
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN

!           write ( logdev,* ) '=0= spc,var,bcname: ', spc, var, bcname( var )

            IF ( .NOT. INTERPB ( BNDY_NONR_1, BCNAME( VAR ), PNAME,
 !          IF ( .NOT. INTERPB ( BNDY_NONR_1, BCNAME( VAR ), 'DBG',
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) )
     &              // ' from ' // BNDY_NONR_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

               ELSE   ! found bc's (PPM) on file; convert

               DO BND = 1, NBNDY
                  BCON( BND,VAR ) = BCFAC( VAR ) * BBUF( BND,LVL )
     &                            * RHOJ_BND( BND,LVL )
                  END DO
               END IF
            ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
               END DO
            END IF
161      CONTINUE

#ifdef verbose_rdbcon
      if ( lvl .eq. 1 ) then
         do var = strt, fini
            sumbc = 0.0
            do bnd = 1, nbndy
               sumbc = sumbc + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumbc
         end do
      end if
#endif

      STRT = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV
      DO 171 VAR = STRT, FINI
         IF ( BCNAME( VAR ) .NE. BLNK ) THEN

!           write ( logdev,* ) '=0= spc,var,bcname: ', spc, var, bcname( var )

            IF ( .NOT. INTERPB ( BNDY_TRAC_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) )
     &              // ' from ' // BNDY_TRAC_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            ELSE   ! found bc's (PPM) on file; convert

               DO BND = 1, NBNDY
                  BCON( BND,VAR ) = BCFAC( VAR ) * BBUF( BND,LVL )
     &                            * RHOJ_BND( BND,LVL )
               END DO
            END IF
         ELSE
            DO BND = 1, NBNDY
               BCON( BND,VAR ) = CMIN
            END DO
         END IF
171   CONTINUE

#ifdef verbose_rdbcon
      if ( lvl .eq. 1 ) then
         do var = strt, fini
            sumbc = 0.0
            do bnd = 1, nbndy
               sumbc = sumbc + bcon( bnd,var )
            end do
            write( logdev,'("@@@b ", a, g16.6)') bcname( var ), sumbc
         end do
      end if
#endif

C for advecting Air Density X Jacobian

      DO BND = 1, NBNDY
         BCON( BND,N_SPC_ADV ) = RHOJ_BND( BND,LVL )
      END DO

      RETURN
      END
