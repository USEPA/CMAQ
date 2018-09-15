
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
      SUBROUTINE RDBCON ( JDATE, JTIME, TSTEP, LVL, BCON, LSTAT )

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
C   14 Sep 15 J.Young: change Fortran string comparison to logical from advbc_map
C-----------------------------------------------------------------------

      USE GRID_CONF            ! horizontal & vertical domain specifications
      USE CGRID_SPCS           ! CGRID mechanism species
      USE UTILIO_DEFN
      USE RXNS_DATA, ONLY : MECHNAME
      USE AERO_DATA, ONLY : CHECK_AERO_ICBC, N_MODE

      IMPLICIT NONE
 
C Includes:
      INCLUDE SUBST_CONST      ! constants
      INCLUDE SUBST_FILES_ID   ! file name parameters

C Arguments:
      INTEGER, INTENT( IN ) :: JDATE       ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME       ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP       ! timestep
      INTEGER, INTENT( IN ) :: LVL         ! layer
      REAL,    INTENT( OUT ) :: BCON( :,: ) ! boundary concentrations
      LOGICAL, INTENT( INOUT ) :: LSTAT    ! flag for identifying Aerosol BC flaws

C Parameters:
      REAL, PARAMETER :: KGPMG = 1.0E-09  ! Kg / micro-gram
      REAL, PARAMETER :: CMIN = 1.0E-30

      CHARACTER( 16 ) :: CONCMIN

C File variables:
      REAL, ALLOCATABLE, SAVE :: JACOBM_BND( :,: ) ! boundary Jacobian
      REAL, ALLOCATABLE, SAVE :: RHOJ_BND  ( :,: ) ! mid-layer boundary RhoJ

      REAL, ALLOCATABLE, SAVE :: JACOBM_BUF( :,: ) ! grid Jacobian
      REAL, ALLOCATABLE, SAVE :: RHOJ_BUF  ( :,: ) ! mid-layer grid RhoJ

      REAL, ALLOCATABLE, SAVE :: BBUF      ( :,: ) ! bcon file buffer
      REAL, ALLOCATABLE, SAVE :: BBUF_TMP  ( :,: ) ! bcon file buffer

C External Functions: None

C Local variables:
      CHARACTER( 16 ) :: PNAME = 'RDBCON'
      CHARACTER( 16 ) :: VNAME

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 16 ) :: BLNK = ' '
      CHARACTER( 16 ) :: BCNAME_TMP = ' '
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: BCNAME( : )  ! BC name for adv species
      REAL,            ALLOCATABLE, SAVE :: BCFAC( : )   ! Scale factor for BCs
      LOGICAL,         ALLOCATABLE, SAVE :: ONFILE( : )  ! BC species on the file

      CHARACTER( 96 )  :: XMSG = ' '
      CHARACTER( 199 ) :: XMSG2 = ' '
      INTEGER          :: STAT

      INTEGER, SAVE :: N_SPC_ADV
      INTEGER       :: LMODE    !Identifies the problematic mode from
                                !the BC Check routine
      REAL          :: AER_PAR( 2, N_MODE,5 )  !Modal parameter after the BC 
                                               !check (N, dg, sg)
                                               !      (N, M2, M3) - Before
                                               !      (N, M2, M3) - After

      INTEGER   MDATE             ! mid-advection date
      INTEGER   MTIME             ! mid-advection time
      INTEGER   STEP              ! advection time step in seconds

      INTEGER   BND, VAR, SPC     ! loop counters
      INTEGER   COL, ROW, IMODE   ! loop counters
      INTEGER   STRT, FINI, IAER, J
      INTEGER   ALLOCSTAT

      INTEGER   COUNT             ! Counter for constructing boundary arrays

      INTEGER   GXOFF, GYOFF      ! global origin offset from file
      LOGICAL, SAVE :: WINDOW = .FALSE. ! posit same file and global
                                        ! processing domain

C for INTERPX
      INTEGER, SAVE :: STRTCOL,   ENDCOL,   STRTROW,   ENDROW
      INTEGER       :: STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC

      INTERFACE
         SUBROUTINE ADVBC_MAP( CONCMIN, BCNAME, BCFAC, ONFILE )
            CHARACTER( 16 ), INTENT( IN )  :: CONCMIN
            CHARACTER( 16 ), INTENT( OUT ) :: BCNAME( : )
            REAL,            INTENT( OUT ) :: BCFAC ( : )
            LOGICAL,         INTENT( OUT ) :: ONFILE( : )
         END SUBROUTINE ADVBC_MAP
      END INTERFACE

#ifdef verbose_rdbcon
      real sumbc
      integer s
#endif
C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         WRITE( CONCMIN,'(1PE9.2)' ) CMIN

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
         ALLOCATE ( BBUF_TMP( NBNDY,NLAYS ), STAT = ALLOCSTAT )

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ


         ALLOCATE ( BCNAME( N_SPC_ADV ),
     &              BCFAC ( N_SPC_ADV ),
     &              ONFILE( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BCNAME, BCFAC, or ONFILE'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C create advected species map to bc`s
 
         CALL ADVBC_MAP ( CONCMIN, BCNAME, BCFAC, ONFILE )

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
C According to the METBDY3D File, variables have the following units:
C    JACOBM  - Meters
C    DENSA_J - KG M-2

      IF ( WINDOW ) THEN

         VNAME = 'DENSA_J'
         RHOJ_BUF = 0.0
         IF ( .NOT. INTERPX ( MET_CRO_3D, VNAME, PNAME,
     &                        STRTCOL,ENDCOL, STRTROW,ENDROW, LVL,LVL,
     &                        MDATE, MTIME, RHOJ_BUF ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'JACOBM'
         JACOBM_BUF = 0.0
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

         VNAME = 'JACOBM'
#ifdef parallel
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        JACOBM_BND, LVL ) ) THEN
#else
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        JACOBM_BND ) ) THEN
#endif
            XMSG = 'Could not read' // VNAME // ' from ' // MET_BDY_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'DENSA_J'
#ifdef parallel
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        RHOJ_BND, LVL ) ) THEN
#else
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*NLAYS,
     &                        RHOJ_BND ) ) THEN
#endif
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_BDY_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      END IF   ! WINDOW

      BCON = 0.0

C Read & interpolate boundary concentrations
      ! Gas-Phase Species
      STRT = 1
      FINI = N_GC_TRNS
      DO 141 VAR = STRT, FINI
         IF ( ONFILE( VAR ) ) THEN
            BBUF = 0.0   ! array assignment
#ifdef parallel
            IF ( .NOT. INTERPB( BNDY_GASC_1, BCNAME( VAR ), PNAME,
     &                          MDATE, MTIME, NBNDY*NLAYS, BBUF, LVL ) ) THEN
#else
            IF ( .NOT. INTERPB( BNDY_GASC_1, BCNAME( VAR ), PNAME,
     &                          MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
#endif
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) ) // ' from '
     &              // BNDY_GASC_1
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

      ! Particle Species
      SPC = 0
      STRT = N_GC_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS
      DO 151 VAR = STRT, FINI
         SPC = SPC + 1
         IF ( ONFILE( VAR ) ) THEN
            BBUF = 0.0   ! array assignment
#ifdef parallel
            IF ( .NOT. INTERPB ( BNDY_AERO_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF, LVL ) ) THEN
#else
            IF ( .NOT. INTERPB ( BNDY_AERO_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
#endif
               XMSG = 'Could not read ' // TRIM( BCNAME ( VAR ) ) // ' from '
     &              // BNDY_AERO_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            ELSE   
               ! Add Non-Carbon Mass to Carbon Mass if the model is
               ! running semivolatile POA
               IF ( BCNAME( VAR )( 1:4 ) .EQ. 'APOC' ) THEN 
                  IF ( INDEX( MECHNAME,"NVPOA" ) .EQ. 0 ) THEN
                    BBUF_TMP = 0.0
                    BCNAME_TMP = 'APNCOM' // BCNAME( VAR )( 5:5 )
#ifdef parallel
                    IF ( .NOT. INTERPB ( BNDY_AERO_1, BCNAME_TMP, PNAME,
     &                         MDATE, MTIME, NBNDY*NLAYS, BBUF_TMP, LVL ) ) THEN
#else
                    IF ( .NOT. INTERPB ( BNDY_AERO_1, BCNAME_TMP, PNAME,
     &                         MDATE, MTIME, NBNDY*NLAYS, BBUF_TMP ) ) THEN
#endif
                       XMSG = 'Could not read ' // TRIM( BCNAME_TMP ) 
     &                        // ' from ' // BNDY_AERO_1
                       CALL M3WARN( PNAME, MDATE, MTIME, XMSG ) 
                    END IF
                    BBUF = BBUF + BBUF_TMP
                  END IF
               END IF

               ! Units of JACOBM_BND are Meters, according to the input
               ! file. Units of BBUF are ug m-3 ( m2 m-3 for surface
               ! area and N m-3 for number).
               BCON( 1:NBNDY,VAR ) = BCFAC( VAR ) * BBUF( 1:NBNDY,LVL ) 
     &                               * JACOBM_BND( 1:NBNDY,LVL )
                  
               ! Found bc's (microgram/m**3, m**2/m**3, or number/m**3)
               ! Also Do a Mass conversion as long as this particle
               ! species is not Number or Surface Area.
               IF ( AE_TRNS( SPC )( 1:3 ) .NE. 'NUM' .AND.
     &              AE_TRNS( SPC )( 1:3 ) .NE. 'SRF'      ) THEN
                  BCON( 1:NBNDY,VAR ) = BCON( 1:NBNDY,VAR ) * KGPMG
               END IF
                
            END IF
         ELSE
            BCON( 1:NBNDY,VAR ) = CMIN
         END IF
151   CONTINUE
      
      WHERE( BCON .NE. BCON ) BCON = 0.0

      !Check Aerosol Size Distributions and Warn 
      !the User if They Are Not Robust
      DO BND = 1,NBNDY
        CALL CHECK_AERO_ICBC( BCON( BND,STRT:FINI ), .FALSE., STAT, AER_PAR, LMODE )
        IF ( STAT .GT. 0 ) THEN
          LSTAT = .TRUE.
#ifdef verbose_rdbcon
          WRITE ( LOGDEV,'(7x,A55,I1,/,7x,A20,I1,A42,/,9x,A51,/,9x,A22,I4,2x,I2,/,
     &                 9x,A19,1x,A3,8x,A2,8x,A2,8x,A2,8x,A2,/,
     &                 27x,E8.1,1x,E8.1,1x,E8.1,1x,E8.1,4x,F4.1,/,
     &                 9x,A19,1x,A3,8x,A2,8x,A2,8x,A2,8x,A2,/,
     &                 27x,E8.1,1x,E8.1,1x,E8.1,1x,E8.1,4x,F4.1)'),
     &               'Warning: Applying Aerosol Boundary Conditions for mode ',LMODE,
     &               'The Offending Mode (',LMODE,') had diameter and/or sigma out of bounds.',
     &               'It was overwritten by changing the Num and SrfArea.',
     &               'Grid Cell(Horz,Vert): ',BND, LVL,
     &               'Modal Props Before:','Num','M2','M3','Dg','Sg',(AER_PAR(1,LMODE,j),j=1,5),
     &               'Modal Props After: ','Num','M2','M3','Dg','Sg',(AER_PAR(2,LMODE,j),j=1,5)
#endif
        ENDIF
      END DO

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

      ! Non-Reactive Species
      STRT = N_GC_TRNS + N_AE_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS
      DO 161 VAR = STRT, FINI
         IF ( ONFILE( VAR ) ) THEN
            BBUF = 0.0   ! array assignment
#ifdef parallel
            IF ( .NOT. INTERPB ( BNDY_NONR_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF, LVL ) ) THEN
#else
            IF ( .NOT. INTERPB ( BNDY_NONR_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
#endif
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) ) // ' from '
     &              // BNDY_NONR_1
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
161   CONTINUE

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

      ! Tracer Species
      STRT = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + 1
      FINI = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV
      DO 171 VAR = STRT, FINI
         IF ( ONFILE( VAR ) ) THEN
            BBUF = 0.0   ! array assignment
#ifdef parallel
            IF ( .NOT. INTERPB ( BNDY_TRAC_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF, LVL ) ) THEN
#else
            IF ( .NOT. INTERPB ( BNDY_TRAC_1, BCNAME( VAR ), PNAME,
     &                           MDATE, MTIME, NBNDY*NLAYS, BBUF ) ) THEN
#endif
               XMSG = 'Could not read ' // TRIM( BCNAME( VAR ) ) // ' from '
     &              // BNDY_TRAC_1
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
