
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
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/EMIS_DEFN.F,v 1.10 2011/10/21 16:10:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE S_EMIS_DEFN

C-----------------------------------------------------------------------
C Function: emissions interface to the chemistry-transport model

C Revision History:
C   17 Aug 11 S.Napelenok: Initial implementation
C   21 Jun 13 S.Napelenok: ddm-3d implementation for cmaq5.0.1
C   23 Oct 15 S.Napelenok: ddm-3d implementation for cmaq5.1.1
C   17 Jun 16 S.Napelenok: update for cmaq5.2, remove use of EM_FAC
C-----------------------------------------------------------------------

#ifdef sens

      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN         ! I/O API

      USE DDM3D_DEFN
      USE EMIS_DEFN
      USE LTNG_DEFN           ! NO emissions from lightning strikes
      USE MGEMIS              ! marine gas emissions
      USE PT3D_DEFN           ! plume rise emissions
      USE AERO_EMIS           ! inherits GRID_CONF
      USE AERO_DATA
      USE BIOG_EMIS, ONLY: MSPCS
      USE BEIS_DEFN

      USE PTMAP, ONLY: PTEM_MAP, N_SPC_PTPM, PTPM_MAP, SPC_PTEM_MAP

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: SVDEMIS( :,:,:,:,: ) ! total emissions array

      INTEGER, PRIVATE, SAVE :: STRT_GC_EMIS, FINI_GC_EMIS,
     &                          STRT_AE_EMIS, FINI_AE_EMIS,
     &                          STRT_NR_EMIS, FINI_NR_EMIS,
     &                          STRT_TR_EMIS, FINI_TR_EMIS

      INTEGER, PRIVATE, SAVE :: STRT_GC_SPC, FINI_GC_SPC,
     &                          STRT_AE_SPC, FINI_AE_SPC,
     &                          STRT_NR_SPC, FINI_NR_SPC,
     &                          STRT_TR_SPC, FINI_TR_SPC

      REAL,    PRIVATE, ALLOCATABLE, SAVE :: SBUFF( :,:,: )        ! read buffer
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: VDEMIS_TEMP( :,:,:,: )
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: SVDEMIS_AE( :,:,:,: )  ! aerosol emis
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: S_EMBUFF( :,:,:,: )  ! in all grid cells

      INTEGER, PRIVATE ::  LOGDEV

C     Emission rate of all aerosol species interpolated to current time
         REAL, ALLOCATABLE, SAVE :: S_PM_EM( : )         ! in one grid cell
         REAL, ALLOCATABLE, SAVE :: S_EM_RATE( : )         ! in one grid cell
         REAL,    ALLOCATABLE, SAVE :: EM_PART ( :,: )     !
C *** PVB: temporary hack to speciate PMC
         REAL,    SAVE :: SPFC_ACORS
         REAL,    SAVE :: SPFC_ASO4
         REAL,    SAVE :: SPFC_ANO3
         REAL,    SAVE :: SPFC_ACL
         REAL,    SAVE :: SPFC_AH2O


      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION S_EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP   ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

         CHARACTER( 16 ) :: PNAME = 'S_EMIS_INIT'
         INTEGER STATUS
         CHARACTER( 120 ) :: XMSG = ' '


C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = INIT3()


         ALLOCATE ( SVDEMIS ( N_SPC_EMIS+1,EMLAYS,MY_NCOLS,MY_NROWS,
     &              NPMAX ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'SVDEMIS memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         STRT_GC_EMIS = 1
         FINI_GC_EMIS = N_GC_EMIS
         STRT_AE_EMIS = N_GC_EMIS + 1
         FINI_AE_EMIS = N_GC_EMIS + N_AE_EMIS
         STRT_NR_EMIS = N_GC_EMIS + N_AE_EMIS + 1
         FINI_NR_EMIS = N_GC_EMIS + N_AE_EMIS + N_NR_EMIS
         STRT_TR_EMIS = N_GC_EMIS + N_AE_EMIS + N_NR_EMIS + 1
         FINI_TR_EMIS = N_GC_EMIS + N_AE_EMIS + N_NR_EMIS + N_TR_EMIS

         STRT_GC_SPC = 1
         FINI_GC_SPC = N_GC_SPC
         STRT_AE_SPC = N_GC_SPC + 1
         FINI_AE_SPC = N_GC_SPC + N_AE_SPC
         STRT_NR_SPC = N_GC_SPC + N_AE_SPC + 1
         FINI_NR_SPC = N_GC_SPC + N_AE_SPC + N_NR_SPC
         STRT_TR_SPC = N_GC_SPC + N_AE_SPC + N_NR_SPC + 1
         FINI_TR_SPC = N_GC_SPC + N_AE_SPC + N_NR_SPC + N_TR_SPC

         ALLOCATE ( S_EMBUFF( MY_NCOLS,MY_NROWS,EMLAYS,N_EMIS_PM ),
     &                 STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** EMBUFF memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         S_EMBUFF = 0.0

         ALLOCATE ( VDEMIS_TEMP( N_SPC_EMIS+1,EMLAYS,NCOLS,NROWS ),
     &                          STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_TEMP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         VDEMIS_TEMP = 0.0   ! array assignment

         IF ( N_AE_SPC .GT. 0 ) THEN   ! all EMLAYS
            ALLOCATE ( SVDEMIS_AE( N_AE_EMIS,EMLAYS,MY_NCOLS,MY_NROWS ),
     &                    STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'SVDEMIS_AE memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            SVDEMIS_AE = 0.0   ! array assignment
         ENDIF


         ALLOCATE ( S_PM_EM( N_EMIS_PM ),
     &              S_EM_RATE( N_EMIS_PM ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
           XMSG = '*** S_PM_EM or S_EM_RATE memory allocation failed'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         S_PM_EM = 0.0
         S_EM_RATE = 0.0

         ALLOCATE ( EM_PART ( N_AEROSPC, N_MODE ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = '*** memory allocation failed for '
     &           // 'EM_PART'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         SPFC_ASO4  = REAL( ACORSEM_ASO4_FAC ) * AEROSPC( ASO4_IDX )%EMIS_SPLIT( N_MODE )
         SPFC_ANO3  = REAL( ACORSEM_ANO3_FAC ) * AEROSPC( ANO3_IDX )%EMIS_SPLIT( N_MODE )
         SPFC_ACL   = REAL( ACORSEM_ACL_FAC ) * AEROSPC( ACL_IDX )%EMIS_SPLIT( N_MODE )
         SPFC_AH2O  = REAL( ACORSEM_AH2O_FAC ) * AEROSPC( AH2O_IDX )%EMIS_SPLIT( N_MODE )
         SPFC_ACORS = REAL( ACORS_RENORM ) * AEROSPC( ACORS_IDX )%EMIS_SPLIT( N_MODE )

         SUCCESS = .TRUE.; RETURN


         END FUNCTION S_EMIS_INIT

C-----------------------------------------------------------------------
         SUBROUTINE GET_SEMIS ( MDATE, MTIME, JDATE, JTIME, TSTEP, CONVPA )

         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME

         IMPLICIT NONE

C Arguments:

         INTEGER MDATE, MTIME, JDATE, JTIME
         INTEGER TSTEP( 2 )                 ! time step vector (HHMMSS)
         REAL    CONVPA             ! conversion factor to pressure in Pascals
c        REAL    RDX3F( NLAYS )             ! reciprocal layer thickness
c        REAL    RRHOJ( NCOLS,NROWS,NLAYS ) ! reciprocal density X Jacobian
         REAL    RJACM( NCOLS,NROWS,NLAYS ) ! reciprocal mid-layer Jacobian

         INTEGER COL, ROW, LAY, SPC, EMIS_SPC
         INTEGER N, V, S
         INTEGER FILENUM, LAYSNUM

         LOGICAL :: TIMEFLAG = .TRUE.
         INTEGER DATENUM, HOURNUM

         REAL             CNVTC         ! combined conversion factor
         REAL             CNVTI         ! intermediate combined conv. factor

         CHARACTER( 16 ) :: PNAME = 'GET_SEMIS'
         CHARACTER( 120 ) :: XMSG = ' '

         REAL    CONVEM          ! conversion for emissions rates to Kg/s
         INTEGER STATUS
         LOGICAL :: SUCCESS

         INTEGER FDATE

         INTEGER          S_STRT, S_END ! substitute loop induction variables


C-----------------------------------------------------------------------

C Set the date and hour counters used in sensitivity calls
         DATENUM = 1 + JDATE - STARTDATE
         HOURNUM = 1 + FLOOR ( REAL ( JTIME / 10000.0 ) )

         SVDEMIS = 0.0

c loop through all sensitivity parameters 
         DO NP = 1, NPMAX

            IF ( IPT( NP ) .NE. 3 ) CYCLE   ! do nothing if not an emissions sensitivity

c total emissions
            IF ( ISPLIT( NP, 1 ) .EQ. 1 ) THEN ! use all emissions
               SVDEMIS( :,:,:,:,NP ) = VDEMIS
               GOTO 100
            END IF

c gridded file emissions
            IF ( ISPLIT( NP, 2 ) .EQ. 1 ) THEN ! add emissions from gridded files

               IF ( ES ) THEN

                  DO FILENUM = 1, GRID_NUM( NP )  ! cycle through all files supplied

                     CALL S_OPEMIS( MDATE, MTIME, N_GC_EMIS, EM_TRAC,
     &                               CONVEM, LAYSNUM, GRID_FILE( NP, FILENUM ), FDATE )

                     ALLOCATE ( SBUFF( NCOLS,NROWS,LAYSNUM ), STAT = STATUS )
                     IF ( STATUS .NE. 0 ) THEN
                        XMSG = 'BUFF memory allocation failed'
                        CALL M3WARN ( PNAME, MDATE, MTIME, XMSG )
                        SUCCESS = .FALSE.; RETURN
                     END IF
                     SBUFF = 0.0   ! array assignment

c add the emissions from this file

C reactive gases (conversion to ppmv/s) 
                     DO V = STRT_GC_EMIS, FINI_GC_EMIS

                        S = V + 1 - STRT_GC_EMIS
                        SBUFF = 0.0   ! array assignment
                        IF ( .NOT. INTERPX( GRID_FILE( NP, FILENUM ),
     &                          GC_EMIS( S ), PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,LAYSNUM,
c    &                          MDATE, MTIME, SBUFF ) ) THEN
     &                          FDATE, MTIME, SBUFF ) ) THEN
                           XMSG = 'Could not read  '
     &                            // TRIM(GC_EMIS( S ))
     &                            // ' from ' // GRID_FILE( NP, FILENUM )
c                          CALL M3WARN( PNAME, MDATE, MTIME, XMSG )
                        END IF

                        CNVTI = CNVTP * GC_EMIS_FAC( S )

                        DO ROW = 1, MY_NROWS
                           DO COL = 1, MY_NCOLS
                              DO LAY = 1, LAYSNUM
                                 CNVTC = CNVTI * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY )

                                 VDEMIS_TEMP( V,LAY,COL,ROW ) =
     &                                    CNVTC * SBUFF( COL,ROW,LAY )

                                 SVDEMIS( V,LAY,COL,ROW,NP ) =
     &                                    SVDEMIS( V,LAY,COL,ROW,NP )
     &                                    + VDEMIS_TEMP( V,LAY,COL,ROW )
                              END DO
                           END DO
                        END DO

                     END DO   ! V

C aerosol emissions

                     IF ( N_AE_SPC .GT. 0 ) THEN   ! all EMLAYS

c                       IF ( .NOT. S_AERO_EMIS_INIT ( MDATE, MTIME,
                        IF ( .NOT. S_AERO_EMIS_LOAD ( FDATE, MTIME,
     &                    TSTEP, LAYSNUM, GRID_FILE( NP, FILENUM ) ) ) THEN
                           XMSG = 'Failure in S_AERO_EMIS_LOAD'
                           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                        END IF

                        SVDEMIS_AE = 0.0
                        CALL GET_S_AERO_EMIS ( MDATE, MTIME, TSTEP, 
     &                                         LAYSNUM, RJACM  )

                        DO ROW = 1, MY_NROWS
                           DO COL = 1, MY_NCOLS
                              DO LAY = 1, LAYSNUM
                                 DO V = STRT_AE_EMIS, FINI_AE_EMIS
                                    S = V + 1 - STRT_AE_EMIS
                                    SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                    + SVDEMIS_AE( S,LAY,COL,ROW )

                                 END DO
                              END DO
                           END DO
                        END DO

                     END IF
C non-reactive gases (conversion to ppmv/s) 

                     DO V = STRT_NR_EMIS, FINI_NR_EMIS
                        S = V + 1 - STRT_NR_EMIS
                        SBUFF = 0.0   ! array assignment
                        IF ( .NOT. INTERPX( GRID_FILE( NP, FILENUM ),
     &                             NR_EMIS( S ), PNAME,
     &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,LAYSNUM,
c    &                             MDATE, MTIME, SBUFF ) ) THEN
     &                             FDATE, MTIME, SBUFF ) ) THEN
                           XMSG = 'Could not read  '
     &                            // TRIM(NR_EMIS( S ))
     &                            // ' from ' // GRID_FILE( NP, FILENUM )
c                          CALL M3WARN( PNAME, MDATE, MTIME, XMSG  )
                        END IF

                        CNVTI = CNVTP * NR_EMIS_FAC( S )
                        DO ROW = 1, MY_NROWS
                           DO COL = 1, MY_NCOLS
                              DO LAY = 1, LAYSNUM
                                 CNVTC = CNVTI * Grid_Data%RDX3F( LAY )* Met_Data%RRHOJ( COL,ROW,LAY )

                                 VDEMIS_TEMP( V,LAY,COL,ROW ) =
     &                                    CNVTC * SBUFF( COL,ROW,LAY )

                                 SVDEMIS( V,LAY,COL,ROW,NP ) =
     &                                    SVDEMIS( V,LAY,COL,ROW,NP )
     &                                    + VDEMIS_TEMP( V,LAY,COL,ROW )
                              END DO
                           END DO
                        END DO

                     END DO

C add tracers later



                  DEALLOCATE ( SBUFF )

                  END DO

               ELSE
                  XMSG = 'Split emissions  sensitivity (ES) not set'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
               END IF

            END IF

c inline biogenics emissions
            IF ( ISPLIT( NP, 3 ) .EQ. 1 ) THEN ! add emissions from inline biogenics

               IF ( BIOGEMIS ) THEN
                  LAY = 1
                  CNVTI = CONVPA * CNVTE * Grid_Data%RDX3F( LAY )  ! per sec
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        CNVTC = CNVTI * Met_Data%RRHOJ( COL,ROW,LAY )
                        N = 0
                        DO S = 1, MSPCS
                           V = BEIS_MAP( S )
                           IF ( V .GT. 0 ) THEN
                              N = N + 1
                              SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                    + CNVTC * VDEMIS_BI( N,COL,ROW )
                           END IF
                        END DO
                    END DO
                  END DO
               ELSE
                  XMSG = 'Can not specify sensitivity to inline beis'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
               END IF

            END IF

C marine gas emissions

            IF ( ISPLIT( NP, 6 ) .EQ. 1 ) THEN ! add emissions from marine gases

               IF ( MGEM ) THEN
                  LAY = 1
                  CNVTI = CONVPA * CNVTE * Grid_Data%RDX3F( LAY )  ! per sec
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        CNVTC = CNVTI * Met_Data%RRHOJ( COL,ROW,LAY )
                        N = 0
                        DO S = 1, NMGSPC
                           V = MGSPC_MAP( S )
                           IF ( V .GT. 0 ) THEN
                              N = N + 1
                              SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                       + CNVTC * VDEMIS_MG( N,COL,ROW)
                           END IF
                        END DO
                     END DO
                  END DO
               ELSE
                  XMSG = 'Can not calculate marine gas sensitivity without marine gas emissions'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
               END IF

            END IF


c lightning emissions

            IF ( ISPLIT( NP, 5 ) .EQ. 1 ) THEN ! add emissions from lightning
         
               IF ( LTNG_NO ) THEN
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           CNVTI = CONVPA * CNVTE * Grid_Data%RDX3F( LAY )  ! per sec
                           CNVTC = CNVTI * Met_Data%RRHOJ( COL,ROW,LAY )
                           V = LTNG_MAP
                           SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                                 + CNVTC
     &                                                 * VDEMIS_LT( COL,ROW,LAY )

                        END DO
                     END DO
                  END DO
               ELSE
                  XMSG = 'Can not calculate lightning sensitivity without lightning emissions'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
               END IF

            END IF

c inline point sources
            IF ( ISPLIT( NP, 4 ) .EQ. 1 ) THEN  ! add emissions from inline sources

               IF ( PT3DEMIS ) THEN
c gas phase point sources

                  S_STRT = 1; S_END = N_GC_EMIS
                  IF ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 ) THEN
                     DO ROW = 1, MY_NROWS
                        DO COL = 1, MY_NCOLS
                           DO LAY = 1, EMLAYS
                              CNVTC = CNVTP * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY )
                              DO S = S_STRT, S_END
                                 V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                                 N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                                 IF ( V .LT. 1 ) CYCLE
                                 SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                                       + CNVTC 
     &                                                       * SVDEMIS_PT( COL,ROW,LAY,N,NP )
                                 IF ( GC_SPC( GC_EMIS_MAP( S ) ) .EQ. 'PCVOC' ) THEN
                                    SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                                          - CNVTC 
     &                                                          * SVDEMIS_PT_FIRE( COL,ROW,LAY,N,NP )
                                 END IF
                              END DO
                           END DO
                        END DO
                     END DO

                  ELSE

                     DO ROW = 1, MY_NROWS
                        DO COL = 1, MY_NCOLS
                           DO LAY = 1, EMLAYS
                              CNVTC = CNVTP * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY )
                              DO S = S_STRT, S_END
                                 V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                                 N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                                 IF ( V .LT. 1 ) CYCLE
                                 SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                                       + CNVTC 
     &                                                       * SVDEMIS_PT( COL,ROW,LAY,N,NP )
                              END DO
                           END DO
                        END DO
                     END DO

                  END IF


                  S_STRT = S_END + 1; S_END = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           CNVTC = CNVTP * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY )
                           DO S = S_STRT, S_END
                              V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                              N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                              IF ( V .LT. 1 ) CYCLE
                              SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                                    + CNVTC 
     &                                                    * SVDEMIS_PT( COL,ROW,LAY,N,NP )

                           END DO
                        END DO
                     END DO
                  END DO

c aerosol point sources
                  IF ( N_AE_SPC .GT. 0 ) THEN
c need only SULF out of the gas array, but it's easier to get it all
                     DO V = STRT_GC_EMIS, FINI_GC_EMIS
                        DO ROW = 1, MY_NROWS
                           DO COL = 1, MY_NCOLS
                              DO LAY = 1, EMLAYS
                                 VDEMIS_TEMP( V,LAY,COL,ROW ) = SVDEMIS( V,LAY,COL,ROW,NP )
                              END DO
                           END DO
                        END DO
                     END DO

                     DO S = 1, N_SPC_PTPM   ! merge inline point src PM species
                        V = PTPM_MAP( S )   ! set in PT3D_DEFN
                        DO LAY = 1, EMLAYS
                           DO ROW = 1, MY_NROWS
                              DO COL = 1, MY_NCOLS
                                 S_EMBUFF( COL,ROW,LAY,V ) = SPMEMIS_PT( COL,ROW,LAY,S,NP )
                              END DO
                           END DO
                        END DO
                     END DO 

                     SVDEMIS_AE = 0.0
                     CALL GET_S_AERO_EMIS ( MDATE, MTIME, TSTEP, EMLAYS, RJACM )

                     DO ROW = 1, MY_NROWS
                        DO COL = 1, MY_NCOLS
                           DO LAY = 1, EMLAYS
                              DO V = STRT_AE_EMIS, FINI_AE_EMIS
                                 S = V + 1 - STRT_AE_EMIS
                                 SVDEMIS( V,LAY,COL,ROW,NP ) = SVDEMIS( V,LAY,COL,ROW,NP )
     &                                                       + SVDEMIS_AE( S,LAY,COL,ROW )
                              END DO
                           END DO
                        END DO
                     END DO

                  END IF

               ELSE
                  XMSG = 'PT3DEMIS not set to TRUE'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
               END IF

            END IF

100         CONTINUE

            VDEMIS( N_SPC_EMIS+1,:,:,: ) = 0.0

c subset by IREGION, IDATE, and ITIME
            CALL CKTIME( JDATE,JTIME,NP,TIMEFLAG) ! Check if the current time is within the time range

            IF ( TIMEFLAG ) THEN
              DO ROW = 1, MY_NROWS
                 DO COL = 1, MY_NCOLS
                    DO LAY = 1, EMLAYS
                       DO SPC = 1, N_SPC_EMIS
                          SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                                 SVDEMIS( SPC,LAY,COL,ROW,NP ) *
     &                                 IREGION( COL,ROW,LAY,NP ) *
     &                                 REAL( IDATE( NP,DATENUM ) )
                       END DO
                    END DO
                 END DO
              END DO

            ELSE
              SVDEMIS( :,:,:,:,NP ) = 0.0
            END IF

c subset by species (IPARM) and apply AMOUNT if specified

c gas species
            DO SPC = STRT_GC_EMIS, FINI_GC_EMIS
               IF ( IAMOUNT( NP, GC_EMIS_MAP( SPC ), 25 ) .EQ. 0.0 ) THEN
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                     SVDEMIS( SPC,LAY,COL,ROW,NP ) *
     &                     REAL( IPARM( NP, GC_EMIS_MAP( SPC ) ) )
                        END DO
                     END DO
                  END DO
               ELSE
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                     IAMOUNT( NP, GC_EMIS_MAP( SPC ), HOURNUM ) *
     &                     CNVTE * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY ) *
     &                     REAL( ILAYER ( NP, LAY ) ) *
     &                     IREGION( COL, ROW, LAY, NP ) *
     &                     REAL( IDATE ( NP, DATENUM ) ) *
     &                     REAL( IPARM ( NP, GC_EMIS_MAP( SPC ) ) )
                        END DO
                     END DO
                  END DO
               END IF
            END DO

c aerosol species
            EMIS_SPC = 0
            DO SPC = STRT_AE_EMIS, FINI_AE_EMIS
               EMIS_SPC = EMIS_SPC + 1

               IF ( IAMOUNT( NP, STRT_AE_SPC + AE_EMIS_MAP( EMIS_SPC ),
     &              25 ) .EQ. 0.0 ) THEN
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                     SVDEMIS( SPC,LAY,COL,ROW,NP ) *
     &                     REAL( IPARM( NP, STRT_AE_SPC + AE_EMIS_MAP( EMIS_SPC ) ) )
                        END DO
                     END DO
                  END DO
               ELSE
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                     IAMOUNT( NP, STRT_AE_SPC + AE_EMIS_MAP( EMIS_SPC ), HOURNUM ) *
     &                     CNVTE * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY ) *
     &                     REAL( ILAYER ( NP, LAY ) ) *
     &                     IREGION( COL, ROW, LAY, NP ) *
     &                     REAL( IDATE ( NP, DATENUM ) ) *
     &                     REAL( IPARM ( NP, STRT_AE_SPC + AE_EMIS_MAP( EMIS_SPC ) ) )
                        END DO
                     END DO
                  END DO
               END IF
            END DO

c nonreactive species

            EMIS_SPC = 0
            DO SPC = STRT_NR_EMIS, FINI_NR_EMIS
               EMIS_SPC = EMIS_SPC + 1
               IF ( IAMOUNT( NP, STRT_NR_SPC + NR_EMIS_MAP( EMIS_SPC ), 
     &              25 ) .EQ. 0.0 ) THEN
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                     SVDEMIS( SPC,LAY,COL,ROW,NP ) *
     &                     REAL( IPARM( NP, STRT_NR_SPC + NR_EMIS_MAP( EMIS_SPC ) ) )
                        END DO
                     END DO
                  END DO
               ELSE
                  DO ROW = 1, MY_NROWS
                     DO COL = 1, MY_NCOLS
                        DO LAY = 1, EMLAYS
                           SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                     IAMOUNT( NP, STRT_NR_SPC + NR_EMIS_MAP( EMIS_SPC ), HOURNUM ) *
     &                     CNVTE * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY ) *
     &                     REAL( ILAYER ( NP, LAY ) ) *
     &                     IREGION( COL, ROW, LAY, NP ) *
     &                     REAL( IDATE ( NP, DATENUM ) ) *
     &                     REAL( IPARM ( NP, STRT_NR_SPC + NR_EMIS_MAP( EMIS_SPC ) ) )
                        END DO
                     END DO
                  END DO
               END IF
            END DO

c tracer species

            IF ( N_TR_EMIS .GE. 1 ) THEN

              EMIS_SPC = 0
              DO SPC = STRT_TR_EMIS, FINI_TR_EMIS
                 EMIS_SPC = EMIS_SPC + 1
                 IF ( IAMOUNT( NP, STRT_TR_SPC + TR_EMIS_MAP( EMIS_SPC ),
     &                25 ) .EQ. 0.0 ) THEN
                    DO ROW = 1, MY_NROWS
                       DO COL = 1, MY_NCOLS
                          DO LAY = 1, EMLAYS
                             SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                       SVDEMIS( SPC,LAY,COL,ROW,NP ) *
     &                       REAL( IPARM( NP, STRT_TR_SPC + TR_EMIS_MAP( EMIS_SPC ) ) )
                          END DO
                       END DO
                    END DO
                 ELSE
                    DO ROW = 1, MY_NROWS
                       DO COL = 1, MY_NCOLS
                          DO LAY = 1, EMLAYS
                             SVDEMIS( SPC,LAY,COL,ROW,NP ) =
     &                       IAMOUNT( NP, STRT_TR_SPC + TR_EMIS_MAP( EMIS_SPC ), HOURNUM ) *
     &                       CNVTE * Grid_Data%RDX3F( LAY ) * Met_Data%RRHOJ( COL,ROW,LAY ) *
     &                       REAL( ILAYER ( NP, LAY ) ) *
     &                       IREGION( COL, ROW, LAY, NP ) *
     &                       REAL( IDATE ( NP, DATENUM ) ) *
     &                       REAL( IPARM ( NP, STRT_TR_SPC + TR_EMIS_MAP( EMIS_SPC ) ) )
                          END DO
                       END DO
                    END DO
                 END IF
              END DO

            END IF

         END DO

         END SUBROUTINE GET_SEMIS
C-----------------------------------------------------------------------

         FUNCTION S_AERO_EMIS_LOAD( MDATE, MTIME, TSTEP, LAYSNUM, EMISFILE )
     &                   RESULT ( SUCCESS )

         IMPLICIT NONE

         INTEGER      MDATE                      ! current model date, coded YYYYDDD
         INTEGER      MTIME                      ! current model time, coded HHMMSS
         INTEGER      TSTEP( 2 )                 ! time step vector (HHMMSS)
         INTEGER      LAYSNUM
         CHARACTER( 16 ) :: EMISFILE             ! emissions file to read

         LOGICAL :: SUCCESS

         CHARACTER( 16 ) :: PNAME = 'S_AERO_EMIS_LOAD'

         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER STATUS                   ! ENV..., ALLOCATE status
         INTEGER S, IDX


C-----------------------------------------------------------------------

         S_EMBUFF = 0.0
         DO S = 1, N_EMIS_PM   ! read 3d file
            IDX = PMEM_MAP( S )
c           IF ( .NOT. INTERPX( EMISFILE, AEROSPC( IDX )%EMIS, PNAME,
c    &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,LAYSNUM,
c    &                             MDATE, MTIME, S_EMBUFF( :,:,:,S ) ) ) THEN

            IF ( .NOT. INTERPX( EMISFILE, PMEM_MAP_NAME( S ), PNAME,
     &                        STARTCOL,ENDCOL, STARTROW,ENDROW,1,LAYSNUM,
     &                        MDATE, MTIME, S_EMBUFF( 1,1,1,S ) ) ) THEN

               XMSG = 'Could not read '
     &                 // TRIM( AEROSPC( IDX )%EMIS )
     &                 // ' from ' // EMISFILE
c              CALL M3WARN ( PNAME, MDATE, MTIME, XMSG  )
            END IF
         END DO

         SUCCESS = .TRUE.; RETURN

         END FUNCTION S_AERO_EMIS_LOAD

c-----------------------------------------------------------------------

         SUBROUTINE GET_S_AERO_EMIS ( MDATE, MTIME, TSTEP, LAYSNUM, RJACM ) 
 
         USE PRECURSOR_DATA
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA

         IMPLICIT NONE

         INCLUDE SUBST_CONST     ! physical and mathematical constants

         INTEGER      MDATE                      ! current model date, coded YYYYDDD
         INTEGER      MTIME                      ! current model time, coded HHMMSS
         INTEGER      TSTEP( 2 )                 ! time step vector (HHMMSS)
         INTEGER      LAYSNUM
         REAL         RJACM( NCOLS,NROWS,NLAYS ) ! reciprocal mid-layer Jacobian

         CHARACTER( 16 ) :: PNAME = 'GET_S_AERO_EMIS'
c        LOGICAL, SAVE     :: FIRSTIME = .TRUE.
         INTEGER STATUS                   ! ENV..., ALLOCATE status
c        INTEGER, SAVE     :: LOGDEV
         CHARACTER( 120 ) :: XMSG = ' '


C     Emission rate of all aerosol species interpolated to current time
c        REAL, ALLOCATABLE, SAVE :: S_PM_EM( : )         ! in one grid cell
c        REAL, ALLOCATABLE, SAVE :: S_EM_RATE( : )         ! in one grid cell
c        REAL,    ALLOCATABLE, SAVE :: EM_PART ( :,: )     !
C Third moment emissions rates [m3/m3/s]
         REAL( 8 ) :: EMISM3( N_MODE )
C Number emissions rates [1/m3/s]
         REAL( 8 ) :: EM_NUM( N_MODE )
C Surface area emission rates [m2/m3/s]
         REAL( 8 ) :: EM_SRF( N_MODE )
C Variables for converting mass emissions rate to number emissions rate
         REAL,    SAVE :: FACNUM( N_MODE )
C Variables for converting mass emissions rate to 2nd moment emissions rate
         REAL,    SAVE :: FACSRF( N_MODE )

         INTEGER N, S, IDX
         INTEGER COL, ROW, LAY

         REAL  GSFAC                                ! into [ug/m3/sec]
C Variables for converting emission rates into molar-mixing-ratio units
         REAL, PARAMETER :: RAVO = 1.0 / AVO   ! reciprocal of Avogadro
         REAL  CONVM     ! conversion factor for mass emissions [m3/mol]
         REAL  CONVN     ! conversion factor for number emissions [1e6*m3]
         REAL  CONVS     ! conversion factor for surface area emissions [m3/mol]

         REAL, PARAMETER :: GPKG = 1.0E+03     ! g/kg
         REAL, PARAMETER :: F6DPI = 6.0 / PI
         REAL, PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI

C *** PVB: temporary hack to speciate PMC
c        REAL,    SAVE :: SPFC_ACORS
c        REAL,    SAVE :: SPFC_ASO4
c        REAL,    SAVE :: SPFC_ANO3
c        REAL,    SAVE :: SPFC_ACL
c        REAL,    SAVE :: SPFC_AH2O



C-----------------------------------------------------------------------

c        IF ( FIRSTIME ) THEN
c           FIRSTIME = .FALSE.
c           LOGDEV = INIT3()

c           ALLOCATE ( S_PM_EM( N_EMIS_PM ), 
c    &                 S_EM_RATE( N_EMIS_PM ), STAT = STATUS )
c           IF ( STATUS .NE. 0 ) THEN
c              XMSG = '*** S_PM_EM or S_EM_RATE memory allocation failed'
c              CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
c           END IF
c           S_PM_EM = 0.0
c           S_EM_RATE = 0.0

c           ALLOCATE ( EM_PART ( N_AEROSPC, N_MODE ), STAT = STATUS )
c           IF ( STATUS .NE. 0 ) THEN
c              XMSG = '*** memory allocation failed for '
c    &              // 'EM_PART' 
c              CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
c           END IF

C *** PVB: temporary hack to speciate PMC
c           SPFC_ACORS = ( 1.0 - 0.00325 ) * AEROSPC( ACORS_IDX )%EMIS_SPLIT( N_MODE )
c           SPFC_ASO4  = 0.00100 * AEROSPC( ASO4_IDX )%EMIS_SPLIT( N_MODE )
c           SPFC_ANO3  = 0.00048 * AEROSPC( ANO3_IDX )%EMIS_SPLIT( N_MODE )
c           SPFC_ACL   = 0.00145 * AEROSPC( ACL_IDX )%EMIS_SPLIT( N_MODE )
c           SPFC_AH2O  = 0.00032 * AEROSPC( AH2O_IDX )%EMIS_SPLIT( N_MODE )

c        END IF    ! FIRSTIME

         DO LAY = 1, LAYSNUM
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS

C *** Store aerosol emission rates for this grid cell in a 1D array
                  DO N = 1, N_EMIS_PM
                     S_PM_EM( N ) = S_EMBUFF( COL,ROW,LAY,N )
                  END DO

C *** Calculate scaling factor for converting mass emissions into [ug/m3/s]
C     note: RJACM converts grid heights from sigma coordinates to meters
C     Also calculate scaling factors for converting to molar-mixing-ratio units
                  GSFAC = CONVEM_PM( LAY ) * Met_Data%RJACM( COL,ROW,LAY )
                  CONVM = MWAIR / GPKG / DENS( COL,ROW,LAY )   ! [m3/mol]
                  CONVN = CONVM * RAVO * 1.0E+06         ! 10^6 * [m3]
                  CONVS = CONVM                          ! [m3/mol]

C *** Calculate speciated mass emission rates for fine aerosol [ug/m3/s]
                  DO S = 1, N_EMIS_PM
                     S_EM_RATE( S ) = S_PM_EM( S ) * GSFAC
                  END DO

C *** Extract H2SO4 vapor emission rate from the VDEMIS array, add it to
C     the fine-PM sulfate emissions, and remove it from the gas emissions
                  S_EM_RATE( VPSO4 ) = S_EM_RATE( VPSO4 ) + VDEMIS_TEMP( VSULF,LAY,COL,ROW )
     &                               *  REAL( PRECURSOR_MW( SULF_IDX ), 4 ) / CONVM
                  VDEMIS_TEMP( VSULF,LAY,COL,ROW ) = 0.0

C *** Calculate emissions rate for third moments [m3/m3/s] of each mode 
C     (excluding sea salt and windblown dust), as in Equation 7a of
C     Binkowski & Roselle (2003).
                  EM_PART = 0.0   ! array assignment
                  EMISM3 = 0.0    ! array assignment
                  DO S = 1, N_EMIS_PM
                     IDX = PMEM_MAP( S )
                     DO N = 1, N_MODE - 1
c                       EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N )
c    &                                   * S_EM_RATE( S )
                        EM_PART( IDX,N ) = EM_PART( IDX,N ) +
     &                        AEROSPC( IDX )%EMIS_SPLIT( N ) * S_EM_RATE( S )
     &                            * EM_FAC( IDX,N )
                        IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
c                          EMISM3( N ) = EMISM3( N ) + EM_PART( IDX,N )
c    &                              * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                           EMISM3( N ) = EMISM3( N ) +
     &                        AEROSPC( IDX )%EMIS_SPLIT( N ) * S_EM_RATE( S )
     &                          * EM_FAC( IDX,N ) * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                        END IF
                     END DO 
                     N = N_MODE
                     IF ( IDX .EQ. ASO4_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ASO4 * S_EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. ANO3_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ANO3 * S_EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. ACL_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ACL * S_EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. AH2O_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_AH2O * S_EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE IF ( IDX .EQ. ACORS_IDX ) THEN
                        EM_PART( IDX,N ) = SPFC_ACORS * S_EM_RATE( S ) * EM_FAC( IDX,N )
                     ELSE
c                        EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N ) * S_EM_RATE( S )
                         EM_PART( IDX,N ) = AEROSPC( IDX )%EMIS_SPLIT( N )
     &                                    * S_EM_RATE( S ) * EM_FAC( IDX,N )
                     END IF
                     IF ( .NOT. AEROSPC( IDX )%TRACER ) THEN
                        EMISM3( N ) = EMISM3( N ) + EM_PART( IDX,N )
     &                              * ( F6DPIM9 / AEROSPC( IDX )%DENSITY )
                     END IF
                  END DO

C *** Calculate the number emissions rate for each mode [1/m3/s], using 
C     Equation 7b of Binkowski & Roselle (2003).
                  DO N = 1, N_MODE
                     EM_NUM( N ) = FACNUM( N ) * EMISM3( N )
                  END DO

C *** Calculate the surface area emissions rate for the fine modes [m2/m3/s],
C     using Equation 7c of Binkowski & Roselle (2003).  Multiplying by PI 
C     converts 2nd moment to surface area.
                  DO N = 1, N_MODE
                     EM_SRF( N ) = FACSRF( N ) * EMISM3( N )
                  END DO

C *** Convert emission rates into molar-mixing-ratio units, as required by
C     the vertical diffusion routines.  Mass and number emissions are converted
C     to [ppmv/s].  Note: EM_PART has units of [ug/m3/s] and mol(s)/mol(air) is
C     1.0e6 ppmv; so 1.0e-6 ug/g cancels the 1.0e6 ppmv/[mol(s)/mol(air)].
C     Surface area emissions are converted to [m2/mol/s].
C     Save results in the VDEMIS_AE array.

                  DO N = 1, N_MODE
                     DO S = 1, N_AEROSPC
                        IF ( EM_MAP( S,N ) .GT. 0 ) THEN
                           SVDEMIS_AE( EM_MAP( S,N ),LAY,COL,ROW ) = EM_PART( S,N )
     &                                                             * ( CONVM / AEROSPC_MW( S ) )
                        END IF
                     END DO
                     SVDEMIS_AE( NUM_MAP( N ),LAY,COL,ROW ) = EM_NUM( N ) * NUM_FAC( N ) * CONVN
                     SVDEMIS_AE( SRF_MAP( N ),LAY,COL,ROW ) = EM_SRF( N ) * SRF_FAC( N ) * CONVS
                  END DO

               END DO   ! loop on MY_NCOLS
            END DO   ! loop on MY_NROWS
         END DO ! LAY

         END SUBROUTINE GET_S_AERO_EMIS

C-----------------------------------------------------------------------

#endif

      END MODULE S_EMIS_DEFN
