
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
      MODULE EMIS_DEFN

C-----------------------------------------------------------------------
C Function: emissions interface to the chemistry-transport model

C Revision History:
C     28 Jul 2006 J.Young: initial implementation
C     18 Aug 2007 J.Young: move beis part to separate module; add plume rise
C     23 Sep 2009 B.Hutzell: modified algorithm that loads gas emissions from point
C                 sources into VDEMIS array to enable multi-use of an emission species
C     26 Jan 2010 J.Young: fix bug overwriting point source layer 1 NH3; inline rdemis
C     07 Jan 2011 B.Hutzell: updated for namelist definition of model species
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                            removed deprecated TRIMLEN
C      6 Apr 2011 J.Young, R.Pinder: add lightning NO emissions capability
C     11 May 2011 D.Wong: incorporated twoway model implementation
C      5 Jun 2012 J.Bash: Added support for NH3 bidirectional exchange. Fertilizer
C                         sector emissions are subtracted from the total NH3 emissions
C                         if the CTM_ABFLUX flag is set
C     07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C     24 Feb 16 B.Murphy: Generalize scaling of point source species based on
C                         mapping model species, not point source species
C     03 Mar 16 B.Gantt/G. Sarwar: incorporated halogen emissions
C     08 Aug 2016 B.Murphy: Neglect fire emissions for pcVOC
C     12 Jan 2017 B.Murphy: Remove warning when model species are not
C                           read in correctly. Invoke error and model stop when model 
C                           species are not found on any emission file
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications      

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array
#ifdef verbose_emis_defn
      real,    allocatable, save :: semis( :,:,: )
#endif

      INTEGER,              SAVE :: N_SPC_EMIS ! global emissions species
      INTEGER,              SAVE :: EMLAYS     ! no. of emis integration layers
                                               ! EMLAYS must be .LT. NLAYS
      LOGICAL,              SAVE :: EMISCHK ! Let model proceed even if Emis Surrogates 
                                               !are not properly defined
      INTEGER,              SAVE :: EMLYRS     ! no. of layers on base emis input file
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TRACSPC( : )  ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: AREASPC( : )  ! Variables from Area Emission File

      PUBLIC VDEMIS, N_SPC_EMIS, EMLAYS, EMLYRS, TRACSPC, AREASPC,  ! Variables
     &       EMIS_INIT, GET_EMIS, EMISCHK                           ! Routines
      
#ifdef verbose_emis_defn
      public semis
#endif
      PRIVATE
                                           ! a 2d file if inline, otherwise a 3d file
      INTEGER, SAVE :: NEMIS_AE   ! no. of aero emis species

      LOGICAL, SAVE :: EM_TRAC = .FALSE. ! do tracer emissions?
      REAL,    SAVE :: CNVTE      ! combined conversion factor
      REAL,    SAVE :: DT         ! TSTEP (output) in sec

      REAL,    ALLOCATABLE, SAVE :: BUFF( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: BUFF2( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: FERT( :,:,: )        ! read buffer

      INTEGER, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                 STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, SAVE :: NDATE, NTIME  ! step next date/time (beis)

#ifdef verbose_emis_defn
      character( 16 ), allocatable, save :: emis_spc( : ) ! global emis surrogates
      integer,         allocatable, save :: emis_map( : )  ! name map to CGRID
#endif

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE BEIS_DEFN           ! biogenic emissions
         USE MGEMIS              ! marine gas emissions
         USE LTNG_DEFN           ! NO emissions from lightning strikes
         USE PT3D_DEFN           ! plume rise emissions
         USE UTILIO_DEFN         ! I/O API
         USE AERO_EMIS           ! inherits GRID_CONF
         USE AERO_DATA, ONLY: MAP_PMEMIS  ! access subroutine map_pmemis
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP   ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

C Parameters:
         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio

C Local Variables:
         REAL    CONVEM          ! conversion for emissions rates to Kg/s
         REAL    DX1, DX2        ! CX x1- and x2-cell widths

         CHARACTER( 16 ), SAVE :: CTM_EMLAYS = 'CTM_EMLAYS'    ! env var for no. of
                                                               ! emiss. layers
         
         CHARACTER( 16 ) :: PNAME = 'EMIS_INIT'
         CHARACTER( 80 ) :: VARDESC   ! env variable description
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER V, LOGDEV, STATUS
#ifdef verbose_emis_defn
         integer s
#endif

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = INIT3()

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
         ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
         END IF

C Initialize Online Biogenic Emissions
         IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Online Marine Gas Emissions
         IF ( .NOT. MGEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing marine gas emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Open Area Emissions files
         CALL OPEMIS ( JDATE, JTIME, N_GC_EMIS, EM_TRAC, CONVEM, EMLAYS )

C Get number of emissions layers
         IF ( EMLAYS .LE. 1 ) THEN   ! better be inline plume rise run
            EMLAYS = NLAYS   ! env var default
            VARDESC = 'Number of emission layers'
            EMLAYS = ENVINT( CTM_EMLAYS, VARDESC, EMLAYS, STATUS )
            IF ( STATUS .NE. 0 ) THEN
               WRITE( LOGDEV, '(5X, A)' ) VARDESC
               IF ( STATUS .EQ. 1 ) THEN
                  XMSG = 'Environment variable improperly formatted'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               ELSE
                  XMSG = 'Environment variable not set or empty ... Using default:'
                  WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
               END IF
            END IF
         END IF

         EMLAYS = MIN ( EMLAYS, NLAYS )

         WRITE( LOGDEV,1009 ) EMLAYS, NLAYS
1009     FORMAT( / 10X, 'Number of Emissions Layers:         ', I3
     &           / 10X, 'out of total Number of Model Layers:', I3 )

C Initialize Online Lightning NOx Emissions
         IF ( .NOT. LTNG_INIT( JDATE, JTIME, TSTEP, EMLAYS ) ) THEN
            XMSG = 'Failure initializing lightning emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Aerosol Emissions         
         IF ( N_AE_SPC .GT. 0 ) THEN
            WRITE( LOGDEV,'( /5X, A )' ) 'Aerosol Emissions Processing in '
     &                                // 'Vertical diffusion ...'
            NEMIS_AE = N_AE_EMIS  ! from CGRID_SPCS (originally, AE_EMIS.EXT)
            CALL MAP_PMEMIS ( )   ! set up PM emitted species mapping
         ELSE
            NEMIS_AE = 0
         END IF

         PM_EMLYRS = EMLAYS   ! set PM_EMLYRS in AERO_EMIS module

         IF ( .NOT. AERO_EMIS_INIT( JDATE, JTIME, TSTEP, NEMIS_AE ) ) THEN
            XMSG = 'Failure initializing aerosol emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Map Emissions Surrogates to Global Variables
         N_SPC_EMIS = N_GC_EMIS
     &              + NEMIS_AE
     &              + N_NR_EMIS
     &              + N_TR_EMIS

#ifdef verbose_emis_defn
         allocate ( emis_spc ( n_spc_emis+1 ),
     &              emis_map ( n_spc_emis+1 ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = 'emis_spc or emis_map memory allocation failed'
            call m3warn ( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if
         emis_spc = ' '   ! array assignment
         emis_map = 0     ! array assignment

         s = 0
         do v = 1, n_gc_emis
            s = s + 1
            emis_spc( s ) = gc_emis( v )
            emis_map( s ) = gc_strt - 1 + gc_emis_map( v )
         end do 
         do v = 1, n_ae_emis
            s = s + 1
            emis_spc( s ) = ae_emis( v )
            emis_map( s ) = ae_strt - 1 + ae_emis_map( v )
         end do
         do v = 1, n_nr_emis
            s = s + 1
            emis_spc( s ) = nr_emis( v )
            emis_map( s ) = nr_strt - 1 + nr_emis_map( v )
         end do
         do v = 1, n_tr_emis
            s = s + 1
            emis_spc( s ) = tr_emis( v )
            emis_map( s ) = tr_strt - 1 + tr_emis_map( v )
         end do
         do v = 1, n_spc_emis   ! debug
            write( logdev,'(a,2i4,3x,a)' ) ' v, emis_map(v), emis_spc(v): ',
     &                                       v, emis_map( v ), emis_spc( v )
         end do
#endif
 
C Combined Gas Emssions Conversion Factor
         CNVTE = CMLMR * CONVEM * MWAIR / ( DX1 * DX2 )

C Calculate Indices for Pointing Pollutant Types to Vector Bounds in
C Master EArea Emissions Array
         STRT_GC = 1
         FINI_GC = N_GC_EMIS
         STRT_AE = N_GC_EMIS + 1
         FINI_AE = N_GC_EMIS + NEMIS_AE
         STRT_NR = N_GC_EMIS + NEMIS_AE + 1
         FINI_NR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS
         STRT_TR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS + 1
         FINI_TR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS + N_TR_EMIS
 
C Initialize 3D Point Source Emissions 
C (N_SPC_EMIS+1 serves as the null map for PTEM_MAP)
         IF ( .NOT. PT3D_INIT( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( VDEMIS( N_SPC_EMIS+1,EMLAYS,NCOLS,NROWS ),
#ifdef verbose_emis_defn
     &              semis( emlays,my_ncols,my_nrows ),
#endif
     &              STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( PT3DEMIS ) THEN
            EMLYRS = 1
         ELSE
            EMLYRS = EMLAYS
         END IF

C Check the Emissions Species Available on the Input Files Against the
C Surrogates Identified by the User via the Namelists and Stop the model
C or Print Warnings if Mistakes Are Made.
         EMISCHK = .TRUE.   ! Env Variable Default
         CALL EMIS_SPC_CHECK( JDATE, JTIME )

C Warn the User if there are No Emissions Species On Any File         
         IF ( N_SPC_EMIS .LE. 0 ) THEN
            XMSG = 'There are no emissions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

C Allocate Buffer space for Reading Emissions
         ALLOCATE ( BUFF( NCOLS,NROWS,EMLYRS ), STAT = STATUS )
         ALLOCATE ( BUFF2( NCOLS,NROWS,EMLYRS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'BUFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         BUFF = 0.0   ! array assignment

C Allocate Buffer Space For Reading Fertilizer Emissions for MOSAIC
         IF ( ABFLUX ) THEN
            ALLOCATE ( FERT( NCOLS,NROWS,EMLYRS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'FERT memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            FERT = 0.0   ! array assignment
         END IF

C Return From Initialization
         SUCCESS = .TRUE.; RETURN

         END FUNCTION EMIS_INIT

C-----------------------------------------------------------------------
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA, CGRID )

C NEW APPROACH:
C Apply NML factors to all *input* emissions and inline plume rise, aero emis
C biog, SeaSalt, Dust, and Lightning NO
C GET_AERO_EMIS has to apply factors - don`t do it at this level.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE MGEMIS              ! marine gas emissions
         USE PT3D_DEFN           ! plume rise emissions
         USE PTMAP               ! defines emissions file(s) PM mapping
         USE LTNG_DEFN           ! lightning NO emissions
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL,    INTENT( IN ) :: CONVPA        ! conversion factor to pressure in Pascals
         REAL, POINTER :: CGRID( :,:,:,: )

C Local Variables:
         REAL             CNVTC         ! combined conversion factor
         REAL             CNVTI         ! intermediate combined conv. factor
         REAL,    SAVE :: CNVTP         ! intermediate combined conv. factor
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables

         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '
#ifdef verbose_emis_defn
         integer, save :: logdev
         real sumem
#endif
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
#ifdef verbose_emis_defn
            logdev = init3()
#endif
C Get domain decomp info from the emissions file
            CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
            CNVTP = CONVPA * CNVTE

         END IF    !FirstTime

C read & interpolate emissions (create VDEMIS in the species class order)

         VDEMIS = 0.0   ! array assignment

         DO V = STRT_GC, FINI_GC
            S = V + 1 - STRT_GC
            BUFF = 0.0   ! array assignment
C                                       vvvvvvv - emis surrogate names
            IF ( .NOT. INTERPX( EMIS_1, GC_EMIS( S ), PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read '
     &              // TRIM( GC_EMIS( S ) ) // ' from ' // EMIS_1
               IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF

            !READ Non-Carbon Organic Matter From Gas Emissions
            IF ( GC_EMIS( S ) .EQ. 'POC' ) THEN
                 BUFF2 = 0.0
                 IF ( .NOT. INTERPX( EMIS_1, 'PNCOM', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                 XMSG = 'Could not read '
     &               // TRIM( GC_EMIS( S ) ) // ' from ' // EMIS_1
                 IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              END IF  
              BUFF = BUFF + BUFF2
            ENDIF


            CNVTI = CNVTP * GC_EMIS_FAC( S )

C reactive gases (conversion to ppmv/s) VDEMIS in this order from RDEMIS
C "Stores read an entire cache line, modify the target, then write back the
C  entire line. Thus, non-consecutive stores are worse than non-consecutive
C  loads."
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTI * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * BUFF( C,R,L )
                  END DO
               END DO
            END DO

#ifdef verbose_emis_defn
            write( logdev,* ) '  GC EMIS: ', s, '  ', gc_emis( s )
#endif

         END DO   ! V

C Retrieve BEIS Emissions

         IF ( BIOGEMIS ) THEN

            CALL GET_BEIS ( JDATE, JTIME, TSTEP )

C tmpbeis312 is based on hourly input - need to interpolate return value
C merge: VDEMIS_BI in mol/hr. Convert to ppm/sec

#ifdef verbose_emis_defn
            do s = 1, mspcs
               v = beis_map( s )
               if ( v .gt. 0 ) then
                  write( logdev,* ) '  BEIS_MAP: ', s, v, '  ', gc_emis( v ), gc_emis_fac( v )
               end if
            end do
#endif
            L = 1
            CNVTI = CONVPA * CNVTE * Grid_Data%RDX3F( L )  ! per sec
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  CNVTC = CNVTI * Met_Data%RRHOJ( C,R,L )
                  N = 0
                  DO S = 1, MSPCS
                     V = BEIS_MAP( S )
                     IF ( V .GT. 0 ) THEN
                        N = N + 1
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + CNVTC * GC_EMIS_FAC( V )
     &                                            * VDEMIS_BI( N,C,R )
                      END IF
                  END DO
               END DO
            END DO

         END IF

C marine gas emissions
         IF ( MGEM ) THEN
            CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )

            L = 1
            CNVTI = CONVPA * CNVTE * Grid_Data%RDX3F( L )  ! per sec
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                 CNVTC = CNVTI * Met_Data%RRHOJ( C,R,L )
                  N = 0
                  DO S = 1, NMGSPC
                     V = MGSPC_MAP( S )
                     IF ( V .GT. 0 ) THEN
                        N = N + 1
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + CNVTC * GC_EMIS_FAC( V ) * VDEMIS_MG( N,C,R)
                      END IF
                  END DO
               END DO
            END DO

         END IF

C lightning NO

         IF ( LTNG_NO ) THEN
            CALL GET_LTNG ( JDATE, JTIME, TSTEP )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     CNVTI = CONVPA * CNVTE * Grid_Data%RDX3F( L )  ! per sec
                     CNVTC = CNVTI * Met_Data%RRHOJ( C,R,L )
                     V = LTNG_MAP
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * GC_EMIS_FAC( V )
     &                                         * VDEMIS_LT( C,R,L )
                  END DO
               END DO
            END DO

         END IF

C plume rise

         IF ( PT3DEMIS ) THEN
            CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

C merge: VDEMIS_PT in mol/sec. Convert to ppm/sec
C If aerosols, then the PM emissions obtained from GET_PT3D_EMIS need to be
C accessed by GET_AERO_EMIS to be "speciated" and merged with the AE emissions.

            S_STRT = 1; S_END = N_GC_EMIS
            IF ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 ) THEN
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DO L = 1, EMLAYS
                        CNVTC = CNVTP * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                        DO S = S_STRT, S_END
                           V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                           N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                           IF ( V .LT. 1 ) CYCLE
                           VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                       + CNVTC * SPC_PTEM_FAC( S )
     &                                       * VDEMIS_PT( C,R,L,N )
                           IF ( GC_SPC( GC_EMIS_MAP( S ) ) .EQ. 'PCVOC' ) THEN
                              VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R ) 
     &                                          - CNVTC * SPC_PTEM_FAC( S )
     &                                          * VDEMIS_PT_FIRE( C,R,L,N )
                           END IF
                        END DO
                     END DO
                  END DO
               END DO

            ELSE ! Remove pcSOA from fire point sources. Biomass burning sources
                 ! contributing to pcSOA cause enormous OA plumes.

               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DO L = 1, EMLAYS
                        CNVTC = CNVTP * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                        DO S = S_STRT, S_END
                           V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                           N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                           IF ( V .LT. 1 ) CYCLE
                           VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                       + CNVTC * SPC_PTEM_FAC( S )
     &                                       * VDEMIS_PT( C,R,L,N )
                        END DO
                     END DO
                  END DO
               END DO

            END IF

            S_STRT = S_END + 1; S_END = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     CNVTC = CNVTP * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                     DO S = S_STRT, S_END
                        V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                        N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                        IF ( V .LT. 1 ) CYCLE
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + CNVTC * SPC_PTEM_FAC( S )
     &                                    * VDEMIS_PT( C,R,L,N )
                     END DO
                  END DO
               END DO
            END DO

         END IF

C aerosol emissions - all units conversions done in GET_AERO_EMIS for aerosols
C GET_AERO_EMIS in f90 module AERO_EMIS

         IF ( N_AE_SPC .GT. 0 ) THEN
            CALL GET_AERO_EMIS ( JDATE, JTIME, TSTEP, VDEMIS )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     DO V = STRT_AE, FINI_AE
                        S = V + 1 - STRT_AE
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + VDEMIS_AE( S,L,C,R )
                     END DO
                  END DO
               END DO
            END DO

         END IF

C non-reactive gases (conversion to ppmv/s) VDEMIS in this order from RDEMIS

         DO V = STRT_NR, FINI_NR
            S = V + 1 - STRT_NR
            BUFF = 0.0   ! array assignment
C                                       vvvvvvv - emis surrogate names
            IF ( .NOT. INTERPX( EMIS_1, NR_EMIS( S ), PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read '
     &              // TRIM( NR_EMIS( S ) ) // ' from ' // EMIS_1
               IF ( .NOT. EMISCHK ) 
     &            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
! subtract the fertilizer emissions from the NEI for use in the NH3 bidirectional
! exchange model JOB
            IF ( ABFLUX .AND. NR_EMIS( S ) .EQ. 'NH3' ) THEN
               FERT = 0.0   ! array assignment
               IF ( .NOT. INTERPX( EMIS_1, 'NH3_FERT', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                          JDATE, JTIME, FERT ) ) THEN
                  XMSG = 'Could not read NH3_FERT from' // EMIS_1
                  IF ( .NOT. EMISCHK ) 
     &                 CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               END IF
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     BUFF( C,R,1 ) = BUFF( C,R,1 ) - FERT( C,R,1 )
                     IF ( BUFF( C,R,1 ) .LT. 0.0 ) THEN
                        XMSG = 'Negative values in NH3 emissions' 
     &                       //'Error in NH3 Bidi emissions'                          
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                     END IF
                  END DO
               END DO    
            END IF ! ABFLUX

            CNVTI = CNVTP * NR_EMIS_FAC( S )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTI * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * BUFF( C,R,L )
                  END DO
               END DO
            END DO

#ifdef verbose_emis_defn
            write( logdev,* ) '  NR EMIS: ', s, '  ', nr_emis( s )
#endif

         END DO

C tracer gases (conversion to ppmv/s)

         IF ( EM_TRAC ) THEN

            DO V = STRT_TR, FINI_TR
               S = V + 1 - STRT_TR
               BUFF = 0.0   ! array assignment
C                                               vvvvvvv - emis surrogate names
               IF ( .NOT. INTERPX( EMIS_TRAC_1, TR_EMIS( S ), PNAME,
     &                             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                             JDATE, JTIME, BUFF ) ) THEN
                  XMSG = 'Could not read  ' // TRIM( TR_EMIS( S ) )
     &                 //' from ' // EMIS_TRAC_1
                  IF ( .NOT. EMISCHK )
     &                 CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               END IF
               CNVTI = CNVTP * TR_EMIS_FAC( S )

               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DO L = 1, EMLYRS
                        CNVTC = CNVTI * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                        VDEMIS( V,L,C,R ) = CNVTC * BUFF( C,R,L )
                     END DO
                  END DO
               END DO

#ifdef verbose_emis_defn
               write( logdev,* ) '  TR EMIS: ', s, '  ', tr_emis( s )
#endif

            END DO

         END IF

C zero out emissions values for species not included in diffused list ...
C accounts for emissions species names as a subset of the vert. diffused species list

         VDEMIS( N_SPC_EMIS+1,:,:,: ) = 0.0

#ifdef verbose_emis_defn
!        v = index1 ( 'NO', n_gc_emis, gc_emis )
!        semis = vdemis( v,:,:,: )   ! all layers
!        write( logdev,* ) '  sumVDEMIS( NO ): ', sum( semis )
         write( logdev,'(/"@@@0 ", i8, i7.6)') jdate, jtime
         l = 1
         do v = strt_gc, fini_gc
            s = v + 1 - strt_gc
            sumem = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumem = sumem + vdemis( v,l,c,r )
               end do
            end do
            write( logdev,'("@@@2 ", a, g16.6)') gc_emis( s ), sumem
         end do
         do v = strt_nr, fini_nr
            s = v + 1 - strt_nr
            sumem = 0.0
            do r = 1, my_nrows
               do c = 1, my_ncols
                  sumem = sumem + vdemis( v,l,c,r )
               end do
            end do
            write( logdev,'("@@@3 ", a, g16.6)') nr_emis( s ), sumem
         end do
#endif

         RETURN

         END SUBROUTINE GET_EMIS


C-----------------------------------------------------------------------
         SUBROUTINE EMIS_SPC_CHECK( JDATE, JTIME )

C Check the chemical species from the namelists and AERO_DATA against
C the species that are available on the actual emissions input files. If
C they do not agree, print warngings or crash the program depending on
C how severe the error is.
C
C    16 Mar 2017  B.Murphy     Created Subroutine
C-----------------------------------------------------------------------

         USE BEIS_DEFN, only : BIOGEMIS  ! Online Biogenic Emissions Env Variable
         USE BIOG_EMIS, only : EMSPC,    ! Emitted CMAQ-Species from BEIS
     &                         MSPCS     ! Number of Emitted Species
         USE MGEMIS,    only : MGEM,     ! Flag for Marine Gas Emissions
     &                         MG_SPC    ! Emitted Marine Gas CMAQ-Species
         USE LTNG_DEFN, only : LTNG_NO,  ! Flag for Lightning Emissions
     &                         LTSPC     ! Lightning Emitted Species (i.e. 'NO')
         USE PT3D_DEFN, only : PT3DEMIS, ! Flag for Point Source Emissions
     &                         NPTGRPS
         USE STK_EMIS,  only : STKSPC    ! Species from Point Source Emissions
         USE UTILIO_DEFN
         USE CGRID_SPCS, only: N_GC_EMIS, N_AE_EMIS, N_NR_EMIS, N_TR_EMIS,
     &                         GC_EMIS,   AE_EMIS,   NR_EMIS,   TR_EMIS
         USE AERO_DATA, only : N_MODE, AEROSPC,  ! Aerosol Properties Table
     &                         N_AEROSPC

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME

         INTEGER    :: NTRSPC, NARSPC, NBIOSPC, 
     &                 NPTSPC, NMGSPC, NLTSPC, NDSTSPC
         INTEGER    :: NEMIS_SPC, NEMIS_SPC_UNIQUE, N_UNUSED

         CHARACTER( 16 ), ALLOCATABLE  :: EMIS_SPCS( : ), EMIS_SPC_UNIQUE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: GC_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: AE_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: NR_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: TR_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: AERO_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: EMSPC_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: EMIS_SPC_UNUSED( : )

         INTEGER    :: STRT, PTSTRT, ISPC, IPT, JPT, IDX, LOGDEV, IX

         LOGICAL    :: LERROR
         LOGICAL, ALLOCATABLE :: L_EMIS_SPC( : ), 
     &                           GC_EMIS_MISSING( : ), AE_EMIS_MISSING( : ),
     &                           NR_EMIS_MISSING( : ), TR_EMIS_MISSING( : ),
     &                           AERO_EMIS_MISSING( : ), TMP( : ),
     &                           EMIS_MASK( : ), BIO_IND( : )

         INTEGER, ALLOCATABLE  :: EMIS_IND( : )

         CHARACTER( 16 )  :: CTM_EMISCHK = 'CTM_EMISCHK'    ! Let model proceed even if
                                                                 ! Emissions surrogates are
                                                                 ! not properly defined
         CHARACTER( 16 )  :: PNAME = 'EMIS_SPC_CHECK'
         CHARACTER( 80 )  :: VARDESC
         INTEGER          :: STATUS
         CHARACTER( 200 ) :: XMSG

C Retrieve Environment Variable Letting User Ignore this Check
C and allowing the model to proceed.
         LOGDEV = INIT3()
         VARDESC    = "Let model proceed even if there are emission"
     &              //" surrogates that do not appear on any input file."
         EMISCHK = ENVYN( CTM_EMISCHK, VARDESC, EMISCHK, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty...Using default:'
            WRITE( LOGDEV, '(5X,A,I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set...Using default:'
            WRITE( LOGDEV, '(5X,A,I9)' ) XMSG, JTIME
         END IF


C Find Upper Bound on Number of Possible Species
         NBIOSPC = 0
         NMGSPC  = 0
         NLTSPC  = 0
         NDSTSPC = 0
         NTRSPC  = 0
         NARSPC  = 0
         NPTSPC  = 0

         ! Gridded Area and Tracer Sources 
         IF ( N_TR_EMIS .GT. 0 ) THEN
           NTRSPC  = SIZE( TRACSPC, DIM=1 )
         ELSE
           NTRSPC  = 0
         END IF
         NARSPC  = SIZE( AREASPC, DIM=1 )

         ! Point Sources
         IF ( PT3DEMIS ) THEN
             NPTSPC  = SUM( STKSPC( : )%LEN )
         END IF

         ! Biogenic Sources
         IF ( BIOGEMIS ) NBIOSPC = MSPCS

         ! Marine Gas Sources
         IF ( MGEM ) NMGSPC = SIZE( MG_SPC, DIM=1 )

         ! Lightning Sources
         IF ( LTNG_NO ) NLTSPC = 1

         ! Sum Species from Each Source Category
         NEMIS_SPC = NTRSPC  + NARSPC + NPTSPC 
     &             + NBIOSPC + NMGSPC + NLTSPC 
         ALLOCATE( EMIS_SPCS( NEMIS_SPC ) )
         EMIS_SPCS( 1:NEMIS_SPC ) = "                "

         ! Build the master list of all area, point, bio, sea spray
         ! Print a Table of Species from Every Emissions Source
         WRITE( LOGDEV, '(/,A31,A42)' ),'CHECKING COMPATABILITY BETWEEN ',
     &                  'INTERNAL SPECIES LIST AND EMISSIONS INPUTS' 
         WRITE( LOGDEV, '(A55,/,A58)' ),
     &                  'The following tables list the chemical species present ',
     &                  'on each emission file or from each online emission source.' 

         ! Tracer Species
         STRT = 0
         IF ( NTRSPC .GT. 0 ) THEN 
           EMIS_SPCS( STRT+1:STRT+NTRSPC ) = TRACSPC( 1:NTRSPC )
           WRITE( LOGDEV, '(/,A18,I3,A8,/,4(2x,A16))' ),
     &           'Tracer Emissions: ',NTRSPC,' Species',
     &           TRACSPC( 1:NTRSPC )
           STRT = STRT + NTRSPC
         END IF

         ! Gas and Aerosol Gridded Area Emissions
         IF ( NARSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1:STRT+NARSPC ) = AREASPC( 1:NARSPC )
           WRITE( LOGDEV, '(/,A40,I3,A8,/,4(2x,A16))' ),
     &           'Gas and Aerosol Gridded Area Emissions: ',
     &           NARSPC,' Species',AREASPC( 1:NARSPC )
           STRT = STRT + NARSPC
         END IF

         ! Point Species
         IF ( NPTSPC .GT. 0 ) THEN 
           PTSTRT = STRT
           DO IPT = 1,NPTGRPS
             JPT = STKSPC( IPT )%LEN
             WRITE( LOGDEV, '(/,A11,I2,A12,I3,A8,/,4(2x,A16))' ),
     &             'Point File ',IPT,' Emissions: ',JPT, 'Species',
     &             STKSPC( IPT )%ARRY
             EMIS_SPCS( PTSTRT+1:PTSTRT+JPT ) = STKSPC( IPT )%ARRY( 1:JPT )
             PTSTRT = PTSTRT + JPT
           END DO
           STRT = STRT + NPTSPC
         END IF

         ! Biogenic Species
         IF ( NBIOSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1:STRT+NBIOSPC ) = EMSPC( 1:NBIOSPC )
           ALLOCATE( BIO_IND( NBIOSPC ) )
           BIO_IND = EMSPC .NE. ''
           ALLOCATE( EMSPC_WRITE( COUNT( BIO_IND ) ) )
           EMSPC_WRITE = PACK( EMSPC, BIO_IND )
           
           WRITE( LOGDEV, '(/,A20,I3,A8,/,4(2x,A16))' ),
     &           'Biogenic Emissions: ',COUNT( BIO_IND ),' Species',
     &           EMSPC_WRITE
           STRT = STRT + NBIOSPC
         END IF
         
         ! Marine Gas Species
         IF ( NMGSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1:STRT+NMGSPC ) = MG_SPC( 1:NMGSPC )
           WRITE( LOGDEV, '(/,A26,I3,A8,/,4(2x,A16))' ),
     &           'Marine Gas Emissions: ',NMGSPC,' Species',
     &           MG_SPC( 1:NMGSPC )
           STRT = STRT + NMGSPC
         END IF
         
         ! Lightning Species (NO)
         IF ( NLTSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1 ) = LTSPC
           WRITE( LOGDEV, '(/,A21,I3,A8,/,4(2x,A16))' ),
     &           'Lightning Emissions: ',NLTSPC,' Species',
     &           LTSPC
           STRT = STRT + NLTSPC
         END IF

C Reduce EMIS_SPCS to just unique values
         ALLOCATE( EMIS_MASK( NEMIS_SPC ) )
         EMIS_MASK = .TRUE.
         ! Determine which surrogates to keep
         DO ISPC = NEMIS_SPC,2,-1
             EMIS_MASK( ISPC ) = .NOT.( ANY( EMIS_SPCS(:ISPC-1) == EMIS_SPCS( ISPC ) ) )
         END DO

         ! Define indices of Surrogates to Keep
         ALLOCATE( EMIS_IND( COUNT( EMIS_MASK ) ) )   
         EMIS_IND = PACK([(ix, ix=1,NEMIS_SPC ) ],EMIS_MASK )
         
         ! Reduce the EMIS_SPCS vector down to just the unique values
         NEMIS_SPC_UNIQUE = SIZE( EMIS_IND,DIM=1 )
         ALLOCATE( EMIS_SPC_UNIQUE( NEMIS_SPC_UNIQUE ) )
         EMIS_SPC_UNIQUE = EMIS_SPCS( EMIS_IND )
         
C Print out the Tables of Gas, Aerosol, Nonreactive and Tracer Species
C for reference
         WRITE( LOGDEV, '(/A57,/,A54)' ),
     &                  'The following tables list the emission surrogate species ',
     &                  'present in each namelist or in the aerosol data table.' 
         
         IF ( N_GC_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'GC Namelist: ',N_GC_EMIS,' Species',GC_EMIS(1:N_GC_EMIS)
         
         IF ( N_AEROSPC .GT. 0 ) THEN
              ALLOCATE( AERO_EMIS_WRITE( COUNT( AEROSPC%EMIS .NE. "" ) ) )
              AERO_EMIS_WRITE = PACK( AEROSPC%EMIS, AEROSPC%EMIS .NE. "" )
              WRITE( LOGDEV, '(/,A16,I3,A8,/,4(2x,A16))' ),
     &               'AeroData Table: ',COUNT( AEROSPC%EMIS .NE. "" ),
     &               ' Species',AERO_EMIS_WRITE
              DEALLOCATE( AERO_EMIS_WRITE )
         END IF
         
         IF ( N_NR_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'NR Namelist: ',N_NR_EMIS,' Species',NR_EMIS(1:N_NR_EMIS)
         
         IF ( N_TR_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'TR Namelist: ',N_TR_EMIS,' Species',TR_EMIS(1:N_TR_EMIS)



C Compare with Gas, Aerosol, Nonreactive, and Tracer Species
         ! Initialize variables for tracking surrogate matches
         ALLOCATE( L_EMIS_SPC( NEMIS_SPC_UNIQUE ) )
         L_EMIS_SPC = .FALSE.
         LERROR = .FALSE.

         ! Check Compatability with Gas Emissions Surrogates
         IF ( N_GC_EMIS .GT. 0 ) THEN
             ALLOCATE( GC_EMIS_MISSING( N_GC_EMIS ) )
             GC_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_GC_EMIS
                IDX = INDEX1( GC_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   L_EMIS_SPC( IDX ) = .TRUE.
                ELSE
                   ! This Gas Surrogate is Missing
                   GC_EMIS_MISSING( ISPC ) = .TRUE.
                END IF
             END DO
             IF ( ANY( GC_EMIS_MISSING ) ) THEN
                ALLOCATE( GC_EMIS_WRITE( COUNT( GC_EMIS_MISSING ) ))
                GC_EMIS_WRITE = PACK( GC_EMIS(1:N_GC_EMIS), GC_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A48,/,2x,A51,/,2x,A42,/,5(5x,A16))'),
     &             'The following GAS emission surrogate species are',
     &             'not present from any of the Area, Point, Biogenic,',
     &             'Marine Gas, or Lightning emissions sources:',
     &             GC_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
          
         ! Check Compatability with Aerosol Emissions Surrogates
         IF ( N_AEROSPC .GT. 0 ) THEN 
             ALLOCATE( AERO_EMIS_MISSING( N_AEROSPC ) )
             AERO_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_AEROSPC
                IDX = INDEX1( AEROSPC( ISPC )%EMIS, NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   L_EMIS_SPC( IDX ) = .TRUE.
               ELSE IF ( AEROSPC( ISPC )%EMIS .NE. "" ) THEN
                   ! This Aerosol Surrogate is Missing
                   AERO_EMIS_MISSING( ISPC ) = .TRUE.
                END IF
             END DO 
             IF ( ANY( AERO_EMIS_MISSING ) ) THEN
                ALLOCATE( AERO_EMIS_WRITE( COUNT( AERO_EMIS_MISSING ) ) ) 
                AERO_EMIS_WRITE = PACK( AEROSPC%EMIS, AERO_EMIS_MISSING )

                WRITE( LOGDEV, '(/A40,/,2x,A52,/,2x,A53,/,5(5x,A16))'),
     &             'The following AEROSOL emission surrogate',
     &             'species are not present from any of the Area, Point,',
     &             'Biogenic, Marine Gas, or Lightning emissions sources:',
     &             AERO_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
          
         ! Check Compatability with Nonreactive Emissions Surrogates
         IF ( N_NR_EMIS .GT. 0 ) THEN
             ALLOCATE( NR_EMIS_MISSING( N_NR_EMIS ) )
             NR_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_NR_EMIS
                IDX = INDEX1( NR_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   L_EMIS_SPC( IDX ) = .TRUE.
                ELSE
                   ! This Nonreactive Surrogate is Missing
                   NR_EMIS_MISSING( ISPC ) = .TRUE.
                END IF
             END DO 
             IF ( ANY( NR_EMIS_MISSING ) ) THEN
                ALLOCATE( NR_EMIS_WRITE( COUNT( NR_EMIS_MISSING ) ) ) 
                NR_EMIS_WRITE = PACK( NR_EMIS(1:N_NR_EMIS), NR_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A45,/,A53,/,A54,/,5(5x,A16))'),
     &             'The following NONREACTIVE emission surrogate ',
     &             'species are not present from any of the Area, Point, ',
     &             'Biogenic, Marine Gas, or Lightning emissions sources: ',
     &             NR_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
         
         ! Check Compatability with Tracer Emissions Surrogates
         IF ( N_TR_EMIS .GT. 0 ) THEN
             ALLOCATE( TR_EMIS_MISSING( N_TR_EMIS ) )
             TR_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_TR_EMIS
                IDX = INDEX1( TR_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   L_EMIS_SPC( IDX ) = .TRUE.
                ELSE
                   ! This Tracer Surrogate is Missing
                   TR_EMIS_MISSING( ISPC ) = .TRUE.
                END IF
             END DO 
             IF ( ANY( TR_EMIS_MISSING ) ) THEN
                ALLOCATE( TR_EMIS_WRITE( COUNT( TR_EMIS_MISSING ) ) ) 
                TR_EMIS_WRITE = PACK( TR_EMIS(1:N_TR_EMIS), TR_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A40,/,A53,/,A54,/,5(5x,A16))'),
     &             'The following TRACER emission surrogate ',
     &             'species are not present from any of the Area, Point, ',
     &             'Biogenic, Marine Gas, or Lightning emissions sources: ',
     &             TR_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF

C Print a warning for each emissions file species that is not used.
         N_UNUSED = COUNT( .NOT. L_EMIS_SPC )
         IF ( N_UNUSED .GT. 0 ) THEN 
           ALLOCATE( EMIS_SPC_UNUSED( N_UNUSED ) )
           EMIS_SPC_UNUSED = PACK( EMIS_SPC_UNIQUE,( .NOT. L_EMIS_SPC ) ) 
         END IF
        
C Stop the Model if Any Emissions Surrogate Species are
C Missing from All Emission Input Sources
         IF ( LERROR ) THEN
           IF ( .NOT. EMISCHK ) THEN
              WRITE( LOGDEV, '(5(/,A))') 
     &               '************************WARNING***********************:',
     &               'One or more emissions surrogates assigned to model species ',
     &               'are not found in emissions sources but the CTM_EMISCHK ',
     &               'environment variable set to False so simulation will proceed.',
     &               '*******************************************************'

              WRITE( LOGDEV, '(/,A55,/,A34,I3,A8,/,4(2x,A16))')
     &              'ATTENTION: The following emission species are available',
     &              'from the inputs but are not used: ',N_UNUSED,' Species',
     &              EMIS_SPC_UNUSED

              WRITE( LOGDEV, * )
              WRITE( LOGDEV, * )
              XMSG = 'For optimal predictions, species with the missing surrogates '
     &             //'should have a surrogate found in at least one source.'
              CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
           ELSE     
              WRITE( LOGDEV, '(3(/,A),/,5(/,A),/,A,4(/,A))') 
     &               '*************************ERROR**********************:',
     &               'One or more emission surrogates assigned to model species ',
     &               'are not found in emission sources.',
     &               'Use one of the below options to continue.', 
     &               '1) Change or remove the missing GC, NR and TR emissions surrogates from the',
     &               'namelists where they are defined OR if missing surrogates are for aerosol species,',
     &               'change or remove these surrogates from aerosol data table defined in AERO_DATA.F',
     &               'then recompile.',
     &               'or',
     &               '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &               'if model predictions are acceptable without using the above emissions.',
     &               '*****************************************************'

              WRITE( LOGDEV, '(/,A55,/,A34,I3,A8,/,4(2x,A16))')
     &             'ATTENTION: The following emission species are available',
     &             'from the inputs but are not used: ',N_UNUSED,' Species',
     &              EMIS_SPC_UNUSED
              WRITE( LOGDEV, * )
              WRITE( LOGDEV, * )
              XMSG = 'Species with the missing surrogates ' 
     &             //'must have a surrogate found in at least one source.'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF
         END IF



         END SUBROUTINE EMIS_SPC_CHECK

      END MODULE EMIS_DEFN
