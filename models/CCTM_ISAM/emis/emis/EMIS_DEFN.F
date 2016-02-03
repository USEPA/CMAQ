
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
C   28 Jul 2006 J.Young: initial implementation
C   18 Aug 2007 J.Young: move beis part to separate module; add plume rise
C   23 Sep 2009 B.Hutzell: modified algorithm that loads gas emissions from point
C                 sources into VDEMIS array to enable multi-use of an emission species
C   26 Jan 2010 J.Young: fix bug overwriting point source layer 1 NH3; inline rdemis
C   07 Jan 2011 B.Hutzell: updated for namelist definition of model species
C   16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                            removed deprecated TRIMLEN
C    6 Apr 2011 J.Young, R.Pinder: add lightning NO emissions capability
C   11 May 2011 D.Wong: incorporated twoway model implementation
C    5 Jun 2012 J.Bash: Added support for NH3 bidirectional exchange. Fertilizer
C                       sector emissions are subtracted from the total NH3
C                       emissions if the CTM_ABFLUX flag is set
C   19 Jul 13 R.Kwok: isam
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications      

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array
#ifdef Verbose2
      real,    allocatable, save :: semis( :,:,: )
#endif

      INTEGER,              SAVE :: N_SPC_EMIS ! global emissions species
      INTEGER,              SAVE :: EMLAYS     ! no. of emis integration layers
                                               ! EMLAYS must be .LT. NLAYS
      INTEGER,              SAVE :: EMLYRS     ! no. of layers on base emis input file -

      PUBLIC VDEMIS, N_SPC_EMIS, EMLAYS, EMLYRS, EMIS_INIT, GET_EMIS
#ifdef Verbose2
      public semis
#endif
#ifdef isam
      INTEGER, SAVE :: FRST_GC, LAST_GC, FRST_AE, LAST_AE,
     &                 FRST_NR, LAST_NR, FRST_TR, LAST_TR
      PUBLIC           FRST_GC, LAST_GC, FRST_AE, LAST_AE,
     &                 FRST_NR, LAST_NR, FRST_TR, LAST_TR
#endif
      PRIVATE
                                           ! a 2d file if inline, otherwise a 3d file
      INTEGER, SAVE :: NEMIS_AE   ! no. of aero emis species

      LOGICAL, SAVE :: EM_TRAC = .FALSE. ! do tracer emissions?
      REAL,    SAVE :: CNVTE      ! combined conversion factor
      REAL,    SAVE :: DT         ! TSTEP (output) in sec

      REAL,    ALLOCATABLE, SAVE :: BUFF( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: FERT( :,:,: )        ! read buffer

      INTEGER, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                 STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, SAVE :: NDATE, NTIME  ! step next date/time (beis)

#ifdef Verbose2
      character( 16 ), allocatable, save :: emis_spc( : ) ! global emis surrogates
      integer,         allocatable, save :: emis_map( : )  ! name map to CGRID
#endif

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE BEIS_DEFN           ! biogenic emissions
         USE LTNG_DEFN           ! NO emissions from lightning strikes
         USE PT3D_DEFN           ! plume rise emissions
         USE UTILIO_DEFN         ! I/O API
         USE AERO_EMIS           ! inherits GRID_CONF
         USE AERO_DATA, ONLY: MAP_PMEMIS  ! access subroutine map_pmemis
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
#ifdef isam
         USE SA_DEFN
#endif

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
#ifdef Verbose2
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

C In-line biogenic emissions?

         IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Open Emissions files

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

C lightning NO emissions?

         IF ( .NOT. LTNG_INIT( JDATE, JTIME, TSTEP, EMLAYS ) ) THEN
            XMSG = 'Failure initializing lightning emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

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

         N_SPC_EMIS = N_GC_EMIS
     &              + NEMIS_AE
     &              + N_NR_EMIS
     &              + N_TR_EMIS

#ifdef Verbose2
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

C 3D point source emissions (N_SPC_EMIS+1 serves as the null map for PTEM_MAP)
         IF ( .NOT. PT3D_INIT( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#ifdef isam
         IF ( YES_PRINT ) THEN
           IF ( MYPE .EQ. 0 ) 
     &       WRITE( LOGDEV,'(/5X, A )' ) 'PT3D_INIT done'
         END IF ! yes_print
#endif
         ALLOCATE ( VDEMIS( N_SPC_EMIS+1,EMLAYS,MY_NCOLS,MY_NROWS ),
#ifdef Verbose2
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

         IF ( N_SPC_EMIS .LE. 0 ) THEN
            XMSG = 'There are no emissions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         ALLOCATE ( BUFF( NCOLS,NROWS,EMLYRS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'BUFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         BUFF = 0.0   ! array assignment

         IF ( ABFLUX ) THEN
            ALLOCATE ( FERT( NCOLS,NROWS,EMLYRS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'FERT memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            FERT = 0.0   ! array assignment
         END IF
C combined gas emssions conversion factor

         CNVTE = CMLMR * CONVEM * MWAIR / ( DX1 * DX2 )

         STRT_GC = 1
         FINI_GC = N_GC_EMIS
         STRT_AE = N_GC_EMIS + 1
         FINI_AE = N_GC_EMIS + NEMIS_AE
         STRT_NR = N_GC_EMIS + NEMIS_AE + 1
         FINI_NR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS
         STRT_TR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS + 1
         FINI_TR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS + N_TR_EMIS
#ifdef isam
         FRST_GC = STRT_GC
         LAST_GC = FINI_GC
         FRST_AE = STRT_AE
         LAST_AE = FINI_AE
         FRST_NR = STRT_NR
         LAST_NR = FINI_NR
         FRST_TR = STRT_TR
         LAST_TR = FINI_TR
#endif
         SUCCESS = .TRUE.; RETURN

         END FUNCTION EMIS_INIT

C-----------------------------------------------------------------------
#ifdef isam
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA,
     &                         RDX3F, RRHOJ, RJACM, ISAMEM, MFRC_P )
#else
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA,
     &                         RDX3F, RRHOJ, RJACM )
#endif

C NEW APPROACH:
C Apply NML factors to all *input* emissions and inline plume rise, aero emis
C biog, SeaSalt, Dust, and Lightning NO
C GET_AERO_EMIS has to apply factors - don`t do it at this level.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE PT3D_DEFN           ! plume rise emissions
         USE PTMAP               ! defines emissions file(s) PM mapping
         USE LTNG_DEFN           ! lightning NO emissions
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
#ifdef isam
         USE SA_GRD_DEFN         ! 20120712 for gridded SA emis, implicitly assume sa_defn
         USE OZVOC_DEFN          ! 20120918 for VOC
         USE AERO_DATA, ONLY: N_EMIS_PM ! 20130319 diagnostic
#endif
         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL,    INTENT( IN ) :: CONVPA        ! conversion factor to pressure in Pascals
         REAL,    INTENT( IN ) :: RDX3F( NLAYS )             ! reciprocal layer thickness
         REAL,    INTENT( IN ) :: RRHOJ( NCOLS,NROWS,NLAYS ) ! reciprocal density X Jacobian
         REAL,    INTENT( IN ) :: RJACM( NCOLS,NROWS,NLAYS ) ! reciprocal mid-layer Jacobian
#ifdef isam
         REAL, POINTER :: ISAMEM( :,:,:,:,: ) ! array of tagged emis
         REAL, POINTER :: MFRC_P( :,:,: ) ! fraction of tagging regions
#endif

C Local Variables:
         REAL             CNVTC         ! combined conversion factor
         REAL             CNVTI         ! intermediate combined conv. factor
         REAL,    SAVE :: CNVTP         ! intermediate combined conv. factor
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V ! loop induction variables
#ifdef isam
         INTEGER          V2, V3 ! 20120725 index for non-nox gas emis factor
         REAL             GASFAC ! 20120725 gas emis factor either from gas_emis or nr_emis lists
#endif

         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '
#ifdef Verbose2
         integer, save :: logdev
         real sumem
#endif
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
#ifdef isam
         LOGICAL LAERO, LSULF, LTEST
         INTEGER JSPCTAG, IBGN, ITAG, JSPC, JV
C NOX related....
         CHARACTER( 16 )  NAM_NOX
         INTEGER JNOX, INOX
C SA PM emis in IJK....
         REAL, ALLOCATABLE :: ISAMEM_AE( :,:,:,:,: )
Ckrt......IO-status just for ENVYN
         INTEGER IOS
Ckrt...0922...Diagnostic
         REAL    SAEM_ICON, TTL_VDE_GRD, TTL_PME_GRD
         REAL    TTL_VDE_PT, TTL_PME_PT, TTL_EM_AE
CDc08...Diagnostic
         REAL    SAEM_AE_TTL( NSPC_SA )
         INTEGER N_SA2EMAE
CDc09...Diagnostic
         REAL    TTL_SULF_GRD
C....Diagnostic 20130319
         REAL    SA_CHK
C....Remedy 20130320
!        INTEGER LENSPCNAME
         INTEGER LS
C VOC related 20130311
         INTEGER JVE  ! matching emission factors of the 14 VOC's
#ifdef isam
!20140516
         INTEGER              LOGDEV1
#endif
         INTERFACE
            SUBROUTINE test_sulf( JDATE, JTIME, TSTEP, NSTEPS, FILE_NAME )
               IMPLICIT NONE
               INTEGER   :: JDATE, JTIME
               INTEGER   :: TSTEP( 3 )
               INTEGER   :: NSTEPS
               CHARACTER( 16 ) :: FILE_NAME
            END SUBROUTINE test_sulf
         END INTERFACE
#endif

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
#ifdef Verbose2
            logdev = init3()
#endif
#ifdef isam
! 20140516
            LOGDEV1 = INIT3()
#endif 

C Get domain decomp info from the emissions file
            CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
            CNVTP = CONVPA * CNVTE
         END IF   ! firstime

#ifdef isam
Ckrt allocate or reinitialize isamem_ae....
         IF ( .NOT. ALLOCATED( ISAMEM_AE ) )
     &   ALLOCATE( ISAMEM_AE( MY_NCOLS, MY_NROWS, NLAYS, NSPC_SA, NTAG_SA ) )
         ISAMEM_AE = 0.0
Ckrt...Keep in mind that ISAMEM should also be reinitialized here...
         ISAMEM = 0.0
#endif

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
!              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
            CNVTI = CNVTP * GC_EMIS_FAC( S )

C reactive gases (conversion to ppmv/s) VDEMIS in this order from RDEMIS
C "Stores read an entire cache line, modify the target, then write back the
C  entire line. Thus, non-consecutive stores are worse than non-consecutive
C  loads."
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTI * RDX3F( L ) * RRHOJ( C,R,L )
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * BUFF( C,R,L )
                  END DO
               END DO
            END DO

#ifdef Verbose2
            write( logdev,* ) '  GC EMIS: ', s, '  ', gc_emis( s )
#endif

         END DO   ! V

#ifdef isam
Ckrt....Read **Gridded** SA gas and non-reactants emission data
         CALL SA_GRD_EGAS ( JDATE, JTIME, TSTEP )


Ckrt...Convert gridded SA gas emission into ppmV/sec
         DO R = 1, MY_NROWS
           DO C = 1, MY_NCOLS
             DO L = 1, EMLYRS
               CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
               DO JSPC = 1, NSPC_SA
                 DO ITAG = 1, NTAG_SA-3
                   LS = LEN_TRIM( SPC_NAME( JSPC,ITAG ) )
                   IF ( LS .GT. 0 .AND.
     &                  SPC_NAME( JSPC,ITAG )( 1:LS-1 ) .NE. 'O3' .AND.
     &                  SPC_NAME( JSPC,ITAG )( 1:LS )   .NE. 'SULF' .AND.
     &                  SPC_NAME( JSPC,ITAG )( LS:LS )  .NE. 'I' .AND.
     &                  SPC_NAME( JSPC,ITAG )( LS:LS )  .NE. 'J' ) THEN
                     ISAMEM( C,R,L,JSPC,ITAG ) = ISAMEM( C,R,L,JSPC,ITAG )
     &      + SA_VDEMIS_GRD( C,R,L,JSPC,ITAG ) * CNVTC * MFRC_P( C,R,ITAG )
                   END IF ! excludes VOC, O3, SULF, I's and J's
                 END DO ! itag
               END DO ! jspc

Ckrt Unit conversion for gridded SULF for PM sulate later...
               V = INDEX1( 'SULF', N_GC_EMIS, GC_EMIS )
               IF ( ANY( L_SFATE ) ) THEN
                 DO ITAG = 1, NTAG_SA - 3
                   SA_SULF_GRD( C,R,L,ITAG ) = SA_SULF_GRD( C,R,L,ITAG )
     &               * MFRC_P( C,R,ITAG ) * CNVTC * GC_EMIS_FAC( V )
                 END DO ! loop of tags for sulf
               END IF ! v > 0 ?
             END DO ! L
           END DO ! C
         END DO ! R

Ckrt...Read Gridded SA PM emission data
         CALL SA_GRD_E_PM ( JDATE, JTIME, TSTEP )
#endif

C beis

         IF ( BIOGEMIS ) THEN

            CALL GET_BEIS ( JDATE, JTIME, TSTEP )

C tmpbeis312 is based on hourly input - need to interpolate return value
C merge: VDEMIS_BI in mol/hr. Convert to ppm/sec

#ifdef Verbose2
            do s = 1, mspcs
               v = beis_map( s )
               if ( v .gt. 0 ) then
                  write( logdev,* ) '  BEIS_MAP: ', s, v, '  ', gc_emis( v ), gc_emis_fac( v )
               end if
            end do
#endif
            L = 1
            CNVTI = CONVPA * CNVTE * RDX3F( L )  ! per sec
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  CNVTC = CNVTI * RRHOJ( C,R,L )
                  N = 0
                  DO S = 1, MSPCS
                     V = BEIS_MAP( S )
                     IF ( V .GT. 0 ) THEN
                        N = N + 1
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + CNVTC * GC_EMIS_FAC( V ) * VDEMIS_BI( N,C,R )
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
                     CNVTI = CONVPA * CNVTE * RDX3F( L )  ! per sec
                     CNVTC = CNVTI * RRHOJ( C,R,L )
                     V = LTNG_MAP
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * GC_EMIS_FAC( V ) * VDEMIS_LT( C,R,L )
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

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
                     DO S = 1, N_GC_EMIS + N_NR_EMIS + N_TR_EMIS
                        V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                        N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                        IF ( V .LT. 1 ) CYCLE
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + CNVTC * VDEMIS_PT( C,R,L,N )
                     END DO
                  END DO
               END DO
            END DO
#ifdef isam
Ckrt...update ISAMEM with inline point source gas emission, and convert to ppmV/s
            DO R = 1, MY_NROWS
              DO C = 1, MY_NCOLS
                DO L = 1, EMLAYS
                  CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
Ckrt Inline pt src gas....
                  DO JSPC = 1, NSPC_SA
                    DO ITAG = 1, NTAG_SA - 3
                      LS = LEN_TRIM( SPC_NAME( JSPC,ITAG ) )
                      IF ( LS .GT. 0 ) THEN
                        IF ( SPC_NAME( JSPC,ITAG )( 1:LS ) .NE. 'VOC' .AND.
     &                       SPC_NAME( JSPC,ITAG )( 1:LS-1 ) .NE. 'O3' .AND.
     &                       SPC_NAME( JSPC,ITAG )( 1:LS ) .NE. 'SULF' .AND.
     &                       SPC_NAME( JSPC,ITAG )( LS:LS ) .NE. 'I' .AND.
     &                       SPC_NAME( JSPC,ITAG )( LS:LS ) .NE. 'J' ) THEN
                          V = INDEX1( SPC_NAME( JSPC,ITAG )( 1:LS ), N_SPC_PTEM, STKEM_NAME )
                          V2 = INDEX1( SPC_NAME( JSPC,ITAG )( 1:LS ), N_GC_EMIS, GC_EMIS )
                          V3 = INDEX1( SPC_NAME( JSPC,ITAG )( 1:LS ), N_NR_EMIS, NR_EMIS )
                          IF ( V .GT. 0 ) THEN
                            IF ( V2 .GT. 0 ) GASFAC = GC_EMIS_FAC( V2 )
                            IF ( V3 .GT. 0 .AND. V2 .LE. 0 ) GASFAC = NR_EMIS_FAC( V3 )
                              ISAMEM( C,R,L,JSPC,ITAG ) =
     &                        ISAMEM( C,R,L,JSPC,ITAG )
     &                   + SA_VDEMIS_PT( C,R,L,V,ITAG )
     &                          * MFRC_P( C,R,ITAG ) * CNVTC * GASFAC
                          END IF !  v > 0, meaning that vnam exists
                        END IF ! spc_name not any of voc, o3, sulf, pm spc
                      END IF ! spc_name exists at jscp, itag
                    END DO  ! loop ITAG
                  END DO ! loop JSPC
Ckrt Unit conversion of inline pt src SULF for PM sulfate later......
                  V = INDEX1( 'SULF', N_GC_EMIS, GC_EMIS )
                  IF ( ANY( L_SFATE ) ) THEN
                    DO ITAG = 1, NTAG_SA - 3
                      SA_SULF_PT( C,R,L,ITAG ) = SA_SULF_PT( C,R,L,ITAG )
     &                  * MFRC_P( C,R,ITAG ) * CNVTC * GC_EMIS_FAC( V )
                    END DO ! loop of tags for sulf
                  END IF ! v > 0 ?
               END DO ! layers
             END DO ! columns
           END DO ! rows
#endif
         END IF

C aerosol emissions - all units conversions done in GET_AERO_EMIS for aerosols
C GET_AERO_EMIS in f90 module AERO_EMIS

         IF ( N_AE_SPC .GT. 0 ) THEN
            CALL GET_AERO_EMIS ( JDATE, JTIME, TSTEP, RJACM, VDEMIS )

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

#ifdef isam
Ckrt SA_AERO_EMIS - units conversions, allocation to IJK modes are
Ckrt   done here for tagged aerosols

         LAERO = .FALSE.
         JSPCTAG = 1
         DO
           IF ( JSPCTAG .GT. N_SPCTAG ) EXIT
           IBGN = INDEX( VNAM_SPCTAG( JSPCTAG ), '_', .FALSE. ) - 1
           IF ( VNAM_SPCTAG( JSPCTAG )( 1:1 ) .EQ. 'A' .AND.
     &        ( VNAM_SPCTAG( JSPCTAG )( IBGN:IBGN ) .EQ. 'I' .OR.
     &          VNAM_SPCTAG( JSPCTAG )( IBGN:IBGN ) .EQ. 'J') ) THEN
             LAERO = .TRUE.
             EXIT
           END IF
           JSPCTAG = JSPCTAG + 1
         END DO ! any pm species will do

         IF ( LAERO ) THEN
Ckrt       print*,'Could CALL SA_AERO_EMIS here....'
Ckrt...As long as sulfur is among list of tagging species,
Ckrt.....turn on the SULF extraction.................
           LSULF = .FALSE.
           JSPCTAG = 1
           XTR_SULF: DO
             IF ( JSPCTAG .GT. N_SPCTAG ) EXIT
             IBGN = INDEX( VNAM_SPCTAG( JSPCTAG ), '_', .FALSE. ) - 2
             IF ( VNAM_SPCTAG( JSPCTAG )( 1:IBGN ) .EQ. 'ASO4' ) THEN
               LSULF = .TRUE.
               EXIT
             END IF ! begin with aso4 ?
             JSPCTAG = JSPCTAG + 1
           END DO XTR_SULF

Ckrt...sa_pmemis_pt absorbed into sa_pmemis_grd....
           IF ( PT3DEMIS ) THEN
             DO V = 1, N_SPC_PTPM ! 20120719
               S = PTPM_MAP(V)    ! 20120719
               SA_PMEMIS_GRD( :,:,:,S,: ) = SA_PMEMIS_GRD( :,:,:,S,: )
     &                                    + SA_PMEMIS_PT( :,:,:,V,: )
               IF ( YES_PRINT ) THEN
                 IF ( TRIM(STKPM_NAME(V)) .EQ. 'PEC' ) THEN
                   WRITE( LOGDEV1,*) 
     & 'At C=60, R=23, L=1, tag=1, sa_pmemis_pt(pec)=',
     &  SA_PMEMIS_PT(60,23,1,V,1),' sa_pmemis_grd(pec)=',
     &  SA_PMEMIS_GRD(60,23,1,S,1)
                 END IF  
               END IF
             END DO
             IF ( ALLOCATED( SA_PMEMIS_PT ) ) DEALLOCATE( SA_PMEMIS_PT )
             IF ( LSULF ) THEN
               SA_SULF_GRD = SA_SULF_GRD + SA_SULF_PT
               IF ( ALLOCATED( SA_SULF_PT ) ) DEALLOCATE( SA_SULF_PT )
             END IF
           END IF ! pt3demis

           CALL SA_AERO_EMIS ( JDATE, JTIME, TSTEP, RJACM, ISAMEM_AE )

Ckrt...update isamem with the converted PM isamem_ae
           DO JSPCTAG = 1, N_SPCTAG
             DO L = 1, EMLAYS
               DO R = 1, MY_NROWS
                 DO C = 1, MY_NCOLS
                   IF ( T_SPCTAG( JSPCTAG ) .LE. NTAG_SA - 3 ) THEN
                     ISAMEM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) =
     &               ISAMEM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
     &          + ISAMEM_AE( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
     &             * MFRC_P( C,R,T_SPCTAG( JSPCTAG ) )
                   END IF ! cannot have any of bcontag, othrtag and icontag
                 END DO ! C
               END DO ! R
             END DO ! L
           END DO ! jpsctag

         END IF  ! laero ?
#endif

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
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
! subtract the fertilizer emissions from the NEI for use in the NH3 bidirectional
! exchange model JOB
            IF ( ABFLUX .AND. TRIM( NR_EMIS( S ) ) .EQ. 'NH3' ) THEN
               FERT = 0.0   ! array assignment
               IF ( .NOT. INTERPX( EMIS_1, 'NH3_FERT', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                          JDATE, JTIME, FERT ) ) THEN
                  XMSG = 'Could not read NH3_FERT from'
     &                   // EMIS_1
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     BUFF( C,R,1 ) = BUFF( C,R,1 ) - FERT( C,R,1 )
                     IF(BUFF( C,R,1 ) .LT. 0.0 ) THEN
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
                     CNVTC = CNVTI * RDX3F( L ) * RRHOJ( C,R,L )
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * BUFF( C,R,L )
                  END DO
               END DO
            END DO

#ifdef Verbose2
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
                  XMSG = 'Could not read  '
     &                 // TRIM( TR_EMIS( S ) )
     &                 // ' from ' // EMIS_TRAC_1
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
               CNVTI = CNVTP * TR_EMIS_FAC( S )

               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DO L = 1, EMLYRS
                        CNVTC = CNVTI * RDX3F( L ) * RRHOJ( C,R,L )
                        VDEMIS( V,L,C,R ) = CNVTC * BUFF( C,R,L )
                     END DO
                  END DO
               END DO

#ifdef Verbose2
               write( logdev,* ) '  TR EMIS: ', s, '  ', tr_emis( s )
#endif

            END DO

         END IF

C zero out emissions values for species not included in diffused list ...
C accounts for emissions species names as a subset of the vert. diffused species list

         VDEMIS( N_SPC_EMIS+1,:,:,: ) = 0.0

#ifdef Verbose2
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

      END MODULE EMIS_DEFN
