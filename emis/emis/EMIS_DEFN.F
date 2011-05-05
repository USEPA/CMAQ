
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/EMIS_DEFN.F,v 1.7 2011/05/05 15:14:39 sjr Exp $

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
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array

      INTEGER, SAVE :: N_SPC_EMIS ! global emissions species
      INTEGER, SAVE :: EMLAYS     ! no. of emis integration layers
                                  ! EMLAYS must be .LT. NLAYS

      INTEGER, PRIVATE, SAVE :: EMLYRS     ! no. of layers on base emis input file -
                                           ! a 2d file if inline, otherwise a 3d file
      INTEGER, PRIVATE, SAVE :: NEMIS_AE   ! no. of aero emis species

      LOGICAL, PRIVATE, SAVE :: EM_TRAC = .FALSE. ! do tracer emissions?
      REAL,    PRIVATE, SAVE :: CNVTE      ! combined conversion factor
      REAL,    PRIVATE, SAVE :: DT         ! TSTEP (output) in sec

      REAL,    PRIVATE, ALLOCATABLE, SAVE :: BUFF( :,:,: ) ! read buffer

      INTEGER, PRIVATE, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                          STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, PRIVATE, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER, PRIVATE       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, PRIVATE, SAVE :: NDATE, NTIME  ! step next date/time (beis)

      INTEGER, PRIVATE, SAVE :: LOGDEV
      INTEGER, PRIVATE, EXTERNAL :: SETUP_LOGDEV

#ifdef Verbose
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
         USE AERO_DATA, ONLY: MAP_PMEMIS  ! access subroutine map_pmemis
         USE UTILIO_DEFN
         USE AERO_EMIS           ! inherits GRID_CONF

         IMPLICIT NONE

C Includes:
      
         INCLUDE SUBST_RXCMMN    ! for chem mechanism name
         INCLUDE SUBST_CONST     ! constants

C Arguments:

         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

C External Functions:


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
         INTEGER STATUS
#ifdef Verbose
         integer s, v
#endif

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = SETUP_LOGDEV()

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
            EMLAYS = NLAYS   ! default
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
            NEMIS_AE = N_AE_EMIS  ! defined in CGRID_SPCS
            CALL MAP_PMEMIS( )   ! set up PM emitted species mapping
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

#ifdef Verbose
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

         ALLOCATE ( VDEMIS ( N_SPC_EMIS+1,EMLAYS,MY_NCOLS,MY_NROWS ),
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

         SUCCESS = .TRUE.; RETURN

         END FUNCTION EMIS_INIT

C-----------------------------------------------------------------------
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA,
     &                         RDX3F, RRHOJ, RJACM )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE LTNG_DEFN           ! lightning NO emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE PT3D_DEFN           ! plume rise emissions
         USE PTMAP               ! defines emissions file(s) PM mapping
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:

         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 2 )    ! time step vector (HHMMSS)
         REAL,    INTENT( IN ) :: CONVPA        ! conversion factor to pressure in Pascals
         REAL,    INTENT( IN ) :: RDX3F( NLAYS )             ! reciprocal layer thickness
         REAL,    INTENT( IN ) :: RRHOJ( NCOLS,NROWS,NLAYS ) ! reciprocal density X Jacobian
         REAL,    INTENT( IN ) :: RJACM( NCOLS,NROWS,NLAYS ) ! reciprocal mid-layer Jacobian

C External Functions:

C Local Variables:

         REAL             CNVTC         ! combined beis conversion factor
         REAL             CNVTI         ! intermediate combined beis conv. factor
         REAL, SAVE    :: CNVTP         ! intermediate combined beis conv. factor
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V ! loop induction variables

         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
C Get domain decomp info from the emissions file
            CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
            CNVTP = CONVPA * CNVTE
         END IF

C read & interpolate emissions (create VDEMIS in the species class order)

         VDEMIS = 0.0   ! array assignment

         DO V = STRT_GC, FINI_GC
            S = V + 1 - STRT_GC
            BUFF = 0.0   ! array assignment
C                                       vvvvvvv - emis surrogate names
            IF ( .NOT. INTERPX( EMIS_1, GC_EMIS( S ), PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLYRS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read  '
     &              // TRIM( GC_EMIS( S ) ) // ' from ' // EMIS_1
!              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF

C reactive gases (conversion to ppmv/s) VDEMIS in this order from RDEMIS
C "Stores read an entire cache line, modify the target, then write back the
C  entire line. Thus, non-consecutive stores are worse than non-consecutive
C  loads."
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + GC_EMIS_FAC( S ) * BUFF( C,R,L )
                  END DO
               END DO
            END DO

#ifdef Verbose
         write( logdev,* ) '  GC EMIS: ', s, '  ', gc_emis( s )
#endif

         END DO   ! V

C reactive gases conversion mol/s to ppmv/s

         IF ( N_GC_EMIS .GT. 0 ) THEN
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
                     DO V = STRT_GC, FINI_GC
                        VDEMIS( V,L,C,R ) = CNVTC * VDEMIS( V,L,C,R )
                     END DO
                  END DO
               END DO
            END DO
         END IF

C beis

         IF ( BIOGEMIS ) THEN

            CALL GET_BEIS ( JDATE, JTIME, TSTEP )

C tmpbeis312 is based on hourly input - need to interpolate return value
C merge: VDEMIS_BI in mol/hr. Convert to ppm/sec

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
     &                                    + CNVTC * VDEMIS_BI( N,C,R )
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
     &                                 + VDEMIS_LT( C,R,L ) * CNVTC
                  END DO
               END DO
            END DO

         END IF

C plume rise

         IF ( PT3DEMIS ) THEN

            CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

C merge: VDEMIS_PT in mol/sec. Convert to ppm/sec
C If aerosols, then the 6 PM emissions obtained from GET_PT3D_EMIS need to be
C accessed by GET_AERO_EMIS to be "speciated" and merged with the 27 AE emissions.

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
!                    DO S = 1, N_SPC_PTEM
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
!    &                                    + VDEMIS_AE( S,L,C,R )
     &                                    + VDEMIS_AE( S,L,C,R ) * AE_EMIS_FAC( S )  ! DOES THIS NEED TO BE MAPPED?
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
               XMSG = 'Could not read  '
     &              // TRIM( NR_EMIS( S ) ) // ' from ' // EMIS_1
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
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

#ifdef Verbose
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
     &                 // TRIM( TR_EMIS( S ) ) // ' from ' // EMIS_TRAC_1
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

#ifdef Verbose
               write( logdev,* ) '  TR EMIS: ', s, '  ', tr_emis( s )
#endif

            END DO

         END IF

C zero out emissions values for species not included in diffused list ...
C accounts for emissions species names as a subset of the vert. diffused species list

         VDEMIS( N_SPC_EMIS+1,:,:,: ) = 0.0

         RETURN

         END SUBROUTINE GET_EMIS

      END MODULE EMIS_DEFN
