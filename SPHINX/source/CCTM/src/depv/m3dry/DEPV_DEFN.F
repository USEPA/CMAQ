
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE DEPV_DEFN

!-----------------------------------------------------------------------
C Function: deposition velocity interface to the chemistry-transport model

C Revision History:
C   24 Nov 2006 J.Young: initial implementation using Bill Hutzell's and
C                        Tanya Otte's work
C   29 Jan 2010 D. Wong: Eliminate potential race condition at the MYPE = 0
C                        to open the diagnostic file. Eliminate the BARRIER
C                        and the CLOSE3 and OPEN3.
C   21 Jun 2010 J.Young: convert for Namelist redesign
C   27 Jan 2011 J.Young: remove references to AERO3 and AERO4
C   20 Jan 2011 J. Bash: Passed the component fluxes and tstep between vdiff 
C                        & m3dry
C   16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C    5 May 2011 J.Young: remove writing diagnostic output for initial call
C   22 Sep 2011 D. Wong: - incorporated twoway model implemenation
C                        - used a robust way to compute WTIME
C   11 Oct 2011 J.Young: PUBLIC/PRIVATE sections, re-do WTIME computation
C                        and WTIME computation for MOSAIC
C   13 Dec 2011 J.Young: removed separate SOA variables - no longer used
C   18 Sep 2012 D. Wong: invoke INIT_LSM regardless of MOSAIC or ABFLUX option
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C   7 Nov 2014  J. Bash: Modified for the restructuring of vidff. Moved constants and data to 
C                       ASX_DATA_MOD.
C   Aug 2015 D. Wong:    Replaced MYPE with IO_PE_INCLUSIVE to facilitate
C                          parallel I/O implementation
C                        Added a block of code to let non I/O processors to
C                          open CTM_DEPV_DIAG in parallel I/O implementation
C   1 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                        clauses
C   12 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C   August 2020 J Pleim: Fixed timestep in call to m3dry. Corrects NH3 bidi emis, dep outputs
C-------------------------------------------------------------------------------
      USE RUNTIME_VARS
      USE CGRID_SPCS, ONLY : N_SPC_DEPV

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, S_PVD, S_CGRIDL1, S_PLDV
#endif

      IMPLICIT NONE

C model depositon velocites array
      REAL,    ALLOCATABLE, SAVE :: DEPV( :,:,: )

C no. of deposition velocity model species
      INTEGER, SAVE :: N_GAS_DEPV

C ddep spc conversion factors
      REAL,    ALLOCATABLE, SAVE :: DD_CONV( : )

C model-oriented bi-directional production [ppm-m/s]
      REAL,    ALLOCATABLE, SAVE :: PLDV( :,:,: )

C  Dry dep flux of NH3 for bidi [kg/ha hr]
      REAL,    ALLOCATABLE, SAVE :: NH3_EMIS( :,: )
      REAL,    ALLOCATABLE, SAVE :: NH3_DDEP( :,: )
!C flag for mosaic - luc specific deposition - within in-lining depv
!      LOGICAL, SAVE :: MOSAIC = .FALSE.

!C flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
!      LOGICAL, SAVE :: FST = .FALSE.      

!      PUBLIC DEPV, DEPVJ, DEPVJ_FST, DD_CONV, PLDV, CMP,
!     &             ILDEPV, SFC_HONO, ABFLUX, MOSAIC, FST, N_GAS_DEPV,
!     &             DEPV_INIT, GET_DEPV

      PUBLIC DEPV, N_SPC_DEPV, DD_CONV, PLDV, N_GAS_DEPV,
     &             DEPV_INIT, GET_DEPV, NH3_EMIS, NH3_DDEP
      
      PRIVATE

C gas species (gas-phase and gas non-reactive) deposition velocities
      REAL,    ALLOCATABLE, SAVE :: DEPVEL_GAS( :,:,: )

C m3dry-oriented bi-directional production [ppm-m/s]
      REAL,    ALLOCATABLE, SAVE :: PVD( :,:,: )

C CGRID for m3dry bi-directional flux calculation
      REAL,    ALLOCATABLE, SAVE :: CGRIDL1( :,:,: )

C flag for diagnostic DEPV file
      INTEGER, SAVE                      :: N_UNIQUE_GDEPV   ! gas depv
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: UNIQUE_GDEPV( : )
      INTEGER, ALLOCATABLE, SAVE         :: GAS_DEPV_SUR( : )
      LOGICAL, ALLOCATABLE, SAVE         :: GAS_DEPV_FOUND( : )

      INTEGER, SAVE                      :: N_UNIQUE_ADEPV   ! aero depv
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: UNIQUE_ADEPV( : )
      INTEGER, ALLOCATABLE, SAVE         :: AER_DEPV_SUR( : )

      INTEGER, SAVE :: NR_OFFSET            ! Offset to NR start in CGRID

      REAL,    ALLOCATABLE, SAVE :: ADEPV( :,:,: )    ! Aerosol dep vel from
                                                               ! subroutine AERO_DEPV
      REAL,    ALLOCATABLE, SAVE :: DBUFF( :,: )      ! diagnostic output buffer
      REAL,    ALLOCATABLE, SAVE :: DBUFF3( :,:,: )   ! diagnostic output buffer with 3 dimensions

      REAL,    ALLOCATABLE, SAVE :: UCROSS( :,:,:)
      REAL,    ALLOCATABLE, SAVE :: VCROSS( :,:,:)
      REAL,    ALLOCATABLE, SAVE :: DLUSE( :,: )
      
      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION DEPV_INIT( JDATE, JTIME, TSTEP, CGRID ) RESULT ( SUCCESS )

         USE HGRD_DEFN
         USE RXNS_DATA           ! chemical mechanism data
         USE CGRID_SPCS          ! CGRID mechanism species
         USE DEPVVARS
         USE UTILIO_DEFN
!         USE MOSAIC_MOD, Only: Init_Mosaic
         USE LSM_MOD, Only: Init_LSM !, n_lufrac
         USE BIDI_MOD, Only: Init_Bidi, HgBidi

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER JDATE, JTIME, TSTEP( 3 )
         REAL, POINTER :: CGRID( :,:,:,: )
         LOGICAL SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: FINDEX               ! finds index of a number in a list

C Parameters:
         REAL, PARAMETER :: RGAS1 = 1.0 / RGASUNIV ! univ. gas constant reciprocal
         REAL, PARAMETER :: M2PHA = 1.0E+04        ! 1 hectare = 1.0e4 m**2
         REAL, PARAMETER :: CMLMR = 1.0E+06        ! ppmV/Molar Mixing Ratio
         REAL, PARAMETER :: CNVTD = M2PHA / CMLMR / MWAIR ! combined ddep conv factor
 
C Local variables:
         CHARACTER( 16 ), ALLOCATABLE :: ALL_GAS_DEPV( : )
         LOGICAL, ALLOCATABLE         :: AE_DEPV_FOUND( : )

C variables for deposition velocities diagnostic file
         INTEGER          :: STATUS      ! ENV... status
         CHARACTER( 80 )  :: VARDESC     ! environment variable description

C environment variable for diagnostic DEPV file
         CHARACTER( 16 )  :: CTM_DEPV_FILE = 'CTM_DEPV_FILE'
         
         CHARACTER( 16 )  :: PNAME = 'DEPV_INIT'
         CHARACTER( 120 ) :: XMSG = ' '

         INTEGER          NDX, NDX2, NDX3      ! Temp index
         INTEGER          ALLOCSTAT
         INTEGER          J, L, N, S, V, STRT
         INTEGER, SAVE    :: n_spc_m3dry = ltotg       ! from DEPVVARS module

         INTERFACE
            SUBROUTINE OPDEPV_DIAG ( JDATE, JTIME, TSTEP,
     &                               N_GDEPV_NAMES, GDEPV_NAMES,
     &                               N_ADEPV_NAMES, ADEPV_NAMES )
               INTEGER,         INTENT( IN ) :: JDATE, JTIME, TSTEP
               INTEGER,         INTENT( IN ) :: N_GDEPV_NAMES
               CHARACTER( 16 ), INTENT( IN ) :: GDEPV_NAMES( : )
               INTEGER,         INTENT( IN ) :: N_ADEPV_NAMES
               CHARACTER( 16 ), INTENT( IN ) :: ADEPV_NAMES( : )
            END SUBROUTINE OPDEPV_DIAG

            SUBROUTINE GAS_DEPV_MAP ( N_DEPV_NAMES, DEPV_NAMES, DEPV_SUR )
               INTEGER,         INTENT( IN )    :: N_DEPV_NAMES
               CHARACTER( 16 ), INTENT( INOUT ) :: DEPV_NAMES( : )
               INTEGER,         INTENT( OUT )   :: DEPV_SUR( : )
            END SUBROUTINE GAS_DEPV_MAP
         END INTERFACE

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         N_GAS_DEPV = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV

         ALLOCATE ( DEPV( N_SPC_DEPV+1,NCOLS,NROWS ),
     &              PLDV( N_SPC_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DEPV or PLDV arrays'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

#ifdef sens
         ALLOCATE ( S_PLDV( NPMAX,N_SPC_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating S_PLDV array'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         S_PLDV = 0.0   ! array assignment
#endif

         ALLOCATE ( DEPVEL_GAS( N_GAS_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DEPVEL_GAS array'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

C Initialize deposition velocities for nondeposited species to zero:
C setting DEPV( N_SPC_DEPV+1,C,R ) = 0.0 accounts for dry dep. species names
C as a subset of the vert. diffused species list
         DEPV = 0.0   ! array assignment
         PLDV = 0.0   ! array assignment

         ALLOCATE ( DD_CONV( N_SPC_DEPV+1 ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD_CONV'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

C set dry dep conversion factor (done here so that vdiff doesn`t need the include files)
         S = 0
         DO V = 1, N_GC_DEPV
            S = S + 1
            DD_CONV( S ) = CNVTD * GC_MOLWT( GC_DEPV_MAP( V ) )
         END DO

         DO V = 1, N_AE_DEPV
            S = S + 1
            IF ( AE_SPC( AE_DEPV_MAP( V ) )( 1:3 ) .EQ. 'NUM' ) THEN
               DD_CONV( S ) = M2PHA * 1.0E+3 / MWAIR   ! --> #/Ha
            ELSE IF ( AE_SPC( AE_DEPV_MAP( V ) )( 1:3 ) .EQ. 'SRF' ) THEN
               DD_CONV( S ) = M2PHA * 1.0E+03 / MWAIR  ! --> M**2/Ha
            ELSE
               DD_CONV( S ) = CNVTD * AE_MOLWT( AE_DEPV_MAP( V ) )
            END IF
         END DO

         DO V = 1, N_NR_DEPV
            S = S + 1
            DD_CONV( S ) = CNVTD * NR_MOLWT( NR_DEPV_MAP( V ) )
         END DO

         DO V = 1, N_TR_DEPV
            S = S + 1
            DD_CONV( S ) = CNVTD * TR_MOLWT( TR_DEPV_MAP( V ) )
         END DO

C Initialize
         IF ( N_AE_DEPV .GT. 0 ) THEN
            ALLOCATE ( ADEPV( N_AE_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ADEPV'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
         END IF

         INIT_GAS_DV:
     &   IF ( N_GAS_DEPV .GT. 0 ) THEN

            ALLOCATE ( ALL_GAS_DEPV( N_GAS_DEPV ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ALL_GAS_DEPV'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            DO V = 1, N_GC_DEPV
               ALL_GAS_DEPV( V ) = GC_DEPV( V )
            END DO
            STRT = N_GC_DEPV
            DO V = 1, N_NR_DEPV
               ALL_GAS_DEPV( V + STRT ) = NR_DEPV( V )
            END DO
            STRT = N_GC_DEPV + N_NR_DEPV
            DO V = 1, N_TR_DEPV
               ALL_GAS_DEPV( V + STRT ) = TR_DEPV( V )
            END DO

C Identify and remove multiple occurrences of gas deposition velocities
            ALLOCATE( GAS_DEPV_FOUND( N_GAS_DEPV ),
     &                GAS_DEPV_SUR  ( N_GAS_DEPV ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating GAS_DEPV_FOUND or GAS_DEPV_SUR'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            GAS_DEPV_FOUND = .FALSE.
            N_UNIQUE_GDEPV = 0

            LOOP_UNIQUE:
     &      DO N = 1, N_GAS_DEPV
               IF ( GAS_DEPV_FOUND( N ) ) CYCLE LOOP_UNIQUE
               DO V = 1, N_GAS_DEPV
                  IF ( ALL_GAS_DEPV( N ) .EQ. ALL_GAS_DEPV( V ) ) THEN
                     IF ( GAS_DEPV_FOUND( V ) ) CYCLE LOOP_UNIQUE
                     N_UNIQUE_GDEPV = N_UNIQUE_GDEPV + 1
                     GAS_DEPV_FOUND( V ) = .TRUE.
                     CYCLE LOOP_UNIQUE
                  END IF
               END DO
            END DO LOOP_UNIQUE

            ALLOCATE( UNIQUE_GDEPV( N_UNIQUE_GDEPV ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating UNIQUE_GDEPV'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

#ifdef Verbose
            write( logdev,* ) ' '
            write( logdev,* ) ' All Gas DEPVs           Requested Gas DEPV'
#endif

            V = 1
            DO N = 1, N_GAS_DEPV
               IF ( GAS_DEPV_FOUND( N ) ) THEN
                  UNIQUE_GDEPV( V ) = ALL_GAS_DEPV( N )
#ifdef Verbose
                  write( logdev,1019 ) n, all_gas_depv( n ), v, unique_gdepv( v )
1019              format( 2x, i5, 1x, a16, 2x, i5, 1x, a16 )
#endif
                  V = V + 1
#ifdef Verbose
               else
                  write( logdev,1019 ) n, all_gas_depv( n ), -1, ' '
#endif
               END IF
            END DO

C Get pointers to model species (GAS_DEPV_SUR)
            CALL GAS_DEPV_MAP ( N_UNIQUE_GDEPV, UNIQUE_GDEPV, GAS_DEPV_SUR )

C Identify and remove multiple occurrences of aerosol deposition velocities

            ALLOCATE( AE_DEPV_FOUND( N_AE_DEPV ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating AE_DEPV_FOUND'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            AE_DEPV_FOUND = .FALSE.
            N_UNIQUE_ADEPV = 0

            AERO_UNIQUE:
     &      DO N = 1, N_AE_DEPV
               IF ( AE_DEPV_FOUND( N ) ) CYCLE AERO_UNIQUE
               DO V = 1, N_AE_DEPV
                  IF ( AE_DEPV( N ) .EQ. AE_DEPV( V ) ) THEN
                     IF ( AE_DEPV_FOUND( V ) ) CYCLE AERO_UNIQUE
                     N_UNIQUE_ADEPV = N_UNIQUE_ADEPV + 1
                     AE_DEPV_FOUND( V ) = .TRUE.
                     CYCLE AERO_UNIQUE
                  END IF
               END DO
            END DO AERO_UNIQUE

            ALLOCATE( UNIQUE_ADEPV( N_UNIQUE_ADEPV ) )
            ALLOCATE( AER_DEPV_SUR( N_UNIQUE_ADEPV ) )

#ifdef Verbose
            write( logdev,* ) ' '
            write( logdev,* ) ' Requested Aerosol DEPV and pointer'
#endif

            V = 1
            DO N = 1, N_AE_DEPV
               IF ( AE_DEPV_FOUND( N ) ) THEN
                  UNIQUE_ADEPV( V ) = AE_DEPV( N )
                  AER_DEPV_SUR( V ) = N
#ifdef Verbose
                  write( logdev,1021 ) n, v, unique_adepv( v ), aer_depv_sur( v )
1021              format( 2i5, 1x, a16, i5 )
#endif
                  V = V + 1
               END IF
            END DO

#ifdef Verbose
            write( logdev,* ) ' '
#endif

            ALLOCATE ( CGRIDL1( N_GAS_DEPV,NCOLS,NROWS ),
     &                 PVD    ( N_GAS_DEPV,NCOLS,NROWS ), 
     &                 NH3_EMIS( NCOLS,NROWS),
     &                 NH3_DDEP(NCOLS,NROWS), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating CGRIDL1, CMP or PVD arrays'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            CGRIDL1 = 0.0   ! array assignment (PVD initalized in m3dry)
            NH3_EMIS = 0.0
            NH3_DDEP = 0.0

#ifdef sens
               ALLOCATE ( S_CGRIDL1( NPMAX,N_GAS_DEPV,NCOLS,NROWS ),
     &                    S_PVD    ( NPMAX,N_GAS_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating S_CGRIDL1 or S_PVD arrays'
                  CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF
               S_CGRIDL1 = 0.0
#endif

         END IF INIT_GAS_DV

         WRITE( LOGDEV,'( 5X, A / )' ) 'DEPV_INIT: completed INIT_GAS_DV block '

         IF ( DEPV_DIAG ) THEN

            ALLOCATE ( DBUFF( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DBUFF'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( IO_PE_INCLUSIVE ) THEN
               CALL OPDEPV_DIAG ( JDATE, JTIME, TSTEP( 1 ),
     &                            N_UNIQUE_GDEPV, UNIQUE_GDEPV,
     &                            N_UNIQUE_ADEPV, UNIQUE_ADEPV )
            END IF

         END IF

         END FUNCTION DEPV_INIT
C-----------------------------------------------------------------------


         SUBROUTINE GET_DEPV( JDATE, JTIME, TSTEP, CGRID)
C-----------------------------------------------------------------------

         USE HGRD_DEFN
         USE CGRID_SPCS          ! CGRID mechanism species
         USE DEPVVARS
         USE UTILIO_DEFN
         USE ASX_DATA_MOD, Only: GRID_DATA !, MOSAIC_DATA
#ifdef mpas
         Use util_module, only : time2sec, nextime, currstep
         use mio_module
#endif

         IMPLICIT NONE

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME     ! simulation date&time, tstep
         INTEGER, INTENT( IN ) :: TSTEP( 3 )       ! time step vector (HHMMSS)
                                                   ! TSTEP(1) = local output step
                                                   ! TSTEP(2) = sciproc sync. step (chem)
                                                   ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                                   !            step and wrf/cmaq call frequency

         REAL, POINTER         :: CGRID( :,:,:,: ) ! concentrations

         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Parameters:
         REAL, PARAMETER :: RGAS1 = 1.0 / RGASUNIV ! univ. gas constant reciprocal
         REAL, PARAMETER :: H_VAP = 156.0E+03      ! enthalpy of vaporization (J/mol)
         REAL, PARAMETER :: TREF = 298.0           ! Reference temperature for Cstar
         REAL, PARAMETER :: TREF1 = 1.0 / TREF     ! Reciprocal ref temperature

C Local variables:
         INTEGER          STRT, FINI
         LOGICAL       :: WRITE_TIME     ! write to file flag
         LOGICAL       :: WRITE_TIME_MOS ! write to file flag
         INTEGER, SAVE :: WSTEP = 0      ! local write counter [HHMMSS]
         INTEGER, SAVE :: WSTEP_MOS = 0  ! local write counter [HHMMSS]
         INTEGER       :: CDATE, CTIME   ! step beginning date and time
         INTEGER       :: WDATE, WTIME   ! write date and time
         REAL   DTSEC
         CHARACTER( 16 )  :: PNAME = 'GET_DEPV        '
         CHARACTER( 120 ) :: XMSG = ' '

         INTEGER C, R, N, V, NDX, J

         LOGICAL, SAVE :: FIRSTIME = .TRUE.

         CHARACTER (20) :: TIME_STAMP
         integer :: year, month, day, hour, minute

#ifdef Verbose
         integer cw, rw
#endif

         INTERFACE
            SUBROUTINE RDDEPV ( JDATE, JTIME, DEPV )
               INTEGER, INTENT( IN )            :: JDATE, JTIME
               REAL, INTENT( OUT )              :: DEPV( :,:,: )
            END SUBROUTINE RDDEPV            
            SUBROUTINE CGRID_DEPV( GAS_DEPV_FOUND, GAS_DEPV_SUR, CGRID, CGRIDL1 )
               LOGICAL, INTENT( IN )  :: GAS_DEPV_FOUND( : )
               INTEGER, INTENT( IN )  :: GAS_DEPV_SUR( : )
               REAL,    POINTER       :: CGRID( :,:,:,: )
               REAL,    INTENT( OUT ) :: CGRIDL1( :,:,: )
            END SUBROUTINE CGRID_DEPV
            SUBROUTINE M3DRY ( JDATE, JTIME, DTSEC, 
     &                         CGRIDL1, DEPVEL_GAS, PVD, NH3_DDEP)
               INTEGER, INTENT( IN )  :: JDATE, JTIME
               REAL,    INTENT( IN )  :: dtsec
               REAL,    INTENT( IN )  :: CGRIDL1( :,:,: )
               REAL,    INTENT( OUT ) :: DEPVEL_GAS( :,:,: ), PVD( :,:,: ),NH3_DDEP(:,:)
            END SUBROUTINE M3DRY
            SUBROUTINE AERO_DEPV ( CGRID, JDATE, JTIME, TSTEP, ADEPV )
               REAL, POINTER          :: CGRID( :,:,:,: )
               INTEGER, INTENT( IN )  :: JDATE, JTIME, TSTEP
               REAL,    INTENT( OUT ) :: ADEPV( :,:,: )
            END SUBROUTINE AERO_DEPV
         END INTERFACE

C-----------------------------------------------------------------------
         DEPV = 0.0         

! Made default for Hg bidi JOB 9/12/11
         CALL CGRID_DEPV( GAS_DEPV_FOUND, GAS_DEPV_SUR, CGRID, CGRIDL1 )

         DTSEC = FLOAT( TIME2SEC( TSTEP( 2 ) ) )
         CALL M3DRY ( JDATE, JTIME, DTSEC, 
     &                CGRIDL1, DEPVEL_GAS, PVD, NH3_DDEP )

#ifdef Verbose
         cw = ncols/2; rw = nrows/2
!        cw = 17; rw = 43
         write( logdev,1033 ) cw, rw
1033     format( /5x, 'Depv diagnostics for my_col, my_row:', i4, ', ', i4
     &           /6x, 'v', 3x, 'n', 1x, 'variable', 10x, 'ndx',
     &            2x, 'depvel_gas', 4x, 'depv', 9x, 'pvd', 10x, 'pldv' )
#endif

         N = 0
         STRT = 1
         FINI = N_GC_DEPV
         DO V = STRT, FINI
            N = N + 1
            NDX = GAS_DEPV_SUR( N )
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DEPV( V,C,R ) = GC_DEPV_FAC( N ) * DEPVEL_GAS( NDX,C,R )
                  PLDV( V,C,R ) = PVD( NDX,C,R )
#ifdef sens
                  DO NP = 1, NPMAX
                     S_PLDV( NP,V,C,R ) = S_PVD( NP,NDX,C,R )
                  END DO
#endif
#ifdef Verbose
                  if ( c .eq. cw .and. r .eq. rw ) then
                     write( logdev,1039 ) v, n, gc_depv( n ), ndx,
     &                                    depvel_gas( ndx,c,r ), depv( v,c,r ),
     &                                    pvd( ndx,c,r ), pldv( v,c,r )
1039                 format( 3x, 2i4, 1x, a16, 1x, i3, 4( 1x, 1pe12.4 ) )
                  end if
#endif
               END DO
            END DO

         END DO

         N = 0
         STRT = N_GC_DEPV + N_AE_DEPV + 1
         FINI = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV
         DO V = STRT, FINI
            N = N + 1
            NDX = GAS_DEPV_SUR( N+N_GC_DEPV )
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DEPV( V,C,R ) = NR_DEPV_FAC( N ) * DEPVEL_GAS( NDX,C,R )
                  PLDV( V,C,R ) = PVD( NDX,C,R )
#ifdef sens
                  DO NP = 1, NPMAX
                     S_PLDV( NP,V,C,R ) = S_PVD( NP,NDX,C,R )
                  END DO
#endif

#ifdef Verbose
                  if ( c == cw .and. r == rw ) then
                     write( logdev,1039 ) v, n, nr_depv( n ), ndx,
     &                                    depvel_gas( ndx,c,r ), depv( v,c,r ),
     &                                    pvd( ndx,c,r ), pldv( v,c,r )
                  end if
#endif
               END DO
            END DO
         END DO

         N = 0
         STRT = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + 1
         FINI = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + N_TR_DEPV
         DO V = STRT, FINI
            N = N + 1
            NDX = GAS_DEPV_SUR( N + N_GC_DEPV + N_NR_DEPV )

            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DEPV( V,C,R ) = TR_DEPV_FAC( N ) * DEPVEL_GAS( NDX,C,R )
                  PLDV( V,C,R ) = PVD( NDX,C,R )
#ifdef sens
                  DO NP = 1, NPMAX
                     S_PLDV( NP,V,C,R ) = S_PVD( NP,NDX,C,R )
                  END DO
#endif
#ifdef Verbose
                  if ( c == cw .and. r == rw ) then
                     write( logdev,1039 ) v, n, tr_depv( n ), ndx,
     &                                    depvel_gas( ndx,c,r ), depv( v,c,r ),
     &                                    pvd( ndx,c,r ), pldv( v,c,r )
                  end if
#endif
               END DO
            END DO

         END DO      
         
         IF ( N_AE_DEPV .GT. 0 ) THEN
            CALL AERO_DEPV( CGRID, JDATE, JTIME, TSTEP( 1 ), ADEPV )

            STRT = N_GC_DEPV + 1
            FINI = N_GC_DEPV + N_AE_DEPV

            DO R = 1, NROWS
               DO C = 1, NCOLS
                  N = 0
                  DO V = STRT, FINI
                     N = N + 1
                     DEPV( V,C,R ) = AE_DEPV_FAC( N ) * ADEPV( N,C,R )
                  END DO
               END DO
            END DO

         END IF

         IF ( DEPV_DIAG ) THEN

            WRITE_TIME = .FALSE.
            WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
            IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
               WDATE = JDATE; WTIME = JTIME
               CALL NEXTIME( WDATE, WTIME, TSTEP( 2 ) )
               IF ( .NOT. CURRSTEP( WDATE, WTIME, STDATE, STTIME, TSTEP( 1 ),
     &                              CDATE, CTIME ) ) THEN
                  XMSG = 'Cannot get step date and time'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               END IF
               WDATE = CDATE; WTIME = CTIME
               WSTEP = 0
               WRITE_TIME = .TRUE.
            END IF

            IF ( WRITE_TIME ) THEN

#ifdef mpas
               call mio_time_format_conversion (wdate, wtime, time_stamp)
#else
#ifdef parallel_io
               IF ( FIRSTIME ) THEN
                  FIRSTIME = .FALSE.
                  IF ( DEPV_DIAG ) THEN
                     IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                        IF ( .NOT. OPEN3( CTM_DEPV_DIAG, FSREAD3, PNAME ) ) THEN
                           XMSG = 'Could not open ' // TRIM( CTM_DEPV_DIAG )
                           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                        END IF
                     END IF
                  END IF

               END IF
#endif
#endif

               N = 0
               DO V = 1, N_GC_DEPV
                  N = N + 1

                  IF ( GAS_DEPV_FOUND( N ) ) THEN
                     NDX = GAS_DEPV_SUR( N )

                     DO R = 1, NROWS
                        DO C = 1, NCOLS
                           DBUFF( C,R ) = 100.0 * DEPV( N,C,R )   ! cm/sec
                        END DO
                     END DO
                     
#ifdef mpas
                     call mio_fwrite (CTM_DEPV_DIAG, UNIQUE_GDEPV( NDX), pname,
     &                                real(DBUFF(:,1), 4), TIME_STAMP)
#else

                     IF ( .NOT. WRITE3( CTM_DEPV_DIAG, UNIQUE_GDEPV( NDX ),
     &                                  WDATE, WTIME, DBUFF ) ) THEN
                         XMSG = 'Could not write ' // CTM_DEPV_DIAG // ' file'
                         CALL M3EXIT ( PNAME, WDATE, WTIME, XMSG, XSTAT1 )
                     END IF
#endif
                  END IF

               END DO

               DO V = 1, N_NR_DEPV
                  N = N + 1

                  IF ( GAS_DEPV_FOUND( N ) ) THEN
                     NDX = GAS_DEPV_SUR( N )

                     DO R = 1, NROWS
                        DO C = 1, NCOLS
                           DBUFF( C,R ) = 100.0 * DEPV( N+N_AE_DEPV,C,R )   ! cm/sec
                        END DO
                     END DO
#ifdef mpas
                     call mio_fwrite (CTM_DEPV_DIAG, UNIQUE_GDEPV( NDX), pname,
     &                                real(DBUFF(:,1), 4), TIME_STAMP)
#else
                     IF ( .NOT. WRITE3( CTM_DEPV_DIAG, UNIQUE_GDEPV( NDX ),
     &                                  WDATE, WTIME, DBUFF ) ) THEN
                         XMSG = 'Could not write ' // CTM_DEPV_DIAG // ' file'
                         CALL M3EXIT ( PNAME, WDATE, WTIME, XMSG, XSTAT1 )
                     END IF
#endif

                  END IF

               END DO

               DO N = 1, N_UNIQUE_ADEPV

                  NDX = AER_DEPV_SUR( N )

                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        DBUFF( C,R ) = 100.0 * ADEPV( NDX,C,R )   ! cm/sec
                     END DO
                  END DO
#ifdef mpas
                  call mio_fwrite (CTM_DEPV_DIAG, UNIQUE_ADEPV( N ), pname,
     &                             real(DBUFF(:,1), 4), TIME_STAMP)
#else
                  IF ( .NOT. WRITE3( CTM_DEPV_DIAG, UNIQUE_ADEPV( N ),
     &                               WDATE, WTIME, DBUFF ) ) THEN
                      XMSG = 'Could not write ' // CTM_DEPV_DIAG // ' file'
                      CALL M3EXIT ( PNAME, WDATE, WTIME, XMSG, XSTAT1 )
                  END IF
#endif

               END DO

            END IF   ! write_time

         END IF   ! DEPV_DIAG

         END SUBROUTINE GET_DEPV

      END MODULE DEPV_DEFN
