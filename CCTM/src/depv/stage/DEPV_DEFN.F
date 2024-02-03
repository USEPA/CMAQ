
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
      MODULE DEPV_DEFN

C-----------------------------------------------------------------------
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
C   Feb 2019 D. Wong:    removed all MY_N clauses
C-------------------------------------------------------------------------------
      USE RUNTIME_VARS
      USE CGRID_SPCS, ONLY : N_SPC_DEPV

      IMPLICIT NONE

C no. of deposition velocity model species
      INTEGER, SAVE :: N_GAS_DEPV

C ddep spc conversion factors
      REAL,    ALLOCATABLE, SAVE :: DD_CONV( : )

      PUBLIC DD_CONV, N_GAS_DEPV, DEPV_INIT, GET_DEPV

      PRIVATE

C CGRID for m3dry bi-directional flux calculation
      REAL,    ALLOCATABLE, SAVE :: CGRIDL1( :,:,: )
      REAL,                 SAVE :: DTSEC

C flag for diagnostic DEPV file
      INTEGER, SAVE                      :: N_UNIQUE_GDEPV   ! gas depv
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: UNIQUE_GDEPV( : )
      INTEGER, ALLOCATABLE, SAVE         :: GAS_DEPV_SUR( : )
      LOGICAL, ALLOCATABLE, SAVE         :: GAS_DEPV_FOUND( : )

      INTEGER, SAVE                      :: N_UNIQUE_ADEPV   ! aero depv
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: UNIQUE_ADEPV( : )
      INTEGER, ALLOCATABLE, SAVE         :: AER_DEPV_SUR( : )

      INTEGER, SAVE :: NR_OFFSET            ! Offset to NR start in CGRID

      INTEGER :: GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      REAL,    ALLOCATABLE, SAVE :: UCROSS( :,:,:)
      REAL,    ALLOCATABLE, SAVE :: VCROSS( :,:,:)
      REAL,    ALLOCATABLE, SAVE :: DLUSE( :,: )
      
      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION DEPV_INIT( JDATE, JTIME, TSTEP, CGRID ) RESULT ( SUCCESS )

         USE HGRD_DEFN
         USE RXNS_DATA           ! chemical mechanism data
         USE CGRID_SPCS          ! CGRID mechanism species
         USE STAGE_DATA
         USE UTILIO_DEFN
         USE LSM_MOD, Only: Init_LSM
         USE MOSAIC_MOD, Only: Tile_Data
         USE BIDI_MOD, Only: Init_Bidi
         USE STAGE_MOD, Only: INIT_STAGE
         USE STAGE_OUTPUT

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER JDATE, JTIME, TSTEP( 3 )
         REAL, POINTER :: CGRID( :,:,:,: )
         LOGICAL SUCCESS

C Parameters:
         REAL, PARAMETER :: RGAS1 = 1.0 / RGASUNIV ! univ. gas constant reciprocal
         REAL, PARAMETER :: M2PHA = 1.0E+04        ! 1 hectare = 1.0e4 m**2
         REAL, PARAMETER :: CMLMR = 1.0E+06        ! ppmV/Molar Mixing Ratio
         REAL, PARAMETER :: CNVTD = M2PHA / CMLMR / MWAIR ! combined ddep conv factor
 
C Local variables:
         CHARACTER( 16 ), ALLOCATABLE :: ALL_GAS_DEPV( : )
         LOGICAL, ALLOCATABLE         :: AE_DEPV_FOUND( : )
         
         CHARACTER( 16 )  :: PNAME = 'DEPV_INIT'
         CHARACTER( 120 ) :: XMSG = ' '

         INTEGER          NDX, NDX2, NDX3      ! Temp index
         INTEGER          ALLOCSTAT
         INTEGER          J, L, N, S, V, STRT, FINI

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         N_GAS_DEPV = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV
         N_SPC_DEPV = N_GAS_DEPV + N_AE_DEPV


C Initialize deposition velocities for nondeposited species to zero:
C setting DEPV( N_SPC_DEPV+1,C,R ) = 0.0 accounts for dry dep. species names
C as a subset of the vert. diffused species list
         ALLOCATE ( DD_CONV( N_SPC_DEPV ), STAT = ALLOCSTAT )
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
               DD_CONV( S ) = M2PHA * 1.0E+03 / MWAIR  ! --> #/Ha
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

         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )                       

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
! Read the CMAQ_Control_STAGE namelist and map deposition species to the
! gas species namelists, and map surrogate species
            Call Map_Stage ( N_UNIQUE_GDEPV, UNIQUE_GDEPV, GAS_DEPV_SUR )

C Identify and remove multiple occurrences of aerosol deposition velocities

            ALLOCATE( AE_DEPV_FOUND( N_AE_DEPV ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating AE_DEPV_FOUND'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            AE_DEPV_FOUND = .FALSE.
            N_UNIQUE_ADEPV = 0
! use this for mosaic dry deposition
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

            ALLOCATE ( CGRIDL1   ( N_GAS_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating CGRIDL1 array'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            CGRIDL1 = 0.0   

         END IF INIT_GAS_DV

         Tile_Data%Vd_name   = 'Empty'
         Tile_Data%Vd_Out    = .FALSE.
         Tile_Data%n_Vd = 0

         N = 0
         STRT = 1
         FINI = N_GC_DEPV
         DO V = STRT, FINI
            N = N + 1
            NDX = GAS_DEPV_SUR( N )      
            Tile_Data%Vd_Name ( NDX ) = GC_DEPV( N )
            Tile_Data%Vd_Fac  ( NDX ) = GC_DEPV_FAC( N )
            Tile_Data%dep2vdiff ( V ) = NDX 
         END DO

         N = 0
         STRT = N_GC_DEPV + N_AE_DEPV + 1
         FINI = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV
         DO V = STRT, FINI
            N = N + 1
            NDX = GAS_DEPV_SUR( N+N_GC_DEPV )
            Tile_Data%Vd_Name ( NDX ) = NR_DEPV( N )
            Tile_Data%Vd_Fac  ( NDX ) = NR_DEPV_FAC( N )
            Tile_Data%dep2vdiff ( V ) = NDX 
         END DO

         N = 0
         STRT = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + 1
         FINI = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + N_TR_DEPV
         DO V = STRT, FINI
            N = N + 1
            NDX = GAS_DEPV_SUR( N + N_GC_DEPV + N_NR_DEPV )
            Tile_Data%Vd_Name ( NDX ) = TR_DEPV( N )
            Tile_Data%Vd_Fac  ( NDX ) = TR_DEPV_FAC( N )
            Tile_Data%dep2vdiff ( V ) = NDX 
         END DO                      
         
         IF ( N_AE_DEPV .GT. 0 ) THEN
            STRT = N_GC_DEPV + 1
            FINI = N_GC_DEPV + N_AE_DEPV
            N = 0
            DO V = STRT, FINI
               N = N + 1
               NDX = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV + N
               Tile_Data%Vd_Name ( NDX ) = AE_DEPV( N )
               Tile_Data%Vd_Fac  ( NDX ) = AE_DEPV_FAC( N )
               Tile_Data%dep2vdiff ( V ) = NDX 
            END DO

            DEP_UNIQUE: DO N = 1, N_SPC_DEPV
               IF( Tile_Data%Vd_Out( N ) ) CYCLE DEP_UNIQUE
               DO V = 1, N_SPC_DEPV
                  IF ( Tile_Data%Vd_Name( N ) .EQ. Tile_Data%Vd_NAME( V ) .AND.
     &                 Tile_Data%Vd_Name( V ) .NE. 'Empty') THEN
                     IF ( Tile_Data%Vd_Out( V ) ) CYCLE DEP_UNIQUE
                     Tile_Data%n_Vd = Tile_Data%n_Vd + 1
                     Tile_Data%Vd_Out( V ) = .TRUE.
                     CYCLE DEP_UNIQUE
                  END IF
               End DO
            END DO DEP_UNIQUE
         END IF
!*****************************

         Call INIT_STAGE( JDATE, JTIME )
            
         IF ( ( DEPV_DIAG .Or. MOSAIC ) .And. IO_PE_INCLUSIVE ) THEN
            CALL OPEN_DEPV( JDATE, JTIME, TSTEP( 1 ) )
         END IF                           

         END FUNCTION DEPV_INIT
C-----------------------------------------------------------------------


         SUBROUTINE GET_DEPV( JDATE, JTIME, TSTEP, CGRID )
C-----------------------------------------------------------------------

         USE HGRD_DEFN
         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE Mosaic_Mod
         USE ASX_DATA_MOD, Only: GRID_DATA, MOSAIC_DATA
         Use STAGE_OUTPUT
         Use STAGE_MOD,    Only: GAS_X, AERO_X

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
         LOGICAL       :: WRITE_TIME_MOS ! write to file flag
         INTEGER, SAVE :: WSTEP = 0      ! local write counter [HHMMSS]
         INTEGER, SAVE :: WSTEP_MOS = 0  ! local write counter [HHMMSS]
         INTEGER       :: CDATE, CTIME   ! step beginning date and time
         INTEGER       :: WDATE, WTIME   ! write date and time

         CHARACTER( 16 )  :: PNAME = 'GET_DEPV        '
         CHARACTER( 120 ) :: XMSG = ' '

         INTEGER C, R, N, V, NDX, J

         LOGICAL, SAVE :: FIRSTIME = .TRUE.

#ifdef Verbose
         integer cw, rw
#endif

         INTERFACE
            SUBROUTINE CGRID_DEPV( GAS_DEPV_FOUND, GAS_DEPV_SUR, CGRID, CGRIDL1 )
               LOGICAL, INTENT( IN )  :: GAS_DEPV_FOUND( : )
               INTEGER, INTENT( IN )  :: GAS_DEPV_SUR( : )
               REAL,    POINTER       :: CGRID( :,:,:,: )
               REAL,    INTENT( OUT ) :: CGRIDL1( :,:,: )
            END SUBROUTINE CGRID_DEPV
         END INTERFACE

C-----------------------------------------------------------------------       

! Made default for Hg bidi JOB 9/12/11
         CALL CGRID_DEPV( GAS_DEPV_FOUND, GAS_DEPV_SUR, CGRID, CGRIDL1 )

         DTSEC  = FLOAT( TIME2SEC( TSTEP( 2 ) ) )
         DO R = 1, NROWS
            DO C = 1, NCOLS
               CALL GAS_X ( JDATE, JTIME, DTSEC, C, R, CGRIDL1( :,C, R ) )
               IF ( N_AE_DEPV .GT. 0 ) CALL AERO_X( CGRID, C, R )
            END DO
         END DO

         IF ( DEPV_DIAG .Or. MOSAIC ) THEN

            WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )         
            IF ( Write_time( WSTEP, TIME2SEC( TSTEP( 1 ) ) ) ) THEN
               WDATE = JDATE; WTIME = JTIME
               CALL NEXTIME( WDATE, WTIME, TSTEP( 2 ) )
               IF ( .NOT. CURRSTEP( WDATE, WTIME, STDATE, STTIME, TSTEP( 1 ),
     &                              CDATE, CTIME ) ) THEN
                  XMSG = 'Cannot get step date and time'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               END IF
               WDATE = CDATE; WTIME = CTIME
               WSTEP = 0

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

                  IF ( MOSAIC ) THEN
                     IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                        IF ( .NOT. OPEN3( CTM_DEPV_MOS, FSREAD3, PNAME ) ) THEN
                           XMSG = 'Could not open ' // TRIM( CTM_DEPV_MOS )
                           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                        END IF
                     END IF
                  END IF
               END IF
#endif
            Call Write_depv( WDATE, WTIME )

            END IF ! Write time
         END IF ! DEPV_DIAG .Or. MOSAIC         

         END SUBROUTINE GET_DEPV

      END MODULE DEPV_DEFN
