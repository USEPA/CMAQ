
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/PTMAP.F,v 1.8 2011/10/21 16:10:46 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PTMAP

C-----------------------------------------------------------------------
C Function: species mappings from input point source file groups to
C           the intermediate VDEMIS_PT and the model VDEMIS arrays

C Revision History:
C   29 Nov 07 J.Young: initial implementation
C   31 Mar 09 J.Young: replace include files with mechanism namelists
C   17 Sep 09 B.Hutzell: extended mapping algorithm to allow multiple usage
C                        of an emission species from an individual source  
C    6 Apr 11 S.Howard: replace Module PMEM_DEFN with AERO_DATA
C   19 Dec 13 J.Young: fix index bug in AEROSPC log report
C   22 Jun 16 B.Hutzell: Fixed issue where single emis species maps to multiple model
C                        species
C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: IARRY1, CARRY1 ! implementation for multiple
                                        ! groups of stack input files
      IMPLICIT NONE

      INTEGER, SAVE :: N_GSPC_EMIS = 0   ! number of gas species in diagnostic file
      INTEGER, SAVE :: N_ASPC_EMIS = 0   ! number of aerosol species in diagnostic file

C Species names from input file used for point source non-PM emissions mapping
      INTEGER,         ALLOCATABLE, SAVE :: PTEM_MAP( : )
      INTEGER,         ALLOCATABLE, SAVE :: SPC_PTEM_MAP( : )
      REAL,            ALLOCATABLE, SAVE :: SPC_PTEM_FAC( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: GSPC_EMIS( : ) ! gas emis species, diagnostic
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: ASPC_EMIS( : ) ! aero emis species, diagnostic

C Species names from input file used for point source PM emissions mapping
      INTEGER,         ALLOCATABLE, SAVE :: PTPM_MAP( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: STKPM_NAME( : ) ! diagnostic file

C Mapping for point source non-PM emissions (maps only GC, NR and TR)
      TYPE ( IARRY1 ), ALLOCATABLE, SAVE :: STKEM_MAP( : ) ! STKEM index
      TYPE ( IARRY1 ), ALLOCATABLE, SAVE :: GAS_MAP  ( : ) ! VDEMIS_PT index

      INTEGER, SAVE :: N_SPC_PTPM = 0   ! merged no. of unique species for PM

C Mapping for point source PM emissions (maps pt. src. PM spcs to PMEMIS_PT)
      TYPE ( IARRY1 ), ALLOCATABLE, SAVE :: STKPM_MAP( : ) ! STKPM index
      TYPE ( IARRY1 ), ALLOCATABLE, SAVE :: PM_MAP   ( : ) ! PMEMIS_PT index

C-----------------------------------------------------------------------

      CONTAINS
 
         FUNCTION PTMAP_INIT ( NPTGRPS ) RESULT ( SUCCESS )

         USE CGRID_SPCS            ! CGRID mechanism species
         USE STK_EMIS              ! hourly point source emissions
         USE AERO_DATA             ! defines aerosol species
         USE UTILIO_DEFN

         IMPLICIT NONE

         INTEGER, INTENT( IN )  :: NPTGRPS
         LOGICAL SUCCESS

         INTEGER, EXTERNAL :: SETUP_LOGDEV

         TYPE :: STRNDX
            CHARACTER( 16 ) :: STR
            INTEGER         :: NDX
         END TYPE STRNDX

         TYPE ( STRNDX ), ALLOCATABLE :: V( : )
         TYPE ( STRNDX ), ALLOCATABLE :: VC( : )
         TYPE ( STRNDX )              :: TV

         TYPE ( CARRY1 ), ALLOCATABLE :: STKEM_VAR( : )   ! just to record on log
         TYPE ( CARRY1 ), ALLOCATABLE :: STKPM_VAR( : )   ! just to record on log

         INTEGER          :: N_GAS_EMIS
         INTEGER          :: N_SPC_PTEM  = 0   ! merged no. of unique species for non_PM

         INTEGER, ALLOCATABLE          :: GMAP( : )
         INTEGER, ALLOCATABLE          :: GLST( :,: )
         CHARACTER( 16 ), ALLOCATABLE  :: GVNM( : )
         CHARACTER( 16 ), ALLOCATABLE  :: GSPC( : )
         CHARACTER( 16 ), ALLOCATABLE  :: STKEM_NAME( : ) ! gas emissions names

         INTEGER          :: PMAP( N_EMIS_PM*NPTGRPS )
         INTEGER          :: PLST( N_EMIS_PM,NPTGRPS )
         CHARACTER( 16 )  :: PVNM( N_EMIS_PM*NPTGRPS )
         CHARACTER( 16 )  :: VNAME

         CHARACTER( 16 )  :: PNAME = 'PTMAP_INIT'  ! procedure name
         INTEGER I, I2, J, K, L, M, N, NM, NCULL, IOS, LOGDEV
         INTEGER IDX                                      
         INTEGER NSPC1, NSPC2, NSPC3

C-----------------------------------------------------------------------

         LOGDEV = SETUP_LOGDEV()

         SUCCESS = .TRUE.

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

         N_GAS_EMIS = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS

         ALLOCATE ( GMAP( N_GAS_EMIS*NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GMAP', PNAME )
         GMAP = 0   ! array assignment

         ALLOCATE ( GLST( N_GAS_EMIS,NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GLST', PNAME )
         GLST = 0   ! array assignment

         ALLOCATE ( GVNM( N_GAS_EMIS*NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GVNM', PNAME )
         GVNM = ' '   ! array assignment

         ALLOCATE ( GSPC( N_GAS_EMIS*NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GSPC', PNAME )
         GSPC = ' '   ! array assignment

C point source emissions to gas-phase species map

         ALLOCATE ( STKEM_MAP( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'STKEM_MAP', PNAME )

         ALLOCATE ( GAS_MAP( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GAS_MAP', PNAME )

         ALLOCATE ( STKEM_VAR( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'STKEM_VAR', PNAME )

!        WRITE( LOGDEV,1005 )

         NM = 0; L = 0
         DO N = 1, NPTGRPS
            
            I = 0
            LOOP_GC: DO J = 1, N_GC_EMIS
               VNAME = GC_EMIS( J )
               IDX = INDEX1( VNAME, NVARS_EMIS( N ), STKSPC( N )%ARRY )
               IF ( IDX .GT. 0 ) THEN
#ifdef verbose_ptmap
                  WRITE ( LOGDEV,* ) TRIM( VNAME ), ' added to GVNM array '
#endif
                  I = I + 1
                  L = L + 1
                  GMAP( L ) = J
                  GLST( I,N ) = IDX
                  GVNM( L ) = VNAME
                  GSPC( L ) = GC_SPC( GC_EMIS_MAP( J ) )
#ifdef verbose_ptmap
                  WRITE( LOGDEV,1009 ) N, J, I, L, GLST( I,N ), GMAP( L ),
     &                                 TRIM( GVNM( L ) ), TRIM( GSPC( L ) )
#endif
               END IF
            END DO LOOP_GC

            LOOP_NR: DO J = 1, N_NR_EMIS
               VNAME = NR_EMIS( J )
               IDX = INDEX1( VNAME, NVARS_EMIS( N ), STKSPC( N )%ARRY )
               IF ( IDX .GT. 0 ) THEN
#ifdef verbose_ptmap
                  WRITE ( LOGDEV,* ) TRIM( VNAME ), ' added to GVNM array '
#endif
                  I = I + 1
                  L = L + 1
                  GMAP( L ) = J + NSPC2
                  GLST( I,N ) = IDX
                  GVNM( L ) = VNAME
                  GSPC( L ) = NR_SPC( NR_EMIS_MAP( J ) )
#ifdef verbose_ptmap
                  WRITE( LOGDEV,1009 ) N, J, I, L, GLST( I,N ), GMAP( L ),
     &                                 TRIM( GVNM( L ) ), TRIM( GSPC( L ) )
#endif
               END IF
            END DO LOOP_NR

            LOOP_TR: DO J = 1, N_TR_EMIS
               VNAME = TR_EMIS( J )
               IDX = INDEX1( VNAME, NVARS_EMIS( N ), STKSPC( N )%ARRY )
               IF ( IDX .GT. 0 ) THEN
#ifdef verbose_ptmap
                  WRITE ( LOGDEV,* ) TRIM( VNAME ), ' added to GVNM array '
#endif
                  I = I + 1
                  L = L + 1
                  GMAP( L ) = J + NSPC3
                  GLST( I,N ) = IDX
                  GVNM( L ) = VNAME
                  GSPC( L ) = TR_SPC( TR_EMIS_MAP( J ) )
#ifdef verbose_ptmap
                  WRITE( LOGDEV,1009 ) N, J, I, L, GLST( I,N ), GMAP( L ),
     &                                 TRIM( GVNM( L ) ), TRIM( GSPC( L ) )
#endif
               END IF
            END DO LOOP_TR

            NM = NM + I
            
            STKEM_MAP( N )%LEN = I
            ALLOCATE ( STKEM_MAP( N )%ARRY( I ), STAT=IOS )
            CALL CHECKMEM( IOS, 'STKEM_MAP', PNAME )
            STKEM_MAP( N )%ARRY = 0   ! array assignment

            STKEM_VAR( N )%LEN = I
            ALLOCATE ( STKEM_VAR( N )%ARRY( I ), STAT=IOS )
            CALL CHECKMEM( IOS, 'STKEM_VAR', PNAME )
            STKEM_VAR( N )%ARRY = ' '   ! array assignment

            GAS_MAP( N )%LEN = I
            ALLOCATE ( GAS_MAP( N )%ARRY( I ), STAT=IOS )
            CALL CHECKMEM( IOS, 'GAS_MAP', PNAME )
            GAS_MAP( N )%ARRY = 0   ! array assignment

         END DO   ! NPTGRPS

#ifdef verbose_ptmap
         WRITE( LOGDEV,'(/ 5X, A, I3 /)' ) 'NM =', NM
#endif
         L = 0
         DO N = 1, NPTGRPS
#ifdef verbose_ptmap
            WRITE( LOGDEV,1011 ) N, STKEM_MAP( N )%LEN
#endif
            DO I = 1, STKEM_MAP( N )%LEN
               L = L + 1
               STKEM_MAP( N )%ARRY( I ) = GLST( I,N )
               STKEM_VAR( N )%ARRY( I ) = GVNM( L )
            END DO
#ifdef verbose_ptmap
            DO I = 1, STKEM_MAP( N )%LEN, 8
               I2 = MIN( I + 7, STKEM_MAP( N )%LEN )
               WRITE( LOGDEV,1015 ) N, ( STKEM_MAP( N )%ARRY( J ), J = I, I2 )
               WRITE( LOGDEV,1019 ) N, ( TRIM( STKEM_VAR( N )%ARRY( J ) ), J = I, I2 )
            END DO
#endif
         END DO

         ALLOCATE( V( NM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'V', PNAME )
         ALLOCATE( VC( NM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'VC', PNAME )

         DO I = 1, NM
            V( I )%STR = GVNM( I )
            V( I )%NDX = GMAP( I )
         END DO

C Sort ...
#ifdef verbose_ptmap
         WRITE( LOGDEV,'(5X, A)' ) 'Concatenated Species List:'
         WRITE( LOGDEV,'(5X, I3, 1X, A, I3)' ) 1, V( 1 )
#endif
         DO I = 2, NM
#ifdef verbose_ptmap
            WRITE( LOGDEV,'(5X, I3, 1X, A, I3)' ) I, V( I )
#endif
            TV = V( I )
            DO J = I - 1, 1, -1
               IF ( V( J )%NDX .LE. TV%NDX ) GO TO 101
               V( J+1 ) = V( J )
            END DO
            J = 0
101         CONTINUE
            V( J+1 ) = TV
         END DO

#ifdef verbose_ptmap
         WRITE( LOGDEV,'(5X, A)' ) 'Sorted Species List:'
         DO I = 1, NM
            WRITE( LOGDEV,'(5X, I3, 1X, A, I3)' ) I, V( I )
         END DO
#endif

C Cull ...
         VC( 1 ) = V( 1 )
         NCULL = 1
         DO I = 2, NM
            IF ( V( I )%STR .NE. VC( NCULL )%STR .OR.
     &           V( I )%NDX .NE. VC( NCULL )%NDX ) THEN
               NCULL = NCULL + 1
               VC( NCULL ) = V( I )
            END IF
         END DO
#ifdef verbose_ptmap
         WRITE( LOGDEV,'(/ 5X, A, I3 / )' ) 'Unique Gas =', NCULL
#endif
         N_SPC_PTEM = NCULL

         ALLOCATE ( PTEM_MAP( N_GAS_EMIS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PTEM_MAP', PNAME )

         ALLOCATE ( SPC_PTEM_MAP( N_GAS_EMIS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SPC_PTEM_MAP', PNAME )

         ALLOCATE ( SPC_PTEM_FAC( N_GAS_EMIS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SPC_PTEM_FAC', PNAME )

         ALLOCATE ( GSPC_EMIS( N_GAS_EMIS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GSPC_EMIS', PNAME )

         ALLOCATE ( STKEM_NAME( N_SPC_PTEM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'STKEM_NAME', PNAME )

         GSPC = 'UNDEFINED'
         DO I = 1, N_SPC_PTEM
            PTEM_MAP( I )   = VC( I )%NDX
            STKEM_NAME( I ) = VC( I )%STR

            IF ( PTEM_MAP( I ) .LE. NSPC1 ) THEN
               GSPC( I ) = GC_SPC( GC_EMIS_MAP( PTEM_MAP( I ) ) ) 
            END IF
            IF ( PTEM_MAP( I ) .GT. NSPC2 .AND. PTEM_MAP( I ) .LE. NSPC3 ) THEN
               GSPC( I ) = NR_SPC( NR_EMIS_MAP( PTEM_MAP( I ) - NSPC2 ) )
            END IF
            IF ( PTEM_MAP( I ) .GT. NSPC3 ) THEN
               GSPC( I ) = TR_SPC( TR_EMIS_MAP( PTEM_MAP( I ) - NSPC3 ) )
            END IF

#ifdef verbose_ptmap
            WRITE( LOGDEV,1041 ) I, TRIM( STKEM_NAME( I ) ), PTEM_MAP( I ), TRIM( GSPC( I ) )
#endif
         END DO

         GSPC = 'UNDEFINED'
#ifdef verbose_ptmap
         WRITE( LOGDEV,* ) 'To match the number: N_GC_EMIS + N_NR_EMIS + N_TR_EMIS:',
     &                      N_GC_EMIS + N_NR_EMIS + N_TR_EMIS
         WRITE( LOGDEV,* ) 'Redefining GAS EMISSIONS to MODEL SPECIES MAP '
#endif
         L = 0; M = 0
         DO J = 1, N_GC_EMIS
            L = L + 1
            IDX = INDEX1( GC_EMIS( J ), N_SPC_PTEM, STKEM_NAME )
            IF ( IDX .GT. 0 ) THEN
               GSPC( J ) = GC_SPC( GC_EMIS_MAP( J ) )
               M = M + 1
               GSPC_EMIS( M ) = GSPC( J )
               SPC_PTEM_MAP( L ) = J
               PTEM_MAP( L ) = IDX
               SPC_PTEM_FAC( L ) = GC_EMIS_FAC( J )
#ifdef verbose_ptmap
               WRITE( LOGDEV,1045 ) L, PTEM_MAP( L ), TRIM( STKEM_NAME( IDX ) ),  
     &         SPC_PTEM_MAP( L ), TRIM( GSPC( J ) )
#endif
            ELSE
               SPC_PTEM_MAP( L ) = -1
               PTEM_MAP( L ) = -1
               WRITE( LOGDEV,1046 ) J, TRIM( GC_EMIS( J ) ), TRIM( GC_SPC( GC_EMIS_MAP( J ) ) )
            END IF
         END DO
         DO J = 1, N_NR_EMIS
            L = L + 1
            IDX = INDEX1( NR_EMIS( J ), N_SPC_PTEM, STKEM_NAME )
            IF ( IDX .GT. 0 ) THEN
               GSPC( J ) = NR_SPC( NR_EMIS_MAP( J ) )
               M = M + 1
               GSPC_EMIS( M ) = GSPC( J )
               SPC_PTEM_MAP( L ) = J + NSPC2
               PTEM_MAP( L ) = IDX
               SPC_PTEM_FAC( L ) = NR_EMIS_FAC( J )
#ifdef verbose_ptmap
               WRITE( LOGDEV,1045 ) L, PTEM_MAP( L ), TRIM( STKEM_NAME( IDX ) ),  
     &         SPC_PTEM_MAP( L ), TRIM( GSPC( J ) )
#endif
            ELSE
               SPC_PTEM_MAP( L ) = -1
               PTEM_MAP( L ) = -1
               WRITE( LOGDEV,1046 ) J, TRIM( NR_EMIS( J ) ), TRIM( NR_SPC( GC_EMIS_MAP( J ) ) )
            END IF
         END DO
         DO J = 1, N_TR_EMIS
            L = L + 1
            IDX = INDEX1( TR_EMIS( J ), N_SPC_PTEM, STKEM_NAME )
            IF ( IDX .GT. 0 ) THEN
               GSPC( J ) = TR_SPC( TR_EMIS_MAP( J ) )
               M = M + 1
               GSPC_EMIS( M ) = GSPC( J )
               SPC_PTEM_MAP( L ) = J + NSPC3
               PTEM_MAP( L ) = IDX
               SPC_PTEM_FAC( L ) = TR_EMIS_FAC( J )
#ifdef verbose_ptmap
               WRITE( LOGDEV,1045 ) L, PTEM_MAP( L ), TRIM( STKEM_NAME( IDX ) ),  
     &         SPC_PTEM_MAP( L ), TRIM( GSPC( J ) )
#endif
            ELSE
               SPC_PTEM_MAP( L ) = -1
               PTEM_MAP( L ) = -1
#ifdef verbose_ptmap
               WRITE( LOGDEV,1046 ) J, TRIM( TR_EMIS( J ) ), TRIM( TR_SPC( TR_EMIS_MAP( J ) ) )
#endif
            END IF
         END DO
         N_GSPC_EMIS = M

         IF ( ALLOCATED( V ) ) DEALLOCATE( V )
         IF ( ALLOCATED( VC ) ) DEALLOCATE( VC )

         DO N = 1, NPTGRPS
            J = 0
            DO I = 1, N_SPC_PTEM
               IDX = INDEX1( STKEM_NAME( I ), STKSPC( N )%LEN, STKSPC( N )%ARRY )
               IF ( IDX .NE. 0 ) THEN
                  J = J + 1
                  GAS_MAP( N )%ARRY( J ) = I
#ifdef verbose_ptmap
                  WRITE( LOGDEV,1031 ) N, J, GAS_MAP( N )%ARRY( J )
#endif
               END IF
            END DO
         END DO

         DO N = 1, NPTGRPS
            DO I = 1, STKEM_MAP( N )%LEN
#ifdef verbose_ptmap
               WRITE( LOGDEV,1031 ) N, I, GAS_MAP( N )%ARRY( I )
#endif
               IF ( GAS_MAP( N )%ARRY( I ) .LT. 1 ) THEN
                  WRITE( LOGDEV,* ) 'WARNING: GAS_MAP( ', N, ' )%ARRY( ', I, ' ) = 0'
               END IF
            END DO
         END DO

C point source emissions to PM species map

         ALLOCATE ( STKPM_MAP( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'STKPM_MAP', PNAME )

         ALLOCATE ( PM_MAP( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PM_MAP', PNAME )

         ALLOCATE ( STKPM_VAR( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'STKPM_VAR', PNAME )

         IF ( N_AE_EMIS .LE. 0 ) THEN
            DO N = 1, NPTGRPS
               STKPM_MAP( N )%LEN = 0
               ALLOCATE ( STKPM_MAP( N )%ARRY( 1 ), STAT=IOS )
               CALL CHECKMEM( IOS, 'STKPM_MAP', PNAME )
               STKPM_MAP( N )%ARRY = 0   ! array
            END DO
            WRITE( LOGDEV,'(/ 5X, A/ )' ) 'Not processing point source PM emissions!'
            RETURN
         END IF
            
!        WRITE( LOGDEV,1007 )

         NM = 0; L = 0
         DO N = 1, NPTGRPS  ! Loop through Point Source Files
           
            I = 0
            LOOP_PM: DO J = 1, N_EMIS_PM
               VNAME = PMEM_MAP_NAME( J )
               IDX = INDEX1( VNAME, NVARS_EMIS( N ), STKSPC( N )%ARRY )
               IF ( IDX .GT. 0 ) THEN
#ifdef verbose_ptmap
                  WRITE( LOGDEV,* ) TRIM( VNAME ), ' added to PVNM array'
#endif
                  I = I + 1
                  L = L + 1
                  PMAP( L ) = J      ! Map Global Pt Spcs Index -> AERODATA Index
                  PLST( I,N ) = IDX  ! Map Local Pt Spcs Binary -> Index in File List
                  PVNM( L ) = VNAME  ! Populate global Pt Src Spcs List Names
#ifdef verbose_ptmap
                  WRITE( LOGDEV,1009 ) N, J, I, L, PLST( I,N ), PMAP( L ),
     &                                 TRIM( PVNM( L ) ) 
#endif
               END IF
            END DO LOOP_PM

            NM = NM + I

            STKPM_MAP( N )%LEN = I
            ALLOCATE ( STKPM_MAP( N )%ARRY( I ), STAT=IOS )
            CALL CHECKMEM( IOS, 'STKPM_MAP', PNAME )
            STKPM_MAP( N )%ARRY = 0   ! array assignment

            STKPM_VAR( N )%LEN = I
            ALLOCATE ( STKPM_VAR( N )%ARRY( I ), STAT=IOS )
            CALL CHECKMEM( IOS, 'STKPM_VAR', PNAME )
            STKPM_VAR( N )%ARRY = ' '   ! array assignment

            PM_MAP( N )%LEN = I
            ALLOCATE ( PM_MAP( N )%ARRY( I ), STAT=IOS )
            CALL CHECKMEM( IOS, 'PM_MAP', PNAME )
            PM_MAP( N )%ARRY = 0   ! array assignment

         END DO   ! NPTGRPS

#ifdef verbose_ptmap
         WRITE( LOGDEV,'( / 5X, A, I3 /)' ) 'NM =', NM
#endif
         L = 0
         DO N = 1, NPTGRPS

#ifdef verbose_ptmap
             WRITE( LOGDEV,1021 ) N, STKPM_MAP( N )%LEN
#endif
            DO I = 1, STKPM_MAP( N )%LEN
               L = L + 1
               STKPM_MAP( N )%ARRY( I ) = PLST( I,N )
               STKPM_VAR( N )%ARRY( I ) = PVNM( L )
            END DO
#ifdef verbose_ptmap
            DO I = 1, STKPM_MAP( N )%LEN, 8
               I2 = MIN( I + 7, STKPM_MAP( N )%LEN )
               WRITE( LOGDEV,1025 ) N, ( STKPM_MAP( N )%ARRY( J ), J = I, I2 )
               WRITE( LOGDEV,1029 ) N, ( TRIM( STKPM_VAR( N )%ARRY( J ) ), J = I, I2 )
            END DO
#endif

            IF ( N_AE_EMIS .GT. 0 ) THEN   ! assume PM species required
C Check that the PM species names match on emissions file
               DO I = 1, N_EMIS_PM
                  K = PMEM_MAP( I )
                  IDX = INDEX1( AEROSPC( K )%EMIS, STKPM_VAR( N )%LEN, STKPM_VAR( N )%ARRY )
                  IF ( IDX .LE. 0 ) THEN
                     WRITE( LOGDEV,'(5X, A, I3)' ) TRIM( AEROSPC( K )%EMIS )
     &                                        // ' Not in Stack Emissions file', N
                  END IF
               END DO
            END IF

         END DO

         ALLOCATE( V( NM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'V', PNAME )
         ALLOCATE( VC( NM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'VC', PNAME )

         DO I = 1, NM
            V( I )%STR = PVNM( I )
            V( I )%NDX = PMAP( I )
         END DO

C Sort ...
#ifdef verbose_ptmap
         WRITE( LOGDEV,'(5X, A)' ) 'Concatenated Species List:'
         WRITE( LOGDEV,'(5X, I3, 1X, A, I3)' ) 1, V( 1 )
#endif
         DO I = 2, NM
#ifdef verbose_ptmap
            WRITE( LOGDEV,'(5X, I3, 1X, A, I3)' ) I, V( I )
#endif
            TV = V( I )
            DO J = I - 1, 1, -1
               IF ( V( J )%NDX .LE. TV%NDX ) GO TO 201
               V( J+1 ) = V( J )
            END DO
            J = 0
201         CONTINUE
            V( J+1 ) = TV
         END DO

#ifdef verbose_ptmap
         WRITE( LOGDEV,'(5X, A)' ) 'Sorted Species List:'
         DO I = 1, NM
            WRITE( LOGDEV,'(5X, A, I3)' ) V( I )
         END DO
#endif

C Cull ...
         VC( 1 ) = V( 1 )
         NCULL = 1
         DO I = 2, NM
            IF ( V( I )%STR .NE. VC( NCULL )%STR ) THEN
               NCULL = NCULL + 1
               VC( NCULL ) = V( I )
            END IF
         END DO
#ifdef verbose_ptmap
         WRITE( LOGDEV,'(5X, A, I3)' ) 'Unique PM =', NCULL
#endif
         N_SPC_PTPM = NCULL

         ALLOCATE ( PTPM_MAP( N_SPC_PTPM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PTPM_MAP', PNAME )

         ALLOCATE ( STKPM_NAME( N_SPC_PTPM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'STKPM_NAME', PNAME )

         DO I = 1, N_SPC_PTPM
            PTPM_MAP( I ) = VC( I )%NDX
            STKPM_NAME( I ) = VC( I )%STR
#ifdef verbose_ptmap
            WRITE( LOGDEV,1043 ) I, TRIM( STKPM_NAME( I ) ), PTPM_MAP( I )
#endif
         END DO

         DO N = 1, NPTGRPS
            J = 0
            DO I = 1, N_SPC_PTPM
               IDX = INDEX1( STKPM_NAME( I ), STKSPC( N )%LEN, STKSPC( N )%ARRY )
               IF ( IDX .NE. 0 ) THEN
                  J = J + 1
                  PM_MAP( N )%ARRY( J ) = I
#ifdef verbose_ptmap
                  WRITE( LOGDEV,1033 ) N, J, PM_MAP( N )%ARRY( J )
#endif
               END IF
            END DO
#ifdef verbose_ptmap
            WRITE( LOGDEV,'(5X, A, I4)' ) 'J =', J
#endif
         END DO

C build array listing aerosol diagnostic names

         ALLOCATE ( ASPC_EMIS( N_EMIS_PM ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ASPC_EMIS', PNAME )
         ASPC_EMIS = ' '

         N_ASPC_EMIS = 0
         DO N = 1, N_EMIS_PM
            L = PMEM_MAP( N )
            IF ( L .LT. 1 ) CYCLE
            DO J = 1, N_MODE
               IF ( AERO_MISSING( L,J ) ) CYCLE
               NM = LEN_TRIM( AEROSPC( L )%NAME( J )  ) - 1
               VNAME = AEROSPC( L )%NAME( J )( 1:NM )
C check name to prevent redundancy
               IDX = INDEX1( VNAME, N_EMIS_PM, ASPC_EMIS )
               IF ( IDX .GT. 0 ) EXIT
               N_ASPC_EMIS = N_ASPC_EMIS + 1               
               ASPC_EMIS( N_ASPC_EMIS ) = VNAME
               EXIT
            END DO
         END DO

         IF ( ALLOCATED( GMAP ) ) DEALLOCATE( GMAP )
         IF ( ALLOCATED( GLST ) ) DEALLOCATE( GLST )
         IF ( ALLOCATED( GVNM ) ) DEALLOCATE( GVNM )
         IF ( ALLOCATED( GSPC ) ) DEALLOCATE( GSPC )
         IF ( ALLOCATED( V ) )    DEALLOCATE( V )
         IF ( ALLOCATED( VC ) )   DEALLOCATE( VC )

         IF ( ALLOCATED( STKEM_NAME ) ) DEALLOCATE( STKEM_NAME ) 

         RETURN

1005     FORMAT( / 5X, 'N  J  K  I  L ', 4X, 'GVNM', 15X, 'GLST', 2X, 'GMAP' )
1007     FORMAT( 5X, 'N  J  K  I  L ', 4X, 'PVNM', 15X, 'PLST', 2X, 'PMAP' )
1009     FORMAT( 3X, 6I3, 3X, A, 3X, A )
1010     FORMAT( 3X, 6I3, 3X, A )
1011     FORMAT( 5X, 'STKEM_MAP(', I2, ' )%LEN:', 1X, I3 )
1015     FORMAT( 5X, 'STKEM_MAP(', I2, ' )%ARRY:', 1X, 8I4 )
1019     FORMAT( 5X, 'STKEM_VAR(', I2, ' )%ARRY:', 1X, 8(:A, 1X) )
1021     FORMAT( 5X, 'STKPM_MAP(', I2, ' )%LEN:', 1X, I3 )
1025     FORMAT( 5X, 'STKPM_MAP(', I2, ' )%ARRY:', 1X, 8(:I4) )
1029     FORMAT( 5X, 'STKPM_VAR(', I2, ' )%ARRY:', 1X, 8(:A, 1X) )
1031     FORMAT( 5X, 'GAS_MAP(', I2, ' )%ARRY(', I3, ' ) =', I3 )
1033     FORMAT( 5X, 'PM_MAP(', I2, ' )%ARRY(', I3, ' ) =', I3 )
1041     FORMAT( 5X, I3, 1X,'STKEM_NAME, PTEM_MAP = ', A, I3, ' Model Species ', A )
1043     FORMAT( 5X, I3, 1X,'STKPM_NAME, PTPM_MAP = ', A, I3 )
1045     FORMAT( I3, 2X, 'PTEM_MAP =', I3, ' STKEM_NAME ', A, ' SPC_PTEM_MAP =',
     &           I3, ' into Model Species ', A )
1046     FORMAT( 'WARNING:', I4, ' No Point Emissions of ', A, ' for Species ', A )
         END FUNCTION PTMAP_INIT

      END MODULE PTMAP

