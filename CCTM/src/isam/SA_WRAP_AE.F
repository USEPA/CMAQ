
      SUBROUTINE SA_WRAP_AE( CGRID0, CGRID, JDATE, JTIME, TSTEP )

!*************************************************************
!20140428 As a wrapper to apportion the tags with bulk conc after AERO process
!
!     Called by sciproc.F

! Revision History:
!
!   13 May 19 David Wong: Implemented centralized I/O approach
!*************************************************************
      USE GRID_CONF
      USE CGRID_SPCS
      USE SA_DEFN
      USE UTILIO_DEFN
      USE AERO_BUDGET
      USE AERO_DATA, ONLY : AERO_MISSING, AEROSPC_MAP, N_MODE,
     &                      N_AEROSPC, AEROSPC, AEROSPC_MWINV,
     &                      AEROSPC_MW
      USE CENTRALIZED_IO_MODULE, only : interpolate_var

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID
      INCLUDE SUBST_CONST

! Arguments
      REAL, POINTER             :: CGRID( :,:,:,: )
      REAL                      :: CGRID0( :,:,:,: )
      INTEGER, INTENT( IN )     :: JDATE
      INTEGER, INTENT( IN )     :: JTIME
      INTEGER, INTENT( IN )     :: TSTEP( 3 )

! Scratch
      CHARACTER( 16 ), SAVE :: PNAME = 'SA_WRAP_AE'
      REAL, ALLOCATABLE, SAVE :: BULK0( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: CCOND( :,:,:,: )

! for interpx
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      REAL DENS ( NCOLS,NROWS,NLAYS )
      INTEGER MDATE, MTIME, MSTEP
      CHARACTER( 96 ) :: XMSG = ' '

! ppm2mole conversion
      REAL ppm2mole( NCOLS,NROWS,NLAYS )

! Identify ISAM species indices 20130529
      INTEGER :: ISA1, ISA2, ICG1, ICG2, IAER, IM, K, C, R, L
      INTEGER :: JGAS, CGAS, JCTR, CCTR, JAER
      LOGICAL :: OMH2O

! Arrays for combined ammonium, combined nitrates, and combined sulfates
      REAL, ALLOCATABLE, SAVE :: TOTAER_TAG (:,:,:)

      REAL, ALLOCATABLE, SAVE :: TOTAER(:,:,:)
      REAL, ALLOCATABLE, SAVE :: GASPM_BULK0(:,:,:)
      REAL, ALLOCATABLE, SAVE :: GASPM_TAG(:,:,:)
      REAL, ALLOCATABLE, SAVE :: AERO_TAG(:,:,:)
      REAL, ALLOCATABLE, SAVE :: AERO_BULK(:,:,:)
      REAL, ALLOCATABLE, SAVE :: BULKFRAC(:,:,:,:)
      REAL, ALLOCATABLE, SAVE :: BULKCOND(:,:,:)
      REAL, ALLOCATABLE, SAVE :: BULKH2O(:,:,:)
      REAL, ALLOCATABLE, SAVE :: FRAC1(:,:,:)
      REAL, ALLOCATABLE, SAVE :: FRAC2(:,:,:)

      REAL, ALLOCATABLE, SAVE :: ISAM0(:,:,:,:,:)
      REAL, ALLOCATABLE, SAVE :: ISAM1(:,:,:,:,:)

      LOGICAL, ALLOCATABLE, SAVE :: L_MASK_AERO( : ), L_MASK_OM( : )
      CHARACTER( 3 ), ALLOCATABLE, SAVE :: L_MASK_TYPE( : )
      INTEGER, ALLOCATABLE, SAVE :: L_MASK_IM( : )

      REAL, PARAMETER :: MIN_VAL = 1.0E-25
      REAL, SAVE :: MIN_TAGTOT 

      INTEGER ALLOCSTAT
!-------------------------------------------------------------------


Ckrt Identify species index in ISAM array
      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.

        MIN_TAGTOT = MIN_VAL * NTAG_SA
  
        ALLOCATE( CCOND( NCOLS,NROWS,NLAYS,NSPCSD ),
     &     BULK0( NCOLS,NROWS,NLAYS,NSPC_SA ),
     &     GASPM_BULK0( NCOLS,NROWS,NLAYS ),
     &     GASPM_TAG( NCOLS,NROWS,NLAYS ),
     &     STAT = ALLOCSTAT ) 
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating BULK0, SULFCOND, SULFCOND_AIT, '
     &          //'or SULFCOND_ACC'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE( TOTAER_TAG  ( NCOLS,NROWS,NLAYS ),
     &     AERO_TAG( NCOLS,NROWS,NLAYS ),
     &     AERO_BULK( NCOLS,NROWS,NLAYS ),
     &     BULKFRAC( NCOLS,NROWS,NLAYS,N_MODE ),
     &     BULKCOND( NCOLS,NROWS,NLAYS ),
     &     BULKH2O( NCOLS,NROWS,NLAYS ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating AMMONIA_TAG, NITRATE_TAG, '
     &          //'SULFATE_TAG, or TOTAER_TAG'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE( TOTAER( NCOLS,NROWS,NLAYS ),
     &     FRAC1( NCOLS,NROWS,NLAYS ),
     &     FRAC2( NCOLS,NROWS,NLAYS ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating TOTNO3, TOTSO4, TOTAER, '
     &          //'FRAC1, or FRAC2'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE( ISAM0( NCOLS,NROWS,NLAYS,NSPC_SA,NTAG_SA ),
     &     ISAM1( NCOLS,NROWS,NLAYS,NSPC_SA,NTAG_SA ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ISAM0 or ISAM1 '
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ! Store Masks for Aerosol features in vectors of length NSPC_SA
        ALLOCATE( L_MASK_AERO( NSPC_SA ), L_MASK_TYPE( NSPC_SA ), 
     &            L_MASK_OM( NSPC_SA )  , L_MASK_IM( NSPC_SA ) )
        L_MASK_AERO = .FALSE.
        L_MASK_TYPE = ''
        L_MASK_OM   = .FALSE.
        L_MASK_IM   = 0
        DO IAER = 1,N_AEROSPC
        DO IM = 1,N_MODE
           JAER = MAP_AEROtoSA( IAER,IM ) 
           IF ( JAER  .NE. 0 ) THEN 
              L_MASK_AERO( JAER ) = .TRUE.
              L_MASK_TYPE( JAER ) = AEROSPC( IAER )%VOLTYPE
              L_MASK_OM  ( JAER ) = AEROSPC( IAER )%OM
              L_MASK_IM  ( JAER ) = IM
           END IF
        END DO
        END DO   

      ENDIF ! firstime ?

      ! Extract Density for gas conversion to umol/m3
      MDATE = JDATE
      MTIME = JTIME
      MSTEP = TIME2SEC( TSTEP( 2 ) )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP/2 ) )

      ! Get Air Density in kg/m3
      call interpolate_var ('DENS', MDATE, MTIME, DENS)

      ! Conversion factor to move gases from ppmv to umol/m3
      ppm2mole( :,:,: ) = MAX( DENS( :,:,: ) * 1000. / MWAIR, 1e-10 )
      
      ! Sum up pre-process tags for each species at each grid cell
      CGRID0 = MAX( CGRID0, MIN_TAGTOT )
      ISAM0(:,:,:,:,:) = ISAM(:,:,:,:,:)
      ISAM1(:,:,:,:,:) = ISAM(:,:,:,:,:)
      BULK0( :,:,:,: ) = SUM( ISAM( :,:,:,:,: ),5 )

      CCOND = MAX( CGRID0 + AERO_COND + AERO_NPF, MIN_TAGTOT )

 
c  !------------------------!
c  !>>>>> CONDENSATION <<<<<!
      DO IAER = 1,N_AEROSPC
          IF ( .NOT.L_ISAM_AERO( IAER ) ) CYCLE
          
          ! Reversible Partitioning
          IF ( AEROSPC( IAER )%VOLTYPE .EQ. 'REV' ) THEN
             ! Load Indices for Vapor-Phase Species and Reaction
             ! Counter, if present

             !!! WATER MAY POSE A PROBLEM HERE - IT'S REV but it doesn't
             !have a gas-phase species !!!
             JGAS = MAP_AEROGAStoSA( IAER )
             CGAS = MAP_SAtoCGR( JGAS )
             JCTR = MAP_AEROCTRtoSA( IAER )
             CCTR = 0
             IF ( JCTR .GT. 0 ) CCTR = MAP_SAtoCGR( JCTR )

             ! Sum up Total Gas plus particle initially
             GASPM_BULK0 = 0.0
             DO IM = 1,N_MODE
                IF ( MAP_AEROtoSA( IAER,IM ) .NE. 0 ) 
     &             GASPM_BULK0 = GASPM_BULK0 
     &                 + BULK0( :,:,:,MAP_AEROtoSA( IAER,IM )) * AEROSPC_MWINV( IAER )
             END DO
             GASPM_BULK0 = BULK0( :,:,:,JGAS ) * ppm2mole + GASPM_BULK0( :,:,: )  ! umol m-3
             IF ( JCTR .GT. 0 ) 
     &          GASPM_BULK0 = GASPM_BULK0 + BULK0( :,:,:,JCTR ) * ppm2mole * AEROSPC( IAER )%CTR_YIELD

             ! Find bulk fraction for condensable species and
             DO K = 1,NTAG_SA
                AERO_TAG = 0.0
                DO IM = 1,N_MODE
                   IF ( MAP_AEROtoSA( IAER,IM ) .NE. 0 ) 
     &                AERO_TAG( :,:,: ) = AERO_TAG( :,:,: ) +
     &                    ISAM0( :,:,:,MAP_AEROtoSA( IAER,IM ),K )
                END DO
                AERO_TAG( :,:,: ) = AERO_TAG( :,:,: ) * AEROSPC_MWINV( IAER ) ! umol m-3

                GASPM_TAG = 0.0
                
                ! Add condensation from reaction counter, if applicable
                IF ( JCTR .GT. 0 ) 
     &               GASPM_TAG( :,:,: ) = ISAM0( :,:,:,JCTR,K ) * ppm2mole * AEROSPC(IAER)%CTR_YIELD

                ! Add condensable vapor and aerosol species
                GASPM_TAG( :,:,: ) = ( GASPM_TAG + ISAM0( :,:,:,JGAS,K ) * ppm2mole 
     &                                +AERO_TAG( :,:,: )  ) /                           ! umol m-3 /
     &                                MAX( GASPM_BULK0, MIN_TAGTOT*(REAL(N_MODE)+1.0) ) ! umol m-3

                ! Propagate source apportionment through aerosol condensation changes
                IF ( JCTR .GT. 0 ) ISAM1( :,:,:,JCTR,K ) = MIN_VAL

                ISAM1( :,:,:,JGAS,K ) = GASPM_TAG( :,:,: ) * CCOND( :,:,:,CGAS ) ! ppmv
                DO IM = 1,N_MODE
                   IF ( MAP_AEROtoSA( IAER,IM ) .NE. 0 ) 
     &                ISAM1( :,:,:,MAP_AEROtoSA( IAER,IM ),K ) = 
     &                    GASPM_TAG( :,:,: ) * CCOND( :,:,:,AEROSPC_MAP( IAER,IM ) ) ! ug m-3
                END DO
             END DO

          ! Irreversible Partitioning
          ELSE IF ( AEROSPC( IAER )%VOLTYPE .EQ. 'IRV' ) THEN
             JCTR = MAP_AEROCTRtoSA( IAER )

             ! Find fractional condensation of mass to each aerosol mode
             BULKFRAC = 0.0
             DO IM = 1,N_MODE
                IF ( MAP_AEROtoSA( IAER,IM ) .NE. 0 ) 
     &             BULKFRAC( :,:,:,IM ) = BULKFRAC( :,:,:,IM ) 
     &                              + AERO_COND( :,:,:,AEROSPC_MAP( IAER,IM ) )
     &                              + AERO_NPF ( :,:,:,AEROSPC_MAP( IAER,IM ) )
             END DO
             BULKFRAC = MAX( BULKFRAC, MIN_VAL )
             BULKCOND = SUM( BULKFRAC,4 )
             DO IM = 1,N_MODE
                BULKFRAC( :,:,:,IM ) = BULKFRAC( :,:,:,IM ) / BULKCOND
             END DO

             ! Apportion mass based on contributions from sources to
             ! condensable vapors
             DO K = 1,NTAG_SA
               DO IM = 1,N_MODE
                 IF ( MAP_AEROtoSA( IAER,IM ) .NE. 0 ) 
     &              ISAM1( :,:,:,MAP_AEROtoSA( IAER,IM ),K ) = 
     &                    ISAM0( :,:,:,MAP_AEROtoSA( IAER,IM ),K )
     &                  + ISAM0( :,:,:,JCTR,K )
     &                     * BULKFRAC( :,:,:,IM ) * ppm2mole * AEROSPC_MW( IAER )
               END DO
                 ISAM1( :,:,:,JCTR,K ) = MIN_VAL
             END DO

             ! Let low-volatility vapor species apportionment equal that
             ! of the particle-phase mass
             IF ( MAP_AEROGAStoSA( IAER ) .GT. 0 ) THEN
                AERO_BULK = 0.0
                DO IM = 1,N_MODE
                   IF ( MAP_AEROtoSA( IAER,IM ) .NE. 0 ) 
     &                AERO_BULK = AERO_BULK 
     &                   + SUM( ISAM1( :,:,:,MAP_AEROtoSA( IAER,IM ),: ),4 )
                END DO
                AERO_BULK = MAX( AERO_BULK, MIN_TAGTOT * REAL( N_MODE ) )

                JGAS = MAP_AEROGAStoSA( IAER )
                CGAS = MAP_SAtoCGR( JGAS )
                DO K = 1,NTAG_SA
                   FRAC1 = 0.0
                   DO IM = 1,N_MODE
                      IF ( MAP_AEROtoSA( IAER,IM ) ) 
     &                   FRAC1( :,:,: ) = FRAC1( :,:,: ) + ISAM1( :,:,:,MAP_AEROtoSA( IAER,IM ),K )
                   END DO
                   ISAM1( :,:,:,JGAS,K ) = FRAC1 * CGRID( :,:,:,CGAS ) / AERO_BULK(:,:,:)
                END DO
             END IF

          ! Water Species
          ELSE IF ( AEROSPC( IAER )%VOLTYPE .EQ. 'H2O' ) THEN
             ! Water Vapor Species is not explicitly tracked so special
             ! care must be taken for this species.
             DO IM = 1,N_MODE
                JAER = MAP_AEROtoSA( IAER,IM )
                BULKH2O = 0.0
                IF ( JAER .NE. 0 ) THEN
                   BULKH2O( :,:,: ) = AERO_COND( :,:,:,AEROSPC_MAP( IAER,IM ) )
     &                              + AERO_NPF ( :,:,:,AEROSPC_MAP( IAER,IM ) )
                   OMH2O = .FALSE.
                   IF ( ISAM_SPEC( JAER,1 )(1:4) .EQ. 'AORG' ) OMH2O = .TRUE.
                END IF

                ! Assume that net losses pull proportionally from water species
                FORALL ( C=1:NCOLS, R=1:NROWS, L=1:NLAYS, BULKH2O( C,R,L ) .LT. 0.0 )
                   AERO_BULK( C,R,L ) = MAX( SUM( ISAM0( C,R,L,JAER,: ) ), MIN_TAGTOT )
                   ISAM1( C,R,L,JAER,: ) = ISAM0( C,R,L,JAER,: ) *
     &                     ( 1.0 + 1.0/AERO_BULK( C,R,L ) * BULKH2O( C,R,L ) )
                END FORALL

                ! Assume that net gains are apportioned like current non-water aerosol
                FORALL ( C=1:NCOLS, R=1:NROWS, L=1:NLAYS, BULKH2O( C,R,L ) .GT. 0.0 )
                   ! Determine the total non-water mass separating
                   ! inorganic and organic
                   AERO_BULK( C,R,L ) = MAX( SUM( SUM( ISAM0( C,R,L,:,: ), DIM=2),
     &                    MASK = (L_MASK_AERO .EQ. .TRUE. .AND. 
     &                            L_MASK_TYPE .NE. 'H2O'  .AND.
     &                            OMH2O .EQ. L_MASK_OM    .AND.
     &                            L_MASK_IM . EQ. IM  ) ), MIN_TAGTOT )
                   ISAM1( C,R,L,JAER,: ) = ISAM0( C,R,L,JAER,: ) +
     &                 SUM( SUM( ISAM0( C,R,L,:,: ),DIM=2),
     &                     MASK = (L_MASK_AERO .EQ. .TRUE. .AND. 
     &                             L_MASK_TYPE .NE. 'H2O'  .AND.
     &                             OMH2O .EQ. L_MASK_OM    .AND. 
     &                             L_MASK_IM . EQ. IM  ) )
     &                     /AERO_BULK( C,R,L ) * BULKH2O( C,R,L )
                END FORALL
             END DO
          ! Nonvolatile Species (VOLTYPE = 'NVL')
          ELSE
             ! Nothing to be done for Nonvolatile Species
          END IF
      END DO

c  !-----------------------!
c  !>>>>> COAGULATION <<<<<!
      ! Propagate source apportionment changes through coagulation and
      ! growth processes
      DO IAER = 1,N_AEROSPC
          IF ( AERO_MISSING( IAER,1 ) ) CYCLE

          ! Account for Loss of Aitken Mode from Coagulation and Growth
          ! Move source contribution from Aitken mode to Accumulation
          ! mode
          ISA1 = MAP_AEROtoSA( IAER,1 )  ! Index of source particles (1=Aitken)
          IF ( ISA1 .EQ. 0 ) CYCLE

          ISA2 = MAP_AEROtoSA( IAER,2 )  ! Index of destination particles (2=Accumulation)
          ICG1 = AEROSPC_MAP( IAER,1 )   ! Index of source in aerosol process analysis array
          ICG2 = AEROSPC_MAP( IAER,2 )   ! Index of destination in aerosol process analysis array

          ! Get Current Partitioning of Mass Between Source and
          ! Destination Modes
          TOTAER( :,:,: )  = CGRID( :,:,:,ICG1 ) + CGRID( :,:,:,ICG2 )
          FRAC1( :,:,: ) = CGRID( :,:,:,ICG1 ) / MAX( TOTAER( :,:,: ), MIN_TAGTOT )
          FRAC2( :,:,: ) = 1.0 - FRAC1( :,:,: )

          DO K = 1,NTAG_SA
             ! Sum Total Aerosols Among Relevant Modes for this Tag
             TOTAER_TAG( :,:,: ) = ISAM1( :,:,:,ISA1,K ) + ISAM1( :,:,:,ISA2,K )

             ! Calculate New Tag Concentrations
             ISAM1( :,:,:,ISA1,K ) = TOTAER_TAG * FRAC1
             ISAM1( :,:,:,ISA2,K ) = TOTAER_TAG * FRAC2

          END DO
      END DO

      ! Update global ISAM array with modified attribution array ISAM1
      ISAM = ISAM1

      END 

