
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
      REAL(8), ALLOCATABLE, SAVE :: BULK0( :,:,:,: )
      REAL(8), ALLOCATABLE, SAVE :: CCOND( :,:,:,: )

! for interpx
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      REAL DENS ( NCOLS,NROWS,NLAYS )
      INTEGER MDATE, MTIME, MSTEP
      CHARACTER( 96 ) :: XMSG = ' '

! ppm2mole conversion
      REAL(8) ppm2mole( NCOLS,NROWS,NLAYS )

! Identify ISAM species indices 20130529
      INTEGER :: ISA1, ISA2, ICG1, ICG2, IAER, IM, K, C, R, L
      INTEGER :: JGAS, CGAS, JCTR, CCTR, JAER( N_MODE ), CAER( N_MODE )
      LOGICAL :: OMH2O

! Arrays for combined ammonium, combined nitrates, and combined sulfates

      REAL(8), ALLOCATABLE, SAVE :: SPEC_BULK0(:,:,:)
      REAL(8), ALLOCATABLE, SAVE :: TAG_FRAC(:,:,:)
      REAL(8), ALLOCATABLE, SAVE :: TAG_FRAC2(:,:,:)
      REAL(8), ALLOCATABLE, SAVE :: BULK1(:,:,:)
      REAL(8), ALLOCATABLE, SAVE :: BULK_TRANS_SRC(:,:,:)

      REAL(8), ALLOCATABLE, SAVE :: ISAM0(:,:,:,:,:)
      REAL(8), ALLOCATABLE, SAVE :: ISAM1(:,:,:,:,:)

      LOGICAL, ALLOCATABLE, SAVE :: L_MASK_AERO( : ), L_MASK_OM( : )
      CHARACTER( 3 ), ALLOCATABLE, SAVE :: L_MASK_TYPE( : )
      INTEGER, ALLOCATABLE, SAVE :: L_MASK_IM( : )

      REAL(8), PARAMETER :: MIN_VAL = 1.0E-25
      REAL(8), SAVE :: MIN_TAGTOT 

      INTEGER ALLOCSTAT
!-------------------------------------------------------------------


Ckrt Identify species index in ISAM array
      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.

        MIN_TAGTOT = MIN_VAL * NTAG_SA
  
        ALLOCATE( 
     &     CCOND( NCOLS,NROWS,NLAYS,NSPCSD ),
     &     SPEC_BULK0( NCOLS,NROWS,NLAYS ),
     &     BULK0( NCOLS,NROWS,NLAYS,NSPC_SA ),
     &     BULK1( NCOLS,NROWS,NLAYS ),
     &     BULK_TRANS_SRC( NCOLS,NROWS,NLAYS ),
     &     TAG_FRAC( NCOLS,NROWS,NLAYS ),
     &     TAG_FRAC2( NCOLS,NROWS,NLAYS ),
     &     ISAM0( NCOLS,NROWS,NLAYS,NSPC_SA,NTAG_SA ),
     &     ISAM1( NCOLS,NROWS,NLAYS,NSPC_SA,NTAG_SA ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating CCOND, SPEC_BULK0, '
     &          //'BULK0, BULK1, BULK_TRANS_SRC, TAG_FRAC, '
     &          //'ISAM0, or ISAM1.'
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
           JAER( : ) = MAP_AEROtoSA( IAER,: ) 
           DO IM = 1,N_MODE
              IF ( JAER(IM)  .NE. 0 ) THEN 
                 L_MASK_AERO( JAER(IM) ) = .TRUE.
                 L_MASK_TYPE( JAER(IM) ) = AEROSPC( IAER )%VOLTYPE
                 L_MASK_OM  ( JAER(IM) ) = AEROSPC( IAER )%OM
                 L_MASK_IM  ( JAER(IM) ) = IM
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
      ppm2mole( :,:,: ) = DBLE( MAX( DENS( :,:,: ) * 1000. / MWAIR, 1e-10 ) )
      
      ! Sum up pre-process tags for each species at each grid cell
      ISAM0(:,:,:,:,:) = MAX( DBLE( ISAM(:,:,:,:,:) ), MIN_VAL )
      ISAM1(:,:,:,:,:) = ISAM0(:,:,:,:,:)
      BULK0( :,:,:,: ) = SUM( ISAM0( :,:,:,:,: ),5 )

      CCOND = DBLE( CGRID0 + AERO_COND + AERO_NPF )

 
      !------------------------!
      !>>>>> CONDENSATION <<<<<!
      DO IAER = 1,N_AEROSPC
          IF ( .NOT.L_ISAM_AERO( IAER ) ) CYCLE
          JAER( : ) = MAP_AEROtoSA( IAER,: )
          CAER( : ) = AEROSPC_MAP( IAER,: )

          ! Reversible Partitioning
          IF ( AEROSPC( IAER )%VOLTYPE .EQ. 'REV' ) THEN
             ! Load Indices for Vapor-Phase Species and Reaction
             ! Counter, if present
             JGAS = MAP_AEROGAStoSA( IAER )
             CGAS = MAP_SAtoCGR( JGAS )
             JCTR = MAP_AEROCTRtoSA( IAER )
             CCTR = 0
             IF ( JCTR .GT. 0 ) CCTR = MAP_SAtoCGR( JCTR )

             ! First Move Apportionment to/from the coarse mode
             ! explicitly, if it exists.
             IF ( JAER(3) .GT. 0 ) THEN
                DO K = 1,NTAG_SA
                   TAG_FRAC( :,:,: ) = ISAM0( :,:,:,JGAS,K ) / BULK0( :,:,:,JGAS )
     &                                * DBLE( AERO_COND( :,:,:,CAER(3) ) )
                   TAG_FRAC2( :,:,: )= ISAM0( :,:,:,JAER(3),K ) / BULK0( :,:,:,JAER(3) )
     &                                * DBLE( AERO_COND( :,:,:,CAER(3) ) * -1.0 )
                   DO C = 1,NCOLS
                   DO R = 1,NROWS
                   DO L = 1,NLAYS
                     IF ( AERO_COND( C,R,L,CAER(3) ) .GT. 0.0 ) THEN 
                        ! Condensation - Gas-phase drives attribution
                        TAG_FRAC( C,R,L ) = MIN( TAG_FRAC( C,R,L ), 
     &                                           ISAM1( C,R,L,JGAS,K )-MIN_VAL )
                        ISAM1( C,R,L,JAER(3),K ) = 
     &                         ISAM1( C,R,L,JAER(3),K ) + TAG_FRAC( C,R,L ) 
                        ISAM1( C,R,L,JGAS,K ) = 
     &                         ISAM1( C,R,L,JGAS,K ) - TAG_FRAC( C,R,L )
                     ELSE   
                        ! Evaporation - Aerosol drives attribution
                        TAG_FRAC2( C,R,L ) = MIN( TAG_FRAC2( C,R,L ), 
     &                                           ISAM1( C,R,L,JAER(3),K )-MIN_VAL )
                        ISAM1( C,R,L,JAER(3),K ) = 
     &                         ISAM1( C,R,L,JAER(3),K ) - TAG_FRAC2( C,R,L )
                        ISAM1( C,R,L,JGAS,K ) = 
     &                         ISAM1( C,R,L,JGAS,K ) + TAG_FRAC2( C,R,L )
                     END IF
                   END DO
                   END DO
                   END DO
                END DO
                BULK0( :,:,:,JGAS ) = SUM( ISAM1(:,:,:,JGAS,:),4 )
             END IF

             ! Then apply apportionment uniformly across the gas and 
             ! fine modes, if they exist since they are at equilibrium.

             ! Sum up Initial Bulk Gas plus Bulk Particle
             SPEC_BULK0 = 0.0
             DO IM = 1,2
                IF ( JAER(IM) .GT. 0 ) THEN
                   SPEC_BULK0 = SPEC_BULK0 
     &                  + BULK0( :,:,:,JAER(IM)) 
                END IF
             END DO
             SPEC_BULK0( :,:,: ) = SPEC_BULK0( :,:,: ) * DBLE( AEROSPC_MWINV( IAER ) ) ! umol m-3
             
             ! Add mass from condensable vapor
             SPEC_BULK0 = BULK0( :,:,:,JGAS ) * ppm2mole + SPEC_BULK0( :,:,: )  ! umol m-3
             
             ! Add condensation from reaction counter, if applicable
             IF ( JCTR .GT. 0 ) 
     &          SPEC_BULK0 = SPEC_BULK0 
     &             +BULK0( :,:,:,JCTR ) * ppm2mole 
     &              * DBLE( AEROSPC( IAER )%CTR_YIELD 
     &                     *CGRID_MW( CCTR ) / AEROSPC_MW( IAER ) ) ! umol m-3

             ! For each tag, find fraction of bulk condensable species in gas and 
             ! particle phases and apply that fraction to the species in all phases 
             ! for this tag.
             DO K = 1,NTAG_SA
                ! Add Aerosol Species for each mode if they exist
                TAG_FRAC = 0.0
                DO IM = 1,2
                   IF ( JAER(IM) .GT. 0 ) THEN
                      TAG_FRAC( :,:,: ) = TAG_FRAC( :,:,: ) +
     &                     ISAM0( :,:,:,JAER(IM),K )
                   END IF
                END DO
                TAG_FRAC( :,:,: ) = TAG_FRAC * DBLE( AEROSPC_MWINV( IAER ) ) ! umol m-3

                ! Add condensable vapor - use ISAM1 here instead of
                ! ISAM0 in case gas has been updated in the coarse mode
                ! section.
                TAG_FRAC( :,:,: ) = TAG_FRAC + ISAM1( :,:,:,JGAS,K ) * ppm2mole  ! umol m-3 /
                
                ! Add condensation from reaction counter, if applicable
                IF ( JCTR .GT. 0 ) 
     &               TAG_FRAC( :,:,: ) = TAG_FRAC 
     &                      +ISAM0( :,:,:,JCTR,K ) * ppm2mole 
     &                       * DBLE( AEROSPC(IAER)%CTR_YIELD
     &                              *CGRID_MW( CCTR ) / AEROSPC_MW( IAER ) ) ! umol m-3

                ! Calculate Fraction of source k in bulk
                TAG_FRAC = TAG_FRAC / SPEC_BULK0 ! umol m-3

                ! Propagate source apportionment based on aerosol condensation changes to 
                ! semivolatile vapor and aerosol.
                DO IM = 1,2
                   IF ( JAER(IM) .NE. 0 ) 
     &                ISAM1( :,:,:,JAER(IM),K ) = 
     &                    TAG_FRAC( :,:,: ) * CCOND( :,:,:,CAER(IM) ) ! ug m-3
                END DO
                
                ! Caclulate Apportionment for Condensable Vapor
                ISAM1( :,:,:,JGAS,K ) = TAG_FRAC( :,:,: ) * CGRID( :,:,:,CGAS ) ! ppmv
                
                ! Reset Reaction Counter if it is defined
                IF ( JCTR .GT. 0 ) ISAM1( :,:,:,JCTR,K ) = MIN_VAL

             END DO

          ! Irreversible Partitioning
          ELSE IF ( AEROSPC( IAER )%VOLTYPE .EQ. 'IRV' ) THEN
             ! Retrieve index of reaction counter relevant for this
             ! irreversible partioning species. This index is in terms
             ! of the location of the counter in the ISAM array. Also
             ! retrieve low-volaitlity vapor index.
             JCTR = MAP_AEROCTRtoSA( IAER )
             JGAS = MAP_AEROGAStoSA( IAER )

             DO K = 1,NTAG_SA
               ! Find Source Apportionment of the low volatility vapor species
               ! or, if it doesn't exist, the reaction counter
               IF ( JGAS .GT. 0 ) THEN
                  TAG_FRAC( :,:,: ) = ISAM0( :,:,:,JGAS,K ) / BULK0( :,:,:,JGAS )
               ELSE  IF ( JCTR .GT. 0 ) THEN
                  TAG_FRAC( :,:,: ) = ISAM0( :,:,:,JCTR,K ) / BULK0( :,:,:,JCTR )
               ELSE
                  XMSG = 'Either a Low-Volatility Gas or a Reaction Counter ' //
     &                   'must be defined for Irreversible Aerosol Species '// 
     &                   'source apporitonment. Please check the AERO_DATA '//
     &                   'aerosol properties table (gasname, ctrname).'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               ! Apportion condensed mass based on gas apporitonment
               DO IM = 1,N_MODE
                  IF ( JAER(IM) .NE. 0 ) 
     &               ISAM1( :,:,:,JAER(IM),K ) = 
     &                    ISAM0( :,:,:,JAER(IM),K )
     &                  + DBLE( AERO_COND( :,:,:,CAER(IM) )
     &                         +AERO_NPF ( :,:,:,CAER(IM) ) )
     &                    * TAG_FRAC( :,:,: )              
               END DO

               ! Apportion the change in the low volaitlity vapor
               IF ( JGAS .GT. 0 ) THEN
                  CGAS = MAP_SAtoCGR( JGAS )
                  ISAM1( :,:,:,JGAS,K ) = CGRID( :,:,:,CGAS ) * TAG_FRAC( :,:,: )
               END IF

               ! Reset the reaction counter
               IF ( JCTR .GT. 0 ) ISAM1( :,:,:,JCTR,K ) = MIN_VAL

             END DO

          ! Water Species
          ELSE IF ( AEROSPC( IAER )%VOLTYPE .EQ. 'H2O' ) THEN
             ! Water Vapor Species is not explicitly tracked so special
             ! care must be taken for this species.
             DO IM = 1,N_MODE
                BULK_TRANS_SRC = 0.0
                IF ( JAER(IM) .NE. 0 ) THEN
                   BULK_TRANS_SRC( :,:,: ) = AERO_COND( :,:,:,CAER(IM) )
     &                              + AERO_NPF ( :,:,:,CAER(IM) )
                   OMH2O = .FALSE.
                   IF ( ISAM_SPEC( JAER(IM),1 )(1:4) .EQ. 'AORG' ) OMH2O = .TRUE.
                END IF

                DO C = 1,NCOLS
                DO R = 1,NROWS
                DO L = 1,NLAYS
                   IF ( BULK_TRANS_SRC( C,R,L ) .LT. 0.0 ) THEN
                     ! Assume that net losses pull proportionally from water species
                     IF ( JAER(IM) .NE. 0 ) THEN
                       SPEC_BULK0( C,R,L ) = SUM( ISAM0( C,R,L,JAER(IM),: ) )
                       ISAM1( C,R,L,JAER(IM),: ) = ISAM0( C,R,L,JAER(IM),: ) *
     &                       ( 1.0 + BULK_TRANS_SRC( C,R,L )/SPEC_BULK0( C,R,L ) )
                     END IF
                   ELSE
                     ! Assume that net gains are apportioned like current non-water aerosol
                     ! Determine the total non-water mass separating
                     ! inorganic and organic
                     SPEC_BULK0( C,R,L ) = SUM( SUM( ISAM0( C,R,L,:,: ), DIM=2),
     &                      MASK = (L_MASK_AERO .AND. 
     &                            L_MASK_TYPE .NE. 'H2O'  .AND.
     &                            OMH2O .EQV. L_MASK_OM   .AND.
     &                            IM .EQ. L_MASK_IM   ) )
                     DO K = 1,NTAG_SA
                       IF ( JAER(IM) .NE. 0 ) THEN
                          ISAM1( C,R,L,JAER(IM),K ) = ISAM0( C,R,L,JAER(IM),K ) +
     &                      SUM( ISAM0( C,R,L,:,K ),
     &                          MASK = (L_MASK_AERO .AND. 
     &                                L_MASK_TYPE .NE. 'H2O'  .AND.
     &                                OMH2O .EQV. L_MASK_OM   .AND. 
     &                                IM .EQ. L_MASK_IM  ) )
     &                          /SPEC_BULK0( C,R,L ) * BULK_TRANS_SRC( C,R,L )
                       END IF
                     END DO
                   END IF
                END DO
                END DO
                END DO
             END DO                       

          ! Nonvolatile Species (VOLTYPE = 'NVL')
          ELSE
             ! Nothing to be done for Nonvolatile Species
          END IF
      END DO

      ISAM1 = MAX( ISAM1, MIN_VAL )

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
          ICG1 = AEROSPC_MAP( IAER,1 )   ! Index of source in CGRID and process analysis arrays
          ICG2 = AEROSPC_MAP( IAER,2 )   ! Index of destination in CGRID and  process analysis arrays
          
          BULK1( :,:,: ) = SUM( ISAM1( :,:,:,ISA1,: ),4 ) 
          BULK_TRANS_SRC( :,:,: ) = -1.0 * DBLE( AERO_COAG( :,:,:,ICG1 ) + AERO_GROWTH(:,:,:,ICG1 ) )
          DO K = 1,NTAG_SA
             ! Calculate Apportioned mass leaving the source mode using the
             ! COAG and GROWTH process analysis arrays
             TAG_FRAC( :,:,: ) = MIN( ISAM1(:,:,:,ISA1,K) / BULK1(:,:,:) 
     &                                * BULK_TRANS_SRC( :,:,: ), 
     &                               ISAM1( :,:,:,ISA1,K ) - MIN_VAL )
             ISAM1( :,:,:,ISA1,K ) = ISAM1( :,:,:,ISA1,K ) - TAG_FRAC( :,:,: )

             ! Add that mass to the destination mode and recalculate
             ! apportionment
             ISAM1( :,:,:,ISA2,K ) = ISAM1( :,:,:,ISA2,K ) + TAG_FRAC( :,:,: ) 
          END DO

      END DO

      ! Update global ISAM array with modified attribution array ISAM1
      ISAM = ISAM1
      


      END 

