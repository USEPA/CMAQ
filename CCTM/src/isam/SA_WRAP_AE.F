
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
     &                      N_AEROSPC, AEROSPC
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
      INTEGER S,V,K
      INTEGER JSPCTAG
      INTEGER IBGN
      REAL    TOTAL_OLD, TOTAL_NEW
      CHARACTER( 16 ) :: PRECUR
      REAL, ALLOCATABLE, SAVE :: TTLB4( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: SULFCOND( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SULFCOND_AIT( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SULFCOND_ACC( :,:,: )
      REAL, ALLOCATABLE, SAVE :: CCOND( :,:,:,: )

! for interpx
      REAL DX1, DX2
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      REAL DENS ( NCOLS,NROWS,NLAYS )
      INTEGER MDATE, MTIME, MSTEP
      CHARACTER( 96 ) :: XMSG = ' '

! Constants of Gas Law
      REAL, PARAMETER :: SLP0 = 101325.0 ! standard atm (centibars) * 1000
      REAL, PARAMETER :: GAS_CONST = 8.2057E-05 ! gas const (L-atm/mole-k) / 1000
      REAL, PARAMETER :: MWNH3 = 17.03061
      REAL, PARAMETER :: MWNH4 = 18.03858
      REAL, PARAMETER :: MWHNO3 = 63.01287
      REAL, PARAMETER :: MWNO3 = 62.0049
      REAL, PARAMETER :: MWCL  = 35.453
      REAL, PARAMETER :: MWSO4 = 96.0576

! ppm2mole conversion
      REAL ppm2mole( NCOLS,NROWS,NLAYS )

! Identify ISAM species indices 20130529
      INTEGER, SAVE :: JNH3, JNH4I, JNH4J, JHNO3, JNO3I, JNO3J
      INTEGER, SAVE :: JHCL, JCLI , JCLJ
      INTEGER, SAVE :: JSULF, JSO4I, JSO4J, JSRXN

      INTEGER :: ISA1, ISA2, ICG1, ICG2, IAER, IM, INDX

      INTEGER, SAVE :: CNH3, CNH4I, CNH4J, CHNO3, CNO3I, CNO3J
      INTEGER, SAVE :: CHCL, CCLI,  CCLJ


! Arrays for combined ammonium, combined nitrates, and combined sulfates
      REAL, ALLOCATABLE, SAVE :: AMMONIA_TAG (:,:,:)
      REAL, ALLOCATABLE, SAVE :: NITRATE_TAG (:,:,:)
      REAL, ALLOCATABLE, SAVE :: CHLORINE_TAG(:,:,:)
      REAL, ALLOCATABLE, SAVE :: SULFATE_TAG (:,:,:)
      REAL, ALLOCATABLE, SAVE :: TOTAER_TAG (:,:,:)

      REAL, ALLOCATABLE, SAVE :: TOTNH3(:,:,:)
      REAL, ALLOCATABLE, SAVE :: TOTNO3(:,:,:)
      REAL, ALLOCATABLE, SAVE :: TOTCL (:,:,:)
      REAL, ALLOCATABLE, SAVE :: TOTSO4(:,:,:)
      REAL, ALLOCATABLE, SAVE :: TOTAER(:,:,:)
      REAL, ALLOCATABLE, SAVE :: FRAC1(:,:,:)
      REAL, ALLOCATABLE, SAVE :: FRAC2(:,:,:)

      REAL, ALLOCATABLE, SAVE :: ISAM0(:,:,:,:,:)
      REAL, ALLOCATABLE, SAVE :: ISAM1(:,:,:,:,:)

      REAL ::  FRAC( NCOLS,NROWS,NLAYS )
      REAL ::  A(3)

! Logicals for determining presence of inorganic ions
      LOGICAL, SAVE :: LAE_NH4 
      LOGICAL, SAVE :: LAE_NO3 
      LOGICAL, SAVE :: LAE_CL 
      LOGICAL, SAVE :: LAE_SO4 
     
      REAL, PARAMETER :: MIN_VAL = 1.0E-25
      REAL, SAVE :: MIN_TAGTOT 

      INTEGER ALLOCSTAT
!-------------------------------------------------------------------


Ckrt Identify species index in ISAM array
      ! Decompose domain for Density
      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.

        LAE_NH4 = .FALSE.
        LAE_NO3 = .FALSE.
        LAE_CL  = .FALSE.
        LAE_SO4 = .FALSE.

        JNH3  = INDEX1( 'NH3', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNH4I = INDEX1( 'ANH4I', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNH4J = INDEX1( 'ANH4J', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        IF ( JNH3 .GT. 0 .AND. JNH4I .GT. 0 .AND. JNH4J .GT. 0 ) THEN
          LAE_NH4 = .TRUE.
          CNH3  = SPC_INDEX( JNH3, 2 )
          CNH4I = SPC_INDEX( JNH4I,2 )
          CNH4J = SPC_INDEX( JNH4J,2 )
        END IF

        JHNO3 = INDEX1( 'HNO3', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNO3I = INDEX1( 'ANO3I', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNO3J = INDEX1( 'ANO3J', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        IF ( JHNO3 .GT. 0 .AND. JNO3I .GT. 0 .AND. JNO3J .GT. 0 ) THEN
          LAE_NO3 = .TRUE.
          CHNO3 = SPC_INDEX( JHNO3,2 )
          CNO3I = SPC_INDEX( JNO3I,2 )
          CNO3J = SPC_INDEX( JNO3J,2 )
        END IF
 
        JHCL = INDEX1( 'HCL',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JCLI = INDEX1( 'ACLI', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JCLJ = INDEX1( 'ACLJ', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        IF ( JHCL .GT. 0 .AND. JCLI .GT. 0 .AND. JCLJ .GT. 0 ) THEN
          LAE_CL = .TRUE.
          CHCL = SPC_INDEX( JHCL,2 )
          CCLI = SPC_INDEX( JCLI,2 )
          CCLJ = SPC_INDEX( JCLJ,2 )
        END IF
 
        JSULF = INDEX1( 'SULF', NSPC_SA, SPC_NAME( :,OTHRTAG ) )  
        JSO4I = INDEX1( 'ASO4I', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSO4J = INDEX1( 'ASO4J', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSRXN = INDEX1( 'SULRXN', NSPC_SA, SPC_NAME( :,OTHRTAG ) )  
        IF ( JSULF .GT. 0 .AND. JSO4I .GT. 0 .AND. JSO4J .GT. 0 
     &       .AND. JSRXN .GT. 0 ) LAE_SO4 = .TRUE.

        ! Map Aerosol Modes and Names to Source Apportionment Species
        ALLOCATE( MAP_AEROtoSA( N_AEROSPC, N_MODE ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating MAP_AEROtoSA'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        MAP_AEROtoSA( :,: ) = 0
        DO IM = 1,N_MODE
            DO IAER = 1,N_AEROSPC
                INDX = INDEX1( AEROSPC( IAER )%NAME( IM ), NSPC_SA, SPC_NAME( :,2 ) )
                IF ( INDX .NE. 0 ) MAP_AEROtoSA( IAER,IM ) = INDX
            END DO
        END DO

        MIN_TAGTOT = MIN_VAL * NTAG_SA
  
        IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open  MET_CRO_3D  file '
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE( CCOND( NCOLS,NROWS,NLAYS,NSPCSD ),
     &     TTLB4( NCOLS,NROWS,NLAYS,NSPC_SA ),
     &     SULFCOND( NCOLS,NROWS,NLAYS ), 
     &     SULFCOND_AIT( NCOLS,NROWS,NLAYS ),
     &     SULFCOND_ACC( NCOLS,NROWS,NLAYS ),
     &     STAT = ALLOCSTAT ) 
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating TTLB4, SULFCOND, SULFCOND_AIT, '
     &          //'or SULFCOND_ACC'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE( AMMONIA_TAG( NCOLS,NROWS,NLAYS ),
     &     NITRATE_TAG ( NCOLS,NROWS,NLAYS ),
     &     CHLORINE_TAG( NCOLS,NROWS,NLAYS ),
     &     SULFATE_TAG ( NCOLS,NROWS,NLAYS ),
     &     TOTAER_TAG  ( NCOLS,NROWS,NLAYS ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating AMMONIA_TAG, NITRATE_TAG, '
     &          //'SULFATE_TAG, or TOTAER_TAG'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE( TOTNH3( NCOLS,NROWS,NLAYS ),
     &     TOTNO3( NCOLS,NROWS,NLAYS ),
     &     TOTCL ( NCOLS,NROWS,NLAYS ),
     &     TOTSO4( NCOLS,NROWS,NLAYS ),
     &     TOTAER( NCOLS,NROWS,NLAYS ),
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
      
      ! Create 1D array of combined ammonium tags and combined nitrate tags
      AMMONIA_TAG  = 0.0
      NITRATE_TAG  = 0.0
      CHLORINE_TAG = 0.0
      SULFATE_TAG  = 0.0

      ! Sum up pre-process tags for each species at each grid cell
c     ISAM = MAX( ISAM, MIN_VAL )
      CGRID0 = MAX( CGRID0, MIN_TAGTOT )
      ISAM0(:,:,:,:,:) = ISAM(:,:,:,:,:)
      ISAM1(:,:,:,:,:) = ISAM(:,:,:,:,:)
      TTLB4( :,:,:,: ) = SUM( ISAM( :,:,:,:,: ),5 )

      CCOND = MAX( CGRID0 + AERO_COND, MIN_TAGTOT )

      IF ( LAE_SO4 ) THEN
         SULFCOND = MAX( ( AERO_COND( :,:,:,SPC_INDEX(JSO4I,2) ) 
     &            + AERO_COND( :,:,:,SPC_INDEX(JSO4J,2) ) 
     &            + AERO_NPF ( :,:,:,SPC_INDEX(JSO4I,2) ) ), MIN_VAL )
         SULFCOND_AIT = (AERO_COND( :,:,:,SPC_INDEX( JSO4I,2 ))  
     &                + AERO_NPF( :,:,:,SPC_INDEX(JSO4I,2)) ) / SULFCOND  
         SULFCOND_ACC = 1.0 - SULFCOND_AIT
      END IF

      ! Preprocess total - ammonium and ammonia
      IF ( LAE_NH4 ) THEN
        CCOND( :,:,:,CNH3 )  = CCOND( :,:,:,CNH3 ) * ppm2mole ! umol / m3
        CCOND( :,:,:,CNH4I ) = CCOND( :,:,:,CNH4I ) / MWNH4   ! umol / m3
        CCOND( :,:,:,CNH4J ) = CCOND( :,:,:,CNH4J ) / MWNH4   ! umol / m3
        TOTNH3 = TTLB4( :,:,:,JNH3 ) * ppm2mole + TTLB4( :,:,:,JNH4I ) / MWNH4 
     &          +TTLB4( :,:,:,JNH4J ) / MWNH4
      ENDIF 

      ! Preprocess total nitrate
      IF ( LAE_NO3 ) THEN
        CCOND( :,:,:,CHNO3 ) = CCOND( :,:,:,CHNO3 ) * ppm2mole
        CCOND( :,:,:,CNO3I ) = CCOND( :,:,:,CNO3I ) / MWNO3
        CCOND( :,:,:,CNO3J ) = CCOND( :,:,:,CNO3J ) / MWNO3
        TOTNO3 = TTLB4( :,:,:,JHNO3 ) * ppm2mole + TTLB4( :,:,:,JNO3I ) / MWNO3 
     &          +TTLB4( :,:,:,JNO3J ) / MWNO3
      ENDIF 
 
      ! Preprocess total chlorine
      IF ( LAE_CL ) THEN
        CCOND( :,:,:,CHCL ) = CCOND( :,:,:,CHCL ) * ppm2mole
        CCOND( :,:,:,CCLI ) = CCOND( :,:,:,CCLI ) / MWCL
        CCOND( :,:,:,CCLJ ) = CCOND( :,:,:,CCLJ ) / MWCL
        TOTCL = TTLB4( :,:,:,JHCL ) * ppm2mole + TTLB4( :,:,:,JCLI ) / MWCL
     &         +TTLB4( :,:,:,JCLJ ) / MWCL
      ENDIF 
 
      ! Preprocess total sulfate
      IF ( LAE_SO4 ) THEN
        TTLB4( :,:,:,JSULF ) = TTLB4( :,:,:,JSULF ) * ppm2mole
        TTLB4( :,:,:,JSRXN ) = TTLB4( :,:,:,JSRXN ) * ppm2mole
        TTLB4( :,:,:,JSO4I ) = TTLB4( :,:,:,JSO4I ) / MWSO4
        TTLB4( :,:,:,JSO4J ) = TTLB4( :,:,:,JSO4J ) / MWSO4
      ENDIF 

c  !------------------------!
c  !>>>>> CONDENSATION <<<<<!
      ! Find preprocess bulk fraction for condensable species and
      ! propagate source apporitonment through aerosol condensation changes
      DO K = 1, NTAG_SA

        ! Ammonium
        IF ( LAE_NH4 ) THEN
          
          AMMONIA_TAG( :,:,: ) = (  ISAM0( :,:,:,JNH3,K ) * ppm2mole
     &                              + ISAM0( :,:,:,JNH4I,K )/ MWNH4
     &                              + ISAM0( :,:,:,JNH4J,K )/ MWNH4 ) /
     &                                MAX( TOTNH3, MIN_TAGTOT*3.0 )
c     &                            (    TTLB4( :,:,:,JNH4I ) / MWNH4
c     &                              + TTLB4( :,:,:,JNH4J ) / MWNH4
c     &                              + TTLB4( :,:,:,JNH3 ) * ppm2mole )     

          ISAM1( :,:,:,JNH3,K )  = AMMONIA_TAG( :,:,: ) * CCOND( :,:,:,CNH3 ) / ppm2mole
          ISAM1( :,:,:,JNH4I,K ) = AMMONIA_TAG( :,:,: ) * CCOND( :,:,:,CNH4I ) * MWNH4
          ISAM1( :,:,:,JNH4J,K ) = AMMONIA_TAG( :,:,: ) * CCOND( :,:,:,CNH4J ) * MWNH4
        ENDIF 

        ! Nitrate
        IF ( LAE_NO3 ) THEN
           NITRATE_TAG( :,:,: ) = ( ISAM0( :,:,:,JHNO3,K ) * ppm2mole
     &                              + ISAM0( :,:,:,JNO3I,K )/ MWNO3
     &                              + ISAM0( :,:,:,JNO3J,K )/ MWNO3 ) /
     &                                MAX( TOTNO3, MIN_TAGTOT*3.0 )
c     &                            (    TTLB4( :,:,:,JNO3I ) / MWNO3
c     &                              + TTLB4( :,:,:,JNO3J ) / MWNO3
c     &                              + TTLB4( :,:,:,JHNO3 ) * ppm2mole )

          ISAM1( :,:,:,JHNO3,K ) = NITRATE_TAG( :,:,: ) * CCOND( :,:,:,CHNO3 ) / ppm2mole
          ISAM1( :,:,:,JNO3I,K ) = NITRATE_TAG( :,:,: ) * CCOND( :,:,:,CNO3I ) * MWNO3
          ISAM1( :,:,:,JNO3J,K ) = NITRATE_TAG( :,:,: ) * CCOND( :,:,:,CNO3J ) * MWNO3
        ENDIF

        ! Chlorine
        IF ( LAE_CL ) THEN
           CHLORINE_TAG( :,:,: ) = ( ISAM0( :,:,:,JHCL,K ) * ppm2mole
     &                             + ISAM0( :,:,:,JCLI,K )/ MWCL
     &                             + ISAM0( :,:,:,JCLJ,K )/ MWCL ) /
     &                                MAX( TOTCL, MIN_TAGTOT*3.0 )
c     &                            (    TTLB4( :,:,:,JNO3I ) / MWNO3
c     &                              + TTLB4( :,:,:,JNO3J ) / MWNO3
c     &                              + TTLB4( :,:,:,JHNO3 ) * ppm2mole )

          ISAM1( :,:,:,JHCL,K ) = CHLORINE_TAG( :,:,: ) * CCOND( :,:,:,CHCL ) / ppm2mole
          ISAM1( :,:,:,JCLI,K ) = CHLORINE_TAG( :,:,: ) * CCOND( :,:,:,CCLI ) * MWCL
          ISAM1( :,:,:,JCLJ,K ) = CHLORINE_TAG( :,:,: ) * CCOND( :,:,:,CCLJ ) * MWCL
        ENDIF
 
        ! Sulfate
        IF ( LAE_SO4 ) THEN
          ! Use Source Distribution of Sulfuric Acid Produced and apply
          ! that profile to the mass that was condensed to each mode.
          ISAM1( :,:,:,JSO4I,K ) = ISAM0( :,:,:,JSO4I,K ) + 
     &                             ISAM0( :,:,:,JSRXN,K ) * SULFCOND_AIT( :,:,: )
     &                             * ppm2mole ( :,:,: ) * 98.0

          ISAM1( :,:,:,JSO4J,K ) = ISAM0( :,:,:,JSO4J,K ) + 
     &                             ISAM0( :,:,:,JSRXN,K ) * SULFCOND_ACC( :,:,: )
     &                             * ppm2mole ( :,:,: ) * 98.0

          ISAM1( :,:,:,JSRXN,K ) = MIN_VAL
        ENDIF 

      ENDDO ! k tags

      ! Let the Sulfuric Acid Vapor Source Distribution equal that of the
      ! total condensed sulfate
      IF ( LAE_SO4 ) THEN
         TOTSO4 = MAX( SUM( ISAM1( :,:,:,JSO4I,: ),4 ) 
     &          + SUM( ISAM1(:,:,:,JSO4J,: ),4 ), MIN_TAGTOT*2.0 )
         DO K = 1,NTAG_SA
            FRAC( :,:,: ) = ( ISAM1( :,:,:,JSO4I,K ) + ISAM1( :,:,:,JSO4J,K ) ) / TOTSO4
            ISAM1( :,:,:,JSULF,K ) = FRAC( :,:,: ) * CGRID( :,:,:,SPC_INDEX(JSULF,2) )
         END DO
      END IF

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

