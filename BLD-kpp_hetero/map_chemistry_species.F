         FUNCTION MAP_CHEMISTRY_SPECIES() RESULT ( SUCCESS )

            USE UTILIO_DEFN
            USE CGRID_SPCS
            USE RXNS_DATA
            
            IMPLICIT NONE

            INTEGER, SAVE :: LOGDEV
            
            LOGICAL SUCCESS
            INTEGER I, IOS, J
            INTEGER I1, I2, I3, I4      ! SURROGATE TYPE 1 COUNTERS
            INTEGER J1, J2              ! SURROGATE TYPE 2 COUNTERS
            INTEGER K1, K2, K3, K4, K5  ! CONTROL TYPE COUNTERS
            INTEGER ICALL
            CHARACTER(  1 ), PARAMETER :: BL = ' '
            INTEGER, PARAMETER :: SPC_DIM = 200
            LOGICAL :: ORDER = .TRUE., FOUND = .TRUE.
            CHARACTER( 120 ) :: XMSG

            CHARACTER( 16 ), ALLOCATABLE     :: CGRID_SPC  ( : )
            CHARACTER( 16 ), ALLOCATABLE     :: NML_SPC    ( : )
            CHARACTER(  2 ), ALLOCATABLE     :: NML_TYPE   ( : )
            INTEGER,         ALLOCATABLE     :: NML_INDEX  ( : )
	    LOGICAL,         ALLOCATABLE     :: NML_CONVERT( : )

            LOGDEV = JUNIT ()
            SUCCESS = .TRUE.
            
            
            ALLOCATE ( CGRID_SPC( NSPCSD - 1 ), 
     &                 NML_SPC  ( NSPCSD - 1 ),  
     &                 NML_INDEX( NSPCSD - 1 ),  
     &                 NML_TYPE( NSPCSD - 1 ), 
     &                 NML_CONVERT( NSPCSD - 1 ), 
     &                 STAT = IOS )
           

            J = 0
                       
            NML_INDEX     = -1
            TYPE_INDEX    = -1
            NML_TYPE      = '??'
            SPECIES_MOLWT = -1.0
	    NML_CONVERT   = .FALSE.
            
            DO I = 1, N_GC_SPC ! load gc names and indices 
               J = J + 1
               CGRID_SPC( I )     = GC_SPC( I )
               NML_INDEX( J )     = I + GC_STRT -1
               NML_TYPE( J )      = 'GC'
               SPECIES_MOLWT( J ) = GC_MOLWT( I )
            END DO  

            DO I = 1, N_AE_SPC ! load ae names and indices 
               J = J + 1
               CGRID_SPC( J )     = AE_SPC( I )
               NML_INDEX( J )     = I + AE_STRT - 1
               NML_TYPE( J )      = 'AE'
               NML_CONVERT J )    = .TRUE.
               SPECIES_MOLWT( J ) = AE_MOLWT( I )
            END DO  

            DO I = 1, N_NR_SPC ! load nr names and indices 
               J = J + 1
               CGRID_SPC( J )     = NR_SPC( I )
               NML_INDEX( J )     = I + NR_STRT - 1
               NML_TYPE( J )      = 'NR'
               SPECIES_MOLWT( J ) = NR_MOLWT( I )
            END DO  

            DO I = 1, N_TR_SPC ! load tr names and indices 
               J = J + 1
               CGRID_SPC( J )     = TR_SPC( I )
               NML_INDEX( J )     = I + TR_STRT - 1
               NML_TYPE( J )      = 'TR'
               SPECIES_MOLWT( J ) = TR_MOLWT( I )
            END DO  

            NML_SPC( 1:(NSPCSD-1) ) = CGRID_SPC( 1:(NSPCSD-1) )
            
            
! determine if mechanism species are in cgrid species            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
            DO I = 1, NUMB_MECH_SPCS
               I1 = INDEX1R( MECHANISM_SPC( I ), (NSPCSD-1), CGRID_SPC )
               IF ( I1 .LT. 1 ) THEN
                  FOUND = .FALSE.
               ELSE
                  FOUND = .TRUE.
		  IF( .NOT. MAPPED_TO_CGRID )THEN
                      CGRID_INDEX( I )   = NML_INDEX( I1 )
                      SPECIES_TYPE( I )  = NML_TYPE ( I1 )
                      SPECIES_MOLWT( I ) = NML_MOLWT( I1 )
                      CONVERT_CONC( I )  = NML_CONVERT( I1 )
		  END IF
	          IF(CGRID_INDEX( I ) .NE. NML_INDEX( I1 ) .OR. CONVERT_CONC( I ) .NE. NML_CONVERT( I1 ))THEN
                      SUCCESS = .FALSE.
                      XMSG = '*** For Species ' // TRIM( MECHANISM_SPC( I )
    &                     // 'either cgrid index or species uniit conversion flag does not match mechcanism value.'
                      WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
		      WRITE( XMSG,'(A,I3,1X,A3,1X,L2)')'Mechanism INDEX,CONVERSION FLAG,SPECIES TYPE= ',CGRID_INDEX( I ),CONVERT_CONC( I ),
    &                 SPECIES_TYPE( I )
		      WRITE( LOGDEV,'( /5X, A )' )XMSG
		      WRITE( XMSG,'(A,I3,1X,A3,1X,L2)')'Namelists INDEX,CONVERSION FLAG,SPECIES TYPE= ',NML_INDEX( I1 ),CONVERT_NML( I1 ),
    &                 NML_TYPE( I1 )
		      WRITE( LOGDEV,'( /5X, A )' )XMSG
	          END IF
               END IF
               IF( INDEX( MECHANISM_SPC( I ), 'SRF') .GT. 0 )THEN
                   FOUND = .FALSE.
                   XMSG = '*** reactions cannot use modal aerosol surface area as species'
                   WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                   XMSG = TRIM( MECHANISM_SPC( I ) )
                   WRITE( LOGDEV,'( 9X, I4, 2X, A )' ) I, TRIM( XMSG )
               END IF
               IF( INDEX( MECHANISM_SPC( I ), 'NUM') .GT. 0 )THEN
                   FOUND = .FALSE.
                   XMSG = '*** reactions cannot use modal aerosol number density as species'
                   WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                   XMSG = TRIM( MECHANISM_SPC( I ) )
                   WRITE( LOGDEV,'( 9X, I4, 2X, A )' ) I, TRIM( XMSG )
               END IF
               IF ( .NOT. FOUND ) THEN
                  XMSG = 'Fatal error: Mechanism Species found not in species namelist:'
                  WRITE( LOGDEV,'( /5X, A )', ADVANCE = 'NO' ) TRIM( XMSG )
                  XMSG = TRIM( MECHANISM_SPC( I ) )
                  WRITE( LOGDEV,'( 9X, I4, 2X, A )' ) I, TRIM( XMSG )
                  SUCCESS = FOUND
               END IF
            END DO

         RETURN

         END FUNCTION MAP_CHEMISTRY_SPECIES
!----------------------------------------------------------------------------------------
         INTEGER FUNCTION INDEX1R ( NAME, N, NLIST )
            IMPLICIT NONE
            CHARACTER( * ) NAME        ! character string being searched for
            INTEGER N                  ! length of array to be searched
            CHARACTER( * ) NLIST( : )  ! array to be searched

            INTEGER I

            DO I = 1, N
               IF ( NAME .EQ. NLIST( I ) ) THEN
                  INDEX1R = I
                  RETURN
               END IF
           END DO
           INDEX1R = 0
           RETURN

          END FUNCTION INDEX1R
