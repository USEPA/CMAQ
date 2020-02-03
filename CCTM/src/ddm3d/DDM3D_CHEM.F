#ifdef sens


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module DDM3D_CHEM

C-----------------------------------------------------------------------
C   9 Nov 15 S.L.Napelenok: initial version for cmaq5.1.1
C  16 Jun 16 S.L.Napelenok: update for cmaq5.2 and het chem inclusion
C-----------------------------------------------------------------------

      Use DDM3D_DEFN, Only: NPMAX
      
      Implicit None

      Real(8), Allocatable, Save ::  YCDDM( : )  ! Concs used by DDM
                                              ! (avg of pre- and post-chem,
                                              !  or mid-chemstep concs,
                                              !  depending on implementation)
      Logical, Allocatable, Save ::  DDM_CHECK( : ) ! check YCDDM set for all species

      Logical, Allocatable, Save :: RXNFLAG( : )
      INTEGER RXN
      Real*8, Allocatable, Save :: SRK( : )                ! rate constants

      Real, Allocatable, Save :: A( :, : )                 ! An upper triangular matrix and the
                                              ! multipliers used to obtain it
                                              ! (See s_lu.F)
      Real, Allocatable, Save :: A1( :, : )
      Real, Allocatable, Save :: PDT( :, : )
      Real, Allocatable, Save :: PRD( : )
      Real, Allocatable, Save :: PRD_RATE( : )             ! Contribution to PRD from rxn rate sens
      Real, Allocatable, Save :: SOLD( : )
      Integer, Allocatable, Save :: IPVT ( : )             ! an integer vector of pivot indices.

C Variables used for hddm-3d
      Real*8, Allocatable, Save :: SRK2 ( : )              ! rate constants
      LOGICAL, Allocatable, Save :: ORDER1 ( : )           ! true if order 1; else, false
      Real, Allocatable, Save :: PDT2( :, : )              ! Used for 2nd order call of JAC
      Real, Allocatable, Save :: SMID( :, : )              ! SENGRID in middle of timestep
      Real(8), Allocatable, Save :: SMIDJAC( : )              ! SMID for the relevant 1st order
                                              ! sensitivity parameter
      Real*8, Allocatable, Save :: RK ( : )

      Integer N_EBI_MID            ! the midpoint ebi step; half of N_EBI_STEPS
      Logical ODD_STEPS            ! true if N_EBI_STEPS is odd


      Contains

C-----------------------------------------------------------------------
      Subroutine INIT_DDM3D_CHEM

      Use RXNS_DATA, Only: NUMB_MECH_SPC, NRXNS
      Use UTILIO_DEFN                   ! IOAPI parameters and functions declarations

      Implicit None

      Character( 16 ), Save :: PNAME = 'INIT_DDM3D_CHEM'
      Integer LOGDEV 
      Character( 96 ) :: XMSG = ' '
      Integer ALLOCSTAT
      LOGDEV = INIT3 ()


      ALLOCATE( YCDDM ( NUMB_MECH_SPC), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating DDM_CHECK'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( DDM_CHECK( NUMB_MECH_SPC), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating YCDDM'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( RXNFLAG( NPMAX ),  STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating RXNFLAG'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( SRK( NRXNS ), 
     &          SRK2 ( NRXNS ),
     &          RK ( NRXNS ),
     &          ORDER1 ( NRXNS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating SRK, SRK2, RK, or ORDER1'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( A( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &          A1( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &          PDT( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &          PDT2( NUMB_MECH_SPC, NUMB_MECH_SPC ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating A, A1, PDT, or PDT2'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( SMID( NPMAX, NUMB_MECH_SPC ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating SMID'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( PRD( NUMB_MECH_SPC ),
     &          SOLD( NUMB_MECH_SPC ),
     &          IPVT ( NUMB_MECH_SPC ),
     &          PRD_RATE( NUMB_MECH_SPC ),
     &          SMIDJAC( NUMB_MECH_SPC ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        XMSG = 'Failure allocating PRD, SOLD, IPVT, PRD_RATE,or SMIDJAC'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      Return
      End Subroutine INIT_DDM3D_CHEM

C-----------------------------------------------------------------------

      Subroutine SOLVE_DDM3D_CHEM( C,R,L, CHEMSTEP)

      Use DDM3D_DEFN, Only: SENGRID, NPMAX, NP, DATENUM, IPT, 
     &                      IDATE, HIGH, IREGION, IRXN, IPARM, STARTDATE
      Use RXNS_DATA, Only: NRXNS, NREACT, NPRDCT, SC, IRR, CGRID_INDEX, NUMB_MECH_SPC

      Use MECHANISM_FUNCTIONS  

      Use UTILIO_DEFN                   ! IOAPI parameters and functions declarations

      Implicit None

      Character( 16 ), Save :: PNAME = 'SOLVE_DDM3D_CHEM'

      Integer C,R,L
      Real( 8 )  CHEMSTEP

      Integer I,J,S,N
      Integer INFO                            ! see s_lu.F
      Real SUM
      Real IREGTEMP                           ! Holds relevant value of IREGION
      Real KSTEP                              ! Holds k times timestep(in min)
      Real RXNMULT                            ! Holds product of concs of reactants
      Integer NRCT                            ! Counter over reactants
      Integer NPROD                           ! Counter over products

      Integer HIPARM( 2 )                     ! index numbers of the 1st order sens
                                              ! parameters to which
                                              ! we're taking
                                              ! 2nd order sensitivity
      Integer HITMP1
      Integer HITMP2

      Character( 96 ) :: XMSG = ' '
      Logical, Save :: FIRSTIME = .TRUE.
      Integer, Save :: LOGDEV



      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         CALL SET_MECHANISM( ) ! determine formulas for Mechanism Jacobain and Species Rate of Change

      END IF

      CALL  EVALUATE_F_JAC_MECH( YCDDM, SRK, PDT ) ! Evaluate Jacobian based on YCDDM and SKR values

      DO 433 J = 1, NUMB_MECH_SPC
         DO 434 I = 1, NUMB_MECH_SPC
            A( I, J ) = 0.0
            A1( I, J ) = 0.0
            A( I, J ) = -0.5 * CHEMSTEP * PDT( I, J )
            IF ( I .EQ. J ) THEN
               A( I, J ) = 1.0 + A( I, J )
            END IF
            A1( I, J ) = 0.5 * CHEMSTEP * PDT( I, J )
            IF ( I .EQ. J ) THEN
               A1( I, J ) = 1.0 + A1( I, J )
            END IF
434      CONTINUE
433   CONTINUE
C Factor matrix A by Gaussian elimination

      CALL SGEFA(A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT,INFO)

      DO 495 NP = 1, NPMAX

         IF ( IPT( NP ) .NE. 4 ) THEN

            DO S = 1, NUMB_MECH_SPC
               PRD( S ) = 0.0
               PRD_RATE( S ) = 0.0
               SOLD(S) = SENGRID( C, R, L, NP, CGRID_INDEX(S) )
               IF (ABS(SOLD( S )) .LT. 1.e-25 ) THEN
                  SOLD(S) = 0.
               END IF
            END DO

C Begin code specific to reaction rate sensitivities

            IF ( RXNFLAG( NP ) ) THEN ! RXNFLAG is true if IPT= 5 and time, date within bounds
               IREGTEMP = IREGION( C, R, L, NP )
               DO RXN = 1, NRXNS
                  IF ( IRXN( NP, RXN ) .EQ. 1 ) THEN ! This checks if it's a reaction in the sens parameter
                              KSTEP = SRK( RXN ) * CHEMSTEP ! KSTEP = k * timestep(in min)
                     ! RXNMULT is the product of the concs of
                     ! the reactants
                     ! Note that the first 3 slots of IRR are
                     ! for reactants,
                     ! and slots 4- are for products
                     IF ( NREACT( RXN ) .EQ. 1 ) THEN
                        RXNMULT = KSTEP
     &                          * YCDDM( IRR( RXN, 1 ) )
                     ELSE IF ( NREACT( RXN ) .EQ. 2 ) THEN
                        RXNMULT = KSTEP
     &                          * YCDDM( IRR( RXN, 1 ) )
     &                          * YCDDM( IRR( RXN, 2 ) )
                     ELSE IF ( NREACT( RXN ) .EQ. 3 ) THEN
                        RXNMULT = KSTEP
     &                          * YCDDM( IRR( RXN, 1 ) )
     &                          * YCDDM( IRR( RXN, 2 ) )
     &                          * YCDDM( IRR( RXN, 3 ) )
                     ELSE
                        XMSG = 'NREACT out of expected bounds of 1-3.'
                        CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                     END IF
  
                     DO NRCT = 1, NREACT( RXN ) ! Loop over the reactants
                        PRD_RATE( IRR( RXN, NRCT ) ) = PRD_RATE( IRR( RXN, NRCT ) )
     &                                               - RXNMULT * IREGTEMP  ! Subtract RXNMULT from PRD_RATE for reactant species
                     END DO

                     DO NPROD = 1, NPRDCT( RXN ) ! Loop over the products
                        ! Add RXNMULT to PRD_RATE for product
                        ! species
                        ! The +3 reflects that slots >=4 of IRR
                        ! are for products
                        ! SC are the stoichiometric
                        ! coefficients of products
                        !    and do not need the +3 (see
                        !    RXDT.EXT)
                        PRD_RATE( IRR( RXN, 3+NPROD ) ) = PRD_RATE( IRR( RXN, 3+NPROD ) )
     &                                                  + ( RXNMULT * SC( RXN, NPROD ) * IREGTEMP )
                     END DO
                  END IF
               END DO  ! RXN
            END IF  ! RXNFLAG
C End code specific to reaction rate sensitivities
            DO S = 1, NUMB_MECH_SPC
               SUM = 0.0
               DO J = 1, NUMB_MECH_SPC
                  SUM = SUM + A1( S, J ) * SOLD( J )
               END DO
C edits by A.Digar
                PRD( S ) = SUM + PRD_RATE( S )
C end edit
            END DO

            CALL SGESL( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, PRD, 0 )
            DO S = 1, NUMB_MECH_SPC
               IF ( ABS ( PRD ( S ) ) .LT. 1.e-25 ) THEN
                  IF ( HIGH ) THEN
                     SMID( NP, S ) = 0.5 * SENGRID( C, R, L, NP, CGRID_INDEX(S) )
                  END IF
                  SENGRID( C, R, L, NP, CGRID_INDEX(S) ) = 0.
               ELSE
                  IF ( HIGH ) THEN ! SMID is the average of SENGRID before and after chemistry
                     SMID( NP, S ) = 0.5 * ( SENGRID( C, R, L, NP, CGRID_INDEX(S) ) + PRD( S) )
                  END IF
                  SENGRID( C, R, L, NP, CGRID_INDEX(S) ) = PRD( S )
               END IF
            END DO

         ELSE ! IPT( NP ) = 4 2nd order sensitivity
            HIPARM( 1 ) = 0
            HIPARM( 2 ) = 0
            DO J = 1, NP - 1
               IF ( IPARM( NP, J ) .EQ. 1 ) THEN
                  HIPARM( 1 ) = J
               ELSE IF ( IPARM( NP, J ) .EQ. 2 ) THEN
                  HIPARM( 2 ) = J
               ELSE IF ( IPARM( NP, J ) .EQ. 3 ) THEN
                  HIPARM( 1 ) = J
                  HIPARM( 2 ) = J
               END IF
            END DO

            DO S = 1, NUMB_MECH_SPC
               SMIDJAC( S ) = SMID( HIPARM( 1 ), S )
            END DO
C Added by A.Digar
            DO S = 1, NUMB_MECH_SPC
               PRD( S ) = 0.0
               SOLD(S) = SENGRID( C, R, L,NP,CGRID_INDEX(S) )
               IF (ABS(SOLD( S )) .LT. 1.e-25 ) THEN
                  SOLD(S) = 0.
               END IF
            END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Begin code specific to high-order sensitivity with one/more 1st order
C term/s
C being reaction rate sensitivity
C added by A.Digar
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            HITMP1 = HIPARM( 1 )
            HITMP2 = HIPARM( 2 )

            DO N = 1, 2 ! loop for two 1st-order sens parameters
               IF ( ( IPT( HITMP1 ) .EQ. 5 ) .AND. ( RXNFLAG( HITMP1 ) ) ) THEN ! check for rate constant sens, date & time
                  IREGTEMP = IREGION ( C, R, L, HITMP1 )
                  DO RXN = 1, NRXNS
                     ! keeping the rate terms that contain only
                     ! the
                     ! rate constants of interest and setting
                     ! rest to zero
                     RK( RXN ) = SRK( RXN ) * IRXN( HITMP1, RXN )
                  END DO
                  ! Jacobian for first-order,
                  ! called with sensitivities and
                  ! rxn rates with 1st order rxns effective  
                  CALL  EVALUATE_F_JAC_MECH( YCDDM, RK, PDT ) ! Evaluate Jacobian based on YCDDM and RK values
                  DO S = 1, NUMB_MECH_SPC
                     DO J = 1, NUMB_MECH_SPC
                        PRD( S ) = PRD( S ) + CHEMSTEP * PDT( S,J ) * SMID( HITMP2,J ) * IREGTEMP
                     END DO
                  END DO
                  IF ( IPT( HITMP1 ) .eq. IPT( HITMP2 ) ) THEN
                     PRD = 2.0 * PRD
                     EXIT
                  ENDIF
               ENDIF
               HITMP1 = HIPARM( 2 )
               HITMP2 = HIPARM( 1 )
            END DO
C End of modification by A.Digar

            ! Jacobian for higher-order,
            ! called with sensitivities and
            ! rxn rates with 1st order rxns removed
            CALL  EVALUATE_F_JAC_MECH( SMIDJAC, SRK2, PDT2 ) ! Evaluate Jacobian based on SMIDJAC and SKR2 values

            DO S = 1, NUMB_MECH_SPC
               SUM = 0.0
               DO J = 1, NUMB_MECH_SPC
                  SUM = SUM + A1( S, J ) * SOLD( J )
     &                + CHEMSTEP * PDT2( S,J ) * SMID( HIPARM( 2 ),J )
               END DO
C edits by A.Digar
               PRD( S ) = SUM + PRD( S )
C end of edits
            END DO

            CALL SGESL( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, PRD, 0 )
 
            DO S = 1, NUMB_MECH_SPC
               IF ( ABS ( PRD ( S ) ) .LT. 1.e-25 ) THEN
                  SENGRID( C, R, L, NP, CGRID_INDEX(S) ) = 0.
               ELSE
                  SENGRID( C, R, L, NP, CGRID_INDEX(S) ) = PRD( S )
               END IF
            END DO

         END IF

495   CONTINUE    ! End of do loop over NP


      Return

      End Subroutine SOLVE_DDM3D_CHEM

C-----------------------------------------------------------------------

      End Module DDM3D_CHEM

#endif
