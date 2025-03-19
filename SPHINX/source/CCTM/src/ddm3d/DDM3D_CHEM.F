#ifdef sens


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module DDM3D_CHEM

C-----------------------------------------------------------------------
C   9 Nov 15 S.L.Napelenok: initial version for cmaq5.1.1
C  16 Jun 16 S.L.Napelenok: update for cmaq5.2 and het chem inclusion
C-----------------------------------------------------------------------

      Use DDM3D_DEFN, Only: NPMAX
      
      Implicit None

      Real(8), Allocatable, Save ::  YCHECK( : )  ! Concs used by DDM
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
      Integer, Allocatable       :: SENS_INDEX( : )        ! CGRID_INDEX translated by JNEW2OLD
      Integer, Allocatable       :: MECH_INDEX( : )        ! CGRID_INDEX translated by JNEW2OLD

C Variables used for hddm-3d
      Real(8), Allocatable, Save :: SRK2 ( : )             ! rate constants
      LOGICAL, Allocatable, Save :: ORDER1 ( : )           ! true if order 1; else, false
      Real,    Allocatable, Save :: PDT2( :, : )           ! Used for 2nd order call of JAC
      Real,    Allocatable, Save :: SMID( :, : )           ! SENGRID in middle of timestep
      Real(8), Allocatable, Save :: SEND( :, : )           ! SENGRID at end of timestep
      Real(8), Allocatable, Save :: SMIDJAC( : )           ! SMID for the relevant 1st order
                                                           ! sensitivity parameter
      Real(8), Allocatable, Save :: RK ( : )

      Integer N_EBI_MID            ! the midpoint ebi step; half of N_EBI_STEPS
      Logical ODD_STEPS            ! true if N_EBI_STEPS is odd


      Contains

C-----------------------------------------------------------------------
      Subroutine INIT_DDM3D_CHEM

      Use RXNS_DATA, Only: NUMB_MECH_SPC, NRXNS, CGRID_INDEX
      Use UTILIO_DEFN                   ! IOAPI parameters and functions declarations

      Implicit None

      Character( 16 ), Save :: PNAME = 'INIT_DDM3D_CHEM'
      Integer               :: LOGDEV 
      Character( 96 )       :: XMSG = ' '
      Integer               :: ALLOCSTAT
      Integer               :: ISPC

      LOGDEV = INIT3 ()


      ALLOCATE( YCHECK ( NUMB_MECH_SPC), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating YCHECK'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( YCDDM ( NUMB_MECH_SPC), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating YCDDM'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( DDM_CHECK( NUMB_MECH_SPC), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating  DDM_CHECK'
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

      ALLOCATE( SMID( NUMB_MECH_SPC,NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating SMID'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( SEND( 1,NUMB_MECH_SPC ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating SEND'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF
      SEND = 0.0D0

      ALLOCATE( PRD( NUMB_MECH_SPC ),
     &          SOLD( NUMB_MECH_SPC ),
     &          IPVT ( NUMB_MECH_SPC ),
     &          PRD_RATE( NUMB_MECH_SPC ),
     &          SMIDJAC( NUMB_MECH_SPC ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        XMSG = 'Failure allocating PRD, SOLD, IPVT, PRD_RATE,or SMIDJAC'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( SENS_INDEX( NUMB_MECH_SPC ),
     &          MECH_INDEX( NUMB_MECH_SPC ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating SENS_INDEX or MECH_INDEX'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF
      
      DO ISPC = 1, NUMB_MECH_SPC
         MECH_INDEX(ISPC) = CGRID_INDEX(ISPC)
         SENS_INDEX(ISPC) = CGRID_INDEX(ISPC)
      END DO

      Return
      End Subroutine INIT_DDM3D_CHEM

C-----------------------------------------------------------------------

      Subroutine SOLVE_DDM3D_CHEM( C,R,L,CHEMSTEP,OLD2NEW,NEW2OLD )

      Use DDM3D_DEFN, Only: SENGRID, NPMAX, NP, DATENUM, IPT, 
     &                      IDATE, HIGH, IREGION, IRXN, IPARM, STARTDATE
      Use RXNS_DATA,  Only: NRXNS, NREACT, NPRDCT, SC, IRR, CGRID_INDEX, NUMB_MECH_SPC,
     &                      RXLABEL, CHEMISTRY_SPC

      Use MECHANISM_FUNCTIONS  

      Use UTILIO_DEFN                   ! IOAPI parameters and functions declarations

      Implicit None

      Character( 16 ), Save :: PNAME = 'SOLVE_DDM3D_CHEM'

      Integer,   Intent( In ) :: C,R,L
      Real( 8 ), Intent( In ) :: CHEMSTEP
      Integer,   Optional, Intent( In ) :: OLD2NEW( :,: )
      Integer,   Optional, Intent( In ) :: NEW2OLD( :,: )

      Integer I,J,S,N
      Integer JROW, JCOL
      Integer INFO                            ! see s_lu.F

      Real(8) SUMAT
      Real(8) SUMSP
      Real(8) TOTAL
      Real(8) DIFF
      Real(8) DIFFSP
      
      Real IREGTEMP                           ! Holds relevant value of IREGION
      Real KSTEP                              ! Holds k times timestep(in min)
      Real RXNMULT                            ! Holds product of concs of reactants
      
      Integer NRCT                            ! Counter over reactants
      Integer NPROD                           ! Counter over products
      Integer IREACT                          ! index for reaction reactant
      Integer IPROD                           ! index for reaction produce
      Integer HIPARM( 2 )                     ! index numbers of the 1st order sens
                                              ! parameters to which
                                              ! we're taking
                                              ! 2nd order sensitivity
      Integer HITMP1
      Integer HITMP2

      Logical, Save :: FIRSTIME = .TRUE.
      Logical, Save :: REORDER  = .TRUE. ! reorder YCDDM
      Integer, Save :: LOGDEV

      Character( 96 ) :: XMSG = ' '

      IF ( FIRSTIME ) THEN

         FIRSTIME  = .FALSE.
         LOGDEV    = INIT3()
         DDM_LOG   = LOGDEV
         ERROR_LOG = LOGDEV

         CALL SET_MECHANISM( ) ! determine formulas for Mechanism Jacobain and Species Rate of Change
  

         IF( PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW ) )THEN

             REORDER = .FALSE. ! no reordering because YCDMM is already redeorderd by chemsolver
             
!             YCHECK = 0.0D0
!             DO I = 1,NUMB_MECH_SPC
!                S  = NEW2OLD( I,1 )
!                YCHECK( S ) = YCDDM( I )
!                SEND(1,I) = YCDDM( I )
!                WRITE(LOGDEV,'(A,A,ES16.4)')' ORIG: ' // CHEMISTRY_SPC(I),' = ',YCHECK(S)
!             END DO
!             DO I = 1,NUMB_MECH_SPC
!                S  = NEW2OLD( I,1 )
!                WRITE(LOGDEV,'(A,A,2(ES16.4,1X))')' SORT: ' // CHEMISTRY_SPC(S),' = ',YCDDM( I ),YCHECK(S)-YCDDM( I )
!             END DO
!             CALL  EVALUATE_F_JAC_MECH( YCHECK, SRK, PDT2 ) ! Evaluate Jacobian based on YCDDM and SKR values

! overwrite reordering using chemsolver conversing maps

             JNEW2OLD(1:NUMB_MECH_SPC,1) = NEW2OLD(1:NUMB_MECH_SPC,1)
             JOLD2NEW(1:NUMB_MECH_SPC,1) = OLD2NEW(1:NUMB_MECH_SPC,1)
             RESET_JACOBIAN = .TRUE.
             DO RXN = 1, NRXNS
                DO NRCT = 1, NREACT( RXN )
                   IREACT = ISPECIES_REACTION( NRCT,RXN )
                   ISPECIES_REACTION( NRCT,RXN ) = OLD2NEW( IREACT,1 )
                END DO

                DO NPROD = 1, NPRDCT( RXN )
                   IPROD = ISPECIES_REACTION( NPROD+3,RXN )
                   ISPECIES_REACTION( NPROD+3,RXN ) = OLD2NEW( IPROD,1 )
               END DO
             END DO

!             DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!             END DO

!         ELSE
!
!            DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JOLD2NEW( I,1 )
!               YCHECK( S ) = YCDDM( I )
!               WRITE(LOGDEV,'(A,A,ES16.4)')' SORT: ' // CHEMISTRY_SPC(I),' = ',YCHECK(S)
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JNEW2OLD( I,1 )
!               WRITE(LOGDEV,'(A,A,2(ES16.4,1X))')' ORIGINAL: ' // CHEMISTRY_SPC(S),' = ',YCDDM( S ),YCHECK(I)-YCDDM( S )
!            END DO
!            YCDDM = YCHECK
         
         END IF
 
!             DO RXN = 1, NRXNS

!                WRITE(LOGDEV,'(A,1X,I4,1X,A)')'For reaction number and label,',RXN,
!     &                                         TRIM(RXLABEL(RXN))
!                WRITE(LOGDEV,*)'Reactants'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NRCT,RXN ),1)),NRCT = 1, NREACT( RXN ))

!                WRITE(LOGDEV,*)'Products'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NPROD+3,RXN ),1)),NPROD = 1, NPRDCT( RXN ))

!             END DO
!             WRITE(LOGDEV,*)'Original order: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(S),CGRID_INDEX(S)
!             END DO
!             WRITE(LOGDEV,*)'Sort ordered: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(JOLD2NEW(S,1)),MECH_INDEX(S)
!             END DO

      END IF
      
      IF ( REORDER ) THEN  ! reorder to speed-up LU decomposition of Jacobian

            DO I = 1,NUMB_MECH_SPC
               S  = JOLD2NEW( I,1 )
               YCHECK( S ) = YCDDM( I )
            END DO
            YCDDM = YCHECK
         
      END IF

      CALL  EVALUATE_F_JAC_MECH( YCDDM, SRK, PDT ) ! Evaluate Jacobian based on YCDDM and SKR values

      DO 433 J = 1, NUMB_MECH_SPC
         DO 434 I = 1, NUMB_MECH_SPC
            A( I, J )  = 0.0
            A1( I, J ) = 0.0
            A( I, J )  = -0.5 * CHEMSTEP * PDT( I, J )
            A1( I, J ) =  0.5 * CHEMSTEP * PDT( I, J )
            IF ( I .EQ. J ) THEN
               A( I, J )  = 1.0 + A( I, J )
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
               J = JOLD2NEW(S,1)
               SOLD(J) = SENGRID( C, R, L, NP,CGRID_INDEX(S))
               IF ( ABS(SOLD( J )) .LT. 1.e-25 ) SOLD(J) = 0.0
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
     &                          * YCDDM( ISPECIES_REACTION( 1,RXN ) )
                     ELSE IF ( NREACT( RXN ) .EQ. 2 ) THEN
                        RXNMULT = KSTEP
     &                          * YCDDM( ISPECIES_REACTION( 1,RXN ) )
     &                          * YCDDM( ISPECIES_REACTION( 2,RXN ) )
                     ELSE IF ( NREACT( RXN ) .EQ. 3 ) THEN
                        RXNMULT = KSTEP
     &                          * YCDDM( ISPECIES_REACTION( 1,RXN ) )
     &                          * YCDDM( ISPECIES_REACTION( 2,RXN ) )
     &                          * YCDDM( ISPECIES_REACTION( 3,RXN ) )
                     ELSE
                        XMSG = 'NREACT out of expected bounds of 1-3.'
                        CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                     END IF
  
                     DO NRCT = 1, NREACT( RXN ) ! Loop over the reactants
                        PRD_RATE( ISPECIES_REACTION( NRCT,RXN ) ) = PRD_RATE( ISPECIES_REACTION( NRCT,RXN ) )
     &                                                            - RXNMULT * IREGTEMP  ! Subtract RXNMULT from PRD_RATE for reactant species
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
                        PRD_RATE( ISPECIES_REACTION( 3+NPROD,RXN ) ) = PRD_RATE( ISPECIES_REACTION( 3+NPROD,RXN ) )
     &                                                               + ( RXNMULT * SC( RXN, NPROD ) * IREGTEMP )
                     END DO
                  END IF
               END DO  ! RXN
            END IF  ! RXNFLAG
C End code specific to reaction rate sensitivities
            DO S = 1, NUMB_MECH_SPC
               TOTAL = 0.0D0
               DO J = 1, NUMB_MECH_SPC
                  TOTAL = TOTAL + A1( S, J ) * SOLD( J )
               END DO
C edits by A.Digar
                PRD( S ) = TOTAL + PRD_RATE( S )
C end edit
            END DO

            CALL SGESL( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, PRD, 0 )
            DO S = 1, NUMB_MECH_SPC
               J = CGRID_INDEX(JNEW2OLD(S,1))
               IF ( ABS ( PRD ( S ) ) .LT. 1.e-25 ) THEN
                  IF ( HIGH ) THEN
                      SMID( S,NP ) = 0.5 * SOLD(S)
                  END IF
                  SENGRID( C, R, L, NP,J )  = 0.0
               ELSE
                  IF ( HIGH ) THEN ! SMID is the average of SENGRID before and after chemistry
                      SMID( S,NP ) = 0.5 * ( SOLD(S) + PRD( S) )
                  END IF
                  SENGRID( C, R, L, NP,J  ) = PRD( S )
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
               SMIDJAC( S ) = SMID( S,HIPARM( 1 ) )
            END DO
C Added by A.Digar
            DO S = 1, NUMB_MECH_SPC
               PRD(S) = 0.0
               J = JOLD2NEW(S,1)
               SOLD(J) = SENGRID( C, R, L, NP, CGRID_INDEX(S) ) 
               IF (ABS(SOLD( J )) .LT. 1.e-25 ) SOLD(J) = 0.0
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
                        PRD( S ) = PRD( S ) + CHEMSTEP * PDT( S,J ) * SMID( J,HITMP2 ) * IREGTEMP
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
               TOTAL = 0.0
               DO J = 1, NUMB_MECH_SPC
                  TOTAL = TOTAL + A1( S, J ) * SOLD( J )
     &                  + CHEMSTEP * PDT2( S,J ) * SMID( J,HIPARM( 2 ) )
               END DO
C edits by A.Digar
               PRD( S ) = TOTAL + PRD( S )
C end of edits
            END DO

            CALL SGESL( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, PRD, 0 )
 
            DO S = 1, NUMB_MECH_SPC
               IF ( ABS ( PRD ( S ) ) .LT. 1.e-25 ) THEN
                  SENGRID( C, R, L, NP, CGRID_INDEX(JNEW2OLD(S,1)) ) = 0.0
               ELSE
                  SENGRID( C, R, L, NP, CGRID_INDEX(JNEW2OLD(S,1)) ) = PRD( S )
               END IF
            END DO

         END IF

495   CONTINUE    ! End of do loop over NP


      Return

      End Subroutine SOLVE_DDM3D_CHEM

C-----------------------------------------------------------------------

      End Module DDM3D_CHEM

#endif
