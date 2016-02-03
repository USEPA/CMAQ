
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
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C  Function: Driver subroutine for Euler Backward Iterative solver
C
C  Preconditions: For the SAPRC99 family of mechanisms
C
C  Key Subroutines/Functions Called:  HRINIT, PHOT, HRCALCKS, HRSOLVER
C
C  Revision History: Created by EBI solver program, Jan. 21, 2011
C                       Based on the algorithm in "Test of Two Numerical
C                       Schemes for Use in Atmospheric Transport-Chemistry
C                       Models", O. Hertel, R. Berkowicz, J. Christensen,
C                       and O. Hov, Atm Env., Vol. 27A, No. 16, 1993.
C                       Original MEBI code developed by Ho-Chun Huang,
C                       SUNY, Albany -- "On the performance of numerical
C                       solvers for a chemistry submodel in three-dimensional
C                       air quality models 1. Box model simulations",
C                       H. Huang and J.S. Chang, JGR, Vol 106, No. D17, 2001.
C                       This version replaces Huang and Chang use of numerical
C                       solutions with analytical solutions derived in
C                       Hertel et al.
C  21 Jun 10 J.Young: convert for Namelist redesign
C  29 Mar 11 S.Roselle: Replaced I/O API include files with UTILIO_DEFN
C  11 May 11 D.Wong: incorporated twoway model implementation
C  13 Nov 13 S.Napelenok: ddm-3d implementation for cmaq5.0.2
C   9 Sep 14 S.Napelenok: bug fix for IRXN logic check
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE VGRD_DEFN             ! vertical layer specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE HRDATA

#ifdef sens
      USE DDM3D_DEFN
#endif 

      IMPLICIT NONE

C..Includes:
      INCLUDE SUBST_FILES_ID  ! CMAQ files
      INCLUDE SUBST_CONST     ! CMAQ constants
      INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block
      INCLUDE SUBST_EMISPRM   ! Emissions processing control parameters
      INCLUDE SUBST_PACTL_ID  ! Process analysis control parameters

C..Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )  ! Species concentrations
      INTEGER JDATE           ! Current date (YYYYDDD)
      INTEGER JTIME           ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )      ! Time step vector (HHMMSS)

C..Parameters:
      REAL, PARAMETER :: PA2ATM = 1.0 / STDATMPA    ! Pascal to atm conv fac

C..External Functions:

C..Saved Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'HRDRIVER'     ! Program name

      INTEGER, SAVE :: ISTFL            ! Unit no. of iteration stat output file
      LOGICAL, SAVE :: LFIRST = .TRUE.  ! Flag for first call to this subroutine

      REAL, SAVE :: MAOMV               ! Mol Wt of air over Mol Wt of water

C..Scratch Local Variables:
      CHARACTER( 132 ) :: MSG           ! Message text
      CHARACTER(  16 ) :: VNAME         ! Name of I/O API data variable

      INTEGER C, E, L, R, S   ! Loop indices

      INTEGER AVGEBI          ! Average no. of EBI iterations
      INTEGER DELT_SEC        ! EBI max time step in seconds
      INTEGER ESP             ! Loop index for emissions species
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)
      INTEGER LEV             ! Layer index
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER MNEBI           ! Min no. of EBI iterations
      INTEGER MXEBI           ! Max no. of EBI iterations
      INTEGER NDARK           ! Number of layer 1 cells in darkness
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER SPC             ! Species loop index
      INTEGER STATUS          ! Status code
      INTEGER VAR             ! Variable number on I/O API file

      LOGICAL LSUNLIGHT       ! Flag for sunlight

      REAL ATMPRES            ! Cell pressure
      REAL CHEMSTEP           ! Chemistry integration interval (min)
      REAL H2O                ! Cell H2O mixing ratio (ppmV)
      REAL SUMEBI             ! Sum of EBI iterations
      REAL TEMP               ! Cell Temperature

      REAL PRES(    NCOLS, NROWS, NLAYS )        ! Cell pressure (Pa)
      REAL QV(      NCOLS, NROWS, NLAYS )        ! Cell water vapor (Kg/Kg air)
      REAL TA(      NCOLS, NROWS, NLAYS )        ! Cell temperature (K)
      REAL RJIN( NPHOTAB )                       ! J-values for a cell
      REAL RJ( NCOLS, NROWS, NLAYS, NPHOTAB )    ! J-values for each cell

      INTEGER     GXOFF, GYOFF            ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

#ifdef sens
      LOGICAL RXNFLAG( NPMAX )
      INTEGER I, J, RXN, N
      REAL*8 SRK( NRXNS )              ! rate constants

      REAL A( N_GC_SPC, N_GC_SPC )   ! An upper triangular matrix and the
                                     ! multipliers used to obtain it
                                     ! (See s_lu.F)
      REAL A1( N_GC_SPC, N_GC_SPC )
      REAL PDT( N_GC_SPC, N_GC_SPC )
      REAL PRD( N_GC_SPC )
      REAL PRD_RATE( N_GC_SPC )      ! Contribution to PRD from rxn rate sens
      REAL SOLD( N_GC_SPC )
      INTEGER INFO                   ! see s_lu.F
      INTEGER IPVT ( N_GC_SPC )      ! an integer vector of pivot indices.
      REAL SUM
      REAL IREGTEMP                  ! Holds relevant value of IREGION
      REAL KSTEP                     ! Holds k times timestep(in min)
      REAL RXNMULT                   ! Holds product of concs of reactants
      INTEGER NRCT                   ! Counter over reactants
      INTEGER NPROD                  ! Counter over products



C Variables used for hddm-3d
      REAL*8 SRK2 ( NRXNS )            ! rate constants
      LOGICAL ORDER1 ( NRXNS )       ! true if order 1; else, false
      INTEGER HIPARM( 2 )            ! index numbers of the 1st order sens
                                     ! parameters to which we're taking
                                     ! 2nd order sensitivity
      REAL PDT2( N_GC_SPC, N_GC_SPC )  ! Used for 2nd order call of JAC
      REAL SMID( NPMAX, N_GC_SPC )     ! SENGRID in middle of timestep
      REAL SMIDJAC( N_GC_SPC )         ! SMID for the relevant 1st order
                                       ! sensitivity parameter
      REAL*8 RK ( NRXNS )
      INTEGER HITMP1
      INTEGER HITMP2


#endif

C-----------------------------------------------------------------------

      IF ( N_GC_SPC .EQ. 0 ) RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for EBI solver
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

         LOGDEV = INIT3( )

         IF ( MECHNAME .NE. 'SAPRC99_AE5_AQ' .AND.
     &        MECHNAME .NE. 'SAPRC99_AE6_AQ'  ) THEN
             MSG = 'This version of the EBI solver can only be used with'
     &            // ' the SAPRC99 chemical mechanisms'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

#ifdef emis_chem
         EMISVD = .FALSE.
#else
         EMISVD = .TRUE.
#endif

         IF ( INDEX( MECHNAME, 'AE' ) .NE. 0 ) THEN
           L_AE_VRSN = .TRUE.
         ELSE
           L_AE_VRSN = .FALSE.
         END IF

         IF ( INDEX( MECHNAME, 'AQ' ) .NE. 0 ) THEN
           L_AQ_VRSN = .TRUE.
         ELSE
           L_AQ_VRSN = .FALSE.
         END IF

         IF ( LIRR ) THEN
            MSG = 'IRR Analysis not allowed with EBI solver'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         CALL HRINIT

         ITMSTEP = TIME2SEC( TSTEP( 2 ) )
         CHEMSTEP = FLOAT( ITMSTEP ) / 60.0
         WRITE( LOGDEV, 92000 ) CHEMSTEP, DELTAT

         WRITE( LOGDEV, 92020 )
         DO SPC = 1, N_GC_SPC
            WRITE( LOGDEV, 92040 ) GC_SPC( SPC ), RTOL( SPC )
         END DO

         MAOMV =  MWAIR / MWWAT

c..If emissions processing requested stop
         IF ( .NOT. EMISVD ) THEN  ! assumes emis processing in gas chem
            MSG = 'ERROR: EBI solver not configured to '//
     &            'process emissions in chemistry'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF   ! End if doing emissions

#ifdef hrstats
         ISTFL = JUNIT()
         OPEN( UNIT=ISTFL, FILE='iterstat.dat' )
         WRITE( ISTFL, 94020 )
#endif

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         LFIRST = .FALSE.

      END IF      ! First time

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical
C  data, and get photolysis rates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = FLOAT( ITMSTEP ) / 60.0D+00
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )

C.. Compute number of time step loops and step size for EBI solver
      DELT_SEC = DELTAT * 60.0 + 0.1
      IF ( DELT_SEC .GE. ITMSTEP ) THEN
         N_EBI_STEPS = 1
         EBI_TMSTEP = FLOAT( ITMSTEP ) / 60.0
      ELSE
         IF ( MOD( ITMSTEP, DELT_SEC ) .EQ. 0 ) THEN
            N_EBI_STEPS = ITMSTEP / DELT_SEC
         ELSE
            N_EBI_STEPS = ITMSTEP / DELT_SEC + 1
         END IF
         EBI_TMSTEP =  FLOAT( ITMSTEP ) / FLOAT( N_EBI_STEPS ) / 60.0
      END IF

      N_INR_STEPS = 1

C.. Get ambient temperature in K

      VNAME = 'TA'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, TA ) ) THEN
         MSG = 'Could not read TA from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get specific humidity in Kg H2O / Kg air
      VNAME = 'QV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, QV ) ) THEN
         MSG = 'Could not read QV from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get pressure in Pascals
      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, PRES ) ) THEN
         MSG = 'Could not read PRES from MET_CRO_3D'
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get photolysis rates in /min
      CALL PHOT ( MIDDATE, MIDTIME, JDATE, JTIME, NDARK, RJ )


#ifdef sens
      DATENUM = 1 + JDATE - STARTDATE !  Set the date and hour counters used in sensitivity calls

C For reaction rate sensitivities
      DO NP = 1, NPMAX
         IF ( IPT( NP ) .EQ. 5 ) THEN
            CALL CKTIME( JDATE,JTIME,NP,RXNFLAG(NP) ) ! Rxnflag set to true iff ipt=5 and time, date within bounds
            IF ( IDATE( NP, DATENUM ) .NE. 1 ) RXNFLAG( NP ) = .FALSE.
         ELSE
            RXNFLAG( NP ) = .FALSE.
         END IF
      END DO

C For higher order sensitivities
      IF ( HIGH ) THEN
         DO RXN = 1, NRXNS
            IF( NREACT( RXN ) .EQ. 1 ) THEN
               ORDER1( RXN ) = .TRUE.
            ELSE
               ORDER1( RXN ) = .FALSE.
            END IF
         END DO
      END IF
#endif sens

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Top of loop over cells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef hrstats
      MNEBI = 1000
      MXEBI = 0
      SUMEBI = 0.0
#endif

      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

c..Load ICs
               DO S = 1, N_GC_SPC
                  YC( S ) = MAX( CGRID( C, R, L, S ), 1.0E-30 )
               END DO

c..Set physical quantities
               TEMP = TA( C, R, L )
               ATMPRES = PA2ATM * PRES( C, R, L )
               H2O  = MAX ( QV( C, R, L ) * MAOMV *  1.0E+06, 0.0 )

c..Get rate constants
               LSUNLIGHT = .FALSE.
               DO NPH = 1, NPHOTAB
                  RJIN( NPH ) = RJ( C, R, L, NPH )
                  IF ( RJ( C, R, L, NPH ) .GT. 0.0 ) LSUNLIGHT = .TRUE.
               END DO

               CALL HRCALCKS( NPHOTAB, LSUNLIGHT, RJIN, TEMP,
     &                        ATMPRES, H2O, RKI )

c..Call EBI solver
               N_EBI_IT = 0

               CALL HRSOLVER( JDATE, JTIME, C, R, L )

#ifdef hrstats
               MXEBI  = MAX( MXEBI, N_EBI_IT )
               MNEBI  = MIN( MNEBI, N_EBI_IT )
               SUMEBI = SUMEBI + FLOAT( N_EBI_IT )
#endif

c..Update concentration array
               DO S = 1, N_GC_SPC
                  CGRID( C, R, L, S ) = YC( S )
               END DO

#ifdef sens
               DO RXN = 1, NRXNS
                  SRK( RXN ) = RKI( RXN )
                  IF ( HIGH ) THEN
                     IF ( ORDER1 (RXN ) ) THEN
                        SRK2( RXN ) = 0.0
                     ELSE
                        SRK2( RXN ) = RKI( RXN )
                     END IF
                  END IF
               END DO

               CALL JAC ( YCDDM, PDT, SRK ) ! Create the Jacobian matrix; YCDDM is set in mid-timestep of hrsolver.F

               DO 433 J = 1, N_GC_SPC
                  DO 434 I = 1, N_GC_SPC
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
434               CONTINUE
433            CONTINUE

C Factor matrix A by Gaussian elimination
               CALL SGEFA(A, N_GC_SPC, N_GC_SPC, IPVT,INFO)


               DO 495 NP = 1, NPMAX

                  IF ( IPT( NP ) .NE. 4 ) THEN

                     DO S = 1, N_GC_SPC
                        PRD( S ) = 0.0
                        PRD_RATE( S ) = 0.0
                        SOLD(S) = SENGRID( C, R, L, NP,S )
                        IF (ABS(SOLD( S )) .LT. 1.e-25 ) THEN
                           SOLD(S) = 0.
                        END IF
                     END DO

C Begin code specific to reaction rate sensitivities
                     IF ( RXNFLAG( NP ) ) THEN ! RXNFLAG is true if IPT = 5 and time, date within bounds
                        IREGTEMP = IREGION( C, R, L, NP )
                        DO RXN = 1, NRXNS
c                          IF ( IRXN( NP, RXN ) ) THEN ! This checks if it's a reaction in the sens parameter
                           IF ( IRXN( NP, RXN ) .EQ. 1 ) THEN ! This checks if it's a reaction in the sens parameter
                              KSTEP = SRK( RXN ) * CHEMSTEP ! KSTEP = k * timestep(in min)
                              ! RXNMULT is the product of the concs of the reactants
                              ! Note that the first 3 slots of IRR are for reactants,
                              ! and slots 4- are for products
                              IF ( NREACT( RXN ) .EQ. 1 ) THEN
                                 RXNMULT = KSTEP
     &                                   * YCDDM( IRR( RXN, 1 ) )
                              ELSE IF ( NREACT( RXN ) .EQ. 2 ) THEN
                                 RXNMULT = KSTEP
     &                                   * YCDDM( IRR( RXN, 1 ) )
     &                                   * YCDDM( IRR( RXN, 2 ) )
                              ELSE IF ( NREACT( RXN ) .EQ. 3 ) THEN
                                 RXNMULT = KSTEP
     &                                   * YCDDM( IRR( RXN, 1 ) )
     &                                   * YCDDM( IRR( RXN, 2 ) )
     &                                   * YCDDM( IRR( RXN, 3 ) )
                              ELSE
                                 MSG = 'NREACT out of expected bounds of 1-3.'
                                 CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                              END IF
  
                              DO NRCT = 1, NREACT( RXN ) ! Loop over the reactants
                                 PRD_RATE( IRR( RXN, NRCT ) ) = PRD_RATE( IRR( RXN, NRCT ) )
     &                                                        - RXNMULT * IREGTEMP  ! Subtract RXNMULT from PRD_RATE for reactant species
                              END DO

                              DO NPROD = 1, NPRDCT( RXN ) ! Loop over the products
                                 ! Add RXNMULT to PRD_RATE for product species
                                 ! The +3 reflects that slots >=4 of IRR are for products
                                 ! SC are the stoichiometric coefficients of products
                                 !    and do not need the +3 (see RXDT.EXT)
                                 PRD_RATE( IRR( RXN, 3+NPROD ) ) = PRD_RATE( IRR( RXN, 3+NPROD ) )
     &                                                           + ( RXNMULT * SC( RXN, NPROD ) * IREGTEMP )
                              END DO
                           END IF
                        END DO  ! RXN
                     END IF  ! RXNFLAG
C End code specific to reaction rate sensitivities

                     DO S = 1, N_GC_SPC
                        SUM = 0.0
                        DO J = 1, N_GC_SPC
                           SUM = SUM + A1( S, J ) * SOLD( J )
                        END DO
C edits by A.Digar
                        PRD( S ) = SUM + PRD_RATE( S )
C end edit
                     END DO

                     CALL SGESL( A, N_GC_SPC, N_GC_SPC, IPVT, PRD, 0 )

                     DO S = 1, N_GC_SPC
                        IF ( ABS ( PRD ( S ) ) .LT. 1.e-25 ) THEN
                           IF ( HIGH ) THEN
                              SMID( NP, S ) = 0.5 * SENGRID( C, R, L, NP, S )
                           END IF
                           SENGRID( C, R, L, NP, S ) = 0.
                        ELSE
                           IF ( HIGH ) THEN ! SMID is the average of SENGRID before and after chemistry
                              SMID( NP, S ) = 0.5 * ( SENGRID( C, R, L, NP, S ) + PRD( S) )
                           END IF
                           SENGRID( C, R, L, NP, S ) = PRD( S )
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

                     DO S = 1, N_GC_SPC
                        SMIDJAC( S ) = SMID( HIPARM( 1 ), S )
                     END DO
C Added by A.Digar
                     DO S = 1, N_GC_SPC
                        PRD( S ) = 0.0
                        SOLD(S) = SENGRID( C, R, L,NP,S )
                        IF (ABS(SOLD( S )) .LT. 1.e-25 ) THEN
                           SOLD(S) = 0.
                        END IF
                     END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Begin code specific to high-order sensitivity with one/more 1st order term/s
C being reaction rate sensitivity
C added by A.Digar
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                     HITMP1 = HIPARM( 1 )
                     HITMP2 = HIPARM( 2 )

                     DO N = 1, 2 ! loop for two 1st-order sens parameters
                        IF ( ( IPT( HITMP1 ) .EQ. 5 ) .AND. ( RXNFLAG( HITMP1 ) ) ) THEN ! check for rate constant sens, date & time
                           IREGTEMP = IREGION ( C, R, L, HITMP1 )
                           DO RXN = 1, NRXNS
                              ! keeping the rate terms that contain only the
                              ! rate constants of interest and setting rest to zero
                              RK( RXN ) = SRK( RXN ) * IRXN( HITMP1, RXN )
                           END DO
                           ! Jacobian for first-order,
                           ! called with sensitivities and
                           ! rxn rates with 1st order rxns effective  
                           CALL JAC ( YCDDM, PDT, RK )
                           DO S = 1, N_GC_SPC
                              DO J = 1, N_GC_SPC
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
                     CALL JAC ( SMIDJAC, PDT2, SRK2 )

C removed by A.Digar
c              DO S = 1, N_GC_SPC
c                PRD( S ) = 0.0
c                SOLD(S) = SENGRID( C, R, L,IP,S )
c                IF (ABS(SOLD( S )) .LT. 1.e-25 ) THEN
c                  SOLD(S) = 0.
c                END IF
c              END DO
C end of edits

                     DO S = 1, N_GC_SPC
                        SUM = 0.0
                        DO J = 1, N_GC_SPC
                           SUM = SUM + A1( S, J ) * SOLD( J )
     &                         + CHEMSTEP * PDT2( S,J ) * SMID( HIPARM( 2 ),J )
                        END DO
C edits by A.Digar
                        PRD( S ) = SUM + PRD( S )
C end of edits
                     END DO

                     CALL SGESL( A, N_GC_SPC, N_GC_SPC, IPVT, PRD, 0 )
 
                     DO S = 1, N_GC_SPC
                        IF ( ABS ( PRD ( S ) ) .LT. 1.e-25 ) THEN
                           SENGRID( C, R, L, NP, S ) = 0.
                        ELSE
                           SENGRID( C, R, L, NP, S ) = PRD( S )
                        END IF
                     END DO

                  END IF

495            CONTINUE    ! End of do loop over NP

#endif

            END DO
         END DO
      END DO

#ifdef hrstats
      AVGEBI = SUMEBI / FLOAT( NCOLS * NROWS * NLAYS )
      WRITE( ISTFL, 94040 ) JDATE, JTIME, MNEBI, AVGEBI, MXEBI
#endif

      RETURN

C----------------------- FORMAT STATEMENTS ----------------------------

92000 FORMAT( / 10X, 'Euler Backward Iterative Parameters -'
     &        / 10X, 'Chemistry Integration Time Interval (min):', F12.4,
     &        / 10X, 'EBI maximum time step (min):              ', F12.4 )

92020 FORMAT( //10X, 'Species convergence tolerances:' )

92040 FORMAT(   10X, A16, 2X, 1PE12.2 )

92060 FORMAT( / 10X, 'Emissions Processing in Chemistry ...'
     &        / 10X, 'Number of Emissions Layers:         ', I3
     &        / 10X, 'out of total Number of Model Layers:', I3 )


94020 FORMAT( 'DATE      TIME ', 'MNEBI AVEBI MXEBI' )

94040 FORMAT( I7, 1X, I6, 1X, 3( I5, 1X ) )
      END
