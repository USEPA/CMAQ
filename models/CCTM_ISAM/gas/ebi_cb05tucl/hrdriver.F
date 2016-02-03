
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
C $Header: /project/yoj/arc/CCTM/src/gas/ebi_cb05tucl/hrdriver.F,v 1.4 2011/10/21 16:10:52 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ifdef isam
       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP, ISAM, CBFOR, ISAMB4 )
#else
       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP )
#endif

C-----------------------------------------------------------------------
C  Function: Driver subroutine for Euler Backward Iterative solver
C
C  Preconditions: For the CB05TUCL family of mechanisms
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
C  19 Jul 13 R.Kwok: isam
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE VGRD_DEFN             ! vertical layer specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
#ifdef isam
      USE SA_IRR_DEFN           ! 20120905 inherits hrdata
#else
      USE HRDATA
#endif
#ifdef isam
      USE SA_DEFN            ! 20130523
      USE OZVOC_DEFN         ! 20130606
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
#ifdef isam
      REAL, POINTER :: ISAM( :,:,:,:,: ) ! 20130523
      REAL, POINTER :: CBFOR( :,:,:,: ) ! 20130606
      REAL, POINTER :: ISAMB4( :,:,:,:,: ) ! 20130606
#endif

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

!Below inserted 20130523
#ifdef isam
      INTEGER  JSPC, KTAG
      ! nitrogen indices in isam
      INTEGER, SAVE :: JNO, JNO2, JNO3, JHONO, JN2O5
      INTEGER, SAVE :: JPNA, JPAN, JPANX, JHNO3, JNTR
      ! tag class sulfate 20130529
      INTEGER, SAVE :: JSO2, JSULF, JSRXN
      ! tag class CO  20130529
      INTEGER, SAVE :: JCO
      ! vocs for tag class ozone  20130606; voc species made explicit, 20131211
      INTEGER, ALLOCATABLE, SAVE :: JVOC( : )
      INTEGER   IVOC   ! running voc index 20131211
      ! 1-ozone 20130627
      INTEGER, SAVE :: JO3A
      ! variables borrowed from DDM
      INTEGER I, J, RXN
      REAL( 8 ) :: SRK( NRXNS )
      REAL      :: A( N_GC_SPC, N_GC_SPC )
      REAL      :: A1( N_GC_SPC, N_GC_SPC )
      REAL      :: PDT( N_GC_SPC, N_GC_SPC )
      REAL      :: PRD( N_GC_SPC )
      REAL      :: SOLD( N_GC_SPC )
      REAL      :: SUUM
      INTEGER   :: INFO
      INTEGER   :: IPVT( N_GC_SPC )
#endif

C-----------------------------------------------------------------------

      IF ( N_GC_SPC .EQ. 0 ) RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for EBI solver
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

         LOGDEV = INIT3( )

         IF ( MECHNAME .NE. 'CB05TUCL_AE5_AQ' .AND.
     &        MECHNAME .NE. 'CB05TUCL_AE6_AQ' ) THEN
             MSG = 'This version of the EBI solver can only be used with'
     &            // ' the CB05TUCL chemical mechanisms'
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

#ifdef isam
         CALL SA_IRR_INIT   ! re-introduced 20130606
         ALLOCATE( YCDDM( N_GC_SPC ) )

        ! krt Identify species index in ISAM array: tag class nitrate
        JNO   = INDEX1( 'NO' ,  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNO2  = INDEX1( 'NO2',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNO3  = INDEX1( 'NO3',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JHONO = INDEX1( 'HONO', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JN2O5 = INDEX1( 'N2O5', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JPNA  = INDEX1( 'PNA',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JPAN  = INDEX1( 'PAN',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JPANX = INDEX1( 'PANX', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JHNO3 = INDEX1( 'HNO3', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNTR  = INDEX1( 'NTR',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        ! tag class sulfate
        JSO2  = INDEX1( 'SO2',  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSULF = INDEX1( 'SULF', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSRXN = INDEX1( 'SULRXN', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        ! tag class CO
        JCO   = INDEX1( 'CO' ,  NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        ! vocs for tag class ozone
        IF ( .NOT. ALLOCATED( JVOC ) ) ALLOCATE ( JVOC( N_EVOC ) )
        DO IVOC = 1, N_EVOC
          JVOC( IVOC ) = INDEX1( NAM_VOC( IVOC ), NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        ENDDO ! ivoc
        ! 1-ozone 20130627
        JO3A  = INDEX1( 'O3A', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
#endif

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

#ifdef isam
               DO RXN = 1, NRXNS
                 SRK( RXN ) = RKI( RXN )
               ENDDO

               ! Construct Jacobian matrix
               CALL JAC ( YCDDM, PDT, SRK )

               ! Construct matrices (Id - dt/2*Jac) and (Id + dt/2*Jac)
               DO J = 1, N_GC_SPC
               DO I = 1, N_GC_SPC
                  A( I,J ) = 0.0
                  A1( I,J ) = 0.0
                  A( I,J ) = -0.5 * CHEMSTEP * PDT( I, J )
                  IF ( I .EQ. J ) A( I,J ) = 1.0 + A( I,J )
                  A1( I,J ) = 0.5 * CHEMSTEP * PDT( I,J )
                  IF ( I .EQ. J ) A1( I,J ) = 1.0 + A1( I,J )
               END DO
               END DO

               ! Factorize A = inv(Id-dt/2*Jac)
               CALL SGEFA( A, N_GC_SPC, N_GC_SPC, IPVT, INFO )


               DO 495 KTAG = 1, NTAG_SA
                  SOLD = 0.0
                  PRD = 0.0
                  DO JSPC = 1, NSPC_SA
                     IF ( JSPC .EQ. JNO .OR. JSPC .EQ. JNO2 .OR.
     &                    JSPC .EQ. JNO3 .OR. JSPC .EQ. JHONO .OR. JSPC .EQ. JN2O5 .OR.
     &                    JSPC .EQ. JPNA .OR. JSPC .EQ. JPAN .OR. JSPC .EQ. JPANX .OR.
     &                    JSPC .EQ. JHNO3 .OR. JSPC .EQ. JNTR ) THEN
                        ! Populates species from nitrate tag class
                        SOLD( SPC_INDEX( JSPC, 2 ) ) = ISAMB4( C,R,L,JSPC,KTAG )
                     ELSEIF ( JSPC .EQ. JVOC( 1 ) .OR. JSPC .EQ. JVOC( 2 )
     &    .OR. JSPC .EQ. JVOC( 3 ) .OR. JSPC .EQ. JVOC( 4 )
     &    .OR. JSPC .EQ. JVOC( 5 ) .OR. JSPC .EQ. JVOC( 6 )
     &    .OR. JSPC .EQ. JVOC( 7 ) .OR. JSPC .EQ. JVOC( 8 )
     &    .OR. JSPC .EQ. JVOC( 9 ) .OR. JSPC .EQ. JVOC(10 )
     &    .OR. JSPC .EQ. JVOC( 11) .OR. JSPC .EQ. JVOC(12 )
     &    .OR. JSPC .EQ. JVOC( 13) .OR. JSPC .EQ. JVOC(14 ) ) THEN
                        SOLD( SPC_INDEX( JSPC, 2 ) ) = ISAMB4( C,R,L,JSPC,KTAG ) 
                     ELSE IF ( JSPC .EQ. JO3A ) THEN
                     ! For 1-ozone
                        SOLD( SPC_INDEX( JSPC, 2 ) ) = ISAMB4( C,R,L,JSPC,KTAG )
                     ELSE IF ( JSPC .EQ. JCO ) THEN
                     ! Populates CO tag class
                        SOLD( SPC_INDEX( JSPC, 2 ) ) = ISAMB4( C,R,L,JSPC,KTAG )
                     ELSE IF ( JSPC .EQ. JSO2 .OR. JSPC .EQ. JSULF .OR. JSPC .EQ. JSRXN ) THEN
                     ! Populates species from sulfate tag class
                        SOLD( SPC_INDEX( JSPC, 2 ) ) = ISAMB4( C,R,L,JSPC,KTAG )
                     END IF ! jspc from either tag classes
                  END DO ! loop jspc

                  ! Evaluate prd = A1*sold = (Id+dt/2*Jac)*sold
                  DO S = 1, N_GC_SPC
                     SUUM = 0.0
                     DO J = 1, N_GC_SPC
                        SUUM = SUUM + A1( S, J ) * SOLD( J )
                     END DO
                     PRD( S ) = SUUM
                  END DO

                  ! Evaluate inv(A)*prd = inv(Id-dt/2*Jac) * prd
                  ! Store results in prd
                  CALL SGESL( A, N_GC_SPC, N_GC_SPC, IPVT, PRD, 0 )

                  DO JSPC = 1, NSPC_SA
                     IF ( JSPC .EQ. JNO .OR. JSPC .EQ. JNO2 .OR.
     &                    JSPC .EQ. JNO3 .OR. JSPC .EQ. JHONO .OR. JSPC .EQ. JN2O5 .OR.
     &                    JSPC .EQ. JPNA .OR. JSPC .EQ. JPAN .OR. JSPC .EQ. JPANX .OR.
     &                    JSPC .EQ. JHNO3 .OR. JSPC .EQ. JNTR ) THEN
                       ! Updates species from nitrate tag class
                       ISAM( C,R,L,JSPC,KTAG ) = MAX( 0.0, PRD(SPC_INDEX(JSPC,2) ) )
                     ELSEIF ( JSPC .EQ. JVOC( 1 ) .OR. JSPC .EQ. JVOC( 2 )
     &    .OR. JSPC .EQ. JVOC( 3 ) .OR. JSPC .EQ. JVOC( 4 )
     &    .OR. JSPC .EQ. JVOC( 5 ) .OR. JSPC .EQ. JVOC( 6 )
     &    .OR. JSPC .EQ. JVOC( 7 ) .OR. JSPC .EQ. JVOC( 8 )
     &    .OR. JSPC .EQ. JVOC( 9 ) .OR. JSPC .EQ. JVOC(10 )
     &    .OR. JSPC .EQ. JVOC( 11) .OR. JSPC .EQ. JVOC(12 )
     &    .OR. JSPC .EQ. JVOC( 13) .OR. JSPC .EQ. JVOC(14 ) ) THEN
                       ISAM( C,R,L,JSPC,KTAG ) = MAX( 0.0, PRD(SPC_INDEX(JSPC,2) ) )
                     ELSE IF ( JSPC .EQ. JO3A ) THEN
                        ! Updates 1-ozone
                        ISAM( C,R,L,JSPC,KTAG ) = MAX( 0.0, PRD( SPC_INDEX( JSPC,2 ) ) )
                     ELSE IF ( JSPC .EQ. JCO ) THEN
                        ! Updates CO tag class
                        ISAM( C,R,L,JSPC,KTAG ) = MAX( 0.0, PRD( SPC_INDEX( JSPC,2 ) ) )
                     ELSE IF ( JSPC .EQ. JSO2 .OR. JSPC .EQ. JSULF .OR. JSPC .EQ. JSRXN ) THEN
                        ! Updates species from sulfate tag class
                        ISAM( C,R,L,JSPC,KTAG ) = MAX( 0.0, PRD( SPC_INDEX( JSPC,2 ) ) )
                     END IF ! jspc is from either tag classes
                  END DO   ! jspc

495            CONTINUE  ! ktag loop
#endif

            END DO   ! columns
         END DO   ! rows
      END DO   ! layers

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
