
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


       SUBROUTINE HRSOLVER( JDATE, JTIME, C, R, L )


C**********************************************************************
C
C  FUNCTION: EBI solver
C
C  PRECONDITIONS: For the CRACMM2 mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  HRRATES, HRG1, HRG2, HRG3
C                                     HRG4, HRPRODLOSS
C                                     DEGRADE
C
C  REVISION HISTORY: Created by EBI solver program, Mar 15, 2024
C   21 Jun 10 J.Young: convert for Namelist redesign
C   18 Jul 14 B.Hutzell: revised 1) to use the RXNS_DATA and RXNS_FUNCTION
C                        modules instead of include files and 2) to use
C                        real(8) variables
C**********************************************************************

      USE RUNTIME_VARS
      USE UTILIO_DEFN
      USE RXNS_DATA
      USE HRDATA
      USE PA_IRR_MODULE
#ifdef isam
      USE SA_IRR_DEFN
#endif
#ifdef sens
      USE DDM3D_CHEM, ONLY: YCDDM
      USE CGRID_SPCS, ONLY: GC_SPC
#endif
      USE DEGRADE_ROUTINES, ONLY : DEGRADE, SA_DEGRADE_STEP


      IMPLICIT NONE

C..INCLUDES:


C..ARGUMENTS:
      INTEGER, INTENT( IN ) :: JDATE    ! Current date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME    ! Current time (HHMMSS)
      INTEGER, INTENT( IN ) :: C, R, L  ! Cell col, row, lev

C..PARAMETERS:
      INTEGER, PARAMETER :: MXBKUPS = 5  ! Max no. of back-ups allowed
      INTEGER, PARAMETER :: STAT = 1     ! Status code

      REAL( 8 ), PARAMETER :: DTMIN   = 1.0D-08    ! Smallest time step allowed, min
      REAL( 8 ), PARAMETER :: EPSLON  = 1.0D-30    ! Small number
      REAL( 8 ), PARAMETER :: MAXPRED = 1.0D+03    ! Upper limit on predicted conc
      REAL( 8 ), PARAMETER :: ZERO    = 1.0D-40    ! effective zero
      REAL( 8 ), PARAMETER :: FLOOR   = 1.0D-08    ! Min conc for RTOL

C..EXTERNAL FUNCTIONS:


C..SAVED LOCAL VARIABLES:
      CHARACTER( 16 ),      SAVE :: PNAME  = 'HRSOLVER' ! Program name
      LOGICAL,              SAVE :: LFIRST = .TRUE.     ! Flag for first call
      LOGICAL, ALLOCATABLE, SAVE :: LEBISPFL( : )       ! Convergence Error Flag for EBI species
      LOGICAL, ALLOCATABLE, SAVE :: MAXCONC ( : )       ! MAXCONC ERROR Flag for EBI species
      LOGICAL, ALLOCATABLE, SAVE :: NOTMAX  ( : )       ! Initial concentration not greater than MAXPRED

      REAL( 8 ), ALLOCATABLE, SAVE :: RERROR  ( : )     ! Relative Error
      REAL( 8 ), ALLOCATABLE, SAVE :: AERROR  ( : )     ! Absolute Error


C..SCRATCH LOCAL VARIABLES:

      CHARACTER( 132 ) :: MSG           ! Message text

      INTEGER CELLNO          ! Cell no. fo debug output
      INTEGER ITER            ! Loop index for Backward Euler iterations
      INTEGER S               ! Loop index for species
      INTEGER SP              ! Imbedded loop index for species
      INTEGER NEBI            ! Loop index for time steps
      INTEGER NINR            ! No. of inner time steps
#ifdef hrdebug
      INTEGER N               ! Loop index
#endif
      INTEGER M               ! species index
      INTEGER EBI             ! Loop index
      INTEGER NBKUPS          ! No. of times time step reduced
      INTEGER ERR             ! Allocate error flag


      LOGICAL LEBI_CONV          ! Flag for EBI convergence
      LOGICAL MXFL               ! hit MAXPRED flag

      REAL( 8 ) DTC              ! Time step to take
      REAL( 8 ) DTG         ! Time step in degrade routines, sec
      REAL( 8 ) FXDLOSS          ! Total loss due to negative stoichiometry
      REAL( 8 ) VARLOSS          ! Loss excluding negative stoichiometry


#ifdef hrdebug
      CHARACTER( 8 ) :: NOTE  ! Convergence fail note

      INTEGER COL             ! Column to generate deboug output for
      INTEGER ROW             ! Row to generate deboug output for
      INTEGER LEV             ! Level to generate deboug output for
      INTEGER DBGOUT          ! Output unit for debu outpt

      LOGICAL LDEBUG          ! Debug output flag
      LOGICAL, SAVE  :: LOPEN = .FALSE.
#endif


C**********************************************************************


       IF( LFIRST ) THEN
          LFIRST = .FALSE.

          ALLOCATE ( LEBISPFL( NUMB_MECH_SPC ), STAT = ERR )
          IF ( ERR .NE. 0 ) THEN
             MSG = 'Error allocating LEBISPFL'
             CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
          END IF

          ALLOCATE ( MAXCONC( NUMB_MECH_SPC ), STAT = ERR )
          IF ( ERR .NE. 0 ) THEN
             MSG = 'Error allocating MAXCONC'
             CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
          END IF

          ALLOCATE ( NOTMAX( NUMB_MECH_SPC ), STAT = ERR )
          IF ( ERR .NE. 0 ) THEN
             MSG = 'Error allocating NOTMAX'
             CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
          END IF

          ALLOCATE ( RERROR( NUMB_MECH_SPC ), STAT = ERR )
          IF ( ERR .NE. 0 ) THEN
             MSG = 'Error allocating ERROR'
             CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
          END IF

          ALLOCATE ( AERROR( NUMB_MECH_SPC ), STAT = ERR )
          IF ( ERR .NE. 0 ) THEN
             MSG = 'Error allocating AERROR'
             CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
          END IF

       END IF

#ifdef sens
       YCDDM = 0.0D0
#endif

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
#ifdef hrdebug
      COL = 0
      ROW = 0
      LEV = 0
      IF( C .EQ. COL .AND. R .EQ. ROW .AND. L .EQ. LEV ) THEN
c      IF( JTIME .EQ. 160000 ) THEN
         LDEBUG = .TRUE.
      ELSE
         LDEBUG = .FALSE.
      END IF

      IF( LDEBUG ) THEN
           IF( .NOT. LOPEN ) THEN
              DBGOUT = JUNIT()
              OPEN( UNIT = DBGOUT, FILE = 'debug.out' )
              LOPEN = .TRUE.
           END IF

           WRITE( DBGOUT, '( A, 2I4, I3, 1X, I7, 1X, I6 ) ' )
     &             'Debug output for col/row/lev/date/time:',
     &              C, R, L, JDATE, JTIME
           WRITE( DBGOUT, '( A, F7.2) ' )
     &             'EBI_TMSTEP = ', EBI_TMSTEP
           WRITE( DBGOUT, '( A )' ) 'Starting concs and rate constants'
           DO N = 1, NUMB_MECH_SPC
             WRITE( DBGOUT,  '( A, I3, 1X, A, 1X, 1PD13.5 )' )
     &                     'SP ',N, CHEMISTRY_SPC( N ), YC( N )
           END DO
           DO N = 1, N_RXNS
             WRITE( DBGOUT, '( A, I3, 1X, 1PD13.5 )' )
     &                     'RKI ', N, RKI(  N )
           END DO
      END IF
#endif
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++



      DTC         = EBI_TMSTEP

      N_EBI_IT    = 0
      NBKUPS      = 0
      N_INR_STEPS = 1
      NEBI        = 1
      LEBI_CONV   = .TRUE.
#ifdef isam
      UPDATE_SOLD = .FALSE.
      UPDATE_PROBABILITIES = .TRUE.
      CALL SA_IRR_UNBLOCKED ( .TRUE., RKI, YC, DTC )
#endif


      DO S = 1, NUMB_MECH_SPC
         IF( YC( S ) .LE. MAXPRED )THEN
           NOTMAX( S ) = .TRUE.
         ELSE
           NOTMAX( S ) = .FALSE.
           WRITE(LOGDEV,91000)C, R, L, CHEMISTRY_SPC(S), YC(S)
         END IF
      END DO

! Initial PA_IRR
      IF( CALL_IRR ) CALL PA_IRR ( .TRUE., RKI, YC, DTC )
      SA_DEGRADE_STEP = 0
      TSTEP_EBI: DO    ! EBI time-step loop

         SUBSTEP_EBI: DO NINR = 1, N_INR_STEPS   ! time substeps loop
!  first attempt is sub time-step equals EBI time-step

            IF( LEBI_CONV ) YC0 = YC   ! Set ICs for EBI iterations

            ITER_SUBSTEP: DO ITER = 1, NEBITER  ! iteration loop solving for sub time-step


               N_EBI_IT = N_EBI_IT + 1
               CALL HRRATES

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
#ifdef hrdebug
               IF( LDEBUG ) THEN
                  WRITE( DBGOUT, '( A, I5 )' ) 'ITER NO ', ITER
                  WRITE( DBGOUT, '( A, F12.5 )' )
     &               ' DTC=', DTC

                  IF( ITER .EQ. 1 ) THEN
                     WRITE( DBGOUT, '( A )' ) 'Starting reaction rates'
                     DO N = 1, N_RXNS
                        WRITE( DBGOUT, '( A, I3, 1X, 1PD13.5 )' )
     &                        'RXRAT ', N, RXRAT( NCELL, N )
                     END DO
                  END IF
               END IF
#endif
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call routines to compute concentrations of groups 1-4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

               CALL HRG1( DTC )

               CALL HRG2( DTC )

               CALL HRG3( DTC )

               CALL HRG4( DTC )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the Euler backward method
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
               CALL HRPRODLOSS

               DO EBI = 1, N_EBISP
                  S = EBISP( EBI )
                  YCP( S ) =  YC( S )*( ( YC0( S ) + PROD( S ) * DTC )
     &                     / ( YC( S ) + LOSS( S ) * DTC ) )
               END DO




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check for convergence
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
               LEBI_CONV = .TRUE.
               MXFL      = .FALSE.

               DO S = 1, NUMB_MECH_SPC
                  LEBISPFL( S ) = .FALSE.
                  YCP( S ) = MAX( ZERO, YCP( S ) )
                  AERROR( S ) = MAX( ABS( YC(S)-YCP(S) ), EPSLON )
                  RERROR( S ) = AERROR( S ) / MAX( FLOOR, ABS( YC(S)+YCP(S) ) )
                  IF( RERROR( S ) .GT. RTOL(S) .OR. AERROR( S ) .GT. 1.0D-5 )THEN
                     LEBI_CONV     = .FALSE.
                     LEBISPFL( S ) = .TRUE.
                  END IF
c..test if predictions growing too large
                  IF( YCP( S ) .GT. MAXPRED .AND. NOTMAX( S ) ) THEN
                     MXFL          = .TRUE.
                     LEBI_CONV     = .FALSE.
                     LEBISPFL( S ) = .TRUE.
                  END IF
                  YC( S ) = YCP( S )
               END DO
c..test if predictions growing too large, abort interation loop
               IF( MXFL ) EXIT ITER_SUBSTEP

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
#ifdef hrdebug
               IF( LDEBUG ) THEN
                  WRITE( DBGOUT, '( A, I5 )' ) 'Concs after ITER= ', ITER
                  DO S = 1, NUMB_MECH_SPC

                     IF( LEBISPFL( S ) ) THEN
                        NOTE = 'CONV FAIL'
                     ELSE
                        NOTE = '         '
                     END IF

                     WRITE( DBGOUT, '( I3, 1X, A, 1PD13.5, 1X, A )' )
     &                            S, CHEMISTRY_SPC( S ), YC( S ), NOTE
                  END DO
                  IF( LEBI_CONV ) WRITE( DBGOUT, '( A )' )
     &                 '****Convergence achieved'
               END IF
#endif
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++

               IF( LEBI_CONV ) THEN

                DTG = 60.0D0 * DTC
                DO S = 1, NUMB_MECH_SPC
                   M = CGRID_INDEX( S )
                   YCCELL( M ) = YC( S )
                END DO
#if defined(isam) || defined(verbose_isam)
                SA_DEGRADE_STEP = SA_DEGRADE_STEP + 1
#endif
                IF( CALL_DEG )CALL DEGRADE(YCCELL, DTG, JDATE, JTIME) ! :WTH Call degradation routine

                 IF( CALL_IRR ) CALL PA_IRR ( .FALSE., RKI, YC, DTC )
#ifdef isam
                 IF( NEBI .EQ. N_EBI_STEPS )UPDATE_SOLD = .TRUE.
                 CALL SA_IRR_UNBLOCKED ( .FALSE., RKI, YC, DTC )
                 UPDATE_PROBABILITIES = .FALSE.
#endif

#ifdef sens
C Update the sum for the average over the chemistry integration
                 YCDDM = YCDDM
     &                 + ( ( 0.5D0 * DTC ) * ( YC + YC0 )  )
#endif

                 CYCLE SUBSTEP_EBI ! solve for next sub time-step

               END IF

            END DO ITER_SUBSTEP
! interating for substep failed, attempt to increase number of sub time-steps
            NBKUPS = NBKUPS + 1

!            IF( NBKUPS .LE. MXBKUPS ) THEN
            IF ( DTC .GT. DTMIN ) THEN
! reset YC and cut sub time-step in half
               IF ( MXFL ) THEN
                  WRITE( LOGDEV, 92008 ) NBKUPS
                  WRITE( LOGDEV, 92009 ) C+PECOL_OFFSET, R+PEROW_OFFSET, L
                  DO S = 1, NUMB_MECH_SPC
                     IF( LEBISPFL( S ) )WRITE( LOGDEV, 92010 )TRIM( CHEMISTRY_SPC( S ) ),
     &               YC0(S), YCP(S)
                  END DO
               ELSE
                  WRITE( LOGDEV, 92000 ) C+PECOL_OFFSET, R+PEROW_OFFSET, L, NBKUPS
                  DO S = 1, NUMB_MECH_SPC
                     IF( LEBISPFL( S ) )WRITE( LOGDEV, 92010 )TRIM( CHEMISTRY_SPC( S ) ),
     &               YC0(S), YCP(S)
                  END DO
               END IF

               YC = YC0   ! Set ICs for EBI time-step

               DTC = 0.5D0 * DTC

               N_INR_STEPS = 2 ** NBKUPS

               EXIT SUBSTEP_EBI

            ELSE

               WRITE( LOGDEV, 92040 ) C+PECOL_OFFSET, R+PEROW_OFFSET, L

               WRITE( LOGDEV, 92060 )
               DO S = 1, NUMB_MECH_SPC
                  IF( LEBISPFL( S ) ) WRITE( LOGDEV, 92080 ) CHEMISTRY_SPC( S ),
     &            YC0(S), YCP(S)
               END DO

               MSG = 'ERROR: Stopping because of EBI convergence failures'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )

            END IF

         END DO SUBSTEP_EBI

         IF( LEBI_CONV )THEN
             NEBI        = 1 + NEBI
! test for completing final EBI time-step
             IF( NEBI .GT. N_EBI_STEPS )EXIT TSTEP_EBI
! test whether backups were done
             IF( NBKUPS .EQ. 0 )CYCLE TSTEP_EBI
! Reset NBKUPS, N_INR_STEPS and sub-time step
             NBKUPS      = 0
             N_INR_STEPS = 1
             DTC         = EBI_TMSTEP
         END IF

      END DO TSTEP_EBI

#ifdef sens
! Compute average over integration and filter values
      YCDDM = YCDDM / CHEMSTEP
      WHERE (  YCDDM .LT. 1.0D-25 ) YCDDM = 0.0D0
#endif


      RETURN


91000 FORMAT( 'WARNING: EBI solver in cell (',2(I4,','),I4,') Init.Conc. for ',
     &         A16, ' = ', ES12.4,' ppmV')

92000 FORMAT( 'WARNING: EBI Euler convergence failure' /
     &        '         Reducing EBI time step because of ',
     &        '         convergence failure in ' /
     &        '         Cell (', I3, ', ', I3, ', ', I3, ')' ,
     &        '         Solution Attempt #', I2 /
     &        '         Below Species Causing Error: Init.Conc, Pred.Conc.'  )

92008 FORMAT( 'WARNING: At solution attempt #', I2  )
92009 FORMAT( 'WARNING: EBI Euler convergence failure' /
     &        '         Reducing EBI time step because of ',
     &        '         MAXPRED failure in ' /
     &        '         Cell (', I3, ', ', I3, ', ', I3, ')' ,
     &        '         for the below species: : Init.Conc, Pred.Conc.')
92010 FORMAT( A16, 2(1X,ES12.4), ' ppmV'  )

92040 FORMAT( 'ERROR: Max number of EBI time step reductions exceeded'
     &      / '      Convergence failure for cell (', I3, ', ', I3,
     &                ', ', I3, ')' )

92060 FORMAT( '      Convergence failure for the following species:',
     &        'Init.Conc, Pred.Conc.' )

92080 FORMAT( 10X, A, 2(1X,ES12.4), ' ppmV ' )

92061 FORMAT( '      Convergence failure for the following species:',
     &        'Init.Conc, Pred.Conc.,Rel.Error,' )

92081 FORMAT( 10X, A, 2(1X,ES12.4), ' ppmV ', ES12.4,'%' )

      END
