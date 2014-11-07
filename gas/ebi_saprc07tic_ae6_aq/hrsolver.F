
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE HRSOLVER( JDATE, JTIME, C, R, L )


C**********************************************************************
C
C  FUNCTION: EBI solver
C
C  PRECONDITIONS: For the SAPRC07TIC family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  HRRATES, HRG1, HRG2, HRG3
C                                     HRG4, HRPRODLOSS
C
C  REVISION HISTORY: Created by EBI solver program, Nov. 7, 2014
C                     Revised hrsolver.F by Golam Sarwar, December, 2007
C                             rearranged a statement suggested by CARB
C                             to reduce the chance of code crash; it does
C                             not change results
C   21 Jun 10 J.Young: convert for Namelist redesign
C   18 Jul 14 B.Hutzell: revised 1) to use the RXNS_DATA and RXNS_FUNCTION
C                        modules instead of include files and 2) to use
C                        real(8) variables
C**********************************************************************

      USE UTILIO_DEFN
      USE RXNS_DATA
      USE HRDATA

      IMPLICIT NONE

C..INCLUDES:


C..ARGUMENTS:
      INTEGER, INTENT( IN ) :: JDATE    ! Current date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME    ! Current time (HHMMSS)
      INTEGER, INTENT( IN ) :: C, R, L  ! Cell col, row, lev

C..PARAMETERS:
      INTEGER, PARAMETER :: MXBKUPS = 5  ! Max no. of back-ups allowed
      INTEGER, PARAMETER :: STAT = 1     ! Status code

      REAL( 8 ), PARAMETER :: EPSLON  = 1.0D-30    ! Small number
      REAL( 8 ), PARAMETER :: MAXPRED = 1.0D+03    ! Upper limit on predicted conc
      REAL( 8 ), PARAMETER :: ZERO    = 0.0D0      ! zero

C..EXTERNAL FUNCTIONS:


C..SAVED LOCAL VARIABLES:
      CHARACTER( 16 ),      SAVE :: PNAME  = 'HRSOLVER' ! Program name
      LOGICAL,              SAVE :: LFIRST = .TRUE.     ! Flag for first call
      LOGICAL, ALLOCATABLE, SAVE :: LEBISPFL( : )       ! Flag for EBI species


C..SCRATCH LOCAL VARIABLES:

      CHARACTER( 132 ) :: MSG           ! Message text

      INTEGER CELLNO          ! Cell no. fo debug output
      INTEGER ITER            ! Loop index for Backward Euler iterations
      INTEGER S               ! Loop index for species
      INTEGER NEBI            ! Loop index for time steps
      INTEGER NINR            ! No. of inner time steps
      INTEGER N               ! Loop index
      INTEGER EBI             ! Loop index
      INTEGER NBKUPS          ! No. of times time step reduced
      INTEGER ERR             ! Allocate error flag


      LOGICAL LEBI_CONV             ! Flag for EBI convergence
      LOGICAL MXFL                  ! hit MAXPRED flag

      REAL( 8 ) DTC              ! Time step to take
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
          LOGDEV = INIT3( )

          ALLOCATE ( LEBISPFL( NUMB_MECH_SPC ), STAT = ERR )
          IF ( ERR .NE. 0 ) THEN
             MSG = 'Error allocating LEBISPFL'
             CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
          END IF
       END IF


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
           DO N = 1, N_SPEC
             WRITE( DBGOUT,  '( A, I3, 1X, A, 1X, 1PE13.5 )' )
     &                     'SP ',N, CHEMISTRY_SPC( N ), YC( N )
           END DO
           DO N = 1, N_RXNS
             WRITE( DBGOUT, '( A, I3, 1X, 1PE13.5 )' )
     &                     'RKI ', N, RKI(  N )
           END DO
      END IF
#endif
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++



      N_EBI_IT = 0
      DTC = REAL( EBI_TMSTEP, 8 )


      DO 3000 NEBI = 1, N_EBI_STEPS    ! outer EBI time-tep loop

          NBKUPS = 0
         N_INR_STEPS = 1

100      CONTINUE                        !  Restart location

         DO 2000 NINR = 1, N_INR_STEPS   ! No. of time steps for back-up

            FORALL( S = 1:N_SPEC )YC0( S ) = YC( S )   ! Set ICs for EBI iterations

            DO 1000 ITER = 1, NEBITER    ! EBI iteration loop

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
                        WRITE( DBGOUT, '( A, I3, 1X, 1PE13.5 )' )
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
               MXFL = .FALSE.
               DO S = 1, N_SPEC
                  LEBISPFL( S ) = .FALSE.
                  YCP( S ) = MAX( ZERO, YCP( S ) )
                  IF( ABS( YC( S ) - YCP( S ) ) .GT. RTOL( S ) *
     &               ( YC( S ) + YCP( S ) ) ) THEN
                     LEBI_CONV = .FALSE.
                     LEBISPFL( S ) = .TRUE.
                  END IF
c..if predictions growing too large, treat as a convergence failure
                  IF( YCP( S ) .GT. MAXPRED ) then
                     MXFL = .TRUE.
                     GO TO 1010
                  END IF
                  YC( S ) = YCP( S )
               END DO

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
#ifdef hrdebug
               IF( LDEBUG ) THEN
                  WRITE( DBGOUT, '( A, I5 )' ) 'Concs after ITER= ', ITER
                  DO S = 1, N_SPEC

                     IF( LEBISPFL( S ) ) THEN
                        NOTE = 'CONV FAIL'
                     ELSE
                        NOTE = '         '
                     END IF

                     WRITE( DBGOUT, '( I3, 1X, A, 1PE13.5, 1X, A )' )
     &                            S, CHEMISTRY_SPC( S ), YC( S ), NOTE
                  END DO
                  IF( LEBI_CONV ) WRITE( DBGOUT, '( A )' )
     &                 '****Convergence achieved'
               END IF
#endif
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++


                IF( LEBI_CONV )THEN
                   GO TO 2000
                END IF

1000        CONTINUE

c...Convergence failure section; cut the inner time step in half &
c.....start inner loop over unless max backups exceeded

1010        CONTINUE

            NBKUPS = NBKUPS + 1

            IF( NBKUPS .LE. MXBKUPS ) THEN

               IF ( MXFL ) THEN
                  WRITE( LOGDEV, 92010 ) C, R, L, TRIM( CHEMISTRY_SPC( S ) ), NBKUPS
               ELSE
                  WRITE( LOGDEV, 92000 ) C, R, L, NBKUPS
               END IF

               DO S = 1, N_SPEC
                  YC( S ) = YC0( s )
               END DO

               DTC = 0.5D0 * DTC

               N_INR_STEPS = 2 ** NBKUPS

               GO TO 100

            ELSE

               WRITE( LOGDEV, 92040 ) C, R, L

               WRITE( LOGDEV, 92060 )
               DO S = 1, N_SPEC
                  IF( LEBISPFL( S ) ) WRITE( LOGDEV, 92080 ) CHEMISTRY_SPC( S )
               END DO

               MSG = 'ERROR: Stopping because of EBI convergence failures'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )

            END IF

2000     CONTINUE

3000  CONTINUE

      RETURN


92000 FORMAT( 'WARNING: EBI Euler convergence failure' /
     &        '         Reducing EBI time step because of ',
     &         'convergence failure for ' /
     &        '         Cell (', I3, ', ', I3, ', ', I3, ')' ,
     &        '  Back-up number', I2 )

92010 FORMAT( 'WARNING: EBI Euler convergence failure' /
     &        '         Reducing EBI time step because of ',
     &         'MAXPRED convergence failure for ' /
     &        '         Cell (', I3, ', ', I3, ', ', I3, ')' ,
     &        ' and species ', A,
     &        '  Back-up number', I2 )

92040 FORMAT( 'ERROR: Max number of EBI time step reductions exceeded'
     &      / '      Convergence failure for cell (', I3, ', ', I3,
     &                ', ', I3, ')' )

92060 FORMAT( '      Convergence failure for the following species:' )

92080 FORMAT( 10X, A )

      END
