
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


      SUBROUTINE DEGRADE( CBLK, DT, JDATE, JTIME )

C**********************************************************************
C
C Function: Calculate changes in gas species based on a exponential decay.
C           The decay rate sums losses from processes in DEGRADE_DATA.
C
C CALLED BY: HRSOLVER
C
C WARNING: THIS ROUTINE ASSUMES SIMPLE AND LINEAR TRANSFORMATIONS FROM
C          ATMOSPHERIC CHEMISTRY.
C
C Species being degraded are governed by the equation,
C     dx/dt = -b*x, where b is the sum of N loss rates
C
C IT DOES NOT SOLVE A SYSTEM OF ODE's AS IN SMVGEAR, ROS3, and EBI SOLVERS.
C
C  REVISION HISTORY:  07/29/05 : B.Hutzell - Initial version
C                     09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                                optional degraded species i.e., RXTANT_MAP( I )
C                                is less than zero
C**********************************************************************

      USE RXNS_DATA
      USE DEGRADE_SETUP_TOX

      IMPLICIT NONE

C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK( : )      ! array holding species concentrations
      REAL( 8 ), INTENT( IN ) :: DT             ! time step for integrations [sec]
      INTEGER,   INTENT( IN ) :: JDATE          ! current model date , coded YYYYDDD
      INTEGER,   INTENT( IN ) :: JTIME          ! current model time , coded HHMMSS

C.....PARAMETERS:

      CHARACTER(16), PARAMETER :: PNAME  = ' DEGRADE    '  ! name of routine calling I/OAPI

      INTEGER, PARAMETER :: LOCAL_DT = 3     ! minimum time step, mili-seconds

      REAL(8), PARAMETER :: CONMIN = 1.0D-30 ! concentration lower limit

C.....LOCAL VARIABLES:

      CHARACTER(16)  ::  VNAME                    ! variable name
      CHARACTER(120) ::  XMSG

      INTEGER        :: TIME_SECONDS                ! TIME, sec
      INTEGER        :: I_RXT, I_RAD, J_RAD, I_PROD ! indices
      INTEGER        :: I, J, K, L                  ! loop counters
      INTEGER, SAVE  :: I_SIZE                      ! scratch

      LOGICAL, SAVE  :: FIRSTCALL  = .TRUE.

      REAL(8)        :: TRANS                    ! molecules/cm^3 transferred to products
      REAL(8)        :: LOSS_RATE( N_PROCESSES ) ! individual loss rates  [sec^-1]
      REAL(8)        :: NET_RATE                 ! net rate of transfer   [sec^-1]
      REAL(8)        :: NET_LIFE                 ! lifetime based on net transfer rate  [sec]
      REAL(8)        :: TSTEP                    ! degradation time step, sec

C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize maps
         I_SIZE = SIZE( CBLK )
         FIRSTCALL = .FALSE.
      ENDIF

C..Update concentrations except degraded species

      LOOP_NEW: DO I = 1, I_SIZE
         DO J = 1, N_REACT
            IF( RXTANT_MAP( J ) .EQ. I )THEN
                CYCLE LOOP_NEW
            END IF
         ENDDO
         CURR_CONC( I ) = CBLK( I ) 
      ENDDO LOOP_NEW
      
      DELT_CONC = 0.0D0

C..Quality Control on time step

      TSTEP = DT
      BLOCK_A : IF ( TSTEP < 0.0D0 ) THEN
         XMSG = ' Time step has negative value. '
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      ENDIF BLOCK_A

C..Loop over each reactant

      LOOP_REACT: DO I = 1, N_REACT
      

         LOSS_RATE = 0.0D0
         NET_RATE  = 0.0D0
         NET_LIFE  = 0.0D0

         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_REACT
         

         IF( CURR_CONC( I_RXT ) <= CONMIN )CYCLE LOOP_REACT

         LOOP_UNIRATE: DO J = UNI_START, UNI_STOP
             LOSS_RATE( J ) = RATE_CONST( I, J )
         ENDDO LOOP_UNIRATE

         L = 0

         LOOP_BIRATE: DO J = BI_START, BI_STOP
            L = L + 1
            I_RAD = RAD_MAP( I, L )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF ( I_RAD > 900 ) THEN
               LOSS_RATE( J ) = RATE_CONST( I, J )
     &                        * NUMB_DENS
            ELSE
               LOSS_RATE( J ) = 0.5D0 * RATE_CONST( I, J )
     &                        * ( PREV_CONC( I_RAD ) + CURR_CONC( I_RAD ) )
            ENDIF
         ENDDO LOOP_BIRATE

         L = 0

         LOOP_TRIRATE: DO J = TRI_START, TRI_STOP
            L = L + 1
            I_RAD = RAD2_MAP( I, L, 1 )
            J_RAD = RAD2_MAP( I, L, 2 )
            IF ( I_RAD < 1 .OR. J_RAD < 1 ) CYCLE   ! radical species are undefined
            LOSS_RATE( J ) = 0.5D0 * RATE_CONST( I, J )
     &                     * ( PREV_CONC( I_RAD ) * PREV_CONC( J_RAD )
     &                     +   CURR_CONC( I_RAD ) * CURR_CONC( J_RAD ) )
         ENDDO LOOP_TRIRATE

         L = 0
         LOOP_PHOTORATE: DO J = PHOTO_START, PHOTO_STOP
            L = L + 1
            LOSS_RATE( J ) = RATE_CONST( I, J )
         ENDDO LOOP_PHOTORATE

         LOOP_RATE :  DO J = 1, N_PROCESSES
               NET_RATE = NET_RATE + LOSS_RATE( J )
         ENDDO LOOP_RATE
                  
         IF ( NET_RATE * DT .LE. 1.0D-20 ) CYCLE LOOP_REACT

         NET_LIFE = 1.0D0 / NET_RATE         
         
         TRANS = CURR_CONC( I_RXT ) * ( 1.0D0 - DEXP( -NET_RATE * DT ) )

         IF ( TRANS <= CONMIN ) CYCLE LOOP_REACT

         DELT_CONC( I_RXT ) = TRANS

         LOOP_PROD: DO J = 1, N_PROCESSES
            I_PROD = PROD_MAP( I, J )
            IF ( I_PROD < 1 ) CYCLE ! no specified product
            DELT_CONC( I_PROD ) = ( LOSS_RATE( J ) * NET_LIFE )
     &                          * TRANS
         ENDDO LOOP_PROD

      ENDDO LOOP_REACT

      PREV_CONC( 1:NSPCSD ) = CURR_CONC( 1:NSPCSD )

C..update concentrations

      LOOP_UPDATE1: DO I = 1, N_REACT
      
         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_UPDATE1

         IF ( DELT_CONC( I_RXT ) <= CONMIN .OR. CURR_CONC( I_RXT ) <= CONMIN ) CYCLE
         CURR_CONC( I_RXT ) = MAX( CURR_CONC( I_RXT ) - DELT_CONC( I_RXT ), CONMIN )

         LOOP_UPDATE2: DO J = 1, N_PROCESSES
            I_PROD = PROD_MAP( I, J )
            IF ( I_PROD < 1 ) CYCLE ! no specified product
            CURR_CONC( I_PROD ) = CURR_CONC( I_PROD )
     &                          + RATE_YIELD( I, J )
     &                          * MAX( DELT_CONC( I_PROD ), CONMIN )
         ENDDO LOOP_UPDATE2

      ENDDO LOOP_UPDATE1

      RETURN
      END
