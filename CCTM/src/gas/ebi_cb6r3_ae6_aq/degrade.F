
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
#ifdef isam
      USE SA_DEFN
#endif      

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
      REAL(8), PARAMETER :: ONE    = 1.0D0
      REAL(8), PARAMETER :: ZERO   = 0.0D0


C.....LOCAL VARIABLES:

      CHARACTER(16)  ::  VNAME                    ! variable name
      CHARACTER(120) ::  XMSG

      INTEGER        :: TIME_SECONDS                ! TIME, sec
      INTEGER        :: I_RXT, I_RAD, J_RAD, I_PROD ! indices
      INTEGER        :: I, J, K, L                  ! loop counters
      INTEGER, SAVE  :: I_SIZE                      ! scratch
      
      integer        :: icount

  
      LOGICAL, SAVE  :: FIRSTCALL  = .TRUE.
      LOGICAL, SAVE  :: ANY_PRODUCTS( N_REACT )  ! does degraded species have any daughter products
      
      REAL(8)        :: TRANS                    ! molecules/cm^3 transferred to products
      REAL(8)        :: LOSS_RATE( N_PROCESSES ) ! individual loss rates  [sec^-1]
      REAL(8)        :: NET_RATE                 ! net rate of transfer   [sec^-1]
      REAL(8)        :: NET_LIFE( N_REACT)       ! lifetime based on net transfer rate  [sec]
      REAL(8)        :: TSTEP                    ! degradation time step, sec
      REAL(8)       ::  EQU_FACTOR               ! equilibrium concentration, [ dimensionaless ] 
#ifdef isam
      REAL(8)        :: FACTOR                   ! relative change in bulk concentration
#endif      
C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize maps
         I_SIZE = SIZE( CBLK )
         FIRSTCALL = .FALSE.
 
         DO I = 1, N_REACT
           IF( ANY( PROD_MAP( 1:N_PROCESSES, I ) > 0 ) )THEN
              ANY_PRODUCTS( I ) = .TRUE.
           ELSE
              ANY_PRODUCTS( I ) = .FALSE.
           END IF
         END DO
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
      
      DELT_CONC = ZERO

C..Quality Control on time step

      TSTEP = DT
      BLOCK_A : IF ( TSTEP < 0.0D0 ) THEN
         XMSG = ' Time step has negative value. '
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      ENDIF BLOCK_A

C..Loop over each reactant

      LOOP_REACT: DO I = 1, N_REACT
      
         LOSS_RATE = ZERO
         NET_RATE  = ZERO
         NET_LIFE( I ) = ZERO

         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_REACT
         

         IF( CURR_CONC( I_RXT ) <= CONMIN )CYCLE LOOP_REACT


         LOOP_UNIRATE: DO J = UNI_START, UNI_STOP
             LOSS_RATE( J ) = RATE_CONST( J, I )
         ENDDO LOOP_UNIRATE

         L = 0

         LOOP_BIRATE: DO J = BI_START, BI_STOP
            L = L + 1
            I_RAD = RAD_MAP( L, I )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF( I_RAD < 9000 )THEN
                LOSS_RATE( J ) = 0.5D0 * RATE_CONST( J, I )
     &                         * ( PREV_CONC( I_RAD ) + CURR_CONC( I_RAD ) )
            ELSE
                LOSS_RATE( J ) = RATE_CONST( J, I )
            END IF
         ENDDO LOOP_BIRATE

         L = 0

         LOOP_TRIRATE: DO J = TRI_START, TRI_STOP
            L = L + 1
            I_RAD = RAD2_MAP( 1, L, I )
            J_RAD = RAD2_MAP( 2, L, I )
            IF ( I_RAD < 1 .OR. J_RAD < 1 ) CYCLE   ! radical species are undefined
            IF ( I_RAD > 9000 .AND. J_RAD < 9000 ) THEN
               LOSS_RATE( J ) = 0.5D0 * RATE_CONST( J, I )
     &                        * ( PREV_CONC( J_RAD ) + CURR_CONC( J_RAD ) )
            ELSE IF ( J_RAD > 9000 .AND. I_RAD < 9000 ) THEN
               LOSS_RATE( J ) = 0.5D0 * RATE_CONST( J, I )
     &                        * ( PREV_CONC( I_RAD ) + CURR_CONC( I_RAD ) )
            ELSE IF ( J_RAD < 9000 .AND. I_RAD < 9000 ) THEN
               LOSS_RATE( J ) = 0.5D0 * RATE_CONST( J, I )
     &                        * ( PREV_CONC( I_RAD ) * PREV_CONC( J_RAD )
     &                        +   CURR_CONC( I_RAD ) * CURR_CONC( J_RAD ) )
            ELSE 
               LOSS_RATE( J ) = RATE_CONST( J, I )
            END IF
         ENDDO LOOP_TRIRATE

         L = 0
         LOOP_PHOTORATE: DO J = PHOTO_START, PHOTO_STOP
            L = L + 1
            LOSS_RATE( J ) = RATE_CONST( J, I )
         ENDDO LOOP_PHOTORATE

         L = 0 

         LOOP_LHRATE: DO J = LANHIN_START, LANHIN_STOP
            L = L + 1
            I_RAD = RAD_MAP( L + N_BI_LOSS, I )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF ( I_RAD < 9000 )THEN
               EQU_FACTOR = 0.5D0 * RATE_CONST( J, I )
     &                    * ( PREV_CONC( I_RAD ) + CURR_CONC( I_RAD ) )
            ELSE
               EQU_FACTOR = RATE_CONST( J, I ) 
            END IF
            LOSS_RATE( J ) = LHRATE( L, I ) * EQU_FACTOR 
     &                     / ( 1.0D0 + EQU_FACTOR )
         ENDDO LOOP_LHRATE

         LOOP_RATE :  DO J = 1, N_PROCESSES
               NET_RATE = NET_RATE + LOSS_RATE( J )
         ENDDO LOOP_RATE
                  
         
         IF ( NET_RATE * DT .LE. EFFECTIVE_ZERO ) THEN
            TRANS = 0.0D0
         ELSE
            NET_LIFE( I ) = 1.0D0 / NET_RATE         
            TRANS = CURR_CONC( I_RXT ) * ( 1.0D0 - DEXP( -NET_RATE * DT ) )
         END IF

         IF ( TRANS > CONMIN ) THEN
             DELT_CONC( I_RXT ) = TRANS    
             IF( ANY( PROD_MAP( 1:N_PROCESSES, I ) > 0 ) )THEN
               LOOP_PROD: DO J = 1, N_PROCESSES
                  I_PROD = PROD_MAP( J, I )
                  IF( I_PROD > 0 )THEN
                    DELT_CONC( I_PROD ) = ( LOSS_RATE( J ) * NET_LIFE( I ) )
                  END IF
                END DO LOOP_PROD
             END IF
          END IF

      ENDDO LOOP_REACT

      PREV_CONC( 1:NSPCSD ) = CURR_CONC( 1:NSPCSD )

C..update concentrations

      LOOP_UPDATE1: DO I = 1, N_REACT
      
         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_UPDATE1

         IF ( DELT_CONC( I_RXT ) <= EFFECTIVE_ZERO .OR. CURR_CONC( I_RXT ) <= CONMIN ) CYCLE
         CURR_CONC( I_RXT ) = MAX( CURR_CONC( I_RXT )- DELT_CONC( I_RXT ),CONMIN )
         IF( ANY_PRODUCTS( I ) )THEN
            LOOP_UPDATE2: DO J = 1, N_PROCESSES
               I_PROD = PROD_MAP( J, I )
               IF ( I_PROD > 0 ) THEN ! specified product
                  DELT_CONC( I_PROD ) =  DELT_CONC( I_PROD )
     &                                *  MAX( PREV_CONC( I_RXT )-CURR_CONC( I_RXT ),
     &                                        ZERO ) 
                  CURR_CONC( I_PROD ) = CURR_CONC( I_PROD )
     &                                + RATE_YIELD( J, I )
     &                                * MAX( DELT_CONC( I_PROD ), CONMIN )
               END IF
            ENDDO LOOP_UPDATE2
         END IF
      ENDDO LOOP_UPDATE1
#if defined(isam) && defined(verbose_isam)
      IF( DEG_LAY .EQ. 1 .AND. DEG_ROW .EQ. 1 .AND. DEG_COL .EQ. 1 )THEN
         WRITE(LOGDEV,'(/,A,I3)')'SA_DEGRADE_STEP: ',SA_DEGRADE_STEP
         WRITE(LOGDEV,'(/A18,(1X,A12),1X,A18,4(1X,A18))')'S ISAM_DEGRADED', 'FACTOR', 'REACT',
     &   'PREV_CONC','CURR_CONC','ISAM_INIT','LIFE (days)'
      END IF
#endif      
#ifdef isam
      LOOP_ISAM: DO I = 1, ISAM_DEGRADED_SPC
      
         I_RXT = ISAM_TO_DEGRADED( I )
         I_RAD = ISAM_TO_REACTANT( I )
         IF( I_RXT .GT. 0 )THEN 

#if defined(isam) && defined(verbose_isam)
             IF ( DELT_CONC( I_RXT ) .GT. EFFECTIVE_ZERO .AND. PREV_CONC( I_RXT ) .GT. CONMIN )THEN
! scale loss of tag based on their fraction of the bulk
               FACTOR = 1.0D0 - MIN( 1.0D0,( DELT_CONC( I_RXT )/PREV_CONC( I_RXT ) )  )
               IF( DEG_LAY .EQ. 1 .AND. DEG_ROW .EQ. 1 .AND. DEG_COL .EQ. 1 )THEN
                  WRITE(LOGDEV,'(A2,A16,1X,ES18.10,1X,A16,4(1X,ES18.10))')'S ',ISAM_DEGRADED( I ), FACTOR, REACT( I_RAD ),
     &            PREV_CONC( I_RXT ),CURR_CONC( I_RXT ),SUM(CONC_ISAM( 1:NTAG_SA,I)),1.15741D-5*NET_LIFE( I_RAD )
               END IF
               DO J = 1,  NTAG_SA
                  CONC_ISAM( J,I ) = MAX( CONC_ISAM( J,I ) * FACTOR,0.0D0 )
               END DO ! ktag loop
             END IF
#endif          
         END IF
      ENDDO LOOP_ISAM
#endif

      RETURN
      END
