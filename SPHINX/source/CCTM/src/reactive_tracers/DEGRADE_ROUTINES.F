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

      MODULE DEGRADE_ROUTINES

         USE DEGRADE_SETUP_TOX

         IMPLICIT NONE

      CONTAINS



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
      REAL(8)        :: ISAM_INIT                ! sum of initial tag concentrations
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
         !TAG_CONMIN = CONMIN / REAL( NTAG_SA,8 )
      ENDIF

C..Update concentrations except degraded species

      LOOP_NEW: DO I = 1, I_SIZE
         DO J = 1, N_REACT
            IF( RXTANT_MAP( J ) .EQ. I )THEN
                CYCLE LOOP_NEW
            END IF
         ENDDO
         NEW_CONC( I ) = CBLK( I ) 
      ENDDO LOOP_NEW
      
      CHANGE_CONC = ZERO

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
         

         IF( NEW_CONC( I_RXT ) <= CONMIN )CYCLE LOOP_REACT


         LOOP_UNIRATE: DO J = UNI_START, UNI_STOP
             LOSS_RATE( J ) = CELL_RKI( J, I )
         ENDDO LOOP_UNIRATE

         L = 0

         LOOP_BIRATE: DO J = BI_START, BI_STOP
            L = L + 1
            I_RAD = RAD_MAP( L, I )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF( I_RAD < 9000 )THEN
                LOSS_RATE( J ) = 0.5D0 * CELL_RKI( J, I )
     &                         * ( OLD_CONC( I_RAD ) + NEW_CONC( I_RAD ) )
            ELSE
                LOSS_RATE( J ) = CELL_RKI( J, I )
            END IF
         ENDDO LOOP_BIRATE

         L = 0

         LOOP_TRIRATE: DO J = TRI_START, TRI_STOP
            L = L + 1
            I_RAD = RAD2_MAP( 1, L, I )
            J_RAD = RAD2_MAP( 2, L, I )
            IF ( I_RAD < 1 .OR. J_RAD < 1 ) CYCLE   ! radical species are undefined
            IF ( I_RAD > 9000 .AND. J_RAD < 9000 ) THEN
               LOSS_RATE( J ) = 0.5D0 * CELL_RKI( J, I )
     &                        * ( OLD_CONC( J_RAD ) + NEW_CONC( J_RAD ) )
            ELSE IF ( J_RAD > 9000 .AND. I_RAD < 9000 ) THEN
               LOSS_RATE( J ) = 0.5D0 * CELL_RKI( J, I )
     &                        * ( OLD_CONC( I_RAD ) + NEW_CONC( I_RAD ) )
            ELSE IF ( J_RAD < 9000 .AND. I_RAD < 9000 ) THEN
               LOSS_RATE( J ) = 0.5D0 * CELL_RKI( J, I )
     &                        * ( OLD_CONC( I_RAD ) * OLD_CONC( J_RAD )
     &                        +   NEW_CONC( I_RAD ) * NEW_CONC( J_RAD ) )
            ELSE 
               LOSS_RATE( J ) = CELL_RKI( J, I )
            END IF
         ENDDO LOOP_TRIRATE

         L = 0
         LOOP_PHOTORATE: DO J = PHOTO_START, PHOTO_STOP
            L = L + 1
            LOSS_RATE( J ) = CELL_RKI( J, I )
         ENDDO LOOP_PHOTORATE

         L = 0 

         LOOP_LHRATE: DO J = LANHIN_START, LANHIN_STOP
            L = L + 1
            I_RAD = RAD_MAP( L + N_BI_LOSS, I )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF ( I_RAD < 9000 )THEN
               EQU_FACTOR = 0.5D0 * CELL_RKI( J, I )
     &                    * ( OLD_CONC( I_RAD ) + NEW_CONC( I_RAD ) )
            ELSE
               EQU_FACTOR = CELL_RKI( J, I ) 
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
            TRANS = NEW_CONC( I_RXT ) * ( 1.0D0 - DEXP( -NET_RATE * DT ) )
         END IF

         IF ( TRANS > CONMIN ) THEN
             CHANGE_CONC( I_RXT ) = TRANS    
             IF( ANY( PROD_MAP( 1:N_PROCESSES, I ) > 0 ) )THEN
               LOOP_PROD: DO J = 1, N_PROCESSES
                  I_PROD = PROD_MAP( J, I )
                  IF( I_PROD > 0 )THEN
                    CHANGE_CONC( I_PROD ) = ( LOSS_RATE( J ) * NET_LIFE( I ) )
                  END IF
                END DO LOOP_PROD
             END IF
          END IF

      ENDDO LOOP_REACT

      OLD_CONC( 1:NSPCSD ) = NEW_CONC( 1:NSPCSD )

C..update concentrations

      LOOP_UPDATE1: DO I = 1, N_REACT
      
         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_UPDATE1

         IF ( CHANGE_CONC( I_RXT ) <= EFFECTIVE_ZERO .OR. NEW_CONC( I_RXT ) <= CONMIN ) CYCLE
         NEW_CONC( I_RXT ) = MAX( NEW_CONC( I_RXT )- CHANGE_CONC( I_RXT ),CONMIN )
         IF( ANY_PRODUCTS( I ) )THEN
            LOOP_UPDATE2: DO J = 1, N_PROCESSES
               I_PROD = PROD_MAP( J, I )
               IF ( I_PROD > 0 ) THEN ! specified product
                  CHANGE_CONC( I_PROD ) =  CHANGE_CONC( I_PROD )
     &                                *  MAX( OLD_CONC( I_RXT )-NEW_CONC( I_RXT ),
     &                                        ZERO ) 
                  NEW_CONC( I_PROD ) = NEW_CONC( I_PROD )
     &                                + RATE_YIELD( J, I )
     &                                * MAX( CHANGE_CONC( I_PROD ), CONMIN )
               END IF
            ENDDO LOOP_UPDATE2
         END IF
      ENDDO LOOP_UPDATE1
#if defined(isam) && defined(verbose_isam_deg)
      IF( DEG_LAY .EQ. 1 .AND. DEG_ROW .EQ. 1 .AND. DEG_COL .EQ. 1 )THEN
         WRITE(LOGDEV,'(/,A,I3)')'SA_DEGRADE_STEP: ',SA_DEGRADE_STEP
         WRITE(LOGDEV,'(/A18,(1X,A12),1X,A12,6(1X,A12))')'S ISAM_DEGRADED', 'FACTOR', 'REACT',
     &   'INIT_BULK','ISAM_INIT','FINI_BULK','ISAM_FINI','LIFE (days)'
      END IF
#endif      
#ifdef isam
      LOOP_ISAM: DO I = 1, ISAM_DEGRADED_SPC
      
         I_RXT = ISAM_TO_DEGRADED( I )
         I_RAD = ISAM_TO_REACTANT( I )
         IF( I_RXT .GT. 0 )THEN 

! scale loss of tag based on their fraction of the bulk
               FACTOR = NEW_CONC( I_RXT ) / MAX( OLD_CONC( I_RXT ),CONMIN )
#if defined(isam) && defined(verbose_isam_deg)
               ISAM_INIT = SUM(CELL_ISAM( 1:NTAG_SA,I))
#endif
               DO J = 1,  NTAG_SA
                  CELL_ISAM( J,I ) = CELL_ISAM( J,I ) * FACTOR
               END DO ! ktag loop
#if defined(isam) && defined(verbose_isam_deg)
               IF( DEG_LAY .EQ. 1 .AND. DEG_ROW .EQ. 1 .AND. DEG_COL .EQ. 1 )THEN
                  WRITE(LOGDEV,'(A2,A16,1X,ES12.4,1X,A16,6(1X,ES12.4))')'S ',ISAM_DEGRADED( I ), FACTOR, REACT( I_RAD ),
     &            OLD_CONC( I_RXT ),ISAM_INIT,
     &            NEW_CONC( I_RXT ),SUM(CELL_ISAM( 1:NTAG_SA,I)),
     &            1.15741D-5*NET_LIFE( I_RAD )
               END IF
#endif          
         END IF
      ENDDO LOOP_ISAM
#endif

      RETURN
      END SUBROUTINE DEGRADE

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
C $Header: /project/work/rep/arc/CCTM/src/gas/ros3/degrade.F,v 1.3 2011/10/21 16:11:12 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE DEGRADE_BLK( CBLK, DT, JDATE, JTIME, BLKID )
C**********************************************************************
C
C Function: Calculate changes in gas species based on a exponential decay.
C           The decay rate sums losses from processes in DEGRADE_DATA.
C
C CALLED BY: RBSOLVER or GRSMVGEAR
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
C
C**********************************************************************

      USE RXNS_DATA

      IMPLICIT NONE

C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK( :,  : ) ! array holding species concentrations
      REAL( 8 ), INTENT( IN ) :: DT            ! time step for integrations [sec]
      INTEGER,   INTENT( IN ) :: JDATE         ! current model date , coded YYYYDDD
      INTEGER,   INTENT( IN ) :: JTIME         ! current model time , coded HHMMSS
      INTEGER,   INTENT( IN ) :: BLKID         ! ID number for the BLK

C.....PARAMETERS:

      CHARACTER(16), PARAMETER :: PNAME = ' DEGRADE    '  ! name of routine calling I/OAPI

      INTEGER, PARAMETER :: LOCAL_DT = 3     ! minimum time step, mili-seconds

      REAL(8), PARAMETER :: CONMIN = 1.0D-30 ! concentration lower limit
      REAL(8), PARAMETER :: ZERO   = 0.00D00 ! concentration lower limit

C.....LOCAL VARIABLES:

      CHARACTER(16)  ::  VNAME                   ! variable name
      CHARACTER(120) ::  XMSG

      LOGICAL, SAVE  :: FIRSTCALL  = .TRUE.
      LOGICAL, SAVE  :: ANY_PRODUCTS( N_REACT )  ! does degraded species have any daughter products

      INTEGER        :: TIME_SECONDS                ! TIME, sec
      INTEGER        :: I_RXT, I_RAD, J_RAD, I_PROD ! indices
      INTEGER        :: I, J, K, L, I_CELL          ! loop counters
      INTEGER, SAVE  :: I_SIZE                      ! scratch

      REAL           ::  TSTEP                             ! time step for integrations
      REAL(8)        ::  TRANS    ( BLKSIZE )              ! molecules/cm^3 transferred to products
      REAL(8)        ::  NET_RATE ( BLKSIZE )              ! net rate of transfer   [sec^-1]
      REAL(8)        ::  NET_LIFE ( BLKSIZE )              ! lifetime based on net transfer rate  [sec]
      REAL(8)        ::  LOSS_RATE( BLKSIZE, N_PROCESSES ) ! individual loss rates  [sec^-1]
      
      REAL(8)        ::  FACTOR
      REAL(8)        ::  LIFETIME( N_REACT ) = 0.0D0

C***********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize maps
         I_SIZE = SIZE( CURR_CONC, 2 )
         FIRSTCALL = .FALSE.
         DO I = 1, N_REACT
           IF( ANY( PROD_MAP( 1:N_PROCESSES, I ) > 0 ) )THEN
              ANY_PRODUCTS( I ) = .TRUE.
           ELSE
              ANY_PRODUCTS( I ) = .FALSE.
           END IF
         END DO
      ENDIF

C..Initialize concentrations changes

      DELT_CONC = 0.0D0

C..Quality Control on time step

      TSTEP = DT
      BLOCK_A : IF ( TSTEP < 0.0D0 ) THEN
         WRITE(XMSG,'(A)')TRIM(' Time step has negative value. ')
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      ENDIF BLOCK_A

C..Update concentrations except degraded species

      LOOP_UPDATE0: DO J = 1, NSPCSD
         DO I = 1, N_REACT
            IF( RXTANT_MAP( I ) .EQ. J )CYCLE LOOP_UPDATE0
         END DO
         DO I_CELL = 1, NCELLS               
            CURR_CONC( I_CELL, J ) = CBLK( I_CELL, J )
         ENDDO
      ENDDO LOOP_UPDATE0

C..Loop over each reactant

      LOOP_REACT: DO I = 1, N_REACT

         I_RXT = RXTANT_MAP( I )
         
         IF( I_RXT < 0 )CYCLE LOOP_REACT


         LOSS_RATE = 0.0D0
         NET_RATE  = 0.0D0
         NET_LIFE  = 0.0D0

         LOOP_UNIRATE: DO J = UNI_START, UNI_STOP
            DO I_CELL = 1, NCELLS
               IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
               LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, J, I )
            ENDDO
         ENDDO LOOP_UNIRATE

         L = 0

         LOOP_BIRATE: DO J = BI_START, BI_STOP
            L = L + 1
            I_RAD = RAD_MAP( L, I )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF ( I_RAD > 9000 ) THEN
               DO I_CELL = 1, NCELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, J, I )
     &                                   * NUMB_DENS( I_CELL )
               ENDDO
            ELSE
               DO I_CELL = 1, NCELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) = 0.5D0 * RATE_CONST( I_CELL, J, I )
     &                                   * ( PREV_CONC( I_CELL, I_RAD )
     &                                   +   CURR_CONC( I_CELL, I_RAD ) )
               ENDDO
            ENDIF
         ENDDO LOOP_BIRATE

         L = 0

         LOOP_TRIRATE: DO J = TRI_START, TRI_STOP
            L = L + 1
            I_RAD = RAD2_MAP( 1, L, I )
            J_RAD = RAD2_MAP( 2, L, I )
            IF ( I_RAD < 1 .OR. J_RAD < 1 ) CYCLE   ! radical species are undefined
            IF ( I_RAD < 9000 .AND. J_RAD < 9000 ) THEN
               DO I_CELL = 1, NCELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) =  0.5D0 * RATE_CONST( I_CELL, J, I )
     &                                   * ( PREV_CONC( I_CELL, I_RAD )
     &                                   *   PREV_CONC( I_CELL, J_RAD )
     &                                   +   CURR_CONC( I_CELL, I_RAD )
     &                                   *   CURR_CONC( I_CELL, J_RAD ) )
               ENDDO
            ELSE IF ( J_RAD > 9000 .AND. I_RAD < 9000 ) THEN
               DO I_CELL = 1, NCELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) =  0.5D0 * RATE_CONST( I_CELL, J, I )
     &                                   * ( PREV_CONC( I_CELL, I_RAD )
     &                                   +   CURR_CONC( I_CELL, I_RAD ) )
               ENDDO
            ELSE IF ( J_RAD < 9000 .AND. I_RAD > 9000 ) THEN
               DO I_CELL = 1, NCELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) =  0.5D0 * RATE_CONST( I_CELL, J, I )
     &                                   * ( PREV_CONC( I_CELL, J_RAD )
     &                                   +   CURR_CONC( I_CELL, J_RAD ) )
               ENDDO
            END IF
         ENDDO LOOP_TRIRATE

         L = 0
         LOOP_PHOTORATE: DO J = PHOTO_START, PHOTO_STOP
            L = L + 1
            DO I_CELL = 1, NCELLS
               LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, J, I )
            ENDDO
         ENDDO LOOP_PHOTORATE

         L = 0
         LOOP_LHRATE: DO J = LANHIN_START, LANHIN_STOP
            L = L + 1
            I_RAD = RAD_MAP( L + N_BI_LOSS, I )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            DO I_CELL = 1, NCELLS
               IF ( I_RAD < 9000 )THEN
                   LOSS_RATE( I_CELL, J ) = 0.5D0 * RATE_CONST( I_CELL, J, I )
     &                                    * ( PREV_CONC( I_CELL, I_RAD ) + CURR_CONC( I_CELL, I_RAD ) )
               ELSE
                   LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, J, I ) 
               END IF
               LOSS_RATE( I_CELL, J ) = LHRATE( L, I ) * LOSS_RATE( I_CELL, J ) 
     &                                / ( 1.0D0 + LOSS_RATE( I_CELL, J ) )
            END DO
         ENDDO LOOP_LHRATE


         LOOP_RATE :  DO J = 1, N_PROCESSES
            DO I_CELL = 1, NCELLS
               NET_RATE( I_CELL ) = NET_RATE( I_CELL )
     &                            + LOSS_RATE( I_CELL, J )
            ENDDO
         ENDDO LOOP_RATE

         LOOP_LIFE: DO I_CELL = 1, NCELLS
            IF ( NET_RATE( I_CELL ) * DT .LE. EFFECTIVE_ZERO .OR. CURR_CONC( I_CELL, I_RXT ) <=  CONMIN ) THEN
               TRANS( I_CELL ) = ZERO
               IF( WRITE_CELL( I_CELL ) )LIFETIME( I ) = INFINITY
            ELSE
               NET_LIFE( I_CELL ) = 1.0D0 / NET_RATE( I_CELL )
               IF( WRITE_CELL( I_CELL ) )LIFETIME( I ) = NET_LIFE( I_CELL ) 
               TRANS( I_CELL ) = CURR_CONC( I_CELL, I_RXT )
     &                         * ( 1.0D0 - DEXP( - NET_RATE( I_CELL )*DT ) )
               DELT_CONC( I_CELL, I_RXT ) = - MAX( TRANS( I_CELL ), ZERO )
            END IF
         END DO LOOP_LIFE
         
         IF( ANY( PROD_MAP( 1:N_PROCESSES, I ) > 0 ) )THEN
            IF( ANY( DELT_CONC( :, I_RXT ) > -CONMIN ) )THEN
               LOOP_PROD: DO J = 1, N_PROCESSES
                  I_PROD = PROD_MAP( J, I )
                  IF( I_PROD > 0 )THEN
                    DO I_CELL = 1, NCELLS
                       DELT_CONC( I_CELL, I_PROD ) = ( LOSS_RATE( I_CELL, J ) * NET_LIFE( I_CELL ) )
                    END DO
                  END IF
               END DO LOOP_PROD
             END IF
          END IF
         
      ENDDO LOOP_REACT

C..update previous concentrations

      DO J = 1, NSPCSD
         DO I_CELL = 1, NCELLS
            PREV_CONC( I_CELL, J ) = CURR_CONC( I_CELL, J )
         ENDDO
      ENDDO

C..update current concentrations
#ifdef verbose_gas
      IF( WRITE_BLOCK )THEN
         DEGRADE_STEP = DEGRADE_STEP + 1
         WRITE(LOGDEV,'(/,A,I3)')'DEGRADE_STEP: ',DEGRADE_STEP
         WRITE(LOGDEV,'(/A18,4(1X,A18))')'S DEGRADED', 'FACTOR', 
     &   'PREV_CONC','CURR_CONC','LIFE (days)'
      END IF
#endif

      LOOP_UPDATE1: DO I = 1, N_REACT
      
         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_UPDATE1
         DO I_CELL = 1, NCELLS
            IF( -DELT_CONC( I_CELL, I_RXT ) > CONMIN .AND. CURR_CONC( I_CELL, I_RXT ) > CONMIN )THEN
              CURR_CONC( I_CELL, I_RXT  ) = MAX( (PREV_CONC( I_CELL, I_RXT  ) + DELT_CONC( I_CELL, I_RXT )),
     &                                            CONMIN )
            END IF
#ifdef verbose_gas
            IF( WRITE_CELL( I_CELL ) )THEN
                FACTOR = 1.0D0 + MIN( -1.0D0,( DELT_CONC( I_CELL,I_RXT )/PREV_CONC( I_CELL,I_RXT ) )  )
                WRITE(LOGDEV,'(A2,A16,5(1X,ES18.10))')'S ',REACT( I ), FACTOR, 
     &          PREV_CONC( I_CELL,I_RXT ),CURR_CONC( I_CELL,I_RXT ),1.15741D-5*LIFETIME( I ),DELT_CONC( I_CELL,I_RXT )
            END IF
#endif
         ENDDO

         IF( ANY_PRODUCTS( I ) )THEN
            LOOP_UPDATE2: DO J = 1, N_PROCESSES
               I_PROD = PROD_MAP( I, J )
               IF ( I_PROD > 0 ) THEN ! a specified product
                  DO I_CELL = 1, NCELLS
                       DELT_CONC( I_CELL, I_PROD ) = DELT_CONC( I_CELL, I_PROD )
     &                                             * MAX( PREV_CONC(I_CELL, I_RXT )-CURR_CONC( I_CELL, I_RXT ),
     &                                                    ZERO ) 
                       CURR_CONC( I_CELL, I_PROD ) = CURR_CONC( I_CELL, I_PROD )
     &                                             + RATE_YIELD( I, J ) * DELT_CONC( I_CELL, I_PROD )
                  ENDDO
               END IF
            END DO LOOP_UPDATE2
         END IF

      ENDDO LOOP_UPDATE1

      RETURN
      END SUBROUTINE DEGRADE_BLK

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


      SUBROUTINE FINAL_DEGRADE( CBLK )
C**********************************************************************
C
C  FUNCTION: Update CBLK concentrations with concentrations from degrade
C            routines
C
C  CALLED BY: HRDRIVER
C
C  REVISION HISTORY: 07/29/05 : B.Hutzell - Initial version
C                    09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                               optional degraded species i.e., RXTANT_MAP( I )
C                               is less than zero
C
C**********************************************************************


      IMPLICIT NONE

C.....ARGUMENTS:

      REAL( 8 ), INTENT( INOUT ) :: CBLK( : )    !  species concentration in cell

C.....LOCAL VARIABLES:

      REAL, PARAMETER ::  CONMIN = 1.0E-30

      INTEGER         ::  I_RXT, I_PROD   ! indices
      INTEGER         ::  I, J, K         ! loop counters

C**********************************************************************

      LOOP_REACT: DO I = 1, N_REACT ! Loop over each reactant

c..update CBLK

         I_RXT = RXTANT_MAP( I )

         IF( I_RXT <= 0 )CYCLE LOOP_REACT

         CBLK( I_RXT ) = NEW_CONC( I_RXT )

         LOOP_PROD: DO J = 1, N_PROCESSES

            I_PROD = PROD_MAP( J, I )

            IF( I_PROD < 1 ) CYCLE ! no specified product

            CBLK( I_PROD ) = NEW_CONC( I_PROD )

         ENDDO LOOP_PROD

      ENDDO LOOP_REACT

      END SUBROUTINE FINAL_DEGRADE


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

      SUBROUTINE FINAL_DEGRADE_BLK( CBLK )
C**********************************************************************
C
C  FUNCTION:  Update CBLK concentrations with concentrations from degrade
C             routines
C
C  CALLED BY: GRDRIVER or RBDRIVER
C
C  REVISION HISTORY: 07/29/05 : B.Hutzell - Initial version
C                    09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                               optional degraded species i.e., RXTANT_MAP( I )
C                               is less than zero
C
C
C**********************************************************************


      IMPLICIT NONE


C.....ARGUMENTS:

      REAL( 8 ),   INTENT( INOUT ) ::  CBLK( :, : ) ! species conc in cell

C.....LOCAL VARIABLES:

      REAL, PARAMETER ::  CONMIN = 1.0E-30

      INTEGER         ::  I_RXT, I_PROD     ! indices
      INTEGER         ::  I, J, K, I_CELL   ! loop counters

C**********************************************************************

      LOOP_BLOCK: DO I_CELL = 1, NCELLS

         LOOP_REACT: DO I = 1, N_REACT  ! Loop over each reactant

c..update CBLK

            I_RXT = RXTANT_MAP( I )

            IF( I_RXT < 0 )CYCLE LOOP_REACT

            CBLK( I_CELL, I_RXT ) = CURR_CONC( I_CELL, I_RXT )

            LOOP_PROD: DO J = 1, N_PROCESSES

               I_PROD = PROD_MAP( J, I )

               IF( I_PROD < 1 ) CYCLE   ! no specified product

               CBLK( I_CELL, I_PROD ) = CURR_CONC( I_CELL, I_PROD )

            ENDDO LOOP_PROD

         ENDDO LOOP_REACT

      ENDDO LOOP_BLOCK

      END SUBROUTINE FINAL_DEGRADE_BLK

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

         SUBROUTINE FIND_DEGRADED( JDATE, JTIME, CALL_DEGRADE )

C**********************************************************************
C
C  Function:  Set Logic Flag in whether to call degradation routines
C
C  CALLED BY: HRDRIVER
C
C**********************************************************************
 

           IMPLICIT NONE
 
C.....INCLUDES: NONE

C.....ARGUMENTS:

           INTEGER, INTENT( IN )  :: JDATE        ! current model date , coded YYYYDDD
           INTEGER, INTENT( IN )  :: JTIME        ! current model time , coded HHMMSS
           LOGICAL, INTENT( OUT ) :: CALL_DEGRADE ! whether to call degradation routines

C.....LOCAL VARIABLES:

           CHARACTER( 144 )        :: XMSG                    ! Message text
           CHARACTER( 16  ), SAVE  :: PNAME = 'FIND_DEGRADED' ! Routine name

           CALL DEGRADE_DATA()

           CALL DEGRADE_MAP( JDATE, JTIME )
           
           IF( N_REACT_FOUND .GT. 0 )THEN
               CALL_DEGRADE = .TRUE.
           ELSE
               CALL_DEGRADE = .FALSE.
           ENDIF
        
           RETURN
         
         END SUBROUTINE FIND_DEGRADED

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


      SUBROUTINE INIT_DEGRADE( CBLK, TCELL, DCELL, PRESS_CELL, QV_CELL, PHOTO_CELL,
     &                         JDATE, JTIME )
C**********************************************************************
C
C  FUNCTION:  Initialize arrays used by degrade routines then load
C             CBLK concentration needed in degrade routines.
C
C  CALLED BY: HRDRIVER
C
C  REVISION HISTORY:  07/29/05 : B.Hutzell - Initial version
C                     09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                                optional degraded species i.e., RXTANT_MAP( I )
C                                is less than zero
C
C**********************************************************************

      USE RUNTIME_VARS
      USE RXNS_DATA
      USE AERO_DATA
#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
#endif

      IMPLICIT NONE


C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK( : )                 !  species concentration in cell
      REAL,      INTENT( IN ) :: TCELL                     !  cell temperature  [ k ]
      REAL,      INTENT( IN ) :: DCELL                     !  cell air density  [ kg/m^3 ]
      REAL,      INTENT( IN ) :: PRESS_CELL                !  cell Pressure  [ Pa ]
      REAL,      INTENT( IN ) :: QV_CELL                   !  cell water vapor mass mixing ratio  [ kg/kg ]
      REAL( 8 ), INTENT( IN ) :: PHOTO_CELL( : )           !  Photolysis table for cell [ 1/min ]

      INTEGER, INTENT( IN ) :: JDATE  ! current model date , coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME  ! current model time , coded HHMMSS

C.....LOCAL VARIABLES:

      CHARACTER( 144 )        :: XMSG                   ! Message text
      CHARACTER( 16  ), SAVE  :: PNAME = 'INIT_DEGRADE' ! Routine name

      REAL(8), SAVE ::  MASS_TO_NUMBER ! air mass density( Kg/m3) to number density( #/cm3 ) [ (# per moles)/Kg ]

      REAL(8), SAVE ::  CONV_M2N       ! factor to convert ppm times mass density in [kg/m^3]
                                       ! into number density in [molecules/cm^3]
                                       
      REAL(8)       ::  PPM_2_NUMBER   ! conversion factor from ppm to molecules/cm^3
      REAL(8)       ::  INV_TAIR       ! reciprocal of temperature, [K^-1]
      REAL(8)       ::  FACTOR         ! scale factor
      

      INTEGER       :: I, J, K        ! loop counters
      INTEGER, SAVE :: ISIZE          ! dimension of CBLK array
      
      LOGICAL, SAVE ::  FIRSTCALL = .TRUE. 

      REAL, PARAMETER :: MAOMW   =  MWAIR / MWWAT ! Mol Wt of air over Mol Wt of water 
      
!   Murray, F.W. 1966. ``On the computation of Saturation Vapor Pressure,'
!   J. Appl. Meteor.,  6, p. 204.
!   esw (in mb) = 6.1078exp[ a(T-273.16)/ (T-b) ], 1 mb = 100 Pa
!     SVP1 => 610.78
!     SVP2 => a and SV3 => 35.85
!     over water  
      REAL, PARAMETER :: EP_2  = RDGAS / RWVAP
      REAL, PARAMETER :: SVP1  = 610.78         ! [ Pa ]
      REAL, PARAMETER :: SVP2  = 17.2693882
      REAL, PARAMETER :: SVP3  = 35.86

      REAL    :: ESW        ! water vapor liquid saturaturion vapor pressure (Pa)
      REAL    :: QVSW       ! water vapor saturation mixing ratio (Kg/Kg)
      REAL    :: RHUM       ! relative humidity (fraction)

      REAL    :: ORG_H2O    ! moles water in organic aeosol mass
      REAL    :: ORG_AERO   ! moles organic mass
      REAL    :: H2O_MOLAR_FRACTION ! molar fraction of water in organic aerosol mass

C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize constants and allocate arrays

         MASS_TO_NUMBER = REAL( 1.0E-3*AVO / MWAIR, 8 )
         
         CONV_M2N       = 1.0D-6 * MASS_TO_NUMBER

         ISIZE = SIZE( CBLK )
         
         ALLOCATE( OLD_CONC( ISIZE ) )
         ALLOCATE( NEW_CONC( ISIZE ) )
#ifdef sens
         ALLOCATE( SENS_CONC ( NPMAX, N_AE_SPC ) )
         SENS_CONC = 0.0
#endif
         ALLOCATE( CHANGE_CONC( ISIZE ) )
         ALLOCATE( IS_AERO_ORGANIC ( N_AEROSPC ) )
         ALLOCATE( CELL_RKI( N_PROCESSES, N_REACT ) )
         
         IS_AERO_ORGANIC( : ) = ( AEROSPC( : )%OM .AND. .NOT. AEROSPC( : )%TRACER )

         FIRSTCALL = .FALSE.

         EFFECTIVE_ZERO  = TINY( CONV_M2N )

      ENDIF

C..initialize concentrations and their changes
      CHANGE_CONC  = 0.0D0
      CELL_RKI = 0.0D0

      DO I = 1, ISIZE
         OLD_CONC( I ) = MAX( CBLK( I ), 0.0D0 )
         NEW_CONC( I ) = OLD_CONC( I )
      END DO
         

      CONC_AIR  = MASS_TO_NUMBER * REAL( DCELL, 8 )
      CONC_N2   = ATM_N2  * CONC_AIR  
      CONC_O2   = ATM_O2  * CONC_AIR  
      CONC_CH4  = ATM_CH4 * CONC_AIR  
      CONC_H2   = ATM_H2  * CONC_AIR  
      CONC_H2O  = MAOMW   * QV_CELL * CONC_AIR  
      
      TEMP_AIR = REAL( TCELL, 8 )

      PPM_2_NUMBER  = CONV_M2N * REAL( DCELL, 8 )
      INV_TAIR   = 1.0D0 / TEMP_AIR

      LOOP_REACT: DO I = 1, N_REACT ! calculated rate constants

         IF( RXTANT_MAP( I ) < 0 )CYCLE LOOP_REACT

         LOOP_UNIRATE: DO J = 1, N_UNI_LOSS
            IF( UNIRATE( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
            CELL_RKI( J, I ) = UNIRATE( J, I )
     &                         * TEMP_AIR**UNI_TEXP( J, I )
     &                         * DEXP( -UNI_ACT( J, I )*INV_TAIR )
         END DO LOOP_UNIRATE

         LOOP_BIRATE: DO J = 1, N_BI_LOSS
            IF( BIRATE( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
             IF ( RAD_MAP( J, I ) < 0 ) CYCLE
             SELECT CASE ( RAD_MAP( J, I ) )
               CASE ( 9999 )
                  FACTOR = CONC_AIR  
               CASE ( 9998 )
                  FACTOR = CONC_N2
               CASE ( 9997 )
                  FACTOR = CONC_O2
               CASE ( 9996 )
                  FACTOR = CONC_CH4
               CASE ( 9995 )
                  FACTOR = CONC_H2
               CASE ( 9994 )
                  FACTOR = CONC_H2O
               CASE DEFAULT
                  FACTOR = PPM_2_NUMBER 
             END SELECT 
             CELL_RKI( J+UNI_STOP, I  ) = FACTOR * BIRATE( J, I )
     &                                    * TEMP_AIR**BI_TEXP( J, I )
     &                                    * DEXP( -BI_ACT( J, I )*INV_TAIR )

         END DO LOOP_BIRATE

         LOOP_TRIRATE: DO J = 1, N_TRI_LOSS
            IF( TRIRATE( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
             IF ( RAD2_MAP( 1, J, I ) < 0 .OR. RAD2_MAP( 2, J, I ) < 0 ) CYCLE
             SELECT CASE ( RAD2_MAP( 1, J, I ) )
               CASE ( 9999 )
                  FACTOR = CONC_AIR  
               CASE ( 9998 )
                  FACTOR = CONC_N2
               CASE ( 9997 )
                  FACTOR = CONC_O2
               CASE ( 9996 )
                  FACTOR = CONC_CH4
               CASE ( 9995 )
                  FACTOR = CONC_H2
               CASE ( 9994 )
                  FACTOR = CONC_H2O
               CASE DEFAULT
                  FACTOR = PPM_2_NUMBER 
             END SELECT 
             SELECT CASE ( RAD2_MAP( 2, J, I ) )
               CASE ( 9999 )
                  FACTOR = FACTOR * CONC_AIR  
               CASE ( 9998 )
                  FACTOR = FACTOR * CONC_N2
               CASE ( 9997 )
                  FACTOR = FACTOR * CONC_O2
               CASE ( 9996 )
                  FACTOR = FACTOR * CONC_CH4
               CASE ( 9995 )
                  FACTOR = FACTOR * CONC_H2
               CASE ( 9994 )
                  FACTOR = FACTOR * CONC_H2O
               CASE DEFAULT
                  FACTOR = FACTOR * PPM_2_NUMBER 
             END SELECT
             CELL_RKI( J+BI_STOP, I ) = FACTOR * TRIRATE( J, I )
     &                                  * TEMP_AIR**TRI_TEXP( J, I )
     &                                  * DEXP( -TRI_ACT( J, I )*INV_TAIR )
         END DO LOOP_TRIRATE

         LOOP_PHOTORATE: DO J = 1, N_PHOTO_LOSS

            K = PHOTO_MAP( J, I )
            IF ( K < 1 ) CYCLE
            IF( A_PHOTO( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
            CELL_RKI( J+TRI_STOP, I ) = 60.0D0 * A_PHOTO( J, I )
     &                                  * PHOTO_CELL( K )

         END DO LOOP_PHOTORATE


         IF ( AE7ORGH2O ) THEN ! use water absorbed by organic mass
#ifdef sens
             CALL EXTRACT_AERO( REAL( CBLK( : ),4 ), .FALSE., SENS_CONC, .FALSE. )
#else
             CALL EXTRACT_AERO( REAL( CBLK( : ),4 ), .FALSE. )
#endif
             ORG_H2O  = SUM( AEROSPC_CONC( AORGH2O_IDX, 1:2 ) ) * AEROSPC_MWINV( AORGH2O_IDX )
             ORG_AERO = SUM( SUM( AEROSPC_CONC( :,1:2 ),2 ) * AEROSPC_MWINV( : ), 
     &                            MASK=IS_AERO_ORGANIC( : ) ) 
             H2O_MOLAR_FRACTION = ORG_H2O / MAX( (ORG_H2O + ORG_AERO), EFFECTIVE_ZERO )
         ELSE                  ! use relative humidity as surrogate
             ESW  = SVP1 * EXP( SVP2  * ( TCELL - STDTEMP ) / ( TCELL - SVP3  ) )
             QVSW = ( EP_2 * ESW ) / ( PRESS_CELL - ESW )
             RHUM = QV_CELL / QVSW                   
             H2O_MOLAR_FRACTION = RHUM
         END IF
         
         LOOP_LHRATE: DO J = 1, N_LANHIN_LOSS

            IF ( RAD_MAP( J + N_BI_LOSS, I ) < 0 ) CYCLE
            IF( LHRATE( J, I ) .GT. EFFECTIVE_ZERO 
     &             .AND. TCELL .GT. LH_TAMIN( J, I ) 
     &                 .AND. H2O_MOLAR_FRACTION .GT. LH_RHMIN( J, I )  )THEN
             SELECT CASE ( RAD_MAP( J + N_BI_LOSS, I ) )
               CASE ( 9999 )
                  FACTOR = CONC_AIR  
               CASE ( 9998 )
                  FACTOR = CONC_N2
               CASE ( 9997 )
                  FACTOR = CONC_O2
               CASE ( 9996 )
                  FACTOR = CONC_CH4
               CASE ( 9995 )
                  FACTOR = CONC_H2
               CASE ( 9994 )
                  FACTOR = CONC_H2O
               CASE DEFAULT
                  FACTOR = PPM_2_NUMBER 
             END SELECT 
             CELL_RKI( J+PHOTO_STOP, I ) = LH_EQU( J, I ) * FACTOR
            END IF

         END DO LOOP_LHRATE

      END DO LOOP_REACT

      RETURN

      END SUBROUTINE INIT_DEGRADE

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


      SUBROUTINE INIT_DEGRADE_BLK( CBLK, TCELL, DCELL, PRESS_CELL, H2O_CELL, PHOTO_CELL,
     &                         JDATE, JTIME )
C**********************************************************************
C
C  FUNCTION:  Initialize arrays used by degrade routines then load
C             CBLK concentration needed in degrade routines.
C
C  CALLED BY: GRDRIVER or RBDRIVER
C
C  REVISION HISTORY:  07/29/05 : B.Hutzell - Initial version
C                     09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                                optional degraded species i.e., RXTANT_MAP( I )
C                                is less than zero
C
C**********************************************************************

      USE RUNTIME_VARS
      USE RXNS_DATA
      USE AERO_DATA
#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
#endif

      IMPLICIT NONE


C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK( :,: )               !  species concentration in cell
      REAL( 8 ), INTENT( IN ) :: TCELL( : )                !  cell temperature  [ k ]
      REAL( 8 ), INTENT( IN ) :: DCELL( : )                !  cell air density  [ kg/m^3 ]
      REAL( 8 ), INTENT( IN ) :: PRESS_CELL( : )           !  cell Pressure  [ atm ]
      REAL( 8 ), INTENT( IN ) :: H2O_CELL( : )             !  cell water vapor mass mixing ratio  [ ppm ]
      REAL( 8 ), INTENT( IN ) :: PHOTO_CELL( :,: )         !  Photolysis table for cell [1/min]

      INTEGER, INTENT( IN ) :: JDATE  ! current model date , coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME  ! current model time , coded HHMMSS

C.....LOCAL VARIABLES:

      CHARACTER( 144 )        :: XMSG                   ! Message text
      CHARACTER( 16  ), SAVE  :: PNAME = 'INIT_DEGRADE' ! Routine name

      REAL(8), SAVE ::  MASS_TO_NUMBER ! air mass density( Kg/m3) to number density( #/cm3 ) [ (# per moles)/Kg ]

      REAL(8), SAVE ::  CONV_M2N       ! factor to convert ppm times mass density in [kg/m^3]
                                       ! into number density in [molecules/cm^3]
                                       
      REAL(8)       ::  FACTOR         ! scale factor
      

      INTEGER       :: I, J, K        ! loop counters
      INTEGER, SAVE :: ISIZE          ! dimension of CBLK array
      
      LOGICAL, SAVE ::  FIRSTCALL = .TRUE. 

      REAL,      PARAMETER :: MAOMW     =  MWAIR / MWWAT           ! Mol Wt of air over Mol Wt of water 
      REAL( 8 ), PARAMETER :: MWOMA     =  REAL( MWWAT / MWAIR,8 ) ! Mol Wt of water over Mol Wt of air 
      REAL( 8 ), PARAMETER :: ATM_TO_PA =  REAL( STDATMPA,8 )      ! Pascals per Atmosphere
      
!   Murray, F.W. 1966. ``On the computation of Saturation Vapor Pressure,'
!   J. Appl. Meteor.,  6, p. 204.
!   esw (in mb) = 6.1078exp[ a(T-273.16)/ (T-b) ], 1 mb = 100 Pa
!     SVP1 => 610.78
!     SVP2 => a and SV3 => 35.85
!     over water  
      REAL, PARAMETER :: EP_2  = RDGAS / RWVAP
      REAL, PARAMETER :: SVP1  = 610.78         ! [ Pa ]
      REAL, PARAMETER :: SVP2  = 17.2693882
      REAL, PARAMETER :: SVP3  = 35.86

      REAL    :: ESW        ! water vapor liquid saturaturion vapor pressure (Pa)
      REAL    :: QVSW       ! water vapor saturation mixing ratio (Kg/Kg)
      REAL    :: RHUM       ! relative humidity (fraction)

      REAL    :: ORG_H2O    ! moles water in organic aeosol mass
      REAL    :: ORG_AERO   ! moles organic mass
      REAL, ALLOCATABLE, SAVE :: H2O_MOLAR_FRACTION( : ) ! molar fraction of water in organic aerosol mass
      
      INTEGER :: ICELL

C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize constants and allocate arrays


         MASS_TO_NUMBER = REAL( 1.0E-3*AVO / MWAIR, 8 )
         
         CONV_M2N       = 1.0D-6 * MASS_TO_NUMBER

         ALLOCATE( PREV_CONC ( BLKSIZE, NSPCSD ) )
         ALLOCATE( CURR_CONC ( BLKSIZE, NSPCSD ) )
#ifdef sens
         ALLOCATE( SENS_BLK  ( BLKSIZE, NPMAX, N_AE_SPC ) )
         SENS_BLK = 0.0
#endif
         ALLOCATE( DELT_CONC ( BLKSIZE, NSPCSD ) )
         ALLOCATE( TEMP      ( BLKSIZE ) )
         ALLOCATE( INV_TEMP  ( BLKSIZE ) )
         ALLOCATE( NUMB_DENS ( BLKSIZE ) )
         ALLOCATE( NUMB_H2O  ( BLKSIZE ) )
         ALLOCATE( CONV_FACT ( BLKSIZE ) )
         ALLOCATE( WRITE_CELL( BLKSIZE ) )
         ALLOCATE( IS_AERO_ORGANIC ( N_AEROSPC ) )

         ALLOCATE( RATE_CONST( BLKSIZE, N_PROCESSES, N_REACT ) )

         ALLOCATE( H2O_MOLAR_FRACTION( BLKSIZE ) )
         
         IS_AERO_ORGANIC( : ) = ( AEROSPC( : )%OM .AND. .NOT. AEROSPC( : )%TRACER )

         INFINITY  = HUGE( CONV_M2N )
         EFFECTIVE_ZERO  = TINY( CONV_M2N )
         WRITE_CELL      = .FALSE.

         FIRSTCALL = .FALSE.
      ENDIF

C..initialize concentrations and their changes
      DELT_CONC  = 0.0D0
      RATE_CONST = 0.0D0
      DEGRADE_STEP = 0
      
      DO I = 1, NSPCSD
         PREV_CONC( :,I ) = MAX( CBLK( :,I ), 0.0D0 )
         CURR_CONC( :,I ) = PREV_CONC( :,I )
      END DO
         
      NUMB_DENS = MASS_TO_NUMBER * REAL( DCELL, 8 )
      NUMB_H2O  = 1.0D-6 * H2O_CELL * NUMB_DENS
      CONV_FACT = CONV_M2N * REAL( DCELL, 8 )
      
      TEMP =  REAL( TCELL, 8 )
      PRESS = ATM_TO_PA * PRESS_CELL

      WHERE( TEMP .GT. 0 )
         INV_TEMP = 1.0D0 / TEMP
      ELSE WHERE
         INV_TEMP = 0.0D0
      END WHERE

      LOOP_REACT: DO I = 1, N_REACT ! calculated rate constants

         IF( RXTANT_MAP( I ) < 0 )CYCLE LOOP_REACT

         LOOP_UNIRATE: DO J = 1, N_UNI_LOSS
            IF( UNIRATE( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
            LOOP_CELL: DO ICELL = 1, NCELLS
               RATE_CONST( ICELL, J, I ) = UNIRATE( J, I )
     &                                   * TEMP( ICELL )**UNI_TEXP( J, I )
     &                                   * DEXP( -UNI_ACT( J, I )*INV_TEMP( ICELL ) )
            END DO LOOP_CELL
         END DO LOOP_UNIRATE
#ifdef verbose_gas
         IF( WRITE_BLOCK )THEN
            WRITE(LOGDEV,'(A,6(1X,ES12.4))')'TEMP,NUMB,PRESS = ',TCELL(ICELL_WRITE),NUMB_DENS(ICELL_WRITE),
     &      PRESS(ICELL_WRITE)
            WRITE(LOGDEV,*)'UNIRATES for ',REACT(I)
            DO J = 1, N_UNI_LOSS
               WRITE(LOGDEV,*)J,RATE_CONST( ICELL_WRITE, J, I  )
            END DO
         END IF
#endif

         LOOP_BIRATE: DO J = 1, N_BI_LOSS
            IF( BIRATE( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
             IF ( RAD_MAP( J, I ) < 0 ) CYCLE
             SELECT CASE ( RAD_MAP( J, I ) )
               CASE ( 9999 )
                  FACTOR = ATM_AIR 
               CASE ( 9998 )
                  FACTOR = ATM_N2
               CASE ( 9997 )
                  FACTOR = ATM_O2
               CASE ( 9996 )
                  FACTOR = ATM_CH4
               CASE ( 9995 )
                  FACTOR = ATM_H2
               CASE DEFAULT
                  FACTOR = 1.0D0 
             END SELECT 
             IF ( RAD_MAP( J, I ) .EQ. 9994 ) THEN
                DO ICELL = 1, NCELLS
                   RATE_CONST( ICELL, J+UNI_STOP, I  ) = NUMB_H2O( ICELL ) * BIRATE( J, I )
     &                                                 * TEMP( ICELL )**BI_TEXP( J, I )
     &                                                 * DEXP( -BI_ACT( J, I )*INV_TEMP( ICELL ) )
                END DO
             ELSE
                DO ICELL = 1, NCELLS
                   RATE_CONST( ICELL, J+UNI_STOP, I  ) = FACTOR * CONV_FACT( ICELL ) * BIRATE( J, I )
     &                                                 * TEMP( ICELL )**BI_TEXP( J, I )
     &                                                 * DEXP( -BI_ACT( J, I )*INV_TEMP( ICELL ) )
                END DO      
             END IF
         END DO LOOP_BIRATE
#ifdef verbose_gas
         IF( WRITE_BLOCK )THEN
            WRITE(LOGDEV,'(A,6(1X,ES12.4))')'TEMP,NUMB,PRESS = ',TCELL(ICELL_WRITE),NUMB_DENS(ICELL_WRITE),
     &      PRESS(ICELL_WRITE)
            WRITE(LOGDEV,*)'BIRATES for ',REACT(I)
            DO J = 1, N_BI_LOSS
               WRITE(LOGDEV,*)J+UNI_STOP,RATE_CONST( ICELL_WRITE, J+UNI_STOP, I  )
            END DO
         END IF
#endif

         LOOP_TRIRATE: DO J = 1, N_TRI_LOSS
            IF( TRIRATE( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
             IF ( RAD2_MAP( 1, J, I ) < 0 .OR. RAD2_MAP( 2, J, I ) < 0 ) CYCLE
             SELECT CASE ( RAD2_MAP( 1, J, I ) )
               CASE ( 9999 )
                  FACTOR = ATM_AIR
               CASE ( 9998 )
                  FACTOR = ATM_N2
               CASE ( 9997 )
                  FACTOR = ATM_O2
               CASE ( 9996 )
                  FACTOR = ATM_CH4
               CASE ( 9995 )
                  FACTOR = ATM_H2
               CASE DEFAULT
                  FACTOR = 1.0D0
             END SELECT
             SELECT CASE ( RAD2_MAP( 2, J, I ) )
               CASE ( 9999 )
                  FACTOR = FACTOR
               CASE ( 9998 )
                  FACTOR = FACTOR * ATM_N2
               CASE ( 9997 )
                  FACTOR = FACTOR * ATM_O2
               CASE ( 9996 )
                  FACTOR = FACTOR * ATM_CH4
               CASE ( 9995 )
                  FACTOR = FACTOR * ATM_H2
               CASE DEFAULT
                  FACTOR = FACTOR 
             END SELECT
             IF ( RAD2_MAP( 1, J, I ) .EQ. 9994 .AND. RAD2_MAP( 2, J, I ) .NE. 9994 ) THEN
                DO ICELL = 1, NCELLS
                   RATE_CONST( ICELL, J+BI_STOP, I ) = FACTOR * TRIRATE( J, I )
     &                                               * NUMB_H2O( ICELL ) * CONV_FACT( ICELL )           
     &                                               * TEMP( ICELL )**TRI_TEXP( J, I )
     &                                               * DEXP( -TRI_ACT( J, I )*INV_TEMP( ICELL ) )
                END DO
             ELSE IF  ( RAD2_MAP( 1, J, I ) .NE. 9994 .AND. RAD2_MAP( 2, J, I ) .EQ. 9994 ) THEN
                DO ICELL = 1, NCELLS
                   RATE_CONST( ICELL, J+BI_STOP, I ) = FACTOR * TRIRATE( J, I )
     &                                               * NUMB_H2O( ICELL ) * CONV_FACT( ICELL )           
     &                                               * TEMP( ICELL )**TRI_TEXP( J, I )
     &                                               * DEXP( -TRI_ACT( J, I )*INV_TEMP( ICELL ) )
                END DO
             ELSE IF  ( RAD2_MAP( 1, J, I ) .EQ. 9994 .AND. RAD2_MAP( 2, J, I ) .EQ. 9994 ) THEN
                DO ICELL = 1, NCELLS
                   RATE_CONST( ICELL, J+BI_STOP, I ) = TRIRATE( J, I )
     &                                               * NUMB_H2O( ICELL ) * NUMB_H2O( ICELL )           
     &                                               * TEMP( ICELL )**TRI_TEXP( J, I )
     &                                               * DEXP( -TRI_ACT( J, I )*INV_TEMP( ICELL ) )
                END DO
             ELSE
                DO ICELL = 1, NCELLS
                   RATE_CONST( ICELL, J+BI_STOP, I ) = FACTOR * TRIRATE( J, I )
     &                                               * CONV_FACT( ICELL ) * CONV_FACT( ICELL )           
     &                                               * TEMP( ICELL )**TRI_TEXP( J, I )
     &                                               * DEXP( -TRI_ACT( J, I )*INV_TEMP( ICELL ) )
                END DO
             END IF
         END DO LOOP_TRIRATE
#ifdef verbose_gas
         IF( WRITE_BLOCK )THEN
            WRITE(LOGDEV,'(A,6(1X,ES12.4))')'TEMP,NUMB,PRESS = ',TCELL(ICELL_WRITE),NUMB_DENS(ICELL_WRITE),
     &      PRESS(ICELL_WRITE)
            WRITE(LOGDEV,*)'TRI_RATES for ',REACT(I)
            DO J = 1, N_TRI_LOSS
               WRITE(LOGDEV,*)J+BI_STOP,RATE_CONST( ICELL_WRITE, J+BI_STOP, I  )
            END DO
         END IF
#endif

         LOOP_PHOTORATE: DO J = 1, N_PHOTO_LOSS

            K = PHOTO_MAP( J, I )
            IF ( K < 1 ) CYCLE
            IF( A_PHOTO( J, I ) .LT. EFFECTIVE_ZERO )CYCLE
            DO ICELL = 1, NCELLS
               RATE_CONST( ICELL, J+TRI_STOP, I ) = 60.0D0 * A_PHOTO( J, I )
     &                                            * PHOTO_CELL( ICELL, K )
            END DO

         END DO LOOP_PHOTORATE
#ifdef verbose_gas
         IF( WRITE_BLOCK )THEN
            WRITE(LOGDEV,'(A,6(1X,ES12.4))')'TEMP,NUMB,PRESS = ',TCELL(ICELL_WRITE),NUMB_DENS(ICELL_WRITE),
     &      PRESS(ICELL_WRITE)
            WRITE(LOGDEV,*)'PHOTO_RATES for ',REACT(I)
            DO J = 1, N_PHOTO_LOSS
               WRITE(LOGDEV,*)J+TRI_STOP,RATE_CONST( ICELL_WRITE,J+TRI_STOP, I  )
            END DO
         END IF
#endif



         IF ( AE7ORGH2O ) THEN ! use water absorbed by organic mass
             DO ICELL = 1, NCELLS
#ifdef sens
                CALL EXTRACT_AERO( REAL( CBLK( ICELL,: ),4 ), .FALSE., SENS_BLK(ICELL,:,:), .FALSE.)
#else
 
                CALL EXTRACT_AERO( REAL( CBLK(ICELL,: ),4 ), .FALSE. )
#endif
                ORG_H2O  = SUM( AEROSPC_CONC( AORGH2O_IDX, 1:2 ) ) * AEROSPC_MWINV( AORGH2O_IDX )
                ORG_AERO = SUM( SUM( AEROSPC_CONC( :,1:2 ),2 ) * AEROSPC_MWINV( : ),
     &                          MASK=IS_AERO_ORGANIC( : ) )
                H2O_MOLAR_FRACTION( ICELL ) = ORG_H2O 
     &                                      / MAX( (ORG_H2O + ORG_AERO), EFFECTIVE_ZERO )
#ifdef verbose_gas
                IF( ICELL .EQ. ICELL_WRITE )THEN
                  WRITE(LOGDEV,'(A,6(1X,ES12.4))')'ORG_H2O, ORG_H2O,H2O_MOLAR_FRACTION = ',  
     &            ORG_H2O, ORG_H2O,H2O_MOLAR_FRACTION(ICELL)                    
                END IF 
#endif
             END DO
         ELSE                  ! use relative humidity as surrogate
             DO ICELL = 1, NCELLS
                ESW  = SVP1 * EXP( SVP2  * ( TEMP( ICELL ) - STDTEMP ) / ( TEMP( ICELL ) - SVP3  ) )
                QVSW = ( EP_2 * ESW ) / ( PRESS( ICELL ) - ESW )
                RHUM = ( 1.0D-6 * MWOMA * H2O_CELL( ICELL ) ) / QVSW                   
                H2O_MOLAR_FRACTION( ICELL ) = RHUM
             END DO             
         END IF
         
         LOOP_LHRATE: DO J = 1, N_LANHIN_LOSS

            IF ( RAD_MAP( J + N_BI_LOSS, I ) < 0 ) CYCLE
            IF( LHRATE( J, I ) .GT. EFFECTIVE_ZERO )THEN
               DO ICELL = 1, NCELLS
                  IF ( TCELL( ICELL ) .GT. LH_TAMIN( J, I ) 
     &                    .AND. H2O_MOLAR_FRACTION( ICELL ) .GT. LH_RHMIN( J, I )  ) THEN
                     SELECT CASE ( RAD_MAP( J, I ) )
                       CASE ( 9999 )
                          FACTOR = ATM_AIR
                       CASE ( 9998 )
                          FACTOR = ATM_N2
                       CASE ( 9997 )
                          FACTOR = ATM_O2
                       CASE ( 9996 )
                         FACTOR = ATM_CH4
                       CASE ( 9995 )
                         FACTOR = ATM_H2
                       CASE DEFAULT
                         FACTOR = 1.0D0 
                     END SELECT 
                     IF ( RAD_MAP( J, I ) .EQ. 9994 ) THEN
                        RATE_CONST( ICELL, J+PHOTO_STOP, I  ) = NUMB_H2O( ICELL ) * LH_EQU( J, I )
                     ELSE
                        RATE_CONST( ICELL, J+PHOTO_STOP, I  ) = FACTOR * CONV_FACT( ICELL ) * LH_EQU( J, I )
                     END IF
                  END IF                  
               END DO 
            END IF

         END DO LOOP_LHRATE
#ifdef verbose_gas
         IF( WRITE_BLOCK )THEN
            WRITE(LOGDEV,'(A,6(1X,ES12.4))')'TEMP,NUMB,PRESS,H2O = ',TCELL(ICELL_WRITE),NUMB_DENS(ICELL_WRITE),
     &      PRESS(ICELL_WRITE),NUMB_H2O(ICELL_WRITE)
            WRITE(LOGDEV,*)'LH_RATES for ',REACT(I)
            DO J = 1, N_LANHIN_LOSS
               WRITE(LOGDEV,'(A,1X,I3,6(1X,ES12.4))')'LH_RCONST:', J+PHOTO_STOP,RATE_CONST( ICELL_WRITE, J+PHOTO_STOP, I  )
            END DO
         END IF
#endif

      END DO LOOP_REACT

      RETURN

      END SUBROUTINE INIT_DEGRADE_BLK

      END MODULE DEGRADE_ROUTINES
