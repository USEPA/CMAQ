
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


      SUBROUTINE DEGRADE( CBLK, DT, JDATE, JTIME, BLKID )
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
      USE DEGRADE_SETUP_TOX

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

C.....LOCAL VARIABLES:

      CHARACTER(16)  ::  VNAME                   ! variable name
      CHARACTER(120) ::  XMSG

      LOGICAL, SAVE  ::  FIRSTCALL  = .TRUE.

      INTEGER        :: TIME_SECONDS                ! TIME, sec
      INTEGER        :: I_RXT, I_RAD, J_RAD, I_PROD ! indices
      INTEGER        :: I, J, K, L, I_CELL          ! loop counters
      INTEGER, SAVE  :: I_SIZE                      ! scratch

      REAL           ::  TSTEP                             ! time step for integrations
      REAL(8)        ::  TRANS    ( BLKSIZE )              ! molecules/cm^3 transferred to products
      REAL(8)        ::  NET_RATE ( BLKSIZE )              ! net rate of transfer   [sec^-1]
      REAL(8)        ::  NET_LIFE ( BLKSIZE )              ! lifetime based on net transfer rate  [sec]
      REAL(8)        ::  LOSS_RATE( BLKSIZE, N_PROCESSES ) ! individual loss rates  [sec^-1]

C***********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize maps
         I_SIZE = SIZE( CURR_CONC, 2 )
         FIRSTCALL = .FALSE.
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
         DO I_CELL = 1, NUM_CELLS	       
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
            DO I_CELL = 1, NUM_CELLS
               IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
               LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, I, J )
            ENDDO
         ENDDO LOOP_UNIRATE

         L = 0

         LOOP_BIRATE: DO J = BI_START, BI_STOP
            L = L + 1
            I_RAD = RAD_MAP( I, L )
            IF ( I_RAD < 1 ) CYCLE   ! radical species is undefined
            IF ( I_RAD > 900 ) THEN
               DO I_CELL = 1, NUM_CELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, I, J )
     &                                   * NUMB_DENS( I_CELL )
               ENDDO
            ELSE
               DO I_CELL = 1, NUM_CELLS
                  IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
                  LOSS_RATE( I_CELL, J ) = 0.5D0 * RATE_CONST( I_CELL, I, J )
     &                                   * ( PREV_CONC( I_CELL, I_RAD )
     &                                   +   CURR_CONC( I_CELL, I_RAD ) )
               ENDDO
            ENDIF
         ENDDO LOOP_BIRATE

         L = 0

         LOOP_TRIRATE: DO J = TRI_START, TRI_STOP
            L = L + 1
            I_RAD = RAD2_MAP( I, L, 1 )
            J_RAD = RAD2_MAP( I, L, 2 )
            IF ( I_RAD < 1 .OR. J_RAD < 1 ) CYCLE   ! radical species are undefined
            DO I_CELL = 1, NUM_CELLS
               IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
               LOSS_RATE( I_CELL, J ) =  0.5D0 * RATE_CONST( I_CELL, I, J )
     &                                * ( PREV_CONC( I_CELL, I_RAD )
     &                                *   PREV_CONC( I_CELL, J_RAD )
     &                                +   CURR_CONC( I_CELL, I_RAD )
     &                                *   CURR_CONC( I_CELL, J_RAD ) )
            ENDDO
         ENDDO LOOP_TRIRATE

         L = 0
         LOOP_PHOTORATE: DO J = PHOTO_START, PHOTO_STOP
            L = L + 1
            DO I_CELL = 1, NUM_CELLS
               IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
               LOSS_RATE( I_CELL, J ) = RATE_CONST( I_CELL, I, J )
            ENDDO
         ENDDO LOOP_PHOTORATE

         LOOP_RATE :  DO J = 1, N_PROCESSES
            DO I_CELL = 1, NUM_CELLS
               IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
               NET_RATE( I_CELL ) = NET_RATE( I_CELL )
     &                            + LOSS_RATE( I_CELL, J )
            ENDDO
         ENDDO LOOP_RATE

         LOOP_LIFE: DO I_CELL = 1, NUM_CELLS
            IF( CURR_CONC( I_CELL, I_RXT ) <= CONMIN )CYCLE 
            IF ( NET_RATE( I_CELL ) * DT .LE. 1.0D-20 ) CYCLE
            NET_LIFE( I_CELL ) = 1.0D0 / NET_RATE( I_CELL )
            TRANS( I_CELL ) = CURR_CONC( I_CELL, I_RXT )
     &                      * ( 1.0D0 - DEXP( - NET_RATE( I_CELL )*DT ) )
     &                      - CONMIN
            DELT_CONC( I_CELL, I_RXT ) = - MAX( TRANS( I_CELL ), 0.0D0 )
         ENDDO LOOP_LIFE

         LOOP_PROD: DO J = 1, N_PROCESSES
            I_PROD = PROD_MAP( I, J )
            IF ( I_PROD < 1 ) CYCLE    !  No specified product
            DO I_CELL = 1, NUM_CELLS
              IF ( DELT_CONC( I_CELL, I_RXT ) >=  -CONMIN ) THEN
                  DELT_CONC( I_CELL, I_PROD ) = -(LOSS_RATE( I_CELL, J ) * NET_LIFE ( I_CELL ))
     &                                        *  DELT_CONC( I_CELL, I_RXT )
              ELSE
	          DELT_CONC( I_CELL, I_PROD ) = 0.0D0
              ENDIF
            ENDDO
         ENDDO LOOP_PROD

      ENDDO LOOP_REACT

C..update previous concentrations

      DO J = 1, NSPCSD
         DO I_CELL = 1, NUM_CELLS
            PREV_CONC( I_CELL, J ) = CURR_CONC( I_CELL, J )
         ENDDO
      ENDDO

C..update current concentrations

      LOOP_UPDATE1: DO I = 1, N_REACT
      
         I_RXT = RXTANT_MAP( I )
         IF( I_RXT < 0 )CYCLE LOOP_UPDATE1
         DO I_CELL = 1, NUM_CELLS
            IF ( DELT_CONC( I_CELL, I_RXT ) >= -CONMIN .OR. CURR_CONC( I_CELL, I_RXT ) <= CONMIN ) CYCLE
            CURR_CONC( I_CELL, I_RXT  ) = PREV_CONC( I_CELL, I_RXT  )
     &                                  + DELT_CONC( I_CELL, I_RXT  )
         ENDDO

         LOOP_UPDATE2: DO J = 1, N_PROCESSES
            I_PROD = PROD_MAP( I, J )
            IF ( I_PROD < 1 ) CYCLE ! no specified product
            DO I_CELL = 1, NUM_CELLS
                 CURR_CONC( I_CELL, I_PROD ) = CURR_CONC( I_CELL, I_PROD )
     &                                       + RATE_YIELD( I, J ) * DELT_CONC( I_CELL, I_PROD )
            ENDDO
         ENDDO LOOP_UPDATE2

      ENDDO LOOP_UPDATE1

      RETURN
      END
