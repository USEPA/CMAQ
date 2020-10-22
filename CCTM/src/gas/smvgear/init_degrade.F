
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


      SUBROUTINE INIT_DEGRADE( CBLK, TCELL, DCELL, PRESS_CELL, H2O_CELL, PHOTO_CELL,
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
      USE DEGRADE_SETUP_TOX

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
         ALLOCATE( DELT_CONC ( BLKSIZE, NSPCSD ) )
         ALLOCATE( TEMP      ( BLKSIZE ) )
         ALLOCATE( INV_TEMP  ( BLKSIZE ) )
         ALLOCATE( NUMB_DENS ( BLKSIZE ) )
         ALLOCATE( NUMB_H2O  ( BLKSIZE ) )
         ALLOCATE( CONV_FACT ( BLKSIZE ) )
         ALLOCATE( IS_AERO_ORGANIC ( N_AEROSPC ) )

         ALLOCATE( RATE_CONST( BLKSIZE, N_PROCESSES, N_REACT ) )

         ALLOCATE( H2O_MOLAR_FRACTION( BLKSIZE ) )
         
         IS_AERO_ORGANIC( : ) = ( AEROSPC( : )%OM .AND. .NOT. AEROSPC( : )%TRACER )

         INFINITY  = HUGE( CONV_M2N )
         EFFECTIVE_ZERO  = TINY( CONV_M2N )

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
                CALL EXTRACT_AERO( REAL( CBLK(ICELL, : ),4 ), .FALSE. )
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

      END SUBROUTINE INIT_DEGRADE
