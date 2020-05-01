
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


      SUBROUTINE INIT_DEGRADE( CBLK, TCELL, DCELL, PHOTO_CELL,
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
      USE DEGRADE_SETUP_TOX

      IMPLICIT NONE


C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK( : )                 !  species concentration in cell
      REAL,      INTENT( IN ) :: TCELL                     !  cell temperature  [ k ]
      REAL,      INTENT( IN ) :: DCELL                     !  cell air density  [ kg/m^3 ]
      REAL( 8 ), INTENT( IN ) :: PHOTO_CELL( : )           !  Photolysis table for cell [1/s]

      INTEGER, INTENT( IN ) :: JDATE  ! current model date , coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME  ! current model time , coded HHMMSS

C.....LOCAL VARIABLES:

      CHARACTER( 144 )        :: XMSG                   ! Message text
      CHARACTER( 16  ), SAVE  :: PNAME = 'INIT_DEGRADE' ! Routine name

      REAL(8), SAVE ::  MASS_TO_NUMBER ! air mass density( Kg/m3) to number density( #/cm3 ) [ (# per moles)/Kg ]

      REAL(8), SAVE ::  CONV_M2N       ! factor to convert ppm times mass density in [kg/m^3]
                                       ! into number density in [molecules/cm^3]
                                       
      REAL(8)       ::  CONV_FACT      ! conversion factor from ppm to molecules/cm^3
      REAL(8)       ::  INV_TEMP       ! reciprocal of temperature, [K^-1]

      INTEGER       :: I, J, K        ! loop counters
      INTEGER, SAVE :: ISIZE          ! dimension of CBLK array
      
      LOGICAL, SAVE ::  FIRSTCALL = .TRUE. 

C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize constants and allocate arrays


         MASS_TO_NUMBER = REAL( 1.0E-3*AVO / MWAIR, 8 )
         
         CONV_M2N       = 1.0D-6 * MASS_TO_NUMBER

         ISIZE = SIZE( CBLK )
         
         ALLOCATE( PREV_CONC( ISIZE ) )
         ALLOCATE( CURR_CONC( ISIZE ) )
         ALLOCATE( DELT_CONC( ISIZE ) )

         FIRSTCALL = .FALSE.

         EFFECTIVE_ZERO  = 5.0D0 * TINY( CONV_M2N )

      ENDIF

C..initialize concentrations and their changes
      DELT_CONC  = 0.0D0
      RATE_CONST = 0.0D0

      DO I = 1, ISIZE
         PREV_CONC( I ) = MAX( CBLK( I ), 0.0D0 )
         CURR_CONC( I ) = PREV_CONC( I )
      END DO
         

      NUMB_DENS = MASS_TO_NUMBER * REAL( DCELL, 8 )
      TEMP = REAL( TCELL, 8 )

      CONV_FACT  = CONV_M2N * REAL( DCELL, 8 )
      INV_TEMP   = 1.0D0 / TEMP

      LOOP_REACT: DO I = 1, N_REACT ! calculated rate constants

         IF( RXTANT_MAP( I ) < 0 )CYCLE LOOP_REACT

         LOOP_UNIRATE: DO J = 1, N_UNI_LOSS
            IF( UNIRATE( I, J ) .LT. EFFECTIVE_ZERO )CYCLE
            RATE_CONST( I, J ) = UNIRATE( I, J )
     &                         * TEMP**UNI_TEXP( I, J )
     &                         * DEXP( -UNI_ACT( I, J )*INV_TEMP )

         END DO LOOP_UNIRATE

         LOOP_BIRATE: DO J = 1, N_BI_LOSS
            IF( BIRATE( I, J ) .LT. EFFECTIVE_ZERO )CYCLE
            RATE_CONST( I, J+UNI_STOP ) = CONV_FACT * BIRATE( I, J )
     &                                  * TEMP**BI_TEXP( I, J )
     &                                  * DEXP( -BI_ACT( I, J )*INV_TEMP )

         END DO LOOP_BIRATE

         LOOP_TRIRATE: DO J = 1, N_TRI_LOSS
            IF( TRIRATE( I, J ) .LT. EFFECTIVE_ZERO )CYCLE
            RATE_CONST( I, J+BI_STOP ) = CONV_FACT * CONV_FACT * TRIRATE( I, J )
     &                                 * TEMP**TRI_TEXP( I, J )
     &                                 * DEXP( -TRI_ACT( I, J )*INV_TEMP )

         END DO LOOP_TRIRATE

         LOOP_PHOTORATE: DO J = 1, N_PHOTO_LOSS

            K = PHOTO_MAP( I, J )
            IF ( K < 1 ) CYCLE
            IF( A_PHOTO( I, J ) .LT. EFFECTIVE_ZERO )CYCLE
            RATE_CONST( I, J+TRI_STOP ) = A_PHOTO( I, J )
     &                                  * PHOTO_CELL( K )

         END DO LOOP_PHOTORATE

      END DO LOOP_REACT


      DO I = 1, N_REACT
         J = RXTANT_MAP( I )
         IF( J < 0 )CYCLE 
      END DO

      RETURN

      END SUBROUTINE INIT_DEGRADE
