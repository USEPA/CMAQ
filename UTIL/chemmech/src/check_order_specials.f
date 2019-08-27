
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


      SUBROUTINE CHECK_ORDER_SPECIAL(  )

!**********************************************************************
!  Function: Determine the order of the special rate operators so each operator
!            terms has consistent units between its terms
!**********************************************************************

      USE MECHANISM_DATA

      IMPLICIT NONE

!..Includes:


!..Arguments: NONE


!..Parameters: None

!..External Functions: None

!..Scratch Local Variables:

      INTEGER ISP            ! Loop index for special rates
      INTEGER ISP1,ISP2,ISP3 ! Pointers to species numbers
      INTEGER ISP4,ISP5,ISP6 ! Pointers to species numbers
      INTEGER NCELL          ! Loop index for number of cells
      INTEGER IKC_TERM       ! Loop index for special of KC terms
      INTEGER I_OPERATOR     ! Loop index for number of preceeding special rates
      INTEGER NRK            ! Reaction number
      INTEGER NRX            ! Loop index for number of reactions

      INTEGER COUNT_TERMS    ! count number of term used by each expressions 

      INTEGER ORDER_TERM( 2*MAXSPECTERMS )
      LOGICAL IS_FALLOFF
      LOGICAL ERROR_FLAG

!..Saved Local Variables: None

      LOGICAL, SAVE :: FIRSTCALL  = .TRUE.
      
      IF( .NOT. FIRSTCALL )RETURN
      
      IF ( NSPECIAL .LT. 1 )THEN
         WRITE( 6, 90001)(MSPECTERMS-1)
         RETURN
      ENDIF

      ORDER_TERM     = -999
      ERROR_FLAG     = .FALSE.
      
      ALLOCATE( ORDER_SPECIAL( NSPECIAL ) )
      ORDER_SPECIAL  = 0
! reset       
      DO 220 ISP = 1, NSPECIAL

!  Start with rate constant times concentration terms
         COUNT_TERMS = 0
         LOOP_KC: DO IKC_TERM = 1, MAXSPECTERMS
            NRK = INDEX_KTERM( ISP, IKC_TERM )

            IF ( NRK .LT. 0 )THEN ! empty array entry
               COUNT_TERMS = 1 + COUNT_TERMS
!               CYCLE LOOP_KC  
	    ELSE IF( NRK .GT. 0 )THEN ! existing rate constant
               COUNT_TERMS = 1 + COUNT_TERMS
               ORDER_TERM( IKC_TERM ) = IORDER( NRK )
! correct if rate constant is a falloff type
               IS_FALLOFF = ( KTYPE( NRK ) .GT. 7 .AND. KTYPE( NRK ) .LT. 11 )
               IF( KUNITS .NE. 2 .AND. IS_FALLOFF )THEN
	           ORDER_TERM( IKC_TERM ) = ORDER_TERM( IKC_TERM ) + 1
	       END IF
               ISP2 = INDEX_CTERM( ISP, IKC_TERM )
               IF ( ISP2 .LT. 1 ) CYCLE LOOP_KC  ! empty array entery
	       ORDER_TERM( IKC_TERM ) = ORDER_TERM( IKC_TERM ) - 1
            ELSE ! NRK = 0, KC term is a pure concentration
               ISP2 = INDEX_CTERM( ISP, IKC_TERM )
               IF ( ISP2 .LT. 1 ) CYCLE LOOP_KC  ! empty array entery
               COUNT_TERMS = 1 + COUNT_TERMS
               ORDER_TERM( IKC_TERM ) = 0
            END IF
         END DO LOOP_KC
	 

!  set order for terms using existing operators

         LOOP_OP: DO I_OPERATOR = 1, MAXSPECTERMS
            ISP1 = OPERATORS( ISP, I_OPERATOR )
            IF ( ISP1 .LT. 1 ) CYCLE LOOP_OP
	    ORDER_TERM( I_OPERATOR + MAXSPECTERMS ) = ORDER_SPECIAL( ISP1 )	    
            COUNT_TERMS = 1 + COUNT_TERMS
         END DO LOOP_OP
	 
	 ISP3 = 1
	 DO ISP1 = 2, 2*MAXSPECTERMS
            IF( ORDER_TERM( ISP1 ) .GE. -999 )CYCLE
	    ISP3 = ISP3 + 1
	    IF( ORDER_TERM( ISP1 ) .NE. ORDER_TERM( 1 ) )THEN
	       WRITE( 6, * )'ERROR: ' // TRIM( SPECIAL( ISP ) ) // ' uses inconsistent rate orders '
	       WRITE( 6, * )'ORDER FIRST TERM = ', ORDER_TERM( 1 ),
     &                     ' ORDER ', ISP3,'th TERM = ', ORDER_TERM( ISP1 )
	       ERROR_FLAG = .TRUE.
	    END IF
	 END DO
	 
	 ORDER_SPECIAL( ISP ) = ORDER_TERM( 1 )
	 
	 WRITE(6,90000 )SPECIAL( ISP ), ORDER_SPECIAL( ISP ), COUNT_TERMS

	 MSPECTERMS = MAX( MSPECTERMS, COUNT_TERMS )
220   CONTINUE

      WRITE( 6, 90001)MSPECTERMS
      IF( ERROR_FLAG )THEN
          WRITE( 6, * )'FATAL ERROR detected in routine CHECK_ORDER_SPECIAL'
	  WRITE( 6, * )'Consult the above information'
	  STOP
      END IF

      FIRSTCALL   = .FALSE.
90000 FORMAT('ORDER SPECIAL OPERATOR, ',A16,':',I2,' Number Terms in Operator: ',I4 )
90001 FORMAT('Maximum Number Terms used in the Operators: ',I4 )
      RETURN
      END
