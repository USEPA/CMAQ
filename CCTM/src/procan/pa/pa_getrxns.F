
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

      SUBROUTINE GETRXNS( NOUT, NTERM, NR, RXNUM, COEFF, LRXINC )

C-----------------------------------------------------------------------
C Function: To get the reaction numbers and coefficients for the
C           PROD/LOSS/NET operators
             
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
C  Sep 2018 C. Nolte: remove unused M3UTILIO 
C-----------------------------------------------------------------------

      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      INTEGER NOUT     ! The incoming output number
      INTEGER NTERM    ! The incoming term number in the output list
      INTEGER NR       ! The outgoing number of reactions found
      INTEGER :: RXNUM ( MXRXNS )  ! The reaction numbers found
      REAL    :: COEFF ( MXRXNS )  ! The coefficients of the reactions found
      LOGICAL :: LRXINC( MXRXNS )  ! Flag to include reaction in prod,
                                   ! loss, or net operations
      
C Parameters: None

C External Functions: 

      REAL MOLCLOSS    ! Function to get number of species or 
                       ! family molecules lost in a reaction
      REAL MOLCPROD    ! Function to get number of species or 
                       ! family molecules produced in a reaction

C Local Variables:

      CHARACTER(  4 ) :: NAMETYPE  ! Type of name (mech or family)
      CHARACTER(  4 ) :: TYPE      ! Type of term
      CHARACTER( 11 ) :: SP1      ! Species name holder
      CHARACTER( 11 ) :: SP2      ! species name holder

      INTEGER INDX       ! Holder for pointer 
      INTEGER NPFND      ! Number of reactant species found
      INTEGER NRFND      ! Number of reactant species found
      INTEGER NRFND1     ! Holder for number of reactant species found
      INTEGER NRX        ! Loop index for number of reactions

      LOGICAL LINCL      ! Flag to include reaction in output
      LOGICAL LTEST1, LTEST2    ! Flags to indicate that test passed

      REAL COEF          ! Coefficient for product or reactant 
      REAL COEF2         ! Coefficient for reactant for LOSS/OR operator
      REAL SUM           ! Sum of molecular production and loss
         
C-----------------------------------------------------------------------

      TYPE = OUTTYPE( NOUT,NTERM )
      NR = 0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the production operator
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( TYPE .EQ. 'PROD' .OR. TYPE .EQ. 'NETP' ) THEN

         DO 20 NRX = 1, NRXNS

c..Skip the reaction if it is in a cycle or the species is not produced
            IF ( .NOT. LRXINC( NRX ) ) GO TO 20
            NAMETYPE = OUTS1TYP( NOUT,NTERM )
            INDX = OUTS1NUM( NOUT,NTERM )
            IF ( TYPE .EQ. 'PROD' ) THEN 
               COEF = MOLCPROD( NAMETYPE, INDX, NRX, NPFND )              
            ELSE
               COEF = MOLCPROD( NAMETYPE, INDX, NRX, NPFND ) -
     &                MOLCLOSS( NAMETYPE, INDX, NRX, NRFND )
            END IF
            IF ( COEF .LE. 0.0 ) GO TO 20

            NRFND1 = 0

c..Set the test flag for the FROM operator
            LTEST1 = .FALSE.
            IF ( OUTSPEC2( NOUT,NTERM ) .EQ. '' ) THEN
               LTEST1 = .TRUE.
            ELSE
               NAMETYPE = OUTS2TYP( NOUT,NTERM )
               INDX  = OUTS2NUM( NOUT,NTERM ) 
               IF ( OUTSPEC2( NOUT,NTERM ) .NE. 'hv' ) THEN
                  SUM = MOLCLOSS( NAMETYPE, INDX, NRX, NRFND )
                  NRFND1 = NRFND
                  IF ( NRFND .NE. 0 ) LTEST1 = .TRUE.
               ELSE
                  IF ( BTEST( IRXBITS( NRX ), 1 ) ) LTEST1 = .TRUE.
               END IF
            END IF

c..Set the test flag for the AND/OR operator 
            LTEST2 = .FALSE.
            IF ( OUTSPEC3( NOUT,NTERM  ) .EQ. ''  ) THEN
               LTEST2 = .TRUE.
            ELSE
               SP1  = OUTSPEC2( NOUT,NTERM )
               SP2  = OUTSPEC3( NOUT,NTERM )
               NAMETYPE  = OUTS3TYP( NOUT,NTERM )
               INDX = OUTS3NUM( NOUT, NTERM ) 
               IF ( OUTSPEC3( NOUT,NTERM ) .NE. 'hv' ) THEN
                  SUM = MOLCLOSS( NAMETYPE, INDX, NRX, NRFND )
                  IF ( SP1 .NE. SP2 ) THEN
                     IF ( NRFND .GT. 0 ) LTEST2 = .TRUE.
                  ELSE
                     NRFND = NRFND + NRFND1
                     IF ( NRFND .GE. 4 ) LTEST2 = .TRUE.
                  END IF
               ELSE
                  IF ( BTEST( IRXBITS( NRX ), 1 ) ) LTEST2 = .TRUE.
               END IF
            END IF

c..Determine whether this reaction is to be included
            LINCL = .FALSE.
            IF ( OUTSPEC2( NOUT,NTERM ) .EQ. '' ) THEN
               LINCL = .TRUE.
            ELSE 
               IF ( OUTSPEC3( NOUT,NTERM ) .EQ. '' ) THEN
                  IF ( LTEST1 ) LINCL = .TRUE.
               ELSE                                    
                  IF ( OUTOP2( NOUT,NTERM ) .EQ. 'OR' ) THEN
                     IF ( LTEST1 .OR. LTEST2 ) LINCL = .TRUE.
                  ELSE IF ( OUTOP2( NOUT,NTERM ) .EQ. 'AND' ) THEN
                     IF ( LTEST1 .AND. LTEST2 ) LINCL = .TRUE. 
                  END IF
               END IF
            END IF
  
c..Store reaction data for this term
            IF ( LINCL ) THEN
                NR = NR + 1
                RXNUM( NR ) = NRX
                COEFF( NR ) = COEF
            END IF
20       CONTINUE
         RETURN
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the loss operator
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( TYPE .EQ. 'LOSS' .OR. TYPE .EQ. 'NETL' ) THEN
         NRFND1 = 0
         DO 40 NRX = 1, NRXNS

c..Skip the reaction if it is in a cycle and set test1 flag
            IF ( .NOT. LRXINC( NRX ) ) GO TO 40
            LTEST1 = .TRUE.
            NAMETYPE  = OUTS1TYP( NOUT,NTERM )
            INDX = OUTS1NUM( NOUT, NTERM )
            COEF = MOLCLOSS( NAMETYPE, INDX, NRX, NRFND ) -
     &             MOLCPROD( NAMETYPE, INDX, NRX, NPFND )
            IF ( TYPE .EQ. 'LOSS' .AND. NRFND .GT. 0 ) COEF = 1.0
            IF ( COEF .LE. 0.0 ) LTEST1 = .FALSE.

c..Set the test flag for the AND/OR operator 
            LTEST2 = .FALSE.
            IF ( OUTSPEC3( NOUT,NTERM ) .EQ. '' ) THEN
               LTEST2 = .TRUE.
            ELSE
               SP1 = OUTSPEC1( NOUT,NTERM )
               SP2 = OUTSPEC3( NOUT,NTERM )
               NAMETYPE = OUTS3TYP( NOUT,NTERM )
               INDX = OUTS3NUM( NOUT,NTERM ) 
               IF ( OUTSPEC3( NOUT,NTERM ) .NE. 'hv' ) THEN
                  COEF2 = MOLCLOSS( NAMETYPE, INDX, NRX, NRFND ) -
     &                    MOLCPROD( NAMETYPE, INDX, NRX, NPFND )
                  IF ( TYPE .EQ. 'LOSS' .AND. NRFND .GT. 0 ) COEF2 = 1.0
                  IF ( SP1 .NE. SP2 ) THEN
                     IF ( NRFND .GT. 0 ) LTEST2 = .TRUE.
                  ELSE
                     NRFND = NRFND + NRFND1
                     IF ( NRFND .GE. 4 ) LTEST2 = .TRUE.
                  END IF
               ELSE
                  IF ( BTEST( IRXBITS( NRX ), 1 ) ) LTEST2 = .TRUE.
               END IF
            END IF

c..Determine whether this reaction is to be included
            LINCL = .FALSE.
            IF ( OUTSPEC3( NOUT,NTERM ) .EQ. '' ) THEN
               IF ( LTEST1 ) LINCL = .TRUE.
            ELSE                       
               IF ( OUTOP2( NOUT,NTERM ) .EQ. 'OR' ) THEN
                  IF ( LTEST1 .OR. LTEST2 ) LINCL = .TRUE.
                  IF ( LINCL .AND. .NOT. LTEST1 ) COEF = COEF2
               ELSE IF ( OUTOP2( NOUT,NTERM ) .EQ. 'AND' ) THEN
                  IF ( LTEST1 .AND. LTEST2 ) LINCL = .TRUE. 
               END IF
            END IF

c..Store reaction data for this term
            IF ( LINCL ) THEN
                NR = NR + 1
                RXNUM( NR ) = NRX
                COEFF( NR ) = COEF
            END IF
40       CONTINUE
         RETURN
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the net operator
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( TYPE .EQ. 'NET' ) THEN
         DO 60 NRX = 1, NRXNS

c..Skip the reaction if it is in a cycle and set test1 flag
            IF ( .NOT. LRXINC( NRX ) ) GO TO 60
            LTEST1 = .TRUE.
            NAMETYPE = OUTS1TYP( NOUT,NTERM )
            INDX = OUTS1NUM( NOUT,NTERM )
            COEF = MOLCPROD( NAMETYPE, INDX, NRX, NPFND ) -
     &             MOLCLOSS( NAMETYPE, INDX, NRX, NRFND )
            IF ( COEF .NE. 0.0 ) THEN
                NR = NR + 1
                RXNUM( NR ) = NRX
                COEFF( NR ) = COEF
            END IF
60       CONTINUE
         RETURN
      END IF 

      RETURN

      END SUBROUTINE GETRXNS
