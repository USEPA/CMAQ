
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

      REAL FUNCTION MOLCLOSS( NAMETYP, NAMINDX, NRX, NRFND )

C-----------------------------------------------------------------------
C Function: To find the number of species/family molecules lost in a reaction 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C-----------------------------------------------------------------------

      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      CHARACTER( * ) :: NAMETYP  ! Type of name (mechanism species of family)

      INTEGER NAMINDX    ! Index for species or family name location
      INTEGER NRX        ! Reaction number
      INTEGER NRFND      ! Number of reactants found
                                        
C Parameters: None

C External Functions: None 

C Local Variables:

      CHARACTER( 16 ) :: SPECIS    ! Species name to check

      INTEGER INDX       ! Pointer to reactant name
      INTEGER N          ! Loop index for number of family members
      INTEGER NRC        ! Loop index for number of reactants
      INTEGER NUMSPEC    ! Number of species to check

      REAL    SUM        ! Sum of molecular production and loss
      REAL    COEF       ! Defined stoichiometric coefficient
         
C-----------------------------------------------------------------------
      NRFND = 0
      SUM = 0.0
      
c..Determine loop index for family name versus mechanism species
      IF ( NAMETYP .EQ. 'FAM' ) THEN
         NUMSPEC = NUMFAMMEM( NAMINDX )
      ELSE
         NUMSPEC = 1
      END IF

c..Add the number of species/family member molecules lost in this reaction
      DO NRC = 1, NREACT( NRX )
         INDX = IRR( NRX,NRC )
         IF ( INDX .NE. 0 ) THEN
            DO N = 1, NUMSPEC
               IF ( NAMETYP .EQ. 'FAM' ) THEN
                  SPECIS = FAMMEMNAM( NAMINDX,N )
                  COEF = FAMSC( NAMINDX,N )
               ELSE
                  SPECIS = CHEMISTRY_SPC( NAMINDX )
                  COEF = 1.0
               END IF
               IF ( CHEMISTRY_SPC( INDX ) .EQ. SPECIS ) THEN
                  SUM = SUM + COEF
                  NRFND = NRFND + 1
               END IF
            END DO
          END IF
      END DO

      MOLCLOSS = SUM

      RETURN

      END FUNCTION MOLCLOSS
