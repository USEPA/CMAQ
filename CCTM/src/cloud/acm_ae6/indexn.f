
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

      INTEGER FUNCTION INDEXN ( NAME1, N, NAME2, INDICES )

C***********************************************************************
C
C  FUNCTION:
C
C    This routine searches for all occurrences of NAME1 in list NAME2
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C      NAME1       Character string being searched for
C      N           Length of array to be searched
C      NAME2       Character array to be searched
C      INDICES     Index array of all occurrences
C
C    Output arguments:
C
C      INDEXN      The number of occurrences of NAME1 within the NAME2
C                  array.  If string was not found, INDEXN = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C
C      None
C
C***********************************************************************

      IMPLICIT NONE

      INTEGER     N
      INTEGER     I
      INTEGER     INDICES(*)

      CHARACTER*(*) NAME1
      CHARACTER*(*) NAME2(*)

C...Assume NAME1 is not in list NAME2    

      INDEXN = 0

      DO I = 1, N

        IF ( INDEX( NAME2( I ), NAME1 ) .GT. 0 ) THEN
          INDEXN = INDEXN + 1
          INDICES( INDEXN ) = I
        END IF

      END DO

      RETURN
      END              
