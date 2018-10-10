
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION FINDEX (INDX, N, NLIST)

C Searches for INDX in list NLIST and returns the subscript
C (1...N) at which it is found, or returns 0 if INDX not found

      IMPLICIT NONE
 
      INTEGER INDX        !  index being searched for
      INTEGER N           !  Length of array to be searched
      INTEGER NLIST(*)    !  array to be searched

      INTEGER I

      DO I = 1, N
         IF ( INDX .EQ. NLIST( I ) ) THEN  ! found
            FINDEX = I
            RETURN
            END IF
         END DO
      FINDEX = 0        !  not found
      RETURN

      END

