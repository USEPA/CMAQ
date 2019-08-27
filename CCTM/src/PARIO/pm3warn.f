
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

        SUBROUTINE PM3WARN( CALLER, JDATE, JTIME, ERRTXT )
C.....................................................................
C
C  PURPOSE:   Wrapper for M3WARN in parallel environment, to 
C             add the processor-id suffix to the name of the
C             caller.
C
C
C  PRECONDITIONS REQUIRED:  Same as for M3WARN.
C
C
C  REVISION  HISTORY:
C       Original version 07/1998 by Al Bourgeois.
C       Modified 12/07/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO; removed
C             deprecated TRIMLEN
C
C
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C     CHARACTER*(*)   CALLER          ! Name of the caller.
C     INTEGER         JDATE           ! Model date for the error.
C     INTEGER         JTIME           ! Model time for the error.
C     CHARACTER*(*)   ERRTXT          ! Error message.
C
C     COMMON BLOCK PIOVARS:
C     INTEGER  MY_PE               !  Local processor id.
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  M3WARN.
C
C***********************************************************************

      USE M3UTILIO              ! i/o api

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PIOVARS.EXT'      ! Parameters for parallel implementation.


C...........   ARGUMENTS and their descriptions:

      CHARACTER*(*)   CALLER          ! Name of the caller.
      INTEGER         JDATE           ! Model date for the error.
      INTEGER         JTIME           ! Model time for the error.
      CHARACTER*(*)   ERRTXT          ! Error message.


C...........   LOCAL VARIABLES

      INTEGER      LENSTR       ! String length of CALLER.
      CHARACTER*3  CMYPE        ! Processor ID string.
      CHARACTER*7  PE_STR       ! String suffix to go with processor ID.
      CHARACTER*16 CALL16       ! First 16 characters of CALLER.
      CHARACTER*26 PCALLER      ! New caller string with PE information.

C.............................................................................
C   begin subroutine PM3WARN
  

C.......  Create strings to append to CALLER.
      WRITE (PE_STR,'(A7)') ' on PE '
      WRITE(CMYPE,'(I3.3)') MY_PE


C.......  Construct new CALLER string.
      LENSTR = MIN( 16, LEN_TRIM(CALLER) )
      CALL16 = CALLER( 1: LENSTR )
      PCALLER = CALL16(1:LENSTR)//PE_STR//CMYPE

C.......  Pass the new sting to M3WARN.
      CALL M3WARN( PCALLER, JDATE, JTIME, ERRTXT )

      RETURN
      END
  
