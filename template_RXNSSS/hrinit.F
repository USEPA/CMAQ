
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

      SUBROUTINE HRINIT
C***********************************************************************
C
C  FUNCTION: To initialize species tolerances, arrays, and indices 
C
R1  PRECONDITIONS: For CB4 mechanisms only
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2002
C   21 Jun 10 J.Young: convert for Namelist redesign
C***********************************************************************

      USE UTILIO_DEFN
      USE HRDATA
      USE RXNS_DATA
       
      IMPLICIT NONE

C.....INCLUDES:
      Include SUBST_CONST          ! common constants

C.....ARGUMENTS: NONE
 
C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS:
      INTEGER  FINDEX         ! Finds location of a number in a list

C.....SAVED VARIABLES:
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRINIT'   ! Program name

 
C.....LOCAL VARIABLES: 
      CHARACTER( 132 ) ::  MSG       ! Log message

      INTEGER IND             ! Species index
      INTEGER N               ! Loop index
      INTEGER IOS             ! Allocate error status

C***********************************************************************

      N_SPEC = NUMB_MECH_SPC
      N_RXNS = NRXNS
R3
      ALLOCATE( RKI( MAX_CELLS_EBI,  NRXNS ),
     &          FORWARD_CONV( N_SPEC),
     &          REVERSE_CONV( N_SPEC),
     &          RXRAT( NRXNS ),
     &          RTOL( N_SPEC),
     &          YC(   N_SPEC),
     &          YC0(  N_SPEC),
     &          YCP(  N_SPEC),
     &          PROD( N_SPEC),
     &          LOSS( N_SPEC),
R4   &          PNEG( N_SPEC), 
     &          STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating RKI, RXRAT, RTOL, YC, YC0, YCP, FORWARD_CONV, '
     &       // 'REVERSE_CONV, PROD, LOSS, or PNEG'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

      DO N = 1, NUMB_MECH_SPC
         IF( TRIM( SPECIES_TYPE( N ) ) .NE. 'AE' )THEN
             FORWARD_CONV( N ) = 1.0D0
             REVERSE_CONV( N ) = 1.0D0
         ELSE
             FORWARD_CONV( N ) = 1.0D-3 * MWAIR / SPECIES_MOLWT( N )
             REVERSE_CONV( N ) = 1.0D+3 / MWAIR * SPECIES_MOLWT( N )
         END IF
       END DO

      NING1 = 4
      NING2 = 4

R5    ALLOCATE( YCCELL( MODEL_SPECIES), STAT = IOS )  ! array for degradation routines
R5    IF ( IOS .NE. 0 ) THEN
R5       MSG = 'Error allocating YCCELL '
R5       CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
R5    END IF

