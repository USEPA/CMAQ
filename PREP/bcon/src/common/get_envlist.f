
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
C $Header: /project/yoj/arc/BCON/src/common/get_envlist.f,v 1.2 2011/10/21 16:52:32 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )

C get a list env var (quoted string of items delimited by white space,
C commas or semi-colons) and parse out the items into variables. Two data
C types: character strings and integers (still represented as strings in
C the env var vaules).
C Examples:
C 1)   setenv AVG_CONC_VARS "O3 NO NO2"
C 2)   setenv AVG_CONC_LAYS "2 5"          < start at two, end at 5
C 3)   setenv NPCOLSXNPROWS "4 3"
C 4)   setenv BCOL_ECOL "3 8"
C 5)   setenv BROW_EROW "2 10"
C 6)   setenv BLAY_ELAY "1 5"

C In example (1), not only parse out the named items "O3", "NO" and "NO2",
C but also obtain the count on the number of itmes (=3).

      IMPLICIT NONE

      CHARACTER( * ), INTENT ( IN ) :: ENV_VAR
      INTEGER, INTENT ( OUT ) :: NVARS
      CHARACTER( 16 ), INTENT ( OUT ) :: VAL_LIST( : )

      CHARACTER(  16 ) :: PNAME = 'GET_ENVLIST'
      CHARACTER( 256 ) :: E_VAL
      CHARACTER(   1 ) :: CHR
      CHARACTER(  96 ) :: XMSG
      INTEGER STATUS

      INTEGER :: JP( SIZE( VAL_LIST ) ), KP( SIZE( VAL_LIST ) )
      INTEGER IP, V

C               env_var_name description default_env_var_name
C                    |          |          |   env_var_value
C                    |          |          |        |
      CALL ENVSTR( ENV_VAR, 'Env_Var', 'VARLIST', E_VAL, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'Environment variable ' // ENV_VAR // ' not set'
         CALL M3EXIT( PNAME, 0, 0, XMSG, 2 )
         END IF

C Parse:

      NVARS = 1

C don't count until 1st char in string

      IP = 0

101   CONTINUE
      IP = IP + 1
      IF ( IP .GT. 256 ) GO TO 301
      CHR = E_VAL( IP:IP )
      IF ( CHR .EQ. ' ' .OR. ICHAR ( CHR ) .EQ. 09 ) GO TO 101
      JP( NVARS ) = IP   ! 1st char

201   CONTINUE
      IP = IP + 1
      IF ( IP .GT. 256 ) THEN
         XMSG = 'Environment variable value too long'
         CALL M3EXIT( PNAME, 0, 0, XMSG, 2 )
         END IF
      CHR = E_VAL( IP:IP )
      IF ( CHR .NE. ' ' .AND.
     &     CHR .NE. ',' .AND.
     &     CHR .NE. ';' .OR.
     &     ICHAR ( CHR ) .EQ. 09 ) THEN  ! 09 = horizontal tab
         GO TO 201
         ELSE
         KP( NVARS ) = IP - 1 ! last char in this item
         NVARS = NVARS + 1
         END IF 

      GO TO 101

301   CONTINUE
      NVARS = NVARS - 1

      DO V = 1, NVARS
         VAL_LIST( V ) = E_VAL( JP( V ):KP( V ) )
         END DO

      RETURN 
      END
