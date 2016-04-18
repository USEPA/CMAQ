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

C  Routine to return the units of a variable for file M3_FLNAME(1)

      SUBROUTINE GET_UNITS( VARNAME, UNITS )

      USE M3FILES
      USE ENV_VARS
      USE M3UTILIO

      IMPLICIT NONE     

C..INCLUDE FILES:
C      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C      INCLUDE SUBST_IOFDESC     ! IOAPI file description
C      INCLUDE SUBST_IODECL      ! IOAPI declarations


C..ARGUMENTS:
      CHARACTER*(*) VARNAME
      CHARACTER*(*) UNITS

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16  PNAME             ! Program Name
      CHARACTER*256  MSG
      INTEGER I


      DATA PNAME / 'CK_M3FLS' /
      UNITS = 'na'

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get header data for CTM file 1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( .NOT. DESC3 ( M3_FLNAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // M3_FLNAME( 1 ) 
     &         // ' file'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         RETURN
      ENDIF


      !  find matching variable, and return it's units
      DO I=1,MXVARS3
        if( VNAME3D(i) .eq. VARNAME ) UNITS = UNITS3D(I)
      EndDo

      RETURN
 
      END
