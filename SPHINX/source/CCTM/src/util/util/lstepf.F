
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
      SUBROUTINE LSTEPF ( FNAME, LDATE, LTIME )

C returns the last data date & time on file FNAME
C specifically used only for input files

      USE UTILIO_DEFN

      IMPLICIT NONE
 
C Includes:
 
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      CHARACTER( 16 ) :: FNAME
      INTEGER      LDATE        ! last file data date, coded YYYYDDD
      INTEGER      LTIME        ! last file data time, coded HHMMSS

C External Functions:
  
C local variables:

      CHARACTER( 16 ) :: PNAME = 'LSTEPF'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      LSTEP

C-----------------------------------------------------------------------

C open existing file for readonly access

#ifndef mpas
      IF ( .NOT. OPEN3( FNAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// FNAME
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. DESC3( FNAME ) ) THEN
         XMSG = 'Could not get ' // FNAME // ' file description'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF
 
      LSTEP = SEC2TIME( ( MXREC3D - 1 ) * TIME2SEC( TSTEP3D ) )
      LDATE = SDATE3D
      LTIME = STIME3D
      CALL NEXTIME( LDATE, LTIME, LSTEP )
#endif
 
      RETURN
      END
