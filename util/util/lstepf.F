
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/util/util/lstepf.F,v 1.2 2011/04/01 15:41:57 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

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
 
      RETURN
      END
