
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/opmet.F,v 1.2 2011/04/01 15:42:00 sjr Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPMET ( JDATE, JTIME, CONVPA )

      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      REAL         CONVPA     ! conversion factor to pressure in Pascals

C External Functions: None

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPMET'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 16 ) :: UNITSCK

      INTEGER      V

C-----------------------------------------------------------------------

C Open the met files
 
      IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// MET_CRO_3D // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// MET_CRO_2D // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
         XMSG = 'Could not get ' // MET_CRO_2D // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF         !  error abort if if desc3() failed

      V = INDEX1( 'PRSFC', NVARS3D, VNAME3D )
      IF ( V .NE. 0 ) THEN
         UNITSCK = UNITS3D( V ) 
         ELSE
         XMSG = 'Could not get variable PRSFC from ' // MET_CRO_2D
     &        // ' Try for variable PSTAR'
         CALL M3MESG( XMSG )
         V = INDEX1( 'PSTAR', NVARS3D, VNAME3D )
         IF ( V .NE. 0 ) THEN
            UNITSCK = UNITS3D( V ) 
            ELSE
            V = INDEX1( 'PSTAR', NVARS3D, VNAME3D )
            XMSG = 'Could not get variable PSTAR from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
         END IF

      IF (UNITSCK .EQ. 'PASCAL' .OR. UNITSCK .EQ. 'pascal' .OR.
     &    UNITSCK .EQ. 'Pascal' .OR. UNITSCK .EQ. 'PA'     .OR.
     &    UNITSCK .EQ. 'pa'     .OR. UNITSCK .EQ. 'Pa') THEN
         CONVPA = 1.0
         ELSE IF (UNITSCK .EQ. 'MILLIBAR' .OR. UNITSCK .EQ. 'millibar' .OR.
     &            UNITSCK .EQ. 'Millibar' .OR. UNITSCK .EQ. 'MB'       .OR.
     &            UNITSCK .EQ. 'mb'       .OR. UNITSCK .EQ. 'Mb') THEN
         CONVPA = 1.0E-02
         ELSE IF (UNITSCK .EQ. 'CENTIBAR' .OR. UNITSCK .EQ. 'centibar' .OR.
     &            UNITSCK .EQ. 'Centibar' .OR. UNITSCK .EQ. 'CB'       .OR.
     &            UNITSCK .EQ. 'cb'       .OR. UNITSCK .EQ. 'Cb') THEN
         CONVPA = 1.0E-03
         ELSE
         XMSG = 'Units incorrect on ' // MET_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
                       
      RETURN
      END
