
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
C $Header: /project/yoj/arc/CCTM/src/phot/phot_table/opphot.F,v 1.2 2011/10/21 16:11:29 yoj Exp $



C $Header: /project/yoj/arc/CCTM/src/phot/phot_table/opphot.F,v 1.2 2011/10/21 16:11:29 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPPHOT ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C
C  FUNCTION:  Opens the photolysis diagnostic file that contains values
C             of photolysis rates used by gas chemistry
C
C  PRECONDITIONS REQUIRED:
C     None
C
C  REVISION  HISTORY:
C       Date   Who          What
C     -------- ----------   -----------------------------------------
C     04/2011  B.Hutzell    initial version created based on opphot.F in the 
C                           inline_phot module
C     07 Jul 14 B.Hutzell:  replaced mechanism include file(s) with fortran module
C
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE RXNS_DATA           ! chemical mechanism data
      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

C...Arguments

      INTEGER, INTENT( IN ) :: JDATE  ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME  ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP  ! output time step

C...Local variables

      CHARACTER( 16 ), SAVE :: PNAME = 'OPPHOT'
      CHARACTER( 16 )       :: LAMBDA
      CHARACTER( 96 )       :: XMSG = ' '

      INTEGER N, L, JWL       ! loop variables
C-----------------------------------------------------------------------

C...Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_RJ_2, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_RJ_2 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG ( XMSG )

C...Set output file characteristics based on COORD.EXT and open
C...  the photolysis diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP

         NVARS3D = NPHOTAB
         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D = NLAYS
         NTHIK3D =     1
         GDTYP3D = GDTYP_GD
         P_ALP3D = P_ALP_GD
         P_BET3D = P_BET_GD
         P_GAM3D = P_GAM_GD
         XORIG3D = XORIG_GD
         YORIG3D = YORIG_GD
         XCENT3D = XCENT_GD
         YCENT3D = YCENT_GD
         XCELL3D = XCELL_GD
         YCELL3D = YCELL_GD
         VGTYP3D = VGTYP_GD
         VGTOP3D = VGTOP_GD
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO

         FDESC3D( 1 ) = 'Photolysis rates file output'

         DO N = 2, MXDESC3
            FDESC3D( N ) = ' '
         END DO

C...load data from photolysis reaction list

         DO N = 1, NPHOTAB
            VNAME3D( N ) = PHOTAB( N )
            VTYPE3D( N ) = M3REAL
            UNITS3D( N ) = 'min-1'
            VDESC3D( N ) = 'Photolysis rates for ' // VNAME3D( N )
         END DO

C...Open the 2nd photolysis diagnostic file

         IF ( .NOT. OPEN3( CTM_RJ_2, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_2 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN

      END SUBROUTINE OPPHOT
