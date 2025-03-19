
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
C $Header: /project/yoj/arc/CCTM/src/plrise/smoke/oppt3d_diag.F,v 1.3 2011/10/21 16:11:31 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPPT3D_DIAG( PT3DNAME, TSTEP, 
     &                        N_SPC, SPEC_NAME, SPEC_UNITS )

C-----------------------------------------------------------------------
 
C  Description:
C    Opens the output 3d point source emissions diagnostic file
 
C  Preconditions:
 
C  Subroutines and Functions Called:
C       I/O API 

C Revision History:
C      4 Sep 2007 J.Young: initial
C     29 Jan 2010 D. Wong: Eliminate potential race condition at the MYPE = 0
C                          to open the diagnostic file. Eliminate the BARRIER
C                          and the CLOSE3 and OPEN3.
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     Aug 2015 D. Wong:    Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O
C                            implementation
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE DESID_VARS, ONLY : DESID_LAYS

      IMPLICIT NONE
 
C Includes:

C External Functions:

C Local Parameters:

C Arguments:
      CHARACTER( 16 ), INTENT( IN ) :: PT3DNAME ! diagnostic file logical name
      INTEGER,         INTENT( IN ) :: TSTEP    ! time step (HHMMSS)
      INTEGER,         INTENT( IN ) :: N_SPC    ! number of point source surrogates
      CHARACTER( 16 ), INTENT( IN ) :: SPEC_NAME( N_SPC )  ! species names
      CHARACTER( 16 ), INTENT( IN ) :: SPEC_UNITS( N_SPC ) ! units

C Local variables:

      INTEGER L, V, IOS
      CHARACTER( 256 ) :: XMSG      ! buffer for M3EXIT() messages
      CHARACTER( 256 ) :: RET_VAL   ! env var returned value
      CHARACTER(  16 ) :: PNAME = 'OPPT3DDIAG'   ! procedure name

C-----------------------------------------------------------------------

#ifndef mpas
C Set output file characteristics based on GRID_CONF module and open the 3D emis file

      FTYPE3D = GRDDED3
      SDATE3D = STDATE
      STIME3D = STTIME
      TSTEP3D = TSTEP
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = DESID_LAYS
      NTHIK3D = 1
      NVARS3D = N_SPC
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

      VNAME3D = ' '     ! array
      DO V = 1, N_SPC
         VNAME3D( V ) = SPEC_NAME( V )
      END DO

      VTYPE3D = 0       ! array
      VTYPE3D( 1:NVARS3D ) = M3REAL

      UNITS3D = ' '     ! array
      DO V = 1, N_SPC
         UNITS3D( V ) = SPEC_UNITS( V )
      END DO

      VDESC3D = ' '     ! array
      DO V = 1, N_SPC
         VDESC3D( V ) = 'Model species: ' // TRIM( SPEC_NAME( V ) )
      END DO

      FDESC3D = ' '  ! array
      FDESC3D( 1 ) = 'gridded hourly point source layered emissions'
      FDESC3D( 2 ) = '/From/ ' // PNAME
      FDESC3D( 3 ) = '/Version/ Inline'

C create header

      IF ( IO_PE_INCLUSIVE ) THEN   ! open new
         IF ( .NOT. OPEN3( PT3DNAME, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not open ' // TRIM( PT3DNAME )
            CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
         END IF
      END IF
#endif

      RETURN

      END SUBROUTINE OPPT3D_DIAG

