
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/opdepv_mos.F,v 1.4 2012/01/19 14:27:24 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPDEPV_MOS  ( JDATE, JTIME, TSTEP,
     &                         N_GDEPV_NAMES, GDEPV_NAMES,
     &                         N_ADEPV_NAMES, ADEPV_NAMES )
!    &                         N_SDEPV_NAMES, SDEPV_NAMES )

C 2011-May-20 - created by D.Schwede - modified from opdepv_diag.F

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE DEPVVARS
      USE UTILIO_DEFN
      USE LSM_Mod, Only: N_LUFRAC

      IMPLICIT NONE
 
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER, INTENT( IN ) :: JDATE   ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME   ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP   ! output time step

C number and names of gas deposition velocities
      INTEGER, INTENT( IN ) :: N_GDEPV_NAMES
      CHARACTER( 16 ), INTENT( IN ) :: GDEPV_NAMES( : )

C number and names of aerosol deposition velocities
      INTEGER, INTENT( IN ) :: N_ADEPV_NAMES
      CHARACTER( 16 ), INTENT( IN ) :: ADEPV_NAMES( : )

C number and names of (SOA+aerosol) species deposition velocities
!     INTEGER            N_SDEPV_NAMES
!     CHARACTER( 16 ) :: SDEPV_NAMES( : )

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPDEPV_MOS'
      CHARACTER( 96 ) :: XMSG = ' '
      INTEGER         :: LOGDEV

      INTEGER      V, N, L     ! loop induction variables

C-----------------------------------------------------------------------

      LOGDEV = INIT3()

C Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_DEPV_MOS, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_DEPV_MOS // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C Set output file characteristics based on GRIDDESC and open the dep velocity dignostic file

         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP
         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

         FTYPE3D = GRDDED3
!        NVARS3D = N_GDEPV_NAMES + N_ADEPV_NAMES + N_SDEPV_NAMES + 4
         NVARS3D = N_GDEPV_NAMES + N_ADEPV_NAMES + 4
         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D = N_LUFRAC   ! preempting normal use of vertical layers
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
!        DO L = 1, NLAYS3D + 1
!           VGLVS3D( L ) = VGLVS_GD( L )
!        END DO
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         FDESC3D = ' '   ! array

         L = LEN_TRIM( DEPV_METHOD )
         FDESC3D( 1 ) = 'Instanteous values of land use specific'
     &               // ' dry deposition velocities:'
     &               // ' the gas phase velocities are' 
         FDESC3D( 2 ) = 'estimated by the '
     &               //  DEPV_METHOD( 1:L ) // ' algorithm.'

         N = 0

         DO V = 1, N_GDEPV_NAMES
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = GDEPV_NAMES( V )
            UNITS3D( V ) = 'cm/sec'
            VDESC3D( V ) = 'gas phase dry deposition velocity'
         END DO

         N = N_GDEPV_NAMES

         DO V = 1, N_ADEPV_NAMES
            N = N + 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = ADEPV_NAMES( V )
            UNITS3D( N ) = 'cm/sec'
            VDESC3D( N ) = 'aerosol phase dry deposition velocity'
         END DO

         N = N_GDEPV_NAMES + N_ADEPV_NAMES

!        DO V = 1, N_SDEPV_NAMES
!           N = N + 1
!           VTYPE3D( N ) = M3REAL
!           VNAME3D( N ) = SDEPV_NAMES( V )
!           UNITS3D( N ) = 'cm/sec'
!           VDESC3D( N ) = 'deposition velocity for (GAS-AEROSOL) '
!    &                  // ' semi-volatile compound'
!        END DO

         N = N + 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'LAIJ'
            UNITS3D( N ) = 'dimensionless'
            VDESC3D( N ) = 'leaf area index for land use category'

         N = N + 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'RAJ'
            UNITS3D( N ) = 'm/sec'
            VDESC3D( N ) = 'aerodynamic resistance for land use category'

         N = N + 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'USTARJ'
            UNITS3D( N ) = 'm/sec'
            VDESC3D( N ) = 'friction velocity for land use category'

         N = N + 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'Z0J'
            UNITS3D( N ) = 'm/sec'
            VDESC3D( N ) = 'surface roughness for land use category'


C Open diagnostic file

         IF ( .NOT. OPEN3( CTM_DEPV_MOS, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// TRIM( CTM_DEPV_MOS) // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN
      END
