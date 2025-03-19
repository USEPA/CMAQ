
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
      SUBROUTINE OPDEPV_DIAG ( JDATE, JTIME, TSTEP,
     &                         N_GDEPV_NAMES, GDEPV_NAMES,
     &                         N_ADEPV_NAMES, ADEPV_NAMES )
!    &                         N_SDEPV_NAMES, SDEPV_NAMES)

C 2006-Jun-30 - created by W Hutzell
C 2006-Dec-05 - modified by J.Young
C 2010-Jan-29 - removed unneccesary CLOSE3 statement for CTM_DEPV_DIAG file
C               to eliminate potential MPI race condition (D.Wong)
C 2011-Feb-16 - replaced I/O API include files with UTILIO_DEFN (S.Roselle)
C 2011-May-20 - modified diagnostic file header creation (D.Schwede)
C 2015-Aug    - added a conditional statement to allow only I/O
C                 processors to open CTM_DEPV_DIAG file (D. Wong)

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE DEPVVARS
      USE UTILIO_DEFN
!     USE pio_module

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
!     INTEGER, INTENT( IN ) :: N_SDEPV_NAMES
!     CHARACTER( 16 ), INTENT( IN ) :: SDEPV_NAMES( : )

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPDDEP_DIAG'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      V, N, L     ! loop induction variables

C-----------------------------------------------------------------------

#ifndef mpas
C Try to open existing file for update

      IF ( IO_PE_INCLUSIVE ) THEN   ! open new

         IF ( .NOT. OPEN3( CTM_DEPV_DIAG, FSRDWR3, PNAME ) ) THEN

            XMSG = 'Could not open ' // CTM_DEPV_DIAG // ' file for update - '
     &           // 'try to open new'
            CALL M3MESG( XMSG )

C Set output file characteristics based on GRIDDESC and open the dep velocity dignostic file

            SDATE3D = JDATE
            STIME3D = JTIME
            TSTEP3D = TSTEP
            CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

            FTYPE3D = GRDDED3
!           NVARS3D = N_GDEPV_NAMES + N_ADEPV_NAMES + N_SDEPV_NAMES
            NVARS3D = N_GDEPV_NAMES + N_ADEPV_NAMES
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D =     1
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
            DO L = 1, NLAYS3D + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
            GDNAM3D = GRID_NAME  ! from HGRD_DEFN

            FDESC3D = ' '   ! array

            L = LEN_TRIM( DEPV_METHOD )
            FDESC3D( 1 ) = 'Instanteous values of dry deposition velocities:'
     &                  // ' the gas phase velocities are' 
            FDESC3D( 2 ) = 'estimated by the '
     &                  //  DEPV_METHOD( 1:L ) // ' algorithm.'

            N = 0

            DO V = 1, N_GDEPV_NAMES
               N = N + 1
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = GDEPV_NAMES( V )
               UNITS3D( V ) = 'cm s-1'
               VDESC3D( V ) = 'gas phase dry deposition velocity'
            END DO

            N = N_GDEPV_NAMES

            DO V = 1, N_ADEPV_NAMES
               N = N + 1
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = ADEPV_NAMES( V )
               UNITS3D( N ) = 'cm s-1'
               VDESC3D( N ) = 'aerosol phase dry deposition velocity'
            END DO

!        N = N_GDEPV_NAMES + N_ADEPV_NAMES

!        DO V = 1, N_SDEPV_NAMES
!           N = N + 1
!           VTYPE3D( N ) = M3REAL
!           VNAME3D( N ) = SDEPV_NAMES( V )
!           UNITS3D( N ) = 'cm s-1'
!           VDESC3D( N ) = 'deposition velocity for (GAS-AEROSOL) '
!    &                  // ' semi-volatile compound'
!        END DO

C Open diagnostic file, then close it for subsequent open by all processors

            IF ( .NOT. OPEN3( CTM_DEPV_DIAG, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// TRIM( CTM_DEPV_DIAG ) // ' file'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END IF
      END IF   ! IO_PE_INCLUSIVE
#endif

      RETURN
      END
