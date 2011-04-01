
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
C $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/opphot.F,v 1.2 2011/04/01 15:41:52 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPPHOT ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C
C  FUNCTION:  Opens the photolysis diagnostic files
C
C  PRECONDITIONS REQUIRED:
C     None
C
C  REVISION  HISTORY:
C       Date   Who          What
C     -------- ----------   -----------------------------------------
C     01/2008  S.Roselle    Adapted from OPDIAM in the aerosol module
C                           for opening the photolysis diagnostic files
C     03/2011  B.Hutzell    Generalized and modified to write out surface albedo
C     03/29/11 S.Roselle    Replaced I/O API include files with UTILIO_DEFN
C
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE PHOT_MOD            ! photolysis in-line module

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters
      INCLUDE SUBST_RXCMMN    ! chemical mechamism reactions COMMON

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

      IF ( .NOT. OPEN3( CTM_RJ_1, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_RJ_1 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C...Set output file characteristics based on COORD.EXT and open
C...  the photolysis diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP

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

C...CSA Variables, Units and Descriptions for RJ_FILE

         N = 1
         VNAME3D( N ) = 'COSZENS'
         UNITS3D( N ) = ' '
         VDESC3D( N ) = 'Cosine of Solar Zenith Angle'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TOC'
         UNITS3D( N ) = 'DU'
         VDESC3D( N ) = 'Total Ozone Column'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'JNO2'
         UNITS3D( N ) = '1/min'
         VDESC3D( N ) = 'J-value for NO2'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'JO3O1D'
         UNITS3D( N ) = '1/min'
         VDESC3D( N ) = 'J-value for O3O1D'
         VTYPE3D( N ) = M3REAL

         DO JWL = 1, NWL

C...assumes that lamba in nanometers is on order of 100 or less

            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME3D( N ) = 'ETOT_SFC_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'Watts/m**2'
            VDESC3D( N ) = 'Total Downward Irradiance at sfc, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAU_AERO_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ' '
            VDESC3D( N ) = 'Aerosol Optical Depth, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAU_TOT_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ' '
            VDESC3D( N ) = 'Total Optical Depth, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAUO3_TOP_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ' '
            VDESC3D( N ) = 'Optical Depth of O3 above model domain, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'ALBEDO_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ' '
            VDESC3D( N ) = 'Surface Albedo at the wavelength '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

         ENDDO

         NVARS3D = N

         FDESC3D( 1 ) = 'Photolysis reactions diagnostics'
         DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
         END DO

C...Open the 1st photolysis diagnostic file

         IF ( .NOT. OPEN3( CTM_RJ_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_1 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

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
            UNITS3D( N ) = '1/min'
            VDESC3D( N ) = 'Photolysis rates for ' // VNAME3D(N)
         END DO

C...Open the 2nd photolysis diagnostic file

         IF ( .NOT. OPEN3( CTM_RJ_2, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_2 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN

      END SUBROUTINE OPPHOT
