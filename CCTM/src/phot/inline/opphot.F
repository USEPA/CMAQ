
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
C $Header: /project/work/rep/arc/CCTM/src/phot/phot_inline/opphot.F,v 1.3 2011/10/21 16:11:28 yoj Exp $

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
C     09/30/14 B.Hutzell    Added several diagnostics based on changes to cloud
C                           and aerosol description in radiation transfer solution
C-----------------------------------------------------------------------

      USE GRID_CONF               ! horizontal & vertical domain specifications
      USE RXNS_DATA               ! chemical mechanism declarations and data
      USE UTILIO_DEFN
      USE PHOT_MET_DATA, ONLY:  USE_ACM_CLOUD ! Met and Grid data
      USE PHOT_MOD                            ! photolysis in-line routines and data

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

      INTEGER N, L, JWL, INCR     ! loop variables
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
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'Cosine of Solar Zenith Angle'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'OZONE_COLUMN'
         UNITS3D( N ) = 'DU'
         VDESC3D( N ) = 'Total Ozone Column Density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TROPO_O3_COLUMN'
         UNITS3D( N ) = 'DU'
         VDESC3D( N ) = 'Tropospheric Ozone Column Density'
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


         N = N + 1
         VNAME3D( N ) = 'RESOLVED_CFRAC'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'Resolved Cloud Fraction averaged over cloudy layers'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'RESOLVED_WBAR'
         UNITS3D( N ) = 'G/M**3'
         VDESC3D( N ) = 'Resolved Cloud Hydrometeor Content averaged over cloudy layers'
         VTYPE3D( N ) = M3REAL
         
         IF( USE_ACM_CLOUD )THEN
             N = N + 1
             VNAME3D( N ) = 'SUBGRID_CFRAC'
             UNITS3D( N ) = 'NONE'
             VDESC3D( N ) = 'Subgrid Cloud Fraction averaged over cloudy layers'
             VTYPE3D( N ) = M3REAL

             N = N + 1
             VNAME3D( N ) = 'SUBGRID_WBAR'
             UNITS3D( N ) = 'G/M**3'
             VDESC3D( N ) = 'Subgrid Cloud Hydrometeor Content averaged over cloudy layers'
             VTYPE3D( N ) = M3REAL
         END IF             

         N = N + 1
         VNAME3D( N ) = 'TRANS_DIFFUSE'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'broad band transmission coefficient for diffuse radiation at surface'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TRANS_DIRECT'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'broad band transmission coefficient for direct radiation at surface'
         VTYPE3D( N ) = M3REAL
 
         N = N + 1
         VNAME3D( N ) = 'REFLECTION'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'broad band reflection coefficient at top of atmosphere'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'CLR_TRANS_DIF'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'broad band diffuse transmission for clear sky at surface'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'CLR_TRANS_DIR'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'broad band direct transmission for clear sky at surface'
         VTYPE3D( N ) = M3REAL
              
         N = N + 1
         VNAME3D( N ) = 'CLR_REFLECTION'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'broad band reflection for clear sky at top of atmosphere'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TROPO_O3_EXCEED'
         UNITS3D( N ) = 'Rel. Fraction'
         VDESC3D( N ) = 'Average Exceedance of modeled ozone column from max fraction of Total Column'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'N_EXCEED_TROPO3'
         UNITS3D( N ) = 'per TSTEP'
         VDESC3D( N ) = 'Number of exceedances to tropospheric ozone column'
         VTYPE3D( N ) = M3REAL

!         N = N + 1
!         VNAME3D( N ) = 'MIN_FRAC_STRATO3'
!         UNITS3D( N ) = 'NONE'
!         VDESC3D( N ) = 'Minimum fraction of total ozone column density above the top pressure level'
!         VTYPE3D( N ) = M3REAL



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
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Aerosol Optical Depth, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAU_CLOUD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Cloud Optical Depth, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
#ifdef phot_debug
            N = N + 1
            VNAME3D( N ) = 'SSA_CLOUD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Column Averaged Cloud Single Scattering Albedo, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'ASY_CLOUD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Column Averaged Cloud Asymmetry Factor, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
#endif
            N = N + 1
            VNAME3D( N ) = 'TAU_TOT_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Total Optical Depth, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAUO3_TOP_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Optical Depth of O3 above model domain, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'ALBEDO_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
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

C...set pointers for diagnostic wavelengths for vertical profiles
C...of optical depth and aerosol optics 

         
         N = NPHOTAB

         DO L = 1, N_DIAG_WVL

C...assumes that lamba in nanometers is on order of 100 or less

            JWL = DIAG_WVL( L )
            
            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME3D( N ) = 'AERO_SSA_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Aerosol Single Scattering Albedo at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'AERO_ASYM_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Aerosol Asymetry Factor at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAU_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Optical Depth at bottom of layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

 
            N = N + 1
            VNAME3D( N ) = 'TAU_AERO_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'NONE'
            VDESC3D( N ) = 'Aerosol Optical Depth at bottom of layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
            
        END DO
        
         N = N + 1
         VNAME3D( N ) = 'CFRAC_3D'
         UNITS3D( N ) = 'NONE'
         VDESC3D( N ) = 'Resolved Cloud Fraction in grid cell'
         VTYPE3D( N ) = M3REAL
        

        DO JWL = 1, NWL

C...assumes that lamba in nanometers is on order of 100 or less

            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME3D( N ) = 'ACTINIC_FX_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'Watts/m**2'
            VDESC3D( N ) = 'Net Actinic Flux, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

        END DO

        NVARS3D = N

C...Open the 2nd photolysis diagnostic file

         IF ( .NOT. OPEN3( CTM_RJ_2, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_2 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN

      END SUBROUTINE OPPHOT
