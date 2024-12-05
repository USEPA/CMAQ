
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

      IMPLICIT none

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

#ifndef mpas
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
         UNITS3D( N ) = ''
         VDESC3D( N ) = 'Cosine of Solar Zenith Angle'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'OZONE_COLUMN'
         UNITS3D( N ) = 'DU'
         VDESC3D( N ) = 'Observed Total Ozone Column Density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'NO2_COLUMN'
         UNITS3D( N ) = 'petamolec cm-2'
         VDESC3D( N ) = 'Predicted nitrogen dioxide column density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'CO_COLUMN'
         UNITS3D( N ) = 'petamolec cm-2'
         VDESC3D( N ) = 'Predicted carbon monoxide column density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'SO2_COLUMN'
         UNITS3D( N ) = 'petamolec cm-2'
         VDESC3D( N ) = 'Predicted sulfur dioxide column density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'HCHO_COLUMN'
         UNITS3D( N ) = 'petamolec cm-2'
         VDESC3D( N ) = 'Predicted formaldehyde column density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TROPO_O3_COLUMN'
         UNITS3D( N ) = 'DU'
         VDESC3D( N ) = 'Predicted Tropospheric Ozone Column density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'JNO2'
         UNITS3D( N ) = 'min-1'
         VDESC3D( N ) = 'Photodissociation rate of NO2'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'JO3O1D'
         UNITS3D( N ) = 'min-1'
         VDESC3D( N ) = 'Photodissociation rate of ozone producing O(1D)'
         VTYPE3D( N ) = M3REAL


         N = N + 1
         VNAME3D( N ) = 'RESOLVED_CFRAC'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'Resolved Cloud Fraction averaged over cloudy layers'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'RESOLVED_WBAR'
         UNITS3D( N ) = 'g m-3'
         VDESC3D( N ) = 'Resolved Cloud Hydrometeor Content averaged over cloudy layers'
         VTYPE3D( N ) = M3REAL
         
         IF( USE_ACM_CLOUD )THEN
             N = N + 1
             VNAME3D( N ) = 'SUBGRID_CFRAC'
             UNITS3D( N ) = '1'
             VDESC3D( N ) = 'Subgrid Cloud Fraction averaged over cloudy layers'
             VTYPE3D( N ) = M3REAL

             N = N + 1
             VNAME3D( N ) = 'SUBGRID_WBAR'
             UNITS3D( N ) = 'g m-3'
             VDESC3D( N ) = 'Subgrid Cloud Hydrometeor Content averaged over cloudy layers'
             VTYPE3D( N ) = M3REAL
         END IF             

         N = N + 1
         VNAME3D( N ) = 'TRANS_DIFFUSE'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'broad band transmission coefficient for diffuse radiation at surface'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TRANS_DIRECT'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'broad band transmission coefficient for direct radiation at surface'
         VTYPE3D( N ) = M3REAL
 
         N = N + 1
         VNAME3D( N ) = 'REFLECTION'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'broad band reflection coefficient at top of atmosphere'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'CLR_TRANS_DIF'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'broad band diffuse transmission for clear sky at surface'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'CLR_TRANS_DIR'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'broad band direct transmission for clear sky at surface'
         VTYPE3D( N ) = M3REAL
              
         N = N + 1
         VNAME3D( N ) = 'CLR_REFLECTION'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'broad band reflection for clear sky at top of atmosphere'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'TROPO_O3_EXCEED'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'Average Exceedance of modeled ozone column from max fraction of Total Column, '
     &               // ' a relative fraction from total column.'         
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'N_EXCEED_TROPO3'
         UNITS3D( N ) = ''
         VDESC3D( N ) = '# of times predicted tropospheric ozone column exceeds observed total column '
     &               // 'per file time step'          
         VTYPE3D( N ) = M3REAL


         DO JWL = 1, NWL

C...assumes that lamba in nanometers is on order of 100 or less

            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME3D( N ) = 'ETOT_SFC_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'W m-2'
            VDESC3D( N ) = 'Total Downward Irradiance at surface at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'AOD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Total Aerosol Optical Depth at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'AOD_ABS_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Absorption Aerosol Optical Depth at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAU_CLOUD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Cloud Optical Depth at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
#ifdef phot_debug
            N = N + 1
            VNAME3D( N ) = 'SSA_CLOUD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = '1'
            VDESC3D( N ) = 'Column Averaged Cloud Single Scattering Albedo at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'ASY_CLOUD_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Column Averaged Cloud Asymmetry Factor at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
#endif
            N = N + 1
            VNAME3D( N ) = 'TAU_TOT_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Total Optical Depth at'
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'TAUO3_TOP_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Optical Depth of O3 above model domain at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'ALBEDO_W' // TRIM( LAMBDA )
            UNITS3D( N ) = '1'
            VDESC3D( N ) = 'Surface Albedo at the wavelength at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

         ENDDO

         N = N + 1
         VNAME3D( N ) = 'AOD_W550_ANGST' 
         UNITS3D( N ) = ''
         VDESC3D( N ) = 'Aerosol Optical Depth at'
     &                // ' 550 nm based on an Angstrom Interpolation'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'AAOD_W550_ANGST' 
         UNITS3D( N ) = ''
         VDESC3D( N ) = 'Aerosol Absorption Optical Depth at'
     &                // ' 550 nm based on an Angstrom Interpolation'
         VTYPE3D( N ) = M3REAL

         NVARS3D = N

         FDESC3D( 1 ) = 'Surface Values of Optical Inputs and Radiative Results '
         FDESC3D( 2 ) = 'from the In-line calculation of Photolysis Rates '
         FDESC3D( 3 ) = 'for the ' // TRIM( MECHNAME ) // ' photochemical mechanism '
         DO L = 4, MXDESC3
            FDESC3D( L ) = ' '
         END DO

!C...Write ascii table describing variables
!         WRITE(LOGDEV,'(A)')'*PHOTDIAG1 File Contents'
!         WRITE(LOGDEV,'(A)')'**Surface Values of Optical Inputs and Radiative '
!     &                   // 'Results from the In-line calculation of Photolysis Rates '
!         WRITE(LOGDEV,99950)
!         WRITE(LOGDEV,99951)
!         DO L = 1, NVARS3D
!            WRITE(LOGDEV,99952)TRIM(VNAME3D( L )),TRIM(UNITS3D( L )),TRIM(VDESC3D( L ))
!         END DO

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
         NLAYS3D = NLAYS_DIAG
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

         FDESC3D( 1 ) = 'Three dimensional values of Photolysis rates '
         FDESC3D( 2 ) = 'used to make predictions for the ' // TRIM( MECHNAME )
         FDESC3D( 3 ) = 'photochemical mechanism from the In-line calculation.'
         FDESC3D( 4 ) = 'Data files can be found in CMAQ repository under subdirectory,'
         FDESC3D( 5 ) = 'UTIL/inline_phot_preproc/photolysis_CSQY_data'
         DO N = 6, MXDESC3
            FDESC3D( N ) = ' '
         END DO

C...load data from photolysis reaction list

         DO N = 1, NPHOTAB
            VNAME3D( N ) = PHOTAB( N )
            VTYPE3D( N ) = M3REAL
            UNITS3D( N ) = 'min-1'
            VDESC3D( N ) = 'Photolysis rates calculated based on data file; ' // VNAME3D(N)
         END DO

         NVARS3D = NPHOTAB
         
!C...Write ascii table describing variables
!         WRITE(LOGDEV,'(A)')'*PHOTDIAG2 File Contents'
!         WRITE(LOGDEV,'(A)')'**Three dimensionals values of Photolysis rates '
!     &                   // 'used to make predictions from the In-line calculation '
!     &                   // ' of Photolysis Rates '
!         WRITE(LOGDEV,99950)
!         WRITE(LOGDEV,99951)
!         DO L = 1, NVARS3D
!            WRITE(LOGDEV,99952)TRIM(VNAME3D( L )),TRIM(UNITS3D( L )),TRIM(VDESC3D( L ))
!         END DO

         IF ( .NOT. OPEN3( CTM_RJ_2, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_2 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

C...Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_RJ_3, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_RJ_3 // ' file for update - '
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
         NLAYS3D = NLAYS_DIAG
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

         FDESC3D( 1 ) = 'Three dimensionals values of Optical Inputs and Radiative'
         FDESC3D( 2 ) = 'Results from the In-line photolysis calculation using the'
         FDESC3D( 3 ) =  TRIM( MECHNAME ) // ' photochemical mechanism.'

         DO N = 4, MXDESC3
            FDESC3D( N ) = ' '
         END DO
         
         N = 0
         
         DO L = 1, N_DIAG_WVL

C...assumes that lamba in nanometers is on order of 100 or less

            JWL = DIAG_WVL( L )
            
            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME3D( N ) = 'AERO_SCAT_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'Km-1'
            VDESC3D( N ) = 'Aerosol Scattering of layer at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'AERO_ASYM_W' // TRIM( LAMBDA )
            UNITS3D( N ) = ''
            VDESC3D( N ) = 'Aerosol Asymmetry Factor at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'EXT_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'Km-1'
            VDESC3D( N ) = 'Total Extinction of layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

            N = N + 1
            VNAME3D( N ) = 'GAS_EXT_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'Km-1' 
            VDESC3D( N ) = 'Total Extinction from Rayleigh scattering NO2 and O3 in layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
 
            N = N + 1
            VNAME3D( N ) = 'EXT_AERO_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'Km-1'
            VDESC3D( N ) = 'Aerosol Extinction in layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL
            
            N = N + 1
            VNAME3D( N ) = 'ACTINIC_FX_W' // TRIM( LAMBDA )
            UNITS3D( N ) = 'W m-2'
            VDESC3D( N ) = 'Net Actinic Flux, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE3D( N ) = M3REAL

        END DO
        
         N = N + 1
         VNAME3D( N ) = 'CFRAC_3D'
         UNITS3D( N ) = '1'
         VDESC3D( N ) = 'Resolved Cloud Fraction in grid cell'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'EXT_AERO_W550'
         UNITS3D( N ) = 'Km-1'
         VDESC3D( N ) = ' Aerosol Extinction of layer for '
     &                // '550 nm based on an Angstrom Interpolation'
         VTYPE3D( N ) = M3REAL
        

        NVARS3D = N

!C...Write ascii table describing variables
!         WRITE(LOGDEV,'(A)')'*PHOTDIAG3 File Contents'
!         WRITE(LOGDEV,'(A)')'**Three dimensionals values of Optical Inputs and Radiative '
!     &                    // 'Results from the In-line photolysis calculation.'
!         WRITE(LOGDEV,99950)
!         WRITE(LOGDEV,99951)
!         DO L = 1, NVARS3D
!            WRITE(LOGDEV,99952)TRIM(VNAME3D( L )),TRIM(UNITS3D( L )),TRIM(VDESC3D( L ))
!         END DO

C...Open the 3rd photolysis diagnostic file

         IF ( .NOT. OPEN3( CTM_RJ_3, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_3 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF
#endif

99950    FORMAT('|Variable Name|Units|Description                                   |')         
99951    FORMAT('|:----|:----:|:---------------------------------------------|')
99952    FORMAT('|', A16, '|', A16, '|', A, '|')

      RETURN

      END SUBROUTINE OPPHOT
