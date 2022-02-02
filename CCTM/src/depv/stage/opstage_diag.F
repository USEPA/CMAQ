
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
      SUBROUTINE OPSTAGE_DIAG( JDATE, JTIME, TSTEP )

C 2011-May-20 - created by D.Schwede - modified from opdepv_diag.F

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE STAGE_DATA
      USE UTILIO_DEFN
      USE MOSAIC_MOD, Only: Tile_Data

      IMPLICIT NONE
 
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER, INTENT( IN ) :: JDATE   ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME   ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP   ! output time step

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPSTAGE_DIAG'
      CHARACTER( 16 ) :: VNAME            ! scratch string
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      N_Vars
      INTEGER      V, N, L     ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_DEPV_MOS, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_DEPV_MOS // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C Set output file characteristics based on GRIDDESC and open the dep velocity dignostic file
         IF( FST ) THEN
            N_Vars = STAGE_Diag_Data%n_gas_spc + STAGE_Diag_Data%n_aero_spc + 7
         ELSE
            N_Vars = STAGE_Diag_Data%n_gas_spc + STAGE_Diag_Data%n_aero_spc + 6
!            N_Vars = STAGE_Diag_Data%n_gas_spc + 6
         END IF

         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP
         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

         FTYPE3D = GRDDED3
         NVARS3D = N_Vars
         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D = Tile_Data%N_LUFRAC   ! preempting normal use of vertical layers
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

         FDESC3D = ' '   ! array

         L = LEN_TRIM( DEPV_METHOD )
         FDESC3D( 1 ) = 'Instanteous values of land use specific'
     &               // ' resistances and conductances:'
     &               // ' these parameters are' 
         FDESC3D( 2 ) = 'estimated by the '
     &               //  DEPV_METHOD( 1:L ) // ' algorithm.'

         N = 0
         N = N + 1 ! 1
         VTYPE3D( N ) = M3REAL
         VNAME3D( N ) = 'RA'
         UNITS3D( N ) = 'sec/m'
         VDESC3D( N ) = 'aerodynamic resistance for land use category'

         N = N + 1 ! 2
         VTYPE3D( N ) = M3REAL
         VNAME3D( N ) = 'LUFRAC'
         UNITS3D( N ) = 'dimensionless'
         VDESC3D( N ) = 'Fractional land use'

         N = N + 1 ! 3
         VTYPE3D( N ) = M3REAL
         VNAME3D( N ) = 'LAI'
         UNITS3D( N ) = 'dimensionless'
         VDESC3D( N ) = 'leaf area index for land use category'

         N = N + 1 ! 4
         VTYPE3D( N ) = M3REAL
         VNAME3D( N ) = 'USTAR'
         UNITS3D( N ) = 'm/sec'
         VDESC3D( N ) = 'friction velocity for land use category'

         N = N + 1 ! 5
         VTYPE3D( N ) = M3REAL
         VNAME3D( N ) = 'Z0'
         UNITS3D( N ) = 'm'
         VDESC3D( N ) = 'surface roughness for land use category'

         N = N + 1 ! 6
         VTYPE3D( N ) = M3REAL
         VNAME3D( N ) = 'RST'
         UNITS3D( N ) = 'sec/m'
         VDESC3D( N ) = 'Stomatal resistance to water vapor'

         DO V = 1, dep_gas_all
            If( STAGE_Diag_Data%dep_map_gas( V ) ) Then

               N = N + 1 
               VNAME = 'VD_' // trim( STAGE_Diag_Data%dep_name_gas( V ) )
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = VNAME
               UNITS3D( N ) = 'cm/sec'
               VDESC3D( N ) = 'Gaseous Dry deposition velocity'

            End If
         END DO
         DO V = 1, N_AE_DEPV
            If( STAGE_Diag_Data%dep_map_aero( V ) ) Then

               N = N + 1 
               VNAME = trim( STAGE_Diag_Data%dep_name_aero( V ) )
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = VNAME
               UNITS3D( N ) = 'cm/sec'
               VDESC3D( N ) = 'Aerosol Dry deposition velocity'

            End If
         END DO
         
C Open diagnostic file

         IF ( .NOT. OPEN3( CTM_DEPV_MOS, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// TRIM( CTM_DEPV_MOS) // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN
      END
