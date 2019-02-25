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

!------------------------------------------------------------------------!
! This module contains essential data structure and functions for
! centralized I/O implementation

! Revision History:
!  02/01/19, D. Wong: initial implementation
!------------------------------------------------------------------------!

      MODULE CENTRALIZED_IO_MODULE

        use RUNTIME_VARS, only : LTNG_NO, STDATE, STTIME, ABFLUX, MOSAIC, NPTGRPS
        use CENTRALIZED_IO_UTIL_MODULE
   
        implicit none

        integer, parameter :: max_nfiles = 3

        character (20), parameter :: bio_season_fname = 'BIOSEASON'
        character (20), parameter :: biogemis_fname   = 'B3GRD'
        character (20), parameter :: dust_fname       = 'MODIS_FPAR'

        integer, parameter :: f_met_cro_2d  = 1
        integer, parameter :: f_met_cro_3d  = 2
        integer, parameter :: f_met_dot_3d  = 3

        integer :: file_sdate(max_nfiles)
        integer :: file_stime(max_nfiles)
        integer :: file_tstep(max_nfiles)
        real*8  :: file_xcell(max_nfiles)
        real*8  :: file_ycell(max_nfiles)

! time independent data
        real, allocatable :: MSFX2(:,:),        ! from GRID_CRO_2D data
     &                       LWMASK(:,:),       ! from GRID_CRO_2D data
     &                       HT(:,:),           ! from GRID_CRO_2D data
     &                       LAT(:,:),          ! from GRID_CRO_2D data
     &                       LON(:,:),          ! from GRID_CRO_2D data
     &                       PURB(:,:),         ! from GRID_CRO_2D data
     &                       LUFRAC(:,:,:),     ! from GRID_CRO_2D data
     &                       SOILCAT_A(:,:),    ! from MET_CRO_2D
     &                       MSFD2(:,:),        ! from GRID_DOT_2D data
     &                       X3HT0M(:,:),       ! from GRID_CRO_3D data
     &                       X3HT0F(:,:),       ! from GRID_CRO_3D data
     &                       ocean(:,:),        ! from OCEAN data
     &                       szone(:,:),        ! from OCEAN data
     &                       chlr(:,:),         ! from OCEAN data
     &                       dmsl(:,:),         ! from OCEAN data
     &                       OCEAN_MASK(:,:),   ! from LTNG parameter data
     &                       SLOPE(:,:),        ! from LTNG parameter data
     &                       INTERCEPT(:,:),    ! from LTNG parameter data
     &                       SLOPE_lg(:,:),     ! from LTNG parameter data
     &                       INTERCEPT_lg(:,:), ! from LTNG parameter data
     &                       ICCG_SUM(:,:),     ! from LTNG parameter data
     &                       ICCG_WIN(:,:),     ! from LTNG parameter data
     &                       AVGEMIS(:,:,:,:),  ! from BIOGEMIS data
     &                       AVGLAI(:,:,:,:),   ! from BIOGEMIS data
     &                       GROWAGNO(:,:),     ! from B3GRD data
     &                       NGROWAGNO(:,:),    ! from B3GRD data
     &                       NONAGNO(:,:),      ! from B3GRD data
     &                       RAINFALL(:,:,:)    ! from SOILINP data

        integer, allocatable :: PTYPE(:,:),        ! from SOILINP data
     &                          PULSEDATE(:,:),    ! from SOILINP data
     &                          PULSETIME(:,:)     ! from SOILINP data

        character( 16 ), allocatable :: DDTTM( : ) ! for SIOLINP data, description date and time

! time dependent data: 
! gridded 
        integer :: n_grid_cro_data_vars
        integer :: n_cio_grid_vars
        real, allocatable :: cio_grid_data(:)
        character (16), allocatable :: cio_grid_var_name(:,:)
        integer, allocatable :: cio_grid_data_inx (:,:,:),
     &                          head_grid(:), tail_grid(:),        ! head and tail of the gridded data circular buffer
     &                          cio_grid_data_tstamp(:,:,:)

        character (16) :: cio_dust_land_scheme

! boundary data
        integer :: n_cio_bndy_vars, n_cio_bc_file_vars
        real, allocatable :: cio_bndy_data(:)
        character (16), allocatable :: cio_bndy_var_name(:,:), cio_bc_file_var_name(:)
        integer, allocatable :: cio_bndy_data_inx (:,:,:),
     &                          head_bndy(:), tail_bndy(:),        ! head and tail of the boundary data circular buffer
     &                          cio_bndy_data_tstamp(:,:,:)

! emission data
! - gridded emission data
        character (16), allocatable :: cio_emis_file_name(:),
     &                                 cio_emis_var_name(:,:)
        integer, allocatable :: cio_emis_nvars(:)

! - stack emission data
        real, allocatable    :: cio_stack_data(:)
        character (16), allocatable :: cio_stack_file_name(:),
     &                                 cio_stack_var_name(:,:),
     &                                 STKGNAME( : ) ! stack groups file name

        integer, allocatable :: n_cio_stack_emis_vars(:),
     &                          n_cio_stack_emis_lays(:),
     &                          n_cio_stack_emis_pts(:),
     &                          cio_stack_emis_data_inx (:,:,:,:),
     &                          head_stack_emis(:,:), tail_stack_emis(:,:),  ! head and tail of the stack emis data circular buffer
     &                          cio_stack_emis_data_tstamp(:,:,:,:),
     &                          cio_stack_emis_sdate(:),
     &                          cio_stack_emis_stime(:)

        logical, allocatable :: cio_stack_emis_sdate_normal(:)               ! indication of date in file is same as model simulation 

        integer :: modis_data_sdate         ! modis dust data start date

        integer :: cio_model_sdate, 
     &             cio_model_stime,         ! model start date and time
     &             cio_data_tstep           ! grided data time step

        logical, private :: cio_LTNG_NO 

        real :: CONVPA             ! Pressure conversion factor file units to Pa  
        Real :: P0                 ! reference pressure (100000.0 Pa) for Potential Temperature,
                                   ! note that in meteorology they do not use the SI 1 ATM.  

! availability of various variable
        logical :: CFRAC_3D_AVAIL,          ! CFRAC_3D is available or not
     &             TSEASFC_AVAIL,           ! SST is available or not
     &             WSPD10_AVAIL,            ! WSPD10 is available or not
     &             UWINDC_AVAIL,            ! UWINDC is available in DOT file or not
     &             VWINDC_AVAIL,            ! VWINDC is available in DOT file or not
     &             USE_MARINE_GAS_EMISSION,
     &             QG_AVAIL,                ! flag for QG available in MET_CRO_3D
     &             QI_AVAIL,                ! flag for QI available in MET_CRO_3D
     &             QS_AVAIL,                ! flag for QS available in MET_CRO_3D
     &             QC_AVAIL,                ! flag for QC and it is always set to .true.
     &             JACOBF_AVAIL,            ! flag for JACOBF available in MET_CRO_3D
     &             RNA_AVAIL,               ! flag for RNA available in MET_CRO_2D
     &             RCA_AVAIL,               ! flag for RCA available in MET_CRO_2D
     &             RA_RS_AVAIL,             ! flag for RA and RS available in MET_CRO_2D
     &             Q2_AVAIL,                ! flag for Q2, two meter mixing ratio available in MET_CRO_2D
     &             LH_AVAIL,                ! flag for LH, two meter mixing ratio available in MET_CRO_2D
     &             HAS_SEAICE,              ! flag for SEAICE in MET_CRO_2D
     &             WR_AVAIL,                ! flag for WR, canopy wetness available in MET_CRO_2D
     &             INIT_MEDC_1_AVAIL,       ! file INIT_MEDC_1 is available
     &             E2C_FERT_AVAIL           ! file E2C_FERT is available

! Met data is large enough to cover boundary and no MET_BDY_3D will be used
        logical :: window

        logical :: east_pe, south_pe, west_pe, north_pe

        INTEGER :: TEMPG_LOC
        INTEGER :: TSEASFC_LOC

        integer :: STRTCOLGC2,  ENDCOLGC2,  STRTROWGC2,  ENDROWGC2,
     &             STRTCOLGD2,  ENDCOLGD2,  STRTROWGD2,  ENDROWGD2,
     &             STRTCOLMC2,  ENDCOLMC2,  STRTROWMC2,  ENDROWMC2,
     &             STRTCOLMC2x, ENDCOLMC2x, STRTROWMC2x, ENDROWMC2x,  ! extension setup for READMC2
     &             STRTCOLMC3,  ENDCOLMC3,  STRTROWMC3,  ENDROWMC3,
     &             STRTCOLMD3,  ENDCOLMD3,  STRTROWMD3,  ENDROWMD3,
     &             STRTCOLMD3x, ENDCOLMD3x, STRTROWMD3x, ENDROWMD3x   ! extension setup for READMD3

        private :: gridded_files_setup,
     &             boundary_files_setup,
     &             retrieve_grid_cro_2d_data,
     &             retrieve_grid_dot_2d_data,
     &             retrieve_ocean_data

        integer, private :: count = 0
        integer, private :: cio_logdev, 
     &                      n_c2d,  size_c2d,    ! cro 2d file info: # of variables and a variable size
     &                              size_c2dx,   ! extended cro 2d variable size
     &                              size_d2d,    ! a 2d dot variable size
     &                              size_d2dx,   ! extended 2d dot variable spatial size
     &                      n_c3d,  size_c3d,    ! cro 3d file info: # of variables and a variable size
     &                      n_d3d,  size_d3d,    ! dot 3d file info: # of variables and a variable size
     &                              size_d3dx,   ! extended dot 3d variable size
     &                      n_e2d,               ! # of 2d emission variables
     &                      n_e3d,  size_e3d,    ! # of 3d emission variables and a variable size
     &                      n_mb3d,              ! # of 3d met boundary variables
     &                      n_b3d,               ! # of 3d boundary variables
     &                              size_b3d,    ! a 3d boundary variable size
     &                              size_b2d,    ! a 2d boundary variable size
     &                      n_l2d,               ! # of lightning strikes file variables
     &                              size_lt      ! lightning file variable size

        integer, private ::   cro_ncols,   cro_nrows,   ! cro file nools and nrows 
     &                      w_cro_ncols, w_cro_nrows,   ! window cro file nools and nrows 
     &                      x_cro_ncols, x_cro_nrows,   ! extended cro file nools and nrows 
     &                        dot_ncols,   dot_nrows,   ! dot file nools and nrows 
     &                      x_dot_ncols, x_dot_nrows    ! extended dot file nools and nrows 

        integer, private :: cio_LTLYRS                  ! number of layers in lightning strike dataset

        interface interpolate_var
          module procedure r_interpolate_var_1dx,
     &                     r_interpolate_var_1ds,
     &                     r_interpolate_var_2d,
     &                     i_interpolate_var_2d,
     &                     r_interpolate_var_2dx,
     &                     r_interpolate_var_2db,
     &                     r_interpolate_var_3d
        end interface

        contains

! -------------------------------------------------------------------------
        subroutine gridded_files_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, only : VGTYP_GD, nlays
          USE RUNTIME_VARS, only : N_FILE_GR, BIOGEMIS, BIOGEMIS_SEASON, WB_DUST, ILDEPV
          use LSM_Mod, only : LAND_SCHEME

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'gridded_files_setup'
          CHARACTER( 40 ), parameter :: NLDN_STRIKES = 'NLDN_STRIKES'

          CHARACTER( 120 ) :: XMSG = ' '
          INTEGER          :: GXOFF, GYOFF, stat, n, v, d_size, begin, end, adj,
     &                        n_bio_season_vars, n_dust_vars
          character( 16 )  :: tname, fname

          character( 20 ), allocatable :: c2d_name(:, :), c3d_name(:, :), 
     &                                    d3d_name(:,:), emis_name(:,:),
     &                                    l2d_name(:,:), medc_name(:,:)
          logical :: done = .false.

          IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// GRID_CRO_2D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
             XMSG = 'Could not get ' // GRID_CRO_2D //' file description'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          n_grid_cro_data_vars = nvars3d
          CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                    STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

          LAND_SCHEME = 'UNKNOWN'

          v = 0
          DO WHILE ((v .LT. NVARS3D) .and. (.not. done))
             v = v + 1
             IF ( VNAME3D( v ) .EQ. 'DLUSE' ) THEN
                IF ( INDEX( VDESC3D( v ), 'USGS24' ) .NE. 0 ) THEN
                   LAND_SCHEME = 'USGS24'
                   cio_dust_land_scheme = 'USGS24'
                ELSE IF ( INDEX( VDESC3D( v ), 'NLCD40' ) .NE. 0 ) THEN
                   LAND_SCHEME = 'NLCD40'
                   cio_dust_land_scheme = 'NLCD40'
                ELSE IF ( INDEX( VDESC3D( v ), 'NLCD50' ) .NE. 0 ) THEN
                   LAND_SCHEME = 'NLCD50'
                   cio_dust_land_scheme = 'NLCD50'
                ELSE IF ( INDEX( VDESC3D( v ), 'NLCD-MODIS' ) .NE. 0 ) THEN
                   LAND_SCHEME = 'NLCD50'
                   cio_dust_land_scheme = 'NLCD-MODIS'
                ELSE IF ( INDEX( VDESC3D( v ), 'MODIS' ) .NE. 0 ) THEN
                   LAND_SCHEME = 'MODIS'
                   IF ( INDEX( VDESC3D( v ), 'MODIS NOAH' ) .ne.  0) THEN
                      cio_dust_land_scheme = 'MODIS_NOAH'
                   ELSE
                      cio_dust_land_scheme = 'MODIS'
                   END IF
                END IF
                done = .true.
             END IF
          END DO

          IF ( .NOT. OPEN3( GRID_DOT_2D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// GRID_DOT_2D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          CALL SUBHFILE ( GRID_DOT_2D, GXOFF, GYOFF,
     &                    STRTCOLGD2, ENDCOLGD2, STRTROWGD2, ENDROWGD2 )

! met cro 2d file
          IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// MET_CRO_2D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
             XMSG = 'Could not get ' // MET_CRO_2D //' file description'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          file_sdate(f_met_cro_2d) = sdate3d
          file_stime(f_met_cro_2d) = stime3d
          file_tstep(f_met_cro_2d) = tstep3d
          file_xcell(f_met_cro_2d) = xcell3d
          file_ycell(f_met_cro_2d) = ycell3d

          IF (INDEX1( 'TSEASFC', NVARS3D, VNAME3D ) .gt. 0) then
             TSEASFC_AVAIL = .true.
             adj = 0
          else
             TSEASFC_AVAIL = .false.
             adj = 1
          end if

          HAS_SEAICE = (INDEX1( 'SEAICE', NVARS3D, VNAME3D ) .gt. 0)

! include an additional variable TSEASFC when MET_CRO_2D does not have it and CMAQ code is looking for it
          n_c2d = nvars3d + 1
          allocate (c2d_name(n_c2d, 2), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating c2d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          c2d_name = '2'    ! denote 2d variable
          c2d_name(1:nvars3d,1) = vname3d(1:nvars3d) 
          if (adj .eq. 1) then
             c2d_name(n_c2d,1) = 'TSEASFC'
          end if

          WSPD10_AVAIL = (INDEX1( 'WSPD10', NVARS3D, VNAME3D ) .gt. 0)
          RNA_AVAIL    = (INDEX1( 'RNA', NVARS3D, VNAME3D ) .gt. 0)
          RCA_AVAIL    = (INDEX1( 'RCA', NVARS3D, VNAME3D ) .gt. 0)
          RA_RS_AVAIL  = (INDEX1( 'RA', NVARS3D, VNAME3D ) .gt. 0)
          WR_AVAIL     = (INDEX1( 'WR', NVARS3D, VNAME3D ) .gt. 0)
          Q2_AVAIL     = (INDEX1( 'Q2', NVARS3D, VNAME3D ) .gt. 0)
          LH_AVAIL     = (INDEX1( 'LH', NVARS3D, VNAME3D ) .gt. 0)

          CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                    STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

          STRTCOLMC2x = STRTCOLMC2
          STRTROWMC2x = STRTROWMC2
          if (north_pe .and. east_pe) then
             ENDCOLMC2x = ENDCOLMC2
             ENDROWMC2x = ENDROWMC2
          else if (north_pe) then
             ENDCOLMC2x = ENDCOLMC2 + 1
             ENDROWMC2x = ENDROWMC2
          else if (east_pe) then
             ENDCOLMC2x = ENDCOLMC2
             ENDROWMC2x = ENDROWMC2 + 1
          else
             ENDROWMC2x = ENDROWMC2 + 1
             ENDCOLMC2x = ENDCOLMC2 + 1
          end if

! met cro 3d file
          IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// MET_CRO_3D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
             XMSG = 'Could not get ' // MET_CRO_3D //' file description'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          file_sdate(f_met_cro_3d) = sdate3d
          file_stime(f_met_cro_3d) = stime3d
          file_tstep(f_met_cro_3d) = tstep3d
          file_xcell(f_met_cro_3d) = xcell3d
          file_ycell(f_met_cro_3d) = ycell3d

          n_c3d = nvars3d
          allocate (c3d_name(n_c3d, 2), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating c3d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          c3d_name = '3'    ! denote 3d variable
          c3d_name(:,1) = vname3d(1:n_c3d) 

          CFRAC_3D_AVAIL = (INDEX1( 'CFRAC_3D', NVARS3D, VNAME3D ) .gt. 0)
          QI_AVAIL       = (INDEX1( 'QI', NVARS3D, VNAME3D ) .gt. 0)
          QS_AVAIL       = (INDEX1( 'QS', NVARS3D, VNAME3D ) .gt. 0)
          QG_AVAIL       = (INDEX1( 'QG', NVARS3D, VNAME3D ) .gt. 0)
          JACOBF_AVAIL   = (INDEX1( 'JACOBF', NVARS3D, VNAME3D ) .gt. 0)
          QC_AVAIL       = .true.

          cio_data_tstep = tstep3d

          CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                    STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

          IF ( (ENDCOLMC3 - STRTCOLMC3 + 1) .NE. NCOLS .OR.
     &         (ENDROWMC3 - STRTROWMC3 + 1) .NE. NROWS ) THEN
               WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &         (ENDCOLMC3 - STRTCOLMC3 + 1), NCOLS, (ENDROWMC3 - STRTROWMC3 + 1), NROWS
             CALL M3EXIT ( PNAME, cio_model_sdate, cio_model_stime, XMSG, XSTAT1 )
          END IF

          IF ( GXOFF .NE. 0 .AND. GYOFF .NE. 0 ) THEN
             window = .TRUE. ! windowing from file
             STRTCOLMC3 = STRTCOLMC3 - 1
             ENDCOLMC3  = ENDCOLMC3 + 1
             STRTROWMC3 = STRTROWMC3 - 1
             ENDROWMC3  = ENDROWMC3 + 1
             w_cro_ncols = ENDCOLMC3 - STRTCOLMC3 + 1
             w_cro_nrows = ENDROWMC3 - STRTROWMC3 + 1
          ELSE
             window = .FALSE.
             w_cro_ncols = -1
             w_cro_nrows = -1
             if (.not. east_pe) then
                ENDCOLMC3  = ENDCOLMC3 + 1
             end if
             if (.not. north_pe) then
                ENDROWMC3  = ENDROWMC3 + 1
             end if
          END IF

          V = INDEX1( 'PRES', NVARS3D, VNAME3D )
          If ( V .eq. 0 ) Then
             XMSG = 'Could not get variable PRES from ' // MET_CRO_3D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          Select Case (UNITS3D( V ))
             Case ( 'PASCAL','pascal','Pascal','PA','pa','Pa' )
                CONVPA = 1.0
                P0     = 100000.0
             Case ( 'MILLIBAR','millibar','Millibar','MB','mb','Mb' )
                CONVPA = 1.0E-02
                P0     = 100000.0 * CONVPA
             Case ( 'CENTIBAR','centibar','Centibar','CB','cb','Cb' )
                CONVPA = 1.0E-03
                P0     = 100000.0 * CONVPA
             Case Default
                XMSG = 'PRES units incorrect on ' // MET_CRO_3D
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End Select

! met dot 3d file
          IF ( .NOT. OPEN3( MET_DOT_3D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// MET_DOT_3D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( MET_DOT_3D ) ) THEN
             XMSG = 'Could not get description of file  '// MET_DOT_3D 
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          file_sdate(f_met_dot_3d) = sdate3d
          file_stime(f_met_dot_3d) = stime3d
          file_tstep(f_met_dot_3d) = tstep3d
          file_xcell(f_met_dot_3d) = xcell3d
          file_ycell(f_met_dot_3d) = ycell3d

          n_d3d = nvars3d
          allocate (d3d_name(n_d3d, 2), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating d3d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          d3d_name = 'd'     ! denote dot variable
          d3d_name(:,1) = vname3d(1:n_d3d) 

          CALL SUBHFILE ( MET_DOT_3D, GXOFF, GYOFF,
     &                    STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3 )

          STRTCOLMD3x = STRTCOLMD3
          STRTROWMD3x = STRTROWMD3
          if (north_pe .and. east_pe) then
             ENDCOLMD3x = ENDCOLMD3
             ENDROWMD3x = ENDROWMD3
          else if (north_pe) then
             ENDCOLMD3x = ENDCOLMD3 + 1
             ENDROWMD3x = ENDROWMD3
          else if (east_pe) then
             ENDCOLMD3x = ENDCOLMD3
             ENDROWMD3x = ENDROWMD3 + 1
          else
             ENDROWMD3x = ENDROWMD3 + 1
             ENDCOLMD3x = ENDCOLMD3 + 1
          end if

          dot_ncols = ENDCOLMD3 - STRTCOLMD3 + 1
          dot_nrows = ENDROWMD3 - STRTROWMD3 + 1
          size_d3d  = dot_ncols * dot_nrows * nlays

          x_dot_ncols = ENDCOLMD3x - STRTCOLMD3x + 1
          x_dot_nrows = ENDROWMD3x - STRTROWMD3x + 1
          size_d2dx = x_dot_ncols * x_dot_nrows
          size_d3dx = size_d2dx * nlays

          UWINDC_AVAIL = (INDEX1( 'UWINDC', NVARS3D, VNAME3D ) .gt. 0)
          VWINDC_AVAIL = (INDEX1( 'VWINDC', NVARS3D, VNAME3D ) .gt. 0)

! emission file, could be one or multiple layer

          allocate (cio_emis_file_name(N_FILE_GR),
     &              cio_emis_nvars(N_FILE_GR),
     &              stat=stat)

          n_e2d = 0
          n_e3d = 0
          do n = 1, N_FILE_GR
             write (fname, '(a8, i3.3)') "GR_EMIS_", n

             cio_emis_file_name(n) = fname

             IF ( .NOT. OPEN3( fname, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// fname // ' file'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             IF ( .NOT. DESC3( fname ) ) THEN
                XMSG = 'Could not get description of file  '// fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF

             cio_emis_nvars(n) = nvars3d
             if (nlays3d .eq. 1) then
                n_e2d = n_e2d + cio_emis_nvars(n)
             else
                n_e3d = n_e3d + nvars3d
             end if
          end do

! Bio_season data
          n_bio_season_vars = 0
          if (BIOGEMIS_SEASON) then
             n_bio_season_vars = 1
             IF ( .NOT. OPEN3( bio_season_fname, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// bio_season_fname // ' file'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             IF ( .NOT. DESC3( bio_season_fname ) ) THEN
                XMSG = 'Could not get description of file  '// bio_season_fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
          end if

! Wind blown dust data
          n_dust_vars = 0
          if (WB_DUST) then
             IF ( .NOT. OPEN3( dust_fname, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// dust_fname // ' file'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             IF ( .NOT. DESC3( dust_fname ) ) THEN
                XMSG = 'Could not get description of file  '// dust_fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             n_dust_vars = nvars3d
             modis_data_sdate = sdate3d
          end if

          n_e2d = n_e2d + n_bio_season_vars + n_dust_vars

          allocate (emis_name(n_e2d+n_e3d, 2), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating emis_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

          if (BIOGEMIS_SEASON) then
             IF ( .NOT. DESC3( bio_season_fname ) ) THEN
                XMSG = 'Could not get description of file  '// bio_season_fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             begin = n_e2d+n_e3d-n_dust_vars
             emis_name(begin:begin, 2) = 'bs'          ! bs denote bio season
             emis_name(begin:begin, 1) = vname3d(1)
          end if

          if (WB_DUST) then
             IF ( .NOT. DESC3( dust_fname ) ) THEN
                XMSG = 'Could not get description of file  '// dust_fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             begin = n_e2d+n_e3d-n_dust_vars+1
             end   = begin + n_dust_vars - 1
             emis_name(begin:end, 2) = 'wb'          ! bs denote bio season
             emis_name(begin:end, 1) = vname3d(1:nvars3d)
          end if

! setup gridded emission file
          end = 0
          do n = 1, N_FILE_GR
             WRITE (fname, '(a8, i3.3)') "GR_EMIS_", n
             IF ( .NOT. DESC3( fname ) ) THEN
                XMSG = 'Could not get description of file  '// fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF

             begin = end + 1

             write (tname, '(a1, i3.3)') '_', n
             do v = 1, nvars3d
                end = end + 1
                emis_name(end,1) = trim(vname3d(v)) // tname
             end do

             if (nlays3d .eq. 1) then
                emis_name(begin:end, 2) = 'e'          ! e denote emission 2d variable
             else
                emis_name(begin:end, 2) = 'E'          ! E denote emission 2d variable
             end if

          end do

! lightning file
          n_l2d = 0
          if (cio_LTNG_NO) then
             IF ( .NOT. OPEN3( NLDN_STRIKES, FSREAD3, PNAME ) ) THEN
                XMSG = 'Open failure for ' // NLDN_STRIKES
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             IF ( .NOT. DESC3( NLDN_STRIKES ) ) THEN
                XMSG = 'Could not get description of file  '// NLDN_STRIKES 
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             n_l2d = nvars3d
             cio_LTLYRS = nlays3d
             allocate (l2d_name(n_l2d, 2), stat=stat)
             if (stat .ne. 0) then
                xmsg = 'Failure allocating l2d_name '
                call m3exit (pname, 0, 0, xmsg, xstat1 )
             end if
             l2d_name = 'l'     ! denote lightning variable
             l2d_name(:,1) = vname3d(1:n_l2d) 
          end if

! combining all files
          n_cio_grid_vars = n_c2d + n_c3d + n_d3d + n_e2d + n_e3d + n_l2d

          cro_ncols = ENDCOLMC2 - STRTCOLMC2 + 1
          cro_nrows = ENDROWMC2 - STRTROWMC2 + 1
          size_c2d = cro_ncols * cro_nrows

          if ((cro_ncols .ne. ncols) .or. (cro_nrows .ne. nrows)) then
             print *, ' ==d== NO ncols nrows '
             stop
          end if

          x_cro_ncols = ENDCOLMC2x - STRTCOLMC2x + 1
          x_cro_nrows = ENDROWMC2x - STRTROWMC2x + 1
          size_c2dx = x_cro_ncols * x_cro_nrows

          size_d2d = dot_ncols * dot_nrows

          if (window) then
             size_c3d = w_cro_ncols * w_cro_nrows * nlays
          else
             size_c3d = size_c2dx * nlays
          end if
          size_e3d = size_c2d * nlays

          size_lt = size_c2d * cio_LTLYRS

          allocate (cio_grid_var_name(n_cio_grid_vars, 2),
     &              cio_grid_data_inx(2, 0:2, n_cio_grid_vars),
     &              head_grid(n_cio_grid_vars),
     &              tail_grid(n_cio_grid_vars),
     &              cio_grid_data_tstamp(2, 0:2, n_cio_grid_vars),
     &              cio_grid_data(  size_c2dx * 3 * n_c2d               ! 2d met data
     &                            + size_c2d  * 3 * n_e2d               ! 2d emis data
     &                            + size_c3d  * 3 * n_c3d                ! 3D met data
     &                            + size_e3d  * 3 * n_e3d                ! 3d emis data 
     &                            + size_d3dx * 3 * n_d3d                ! 3d dot data
     &                            + size_lt   * 3 * n_l2d),              ! lightning data
     &              stat = stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating cio_grid_var_name and associated arrays '
               call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

          begin = 1
          end = n_c2d
          cio_grid_var_name(begin:end, :) = c2d_name

          begin = end + 1
          end = end + n_c3d
          cio_grid_var_name(begin:end, :) = c3d_name

          begin = end + 1
          end = end + n_d3d
          cio_grid_var_name(begin:end, :) = d3d_name

          begin = end + 1
          end = end + n_e2d + n_e3d
          cio_grid_var_name(begin:end, :) = emis_name

          if (cio_LTNG_NO) then
             begin = end + 1
             end = end + n_l2d
             cio_grid_var_name(begin:end, :) = l2d_name
             deallocate (l2d_name)
          end if

          deallocate (c2d_name, c3d_name, emis_name)
          if (.not. window) then
             deallocate (d3d_name)
          end if

          call quicksort(cio_grid_var_name, 1, n_cio_grid_vars)

          begin = 1
          do v = 1, n_cio_grid_vars

! locate certain species
             if (cio_grid_var_name(v,1) .eq. 'TEMPG') then
                tempg_loc = v
             else if (cio_grid_var_name(v,1) .eq. 'TSEASFC') then
                tseasfc_loc = v
             end if

             if (cio_grid_var_name(v,2) .eq. '2')  then
                d_size = size_c2dx
             else if (cio_grid_var_name(v,2) .eq. 'e') then
                d_size = size_c2d
             else if (cio_grid_var_name(v,2) .eq. '3') then
                d_size = size_c3d
             else if (cio_grid_var_name(v,2) .eq. 'E') then
                d_size = size_e3d
             else if (cio_grid_var_name(v,2) .eq. 'd') then
                d_size = size_d3dx
             else if ((cio_grid_var_name(v,2) .eq. 'l') .or.
     &                (cio_grid_var_name(v,2) .eq. 'bs') .or.
     &                (cio_grid_var_name(v,2) .eq. 'wb')) then
                d_size = size_c2d
             else
                print *, ' ==d== UNKNOWN'
                stop
             end if

             do n = 0, 2
                 cio_grid_data_inx(1, n, v) = begin
                 end = begin + d_size - 1
                 cio_grid_data_inx(2, n, v) = end
                 begin = end + 1
             end do
          end do

        end subroutine gridded_files_setup

! -------------------------------------------------------------------------
        subroutine boundary_files_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, only : VGTYP_GD, nlays

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'boundary_files_setup'

          CHARACTER( 120 ) :: XMSG = ' '
          INTEGER          :: GXOFF, GYOFF, stat, n, v, d_size, begin, end

          character( 16 ), allocatable :: b3d_name(:,:)
          character( 16 ) :: mb3d_name(2, 2)

! MET_BDY_3D file, need to be opened when window is F
          if (.not. window) then
             IF ( .NOT. OPEN3( MET_BDY_3D, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// MET_BDY_3D // ' file'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             n_mb3d = 2
             mb3d_name = 'mb'     ! denote met 3D boundary variable
             mb3d_name(1,1) = 'DENSA_J'
             mb3d_name(2,1) = 'JACOBM'
          else
             n_mb3d = 0
          end if

! BCON file
          IF ( .NOT. OPEN3( BNDY_GASC_1, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// BNDY_GASC_1 // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( BNDY_GASC_1 ) ) THEN
             XMSG = 'Could not get description of file  '// BNDY_GASC_1 
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          n_b3d = nvars3d
          size_b2d = (ncols3d + nrows3d + 2 * nthik3d) * 2 * nthik3d
          size_b3d = size_b2d * nlays

          allocate (b3d_name(n_b3d, 2),
     &              cio_bc_file_var_name(nvars3d),
     &              stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating mb3d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          b3d_name = 'b'     ! denote 3D boundary variable
          b3d_name(:,1) = vname3d(1:nvars3d) 
          cio_bc_file_var_name = vname3d(1:nvars3d)
          n_cio_bc_file_vars = nvars3d

! combining all files
          n_cio_bndy_vars = n_mb3d + n_b3d

          allocate (cio_bndy_var_name(n_cio_bndy_vars, 2),
     &              cio_bndy_data_inx(2, 0:2, n_cio_bndy_vars),
     &              head_bndy(n_cio_bndy_vars),
     &              tail_bndy(n_cio_bndy_vars),
     &              cio_bndy_data_tstamp(2, 0:2, n_cio_bndy_vars),
     &              cio_bndy_data(size_b3d * 3 * (n_mb3d + n_b3d)),   ! boundary data
     &              stat = stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating cio_bndy_var_name and associated arrays '
               call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

          begin = 1
          end = n_b3d
          cio_bndy_var_name(begin:end, :) = b3d_name
          if (.not. window) then
             begin = end + 1
             end = end + 2
             cio_bndy_var_name(begin:end, :) = mb3d_name
          end if

          deallocate (b3d_name)

          call quicksort(cio_bndy_var_name, 1, n_cio_bndy_vars)

          begin = 1
          do v = 1, n_cio_bndy_vars

             do n = 0, 2
                 cio_bndy_data_inx(1, n, v) = begin
                 end = begin + size_b3d - 1
                 cio_bndy_data_inx(2, n, v) = end
                 begin = end + 1
             end do
          end do

        end subroutine boundary_files_setup

! -------------------------------------------------------------------------
        subroutine stack_files_setup

          USE UTILIO_DEFN
          USE STK_PRMS
          USE stack_group_data_module
          USE HGRD_DEFN, only : XORIG_GD, YORIG_GD, XCELL_GD, YCELL_GD

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'stack_files_setup'

          CHARACTER( 120 ) :: XMSG = ' '
          integer :: n, v, pt, max_nsrc_pts, max_nvars, begin, end, stat, delta
          integer, allocatable :: d_size(:), pt_size(:),
     &                            stk_gp_sdate(:), stk_gp_stime(:),
     &                            stk_gp_nlays(:)

          allocate (cio_stack_file_name(NPTGRPS), 
     &              n_cio_stack_emis_vars(NPTGRPS), 
     &              n_cio_stack_emis_lays(NPTGRPS), 
     &              n_cio_stack_emis_pts(NPTGRPS),
     &              cio_stack_emis_sdate(NPTGRPS),
     &              cio_stack_emis_stime(NPTGRPS),
     &              cio_stack_emis_sdate_normal(NPTGRPS),
     &              STKGNAME(NPTGRPS),
     &              d_size(NPTGRPS),
     &              pt_size(NPTGRPS),
     &              stk_gp_sdate(NPTGRPS),
     &              stk_gp_stime(NPTGRPS),
     &              stk_gp_nlays(NPTGRPS),
     &              FIRE_ON(NPTGRPS),
     &              NSRC(NPTGRPS),
     &              stat=stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating cio_stack_file_name and other arrays'
               call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

          FIRE_ON = .FALSE.   ! array assignment
! go through all stack group one time to figure out max number of source points
          STKGNAME = ' '   ! array
          DO N = 1, NPTGRPS
             WRITE( STKGNAME( N ),'( "STK_GRPS_",I3.3 )' ) N
          END DO

          do N = 1, NPTGRPS
             IF ( .NOT. OPEN3( STKGNAME( N ), FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// TRIM( STKGNAME( N ) ) // ' file'
                call m3exit (pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             END IF

             IF ( .NOT. DESC3( STKGNAME( N ) ) ) THEN
                XMSG = 'Could not get ' // TRIM( STKGNAME( N ) ) // ' file description'
                call m3exit (pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             END IF

             stk_gp_sdate(n) = sdate3d
             stk_gp_stime(n) = stime3d
             stk_gp_nlays(n) = nlays3d

             NSRC( N ) = NROWS3D

             DO V = 1, NVARS3D
                IF ( VNAME3D( V ) .EQ. 'ACRESBURNED' ) FIRE_ON( N ) = .TRUE.
             END DO
          end do
          max_nsrc_pts = maxval(NSRC)

          allocate (xloca(max_nsrc_pts, NPTGRPS),
     &              yloca(max_nsrc_pts, NPTGRPS),
     &              stkid(max_nsrc_pts, NPTGRPS),
     &              stat = stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating other stack group variable arrays'
             call m3exit (pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
          end if

! read in stack group data

          do N = 1, NPTGRPS
             IF ( .NOT. READ3( STKGNAME( N ), 'XLOCA', ALLAYS3,
     &                         stk_gp_sdate(n), stk_gp_stime(n), XLOCA(:,N) ) ) THEN
                XMSG = 'Could not read XLOCA from ' // TRIM( STKGNAME( N))
                call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
             END IF

             IF ( .NOT. READ3( STKGNAME( N ), 'YLOCA', ALLAYS3,
     &                         stk_gp_sdate(n), stk_gp_stime(n), YLOCA(:,N) ) ) THEN
                XMSG = 'Could not read YLOCA from ' // TRIM( STKGNAME( N))
                call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
             END IF

             IF ( .NOT. READ3( STKGNAME( N ), 'ISTACK', ALLAYS3,
     &                         stk_gp_sdate(n), stk_gp_stime(n), STKID(:,N) ) ) THEN
                XMSG = 'Could not read ISTACK from ' // TRIM( STKGNAME( N) )
                call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
             END IF
          end do

          IF ( .NOT. STK_PRMS_INIT( STKGNAME ) ) THEN
             write (cio_logdev, *) 'Could not initialize stack parameters'
             stop
          END IF

          do N = 1, NPTGRPS

             IF ( MY_NSRC( N ) .GT. 0 ) THEN

                IF ( .NOT. XTRACT3( STKGNAME( N ), 'STKDM', 1, stk_gp_nlays(n),
     &                              MY_STRT_SRC( N ), MY_END_SRC( N ),
     &                              1, 1, stk_gp_sdate(n), stk_gp_stime(n), STKDIAM( N )%ARRY) ) THEN
                   XMSG = 'Could not read STKDM from ' // TRIM( STKGNAME( N ) )
                   call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
                END IF

                IF ( .NOT. XTRACT3( STKGNAME( N ), 'STKHT', 1, stk_gp_nlays(n),
     &                              MY_STRT_SRC( N ), MY_END_SRC( N ),
     &                              1, 1, stk_gp_sdate(n), stk_gp_stime(n), STKHT( N )%ARRY) ) THEN
                   XMSG = 'Could not read STKHT from ' // TRIM( STKGNAME( N ) )
                   call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
                END IF

                IF ( .NOT. XTRACT3( STKGNAME( N ), 'STKTK', 1, stk_gp_nlays(n),
     &                              MY_STRT_SRC( N ), MY_END_SRC( N ),
     &                              1, 1, stk_gp_sdate(n), stk_gp_stime(n), STKTK( N )%ARRY) ) THEN
                   XMSG = 'Could not read STKTK from ' // TRIM( STKGNAME( N ) )
                   call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
                END IF

                IF ( .NOT. XTRACT3( STKGNAME( N ), 'STKVE', 1, stk_gp_nlays(n),
     &                              MY_STRT_SRC( N ), MY_END_SRC( N ),
     &                              1, 1, stk_gp_sdate(n), stk_gp_stime(n), STKVEL( N )%ARRY) ) THEN
                   XMSG = 'Could not read STKVE from ' // TRIM( STKGNAME( N ) )
                   call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
                END IF

                IF ( FIRE_ON( N ) ) THEN
                   IF ( .NOT. XTRACT3( STKGNAME( N ), 'ACRESBURNED', 1, stk_gp_nlays(n),
     &                                 MY_STRT_SRC( N ), MY_END_SRC( N ),
     &                                 1, 1, stk_gp_sdate(n), stk_gp_stime(n), ACRES_BURNED( N )%ARRY) ) THEN
                      XMSG = 'Could not read ACRESBURNED from ' // TRIM( STKGNAME( N ) )
                      call m3exit (pname, stk_gp_sdate(n), stk_gp_stime(n), xmsg, xstat1 )
                   END IF
                END IF

             END IF

          end do

! process stack emission files
          max_nvars = 0
          d_size    = 0
          do pt = 1, NPTGRPS
             WRITE( cio_stack_file_name(pt), '( "STK_EMIS_",I3.3 )' ) pt

             IF ( .NOT. OPEN3( cio_stack_file_name( pt ), FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// TRIM( cio_stack_file_name( pt ) ) // ' file'
                CALL M3MESG( XMSG )
             END IF

             IF ( .NOT. DESC3( cio_stack_file_name( pt ) ) ) THEN
                XMSG = 'Could not get ' // TRIM( cio_stack_file_name( pt ) ) // ' file description'
                CALL M3MESG( XMSG )
             END IF
             n_cio_stack_emis_vars(pt) = nvars3d
             n_cio_stack_emis_lays(pt) = nlays3d
             n_cio_stack_emis_pts(pt)  = nrows3d
             cio_stack_emis_sdate(pt)  = sdate3d
             cio_stack_emis_stime(pt)  = stime3d

             cio_stack_emis_sdate_normal(pt) = .true.

             if (max_nvars .lt. nvars3d) then
                max_nvars = nvars3d
             end if
             if (MY_STRT_SRC(pt) .gt. 0) then
                pt_size(pt) = (MY_END_SRC(pt) - MY_STRT_SRC(pt) + 1) * n_cio_stack_emis_lays(pt)
                d_size(pt)  = nvars3d * pt_size(pt) * 3
             else
                pt_size(pt) = 0
                d_size(pt)  = 0
             end if

          end do

          allocate (cio_stack_var_name(max_nvars, NPTGRPS), 
     &              head_stack_emis(max_nvars, NPTGRPS), 
     &              tail_stack_emis(max_nvars, NPTGRPS), 
     &              cio_stack_emis_data_inx(2, 0:2, max_nvars, NPTGRPS), 
     &              cio_stack_emis_data_tstamp(2, 0:2, max_nvars, NPTGRPS), 
     &              cio_stack_data(sum(d_size)),
     &              stat = stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating other stack arrays'
               call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

          begin = 1
          cio_stack_emis_data_inx = -1
          do pt = 1, NPTGRPS
             IF ( .NOT. DESC3( cio_stack_file_name( pt ) ) ) THEN
                XMSG = 'Could not get ' // TRIM( cio_stack_file_name( pt ) ) // ' file description'
                CALL M3MESG( XMSG )
             END IF

             cio_stack_var_name(1:nvars3d, pt) = vname3d(1:nvars3d)
             call quicksort(cio_stack_var_name(:,pt), 1, nvars3d)

             if (MY_NSRC(pt) .gt. 0) then
                do v = 1, nvars3d
                   do n = 0, 2
                      cio_stack_emis_data_inx(1,n,v,pt) = begin
                      end = begin + pt_size(pt) - 1
                      cio_stack_emis_data_inx(2,n,v,pt) = end
                      begin = end + 1
                   end do
                end do
             end if
          end do

          deallocate (d_size)

        end subroutine stack_files_setup

! -------------------------------------------------------------------------
        subroutine biogemis_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE biog_emis_param_module

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'biogemis_setup'

          CHARACTER( 120 ) :: XMSG = ' '
          CHARACTER( 256 ) :: MESG
          CHARACTER( 16 ) :: VAR
          INTEGER :: STAT, i, j, k

          ALLOCATE( AVGEMIS( NCOLS,NROWS,NSEF-1,NSEASONS ),
     &              AVGLAI( NCOLS,NROWS,NLAI,NSEASONS ), 
     &              STAT=STAT )

          IF ( .NOT. OPEN3( biogemis_fname, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open ' // trim(biogemis_fname) // ' file'
             CALL M3MESG( XMSG )
          END IF

          IF ( .NOT. DESC3( biogemis_fname ) ) THEN
             XMSG = 'Could not get ' // trim(biogemis_fname) // ' file description'
             CALL M3MESG( XMSG )
          END IF

C Read the various categories of normalized emissions
          DO I = 1, NSEASONS

             DO J = 1, NSEF-1
                VAR = 'AVG_' // TRIM( BIOTYPES( J ) ) // SEASON( I )

                IF ( .NOT. XTRACT3( biogemis_fname, VAR,
     &                              1,1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                              0, 0, AVGEMIS( :,:,J,I ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( biogemis_fname ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
             END DO

             DO K = 1, NLAI
                VAR = 'LAI_' // TRIM( LAITYPES( K ) ) // SEASON( I )
  
                IF ( .NOT. XTRACT3( biogemis_fname, VAR,
     &                              1,1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                              0, 0, AVGLAI( :,:,K,I ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( biogemis_fname ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
             END DO

          END DO   ! end loop over seasons

        end subroutine biogemis_setup

! -------------------------------------------------------------------------
        subroutine b3grd_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows

          Character( 40 ), parameter :: pname = 'b3grd_setup'
          Character( 40 ), parameter :: fname = 'B3GRD'

          CHARACTER( 256 ) :: MESG
          CHARACTER( 16 ) :: VAR
          INTEGER :: STAT

          ALLOCATE( GROWAGNO( NCOLS,NROWS ),
     &              NGROWAGNO( NCOLS,NROWS ),
     &              NONAGNO( NCOLS,NROWS ),
     &              STAT=STAT )

          IF ( .NOT. OPEN3( fname, FSREAD3, PNAME ) ) THEN
             MESG = 'Could not open ' // trim(fname) // ' file '
             CALL M3MESG( MESG )
          END IF

          VAR = 'AVG_NOAG_GROW'
          IF ( .NOT. XTRACT3( fname, VAR,
     &                        1,1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                        0, 0, GROWAGNO ) ) THEN
             MESG = 'Could not read "' // TRIM( VAR ) //
     &              '" from file "' // TRIM( fname ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

          VAR = 'AVG_NOAG_NONGROWNB3'
          IF ( .NOT. XTRACT3( fname, VAR,
     &                        1,1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                        0, 0, NGROWAGNO ) ) THEN
             MESG = 'Could not read "' // TRIM( VAR ) //
     &              '" from file "' // TRIM( fname ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

          VAR = 'AVG_NONONAG'
          IF ( .NOT. XTRACT3( fname, VAR,
     &                        1,1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                        0, 0, NONAGNO ) ) THEN
             MESG = 'Could not read "' // TRIM( VAR ) //
     &              '" from file "' // TRIM( fname ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

        end subroutine b3grd_setup

! -------------------------------------------------------------------------
        subroutine beld_data_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          use beld_data_module

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'beld_data_setup'

          CHARACTER( 256 ) :: MESG
          CHARACTER( 16 ) :: vname
          INTEGER :: STAT, i, j, k

          Allocate ( Beld_ag ( ncols, nrows, e2c_cats ),
! for E2C_SOIL file
     &               pHs1 ( ncols, nrows, e2c_cats ),
     &               pHs2 ( ncols, nrows, e2c_cats ),
     &               por1 ( ncols, nrows, e2c_cats ),
     &               por2 ( ncols, nrows, e2c_cats ),
     &               wp1  ( ncols, nrows, e2c_cats ),
     &               wp2  ( ncols, nrows, e2c_cats ),
     &               cec1 ( ncols, nrows, e2c_cats ),
     &               cec2 ( ncols, nrows, e2c_cats ),
! for E2C_FERT file
     &               NH4ps1 ( ncols, nrows, e2c_cats ),
     &               NH4ps2 ( ncols, nrows, e2c_cats ),
     &               wep1   ( ncols, nrows, e2c_cats ),
     &               wep2   ( ncols, nrows, e2c_cats ),
     &               dep2   ( ncols, nrows, e2c_cats ),
     &               STAT=STAT )

          IF ( .NOT. OPEN3( BELD4_LU, FSREAD3, PNAME ) ) THEN
             mesg = 'Could not open ' // trim(BELD4_LU) // ' file'
             CALL M3MESG( mesg )
          END IF

!         IF ( .NOT. DESC3( BELD4_LU ) ) THEN
!            mesg = 'Could not get ' // trim(BELD4_LU) // ' file description'
!            CALL M3MESG( mesg )
!         END IF

          Do k = 1, e2c_cats
             vname = BELD_Names(k)
             IF ( .NOT. XTRACT3( BELD4_LU, vname,
     &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                            0, 0, Beld_ag( :,:,k ) ) ) Then
                MESG = 'Could not read "' // TRIM( vname ) //
     &                 '" from file "' // TRIM( BELD4_LU ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If
          End Do

! for E2C_SOIL file
          If ( .Not. Open3( E2C_SOIL, fsread3, pname ) ) Then
            mesg = 'Could not open '// E2C_SOIL // ' file'
            Call M3exit ( pname, 0, 0, mesg, xstat1 )
          End If

          vname = 'L1_PH'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, pHs1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_PH'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, pHs2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_Porosity'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, por1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, por2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_Wilt_P'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, wp1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_Wilt_P'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, wp2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_Cation'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, cec1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If
 
          vname = 'L2_Cation'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                         STRTCOLGC2, ENDCOLGC2, 0, 0, cec2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If
 
! for E2C_FERT file
          If ( .Not. Open3( E2C_FERT, fsread3, pname ) ) Then
            mesg = 'Could not open '// E2C_FERT // ' file'
            Call M3exit ( pname, 0, 0, mesg, xstat1 )
          End If

          vname = 'L1_NH3'
          If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                     STRTCOLGC2, ENDCOLGC2, cio_model_sdate, 0, NH4ps1) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_FERT ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_NH3'
          If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                     STRTCOLGC2, ENDCOLGC2, cio_model_sdate, 0, NH4ps2) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_FERT ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_SW'
          If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                     STRTCOLGC2, ENDCOLGC2, cio_model_sdate, 0, wep1)) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_FERT ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_SW'
          If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                     STRTCOLGC2, ENDCOLGC2, cio_model_sdate, 0, wep2)) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_FERT ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_DEP'
          If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, STRTROWGC2, ENDROWGC2, 
     &                     STRTCOLGC2, ENDCOLGC2, cio_model_sdate, 0, dep2)) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_FERT ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

        end subroutine beld_data_setup

! -------------------------------------------------------------------------
        subroutine wb_dust_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          use crop_data_module
          use lus_data_module
          use RUNTIME_VARS, only : dust_land_scheme, erode_agland

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'wb_dust_setup'

          CHARACTER( 256 ) :: xmsg
          CHARACTER( 16 ) :: vname
          INTEGER :: STAT, i, m
          logical :: found

          Allocate ( crop_ladut  ( ncols, nrows, nlcrp ),
     &               crplnd ( ncols, nrows, nlcrp ),
     &               cropdt ( ncols, nrows, ncrop, 3 ),
     &               STAT=STAT )

! Open BELD1
          if ( .not. open3( dust_lu_1, fsread3, pname ) ) then
             xmsg = 'Could not open ' // dust_lu_1
             call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
          end if

! Open crop calendar
          do i = 1, 3
             if ( .not. open3( crname( i ), fsread3, pname ) ) then
                xmsg = 'Could not open ' // crname( i )
                call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             end if
          end do

! Read in 17 species crop land fraction
          do m = 1, nlcrp
             if ( .not. xtract3( dust_lu_1, crop_vnmld( m ), 1, 1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,
     &                           0, 0, crop_ladut( :,:,m ) ) ) then
                xmsg = 'Could not read ' // trim( crop_vnmld( m ) ) // ' from ' // dust_lu_1
                call m3exit ( pname, 0, 0, xmsg, xstat1 )
             end if
          end do

! Read crop calendar: 1 = begin planting, 2 = end planting, 3 = end harvesting
          do i = 1, 3
             do m = 1, ncrop
                if ( .not. xtract3( crname( i ), vcrop( m ), 1,1,
     &                              STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                              0, 0, cropdt( 1,1,m,i ) ) ) then
                   xmsg = 'Could not read ' // trim( vcrop( m ) ) // ' from ' // crname( i )
                   call m3exit ( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
                end if
             end do
          end do

! this is for LU data

         if ( isbeld ) then  ! get the 2nd file (currently same as 1st for BELD4)
            if ( .not. open3( lufile( 2 ), fsread3, pname ) ) then
               xmsg = 'could not open ' // trim( lufile( 2 ) )
               call m3exit ( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
            end if
         end if

! Get desert land (fraction) data (assume if BELD, all desert types are in file 1)
          do i = 1, n_dlcat
             if ( .not. xtract3( lufile( 1 ), vnmld( i )%name, 1,1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, ladut( :,:,i ) ) ) then
                xmsg = 'Could not read ' // trim( vnmld( i )%name )
     &               // ' from ' // trim( lufile( 1 ) )
                call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             end if
          end do

! Get landuse (fraction) data
          do i = 1, n_lucat-1
             if ( .not. xtract3( lufile( 1 ), vnmlu( i )%name, 1,1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, lut( :,:,i ) ) ) then
                xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &               // ' from ' // trim( lufile( 1 ) )
                call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             end if
          end do

          i = n_lucat
          if ( .not. isbeld ) then
             if ( .not. xtract3( lufile( 1 ), vnmlu( i )%name, 1,1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, lut( :,:,i ) ) ) then
                xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &               // ' from ' // trim( lufile( 1 ) )
                call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             end if
          else
             if ( .not. xtract3( lufile( 2 ), vnmlu( i )%name, 1,1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, lut( :,:,i ) ) ) then
                xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &               // ' from ' // trim( lufile( 2 ) )
                call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
             end if
          end if

        end subroutine wb_dust_setup

! -------------------------------------------------------------------------
        subroutine medc_file_setup

          USE UTILIO_DEFN
          use bidi_mod

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'medc_file_setup'

          CHARACTER( 256 ) :: xmsg
          integer :: v

          CALL INIT_BIDI( )

          IF ( .NOT. OPEN3( INIT_MEDC_1, FSREAD3, PNAME ) ) THEN
             XMSG = 'Open failure for ' // INIT_MEDC_1
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          DO v = 1, Hg_TOT
            IF ( .NOT. Xtract3( INIT_MEDC_1, MEDIA_NAMES( V ), 1, 1, 
     &                          STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                          cio_model_sdate, 0, CMEDIA(:,:,v) ) )THEN
               xmsg = 'Could not read ' // trim( MEDIA_NAMES( V ) )
     &                // ' from ' // trim( INIT_MEDC_1 )
               call m3exit( pname, cio_model_sdate, 0, xmsg, xstat1 )
            END IF
          END DO

        end subroutine medc_file_setup

! -------------------------------------------------------------------------
        subroutine soilinp_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          use RUNTIME_VARS, only : NEW_START

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, parameter :: mxhrs = 24
          Character( 40 ), parameter :: pname = 'soilinp_setup'
          Character( 40 ), parameter :: soilinp = 'SOILINP'

          CHARACTER( 16 )  :: var
          CHARACTER( 256 ) :: mesg
          integer :: stat, i, j, k

          ALLOCATE( PTYPE( NCOLS,NROWS ),
     &              PULSEDATE( NCOLS,NROWS ),
     &              PULSETIME( NCOLS,NROWS ),
     &              RAINFALL( NCOLS,NROWS, mxhrs ),
     &              DDTTM( mxhrs ),
     &              STAT=STAT )

          DDTTM = ' '   ! array

          if (.not. NEW_START) then
             IF ( .NOT. OPEN3( SOILINP, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // SOILINP
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF

! Get description of NO rain data file
             IF ( .NOT. DESC3( SOILINP ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( SOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

! Check that the file start date and time are consistent
             IF ( SDATE3D .NE. cio_model_sdate ) THEN
                WRITE( MESG, 94010 ) 'Cannot use SOILINP file; ' //
     &              'found date ', SDATE3D, ' expecting ', cio_model_sdate
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             IF ( STIME3D .NE. cio_model_stime ) THEN
                WRITE( MESG, 94010 ) 'Cannot use SOILINP file; ' //
     &              'found time ', STIME3D, ' expecting ', cio_model_stime
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

94010        FORMAT( A, F10.2, 1X, A, I3, ',', I3 )

             VAR = 'PTYPE'
             IF ( .NOT. XTRACT3( SOILINP, 'PTYPE', 1, 1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, PTYPE ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( SOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             VAR = 'PULSEDATE'
             IF ( .NOT. XTRACT3( SOILINP, VAR, 1, 1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, PULSEDATE ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( SOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             VAR = 'PULSETIME'
             IF ( .NOT. XTRACT3( SOILINP, VAR, 1, 1,
     &                           STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                           0, 0, PULSETIME ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( SOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             RAINFALL = 0.0
             DDTTM = ' '   ! array
             DO I = 1, mxhrs
                WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I
                IF ( .NOT. XTRACT3( SOILINP, VAR, 1, 1,
     &                              STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2,  
     &                              0, 0, RAINFALL( :,:,I ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( SOILINP ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
                J = INDEX( VDESC3D( I+3 ), 'for' ) + 3
                K = LEN_TRIM( VDESC3D( I+3 ) )
                DDTTM( I ) = VDESC3D( I+3 )( J:K )
             END DO

          end if

        end subroutine soilinp_setup

! -------------------------------------------------------------------------
        subroutine retrieve_grid_cro_2d_data

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE LSM_Mod, ONLY: n_lufrac, init_lsm

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_grid_cro_2d_data'

          CHARACTER( 120 ) :: XMSG = ' '
          Character( 16 )  :: vname
          INTEGER          :: STAT, L

          allocate (MSFX2(ncols, nrows), 
     &              LWMASK(ncols, nrows), 
     &              HT(ncols, nrows), 
     &              LAT(ncols, nrows),
     &              LON(ncols, nrows), 
     &              PURB(ncols, nrows),
     &              STAT=STAT)
          IF ( STAT .NE. 0 ) THEN
               XMSG = 'Failure allocating MSFX2 or other arrays'
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          IF ( .NOT. XTRACT3( GRID_CRO_2D, 'MSFX2',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, MSFX2 ) ) THEN
             XMSG = ' Error interpolating variable MSFX2 from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. XTRACT3( GRID_CRO_2D, 'LWMASK',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, LWMASK ) ) THEN
             XMSG = ' Error interpolating variable LWMASK from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. XTRACT3( GRID_CRO_2D, 'HT',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, HT ) ) THEN
             XMSG = ' Error interpolating variable HT from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. XTRACT3( GRID_CRO_2D, 'LAT',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, LAT ) ) THEN
             XMSG = ' Error interpolating variable LAT from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. XTRACT3( GRID_CRO_2D, 'LON',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, LON ) ) THEN
             XMSG = ' Error interpolating variable LON from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. XTRACT3( GRID_CRO_2D, 'PURB',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, PURB ) ) THEN
             XMSG = ' Error interpolating variable PURB from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          CALL INIT_LSM( 0, 0 )

          allocate (LUFRAC(ncols, nrows, n_lufrac), STAT=STAT)
          IF ( STAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LUFRAC array'
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          DO l = 1, n_lufrac
             Write( vname,'( "LUFRAC_",I2.2 )' ) l
             IF ( .Not. XTRACT3( GRID_CRO_2D, VNAME,
     &                           1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                           0, 0, LUFRAC( :,:,l ) ) ) THEN
                XMSG = 'Error interpolating variable' // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
          END DO

        end subroutine retrieve_grid_cro_2d_data

! -------------------------------------------------------------------------
        subroutine retrieve_grid_dot_2d_data

          USE UTILIO_DEFN
          USE HGRD_DEFN

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_grid_dot_2d_data'

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '

          ALLOCATE ( MSFD2( NCOLS+1, NROWS+1 ), STAT = STAT )
          IF ( STAT .NE. 0 ) THEN
             XMSG = 'Failure allocating MSFD2 array'
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          IF ( .NOT. XTRACT3( GRID_DOT_2D, 'MSFD2',
     &                        1, 1, STRTROWGD2, ENDROWGD2, STRTCOLGD2, ENDCOLGD2, 
     &                        0, 0, MSFD2 ) ) THEN
             XMSG = 'Could not interpolate MSFD2 from ' // GRID_DOT_2D
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

        end subroutine retrieve_grid_dot_2d_data

! -------------------------------------------------------------------------
        subroutine retrieve_ocean_data

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE RXNS_DATA, ONLY : MECHNAME

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_ocean_data'

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '

          allocate (ocean(ncols, nrows), 
     &              szone(ncols, nrows), 
     &              STAT=STAT)
          IF ( STAT .NE. 0 ) THEN
             XMSG = 'Failure allocating OPEN and SURF array'
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          IF ( .NOT. OPEN3( OCEAN_1, FSREAD3, PNAME ) ) THEN
             XMSG = 'Open failure for ' // OCEAN_1
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          If ( .Not. XTRACT3( OCEAN_1, 'OPEN',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, ocean ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // OCEAN_1
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          If ( .Not. XTRACT3( OCEAN_1, 'SURF',
     &                        1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, szone ) ) Then
             XMSG = 'Could not interpolate SURF from ' // OCEAN_1
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( INDEX( MECHNAME, 'CB05EH51' ) .GT. 0 ) then
             USE_MARINE_GAS_EMISSION = .TRUE.
             allocate (chlr(ncols, nrows), STAT=STAT)
             IF ( STAT .NE. 0 ) THEN
                XMSG = 'Failure allocating CHLO array'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
             END IF

             If ( .Not. XTRACT3( OCEAN_1, 'CHLO',
     &                           1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                           0, 0, chlr ) ) Then
                XMSG = 'Could not interpolate CHLO from ' // OCEAN_1
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             End If
             If ( .Not. XTRACT3( OCEAN_1, 'DMS',
     &                           1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                           0, 0, dmsl ) ) Then
                XMSG = 'Could not interpolate CHLO from ' // OCEAN_1
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             End If
          ELSE
             USE_MARINE_GAS_EMISSION = .TRUE.
          END IF

        end subroutine retrieve_ocean_data

! -------------------------------------------------------------------------
        subroutine retrieve_ltng_param_data (jdate, jtime)

          USE UTILIO_DEFN
          USE HGRD_DEFN

          integer, intent(in) :: jdate, jtime

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_ltng_param_data'
          Character( 40 ), parameter :: LTNGPARMS_FILE = 'LTNGPARMS_FILE'

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '

          allocate (OCEAN_MASK(ncols, nrows), 
     &              SLOPE(ncols, nrows), 
     &              INTERCEPT(ncols, nrows), 
     &              SLOPE_lg(ncols, nrows), 
     &              INTERCEPT_lg(ncols, nrows), 
     &              ICCG_SUM(ncols, nrows), 
     &              ICCG_WIN(ncols, nrows), 
     &              STAT=STAT)
          IF ( STAT .NE. 0 ) THEN
             XMSG = 'Failure allocating ltng parameter arrays'
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          IF ( .NOT. OPEN3( LTNGPARMS_FILE, FSREAD3, PNAME ) ) THEN
             XMSG = 'Open failure for ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "OCNMASK", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, OCEAN_MASK ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "SLOPE", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, SLOPE ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "INTERCEPT", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, INTERCEPT ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "SLOPE_lg", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, SLOPE_lg ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "INTERCEPT_lg", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, INTERCEPT_lg ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "ICCG_SUM", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, ICCG_SUM ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "ICCG_WIN", 1, 1,
     &                        STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                        0, 0, ICCG_WIN ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

        end subroutine retrieve_ltng_param_data

! -------------------------------------------------------------------------
        subroutine retrieve_abflux_data

          USE UTILIO_DEFN
          USE HGRD_DEFN
!         USE ABFlux_Mod

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_abflux_data'

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '
          integer :: k

!         allocate (Beld_ag(ncols, nrows, e2c_cats), 
!    &              STAT=STAT)
!         IF ( STAT .NE. 0 ) THEN
!            XMSG = 'Failure allocating Beld_ag array'
!            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
!         END IF

          IF ( .NOT. OPEN3( BELD4_LU, FSREAD3, PNAME ) ) THEN
             XMSG = 'Open failure for ' // BELD4_LU
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

!         Do k = 1, e2c_cats
!            If ( .Not. XTRACT3 ( BELD4_LU, BELD_Names(k), pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, Beld_ag( :,:,k ) ) ) Then
!               XMSG = 'Failure reading ' // BELD_Names(k) // ' from ' // BELD4_LU
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If
!         End Do

          INIT_MEDC_1_AVAIL = .true.
          If ( .Not. Open3( INIT_MEDC_1, fsread3, pname ) ) Then
             INIT_MEDC_1_AVAIL = .false.
             E2C_FERT_AVAIL = .true.
             If ( .Not. Open3( E2C_FERT, fsread3, pname ) ) Then
                XMSG = 'Open failure for ' // E2C_FERT
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                E2C_FERT_AVAIL = .false.
             END IF
          END IF

          if (E2C_FERT_AVAIL) then
!            allocate (NH4ps1(ncols, nrows, e2c_cats), 
!    &                 NH4ps2(ncols, nrows, e2c_cats), 
!    &                 STAT=STAT)
!            IF ( STAT .NE. 0 ) THEN
!               XMSG = 'Failure allocating NH4ps1 and NH4ps2 array'
!               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
!            END IF
!            NH4ps1 = 0.0
!            NH4ps2 = 0.0

!            If ( .Not. XTRACT3 ( E2C_FERT, 'L1_NH3', pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, NH4ps1) ) then
!               XMSG = 'Failure reading L1_NH3 from ' // E2C_FERT
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If

!            If ( .Not. XTRACT3 ( E2C_FERT, 'L2_NH3', pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, NH4ps2) ) then
!               XMSG = 'Failure reading L2_NH3 from ' // E2C_FERT
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If
!         else
!            allocate (gamma1(ncols, nrows), 
!    &                 gamma2(ncols, nrows), 
!    &                 MHp1(ncols, nrows), 
!    &                 MHp2(ncols, nrows), 
!    &                 STAT=STAT)
!            IF ( STAT .NE. 0 ) THEN
!               XMSG = 'Failure allocating gamma1, gamma2, MHp1 and MHp2 array'
!               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
!            END IF
!            gamma1 = 0.0
!            gamma2 = 0.0
!            MHp1   = 0.0
!            MHp2   = 0.0

!            If ( .Not. XTRACT3 ( INIT_MEDC_1, 'Gamma1', pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, gamma1) ) then
!               XMSG = 'Failure reading Gamma1 from ' // INIT_MEDC_1
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If

!            If ( .Not. XTRACT3 ( INIT_MEDC_1, 'Gamma2', pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, gamma2) ) then
!               XMSG = 'Failure reading Gamma2 from ' // INIT_MEDC_1
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If

!            If ( .Not. XTRACT3 ( INIT_MEDC_1, 'MHpsl1', pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, MHp1) ) then
!               XMSG = 'Failure reading MHpsl1 from ' // INIT_MEDC_1
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If

!            If ( .Not. XTRACT3 ( INIT_MEDC_1, 'MHpsl2', pname, 
!    &                            1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
!    &                            0, 0, MHp2) ) then
!               XMSG = 'Failure reading MHpsl2 from ' // INIT_MEDC_1
!               Call M3exit( pname, 0, 0, xmsg, xstat1 )
!            End If

          end if

        end subroutine retrieve_abflux_data

! -------------------------------------------------------------------------
        subroutine retrieve_time_dep_gridded_data (jdate, jtime, vname)

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE VGRD_DEFN, ONLY : NLAYS
          USE CGRID_SPCS

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: vname

          Character( 40 ), parameter :: pname = 'retrieve_time_dep_gridded_data'
          CHARACTER( 40 ), parameter :: NLDN_STRIKES = 'NLDN_STRIKES'
          Character( 40 ), parameter :: GR_EMIS_001 = 'GR_EMIS_001'

          LOGICAL :: firstime = .true.
          INTEGER :: STAT, i, j, begin, end, buf_loc, iterations,
     &               iter, loc_jdate, loc_jtime, bs_tjdate, wb_tjdate, v, beg_v, end_v, 
     &               fnum, str_len, bs_tjtime, wb_tjtime
          integer, allocatable :: tdata(:)
          character (16) :: loc_vname
          logical :: advanced

          CHARACTER( 120 ) :: XMSG = ' '

          if (firstime) then

             allocate (SOILCAT_A(ncols, nrows), STAT=STAT)

             IF ( STAT .NE. 0 ) THEN
                XMSG = 'Failure allocating SLTYP array'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
             END IF

             If ( .Not. XTRACT3( MET_CRO_2D, 'SLTYP',
     &                           1, 1, STRTROWMC2, ENDROWMC2, STRTCOLMC2, ENDCOLMC2, 
     &                           jdate, jtime, SOILCAT_A ) ) THEN
                XMSG = ' Error interpolating variable MSFX2 from ' // GRID_CRO_2D
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF

             head_grid = -1
             tail_grid = -1

          end if  ! firstime

          if (firstime) then
             iterations = 2
          else
             iterations = 1
          end if

          if (present(vname)) then
             beg_v = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
             end_v = beg_v
          else
             beg_v = 1
             end_v = n_cio_grid_vars
          end if

          loc_jdate = jdate
          loc_jtime = jtime

          advanced = .false.
          do iter = 1, iterations
             do v = beg_v, end_v
                buf_loc = mod((tail_grid(v) + iter), 2)

                cio_grid_data_tstamp(1, buf_loc, v) = loc_jdate
                cio_grid_data_tstamp(2, buf_loc, v) = loc_jtime

                begin = cio_grid_data_inx(1,buf_loc,v)
                end   = cio_grid_data_inx(2,buf_loc,v)

                if (cio_grid_var_name(v,2) == '2') then
                   if ((cio_grid_var_name(v,1) .ne. 'TSEASFC') .or.  TSEASFC_AVAIL) then
                      IF ( .NOT. XTRACT3( MET_CRO_2D, cio_grid_var_name(v,1), 
     &                                    1, 1, STRTROWMC2x, ENDROWMC2x, STRTCOLMC2x, ENDCOLMC2x, 
     &                                    loc_jdate, loc_jtime, cio_grid_data(begin:end) ) ) THEN
                         XMSG = 'Could not extract ' // MET_CRO_2D // ' file'
                         CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                      END IF
                   END IF

                else if (cio_grid_var_name(v,2) == '3') then

                   IF ( .NOT. XTRACT3( MET_CRO_3D, cio_grid_var_name(v,1), 
     &                                 1, nlays, STRTROWMC3, ENDROWMC3, STRTCOLMC3, ENDCOLMC3, 
     &                                 loc_jdate, loc_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // MET_CRO_3D // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF

                else if (cio_grid_var_name(v,2) == 'd') then

                   IF ( .NOT. XTRACT3( MET_DOT_3D, cio_grid_var_name(v,1), 
     &                                 1, nlays, STRTROWMD3x, ENDROWMD3x, STRTCOLMD3x, ENDCOLMD3x, 
     &                                 loc_jdate, loc_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // MET_DOT_3D // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF

                else if (cio_grid_var_name(v,2) == 'e') then

                   str_len = len_trim(cio_grid_var_name(v,1))
                   read (cio_grid_var_name(v,1)(str_len-2:str_len), *) fnum
                   loc_vname = cio_grid_var_name(v,1)(1:str_len-4)

                   IF ( .NOT. XTRACT3( cio_emis_file_name(fnum), loc_vname,
     &                                 1, 1, STRTROWMC2, ENDROWMC2, STRTCOLMC2, ENDCOLMC2, 
     &                                 loc_jdate, loc_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // cio_emis_file_name(fnum) // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF

                else if (cio_grid_var_name(v,2) == 'E') then

                   str_len = len_trim(cio_grid_var_name(v,1))
                   read (cio_grid_var_name(v,1)(str_len-2:str_len), *) fnum
                   loc_vname = cio_grid_var_name(v,1)(1:str_len-4)

                   IF ( .NOT. XTRACT3( cio_emis_file_name(fnum), loc_vname,
     &                                 1, nlays, STRTROWMC2, ENDROWMC2, STRTCOLMC2, ENDCOLMC2, 
     &                                 loc_jdate, loc_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // GR_EMIS_001 // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF

                else if (cio_grid_var_name(v,2) == 'l') then

                   IF ( .NOT. XTRACT3( NLDN_STRIKES, cio_grid_var_name(v,1), 
     &                                 1, cio_LTLYRS, STRTROWMC2, ENDROWMC2, STRTCOLMC2, ENDCOLMC2, 
     &                                 loc_jdate, loc_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // NLDN_STRIKES // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF

                else if (cio_grid_var_name(v,2) == 'bs') then

                   if (iter .eq. 1) then
                      bs_tjdate = loc_jdate
                      bs_tjtime = 0
                   else
                      if (.not. advanced) then
                         CALL NEXTIME ( bs_tjdate, bs_tjtime, 240000 )
                         advanced = .true.
                      end if
                   end if

                   allocate (tdata((ENDCOLGC2-STRTCOLGC2+1)*(ENDROWGC2-STRTROWGC2+1)), stat=stat)

                   IF ( .NOT. XTRACT3( bio_season_fname, 'SEASON',
     &                                 1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                                 bs_tjdate, bs_tjtime, tdata) ) then
                      XMSG = 'Could not extract ' // bio_season_fname // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF
                   
                   cio_grid_data(begin:end) = real(tdata)

                   deallocate (tdata)

                else if (cio_grid_var_name(v,2) == 'wb') then

                   if (iter .eq. 1) then
                      if ( modis_data_sdate .eq. 2005001 ) then    ! climatological
                         wb_tjdate = 2005000 + mod( jdate, 1000 )
                      else                                         ! current
                         wb_tjdate = jdate
                      end if
                      wb_tjtime = 0
                   else
                      if (.not. advanced) then
                         CALL NEXTIME ( wb_tjdate, wb_tjtime, 240000 )
                         advanced = .true.
                      end if
                   end if

                   IF ( .NOT. XTRACT3( dust_fname, cio_grid_var_name(v,1),
     &                                 1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                                 wb_tjdate, wb_tjtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // dust_fname // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF
                   
                else
                   write (cio_logdev, *) ' ==d== Unknown type of file'
                   stop
                end if
             end do

! assign TEMPG to TSEASFC when TSEASFC is not available in the input file
             if (.not. TSEASFC_AVAIL) then
                begin = cio_grid_data_inx(1,buf_loc,tempg_loc)
                end   = cio_grid_data_inx(2,buf_loc,tempg_loc)
                i     = cio_grid_data_inx(1,buf_loc,tseasfc_loc)
                j     = cio_grid_data_inx(2,buf_loc,tseasfc_loc)
                cio_grid_data(i:j) = cio_grid_data(begin:end)
             end if

             CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )

          end do  ! end iter

          if (firstime) then
             firstime = .false.
             head_grid = 0
             tail_grid = 1
          else
             do v = beg_v, end_v
                head_grid(v) = mod(head_grid(v)+1, 2)
                tail_grid(v) = mod(tail_grid(v)+1, 2)
             end do
          end if

        end subroutine retrieve_time_dep_gridded_data

! -------------------------------------------------------------------------
        subroutine retrieve_time_dep_boundary_data (jdate, jtime, vname)

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE VGRD_DEFN, ONLY : NLAYS
          USE CGRID_SPCS

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: vname

          Character( 40 ), parameter :: pname = 'retrieve_time_dep_boundary_data'

          LOGICAL :: firstime = .true.
          INTEGER :: STAT, i, j, begin, end, buf_loc, iterations,
     &               iter, loc_jdate, loc_jtime, v, beg_v, end_v

          CHARACTER( 120 ) :: XMSG = ' '

          if (firstime) then

             head_bndy = -1
             tail_bndy = -1

          end if  ! firstime

          if (firstime) then
             iterations = 2
          else
             iterations = 1
          end if

          if (present(vname)) then
             beg_v = binary_search (vname, cio_bndy_var_name(:,1), n_cio_bndy_vars)
             end_v = beg_v
          else
             beg_v = 1
             end_v = n_cio_bndy_vars
          end if

          loc_jdate = jdate
          loc_jtime = jtime

          do iter = 1, iterations
             do v = beg_v, end_v
                buf_loc = mod((tail_bndy(v) + iter), 2)

                cio_bndy_data_tstamp(1, buf_loc, v) = loc_jdate
                cio_bndy_data_tstamp(2, buf_loc, v) = loc_jtime

                begin = cio_bndy_data_inx(1,buf_loc,v)
                end   = cio_bndy_data_inx(2,buf_loc,v)

                if (cio_bndy_var_name(v,2) == 'mb') then
                   if (.not. read3 (MET_BDY_3D, cio_bndy_var_name(v,1), -1,
     &                              loc_jdate, loc_jtime, cio_bndy_data(begin:end) ) ) THEN
                      XMSG = 'Could not read ' // MET_BDY_3D // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF
                else if (cio_bndy_var_name(v,2) == 'b') then
                   if (.not. read3 (BNDY_GASC_1, cio_bndy_var_name(v,1), -1,
     &                              loc_jdate, loc_jtime, cio_bndy_data(begin:end) ) ) THEN
                      XMSG = 'Could not read ' // MET_BDY_3D // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jdate, XMSG, XSTAT1 )
                   END IF
                else
                   write (cio_logdev, *) ' ==d== Unknown type of file'
                   stop
                end if

             end do

             CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )

          end do  ! end iter

          if (firstime) then
             firstime = .false.
             head_bndy = 0
             tail_bndy = 1
          else
             do v = beg_v, end_v
                head_bndy(v) = mod(head_bndy(v)+1, 2)
                tail_bndy(v) = mod(tail_bndy(v)+1, 2)
             end do
          end if

        end subroutine retrieve_time_dep_boundary_data

! -------------------------------------------------------------------------
        subroutine retrieve_stack_data (jdate, jtime, fname, vname)

          USE UTILIO_DEFN
          USE STK_PRMS, only : MY_STRT_SRC, MY_END_SRC

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: fname, vname

          Character( 40 ), parameter :: pname = 'retrieve_stack_data'

          LOGICAL :: firstime = .true.
          INTEGER :: STAT, i, j, begin, end, buf_loc, iterations,
     &               iter, loc_jdate, loc_jtime, v, beg_v, end_v, 
     &               beg_pt, end_pt, pt, fnum

          CHARACTER( 120 ) :: XMSG = ' '

          if (firstime) then

             head_stack_emis = -1
             tail_stack_emis = -1

             iterations = 2
          else
             iterations = 1
          end if

          if (present(vname)) then
             beg_pt = binary_search (fname, cio_stack_file_name, NPTGRPS)
             end_pt = beg_pt
             beg_v = binary_search (vname, cio_stack_var_name(:,beg_pt), n_cio_stack_emis_vars(beg_pt))
             end_v = beg_v
          else
             beg_pt = 1
             end_pt = NPTGRPS
          end if

          do pt = beg_pt, end_pt

             if (firstime) then
                loc_jdate = cio_stack_emis_sdate(pt)
                loc_jtime = cio_stack_emis_stime(pt)
             else
                loc_jdate = jdate
                loc_jtime = jtime
             end if

             if (.not. present(vname)) then
                beg_v = 1
                end_v = n_cio_stack_emis_vars(pt)
             end if

! cio_stack_emis_data_inx

             do iter = 1, iterations

                do v = beg_v, end_v
                   buf_loc = mod((tail_stack_emis(v, pt) + iter), 2)

                   cio_stack_emis_data_tstamp(1, buf_loc, v, pt) = loc_jdate
                   cio_stack_emis_data_tstamp(2, buf_loc, v, pt) = loc_jtime

                   begin = cio_stack_emis_data_inx(1, buf_loc, v, pt)
                   end   = cio_stack_emis_data_inx(2, buf_loc, v, pt)

                   if (begin .gt. 0) then
                      IF ( .NOT. XTRACT3( cio_stack_file_name(pt), cio_stack_var_name(v, pt), 
     &                                    1,1, MY_STRT_SRC( pt ),MY_END_SRC( pt), 1,1,
     &                                    loc_jdate, loc_jtime, cio_stack_data(begin:end) ) ) THEN
                         XMSG = 'Could not extract ' // cio_stack_file_name(pt) // ' file'
                         CALL M3EXIT ( PNAME, loc_jdate, loc_jtime, XMSG, XSTAT1 )
                      END IF
                   end if
                end do

                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )

             end do  ! end iter
          end do

          if (firstime) then
             firstime = .false.
             head_stack_emis = 0
             tail_stack_emis = 1
          else
             do pt = beg_pt, end_pt
                do v = beg_v, end_v
                   head_stack_emis(v, pt) = mod(head_stack_emis(v, pt)+1, 2)
                   tail_stack_emis(v, pt) = mod(tail_stack_emis(v, pt)+1, 2)
                end do
             end do
          end if

        end subroutine retrieve_stack_data

! -------------------------------------------------------------------------
        subroutine lus_setup

          use RUNTIME_VARS
          use UTILIO_DEFN
          use lus_data_module
          use HGRD_DEFN, only : ncols, nrows

          INCLUDE SUBST_FILES_ID             ! file name parameters

          character (40), parameter :: pname = 'lus_setup'

          character (256) :: xmsg
          integer :: i, err

          if ( dust_land_scheme .eq. 'BELD3' ) then
             isbeld = .true.
             lufile( 1 ) = dust_lu_1
             lufile( 2 ) = dust_lu_2    ! total forest coverage
          else if ( dust_land_scheme .eq. 'BELD4' ) then
             isbeld = .true.
             lufile( 1 ) = beld4_lu
             lufile( 2 ) = beld4_lu
          else
             isbeld = .false.
             lufile( 1 ) = grid_cro_2d
          end if

          if ( .not. isbeld ) then   ! determine land_scheme from GRID_CRO_2D

#ifdef twoway

C   mminlu and num_land_cat are WRF global variables

             select case( mminlu )

                case( 'USGS24' )
                   if ( num_land_cat .ne. 24 ) then
                      write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                       ' for ' // trim( mminlu )
                      call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                   end if
                   dust_land_scheme = 'USGS24'
                case( 'NLCD40' )
                   if ( num_land_cat .ne. 40 ) then
                      write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                       ' for ' // trim( mminlu )
                      call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                   end if
                   dust_land_scheme = 'NLCD40'
                case( 'NLCD-MODIS' )
                   if ( num_land_cat .ne. 50 ) then
                      write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                       ' for ' // trim( mminlu )
                      call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                   end if
                   dust_land_scheme = 'NLCD-MODIS'
                case( 'MODIFIED_IGBP_MODIS_NOAH' )
                   if ( num_land_cat .ne. 20 ) then
                      write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                       ' for ' // trim( mminlu )
                      call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                   end if
                   dust_land_scheme = 'MODIS_NOAH'
                case( 'MODIS' )
                   if ( num_land_cat .ne. 20 ) then
                      write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                       ' for ' // trim( mminlu )
                      call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                   end if
                   dust_land_scheme = 'MODIS'
                case default
                   xmsg = 'Land use scheme not supported'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )

             end select

#else
!            l_s: do i = 1, n_grid_cro_data_vars   ! determine land scheme from GRID_CRO_2D
!               if ( vname3d( i ) .eq. 'DLUSE' ) then
!                  if ( index( vdesc3d( i ), 'USGS24' ) .ne. 0 ) then
!                     dust_land_scheme = 'USGS24'; exit l_s
!                  else if ( index( vdesc3d( i ), 'NLCD40' ) .ne. 0 ) then
!                     dust_land_scheme = 'NLCD40'; exit l_s
!                  else if ( index( vdesc3d( i ), 'NLCD50' ) .ne. 0 ) then
!                     dust_land_scheme = 'NLCD50'; exit l_s
!                  else if ( index( vdesc3d( i ), 'MODIS' ) .ne. 0 ) then
!                     if ( index( vdesc3d( i ), 'NLCD-MODIS' ) .ne. 0 ) then
!                        dust_land_scheme = 'NLCD-MODIS'; exit l_s
!                     else if ( index( vdesc3d( i ), 'MODIS NOAH' ) .ne.  0 ) then
!                        dust_land_scheme = 'MODIS_NOAH'; exit l_s
!                     else
!                        dust_land_scheme = 'MODIS'; exit l_s
!                     end if
!                  end if
!               end if
!            end do l_s
             dust_land_scheme = cio_dust_land_scheme
#endif
          end if   ! .not. isbeld

          select case( dust_land_scheme )

             case( 'BELD3' )
                n_lucat = n_lucat_beld3
                n_dlcat = n_dlcat_beld3
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_beld3   ! array assignment
                vnmld = vnmld_beld3   ! array assignment
                dmsk = dmsk_beld3     ! array assignment
                dmap = dmap_beld3     ! array assignment

             case( 'BELD4' )
                n_lucat = n_lucat_beld4
                n_dlcat = n_dlcat_beld4
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_beld4   ! array assignment
                vnmld = vnmld_beld4   ! array assignment
                dmsk = dmsk_beld4     ! array assignment
                dmap = dmap_beld4     ! array assignment

             case( 'USGS24' )
                n_lucat = n_lucat_usgs24
                n_dlcat = n_dlcat_usgs24
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_usgs24   ! array assignment
                vnmld = vnmld_usgs24   ! array assignment
                dmsk = dmsk_usgs24     ! array assignment
                dmap = dmap_usgs24     ! array assignment

             case( 'MODIS' )
                n_lucat = n_lucat_modis
                n_dlcat = n_dlcat_modis
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_modis   ! array assignment
                vnmld = vnmld_modis   ! array assignment
                dmsk = dmsk_modis     ! array assignment
                dmap = dmap_modis     ! array assignment

             case( 'NLCD40' )
                n_lucat = n_lucat_nlcd40
                n_dlcat = n_dlcat_nlcd40
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_nlcd40   ! array assignment
                vnmld = vnmld_nlcd40   ! array assignment
                dmsk = dmsk_nlcd40     ! array assignment
                dmap = dmap_nlcd40     ! array assignment

             case( 'NLCD-MODIS', 'NLCD50' )
                n_lucat = n_lucat_nlcd_modis
                n_dlcat = n_dlcat_nlcd_modis
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_nlcd_modis   ! array assignment
                vnmld = vnmld_nlcd_modis   ! array assignment
                dmsk = dmsk_nlcd_modis     ! array assignment
                dmap = dmap_nlcd_modis     ! array assignment

             case( 'MODIS_NOAH' )
                n_lucat = n_lucat_modis_noah
                n_dlcat = n_dlcat_modis_noah
                allocate( vnmlu( n_lucat ),
     &                    vnmld( n_dlcat ),
     &                    dmsk( n_dlcat ),
     &                    dmap( n_dlcat+1 ), stat = err )
                if ( err .ne. 0 ) then
                   xmsg = '*** Error allocating vnmlu, vnmld, dmsk or dmap'
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                vnmlu = vnmlu_modis_noah   ! array assignment
                vnmld = vnmld_modis_noah   ! array assignment
                dmsk = dmsk_modis_noah     ! array assignment
                dmap = dmap_modis_noah     ! array assignment

             case default
                xmsg = 'Land use scheme not supported'
                call m3exit ( pname, stdate, sttime, xmsg, xstat1 )

          end select

          write( logdev,* ) ' '
          write( logdev,* ) '    Land use scheme is ', trim( dust_land_scheme )
          write( logdev,* ) '    n_lucat,n_dlcat: ', n_lucat, n_dlcat
          write( logdev,* ) '    desert land categories ------------------------'
          do i = 1, n_dlcat
             write( logdev,* ) '    ', trim( vnmld( i )%name ), ' ', trim( vnmld( i )%desc )
          end do
          write( logdev,* ) '    land use categories ---------------------------'
          do i = 1, n_lucat
             write( logdev,* ) '    ', trim( vnmlu( i )%name ), ' ', trim( vnmlu( i )%desc )
          end do
          write( logdev,* ) ' '

          allocate( ladut( ncols,nrows,n_dlcat ),
     &              lut( ncols,nrows,n_lucat ),
     &              uland( ncols,nrows,4 ), stat = err )
          if ( err .ne. 0 ) then
             xmsg = '*** Error allocating ladut, lut or uland'
             call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
          end if

        end subroutine lus_setup

! -------------------------------------------------------------------------
        subroutine centralized_io_init

          USE UTILIO_DEFN
          USE HGRD_DEFN
          use cgrid_spcs, only : GC_DDEP, N_GC_DDEP

          Character( 40 ), parameter :: pname = 'centralized_io_init'

          logical, save :: first_time = .true.
          INTEGER       :: STAT
          CHARACTER( 120 ) :: XMSG = ' '
          Character( 16 ) :: vname

          if (first_time) then
             first_time = .false.

             cio_logdev = init3()

             cio_model_sdate = STDATE
             cio_model_stime = STTIME

             east_pe = (mod(mype, npcol) .eq. npcol - 1)
             west_pe = (mod(mype, npcol) .eq. 0)
             north_pe = (mype .ge. npcol * (nprow - 1))
             south_pe = (mype .lt. npcol)

             cio_LTNG_NO = LTNG_NO

             call gridded_files_setup

             call boundary_files_setup

             call stack_files_setup

             if (BIOGEMIS) then
                call biogemis_setup
                call b3grd_setup
             end if

             if (ABFLUX) then
                call beld_data_setup
             end if

             if (WB_DUST) then
                call lus_setup

                call wb_dust_setup
             end if

             call READ_EMISS_NAMELIST
             call INIT_EMIS_REGIONS()

             if (HGBIDI) then   ! two level check, 1. environment variable and then GC_DDEP species list
                if ( search( 'HG', GC_DDEP, N_GC_DDEP) .gt. 0 ) then
                   call medc_file_setup
                end if
             end if

             call soilinp_setup

             call retrieve_grid_cro_2d_data

             call retrieve_grid_dot_2d_data

             call retrieve_ocean_data

             if (cio_LTNG_NO) then
                call retrieve_ltng_param_data (cio_model_sdate, cio_model_stime)
             end if

             if (abflux) then
                call retrieve_abflux_data
             end if

          end if

          call retrieve_time_dep_gridded_data (cio_model_sdate, cio_model_stime)

          call retrieve_time_dep_boundary_data (cio_model_sdate, cio_model_stime)

          call retrieve_stack_data (cio_model_sdate, cio_model_stime)

        end subroutine centralized_io_init
 
!-----------------------------------------------------------------------
      SUBROUTINE INIT_EMIS_REGIONS( )
!
!     This subroutine defines several hardcoded rules for emissions
!     scaling that will apply by default. These include subtracting NH3
!     from fertilizer emissions if BiDi is turned on, moving all
!     sulfuric acid vapor to the particle phase upon emission and
!     splitting up the coarse mode anthropogenic emissions mass into
!     speciated compounds.
!----------------------------------------------------------------------- 
      USE GRID_CONF
      USE UTILIO_DEFN
      USE em_param_module

      IMPLICIT NONE
                                                     
      TYPE( EM_REG      ) :: EM_REG_READ( N_REGIONS_REG )
      INTEGER, PARAMETER :: NFILE0 = 200
      CHARACTER( 32 )    :: FILENAMES( NFILE0 ) = ''
      CHARACTER( 32 ) :: VNAME

      INTEGER :: IRGN, NFILE, IDX, IFILE, IREAD, IVAR
      INTEGER :: GXOFF, GYOFF, ENDCOL, ENDROW, STARTCOL, STARTROW
      INTEGER :: N_EM_READ
      CHARACTER( 16 )    :: PNAME = "INIT_REGIONS"
      CHARACTER( 250)    :: XMSG

      ! Find the total number of regions to be processed
      N_EM_READ = 0  ! The first region is the entire domain
      DO IRGN = 1,N_REGIONS_REG
          IF ( RGN_NML( IRGN )%LABEL .EQ. '' ) EXIT
          N_EM_READ = N_EM_READ + 1
      END DO

      ! Allocate Vectors and Arrays for Each Region
      ALLOCATE( EM_REGIONS( N_REGIONS_REG ) )
      EM_REGIONS( 1 )%LABEL = 'EVERYWHERE'
      EM_REGIONS( 1 )%FILE  = 'N/A'
      EM_REGIONS( 1 )%VAR   = 'N/A'
      EM_REGIONS( 1 )%FILENUM = 1
      N_EM_RGN = 1
      
      ALLOCATE( EM_REG_FAC( NCOLS,NROWS,N_REGIONS_REG ) )
      EM_REG_FAC = 0.0
      EM_REG_FAC( :,:,1 ) = 1.0

      ! Populate global Region properties structure. Also assign each
      ! region a number according to the file it comes from. 1 =
      ! domain-wide.
      NFILE = 1
      FILENAMES( 1 ) = 'N/A'

      IF ( N_EM_READ .GT. 0 ) THEN
        DO IREAD = 1,N_EM_READ
           CALL UPCASE( RGN_NML( IREAD )%LABEL )
           CALL UPCASE( RGN_NML( IREAD )%FILE  )
           CALL UPCASE( RGN_NML( IREAD )%VAR   )

           EM_REG_READ( IREAD )%LABEL = RGN_NML( IREAD )%LABEL  ! Region Name
           EM_REG_READ( IREAD )%FILE  = RGN_NML( IREAD )%FILE   ! Logical filename
           EM_REG_READ( IREAD )%VAR   = RGN_NML( IREAD )%VAR    ! Variable from file 
                                                                !   used to inform mask

           IDX = INDEX1( EM_REG_READ( IREAD )%FILE, NFILE, FILENAMES(1:NFILE) )
           IF ( IDX .NE. 0 ) THEN
             EM_REG_READ( IREAD )%FILENUM = IDX
           ELSE
             NFILE = NFILE + 1
             EM_REG_READ( IREAD )%FILENUM = NFILE
             FILENAMES( NFILE ) = EM_REG_READ( IREAD )%FILE
           END IF                                                           
        END DO
      
        ! Process each region by looping through the pertinent files, 
        ! look up maps and save the data in a global array
        DO IFILE = 1,NFILE
          IF ( FILENAMES( IFILE ) .EQ. 'N/A' ) CYCLE
         
          ! Get domain decomp info from the emissions file
          CALL SUBHFILE ( FILENAMES( IFILE ), GXOFF, GYOFF,
     &                    STARTCOL, ENDCOL, STARTROW, ENDROW )
         
          ! Open input file
          IF ( .NOT. OPEN3( FILENAMES( IFILE ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open '// FILENAMES( IFILE ) // ' file'
             CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF
         
          ! Retrieve File Header Information
          IF ( .NOT. DESC3( FILENAMES( IFILE ) ) ) THEN
              XMSG = 'Could not get ' // FILENAMES( IFILE ) // ' file description'
             CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF
         
          ! Read data from regions file into region array 
          DO IREAD = 1,N_EM_READ
              IF ( EM_REG_READ( IREAD )%FILENUM .EQ. IFILE ) THEN
                  IF ( EM_REG_READ( IREAD )%VAR .EQ. 'ALL' ) THEN
                     ! Populate the region array with all of the
                     ! variables on this file
                     IF ( EM_REG_READ( IREAD )%LABEL .NE. 'ALL' ) THEN
                        XMSG = 'Error reading Region input in Emissions Control file.'//
     &                         'If the variable name is set to "ALL", then the label must'//
     &                         'also be set to "ALL".'
                        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
                     ELSE   
                        DO IVAR = 1,NVARS3D
                           N_EM_RGN = N_EM_RGN + 1
                           EM_REGIONS( N_EM_RGN )%LABEL   = VNAME3D( IVAR )
                           EM_REGIONS( N_EM_RGN )%VAR     = VNAME3D( IVAR )
                           EM_REGIONS( N_EM_RGN )%FILE    = EM_REG_READ( IREAD )%FILE
                           EM_REGIONS( N_EM_RGN )%FILENUM = EM_REG_READ( IREAD )%FILENUM

                           IF ( .NOT. INTERPX( FILENAMES( IFILE ), VNAME3D(IVAR), 
     &                          PNAME, STARTCOL, ENDCOL, STARTROW, ENDROW, 1, 1,
     &                          SDATE3D, STIME3D, EM_REG_FAC( 1,1,N_EM_RGN ) ) )
     &                          CALL M3ERR( PNAME, SDATE3D, STIME3D,
     &                              'Could not read ' // VNAME //
     &                              'from ' // FILENAMES( IFILE ) // ' file', .TRUE. )
                        END DO
                     END IF
                  ELSE
                     ! Populate the region array with only this variable
                     N_EM_RGN = N_EM_RGN + 1
                     EM_REGIONS( N_EM_RGN ) = EM_REG_READ( IREAD )
                     VNAME = EM_REG_READ( IREAD )%VAR

                     IF ( .NOT. INTERPX( FILENAMES( IFILE ), VNAME, PNAME,
     &                    STARTCOL, ENDCOL, STARTROW, ENDROW, 1, 1,
     &                    SDATE3D, STIME3D, EM_REG_FAC( 1,1,N_EM_RGN ) ) )
     &                    CALL M3ERR( PNAME, SDATE3D, STIME3D,
     &                        'Could not read ' // VNAME //
     &                        'from ' // FILENAMES( IFILE ) // ' file', .TRUE. )
                  END IF
              END IF
          END DO
          EM_REGIONS = EM_REGIONS( 1:N_EM_RGN )
          EM_REG_FAC = EM_REG_FAC( :,:,1:N_EM_RGN )

          ! Close Regions File
          IF ( .NOT. CLOSE3( FILENAMES( IFILE ) ) ) THEN
            XMSG = 'Could not close ' // FILENAMES( IFILE )
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF
         
          ! Error Check the Regions Array
          ! Any Negatives?
          DO IRGN = 1,N_EM_RGN
            IF ( ANY( EM_REG_FAC( :,:,IRGN ) .LT. 0.0 ) ) THEN
               XMSG = 'Region ' // TRIM( EM_REGIONS( IRGN )%LABEL) // ' on file ' //
     &                TRIM( FILENAMES( IFILE )) // ' contains a ' //
     &                'negative value. The expected range is 0-1.'
               CALL M3ERR( PNAME, STDATE, STTIME, XMSG, .TRUE. )
            ELSE IF ( ANY( EM_REG_FAC( :,:,IRGN ) .GT. 1.01 ) ) THEN
               XMSG = 'Region ' // TRIM( EM_REGIONS( IRGN )%LABEL) // ' on file ' //
     &                TRIM( FILENAMES( IFILE )) // ' contains a ' //
     &                'value greater than 1. The expected range is 0-1.'
               CALL M3ERR( PNAME, STDATE, STTIME, XMSG, .TRUE. )
            END IF
          END DO

        END DO ! IFILE
      END IF
      
      END SUBROUTINE INIT_EMIS_REGIONS

!-----------------------------------------------------------------------
      SUBROUTINE READ_EMISS_NAMELIST( )
!
!     This subroutine opens and reads the Emissions Control Namelist. It 
!     attempts to deal with errors like missing file or missing file 
!     sections by error checking and setting defaults.
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      use em_param_module
      use RUNTIME_VARS, only : EMISSCTRL, logdev

      IMPLICIT NONE

         ! Define Dummy Variables for Opening Emission Control Namelist
         CHARACTER( 300 ) :: EQNAME
         INTEGER          :: EMCTRL_NML
         INTEGER          :: STAT

         ! Define General Parameters in Emission Control Namelist
         NAMELIST / GeneralSpecs / Guard_BiogenicVOC,
     &              Guard_MarineGas, Guard_LightningNO, Guard_WindBlownDust,
     &              Guard_SeaSpray

         ! Define Emissions Rules Input from Emissions Control Namelist
         NAMELIST / EmissionScalingRules / EM_NML
         NAMELIST / SizeDistributions    / SD_NML 
         NAMELIST / RegionsRegistry      / RGN_NML

         ! Allocate and Initialize Namelist Variables
         ALLOCATE( EM_NML( N_EM_RULE_REG ) )
         EM_NML%REGION = ''
         EM_NML%STREAM = ''
         EM_NML%SURR   = ''
         EM_NML%SPEC   = ''
         EM_NML%PHASE  = ''
         EM_NML%OP     = ''
         EM_NML%BASIS  = ''
         EM_NML%FAC    = 0.

         ALLOCATE( SD_NML( N_SD_REG ) )
         SD_NML%STREAM   = ''
         SD_NML%MODE     = ''
         SD_NML%MODE_REF = ''

         ALLOCATE( RGN_NML( N_REGIONS_REG ) )
         RGN_NML%LABEL = ''
         RGN_NML%FILE  = ''
         RGN_NML%VAR   = ''

         ! Retrieve the Name of the Emission Control File
         IF ( EMISSCTRL .EQ. "EMISSCTRL_NML" ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &              'You have chosen not to indicate the location of an',
     &              'Emission Control namelist file. Default settings ',
     &              'will be assumed.'
             Guard_BiogenicVOC   = .FALSE.
             Guard_MarineGas     = .FALSE.
             Guard_LightningNO   = .FALSE.
             Guard_WindBlownDust = .FALSE.
             Guard_SeaSpray      = .FALSE.
             RETURN
         END IF

         ! Open Emission Control Namelist File
         EMCTRL_NML = JUNIT()
         OPEN( FILE = EMISSCTRL, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &         POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

         ! Check for Error in File Open Process
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, '(5x,A,A,/,8x,A)' ),'ERROR: Could not read ',
     &              'emissions control namelist file: ',TRIM( EQNAME )
             CALL EXIT(1)
         END IF
         
         ! Read General Specification Section
         READ( NML = GeneralSpecs, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'GeneralSpecs section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             Guard_BiogenicVOC   = .FALSE.
             Guard_MarineGas     = .FALSE.
             Guard_LightningNO   = .FALSE.
             Guard_WindBlownDust = .FALSE.
             Guard_SeaSpray      = .FALSE.
         END IF

         ! Read the Regions Registry
         REWIND( EMCTRL_NML )
         READ( NML = RegionsRegistry, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'Regions_Registry section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             RGN_NML%LABEL = ''
             RGN_NML%FILE  = ''
             RGN_NML%VAR   = ''
         END IF

         ! Read the size distribution specification section
         REWIND( EMCTRL_NML )
         READ( NML = SizeDistributions, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'SizeDistReg section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             SD_NML%STREAM   = ''
             SD_NML%MODE     = ''
             SD_NML%MODE_REF = ''
         END IF

         ! Read the Emissions Rules to inform scaling operations
         REWIND( EMCTRL_NML )
         READ( NML = EmissionScalingRules, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'EmissionScaling section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             EM_NML%REGION = ''
             EM_NML%STREAM = ''
             EM_NML%SURR   = ''
             EM_NML%SPEC   = ''
             EM_NML%PHASE  = ''
             EM_NML%OP     = ''
             EM_NML%BASIS  = ''
             EM_NML%FAC    = 0.
         END IF
 
         CLOSE( UNIT = EMCTRL_NML )

      END SUBROUTINE READ_EMISS_NAMELIST  

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_1dx (vname, date, time, data)

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, ONLY : NLAYS

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:)

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,
     &               loc_jdate, loc_jtime, dsize
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          var_loc = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          dsize = size(data)

          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in any 2D file.'
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(2, loc_tail, var_loc) .ne. 0))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, vname)
                loc_head = head_grid(var_loc)
                loc_tail = tail_grid(var_loc)
             end if

             if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                cio_grid_data_tstamp(1, 2, var_loc) = date
                cio_grid_data_tstamp(2, 2, var_loc) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc))) then
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_grid_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_grid_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_grid_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_grid_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                store_end_ind = cio_grid_data_inx(2,2,var_loc)

                cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind) * ratio1
     &                                                       + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2

             end if
 
             store_beg_ind = cio_grid_data_inx(1,2,var_loc)

             data = cio_grid_data(store_beg_ind:store_beg_ind+dsize-1)

          end if

        end subroutine r_interpolate_var_1dx

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_1ds (fname, vname, date, time, data)

          USE UTILIO_DEFN
          USE STK_PRMS, only : MY_STRT_SRC, MY_END_SRC

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:)

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,
     &               loc_jdate, loc_jtime, dsize, pt
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          pt = binary_search (fname, cio_stack_file_name, NPTGRPS)

          var_loc = binary_search (vname, cio_stack_var_name(:,pt), n_cio_stack_emis_vars(pt))

          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in a stack file.'
          else
             dsize = MY_END_SRC( pt ) - MY_STRT_SRC( pt ) + 1

             loc_head = head_stack_emis(var_loc, pt)
             loc_tail = tail_stack_emis(var_loc, pt)

             if ((cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .lt. date) .or.
     &           ((cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .lt. time) .and.
     &            (cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .ne. 0))) then

                loc_jdate = cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt)
                loc_jtime = cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) 
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_stack_data (loc_jdate, loc_jtime, fname, vname)
                loc_head = head_stack_emis(var_loc, pt)
                loc_tail = tail_stack_emis(var_loc, pt)
             end if

             if ((cio_stack_emis_data_tstamp(1, 2, var_loc, pt) .eq. date) .and.
     &           (cio_stack_emis_data_tstamp(2, 2, var_loc, pt) .eq. time)) then
                count = count + 1
             else

                cio_stack_emis_data_tstamp(1, 2, var_loc, pt) = date
                cio_stack_emis_data_tstamp(2, 2, var_loc, pt) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) then
                   if (cio_stack_emis_data_tstamp(1, loc_head, var_loc, pt) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_stack_emis_data_inx(1,loc_head,var_loc, pt)
                head_end_ind  = cio_stack_emis_data_inx(2,loc_head,var_loc, pt)
                tail_beg_ind  = cio_stack_emis_data_inx(1,loc_tail,var_loc, pt)
                tail_end_ind  = cio_stack_emis_data_inx(2,loc_tail,var_loc, pt)
                store_beg_ind = cio_stack_emis_data_inx(1,2,var_loc, pt)
                store_end_ind = cio_stack_emis_data_inx(2,2,var_loc, pt)

                cio_stack_data(store_beg_ind:store_end_ind) =   cio_stack_data(head_beg_ind:head_end_ind) * ratio1
     &                                                        + cio_stack_data(tail_beg_ind:tail_end_ind) * ratio2

             end if
 
             store_beg_ind = cio_stack_emis_data_inx(1,2,var_loc, pt)

             data(1:dsize) = cio_stack_data(store_beg_ind:store_beg_ind+dsize-1)

          end if

        end subroutine r_interpolate_var_1ds

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_2d (vname, date, time, data,
     &                                   scol, ecol, srow, erow, slay)

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, ONLY : NLAYS

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:,:)
          integer, intent(in), optional :: scol, ecol, srow, erow, slay

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,
     &               loc_jdate, loc_jtime, adj_lvl, adj1, adj2,
     &               loc_size_spatial
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          var_loc = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in any 2D file.'
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if (cio_grid_var_name(var_loc,2) .eq. 'd') then
                loc_size_spatial = size_d2dx
             else
                loc_size_spatial = size_c3d / nlays
             end if

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(2, loc_tail, var_loc) .ne. 0))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, vname)
                loc_head = head_grid(var_loc)
                loc_tail = tail_grid(var_loc)
             end if

             if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                cio_grid_data_tstamp(1, 2, var_loc) = date
                cio_grid_data_tstamp(2, 2, var_loc) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc))) then
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_grid_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_grid_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_grid_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_grid_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                store_end_ind = cio_grid_data_inx(2,2,var_loc)

                cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind) * ratio1
     &                                                       + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2

             end if
 
             adj_lvl = 0
             adj1 = 0
             adj2 = 0
             if (present(slay)) then
                if (cio_grid_var_name(var_loc,2) .eq. '3') then
                   if ((window) .and. 
     &                 ((size(data,1) - ncols) .eq. 0)) then
                      adj1 = ncols + 3
                      adj2 = 2
                   end if
                   adj_lvl = (slay - 1) * loc_size_spatial
                else if (cio_grid_var_name(var_loc,2) .eq. 'd') then
                   adj_lvl = (slay - 1) * size_d2dx
                   if (.not. east_pe) then
                      adj2 = 1
                   end if
                end if
             else if (cio_grid_var_name(var_loc,2) .eq. '2') then
                if (.not. east_pe) then
                   adj2 = 1
                end if
             end if

             store_beg_ind = cio_grid_data_inx(1,2,var_loc)
             m = store_beg_ind - 1 + adj_lvl + adj1

             do r = 1, size(data,2)
                do c = 1, size(data,1)
                   m = m + 1
                   data(c,r) = cio_grid_data(m)
                end do
                m = m + adj2
             end do

          end if

        end subroutine r_interpolate_var_2d

! -------------------------------------------------------------------------
        subroutine i_interpolate_var_2d (vname, date, time, data)

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, ONLY : NLAYS

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          integer, intent(out)      :: data(:,:)

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,
     &               loc_jdate, loc_jtime, adj_lvl, adj1, adj2,
     &               loc_size_spatial
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          var_loc = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in any 2D file.'
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if (cio_grid_var_name(var_loc,2) .eq. 'd') then
                loc_size_spatial = size_d2dx
             else
                loc_size_spatial = size_c3d / nlays
             end if

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(2, loc_tail, var_loc) .ne. 0))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, vname)
                loc_head = head_grid(var_loc)
                loc_tail = tail_grid(var_loc)
             end if

             if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                cio_grid_data_tstamp(1, 2, var_loc) = date
                cio_grid_data_tstamp(2, 2, var_loc) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc))) then
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_grid_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_grid_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_grid_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_grid_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                store_end_ind = cio_grid_data_inx(2,2,var_loc)

                cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind) * ratio1
     &                                                       + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2

             end if
 
             adj_lvl = 0
             adj1 = 0
             adj2 = 0

             store_beg_ind = cio_grid_data_inx(1,2,var_loc)
             m = store_beg_ind - 1 + adj_lvl + adj1

             do r = 1, size(data,2)
                do c = 1, size(data,1)
                   m = m + 1
                   data(c,r) = int(cio_grid_data(m))
                end do
                m = m + adj2
             end do

          end if

        end subroutine i_interpolate_var_2d

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_2dx (vname, date, time, data, flag)

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, ONLY : NLAYS

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          logical, intent(in)       :: flag
          real, intent(out)         :: data(:,:)

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c, k, w,
     &               beg_k, end_k,
     &               loc_jdate, loc_jtime, adj1, adj2, adj3,
     &               loc_ncols, loc_nrows
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          var_loc = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in any 2D file.'
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if (cio_grid_var_name(var_loc, 2) .eq. 'd') then
                loc_ncols = ENDCOLMD3x - STRTCOLMD3x + 1
                loc_nrows = ENDROWMD3x - STRTROWMD3x + 1
             else
                loc_ncols = ENDCOLMC2x - STRTCOLMC2x + 1
                loc_nrows = ENDROWMC2x - STRTROWMC2x + 1
             end if

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(2, loc_tail, var_loc) .ne. 0))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, vname)
                loc_head = head_grid(var_loc)
                loc_tail = tail_grid(var_loc)
             end if

             if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                cio_grid_data_tstamp(1, 2, var_loc) = date
                cio_grid_data_tstamp(2, 2, var_loc) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc))) then
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_grid_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_grid_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_grid_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_grid_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                store_end_ind = cio_grid_data_inx(2,2,var_loc)

                cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind) * ratio1
     &                                                       + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2

             end if
 
             if (cio_grid_var_name(var_loc, 2) .eq. 'd') then
                loc_ncols = ENDCOLMD3x - STRTCOLMD3x + 1
                loc_nrows = ENDROWMD3x - STRTROWMD3x + 1
             else
                loc_ncols = ENDCOLMC2x - STRTCOLMC2x + 1
                loc_nrows = ENDROWMC2x - STRTROWMC2x + 1
             end if

             adj1 = 0
             adj2 = 0
             adj3 = 0
             if (window) then
                if (cio_grid_var_name(var_loc, 2) .ne. 'd') then

                   adj1 = ncols + 3
                   if (.not. east_pe) then
                      adj2 = 1
                   else
                      adj2 = 2
                   end if

                   if (north_pe .and. east_pe) then
                      adj3 = 2 * ncols + 6
                   else if (north_pe) then
                      adj3 = 2 * ncols + 5
                   else if (east_pe) then
                      adj3 = ncols + 4
                   else
                      adj3 = ncols + 3
                   end if
                end if
             end if

             store_beg_ind = cio_grid_data_inx(1,2,var_loc)
             m = store_beg_ind - 1 + adj1

             beg_k = 1
             if (cio_grid_var_name(var_loc, 2) .eq. '2') then
                end_k = 1
             else
                end_k = size(data,2)
             end if

             do k = beg_k, end_k
                w = 0
                do r = 1, loc_nrows
                   do c = 1, loc_ncols
                      m = m + 1
                      w = w + 1
                      data(w,k) = cio_grid_data(m)
                   end do
                   m = m + adj2
                end do
                m = m - adj2 + adj3
             end do

          end if

        end subroutine r_interpolate_var_2dx

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_2db (vname, date, time, data, type, lvl)

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE VGRD_DEFN, ONLY : NLAYS

          character (*), intent(in) :: vname
          character (1), intent(in) :: type
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:,:)
          integer, intent(in), optional :: lvl

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,k, ib,
     &               loc_jdate, loc_jtime, starting_pt, mype_p1,
     &               beg_k, end_k
          integer, save :: lns_size, lew_size, gns_size, gew_size,
     &                     ls_start, ls_end, ln_start, ln_end,
     &                     le_start, le_end, lw_start, lw_end,
     &                     gs_skip, ge_skip, gn_skip, gw_skip
          logical :: loc_firstime = .true.
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          if (loc_firstime) then
             loc_firstime = .false.

             mype_p1 = mype + 1 
             LNS_SIZE = NTHIK * ( NCOLS + NTHIK )
             LEW_SIZE = NTHIK * ( NROWS + NTHIK )

             LS_START = 1
             LS_END   = LNS_SIZE
             LE_START = LS_END + 1
             LE_END   = LE_START + LEW_SIZE - 1
             LN_START = LE_END + 1
             LN_END   = LN_START + LNS_SIZE - 1
             LW_START = LN_END + 1
             LW_END   = LW_START + LEW_SIZE - 1

             GNS_SIZE = NTHIK * ( GL_NCOLS + NTHIK )
             GEW_SIZE = NTHIK * ( GL_NROWS + NTHIK )

             GS_SKIP = NTHIK*( COLSX_PE( 1, mype_p1 ) - 1 ) - LS_START + 1
             GE_SKIP = GNS_SIZE + NTHIK*( ROWSX_PE( 1, mype_p1 ) - 1 ) - LE_START + 1
             GN_SKIP = GNS_SIZE + GEW_SIZE + NTHIK*( COLSX_PE( 1, mype_p1 ) - 1 ) - LN_START + 1
             GW_SKIP = 2*GNS_SIZE + GEW_SIZE + NTHIK*( ROWSX_PE( 1, mype_p1 ) - 1 ) - LW_START + 1

          end if

          var_loc = binary_search (vname, cio_bndy_var_name(:,1), n_cio_bndy_vars)

          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a35)') 'Warning: ', trim(vname), ' is not available in any bndy file.'
          else
             loc_head = head_bndy(var_loc)
             loc_tail = tail_bndy(var_loc)

             if ((cio_bndy_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_bndy_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_bndy_data_tstamp(2, loc_tail, var_loc) .ne. 0))) then

                loc_jdate = cio_bndy_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_bndy_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_time_dep_boundary_data (loc_jdate, loc_jtime, vname)
                loc_head = head_bndy(var_loc)
                loc_tail = tail_bndy(var_loc)
             end if

             if ((cio_bndy_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_bndy_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
          write (cio_logdev, '(a17, i15, i8, 1x, a16)') ' ==d== count 2db ', count, time, vname
             else

                cio_bndy_data_tstamp(1, 2, var_loc) = date
                cio_bndy_data_tstamp(2, 2, var_loc) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_bndy_data_tstamp(2, loc_head, var_loc))) then
                   if (cio_bndy_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_bndy_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_bndy_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_bndy_data_tstamp(2, loc_head, var_loc)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_bndy_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_bndy_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_bndy_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_bndy_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_bndy_data_inx(1,2,var_loc)
                store_end_ind = cio_bndy_data_inx(2,2,var_loc)

                cio_bndy_data(store_beg_ind:store_end_ind) =   cio_bndy_data(head_beg_ind:head_end_ind) * ratio1
     &                                                       + cio_bndy_data(tail_beg_ind:tail_end_ind) * ratio2

             end if

             if (present(lvl)) then
                beg_k = lvl
                end_k = lvl
             else
                beg_k = 1
                end_k = nlays
             end if

             data = 0.0
             store_beg_ind = cio_bndy_data_inx(1,2,var_loc)
             DO k = beg_k, end_k
                starting_pt = store_beg_ind + (k - 1) * size_b2d - 1
! Construct SOUTH boundary
                IF ( SOUTH_PE ) THEN
                   m = starting_pt + GS_SKIP
                   DO IB = LS_START, LS_END
                      data( IB,k ) = cio_bndy_data( m+IB )
                   END DO
                END IF

! Construct EAST boundary
                IF ( EAST_PE ) THEN
                   m = starting_pt + GE_SKIP
                   DO IB = LE_START, LE_END
                      data( IB,k ) = cio_bndy_data( m+IB)
                   END DO
                END IF

! Construct NORTH boundary
                IF ( NORTH_PE ) THEN
                   m = starting_pt + GN_SKIP
                   DO IB = LN_START, LN_END
                      data( IB,k ) = cio_bndy_data( m+IB)
                   END DO
                END IF

! Construct WEST boundary
                IF ( WEST_PE ) THEN
                   m = starting_pt + GW_SKIP
                   DO IB = LW_START, LW_END
                      data( IB,k ) = cio_bndy_data( m+IB)
                   END DO
                END IF
             END DO

          end if

        end subroutine r_interpolate_var_2db

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_3d (vname, date, time, data, fname)

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:,:,:)
          character (*), intent(in), optional :: fname

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c, k,
     &               loc_jdate, loc_jtime, beg_k, end_k, dot,
     &               col_size, extra_c, extra_r, adj1, adj2, adj3,
     &               slen
          character (20) :: loc_vname
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2

          if (present(fname)) then
             slen = len_trim(fname)
             loc_vname = trim(vname) // fname(slen-3:slen)
          else
             loc_vname = vname
          end if

          var_loc = binary_search (loc_vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(loc_vname), ' is not available in any 3D file.'
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(2, loc_tail, var_loc) .ne. 0))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, cio_data_tstep )
                call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, loc_vname)
                loc_head = head_grid(var_loc)
                loc_tail = tail_grid(var_loc)
             end if

             if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                cio_grid_data_tstamp(1, 2, var_loc) = date
                cio_grid_data_tstamp(2, 2, var_loc) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc))) then
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time_to_sec(cio_data_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                else
                   lcount = lcount + 1
                end if

                head_beg_ind  = cio_grid_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_grid_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_grid_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_grid_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                store_end_ind = cio_grid_data_inx(2,2,var_loc)

                cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind) * ratio1
     &                                                       + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2

             end if
 
             beg_k = 1
             if (cio_grid_var_name(var_loc, 2) .eq. 'e') then
                end_k = 1
             else
                end_k = size(data,3)
             end if

             adj1 = 0
             adj2 = 0
             adj3 = 0
             if (window) then
                if (((size(data,1) - ncols) .eq. 0) .and.
     &              (cio_grid_var_name(var_loc, 2) .eq. '3')) then
                   adj1 = ncols + 3
                   adj2 = 2
                   adj3 = 2 * ncols + 4
                else if (cio_grid_var_name(var_loc, 2) .eq. 'd') then
                   adj1 = 0

                   if (.not. east_pe) then
                      adj2 = 1
                   else
                      adj2 = 0
                   end if

                   if (north_pe .and. east_pe) then
                      adj3 = 0
                   else if (north_pe) then
                      adj3 = 1
                   else if (east_pe) then
                      adj3 = x_dot_ncols
                   else
                      adj3 = x_dot_ncols + 1
                   end if
                end if
             else
                extra_c = 0
                extra_r = 0

                if (cio_grid_var_name(var_loc, 2) .eq. 'd') then
                    extra_c = x_dot_ncols - size(data, 1)
                    extra_r = x_dot_nrows - size(data, 2)
                    col_size = dot_ncols
                    dot = 1
                else
                    extra_c = x_cro_ncols - size(data, 1)
                    extra_r = x_cro_nrows - size(data, 2)
                    col_size = cro_ncols
                    dot = 0
                end if

                adj2 = extra_c
                adj3 = extra_r * col_size + extra_c
                if (north_pe .and. east_pe) then
                   adj3 = 0
                else if (north_pe) then
                   adj3 = adj3 - 1
                end if

             end if

             store_beg_ind = cio_grid_data_inx(1,2,var_loc)
             m = store_beg_ind - 1 + adj1
             do k = beg_k, end_k
                do r = 1, size(data,2)
                   do c = 1, size(data,1)
                      m = m + 1
                      data(c,r,k) = cio_grid_data(m)
                   end do
                   m = m + adj2
                end do
                if (window .and. (cio_grid_var_name(var_loc, 2) .eq. 'd')) then
                   m = m - adj2 + adj3
                else
                   m = m + adj3
                end if
             end do

          end if

        end subroutine r_interpolate_var_3d

      END MODULE CENTRALIZED_IO_MODULE
