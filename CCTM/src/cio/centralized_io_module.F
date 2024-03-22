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
!  02/11/19, D. Wong: Updated to accommodate STAGE option
!  03/06/19, D. Wong: fixed a bug to handle 3D emission data structure
!                     correctly and fixed a bug to deal with the case of
!                     ABFLUX turned off
!  04/01/19, D. Wong: -- enhanced robustness to handle time independent or 
!                        dependent boundary condition file
!                     -- used two different CPP flags, m3dry_opt and stage_opt
!                        to distinguish these two deposition options
!                     -- reorganized the code to read in certain files when
!                        they are available as well as based on environmental 
!                        variable setting
!  05/02/19, D. Wong: -- added a logic to call soilinp_setup when BIOGEMIS is true
!  05/03/19, D. Wong: -- reorganized the flow of reading in LUS data
!  05/06/19, D. Wong: -- added a new logic to read in INIT_MEDC_1 when it is not NEW_START
!  05/07/19, D. Wong: -- removed duplicated array allocation for NH4ps1 and NH4ps2
!  05/13/19, D. Wong: -- expanded implementation to hanndle ISAM model
!  05/15/19, D. Wong: -- used USE_MARINE_GAS_EMISSION variable defined in RUNTIME_VAR.F 
!                        to turn on a block of code related to marine gas emssion
!  06/18/19, D. Wong: -- modified cio implementation to handle:
!                        * emission file date is differ from simulation date
!                        * region files for scaling purposes
!  06/19/19, D. Wong: -- fixed a bug in the EMIS regions subroutine
!  07/08/19, F. Sidi: -- Renamed E2C_FERT -> E2C_CHEM & BELD4_LU -> E2C_LU
!  07/09/19, T. Spero: -- Changed file for fractional land use from
!                         GRIDCRO2D to LUFRAC_CRO.  Allow backward
!                         compatibility.
!  07/17/19, R. Gilliam:- Removed the FPAR file call for windblow dust. MCIP VEG is used.
!  08/12/19, F. Sidi: -- Allowed lus_setup to use fractional land use from
!                        GRIDCRO2D or LUFRAC_CRO. Allows backward compatibility.
!  08/01/19, D. Wong:- Made modification so centralized I/O works with two-way model
!                    - used new variable type descriptor
!  09/10/19, D. Wong:- Extended to handle BC file with non 1-hr time step
!  09/19/19, D. Wong:- Used the start simulation time to pick up the very first emission 
!                      data point rather than the start time in the emission file
!  09/20/19, D. Wong:- Extended the capability to handle 3D emission files with various
!                      number of layers less than of equal to the model number of layers
!  10/04/19, D. Wong:- fixed the time advancement, NEXTIME, for a multi-day run
!  11/22/19, F. Sidi:- Updated cio with new algorithm (developed by D. Wong) 
!                      to enable running CMAQ with different files having
!                      different time steps, cleaned up code no longer needed
!                      & two-way model bugfixes
!  01/30/20, D. Wong:- fixed IC file interpolation time stamp issue by bypassing the
!                      check whether the new request falls within the circular buffer
!                      for IC variable which only has one time step of data.
!  02/10/20, F. Sidi:- Changed file_tstep from tstep3d to met_tstep an environment
!                      variable the flexlible allows users to toggle the temporal
!                      frequency of their input meterology.
!  03/05/20, D. Wong: Expanded CIO functionalities to MPAS as well
!  07/24/20, D. Wong: Fixed a bug, the code did not handle calling NEXTIME properly in
!                     an extreme case, i.e. simulation runs in a hourly basis, in the
!                     retrieve_boundary_data subroutine.
!  08/06/20, D. Wong:- fixed excessive reading of time independent boundary file data
!  02/23/21, D. Wong:- used KZMIN setting to determine reading in PURB or not
!  03/23/21. D. Wong:- modified code to accommodate a flexibility to allow each input 
!                      can have different XORIG and YORIG settings than the simulation
!                      domain if it can be overlapped with the simulation domain 
!                      perfectly w.r.t. domain resolution
!  11/17/21, G. Sarwar: Changed minimum values from 0.0 to 0.001 for ocean and szone  
!                       to ensure values are nonnegative and greater than 0.001
!  01/17/22, D. Wong: Added SAVE attribute to variable FIRSTIME
!  03/31/22, J. Willison: Removed wb_dust_setup and modified lus_setup to remove
!                         BELD as an option for desert land information.  
!  04/12/22, G. Sarwar: Revised to include "DMS" into cb6r5_ae7_aq 
!------------------------------------------------------------------------!

!------------------------------------------------------------------------!
! Variable type notation:
!   'mc2'    denote met cro 2d variable
!   'mc3'    denote 3d variable
!   'md3'    denote dot variable
!   'wb'     denote wind blown dust
!   'ic'     denote initial condition variable
!   'is'     denote ISAM initial condition variable
!   'e2d'    denote emission 2d variable
!   'e3d'    denote emission 3d variable
!   'lnt'    denote lightning variable
!   'mb'     denote met 3D boundary variable
!   'bct'    denote time dependent   3D boundary variable
!   'bc'     denote time independent 3D boundary variable
!------------------------------------------------------------------------!

      MODULE CENTRALIZED_IO_MODULE

        use RUNTIME_VARS, only : LTNG_NO, STDATE, STTIME, ABFLUX, MOSAIC, 
     &                           NPTGRPS, USE_MARINE_GAS_EMISSION, logdev,
     &                           CONVECTIVE_SCHEME, EMIS_SYM_DATE
        use CENTRALIZED_IO_UTIL_MODULE
        use get_env_module 
        USE UTILIO_DEFN
#ifdef mpas
        use coupler_module
        use mio_module
#endif

        implicit none

        integer, parameter :: max_nfiles = 500

        character (20), parameter :: biogemis_fname   = 'BEIS_NORM_EMIS'

! to recognize the time step in each file could be different, in the new revised 
! implementation will address that and here is the algorithm. When open a new file, 
! n_opened_file will be incremented by one to keep track of how many have been 
! opened. Each file has a unique f_name except met files which will be shared with 
! one f_met since their tsteps should be the same. Then n_opened_file is assigned 
! to an opened time dependent file (defined below) and time information will be 
! stored accordingly.

        integer :: n_opened_file = 0
        integer :: f_met, f_ltng, f_bcon, f_icon, f_is_icon, f_mbiog
        integer, allocatable :: f_emis(:), f_stk_emis(:)

        integer :: file_sdate(max_nfiles) = -1
        integer :: file_stime(max_nfiles) = -1
        integer :: file_tstep(max_nfiles) = -1
        real*8  :: file_xcell(max_nfiles) = 0.0d0
        real*8  :: file_ycell(max_nfiles) = 0.0d0
        logical :: file_sym_date(max_nfiles)  

        CHARACTER( 40 ), parameter :: NLDN_STRIKES = 'NLDN_STRIKES'
        CHARACTER( 40 ), parameter :: ICFILE       = 'INIT_CONC_1'
        CHARACTER( 40 ), parameter :: BCFILE       = 'BNDY_CONC_1'
        CHARACTER( 40 ), parameter :: ISAM_PREVDAY = 'ISAM_PREVDAY'

! time independent data
        real, allocatable :: MSFX2(:,:),         ! from GRID_CRO_2D data
     &                       LWMASK(:,:),        ! from GRID_CRO_2D data
     &                       HT(:,:),            ! from GRID_CRO_2D data
     &                       LAT(:,:),           ! from GRID_CRO_2D data
     &                       LON(:,:),           ! from GRID_CRO_2D data
     &                       PURB(:,:),          ! from GRID_CRO_2D data
     &                       LUFRAC(:,:,:),      ! from LUFRAC_CRO data
     &                       SOILCAT_A(:,:),     ! from MET_CRO_2D
     &                       MSFD2(:,:),         ! from GRID_DOT_2D data
     &                       X3HT0M(:,:),        ! from GRID_CRO_3D data
     &                       X3HT0F(:,:),        ! from GRID_CRO_3D data
     &                       ocean(:,:),         ! from OCEAN data
     &                       szone(:,:),         ! from OCEAN data
     &                       chlr(:,:),          ! from OCEAN data
     &                       dmsl(:,:),          ! from OCEAN data
     &                       OCEAN_MASK(:,:),    ! from LTNG parameter data
     &                       SLOPE(:,:),         ! from LTNG parameter data
     &                       INTERCEPT(:,:),     ! from LTNG parameter data
     &                       SLOPE_lg(:,:),      ! from LTNG parameter data
     &                       INTERCEPT_lg(:,:),  ! from LTNG parameter data
     &                       ICCG_SUM(:,:),      ! from LTNG parameter data
     &                       ICCG_WIN(:,:),      ! from LTNG parameter data
     &                       AVGEMIS(:,:,:,:),   ! from BIOGEMIS data
     &                       GROWAGNO(:,:),      ! from BEIS_NORM_EMIS data
     &                       NGROWAGNO(:,:),     ! from BEIS_NORM_EMIS data
     &                       NONAGNO(:,:),       ! from BEIS_NORM_EMIS data
     &                       RAINFALL(:,:,:),    ! from SOILINP data
     &                       HRNO_SW(:,:,:),     ! from SOILINP data
     &                       HRNO_T2M(:,:,:),    ! from SOILINP data
     &                       LDF(:,:,:),         ! from MEGANMAP data
     &                       LAI_M(:,:,:),       ! from MEGANMAP data
     &                       EFMAPS(:,:,:),      ! from MEGANMAP data
     &                       CTF(:,:,:),         ! from MEGANMAP data
     &                       BDSNP_NDEP(:,:,:),  ! from MEGAN_BDSNP data
     &                       BDSNP_FERT(:,:),    ! from MEGAN_BDSNP data
     &                       DRYPERIOD(:,:),     ! from BDSNPINP data
     &                       NDEPRES(:,:),       ! from BDSNPINP data
     &                       NDEPRATE(:,:),      ! from BDSNPINP data
     &                       PFACTOR(:,:),       ! from BDSNPINP data
     &                       SOILMPREV(:,:),     ! from BDSNPINP data
     &                       T24y(:,:),          ! from MEGAN_SOILINP data
     &                       SW24y(:,:),         ! from MEGAN_SOILINP data
     &                       lai_y(:,:)          ! from MEGAN_SOILINP data


        integer, allocatable :: PTYPE(:,:),           ! from SOILINP data
     &                          PULSEDATE(:,:),       ! from SOILINP data
     &                          PULSETIME(:,:),       ! from SOILINP data
     &                          BDSNP_LANDTYPE(:,:),  ! from MEGAN_BDSNP data
     &                          BDSNP_ARID(:,:),      ! from MEGAN_BDSNP data
     &                          BDSNP_NONARID(:,:)    ! from MEGAN_BDSNP data

        character( 16 ), allocatable :: DDTTM( : )    ! for SOILINP data, description date and time

! time dependent data: 
! gridded 
        integer :: n_grid_cro_data_vars
        integer :: n_cio_grid_vars
        real, allocatable :: cio_grid_data(:)
        character (24), allocatable :: cio_grid_var_name(:,:)      ! stores variable name, variable type and met variable 
                                                                   ! or not information for each variable
        integer, allocatable :: cio_grid_data_inx (:,:,:),
     &                          head_grid(:), tail_grid(:),        ! head and tail of the gridded data circular buffer
     &                          cio_grid_data_tstamp(:,:,:)

        character (16) :: cio_dust_land_scheme
        character (20), allocatable :: cio_mpas_grid_data_tstamp(:,:)

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
        integer, allocatable :: cio_emis_file_loc(:)
        integer, allocatable :: cio_emis_nvars(:)
        integer, allocatable :: cio_emis_file_layer(:)
        integer, allocatable :: cio_emis_file_startcol(:)
        integer, allocatable :: cio_emis_file_endcol(:)
        integer, allocatable :: cio_emis_file_startrow(:)
        integer, allocatable :: cio_emis_file_endrow(:)
        integer              :: cio_emis_nlays                     ! max value among cio_emis_file_layer

! this is for MPAS only
        integer, allocatable :: num_dist_layers(:,:)      ! number of layers in MPAS grid has re-distributed emission data
        real, allocatable    :: dist_frac(:,:,:)          ! calculated layer distribution fraction
        real, allocatable    :: emis_file_layer_frac(:,:) ! given layer faction information
        integer              :: mpas_tstep                ! this is assigned in CMAQ_DRIVER

! - stack emission data
        real, allocatable    :: cio_stack_data(:)
        character (16), allocatable :: cio_stack_file_name(:),
     &                                 cio_stack_var_name(:,:),
     &                                 STKGNAME( : ),                      ! stack groups file name
     &                                 cio_mpas_stack_emis_timestamp(:)    ! for MPAS only

        integer, allocatable :: n_cio_stack_emis_vars(:),
     &                          cio_stack_file_loc(:),
     &                          n_cio_stack_emis_lays(:),
     &                          n_cio_stack_emis_pts(:),
     &                          cio_stack_emis_data_inx (:,:,:,:),
     &                          head_stack_emis(:,:), tail_stack_emis(:,:),  ! head and tail of the stack emis data circular buffer
     &                          cio_stack_emis_data_tstamp(:,:,:,:)

        integer :: modis_data_sdate         ! modis dust data start date

        integer :: cio_model_sdate, 
     &             cio_model_stime         ! model start date and time

        logical, private :: cio_LTNG_NO 

        real :: CONVPA             ! Pressure conversion factor file units to Pa  
        Real :: P0                 ! reference pressure (100000.0 Pa) for Potential Temperature,
                                   ! note that in meteorology they do not use the SI 1 ATM.  

! availability of various variable
        logical :: CFRAC_3D_AVAIL = .true.,  ! CFRAC_3D is available or not
     &             PV_AVAIL       = .false., ! Potential Vorticity is available or not
     &             TSEASFC_AVAIL  = .false., ! SST is available or not
     &             WSPD10_AVAIL,             ! WSPD10 is available or not
     &             UWINDC_AVAIL,             ! UWINDC is available in DOT file or not
     &             VWINDC_AVAIL,             ! VWINDC is available in DOT file or not
     &             QG_AVAIL       = .true.,  ! flag for QG available in MET_CRO_3D
     &             QI_AVAIL,                 ! flag for QI available in MET_CRO_3D
     &             QS_AVAIL,                 ! flag for QS available in MET_CRO_3D
     &             QC_AVAIL       = .true.,  ! flag for QC and it is always set to .true.
     &             JACOBF_AVAIL,             ! flag for JACOBF available in MET_CRO_3D
     &             RNA_AVAIL      = .false., ! flag for RNA available in MET_CRO_2D
     &             RCA_AVAIL      = .false., ! flag for RCA available in MET_CRO_2D
     &             RA_RS_AVAIL    = .true.,  ! flag for RA and RS available in MET_CRO_2D
     &             Q2_AVAIL       = .true.,  ! flag for Q2, two meter mixing ratio available in MET_CRO_2D
     &             LH_AVAIL,                 ! flag for LH, two meter mixing ratio available in MET_CRO_2D
     &             HAS_SEAICE,               ! flag for SEAICE in MET_CRO_2D
     &             WR_AVAIL       = .true.,  ! flag for WR, canopy wetness available in MET_CRO_2D
     &             MEDC_AVAIL     = .true.,  ! file INIT_MEDC_1 is available
     &             E2C_CHEM_AVAIL = .true.,  ! file E2C_CHEM is available
     &             GMN_AVAIL      = .false., ! variable GMN available in E2C_CHEM or not
     &             LUCRO_AVAIL,              ! file LUFRAC_CRO is available
     &             PXSOIL_AVAIL              ! flag for WRFv4.1+ PX LSM soil extras in MET_CRO_2D

! Met data is large enough to cover boundary and no MET_BDY_3D will be used
        logical :: window

        logical :: east_pe, south_pe, west_pe, north_pe

        INTEGER :: TEMPG_LOC
        INTEGER :: TSEASFC_LOC

        integer :: STRTCOLSTD,  ENDCOLSTD,  STRTROWSTD,  ENDROWSTD,   ! this is for standard domain useful for coupled model
     &             STRTCOLMC2,  ENDCOLMC2,  STRTROWMC2,  ENDROWMC2,
     &             STRTCOLMC2x, ENDCOLMC2x, STRTROWMC2x, ENDROWMC2x,  ! extension setup for READMC2
     &             STRTCOLMC3,  ENDCOLMC3,  STRTROWMC3,  ENDROWMC3,
     &             STRTCOLMD3,  ENDCOLMD3,  STRTROWMD3,  ENDROWMD3,
     &             STRTCOLMD3x, ENDCOLMD3x, STRTROWMD3x, ENDROWMD3x,  ! extension setup for READMD3
     &             STRTCOLIC,   ENDCOLIC,   STRTROWIC,   ENDROWIC,    ! for ICFILE
     &             STRTCOLISIC, ENDCOLISIC, STRTROWISIC, ENDROWISIC,  ! for ISAM ICFILE
     &             STRTCOLLNT,  ENDCOLLNT,  STRTROWLNT,  ENDROWLNT   ! for lightning strike file

        private :: gridded_files_setup,
     &             retrieve_lufrac_cro_data
#ifdef mpas
     &             ,retrieve_ocean_data_mpas
#else
     &             ,boundary_files_setup,
     &             retrieve_grid_cro_2d_data,
     &             retrieve_grid_dot_2d_data,
     &             retrieve_ocean_data
#endif

        integer, private :: count = 0
        integer, private :: cio_logdev, 
     &                              size_s2d,    ! standard 2d cro file size (in twoway model, size_s2d not equal to size_c2d
     &                              size_s3d,    ! standard 3d file size
     &                      n_c2d,  size_c2d,    ! cro 2d file info: # of variables and a variable size
     &                              size_c2dx,   ! extended cro 2d variable size
     &                              size_d2d,    ! a 2d dot variable size
     &                              size_d2dx,   ! extended 2d dot variable spatial size
     &                      n_c3d,  size_c3d,    ! cro 3d file info: # of variables and a variable size
     &                      n_d3d,  size_d3d,    ! dot 3d file info: # of variables and a variable size
     &                              size_d3dx,   ! extended dot 3d variable size
     &                      n_i3d,               ! # of initial condition 3d variables
     &                      n_is3d,              ! # of initial condition 3d variables for ISAM
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
     &                      s_cro_ncols, s_cro_nrows,   ! standard cro file nools and nrows (this is used to distinguish 
                                                        ! met cro and regular cro file in twoway coupled model
     &                        dot_ncols,   dot_nrows,   ! dot file nools and nrows 
     &                      x_dot_ncols, x_dot_nrows    ! extended dot file nools and nrows 

        integer, private :: cio_LTLYRS                  ! number of layers in lightning strike dataset
        CHARACTER( 16 )  :: LT_NAME                     ! LNT name: old Cis NLDNstrk and new is LNT

        interface interpolate_var
#ifdef mpas
          module procedure r_interpolate_var_1ds_mpas,
     &                     r_interpolate_var_2d_mpas,
     &                     i_interpolate_var_2d_mpas,
     &                     r_interpolate_var_3d_mpas
#else
          module procedure r_interpolate_var_1ds,       ! Interpolation for Stack Group Real 1-D Data 
     &                     r_interpolate_var_2d,        ! Interpolation for generic Real 2-D Data 
     &                     i_interpolate_var_2d,        ! Interpolation for generic Integer 2-D Data
     &                     r_interpolate_var_2db,       ! Interpolation for Boundary Real 2-D Data
     &                     r_interpolate_var_3d         ! Interpolation for generic Real 3-D Data 
#endif
        end interface

! MPAS only routines:

!  stack_files_setup_mpas
!  retrieve_stack_data_mpas
!  retrieve_ocean_data_mpas

!  r_interpolate_var_1d_mpas    ???
!  r_interpolate_var_1ds_mpas
!  r_interpolate_var_2d_mpas
!  i_interpolate_var_2d_mpas
!  r_interpolate_var_2dx_mpas   ???
!  r_interpolate_var_3d_mpas
 
! Non MPAS routines:

!  boundary_files_setup
!  stack_files_setup
!  biogemis_setup
!  beis_norm_emis_setup
!  depv_data_setup
!  medc_file_setup
!  retrieve_grid_cro_2d_data
!  retrieve_grid_dot_2d_data
!  retrieve_ocean_data
!  retrieve_ltng_param_data
!  retrieve_boundary_data
!  retrieve_stack_data

!  r_interpolate_var_1ds
!  r_interpolate_var_2d
!  i_interpolate_var_2d
!  r_interpolate_var_2db
!  r_interpolate_var_3d

! Common routines:

!  centralized_io_init
!  gridded_files_setup
!  retrieve_time_dep_gridded_data
!  retrieve_lufrac_cro_data
!  DESID_INIT_REGIONS
!  DESID_READ_NAMELIST
!  soilinp_setup
!  lus_setup
!  megan_setup

        contains

! -------------------------------------------------------------------------
        subroutine centralized_io_init (in_ncols)

          use lsm_mod, only: n_lufrac, init_lsm
          USE UTILIO_DEFN
          USE RUNTIME_VARS, only: log_heading, logdev

#ifdef mpas
          use hgrd_defn, only : ncols
          use RUNTIME_VARS, only : WB_DUST, ocean_chem, BIOGEMIS_MEGAN, BIOGEMIS_BEIS
          use lus_defn, only : lus_init
#else
          USE HGRD_DEFN
          use cgrid_spcs, only : GC_DDEP, N_GC_DDEP
!         use util_module, only : index1

          INCLUDE SUBST_FILES_ID             ! file name parameters
#endif

          integer, intent(in), optional :: in_ncols

          Character( 40 ), parameter :: pname = 'centralized_io_init'

          logical, save :: first_time = .true.
          INTEGER       :: STAT
          CHARACTER( 120 ) :: XMSG = ' '
          Character( 16 ) :: vname

          if (first_time) then
             first_time = .false.
             call log_heading( logdev, 'Opening CMAQ Input Files' )

#ifdef mpas
             call gridded_files_setup

             call retrieve_lufrac_cro_data

             if (wb_dust) then
                call lus_setup
             end if

!            cio_logdev = 6

             if ( WB_DUST ) then
                if (.not. lus_init (mminlu_mpas, lufrac_data) ) then
                   print *, ' Error: Cannot initialize Land Use category'
                   stop
                end if
             end if

             allocate (lwmask(in_ncols, 1),
     &                 lat(in_ncols, 1),
     &                 lon(in_ncols, 1),
     &                 ht(in_ncols, 1),
     &                 stat=stat)

             lon    = g2ddata(:,:,lon_ind)
             lat    = g2ddata(:,:,lat_ind)
             ht     = g2ddata(:,:,ht_ind)
             lwmask = g2ddata(:,:,lwmask_ind)

             call retrieve_ocean_data_mpas

             cio_model_sdate = stdate
             cio_model_stime = sttime

             call stack_files_setup_mpas

             if (BIOGEMIS_MEGAN) then
                call megan_setup
             end if
#else
             cio_logdev = init3()

             cio_model_sdate = STDATE
             cio_model_stime = STTIME

             east_pe = (mod(mype, npcol) .eq. npcol - 1)
             west_pe = (mod(mype, npcol) .eq. 0)
             north_pe = (mype .ge. npcol * (nprow - 1))
             south_pe = (mype .lt. npcol)

             cio_LTNG_NO = LTNG_NO

             MEDC_AVAIL = .true.
             If ( .Not. Open3( INIT_MEDC_1, fsread3, pname ) ) Then
                MEDC_AVAIL = .false.
                if (abflux) then
                   E2C_CHEM_AVAIL = .true.
                   If ( .Not. Open3( E2C_CHEM, fsread3, pname ) ) Then
                      XMSG = 'Open failure for ' // E2C_CHEM
                      Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                      E2C_CHEM_AVAIL = .false.
                   END IF
                   n_opened_file = n_opened_file + 1
                else
                   E2C_CHEM_AVAIL = .false.
                end if
             END IF

             if (MEDC_AVAIL) then
                n_opened_file = n_opened_file + 1
             end if

             call gridded_files_setup

             call boundary_files_setup

             call stack_files_setup

             if (BIOGEMIS_BEIS) then
                call biogemis_setup
                call beis_norm_emis_setup
             end if
             if (BIOGEMIS_MEGAN) then
                call megan_setup
             end if


             if (ABFLUX) then
                call depv_data_setup
             end if

             if (LUCRO_AVAIL) then
                call retrieve_lufrac_cro_data
             end if

             if (WB_DUST) then
                if (.not. PX_LSM) then
                   XMSG = 'WB_DUST requires PX LSM (PX_VERSION Y)'
                   Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                end if
                call lus_setup
             end if

             if (HGBIDI .and. (.not. NEW_START)) then   ! two level check, 1. environment variable and then GC_DDEP species list
                if ( index1 ( 'HG', N_GC_DDEP, GC_DDEP) .gt. 0 ) then 
                   call medc_file_setup
                end if
             end if

             call retrieve_grid_cro_2d_data

             call retrieve_grid_dot_2d_data

             call retrieve_ocean_data

             if (cio_LTNG_NO) then
                call retrieve_ltng_param_data 
             end if
#endif
             if (BIOGEMIS_BEIS .or. BIOGEMIS_MEGAN) then
                call soilinp_setup
             end if

          end if

          call retrieve_time_dep_gridded_data (cio_model_sdate, cio_model_stime)

#ifdef mpas
          call retrieve_stack_data_mpas (cio_model_sdate, cio_model_stime)
#else
          call retrieve_boundary_data (cio_model_sdate, cio_model_stime)

          call retrieve_stack_data (cio_model_sdate, cio_model_stime)
#endif

        end subroutine centralized_io_init
 
! -------------------------------------------------------------------------
        subroutine gridded_files_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows, mype, colsx_pe, rowsx_pe
          USE VGRD_DEFN, only : VGTYP_GD, nlays
          USE RUNTIME_VARS, only : N_FILE_GR,
     &                             STDATE, WB_DUST, ISAM_NEW_START,
     &                             local_tstep, met_tstep, NLDNSTRIKE,
     &                             LPVO3
          use LSM_Mod, only : LAND_SCHEME
          use cgrid_spcs, only : n_gc_spcd, n_ae_spc
#ifdef mpas
          use centralized_io_util_module, only : ext_layer_info, cal_distribution , 
     &                                           binary_search, quicksort
          use util_module, only : sec2time, nextime, index1, secsdiff
#endif

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'gridded_files_setup'

          CHARACTER( 120 ) :: XMSG = ' '
          INTEGER          :: GXOFF, GYOFF, stat, n, v, d_size, begin, end, adj,
     &                        n_dust_vars, idx, t, ldate, ltime,
     &                        nl, s, e, c, time, floc
          character( 32 )  :: tname, fname

          character( 24 ), allocatable :: c2d_name(:, :), c3d_name(:, :), 
     &                                    d3d_name(:,:), emis_name(:,:),
     &                                    i3d_name(:,:), is3d_name(:,:),
     &                                    l2d_name(:,:), medc_name(:,:)
          logical :: done = .false.
          logical :: found

          integer, allocatable :: bottom(:), top(:)
          integer :: emis_file_dist_layer, tdate(2), ttime(2), diffsec

          logical :: layer_exist

#ifdef mpas
          n_c2d = 0
          n_c3d = 0
          n_d3d = 0
          size_d3dx = 1

          if (binary_search( 'LH', vname_2d, n2d_data) .gt. 0) then
             lh_avail = .true.
          else
             lh_avail = .false.
          end if

          n_opened_file = n_opened_file + 1
          f_met         = n_opened_file
          file_tstep(f_met) = mpas_tstep

          wspd10_avail = .true.

          P0 = 100000.0

          QI_AVAIL     = .true.
          QS_AVAIL     = .true.
          JACOBF_AVAIL = .false.
#else
! met grid cro 2d file
          IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// GRID_CRO_2D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
             XMSG = 'Could not get ' // GRID_CRO_2D //' file description'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          n_grid_cro_data_vars = nvars3d

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

! lufrac cro file
          IF ( .NOT. OPEN3( LUFRAC_CRO, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// LUFRAC_CRO // ' file'
             CALL M3WARN ( PNAME, 0, 0, XMSG )
             LUCRO_AVAIL = .FALSE.
             XMSG = 'Solution: Reading Land Use Fractions from GRID_CRO_2D file'
             WRITE(LOGDEV,'(5X,A)')TRIM( XMSG )
          ELSE
             n_opened_file = n_opened_file + 1
             LUCRO_AVAIL = .TRUE.
             IF ( .NOT. DESC3( LUFRAC_CRO ) ) THEN
                XMSG = 'Could not get ' // LUFRAC_CRO //' file description'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
          END IF

! met cro 2d file
          IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// MET_CRO_2D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          n_opened_file = n_opened_file + 1
          f_met = n_opened_file
          IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
             XMSG = 'Could not get ' // MET_CRO_2D //' file description'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          file_sdate(f_met) = sdate3d
          file_stime(f_met) = stime3d
#ifdef twoway
          file_tstep(f_met) = tstep3d
#else
          file_tstep(f_met) = met_tstep ! offline model controlled by runtime var MET_TSTEP
#endif
          file_xcell(f_met) = xcell3d
          file_ycell(f_met) = ycell3d

          IF (INDEX1( 'TSEASFC', NVARS3D, VNAME3D ) .gt. 0) then
             TSEASFC_AVAIL = .true.
             adj = 0
          else
             TSEASFC_AVAIL = .false.
             adj = 1
          end if

          HAS_SEAICE = (INDEX1( 'SEAICE', NVARS3D, VNAME3D ) .gt. 0)

! include an additional variable TSEASFC when MET_CRO_2D does not have it and CMAQ code is looking for it
          n_c2d = nvars3d + adj
          allocate (c2d_name(n_c2d, 3), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating c2d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

! only met data has 'm' distinction and since twoway model does not provide 
! boundary data, so this distinction only apply to non boundary met data

          c2d_name(1:nvars3d,1) = vname3d(1:nvars3d)
          c2d_name(:,2)         = 'mc2'     ! denote 2d variable
          c2d_name(:,3)         = 'm'       ! denote met variable
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
          PXSOIL_AVAIL = (INDEX1( 'CLAY_PX', NVARS3D, VNAME3D ) .gt. 0)

          CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                    STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

#ifdef twoway
          STRTCOLMC2x = STRTCOLMC2
          STRTROWMC2x = STRTROWMC2
          ENDCOLMC2x  = ENDCOLMC2
          ENDROWMC2x  = ENDROWMC2
#else
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
#endif

! met cro 3d file
          IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// MET_CRO_3D // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
             XMSG = 'Could not get ' // MET_CRO_3D //' file description'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          n_c3d = nvars3d
          allocate (c3d_name(n_c3d, 3), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating c3d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          c3d_name(:,1) = vname3d(1:n_c3d) 
          c3d_name(:,2) = 'mc3'  ! denote 3d variable
          c3d_name(:,3) = 'm'    ! denote met variable

          CFRAC_3D_AVAIL = (INDEX1( 'CFRAC_3D', NVARS3D, VNAME3D ) .gt. 0)

          PV_AVAIL       = (INDEX1( 'PV', NVARS3D, VNAME3D ) .gt. 0)
          IF ( .NOT. PV_AVAIL .AND. LPVO3 ) THEN
             XMSG = 'PV, potential vorticity, not found in ' // MET_CRO_3D
     &          //  ' but CTM_PVO3 option set to Yes in run-script'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          QI_AVAIL       = (INDEX1( 'QI', NVARS3D, VNAME3D ) .gt. 0)
          QS_AVAIL       = (INDEX1( 'QS', NVARS3D, VNAME3D ) .gt. 0)
          QG_AVAIL       = (INDEX1( 'QG', NVARS3D, VNAME3D ) .gt. 0)
          JACOBF_AVAIL   = (INDEX1( 'JACOBF', NVARS3D, VNAME3D ) .gt. 0)
          QC_AVAIL       = .true.

          CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                    STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

          IF ( (ENDCOLMC3 - STRTCOLMC3 + 1) .NE. NCOLS .OR.
     &         (ENDROWMC3 - STRTROWMC3 + 1) .NE. NROWS ) THEN
               WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &         (ENDCOLMC3 - STRTCOLMC3 + 1), NCOLS, (ENDROWMC3 - STRTROWMC3 + 1), NROWS
             CALL M3EXIT ( PNAME, cio_model_sdate, cio_model_stime, XMSG, XSTAT1 )
          END IF

#ifdef twoway
          window = .TRUE.

          STRTCOLMC3 = STRTCOLMC3 - 1
          ENDCOLMC3  = ENDCOLMC3 + 1
          STRTROWMC3 = STRTROWMC3 - 1
          ENDROWMC3  = ENDROWMC3 + 1
          w_cro_ncols = ENDCOLMC3 - STRTCOLMC3 + 1
          w_cro_nrows = ENDROWMC3 - STRTROWMC3 + 1

#else
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
#endif

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

          n_d3d = nvars3d
          allocate (d3d_name(n_d3d, 3), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating d3d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          d3d_name(:,1) = vname3d(1:n_d3d) 
          d3d_name(:,2) = 'md3'   ! denote dot variable
          d3d_name(:,3) = 'm'     ! denote met variable

          CALL SUBHFILE ( MET_DOT_3D, GXOFF, GYOFF,
     &                    STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3 )

#ifdef twoway
          STRTCOLMD3x = STRTCOLMD3
          STRTROWMD3x = STRTROWMD3
          ENDROWMD3x  = ENDROWMD3
          ENDCOLMD3x  = ENDCOLMD3
#else
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
#endif

          dot_ncols = ENDCOLMD3 - STRTCOLMD3 + 1
          dot_nrows = ENDROWMD3 - STRTROWMD3 + 1
          size_d3d  = dot_ncols * dot_nrows * nlays

          x_dot_ncols = ENDCOLMD3x - STRTCOLMD3x + 1
          x_dot_nrows = ENDROWMD3x - STRTROWMD3x + 1
          size_d2dx = x_dot_ncols * x_dot_nrows
          size_d3dx = size_d2dx * nlays

          UWINDC_AVAIL = (INDEX1( 'UWINDC', NVARS3D, VNAME3D ) .gt. 0)
          VWINDC_AVAIL = (INDEX1( 'VWINDC', NVARS3D, VNAME3D ) .gt. 0)
#endif

! emission file, could be one or multiple layer

          call desid_read_namelist()
          call desid_init_regions()

          allocate (cio_emis_file_name(N_FILE_GR),
     &              cio_emis_file_loc(N_FILE_GR),
     &              cio_emis_nvars(N_FILE_GR),
#ifdef mpas
     &              num_dist_layers(ncols, n_file_gr),
     &              dist_frac(nlays, ncols, n_file_gr),
     &              bottom(nlays), 
     &              top(nlays), 
     &              emis_file_layer_frac(nlays, n_file_gr), 
#else
     &              cio_emis_file_startcol(N_FILE_GR),
     &              cio_emis_file_endcol(N_FILE_GR),
     &              cio_emis_file_startrow(N_FILE_GR),
     &              cio_emis_file_endrow(N_FILE_GR),
#endif
     &              f_emis(N_FILE_GR),
     &              stat=stat)

          n_e2d = 0
          n_e3d = 0
          do n = 1, N_FILE_GR

             n_opened_file = n_opened_file + 1
             f_emis(n)     = n_opened_file

! Check whether file is a representative day type
             file_sym_date(f_emis(n)) = emis_sym_date ! Master switch to change default
             write (fname, '(a15, i3.3)') "GR_EM_SYM_DATE_", n
             call get_env(file_sym_date(f_emis(n)), fname,
     &                    file_sym_date(f_emis(n)), logdev )

             write (fname, '(a8, i3.3)') "GR_EMIS_", n
             cio_emis_file_name(n) = fname

#ifdef mpas
             floc = mio_search (cio_emis_file_name(n))
             cio_emis_file_loc(n) = floc

             call mio_time_format_conversion (mio_file_data(floc)%timestamp(1), tdate(1), ttime(1))
             call mio_time_format_conversion (mio_file_data(floc)%timestamp(2), tdate(2), ttime(2))

             file_sdate(f_emis(n)) = tdate(1)
             file_stime(f_emis(n)) = ttime(1)

             diffsec = secsdiff (tdate(1), ttime(1), tdate(2), ttime(2))

             file_tstep(f_emis(n)) = sec2time(diffsec)
             mio_file_data(floc)%tstep = file_tstep(f_emis(n))

             layer_exist = .false.
             do v = 1, mio_file_data(floc)%n_global_atts
                if (mio_file_data(floc)%glo_att_name(v) .eq. 'layers') then
                   layer_exist = .true.
                   s = mio_file_data(floc)%glo_att_crange(2*v-1)
                   e = mio_file_data(floc)%glo_att_crange(2*v)
                   call ext_layer_info (mio_file_data(floc)%glo_att_cval(s:e), 
     &                                  emis_file_dist_layer, bottom, top, 
     &                                  emis_file_layer_frac(:,n))
                end if
             end do

             if (layer_exist) then
                do c = 1, ncols
                   call cal_distribution (bottom, top, g3ddata(c,1,:,zh_ind), 
     &                                    emis_file_layer_frac(:,n),
     &                                    emis_file_dist_layer, 
     &                                    num_dist_layers(c,n),
     &                                    dist_frac(:,c,n))
                end do
             else
                num_dist_layers(:,n) = 1
                dist_frac(:,:,n) = 1.0
             end if

             cio_emis_nvars(n) = mio_file_data(floc)%nvars
             if (mio_file_data(floc)%nlays .eq. 1) then
                n_e2d = n_e2d + cio_emis_nvars(n)
             else
                n_e3d = n_e3d + cio_emis_nvars(n)
             end if

             call mio_time_format_conversion (mio_file_data(floc)%timestamp(1), file_sdate(f_emis(n)), time)
#else
             IF ( .NOT. OPEN3( fname, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// fname // ' file'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             IF ( .NOT. DESC3( fname ) ) THEN
                XMSG = 'Could not get description of file  '// fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF

             call subhfile ( cio_emis_file_name(n), gxoff, gyoff,
     &                       cio_emis_file_startcol(n), cio_emis_file_endcol(n), 
     &                       cio_emis_file_startrow(n), cio_emis_file_endrow(n) )

             file_sdate(f_emis(n)) = sdate3d
             file_stime(f_emis(n)) = stime3d
             file_tstep(f_emis(n)) = tstep3d
             file_xcell(f_emis(n)) = xcell3d
             file_ycell(f_emis(n)) = ycell3d

             found = .false.
             ldate = sdate3d
             ltime = stime3d
             if (ldate == stdate) then
                found = .true.
             else
                t = 1
                do while ((t < mxrec3d) .and. (.not. found))
                   call nextime (ldate, ltime, tstep3d)
                   if (ldate == stdate) then
                      found = .true.
                   end if
                   t = t + 1
                end do
             end if

             cio_emis_nvars(n) = nvars3d
             if (nlays3d .eq. 1) then
                n_e2d = n_e2d + cio_emis_nvars(n)
             else
                n_e3d = n_e3d + cio_emis_nvars(n)
             end if

#endif
          end do

#ifdef mpas
          deallocate (bottom, top)

          n_dust_vars       = 0
#else

! Wind blown dust data
          n_dust_vars = 0
#endif

          n_e2d = n_e2d + n_dust_vars

          allocate (emis_name(n_e2d+n_e3d, 3), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating emis_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if

#ifndef mpas
! setup initial condition file
          n_i3d = 0
          IF ( .NOT. OPEN3( ICFILE, FSREAD3, PNAME ) ) THEN
             XMSG = 'Open failure for ' // ICFILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          n_opened_file = n_opened_file + 1
          f_icon = n_opened_file
          IF ( .NOT. DESC3( ICFILE ) ) THEN
             XMSG = 'Could not get description of file  '// ICFILE 
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          call subhfile ( ICFILE, gxoff, gyoff,
     &                    STRTCOLIC, ENDCOLIC, STRTROWIC, ENDROWIC )

! remove duplicate name from MET_CRO_3D file
          adj = nvars3d
          do v = nvars3d, 1, -1
             n = index1 (vname3d(v), n_c3d, c3d_name) 
             if (n .gt. 0) then
                do idx = v+1, adj
                   vname3d(idx-1) = vname3d(idx)
                end do
                adj = adj - 1
             end if
          end do
          n_i3d = adj

          allocate (i3d_name(n_i3d, 3), stat=stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating i3d_name '
             call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          i3d_name(:,1) = vname3d(1:n_i3d) 
          i3d_name(:,2) = 'ic'                  ! denote initial condition variable
          i3d_name(:,3) = ' '                   ! denote non met variable

! setup initial condition file for ISAM
          n_is3d = 0

          if (ISAM_NEW_START == 'N') then
             IF ( .NOT. OPEN3( ISAM_PREVDAY, FSREAD3, PNAME ) ) THEN
                XMSG = 'Open failure for ' // ISAM_PREVDAY
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             n_opened_file = n_opened_file + 1
             f_is_icon = n_opened_file
             IF ( .NOT. DESC3( ISAM_PREVDAY ) ) THEN
                XMSG = 'Could not get description of file  '// ISAM_PREVDAY 
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             call subhfile ( ISAM_PREVDAY, gxoff, gyoff,
     &                       STRTCOLISIC, ENDCOLISIC, STRTROWISIC, ENDROWISIC )

             n_is3d = nvars3d
             allocate (is3d_name(n_is3d, 3), stat=stat)
             if (stat .ne. 0) then
                xmsg = 'Failure allocating i3d_name '
                call m3exit (pname, 0, 0, xmsg, xstat1 )
             end if
             is3d_name(:,1) = vname3d(1:n_is3d) 
             is3d_name(:,2) = 'is'                   ! denote ISAM initial condition variable
             is3d_name(:,3) = ' '                    ! denote non met variable

             file_sdate(f_is_icon) = sdate3d
             file_stime(f_is_icon) = stime3d
             file_tstep(f_is_icon) = tstep3d
             file_xcell(f_is_icon) = xcell3d
             file_ycell(f_is_icon) = ycell3d

          end if   ! ISAM_NEW_START
#endif

! setup gridded emission file
          end = 0
          allocate (cio_emis_file_layer(N_FILE_GR), stat=stat)
          do n = 1, N_FILE_GR
             WRITE (fname, '(a8, i3.3)') "GR_EMIS_", n
#ifdef mpas
             floc = cio_emis_file_loc(n)
             nl = mio_file_data(floc)%nlays
#else
             IF ( .NOT. DESC3( fname ) ) THEN
                XMSG = 'Could not get description of file  '// fname
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             nl = nlays3d
#endif

             begin = end + 1

             write (tname, '(a1, i3.3)') '_', n
#ifdef mpas
             do v = 1, mio_file_data(floc)%nvars
                end = end + 1
                emis_name(end,1) = trim(mio_file_data(floc)%var_name(v)) // tname
             end do

#else
             do v = 1, nvars3d
                end = end + 1
                emis_name(end,1) = trim(vname3d(v)) // tname
             end do
#endif

             if (nl .eq. 1) then
                emis_name(begin:end, 2) = 'e2d'        ! e denote emission 2d variable
             else
                emis_name(begin:end, 2) = 'e3d'        ! E denote emission 3d variable
             end if
             emis_name(begin:end, 3) = ' '             ! denote non met variable
             cio_emis_file_layer(n) = nl
          end do

          cio_emis_nlays = maxval(cio_emis_file_layer)
          ! If there are 3D (inline point or Lightning) sources, 
          ! revise the top to be the model top.
          IF ( NPTGRPS .GT. 0 .OR. LTNG_NO ) cio_emis_nlays = NLAYS
 
          ! Make sure the top is not greater than the model top
          cio_emis_nlays = MAX( MIN( cio_emis_nlays, NLAYS ), 1 )
 
          WRITE( LOGDEV,1009 ) cio_emis_nlays, NLAYS
 1009     FORMAT(    5X, 'Number of Emissions Layers:         ', I3
     &            /  5X, 'out of total Number of Model Layers:', I3 )

! lightning file
          n_l2d = 0
#ifndef mpas
          if (NLDNSTRIKE) then
             IF ( .NOT. OPEN3( NLDN_STRIKES, FSREAD3, PNAME ) ) THEN
                XMSG = 'Open failure for ' // NLDN_STRIKES
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             n_opened_file = n_opened_file + 1
             f_ltng = n_opened_file
             IF ( .NOT. DESC3( NLDN_STRIKES ) ) THEN
                XMSG = 'Could not get description of file  '// NLDN_STRIKES 
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             call subhfile ( NLDN_STRIKES, gxoff, gyoff,
     &                       STRTCOLLNT, ENDCOLLNT, STRTROWLNT, ENDROWLNT)

             file_sdate(f_ltng) = sdate3d
             file_stime(f_ltng) = stime3d
             file_tstep(f_ltng) = tstep3d
             file_xcell(f_ltng) = xcell3d
             file_ycell(f_ltng) = ycell3d

             n_l2d = nvars3d
             cio_LTLYRS = nlays3d
             allocate (l2d_name(n_l2d, 3), stat=stat)
             if (stat .ne. 0) then
                xmsg = 'Failure allocating l2d_name '
                call m3exit (pname, 0, 0, xmsg, xstat1 )
             end if
             l2d_name(:,1) = vname3d(1:n_l2d) 
             l2d_name(:,2) = 'lnt'   ! denote lightning variable
             l2d_name(:,3) = ' '     ! denote non met variable
             ! Check to see what the lightning variable name is called
             ! backwards (NLDNstrk) & forwards compatible (LNT)  
             if ( index1('NLDNstrk', n_l2d ,l2d_name(:,1)) .gt. 0 ) then
                lt_name = 'NLDNstrk'
             else
                lt_name = 'LNT'
             end if

          end if
#endif

! combining all files
          n_cio_grid_vars = n_c2d + n_c3d + n_d3d + n_e2d + n_e3d + n_l2d + n_i3d + n_is3d

#ifdef mpas
          cro_ncols = ncols
          cro_nrows = 1
          size_c2dx = 1

! for standard domain
          s_cro_ncols = ncols
          s_cro_nrows = 1
#else
          cro_ncols = ENDCOLMC2 - STRTCOLMC2 + 1
          cro_nrows = ENDROWMC2 - STRTROWMC2 + 1

! for standard domain
          STRTCOLSTD = COLSX_PE( 1, MYPE+1 )
          ENDCOLSTD  = COLSX_PE( 2, MYPE+1 )
          STRTROWSTD = ROWSX_PE( 1, MYPE+1 )
          ENDROWSTD  = ROWSX_PE( 2, MYPE+1 )

          s_cro_ncols = ENDCOLSTD - STRTCOLSTD + 1
          s_cro_nrows = ENDROWSTD - STRTROWSTD + 1
#endif
          size_c2d = cro_ncols * cro_nrows

          size_s2d = s_cro_ncols * s_cro_nrows

          if ((cro_ncols .ne. ncols) .or. (cro_nrows .ne. nrows)) then
             call m3exit( 'Centralized I/O',0,0,' ==d== NO ncols nrows ',1 )
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

          size_e3d = size_s2d * cio_emis_nlays
          size_s3d = size_s2d * nlays

          size_lt = size_s2d * cio_LTLYRS

          allocate (cio_grid_var_name(n_cio_grid_vars, 3),
     &              cio_grid_data_inx(2, 0:2, n_cio_grid_vars),
     &              head_grid(n_cio_grid_vars),
     &              tail_grid(n_cio_grid_vars),
     &              cio_grid_data_tstamp(2, 0:2, n_cio_grid_vars),
     &              cio_grid_data(  size_c2dx * 3 * n_c2d                ! 2d met data
     &                            + size_c2d  * 3 * n_e2d                ! 2d emis data
     &                            + size_c3d  * 3 * n_c3d                ! 3D met data
     &                            + size_e3d  * 3 * n_e3d                ! 3d emis data 
     &                            + size_s3d  * 3 * n_i3d                ! 3d initial condition data 
     &                            + size_s3d  * 3 * n_is3d               ! 3d ISAM initial condition data 
     &                            + size_d3dx * 3 * n_d3d                ! 3d dot data
     &                            + size_lt   * 3 * n_l2d),              ! lightning data
     &              stat = stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating cio_grid_var_name and associated arrays '
               call m3exit (pname, 0, 0, xmsg, xstat1 )
          end if
          cio_grid_data = 0.0

#ifdef mpas
          end = 0
          allocate (cio_mpas_grid_data_tstamp(0:2, n_cio_grid_vars),
     &              stat = stat)

#else
          begin = 1
          end = n_c2d
          cio_grid_var_name(begin:end, :) = c2d_name

          begin = end + 1
          end = end + n_c3d
          cio_grid_var_name(begin:end, :) = c3d_name

          begin = end + 1
          end = end + n_d3d
          cio_grid_var_name(begin:end, :) = d3d_name
#endif

          begin = end + 1
          end = end + n_e2d + n_e3d
          cio_grid_var_name(begin:end, :) = emis_name

#ifndef mpas
          begin = end + 1
          end = end + n_i3d
          cio_grid_var_name(begin:end, :) = i3d_name

          if (ISAM_NEW_START == 'N') then
             begin = end + 1
             end = end + n_is3d
             cio_grid_var_name(begin:end, :) = is3d_name
          end if

          if (NLDNSTRIKE) then
             begin = end + 1
             end = end + n_l2d
             cio_grid_var_name(begin:end, :) = l2d_name
             deallocate (l2d_name)
          end if

          deallocate (c2d_name, c3d_name, i3d_name)
          if (ISAM_NEW_START == 'N') then
             deallocate (is3d_name)
          end if
          if (.not. window) then
             deallocate (d3d_name)
          end if
#endif
          deallocate (emis_name)

          call quicksort(cio_grid_var_name, 1, n_cio_grid_vars)

          begin = 1
          do v = 1, n_cio_grid_vars

! locate certain species
             if (cio_grid_var_name(v,1) .eq. 'TEMPG') then
                tempg_loc = v
             else if (cio_grid_var_name(v,1) .eq. 'TSEASFC') then
                tseasfc_loc = v
             end if

             if (cio_grid_var_name(v,2) .eq. 'mc2')  then
                d_size = size_c2dx
             else if (cio_grid_var_name(v,2) .eq. 'e2d') then
                d_size = size_s2d
             else if (cio_grid_var_name(v,2) .eq. 'mc3') then
                d_size = size_c3d
             else if (cio_grid_var_name(v,2) .eq. 'e3d') then
                d_size = size_e3d
             else if ((cio_grid_var_name(v,2) .eq. 'ic') .or.
     &                (cio_grid_var_name(v,2) .eq. 'is')) then
                d_size = size_s3d
             else if (cio_grid_var_name(v,2) .eq. 'md3') then
                d_size = size_d3dx
             else if ((cio_grid_var_name(v,2) .eq. 'lnt') .or.
     &                (cio_grid_var_name(v,2) .eq. 'wb')) then
                d_size = size_s2d
             else
                call m3exit( 'Centralized I/O',0,0,' ==d== UNKOWN',1 )
             end if

             do n = 0, 2
                cio_grid_data_inx(1, n, v) = begin
                end = begin + d_size - 1
                cio_grid_data_inx(2, n, v) = end
                begin = end + 1
             end do
! this is for checking purposes
!            write (logdev, '(a12, i5, 1x, a16, 2a4, 10i10)') ' ==d== file ', v,
!    &                cio_grid_var_name(v,:), cio_grid_data_inx(:,:,v)
          end do

        end subroutine gridded_files_setup

! -------------------------------------------------------------------------
        subroutine retrieve_time_dep_gridded_data (jdate, jtime, vname)

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE VGRD_DEFN, ONLY : NLAYS
          USE CGRID_SPCS
          use get_env_module
#ifdef mpas
          use mio_module
          use util_module, only : nextime
          use coupler_module, only : cell_area
!         use centralized_io_util_module, only : binary_search
!#else
!         use centralized_io_util_module, only : binary_search
#endif
          use centralized_io_util_module, only : binary_search

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: vname

          Character( 40 ), parameter :: pname = 'retrieve_time_dep_gridded_data'

          LOGICAL, SAVE :: firstime = .true.
          INTEGER :: STAT, i, j, k, begin, end, buf_loc, iterations, iter,
     &               data_jdate, data_jtime,
     &               v, beg_v, end_v, fnum, str_len, 
     &               t_beg, t_end, floc
          integer, allocatable :: tdata(:), loc_jdate(:), loc_jtime(:)
          character (16) :: loc_vname
          character (20) :: fname
          character (20), allocatable, save :: mpas_loc_time_stamp(:)

          CHARACTER( 120 ) :: XMSG = ' '

#ifdef mpas
          real, save, allocatable :: mpas_tdata(:,:), temp_data_1d(:), temp_data_2d(:,:)
          character (20) :: loc_mpas_time_stamp                           ! this is for mpas only
          character (20), save :: mpas_time_stamp                         ! this is for mpas only
          integer, save :: pre_jdate, pre_jtime                           ! this is fore mpas only
          character( 40 ), save :: exception1, exception2
#endif

          allocate (loc_jdate(n_opened_file), loc_jtime(n_opened_file), STAT=STAT)

          if (firstime) then

#ifdef mpas
             allocate (mpas_loc_time_stamp(n_opened_file), STAT=STAT)

             do k = 1, N_FILE_GR
                write (fname, '(a8, i3.3)') "GR_EMIS_", k
                i = mio_search (fname)
                mpas_loc_time_stamp(f_emis(k)) = mio_file_data(i)%timestamp(1)
             end do

             pre_jdate = -1
             pre_jtime = -1

             call get_env (exception1, 'exception1', ' ')
             call get_env (exception2, 'exception2', ' ')

#else
             allocate (SOILCAT_A(ncols, nrows), STAT=STAT)

             IF ( STAT .NE. 0 ) THEN
                XMSG = 'Failure allocating SLTYP array'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
             END IF
#ifdef twoway
             If ( .Not. INTERPX( MET_CRO_2D, 'SLTYP', PNAME, 
     &                           STRTCOLMC2, ENDCOLMC2,STRTROWMC2, ENDROWMC2, 1, 1,
     &                           jdate, jtime, SOILCAT_A ) ) THEN
                XMSG = ' Error interpolating variable SLTYP from ' // MET_CRO_2D
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
#else
             If ( .Not. XTRACT3( MET_CRO_2D, 'SLTYP',
     &                           1, 1, STRTROWMC2, ENDROWMC2, STRTCOLMC2, ENDCOLMC2, 
     &                           jdate, jtime, SOILCAT_A ) ) THEN
                XMSG = ' Error interpolating variable SLTYP from ' // MET_CRO_2D
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
#endif

#endif
             head_grid = -1
             tail_grid = -1

             iterations = 2
          else
             iterations = 1
          end if  ! firstime

          if (present(vname)) then
             beg_v = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
             end_v = beg_v
          else
             beg_v = 1
             end_v = n_cio_grid_vars
          end if

          loc_jdate = jdate
          loc_jtime = jtime

          do iter = 1, iterations
             do v = beg_v, end_v
                buf_loc = mod((tail_grid(v) + iter), 2)

                begin = cio_grid_data_inx(1,buf_loc,v)
                end   = cio_grid_data_inx(2,buf_loc,v)

                if (cio_grid_var_name(v,2) == 'mc2') then

#ifndef mpas
                   data_jdate = loc_jdate(f_met)
                   data_jtime = loc_jtime(f_met)

                   if ((cio_grid_var_name(v,1) .ne. 'TSEASFC') .or.  TSEASFC_AVAIL) then
#ifdef twoway
                      IF ( .NOT. INTERPX( MET_CRO_2D, cio_grid_var_name(v,1), PNAME, 
     &                                    STRTCOLMC2x, ENDCOLMC2x, STRTROWMC2x, ENDROWMC2x, 1, 1,
     &                                    data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                         XMSG = 'Could not extract ' // MET_CRO_2D // ' file'
                         CALL M3EXIT ( PNAME, DATA_JDATE, DATA_JTIME, XMSG, XSTAT1 )
                      END IF
#else
                      IF ( .NOT. XTRACT3( MET_CRO_2D, cio_grid_var_name(v,1), 
     &                                    1, 1, STRTROWMC2x, ENDROWMC2x, STRTCOLMC2x, ENDCOLMC2x, 
     &                                    data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                         XMSG = 'Could not extract ' // MET_CRO_2D // ' file'
                         CALL M3EXIT ( PNAME, DATA_JDATE, DATA_JTIME, XMSG, XSTAT1 )
                      END IF
#endif
                   END IF

! deal with convective scheme
                   if ((cio_grid_var_name(v,1) .eq. 'RC') .or.
     &                 (cio_grid_var_name(v,1) .eq. 'RCA')) then
                      if (maxval(cio_grid_data(begin:end)) .lt. 0.0) then
                         convective_scheme = .false.
                         cio_grid_data(begin:end) = 0.0
                         XMSG = 'MCIP files indicate no convective parameterization was '
     &                          // 'used in the WRF simulation'
                         CALL M3WARN (PNAME, JDATE, JTIME, XMSG)
                         XMSG = 'Processing will continue without subgrid clouds'
                         CALL M3MESG (XMSG)
                      else
                         where (cio_grid_data(begin:end) .lt. 0.0) cio_grid_data(begin:end) = 0.0
                      end if
                   end if

                else if (cio_grid_var_name(v,2) == 'mc3') then

                   data_jdate = loc_jdate(f_met)
                   data_jtime = loc_jtime(f_met)
#ifdef twoway
                   IF ( .NOT. INTERPX( MET_CRO_3D, cio_grid_var_name(v,1), PNAME, 
     &                                 STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3, 1, nlays, 
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // MET_CRO_3D // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF
#else
                   IF ( .NOT. XTRACT3( MET_CRO_3D, cio_grid_var_name(v,1), 
     &                                 1, nlays, STRTROWMC3, ENDROWMC3, STRTCOLMC3, ENDCOLMC3, 
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // MET_CRO_3D // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF
#endif
                else if (cio_grid_var_name(v,2) == 'md3') then

                   data_jdate = loc_jdate(f_met)
                   data_jtime = loc_jtime(f_met)
#ifdef twoway
                   IF ( .NOT. INTERPX( MET_DOT_3D, cio_grid_var_name(v,1), PNAME,
     &                                 STRTCOLMD3x, ENDCOLMD3x, STRTROWMD3x, ENDROWMD3x, 1, nlays, 
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // MET_DOT_3D // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF
#else
                   IF ( .NOT. XTRACT3( MET_DOT_3D, cio_grid_var_name(v,1), 
     &                                 1, nlays, STRTROWMD3x, ENDROWMD3x, STRTCOLMD3x, ENDCOLMD3x, 
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // MET_DOT_3D // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF
#endif
#endif
                else if (cio_grid_var_name(v,2) == 'e2d') then

                   str_len = len_trim(cio_grid_var_name(v,1))
                   read (cio_grid_var_name(v,1)(str_len-2:str_len), *) fnum
                   loc_vname = cio_grid_var_name(v,1)(1:str_len-4)

#ifdef mpas
                   floc = cio_emis_file_loc(fnum)

                   if (.not. allocated(mpas_tdata)) then
                      allocate (mpas_tdata(ncols, nlays), 
     &                          temp_data_1d(ncols),
     &                          stat=stat)
                   end if

                   loc_mpas_time_stamp = mpas_loc_time_stamp(f_emis(fnum))

                   call mio_fread (cio_emis_file_name(fnum),
     &                             loc_vname,
     &                             pname,
     &                             mpas_tdata,
     &                             loc_mpas_time_stamp)

! de-normalized the data 
                   mpas_tdata = 0.0
                   if ((cio_emis_file_name(fnum) .eq. exception1) .or. 
     &             (cio_emis_file_name(fnum) .eq.  exception2)) then
                      do i = 1, ncols
                         do k = 1, num_dist_layers(i,fnum)
                            mpas_tdata(i,k) = temp_data_1d(i) * dist_frac(k,i,fnum)
                         end do
                      end do
                   else
                      do i = 1, ncols
                         do k = 1, num_dist_layers(i,fnum)
                            mpas_tdata(i,k) = temp_data_1d(i) * dist_frac(k,i,fnum) !* cell_area(i,1)
                         end do
                      end do
                   end if

!                  do i = 1, ncols
!                     mpas_tdata(i,:) = mpas_tdata(i,:) * cell_area(i,1)
!                  end do

                   cio_grid_data(begin:end) = reshape(mpas_tdata, (/ end-begin+1 /))

#else

                   ! Check if its a representative day on start-up (every other time it will
                   ! be managed by the emissions processing) 
                   if (firstime) then  
                     if (file_sym_date(f_emis(fnum))) loc_jdate(f_emis(fnum)) = file_sdate(f_emis(fnum))
                   end if
                   data_jdate = loc_jdate(f_emis(fnum))
                   data_jtime = loc_jtime(f_emis(fnum))

                   IF ( .NOT. XTRACT3( cio_emis_file_name(fnum), loc_vname, 1, 1,
     &                                 cio_emis_file_startrow(fnum), cio_emis_file_endrow(fnum),
     &                                 cio_emis_file_startcol(fnum), cio_emis_file_endcol(fnum),
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // cio_emis_file_name(fnum) // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF
#endif

                else if (cio_grid_var_name(v,2) == 'e3d') then

                   str_len = len_trim(cio_grid_var_name(v,1))
                   read (cio_grid_var_name(v,1)(str_len-2:str_len), *) fnum
                   loc_vname = cio_grid_var_name(v,1)(1:str_len-4)

#ifdef mpas
                   floc = cio_emis_file_loc(fnum)

                   if (.not. allocated(mpas_tdata)) then
                      allocate (mpas_tdata(ncols, nlays), 
     &                          temp_data_1d(ncols),
     &                          stat=stat)
                   end if

                   file_tstep(f_emis(fnum)) = mio_file_data(floc)%tstep

                   ! Check if its a representative day on start-up (every other
                   ! time it will be managed by the emissions processing)
                   if (firstime) then
                     if (file_sym_date(f_emis(fnum))) loc_jdate(f_emis(fnum)) = file_sdate(f_emis(fnum))
                   end if
                   data_jdate = loc_jdate(f_emis(fnum))
                   data_jtime = loc_jtime(f_emis(fnum))

                   call mio_time_format_conversion (data_jdate, data_jtime, loc_mpas_time_stamp)

                   call mio_fread (cio_emis_file_name(fnum),
     &                             loc_vname,
     &                             pname,
     &                             temp_data_1d,
     &                             loc_mpas_time_stamp)

                   cio_mpas_grid_data_tstamp(buf_loc, v) = loc_mpas_time_stamp

                   call mio_time_format_conversion (loc_mpas_time_stamp, data_jdate, data_jtime)

! de-normalized the data 
                   mpas_tdata = 0.0
                   do i = 1, ncols
                      do k = 1, num_dist_layers(i,fnum)
!                        mpas_tdata(i,k) = temp_data_1d(i) * dist_frac(k,i,fnum)
                         mpas_tdata(i,k) = temp_data_1d(i) * dist_frac(k,i,fnum) * cell_area(i,1)
                      end do
                   end do

                   t_beg = begin
                   t_end = begin + ncols - 1
                   do k = 1, nlays
                      cio_grid_data(t_beg:t_end) = mpas_tdata(:,k)
                      t_beg = t_end + 1
                      t_end = t_end + ncols
                   end do

#else
                   ! Check if its a representative day on start-up (every other time it will
                   ! be managed by the emissions processing) 
                   if (firstime) then  
                     if (file_sym_date(f_emis(fnum))) loc_jdate(f_emis(fnum)) = file_sdate(f_emis(fnum))
                   end if
                   data_jdate = loc_jdate(f_emis(fnum))
                   data_jtime = loc_jtime(f_emis(fnum))

                   IF ( .NOT. XTRACT3( cio_emis_file_name(fnum), loc_vname,
     &                                 1, cio_emis_file_layer(fnum), 
     &                                 cio_emis_file_startrow(fnum), cio_emis_file_endrow(fnum),
     &                                 cio_emis_file_startcol(fnum), cio_emis_file_endcol(fnum),
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // cio_emis_file_name(fnum) // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF

#endif

                else if (cio_grid_var_name(v,2) == 'ic') then

#ifndef mpas
                      data_jdate = loc_jdate(f_icon)
                      data_jtime = loc_jtime(f_icon)

                   if (iter == 1) then
                   
                      IF ( .NOT. XTRACT3( ICFILE, cio_grid_var_name(v,1),
     &                                    1, nlays, STRTROWIC, ENDROWIC, STRTCOLIC, ENDCOLIC, 
     &                                    data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                         XMSG = 'Could not extract ' // ICFILE // ' file'
                         CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                      END IF
                   end if
#endif

                else if (cio_grid_var_name(v,2) == 'is') then

#ifndef mpas
                      data_jdate = loc_jdate(f_is_icon)
                      data_jtime = loc_jtime(f_is_icon)

                   if ((iter == 1) .and. (ISAM_NEW_START == 'N')) then
                      
                      IF ( .NOT. XTRACT3( ISAM_PREVDAY, cio_grid_var_name(v,1),
     &                                    1, nlays, STRTROWISIC, ENDROWISIC, STRTCOLISIC, ENDCOLISIC, 
     &                                    data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                         XMSG = 'Could not extract ' // ISAM_PREVDAY // ' file'
                         CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                      END IF
                   end if
#endif

                else if (cio_grid_var_name(v,2) == 'lnt') then

#ifndef mpas
                   data_jdate = loc_jdate(f_ltng)
                   data_jtime = loc_jtime(f_ltng)

                   IF ( .NOT. XTRACT3( NLDN_STRIKES, cio_grid_var_name(v,1), 
     &                                 1, cio_LTLYRS, STRTROWLNT, ENDROWLNT, STRTCOLLNT, ENDCOLLNT, 
     &                                 data_jdate, data_jtime, cio_grid_data(begin:end) ) ) THEN
                      XMSG = 'Could not extract ' // NLDN_STRIKES // ' file'
                      CALL M3EXIT ( PNAME, data_jdate, data_jtime, XMSG, XSTAT1 )
                   END IF
#endif

                else
                   CALL M3EXIT( 'Centralized I/O',0,0,' ==d== UNKOWN Type of File',1 )
                end if

                cio_grid_data_tstamp(1, buf_loc, v) = data_jdate
                cio_grid_data_tstamp(2, buf_loc, v) = data_jtime

             end do

#ifndef mpas
! assign TEMPG to TSEASFC when TSEASFC is not available in the input file
             if (.not. TSEASFC_AVAIL) then
                begin = cio_grid_data_inx(1,buf_loc,tempg_loc)
                end   = cio_grid_data_inx(2,buf_loc,tempg_loc)
                i     = cio_grid_data_inx(1,buf_loc,tseasfc_loc)
                j     = cio_grid_data_inx(2,buf_loc,tseasfc_loc)
                cio_grid_data(i:j) = cio_grid_data(begin:end)
             end if

             CALL NEXTIME ( loc_jdate(f_met), loc_jtime(f_met), file_tstep(f_met) )
             if (NLDNSTRIKE) then
                 CALL NEXTIME ( loc_jdate(f_ltng), loc_jtime(f_ltng), file_tstep(f_ltng) )
             end if
             CALL NEXTIME ( loc_jdate(f_bcon), loc_jtime(f_bcon), file_tstep(f_bcon) )
#endif

             do i = 1, N_FILE_GR
                CALL NEXTIME ( loc_jdate(f_emis(i)), loc_jtime(f_emis(i)), file_tstep(f_emis(i)) )
             end do

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

#ifdef mpas
          pre_jdate = jdate
          pre_jtime = jtime
#endif
          deallocate (loc_jdate, loc_jtime)

        end subroutine retrieve_time_dep_gridded_data

! -------------------------------------------------------------------------
        subroutine retrieve_lufrac_cro_data

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE LSM_Mod, ONLY: n_lufrac, init_lsm

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_lufrac_cro_data'
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

          CHARACTER( 120 ) :: XMSG = ' '
          INTEGER          :: STAT, n, c

          CALL INIT_LSM( 0, 0 )

          allocate (LUFRAC(ncols, nrows, n_lufrac), STAT=STAT)
          IF ( STAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LUFRAC array'
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

#ifdef mpas
          do n = 1, n_lufrac
             do c = 1, ncols
                lufrac(c,1,n) = lufrac_data(n,c)
             end do
          end do
#else
          CALL SUBHFILE ( LUFRAC_CRO, GXOFF, GYOFF,
     &                    startcol, endcol, startrow, endrow )

          IF ( .Not. XTRACT3( LUFRAC_CRO, 'LUFRAC',
     &                        1, n_lufrac, startrow, endrow, startcol, endcol,
     &                        0, 0, LUFRAC ) ) THEN
             XMSG = 'Error interpolating variable LUFRAC from ' // LUFRAC_CRO
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
#endif

        end subroutine retrieve_lufrac_cro_data

!-----------------------------------------------------------------------
      SUBROUTINE DESID_INIT_REGIONS( )
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
      USE desid_param_module
      USE UTIL_FAMILY_MODULE
      USE UTILIO_DEFN
#ifdef mpas
      USE util_module, only : index1, upcase
#else
#ifdef parallel
      USE SE_MODULES   ! stenex (using SE_UTIL_MODULE,SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES ! stenex (using NOOP_UTIL_MODULE,NOOP_DATA_COPY_MODULE)
#endif
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY: NP, NPMAX, S_NRGN, S_RGNLBL, IREGION
#endif

      IMPLICIT NONE
                                                     
      TYPE( DESID_REG_TYPE) :: DESID_REG_READ( DESID_MAX_REG )
      INTEGER, PARAMETER :: NFILE0 = 200
      CHARACTER( 32 )    :: FILENAMES( NFILE0 ) = ''
      CHARACTER( 32 ) :: VNAME

      INTEGER :: IRGN, NFILE, IDX, IFILE, IREAD, IVAR, IFAM, JRGN
      INTEGER :: GXOFF, GYOFF, ENDCOL, ENDROW, STARTCOL, STARTROW
      INTEGER :: N_REG_RULE
      CHARACTER( 16 )    :: PNAME = "DESID_INIT_REGIONS"
      CHARACTER( 250)    :: XMSG
      REAL, ALLOCATABLE  :: REG_FACI(:,:), REG_FACJ(:,:)
      integer :: ldate, ltime, floc
      CHARACTER( 16 ) :: lvname

      ! Find the total number of regions to be processed
      N_REG_RULE = 0  ! The first region is the entire domain
      DO IRGN = 1,DESID_MAX_REG
          IF ( DESID_REG_NML( IRGN )%LABEL .EQ. '' ) EXIT
          N_REG_RULE = N_REG_RULE + 1
      END DO

      ! Allocate Vectors and Arrays for Each Region
      ALLOCATE( DESID_REG( DESID_MAX_REG ) )
      DESID_REG( 1 )%LABEL = 'EVERYWHERE'
      DESID_REG( 1 )%FILE  = 'N/A'
      DESID_REG( 1 )%VAR   = 'N/A'
      DESID_REG( 1 )%FILENUM = 1
      DESID_N_REG = 1
      
      ALLOCATE( DESID_REG_FAC( NCOLS,NROWS,DESID_MAX_REG ) )
      DESID_REG_FAC = 0.0
      DESID_REG_FAC( :,:,1 ) = 1.0

      ! Populate global Region properties structure. Also assign each
      ! region a number according to the file it comes from. 1 =
      ! domain-wide.
      NFILE = 1
      FILENAMES( 1 ) = 'N/A'

      IF ( N_REG_RULE .GT. 0 ) THEN
        DO IREAD = 1,N_REG_RULE
           CALL UPCASE( DESID_REG_NML( IREAD )%LABEL )
           CALL UPCASE( DESID_REG_NML( IREAD )%FILE  )
           CALL UPCASE( DESID_REG_NML( IREAD )%VAR   )

           DESID_REG_READ( IREAD )%LABEL = DESID_REG_NML( IREAD )%LABEL  ! Region Name
           DESID_REG_READ( IREAD )%FILE  = DESID_REG_NML( IREAD )%FILE   ! Logical filename
           DESID_REG_READ( IREAD )%VAR   = DESID_REG_NML( IREAD )%VAR    ! Variable from file 
                                                                         !   used to inform mask

           IDX = INDEX1( DESID_REG_READ( IREAD )%FILE, NFILE, FILENAMES(1:NFILE) )
           IF ( IDX .NE. 0 ) THEN
             DESID_REG_READ( IREAD )%FILENUM = IDX
           ELSE
             NFILE = NFILE + 1
             DESID_REG_READ( IREAD )%FILENUM = NFILE
             FILENAMES( NFILE ) = DESID_REG_READ( IREAD )%FILE
           END IF                                                           
        END DO
      
        ! Process each region by looping through the pertinent files, 
        ! look up maps and save the data in a global array
        DO IFILE = 1,NFILE
          IF ( FILENAMES( IFILE ) .EQ. 'N/A' ) CYCLE
         
#ifdef mpas
          floc = mio_search (filenames( ifile ))

          ldate = 0
          ltime = 0
#else
          ! Get domain decomp info from the emissions file
          CALL SUBHFILE ( FILENAMES( IFILE ), GXOFF, GYOFF,
     &                    STARTCOL, ENDCOL, STARTROW, ENDROW )
        
          ! Open input file
          IF ( .NOT. OPEN3( FILENAMES( IFILE ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open '// FILENAMES( IFILE ) // ' file'
             CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF
          n_opened_file = n_opened_file + 1
         
          ! Retrieve File Header Information
          IF ( .NOT. DESC3( FILENAMES( IFILE ) ) ) THEN
              XMSG = 'Could not get ' // FILENAMES( IFILE ) // ' file description'
             CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF
         
#endif

          ! Read data from regions file into region array 
          DO IREAD = 1,N_REG_RULE
              IF ( DESID_REG_READ( IREAD )%FILENUM .EQ. IFILE ) THEN
                  IF ( DESID_REG_READ( IREAD )%VAR .EQ. 'ALL' ) THEN
                     ! Populate the region array with all of the
                     ! variables on this file
                     IF ( DESID_REG_READ( IREAD )%LABEL .NE. 'ALL' ) THEN
                        XMSG = 'Error reading Region input in Emissions Control file.'//
     &                         'If the variable name is set to "ALL", then the label must'//
     &                         'also be set to "ALL".'
                        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
                     ELSE   
#ifdef mpas
                        DO IVAR = 1, cio_emis_nvars(ifile)
                           lvname = mio_file_data(floc)%var_name(ivar)
#else
                        DO IVAR = 1,NVARS3D
                           lvname = vname3d( ivar )
#endif
                           DESID_N_REG = DESID_N_REG + 1
                           DESID_REG( DESID_N_REG )%LABEL   = lvname
                           DESID_REG( DESID_N_REG )%VAR     = lvname
                           DESID_REG( DESID_N_REG )%FILE    = DESID_REG_READ( IREAD )%FILE
                           DESID_REG( DESID_N_REG )%FILENUM = DESID_REG_READ( IREAD )%FILENUM

#ifdef mpas
                           call mio_fread (FILENAMES(IFILE), lvname, pname, DESID_REG_FAC(:,1,DESID_N_REG))
#else
                           IF ( .NOT. XTRACT3( FILENAMES( IFILE ), VNAME3D(IVAR), 1, 1,
     &                                         STARTROW, ENDROW, STARTCOL, ENDCOL,
     &                                         0, 0, DESID_REG_FAC( 1,1,DESID_N_REG ) ) ) Then
                              XMSG = 'Could not read ' // VNAME3D(IVAR) //
     &                               'from file ' // FILENAMES( IFILE ) 
                              CALL M3WARN ( PNAME, 0, 0, XMSG )
                           End If
#endif

                        END DO
                     END IF
                  ELSE
                     ! Populate the region array with only this variable
                     DESID_N_REG = DESID_N_REG + 1
                     DESID_REG( DESID_N_REG ) = DESID_REG_READ( IREAD )
                     VNAME = DESID_REG_READ( IREAD )%VAR

#ifdef mpas
                     call mio_fread (FILENAMES(IFILE), VNAME, pname, DESID_REG_FAC(:,1,DESID_N_REG))
#else
                     IF ( .NOT. XTRACT3( FILENAMES( IFILE ), VNAME, 1, 1,
     &                                   STARTROW, ENDROW, STARTCOL, ENDCOL,
     &                                   0, 0, DESID_REG_FAC( 1,1,DESID_N_REG ) ) ) Then
                        XMSG = 'Could not read ' // VNAME //
     &                         'from file ' // FILENAMES( IFILE ) 
                        CALL M3WARN ( PNAME, 0, 0, XMSG )
                     End If
#endif

                  END IF
              END IF
          END DO

#ifndef mpas
          ! Close Regions File
          IF ( .NOT. CLOSE3( FILENAMES( IFILE ) ) ) THEN
            XMSG = 'Could not close ' // FILENAMES( IFILE )
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF
#endif
         
          ! Error Check the Regions Array
          ! Any Negatives?
          DO IRGN = 1,DESID_N_REG
            IF ( ANY( DESID_REG_FAC( :,:,IRGN ) .LT. 0.0 ) ) THEN
               XMSG = 'Region ' // TRIM( DESID_REG( IRGN )%LABEL) // ' on file ' //
     &                TRIM( FILENAMES( IFILE )) // ' contains a ' //
     &                'negative value. The expected range is 0-1.'
               CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, 1 )
            ELSE IF ( ANY( DESID_REG_FAC( :,:,IRGN ) .GT. 1.01 ) ) THEN
               XMSG = 'Region ' // TRIM( DESID_REG( IRGN )%LABEL) // ' on file ' //
     &                TRIM( FILENAMES( IFILE )) // ' contains a ' //
     &                'value greater than 1. The expected range is 0-1.'
               CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, 1 )
            END IF
          
            ! Condition mask values to be at most 1.0
            DESID_REG_FAC( :,:,IRGN ) = MIN( 1.0, DESID_REG_FAC( :,:,IRGN ) )

          END DO
          
        END DO ! IFILE
 
        ! Augment Emission Region Structure with Region Families
        DO IFAM = 1,Desid_N_Reg_Fams
           DESID_N_REG = DESID_N_REG + 1
           CALL UPCASE( RegionFamilyName( IFAM ) )
           DESID_REG( DESID_N_REG )%LABEL   = RegionFamilyName( IFAM )
           DESID_REG( DESID_N_REG )%VAR     = 'Family'
           DESID_REG( DESID_N_REG )%FILE    = 'Family'
           DESID_REG( DESID_N_REG )%FILENUM = 0

           DO IRGN = 1,RegionFamilyNum( IFAM )
              CALL UPCASE( RegionFamilyMembers( IFAM,IRGN ) )
              JRGN = INDEX1( RegionFamilyMembers( IFAM,IRGN ), DESID_N_REG-1,
     &                      DESID_REG( 1:(DESID_N_REG-1) )%VAR )
              IF ( JRGN .GT. 0 ) 
     &             DESID_REG_FAC( :,:,DESID_N_REG ) = 
     &                MIN( 1.0, DESID_REG_FAC( :,:,DESID_N_REG ) + 
     &                          DESID_REG_FAC( :,:,JRGN ) )
           END DO
        END DO
      END IF

      DESID_REG = DESID_REG( 1:DESID_N_REG )
      DESID_REG_FAC = DESID_REG_FAC( :,:,1:DESID_N_REG )

      ! Determine Which Regions are Subsets of Larger Regions and
      ! save special relationship for use in EMISS_SCALING.
      ALLOCATE( DESID_REG_SUB( DESID_N_REG, DESID_N_REG ) )
      DESID_REG_SUB(:,:) = .FALSE.  ! Initialize with no region subsets
      DESID_REG_SUB(1,:) = .TRUE.   ! All regions are a subset of Region 1 (Everywhere)
      DESID_REG_SUB(1,1) = .FALSE.  ! No regions are subsets of themselves

      ALLOCATE( REG_FACI(GL_NCOLS,GL_NROWS), 
     &          REG_FACJ(GL_NCOLS,GL_NROWS)  )

      DO IRGN = 2,DESID_N_REG
#ifndef mpas
#ifdef parallel
        CALL SUBST_GLOBAL_GATHER( DESID_REG_FAC(:,:,IRGN), REG_FACI )
#else  
        REG_FACI = DESID_REG_FAC(:,:,IRGN)
#endif
#endif
        DO JRGN = 1,DESID_N_REG
#ifndef mpas
#ifdef parallel
          CALL SUBST_GLOBAL_GATHER( DESID_REG_FAC(:,:,JRGN), REG_FACJ )
#else      
          REG_FACJ = DESID_REG_FAC(:,:,JRGN)
#endif
#endif
          IF ( MYPE .EQ. 0 ) THEN
             IF (  JRGN .NE. IRGN .AND.
     &                ANY( REG_FACJ(:,:) .GT. 0. ) .AND.
     &                ALL( REG_FACI(:,:)+1.0E-6 .GT.
     &                     REG_FACJ(:,:) ) ) THEN
               ! Assume JRGN is a subset of IRGN. Both have to be
               ! non-zero somewhere in the domain.
               DESID_REG_SUB( IRGN,JRGN ) = .TRUE.
             END IF
          END IF  ! Only perform algorithm on main processor
        END DO
      END DO

      DEALLOCATE( REG_FACI, REG_FACJ )

#ifndef mpas
#ifdef parallel
      CALL SUBST_GLOBAL_BCAST( DESID_REG_SUB )
#endif
#endif

#ifdef sens
! Populate IREGION(NCOLS,NROW,NLAYS,NPMAX) with regions data if specified
!'

      DO NP = 1, NPMAX
        IF ( S_NRGN( NP ) .GT. 0 .AND. S_NRGN( NP ) .LT. 99 ) THEN !  
          DO IRGN = 1, S_NRGN( NP )
            IREAD = INDEX1( S_RGNLBL(NP,IRGN), DESID_N_REG, DESID_REG%LABEL ) ! identify region
            IF ( IREAD .EQ. 0 ) THEN
              XMSG = " User specified DDM3D region - " //
     &               TRIM( S_RGNLBL(NP,IRGN) ) //
     &               " - not found in available emissions regions. " //
     &               " Check sensinput.dat file "
              WRITE(LOGDEV,*) " Available region definitions: "
              DO IFILE = 1, DESID_N_REG
                WRITE(LOGDEV,*) IFILE, DESID_REG( IFILE )%LABEL
              END DO
              CALL M3EXIT( PNAME, 1, 1, XMSG, XSTAT1 )
            ELSE
              IREGION(:,:,1,NP) = IREGION( :,:,1,NP )
     &                          + DESID_REG_FAC( :,:,IREAD )
            END IF
          END DO
! Limit IREGION to < 1.0 incase some regions overlap.
          IREGION(:,:,:,NP) = MIN ( IREGION(:,:,:,NP), 1.0 )
! Copy up to layers above
          DO IFILE = 1, NLAYS
            IREGION(:,:,IFILE,NP) = IREGION(:,:,1,NP)
          END DO
        END IF
      END DO

#endif
      
      END SUBROUTINE DESID_INIT_REGIONS

!-----------------------------------------------------------------------
      SUBROUTINE DESID_READ_NAMELIST( )
!
!     This subroutine opens and reads the Emissions Control Namelist. It 
!     attempts to deal with errors like missing file or missing file 
!     sections by error checking and setting defaults.
!-----------------------------------------------------------------------

      use desid_param_module
      use util_family_module
      use RUNTIME_VARS, only: MISC_CTRL, DESID_CTRL, DESID_CHEM_CTRL,
     &                        logdev, log_message, log_subheading
      use PA_DEFN, ONLY : BudgetVariables, MAX_BUDGET_VARS_NML, BUDGET_DIAG
#ifdef mpas
      use util_module, only : junit, upcase
#endif
      use utilio_defn

      IMPLICIT NONE

         ! Define Dummy Variables for Opening Emission Control Namelist
         CHARACTER( 300 ) :: XMSG    
         INTEGER          :: Desid_N_Diag_Rules, Desid_Max_Area, Desid_Max_Sd
         INTEGER          :: FUNIT
         INTEGER          :: IOST, IFAM, INUM, IRULE
         CHARACTER( 200 ) :: TMPLINE
         
         ! Define Namelist Input from Control Files
         ! CMAQ Control Util
         Namelist / Budget_Options       / Budget_Diag, BudgetVariables
         
         ! DESID Chem Control
         Namelist / Desid_ScalingVars    / Desid_Max_Rules
         Namelist / Desid_Scaling        / Desid_Rules_Nml
         
         ! DESID Control
         Namelist / Desid_Options        / Desid_MaxLays

         Namelist / Desid_AreaNormVars   / Desid_Max_Area
         Namelist / Desid_AreaNorm       / Desid_Area_Nml

         Namelist / Desid_SizeDistVars   / Desid_Max_Sd
         Namelist / Desid_SizeDist       / Desid_Sd_Nml

         Namelist / Desid_RegionDefVars  / Desid_Max_Reg,
     &                                     Desid_N_Reg_Fams, 
     &                                     Desid_Max_Reg_Fam_Members
         Namelist / Desid_RegionDef      / Desid_Reg_Nml

         Namelist / Desid_DiagVars       / Desid_N_Diag_Rules,
     &                                     Desid_Max_Diag_Streams,
     &                                     Desid_Max_Diag_Spec
         Namelist / Desid_Diag           / Desid_Diag_Streams_Nml,
     &                                     Desid_Diag_Fmt_Nml, 
     &                                     Desid_Diag_Spec_Nml

         CALL LOG_MESSAGE( LOGDEV, ' ' )
         CALL LOG_SUBHEADING( LOGDEV, 'Reading Emission Control Namelist')
         
         !!! Budget Options !!!
           ! Allocate and Initialize Budget Variables
           Budget_Diag = .FALSE.
           ALLOCATE( BudgetVariables( Max_Budget_Vars_Nml ) )
           BudgetVariables = ''
          
           ! Retrieve the Name of the Emission Control File
           IF ( MISC_CTRL .EQ. "MISC_CTRL_NML" ) THEN
               XMSG = 'You have chosen not to indicate the location of an' //
     &                'CMAQ Control namelist file. You must give a value ' //
     &                'for the MISC_CTRL variable in the CMAQ runscript.'
               CALL M3EXIT( 'DESID_READ_NAMELIST',0,0,XMSG,1 )
           END IF
          
           ! Open Emission Control Namelist File
           FUNIT = JUNIT()
           OPEN( FILE = MISC_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &           POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = IOST )
          
           ! Check for Error in File Open Process
           IF ( IOST .NE. 0 ) THEN
               WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &                'CMAQ control namelist file: ',TRIM( MISC_CTRL )
               CALL M3EXIT( 'DESID_READ_NAMELIST',0,0,XMSG,1 )
           END IF
          
           ! Read Budget Variables Specification Section
           REWIND( FUNIT )
           READ( NML = Budget_Options, UNIT = FUNIT, IOSTAT= IOST )
           IF ( IOST .EQ. -1 ) THEN
               WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &                'Note: The BudgetOptions section of the Emissions Control ',
     &                'Namelist is missing. Default values for this section will be ',
     &                'assumed.'
               Budget_Diag   = .FALSE.
               BudgetVariables  = 'ALL'
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for BudgetOptions
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the Budget_Options '//
     &                'variable in the CMAQ control namelist. Please check the format of '//
     &                'each line for syntax errors. The invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix the Budget Variables specification', 1 ) 
           END IF
           ! Capitalize All Budget Variables Names
           DO IFAM = 1,Max_Budget_Vars_Nml
               CALL UPCASE( BudgetVariables( IFAM ) )
           END DO
          
           CLOSE( FUNIT )

         !----------------------------!
         !!! DESID Chemical Mapping !!!
           ! Retrieve the Name of the Emission Control File
           IF ( DESID_CHEM_CTRL .EQ. "DESID_CHEM_CTRL_NML" ) THEN
               XMSG = 'You have chosen not to indicate the location of an' //
     &                'Emission Control namelist file. You must give a value ' //
     &                'for the DESID_CHEM_CTRL variable in the CMAQ runscript.'
               CALL M3EXIT( 'DESID_READ_NAMELIST',0,0,XMSG,1 )
           END IF
           
           ! Open Emission Control Namelist File
           FUNIT = JUNIT()
           OPEN( FILE = DESID_CHEM_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &           POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = IOST )
           
           ! Check for Error in File Open Process
           IF ( IOST .NE. 0 ) THEN
               WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &                'emissions control namelist file: ',TRIM( DESID_CHEM_CTRL )
               CALL M3EXIT( 'DESID_READ_NAMELIST',0,0,XMSG,1 )
           END IF
           
           ! Read the number of Max Emissions Rules to inform scaling operations
           REWIND( FUNIT )
           READ( NML = Desid_ScalingVars, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The DESID_Max_Rules Variable was completely missing
               XMSG = 'WARNING: Maximum Number of DESID Scaling Rules was not specified. '//
     &                'If you intended to specify Desid_Max_Rules, check the DESID_CHEM_CTRL file.'
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_MAX_RULES = 500
           
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_RULES_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading ithe max number of '//
     &                'Emission Scaling Rules for use by the DESID module. Please '//
     &                'check the format of each line for syntax errors. The '//
     &                'invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_Max_Rules', 1 )
           END IF
           
           ! Allocate Initialize Namelist Variables
           ALLOCATE( DESID_RULES_NML( DESID_MAX_RULES ), STAT=IOST )
           CALL CHECKMEM( IOST, 'DESID_RULES_NML','DESID_READ_NAMELIST')
           DESID_RULES_NML%REGION = ''
           DESID_RULES_NML%STREAM = ''
           DESID_RULES_NML%EMVAR  = ''
           DESID_RULES_NML%SPEC   = ''
           DESID_RULES_NML%PHASE  = ''
           DESID_RULES_NML%OP     = ''
           DESID_RULES_NML%BASIS  = ''
           DESID_RULES_NML%FAC    = 0.
           
           ! Read the Emissions Rules to inform scaling operations
           REWIND( FUNIT )
           READ( NML = Desid_Scaling, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The DESID_RULES_NML Variable was completely missing
               XMSG = 'WARNING: There were no valid Emission Scaling Rules specified '//
     &                'for use by the DESID module. If you intended to specify '//
     &                'rules in the emission control file, check the file you have '//
     &                'provided for DESID_CHEM_CTRL.' 
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_RULES_NML%REGION = ''
               DESID_RULES_NML%STREAM = ''
               DESID_RULES_NML%EMVAR  = ''
               DESID_RULES_NML%SPEC   = ''
               DESID_RULES_NML%PHASE  = ''
               DESID_RULES_NML%OP     = ''
               DESID_RULES_NML%BASIS  = ''
               DESID_RULES_NML%FAC    = 0.
           
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_RULES_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading Emission Scaling '//
     &                'Rules for use by the DESID module. Please check the format of '//
     &                'each line for syntax errors. The invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix the Emission Scaling Rules', 1 )
           END IF
           
           CALL LOG_MESSAGE( LOGDEV,' ' )
           CALL LOG_MESSAGE( LOGDEV,'Performing Basic Error Checks for Emission Scaling Rules' )
           
           ! Check that the operator field is correct since it has not so
           ! many possible values.
           DO IRULE = 1,DESID_MAX_RULES
               IF( DESID_RULES_NML( IRULE )%SPEC .EQ. '' ) EXIT
               IF( DESID_RULES_NML( IRULE )%OP .NE. 'a' .AND. DESID_RULES_NML( IRULE )%OP .NE. 'A' .AND.
     &              DESID_RULES_NML( IRULE )%OP .NE. 'o' .AND. DESID_RULES_NML( IRULE )%OP .NE. 'O'.AND.
     &              DESID_RULES_NML( IRULE )%OP .NE. 'm' .AND. DESID_RULES_NML( IRULE )%OP .NE. 'M' ) THEN
                 WRITE( XMSG, '(A23,I4,A27,A3)'),'Emission Scaling Rule #',IRULE,
     &                ' has a bad operator value: ',DESID_RULES_NML(IRULE)%OP
                 call m3exit ( 'DESID_READ_NAMELIST', 0, 0, XMSG, 1 )
               END IF
           ENDDO     
           
           CLOSE( FUNIT )

         !-----------------------------!
         !!! Open DESID Control File !!!
         !-----------------------------!
         ! Retrieve the Name of the Emission Control File
         IF ( DESID_CTRL .EQ. "DESID_CTRL_NML" ) THEN
             XMSG = 'You have chosen not to indicate the location of an' //
     &              'Emission Control namelist file. You must give a value ' //
     &              'for the DESID_CTRL variable in the CMAQ runscript.'
             CALL M3EXIT( 'DESID_READ_NAMELIST',0,0,XMSG,1 )
         END IF

         ! Open Emission Control Namelist File
         FUNIT = JUNIT()
         OPEN( FILE = DESID_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &         POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = IOST )

         ! Check for Error in File Open Process
         IF ( IOST .NE. 0 ) THEN
             WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &              'emissions control namelist file: ',TRIM( DESID_CTRL )
             CALL M3EXIT( 'DESID_READ_NAMELIST',0,0,XMSG,1 )
         END IF
 
         !----------------------!
         !!! DESID Top Layer  !!!
           ! Read the Maximum Emissions Layer
           REWIND( FUNIT )
           READ( NML = Desid_Options, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The Desid_Max_Lays Variable was completely missing
               XMSG = 'WARNING: Maximum Layer for emissions input was not specified. '//
     &                'If you intended to specify Desid_Max_Lays, check the DESID_CTRL file.'
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               Desid_MaxLays = 0
           
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for Desid_Max_Lays
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the max number of '//
     &                'Emission Layers for use by the DESID module. Please '//
     &                'check the format of each line for syntax errors. The '//
     &                'invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_Max_Lays', 1 )
           END IF
 
         !------------------------------!
         !!! DESID Area Normalization !!!
           ! Read the number of Max Emissions Rules to inform scaling operations
           REWIND( FUNIT )
           READ( NML = Desid_AreaNormVars, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The DESID_Max_Rules Variable was completely missing
               XMSG = 'WARNING: Maximum Number of DESID Area Normalization Rules was not specified. '//
     &                'If you intended to specify Desid_Max_Area, check the DESID_CTRL file.'
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_MAX_AREA = 30
           
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_RULES_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the max number of '//
     &                'Emission Area Normalization Rules for use by the DESID module. Please '//
     &                'check the format of each line for syntax errors. The '//
     &                'invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_Max_Area', 1 )
           END IF
           
           ! Allocate Initialize Namelist Variables
           ALLOCATE( DESID_AREA_NML( DESID_MAX_AREA ), STAT=IOST )
           CALL CHECKMEM( IOST, 'DESID_AREA_NML','DESID_READ_NAMELIST')
           DESID_AREA_NML%STREAM  = 'ALL'
           DESID_AREA_NML%AREA    = 'AUTO'
           DESID_AREA_NML%ADJ     = 'AUTO'
           
           ! Read the Area Normalization Registry
           REWIND( FUNIT )
           READ( NML = Desid_AreaNorm, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               XMSG = 'Note: The Area Normalization section of the Emissions Control '//
     &                'Interface is missing. Default values for this section will be '//
     &                'assumed.'
               CALL LOG_MESSAGE( LOGDEV,' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_AREA_NML%STREAM = 'ALL'
               DESID_AREA_NML%AREA   = 'AUTO'
               DESID_AREA_NML%ADJ    = 'AUTO'
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_AREA_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the Area Normalization '//
     &                'variable for use by the DESID module. Please check the format of '//
     &                'each line for syntax errors. The invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '// 
     &                       'fix the Area Normalization section', 1 ) 
           END IF
              
         !-----------------------------!
         !!! DESID Region Definition !!!
           ! Read the number of Max Emissions Rules to inform scaling operations
           REWIND( FUNIT )
           READ( NML = Desid_RegionDefVars, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The DESID_Max_Rules Variable was completely missing
               XMSG = 'WARNING: Maximum Number of DESID Region Def Variables was not specified. '//
     &                'If you intended to specify Desid_Max_Reg, check the DESID_CTRL file.'
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_MAX_REG = 30
           
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_RULES_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the max number of '//
     &                'Region Definitions for use by the DESID module. Please '//
     &                'check the format of each line for syntax errors. The '//
     &                'invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_Max_Reg', 1 )
           END IF
 
           ! Allocate and Initialize Namelist Variables
           ALLOCATE( DESID_REG_NML( DESID_MAX_REG ), STAT=IOST )
           CALL CHECKMEM( IOST, 'DESID_REG_NML','DESID_READ_NAMELIST')
           DESID_REG_NML%LABEL = ''
           DESID_REG_NML%FILE  = ''
           DESID_REG_NML%VAR   = ''
           
           ! Read the Regions Registry
           REWIND( FUNIT )
           READ( NML = Desid_RegionDef, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               XMSG = 'Note: The Desid_RegionDef component of the Emissions Control '//
     &                'Interface is missing. Default values for this section will be '//
     &                'assumed.'
               CALL LOG_MESSAGE( LOGDEV,' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_REG_NML%LABEL = ''
               DESID_REG_NML%FILE  = ''
               DESID_REG_NML%VAR   = ''
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_REG_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the Desid_RegionDef '//
     &                'variable for use by the DESID module. Please check the format of '//
     &                'each line for syntax errors. The invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_RegionDef', 1 ) 
           END IF
           
           
         !------------------------------!
         !!! DESID Size Distributions !!!
           ! Read the number of Max Size Dist Rules to inform scaling operations
           REWIND( FUNIT )
           READ( NML = Desid_SizeDistVars, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The DESID_Max_Sd Variable was completely missing
               XMSG = 'WARNING: Maximum Number of DESID Size Dist Rules was not specified. '//
     &                'If you intended to specify Desid_Max_Sd, check the DESID_CTRL file.'
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_MAX_SD = 10
           
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_RULES_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the max number of '//
     &                'Size Distribution RUles for use by the DESID module. Please '//
     &                'check the format of each line for syntax errors. The '//
     &                'invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_Max_Sd', 1 )
           END IF
 
           ! Allocate and Initialize Namelist Variables
           ALLOCATE( DESID_SD_NML( DESID_MAX_SD ), STAT=IOST )
           CALL CHECKMEM( IOST, 'DESID_SD_NML','DESID_READ_NAMELIST') 
           DESID_SD_NML%STREAM   = ''
           DESID_SD_NML%MODE     = ''
           DESID_SD_NML%MODE_REF = ''
           
        !!! Read the size distribution specification section
           REWIND( FUNIT )
           READ( NML = Desid_SizeDist, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               XMSG = 'Note: The Desid_SizeDist component of the Emissions Control '//
     &                'Interface is missing. Default values for this section '//
     &                'will be assumed.'
               CALL LOG_MESSAGE( LOGDEV,' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_SD_NML%STREAM   = ''
               DESID_SD_NML%MODE     = ''
               DESID_SD_NML%MODE_REF = ''
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_SD_NML
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the Desid_SizeDist '//
     &                'variable for use by the DESID module. Please check the format of '//
     &                'each line for syntax errors. The invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix the Size Distribution Rule', 1 ) 
           END IF

         !----------------------------------!
         !!! DESID Diagnostic File Inputs !!!
           ! Read the number of Max Emissions Rules to inform scaling operations
           REWIND( FUNIT )
           READ( NML = Desid_DiagVars, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The DESID_N_Diag_Rules Variable was completely missing
               XMSG = 'WARNING: Number of DESID Diagnostic Rules was not specified. '//
     &                'If you intended to specify Desid_N_Diag_Rules, check the DESID_CTRL file.'
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               DESID_N_DIAG_RULES = 0
               DESID_MAX_DIAG_STREAMS = 0
               DESID_MAX_DIAG_SPEC = 0

           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error Detected for DESID_N_DIAG_RULES
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading the number of '//
     &                'Diagnostic Rules for use by the DESID module. Please '//
     &                'check the format of each line for syntax errors. The '//
     &                'invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix Desid_N_Diag_Rules', 1 )
           END IF

           ! Allocate and Initialize Namelist Variables
           ALLOCATE( DESID_DIAG_STREAMS_NML( DESID_N_DIAG_RULES,DESID_MAX_DIAG_SPEC ),
     &               DESID_DIAG_FMT_NML( DESID_N_DIAG_RULES ),
     &               DESID_DIAG_SPEC_NML( DESID_N_DIAG_RULES,DESID_MAX_DIAG_SPEC ),
     &               STAT=IOST )
           CALL CHECKMEM( IOST, 'DESID_DIAG_NML','DESID_READ_NAMELIST')
           Desid_Diag_Streams_Nml = ''
           Desid_Diag_Fmt_Nml     = ''  
           Desid_Diag_Spec_Nml    = ''
           
           ! Read the Emissions Diagnostic Section
           REWIND( FUNIT )
           READ( NML = Desid_Diag, UNIT = FUNIT, IOSTAT=IOST )
           IF ( IOST .EQ. -1 ) THEN
               ! The Emissions Diagnostic Section was completely missing
               XMSG = 'WARNING: There were no valid Emission Diagnostic Values specified '//
     &                'for use by the DESID module. If you intended to specify '//
     &                'diagnostic output in the emission control interface, check the '//
     &                'file you have provided for DESID_CTRL_NML.' 
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               Desid_Diag_Streams_Nml = ''
               Desid_Diag_Fmt_Nml     = ''  
               Desid_Diag_Spec_Nml    = ''
           ELSE IF ( IOST .NE. 0 ) THEN
               ! Read Error for Emissions Diagnostic
               backspace( FUNIT )
               read( FUNIT, fmt='(A)' ) tmpline
               XMSG = 'ERROR: There was a syntax error reading Emission Diagnostic '//
     &                'Variables for output by the DESID module. Please check the format of '//
     &                'each line for syntax errors. The invalid line was likely: '
               CALL LOG_MESSAGE( LOGDEV, ' ')
               CALL LOG_MESSAGE( LOGDEV, XMSG )
               WRITE( LOGDEV, '(8x,A)' ) TMPLINE
               CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                       'fix the Emission Diagnostic Specification.', 1 )
           END IF
           
         CLOSE( UNIT = FUNIT )

      END SUBROUTINE DESID_READ_NAMELIST  

! -------------------------------------------------------------------------
        subroutine soilinp_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          use RUNTIME_VARS, only : NEW_START, BIOGEMIS_MEGAN, BIOGEMIS_BEIS,
     &                             BDSNP_MEGAN, IGNORE_SOILINP


          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, parameter :: mxhrs = 24
          Character( 40 ), parameter :: pname = 'soilinp_setup'
          character( 20 )            :: loc_time_stamp
          Character( 40 ), parameter :: msoilinp = 'MEGAN_SOILINP'
          Character( 40 ), parameter :: bsoilinp = 'BEIS_SOILINP'
          Character( 40 ), parameter :: bdsnpinp = 'BDSNPINP'

          CHARACTER( 16 )  :: var
          CHARACTER( 256 ) :: mesg
          integer :: stat, i, j, k, fnumb, fnumm
          real, allocatable :: t24sum(:,:), sw24sum(:,:)

          ALLOCATE( PTYPE( NCOLS,NROWS ),
     &              PULSEDATE( NCOLS,NROWS ),
     &              PULSETIME( NCOLS,NROWS ),
     &              RAINFALL( NCOLS,NROWS, mxhrs ),
     &              DDTTM( mxhrs ),
     &              STAT=STAT )
          if (stat .ne. 0) then
            CALL M3EXIT (PNAME, 0, 0, "allocation error", 1)
          end if

          RAINFALL = 0.0
          DDTTM    = ' '   ! array

          if (BIOGEMIS_MEGAN) then
             ALLOCATE (t24y     ( ncols,nrows        ),
     &                 sw24y    ( ncols,nrows        ),
     &                 lai_y    ( ncols,nrows        ),
     &                 HRNO_SW  ( NCOLS,NROWS, mxhrs ),
     &                 HRNO_T2M ( NCOLS,NROWS, mxhrs ),
     &                 t24sum   ( ncols,nrows        ),
     &                 sw24sum  ( ncols,nrows        ),
     &                 stat=stat)
     
             IF ( STAT .NE. 0 ) THEN
                MESG = 'Failure BIOGEMIS_MEGAN arrays'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT3 )
             END IF

             t24y     = 0.0
             sw24y    = 0.0
             lai_y    = 0.0
             HRNO_SW  = 0.0
             HRNO_T2M = 0.0
             sw24sum  = 0.0
             t24sum   = 0.0
          end if
      
          if (BDSNP_MEGAN) then
             ALLOCATE (PFACTOR   ( ncols,nrows       ),
     &                 DRYPERIOD ( ncols,nrows       ),
     &                 NDEPRES   ( ncols,nrows       ),
     &                 NDEPRATE  ( ncols,nrows       ),
     &                 SOILMPREV ( ncols,nrows       ),
     &                 stat=stat)

             IF ( STAT .NE. 0 ) THEN
                MESG = 'Failure BDSNP_MEGAN arrays'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT3 )
             END IF

             pfactor   = 0.0
             dryperiod = 0.0
             soilmprev = 0.0
             ndepres   = 0.0
             ndeprate  = 0.0
          end if
           
          if (BIOGEMIS_BEIS .and. .not. NEW_START .and. .not. IGNORE_SOILINP) then
#ifdef mpas
             ! file will be opened automatically when it is indicated in FILE_INFO

             call mio_fread (BSOILINP, 'PTYPE', pname, ptype(:,1))
             call mio_fread (BSOILINP, 'PULSEDATE', pname, pulsedate(:,1))
             call mio_fread (BSOILINP, 'PULSETIME', pname, pulsetime(:,1))
             fnumb = mio_search (BSOILINP)
#else
             IF ( .NOT. OPEN3( BSOILINP, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // BSOILINP
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF
             n_opened_file = n_opened_file + 1

! Get description of NO rain data file
             IF ( .NOT. DESC3( BSOILINP ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( BSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

! Check that the file start date and time are consistent
             IF ( SDATE3D .NE. cio_model_sdate ) THEN
                WRITE( MESG, 94011 ) 'Cannot use BEIS_SOILINP file; ' //
     &              'found date ', SDATE3D, ' expecting ', cio_model_sdate
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             IF ( STIME3D .NE. cio_model_stime ) THEN
                WRITE( MESG, 94011 ) 'Cannot use BEIS_SOILINP file; ' //
     &              'found time ', STIME3D, ' expecting ', cio_model_stime
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

94011        FORMAT( A, F10.2, 1X, A, I3, ',', I3 )

             VAR = 'PTYPE'
             IF ( .NOT. XTRACT3( BSOILINP, 'PTYPE', 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                           0, 0, PTYPE ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( BSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             VAR = 'PULSEDATE'
             IF ( .NOT. XTRACT3( BSOILINP, VAR, 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                           0, 0, PULSEDATE ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( BSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             VAR = 'PULSETIME'
             IF ( .NOT. XTRACT3( BSOILINP, VAR, 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                           0, 0, PULSETIME ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( BSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF
#endif

             RAINFALL = 0.0

             DDTTM = ' '   ! array
             DO I = 1, mxhrs
                WRITE ( VAR, '(A8,I2.2)' ) 'RAINFALL', I
#ifdef mpas
                loc_time_stamp = mio_file_data(fnumb)%timestamp(i)

                call mio_fread (BSOILINP, 'RAINFALL', pname,
     &                          rainfall(:,1,i), loc_time_stamp)

#else
                IF ( .NOT. XTRACT3( BSOILINP, VAR, 1, 1,
     &                              STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                              0, 0, RAINFALL( :,:,I ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( BSOILINP ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
                J = INDEX( VDESC3D( I+3 ), 'for' ) + 3
                K = LEN_TRIM( VDESC3D( I+3 ) )
                DDTTM( I ) = VDESC3D( I+3 )( J:K )
#endif
             END DO

          end if ! end beis section

          if (BIOGEMIS_MEGAN .and. .not. NEW_START .and. .not. IGNORE_SOILINP) then
#ifdef mpas
             ! file will be opened automatically when it is indicated in FILE_INFO
             fnumm = mio_search (MSOILINP)
             ! Use final time step of each variable
             loc_time_stamp = mio_file_data(fnumm)%timestamp(24)
             call mio_fread (MSOILINP, 'PTYPE', pname, ptype(:,1),loc_time_stamp)
             call mio_fread (MSOILINP, 'PULSEDATE', pname,pulsedate(:,1),loc_time_stamp)
             call mio_fread (MSOILINP, 'PULSETIME', pname,pulsetime(:,1),loc_time_stamp)
             call mio_fread (MSOILINP, 'LAI', pname,LAI_y(:,1),loc_time_stamp)

#else
             IF ( .NOT. OPEN3( MSOILINP, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // MSOILINP
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF
             n_opened_file = n_opened_file + 1

! Get description of NO rain data file
             IF ( .NOT. DESC3( MSOILINP ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( MSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

! Check that the file start date and time are consistent
             IF ( SDATE3D .NE. cio_model_sdate ) THEN
                WRITE( MESG, 94010 ) 'Cannot use MEGAN_SOILINP file; ' //
     &              'found date ', SDATE3D, ' expecting ', cio_model_sdate
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             IF ( STIME3D .NE. cio_model_stime ) THEN
                WRITE( MESG, 94010 ) 'Cannot use MEGAN_SOILINP file; ' //
     &              'found time ', STIME3D, ' expecting ', cio_model_stime
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

94010        FORMAT( A, F10.2, 1X, A, I3, ',', I3 )

             VAR = 'PTYPE'
             IF ( .NOT. XTRACT3( MSOILINP, 'PTYPE', 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                           0, 0, PTYPE ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( MSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             VAR = 'PULSEDATE'
             IF ( .NOT. XTRACT3( MSOILINP, VAR, 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                           0, 0, PULSEDATE ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( MSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             VAR = 'PULSETIME'
             IF ( .NOT. XTRACT3( MSOILINP, VAR, 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                           0, 0, PULSETIME ) ) THEN
                MESG = 'Could not read "' // TRIM( VAR ) //
     &                 '" from file "' // TRIM( MSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

             IF ( .NOT. XTRACT3( MSOILINP, 'LAI', 1, 1,
     &                           STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                           0, 0, lai_y( :,: ) ) ) THEN
                MESG = 'Could not read "' // 'LAI' //
     &                 '" from file "' // TRIM( MSOILINP ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             END IF

#endif

             sw24sum  = 0.0
             t24sum   = 0.0
             RAINFALL = 0.0

             DDTTM = ' '   ! array
             DO I = 1, mxhrs
                WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I

#ifdef mpas
                loc_time_stamp = mio_file_data(fnumm)%timestamp(i)


                   call mio_fread (MSOILINP, 'RAINFALL', pname,
     &                             rainfall(:,1,i), loc_time_stamp)

                   call mio_fread (MSOILINP, 'T24', pname,
     &                             t24y(:,1), loc_time_stamp)



                   call mio_fread (MSOILINP, 'SW24', pname,
     &                             sw24y(:,1), loc_time_stamp)


#else
                IF ( .NOT. XTRACT3( MSOILINP, VAR, 1, 1,
     &                              STRTROWSTD, ENDROWSTD, STRTCOLSTD, ENDCOLSTD,  
     &                              0, 0, RAINFALL( :,:,I ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( MSOILINP ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
                J = INDEX( VDESC3D( I+3 ), 'for' ) + 3
                K = LEN_TRIM( VDESC3D( I+3 ) )
                DDTTM( I ) = VDESC3D( I+3 )( J:K )

                WRITE( VAR, '(A2,I2.2)' ) 'SW', I
                IF ( .NOT. XTRACT3( MSOILINP, VAR, 1, 1,
     &                              STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                              0, 0, sw24y( :,: ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( MSOILINP ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF

                WRITE( VAR, '(A3,I2.2)' ) 'T2M', I
                IF ( .NOT. XTRACT3( MSOILINP, VAR, 1, 1,
     &                              STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                              0, 0, t24y( :,: ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( MSOILINP ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
#endif
                sw24sum = sw24y + sw24sum
                t24sum  = t24y + t24sum

             END DO ! looping over 24 hrs 

             sw24y = sw24sum/mxhrs
             t24y  = t24sum/mxhrs


             if (BDSNP_MEGAN) then       ! BDSNP daily inputs
#ifdef mpas
                fnumm = mio_search (BDSNPINP)
                loc_time_stamp = mio_file_data(fnumm)%timestamp(24)
                call mio_fread (BDSNPINP, 'SOILMPREV', pname, SOILMPREV(:,1), loc_time_stamp)
                call mio_fread (BDSNPINP, 'DRYPERIOD', pname, DRYPERIOD(:,1), loc_time_stamp)
                call mio_fread (BDSNPINP, 'PFACTOR', pname, PFACTOR(:,1), loc_time_stamp)
                call mio_fread (BDSNPINP, 'NDEPRES', pname, NDEPRES(:,1), loc_time_stamp)
#else
                    IF ( .NOT. OPEN3( BDSNPINP, FSREAD3, PNAME ) ) THEN
                       mesg = 'Open failure for ' // BDSNPINP
                       Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
                    END IF
       
                    IF ( .NOT. DESC3( BDSNPINP ) ) THEN
                       MESG = 'Could not get description of file "' //
     &                  TRIM( BDSNPINP ) // '"'
                       CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
                    END IF
                    IF ( .NOT. XTRACT3( BDSNPINP, 'DRYPERIOD',
     &                                  1, 1, STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                                  0, 0, dryperiod(:,:) ) ) THEN
                       mesg = 'Could not extract ' // BDSNPINP // ' file'
                       CALL M3EXIT ( PNAME, 0, 0, mesg, XSTAT1 )
                    END IF
                    IF ( .NOT. XTRACT3( BDSNPINP, 'NDEPRES',
     &                                  1, 1, STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                                  0, 0, ndepres(:,:) ) ) THEN
                       mesg = 'Could not extract ' // BDSNPINP // ' file'
                       CALL M3EXIT ( PNAME, 0, 0, mesg, XSTAT1 )
                    END IF
                    IF ( .NOT. XTRACT3( BDSNPINP, 'NDEPRATE_DIAG',
     &                                  1, 1, STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                                  0, 0, ndeprate(:,:) ) ) THEN
                       mesg = 'Could not extract ' // BDSNPINP // ' file'
                       CALL M3EXIT ( PNAME, 0, 0, mesg, XSTAT1 )
                    END IF
                    IF ( .NOT. XTRACT3( BDSNPINP, 'PFACTOR',
     &                                  1, 1, STRTROWSTD, ENDROWSTD, STRTCOLSTD,ENDCOLSTD,
     &                                  0, 0, pfactor(:,:) ) ) THEN
                       mesg = 'Could not extract ' // BDSNPINP // ' file'
                       CALL M3EXIT ( PNAME, 0, 0, mesg, XSTAT1 )
                    END IF
                    IF ( .NOT. XTRACT3( BDSNPINP, 'SOILMPREV',
     &                                  1, 1, STRTROWSTD, ENDROWSTD,STRTCOLSTD,ENDCOLSTD,
     &                                  0, 0, soilmprev(:,:) ) ) THEN
                       mesg = 'Could not extract ' // BDSNPINP // ' file'
                       CALL M3EXIT ( PNAME, 0, 0, mesg, XSTAT1 )
                    END IF
#endif
             end if ! bdsnp check
          end if ! megan check

          if (BIOGEMIS_MEGAN) then
             deallocate (t24sum, sw24sum)
          end if

        end subroutine soilinp_setup

! -------------------------------------------------------------------------
        subroutine lus_setup

!     Function:

!         Set-up land-use categories for dust. Allocate and fill in:
!             -- lut array --> landuse category fraction
!             -- ladut array --> % of desertland


          use RUNTIME_VARS
!         use UTILIO_DEFN
          use lus_data_module  ! Data module that contains info. on different land schemes
          use HGRD_DEFN, only : ncols, nrows
#ifdef twoway
          use twoway_data_module
#endif

          INCLUDE SUBST_FILES_ID             ! file name parameters

          character (24), parameter :: strg = 'incorrect num_land_cat, '
          character (40), parameter :: pname = 'lus_setup'

          character (256) :: xmsg
          integer :: i, err, strtcol1,endcol1, strtrow1, endrow1,
     &               strtcol2, endcol2, strtrow2, endrow2, gxoff1,
     &               gyoff1, gxoff2, gyoff2

          lufile( 1 ) = grid_cro_2d

#ifndef mpas
          if ( .not. lucro_avail  ) then ! TRUE if LUFRAC file isn't there 
          
            if ( .not. open3( lufile( 1 ), fsread3, pname ) ) then
               xmsg = 'could not open ' // trim( lufile( 1 ) )
               call m3exit ( pname, 0, 0, xmsg, xstat1 )
            end if
            n_opened_file = n_opened_file + 1

            ! Retrieve domain decomposition offsets for first lufile 
            call subhfile( lufile( 1 ), gxoff1, gyoff1, strtcol1, 
     &                     endcol1, strtrow1, endrow1 )   
 
          
          end if

#endif

    ! determine land_scheme from GRID_CRO_2D

#ifdef mpas
          dust_land_scheme = mminlu_mpas
#else
          dust_land_scheme = cio_dust_land_scheme ! land scheme found from grid_cro_2D 'DLUSE' var-desc

#ifdef twoway

C   mminlu and num_land_cat are WRF global variables

          select case( mminlu )

             case( 'USGS24' )
                if ( num_land_cat .ne. 24 ) then
                   write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                    ' for ' // trim( mminlu )
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                dust_land_scheme = 'USGS24'
             case( 'NLCD40' )
                if ( num_land_cat .ne. 40 ) then
                   write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                    ' for ' // trim( mminlu )
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                dust_land_scheme = 'NLCD40'
             case( 'NLCD-MODIS' )
                if ( num_land_cat .ne. 50 ) then
                   write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                    ' for ' // trim( mminlu )
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                dust_land_scheme = 'NLCD-MODIS'
             case( 'MODIFIED_IGBP_MODIS_NOAH' )
                if ( num_land_cat .ne. 20 ) then
                   write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                    ' for ' // trim( mminlu )
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                dust_land_scheme = 'MODIS_NOAH'
             case( 'MODIS' )
                if ( num_land_cat .ne. 20 ) then
                   write( xmsg, '(a, i3, a )' ) strg, num_land_cat,
     &                                    ' for ' // trim( mminlu )
                   call m3exit ( pname, stdate, sttime, xmsg, xstat1 )
                end if
                dust_land_scheme = 'MODIS'
             case default
                xmsg = 'Land use scheme not supported'
                call m3exit ( pname, stdate, sttime, xmsg, xstat1 )

          end select
#endif
#endif

          select case( dust_land_scheme ) ! After land scheme is determined allocate number of land use categories & number of dustland categories from lus_data_module

             case( 'USGS24' )           ! If USGS34
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

             case( 'MODIS' )            ! If MODIS
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

             case( 'NLCD40' )           ! If NLCD40
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

             case( 'NLCD-MODIS', 'NLCD50' ) ! If NCLD-MODIS or NCLD50
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

             case( 'MODIS_NOAH' )       ! If MODIS-NOAH
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

             case default               ! Other land-schemes not supported
                xmsg = 'Land use scheme not supported'
                call m3exit ( pname, stdate, sttime, xmsg, xstat1 )

          end select

! Writing Landuse categories to logfiles
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

          if ( .not. lucro_avail ) then ! TRUE if LUFRAC file isn't there or the land scheme is beld

#ifdef mpas
            do i = 1, n_dlcat  ! Loop through the number of desertland categories and fill in ladut array
               ladut( :,:,i) = lufrac( :,:,vnmld( i )%lu_idx ) ! Use mapped index in LUFRAC from lus_data_module to fill in ladut
            end do

            lut = lufrac ! landuse category fraction is lufrac that is already been extracted

#else
! Get desert land (fraction) data (assume if BELD, all desert types are in file 1)
            do i = 1, n_dlcat
#ifdef twoway
               if ( .not. interpx( lufile( 1 ), vnmld( i )%name, pname,
     &                             strtcol1, endcol1, strtrow1, endrow1,
     &                             1, 1, 0, 0, ladut( :,:,i ) ) ) then
#else
               if ( .not. xtract3( lufile( 1 ), vnmld( i )%name, 1,1,
     &                             strtrow1, endrow1,strtcol1, endcol1,
     &                             0, 0, ladut( :,:,i ) ) ) then
#endif
                  xmsg = 'Could not read ' // trim( vnmld( i )%name )
     &                 // ' from ' // trim( lufile( 1 ) )
                  call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
               end if
            end do

! Get land  use (fraction) data
            do i = 1, n_lucat-1
#ifdef twoway
               if ( .not. interpx( lufile( 1 ), vnmlu( i )%name, pname,
     &                             strtcol1, endcol1, strtrow1, endrow1,
     &                             1, 1, 0, 0, lut( :,:,i ) ) ) then
#else
               if ( .not. xtract3( lufile( 1 ), vnmlu( i )%name, 1,1,
     &                             strtrow1, endrow1,strtcol1, endcol1,
     &                             0, 0, lut( :,:,i ) ) ) then
#endif 
                  xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &                 // ' from ' // trim( lufile( 1 ) )
                  call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
               end if
            end do

            i = n_lucat
#ifdef twoway
               if ( .not. interpx( lufile( 1 ), vnmlu( i )%name, pname, 
     &                             strtcol1, endcol1, strtrow1, endrow1,
     &                             1, 1, 0, 0, lut( :,:,i ) ) ) then
#else
               if ( .not. xtract3( lufile( 1 ), vnmlu( i )%name, 1,1,
     &                             strtrow1, endrow1,strtcol1, endcol1,
     &                             0, 0, lut( :,:,i ) ) ) then
#endif 
                  xmsg = 'Could not read ' // trim( vnmlu( i )%name )
     &                 // ' from ' // trim( lufile( 1 ) )
                  call m3exit( pname, cio_model_sdate, cio_model_stime, xmsg, xstat1 )
               end if
#endif

          else ! IF LUFRAC is there

            do i = 1, n_dlcat  ! Loop through the number of desertland categories and fill in ladut array

              ladut( :,:,i) = lufrac( :,:,vnmld( i )%lu_idx ) ! Use mapped index in LUFRAC from lus_data_module to fill in ladut

            end do

            lut = lufrac ! landuse category fraction is lufrac that is already been extracted

          end if 

        end subroutine lus_setup

! -------------------------------------------------------------------------

        subroutine megan_setup ! reads in variables 

          use hgrd_defn, only : ncols,nrows
          use RUNTIME_VARS, only : logdev, USE_MEGAN_LAI, BDSNP_MEGAN,
     &                             MGN_ONLN_DEP
          Use, intrinsic :: ieee_arithmetic, only: isnan => ieee_is_nan
          USE UTILIO_DEFN

#ifdef mpas
          use util_module, only : nextime

          integer fnum
#endif
          integer                    :: stat, i, megan_hr, megan_day, strtcol,
     &                                  endcol, strtrow, endrow, gxoff, gyoff, 
     &                                  nsteps, nvars

          character( 20 )            :: loc_time_stamp
          real                       :: t24sum(ncols),sw24sum(ncols) 
          character( 40 ), parameter :: pname = 'megan_setup'

          character( 40 ), parameter :: MEGAN_LDF = 'MEGAN_LDF'
          character( 40 ), parameter :: MEGAN_LAI = 'MEGAN_LAI'
          character( 40 ), parameter :: MEGAN_EFS = 'MEGAN_EFS'
          character( 40 ), parameter :: MEGAN_CTS = 'MEGAN_CTS'
          character( 40 ), parameter :: BDSNP_NFILE = 'BDSNP_NFILE'
          character( 40 ), parameter :: BDSNP_AFILE = 'BDSNP_AFILE'
          character( 40 ), parameter :: BDSNP_NAFILE = 'BDSNP_NAFILE'
          character( 40 ), parameter :: BDSNP_FFILE = 'BDSNP_FFILE'
          character( 40 ), parameter :: BDSNP_LFILE = 'BDSNP_LFILE'
          character( 256 )           :: mesg
          character( 40 )            :: var
      
         if (BDSNP_MEGAN) then
            allocate (bdsnp_fert( ncols,nrows),
     &                bdsnp_arid( ncols,nrows),
     &                bdsnp_nonarid( ncols,nrows),
     &                bdsnp_landtype( ncols,nrows),
     &                bdsnp_ndep( ncols,nrows,12),
     &                stat=stat)
            IF ( STAT .NE. 0 ) THEN
               MESG = 'Failure BIOGEMIS_MEGAN arrays in megan_setup'
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT3 )
            END IF
            bdsnp_fert     = 0.
            bdsnp_arid     = 0.
            bdsnp_nonarid  = 0.
            bdsnp_landtype = 0.
            bdsnp_ndep     = 0.
         end if

#ifdef mpas
          fnum = mio_search (MEGAN_CTS)
          nsteps = mio_file_data(fnum)%nsteps
#else
          IF ( .NOT. OPEN3( MEGAN_CTS, FSREAD3, PNAME ) ) THEN
             mesg = 'Open failure for ' // MEGAN_CTS
             Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
          END IF
          n_opened_file = n_opened_file + 1
          f_mbiog = n_opened_file
           
          IF ( .NOT. DESC3( MEGAN_CTS ) ) THEN
             MESG = 'Could not get description of file "' //
     &               TRIM( MEGAN_CTS ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
          END IF
 
          file_sdate(f_mbiog) = sdate3d
          file_stime(f_mbiog) = stime3d
          file_tstep(f_mbiog) = tstep3d
          file_xcell(f_mbiog) = xcell3d
          file_ycell(f_mbiog) = ycell3d
          nsteps = mxrec3d
#endif

          allocate (ctf(6,ncols,nrows), stat = stat)
 
          if (stat .ne. 0) then
            mesg = 'Failure allocating MEGAN input arrays'
            call m3exit (pname, 0, 0, mesg, xstat1 )
          end if

#ifdef mpas
          do I = 1,6
             call mio_fread (MEGAN_CTS, 'CTS', pname, ctf(I,:,1), mio_file_data(fnum)%timestamp(i))
          end do 
#else
          call subhfile( megan_cts , gxoff, gyoff, strtcol, 
     &                   endcol, strtrow, endrow )  

          megan_day = sdate3d 
          megan_hr  = stime3d
          megan_hr = 0
          do I = 1, mxrec3d 

             IF ( .NOT. XTRACT3( MEGAN_CTS, 'CTS', 
     &                           1, 1, strtrow, endrow, strtcol, endcol, 
     &                           0, megan_hr, ctf(I,:,:) ) ) THEN
                mesg = 'Could not extract ' // MEGAN_CTS // ' file'
                CALL M3EXIT ( PNAME, megan_day, megan_hr, mesg, XSTAT1 )
             END IF
             megan_hr = megan_hr + 10000
!            call nextime (megan_day, megan_hr, tstep3d)

          end do
#endif

          WHERE ( isnan( ctf ) ) ctf = 0.0  ! ensure no NaNs

#ifdef mpas
          fnum = mio_search (MEGAN_EFS)
          nvars = mio_file_data(fnum)%nvars
#else
          IF ( .NOT. OPEN3( MEGAN_EFS, FSREAD3, PNAME ) ) THEN
             mesg = 'Open failure for ' // MEGAN_EFS
             Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
          END IF

          IF ( .NOT. DESC3( MEGAN_EFS ) ) THEN
             MESG = 'Could not get description of file "' //
     &               TRIM( MEGAN_EFS ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
          END IF
          nvars = nvars3d
#endif
          allocate (efmaps(ncols,nrows,nvars), stat = stat)
 
          if (stat .ne. 0) then
            mesg = 'Failure allocating MEGAN input arrays'
            call m3exit (pname, 0, 0, mesg, xstat1 )
          end if

#ifdef mpas
          call mio_fread ('MEGAN_EFS', 'EF_ISOP',    pname, efmaps(:,1, 1))
          call mio_fread ('MEGAN_EFS', 'EF_MBO',     pname, efmaps(:,1, 2))
          call mio_fread ('MEGAN_EFS', 'EF_MT_PINE', pname, efmaps(:,1, 3))
          call mio_fread ('MEGAN_EFS', 'EF_MT_ACYC', pname, efmaps(:,1, 4))
          call mio_fread ('MEGAN_EFS', 'EF_MT_CAMP', pname, efmaps(:,1, 5))
          call mio_fread ('MEGAN_EFS', 'EF_MT_SABI', pname, efmaps(:,1, 6))
          call mio_fread ('MEGAN_EFS', 'EF_MT_AROM', pname, efmaps(:,1, 7))
          call mio_fread ('MEGAN_EFS', 'EF_NO',      pname, efmaps(:,1, 8))
          call mio_fread ('MEGAN_EFS', 'EF_SQT_HR',  pname, efmaps(:,1, 9))
          call mio_fread ('MEGAN_EFS', 'EF_SQT_LR',  pname, efmaps(:,1,10))
          call mio_fread ('MEGAN_EFS', 'EF_MEOH',    pname, efmaps(:,1,11))
          call mio_fread ('MEGAN_EFS', 'EF_ACTO',    pname, efmaps(:,1,12))
          call mio_fread ('MEGAN_EFS', 'EF_ETOH',    pname, efmaps(:,1,13))
          call mio_fread ('MEGAN_EFS', 'EF_ACID',    pname, efmaps(:,1,14))
          call mio_fread ('MEGAN_EFS', 'EF_LVOC',    pname, efmaps(:,1,15))
          call mio_fread ('MEGAN_EFS', 'EF_OXPROD',  pname, efmaps(:,1,16))
          call mio_fread ('MEGAN_EFS', 'EF_STRESS',  pname, efmaps(:,1,17))
          call mio_fread ('MEGAN_EFS', 'EF_OTHER',   pname, efmaps(:,1,18))
          call mio_fread ('MEGAN_EFS', 'EF_CO',      pname, efmaps(:,1,19))
#else
          call subhfile( megan_efs, gxoff, gyoff, strtcol, 
     &                   endcol, strtrow, endrow )  

          megan_day = sdate3d 
          megan_hr  = stime3d
        
          IF ( .NOT. XTRACT3( MEGAN_EFS, ALLVAR3, 
     &                        1, 1, strtrow, endrow, strtcol, endcol, 
     &                        megan_day, megan_hr, efmaps(:,:,:) ) ) THEN
              mesg = 'Could not extract ' // MEGAN_EFS // ' file'
              CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
          END IF
#endif

          IF ( USE_MEGAN_LAI) THEN
#ifdef mpas
             fnum = mio_search (MEGAN_LAI)
             nvars = mio_file_data(fnum)%nvars
#else
             IF ( .NOT. OPEN3( MEGAN_LAI, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // MEGAN_LAI
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF

             IF ( .NOT. DESC3( MEGAN_LAI ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( MEGAN_LAI ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
             END IF
             nvars = nvars3d
#endif

             allocate (lai_m(ncols,nrows,nvars-2), stat = stat)
 
             if (stat .ne. 0) then
               mesg = 'Failure allocating MEGAN input arrays'
               call m3exit (pname, 0, 0, mesg, xstat1 )
             end if

             lai_m = 0.0

#ifdef mpas
             do i = 1, nvars
                WRITE( VAR, '(A3,I2.2)' ) 'LAI', I
                call mio_fread (MEGAN_LAI, var, pname, lai_m(:,1,i))
             end do
#else
             call subhfile( megan_lai, gxoff, gyoff, strtcol, 
     &                      endcol, strtrow, endrow )  

             megan_day = sdate3d 
             megan_hr  = stime3d

             do I = 1, nvars3d-2 ! lat/lon excluded
                WRITE( VAR, '(A3,I2.2)' ) 'LAI', I
                IF ( .NOT. XTRACT3( MEGAN_LAI, VAR, 
     &                              1, 1, strtrow, endrow, strtcol, endcol, 
     &                              megan_day, megan_hr, lai_m(:,:,I) ) ) THEN
                   mesg = 'Could not extract ' // MEGAN_LAI // ' file'
                   CALL M3EXIT ( PNAME, 0, 0, mesg, XSTAT1 )
                END IF
             end do
#endif
          END IF      ! USE_MEGAN_LAI

#ifdef mpas
          fnum = mio_search (MEGAN_LDF)
          nvars = mio_file_data(fnum)%nvars
#else
          IF ( .NOT. OPEN3( MEGAN_LDF, FSREAD3, PNAME ) ) THEN
             mesg = 'Open failure for ' // MEGAN_LDF
             Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
          END IF

          IF ( .NOT. DESC3( MEGAN_LDF ) ) THEN
             MESG = 'Could not get description of file "' //
     &               TRIM( MEGAN_LDF ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
          END IF
          nvars = nvars3d
#endif

          allocate (ldf(ncols,nrows,4), stat = stat)
 
          if (stat .ne. 0) then
            mesg = 'Failure allocating MEGAN input arrays'
            call m3exit (pname, 0, 0, mesg, xstat1 )
          end if

#ifdef mpas
          do i = 3,6
            write( var, '(A3,I2.2)' ) 'LDF', i
            call mio_fread ('MEGAN_LDF', var, pname, ldf(:,1,i-2))
          end do
#else
          call subhfile( megan_ldf, gxoff, gyoff, strtcol, 
     &                   endcol, strtrow, endrow )  

          megan_day = sdate3d 
          megan_hr  = stime3d

             IF ( .NOT. XTRACT3( MEGAN_LDF, 'ALL',
     &                           1, 1, strtrow, endrow, strtcol, endcol,
     &                           megan_day, megan_hr, LDF(:,:,:) ) ) THEN
                 mesg = 'Could not extract ' // MEGAN_LDF // ' file'
                 CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
             END IF

#endif

          if (BDSNP_MEGAN) then

          ! Optional BDSNP nitrogen input
             if (.not. MGN_ONLN_DEP) then
#ifdef mpas
                do i = 1, 12
                   write( var, '(A8,I2.2)' ) 'NITROGEN', i
                   call mio_fread (BDSNP_NFILE, var, pname, bdsnp_ndep(:,1,i))
                end do
#else
                IF ( .NOT. OPEN3( BDSNP_NFILE, FSREAD3, PNAME ) ) THEN
                   mesg = 'Open failure for ' // BDSNP_NFILE
                   Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
                END IF

                IF ( .NOT. DESC3( BDSNP_NFILE ) ) THEN
                   MESG = 'Could not get description of file "' //
     &                     TRIM( BDSNP_NFILE ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
                END IF


                call subhfile( BDSNP_NFILE, gxoff, gyoff, strtcol,
     &                        endcol, strtrow, endrow )

                megan_day = sdate3d
                megan_hr  = stime3d
                do i = 1,12
                   write( var, '(A8,I2.2)' ) 'NITROGEN', i
                   IF ( .NOT. XTRACT3( BDSNP_NFILE, var,
     &                                 1, 1, strtrow, endrow, strtcol, endcol,
     &                                 megan_day, megan_hr, bdsnp_ndep(:,:,i) ) ) THEN
                      mesg = 'Could not extract ' // BDSNP_NFILE // ' file'
                      CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
                   END IF
                end do
#endif
             end if

             ! BDSNP fertilizer input

#ifdef mpas
             i  = FLOAT( MOD( STDATE, 1000 ) )
             write( var, '(A4,I3.3)' ) 'FERT', i
             call mio_fread (BDSNP_FFILE, var, pname, bdsnp_fert(:,1))

             call mio_fread (BDSNP_AFILE, 'ARID', pname, bdsnp_arid(:,1))

             call mio_fread (BDSNP_NAFILE, 'NONARID', pname, bdsnp_nonarid(:,1))

             call mio_fread (BDSNP_LFILE, 'LANDTYPE', pname, bdsnp_landtype(:,1))

#else
             IF ( .NOT. OPEN3( BDSNP_FFILE, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // BDSNP_FFILE
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF

             IF ( .NOT. DESC3( BDSNP_FFILE ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( BDSNP_FFILE ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
             END IF

             call subhfile( BDSNP_FFILE, gxoff, gyoff, strtcol,
     &                      endcol, strtrow, endrow )

             i  = FLOAT( MOD( STDATE, 1000 ) )
             write( var, '(A4,I3.3)' ) 'FERT', i
             IF ( .NOT. XTRACT3( BDSNP_FFILE, var,
     &                           1, 1, strtrow, endrow, strtcol, endcol,
     &                           megan_day, megan_hr, bdsnp_fert(:,:) ) ) THEN
                mesg = 'Could not extract ' // BDSNP_FFILE // ' file'
                CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
             END IF

             ! BDSNP arid input

             IF ( .NOT. OPEN3( BDSNP_AFILE, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // BDSNP_AFILE
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF

             IF ( .NOT. DESC3( BDSNP_AFILE ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( BDSNP_AFILE ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
             END IF

             call subhfile( BDSNP_AFILE, gxoff, gyoff, strtcol,
     &                      endcol, strtrow, endrow )


             IF ( .NOT. XTRACT3( BDSNP_AFILE, 'ARID',
     &                           1, 1, strtrow, endrow, strtcol, endcol,
     &                           megan_day, megan_hr, bdsnp_arid(:,:) ) ) THEN
                mesg = 'Could not extract ' // BDSNP_AFILE // ' file'
                CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
             END IF

             ! BDSNP nonarid input

             IF ( .NOT. OPEN3( BDSNP_NAFILE, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // BDSNP_NAFILE
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF

             IF ( .NOT. DESC3( BDSNP_NAFILE ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( BDSNP_NAFILE ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
             END IF

             call subhfile( BDSNP_NAFILE, gxoff, gyoff, strtcol,
     &                      endcol, strtrow, endrow )


             IF ( .NOT. XTRACT3( BDSNP_NAFILE, 'NONARID',
     &                           1, 1, strtrow, endrow, strtcol, endcol,
     &                           megan_day, megan_hr, bdsnp_nonarid(:,:) ) ) THEN
                mesg = 'Could not extract ' // BDSNP_NAFILE // ' file'
                CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
             END IF

             ! BDSNP landtype input

             IF ( .NOT. OPEN3( BDSNP_LFILE, FSREAD3, PNAME ) ) THEN
                mesg = 'Open failure for ' // BDSNP_LFILE
                Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
             END IF

             IF ( .NOT. DESC3( BDSNP_LFILE ) ) THEN
                MESG = 'Could not get description of file "' //
     &                  TRIM( BDSNP_LFILE ) // '"'
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT1 )
             END IF

             call subhfile( BDSNP_LFILE, gxoff, gyoff, strtcol,
     &                      endcol, strtrow, endrow )

             IF ( .NOT. XTRACT3( BDSNP_LFILE, 'LANDTYPE',
     &                           1, 1, strtrow, endrow, strtcol, endcol,
     &                           megan_day, megan_hr, bdsnp_landtype(:,:) ) ) THEN
                mesg = 'Could not extract ' // BDSNP_LFILE // ' file'
                CALL M3EXIT ( PNAME, 0, megan_hr, mesg, XSTAT1 )
             END IF
#endif

          end if   ! BDSNP_MEGAN

        end subroutine megan_setup

! MPAS only routines:
#ifdef mpas
! -------------------------------------------------------------------------
        subroutine stack_files_setup_mpas

!         USE UTILIO_DEFN
          use stk_prms
          use stack_group_data_module
          use get_env_module
          use hgrd_defn, only : ncols, mype
          use vgrd_defn, only : nlays
          use coupler_module, only : pres_ind, g3ddata
          use centralized_io_util_module, only : quicksort
          use util_module, only : index1
          use RUNTIME_VARS, only : emis_sym_date

          use mydata_module

          include SUBST_FILES_ID             ! file name parameters

          character( 40 ), parameter :: pname = 'stack_files_setup_mpas'

          character( 120 ) :: xmsg = ' '
          character( 500 ) :: map_fname, fname
          integer :: n, v, pt, max_nsrc_pts, max_nvars, begin, end, stat, delta,
     &               num_mesh_points, my_num_mesh_points, t_nvars, floc
          integer, allocatable :: d_size(:), pt_size(:),
     &                            stk_gp_sdate(:), stk_gp_stime(:),
     &                            stk_gp_nlays(:), mpas_map(:), my_mpas_map_index(:),
     &                            tdata_1di(:)
          real, allocatable :: tdata_1dr(:)

          call get_env (map_fname, 'mpas_dmap_file', ' ')
          call get_env (num_mesh_points, 'num_mesh_points', 1)

          allocate (cio_stack_file_name(nptgrps),
     &              cio_stack_file_loc(nptgrps),
     &              n_cio_stack_emis_vars(nptgrps),
     &              n_cio_stack_emis_lays(nptgrps),
     &              n_cio_stack_emis_pts(nptgrps),
     &              cio_mpas_stack_emis_timestamp(nptgrps),
     &              stkgname(nptgrps),
     &              d_size(nptgrps),
     &              pt_size(nptgrps),
     &              stk_gp_sdate(nptgrps),
     &              stk_gp_stime(nptgrps),
     &              stk_gp_nlays(nptgrps),
     &              fire_on(nptgrps),
     &              nsrc(nptgrps),
     &              mpas_map(num_mesh_points),
     &              my_mpas_map_index(num_mesh_points),                ! my mesh point
     &              stat=stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating cio_stack_file_name and other arrays'
               call prog_interrupt (pname, 0, 0, xmsg, 1)
          end if

          my_data = .false.
          my_cell_num = -1
          open (unit = 97, file = map_fname, status = 'old')
          my_num_mesh_points = 0
          do n = 1, num_mesh_points
             read (97, *) mpas_map(n) 
             if (mpas_map(n) == mype) then
                my_num_mesh_points = my_num_mesh_points + 1
                my_mpas_map_index(my_num_mesh_points) = n
             end if
          end do
          close (97)

          fire_on = .false.   ! array assignment
! go through all stack group one time to figure out max number of source points
          stkgname = ' '   ! array
          do n = 1, nptgrps
             write( stkgname( n ),'( "STK_GRPS_",I3.3 )' ) n
          end do

          if ( .not. stk_prms_init( stkgname ) ) then
             write (cio_logdev, *) 'Could not initialize stack parameters'
             stop
          end if

          do n = 1, nptgrps

             floc = mio_search(stkgname(n))

!            stk_gp_sdate(n) = mio_file_data(floc)%var_name(ivar)
!            stk_gp_stime(n) = mio_file_data(floc)%var_name(ivar)
             stk_gp_nlays(n) = mio_file_data(floc)%dim_value(3)

             nsrc( n ) = mio_file_data(floc)%dim_value(5)


             do v = 1, mio_file_data(floc)%nvars
                if ( mio_file_data(floc)%var_name(v) .eq. 'ACRESBURNED' ) fire_on( n ) = .true.
             end do
          end do
          max_nsrc_pts = maxval(nsrc)

          allocate (stkid(max_nsrc_pts, nptgrps),
     &              my_nsrc_index(max_nsrc_pts, nptgrps),           ! my source number
     &              my_nsrc_mesh_index(max_nsrc_pts, nptgrps),      ! my source w.r.t. to my mesh point
     &              my_nsrc_pressure(nlays, max_nsrc_pts, nptgrps), ! my source pressure
     &              stat = stat)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating other stack group variable arrays'
             call prog_interrupt (pname, cio_model_sdate, cio_model_stime, xmsg, 1)
          end if

! read in stack group data

          do n = 1, nptgrps

             allocate (tdata_1di(nsrc(n)), stat = stat)

             call mio_fread (stkgname(n), 'ROW', pname, tdata_1di)

             my_nsrc(n) = 0
             do v = 1, nsrc(n)
                pt = index1 (tdata_1di(v), my_num_mesh_points, my_mpas_map_index)

                if (pt .gt. 0) then
                   my_nsrc(n) = my_nsrc(n) + 1
                   my_nsrc_index(my_nsrc(n), n) = v
                   my_nsrc_mesh_index(my_nsrc(n), n) = pt
                end if
             end do

             deallocate (tdata_1di)

          end do

          allocate ( my_stkbuff ( maxval( my_nsrc ) ),
     &               my_colbuff ( maxval( my_nsrc ) ),
     &               my_rowbuff ( maxval( my_nsrc ) ), 
     &               stat=stat )

          my_strt_src = 0
          do n = 1, nptgrps

             if ( my_nsrc( n ) .gt. 0 ) then

                my_strt_src(n) = 1
                my_end_src(n) = my_nsrc(n)

                stkdiam(n)%len = my_nsrc(n)
                stkht(n)%len   = my_nsrc(n)
                stktk(n)%len   = my_nsrc(n)
                stkvel(n)%len  = my_nsrc(n)

                allocate (stkdiam(n)%arry(my_nsrc(n)), 
     &                    stkht(n)%arry(my_nsrc(n)), 
     &                    stktk(n)%arry(my_nsrc(n)), 
     &                    stkvel(n)%arry(my_nsrc(n)), 
     &                    tdata_1dr(nsrc(n)),
     &                    stat=stat )

                if ( fire_on(n) ) then
                   acres_burned(n)%len = my_nsrc(n)
                   allocate (acres_burned(n)%arry(my_nsrc(n)), 
     &                       stat=stat )
                end if

                call mio_fread (stkgname(n), 'STKDM', pname, tdata_1dr)

                do v = 1, my_nsrc(n)
                   stkdiam( n )%arry(v) = tdata_1dr(my_nsrc_index(v, n))
                   my_nsrc_pressure(:,v,n) = g3ddata(my_nsrc_mesh_index(v, n),1,:,pres_ind)
                end do

                call mio_fread (stkgname(n), 'STKHT', pname, tdata_1dr)

                do v = 1, my_nsrc(n)
                   stkht( n )%arry(v) = tdata_1dr(my_nsrc_index(v, n))
                end do

                call mio_fread (stkgname(n), 'STKTK', pname, tdata_1dr)

                do v = 1, my_nsrc(n)
                   stktk( n )%arry(v) = tdata_1dr(my_nsrc_index(v, n))
                end do

                call mio_fread (stkgname(n), 'STKVE', pname, tdata_1dr)

                do v = 1, my_nsrc(n)
                   stkvel( n )%arry(v) = tdata_1dr(my_nsrc_index(v, n))
                end do

                if ( fire_on( n ) ) then
                   call mio_fread (stkgname(n), 'ACRESBURNED', pname, tdata_1dr)

                   do v = 1, my_nsrc(n)
                      acres_burned( n )%arry(v) = tdata_1dr(my_nsrc_index(v, n))
                   end do

                end if

                deallocate (tdata_1dr)
             end if

          end do

! process stack emission files
          max_nvars = 0
          d_size    = 0
          do pt = 1, nptgrps

             write( cio_stack_file_name(pt), '( "STK_EMIS_",I3.3 )' ) pt

             floc = mio_search(cio_stack_file_name(pt))
             cio_stack_file_loc(pt) = floc

             n_cio_stack_emis_vars(pt) = mio_file_data(floc)%nvars
             n_cio_stack_emis_lays(pt) = mio_file_data(floc)%nlays
             n_cio_stack_emis_pts(pt)  = nsrc( pt )

             cio_mpas_stack_emis_timestamp(pt) = mio_file_data(floc)%timestamp(1)

             if (max_nvars .lt. mio_file_data(floc)%nvars) then
                max_nvars = mio_file_data(floc)%nvars
             end if

             if (my_strt_src(pt) .gt. 0) then
                pt_size(pt) = (my_end_src(pt) - my_strt_src(pt) + 1) * n_cio_stack_emis_lays(pt)
                d_size(pt)  = mio_file_data(floc)%nvars * pt_size(pt) * 3
             else
                pt_size(pt) = 0
                d_size(pt)  = 0
             end if

          end do

          allocate (cio_stack_var_name(max_nvars, nptgrps),
     &              head_stack_emis(max_nvars, nptgrps),
     &              tail_stack_emis(max_nvars, nptgrps),
     &              cio_stack_emis_data_inx(2, 0:2, max_nvars, nptgrps),
     &              cio_stack_emis_data_tstamp(2, 0:2, max_nvars, nptgrps),
     &              cio_stack_data(sum(d_size)),
     &              f_stk_emis(NPTGRPS),
     &              stat = stat)
          if (stat .ne. 0) then
               xmsg = 'Failure allocating other stack arrays'
               call prog_interrupt (pname, 0, 0, xmsg, 1)
          end if

          begin = 1
          cio_stack_emis_data_inx = -1
          do pt = 1, nptgrps

             floc = cio_stack_file_loc(pt)

             n_opened_file = n_opened_file + 1
             f_stk_emis(pt) = n_opened_file

! Check whether file is a representative day type
             write (fname, '(a16, i3.3)') "STK_EM_SYM_DATE_", pt
             file_sym_date(f_stk_emis(pt)) = emis_sym_date ! Master switch to change default
             call get_env(file_sym_date(f_stk_emis(pt)), fname,
     &                    file_sym_date(f_stk_emis(pt)), logdev)

             file_sdate(f_stk_emis(pt)) = mio_file_data(floc)%tflag(1,1)
             file_stime(f_stk_emis(pt)) = mio_file_data(floc)%tflag(2,1)
             file_tstep(f_stk_emis(pt)) = mio_file_data(floc)%tstep

             t_nvars = mio_file_data(floc)%nvars

             cio_stack_var_name(1:t_nvars, pt) = mio_file_data(floc)%var_name(1:t_nvars)

             call quicksort(cio_stack_var_name(1:t_nvars,pt), 1, t_nvars)

             if (my_nsrc(pt) .gt. 0) then
                do v = 1, mio_file_data(floc)%nvars
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

        end subroutine stack_files_setup_mpas

! -------------------------------------------------------------------------
        subroutine retrieve_stack_data_mpas (jdate, jtime, fname, vname)

!         USE UTILIO_DEFN
          use stk_prms, only : my_strt_src, my_end_src, my_nsrc, my_nsrc_index
          use stack_group_data_module, only : nsrc
          use util_module, only : NEXTIME
          use centralized_io_util_module, only : binary_search

          include SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: fname, vname

          character( 40 ), parameter :: pname = 'retrieve_stack_data_mpas'

          logical, save :: firstime = .true.
          integer :: stat, i, j, begin, end, buf_loc, iterations,
     &               iter, loc_jdate, loc_jtime, v, beg_v, end_v,
     &               beg_gp, end_gp, gp, fnum
          real, allocatable :: tdata_1dr(:)
          character (20) :: mpas_time_stamp
          character (20), allocatable, save :: mpas_stack_loc_time_stamp(:)

          character( 120 ) :: xmsg = ' '

          if (firstime) then

             allocate (mpas_stack_loc_time_stamp(nptgrps), stat=stat)

             do i = 1, nptgrps
                j = mio_search (cio_stack_file_name(i))
                mpas_stack_loc_time_stamp(i) = mio_file_data(j)%timestamp(1)
             end do

             head_stack_emis = -1
             tail_stack_emis = -1

             iterations = 2
          else
             iterations = 1
          end if

          if (present(vname)) then
             beg_gp = binary_search (fname, cio_stack_file_name, nptgrps)
             end_gp = beg_gp
             beg_v = binary_search (vname, cio_stack_var_name(:,beg_gp), n_cio_stack_emis_vars(beg_gp))
             end_v = beg_v
          else
             beg_gp = 1
             end_gp = nptgrps
          end if

          do gp = beg_gp, end_gp

             allocate (tdata_1dr(nsrc(gp)), stat = stat)

             if (firstime) then
                loc_jdate = jdate
                if (file_sym_date(f_stk_emis(gp))) loc_jdate = file_sdate(f_stk_emis(gp)) ! Representative day check
                loc_jtime = jtime
             else
                loc_jdate = jdate
                loc_jtime = jtime
             end if

             if (.not. present(vname)) then
                beg_v = 1
                end_v = n_cio_stack_emis_vars(gp)
             end if

! cio_stack_emis_data_inx

             do iter = 1, iterations

                call mio_time_format_conversion (loc_jdate, loc_jtime, mpas_time_stamp)

                do v = beg_v, end_v
                   buf_loc = mod((tail_stack_emis(v, gp) + iter), 2)

                   cio_stack_emis_data_tstamp(1, buf_loc, v, gp) = loc_jdate
                   cio_stack_emis_data_tstamp(2, buf_loc, v, gp) = loc_jtime

                   begin = cio_stack_emis_data_inx(1, buf_loc, v, gp)
                   end   = cio_stack_emis_data_inx(2, buf_loc, v, gp)

                   if ((begin .gt. 0) .and. (my_nsrc(gp) .gt. 0)) then

                      call mio_fread (cio_stack_file_name(gp),
     &                                cio_stack_var_name(v, gp), 
     &                                pname,
     &                                tdata_1dr,
     &                                mpas_time_stamp)

                      do i = 1, my_nsrc(gp)
                         cio_stack_data(begin+i-1) = tdata_1dr(my_nsrc_index(i, gp))
                      end do
                   end if
                end do

                call nextime ( loc_jdate, loc_jtime, file_tstep(f_stk_emis(gp)) )

             end do  ! end iter

             deallocate (tdata_1dr)

          end do

          if (firstime) then
             firstime = .false.
             head_stack_emis = 0
             tail_stack_emis = 1
          else
             do gp = beg_gp, end_gp
                do v = beg_v, end_v
                   head_stack_emis(v, gp) = mod(head_stack_emis(v, gp)+1, 2)
                   tail_stack_emis(v, gp) = mod(tail_stack_emis(v, gp)+1, 2)
                end do
             end do
          end if

        end subroutine retrieve_stack_data_mpas

! -------------------------------------------------------------------------
        subroutine retrieve_ocean_data_mpas

          USE HGRD_DEFN
          USE mio_module, only : mio_search
                                              
          character (30), parameter :: pname = 'retrieve_ocean_data_mpas'
          character (20)   :: ocean_file = 'OCEAN_1'
          character (120)  :: xmsg = ' '
          character (1000) :: fname
          integer          :: floc, stat
          logical          :: exist

          call get_env (fname, ocean_file, ' ')
          inquire (file=fname, exist=exist)

          allocate (ocean(ncols, nrows), 
     &              szone(ncols, nrows),   
     &              chlr(ncols, nrows),
     &              dmsl(ncols, nrows),                
     &              STAT=STAT)
          if (stat .ne. 0) then
             xmsg = 'Failure allocating OPEN, SURF, CHLO, DMS array'
             call M3EXIT (PNAME, 0, 0, XMSG, 1)
          end if

          if (ocean_chem) then

! if OCEAN file does not exist, g2ddata with open_ind and surf_ind have
! been setup in subroutne mpas_cmaq_coupler, mpas_atmchem_interface.F
             if (exist) then
                floc = mio_search (ocean_file)

                call mio_fread (ocean_file,
     &                          'OPEN',
     &                          pname,
     &                          g2ddata(:, 1, open_ind),
     &                          mio_file_data(floc)%timestamp(1))

                call mio_fread (ocean_file,
     &                          'SURF',
     &                          pname,
     &                          g2ddata(:, 1, surf_ind),
     &                          mio_file_data(floc)%timestamp(1))

                call mio_fread (ocean_file,
     &                          'CHLO',
     &                          pname,
     &                          g2ddata(:, 1, chlo_ind),
     &                          mio_file_data(floc)%timestamp(1))

                call mio_fread (ocean_file,
     &                          'DMS',
     &                          pname,
     &                          g2ddata(:, 1, dms_ind),
     &                          mio_file_data(floc)%timestamp(1))
             end if

             ocean(:,1) = g2ddata(:,1,open_ind)
             szone(:,1) = g2ddata(:,1,surf_ind)
             dmsl(:,1)  = g2ddata(:,1,dms_ind)
             chlr(:,1)  = g2ddata(:,1,chlo_ind)
          else
             ocean = 0.0
             szone = 0.0          
             dmsl  = 0.0          
             chlr  = 0.0
          end if

        end subroutine retrieve_ocean_data_mpas

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_1d_mpas (vname, date, time, data)

          use hgrd_defn, only : ncols, nrows
          use vgrd_defn, only : nlays
          use centralized_io_util_module, only : binary_search

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:)

          integer :: var_loc
          character (40) :: msg

          var_loc = binary_search (vname, vname_2d, n2d_data)

          if (var_loc .gt. 0) then
             data = g2ddata(:,1,var_loc)
          else
             write (msg, *) ' Error: Cannot find species ', trim(vname)
             call prog_interrupt ( 'interpolate_var', 0, 0, msg, 1)
          end if

        end subroutine r_interpolate_var_1d_mpas

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_1ds_mpas (fname, vname, date, time, data)

          use stk_prms, only : my_strt_src, my_end_src, my_nsrc
          use util_module, only : nextime , secsdiff
          use centralized_io_util_module, only : binary_search
          use util_module, only : time2sec

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:)

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,
     &               loc_jdate, loc_jtime, dsize, pt, loc_tstep
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: prev_tail_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2
          character(200) :: xmsg

          pt = binary_search (fname, cio_stack_file_name, NPTGRPS)

          var_loc = binary_search (vname, cio_stack_var_name(:,pt), n_cio_stack_emis_vars(pt))

          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in a stack file.'
             write (xmsg, '(A9,A,A,A)' ) 'ERROR: ',trim(vname), ' is not available ',
     &             'on a Stack Emisison file. Simulation will now terminate.'
             call M3EXIT ( 'Centralized I/O Module', date, time, xmsg, 1 )
          else
             dsize = my_nsrc(pt)

             loc_tstep = file_tstep(f_stk_emis(pt)) 

             loc_head = head_stack_emis(var_loc, pt)
             loc_tail = tail_stack_emis(var_loc, pt)

             if ((cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .lt. date) .or.
     &           ((cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .eq. date) .and.
     &            (cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .eq. 0)) .or.
     &           ((cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .lt. time) .and.
     &            (cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .eq. date))) then

                loc_jdate = cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt)
                loc_jtime = cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) 
                CALL NEXTIME ( loc_jdate, loc_jtime, loc_tstep )
                call retrieve_stack_data_mpas (loc_jdate, loc_jtime, fname, vname)
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
     &              (prev_head_time .ne. cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt)) .or.
     &              (prev_tail_time .ne. cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt))) then

                   if (cio_stack_emis_data_tstamp(1, loc_head, var_loc, pt) .eq. date) then
                      ratio2 =   real(secsdiff(time, cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(secsdiff(240000, cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt)
                   prev_tail_time = cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt)
                   
                   if ( (ratio1 .lt. 0) .or. (ratio2 .lt. 0) 
     &             .or. (ratio1 .gt. 1) .or. (ratio2 .gt. 1)) then
                      write(logdev,'(5X,a,a)'),
     &               'ERROR: Incorrect Interpolation in 1-D Stack Interpolation for variable: ',
     &                trim(vname)
                     
                     write(logdev,'(5X,a,i7,a,i6)'), 
     &               'Requested TIME & DATE: ',date,':',time
                     
                     write(logdev,'(5X,a,i7,a,i6,a,i7,a,i6)'),
     &               'Interpolation Bounds ',cio_stack_emis_data_tstamp(1,0,var_loc,pt),
     &               ':',cio_stack_emis_data_tstamp(2,0,var_loc,pt),' to ',
     &                cio_stack_emis_data_tstamp(1,1,var_loc,pt),':',cio_stack_emis_data_tstamp(2,1,var_loc,pt)
                     call M3EXIT( 'Centralized I/O',date,time,'',1 )
                      write(logdev,'(5X,a)'),
     &                'ERROR: Program EXIT in subroutine r_interpolate_var_1ds in module centralized io'
                   end if 
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

        end subroutine r_interpolate_var_1ds_mpas

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_2d_mpas (vname, date, time, data,
     &                                        scol, ecol, srow, erow, slay)

          use hgrd_defn, only : ncols, nrows
          use vgrd_defn, only : nlays
          use centralized_io_util_module, only : binary_search

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:,:)
          integer, intent(in), optional :: scol, ecol, srow, erow, slay

          integer :: var_loc
          character (40) :: msg

          var_loc = binary_search (vname, vname_2d, n2d_data)

          if (var_loc .gt. 0) then
             data = g2ddata(:,:,var_loc)
          else
             write (msg, *) ' Error: Cannot find species ', trim(vname)
             call prog_interrupt ( 'interpolate_var', 0, 0, msg, 1)
          end if

        end subroutine r_interpolate_var_2d_mpas

! -------------------------------------------------------------------------
        subroutine i_interpolate_var_2d_mpas (vname, date, time, data)

          use hgrd_defn, only : ncols, nrows
          use vgrd_defn, only : nlays
          use centralized_io_util_module, only : binary_search

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          integer, intent(out)      :: data(:,:)

          integer :: var_loc
          character (40) :: msg

          var_loc = binary_search (vname, vname_2d, n2d_data)

          if (var_loc .gt. 0) then
             data = g2ddata(:,:,var_loc)
          else
             write (msg, *) ' Error: Cannot find species ', trim(vname)
             call prog_interrupt ( 'interpolate_var', 0, 0, msg, 1)
          end if

        end subroutine i_interpolate_var_2d_mpas

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_2dx_mpas (vname, date, time, data, flag)

          use hgrd_defn, only : ncols, nrows
          use vgrd_defn, only : nlays
          use centralized_io_util_module, only : binary_search

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          logical, intent(in)       :: flag
          real, intent(out)         :: data(:,:)

          integer :: var_loc
          character (40) :: msg

          var_loc = binary_search (vname, vname_2d, n2d_data)

          if (var_loc .gt. 0) then
             data = g2ddata(:,:,var_loc)
          else
             write (msg, *) ' Error: Cannot find species ', trim(vname)
             call prog_interrupt ( 'interpolate_var', 0, 0, msg, 1)
          end if

        end subroutine r_interpolate_var_2dx_mpas

! -------------------------------------------------------------------------
        subroutine r_interpolate_var_3d_mpas (vname, date, time, data, fname)

          use hgrd_defn, only : ncols, nrows
          use util_module, only : nextime
          use centralized_io_util_module, only : binary_search, time_diff
          use util_module, only : time2sec

          character (*), intent(in) :: vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:,:,:)
          character (*), intent(in), optional :: fname

          integer :: var_loc, slen, loc_head, loc_tail, 
     &               loc_jdate, loc_jtime, beg_k, end_k,
     &               m, k, r, c,
     &               head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind, loc_tstep, fnum
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2
          character (40) :: msg, loc_vname
          character (20) :: loc_mpas_time_stamp

          if (present(fname)) then
             slen = len_trim(fname)
             loc_vname = trim(vname) // fname(slen-3:slen)
          else
             loc_vname = vname
          end if

          var_loc = binary_search (loc_vname, vname_3d, n3d_data)

          if (var_loc .gt. 0) then
             data = g3ddata(:,:,:,var_loc)
          else

             var_loc = binary_search (loc_vname, cio_grid_var_name(:,1), n_cio_grid_vars)

             if (var_loc .lt. 0) then
                write (msg, *) ' Error: Cannot find species ', trim(vname)
                call prog_interrupt ( 'interpolate_var', 0, 0, msg, 1)
             else
                loc_head = head_grid(var_loc)
                loc_tail = tail_grid(var_loc)

                if (cio_grid_var_name(var_loc,3) == 'm') then
                   loc_tstep = file_tstep(f_met) 
                else if ((cio_grid_var_name(var_loc,2) == 'e2d') .or.
     &              (cio_grid_var_name(var_loc,2) == 'e3d')) then

                   slen = len_trim(cio_grid_var_name(var_loc,1))
                   read (cio_grid_var_name(var_loc,1)(slen-2:slen), *) fnum

                   loc_tstep = file_tstep(f_emis(fnum)) 
                else if (cio_grid_var_name(var_loc,2) == 'lnt') then
                   loc_tstep = file_tstep(f_ltng)
                else if (cio_grid_var_name(var_loc,2) == 'ic') then
                   loc_tstep = file_tstep(f_icon)
                else if (cio_grid_var_name(var_loc,2) == 'bct') then
                   loc_tstep = file_tstep(f_bcon)
                else if (cio_grid_var_name(var_loc,2) == 'is') then
                   loc_tstep = file_tstep(f_is_icon)
                end if

                call mio_time_format_conversion (date, time, loc_mpas_time_stamp)

                if (cio_mpas_grid_data_tstamp(loc_tail, var_loc) .lt. loc_mpas_time_stamp) then

                   call mio_time_format_conversion (cio_mpas_grid_data_tstamp(loc_tail, var_loc), loc_jdate, loc_jtime)

                   call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, loc_vname)
                   loc_head = head_grid(var_loc)
                   loc_tail = tail_grid(var_loc)
                end if

                if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &              (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                   count = count + 1
                else

                   cio_grid_data_tstamp(1, 2, var_loc) = date
                   cio_grid_data_tstamp(2, 2, var_loc) = time

                   if ((prev_time .ne. time) .or.
     &                 (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc))) then
                      if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                         ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc)))
     &                            / real(time2sec(loc_tstep))
                         ratio1 = 1.0 - ratio2
                      else
                         ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc)))
     &                            / real(time2sec(loc_tstep))
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
     &                                                          + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2

                end if

                beg_k = 1
                if (cio_grid_var_name(var_loc, 2) .eq. 'e2d') then
                   end_k = 1
                else
                   end_k = size(data,3)
                end if

                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                m = store_beg_ind - 1
                do k = beg_k, end_k
                   do r = 1, size(data,2)
                      do c = 1, size(data,1)
                         m = m + 1
                         data(c,r,k) = cio_grid_data(m)
                      end do
                   end do
                end do

             end if
          end if

        end subroutine r_interpolate_var_3d_mpas

#else

! -------------------------------------------------------------------------
        subroutine boundary_files_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          USE VGRD_DEFN, only : VGTYP_GD, nlays
          use centralized_io_util_module, only : quicksort

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'boundary_files_setup'

          CHARACTER( 120 ) :: XMSG = ' '
          INTEGER          :: GXOFF, GYOFF, stat, n, v, d_size, begin, end

          character( 16 ), allocatable :: b3d_name(:,:)
          character( 16 ) :: mb3d_name(2, 2)

! MET_BDY_3D file, need to be opened when window is F
          if (.not. window) then
#ifndef twoway
             IF ( .NOT. OPEN3( MET_BDY_3D, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// MET_BDY_3D // ' file'
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
             IF ( .NOT. DESC3( MET_BDY_3D ) ) THEN
                XMSG = 'Could not get file description from '// MET_BDY_3D
                CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
#endif
             n_mb3d = 2
             mb3d_name = 'mb'     ! denote met 3D boundary variable
             mb3d_name(1,1) = 'DENSA_J'
             mb3d_name(2,1) = 'JACOBM'
          else
             n_mb3d = 0
          end if

! BCON file
          IF ( .NOT. OPEN3( BCFILE, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open '// BCFILE // ' file'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          n_opened_file = n_opened_file + 1
          f_bcon = n_opened_file
          IF ( .NOT. DESC3( BCFILE ) ) THEN
             XMSG = 'Could not get description of file  '// BCFILE 
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          file_sdate(f_bcon) = sdate3d
          file_stime(f_bcon) = stime3d
          file_tstep(f_bcon) = tstep3d
          file_xcell(f_bcon) = xcell3d
          file_ycell(f_bcon) = ycell3d

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

          if (tstep3d == 0) then
             b3d_name = 'bc'      ! denote time independent 3D boundary variable
          else
             b3d_name = 'bct'     ! denote time dependent   3D boundary variable
          end if

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
! this is for checking purposes
!            write (logdev, '(a13, i5, 1x, a16, a4, 10i10)') ' ==d== bfile ', v,
!    &                cio_bndy_var_name(v,:), cio_bndy_data_inx(:,:,v)
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

          Character( 32 )  :: fname
          CHARACTER( 120 ) :: XMSG = ' '
          integer :: n, v, pt, max_nsrc_pts, max_nvars, begin, end, stat, delta
          integer, allocatable :: d_size(:), pt_size(:),
     &                            stk_gp_sdate(:), stk_gp_stime(:),
     &                            stk_gp_nlays(:)

          integer :: ldate, ltime, t
          logical :: found, done

          allocate (cio_stack_file_name(NPTGRPS), 
     &              n_cio_stack_emis_vars(NPTGRPS), 
     &              n_cio_stack_emis_lays(NPTGRPS), 
     &              n_cio_stack_emis_pts(NPTGRPS),
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
             n_opened_file = n_opened_file + 1

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
     &              f_stk_emis(NPTGRPS),
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
             xmsg = 'Could not initialize stack parameters'
             call m3exit( 'Stack Files Setup', 0, 0, xmsg, 2 )
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
             n_opened_file = n_opened_file + 1
             f_stk_emis(pt) = n_opened_file

             IF ( .NOT. DESC3( cio_stack_file_name( pt ) ) ) THEN
                XMSG = 'Could not get ' // TRIM( cio_stack_file_name( pt ) ) // ' file description'
                CALL M3MESG( XMSG )
             END IF

             n_cio_stack_emis_vars(pt) = nvars3d
             n_cio_stack_emis_lays(pt) = nlays3d
             n_cio_stack_emis_pts(pt)  = nrows3d

             file_sdate(f_stk_emis(pt)) = sdate3d
             file_stime(f_stk_emis(pt)) = stime3d
             file_tstep(f_stk_emis(pt)) = tstep3d
             file_xcell(f_stk_emis(pt)) = xcell3d
             file_ycell(f_stk_emis(pt)) = ycell3d

! Check whether file is a representative day type
             write (fname, '(a16, i3.3)') "STK_EM_SYM_DATE_", pt 
             file_sym_date(f_stk_emis(pt)) = emis_sym_date ! Master switch to change default 
             call get_env(file_sym_date(f_stk_emis(pt)), fname, 
     &                    file_sym_date(f_stk_emis(pt)), logdev)             

             found = .false.
             ldate = sdate3d
             ltime = stime3d
             if ((ldate == stdate) .and. (mxrec3d > 1)) then
                found = .true.
             else
                t = 1
                do while ((t < mxrec3d) .and. (.not. found))
                   call nextime (ldate, ltime, tstep3d)
                   if (ldate == stdate) then
                      found = .true.
                   end if
                   t = t + 1
                end do
             end if

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
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

          ALLOCATE( AVGEMIS( NCOLS,NROWS,NSEF-1,NSEASONS ),
     &              STAT=STAT )

          IF ( .NOT. OPEN3( biogemis_fname, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open ' // trim(biogemis_fname) // ' file'
             CALL M3MESG( XMSG )
          END IF
          n_opened_file = n_opened_file + 1

          IF ( .NOT. DESC3( biogemis_fname ) ) THEN
             XMSG = 'Could not get ' // trim(biogemis_fname) // ' file description'
             CALL M3MESG( XMSG )
          END IF

          call subhfile ( biogemis_fname, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

C Read the various categories of normalized emissions
          DO I = 1, NSEASONS

             DO J = 1, NSEF-1
                VAR = 'AVG_' // TRIM( BIOTYPES( J ) ) // SEASON( I )

                IF ( .NOT. XTRACT3( biogemis_fname, VAR,
     &                              1,1, startrow, endrow, startcol, endcol,
     &                              0, 0, AVGEMIS( :,:,J,I ) ) ) THEN
                   MESG = 'Could not read "' // TRIM( VAR ) //
     &                    '" from file "' // TRIM( biogemis_fname ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF
             END DO

          END DO   ! end loop over seasons

        end subroutine biogemis_setup

! -------------------------------------------------------------------------
        subroutine beis_norm_emis_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows

          Character( 40 ), parameter :: pname = 'beis_norm_emis_setup'
          Character( 40 ), parameter :: fname = 'BEIS_NORM_EMIS'

          CHARACTER( 256 ) :: MESG
          CHARACTER( 16 ) :: VAR
          INTEGER :: STAT
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

          ALLOCATE( GROWAGNO( NCOLS,NROWS ),
     &              NGROWAGNO( NCOLS,NROWS ),
     &              NONAGNO( NCOLS,NROWS ),
     &              STAT=STAT )

          IF ( .NOT. OPEN3( fname, FSREAD3, PNAME ) ) THEN
             MESG = 'Could not open ' // trim(fname) // ' file '
             CALL M3MESG( MESG )
          END IF
          n_opened_file = n_opened_file + 1

          call subhfile ( fname, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

          VAR = 'AVG_NOAG_GROW'
          IF ( .NOT. XTRACT3( fname, VAR,
     &                        1,1, startrow, endrow, startcol, endcol,
     &                        0, 0, GROWAGNO ) ) THEN
             MESG = 'Could not read "' // TRIM( VAR ) //
     &              '" from file "' // TRIM( fname ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

          VAR = 'AVG_NOAG_NONGROWNB3'
          IF ( .NOT. XTRACT3( fname, VAR,
     &                        1,1, startrow, endrow, startcol, endcol,
     &                        0, 0, NGROWAGNO ) ) THEN
             MESG = 'Could not read "' // TRIM( VAR ) //
     &              '" from file "' // TRIM( fname ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

          VAR = 'AVG_NONONAG'
          IF ( .NOT. XTRACT3( fname, VAR,
     &                        1,1, startrow, endrow, startcol, endcol,
     &                        0, 0, NONAGNO ) ) THEN
             MESG = 'Could not read "' // TRIM( VAR ) //
     &              '" from file "' // TRIM( fname ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

        end subroutine beis_norm_emis_setup

! -------------------------------------------------------------------------
        subroutine depv_data_setup

          USE UTILIO_DEFN
          use HGRD_DEFN, only : ncols, nrows
          use depv_data_module
!         use util_module, only : index1

          INCLUDE SUBST_FILES_ID  ! file name parameters

          Character( 40 ), parameter :: pname = 'depv_data_setup'

          CHARACTER( 256 ) :: MESG
          CHARACTER( 16 ) :: vname
          INTEGER :: STAT, i, j, k, jdate_yest
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

          Allocate ( Beld_ag ( ncols, nrows, e2c_cats ),
     &               pHs1 ( ncols, nrows, e2c_cats ),      ! for E2C_SOIL file
     &               pHs2 ( ncols, nrows, e2c_cats ),      ! for E2C_SOIL file
     &               NH4ps1 ( ncols, nrows, e2c_cats ),    ! for E2C_CHEM file
     &               NH4ps2 ( ncols, nrows, e2c_cats ),    ! for E2C_CHEM file
     &               STAT=STAT )

          IF ( .NOT. OPEN3( E2C_LU, FSREAD3, PNAME ) ) THEN
             mesg = 'Could not open ' // trim(E2C_LU) // ' file'
             CALL M3MESG( mesg )
          END IF
          n_opened_file = n_opened_file + 1

          call subhfile ( E2C_LU, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

          Do k = 1, e2c_cats
             vname = BELD_Names(k)
             IF ( .NOT. XTRACT3( E2C_LU, vname,
     &                            1, 1, startrow, endrow, startcol, endcol,
     &                            0, 0, Beld_ag( :,:,k ) ) ) Then
                MESG = 'Could not read "' // TRIM( vname ) //
     &                 '" from file "' // TRIM( E2C_LU ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If
          End Do

! for E2C_SOIL file
          If ( .Not. Open3( E2C_SOIL, fsread3, pname ) ) Then
            mesg = 'Could not open '// E2C_SOIL // ' file'
            Call M3exit ( pname, 0, 0, mesg, xstat1 )
          End If
          n_opened_file = n_opened_file + 1

          call subhfile ( E2C_SOIL, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

          vname = 'L1_PH'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, pHs1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_PH'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, pHs2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

#ifdef m3dry_opt
          Allocate ( por1 ( ncols, nrows, e2c_cats ),
     &               por2 ( ncols, nrows, e2c_cats ),
     &               wp1  ( ncols, nrows, e2c_cats ),
     &               wp2  ( ncols, nrows, e2c_cats ),
     &               cec1 ( ncols, nrows, e2c_cats ),
     &               cec2 ( ncols, nrows, e2c_cats ),
     &               STAT=STAT )

          vname = 'L1_Porosity'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, por1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_Porosity'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, por2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_Wilt_P'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, wp1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_Wilt_P'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, wp2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_Cation'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, cec1 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If
 
          vname = 'L2_Cation'
          If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, startrow, endrow,
     &                         startcol, endcol, 0, 0, cec2 ) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_SOIL ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If
#endif
 
! for E2C_CHEM file
          If ( .Not. Open3( E2C_CHEM, fsread3, pname ) ) Then
             mesg = 'Could not open '// E2C_CHEM // ' file'
             Call M3exit ( pname, 0, 0, mesg, xstat1 )
          End If
          n_opened_file = n_opened_file + 1

          IF ( .NOT. DESC3( E2C_CHEM ) ) THEN
             MESG = 'Could not get description of file "' //
     &               TRIM( E2C_CHEM ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          END IF

          call subhfile ( E2C_CHEM, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

          GMN_AVAIL = .false.
          if (index1 ('GMN', nvars3d, vname3d) .gt. 0) then
             GMN_AVAIL = .true.          
          end if

          vname = 'L1_NH3'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, NH4ps1) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_CHEM ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_NH3'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, NH4ps2) ) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_CHEM ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

#ifdef m3dry_opt

          Allocate ( wep1 ( ncols, nrows, e2c_cats ),
     &               wep2 ( ncols, nrows, e2c_cats ),
     &               dep2 ( ncols, nrows, e2c_cats ),
     &               STAT=STAT )

          vname = 'L1_SW'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, wep1)) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_CHEM ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_SW'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, wep2)) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_CHEM ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_DEP'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, dep2)) Then
             MESG = 'Could not read "' // TRIM( vname ) //
     &              '" from file "' // TRIM( E2C_CHEM ) // '"'
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If
#else
#ifdef stage_opt
          Allocate( Nit1       ( ncols,nrows,e2c_cats ),
     &              Nit2       ( ncols,nrows,e2c_cats ),
     &              L1_ON      ( ncols,nrows,e2c_cats ),
     &              L2_ON      ( ncols,nrows,e2c_cats ),
     &              BDc1       ( ncols,nrows,e2c_cats ),
     &              BDc2       ( ncols,nrows,e2c_cats ),
     &              STAT=STAT )

          vname = 'L1_NITR'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, Nit1 ) ) Then
             Write( mesg,9001 ) vname, E2C_CHEM
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_NITR'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, Nit2 ) ) Then
             Write( mesg,9001 ) vname, E2C_CHEM
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_ON'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, L1_ON ) ) Then
             Write( mesg,9001 ) vname, E2C_CHEM
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_ON'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, L2_ON ) ) Then
             Write( mesg,9001 ) vname, E2C_CHEM
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L1_BD'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, BDc1 ) ) Then
             Write( mesg,9001 ) vname, E2C_CHEM
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          vname = 'L2_BD'
          If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, BDc2 ) ) Then
             Write( mesg,9001 ) vname, E2C_CHEM
             CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
          End If

          If ( GMN_AVAIL ) Then ! Using Fest-C 1.4 output
             Allocate( GMN     ( ncols,nrows,e2c_cats ), STAT = STAT )
             If ( STAT .Ne. 0 ) Then
                mesg = 'Failure allocating GMN'
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If

             vname = 'GMN'
             If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                        startcol, endcol, cio_model_sdate, 0, GMN ) ) Then
                Write( mesg,9001 ) vname, E2C_CHEM
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If

          End If

          Allocate( gamma1     ( ncols,nrows ),
     &              gamma2     ( ncols,nrows ),
     &              F1_NH4     ( ncols,nrows,e2c_cats ),
     &              F2_NH4     ( ncols,nrows,e2c_cats ),
     &              STAT=STAT )

          if ( MEDC_AVAIL ) then
             call subhfile ( INIT_MEDC_1, gxoff, gyoff,
     &                       startcol, endcol, startrow, endrow )

             vname = 'Gamma1'
             If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, startrow, endrow,
     &                            startcol, endcol, cio_model_sdate, 0, gamma1 ) ) Then
                Write( mesg,9001 ) vname, INIT_MEDC_1
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If

             vname = 'Gamma2'
             If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, startrow, endrow,
     &                            startcol, endcol, cio_model_sdate, 0, gamma2 ) ) Then
                Write( mesg,9001 ) vname, INIT_MEDC_1
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If

             vname = 'L1_ANH3'
             If ( .Not. Xtract3 ( E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                            startcol, endcol, cio_model_sdate, 0, F1_NH4 ) ) Then
                Write( mesg,9001 ) vname, E2C_CHEM
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If

             vname = 'L2_ANH3'
             If ( .Not. Xtract3 ( E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                            startcol, endcol, cio_model_sdate, 0, F2_NH4 ) ) Then
                Write( mesg,9001 ) vname, E2C_CHEM
                CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
             End If

             If( .not. GMN_AVAIL ) Then

                Allocate( L1_ON_Yest ( ncols,nrows,e2c_cats ),
     &                    L2_ON_Yest ( ncols,nrows,e2c_cats ),
     &                    F1_ON      ( ncols,nrows,e2c_cats ),
     &                    F2_ON      ( ncols,nrows,e2c_cats ),
     &                    STAT = STAT )
                If ( STAT .Ne. 0 ) Then
                   mesg = 'Failure allocating organic N vars'
                   Call M3EXIT( PNAME, 0, 0, mesg, XSTAT1 )
                End If

                If( MOD(cio_model_sdate,1000) .Eq. 1 ) Then
                   If( MOD(cio_model_sdate,4000) .Eq. 0 .And. 
     &                 MOD(cio_model_sdate,100000) .Ne. 0 ) Then
                      jdate_yest = (INT(cio_model_sdate/1000)-1)*1000+366
                   Else If( MOD(cio_model_sdate,400000) .Eq. 0) Then
                      jdate_yest = (INT(cio_model_sdate/1000)-1)*1000+366
                   Else ! not a leap year
                      jdate_yest = (INT(cio_model_sdate/1000)-1)*1000+365
                   End If
                Else
                   jdate_yest = cio_model_sdate-1
                End If

                If ( .Not. Open3( E2C_CHEM_YEST, fsread3, pname ) ) Then
                   mesg = 'Could not open '// E2C_CHEM_YEST // ' file'
                   Call M3exit ( pname, 0, 0, mesg, xstat1 )
                End If
                n_opened_file = n_opened_file + 1

                IF ( .NOT. DESC3( E2C_CHEM_YEST ) ) THEN
                   MESG = 'Could not get description of file "' //
     &                     TRIM( E2C_CHEM_YEST ) // '"'
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                END IF

                call subhfile ( E2C_CHEM, gxoff, gyoff,
     &                          startcol, endcol, startrow, endrow )

                vname = 'L1_AON'
                If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                              startcol, endcol, cio_model_sdate, 0, F1_ON ) ) Then
                   Write( mesg,9001 ) vname, E2C_CHEM
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                End If

                vname = 'L2_AON'
                If ( .Not. Xtract3 (E2C_CHEM, vname, 1, e2c_cats, startrow, endrow,
     &                              startcol, endcol, cio_model_sdate, 0, F2_ON ) ) Then
                   Write( mesg,9001 ) vname, E2C_CHEM
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                End If

                call subhfile ( E2C_CHEM_YEST, gxoff, gyoff,
     &                          startcol, endcol, startrow, endrow )

                vname = 'L1_ON'
                If ( .Not. Xtract3 (E2C_CHEM_YEST, vname, 1, e2c_cats, startrow, endrow,
     &                              startcol, endcol, jdate_yest, 0, L1_ON_Yest ) ) Then
                   Write( mesg,9001 ) vname, E2C_CHEM
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                End If

                vname = 'L2_ON'
                If ( .Not. Xtract3 (E2C_CHEM_YEST, vname, 1, e2c_cats, startrow, endrow,
     &                              startcol, endcol, jdate_yest, 0, L2_ON_Yest ) ) Then
                   Write( mesg,9001 ) vname, E2C_CHEM
                   CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
                End If
             end if    ! .not. GMN_AVAIL
          end if   ! MEDC_AVAIL

9001      Format( 'Failure reading ', a, 1x, 'from ', a )

#endif   ! end if stage option
#endif   ! end if m3dry option

        end subroutine depv_data_setup

! -------------------------------------------------------------------------
        subroutine medc_file_setup

          USE UTILIO_DEFN
          use bidi_mod

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'medc_file_setup'

          CHARACTER( 256 ) :: xmsg
          integer :: v
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

          CALL INIT_BIDI( )

          IF ( .NOT. OPEN3( INIT_MEDC_1, FSREAD3, PNAME ) ) THEN
             XMSG = 'Open failure for ' // INIT_MEDC_1
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
          n_opened_file = n_opened_file + 1

          call subhfile ( INIT_MEDC_1, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

          DO v = 1, Hg_TOT
            IF ( .NOT. Xtract3( INIT_MEDC_1, MEDIA_NAMES( V ), 1, 1, 
     &                          startrow, endrow, startcol, endcol,
     &                          cio_model_sdate, 0, CMEDIA(:,:,v) ) )THEN
               xmsg = 'Could not read ' // trim( MEDIA_NAMES( V ) )
     &                // ' from ' // trim( INIT_MEDC_1 )
             call m3exit( pname, cio_model_sdate, 0, xmsg, xstat1 )
            END IF
          END DO

        end subroutine medc_file_setup

! -------------------------------------------------------------------------
        subroutine retrieve_grid_cro_2d_data

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE LSM_Mod, ONLY: n_lufrac, init_lsm

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_grid_cro_2d_data'
          integer :: gxoff, gyoff, 
     &               STRTCOLGC2,  ENDCOLGC2,  STRTROWGC2,  ENDROWGC2

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

          CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                    STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

#ifdef twoway
          IF ( .NOT. INTERPX( GRID_CRO_2D, 'MSFX2', PNAME,
     &                        STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1,
     &                        0, 0, MSFX2 ) ) THEN
             XMSG = ' Error interpolating variable MSFX2 from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. INTERPX( GRID_CRO_2D, 'LWMASK', PNAME,
     &                        STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1, 
     &                        0, 0, LWMASK ) ) THEN
             XMSG = ' Error interpolating variable LWMASK from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. INTERPX( GRID_CRO_2D, 'HT', PNAME,
     &                        STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1 ,
     &                        0, 0, HT ) ) THEN
             XMSG = ' Error interpolating variable HT from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. INTERPX( GRID_CRO_2D, 'LAT', PNAME, 
     &                        STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1 ,
     &                        0, 0, LAT ) ) THEN
             XMSG = ' Error interpolating variable LAT from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          IF ( .NOT. INTERPX( GRID_CRO_2D, 'LON', PNAME,
     &                        STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1 ,
     &                        0, 0, LON ) ) THEN
             XMSG = ' Error interpolating variable LON from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF

          if (minkz) then
             IF ( .NOT. INTERPX( GRID_CRO_2D, 'PURB', PNAME,
     &                           STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1 ,
     &                           0, 0, PURB ) ) THEN
                XMSG = ' Error interpolating variable PURB from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
          else
             purb = 0.0
          end if

          IF ( .NOT. LUCRO_AVAIL ) THEN

             CALL INIT_LSM( 0, 0 )

             allocate (LUFRAC(ncols, nrows, n_lufrac), STAT=STAT)
             IF ( STAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating LUFRAC array'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
             END IF

             DO l = 1, n_lufrac
                Write( vname,'( "LUFRAC_",I2.2 )' ) l
                IF ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                              STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2, 1, 1,
     &                              0, 0, LUFRAC( :,:,l ) ) ) THEN
                   XMSG = 'Error interpolating variable' // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
                   Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                END IF
             END DO

          END IF

#else
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

          if (minkz) then
             IF ( .NOT. XTRACT3( GRID_CRO_2D, 'PURB',
     &                           1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                           0, 0, PURB ) ) THEN
                XMSG = ' Error interpolating variable PURB from ' // GRID_CRO_2D
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
          else
             purb = 0.0
          end if

          IF ( .NOT. LUCRO_AVAIL ) THEN

             CALL INIT_LSM( 0, 0 )

             allocate (LUFRAC(ncols, nrows, n_lufrac), STAT=STAT)
             IF ( STAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating LUFRAC array'
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
             END IF

             DO l = 1, n_lufrac
                Write( vname,'( "LUFRAC_",I2.2 )' ) l
                IF ( .Not. XTRACT3( GRID_CRO_2D, VNAME,
     &                              1, 1, STRTROWGC2, ENDROWGC2, STRTCOLGC2, ENDCOLGC2, 
     &                              0, 0, LUFRAC( :,:,l ) ) ) THEN
                   XMSG = 'Error interpolating variable' // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
                   Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                END IF
             END DO

          END IF
#endif
        end subroutine retrieve_grid_cro_2d_data

! -------------------------------------------------------------------------
        subroutine retrieve_grid_dot_2d_data

          USE UTILIO_DEFN
          USE HGRD_DEFN

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_grid_dot_2d_data'

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '
          INTEGER          :: gxoff, gyoff, 
     &                        STRTCOLGD2, ENDCOLGD2, STRTROWGD2, ENDROWGD2

          ALLOCATE ( MSFD2( NCOLS+1, NROWS+1 ), STAT = STAT )
          IF ( STAT .NE. 0 ) THEN
             XMSG = 'Failure allocating MSFD2 array'
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          CALL SUBHFILE ( GRID_DOT_2D, GXOFF, GYOFF,
     &                    STRTCOLGD2, ENDCOLGD2, STRTROWGD2, ENDROWGD2 )

#ifdef twoway
          IF ( .NOT. INTERPX( GRID_DOT_2D, 'MSFD2', PNAME, 
     &                        STRTCOLGD2, ENDCOLGD2, STRTROWGD2, ENDROWGD2, 1, 1,
     &                        0, 0, MSFD2 ) ) THEN
             XMSG = 'Could not interpolate MSFD2 from ' // GRID_DOT_2D
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
#else
          IF ( .NOT. XTRACT3( GRID_DOT_2D, 'MSFD2',
     &                        1, 1, STRTROWGD2, ENDROWGD2, STRTCOLGD2, ENDCOLGD2,
     &                        0, 0, MSFD2 ) ) THEN
             XMSG = 'Could not interpolate MSFD2 from ' // GRID_DOT_2D
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          END IF
#endif

        end subroutine retrieve_grid_dot_2d_data

! -------------------------------------------------------------------------
        subroutine retrieve_ocean_data

          USE RXNS_DATA, ONLY : MECHNAME
          USE UTILIO_DEFN
          USE HGRD_DEFN

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_ocean_data'
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '

          allocate (ocean(ncols, nrows), 
     &              szone(ncols, nrows),   
     &              chlr(ncols, nrows),
     &              dmsl(ncols, nrows),                
     &              STAT=STAT)
          IF ( STAT .NE. 0 ) THEN
             XMSG = 'Failure allocating OPEN, SURF, CHLO, DMS array'
             CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
          END IF

          IF ( .NOT. OCEAN_CHEM ) THEN

             WRITE( LOGDEV, '(/,5x,A,/,5x,A,/5x,A)' ), 
     &              'CTM_OCEAN_CHEM set to FALSE. Open ocean and surf zone',
     &              'fractions will be set to 0. There will be no oceanic',
     &              'halogen-mediated loss of ozone, dms chemistry, or sea spray aerosol emissions.'
             ocean = 0.0
             szone = 0.0          
             dmsl  = 0.0          
             chlr  = 0.0

             If ( INDEX( MECHNAME, 'CB6R5M_AE7_AQ' ) .GT. 0 ) then
                XMSG = 'CTM_OCEAN_CHEM must be set to TRUE when using CB6R5M_AE7_AQ mechanism' 
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             endif

          ELSEIF ( OCEAN_CHEM .AND. .NOT. USE_MARINE_GAS_EMISSION ) THEN

             IF ( .NOT. OPEN3( OCEAN_1, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open ' // OCEAN_1
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             ELSE 
                n_opened_file = n_opened_file + 1

                call subhfile ( OCEAN_1, gxoff, gyoff,
     &                          startcol, endcol, startrow, endrow )

                IF ( .NOT. XTRACT3( OCEAN_1, 'OPEN',
     &                        1, 1, startrow, endrow, startcol, endcol,
     &                        0, 0, ocean ) ) Then
                   XMSG = 'Could not read OPEN from ' // OCEAN_1
                   CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                END IF

                IF ( .NOT. XTRACT3( OCEAN_1, 'SURF',
     &                        1, 1, startrow, endrow, startcol, endcol,
     &                        0, 0, szone ) ) Then
                   XMSG = 'Could not interpolate SURF from ' // OCEAN_1
                   CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                END IF

                WHERE ( ocean .LT. 0.001 ) ocean = 0.0  ! ensure values are nonnegative and greater than 0.001
                WHERE ( szone .LT. 0.001 ) szone = 0.0  ! ensure values are nonnegative and greater than 0.001  

                dmsl  = 0.0          
                chlr  = 0.0
             
             ENDIF

          ELSEIF ( OCEAN_CHEM .AND. USE_MARINE_GAS_EMISSION ) THEN

             IF ( .NOT. OPEN3( OCEAN_1, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open ' // OCEAN_1
                Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             ELSE 
                n_opened_file = n_opened_file + 1

                call subhfile ( OCEAN_1, gxoff, gyoff,
     &                          startcol, endcol, startrow, endrow )

                IF ( .NOT. XTRACT3( OCEAN_1, 'OPEN',
     &                        1, 1, startrow, endrow, startcol, endcol,
     &                        0, 0, ocean ) ) Then
                   XMSG = 'Could not read OPEN from ' // OCEAN_1
                   CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                END IF

                IF ( .NOT. XTRACT3( OCEAN_1, 'SURF',
     &                        1, 1, startrow, endrow, startcol, endcol,
     &                        0, 0, szone ) ) Then
                   XMSG = 'Could not interpolate SURF from ' // OCEAN_1
                   CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                END IF                

                WHERE ( ocean .LT. 0.001 ) ocean = 0.0  ! ensure values are nonnegative and greater than 0.001
                WHERE ( szone .LT. 0.001 ) szone = 0.0  ! ensure values are nonnegative and greater than 0.001 

                If ( INDEX( MECHNAME, 'CB6R5M_AE7_AQ' ) .GT. 0 ) then
      
                   If ( .Not. XTRACT3( OCEAN_1, 'CHLO',
     &                                 1, 1, startrow, endrow, startcol, endcol,
     &                                 0, 0, chlr ) ) Then
                      XMSG = 'Could not read CHLO from ' // OCEAN_1
                      Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                   End If

                   If ( .Not. XTRACT3( OCEAN_1, 'DMS',
     &                                 1, 1, startrow, endrow, startcol, endcol,
     &                                 0, 0, dmsl ) ) Then
                      XMSG = 'Could not read DMS from ' // OCEAN_1
                      Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                   End If

                ELSEIF ( INDEX( MECHNAME, 'CB6R5_AE7_AQ' ) .GT. 0 ) then

                   chlr = 0.0
                
                   If ( .Not. XTRACT3( OCEAN_1, 'DMS',
     &                                 1, 1, startrow, endrow, startcol, endcol,
     &                                 0, 0, dmsl ) ) Then
                      XMSG = 'Could not read DMS from ' // OCEAN_1
                      Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                   End If

                END IF

             END IF
          END IF

        end subroutine retrieve_ocean_data

! -------------------------------------------------------------------------
        subroutine retrieve_ltng_param_data 

          USE UTILIO_DEFN
          USE HGRD_DEFN

          INCLUDE SUBST_FILES_ID             ! file name parameters

          Character( 40 ), parameter :: pname = 'retrieve_ltng_param_data'
          Character( 40 ), parameter :: LTNGPARMS_FILE = 'LTNGPARMS_FILE'

          INTEGER          :: STAT
          CHARACTER( 120 ) :: XMSG = ' '
          integer :: startcol, endcol, startrow, endrow, gxoff, gyoff

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
          n_opened_file = n_opened_file + 1

          call subhfile ( LTNGPARMS_FILE, gxoff, gyoff,
     &                    startcol, endcol, startrow, endrow )

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "OCNMASK", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, OCEAN_MASK ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "SLOPE", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, SLOPE ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "INTERCEPT", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, INTERCEPT ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "SLOPE_lg", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, SLOPE_lg ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "INTERCEPT_lg", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, INTERCEPT_lg ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "ICCG_SUM", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, ICCG_SUM ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

          IF ( .NOT. XTRACT3( LTNGPARMS_FILE, "ICCG_WIN", 1, 1,
     &                        startrow, endrow, startcol, endcol,
     &                        0, 0, ICCG_WIN ) ) Then
             XMSG = 'Could not interpolate OPEN from ' // LTNGPARMS_FILE
             Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
          End If

        end subroutine retrieve_ltng_param_data

! -------------------------------------------------------------------------
        subroutine retrieve_boundary_data (jdate, jtime, vname)

          USE UTILIO_DEFN
          USE HGRD_DEFN
          USE VGRD_DEFN, ONLY : NLAYS
          USE CGRID_SPCS

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: vname

          Character( 40 ), parameter :: pname = 'retrieve_boundary_data'

          LOGICAL, SAVE :: firstime = .true.
          INTEGER :: STAT, i, j, begin, end, buf_loc, iterations,
     &               iter, loc_jdate_met, loc_jdate, loc_jtime_met, 
     &               loc_jtime, v, beg_v, end_v

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

          loc_jdate     = jdate
          loc_jdate_met = jdate
          loc_jtime     = jtime
          loc_jtime_met = jtime

          do iter = 1, iterations
             do v = beg_v, end_v
                buf_loc = mod((tail_bndy(v) + iter), 2)

                cio_bndy_data_tstamp(1, buf_loc, v) = loc_jdate
                if (cio_bndy_var_name(v,2) == 'mb') then
                   cio_bndy_data_tstamp(2, buf_loc, v) = loc_jtime_met
                else
                   cio_bndy_data_tstamp(2, buf_loc, v) = loc_jtime
                end if

                begin = cio_bndy_data_inx(1,buf_loc,v)
                end   = cio_bndy_data_inx(2,buf_loc,v)

                if (cio_bndy_var_name(v,2) == 'mb') then
#ifdef twoway
                   cio_bndy_data(begin:end) = 0.0
#else
                   if (.not. read3 (MET_BDY_3D, cio_bndy_var_name(v,1), -1,
     &                              loc_jdate_met, loc_jtime_met, cio_bndy_data(begin:end) ) ) THEN
                      XMSG = 'Could not read ' // MET_BDY_3D // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate_met, loc_jtime_met, XMSG, XSTAT1 )
                   END IF
#endif
                else if (cio_bndy_var_name(v,2) == 'bct') then

                   if (.not. read3 (BCFILE, cio_bndy_var_name(v,1), -1,
     &                              loc_jdate, loc_jtime, cio_bndy_data(begin:end) ) ) THEN
                      XMSG = 'Could not read ' // BCFILE // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jtime, XMSG, XSTAT1 )
                   END IF

                else if (cio_bndy_var_name(v,2) == 'bc') then

                   if (iter == 1) then
                      if (.not. read3 (BCFILE, cio_bndy_var_name(v,1), -1,
     &                                 loc_jdate, loc_jtime, cio_bndy_data(begin:end) ) ) THEN
                         XMSG = 'Could not read ' // BCFILE // ' file'
                      CALL M3EXIT ( PNAME, loc_jdate, loc_jtime, XMSG, XSTAT1 )
                      END IF
                   else
                      cio_bndy_data_tstamp(1, buf_loc, v) = jdate + 999 ! this will ensure future never falls out of the circular buffer
                   end if

                else
                   call m3exit( 'Centralized I/O',0,0,' ==d== UNKOWNi Type of File',1 )
                end if

             end do

             CALL NEXTIME ( loc_jdate_met, loc_jtime_met, file_tstep(f_met))
             CALL NEXTIME ( loc_jdate, loc_jtime, file_tstep(f_bcon))

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

        end subroutine retrieve_boundary_data

! -------------------------------------------------------------------------
        subroutine retrieve_stack_data (jdate, jtime, fname, vname)

          USE UTILIO_DEFN
          USE STK_PRMS, only : MY_STRT_SRC, MY_END_SRC

          INCLUDE SUBST_FILES_ID             ! file name parameters

          integer, intent(in) :: jdate, jtime
          character (*), intent(in), optional :: fname, vname

          Character( 40 ), parameter :: pname = 'retrieve_stack_data'

          LOGICAL, SAVE :: firstime = .true.
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
                loc_jdate = jdate
                if (file_sym_date(f_stk_emis(pt))) loc_jdate = file_sdate(f_stk_emis(pt)) ! Representative day check
                loc_jtime = jtime
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

                CALL NEXTIME ( loc_jdate, loc_jtime, file_tstep(f_stk_emis(pt)) )
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
        subroutine r_interpolate_var_1ds (fname, vname, date, time, data)

! Function: Interpolation for Stack Group Real 1-D Data

          USE UTILIO_DEFN
          USE STK_PRMS, only : MY_STRT_SRC, MY_END_SRC

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          real, intent(out)         :: data(:)

          integer :: head_beg_ind, head_end_ind,
     &               tail_beg_ind, tail_end_ind,
     &               store_beg_ind, store_end_ind,
     &               var_loc, loc_head, loc_tail, m, r, c,
     &               loc_jdate, loc_jtime, dsize, pt, loc_tstep
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: prev_tail_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2
          character(200) :: xmsg

          pt = binary_search (fname, cio_stack_file_name, NPTGRPS)

          var_loc = binary_search (vname, cio_stack_var_name(:,pt), n_cio_stack_emis_vars(pt))

          if (var_loc .lt. 0) then
             write (cio_logdev, '(a9, a, a33)') 'Warning: ', trim(vname), ' is not available in a stack file.'
             write (xmsg, '(A9,A,A,A)' ) 'ERROR: ',trim(vname), ' is not available ',
     &             'on a Stack Emisison file. Simulation will now terminate.'
             call m3exit ( 'Centralized I/O Module', date, time, xmsg, 1 )
          else
             dsize = MY_END_SRC( pt ) - MY_STRT_SRC( pt ) + 1

             loc_tstep = file_tstep(f_stk_emis(pt)) 

             loc_head = head_stack_emis(var_loc, pt)
             loc_tail = tail_stack_emis(var_loc, pt)

             if ((cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .eq. date) .and.
     &           (cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .eq. time)) then
                ! do nothing since it is an exact date time data exist
             else
                if ((cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .lt. date) .or.
     &              ((cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .eq. date) .and.
     &               (cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .eq. 0)) .or.
     &              ((cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) .lt. time) .and.
     &               (cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt) .eq. date))) then

                   loc_jdate = cio_stack_emis_data_tstamp(1, loc_tail, var_loc, pt)
                   loc_jtime = cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt) 
                   CALL NEXTIME ( loc_jdate, loc_jtime, loc_tstep )
                   call retrieve_stack_data (loc_jdate, loc_jtime, fname, vname)
                   loc_head = head_stack_emis(var_loc, pt)
                   loc_tail = tail_stack_emis(var_loc, pt)
                end if
             end if

             if ((cio_stack_emis_data_tstamp(1, 2, var_loc, pt) .eq. date) .and.
     &           (cio_stack_emis_data_tstamp(2, 2, var_loc, pt) .eq. time)) then
                count = count + 1
             else

                cio_stack_emis_data_tstamp(1, 2, var_loc, pt) = date
                cio_stack_emis_data_tstamp(2, 2, var_loc, pt) = time

                if ((prev_time .ne. time) .or.
     &              (prev_head_time .ne. cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt)) .or.
     &              (prev_tail_time .ne. cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt))) then

                   if (cio_stack_emis_data_tstamp(1, loc_head, var_loc, pt) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_stack_emis_data_tstamp(2, loc_head, var_loc, pt)
                   prev_tail_time = cio_stack_emis_data_tstamp(2, loc_tail, var_loc, pt)
                   
#ifdef verbose_cio
                   if ( (ratio1 .lt. 0) .or. (ratio2 .lt. 0) 
     &             .or. (ratio1 .gt. 1) .or. (ratio2 .gt. 1)) then
                      write(logdev,'(5X,a,a)'),
     &               'ERROR: Incorrect Interpolation in 1-D Stack Interpolation for variable: ',
     &                trim(vname)
                     
                     write(logdev,'(5X,a,i7,a,i6)'), 
     &               'Requested TIME & DATE: ',date,':',time
                     
                     write(logdev,'(5X,a,i7,a,i6,a,i7,a,i6)'),
     &               'Interpolation Bounds ',cio_stack_emis_data_tstamp(1,0,var_loc,pt),
     &               ':',cio_stack_emis_data_tstamp(2,0,var_loc,pt),' to ',
     &                cio_stack_emis_data_tstamp(1,1,var_loc,pt),':',cio_stack_emis_data_tstamp(2,1,var_loc,pt)
                      call m3exit( 'Centralized I/O',date,time,'',1 )
                      write(logdev,'(5X,a)'),
     &                'ERROR: Program EXIT in subroutine r_interpolate_var_1ds in module centralized io'
                   end if 
#endif
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

! Function: Interpolation for generic Real 2-D Data

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
     &               loc_size_spatial, loc_tstep, str_len, fnum
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: prev_tail_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2
          character(200) :: xmsg

          var_loc = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (xmsg, '(A9,A,A,A)' ) 'ERROR: ',trim(vname), ' is not available ',
     &             'on any 2D file. Simulation will now terminate.'
             call m3exit ( 'Centralized I/O Module', date, time, xmsg, 1 )
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if (cio_grid_var_name(var_loc,3) == 'm') then
                loc_tstep = file_tstep(f_met) 
             else if ((cio_grid_var_name(var_loc,2) == 'e2d') .or.
     &           (cio_grid_var_name(var_loc,2) == 'e3d')) then

                str_len = len_trim(cio_grid_var_name(var_loc,1))
                read (cio_grid_var_name(var_loc,1)(str_len-2:str_len), *) fnum

                loc_tstep = file_tstep(f_emis(fnum)) 
             else if (cio_grid_var_name(var_loc,2) == 'lnt') then
                loc_tstep = file_tstep(f_ltng)
             else if (cio_grid_var_name(var_loc,2) == 'ic') then
                loc_tstep = file_tstep(f_icon)
             else if (cio_grid_var_name(var_loc,2) == 'bct') then
                loc_tstep = file_tstep(f_bcon)
             else if (cio_grid_var_name(var_loc,2) == 'is') then
                loc_tstep = file_tstep(f_is_icon)
             end if

             if (cio_grid_var_name(var_loc,2) .eq. 'md3') then
                loc_size_spatial = size_d2dx
             else
                loc_size_spatial = size_c3d / nlays
             end if

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(1, loc_tail, var_loc) .eq. date))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)

                CALL NEXTIME ( loc_jdate, loc_jtime, loc_tstep )

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
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc)) .or.
     &              (prev_tail_time .ne. cio_grid_data_tstamp(2, loc_tail, var_loc))) then
                
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                   prev_tail_time = cio_grid_data_tstamp(2, loc_tail, var_loc)
                   
#ifdef verbose_cio
                   if ( (ratio1 .lt. 0) .or. (ratio2 .lt. 0) 
     &             .or. (ratio1 .gt. 1) .or. (ratio2 .gt. 1)) then
                      write(logdev,'(5X,a,a)'),
     &               'ERROR: Incorrect Interpolation in 2-D Generic Real Interpolation for variable: ', 
     &                trim(vname) 

                     write(logdev,'(5X,a,i7,a,i6)'), 
     &               'Requested TIME & DATE: ',date,':',time
                     
                     write(logdev,'(5X,a,i7,a,i6,a,i7,a,i6)'),
     &               'Interpolation Bounds ',cio_grid_data_tstamp(1,0,var_loc),
     &               ':',cio_grid_data_tstamp(2,0,var_loc),' to ',
     &                cio_grid_data_tstamp(1,1,var_loc),':',cio_grid_data_tstamp(2,1,var_loc)

                      write(logdev,'(5X,a)'),
     &                'ERROR: Program EXIT in subroutine r_interpolate_var_2d in module centralized io'
                     
                     call m3exit( 'Centralized I/O',date,time,'',1 )
                   end if 
#endif
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
                if (cio_grid_var_name(var_loc,2) .eq. 'mc3') then
                   if ((window) .and. 
     &                 ((size(data,1) - ncols) .eq. 0)) then
                      adj1 = ncols + 3
                      adj2 = 2
                   end if
                   adj_lvl = (slay - 1) * loc_size_spatial
                else if (cio_grid_var_name(var_loc,2) .eq. 'md3') then
                   adj_lvl = (slay - 1) * size_d2dx
#ifndef twoway
                   if (.not. east_pe) then
                      adj2 = 1
                   end if
#endif
                end if
             else if (cio_grid_var_name(var_loc,2) .eq. 'mc2') then
#ifndef twoway
                if (.not. east_pe) then
                   adj2 = 1
                end if
#endif
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

! Function: Interpolation for generic 4 byte Integer 2-D Data
 
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
     &               loc_size_spatial, loc_tstep, str_len, fnum
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: prev_tail_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2
          character(200) :: xmsg

          var_loc = binary_search (vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (xmsg, '(A9,A,A,A)' ) 'ERROR: ',trim(vname), ' is not available ',
     &             'on any 2D file. Simulation will now terminate.'
             call m3exit ( 'Centralized I/O Module', date, time, xmsg, 1 )
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if (cio_grid_var_name(var_loc,3) == 'm') then
                loc_tstep = file_tstep(f_met) 
             else if ((cio_grid_var_name(var_loc,2) == 'e2d') .or.
     &           (cio_grid_var_name(var_loc,2) == 'e3d')) then

                str_len = len_trim(cio_grid_var_name(var_loc,1))
                read (cio_grid_var_name(var_loc,1)(str_len-2:str_len), *) fnum

                loc_tstep = file_tstep(f_emis(fnum)) 
             else if (cio_grid_var_name(var_loc,2) == 'lnt') then
                loc_tstep = file_tstep(f_ltng)
             else if (cio_grid_var_name(var_loc,2) == 'ic') then
                loc_tstep = file_tstep(f_icon)
             else if (cio_grid_var_name(var_loc,2) == 'bct') then
                loc_tstep = file_tstep(f_bcon)
             else if (cio_grid_var_name(var_loc,2) == 'is') then
                loc_tstep = file_tstep(f_is_icon)
             end if
             
             if (cio_grid_var_name(var_loc,2) .eq. 'md3') then
                loc_size_spatial = size_d2dx
             else
                loc_size_spatial = size_c3d / nlays
             end if

             if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_grid_data_tstamp(1, loc_tail, var_loc) .eq. date))) then

                loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)
                CALL NEXTIME ( loc_jdate, loc_jtime, loc_tstep )
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
     &              (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc)) .or.
     &              (prev_tail_time .ne. cio_grid_data_tstamp(2, loc_tail, var_loc))) then
                
                   if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                   prev_tail_time = cio_grid_data_tstamp(2, loc_tail, var_loc) 

#ifdef verbose_cio
                   if ( (ratio1 .lt. 0) .or. (ratio2 .lt. 0) 
     &             .or. (ratio1 .gt. 1) .or. (ratio2 .gt. 1)) then
                      write(logdev,'(5X,a,a)'),
     &               'ERROR: Incorrect Interpolation in 2-D Generic Integer Interpolation for variable: ',
     &                trim(vname) 
                     
                     write(logdev,'(5X,a,i7,a,i6)'), 
     &               'Requested TIME & DATE: ',date,':',time
                     
                     write(logdev,'(5X,a,i7,a,i6,a,i7,a,i6)'),
     &               'Interpolation Bounds ',cio_grid_data_tstamp(1,0,var_loc),
     &               ':',cio_grid_data_tstamp(2,0,var_loc),' to ',
     &                cio_grid_data_tstamp(1,1,var_loc),':',cio_grid_data_tstamp(2,1,var_loc)
                     
                      call m3exit( 'Centralized I/O',date,time,'',1 )
                      write(logdev,'(5X,a)'),
     &                'ERROR: Program EXIT in subroutine i_interpolate_var_2d in module centralized io'
                  
                  end if 
#endif
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
        subroutine r_interpolate_var_2db (vname, date, time, data, type, lvl)

! Function: Interpolation for Boundary Real 2-D Data
          
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
     &               beg_k, end_k, loc_tstep
          integer, save :: lns_size, lew_size, gns_size, gew_size,
     &                     ls_start, ls_end, ln_start, ln_end,
     &                     le_start, le_end, lw_start, lw_end,
     &                     gs_skip, ge_skip, gn_skip, gw_skip
          logical, save :: loc_firstime = .true.
          integer, save :: prev_time = -1
          real :: ratio1, ratio2
          character(200) :: xmsg

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
             write (xmsg, '(A9,A,A,A)' ) 'ERROR: ',trim(vname), ' is not available ',
     &             'on any BNDY file. Simulation will now terminate.'
             call m3exit ( 'Centralized I/O Module', date, time, xmsg, 1 )
          else
             loc_head = head_bndy(var_loc)
             loc_tail = tail_bndy(var_loc)

             if (cio_bndy_var_name(var_loc,2) == 'mb') then
                loc_tstep = file_tstep(f_met)
             else
                loc_tstep = file_tstep(f_bcon)
             end if

             if (cio_bndy_var_name(var_loc, 2) .ne. 'bc') then
                if ((cio_bndy_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &              ((cio_bndy_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &               (cio_bndy_data_tstamp(1, loc_tail, var_loc) .eq. date))) then

                   loc_jdate = cio_bndy_data_tstamp(1, loc_tail, var_loc)
                   loc_jtime = cio_bndy_data_tstamp(2, loc_tail, var_loc)

                   CALL NEXTIME ( loc_jdate, loc_jtime, loc_tstep )

                   call retrieve_boundary_data (loc_jdate, loc_jtime, vname)

                   loc_head = head_bndy(var_loc)
                   loc_tail = tail_bndy(var_loc)
                end if
             end if

             if ((cio_bndy_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_bndy_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                cio_bndy_data_tstamp(1, 2, var_loc) = date
                cio_bndy_data_tstamp(2, 2, var_loc) = time

                head_beg_ind  = cio_bndy_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_bndy_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_bndy_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_bndy_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_bndy_data_inx(1,2,var_loc)
                store_end_ind = cio_bndy_data_inx(2,2,var_loc)

                if (cio_bndy_var_name(var_loc, 2) == 'bc') then
                   cio_bndy_data(store_beg_ind:store_end_ind) = cio_bndy_data(head_beg_ind:head_end_ind)
                else
                   if (cio_bndy_data_tstamp(1, loc_head, var_loc) .eq. date) then
                      ratio2 =   real(time_diff(time, cio_bndy_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   else
                      ratio2 =   real(time_diff(240000, cio_bndy_data_tstamp(2, loc_head, var_loc))) 
     &                         / real(time2sec(loc_tstep))
                      ratio1 = 1.0 - ratio2
                   end if
                   prev_time = time
                   
#ifdef verbose_cio
                   if ( (ratio1 .lt. 0) .or. (ratio2 .lt. 0) 
     &             .or. (ratio1 .gt. 1) .or. (ratio2 .gt. 1)) then
                      write(logdev,'(5X,a)'),
     &               'ERROR: Incorrect Interpolation in 2-D Boundary Interpolation for variable: ',
     &                trim(vname) 
                     
                     write(logdev,'(5X,a,i7,a,i6)'), 
     &               'Requested TIME & DATE: ',date,':',time
                     
                     write(logdev,'(5X,a,i7,a,i6,a,i7,a,i6)'),
     &               'Interpolation Bounds ',cio_bndy_data_tstamp(1,0,var_loc),
     &               ':',cio_bndy_data_tstamp(2,0,var_loc),' to ',
     &                cio_bndy_data_tstamp(1,1,var_loc),':',cio_bndy_data_tstamp(2,1,var_loc)

                     call m3exit( 'Centralized I/O',date,time,'',1 )

                      write(logdev,'(5X,a)'),
     &                'ERROR: Program EXIT in subroutine r_interpolate_var_2db in module centralized io'
           
                   end if 
#endif
                   cio_bndy_data(store_beg_ind:store_end_ind) =   cio_bndy_data(head_beg_ind:head_end_ind) * ratio1
     &                                                          + cio_bndy_data(tail_beg_ind:tail_end_ind) * ratio2

                end if

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

!Function: Interpolation for generic Real 3-D Data 

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
     &               slen, str_len, fnum, loc_tstep

          character (20) :: loc_vname
          integer, save :: prev_time = -1
          integer, save :: prev_head_time = -1
          integer, save :: prev_tail_time = -1
          integer, save :: lcount = 0
          real, save :: ratio1, ratio2
          character(200) :: xmsg

          if (present(fname)) then
             slen = len_trim(fname)
             loc_vname = trim(vname) // fname(slen-3:slen)
          else
             loc_vname = vname
          end if

          var_loc = binary_search (loc_vname, cio_grid_var_name(:,1), n_cio_grid_vars)
          if (var_loc .lt. 0) then
             write (xmsg, '(A9,A,A,A)' ) 'ERROR: ',trim(vname), ' is not available ',
     &             'on any 3D file. Simulation will now terminate.'
             call m3exit ( 'Centralized I/O Module', date, time, xmsg, 1 )
          else
             loc_head = head_grid(var_loc)
             loc_tail = tail_grid(var_loc)

             if (cio_grid_var_name(var_loc,3) == 'm') then
                loc_tstep = file_tstep(f_met) 
             else if ((cio_grid_var_name(var_loc,2) == 'e2d') .or.
     &           (cio_grid_var_name(var_loc,2) == 'e3d')) then

                str_len = len_trim(cio_grid_var_name(var_loc,1))
                read (cio_grid_var_name(var_loc,1)(str_len-2:str_len), *) fnum

                loc_tstep = file_tstep(f_emis(fnum)) 
             else if (cio_grid_var_name(var_loc,2) == 'lnt') then
                loc_tstep = file_tstep(f_ltng)
             else if (cio_grid_var_name(var_loc,2) == 'ic') then
                loc_tstep = file_tstep(f_icon)
             else if (cio_grid_var_name(var_loc,2) == 'bct') then
                loc_tstep = file_tstep(f_bcon)
             else if (cio_grid_var_name(var_loc,2) == 'is') then
                loc_tstep = file_tstep(f_is_icon)
             end if

             if (cio_grid_var_name(var_loc,2) .ne. 'ic') then
                if ((cio_grid_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &              ((cio_grid_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &               (cio_grid_data_tstamp(1, loc_tail, var_loc) .eq. date))) then

                   loc_jdate = cio_grid_data_tstamp(1, loc_tail, var_loc)
                   loc_jtime = cio_grid_data_tstamp(2, loc_tail, var_loc)

                   CALL NEXTIME ( loc_jdate, loc_jtime, loc_tstep )

                   call retrieve_time_dep_gridded_data (loc_jdate, loc_jtime, loc_vname)
                   loc_head = head_grid(var_loc)
                   loc_tail = tail_grid(var_loc)
                end if
             end if

             if ((cio_grid_data_tstamp(1, 2, var_loc) .eq. date) .and.
     &           (cio_grid_data_tstamp(2, 2, var_loc) .eq. time)) then
                count = count + 1
             else

                head_beg_ind  = cio_grid_data_inx(1,loc_head,var_loc)
                head_end_ind  = cio_grid_data_inx(2,loc_head,var_loc)
                tail_beg_ind  = cio_grid_data_inx(1,loc_tail,var_loc)
                tail_end_ind  = cio_grid_data_inx(2,loc_tail,var_loc)
                store_beg_ind = cio_grid_data_inx(1,2,var_loc)
                store_end_ind = cio_grid_data_inx(2,2,var_loc)

                if ((cio_grid_var_name(var_loc, 2) .eq. 'ic') .or.
     &              (cio_grid_var_name(var_loc, 2) .eq. 'is')) then
                   cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind)
                else
                   cio_grid_data_tstamp(1, 2, var_loc) = date
                   cio_grid_data_tstamp(2, 2, var_loc) = time

                   if ((prev_time .ne. time) .or.
     &                 (prev_head_time .ne. cio_grid_data_tstamp(2, loc_head, var_loc)) .or.
     &                 (prev_tail_time .ne. cio_grid_data_tstamp(2, loc_tail, var_loc))) then

                      if (cio_grid_data_tstamp(1, loc_head, var_loc) .eq. date) then
                         ratio2 =   real(time_diff(time, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                            / real(time_to_sec(loc_tstep))
                         ratio1 = 1.0 - ratio2
                      else
                         ratio2 =   real(time_diff(240000, cio_grid_data_tstamp(2, loc_head, var_loc))) 
     &                            / real(time_to_sec(loc_tstep))
                         ratio1 = 1.0 - ratio2
                      end if
                      prev_time = time
                      prev_head_time = cio_grid_data_tstamp(2, loc_head, var_loc)
                      prev_tail_time = cio_grid_data_tstamp(2, loc_tail, var_loc)
                   
#ifdef verbose_cio
                      if ( (ratio1 .lt. 0) .or. (ratio2 .lt. 0) 
     &                .or. (ratio1 .gt. 1) .or. (ratio2 .gt. 1)) then
                        write(logdev,'(5X,a)'),
     &                  'ERROR: Incorrect Interpolation in 3-D Generic Interpolation for variable: ',
     &                   trim(vname) 
                     
                        write(logdev,'(5X,a,i7,a,i6)'), 
     &                  'Requested TIME & DATE: ',date,':',time
                     
                        write(logdev,'(5X,a,i7,a,i6,a,i7,a,i6)'),
     &                  'Interpolation Bounds ',cio_grid_data_tstamp(1,0,var_loc),
     &                  ':',cio_grid_data_tstamp(2,0,var_loc),' to ',
     &                   cio_grid_data_tstamp(1,1,var_loc),':',cio_grid_data_tstamp(2,1,var_loc)

                        call m3exit( 'Centralized I/O',date,time,'',1 )
                         write(logdev,'(5X,a)'),
     &                   'ERROR: Program EXIT in subroutine r_interpolate_var_3d in module centralized io'
                    
                       end if 
#endif
                   else
                      lcount = lcount + 1
                   end if

                   cio_grid_data(store_beg_ind:store_end_ind) =   cio_grid_data(head_beg_ind:head_end_ind) * ratio1
     &                                                          + cio_grid_data(tail_beg_ind:tail_end_ind) * ratio2
                end if
             end if
 
             beg_k = 1
             if (cio_grid_var_name(var_loc, 2) .eq. 'e2d') then
                end_k = 1
             else
                end_k = size(data,3)
             end if

             adj1 = 0
             adj2 = 0
             adj3 = 0
             if (window) then
                if (((size(data,1) - ncols) .eq. 0) .and.
     &              (cio_grid_var_name(var_loc, 2) .eq. 'mc3')) then
                   adj1 = ncols + 3
                   adj2 = 2
                   adj3 = 2 * ncols + 4
                else if (cio_grid_var_name(var_loc, 2) .eq. 'md3') then
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
#ifdef twoway
                   adj2 = 0
                   adj3 = 0
#endif
                end if
             else
                extra_c = 0
                extra_r = 0

                if (cio_grid_var_name(var_loc, 2) .eq. 'md3') then
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

                if ((cio_grid_var_name(var_loc, 2) .ne. 'e2d') .and.
     &              (cio_grid_var_name(var_loc, 2) .ne. 'e3d') .and.
     &              (cio_grid_var_name(var_loc, 2) .ne. 'ic') .and.
     &              (cio_grid_var_name(var_loc, 2) .ne. 'is')) then
                   adj2 = extra_c
                   adj3 = extra_r * col_size + extra_c
                   if (north_pe .and. east_pe) then
                      adj3 = 0
                   else if (north_pe) then
                      adj3 = adj3 - 1
                   end if
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
                if (window .and. (cio_grid_var_name(var_loc, 2) .eq. 'md3')) then
                   m = m - adj2 + adj3
                else
                   m = m + adj3
                end if
             end do
          end if

        end subroutine r_interpolate_var_3d
#endif

      END MODULE CENTRALIZED_IO_MODULE

